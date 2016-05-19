!     #########
      SUBROUTINE WATER_FLUX(PZ0SEA,                                         &
                              PTA, PEXNA, PRHOA, PSST, PEXNS, PQA, PRR, PRS,  &
                              PTT, PVMOD, PZREF, PUREF,                       &
                              PPS, PQSAT,                                     &
                              PSFTH, PSFTQ, PUSTAR,                           &
                              PCD, PCDN, PCH, PRI, PRESA, PZ0HSEA             )  
!     #######################################################################
!
!
!!****  *WATER_FLUX*  
!!
!!    PURPOSE
!!    -------
!      Calculate the surface fluxes of heat, moisture, and momentum over
!      water surfaces.  
!     
!!**  METHOD
!!    ------
!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------ 
!!
!!      
!!    REFERENCE
!!    ---------
!!      
!!    AUTHOR
!!    ------
!!	S. Belair           * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original      01/09/95 
!!      (J.Stein)     16/11/95  use PUSLOPE and Theta to compute Ri
!!      (P.Lacarrere) 19/03/96  bug in the ZTHVI and ZTHVIS computations
!!      (J.Stein)     27/03/96  use only H and LE in the soil scheme
!!      (P.Jabouille) 12/11/96  bug in the Z0 computation
!!      (V.Masson)    01/02/00  detection of sea ice
!!      (P. Tulet)    01/10/03  aerodynamical resistance output
!!      (P. LeMoigne) 29/03/04  bug in the heat flux computation
!!      (P. LeMoigne) 29/03/04  use z0h for diagnostics (ice)
!!      (P. LeMoigne) 20/06/07  minimum wind speed and/or shear
!!      B. Decharme    06/2009 limitation of Ri
!!      B. Decharme    09/2012 limitation of Ri in surface_ri.F90
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_CSTS,       ONLY : XG, XCPD, XLSTT
USE MODD_SURF_PAR,   ONLY : XUNDEF
USE MODD_SNOW_PAR,   ONLY : XZ0SN, XZ0HSN
!
USE MODI_SURFACE_RI
USE MODI_SURFACE_AERO_COND
USE MODI_SURFACE_CD
USE MODI_SURFACE_CDCH_1DARP
USE MODI_WIND_THRESHOLD
!
USE MODE_THERMOS
!
USE MODD_SURF_ATM,    ONLY : LDRAG_COEF_ARP, XVCHRNK, XVZ0CM, LVZIUSTAR0_ARP, XVZIUSTAR0,  &
                               LRRGUST_ARP, XRRSCALE, XRRGAMMA, XUTILGUST, XRZHZ0M  

!
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
REAL, DIMENSION(:), INTENT(IN)       :: PTA   ! air temperature at atm. level
REAL, DIMENSION(:), INTENT(IN)       :: PQA   ! air humidity at atm. level (kg/kg)
REAL, DIMENSION(:), INTENT(IN)       :: PEXNA ! Exner function at atm. level
REAL, DIMENSION(:), INTENT(IN)       :: PRHOA ! air density at atm. level
REAL, DIMENSION(:), INTENT(IN)       :: PVMOD ! module of wind at atm. wind level
REAL, DIMENSION(:), INTENT(IN)       :: PZREF ! atm. level for temp. and humidity
REAL, DIMENSION(:), INTENT(IN)       :: PUREF ! atm. level for wind
REAL, DIMENSION(:), INTENT(IN)       :: PSST  ! Sea Surface Temperature
REAL, DIMENSION(:), INTENT(IN)       :: PEXNS ! Exner function at sea surface
REAL, DIMENSION(:), INTENT(IN)       :: PPS   ! air pressure at sea surface
REAL, DIMENSION(:), INTENT(IN)       :: PRR   ! rain rate
REAL, DIMENSION(:), INTENT(IN)       :: PRS   ! snow rate
REAL,               INTENT(IN)       :: PTT   ! temperature of freezing point
!
REAL, DIMENSION(:), INTENT(INOUT)    :: PZ0SEA! roughness length over the ocean
!                                         
!                                         
!  surface fluxes : latent heat, sensible heat, friction fluxes
REAL, DIMENSION(:), INTENT(OUT)      :: PSFTH ! heat flux  (W/m2)
REAL, DIMENSION(:), INTENT(OUT)      :: PSFTQ ! water flux (kg/m2/s)
REAL, DIMENSION(:), INTENT(OUT)      :: PUSTAR! friction velocity (m/s)
!
! diagnostics
REAL, DIMENSION(:), INTENT(OUT)      :: PQSAT ! humidity at saturation
REAL, DIMENSION(:), INTENT(OUT)      :: PCD   ! heat drag coefficient
REAL, DIMENSION(:), INTENT(OUT)      :: PCDN  ! momentum drag coefficient
REAL, DIMENSION(:), INTENT(OUT)      :: PCH   ! neutral momentum drag coefficient
REAL, DIMENSION(:), INTENT(OUT)      :: PRI   ! Richardson number
REAL, DIMENSION(:), INTENT(OUT)      :: PRESA ! aerodynamical resistance
REAL, DIMENSION(:), INTENT(OUT)      :: PZ0HSEA ! heat roughness length over the ocean
!
!
!*      0.2    declarations of local variables
!
!
REAL, DIMENSION(SIZE(PTA)) :: ZVMOD     ! wind modulus
REAL, DIMENSION(SIZE(PTA)) :: ZUSTAR2   ! square of friction velocity
REAL, DIMENSION(SIZE(PTA)) :: ZAC       ! Aerodynamical conductance
REAL, DIMENSION(SIZE(PTA)) :: ZRA       ! Aerodynamical resistance
REAL, DIMENSION(SIZE(PTA)) :: ZDIRCOSZW ! orography slope cosine (=1 on water!)
REAL, DIMENSION(SIZE(PTA)) :: ZFP       ! working variable
REAL, DIMENSION(SIZE(PTA)) :: ZRRCOR    ! correction of CD, CH, CDN due to moist-gustiness
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!       1.     Initializations
!              ---------------
!
IF (LHOOK) CALL DR_HOOK('WATER_FLUX',0,ZHOOK_HANDLE)
ZDIRCOSZW=1.
!
PRI(:) = XUNDEF
PCH(:) = XUNDEF
PCD(:) = XUNDEF
PCDN(:) = XUNDEF
!
PSFTH (:)=XUNDEF
PSFTQ (:)=XUNDEF
PUSTAR(:)=XUNDEF
PRESA(:)=XUNDEF
!
!
!       1.1    Saturated specified humidity near the water surface
!              ---------------------------------------------------
!
PQSAT(:) = QSAT(PSST(:),PPS(:))
!
!-------------------------------------------------------------------------------
!
!       2.     Calculate the drag coefficient for momentum (PCD)
!              -------------------------------------------------
!
!       2.1    Richardson number
!              -----------------
!
 CALL SURFACE_RI(PSST,PQSAT,PEXNS,PEXNA,PTA,PQA,  &
                  PZREF, PUREF, ZDIRCOSZW,PVMOD,PRI)
!
!       2.2    Detection of sea ice
!              --------------------
!
IF (LVZIUSTAR0_ARP) THEN
  WHERE (PSST(:) >= PTT)
    PZ0HSEA(:)=MIN(PZ0SEA(:),PZ0SEA(:)*XRZHZ0M)
  END WHERE
ELSEIF ( .NOT. LVZIUSTAR0_ARP ) THEN
  WHERE (PSST(:) >= PTT)
    PZ0HSEA(:)=PZ0SEA(:)
  END WHERE
ENDIF
WHERE (PSST(:) < PTT)
  PZ0HSEA(:) = XZ0HSN
END WHERE
!
!       2.3    Drag coefficient
!              ----------------
!
ZVMOD(:)=WIND_THRESHOLD(PVMOD(:),PUREF(:))
!
IF (LDRAG_COEF_ARP) THEN
 
  CALL SURFACE_CDCH_1DARP(PZREF, PZ0SEA, PZ0HSEA, ZVMOD, PTA, PSST, &
                            PQA, PQSAT, PCD, PCDN, PCH                )  

  ZRA(:) = 1. / ( PCH(:) * ZVMOD(:) )
!
!       2.4    Calculate u* and the roughness length over the ocean
!              ----------------------------------------------------
!
!                              According to Charnock's expression...
!
  ZUSTAR2(:) = PCD(:)*ZVMOD(:)*ZVMOD(:)
  WHERE (PSST(:)>=PTT)
    PZ0SEA(:) = XVCHRNK * ZUSTAR2(:) / XG + XVZ0CM * PCD(:) / PCDN(:)
  ELSEWHERE
    PZ0SEA(:) = XZ0SN
  END WHERE
  IF (LVZIUSTAR0_ARP .AND. XVZIUSTAR0>0.) THEN
    WHERE (PSST(:)>=PTT)
      PZ0HSEA(:)=PZ0SEA(:)*EXP(-SQRT(ZUSTAR2(:))*XVZIUSTAR0)
    END WHERE
  ELSE
    WHERE (PSST(:)>=PTT)
      PZ0HSEA(:)=PZ0SEA(:)
    END WHERE
  ENDIF     

ELSE
!
  CALL SURFACE_CD(PRI, PZREF, PUREF, PZ0SEA, PZ0HSEA, PCD, PCDN)
!
!-------------------------------------------------------------------------------
!
!       3.     Calculate u* and the roughness length over the ocean
!              ----------------------------------------------------
!
!                              According to Charnock's expression...
!
  ZUSTAR2(:) = PCD(:)*ZVMOD(:)*ZVMOD(:)
!
  WHERE (PSST(:)>=PTT)
    PZ0SEA(:) = XVCHRNK * ZUSTAR2(:) / XG + XVZ0CM * PCD(:) / PCDN(:)
  ELSEWHERE
    PZ0SEA(:) = XZ0SN
  END WHERE
  IF (LVZIUSTAR0_ARP .AND. XVZIUSTAR0>0.) THEN
    WHERE (PSST(:)>=PTT)
      PZ0HSEA(:)=PZ0SEA(:)*EXP(-SQRT(ZUSTAR2(:))*XVZIUSTAR0)
    END WHERE
  ELSE
    WHERE (PSST(:)>=PTT)
      PZ0HSEA(:)=PZ0SEA(:)
    END WHERE
  ENDIF     
!
!-------------------------------------------------------------------------------
!
!       4.     Drag coefficient for heat and aerodynamical resistance
!              -------------------------------------------------------
!
  CALL SURFACE_AERO_COND(PRI, PZREF, PUREF, ZVMOD, PZ0SEA, PZ0HSEA, ZAC, ZRA, PCH)
!
ENDIF
!
IF (LRRGUST_ARP) THEN
  ZFP(:)=MAX(0.0,PRR(:)+PRS(:))
  ZRRCOR(:)=SQRT(1.0+((((ZFP(:)/(ZFP(:)+XRRSCALE))**XRRGAMMA)*XUTILGUST)**2) &
      /(PCD(:)*ZVMOD(:)**2))  

  PCD  = PCD*ZRRCOR
  PCH  = PCH*ZRRCOR
  PCDN = PCDN*ZRRCOR
ENDIF
!
PRESA(:) = ZRA(:)
!
!-------------------------------------------------------------------------------
!
!       5.     The fluxes
!              ----------
!
PSFTH (:) =  XCPD * PRHOA(:) * PCH(:) * ZVMOD(:) * ( PSST(:) -PTA(:) * PEXNS(:) / PEXNA(:) ) / PEXNS(:)
PSFTQ (:) =  PRHOA(:) * PCH(:) * ZVMOD(:) * ( PQSAT(:)-PQA(:) )
PUSTAR(:) = SQRT(ZUSTAR2(:))
IF (LHOOK) CALL DR_HOOK('WATER_FLUX',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
!       6.     Specific fields for GELATO
!              --------------------------
!
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE WATER_FLUX
