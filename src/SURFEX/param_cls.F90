!     #########
       SUBROUTINE PARAM_CLS(PTA, PTS, PQA, PPA, PRHOA, PZONA, PMERA, PH, PHW, &
                              PSFTH, PSFTQ, PSFZON, PSFMER,                      &
                              PT2M, PQ2M, PHU2M, PZON10M, PMER10M               )  
!     #####################################################################
!
!!****  *PARAMCLS*  interpolates wind at 10m and temperature/humidity at 2m
!!
!!    PURPOSE
!!    -------
!
!         
!     
!!**  METHOD
!!    ------
!
!!    EXTERNAL
!!    --------
!!
!!    none
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------ 
!!
!!
!!      
!!    REFERENCE
!!    ---------
!!
!!      
!!    AUTHOR
!!    ------
!!
!!   Rui Salgado
!!
!!    MODIFICATIONS
!!    -------------
!!
!!      Original    26/10/98
!!                  06/2003  (V. Masson) use of Paulson functions and
!!                                       atmospheric level only
!!                  11/2006  (P. LeMoigne) min value for LMO for unstable case
!!                  01/2010  (S. Riette) XUNDEF is sent for wind where forcing
!!                                       level is below heigt of diagnostic
!!                                       (no extrapolation, only interpolation)
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_SURF_PAR,   ONLY : XUNDEF
USE MODD_CSTS,       ONLY : XKARMAN, XRD, XCPD, XP00, XRV, XG
!
USE MODE_SBLS
USE MODE_THERMOS
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
!
REAL, DIMENSION(:), INTENT(IN)       :: PTA    ! atmospheric temperature
REAL, DIMENSION(:), INTENT(IN)       :: PTS    ! surface temperature
REAL, DIMENSION(:), INTENT(IN)       :: PQA    ! atmospheric specific humidity
REAL, DIMENSION(:), INTENT(IN)       :: PPA    ! atmospheric level pressure
REAL, DIMENSION(:), INTENT(IN)       :: PRHOA  ! air density
REAL, DIMENSION(:), INTENT(IN)       :: PZONA  ! zonal wind
REAL, DIMENSION(:), INTENT(IN)       :: PMERA  ! meridian wind
REAL, DIMENSION(:), INTENT(IN)       :: PH     ! atmospheric level height
REAL, DIMENSION(:), INTENT(IN)       :: PHW    ! atmospheric level height for wind
REAL, DIMENSION(:), INTENT(IN)       :: PSFZON ! zonal friction
REAL, DIMENSION(:), INTENT(IN)       :: PSFMER ! meridian friction
REAL, DIMENSION(:), INTENT(IN)       :: PSFTH  ! heat flux  (W/m2)
REAL, DIMENSION(:), INTENT(IN)       :: PSFTQ  ! vapor flux (kg/m2/s)
!
REAL, DIMENSION(:), INTENT(OUT)      :: PT2M   ! temperature at 2 meters
REAL, DIMENSION(:), INTENT(OUT)      :: PQ2M   ! specific humidity at 2 meters
REAL, DIMENSION(:), INTENT(OUT)      :: PHU2M  ! relative humidity at 2 meters
REAL, DIMENSION(:), INTENT(OUT)      :: PZON10M! zonal wind component at 10 meters
REAL, DIMENSION(:), INTENT(OUT)      :: PMER10M! meridian wind component at 10 meters
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(SIZE(PTA)) :: ZUSTAR     ! friction
REAL, DIMENSION(SIZE(PTA)) :: ZTH        ! potential temperature
REAL, DIMENSION(SIZE(PTA)) :: ZRV        ! H2O mixing ratio
REAL, DIMENSION(SIZE(PTA)) :: ZLMO       ! Monin Obhukov length
REAL, DIMENSION(SIZE(PTA)) :: ZH_O_LMO   ! h/LMO
REAL, DIMENSION(SIZE(PTA)) :: Z10M_O_LMO ! 10m/LMO
REAL, DIMENSION(SIZE(PTA)) :: Z2M_O_LMO  ! 2m/LMO
REAL, DIMENSION(SIZE(PTA)) :: ZTSTAR     ! Scale of Temperature
REAL, DIMENSION(SIZE(PTA)) :: ZQSTAR     ! Scale of humidity
REAL, DIMENSION(SIZE(PTA)) :: ZTH2M      ! Potential temperature at 2m
REAL, DIMENSION(SIZE(PTA)) :: ZP2M       ! Pressure at 2m
REAL, DIMENSION(SIZE(PTA)) :: ZQSAT2M    ! saturation specific humidity at 2m
REAL                       :: Z10M       ! 10m
REAL                       :: Z2M        ! 2m
REAL, DIMENSION(SIZE(PTA)) :: ZWT        ! potential temperature flux (Km/s)
REAL, DIMENSION(SIZE(PTA)) :: ZWQ        ! water vapor flux           (kg/kg*m/s)
REAL, DIMENSION(SIZE(PTA)) :: ZEXN       ! Exner function
REAL                       :: ZLMOMIN    ! Minimum value of ZLMO for unstable cases
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('PARAM_CLS',0,ZHOOK_HANDLE)
Z10M = 10.
Z2M  = 2.
!
!* friction
!
ZUSTAR(:) = SQRT(SQRT(PSFZON(:)**2+PSFMER(:)**2))
!
!* Exner function
!
ZEXN(:) =  (PPA(:)/XP00)**(XRD/XCPD)
!
!* Potential temperature
!
ZTH(:) = PTA(:) / ZEXN(:)
!
!* Vapor mixing ratio
!
WHERE (PQA(:)/=0.)
  ZRV = 1./(1./PQA(:) - 1.)
ELSEWHERE
  ZRV = 0.
END WHERE
!
!* Kinematic fluxes
!
ZWT(:) = PSFTH(:) / (PRHOA(:) * XCPD / ZEXN(:))
ZWQ(:) = PSFTQ(:) /  PRHOA(:)
!
!
!* Monin Obhukov length
!
ZLMO = LMO(ZUSTAR,ZTH,ZRV,ZWT,ZWQ)
!
! min value of 1 meter for ZLMO for unstable case
!
ZLMOMIN = 1.
ZLMO    = ZLMO * (1.-SIGN(1.,ZLMO))/2. + MAX(ZLMOMIN,ZLMO) * (1.+SIGN(1.,ZLMO))/2.
!
!* h/LMO; 10m/LMO; 2m/LMO
!
ZH_O_LMO   = 0.
Z10M_O_LMO = 0.
Z2M_O_LMO  = 0.
!
WHERE (ZLMO/=XUNDEF)
  Z10M_O_LMO = Z10M/ZLMO
  Z2M_O_LMO  = Z2M/ZLMO
END WHERE
!
!-------------------------------------------------------------------------------
!
!* use of wind forcing height
!
WHERE (ZLMO/=XUNDEF)
  ZH_O_LMO   = PHW/ZLMO
END WHERE
!
!* Wind at 10m
!
!* note : is set to zero value where the law does not apply correctly
!  (e.g. over high mountains)
!         and is set to XUNDEF when forcing level is below 10m diagnostic level
PZON10M(:) = XUNDEF
PMER10M(:) = XUNDEF
WHERE(PHW(:)>=Z10M)
WHERE (PSFZON(:)>=0.) 
  PZON10M(:) = PZONA(:) - SQRT( PSFZON(:))/XKARMAN *(   LOG ( Z10M/PHW)           &
                                                        - PAULSON_PSIM(Z10M_O_LMO)  &
                                                        + PAULSON_PSIM(ZH_O_LMO)    )  
  PZON10M(:) = MIN ( 0., PZON10M(:) )
END WHERE
!
WHERE (PSFZON(:)< 0.) 
  PZON10M(:) = PZONA(:) + SQRT(-PSFZON(:))/XKARMAN *(   LOG ( Z10M/PHW)           &
                                                        - PAULSON_PSIM(Z10M_O_LMO)  &
                                                        + PAULSON_PSIM(ZH_O_LMO)    )  
  PZON10M(:) = MAX ( 0., PZON10M(:) )
END WHERE
!
WHERE (PSFMER(:)>=0.) 
  PMER10M(:) = PMERA(:) - SQRT( PSFMER(:))/XKARMAN *(   LOG ( Z10M/PHW)           &
                                                        - PAULSON_PSIM(Z10M_O_LMO)  &
                                                        + PAULSON_PSIM(ZH_O_LMO)    )  
  PMER10M(:) = MIN ( 0., PMER10M(:) )
END WHERE
!
WHERE (PSFMER(:)< 0.) 
  PMER10M(:) = PMERA(:) + SQRT(-PSFMER(:))/XKARMAN *(   LOG ( Z10M/PHW)           &
                                                        - PAULSON_PSIM(Z10M_O_LMO)  &
                                                        + PAULSON_PSIM(ZH_O_LMO)    )  
  PMER10M(:) = MAX ( 0., PMER10M(:) )
END WHERE
END WHERE
!
!-------------------------------------------------------------------------------
!
!* use of temperature forcing height
!
WHERE (ZLMO/=XUNDEF)
  ZH_O_LMO   = PH/ZLMO
END WHERE
!
!* Temperature scale
!
ZTSTAR(:) = - ZWT(:) / MAX(ZUSTAR,0.01)
!
!* Potential Temperature at 2m
!
ZTH2M(:) = ZTH(:) + 0.74 * ZTSTAR(:)/XKARMAN *(  LOG ( Z2M/PH)             &
                                                 - PAULSON_PSIH(Z2M_O_LMO)   &
                                                 + PAULSON_PSIH(ZH_O_LMO)    )  
!
!* Pressure at 2m
!
ZP2M(:) = PPA(:) - XG * PRHOA(:) * (Z2M-PH(:))
!
!* Temperature at 2m
!
WHERE (ZWT(:) > 0. .OR. PTS(:) == XUNDEF)
  ! Businger formulation in unstable case
  PT2M(:) = ZTH2M(:) * (ZP2M(:)/XP00)**(XRD/XCPD)
ELSEWHERE 
  ! Linear interpolation between Ts and Ta in stable case
  PT2M(:) = PTS(:) + (PTA(:)-PTS(:))*Z2M/PH(:)
END WHERE
!
!-------------------------------------------------------------------------------
!
!* Humidity scale
!
ZQSTAR(:) = - ZWQ(:) / MAX(ZUSTAR,0.01)
!
!* Specific humidity at 2m
!
PQ2M(:) = PQA(:) + 0.74 * ZQSTAR(:)/XKARMAN *(  LOG ( Z2M/PH)             &
                                                - PAULSON_PSIH(Z2M_O_LMO)   &
                                                + PAULSON_PSIH(ZH_O_LMO)    )  
!
!* must be below saturation
!
ZQSAT2M(:) = QSAT(PT2M(:),ZP2M(:))
PQ2M(:) = MIN (ZQSAT2M(:),PQ2M(:))
!
PHU2M(:) = PQ2M(:) / ZQSAT2M(:)
IF (LHOOK) CALL DR_HOOK('PARAM_CLS',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE PARAM_CLS
