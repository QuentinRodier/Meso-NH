!     #########
      SUBROUTINE ISBA_FLOOD_PROPERTIES(KNI,PTA,PEXNA,PRHOA,PTG,PEXNS,PQA,PVMOD,  &
                                       PZREF,PUREF,PPS,PDIRCOSZW,PVEG,PLAI,      &
                                       PFFLOOD,PFFROZEN,PZ0_FLOOD,PFFG_NOSNOW,   &
                                       PFFV_NOSNOW)  
!     ############################################################################
!
!
!!****  *ISBA_FLOOD_PROPERTIES*  
!!
!!    PURPOSE
!!    -------
!      Calculate the Flood fractions, roughness length and albedo.  
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
!!	B. Decharme           * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original      25/05/08 
!!      Modified      09/2009  B. Decharme: limitation of Ri in surface_ri.F90
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_CSTS,       ONLY : XTT, XG, XCPD
USE MODD_SNOW_PAR,   ONLY : XZ0SN, XZ0HSN
USE MODD_WATER_PAR,  ONLY : XALBSCA_WAT, XEMISWAT, XALBWATICE, XEMISWATICE 
!
USE MODI_SURFACE_RI
USE MODI_SURFACE_CD
USE MODI_SURFACE_CDCH_1DARP
USE MODI_WIND_THRESHOLD
!
USE MODE_SURF_FLOOD_FRAC
USE MODE_THERMOS
!
USE MODD_SURF_ATM,    ONLY : LDRAG_COEF_ARP, XVCHRNK, XVZ0CM
!
USE MODI_PACK_SAME_RANK
USE MODI_UNPACK_SAME_RANK
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_GET_1D_MASK
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!        
INTEGER, INTENT(IN)                  :: KNI
!
REAL, DIMENSION(:), INTENT(IN)       :: PTA   ! air temperature at atm. level
REAL, DIMENSION(:), INTENT(IN)       :: PQA   ! air humidity at atm. level (kg/kg)
REAL, DIMENSION(:), INTENT(IN)       :: PEXNA ! Exner function at atm. level
REAL, DIMENSION(:), INTENT(IN)       :: PRHOA ! air density at atm. level
REAL, DIMENSION(:), INTENT(IN)       :: PVMOD ! module of wind at atm. wind level
REAL, DIMENSION(:), INTENT(IN)       :: PZREF ! atm. level for temp. and humidity
REAL, DIMENSION(:), INTENT(IN)       :: PUREF ! atm. level for wind
REAL, DIMENSION(:), INTENT(IN)       :: PTG   ! Composite Temperature
REAL, DIMENSION(:), INTENT(IN)       :: PEXNS ! Exner function at sea surface
REAL, DIMENSION(:), INTENT(IN)       :: PPS   ! air pressure at sea surface
REAL, DIMENSION(:), INTENT(IN)       :: PDIRCOSZW ! orography slope cosine
REAL, DIMENSION(:), INTENT(IN)       :: PVEG  ! vegetation fraction
REAL, DIMENSION(:), INTENT(IN)       :: PLAI  ! leaf area index
REAL, DIMENSION(:), INTENT(IN)       :: PFFLOOD
REAL, DIMENSION(:), INTENT(IN)       :: PFFROZEN
!
REAL, DIMENSION(:), INTENT(INOUT)    :: PZ0_FLOOD! roughness length over floodplains
!
REAL, DIMENSION(:), INTENT(OUT)      :: PFFG_NOSNOW
REAL, DIMENSION(:), INTENT(OUT)      :: PFFV_NOSNOW
!
!*      0.2    declarations of local variables
!
REAL, PARAMETER :: ZMISS = 0.0
!
INTEGER, DIMENSION(KNI) :: IMASK  ! mask to pack/unpack each variable
!
REAL, DIMENSION(KNI) :: ZUSTAR2   ! square of friction velocity
REAL, DIMENSION(KNI) :: ZAC       ! Aerodynamical conductance
REAL, DIMENSION(KNI) :: ZRA       ! Aerodynamical resistance
REAL, DIMENSION(KNI) :: ZZ0HF     ! heat roughness length over floodplains
REAL, DIMENSION(KNI) :: ZRI       ! Richardson number
REAL, DIMENSION(KNI) :: ZQSAT     ! humidity at saturation
REAL, DIMENSION(KNI) :: ZCD       ! heat drag coefficient
REAL, DIMENSION(KNI) :: ZCDN      ! momentum drag coefficient
REAL, DIMENSION(KNI) :: ZCH       ! neutral momentum drag coefficient
!
!*      0.3    declarations of local pack/unpack variables
!
REAL, DIMENSION(KNI) :: ZZ0_FLOOD
REAL, DIMENSION(KNI) :: ZFFG_NOSNOW
REAL, DIMENSION(KNI) :: ZFFV_NOSNOW
!
!*      0.4    declarations of local packing variables
!
REAL, DIMENSION(KNI) :: ZPSNG
REAL, DIMENSION(KNI) :: ZPSNV
!
REAL, DIMENSION(KNI) :: ZFFLOOD
REAL, DIMENSION(KNI) :: ZFFROZEN
REAL, DIMENSION(KNI) :: ZVEG
REAL, DIMENSION(KNI) :: ZLAI
REAL, DIMENSION(KNI) :: ZVMOD     ! wind modulus
REAL, DIMENSION(KNI) :: ZTG
REAL, DIMENSION(KNI) :: ZPS
REAL, DIMENSION(KNI) :: ZEXNS
REAL, DIMENSION(KNI) :: ZEXNA
REAL, DIMENSION(KNI) :: ZTA
REAL, DIMENSION(KNI) :: ZQA
REAL, DIMENSION(KNI) :: ZZREF
REAL, DIMENSION(KNI) :: ZUREF
REAL, DIMENSION(KNI) :: ZDIRCOSZW
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!       0.     Initializations
!              ---------------
!
IF (LHOOK) CALL DR_HOOK('ISBA_FLOOD_PROPERTIES',0,ZHOOK_HANDLE)
!        
 CALL GET_1D_MASK(KNI,SIZE(PFFLOOD(:)),PFFLOOD(:),IMASK(:))
!
 CALL PACK_SAME_RANK(IMASK(:),PZ0_FLOOD(:),ZZ0_FLOOD(:))
 CALL PACK_SAME_RANK(IMASK(:),PFFLOOD  (:),ZFFLOOD  (:))
 CALL PACK_SAME_RANK(IMASK(:),PFFROZEN (:),ZFFROZEN (:))
 CALL PACK_SAME_RANK(IMASK(:),PVEG     (:),ZVEG     (:))
 CALL PACK_SAME_RANK(IMASK(:),PLAI     (:),ZLAI     (:))
 CALL PACK_SAME_RANK(IMASK(:),PVMOD    (:),ZVMOD    (:))
 CALL PACK_SAME_RANK(IMASK(:),PTG      (:),ZTG      (:))
 CALL PACK_SAME_RANK(IMASK(:),PPS      (:),ZPS      (:))
 CALL PACK_SAME_RANK(IMASK(:),PEXNS    (:),ZEXNS    (:))
 CALL PACK_SAME_RANK(IMASK(:),PEXNA    (:),ZEXNA    (:))
 CALL PACK_SAME_RANK(IMASK(:),PTA      (:),ZTA      (:))
 CALL PACK_SAME_RANK(IMASK(:),PQA      (:),ZQA      (:))
 CALL PACK_SAME_RANK(IMASK(:),PZREF    (:),ZZREF    (:))
 CALL PACK_SAME_RANK(IMASK(:),PUREF    (:),ZUREF    (:))
 CALL PACK_SAME_RANK(IMASK(:),PDIRCOSZW(:),ZDIRCOSZW(:)) 
!
!-------------------------------------------------------------------------------
!
!       1.     Flood fractions without snow
!              ----------------------------
!
ZPSNG(:)=0.0
ZPSNV(:)=0.0
ZFFG_NOSNOW(:) = FLOOD_FRAC_GROUND(ZPSNG,ZFFLOOD)
ZFFV_NOSNOW(:) = FLOOD_FRAC_VEG(ZLAI,ZPSNV,ZFFLOOD)
!
!-------------------------------------------------------------------------------
!
!       2.     roughness length
!              ----------------
!
! * Richardson number (and possible limitation)
!
 CALL SURFACE_RI(ZTG,ZQSAT,ZEXNS,ZEXNA,ZTA,ZQA,  &
                  ZZREF,ZUREF,ZDIRCOSZW,ZVMOD,ZRI)  
!
! * Wind threshold
!
ZVMOD(:)=WIND_THRESHOLD(ZVMOD(:),ZUREF(:))
!
! * Saturated specified humidity near the water surface
!
ZQSAT(:) = QSAT(ZTG(:),ZPS(:))
!
! * Detection of flood ice
!
WHERE(ZFFROZEN(:)==0.0)
  ZZ0HF(:) = ZZ0_FLOOD(:)
ELSEWHERE
  ZZ0HF(:) = XZ0HSN
END WHERE
!
! * Drag coefficient
!
IF (LDRAG_COEF_ARP) THEN
 
  CALL SURFACE_CDCH_1DARP(ZZREF, ZZ0_FLOOD, ZZ0HF, ZVMOD, ZTA, ZTG, &
                          ZQA, ZQSAT, ZCD, ZCDN, ZCH                )  
  !
  ! * Calculate the roughness length over floodplains according to Charnock's expression.
  !
  ZUSTAR2(:) = ZCD(:)*ZVMOD(:)*ZVMOD(:)
  WHERE(ZFFROZEN(:)==0.0)
    ZZ0_FLOOD(:) = XVCHRNK * ZUSTAR2(:) / XG + XVZ0CM * ZCD(:) / ZCDN(:)
  ELSEWHERE
    ZZ0_FLOOD(:) = XZ0SN
  END WHERE

ELSE
  !
  CALL SURFACE_CD(ZRI, ZZREF, ZUREF, ZZ0_FLOOD, ZZ0HF, ZCD, ZCDN)
  !
  ! * Calculate the roughness length over floodplains according to Charnock's expression.
  !
  ZUSTAR2(:) = ZCD(:)*ZVMOD(:)*ZVMOD(:)
  !
  WHERE(ZFFROZEN(:)==0.0)
    ZZ0_FLOOD(:) = XVCHRNK * ZUSTAR2(:) / XG + XVZ0CM * ZCD(:) / ZCDN(:)
  ELSEWHERE
    ZZ0_FLOOD(:) = XZ0SN
  END WHERE
  !
ENDIF
!
 CALL UNPACK_SAME_RANK(IMASK(:),ZZ0_FLOOD(:),PZ0_FLOOD(:),XZ0SN)
!
 CALL UNPACK_SAME_RANK(IMASK(:),ZFFG_NOSNOW(:),PFFG_NOSNOW(:),ZMISS)
 CALL UNPACK_SAME_RANK(IMASK(:),ZFFV_NOSNOW(:),PFFV_NOSNOW(:),ZMISS)
!
IF (LHOOK) CALL DR_HOOK('ISBA_FLOOD_PROPERTIES',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE ISBA_FLOOD_PROPERTIES
