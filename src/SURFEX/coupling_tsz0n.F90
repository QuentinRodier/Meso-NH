!     ###############################################################################
SUBROUTINE COUPLING_TSZ0_n(HPROGRAM, HCOUPLING,                                              &
                 PTSTEP, KYEAR, KMONTH, KDAY, PTIME, KI, KSV, KSW, PTSUN, PZENITH, PZENITH2, &
                 PAZIM, PZREF, PUREF, PZS, PU, PV, PQA, PTA, PRHOA, PSV, PCO2, HSV,          &
                 PRAIN, PSNOW, PLW, PDIR_SW, PSCA_SW, PSW_BANDS, PPS, PPA,                   &
                 PSFTQ, PSFTH, PSFTS, PSFCO2, PSFU, PSFV,                                    &
                 PTRAD, PDIR_ALB, PSCA_ALB, PEMIS,                                           &
                 PPEW_A_COEF, PPEW_B_COEF,                                                   &
                 PPET_A_COEF, PPEQ_A_COEF, PPET_B_COEF, PPEQ_B_COEF,                         &
                 HTEST                                                                       )  
!     ###############################################################################
!
!!****  *COUPLING_TSZ0_n * - Call of fluxes from vegetation scheme ISBA but 
!!        without temporal evolution of the soil/vegetation.
!!
!!    PURPOSE
!!    -------
!
!!**  METHOD
!!    ------
!!
!!    REFERENCE
!!    ---------
!!      
!!
!!    AUTHOR
!!    ------
!!     V. Masson 
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2004
!!      Modified    09/2012 : J. Escobar , SIZE(PTA) not allowed without-interface , replace by KI
!!------------------------------------------------------------------
!
!
USE MODD_SURF_PAR, ONLY : XUNDEF
USE MODD_CSTS,   ONLY : XP00, XRD, XCPD
USE MODD_ISBA_n, ONLY : XTG, XWG, XWGI, XWR, XRESA, TSNOW, NPATCH, NGROUND_LAYER, XWFC
!
USE MODI_TSZ0
USE MODI_COUPLING_ISBA_OROGRAPHY_n
! 
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
 CHARACTER(LEN=6),    INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
 CHARACTER(LEN=1),    INTENT(IN)  :: HCOUPLING ! type of coupling
                                              ! 'E' : explicit
                                              ! 'I' : implicit
INTEGER,             INTENT(IN)  :: KYEAR     ! current year (UTC)
INTEGER,             INTENT(IN)  :: KMONTH    ! current month (UTC)
INTEGER,             INTENT(IN)  :: KDAY      ! current day (UTC)
REAL,                INTENT(IN)  :: PTIME     ! current time since midnight (UTC, s)
INTEGER,             INTENT(IN)  :: KI        ! number of points
INTEGER,             INTENT(IN)  :: KSV       ! number of scalars
INTEGER,             INTENT(IN)  :: KSW       ! number of short-wave spectral bands
REAL, DIMENSION(KI), INTENT(IN)  :: PTSUN     ! solar time                    (s from midnight)
REAL,                INTENT(IN)  :: PTSTEP    ! atmospheric time-step                 (s)
REAL, DIMENSION(KI), INTENT(IN)  :: PZREF     ! height of T,q forcing                 (m)
REAL, DIMENSION(KI), INTENT(IN)  :: PUREF     ! height of wind forcing                (m)
!
REAL, DIMENSION(KI), INTENT(IN)  :: PTA       ! air temperature forcing               (K)
REAL, DIMENSION(KI), INTENT(IN)  :: PQA       ! air humidity forcing                  (kg/m3)
REAL, DIMENSION(KI), INTENT(IN)  :: PRHOA     ! air density                           (kg/m3)
REAL, DIMENSION(KI,KSV),INTENT(IN) :: PSV     ! scalar variables
!                                             ! chemistry:   first char. in HSV: '#'  (molecule/m3)
!                                             !
 CHARACTER(LEN=6), DIMENSION(KSV),INTENT(IN):: HSV  ! name of all scalar variables
REAL, DIMENSION(KI), INTENT(IN)  :: PU        ! zonal wind                            (m/s)
REAL, DIMENSION(KI), INTENT(IN)  :: PV        ! meridian wind                         (m/s)
REAL, DIMENSION(KI,KSW),INTENT(IN) :: PDIR_SW ! direct  solar radiation (on horizontal surf.)
!                                             !                                       (W/m2)
REAL, DIMENSION(KI,KSW),INTENT(IN) :: PSCA_SW ! diffuse solar radiation (on horizontal surf.)
!                                             !                                       (W/m2)
REAL, DIMENSION(KSW),INTENT(IN)  :: PSW_BANDS ! mean wavelength of each shortwave band (m)
REAL, DIMENSION(KI), INTENT(IN)  :: PZENITH   ! zenithal angle at t      (radian from the vertical)
REAL, DIMENSION(KI), INTENT(IN)  :: PZENITH2  ! zenithal angle at t+1    (radian from the vertical)
REAL, DIMENSION(KI), INTENT(IN)  :: PAZIM     ! azimuthal angle      (radian from North, clockwise)
REAL, DIMENSION(KI), INTENT(IN)  :: PLW       ! longwave radiation (on horizontal surf.)
!                                             !                                       (W/m2)
REAL, DIMENSION(KI), INTENT(IN)  :: PPS       ! pressure at atmospheric model surface (Pa)
REAL, DIMENSION(KI), INTENT(IN)  :: PPA       ! pressure at forcing level             (Pa)
REAL, DIMENSION(KI), INTENT(IN)  :: PZS       ! atmospheric model orography           (m)
REAL, DIMENSION(KI), INTENT(IN)  :: PCO2      ! CO2 concentration in the air          (kg/m3)
REAL, DIMENSION(KI), INTENT(IN)  :: PSNOW     ! snow precipitation                    (kg/m2/s)
REAL, DIMENSION(KI), INTENT(IN)  :: PRAIN     ! liquid precipitation                  (kg/m2/s)
!
!
REAL, DIMENSION(KI), INTENT(OUT) :: PSFTH     ! flux of heat                          (W/m2)
REAL, DIMENSION(KI), INTENT(OUT) :: PSFTQ     ! flux of water vapor                   (kg/m2/s)
REAL, DIMENSION(KI), INTENT(OUT) :: PSFU      ! zonal momentum flux                   (Pa)
REAL, DIMENSION(KI), INTENT(OUT) :: PSFV      ! meridian momentum flux                (Pa)
REAL, DIMENSION(KI), INTENT(OUT) :: PSFCO2    ! flux of CO2                           (kg/m2/s)
REAL, DIMENSION(KI,KSV),INTENT(OUT):: PSFTS   ! flux of scalar var.                   (kg/m2/s)
!
REAL, DIMENSION(KI), INTENT(OUT) :: PTRAD     ! radiative temperature                 (K)
REAL, DIMENSION(KI,KSW),INTENT(OUT):: PDIR_ALB! direct albedo for each spectral band  (-)
REAL, DIMENSION(KI,KSW),INTENT(OUT):: PSCA_ALB! diffuse albedo for each spectral band (-)
REAL, DIMENSION(KI), INTENT(OUT) :: PEMIS     ! emissivity                            (-)
!
REAL, DIMENSION(KI), INTENT(IN) :: PPEW_A_COEF! implicit coefficients
REAL, DIMENSION(KI), INTENT(IN) :: PPEW_B_COEF! needed if HCOUPLING='I'
REAL, DIMENSION(KI), INTENT(IN) :: PPET_A_COEF
REAL, DIMENSION(KI), INTENT(IN) :: PPEQ_A_COEF
REAL, DIMENSION(KI), INTENT(IN) :: PPET_B_COEF
REAL, DIMENSION(KI), INTENT(IN) :: PPEQ_B_COEF
 CHARACTER(LEN=2),    INTENT(IN) :: HTEST ! must be equal to 'OK'

!
!*      0.2    declarations of local variables
!
!
REAL, DIMENSION(KI,NGROUND_LAYER,NPATCH) :: ZTG   ! soil temperature
REAL, DIMENSION(KI,NGROUND_LAYER,NPATCH) :: ZWG   ! soil water content
REAL, DIMENSION(KI,NGROUND_LAYER,NPATCH) :: ZWGI  ! soil ice content
REAL, DIMENSION(KI,NPATCH) :: ZWR   ! interception reservoir
REAL, DIMENSION(KI,NPATCH) :: ZRESA ! aerodynamical resistance
REAL, DIMENSION(KI,TSNOW%NLAYER,NPATCH) :: ZWSNOW! snow reservoir
REAL, DIMENSION(KI,TSNOW%NLAYER,NPATCH) :: ZRHOSN! snow density
REAL, DIMENSION(KI,TSNOW%NLAYER,NPATCH) :: ZHEASN! snow heat content
REAL, DIMENSION(KI,NPATCH) :: ZALBSN! snow albedo
REAL, DIMENSION(KI,NPATCH) :: ZEMISN! snow emissivity
!
REAL, DIMENSION(KI)     :: ZPEW_A_COEF ! implicit coefficients
REAL, DIMENSION(KI)     :: ZPEW_B_COEF ! needed if HCOUPLING='I'
REAL, DIMENSION(KI)     :: ZPET_A_COEF
REAL, DIMENSION(KI)     :: ZPEQ_A_COEF
REAL, DIMENSION(KI)     :: ZPET_B_COEF
REAL, DIMENSION(KI)     :: ZPEQ_B_COEF
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('COUPLING_TSZ0_N',0,ZHOOK_HANDLE)
!
!*      1.     Specified evolution of ISBA prognostic variables
!              ------------------------------------------------
!
 CALL TSZ0(PTIME, PTSTEP, XWFC, XTG, XWG)
!
!
!*      2.     Saves the prognostic variables
!              ------------------------------
!
ZTG  (:,:,:) = XTG        (:,:,:)
ZWG  (:,:,:) = XWG        (:,:,:)
ZWGI (:,:,:) = XWGI       (:,:,:)
ZWR  (:,:)   = XWR        (:,:)
ZRESA(:,:)   = XRESA      (:,:)
ZWSNOW(:,:,:)= TSNOW%WSNOW(:,:,:)
ZRHOSN(:,:,:)= TSNOW%RHO  (:,:,:)
ZALBSN(:,:)  = TSNOW%ALB  (:,:)
IF (TSNOW%SCHEME=='3-L' .OR. TSNOW%SCHEME=='CRO') THEN
  ZHEASN(:,:,:)= TSNOW%HEAT (:,:,:)
  ZEMISN(:,:)  = TSNOW%EMIS (:,:)
END IF
!
!*      3.     Explicit coupling only
!              ----------------------
!
ZPET_A_COEF =  XUNDEF
!this modif changes results in MUSC
ZPET_B_COEF =  XUNDEF
ZPEQ_A_COEF =  XUNDEF
ZPEQ_B_COEF =  XUNDEF
ZPEW_A_COEF =  XUNDEF
ZPEW_B_COEF =  XUNDEF
!
!
!*      4.     Call to surface scheme
!              ----------------------
!
 CALL COUPLING_ISBA_OROGRAPHY_n(HPROGRAM, 'E',                                              &
                 0.001, KYEAR, KMONTH, KDAY, PTIME,                                          &
                 KI, KSV, KSW,                                                               &
                 PTSUN, PZENITH, PZENITH2, PAZIM,                                            &
                 PZREF, PUREF, PZS, PU, PV, PQA, PTA, PRHOA, PSV, PCO2, HSV,                 &
                 PRAIN, PSNOW, PLW, PDIR_SW, PSCA_SW, PSW_BANDS, PPS, PPA,                   &
                 PSFTQ, PSFTH, PSFTS, PSFCO2, PSFU, PSFV,                                    &
                 PTRAD, PDIR_ALB, PSCA_ALB, PEMIS,                                           &
                 ZPEW_A_COEF, ZPEW_B_COEF,                                                   &
                 ZPET_A_COEF, ZPEQ_A_COEF, ZPET_B_COEF, ZPEQ_B_COEF,                         &
                 'OK'                                                                        )  
!
!
!*      5.     Removes temporal evolution of ISBA variables
!              --------------------------------------------
!
!
XTG  (:,:,:) = ZTG
XWG  (:,:,:) = ZWG
XWGI (:,:,:) = ZWGI
XWR  (:,:)   = ZWR
XRESA(:,:)   = ZRESA
TSNOW%WSNOW(:,:,:) = ZWSNOW
TSNOW%RHO  (:,:,:) = ZRHOSN
TSNOW%ALB  (:,:)   = ZALBSN
IF (TSNOW%SCHEME=='3-L' .OR. TSNOW%SCHEME=='CRO') THEN
  TSNOW%HEAT (:,:,:) = ZHEASN
  TSNOW%EMIS (:,:)   = ZEMISN
END IF
!
IF (LHOOK) CALL DR_HOOK('COUPLING_TSZ0_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE COUPLING_TSZ0_n
