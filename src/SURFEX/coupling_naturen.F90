!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     ###############################################################################
SUBROUTINE COUPLING_NATURE_n(HPROGRAM, HCOUPLING, PTIMEC,                                    &
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
!!****  *COUPLING_NATURE_n * - Chooses the surface schemes for natural continental parts  
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
!!--------------------------------------------------------------------
!
USE MODD_SURF_ATM_n, ONLY : CNATURE
USE MODD_CSTS,       ONLY : XTT
!
! 
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_COUPLING_IDEAL_FLUX
!
USE MODI_COUPLING_ISBA_SVAT_n
!
USE MODI_COUPLING_TSZ0_n
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
 CHARACTER(LEN=6),    INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
 CHARACTER(LEN=1),    INTENT(IN)  :: HCOUPLING ! type of coupling
                                              ! 'E' : explicit
                                              ! 'I' : implicit
REAL,                INTENT(IN)  :: PTIMEC    ! cumulated time since beginning of simulation
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
REAL, DIMENSION(KI), INTENT(IN)  :: PZENITH2  ! zenithal angle at t+1      (radian from the vertical)
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
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*      0.2    declarations of local variables
!
!-------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('COUPLING_NATURE_N',0,ZHOOK_HANDLE)
IF (CNATURE=='ISBA  ') THEN
  CALL COUPLING_ISBA_SVAT_n(HPROGRAM, HCOUPLING,                                           &
                 PTSTEP, KYEAR, KMONTH, KDAY, PTIME,                                         &
                 KI,KSV,KSW,                                                                 &
                 PTSUN, PZENITH, PZENITH2, PAZIM,                                            &
                 PZREF, PUREF, PZS, PU, PV, PQA, PTA, PRHOA, PSV, PCO2, HSV,                 &
                 PRAIN, PSNOW, PLW, PDIR_SW, PSCA_SW, PSW_BANDS, PPS, PPA,                   &
                 PSFTQ, PSFTH, PSFTS, PSFCO2, PSFU, PSFV,                                    &
                 PTRAD, PDIR_ALB, PSCA_ALB, PEMIS,                                           &
                 PPEW_A_COEF, PPEW_B_COEF,                                                   &
                 PPET_A_COEF, PPEQ_A_COEF, PPET_B_COEF, PPEQ_B_COEF,                         &
                 'OK'                                                                        )  
ELSE IF (CNATURE=='TSZ0  ') THEN
  CALL COUPLING_TSZ0_n(HPROGRAM, HCOUPLING,                                                &
                 PTSTEP, KYEAR, KMONTH, KDAY, PTIME,                                         &
                 KI,KSV,KSW,                                                                 &
                 PTSUN, PZENITH,  PZENITH2, PAZIM,                                           &
                 PZREF, PUREF, PZS, PU, PV, PQA, PTA, PRHOA, PSV, PCO2, HSV,                 &
                 PRAIN, PSNOW, PLW, PDIR_SW, PSCA_SW, PSW_BANDS, PPS, PPA,                   &
                 PSFTQ, PSFTH, PSFTS, PSFCO2, PSFU, PSFV,                                    &
                 PTRAD, PDIR_ALB, PSCA_ALB, PEMIS,                                           &
                 PPEW_A_COEF, PPEW_B_COEF,                                                   &
                 PPET_A_COEF, PPEQ_A_COEF, PPET_B_COEF, PPEQ_B_COEF,                         &
                 'OK'                                                                        )  
ELSE IF (CNATURE=='FLUX  ') THEN
  CALL COUPLING_IDEAL_FLUX(HPROGRAM, HCOUPLING, PTIMEC,                                      &
                 PTSTEP, KYEAR, KMONTH, KDAY, PTIME,                                         &
                 KI,KSV,KSW,                                                                 &
                 PTSUN, PZENITH, PAZIM,                                                      &
                 PZREF, PUREF, PZS, PU, PV, PQA, PTA, PRHOA, PSV, PCO2, HSV,                 &
                 PRAIN, PSNOW, PLW, PDIR_SW, PSCA_SW, PSW_BANDS, PPS, PPA,                   &
                 PSFTQ, PSFTH, PSFTS, PSFCO2, PSFU, PSFV,                                    &
                 PTRAD, PDIR_ALB, PSCA_ALB, PEMIS,                                           &
                 PPEW_A_COEF, PPEW_B_COEF,                                                   &
                 PPET_A_COEF, PPEQ_A_COEF, PPET_B_COEF, PPEQ_B_COEF,                         &
                 'OK'                                                                        )  
ELSE IF (CNATURE=='NONE  ') THEN
  PSFTH = 0.
  PSFTQ = 0.
  PSFTS = 0.
  PSFU  = 0.
  PSFV  = 0.
  PSFCO2= 0.
!
  PTRAD = XTT
  PDIR_ALB = 0.
  PSCA_ALB = 0.
  PEMIS   = 1.
END IF
IF (LHOOK) CALL DR_HOOK('COUPLING_NATURE_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE COUPLING_NATURE_n
