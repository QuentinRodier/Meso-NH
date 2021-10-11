!SFX_LIC Copyright 2004-2019 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SFX_LIC for details. version 1.
!     #################################################################################
SUBROUTINE COUPLING_SURF_ATM_n (YSC, HPROGRAM, HCOUPLING, PTIMEC, PTSTEP, KYEAR, KMONTH,  &
                                KDAY, PTIME, KI, KSV, KSW, PTSUN, PZENITH, PZENITH2,      &
                                PAZIM, PZREF, PUREF, PZS, PU, PV, PQA, PTA, PRHOA, PSV,   &
                                PCO2, PIMPWET,PIMPDRY, HSV, PRAIN, PSNOW, PLW,            & 
                                PDIR_SW, PSCA_SW, PSW_BANDS,                              &
                                PPS, PPA, PSFTQ, PSFTH, PSFTS, PSFCO2, PSFU, PSFV, PTRAD, &
                                PDIR_ALB, PSCA_ALB, PEMIS, PTSURF, PZ0, PZ0H, PQSURF,     &
                                PPEW_A_COEF, PPEW_B_COEF, PPET_A_COEF, PPEQ_A_COEF,       &
                                PPET_B_COEF, PPEQ_B_COEF, PZWS, HTEST           )
!     #################################################################################
!
!!****  *COUPLING_SURF_ATM_n * - Driver to call the schemes for the 
!!       four surface types (SEA, WATER, NATURE, TOWN)
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
!!      Original     01/2004
!!      Modified     09/2011 by S.Queguiner: Add total CO2 surface flux (anthropo+biogenic) as diagnostic
!!      Modified     11/2011 by S.Queguiner: Add total Chemical surface flux (anthropo) as diagnostic
!!      B. Decharme  04/2013 new coupling variables and replace RW_PRECIP_n by CPL_GCM_n
!!      Modified     06/2013 by J.Escobar  : replace DOUBLE PRECISION by REAL to handle problem for promotion of real on IBM SP
!!      R. Séférian  03/2014 Adding decoupling between CO2 seen by photosynthesis and radiative CO2
!!      A. Mary      04/2016 add ORORAD
!!      M. Goret     03/2017 correct unity comment mistakes
!!      P. Wautelet  02/2019 bug correction KI->KSIZE for size of KMASK argument in TREAT_SURF
!!      Bielli S.    02/2019 Sea salt : significant sea wave height influences salt emission; 5 salt modes
!!      R. Schoetter 11/2020 Replace by DUMMY routine passing arguments to coupling_surf_atm_multi_level
!!-------------------------------------------------------------
!
!
USE MODD_SURFEX_n, ONLY : SURFEX_t
USE MODD_PREP_SNOW, ONLY : NIMPUR
!
#ifdef SFX_OL
USE MODN_IO_OFFLINE, ONLY : LFORCIMP
#endif
!
USE MODI_ABOR1_SFX
USE MODI_COUPLING_SURF_ATM_MULTI_LEVEL_N
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ORORAD
!
IMPLICIT NONE
!
#ifdef SFX_MPI
INCLUDE 'mpif.h'
#endif
!
!*      0.1    declarations of arguments
!
TYPE(SURFEX_t), INTENT(INOUT) :: YSC
!
 CHARACTER(LEN=6),    INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
 CHARACTER(LEN=1),    INTENT(IN)  :: HCOUPLING ! type of coupling
                                              ! 'E' : explicit
                                              ! 'I' : implicit
REAL,                INTENT(IN)  :: PTIMEC    ! cumulated time since beginning of simulation
INTEGER,             INTENT(INOUT)  :: KYEAR     ! current year (UTC)
INTEGER,             INTENT(INOUT)  :: KMONTH    ! current month (UTC)
INTEGER,             INTENT(INOUT)  :: KDAY      ! current day (UTC)
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
REAL, DIMENSION(KI,KSW),INTENT(INOUT) :: PDIR_SW ! direct  solar radiation (on horizontal surf.)
!                                             !                                       (W/m2)
REAL, DIMENSION(KI,KSW),INTENT(INOUT) :: PSCA_SW ! diffuse solar radiation (on horizontal surf.)
!                                             !                                       (W/m2)
REAL, DIMENSION(KSW),INTENT(IN)  :: PSW_BANDS ! mean wavelength of each shortwave band (m)
REAL, DIMENSION(KI), INTENT(IN)  :: PZENITH   ! zenithal angle at t  (radian from the vertical)
REAL, DIMENSION(KI), INTENT(IN)  :: PZENITH2  ! zenithal angle at t+1(radian from the vertical)
REAL, DIMENSION(KI), INTENT(IN)  :: PAZIM     ! azimuthal angle      (radian from North, clockwise)
REAL, DIMENSION(KI), INTENT(IN)  :: PLW       ! longwave radiation (on horizontal surf.)
!                                             !                                       (W/m2)
REAL, DIMENSION(KI), INTENT(IN)  :: PPS       ! pressure at atmospheric model surface (Pa)
REAL, DIMENSION(KI), INTENT(IN)  :: PPA       ! pressure at forcing level             (Pa)
REAL, DIMENSION(KI), INTENT(IN)  :: PZWS      ! significant sea wave                  (m)
REAL, DIMENSION(KI), INTENT(IN)  :: PZS       ! atmospheric model orography           (m)
REAL, DIMENSION(KI), INTENT(IN)  :: PCO2      ! CO2 concentration in the air          (kg/m3)
REAL, DIMENSION(KI,NIMPUR), INTENT(IN)  :: PIMPWET      ! 
REAL, DIMENSION(KI,NIMPUR), INTENT(IN)  :: PIMPDRY      !
REAL, DIMENSION(KI), INTENT(IN)  :: PSNOW     ! snow precipitation                    (kg/m2/s)
REAL, DIMENSION(KI), INTENT(IN)  :: PRAIN     ! liquid precipitation                  (kg/m2/s)
!
!
REAL, DIMENSION(KI), INTENT(OUT) :: PSFTH     ! flux of heat                          (W/m2)
REAL, DIMENSION(KI), INTENT(OUT) :: PSFTQ     ! flux of water vapor                   (kg/m2/s)
REAL, DIMENSION(KI), INTENT(OUT) :: PSFU      ! zonal momentum flux                   (Pa)
REAL, DIMENSION(KI), INTENT(OUT) :: PSFV      ! meridian momentum flux                (Pa)
REAL, DIMENSION(KI), INTENT(OUT) :: PSFCO2    ! flux of CO2                           (m/s*kg_CO2/kg_air)
REAL, DIMENSION(KI,KSV),INTENT(OUT):: PSFTS   ! flux of scalar var.                   
!
REAL, DIMENSION(KI), INTENT(INOUT) :: PTRAD     ! radiative temperature                 (K)
REAL, DIMENSION(KI,KSW),INTENT(OUT):: PDIR_ALB  ! direct albedo for each spectral band  (-)
REAL, DIMENSION(KI,KSW),INTENT(OUT):: PSCA_ALB  ! diffuse albedo for each spectral band (-)
REAL, DIMENSION(KI), INTENT(INOUT) :: PEMIS     ! emissivity                            (-)
!
REAL, DIMENSION(KI), INTENT(OUT)   :: PTSURF    ! surface effective temperature         (K)
REAL, DIMENSION(KI), INTENT(INOUT) :: PZ0       ! roughness length for momentum         (m)
REAL, DIMENSION(KI), INTENT(INOUT) :: PZ0H      ! roughness length for heat             (m)
REAL, DIMENSION(KI), INTENT(INOUT) :: PQSURF    ! specific humidity at surface          (kg/kg)
!
REAL, DIMENSION(KI), INTENT(IN) :: PPEW_A_COEF! implicit coefficients
REAL, DIMENSION(KI), INTENT(IN) :: PPEW_B_COEF! needed if HCOUPLING='I'
REAL, DIMENSION(KI), INTENT(IN) :: PPET_A_COEF
REAL, DIMENSION(KI), INTENT(IN) :: PPEQ_A_COEF
REAL, DIMENSION(KI), INTENT(IN) :: PPET_B_COEF
REAL, DIMENSION(KI), INTENT(IN) :: PPEQ_B_COEF
CHARACTER(LEN=2),    INTENT(IN) :: HTEST ! must be equal to 'OK'
!
!
!*      0.2    declarations of local variables
!
INTEGER :: KLEV = 1 ! Number of levels to be coupled, 1 by definition when this routine is called
!
REAL, DIMENSION(KI)  :: ZLW
!
REAL, DIMENSION(KI) :: ZTKE        ! TKE of forcing model, not used for single level coupling
REAL, DIMENSION(KI) :: ZSFTQ_SURF  ! Evaporation at surface, not used for single level coupling
REAL, DIMENSION(KI) :: ZSFTQ_WALL  ! Evaporation at wall level, not used for single level coupling
REAL, DIMENSION(KI) :: ZSFTQ_ROOF  ! Evaporation at roof level, not used for single level coupling
REAL, DIMENSION(KI) :: ZSFTH_SURF  ! Heat flux at surface, not used for single level coupling
REAL, DIMENSION(KI) :: ZSFTH_WALL  ! Heat flux at wall level, not used for single level coupling
REAL, DIMENSION(KI) :: ZSFTH_ROOF  ! Heat flux at roof level, not used for single level coupling
REAL, DIMENSION(KI) :: ZCD_ROOF    ! Drag due to roofs, not used for single level coupling
!
INTEGER         :: JIMP
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('COUPLING_SURF_ATM_N',0,ZHOOK_HANDLE)
!
IF (HTEST/='OK') THEN
  CALL ABOR1_SFX('COUPLING_SURF_ATMN: FATAL ERROR DURING ARGUMENT TRANSFER')
END IF
!
CALL COUPLING_SURF_ATM_MULTI_LEVEL_n (YSC, HPROGRAM, HCOUPLING, PTIMEC, PTSTEP,      &
                  KYEAR, KMONTH, KDAY, PTIME, KI, KSV, KSW, KLEV, PTSUN, PZENITH,      &
                  PZENITH2, PAZIM, PZREF, PUREF, PZS, PU, PV, PQA, PTA, PRHOA, PSV,    &
                  PCO2, PIMPWET, PIMPDRY, HSV, PRAIN, PSNOW, PLW, PDIR_SW, PSCA_SW, PSW_BANDS,&
                  PPS, PPA, ZTKE, PSFTQ, ZSFTQ_SURF, ZSFTQ_WALL, ZSFTQ_ROOF, PSFTH, ZSFTH_SURF,  &
                  ZSFTH_WALL, ZSFTH_ROOF, ZCD_ROOF, PSFTS, PSFCO2, PSFU, PSFV, PTRAD,  &
                  PDIR_ALB, PSCA_ALB, PEMIS, PTSURF, PZ0, PZ0H, PQSURF, PPEW_A_COEF,   &
                  PPEW_B_COEF, PPET_A_COEF, PPEQ_A_COEF, PPET_B_COEF, PPEQ_B_COEF,     &
                  PZWS, HTEST )
!
IF (LHOOK) CALL DR_HOOK('COUPLING_SURF_ATM_N',1,ZHOOK_HANDLE)
!
!=======================================================================================
END SUBROUTINE COUPLING_SURF_ATM_n
