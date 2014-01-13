!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!#############################################################
SUBROUTINE INIT_TEB_GREENROOF_n(HPROGRAM,HINIT,KI,KSW,PSW_BANDS)
!#############################################################
!
!!****  *INIT_TEB_GREENROOF_n* - routine to initialize ISBA
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!	A. Lemonsu  *Meteo France*	
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    09/2009
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_TYPE_DATE_SURF
USE MODD_TYPE_SNOW
!
USE MODD_TEB_n,                ONLY: TTIME, NTEB_PATCH, LCANOPY
USE MODD_TEB_VEG_n,            ONLY: CALBEDO, CCPSURF,  CROUGH, CPHOTO
USE MODD_TEB_GREENROOF_n,      ONLY: CISBA_GR, LTR_ML_GR,                  &
                                     TSNOW, XLAI, XLAIMIN, XZ0, XVEG, XEMIS, &
                                     XALBNIR_SOIL, XALBVIS_SOIL, XALBUV_SOIL, &
                                     XALBNIR, XALBVIS, XALBUV, XWG, XTG,      &
                                     XWSAT, XFAPARC, XFAPIRC, XLAI_EFFC, XMUS,&                                     
                                     XWSAT,                                   &
                                     LPAR_GREENROOF, XVEGTYPE, XH_TREE,       &
                                     NLAYER_GR,                               &
                                     XALBVIS_DRY, XALBNIR_DRY, XALBUV_DRY,    &
                                     XALBNIR_VEG, XALBVIS_VEG, XALBUV_VEG,    &
                                     XALBVIS_WET, XALBNIR_WET, XALBUV_WET,    &
                                     XPSN, XPSNG, XPSNV, XPSNV_A
USE MODD_DIAG_MISC_TEB_n,      ONLY: LSURF_DIAG_ALBEDO
!
USE MODD_DATA_COVER_PAR,       ONLY: NVEGTYPE
USE MODD_SURF_PAR,             ONLY: XUNDEF, NUNDEF
!
USE MODD_SURF_ATM,             ONLY: LCPL_ARP
!
USE MODI_GET_LUOUT
USE MODI_READ_PREP_GREENROOF_SNOW
USE MODI_ALLOCATE_TEB_GREENROOF
USE MODI_ABOR1_SFX
USE MODI_GET_CURRENT_TEB_PATCH
USE MODI_READ_TEB_GREENROOF_n
USE MODI_INIT_VEG_GARDEN_n
USE MODI_SOIL_ALBEDO
USE MODI_INIT_FROM_DATA_GREENROOF_n
USE MODI_AVG_ALBEDO_EMIS_GREENROOF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
 CHARACTER(LEN=6),                   INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
 CHARACTER(LEN=3),                   INTENT(IN)  :: HINIT     ! choice of fields to initialize
INTEGER,                            INTENT(IN)  :: KI        ! number of points
INTEGER,                            INTENT(IN)  :: KSW       ! number of short-wave spectral bands
REAL,             DIMENSION(KSW),   INTENT(IN)  :: PSW_BANDS ! middle wavelength of each band
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER           :: ILUOUT   ! unit of output listing file
!
INTEGER           :: IDECADE  ! decade of simulation
!
INTEGER :: JTEB_PATCH  ! loop counter on TEB patches
 CHARACTER(LEN=3) :: YPATCH ! patch identificator
!
REAL, DIMENSION(KI)               :: ZWG1 ! work array for surface water content
REAL, DIMENSION(KI)               :: ZTG1 ! work array for surface temperature
REAL, DIMENSION(KI,KSW)           :: ZDIR_ALB  ! direct albedo for each band
REAL, DIMENSION(KI,KSW)           :: ZSCA_ALB  ! diffuse albedo for each band
REAL, DIMENSION(KI)               :: ZEMIS     ! emissivity
REAL, DIMENSION(KI)               :: ZTSRAD    ! radiative temperature
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!               Initialisation for IO
!
IF (LHOOK) CALL DR_HOOK('INIT_TEB_GREENROOF_N',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
!-------------------------------------------------------------------------------
!
!*       1.     Reading of snow configuration:
!               ------------------------------
!
!* initialization of snow scheme (TSNOW defined in MODD_TEB_GREENROOF_n)
!
IF (HINIT=='PRE') THEN
   CALL READ_PREP_GREENROOF_SNOW(HPROGRAM,TSNOW%SCHEME,TSNOW%NLAYER)
!
   IF (TSNOW%SCHEME.NE.'3-L' .AND. TSNOW%SCHEME.NE.'CRO' .AND. CISBA_GR=='DIF') THEN
    CALL ABOR1_SFX("INIT_TEB_GREENROOF_n: WITH CISBA_GR = DIF, CSNOW MUST BE 3-L OR CRO")
  ENDIF
  IF (LHOOK) CALL DR_HOOK('INIT_TEB_GREENROOF_N',1,ZHOOK_HANDLE)
  RETURN
ENDIF
!-------------------------------------------------------------------------------
!
 CALL ALLOCATE_TEB_GREENROOF(KI, NLAYER_GR)  
!
!-------------------------------------------------------------------------------
!
IF( CCPSURF=='DRY' .AND. LCPL_ARP ) THEN
  CALL ABOR1_SFX('CCPSURF=DRY must not be used with LCPL_ARP')
ENDIF
!
!-------------------------------------------------------------------------------
!
IF (HINIT/='ALL') THEN
  IF (LHOOK) CALL DR_HOOK('INIT_TEB_GREENROOF_N',1,ZHOOK_HANDLE)      
  RETURN
ENDIF
!
!-------------------------------------------------------------------------------
!
!*       2.     Prognostic and semi-prognostic fields
!               -------------------------------------
!
!* allocation of urban green area variables
!
!
  YPATCH='   '
  CALL GET_CURRENT_TEB_PATCH(JTEB_PATCH)
  IF (NTEB_PATCH>1) WRITE(YPATCH,FMT='(A,I1,A)') 'T',JTEB_PATCH,'_'
!
  CALL READ_TEB_GREENROOF_n(HPROGRAM,YPATCH)
!
!
 CALL INIT_VEG_GARDEN_n(KI, LCANOPY, CROUGH, TSNOW, &
                   CPHOTO, XLAIMIN, XH_TREE, XVEGTYPE, XLAI, XZ0, XVEG, XEMIS, &
                   LTR_ML_GR, XFAPARC, XFAPIRC, XLAI_EFFC, XMUS, &
                   XALBNIR_SOIL, XALBVIS_SOIL, XALBUV_SOIL, XALBNIR, XALBVIS, XALBUV, &
                   LSURF_DIAG_ALBEDO, XPSN, XPSNG, XPSNV, XPSNV_A, &
                   ZDIR_ALB, ZSCA_ALB, ZEMIS, ZTSRAD )
!
ZWG1(:) = XWG(:,1)
ZTG1(:) = XTG(:,1)
!
IF (.NOT. LPAR_GREENROOF) THEN
  CALL SOIL_ALBEDO(CALBEDO,                               &
                     XWSAT(:,1),ZWG1,                       &
                     XALBVIS_DRY,XALBNIR_DRY,XALBUV_DRY,    &
                     XALBVIS_WET,XALBNIR_WET,XALBUV_WET,    &
                     XALBVIS_SOIL,XALBNIR_SOIL,XALBUV_SOIL  )  
ELSE
  IF (TTIME%TDATE%MONTH /= NUNDEF) THEN
    IDECADE = 3 * ( TTIME%TDATE%MONTH - 1 ) + MIN(TTIME%TDATE%DAY-1,29) / 10 + 1
  ELSE
    IDECADE = 1
  END IF
  CALL INIT_FROM_DATA_GREENROOF_n(IDECADE,CPHOTO,              &
                                  PALBNIR_SOIL=XALBNIR_SOIL,   &
                                  PALBVIS_SOIL=XALBVIS_SOIL,   &
                                  PALBUV_SOIL=XALBUV_SOIL      )  
END IF
!
! 
 CALL AVG_ALBEDO_EMIS_GREENROOF(CALBEDO,                                &
                               XVEG,XZ0,XLAI,ZTG1,                     &
                               PSW_BANDS,                              &
                               XALBNIR_VEG,XALBVIS_VEG,XALBUV_VEG,     &
                               XALBNIR_SOIL,XALBVIS_SOIL,XALBUV_SOIL,  &
                               XEMIS,                                  &
                               TSNOW,                                  &
                               XALBNIR,XALBVIS,XALBUV,                 &
                               ZDIR_ALB, ZSCA_ALB,                     &
                               ZEMIS,ZTSRAD                            )  
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('INIT_TEB_GREENROOF_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
!
END SUBROUTINE INIT_TEB_GREENROOF_n
