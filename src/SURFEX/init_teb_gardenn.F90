!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!#############################################################
SUBROUTINE INIT_TEB_GARDEN_n (DTCO, DGU, UG, U, DGMTO, TOP, GDM, &
                              HPROGRAM,HINIT,KI,KSW,PSW_BANDS,KPATCH)
!#############################################################
!
!!****  *INIT_TEB_GARDEN_n* - routine to initialize ISBA
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
!!      A. Lemonsu  *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    09/2009
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_DIAG_SURF_ATM_n, ONLY : DIAG_SURF_ATM_t
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_DIAG_MISC_TEB_OPTION_n, ONLY : DIAG_MISC_TEB_OPTIONS_t
USE MODD_TEB_OPTION_n, ONLY : TEB_OPTIONS_t
USE MODD_SURFEX_n, ONLY : TEB_GARDEN_MODEL_t
!
USE MODD_TYPE_DATE_SURF
USE MODD_TYPE_SNOW
!

USE MODD_DATA_COVER_PAR,  ONLY: NVEGTYPE
USE MODD_SURF_PAR,        ONLY: XUNDEF, NUNDEF

USE MODD_SURF_ATM,        ONLY: LCPL_ARP
!
USE MODI_GET_LUOUT
USE MODI_READ_PREP_GARDEN_SNOW
USE MODI_ALLOCATE_TEB_GARDEN
USE MODI_ABOR1_SFX
USE MODI_READ_TEB_GARDEN_n
USE MODI_INIT_VEG_GARDEN_n
USE MODI_SOIL_ALBEDO
USE MODI_INIT_FROM_DATA_GRDN_n
USE MODI_AVG_ALBEDO_EMIS_GARDEN
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(DIAG_SURF_ATM_t), INTENT(INOUT) :: DGU
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(DIAG_MISC_TEB_OPTIONS_t), INTENT(INOUT) :: DGMTO
TYPE(TEB_OPTIONS_t), INTENT(INOUT) :: TOP
TYPE(TEB_GARDEN_MODEL_t), INTENT(INOUT) :: GDM
!
 CHARACTER(LEN=6),                   INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
 CHARACTER(LEN=3),                   INTENT(IN)  :: HINIT     ! choice of fields to initialize
INTEGER,                            INTENT(IN)  :: KI        ! number of points
INTEGER,                            INTENT(IN)  :: KSW       ! number of short-wave spectral bands
REAL,             DIMENSION(KSW),   INTENT(IN)  :: PSW_BANDS ! middle wavelength of each band
INTEGER,                            INTENT(IN)  :: KPATCH
!
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER           :: ILUOUT   ! unit of output listing file
!
INTEGER           :: IDECADE  ! decade of simulation
!
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
IF (LHOOK) CALL DR_HOOK('INIT_TEB_GARDEN_N',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
!*       1.     Reading of snow configuration:
!               ------------------------------
!
!* initialization of snow scheme (TSNOW defined in MODD_TEB_GARDEN_n)
!
IF (HINIT=='PRE') THEN
  CALL READ_PREP_GARDEN_SNOW(HPROGRAM,GDM%TGD%CUR%TSNOW%SCHEME,GDM%TGD%CUR%TSNOW%NLAYER)
!
  IF (GDM%TGD%CUR%TSNOW%SCHEME.NE.'3-L' .AND. &
                GDM%TGD%CUR%TSNOW%SCHEME.NE.'CRO' .AND. GDM%TVG%CISBA=='DIF') THEN
    CALL ABOR1_SFX("INIT_TEB_GARDEN_n: WITH CISBA = DIF, CSNOW MUST BE 3-L OR CRO")
  ENDIF
  IF (LHOOK) CALL DR_HOOK('INIT_TEB_GARDEN_N',1,ZHOOK_HANDLE)
  RETURN
ENDIF
!-------------------------------------------------------------------------------
!
 CALL ALLOCATE_TEB_GARDEN(GDM%TGD, GDM%TVG, &
                          KI, GDM%TGDO%NGROUND_LAYER)  
!
!-------------------------------------------------------------------------------
!
IF( GDM%TVG%CCPSURF=='DRY' .AND. LCPL_ARP ) THEN
  CALL ABOR1_SFX('CCPSURF=DRY must not be used with LCPL_ARP')
ENDIF
!
!-------------------------------------------------------------------------------
!
IF (HINIT/='ALL') THEN
  IF (LHOOK) CALL DR_HOOK('INIT_TEB_GARDEN_N',1,ZHOOK_HANDLE)      
  RETURN
ENDIF
!
!-------------------------------------------------------------------------------
!
!*      10.     Prognostic and semi-prognostic fields
!               -------------------------------------
!
!* allocation of urban green area variables
!
!
  YPATCH='   '
  IF (TOP%NTEB_PATCH>1) WRITE(YPATCH,FMT='(A,I1,A)') 'T',KPATCH,'_'
!
  CALL READ_TEB_GARDEN_n(DTCO, DGU, U, GDM, &
                         HPROGRAM,YPATCH)
!
!
 CALL INIT_VEG_GARDEN_n(KI, TOP%LCANOPY, GDM%TVG%CROUGH, GDM%TGD%CUR%TSNOW, &
                   GDM%TVG%CPHOTO, GDM%TGDP%XLAIMIN, GDM%TGDP%XH_TREE, GDM%TGDP%XVEGTYPE, &
                   GDM%TGDPE%CUR%XLAI, GDM%TGDPE%CUR%XZ0, GDM%TGDPE%CUR%XVEG, GDM%TGDPE%CUR%XEMIS, &
                   GDM%TVG%LTR_ML, GDM%TGD%CUR%XFAPARC, GDM%TGD%CUR%XFAPIRC, GDM%TGD%CUR%XLAI_EFFC, &
                   GDM%TGD%CUR%XMUS, GDM%TGDP%XALBNIR_SOIL, GDM%TGDP%XALBVIS_SOIL, &
                   GDM%TGDP%XALBUV_SOIL, GDM%TGDPE%CUR%XALBNIR, GDM%TGDPE%CUR%XALBVIS, &
                   GDM%TGDPE%CUR%XALBUV, DGMTO%LSURF_DIAG_ALBEDO, GDM%TGD%CUR%XPSN, &
                   GDM%TGD%CUR%XPSNG, GDM%TGD%CUR%XPSNV, GDM%TGD%CUR%XPSNV_A, &
                   ZDIR_ALB, ZSCA_ALB, ZEMIS, ZTSRAD )
!
ZWG1(:) = GDM%TGD%CUR%XWG(:,1)
ZTG1(:) = GDM%TGD%CUR%XTG(:,1)
!
IF (.NOT. GDM%TGDO%LPAR_GARDEN) THEN
  CALL SOIL_ALBEDO(GDM%TVG%CALBEDO,                               &
                     GDM%TGDP%XWSAT(:,1),ZWG1,                       &
                     GDM%TGDP%XALBVIS_DRY,GDM%TGDP%XALBNIR_DRY,GDM%TGDP%XALBUV_DRY,    &
                     GDM%TGDP%XALBVIS_WET,GDM%TGDP%XALBNIR_WET,GDM%TGDP%XALBUV_WET,    &
                     GDM%TGDP%XALBVIS_SOIL,GDM%TGDP%XALBNIR_SOIL,GDM%TGDP%XALBUV_SOIL  )  
ELSE
  IF (TOP%TTIME%TDATE%MONTH /= NUNDEF) THEN
    IDECADE = 3 * ( TOP%TTIME%TDATE%MONTH - 1 ) + MIN(TOP%TTIME%TDATE%DAY-1,29) / 10 + 1
  ELSE
    IDECADE = 1
  END IF
  CALL INIT_FROM_DATA_GRDN_n(GDM%DTGD, &
                             IDECADE,GDM%TVG%CPHOTO,              &
                               PALBNIR_SOIL=GDM%TGDP%XALBNIR_SOIL,   &
                               PALBVIS_SOIL=GDM%TGDP%XALBVIS_SOIL,   &
                               PALBUV_SOIL=GDM%TGDP%XALBUV_SOIL      )  
END IF
!
 CALL AVG_ALBEDO_EMIS_GARDEN(GDM%TGD, GDM%TVG%CALBEDO,                  &
                             GDM%TGDPE%CUR%XVEG,GDM%TGDPE%CUR%XZ0,GDM%TGDPE%CUR%XLAI,ZTG1,      &
                             PSW_BANDS,                             &
                             GDM%TGDP%XALBNIR_VEG,GDM%TGDP%XALBVIS_VEG,GDM%TGDP%XALBUV_VEG,     &
                             GDM%TGDP%XALBNIR_SOIL,GDM%TGDP%XALBVIS_SOIL,GDM%TGDP%XALBUV_SOIL,  &
                             GDM%TGDPE%CUR%XEMIS, GDM%TGD%CUR%TSNOW,                            &
                             GDM%TGDPE%CUR%XALBNIR,GDM%TGDPE%CUR%XALBVIS,GDM%TGDPE%CUR%XALBUV,  &
                                 ZDIR_ALB, ZSCA_ALB,                     &
                                 ZEMIS,ZTSRAD                            )  
!
!
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('INIT_TEB_GARDEN_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
!
END SUBROUTINE INIT_TEB_GARDEN_n
