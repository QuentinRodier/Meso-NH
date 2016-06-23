!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!#############################################################
SUBROUTINE INIT_TEB_GREENROOF_n (DTCO, U, DGMTO, TOP, TVG, GRM, &
                                 HPROGRAM,HINIT,KI,KSW,PSW_BANDS,KPATCH)
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
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_DIAG_MISC_TEB_OPTION_n, ONLY : DIAG_MISC_TEB_OPTIONS_t
USE MODD_TEB_OPTION_n, ONLY : TEB_OPTIONS_t
USE MODD_TEB_VEG_n, ONLY : TEB_VEG_OPTIONS_t
USE MODD_SURFEX_n, ONLY : TEB_GREENROOF_MODEL_t
!
USE MODD_TYPE_DATE_SURF
USE MODD_TYPE_SNOW
!

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
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(DIAG_MISC_TEB_OPTIONS_t), INTENT(INOUT) :: DGMTO
TYPE(TEB_OPTIONS_t), INTENT(INOUT) :: TOP
TYPE(TEB_VEG_OPTIONS_t), INTENT(INOUT) :: TVG
TYPE(TEB_GREENROOF_MODEL_t), INTENT(INOUT) :: GRM
!
 CHARACTER(LEN=6),                   INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
 CHARACTER(LEN=3),                   INTENT(IN)  :: HINIT     ! choice of fields to initialize
INTEGER,                            INTENT(IN)  :: KI        ! number of points
INTEGER,                            INTENT(IN)  :: KSW       ! number of short-wave spectral bands
REAL,             DIMENSION(KSW),   INTENT(IN)  :: PSW_BANDS ! middle wavelength of each band
INTEGER,                            INTENT(IN)  :: KPATCH
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
   CALL READ_PREP_GREENROOF_SNOW(HPROGRAM,GRM%TGR%CUR%TSNOW%SCHEME,GRM%TGR%CUR%TSNOW%NLAYER)
!
   IF (GRM%TGR%CUR%TSNOW%SCHEME.NE.'3-L' .AND. GRM%TGR%CUR%TSNOW%SCHEME.NE.'CRO' &
           .AND. GRM%TGRO%CISBA_GR=='DIF') THEN
    CALL ABOR1_SFX("INIT_TEB_GREENROOF_n: WITH CISBA_GR = DIF, CSNOW MUST BE 3-L OR CRO")
  ENDIF
  IF (LHOOK) CALL DR_HOOK('INIT_TEB_GREENROOF_N',1,ZHOOK_HANDLE)
  RETURN
ENDIF
!-------------------------------------------------------------------------------
!
 CALL ALLOCATE_TEB_GREENROOF(GRM%TGR, TVG, &
                             KI, GRM%TGRO%NLAYER_GR)  
!
!-------------------------------------------------------------------------------
!
IF( TVG%CCPSURF=='DRY' .AND. LCPL_ARP ) THEN
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
  IF (TOP%NTEB_PATCH>1) WRITE(YPATCH,FMT='(A,I1,A)') 'T',KPATCH,'_'
!
  CALL READ_TEB_GREENROOF_n(DTCO, U, TVG, GRM, &
                            HPROGRAM,YPATCH)
!
!
 CALL INIT_VEG_GARDEN_n(KI, TOP%LCANOPY, TVG%CROUGH, GRM%TGR%CUR%TSNOW, &
                   TVG%CPHOTO, GRM%TGRP%XLAIMIN, GRM%TGRP%XH_TREE, GRM%TGRP%XVEGTYPE, &
                   GRM%TGRPE%CUR%XLAI, GRM%TGRPE%CUR%XZ0, GRM%TGRPE%CUR%XVEG, &
                   GRM%TGRPE%CUR%XEMIS, GRM%TGRO%LTR_ML_GR, GRM%TGR%CUR%XFAPARC, &
                   GRM%TGR%CUR%XFAPIRC, GRM%TGR%CUR%XLAI_EFFC, GRM%TGR%CUR%XMUS, &
                   GRM%TGRP%XALBNIR_SOIL, GRM%TGRP%XALBVIS_SOIL, GRM%TGRP%XALBUV_SOIL, &
                   GRM%TGRPE%CUR%XALBNIR, GRM%TGRPE%CUR%XALBVIS, GRM%TGRPE%CUR%XALBUV, &
                   DGMTO%LSURF_DIAG_ALBEDO, GRM%TGR%CUR%XPSN, GRM%TGR%CUR%XPSNG, &
                   GRM%TGR%CUR%XPSNV, GRM%TGR%CUR%XPSNV_A, &
                   ZDIR_ALB, ZSCA_ALB, ZEMIS, ZTSRAD )
!
ZWG1(:) = GRM%TGR%CUR%XWG(:,1)
ZTG1(:) = GRM%TGR%CUR%XTG(:,1)
!
IF (.NOT. GRM%TGRO%LPAR_GREENROOF) THEN
  CALL SOIL_ALBEDO(TVG%CALBEDO,                               &
                     GRM%TGRP%XWSAT(:,1),ZWG1,                       &
                     GRM%TGRP%XALBVIS_DRY,GRM%TGRP%XALBNIR_DRY,GRM%TGRP%XALBUV_DRY,    &
                     GRM%TGRP%XALBVIS_WET,GRM%TGRP%XALBNIR_WET,GRM%TGRP%XALBUV_WET,    &
                     GRM%TGRP%XALBVIS_SOIL,GRM%TGRP%XALBNIR_SOIL,GRM%TGRP%XALBUV_SOIL  )  
ELSE
  IF (TOP%TTIME%TDATE%MONTH /= NUNDEF) THEN
    IDECADE = 3 * ( TOP%TTIME%TDATE%MONTH - 1 ) + MIN(TOP%TTIME%TDATE%DAY-1,29) / 10 + 1
  ELSE
    IDECADE = 1
  END IF
  CALL INIT_FROM_DATA_GREENROOF_n(GRM%DTGR, GRM%TGRO, &
                                  IDECADE,TVG%CPHOTO,              &
                                  PALBNIR_SOIL=GRM%TGRP%XALBNIR_SOIL,   &
                                  PALBVIS_SOIL=GRM%TGRP%XALBVIS_SOIL,   &
                                  PALBUV_SOIL=GRM%TGRP%XALBUV_SOIL      )  
END IF
!
! 
 CALL AVG_ALBEDO_EMIS_GREENROOF(GRM%TGR, &
                                TVG%CALBEDO,                                &
                               GRM%TGRPE%CUR%XVEG,GRM%TGRPE%CUR%XZ0,GRM%TGRPE%CUR%XLAI,ZTG1,  &
                               PSW_BANDS,                              &
                               GRM%TGRP%XALBNIR_VEG,GRM%TGRP%XALBVIS_VEG,GRM%TGRP%XALBUV_VEG,     &
                               GRM%TGRP%XALBNIR_SOIL,GRM%TGRP%XALBVIS_SOIL,GRM%TGRP%XALBUV_SOIL,  &
                               GRM%TGRPE%CUR%XEMIS, GRM%TGR%CUR%TSNOW,                     &
                               GRM%TGRPE%CUR%XALBNIR,GRM%TGRPE%CUR%XALBVIS,GRM%TGRPE%CUR%XALBUV, &
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
