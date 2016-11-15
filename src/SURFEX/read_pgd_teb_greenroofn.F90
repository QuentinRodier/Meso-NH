!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE READ_PGD_TEB_GREENROOF_n (CHT, DTCO, DTGR, GBGR, U, TGRO, TGRP, TG, &
                                           HPROGRAM,KVERSION)
!     #########################################
!
!!****  *READ_PGD_TEB_GREENROOF_n* - routine to initialise ISBA physiographic variables 
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
!!     based on read_pgd_teb_gardenn
!!
!!    AUTHOR
!!    ------
!!      C. de Munck & A. Lemonsu   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    07/2011 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
!
!
!
USE MODD_CH_TEB_n, ONLY : CH_TEB_t
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_DATA_TEB_GREENROOF_n, ONLY : DATA_TEB_GREENROOF_t
USE MODD_GR_BIOG_GREENROOF_n, ONLY : GR_BIOG_GREENROOF_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_TEB_GREENROOF_OPTION_n, ONLY : TEB_GREENROOF_OPTIONS_t
USE MODD_TEB_GREENROOF_PGD_n, ONLY : TEB_GREENROOF_PGD_t
USE MODD_TEB_GRID_n, ONLY : TEB_GRID_t
!
USE MODD_ISBA_PAR,        ONLY : XOPTIMGRID
!
USE MODI_READ_SURF
USE MODI_READ_PGD_TEB_GREENROOF_PAR_n
!
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_GET_TYPE_DIM_n
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
!
TYPE(CH_TEB_t), INTENT(INOUT) :: CHT
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(DATA_TEB_GREENROOF_t), INTENT(INOUT) :: DTGR
TYPE(GR_BIOG_GREENROOF_t), INTENT(INOUT) :: GBGR
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(TEB_GREENROOF_OPTIONS_t), INTENT(INOUT) :: TGRO
TYPE(TEB_GREENROOF_PGD_t), INTENT(INOUT) :: TGRP
TYPE(TEB_GRID_t), INTENT(INOUT) :: TG
!
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! calling program
INTEGER,           INTENT(IN)  :: KVERSION ! version of SURFEX of the file being read
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER           :: IRESP          ! Error code after redding
!
 CHARACTER(LEN=LEN_HREC) :: YRECFM         ! Name of the article to be read
!
!
INTEGER           :: JLAYER         ! loop counter on layers ! not used
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!* 1D physical dimension
!
IF (LHOOK) CALL DR_HOOK('READ_PGD_TEB_GREENROOF_N',0,ZHOOK_HANDLE)
YRECFM='SIZE_TOWN'
 CALL GET_TYPE_DIM_n(DTCO, U, &
                     'TOWN  ',TG%NDIM)
!
!
!*       2.     Initialisation of ISBA options for greenroofs
!               ---------------------------------------------
!
!
YRECFM='GR_ISBA'
 CALL READ_SURF(&
                HPROGRAM,YRECFM,TGRO%CISBA_GR,IRESP)
!
YRECFM='GR_SCOND'
 CALL READ_SURF(&
                HPROGRAM,YRECFM,TGRO%CSCOND_GR,IRESP)
!
!*       3.     Physiographic data fields:
!               -------------------------
!
!* orographic runoff coefficient
!
ALLOCATE(TGRP%XRUNOFFB_GR(TG%NDIM))
YRECFM='GR_RUNOFFB' 
 CALL READ_SURF(&
                HPROGRAM,YRECFM,TGRP%XRUNOFFB_GR,IRESP)
!
!* subgrid drainage coefficient
!
ALLOCATE(TGRP%XWDRAIN_GR(TG%NDIM))
IF (KVERSION<=6) THEN
  TGRP%XWDRAIN_GR = 0.
ELSE
  YRECFM='GR_WDRAIN'
  CALL READ_SURF(&
                HPROGRAM,YRECFM,TGRP%XWDRAIN_GR,IRESP)
ENDIF
!
!-------------------------------------------------------------------------------
!* biogenic chemical emissions
!
IF (CHT%LCH_BIO_FLUX) THEN
  ALLOCATE(GBGR%XISOPOT(TG%NDIM))
  YRECFM='E_ISOPOT'
  CALL READ_SURF(&
                HPROGRAM,YRECFM,GBGR%XISOPOT,IRESP)
  !
  ALLOCATE(GBGR%XMONOPOT(TG%NDIM))
  YRECFM='E_MONOPOT'
  CALL READ_SURF(&
                HPROGRAM,YRECFM,GBGR%XMONOPOT,IRESP)
ELSE
  ALLOCATE(GBGR%XISOPOT (0))
  ALLOCATE(GBGR%XMONOPOT(0))
END IF
!
!-------------------------------------------------------------------------------
!
!*       4.     Physiographic data fields not to be computed by ecoclimap
!               ---------------------------------------------------------
!
!
!LPAR_GREENROOF = .FALSE.
!IF (KVERSION>=7) THEN
!  YRECFM='PAR_GREENROOF'
!  CALL READ_SURF(HPROGRAM,YRECFM,LPAR_GREENROOF,IRESP)
!END IF
!
!IF (LPAR_GREENROOF) CALL READ_PGD_TEB_GREENROOF_PAR_n(HPROGRAM)
!
 CALL READ_PGD_TEB_GREENROOF_PAR_n(&
                                                       DTGR, TGRO, TG, &
                                                       HPROGRAM)
!
IF (LHOOK) CALL DR_HOOK('READ_PGD_TEB_GREENROOF_N',1,ZHOOK_HANDLE)
!
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE READ_PGD_TEB_GREENROOF_n
