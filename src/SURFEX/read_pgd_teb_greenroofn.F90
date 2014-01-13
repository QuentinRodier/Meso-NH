!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
      SUBROUTINE READ_PGD_TEB_GREENROOF_n(HPROGRAM,KVERSION)
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
!!	C. de Munck & A. Lemonsu   *Meteo France*	
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    07/2011 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_TEB_VEG_n,          ONLY : CPEDOTF, CPHOTO, NNBIOMASS
USE MODD_TEB_GREENROOF_n,    ONLY : CISBA_GR, CSCOND_GR, CRUNOFF_GR, &
                                    XRUNOFFB_GR, XWDRAIN_GR,         &
                                    LPAR_GREENROOF
USE MODD_GR_BIOG_GREENROOF_n,ONLY : XISOPOT, XMONOPOT 
USE MODD_CH_TEB_n,           ONLY : LCH_BIO_FLUX    
USE MODD_TEB_GRID_n,         ONLY : NDIM
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
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! calling program
INTEGER,           INTENT(IN)  :: KVERSION ! version of SURFEX of the file being read
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER           :: IRESP          ! Error code after redding
!
 CHARACTER(LEN=12) :: YRECFM         ! Name of the article to be read
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
 CALL GET_TYPE_DIM_n('TOWN  ',NDIM)
!
!
!*       2.     Initialisation of ISBA options for greenroofs
!               ---------------------------------------------
!
!
YRECFM='GR_ISBA'
 CALL READ_SURF(HPROGRAM,YRECFM,CISBA_GR,IRESP)
!
YRECFM='GR_SCOND'
 CALL READ_SURF(HPROGRAM,YRECFM,CSCOND_GR,IRESP)
!
!*       3.     Physiographic data fields:
!               -------------------------
!
!* orographic runoff coefficient
!
ALLOCATE(XRUNOFFB_GR(NDIM))
YRECFM='GR_RUNOFFB' 
 CALL READ_SURF(HPROGRAM,YRECFM,XRUNOFFB_GR,IRESP)
!
!* subgrid drainage coefficient
!
ALLOCATE(XWDRAIN_GR(NDIM))
IF (KVERSION<=6) THEN
  XWDRAIN_GR = 0.
ELSE
  YRECFM='GR_WDRAIN'
  CALL READ_SURF(HPROGRAM,YRECFM,XWDRAIN_GR,IRESP)
ENDIF
!
!-------------------------------------------------------------------------------
!* biogenic chemical emissions
!
IF (LCH_BIO_FLUX) THEN
  ALLOCATE(XISOPOT(NDIM))
  YRECFM='EMIS_ISOPOT'
  CALL READ_SURF(HPROGRAM,YRECFM,XISOPOT,IRESP)
  !
  ALLOCATE(XMONOPOT(NDIM))
  YRECFM='EMIS_MONOPOT'
  CALL READ_SURF(HPROGRAM,YRECFM,XMONOPOT,IRESP)
ELSE
  ALLOCATE(XISOPOT (0))
  ALLOCATE(XMONOPOT(0))
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
 CALL READ_PGD_TEB_GREENROOF_PAR_n(HPROGRAM)
!
IF (LHOOK) CALL DR_HOOK('READ_PGD_TEB_GREENROOF_N',1,ZHOOK_HANDLE)
!
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE READ_PGD_TEB_GREENROOF_n
