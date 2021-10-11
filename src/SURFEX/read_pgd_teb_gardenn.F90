!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE READ_PGD_TEB_GARDEN_n (OCH_BIO_FLUX, HPARAMBVOC, DTCO, DTV, DTHV, T, GB, U, &
                                        IO, K, KDIM, TOP, HPROGRAM,KVERSION,KBUGFIX)
!     #########################################
!
!!****  *READ_PGD_TEB_GARDEN_n* - routine to initialise ISBA physiographic variables 
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
!!      V. Masson   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2003 
!!      P. Le Moigne  12/2004 : add type of photosynthesis
!!      B. Decharme      2008 : add XWDRAIN
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
!
USE MODD_DATA_ISBA_n, ONLY : DATA_ISBA_t
USE MODD_GR_BIOG_n, ONLY : GR_BIOG_t
!
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
!
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
USE MODD_ISBA_n, ONLY : ISBA_K_t
!
USE MODD_TEB_n, ONLY : TEB_t
USE MODD_TEB_OPTION_n, ONLY : TEB_OPTIONS_t
!
USE MODD_SURF_PAR,        ONLY : XUNDEF, LEN_HREC
USE MODD_ISBA_PAR,        ONLY : XOPTIMGRID
!
USE MODI_READ_PGD_TEB_GARDEN_PAR_n
USE MODI_READ_SURF
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
LOGICAL, INTENT(IN) :: OCH_BIO_FLUX
CHARACTER(LEN=*), INTENT(IN) :: HPARAMBVOC
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(DATA_ISBA_t), INTENT(INOUT) :: DTV, DTHV
TYPE(GR_BIOG_t), INTENT(INOUT) :: GB
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(ISBA_OPTIONS_t), INTENT(INOUT) :: IO
TYPE(ISBA_K_t), INTENT(INOUT) :: K
INTEGER, INTENT(INOUT) :: KDIM
TYPE(TEB_OPTIONS_t), INTENT(INOUT) :: TOP
TYPE(TEB_t), INTENT(INOUT) :: T
!
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! calling program
INTEGER,           INTENT(IN)  :: KVERSION ! version of SURFEX of the file being read
INTEGER,           INTENT(IN)  :: KBUGFIX
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER           :: IRESP          ! Error code after redding
!
 CHARACTER(LEN=LEN_HREC) :: YRECFM         ! Name of the article to be read
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!* 1D physical dimension
!
IF (LHOOK) CALL DR_HOOK('READ_PGD_TEB_GARDEN_N',0,ZHOOK_HANDLE)
YRECFM='SIZE_TOWN'
 CALL GET_TYPE_DIM_n(DTCO, U, 'TOWN  ',KDIM)
!
!

!* orographic runoff coefficient
!
YRECFM='TWN_RUNOFFB'
IF (KVERSION>7 .OR. KVERSION==7 .AND. KBUGFIX>=3) YRECFM='GD_RUNOFFB'
 CALL READ_SURF(HPROGRAM,YRECFM,K%XRUNOFFB,IRESP)
!
!* subgrid drainage coefficient
!
IF (KVERSION<=3) THEN
  K%XWDRAIN = 0.
ELSE
  YRECFM='TWN_WDRAIN'
  IF (KVERSION>7 .OR. KVERSION==7 .AND. KBUGFIX>=3) YRECFM='GD_WDRAIN'
  CALL READ_SURF(HPROGRAM,YRECFM,K%XWDRAIN,IRESP)
ENDIF
!
!-------------------------------------------------------------------------------
!
!* biogenic chemical emissions
!
IF (OCH_BIO_FLUX.AND.HPARAMBVOC=="SOLMON") THEN
  ALLOCATE(GB%XISOPOT(KDIM))
  YRECFM='E_ISOPOT'
  CALL READ_SURF(HPROGRAM,YRECFM,GB%XISOPOT,IRESP)
  !
  ALLOCATE(GB%XMONOPOT(KDIM))
  YRECFM='E_MONOPOT'
  CALL READ_SURF(HPROGRAM,YRECFM,GB%XMONOPOT,IRESP)
ELSE
  ALLOCATE(GB%XISOPOT (0))
  ALLOCATE(GB%XMONOPOT(0))
END IF
!
!-------------------------------------------------------------------------------
!
!*       4.     Physiographic data fields not to be computed by ecoclimap
!               ---------------------------------------------------------
!
IF (KVERSION>=7) THEN
  YRECFM='PAR_GARDEN'
  CALL READ_SURF(HPROGRAM,YRECFM,IO%LPAR,IRESP)
ELSEIF (.NOT.TOP%LECOCLIMAP) THEN
  IO%LPAR = .TRUE.
ELSE
  IO%LPAR = .FALSE.
ENDIF
!
IO%LECOCLIMAP = (.NOT. IO%LPAR)
!
! These flags are set for computations within convert_patch_isba routine
!
CALL SET_FLAG(DTV)
 IF (TOP%CURBTREE/='NONE') &
CALL SET_FLAG(DTHV)
!
!-------------------------------------------------------------------------------
!
! All fields computed from vegetation fractions and LAI of high and low vegetation
! --------------------------------------------------------------------------------
!
IF (IO%LPAR) THEN
  IF (TOP%CURBTREE=='NONE') THEN
    CALL READ_PGD_TEB_GARDEN_PAR_n(DTV, IO, T, TOP, KDIM, HPROGRAM,'MIX ')
  ELSE
    CALL READ_PGD_TEB_GARDEN_PAR_n(DTV,  IO, T, TOP, KDIM, HPROGRAM,'LVEG')
    CALL READ_PGD_TEB_GARDEN_PAR_n(DTHV, IO, T, TOP, KDIM, HPROGRAM,'HVEG',DTV)
  END IF
END IF
!
!-------------------------------------------------------------------------------
!
! Fields individually specified
! -----------------------------
!
! Warning, there could be a lack of coherence with the other parameters if done unwisely
!
!
! Read ecosystem Respiration parameter (kg.m-2.s-1)
!
!IF ((KVERSION==8 .AND. KBUGFIX>=2) .OR. KVERSION>8) THEN
IF (KVERSION>=9) THEN
  YRECFM='L_GD_RE25'
  CALL READ_SURF(HPROGRAM,YRECFM,DTV%LDATA_RE25(1),IRESP)
ELSE
  DTV%LDATA_RE25 = .FALSE.
ENDIF
!
IF (DTV%LDATA_RE25(1)) THEN
   IF (.NOT. ASSOCIATED(DTV%XPAR_RE25)) ALLOCATE(DTV%XPAR_RE25(KDIM,1))
   IF (.NOT. ASSOCIATED(DTHV%XPAR_RE25)) ALLOCATE(DTHV%XPAR_RE25(KDIM,1))
   YRECFM='D_GD_RE25'
   CALL READ_SURF(HPROGRAM,YRECFM,DTV%XPAR_RE25(:,1),IRESP)
   DTHV%XPAR_RE25(:,1)=DTV%XPAR_RE25(:,1) 
ENDIF

!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('READ_PGD_TEB_GARDEN_N',1,ZHOOK_HANDLE)
!
CONTAINS
!
!-------------------------------------------------------------------------------
!
SUBROUTINE SET_FLAG(D)
!
TYPE(DATA_ISBA_t), INTENT(INOUT) :: D
!
! These flags are set for computations within convert_patch_isba routine
!
ALLOCATE(D%LDATA_LAI        (1))
ALLOCATE(D%LDATA_VEG        (1))
ALLOCATE(D%LDATA_Z0         (1))
ALLOCATE(D%LDATA_EMIS       (1))
ALLOCATE(D%LDATA_ALBNIR_VEG (1))
ALLOCATE(D%LDATA_ALBVIS_VEG (1))
ALLOCATE(D%LDATA_ALBUV_VEG  (1))
ALLOCATE(D%LDATA_ALBNIR_SOIL(1))
ALLOCATE(D%LDATA_ALBVIS_SOIL(1))
ALLOCATE(D%LDATA_ALBUV_SOIL (1))
!
IF (.NOT.IO%LPAR) THEN
  D%LDATA_LAI        = .FALSE.
  D%LDATA_VEG        = .FALSE.
  D%LDATA_Z0         = .FALSE.
  D%LDATA_EMIS       = .FALSE.
  D%LDATA_ALBNIR_VEG = .FALSE.
  D%LDATA_ALBVIS_VEG = .FALSE.
  D%LDATA_ALBUV_VEG  = .FALSE.
  D%LDATA_ALBNIR_SOIL= .FALSE.
  D%LDATA_ALBVIS_SOIL= .FALSE.
  D%LDATA_ALBUV_SOIL = .FALSE.
ELSE
  D%LDATA_LAI        = .TRUE.
  D%LDATA_VEG        = .TRUE.
  D%LDATA_Z0         = .TRUE.
  D%LDATA_EMIS       = .TRUE.
  D%LDATA_ALBNIR_VEG = .TRUE.
  D%LDATA_ALBVIS_VEG = .TRUE.
  D%LDATA_ALBUV_VEG  = .TRUE.
  D%LDATA_ALBNIR_SOIL= .TRUE.
  D%LDATA_ALBVIS_SOIL= .TRUE.
  D%LDATA_ALBUV_SOIL = .TRUE.        
ENDIF 

END SUBROUTINE SET_FLAG
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE READ_PGD_TEB_GARDEN_n
