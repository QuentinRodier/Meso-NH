!     #########
      SUBROUTINE READ_PGD_TEB_GARDEN_n(HPROGRAM,KVERSION,KBUGFIX)
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
!!	V. Masson   *Meteo France*	
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
USE MODD_SURF_PAR,        ONLY : XUNDEF
USE MODD_TEB_n,           ONLY : LECOCLIMAP
USE MODD_TEB_GARDEN_n,    ONLY :  XCLAY, XSAND, NGROUND_LAYER,    &
                                  XRUNOFFB, XWDRAIN, LPAR_GARDEN                           
USE MODD_GR_BIOG_GARDEN_n,ONLY : XISOPOT, XMONOPOT
USE MODD_CH_TEB_n,        ONLY : LCH_BIO_FLUX
USE MODD_TEB_GRID_n,      ONLY : NDIM
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
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! calling program
INTEGER,           INTENT(IN)  :: KVERSION ! version of SURFEX of the file being read
INTEGER,           INTENT(IN)  :: KBUGFIX
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER           :: IRESP          ! Error code after redding
!
 CHARACTER(LEN=12) :: YRECFM         ! Name of the article to be read
!
INTEGER           :: JLAYER         ! loop counter on layers
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!* 1D physical dimension
!
IF (LHOOK) CALL DR_HOOK('READ_PGD_TEB_GARDEN_N',0,ZHOOK_HANDLE)
YRECFM='SIZE_TOWN'
 CALL GET_TYPE_DIM_n('TOWN  ',NDIM)
!
!
!* clay fraction : attention, seul un niveau est present dans le fichier
!* on rempli tout les niveaux de  XCLAY avec les valeurs du fichiers
!
ALLOCATE(XCLAY(NDIM,NGROUND_LAYER))
YRECFM='TWN_CLAY'
IF (KVERSION>7 .OR. KVERSION==7 .AND. KBUGFIX>=3) YRECFM='GD_CLAY'
 CALL READ_SURF(HPROGRAM,YRECFM,XCLAY(:,1),IRESP)
DO JLAYER=2,NGROUND_LAYER
  XCLAY(:,JLAYER)=XCLAY(:,1)
END DO
!
!* sand fraction
!
ALLOCATE(XSAND(NDIM,NGROUND_LAYER))
YRECFM='TWN_SAND'
IF (KVERSION>7 .OR. KVERSION==7 .AND. KBUGFIX>=3) YRECFM='GD_SAND'
 CALL READ_SURF(HPROGRAM,YRECFM,XSAND(:,1),IRESP)
DO JLAYER=2,NGROUND_LAYER
  XSAND(:,JLAYER)=XSAND(:,1)
END DO
!
!* orographic runoff coefficient
!
ALLOCATE(XRUNOFFB(NDIM))
YRECFM='TWN_RUNOFFB'
IF (KVERSION>7 .OR. KVERSION==7 .AND. KBUGFIX>=3) YRECFM='GD_RUNOFFB'
 CALL READ_SURF(HPROGRAM,YRECFM,XRUNOFFB,IRESP)
!
!* subgrid drainage coefficient
!
ALLOCATE(XWDRAIN(NDIM))
IF (KVERSION<=3) THEN
  XWDRAIN = 0.
ELSE
  YRECFM='TWN_WDRAIN'
  IF (KVERSION>7 .OR. KVERSION==7 .AND. KBUGFIX>=3) YRECFM='GD_WDRAIN'
  CALL READ_SURF(HPROGRAM,YRECFM,XWDRAIN,IRESP)
ENDIF
!
!-------------------------------------------------------------------------------
!
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
IF (KVERSION>=7) THEN
  YRECFM='PAR_GARDEN'
  CALL READ_SURF(HPROGRAM,YRECFM,LPAR_GARDEN,IRESP)
ELSEIF (.NOT.LECOCLIMAP) THEN
  LPAR_GARDEN = .TRUE.
ELSE
  LPAR_GARDEN = .FALSE.
ENDIF
!
IF (LPAR_GARDEN) CALL READ_PGD_TEB_GARDEN_PAR_n(HPROGRAM)
IF (LHOOK) CALL DR_HOOK('READ_PGD_TEB_GARDEN_N',1,ZHOOK_HANDLE)
!
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE READ_PGD_TEB_GARDEN_n
