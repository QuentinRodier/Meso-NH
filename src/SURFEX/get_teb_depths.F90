!     #########
      SUBROUTINE GET_TEB_DEPTHS(HFILEPGDTYPE, PD_ROOF, PD_ROAD, PD_WALL, PD_FLOOR)
!     ##############################################################
!
!!**** *CONVERT_COVER* 
!!
!!    PURPOSE
!!    -------
!!
!!    METHOD
!!    ------
!!   
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!
!!    V. Masson        Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    01/2004
!     
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_DATA_COVER,     ONLY : XDATA_D_ROOF, XDATA_D_ROAD, XDATA_D_WALL, XDATA_D_FLOOR
USE MODD_DATA_COVER_PAR, ONLY : JPCOVER, NDATA_ROOF_LAYER, NDATA_ROAD_LAYER, &
                                NDATA_WALL_LAYER, NDATA_FLOOR_LAYER
!
USE MODI_READ_SURF
USE MODI_AV_PGD
USE MODI_OLD_NAME
USE MODI_THERMAL_LAYERS_CONF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABOR1_SFX
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
 CHARACTER(LEN=6),   INTENT(IN)  :: HFILEPGDTYPE ! type of input file
!
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL   :: PD_ROOF
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL   :: PD_ROAD
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL   :: PD_WALL
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL   :: PD_FLOOR
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
LOGICAL, DIMENSION(JPCOVER)          :: GCOVER ! flag to read the covers
REAL,    DIMENSION(:,:), ALLOCATABLE :: ZCOVER ! cover fractions
REAL,    DIMENSION(:,:), ALLOCATABLE :: ZD     ! depth of surface layers
REAL,    DIMENSION(:,:), ALLOCATABLE :: ZPAR_D ! depth of data_surface layers
REAL,    DIMENSION(:,:), ALLOCATABLE :: ZPAR_HC, ZPAR_TC, ZHC, ZTC ! work arrays
!
INTEGER           :: IVERSION       ! surface version
INTEGER           :: IBUGFIX        ! surface bugfix version
 CHARACTER(LEN=5)  :: YSURF          ! Type of surface
 CHARACTER(LEN=12) :: YRECFM         ! Name of the article to be read
 CHARACTER(LEN=12) :: YRECFM0        ! Name of the article to be read
 CHARACTER(LEN=12) :: YRECFM1        ! Name of the article to be read
 CHARACTER(LEN=12) :: YRECFM2        ! Name of the article to be read
 CHARACTER(LEN=3)  :: YAREA          ! Area where field is to be averaged
INTEGER           :: IRESP          ! reading return code
LOGICAL           :: GDATA          ! T if depth is to be read in the file
REAL, DIMENSION(SIZE(XDATA_D_ROOF,1),SIZE(XDATA_D_ROOF,2)) :: ZDATA
INTEGER :: ILAYER                   ! number of surface layers
INTEGER :: JLAYER                   ! loop counter on surface layers
INTEGER :: IPAR_LAYER               ! number of data surface layers
INTEGER :: IDATA_LAYER              ! number of data surface layers from ecoclimap
INTEGER :: ILU                      ! number of points
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!*    2.      SECONDARY VARIABLES
!             -------------------
!
!*    2.2     fields on artificial surfaces only
!             ----------------------------------
!
IF (LHOOK) CALL DR_HOOK('GET_TEB_DEPTHS',0,ZHOOK_HANDLE)
!
YRECFM='VERSION'
 CALL READ_SURF(HFILEPGDTYPE,YRECFM,IVERSION,IRESP)
YRECFM='BUG'
 CALL READ_SURF(HFILEPGDTYPE,YRECFM,IBUGFIX,IRESP)
!
IF (PRESENT(PD_ROOF)) THEN
  YSURF='ROOF '
  ZDATA = XDATA_D_ROOF
  YRECFM0 = 'PAR_RF_LAYER'
  YRECFM1 = 'L_D_ROOF'
  YRECFM2 = 'D_D_ROOF'
  IDATA_LAYER = NDATA_ROOF_LAYER
  ILU     = SIZE(PD_ROOF,1)
  ILAYER  = SIZE(PD_ROOF,2)
  YAREA   = 'BLD'
END IF
IF (PRESENT(PD_WALL)) THEN
  YSURF='WALL '
  ZDATA = XDATA_D_WALL
  YRECFM0 = 'PAR_WL_LAYER'
  YRECFM1 = 'L_D_WALL'
  YRECFM2 = 'D_D_WALL'
  IDATA_LAYER = NDATA_WALL_LAYER
  ILU     = SIZE(PD_WALL,1)
  ILAYER  = SIZE(PD_WALL,2)
  YAREA   = 'BLD'
END IF
IF (PRESENT(PD_ROAD)) THEN
  YSURF='ROAD '
  ZDATA = XDATA_D_ROAD
  YRECFM0 = 'PAR_RD_LAYER'
  YRECFM1 = 'L_D_ROAD'
  YRECFM2 = 'D_D_ROAD'
  IDATA_LAYER = NDATA_ROAD_LAYER
  ILU     = SIZE(PD_ROAD,1)
  ILAYER  = SIZE(PD_ROAD,2)
  YAREA   = 'STR'
END IF
IF (PRESENT(PD_FLOOR)) THEN
  YSURF='FLOOR'
  ZDATA = XDATA_D_FLOOR
  YRECFM0 = 'PAR_FL_LAYER'
  YRECFM1 = 'L_D_FLOOR'
  YRECFM2 = 'D_D_FLOOR'
  IDATA_LAYER = NDATA_FLOOR_LAYER
  ILU     = SIZE(PD_FLOOR,1)
  ILAYER  = SIZE(PD_FLOOR,2)
  YAREA   = 'BLD'
END IF

ALLOCATE(ZD(ILU,ILAYER))
!
!* read if the depths description are written in the file
IF (IVERSION<7 .OR. (IVERSION==7 .AND. IBUGFIX<=2)) THEN
  GDATA = .FALSE.
ELSE
  CALL READ_SURF(HFILEPGDTYPE,YRECFM1,GDATA,IRESP)
END IF
!
!* depths are read in the file
IF (GDATA) THEN
  !* gets number of data layers
  CALL READ_SURF(HFILEPGDTYPE,YRECFM0,IPAR_LAYER,IRESP)
  !* gets the data layers depths
  ALLOCATE(ZPAR_D(ILU,IPAR_LAYER))
  DO JLAYER=1,IPAR_LAYER
    WRITE(YRECFM,FMT='(A,I1)') TRIM(YRECFM2),JLAYER
    CALL READ_SURF(HFILEPGDTYPE,YRECFM,ZPAR_D(:,JLAYER),IRESP,HDIR='A')
  END DO
!
ELSE
!* depths are deduced from the cover types
  ALLOCATE(ZPAR_D(ILU,IDATA_LAYER))
  !* reading of the cover to obtain the thickness of layers
  CALL OLD_NAME(HFILEPGDTYPE,'COVER_LIST      ',YRECFM)
  CALL READ_SURF(HFILEPGDTYPE,YRECFM,GCOVER(:),IRESP,HDIR='-')
  !* reading of the cover fractions
  ALLOCATE(ZCOVER(ILU,JPCOVER))
  YRECFM='COVER'
  CALL READ_SURF(HFILEPGDTYPE,YRECFM,ZCOVER(:,:),GCOVER,IRESP,HDIR='A')
  !
  !* deduces the depths of each layer
  DO JLAYER=1,IDATA_LAYER
    CALL AV_PGD (ZPAR_D(:,JLAYER), ZCOVER, ZDATA(:,JLAYER),YAREA,'ARI')
  END DO
  DEALLOCATE(ZCOVER)
ENDIF
!
!* recomputes the grid from the available data
!
IF (IVERSION<7 .OR. (IVERSION==7 .AND. IBUGFIX<=2)) THEN
  !* in old version of TEB, the computational grid was equal to the data grid
  ZD(:,:) = ZPAR_D(:,:)
ELSE
  !* recomputes the grid from the available data
  ALLOCATE(ZPAR_HC(ILU,SIZE(ZPAR_D,2)))
  ALLOCATE(ZPAR_TC(ILU,SIZE(ZPAR_D,2)))
  ALLOCATE(ZTC    (ILU,ILAYER))
  ALLOCATE(ZHC    (ILU,ILAYER))
  ZPAR_HC = 1.E6  ! not physically used
  ZPAR_TC = 1.    ! not physically used
  CALL THERMAL_LAYERS_CONF(YSURF,ZPAR_HC,ZPAR_TC,ZPAR_D,ZHC,ZTC,ZD)
  DEALLOCATE(ZPAR_HC)
  DEALLOCATE(ZPAR_TC)
  DEALLOCATE(ZHC)
  DEALLOCATE(ZTC)
END IF
!
IF (PRESENT(PD_ROOF )) PD_ROOF  = ZD
IF (PRESENT(PD_WALL )) PD_WALL  = ZD
IF (PRESENT(PD_ROAD )) PD_ROAD  = ZD
IF (PRESENT(PD_FLOOR)) PD_FLOOR = ZD
!
DEALLOCATE(ZD)
!
IF (LHOOK) CALL DR_HOOK('GET_TEB_DEPTHS',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE GET_TEB_DEPTHS
