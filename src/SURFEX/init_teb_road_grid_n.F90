!#############################################################
SUBROUTINE INIT_TEB_ROAD_GRID_n(HPROGRAM, TOP, DTCO, DTT, KI)
!#############################################################
!
!!****  *INIT_TEB_ROAD_GRID_n* - routine to initialize the indices for road calculations
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
!!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_TEB_OPTION_n,    ONLY : TEB_OPTIONS_t
USE MODD_DATA_COVER_n,    ONLY : DATA_COVER_t
USE MODD_DATA_TEB_n,      ONLY : DATA_TEB_t
USE MODD_DATA_COVER,      ONLY : XDATA_D_ROAD
!
USE MODI_AV_PGD
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
TYPE(TEB_OPTIONS_t), INTENT(INOUT) :: TOP
TYPE(DATA_COVER_t),  INTENT(INOUT) :: DTCO
TYPE(DATA_TEB_t),    INTENT(INOUT) :: DTT
CHARACTER(LEN=6),    INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
INTEGER,             INTENT(IN)  :: KI        ! number of points
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER :: JL ! loop increment
!
REAL, DIMENSION(KI) :: ZD_COAT_ROAD ! depth of road caoting
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!               Initialisation for IO
!
IF (LHOOK) CALL DR_HOOK('INIT_TEB_ROAD_GRID_n',0,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
!               1. Determines the depth of coating of the road
!                  -------------------------------------------
!
CALL AV_PGD(DTCO, ZD_COAT_ROAD(:), TOP%XCOVER, XDATA_D_ROAD (:,1),'BLD','ARI',TOP%LCOVER)
IF (DTT%LDATA_D_COATING_ROAD)   ZD_COAT_ROAD(:)  = DTT%XPAR_D_COATING_ROAD
!
!-------------------------------------------------------------------------------
!
!               2. Determines the road coating number of layers
!                  --------------------------------------------
!
!
ALLOCATE(TOP%NCOAT_ROAD(KI))
!
TOP%NCOAT_ROAD(:) = 1
DO JL=1,TOP%NTEB_ROAD
  WHERE( ZD_COAT_ROAD(:) >= TOP%XTEB_SOILGRID(JL) - 1.E-6 ) TOP%NCOAT_ROAD(:) = JL
END DO
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('INIT_TEB_ROAD_GRID_n',1,ZHOOK_HANDLE)
!
END SUBROUTINE INIT_TEB_ROAD_GRID_n
