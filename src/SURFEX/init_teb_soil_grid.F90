!#############################################################
SUBROUTINE INIT_TEB_SOIL_GRID(HPROGRAM, TOP, KI)
!#############################################################
!
!!****  *INIT_TEB_SOIL_GRID* - routine to initialize thermal and hydrological
!!                              characteristics of road and building soil columns
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
!!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_TEB_OPTION_n,    ONLY : TEB_OPTIONS_t
!
USE MODI_AV_PGD
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
USE MODI_ABOR1_SFX
USE MODI_GET_LUOUT
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
TYPE(TEB_OPTIONS_t), INTENT(INOUT) :: TOP
CHARACTER(LEN=6),    INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
INTEGER,             INTENT(IN)  :: KI        ! number of points
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER                 :: ILUOUT         ! Unit name
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!               Initialisation for IO
!
IF (LHOOK) CALL DR_HOOK('INIT_TEB_SOIL_GRID',0,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
!*       1.     Definition of soil vertical discretization for roads
!               ----------------------------------------------------
!
! Now, users define in namelist only the characteristics of structural roads as well as 
! the minimum number of vertical layers required for computation. The final vertical grid
! including structural roads + natural soils below is re-defined here
!
!               1.a   CROAD_GRID='LOW' : low-resolution for structural-road vertical grid
!                     -------------------------------------------------------------------------
!
!
  IF (TOP%CROAD_GRID=='LOW3  ') THEN
      TOP%NTEB_ROAD         = 3
      TOP%NTEB_SOIL         = 3
      ALLOCATE(TOP%XTEB_SOILGRID(TOP%NTEB_SOIL))
      TOP%XTEB_SOILGRID(1) = 0.01
      TOP%XTEB_SOILGRID(2) = 0.1
      TOP%XTEB_SOILGRID(3) = 1.0

  ELSEIF (TOP%CROAD_GRID=='LOW5  ') THEN
      TOP%NTEB_ROAD         = 4
      TOP%NTEB_SOIL         = 5
      ALLOCATE(TOP%XTEB_SOILGRID(TOP%NTEB_SOIL))
      TOP%XTEB_SOILGRID(1) = 0.01
      TOP%XTEB_SOILGRID(2) = 0.1
      TOP%XTEB_SOILGRID(3) = 0.5
      TOP%XTEB_SOILGRID(4) = 1.0
      TOP%XTEB_SOILGRID(5) = 3.0
!
!
!               2.    CROAD_GRID='MEDIUM' : medium-resolution for structural-road vertical grid
!                     -------------------------------------------------------------------------
!
  ELSE IF (TOP%CROAD_GRID=='MEDIUM') THEN 
    TOP%NTEB_ROAD     = 9
    TOP%NTEB_SOIL     = 12
    ALLOCATE(TOP%XTEB_SOILGRID(TOP%NTEB_SOIL))
    TOP%XTEB_SOILGRID(:) = (/0.001 ,0.01  ,0.05  ,0.10  ,0.15  ,0.20  ,&
                             0.30  ,0.60  ,1.00  ,1.50  ,2.00  ,3.00  /)
!
!               3.c   CROAD_GRID='HIGH  ' : high-resolution for structural-road vertical grid
!                     -------------------------------------------------------------------------
!
  ELSE IF (TOP%CROAD_GRID=='HIGH  ') THEN
    TOP%NTEB_ROAD     = 11
    TOP%NTEB_SOIL     = 14
    ALLOCATE(TOP%XTEB_SOILGRID(TOP%NTEB_SOIL))
    TOP%XTEB_SOILGRID(:) = (/0.001 ,0.005 ,0.0435,0.0820,0.210 ,0.338 ,& !! ==> to verify if we keep this grid definition (see Bouilloud)
                             0.466 ,0.594 ,0.768 ,0.80  ,1.00  ,1.50  ,&
                             2.00  ,3.00                              /)
!
!               4.    Other cases
!                     -----------
!
  ELSE
    CALL GET_LUOUT(HPROGRAM,ILUOUT)
    WRITE(ILUOUT,*) " "
    WRITE(ILUOUT,*) "In init_teb_soil_grid : No rule implemented for this road grid type"
    WRITE(ILUOUT,*) "TOP%CROAD_GRID ",TOP%CROAD_GRID
    CALL FLUSH(ILUOUT)
    CALL ABOR1_SFX("INIT_TEB_SOIL_GRID: No rule implemented for this road grid type, check report")
  ENDIF
!
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('INIT_TEB_SOIL_GRID',1,ZHOOK_HANDLE)
!
END SUBROUTINE INIT_TEB_SOIL_GRID
