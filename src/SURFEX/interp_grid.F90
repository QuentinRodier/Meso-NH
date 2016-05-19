!######################
MODULE MODI_INTERP_GRID
!######################

INTERFACE INTERP_GRID

SUBROUTINE INTERP_GRID_1D(PZ1,PT1,PZ2,PT2)
!
REAL, DIMENSION(:,:), INTENT(IN)  :: PZ1  ! input vertical grid
REAL, DIMENSION(:,:), INTENT(IN)  :: PT1  ! input temperatures
REAL, DIMENSION(:),   INTENT(IN)  :: PZ2  ! output vertical grid
REAL, DIMENSION(:,:), INTENT(OUT)  :: PT2  ! output temperatures
!
END SUBROUTINE INTERP_GRID_1D
!
SUBROUTINE INTERP_GRID_2D(PZ1,PT1,PZ2,PT2)
!
REAL, DIMENSION(:,:), INTENT(IN) :: PZ1  ! input vertical grid
REAL, DIMENSION(:,:), INTENT(IN) :: PT1  ! input temperatures
REAL, DIMENSION(:,:), INTENT(IN) :: PZ2  ! output vertical grid
REAL, DIMENSION(:,:), INTENT(OUT) :: PT2  ! output temperatures
!
END SUBROUTINE INTERP_GRID_2D
!
END INTERFACE

END MODULE MODI_INTERP_GRID

!     ##########################################
      SUBROUTINE INTERP_GRID_1D(PZ1,PT1,PZ2,PT2)
!     ##########################################
!!
!!****  *INTERP_GRID* - interpolation on the vertical
!!
!!    PURPOSE
!!    -------
!!
!! input  grid/data is (x,z1)
!! output grid/data is (x,z2)
!!
!!**  METHOD
!!    ------
!!
!!    REFERENCE
!!    ---------
!!      
!!
!!    AUTHOR
!!    ------
!!     V. Masson 
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2004
!!------------------------------------------------------------------
!
USE MODD_SURF_PAR,   ONLY : XUNDEF
USE MODI_COEF_VER_INTERP_LIN_SURF
USE MODI_VER_INTERP_LIN_SURF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!* 0.1 Declaration of dummy arguments
!
REAL, DIMENSION(:,:), INTENT(IN)   :: PZ1  ! input vertical grid
REAL, DIMENSION(:,:), INTENT(IN)   :: PT1  ! input temperatures
REAL, DIMENSION(:),   INTENT(IN)   :: PZ2  ! output vertical grid
REAL, DIMENSION(:,:), INTENT(OUT)  :: PT2  ! output temperatures
!
!* 0.2 Declaration of local variables
!
INTEGER :: JL, JI ! loop counter
REAL,    DIMENSION(SIZE(PZ1,1),SIZE(PZ2)) :: ZZ2      ! output grid
REAL,    DIMENSION(SIZE(PZ1,1),SIZE(PZ2)) :: ZCOEFLIN ! interpolation coefficients
INTEGER, DIMENSION(SIZE(PZ1,1),SIZE(PZ2)) :: IKLIN    ! lower interpolating level of
!                                                     ! grid 1 for each level of grid 2 
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-----------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('MODI_INTERP_GRID:INTERP_GRID_1D',0,ZHOOK_HANDLE)
DO JL=1,SIZE(PZ2)
  ZZ2(:,JL) = PZ2(JL)
END DO
!
 CALL COEF_VER_INTERP_LIN_SURF(PZ1,ZZ2,KKLIN=IKLIN,PCOEFLIN=ZCOEFLIN)
!
!
PT2= VER_INTERP_LIN_SURF(PT1,IKLIN,ZCOEFLIN)
!
!  On reporte le mask sur tous les niveaux
!
DO JL=1,SIZE(PT1,2)
  DO JI=1,SIZE(PT1,1)
    IF (PT1(JI,JL)==XUNDEF) THEN
      PT2(JI,:)=XUNDEF
    ENDIF
  END DO
END DO
IF (LHOOK) CALL DR_HOOK('MODI_INTERP_GRID:INTERP_GRID_1D',1,ZHOOK_HANDLE)
!-----------------------------------------------------------------------------
END SUBROUTINE INTERP_GRID_1D
!
!     ##########################################
      SUBROUTINE INTERP_GRID_2D(PZ1,PT1,PZ2,PT2)
!     ##########################################
!
USE MODD_SURF_PAR,   ONLY : XUNDEF
USE MODI_COEF_VER_INTERP_LIN_SURF
USE MODI_VER_INTERP_LIN_SURF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!* 0.1 Declaration of dummy arguments
!
REAL, DIMENSION(:,:), INTENT(IN)  :: PZ1  ! input vertical grid
REAL, DIMENSION(:,:), INTENT(IN)  :: PT1  ! input temperatures
REAL, DIMENSION(:,:), INTENT(IN)  :: PZ2  ! output vertical grid
REAL, DIMENSION(:,:), INTENT(OUT) :: PT2  ! output temperatures
!
!* 0.2 Declaration of local variables
!
INTEGER :: JL, JI ! loop counter
REAL,    DIMENSION(SIZE(PZ1,1),SIZE(PZ2,2)) :: ZCOEFLIN ! interpolation coefficients
INTEGER, DIMENSION(SIZE(PZ1,1),SIZE(PZ2,2)) :: IKLIN    ! lower interpolating level of
                                                        ! grid 1 for each level of grid 2
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-----------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('MODI_INTERP_GRID:INTERP_GRID_2D',0,ZHOOK_HANDLE)
 CALL COEF_VER_INTERP_LIN_SURF(PZ1,PZ2,IKLIN,ZCOEFLIN)
!
PT2= VER_INTERP_LIN_SURF(PT1,IKLIN,ZCOEFLIN)
!
!  On reporte le mask sur tous les niveaux
!
DO JL=1,SIZE(PT1,2)
  DO JI=1,SIZE(PT1,1)
    IF (PT1(JI,JL)==XUNDEF) THEN
      PT2(JI,:)=XUNDEF
    ENDIF
  END DO
END DO
IF (LHOOK) CALL DR_HOOK('MODI_INTERP_GRID:INTERP_GRID_2D',1,ZHOOK_HANDLE)
!-----------------------------------------------------------------------------
END SUBROUTINE INTERP_GRID_2D
