!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
      SUBROUTINE COEF_VER_INTERP_LIN_SURF(PZ1,PZ2,KKLIN,PCOEFLIN)
!     ###############################################################
!
!!****  *VER_INTERP_LIN* - vertical linear interpolation
!!
!!    PURPOSE
!!    -------
!     This function computes the interpolation coefficient XCOEFLIN
!     of the level XKLIN of grid PZ1 which is just under the points of
!     grid PZ2 (respectively called hereafter 'initial' and 'target'), 
!     in order to perform linear interpolations between these 2 grids.
!
!     CAUTION:
!     * The interpolation occurs on the WHOLE grid. Therefore, one must
!     only give as argument to this function the inner points of the domain,
!     particularly for the vertical grid, where there is no physical information
!     under the ground or at and over H.
!     * The level numbers must increase from bottom to top.
!!
!!**  METHOD
!!    ------
!!    two extrapolations are possible: with the two or four nearest points.
!!
!!   Interpolation with 2 points:
!!
!!    If there is less than two points on one side, the interpolation is linear.
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
!!
!!    AUTHOR
!!    ------
!!	
!     V.Masson  Meteo-France
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    18/07/97
!!                  20/01/98  use explicit arguments
!!      P Jabouille 20/12/02  no extrapolation under the ground
!!      S. Malardel 11/2003   bug of no extrapolation under the ground
!!      V. Masson   10/2003   no extrapolation above top
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declaration of arguments
!              ------------------------
REAL,   DIMENSION(:,:), INTENT(IN)   :: PZ1   ! altitudes of the points of the
!                                             ! initial grid 
REAL,   DIMENSION(:,:), INTENT(IN)   :: PZ2   ! altitudes of the points of the
!                                             ! target grid 
INTEGER, DIMENSION(:,:), INTENT(OUT) :: KKLIN ! number of the level
                                              ! of the data to be interpolated
!
REAL,    DIMENSION(:,:), INTENT(OUT):: PCOEFLIN ! interpolation
                                              ! coefficient
!
!
!*       0.2   Declaration of local variables
!              ------------------------------
!
LOGICAL,DIMENSION(SIZE(PZ1,1),SIZE(PZ1,2))            :: GLEVEL
INTEGER                                               :: JK2,JI
INTEGER,DIMENSION(SIZE(PZ1,1))                        :: ILEVEL
INTEGER,DIMENSION(SIZE(PZ1,1))                        :: IUNDER
REAL                                                  :: ZEPS ! a small number
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('COEF_VER_INTERP_LIN_SURF',0,ZHOOK_HANDLE)
ZEPS=1.E-12
!
!-------------------------------------------------------------------------------
!
!*       2.    LOOP ON THE TARGET VERTICAL GRID
!              --------------------------------
!
DO JK2=1,SIZE(PZ2,2)
!
!-------------------------------------------------------------------------------
!
!*       3.    Determination of the initial level under the target level JK2
!              -------------------------------------------------------------
!
  GLEVEL(:,:)=PZ1(:,:)<=SPREAD(PZ2(:,JK2),2,SIZE(PZ1,2)) *(1.-ZEPS)
  ILEVEL(:)  =COUNT(GLEVEL(:,:),2)
!
!* linear extrapolation under the ground
  IUNDER=ILEVEL
  ILEVEL(:)=MAX(ILEVEL(:),1)
!
!* linear extrapolation above the uppest level
  ILEVEL(:)=MIN(ILEVEL(:),SIZE(PZ1,2)-1)
!
  KKLIN(:,JK2)=ILEVEL(:)

!-------------------------------------------------------------------------------
!
!*       4.    Linear interpolation coefficients
!              ---------------------------------
!
  DO JI=1,SIZE(PZ1,1)
    IF (PZ1(JI,ILEVEL(JI))==PZ1(JI,ILEVEL(JI)+1)) THEN
      PCOEFLIN(JI,JK2)= 0.
    ELSE
      PCOEFLIN(JI,JK2)=(PZ2(JI,JK2)-PZ1(JI,ILEVEL(JI)+1))              &
                        /(PZ1(JI,ILEVEL(JI))-PZ1(JI,ILEVEL(JI)+1))   
    END IF
    IF (IUNDER(JI) < 1 )           PCOEFLIN(JI,JK2)=1.                       ! no extrapolation
    IF (ILEVEL(JI)==SIZE(PZ1,2)-1) PCOEFLIN(JI,JK2)=MAX(PCOEFLIN(JI,JK2),0.) ! no extrapolation
  ENDDO
!
!-------------------------------------------------------------------------------
!
END DO
IF (LHOOK) CALL DR_HOOK('COEF_VER_INTERP_LIN_SURF',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE COEF_VER_INTERP_LIN_SURF
