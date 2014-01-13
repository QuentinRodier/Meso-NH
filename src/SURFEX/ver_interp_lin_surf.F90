!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     ######################
      MODULE MODI_VER_INTERP_LIN_SURF
!     ######################
INTERFACE VER_INTERP_LIN_SURF
!     ##############################################
      FUNCTION VER_INTERP_LIN3D_SURF_BIS(PVAR1,KKLIN,PCOEFLIN) RESULT(PVAR2)
!     ##############################################
!
! third dimension of the arrays is vertical
!
REAL,   DIMENSION(:,:,:), INTENT(IN) :: PVAR1 ! variable values on the initial
!                                             ! grid
INTEGER,DIMENSION(:,:,:), INTENT(IN) :: KKLIN ! lower interpolating level of
!                                             ! grid 1 for each level of grid 2 
REAL,   DIMENSION(:,:,:), INTENT(IN) :: PCOEFLIN ! coefficient for level KKLIN
!
REAL,   DIMENSION(SIZE(KKLIN,1),SIZE(KKLIN,2),SIZE(KKLIN,3)) &
                                       :: PVAR2 ! variable values on target  
!                                             ! grid 
END FUNCTION VER_INTERP_LIN3D_SURF_BIS
!     ##############################################
      FUNCTION VER_INTERP_LIN2D_SURF(PVAR1,KKLIN,PCOEFLIN) RESULT(PVAR2)
!     ##############################################
!
! second dimension of the arrays is vertical
!
REAL,   DIMENSION(:,:),   INTENT(IN) :: PVAR1 ! variable values on the initial
!                                             ! grid
INTEGER,DIMENSION(:,:), INTENT(IN) :: KKLIN ! lower interpolating level of
!                                             ! grid 1 for each level of grid 2 
REAL,   DIMENSION(:,:), INTENT(IN) :: PCOEFLIN ! coefficient for level KKLIN
!
REAL,   DIMENSION(SIZE(KKLIN,1),SIZE(KKLIN,2))                               &
                                       :: PVAR2 ! variable values on target  
!                                             ! grid 
END FUNCTION VER_INTERP_LIN2D_SURF
!     ##############################################
      FUNCTION VER_INTERP_LIN1D_SURF(PVAR1,KKLIN,PCOEFLIN) RESULT(PVAR2)
!     ##############################################
!
! first dimension of the arrays is vertical
!
REAL,   DIMENSION(:), INTENT(IN) :: PVAR1 ! variable values on the initial
!                                         ! grid
INTEGER,DIMENSION(:), INTENT(IN) :: KKLIN ! lower interpolating level of
!                                             ! grid 1 for each level of grid 2 
REAL,   DIMENSION(:), INTENT(IN) :: PCOEFLIN ! coefficient for level KKLIN
!
REAL,   DIMENSION(SIZE(KKLIN)) :: PVAR2 ! variable values on target
!                                         ! grid 
END FUNCTION VER_INTERP_LIN1D_SURF
!
!
END INTERFACE
END MODULE MODI_VER_INTERP_LIN_SURF
!     ##############################################
      FUNCTION VER_INTERP_LIN3D_SURF_BIS(PVAR1,KKLIN,PCOEFLIN) RESULT(PVAR2)
!     ##############################################
!
!!****  *VER_INTERP_LIN* - vertical linear interpolation
!!
!!    PURPOSE
!!    -------
!     This function interpolates the 3D fields from one grid
!     to another using linear interpolation cofficients stored in module
!     MODD_VER_INTERP_LIN.
!
!!
!!**  METHOD
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
!!    Book 2
!!
!!    AUTHOR
!!    ------
!!	
!     V.Masson  Meteo-France
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    18/07/97
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_VER_INTERP_LIN3D_SURF
!
IMPLICIT NONE
!
!*       0.1   Declaration of arguments
!              ------------------------
REAL,   DIMENSION(:,:,:), INTENT(IN) :: PVAR1 ! variable values on the initial
!                                             ! grid
INTEGER,DIMENSION(:,:,:), INTENT(IN) :: KKLIN ! lower interpolating level of
!                                             ! grid 1 for each level of grid 2 
REAL,   DIMENSION(:,:,:), INTENT(IN) :: PCOEFLIN ! coefficient for level KKLIN
!
REAL,   DIMENSION(SIZE(KKLIN,1),SIZE(KKLIN,2),SIZE(KKLIN,3))                   &
                                       :: PVAR2 ! variable values on target  
!                                             ! grid 
!
!*       0.2   Declaration of local variables
!              ------------------------------
!
INTEGER                                               :: JI,JJ,JK2
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('MODI_VER_INTERP_LIN_SURF:VER_INTERP_LIN3D_SURF_BIS',0,ZHOOK_HANDLE)
PVAR2(:,:,:)=VER_INTERP_LIN3D_SURF(PVAR1(:,:,:),KKLIN(:,:,:),PCOEFLIN(:,:,:))
IF (LHOOK) CALL DR_HOOK('MODI_VER_INTERP_LIN_SURF:VER_INTERP_LIN3D_SURF_BIS',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END FUNCTION VER_INTERP_LIN3D_SURF_BIS
!     ##############################################
      FUNCTION VER_INTERP_LIN2D_SURF(PVAR1,KKLIN,PCOEFLIN) RESULT(PVAR2)
!     ##############################################
!
!!****  *VER_INTERP_LIN* - vertical linear interpolation
!!
!!    PURPOSE
!!    -------
!
!!
!!**  METHOD
!!    ------
!!
!!    This routine calls the 3D version of VER_INTERP_LIN after rewritting of
!!    the fields under 3D form.
!!
!!    EXTERNAL
!!    --------
!!
!!    function VER_INTERP_LIN3D
!!    module   MODI_VER_INTERP_LIN3D
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!    Book 2
!!
!!    AUTHOR
!!    ------
!!	
!     V.Masson  Meteo-France
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    17/07/97
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODI_VER_INTERP_LIN3D_SURF
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declaration of arguments
!              ------------------------
REAL,   DIMENSION(:,:), INTENT(IN) :: PVAR1 ! variable values on the initial
!                                           ! grid
INTEGER,DIMENSION(:,:), INTENT(IN) :: KKLIN ! lower interpolating level of
!                                             ! grid 1 for each level of grid 2 
REAL,   DIMENSION(:,:), INTENT(IN) :: PCOEFLIN ! coefficient for level KKLIN
!
REAL,   DIMENSION(SIZE(KKLIN,1),SIZE(KKLIN,2)) :: PVAR2 ! variable values on
!                                                       ! target grid 
!
!*       0.2   Declaration of local variables
!              ------------------------------
!
REAL,   DIMENSION(1,SIZE(PVAR1,1),SIZE(PVAR1,2)) :: ZVAR1 ! variable values on the initial
!                                                         ! grid
REAL,   DIMENSION(1,SIZE(KKLIN,1),SIZE(KKLIN,2)) :: ZVAR2 ! variable values on target
!
INTEGER,DIMENSION(1,SIZE(KKLIN,1),SIZE(KKLIN,2)) :: IKLIN ! lower interpolating level of
!                                             ! grid 1 for each level of grid 2 
REAL,   DIMENSION(1,SIZE(PCOEFLIN,1),SIZE(PCOEFLIN,2)):: ZCOEFLIN ! coefficient for level KKLIN
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('MODI_VER_INTERP_LIN_SURF:VER_INTERP_LIN2D_SURF',0,ZHOOK_HANDLE)
ZVAR1(1,:,:)=PVAR1(:,:)
IKLIN(1,:,:)=KKLIN(:,:)
ZCOEFLIN(1,:,:)=PCOEFLIN(:,:)
!
ZVAR2(:,:,:)=VER_INTERP_LIN3D_SURF(ZVAR1(:,:,:),IKLIN(:,:,:),ZCOEFLIN(:,:,:))
!
PVAR2(:,:)  =ZVAR2(1,:,:)
IF (LHOOK) CALL DR_HOOK('MODI_VER_INTERP_LIN_SURF:VER_INTERP_LIN2D_SURF',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END FUNCTION VER_INTERP_LIN2D_SURF
!     ##############################################
      FUNCTION VER_INTERP_LIN1D_SURF(PVAR1,KKLIN,PCOEFLIN) RESULT(PVAR2)
!     ##############################################
!
!!****  *VER_INTERP_LIN* - vertical linear interpolation
!!
!!    PURPOSE
!!    -------
!
!!
!!**  METHOD
!!    ------
!!
!!    This routine calls the 3D version of VER_INTERP_LIN after rewritting of
!!    the fields under 3D form.
!!
!!    EXTERNAL
!!    --------
!!
!!    function VER_INTERP_LIN3D
!!    module   MODI_VER_INTERP_LIN3D
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!    Book 2
!!
!!    AUTHOR
!!    ------
!!	
!     V.Masson  Meteo-France
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    17/07/97
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODI_VER_INTERP_LIN3D_SURF
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declaration of arguments
!              ------------------------
REAL,   DIMENSION(:), INTENT(IN) :: PVAR1 ! variable values on the initial
!                                         ! grid
INTEGER,DIMENSION(:), INTENT(IN) :: KKLIN ! lower interpolating level of
!                                             ! grid 1 for each level of grid 2 
REAL,   DIMENSION(:), INTENT(IN) :: PCOEFLIN ! coefficient for level KKLIN

REAL,   DIMENSION(SIZE(KKLIN)) :: PVAR2 ! variable values on target
!                                         ! grid 
!
!*       0.2   Declaration of local variables
!              ------------------------------
!
INTEGER,DIMENSION(1,1,SIZE(KKLIN)) :: IKLIN ! lower interpolating level of
!                                             ! grid 1 for each level of grid 2 
REAL,   DIMENSION(1,1,SIZE(PCOEFLIN)) :: ZCOEFLIN ! coefficient for level KKLIN
!
REAL,   DIMENSION(1,1,SIZE(PVAR1)) :: ZVAR1 ! variable values on the initial
!                                           ! grid
REAL,   DIMENSION(1,1,SIZE(KKLIN)) :: ZVAR2 ! variable values on target
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('MODI_VER_INTERP_LIN_SURF:VER_INTERP_LIN1D_SURF',0,ZHOOK_HANDLE)
ZVAR1(1,1,:)=PVAR1(:)
IKLIN(1,1,:)=KKLIN(:)
ZCOEFLIN(1,1,:)=PCOEFLIN(:)
!
ZVAR2(:,:,:)=VER_INTERP_LIN3D_SURF(ZVAR1(:,:,:),IKLIN(:,:,:),ZCOEFLIN(:,:,:))
!
PVAR2(:)    =ZVAR2(1,1,:)
IF (LHOOK) CALL DR_HOOK('MODI_VER_INTERP_LIN_SURF:VER_INTERP_LIN1D_SURF',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END FUNCTION VER_INTERP_LIN1D_SURF
