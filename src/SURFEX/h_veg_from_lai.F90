!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!#######################
MODULE MODI_H_VEG_FROM_LAI
!#######################
!
INTERFACE H_VEG_FROM_LAI
!
    FUNCTION H_VEG_FROM_LAI_0D(PLAI,PH_TREE,PVEGTYPE,NPAR_VEG_IRR_USE) RESULT(PH_VEG)
!
REAL,                 INTENT(IN) :: PLAI         ! Leaf area Index
REAL,                 INTENT(IN) :: PH_TREE      ! height of trees
REAL,   DIMENSION(:), INTENT(IN) :: PVEGTYPE     ! type of vegetation
INTEGER,DIMENSION(:), INTENT(IN) :: NPAR_VEG_IRR_USE ! vegtype with irrigation
!
REAL                             :: PH_VEG       ! vegetation height
!
END FUNCTION H_VEG_FROM_LAI_0D
!
!
    FUNCTION H_VEG_FROM_LAI_1D(PLAI,PH_TREE,PVEGTYPE,NPAR_VEG_IRR_USE) RESULT(PH_VEG)
!
REAL,   DIMENSION(:),   INTENT(IN) :: PLAI         ! Leaf area Index
REAL,   DIMENSION(:),   INTENT(IN) :: PH_TREE      ! height of trees
REAL,   DIMENSION(:,:), INTENT(IN) :: PVEGTYPE     ! type of vegetation
INTEGER,DIMENSION(:), INTENT(IN) :: NPAR_VEG_IRR_USE ! vegtype with irrigation
!
REAL,   DIMENSION(SIZE(PLAI))      :: PH_VEG       ! vegetation height
!
END FUNCTION H_VEG_FROM_LAI_1D
!
!
    FUNCTION H_VEG_FROM_LAI_2D(PLAI,PH_TREE,PVEGTYPE) RESULT(PH_VEG)
!
REAL,   DIMENSION(:,:),   INTENT(IN) :: PLAI         ! Leaf area Index
REAL,   DIMENSION(:,:),   INTENT(IN) :: PH_TREE      ! height of trees
REAL,   DIMENSION(:,:,:), INTENT(IN) :: PVEGTYPE     ! type of vegetation
!
REAL,   DIMENSION(SIZE(PLAI,1),SIZE(PLAI,2)) :: PH_VEG  ! vegetation height
!
END FUNCTION H_VEG_FROM_LAI_2D
!
END INTERFACE
!
END MODULE MODI_H_VEG_FROM_LAI
!

!   ###########################################################
    FUNCTION H_VEG_FROM_LAI_0D(PLAI,PH_TREE,PVEGTYPE,NPAR_VEG_IRR_USE) RESULT(PH_VEG)
!   ###########################################################
!!
!!    PURPOSE
!!    -------
!
!     Calculates vegetation height from leaf
!    area index and type of vegetation
!    (most of types; forest and vineyards; grassland)
!              
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!    none
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!      
!!    none
!!
!!    REFERENCE
!!    ---------
!!
!!      
!!    AUTHOR
!!    ------
!!
!!      B. Decharme          * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2021
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODI_VEG_HEIGHT_FROM_LAI
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL,                 INTENT(IN) :: PLAI         ! Leaf area Index
REAL,                 INTENT(IN) :: PH_TREE      ! height of trees
REAL,   DIMENSION(:), INTENT(IN) :: PVEGTYPE     ! type of vegetation
INTEGER,DIMENSION(:), INTENT(IN) :: NPAR_VEG_IRR_USE ! vegtype with irrigation
!
REAL                             :: PH_VEG          ! vegetation height
!
!*      0.2    declarations of local variables
!
!
REAL, DIMENSION(SIZE(PVEGTYPE)) :: ZH_VEG      ! height for each type
REAL                            :: ZAVG_H      ! averaged height
REAL                            :: ZWEIGHT       
!
INTEGER                         :: JTYPE       ! loop counter
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-----------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('MODI_H_VEG_FROM_LAI:H_VEG_FROM_LAI_0D',0,ZHOOK_HANDLE)
!
PH_VEG = 0.0
!
ZH_VEG(:) = VEG_HEIGHT_FROM_LAI(PLAI,PH_TREE,PVEGTYPE,NPAR_VEG_IRR_USE)
!
ZWEIGHT= 0.0
ZAVG_H = 0.0
DO JTYPE=4,SIZE(PVEGTYPE)
  ZWEIGHT = ZWEIGHT + PVEGTYPE(JTYPE)
  ZAVG_H  = ZAVG_H  + PVEGTYPE(JTYPE)*ZH_VEG(JTYPE)
END DO
!
IF(ZWEIGHT>0.0) PH_VEG = ZAVG_H/ZWEIGHT
!
IF (LHOOK) CALL DR_HOOK('MODI_H_VEG_FROM_LAI:H_VEG_FROM_LAI_0D',1,ZHOOK_HANDLE)
!
!-----------------------------------------------------------------
!
END FUNCTION H_VEG_FROM_LAI_0D
!
!   ###########################################################
    FUNCTION H_VEG_FROM_LAI_1D(PLAI,PH_TREE,PVEGTYPE,NPAR_VEG_IRR_USE) RESULT(PH_VEG)
!   ###########################################################
!!
!!    PURPOSE
!!    -------
!
!     Calculates vegetation height from leaf
!    area index and type of vegetation
!    (most of types; forest and vineyards; grassland)
!              
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!    none
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!      
!!    none
!!
!!    REFERENCE
!!    ---------
!!
!!      
!!    AUTHOR
!!    ------
!!
!!      B. Decharme          * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2021
!!
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODI_VEG_HEIGHT_FROM_LAI
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL,   DIMENSION(:),   INTENT(IN) :: PLAI         ! Leaf area Index
REAL,   DIMENSION(:),   INTENT(IN) :: PH_TREE      ! height of trees
REAL,   DIMENSION(:,:), INTENT(IN) :: PVEGTYPE     ! type of vegetation
INTEGER,DIMENSION(:),   INTENT(IN) :: NPAR_VEG_IRR_USE ! vegtype with irrigation
!
REAL,   DIMENSION(SIZE(PLAI))      :: PH_VEG          ! vegetation height
!
!*      0.2    declarations of local variables
!
!
REAL, DIMENSION(SIZE(PLAI),SIZE(PVEGTYPE,2)) :: ZH_VEG      ! height for each type
REAL, DIMENSION(SIZE(PLAI))                  :: ZAVG_H      ! averaged height
REAL, DIMENSION(SIZE(PLAI))                  :: ZWEIGHT            
!
INTEGER                                      :: JTYPE       ! loop counter
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-----------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('MODI_H_VEG_FROM_LAI:H_VEG_FROM_LAI_1D',0,ZHOOK_HANDLE)
!
PH_VEG(:) = 0.0
!
ZH_VEG(:,:) = VEG_HEIGHT_FROM_LAI(PLAI,PH_TREE,PVEGTYPE,NPAR_VEG_IRR_USE)
!
ZWEIGHT(:) = 0.0
ZAVG_H (:) = 0.0
DO JTYPE=4,SIZE(PVEGTYPE,2)
   ZWEIGHT(:) = ZWEIGHT(:) + PVEGTYPE(:,JTYPE) 
   ZAVG_H (:) = ZAVG_H (:) + PVEGTYPE(:,JTYPE) * ZH_VEG(:,JTYPE)
END DO
!
WHERE(ZWEIGHT(:)>0.0) PH_VEG(:) = ZAVG_H(:)/ZWEIGHT(:)
!
IF (LHOOK) CALL DR_HOOK('MODI_H_VEG_FROM_LAI:H_VEG_FROM_LAI_1D',1,ZHOOK_HANDLE)
!-----------------------------------------------------------------
!
END FUNCTION H_VEG_FROM_LAI_1D
!
!   ###########################################################
    FUNCTION H_VEG_FROM_LAI_2D(PLAI,PH_TREE,PVEGTYPE) RESULT(PH_VEG)
!   ###########################################################
!!
!!    PURPOSE
!!    -------
!
!     Calculates vegetation height from leaf
!    area index and type of vegetation
!    (most of types; forest and vineyards; grassland)
!              
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!    none
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!      
!!    none
!!
!!    REFERENCE
!!    ---------
!!
!!      
!!    AUTHOR
!!    ------
!!
!!      B. Decharme          * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2021
!!
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODI_VEG_HEIGHT_FROM_LAI
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL,   DIMENSION(:,:),   INTENT(IN) :: PLAI         ! Leaf area Index
REAL,   DIMENSION(:,:),   INTENT(IN) :: PH_TREE      ! height of trees
REAL,   DIMENSION(:,:,:), INTENT(IN) :: PVEGTYPE     ! type of vegetation
!
REAL,   DIMENSION(SIZE(PLAI,1),SIZE(PLAI,2)) :: PH_VEG  ! vegetation height
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(SIZE(PLAI,1),SIZE(PLAI,2),SIZE(PVEGTYPE,3)) :: ZH_VEG       ! height for each type
REAL, DIMENSION(SIZE(PLAI,1),SIZE(PLAI,2))                  :: ZAVG_H   ! averaged height
REAL, DIMENSION(SIZE(PLAI,1),SIZE(PLAI,2))                  :: ZWEIGHT  ! reference height        
!
INTEGER                                                     :: JTYPE    ! loop counter
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-----------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('MODI_H_VEG_FROM_LAI:H_VEG_FROM_LAI_2D',0,ZHOOK_HANDLE)
!
PH_VEG(:,:) = 0.0
!
ZH_VEG(:,:,:) = VEG_HEIGHT_FROM_LAI(PLAI,PH_TREE,PVEGTYPE)
!
ZWEIGHT(:,:) = 0.0
ZAVG_H (:,:) = 0.0
DO JTYPE=4,SIZE(PVEGTYPE,2)
   ZWEIGHT(:,:) = ZWEIGHT(:,:) + PVEGTYPE(:,:,JTYPE) 
   ZAVG_H (:,:) = ZAVG_H (:,:) + PVEGTYPE(:,:,JTYPE) * ZH_VEG(:,:,JTYPE)
END DO
!
WHERE(ZWEIGHT(:,:)>0.0) PH_VEG(:,:) = ZAVG_H(:,:)/ZWEIGHT(:,:)
!
IF (LHOOK) CALL DR_HOOK('MODI_H_VEG_FROM_LAI:H_VEG_FROM_LAI_2D',1,ZHOOK_HANDLE)
!-----------------------------------------------------------------
!
END FUNCTION H_VEG_FROM_LAI_2D
