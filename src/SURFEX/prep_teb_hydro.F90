!     #########
SUBROUTINE PREP_TEB_HYDRO (PE, TH)
!     #################################################################################
!
!!****  *PREP_TEB_HYDRO* - Prepares ISBA fields for urban hydrology (based on GARDEN fields)
!!
!!    PURPOSE
!!    -------
!
!!**  METHOD
!!    ------
!!
!!    REFERENCE
!!    ---------
!!      
!!
!!    AUTHOR
!!    ------
!!     A. Lemonsu
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    02/2013
!!------------------------------------------------------------------
!
USE MODD_ISBA_n, ONLY : ISBA_PE_t
USE MODD_TEB_HYDRO_n, ONLY : TEB_HYDRO_t
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
TYPE(ISBA_PE_t), INTENT(INOUT) :: PE
TYPE(TEB_HYDRO_t),  INTENT(INOUT) :: TH
!
!*      0.2    declarations of local variables
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('PREP_TEB_HYDRO',0,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------------
!
! *     1.     Allocations
!
ALLOCATE(TH%XWG_ROAD        (SIZE(PE%XTG,1),SIZE(PE%XTG,2)))
ALLOCATE(TH%XWGI_ROAD       (SIZE(PE%XTG,1),SIZE(PE%XTG,2)))
ALLOCATE(TH%XWG_BLD         (SIZE(PE%XTG,1),SIZE(PE%XTG,2)))
ALLOCATE(TH%XWGI_BLD        (SIZE(PE%XTG,1),SIZE(PE%XTG,2)))
!
!
! *     3.     Initialisation of prognostic variables
!
! Soil column under roads
! No phase changes considered in the physics below roads yet
!TH%XWG_ROAD (:,:) = PE%XWG (:,:)
!TH%XWGI_ROAD(:,:) = PE%XWGI(:,:)
TH%XWG_ROAD (:,:) = PE%XWG (:,:) + PE%XWGI(:,:)
TH%XWGI_ROAD(:,:) = 0.
!
! Soil column under buildings
! No phase changes considered in the physics below buildings yet
!TH%XWG_BLD  (:,:) = PE%XWG (:,:)
!TH%XWGI_BLD (:,:) = PE%XWGI(:,:)
TH%XWG_BLD  (:,:) = PE%XWG (:,:)  + PE%XWGI(:,:)
TH%XWGI_BLD (:,:) = 0.
!
!
!-------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('PREP_TEB_HYDRO',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------------
!
END SUBROUTINE PREP_TEB_HYDRO
