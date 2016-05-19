!     #########
SUBROUTINE HOR_INTERPOL_BUFFER(KLUOUT,PFIELDIN,PFIELDOUT)
!     #################################################################################
!
!!****  *HOR_INTERPOL_BUFFER * - Only extrapolation
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
!!     S.Malardel
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    03/2005
!!------------------------------------------------------------------
!
!
!
USE MODD_PREP,       ONLY : CMASK
!
USE MODD_GRID_BUFFER,  ONLY : NNI
USE MODD_SURF_PAR,   ONLY : XUNDEF
!
USE MODI_PACK_SAME_RANK
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_GET_SURF_MASK_n
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
INTEGER,            INTENT(IN)  :: KLUOUT    ! logical unit of output listing
REAL, DIMENSION(:,:), INTENT(IN)  :: PFIELDIN  ! field to interpolate horizontally
REAL, DIMENSION(:,:), INTENT(OUT) :: PFIELDOUT ! interpolated field
!
!*      0.2    declarations of local variables
!
INTEGER, DIMENSION(:), ALLOCATABLE :: IMASKOUT ! output mask
INTEGER                         :: INO      ! output number of points
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!------------------------------------------------------------------------------
!
!*      1.    Initialisation of the output mask
!
IF (LHOOK) CALL DR_HOOK('HOR_INTERPOL_BUFFER',0,ZHOOK_HANDLE)
INO = SIZE(PFIELDOUT,1)
ALLOCATE(IMASKOUT(INO))
 CALL GET_SURF_MASK_n(CMASK,INO,IMASKOUT,NNI,KLUOUT)
!
!*      2.    Mask the input field with the output mask
!!mask du tableau de taille FULL en fonction du type de surface
 CALL PACK_SAME_RANK(IMASKOUT,PFIELDIN,PFIELDOUT)
!
!*      6.    Deallocations
!
DEALLOCATE(IMASKOUT)
IF (LHOOK) CALL DR_HOOK('HOR_INTERPOL_BUFFER',1,ZHOOK_HANDLE)

!-------------------------------------------------------------------------------------
END SUBROUTINE HOR_INTERPOL_BUFFER
