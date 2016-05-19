!     #########
SUBROUTINE VEGTYPE_GRID_TO_PATCH_GRID(KPATCH,PVEGTYPE_PATCH,PPATCH,PFIELDOUT,PW)
!        ################################################
!!
!!****  *VEGTYPE_GRID_TO_PATCH_GRID* averages fields from all (12) vegtypes 
!!                                   on only a few patches
!!    PURPOSE
!!    -------
!
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
!!
!!	V. Masson          * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2004
!!
!-------------------------------------------------------------------------------

!
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODD_DATA_COVER_PAR, ONLY : NVEGTYPE
!
USE MODI_VEGTYPE_TO_PATCH
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!
!*      0.1    declarations of arguments
!
INTEGER, INTENT(IN) :: KPATCH
REAL, DIMENSION(:,:,:), INTENT(IN)                          :: PVEGTYPE_PATCH
REAL, DIMENSION(:,:), INTENT(IN)                            :: PPATCH
REAL, DIMENSION(:,:,:), INTENT(IN)                          :: PFIELDOUT
REAL, DIMENSION(:,:,:), INTENT(OUT)                         :: PW
!
!
!*      0.2    declarations of local variables
!
INTEGER                       :: JPATCH    ! loop on patches
INTEGER                       :: JVEGTYPE  ! loop on vegtypes
INTEGER                       :: JLAYER    ! loop on layers
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!---------------------------------------------------------------------------
!
!* averages from vegtypes to chosen number of patches
IF (LHOOK) CALL DR_HOOK('VEGTYPE_GRID_TO_PATCH_GRID',0,ZHOOK_HANDLE)
PW(:,:,:) = 0.
DO JVEGTYPE=1,NVEGTYPE
  JPATCH = VEGTYPE_TO_PATCH(JVEGTYPE,KPATCH)
  DO JLAYER=1,SIZE(PW,2)
    PW(:,JLAYER,JPATCH) = PW(:,JLAYER,JPATCH)                                            &
                          + PVEGTYPE_PATCH(:,JVEGTYPE,JPATCH) * PFIELDOUT(:,JLAYER,JVEGTYPE)  
  END DO
END DO
!
!* insures undefined value when patch is not present
DO JPATCH=1,KPATCH
  DO JLAYER=1,SIZE(PW,2)
    WHERE(PPATCH(:,JPATCH)==0.) PW(:,JLAYER,JPATCH) = XUNDEF
  END DO
END DO
WHERE( ABS(PW-XUNDEF)/XUNDEF < 1.E-6 ) PW = XUNDEF
IF (LHOOK) CALL DR_HOOK('VEGTYPE_GRID_TO_PATCH_GRID',1,ZHOOK_HANDLE)
!
!---------------------------------------------------------------------------
!
END SUBROUTINE VEGTYPE_GRID_TO_PATCH_GRID
