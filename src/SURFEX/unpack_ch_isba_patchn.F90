!     #########
SUBROUTINE UNPACK_CH_ISBA_PATCH_n(KMASK,KSIZE,KNPATCH,KPATCH)
!##############################################
!
!!****  *UNPACK_CH_ISBA_PATCH_n* - unpacks ISBA prognostic variables
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
!!     A. Boone
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2004
!!------------------------------------------------------------------
!
USE MODD_CH_ISBA_n,      ONLY : XDEP
USE MODD_PACK_CH_ISBA,   ONLY : XP_DEP, XP_SOILRC_SO2, XP_SOILRC_O3, &
                                XBLOCK_SIMPLE
!
USE MODD_SURF_PAR,       ONLY : XUNDEF
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
INTEGER, INTENT(IN)               :: KSIZE, KPATCH, KNPATCH
!
INTEGER, DIMENSION(:), INTENT(IN) :: KMASK
!
INTEGER :: JJ, JI, JSV
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!
! Only save values for patches which are in use:
!
IF (LHOOK) CALL DR_HOOK('UNPACK_CH_ISBA_PATCH_N',0,ZHOOK_HANDLE)
XDEP(:,:,KPATCH) = XUNDEF
!
IF (KNPATCH==1) THEN
  DO JSV=1,SIZE(XDEP,2)
    XDEP(:,JSV,KPATCH) = XP_DEP        (:,JSV) 
  END DO

ELSE
  DO JSV=1,SIZE(XDEP,2)
    DO JJ=1,KSIZE
      JI                  = KMASK         (JJ)
      XDEP(JI,JSV,KPATCH) = XP_DEP        (JJ,JSV) 
    END DO
  END DO
END IF
!
XP_SOILRC_SO2 => NULL()
XP_SOILRC_O3  => NULL()
!
DEALLOCATE(XBLOCK_SIMPLE)
DEALLOCATE(XP_DEP)
!
IF (LHOOK) CALL DR_HOOK('UNPACK_CH_ISBA_PATCH_N',1,ZHOOK_HANDLE)
!
END SUBROUTINE UNPACK_CH_ISBA_PATCH_n
