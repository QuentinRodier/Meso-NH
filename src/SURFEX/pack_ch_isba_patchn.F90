!     #########
SUBROUTINE PACK_CH_ISBA_PATCH_n(KMASK,KSIZE,KNPATCH,KPATCH)
!##############################################
!
!
!!****  *PACK_CH_ISBA_PATCH_n * - packs chemistry variables
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
!!     V. Masson
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2004
!!------------------------------------------------------------------
!
USE MODD_CH_ISBA_n,      ONLY : XSOILRC_SO2, XSOILRC_O3, NBEQ
USE MODD_PACK_CH_ISBA,   ONLY : XP_DEP, XP_SOILRC_SO2, XP_SOILRC_O3, &
                                XBLOCK_SIMPLE
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
INTEGER JJ, JI
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!------------------------------------------------------------------------
!
! Packed surface module variables:
!
IF (LHOOK) CALL DR_HOOK('PACK_CH_ISBA_PATCH_N',0,ZHOOK_HANDLE)
!
ALLOCATE(XBLOCK_SIMPLE(KSIZE,2))
!
XP_SOILRC_SO2 => XBLOCK_SIMPLE(:,1)
XP_SOILRC_O3 => XBLOCK_SIMPLE(:,2)
!
ALLOCATE(XP_DEP(KSIZE,NBEQ))
!
!------------------------------------------------------------------------
!
IF (KNPATCH==1) THEN
  XP_SOILRC_SO2   (:)    =    XSOILRC_SO2   (:, 1)
  XP_SOILRC_O3    (:)    =    XSOILRC_O3    (:, 1)
ELSE
  DO JJ=1,KSIZE
    JI                      =    KMASK(JJ)
    XP_SOILRC_SO2   (JJ)    =    XSOILRC_SO2   (JI, KPATCH)
    XP_SOILRC_O3    (JJ)    =    XSOILRC_O3    (JI, KPATCH)
  ENDDO
END IF
IF (LHOOK) CALL DR_HOOK('PACK_CH_ISBA_PATCH_N',1,ZHOOK_HANDLE)
!
!------------------------------------------------------------------------
!
END SUBROUTINE PACK_CH_ISBA_PATCH_n
