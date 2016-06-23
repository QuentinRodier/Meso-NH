!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE UNPACK_CH_ISBA_PATCH_n (CHI, PKCI, &
                                   KMASK,KSIZE,KNPATCH,KPATCH)
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
!
!
USE MODD_CH_ISBA_n, ONLY : CH_ISBA_t
USE MODD_PACK_CH_ISBA, ONLY : PACK_CH_ISBA_t
!
USE MODD_SURF_PAR,       ONLY : XUNDEF
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!
TYPE(CH_ISBA_t), INTENT(INOUT) :: CHI
TYPE(PACK_CH_ISBA_t), INTENT(INOUT) :: PKCI
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
 CHI%XDEP(:,:,KPATCH) = XUNDEF
!
IF (KNPATCH==1) THEN
  DO JSV=1,SIZE(CHI%XDEP,2)
    CHI%XDEP(:,JSV,KPATCH) = PKCI%XP_DEP        (:,JSV) 
  END DO

ELSE
  DO JSV=1,SIZE(CHI%XDEP,2)
    DO JJ=1,KSIZE
      JI                  = KMASK         (JJ)
      CHI%XDEP(JI,JSV,KPATCH) = PKCI%XP_DEP        (JJ,JSV) 
    END DO
  END DO
END IF
!
PKCI%XP_SOILRC_SO2 => NULL()
PKCI%XP_SOILRC_O3  => NULL()
!
DEALLOCATE(PKCI%XBLOCK_SIMPLE)
DEALLOCATE(PKCI%XP_DEP)
!
IF (LHOOK) CALL DR_HOOK('UNPACK_CH_ISBA_PATCH_N',1,ZHOOK_HANDLE)
!
END SUBROUTINE UNPACK_CH_ISBA_PATCH_n
