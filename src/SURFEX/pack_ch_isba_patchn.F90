!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE PACK_CH_ISBA_PATCH_n (CHI, PKCI, &
                                 KMASK,KSIZE,KNPATCH,KPATCH)
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
!
!
USE MODD_CH_ISBA_n, ONLY : CH_ISBA_t
USE MODD_PACK_CH_ISBA, ONLY : PACK_CH_ISBA_t
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
INTEGER JJ, JI
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!------------------------------------------------------------------------
!
! Packed surface module variables:
!
IF (LHOOK) CALL DR_HOOK('PACK_CH_ISBA_PATCH_N',0,ZHOOK_HANDLE)
!
ALLOCATE(PKCI%XBLOCK_SIMPLE(KSIZE,2))
!
PKCI%XP_SOILRC_SO2 => PKCI%XBLOCK_SIMPLE(:,1)
PKCI%XP_SOILRC_O3 => PKCI%XBLOCK_SIMPLE(:,2)
!
ALLOCATE(PKCI%XP_DEP(KSIZE,CHI%SVI%NBEQ))
!
!------------------------------------------------------------------------
!
IF (KNPATCH==1) THEN
  PKCI%XP_SOILRC_SO2   (:)    =    CHI%XSOILRC_SO2   (:, 1)
  PKCI%XP_SOILRC_O3    (:)    =    CHI%XSOILRC_O3    (:, 1)
ELSE
  DO JJ=1,KSIZE
    JI                      =    KMASK(JJ)
    PKCI%XP_SOILRC_SO2   (JJ)    =    CHI%XSOILRC_SO2   (JI, KPATCH)
    PKCI%XP_SOILRC_O3    (JJ)    =    CHI%XSOILRC_O3    (JI, KPATCH)
  ENDDO
END IF
IF (LHOOK) CALL DR_HOOK('PACK_CH_ISBA_PATCH_N',1,ZHOOK_HANDLE)
!
!------------------------------------------------------------------------
!
END SUBROUTINE PACK_CH_ISBA_PATCH_n
