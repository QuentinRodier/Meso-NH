!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ######################
      MODULE MODD_PACK_CH_ISBA
!     ######################
!
!!****  *MODD_PACK_CH_ISBA - declaration of packed surface parameters for chemistry
!!
!!    PURPOSE
!!    -------
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      None 
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!      A. Boone   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original       20/09/02
!
!*       0.   DECLARATIONS
!             ------------
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!-------------------------------------------------------------------------------
!
TYPE PACK_CH_ISBA_t

  REAL, POINTER, DIMENSION(:,:) :: XBLOCK_SIMPLE
  REAL, POINTER, DIMENSION(:)   :: XP_SOILRC_SO2 ! for SO2 deposition
  REAL, POINTER, DIMENSION(:)   :: XP_SOILRC_O3  ! for SO2 deposition
  REAL, POINTER, DIMENSION(:,:) :: XP_DEP        ! deposition velocity

END TYPE PACK_CH_ISBA_t
!
!-------------------------------------------------------------------------------
!


 CONTAINS

!
!




SUBROUTINE PACK_CH_ISBA_INIT(YPACK_CH_ISBA)
TYPE(PACK_CH_ISBA_t), INTENT(INOUT) :: YPACK_CH_ISBA
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK("MODD_PACK_CH_ISBA_N:PACK_CH_ISBA_INIT",0,ZHOOK_HANDLE)
  NULLIFY(YPACK_CH_ISBA%XBLOCK_SIMPLE)
  NULLIFY(YPACK_CH_ISBA%XP_SOILRC_SO2)
  NULLIFY(YPACK_CH_ISBA%XP_SOILRC_O3)
  NULLIFY(YPACK_CH_ISBA%XP_DEP)
IF (LHOOK) CALL DR_HOOK("MODD_PACK_CH_ISBA_N:PACK_CH_ISBA_INIT",1,ZHOOK_HANDLE)
END SUBROUTINE PACK_CH_ISBA_INIT


END MODULE MODD_PACK_CH_ISBA
