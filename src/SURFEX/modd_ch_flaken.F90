!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #####################
      MODULE MODD_CH_FLAKE_n
!     ######################
!
!!
!!    PURPOSE
!!    -------
!     
!   
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      None
!!
!
!!    AUTHOR
!!    ------
!!  P. Tulet   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!  16/07/03 (P. Tulet)  restructured for externalization
!!      Modified    04/2013, P. Le Moigne: FLake chemistry
!------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
USE MODD_SV_n, ONLY : SV_t, SV_INIT
USE MODD_DST_n, ONLY : DST_t, DST_INIT
USE MODD_SLT_n, ONLY : SLT_t, SLT_INIT
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE

TYPE CH_FLAKE_t
!
  CHARACTER(LEN=6)              :: CCH_DRY_DEP              ! deposition scheme
  REAL, DIMENSION(:,:), POINTER :: XDEP                     ! final dry deposition  
                                                            ! velocity  for lakes
  TYPE(SV_t) :: SVF                                                
  TYPE(DST_t) :: DSTF                                                
  TYPE(SLT_t) :: SLTF                                                
                                                            ! (FOR DIAG ONLY)
  CHARACTER(LEN=6), DIMENSION(:), POINTER :: CCH_NAMES      ! NAME OF CHEMICAL SPECIES
                                                            ! (FOR DIAG ONLY)
  CHARACTER(LEN=6), DIMENSION(:), POINTER :: CDSTNAMES
  CHARACTER(LEN=6), DIMENSION(:), POINTER :: CSLTNAMES
  CHARACTER(LEN=6), DIMENSION(:), POINTER :: CAER_NAMES
  CHARACTER(LEN=6), DIMENSION(:), POINTER :: CSNWNAMES
!
END TYPE CH_FLAKE_t



CONTAINS

!
SUBROUTINE CH_FLAKE_INIT(YCH_FLAKE)
TYPE(CH_FLAKE_t), INTENT(INOUT) :: YCH_FLAKE
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK("MODD_CH_FLAKE_N:CH_FLAKE_INIT",0,ZHOOK_HANDLE)
NULLIFY(YCH_FLAKE%XDEP)
NULLIFY(YCH_FLAKE%CCH_NAMES)
NULLIFY(YCH_FLAKE%CAER_NAMES)
NULLIFY(YCH_FLAKE%CDSTNAMES)
NULLIFY(YCH_FLAKE%CSLTNAMES)
NULLIFY(YCH_FLAKE%CSNWNAMES)
YCH_FLAKE%CCH_DRY_DEP=' '
CALL SV_INIT(YCH_FLAKE%SVF)
CALL SLT_INIT(YCH_FLAKE%SLTF)
CALL DST_INIT(YCH_FLAKE%DSTF)
IF (LHOOK) CALL DR_HOOK("MODD_CH_FLAKE_N:CH_FLAKE_INIT",1,ZHOOK_HANDLE)
END SUBROUTINE CH_FLAKE_INIT


END MODULE MODD_CH_FLAKE_n
