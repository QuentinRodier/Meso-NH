!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
      SUBROUTINE PGD_TEB_GREENROOF(HPROGRAM)
!     ##############################################################
!
!!**** *PGD_TEB_GREENROOF* monitor for averaging and interpolations of TEB GR physiographic fields
!!
!!    PURPOSE
!!    -------
!!
!!    METHOD
!!    ------
!!   
!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!
!!    C.de Munck & A. Lemonsu        Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    07/2011
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_PGD_GRID,             ONLY : NL
USE MODD_DATA_COVER_PAR,       ONLY : NVEGTYPE
USE MODD_TEB_n,                ONLY : LECOCLIMAP, XCOVER, LCOVER, XZS
USE MODD_TEB_GRID_n,           ONLY : NDIM
USE MODD_TEB_VEG_n,            ONLY : CPEDOTF, CPHOTO, NNBIOMASS
USE MODD_TEB_GREENROOF_n,      ONLY : LTR_ML_GR, NLAYER_GR, NTIME_GR, CISBA_GR, &
                                      CSCOND_GR, CHORT_GR, CKSAT_GR, CSOC_GR, &
                                      XRUNOFFB_GR, XWDRAIN_GR 
!
USE MODI_PGD_TEB_GREENROOF_PAR
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABOR1_SFX
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
 CHARACTER(LEN=6), INTENT(IN)  :: HPROGRAM   ! Type of program
!                                           ! F if all parameters must be specified
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
!*    0.3    Declaration of namelists
!            ------------------------
!
REAL(KIND=JPRB)          :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('PGD_TEB_GREENROOF',0,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
!*    1.      ISBA specific fields for green roofs
!             ------------------------------------
!
! for green roofs, CISBA = DIF / CSCOND = 'DEF '
CISBA_GR  = 'DIF'
CSCOND_GR = 'PL98 ' ! CSCOND_GR = 'PL98' !begin test 29092011 ! normalement pas besoin
CHORT_GR  = 'DEF '
CKSAT_GR  = 'DEF '
CSOC_GR   = 'DEF '
LTR_ML_GR = .FALSE.
!
ALLOCATE(XRUNOFFB_GR(NDIM))
ALLOCATE(XWDRAIN_GR (NDIM))
!
XRUNOFFB_GR(:) = 0.5 
XWDRAIN_GR (:) = 0.0
!
NTIME_GR = 12
 CALL PGD_TEB_GREENROOF_PAR(HPROGRAM)
!
!
IF (LHOOK) CALL DR_HOOK('PGD_TEB_GREENROOF',1,ZHOOK_HANDLE)
!
!
!-------------------------------------------------------------------------------
!
!
END SUBROUTINE PGD_TEB_GREENROOF
