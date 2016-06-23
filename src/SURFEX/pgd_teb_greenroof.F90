!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE PGD_TEB_GREENROOF (DTCO, UG, U, USS, GRM, TG, &
                                    HPROGRAM)
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
!
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_SURF_ATM_SSO_n, ONLY : SURF_ATM_SSO_t
USE MODD_SURFEX_n, ONLY : TEB_GREENROOF_MODEL_t
USE MODD_TEB_GRID_n, ONLY : TEB_GRID_t
!
USE MODD_PGD_GRID,             ONLY : NL
USE MODD_DATA_COVER_PAR,       ONLY : NVEGTYPE
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
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(SURF_ATM_SSO_t), INTENT(INOUT) :: USS
TYPE(TEB_GREENROOF_MODEL_t), INTENT(INOUT) :: GRM
TYPE(TEB_GRID_t), INTENT(INOUT) :: TG
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
GRM%TGRO%CISBA_GR  = 'DIF'
GRM%TGRO%CSCOND_GR = 'PL98 ' ! CSCOND_GR = 'PL98' !begin test 29092011 ! normalement pas besoin
GRM%TGRO%CHORT_GR  = 'DEF '
GRM%TGRO%CKSAT_GR  = 'DEF '
GRM%TGRO%LSOC_GR   = .FALSE.
GRM%TGRO%LTR_ML_GR = .FALSE.
!
ALLOCATE(GRM%TGRP%XRUNOFFB_GR(TG%NDIM))
ALLOCATE(GRM%TGRP%XWDRAIN_GR (TG%NDIM))
!
GRM%TGRP%XRUNOFFB_GR(:) = 0.5 
GRM%TGRP%XWDRAIN_GR (:) = 0.0
!
GRM%TGRO%NTIME_GR = 12
 CALL PGD_TEB_GREENROOF_PAR(DTCO, GRM%DTGR, UG, U, USS, GRM%TGRO, TG, &
                           HPROGRAM)
!
!
IF (LHOOK) CALL DR_HOOK('PGD_TEB_GREENROOF',1,ZHOOK_HANDLE)
!
!
!-------------------------------------------------------------------------------
!
!
END SUBROUTINE PGD_TEB_GREENROOF
