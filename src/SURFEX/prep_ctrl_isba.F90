!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE PREP_CTRL_ISBA(DGO,OSURF_EVAP_BUDGET,OSURF_MISC_BUDGET,OSURF_MISC_DIF,OUTCI,KLUOUT )  
!     #################################################################################################################
!
!!****  *PREP_CTRL_ISBA* - routine to check that diagnostics are switched off
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!      P. Le Moigne   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    04/2007 
!!      Modified by A.L. Gibelin, 04/2009: add carbon spinup
!!      Modified by R. Séférian 08/2016: add key for lulcc tiling
!!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_DIAG_n, ONLY : DIAG_OPTIONS_t
!
USE MODI_PREP_CTRL
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
TYPE(DIAG_OPTIONS_t), INTENT(INOUT) :: DGO
!
LOGICAL,  INTENT(INOUT) :: OSURF_EVAP_BUDGET  ! flag for surface evaporation budget
LOGICAL,  INTENT(INOUT) :: OSURF_MISC_BUDGET  ! flag for surface miscellaneous budget
LOGICAL,  INTENT(INOUT) :: OSURF_MISC_DIF     ! flag for surface miscellaneous dif variables
LOGICAL,  INTENT(INOUT) :: OUTCI              ! flag for UTCI confort index
INTEGER,  INTENT(IN)    :: KLUOUT             ! unit number
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('PREP_CTRL_ISBA',0,ZHOOK_HANDLE)
!
CALL PREP_CTRL(DGO,KLUOUT)
!
DGO%N2M = 0
!
DGO%LPATCH_BUDGET     = .FALSE.
!
DGO%LLUTILES_BUDGET   = .FALSE.
!
OSURF_EVAP_BUDGET = .FALSE.
OSURF_MISC_BUDGET = .FALSE.
OSURF_MISC_DIF    = .FALSE.
OUTCI             = .FALSE.
!
WRITE(KLUOUT,*)'ISBA DIAGNOSTICS DESACTIVATED'
!
IF (LHOOK) CALL DR_HOOK('PREP_CTRL_ISBA',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE PREP_CTRL_ISBA
