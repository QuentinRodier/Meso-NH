!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE PREP_CTRL_SURF_ATM(K2M,OSURF_BUDGET,O2M_MIN_ZS,ORAD_BUDGET, &
                                         OCOEF,OSURF_VARS,OSURF_BUDGETC,       &
                                         ORESET_BUDGETC,ONOWRITE_TEXFILE,      &
                                         OSELECT,KLUOUT,OPROVAR_TO_DIAG)  
!     ########################################################################
!
!!****  *PREP_CTRL_SURF_ATM* - routine to check that diagnostics are switched off
!!                             
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
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
!
INTEGER,  INTENT(INOUT) :: K2M           ! flag for operational 2m quantities
LOGICAL,  INTENT(INOUT) :: OSURF_BUDGET  ! flag for surface budget
LOGICAL,  INTENT(INOUT) :: O2M_MIN_ZS    ! flag for 2m quantities on min.  orography
LOGICAL,  INTENT(INOUT) :: ORAD_BUDGET   ! flag for radiative budget
LOGICAL,  INTENT(INOUT) :: OCOEF         ! flag for transfer coefficients
LOGICAL,  INTENT(INOUT) :: OSURF_VARS    ! flag for surface variables
LOGICAL,  INTENT(INOUT) :: ONOWRITE_TEXFILE    ! flag for surface variables
INTEGER,  INTENT(IN)    :: KLUOUT        ! unit number
LOGICAL,  INTENT(INOUT) :: OSURF_BUDGETC      ! flag for cumulated surface budget
LOGICAL,  INTENT(INOUT) :: ORESET_BUDGETC     ! flag for cumulated surface budget
LOGICAL,  INTENT(INOUT) :: OSELECT       ! switch to control which fields are written
LOGICAL,  INTENT(INOUT) :: OPROVAR_TO_DIAG     ! switch to write (or not) prognostic variable
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('PREP_CTRL_SURF_ATM',0,ZHOOK_HANDLE)
K2M = 0
!
OSURF_BUDGET  = .FALSE.
O2M_MIN_ZS    = .FALSE.
ORAD_BUDGET   = .FALSE.
OCOEF         = .FALSE.
OSURF_VARS    = .FALSE.
!
OSURF_BUDGETC     = .FALSE.
ORESET_BUDGETC    = .FALSE.
!
ONOWRITE_TEXFILE  = .TRUE.
OSELECT           = .FALSE.
OPROVAR_TO_DIAG   = .FALSE.
!
WRITE(KLUOUT,*)'SURF_ATM DIAGNOSTICS DESACTIVATED'
IF (LHOOK) CALL DR_HOOK('PREP_CTRL_SURF_ATM',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE PREP_CTRL_SURF_ATM
