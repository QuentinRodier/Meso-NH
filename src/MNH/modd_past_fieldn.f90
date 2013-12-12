!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! MASDEV4_7 modd 2006/06/27 14:17:24
!-----------------------------------------------------------------
!     ###################
      MODULE MODD_PAST_FIELD_n
!     ###################
!
!!****  *MODD_PAST_FIELD$n* - declaration of prognostic variables
!!
!!    PURPOSE
!!    -------
!       The purpose of this declarative module is to specify  the 
!     prognostic variables. 
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      None 
!!
!!    REFERENCE
!!    ---------
!!      Book2 of documentation of Meso-NH (module MODD_PAST_FIELDn)
!!      Technical Specifications Report of the Meso-NH (chapters 2 and 3)
!!      
!!
!!    AUTHOR
!!    ------
!!	V. Masson    *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original       01/2013                       

!!
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
USE MODD_PARAMETERS, ONLY: JPMODELMAX
IMPLICIT NONE

TYPE FIELD_t
  REAL, DIMENSION(:,:,:), POINTER :: XUM=>NULL(),XVM=>NULL(),XWM=>NULL()
                                      ! U,V,W  at time t-dt
  REAL, DIMENSION(:,:,:), POINTER :: XDUM=>NULL(),XDVM=>NULL(),XDWM=>NULL()
!
END TYPE FIELD_t

TYPE(FIELD_t), DIMENSION(JPMODELMAX), TARGET, SAVE :: FIELD_MODEL

REAL, DIMENSION(:,:,:), POINTER :: XUM=>NULL(),XVM=>NULL(),XWM=>NULL()
REAL, DIMENSION(:,:,:), POINTER :: XDUM=>NULL(),XDVM=>NULL(),XDWM=>NULL()

CONTAINS

SUBROUTINE PAST_FIELD_GOTO_MODEL(KFROM, KTO)
INTEGER, INTENT(IN) :: KFROM, KTO
!
! Save current state for allocated arrays
FIELD_MODEL(KFROM)%XUM=>XUM
FIELD_MODEL(KFROM)%XVM=>XVM
FIELD_MODEL(KFROM)%XWM=>XWM
FIELD_MODEL(KFROM)%XDUM=>XDUM
FIELD_MODEL(KFROM)%XDVM=>XDVM
FIELD_MODEL(KFROM)%XDWM=>XDWM
!
! Current model is set to model KTO
XUM=>FIELD_MODEL(KTO)%XUM
XVM=>FIELD_MODEL(KTO)%XVM
XWM=>FIELD_MODEL(KTO)%XWM
XDUM=>FIELD_MODEL(KTO)%XDUM
XDVM=>FIELD_MODEL(KTO)%XDVM
XDWM=>FIELD_MODEL(KTO)%XDWM

END SUBROUTINE PAST_FIELD_GOTO_MODEL

END MODULE MODD_PAST_FIELD_n
