!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! MASDEV4_7 modd 2006/06/27 14:05:40
!-----------------------------------------------------------------
!     #####################
      MODULE MODD_CH_DEP_n
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
!!  16/01/01 (P. Tulet)  restructured
!------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
USE MODD_PARAMETERS, ONLY: JPMODELMAX
IMPLICIT NONE

TYPE CH_DEP_t
!
  REAL  :: XRCSANDSO2            ! SO2 sand surface resistance
  REAL  :: XRCSANDO3             ! O3  sand surface resistance
  REAL  :: XRCCLAYSO2            ! SO2 clay surface resistance
  REAL  :: XRCCLAYO3             ! O3  clay surface resistance
  REAL  :: XRCSNOWSO2            ! SO2 snow surface resistance
  REAL  :: XRCSNOWO3             ! O3  snow surface resistance
  REAL  :: XLANDREXT            ! land type for external leaf resistance
  REAL, DIMENSION(:,:), POINTER :: XDIFFMOLH2O=>NULL() ! H2O molecular diffusivity

  REAL, DIMENSION(:,:,:), POINTER :: XHENRYVALCOR=>NULL() ! temperature correction for
                                                         ! chemical Henry constant value
  REAL, DIMENSION(:,:,:), POINTER :: XVDEPT=>NULL() ! final dry deposition velocity at t
!
!
!


END TYPE CH_DEP_t

TYPE(CH_DEP_t), DIMENSION(JPMODELMAX), TARGET, SAVE :: CH_DEP_MODEL

REAL, POINTER :: XRCSANDSO2=>NULL()
REAL, POINTER :: XRCSANDO3=>NULL()
REAL, POINTER :: XRCCLAYSO2=>NULL()
REAL, POINTER :: XRCCLAYO3=>NULL()
REAL, POINTER :: XRCSNOWSO2=>NULL()
REAL, POINTER :: XRCSNOWO3=>NULL()
REAL, POINTER :: XLANDREXT=>NULL()
REAL, DIMENSION(:,:), POINTER :: XDIFFMOLH2O=>NULL()
REAL, DIMENSION(:,:,:), POINTER :: XHENRYVALCOR=>NULL()
REAL, DIMENSION(:,:,:), POINTER :: XVDEPT=>NULL()

CONTAINS

SUBROUTINE CH_DEP_GOTO_MODEL(KFROM, KTO)
INTEGER, INTENT(IN) :: KFROM, KTO
!
! Save current state for allocated arrays
CH_DEP_MODEL(KFROM)%XDIFFMOLH2O=>XDIFFMOLH2O
CH_DEP_MODEL(KFROM)%XHENRYVALCOR=>XHENRYVALCOR
CH_DEP_MODEL(KFROM)%XVDEPT=>XVDEPT
!
! Current model is set to model KTO
XRCSANDSO2=>CH_DEP_MODEL(KTO)%XRCSANDSO2
XRCSANDO3=>CH_DEP_MODEL(KTO)%XRCSANDO3
XRCCLAYSO2=>CH_DEP_MODEL(KTO)%XRCCLAYSO2
XRCCLAYO3=>CH_DEP_MODEL(KTO)%XRCCLAYO3
XRCSNOWSO2=>CH_DEP_MODEL(KTO)%XRCSNOWSO2
XRCSNOWO3=>CH_DEP_MODEL(KTO)%XRCSNOWO3
XLANDREXT=>CH_DEP_MODEL(KTO)%XLANDREXT
XDIFFMOLH2O=>CH_DEP_MODEL(KTO)%XDIFFMOLH2O
XHENRYVALCOR=>CH_DEP_MODEL(KTO)%XHENRYVALCOR
XVDEPT=>CH_DEP_MODEL(KTO)%XVDEPT

END SUBROUTINE CH_DEP_GOTO_MODEL

END MODULE MODD_CH_DEP_n
