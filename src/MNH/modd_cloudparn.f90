!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! MASDEV4_7 modd 2006/05/18 13:07:25
!-----------------------------------------------------------------
!     ######################
      MODULE MODD_CLOUDPAR_n
!     ######################
!
!!****  *MODD_CLOUDPAR$n* - declaration of the model-n dependant Microphysics 
!!   constants 
!!
!!    PURPOSE
!!    -------
!       The purpose of this declarative module is to declare  the 
!     model-n dependant Microhysics constants.    
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      None 
!!
!!    REFERENCE
!!    ---------
!!      Book2 of documentation of Meso-NH (MODD_CLOUDPARn)
!!          
!!    AUTHOR
!!    ------
!!	E. Richard   *Laboratoire d'Aerologie*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    20/12/95                      
!!       J.-P. Pinty   29/11/02 add C3R5, ICE2, ICE4, ELEC
!!
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
USE MODD_PARAMETERS, ONLY: JPMODELMAX
IMPLICIT NONE

TYPE CLOUDPAR_t
!
  INTEGER :: NSPLITR      ! Number of required small time step integration
                             ! for rain sedimentation computation
  INTEGER :: NSPLITG      ! Number of required small time step integration
                             ! for ice hydrometeor sedimentation computation
!
!
END TYPE CLOUDPAR_t

TYPE(CLOUDPAR_t), DIMENSION(JPMODELMAX), TARGET, SAVE :: CLOUDPAR_MODEL

INTEGER, POINTER :: NSPLITR=>NULL()
INTEGER, POINTER :: NSPLITG=>NULL()

CONTAINS

SUBROUTINE CLOUDPAR_GOTO_MODEL(KFROM, KTO)
INTEGER, INTENT(IN) :: KFROM, KTO
!
! Save current state for allocated arrays
!
! Current model is set to model KTO
NSPLITR=>CLOUDPAR_MODEL(KTO)%NSPLITR
NSPLITG=>CLOUDPAR_MODEL(KTO)%NSPLITG

END SUBROUTINE CLOUDPAR_GOTO_MODEL

END MODULE MODD_CLOUDPAR_n
