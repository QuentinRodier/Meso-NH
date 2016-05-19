!     ####################
      MODULE MODD_WRITE_SURF_ATM
!     ####################
!
!!****  *MODD_WRITE_SURF_ATM - declaration of writing surface ATM
!!
!!    PURPOSE
!!    -------
!     Declaration of flags to write out fields in historical fields
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
!!	P. Le Moigne *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original       02/2008
!
!*       0.   DECLARATIONS
!             ------------
!
!
IMPLICIT NONE
!
!-----------------------------------------------------------------------------------------------------
LOGICAL    :: LNOWRITE_CANOPY  ! flag used to avoid writing of canopy fields in OUTPUT file
LOGICAL    :: LNOWRITE_TEXFILE ! flag used to avoid writing of tex file describing parameters
!-----------------------------------------------------------------------------------------------------
!
END MODULE MODD_WRITE_SURF_ATM
