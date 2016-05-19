!!
!!    #####################
      MODULE MODN_WRITE_SURF_ATM
!!    #####################
!!
!!*** *MODN_WRITE_SURF_ATM*
!!
!!    PURPOSE
!!    -------
!       Namelist for writing into output files
!!
!!**  AUTHOR
!!    ------
!!    P. Le Moigne      *CNRM*
!
!!    MODIFICATIONS
!!    -------------
!!    Original 02/2008
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
USE MODD_WRITE_SURF_ATM, ONLY : LNOWRITE_CANOPY, LNOWRITE_TEXFILE
!!
!-----------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!        -----------------
IMPLICIT NONE
SAVE
NAMELIST /NAM_WRITE_SURF_ATM/LNOWRITE_CANOPY, LNOWRITE_TEXFILE
!
END MODULE MODN_WRITE_SURF_ATM
