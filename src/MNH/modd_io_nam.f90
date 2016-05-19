!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! MASDEV4_7 modd 2006/05/18 13:07:25
!-----------------------------------------------------------------
!     ######################
      MODULE MODD_IO_NAM
!     ######################
!
!!****  *MODD_IO_NAM* Keep in memory the namelist file names
!!
!!    PURPOSE
!!    -------
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
!!	V. Masson   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!
!*       0.   DECLARATIONS
!             ------------
!
!
IMPLICIT NONE
!------------------------------------------------------------------------------
!
CHARACTER(LEN=28), SAVE :: CNAM ="                            " ! name of namelist
CHARACTER(LEN=28), SAVE :: CFILE="                            " ! name of file
!
!------------------------------------------------------------------------------
!
END MODULE MODD_IO_NAM

