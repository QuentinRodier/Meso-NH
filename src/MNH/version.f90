! $Source$ $Revision$
!-----------------------------------------------------------------
!     ##################
      SUBROUTINE VERSION
!     ##################
!
!!****  *VERSION * - subroutine to initialize the Mesonh version
!!
!!    PURPOSE
!!    -------
!       The purpose of this routine is to initialize NMASDEV and NBUGFIX
!     corresponding to the version chosen by the MesoNH user.
!       The user can also set the name of his own binary library
!      These values will be writen in the MesoNH output files
!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!      Module MODD_CONF       : NMASDEV,NBUGFIX,CBIBUSER
!!
!!    REFERENCE
!!    ---------
!!      Book2 of documentation (routine VERSION)
!!
!!
!!    AUTHOR
!!    ------
!!      P. Jabouille       * Meteo France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    17/04/02
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
USE MODD_CONF, ONLY : NMASDEV,NBUGFIX,CBIBUSER
!
IMPLICIT NONE
!
NMASDEV=51 
NBUGFIX=0
CBIBUSER=''
!
END SUBROUTINE VERSION
