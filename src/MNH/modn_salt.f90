!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! MASDEV4_7 modn 2006/12/21 14:53:53
!-----------------------------------------------------------------
!!    #####################
      MODULE MODN_SALT
!!    #####################
!!
!!*** *MODN_SALT*
!!
!!    PURPOSE
!!    -------
!       Namelist for marine sea salt scheme parameters 
!!
!!**  AUTHOR
!!    ------
!!    P. Tulet      *CNRM*
!
!!    MODIFICATIONS
!!    -------------
!!    Original 24/02/05
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!-----------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!        -----------------
USE MODD_SALT
IMPLICIT NONE
!
NAMELIST /NAM_SALT/ LSALT, CRGUNITS, LVARSIG_SLT,LSEDIMSALT,XN0MIN_SLT, XINIRADIUS_SLT, &
               XINISIG_SLT, XSIGMIN_SLT, XSIGMAX_SLT, XCOEFRADMAX_SLT, XCOEFRADMIN_SLT, &
               NMODE_SLT, LRGFIX_SLT, LDEPOS_SLT
!
END MODULE MODN_SALT
