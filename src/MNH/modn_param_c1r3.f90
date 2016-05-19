!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! MASDEV4_7 modn 2006/05/18 13:07:25
!-----------------------------------------------------------------
!     ######################  
      MODULE MODN_PARAM_C1R3
!     ######################
!
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
USE MODD_PARAM_C1R3
!
IMPLICIT NONE
!
NAMELIST/NAM_PARAM_C1R3/XALPHAI,XNUI,XALPHAS,XNUS,XALPHAG,XNUG, &
                        XFACTNUC_DEP,XFACTNUC_CON,              &
                        LSEDI,LHHONI,                           &
                        CPRISTINE_ICE_C1R3,CHEVRIMED_ICE_C1R3
!
END MODULE MODN_PARAM_C1R3
