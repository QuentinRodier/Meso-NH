!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! MASDEV4_7 prep_real 2006/05/18 13:07:25
!-----------------------------------------------------------------
!     ##########################
      SUBROUTINE DEALLOC_PARA_ll
!     ##########################
!
!!****  *DEALLOC_PARA_ll* -  deallocates the // variables
!! 
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!
!!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!	
!!
!!    MODIFICATIONS
!!    -------------
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_DIM_ll
!
IMPLICIT NONE
!
!*       0.1   Declaration of arguments
!              ------------------------
!
!*       0.2   Declaration of local variables
!              ------------------------------
!-------------------------------------------------------------------------------
!
DEALLOCATE(NDXRATIO_ALL, NDYRATIO_ALL)
DEALLOCATE(NXOR_ALL, NYOR_ALL) 
DEALLOCATE(NXEND_ALL, NYEND_ALL) 
DEALLOCATE(NDAD) 
DEALLOCATE(CLBCX, CLBCY) 
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE DEALLOC_PARA_ll
