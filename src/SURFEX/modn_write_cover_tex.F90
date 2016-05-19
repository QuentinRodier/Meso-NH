!!
!!    #####################
      MODULE MODN_WRITE_COVER_TEX
!!    #####################
!!
!!*** *MODN_DUST*
!!
!!    PURPOSE
!!    -------
!       Choice of language for tex file
!!
!!**  AUTHOR
!!    ------
!!    P. Le Moigne      *CNRM*
!
!!    MODIFICATIONS
!!    -------------
!!    Original 09/2009
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
USE MODD_WRITE_COVER_TEX, ONLY : CLANG
!!
!-----------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!        -----------------
IMPLICIT NONE
SAVE
NAMELIST /NAM_WRITE_COVER_TEX/CLANG
!
END MODULE MODN_WRITE_COVER_TEX
