!     ##################
      MODULE MODN_PGD_GRID
!     ##################
!
!!
!!    PURPOSE
!!    -------
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!       
!!    AUTHOR
!!    ------
!!	V. Masson    *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    10/2003                    
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
USE MODD_PGD_GRID, ONLY : CGRID
!
IMPLICIT NONE
!
 CHARACTER(LEN=28):: YINIFILE ! name of input file
 CHARACTER(LEN=6) :: YFILETYPE! type of input file
!
!
NAMELIST/NAM_PGD_GRID/CGRID,YINIFILE,YFILETYPE
!
END MODULE MODN_PGD_GRID
