!     ######################
      MODULE MODD_WRITE_COVER_TEX
!     ######################
!
!!****  *MODD_WRITE_COVER_TEX* -
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
!!
!!    AUTHOR
!!    ------
!!	V. Masson   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
IMPLICIT NONE
!
INTEGER           :: NTEX       ! logical unit of file 'class.tex'
INTEGER, PARAMETER:: NLINES = 50! number of lines per page
!
 CHARACTER(LEN=2)                               :: CLANG = 'EN' ! language used
 CHARACTER(LEN=60), DIMENSION(:), ALLOCATABLE   :: CNAME        ! names of ecosystems (1 language)
!-------------------------------------------------------------------------------
!
END MODULE MODD_WRITE_COVER_TEX
