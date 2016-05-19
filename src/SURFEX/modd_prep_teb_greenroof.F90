!     ################
      MODULE MODD_PREP_TEB_GREENROOF
!     ################
!
!!****  *MODD_PREP - declaration for field interpolations
!!
!!    PURPOSE
!!    -------
!     Declaration of surface parameters
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
!!	A. Lemonsu & C. de Munck 
!!
!!    MODIFICATIONS
!!    -------------
!!      Original       07/2011
!
!*       0.   DECLARATIONS
!             ------------
!
IMPLICIT NONE
!
!--------------------------------------------------------------------------
!
 CHARACTER(LEN=28) :: CFILE_ISBA     ! input file name
 CHARACTER(LEN=6)  :: CTYPE          ! input file type
 CHARACTER(LEN=28) :: CFILEPGD_ISBA   ! input file name
 CHARACTER(LEN=6)  :: CTYPEPGD       ! input file type
 CHARACTER(LEN=28) :: CFILE_SNOW     ! input file name for Snow
 CHARACTER(LEN=6)  :: CTYPE_SNOW     ! input file type for Snow
 CHARACTER(LEN=28) :: CFILE_HUG      ! input file name for Wg, Wgi
 CHARACTER(LEN=6)  :: CTYPE_HUG      ! input file type for Wg, Wgi
 CHARACTER(LEN=28) :: CFILE_TG       ! input file name for Tg
 CHARACTER(LEN=6)  :: CTYPE_TG       ! input file type for Tg
 CHARACTER(LEN=28) :: CFILE_HUG_SURF ! input file name for HUG_SURF
 CHARACTER(LEN=28) :: CFILE_HUG_ROOT ! input file name for HUG_ROOT
 CHARACTER(LEN=28) :: CFILE_HUG_DEEP ! input file name for HUG_DEEP
 CHARACTER(LEN=28) :: CFILE_TG_SURF  ! input file name for TG_SURF
 CHARACTER(LEN=28) :: CFILE_TG_ROOT  ! input file name for TG_ROOT
 CHARACTER(LEN=28) :: CFILE_TG_DEEP  ! input file name for TG_DEEP
!
REAL              :: XHUG_SURF      ! surface relative soil humidity
REAL              :: XHUG_ROOT      ! root layer relative soil humidity
REAL              :: XHUG_DEEP      ! deep layer relative soil humidity
REAL              :: XHUGI_SURF     ! surf layer relative ice content
REAL              :: XHUGI_ROOT     ! root layer relative ice content
REAL              :: XHUGI_DEEP     ! deep layer relative ice content
REAL              :: XTG_SURF       ! surface temperature
REAL              :: XTG_ROOT       ! root layer temperature
REAL              :: XTG_DEEP       ! deep layer temperature
!
LOGICAL :: LSNOW_IDEAL 
!
REAL, DIMENSION(:), POINTER :: XWSNOW         ! Snow reservoir
REAL, DIMENSION(:), POINTER :: XRSNOW         ! snow density
REAL, DIMENSION(:), POINTER :: XTSNOW         ! snow temperature
REAL                        :: XASNOW         ! snow albedo
!
REAL                        :: XWR_DEF        ! default for leaves interception reservoir
!--------------------------------------------------------------------------
!
!* normalized dimensions for interpolation grids for soil
!
INTEGER, PARAMETER           :: NGRID_LEVEL = 7 
REAL, DIMENSION(NGRID_LEVEL) :: XGRID_SOIL = (/ 0., 0.004, 0.036, 0.068, 0.100, 0.125, 0.150/)
!--------------------------------------------------------------------------
!
END MODULE MODD_PREP_TEB_GREENROOF


