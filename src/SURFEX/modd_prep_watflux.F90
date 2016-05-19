!     ################
      MODULE MODD_PREP_WATFLUX
!     ################
!
!!****  *MODD_PREP_WATFLUX - declaration for field interpolations
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
!!	S.Malardel    *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original       10/03
!
!*       0.   DECLARATIONS
!             ------------
!
IMPLICIT NONE
!
SAVE
!--------------------------------------------------------------------------
!
 CHARACTER(LEN=28) :: CFILE_WATFLX   ! input file name
 CHARACTER(LEN=6)  :: CTYPE          ! input file type
 CHARACTER(LEN=28) :: CFILEPGD_WATFLX   ! input file name
 CHARACTER(LEN=6)  :: CTYPEPGD          ! input file type
!
REAL              :: XTS_WATER_UNIF   !  uniform prescribed 
                                      !  surface temperature for inland water
!
!--------------------------------------------------------------------------
!
END MODULE MODD_PREP_WATFLUX


