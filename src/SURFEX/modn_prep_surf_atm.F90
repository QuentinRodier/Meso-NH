!     ##################
      MODULE MODN_PREP_SURF_ATM
!     ##################
!
!!****  *MODN_PREP_SURF_ATM* - declaration of namelist NAM_PREP_SURF_ATM
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
!!      Original    01/2004                    
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
!
IMPLICIT NONE
!
 CHARACTER(LEN=28) :: CFILE        ! file name
 CHARACTER(LEN=6)  :: CFILETYPE    ! file type
 CHARACTER(LEN=28) :: CFILEPGD        ! file name
 CHARACTER(LEN=6)  :: CFILEPGDTYPE    ! file type
INTEGER           :: NYEAR        ! YEAR for surface
INTEGER           :: NMONTH       ! MONTH for surface
INTEGER           :: NDAY         ! DAY for surface
REAL              :: XTIME        ! TIME for surface

!
NAMELIST/NAM_PREP_SURF_ATM/CFILE, CFILETYPE, CFILEPGD, CFILEPGDTYPE, NYEAR, NMONTH, NDAY, XTIME
!
!-------------------------------------------------------------------------------
!
END MODULE MODN_PREP_SURF_ATM
