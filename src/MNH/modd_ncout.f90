#ifdef MNH_NCWRIT
!     #################
      MODULE MODD_NCOUT
!     #################
!
!!****  *MODD_NCOUT* - declaration of configuration variables
!!
!!    PURPOSE
!!    -------
!    Flag for NETCDF output 
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      None 
!!
!!    REFERENCE
!!    ---------
!!    AUTHOR
!!    ------
!!	S. Bielli L.A.
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    26/03/2012    
!!
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
IMPLICIT NONE
!
LOGICAL,SAVE      :: LNETCDF = .FALSE.  ! default no netcdf output
LOGICAL,SAVE      :: LLFIFM = .TRUE.  ! default lfi output
!
END MODULE MODD_NCOUT
#endif
