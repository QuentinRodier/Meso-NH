#ifdef MNH_NCWRIT
!     ################
      MODULE MODN_NCOUT
!     ################
!
!!****  *MODN_NCOUT* - declaration of namelist NAM_NCOUT
!!
!!    PURPOSE
!!    -------
!    writting of NETCDF output
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      Module MODD_NCOUT : contains declaration of configuration variables
!!
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!	S. Bielli L.A.
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    26/03/2012    
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
USE MODD_NCOUT
!
IMPLICIT NONE
!
NAMELIST/NAM_NCOUT/ LNETCDF,LLFIFM
!
END MODULE MODN_NCOUT
#endif
