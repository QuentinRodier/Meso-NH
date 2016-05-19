!     ######spl
      MODULE MODN_MEAN
!     ###################
!
!!****  *MODN_MEAN* - declaration of namelist NAM_MEAN
!!
!!    PURPOSE
!!    -------
!       The purpose of this module is to specify  the namelist NAM_MEAN
!     which controls the averages used in the MEAN mode.
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      Module MODD_MEAN : 
!! 
!!
!!    REFERENCE
!!    ---------
!!          
!!    AUTHOR
!!    ------
!!	    P.Aumond * CNRM *                                           
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    July, 2011         
! ------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
USE MODD_MEAN_FIELD
!
IMPLICIT NONE
!
NAMELIST/NAM_MEAN/LMEAN_FIELD
!
!
END MODULE MODN_MEAN
