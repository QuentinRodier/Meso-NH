!     ##################
      MODULE MODN_PREP_ISBA_SNOW
!     ##################
!
!!****  *MODN_PREP_ISBA_SNOW* - declaration of namelist NAM_PREP_ISBA
!!
!!    PURPOSE
!!    -------
!       The purpose of this module is to specify  the namelist NAM_PREP_ISBA
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
IMPLICIT NONE
!
 CHARACTER(LEN=3)  :: CSNOW          ! snow scheme
INTEGER           :: NSNOW_LAYER    ! number of snow layers
!
END MODULE MODN_PREP_ISBA_SNOW
