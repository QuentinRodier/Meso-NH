!     ##################
      MODULE MODN_PREP_GREENROOF_SNOW
!     ##################
!
!!****  *MODN_PREP_GREENROOF_SNOW* - declaration of namelist NAM_PREP_GREENROOF_SNOW
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
!!    Based on modn_prep_garden_snow
!!       
!!    AUTHOR
!!    ------
!!	C. de Munck    *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    08/2011
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
END MODULE MODN_PREP_GREENROOF_SNOW
