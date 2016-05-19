!     ##################
      MODULE MODN_PREP_TEB_SNOW
!     ##################
!
!!****  *MODN_PREP_TEB_SNOW* - declaration of namelist NAM_PREP_ISBA
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
 CHARACTER(LEN=3)  :: CSNOW_ROOF     ! snow scheme for roof
 CHARACTER(LEN=3)  :: CSNOW_ROAD     ! snow scheme for road
INTEGER :: NSNOW_ROOF ! snow scheme layers for roofs
INTEGER :: NSNOW_ROAD ! snow scheme layers for roads
!
END MODULE MODN_PREP_TEB_SNOW
