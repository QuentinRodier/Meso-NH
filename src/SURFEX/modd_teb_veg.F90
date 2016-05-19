!     ###################
      MODULE MODD_TEB_VEG
!     ###################
!
!!****  *MODD_TEB_VEG * - declaration of constant parameters

!!    PURPOSE
!!    -------
!       The purpose of this declarative module is to declare the 
!       constant flags for agricultural practices, assimilation scheme,
!       ST and soil water ice contents & deep soil fields
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
!!	C. de Munck & A. Lemonsu  *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    07/2011
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
IMPLICIT NONE 
!
INTEGER, PARAMETER       :: NTIME_GR_MAX  = 1
INTEGER, PARAMETER       :: NLAYER_GR_MAX = 6
!
END MODULE MODD_TEB_VEG
