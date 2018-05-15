!     ###################
      MODULE MODN_VISC
!     ###################
!
!!****  *MODN_VISCOSITY* - declaration of namelist NAM_VISC
!!
!!    PURPOSE
!!    -------
!       The purpose of this module is to specify  the namelist NAM_VISC
!     which concern the parameters of the viscosity forces for all models
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!          
!!    AUTHOR
!!    ------
!!	    J. Colin                  * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    April 2011
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
USE MODD_VISC
!
IMPLICIT NONE
!
NAMELIST/NAM_VISC/LVISC,LVISC_UVW,LVISC_TH,LVISC_SV,LVISC_R,XMU_v,XPRANDTL
!
END MODULE MODN_VISC
