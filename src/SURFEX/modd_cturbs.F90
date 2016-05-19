!      #######################
          MODULE MODD_CTURBS
!      #######################
!
!!****   *MODD_CTURB*  - declaration of the turbulent scheme constants
!!
!!     PURPOSE
!!     -------
!        The purpose of this declarative module is to declare the 
!      turbulence scheme constants.
!
!!
!!**   IMPLICIT ARGUMENTS
!!     ------------------
!!       NONE
!!
!!     REFERENCE
!!     ---------
!!
!!     AUTHOR
!!     ------
!1       Joan Cuxart         * Meteo-France *
!!
!!     MODIFICATIONS
!!     -------------
!!       Original            08/08/94
!!     Nov 06, 2002 (V. Masson)  add XALPSBL and XASBL
!----------------------------------------------------------------------------
!
!*       0. DECLARATIONS
!           ------------
!
IMPLICIT NONE
!
REAL,SAVE :: XALPSBL = 4.63 ! constant linking TKE and friction velocity in the SBL
!                             Redelsperger et al 2001     = 4.63
!                             Wyngaard et al. 1974        = 3.75
!                             Stull 1988                  = 4.75
!
END MODULE MODD_CTURBS
