!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #####################
      MODULE MODD_TYPE_SNOW
!     #####################
!
!!****  *MODD_TYPE_SNOW* - declaration of surface snow types
!!
!!    PURPOSE
!!    -------
!       The purpose of this declarative module is to define
!      the surface snow types. 
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      NONE 
!!
!!    REFERENCE
!!    --------- 
!!       
!!    AUTHOR
!!    ------
!!      P. Jabouille   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    20/01/99                      
!!      F. solmon   01/06/00 adaptation for patch approach  (+1D)
!!      V. Masson   01/2004  surface externalization
!!      P. Samuelsson 07/2014 additional snow albedos
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
!
IMPLICIT NONE
!
TYPE SURF_SNOW
 CHARACTER(LEN=3)                :: SCHEME    ! snow scheme used
INTEGER                         :: NLAYER    ! number of layers
REAL, DIMENSION(:,:,:), POINTER :: WSNOW     ! snow (& liq. water) content (kg/m2)
REAL, DIMENSION(:,:,:), POINTER :: HEAT      ! heat content                (J/m2)
REAL, DIMENSION(:,:,:), POINTER :: T         ! temperature '1-L'
REAL, DIMENSION(:,:,:), POINTER :: TEMP      ! temperature '3-L' (K)
REAL, DIMENSION(:,:,:), POINTER :: RHO       ! density
REAL, DIMENSION(:,:),   POINTER :: ALB       ! snow surface albedo
REAL, DIMENSION(:,:),   POINTER :: ALBVIS    ! snow surface visible albedo
REAL, DIMENSION(:,:),   POINTER :: ALBNIR    ! snow surface near-infrared albedo
REAL, DIMENSION(:,:),   POINTER :: ALBFIR    ! snow surface far-infrared albedo
REAL, DIMENSION(:,:),   POINTER :: EMIS      ! snow surface emissivity
REAL, DIMENSION(:,:),   POINTER :: TS        ! snow surface temperature
REAL, DIMENSION(:,:,:),   POINTER :: GRAN1   ! snow grain parameter 1
REAL, DIMENSION(:,:,:),   POINTER :: GRAN2   ! snow grain parameter 2
REAL, DIMENSION(:,:,:),   POINTER :: HIST    ! snow historical variable
!                                               (non dendritic case)
REAL, DIMENSION(:,:,:),   POINTER :: AGE ! snow grain age
END TYPE SURF_SNOW
!
END MODULE MODD_TYPE_SNOW


