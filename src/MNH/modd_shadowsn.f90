!MNH_LIC Copyright 2012-2022 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!      ########################
       MODULE MODD_SHADOWS_n
!      ########################
!
!!****  *MODD_SHADOWS$n* - declaration of parameters for shadows computations
!!
!!    PURPOSE
!!    -------
!!      The purpose of this declarative module is to define global orographic
!!      features necessary for the computation of shadows by mountains.
!!
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
!!     V. Masson     *Meteo-France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original      04/2012
!  P. Wautelet 22/09/2022: remove XXHAT_ll and XYHAT_ll (now in modd_grid_n)
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
!
USE MODD_PARAMETERS, ONLY: JPMODELMAX
IMPLICIT NONE

TYPE SHADOWS_t
!
!
  REAL    :: XZS_MAX_ll ! maximum orography in the domain
!
  REAL, DIMENSION(:,:),         POINTER :: XZS_XY=>NULL()     !  orography at vort. points
  REAL, DIMENSION(:,:),         POINTER :: XZS_ll=>NULL()     !  orography at mass points (all domain)
  REAL, DIMENSION(:,:),         POINTER :: XZS_XY_ll=>NULL()  !  orography at vort. points (all domain)
!
!
END TYPE SHADOWS_t

TYPE(SHADOWS_t), DIMENSION(JPMODELMAX), TARGET, SAVE :: SHADOWS_MODEL

REAL, POINTER :: XZS_MAX_ll=>NULL()
REAL, DIMENSION(:,:),   POINTER :: XZS_XY=>NULL()
REAL, DIMENSION(:,:),   POINTER :: XZS_ll=>NULL()
REAL, DIMENSION(:,:),   POINTER :: XZS_XY_ll=>NULL()

CONTAINS

SUBROUTINE SHADOWS_GOTO_MODEL(KFROM, KTO)
INTEGER, INTENT(IN) :: KFROM, KTO
!
! Save current state for allocated arrays
SHADOWS_MODEL(KFROM)%XZS_XY=>XZS_XY
SHADOWS_MODEL(KFROM)%XZS_ll=>XZS_ll
SHADOWS_MODEL(KFROM)%XZS_XY_ll=>XZS_XY_ll
!
! Current model is set to model KTO
XZS_MAX_ll=>SHADOWS_MODEL(KTO)%XZS_MAX_ll
XZS_XY=>SHADOWS_MODEL(KTO)%XZS_XY
XZS_ll=>SHADOWS_MODEL(KTO)%XZS_ll
XZS_XY_ll=>SHADOWS_MODEL(KTO)%XZS_XY_ll

END SUBROUTINE SHADOWS_GOTO_MODEL

END MODULE MODD_SHADOWS_n
