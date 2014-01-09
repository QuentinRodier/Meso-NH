!MNH_LIC Copyright 1994-2013 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENCE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$ $Date$
!-----------------------------------------------------------------
!     ##############################
      MODULE MODD_DEF_EDDYUV_FLUX_n
!     ##############################
!
!!****  *MODDB_DEF_EDDYUV_FLUX_n* - declaration FLUX V'U'
!!
!!    PURPOSE
!!    -------
!! To write fluxes in FM FILE
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      None 
!!
!!    AUTHOR
!!    ------
!!	P.Peyrille 18/02/04
!!
!!    MODIFICATIONS
!!    -------------
!!    05/05/09 M.Tomasini Grid-nesting
!!    25/06/11 M.Tomasini Add a source term for the one_way grid-nesting
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
USE MODD_PARAMETERS, ONLY: JPMODELMAX
IMPLICIT NONE
!
TYPE EDDYUV_FLUX_t
!
REAL, DIMENSION(:,:,:), POINTER :: XVU_FLUX_M=>NULL()
REAL, DIMENSION(:,:,:), POINTER :: XMT_FLUX=>NULL()
REAL, DIMENSION(:,:,:), POINTER :: XRVS_EDDY_FLUX=>NULL()

!
END TYPE EDDYUV_FLUX_t
!
TYPE(EDDYUV_FLUX_t), DIMENSION(JPMODELMAX), TARGET, SAVE :: EDDYUV_FLUX_MODEL
!
REAL, DIMENSION(:,:,:), POINTER :: XVU_FLUX_M=>NULL()
REAL, DIMENSION(:,:,:), POINTER :: XMT_FLUX=>NULL()
REAL, DIMENSION(:,:,:), POINTER :: XRVS_EDDY_FLUX=>NULL()
!
CONTAINS
!
SUBROUTINE EDDYUV_FLUX_GOTO_MODEL(KFROM, KTO)
!
INTEGER, INTENT(IN) :: KFROM, KTO
!
! Save current state for allocated arrays
EDDYUV_FLUX_MODEL(KFROM)%XVU_FLUX_M=>XVU_FLUX_M
EDDYUV_FLUX_MODEL(KFROM)%XMT_FLUX=>XMT_FLUX
EDDYUV_FLUX_MODEL(KFROM)%XRVS_EDDY_FLUX=>XRVS_EDDY_FLUX
!
! Current model is set to model KTO
XVU_FLUX_M=>EDDYUV_FLUX_MODEL(KTO)%XVU_FLUX_M
XMT_FLUX=>EDDYUV_FLUX_MODEL(KTO)%XMT_FLUX
XRVS_EDDY_FLUX=>EDDYUV_FLUX_MODEL(KTO)%XRVS_EDDY_FLUX
!
END SUBROUTINE EDDYUV_FLUX_GOTO_MODEL
!
END MODULE MODD_DEF_EDDYUV_FLUX_n
