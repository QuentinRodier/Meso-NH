!MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$ $Date$
!-----------------------------------------------------------------
!     ############################
      MODULE MODD_DEF_EDDY_FLUX_n
!     ############################
!
!!**  MODD_DEF_EDDY_FLUX$n* - declaration FLUX W'T' V'T'
!!
!!    PURPOSE
!!    -------
!!    To write non zonal eddy fluxes in FM FILE
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!    None 
!!
!!    AUTHOR
!!    ------
!!	  P.Peyrille 18/02/04
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
TYPE EDDY_FLUX_t
!
REAL, DIMENSION(:,:,:), POINTER :: XVTH_FLUX_M=>NULL()    ! V'th' flux 
REAL, DIMENSION(:,:,:), POINTER :: XWTH_FLUX_M=>NULL()
REAL, DIMENSION(:,:,:), POINTER :: XRTHS_EDDY_FLUX=>NULL()
!
END TYPE EDDY_FLUX_t
!
TYPE(EDDY_FLUX_t), DIMENSION(JPMODELMAX), TARGET, SAVE :: EDDY_FLUX_MODEL
!
REAL, DIMENSION(:,:,:), POINTER :: XVTH_FLUX_M=>NULL()
REAL, DIMENSION(:,:,:), POINTER :: XWTH_FLUX_M=>NULL()
REAL, DIMENSION(:,:,:), POINTER :: XRTHS_EDDY_FLUX=>NULL()
!
CONTAINS
!
SUBROUTINE EDDY_FLUX_GOTO_MODEL(KFROM, KTO)
!
INTEGER, INTENT(IN) :: KFROM, KTO
!
! Save current state for allocated arrays
EDDY_FLUX_MODEL(KFROM)%XVTH_FLUX_M=>XVTH_FLUX_M
EDDY_FLUX_MODEL(KFROM)%XWTH_FLUX_M=>XWTH_FLUX_M
EDDY_FLUX_MODEL(KFROM)%XRTHS_EDDY_FLUX=>XRTHS_EDDY_FLUX
!
! Current model is set to model KTO
XVTH_FLUX_M=>EDDY_FLUX_MODEL(KTO)%XVTH_FLUX_M
XWTH_FLUX_M=>EDDY_FLUX_MODEL(KTO)%XWTH_FLUX_M
XRTHS_EDDY_FLUX=>EDDY_FLUX_MODEL(KTO)%XRTHS_EDDY_FLUX
!
END SUBROUTINE EDDY_FLUX_GOTO_MODEL
!
END MODULE MODD_DEF_EDDY_FLUX_n
