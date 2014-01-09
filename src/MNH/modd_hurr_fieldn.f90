!MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! MASDEV4_7 modd 2006/06/27 13:53:47
!-----------------------------------------------------------------
!     ############################
      MODULE MODD_HURR_FIELD_n
!     ############################
!
!!****  *MODD_HURR_FIELD$n* - declaration of filtered fields 
!!
!!    PURPOSE
!!    -------
!       The purpose of this declarative module is to specify  the 
!     fields obtained during the different steps of the filtering. 
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      None 
!!
!!    REFERENCE
!!    ---------
!!       This routine is not available in Meso-NH documentation yet.     
!!
!!    AUTHOR
!!    ------
!!
!!       O. Nuissier         * L.A. *
!!       R. Rogers           * NOAA/AOML/HRD (Hurricane Research Division) *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original       01/12/01                      
!!
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
USE MODD_PARAMETERS, ONLY: JPMODELMAX
IMPLICIT NONE

TYPE HURR_FIELD_t
!
  REAL, DIMENSION(:,:,:), POINTER :: XUTOT=>NULL(),XVTOT=>NULL(),XTTOT=>NULL()   ! Total fields
  REAL, DIMENSION(:,:),   POINTER :: XPTOT=>NULL()
!
  REAL, DIMENSION(:,:,:), POINTER :: XUENV=>NULL(),XVENV=>NULL(),XTENV=>NULL()  !  Environmental
  REAL, DIMENSION(:,:),   POINTER :: XPENV=>NULL()              ! fields
!
  REAL, DIMENSION(:,:,:), POINTER :: XUBASIC=>NULL(),XVBASIC=>NULL(),XTBASIC=>NULL() ! Basic
  REAL, DIMENSION(:,:,:), POINTER :: XPBASIC=>NULL()                 !fields
!
  REAL, DIMENSION(:,:,:), POINTER :: XVTDIS=>NULL() ! Total disturbance
                                                   !tangential wind
!
  REAL, DIMENSION(:,:,:), POINTER :: XQTOT=>NULL(),XQENV=>NULL(),XQBASIC=>NULL()
END TYPE HURR_FIELD_t

TYPE(HURR_FIELD_t), DIMENSION(JPMODELMAX), TARGET, SAVE :: HURR_FIELD_MODEL

REAL, DIMENSION(:,:,:), POINTER :: XUTOT=>NULL(),XVTOT=>NULL(),XTTOT=>NULL()
REAL, DIMENSION(:,:),   POINTER :: XPTOT=>NULL()
REAL, DIMENSION(:,:,:), POINTER :: XUENV=>NULL(),XVENV=>NULL(),XTENV=>NULL()
REAL, DIMENSION(:,:),   POINTER :: XPENV=>NULL()
REAL, DIMENSION(:,:,:), POINTER :: XUBASIC=>NULL(),XVBASIC=>NULL(),XTBASIC=>NULL()
REAL, DIMENSION(:,:,:), POINTER :: XPBASIC=>NULL()
REAL, DIMENSION(:,:,:), POINTER :: XVTDIS=>NULL()
  REAL, DIMENSION(:,:,:), POINTER :: XQTOT=>NULL(),XQENV=>NULL(),XQBASIC=>NULL()

CONTAINS

SUBROUTINE HURR_FIELD_GOTO_MODEL(KFROM, KTO)
INTEGER, INTENT(IN) :: KFROM, KTO
!
! Save current state for allocated arrays
HURR_FIELD_MODEL(KFROM)%XUTOT=>XUTOT
HURR_FIELD_MODEL(KFROM)%XVTOT=>XVTOT
HURR_FIELD_MODEL(KFROM)%XTTOT=>XTTOT
HURR_FIELD_MODEL(KFROM)%XPTOT=>XPTOT
HURR_FIELD_MODEL(KFROM)%XUENV=>XUENV
HURR_FIELD_MODEL(KFROM)%XVENV=>XVENV
HURR_FIELD_MODEL(KFROM)%XTENV=>XTENV
HURR_FIELD_MODEL(KFROM)%XPENV=>XPENV
HURR_FIELD_MODEL(KFROM)%XUBASIC=>XUBASIC
HURR_FIELD_MODEL(KFROM)%XVBASIC=>XVBASIC
HURR_FIELD_MODEL(KFROM)%XTBASIC=>XTBASIC
HURR_FIELD_MODEL(KFROM)%XPBASIC=>XPBASIC
HURR_FIELD_MODEL(KFROM)%XVTDIS=>XVTDIS
HURR_FIELD_MODEL(KFROM)%XQTOT=>XQTOT
HURR_FIELD_MODEL(KFROM)%XQENV=>XQENV
HURR_FIELD_MODEL(KFROM)%XQBASIC=>XQBASIC
!
! Current model is set to model KTO
XUTOT=>HURR_FIELD_MODEL(KTO)%XUTOT
XVTOT=>HURR_FIELD_MODEL(KTO)%XVTOT
XTTOT=>HURR_FIELD_MODEL(KTO)%XTTOT
XPTOT=>HURR_FIELD_MODEL(KTO)%XPTOT
XUENV=>HURR_FIELD_MODEL(KTO)%XUENV
XVENV=>HURR_FIELD_MODEL(KTO)%XVENV
XTENV=>HURR_FIELD_MODEL(KTO)%XTENV
XPENV=>HURR_FIELD_MODEL(KTO)%XPENV
XUBASIC=>HURR_FIELD_MODEL(KTO)%XUBASIC
XVBASIC=>HURR_FIELD_MODEL(KTO)%XVBASIC
XTBASIC=>HURR_FIELD_MODEL(KTO)%XTBASIC
XPBASIC=>HURR_FIELD_MODEL(KTO)%XPBASIC
XVTDIS=>HURR_FIELD_MODEL(KTO)%XVTDIS
XQTOT=>HURR_FIELD_MODEL(KTO)%XQTOT
XQENV=>HURR_FIELD_MODEL(KTO)%XQENV
XQBASIC=>HURR_FIELD_MODEL(KTO)%XQBASIC

END SUBROUTINE HURR_FIELD_GOTO_MODEL

END MODULE MODD_HURR_FIELD_n

