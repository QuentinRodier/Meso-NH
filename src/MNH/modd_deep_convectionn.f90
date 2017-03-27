!MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! MASDEV4_7 modd 2006/07/12 18:36:21
!-----------------------------------------------------------------
!     #############################
      MODULE MODD_DEEP_CONVECTION_n
!     #############################
!
!!****  *MODD_DEEP_CONVECTION$n* - Contains convective tendencies 
!!
!!    PURPOSE
!!    -------
!!      Contains global convective tendencies and convective counter
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      None
!!
!!    REFERENCE
!!    ---------
!!      Book2 of documentation of Meso-NH (MODD_DEEP_CONVECTION$n)
!!
!!    AUTHOR
!!    ------
!!      P. Bechtold   *Laboratoire d'Aerologie*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    26/03/96
!!      Modif       11/12/98 : add diagnostic variables
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
USE MODD_PARAMETERS, ONLY: JPMODELMAX
IMPLICIT NONE

TYPE DEEP_CONVECTION_t
!
!  INTEGER, DIMENSION(:,:),POINTER :: NCOUNTCONV=>NULL()! convective counter used
!                                                      ! to refresh the
!                                                      ! convective tendencies
!  REAL, DIMENSION(:,:,:) ,POINTER :: XDTHCONV=>NULL()  ! convective TH tendency (K/s)
!  REAL, DIMENSION(:,:,:) ,POINTER :: XDRVCONV=>NULL()  ! convective r_v tendency (1/s)
!  REAL, DIMENSION(:,:,:) ,POINTER :: XDRCCONV=>NULL()  ! convective r_c tendency (1/s)
!  REAL, DIMENSION(:,:,:) ,POINTER :: XDRICONV=>NULL()  ! convective r_i tendency (1/s)
!  REAL, DIMENSION(:,:)   ,POINTER :: XPRCONV=>NULL()   ! total precipitation rate (m/s)
!  REAL, DIMENSION(:,:)   ,POINTER :: XPRSCONV=>NULL()  ! solid precipitation rate (m/s)
!  REAL, DIMENSION(:,:)   ,POINTER :: XPACCONV=>NULL()  ! accumulated convective
!                                                      ! precipitation (m)
  REAL, DIMENSION(:,:,:,:),POINTER ::XDSVCONV=>NULL()  ! tracer tendencies (1/s)
! diagnostic variables
  REAL, DIMENSION(:,:,:) ,POINTER :: XUMFCONV=>NULL()  ! updraft mass flux (kg/s m2)
  REAL, DIMENSION(:,:,:) ,POINTER :: XDMFCONV=>NULL()  ! downdraft mass flux (kg/s m2)
  REAL, DIMENSION(:,:,:) ,POINTER :: XMFCONV=>NULL()   ! convective mass flux (kg/s m2)
  REAL, DIMENSION(:,:,:) ,POINTER ::XPRLFLXCONV=>NULL()! liquid precip flux (m/s)
  REAL, DIMENSION(:,:,:) ,POINTER ::XPRSFLXCONV=>NULL()! solid  precip flux (m/s)
  REAL, DIMENSION(:,:)   ,POINTER :: XCAPE=>NULL()     ! CAPE (J/kg)
  INTEGER,DIMENSION(:,:), POINTER :: NCLTOPCONV=>NULL()! convective cloud top level
  INTEGER,DIMENSION(:,:), POINTER :: NCLBASCONV=>NULL()! convective cloud base level
!
  REAL, DIMENSION(:,:) ,  POINTER :: XIC_RATE=>NULL()  ! IC lightning frequency
  REAL, DIMENSION(:,:) ,  POINTER :: XCG_RATE=>NULL()  ! CG lightning frequency
  REAL, DIMENSION(:,:) ,  POINTER :: XIC_TOTAL_NUMBER=>NULL()  !Total number of IC
  REAL, DIMENSION(:,:) ,  POINTER :: XCG_TOTAL_NUMBER=>NULL()  !Total number of CG

END TYPE DEEP_CONVECTION_t

TYPE(DEEP_CONVECTION_t), DIMENSION(JPMODELMAX), TARGET, SAVE :: DEEP_CONVECTION_MODEL

INTEGER, DIMENSION(:,:),POINTER :: NCOUNTCONV=>NULL()
REAL, DIMENSION(:,:,:) ,POINTER :: XDTHCONV=>NULL()
REAL, DIMENSION(:,:,:) ,POINTER :: XDRVCONV=>NULL()
REAL, DIMENSION(:,:,:) ,POINTER :: XDRCCONV=>NULL()
REAL, DIMENSION(:,:,:) ,POINTER :: XDRICONV=>NULL()
REAL, DIMENSION(:,:)   ,POINTER :: XPRCONV=>NULL()
REAL, DIMENSION(:,:)   ,POINTER :: XPRSCONV=>NULL()
REAL, DIMENSION(:,:)   ,POINTER :: XPACCONV=>NULL()
REAL, DIMENSION(:,:,:,:),POINTER :: XDSVCONV=>NULL()
REAL, DIMENSION(:,:,:) ,POINTER :: XUMFCONV=>NULL()
REAL, DIMENSION(:,:,:) ,POINTER :: XDMFCONV=>NULL()
REAL, DIMENSION(:,:,:) ,POINTER :: XMFCONV=>NULL()
REAL, DIMENSION(:,:,:) ,POINTER :: XPRLFLXCONV=>NULL()
REAL, DIMENSION(:,:,:) ,POINTER :: XPRSFLXCONV=>NULL()
REAL, DIMENSION(:,:)   ,POINTER :: XCAPE=>NULL()
INTEGER,DIMENSION(:,:), POINTER :: NCLTOPCONV=>NULL()
INTEGER,DIMENSION(:,:), POINTER :: NCLBASCONV=>NULL()
REAL, DIMENSION(:,:) ,POINTER   :: XIC_RATE=>NULL()
REAL, DIMENSION(:,:) ,POINTER   :: XCG_RATE=>NULL()
REAL, DIMENSION(:,:) ,POINTER   :: XIC_TOTAL_NUMBER=>NULL()
REAL, DIMENSION(:,:) ,POINTER   :: XCG_TOTAL_NUMBER=>NULL()

CONTAINS

SUBROUTINE DEEP_CONVECTION_GOTO_MODEL(KFROM, KTO)
INTEGER, INTENT(IN) :: KFROM, KTO
!
! Save current state for allocated arrays
!DEEP_CONVECTION_MODEL(KFROM)%NCOUNTCONV=>NCOUNTCONV !Done in FIELDLIST_GOTO_MODEL
!DEEP_CONVECTION_MODEL(KFROM)%XDTHCONV=>XDTHCONV !Done in FIELDLIST_GOTO_MODEL
!DEEP_CONVECTION_MODEL(KFROM)%XDRVCONV=>XDRVCONV !Done in FIELDLIST_GOTO_MODEL
!DEEP_CONVECTION_MODEL(KFROM)%XDRCCONV=>XDRCCONV !Done in FIELDLIST_GOTO_MODEL
!DEEP_CONVECTION_MODEL(KFROM)%XDRICONV=>XDRICONV !Done in FIELDLIST_GOTO_MODEL
!DEEP_CONVECTION_MODEL(KFROM)%XPRCONV=>XPRCONV !Done in FIELDLIST_GOTO_MODEL
!DEEP_CONVECTION_MODEL(KFROM)%XPRSCONV=>XPRSCONV !Done in FIELDLIST_GOTO_MODEL
!DEEP_CONVECTION_MODEL(KFROM)%XPACCONV=>XPACCONV !Done in FIELDLIST_GOTO_MODEL
DEEP_CONVECTION_MODEL(KFROM)%XDSVCONV=>XDSVCONV
DEEP_CONVECTION_MODEL(KFROM)%XUMFCONV=>XUMFCONV
DEEP_CONVECTION_MODEL(KFROM)%XDMFCONV=>XDMFCONV
DEEP_CONVECTION_MODEL(KFROM)%XMFCONV=>XMFCONV
DEEP_CONVECTION_MODEL(KFROM)%XPRLFLXCONV=>XPRLFLXCONV
DEEP_CONVECTION_MODEL(KFROM)%XPRSFLXCONV=>XPRSFLXCONV
DEEP_CONVECTION_MODEL(KFROM)%XCAPE=>XCAPE
DEEP_CONVECTION_MODEL(KFROM)%NCLTOPCONV=>NCLTOPCONV
DEEP_CONVECTION_MODEL(KFROM)%NCLBASCONV=>NCLBASCONV
DEEP_CONVECTION_MODEL(KFROM)%XIC_RATE=>XIC_RATE
DEEP_CONVECTION_MODEL(KFROM)%XCG_RATE=>XCG_RATE
DEEP_CONVECTION_MODEL(KFROM)%XIC_TOTAL_NUMBER=>XIC_TOTAL_NUMBER
DEEP_CONVECTION_MODEL(KFROM)%XCG_TOTAL_NUMBER=>XCG_TOTAL_NUMBER
!
! Current model is set to model KTO
!NCOUNTCONV=>DEEP_CONVECTION_MODEL(KTO)%NCOUNTCONV !Done in FIELDLIST_GOTO_MODEL
!XDTHCONV=>DEEP_CONVECTION_MODEL(KTO)%XDTHCONV !Done in FIELDLIST_GOTO_MODEL
!XDRVCONV=>DEEP_CONVECTION_MODEL(KTO)%XDRVCONV !Done in FIELDLIST_GOTO_MODEL
!XDRCCONV=>DEEP_CONVECTION_MODEL(KTO)%XDRCCONV !Done in FIELDLIST_GOTO_MODEL
!XDRICONV=>DEEP_CONVECTION_MODEL(KTO)%XDRICONV !Done in FIELDLIST_GOTO_MODEL
!XPRCONV=>DEEP_CONVECTION_MODEL(KTO)%XPRCONV !Done in FIELDLIST_GOTO_MODEL
!XPRSCONV=>DEEP_CONVECTION_MODEL(KTO)%XPRSCONV !Done in FIELDLIST_GOTO_MODEL
!XPACCONV=>DEEP_CONVECTION_MODEL(KTO)%XPACCONV !Done in FIELDLIST_GOTO_MODEL
XDSVCONV=>DEEP_CONVECTION_MODEL(KTO)%XDSVCONV
XUMFCONV=>DEEP_CONVECTION_MODEL(KTO)%XUMFCONV
XDMFCONV=>DEEP_CONVECTION_MODEL(KTO)%XDMFCONV
XMFCONV=>DEEP_CONVECTION_MODEL(KTO)%XMFCONV
XPRLFLXCONV=>DEEP_CONVECTION_MODEL(KTO)%XPRLFLXCONV
XPRSFLXCONV=>DEEP_CONVECTION_MODEL(KTO)%XPRSFLXCONV
XCAPE=>DEEP_CONVECTION_MODEL(KTO)%XCAPE
NCLTOPCONV=>DEEP_CONVECTION_MODEL(KTO)%NCLTOPCONV
NCLBASCONV=>DEEP_CONVECTION_MODEL(KTO)%NCLBASCONV
XIC_RATE=>DEEP_CONVECTION_MODEL(KTO)%XIC_RATE
XCG_RATE=>DEEP_CONVECTION_MODEL(KTO)%XCG_RATE
XIC_TOTAL_NUMBER=>DEEP_CONVECTION_MODEL(KTO)%XIC_TOTAL_NUMBER
XCG_TOTAL_NUMBER=>DEEP_CONVECTION_MODEL(KTO)%XCG_TOTAL_NUMBER

END SUBROUTINE DEEP_CONVECTION_GOTO_MODEL

END MODULE MODD_DEEP_CONVECTION_n
