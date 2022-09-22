!MNH_LIC Copyright 1994-2022 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!     ##################
      MODULE MODD_GRID_n
!     ##################
!
!!****  *MODD_GRID$n* - declaration of grid variables
!!
!!    PURPOSE
!!    -------
!       The purpose of this declarative module is to declare  the variables
!     describing the grid. 
!    
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      None 
!!
!!    REFERENCE
!!    ---------
!!      Book2 of documentation of Meso-NH (module MODD_GRIDn)
!!      Technical Specifications Report of the Meso-NH (chapters 2 and 3)
!!
!!    AUTHOR
!!    ------
!!	V. Ducrocq   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!  Original    05/05/94
!  J. Stein    15/11/95:  add the slope angle
!  V. Ducrocq  13/08/98: //: add XLATOR_ll and XLONOR_ll
!  V. Masson      11/2004: supress XLATOR, XLONOR, XLATOR_ll, XLONOR_ll
!  P. Wautelet 05/2016-04/2018: new data structures and calls for I/O
!  P. Wautelet    09/2022: add XXHATM, XYHATM, XZHATM, XHAT_BOUND, XHATM_BOUND,
!                          XXHAT_ll, XYHAT_ll, XXHATM_ll and XYHATM_ll
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
USE MODD_PARAMETERS, ONLY: JPMODELMAX

IMPLICIT NONE

SAVE

! Parameters for XHAT_BOUND and XHATM_BOUND
INTEGER, PARAMETER :: NHAT_BOUND_SIZE = 12
INTEGER, PARAMETER :: NPHYS_XMIN = 1  ! Position of minimum position in physical domain in x direction
INTEGER, PARAMETER :: NPHYS_XMAX = 2  ! Position of maximum position in physical domain in x direction
INTEGER, PARAMETER :: NPHYS_YMIN = 3  ! Position of minimum position in physical domain in y direction
INTEGER, PARAMETER :: NPHYS_YMAX = 4  ! Position of maximum position in physical domain in y direction
INTEGER, PARAMETER :: NPHYS_ZMIN = 5  ! Position of minimum position in physical domain in z direction
INTEGER, PARAMETER :: NPHYS_ZMAX = 6  ! Position of maximum position in physical domain in z direction
INTEGER, PARAMETER :: NEXTE_XMIN = 7  ! Position of minimum position in extended domain in x direction
INTEGER, PARAMETER :: NEXTE_XMAX = 8  ! Position of maximum position in extended domain in x direction
INTEGER, PARAMETER :: NEXTE_YMIN = 9  ! Position of minimum position in extended domain in y direction
INTEGER, PARAMETER :: NEXTE_YMAX = 10 ! Position of maximum position in extended domain in y direction
INTEGER, PARAMETER :: NEXTE_ZMIN = 11 ! Position of minimum position in extended domain in z direction
INTEGER, PARAMETER :: NEXTE_ZMAX = 12 ! Position of maximum position in extended domain in z direction


REAL, DIMENSION(:,:),  POINTER :: XLON=>NULL(),XLAT=>NULL() ! Longitude and latitude  
REAL, DIMENSION(:),    POINTER :: XXHAT=>NULL()             ! Position x in the conformal or cartesian plane
REAL, DIMENSION(:),    POINTER :: XYHAT=>NULL()             ! Position y in the conformal or cartesian plane
REAL, DIMENSION(:),    POINTER :: XXHATM=>NULL()            ! Position x in the conformal or cartesian plane at mass points
REAL, DIMENSION(:),    POINTER :: XYHATM=>NULL()            ! Position y in the conformal or cartesian plane at mass points
REAL, DIMENSION(:),    POINTER :: XDXHAT=>NULL()            ! horizontal stretching in x
REAL, DIMENSION(:),    POINTER :: XDYHAT=>NULL()            ! horizontal stretching in y
REAL, DIMENSION(:,:),  POINTER :: XMAP=>NULL()              ! Map factor 
REAL, DIMENSION(:,:),  POINTER :: XZS=>NULL()               ! orography
REAL, DIMENSION(:,:,:),POINTER :: XZZ=>NULL()               ! height z 
REAL,                  POINTER :: XZTOP=>NULL()             ! model top (m)
REAL, DIMENSION(:),    POINTER :: XZHAT=>NULL()             ! height level without orography
REAL, DIMENSION(:),    POINTER :: XZHATM=>NULL()            ! height level without orography at mass points
REAL, DIMENSION(:,:),  POINTER :: XDIRCOSXW=>NULL(),XDIRCOSYW=>NULL(),XDIRCOSZW=>NULL() ! director cosinus of the normal
                                                                                        ! to the ground surface
REAL, DIMENSION(:,:),  POINTER  :: XCOSSLOPE=>NULL()         ! cosinus of the angle between i and the slope vector
REAL, DIMENSION(:,:),  POINTER  :: XSINSLOPE=>NULL()         ! sinus   of the angle between i and the slope vector
! Quantities for SLEVE vertical coordinate
LOGICAL,               POINTER  :: LSLEVE=>NULL()            ! Logical for SLEVE coordinate
REAL,                  POINTER  :: XLEN1=>NULL()             ! Decay scale for smooth topography
REAL,                  POINTER  :: XLEN2=>NULL()             ! Decay scale for small-scale topography deviation
REAL, DIMENSION(:,:),  POINTER  :: XZSMT=>NULL()             ! smooth orography for SLEVE coordinate
REAL, DIMENSION(:),    POINTER :: XHAT_BOUND  => NULL() ! Boundaries of global domain at u and v points
REAL, DIMENSION(:),    POINTER :: XHATM_BOUND => NULL() ! Boundaries of global domain at mass points
REAL, DIMENSION(:),    POINTER :: XXHAT_ll  => NULL()   ! Position x in the conformal or cartesian plane (all domain)
REAL, DIMENSION(:),    POINTER :: XYHAT_ll  => NULL()   ! Position y in the conformal or cartesian plane (all domain)
REAL, DIMENSION(:),    POINTER :: XXHATM_ll => NULL()   ! Position x in the conformal or cartesian plane at mass points (all domain)
REAL, DIMENSION(:),    POINTER :: XYHATM_ll => NULL()   ! Position y in the conformal or cartesian plane (all domain) at mass points

END MODULE MODD_GRID_n
