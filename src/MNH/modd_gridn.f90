!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! MASDEV4_7 modd 2006/06/27 13:54:55
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
!!      Original    05/05/94                      
!!      J. Stein    15/11/95  add the slope angle
!!      V. Ducrocq   13/08/98  // : add XLATOR_ll and XLONOR_ll       
!!      V. Masson   nov 2004  supress XLATOR,XLONOR,XLATOR_ll,XLONOR_ll
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
USE MODD_PARAMETERS, ONLY: JPMODELMAX
IMPLICIT NONE

TYPE GRID_t
  REAL, DIMENSION(:,:), POINTER :: XLON=>NULL(),XLAT=>NULL() ! Longitude and latitude  
!
  REAL, DIMENSION(:),   POINTER :: XXHAT=>NULL()   ! Position x in the 
                                         ! conformal or cartesian plane
  REAL, DIMENSION(:),   POINTER :: XYHAT=>NULL()   ! Position y in the 
                                         ! conformal or cartesian plane
  REAL, DIMENSION(:),   POINTER :: XDXHAT=>NULL()  ! horizontal stretching in x
  REAL, DIMENSION(:),   POINTER :: XDYHAT=>NULL()  ! horizontal stretching in y
  REAL, DIMENSION(:,:), POINTER :: XMAP=>NULL()    ! Map factor 
!
  REAL, DIMENSION(:,:),   POINTER :: XZS=>NULL()   ! orography
  REAL, DIMENSION(:,:,:), POINTER :: XZZ=>NULL()   ! height z 
  REAL, DIMENSION(:),     POINTER :: XZHAT=>NULL() ! height level without orography
!
  REAL, DIMENSION(:,:)  , POINTER :: XDIRCOSXW=>NULL(),XDIRCOSYW=>NULL(),XDIRCOSZW=>NULL() 
                                               ! director cosinus of the normal 
                                               ! to the ground surface 
!  
  REAL, DIMENSION(:,:),  POINTER  ::  XCOSSLOPE=>NULL()  ! cosinus of the angle
                                 ! between i and the slope vector
  REAL, DIMENSION(:,:),  POINTER  ::  XSINSLOPE=>NULL()  ! sinus of the angle
                                 ! between i and the slope vector
! quantities for SLEVE vertical coordinate
  LOGICAL                         :: LSLEVE    ! Logical for SLEVE coordinate
  REAL                            :: XLEN1     ! Decay scale for smooth topography
  REAL                            :: XLEN2     ! Decay scale for small-scale topography deviation
  REAL, DIMENSION(:,:),   POINTER :: XZSMT=>NULL()   ! smooth orography for SLEVE coordinate
END TYPE GRID_t

TYPE(GRID_t), DIMENSION(JPMODELMAX), TARGET, SAVE :: GRID_MODEL

REAL, DIMENSION(:,:), POINTER :: XLON=>NULL(),XLAT=>NULL()
REAL, DIMENSION(:),   POINTER :: XXHAT=>NULL()
REAL, DIMENSION(:),   POINTER :: XYHAT=>NULL()
REAL, DIMENSION(:),   POINTER :: XDXHAT=>NULL()
REAL, DIMENSION(:),   POINTER :: XDYHAT=>NULL()
REAL, DIMENSION(:,:), POINTER :: XMAP=>NULL()
REAL, DIMENSION(:,:),   POINTER :: XZS=>NULL()
REAL, DIMENSION(:,:,:), POINTER :: XZZ=>NULL()
REAL, DIMENSION(:),     POINTER :: XZHAT=>NULL()
REAL, DIMENSION(:,:)  , POINTER :: XDIRCOSXW=>NULL(),XDIRCOSYW=>NULL(),XDIRCOSZW=>NULL()
REAL, DIMENSION(:,:),  POINTER  :: XCOSSLOPE=>NULL()
REAL, DIMENSION(:,:),  POINTER  :: XSINSLOPE=>NULL()
LOGICAL,               POINTER  :: LSLEVE=>NULL()
REAL,                  POINTER  :: XLEN1=>NULL()
REAL,                  POINTER  :: XLEN2=>NULL()
REAL, DIMENSION(:,:),  POINTER  :: XZSMT=>NULL()

CONTAINS

SUBROUTINE GRID_GOTO_MODEL(KFROM, KTO)
INTEGER, INTENT(IN) :: KFROM, KTO
!
! Save current state for allocated arrays
GRID_MODEL(KFROM)%XLON=>XLON
GRID_MODEL(KFROM)%XLAT=>XLAT
GRID_MODEL(KFROM)%XXHAT=>XXHAT
GRID_MODEL(KFROM)%XYHAT=>XYHAT
GRID_MODEL(KFROM)%XDXHAT=>XDXHAT
GRID_MODEL(KFROM)%XDYHAT=>XDYHAT
GRID_MODEL(KFROM)%XMAP=>XMAP
GRID_MODEL(KFROM)%XZS=>XZS
GRID_MODEL(KFROM)%XZZ=>XZZ
GRID_MODEL(KFROM)%XZHAT=>XZHAT
GRID_MODEL(KFROM)%XDIRCOSXW=>XDIRCOSXW
GRID_MODEL(KFROM)%XDIRCOSYW=>XDIRCOSYW
GRID_MODEL(KFROM)%XDIRCOSZW=>XDIRCOSZW
GRID_MODEL(KFROM)%XCOSSLOPE=>XCOSSLOPE
GRID_MODEL(KFROM)%XSINSLOPE=>XSINSLOPE
GRID_MODEL(KFROM)%XZSMT=>XZSMT
!
! Current model is set to model KTO
XLON=>GRID_MODEL(KTO)%XLON
XLAT=>GRID_MODEL(KTO)%XLAT
XXHAT=>GRID_MODEL(KTO)%XXHAT
XYHAT=>GRID_MODEL(KTO)%XYHAT
XDXHAT=>GRID_MODEL(KTO)%XDXHAT
XDYHAT=>GRID_MODEL(KTO)%XDYHAT
XMAP=>GRID_MODEL(KTO)%XMAP
XZS=>GRID_MODEL(KTO)%XZS
XZZ=>GRID_MODEL(KTO)%XZZ
XZHAT=>GRID_MODEL(KTO)%XZHAT
XDIRCOSXW=>GRID_MODEL(KTO)%XDIRCOSXW
XDIRCOSYW=>GRID_MODEL(KTO)%XDIRCOSYW
XDIRCOSZW=>GRID_MODEL(KTO)%XDIRCOSZW
XCOSSLOPE=>GRID_MODEL(KTO)%XCOSSLOPE
XSINSLOPE=>GRID_MODEL(KTO)%XSINSLOPE
LSLEVE=>GRID_MODEL(KTO)%LSLEVE
XLEN1=>GRID_MODEL(KTO)%XLEN1
XLEN2=>GRID_MODEL(KTO)%XLEN2
XZSMT=>GRID_MODEL(KTO)%XZSMT

END SUBROUTINE GRID_GOTO_MODEL

END MODULE MODD_GRID_n
