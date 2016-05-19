!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! MASDEV4_7 modd 2007/02/19 11:58:44
!-----------------------------------------------------------------
!     ################
      MODULE MODD_ADV_n
!     ################
!
!!****  *MODD_ADV$n* - declaration of scalar advection scheme control variables
!!
!!    PURPOSE
!!    -------
!       The purpose of this declarative module is to declare the advective 
!     control variables.    
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      None 
!!
!!    REFERENCE
!!    ---------
!!      Book2 of documentation of Meso-NH (module MODD_ADVn)
!!          
!!    AUTHOR
!!    ------
!!	Vila, Lafore   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original     23/10/95  (Vila, lafore) For new scalar advection schemes
!!      C.Lac       24/04/06  Introduction of CUVW_ADV_SCHEME and
!!                            removal of CFV_ADV_SCHEME
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
USE MODD_PARAMETERS, ONLY: JPMODELMAX
IMPLICIT NONE

TYPE ADV_t
!
  CHARACTER(LEN=6)       :: CMET_ADV_SCHEME, CSV_ADV_SCHEME, CUVW_ADV_SCHEME
                                  ! Control the selected advection scheme
                                  ! for the scalar variables
!
  INTEGER                :: NLITER  ! Number iterations MPDATA
  REAL, DIMENSION(:,:,:), POINTER :: XRTHMS=>NULL()    ! Source of (rho theta)
                                        ! advection for PPM
  REAL, DIMENSION(:,:,:), POINTER :: XRTKEMS=>NULL()   ! Idem for kinetic energy
  REAL, DIMENSION(:,:,:,:), POINTER :: XRRMS=>NULL()   ! Idem for Moist variables
  REAL, DIMENSION(:,:,:,:), POINTER :: XRSVMS=>NULL()  ! Idem for  addi. scalar
!
END TYPE ADV_t

TYPE(ADV_t), DIMENSION(JPMODELMAX), TARGET, SAVE :: ADV_MODEL

CHARACTER(LEN=6), POINTER :: CMET_ADV_SCHEME=>NULL(), CSV_ADV_SCHEME=>NULL(), CUVW_ADV_SCHEME=>NULL()
INTEGER, POINTER :: NLITER=>NULL()
REAL, DIMENSION(:,:,:), POINTER :: XRTHMS=>NULL()
REAL, DIMENSION(:,:,:), POINTER :: XRTKEMS=>NULL()
REAL, DIMENSION(:,:,:,:), POINTER :: XRRMS=>NULL()
REAL, DIMENSION(:,:,:,:), POINTER :: XRSVMS=>NULL()


CONTAINS

SUBROUTINE ADV_GOTO_MODEL(KFROM, KTO)
INTEGER, INTENT(IN) :: KFROM, KTO
!
! Save current state for allocated arrays
ADV_MODEL(KFROM)%XRTHMS=>XRTHMS
ADV_MODEL(KFROM)%XRTKEMS=>XRTKEMS
ADV_MODEL(KFROM)%XRRMS=>XRRMS
ADV_MODEL(KFROM)%XRSVMS=>XRSVMS
!
! Current model is set to model KTO
CUVW_ADV_SCHEME=>ADV_MODEL(KTO)%CUVW_ADV_SCHEME
CMET_ADV_SCHEME=>ADV_MODEL(KTO)%CMET_ADV_SCHEME
CSV_ADV_SCHEME=>ADV_MODEL(KTO)%CSV_ADV_SCHEME
NLITER=>ADV_MODEL(KTO)%NLITER
XRTHMS=>ADV_MODEL(KTO)%XRTHMS
XRTKEMS=>ADV_MODEL(KTO)%XRTKEMS
XRRMS=>ADV_MODEL(KTO)%XRRMS
XRSVMS=>ADV_MODEL(KTO)%XRSVMS

END SUBROUTINE ADV_GOTO_MODEL

END MODULE MODD_ADV_n
