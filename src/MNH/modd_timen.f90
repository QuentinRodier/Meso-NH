!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! MASDEV4_7 modd 2006/05/18 13:07:25
!-----------------------------------------------------------------
!     ##################
      MODULE MODD_TIME_n
!     ##################
!
!!****  *MODD_TIME$n* - declaration of temporal grid variables
!!
!!    PURPOSE
!!    -------
!       The purpose of this declarative module is to specify  the variables
!     which concern the time for one nested model.
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      Module MODD_TIME : contains the definition of the types for time 
!!                              variables and time variables for all model
!!    REFERENCE
!!    ---------
!!      Book2 of documentation of Meso-NH (module MODD_TIME$n)
!!       
!!    AUTHOR
!!    ------
!!	V. Ducrocq   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    07/07/94       
!!      J.Stein     27/10/95   add the radiation call's instants               
!!      P.Bechtold  26/03/96   add the last deep convection call
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
USE MODD_TYPE_DATE
!
USE MODD_PARAMETERS, ONLY: JPMODELMAX
IMPLICIT NONE

TYPE TIME_t
!
  TYPE (DATE_TIME) :: TDTMOD        ! Time and Date of the  model beginning 
  TYPE (DATE_TIME) :: TDTCUR        ! Current Time and Date  
  TYPE (DATE_TIME) :: TDTRAD_FULL   ! Time and Date of the last full
                                       ! radiation call
  TYPE (DATE_TIME) :: TDTRAD_CLONLY ! Time and Date of the last radiation 
                                       ! call for only the cloudy verticals
  TYPE (DATE_TIME) :: TDTDCONV ! Time and Date of the last deep convection
                                  ! call
!
END TYPE TIME_t

TYPE(TIME_t), DIMENSION(JPMODELMAX), TARGET, SAVE :: TIME_MODEL

TYPE (DATE_TIME), POINTER :: TDTMOD=>NULL()
TYPE (DATE_TIME), POINTER :: TDTCUR=>NULL()
TYPE (DATE_TIME), POINTER :: TDTRAD_FULL=>NULL()
TYPE (DATE_TIME), POINTER :: TDTRAD_CLONLY=>NULL()
TYPE (DATE_TIME), POINTER :: TDTDCONV=>NULL()

CONTAINS

SUBROUTINE TIME_GOTO_MODEL(KFROM, KTO)
INTEGER, INTENT(IN) :: KFROM, KTO
!
! Save current state for allocated arrays
!
! Current model is set to model KTO
TDTMOD=>TIME_MODEL(KTO)%TDTMOD
TDTCUR=>TIME_MODEL(KTO)%TDTCUR
TDTRAD_FULL=>TIME_MODEL(KTO)%TDTRAD_FULL
TDTRAD_CLONLY=>TIME_MODEL(KTO)%TDTRAD_CLONLY
TDTDCONV=>TIME_MODEL(KTO)%TDTDCONV

END SUBROUTINE TIME_GOTO_MODEL

END MODULE MODD_TIME_n
