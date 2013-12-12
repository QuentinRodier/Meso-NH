!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! MASDEV4_7 budget 2006/05/18 13:07:25
!-----------------------------------------------------------------
!##########################
 MODULE MODI_ENDSTEP_BUDGET
!##########################
!
INTERFACE
!
      SUBROUTINE ENDSTEP_BUDGET(HFMDIAC,HLUOUT,KTCOUNT,              &
                               TPDTCUR,TPDTMOD,PTSTEP,KSV            )
!
USE MODD_TYPE_DATE
!
CHARACTER (LEN=*), INTENT(IN) :: HFMDIAC    ! name of FM-file to write
CHARACTER (LEN=*), INTENT(IN) :: HLUOUT     ! name of output listing
INTEGER, INTENT(IN)            :: KTCOUNT    ! temporal loop counter
TYPE (DATE_TIME),   INTENT(IN) :: TPDTCUR    ! Current date and time
TYPE (DATE_TIME),   INTENT(IN) :: TPDTMOD    ! Creation date and time
REAL,               INTENT(IN) :: PTSTEP     ! time step
INTEGER,            INTENT(IN) :: KSV        ! Number of Scalar Variables

!
END SUBROUTINE ENDSTEP_BUDGET  
!
END INTERFACE
!
END MODULE MODI_ENDSTEP_BUDGET
!
!     ###############################################################
      SUBROUTINE ENDSTEP_BUDGET(HFMDIAC,HLUOUT,KTCOUNT,             &
                               TPDTCUR,TPDTMOD,PTSTEP,KSV           )
!     ###############################################################
!
!!****  *ENDSTEP_BUDGET* - routine to call the routine write_budget 
!!                           
!!
!!    PURPOSE
!!    -------
!       If CART case, this routine sets the logical variable LBU_BEG (for budget
!     beginning) to .TRUE. calls the routine write_budget and reset the budget
!     arrays to 0.
!       If MASK case this routine increases the time variable NBUTIME if the 
!     budget is not terminated or calls the routine write_budget if it is.
! 
!!**  METHOD
!!    ------
!!      
!!      
!!
!!    EXTERNAL
!!    --------
!!       NONE
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!       Module MODD_BUDGET
!!         CBUTYPE     : budget type : CART, MASK or NONE
!!         LBU_BEG     : switch for the budget begining
!!         NBUTIME     : number of the budget step
!!         NBUWRI      : NUMBER of budget steps when the budget 
!!                       is written on FM-file
!!         XBURU       : budget array of the variable RU
!!         XBURV       : budget array of the variable RV
!!         XBURW       : budget array of the variable RW
!!         XBURTH      : budget array of the variable RTH
!!         XBURTKE     : budget array of the variable RTKE
!!         XBURRV      : budget array of the variable RRV
!!         XBURRC      : budget array of the variable RRC
!!         XBURRR      : budget array of the variable RRR
!!         XBURRI      : budget array of the variable RRI
!!         XBURRS      : budget array of the variable RRS
!!         XBURRG      : budget array of the variable RRG
!!         XBURRH      : budget array of the variable RRH
!!         XBURSVx     : budget array of the variable RSVx
!!
!!
!!    REFERENCE
!!    ---------
!!      Book2 of MESO-NH documentation (routine ENDSTEP_BUDGET)
!!
!!
!!    AUTHOR
!!    ------
!!  	J. Nicolau       * Meteo France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    27/02/95
!!                  09/07/96  control the writing in the CART case
!!      JP Lafore   10/02/98  reinitialization of the BURHODJ after writings
!!      V. Ducrocq  07/06/99  //
!!      N. Asensio  22/06/99  // MASK case : delete KIU,KJU,KKU arguments
!!                            and change the write_budget call
!!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_TIME
USE MODD_BUDGET
!
USE MODI_WRITE_BUDGET
!
IMPLICIT NONE
!  
!  
!*       0.1   Declarations of arguments :
!
CHARACTER (LEN=*), INTENT(IN)  :: HFMDIAC    ! FM-file where the budget is to be
                                             ! written
CHARACTER (LEN=*), INTENT(IN)  :: HLUOUT     ! Name for output listing
INTEGER, INTENT(IN)            :: KTCOUNT    ! temporal loop counter
TYPE (DATE_TIME),   INTENT(IN) :: TPDTCUR    ! Current date and time
TYPE (DATE_TIME),   INTENT(IN) :: TPDTMOD    ! Creation date and time
REAL,               INTENT(IN) :: PTSTEP     ! time step 
INTEGER,            INTENT(IN) :: KSV        ! Number of Scalar Variables
!
!-------------------------------------------------------------------------------
!
SELECT CASE(CBUTYPE)
!
!
!*	 1.     'CART' CASE
!               -----------
!
  CASE('CART','SKIP') 
!
!*	 1.1    storage of the budget fields 
!
    IF( MODULO(KTCOUNT+1,NBUSTEP*NBUWRNB) == 0 ) THEN  
      CALL WRITE_BUDGET(HFMDIAC,HLUOUT,TPDTCUR,TPDTMOD,PTSTEP, KSV )
!
!*	 1.2    resetting the budget arrays to 0.
!
      IF (ALLOCATED(XBURU)) XBURU=0.
      IF (ALLOCATED(XBURV)) XBURV=0.
      IF (ALLOCATED(XBURW)) XBURW=0.
      IF (ALLOCATED(XBURTH)) XBURTH=0.
      IF (ALLOCATED(XBURTKE)) XBURTKE=0.
      IF (ALLOCATED(XBURRV)) XBURRV=0.
      IF (ALLOCATED(XBURRC)) XBURRC=0.
      IF (ALLOCATED(XBURRR)) XBURRR=0.
      IF (ALLOCATED(XBURRI)) XBURRI=0.
      IF (ALLOCATED(XBURRS)) XBURRS=0.
      IF (ALLOCATED(XBURRG)) XBURRG=0.
      IF (ALLOCATED(XBURRH)) XBURRH=0.
      IF (ALLOCATED(XBURSV)) XBURSV=0.
      IF (ALLOCATED(XBURHODJU)) XBURHODJU=0.
      IF (ALLOCATED(XBURHODJV)) XBURHODJV=0.
      IF (ALLOCATED(XBURHODJW)) XBURHODJW=0.
      IF (ALLOCATED(XBURHODJ)) XBURHODJ =0.
!
!*	 1.3    reset  budget beginning flag to TRUE
!
      LBU_BEG=.TRUE.
    END IF
!
!
!*	 2.     'MASK' CASE
!               -----------
!  
  CASE('MASK')
    IF( MODULO(KTCOUNT+1,NBUSTEP*NBUWRNB) == 0 ) THEN  
!
!*	 2.1    storage of the budget fields
! 
      CALL WRITE_BUDGET(HFMDIAC,HLUOUT,TPDTCUR,TPDTMOD,PTSTEP, KSV)
!
!*	 2.2    reset the budget fields to 0.
!
      IF (ALLOCATED(XBURU)) XBURU=0.
      IF (ALLOCATED(XBURV)) XBURV=0.
      IF (ALLOCATED(XBURW)) XBURW=0.
      IF (ALLOCATED(XBURTH)) XBURTH=0.
      IF (ALLOCATED(XBURTKE)) XBURTKE=0.
      IF (ALLOCATED(XBURRV)) XBURRV=0.
      IF (ALLOCATED(XBURRC)) XBURRC=0.
      IF (ALLOCATED(XBURRR)) XBURRR=0.
      IF (ALLOCATED(XBURRI)) XBURRI=0.
      IF (ALLOCATED(XBURRS)) XBURRS=0.
      IF (ALLOCATED(XBURRG)) XBURRG=0.
      IF (ALLOCATED(XBURRH)) XBURRH=0.
      IF (ALLOCATED(XBURSV)) XBURSV=0.
      IF (ALLOCATED(XBURHODJU)) XBURHODJU=0.
      IF (ALLOCATED(XBURHODJV)) XBURHODJV=0.
      IF (ALLOCATED(XBURHODJW)) XBURHODJW=0.
      IF (ALLOCATED(XBURHODJ)) XBURHODJ =0.
!
      NBUTIME=0
!       
    END IF
!
!*	 2.3    update of the budget temporal increment and reset the budget
!               initialization
!
    IF( MODULO(KTCOUNT+1,NBUSTEP) == 0 ) THEN  
      NBUTIME=NBUTIME+1
      LBU_BEG=.TRUE.
    END IF
!
END SELECT            
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE ENDSTEP_BUDGET
