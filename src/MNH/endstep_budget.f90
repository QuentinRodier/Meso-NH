!MNH_LIC Copyright 1995-2021 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!##########################
 MODULE MODI_ENDSTEP_BUDGET
!##########################
!
INTERFACE
!
      SUBROUTINE ENDSTEP_BUDGET(TPDIAFILE,KTCOUNT,       &
                               TPDTCUR,PTSTEP,KSV)
!
USE MODD_IO, ONLY: TFILEDATA
USE MODD_TYPE_DATE
!
TYPE(TFILEDATA),   INTENT(IN) :: TPDIAFILE  ! file to write
INTEGER,           INTENT(IN) :: KTCOUNT    ! temporal loop counter
TYPE (DATE_TIME),  INTENT(IN) :: TPDTCUR    ! Current date and time
REAL,              INTENT(IN) :: PTSTEP     ! time step
INTEGER,           INTENT(IN) :: KSV        ! Number of Scalar Variables
!
END SUBROUTINE ENDSTEP_BUDGET  
!
END INTERFACE
!
END MODULE MODI_ENDSTEP_BUDGET
!
!     ####################################################
      SUBROUTINE ENDSTEP_BUDGET(TPDIAFILE,KTCOUNT,       &
                               TPDTCUR,PTSTEP,KSV)
!     ####################################################
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
!!      C.Lac       11/09/15 adaptation to FIT temporal scheme
!  P. Wautelet: 05/2016-04/2018: new data structures and calls for I/O
!  P. Wautelet 01-03/2020: use the new data structures and subroutines for budgets
!  P. Wautelet 25/01/2021: bugfix: do not call Write_budget at the beginning of the simulation
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_BUDGET
USE MODD_IO, ONLY: TFILEDATA
USE MODD_TIME
!
use mode_msg
use mode_write_budget, only: Write_budget
!
IMPLICIT NONE
!  
!*       0.1   Declarations of arguments :
!
TYPE(TFILEDATA),   INTENT(IN) :: TPDIAFILE  ! file to write
INTEGER,           INTENT(IN) :: KTCOUNT    ! temporal loop counter
TYPE (DATE_TIME),  INTENT(IN) :: TPDTCUR    ! Current date and time
REAL,              INTENT(IN) :: PTSTEP     ! time step
INTEGER,           INTENT(IN) :: KSV        ! Number of Scalar Variables

integer :: jbu, jgrp

!-------------------------------------------------------------------------------
!
call Print_msg( NVERB_DEBUG, 'BUD', 'Endstep_budget', 'called' )

IF ( KTCOUNT == 1 ) RETURN

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
    IF( MODULO(KTCOUNT,NBUSTEP*nbusubwrite) == 0 ) THEN
      call Write_budget( tpdiafile, tpdtcur, ptstep, ksv )
!
!*	 1.2    resetting the budget arrays to 0.
!
      ! Rhodj arrays
      if ( tbudgets(NBUDGET_U)%lenabled ) tbudgets(NBUDGET_U)%trhodj%xdata(:, :, :) = 0.
      if ( tbudgets(NBUDGET_V)%lenabled ) tbudgets(NBUDGET_V)%trhodj%xdata(:, :, :) = 0.
      if ( tbudgets(NBUDGET_W)%lenabled ) tbudgets(NBUDGET_W)%trhodj%xdata(:, :, :) = 0.
      ! Rhodj array for other budgets than U, V, W
      if ( associated( tburhodj ) ) tburhodj%xdata(:, :, :) = 0.
      ! Budget arrays
      do jbu = 1, nbudgets
        if ( tbudgets(jbu)%lenabled ) then
          do jgrp = 1, tbudgets(jbu)%ngroups
            tbudgets(jbu)%tgroups(jgrp)%xdata(:, :, : ) = 0.
          end do
        end if
      end do
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
    IF( MODULO(KTCOUNT,NBUSTEP*nbusubwrite) == 0 ) THEN
!
!*	 2.1    storage of the budget fields
! 
      call Write_budget( tpdiafile, tpdtcur, ptstep, ksv )
!
!*	 2.2    reset the budget fields to 0.
!
      ! Rhodj arrays
      if ( tbudgets(NBUDGET_U)%lenabled ) tbudgets(NBUDGET_U)%trhodj%xdata(:, :, :) = 0.
      if ( tbudgets(NBUDGET_V)%lenabled ) tbudgets(NBUDGET_V)%trhodj%xdata(:, :, :) = 0.
      if ( tbudgets(NBUDGET_W)%lenabled ) tbudgets(NBUDGET_W)%trhodj%xdata(:, :, :) = 0.
      ! Rhodj array for other budgets than U, V, W
      if ( associated( tburhodj ) ) tburhodj%xdata(:, :, :) = 0.
      ! Budget arrays
      do jbu = 1, nbudgets
        if ( tbudgets(jbu)%lenabled ) then
          do jgrp = 1, tbudgets(jbu)%ngroups
            tbudgets(jbu)%tgroups(jgrp)%xdata(:, :, : ) = 0.
          end do
        end if
      end do
!
      NBUTIME=0
!       
    END IF
!
!*	 2.3    update of the budget temporal increment and reset the budget
!               initialization
!
    IF( MODULO(KTCOUNT,NBUSTEP) == 0 ) THEN  
      NBUTIME=NBUTIME+1
      LBU_BEG=.TRUE.
    END IF
!
END SELECT            
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE ENDSTEP_BUDGET
