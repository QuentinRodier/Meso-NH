!MNH_LIC Copyright 1999-2020 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!     ###################
      MODULE MODI_TWO_WAY
!     ###################
!
INTERFACE 
!
      SUBROUTINE TWO_WAY   (     KRR,KSV,KTCOUNT,PRHODJ,KMI,PTSTEP,                &
                            PUM ,PVM, PWM, PTHM, PRM, PSVM,                        &
                            PRUS,PRVS,PRWS,PRTHS,PRRS,PRSVS,                       &
                            PINPRC,PINPRR,PINPRS,PINPRG,PINPRH,PPRCONV,PPRSCONV,   &
                            PDIRFLASWD,PSCAFLASWD,PDIRSRFSWD,OMASKkids             )
! 
INTEGER,                  INTENT(IN)  :: KRR     ! Number of moist variables
INTEGER,                  INTENT(IN)  :: KSV     ! Number of Scalar Variables
INTEGER,                  INTENT(IN)  :: KTCOUNT ! Temporal loop COUNTer
                                                 ! (=1 at the segment beginning)
INTEGER,                  INTENT(IN)  :: KMI     ! Model index     
!
REAL,                     INTENT(IN)  :: PTSTEP  ! Timestep duration
!
REAL, DIMENSION(:,:,:),   INTENT(IN)  :: PRHODJ         ! (Rho) dry * Jacobian
!
REAL, DIMENSION(:,:,:),   INTENT(IN)  :: PUM, PVM, PWM  ! Variables at t-dt 
REAL, DIMENSION(:,:,:),   INTENT(IN)  :: PTHM
REAL, DIMENSION(:,:,:,:), INTENT(IN)  :: PRM, PSVM
!
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PRUS, PRVS, PRWS         ! Source
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PRTHS
REAL, DIMENSION(:,:,:,:), INTENT(INOUT) :: PRRS, PRSVS              !  terms
!
REAL, DIMENSION(:,:), INTENT(INOUT)     :: PINPRC,PINPRR,PINPRS,PINPRG,PINPRH &
                                          ,PPRCONV,PPRSCONV     !  precipitating variables
LOGICAL, DIMENSION(:,:), INTENT(INOUT)  :: OMASKkids ! true where kids exist
REAL, DIMENSION(:,:,:), INTENT(INOUT)   :: PDIRFLASWD,PSCAFLASWD   ! Short wave radiation
REAL, DIMENSION(:,:,:), INTENT(INOUT)   :: PDIRSRFSWD

!
END SUBROUTINE TWO_WAY 
!
END INTERFACE
!
END MODULE MODI_TWO_WAY 
!
!     ########################################################################
      SUBROUTINE TWO_WAY   (     KRR,KSV,KTCOUNT,PRHODJ,KMI,PTSTEP,                &
                            PUM ,PVM, PWM, PTHM, PRM, PSVM,                        &
                            PRUS,PRVS,PRWS,PRTHS,PRRS,PRSVS,                       &
                            PINPRC,PINPRR,PINPRS,PINPRG,PINPRH,PPRCONV,PPRSCONV,   &
                            PDIRFLASWD,PSCAFLASWD,PDIRSRFSWD,OMASKkids             )
!     ########################################################################
!
!!****  *TWO_WAY* - relaxation toward the fine-mesh model result
!!
!!    PURPOSE
!!    -------
!!      The purpose of TWO_WAY  is to select the KID model and to call the
!!    subroutine TWO_WAY$n which effectively performs the relaxation toward the 
!!    fine-mesh model result. 
!
!
!!**  METHOD
!!    ------
!!      
!!       We choose the right KID model and CALL the appropriate TWO_WAY$n
!!
!!    EXTERNAL
!!    --------
!!       TWO_WAYY1,...,8 : performs the relaxation
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    MODULE MODD_:   
!!
!!    REFERENCE
!!    ---------
!!    
!!
!!    AUTHOR
!!    ------
!!    J. Stein  *Meteo-France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original     8/4/99 
!!      N. Asencio   18/07/05  Add the surface parameters : precipitating
!!                             hydrometeors, the Short and Long Wave 
!!                              + MASKkids array
!!                   20/05/06 Remove EPS
!  P. Wautelet 05/2016-04/2018: new data structures and calls for I/O
!  P. Wautelet   /02/2020: use the new data structures and subroutines for budgets
!
!------------------------------------------------------------------------------
!
!*      0.   DECLARATIONS
!            ------------
!
use modd_budget,     only: lbudget_u,  lbudget_v,  lbudget_w,  lbudget_th, lbudget_rv,  lbudget_rc,  &
                           lbudget_rr, lbudget_ri, lbudget_rs, lbudget_rg, lbudget_rh,  lbudget_sv,  &
                           NBUDGET_U,  NBUDGET_V,  NBUDGET_W,  NBUDGET_TH, NBUDGET_RV,  NBUDGET_RC,  &
                           NBUDGET_RR, NBUDGET_RI, NBUDGET_RS, NBUDGET_RG, NBUDGET_RH,  NBUDGET_SV1, &
                           tbudgets
USE MODD_CONF
USE MODD_NESTING

use mode_budget,     only: Budget_store_init, Budget_store_end
USE MODE_MODELN_HANDLER

USE MODI_TWO_WAY_n

IMPLICIT NONE
!
!
!
!*       0.1   declarations of arguments 
! 
!
INTEGER,                  INTENT(IN)  :: KRR     ! Number of moist variables
INTEGER,                  INTENT(IN)  :: KSV     ! Number of Scalar Variables
INTEGER,                  INTENT(IN)  :: KTCOUNT ! Temporal loop COUNTer
                                                 ! (=1 at the segment beginning)
INTEGER,                  INTENT(IN)  :: KMI     ! Model index     
!
REAL,                     INTENT(IN)  :: PTSTEP  ! Timestep duration
!
REAL, DIMENSION(:,:,:),   INTENT(IN)  :: PRHODJ         ! (Rho) dry * Jacobian
!
REAL, DIMENSION(:,:,:),   INTENT(IN)  :: PUM, PVM, PWM  ! Variables at t-dt 
REAL, DIMENSION(:,:,:),   INTENT(IN)  :: PTHM
REAL, DIMENSION(:,:,:,:), INTENT(IN)  :: PRM, PSVM
!
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PRUS, PRVS, PRWS         ! Source
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PRTHS
REAL, DIMENSION(:,:,:,:), INTENT(INOUT) :: PRRS, PRSVS              !  terms
!
REAL, DIMENSION(:,:), INTENT(INOUT)     :: PINPRC,PINPRR,PINPRS,PINPRG,PINPRH &
                                          ,PPRCONV,PPRSCONV     !  precipitating variables
LOGICAL, DIMENSION(:,:), INTENT(INOUT)  :: OMASKkids ! true where kids exist
REAL, DIMENSION(:,:,:), INTENT(INOUT)   :: PDIRFLASWD,PSCAFLASWD   ! Short wave radiation
REAL, DIMENSION(:,:,:), INTENT(INOUT)   :: PDIRSRFSWD
!
!*       0.2   declarations of local variables
!
INTEGER :: JKID        ! loop index to look for the KID models
INTEGER :: JSV,JRR     ! Loop index for scalar and moist variables
!
!-------------------------------------------------------------------------------

if ( lbudget_u  ) call Budget_store_init( tbudgets(NBUDGET_U ), 'NEST', prus (:, :, : ) )
if ( lbudget_v  ) call Budget_store_init( tbudgets(NBUDGET_V ), 'NEST', prvs (:, :, : ) )
if ( lbudget_w  ) call Budget_store_init( tbudgets(NBUDGET_W ), 'NEST', prws (:, :, : ) )
if ( lbudget_th ) call Budget_store_init( tbudgets(NBUDGET_TH), 'NEST', prths(:, :, : ) )
if ( lbudget_rv ) call Budget_store_init( tbudgets(NBUDGET_RV), 'NEST', prrs (:, :, :, 1) )
if ( lbudget_rc ) call Budget_store_init( tbudgets(NBUDGET_RC), 'NEST', prrs (:, :, :, 2) )
if ( lbudget_rr ) call Budget_store_init( tbudgets(NBUDGET_RR), 'NEST', prrs (:, :, :, 3) )
if ( lbudget_ri ) call Budget_store_init( tbudgets(NBUDGET_RI), 'NEST', prrs (:, :, :, 4) )
if ( lbudget_rs ) call Budget_store_init( tbudgets(NBUDGET_RS), 'NEST', prrs (:, :, :, 5) )
if ( lbudget_rg ) call Budget_store_init( tbudgets(NBUDGET_RG), 'NEST', prrs (:, :, :, 6) )
if ( lbudget_rh ) call Budget_store_init( tbudgets(NBUDGET_RH), 'NEST', prrs (:, :, :, 7) )
if ( lbudget_sv ) then
  do jsv = 1, ksv
    call Budget_store_init( tbudgets(jsv + NBUDGET_SV1 - 1), 'NEST', prsvs(:, :, :, jsv) )
  end do
end if
!
!*       1.    CALL THE RIGHT TWO_WAY$n
!              ------------------------
!
DO JKID = KMI+1,NMODEL  ! min value of the possible kids
  IF (KMI == NDAD(JKID) .AND. (XWAY(JKID) == 2. ) &
   .AND. (CCONF == 'RESTA' .OR. (CCONF == 'START' .AND. KTCOUNT /= 1))) THEN
    CALL GOTO_MODEL(JKID)
    CALL TWO_WAY_n (KRR,KSV,PRHODJ,KMI,PTSTEP,                             &
                    PUM ,PVM, PWM, PTHM, PRM, PSVM,                        &
                    PRUS,PRVS,PRWS,PRTHS,PRRS,PRSVS,                       &
                    PINPRC,PINPRR,PINPRS,PINPRG,PINPRH,PPRCONV,PPRSCONV,   &
                    PDIRFLASWD,PSCAFLASWD,PDIRSRFSWD,OMASKkids             )
  END IF
END DO
CALL GOTO_MODEL(KMI)
!
!*       2.    BUDGET COMPUTATION
!              ------------------
!
if ( lbudget_u  ) call Budget_store_end( tbudgets(NBUDGET_U ), 'NEST', prus (:, :, : ) )
if ( lbudget_v  ) call Budget_store_end( tbudgets(NBUDGET_V ), 'NEST', prvs (:, :, : ) )
if ( lbudget_w  ) call Budget_store_end( tbudgets(NBUDGET_W ), 'NEST', prws (:, :, : ) )
if ( lbudget_th ) call Budget_store_end( tbudgets(NBUDGET_TH), 'NEST', prths(:, :, : ) )
if ( lbudget_rv ) call Budget_store_end( tbudgets(NBUDGET_RV), 'NEST', prrs (:, :, :, 1) )
if ( lbudget_rc ) call Budget_store_end( tbudgets(NBUDGET_RC), 'NEST', prrs (:, :, :, 2) )
if ( lbudget_rr ) call Budget_store_end( tbudgets(NBUDGET_RR), 'NEST', prrs (:, :, :, 3) )
if ( lbudget_ri ) call Budget_store_end( tbudgets(NBUDGET_RI), 'NEST', prrs (:, :, :, 4) )
if ( lbudget_rs ) call Budget_store_end( tbudgets(NBUDGET_RS), 'NEST', prrs (:, :, :, 5) )
if ( lbudget_rg ) call Budget_store_end( tbudgets(NBUDGET_RG), 'NEST', prrs (:, :, :, 6) )
if ( lbudget_rh ) call Budget_store_end( tbudgets(NBUDGET_RH), 'NEST', prrs (:, :, :, 7) )
if ( lbudget_sv ) then
  do jsv = 1, ksv
    call Budget_store_end( tbudgets(jsv + NBUDGET_SV1 - 1), 'NEST', prsvs(:, :, :, jsv) )
  end do
end if
!------------------------------------------------------------------------------
!
END SUBROUTINE TWO_WAY
