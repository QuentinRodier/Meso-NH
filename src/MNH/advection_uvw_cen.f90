!MNH_LIC Copyright 2013-2023 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!     #####################
      MODULE MODI_ADVECTION_UVW_CEN
!     #####################
!
INTERFACE
      SUBROUTINE ADVECTION_UVW_CEN(HUVW_ADV_SCHEME,                &
                           HLBCX, HLBCY,                           &
                           PTSTEP, KTCOUNT,                        &
                           PUM, PVM, PWM,                          &
                           PDUM, PDVM, PDWM,                       &
                           PUT, PVT, PWT,                          &
                           PRHODJ, PDXX, PDYY, PDZZ, PDZX, PDZY,   &
                           PRUS,PRVS, PRWS,                        &
                           TPHALO2MLIST                            )
!
USE MODD_ARGSLIST_ll, ONLY : HALO2LIST_ll
!
CHARACTER(LEN=6),         INTENT(IN)    :: HUVW_ADV_SCHEME
!
CHARACTER(LEN=4),DIMENSION(2),INTENT(IN):: HLBCX, HLBCY  ! X- and Y-direc LBC
!
REAL,                     INTENT(IN)    :: PTSTEP!  time step
INTEGER,                  INTENT(IN)    :: KTCOUNT
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PUM, PVM, PWM
                                                  ! Variables at t-dt
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PDUM, PDVM, PDWM
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PUT , PVT  , PWT, PRHODJ
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PDXX,PDYY,PDZZ
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PDZX,PDZY
                                                  !  metric coefficients
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PRUS , PRVS  , PRWS
                                                  ! Sources terms 
!
! halo lists for 4th order advection
TYPE(HALO2LIST_ll), POINTER :: TPHALO2MLIST ! momentum variables
!
END SUBROUTINE ADVECTION_UVW_CEN
!
END INTERFACE
!
END MODULE MODI_ADVECTION_UVW_CEN
!     ##########################################################################
      SUBROUTINE ADVECTION_UVW_CEN(HUVW_ADV_SCHEME,                &
                           HLBCX, HLBCY,                           &
                           PTSTEP, KTCOUNT,                        &
                           PUM, PVM, PWM,                          &
                           PDUM, PDVM, PDWM,                       &
                           PUT, PVT, PWT,                          &
                           PRHODJ, PDXX, PDYY, PDZZ, PDZX, PDZY,   &
                           PRUS,PRVS, PRWS,                        &
                           TPHALO2MLIST                            )
!     ##########################################################################
!
!!****  *ADVECTION * - routine to call the specialized advection routines
!!
!!    PURPOSE
!!    -------
!!      The purpose of this routine is to control the advection routines.
!!    For that, it is first necessary to compute the metric coefficients
!!    and the contravariant components of the momentum.
!!
!!**  METHOD
!!    ------
!!      The advection of momenta is calculated using a centred (second order) 
!!    scheme. 
!!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!      NONE
!!
!!    REFERENCE
!!    ---------
!!      Book1 and book2 ( routine ADVECTION )
!!
!!    AUTHOR
!!    ------
!!	V. Masson        * Meteo France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2013  (from ADVECTION routine)
!!      Modif
!!      J.Escobar 21/03/2013: for HALOK comment all NHALO=1 test
!  P. Wautelet 20/05/2019: add name argument to ADDnFIELD_ll + new ADD4DFIELD_ll subroutine
!  P. Wautelet    02/2020: use the new data structures and subroutines for budgets
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_ARGSLIST_ll, ONLY : LIST_ll, HALO2LIST_ll
USE MODD_CONF
use modd_budget,      only: lbudget_u, lbudget_v, lbudget_w, NBUDGET_U, NBUDGET_V, NBUDGET_W, tbudgets
USE MODD_GRID_n
USE MODD_PARAMETERS

use mode_budget,     only: Budget_store_init, Budget_store_end
USE MODE_ll

USE MODI_ADVECUVW_2ND
USE MODI_ADVECUVW_4TH
USE MODI_CONTRAV
USE MODI_SHUMAN

!-------------------------------------------------------------------------------
!
IMPLICIT NONE
!
!*       0.1   Declarations of dummy arguments :
!
CHARACTER(LEN=6),         INTENT(IN)    :: HUVW_ADV_SCHEME
!
CHARACTER(LEN=4),DIMENSION(2),INTENT(IN):: HLBCX, HLBCY  ! X- and Y-direc LBC
!
REAL,                     INTENT(IN)    :: PTSTEP!  time step
INTEGER,                  INTENT(IN)    :: KTCOUNT
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PUM, PVM, PWM
                                                  ! Variables at t-dt
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PDUM, PDVM, PDWM
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PUT , PVT  , PWT, PRHODJ
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PDXX,PDYY,PDZZ
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PDZX,PDZY
                                                  !  metric coefficients
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PRUS , PRVS  , PRWS
                                                  ! Sources terms 
!
! halo lists for 4th order advection
TYPE(HALO2LIST_ll), POINTER :: TPHALO2MLIST ! momentum variables
!
!
!*       0.2   declarations of local variables
!
!
REAL, DIMENSION(SIZE(PUT,1),SIZE(PUT,2),SIZE(PUT,3)) :: ZUS
REAL, DIMENSION(SIZE(PUT,1),SIZE(PUT,2),SIZE(PUT,3)) :: ZVS 
REAL, DIMENSION(SIZE(PUT,1),SIZE(PUT,2),SIZE(PUT,3)) :: ZWS
                                                  ! guess of cartesian components of
                                                  ! momentum at future (+PTSTEP) timestep
REAL, DIMENSION(SIZE(PUT,1),SIZE(PUT,2),SIZE(PUT,3)) :: ZRUS
REAL, DIMENSION(SIZE(PUT,1),SIZE(PUT,2),SIZE(PUT,3)) :: ZRVS 
REAL, DIMENSION(SIZE(PUT,1),SIZE(PUT,2),SIZE(PUT,3)) :: ZRWS
                                                  ! cartesian components of
                                                  ! rhodJ times the tendency of
                                                  ! momentum from previous (-PTSTEP)
                                                  ! to future (+PTSTEP) timestep
!  
REAL, DIMENSION(SIZE(PUT,1),SIZE(PUT,2),SIZE(PUT,3)) :: ZRUT 
REAL, DIMENSION(SIZE(PUT,1),SIZE(PUT,2),SIZE(PUT,3)) :: ZRVT 
REAL, DIMENSION(SIZE(PUT,1),SIZE(PUT,2),SIZE(PUT,3)) :: ZRWT
                                                  ! cartesian 
                                                  ! components of
                                                  ! momentum
!
REAL, DIMENSION(SIZE(PUT,1),SIZE(PUT,2),SIZE(PUT,3)) :: ZRUCT 
REAL, DIMENSION(SIZE(PUT,1),SIZE(PUT,2),SIZE(PUT,3)) :: ZRVCT
REAL, DIMENSION(SIZE(PUT,1),SIZE(PUT,2),SIZE(PUT,3)) :: ZRWCT
                                                  ! contravariant
                                                  ! components
                                                  ! of momentum
REAL, DIMENSION(SIZE(PUT,1),SIZE(PUT,2),SIZE(PUT,3)) :: ZMXM_RHODJ
REAL, DIMENSION(SIZE(PUT,1),SIZE(PUT,2),SIZE(PUT,3)) :: ZMYM_RHODJ
REAL, DIMENSION(SIZE(PUT,1),SIZE(PUT,2),SIZE(PUT,3)) :: ZMZM_RHODJ
!
INTEGER                     :: IINFO_ll    ! return code of parallel routine
TYPE(LIST_ll), POINTER      :: TZFIELDS_ll ! list of fields to exchange

!
!-------------------------------------------------------------------------------
!
if ( lbudget_u ) call Budget_store_init( tbudgets(NBUDGET_U), 'ADV', prus(:, :, :) )
if ( lbudget_v ) call Budget_store_init( tbudgets(NBUDGET_V), 'ADV', prvs(:, :, :) )
if ( lbudget_w ) call Budget_store_init( tbudgets(NBUDGET_W), 'ADV', prws(:, :, :) )

ZMXM_RHODJ = MXM(PRHODJ)
ZMYM_RHODJ = MYM(PRHODJ)
ZMZM_RHODJ = MZM(PRHODJ)
!
!*       1.     COMPUTES THE CONTRAVARIANT COMPONENTS
!	        -------------------------------------
!
ZRUT = PUT(:,:,:) * ZMXM_RHODJ
ZRVT = PVT(:,:,:) * ZMYM_RHODJ
ZRWT = PWT(:,:,:) * ZMZM_RHODJ
!
IF (HUVW_ADV_SCHEME=='CEN2ND' ) THEN                                      
  CALL CONTRAV (HLBCX,HLBCY,ZRUT,ZRVT,ZRWT,PDXX,PDYY,PDZZ,PDZX,PDZY,ZRUCT,ZRVCT,ZRWCT,2)
ELSEIF (HUVW_ADV_SCHEME=='CEN4TH') THEN
  CALL CONTRAV (HLBCX,HLBCY,ZRUT,ZRVT,ZRWT,PDXX,PDYY,PDZZ,PDZX,PDZY,ZRUCT,ZRVCT,ZRWCT,4)
END IF

!
NULLIFY(TZFIELDS_ll)
!!$IF(NHALO == 1) THEN
  CALL ADD3DFIELD_ll( TZFIELDS_ll, ZRWCT, 'ADVECTION_UVW_CEN::ZRWCT' )
  CALL ADD3DFIELD_ll( TZFIELDS_ll, ZRUCT, 'ADVECTION_UVW_CEN::ZRUCT' )
  CALL ADD3DFIELD_ll( TZFIELDS_ll, ZRVCT, 'ADVECTION_UVW_CEN::ZRVCT' )
  CALL UPDATE_HALO_ll(TZFIELDS_ll,IINFO_ll)
  CALL CLEANLIST_ll(TZFIELDS_ll)
!!$END IF
!
!-------------------------------------------------------------------------------
!
!*       2.     TERM FROM PREVIOUS TIME-STEP (from initial_guess)
!	        ----------------------------
!
ZRUS(:,:,:)   = PUM(:,:,:)  * ZMXM_RHODJ/(2.*PTSTEP)
ZRVS(:,:,:)   = PVM(:,:,:)  * ZMYM_RHODJ/(2.*PTSTEP)
ZRWS(:,:,:)   = PWM(:,:,:)  * ZMZM_RHODJ/(2.*PTSTEP)
!
!-------------------------------------------------------------------------------
!
!*       3.     CALLS THE ADVECTION ROUTINES FOR THE MOMENTUM 
!	        ---------------------------------------------
!
! choose between 2nd and 4th order momentum advection.
IF (HUVW_ADV_SCHEME=='CEN2ND' ) THEN                                      
!
   CALL ADVECUVW_2ND (PUT,PVT,PWT,ZRUCT,ZRVCT,ZRWCT,ZRUS,ZRVS,ZRWS)
!
ELSEIF (HUVW_ADV_SCHEME=='CEN4TH') THEN
! 
   CALL ADVECUVW_4TH ( HLBCX, HLBCY, ZRUCT, ZRVCT, ZRWCT,            &
                       PUT, PVT, PWT, ZRUS, ZRVS, ZRWS, TPHALO2MLIST )                 
!
END IF
!
ZUS = ZRUS(:,:,:)/ZMXM_RHODJ*2.*PTSTEP
ZVS = ZRVS(:,:,:)/ZMYM_RHODJ*2.*PTSTEP
ZWS = ZRWS(:,:,:)/ZMZM_RHODJ*2.*PTSTEP
!-------------------------------------------------------------------------------
!
!*       5.     Extracts the variation between current and future time step
!	        -----------------------------------------------------------
!
PRUS(:,:,:) = PRUS(:,:,:) + ( ZUS(:,:,:) - PUM(:,:,:) - 0.5* PDUM) * ZMXM_RHODJ/(PTSTEP)
PRVS(:,:,:) = PRVS(:,:,:) + ( ZVS(:,:,:) - PVM(:,:,:) - 0.5* PDVM) * ZMYM_RHODJ/(PTSTEP)
PRWS(:,:,:) = PRWS(:,:,:) + ( ZWS(:,:,:) - PWM(:,:,:) - 0.5* PDWM) * ZMZM_RHODJ/(PTSTEP)
!
PDUM = ZUS(:,:,:) - PUM(:,:,:)
PDVM = ZVS(:,:,:) - PVM(:,:,:)
PDWM = ZWS(:,:,:) - PWM(:,:,:)

if ( lbudget_u ) call Budget_store_end( tbudgets(NBUDGET_U), 'ADV', prus(:, :, :) )
if ( lbudget_v ) call Budget_store_end( tbudgets(NBUDGET_V), 'ADV', prvs(:, :, :) )
if ( lbudget_w ) call Budget_store_end( tbudgets(NBUDGET_W), 'ADV', prws(:, :, :) )

!-------------------------------------------------------------------------------
!
END SUBROUTINE ADVECTION_UVW_CEN
