!MNH_LIC Copyright 1994-2023 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!     #########################
      MODULE MODI_ADVECTION_UVW
!     #########################
!
INTERFACE
      SUBROUTINE ADVECTION_UVW (HUVW_ADV_SCHEME,                               &
                            HTEMP_SCHEME, KWENO_ORDER, OSPLIT_WENO,            &
                            HLBCX, HLBCY, PTSTEP,                              &
                            PUT, PVT, PWT,                                     &
                            PRHODJ, PDXX, PDYY, PDZZ, PDZX, PDZY,              &
                            PRUS, PRVS, PRWS,                                  &
                            PRUS_PRES, PRVS_PRES, PRWS_PRES                    )
!
CHARACTER(LEN=6),         INTENT(IN)    :: HUVW_ADV_SCHEME     ! to the selected
CHARACTER(LEN=4),         INTENT(IN)    :: HTEMP_SCHEME   ! Temporal scheme
!
INTEGER,                  INTENT(IN)    :: KWENO_ORDER   ! Order of the WENO
                                                         ! scheme (3 or 5)
LOGICAL,                  INTENT(IN)   :: OSPLIT_WENO  ! flag to add a time
                                                       ! splitting to RK for WENO
!
CHARACTER(LEN=4),DIMENSION(2),INTENT(IN):: HLBCX, HLBCY  ! X- and Y-direc LBC
!
REAL,                     INTENT(IN)    :: PTSTEP
!
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PUT , PVT  , PWT
                                                  ! Variables at t
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRHODJ
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PDXX,PDYY,PDZZ
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PDZX,PDZY
                                                  !  metric coefficients
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PRUS , PRVS, PRWS
                                                  ! Sources terms 
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRUS_PRES, PRVS_PRES, PRWS_PRES
!
END SUBROUTINE ADVECTION_UVW
!
END INTERFACE
!
END MODULE MODI_ADVECTION_UVW
!     ##########################################################################
      SUBROUTINE ADVECTION_UVW (HUVW_ADV_SCHEME,                               &
                            HTEMP_SCHEME, KWENO_ORDER, OSPLIT_WENO,            &
                            HLBCX, HLBCY, PTSTEP,                              &
                            PUT, PVT, PWT,                                     &
                            PRHODJ, PDXX, PDYY, PDZZ, PDZX, PDZY,              &
                            PRUS, PRVS, PRWS,                                  &
                            PRUS_PRES, PRVS_PRES, PRWS_PRES                    )
!     ##########################################################################
!
!!****  *ADVECTION_UVW * - routine to call the specialized advection routines for wind
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
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
!!	J.-P. Pinty      * Laboratoire d'Aerologie*
!!	J.-P. Lafore     * Meteo France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    06/07/94 
!!                  01/04/95 (Ph. Hereil J. Nicolau) add the model number
!!                  23/10/95 (J. Vila and JP Lafore) advection schemes scalar
!!                  16/01/97 (JP Pinty)              change presentation 
!!                  30/04/98 (J. Stein P Jabouille)  extrapolation for the cyclic
!!                                                   case and parallelisation
!!                  24/06/99 (P Jabouille)           case of NHALO>1
!!                  25/10/05 (JP Pinty)              4th order scheme
!!                  04/2011  (V. Masson & C. Lac)    splits the routine and adds
!!                                                   time splitting
!!                  J.Escobar 21/03/2013: for HALOK comment all NHALO=1 test
!!                  J.Escobar : 15/09/2015 : WENO5 & JPHEXT <> 1 
!!                  C.LAC 10/2016 : Add OSPLIT_WENO
!  P. Wautelet 20/05/2019: add name argument to ADDnFIELD_ll + new ADD4DFIELD_ll subroutine
!  P. Wautelet    02/2020: use the new data structures and subroutines for budgets
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_ARGSLIST_ll, ONLY : LIST_ll, HALO2LIST_ll
use modd_budget,      only: lbudget_u, lbudget_v, lbudget_w, NBUDGET_U, NBUDGET_V, NBUDGET_W, tbudgets
USE MODD_CONF,        ONLY : NHALO
USE MODD_PARAMETERS,  ONLY : JPVEXT

use mode_budget,      only: Budget_store_init, Budget_store_end
USE MODE_ll

USE MODI_ADV_BOUNDARIES
USE MODI_ADVECUVW_RK
USE MODI_CONTRAV
USE MODI_SHUMAN
!
!-------------------------------------------------------------------------------
!
IMPLICIT NONE
!
!*       0.1   Declarations of dummy arguments :
!
CHARACTER(LEN=6),         INTENT(IN)    :: HUVW_ADV_SCHEME     ! to the selected
CHARACTER(LEN=4),         INTENT(IN)    :: HTEMP_SCHEME   ! Temporal scheme
!
INTEGER,                  INTENT(IN)    :: KWENO_ORDER   ! Order of the WENO
                                                         ! scheme (3 or 5)
LOGICAL,                  INTENT(IN)   :: OSPLIT_WENO  ! flag to add a time
                                                       ! splitting to RK for WENO
!
CHARACTER(LEN=4),DIMENSION(2),INTENT(IN):: HLBCX, HLBCY  ! X- and Y-direc LBC
!
REAL,                     INTENT(IN)    :: PTSTEP
!
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PUT , PVT  , PWT
                                                  ! Variables at t
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRHODJ               
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PDXX,PDYY,PDZZ
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PDZX,PDZY
                                                  !  metric coefficients
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PRUS , PRVS, PRWS
                                                  ! Sources terms 
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRUS_PRES, PRVS_PRES, PRWS_PRES
!
!
!*       0.2   declarations of local variables
!
!
!  
INTEGER             :: IKE       ! indice K End       in z direction
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
!
REAL, DIMENSION(SIZE(PUT,1),SIZE(PUT,2),SIZE(PUT,3)) :: ZU, ZV, ZW
! Guesses at the end of the sub time step
REAL, DIMENSION(SIZE(PUT,1),SIZE(PUT,2),SIZE(PUT,3)) :: ZRUS_OTHER
REAL, DIMENSION(SIZE(PUT,1),SIZE(PUT,2),SIZE(PUT,3)) :: ZRVS_OTHER
REAL, DIMENSION(SIZE(PUT,1),SIZE(PUT,2),SIZE(PUT,3)) :: ZRWS_OTHER
! Contribution of the RK time step            
REAL, DIMENSION(SIZE(PUT,1),SIZE(PUT,2),SIZE(PUT,3)) :: ZRUS_ADV
REAL, DIMENSION(SIZE(PUT,1),SIZE(PUT,2),SIZE(PUT,3)) :: ZRVS_ADV
REAL, DIMENSION(SIZE(PUT,1),SIZE(PUT,2),SIZE(PUT,3)) :: ZRWS_ADV
REAL, DIMENSION(SIZE(PUT,1),SIZE(PUT,2),SIZE(PUT,3)) :: ZMXM_RHODJ
REAL, DIMENSION(SIZE(PUT,1),SIZE(PUT,2),SIZE(PUT,3)) :: ZMYM_RHODJ
REAL, DIMENSION(SIZE(PUT,1),SIZE(PUT,2),SIZE(PUT,3)) :: ZMZM_RHODJ
!
! Momentum tendencies due to advection
INTEGER :: ISPLIT              ! Number of splitting loops
INTEGER :: JSPL                ! Loop index
REAL    :: ZTSTEP              ! Sub Time step 
!
INTEGER                     :: IINFO_ll    ! return code of parallel routine
TYPE(LIST_ll), POINTER      :: TZFIELD_ll  ! list of fields to exchange
TYPE(LIST_ll), POINTER      :: TZFIELDS_ll ! list of fields to exchange
TYPE(LIST_ll), POINTER      :: TZFIELDS0_ll ! list of fields to exchange
!
!
!-------------------------------------------------------------------------------
!
!*       0.     INITIALIZATION                        
!	        --------------
!
IKE = SIZE(PWT,3) - JPVEXT
!
ZMXM_RHODJ = MXM(PRHODJ)
ZMYM_RHODJ = MYM(PRHODJ)
ZMZM_RHODJ = MZM(PRHODJ)

if ( lbudget_u ) call Budget_store_init( tbudgets(NBUDGET_U), 'ADV', prus(:, :, :) )
if ( lbudget_v ) call Budget_store_init( tbudgets(NBUDGET_V), 'ADV', prvs(:, :, :) )
if ( lbudget_w ) call Budget_store_init( tbudgets(NBUDGET_W), 'ADV', prws(:, :, :) )

!-------------------------------------------------------------------------------
!
!*       1.     COMPUTES THE CONTRAVARIANT COMPONENTS
!	        -------------------------------------
!
ZRUT = PUT(:,:,:) * ZMXM_RHODJ
ZRVT = PVT(:,:,:) * ZMYM_RHODJ
ZRWT = PWT(:,:,:) * ZMZM_RHODJ
!
NULLIFY(TZFIELD_ll)
!!$IF(NHALO == 1) THEN
  CALL ADD3DFIELD_ll( TZFIELD_ll, ZRUT, 'ADVECTION_UVW::ZRUT' )
  CALL ADD3DFIELD_ll( TZFIELD_ll, ZRVT, 'ADVECTION_UVW::ZRVT' )
  CALL UPDATE_HALO_ll(TZFIELD_ll,IINFO_ll)
  CALL CLEANLIST_ll(TZFIELD_ll)
!!$END IF
!
CALL CONTRAV (HLBCX,HLBCY,ZRUT,ZRVT,ZRWT,PDXX,PDYY,PDZZ,PDZX,PDZY,ZRUCT,ZRVCT,ZRWCT,4)
!
NULLIFY(TZFIELDS_ll)
!!$IF(NHALO == 1) THEN
  CALL ADD3DFIELD_ll( TZFIELDS_ll, ZRWCT, 'ADVECTION_UVW::ZRWCT' )
  CALL ADD3DFIELD_ll( TZFIELDS_ll, ZRUCT, 'ADVECTION_UVW::ZRUCT' )
  CALL ADD3DFIELD_ll( TZFIELDS_ll, ZRVCT, 'ADVECTION_UVW::ZRVCT' )
  CALL UPDATE_HALO_ll(TZFIELDS_ll,IINFO_ll)
  CALL CLEANLIST_ll(TZFIELDS_ll)
!!$END IF
!
!-------------------------------------------------------------------------------
!
!
!*       2.     COMPUTES THE TENDENCIES SINCE THE BEGINNING OF THE TIME STEP
!	        ------------------------------------------------------------
!
ZRUS_OTHER = PRUS - ZRUT / PTSTEP + PRUS_PRES
ZRVS_OTHER = PRVS - ZRVT / PTSTEP + PRVS_PRES
ZRWS_OTHER = PRWS - ZRWT / PTSTEP + PRWS_PRES
!
! Top and bottom Boundaries 
!
CALL ADV_BOUNDARIES (HLBCX, HLBCY, ZRUS_OTHER)
CALL ADV_BOUNDARIES (HLBCX, HLBCY, ZRVS_OTHER)
CALL ADV_BOUNDARIES (HLBCX, HLBCY, ZRWS_OTHER)
ZRWS_OTHER(:,:,IKE+1) = 0.

NULLIFY(TZFIELDS0_ll)
!!$IF(NHALO == 1) THEN
  CALL ADD3DFIELD_ll( TZFIELDS0_ll, ZRUS_OTHER, 'ADVECTION_UVW::ZRUS_OTHER' )
  CALL ADD3DFIELD_ll( TZFIELDS0_ll, ZRVS_OTHER, 'ADVECTION_UVW::ZRVS_OTHER' )
  CALL ADD3DFIELD_ll( TZFIELDS0_ll, ZRWS_OTHER, 'ADVECTION_UVW::ZRWS_OTHER' )
  CALL UPDATE_HALO_ll(TZFIELDS0_ll,IINFO_ll)
  CALL CLEANLIST_ll(TZFIELDS0_ll)
!!$END IF
!
!
!
!-------------------------------------------------------------------------------
!
IF ( HUVW_ADV_SCHEME == 'CEN4TH' ) THEN
  ISPLIT = 1
ELSE IF (OSPLIT_WENO) THEN
  ISPLIT = 2
ELSE
  ISPLIT = 1
END IF
ZTSTEP = PTSTEP / REAL(ISPLIT)
!
!-------------------------------------------------------------------------------
!
ZU    = PUT
ZV    = PVT
ZW    = PWT
!
!
!*       3.     TIME SPLITTING
!	        --------------
!
DO JSPL=1,ISPLIT
!
  CALL ADVECUVW_RK (HUVW_ADV_SCHEME,                                   &
                    HTEMP_SCHEME, KWENO_ORDER,                         &
                    HLBCX, HLBCY, ZTSTEP,                              &
                    ZU, ZV, ZW,                                        &
                    PUT, PVT, PWT,                                     &
                    ZMXM_RHODJ, ZMYM_RHODJ, ZMZM_RHODJ,                &
                    ZRUCT, ZRVCT, ZRWCT,                               &
                    ZRUS_ADV, ZRVS_ADV, ZRWS_ADV,                      &
                    ZRUS_OTHER, ZRVS_OTHER, ZRWS_OTHER                 )
!
! Tendencies on wind

  PRUS(:,:,:) = PRUS(:,:,:) + ZRUS_ADV(:,:,:) / ISPLIT
  PRVS(:,:,:) = PRVS(:,:,:) + ZRVS_ADV(:,:,:) / ISPLIT
  PRWS(:,:,:) = PRWS(:,:,:) + ZRWS_ADV(:,:,:) / ISPLIT

  IF (JSPL<ISPLIT) THEN
!
! Guesses for next time splitting loop
!
  ZU(:,:,:) = ZU(:,:,:) + ZTSTEP / ZMXM_RHODJ *  &
              (ZRUS_OTHER(:,:,:) + ZRUS_ADV(:,:,:))
  ZV(:,:,:) = ZV(:,:,:) + ZTSTEP / ZMYM_RHODJ *  &
              (ZRVS_OTHER(:,:,:) + ZRVS_ADV(:,:,:))
  ZW(:,:,:) = ZW(:,:,:) + ZTSTEP / ZMZM_RHODJ *  &
              (ZRWS_OTHER(:,:,:) + ZRWS_ADV(:,:,:))
!
! Top and bottom Boundaries 
!
  CALL ADV_BOUNDARIES (HLBCX, HLBCY, ZU, PUT, 'U' )    
  CALL ADV_BOUNDARIES (HLBCX, HLBCY, ZV, PVT, 'V' )    
  CALL ADV_BOUNDARIES (HLBCX, HLBCY, ZW, PWT, 'W' )
  ZW (:,:,IKE+1 ) = 0.
  END IF
!
! End of the time splitting loop
END DO
!
!
!*       4.     BUDGETS              
!	        -------
!
if ( lbudget_u ) call Budget_store_end( tbudgets(NBUDGET_U), 'ADV', prus(:, :, :) )
if ( lbudget_v ) call Budget_store_end( tbudgets(NBUDGET_V), 'ADV', prvs(:, :, :) )
if ( lbudget_w ) call Budget_store_end( tbudgets(NBUDGET_W), 'ADV', prws(:, :, :) )

!-------------------------------------------------------------------------------
!
END SUBROUTINE ADVECTION_UVW
