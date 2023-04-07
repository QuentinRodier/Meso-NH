!MNH_LIC Copyright 2020-2021 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!     #######################
       MODULE MODI_EOL_MAIN
!     #######################
!
INTERFACE
!
SUBROUTINE EOL_MAIN(KTCOUNT, PTSTEP,         &
                    PDXX, PDYY, PDZZ,        &
                    PRHODJ, PUT, PVT, PWT,   &
                    PRUS, PRVS, PRWS         )
!
INTEGER,                  INTENT(IN)    :: KTCOUNT                  ! iteration count
REAL,                     INTENT(IN)    :: PTSTEP                   ! timestep except
!
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PDXX,PDYY,PDZZ           ! mesh size
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRHODJ                   ! dry Density * Jacobian
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PUT, PVT, PWT            ! wind speed variables
!
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PRUS, PRVS, PRWS         ! Sources of Momentum
!
END SUBROUTINE EOL_MAIN
!
END INTERFACE
!
END MODULE MODI_EOL_MAIN
!
!     ###################################################################
        SUBROUTINE EOL_MAIN(KTCOUNT, PTSTEP,        &
                            PDXX, PDYY, PDZZ,       &
                            PRHODJ, PUT, PVT, PWT,  &
                            PRUS, PRVS, PRWS        ) 
!     ###################################################################
!
!!****  *EOL_MAIN * -
!!
!!    PURPOSE
!!    -------
!!       It is possible to include wind turbines parameterization in Meso-NH,
!!       and several models are available. EOL_MAIN is the main subroutine 
!!       to compute the aerodynamics of the wind turbine, according to the
!!       model chosen.
!! 
!!**  METHOD
!!    ------
!!
!!    REFERENCE
!!    ---------
!!      
!!
!!    AUTHOR
!!    ------
!!     PA. Joulin 		*CNRM & IFPEN*
!!
!!    MODIFICATIONS
!!    -------------
!!     21/10/20      Original
!!        09/22      H. Toumi : adding ADR model
!!
!!---------------------------------------------------------------
!
!
!*       0.    DECLARATIONS
!              ------------
!
! To work with wind turbines
USE MODD_EOL_MAIN
USE MODI_EOL_ADNR
USE MODI_EOL_ADR
USE MODI_EOL_ALM
USE MODI_EOL_SMEAR
! To play with MPI
USE MODD_ARGSLIST_ll, ONLY: LIST_ll
USE MODE_ll         , ONLY: ADD3DFIELD_ll
USE MODE_ll         , ONLY: UPDATE_HALO_ll
USE MODE_ll         , ONLY: CLEANLIST_ll
! To use some toolkit
USE MODI_SHUMAN     , ONLY: MXF, MYF, MZF
USE MODI_SHUMAN     , ONLY: MXM, MYM, MZM
!
use modd_budget, only: lbudget_u, lbudget_v, lbudget_w, NBUDGET_U, NBUDGET_V, NBUDGET_W, tbudgets
use mode_budget, only: budget_store_init, budget_store_end
!
!
IMPLICIT NONE
!
!*       0.1   Declarations of dummy arguments :
!
INTEGER, INTENT(IN)                   :: KTCOUNT          ! iteration count
REAL,    INTENT(IN)                   :: PTSTEP           ! timestep except
REAL, DIMENSION(:,:,:), INTENT(IN)    :: PDXX,PDYY,PDZZ   ! mesh size
REAL, DIMENSION(:,:,:), INTENT(IN)    :: PRHODJ           ! dry Density * Jacobian
REAL, DIMENSION(:,:,:), INTENT(IN)    :: PUT, PVT, PWT    ! Wind speed 
REAL, DIMENSION(:,:,:), INTENT(INOUT) :: PRUS, PRVS, PRWS ! Sources of Momentum
!
!
!*       0.2   Declarations of local variables :
!
! Pointeurs and exchanges
TYPE(LIST_ll), POINTER :: TZFIELDS_W_ll  ! Field list of Wind for exchange
TYPE(LIST_ll), POINTER :: TZFIELDS_F_ll  ! Field list of aero Forces for exchange
TYPE(LIST_ll), POINTER :: TZFIELDS_S_ll  ! Field list of Smeared aero forces for exchange
TYPE(LIST_ll), POINTER :: TZFIELDS_R_ll  ! Field list of mnh foRces for exchange
INTEGER                :: IINFO          ! Info integer
INTEGER                :: IKU            ! Vertical size of the domain
!
! ABL
REAL, DIMENSION(SIZE(PUT,1),SIZE(PUT,2),SIZE(PUT,3)) :: ZUT_M, ZVT_M, ZWT_M ! Wind speed at mass point
REAL, DIMENSION(SIZE(PUT,1),SIZE(PUT,2),SIZE(PUT,3)) :: ZRHO_M              ! Air density at mass point
!
!
!*       0.3     Implicit arguments
!* From MODD_EOL_MAIN
! Aerodynamic forces in cartesian mesh
!REAL, DIMENSION(:,:,:),   ALLOCATABLE :: XFX_RG     ! Along X in RG frame [F]
!REAL, DIMENSION(:,:,:),   ALLOCATABLE :: XFY_RG     ! Along Y in RG frame [F]
!REAL, DIMENSION(:,:,:),   ALLOCATABLE :: XFZ_RG     ! Along Z in RG frame [F]
! Smeared forces 
!REAL, DIMENSION(:,:,:),   ALLOCATABLE :: XFX_SMR_RG ! Along X in RG frame [F]
!REAL, DIMENSION(:,:,:),   ALLOCATABLE :: XFY_SMR_RG ! Along Y in RG frame [F]
!REAL, DIMENSION(:,:,:),   ALLOCATABLE :: XFZ_SMR_RG ! ALong Z in RG frame [F]
!
!* From NAM_EOL namelist
!CHARACTER(LEN=4) :: CMETH_EOL     ! Aerodynamic method
!CHARACTER(LEN=4) :: CSMEAR        ! Type of smearing
!
!
!--------------------------------------------------------
!
!*      1.     INITIALIZATIONS
!              ---------------
!
!*       1.1    Indices
!
IKU = SIZE(PUT,3)           
!
!*       1.2    Pointers
!
NULLIFY(TZFIELDS_W_ll)
NULLIFY(TZFIELDS_F_ll)
NULLIFY(TZFIELDS_S_ll)
NULLIFY(TZFIELDS_R_ll)
!
!*       1.3    Forces
!
XFX_RG(:,:,:) = 0.
XFY_RG(:,:,:) = 0.
XFZ_RG(:,:,:) = 0.
XFX_SMR_RG(:,:,:) = 0.
XFY_SMR_RG(:,:,:) = 0.
XFZ_SMR_RG(:,:,:) = 0.
!
!
!-----------------------------------------------------------------------
! 
!*       2.     COMPUTES VELOCITY COMPONENTS AND DENSITY AT MASS POINT
!	        ------------------------------------------------------
!
!*       2.1    Sharing the input
! 
CALL ADD3DFIELD_ll( TZFIELDS_W_ll,PUT, 'EOL_MAIN::PUT')
CALL ADD3DFIELD_ll( TZFIELDS_W_ll,PWT, 'EOL_MAIN::PWT')
CALL ADD3DFIELD_ll( TZFIELDS_W_ll,PVT, 'EOL_MAIN::PVT')
CALL UPDATE_HALO_ll(TZFIELDS_W_ll,IINFO)
CALL CLEANLIST_ll(  TZFIELDS_W_ll)
!
!*       2.2    Masss point evaluation
!
ZUT_M(:,:,:)  = MXF( PUT(:,:,:) )
ZVT_M(:,:,:)  = MYF( PVT(:,:,:) )
ZWT_M(:,:,:)  = MZF( PWT(:,:,:) )
ZRHO_M(:,:,:) = PRHODJ(:,:,:)/(PDXX(:,:,:)*PDYY(:,:,:)*PDZZ(:,:,:))
!
!*       2.3    Sharing the new wind
!
CALL ADD3DFIELD_ll( TZFIELDS_W_ll,ZUT_M, 'EOL_MAIN::ZUT_M')
CALL ADD3DFIELD_ll( TZFIELDS_W_ll,ZWT_M, 'EOL_MAIN::ZWT_M')
CALL ADD3DFIELD_ll( TZFIELDS_W_ll,ZVT_M, 'EOL_MAIN::ZVT_M')
CALL UPDATE_HALO_ll(TZFIELDS_W_ll,IINFO)
CALL CLEANLIST_ll(  TZFIELDS_W_ll)
!
!
!--------------------------------------------------------
! 
!*       3.     COMPUTES AERODYNAMICS FORCES
!	        ----------------------------
!
!*       3.1    Model selection
!
!
SELECT CASE(CMETH_EOL)
! 
 CASE('ADNR') ! Actuator Disc Non-Rotating
  CALL EOL_ADNR(PDXX, PDYY, PDZZ,       & 
                ZRHO_M,                 &
                ZUT_M,                  &
                XFX_RG                  )
!
 CASE('ALM') ! Actuator Line Method
   CALL EOL_ALM(KTCOUNT, PTSTEP,        &
                PDXX, PDYY, PDZZ,       &
                ZRHO_M,                 &
                ZUT_M, ZVT_M, ZWT_M,    &
                XFX_RG, XFY_RG, XFZ_RG  )
!
 CASE('ADR') ! Actuator Disc Rotating
   CALL EOL_ADR(KTCOUNT, PTSTEP,        &
                PDXX, PDYY, PDZZ,       &
                ZRHO_M,                 & 
                ZUT_M, ZVT_M, ZWT_M,    &
                XFX_RG, XFY_RG, XFZ_RG  )
END SELECT
!
!*       3.2    Sharing 3D field
!
CALL ADD3DFIELD_ll( TZFIELDS_F_ll,XFX_RG, 'EOL_MAIN::XFX_RG' )
CALL ADD3DFIELD_ll( TZFIELDS_F_ll,XFY_RG, 'EOL_MAIN::XFY_RG' )
CALL ADD3DFIELD_ll( TZFIELDS_F_ll,XFZ_RG, 'EOL_MAIN::XFZ_RG' )
CALL UPDATE_HALO_ll(TZFIELDS_F_ll,IINFO)
CALL CLEANLIST_ll(  TZFIELDS_F_ll)
!
!
!--------------------------------------------------------
! 
!*       4.     SMEARING THE FORCES
!	        -------------------
!
!*       4.1    Smearing technique selection
!
SELECT CASE (CSMEAR)
!
 CASE( 'NULL' ) ! No smearing
  XFX_SMR_RG(:,:,:) = XFX_RG(:,:,:)
  XFY_SMR_RG(:,:,:) = XFY_RG(:,:,:)
  XFZ_SMR_RG(:,:,:) = XFZ_RG(:,:,:)
!
 CASE( '1LIN' ) ! Linear smearing
  CALL SMEAR_1LIN(XFX_RG,    &
                  XFX_SMR_RG)
!
 CASE( '3LIN' ) ! Linear smearing
  CALL SMEAR_3LIN(XFX_RG,    &
                  XFY_RG,    &
                  XFZ_RG,    &
                  XFX_SMR_RG,&
                  XFY_SMR_RG,&
                  XFZ_SMR_RG)
!
END SELECT
!
!*       4.2    Sharing 3D field
!
CALL ADD3DFIELD_ll( TZFIELDS_S_ll,XFX_SMR_RG, 'EOL_MAIN::XFX_SMR_RG' )
CALL ADD3DFIELD_ll( TZFIELDS_S_ll,XFY_SMR_RG, 'EOL_MAIN::XFY_SMR_RG' )
CALL ADD3DFIELD_ll( TZFIELDS_S_ll,XFZ_SMR_RG, 'EOL_MAIN::XFZ_SMR_RG' )
CALL UPDATE_HALO_ll(TZFIELDS_S_ll,IINFO)
CALL CLEANLIST_ll(  TZFIELDS_S_ll)
!
!
!-------------------------------------------------------------------------------
! 
!*       5.     ADDING THE FORCES TO THE FIELD
!	        ------------------------------
!
!*       5.1    Adding them to flux points, rotor->wind
!
if (lbudget_u) call Budget_store_init( tbudgets(NBUDGET_U), 'DRAGEOL', prus(:,:,:) )
if (lbudget_v) call Budget_store_init( tbudgets(NBUDGET_V), 'DRAGEOL', prvs(:,:,:) )
if (lbudget_w) call Budget_store_init( tbudgets(NBUDGET_W), 'DRAGEOL', prws(:,:,:) )
!
PRUS(:,:,:)=PRUS(:,:,:)-MXM(XFX_SMR_RG(:,:,:))
PRVS(:,:,:)=PRVS(:,:,:)-MYM(XFY_SMR_RG(:,:,:))
PRWS(:,:,:)=PRWS(:,:,:)-MZM(XFZ_SMR_RG(:,:,:))
!
if (lbudget_u) call Budget_store_end( tbudgets(NBUDGET_U), 'DRAGEOL', prus(:,:,:) )
if (lbudget_v) call Budget_store_end( tbudgets(NBUDGET_V), 'DRAGEOL', prvs(:,:,:) )
if (lbudget_w) call Budget_store_end( tbudgets(NBUDGET_W), 'DRAGEOL', prws(:,:,:) )
!
!
!*       5.2    Sharing the field
!
CALL ADD3DFIELD_ll( TZFIELDS_R_ll,PRUS,'EOL_MAIN::PRUS' )
CALL ADD3DFIELD_ll( TZFIELDS_R_ll,PRVS,'EOL_MAIN::PRVS' )
CALL ADD3DFIELD_ll( TZFIELDS_R_ll,PRWS,'EOL_MAIN::PRWS' )
CALL UPDATE_HALO_ll(TZFIELDS_R_ll,IINFO)
CALL CLEANLIST_ll(  TZFIELDS_R_ll)
!
END SUBROUTINE EOL_MAIN
