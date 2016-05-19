!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! MASDEV4_7 adiab 2006/05/18 13:07:25
!-----------------------------------------------------------------
!     ###########################
      MODULE MODI_ADVECUVW_4TH
!     ###########################
!
INTERFACE
!
      SUBROUTINE ADVECUVW_4TH ( HLBCX, HLBCY, PRUCT, PRVCT, PRWCT,           &
                                PUT, PVT, PWT, PRUS, PRVS, PRWS, TPHALO2LIST )              
!
USE MODD_ARGSLIST_ll, ONLY : HALO2LIST_ll
!
CHARACTER (LEN=4), DIMENSION(2), INTENT(IN) :: HLBCX ! X direction LBC type
CHARACTER (LEN=4), DIMENSION(2), INTENT(IN) :: HLBCY ! Y direction LBC type
!
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRUCT ! contravariant
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRVCT !  components
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRWCT ! of momentum
!
REAL, DIMENSION(:,:,:),   INTENT(IN) :: PUT, PVT, PWT        ! U,V,W at t
!
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PRUS, PRVS, PRWS     ! Source terms
!
TYPE(HALO2LIST_ll), POINTER :: TPHALO2LIST ! list for diffusion
!
END SUBROUTINE ADVECUVW_4TH
!
END INTERFACE
!
END MODULE MODI_ADVECUVW_4TH
!
!
!     ######################################################################
      SUBROUTINE ADVECUVW_4TH ( HLBCX, HLBCY, PRUCT, PRVCT, PRWCT,           &
                                PUT, PVT, PWT, PRUS, PRVS, PRWS, TPHALO2LIST )              
!     ######################################################################
!
!!****  *ADVECUVW_4TH * - routine to compute the 4th order centered
!!                           advection tendency of momentum (U,V,W)
!!
!!    PURPOSE
!!    -------
!!      The purpose of this routine is to call the ADVEC_4TH_ORDER_ALGO
!!    routine for the horizontal advection and the MZM4 and MZF4 functions for
!!    the vertical advection of momentum. The code is 
!!    parallelized and works for various boundary conditions.
!!
!!**  METHOD
!!    ------
!!      For each wind component the ADVECUVW_4TH routine calls
!!    the ADVEC_4TH_ORDER_ALGO routine which computes the numerical advection
!!    of any 3D field.
!!      The following variables are passed as argument to ADVEC_4TH_ORDER_ALGO :
!!
!!    -- The variable at t
!!    -- The second layer of the halo of the field at t
!!    -- The horizontal advection fluxes
!!    -- The localisation on the model grid :
!!
!!        IGRID = 1 for mass grid point
!!        IGRID = 2 for U grid point
!!        IGRID = 3 for V grid point
!!        IGRID = 4 for W grid point
!!
!!    EXTERNAL
!!    --------
!!      BUDGET      : Stores the different budget components
!!                    (not used in current version)
!!      ADVEC_4TH_ORDER_ALGO : computes the horizontal advection fluxes
!!      MZF4 and MZM4 : computes the vertical advection fluxes
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!    MODULE MODD_BUDGET:
!!         NBUMOD       : model in which budget is calculated
!!         CBUTYPE      : type of desired budget
!!                          'CART' for cartesian box configuration
!!                          'MASK' for budget zone defined by a mask
!!                          'NONE'  ' for no budget
!!         NBUPROCCTR   : process counter used for each budget variable
!!         Switches for budgets activations:
!!
!!         LBU_RU       : logical for budget of RU (wind component along x)
!!
!!         LBU_RU       : logical for budget of RU (wind component along x)
!!                        .TRUE. = budget of RU
!!                        .FALSE. = no budget of RU
!!         LBU_RV       : logical for budget of RV (wind component along y)
!!                        .TRUE. = budget of RV
!!                        .FALSE. = no budget of RV
!!         LBU_RW        : logical for budget of RW (wind component along z)
!!                        .TRUE. = budget of RW
!!                        .FALSE. = no budget of RW
!!         LBU_RTH      : logical for budget of RTH (potential temperature)
!!                        .TRUE. = budget of RTH
!!                        .FALSE. = no budget of RTH
!!         LBU_RTKE     : logical for budget of RTKE (turbulent kinetic energy)
!!                        .TRUE. = budget of RTKE
!!                        .FALSE. = no budget of RTKE
!!         LBU_RRV      : logical for budget of RRV (water vapor)
!!                        .TRUE. = budget of RRV
!!                        .FALSE. = no budget of RRV
!!         LBU_RRC      : logical for budget of RRC (cloud water)
!!                        .TRUE. = budget of RRC
!!                        .FALSE. = no budget of RRC
!!         LBU_RRR      : logical for budget of RRR (rain water)
!!                        .TRUE. = budget of RRR
!!                        .FALSE. = no budget of RRR
!!         LBU_RRI      : logical for budget of RRI (ice)
!!                        .TRUE. = budget of RRI
!!                        .FALSE. = no budget of RRI
!!         LBU_RRS      : logical for budget of RRS (snow)
!!                        .TRUE. = budget of RRS
!!                        .FALSE. = no budget of RRS
!!         LBU_RRG      : logical for budget of RRG (graupel)
!!                        .TRUE. = budget of RRG
!!                        .FALSE. = no budget of RRG
!!         LBU_RRH      : logical for budget of RRH (hail)
!!                        .TRUE. = budget of RRH
!!                        .FALSE. = no budget of RRH
!!         LBU_RSV      : logical for budget of RSVx (scalar variable)
!!                        .TRUE. = budget of RSVx
!!                        .FALSE. = no budget of RSVx
!!
!!    MODULE MODD_ARGSLIST
!!         HALO2LIST_ll : type for a list of "HALO2_lls"
!!
!!    REFERENCE
!!    ---------
!!      Book2 of documentation ( routine ADVECUVW_4TH )
!!
!!    AUTHOR
!!    ------
!!      J.-P. Pinty      * Laboratoire d'Aerologie*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original   25/10/05
!!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODE_ll
!
USE MODD_PARAMETERS
USE MODD_CONF
USE MODD_GRID_n
USE MODD_BUDGET
USE MODD_ARGSLIST_ll, ONLY : HALO2LIST_ll
!
USE MODI_SHUMAN
USE MODI_BUDGET
!
USE MODI_ADVEC_4TH_ORDER_AUX
!
IMPLICIT NONE
!
!*       0.1   Declarations of dummy arguments :
!
!
CHARACTER (LEN=4), DIMENSION(2), INTENT(IN) :: HLBCX ! X direction LBC type
CHARACTER (LEN=4), DIMENSION(2), INTENT(IN) :: HLBCY ! Y direction LBC type
!
!
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRUCT  ! contravariant
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRVCT  !  components
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRWCT  ! of momentum
!
REAL, DIMENSION(:,:,:),   INTENT(IN) :: PUT, PVT, PWT        ! Variables at t
!
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PRUS, PRVS, PRWS     ! Source terms
!
TYPE(HALO2LIST_ll), POINTER :: TPHALO2LIST ! list for diffusion
!
!*       0.2   Declarations of local variables :
!
INTEGER:: IIB,IJB        ! Begining useful area  in x,y,z directions
INTEGER:: IIE,IJE        ! End useful area in x,y,z directions
INTEGER :: IKU
!
TYPE(HALO2LIST_ll), POINTER :: TZHALO2LIST
!
INTEGER :: IGRID ! localisation on the model grid
REAL, DIMENSION(SIZE(PUT,1),SIZE(PUT,2),SIZE(PUT,3)) :: ZMEANX, ZMEANY ! fluxes
!
!-------------------------------------------------------------------------------
!
!*       1.     COMPUTES THE DOMAIN DIMENSIONS
!               ------------------------------
!
CALL GET_INDICE_ll(IIB,IJB,IIE,IJE)
!
IKU=SIZE(XZHAT)
!-------------------------------------------------------------------------------
!
!*       2.     CALL THE ADVEC_4TH_ORDER_ALGO ROUTINE FOR MOMENTUM
!               --------------------------------------------------
!
IGRID = 2
IF(NHALO == 1) THEN
  TZHALO2LIST => TPHALO2LIST
  CALL ADVEC_4TH_ORDER_ALGO(HLBCX, HLBCY, PUT, IGRID, ZMEANX, ZMEANY, &
                            TZHALO2LIST%HALO2 )
ELSE
  CALL ADVEC_4TH_ORDER_ALGO(HLBCX, HLBCY, PUT, IGRID, ZMEANX, ZMEANY)
ENDIF
!
PRUS(:,:,:) = PRUS(:,:,:)                          &
             -DXM( MXF(PRUCT(:,:,:))*ZMEANX(:,:,:) ) 
IF (LBUDGET_U)  CALL BUDGET (PRUS,1,'ADVX_BU_RU')
!
PRUS(:,:,:) = PRUS(:,:,:)                          &
             -DYF( MXM(PRVCT(:,:,:))*ZMEANY(:,:,:) ) 
IF (LBUDGET_U)  CALL BUDGET (PRUS,1,'ADVY_BU_RU')
!
PRUS(:,:,:) = PRUS(:,:,:)                             &
             -DZF(1,IKU,1, MXM(PRWCT(:,:,:))*MZM4(PUT(:,:,:)) )
IF (LBUDGET_U)  CALL BUDGET (PRUS,1,'ADVZ_BU_RU')
!
!
IGRID = 3
IF(NHALO == 1) THEN
  TZHALO2LIST => TZHALO2LIST%NEXT
  CALL ADVEC_4TH_ORDER_ALGO(HLBCX, HLBCY, PVT, IGRID, ZMEANX, ZMEANY, &
                            TZHALO2LIST%HALO2 )
ELSE
  CALL ADVEC_4TH_ORDER_ALGO(HLBCX, HLBCY, PVT, IGRID, ZMEANX, ZMEANY)
ENDIF
!
PRVS(:,:,:) = PRVS(:,:,:)                          &
             -DXF( MYM(PRUCT(:,:,:))*ZMEANX(:,:,:) ) 
IF (LBUDGET_V)  CALL BUDGET (PRVS,2,'ADVX_BU_RV')
!
PRVS(:,:,:) = PRVS(:,:,:)                          &
             -DYM( MYF(PRVCT(:,:,:))*ZMEANY(:,:,:) )  
IF (LBUDGET_V)  CALL BUDGET (PRVS,2,'ADVY_BU_RV')
!
PRVS(:,:,:) = PRVS(:,:,:)                             &
             -DZF(1,IKU,1, MYM(PRWCT(:,:,:))*MZM4(PVT(:,:,:)) )
IF (LBUDGET_V)  CALL BUDGET (PRVS,2,'ADVZ_BU_RV')
!
!
IGRID = 4
!
IF(NHALO == 1) THEN
  TZHALO2LIST => TZHALO2LIST%NEXT
  CALL ADVEC_4TH_ORDER_ALGO(HLBCX, HLBCY, PWT, IGRID, ZMEANX, ZMEANY, &
                            TZHALO2LIST%HALO2 )
ELSE
  CALL ADVEC_4TH_ORDER_ALGO(HLBCX, HLBCY, PWT, IGRID, ZMEANX, ZMEANY)
ENDIF
!
PRWS(:,:,:) = PRWS(:,:,:)                          &
             -DXF( MZM(1,IKU,1,PRUCT(:,:,:))*ZMEANX(:,:,:) ) 
IF (LBUDGET_W)  CALL BUDGET (PRWS,3,'ADVX_BU_RW')
!
PRWS(:,:,:) = PRWS(:,:,:)                          &
             -DYF( MZM(1,IKU,1,PRVCT(:,:,:))*ZMEANY(:,:,:) ) 
IF (LBUDGET_W)  CALL BUDGET (PRWS,3,'ADVY_BU_RW')
!
PRWS(:,:,:) = PRWS(:,:,:)                             &
             -DZM(1,IKU,1, MZF(1,IKU,1,PRWCT(:,:,:))*MZF4(PWT(:,:,:)) )
IF (LBUDGET_W)  CALL BUDGET (PRWS,3,'ADVZ_BU_RW')
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE ADVECUVW_4TH
