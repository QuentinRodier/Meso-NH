!-----------------------------------------------------------------
!########################
MODULE MODI_ION_BOUNDARIES
!########################
!
INTERFACE
!
      SUBROUTINE ION_BOUNDARIES (HLBCX,HLBCY,PUT,PVT,PSVT)

CHARACTER(LEN=4), DIMENSION(2), INTENT(IN)  :: HLBCX,HLBCY  
REAL, DIMENSION(:,:,:,:),       INTENT(INOUT) :: PSVT
REAL, DIMENSION(:,:,:),         INTENT(IN)  :: PUT,PVT
!
END SUBROUTINE ION_BOUNDARIES
!
END INTERFACE
!
END MODULE MODI_ION_BOUNDARIES
!
!
!     ####################################################################
      SUBROUTINE ION_BOUNDARIES (HLBCX,HLBCY,PUT,PVT,PSVT)
!     ####################################################################
!
!!****  *ION_BOUNDARIES* - routine to force the Lateral Boundary Conditions for
!!                 ion variables by the fair weather concentrations
!!
!!    PURPOSE
!!    -------
!
!!**  METHOD
!!    ------
!!      Only for 'OPEN' case  Boundary Condition type
!!   
!!    EXTERNAL 
!!    --------  
!!    GET_INDICE_ll  : get physical sub-domain bounds
!!    LWEAST_ll,LEAST_ll,LNORTH_ll,LSOUTH_ll : position functions
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------  
!!      Module MODD_PARAMETERS : 
!!        JPHEXT ,JPVEXT 
!!
!!      Module MODD_CONF :
!!        CCONF
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!          M. Chong     12/2010
!!	
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!         
USE MODD_PARAMETERS
USE MODD_CONF
USE MODD_NSV,  ONLY: NSV_ELECBEG, NSV_ELECEND, XSVMIN
USE MODD_ELEC_n, ONLY : XCION_POS_FW, XCION_NEG_FW
!
USE MODE_ll
!
IMPLICIT NONE
!
!
!*       0.1   declarations of arguments
!
!
CHARACTER(LEN=4), DIMENSION(2), INTENT(IN)  :: HLBCX,HLBCY  
REAL, DIMENSION(:,:,:,:),       INTENT(INOUT) :: PSVT
REAL, DIMENSION(:,:,:),         INTENT(IN)  :: PUT,PVT
!
!
!*       0.2   declarations of local variables
!
REAL, DIMENSION(SIZE(PUT,1), SIZE(PUT,2), SIZE(PUT,3)) :: ZUX, ZVY
INTEGER :: IIB, IIE  ! index of first and last inner mass points along x
INTEGER :: IJB, IJE  ! index of first and last inner mass points along y
!
!
!-------------------------------------------------------------------------------
!
!*       1.    COMPUTE DIMENSIONS OF ARRAYS AND OTHER INDICES:
!              ----------------------------------------------
!
! beginning and end indexes of the physical subdomain
IIB = 1 + JPHEXT
IIE = SIZE(PUT,1) - JPHEXT
IJB = 1 + JPHEXT
IJE = SIZE(PUT,2) - JPHEXT
!
!
!-------------------------------------------------------------------------------
!
!*       2.    EXTRAPOLATE VELOCITY COMPONENTS TO T + DT/2 (PPM advection scheme)
!              ------------------------------------------------------------------
!
ZUX =  PUT 
ZVY =  PVT 
!
!
!-------------------------------------------------------------------------------
!
!*       3.    LBC FILLING IN THE X DIRECTION (LEFT WEST SIDE):   
!              ------------------------------------------------
!
IF (LWEST_ll( ) .AND. HLBCX(1)=='OPEN') THEN
!
  WHERE ( ZUX(IIB,:,:) <= 0. )         !  OUTFLOW condition
    PSVT(IIB-1,:,:,NSV_ELECBEG) = MAX( 2.*PSVT(IIB,:,:,NSV_ELECBEG) -          &
                             PSVT(IIB+1,:,:,NSV_ELECBEG), XSVMIN(NSV_ELECBEG) )
    PSVT(IIB-1,:,:,NSV_ELECEND) = MAX( 2.*PSVT(IIB,:,:,NSV_ELECEND) -          &
                             PSVT(IIB+1,:,:,NSV_ELECEND), XSVMIN(NSV_ELECEND) )
  ELSEWHERE                            !  INFLOW
    PSVT(IIB-1,:,:,NSV_ELECBEG) = XCION_POS_FW(IIB,:,:)  ! Nb/kg
    PSVT(IIB-1,:,:,NSV_ELECEND) = XCION_NEG_FW(IIB,:,:)  ! Nb/kg
  ENDWHERE
END IF
!
!
!-------------------------------------------------------------------------------
!
!*       4.    LBC FILLING IN THE X DIRECTION (RIGHT EAST SIDE): 
!              -------------------------------------------------
!
IF (LEAST_ll( ) .AND. HLBCX(2)=='OPEN') THEN
! 
  WHERE ( ZUX(IIE+1,:,:) >= 0. )         !  OUTFLOW condition
    PSVT(IIE+1,:,:,NSV_ELECBEG) = MAX( 2.*PSVT(IIE,:,:,NSV_ELECBEG) -          &
                             PSVT(IIE-1,:,:,NSV_ELECBEG), XSVMIN(NSV_ELECBEG) )
    PSVT(IIE+1,:,:,NSV_ELECEND) = MAX( 2.*PSVT(IIE,:,:,NSV_ELECEND) -          &
                             PSVT(IIE-1,:,:,NSV_ELECEND), XSVMIN(NSV_ELECEND) )
  ELSEWHERE                              !  INFLOW
    PSVT(IIE+1,:,:,NSV_ELECBEG) = XCION_POS_FW(IIE,:,:)  ! Nb/kg
    PSVT(IIE+1,:,:,NSV_ELECEND) = XCION_NEG_FW(IIE,:,:)  ! Nb/kg
  ENDWHERE
END IF
!
!
!-------------------------------------------------------------------------------
!
!*       5.    LBC FILLING IN THE Y DIRECTION (BOTTOM SOUTH SIDE): 
!              ---------------------------------------------------
!
IF (LSOUTH_ll( ) .AND. HLBCY(1)=='OPEN') THEN
!
  WHERE ( ZVY(:,IJB,:) <= 0. )         !  OUTFLOW condition
    PSVT(:,IJB-1,:,NSV_ELECBEG) = MAX( 2.*PSVT(:,IJB,:,NSV_ELECBEG) -          &
                             PSVT(:,IJB+1,:,NSV_ELECBEG), XSVMIN(NSV_ELECBEG) )
    PSVT(:,IJB-1,:,NSV_ELECEND) = MAX( 2.*PSVT(:,IJB,:,NSV_ELECEND) -          &
                             PSVT(:,IJB+1,:,NSV_ELECEND), XSVMIN(NSV_ELECEND) )
  ELSEWHERE                            !  INFLOW
    PSVT(:,IJB-1,:,NSV_ELECBEG) = XCION_POS_FW(:,IJB,:)  ! Nb/kg
    PSVT(:,IJB-1,:,NSV_ELECEND) = XCION_NEG_FW(:,IJB,:)  ! Nb/kg
  ENDWHERE
END IF
!
!
!-------------------------------------------------------------------------------
!
!*       6.    LBC FILLING IN THE Y DIRECTION (TOP NORTH SIDE): 
!              ------------------------------------------------
!
IF (LNORTH_ll( ) .AND. HLBCY(2)=='OPEN') THEN
! 
  WHERE ( ZVY(:,IJE+1,:) >= 0. )         !  OUTFLOW condition
    PSVT(:,IJE+1,:,NSV_ELECBEG) = MAX( 2.*PSVT(:,IJE,:,NSV_ELECBEG) -          &
                             PSVT(:,IJE-1,:,NSV_ELECBEG), XSVMIN(NSV_ELECBEG) )
    PSVT(:,IJE+1,:,NSV_ELECEND) = MAX( 2.*PSVT(:,IJE,:,NSV_ELECEND) -          &
                             PSVT(:,IJE-1,:,NSV_ELECEND), XSVMIN(NSV_ELECEND) )
  ELSEWHERE                              !  INFLOW
    PSVT(:,IJE+1,:,NSV_ELECBEG) = XCION_POS_FW(:,IJE,:)  ! Nb/kg
    PSVT(:,IJE+1,:,NSV_ELECEND) = XCION_NEG_FW(:,IJE,:)  ! Nb/kg
  ENDWHERE
END IF
!
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE ION_BOUNDARIES
