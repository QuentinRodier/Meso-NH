!     ######################################################################
      SUBROUTINE THERMAL_LAYERS_CONF(HTYPE,PHC,PTC,PD,PHC_OUT,PTC_OUT,PD_OUT)
!     ######################################################################
!
!!****  *THERMAL_LAYERS_CONF* 
!!
!!    PURPOSE
!!    -------
!     Adjust the thermal characteristics of the layers in road, wall, roof or
!     floor depending on the number of layers that the user wants to use during
!     the simulations.
!     Initial data are prescribed depending on user preference.
!     They have to be averaged on the layers use in the simulation
!  
!!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!	V. Masson   *Meteo France*	
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    05/2012
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_SURF_PAR, ONLY : XUNDEF
USE MODI_TEBGRID
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
 CHARACTER(LEN=5),     INTENT(IN)  :: HTYPE     ! type of surface
REAL, DIMENSION(:,:), INTENT(IN)  :: PHC       ! input Heat Capacity
REAL, DIMENSION(:,:), INTENT(IN)  :: PTC       ! input Thermal conductivity
REAL, DIMENSION(:,:), INTENT(IN)  :: PD        ! input Layer Thickness
REAL, DIMENSION(:,:), INTENT(OUT) :: PHC_OUT   ! output Heat Capacity
REAL, DIMENSION(:,:), INTENT(OUT) :: PTC_OUT   ! output Thermal conductivity
REAL, DIMENSION(:,:), INTENT(OUT) :: PD_OUT    ! output Layer Thickness
!
!*       0.2   Declarations of local variables
!
REAL, DIMENSION(SIZE(PHC,1))   :: ZD_TOT    ! Total depth
REAL, DIMENSION(SIZE(PHC,1))   :: ZD_HALF   ! Depth of the half of the total surface
!                                           ! (excluding central layer in case
!                                           ! of odd number of layers)
REAL, DIMENSION(SIZE(PHC,1))   :: ZD_MID    ! Thickness of the layer in the middle
!                                           ! in case of odd number of layers
REAL, DIMENSION(SIZE(PHC,1),0:SIZE(PHC    ,2))::ZD_IN  ! Depth from the surface 
!                                                      ! to the layer bottom
REAL, DIMENSION(SIZE(PHC,1),0:SIZE(PHC_OUT,2))::ZD_OUT ! Depth from the surface 
!                                                      ! to the layer bottom
REAL, DIMENSION(SIZE(PHC,1),SIZE(PHC,2))     :: ZW     ! 1/TC
REAL, DIMENSION(SIZE(PHC,1),SIZE(PHC_OUT,2)) :: ZW_OUT ! 1/TC
INTEGER                        :: IIN       ! Number of layer in input data
INTEGER                        :: IOUT      ! Number of layer in output fields
INTEGER                        :: JIN       ! Loop counter on input layers
INTEGER                        :: JOUT      ! Loop counter on output layers
!
REAL, PARAMETER                :: ZD_G1 = 0.001  ! uppermost soil layer 
!                                                ! thickness/depth       ( m)
!                                                ! Can not be too thin as 
!                                                ! then definition of soil
!                                                ! properties (i.e. phyiscal
!                                                ! representation of) and 
!                                                ! accuarcy of
!                                                ! numerical solution come
!                                                ! into question. If it is too
!                                                ! thick, then resolution of
!                                                ! diurnal cycle not as valid.
!-------------------------------------------------------------------------------
!
IIN = SIZE(PHC,2)
IOUT= SIZE(PHC_OUT,2)
!
!-------------------------------------------------------------------------------
!
!* Depths for the computational grid
!
!* total depth:
!
!
ZD_IN(:,0) = 0.
DO JIN=1,IIN
  ZD_IN(:,JIN) = ZD_IN(:,JIN-1) + PD(:,JIN)
END DO
ZD_TOT(:) = ZD_IN(:,IIN)
!
!* surface like road or floor (thin grid at the surface, coarse at the bottom)
!
IF (HTYPE=='ROAD ' .OR. HTYPE=='FLOOR') THEN
  ZD_OUT(:,0) = 0.
  CALL TEBGRID(ZD_TOT,ZD_OUT(:,1:),ZD_G1)
  PD_OUT(:,1) = ZD_OUT(:,1)
  DO JOUT=2,IOUT
    PD_OUT(:,JOUT) = ZD_OUT(:,JOUT) - ZD_OUT(:,JOUT-1) ! Depths => Thickness of layer
  END DO
ELSE
!
!* surface like roof or wall (thin grid on both sides, coarse in the middle)
!
  IF (MOD(IOUT,2)==0) THEN   ! even number of output layers
    ZD_HALF(:) = ZD_TOT(:) / 2.
  ELSE                       ! odd  number of output layers
    ZD_MID (:) = 2. * ZD_TOT(:) / IOUT ! middle layer is arbitrarily fixed
    IF (IOUT==3) ZD_MID=MAX(ZD_MID,ZD_TOT-2.*ZD_G1) ! to impose layers equal
                                                    ! to ZD_G1 on both sides
    ZD_HALF(:) = (ZD_TOT(:)-ZD_MID(:)) / 2.
    PD_OUT (:,IOUT/2+1) = ZD_MID (:)
  END IF
  ZD_OUT(:,0) = 0.
  CALL TEBGRID(ZD_HALF,ZD_OUT(:,1:IOUT/2),ZD_G1)
  PD_OUT(:,1) = ZD_OUT(:,1)
  DO JOUT=2,IOUT/2
    PD_OUT(:,JOUT) = ZD_OUT(:,JOUT) - ZD_OUT(:,JOUT-1) ! Depths => Thickness of layer
  END DO
  DO JOUT=1,IOUT/2
    PD_OUT(:,IOUT+1-JOUT) = PD_OUT(:,JOUT)
  END DO
  !* recomputes Depths for further averagings
  DO JOUT=2,IOUT
    ZD_OUT(:,JOUT) = ZD_OUT(:,JOUT-1) + PD_OUT(:,JOUT)
  END DO

END IF
!
DO JOUT=1,IOUT
  WHERE (PD(:,1)==XUNDEF)  PD_OUT(:,JOUT) = XUNDEF
END DO
!-------------------------------------------------------------------------------
!
!* Averaging of the Heat Capacity and the Thermal conductivity
!
ZW=1./PTC(:,:)
 CALL AV_THERMAL_DATA(PHC,ZW,PHC_OUT,ZW_OUT)
PTC_OUT=XUNDEF
WHERE (ZW_OUT/=XUNDEF) PTC_OUT=1./ZW_OUT
!
!-------------------------------------------------------------------------------
CONTAINS
!-------------------------------------------------------------------------------
SUBROUTINE AV_THERMAL_DATA(PF1,PF2,PF1_OUT,PF2_OUT)
REAL, DIMENSION(:,:), INTENT(IN)  :: PF1
REAL, DIMENSION(:,:), INTENT(IN)  :: PF2
REAL, DIMENSION(:,:), INTENT(OUT) :: PF1_OUT
REAL, DIMENSION(:,:), INTENT(OUT) :: PF2_OUT
!
REAL    :: ZF1! ponderated field
REAL    :: ZF2! ponderated field
REAL    :: ZS ! sum of weights
REAL    :: ZC ! coefficient of ponderation
REAL    :: ZD_LIM ! limit of previous layer that has been treated
!
INTEGER :: JL ! loop counter on spatial points
REAL    :: ZEPS=1.E-6
!
DO JL=1,SIZE(PF1,1)
 IF (PD(JL,1)==XUNDEF) THEN
   PF1_OUT(JL,:) = XUNDEF
   PF2_OUT(JL,:) = XUNDEF
   CYCLE
 END IF
 !
 ZF1 = 0.
 ZF2 = 0.
 ZS  = 0.
 JIN = 1
 JOUT= 1
 ZD_LIM = 0.
 DO
  IF (JOUT>IOUT) EXIT
  !
  IF (ZD_IN(JL,JIN)< ZD_OUT(JL,JOUT)-ZEPS) THEN
!    ZC = ZD_IN(JL,JIN) - MAX(ZD_IN(JL,JIN-1),ZD_OUT(JL,JOUT-1))
    ZC = ZD_IN(JL,JIN) - ZD_LIM
    ZF1 = ZF1 + ZC * PF1(JL,JIN)
    ZF2 = ZF2 + ZC * PF2(JL,JIN)
    ZS = ZS + ZC
    ZD_LIM = ZD_IN(JL,JIN)
    !
    JIN=JIN+1
  ELSE
!    ZC = ZD_OUT(JL,JOUT) - MAX(ZD_IN(JL,JIN-1),ZD_OUT(JL,JOUT-1))
    ZC = ZD_OUT(JL,JOUT) - ZD_LIM
    ZF1 = ZF1 + ZC * PF1(JL,JIN)
    ZF2 = ZF2 + ZC * PF2(JL,JIN)
    ZS = ZS + ZC
    PF1_OUT(JL,JOUT) = ZF1/ZS
    PF2_OUT(JL,JOUT) = ZF2/ZS
    ZD_LIM = ZD_OUT(JL,JOUT)
    !
    JOUT = JOUT+1
    ZF1 = 0.
    ZF2 = 0.
    ZS  = 0.
  END IF
 END DO
END DO
!
END SUBROUTINE
!
END SUBROUTINE THERMAL_LAYERS_CONF
