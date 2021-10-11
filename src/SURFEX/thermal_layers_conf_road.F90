!     ######################################################################
      SUBROUTINE THERMAL_LAYERS_CONF_ROAD(HTYPE,                           &
                                          KTEB_SOIL, PTEB_SOILGRID,        &
                                          PHC, PTC, PD,                    &
                                          PCONDDRY_ROAD, PHCAPSOIL_ROAD,   &
                                          PHC_OUT,PTC_OUT,PD_OUT, KTEB_ROAD)
!     ######################################################################
!
!!****  *THERMAL_LAYERS_CONF_ROAD* 
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
USE MODD_SURF_PAR, ONLY : XUNDEF
!
IMPLICIT NONE
!
!*       0.    DECLARATIONS
!              ------------
!
!*       0.1   Declarations of arguments
!              -------------------------
!
 CHARACTER(LEN=5),     INTENT(IN)  :: HTYPE                      ! type of surface
INTEGER             , INTENT(IN)  :: KTEB_SOIL                  ! total number of soil layers for roads
REAL, DIMENSION(:)  , INTENT(IN)  :: PTEB_SOILGRID              ! vertical grid for roads
REAL, DIMENSION(:,:), INTENT(IN)  :: PHC                        ! input Heat Capacity for soil
REAL, DIMENSION(:,:), INTENT(IN)  :: PTC                        ! input Thermal conductivity for soil
REAL, DIMENSION(:,:), INTENT(IN)  :: PD                         ! input Layer Thickness
REAL, DIMENSION(:,:), INTENT(IN)  :: PCONDDRY_ROAD              ! input Heat Capacity
REAL, DIMENSION(:,:), INTENT(IN)  :: PHCAPSOIL_ROAD             ! input Thermal conductivity
REAL, DIMENSION(:,:), INTENT(OUT) :: PHC_OUT                    ! output Heat Capacity
REAL, DIMENSION(:,:), INTENT(OUT) :: PTC_OUT                    ! output Thermal conductivity
REAL, DIMENSION(:,:), INTENT(OUT) :: PD_OUT                     ! output Layer Thickness
INTEGER, INTENT(IN) :: KTEB_ROAD                  ! output Layer Number of structural road
!
!*       0.2   Declarations of local variables
!
!
REAL, DIMENSION(:,:), ALLOCATABLE :: ZDG_IN                     ! Depth from the surface to the layer bottom
REAL, DIMENSION(:,:), ALLOCATABLE :: ZDG_OUT                    ! Depth from the surface 
REAL, DIMENSION(:,:), ALLOCATABLE :: ZDG_TOT                    ! Complete grid including all levels (ranked) 
REAL, DIMENSION(:,:), ALLOCATABLE :: ZDZ_TOT                    ! Layer thickness for complete grid 
REAL, DIMENSION(:,:), ALLOCATABLE :: ZHC_TOT                    ! Thermal properties
REAL, DIMENSION(:,:), ALLOCATABLE :: ZTC_TOT
!
REAL, DIMENSION(:,:), ALLOCATABLE :: ZDG_TMP                    ! Complete grid including all levels 
REAL, DIMENSION(:,:), ALLOCATABLE :: ZDZ_TMP, ZHC_TMP, ZTC_TMP
!
INTEGER                           :: JI                         ! Loop counter
INTEGER                           :: JLAYER,JLAYER2             ! Loop counter
INTEGER                           :: IIN                        ! Number of layers for input grid
INTEGER                           :: IOUT                       ! Number of layers for output grid
INTEGER                           :: ITMP                       ! Number of layers for intermediate grid
INTEGER                           :: ITEB_ROAD1,ITEB_ROAD2      ! Number of layers for structural road
!
!-------------------------------------------------------------------------------
!
!
!*       0.     Initialization:
!               ---------------
!
! Number of vertical levels for grids
IIN  = SIZE(PHC,2)
IOUT = SIZE(PHC_OUT,2)
ITMP = IIN + IOUT
!
! Allocations
ALLOCATE(ZDG_IN (SIZE(PHC,1),IIN ))
ALLOCATE(ZDG_OUT(SIZE(PHC,1),IOUT))
ALLOCATE(ZDG_TMP(SIZE(PHC,1),ITMP))
ALLOCATE(ZDZ_TMP(SIZE(PHC,1),ITMP))
ALLOCATE(ZHC_TMP(SIZE(PHC,1),ITMP))
ALLOCATE(ZTC_TMP(SIZE(PHC,1),ITMP))
ALLOCATE(ZDG_TOT(SIZE(PHC,1),ITMP))
ALLOCATE(ZDZ_TOT(SIZE(PHC,1),ITMP))
ALLOCATE(ZHC_TOT(SIZE(PHC,1),ITMP))
ALLOCATE(ZTC_TOT(SIZE(PHC,1),ITMP))
!
!
! Definition of the output grid and cumulative depth
DO JI=1,SIZE(PHC,1)
  ZDG_OUT(JI,:) = PTEB_SOILGRID(:IOUT)
ENDDO
!
! Depth of structural road (as defined in the namelist by users)
ZDG_IN (:,1) = PD(:,1)
DO JLAYER=2,IIN
 ZDG_IN (:,JLAYER) = PD(:,JLAYER) + ZDG_IN(:,JLAYER-1)
ENDDO
!
!
!*       1.     Intermediate grid of computation
!               --------------------------------
!
!               Building of a new grid including all road and soil vertical levels
!               (for computation only)
!
ZDG_TOT(:,:) = XUNDEF
ZDG_TMP(:,:) = XUNDEF
!
DO JI=1,SIZE(PHC,1)
!
!         1.a  Definition of a cumulative grid with all layers
!              (= addition of vertical levels from input and output grids)
!
 ZDG_TMP(JI,:IIN  ) = ZDG_IN (JI,:)
 ZDG_TMP(JI,IIN+1:) = ZDG_OUT(JI,:)
!
!         1.b  Ranking of soil depth in ascending order
!              and removing the levels that are duplicated
!
 DO JLAYER=1,ITMP
  ZDG_TOT(JI,JLAYER) = MINVAL(ZDG_TMP(JI,:))
  IF (JLAYER.GT.1) THEN
    IF (ZDG_TOT(JI,JLAYER)==ZDG_TOT(JI,JLAYER-1)) ZDG_TOT(JI,JLAYER) = XUNDEF
  END IF
  ZDG_TMP(JI,MINLOC(ZDG_TMP(JI,:)))=XUNDEF
 ENDDO
!
 ZDG_TMP(JI,:) = XUNDEF 
 ZDG_TMP(JI,1:COUNT(ZDG_TOT(JI,:).NE.XUNDEF)) = PACK(ZDG_TOT(JI,:),MASK=ZDG_TOT(JI,:).NE.XUNDEF)
 ZDG_TOT(JI,:) = ZDG_TMP(JI,:)
!
ENDDO
!
!           1.c  Computation of layer thicknesses
!
ZDZ_TOT(:,:) = XUNDEF
ZDZ_TOT(:,1) = ZDG_TOT(:,1)
!
DO JI=1,SIZE(PHC,1)
 DO JLAYER=2,COUNT(ZDG_TOT(JI,:).NE.XUNDEF)
  ZDZ_TOT(JI,JLAYER) = ZDG_TOT(JI,JLAYER) - ZDG_TOT(JI,JLAYER-1)
 ENDDO
ENDDO
!
!           1.d  Re-Distribution of soil properties on the new grid : 
!                - Road properties for the first layers
!                - Soil properties for the lower layers
!
ZHC_TOT(:,:) = XUNDEF
ZTC_TOT(:,:) = XUNDEF
!
!                * Structural road properties are attributed to first layers 
!
!KTEB_ROAD(:) = 0
!KTEB_ROAD = 0
!
!
DO JI=1,SIZE(PHC,1)
!
 ITEB_ROAD1   = 0
 ITEB_ROAD2   = 0
!
 DO JLAYER=1,IIN
  DO JLAYER2=1,COUNT(ZDG_TOT(JI,:).NE.XUNDEF)
   IF (ZDG_IN(JI,JLAYER).GE.ZDG_TOT(JI,JLAYER2) .AND. ZHC_TOT(JI,JLAYER2)==XUNDEF) THEN
       ZHC_TOT(JI,JLAYER2) = PHC(JI,JLAYER)
       ZTC_TOT(JI,JLAYER2) = PTC(JI,JLAYER)
       ITEB_ROAD1          = JLAYER2
   ENDIF
  ENDDO
 ENDDO
!
!                * Natural soil properties are attributed below
!
 DO JLAYER=1,IOUT
  DO JLAYER2=1,ITMP
   IF (ZDG_OUT(JI,JLAYER).GE.ZDG_TOT(JI,JLAYER2) .AND. ZHC_TOT(JI,JLAYER2)==XUNDEF) THEN
       ZHC_TOT(JI,JLAYER2) = PHCAPSOIL_ROAD(JI,JLAYER)
       ZTC_TOT(JI,JLAYER2) = PCONDDRY_ROAD (JI,JLAYER)
       IF (ITEB_ROAD2.EQ.0) ITEB_ROAD2 = JLAYER2-1
   ENDIF
  ENDDO
 ENDDO
!
! IF (ITEB_ROAD1.EQ.ITEB_ROAD2) THEN 
!   !KTEB_ROAD(JI) = ITEB_ROAD1-1 
!   KTEB_ROAD = ITEB_ROAD1-1 
! ELSE IF (ITEB_ROAD1.LT.ITEB_ROAD2) THEN
!   !KTEB_ROAD(JI) = ITEB_ROAD1 
!   KTEB_ROAD = ITEB_ROAD1 
! ENDIF
!
ENDDO
!
!
!*       2.     Transfer to the reference grid
!               ------------------------------
!
!               Re-computation of soil properties on the reference grid : 
!
ZDZ_TMP(:,:) = 0.
ZHC_TMP(:,:) = 0.
ZTC_TMP(:,:) = 0.
!
DO JI=1,SIZE(PHC,1)
 !
 JLAYER2 = 1
 DO JLAYER=1,IOUT
  DO WHILE(ZDG_TOT(JI,JLAYER2).LE.ZDG_OUT(JI,JLAYER))
   ZDZ_TMP(JI,JLAYER) = ZDZ_TMP(JI,JLAYER) + ZDZ_TOT(JI,JLAYER2) 
   ZHC_TMP(JI,JLAYER) = ZHC_TMP(JI,JLAYER) + ZDZ_TOT(JI,JLAYER2)*ZHC_TOT(JI,JLAYER2)
   ZTC_TMP(JI,JLAYER) = ZTC_TMP(JI,JLAYER) + ZDZ_TOT(JI,JLAYER2)/ZTC_TOT(JI,JLAYER2) 
   JLAYER2 = JLAYER2+1
   IF (JLAYER2>ITMP) EXIT
  ENDDO
  !
  ZHC_TMP(JI,JLAYER) = ZHC_TMP(JI,JLAYER)/ZDZ_TMP(JI,JLAYER)
  ZTC_TMP(JI,JLAYER) = ZTC_TMP(JI,JLAYER)/ZDZ_TMP(JI,JLAYER)
  !
 ENDDO
ENDDO
!
PD_OUT (:,1) = ZDG_OUT(:,1)
DO JLAYER=2,IOUT
 PD_OUT(:,JLAYER) = ZDG_OUT(:,JLAYER)-ZDG_OUT(:,JLAYER-1)
ENDDO
!
PHC_OUT(:,:) = ZHC_TMP(:,1:IOUT)
PTC_OUT(:,:) = 1./ZTC_TMP(:,1:IOUT)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE THERMAL_LAYERS_CONF_ROAD
