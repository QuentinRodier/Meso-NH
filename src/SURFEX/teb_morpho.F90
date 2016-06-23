!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ###########################################################################################################
      SUBROUTINE TEB_MORPHO(HPROGRAM, PBLD,PWALL_O_HOR, PGARDEN, PBLD_HEIGHT, PROAD, &
                            PROAD_O_GRND, PGARDEN_O_GRND, PWALL_O_GRND,              &
                            PCAN_HW_RATIO, PSVF_ROAD, PSVF_GARDEN, PSVF_WALL,        &
                            PZ0_TOWN, PWALL_O_BLD, PH_TRAFFIC, PLE_TRAFFIC           )
!     ###########################################################################################################
!
!!****  *TEB_MORPHO* 
!!
!!    PURPOSE
!!    -------
!!**** routine to verify and compute the canyon/building morphology in TEB
!!
!!**  METHOD
!!    ------
!! the routine controls the canyon/building morphology
!!    - in the case of low building fraction (lower than 10^-4)
!!    - in the case of high building fraction (higher than 0.9999)
!!    - building height
!!    - in the case of low road fraction
!!    - in the case of low/hight wall surface ratio 
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
!!      G. Pigeon   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    10/2011
!!      C. de Munck and A. lemonsu 05/2013 : - corrections in case of too high WALL_O_HOR (6.)
!!                                           - final check of parameters range added
!----------------------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODI_GET_LUOUT
USE MODI_ABOR1_SFX
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
 CHARACTER(LEN=6),     INTENT(IN)  :: HPROGRAM    ! program calling surf. schemes
REAL, DIMENSION(:),   INTENT(INOUT)  :: PBLD         ! Urban horizontal building density
REAL, DIMENSION(:),   INTENT(INOUT)  :: PWALL_O_HOR  ! Wall to horizontal surface ratio
REAL, DIMENSION(:),   INTENT(INOUT)  :: PGARDEN      ! Urban horizontal garden density
REAL, DIMENSION(:),   INTENT(INOUT)  :: PBLD_HEIGHT  ! Average building height [m]
REAL, DIMENSION(:),   INTENT(OUT)  :: PROAD  ! Urban horizontal road density
REAL, DIMENSION(:),   INTENT(OUT)  :: PROAD_O_GRND  ! Road relative surface over ground (road + garden)
REAL, DIMENSION(:),   INTENT(OUT)  :: PGARDEN_O_GRND  ! Garden relative surface over ground (road + garden)
REAL, DIMENSION(:),   INTENT(OUT)  :: PWALL_O_GRND  ! Wall relative surface over ground (road + garden)
REAL, DIMENSION(:),   INTENT(OUT)  :: PCAN_HW_RATIO  ! Urban canyon Height-Width ratio
REAL, DIMENSION(:),   INTENT(OUT)  :: PSVF_ROAD  ! road sky view factor
REAL, DIMENSION(:),   INTENT(OUT)  :: PSVF_GARDEN  ! garden sky view factor
REAL, DIMENSION(:),   INTENT(OUT)  :: PSVF_WALL  ! wall sky view factor
REAL, DIMENSION(:),   INTENT(OUT)  :: PZ0_TOWN  ! Urban roughness length
REAL, DIMENSION(:),   INTENT(OUT)  :: PWALL_O_BLD  ! Wall relative surface over ground (road + garden)
REAL, DIMENSION(:),   INTENT(INOUT)  :: PH_TRAFFIC   ! sensible heat flux due to traffic
REAL, DIMENSION(:),   INTENT(INOUT)  :: PLE_TRAFFIC  ! latent heat flux due to traffic
!
!*       0.2   Declarations of local variables
!
INTEGER :: JJ
INTEGER :: ILUOUT
!
REAL, DIMENSION(SIZE(PBLD)) :: ZWALL_O_BLD   ! Initial wall to built surface ratio
REAL, DIMENSION(SIZE(PBLD)) :: ZWALL_O_HOR   ! Initial wall to horizontal surface ratio
!
REAL, DIMENSION(2) :: ZRANGE_BLD        = (/ 0.0001  ,   0.9999 /) ! Range allowed for PBLD variation
REAL, DIMENSION(2) :: ZRANGE_ROAD       = (/ 0.0001  ,   0.9999 /) ! Range allowed for PROAD variation
REAL, DIMENSION(2) :: ZRANGE_BLD_HEIGHT = (/ 3.      , 829.84   /) ! Range allowed for PBLD_HEIGHT variation
REAL, DIMENSION(2) :: ZRANGE_WALL_O_HOR = (/ 0.00012 , 322.     /) ! Range allowed for PWALL_O_HOR variation
!
!
!*       1.   Get listing file for warnings
!
 CALL GET_LUOUT(HPROGRAM, ILUOUT)
!

ZWALL_O_BLD(:) = 0.
ZWALL_O_HOR(:) = 0.

DO JJ=1,SIZE(PBLD)
   !
   !*    2.   Control building height no lower than 3.m and no higher than 829.84m
   !          reference: http://en.wikipedia.org/wiki/List_of_tallest_buildings_and_structures_in_the_world (2011)
   !          and control Z0_TOWN
   !
   IF (PBLD_HEIGHT(JJ) < ZRANGE_BLD_HEIGHT(1) ) THEN
      PBLD_HEIGHT(JJ) = ZRANGE_BLD_HEIGHT(1)
   ENDIF
   IF (PBLD_HEIGHT(JJ) > ZRANGE_BLD_HEIGHT(2)) &
           CALL ABOR1_SFX('TEB_MORPHO: PBLD_HEIGHT higher than 829.84, highest building in the world, should be lower')
   !
   IF (PZ0_TOWN(JJ) > PBLD_HEIGHT(JJ)) THEN
      CALL ABOR1_SFX('TEB_MORPHO: PZ0_TOWN higher than PBLD_HEIGHT, should be lower')
   ENDIF
   !
   !*    3.   Control no and almost no building in the cell
   !          authorize building up to 10m and W_O_H 0.001
   !
   IF (PBLD(JJ) < ZRANGE_BLD(1) ) THEN
      PBLD(JJ) = ZRANGE_BLD(1)
      PGARDEN(JJ) = MIN(PGARDEN(JJ), 1.-2.*PBLD(JJ))
   ENDIF
   !
   !*    4.   Control only building in the cell: could occur for high resolution 
   !          theoretically W_O_H could be 0. -> impose that at least the wall surface is equal to the mesh perimeter x building 
   !          height for a mesh size of 100 x 100m; the waste heat is released at the roof level
   !
   IF (PBLD(JJ) > ZRANGE_BLD(2)) THEN
      PBLD(JJ) = ZRANGE_BLD(2)
      IF (PGARDEN(JJ) > 0.) THEN
         PGARDEN(JJ) = 0. 
      ENDIF
   ENDIF
   !
   !*    5.   Control wall surface low respective to building density and building height: pb of the input
   !          Evaluation of the minimum woh is done for mesh size of 1000. m
   !          wall surface of the building evaluated considering 1 square building
   !
   IF (PWALL_O_HOR(JJ) < 4. * SQRT(PBLD(JJ))*PBLD_HEIGHT(JJ)/1000.) THEN
      PWALL_O_HOR(JJ) = 4. * SQRT(PBLD(JJ))*PBLD_HEIGHT(JJ)/1000. 
   ENDIF
   !
   !*    6.   Control facade surface vs building height, case of too high WALL_O_HOR
   !
   PWALL_O_BLD(JJ) = PWALL_O_HOR(JJ)/PBLD(JJ)
   !
   IF (PWALL_O_BLD(JJ) > (0.4 * PBLD_HEIGHT(JJ))) THEN ! <=> side_of_building < 10 m
      !     
      ZWALL_O_HOR(JJ) = PWALL_O_HOR(JJ)
      ZWALL_O_BLD(JJ) = PWALL_O_BLD(JJ)
      !
      PWALL_O_HOR(JJ) = 0.4 * PBLD (JJ) * PBLD_HEIGHT(JJ) ! correction WOHOR v2.1
      PWALL_O_BLD(JJ) = PWALL_O_HOR(JJ) / PBLD       (JJ) ! correction WOHOR v2.1

   ENDIF
   !
   !*    7.   Verify road
   !
   PROAD      (JJ) = 1.-(PGARDEN(JJ)+PBLD(JJ))
   IF (PROAD(JJ) <= ZRANGE_ROAD(1) ) THEN
      PROAD(JJ) = ZRANGE_ROAD(1)
      PGARDEN(JJ) = MAX(PGARDEN(JJ) - ZRANGE_ROAD(1), 0.)
      IF (PH_TRAFFIC(JJ) > 0. .OR. PLE_TRAFFIC(JJ) > 0.) THEN
         PH_TRAFFIC(JJ)  = 0.
         PLE_TRAFFIC(JJ) = 0.
      ENDIF
   ENDIF
   !
   !*    8.   Final check of parameters range
   !
   IF ( PBLD(JJ) < ZRANGE_BLD(1) .OR. PBLD(JJ) > ZRANGE_BLD(2) ) THEN
        WRITE(ILUOUT,*) 'WARNING : PBLD is still out of range after final corrections &
        &for grid mesh',JJ,' : ',PBLD(JJ)
   ENDIF
   !
   IF ( PBLD_HEIGHT(JJ) < ZRANGE_BLD_HEIGHT(1) .OR. PBLD_HEIGHT(JJ) > ZRANGE_BLD_HEIGHT(2) ) THEN
        WRITE(ILUOUT,*) 'WARNING : PBLD_HEIGHT is still out of range after final corrections &
        &for grid mesh',JJ,' : ',PBLD_HEIGHT(JJ)
   ENDIF
   !
   IF ( PWALL_O_HOR(JJ) < ZRANGE_WALL_O_HOR(1) .OR. PWALL_O_HOR(JJ) > ZRANGE_WALL_O_HOR(2) ) THEN
        WRITE(ILUOUT,*) 'WARNING : PWALL_O_HOR is still out of range after final corrections &
        &for grid mesh',JJ,' : ',PWALL_O_HOR(JJ)
   ENDIF
   !
   IF ( PWALL_O_BLD(JJ) - (0.4 * PBLD_HEIGHT(JJ)) > 10E-16 ) THEN
        WRITE(ILUOUT,*) 'WARNING : PWALL_O_BLD is still too high after final corrections &
        &for grid mesh',JJ,' : ',PWALL_O_BLD(JJ)
   ENDIF
   !
ENDDO
!
!
!*    9.   Compute morphometric parameters 
!
PCAN_HW_RATIO(:)    = 0.5 * PWALL_O_HOR(:) / (1.-PBLD(:))
!
!* relative surface fraction
!
PROAD_O_GRND(:)   = PROAD(:)       / (PROAD(:) + PGARDEN(:))
PGARDEN_O_GRND(:) = PGARDEN(:)     / (PROAD(:) + PGARDEN(:))
PWALL_O_GRND(:)   = PWALL_O_HOR(:) / (PROAD(:) + PGARDEN(:))
!
!* Sky-view-factors:
!
PSVF_ROAD  (:) = (SQRT(PCAN_HW_RATIO(:)**2+1.) - PCAN_HW_RATIO(:))
PSVF_GARDEN(:) = PSVF_ROAD(:)
PSVF_WALL  (:) =  0.5*(PCAN_HW_RATIO(:)+1.-SQRT(PCAN_HW_RATIO(:)**2+1.))/PCAN_HW_RATIO(:)
!
END SUBROUTINE TEB_MORPHO
