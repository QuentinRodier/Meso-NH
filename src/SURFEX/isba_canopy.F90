!     #########
SUBROUTINE ISBA_CANOPY(KI,KLVL,PZ,PZF,PDZ,PDZF,PHEIGHT,PCANOPY_DENSITY,PU,PTKE,   &
                        PUW_GROUND, PDUWDU_GROUND,                                &
                        PFORC_U,PDFORC_UDU,PFORC_E,PDFORC_EDE)  
!     ###############################################################################
!
!!****  *ISBA_CANOPY_n * - prepares forcing for canopy air model
!!
!!    PURPOSE
!!    -------
!
!!**  METHOD
!!    ------
!!
!!    REFERENCE
!!    ---------
!!      
!!
!!    AUTHOR
!!    ------
!!     V. Masson 
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    07/2006
!!---------------------------------------------------------------
!
USE MODD_ISBA_n  ,     ONLY : XCDRAG
USE MODD_CSTS,         ONLY : XRD, XCPD, XP00, XG
USE MODD_SURF_PAR,     ONLY : XUNDEF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_CANOPY
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
INTEGER,                  INTENT(IN)    :: KI        ! number of points
INTEGER,                  INTENT(IN)    :: KLVL      ! number of levels in canopy
REAL, DIMENSION(KI,KLVL), INTENT(IN)    :: PZ        ! heights of canopy levels              (m)
REAL, DIMENSION(KI,KLVL), INTENT(IN)    :: PZF       ! heights of bottom of canopy levels    (m)
REAL, DIMENSION(KI,KLVL), INTENT(IN)    :: PDZ       ! depth   of canopy levels              (m)
REAL, DIMENSION(KI,KLVL), INTENT(IN)    :: PDZF      ! depth between canopy levels           (m)
REAL, DIMENSION(KI),      INTENT(IN)    :: PHEIGHT     ! canopy height                       (m)
REAL, DIMENSION(KI),      INTENT(IN)    :: PCANOPY_DENSITY ! canopy density                  (-)

REAL, DIMENSION(KI,KLVL), INTENT(IN)    :: PU        ! wind for each canopy layer            (m/s)
REAL, DIMENSION(KI,KLVL), INTENT(IN)    :: PTKE      ! Tke  for each canopy layer            (m2/s2)
!
REAL, DIMENSION(KI),      INTENT(IN)    :: PUW_GROUND  ! friction flux for ground surface       (m2/s2)
REAL, DIMENSION(KI),      INTENT(IN)    :: PDUWDU_GROUND  ! derivative of ground friction flux   (m/s)
!
REAL, DIMENSION(KI,KLVL), INTENT(OUT)   :: PFORC_U   ! tendency of wind due to canopy drag   (m/s2)
REAL, DIMENSION(KI,KLVL), INTENT(OUT)   :: PDFORC_UDU! formal derivative of the tendency of
!                                                    ! wind due to canopy drag               (1/s)
REAL, DIMENSION(KI,KLVL), INTENT(OUT)   :: PFORC_E   ! tendency of TKE  due to canopy drag   (m2/s3)
REAL, DIMENSION(KI,KLVL), INTENT(OUT)   :: PDFORC_EDE! formal derivative of the tendency of
!                                                    ! TKE  due to canopy drag               (1/s)
!
!*      0.2    declarations of local variables
!
INTEGER                  :: JLAYER    ! loop counter on canopy heights
!         
REAL, DIMENSION(KI,KLVL) :: ZCDRAG    ! drag coefficient in canopy
REAL, DIMENSION(KI,KLVL) :: ZDENSITY  ! vegetation density for each canopy level
REAL, DIMENSION(KI,KLVL) :: ZSV       ! vertical surface for each canopy level
REAL, DIMENSION(KI,KLVL) :: ZFORC
REAL, DIMENSION(KI,KLVL) :: ZAIRVOL   ! Fraction of air for each canopy level total volume
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------
!
!*      1.     Computations of canopy grid characteristics
!              -------------------------------------------
!
!
!*      1.1    Proportion of leaves for each canopy level 
!             (parabolic shape, maximum at mid canopy height, with the same
!             total LAI on the canopy)
!
IF (LHOOK) CALL DR_HOOK('ISBA_CANOPY',0,ZHOOK_HANDLE)
ZDENSITY(:,:) = 0.
DO JLAYER=1,KLVL
  WHERE(PHEIGHT(:)>0.)  &
    ZDENSITY(:,JLAYER) = 1.5 * MAX(PCANOPY_DENSITY(:) * 4. * PZ(:,JLAYER) * (PHEIGHT(:)-PZ(:,JLAYER)) / PHEIGHT(:)**2, 0.)  
END DO
!
!*      2.1    Drag coefficient by vegetation (Patton et al 2001)
!              ------------------------------
!
ZCDRAG(:,:) = XCDRAG
!
!*      1.4    No building volume
!
! * in order to take into account building volume, further developments must be
!   done in the atmospheric model.
!   If these changes are not done, to take into account building volume in the
!   present routine alone would not be energetically coeherent (there would be
!   too much energy release for heat and vapor or consumed for wind).
!
ZAIRVOL(:,:) = 1.
!
!*      1.2    Discretization on each canopy level
!
 CALL CANOPY(KI, KLVL, PZF, PDZ, PHEIGHT, ZDENSITY, ZCDRAG, PU, ZAIRVOL, &
            ZSV, ZFORC, PFORC_U, PDFORC_UDU, PFORC_E, PDFORC_EDE )
!
!
!*      2.4    Drag force by ground surface
!              ----------------------------
!
PFORC_U   (:,1) = PUW_GROUND(:) / PDZ(:,1)
PDFORC_UDU(:,1) = PDFORC_UDU(:,1) + PDUWDU_GROUND(:) / PDZ(:,1)

!-------------------------------------------------------------------------------------
!
!*      3.2    Destruction of TKE due to small-scale motions forced by leaves
!              --------------------------------------------------------------
!
! from Kanda and Hino (1994)
!
! Ext = - Cd * e * u  * Sv        trees
!
PFORC_E   (:,:) = PFORC_E    - 2.*PTKE(:,:)*ZFORC(:,:)
PDFORC_EDE(:,:) = PDFORC_EDE - 2.*ZFORC(:,:)
!
IF (LHOOK) CALL DR_HOOK('ISBA_CANOPY',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------------
!
END SUBROUTINE ISBA_CANOPY
