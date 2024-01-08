!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE AVG_URBAN_FLUXES(TOP, T, B, TPN, DMT,GDD, HPROGRAM, PTS_TWN, PEMIS_TWN, &
     PT_CAN, PQ_CAN, PT_LOWCAN, PQ_LOWCAN, PTA, PQA, PRHOA, PPS, PH_TRAFFIC,       &
     PLE_TRAFFIC, PWL_O_GRND, PESN_RF, PEMIS_GR, PLW_RAD, PAC_RF, PAC_RF_WAT,      &
     PAC_WL, PAC_RD, PAC_RD_WAT, PAC_TOP, PAC_GD, PQSAT_GD, PAC_AGG_GD, PHU_AGG_GD,&
     PT_HVEG, PQSAT_HVEG, PAC_HVEG, PHU_HVEG,                                      &
     PQSAT_RF, PQSAT_RD, PDELT_RF, PDELT_RD, PRF_FRAC, PWL_FRAC, PRD_FRAC,         &
     PGD_FRAC,PHVEG_FRAC, PDF_RF, PDN_RF,                                          &
     PDF_RD, PDN_RD, PLEW_RF, PLESN_RF, PLEW_RD, PLESN_RD, PHSN_RD, PTSRAD_GD,     &
     PEVAP_GD, PRUNOFF_GD, PEVAP_GR, PRUNOFF_GR, PDRAIN_GR,                        &
     PRN_GRND, PH_GRND, PLE_GRND, PGFLX_GRND, PRN_TWN, PH_TWN,                     &
     PH_TWN_SURF, PH_TWN_WALL, PH_TWN_ROOF, PLE_TWN, PGFLX_TWN, PQF_TWN,           &
     PEVAP_TWN, PEVAP_TWN_SURF, PEVAP_TWN_WALL, PEVAP_TWN_ROOF, PEMIT_LW_RD,       &
     PEMIT_LW_GD, PEMIT_LW_GRND, PEMIS_GD, PCST_H_WASTE_CANY, PCST_LE_WASTE_CANY,  &
     PCOE_H_WASTE_CANY,PCOE_LE_WASTE_CANY, PMUL_H_WASTE_CANY, PMUL_LE_WASTE_CANY   )
!
!   ##########################################################################
!
!!****  *AVG_URBAN_FLUXES* computes fluxes on urbanized surfaces  
!!
!!    PURPOSE
!!    -------
!         
!     
!!**  METHOD
!     ------
!
!
!
!
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    MODD_CST
!!
!!      
!!    REFERENCE
!!    ---------
!!
!!      
!!    AUTHOR
!!    ------
!!
!!      V. Masson           * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    23/01/98 
!!                     12/02 (A. Lemonsu) modifications of emissivity and Tstown
!!                     07/07 (P. LeMoigne) expression of latent heat fluxes as 
!!                           functions of w'theta' instead of w'T'
!!                     17/10 (G. Pigeon)  computation of anthropogenic heat due
!!                           to domestic heating
!!                     10/11 (G. Pigeon) simplification for road, garden, roof,
!!                           wall fractions
!!                     08/13 (V. Masson) adds solar panels
!!                     07/15 (R. Schoetter) Implicitation of T_CANYON and Q_CANYON
!!                           with respect to (TI_BLD-TCANYON) and (QI_BLD-QCANYON).
!!                           Check for negative humidity.
!!                     01/16 (E.Redon/A.Lemonsu) adds high vegetation for emissivity calculation
!!                     10/16 (P. Marguinaud) Port to single precision
!!                     07/17 (M. Goret) add anthropogenic flux diagnostics
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_TEB_OPTION_n, ONLY : TEB_OPTIONS_t
USE MODD_TEB_n, ONLY : TEB_t
USE MODD_BEM_n, ONLY : BEM_t
USE MODD_TEB_PANEL_n, ONLY : TEB_PANEL_t
USE MODD_DIAG_MISC_TEB_n, ONLY : DIAG_MISC_TEB_t
USE MODD_DIAG_n, ONLY : DIAG_t
!
USE MODD_CSTS,ONLY : XCPD, XLVTT, XLSTT, XSTEFAN, XSURF_EPSILON
USE MODD_ISBA_PAR, ONLY : XEMISVEG
!
USE MODE_THERMOS
USE MODI_ABOR1_SFX
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
TYPE(TEB_OPTIONS_t), INTENT(INOUT) :: TOP
TYPE(TEB_t), INTENT(INOUT) :: T
TYPE(BEM_t), INTENT(INOUT) :: B
TYPE(TEB_PANEL_t), INTENT(INOUT) :: TPN
TYPE(DIAG_MISC_TEB_t), INTENT(INOUT) :: DMT
TYPE(DIAG_t),INTENT(IN) :: GDD
!
CHARACTER(LEN=6), INTENT(IN) :: HPROGRAM ! program calling surf. schemes
REAL, DIMENSION(:), INTENT(OUT)   :: PTS_TWN           ! town surface temperature
REAL, DIMENSION(:), INTENT(OUT)   :: PEMIS_TWN         ! town equivalent emissivity
REAL, DIMENSION(:), INTENT(INOUT) :: PT_CAN            ! canyon air temperature
REAL, DIMENSION(:), INTENT(INOUT) :: PQ_CAN            ! canyon air specific humidity
REAL, DIMENSION(:), INTENT(IN)    :: PT_LOWCAN         ! low canyon air temperature
REAL, DIMENSION(:), INTENT(IN)    :: PQ_LOWCAN         ! low canyon air specific humidity
!
REAL, DIMENSION(:), INTENT(IN)    :: PTA               ! temperature at roof level
REAL, DIMENSION(:), INTENT(IN)    :: PQA               ! specific humidity
                                                       ! at roof level
REAL, DIMENSION(:), INTENT(IN)    :: PRHOA             ! air density
                                                       ! at the lowest level
REAL, DIMENSION(:), INTENT(IN)    :: PPS               ! surface pressure
REAL, DIMENSION(:), INTENT(IN)    :: PH_TRAFFIC        ! anthropogenic sensible
!                                                      ! heat fluxes due to traffic
REAL, DIMENSION(:), INTENT(IN)    :: PLE_TRAFFIC       ! anthropogenic latent
!                                                      ! heat fluxes due to traffic
REAL, DIMENSION(:), INTENT(IN)    :: PWL_O_GRND        ! wall Surf. / ground (road+green) Surf.
!   
REAL, DIMENSION(:), INTENT(IN)    :: PESN_RF           ! snow roof emissivity
!
REAL, DIMENSION(:), INTENT(IN)    :: PEMIS_GR          ! green roof emissivity
!
REAL, DIMENSION(:), INTENT(IN)    :: PLW_RAD           ! incoming longwave rad.
!
REAL, DIMENSION(:), INTENT(IN)    :: PAC_RF            ! surface conductance
!                                                      ! for heat transfers
!                                                      ! above roofs
REAL, DIMENSION(:), INTENT(IN)    :: PAC_RF_WAT        ! surface conductance
!                                                      ! for heat transfers
!                                                      ! above roofs (for water)
REAL, DIMENSION(:), INTENT(IN)    :: PAC_WL            ! surface conductance
!                                                      ! for heat transfer
!                                                      ! between wall and canyon
REAL, DIMENSION(:), INTENT(IN)    :: PAC_RD            ! surface conductance
!                                                      ! for heat transfers
!                                                      ! between road and canyon
REAL, DIMENSION(:), INTENT(IN)    :: PAC_RD_WAT        ! surface conductance
!                                                      ! for heat transfers
!                                                      ! inside canyon (for water)
REAL, DIMENSION(:), INTENT(IN)    :: PAC_TOP           ! aerodynamical conductance
!                                                      ! between atmosphere and
!                                                      ! canyon top
REAL, DIMENSION(:), INTENT(IN)    :: PAC_GD            ! aerodynamical conductance
!                                                      ! between atmosphere and
!                                                      ! green areas
REAL, DIMENSION(:), INTENT(IN)    :: PQSAT_GD      ! q_sat(Ts)
REAL, DIMENSION(:), INTENT(IN)    :: PAC_AGG_GD    ! aggregated aerodyn resistance for green areas
REAL, DIMENSION(:), INTENT(IN)    :: PHU_AGG_GD    ! aggregated relative humidity for green areas
!
REAL, DIMENSION(:), INTENT(IN)    :: PT_HVEG       ! Tv : temperature of leaves of trees
REAL, DIMENSION(:), INTENT(IN)    :: PQSAT_HVEG    ! q_sat(Tv)
REAL, DIMENSION(:), INTENT(IN)    :: PAC_HVEG      ! aerodyn conductance for trees (sensible heat flux)
REAL, DIMENSION(:), INTENT(IN)    :: PHU_HVEG      ! relative humidity for trees
!
REAL, DIMENSION(:), INTENT(IN)    :: PQSAT_RF        ! q_sat(Ts)
REAL, DIMENSION(:), INTENT(IN)    :: PQSAT_RD        ! q_sat(Ts)
REAL, DIMENSION(:), INTENT(IN)    :: PDELT_RF        ! water fraction on snow-free
REAL, DIMENSION(:), INTENT(IN)    :: PDELT_RD        ! roof and roads
REAL, DIMENSION(:), INTENT(IN)    :: PRF_FRAC        ! roof, wall,
REAL, DIMENSION(:), INTENT(IN)    :: PWL_FRAC        ! road, green area,
REAL, DIMENSION(:), INTENT(IN)    :: PRD_FRAC        ! and high vegetation
REAL, DIMENSION(:), INTENT(IN)    :: PGD_FRAC        ! fractions
REAL, DIMENSION(:), INTENT(IN)    :: PHVEG_FRAC      ! of exchange surf.
REAL, DIMENSION(:), INTENT(IN)    :: PDF_RF          ! snow-free    roof fraction
REAL, DIMENSION(:), INTENT(IN)    :: PDN_RF          ! snow-covered roof fraction
REAL, DIMENSION(:), INTENT(IN)    :: PDF_RD          ! snow-free    road fraction
REAL, DIMENSION(:), INTENT(IN)    :: PDN_RD          ! snow-covered road fraction
!
REAL, DIMENSION(:), INTENT(IN)    :: PLEW_RF         ! latent heat flux of snowfree roof
REAL, DIMENSION(:), INTENT(IN)    :: PLESN_RF        ! latent heat flux over snow
REAL, DIMENSION(:), INTENT(IN)    :: PLEW_RD         ! latent heat flux of snowfree road
REAL, DIMENSION(:), INTENT(IN)    :: PLESN_RD        ! latent heat flux over snow
REAL, DIMENSION(:), INTENT(IN)    :: PHSN_RD         ! sensible heat flux over snow

REAL, DIMENSION(:), INTENT(IN)    :: PTSRAD_GD     ! green area surface temperature
REAL, DIMENSION(:), INTENT(IN)    :: PEVAP_GD      ! evaporation over ground vegetation
REAL, DIMENSION(:), INTENT(IN)    :: PRUNOFF_GD    ! surface runoff over ground vegetation  (kg/m2/s)
REAL, DIMENSION(:), INTENT(IN)    :: PEVAP_GR      ! evaporation over green roofs
REAL, DIMENSION(:), INTENT(IN)    :: PRUNOFF_GR    ! surface runoff over green roofs      (kg/m2/s)
REAL, DIMENSION(:), INTENT(IN)    :: PDRAIN_GR     ! outlet drainage at green roof base   (kg/m2/s)
!
!
REAL, DIMENSION(:), INTENT(OUT)   :: PRN_GRND         ! net radiation over ground
REAL, DIMENSION(:), INTENT(OUT)   :: PH_GRND          ! sensible heat flux over ground
REAL, DIMENSION(:), INTENT(OUT)   :: PLE_GRND         ! latent heat flux over ground
REAL, DIMENSION(:), INTENT(OUT)   :: PGFLX_GRND       ! flux through the ground
REAL, DIMENSION(:), INTENT(OUT)   :: PRN_TWN          ! net radiation over town
REAL, DIMENSION(:), INTENT(OUT)   :: PH_TWN           ! sensible heat flux over town
REAL, DIMENSION(:), INTENT(OUT)   :: PH_TWN_SURF      ! sensible heat flux over town, surface level
REAL, DIMENSION(:), INTENT(OUT)   :: PH_TWN_WALL      ! sensible heat flux over town, wall level
REAL, DIMENSION(:), INTENT(OUT)   :: PH_TWN_ROOF      ! sensible heat flux over town, roof level
REAL, DIMENSION(:), INTENT(OUT)   :: PLE_TWN          ! latent heat flux over town
REAL, DIMENSION(:), INTENT(OUT)   :: PGFLX_TWN        ! flux through the ground for town
REAL, DIMENSION(:), INTENT(OUT)   :: PQF_TWN          ! anthropogenic flux for town
REAL, DIMENSION(:), INTENT(OUT)   :: PEVAP_TWN        ! evaporation (kg/m2/s)
REAL, DIMENSION(:), INTENT(OUT)   :: PEVAP_TWN_SURF   ! evaporation flux, surface level (kg/m2/s)
REAL, DIMENSION(:), INTENT(OUT)   :: PEVAP_TWN_WALL   ! evaporation flux, wall level (kg/m2/s)
REAL, DIMENSION(:), INTENT(OUT)   :: PEVAP_TWN_ROOF   ! evaporation flux, roof level (kg/m2/s)
!
REAL, DIMENSION(:), INTENT(IN)    :: PEMIT_LW_RD   ! LW emitted by the road (W/m2 road)
REAL, DIMENSION(:), INTENT(IN)    :: PEMIT_LW_GD   ! LW emitted by the garden (W/m2 garden)
REAL, DIMENSION(:), INTENT(OUT)   :: PEMIT_LW_GRND ! LW emitted by the ground (road+garden) (W/m2 ground)
!
REAL, DIMENSION(:), INTENT(IN)    :: PEMIS_GD  ! garden emissivity
REAL, DIMENSION(:), INTENT(IN)    :: PCST_H_WASTE_CANY     ! sensible waste heat released to canyon
REAL, DIMENSION(:), INTENT(IN)    :: PCST_LE_WASTE_CANY    ! latent waste heat released to canyon
REAL, DIMENSION(:), INTENT(IN)    :: PCOE_H_WASTE_CANY
REAL, DIMENSION(:), INTENT(IN)    :: PCOE_LE_WASTE_CANY
REAL, DIMENSION(:), INTENT(IN)    :: PMUL_H_WASTE_CANY
REAL, DIMENSION(:), INTENT(IN)    :: PMUL_LE_WASTE_CANY
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(SIZE(PLW_RAD)) :: ZLW_UP            ! upwards radiations
REAL, DIMENSION(SIZE(T%XROAD)) :: ZRD, ZGD
!
REAL :: ZINTER
INTEGER :: JJ
INTEGER :: ILUOUT
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('AVG_URBAN_FLUXES',0,ZHOOK_HANDLE)
!
ZRD(:)=0.
ZGD(:)=0.
!
DO JJ=1,SIZE(T%XROAD)
!  
  IF (T%XROAD(JJ)+T%XGARDEN(JJ).NE.0.) THEN
    ZRD(JJ)  = T%XROAD(JJ)   / (T%XROAD(JJ)+T%XGARDEN(JJ))
    ZGD(JJ) =  T%XGARDEN(JJ) / (T%XROAD(JJ)+T%XGARDEN(JJ))
  ELSE
    ZRD(JJ)=0.
    ZGD(JJ)=0.
  ENDIF
!
!*      1.     Averaged fluxes for ground (green areas + road)
!              -----------------------------------------------
!
  PRN_GRND(JJ)    = ZRD(JJ) * DMT%XRN_ROAD   (JJ) 
!
  PH_GRND (JJ)    = ZRD(JJ) *  DMT%XH_ROAD   (JJ) + PH_TRAFFIC (JJ) / (1.-T%XBLD (JJ))
!
  PLE_GRND(JJ)    = ZRD(JJ) * DMT%XLE_ROAD   (JJ) + PLE_TRAFFIC(JJ) / (1.-T%XBLD (JJ))
!
  PGFLX_GRND(JJ)  = ZRD(JJ) * DMT%XGFLUX_ROAD(JJ)
!
  IF (TOP%LGARDEN) THEN
    PRN_GRND(JJ)    = PRN_GRND  (JJ) + ZGD(JJ) * GDD%XRN   (JJ)
!
    PH_GRND (JJ)    = PH_GRND   (JJ) + ZGD(JJ) * GDD%XH    (JJ)
!
    PLE_GRND(JJ)    = PLE_GRND  (JJ) + ZGD(JJ) * GDD%XLE   (JJ)
!
    PGFLX_GRND(JJ)  = PGFLX_GRND(JJ) + ZGD(JJ) * GDD%XGFLUX(JJ)
  END IF
  !
  IF (TOP%LSPARTACUS) THEN
     PEMIT_LW_GRND(JJ) = DMT%XEMIT_LW_GRND(JJ)
  ELSE
     PEMIT_LW_GRND(JJ) = T%XROAD_O_GRND(JJ) * PEMIT_LW_RD(JJ) + T%XGARDEN_O_GRND(JJ) * PEMIT_LW_GD(JJ)
  ENDIF
  !
!-------------------------------------------------------------------------------
!
!*      2.     Averaged fluxes JJ built + green areas
!              -------------------------------------
!
  PRN_TWN(JJ)    = PRF_FRAC(JJ) * DMT%XRN_ROOF   (JJ)                  &
                 + PRD_FRAC(JJ) * DMT%XRN_ROAD   (JJ)                  &
                 + PWL_FRAC(JJ) * DMT%XRN_WALL_A (JJ) * 0.5            &
                 + PWL_FRAC(JJ) * DMT%XRN_WALL_B (JJ) * 0.5   
!
  PH_TWN (JJ)    = PRF_FRAC(JJ) * DMT%XH_ROOF   (JJ)                   &
                 + PRD_FRAC(JJ) * DMT%XH_ROAD   (JJ)                   &
                 + PWL_FRAC(JJ) * DMT%XH_WALL_A (JJ) * 0.5             &
                 + PWL_FRAC(JJ) * DMT%XH_WALL_B (JJ) * 0.5             &
                 + PH_TRAFFIC(JJ) + T%XH_INDUSTRY(JJ)  
  !
  ! Dispatch between surface, wall, and roof level
  !
  PH_TWN_SURF(JJ) = PRD_FRAC(JJ) * DMT%XH_ROAD(JJ)  &
                 + PH_TRAFFIC(JJ) + T%XH_INDUSTRY(JJ)
  !
  PH_TWN_WALL (JJ) = PWL_FRAC(JJ) * DMT%XH_WALL_A(JJ) * 0.5  &
                   + PWL_FRAC(JJ) * DMT%XH_WALL_B(JJ) * 0.5 
  !
  PH_TWN_ROOF (JJ) = PRF_FRAC(JJ) * DMT%XH_ROOF(JJ)
  !
  ! Check sum of fluxes
  !
  IF ( ABS(PH_TWN(JJ)-PH_TWN_SURF(JJ)-PH_TWN_WALL(JJ)-PH_TWN_ROOF(JJ)).GT.1.0E-6 ) THEN
     CALL ABOR1_SFX ("AVG_URBAN_FLUX: Wrong dispatching of sensible heat flux")
  ENDIF
  !
  PLE_TWN(JJ)    = PRF_FRAC(JJ) * DMT%XLE_ROOF  (JJ)                   &
                 + PRD_FRAC(JJ) * DMT%XLE_ROAD  (JJ)                   &
                 + PWL_FRAC(JJ) * DMT%XLE_WALL_A(JJ) * 0.5        &
                 + PWL_FRAC(JJ) * DMT%XLE_WALL_B(JJ) * 0.5        &
                 + PLE_TRAFFIC (JJ) + T%XLE_INDUSTRY(JJ)  
!
  PGFLX_TWN(JJ)=  PRF_FRAC(JJ) * DMT%XGFLUX_ROOF  (JJ)                 &
                + PRD_FRAC(JJ) * DMT%XGFLUX_ROAD  (JJ)                 &
                + PWL_FRAC(JJ) * DMT%XGFLUX_WALL_A(JJ) * 0.5           &
                + PWL_FRAC(JJ) * DMT%XGFLUX_WALL_B(JJ) * 0.5    
!
  IF (TOP%LGARDEN) THEN
    !
    PRN_TWN(JJ)   = PRN_TWN(JJ)   +  PGD_FRAC  (JJ) * GDD%XRN     (JJ) + PHVEG_FRAC(JJ) * DMT%XRN_HVEG(JJ)
    PH_TWN (JJ)   = PH_TWN (JJ)   +  PGD_FRAC  (JJ) * GDD%XH      (JJ) + PHVEG_FRAC(JJ) * DMT%XH_HVEG (JJ)
    PLE_TWN(JJ)   = PLE_TWN(JJ)   +  PGD_FRAC  (JJ) * GDD%XLE     (JJ) + PHVEG_FRAC(JJ) * DMT%XLE_HVEG(JJ)
    PGFLX_TWN(JJ) = PGFLX_TWN(JJ) +  PGD_FRAC  (JJ) * GDD%XGFLUX  (JJ) + PHVEG_FRAC(JJ) * DMT%XGFLUX_HVEG(JJ)
    !
    ! Both garden and hveg fluxes are attributed to the surface fluxes
    !
    PH_TWN_SURF(JJ) = PH_TWN_SURF(JJ) + PGD_FRAC(JJ) * GDD%XH(JJ) + PHVEG_FRAC(JJ) * DMT%XH_HVEG(JJ)
    !
    ! Check sum of fluxes
    !
    IF ( ABS(PH_TWN(JJ)-PH_TWN_SURF(JJ)-PH_TWN_WALL(JJ)-PH_TWN_ROOF(JJ)).GT.1.0E-6 ) THEN
       CALL ABOR1_SFX ("AVG_URBAN_FLUX: Wrong dispatching of sensible heat flux")
    ENDIF
    !
  ENDIF
  !
  IF (TOP%LSOLAR_PANEL) THEN
    !
    PRN_TWN(JJ) = PRN_TWN(JJ) + PRF_FRAC(JJ) * DMT%XRN_PANEL(JJ) * TPN%XFRAC_PANEL(JJ)
    PH_TWN (JJ) = PH_TWN (JJ) + PRF_FRAC(JJ) * DMT%XH_PANEL (JJ) * TPN%XFRAC_PANEL(JJ)
    !
    PH_TWN_ROOF(JJ) = PH_TWN_ROOF(JJ) + PRF_FRAC(JJ) * DMT%XH_PANEL (JJ) * TPN%XFRAC_PANEL(JJ)
    !
    ! Check sum of fluxes
    !
    IF ( ABS(PH_TWN(JJ)-PH_TWN_SURF(JJ)-PH_TWN_WALL(JJ)-PH_TWN_ROOF(JJ)).GT.1.0E-6 ) THEN
       CALL ABOR1_SFX ("AVG_URBAN_FLUX: Wrong dispatching of sensible heat flux")
    ENDIF
    !
  ENDIF
  !
  PQF_TWN(JJ) = DMT%XQF_TOWN(JJ)
!-------------------------------------------------------------------------------
!
!*      3.     Infra-red Radiative properties
!              ------------------------------
!
!*      3.1    Upward IR radiation for town
!              ----------------------------
!
  ZLW_UP(JJ) = PLW_RAD  (JJ)      &
            - ( PRF_FRAC(JJ) * (1.-T%XGREENROOF(JJ)) * PDF_RF(JJ) * DMT%XABS_LW_ROOF     (JJ) &
               +PRF_FRAC(JJ) * (1.-T%XGREENROOF(JJ)) * PDN_RF(JJ) * DMT%XABS_LW_SNOW_ROOF(JJ) &
               +PRF_FRAC(JJ) *     T%XGREENROOF(JJ)               * DMT%XABS_LW_GREENROOF(JJ) &
               +PRD_FRAC(JJ)                         * PDF_RD(JJ) * DMT%XABS_LW_ROAD     (JJ) &
               +PRD_FRAC(JJ)                         * PDN_RD(JJ) * DMT%XABS_LW_SNOW_ROAD(JJ) &
               +PGD_FRAC(JJ)                                      * DMT%XABS_LW_GARDEN   (JJ) &
               +PHVEG_FRAC(JJ)                                    * DMT%XABS_LW_HVEG     (JJ) & 
               +PWL_FRAC(JJ)*0.5*(1.-B%XGR(JJ))                   * DMT%XABS_LW_WALL_A   (JJ) &
               +PWL_FRAC(JJ)*0.5*(1.-B%XGR(JJ))                   * DMT%XABS_LW_WALL_B   (JJ) &
               +PWL_FRAC(JJ)*        B%XGR(JJ)                    * DMT%XABS_LW_WIN      (JJ) &
              )
!
!
  IF (TOP%LSOLAR_PANEL) THEN
    ZLW_UP(JJ) = ZLW_UP(JJ) - PRF_FRAC(JJ) * DMT%XABS_LW_PANEL(JJ) * TPN%XFRAC_PANEL(JJ)
  ENDIF
!
!* Upward IR radiation from the canyon     
  DMT%XLW_UP_CAN(JJ) = PLW_RAD      (JJ)                                         &
                     - ( ZRD(JJ)                  *PDF_RD (JJ)*DMT%XABS_LW_ROAD      (JJ) &
                     +ZRD(JJ)                  *PDN_RD (JJ)*DMT%XABS_LW_SNOW_ROAD (JJ) &
                     +ZGD(JJ)                              *DMT%XABS_LW_GARDEN    (JJ) &
                     +T%XURBTREE(JJ)                       *DMT%XABS_LW_HVEG     (JJ)  &   
                     +PWL_O_GRND(JJ)*0.5*(1.-B%XGR(JJ))    *DMT%XABS_LW_WALL_A    (JJ) &   
                     +PWL_O_GRND(JJ)*0.5*(1.-B%XGR(JJ))    *DMT%XABS_LW_WALL_B    (JJ) &
                     +PWL_O_GRND(JJ)*        B%XGR(JJ)     *DMT%XABS_LW_WIN       (JJ) )
!
!* Upward IR radiation from the roof
  DMT%XLW_UP_ROOF(JJ) = PLW_RAD      (JJ)                                      &
                      - ( (1.-T%XGREENROOF(JJ))*PDF_RF (JJ)*DMT%XABS_LW_ROOF      (JJ) &
                      +(1.-T%XGREENROOF(JJ))*PDN_RF (JJ)*DMT%XABS_LW_SNOW_ROOF (JJ) &
                      +    T%XGREENROOF(JJ)             *DMT%XABS_LW_GREENROOF (JJ) ) 
!
  IF (TOP%LSOLAR_PANEL) THEN
    DMT%XLW_UP_ROOF(JJ) =DMT%XLW_UP_ROOF(JJ) -  TPN%XFRAC_PANEL(JJ) *DMT%XABS_LW_PANEL     (JJ) 
  ENDIF
!
!*      3.2    Town emissivity
!              ---------------
!
! simplifications are made to evaluate the emissivity in case of high vegetation: 
!  walls are not obstructed, high vegetation only obstruct road and garden.
!
    PEMIS_TWN(JJ) = T%XBLD        (JJ)  * (1.-T%XGREENROOF(JJ)) * PDF_RF(JJ) * T%XEMIS_ROOF(JJ)      * (1.-TPN%XFRAC_PANEL(JJ)) &
                  + T%XBLD        (JJ)  * (1.-T%XGREENROOF(JJ)) * PDN_RF(JJ) * PESN_RF     (JJ)      * (1.-TPN%XFRAC_PANEL(JJ)) &
                  + T%XBLD        (JJ)  *     T%XGREENROOF(JJ)               * PEMIS_GR    (JJ)      * (1.-TPN%XFRAC_PANEL(JJ)) &
                  + T%XBLD        (JJ)                                       * TPN%XEMIS_PANEL(JJ)   *     TPN%XFRAC_PANEL(JJ)  &
                  + T%XWALL_O_HOR (JJ)  *  T%XSVF_WS(JJ)                     * T%XEMIS_WALL(JJ)                                 &
                  + (1.-T%XBLD    (JJ)) *  T%XSVF_RS(JJ)                     * XEMISVEG              *     T%XURBTREE(JJ)       &
                  + T%XROAD       (JJ)  *  T%XSVF_RS(JJ)        *( PDF_RD(JJ)* T%XEMIS_ROAD(JJ)                                 &
                                                                  +PDN_RD(JJ)* T%TSNOW_ROAD%EMIS(JJ))* (1.-T%XURBTREE(JJ))      &
                  + T%XGARDEN     (JJ)  *  T%XSVF_RS(JJ)                     * PEMIS_GD(JJ)          * (1.-T%XURBTREE(JJ))
!
!*      3.3    Town radiative surface temperature
!              ----------------------------------
!
  PTS_TWN(JJ)   = ((ZLW_UP(JJ) - PLW_RAD(JJ)*(1.-PEMIS_TWN(JJ))) /PEMIS_TWN(JJ)/XSTEFAN)**0.25
!
!-------------------------------------------------------------------------------
!
!*      4.     Averaged evaporative flux (kg/m2/s)
!              -----------------------------------
!
  PEVAP_TWN(JJ) = PRF_FRAC  (JJ) * PDF_RF(JJ) * (1.-T%XGREENROOF(JJ)) * PLEW_RF   (JJ) / XLVTT  &
                 + PRF_FRAC  (JJ) * PDN_RF(JJ) * (1.-T%XGREENROOF(JJ)) * PLESN_RF  (JJ) / XLSTT  &
                 + PRF_FRAC  (JJ)              *     T%XGREENROOF(JJ)  * PEVAP_GR  (JJ)          &
                 + PRD_FRAC  (JJ) * PDF_RD(JJ)                         * PLEW_RD   (JJ) / XLVTT  &
                 + PRD_FRAC  (JJ) * PDN_RD(JJ)                         * PLESN_RD  (JJ) / XLSTT  &
                 + PGD_FRAC  (JJ)                                      * PEVAP_GD  (JJ)          &
                 + PHVEG_FRAC(JJ)                                      * DMT%XLE_HVEG(JJ)/XLVTT  &
                 + PWL_FRAC  (JJ) * 0.5  * (DMT%XLE_WALL_A(JJ) + DMT%XLE_WALL_B(JJ)) / XLVTT     &
                 +                                                     PLE_TRAFFIC (JJ) / XLVTT  &
                 +                                                   T%XLE_INDUSTRY(JJ) / XLVTT  &
                 + PRF_FRAC  (JJ) * DMT%XLE_WASTE_ROOF(JJ) / XLVTT
  !
  PEVAP_TWN_SURF(JJ) =  PRD_FRAC  (JJ)*PDF_RD(JJ)                  *PLEW_RD  (JJ) /XLVTT  &
                      + PRD_FRAC  (JJ)*PDN_RD(JJ)                  *PLESN_RD (JJ) /XLSTT  &
                      + PGD_FRAC  (JJ)                             *PEVAP_GD (JJ)        & 
                      + PHVEG_FRAC(JJ)                             *DMT%XLE_HVEG(JJ)/XLVTT  &
                      + PLE_TRAFFIC (JJ) /XLVTT                                           &
                      + T%XLE_INDUSTRY(JJ) /XLVTT
  !
  PEVAP_TWN_WALL(JJ) = PWL_FRAC  (JJ)*0.5 * (DMT%XLE_WALL_A(JJ)+DMT%XLE_WALL_B (JJ))/XLVTT
  !
  PEVAP_TWN_ROOF(JJ) = PRF_FRAC  (JJ)*PDF_RF(JJ)*(1.-T%XGREENROOF(JJ))*PLEW_RF(JJ)  / XLVTT &
                 + PRF_FRAC  (JJ)*PDN_RF(JJ)*(1.-T%XGREENROOF(JJ))*PLESN_RF(JJ) / XLSTT &
                 + PRF_FRAC  (JJ)           *    T%XGREENROOF(JJ) *PEVAP_GR(JJ)         &
                 + PRF_FRAC  (JJ)             *  DMT%XLE_WASTE_ROOF(JJ) / XLVTT  
  !
  IF (ABS(PEVAP_TWN(JJ)-PEVAP_TWN_SURF(JJ)-PEVAP_TWN_WALL(JJ)-PEVAP_TWN_ROOF(JJ)).GT.1.0E-8) THEN
     CALL ABOR1_SFX("AVG_URBAN_FLUXES: Wrong dispatching of evaporation flux")
  ENDIF
  !
!-------------------------------------------------------------------------------
!
!*      5.     Averaged runoff flux (kg/m2/s)
!              -----------------------------------
!
    DMT%XRUNOFF_TOWN(JJ) =  ((1.-T%XGREENROOF(JJ))* DMT%XRUNOFF_STRLROOF (JJ)                        &
                              +   T%XGREENROOF(JJ) *(PRUNOFF_GR(JJ) + PDRAIN_GR(JJ))) * T%XBLD(JJ)   &
                              +    T%XROAD    (JJ) * DMT%XRUNOFF_ROAD(JJ)                            &
                              +    T%XGARDEN  (JJ) * PRUNOFF_GD(JJ)                                  &
                              +    T%XROAD    (JJ) * DMT%XRUNOFFSOIL_ROAD(JJ)                        & 
                              +    T%XBLD     (JJ) * DMT%XRUNOFFSOIL_BLD(JJ)        
  IF (TOP%LURBHYDRO) &
    DMT%XRUNOFF_TOWN(JJ) =   DMT%XRUNOFF_TOWN(JJ)                                                    &
                              +    T%XROAD    (JJ) *(DMT%XRUNOFF_WW(JJ) + DMT%XRUNOFF_SW(JJ))
!
!-------------------------------------------------------------------------------
!
!*      6.    Air canyon temperature at time t+dt
!             -----------------------------------
!
  IF (.NOT. TOP%LCANOPY) THEN
    ZINTER = ZRD(JJ) * PAC_RD(JJ) * PDF_RD(JJ) +   ZGD(JJ) * PAC_GD(JJ) + PAC_WL(JJ) * PWL_O_GRND(JJ) &
           + PAC_TOP(JJ)                        +  PAC_HVEG(JJ) * PHVEG_FRAC(JJ) 
    IF (TOP%CBEM=="BEM") THEN
      ZINTER = ZINTER +  PCOE_H_WASTE_CANY(JJ)/ ( (1.-T%XBLD (JJ)) * PRHOA(JJ) * XCPD )
    ENDIF
    PT_CAN(JJ) =  (  T%XT_ROAD  (JJ,1) * PAC_RD (JJ) * PDF_RD (JJ) * ZRD(JJ)        &
                   + T%XT_WALL_A(JJ,1) * PAC_WL (JJ) * (1.-B%XGR(JJ)) * PWL_O_GRND(JJ) * 0.5 &
                   + T%XT_WALL_B(JJ,1) * PAC_WL (JJ) * (1.-B%XGR(JJ)) * PWL_O_GRND(JJ) * 0.5 &
                   + B%XT_WIN1    (JJ) * PAC_WL (JJ) *     B%XGR(JJ)  * PWL_O_GRND(JJ)       &
                   + PTA          (JJ) * PAC_TOP(JJ)                                         &
                   + PH_TRAFFIC   (JJ) / (1.-T%XBLD (JJ))               / PRHOA(JJ) / XCPD   &
                   + PHSN_RD(JJ) * PDN_RD(JJ)                           / PRHOA(JJ) / XCPD  ) &
                       / ZINTER  
!   
    PT_CAN(JJ) = PT_CAN(JJ) + (ZGD(JJ) * PTSRAD_GD(JJ) * PAC_GD  (JJ)) / ZINTER                 
    PT_CAN(JJ) = PT_CAN(JJ) + ( PT_HVEG(JJ)  * PAC_HVEG(JJ) * PHVEG_FRAC(JJ) ) / ZINTER   
    !
    IF (TOP%CBEM=="BEM") THEN
      PT_CAN(JJ) = PT_CAN(JJ) +  (   PCST_H_WASTE_CANY(JJ) / ( (1.-T%XBLD (JJ)) * PRHOA(JJ) * XCPD )             &
                 + PMUL_H_WASTE_CANY(JJ) / ( (1.-T%XBLD (JJ)) * PRHOA(JJ) * XCPD ) )  / ZINTER 
    ENDIF
!
!-------------------------------------------------------------------------------
!
!*      7.     Air canyon specific humidity
!              ----------------------------
!
    ZINTER = ZRD(JJ) * PAC_RD_WAT  (JJ) * PDF_RD      (JJ) * PDELT_RD(JJ)        &
           + ZGD(JJ) * PAC_AGG_GD  (JJ) * PHU_AGG_GD  (JJ)                       &
           + PAC_HVEG    (JJ) * PHU_HVEG    (JJ) *                PHVEG_FRAC(JJ) &
           + PAC_TOP     (JJ)
    IF (TOP%CBEM=="BEM") THEN
      ZINTER = ZINTER +  PCOE_LE_WASTE_CANY(JJ)/ ( (1.-T%XBLD (JJ)) * PRHOA(JJ) * XLVTT )
    ENDIF
    PQ_CAN(JJ) = (  ZRD(JJ) * PQSAT_RD   (JJ) * PAC_RD_WAT  (JJ) * PDF_RD    (JJ) * PDELT_RD(JJ)  &
                  + ZGD(JJ) * PQSAT_GD   (JJ) * PAC_AGG_GD  (JJ) * PHU_AGG_GD(JJ)                 &
                  + PQSAT_HVEG (JJ) * PAC_HVEG    (JJ) * PHU_HVEG  (JJ) * PHVEG_FRAC(JJ)          &
                  + PQA        (JJ) * PAC_TOP(JJ)                                                 &
                  + PLE_TRAFFIC(JJ) / (1.-T%XBLD(JJ)) / PRHOA(JJ) / XLVTT                         &
                  + PLESN_RD   (JJ) * PDN_RD(JJ)      / PRHOA(JJ) / XLVTT * ZRD(JJ)  ) / ZINTER

    IF (TOP%CBEM=="BEM") THEN
      PQ_CAN(JJ) = PQ_CAN(JJ) +  (   PCST_LE_WASTE_CANY(JJ) / ( (1.-T%XBLD (JJ)) * PRHOA(JJ) * XLVTT )             &
                 + PMUL_LE_WASTE_CANY(JJ) / ( (1.-T%XBLD (JJ)) * PRHOA(JJ) * XLVTT ) )  / ZINTER 
    ENDIF
  ENDIF
!
! Check for negative humidities
!
   IF (PQ_CAN(JJ).LT.-XSURF_EPSILON) THEN
      !
      CALL GET_LUOUT(HPROGRAM,ILUOUT)
      !
      WRITE(ILUOUT,*) "                                          "
      WRITE(ILUOUT,*) "In avg urban fluxes                       "
      WRITE(ILUOUT,*) "Check terms leading to negative humidity  "
      WRITE(ILUOUT,*) "                                          "
      WRITE(ILUOUT,*) "JJ                     : ",JJ
      WRITE(ILUOUT,*) "PQSAT_RD(JJ)           : ",PQSAT_RD(JJ)
      WRITE(ILUOUT,*) "PAC_RD_WAT(JJ)         : ",PAC_RD_WAT(JJ)
      WRITE(ILUOUT,*) "PDF_RD(JJ)             : ",PDF_RD(JJ)
      WRITE(ILUOUT,*) "ZRD (JJ)               : ",ZRD(JJ)
      WRITE(ILUOUT,*) "PDELT_RD(JJ)           : ",PDELT_RD(JJ)
      WRITE(ILUOUT,*) "PQSAT_GD(JJ)           : ",PQSAT_GD(JJ)
      WRITE(ILUOUT,*) "PAC_AGG_GD(JJ)         : ",PAC_AGG_GD(JJ)
      WRITE(ILUOUT,*) "PHU_AGG_GD(JJ)         : ",PHU_AGG_GD(JJ)
      WRITE(ILUOUT,*) "PAC_HVEG(JJ)           : ",PAC_HVEG(JJ)
      WRITE(ILUOUT,*) "PHU_HVEG(JJ)           : ",PHU_HVEG(JJ)
      WRITE(ILUOUT,*) "ZGD(JJ)                : ",ZGD(JJ)
      WRITE(ILUOUT,*) "PHVEG_FRAC(JJ)         : ",PHVEG_FRAC(JJ)
      WRITE(ILUOUT,*) "PQA(JJ)                : ",PQA(JJ)
      WRITE(ILUOUT,*) "PAC_TOP(JJ)            : ",PAC_TOP(JJ)
      WRITE(ILUOUT,*) "PLE_TRAFFIC(JJ)        : ",PLE_TRAFFIC(JJ)
      WRITE(ILUOUT,*) "T%XBLD(JJ)             : ",T%XBLD(JJ)
      WRITE(ILUOUT,*) "PRHOA(JJ)              : ",PRHOA(JJ)
      WRITE(ILUOUT,*) "PLESN_RD(JJ)           : ",PLESN_RD(JJ)
      WRITE(ILUOUT,*) "PDN_RD(JJ)             : ",PDN_RD(JJ)
      WRITE(ILUOUT,*) "PCST_LE_WASTE_CANY(JJ) : ",PCST_LE_WASTE_CANY(JJ)
      WRITE(ILUOUT,*) "PMUL_LE_WASTE_CANY(JJ) : ",PMUL_LE_WASTE_CANY(JJ)
      WRITE(ILUOUT,*) "------------------------------------------"
      WRITE(ILUOUT,*) "PQ_CAN(JJ)             : ",PQ_CAN(JJ)
      WRITE(ILUOUT,*) "                                          "
      CALL FLUSH(ILUOUT)
      CALL ABOR1_SFX("In avg_urban_fluxes: negative humidity in canyon")
   ENDIF
!
ENDDO
!
IF (LHOOK) CALL DR_HOOK('AVG_URBAN_FLUXES',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE AVG_URBAN_FLUXES
