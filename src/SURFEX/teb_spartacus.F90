!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
!
SUBROUTINE TEB_SPARTACUS (TOP, SPAOP, T, B, DMT, GDP, SB, TPN, PDIR_SW, PSCA_SW, &
     PZENITH, PLW_RAD, PFRAC_PANEL, PALB_PANEL, PALB_GD, PALB_GR, PALB_HVEG,     &
     PEMIS_GD, PEMIS_GR, PEMIS_HVEG, PTSRAD_GD, PTSRAD_GR, PTHVEG, PLAI_HVEG,    &
     PDN_RF, PDF_RF, PDN_RD, PDF_RD, PTRANS_HVCR, PREC_SW_GD, PREC_SW_RF,        &
     PDIR_ALB_TWN, PSCA_ALB_TWN, PREF_SW_GRND, PREF_SW_FAC, PREF_SW_HV,          &
     PE_SHADING, PSHAD_BEHAV_ANYWAY, PSHAD_BEHAV_ADAPTI, PSCA_SW_GROUND_DOWN,    &
     PSCA_SW_GROUND_UP, PSCA_SW_GROUND_HOR, PLW_GROUND_DOWN, PLW_GROUND_HOR,     &
     HTEST )
  !
  !   ##########################################################################
  !
  !!****  *TEB_SPARTACUS*  
  !!
  !!    PURPOSE
  !!    -------
  !     CALL the SPARTACUS-Surface scheme from the TEB model
  !     
  !!**  METHOD
  !     ------
  !
  !
  !!    EXTERNAL
  !!    --------
  !!
  !!
  !!    IMPLICIT ARGUMENTS
  !!    ------------------
  !!
  !!      
  !!    REFERENCE
  !!    ---------
  !!
  !!      
  !!    AUTHOR
  !!    ------
  !!
  !!      R. Schoetter          * Meteo-France *
  !!
  !!    MODIFICATIONS
  !!    -------------
  !!    Original    10/2020
  !----------------------------------------------------------------------------------------------
  !
  !*       0.     DECLARATIONS
  !               ------------
  !
  USE MODD_BEM_n, ONLY : BEM_t
  USE MODD_BEM_CST, ONLY : XWIN_SW_MAX
  USE MODD_CANOPY_n, ONLY : CANOPY_t
  USE MODD_CSTS, ONLY : XSTEFAN
  USE MODD_DIAG_MISC_TEB_n, ONLY : DIAG_MISC_TEB_t
  USE MODD_ISBA_n, ONLY : ISBA_P_t
  USE MODD_SURF_PAR, ONLY : XUNDEF, XSURF_EPSILON
  USE MODD_TEB_n, ONLY : TEB_t
  USE MODD_TEB_OPTION_n, ONLY : TEB_OPTIONS_t
  USE MODD_SPARTACUS_OPTION_n, ONLY : SPARTACUS_OPTIONS_t  
  USE MODD_TEB_PANEL_n, ONLY : TEB_PANEL_t
  USE MODD_TEB_PAR, ONLY: XEMIS_WIN_CST
  !
  USE MODI_ABOR1_SFX
  USE MODI_INTERPOL_SBL
  USE MODI_WINDOW_SHADING
  !
  USE RADSURF_BOUNDARY_CONDS_OUT, ONLY : BOUNDARY_CONDS_OUT_TYPE, ALLOCATE_BOUNDARY_CONDS_OUT
  USE RADSURF_CANOPY_FLUX, ONLY : CANOPY_FLUX_TYPE
  USE RADSURF_CANOPY_PROPERTIES, ONLY : CANOPY_PROPERTIES_TYPE
  USE RADSURF_CONFIG, ONLY : CONFIG_TYPE
  USE RADSURF_INTERFACE, ONLY : RADSURF
  USE RADSURF_LW_SPECTRAL_PROPERTIES, ONLY : LW_SPECTRAL_PROPERTIES_TYPE
  USE RADSURF_SW_SPECTRAL_PROPERTIES, ONLY : SW_SPECTRAL_PROPERTIES_TYPE
  USE RADSURF_SIMPLE_SPECTRUM
  !
  USE YOMHOOK, ONLY : LHOOK, DR_HOOK
  USE PARKIND1, ONLY : JPRB
  !
  IMPLICIT NONE
  !
  !*      0.1    Declarations of arguments
  !
  TYPE(BEM_t), INTENT(IN) :: B
  TYPE(CANOPY_t), INTENT(IN) :: SB
  TYPE(DIAG_MISC_TEB_t), INTENT(IN) :: DMT
  TYPE(ISBA_P_t), INTENT(IN) :: GDP
  TYPE(TEB_t), INTENT(IN) :: T  
  TYPE(TEB_OPTIONS_t), INTENT(IN) :: TOP
  TYPE(SPARTACUS_OPTIONS_t), INTENT(INOUT) :: SPAOP
  TYPE(TEB_PANEL_t), INTENT(IN) :: TPN
  !
  CHARACTER(LEN=2),   INTENT(IN) :: HTEST       ! must be equal to 'OK'  
  REAL, DIMENSION(:), INTENT(IN) :: PDIR_SW     ! incoming direct solar radiation
  REAL, DIMENSION(:), INTENT(IN) :: PSCA_SW     ! scattered incoming solar rad.
  REAL, DIMENSION(:), INTENT(IN) :: PZENITH     ! Solar zenithal angle
  REAL, DIMENSION(:), INTENT(IN) :: PLW_RAD     ! Downwelling longwave radiation
  REAL, DIMENSION(:), INTENT(IN) :: PFRAC_PANEL ! Fraction of solar panel on roofs (-)
  REAL, DIMENSION(:), INTENT(IN) :: PALB_PANEL ! Albedo     of solar panels (-)
  REAL, DIMENSION(:), INTENT(IN) :: PALB_GD    ! GD areas albedo
  REAL, DIMENSION(:), INTENT(IN) :: PALB_GR    ! green roof albedo
  REAL, DIMENSION(:), INTENT(IN) :: PALB_HVEG  ! urban tree albedo
  REAL, DIMENSION(:), INTENT(IN) :: PEMIS_GD   ! Garden emissivity
  REAL, DIMENSION(:), INTENT(IN) :: PEMIS_GR   ! Green roof emissivity
  REAL, DIMENSION(:), INTENT(IN) :: PEMIS_HVEG ! Urban tree emissivity
  REAL, DIMENSION(:), INTENT(IN) :: PTSRAD_GD  ! Skin surface temperature of low vegetation
  REAL, DIMENSION(:), INTENT(IN) :: PTSRAD_GR  ! Skin surface temperature of green roof
  REAL, DIMENSION(:), INTENT(IN) :: PTHVEG     ! Skin surface temperature of urban trees
  REAL, DIMENSION(:), INTENT(IN) :: PLAI_HVEG  ! LAI of urban trees  
  REAL, DIMENSION(:), INTENT(IN) :: PDN_RF     ! snow-covered roof fraction
  REAL, DIMENSION(:), INTENT(IN) :: PDF_RF     ! snow-free    roof fraction
  REAL, DIMENSION(:), INTENT(IN) :: PDN_RD     ! snow-covered road fraction
  REAL, DIMENSION(:), INTENT(IN) :: PDF_RD     ! snow-free    road fraction
  !
  REAL, DIMENSION(:,:), INTENT(IN) :: PSHAD_BEHAV_ANYWAY  ! Fraction of shades closes anyway
  REAL, DIMENSION(:,:), INTENT(IN) :: PSHAD_BEHAV_ADAPTI  ! Fraction of shades available for adaptive closing
  !
  !new argument for parametrization of urban trees 
  REAL, DIMENSION(:),   INTENT(IN)     :: PTRANS_HVCR      ! transmissivity for all crown of high veg
  !
  !new arguments for shading
  REAL, DIMENSION(:), INTENT(OUT) :: PREC_SW_GD   ! solar radiation received by GD areas
  REAL, DIMENSION(:), INTENT(OUT) :: PREC_SW_RF   ! solar radiation received
  !                                               ! by RF areas (below solar panels if any)
  REAL, DIMENSION(:), INTENT(OUT) :: PDIR_ALB_TWN ! town direct albedo
  REAL, DIMENSION(:), INTENT(OUT) :: PSCA_ALB_TWN ! town diffuse albedo
  !
  REAL, DIMENSION(:), INTENT(OUT) :: PREF_SW_GRND ! total solar radiation reflected by ground
  REAL, DIMENSION(:), INTENT(OUT) :: PREF_SW_FAC  ! total solar radiation reflected by wall
  REAL, DIMENSION(:), INTENT(OUT) :: PREF_SW_HV   ! total solar radiation reflected by high vegetation
  !new arguments for shading
  REAL, DIMENSION(:), INTENT(OUT) :: PE_SHADING   ! Energy that is not reflected by the shading, nor transmitted through
  !                                                 the bld, nor absorbed by the window [W/m2(win)]
  REAL, DIMENSION(:), INTENT(OUT) :: PSCA_SW_GROUND_DOWN ! Diffusive downwelling solar radiation at ground level (W/m2)
  REAL, DIMENSION(:), INTENT(OUT) :: PSCA_SW_GROUND_UP   ! Diffusive upwelling solar radiation at ground level (W/m2)
  REAL, DIMENSION(:), INTENT(OUT) :: PSCA_SW_GROUND_HOR  ! Diffusive solar radiation in horizontal direction at ground level (W/m2)
  REAL, DIMENSION(:), INTENT(OUT) :: PLW_GROUND_DOWN     ! Downwelling longwave radiation at ground level (W/m2)
  REAL, DIMENSION(:), INTENT(OUT) :: PLW_GROUND_HOR      ! Longwave radiation in horizontal direction at ground level (W/m2)
  !
  !*      0.2    Declarations of local variables
  !
  TYPE(CONFIG_TYPE) :: CONFIG
  !
  TYPE(CANOPY_PROPERTIES_TYPE)      :: CANOPY_PROPS
  TYPE(SW_SPECTRAL_PROPERTIES_TYPE) :: SW_SPECTRAL_PROPS
  TYPE(LW_SPECTRAL_PROPERTIES_TYPE) :: LW_SPECTRAL_PROPS
  !
  TYPE(BOUNDARY_CONDS_OUT_TYPE) :: BC_OUT
  TYPE(CANOPY_FLUX_TYPE) :: SW_NORM_DIFF, SW_NORM_DIR, LW_NORM, LW_INTERNAL
  TYPE(CANOPY_FLUX_TYPE) :: LW_FLUX, SW_FlUX
  ! 
  INTEGER :: NCOL      ! The number of columns for SPARTACUS-Surface
  INTEGER :: NTOTLAY   ! The total number of layers for SPARTACUS-Surface
  INTEGER :: ISTARTCOL ! Start of columns to be processed by SPARTACUS-Surface
  INTEGER :: IENDCOL   ! End of columns to be processed by SPARTACUS-Surface
  INTEGER :: JI, JLAYER, JCOMP, JLW, JSW
  !
  REAL, DIMENSION(SIZE(SB%XZ,1)) :: ZALB_AGG_GROUND  ! Aggregated ground albedo [1]
  REAL, DIMENSION(SIZE(SB%XZ,1)) :: ZALB_AGG_ROOF    ! Aggregated roof albedo [1]
  REAL, DIMENSION(SIZE(SB%XZ,1)) :: ZALB_AGG_FACADE  ! Aggregated facade albedo [1]
  !
  REAL, DIMENSION(SIZE(SB%XZ,1)) :: ZEMIS_AGG_GROUND ! Aggregated ground emissivity [1]
  REAL, DIMENSION(SIZE(SB%XZ,1)) :: ZEMIS_AGG_ROOF   ! Aggregated roof emissivity [1]
  REAL, DIMENSION(SIZE(SB%XZ,1)) :: ZEMIS_AGG_FACADE ! Aggregated facade emissivity [1]
  !
  REAL, DIMENSION(SIZE(SB%XZ,1)) :: ZTSUR_AGG        ! Aggregated skin surface temperature [K]
  REAL, DIMENSION(SIZE(SB%XZ,1)) :: ZAGG_ABS_WIN     ! Aggregated absorption by windows [W/m2]
  REAL, DIMENSION(SIZE(SB%XZ,1)) :: ZREC_SW_WIN      ! Solar rad received by windows [W/m2]
  REAL, DIMENSION(SIZE(SB%XZ,1)) :: ZFRAC_SHAD       ! Fraction of shading elements closed [1]
  !
  REAL, DIMENSION(SIZE(SB%XZ,1),SIZE(B%XTI_BLD,2)) :: ZEFF_SHAD       ! Indicator for shading status per compartment [1]
  REAL, DIMENSION(SIZE(SB%XZ,1),SIZE(B%XTI_BLD,2)) :: ZTRAN_WIN       ! Transmissivity of windows per compartment
  REAL, DIMENSION(SIZE(SB%XZ,1),SIZE(B%XTI_BLD,2)) :: ZALB_WIN        ! Albedo of windows per compartment
  REAL, DIMENSION(SIZE(SB%XZ,1),SIZE(B%XTI_BLD,2)) :: ZABS_WINSH      ! Solar radiation absorbed by shaded window [W/m2]
  REAL, DIMENSION(SIZE(SB%XZ,1),SIZE(B%XTI_BLD,2)) :: ZCOMP_E_SHADING !
  !
  REAL, DIMENSION(SIZE(SB%XZ,1)) :: ZAGC_GARDEN
  REAL, DIMENSION(SIZE(SB%XZ,1)) :: ZAGC_HVEG  
  REAL, DIMENSION(SIZE(SB%XZ,1)) :: ZGARDEN
  !
  REAL :: ZLAD_MEAN
  REAL, DIMENSION(SIZE(SB%XZ,1)) :: ZINTERPOL
  !
  REAL, DIMENSION(:,:), ALLOCATABLE :: ZTOP_FLUX_DN_DIRECT_SW
  REAL, DIMENSION(:,:), ALLOCATABLE :: ZTOP_FLUX_DN_DIFFUSE_SW
  REAL, DIMENSION(:,:), ALLOCATABLE :: ZTOP_FLUX_DN_LW
  !
  REAL :: ZDZ_SP
  REAL :: ZH_COUNT
  REAL :: ZALB_HVEG
  REAL :: ZHEIGHT_ELEMENT1, ZHEIGHT_ELEMENT2
  !
  INTEGER, PARAMETER :: NMAX_LAYER = 50
  !
  INTEGER, DIMENSION(SIZE(SB%XZ,1)) :: ICOUNT_LAY
  INTEGER, DIMENSION(SIZE(SB%XZ,1)) :: ISTART_LAY
  !
  REAL, DIMENSION(SIZE(SB%XZ,1),NMAX_LAYER) :: ZDZ_LAY
  REAL, DIMENSION(SIZE(SB%XZ,1),NMAX_LAYER) :: ZHMEAN_LAYER
  REAL, DIMENSION(SIZE(SB%XZ,1)) :: ZH_TREE
  !
  REAL(KIND=JPRB) :: ZHOOK_HANDLE
  !
  !-------------------------------------------------------------------------------
  !
  IF (LHOOK) CALL DR_HOOK('TEB_SPARTACUS',0,ZHOOK_HANDLE)
  !
  IF (HTEST/='OK') THEN
     CALL ABOR1_SFX('TEB_SPARTACUS: FATAL ERROR DURING ARGUMENT TRANSFER')
  ENDIF
  !
  ! FIXME:
  ! The height of the SPARTACUS layers is hardcoded here at the moment
  !
  ZDZ_SP = 5.0
  !
  CONFIG%DO_SW = SPAOP%LDO_SW 
  CONFIG%DO_LW = SPAOP%LDO_LW
  CONFIG%USE_SW_DIRECT_ALBEDO = SPAOP%LUSE_SW_DIRECT_ALBEDO
  CONFIG%DO_URBAN = SPAOP%LDO_URBAN
  CONFIG%N_VEGETATION_REGION_URBAN = SPAOP%N_VEGETATION_REGION_URBAN
  CONFIG%N_VEGETATION_REGION_FOREST = SPAOP%N_VEGETATION_REGION_FOREST
  CONFIG%NSW = SPAOP%NSW
  CONFIG%NLW = SPAOP%NLW
  CONFIG%N_STREAM_SW_URBAN = SPAOP%N_STREAM_SW_URBAN
  CONFIG%N_STREAM_LW_URBAN = SPAOP%N_STREAM_LW_URBAN
  CONFIG%N_STREAM_SW_FOREST = SPAOP%N_STREAM_SW_FOREST
  CONFIG%N_STREAM_LW_FOREST = SPAOP%N_STREAM_LW_FOREST
  CONFIG%USE_SYMMETRIC_VEGETATION_SCALE_URBAN = SPAOP%LUSE_SYMMETRIC_VEGETATION_SCALE_URBAN
  CONFIG%USE_SYMMETRIC_VEGETATION_SCALE_FOREST = SPAOP%LUSE_SYMMETRIC_VEGETATION_SCALE_FOREST
  CONFIG%VEGETATION_ISOLATION_FACTOR_URBAN = SPAOP%XVEGETATION_ISOLATION_FACTOR_URBAN
  CONFIG%VEGETATION_ISOLATION_FACTOR_FOREST = SPAOP%XVEGETATION_ISOLATION_FACTOR_FOREST
  CONFIG%MIN_VEGETATION_FRACTION = SPAOP%XMIN_VEGETATION_FRACTION
  CONFIG%MIN_BUILDING_FRACTION = SPAOP%XMIN_BUILDING_FRACTION
  !
  CALL CONFIG%CONSOLIDATE()
  !
  ! Prepare the inputs of SPARTACUS
  !
  NCOL = SIZE(SB%XZ,1)
  !
  ICOUNT_LAY(:) = 0
  ZDZ_LAY(:,:)  = -9999.0
  ZHMEAN_LAYER(:,:) = XUNDEF
  !
  IF (TOP%CURBTREE/="NONE") THEN
     ZH_TREE(:) = GDP%XH_TREE(:)
  ELSE
     ZH_TREE(:) = 0.0
  ENDIF
  !
  ! The layer discretisation takes into account the building and tree height
  ! Depending on whether trees or buildings are lower, first layers
  ! up to the building/tree height are created and are continued
  ! up to the higher of the two elements.
  !
  DO JI = 1, NCOL
     !
     IF (JI.EQ.1) ISTART_LAY(JI) = 1
     IF (JI.GE.2) ISTART_LAY(JI) = ISTART_LAY(JI-1)+ICOUNT_LAY(JI-1)
     !
     ZH_COUNT=0.0
     !
     ZHEIGHT_ELEMENT1 = MIN(ZH_TREE(JI),T%XBLD_HEIGHT(JI))
     ZHEIGHT_ELEMENT2 = MAX(ZH_TREE(JI),T%XBLD_HEIGHT(JI))
     !
     IF ((ZHEIGHT_ELEMENT1.LT.XSURF_EPSILON).AND.(ZHEIGHT_ELEMENT2.LT.XSURF_EPSILON)) THEN
        WRITE(1012,*) "ZHEIGHT_ELEMENT1 ",ZHEIGHT_ELEMENT1
        WRITE(1012,*) "ZHEIGHT_ELEMENT2 ",ZHEIGHT_ELEMENT2
        CALL FLUSH(1012)
        CALL ABOR1_SFX ("TEB-SPARTACUS: Tree and building height cannot be both 0.0")
     ENDIF        
     !
     ! Create layers up to the first element height
     !
     IF (ZHEIGHT_ELEMENT1.GT.XSURF_EPSILON) THEN
        !
        DO JLAYER = 1, NMAX_LAYER
           !
           IF ( ZHEIGHT_ELEMENT1.LE.(ZH_COUNT + ZDZ_SP) ) THEN
              ICOUNT_LAY(JI) = ICOUNT_LAY(JI) + 1
              ZDZ_LAY(JI,ICOUNT_LAY(JI)) = ZHEIGHT_ELEMENT1-ZH_COUNT
              ZHMEAN_LAYER(JI,ICOUNT_LAY(JI)) = ZH_COUNT + 0.5 * ( ZHEIGHT_ELEMENT1-ZH_COUNT )
              ZH_COUNT = ZH_COUNT + ( ZHEIGHT_ELEMENT1-ZH_COUNT )
              EXIT
           ELSE
              ICOUNT_LAY(JI) = ICOUNT_LAY(JI) + 1
              ZDZ_LAY(JI,ICOUNT_LAY(JI)) = ZDZ_SP
              ZHMEAN_LAYER(JI,ICOUNT_LAY(JI)) = ZH_COUNT + 0.5 * ZDZ_SP
              ZH_COUNT = ZH_COUNT + ZDZ_SP
           ENDIF
           !
           IF (JLAYER.EQ.NMAX_LAYER) CALL ABOR1_SFX ("Element 1 too high to be represented")
           !
        ENDDO
        !
        IF (ZHEIGHT_ELEMENT1.NE.ZH_COUNT) THEN
           WRITE(1012,*) "                 "
           WRITE(1012,*) "ZHEIGHT_ELEMENT1 ",ZHEIGHT_ELEMENT1
           WRITE(1012,*) "ZH_COUNT         ",ZH_COUNT
           CALL FLUSH(1012)
           CALL ABOR1_SFX ("First element in city is not well captured by SPARTACUS layers")
        ENDIF
        !
     END IF 
     !
     ! Create layers up to the second element height
     !
     IF (ZHEIGHT_ELEMENT2.GT.(ZHEIGHT_ELEMENT1+XSURF_EPSILON)) THEN
        !
        DO JLAYER = 1, NMAX_LAYER
           !
           IF ( ZHEIGHT_ELEMENT2.LE.(ZH_COUNT + 1.0e-6) ) EXIT
           !
           IF ( ZHEIGHT_ELEMENT2.LE.(ZH_COUNT + ZDZ_SP) ) THEN
              ICOUNT_LAY(JI) = ICOUNT_LAY(JI) + 1
              ZDZ_LAY(JI,ICOUNT_LAY(JI)) = ZHEIGHT_ELEMENT2-ZH_COUNT
              ZHMEAN_LAYER(JI,ICOUNT_LAY(JI)) = ZH_COUNT + 0.5 * ( ZHEIGHT_ELEMENT2 - ZH_COUNT )
              ZH_COUNT = ZH_COUNT + (ZHEIGHT_ELEMENT2-ZH_COUNT)
              EXIT
           ELSE
              ICOUNT_LAY(JI) = ICOUNT_LAY(JI) + 1
              ZDZ_LAY(JI,ICOUNT_LAY(JI)) = ZDZ_SP
              ZHMEAN_LAYER(JI,ICOUNT_LAY(JI)) = ZH_COUNT + 0.5 * ZDZ_SP
              ZH_COUNT = ZH_COUNT + ZDZ_SP
           ENDIF
           !
           IF (JLAYER.EQ.NMAX_LAYER) CALL ABOR1_SFX ("Element 2 too high to be represented")
           !
        ENDDO
        !
        IF ( ABS(ZHEIGHT_ELEMENT2-ZH_COUNT).GT.1.0e-5 ) THEN
           WRITE(1012,*) "                  "
           WRITE(1012,*) "ZHEIGHT_ELEMENT2  ",ZHEIGHT_ELEMENT2
           WRITE(1012,*) "ZH_COUNT          ",ZH_COUNT
           CALL FLUSH(1012)
           CALL ABOR1_SFX ("Element 2 too high to be captured by SPARTACUS layers")
        ENDIF
        !
     ENDIF
     !
  ENDDO
  !
  NTOTLAY = SUM(ICOUNT_LAY)
  !
  ! Initialise the canopy properties
  !
  CALL CANOPY_PROPS%ALLOCATE(CONFIG, NCOL, NTOTLAY)
  !
  ! Type of tile
  !
  IF (TOP%CURBTREE=="NONE") THEN
     CANOPY_PROPS%I_REPRESENTATION(:) = 2
  ELSE
     WHERE (T%XFRAC_HVEG(:).GT.CONFIG%MIN_VEGETATION_FRACTION)
        CANOPY_PROPS%I_REPRESENTATION(:) = 3
     ELSEWHERE
        CANOPY_PROPS%I_REPRESENTATION(:) = 2
     ENDWHERE
  ENDIF
  !
  IF (TOP%CURBTREE=="NONE") THEN
     SPAOP%LDO_VEGETATION = .FALSE.
  ELSE
     SPAOP%LDO_VEGETATION = .TRUE.
  ENDIF
  !
  CONFIG%DO_VEGETATION = SPAOP%LDO_VEGETATION
  !
  IF (MAXVAL(CANOPY_PROPS%I_REPRESENTATION(:)).EQ.2) THEN
     CONFIG%DO_VEGETATION = .FALSE.
  ENDIF
  !
  CANOPY_PROPS%ISTARTLAY(:) = ISTART_LAY(:)
  CANOPY_PROPS%NLAY(:)      = ICOUNT_LAY(:)
  !
  ! Layer thickness [m] (ntotlay)
  !
  DO JI = 1, NCOL
     CANOPY_PROPS%DZ(CANOPY_PROPS%ISTARTLAY(JI): &
          (CANOPY_PROPS%ISTARTLAY(JI)+CANOPY_PROPS%NLAY(JI)-1)) = ZDZ_LAY(JI,1:ICOUNT_LAY(JI))
  ENDDO
  !
  ! Cosine of solar zenith angle (ncol)
  !
  CANOPY_PROPS%COS_SZA(:) = COS(PZENITH)
  !
  ! Skin temperature of the ground, aggregated for low vegetation, snow free and snow covered roads [K] (ncol)
  !
  WHERE ( (T%XROAD(:) + T%XGARDEN(:)).GT.XSURF_EPSILON )
     ZEMIS_AGG_GROUND(:) = ( T%XROAD(:) * (PDF_RD(:) * T%XEMIS_ROAD(:) + PDN_RD(:) * T%TSNOW_ROAD%EMIS(:)) + &
       T%XGARDEN(:) * PEMIS_GD (:) ) / ( T%XROAD(:) + T%XGARDEN(:) )
     ZTSUR_AGG(:) = ( ( T%XROAD(:) * ( PDF_RD(:) * T%XEMIS_ROAD(:) * XSTEFAN * T%XT_ROAD(:,1)**4 + &
         PDN_RD(:) * T%TSNOW_ROAD%EMIS(:) * XSTEFAN * T%TSNOW_ROAD%TS(:)**4 ) +                   &
         T%XGARDEN(:) * PEMIS_GD(:) * XSTEFAN * PTSRAD_GD(:)**4 ) / ( T%XROAD(:) + T%XGARDEN(:) ) &
         / ZEMIS_AGG_GROUND(:) / XSTEFAN )**0.25
  ELSEWHERE
     ZEMIS_AGG_GROUND(:) = T%XEMIS_ROAD(:)
     ZTSUR_AGG(:) = T%XT_ROAD(:,1)**4
  END WHERE
  !
  CANOPY_PROPS%GROUND_TEMPERATURE(:) = ZTSUR_AGG(:)
  !
  ! Skin temperature of the roofs, aggregated for snow free and snow covered roofs, green roofs, and solar panels [K] (ntotlay)
  ! FIXME: Not discretised in the vertical direction at the moment
  !
  ZEMIS_AGG_ROOF(:) = (1.-T%XGREENROOF(:)) * (1.-TPN%XFRAC_PANEL(:)) * PDF_RF(:) * T%XEMIS_ROOF(:) &
       + (1.-T%XGREENROOF(:)) * (1.-TPN%XFRAC_PANEL(:)) * PDN_RF(:) * T%TSNOW_ROOF%EMIS(:)         &
       +     T%XGREENROOF(:)  * (1.-TPN%XFRAC_PANEL(:)) * PEMIS_GR (:)                             &
       +                            TPN%XFRAC_PANEL(:)  * TPN%XEMIS_PANEL(:)
  !
  ZTSUR_AGG(:) = ( ( (1.-T%XGREENROOF(:)) * (1.-TPN%XFRAC_PANEL(:)) *     &
       PDF_RF(:) * T%XEMIS_ROOF(:) * XSTEFAN * T%XT_ROOF(:,1)**4          &
       + (1.-T%XGREENROOF(:)) * (1.-TPN%XFRAC_PANEL(:)) *                 &
       PDN_RF(:) * T%TSNOW_ROOF%EMIS(:) * XSTEFAN * T%TSNOW_ROOF%TS(:)**4 &
       +     T%XGREENROOF(:)  * (1.-TPN%XFRAC_PANEL(:)) *                 &
       PEMIS_GR (:) * XSTEFAN * PTSRAD_GR(:)**4                           &
       +                            TPN%XFRAC_PANEL(:)  *                 &
       TPN%XEMIS_PANEL(:) * XSTEFAN * DMT%XTS_PANEL(:)**4  )              &
       / ZEMIS_AGG_ROOF(:) / XSTEFAN )**0.25
  !
  DO JI = 1, NCOL
     CANOPY_PROPS%ROOF_TEMPERATURE(CANOPY_PROPS%ISTARTLAY(JI): &
          (CANOPY_PROPS%ISTARTLAY(JI)+CANOPY_PROPS%NLAY(JI)-1)) = ZTSUR_AGG(JI)
  ENDDO
  !
  ! Skin temperature of the walls aggregated for WALL A, B and the windows [K] (ntotlay)
  ! FIXME: Not discretised in the vertical direction at the moment
  !
  ZEMIS_AGG_FACADE(:) = (1.-B%XGR(:)) * T%XEMIS_WALL(:) + B%XGR(:) * XEMIS_WIN_CST
  !
  ZTSUR_AGG(:) = ( ( (1.-B%XGR(:)) * 0.5 * T%XEMIS_WALL(:) * XSTEFAN * T%XT_WALL_A(:,1)**4 &
       + (1.-B%XGR(:)) * 0.5 * T%XEMIS_WALL(:) * XSTEFAN * T%XT_WALL_B(:,1)**4             &
       +     B%XGR(:)        * XEMIS_WIN_CST   * XSTEFAN * B%XT_WIN1(:)**4  )              &
       / ZEMIS_AGG_FACADE(:) / XSTEFAN )**0.25
  !
  DO JI = 1, NCOL
     CANOPY_PROPS%WALL_TEMPERATURE(CANOPY_PROPS%ISTARTLAY(JI):              &
          (CANOPY_PROPS%ISTARTLAY(JI)+CANOPY_PROPS%NLAY(JI)-1)) = ZTSUR_AGG(JI)
  ENDDO
  !
  ! Air temperature in canopy, separately specifying the temperature of the air
  ! in the clear and vegetated part of a layer, and the leaves [K] (ntotlay)
  ! FIXME: At the moment "veg" and "veg_air" not different from "air"
  !
  DO JI = 1, NCOL
     DO JLAYER = 1, CANOPY_PROPS%NLAY(JI)
        CALL INTERPOL_SBL(SB%XZ(:,:),SB%XT(:,:),ZHMEAN_LAYER(JI,JLAYER),ZINTERPOL(:))
        CANOPY_PROPS%CLEAR_AIR_TEMPERATURE(CANOPY_PROPS%ISTARTLAY(JI)+JLAYER-1) = ZINTERPOL(JI)
     ENDDO
  ENDDO
  !
  IF (TOP%CURBTREE/="NONE") THEN
     !
     DO JI = 1, NCOL
        CANOPY_PROPS%VEG_TEMPERATURE(CANOPY_PROPS%ISTARTLAY(JI):           &
          (CANOPY_PROPS%ISTARTLAY(JI)+CANOPY_PROPS%NLAY(JI)-1)) = PTHVEG(JI)
     ENDDO
     !
     WRITE(1012,*) "CANOPY_PROPS%VEG_TEMPERATURE ",CANOPY_PROPS%VEG_TEMPERATURE
     CALL FLUSH(1012)
     !
     CANOPY_PROPS%VEG_AIR_TEMPERATURE(:) = CANOPY_PROPS%CLEAR_AIR_TEMPERATURE(:)
     !
  ENDIF
  !
  ! Fractional coverage of buildings (ntotlay)
  ! FIXME: At the moment not discretised in the vertical direction
  !
  DO JI = 1, NCOL
     DO JLAYER = 1, CANOPY_PROPS%NLAY(JI)
        IF (ZHMEAN_LAYER(JI,JLAYER).LT.T%XBLD_HEIGHT(JI)) THEN
           CANOPY_PROPS%BUILDING_FRACTION(CANOPY_PROPS%ISTARTLAY(JI)+JLAYER-1) = T%XBLD(JI)
        ELSE
           CANOPY_PROPS%BUILDING_FRACTION(CANOPY_PROPS%ISTARTLAY(JI)+JLAYER-1) = 0.0
        ENDIF
     ENDDO
  ENDDO
  !
  ! Fractional coverage of urban trees
  ! FIXME: At the moment not discretised in the vertical direction
  !
  IF (TOP%CURBTREE/="NONE") THEN
     !
     DO JI = 1, NCOL
        !
        IF ( (CANOPY_PROPS%I_REPRESENTATION(JI).EQ.3).AND.(T%XFRAC_HVEG(JI).LT.CONFIG%MIN_VEGETATION_FRACTION)) THEN
           CALL ABOR1_SFX ("TEB_SPARTACUS: fraction of high vegetation is below, but vegetated urban shall be done")
        ENDIF
        !
        DO JLAYER = 1, CANOPY_PROPS%NLAY(JI)
           IF (ZHMEAN_LAYER(JI,JLAYER).LT.GDP%XH_TREE(JI)) THEN
              CANOPY_PROPS%VEG_FRACTION(CANOPY_PROPS%ISTARTLAY(JI)+JLAYER-1) = T%XFRAC_HVEG(JI)
           ELSE
              CANOPY_PROPS%VEG_FRACTION(CANOPY_PROPS%ISTARTLAY(JI)+JLAYER-1) = 0.0
           ENDIF
        ENDDO
        !
     ENDDO
     !
  ENDIF
  !
  ! Horizontal scale of buildings [m] (ntotlay)
  ! FIXME: At the moment not discretised in the vertical direction
  !
  DO JI = 1, NCOL  
     DO JLAYER = 1, CANOPY_PROPS%NLAY(JI)
        IF (ZHMEAN_LAYER(JI,JLAYER).LT.T%XBLD_HEIGHT(JI)) THEN
           CANOPY_PROPS%BUILDING_SCALE(CANOPY_PROPS%ISTARTLAY(JI)+JLAYER-1) = &
                4.0 * T%XBLD(JI) * T%XBLD_HEIGHT(JI) / T%XWALL_O_HOR(JI)
        ELSE
           CANOPY_PROPS%BUILDING_SCALE(CANOPY_PROPS%ISTARTLAY(JI)+JLAYER-1) = 0.0
        ENDIF
     ENDDO
  ENDDO
  !
  ! Horizontal scale of urban vegetation [m] (ntotlay)
  ! FIXME: At the moment not discretised in the vertical direction
  ! FIXME: Hardcoded here, could become a new TEB input parameter
  !
  IF (TOP%CURBTREE/="NONE") THEN
     !
     DO JI = 1, NCOL  
        DO JLAYER = 1, CANOPY_PROPS%NLAY(JI)
           IF (ZHMEAN_LAYER(JI,JLAYER).LT.GDP%XH_TREE(JI)) THEN
              CANOPY_PROPS%VEG_SCALE(CANOPY_PROPS%ISTARTLAY(JI)+JLAYER-1) = 5.0
           ELSE
              CANOPY_PROPS%VEG_SCALE(CANOPY_PROPS%ISTARTLAY(JI)+JLAYER-1) = 5.0
           ENDIF
        ENDDO
     ENDDO
     !
  ENDIF
  !
  ! Vegetation extinction coefficient, which is treated as wavelength independent [m-1] (ntotlay)
  ! Is taken as LAD/2 (LAD = Leaf area density on canopy levels)
  ! FIXME: Simplified LAD profile, not considering the trunk and crown shape.
  !
  IF (TOP%CURBTREE/='NONE') THEN
     !
     DO JI=1, NCOL
        !
        IF (T%XURBTREE(JI).GT.0.) THEN
           ZLAD_MEAN = MAX(1.0e-6,PLAI_HVEG(JI)) / GDP%XH_TREE(JI)
        ELSE
           ZLAD_MEAN = 1.0E-6
        ENDIF
        !
        DO JLAYER = 1, CANOPY_PROPS%NLAY(JI)
           !
           IF (ZHMEAN_LAYER(JI,JLAYER).LT.GDP%XH_TREE(JI)) THEN
              CANOPY_PROPS%VEG_EXT(CANOPY_PROPS%ISTARTLAY(JI)+JLAYER-1) = 0.5 * ZLAD_MEAN
           ELSE
              CANOPY_PROPS%VEG_EXT(CANOPY_PROPS%ISTARTLAY(JI)+JLAYER-1) = 0.5 * ZLAD_MEAN
           ENDIF
           !
        ENDDO
        !
     ENDDO
     !
  ENDIF
  !
  ! Fractional standard deviation of vegetation optical depth (ntotlay)
  ! FIXME: hardcoded to 0.0 here, no variation of vegetation optical depth
  !
  IF (TOP%CURBTREE/='NONE') THEN
     IF (SPAOP%N_VEGETATION_REGION_URBAN.GT.1) THEN
        DO JI = 1, NCOL
           CANOPY_PROPS%VEG_FSD(CANOPY_PROPS%ISTARTLAY(JI):      &
                (CANOPY_PROPS%ISTARTLAY(JI)+CANOPY_PROPS%NLAY(JI)-1)) = 0.0
        ENDDO
     ENDIF
  ENDIF
  !
  ! Fraction of building wall in contact with vegetation (ntotlay)
  ! Its default value is the fraction of the non-building area that is vegetation
  ! Calculated assuming a random positioning of the trees in the urban canyon.
  ! veg_contact_fraction = veg_fraction / (1-building_fraction).
  !
  IF (TOP%CURBTREE/='NONE') THEN
     DO JI = 1, NCOL 
        CANOPY_PROPS%VEG_CONTACT_FRACTION(CANOPY_PROPS%ISTARTLAY(JI):     &
             (CANOPY_PROPS%ISTARTLAY(JI)+CANOPY_PROPS%NLAY(JI)-1)) =      &
        MIN(1.0, CANOPY_PROPS%VEG_FRACTION(CANOPY_PROPS%ISTARTLAY(JI):    &
        (CANOPY_PROPS%ISTARTLAY(JI)+CANOPY_PROPS%NLAY(JI)-1)) /           &
        (1.0 - CANOPY_PROPS%BUILDING_FRACTION(CANOPY_PROPS%ISTARTLAY(JI): &
        (CANOPY_PROPS%ISTARTLAY(JI)+CANOPY_PROPS%NLAY(JI)-1))))
     ENDDO
  ENDIF
  !
  ! Initialise the shortwave spectral properties
  !
  CALL SW_SPECTRAL_PROPS%ALLOCATE(CONFIG, NCOL, NTOTLAY, CONFIG%NSW)
  !
  ! Extinction coefficient of air [m-1] (NSW,NTOTLAY)
  ! FIXME: Set to 0.0 here since no gas optics scheme anyway.
  !
  DO JSW = 1, CONFIG%NSW
     DO JI = 1, NCOL
        SW_SPECTRAL_PROPS%AIR_EXT(JSW,CANOPY_PROPS%ISTARTLAY(JI): &
             (CANOPY_PROPS%ISTARTLAY(JI)+CANOPY_PROPS%NLAY(JI)-1)) = 1.0E-6
     ENDDO
  ENDDO
  !
  ! Single scattering albedo of air (NSW,NTOTLAY)
  ! FIXME: Set to 1.0E-6 here since no gas optics scheme, but not
  !        set to 0.0 to avoid completely empty layers
  !
  DO JSW = 1, CONFIG%NSW
     DO JI = 1, NCOL
        SW_SPECTRAL_PROPS%AIR_SSA(JSW,CANOPY_PROPS%ISTARTLAY(JI):  &
             (CANOPY_PROPS%ISTARTLAY(JI)+CANOPY_PROPS%NLAY(JI)-1)) = 1.0E-6
     ENDDO
  ENDDO
  !
  ! Single scattering albedo of urban trees (NSW,NTOTLAY)
  ! FIXME: Set to the albedo of urban trees
  ! FIXME: Not considering the spectral dependency
  ! FIXME: Use hardcoded value for single scattering albedo of leaves
  !        that is more appropriate for SPARTACUS assumptions.
  !
  IF (TOP%CURBTREE/="NONE") THEN
     !
     ZALB_HVEG=0.4
     !
     DO JSW = 1, CONFIG%NSW
        DO JI = 1, NCOL
           IF (CANOPY_PROPS%I_REPRESENTATION(JI).EQ.3) THEN
              SW_SPECTRAL_PROPS%VEG_SSA(JSW,CANOPY_PROPS%ISTARTLAY(JI):  &
                   (CANOPY_PROPS%ISTARTLAY(JI)+CANOPY_PROPS%NLAY(JI)-1)) = ZALB_HVEG
           ENDIF
        ENDDO
     ENDDO
     !
  ENDIF
  !
  ! Ground albedo as composite of snow covered and snow free road, and low vegetation (NSW,NCOL)
  ! FIXME: Not considering the spectral dependency
  !
  WHERE ( (T%XROAD(:) + T%XGARDEN(:)).GT.XSURF_EPSILON )
     ZALB_AGG_GROUND(:) = ( T%XROAD(:) * (PDF_RD(:) * T%XALB_ROAD(:) + PDN_RD(:) * T%TSNOW_ROAD%ALB(:)) + &
         T%XGARDEN(:) * PALB_GD (:) ) / ( T%XROAD(:) + T%XGARDEN(:) )
  ELSEWHERE
     ZALB_AGG_GROUND(:) = T%XALB_ROAD(:)
  ENDWHERE
  !
  DO JSW = 1, CONFIG%NSW
     SW_SPECTRAL_PROPS%GROUND_ALBEDO(JSW,:) = ZALB_AGG_GROUND(:)
  ENDDO
  !
  ! Roof albedo as composite of snow covered and snow free roofs, green roofs, and solar panels (NSW,NTOTLAY)
  ! FIXME: Not considering the vertical dependency
  ! FIXME: Not considering the spectral dependency
  !
  ZALB_AGG_ROOF(:) = (1.-T%XGREENROOF(:)) * (1.-TPN%XFRAC_PANEL(:)) * PDF_RF(:) * T%XALB_ROOF(:)      &
       + (1.-T%XGREENROOF(:)) * (1.-TPN%XFRAC_PANEL(:)) * PDN_RF(:) * T%TSNOW_ROOF%ALB(:) &
       +     T%XGREENROOF(:)  * (1.-TPN%XFRAC_PANEL(:)) * PALB_GR (:)                     &
       +                            TPN%XFRAC_PANEL(:)  * TPN%XALB_PANEL(:)  
  !
  DO JSW = 1, CONFIG%NSW
     DO JI = 1, NCOL
        SW_SPECTRAL_PROPS%ROOF_ALBEDO(JSW,CANOPY_PROPS%ISTARTLAY(JI):  &
             (CANOPY_PROPS%ISTARTLAY(JI)+CANOPY_PROPS%NLAY(JI)-1)) =ZALB_AGG_ROOF(JI)
     ENDDO
  ENDDO
  !
  ! Wall albedo as composite between walls and windows (NSW,NTOTLAY)
  ! FIXME: Not considering vertical dependency
  ! FIXME: Not considering the spectral dependency
  !
  ! First the shading related calculations need to be done since they change the window albedo
  !
  IF (TOP%CBEM=='BEM') THEN
     !
     ZEFF_SHAD(:,:) = -9999.0
     DMT%XDIAGSHAD(:,:) = 0.0
     !
     DO JI=1,SIZE(B%XSHADEARCHI,1)
        DO JCOMP=1,SIZE(B%XFRACOMP,2) 
           !
           IF (B%XSHADEARCHI(JI).LT.0.5) THEN
              !
              ! If shading elements are not present, shading is not possible
              !
              ZEFF_SHAD(JI,JCOMP) = 0.0
              DMT%XDIAGSHAD(JI,JCOMP) = 0.0
              !
           ELSE IF (B%XSHADEARCHI(JI).GT.1.5) THEN
              !
              ! In this case shading elements are always present
              ! However, PDIAGSHAD=0 since this diagnostic
              ! represents only shading due to human behaviour.
              !
              ZEFF_SHAD(JI,JCOMP) = 1.0
              DMT%XDIAGSHAD(JI,JCOMP) = 0.0
              !
           ELSE
              !
              ! If shading elements are present, the shading behavioural
              ! indicators are used to determine the fraction of shades closed
              ! A logistic function is used to determine the fractions of shades actually closed.
              !
              ZFRAC_SHAD(JI) = 1.0/(1.0+EXP(-0.05*(DMT%XREC_SW_WALL(JI)-B%XWIN_SW_MAX(JI))))
              !
              ZEFF_SHAD(JI,JCOMP) = PSHAD_BEHAV_ANYWAY(JI,JCOMP) + PSHAD_BEHAV_ADAPTI(JI,JCOMP)*ZFRAC_SHAD(JI)
              !
              DMT%XDIAGSHAD(JI,JCOMP) = ZEFF_SHAD(JI,JCOMP)          
              !
           ENDIF
           !
        ENDDO
     ENDDO
     !
     IF ((MINVAL(DMT%XDIAGSHAD).LT.-XSURF_EPSILON).OR. (MAXVAL(DMT%XDIAGSHAD).GT. &
          (1.0+XSURF_EPSILON))) CALL ABOR1_SFX("Wrong shading fraction")
     !
     DO JCOMP=1,SIZE(B%XFRACOMP,2)
        ZTRAN_WIN (:,JCOMP) = B%XTRAN_WIN(:)
     ENDDO
     !
     CALL WINDOW_SHADING(B%XSHGC_SH, ZEFF_SHAD, T%XALB_WALL, B%XABS_WIN, ZABS_WINSH, ZALB_WIN, ZTRAN_WIN )
     !
     ! The values of the window albedo and window absorbance are aggregated
     ! over the bem compartments
     !
     ZAGG_ABS_WIN(:)=0.0
     B%XALB_WIN(:)=0.0
     DO JCOMP=1,SIZE(B%XFRACOMP,2)
        B%XALB_WIN(:)   = B%XALB_WIN(:)   + B%XFRACOMP(:,JCOMP) * ZALB_WIN(:,JCOMP)
        ZAGG_ABS_WIN(:) = ZAGG_ABS_WIN(:) + B%XFRACOMP(:,JCOMP) * ZABS_WINSH(:,JCOMP)
     ENDDO
     !
  ELSE
     !
     ZAGG_ABS_WIN(:) = 0.0
     ZABS_WINSH (:,:)= 0.0
     B%XALB_WIN (:)  = 0.0
     ZALB_WIN (:,:)  = 0.0
     ZTRAN_WIN(:,:)  = 0.0
     !
  ENDIF
  !
  ! After the shading calculations have been made, the aggregated albedo of the facade is calculated
  !
  ZALB_AGG_FACADE(:) = (1.-B%XGR(:)) * T%XALB_WALL(:) + B%XGR(:) * B%XALB_WIN(:)
  !
  DO JSW = 1, CONFIG%NSW
     DO JI = 1, NCOL
        SW_SPECTRAL_PROPS%WALL_ALBEDO(JSW,CANOPY_PROPS%ISTARTLAY(JI):  &
             (CANOPY_PROPS%ISTARTLAY(JI)+CANOPY_PROPS%NLAY(JI)-1)) = ZALB_AGG_FACADE(JI)
     ENDDO
  ENDDO
  !
  IF (CONFIG%USE_SW_DIRECT_ALBEDO) THEN
     !
     ! Ground albedo, direct beam (NSW,NCOL)
     ! FIXME: Set equal to ground albedo
     !
     SW_SPECTRAL_PROPS%GROUND_ALBEDO_DIR(:,:)=SW_SPECTRAL_PROPS%GROUND_ALBEDO(:,:)
     !
     ! Roof albedo, direct beam (CONFIG%NSW,NTOTLAY)
     ! FIXME: Set equal to roof albedo
     !
     SW_SPECTRAL_PROPS%ROOF_ALBEDO_DIR(:,:)=SW_SPECTRAL_PROPS%ROOF_ALBEDO(:,:)
     !
  ENDIF
  !
  ! Wall specular fraction (NSW,NTOTLAY)
  ! FIXME: Hardcoded to 0.0, set as a function of glazing ratio
  !
  SW_SPECTRAL_PROPS%WALL_SPECULAR_FRAC(:,:)=0.0
  !
  ! Initialise the longwave spectral properties
  ! FIXME: This call is not like in the documentation
  !
  CALL LW_SPECTRAL_PROPS%ALLOCATE(CONFIG, CONFIG%NLW, NCOL, NTOTLAY, CANOPY_PROPS%I_REPRESENTATION)
  !
  ! Air extinction coefficient [m-1] (NLW,NTOTLAY)
  ! FIXME: Set to 0.0 since no gas optics scheme anyway
  !
  DO JLW = 1, CONFIG%NLW
     LW_SPECTRAL_PROPS%AIR_EXT(JLW,:) = 1.0E-6
  ENDDO
  !
  ! Air single scattering albedo (NLW,NTOTLAY)
  ! FIXME: Set to 1.0E-6 since no gas optics scheme, but not to 0.0 to avoid
  !        completely empty layers.
  !
  DO JLW = 1, CONFIG%NLW
     LW_SPECTRAL_PROPS%AIR_SSA(JLW,:) = 1.0E-6
  ENDDO
  !
  ! Urban vegetation single scattering albedo (NLW,NTOTLAY)
  ! FIXME Neglect spectral dependency
  !
  IF (TOP%CURBTREE/="NONE") THEN
     !
     DO JLW = 1, CONFIG%NLW
        DO JI = 1, NCOL
           IF (CANOPY_PROPS%I_REPRESENTATION(JI).EQ.3) THEN
              LW_SPECTRAL_PROPS%VEG_SSA(JLW,CANOPY_PROPS%ISTARTLAY(JI):  &
              (CANOPY_PROPS%ISTARTLAY(JI)+CANOPY_PROPS%NLAY(JI)-1)) = 1.0 - PEMIS_HVEG(JI)
           ENDIF
        ENDDO
     ENDDO
     !
  ENDIF
  !
  ! Ground emissivity as composite of snow free and snow covered road, and urban vegetation (NLW,NCOL)
  ! FIXME: Neglect spectral dependency
  !
  DO JLW = 1, CONFIG%NLW
     DO JI = 1, NCOL
        LW_SPECTRAL_PROPS%GROUND_EMISSIVITY(JLW,JI) = ZEMIS_AGG_GROUND(JI)
     ENDDO
  ENDDO
  !
  ! Roof emissivity as composite of snow free and snow covered roof, green roofs, and solar panels (NLW,NTOTLAY)
  ! FIXME: Neglect vertical dependency
  ! FIXME: Neglect spectral dependency
  !
  DO JLW = 1, CONFIG%NLW
     DO JI = 1, NCOL
        LW_SPECTRAL_PROPS%ROOF_EMISSIVITY(JLW,CANOPY_PROPS%ISTARTLAY(JI):  &
             (CANOPY_PROPS%ISTARTLAY(JI)+CANOPY_PROPS%NLAY(JI)-1)) = ZEMIS_AGG_ROOF(JI)
     ENDDO
  ENDDO
  !
  ! Wall emissivity as composite of walls and windows (NWM,NTOTLAY)
  ! FIXME: Neglect vertical dependency
  ! FIXME: Neglect spectral dependency
  !
  DO JLW = 1, SPAOP%NLW
     DO JI = 1, NCOL
        LW_SPECTRAL_PROPS%WALL_EMISSIVITY(JLW,CANOPY_PROPS%ISTARTLAY(JI): &
             (CANOPY_PROPS%ISTARTLAY(JI)+CANOPY_PROPS%NLAY(JI)-1)) = ZEMIS_AGG_FACADE(JI)
     ENDDO
  ENDDO
  !
  ! Initialise the longwave emission
  !
  ISTARTCOL = 1
  IENDCOL = NCOL
  !
  CALL LW_SPECTRAL_PROPS%CALC_MONOCHROMATIC_EMISSION(CANOPY_PROPS)
  !
  IF (IENDCOL.GE.1) THEN
     CALL CALC_SIMPLE_SPECTRUM_LW(CONFIG, CANOPY_PROPS, LW_SPECTRAL_PROPS, ISTARTCOL, IENDCOL)
  ENDIF
  !
  ! Allocate SPARTACUS-Surface output arrays
  !
  CALL ALLOCATE_BOUNDARY_CONDS_OUT(BC_OUT, NCOL, CONFIG%NSW, CONFIG%NLW)
  CALL SW_NORM_DIFF%ALLOCATE(CONFIG, NCOL, NTOTLAY, CONFIG%NSW, USE_DIRECT=.TRUE.)
  CALL SW_NORM_DIR%ALLOCATE(CONFIG, NCOL, NTOTLAY, CONFIG%NSW, USE_DIRECT=.TRUE.)
  CALL LW_INTERNAL%ALLOCATE(CONFIG, NCOL, NTOTLAY, CONFIG%NLW, USE_DIRECT=.FALSE.)
  CALL LW_NORM%ALLOCATE(CONFIG, NCOL, NTOTLAY, CONFIG%NLW, USE_DIRECT=.FALSE.)
  !
  ! Set all output fluxes to zero
  !
  CALL SW_NORM_DIR%ZERO_ALL()
  CALL SW_NORM_DIFF%ZERO_ALL()
  CALL LW_INTERNAL%ZERO_ALL()
  CALL LW_NORM%ZERO_ALL()
  !
  ! Call the SPARTACUS-Surface radiation scheme
  !
  IF (IENDCOL.GE.1) THEN
     !
     CALL RADSURF(CONFIG, CANOPY_PROPS, SW_SPECTRAL_PROPS, LW_SPECTRAL_PROPS, & ! Inputs
       BC_OUT,                                                                & ! Outputs
       ISTARTCOL, IENDCOL,                                                    & ! Optional inputs
       SW_NORM_DIR, SW_NORM_DIFF, LW_INTERNAL, LW_NORM)                         ! Optional outputs
     !
  ENDIF
  !
  ! Assign the values of town equivalent direct and diffuse albedo
  ! FIXME: Spectral dependency is neglected
  !
  PDIR_ALB_TWN(:) = BC_OUT%SW_ALBEDO_DIR(1,:)
  PSCA_ALB_TWN(:) = BC_OUT%SW_ALBEDO(1,:)
  !
  ! FIXME: Unplausible values when sun below the horizon: set to 0.0
  !
  WHERE(PDIR_SW(:).LT.XSURF_EPSILON) PDIR_ALB_TWN(:) = 0.0
  WHERE(PSCA_SW(:).LT.XSURF_EPSILON) PSCA_ALB_TWN(:) = 0.0
  !
  ! Check whether plausible values are coupled with the atmospheric model
  !
  IF ( (MINVAL(PDIR_ALB_TWN).LT.0.0_JPRB).OR.(MAXVAL(PDIR_ALB_TWN).GT.1.0_JPRB) ) THEN
     WRITE(1012,*) "PDIR_ALB_TWN ",PDIR_ALB_TWN
     CALL FLUSH(1012)   
     CALL ABOR1_SFX("TEB_SPARTACUS: Unplausible value of TOWN albedo for direct solar radiation")
  ENDIF
  !
  IF ( (MINVAL(PSCA_ALB_TWN).LT.0.0_JPRB).OR.(MAXVAL(PSCA_ALB_TWN).GT.1.0_JPRB) ) THEN
     WRITE(1012,*) "PSCA_ALB_TWN ",PSCA_ALB_TWN
     CALL FLUSH(1012)   
     CALL ABOR1_SFX("TEB_SPARTACUS: Unplausible value of TOWN albedo for scattered solar radiation")
  ENDIF
  !
  ! Rescale the outputs
  !
  CALL SW_FLUX%ALLOCATE(CONFIG, NCOL, NTOTLAY, CONFIG%NSW, USE_DIRECT=.TRUE.)
  CALL LW_FLUX%ALLOCATE(CONFIG, NCOL, NTOTLAY, CONFIG%NLW, USE_DIRECT=.FALSE.)
  !
  ! FIXME: No spectral dependency is considered here, hardcoded to one band
  !
  ALLOCATE(ZTOP_FLUX_DN_DIFFUSE_SW(CONFIG%NSW,NCOL))
  ALLOCATE(ZTOP_FLUX_DN_DIRECT_SW(CONFIG%NSW,NCOL))
  ALLOCATE(ZTOP_FLUX_DN_LW(CONFIG%NLW,NCOL))
  !
  DO JLW = 1, CONFIG%NLW
     ZTOP_FLUX_DN_LW(JLW,:) = PLW_RAD(:)
  ENDDO
  !
  DO JSW = 1, CONFIG%NSW
     ZTOP_FLUX_DN_DIFFUSE_SW(JSW,:) = PSCA_SW(:)
     ZTOP_FLUX_DN_DIRECT_SW(JSW,:)  = PDIR_SW(:)
  ENDDO
  !
  CALL SW_NORM_DIFF%SCALE(CANOPY_PROPS%NLAY, ZTOP_FLUX_DN_DIFFUSE_SW)
  CALL SW_NORM_DIR%SCALE(CANOPY_PROPS%NLAY, ZTOP_FLUX_DN_DIRECT_SW)
  CALL LW_NORM%SCALE(CANOPY_PROPS%NLAY, ZTOP_FLUX_DN_LW)
  !
  CALL SW_FLUX%SUM(SW_NORM_DIR, SW_NORM_DIFF)
  CALL LW_FLUX%SUM(LW_INTERNAL, LW_NORM)
  !
  ! ########################################################
  ! All outputs for shortwave radiation from urban_solar_abs
  ! are populated based on the SPARTACUS results
  ! ########################################################
  !
  DMT%XABS_SW_ROAD(:)      = 0.0
  DMT%XABS_SW_SNOW_ROAD(:) = 0.0
  DMT%XABS_SW_GARDEN(:)    = 0.0
  DMT%XABS_SW_ROOF(:)      = 0.0
  DMT%XABS_SW_GREENROOF(:) = 0.0
  DMT%XABS_SW_PANEL(:)     = 0.0
  DMT%XABS_SW_SNOW_ROOF(:) = 0.0
  DMT%XABS_SW_WALL_A(:)    = 0.0
  DMT%XABS_SW_WALL_B(:)    = 0.0
  DMT%XABS_SW_HVEG(:)      = 0.0
  !
  DMT%XDIR_SW_ROAD(:)      = 0.0
  DMT%XDIR_SW_GARDEN(:)    = 0.0
  !
  PREC_SW_RF(:)       = 0.0
  ZREC_SW_WIN(:)      = 0.0
  DMT%XREC_SW_WALL(:) = 0.0
  PREC_SW_GD(:)       = 0.0
  DMT%XREC_SW_HVEG(:) = 0.0
  !
  PREF_SW_GRND(:) = 0.0
  PREF_SW_FAC(:)  = 0.0
  PREF_SW_HV(:)   = 0.0
  !
  PSCA_SW_GROUND_DOWN(:) = 0.0
  PSCA_SW_GROUND_UP(:)   = 0.0
  PSCA_SW_GROUND_HOR(:)   = 0.0
  !
  DO JSW = 1, CONFIG%NSW
     DO JI = 1, NCOL
        !
        ! Radiation absorbed by various facets
        !
        DMT%XABS_SW_ROAD(JI) = DMT%XABS_SW_ROAD(JI) +                           &
             (1.0 - T%XALB_ROAD(JI)) * SW_FLUX%GROUND_DN(JSW,JI)/(1.0 - T%XBLD(JI))
        !
        IF (PDN_RD(JI).GT.0.0) THEN
           DMT%XABS_SW_SNOW_ROAD(JI) = DMT%XABS_SW_SNOW_ROAD(JI) +                      &
                (1.0 - T%TSNOW_ROAD%ALB(JI)) * SW_FLUX%GROUND_DN(JSW,JI)/(1.0 - T%XBLD(JI))
        ELSE
           DMT%XABS_SW_SNOW_ROAD(JI) = XUNDEF
        ENDIF
        !
        DMT%XABS_SW_GARDEN(JI) = DMT%XABS_SW_GARDEN(JI) +                   &
             (1.0 - PALB_GD(JI)) * SW_FLUX%GROUND_DN(JSW,JI)/(1.0 - T%XBLD(JI))      
        !
        DMT%XABS_SW_ROOF(JI) = DMT%XABS_SW_ROOF(JI) +                                    &
             (1.0 - T%XALB_ROOF(JI)) * SUM(SW_FLUX%ROOF_IN(JSW,CANOPY_PROPS%ISTARTLAY(JI): &
             (CANOPY_PROPS%ISTARTLAY(JI)+CANOPY_PROPS%NLAY(JI)-1))) / T%XBLD(JI)
        !
        IF (T%XGREENROOF(JI).GT.0.0) THEN
           DMT%XABS_SW_GREENROOF(JI) = DMT%XABS_SW_GREENROOF(JI) +                       &
                (1.0 - PALB_GR(JI)) * SUM(SW_FLUX%ROOF_IN(JSW,CANOPY_PROPS%ISTARTLAY(JI): &
                (CANOPY_PROPS%ISTARTLAY(JI)+CANOPY_PROPS%NLAY(JI)-1))) / T%XBLD(JI)
        ELSE
           DMT%XABS_SW_GREENROOF(JI) = XUNDEF          
        ENDIF
        !
        DMT%XABS_SW_PANEL(JI) = DMT%XABS_SW_PANEL(JI) +                                       &
             (1.0 - TPN%XALB_PANEL(JI)) * SUM(SW_FLUX%ROOF_IN(JSW,CANOPY_PROPS%ISTARTLAY(JI): &
             (CANOPY_PROPS%ISTARTLAY(JI)+CANOPY_PROPS%NLAY(JI)-1))) / T%XBLD(JI)
        !
        IF (PDN_RF(JI).GT.0.0) THEN
           DMT%XABS_SW_SNOW_ROOF(JI) = DMT%XABS_SW_SNOW_ROOF(JI) +                                 &
                (1.0 - T%TSNOW_ROOF%ALB(JI)) * SUM(SW_FLUX%ROOF_IN(JSW,CANOPY_PROPS%ISTARTLAY(JI): &
                (CANOPY_PROPS%ISTARTLAY(JI)+CANOPY_PROPS%NLAY(JI)-1))) / T%XBLD(JI)
        ELSE
           DMT%XABS_SW_SNOW_ROOF(JI) = XUNDEF           
        ENDIF
        !
        ! FIXME: add option for two walls
        !
        DMT%XABS_SW_WALL_A(JI) = DMT%XABS_SW_WALL_A(JI) +                                  &
             (1.0 - T%XALB_WALL(JI)) * SUM(SW_FLUX%WALL_IN(JSW,CANOPY_PROPS%ISTARTLAY(JI): &
             (CANOPY_PROPS%ISTARTLAY(JI)+CANOPY_PROPS%NLAY(JI)-1))) / T%XWALL_O_HOR(JI)
        !
        DMT%XABS_SW_WALL_B(JI) = DMT%XABS_SW_WALL_A(JI)
        !
        ! FIXME: Best guess values are used for those grid points
        !        for which FRAC_HVEG is below the threshold.
        !        This leads to small energetic inconsistancies.
        !
        IF (TOP%CURBTREE/="NONE") THEN
           !
           IF (T%XFRAC_HVEG(JI).GT.CONFIG%MIN_VEGETATION_FRACTION) THEN
              !
              DMT%XABS_SW_HVEG(JI) = DMT%XABS_SW_HVEG(JI) +                              &
                   SUM(SW_FLUX%VEG_ABS(JSW,CANOPY_PROPS%ISTARTLAY(JI):                      &
                   (CANOPY_PROPS%ISTARTLAY(JI)+CANOPY_PROPS%NLAY(JI)-1))) / T%XFRAC_HVEG(JI)
              !
              ! FIXME: division by zero is possible here
              !
              DMT%XREC_SW_HVEG(JI) = DMT%XREC_SW_HVEG(JI) +                                 &
                   SUM(SW_FLUX%VEG_ABS(JSW,CANOPY_PROPS%ISTARTLAY(JI):                        &
                   (CANOPY_PROPS%ISTARTLAY(JI)+CANOPY_PROPS%NLAY(JI)-1)) / T%XFRAC_HVEG(JI) / &
                   (1.0 - SW_SPECTRAL_PROPS%VEG_SSA(JSW,CANOPY_PROPS%ISTARTLAY(JI):           &
                   (CANOPY_PROPS%ISTARTLAY(JI)+CANOPY_PROPS%NLAY(JI)-1)) ) ) 
              !
              ! FIXME: division by zero is possible here
              !
              PREF_SW_HV(JI) = PREF_SW_HV(JI) +                                            &
                   SUM( SW_SPECTRAL_PROPS%VEG_SSA(JSW,CANOPY_PROPS%ISTARTLAY(JI):             &
                   (CANOPY_PROPS%ISTARTLAY(JI)+CANOPY_PROPS%NLAY(JI)-1)) *                    &
                   SW_FLUX%VEG_ABS(JSW,CANOPY_PROPS%ISTARTLAY(JI):                            &
                   (CANOPY_PROPS%ISTARTLAY(JI)+CANOPY_PROPS%NLAY(JI)-1)) / T%XFRAC_HVEG(JI) / &
                   (1.0 - SW_SPECTRAL_PROPS%VEG_SSA(JSW,CANOPY_PROPS%ISTARTLAY(JI):           &
                   (CANOPY_PROPS%ISTARTLAY(JI)+CANOPY_PROPS%NLAY(JI)-1)) ) )
              !
           ELSE
              DMT%XABS_SW_HVEG(JI) = 0.0
              DMT%XREC_SW_HVEG(JI) = 0.0
              PREF_SW_HV(JI)       = 0.0
           ENDIF
           !
        ENDIF
        !
        ! Radiation received by various facets
        !
        PREC_SW_RF(JI) = PREC_SW_RF(JI) +                          &
             SUM(SW_FLUX%ROOF_IN(JSW,CANOPY_PROPS%ISTARTLAY(JI):   &
             (CANOPY_PROPS%ISTARTLAY(JI)+CANOPY_PROPS%NLAY(JI)-1))) / T%XBLD(JI)
        !
        DMT%XREC_SW_WALL(JI) = DMT%XREC_SW_WALL(JI) +              &
             SUM(SW_FLUX%WALL_IN(JSW,CANOPY_PROPS%ISTARTLAY(JI):   &
             (CANOPY_PROPS%ISTARTLAY(JI)+CANOPY_PROPS%NLAY(JI)-1))) / T%XWALL_O_HOR(JI)
        !
        ZREC_SW_WIN(JI) = ZREC_SW_WIN(JI) +                        &
             SUM(SW_FLUX%WALL_IN(JSW,CANOPY_PROPS%ISTARTLAY(JI):   &
             (CANOPY_PROPS%ISTARTLAY(JI)+CANOPY_PROPS%NLAY(JI)-1))) / T%XWALL_O_HOR(JI)
        !
        PREC_SW_GD(JI) = PREC_SW_GD(JI) + SW_FLUX%GROUND_DN(JSW,JI) / (1.0 - T%XBLD(JI))
        !
        ! Direct solar radiation received by surfaces on the ground
        !
        DMT%XDIR_SW_ROAD(JI) = DMT%XDIR_SW_ROAD(JI) + SW_FLUX%GROUND_DN_DIR(JSW,JI) / (1.0 - T%XBLD(JI))
        !
        DMT%XDIR_SW_GARDEN(JI) = DMT%XDIR_SW_ROAD(JI)
        !
        ! Downwelling diffusive radiation at ground level
        !
        PSCA_SW_GROUND_DOWN(JI) = PSCA_SW_GROUND_DOWN(JI) + &
             ( SW_FLUX%GROUND_DN(JSW,JI) - SW_FLUX%GROUND_DN_DIR(JSW,JI) ) / (1.0 - T%XBLD(JI))
        !
        ! Diffusive radiation in horizontal direction at ground level
        !
        PSCA_SW_GROUND_HOR(JI) = PSCA_SW_GROUND_HOR(JI) + &
             SW_FLUX%GROUND_VERTICAL_DIFF(JSW,JI) / (1.0 - T%XBLD(JI))
        !
        ! Solar radiation reflected by various facets
        !
        PREF_SW_GRND(JI) = PREF_SW_GRND(JI) + ZALB_AGG_GROUND(JI) * &
             SW_FLUX%GROUND_DN(JSW,JI)/(1.0 - T%XBLD(JI))
        !
        PREF_SW_FAC(JI) = PREF_SW_FAC(JI) + ZALB_AGG_FACADE(JI) * &
             SUM(SW_FLUX%WALL_IN(JSW,CANOPY_PROPS%ISTARTLAY(JI):  &
             (CANOPY_PROPS%ISTARTLAY(JI)+CANOPY_PROPS%NLAY(JI)-1))) / T%XWALL_O_HOR(JI)
        !
        ! Upwelling diffusive radiation at ground level
        ! FIXME: It is assumed that the ground reflection is entirely diffusive
        !
        PSCA_SW_GROUND_UP(JI) = PREF_SW_GRND(JI)
        !
     ENDDO
     !
  ENDDO
  !
  ! The fraction of shaded roads
  !
  DMT%XROAD_SHADE(:) = 1.0 - SW_FLUX%GROUND_SUNLIT_FRAC(:)
  !
  IF (TOP%CBEM=='BEM') THEN
     !
     ! Solar radiation absorbed by the window
     !
     DMT%XABS_SW_WIN(:) = ZREC_SW_WIN(:) * ZAGG_ABS_WIN(:)
     !
     ! Calculate the total solar radiation transmitted inside building
     ! and energy not ref., nor absorbed, nor transmitted
     !
     PE_SHADING(:) = 0.0
     DO JCOMP=1,SIZE(B%XFRACOMP,2)
        DMT%XTR_SW_WIN(:,JCOMP) = ZREC_SW_WIN(:) * ZTRAN_WIN(:,JCOMP)
        ZCOMP_E_SHADING(:,JCOMP) = ZREC_SW_WIN(:) * &
             (1.-ZALB_WIN(:,JCOMP)-ZABS_WINSH(:,JCOMP)-ZTRAN_WIN(:,JCOMP))
        PE_SHADING(:) = PE_SHADING(:) + B%XFRACOMP(:,JCOMP) * ZCOMP_E_SHADING(:,JCOMP)
     ENDDO
     !
  ENDIF
  !
  ! Some old shortwave diagnostics not further used in the remaining code are not calculated.
  !
  DMT%XSW_UP_ROOF      (:) = -XUNDEF
  DMT%XSW_UP_CAN       (:) = -XUNDEF
  DMT%XABS_SW_SKY      (:) = -XUNDEF
  DMT%XDIR_SW_WALL_A   (:) = -XUNDEF
  DMT%XDIR_SW_WALL_B   (:) = -XUNDEF
  DMT%XDIR_SW_WALL     (:) = -XUNDEF
  DMT%XDIR_SW_HVEG     (:) = -XUNDEF
  DMT%XNTR_DIR_SW_HVEG (:) = -XUNDEF
  DMT%XSCA_SW_ROAD     (:) = -XUNDEF
  DMT%XSCA_SW_WALL     (:) = -XUNDEF
  DMT%XSCA_SW_GARDEN   (:) = -XUNDEF
  DMT%XSCA_SW_HVEG     (:) = -XUNDEF
  !
  ! ########################################################
  ! Longwave radiation quantities are populated using the
  ! SPARTACUS-Surface outputs
  ! FIXME: No clear air or vegetated air absorption.
  ! ########################################################
  !
  DMT%XABS_LW_ROAD(:)      = 0.0
  DMT%XABS_LW_GARDEN(:)    = 0.0
  DMT%XABS_LW_ROOF(:)      = 0.0
  DMT%XABS_LW_WALL_A(:)    = 0.0
  DMT%XABS_LW_WALL_B(:)    = 0.0
  DMT%XABS_LW_WIN(:)       = 0.0
  DMT%XABS_LW_SNOW_ROAD(:) = 0.0
  DMT%XABS_LW_SNOW_ROOF(:) = 0.0
  DMT%XABS_LW_GREENROOF(:) = 0.0
  DMT%XABS_LW_PANEL(:)     = 0.0
  DMT%XABS_LW_HVEG(:)      = 0.0
  !
  DMT%XREC_LW_GARDEN(:) = 0.0
  DMT%XREC_LW_HVEG(:)   = 0.0
  !
  DMT%XEMIT_LW_GRND(:) = 0.0
  DMT%XEMIT_LW_FAC(:)  = 0.0
  PLW_GROUND_DOWN(:)   = 0.0
  PLW_GROUND_HOR(:)   = 0.0
  !
  DO JLW = 1, CONFIG%NLW
     !
     DMT%XABS_LW_ROAD(:) = DMT%XABS_LW_ROAD(:) + T%XEMIS_ROAD(:) * &
          ( LW_FLUX%GROUND_DN(JLW,:)/(1.0 - T%XBLD(:)) - XSTEFAN * T%XT_ROAD(:,1)**4 )
     !
     DMT%XABS_LW_GARDEN(:) = DMT%XABS_LW_GARDEN(:) + PEMIS_GD(:) * &
          ( LW_FLUX%GROUND_DN(JLW,:)/(1.0 - T%XBLD(:)) - XSTEFAN * PTSRAD_GD(:)**4 )
     !
     DMT%XREC_LW_GARDEN(:) = DMT%XREC_LW_GARDEN(:) + LW_FLUX%GROUND_DN(JLW,:) / (1.0 - T%XBLD(:))
     !
     DMT%XEMIT_LW_GRND(:) = DMT%XEMIT_LW_GRND(:) + (LW_FLUX%GROUND_DN(JLW,:) - LW_FLUX%GROUND_NET(JLW,:))/(1.0 - T%XBLD(:))
     !
     PLW_GROUND_DOWN(:) = PLW_GROUND_DOWN(:) + LW_FLUX%GROUND_DN(JLW,:) / (1.0 - T%XBLD(:))
     !
     PLW_GROUND_HOR(:) = PLW_GROUND_HOR(:) + LW_FLUX%GROUND_VERTICAL_DIFF(JLW,:) / (1.0 - T%XBLD(:))
     !
     DO JI = 1, NCOL
        !
        IF (PDN_RD(JI).GT.0.0) THEN
           DMT%XABS_LW_SNOW_ROAD(JI) = DMT%XABS_LW_SNOW_ROAD(JI) + T%TSNOW_ROAD%EMIS(JI) * &
                ( LW_FLUX%GROUND_DN(JLW,JI)/(1.0 - T%XBLD(JI)) - XSTEFAN * T%TSNOW_ROAD%TS(JI)**4 )
        ELSE
           DMT%XABS_LW_SNOW_ROAD(JI) = XUNDEF
        ENDIF
        !
        DMT%XABS_LW_ROOF(JI) = DMT%XABS_LW_ROOF(JI) +         &
             T%XEMIS_ROOF(JI) * (                                &
             SUM(LW_FLUX%ROOF_IN(JLW,CANOPY_PROPS%ISTARTLAY(JI): &
             (CANOPY_PROPS%ISTARTLAY(JI)+CANOPY_PROPS%NLAY(JI)-1))) / T%XBLD(JI) - &
             XSTEFAN * T%XT_ROOF(JI,1)**4 )
        !
        IF (T%XGREENROOF(JI).GT.0.0) THEN
           !
           DMT%XABS_LW_GREENROOF(JI) = DMT%XABS_LW_GREENROOF(JI) + &
                PEMIS_GR(JI) * (                                      &
                SUM(LW_FLUX%ROOF_IN(JLW,CANOPY_PROPS%ISTARTLAY(JI):   &
                (CANOPY_PROPS%ISTARTLAY(JI)+CANOPY_PROPS%NLAY(JI)-1))) / T%XBLD(JI) - &
                XSTEFAN * PTSRAD_GR(JI)**4 )
           !
        ELSE
           DMT%XABS_LW_GREENROOF(JI) = XUNDEF
        ENDIF
        !
        IF (TPN%XFRAC_PANEL(JI).GT.0.0) THEN
           DMT%XABS_LW_PANEL(JI) = DMT%XABS_LW_PANEL(JI) +       &
                TPN%XEMIS_PANEL(JI) * (                             &
                SUM(LW_FLUX%ROOF_IN(JLW,CANOPY_PROPS%ISTARTLAY(JI): &
                (CANOPY_PROPS%ISTARTLAY(JI)+CANOPY_PROPS%NLAY(JI)-1))) / T%XBLD(JI) - &
                XSTEFAN * DMT%XTS_PANEL(JI)**4 )
        ELSE
           DMT%XABS_LW_PANEL(JI) = XUNDEF       
        ENDIF
        !
        DMT%XABS_LW_WALL_A(JI) = DMT%XABS_LW_WALL_A(JI) +        &
             T%XEMIS_WALL(JI) * (                                &
             SUM(LW_FLUX%WALL_IN(JLW,CANOPY_PROPS%ISTARTLAY(JI): &
             (CANOPY_PROPS%ISTARTLAY(JI)+CANOPY_PROPS%NLAY(JI)-1))) / T%XWALL_O_HOR(JI) - &
             XSTEFAN * T%XT_WALL_A(JI,1)**4 )
        !
        DMT%XABS_LW_WALL_B(JI) = DMT%XABS_LW_WALL_B(JI) +        &
             T%XEMIS_WALL(JI) * (                                &
             SUM(LW_FLUX%WALL_IN(JLW,CANOPY_PROPS%ISTARTLAY(JI): &
             (CANOPY_PROPS%ISTARTLAY(JI)+CANOPY_PROPS%NLAY(JI)-1))) / T%XWALL_O_HOR(JI) - &
             XSTEFAN * T%XT_WALL_B(JI,1)**4 )
        !
        DMT%XABS_LW_WIN(JI) = DMT%XABS_LW_WIN(JI) +              &
             XEMIS_WIN_CST * (                                   &
             SUM(LW_FLUX%WALL_IN(JLW,CANOPY_PROPS%ISTARTLAY(JI): &
             (CANOPY_PROPS%ISTARTLAY(JI)+CANOPY_PROPS%NLAY(JI)-1))) / T%XWALL_O_HOR(JI) - &
             XSTEFAN * B%XT_WIN1(JI)**4 )        
        !
        IF (PDN_RF(JI).GT.0.0) THEN
           !
           DMT%XABS_LW_SNOW_ROOF(JI) = DMT%XABS_LW_SNOW_ROOF(JI) +      &
                T%TSNOW_ROOF%EMIS(JI) * (                               &
                SUM(LW_FLUX%ROOF_IN(JLW,CANOPY_PROPS%ISTARTLAY(JI):     &
                (CANOPY_PROPS%ISTARTLAY(JI)+CANOPY_PROPS%NLAY(JI)-1))) / T%XBLD(JI) - &
                XSTEFAN * T%TSNOW_ROOF%TS(JI)**4 )
           !
        ELSE
           DMT%XABS_LW_SNOW_ROOF(JI) = XUNDEF
        ENDIF
        !
        DMT%XEMIT_LW_FAC(JI) = DMT%XEMIT_LW_FAC(JI) +                 &
             ( SUM(LW_FLUX%WALL_IN(JLW,CANOPY_PROPS%ISTARTLAY(JI):    &
             (CANOPY_PROPS%ISTARTLAY(JI)+CANOPY_PROPS%NLAY(JI)-1))) - &
             SUM(LW_FLUX%WALL_NET(JLW,CANOPY_PROPS%ISTARTLAY(JI):     &
             (CANOPY_PROPS%ISTARTLAY(JI)+CANOPY_PROPS%NLAY(JI)-1))) ) &
             / T%XWALL_O_HOR(JI)
        !
        IF (TOP%CURBTREE/="NONE") THEN
           !
           ! FIXME: Best guess values are used for those grid points
           !        for which FRAC_HVEG is below the threshold.
           !        This leads to small energetic inconsistancies.
           !
           IF (T%XFRAC_HVEG(JI).GT.CONFIG%MIN_VEGETATION_FRACTION) THEN
              !
              DMT%XABS_LW_HVEG(JI) = DMT%XABS_LW_HVEG(JI) +                                 &
                   SUM(LW_FLUX%VEG_ABS(JLW,CANOPY_PROPS%ISTARTLAY(JI):                      &
                   (CANOPY_PROPS%ISTARTLAY(JI)+CANOPY_PROPS%NLAY(JI)-1))) / T%XFRAC_HVEG(JI)
              !
              ! FIXME: division by zero is possible here
              !
              DO JLAYER = 1, CANOPY_PROPS%NLAY(JI)
                 !
                 IF (ZHMEAN_LAYER(JI,JLAYER).LT.GDP%XH_TREE(JI)) THEN
                   !
                    DMT%XREC_LW_HVEG(JI) = DMT%XREC_LW_HVEG(JI) +                                      &
                    CANOPY_PROPS%DZ(CANOPY_PROPS%ISTARTLAY(JI)+JLAYER-1) * (                           &
                    LW_FLUX%VEG_ABS(JLW,CANOPY_PROPS%ISTARTLAY(JI)+JLAYER-1) / T%XFRAC_HVEG(JI) /      &
                    (1.0 - LW_SPECTRAL_PROPS%VEG_SSA(JLW,CANOPY_PROPS%ISTARTLAY(JI)+JLAYER-1))  +      &
                    XSTEFAN * CANOPY_PROPS%VEG_TEMPERATURE(CANOPY_PROPS%ISTARTLAY(JI)+JLAYER-1)**4 ) / &
                    GDP%XH_TREE(JI)
                    !
                 ENDIF
                 !
              ENDDO
              !
           ELSE
              DMT%XABS_LW_HVEG(JI) = 0.0
              DMT%XREC_LW_HVEG(JI) = PLW_RAD(JI)      
           ENDIF
           !
        ENDIF
        !
     ENDDO
  ENDDO
  !
  ! Radiation received by composite vegetation
  !
  DO JI=1,SIZE(T%XROAD)
     !
     ZAGC_GARDEN (JI) = 1.
     ZAGC_HVEG   (JI) = 0.
     !
     IF (T%XROAD(JI)+T%XGARDEN(JI).NE.0.) THEN
        ZGARDEN(JI) =  T%XGARDEN(JI) / (T%XROAD(JI)+T%XGARDEN(JI))
     ELSE
        ZGARDEN(JI)=0.
     ENDIF
     !
     ! New garden/hveg fractions relative to the total fraction of natural covers
     !
     IF (TOP%CURBTREE.NE.'NONE' .AND. ZGARDEN(JI) .GT. 0.) THEN
        !
        ZAGC_GARDEN(JI) = ZGARDEN(JI)    / (T%XURBTREE(JI)+ZGARDEN(JI))
        ZAGC_HVEG(JI)   = T%XURBTREE(JI) / (T%XURBTREE(JI)+ZGARDEN(JI))
        !
     ENDIF
     !
     DMT%XREC_SW_VEG(JI) = DMT%XREC_SW_GARDEN(JI)
     DMT%XREC_LW_VEG(JI) = DMT%XREC_LW_GARDEN(JI)
     !
     IF (T%XFRAC_HVEG(JI).GT.0.) THEN
        !
        DMT%XREC_SW_VEG(JI) = DMT%XREC_SW_GARDEN(JI) * ZAGC_GARDEN(JI) + &
             DMT%XREC_SW_HVEG(JI)   * ZAGC_HVEG(JI)
        !
        DMT%XREC_LW_VEG(JI) = DMT%XREC_LW_GARDEN(JI) * ZAGC_GARDEN(JI) + &
             DMT%XREC_LW_HVEG(JI)   * ZAGC_HVEG(JI)
        !
     ENDIF
     !
  ENDDO
  !
  ! Some old shortwave diagnostics not further used in the remaining code are not calculated.
  !
  DMT%XLW_UP_CAN(:)   = -XUNDEF
  DMT%XLW_UP_ROOF(:)  = -XUNDEF
  DMT%XABS_LW_SKY(:)  = -XUNDEF
  DMT%XNET_LW_HVEG(:) = -XUNDEF
  !
  IF (LHOOK) CALL DR_HOOK('TEB_SPARTACUS',1,ZHOOK_HANDLE)
  !
END SUBROUTINE TEB_SPARTACUS
