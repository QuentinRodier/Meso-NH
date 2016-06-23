!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
    SUBROUTINE VEGETATION_EVOL(HISBA, HPHOTO, HRESPSL, HALBEDO, OAGRIP,   &
                               OTR_ML, ONITRO_DILU, OAGRI_TO_GRASS,       &
                               OIMP_VEG, OIMP_Z0, OIMP_EMIS,              &
                               PTSTEP, KMONTH, KDAY, KSPINW,              &
                               PTIME, PLAT, PRHOA,                        &
                               PDG, PDZG, KWG_LAYER,                      &
                               PTG, PALBNIR_VEG, PALBVIS_VEG, PALBUV_VEG, &
                               PALBNIR_SOIL, PALBVIS_SOIL, PALBUV_SOIL,   &
                               PVEGTYPE, PSEFOLD, PANMAX, PH_TREE, PBSLAI,& 
                               PLAIMIN, P_CO2, PCE_NITRO, PCF_NITRO,      &
                               PCNA_NITRO, PBSLAI_NITRO, PGMES, PTAU_WOOD,&
                               TPSEED, TPREAP, PAOSIP, PAOSIM, PAOSJP,    &
                               PAOSJM, PHO2IP, PHO2IM, PHO2JP, PHO2JM,    &
                               PZ0EFFIP, PZ0EFFIM, PZ0EFFJP, PZ0EFFJM,    &
                               PLAI, PVEG, PZ0, PALBNIR, PALBVIS, PALBUV, &
                               PEMIS, PANFM, PANDAY, PBIOMASS, PRESP_BIOMASS,&
                               PRESP_BIOMASS_INST, PINCREASE, PTURNOVER,  &
                               PSWDIR)  
!   ###############################################################
!!****  *VEGETATION EVOL*
!!
!!    PURPOSE
!!    -------
!
!     performs the time evolution of vegetation parameters
!     at solar midnight in the case of interactive vegetation (ISBA-Ags)
!              
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!    none
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!      
!!    none
!!
!!    REFERENCE
!!    ---------
!!
!!      
!!    AUTHOR
!!    ------
!!
!!      V. Masson          * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/03/03 
!!      P. Le Moigne 12/2004 : NIT version 
!!      P Le Moigne  09/2005 : AGS modifs of L. Jarlan
!!      A.L. Gibelin 04/2009 : BIOMASS and RESP_BIOMASS arrays
!!      A.L. Gibelin 04/2009 : Add NCB option 
!!      D. Carrer    01/2012 : representation of nitrogen dilution fct of CO2 (from Calvet et al. 2008)
!!      B. Decharme  05/2012 : Optimization and ISBA-DIF coupling
!!      C. Delire    01/2014 : IBIS respiration for tropical evergreen
!!      R. Seferian  05/2015 : expanding of Nitrogen dilution option to the complete formulation proposed by Yin et al. GCB 2002 
!!Seferian & Delire  06/2015 : accouting for living woody biomass respiration (expanding work of E Joetzjer to all woody PFTs) 
!!      B. Decharme    01/16 : Bug when vegetation veg, z0 and emis are imposed whith interactive vegetation
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_CO2V_PAR,       ONLY : XMC, XMCO2, XPCCO2, XRESPFACTOR_NIT,       &
                                XCOEFF_MAINT_RESP_ZERO, XSLOPE_MAINT_RESP, &
                                XPARAM, XPARCF, XDILUDEC
USE MODD_CSTS,           ONLY : XDAY, XTT, XMD
!
USE MODI_ALBEDO
USE MODI_LAIGAIN
USE MODI_LAILOSS
USE MODI_NITRO_DECLINE
USE MODI_EMIS_FROM_VEG
USE MODI_VEG_FROM_LAI
USE MODI_Z0V_FROM_LAI
USE MODI_SUBSCALE_Z0EFF
USE MODD_TYPE_DATE_SURF
USE MODD_DATA_COVER_PAR, ONLY : NVT_TEBD, NVT_TRBE, NVT_BONE,   &
                                NVT_TRBD, NVT_TEBE, NVT_TENE,   &
                                NVT_BOBD, NVT_BOND, NVT_SHRB,   &
                                NVT_TRBE, NVT_C3, NVT_C4,       &
                                NVT_IRR, NVT_GRAS
!
USE MODD_SURF_PAR
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
 CHARACTER(LEN=3),     INTENT(IN)    :: HISBA   ! type of ISBA version:
!                                              ! '2-L' (default)
!                                              ! '3-L'
!                                              ! 'DIF'
 CHARACTER(LEN=3),     INTENT(IN)    :: HPHOTO  ! type of photosynthesis
!                                              ! 'NON'
!                                              ! 'AGS'
!                                              ! 'LAI'
 CHARACTER(LEN=3),     INTENT(IN)    :: HRESPSL ! Soil Respiration
!                                              ! 'DEF' = Norman 1992
!                                              ! 'PRM' = Rivalland PhD Thesis (2003)
!                                              ! 'CNT' = CENTURY model (Gibelin 2008)
 CHARACTER(LEN=4),     INTENT(IN)    :: HALBEDO ! albedo type
!                                              ! 'DRY ' 
!                                              ! 'EVOL' 
!                                              ! 'WET ' 
!                                              ! 'USER'
LOGICAL,              INTENT(IN)    :: OAGRIP  ! agricultural practices
LOGICAL,              INTENT(IN)    :: OTR_ML  ! new radiative transfert
LOGICAL,              INTENT(IN)    :: ONITRO_DILU ! nitrogen dilution fct of CO2 (Calvet et al. 2008)
LOGICAL,              INTENT(IN)    :: OAGRI_TO_GRASS
!
LOGICAL,              INTENT(IN)    :: OIMP_VEG
LOGICAL,              INTENT(IN)    :: OIMP_Z0
LOGICAL,              INTENT(IN)    :: OIMP_EMIS
!
REAL,                 INTENT(IN)    :: PTSTEP  ! time step
INTEGER,              INTENT(IN)    :: KMONTH  ! current month
INTEGER,              INTENT(IN)    :: KDAY    ! current day
INTEGER,              INTENT(IN)    :: KSPINW ! spinup wood
REAL,                 INTENT(IN)    :: PTIME   ! current time since midnight
REAL,   DIMENSION(:), INTENT(IN)    :: PLAT    ! latitude of each grid point
REAL,   DIMENSION(:), INTENT(IN)    :: PRHOA   ! air density
!
REAL, DIMENSION(:,:), INTENT(IN)    :: PDG           ! Depth of Bottom of Soil layers       (m)
REAL, DIMENSION(:,:), INTENT(IN)    :: PDZG          ! soil layers thicknesses (DIF option) (m)
INTEGER, DIMENSION(:),INTENT(IN)    :: KWG_LAYER     ! Number of soil moisture layers (DIF option)
!
REAL, DIMENSION(:,:), INTENT(IN)    :: PTG     ! soil layer average temperatures (K)
REAL,   DIMENSION(:), INTENT(IN)    :: PALBVIS_VEG ! visible, near infra-red and UV
REAL,   DIMENSION(:), INTENT(IN)    :: PALBNIR_VEG ! albedo of the vegetation
REAL,   DIMENSION(:), INTENT(IN)    :: PALBUV_VEG  !
REAL,   DIMENSION(:), INTENT(IN)    :: PALBVIS_SOIL! visible, near infra-red and UV
REAL,   DIMENSION(:), INTENT(IN)    :: PALBNIR_SOIL! soil albedo
REAL,   DIMENSION(:), INTENT(IN)    :: PALBUV_SOIL !
!
REAL, DIMENSION(:,:), INTENT(IN)    :: PVEGTYPE! fraction of each vegetation type
REAL,   DIMENSION(:), INTENT(IN)    :: PSEFOLD ! e-folding time for senescence (s)
REAL,   DIMENSION(:), INTENT(IN)    :: PANMAX  ! maximum photosynthesis rate
REAL,   DIMENSION(:), INTENT(IN)    :: PH_TREE ! height of trees
REAL,   DIMENSION(:), INTENT(IN)    :: PBSLAI  ! ratio of biomass to LAI
REAL,   DIMENSION(:), INTENT(IN)    :: PLAIMIN ! minimum LAI
!
REAL,   DIMENSION(:), INTENT(IN)    :: P_CO2 ! CO2 concentration [ppmm]
!
REAL,   DIMENSION(:), INTENT(IN)    :: PCE_NITRO    ! leaf aera ratio sensibility to nitrogen 
!                                                     concentration (10**2 m2 kg-1)
REAL,   DIMENSION(:), INTENT(IN)    :: PCF_NITRO    ! lethal minimum value of leaf aera ratio 
!                                                     (m2 kg-1)
REAL,   DIMENSION(:), INTENT(IN)    :: PCNA_NITRO   ! nitrogen concentration of active biomass (%)
REAL,   DIMENSION(:), INTENT(IN)    :: PBSLAI_NITRO ! ratio of biomass to LAI
!
REAL,   DIMENSION(:), INTENT(IN)    :: PGMES      ! mesophyll conductance (m s-1)
REAL,   DIMENSION(:), INTENT(IN)    :: PTAU_WOOD  ! residence time in wood (s)
!
!
TYPE (DATE_TIME),   DIMENSION(:), INTENT(IN) :: TPSEED ! seeding date
TYPE (DATE_TIME),   DIMENSION(:), INTENT(IN) :: TPREAP ! reaping date
!
REAL, DIMENSION(:), INTENT(IN)  :: PAOSIP  ! A/S for increasing x
REAL, DIMENSION(:), INTENT(IN)  :: PAOSIM  ! A/S for decreasing x
REAL, DIMENSION(:), INTENT(IN)  :: PAOSJP  ! A/S for increasing y
REAL, DIMENSION(:), INTENT(IN)  :: PAOSJM  ! A/S for decreasing y
REAL, DIMENSION(:), INTENT(IN)  :: PHO2IP  ! h/2 for increasing x
REAL, DIMENSION(:), INTENT(IN)  :: PHO2IM  ! h/2 for decreasing x
REAL, DIMENSION(:), INTENT(IN)  :: PHO2JP  ! h/2 for increasing y
REAL, DIMENSION(:), INTENT(IN)  :: PHO2JM  ! h/2 for decreasing y
!
REAL, DIMENSION(:), INTENT(INOUT) :: PZ0EFFIP! roughness length for increasing x
REAL, DIMENSION(:), INTENT(INOUT) :: PZ0EFFIM! roughness length for decreasing x
REAL, DIMENSION(:), INTENT(INOUT) :: PZ0EFFJP! roughness length for increasing y
REAL, DIMENSION(:), INTENT(INOUT) :: PZ0EFFJM! roughness length for decreasing y
!
REAL,   DIMENSION(:), INTENT(INOUT) :: PLAI    ! leaf area index (LAI) 
REAL,   DIMENSION(:), INTENT(INOUT) :: PVEG    ! vegetation fraction
REAL,   DIMENSION(:), INTENT(INOUT) :: PZ0     ! roughness length: momentum
REAL,   DIMENSION(:), INTENT(INOUT) :: PALBNIR ! snow-free near-infra-red albedo
REAL,   DIMENSION(:), INTENT(INOUT) :: PALBVIS ! snow-free visible albedo
REAL,   DIMENSION(:), INTENT(INOUT) :: PALBUV  ! snow-free UV albedo
REAL,   DIMENSION(:), INTENT(INOUT) :: PEMIS   ! snow-free emissivity
!
REAL,   DIMENSION(:), INTENT(INOUT) :: PANFM              ! maximum leaf assimilation
REAL,   DIMENSION(:), INTENT(INOUT) :: PANDAY             ! daily net CO2 assimilation
REAL, DIMENSION(:,:), INTENT(INOUT) :: PBIOMASS           ! biomass of day-1
REAL, DIMENSION(:,:), INTENT(INOUT) :: PRESP_BIOMASS      ! daily cumulated respiration of biomass (kgDM/m2/day)
REAL, DIMENSION(:,:), INTENT(INOUT) :: PRESP_BIOMASS_INST ! instantaneous respiration of biomass (kgCO2/kgair m/s)
!
REAL, DIMENSION(:,:), INTENT(OUT)   :: PINCREASE          ! increment of biomass (gC m-2 s-1)
REAL, DIMENSION(:,:), INTENT(OUT)   :: PTURNOVER          ! biomass turnover going into litter (gC m-2 s-1)
!
REAL, DIMENSION(:),   INTENT(IN),   OPTIONAL :: PSWDIR    ! Global incoming shortwave radiation (W m-2)
!
!*      0.2    declarations of local parameter
!
REAL, PARAMETER                   :: ZCOEF1 = 10.0
REAL, PARAMETER                   :: ZCOEF2 = 25.0
REAL, PARAMETER                   :: ZDEPTH = 1.0   !Temp depth m
!
REAL, PARAMETER                   :: ZWOOD_IBIS=0.0125
REAL, PARAMETER                   :: ZROOT_IBIS=1.25 
REAL, PARAMETER                   :: ZCIBIS1   =3500.
REAL, PARAMETER                   :: ZCIBIS2   =1./288.
REAL, PARAMETER                   :: ZNDAY     =365.
!
REAL, PARAMETER                   :: ZCDILU1 = -0.048
REAL, PARAMETER                   :: ZCDILU2 = 6.3
REAL, PARAMETER                   :: ZCDILU3 = 371.
! Required for Yin et al., nitrogen dilu param
REAL, PARAMETER                   :: ZPHOTON    = 2.010402e-3 ! conversion coef for W m-2 in photon m-2
REAL, PARAMETER                   :: ZDEPTH_VEG = 0.40        !Depth in meters for daily temperature
REAL, PARAMETER                   :: ZTEMP_VEG  = 23.         !Average temperature of the vegetation
REAL, PARAMETER                   :: ZDECIDUS   = 0.75        !Coef for decidus trees
!
!*      0.3    declarations of local variables
!
REAL, DIMENSION(SIZE(PRESP_BIOMASS,1),SIZE(PRESP_BIOMASS,2)) :: ZRESP_BIOMASS_LAST ! biomass at t-1 (kg_DM/m2/day)
REAL,    DIMENSION(SIZE(PLAI))    :: ZBIOMASS_LEAF   ! temporary leaf biomass 
REAL,    DIMENSION(SIZE(PLAI))    :: ZBSLAI_NITRO    ! (Calvet et al. 2008) ratio of biomass to LAI
                                                     ! with representation of nitrogen dilution
REAL,    DIMENSION(SIZE(PLAI)) :: ZCO2, ZCNA_NITRO   ! fct of CO2        
REAL,    DIMENSION(SIZE(PLAI)) :: ZPARAM
REAL,    DIMENSION(SIZE(PLAI)) :: ZHTREE, ZSAPFRAC   ! tree height & sap fraction used for estimation of 
                                                     ! sapwood fraction
!
REAL                              :: ZLOG2, ZWORK
!
REAL, DIMENSION(SIZE(PTG,1))      :: ZTG_VEG      ! surface temperature   (C)
REAL, DIMENSION(SIZE(PTG,1))      :: ZTG_SOIL     ! soil temperature   (C)
REAL, DIMENSION(SIZE(PTG,1))      :: ZDG_SOIL     ! soil depth for DIF (m)
REAL                              :: ZWGHT_SOIL   ! Weight for DIF (m)
!
LOGICAL, DIMENSION(SIZE(PLAI))    :: GWOOD,GHERB
LOGICAL, DIMENSION(SIZE(PLAI))    :: GMASK_AGRI
LOGICAL                           :: GMASK
INTEGER                           :: INI, INL, JI, JL, IDEPTH, JTYPE
!
REAL,    DIMENSION(SIZE(PVEGTYPE,1),SIZE(PVEGTYPE,2)) :: ZPARAM_TYPE
!
! * Azote
REAL,    DIMENSION(SIZE(PLAI)) :: ZFERT
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-----------------------------------------------------------------
!
!*      1.     Preliminaries
!              -------------
!
IF (LHOOK) CALL DR_HOOK('VEGETATION_EVOL',0,ZHOOK_HANDLE)
!
INI=SIZE(PTG,1)
INL=SIZE(PTG,2)
!
ZLOG2 = LOG(2.0)
!
ZTG_SOIL(:) = 0.0
ZTG_VEG (:) = 0.0
!
! Define herbaceous and woody patches
GHERB(:) = ( PVEGTYPE(:,NVT_TEBD) + PVEGTYPE(:,NVT_TRBE) + PVEGTYPE(:,NVT_BONE)    &
&          + PVEGTYPE(:,NVT_TRBD) + PVEGTYPE(:,NVT_TEBE) + PVEGTYPE(:,NVT_TENE)    &
&          + PVEGTYPE(:,NVT_BOBD) + PVEGTYPE(:,NVT_BOND) + PVEGTYPE(:,NVT_SHRB)<0.5)
GWOOD(:) = (.NOT.GHERB (:))
!
! Mask where vegetation evolution is performed (just before solar midnight)
GMASK = ( PTIME - PTSTEP < 0. ) .AND. ( PTIME >= 0. )
!
! Save RESP_BIOMASS at t-1
IF (GMASK) THEN
  PRESP_BIOMASS     (:,1) = 0.0
  ZRESP_BIOMASS_LAST(:,:) = 0.0
ELSE
  PRESP_BIOMASS     (:,1) = PRESP_BIOMASS(:,1) + PRESP_BIOMASS_INST(:,1) * (PTSTEP*PRHOA(:)*XMC)/(XPCCO2*XMCO2)
  ZRESP_BIOMASS_LAST(:,:) = PRESP_BIOMASS(:,:)
ENDIF
!
!*      2.     Interactive vegetation
!              ----------------------
!
!  LAI daily mortality and assimilation
!
ZBIOMASS_LEAF(:) = PBIOMASS(:,1)
!
IF (GMASK) THEN
!        
  IF (HPHOTO=='LAI' .OR. HPHOTO=='LST')THEN
!
    CALL LAILOSS(PVEG, PSEFOLD, PANMAX, PANDAY, PANFM, ZBIOMASS_LEAF)  
    CALL LAIGAIN(PBSLAI, PLAIMIN, PVEG, ZBIOMASS_LEAF, PLAI, PANDAY)
    PBIOMASS(:,1) = ZBIOMASS_LEAF(:)
!    
  ELSE IF (HPHOTO=='NIT' .OR. HPHOTO=='NCB') THEN
!    
    PINCREASE   (:,:) = 0.0
    PTURNOVER   (:,:) = 0.0
    ZBSLAI_NITRO(:  ) = PBSLAI_NITRO(:)  
!
    IF(ONITRO_DILU)THEN
!
!     * Compute Vegetation temperature
!       We use the temperature of the second layer of the soil (<40cm)
!       since the parametrization employs a daily temperature
!
      IF(HISBA/='DIF')THEN        
        ZTG_VEG(:) = PTG(:,2)
      ELSE 
        DO JI=1,INI
           IDEPTH=KWG_LAYER(JI)
           ZDG_SOIL(JI)=MIN(ZDEPTH_VEG,PDG(JI,IDEPTH))
        ENDDO  
        DO JL=1,INL
           DO JI=1,INI     
              ZWGHT_SOIL=MIN(PDZG(JI,JL),MAX(0.0,ZDG_SOIL(JI)-PDG(JI,JL)+PDZG(JI,JL)))        
              ZTG_VEG(JI)=ZTG_VEG(JI)+PTG(JI,JL)*ZWGHT_SOIL/ZDG_SOIL(JI)
           ENDDO
        ENDDO 
      ENDIF
!
      ZPARAM(:) = 0.0
      ZFERT (:) = 0.0
      DO JTYPE=1,SIZE(PVEGTYPE,2)
        DO JI = 1,INI
            ZPARAM_TYPE(JI,JTYPE) = XDILUDEC(JTYPE) * (ZDECIDUS + 1.1 * ZPHOTON * XPARCF * PSWDIR(JI)       &
                                  + (ZTG_VEG(JI)-XTT)/ZTEMP_VEG - 0.33 * ZFERT(JI))                         &
                                  + (1 - XDILUDEC(JTYPE)) * (           1.1 * ZPHOTON * XPARCF * PSWDIR(JI) &
                                  + (ZTG_VEG(JI)-XTT)/ZTEMP_VEG - 0.33 * ZFERT(JI))
            ZPARAM(JI) = ZPARAM(JI) + ZPARAM_TYPE(JI,JTYPE) * PVEGTYPE(JI,JTYPE)
        ENDDO 
      ENDDO  

      WHERE((PCE_NITRO(:)*PCNA_NITRO(:)+PCF_NITRO(:))/=0.0.AND.PCNA_NITRO(:)/=0.0)
            ZCO2        (:) = P_CO2(:)*(XMD/(1.E-6*XMCO2))  ! (ppmm ->  ppm)
            ZCNA_NITRO  (:) = PCNA_NITRO(:)*EXP(ZCDILU1*EXP(ZPARAM(:)-PCNA_NITRO(:)/ZCDILU2)*ALOG(MAX(1.,ZCO2(:)/ZCDILU3)))
            ZBSLAI_NITRO(:) = 1. / (PCE_NITRO(:)*ZCNA_NITRO(:)+PCF_NITRO(:))
      ENDWHERE
!
    ENDIF
!    
    IF(ANY(PLAI(:)/=XUNDEF))THEN
      CALL NITRO_DECLINE(HPHOTO, HRESPSL, OTR_ML, KSPINW,                     &
                         ZBSLAI_NITRO, PSEFOLD, PGMES, PANMAX, PANDAY,        &
                         PLAT, PLAIMIN, PVEGTYPE, PTAU_WOOD,                  &
                         PANFM, PLAI, PBIOMASS, PRESP_BIOMASS, ZBIOMASS_LEAF, &
                         PINCREASE, PTURNOVER                               )
      CALL LAIGAIN(ZBSLAI_NITRO, PLAIMIN, PVEG, ZBIOMASS_LEAF, PLAI, PANDAY)
    ENDIF
!    
  ENDIF
!  
! CASE CPHOTO=AST reinitialise  PANDAY and PANFM 
  PANDAY=0.0
  PANFM =0.0
!
ENDIF
!
!
IF (HPHOTO == 'NIT' .OR. HPHOTO=='NCB') THEN
  !
  ! * soil temperature in K (over 1m depth for DIF)
  !
  ZTG_VEG(:) = PTG(:,1)
  !
  IF(HISBA/='DIF')THEN        
    ZTG_SOIL(:) = PTG(:,2)
  ELSE       
    DO JI=1,INI
       IDEPTH=KWG_LAYER(JI)
       ZDG_SOIL(JI)=MIN(ZDEPTH,PDG(JI,IDEPTH))
    ENDDO  
    DO JL=1,INL
       DO JI=1,INI     
          ZWGHT_SOIL=MIN(PDZG(JI,JL),MAX(0.0,ZDG_SOIL(JI)-PDG(JI,JL)+PDZG(JI,JL)))        
          ZTG_SOIL(JI)=ZTG_SOIL(JI)+PTG(JI,JL)*ZWGHT_SOIL/ZDG_SOIL(JI)
       ENDDO
    ENDDO 
  ENDIF
  !
  !
  ! * Respiration of structural biomass pools
  !
  WHERE(GWOOD(:))
  ! IBIS respiration with either respiration factor rwood=0.0125 - otherwise rroot=1.25 
  ! (Kucharik et al, 2000, eq 6-8) Soil temp in K         
    PRESP_BIOMASS(:,2) = PRESP_BIOMASS(:,2) + PBIOMASS(:,2) * PTSTEP &
                                            * MAX(0.,ZROOT_IBIS*EXP(ZCIBIS1*(ZCIBIS2-1./ZTG_VEG(:)))/(ZNDAY*XDAY)) 
  ELSEWHERE 
    PRESP_BIOMASS(:,2) = PRESP_BIOMASS(:,2) + PBIOMASS(:,2) * XRESPFACTOR_NIT    &
                                            * EXP((ZLOG2/ZCOEF1)*(ZTG_VEG(:)-XTT-ZCOEF2)) * PTSTEP  
  ! before optimization                   * 2.0**((PTG(:,2)-XTT-ZCOEF2)/ZCOEF1) * PTSTEP               
  ENDWHERE
  !
  IF (HPHOTO == 'NIT') THEN
    !
    PRESP_BIOMASS(:,3) = PRESP_BIOMASS(:,3) + PBIOMASS(:,3) * XRESPFACTOR_NIT &
                                            * EXP((ZLOG2/ZCOEF1)*(ZTG_SOIL(:)-XTT-ZCOEF2)) * PTSTEP  
    ! before optimization                   * 2.0**((PTG(:,2)-XTT-ZCOEF2)/ZCOEF1) * PTSTEP               
    !
  ELSEIF (HPHOTO == 'NCB') THEN
    !
    PRESP_BIOMASS(:,2) = MIN(PRESP_BIOMASS(:,2), PBIOMASS(:,2))
    ! 
    PRESP_BIOMASS(:,3) = PRESP_BIOMASS(:,3) + PBIOMASS(:,3) * MAX( 0., &
        XCOEFF_MAINT_RESP_ZERO * (1. + XSLOPE_MAINT_RESP*(ZTG_VEG(:)-XTT))) * PTSTEP  
    PRESP_BIOMASS(:,3) = MIN(PRESP_BIOMASS(:,3), PBIOMASS(:,3))
    ! 
    WHERE(GWOOD(:))
    ! Resp IBIS (Soil temp in K)
      PRESP_BIOMASS(:,4) = PRESP_BIOMASS(:,4) + PBIOMASS(:,4) * PTSTEP &
                                              * MAX(0.,ZROOT_IBIS * EXP(ZCIBIS1*(ZCIBIS2-1./ZTG_SOIL(:)))/(ZNDAY*XDAY))
    ELSEWHERE 
    PRESP_BIOMASS(:,4) = PRESP_BIOMASS(:,4) + PBIOMASS(:,4) * MAX( 0., &
        XCOEFF_MAINT_RESP_ZERO * (1. + XSLOPE_MAINT_RESP*(ZTG_SOIL(:)-XTT))) * PTSTEP  
    ENDWHERE
    !
    PRESP_BIOMASS(:,4) = MIN(PRESP_BIOMASS(:,4), PBIOMASS(:,4))
    !
    WHERE( (GWOOD(:)).AND.(PBIOMASS(:,5)>0.) )
    ! IBIS estimation of sapwood fraction based on the height of tree, sapspeed and 
    ! max transpiration rates. Conversion from DM to C. To be changed with DGVM.  (Soil temp in K)        
      ZHTREE(:) = 2.5*0.75*(PBIOMASS(:,1)+PBIOMASS(:,2)+PBIOMASS(:,3)+PBIOMASS(:,4)+PBIOMASS(:,5)+PBIOMASS(:,6))*0.4
      ZSAPFRAC(:) = MIN(0.5, MAX(0.05,0.0025/25.*ZHTREE(:)*0.75*400/(PBIOMASS(:,5)*0.4)))
      PRESP_BIOMASS(:,5) = PRESP_BIOMASS(:,5) + PBIOMASS(:,5) * ZSAPFRAC(:) * PTSTEP &
                                              * MAX(0.,ZWOOD_IBIS*EXP(ZCIBIS1*(ZCIBIS2-1./ZTG_VEG(:)))/(ZNDAY*XDAY))
      PRESP_BIOMASS(:,5) = MIN(PRESP_BIOMASS(:,5), PBIOMASS(:,5))
    ELSEWHERE
      PRESP_BIOMASS(:,5) = 0.0
    ENDWHERE

    !
  ENDIF
  !
  ! * Instantaneous respiration (kgCO2/kgair m/s)
  !
  DO JL=2,SIZE(PRESP_BIOMASS,2)
      PRESP_BIOMASS_INST(:,JL) = (PRESP_BIOMASS(:,JL) - ZRESP_BIOMASS_LAST(:,JL)) &
                                     * XPCCO2*XMCO2/(PTSTEP*PRHOA(:)*XMC)                              
  ENDDO
 !  
ENDIF

!*      3.     Agricultural practices
!              ----------------------
!
IF (OAGRIP) THEN
  !
  GMASK_AGRI(:) = .FALSE.
  WHERE ( TPSEED(:)%TDATE%MONTH /= NUNDEF .AND. ( KMONTH < TPSEED(:)%TDATE%MONTH .OR. &
         (KMONTH == TPSEED(:)%TDATE%MONTH .AND. KDAY < TPSEED(:)%TDATE%DAY) ) )  GMASK_AGRI(:) = .TRUE.
  WHERE ( TPREAP(:)%TDATE%MONTH /= NUNDEF .AND. ( KMONTH > TPREAP(:)%TDATE%MONTH .OR. &
         (KMONTH == TPREAP(:)%TDATE%MONTH .AND. KDAY >= TPREAP(:)%TDATE%DAY) ) ) GMASK_AGRI(:) = .TRUE. 
  !
  WHERE (GMASK_AGRI(:))
    PLAI(:)             = PLAIMIN(:)
    ZBIOMASS_LEAF(:)    = PLAI(:) * ZBSLAI_NITRO(:)
  END WHERE

  IF (HPHOTO == 'NIT' .OR. HPHOTO == 'NCB') THEN
    !
    WHERE (GMASK_AGRI(:))
      PBIOMASS(:,1)       = 0.0
      PBIOMASS(:,2)       = 0.0
      PBIOMASS(:,3)       = 0.0
      PRESP_BIOMASS(:,2)  = 0.0
      PRESP_BIOMASS(:,3)  = 0.0
    END WHERE
    !
    IF (HPHOTO == 'NCB') THEN
      !
      WHERE (GMASK_AGRI(:)) 
        PBIOMASS(:,4)       = 0.0
        PBIOMASS(:,5)       = 0.0
        PBIOMASS(:,6)       = 0.0
        PRESP_BIOMASS(:,4)  = 0.0
      END WHERE
      !
    ENDIF
    !
  ENDIF
  !
ENDIF
!
!*      4.     Physical parameters depending on vegetation
!              -------------------------------------------
!
IF (GMASK) THEN
  !
  ! Evolution of vegetation fraction and roughness length due to LAI change
  IF(.NOT.OIMP_Z0) THEN
    WHERE( PVEG(:) > 0. ) PZ0 (:) = Z0V_FROM_LAI(PLAI(:),PH_TREE(:),PVEGTYPE(:,:),OAGRI_TO_GRASS) 
  ENDIF
  IF(.NOT.OIMP_VEG) THEN
    WHERE( PVEG(:) > 0. ) PVEG(:) = VEG_FROM_LAI(PLAI(:),PVEGTYPE(:,:),OAGRI_TO_GRASS)
  ENDIF
  !
  ! Evolution of radiative parameters due to vegetation fraction change
  IF(.NOT.OIMP_EMIS) THEN
    WHERE( PVEG(:) > 0. ) PEMIS(:)= EMIS_FROM_VEG(PVEG(:),PVEGTYPE(:,:))
  ENDIF
  !
  CALL ALBEDO(HALBEDO,                                  &
              PALBVIS_VEG,PALBNIR_VEG,PALBUV_VEG,PVEG,  &
              PALBVIS_SOIL,PALBNIR_SOIL,PALBUV_SOIL,    &
              PALBVIS,PALBNIR,PALBUV                    )  
  !
  ! Evolution of effective roughness length due to new surface roughness length
  !
  IF (SIZE(PAOSIP)>0) &
  CALL SUBSCALE_Z0EFF(PAOSIP,PAOSIM,PAOSJP,PAOSJM,         &
                      PHO2IP,PHO2IM,PHO2JP,PHO2JM,PZ0,     &
                      PZ0EFFIP,PZ0EFFIM,PZ0EFFJP,PZ0EFFJM  ) 
  !
ENDIF
!
IF (LHOOK) CALL DR_HOOK('VEGETATION_EVOL',1,ZHOOK_HANDLE)
!-----------------------------------------------------------------
!
END SUBROUTINE VEGETATION_EVOL
