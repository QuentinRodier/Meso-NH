!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE GAS_SOILDIF_FLUXES (IO, KK, PK, PEK, DK, DEK, DMK, PTSTEP, HEBUL,    &
                               PPO2, PPCO2, PPCH4, PHCC_O2, PHCC_CO2, PHCC_CH4, &
                               PPI_AERENCHYMA, PO2_ATM, PXEBU,                  &
                               PKO2_AVEG, PKCO2_AVEG, PKCH4_AVEG,               &
                               PKO2_AROOT, PKCO2_AROOT, PKCH4_AROOT,            &
                               PKO2_SURF, PKCO2_SURF, PKCH4_SURF,               &
                               PCOEF_EVAP_O2, PCOEF_EVAP_CO2, PCOEF_EVAP_CH4,   &
                               PCOEF_SURF_O2, PCOEF_SURF_CO2, PCOEF_SURF_CH4,   &
                               PCOEF_PMT_O2, PCOEF_PMT_CO2, PCOEF_PMT_CH4,      &
                               PRO2_MT, PRCO2_MT, PRCH4_MT,                     &
                               PBUBBLE_OUT_CH4, PBUBBLE_IN_CH4                  )
!   ###############################################################
!!**  GAS_SOILDIF_FLUXES 
!!
!!    PURPOSE
!!    -------
!!    Calculates coefficents of fluxes out of gas pools
!!
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
!!      Morel (2018) THESIS
!!      Morel et al. (2019) JAMES
!!      
!!    AUTHOR
!!    ------
!!
!!      B. Decharme           * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    23/06/20
!!
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_ISBA_OPTIONS_n,   ONLY : ISBA_OPTIONS_t
USE MODD_ISBA_n,           ONLY : ISBA_K_t, ISBA_P_t, ISBA_PE_t
USE MODD_DIAG_n,           ONLY : DIAG_t
USE MODD_DIAG_EVAP_ISBA_n, ONLY : DIAG_EVAP_ISBA_t
USE MODD_DIAG_MISC_ISBA_n, ONLY : DIAG_MISC_ISBA_t
!
USE MODD_CSTS,        ONLY : XRHOLW, XLVTT, XTT
!
USE MODD_ISBA_PAR,    ONLY : XDENOM_MIN
!
USE MODD_CO2V_PAR,    ONLY : XMO2, XMCO2, XMCH4, XKGTOG
!
USE MODD_SOILGAS_PAR, ONLY : XALPHA_PMT_O2, XALPHA_PMT_CO2, XALPHA_PMT_CH4, &
                             XROOTPOROSITY, XKO2, XCT1_MT, XCT2_MT, XCT3_MT,&
                             XTAU_CH4_MT
!
USE MODD_SURF_PAR,    ONLY : XUNDEF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!-------------------------------------------------------------------------------
!
!*       0.1 input
!
!
TYPE(ISBA_OPTIONS_t),   INTENT(INOUT) :: IO
TYPE(ISBA_K_t),         INTENT(INOUT) :: KK
TYPE(ISBA_P_t),         INTENT(INOUT) :: PK
TYPE(ISBA_PE_t),        INTENT(INOUT) :: PEK
TYPE(DIAG_t),           INTENT(INOUT) :: DK
TYPE(DIAG_EVAP_ISBA_t), INTENT(INOUT) :: DEK
TYPE(DIAG_MISC_ISBA_t), INTENT(INOUT) :: DMK
!
!
REAL,                 INTENT(IN)      :: PTSTEP         ! time step
!
CHARACTER(LEN=6),     INTENT(IN)      :: HEBUL          ! Method to average water and air diffusivity
                                                        ! 'ADVECT' = Ebullution as an advective layer-by-layer transport (Morel et al. 2019)
                                                        ! 'DIRECT' = Ebullution as direct emissions
!
REAL, DIMENSION(:,:), INTENT(IN)      :: PPO2           ! O2 total soil porosity (m3/m3)
REAL, DIMENSION(:,:), INTENT(IN)      :: PPCO2          ! CO2 total soil porosity (m3/m3)
REAL, DIMENSION(:,:), INTENT(IN)      :: PPCH4          ! CH4 total soil porosity (m3/m3)
!
REAL, DIMENSION(:,:), INTENT(IN)      :: PHCC_O2        ! Henry's constant for O2 (-)
REAL, DIMENSION(:,:), INTENT(IN)      :: PHCC_CO2       ! Henry's constant for CO2 (-)
REAL, DIMENSION(:,:), INTENT(IN)      :: PHCC_CH4       ! Henry's constant for CH4 (-)
!
REAL, DIMENSION(:),   INTENT(IN)      :: PPI_AERENCHYMA ! Vegetation (aerenchyma) permeability (eq. 29) in Morel et al. (2019)
REAL, DIMENSION(:),   INTENT(IN)      :: PO2_ATM        ! O2 atmospheric concentrations for boundary conditions (g/m3 air)
REAL, DIMENSION(:),   INTENT(IN)      :: PKO2_AVEG      ! O2 gas diffusion coefficient in the air into plant media (m/s)
REAL, DIMENSION(:),   INTENT(IN)      :: PKCO2_AVEG     ! CO2 gas diffusion coefficient in the air into plant media (m/s)
REAL, DIMENSION(:),   INTENT(IN)      :: PKCH4_AVEG     ! CH4 gas diffusion coefficient in the air into plant media (m/s)
REAL, DIMENSION(:,:), INTENT(IN)      :: PKO2_AROOT     ! O2 gas diffusion coefficient in the air into root media (m/s)
REAL, DIMENSION(:,:), INTENT(IN)      :: PKCO2_AROOT    ! CO2 gas diffusion coefficient in the air into root media (m/s)
REAL, DIMENSION(:,:), INTENT(IN)      :: PKCH4_AROOT    ! CH4 gas diffusion coefficient in the air into root media (m/s)
REAL, DIMENSION(:),   INTENT(IN)      :: PKO2_SURF      ! Bulk O2 gas conductivity coefficient at soil/snow/atmosphere interface (m/s)
REAL, DIMENSION(:),   INTENT(IN)      :: PKCO2_SURF     ! Bulk CO2 gas conductivity coefficient at soil/snow/atmosphere interface (m/s)
REAL, DIMENSION(:),   INTENT(IN)      :: PKCH4_SURF     ! Bulk CH4 gas conductivity coefficient at soil/snow/atmosphere interface (m/s)
!
REAL, DIMENSION(:,:), INTENT(IN)      :: PXEBU          ! CH4 ebullition concentration threshold (g/m3 Air)
!
!*       0.2 output
!
REAL, DIMENSION(:,:), INTENT(OUT)     :: PCOEF_EVAP_O2  ! flux coefficient for O2 transported by evapotranspiration (s-1)
REAL, DIMENSION(:,:), INTENT(OUT)     :: PCOEF_EVAP_CO2 ! flux coefficient for CO2 transported by evapotranspiration (s-1)
REAL, DIMENSION(:,:), INTENT(OUT)     :: PCOEF_EVAP_CH4 ! flux coefficient for CH4 transported by evapotranspiration (s-1)
!
REAL, DIMENSION(:),   INTENT(OUT)     :: PCOEF_SURF_O2  ! soil/atmosphere interface flux coefficient for O2 (s-1)
REAL, DIMENSION(:),   INTENT(OUT)     :: PCOEF_SURF_CO2 ! soil/atmosphere interface flux coefficient for CO2 (s-1)
REAL, DIMENSION(:),   INTENT(OUT)     :: PCOEF_SURF_CH4 ! soil/atmosphere interface flux coefficient for CH4 (s-1)
!
REAL, DIMENSION(:,:), INTENT(OUT)     :: PCOEF_PMT_O2   ! PMT coefficiet for O2 (s-1)
REAL, DIMENSION(:,:), INTENT(OUT)     :: PCOEF_PMT_CO2  ! PMT coefficient for CO2 (s-1)
REAL, DIMENSION(:,:), INTENT(OUT)     :: PCOEF_PMT_CH4  ! PMT coefficient for CH4 (s-1)
!
REAL, DIMENSION(:,:), INTENT(OUT)     :: PRO2_MT        ! O2 consumed during methanotrophy (gO2/m2/s soil)
REAL, DIMENSION(:,:), INTENT(OUT)     :: PRCO2_MT       ! CO2 produced during methanotrophy (gCO2/m2/s soil)
REAL, DIMENSION(:,:), INTENT(OUT)     :: PRCH4_MT       ! CH4 consumed during methanotrophy (gCH4/m2/s soil)
!
REAL, DIMENSION(:,:), INTENT(OUT)     :: PBUBBLE_OUT_CH4 ! gCH4/m2/s soil
REAL, DIMENSION(:,:), INTENT(OUT)     :: PBUBBLE_IN_CH4  ! gCH4/m2/s soil
!
!-------------------------------------------------------------------------------
!
!*       0.4 local
!
REAL, DIMENSION(SIZE(PK%XDZG,1))                      :: ZHLAI, ZSND, ZF2
!
REAL, DIMENSION(SIZE(PK%XDZG,1))                      :: ZRSOIL ! Soil resitance (s/m)
!
REAL, DIMENSION(SIZE(PK%XDZG,1),SIZE(PK%XDZG,2))      :: ZFEVAP ! amount of water transported by evapotranspiration (kg.m-3.s-1)
!
REAL, DIMENSION(SIZE(PK%XDZG,1),SIZE(PK%XDZG,2))      :: ZCOEF_PMT, ZRA_PMT, ZROOTFRAC, ZWSAT, ZGO2, ZTG, ZKMT
!
LOGICAL, DIMENSION(SIZE(PK%XDZG,1),SIZE(PK%XDZG,2))   :: GWSAT, GBUBBLE_IN
INTEGER, DIMENSION(SIZE(PK%XDZG,1),SIZE(PK%XDZG,2))   :: INLAYER
INTEGER, DIMENSION(SIZE(PK%XDZG,1),SIZE(PK%XDZG,2),1) :: IIN_LAYER
!
REAL                                       :: ZDT, ZSPIN
!
! dimensions
INTEGER         :: INI, INL, INS, IDEPTH
!
! indices
INTEGER         :: JI, JL
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('GAS_SOILDIF_FLUXES',0,ZHOOK_HANDLE)
!
!*       1. Initialisations
!        ------------------
!
!*       1.1 dimensions
!
ZSPIN = REAL(IO%NSPINS)
!
!ZDT = ZSPIN*PTSTEP
ZDT = PTSTEP
!
INI = SIZE(PK%XDZG,1)
INL = SIZE(PK%XDZG,2)
INS = SIZE(PEK%TSNOW%WSNOW,2)
!
!*       1.2 set output to zero
!
PCOEF_EVAP_O2 (:,:) = 0.0
PCOEF_EVAP_CO2(:,:) = 0.0
PCOEF_EVAP_CH4(:,:) = 0.0
!
PCOEF_SURF_O2 (:) = 0.0
PCOEF_SURF_CO2(:) = 0.0
PCOEF_SURF_CH4(:) = 0.0
!
PCOEF_PMT_O2 (:,:) = 0.0
PCOEF_PMT_CO2(:,:) = 0.0
PCOEF_PMT_CH4(:,:) = 0.0
!
PRO2_MT (:,:) = 0.0
PRCO2_MT(:,:) = 0.0
PRCH4_MT(:,:) = 0.0
!
!*       1.3 set local to zero
!
ZFEVAP(:,:) = 0.0
!
!-------------------------------------------------------------------------------
!
!*       2. Gas transported by evapotranspiration
!        ----------------------------------------
!
ZF2(:) = MAX(XDENOM_MIN,DMK%XF2(:))
!
DO JL=1,INL
   DO JI=1,INI
      IDEPTH=PK%NWG_LAYER(JI)
      IF(JL<=IDEPTH)THEN
        ZFEVAP(JI,JL)=MAX(0.0,DEK%XLETR(JI)*DMK%XF2WGHT(JI,JL))/(XLVTT*PK%XDZG(JI,JL)*ZF2(JI)) !(kg.m-3.s-1)
      ENDIF
   ENDDO
ENDDO
ZFEVAP(:,1) = ZFEVAP(:,1) + MAX(0.0,DEK%XLEG(:)/(XLVTT*PK%XDZG(:,1))) !(kg.m-3.s-1)
!
PCOEF_EVAP_O2 (:,:) = PHCC_O2 (:,:) * ZFEVAP(:,:) / XRHOLW ! (s-1)
PCOEF_EVAP_CO2(:,:) = PHCC_CO2(:,:) * ZFEVAP(:,:) / XRHOLW ! (s-1)
PCOEF_EVAP_CH4(:,:) = PHCC_CH4(:,:) * ZFEVAP(:,:) / XRHOLW ! (s-1)
!
!-------------------------------------------------------------------------------
!
!*       3. Soil/atmosphere interface flux coefficient (s-1)
!        ---------------------------------------------
!
ZRSOIL(:) = 0.0
WHERE(PKO2_SURF(:)>0.0)
      ZRSOIL       (:) = 1.0/PKO2_SURF(:)
      PCOEF_SURF_O2(:) = 1.0/((ZRSOIL(:)+DK%XARES(:))*PK%XDZG(:,1))
ENDWHERE
!
ZRSOIL(:) = 0.0
WHERE(PKCO2_SURF(:)>0.0)
      ZRSOIL        (:) = 1.0/PKCO2_SURF
      PCOEF_SURF_CO2(:) = 1.0/((ZRSOIL(:)+DK%XARES(:))*PK%XDZG(:,1))
ENDWHERE
!
ZRSOIL(:) = 0.0
WHERE(PKCH4_SURF(:)>0.0)
      ZRSOIL        (:) = 1.0/PKCH4_SURF
      PCOEF_SURF_CH4(:) = 1.0/((ZRSOIL(:)+DK%XARES(:))*PK%XDZG(:,1))
ENDWHERE
!
!-------------------------------------------------------------------------------
!
!*       4. Plant-mediated transport (PMT)
!        ---------------------------------
!
ZSND(:) = 0.0
DO JL=1,INS
   DO JI=1,INI
      IF(PEK%TSNOW%WSNOW(JI,JL)>0.0)THEN
        ZSND(JI)=ZSND(JI)+PEK%TSNOW%WSNOW(JI,JL)/PEK%TSNOW%RHO(JI,JL)
      ENDIF
   ENDDO
ENDDO
!
ZROOTFRAC(:,1)=PK%XROOTFRAC(:,1)
DO JL=2,INL
   DO JI=1,INI
      IDEPTH=PK%NWG_LAYER(JI)
      IF(JL<=IDEPTH)THEN
        ZROOTFRAC(JI,JL)=MAX(0.0,PK%XROOTFRAC(JI,JL)-PK%XROOTFRAC(JI,JL-1))
      ENDIF
   ENDDO
ENDDO
!
! PMT vegetation properties
!
ZHLAI(:) = MIN((PEK%XLAI(:)-PEK%XLAIMIN(:))/(2.0-PEK%XLAIMIN(:)),1.)
!
ZCOEF_PMT(:,:) = 0.0
DO JL=1,INL
   DO JI=1,INI
      IDEPTH=PK%NWG_LAYER(JI)
      IF(JL<=IDEPTH)THEN              
        ZCOEF_PMT(JI,JL)=XROOTPOROSITY*PPI_AERENCHYMA(JI)*ZROOTFRAC(JI,JL)*ZHLAI(JI)*PEK%XVEG(JI)  !(-)
      ENDIF
   ENDDO
ENDDO
!
! O2 PMT occurs only for aeration of the roots (so only from atmosphere to soil)
!
ZRA_PMT(:,:) = 0.0
DO JL=1,INL
   DO JI=1,INI
      IF(ZCOEF_PMT(JI,JL)>0.0.AND.PKO2_AVEG(JI)>0.0.AND.PEK%XSGASO2(JI,JL)<PO2_ATM(JI))THEN
        ZRA_PMT     (JI,JL)=DK%XARES(JI)+1.0/PKO2_AROOT(JI,JL)+1.0/PKO2_AVEG(JI)                       !(s/m)
        PCOEF_PMT_O2(JI,JL)=PPO2(JI,JL)*XALPHA_PMT_O2*ZCOEF_PMT(JI,JL)/(ZRA_PMT(JI,JL)*PK%XDZG(JI,JL)) !(s-1)
      ENDIF
   ENDDO
ENDDO        
!
! CO2 PMT from atmosphere to soil and inversely
!
ZRA_PMT(:,:) = 0.0
DO JL=1,INL
   DO JI=1,INI
      IF(ZCOEF_PMT(JI,JL)>0.0.AND.PKCO2_AVEG(JI)>0.0)THEN
        ZRA_PMT      (JI,JL)=DK%XARES(JI)+1.0/PKCO2_AROOT(JI,JL)+1.0/PKCO2_AVEG(JI)                       !(s/m)
        PCOEF_PMT_CO2(JI,JL)=PPCO2(JI,JL)*XALPHA_PMT_CO2*ZCOEF_PMT(JI,JL)/(ZRA_PMT(JI,JL)*PK%XDZG(JI,JL)) !(s-1)   
      ENDIF
   ENDDO
ENDDO
!
! CH4 PMT from atmosphere to soil and inversely
!
ZRA_PMT(:,:) = 0.0
DO JL=1,INL
   DO JI=1,INI
      IF(ZCOEF_PMT(JI,JL)>0.0.AND.PKCH4_AVEG(JI)>0.0)THEN
        ZRA_PMT      (JI,JL)=DK%XARES(JI)+1.0/PKCH4_AROOT(JI,JL)+1.0/PKCH4_AVEG(JI)                       !(s/m)
        PCOEF_PMT_CH4(JI,JL)=PPCH4(JI,JL)*XALPHA_PMT_CH4*ZCOEF_PMT(JI,JL)/(ZRA_PMT(JI,JL)*PK%XDZG(JI,JL)) !(s-1)   
      ENDIF
   ENDDO
ENDDO
!
!-------------------------------------------------------------------------------
!
!
!*       5. Methanotrophy (MT)
!        ---------------------
!
ZTG(:,:) = MIN(PEK%XTG(:,:)-XTT,XCT2_MT)
!
ZKMT(:,:) = EXP(XCT1_MT*(ZTG(:,:)-XCT2_MT)/XCT3_MT)/XTAU_CH4_MT
!
WHERE(PEK%XSGASO2(:,:)/=XUNDEF)       
      ZGO2(:,:) = (PEK%XSGASO2(:,:)/((XKO2*XMO2*XKGTOG)+PEK%XSGASO2(:,:)))
ENDWHERE
!
WHERE(PEK%XSGASCH4(:,:)/=XUNDEF)
      PRCH4_MT(:,:) = PPCH4(:,:)*PEK%XSGASCH4(:,:)*ZKMT(:,:)*ZGO2(:,:)*PK%XDZG(:,:) ! gCH4/m2soil/s
ENDWHERE
!
!CH4 + 2*O2 = CO2 + 2*H2O
!
PRO2_MT (:,:) = 2.0*PRCH4_MT(:,:)*(XMO2/XMCH4) ! gO2/m2soil/s
!
PRCO2_MT(:,:) = PRCH4_MT(:,:)*(XMCO2/XMCH4) ! gCO2/m2soil/s
!
!-------------------------------------------------------------------------------
!
!*       6. Gas transported by ebullition
!        --------------------------------
!
IF(HEBUL=='DIRECT')THEN
!
! Because the bubble gas velocity in porous media is ~ 0.1cm/s, we assume that bubble can 
! leave the soil if all layers above the bubble are saturated. Elsewhere, bubble concentration 
! is added in the first layer above the bubble that is not saturated.
!
! Soil layer saturation
!
  GWSAT     (:,:) = .FALSE.
  GBUBBLE_IN(:,:) = .TRUE.
!
  WHERE(PEK%XWG(:,:)/=XUNDEF)
        ZWSAT(:,:) = KK%XWSAT(:,:) - PEK%XWGI(:,:)
        GWSAT(:,:) = (PEK%XWG(:,:)>=0.9*ZWSAT(:,:))
  ENDWHERE
!
! Layers saturated or not above each layer
!
  DO JL=1,INL
     INLAYER(:,JL)=JL
  ENDDO
  WHERE(GWSAT(:,:))INLAYER(:,:)=0
!
  IIN_LAYER(:,:,:)=0
!
  GBUBBLE_IN(:,1)=(.NOT.GWSAT(:,1))
  WHERE(GBUBBLE_IN(:,1))IIN_LAYER(:,1,1)=1
  DO JL=2,INL
     DO JI=1,INI   
        GBUBBLE_IN(JI,JL) = ANY(.NOT.GWSAT(JI,1:JL-1))
        IF(GBUBBLE_IN(JI,JL))THEN
       IIN_LAYER(JI,JL,:) = MAXLOC(INLAYER(JI,1:JL-1))
        ENDIF
     ENDDO
  ENDDO
!
  WHERE(PEK%XWG(:,:)==XUNDEF)IIN_LAYER(:,:,1)=-2
!
! CH4 concentration in bubble thta leave a layers and go into another layer
!
  PBUBBLE_OUT_CH4(:,:)=0.0
  PBUBBLE_IN_CH4(:,:)=0.0
!
  WHERE(GWSAT(:,1))
        PBUBBLE_OUT_CH4(:,1) = MAX(0.0,PEK%XSGASCH4(:,1)-PXEBU(:,1))*PPCH4(:,1)*PK%XDZG(:,1)/ZDT
  ENDWHERE
!
  DO JL=2,INL
     DO JI=1,INI
        IF(GWSAT(JI,JL))THEN
          PBUBBLE_OUT_CH4(JI,JL) = MAX(0.0,PEK%XSGASCH4(JI,JL)-PXEBU(JI,JL))*PPCH4(JI,JL)*PK%XDZG(JI,JL)/ZDT ! gCH4/m2/s soil
        ENDIF
        IF(GBUBBLE_IN(JI,JL).AND.JL<=IDEPTH)THEN
          PBUBBLE_IN_CH4(JI,IIN_LAYER(JI,JL,1)) = PBUBBLE_IN_CH4(JI,IIN_LAYER(JI,JL,1)) + PBUBBLE_OUT_CH4(JI,JL) ! gCH4/m2/s soil
        ENDIF
     ENDDO
  ENDDO
!
ENDIF
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('GAS_SOILDIF_FLUXES',1,ZHOOK_HANDLE)
!
END SUBROUTINE GAS_SOILDIF_FLUXES
