!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE GAS_SOILDIF_PROPERTIES(KK, PK, PEK, DMK, HEBUL,             &
                                  PTA, PRHOA, PPS, PO2_ATM,            &
                                  PPI_AERENCHYMA, PPO2, PPCO2, PPCH4,  &
                                  PHCC_O2, PHCC_CO2, PHCC_CH4,         &
                                  PKO2_AVEG, PKCO2_AVEG, PKCH4_AVEG,   &
                                  PKO2_AROOT, PKCO2_AROOT, PKCH4_AROOT,&
                                  PKO2_SURF, PKCO2_SURF, PKCH4_SURF,   &
                                  PDIFFO2, PDIFFCO2, PDIFFCH4,         &
                                  PCONTROL_TEMP_MG, PCONTROL_MG,       &
                                  PXEBU, PVBUBBLE                      )
!   ###############################################################
!!**  GAS_SOILDIF_PROPERTIES 
!!
!!    PURPOSE
!!    -------
!!    Calculates soil gas properties for soil gas scheme
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
USE MODD_ISBA_n,           ONLY : ISBA_K_t, ISBA_P_t, ISBA_PE_t
USE MODD_DIAG_MISC_ISBA_n, ONLY : DIAG_MISC_ISBA_t
!
USE MODD_CSTS,           ONLY : XG, XTT, XRHOLW, XRHOLI
USE MODD_ISBA_PAR,       ONLY : XWGMIN
!
USE MODD_SOILGAS_PAR
!
USE MODD_CO2V_PAR,       ONLY : XMCH4, XKGTOG
!
USE MODD_SNOW_METAMO,    ONLY : XSNOWDZMIN
!
USE MODD_SURF_PAR,       ONLY : XUNDEF
!
USE MODI_CONTROL_TEMP_FUNC_MG
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
TYPE(ISBA_K_t),         INTENT(INOUT) :: KK
TYPE(ISBA_P_t),         INTENT(INOUT) :: PK
TYPE(ISBA_PE_t),        INTENT(INOUT) :: PEK
TYPE(DIAG_MISC_ISBA_t), INTENT(INOUT) :: DMK
!
!
CHARACTER(LEN=6),     INTENT(IN)      :: HEBUL          ! Method to average water and air diffusivity
                                                        ! 'ADVECT' = Ebullution as an advective layer-by-layer transport (Morel et al. 2019)
                                                        ! 'DIRECT' = Ebullution as direct emissions
!
REAL, DIMENSION(:),   INTENT(IN)      :: PTA           ! air temperature forcing               (K)
REAL, DIMENSION(:),   INTENT(IN)      :: PRHOA         ! air density (kg/m3)
REAL, DIMENSION(:),   INTENT(IN)      :: PPS           ! pressure at atmospheric model surface (Pa)
REAL, DIMENSION(:),   INTENT(IN)      :: PO2_ATM       ! O2 concentration in air (gO2/m3 air)
!
!*       0.2 output
!
REAL, DIMENSION(:),   INTENT(OUT)     :: PPI_AERENCHYMA   ! Vegetation (aerenchyma) permeability (eq. 29) in Morel et al. (2019)
!
REAL, DIMENSION(:,:), INTENT(OUT)     :: PPO2             ! O2 total soil porosity (m3/m3)
REAL, DIMENSION(:,:), INTENT(OUT)     :: PPCO2            ! CO2 total soil porosity (m3/m3)
REAL, DIMENSION(:,:), INTENT(OUT)     :: PPCH4            ! CH4 total soil porosity (m3/m3)
!
REAL, DIMENSION(:,:), INTENT(OUT)     :: PHCC_O2          ! Henry's constant for O2 (-)
REAL, DIMENSION(:,:), INTENT(OUT)     :: PHCC_CO2         ! Henry's constant for CO2 (-)
REAL, DIMENSION(:,:), INTENT(OUT)     :: PHCC_CH4         ! Henry's constant for CH4 (-)
!
REAL, DIMENSION(:),   INTENT(OUT)     :: PKO2_AVEG        ! O2 gas conductivity coefficient in the air into plant media (m/s)
REAL, DIMENSION(:),   INTENT(OUT)     :: PKCO2_AVEG       ! CO2 gas conductivity coefficient in the air into plant media (m/s)
REAL, DIMENSION(:),   INTENT(OUT)     :: PKCH4_AVEG       ! CH4 gas conductivity coefficient in the air into plant media (m/s)
!
REAL, DIMENSION(:,:), INTENT(OUT)     :: PKO2_AROOT       ! O2 gas conductivity coefficient in the air into root media (m/s)
REAL, DIMENSION(:,:), INTENT(OUT)     :: PKCO2_AROOT      ! CO2 gas conductivity coefficient in the air into root media (m/s)
REAL, DIMENSION(:,:), INTENT(OUT)     :: PKCH4_AROOT      ! CH4 gas conductivity coefficient in the air into root media (m/s)
!
REAL, DIMENSION(:),   INTENT(OUT)     :: PKO2_SURF        ! Bulk O2 gas conductivity coefficient at soil/snow/atmosphere interface (m/s)
REAL, DIMENSION(:),   INTENT(OUT)     :: PKCO2_SURF       ! Bulk CO2 gas conductivity coefficient at soil/snow/atmosphere interface (m/s)
REAL, DIMENSION(:),   INTENT(OUT)     :: PKCH4_SURF       ! Bulk CH4 gas conductivity coefficient at soil/snow/atmosphere interface (m/s)
!
REAL, DIMENSION(:,:), INTENT(OUT)     :: PDIFFO2          ! Bulk soil O2 gas diffusion coefficient (m2/s)
REAL, DIMENSION(:,:), INTENT(OUT)     :: PDIFFCO2         ! Bulk soil CO2 gas diffusion coefficient (m2/s)
REAL, DIMENSION(:,:), INTENT(OUT)     :: PDIFFCH4         ! Bulk soil CH4 gas diffusion coefficient (m2/s)
!
REAL, DIMENSION(:,:), INTENT(OUT)     :: PCONTROL_TEMP_MG ! temperature control of methanogenesis
REAL, DIMENSION(:,:), INTENT(OUT)     :: PCONTROL_MG      ! biophysical control of methanogenesis
!
REAL, DIMENSION(:,:), INTENT(OUT)     :: PXEBU            ! CH4 ebullition concentration threshold (g/m3 Air)
REAL, DIMENSION(:,:), INTENT(OUT)     :: PVBUBBLE         ! Ebulition bubble veliocity (m/s)
!
!-------------------------------------------------------------------------------
!
!*       0.3 local parameters
!
REAL,             PARAMETER                       :: ZO2_LIM = 2.0 ! gO2/m3 Water
REAL,             PARAMETER                       :: ZFACLIM = 0.9
!
CHARACTER(LEN=3), PARAMETER                       :: CAVG    = 'GEO' ! Method to average water and air diffusivity
                                                                     ! 'N84' = Nielson et al. 1984
                                                                     ! 'GEO' = geometric mean as in Morel et al
                                                                     ! 'HAR' = harmonic mean
                                                                     ! 'ARI' = aritmethic mean
                                                                     ! 'CLM' = CLM method as in Riley and al. 2011

!
!-------------------------------------------------------------------------------
!
!*       0.4 local
!
REAL, PARAMETER                                    :: ZDG_VEG = 0.2   !Top depth m
!
REAL, DIMENSION(SIZE(PEK%XTG,1))                   :: ZWORK, ZSND, ZTVEG, ZTSN, ZFSN, ZH_VEG_G, ZLOG,        &
                                                      ZSN_DIFFO2_WRK, ZSN_DIFFCO2_WRK, ZSN_DIFFCH4_WRK,      &
                                                      ZSOIL_DIFFO2_WRK, ZSOIL_DIFFCO2_WRK, ZSOIL_DIFFCH4_WRK,&
                                                      ZDIFFO2_ATM, ZDIFFCO2_ATM, ZDIFFCH4_ATM, ZREFO2, ZO2_SUP
!
REAL, DIMENSION(SIZE(PEK%XTG,1),SIZE(PEK%XTG,2))   :: ZAIRFRAC, ZNU_A, ZNU_W, ZWSAT, ZTG, &
                                                      ZDIFFO2_A, ZDIFFCO2_A, ZDIFFCH4_A,  &
                                                      ZDIFFO2_W, ZDIFFCO2_W, ZDIFFCH4_W,  &
                                                      ZMOIST, ZSAT, ZO2, ZSOILPS,         &
                                                      ZWORKPS, ZAIRFRAC2
!
REAL, DIMENSION(SIZE(PEK%TSNOW%WSNOW,1),SIZE(PEK%TSNOW%WSNOW,2)) :: ZSNPOR, ZSNDZ, ZSNRHO, ZSN_WG, ZTSNL,      &
                                                      ZSN_DIFFO2_A, ZSN_DIFFCO2_A, ZSN_DIFFCH4_A,&
                                                      ZSN_DIFFO2_W, ZSN_DIFFCO2_W, ZSN_DIFFCH4_W,&       
                                                      ZSN_AIRFRAC, ZSN_PO2, ZSN_PCO2, ZSN_PCH4,  &
                                                      ZSN_HCC_O2, ZSN_HCC_CO2, ZSN_HCC_CH4,      &
                                                      ZSN_DIFFO2, ZSN_DIFFCO2, ZSN_DIFFCH4
!
REAL                                               :: ZLOG10
!
LOGICAL, DIMENSION(SIZE(PEK%XTG,1))                    :: LSNCOND_LIMIT
!
LOGICAL, DIMENSION(SIZE(PK%XDZG,1),SIZE(PK%XDZG,2))      :: GWSAT
!
! dimensions
INTEGER         :: INI, INL, INS, INTYPE
!
! indices
INTEGER         :: JI, JL, JTYPE
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('GAS_SOILDIF_PROPERTIES',0,ZHOOK_HANDLE)
!
!*       1 Initialisations
!        -----------------------------------------------
!
!*       1.1 dimensions
!
INI    = SIZE(PEK%XTG,1)
INL    = SIZE(PEK%XTG,2)
INS    = SIZE(PEK%TSNOW%RHO,2)
INTYPE = SIZE(PK%XVEGTYPE_PATCH,2)
!
!*       1.2 set output to zero
!
PPO2 (:,:) = XUNDEF
PPCO2(:,:) = XUNDEF
PPCH4(:,:) = XUNDEF
!
PHCC_O2 (:,:) = XUNDEF
PHCC_CO2(:,:) = XUNDEF
PHCC_CH4(:,:) = XUNDEF
!
PPI_AERENCHYMA(:) = 0.0
!
PKO2_AVEG  (:) = 0.0
PKCO2_AVEG (:) = 0.0
PKCH4_AVEG (:) = 0.0
!
PKO2_AROOT (:,:) = 0.0
PKCO2_AROOT(:,:) = 0.0
PKCH4_AROOT(:,:) = 0.0
!
PKO2_SURF (:) = 0.0
PKCO2_SURF(:) = 0.0
PKCH4_SURF(:) = 0.0
!
PDIFFO2 (:,:) = 0.0
PDIFFCO2(:,:) = 0.0
PDIFFCH4(:,:) = 0.0
!
PCONTROL_TEMP_MG(:,:) = 0.0
PCONTROL_MG     (:,:) = 0.0
!
PXEBU   (:,:) = 0.0
PVBUBBLE(:,:) = 0.0
!
!*       1.3 set local to zero
!
ZFSN (:) = 0.0
ZSND (:) = 0.0
ZWORK(:) = 0.0
!
ZSNDZ     (:,:) = 0.0
ZAIRFRAC  (:,:) = XUNDEF
!
!-------------------------------------------------------------------------------
!
!*    2. Henry's constant, total soil gas porosity and Gas Diffusion in air
!        ------------------------------------------------------------------
!
! Henry's constant
!
ZTG(:,:) = MAX(PEK%XTG(:,:),XTT)
!
PHCC_O2 (:,:) = ZTG(:,:) * (XBUNSEN_O2/XTT)
PHCC_CO2(:,:) = ZTG(:,:) * (XBUNSEN_CO2/XTT)
PHCC_CH4(:,:) = ZTG(:,:) * (XBUNSEN_CH4/XTT)
!
! total soil gas porosity
!
DO JL=1,INL
   DO JI=1,INI
      IF(JL<=PK%NWG_LAYER(JI))THEN
!                
        ZAIRFRAC(JI,JL) = MAX(0.,KK%XWSAT(JI,JL)-PEK%XWGI(JI,JL)-PEK%XWG(JI,JL))
!
        PPO2 (JI,JL) = ZAIRFRAC(JI,JL) + PHCC_O2 (JI,JL) * PEK%XWG(JI,JL)
        PPCO2(JI,JL) = ZAIRFRAC(JI,JL) + PHCC_CO2(JI,JL) * PEK%XWG(JI,JL)
        PPCH4(JI,JL) = ZAIRFRAC(JI,JL) + PHCC_CH4(JI,JL) * PEK%XWG(JI,JL)
!
      ENDIF
   ENDDO
ENDDO
!
! Gas Diffusion coefficients in air at air temperature (m2/s)
!
ZDIFFO2_ATM (:) = XA_O2_A  + XB_O2_A  * (PTA(:)-XTT)
ZDIFFCO2_ATM(:) = XA_CO2_A + XB_CO2_A * (PTA(:)-XTT)
ZDIFFCH4_ATM(:) = XA_CH4_A + XB_CH4_A * (PTA(:)-XTT)
!
!-------------------------------------------------------------------------------
!
!*    3. Vegetation (aerenchyma) permeability
!        --------------------------------------------
!
DO JTYPE=1,INTYPE
   DO JI=1,INI
        PPI_AERENCHYMA(JI)=PPI_AERENCHYMA(JI)+XPI_AERENCHYMA(JTYPE)*PK%XVEGTYPE_PATCH(JI,JTYPE)
   ENDDO
ENDDO
!
!-------------------------------------------------------------------------------
!
!*    4. Bulk medium diffusion coefficients in soil
!        ------------------------------------------
!
! Diffusion coefficients in air at soil temperature (m2/s)
!
ZDIFFO2_A (:,:) = XA_O2_A  + XB_O2_A  * (PEK%XTG(:,:)-XTT)
ZDIFFCO2_A(:,:) = XA_CO2_A + XB_CO2_A * (PEK%XTG(:,:)-XTT)
ZDIFFCH4_A(:,:) = XA_CH4_A + XB_CH4_A * (PEK%XTG(:,:)-XTT)
!
! Diffusion coefficients in water at soil temperature (m2/s)
!
ZTG(:,:) = MAX(0.0,PEK%XTG(:,:)-XTT)
!
ZDIFFO2_W (:,:) = XA_O2_W  + XB_O2_W  * ZTG(:,:) + XC_O2_W  * ZTG(:,:)*ZTG(:,:)
ZDIFFCO2_W(:,:) = XA_CO2_W + XB_CO2_W * ZTG(:,:) + XC_CO2_W * ZTG(:,:)*ZTG(:,:)
ZDIFFCH4_W(:,:) = XA_CH4_W + XB_CH4_W * ZTG(:,:) + XC_CH4_W * ZTG(:,:)*ZTG(:,:)
!
WHERE(PEK%XWGI(:,:)/=XUNDEF)
      ZWSAT(:,:) = MAX(XWGMIN,KK%XWSAT(:,:)-PEK%XWGI(:,:))
ENDWHERE
!
CALL BULK_GAS_DIFCOEF('SOIL',CAVG,ZWSAT,ZAIRFRAC,PEK%XWG,ZDIFFO2_A ,ZDIFFO2_W ,PHCC_O2 ,PDIFFO2 )
CALL BULK_GAS_DIFCOEF('SOIL',CAVG,ZWSAT,ZAIRFRAC,PEK%XWG,ZDIFFCO2_A,ZDIFFCO2_W,PHCC_CO2,PDIFFCO2)
CALL BULK_GAS_DIFCOEF('SOIL',CAVG,ZWSAT,ZAIRFRAC,PEK%XWG,ZDIFFCH4_A,ZDIFFCH4_W,PHCC_CH4,PDIFFCH4)
!
!-------------------------------------------------------------------------------
!
!*    5. Bulk medium diffusion coefficients in snow
!        ------------------------------------------
!
ZSNDZ        (:,:) = 0.0
ZSN_HCC_O2   (:,:) = 0.0
ZSN_HCC_CO2  (:,:) = 0.0
ZSN_HCC_CH4  (:,:) = 0.0
ZSN_DIFFO2_A (:,:) = 0.0
ZSN_DIFFCO2_A(:,:) = 0.0
ZSN_DIFFCH4_A(:,:) = 0.0
ZSN_DIFFO2_W (:,:) = 0.0
ZSN_DIFFCO2_W(:,:) = 0.0
ZSN_DIFFCH4_W(:,:) = 0.0
ZSN_WG       (:,:) = 0.0
ZSNRHO       (:,:) = 0.0
ZSNPOR       (:,:) = XUNDEF
ZSN_AIRFRAC  (:,:) = XUNDEF
ZSN_PO2      (:,:) = XUNDEF
ZSN_PCO2     (:,:) = XUNDEF
ZSN_PCH4     (:,:) = XUNDEF
!
WHERE(PEK%TSNOW%WSNOW(:,:)>0.0)
!
! Henry's constant
!
  ZTSNL(:,:) = MAX(DMK%XSNOWTEMP(:,:),XTT)
!
  ZSN_HCC_O2 (:,:) = ZTSNL(:,:) * (XBUNSEN_O2/XTT)
  ZSN_HCC_CO2(:,:) = ZTSNL(:,:) * (XBUNSEN_CO2/XTT)
  ZSN_HCC_CH4(:,:) = ZTSNL(:,:) * (XBUNSEN_CH4/XTT)
!
! Diffusion coefficients in air at soil temperature (m2/s)
!
  ZSN_DIFFO2_A (:,:) = XA_O2_A  + XB_O2_A  * (DMK%XSNOWTEMP(:,:)-XTT)
  ZSN_DIFFCO2_A(:,:) = XA_CO2_A + XB_CO2_A * (DMK%XSNOWTEMP(:,:)-XTT)
  ZSN_DIFFCH4_A(:,:) = XA_CH4_A + XB_CH4_A * (DMK%XSNOWTEMP(:,:)-XTT)
!
! Diffusion coefficients in water at soil temperature (m2/s)
!
  ZTSNL(:,:) = MAX(0.0,DMK%XSNOWTEMP(:,:)-XTT)
!
  ZSN_DIFFO2_W (:,:) = XA_O2_W  + XB_O2_W  * ZTSNL(:,:) + XC_O2_W  * ZTSNL(:,:)*ZTSNL(:,:)
  ZSN_DIFFCO2_W(:,:) = XA_CO2_W + XB_CO2_W * ZTSNL(:,:) + XC_CO2_W * ZTSNL(:,:)*ZTSNL(:,:)
  ZSN_DIFFCH4_W(:,:) = XA_CH4_W + XB_CH4_W * ZTSNL(:,:) + XC_CH4_W * ZTSNL(:,:)*ZTSNL(:,:)
!
! Snow layers thicknesses
!
  ZSNDZ(:,:) = MAX(XSNOWDZMIN,PEK%TSNOW%WSNOW(:,:)/PEK%TSNOW%RHO(:,:))
!
! Snow porosity = Liquid water content has to be substracted from total density 
! to compute the porosity (Rho_ice-(Rho_total_snow-Rho_liqwat))/Rho_ice
!
  ZSN_WG(:,:) = DMK%XSNOWLIQ(:,:)/ZSNDZ(:,:)
!
  ZSNRHO(:,:) = (PEK%TSNOW%RHO(:,:)-XRHOLW*ZSN_WG(:,:))
!
  ZSNPOR(:,:) = 1.0-MIN(1.0,ZSN_WG(:,:)+ZSNRHO(:,:)/XRHOLI)
!
  ZSN_AIRFRAC(:,:) =  MAX(0.,ZSNPOR(:,:)-ZSN_WG(:,:))
!
  ZSN_PO2 (:,:) = ZSN_AIRFRAC(:,:) + ZSN_HCC_O2 (:,:)*ZSN_WG(:,:)
  ZSN_PCO2(:,:) = ZSN_AIRFRAC(:,:) + ZSN_HCC_CO2(:,:)*ZSN_WG(:,:)
  ZSN_PCH4(:,:) = ZSN_AIRFRAC(:,:) + ZSN_HCC_CH4(:,:)*ZSN_WG(:,:)
!
ENDWHERE
!
CALL BULK_GAS_DIFCOEF('SNOW',CAVG,ZSNPOR,ZSN_AIRFRAC,ZSN_WG,ZSN_DIFFO2_A ,ZSN_DIFFO2_W ,PHCC_O2 ,ZSN_DIFFO2 )
CALL BULK_GAS_DIFCOEF('SNOW',CAVG,ZSNPOR,ZSN_AIRFRAC,ZSN_WG,ZSN_DIFFCO2_A,ZSN_DIFFCO2_W,PHCC_CO2,ZSN_DIFFCO2)
CALL BULK_GAS_DIFCOEF('SNOW',CAVG,ZSNPOR,ZSN_AIRFRAC,ZSN_WG,ZSN_DIFFCH4_A,ZSN_DIFFCH4_W,PHCC_CH4,ZSN_DIFFCH4)
!
! Snow depth
!
ZSND(:) = 0.0
DO JL=1,INS
   DO JI=1,INI
      IF(PEK%TSNOW%WSNOW(JI,JL)>0.0)THEN
        ZSND(JI)=ZSND(JI)+ZSNDZ(JI,JL)
      ENDIF
   ENDDO
ENDDO
!
! Snow limit conductivity if snow present or snow porosity > 0.0 
!
LSNCOND_LIMIT(:)=.TRUE.
DO JL=1,INS
   DO JI=1,INI
      IF(ZSNPOR(JI,JL)==0.0.OR.ZSND(JI)==0.0)THEN
        LSNCOND_LIMIT(JI)=.FALSE.
      ENDIF
   ENDDO
ENDDO
!
!-------------------------------------------------------------------------------
!
!*    6. Bulk medium conductivity coefficients at soil/snow/atmosphere interface
!        -----------------------------------------------------------------------
!
! Gas diffusion coefficients in snow accounting for gaz porosity (m2/s)
!
ZSN_DIFFO2_WRK (:)=0.0
ZSN_DIFFCO2_WRK(:)=0.0
ZSN_DIFFCH4_WRK(:)=0.0
!
DO JL=1,INS
   DO JI=1,INI
      IF(LSNCOND_LIMIT(JI))THEN
        ZSN_DIFFO2_WRK (JI)=ZSN_DIFFO2_WRK (JI)+ZSNDZ(JI,JL)/(ZSN_DIFFO2 (JI,JL)*ZSN_PO2 (JI,JL))
        ZSN_DIFFCO2_WRK(JI)=ZSN_DIFFCO2_WRK(JI)+ZSNDZ(JI,JL)/(ZSN_DIFFCO2(JI,JL)*ZSN_PCO2(JI,JL))
        ZSN_DIFFCH4_WRK(JI)=ZSN_DIFFCH4_WRK(JI)+ZSNDZ(JI,JL)/(ZSN_DIFFCH4(JI,JL)*ZSN_PCH4(JI,JL))
      ENDIF
   ENDDO
ENDDO
!
WHERE(LSNCOND_LIMIT(:))
   ZSN_DIFFO2_WRK (:)=ZSND(:)/ZSN_DIFFO2_WRK (:)
   ZSN_DIFFCO2_WRK(:)=ZSND(:)/ZSN_DIFFCO2_WRK(:)
   ZSN_DIFFCH4_WRK(:)=ZSND(:)/ZSN_DIFFCH4_WRK(:)
ENDWHERE
!
! Gas diffusion coefficients at soil/snow interface using harmonic mean 
!
WHERE(LSNCOND_LIMIT(:))
!
  ZSN_DIFFO2_WRK (:) = 1.0/(PDIFFO2 (:,1)*PPO2 (:,1))+1.0/ZSN_DIFFO2_WRK (:)
  ZSN_DIFFCO2_WRK(:) = 1.0/(PDIFFCO2(:,1)*PPCO2(:,1))+1.0/ZSN_DIFFCO2_WRK(:)
  ZSN_DIFFCH4_WRK(:) = 1.0/(PDIFFCH4(:,1)*PPCH4(:,1))+1.0/ZSN_DIFFCH4_WRK(:)
!
  ZSN_DIFFO2_WRK (:) = 2.0/ZSN_DIFFO2_WRK (:)
  ZSN_DIFFCO2_WRK(:) = 2.0/ZSN_DIFFCO2_WRK(:)
  ZSN_DIFFCH4_WRK(:) = 2.0/ZSN_DIFFCH4_WRK(:)
!
ENDWHERE
!
! Gas diffusion coefficients at soil/atmosphere interface (snow free) using harmonic mean 
! accounting for gaz porosity
!
ZSOIL_DIFFO2_WRK (:) = 1.0/(PDIFFO2 (:,1)*PPO2 (:,1))+1.0/ZDIFFO2_ATM (:)
ZSOIL_DIFFCO2_WRK(:) = 1.0/(PDIFFCO2(:,1)*PPCO2(:,1))+1.0/ZDIFFCO2_ATM(:)
ZSOIL_DIFFCH4_WRK(:) = 1.0/(PDIFFCH4(:,1)*PPCH4(:,1))+1.0/ZDIFFCH4_ATM(:)
!
ZSOIL_DIFFO2_WRK (:) = 2.0/ZSOIL_DIFFO2_WRK (:)
ZSOIL_DIFFCO2_WRK(:) = 2.0/ZSOIL_DIFFCO2_WRK(:)
ZSOIL_DIFFCH4_WRK(:) = 2.0/ZSOIL_DIFFCH4_WRK(:)
!
! Gas conductivity coefficients in air at surface/atmosphere interface (m/s)
!
PKO2_SURF (:) = (1.0-PEK%XPSNG(:))*ZSOIL_DIFFO2_WRK (:) + PEK%XPSNG(:)*ZSN_DIFFO2_WRK (:)
PKCO2_SURF(:) = (1.0-PEK%XPSNG(:))*ZSOIL_DIFFCO2_WRK(:) + PEK%XPSNG(:)*ZSN_DIFFCO2_WRK(:)
PKCH4_SURF(:) = (1.0-PEK%XPSNG(:))*ZSOIL_DIFFCH4_WRK(:) + PEK%XPSNG(:)*ZSN_DIFFCH4_WRK(:)
!
PKO2_SURF (:) = ZSOIL_DIFFO2_WRK (:)/PK%XDZG(:,1)
PKCO2_SURF(:) = ZSOIL_DIFFCO2_WRK(:)/PK%XDZG(:,1)
PKCH4_SURF(:) = ZSOIL_DIFFCH4_WRK(:)/PK%XDZG(:,1)
!
!-------------------------------------------------------------------------------
!
!*    7. Conductivity coefficients in vegetation
!        ------------------------------------
!
! Conductivity coefficients in air into root media (m/s)
! We assume that root temperature = soil temperature
!
PKO2_AROOT (:,:) = ZDIFFO2_A (:,:)/(XROOTRATIO*PK%XDG(:,:))
PKCO2_AROOT(:,:) = ZDIFFCO2_A(:,:)/(XROOTRATIO*PK%XDG(:,:))
PKCH4_AROOT(:,:) = ZDIFFCH4_A(:,:)/(XROOTRATIO*PK%XDG(:,:))
!
! Vegetation height where gas emissions occur (m)
!
ZH_VEG_G(:)=0.5*PEK%XH_VEG(:)
!
! Plant temperature without snow = dailly Tair or average 20cm soil temperature
!
ZTVEG(:) = 0.0
DO JL=1,INL
   DO JI=1,INI
      ZWORK(JI)=MIN(PK%XDZG(JI,JL),MAX(0.0,ZDG_VEG-PK%XDG(JI,JL)+PK%XDZG(JI,JL)))/ZDG_VEG
      ZTVEG(JI)=ZTVEG(JI)+PEK%XTG(JI,JL)*ZWORK(JI)
   ENDDO
ENDDO
!
! Plant temperature with snow
!
ZTSN(:) = 0.0
ZSND(:) = 0.0
DO JL=1,INS
   DO JI=1,INI
      IF(PEK%TSNOW%WSNOW(JI,JL)>0.0)THEN
        ZTSN(JI)=ZTSN(JI)+ZSNDZ(JI,JL)*DMK%XSNOWTEMP(JI,JL)
        ZSND(JI)=ZSND(JI)+ZSNDZ(JI,JL)
      ENDIF
   ENDDO
ENDDO
WHERE(ZSND(:)>0.0)
      ZTSN(:)=ZTSN(:)/ZSND(:)
ENDWHERE
!   
ZFSN(:)=0.0
WHERE(PEK%XH_VEG(:)>0.0.AND.PEK%XH_VEG(:)/=XUNDEF)        
     ZFSN(:) = PEK%XPSNV(:)*MIN(1.0,ZSND(:)/PEK%XH_VEG(:))
ENDWHERE
ZTVEG(:) = (ZTVEG(:)*(1.0-ZFSN(:))+ZTSN(:)*ZFSN(:))-XTT
!
! Diffusion coefficients in air into plant media (m2/s)
!
PKO2_AVEG (:) = XA_O2_A  + XB_O2_A  * ZTVEG(:)
PKCO2_AVEG(:) = XA_CO2_A + XB_CO2_A * ZTVEG(:)
PKCH4_AVEG(:) = XA_CH4_A + XB_CH4_A * ZTVEG(:)
!
! Limitation due to snow only if snow depth exeeds vegetation height.
! In this case if snow porosity == 0.0 (ice), PMT is stop
!
ZFSN(:) = 0.0
WHERE(ZSND(:)>=PEK%XH_VEG(:).AND.PEK%XH_VEG(:)/=XUNDEF)        
     ZFSN(:)=PEK%XPSNV(:)*MIN(1.0,MAX(0.0,ZSND(:)-ZH_VEG_G(:))/(PEK%XH_VEG(:)-ZH_VEG_G(:)))
ENDWHERE
!
PKO2_AVEG (:) = (1.0-ZFSN(:))*PKO2_AVEG (:)/ZH_VEG_G(:)+ZFSN(:)*ZSN_DIFFO2_WRK (:)/MAX(ZH_VEG_G(:),ZSND(:))
PKCO2_AVEG(:) = (1.0-ZFSN(:))*PKCO2_AVEG(:)/ZH_VEG_G(:)+ZFSN(:)*ZSN_DIFFCO2_WRK(:)/MAX(ZH_VEG_G(:),ZSND(:))
PKCH4_AVEG(:) = (1.0-ZFSN(:))*PKCH4_AVEG(:)/ZH_VEG_G(:)+ZFSN(:)*ZSN_DIFFCH4_WRK(:)/MAX(ZH_VEG_G(:),ZSND(:))
!
!
!-------------------------------------------------------------------------------
!
!*    8. Methanogenesis control functions
!        --------------------------------
!
! Temperature control on methanogenesis
PCONTROL_TEMP_MG(:,:)=CONTROL_TEMP_FUNC_MG(PEK%XTG(:,:)-XTT)
!
! Note that the superior boundary of the O2 limitation function has been changed compare to eq 25 of Morel et al.
! Now, it is equivalent to 90% of the O2 concentration in air (~ 7 gO2/m3water)
! Was 10 gO2/m3water in Morel et al.
ZO2_SUP(:) = ZFACLIM*MAX(PTA(:),XTT)*(XBUNSEN_O2/XTT)*PO2_ATM(:)
!
ZLOG10 = LOG(10.)
ZREFO2(:) = EXP((ZO2_LIM-ZO2_SUP(:))*(ZLOG10/ZO2_LIM))
!
ZMOIST(:,:) = 0.0
ZSAT  (:,:) = 0.0
ZO2   (:,:) = 0.0
!
DO JL=1,INL
   DO JI=1,INI
      IF(JL<=PK%NWG_LAYER(JI))THEN
        ZMOIST(JI,JL)=MIN(1.0,PEK%XWG(JI,JL)/KK%XWSAT(JI,JL))  
        ZO2   (JI,JL)=EXP((ZO2_LIM-PHCC_O2(JI,JL)*PEK%XSGASO2(JI,JL))*(ZLOG10/ZO2_LIM))
        ZO2   (JI,JL)=MIN(1.0,MAX(0.0,(ZO2(JI,JL)-ZREFO2(JI))/(1.0-ZREFO2(JI))))
      ENDIF
   ENDDO
ENDDO
!
DO JL=1,INL
   DO JI=1,INI
      IF(JL<=PK%NWG_LAYER(JI))THEN
        PCONTROL_MG(JI,JL)=ZO2(JI,JL)*ZMOIST(JI,JL)
      ELSE
        PCONTROL_MG     (JI,JL)=0.0
        PCONTROL_TEMP_MG(JI,JL)=0.0
      ENDIF
   ENDDO
ENDDO
!
!-------------------------------------------------------------------------------
!
!*    9. CH4 ebullition concentration threshold
!        --------------------------------------
!
! Soil pressure
!
ZWORKPS(:,:) = 0.0
!
DO JL=1,INL
   DO JI=1,INI
      IF(JL<=PK%NWG_LAYER(JI))THEN
        ZWORKPS(JI,JL) = XG*(ZAIRFRAC(JI,JL)*PRHOA(JI) + PEK%XWG(JI,JL)*XRHOLW + PEK%XWGI(JI,JL)*XRHOLI)*PK%XDZG(JI,JL)
      ENDIF
   ENDDO
ENDDO
!
ZSOILPS(:,1) = PPS(:) + 0.5*ZWORKPS(:,1)
DO JL=2,INL
   DO JI=1,INI
      IF(JL<=PK%NWG_LAYER(JI))THEN
        ZSOILPS(JI,JL) = ZSOILPS(JI,JL-1) + 0.5*(ZWORKPS(JI,JL-1)+ZWORKPS(JI,JL))
      ENDIF
   ENDDO
ENDDO
!
! ebullition threshold (g/m3)
!
DO JL=1,INL
   DO JI=1,INI
      IF(JL<=PK%NWG_LAYER(JI))THEN
        PXEBU(JI,JL) = (XKGTOG*XRCH4*XMCH4/XRGAS) * ZSOILPS(JI,JL) / PEK%XTG(JI,JL)
      ENDIF
   ENDDO
ENDDO
!
! ebullition velocity (m/s)
!
IF(HEBUL=='ADVECT')THEN
!
  GWSAT(:,:) = .FALSE.
  WHERE(PEK%XWGI(:,:)/=XUNDEF)
        ZWSAT(:,:) = KK%XWSAT(:,:) - PEK%XWGI(:,:)
        GWSAT(:,:) = (PEK%XWG(:,:)>=0.9*KK%XWSAT(:,:))
  ENDWHERE
!
! We assume that bubble velocity is modulated by soil turtosity 
! We assume that soil turtosity is equal to soil porosity
  WHERE(GWSAT(:,:))
       PVBUBBLE(:,:)=XBUBBLEV*KK%XWSAT(:,:)
  ELSEWHERE
       PVBUBBLE(:,:)=0.0
  ENDWHERE
!
  WHERE(PEK%XWG(:,:)==XUNDEF)PVBUBBLE(:,:)=XUNDEF
!
ENDIF
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('GAS_SOILDIF_PROPERTIES',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
CONTAINS
!-------------------------------------------------------------------------------
!
SUBROUTINE BULK_GAS_DIFCOEF(HTYPE,HAVG,PPOR,PAIRFRAC,PWATFRAC,PDIFF_A,PDIFF_W,PHCC,PDIFF)
!
IMPLICIT NONE
!
! Compute effective diffusivity Deff in gas_volume/soil_volume * tortuosity * L**2/Time
!
CHARACTER(LEN=*),     INTENT(IN)  :: HTYPE     ! Soil or Snow ?
CHARACTER(LEN=*),     INTENT(IN)  :: HAVG      ! Method to average water and air diffusivity
REAL, DIMENSION(:,:), INTENT(IN)  :: PPOR      ! Soil total porosity
REAL, DIMENSION(:,:), INTENT(IN)  :: PAIRFRAC  ! Air volume fraction
REAL, DIMENSION(:,:), INTENT(IN)  :: PWATFRAC  ! Water volume fraction
REAL, DIMENSION(:,:), INTENT(IN)  :: PDIFF_A   ! gas diffusion coefficient in air (m2/s)
REAL, DIMENSION(:,:), INTENT(IN)  :: PDIFF_W   ! gas diffusion coefficient in water (m2/s)
REAL, DIMENSION(:,:), INTENT(IN)  :: PHCC      ! Henry's constant
!
REAL, DIMENSION(:,:), INTENT(OUT) :: PDIFF   ! Bulk soil gas diffusion coefficient (m2/s)
!
REAL, DIMENSION(SIZE(PDIFF,1),SIZE(PDIFF,2)) :: ZDIFF_A, ZDIFF_W, ZCOEFA, ZCOEFW, ZSUM
REAL, DIMENSION(SIZE(PDIFF,1),SIZE(PDIFF,2)) :: ZS, ZLOG, ZTOR_A, ZTOR_W, ZTOR_T
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('GAS_SOILDIF_PROPERTIES:BULK_GAS_DIFCOEF',0,ZHOOK_HANDLE)
!
PDIFF(:,:) = 0.0
!
ZTOR_A(:,:) = PPOR(:,:)
ZTOR_W(:,:) = PPOR(:,:)
ZTOR_T(:,:) = PPOR(:,:)
!
IF(HTYPE=='SOIL'.AND.(HAVG=='ARI'.OR.HAVG=='GEO'))THEN
!
! Soil tortuosity given by Moldrup et al. (2003) following Morel et al. (2019)
!
  WHERE(PAIRFRAC(:,:)/=XUNDEF.AND.PAIRFRAC(:,:)>0.0)
        ZS    (:,:) = MIN(1.0,PAIRFRAC(:,:)/PPOR(:,:))
        ZLOG  (:,:) = LOG(ZS(:,:))*3.0/KK%XBCOEF(:,:)
        ZTOR_A(:,:) = PAIRFRAC(:,:)*EXP(ZLOG(:,:))
  ENDWHERE
!
  WHERE(PAIRFRAC(:,:)/=XUNDEF.AND.PWATFRAC(:,:)>0.0)
        ZS    (:,:) = MIN(1.0,PWATFRAC(:,:)/PPOR(:,:))
        ZLOG  (:,:) = LOG(ZS)*(KK%XBCOEF(:,:)/3.0-1.0)
        ZTOR_W(:,:) = PWATFRAC(:,:)*EXP(ZLOG(:,:))
  ENDWHERE
!
ENDIF
!
ZDIFF_A(:,:) = PDIFF_A(:,:)*ZTOR_A(:,:)
ZDIFF_W(:,:) = PDIFF_W(:,:)*ZTOR_W(:,:)
!
ZCOEFA(:,:) = 0.0
ZCOEFW(:,:) = 0.0
!
ZSUM(:,:) = PAIRFRAC(:,:) + PWATFRAC(:,:)
WHERE(PAIRFRAC(:,:)/=XUNDEF.AND.ZSUM(:,:)>0.0)
      ZCOEFA(:,:) = PAIRFRAC(:,:)/ZSUM(:,:)
      ZCOEFW(:,:) = PWATFRAC(:,:)/ZSUM(:,:)
ENDWHERE
!
SELECT CASE(HAVG)
!
  CASE ('ARI')
       WHERE(PAIRFRAC(:,:)/=XUNDEF)
             PDIFF(:,:) = ZDIFF_A(:,:)*PAIRFRAC(:,:)+ZDIFF_W(:,:)*PWATFRAC(:,:)*PHCC(:,:)
       ENDWHERE     
!
  CASE ('GEO')
       WHERE(PAIRFRAC(:,:)/=XUNDEF.AND.(ZDIFF_A(:,:)==0.0.OR.ZDIFF_W(:,:)==0.0))
             PDIFF(:,:) = ZDIFF_A(:,:)*ZCOEFA(:,:)+ZDIFF_W(:,:)*ZCOEFW(:,:)
       ENDWHERE          
       WHERE(PAIRFRAC(:,:)/=XUNDEF.AND.ZDIFF_A(:,:)>0.0.AND.ZDIFF_W(:,:)>0.0)
             PDIFF (:,:) = LOG(ZDIFF_A(:,:))*ZCOEFA(:,:)+LOG(ZDIFF_W(:,:))*ZCOEFW(:,:)
             PDIFF (:,:) = EXP(PDIFF(:,:))
       ENDWHERE
!
  CASE ('CLM')
       ! Soil tortuosity given by Millington and Quirk (1961) as in Wania et al. (2010)
       WHERE(PAIRFRAC(:,:)/=XUNDEF.AND.PAIRFRAC(:,:)>0.0.AND.PWATFRAC(:,:)<=0.95*PPOR(:,:))
             ZLOG  (:,:) = LOG(PAIRFRAC(:,:))*(7.0/3.0)
             ZTOR_A(:,:) = EXP(ZLOG(:,:))/(PPOR(:,:)*PPOR(:,:))
             PDIFF (:,:) = PDIFF_A(:,:) * ZTOR_A(:,:) * PAIRFRAC(:,:)
       ELSEWHERE(PAIRFRAC(:,:)/=XUNDEF)
             PDIFF(:,:) = PDIFF_W(:,:) * ZTOR_W(:,:)
       ENDWHERE
!
  CASE ('HAR')
       WHERE(PAIRFRAC(:,:)/=XUNDEF.AND.ZSUM(:,:)>0.0)
             PDIFF (:,:) = ZCOEFA(:,:)/PDIFF_A(:,:) + ZCOEFW(:,:)/PDIFF_W(:,:)
             PDIFF (:,:) = 1.0/PDIFF(:,:)
       ENDWHERE
       WHERE(PAIRFRAC(:,:)/=XUNDEF)
             PDIFF(:,:) = PDIFF(:,:) * ZTOR_T(:,:)          
       ENDWHERE
!
  CASE ('N84')
       WHERE(PAIRFRAC(:,:)/=XUNDEF.AND.ZSUM(:,:)>0.0)
             PDIFF (:,:) = (ZSUM(:,:) * SQRT(PDIFF_A(:,:)*PDIFF_W(:,:)))  &
                         / (PAIRFRAC(:,:)*SQRT(PDIFF_W(:,:))+PWATFRAC(:,:)*SQRT(PDIFF_A(:,:)))
             PDIFF (:,:) = PDIFF(:,:) * PDIFF(:,:) * ZTOR_T(:,:)
       ENDWHERE
!
END SELECT
!
WHERE(PAIRFRAC(:,:)/=XUNDEF)
      PDIFF(:,:) = MAX(PDIFF(:,:),PDIFF_W(:,:)*ZTOR_T(:,:))
ENDWHERE
!
IF (LHOOK) CALL DR_HOOK('GAS_SOILDIF_PROPERTIES:BULK_GAS_DIFCOEF',1,ZHOOK_HANDLE)
!
END SUBROUTINE BULK_GAS_DIFCOEF       
!
!-------------------------------------------------------------------------------
END SUBROUTINE GAS_SOILDIF_PROPERTIES
