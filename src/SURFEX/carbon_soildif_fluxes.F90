!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE CARBON_SOILDIF_FLUXES (IO, KK, PK, PEK, DMK, PTSTEP,                        &
                                  PCONTROL_TEMP, PCONTROL_MOIST, PCONTROL_LEACH_SOIL,  &
                                  PCONTROL_TEMP_MG, PCONTROL_MG, PPO2, PPCO2,          &
                                  PFOXIC_LITTER, PFMG_LITTER, PFLEACH_LITTER,          &
                                  PFOXIC_SOC, PFMG_SOC, PFLEACH_SOC                    )
!   ###############################################################
!!**  CARBON_SOILDIF_FLUXES 
!!
!!    PURPOSE
!!    -------
!!    Calculates carbon oxic, anoxic and leaching fluxes out of soil litter and carbon pools
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
!!      Parton et al., Biogeochemestry, 1988
!!      Krinner et al., Global Biochemical Cycles, 2005
!!      Gibelin et al. 2008, AFM
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
USE MODD_DIAG_EVAP_ISBA_n, ONLY : DIAG_EVAP_ISBA_t
USE MODD_DIAG_MISC_ISBA_n, ONLY : DIAG_MISC_ISBA_t
!
USE MODD_CO2V_PAR,       ONLY : XTAU_LITTER, XTAU_SOILCARB, XMO2, XMC
!
USE MODD_SOILGAs_PAR,    ONLY : XRATIO_MG
!
USE MODD_SURF_PAR,       ONLY : XUNDEF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!-------------------------------------------------------------------------------
!
!*       0.1 Arguments
!
!
TYPE(ISBA_OPTIONS_t),   INTENT(INOUT) :: IO
TYPE(ISBA_K_t),         INTENT(INOUT) :: KK
TYPE(ISBA_P_t),         INTENT(INOUT) :: PK
TYPE(ISBA_PE_t),        INTENT(INOUT) :: PEK
TYPE(DIAG_MISC_ISBA_t), INTENT(INOUT) :: DMK
!
!
REAL, INTENT(IN)                      :: PTSTEP               ! time step in s
!
REAL, DIMENSION(:,:), INTENT(IN)      :: PCONTROL_TEMP        ! temperature control of heterotrophic respiration
REAL, DIMENSION(:,:), INTENT(IN)      :: PCONTROL_MOIST       ! moisture control of heterotrophic respiration
REAL, DIMENSION(:,:), INTENT(IN)      :: PCONTROL_LEACH_SOIL  ! leaching transfer function for doc
REAL, DIMENSION(:,:), INTENT(IN)      :: PCONTROL_TEMP_MG     ! temperature control of methanogenesis
REAL, DIMENSION(:,:), INTENT(IN)      :: PCONTROL_MG          ! biophysical control of methanogenesis
!
REAL, DIMENSION(:,:), INTENT(IN)      :: PPO2                 ! O2 total soil porosity (m3/m3)
REAL, DIMENSION(:,:), INTENT(IN)      :: PPCO2                ! CO2 total soil porosity (m3/m3)
!
REAL, DIMENSION(:,:,:), INTENT(OUT)   :: PFOXIC_LITTER        ! oxic flux out of structural litter (gC/m**2)
REAL, DIMENSION(:,:,:), INTENT(OUT)   :: PFMG_LITTER          ! methanogenesis flux out of structural litter (gC/m**2)
REAL, DIMENSION(:,:,:), INTENT(OUT)   :: PFLEACH_LITTER       ! leaching flux out of metabolic litter (gC/m**2)
!
REAL, DIMENSION(:,:,:), INTENT(OUT)   :: PFOXIC_SOC           ! oxic flux out of carbon pools by layers (gC/m**2)
REAL, DIMENSION(:,:),   INTENT(OUT)   :: PFMG_SOC             ! methanogenesis flux out of carbon pools by layers (gC/m**2)
REAL, DIMENSION(:,:,:), INTENT(OUT)   :: PFLEACH_SOC          ! leaching flux out of carbon pools by layers (gC/m**2)
!
!-------------------------------------------------------------------------------
!
!*       0.2 local
!
REAL, DIMENSION(SIZE(PEK%XSOILDIF_LITTER,1),SIZE(PEK%XSOILDIF_LITTER,2))                       :: ZLITT_FD   ! fraction of structural or metabolic litter decomposed
!
REAL, DIMENSION(SIZE(PEK%XSOILDIF_CARB,1),SIZE(PEK%XSOILDIF_CARB,2),SIZE(PEK%XSOILDIF_CARB,3)) :: ZTSOILPOOL !soil turn-over frequency by layer (s-1)
!
REAL, DIMENSION(SIZE(PEK%XSOILDIF_CARB,1),SIZE(PEK%XSOILDIF_CARB,2))                           :: ZSOILP_EFFECT, ZCONTROL ! Work array
!
REAL, DIMENSION(SIZE(PEK%XSOILDIF_CARB,1),SIZE(PEK%XSOILDIF_CARB,3))                           :: ZFLUXTOT, ZSOILCARB
!
REAL, DIMENSION(SIZE(PEK%XSOILDIF_CARB,1),SIZE(PEK%XSOILDIF_CARB,2))                           :: ZDCMAX_OXIC ! Maximum carbon available for oxic decomposition (oxygen limitation) (in gC/m2)
!
REAL, DIMENSION(SIZE(PEK%XSOILDIF_CARB,1),SIZE(PEK%XSOILDIF_CARB,2))                           :: ZSUM_FOXIC ! Sum of all oxic fluxes
!
REAL            :: ZDT, ZSPIN, ZDT_LIT1, ZDT_LIT2
!
! dimensions
INTEGER         :: INI, INL, IDEPTH
!
! indices
INTEGER         :: JI, JL
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
! correspondence between array indices and litter levels
! LT_ABOVE = 1
! LT_BELOW = 2
! correspondence between array indices and soil carbon pools
! SL_ACTIVE = 1
! SL_SLOW = 2
! SL_PASSIVE = 3
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('CARBON_SOILDIF_FLUXES',0,ZHOOK_HANDLE)
!
!*       1 Initialisations
!        -----------------------------------------------
!
ZSPIN = REAL(IO%NSPINS)
!
ZDT = ZSPIN*PTSTEP
!
ZDT_LIT1 = ZDT/XTAU_LITTER(1)
ZDT_LIT2 = ZDT/XTAU_LITTER(2)
!
!*       1.1 dimensions
!
INI = SIZE(PEK%XSOILDIF_CARB,1)
INL = SIZE(PEK%XSOILDIF_CARB,2)
!
!*       1.2 set output to zero
!
PFOXIC_LITTER (:,:,:) = 0.0
PFMG_LITTER   (:,:,:) = 0.0
PFLEACH_LITTER(:,:,:) = 0.0
PFOXIC_SOC    (:,:,:) = 0.0
PFLEACH_SOC   (:,:,:) = 0.0
PFMG_SOC      (:,:  ) = 0.0
!
DMK%XTSOILPOOL(:,:  ) = 0.0
!
ZFLUXTOT  (:,:) = 0.0
ZSOILCARB (:,:) = 0.0
!
!-------------------------------------------------------------------------------
!
!*       2. fluxes out of soil litter pools
!        ----------------------------------
!
!*       2.1 Oxic fluxes
!
ZCONTROL(:,:) = (1.-PCONTROL_LEACH_SOIL(:,:))*PCONTROL_TEMP(:,:)*PCONTROL_MOIST(:,:)
!
!* fluxes out of metebolic litter pool
!
ZLITT_FD(:,:) = ZDT_LIT1*ZCONTROL(:,:)
!
PFOXIC_LITTER(:,:,1) = PEK%XSOILDIF_LITTER(:,:,1)*ZLITT_FD(:,:)
!
!* fluxes out of structural litter pool
!
ZLITT_FD(:,:)=ZDT_LIT2*ZCONTROL(:,:)*EXP(-3.0*PEK%XSOILDIF_LIGNIN_STRUC(:,:))  
!
PFOXIC_LITTER(:,:,2) = PEK%XSOILDIF_LITTER(:,:,2)*ZLITT_FD(:,:)
!
!*       2.2 methanogenisis fluxes
!
IF(IO%LSOILGAS)THEN
!
  ZCONTROL(:,:) = (1.-PCONTROL_LEACH_SOIL(:,:))*PCONTROL_MG(:,:)*PCONTROL_TEMP_MG(:,:)
!
!* fluxes out of metebolic litter pool
!
  ZLITT_FD(:,:) = (ZDT_LIT1/XRATIO_MG)*ZCONTROL(:,:)
!
  PFMG_LITTER(:,:,1) = PEK%XSOILDIF_LITTER(:,:,1)*ZLITT_FD(:,:)
!
!* fluxes out of structural litter pool
!
  ZLITT_FD(:,:)=(ZDT_LIT2/XRATIO_MG)*ZCONTROL(:,:)*EXP(-3.0*PEK%XSOILDIF_LIGNIN_STRUC(:,:))  
!
  PFMG_LITTER(:,:,2) = PEK%XSOILDIF_LITTER(:,:,2)*ZLITT_FD(:,:)
!
ENDIF
!
!-------------------------------------------------------------------------------
!
!*       3. fluxes out of soil carbon pools
!        ----------------------------------
!
!*       3.1 soil property dependance (1.0-0.75*(1.0-KK%XSAND))
!
ZSOILP_EFFECT(:,:)=0.25+0.75*KK%XSAND(:,:)
!
!*       3.2 Oxic fluxes
!
!soil temperature and moisture dependance
ZCONTROL(:,:) = (1.0-PCONTROL_LEACH_SOIL(:,:))*PCONTROL_MOIST(:,:)*PCONTROL_TEMP(:,:)
!
!Compute Effective Turn-over frequency by layer (s-1)
ZTSOILPOOL(:,:,1) = ZSOILP_EFFECT(:,:)*ZCONTROL(:,:)/XTAU_SOILCARB(1)
ZTSOILPOOL(:,:,2) =                    ZCONTROL(:,:)/XTAU_SOILCARB(2)
ZTSOILPOOL(:,:,3) =                    ZCONTROL(:,:)/XTAU_SOILCARB(3)
!
!determine flux out of pool by layer
WHERE(PEK%XSOILDIF_CARB(:,:,:)/=XUNDEF)
      PFOXIC_SOC(:,:,:) = ZDT * ZTSOILPOOL(:,:,:) * PEK%XSOILDIF_CARB(:,:,:)
ENDWHERE
!
!*       3.3 Methanogenesis fluxes
!
IF(IO%LSOILGAS)THEN
!
!soil temperature, moisture and O2 dependance
  ZCONTROL(:,:) = (1.0-PCONTROL_LEACH_SOIL(:,:))*PCONTROL_MG(:,:)*PCONTROL_TEMP_MG(:,:)
!
!Compute Effective Turn-over frequency by layer (s-1)
  ZTSOILPOOL(:,:,1) = ZSOILP_EFFECT(:,:)*ZCONTROL(:,:)/(XTAU_SOILCARB(1)*XRATIO_MG)
!
!determine flux out of pool by layer
  WHERE(PEK%XSOILDIF_CARB(:,:,1)/=XUNDEF)
        PFMG_SOC(:,:) = ZDT * ZTSOILPOOL(:,:,1) * PEK%XSOILDIF_CARB(:,:,1)
  ENDWHERE
!
ENDIF
!
!*       3.4 diag of soil turn-over frequency (s-1)
!
DO JL=1,INL
   DO JI=1,INI
      IDEPTH=PK%NWG_LAYER(JI)
      IF(JL<=IDEPTH)THEN
        ZFLUXTOT (JI,1)=ZFLUXTOT (JI,1)+PFOXIC_SOC   (JI,JL,1)+PFMG_SOC(JI,JL)
        ZFLUXTOT (JI,2)=ZFLUXTOT (JI,2)+PFOXIC_SOC   (JI,JL,2)
        ZFLUXTOT (JI,3)=ZFLUXTOT (JI,3)+PFOXIC_SOC   (JI,JL,3)
        ZSOILCARB(JI,1)=ZSOILCARB(JI,1)+PEK%XSOILDIF_CARB(JI,JL,1)
        ZSOILCARB(JI,2)=ZSOILCARB(JI,2)+PEK%XSOILDIF_CARB(JI,JL,2)
        ZSOILCARB(JI,3)=ZSOILCARB(JI,3)+PEK%XSOILDIF_CARB(JI,JL,3)
      ENDIF
   ENDDO
ENDDO
!
WHERE(ZSOILCARB(:,:)>0.0)
      DMK%XTSOILPOOL(:,:) = ZFLUXTOT(:,:)/(ZDT*ZSOILCARB(:,:))
ENDWHERE
!
!-------------------------------------------------------------------------------
!
!
!*    4. Oxic decomposition limitation due to soil O2 concentration
!        ----------------------------------------------------------
!
IF(IO%LSOILGAS)THEN
!
  ZSUM_FOXIC (:,:) = 0.0
  ZDCMAX_OXIC(:,:) = 1.0
!
  WHERE(PEK%XSGASO2(:,:)/=XUNDEF)
        ZSUM_FOXIC (:,:) = PFOXIC_LITTER(:,:,1)+PFOXIC_LITTER(:,:,2)+PFOXIC_SOC(:,:,1)+PFOXIC_SOC(:,:,2)+PFOXIC_SOC(:,:,3)
        ZDCMAX_OXIC(:,:) = MAX(0.0,PEK%XSGASO2(:,:)*PK%XDZG(:,:)*PPO2(:,:)*(XMC/XMO2))
  ENDWHERE  
!
  WHERE(ZSUM_FOXIC(:,:)>0.0)
        ZDCMAX_OXIC(:,:) = MIN(1.0,ZDCMAX_OXIC(:,:)/ZSUM_FOXIC(:,:))
  ENDWHERE  
!
  PFOXIC_LITTER(:,:,1)=PFOXIC_LITTER(:,:,1)*ZDCMAX_OXIC(:,:)
  PFOXIC_LITTER(:,:,2)=PFOXIC_LITTER(:,:,2)*ZDCMAX_OXIC(:,:)
!
  PFOXIC_SOC(:,:,1)=PFOXIC_SOC(:,:,1)*ZDCMAX_OXIC(:,:)
  PFOXIC_SOC(:,:,2)=PFOXIC_SOC(:,:,2)*ZDCMAX_OXIC(:,:)
  PFOXIC_SOC(:,:,3)=PFOXIC_SOC(:,:,3)*ZDCMAX_OXIC(:,:)
!
ENDIF
!
!-------------------------------------------------------------------------------
!
!
!*    5. Dissolved organic carbon
!        ------------------------
!
IF(IO%LCLEACH)THEN
!
! Leaching/Mobilisation Soil Litter (gC m-2)
!
  WHERE(PEK%XSOILDIF_LITTER(:,:,1)/=XUNDEF)
        PFLEACH_LITTER(:,:,1) = ZDT_LIT1*PCONTROL_LEACH_SOIL(:,:)*PCONTROL_TEMP(:,:)*PCONTROL_MOIST(:,:)*PEK%XSOILDIF_LITTER(:,:,1)
  ENDWHERE
!
  WHERE(PEK%XSOILDIF_LITTER(:,:,2)/=XUNDEF)
        PFLEACH_LITTER(:,:,2) = ZDT_LIT2*PCONTROL_LEACH_SOIL(:,:)*PCONTROL_TEMP(:,:)*PCONTROL_MOIST(:,:)*PEK%XSOILDIF_LITTER(:,:,2)
  ENDWHERE
!
  PFLEACH_LITTER(:,:,:) = MIN(PFLEACH_LITTER(:,:,:),PEK%XSOILDIF_LITTER(:,:,:))
!
! Leaching/Mobilisation Soil carbon (gC m-2)
!
  ZCONTROL(:,:) = ZDT*PCONTROL_LEACH_SOIL(:,:)*PCONTROL_TEMP(:,:)*PCONTROL_MOIST(:,:)
!
  WHERE(PEK%XSOILDIF_CARB(:,:,1)/=XUNDEF)
        PFLEACH_SOC(:,:,1) = ZSOILP_EFFECT(:,:)*ZCONTROL(:,:)*PEK%XSOILDIF_CARB(:,:,1)/XTAU_SOILCARB(1)
  ENDWHERE
!
  WHERE(PEK%XSOILDIF_CARB(:,:,2)/=XUNDEF)
        PFLEACH_SOC(:,:,2) = ZCONTROL(:,:)*PEK%XSOILDIF_CARB(:,:,2)/XTAU_SOILCARB(2)
  ENDWHERE
!
  WHERE(PEK%XSOILDIF_CARB(:,:,3)/=XUNDEF)
        PFLEACH_SOC(:,:,3) = ZCONTROL(:,:)*PEK%XSOILDIF_CARB(:,:,3)/XTAU_SOILCARB(3)
  ENDWHERE
!
  PFLEACH_SOC(:,:,:) = MIN(PFLEACH_SOC(:,:,:),PEK%XSOILDIF_CARB(:,:,:))
!
ENDIF
!
!-------------------------------------------------------------------------------

IF (LHOOK) CALL DR_HOOK('CARBON_SOILDIF_FLUXES',1,ZHOOK_HANDLE)
!
END SUBROUTINE CARBON_SOILDIF_FLUXES
