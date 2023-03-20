!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
    SUBROUTINE CARBON_EVOL(IO, KK, PK, PEK, DEK, DMK, PTSTEP, PRESP_BIOMASS_INST)                          
!   ###############################################################
!!****  *CARBON EVOL*
!!
!!    PURPOSE
!!    -------
!!
!!    Diagnoses respiration carbon fluxes and performs the time evolution of 
!!    carbon pools in the case of 'CNT' option (ISBA-CC) 
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
!!      Gibelin et al. 2008, AFM
!!        Modelling energy and CO2 fluxes with an interactive vegetation land surface model -
!!        Evaluation at high and middle latitudes.
!!      
!!    AUTHOR
!!    ------
!!
!!      A.L. Gibelin       * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    22/06/09
!!      S.QUEGUINER 09/2011 Cas 'DEF'- condition si LAI=UNDEF->ZRESP_SOIL_TOT=0
!!      C.   Delire 04/2012 : spinup soil carbon
!!      B. Decharme 05/2012 : Optimization and ISBA-DIF coupling
!!      C. Delire   08/2016 : Add turnover diagnostics
!!      R. Séférian 10/2016 : add module handling interaction between carbon cycle and hydrology
!!      B. Decharme 10/2018 : Ecosystem respiration from Norman et al 1992 noted N92 (old DEF)
!!      B. Decharme 11/2020 : Surface / Soil litter split
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
USE MODD_CO2V_PAR,       ONLY : XMC, XMCO2, XPCCO2, XALPHA_DOC, XGTOKG
USE MODD_CSTS,           ONLY : XTT
USE MODD_SURF_PAR,       ONLY : XUNDEF
!
USE MODI_CONTROL_MOIST_FUNC
USE MODI_CONTROL_TEMP_FUNC
USE MODI_CARBON_LITTER_SURFACE
USE MODI_CARBON_LITTER_SOIL
USE MODI_CARBON_SOIL
USE MODI_CARBON_LEACHING

!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
TYPE(ISBA_OPTIONS_t),   INTENT(INOUT) :: IO
TYPE(ISBA_K_t),         INTENT(INOUT) :: KK
TYPE(ISBA_P_t),         INTENT(INOUT) :: PK
TYPE(ISBA_PE_t),        INTENT(INOUT) :: PEK
TYPE(DIAG_EVAP_ISBA_t), INTENT(INOUT) :: DEK
TYPE(DIAG_MISC_ISBA_t), INTENT(INOUT) :: DMK
!
!
REAL,                 INTENT(IN)      :: PTSTEP             ! time step
REAL, DIMENSION(:,:), INTENT(IN)      :: PRESP_BIOMASS_INST ! instantaneous respiration of biomass (kgCO2/m2/s)
!
!
!*      0.2    declarations of local variables
!  
REAL, PARAMETER                  :: ZCOEF1 = 4.4E-8
REAL, PARAMETER                  :: ZCOEF2 = 13.5
REAL, PARAMETER                  :: ZCOEF3 = 5.4
REAL, PARAMETER                  :: ZCOEF4 = 0.1
REAL, PARAMETER                  :: ZCOEF5 = 25.0
!
REAL, PARAMETER                  :: ZASAND = 0.25
REAL, PARAMETER                  :: ZBSAND = 0.75
!
REAL, PARAMETER                  :: ZDTOP  = 0.1   !Top depth m
REAL, PARAMETER                  :: ZDSUB  = 1.0   !Sub depth m
!
REAL, DIMENSION(SIZE(PEK%XTG,1))     :: ZRESP_SOIL_TOT     ! total soil respiration (kgCO2/m2/s)
REAL, DIMENSION(SIZE(PEK%XTG,1))     :: ZRESP_AUTO_ABOVE   ! total above ground biomass respiration (kgCO2/m2/s)
REAL, DIMENSION(SIZE(PEK%XTG,1))     :: ZRESP_HETERO       ! total heterotrophic respiration (kgCO2/m2/s)
!
!
REAL, DIMENSION(SIZE(PEK%XSOILCARB,1),SIZE(PEK%XSOILCARB,2)) :: ZSOILCARBON_INPUT
!                  quantity of carbon going into carbon pools 
!                  from litter decomposition (gC/m2/s)
!
REAL, DIMENSION(SIZE(PEK%XSOILCARB,1)) :: ZRESP_HETERO_LITTER
!                  litter heterotrophic respiration (gC/m2/s)
REAL, DIMENSION(SIZE(PEK%XSOILCARB,1)) :: ZRESP_HETERO_SOIL   
!                  soil heterotrophic respiration (gC/m2/s)
!
REAL, DIMENSION(SIZE(PEK%XLIGNIN_STRUC,1),SIZE(PEK%XLIGNIN_STRUC,2)) :: ZCONTROL_MOIST, &
                                                                        ZCONTROL_TEMP , &
                                                                        ZCONTROL_LEACH
!                  ZCONTROL_MOIST = moisture control of heterotrophic respiration
!                  ZCONTROL_TEMP  = temperature control of heterotrophic respiration
!                  ZCONTROL_LEACH = water flow control of doc leaching
REAL, DIMENSION(SIZE(PEK%XTG,1))              :: ZCONTROL_SAND   ! 
!                  ZCONTROL_SAND = control in soil texture (linear function of sand content) 
!
REAL, DIMENSION(SIZE(PEK%XTG,1))              :: ZTG_TOP     ! Top soil temperature   (C)
REAL, DIMENSION(SIZE(PEK%XTG,1))              :: ZTG_SUB     ! Sub soil temperature   (C)
REAL, DIMENSION(SIZE(PEK%XTG,1))              :: ZSAND_SUB   ! Sub soil sand fraction (-)
!
REAL, DIMENSION(SIZE(PEK%XTG,1))              :: ZMOIST_TOP  ! Top soil moisture index (-)
REAL, DIMENSION(SIZE(PEK%XTG,1))              :: ZMOIST_SUB  ! Sub soil moisture index (-)
REAL, DIMENSION(SIZE(PEK%XTG,1))              :: ZSAT_TOP    ! Top soil saturated index(-)
REAL, DIMENSION(SIZE(PEK%XTG,1))              :: ZSAT_SUB    ! Sub soil saturated index(-)
!
REAL, DIMENSION(SIZE(PEK%XTG,1))              :: ZMOIST_TOP_WRK  ! Top soil moisture index (-)
REAL, DIMENSION(SIZE(PEK%XTG,1))              :: ZMOIST_SUB_WRK  ! Sub soil moisture index (-)
REAL, DIMENSION(SIZE(PEK%XTG,1))              :: ZSAT_TOP_WRK    ! Top soil saturated index(-)
REAL, DIMENSION(SIZE(PEK%XTG,1))              :: ZSAT_SUB_WRK    ! Sub soil saturated index(-)
!
REAL, DIMENSION(SIZE(PEK%XTG,1))              :: ZDG_TOP     ! Top soil depth for DIF (m)
REAL, DIMENSION(SIZE(PEK%XTG,1))              :: ZDG_SUB     ! Sub soil depth for DIF (m)
!
REAL, DIMENSION(SIZE(PEK%XTG,1),SIZE(PEK%XTG,2))  :: ZWGHT_TOP   ! Weight top for DIF (m)    
REAL, DIMENSION(SIZE(PEK%XTG,1),SIZE(PEK%XTG,2))  :: ZWGHT_SUB   ! Weight sub for DIF (m)  
!
REAL, DIMENSION(SIZE(PEK%XTG,1))              :: ZWORK  ! work array
!
REAL, DIMENSION(SIZE(PEK%XTG,1))              :: ZFDOC_LITTER_SURF, ZFDOC_SOIL
!
REAL     :: ZLOG2
!
INTEGER  :: JNBIOMASS
!
INTEGER  :: INI, INL, JI, JL, IDEPTH, INBIOMASS, INSOILCARB
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!*      1.     Preliminaries
!              -------------
!
IF (LHOOK) CALL DR_HOOK('CARBON_EVOL',0,ZHOOK_HANDLE)
!
DEK%XRESP_AUTO = 0.0
DEK%XRESP_ECO  = 0.0
DEK%XTURNVTOT  = 0.0
DEK%XFLTOSCARB = 0.0
DEK%XRESPSCARB = 0.0
DEK%XRESPLIT   = 0.0
!
INI=SIZE(PEK%XTG,1)
INL=SIZE(PEK%XTG,2)
!
INSOILCARB=SIZE(PEK%XSOILCARB,2)
INBIOMASS=SIZE(PRESP_BIOMASS_INST,2)
!
ZRESP_SOIL_TOT(:)      = XUNDEF
ZRESP_AUTO_ABOVE(:)    = XUNDEF
ZRESP_HETERO(:)        = XUNDEF
ZSOILCARBON_INPUT(:,:) = XUNDEF
ZRESP_HETERO_LITTER(:) = XUNDEF
ZRESP_HETERO_SOIL(:)   = XUNDEF
ZCONTROL_MOIST(:,:)    = XUNDEF
ZCONTROL_TEMP(:,:)     = XUNDEF
ZCONTROL_SAND(:)       = XUNDEF
ZCONTROL_LEACH(:,:)    = XUNDEF
ZWGHT_TOP(:,:)         = XUNDEF
ZWGHT_SUB(:,:)         = XUNDEF
!
ZTG_TOP   (:) = 0.0
ZTG_SUB   (:) = 0.0
!
ZLOG2  = LOG(2.0)
!
ZFDOC_LITTER_SURF(:) = 0.0
ZFDOC_SOIL       (:) = 0.0
!
! convert soil temperature from K to C (over 1m depth for DIF)
!
IF(IO%CISBA/='DIF')THEN        
  ZTG_TOP   (:) = PEK%XTG(:,1)-XTT  
  ZTG_SUB   (:) = PEK%XTG(:,2)-XTT  
ELSE       
  DO JI=1,INI
     IDEPTH=PK%NWG_LAYER(JI)
     ZDG_TOP(JI)=MIN(ZDTOP,PK%XDG(JI,IDEPTH))
     ZDG_SUB(JI)=MIN(ZDSUB,PK%XDG(JI,IDEPTH))
  ENDDO  
  DO JL=1,INL
     DO JI=1,INI     
        ZWGHT_TOP(JI,JL)=MIN(PK%XDZG(JI,JL),MAX(0.0,ZDG_TOP(JI)-PK%XDG(JI,JL)+PK%XDZG(JI,JL)))/ZDG_TOP(JI)
        ZWGHT_SUB(JI,JL)=MIN(PK%XDZG(JI,JL),MAX(0.0,ZDG_SUB(JI)-PK%XDG(JI,JL)+PK%XDZG(JI,JL)))/ZDG_SUB(JI)        
        ZTG_TOP(JI)=ZTG_TOP(JI)+(PEK%XTG(JI,JL)-XTT)*ZWGHT_TOP(JI,JL)
        ZTG_SUB(JI)=ZTG_SUB(JI)+(PEK%XTG(JI,JL)-XTT)*ZWGHT_SUB(JI,JL)
     ENDDO
  ENDDO 
ENDIF
!
!
!*      2.     Autotrophic respiration
!              -----------------------
!
DO JNBIOMASS=1,INBIOMASS
   DEK%XRESP_AUTO(:) = DEK%XRESP_AUTO (:) + PRESP_BIOMASS_INST(:,JNBIOMASS)
ENDDO
!
!
!*      3.     Soil and Ecosystem respiration
!              ------------------------------
!
IF (IO%CRESPSL == 'DEF') THEN
!
! No soil respiration, so ecosystem respiration equals autotrophic respiration
!
DEK%XRESP_ECO(:) = DEK%XRESP_AUTO(:)
!
ELSEIF (IO%CRESPSL == 'N92') THEN
!
! Ecosystem respiration from Norman et al 1992 but only valid locally
!
  ZWORK(:)=0.0   
  IF(IO%CISBA/='DIF')THEN            
     ZWORK (:) = PEK%XWG (:,1)  
  ELSE
     DO JL=1,INL
        DO JI=1,INI        
           ZWORK(JI)=ZWORK(JI)+PEK%XWG(JI,JL)*ZWGHT_TOP(JI,JL)
        ENDDO
     ENDDO  
 ENDIF
!
! Autotrophic respiration of vegetation above soil
!
  ZRESP_AUTO_ABOVE(:)=0.
  IF (IO%CPHOTO=='NIT') THEN       
     DO JNBIOMASS=1,2
        ZRESP_AUTO_ABOVE(:) = ZRESP_AUTO_ABOVE(:) + PRESP_BIOMASS_INST(:,JNBIOMASS)
     ENDDO  
  ELSE IF (IO%CPHOTO=='NCB') THEN       
     DO JNBIOMASS=1,3
        ZRESP_AUTO_ABOVE(:) = ZRESP_AUTO_ABOVE(:) + PRESP_BIOMASS_INST(:,JNBIOMASS)
     ENDDO  
  ELSE IF (IO%CPHOTO=='AGS' .OR. IO%CPHOTO=='AST' .OR. IO%CPHOTO=='LAI' .OR. IO%CPHOTO=='LST') THEN       
        ZRESP_AUTO_ABOVE(:) = PRESP_BIOMASS_INST(:,1) 
  ENDIF
!
! Soil respiration from Norman et al 1992 (kgCO2/m2/s) including roots
!
  WHERE (PEK%XLAI(:) == XUNDEF)
    ZRESP_SOIL_TOT(:) = 0.0
  ELSEWHERE
! Before optimization = (ZCOEF1/PRHOA)*(ZCOEF2+ZCOEF3*PEK%XLAI(:))*PEK%XWG(:,1)*2.**(ZCOEF4*(ZT2(:)-ZCOEF5))        
    ZRESP_SOIL_TOT(:) = ZCOEF1*(ZCOEF2+ZCOEF3*PEK%XLAI(:))*ZWORK(:)*EXP(ZLOG2*(ZCOEF4*(ZTG_SUB(:)-ZCOEF5)))
  ENDWHERE
!
! RESP_ECO is diagnosed from RESP_SOIL_TOT and RESP_AUTO_ABOVE
!
  DEK%XRESP_ECO(:) = ZRESP_SOIL_TOT(:) + ZRESP_AUTO_ABOVE(:)
!  
ELSE IF (IO%CRESPSL == 'PRM') THEN
!
   ZWORK(:)=0.0
!   
   IF(IO%CISBA/='DIF')THEN            
      ZWORK (:) = PEK%XWG (:,1)  
   ELSE
      DO JL=1,INL
         DO JI=1,INI        
            ZWORK(JI)=ZWORK(JI)+MIN(1.0,PEK%XWG(JI,JL)/KK%XWFC(JI,JL))*ZWGHT_TOP(JI,JL)
         ENDDO
      ENDDO  
   ENDIF
!
! Ecosystem respiration : Q10 model following Albergel et al. 2009 for SMOSREX (kgCO2/m2/s)
! RESP_ECO is directly calculated by the parameterization
!
! Before optimization 
! DEK%XRESP_ECO(:) = PK%XRE25(:)/PRHOA(:) * MIN(PEK%XWG(:,1)/KK%XWFC(:,1),1.)*2.**(ZCOEF4*(ZT2(:)-ZCOEF5))        
  DEK%XRESP_ECO(:) = PK%XRE25(:) * ZWORK(:) * EXP(ZLOG2*(ZCOEF4*(ZTG_SUB(:)-ZCOEF5)))
!
ELSE IF (IO%CRESPSL == 'CNT') THEN
!
! Heterotrophic respiration following CENTURY model from Gibelin et al. 2008
!
  ZMOIST_TOP(:)=0.0
  ZSAT_TOP  (:)=0.0
  ZMOIST_SUB(:)=0.0
  ZSAT_SUB  (:)=0.0
  ZSAND_SUB (:)=0.0
!
  IF(IO%CISBA/='DIF')THEN
!          
    ZMOIST_TOP(:) = MIN(1.0,MAX(0.0,(PEK%XWG(:,1)-KK%XWWILT(:,1))/(KK%XWFC (:,1)-KK%XWWILT(:,1))))
    ZSAT_TOP  (:) = MIN(1.0,MAX(0.0,(PEK%XWG(:,1)-KK%XWFC  (:,1))/(KK%XWSAT(:,1)-KK%XWFC  (:,1))))
    ZMOIST_SUB(:) = MIN(1.0,MAX(0.0,(PEK%XWG(:,2)-KK%XWWILT(:,2))/(KK%XWFC (:,2)-KK%XWWILT(:,2))))
    ZSAT_SUB  (:) = MIN(1.0,MAX(0.0,(PEK%XWG(:,2)-KK%XWFC  (:,2))/(KK%XWSAT(:,2)-KK%XWFC  (:,2))))
!    
    ZSAND_SUB (:) = KK%XSAND (:,2)
!    
  ELSE
! 
    ZMOIST_TOP_WRK(:)=0.0
    ZSAT_TOP_WRK  (:)=0.0
    ZMOIST_SUB_WRK(:)=0.0
    ZSAT_SUB_WRK  (:)=0.0
!
    DO JL=1,INL
       DO JI=1,INI
! 
          ZMOIST_TOP(JI)=ZMOIST_TOP(JI)+(PEK%XWG(JI,JL)-KK%XWWILT(JI,JL))*ZWGHT_TOP(JI,JL)
          ZSAT_TOP  (JI)=ZSAT_TOP  (JI)+(PEK%XWG(JI,JL)-KK%XWFC  (JI,JL))*ZWGHT_TOP(JI,JL)
!
          ZMOIST_TOP_WRK(JI)=ZMOIST_TOP_WRK(JI)+(KK%XWFC (JI,JL)-KK%XWWILT(JI,JL))*ZWGHT_TOP(JI,JL)
          ZSAT_TOP_WRK  (JI)=ZSAT_TOP_WRK  (JI)+(KK%XWSAT(JI,JL)-KK%XWFC  (JI,JL))*ZWGHT_TOP(JI,JL)
!
          ZMOIST_SUB(JI)=ZMOIST_SUB(JI)+(PEK%XWG(JI,JL)-KK%XWWILT(JI,JL))*ZWGHT_SUB(JI,JL)
          ZSAT_SUB  (JI)=ZSAT_SUB  (JI)+(PEK%XWG(JI,JL)-KK%XWFC  (JI,JL))*ZWGHT_SUB(JI,JL)          
!
          ZMOIST_SUB_WRK(JI)=ZMOIST_SUB_WRK(JI)+(KK%XWFC (JI,JL)-KK%XWWILT(JI,JL))*ZWGHT_SUB(JI,JL)
          ZSAT_SUB_WRK  (JI)=ZSAT_SUB_WRK  (JI)+(KK%XWSAT(JI,JL)-KK%XWFC  (JI,JL))*ZWGHT_SUB(JI,JL)
!
          ZSAND_SUB(JI)=ZSAND_SUB(JI)+KK%XSAND(JI,JL)*ZWGHT_SUB(JI,JL)
!          
       ENDDO
    ENDDO 
!  
    WHERE(ZMOIST_TOP_WRK(:)>0.0)ZMOIST_TOP(:) = MIN(1.0,MAX(0.0,ZMOIST_TOP(:)/ZMOIST_TOP_WRK(:)))
    WHERE(ZMOIST_SUB_WRK(:)>0.0)ZMOIST_SUB(:) = MIN(1.0,MAX(0.0,ZMOIST_SUB(:)/ZMOIST_SUB_WRK(:)))
!
    WHERE(ZSAT_TOP_WRK(:)>0.0)ZSAT_TOP(:) = MIN(1.0,MAX(0.0,ZSAT_TOP(:)/ZSAT_TOP_WRK(:)))
    WHERE(ZSAT_SUB_WRK(:)>0.0)ZSAT_SUB(:) = MIN(1.0,MAX(0.0,ZSAT_SUB(:)/ZSAT_SUB_WRK(:)))
!
  ENDIF
!
! Total vegetation turnover (kgC/m2/s)
!
  DO JL=1,INBIOMASS
     DO JI=1,INI
        DEK%XTURNVTOT(JI) = DEK%XTURNVTOT(JI) + PK%XTURNOVER(JI,JL) * XGTOKG
     ENDDO
  ENDDO
!
  ZCONTROL_TEMP (:,1) = CONTROL_TEMP_FUNC (ZTG_TOP(:))
  ZCONTROL_TEMP (:,2) = CONTROL_TEMP_FUNC (ZTG_SUB(:))
  ZCONTROL_MOIST(:,1) = CONTROL_MOIST_FUNC(ZMOIST_TOP(:),ZSAT_TOP(:))
  ZCONTROL_MOIST(:,2) = CONTROL_MOIST_FUNC(ZMOIST_SUB(:),ZSAT_SUB(:))
  ZCONTROL_LEACH(:,1) = XALPHA_DOC * MAX(KK%XFSAT(:),KK%XFFLOOD(:)) 
  ZCONTROL_LEACH(:,2) = XALPHA_DOC * KK%XFSAT(:)
  ZCONTROL_SAND (:)   = ZASAND+ZBSAND*ZSAND_SUB
!
  CALL CARBON_LITTER_SURFACE (PTSTEP, IO%LCLEACH, PK%XTURNOVER, PEK%XLITTER(:,:,1), PEK%XLIGNIN_STRUC(:,1), &
                              ZCONTROL_TEMP(:,1), ZCONTROL_MOIST(:,1), ZCONTROL_LEACH(:,1),                 &
                              ZRESP_HETERO_LITTER, ZSOILCARBON_INPUT, ZFDOC_LITTER_SURF                     )
!
!
  CALL CARBON_LITTER_SOIL(PTSTEP, IO%NSPINS, PK%XTURNOVER, PEK%XLITTER(:,:,2), PEK%XLIGNIN_STRUC(:,2), &
                          ZCONTROL_TEMP(:,2), ZCONTROL_MOIST(:,2), ZCONTROL_LEACH(:,2),                &
                          ZRESP_HETERO_LITTER, ZSOILCARBON_INPUT                                       )  
!  
  CALL CARBON_SOIL (PTSTEP, IO%NSPINS, ZSAND_SUB,ZSOILCARBON_INPUT,&
                    ZCONTROL_TEMP,ZCONTROL_MOIST,ZCONTROL_LEACH,   &
                    PEK%XSOILCARB,ZRESP_HETERO_SOIL,DMK%XTSOILPOOL )  
!
  IF(IO%LCLEACH)THEN
    CALL CARBON_LEACHING(PTSTEP, IO%NSPINS,    & ! time step
                         ZCONTROL_TEMP (:,2),  & ! control inn soil temperature  
                         ZCONTROL_MOIST(:,2),  & ! control in soil moisture  
                         ZCONTROL_SAND (:  ),  & ! control in soil soil texture
                         ZCONTROL_LEACH(:,2),  & ! transfer fonction
                         PEK%XLITTER(:,:,2),   & ! soil litter at t
                         PEK%XSOILCARB,        & ! soil carbon at t
                         ZFDOC_SOIL            ) ! doc mass (g/m2/s)
  ENDIF
!
! Total soil carbon input from litter (kgCO2/m2/s)
!
  DO JL=1,INSOILCARB
     DO JI=1,INI
        DEK%XFLTOSCARB(JI) = DEK%XFLTOSCARB(JI) + ZSOILCARBON_INPUT(JI,JL) * XGTOKG
     ENDDO
  ENDDO
!
! Transform units : gC/m2/s -> kgCO2/m2/s
!
  ZRESP_HETERO(:) = (ZRESP_HETERO_LITTER(:) + ZRESP_HETERO_SOIL(:)) * (XGTOKG*XMCO2/XMC)
!  
! RESP_ECO is diagnosed from RESP_HETERO and RESP_AUTO : gC/m2/s -> kgCO2/m2/s
!
  DEK%XRESP_ECO (:)= ZRESP_HETERO(:) + DEK%XRESP_AUTO(:)
!
  DEK%XRESPLIT  (:)= ZRESP_HETERO_LITTER(:) * (XGTOKG*XMCO2/XMC)
  DEK%XRESPSCARB(:)= ZRESP_HETERO_SOIL  (:) * (XGTOKG*XMCO2/XMC)
!
! Dissolved organic carbon (kgC/m2/s)
!
  IF(IO%LCLEACH)THEN
    DEK%XFDOCLIT(:)= ZFDOC_LITTER_SURF(:)                * XGTOKG
    DEK%XFDOC   (:)=(ZFDOC_LITTER_SURF(:)+ZFDOC_SOIL(:)) * XGTOKG
  ENDIF
!
ENDIF
!
IF (LHOOK) CALL DR_HOOK('CARBON_EVOL',1,ZHOOK_HANDLE)
!
!-----------------------------------------------------------------
!
END SUBROUTINE CARBON_EVOL
