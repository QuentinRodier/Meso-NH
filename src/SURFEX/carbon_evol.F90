!     #########
    SUBROUTINE CARBON_EVOL(HISBA, HRESPSL, HPHOTO, PTSTEP, KSPINS,            &
                           PRHOA, PTG, PWG, PWFC, PWWILT, PWSAT, PSAND,       &
                           PDG, PDZG, KWG_LAYER,                              &
                           PRE25, PLAI, PRESP_BIOMASS_INST, PTURNOVER,        &
                           PLITTER, PLIGNIN_STRUC, PSOILCARB,                 &
                           PRESP_AUTO, PRESP_ECO                              )
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
!!	A.L. Gibelin       * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    22/06/09
!!      S.QUEGUINER 09/2011 Cas 'DEF'- condition si LAI=UNDEF->ZRESP_SOIL_TOT=0
!!      C.   Delire 04/2012 : spinup soil carbon
!!      B. Decharme 05/2012 : Optimization and ISBA-DIF coupling
!!
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_CO2V_PAR,       ONLY : XMC, XMCO2, XPCCO2
USE MODD_CSTS,           ONLY : XDAY, XTT
USE MODD_SURF_PAR,       ONLY : XUNDEF
!
USE MODI_CONTROL_MOIST_FUNC
USE MODI_CONTROL_TEMP_FUNC
USE MODI_CARBON_LITTER
USE MODI_CARBON_SOIL

!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
 CHARACTER(LEN=3),INTENT(IN)  :: HISBA                  ! type of ISBA version:
!                                                      ! '2-L' (default)
!                                                      ! '3-L'
!                                                      ! 'DIF'
 CHARACTER(LEN=3), INTENT(IN) :: HRESPSL                ! Soil Respiration
!                                                      ! 'DEF' = Norman 1992
!                                                      ! 'PRM' = Rivalland PhD Thesis (2003)
!                                                      ! 'CNT' = CENTURY model (Gibelin 2008)
 CHARACTER(LEN=3), INTENT(IN) :: HPHOTO                 ! type of photosynthesis
!
REAL, INTENT(IN)             :: PTSTEP                 ! time step
INTEGER, INTENT(IN)          :: KSPINS                 ! (spinup) number of times CARBON_SOIL subroutine
                                                       ! is called for each time step
!
REAL, DIMENSION(:), INTENT(IN)        :: PRHOA         ! air density (kg/m3)
REAL, DIMENSION(:,:), INTENT(IN)      :: PTG           ! soil layer average temperatures (K)
REAL, DIMENSION(:,:), INTENT(IN)      :: PWG           ! soil liquid volumetric water content (m3/m3)
REAL, DIMENSION(:,:), INTENT(IN)      :: PWFC          ! field capacity profile (m3/m3)
REAL, DIMENSION(:,:), INTENT(IN)      :: PWWILT        ! wilting point profile (m3/m3)
REAL, DIMENSION(:,:), INTENT(IN)      :: PWSAT         ! porosity profile (m3/m3)
REAL, DIMENSION(:,:), INTENT(IN)      :: PSAND         ! profile of sand fraction
REAL, DIMENSION(:,:), INTENT(IN)      :: PDG           ! Depth of Bottom of Soil layers       (m)
REAL, DIMENSION(:,:), INTENT(IN)      :: PDZG          ! soil layers thicknesses (DIF option) (m)
INTEGER, DIMENSION(:),INTENT(IN)      :: KWG_LAYER     ! Number of soil moisture layers (DIF option)
!
REAL,DIMENSION(:), INTENT(IN)         :: PRE25         ! Ecosystem respiration parameter (kg m-2 s-1)
REAL, DIMENSION(:), INTENT(IN)        :: PLAI          ! leaf area index
REAL, DIMENSION(:,:), INTENT(IN)      :: PRESP_BIOMASS_INST ! instantaneous respiration of biomass (kgCO2/kgair m/s)
REAL, DIMENSION(:,:), INTENT(IN)      :: PTURNOVER     ! biomass turnover going into litter (gC m-2 s-1)
REAL, DIMENSION(:,:,:), INTENT(INOUT) :: PLITTER       ! litter pools (gC m-2)
REAL, DIMENSION(:,:), INTENT(INOUT)   :: PLIGNIN_STRUC ! L/C ratio in structural litter
REAL, DIMENSION(:,:), INTENT(INOUT)   :: PSOILCARB     ! soil carbon pools (gC m-2)
REAL, DIMENSION(:), INTENT(OUT)       :: PRESP_AUTO    ! autotrophic respiration (kgCO2/kgair m/s)
REAL, DIMENSION(:), INTENT(OUT)       :: PRESP_ECO     ! total ecosystem respiration (kgCO2/kgair m/s)
!
!*      0.2    declarations of local variables
!  
REAL, PARAMETER                  :: ZCOEF1 = 4.4E-8
REAL, PARAMETER                  :: ZCOEF2 = 13.5
REAL, PARAMETER                  :: ZCOEF3 = 5.4
REAL, PARAMETER                  :: ZCOEF4 = 0.1
REAL, PARAMETER                  :: ZCOEF5 = 25.0
!
REAL, PARAMETER                  :: ZDTOP  = 0.1   !Top depth m
REAL, PARAMETER                  :: ZDSUB  = 1.0   !Sub depth m
!
REAL, DIMENSION(SIZE(PTG,1))     :: ZRESP_SOIL_TOT     ! total soil respiration (kgCO2/kgair m/s)
REAL, DIMENSION(SIZE(PTG,1))     :: ZRESP_AUTO_ABOVE   ! total above ground biomass respiration (kgCO2/kgair m/s)
REAL, DIMENSION(SIZE(PTG,1))     :: ZRESP_HETERO       ! total heterotrophic respiration (kgCO2/kgair m/s)
!
!
REAL, DIMENSION(SIZE(PSOILCARB,1),SIZE(PSOILCARB,2)) :: ZSOILCARBON_INPUT
!                  quantity of carbon going into carbon pools 
!                  from litter decomposition (gC/m2/day)
!
REAL, DIMENSION(SIZE(PSOILCARB,1)) :: ZRESP_HETERO_DAY_LITTER 
!                  litter heterotrophic respiration (gC/m2/day)
REAL, DIMENSION(SIZE(PSOILCARB,1)) :: ZRESP_HETERO_DAY_SOIL   
!                  soil heterotrophic respiration (gC/m2/day)
!
REAL, DIMENSION(SIZE(PLIGNIN_STRUC,1),SIZE(PLIGNIN_STRUC,2)) :: ZCONTROL_MOIST, &
                                                                ZCONTROL_TEMP
!                  ZCONTROL_MOIST = moisture control of heterotrophic respiration
!                  ZCONTROL_TEMP = temperature control of heterotrophic respiration
!
REAL, DIMENSION(SIZE(PTG,1))              :: ZTG_TOP     ! Top soil temperature   (C)
REAL, DIMENSION(SIZE(PTG,1))              :: ZTG_SUB     ! Sub soil temperature   (C)
REAL, DIMENSION(SIZE(PTG,1))              :: ZSAND_SUB   ! Sub soil sand fraction (-)
!
REAL, DIMENSION(SIZE(PTG,1))              :: ZMOIST_TOP  ! Top soil moisture index (-)
REAL, DIMENSION(SIZE(PTG,1))              :: ZMOIST_SUB  ! Sub soil moisture index (-)
REAL, DIMENSION(SIZE(PTG,1))              :: ZSAT_TOP    ! Top soil saturated index(-)
REAL, DIMENSION(SIZE(PTG,1))              :: ZSAT_SUB    ! Sub soil saturated index(-)
!
REAL, DIMENSION(SIZE(PTG,1))              :: ZDG_TOP     ! Top soil depth for DIF (m)
REAL, DIMENSION(SIZE(PTG,1))              :: ZDG_SUB     ! Sub soil depth for DIF (m)
!
REAL, DIMENSION(SIZE(PTG,1),SIZE(PTG,2))  :: ZWGHT_TOP   ! Weight top for DIF (m)    
REAL, DIMENSION(SIZE(PTG,1),SIZE(PTG,2))  :: ZWGHT_SUB   ! Weight sub for DIF (m)  
!
REAL, DIMENSION(SIZE(PTG,1))              :: ZWORK  ! work array
!
REAL     :: ZMOISTL, ZSATL, ZLOG2
!
INTEGER  :: JNBIOMASS
INTEGER  :: ITCSPIN
!
INTEGER  :: INI, INL, JI, JL, IDEPTH, INBIOMASS
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
PRESP_AUTO    = 0.0
PRESP_ECO     = 0.0
!
INI=SIZE(PTG,1)
INL=SIZE(PTG,2)
INBIOMASS=SIZE(PRESP_BIOMASS_INST,2)
!
ZRESP_SOIL_TOT(:)          = XUNDEF
ZRESP_AUTO_ABOVE(:)        = XUNDEF
ZRESP_HETERO(:)            = XUNDEF
ZSOILCARBON_INPUT(:,:)     = XUNDEF
ZRESP_HETERO_DAY_LITTER(:) = XUNDEF
ZRESP_HETERO_DAY_SOIL(:)   = XUNDEF
ZCONTROL_MOIST(:,:)        = XUNDEF
ZCONTROL_TEMP(:,:)         = XUNDEF
ZWGHT_TOP(:,:)             = XUNDEF
ZWGHT_SUB(:,:)             = XUNDEF
!
ZTG_TOP   (:) = 0.0
ZTG_SUB   (:) = 0.0
!
ZLOG2  = LOG(2.0)
!
! convert soil temperature from K to C (over 1m depth for DIF)
!
IF(HISBA/='DIF')THEN        
  ZTG_TOP   (:) = PTG(:,1)-XTT  
  ZTG_SUB   (:) = PTG(:,2)-XTT  
ELSE       
  DO JI=1,INI
     IDEPTH=KWG_LAYER(JI)
     ZDG_TOP(JI)=MIN(ZDTOP,PDG(JI,IDEPTH))
     ZDG_SUB(JI)=MIN(ZDSUB,PDG(JI,IDEPTH))
  ENDDO  
  DO JL=1,INL
     DO JI=1,INI     
        ZWGHT_TOP(JI,JL)=MIN(PDZG(JI,JL),MAX(0.0,ZDG_TOP(JI)-PDG(JI,JL)+PDZG(JI,JL)))
        ZWGHT_SUB(JI,JL)=MIN(PDZG(JI,JL),MAX(0.0,ZDG_SUB(JI)-PDG(JI,JL)+PDZG(JI,JL)))        
        ZTG_TOP(JI)=ZTG_TOP(JI)+(PTG(JI,JL)-XTT)*ZWGHT_TOP(JI,JL)/ZDG_TOP(JI)
        ZTG_SUB(JI)=ZTG_SUB(JI)+(PTG(JI,JL)-XTT)*ZWGHT_SUB(JI,JL)/ZDG_SUB(JI)
     ENDDO
  ENDDO 
ENDIF
!
!
!*      2.     Autotrophic respiration
!              -----------------------
!
ZRESP_AUTO_ABOVE(:)=0.
!
IF (HPHOTO=='NIT') THEN
!        
  DO JNBIOMASS=1,2
    ZRESP_AUTO_ABOVE(:) = ZRESP_AUTO_ABOVE(:) + PRESP_BIOMASS_INST(:,JNBIOMASS)
  ENDDO
!  
ELSE IF (HPHOTO=='NCB') THEN
!        
  DO JNBIOMASS=1,3
    ZRESP_AUTO_ABOVE(:) = ZRESP_AUTO_ABOVE(:) + PRESP_BIOMASS_INST(:,JNBIOMASS)
  ENDDO
!  
ELSE IF (HPHOTO=='AGS' .OR. HPHOTO=='AST' .OR. HPHOTO=='LAI' .OR. HPHOTO=='LST') THEN
!        
  ZRESP_AUTO_ABOVE(:) = PRESP_BIOMASS_INST(:,1)
!  
ENDIF
!
DO JNBIOMASS=1,INBIOMASS
   PRESP_AUTO(:) = PRESP_AUTO (:) + PRESP_BIOMASS_INST(:,JNBIOMASS)
ENDDO
!
!
!*      3.     Soil and ecosystem respiration
!              ------------------------------
!
IF (HRESPSL == 'DEF') THEN
!
   ZWORK(:)=0.0
!   
   IF(HISBA/='DIF')THEN            
      ZWORK (:) = PWG (:,1)  
   ELSE
      DO JL=1,INL
         DO JI=1,INI        
            ZWORK(JI)=ZWORK(JI)+PWG(JI,JL)*ZWGHT_TOP(JI,JL)/ZDG_TOP(JI)
         ENDDO
      ENDDO  
   ENDIF
!
! Soil respiration from Norman et al 1992 (kgCO2/kgair m/s)
!
  WHERE (PLAI(:) == XUNDEF)
    ZRESP_SOIL_TOT(:) = 0.0
  ELSEWHERE
! Before optimization = (ZCOEF1/PRHOA)*(ZCOEF2+ZCOEF3*PLAI(:))*PWG(:,1)*2.**(ZCOEF4*(ZT2(:)-ZCOEF5))        
    ZRESP_SOIL_TOT(:) = (ZCOEF1/PRHOA)*(ZCOEF2+ZCOEF3*PLAI(:))*ZWORK(:)*EXP(ZLOG2*(ZCOEF4*(ZTG_SUB(:)-ZCOEF5)))
  ENDWHERE
!
! RESP_ECO is diagnosed from RESP_SOIL_TOT and RESP_AUTO_ABOVE
!
  PRESP_ECO(:) = ZRESP_SOIL_TOT(:) + ZRESP_AUTO_ABOVE(:)
!  
ELSE IF (HRESPSL == 'PRM') THEN
!
   ZWORK(:)=0.0
!   
   IF(HISBA/='DIF')THEN            
      ZWORK (:) = PWG (:,1)  
   ELSE
      DO JL=1,INL
         DO JI=1,INI        
            ZWORK(JI)=ZWORK(JI)+MIN(1.0,PWG(JI,JL)/PWFC(JI,JL))*ZWGHT_TOP(JI,JL)/ZDG_TOP(JI)
         ENDDO
      ENDDO  
   ENDIF
!
! Ecosystem respiration : Q10 model following Albergel et al. 2009 for SMOSREX
! (kgCO2/kgair m/s)
! RESP_ECO is directly calculated by the parameterization
!
! Before optimization 
! PRESP_ECO(:) = PRE25(:)/PRHOA(:) * MIN(PWG(:,1)/PWFC(:,1),1.)*2.**(ZCOEF4*(ZT2(:)-ZCOEF5))        
  PRESP_ECO(:) = PRE25(:)/PRHOA(:) * ZWORK(:)*EXP(ZLOG2*(ZCOEF4*(ZTG_SUB(:)-ZCOEF5)))
!
ELSE IF (HRESPSL == 'CNT') THEN
!
! Heterotrophic respiration following CENTURY model from Gibelin et al. 2008
!
  ZMOIST_TOP(:)=0.0
  ZSAT_TOP  (:)=0.0
  ZMOIST_SUB(:)=0.0
  ZSAT_SUB  (:)=0.0
  ZSAND_SUB (:)=0.0
!
  IF(HISBA/='DIF')THEN
!          
    ZMOIST_TOP(:) = MIN(1.0,MAX(0.0,(PWG(:,1)-PWWILT(:,1))/(PWFC (:,1)-PWWILT(:,1))))
    ZSAT_TOP  (:) = MIN(1.0,MAX(0.0,(PWG(:,1)-PWFC  (:,1))/(PWSAT(:,1)-PWFC  (:,1))))
    ZMOIST_SUB(:) = MIN(1.0,MAX(0.0,(PWG(:,2)-PWWILT(:,2))/(PWFC (:,2)-PWWILT(:,2))))
    ZSAT_SUB  (:) = MIN(1.0,MAX(0.0,(PWG(:,2)-PWFC  (:,2))/(PWSAT(:,2)-PWFC  (:,2))))
!    
    ZSAND_SUB (:) = PSAND (:,2)
!    
  ELSE
!          
    DO JL=1,INL
       DO JI=1,INI
!       
          ZMOISTL=MIN(1.0,MAX(0.0,(PWG(JI,JL)-PWWILT(JI,JL))/(PWFC (JI,JL)-PWWILT(JI,JL))))
          ZSATL  =MIN(1.0,MAX(0.0,(PWG(JI,JL)-PWFC  (JI,JL))/(PWSAT(JI,JL)-PWFC  (JI,JL)))) 
! 
          ZMOIST_TOP(JI)=ZMOIST_TOP(JI)+ZMOISTL*ZWGHT_TOP(JI,JL)/ZDG_TOP(JI)
          ZSAT_TOP  (JI)=ZSAT_TOP  (JI)+ZSATL  *ZWGHT_TOP(JI,JL)/ZDG_TOP(JI)
          ZMOIST_SUB(JI)=ZMOIST_SUB(JI)+ZMOISTL*ZWGHT_SUB(JI,JL)/ZDG_SUB(JI)
          ZSAT_SUB  (JI)=ZSAT_SUB  (JI)+ZSATL  *ZWGHT_SUB(JI,JL)/ZDG_SUB(JI)          
!
          ZSAND_SUB(JI)=ZSAND_SUB(JI)+PSAND(JI,JL)*ZWGHT_SUB(JI,JL)/ZDG_SUB(JI)
!          
       ENDDO
    ENDDO 
!    
  ENDIF
!
  ZCONTROL_TEMP (:,1) = CONTROL_TEMP_FUNC (ZTG_TOP(:))
  ZCONTROL_TEMP (:,2) = CONTROL_TEMP_FUNC (ZTG_SUB(:))
  ZCONTROL_MOIST(:,1) = CONTROL_MOIST_FUNC(ZMOIST_TOP(:),ZSAT_TOP(:))
  ZCONTROL_MOIST(:,2) = CONTROL_MOIST_FUNC(ZMOIST_SUB(:),ZSAT_SUB(:))
!
  CALL CARBON_LITTER (PTSTEP,PTURNOVER,PLITTER,PLIGNIN_STRUC,         &
                     ZCONTROL_TEMP,ZCONTROL_MOIST,                    &
                     ZRESP_HETERO_DAY_LITTER,ZSOILCARBON_INPUT)  
!
  DO ITCSPIN = 1,KSPINS
     CALL CARBON_SOIL (PTSTEP,ZSAND_SUB,ZSOILCARBON_INPUT,ZCONTROL_TEMP,&
                       ZCONTROL_MOIST,PSOILCARB,ZRESP_HETERO_DAY_SOIL   )  
  ENDDO
!
! Transform units : gC/m2/day -> kgCO2/kgair m/s
!
  ZRESP_HETERO(:) = (ZRESP_HETERO_DAY_LITTER(:) + ZRESP_HETERO_DAY_SOIL(:)) &
                    * (XMCO2/XMC) / (1000. * PRHOA(:)* XDAY)  
!  
! RESP_ECO is diagnosed from RESP_HETERO and RESP_AUTO
!
  PRESP_ECO(:) = ZRESP_HETERO(:) + PRESP_AUTO(:)
!  
ENDIF
!
IF (LHOOK) CALL DR_HOOK('CARBON_EVOL',1,ZHOOK_HANDLE)
!
!-----------------------------------------------------------------
!
END SUBROUTINE CARBON_EVOL
