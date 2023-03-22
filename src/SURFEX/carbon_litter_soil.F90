!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE CARBON_LITTER_SOIL(PTSTEP, KSPINS, PTURNOVER, PLITTER, PLIGNIN_STRUC, &
                              PCONTROL_TEMP, PCONTROL_MOIST, PCONTROL_LEACH,     &
                              PRESP_HETERO_LITTER, PSOILCARBON_INPUT             )  

!   ###############################################################
!!**  CARBON_LITTER_SOIL 
!!
!!    PURPOSE
!!    -------
!!    Calculates litter evolution.
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
!!      
!!    AUTHOR
!!    ------
!!
!!      A.-L. Gibelin           * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    23/06/09
!!      B. Decharme 05/2012 : Optimization
!!      R. Séférian & C. Delire 10/2016 : add doc leaching
!!      C. Delire 10/2020 : Bug in structural litter
!!      B. Decharme 11/2020 : Surface / Soil litter split
!!
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!    
USE MODD_CO2V_PAR,       ONLY : XTAU_LITTER, XFRAC_SOILCARB
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_BIOMASS_TO_SOIL_LITTER
!
IMPLICIT NONE
!
!*       0.1 input
!
! time step in s
REAL, INTENT(IN)                                                 :: PTSTEP
!
! (spinup) number of times is called for each time step
INTEGER, INTENT(IN)                                              :: KSPINS                     
!
! Turnover rates (gC/m**2/s)
REAL, DIMENSION(:,:), INTENT(IN)                                 :: PTURNOVER
! temperature control of heterotrophic respiration, above and below
REAL, DIMENSION(:), INTENT(IN)                                   :: PCONTROL_TEMP
! moisture control of heterotrophic respiration
REAL, DIMENSION(:), INTENT(IN)                                   :: PCONTROL_MOIST
! leaching transfer function for doc 
REAL, DIMENSION(:), INTENT(IN)                                   :: PCONTROL_LEACH
!
!*       0.2 modified fields
!
! metabolic and structural litter, above and below ground (gC/m**2)
REAL, DIMENSION(:,:), INTENT(INOUT)                              :: PLITTER
! ratio Lignin/Carbon in structural litter, above and below ground (gC/m**2)
REAL, DIMENSION(:), INTENT(INOUT)                                :: PLIGNIN_STRUC
! litter heterotrophic respiration (in gC/m**2/s)
REAL, DIMENSION(:), INTENT(INOUT)                                :: PRESP_HETERO_LITTER
!
!*       0.3 output
!
! quantity of carbon going into carbon pools from litter decomposition
!   (gC/m**2/s)
REAL, DIMENSION(:,:), INTENT(OUT)                                :: PSOILCARBON_INPUT
!
!*       0.4 local
!
! fraction of structural or metabolic litter decomposed
REAL, DIMENSION(SIZE(PLITTER,1))                                 :: ZFD
! quantity of structural or metabolic litter decomposed (gC/m**2)
REAL, DIMENSION(SIZE(PLITTER,1))                                 :: ZQD
!
REAL, DIMENSION(SIZE(PTURNOVER,1),SIZE(PTURNOVER,2))             :: ZTURNOVER
!
REAL, DIMENSION(SIZE(PLITTER,1),1,SIZE(PLITTER,2))               :: ZSOIL_LITTER
REAL, DIMENSION(SIZE(PLIGNIN_STRUC,1),1)                         :: ZSOIL_LIGNIN_STRUC
!
! time step in s
REAL                                                             :: ZDT, ZDT_LIT1, ZDT_LIT2, ZSPIN
!
! indices
INTEGER                                                          :: INI,JI,JL
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
! correspondence between array indices and litter type
! LT_METABOLIC = 1
! LT_STRUCTURAL = 2
!-------------------------------------------------------------------------------
!
!*    1 Initialisations
!       ---------------
!
IF (LHOOK) CALL DR_HOOK('CARBON_LITTER_SOIL',0,ZHOOK_HANDLE)
!
!*    1.1 dimensions
!
INI = SIZE(PLITTER,1)
!
ZSPIN      = REAL(KSPINS)
!
ZDT      = (ZSPIN*PTSTEP)
ZDT_LIT1 = (ZSPIN*PTSTEP)/XTAU_LITTER(1)
ZDT_LIT2 = (ZSPIN*PTSTEP)/XTAU_LITTER(2)
!
!*    2 Add biomass to different litterpools
!
ZTURNOVER(:,:) = PTURNOVER(:,:) * ZDT
!
ZSOIL_LITTER      (:,1,:) = PLITTER      (:,:)
ZSOIL_LIGNIN_STRUC(:,1  ) = PLIGNIN_STRUC(:  )
!
CALL BIOMASS_TO_SOIL_LITTER (ZTURNOVER(:,:),ZSOIL_LITTER(:,:,:), ZSOIL_LIGNIN_STRUC(:,:))
!
PLITTER      (:,:) = ZSOIL_LITTER      (:,1,:)
PLIGNIN_STRUC(:  ) = ZSOIL_LIGNIN_STRUC(:,1  )
!
!*    3 fluxes from litter to carbon pools and respiration
!
!*    3.1 structural litter: goes into active and slow carbon pools + respiration
!
!*    3.1.1 total quantity of structural litter which is decomposed
!
ZFD(:)=ZDT_LIT2*(1.-PCONTROL_LEACH(:))*PCONTROL_TEMP(:)*PCONTROL_MOIST(:)*EXP(-3.0*PLIGNIN_STRUC(:))  
!
ZQD(:)=PLITTER(:,2)*ZFD(:)
!      
PLITTER(:,2)=PLITTER(:,2)-ZQD(:)
!
!*    3.1.2 non-lignin fraction of structural litter goes into active carbon pool + respiration
!
PSOILCARBON_INPUT(:,1)=PSOILCARBON_INPUT(:,1)+XFRAC_SOILCARB(2,1,2)*ZQD(:)*(1.0-PLIGNIN_STRUC(:))/ZDT 
!
PRESP_HETERO_LITTER(:)=PRESP_HETERO_LITTER(:)+(1.0-XFRAC_SOILCARB(2,1,2))*ZQD(:)*(1.0-PLIGNIN_STRUC(:))/ZDT  
!
!*    3.1.3 lignin fraction of structural litter goes into slow carbon pool + respiration
!
PSOILCARBON_INPUT(:,2)=PSOILCARBON_INPUT(:,2)+XFRAC_SOILCARB(2,2,2)*ZQD(:)*PLIGNIN_STRUC(:)/ZDT
!
PRESP_HETERO_LITTER(:)=PRESP_HETERO_LITTER(:)+(1.0-XFRAC_SOILCARB(2,2,2))*ZQD(:)*PLIGNIN_STRUC(:)/ZDT 
!
!*    3.2 metabolic litter goes into active carbon pool + respiration
!
!*    3.2.1 total quantity of metabolic litter that is decomposed
!
ZFD(:) = ZDT_LIT1*(1.-PCONTROL_LEACH(:))*PCONTROL_TEMP(:)*PCONTROL_MOIST(:)
!
ZQD(:) = PLITTER(:,1)*ZFD(:)
!
PLITTER(:,1)=PLITTER(:,1)-ZQD(:)
!
!*    3.2.2 put decomposed litter into carbon pool + respiration
!
PSOILCARBON_INPUT(:,1)=PSOILCARBON_INPUT(:,1)+XFRAC_SOILCARB(1,1,2)*ZQD(:)/ZDT 
!
PRESP_HETERO_LITTER(:) = PRESP_HETERO_LITTER(:)+(1.0-XFRAC_SOILCARB(1,1,2))*ZQD(:)/ZDT
!
IF (LHOOK) CALL DR_HOOK('CARBON_LITTER_SOIL',1,ZHOOK_HANDLE)

!
END SUBROUTINE CARBON_LITTER_SOIL
