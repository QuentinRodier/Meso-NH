!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE CARBON_LITTER_SURFACE(PTSTEP, ORIVC,                                          &
                                 PTURNOVER, PSURFACE_LITTER, PSURFACE_LIGNIN_STRUC,      &
                                 PCONTROL_TEMP, PCONTROL_MOIST, PCONTROL_LEACH_SURF,     &
                                 PRESP_HETERO_LITTER, PSOILCARBON_INPUT_SURF, PFDOC_SURF )  
   
!   ###############################################################
!!**  CARBON_LITTER_SURFACE 
!!
!!    PURPOSE
!!    -------
!!    Calculates litter evolution above the soil
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
!!      Morel et al. 2019, JAMES
!!      
!!    AUTHOR
!!    ------
!!
!!      A.-L. Gibelin         * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    23/06/09
!!      B. Decharme 05/2012 : Optimization
!!      R. Séférian & C. Delire 10/2016 : add doc leaching
!!      C. Delire 10/2020 : Bug in structural litter
!!      B. Decharme 11/2020 : Surface / Soil litter split and add DOC flux
!!     
!!
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!    
USE MODD_CO2V_PAR,       ONLY : XLC, XTAU_LITTER, XFRAC_LITTER, XFRAC_SOILCARB
USE MODD_CSTS,           ONLY : XTT
USE MODD_SURF_PAR,       ONLY : XUNDEF
!
USE MODI_BIOMASS_TO_SURFACE_LITTER
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1 input
!
! time step in s
REAL, INTENT(IN)                                                 :: PTSTEP
!
! key to activate carbon leaching module
LOGICAL, INTENT(IN)                                              :: ORIVC                  
!
! Turnover rates (gC/m**2/s)
REAL, DIMENSION(:,:), INTENT(IN)                                 :: PTURNOVER
!
! temperature control of heterotrophic respiration, above and below
REAL, DIMENSION(:), INTENT(IN)                                   :: PCONTROL_TEMP
!
! moisture control of heterotrophic respiration
REAL, DIMENSION(:), INTENT(IN)                                   :: PCONTROL_MOIST
!
! leaching transfer function for doc 
REAL, DIMENSION(:), INTENT(IN)                                   :: PCONTROL_LEACH_SURF
!
!
!*       0.2 modified fields
!
! metabolic and structural litter, above ground 
REAL, DIMENSION(:,:),   INTENT(INOUT)                            :: PSURFACE_LITTER       ! Surface litter pools (gC m-2)
!
! ratio Lignin/Carbon in structural litter, above ground (gC/m**2)
REAL, DIMENSION(:),     INTENT(INOUT)                            :: PSURFACE_LIGNIN_STRUC ! Surface L/C ratio in structural litter
!
!*       0.3 output
!
! litter heterotrophic respiration (in gC/m**2/s)
REAL, DIMENSION(:), INTENT(OUT)                                  :: PRESP_HETERO_LITTER
!
! quantity of carbon going into carbon pools from litter decomposition (gC/m**2/s)
REAL, DIMENSION(:,:), INTENT(OUT)                                :: PSOILCARBON_INPUT_SURF
!
! Surface carbon leaching (in gC/m**2)
REAL, DIMENSION(:), INTENT(OUT)                                  :: PFDOC_SURF
!
!
!*       0.4 local
!
! time step in s
REAL                                                             :: ZDT_LIT1, ZDT_LIT2
!
! fraction of structural or metabolic litter decomposed
REAL, DIMENSION(SIZE(PSURFACE_LITTER,1))                         :: ZSURFACE_FD
!
! quantity of structural or metabolic litter decomposed (gC/m**2)
REAL, DIMENSION(SIZE(PSURFACE_LITTER,1))                         :: ZSURFACE_QD
!
REAL, DIMENSION(SIZE(PSURFACE_LITTER,1),SIZE(PSURFACE_LITTER,2)) :: ZFDOC_SURF
!
REAL, DIMENSION(SIZE(PTURNOVER,1),SIZE(PTURNOVER,2))             :: ZTURNOVER
!
! dimensions
INTEGER                                                          :: INI,INLITTER
!
! indices
INTEGER                                                          :: JI,JL
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
! correspondence between array indices and litter type (INLITTER)
! LT_METABOLIC = 1
! LT_STRUCTURAL = 2
!-------------------------------------------------------------------------------
!
!*    1. Initialisations
!     ------------------
!
!*    1.1 dimensions
!
IF (LHOOK) CALL DR_HOOK('CARBON_LITTER_SURFACE',0,ZHOOK_HANDLE)
!
INI        = SIZE(PSURFACE_LITTER,1)
INLITTER   = SIZE(PSURFACE_LITTER,2)
!
ZDT_LIT1 = PTSTEP/XTAU_LITTER(1)
ZDT_LIT2 = PTSTEP/XTAU_LITTER(2)
!
!*    1.2 set output to zero
!
PRESP_HETERO_LITTER(:) = 0.0
!
PSOILCARBON_INPUT_SURF(:,:) = 0.0
!
PFDOC_SURF(:) = 0.0
!
!-------------------------------------------------------------------------------
!
!*    2. Add biomass to different litterpools
!     ---------------------------------------
!
ZTURNOVER(:,:) = PTURNOVER(:,:) * PTSTEP
!
CALL BIOMASS_TO_SURFACE_LITTER(ZTURNOVER(:,:),PSURFACE_LITTER(:,:),PSURFACE_LIGNIN_STRUC(:))
!
!-------------------------------------------------------------------------------
!
!*    3. fluxes from surface litter to "near surface" carbon pools and respiration
!     ----------------------------------------------------------------------------
!
!
!*    3.1 structural litter: goes into active and slow carbon pools + respiration
!
!*    3.1.1 total quantity of structural litter which is decomposed
!
ZSURFACE_FD(:) = ZDT_LIT2*(1.-PCONTROL_LEACH_SURF(:))*PCONTROL_TEMP(:)*PCONTROL_MOIST(:)*EXP(-3.0*PSURFACE_LIGNIN_STRUC(:))  
!
ZSURFACE_QD(:) = PSURFACE_LITTER(:,2)*ZSURFACE_FD(:)
!      
PSURFACE_LITTER(:,2)=PSURFACE_LITTER(:,2)-ZSURFACE_QD(:)
!
!*    3.1.2 non-lignin fraction of structural litter goes into active carbon pool + respiration
!
PSOILCARBON_INPUT_SURF(:,1) = XFRAC_SOILCARB(2,1,1)*ZSURFACE_QD(:)*(1.0-PSURFACE_LIGNIN_STRUC(:))/PTSTEP  
!
PRESP_HETERO_LITTER(:) = (1.0-XFRAC_SOILCARB(2,1,1))*ZSURFACE_QD(:)*(1.0-PSURFACE_LIGNIN_STRUC(:))/PTSTEP  
!
!*    3.1.3 lignin fraction of structural litter goes into slow carbon pool + respiration
!
PSOILCARBON_INPUT_SURF(:,2) = XFRAC_SOILCARB(2,2,1)*ZSURFACE_QD(:)*PSURFACE_LIGNIN_STRUC(:)/PTSTEP  
!
PRESP_HETERO_LITTER(:) = PRESP_HETERO_LITTER(:)+(1.0-XFRAC_SOILCARB(2,2,1))*ZSURFACE_QD(:)*PSURFACE_LIGNIN_STRUC(:)/PTSTEP  
!
!*    3.2 metabolic litter goes into active carbon pool + respiration
!
!*    3.2.1 total quantity of metabolic litter that is decomposed
!
ZSURFACE_FD(:) = ZDT_LIT1*(1.-PCONTROL_LEACH_SURF(:))*PCONTROL_TEMP(:)*PCONTROL_MOIST(:)
!
ZSURFACE_QD(:) = PSURFACE_LITTER(:,1)*ZSURFACE_FD(:)
!
PSURFACE_LITTER(:,1) = PSURFACE_LITTER(:,1)-ZSURFACE_QD(:)
!
!*    3.2.2 put decomposed litter into carbon pool + respiration
!
PSOILCARBON_INPUT_SURF(:,1) = PSOILCARBON_INPUT_SURF(:,1)+XFRAC_SOILCARB(1,1,1)*ZSURFACE_QD(:)/PTSTEP  
!
PRESP_HETERO_LITTER(:) = PRESP_HETERO_LITTER(:)+(1.0-XFRAC_SOILCARB(1,1,1))*ZSURFACE_QD(:)/PTSTEP
!
!-------------------------------------------------------------------------------
!
!
!*    4. Dissolved organic carbon
!     ---------------------------
!
IF(ORIVC)THEN
!
! Leaching/Mobilisation Surface Litter (gC m-2)
  ZFDOC_SURF(:,1) = ZDT_LIT1*PCONTROL_LEACH_SURF(:)*PCONTROL_TEMP(:)*PCONTROL_MOIST(:)*PSURFACE_LITTER(:,1)
  ZFDOC_SURF(:,2) = ZDT_LIT2*PCONTROL_LEACH_SURF(:)*PCONTROL_TEMP(:)*PCONTROL_MOIST(:)*PSURFACE_LITTER(:,2)
!
  ZFDOC_SURF(:,:) = MIN(ZFDOC_SURF(:,:),PSURFACE_LITTER(:,:))
!
! Update reservoirs
  PSURFACE_LITTER(:,:) = PSURFACE_LITTER(:,:) - ZFDOC_SURF(:,:)
!
! Update surface doc flux  in gC m-2 s-1
  PFDOC_SURF(:) = (ZFDOC_SURF(:,1)+ZFDOC_SURF(:,2))/PTSTEP
!
ENDIF
!
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('CARBON_LITTER_SURFACE',1,ZHOOK_HANDLE)
!
END SUBROUTINE CARBON_LITTER_SURFACE
