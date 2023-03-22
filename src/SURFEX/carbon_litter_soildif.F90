!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ######### 
SUBROUTINE CARBON_LITTER_SOILDIF(IO, PK, PEK, PTSTEP,                               &
                                 PFOXIC_LITTER, PFMG_LITTER, PFLEACH_LITTER,        &
                                 PRESP_HETERO_SOIL, PSOILCARBON_INPUT,              &
                                 PRO2_OXIC, PRCO2_OXIC, PRCH4_MG,                   &
                                 PFDOC_LITTER_SOIL                                  )  
   
!   ###############################################################
!!**  CARBON_LITTER_SOILDIF 
!!
!!    PURPOSE
!!    -------
!!    Calculates litter evolution in the soil
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
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
USE MODD_ISBA_n,         ONLY : ISBA_P_t, ISBA_PE_t
!
USE MODD_CO2V_PAR,       ONLY : XFRAC_SOILCARB, XMO2, XMCO2, XMCH4, XMC
USE MODD_SURF_PAR,       ONLY : XUNDEF
!
USE MODI_BIOMASS_TO_SOIL_LITTER
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
TYPE(ISBA_P_t),         INTENT(INOUT) :: PK
TYPE(ISBA_PE_t),        INTENT(INOUT) :: PEK
!
REAL, INTENT(IN)                      :: PTSTEP                     ! time step in s
REAL, DIMENSION(:,:,:), INTENT(IN)    :: PFOXIC_LITTER              ! oxic flux out of structural litter (gC/m2)
REAL, DIMENSION(:,:,:), INTENT(IN)    :: PFMG_LITTER                ! methanogenesis flux out of structural litter (gC/m2)
REAL, DIMENSION(:,:,:), INTENT(IN)    :: PFLEACH_LITTER             ! leaching flux out of metabolic litter (gC/m2)
!
!*       0.2 modified fields 
!
REAL, DIMENSION(:,:,:), INTENT(INOUT) :: PSOILCARBON_INPUT          ! quantity of carbon going into carbon pools from litter decomposition (gC/m2/s)
!
!*       0.3 output
!
REAL, DIMENSION(:),   INTENT(OUT)      :: PRESP_HETERO_SOIL         ! Soil litter heterotrophic respiration (in gCO2/m2/s)
REAL, DIMENSION(:,:), INTENT(OUT)      :: PRO2_OXIC                 ! O2 consumed during oxic decomposition (g/m2/s sol)
REAL, DIMENSION(:,:), INTENT(OUT)      :: PRCO2_OXIC                ! CO2 produced during oxic decomposition (g/m2/s sol)
REAL, DIMENSION(:,:), INTENT(OUT)      :: PRCH4_MG                  ! CH4 produced during methanogenesis (gCH4/m2/s)
!
REAL, DIMENSION(:),   INTENT(OUT)      :: PFDOC_LITTER_SOIL         ! Litter carbon leaching (in gC/m2)
!
!-------------------------------------------------------------------------------
!
!*       0.4 local
!
REAL, DIMENSION(SIZE(PEK%XSOILDIF_LITTER,1),SIZE(PEK%XSOILDIF_LITTER,2),SIZE(PEK%XSOILDIF_LITTER,3)) :: ZFDOC_SOIL  ! DOC (gC m-2)
!
REAL, DIMENSION(SIZE(PK%XTURNOVER,1),SIZE(PK%XTURNOVER,2)) :: ZTURNOVER
!
REAL, DIMENSION(SIZE(PK%XROOTFRAC,1),SIZE(PK%XROOTFRAC,2)) :: ZWORK
!
REAL            :: ZDT, ZSPIN     
!
INTEGER         :: INI,INL,INLITTER,IDEPTH
INTEGER         :: JI,JL
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
! correspondence between array indices and litter type (INLITTER)
! LT_METABOLIC = 1
! LT_STRUCTURAL = 2
!-------------------------------------------------------------------------------
!
!*    1 Initialisations
!       ---------------
!
IF (LHOOK) CALL DR_HOOK('CARBON_LITTER_SOILDIF',0,ZHOOK_HANDLE)
!
!*    1.1 dimensions
!
INI      = SIZE(PEK%XSOILDIF_LITTER,1)
INL      = SIZE(PEK%XSOILDIF_LITTER,2)
INLITTER = SIZE(PEK%XSOILDIF_LITTER,3)
!
ZSPIN    = REAL(IO%NSPINS)
!
ZDT = ZSPIN*PTSTEP
!
!*     1.2 set output to zero
!
PRESP_HETERO_SOIL(:) = 0.0
!
PFDOC_LITTER_SOIL(:) = 0.0
!
PRO2_OXIC (:,:) = 0.0
PRCO2_OXIC(:,:) = 0.0
PRCH4_MG  (:,:) = 0.0
!
!-------------------------------------------------------------------------------
!
!*    2. Add biomass to different litterpools
!        ------------------------------------
!
ZTURNOVER(:,:) = PK%XTURNOVER(:,:) * ZDT ! gC/m2/s -> gC/m2
!
CALL BIOMASS_TO_SOIL_LITTER(ZTURNOVER,PEK%XSOILDIF_LITTER,PEK%XSOILDIF_LIGNIN_STRUC, &
                            kWG_LAYER=PK%NWG_LAYER,PROOTFRAC=PK%XROOTFRAC    )
!
!-------------------------------------------------------------------------------
!
!*    3. fluxes from surface litter to "near surface" carbon pools and respiration
!        -------------------------------------------------------------------------
!
!*    3.1 structural litter: goes into active and slow carbon pools + respiration
!
!*    3.1.1 non-lignin fraction of structural litter goes into active carbon pool + respiration
!
ZWORK(:,:) = PFOXIC_LITTER(:,:,2)*(1.0-PEK%XSOILDIF_LIGNIN_STRUC(:,:))
!
PSOILCARBON_INPUT(:,:,1)=PSOILCARBON_INPUT(:,:,1)+XFRAC_SOILCARB(2,1,2)*ZWORK(:,:)/ZDT
!
IF(IO%LSOILGAS)THEN
! O2 consumed and CO2 produced during oxic decomposition (g/m2sol)
  PRO2_OXIC (:,:) = PRO2_OXIC (:,:) + ((1.0-XFRAC_SOILCARB(2,1,2))*(XMO2/XMC) )*ZWORK(:,:)/ZDT !gO2/m2/s sol
  PRCO2_OXIC(:,:) = PRCO2_OXIC(:,:) + ((1.0-XFRAC_SOILCARB(2,1,2))*(XMCO2/XMC))*ZWORK(:,:)/ZDT !gCO2/m2/s sol
ELSE
!
  DO JL=1,INL
    DO JI=1,INI
       IDEPTH=PK%NWG_LAYER(JI)
       IF (JL<=IDEPTH) THEN
          PRESP_HETERO_SOIL(JI)=PRESP_HETERO_SOIL(JI)+(1.0-XFRAC_SOILCARB(2,1,2))*ZWORK(JI,JL)*(XMCO2/XMC/ZDT)
       ENDIF
    ENDDO
  ENDDO
!
ENDIF
!
!*    3.1.2 lignin fraction of structural litter goes into slow carbon pool + respiration
!
ZWORK(:,:) = PFOXIC_LITTER(:,:,2)*PEK%XSOILDIF_LIGNIN_STRUC(:,:)
!
PSOILCARBON_INPUT(:,:,2)=PSOILCARBON_INPUT(:,:,2)+XFRAC_SOILCARB(2,2,2)*ZWORK(:,:)/ZDT
!
IF(IO%LSOILGAS)THEN
! O2 consumed and CO2 produced during oxic decomposition (g/m2sol)
  PRO2_OXIC (:,:) = PRO2_OXIC (:,:) + ((1.0-XFRAC_SOILCARB(2,2,2))*(XMO2/XMC) )*ZWORK(:,:)/ZDT !gO2/m2 sol
  PRCO2_OXIC(:,:) = PRCO2_OXIC(:,:) + ((1.0-XFRAC_SOILCARB(2,2,2))*(XMCO2/XMC))*ZWORK(:,:)/ZDT !gCO2/m2 sol
ELSE
  DO JL=1,INL
    DO JI=1,INI
       IDEPTH=PK%NWG_LAYER(JI)
       IF (JL<=IDEPTH) THEN
          PRESP_HETERO_SOIL(JI)=PRESP_HETERO_SOIL(JI)+(1.0-XFRAC_SOILCARB(2,2,2))*ZWORK(JI,JL)*(XMCO2/XMC/ZDT)
       ENDIF
    ENDDO
  ENDDO
ENDIF
!
!*    3.2 metabolic litter: goes into active carbon pool + respiration
!
! put decomposed litter into carbon pool + respiration
PSOILCARBON_INPUT(:,:,1)=PSOILCARBON_INPUT(:,:,1)+XFRAC_SOILCARB(1,1,2)*PFOXIC_LITTER(:,:,1)/ZDT  
!
IF(IO%LSOILGAS)THEN
! O2 consumed and CO2 produced during oxic decomposition (g/m2sol)
  PRO2_OXIC (:,:) = PRO2_OXIC (:,:) + ((1.0-XFRAC_SOILCARB(1,1,2))*(XMO2/XMC) )*PFOXIC_LITTER(:,:,1)/ZDT !gO2/m2 sol
  PRCO2_OXIC(:,:) = PRCO2_OXIC(:,:) + ((1.0-XFRAC_SOILCARB(1,1,2))*(XMCO2/XMC))*PFOXIC_LITTER(:,:,1)/ZDT !gCO2/m2 sol
ELSE
  DO JL=1,INL
    DO JI=1,INI
       IDEPTH=PK%NWG_LAYER(JI)
       IF (JL<=IDEPTH) THEN
          PRESP_HETERO_SOIL(JI)=PRESP_HETERO_SOIL(JI)+(1.0-XFRAC_SOILCARB(1,1,2))*PFOXIC_LITTER(JI,JL,1)*(XMCO2/XMC/ZDT)
       ENDIF
    ENDDO
  ENDDO
ENDIF
!
!-------------------------------------------------------------------------------
!
!*    4. Methanogenesis fluxes from soil litter 
!        --------------------------------------
!
IF(IO%LSOILGAS)THEN
! CH4 produced by structural litter (gCH4/m2)
  PRCH4_MG(:,:) = (PFMG_LITTER(:,:,1)+PFMG_LITTER(:,:,2)) * (XMCH4/XMC/ZDT)
ENDIF
!
!-------------------------------------------------------------------------------
!
!*    5. Dissolved organic carbon
!        ------------------------
!
IF(IO%LCLEACH)THEN
!
! Update litter doc flux  in gC m-2 s-1
!
  ZFDOC_SOIL(:,:,:)=PFLEACH_LITTER(:,:,:)/ZDT
!
  PFDOC_LITTER_SOIL(:) = 0.0
  DO JL=1,INL
    DO JI=1,INI
       IDEPTH=PK%NWG_LAYER(JI)
       IF (JL<=IDEPTH) THEN
           PFDOC_LITTER_SOIL(JI) = PFDOC_LITTER_SOIL(JI)+ZFDOC_SOIL(JI,JL,1)+ZFDOC_SOIL(JI,JL,2)
       ENDIF
    ENDDO
  ENDDO
!
ENDIF
!
!-------------------------------------------------------------------------------
!
!*    6. Update litter that is decomposed
!        --------------------------------
!
! Total quantity of litter which is decomposed
!
PEK%XSOILDIF_LITTER(:,:,:)=PEK%XSOILDIF_LITTER(:,:,:)-PFOXIC_LITTER(:,:,:)-PFMG_LITTER(:,:,:)-PFLEACH_LITTER(:,:,:)
!
! No carbon under hydrological depth
!
DO JL=1,INL
  DO JI=1,INI
     IDEPTH=PK%NWG_LAYER(JI)
     IF (JL>IDEPTH) THEN
        PSOILCARBON_INPUT  (JI,JL,1)=0.0
        PSOILCARBON_INPUT  (JI,JL,2)=0.0
        PEK%XSOILDIF_LITTER(JI,JL,1)=XUNDEF
        PEK%XSOILDIF_LITTER(JI,JL,2)=XUNDEF
     ENDIF
  ENDDO
ENDDO
!
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('CARBON_LITTER_SOILDIF',1,ZHOOK_HANDLE)
!
END SUBROUTINE CARBON_LITTER_SOILDIF
