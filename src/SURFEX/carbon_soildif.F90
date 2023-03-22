!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE CARBON_SOILDIF (IO, KK, PK, PEK, PTSTEP, PSOILCARBON_INPUT,&
                           PFOXIC_SOC, PFMG_SOC, PFLEACH_SOC,         &
                           PRO2_OXIC, PRCO2_OXIC, PRCH4_MG,           &
                           PRESP_HETERO_SOIL, PFDOC_SOILCARBON        )  
!   ###############################################################
!!**  CARBON_SOILDIF 
!!
!!    PURPOSE
!!    -------
!!    Calculates soil carbon pools evolution
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
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
USE MODD_ISBA_n,         ONLY : ISBA_K_t, ISBA_P_t, ISBA_PE_t
!
USE MODD_CO2V_PAR,       ONLY : XTAU_SOILCARB, XMO2, XMCO2, XMCH4, XMC
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
!*       0.1 input
!
!
TYPE(ISBA_OPTIONS_t),   INTENT(INOUT) :: IO
TYPE(ISBA_K_t),         INTENT(INOUT) :: KK
TYPE(ISBA_P_t),         INTENT(INOUT) :: PK
TYPE(ISBA_PE_t),        INTENT(INOUT) :: PEK
!
!
REAL, INTENT(IN)                      :: PTSTEP               ! time step in s
!
REAL, DIMENSION(:,:,:), INTENT(IN)    :: PSOILCARBON_INPUT    ! quantity of carbon going into carbon pools from litter decomposition (gC/m2/s)
REAL, DIMENSION(:,:,:), INTENT(IN)    :: PFOXIC_SOC           ! oxic flux out of carbon pools by layers (gC/m2)
REAL, DIMENSION(:,:  ), INTENT(IN)    :: PFMG_SOC             ! methanogenesis flux out of carbon pools by layers (gC/m**2)
REAL, DIMENSION(:,:,:), INTENT(IN)    :: PFLEACH_SOC          ! leaching flux out of carbon pools by layers (gC/m2)
!
!*       0.2 modified fields
!
REAL, DIMENSION(:,:),   INTENT(INOUT) :: PRO2_OXIC             ! O2 consumed during oxic decomposition (g/m2/s sol)
REAL, DIMENSION(:,:),   INTENT(INOUT) :: PRCO2_OXIC            ! CO2 produced during oxic decomposition (g/m2/s sol)
REAL, DIMENSION(:,:),   INTENT(INOUT) :: PRCH4_MG              ! CH4 produced during methanogenesis (gCH4/m2/s)
!
REAL, DIMENSION(:), INTENT(INOUT)     :: PRESP_HETERO_SOIL     ! soil heterotrophic respiration (in gCO2/s/m2)
!
!*       0.3 output
!
REAL, DIMENSION(:), INTENT(OUT)       :: PFDOC_SOILCARBON     ! Soil carbon leaching (in gC/m2)
!
!-------------------------------------------------------------------------------
!
!*       0.4 local
!
REAL, DIMENSION(SIZE(PRCH4_MG,1),SIZE(PEK%XSOILDIF_CARB,2),SIZE(PEK%XSOILDIF_CARB,3),SIZE(PEK%XSOILDIF_CARB,3)) :: ZFRAC_CARB  ! flux fractions within carbon pools
! 
REAL, DIMENSION(SIZE(PRCH4_MG,1),SIZE(PEK%XSOILDIF_CARB,2),SIZE(PEK%XSOILDIF_CARB,3))                           :: ZFRAC_RESP ! fraction of carbon flux which goes into heterotrophic respiration
!

! fluxes between carbon pools (gC/m2)
REAL, DIMENSION(SIZE(PRCH4_MG,1),SIZE(PEK%XSOILDIF_CARB,2),SIZE(PEK%XSOILDIF_CARB,3),SIZE(PEK%XSOILDIF_CARB,3)) :: ZFLUX
!
REAL, DIMENSION(SIZE(PRCH4_MG,1),SIZE(PEK%XSOILDIF_CARB,2),SIZE(PEK%XSOILDIF_CARB,3))                           :: ZMG, ZFDOC_SOIL
!
REAL, DIMENSION(SIZE(PRCH4_MG,1),SIZE(PEK%XSOILDIF_CARB,2))                                                     :: ZWORK ! Work array
!
! dimensions
INTEGER :: INI, INC, INL, IDEPTH
!
! indices
INTEGER :: JI, JL, JJ
!
REAL    :: ZDT, ZSPIN
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
IF (LHOOK) CALL DR_HOOK('CARBON_SOILDIF',0,ZHOOK_HANDLE)
!
!*       1 Initialisations
!        -----------------------------------------------
!
ZSPIN = REAL(IO%NSPINS)
!
ZDT = ZSPIN*PTSTEP
!
!*       1.1 dimensions
!
INI = SIZE(PEK%XSOILDIF_CARB,1)
INL = SIZE(PEK%XSOILDIF_CARB,2)
INC = SIZE(PEK%XSOILDIF_CARB,3)
!
!*       1.2 set local variables to zero
!
ZMG       (:,:,:) = 0.0
ZFDOC_SOIL(:,:,:) = 0.0
!
ZFLUX(:,:,:,:) = 0.0
!
!*       1.2 set output to zero
!
PFDOC_SOILCARBON(:) = 0.0
!
!*       1.3  Flux fractions between carbon pools: depend on soil texture, recalculated each time
!
!from active pool: depends on soil texture
ZFRAC_CARB(:,:,1,1) = 0.0
ZFRAC_CARB(:,:,1,3) = 0.004
ZFRAC_CARB(:,:,1,2) = 1. - ( .85 - .68 * (1.-KK%XSAND(:,:)) ) - ZFRAC_CARB(:,:,1,3)
!
!from slow pool
ZFRAC_CARB(:,:,2,2) = .0
ZFRAC_CARB(:,:,2,1) = .42
ZFRAC_CARB(:,:,2,3) = .03
!
!from passive pool
ZFRAC_CARB(:,:,3,3) = .0
ZFRAC_CARB(:,:,3,1) = .45
ZFRAC_CARB(:,:,3,2) = .0
!
!-------------------------------------------------------------------------------
!
!*       3 fluxes within carbon reservoirs + respiration
!        -----------------------------------------------
!
!*       3.1 O2 consumed, CO2 produced, methanogenesis or Soil respiration
!
!determine fraction of flux that is respiration (diagonal elements of frac_carb are zero)
ZFRAC_RESP(:,:,:) = 1.0 - ZFRAC_CARB(:,:,:,1) - ZFRAC_CARB(:,:,:,2) - ZFRAC_CARB(:,:,:,3)   
!
ZWORK(:,:) =0.0
WHERE(PEK%XSOILDIF_CARB(:,:,1)/=XUNDEF)
      ZWORK(:,:) = PFOXIC_SOC(:,:,1)*ZFRAC_RESP(:,:,1) &
                 + PFOXIC_SOC(:,:,2)*ZFRAC_RESP(:,:,2) &
                 + PFOXIC_SOC(:,:,3)*ZFRAC_RESP(:,:,3)
ENDWHERE
!
IF(IO%LSOILGAS)THEN
!
! Calculation of O2 consumed and CO2 produced by oxic decomposition (g/m2soil)
!
  PRO2_OXIC (:,:) = PRO2_OXIC (:,:) + ZWORK   (:,:) * (XMO2 /XMC/ZDT)
  PRCO2_OXIC(:,:) = PRCO2_OXIC(:,:) + ZWORK   (:,:) * (XMCO2/XMC/ZDT)
  PRCH4_MG  (:,:) = PRCH4_MG  (:,:) + PFMG_SOC(:,:) * (XMCH4/XMC/ZDT)
!
ELSE
!
! Soil respiration espiration (Bulk scheme)
!
  DO JL=1,INL
     DO JI=1,INI
        PRESP_HETERO_SOIL(JI) = PRESP_HETERO_SOIL(JI) + ZWORK(JI,JL) * (XMCO2/XMC/ZDT)
     ENDDO
  ENDDO
!
ENDIF
!
!-------------------------------------------------------------------------------
!
!
!*    4. Dissolved organic carbon
!        ------------------------
!
IF(IO%LCLEACH)THEN
!
! Update soil doc flux  in gC m-2 s-1
!
  ZFDOC_SOIL(:,:,:)=PFLEACH_SOC(:,:,:)/ZDT
!
  PFDOC_SOILCARBON(:) = 0.0
  DO JL=1,INL
    DO JI=1,INI
       IDEPTH=PK%NWG_LAYER(JI)
       IF (JL<=IDEPTH) THEN
           PFDOC_SOILCARBON(JI) = PFDOC_SOILCARBON(JI)+ZFDOC_SOIL(JI,JL,1)+ZFDOC_SOIL(JI,JL,2)
       ENDIF
    ENDDO
  ENDDO
!
ENDIF
!
!-------------------------------------------------------------------------------
!
!
!*    4. Update carbon pools
!        ------------------------
!
!*       4.1 Update carbon pools according to litter input, oxic decomposition, methanogenesis and DOC
!
ZMG(:,:,1) = PFMG_SOC(:,:)
!
WHERE(PEK%XSOILDIF_CARB(:,:,:)/=XUNDEF)
      PEK%XSOILDIF_CARB(:,:,:) = PEK%XSOILDIF_CARB(:,:,:) + PSOILCARBON_INPUT(:,:,:)*ZDT &
                               - PFOXIC_SOC(:,:,:) - ZMG(:,:,:) - PFLEACH_SOC(:,:,:)
ENDWHERE
!
!*       4.2 Update carbon pools according to fluxes between active, slow, and passive pools
!
DO JJ=1,INC
   DO JL=1,INL
      DO JI=1,INI
         IDEPTH=PK%NWG_LAYER(JI)
          IF(JL<=IDEPTH)THEN
            ZFLUX(JI,JL,1,JJ) = ZFRAC_CARB(JI,JL,1,JJ) * PFOXIC_SOC(JI,JL,1)
            ZFLUX(JI,JL,2,JJ) = ZFRAC_CARB(JI,JL,2,JJ) * PFOXIC_SOC(JI,JL,2)
            ZFLUX(JI,JL,3,JJ) = ZFRAC_CARB(JI,JL,3,JJ) * PFOXIC_SOC(JI,JL,3)
          ENDIF
      ENDDO
   ENDDO
ENDDO
!
WHERE(PEK%XSOILDIF_CARB(:,:,1)/=XUNDEF)
      PEK%XSOILDIF_CARB(:,:,1) = PEK%XSOILDIF_CARB(:,:,1)+ZFLUX(:,:,1,1)+ZFLUX(:,:,2,1)+ZFLUX(:,:,3,1) 
ENDWHERE
!
WHERE(PEK%XSOILDIF_CARB(:,:,2)/=XUNDEF)
      PEK%XSOILDIF_CARB(:,:,2) = PEK%XSOILDIF_CARB(:,:,2)+ZFLUX(:,:,1,2)+ZFLUX(:,:,2,2)+ZFLUX(:,:,3,2)
ENDWHERE
!
WHERE(PEK%XSOILDIF_CARB(:,:,3)/=XUNDEF)
      PEK%XSOILDIF_CARB(:,:,3) = PEK%XSOILDIF_CARB(:,:,3)+ZFLUX(:,:,1,3)+ZFLUX(:,:,2,3)+ZFLUX(:,:,3,3)  
ENDWHERE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('CARBON_SOILDIF',1,ZHOOK_HANDLE)
!
END SUBROUTINE CARBON_SOILDIF
