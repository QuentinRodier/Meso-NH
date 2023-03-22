!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!#############################
SUBROUTINE AVERAGE_DIAG_CC_ISBA_n (IO, DGO, DE, DEC, NDE, NDEC, NP)
!#############################
!
!
!!****  *AVERAGE_DIAG_CC_ISBA_n*  
!!
!!    PURPOSE
!!    -------
!      Average the diagnostics from all ISBA-CC tiles
!     
!!**  METHOD
!!    ------
!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------ 
!!
!!      
!!    REFERENCE
!!    ---------
!!      
!!    AUTHOR
!!    ------
!!      B. Decharme           * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    03/2022
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_ISBA_OPTIONS_n,  ONLY : ISBA_OPTIONS_t
USE MODD_DIAG_n,          ONLY : DIAG_OPTIONS_t
USE MODD_DIAG_EVAP_ISBA_n,ONLY : DIAG_EVAP_ISBA_t, DIAG_EVAP_ISBA_NP_t
USE MODD_ISBA_n,          ONLY : ISBA_P_t, ISBA_NP_t
!
USE MODE_DIAG
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
TYPE(ISBA_OPTIONS_t),      INTENT(INOUT) :: IO
TYPE(DIAG_OPTIONS_t),      INTENT(INOUT) :: DGO
TYPE(DIAG_EVAP_ISBA_t),    INTENT(INOUT) :: DE
TYPE(DIAG_EVAP_ISBA_t),    INTENT(INOUT) :: DEC 
TYPE(DIAG_EVAP_ISBA_NP_t), INTENT(INOUT) :: NDE
TYPE(DIAG_EVAP_ISBA_NP_t), INTENT(INOUT) :: NDEC
TYPE(ISBA_NP_t),           INTENT(INOUT) :: NP
!
!
!*      0.2    declarations of local variables
!
TYPE(DIAG_EVAP_ISBA_t), POINTER :: DEK
TYPE(ISBA_P_t),         POINTER :: PK
!
INTEGER         :: INP
INTEGER         :: JP, JI, IMASK
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('AVERAGE_DIAG_CC_ISBA_N',0,ZHOOK_HANDLE)
!
!       0.     Initialization
!              --------------
!
INP = IO%NPATCH
!
!
!       1.     Surface Energy fluxes
!              -----------------------
!
IF (DE%LSURF_EVAP_BUDGET) THEN
  !
  CALL INIT_CC_BUD(DE,IO)
  !
  CALL MAKE_AVERAGE_CC(DE,NDE)  
  ! 
ENDIF
!
!       2.     Surface Cumulated Energy fluxes
!              -------------------------------
!
IF (DGO%LSURF_BUDGETC) THEN
  !
  CALL INIT_CC_BUD(DEC,IO)
  !
  CALL MAKE_AVERAGE_CC(DEC,NDEC)
  !
END IF
!
IF (LHOOK) CALL DR_HOOK('AVERAGE_DIAG_CC_ISBA_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
CONTAINS
!-------------------------------------------------------------------------------        
!
SUBROUTINE MAKE_AVERAGE_CC(DEA,NDEA)
!
TYPE(DIAG_EVAP_ISBA_t),    INTENT(INOUT) :: DEA
TYPE(DIAG_EVAP_ISBA_NP_t), INTENT(INOUT) :: NDEA
!
TYPE(DIAG_EVAP_ISBA_t), POINTER :: DEAK
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('AVERAGE_DIAG_CC_ISBA_N:MAKE_AVERAGE_CC',0,ZHOOK_HANDLE)
!
DO JP=1,INP
  !
  PK => NP%AL(JP)
  DEAK => NDEA%AL(JP)
  !
  DO JI=1,PK%NSIZE_P
    !
    IMASK = PK%NR_P(JI)
    !
    ! Gross primary production
    !
    DEA%XGPP(IMASK) = DEA%XGPP(IMASK) + PK%XPATCH(JI) * DEAK%XGPP(JI)
    !
    ! Autotrophic respiration 
    !
    DEA%XRESP_AUTO(IMASK) = DEA%XRESP_AUTO(IMASK) + PK%XPATCH(JI) * DEAK%XRESP_AUTO(JI)
    !
    ! Ecosystem respiration
    !
    DEA%XRESP_ECO(IMASK) = DEA%XRESP_ECO(IMASK) + PK%XPATCH(JI) * DEAK%XRESP_ECO(JI)  
    !
    ! Biomass total turnover
    !
    DEA%XTURNVTOT(IMASK) = DEA%XTURNVTOT(IMASK) + PK%XPATCH(JI) * DEAK%XTURNVTOT(JI)     
    !
    ! Total litter carbon flux to soil carbon pool
    !
    DEA%XFLTOSCARB(IMASK) = DEA%XFLTOSCARB(IMASK) + PK%XPATCH(JI) * DEAK%XFLTOSCARB(JI)     
    !
    ! Heterotrophic respiration from soil carbon
    !
    DEA%XRESPSCARB(IMASK) = DEA%XRESPSCARB(IMASK) + PK%XPATCH(JI) *DEAK%XRESPSCARB(JI)
    !
    ! Heterotrophic respiration from litter carbon
    !
    DEA%XRESPLIT(IMASK) = DEA%XRESPLIT(IMASK) + PK%XPATCH(JI) *DEAK%XRESPLIT(JI)
    !
  END DO
  !
ENDDO
!
IF(IO%LCLEACH)THEN
  !
  DO JP=1,INP
     !
     PK => NP%AL(JP)
     DEAK => NDEA%AL(JP)
     !
     DO JI=1,PK%NSIZE_P
        !
        IMASK = PK%NR_P(JI)
        !
        ! Dissolved organic carbon export from soil and total litter
        !
        DEA%XFDOC(IMASK) = DEA%XFDOC(IMASK) + PK%XPATCH(JI) *DEAK%XFDOC(JI)
        !
        ! Dissolved organic carbon export from surface litter
        !
        DEA%XFDOCLIT(IMASK) = DEA%XFDOCLIT(IMASK) + PK%XPATCH(JI) *DEAK%XFDOCLIT(JI)
        !
     ENDDO
     !
  ENDDO
  !
ENDIF
!
IF(IO%LFIRE)THEN
  !
  DO JP=1,INP
     !
     PK => NP%AL(JP)
     DEAK => NDEA%AL(JP)
     !
     DO JI=1,PK%NSIZE_P
        !
        IMASK = PK%NR_P(JI)
        !
        ! Conversion of biomass to litter due to biomass fire
        !
        DEA%XFIRETURNOVER(IMASK) = DEA%XFIRETURNOVER(IMASK) + PK%XPATCH(JI) * DEAK%XFIRETURNOVER(JI)
        !
        ! Carbon emitted into the atmosphere by fire
        !
        DEA%XFIRECO2(IMASK) = DEA%XFIRECO2(IMASK) + PK%XPATCH(JI) * DEAK%XFIRECO2(JI)
        !
        ! Black carbon emitted into the atmosphere by fire
        !
        DEA%XFIREBCS(IMASK) = DEA%XFIREBCS(IMASK) + PK%XPATCH(JI) * DEAK%XFIREBCS(JI)
        !
     ENDDO
     !
  ENDDO
  !
ENDIF
!
IF(IO%LLULCC)THEN
  !
  DO JP=1,INP
     !
     PK => NP%AL(JP)
     DEAK => NDEA%AL(JP)
     !
     DO JI=1,PK%NSIZE_P
        !
        IMASK = PK%NR_P(JI)
        !
        ! Harvested carbon flux
        !
        DEA%XFHARVEST(IMASK) = DEA%XFHARVEST(IMASK) + PK%XPATCH(JI) * DEAK%XFHARVEST(JI)
        !
     ENDDO
     !
  ENDDO
  !
ENDIF
!
IF(IO%LSOILGAS)THEN
  !
  DO JP=1,INP
     !
     PK => NP%AL(JP)
     DEAK => NDEA%AL(JP)
     !
     DO JI=1,PK%NSIZE_P
        !
        IMASK = PK%NR_P(JI)
        !
        ! O2 flux from soil and vegetation
        !
        DEA%XO2FLUX  (IMASK) = DEA%XO2FLUX(IMASK) + PK%XPATCH(JI) * DEAK%XO2FLUX(JI)
        !
        ! CH4 flux from soil and vegetation
        !
        DEA%XCH4FLUX (IMASK) = DEA%XCH4FLUX(IMASK) + PK%XPATCH(JI) * DEAK%XCH4FLUX(JI)
        !
        ! Gas (O2, CO2, CH4) transported by diffusion at soil/atmosphere interface
        !
        DEA%XSURF_O2 (IMASK) = DEA%XSURF_O2 (IMASK) + PK%XPATCH(JI) * DEAK%XSURF_O2(JI)
        DEA%XSURF_CO2(IMASK) = DEA%XSURF_CO2(IMASK) + PK%XPATCH(JI) * DEAK%XSURF_CO2(JI)
        DEA%XSURF_CH4(IMASK) = DEA%XSURF_CH4(IMASK) + PK%XPATCH(JI) * DEAK%XSURF_CH4(JI)
        !
        ! Gas (O2, CO2, CH4) transported by evapotranspiration
        !
        DEA%XEVAP_O2 (IMASK) = DEA%XEVAP_O2 (IMASK) + PK%XPATCH(JI) * DEAK%XEVAP_O2(JI)
        DEA%XEVAP_CO2(IMASK) = DEA%XEVAP_CO2(IMASK) + PK%XPATCH(JI) * DEAK%XEVAP_CO2(JI)
        DEA%XEVAP_CH4(IMASK) = DEA%XEVAP_CH4(IMASK) + PK%XPATCH(JI) * DEAK%XEVAP_CH4(JI)
        !
        ! Gas (O2, CO2, CH4) transported by plant
        !
        DEA%XPMT_O2  (IMASK) = DEA%XPMT_O2 (IMASK) + PK%XPATCH(JI) * DEAK%XPMT_O2(JI)
        DEA%XPMT_CO2 (IMASK) = DEA%XPMT_CO2(IMASK) + PK%XPATCH(JI) * DEAK%XPMT_CO2(JI)
        DEA%XPMT_CH4 (IMASK) = DEA%XPMT_CH4(IMASK) + PK%XPATCH(JI) * DEAK%XPMT_CH4(JI)
        !
        ! CH4 transported by ebullition
        !
        DEA%XEBU_CH4 (IMASK) = DEA%XEBU_CH4(IMASK) + PK%XPATCH(JI) * DEAK%XEBU_CH4(JI)
        !
        ! O2 consumed during oxic decomposition and methanotrophy
        !
        DEA%XFCONS_O2 (IMASK) = DEA%XFCONS_O2(IMASK) + PK%XPATCH(JI) * DEAK%XFCONS_O2(JI)
        !
        ! CO2 produced during oxic decomposition and methanotrophy
        !
        DEA%XFPROD_CO2(IMASK) = DEA%XFPROD_CO2(IMASK) + PK%XPATCH(JI) * DEAK%XFPROD_CO2(JI)
        !
        ! CH4 consumed during methanotrophy
        !
        DEA%XFMT_CH4  (IMASK) = DEA%XFMT_CH4(IMASK) + PK%XPATCH(JI) * DEAK%XFMT_CH4(JI)
        !
        ! CH4 produced during methanogenesis
        !
        DEA%XFMG_CH4  (IMASK) = DEA%XFMG_CH4(IMASK) + PK%XPATCH(JI) * DEAK%XFMG_CH4(JI)
        !
     ENDDO
     !
  ENDDO
  !
ENDIF
!
IF (LHOOK) CALL DR_HOOK('AVERAGE_DIAG_CC_ISBA_N:MAKE_AVERAGE_CC',1,ZHOOK_HANDLE)
!
END SUBROUTINE MAKE_AVERAGE_CC
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE AVERAGE_DIAG_CC_ISBA_n
