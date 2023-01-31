!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!#############################
SUBROUTINE AVERAGE_DIAG_EVAP_ISBA_n (IO, DGO, DE, DEC, NDE, NDEC, NP, &
                                     PTSTEP, PRAIN, PSNOW)
!#############################
!
!
!!****  *AVERAGE_DIAG_EVAP_ISBA_n*  
!!
!!    PURPOSE
!!    -------
!      Average the cumulated diagnostics from all ISBA tiles
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
!!      P. Le Moigne           * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    11/03
!!      B. Decharme 2008     New diag for the water budget
!!      B. Decharme 2012     New diag for snow 
!!                                        carbon
!!                                        isab water budget
!!                  2013                  Sublimation
!!                                        Subsurface runoff if SGH (DIF option only)
!!      P. Samuelsson 10/2014: MEB
!!      R. Séférian   08/2016: Fire and carbon leaching module, add turnover diags
!!      B. Decharme   02/2017: add energy/snowmelt diags
!!      B. Decharme   02/2021: Split carbon diag in AVERAGE_DIAG_CC_ISBA
!!      B. Decharme   02/2021: Bug, DE%XRAINFALL must be in kg/m2/s (previously in kg/m2)
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
REAL,                  INTENT(IN) :: PTSTEP        ! time step (s)
REAL,    DIMENSION(:), INTENT(IN) :: PRAIN         ! rainfall rate
REAL,    DIMENSION(:), INTENT(IN) :: PSNOW         ! snowfall rate
!
!
!*      0.2    declarations of local variables
!
TYPE(DIAG_EVAP_ISBA_t), POINTER :: DEK
TYPE(ISBA_P_t),         POINTER :: PK
!
INTEGER         :: INP
INTEGER         :: JP, JI, IMASK
INTEGER         :: ISIZE_LMEB_PATCH   ! Number of patches where multi-energy balance should be applied
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('AVERAGE_DIAG_EVAP_ISBA_N',0,ZHOOK_HANDLE)
!
!       0.     Initialization
!              --------------
!
INP = IO%NPATCH
!
ISIZE_LMEB_PATCH=COUNT(IO%LMEB_PATCH(:))
!
!       1.     Surface Energy fluxes
!              -----------------------
!
IF (DE%LSURF_EVAP_BUDGET) THEN
  !
  CALL INIT_EVAP_BUD(DE)
  !
  IF (ISIZE_LMEB_PATCH>0) THEN
     CALL INIT_MEB_BUD(DE)
  ENDIF
  !
  IF(DE%LWATER_BUDGET)THEN
    !  
    CALL INIT_WATER_BUD(DE)
    !
    !Kg/m2/s
    DE%XRAINFALL(:) = PRAIN(:)
    DE%XSNOWFALL(:) = PSNOW(:)
    !
  ENDIF
  !
  IF(DE%LENERGY_BUDGET)THEN
    !  
    CALL INIT_ENERGY_BUD(DE)
    !
  ENDIF
  !
  CALL MAKE_AVERAGE_EVAP(DE,NDE)  
  !  
ENDIF
!
!       2.     Surface Cumulated Energy fluxes
!              -------------------------------
!
IF (DGO%LSURF_BUDGETC) THEN
  !
  CALL INIT_EVAP_BUD(DEC)
  !
  IF (ISIZE_LMEB_PATCH>0) THEN
    CALL INIT_MEB_BUD(DEC)
  ENDIF
  !
  IF(DE%LWATER_BUDGET)THEN
    !  
    CALL INIT_WATER_BUD(DEC)
    !
    DEC%XRAINFALL  (:) = DEC%XRAINFALL (:) + PRAIN(:) * PTSTEP
    DEC%XSNOWFALL  (:) = DEC%XSNOWFALL (:) + PSNOW(:) * PTSTEP
    !
  ENDIF
  !
  IF(DE%LENERGY_BUDGET)THEN
    !  
    CALL INIT_ENERGY_BUD(DE)
    !
  ENDIF
  !
  CALL MAKE_AVERAGE_EVAP(DEC,NDEC)
  !  
END IF
!
IF (LHOOK) CALL DR_HOOK('AVERAGE_DIAG_EVAP_ISBA_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
CONTAINS
!-------------------------------------------------------------------------------        
!
SUBROUTINE MAKE_AVERAGE_EVAP(DEA,NDEA)
!
TYPE(DIAG_EVAP_ISBA_t),    INTENT(INOUT) :: DEA
TYPE(DIAG_EVAP_ISBA_NP_t), INTENT(INOUT) :: NDEA
!
TYPE(DIAG_EVAP_ISBA_t), POINTER :: DEAK
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('AVERAGE_DIAG_EVAP_ISBA_N:MAKE_AVERAGE_EVAP',0,ZHOOK_HANDLE)
!
! Isba general
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
    ! Potential evaporation
    DEA%XEPOT(IMASK) = DEA%XEPOT(IMASK) + PK%XPATCH(JI) * DEAK%XEPOT(JI)
    !
    ! Latent heat of evaporation over the ground
    DEA%XLEG (IMASK) = DEA%XLEG (IMASK) + PK%XPATCH(JI) * DEAK%XLEG(JI)
    !
    ! Surface soil ice sublimation
    DEA%XLEGI(IMASK) = DEA%XLEGI(IMASK) + PK%XPATCH(JI) * DEAK%XLEGI(JI)
    !
    ! Latent heat of evaporation over vegetation
    DEA%XLEV (IMASK) = DEA%XLEV (IMASK) + PK%XPATCH(JI) * DEAK%XLEV(JI)
    !
    ! Latent heat of sublimation over snow
    DEA%XLES (IMASK) = DEA%XLES (IMASK) + PK%XPATCH(JI) * DEAK%XLES(JI)
    !
    ! Latent heat of evaporation of liquid water over snow
    DEA%XLESL(IMASK) = DEA%XLESL(IMASK) + PK%XPATCH(JI) * DEAK%XLESL(JI)
    !
    ! Evaporation from canopy water interception
    DEA%XLER (IMASK) = DEA%XLER (IMASK) + PK%XPATCH(JI) * DEAK%XLER(JI)
    !
    ! Evapotranspiration of the vegetation
    DEA%XLETR(IMASK) = DEA%XLETR(IMASK) + PK%XPATCH(JI) * DEAK%XLETR(JI)
    !
    ! Blowing snow sublimation (ES or Crocus)
    DEA%XSNDRIFT(IMASK) = DEA%XSNDRIFT(IMASK) + PK%XPATCH(JI) * DEAK%XSNDRIFT(JI)
    !      
    ! Refreezing of water in the entire snowpack 
    DEA%XSNFREE_SWU(IMASK) = DEA%XSNFREE_SWU(IMASK) + PK%XPATCH(JI) * DEAK%XSNFREE_SWU(JI)
    !
    ! Soil drainage flux
    DEA%XDRAIN (IMASK) = DEA%XDRAIN (IMASK) + PK%XPATCH(JI) * DEAK%XDRAIN(JI)
    !
    ! Soil lateral subsurface flux
    DEA%XQSB(IMASK) = DEA%XQSB(IMASK) + PK%XPATCH(JI) * DEAK%XQSB(JI)        
    !
    ! Supersaturation runoff
    DEA%XRUNOFF(IMASK) = DEA%XRUNOFF(IMASK) + PK%XPATCH(JI) * DEAK%XRUNOFF(JI)
    !
    ! Horton runoff
    DEA%XHORT(IMASK) = DEA%XHORT(IMASK) + PK%XPATCH(JI) * DEAK%XHORT(JI)
    !
    ! Vegetation dripping
    DEA%XDRIP(IMASK) = DEA%XDRIP(IMASK) + PK%XPATCH(JI) * DEAK%XDRIP(JI)
    !
    ! Precipitation intercepted by the vegetation
    DEA%XRRVEG (IMASK) = DEA%XRRVEG (IMASK) + PK%XPATCH(JI) * DEAK%XRRVEG(JI)
    !      
    ! Snow melt
    DEA%XMELT(IMASK) = DEA%XMELT(IMASK) + PK%XPATCH(JI) * DEAK%XMELT(JI)
    ! 
    ! Snow melt over the entire snowpack 
    DEA%XMELTSTOT(IMASK) = DEA%XMELTSTOT(IMASK) + PK%XPATCH(JI) * DEAK%XMELTSTOT(JI)
    ! 
    ! Refreezing of water in the entire snowpack 
    DEA%XSNREFREEZ(IMASK) = DEA%XSNREFREEZ(IMASK) + PK%XPATCH(JI) * DEAK%XSNREFREEZ(JI)
    !     
    ! Flood infiltartion
    DEA%XIFLOOD(IMASK) = DEA%XIFLOOD(IMASK) + PK%XPATCH(JI) * DEAK%XIFLOOD(JI)
    !      
    ! Precipitation intercepted by the floodplains   
    DEA%XPFLOOD(IMASK) = DEA%XPFLOOD(IMASK) + PK%XPATCH(JI) * DEAK%XPFLOOD(JI)
    !      
    ! Floodplains Latent heat of evaporation  
    DEA%XLE_FLOOD (IMASK) = DEA%XLE_FLOOD (IMASK) + PK%XPATCH(JI) * DEAK%XLE_FLOOD (JI)
    DEA%XLEI_FLOOD(IMASK) = DEA%XLEI_FLOOD(IMASK) + PK%XPATCH(JI) * DEAK%XLEI_FLOOD(JI)
    !      
    ! irrigation rate (as soil input)
    DEA%XIRRIG_FLUX(IMASK) = DEA%XIRRIG_FLUX(IMASK) + PK%XPATCH(JI) * DEAK%XIRRIG_FLUX(JI)
    !  
    ! Ice calving flux
    DEA%XICEFLUX(IMASK) = DEA%XICEFLUX(IMASK) + PK%XPATCH(JI) * DEAK%XICEFLUX(JI)      
    !
  END DO
  !
ENDDO
!
! Isba meb
!
IF (ISIZE_LMEB_PATCH>0) THEN
  !
  DO JP=1,INP
     !
     PK => NP%AL(JP)
     DEAK => NDEA%AL(JP)
     !
     DO JI=1,PK%NSIZE_P
        !
        DEA%XLELITTER (IMASK) = DEA%XLELITTER (IMASK) + PK%XPATCH(JI) * DEAK%XLELITTER (JI)
        DEA%XLELITTERI(IMASK) = DEA%XLELITTERI(IMASK) + PK%XPATCH(JI) * DEAK%XLELITTERI(JI)
        DEA%XDRIPLIT  (IMASK) = DEA%XDRIPLIT  (IMASK) + PK%XPATCH(JI) * DEAK%XDRIPLIT  (JI)
        DEA%XRRLIT    (IMASK) = DEA%XRRLIT    (IMASK) + PK%XPATCH(JI) * DEAK%XRRLIT    (JI)     
        !
        DEA%XLEV_CV   (IMASK) = DEA%XLEV_CV   (IMASK) + PK%XPATCH(JI) * DEAK%XLEV_CV   (JI)
        DEA%XLES_CV   (IMASK) = DEA%XLES_CV   (IMASK) + PK%XPATCH(JI) * DEAK%XLES_CV   (JI)
        DEA%XLETR_CV  (IMASK) = DEA%XLETR_CV  (IMASK) + PK%XPATCH(JI) * DEAK%XLETR_CV  (JI)
        DEA%XLER_CV   (IMASK) = DEA%XLER_CV   (IMASK) + PK%XPATCH(JI) * DEAK%XLER_CV   (JI)      
        DEA%XLE_CV    (IMASK) = DEA%XLE_CV    (IMASK) + PK%XPATCH(JI) * DEAK%XLE_CV    (JI)       
        DEA%XH_CV     (IMASK) = DEA%XH_CV     (IMASK) + PK%XPATCH(JI) * DEAK%XH_CV     (JI)   
        DEA%XMELT_CV  (IMASK) = DEA%XMELT_CV  (IMASK) + PK%XPATCH(JI) * DEAK%XMELT_CV  (JI)
        DEA%XFRZ_CV   (IMASK) = DEA%XFRZ_CV   (IMASK) + PK%XPATCH(JI) * DEAK%XFRZ_CV   (JI)   

        DEA%XLETR_GV  (IMASK) = DEA%XLETR_GV  (IMASK) + PK%XPATCH(JI) * DEAK%XLETR_GV  (JI)
        DEA%XLER_GV   (IMASK) = DEA%XLER_GV   (IMASK) + PK%XPATCH(JI) * DEAK%XLER_GV   (JI)
        DEA%XLE_GV    (IMASK) = DEA%XLE_GV    (IMASK) + PK%XPATCH(JI) * DEAK%XLE_GV    (JI)      
        DEA%XH_GV     (IMASK) = DEA%XH_GV     (IMASK) + PK%XPATCH(JI) * DEAK%XH_GV     (JI)      

        DEA%XLE_GN    (IMASK) = DEA%XLE_GN    (IMASK) + PK%XPATCH(JI) * DEAK%XLE_GN    (JI)
        DEA%XEVAP_GN  (IMASK) = DEA%XEVAP_GN  (IMASK) + PK%XPATCH(JI) * DEAK%XEVAP_GN  (JI)
        DEA%XH_GN     (IMASK) = DEA%XH_GN     (IMASK) + PK%XPATCH(JI) * DEAK%XH_GN     (JI)      
        DEA%XSR_GN    (IMASK) = DEA%XSR_GN    (IMASK) + PK%XPATCH(JI) * DEAK%XSR_GN    (JI)
        DEA%XSWDOWN_GN(IMASK) = DEA%XSWDOWN_GN(IMASK) + PK%XPATCH(JI) * DEAK%XSWDOWN_GN(JI)
        DEA%XLWDOWN_GN(IMASK) = DEA%XLWDOWN_GN(IMASK) + PK%XPATCH(JI) * DEAK%XLWDOWN_GN(JI)

        DEA%XEVAP_G   (IMASK) = DEA%XEVAP_G   (IMASK) + PK%XPATCH(JI) * DEAK%XEVAP_G   (JI)
        DEA%XLE_CA    (IMASK) = DEA%XLE_CA    (IMASK) + PK%XPATCH(JI) * DEAK%XLE_CA    (JI)
        DEA%XH_CA     (IMASK) = DEA%XH_CA     (IMASK) + PK%XPATCH(JI) * DEAK%XH_CA     (JI)

        DEA%XSWNET_V  (IMASK) = DEA%XSWNET_V  (IMASK) + PK%XPATCH(JI) * DEAK%XSWNET_V(JI)
        DEA%XSWNET_G  (IMASK) = DEA%XSWNET_G  (IMASK) + PK%XPATCH(JI) * DEAK%XSWNET_G(JI)
        DEA%XSWNET_N  (IMASK) = DEA%XSWNET_N  (IMASK) + PK%XPATCH(JI) * DEAK%XSWNET_N(JI)
        DEA%XSWNET_NS (IMASK) = DEA%XSWNET_NS (IMASK) + PK%XPATCH(JI) * DEAK%XSWNET_NS(JI)
        DEA%XLWNET_V  (IMASK) = DEA%XLWNET_V  (IMASK) + PK%XPATCH(JI) * DEAK%XLWNET_V(JI)
        DEA%XLWNET_G  (IMASK) = DEA%XLWNET_G  (IMASK) + PK%XPATCH(JI) * DEAK%XLWNET_G(JI)
        DEA%XLWNET_N  (IMASK) = DEA%XLWNET_N  (IMASK) + PK%XPATCH(JI) * DEAK%XLWNET_N(JI)
        !
     ENDDO
     !
  ENDDO
  !
ENDIF
!
! Isba water budget and reservoir time tendencies
!
IF(DE%LWATER_BUDGET)THEN
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
        DEA%XDWG    (IMASK) = DEA%XDWG    (IMASK) + PK%XPATCH(JI) * DEAK%XDWG    (JI)
        DEA%XDWGI   (IMASK) = DEA%XDWGI   (IMASK) + PK%XPATCH(JI) * DEAK%XDWGI   (JI)
        DEA%XDWR    (IMASK) = DEA%XDWR    (IMASK) + PK%XPATCH(JI) * DEAK%XDWR    (JI)
        DEA%XDSWE   (IMASK) = DEA%XDSWE   (IMASK) + PK%XPATCH(JI) * DEAK%XDSWE   (JI)
        DEA%XDSWFREE(IMASK) = DEA%XDSWFREE(IMASK) + PK%XPATCH(JI) * DEAK%XDSWFREE(JI)
        DEA%XWATBUD (IMASK) = DEA%XWATBUD (IMASK) + PK%XPATCH(JI) * DEAK%XWATBUD (JI)
        !
     ENDDO
     !
  ENDDO
  !
ENDIF
!
! Isba energy budget and time tendencies
!
IF(DE%LENERGY_BUDGET)THEN
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
        DEA%XNRJBUD       (IMASK) = DEA%XNRJBUD       (IMASK) + PK%XPATCH(JI) * DEAK%XNRJBUD       (JI)
        DEA%XNRJBUD_SFC   (IMASK) = DEA%XNRJBUD_SFC   (IMASK) + PK%XPATCH(JI) * DEAK%XNRJBUD_SFC   (JI)
        DEA%XGRNDFLUX     (IMASK) = DEA%XGRNDFLUX     (IMASK) + PK%XPATCH(JI) * DEAK%XGRNDFLUX     (JI)
        DEA%XRESTORE      (IMASK) = DEA%XRESTORE      (IMASK) + PK%XPATCH(JI) * DEAK%XRESTORE      (JI)
        DEA%XRESTOREN     (IMASK) = DEA%XRESTOREN     (IMASK) + PK%XPATCH(JI) * DEAK%XRESTOREN     (JI)
        DEA%XDELHEATG     (IMASK) = DEA%XDELHEATG     (IMASK) + PK%XPATCH(JI) * DEAK%XDELHEATG     (JI)
        DEA%XDELHEATN     (IMASK) = DEA%XDELHEATN     (IMASK) + PK%XPATCH(JI) * DEAK%XDELHEATN     (JI)
        DEA%XDELPHASEG    (IMASK) = DEA%XDELPHASEG    (IMASK) + PK%XPATCH(JI) * DEAK%XDELPHASEG    (JI)
        DEA%XDELPHASEN    (IMASK) = DEA%XDELPHASEN    (IMASK) + PK%XPATCH(JI) * DEAK%XDELPHASEN    (JI)
        DEA%XDELHEATG_SFC (IMASK) = DEA%XDELHEATG_SFC (IMASK) + PK%XPATCH(JI) * DEAK%XDELHEATG_SFC (JI)
        DEA%XDELHEATN_SFC (IMASK) = DEA%XDELHEATN_SFC (IMASK) + PK%XPATCH(JI) * DEAK%XDELHEATN_SFC (JI)
        DEA%XDELPHASEG_SFC(IMASK) = DEA%XDELPHASEG_SFC(IMASK) + PK%XPATCH(JI) * DEAK%XDELPHASEG_SFC(JI)
        DEA%XDELPHASEN_SFC(IMASK) = DEA%XDELPHASEN_SFC(IMASK) + PK%XPATCH(JI) * DEAK%XDELPHASEN_SFC(JI)        
        !
     ENDDO
     !
  ENDDO
  !
ENDIF
!
IF (LHOOK) CALL DR_HOOK('AVERAGE_DIAG_EVAP_ISBA_N:MAKE_AVERAGE_EVAP',1,ZHOOK_HANDLE)
!
END SUBROUTINE MAKE_AVERAGE_EVAP
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE AVERAGE_DIAG_EVAP_ISBA_n
