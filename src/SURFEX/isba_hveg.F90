!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE ISBA_HVEG(IO, KK, PK, PEK, TPTIME, PPOI, PABC, PIACAN,             &
                      PTSTEP, PUREF, PTA, PQA, PEXNS, PRHOA, PPS, PZENITH,          &
                      PABS_SW, PABS_LW, PVMOD, PCSP, PRESP_BIOMASS_INST, PGPP, PHV, &
                      PLETR_HVEG, PRN_HVEG, PH_HVEG, PLE_HVEG, PG_HVEG, NPAR_VEG_IRR_USE)
!     ##########################################################################
!
!
!!****  *ISBA*  
!!
!!    PURPOSE
!!    -------
!       Monitor for the calculation of the surface fluxes and of the
!     prognostic variables of high vegetation only (for TEB urban scheme)
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
!!    Noilhan and Planton (1989)
!!      
!!    AUTHOR
!!    ------
!!      V. Masson           * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original 12/2020  (from isba.F90)
!!
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
USE MODD_ISBA_n, ONLY : ISBA_K_t, ISBA_P_t, ISBA_PE_t
!
USE MODD_CO2V_PAR,       ONLY : XMC, XMCO2, XPCCO2
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODD_ISBA_PAR,       ONLY : XRS_MAX
USE MODD_CSTS,           ONLY : XCPD, XLVTT, XSTEFAN, XSURF_EPSILON
!
USE MODD_TYPE_DATE_SURF, ONLY : DATE_TIME
!
USE MODI_SOILSTRESS
USE MODI_VEG
USE MODI_COTWORES
USE MODI_WIND_THRESHOLD
USE MODI_SURFACE_AERO_COND
!
!
USE MODE_MEB
USE MODE_THERMOS
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!              -------------------------
!
!
!* general variables
!  -----------------
!
TYPE(ISBA_OPTIONS_t), INTENT(INOUT) :: IO
TYPE(ISBA_K_t), INTENT(INOUT) :: KK
TYPE(ISBA_P_t), INTENT(INOUT) :: PK
TYPE(ISBA_PE_t), INTENT(INOUT) :: PEK
!
TYPE(DATE_TIME), INTENT(IN)       :: TPTIME     ! current date and time
!
REAL, DIMENSION(:),    INTENT(IN) :: PPOI       ! Gaussian weights (as above)
REAL, DIMENSION(:), INTENT(INOUT) :: PABC       ! abscissa needed for integration
!                                               ! of net assimilation and stomatal
!                                               ! conductance over canopy depth
REAL, DIMENSION(:,:),   INTENT(OUT) :: PIACAN   ! PAR in the canopy at different gauss level
REAL,                 INTENT(IN)  :: PTSTEP     ! timestep of the integration
REAL, DIMENSION(:),   INTENT(IN) :: PUREF       ! normal distance of the first
!                                               ! atmospheric level to the
!                                               ! orography
INTEGER,DIMENSION(:), INTENT(IN) :: NPAR_VEG_IRR_USE ! vegtype with irrigation
!
!* atmospheric variables
!  ---------------------
!
!            suffix 'A' stands for atmospheric variable at first model level
!            suffix 'S' stands for atmospheric variable at ground level
!
REAL, DIMENSION(:), INTENT(IN)  :: PTA        ! Temperature
REAL, DIMENSION(:), INTENT(IN)  :: PQA        ! specific humidity
REAL, DIMENSION(:), INTENT(IN)  :: PRHOA      ! air density
!
REAL, DIMENSION(:), INTENT(IN)  :: PPS        ! Pressure
REAL, DIMENSION(:), INTENT(IN)  :: PEXNS      ! Exner function
!
!
REAL, DIMENSION(:), INTENT(IN)  :: PZENITH    ! solar zenith angle
REAL, DIMENSION(:), INTENT(IN)  :: PABS_SW    ! absorbed solar   incoming radiation
REAL, DIMENSION(:), INTENT(IN)  :: PABS_LW    ! absorbed thermal incoming radiation
!
REAL, DIMENSION(:), INTENT(IN)  :: PVMOD      ! modulus of the wind
!                                             ! parallel to the orography
!
!* diagnostic variables for Carbon assimilation
!  --------------------------------------------
!
!
REAL, DIMENSION(:),    INTENT(IN) :: PCSP       ! atmospheric CO2 concentration
!                                                 [ppmm]=[kg CO2 / kg air]
REAL, DIMENSION(:,:),  INTENT(OUT) :: PRESP_BIOMASS_INST  ! instantaneous biomass respiration (kgCO2/kgair m/s)
!
REAL, DIMENSION(:),    INTENT(OUT) :: PGPP      ! Gross Primary Production                    (kgCO2/m2/s)
!
!* diagnostic variables
!  --------------------
!
REAL, DIMENSION(:),    INTENT(OUT) :: PHV       ! halfstead coefficient (relative humidity of vegetation)
!
!* High vegetation Energy Balance
!  ------------------------------
!
REAL, DIMENSION(:,:),  INTENT(OUT) :: PLETR_HVEG  ! Transpiration Heat Flux profile in the soil  (W/m2)
REAL, DIMENSION(:),    INTENT(OUT) :: PRN_HVEG  ! Net radiation              (W/m2)
REAL, DIMENSION(:),    INTENT(OUT) :: PH_HVEG   ! Sensible Heat Flux         (W/m2)
REAL, DIMENSION(:),    INTENT(OUT) :: PLE_HVEG  ! Latent Heat Flux           (W/m2)
REAL, DIMENSION(:),    INTENT(OUT) :: PG_HVEG   ! In-vegetation storage flux (W/m2)
!
!
!
!*      0.2    declarations of local variables
!  --------------------------------------------
!
REAL, DIMENSION(SIZE(PEK%XTV)) :: ZDELTA    ! fraction of the foliage
!                                           ! covered with intercepted water
REAL, DIMENSION(SIZE(PEK%XTV)) :: ZPSNV     ! fraction of the foliage
!                                           ! covered with intercepted snow
REAL, DIMENSION(SIZE(PEK%XTV)) :: ZFFV      ! fraction of the foliage flooded
REAL, DIMENSION(SIZE(PEK%XTV)) :: ZWR       ! water on the leaves (neglected)
REAL, DIMENSION(SIZE(PEK%XTV)) :: ZWSN      ! snow on the leaves (neglected)
REAL, DIMENSION(SIZE(PEK%XTV)) :: ZQSAT     ! expression for the saturation 
!                                           ! specific humidity 
REAL, DIMENSION(SIZE(PEK%XTV)) :: ZDQSAT    ! expression for the derivative of the saturation 
!                                           ! specific humidity relatively to temeprature
REAL, DIMENSION(SIZE(PEK%XTV)) :: ZDT       ! evolution of vegetation temperature during the time step
!
REAL, DIMENSION(SIZE(PEK%XTV)) :: ZSW_RAD   ! solar radiaiton reaching the high vegetation
REAL, DIMENSION(SIZE(PEK%XTV)) :: ZF2       ! water stress coefficient
!
REAL, DIMENSION(SIZE(PEK%XTV)) :: ZF5       ! water stress coefficient (based on F2)
!                                       ! to enforce Etv=>0 as F2=>0
!
REAL, DIMENSION(SIZE(PEK%XTV),SIZE(PABC)) :: ZIACAN_SHADE, ZIACAN_SUNLIT
!                                      ! absorbed PAR of each level within the
!                                      ! canopy - Split into shaded and SUNLIT
REAL, DIMENSION(SIZE(PEK%XTV),SIZE(PABC)) :: ZFRAC_SUN  ! fraction of sunlit leaves
!
REAL, DIMENSION(SIZE(PEK%XTV)) :: ZRS       ! stomatal resistance (s/m)
REAL, DIMENSION(SIZE(PEK%XTV)) :: ZCHEATV   ! Heat Capacity of vegetation
!
! for local Aerodynamical resistance calculation
REAL, DIMENSION(SIZE(PEK%XTV)) :: ZUREF     ! Characteristic distance between air and foliage
REAL, DIMENSION(SIZE(PEK%XTV)) :: ZZ0       ! Z0 of individual leaves
!
REAL, PARAMETER            :: ZHVLIM = 1.E-12 ! set halstead coef to 0 at this limit
REAL, DIMENSION(SIZE(PEK%XTV)) :: ZRI       ! Richardson number
REAL, DIMENSION(SIZE(PEK%XTV)) :: ZVMOD     ! Wind strenth at vegetation canopy height
REAL, DIMENSION(SIZE(PEK%XTV)) :: ZAC       ! Aerodynamical conductance (not used)
REAL, DIMENSION(SIZE(PEK%XTV)) :: ZCH       ! Exchange coefficient      (not used)
REAL, DIMENSION(SIZE(PEK%XTV)) :: ZVEG      ! Fraction of vegetation for fluxes computation
REAL, DIMENSION(SIZE(PEK%XTV)) :: ZHV_NIGHT ! weighting to remove LE from high vegetation at night


REAL, DIMENSION(SIZE(PEK%XTV)) :: ZDELTA_H  ! correction of Heat flux
REAL, DIMENSION(SIZE(PEK%XTV)) :: ZDELTA_LE ! correction of Latent flux
REAL, DIMENSION(SIZE(PEK%XTV)) :: ZDELTA_G  ! correction of storage flux
!
! ISBA-DF:
!                                                              
REAL, DIMENSION(SIZE(PEK%XWG,1),SIZE(PEK%XWG,2)) :: ZF2WGHT    ! water stress factor
!
LOGICAL, DIMENSION(SIZE(PEK%XTV))  :: GSHADE         ! mask for radiative canopy computations
!
INTEGER :: JL, JJ
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!*      1.0    Preliminaries
!              -------------
!
IF (LHOOK) CALL DR_HOOK('ISBA',0,ZHOOK_HANDLE)
!
ZF2       (:) = 0.
ZF5       (:) = 0.
ZF2WGHT   (:,:) = XUNDEF
!
ZDELTA = 0.      ! no water on leaves
ZPSNV  = 0.      ! no snow on leaves
ZFFV   = 0.      ! no flooding
ZWR    = 0.      ! no water on leaves
ZWSN   = 0.      ! no snow on leaves
GSHADE = .FALSE. ! No fine in-canopy radiative shortwave computations
!
ZSW_RAD = PABS_SW / (0.5*(PEK%XALBNIR_VEG + PEK%XALBVIS_VEG)) ! incoming solar radiation retrieved from absorbed one & albedo
!
!-------------------------------------------------------------------------------
!
!*      2.0    Plant stress due to soil water deficit
!              --------------------------------------
!
CALL SOILSTRESS(IO%CISBA, ZF2, KK, PK, PEK, ZF2WGHT, ZF5 )  
!
!-------------------------------------------------------------------------------
!
!*      3.0    Plant stress, stomatal resistance and, possibly, CO2 assimilation
!              --------------------------------------------------------------------
!
!
! Note that IO%LTR_ML must be set to .FALSE. for TEB vegetation.
!
IF (IO%CPHOTO=='NON') THEN
   CALL VEG(ZSW_RAD, PTA, PQA, PPS, PEK%XRGL, PEK%XLAI, PEK%XRSMIN, PEK%XGAMMA, ZF2, ZRS)
   PRESP_BIOMASS_INST(:,1) = 0.0
   PGPP(:) = 0.0
ELSE IF (MAXVAL(PEK%XGMES(:)).NE.XUNDEF .OR. MINVAL(PEK%XGMES(:)).NE.XUNDEF) THEN
   ZQSAT(:)=QSAT(PEK%XTV(:),PPS(:))  
   CALL COTWORES(PTSTEP, IO, GSHADE, PK, PEK, PK%XDMAX, PPOI, PCSP, PEK%XTV(:),   &
                 ZF2, ZSW_RAD, PQA, ZQSAT, ZPSNV, ZDELTA, PRHOA, PZENITH,         &
                 ZFFV, NPAR_VEG_IRR_USE, ZIACAN_SUNLIT, ZIACAN_SHADE, ZFRAC_SUN,  &
                 PIACAN, PABC, ZRS, PGPP, PRESP_BIOMASS_INST(:,1)                 )
ELSE
   PRESP_BIOMASS_INST(:,1) = 0.0
   PGPP(:) = 0.0
ENDIF
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!-------------------------------------------------------------------------------
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!
!*      4.0    Exchange coefficient of trees
!              -----------------------------
!
!
ZQSAT(:) = QSAT(PEK%XTV(:),PPS(:))
ZDQSAT(:) = DQSAT(PEK%XTV(:),PPS(:),ZQSAT)
!
!*              SURFACE AERODYNAMIC RESISTANCE FOR HEAT TRANSFERS
!               -------------------------------------------------
!
ZRI = 0. ! neutral effect on exchange coefficient assumed within urban canopy
!
! Computation of aerodynamical resistance does not take into account the whole tree, but
! only the local exchanges between air inside the tree canopy and the leaves
! One takes into account only the roughness length for heat.
!
ZZ0 = 0.03 ! typical value for roughness length for heat for leaves
ZUREF=0.1  ! typîcal distance between air and leaves for thermal exchanges
!
ZVMOD=PVMOD
!
CALL SURFACE_AERO_COND(ZRI, ZUREF, ZUREF, ZVMOD, ZZ0, ZZ0, ZAC,PEK%XRESA , ZCH, 'DEF')
!
!
!*            HALSTEAD COEFFICIENT (RELATIVE HUMIDITY OF THE VEGETATION)
!             ----------------------------------------------------------
!
PHV(:) = 0.   ! we do not consider the case of condensation on foliage (qa>qsat(Tv))
!
!             ! evaporation case
WHERE (ZQSAT(:)>PQA(:)) &
PHV(:) = PEK%XRESA/(PEK%XRESA+ZRS)

!
WHERE(PHV(:)<ZHVLIM) PHV(:)=0.0
!
!
!*            VEGETATION FRACTION (FOR THE CASE WITH VERY LOW LAI)
!             ----------------------------------------------------
!
! If very few vegetation exists, the turbulent and storage fluxes are ponderated by this coefficient.
! The radiation fluxes are already taking into account the few absorbed radiation through small Leaf Area Density
!
ZVEG(:) = 1.
WHERE (PEK%XLAI<0.5) ZVEG(:) = PEK%XLAI/0.5
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!-------------------------------------------------------------------------------
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!
!*      5.0    Energy balance of trees and Heat and Latent Fluxes
!              --------------------------------------------------
!
!* Flag to remove LE fluxes at night (continuous threshold between 0 and 20 W/m2 of absorbed radiation).
!
ZHV_NIGHT(:) = MAX(0., MIN(1., PABS_SW/20. ))
PHV = PHV * ZHV_NIGHT
!
! explicit formulation; stationarized
!
! PABS_SW(:)+PABS_LW(:) - 4. * XSTEFAN * PEK%XTV**3 * ZDT 
!
!   =
!
!   PRHOA * XCPD * ZVEG * (PEK%XTV + ZDT - PTA) / PEK%XRESA / PEXNS
!
! + PRHOA * XLVTT * ZVEG * PHV * (ZQSAT+ZDQSAT*ZDT - PQA) / PEK%XRESA
!
! + ZDT / PTSTEP * ZVEG * ZCHEATV
!
!             Energy balance
!             --------------
!
! This gives in implicit form:
!
ZCHEATV(:) = SFC_HEATCAP_VEG(ZWSN(:),ZWR(:),PEK%XCV) * PEK%XLAI
!
ZDT = (  PABS_SW(:)+PABS_LW(:)                                     &
       - PRHOA * XCPD * ZVEG * (PEK%XTV - PTA) / PEK%XRESA / PEXNS &
       - PRHOA * XLVTT * ZVEG * PHV * (ZQSAT - PQA) / PEK%XRESA  ) &
     /                                                             &
      ( 4. * XSTEFAN * PEK%XTV**3                                  &
       + PRHOA * XCPD * ZVEG / PEK%XRESA / PEXNS                   &
       + PRHOA * XLVTT * ZVEG * PHV * ZDQSAT / PEK%XRESA           &
       + ZVEG * ZCHEATV/PTSTEP                                     )
!
!
!             Energy fluxes
!             -------------
!
! Note that we put here the (small) implcit term due to long wave into sensible heat flux
! in order to conserve more easily the LW budget
!

    PRN_HVEG (:) = PABS_SW(:)+PABS_LW(:)
    
    PH_HVEG  (:) = PRHOA * XCPD * ZVEG * (PEK%XTV + ZDT - PTA) / PEK%XRESA / PEXNS &
                    + 4. * XSTEFAN * PEK%XTV**3 * ZDT

    PLE_HVEG (:) = PRHOA * XLVTT * ZVEG * PHV * (ZQSAT+ZDQSAT*ZDT - PQA) / PEK%XRESA

    PG_HVEG  (:) = ZVEG * ZCHEATV * ZDT / PTSTEP
!
!
!         Evolution of temperature of foliage
!         -----------------------------------
!
PEK%XTV = PEK%XTV + ZDT
!
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!-------------------------------------------------------------------------------
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!
!*      6.0    Post-treatment on fluxes to avoid extra-large positive latent and negative sensible fluxes
!              ------------------------------------------------------------------------------------------
!
!
! In extrelmely hot conditions, typically in the mid-afternoon in heat wave conditions, the foliage temperature can become much
! lower than air temperature. This creates foliage sensible fluxes that are very negative, and latent heat fluxes larger than
! net radiation. This seems irrealistic.
!
! Waiting for a more in depth analysis of this deviation, a post-treatment is applied on the fluxes when the sensible heat flux
! is negative (foliage temperature smaller than 1K below air temperature). Sensible heat flux is then reduced in intensity.
! The remaining energy goes into latent and storage flux (storage flux to conserve energy stored, latent heat takes the residual).
!
ZDELTA_H = 0.
ZDELTA_G = 0.
ZDELTA_LE= 0.
ZDT = 0.
!
WHERE (PLE_HVEG>PRN_HVEG .AND. PEK%XTV<PTA-1.)
  ! A positive change in temperature of foliage is applied authoritatively to limit the foliage temperature 
  ! not to be too far below air temeprature
  ZDT = (PTA - 1.) - PEK%XTV
  ZDELTA_H = PRHOA * XCPD * ZVEG * (ZDT ) / PEK%XRESA / PEXNS
  ZDELTA_G = ZVEG * ZCHEATV * ZDT / PTSTEP
  ZDELTA_LE= - ZDELTA_H - ZDELTA_G
END WHERE
!
PH_HVEG (:) = PH_HVEG (:) + ZDELTA_H (:)
PLE_HVEG(:) = PLE_HVEG(:) + ZDELTA_LE(:)
PG_HVEG (:) = PG_HVEG (:) + ZDELTA_G (:)
PEK%XTV = PEK%XTV + ZDT
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!-------------------------------------------------------------------------------
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!
!*      7.0    Profile of water extraction by trees in the soil
!              ------------------------------------------------
!
IF (IO%CISBA=='DIF') THEN
   !
   PLETR_HVEG(:,:) = 0.
   WHERE(ZF2(:).EQ.0.) PLETR_HVEG(:,1) = PLE_HVEG(:)
   DO JL=1,SIZE(PLETR_HVEG,2)
      WHERE (ZF2(:).GT.0.) PLETR_HVEG(:,JL) = PLE_HVEG(:) * ZF2WGHT(:,JL) / ZF2(:)
   ENDDO
   !
ELSE
  PLETR_HVEG(:,:) = 0.
  PLETR_HVEG(:,2) = PLE_HVEG(:)
END IF
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!
IF (LHOOK) CALL DR_HOOK('ISBA',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE ISBA_HVEG
