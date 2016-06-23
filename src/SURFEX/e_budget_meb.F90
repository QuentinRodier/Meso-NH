!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ##########################################################################
      SUBROUTINE E_BUDGET_MEB(HISBA,HCPSURF,PTSTEP,                                           &
           PPS,PCG,PCT,PCV,PWRVN,PWR,                                                         &
           PTDEEP_A,PTDEEP_B,PD_G,PSOILCONDZ,PSOILHCAPZ,                                      &  
           PSNOWDZ,PSNOWCONDZ,PSNOWHCAPZ,                                                     &  
           PSWNET_V,PSWNET_G,PSWNET_N,PTAU_N,                                                 &
           PLWNET_V,PLWNET_G,PLWNET_N,                                                        &
           PLWNET_V_DTV,PLWNET_V_DTG,PLWNET_V_DTN,                                            &
           PLWNET_G_DTV,PLWNET_G_DTG,PLWNET_G_DTN,                                            &
           PLWNET_N_DTV,PLWNET_N_DTG,PLWNET_N_DTN,                                            &
           PPEW_A_COEF,PPEW_B_COEF,PPET_A_COEF,PPEQ_A_COEF,PPET_B_COEF,PPEQ_B_COEF,           &
           PTHRMA_TA,PTHRMB_TA,PTHRMA_TC,PTHRMB_TC,                                           &
           PTHRMA_TG,PTHRMB_TG,PTHRMA_TV,PTHRMB_TV,PTHRMA_TN,PTHRMB_TN,                       &
           PQSAT_G,PQSAT_V,PQSATI_N,                                                          &
           PFF,PFFROZEN,PPSN,PPSNA,PPSNCV,                                                    &
           PCHEATV,PCHEATG,PCHEATN,                                                           &
           PLEG_DELTA,PLEGI_DELTA,PHUG,PHUGI,PHVG,PHVN,PFROZEN1,                              &
           PFLXC_C_A,PFLXC_G_C,PFLXC_VG_C,PFLXC_VN_C,PFLXC_N_C,PFLXC_N_A,                     &
           PFLXC_MOM,                                                                         &
           PTG,PTV,PTN,                                                                       &
           PFLXC_V_C,PHVGS,PHVNS,                                                             & 
           PDQSAT_G,PDQSAT_V,PDQSATI_N,                                                       & 
           PTC,PQC,PTA_IC,PQA_IC,PUSTAR2_IC,PVMOD,                                            &
           PDELTAT_G,PDELTAT_V,PDELTAT_N,PGRNDFLUX,PCPS,PLVTT,PLSTT,                          &
           PHPSNOW,PMELTADV,PRESTORE,PDEEP_FLUX,                                              &
           PDELHEATV_SFC,PDELHEATG_SFC,PDELHEATG                                              )
!     ##########################################################################
!
!!****  *E_BUDGET*  
!!
!!    PURPOSE
!!    -------
!
!     Calculates the implicit simulataneous solution of multiple surface
!     energy budgets (understory vegetation-soil composite, vegetation canopy
!     and explicit snowpack) and sub-surface soil temperatures. Canopy
!     air space temperature and specific humidities are also diagnosed.
!     Fully implicit coupling with the atmosphere allowed. 
!     Note that a 'test' snow temperature is computed herein which is just
!     use to compute the surface fluxes. These fluxes are then used within
!     the snow scheme as an upper boundary condition to compute the snow 
!     temperatures. In theory, they should be very close, but this method is 
!     done to ensure a high level of energy conservation.
!         
!     
!!**  METHOD
!!    ------
!
!     1- compute coefficients for each energy budget
!     2- solve the equations for snow, vegetation canopy and soil-understory
!        vegetation composite
!     3- solve soil T profile
!     4- diagnose canopy air space variables, and lowest level atmospheric
!        state variable values at t+dt
!
!!    EXTERNAL
!!    --------
!!
!!    none
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------ 
!!
!!
!!      
!!    REFERENCE
!!    ---------
!!
!!    Noilhan and Planton (1989)
!!    Belair (1995)
!!    * to be done * (2011)
!!      
!!    AUTHOR
!!    ------
!!
!!	A. Boone           * CNRM-GAME, Meteo-France *
!!      P. Samuelsson      * SMHI *
!!      S. Gollvik         * SMHI * 
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    22/01/11 
!!                  10/10/14 (A. Boone) Removed understory vegetation
!!
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_CSTS,                    ONLY : XLVTT, XLSTT, XTT, XCPD, XCPV, XCL, &
                                         XDAY, XPI
USE MODD_SURF_ATM,                ONLY : LCPL_ARP
USE MODD_SURF_PAR,                ONLY : XUNDEF
USE MODD_SNOW_METAMO,             ONLY : XSNOWDZMIN
!
USE MODE_THERMOS
USE MODE_MEB,                     ONLY : SFC_HEATCAP_VEG
!
USE MODI_TRIDIAG_GROUND_RM_COEFS
USE MODI_TRIDIAG_GROUND_RM_SOLN
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
 CHARACTER(LEN=*),      INTENT(IN)  :: HISBA     ! type of ISBA version:
!                                               ! '2-L' (default)
!                                               ! '3-L'
!                                               ! 'DIF'
!
 CHARACTER(LEN=*),      INTENT(IN)  :: HCPSURF   ! Specific heat
!                                               ! 'DRY' = dry Cp
!                                               ! 'HUM' = humid Cp fct of qc for MEB
!
REAL,                 INTENT(IN)   :: PTSTEP
!                                     PTSTEP = timestep of the integration (s)
!
REAL, DIMENSION(:),   INTENT(IN)   :: PCT, PCV, PWRVN, PWR, PCG
!                                     PCG     = area-averaged soil heat capacity (m2 K J-1)
!                                     PCT     = surface understory or composite thermal inertia (m2 K J-1)
!                                     PCV     = vegetation canopy thermal inertia (m2 K J-1)
!                                     PWRVN   = liquid water equivalent snow  intercepted on the canopy (kg m-2)
!                                     PWR     = liquid water intercepted on the canopy (kg m-2)
!
REAL, DIMENSION(:), INTENT(IN)     :: PTDEEP_A, PTDEEP_B
!                                      PTDEEP_A = Deep soil temperature
!                                                 coefficient depending on flux
!                                      PTDEEP_B = Deep soil temperature (prescribed)
!                                               which models heating/cooling from
!                                               below the diurnal wave penetration
!                                               (surface temperature) depth. If it
!                                               is FLAGGED as undefined, then the zero
!                                               flux lower BC is applied.
!                                      Tdeep = PTDEEP_B + PTDEEP_A * PDEEP_FLUX
!                                              (with PDEEP_FLUX in W/m2)
!
REAL, DIMENSION(:),   INTENT(IN)   :: PPS
!                                     PPS  = surface pressure (Pa)
!
REAL, DIMENSION(:,:), INTENT(IN)   :: PD_G, PSOILCONDZ, PSOILHCAPZ
!                                     PD_G       = soil layer depth      (m)
!                                     PSOILCONDZ = soil thermal conductivity (W m-1 K-1)
!                                     PSOILHCAPZ = soil heat capacity        (J m-3 K-1)
!
REAL, DIMENSION(:,:), INTENT(IN)   :: PSNOWDZ, PSNOWCONDZ, PSNOWHCAPZ
!                                     PSNOWDZ    = snow layer thickness      (m)
!                                     PSNOWCONDZ = snow thermal conductivity (W m-1 K-1)
!                                     PSNOWHCAPZ = snow heat capacity        (J m-3 K-1)
!
REAL, DIMENSION(:),   INTENT(IN)   :: PSWNET_V, PSWNET_G, PSWNET_N
!                                     PSWNET_G = Understory-ground net SW radiation explicit term (W m-2)
!                                     PSWNET_V = Vegetation canopy net SW radiation explicit term (W m-2)
!                                     PSWNET_N = Ground-based snow net SW radiation explicit term (W m-2)
!
REAL, DIMENSION(:,:), INTENT(IN)   :: PTAU_N
!                                     PTAU_N   = shortwave radiation transmission through (at the base of)
!                                                each snow layer. Implicitly PTAU_N(0) = 1 (-)
!                                                It decreases with depth to zero somewhere in a deep snowpack,
!                                                but can be above 0 at snowpack base for shallow snow, then
!                                                it is presumed to go into the uppermost soil layer.
!
REAL, DIMENSION(:),   INTENT(IN)   :: PLWNET_V, PLWNET_G, PLWNET_N
!                                     PLWNET_G = Understory-ground net LW radiation explicit term (W m-2)
!                                     PLWNET_V = Vegetation canopy net LW radiation explicit term (W m-2)
!                                     PLWNET_N = Ground-based snow net LW radiation explicit term (W m-2)
!
REAL, DIMENSION(:),   INTENT(IN)   :: PLWNET_V_DTV, PLWNET_V_DTG, PLWNET_V_DTN
!                                     PLWNET_V_DTV, PLWNET_V_DTG, PLWNET_V_DTN = Vegetation canopy net LW radiation 
!                                     derivatives w/r/t surface temperature(s) (W m-2 K-1)
!
REAL, DIMENSION(:),   INTENT(IN)   :: PLWNET_G_DTV, PLWNET_G_DTG, PLWNET_G_DTN
!                                     PLWNET_G_DTV, PLWNET_G_DTG, PLWNET_G_DTN = Understory-ground net LW radiation 
!                                          derivatives w/r/t surface temperature(s) (W m-2 K-1)
!
REAL, DIMENSION(:),   INTENT(IN)   :: PLWNET_N_DTV, PLWNET_N_DTG, PLWNET_N_DTN
!                                     PLWNET_N_DTV, PLWNET_N_DTG, PLWNET_N_DTN = Ground-based snow net LW radiation 
!                                          derivatives w/r/t surface temperature(s) (W m-2 K-1)
!
REAL, DIMENSION(:),   INTENT(IN)   :: PTHRMA_TA, PTHRMB_TA, PTHRMA_TC, PTHRMB_TC,                     &
                                      PTHRMA_TG, PTHRMB_TG, PTHRMA_TV, PTHRMB_TV, PTHRMA_TN, PTHRMB_TN
!                                     PTHRMA_TA                                                    (J kg-1 K-1)
!                                     PTHRMB_TA = linear transform coefficinets for atmospheric
!                                                 thermal variable for lowest atmospheric level.   (J kg-1)
!                                                 Transform T to dry static energy or enthalpy.
!                                     PTHRMA_TC                                                    (J kg-1 K-1)
!                                     PTHRMB_TC = linear transform coefficinets for atmospheric
!                                                 thermal variable for canopy air                  (J kg-1)
!                                                 Transform T to dry static energy or enthalpy.
!                                     PTHRMA_TG,V,N                                                (J kg-1 K-1)
!                                     PTHRMB_TG,V,N = linear transform coefficinets for atmospheric
!                                                 thermal variable for surfaces (G, V, and N)      (J kg-1)
!                                                 Transform T to dry static energy or enthalpy.
!
REAL, DIMENSION(:),   INTENT(IN)   :: PPET_A_COEF, PPEQ_A_COEF, PPET_B_COEF, PPEQ_B_COEF,             &
                                      PPEW_A_COEF, PPEW_B_COEF
!                                     PPEW_A_COEF = wind atmospheric coupling coefficient 
!                                     PPEW_B_COEF = wind atmospheric coupling coefficient 
!                                     PPET_A_COEF = A-air temperature atmospheric coupling coefficient 
!                                     PPET_B_COEF = B-air temperature atmospheric coupling coefficient
!                                     PPEQ_A_COEF = A-air specific humidity atmospheric coupling coefficient
!                                     PPEQ_B_COEF = B-air specific humidity atmospheric coupling coefficient
!
REAL, DIMENSION(:),   INTENT(IN)   :: PQSAT_G, PQSAT_V, PQSATI_N
!                                     PQSAT_G  = saturation specific humidity for understory surface    (kg kg-1)
!                                     PQSAT_V  = saturation specific humidity for the vegetation canopy (kg kg-1)
!                                     PQSATI_N = saturation specific humidity over ice for the snowpack (kg kg-1)
!
REAL, DIMENSION(:),   INTENT(IN)   :: PFF, PFFROZEN, PPSN, PPSNA, PPSNCV
!                                     PFF      = total flooded fraction                                        (-) 
!                                     PFFROZEN = total frozen flooded fraction                                 (-) 
!                                     PPSN     = fraction of snow on ground and understory vegetation          (-)
!                                     PPSNA    = fraction of vegetation canopy buried by ground-based snowpack (-)
!                                     PPSNCV   = fraction of vegetation canopy covered by intercepted snow     (-)
!
REAL, DIMENSION(:),   INTENT(IN)   :: PLEG_DELTA, PLEGI_DELTA, PHUG, PHUGI, PHVG, PHVN, PFROZEN1
!                                     PHUG        = relative humidity of the surface soil                             (-)  
!                                     PHUGI       = relative humidity of the frozen surface soil                      (-)                         
!                                     PHVG        = Halstead coefficient of non-buried (snow) canopy vegetation       (-)                         
!                                     PHVN        = Halstead coefficient of paritally-buried (snow) canopy vegetation (-)  
!                                     PLEG_DELTA  = soil evaporation delta fn                                         (-)
!                                     PLEGI_DELTA = soil sublimation delta fn                                         (-)
!                                     PFROZEN1    = fraction of surface soil layer which is frozen                    (-)
!
REAL, DIMENSION(:),   INTENT(IN)   :: PFLXC_C_A, PFLXC_G_C, PFLXC_VG_C, PFLXC_VN_C, PFLXC_N_C, PFLXC_N_A, PFLXC_MOM
!                                     PFLXC_C_A  = Flux form heat transfer coefficient: canopy air to atmosphere (kg m-2 s-1)
!                                     PFLXC_G_C  = As above, but for : ground-understory to canopy air           (kg m-2 s-1)
!                                     PFLXC_VG_C = As above, but for : non-snow buried canopy to canopy air      (kg m-2 s-1)
!                                     PFLXC_VN_C = As above, but for : partially snow-buried canopy air to canopy 
!                                                  air                                                           (kg m-2 s-1)
!                                     PFLXC_N_C  = As above, but for : ground-based snow to atmosphere           (kg m-2 s-1)
!                                     PFLXC_N_A  = As above, but for : ground-based snow to canopy air           (kg m-2 s-1)
!                                     PFLXC_MOM  = flux form drag transfer coefficient: canopy air to atmosphere (kg m-2 s-1)
!
REAL, DIMENSION(:,:), INTENT(INOUT):: PTN
!                                     PTN    = Ground-based snow temperature profile (K)
!
REAL, DIMENSION(:),   INTENT(INOUT):: PTV
!                                     PTV    = Vegetation canopy temperature (K)
!
REAL, DIMENSION(:,:), INTENT(INOUT):: PTG
!                                     PTG    = Soil temperature profile (K)
!
REAL, DIMENSION(:),   INTENT(OUT)  :: PCHEATV, PCHEATG, PCHEATN
!                                     PCHEATV = Vegetation canopy *effective surface* heat capacity          (J m-2 K-1)
!                                     PCHEATG = Understory-ground *effective surface* heat capacity (J m-2 K-1)
!                                     PCHEATN = Ground-based snow *effective surface* heat capacity          (J m-2 K-1)
!
REAL, DIMENSION(:),   INTENT(OUT)  :: PDQSAT_G, PDQSAT_V, PDQSATI_N
!                                     PQSAT_G  = saturation specific humidity derivative for understory 
!                                                surface               (kg kg-1 K-1)
!                                     PQSAT_V  = saturation specific humidity derivative for the vegetation 
!                                                canopy                (kg kg-1 K-1)
!                                     PQSATI_N = saturation specific humidity derivative over ice for the 
!                                                ground-based snowpack (kg kg-1 K-1)
!
REAL, DIMENSION(:),   INTENT(OUT)  :: PFLXC_V_C, PHVGS, PHVNS
!                                     PFLXC_V_C = Flux form heat transfer coefficient: total canopy (partially 
!                                                 snow-buried and non-buried parts) to canopy air               (kg m-2 s-1)
!                                     PHVGS     = Dimensionless pseudo humidity factor for computing vapor
!                                                 fluxes from the non-buried part of the canopy to the canopy air    (-)
!                                     PHVNS     = Dimensionless pseudo humidity factor for computing vapor
!                                                 fluxes from the partly-buried part of the canopy to the canopy air (-)
!
REAL, DIMENSION(:),   INTENT(OUT)  :: PTC, PQC, PTA_IC, PQA_IC, PUSTAR2_IC, PVMOD
!                                     PTC         = Canopy air space temperature       (K)
!                                     PQC         = Canopy air space specific humidity (kg kg-1)
!                                     PTA_IC      = Near-ground air temperature        (K)
!                                     PQA_IC      = Near-ground air specific humidity  (kg kg-1)
!                                     PUSTAR2_IC  = Surface friction velocity squared  (m2 s-2)
!                                                   (modified if implicit coupling with
!                                                   atmosphere used)
!                                     PVMOD       = lowest level wind speed            (m s-1)
!
REAL, DIMENSION(:),   INTENT(OUT)  :: PDELTAT_V, PDELTAT_N, PDELTAT_G
!                                     PDELTAT_V = Time change in vegetation canopy temperature (K)
!                                     PDELTAT_N = Time change in snowpack surface temperature  (K)
!                                     PDELTAT_G = Time change in soil surface temperature      (K)
!
REAL, DIMENSION(:),   INTENT(OUT)  :: PGRNDFLUX 
!                                     PGRNDFLUX = Flux between snowpack base and ground surface (W m-2)
!
REAL, DIMENSION(:),   INTENT(OUT)  :: PCPS, PLVTT, PLSTT
!                                     PCPS = specific heat capacity of the surface (J kg-1 K-1) 
!                                     PLVTT= latent heat of vaporization (J kg-1)
!                                     PLSTT= latent heat of sublimation  (J kg-1)
!
REAL, DIMENSION(:),   INTENT(OUT)  :: PHPSNOW, PMELTADV, PRESTORE
!                                     PHPSNOW = Precipitation heating term for ground-based snowpack (W m-2)
!                                     PMELTADV= Heating resulting when meltwater enters soil at sub-freezing
!                                               temperatures (W m-2)
!                                     PRESTORE= flux between surface and sub-surface in soil (W m-2)
!
REAL, DIMENSION(:),  INTENT(OUT)   :: PDEEP_FLUX ! Heat flux at bottom of ISBA (W/m2)
!
REAL, DIMENSION(:),  INTENT(OUT)   :: PDELHEATV_SFC, PDELHEATG_SFC, PDELHEATG
!                                     PDELHEATV_SFC = change in heat storage of the vegetation canopy layer over the current time step (W m-2)
!                                     PDELHEATG_SFC = change in heat storage of the surface soil layer over the current time step (W m-2)
!                                     PDELHEATG     = change in heat storage of the entire soil column over the current time step (W m-2)
!
!*      0.2    declarations of local variables
!
!
INTEGER                                   :: JNSNOW, JNGRND, JNPTS, JJ, JK, JL
!
REAL                                      :: ZHNS
!
REAL, DIMENSION(SIZE(PTG,1),SIZE(PTG,2))  :: ZTGO
!
REAL, DIMENSION(SIZE(PTN,1),SIZE(PTN,2))  :: ZTNO
!
REAL, DIMENSION(SIZE(PTG,1))              :: ZTVO
!
REAL, DIMENSION(SIZE(PPS))                :: ZHN, ZHS, ZHVS
!
REAL, DIMENSION(SIZE(PPS))                :: ZPSNAG, ZWORK, ZFFF, ZGCOND1
!
REAL, DIMENSION(SIZE(PPS))                :: ZPET_A_COEF_P, ZPET_B_COEF_P, ZPET_C_COEF_P
!
REAL, DIMENSION(SIZE(PPS))                :: ZPEQ_A_COEF_P, ZPEQ_B_COEF_P, ZPEQ_C_COEF_P
!
REAL, DIMENSION(SIZE(PPS))                :: ZCOEFA_TC, ZCOEFB_TC, ZCOEFC_TC, ZCOEFD_TC,          &
                                             ZCOEFA_QC, ZCOEFB_QC, ZCOEFC_QC, ZCOEFD_QC
!
REAL, DIMENSION(SIZE(PPS))                :: ZBETA_V, ZALPHA_V, ZGAMMA_V,                         &
                                             ZBETA_G, ZALPHA_G, ZGAMMA_G,                         &
                                             ZBETA_N, ZALPHA_N, ZGAMMA_N,                         &
                                             ZBETA_P_V, ZALPHA_P_V, ZBETA_P_N, ZALPHA_P_N
!
REAL, DIMENSION(SIZE(PPS))                :: ZRNET_NN, ZRNET_N_DTNN, ZRNET_N_DTGN, ZRNET_N_DTVN
!
REAL, DIMENSION(SIZE(PPS))                :: ZVMOD, ZUSTAR2, ZPSNA
!
REAL, DIMENSION(SIZE(PPS))                :: ZTCONDA_DELZ_G, ZTCONDA_DELZ_N, ZTCONDA_DELZ_NG
!
REAL, DIMENSION(SIZE(PTG,1),SIZE(PTG,2))  :: ZSOIL_COEF_A, ZSOIL_COEF_B
!
REAL, DIMENSION(SIZE(PSNOWDZ,1),SIZE(PSNOWDZ,2)):: ZSNOW_COEF_A, ZSNOW_COEF_B, ZSNOWDZ
!
REAL, DIMENSION(SIZE(PD_G,1),SIZE(PD_G,2)+SIZE(PSNOWDZ,2)) :: ZD, ZT, ZHCAPZ, ZCONDZ,         &
                                             ZCOEF_A, ZCOEF_B, ZSOURCE
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
REAL, PARAMETER                          :: ZERTOL       = 1.0E-6  ! -
REAL, PARAMETER                          :: ZERTOL_FLX_C = 1.0E-12 ! -
!
!-------------------------------------------------------------------------------
!
!*       0.     Initialization:
!               ---------------
!
IF (LHOOK) CALL DR_HOOK('E_BUDGET_MEB',0,ZHOOK_HANDLE)
!
ZTGO(:,:) = PTG(:,:)
ZTNO(:,:) = PTN(:,:)
ZTVO(:)   = PTV(:)

! To prevent possible numerical problems in the limit as psna==>1, we
! limit the value for certain computations:

ZPSNA(:)  = MIN(1.0-ZERTOL, PPSNA(:))

ZSNOWDZ(:,:) = 0.0 ! local working variable

JNSNOW       = SIZE(PTN,2)
JNGRND       = SIZE(PTG,2)
JNPTS        = SIZE(PTG,1)

! sub-surface / surface coupling coefficients:

ZSOIL_COEF_A(:,:) = 0.0 
ZSOIL_COEF_B(:,:) = 0.0
ZSNOW_COEF_A(:,:) = 0.0
ZSNOW_COEF_B(:,:) = 0.0

!-------------------------------------------------------------------------------
!
!*       1.     Some variables/coefficients needed for coupling or solution
!               -----------------------------------------------------------
! - Effective Surface heat capacities for each energy budget (J m-2 K-1)
!
PCHEATG(:)    = 1/PCT(:)                                          ! understory soil/floodplain
!
PCHEATV(:)    = SFC_HEATCAP_VEG(PWRVN,PWR,PCV)                    ! vegetation canopy heat capacity
!
PCHEATN(:)    = PSNOWHCAPZ(:,1)*PSNOWDZ(:,1)                      ! snow surface layer
!
! - Specific humidity derivatives (kg kg-1 K-1)
!
PDQSAT_G(:)   = DQSAT(ZTGO(:,1),  PPS(:),PQSAT_G(:)  )
PDQSAT_V(:)   = DQSAT(ZTVO(:),    PPS(:),PQSAT_V(:)  )
PDQSATI_N(:)  = DQSATI(ZTNO(:,1), PPS(:),PQSATI_N(:) )
!
! - Precipitation heating term for snowpack:
!   Rainfall renders it's heat to the snow when it enters
!   the snowpack:
! NOTE: for now set to zero because we'd need to remove this heat from
!       the atmosphere to conserve energy. This can be done, but should
!       be tested within an atmos model first probably...
!       ALSO, water should include canopy drip etc....but again, conservation
!       would need to be carefully accounted for. So for now within MEB,
!       set to 0. But code within MEB and ES can accept this term, so
!       if this process is added at some point, it would be here. 
!       Also, it is treated explicitly...this might also have to be changed
!       to implicit depending how this term is expressed. That would require
!       modification to this routine and flux routine:  but retain it here for possible
!       future implementation.
!
PHPSNOW(:)   = 0.0   ! W m-2
!
! Snowmeltwater advection which can heat uppermost soil layer if below freezing.
! NOTE this term is currently OFF for MEB since it would mean keeping meltwater
! flux from previous timestep (since snow called *after* energy budget). Also,
! the heat would need to be removed from snowpack to conserve energy (and currently
! not done) so we turn off for now (set to 0), but retain it here for possible
! future implementation.
!
PMELTADV(:)  = 0.0   ! W m-2
!
! - Implicit wind speed at lowest atmospheric level for this patch:
!
ZUSTAR2(:)   =    (PFLXC_MOM(:)*PPEW_B_COEF(:))/        &
              (1.0-PFLXC_MOM(:)*PPEW_A_COEF(:))
!
ZVMOD(:)     = PPEW_A_COEF(:)*ZUSTAR2(:) + PPEW_B_COEF(:)
!
PVMOD(:)     = MAX(ZVMOD,0.)
!
WHERE (PPEW_A_COEF(:) /= 0.) 
     ZUSTAR2(:) = MAX(0., ( PVMOD(:) - PPEW_B_COEF(:) ) / PPEW_A_COEF(:) )
END WHERE
!
PUSTAR2_IC(:)= ZUSTAR2(:) 
!
! NOTE: put in new option HIMPLICIT_WIND=='OLD' or 'NEW' ?
!
ZSOURCE(:,:) = 0. ! heat Eq source term initialization: c dz dT/dt = d/dz(k dT/dz) + Source
!
IF(HISBA == 'DIF')THEN
!
!*       2.a    Compute sub-surface soil coupling coefficients: upward sweep
!               ------------------------------------------------------------
! Upward sweep of the tridiagnal matrix using R&M method. Also compute
! interfacial thermal conductivity to layer thickness ratio (W m-2 K-1) 
! These coefficients are used to compute the temperature profile implicitly.
!
   CALL TRIDIAG_GROUND_RM_COEFS(PTSTEP,PD_G,ZTGO,PSOILHCAPZ,PSOILCONDZ,                &
        ZSOURCE(:,1:JNGRND),PTDEEP_A,PTDEEP_B,ZTCONDA_DELZ_G,ZSOIL_COEF_A,ZSOIL_COEF_B)
!
! Here we repeat this for the snowpack: but note, this is just
! to better estimate surface fluxes via the surface-sub surface heat
! Flux term (we do not actually solve
! the snow T profile here since fractional coverage not included). 
! We start from base of soil up through snow
! to surface. This gives coefficients which are fully implicit
! from the base of the soil to the snow surface, so numerically it is
! quite robust. Note, this only corresponds to the snow-covered part of grid box,
! so it is more accurate as the snow fraction approaches unity.
! Starting from snowpack surface downward to base of ground:
!
   ZSNOWDZ(:,:)           = MAX(XSNOWDZMIN, PSNOWDZ(:,:))
!

   JL                     = 1
   ZD(:,JL)               = ZSNOWDZ(:,1)
   ZT(:,JL)               = ZTNO(:,1)
   ZHCAPZ(:,JL)           = PSNOWHCAPZ(:,1)
   ZCONDZ(:,JL)           = PSNOWCONDZ(:,1)
   ZSOURCE(:,JL)          = 0.               ! Included in energy budget
   DO JK=2,JNSNOW
      JL                  = JL + 1
      DO JJ=1,JNPTS
         ZD(JJ,JL)        = ZSNOWDZ(JJ,JK)
         ZT(JJ,JL)        = ZTNO(JJ,JK)
         ZHCAPZ(JJ,JL)    = PSNOWHCAPZ(JJ,JK)
         ZCONDZ(JJ,JL)    = PSNOWCONDZ(JJ,JK)
         ZSOURCE(JJ,JL)   = PSWNET_N(JJ)*(PTAU_N(JJ,JK-1)-PTAU_N(JJ,JK))
      ENDDO
   ENDDO
   JL                     = JL + 1
   ZD(:,JL)               = PD_G(:,1)
   ZT(:,JL)               = ZTGO(:,1)
   ZHCAPZ(:,JL)           = PCHEATG(:)/PD_G(:,1)
   ZCONDZ(:,JL)           = PSOILCONDZ(:,1)
   ZSOURCE(:,JL)          = PSWNET_N(:)*PTAU_N(:,JNSNOW)
   DO JK=2,JNGRND
      JL                  = JL + 1
      DO JJ=1,JNPTS
         ZD(JJ,JL)        = PD_G(JJ,JK)
         ZT(JJ,JL)        = ZTGO(JJ,JK)
         ZHCAPZ(JJ,JL)    = PSOILHCAPZ(JJ,JK)
         ZCONDZ(JJ,JL)    = PSOILCONDZ(JJ,JK)
         ZSOURCE(JJ,JL)   = 0.
      ENDDO
   ENDDO
!
! Get coefficients from upward sweep (starting from soil base to snow surface):
!
   CALL TRIDIAG_GROUND_RM_COEFS(PTSTEP,ZD,ZT,ZHCAPZ,ZCONDZ,           &
        ZSOURCE,PTDEEP_A,PTDEEP_B,ZTCONDA_DELZ_N,ZCOEF_A,ZCOEF_B)
!
   ZSNOW_COEF_A(:,2)  = ZCOEF_A(:,2)
   ZSNOW_COEF_B(:,2)  = ZCOEF_B(:,2)
!
   ZGCOND1(:)         = PSOILCONDZ(:,1) ! save uppermost soil thermal condcutivity
!
ELSE
!
   ZSOIL_COEF_A(:,2) = (PTSTEP/XDAY)/(1.0 + PTSTEP/XDAY)
   ZSOIL_COEF_B(:,2) = ZTGO(:,2)    /(1.0 + PTSTEP/XDAY)

   ZTCONDA_DELZ_G(:) = (2*XPI/XDAY)/PCT(:)

! - Soil thermal conductivity
!   is implicit in Force-Restore soil method, so it
!   must be backed-out of surface thermal coefficients
!   (Etchevers and Martin 1997):

   ZGCOND1(:)            = (4*XPI/XDAY)/( PCG(:)*PCG(:)/(PD_G(:,1)*PCT(:)) )


   JL                    = 1
   ZD(:,JL)              = ZSNOWDZ(:,1)
   ZT(:,JL)              = ZTNO(:,1)
   ZHCAPZ(:,JL)          = PSNOWHCAPZ(:,1)
   ZCONDZ(:,JL)          = PSNOWCONDZ(:,1)
   ZSOURCE(:,JL)         = 0.               ! Included in energy budget
   DO JK=2,JNSNOW
      JL                 = JL + 1
      DO JJ=1,JNPTS
         ZD(JJ,JL)       = ZSNOWDZ(JJ,JK)
         ZT(JJ,JL)       = ZTNO(JJ,JK)
         ZHCAPZ(JJ,JL)   = PSNOWHCAPZ(JJ,JK)
         ZCONDZ(JJ,JL)   = PSNOWCONDZ(JJ,JK)
         ZSOURCE(JJ,JL)  = PSWNET_N(JJ)*(PTAU_N(JJ,JK-1)-PTAU_N(JJ,JK))
      ENDDO
   ENDDO
   JL                    = JL + 1
   ZD(:,JL)              = PD_G(:,1)
   ZT(:,JL)              = ZTGO(:,1)
   ZHCAPZ(:,JL)          = 1/PCT(:)
   ZCONDZ(:,JL)          = ZGCOND1(:)
   ZSOURCE(:,JL)         = PSWNET_N(:)*PTAU_N(:,JNSNOW)

! Get coefficients from upward sweep (starting from soil base to snow surface):
!
   CALL TRIDIAG_GROUND_RM_COEFS(PTSTEP,ZD(:,1:JL),ZT(:,1:JL),                &
        ZHCAPZ(:,1:JL),ZCONDZ(:,1:JL),ZSOURCE(:,1:JL),                       &
        PTDEEP_A,PTDEEP_B,ZTCONDA_DELZ_N,ZCOEF_A(:,1:JL),ZCOEF_B(:,1:JL))

   ZSNOW_COEF_A(:,2)     = ZCOEF_A(:,2)
   ZSNOW_COEF_B(:,2)     = ZCOEF_B(:,2)
!
ENDIF
!
! Interfacial ground-snowbase thermal conductivity divided by interfacial dz:
!
ZTCONDA_DELZ_NG(:) = 2/((ZSNOWDZ(:,JNSNOW)/PSNOWCONDZ(:,JNSNOW))+(PD_G(:,1)/ZGCOND1(:) ))
!
!
!*       3.     Pseudo humidity factors (-)
!               ---------------------------------------------------------
!
! First, compute the average flux heat exchange coefficient for the canopy: (kg m-2 s-1)
! Numerical: let get very small (fluxes can become essentially negligible), but not zero
!
PFLXC_V_C(:)   = PFLXC_VG_C(:)*(1.-PPSN(:)) + PFLXC_VN_C(:)*PPSN(:)*(1.-ZPSNA(:))
PFLXC_V_C(:)   = MAX(PFLXC_V_C(:), ZERTOL_FLX_C)

! Understory vegetation and ground factors:

ZFFF(:)        = PFF(:)*( 1.0 - PFFROZEN(:)*(1.0 - (XLSTT/XLVTT)) )
 
ZHN(:)         = (1.0-PPSN(:)-ZFFF(:))*(                                             &
                            PLEG_DELTA(:) *(1.0-PFROZEN1(:))                         &
                 +          PLEGI_DELTA(:)*     PFROZEN1(:)*(XLSTT/XLVTT) ) + ZFFF(:)         

ZHS(:)         = (1.0-PPSN(:)-ZFFF(:))*(                                             &
                   PHUG(:) *PLEG_DELTA(:) *(1.0-PFROZEN1(:))                         &
                 + PHUGI(:)*PLEGI_DELTA(:)*     PFROZEN1(:)*(XLSTT/XLVTT) ) + ZFFF(:)

! adjust for local use in solution (since they are multiplied by 1-psn herein):

ZHN(:)         = ZHN(:)/MAX(1.0 - PPSN(:) , ZERTOL)

ZHS(:)         = ZHS(:)/MAX(1.0 - PPSN(:) , ZERTOL)

! Vegetation canopy factor:

PHVGS(:)       = (1.-ZPSNA(:))*PPSN(:) *PHVN(:)*(PFLXC_VN_C(:)/PFLXC_V_C(:)) + &
                           (1.-PPSN(:))*PHVG(:)*(PFLXC_VG_C(:)/PFLXC_V_C(:))

PHVNS(:)       = (1.-ZPSNA(:))*PPSN(:) *        (PFLXC_VN_C(:)/PFLXC_V_C(:)) + &
                           (1.-PPSN(:))*        (PFLXC_VG_C(:)/PFLXC_V_C(:))

! - total canopy H factor (including intercepted snow)

ZHVS(:)        = (1.-PPSNCV(:))*PHVGS(:) + PPSNCV(:)*(XLSTT/XLVTT)*PHVNS(:)   

! Snow latent heating factor:
! NOTE, for now we consider only snow sublimation 
!       (not evaporation from snow liquid)
!
ZHNS           = (XLSTT/XLVTT)
!
!
!*       4.     Transform atmospheric coupling coefficients for T and q
!               ---------------------------------------------------------
!
!  Transform coupling coefficients from flux form to scalar form for local
!  use in the energy budgets.
!
! - first, to shorten computations a bit, define a snow fraction product

ZPSNAG(:)         = 1.0 - PPSN(:)*PPSNA(:)

! T coefficients: where : TA = PET_B_COEF_P + PET_A_COEF_P*TC + PET_C_COEF_P*TN

ZWORK(:)          = PTHRMA_TA(:)*( 1.0 + PPET_A_COEF(:)*(                                        &
                    PFLXC_C_A(:)*ZPSNAG(:) + PFLXC_N_A(:)*PPSN(:)*PPSNA(:)) )
ZPET_A_COEF_P(:)  =   PPET_A_COEF(:)*PFLXC_C_A(:)*ZPSNAG(:)*PTHRMA_TC(:)               /ZWORK(:)
ZPET_B_COEF_P(:)  = ( PPET_B_COEF(:) - PTHRMB_TA(:) +                                            &
                      PPET_A_COEF(:)*(PFLXC_C_A(:)*ZPSNAG(:)*(PTHRMB_TC(:)-PTHRMB_TA(:)) +       &
                      PFLXC_N_A(:)*PPSN(:)*PPSNA(:)*(PTHRMB_TN(:)-PTHRMB_TA(:)) ) )   /ZWORK(:)
ZPET_C_COEF_P(:)  =   PPET_A_COEF(:)*PFLXC_N_A(:)*PPSN(:)*PPSNA(:)*PTHRMA_TN(:)       /ZWORK(:)

! q coefficients:

ZWORK(:)          = 1.0 + PPEQ_A_COEF(:)*(PFLXC_C_A(:)*ZPSNAG(:) +                               &
                                          PFLXC_N_A(:)*PPSN(:)*PPSNA(:)*ZHNS)
ZPEQ_A_COEF_P(:)  = PPEQ_A_COEF(:)*PFLXC_C_A(:)*ZPSNAG(:)            /ZWORK(:)
ZPEQ_B_COEF_P(:)  = PPEQ_B_COEF(:)                                   /ZWORK(:)
ZPEQ_C_COEF_P(:)  = PPEQ_A_COEF(:)*PFLXC_N_A(:)*PPSN(:)*PPSNA(:)*ZHNS/ZWORK(:)


!*       5.     Compute canopy air coefficients (T and q)
!               ---------------------------------------------------------


! - Canopy air T coefs, where : TC = COEFA_TC + COEFB_TC*TV + COEFC_TC*TG + COEFD_TC*TN

ZWORK(:)     = PFLXC_C_A(:) *(PTHRMA_TC(:)-PTHRMA_TA(:)*ZPET_A_COEF_P(:))*ZPSNAG(:)                 &
                   + PFLXC_V_C(:) *PTHRMA_TC(:)                                                     &
                   + PFLXC_G_C(:) *PTHRMA_TC(:)*(1.0-PPSN(:))                                       &
                   + PFLXC_N_C(:) *PTHRMA_TC(:)*     PPSN(:) *(1.0-PPSNA(:))

ZCOEFA_TC(:) = (PFLXC_C_A(:) * ZPSNAG(:) *(PTHRMA_TA(:)*ZPET_B_COEF_P(:)-PTHRMB_TC(:)+PTHRMB_TA(:)) &    
                   + PFLXC_V_C(:) * (PTHRMB_TV(:)-PTHRMB_TC(:))                                     &
                   + PFLXC_G_C(:) * (PTHRMB_TG(:)-PTHRMB_TC(:))*(1.0-PPSN(:))                       &
                   + PFLXC_N_C(:) * (PTHRMB_TN(:)-PTHRMB_TC(:))*     PPSN(:) *(1.0-PPSNA(:))        &
                                                                      )/ZWORK(:)

ZCOEFB_TC(:) = PFLXC_V_C(:)*PTHRMA_TV(:)                               /ZWORK(:)

ZCOEFC_TC(:) = PFLXC_G_C(:)*PTHRMA_TG(:)*(1.0-PPSN(:))                 /ZWORK(:)

ZCOEFD_TC(:) =(PFLXC_N_C(:)*PTHRMA_TN(:)*          PPSN(:)*(1.0-PPSNA(:)) +                         &
               PFLXC_C_A(:) *PTHRMA_TA(:)*ZPET_C_COEF_P(:)*ZPSNAG(:) ) /ZWORK(:)

!-  Canopy air q coefs, where : QC = COEFA_QC + COEFB_QC*TV + COEFC_QC*TG + COEFD_QC*TN


ZWORK(:)       = PFLXC_C_A(:) *(1.-ZPEQ_A_COEF_P(:))*ZPSNAG(:)                                      &
                   + PFLXC_V_C(:)* ZHVS(:)                                                          &
                   + PFLXC_G_C(:) *ZHN(:) *(1.0-PPSN(:))                                            &
                   + PFLXC_N_C(:) *ZHNS   *     PPSN(:) *(1.0-PPSNA(:))
ZWORK(:)       = MAX(ZERTOL, ZWORK(:))

ZCOEFA_QC(:)   = ( PFLXC_C_A(:) *ZPEQ_B_COEF_P(:)*ZPSNAG(:)                                         &
                 + PFLXC_V_C(:) *ZHVS(:)*(PQSAT_V(:)-PDQSAT_V(:)*ZTVO(:)  )                         &
                 + PFLXC_G_C(:) *ZHS(:) *(PQSAT_G(:)-PDQSAT_G(:)*ZTGO(:,1))*(1.0-PPSN(:))           &
                 + PFLXC_N_C(:) *ZHNS   *(PQSATI_N(:)-PDQSATI_N(:)*ZTNO(:,1))*                      &
                                                                      PPSN(:)*(1.0-PPSNA(:))        &
                                                                                )/ZWORK(:) 

ZCOEFB_QC(:)   = PFLXC_V_C(:) *ZHVS(:)*PDQSAT_V(:)                            /ZWORK(:)

ZCOEFC_QC(:)   = PFLXC_G_C(:) *ZHS(:) *PDQSAT_G(:)*(1.0-PPSN(:))              /ZWORK(:)

ZCOEFD_QC(:)   = PFLXC_N_C(:) *ZHNS   *PDQSATI_N(:)*     PPSN(:)*(1.0-PPSNA(:))/ZWORK(:)

!*       6.     Surface Energy Budget(s) coefficients
!               -------------------------------------
! Each of the 'N' energy budgets is linearized, so we have 
! 'N' linear equations and 'N' unknowns. Here we set up the coefficients.
! For computations here, make snow radiative terms relative to snow surface:

ZWORK(:)        = 1/MAX(ZERTOL,       PPSN(:))

ZRNET_NN(:)     = (PSWNET_N(:) + PLWNET_N(:))*ZWORK(:)
ZRNET_N_DTNN(:) = PLWNET_N_DTN(:)            *ZWORK(:)
ZRNET_N_DTGN(:) = PLWNET_N_DTG(:)            *ZWORK(:)
ZRNET_N_DTVN(:) = PLWNET_N_DTV(:)            *ZWORK(:)


! Tv coefs, where TV = BETA_V + ALPHA_V*TG + GAMMA_V*TN

ZWORK(:)    =   (PCHEATV(:)/PTSTEP) - PLWNET_V_DTV(:)                                               &
              + PFLXC_V_C(:)*(PTHRMA_TV(:) - PTHRMA_TC(:)*ZCOEFB_TC(:)                              &
              + XLVTT*ZHVS(:)*(PDQSAT_V(:) - ZCOEFB_QC(:)) )           

ZBETA_V(:)  = ( (PCHEATV(:)/PTSTEP)*ZTVO(:) + PLWNET_V(:) + PSWNET_V(:)                             &
              - PLWNET_V_DTV(:)*ZTVO(:) - PLWNET_V_DTG(:)*ZTGO(:,1) - PLWNET_V_DTN(:)*ZTNO(:,1)     &
              - PFLXC_V_C(:)*( PTHRMB_TV(:)-PTHRMB_TC(:)-PTHRMA_TC(:)*ZCOEFA_TC(:)                  &
              + XLVTT*ZHVS(:)*(PQSAT_V(:) - PDQSAT_V(:)*ZTVO(:)                                     &
              - ZCOEFA_QC(:)) ) )/ZWORK(:)

ZALPHA_V(:) = (PLWNET_V_DTG(:) + PFLXC_V_C(:)*(PTHRMA_TC(:)*ZCOEFC_TC(:)                            &
              + XLVTT*ZHVS(:)*ZCOEFC_QC(:) ) )/ZWORK(:)

ZGAMMA_V(:) = (PLWNET_V_DTN(:) + PFLXC_V_C(:)*(PTHRMA_TC(:)*ZCOEFD_TC(:)                            &
              + XLVTT*ZHVS(:)*ZCOEFD_QC(:) ) )/ZWORK(:)

! Tg coefs, where TG = BETA_G + ALPHA_G*TV + GAMMA_G*TN

ZWORK(:)    =   (PCHEATG(:)/PTSTEP) - PLWNET_G_DTG(:)                                                &
              + (1.0-PPSN(:))*PFLXC_G_C(:)*( (PTHRMA_TG(:) - PTHRMA_TC(:)*ZCOEFC_TC(:))             &
              + XLVTT*(ZHS(:)*PDQSAT_G(:) - ZHN(:)*ZCOEFC_QC(:)) )                                  &
              + ZTCONDA_DELZ_G(:)*(1.0-ZSOIL_COEF_A(:,2))                                           &
              + PPSN(:)*ZTCONDA_DELZ_NG(:)

ZBETA_G(:)  = ( (PCHEATG(:)/PTSTEP)*ZTGO(:,1)  + PLWNET_G(:) + PSWNET_G(:)                          &
              - PLWNET_G_DTV(:)*ZTVO(:) - PLWNET_G_DTG(:)*ZTGO(:,1) - PLWNET_G_DTN(:)*ZTNO(:,1)     &
              - (1.0-PPSN(:))*PFLXC_G_C(:)*( PTHRMB_TG(:)-PTHRMB_TC(:)-PTHRMA_TC(:)*ZCOEFA_TC(:)    &
              + XLVTT*(ZHS(:)*(PQSAT_G(:) - PDQSAT_G(:)*ZTGO(:,1))                                  &
              - ZHN(:)*ZCOEFA_QC(:)) )                                                              &
              + ZTCONDA_DELZ_G(:)*ZSOIL_COEF_B(:,2)                                                 &
              + PPSN(:)*ZTCONDA_DELZ_NG(:)*ZTNO(:,JNSNOW) )/ZWORK(:)

ZALPHA_G(:) =  (PLWNET_G_DTV(:) + PFLXC_G_C(:)*(1.0-PPSN(:))*( PTHRMA_TC(:)*ZCOEFB_TC(:)             &
              + XLVTT*ZHN(:)*ZCOEFB_QC(:) ) )/ZWORK(:)

ZGAMMA_G(:) =  (PLWNET_G_DTN(:) + PFLXC_G_C(:)*(1.0-PPSN(:))*( PTHRMA_TC(:)*ZCOEFD_TC(:)             &
              + XLVTT*ZHN(:)*ZCOEFD_QC(:) ) )/ZWORK(:)

! Tn coefs, where TN = BETA_N + ALPHA_N*TV + GAMMA_N*TG

ZWORK(:)    =   (PCHEATN(:)/PTSTEP) - ZRNET_N_DTNN(:)                                                     &
              + PFLXC_N_C(:)*(1.-PPSNA(:))*( PTHRMA_TN(:) - PTHRMA_TC(:)*ZCOEFD_TC(:)                     &
              + (XLVTT*ZHNS)*(PDQSATI_N(:) - ZCOEFD_QC(:)) )                                              &
              + PFLXC_N_A(:)*    PPSNA(:)*(                                                               &
                PTHRMA_TN(:) - PTHRMA_TA(:)*(ZPET_A_COEF_P(:)*ZCOEFD_TC(:) + ZPET_C_COEF_P(:))            &
              + (XLVTT*ZHNS)*(PDQSATI_N(:)*(1.0-ZPEQ_C_COEF_P(:)) - ZPEQ_A_COEF_P(:)*ZCOEFD_QC(:)) )      &
              + ZTCONDA_DELZ_N(:)*(1.0-ZSNOW_COEF_A(:,2))

ZBETA_N(:)  = ( (PCHEATN(:)/PTSTEP)*ZTNO(:,1)  + ZRNET_NN(:) + PHPSNOW(:) + PMELTADV(:)                   &
              - ZRNET_N_DTVN(:)*ZTVO(:) - ZRNET_N_DTGN(:)*ZTGO(:,1) - ZRNET_N_DTNN(:)*ZTNO(:,1)           &
              - PFLXC_N_C(:)*(1.-PPSNA(:))*( PTHRMB_TN(:) - PTHRMB_TC(:) - PTHRMA_TC(:)*ZCOEFA_TC(:)      &
              + (XLVTT*ZHNS)*(PQSATI_N(:) - PDQSATI_N(:)*ZTNO(:,1)                                        &
              - ZCOEFA_QC(:)) )                                                                           &
              - PFLXC_N_A(:)*PPSNA(:)*( PTHRMB_TC(:) - PTHRMB_TA(:) -                                     &
                                          PTHRMA_TA(:)*(ZPET_B_COEF_P(:) + ZCOEFA_TC(:)*ZPET_A_COEF_P(:)) &
              + (XLVTT*ZHNS)*((PQSATI_N(:) - PDQSATI_N(:)*ZTNO(:,1))*(1.0-ZPEQ_C_COEF_P(:))               &
              - ZPEQ_B_COEF_P(:) - ZPEQ_A_COEF_P(:)*ZCOEFA_QC(:)) )                                       &
              + ZTCONDA_DELZ_N(:)*ZSNOW_COEF_B(:,2) )/ZWORK(:)

ZALPHA_N(:) = ( ZRNET_N_DTVN(:) + PFLXC_N_C(:)*(1.-PPSNA(:))*( PTHRMA_TC(:)*ZCOEFB_TC(:)                  &
              + (XLVTT*ZHNS)*ZCOEFB_QC(:) )                                                               &
              + PFLXC_N_A(:)*    PPSNA(:) *( PTHRMA_TA(:)*ZCOEFB_TC(:)*ZPET_A_COEF_P(:)                   &
              + (XLVTT*ZHNS)*ZCOEFB_QC(:)*ZPEQ_A_COEF_P(:) ) )/ZWORK(:)

ZGAMMA_N(:) = ( ZRNET_N_DTGN(:) + PFLXC_N_C(:)*(1.-PPSNA(:))*( PTHRMA_TC(:)*ZCOEFC_TC(:)                  &
              + (XLVTT*ZHNS)*ZCOEFC_QC(:))                                                                &
              + PFLXC_N_A(:)*    PPSNA(:) *( PTHRMA_TA(:)*ZCOEFC_TC(:)*ZPET_A_COEF_P(:)                   &
              + (XLVTT*ZHNS)*ZCOEFC_QC(:)*ZPEQ_A_COEF_P(:) ) )/ZWORK(:)

!*       7.     Solve multiple energy budgets simultaneously (using an implicit time scheme)
!               ----------------------------------------------------------------------------
! Also compute certain needed diagnostics, like the canopy air T and q, and T and q at
! the lowest model level: They are used within the current patch to compute implicit or explicit
! fluxes (depending upon the values of the atmospheric Implicit Coupling (IC) coefficients)


WHERE(PPSN(:) > 0.0)

   ZWORK(:)      = 1.0 - ZALPHA_V(:)*ZALPHA_G(:)
   ZBETA_P_V(:)  = (ZBETA_V(:)  + ZALPHA_V(:)*ZBETA_G(:) )/ZWORK(:)
   ZALPHA_P_V(:) = (ZGAMMA_V(:) + ZALPHA_V(:)*ZGAMMA_G(:))/ZWORK(:)

   ZWORK(:)      = 1.0 - ZGAMMA_G(:)*ZGAMMA_N(:)
   ZBETA_P_N(:)  = (ZBETA_N(:)  + ZGAMMA_N(:)*ZBETA_G(:) )/ZWORK(:)
   ZALPHA_P_N(:) = (ZALPHA_N(:) + ZGAMMA_N(:)*ZALPHA_G(:))/ZWORK(:)

   PTN(:,1)      = (ZBETA_P_N(:) + ZALPHA_P_N(:)*ZBETA_P_V(:))/           &
                   (1.0 - ZALPHA_P_N(:)*ZALPHA_P_V(:)      )

! Since the fluxes are passed to the snow scheme, we can simply limit Tn here
! for flux computations (to make sure fluxes correspond to a snow sfc which
! doesn't exceed it's physical limit, Tf). The new real snow Tn consistent with these fluxes
! (and the T-profile within the snow) will be computed within the snow scheme.

   PTN(:,1)      = MIN(XTT, PTN(:,1))

   PTV(:)        = ZBETA_P_V(:) + ZALPHA_P_V(:)*PTN(:,1)

   PTG(:,1)      = ZBETA_G(:) + ZALPHA_G(:)*PTV(:) + ZGAMMA_G(:)*PTN(:,1)

   PTC(:)        = ZCOEFA_TC(:) + ZCOEFB_TC(:)*PTV(:) + ZCOEFC_TC(:)*PTG(:,1) + ZCOEFD_TC(:)*PTN(:,1) 

   PQC(:)        = ZCOEFA_QC(:) + ZCOEFB_QC(:)*PTV(:) + ZCOEFC_QC(:)*PTG(:,1) + ZCOEFD_QC(:)*PTN(:,1) 

! Lowest atmospheric level air temperature (for this patch):

   PTA_IC(:)     = ZPET_B_COEF_P(:) + ZPET_A_COEF_P(:) *PTC(:) + ZPET_C_COEF_P(:) *PTN(:,1)

! Lowest atmospheric level specific humidity (for this patch):
! - First, compute the surface specific humidity of the snow:

   ZWORK(:)      = ZHNS*( PQSATI_N(:) + PDQSATI_N(:)*(PTN(:,1) - ZTNO(:,1)) ) ! q_n+

   PQA_IC(:)     = ZPEQ_B_COEF_P(:) + ZPEQ_A_COEF_P(:) *PQC(:) + ZPEQ_C_COEF_P(:) *ZWORK(:)

! diagnostic snow thermal flux: surface to sub-surface thermal transfer

!   ZRESTOREN(:)  = PPSN(:)*ZTCONDA_DELZ_N(:)*(PTN(:,1)*(1.0-ZSNOW_COEF_A(:,2)) - ZSNOW_COEF_B(:,2))

ELSEWHERE ! snow free canopy-understory case:

   PTG(:,1)      = (ZBETA_G(:) + ZALPHA_G(:)*ZBETA_V(:))/           &
                   (1.0 - ZALPHA_G(:)*ZALPHA_V(:)      )

   PTV(:)        = ZBETA_V(:) + ZALPHA_V(:)*PTG(:,1)

   PTC(:)        = ZCOEFA_TC(:) + ZCOEFB_TC(:)*PTV(:) + ZCOEFC_TC(:)*PTG(:,1) 

   PQC(:)        = ZCOEFA_QC(:) + ZCOEFB_QC(:)*PTV(:) + ZCOEFC_QC(:)*PTG(:,1) 

! Lowest atmospheric level air temperature (for this patch):

   PTA_IC(:)     = ZPET_B_COEF_P(:) + ZPET_A_COEF_P(:) *PTC(:) 

! Lowest atmospheric level specific humidity (for this patch):

   PQA_IC(:)     = ZPEQ_B_COEF_P(:) + ZPEQ_A_COEF_P(:) *PQC(:) 

! arbitrary: (as no snow mass present)

   PTN(:,1)      = XTT

!   ZRESTOREN(:)  = 0.0

END WHERE
!
!
! Compute test sub-surface snow temperatures: this improves time split estimate of 
! surface to sub-surface flux estimates. Note that the sub-surface
! snow temperatures are "test" temperatures, with final "true" values
! computed within the snow routine.
!

 CALL TRIDIAG_GROUND_RM_SOLN(ZT,ZCOEF_A,ZCOEF_B)
DO JK=2,JNSNOW
   WHERE(PPSN(:) > 0.0)
      PTN(:,JK) = MIN(XTT,ZT(:,JK))
   ENDWHERE
ENDDO
!
!
!*       8.     Solve soil temperature profile (implicitly coupled to surface energy budgets)
!               -----------------------------------------------------------------------------
! The back-substitution of sub-surface soil T profile
! (update all temperatures *below* the surface)
!
IF(HISBA == 'DIF')THEN
   CALL TRIDIAG_GROUND_RM_SOLN(PTG,ZSOIL_COEF_A,ZSOIL_COEF_B)
ELSE
   PTG(:,2) = ZSOIL_COEF_B(:,2) + ZSOIL_COEF_A(:,2)*PTG(:,1)
ENDIF
!
! Diagnose (semi-implicit) flux across ground base:
! (semi-implicit if T imposed, explicit if flux imposed)
!
WHERE(PTDEEP_B(:) == XUNDEF)
   PDEEP_FLUX(:) = 0.0
ELSEWHERE
   ZWORK(:)      = PSOILCONDZ(:,JNGRND)*2/(PD_G(:,JNGRND)-PD_G(:,JNGRND-1)) ! (W/m2/K)
   PDEEP_FLUX(:) = ZWORK(:)*(PTDEEP_B(:) - PTG(:,JNGRND))/                 &
                              (1. - ZWORK(:)*PTDEEP_A(:))                   ! (W/m2)
END WHERE
!
!*       9.     Temperature tendencies
!               ----------------------
!
! Compute energy budget tendencies (for flux computations) 
! and sub-sfc soil tendencies (K) (for phase changes)

PDELTAT_G(:)   = PTG(:,1) - ZTGO(:,1)
PDELTAT_V(:)   = PTV(:)   - ZTVO(:)
PDELTAT_N(:)   = PTN(:,1) - ZTNO(:,1)
!
!
!*      10.     Flux Diagnostics
!               ----------------
!
! Locally implicit (with respect to the ground T) snow-ground heat flux (W m-2).
! This is a first estimate, since the final fully implicit flux
! is computed in the snow routine: any differences
! between this flux estimate and the final one are
! transferred to the soil as heating/cooling, thus
! conserving energy.
!
PGRNDFLUX(:)    = PPSN(:)*ZTCONDA_DELZ_NG(:)*( PTN(:,JNSNOW) - PTG(:,1) )
!
!
!*      10.     Energy Storage Diagnostics (W m-2)
!               ----------------------------------
!
PDELHEATG_SFC(:) = PCHEATG(:)*PDELTAT_G(:)/PTSTEP 
PDELHEATV_SFC(:) = PCHEATV(:)*PDELTAT_V(:)/PTSTEP 
!
IF(HISBA == 'DIF')THEN

! Flux between surface and sub-surface (W m-2):

   PRESTORE(:)  = (PTG(:,1) - PTG(:,2))* 2/( ((PD_G(:,2)-PD_G(:,1))/PSOILCONDZ(:,2)) +     &
                                             ( PD_G(:,1)           /PSOILCONDZ(:,1)) )
!
!
!*      10.a    Energy Storage Diagnostics (W m-2): DIF
!               ---------------------------------------
!
! These terms are used for computing energy budget diagnostics.
!
!
   PDELHEATG(:)        = PDELHEATG_SFC(:) ! initialize
   DO JK=2,JNGRND
      DO JJ=1,JNPTS
         PDELHEATG(JJ) =  PDELHEATG(JJ) + PSOILHCAPZ(JJ,JK)*(PD_G(JJ,JK)-PD_G(JJ,JK-1))*    &
                          (PTG(JJ,JK) - ZTGO(JJ,JK))/PTSTEP
      ENDDO
   ENDDO
!
ELSE

!*      10.b    Energy Storage Diagnostics (W m-2): Force Restore
!               -------------------------------------------------
!
! Flux between surface and sub-surface (W m-2):

   PRESTORE(:)  = (2*XPI/XDAY)*(PTG(:,1) - PTG(:,2))/PCT(:)

! These terms are used for computing energy budget diagnostics.
!
   PDELHEATG(:) = PDELHEATG_SFC(:) + PRESTORE(:)
!
ENDIF

!
!*      11.     Additional Diagnostics
!               ----------------------
!
! Here compute the updated (time t+dt) effective reference level specific heat capacity:
! (just a diagnostic here):
!
IF (HCPSURF=='DRY') THEN
   PCPS(:) = XCPD
ELSEIF(.NOT.LCPL_ARP)THEN
   PCPS(:) = XCPD + ( XCPV - XCPD ) * PQC(:)   
ENDIF
!
PLVTT(:)   = XLVTT
PLSTT(:)   = XLVTT ! latent heat of sublimation already accounted for
!
!
IF (LHOOK) CALL DR_HOOK('E_BUDGET_MEB',1,ZHOOK_HANDLE)
!
END SUBROUTINE E_BUDGET_MEB


