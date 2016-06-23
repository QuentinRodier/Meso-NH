!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!   ############################################################################
!
SUBROUTINE DRAG_MEB(LFORC_MEASURE,                                     &
                    PTG, PTC, PTV, PSNOWTEMP, PTA, PQC, PQA, PVMOD,    &
                    PWG, PWGI, PWSAT, PWFC,                            &
                    PEXNS, PEXNA, PPS,                                 &
                    PRR, PSR, PRHOA, PZ0G_WITHOUT_SNOW,                &
                    PZ0_MEBV, PZ0H_MEBV, PZ0EFF_MEBV,                  &
                    PZ0_MEBN, PZ0H_MEBN, PZ0EFF_MEBN,                  &
                    PZ0_WITH_SNOW, PZ0H_WITH_SNOW, PZ0EFF,             &
                    PSNOWSWE,                                          &
                    PWR, PCHIP, PTSTEP, PRS_VG, PRS_VN,                &
                    PPSN, PPALPHAN, PZREF, PUREF, PH_VEG, PDIRCOSZW,   &
                    PPSNCV, PDELTA, PLAI, OMEB_GNDRES,                 &
                    PCH, PCD, PCDN, PRI, PRA, PVELC,                   &
                    PCDSNOW, PCHSNOW, PRISNOW, PUSTAR2SNOW,            &
                    PHUG, PHUGI, PHV, PHVG, PHVN, PHU, PQS, PRS,       &
                    PLEG_DELTA, PLEGI_DELTA, PHSGL, PHSGF,             &
                    PFLXC_C_A, PFLXC_N_A, PFLXC_G_C, PFLXC_N_C,        &    
                    PFLXC_VG_C, PFLXC_VN_C, PFLXC_MOM,                 &
                    PQSATG, PQSATV, PQSATC, PQSATN, PDELTAVK           )
!                    
!   ############################################################################
!
!!****  *DRAG_MEB*  
!!
!!    PURPOSE
!!    -------
!!
!!     Calculates the coefficients for heat transfers
!!     for the multiple-energy balance, and Halstead coefficients.
!!     Also estimate patch average specific and relative humidity, as well
!!     as exchange coefficients and Richardson's number
!!         
!!     
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    USE MODD_CST 
!!
!!      
!!    REFERENCE
!!    ---------
!!
!!      
!!    AUTHOR
!!    ------
!!
!!      P.Samuelsson/S.Gollvik            * SMHI *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    06/2010
!!      For MEB     01/2011
!                   10/2014 (A. Boone) Remove understory compsite vegetation
!                   10/2014 (A. Napoly) Added ground/litter resistance
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_CSTS,     ONLY : XPI
USE MODD_SNOW_PAR, ONLY : XZ0SN
USE MODD_ISBA_PAR, ONLY : XWGMIN, XFLXMAX
USE MODD_SURF_ATM, ONLY : LDRAG_COEF_ARP, XRIMAX, LRRGUST_ARP, XRRSCALE,   &
                          XRRGAMMA, XUTILGUST, LCPL_ARP
USE MODD_MEB_PAR,  ONLY : XKDELTA_WR
!
USE MODI_SURFACE_CDCH_1DARP
USE MODI_WIND_THRESHOLD
USE MODI_DISPH_FOR_MEB
USE MODI_PREPS_FOR_MEB_DRAG
USE MODI_SURFACE_AIR_MEB
!
USE MODE_THERMOS, ONLY : QSAT, QSATI
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
LOGICAL, INTENT(IN)              ::   LFORC_MEASURE
!                                       LFORC_MEASURE = switch for using measured data
!
REAL, INTENT(IN)                 ::   PTSTEP 
!                                     PTSTEP = Model time step (s)        
!
REAL, DIMENSION(:), INTENT(IN)   :: PTG, PTC, PTV, PSNOWTEMP, PTA, PQC, PQA, PVMOD, &
                                    PWG, PWGI, PWSAT, PWFC, PEXNS, PEXNA, PPS,      &
                                    PSNOWSWE
!                                     PTG     = surface temperature
!                                     PTC     = canopy air temperature
!                                     PTV     = canopy temperature
!                                     PSNOWTEMP = sfc layer snow temperature (K)
!                                     PSNOWSWE  = sfc layer snow water equiv (SWE) kg m-2
!                                     PTA     = temperature of the atmosphere/forcing-level
!                                     PQC     = canopy air humidity
!                                     PQA = specific humidity of the atmosphere/forcing-level
!                                     PVMOD = module of the horizontal wind
!                                             NOTE it should be input as >= 1. (m)
!                                     PWG     = near-surface volumetric water content
!                                     PWGI    = near-surface frozen volumetric water content
!                                     PWSAT   = volumetric water content at saturation
!                                     PWFC = field capacity volumetric water content
!                                     PEXNS   = Exner function at the surface
!                                     PEXNA= Exner function of the atmosphere/forcing-level
!                                     PPS = surface pressure
!
REAL, DIMENSION(:), INTENT(IN)   ::  PRR, PSR, PRHOA
!                                     PRR = rain rate    
!                                     PSR = snow rate             
!                                     PRHOA = near-ground air density
!
REAL, DIMENSION(:), INTENT(IN)   :: PZ0G_WITHOUT_SNOW, &
                                    PZ0_MEBV, PZ0H_MEBV, PZ0EFF_MEBV, &
                                    PZ0_MEBN, PZ0H_MEBN, PZ0EFF_MEBN, &
                                    PZ0_WITH_SNOW, PZ0H_WITH_SNOW, PZ0EFF
!
!                    PZ0G_WITHOUT_SNOW  ! roughness length for momentum at snow-free canopy floor
!                    PZ0_MEBV           ! roughness length for momentum over MEB vegetation part of patch
!                    PZ0H_MEBV          ! roughness length for heat over MEB vegetataion part of path
!                    PZ0EFF_MEBV        ! roughness length for momentum over MEB vegetation part of patch
!                                         eventuelly including orograhic roughness
!                    PZ0_MEBN           ! roughness length for momentum over MEB snow part of patch
!                    PZ0H_MEBN          ! roughness length for heat over MEB snow part of path
!                    PZ0EFF_MEBN        ! roughness length for momentum over MEB snow part of patch
!                                         eventuelly including orograhic roughness
!                    PZ0_WITH_SNOW      ! roughness length for momentum over MEB total patch
!                    PZ0H_WITH_SNOW     ! roughness length for heat over MEB total path
!                    PZ0EFF             ! roughness length for momentum over MEB total patch
!                                         eventuelly including orograhic roughness
!
REAL, DIMENSION(:), INTENT(IN)   :: PWR,PCHIP
!                                     PWR  = intercepted water for the canopy
!                                     PCHIP = view factor (for LW) 
!
REAL, DIMENSION(:), INTENT(IN)   :: PRS_VG, PRS_VN, PPSN, PPSNCV,  &
                                    PPALPHAN, PZREF, PUREF, PH_VEG, PDIRCOSZW
!
!                                     PRS_VG   = surface resistance for canopy (1-png)
!                                     PRS_VN   = surface resistance for canopy (png)
!                                     PPSN     = fraction of the patch covered by snow
!                                     PPSNCV   = fraction of the canopy vegetation covered by snow
!                                     PPALPHAN = weight between canopy air flow and direct flow 
!                                                 between snow and atmosphere
!                                     PZREF    = reference height of the first
!                                                atmospheric level 
!                                     PUREF    = reference height of the wind
!                                                NOTE this is different from ZZREF
!                                                ONLY in stand-alone/forced mode,
!                                                NOT when coupled to a model (MesoNH)
!                                     PH_VEG   = height of the vegetation
!                                     PDIRCOSZW= Cosinus of the angle between 
!                                                the normal to the surface and the vertical
!
REAL, DIMENSION(:), INTENT(IN)    :: PDELTA, PLAI
!                                     PDELTA   = fraction of the canopy foliage covered
!                                                by intercepted water (-)
!                                     PLAI     = vegetation LAI (m2 m-2)
!
LOGICAL,              INTENT(IN)    :: OMEB_GNDRES   ! Flag for ground resistance
REAL, DIMENSION(:), INTENT(OUT)  :: PDELTAVK
!                                     PDELTAVK = fraction of the canopy foliage covered
!                                                by intercepted water *including* K-factor (-)
!                                                (i.e. that the intercepted water fraction is not 
!                                                 necessarily reaching 100%)
!
REAL, DIMENSION(:), INTENT(OUT)  :: PCH, PCD, PCDN, PRI, PRA, PVELC
!                                     PCH = drag coefficient for heat, averaged over whole patch
!                                     PCD = drag coefficient for momentum, averaged over whole patch
!                                     PCDN= neutral drag coefficient for momentum, 
!                                           averaged over whole patch
!                                     PRI = Richardson number, averaged over whole patch
!                                     PRA = aerodynamic resistance between 
!                                           the whole patch and atmosphere
!                                     PVELC = wind speed at top of vegetation
!
REAL, DIMENSION(:), INTENT(OUT)  :: PHUG, PHUGI, PHVG, PHVN, PHV,                        &
                                    PHU, PQS, PLEG_DELTA, PLEGI_DELTA, PHSGL, PHSGF, PRS
!
!                                    PRS   = total effective stomatal resistance (under and overstory) diagnostic (s m-1)
!                                    PHUG  = ground relative humidity
!                                    PHUGI = ground (ice) relative humidity
!                                    PHV   = Total effective Halstead coefficient 
!                                    PHVN  = Halstead coefficient vegetation canopy above snow
!                                    PHVG  = Halstead coefficient vegetation canopy above ground
!                                    PHU   = effective relative humidity at the surface
!                                    PQS   = effective specific humidity at surface
!                                    PLEG_DELTA  = soil evaporation delta fn
!                                    PLEGI_DELTA = soil sublimation delta fn
!                                    PHSGL = surface halstead cofficient for bare soil (currently==1)
!                                    PHSGF = surface halstead cofficient for bare soil ice (currently==1)
!
!
REAL, DIMENSION(:), INTENT(OUT)  :: PFLXC_C_A, PFLXC_N_A, PFLXC_G_C, PFLXC_N_C, PFLXC_VG_C, PFLXC_VN_C
!  
!                                     EXCHANGE coefficients i.e. rho/resistance [kg/m2/s]
!  
!                                    PFLXC_C_A   between canopy air and atmosphere
!                                    PFLXC_N_A   between the snow on the ground and atmosphere
!                                    PFLXC_G_C   between snow-free ground and canopy air
!                                    PFLXC_N_C   between snow on the ground and canopy air
!                                    PFLXC_VG_C  between canopy over snow-free ground and canopy air
!                                    PFLXC_VN_C  between canopy over the snow on the ground and canopy air
!
REAL, DIMENSION(:), INTENT(OUT)  :: PFLXC_MOM, PQSATG, PQSATV, PQSATC, PQSATN
!                                    PFLXC_MOM = effective drag coefficient for momentum [kg/m2/s]
!                                    PQSATG = qsat for PTG
!                                    PQSATV = qsat for PTV
!                                    PQSATC = qsat for PTC
!                                    PQSATN = qsat for PSNOWTEMP
!
REAL, DIMENSION(:), INTENT(OUT)  :: PCDSNOW, PCHSNOW, PRISNOW, PUSTAR2SNOW 
!                                    PCDSNOW     = drag coefficient over snow (-)
!                                    PCHSNOW     = heat/mass exchange coefficient over snow (-)
!                                    PRISNOW     = Richardson number over snow (-)
!                                    PUSTAR2SNOW = Surface friction velocity squared  (m2 s-2)
!                                                   Just a diagnostic, not used in coupling

!                                    
!*      0.2    declarations of local variables
!
!
!
REAL, DIMENSION(SIZE(PTG)) :: ZAC,ZWFC,ZWSAT,ZFP,ZRRCOR 
!                              ZQSATG = specific humidity at saturation at ground
!                              ZQSATN = specific humidity of snow
!                              ZAC    =  aerodynamical conductance
!                              ZWFC   = field capacity in presence of ice
!                              ZWSAT  = saturation in presence of ice
!                              ZFP    = working variable
!                              ZRRCOR = correction of CD, CH, CDN due to moist-gustiness
!
REAL, DIMENSION(SIZE(PTG)) :: ZCHIL, ZLAISN, ZLW, ZDISPH, ZVELC, ZRICN, ZRA_C_A, &
                              ZRA_G_C, ZG_VG_C, ZRA_N_C, ZG_VN_C 
!                              ZLAISN = leaf area index for snow covered ground
!                              ZLW = leaf width
!                              ZVELC = wind speed at top of vegetation
!                              ZRICN = Richardson nr between canopy air and atmosphere
!                              ZRA_C_A = resistance between canopy air and atmosphere
!                              ZRA_G_C = resistance between canopy air and snow free ground
!                              ZG_VG_C = conductance between canopy and canopy air (1-png)
!                              ZRA_N_C = aerodynamic resistance between the partly
!                                       snowcovered (png>0), ground and canopy air
!                              ZG_VN_C = conductance between canopy and canopy air (png)
!
REAL, DIMENSION(SIZE(PTG)) :: ZCHCN,ZCDNCN,ZCDCN !exchange coefficients between 
!                                                 canopy air and atmosphere
!
REAL, DIMENSION(SIZE(PTG)) :: ZRINN,ZRANN,ZCHNN,ZCDNNN,ZCDNN,ZTEFF,ZDELTAMAX,ZDELTAV,ZVMOD 
!
REAL, DIMENSION(SIZE(PTG)) :: ZRSGL,ZRSGF,ZZ0SN
!                             ZRSGL = surface resistance for bare soil (currently==0)
!                             ZRSGF = surface resistance for bare soil ice (currently==0)
!                             ZZ0SN = array of XZ0SN
!
REAL, DIMENSION(SIZE(PTG)) :: ZRSNFRAC, ZDENOM
!                             ZRSNFRAC = fraction to prevent/reduce sublimation of snow if too thin (-)
!                             ZDENOM   = working variable for denominator of an expression (*)
!
REAL, DIMENSION(SIZE(PTG)) :: ZUSTAR2G, ZCDG, ZCHG, ZRIG, ZPSNA 
!                                   ZUSTAR2G = canopy top friction velocity squared (m2/s2)
!                                   ZCDG     = drag coefficient (-)
!                                   ZCHG     = heat transfor coefficient (ground to canopy air) (-)
!                                   ZRIG     = Richardson number (ground to canopy air) (-)!                             
!                                   ZPSNA    = buried (by snow) canopy fraction (-)
!
!*      0.3    declarations of local parameters
!
REAL, PARAMETER            :: ZRAEPS       = 1.e-3  ! Safe limit of aerodynamic resistance to avoid dividing
!                                                   ! by zero when calculating exchange coefficients
REAL, PARAMETER            :: ZSNOWSWESMIN = 1.E-4  ! (kg m-2) Reduce sublimation from ground based snowpack as it
!                                                   !          becomes vanishingly thin 
REAL, PARAMETER            :: ZRG_COEF1    = 8.206  ! Ground/litter resistance coefficient 
REAL, PARAMETER            :: ZRG_COEF2    = 4.255  ! Ground/litter resistance coefficient 
!
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!*       0.     Initialization:
!               ---------------
!
IF (LHOOK) CALL DR_HOOK('DRAG_MEB',0,ZHOOK_HANDLE)
!
PCH(:)   = 0.
PCD(:)   = 0.
PCDN(:)  = 0.
PRI(:)   = 0.
PHUG(:)  = 0.
PHUGI(:) = 0.
PHV(:)   = 0.
PHVN(:)  = 0.
PHVG(:)  = 0.
PHU(:)   = 0.
PHSGL(:) = 0.
PHSGF(:) = 0.
!
PFLXC_C_A(:)  = 0.
PFLXC_G_C(:)  = 0.
PFLXC_N_C(:)  = 0.
PFLXC_VG_C(:)  = 0.
PFLXC_VN_C(:) = 0.
PFLXC_N_A(:)  = 0.
PFLXC_MOM(:) = 0.
!
ZRSGL(:) = 0.
ZRSGF(:) = 0.
ZZ0SN(:) = XZ0SN
!
!-------------------------------------------------------------------------------
!
!*       1.     RELATIVE HUMIDITY OF THE GROUND HU
!               ----------------------------------
!
! this relative humidity is related to
! the superficial soil moisture and the
! field capacity of the ground
!
ZWSAT(:) = PWSAT(:)-PWGI(:)
ZWFC(:)  = PWFC(:)*ZWSAT(:)/PWSAT(:)
PHUG(:)  = 0.5 * ( 1.-COS(XPI*MIN((PWG(:)-XWGMIN) /ZWFC(:),1.)) )
ZWSAT(:) = MAX(XWGMIN, ZWSAT(:))
ZWFC(:)  = PWFC(:)*ZWSAT(:)/PWSAT(:)
PHUGI(:) = 0.5 * ( 1.-COS(XPI*MIN(PWGI(:)/ZWFC(:),1.)) )
!
! there is a specific treatment for dew
! (see Mahfouf and Noilhan, jam, 1991)
!
PQSATG(:) = QSAT(PTG(:),PPS(:)) 
PQSATV(:) = QSAT(PTV(:),PPS(:)) 
PQSATC(:) = QSAT(PTC(:),PPS(:)) 
PQSATN(:) = QSATI(PSNOWTEMP(:),PPS(:)) 
!
!-------------------------------------------------------------------------------
!
!*       2.     GRID-AVERAGED HUMIDITY OF THE SURFACE
!               -------------------------------------
!       average of canopy air humidity and saturation humidity of snowpack
!
!
   PQS(:) =(1.-PPSN(:)*PPALPHAN(:))*PQC(:)+ PPSN(:)*PPALPHAN(:)*PQSATN(:)
   PHU(:) =(1.-PPSN(:)*PPALPHAN(:))*PQC(:)/PQSATC(:)+ PPSN(:)*PPALPHAN(:)
!
!-------------------------------------------------------------------------------
!
!*       3.     SPECIFIC HEAT OF THE SURFACE
!               ----------------------------
! Comment S.Gollvik 20110120
!     AARON IN E-BUDGET:
!
!IF (HCPSURF=='DRY') THEN
!    PCPS(:) = XCPD
!ELSEIF(.NOT.LCPL_ARP)THEN
!    PCPS(:) = XCPD + ( XCPV - XCPD ) * PQA(:)   
!ENDIF
!
!-------------------------------------------------------------------------------
!
!*       4.     COMPUTATION OF RESISTANCES/CONDUCTIVITES
!               ----------------------------------------------
!
ZCHIL(:)=0.12
ZLW(:)=0.02
!
! Calculate the displacement height
!
 CALL DISPH_FOR_MEB(ZCHIL,PLAI,ZLW,PH_VEG,PZREF,PZ0_MEBV,ZDISPH)
!
! Here ZRICN,ZRA_C_A,ZCHCN,ZCDNCN,ZCDCN are valid for the snow free part in meb:
!
 CALL PREPS_FOR_MEB_DRAG(.TRUE.,LFORC_MEASURE,            &
                   PZ0_MEBV, PZ0H_MEBV, PZ0EFF_MEBV,     &
                   PH_VEG, PZREF,                        &
                   PTC, PTA, PQC, PQA, PUREF, PVMOD,     &
                   PEXNA, PEXNS, PDIRCOSZW, ZDISPH,      &
                   PVELC, ZVMOD, ZRICN, ZRA_C_A,         &
                   ZCHCN,ZCDNCN,ZCDCN                    )
!
IF (LRRGUST_ARP) THEN

   ZFP(:)=MAX(0.0,PRR(:)+PSR(:))
   ZRRCOR(:)=SQRT(1.0+((((ZFP(:)/(ZFP(:)+XRRSCALE))**XRRGAMMA)*XUTILGUST)**2) &
    & /(ZCDCN(:)*ZVMOD(:)**2))

   ZCDCN(:)  = ZCDCN(:)  * ZRRCOR(:)
   ZCHCN(:)  = ZCHCN(:)  * ZRRCOR(:)
   ZCDNCN(:) = ZCDNCN(:) * ZRRCOR(:)

ENDIF
!       
PFLXC_C_A(:)=ZCHCN(:)*ZVMOD(:)*PRHOA(:)
!
!
! The momentum drag for this area:
!
PFLXC_MOM(:)=ZCDCN(:)*ZVMOD(:)*PRHOA(:)
!
!Calculate the aerodynamic resistance between the snowfree part of the ground
!and canopy air, ZRA_G_C, and the conductance between the canopy and canopy air, ZG_VG_C
!  
 CALL SURFACE_AIR_MEB(PZ0_MEBV, PZ0H_MEBV, PZ0G_WITHOUT_SNOW, PH_VEG, PLAI,   &
                     PTG, PTC, PTV, PVELC, ZLW,                              &
                     ZDISPH,                                                 &
                     ZRA_G_C, ZG_VG_C,                                       &
                     ZUSTAR2G, ZCDG, ZCHG, ZRIG                              )
!
!Compute the lai of the canopy that is above snow
!
ZLAISN(:)=PLAI(:)*(1.-PPALPHAN(:))
!
! Note that we use the same displacement height also for the snow covered part
!
! The same as ZRA_G_C/ZG_VG_C but for the snow part (png) =>ZRA_N_C/ZG_VN_C
!
 CALL SURFACE_AIR_MEB(PZ0_MEBN, PZ0H_MEBN, ZZ0SN, PH_VEG, ZLAISN,     &
                     PSNOWTEMP, PTC, PTV, PVELC, ZLW,                &
                     ZDISPH,                                         &
                     ZRA_N_C, ZG_VN_C,                               &
                     ZUSTAR2G, ZCDG, ZCHG, ZRIG                      )

! save values over snow for diagnostic purposes:

PUSTAR2SNOW(:) = ZUSTAR2G(:)
PCDSNOW(:)     = ZCDG(:)  
PCHSNOW(:)     = ZCHG(:)
PRISNOW(:)     = ZRIG(:)
!
!------------------------------------------------------------------------------
! Now calculate the aerodynamic resistance for the completely snow covered part,
! i.e. between the snow surface and atmosphere directly
!
 CALL PREPS_FOR_MEB_DRAG(.FALSE.,LFORC_MEASURE,                           &
                   PZ0_MEBN, PZ0H_MEBN, PZ0EFF_MEBN,                     &
                   PH_VEG, PZREF,                                        &
                   PSNOWTEMP, PTA, PQSATN, PQA, PUREF, PVMOD,            &
                   PEXNA, PEXNS, PDIRCOSZW, ZDISPH,                      &
                   ZVELC, ZVMOD, ZRINN, ZRANN,                           &
                   ZCHNN,ZCDNNN,ZCDNN                                    )
!
IF (LRRGUST_ARP) THEN

   ZRRCOR(:)=SQRT(1.0+((((ZFP(:)/(ZFP(:)+XRRSCALE))**XRRGAMMA)*XUTILGUST)**2) &
    & /(ZCDNN(:)*ZVMOD(:)**2))

   ZCDNN(:)  = ZCDNN(:)  * ZRRCOR(:)
   ZCHNN(:)  = ZCHNN(:)  * ZRRCOR(:)
   ZCDNNN(:) = ZCDNNN(:) * ZRRCOR(:)

ENDIF
!
PFLXC_N_A(:)=ZCHNN(:)*ZVMOD(:)*PRHOA(:)
!
! The effective momentum drag: 
!
PFLXC_MOM(:)=(1.-PPSN(:)*PPALPHAN(:))*PFLXC_MOM(:) +  &
            PPSN(:)*PPALPHAN(:)*ZCDNN(:)*ZVMOD(:)*PRHOA(:)
!
! Now calculate the aerodynamic resistance for  the whole meb-patch
!
! Calculate  the effective temperature
!
ZPSNA(:)   = PPSN(:)*PPALPHAN(:)
!
ZTEFF(:)   = (1.-ZPSNA(:))*PTC(:)+ ZPSNA(:)*PSNOWTEMP(:)
!
! Some additional diagnostics:
!
PUSTAR2SNOW(:) = (1.-ZPSNA(:))*PUSTAR2SNOW(:) + ZPSNA(:)*ZCDNN(:)*ZVMOD(:)**2
PCDSNOW(:)     = (1.-ZPSNA(:))*PCDSNOW(:)     + ZPSNA(:)*ZCDNN(:)
PCHSNOW(:)     = (1.-ZPSNA(:))*PCHSNOW(:)     + ZPSNA(:)*ZCHNN(:)
PRISNOW(:)     = (1.-ZPSNA(:))*PRISNOW(:)     + ZPSNA(:)*ZRINN(:)
!
!-------------------------------------------------------------------------------
!
 CALL PREPS_FOR_MEB_DRAG(.FALSE.,LFORC_MEASURE,            & 
                   PZ0_WITH_SNOW, PZ0H_WITH_SNOW, PZ0EFF, &
                   PH_VEG, PZREF,                         &
                   ZTEFF, PTA, PQS, PQA, PUREF, PVMOD,    &
                   PEXNA, PEXNS, PDIRCOSZW, ZDISPH,       &
                   ZVELC, ZVMOD, PRI, PRA,                &
                   PCH,PCDN,PCD                           )
!
IF (LRRGUST_ARP) THEN

   ZRRCOR(:)=SQRT(1.0+((((ZFP(:)/(ZFP(:)+XRRSCALE))**XRRGAMMA)*XUTILGUST)**2) &
    & /(PCD(:)*ZVMOD(:)**2))

   PCD(:)  = PCD(:)  * ZRRCOR(:)
   PCH(:)  = PCH(:)  * ZRRCOR(:)
   PCDN(:) = PCDN(:) * ZRRCOR(:)

ENDIF

!-------------------------------------------------------------------------------
! Now start to define the internal exchange coefficients 
!-------------------------------------------------------------------------------
!
! PFLXC_G_C between snow-free ground and canopy air
! PFLXC_N_C  between snow on the ground and canopy air
! PFLXC_VG_C  between canopy over snow-free ground and canopy air
! PFLXC_VN_C  between canopy over the snow on the ground and canopy air
!
! ZRA_G_C = resistance between canopy air and snow free ground
! ZRA_N_C = aerodynamic resistance between the partly
!          snowcovered (png>0), ground and canopy air
! ZG_VG_C = conductance between canopy and canopy air (1-png)
! ZG_VN_C = conductance between canopy and canopy air (png)
!
! The resistances between ground/snow and canopy air go to zero when lai => 0,
! limit exchange coefficients to 5000.
!
WHERE(ZRA_G_C(:) > ZRAEPS)
   PFLXC_G_C(:) = PRHOA(:)  / ZRA_G_C(:)
   PHSGL(:)     = ZRA_G_C(:)/(ZRA_G_C(:)+ZRSGL(:))
   PHSGF(:)     = ZRA_G_C(:)/(ZRA_G_C(:)+ZRSGF(:))
ELSEWHERE
   PFLXC_G_C(:) = XFLXMAX
   PHSGL(:)     = 1.
   PHSGF(:)     = 1.
END WHERE
!
WHERE(ZRA_N_C>ZRAEPS)
   PFLXC_N_C(:) = PRHOA(:)/ZRA_N_C(:)
ELSEWHERE
   PFLXC_N_C(:) = XFLXMAX
END WHERE
!
! Reduce sublimation from ground based snowpack as it
! becomes vanishingly thin. This is mainly just for numerical reasons within the snow scheme
! and has little to
! no impact on actual grid box average fluxes since sublimation is multiplied by the snow fraction,
! which in this case is quite small. 
!
PFLXC_N_C(:)  = PFLXC_N_C(:)*MIN(1., (PSNOWSWE(:) + PSR(:)*PTSTEP)/ZSNOWSWESMIN)
!
! unit conversion:
!
PFLXC_VG_C(:) = PRHOA(:)*ZG_VG_C(:)
PFLXC_VN_C(:) = PRHOA(:)*ZG_VN_C(:)
!
!-------------------------------------------------------------------------------
!
!*       5.     HALSTEAD COEFFICIENT (RELATIVE HUMIDITY OF THE VEGETATION)
!               ----------------------------------------------------------
!
ZDELTAMAX(:) = (1.-PCHIP(:))*(1.-PPSN(:)*PPALPHAN(:))*PRR(:)+ PWR(:)/PTSTEP
ZDENOM(:)    = (1.-PPSNCV(:))*XKDELTA_WR*                                               &
               ( PPSN(:)*(1.-PPALPHAN(:))*PFLXC_VN_C(:) + (1.-PPSN(:))*PFLXC_VG_C(:) )* &
               ( PQSATV(:)-PQC(:))
ZDELTAMAX(:) = MAX(0., MIN(1.0, ZDELTAMAX(:)/MAX(1.E-10,ZDENOM(:))))
!
ZDELTAV(:)   = XKDELTA_WR*MIN(ZDELTAMAX(:),PDELTA(:))
PDELTAVK(:)  = XKDELTA_WR*PDELTA(:)                    ! delta including K factor
!
PHVG(:) = 1. - MAX(0.,SIGN(1.,PQSATV(:)-PQC(:)))             &
          *(1.-ZDELTAV(:))*PRS_VG(:)*ZG_VG_C(:) / (1.+PRS_VG(:)*ZG_VG_C(:))
!
PHVN(:) = 1. - MAX(0.,SIGN(1.,PQSATV(:)-PQC(:)))             &
          *(1.-ZDELTAV(:))*PRS_VN(:)*ZG_VN_C(:) / (1.+PRS_VN(:)*ZG_VN_C(:))

! Diagnostics:
! Compute an effective canopy stomatal resistance (s m-1) and Halstead Coef: 
!
PRS(:)  = PPALPHAN(:)*PRS_VN(:) + (1.0-PPALPHAN(:))*PRS_VG(:)
PHV(:)  = PPALPHAN(:)*PHVN(:)   + (1.0-PPALPHAN(:))*PHVG(:)
!
!
!-------------------------------------------------------------------------
!*       6.    LITTER/GROUND RESISTANCE
!              ------------------------
!
! Inclusion of a ground resistance in the computation of the ground evaporation. 
! We use the existing LEG_DELTA (formerly a delta function) as a Beta-type-function
! (based on Sellers et al., 1992, J Geophys Res)
!
IF (OMEB_GNDRES) THEN
PLEG_DELTA(:)  = ZRA_G_C(:) /                                                  &
                ( ZRA_G_C(:) + EXP(ZRG_COEF1 - ZRG_COEF2 * PWG(:) / ZWSAT(:) ) ) 
PLEGI_DELTA(:) =ZRA_G_C(:) /                                                   &
                ( ZRA_G_C(:) + EXP(ZRG_COEF1 - ZRG_COEF2 * PWGI(:)/ ZWSAT(:) ) ) 

ELSE
PLEG_DELTA(:)  = 1.0
PLEGI_DELTA(:) = 1.0 
ENDIF
!
! when hu*qsat < qa, there are two
! possibilities
!
! a) low-level air is dry, i.e., 
!    qa < qsat
!
! NOTE the additional delta fn's
! here are needed owing to linearization 
! of Qsat in the surface energy budget.
!
WHERE ( PHUG(:)*PQSATG(:)  < PQC(:) .AND. PQSATG(:) > PQC(:) )
  PHUG(:)        = PQC(:) / PQSATG(:)
  PLEG_DELTA(:)  = 0.0
END WHERE
WHERE ( PHUGI(:)*PQSATG(:) < PQC(:) .AND. PQSATG(:) > PQC(:) )
  PHUGI(:)       = PQC(:) / PQSATG(:)
  PLEGI_DELTA(:) = 0.0
END WHERE
!
! b) low-level air is humid, i.e., qa >= qsat (condensation)
!
WHERE ( PHUG*PQSATG  < PQC .AND. PQSATG <= PQC )PHUG(:)  = 1.0
WHERE ( PHUGI*PQSATG < PQC .AND. PQSATG <= PQC )PHUGI(:) = 1.0
!
!
IF (LHOOK) CALL DR_HOOK('DRAG_MEB',1,ZHOOK_HANDLE)
!
END SUBROUTINE DRAG_MEB
