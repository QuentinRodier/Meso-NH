!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!#############################
SUBROUTINE AVERAGE_DIAG_EVAP_ISBA_n (DGEI, I, &
                                     PTSTEP,PRAIN,PSNOW)
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
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
!
!
USE MODD_DIAG_EVAP_ISBA_n, ONLY : DIAG_EVAP_ISBA_t
USE MODD_ISBA_n, ONLY : ISBA_t
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
TYPE(DIAG_EVAP_ISBA_t), INTENT(INOUT) :: DGEI
TYPE(ISBA_t), INTENT(INOUT) :: I
!
REAL,                  INTENT(IN) :: PTSTEP        ! time step (s)
REAL,    DIMENSION(:), INTENT(IN) :: PRAIN         ! rainfall rate
REAL,    DIMENSION(:), INTENT(IN) :: PSNOW         ! snowfall rate
!
!
!*      0.2    declarations of local variables
!
INTEGER :: JPATCH ! tile loop counter
INTEGER :: JJ
INTEGER           :: ISIZE_LMEB_PATCH   ! Number of patches where multi-energy balance should be applied
REAL, DIMENSION(SIZE(I%XPATCH,1)) :: ZSUMPATCH
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!       0.     Initialization
!              --------------
!
!
IF (LHOOK) CALL DR_HOOK('AVERAGE_DIAG_EVAP_ISBA_N',0,ZHOOK_HANDLE)
!
ISIZE_LMEB_PATCH=COUNT(I%LMEB_PATCH(:))
!
ZSUMPATCH(:) = 0.
DO JPATCH=1,SIZE(I%XPATCH,2)
   DO JJ=1,SIZE(I%XPATCH,1)
      ZSUMPATCH(JJ) = ZSUMPATCH(JJ) + I%XPATCH(JJ,JPATCH)
  ENDDO
ENDDO
!
!       1.     Surface Energy fluxes
!              -----------------------
!
IF (DGEI%LSURF_EVAP_BUDGET) THEN
!        
   DGEI%XAVG_LEG        (:) = 0.
   DGEI%XAVG_LEGI       (:) = 0.
   DGEI%XAVG_LEV        (:) = 0.
   DGEI%XAVG_LES        (:) = 0.
   DGEI%XAVG_LESL       (:) = 0.
   DGEI%XAVG_LER        (:) = 0.
   DGEI%XAVG_LETR       (:) = 0.
   DGEI%XAVG_EVAP       (:) = 0.
   DGEI%XAVG_SUBL       (:) = 0.
   DGEI%XAVG_SNDRIFT    (:) = 0.
   DGEI%XAVG_DRAIN      (:) = 0.
   DGEI%XAVG_QSB        (:) = 0.
   DGEI%XAVG_RUNOFF     (:) = 0.
   DGEI%XAVG_HORT       (:) = 0.
   DGEI%XAVG_DRIP       (:) = 0.
   DGEI%XAVG_RRVEG      (:) = 0.
   DGEI%XAVG_MELT       (:) = 0.
   DGEI%XAVG_IFLOOD     (:) = 0.
   DGEI%XAVG_PFLOOD     (:) = 0.
   DGEI%XAVG_LE_FLOOD   (:) = 0.
   DGEI%XAVG_LEI_FLOOD  (:) = 0.
   DGEI%XAVG_IRRIG_FLUX (:) = 0.
   DGEI%XAVG_GPP        (:) = 0.
   DGEI%XAVG_RESP_AUTO  (:) = 0.
   DGEI%XAVG_RESP_ECO   (:) = 0.
!
   IF (ISIZE_LMEB_PATCH>0) THEN
     DGEI%XAVG_LEVCV         (:) = 0.
     DGEI%XAVG_LESC          (:) = 0.
     DGEI%XAVG_LETRGV        (:) = 0.
     DGEI%XAVG_LETRCV        (:) = 0.
     DGEI%XAVG_LERGV         (:) = 0.
     DGEI%XAVG_LELITTER      (:) = 0.
     DGEI%XAVG_LELITTERI     (:) = 0.
     DGEI%XAVG_DRIPLIT       (:) = 0.
     DGEI%XAVG_RRLIT         (:) = 0.
     DGEI%XAVG_LERCV         (:) = 0.
     DGEI%XAVG_LE_C_A        (:) = 0.
     DGEI%XAVG_LE_V_C        (:) = 0.
     DGEI%XAVG_LE_G_C        (:) = 0.
     DGEI%XAVG_LE_N_C        (:) = 0.
     !
     DGEI%XAVG_SWNET_V       (:) = 0.
     DGEI%XAVG_SWNET_G       (:) = 0.
     DGEI%XAVG_SWNET_N       (:) = 0.
     DGEI%XAVG_SWNET_NS      (:) = 0.
     DGEI%XAVG_LWNET_V       (:) = 0.
     DGEI%XAVG_LWNET_G       (:) = 0.
     DGEI%XAVG_LWNET_N       (:) = 0.
     DGEI%XAVG_SWDOWN_GN     (:) = 0.
     DGEI%XAVG_LWDOWN_GN     (:) = 0.
     DGEI%XAVG_H_V_C         (:) = 0.
     DGEI%XAVG_H_G_C         (:) = 0.
     DGEI%XAVG_H_C_A         (:) = 0.
     DGEI%XAVG_H_N_C         (:) = 0.
     DGEI%XAVG_SR_GN         (:) = 0.
     DGEI%XAVG_MELTCV        (:) = 0.
     DGEI%XAVG_FRZCV         (:) = 0.
   ENDIF
!
  DO JPATCH=1,SIZE(I%XPATCH,2)
!cdir nodep
    DO JJ=1,SIZE(ZSUMPATCH)
      IF (ZSUMPATCH(JJ) > 0.) THEN
!
! Latent heat of evaporation over the ground
!
        DGEI%XAVG_LEG(JJ)  = DGEI%XAVG_LEG(JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XLEG(JJ,JPATCH)
!
! Surface soil ice sublimation
!
        DGEI%XAVG_LEGI(JJ) = DGEI%XAVG_LEGI(JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XLEGI(JJ,JPATCH)
!
! Latent heat of evaporation over vegetation
!
        DGEI%XAVG_LEV(JJ)  = DGEI%XAVG_LEV(JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XLEV(JJ,JPATCH)
!
! Latent heat of sublimation over snow
!
        DGEI%XAVG_LES(JJ)  = DGEI%XAVG_LES(JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XLES(JJ,JPATCH)
!
! Latent heat of evaporation of liquid water over snow
!
        DGEI%XAVG_LESL(JJ)  = DGEI%XAVG_LESL(JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XLESL(JJ,JPATCH)
!
! Evaporation from canopy water interception
!
        DGEI%XAVG_LER(JJ)  = DGEI%XAVG_LER(JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XLER(JJ,JPATCH)
!
! Evapotranspiration of the vegetation
!
        DGEI%XAVG_LETR(JJ)  = DGEI%XAVG_LETR(JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XLETR(JJ,JPATCH)
!
! Evapotranspiration
!
        DGEI%XAVG_EVAP(JJ)  = DGEI%XAVG_EVAP(JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XEVAP(JJ,JPATCH)
!
! Sublimation
!
        DGEI%XAVG_SUBL(JJ)  = DGEI%XAVG_SUBL(JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XSUBL(JJ,JPATCH)
!
! Blowing snow sublimation (ES or Crocus)
!
        DGEI%XAVG_SNDRIFT(JJ)  = DGEI%XAVG_SNDRIFT(JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XSNDRIFT(JJ,JPATCH)
!
! Soil drainage flux
!
        DGEI%XAVG_DRAIN(JJ)  = DGEI%XAVG_DRAIN(JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XDRAIN(JJ,JPATCH)
!
! Soil lateral subsurface flux
!
        DGEI%XAVG_QSB(JJ)  = DGEI%XAVG_QSB(JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XQSB(JJ,JPATCH)        
!
! Supersaturation runoff
!
        DGEI%XAVG_RUNOFF(JJ) = DGEI%XAVG_RUNOFF(JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XRUNOFF(JJ,JPATCH)
!
! Horton runoff
!
        DGEI%XAVG_HORT(JJ)  = DGEI%XAVG_HORT(JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XHORT(JJ,JPATCH)
!
! Vegetation dripping
!
        DGEI%XAVG_DRIP(JJ)  = DGEI%XAVG_DRIP(JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XDRIP(JJ,JPATCH)
!
! Precipitation intercepted by the vegetation
!
        DGEI%XAVG_RRVEG(JJ)  = DGEI%XAVG_RRVEG(JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XRRVEG(JJ,JPATCH)
!      
! Snow melt
!
        DGEI%XAVG_MELT(JJ)  = DGEI%XAVG_MELT(JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XMELT(JJ,JPATCH)
!      
! Flood infiltartion
!
        DGEI%XAVG_IFLOOD(JJ) = DGEI%XAVG_IFLOOD(JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XIFLOOD(JJ,JPATCH)
!      
! Precipitation intercepted by the floodplains
!     
        DGEI%XAVG_PFLOOD(JJ) = DGEI%XAVG_PFLOOD(JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XPFLOOD(JJ,JPATCH)
!      
! Floodplains evaporation
!     
        DGEI%XAVG_LE_FLOOD (JJ) = DGEI%XAVG_LE_FLOOD (JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XLE_FLOOD (JJ,JPATCH)
        DGEI%XAVG_LEI_FLOOD(JJ) = DGEI%XAVG_LEI_FLOOD(JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XLEI_FLOOD(JJ,JPATCH)
!      
! irrigation rate (as soil input)
!
        DGEI%XAVG_IRRIG_FLUX(JJ)  = DGEI%XAVG_IRRIG_FLUX(JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XIRRIG_FLUX(JJ,JPATCH)
!
! Gross primary production
!
        DGEI%XAVG_GPP(JJ) = DGEI%XAVG_GPP(JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XGPP(JJ,JPATCH)
!
! Autotrophic respiration
!   
        DGEI%XAVG_RESP_AUTO(JJ) = DGEI%XAVG_RESP_AUTO(JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XRESP_AUTO(JJ,JPATCH)
!
! Ecosystem respiration
!
        DGEI%XAVG_RESP_ECO(JJ) = DGEI%XAVG_RESP_ECO(JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XRESP_ECO(JJ,JPATCH)  
!        
        IF (ISIZE_LMEB_PATCH>0) THEN
          DGEI%XAVG_LEVCV(JJ) = DGEI%XAVG_LEVCV(JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XLEVCV(JJ,JPATCH)
          DGEI%XAVG_LESC(JJ) = DGEI%XAVG_LESC(JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XLESC(JJ,JPATCH)
          DGEI%XAVG_LETRCV(JJ) = DGEI%XAVG_LETRCV(JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XLETRCV(JJ,JPATCH)
          DGEI%XAVG_LELITTER(JJ) = DGEI%XAVG_LELITTER(JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XLELITTER(JJ,JPATCH)
          DGEI%XAVG_LELITTERI(JJ) = DGEI%XAVG_LELITTERI(JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XLELITTERI(JJ,JPATCH)
          DGEI%XAVG_DRIPLIT(JJ) = DGEI%XAVG_DRIPLIT(JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XDRIPLIT(JJ,JPATCH)
          DGEI%XAVG_RRLIT(JJ) = DGEI%XAVG_RRLIT(JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XRRLIT(JJ,JPATCH)
          DGEI%XAVG_LERCV(JJ) = DGEI%XAVG_LERCV(JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XLERCV(JJ,JPATCH)
          DGEI%XAVG_LE_C_A(JJ) = DGEI%XAVG_LE_C_A(JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XLE_C_A(JJ,JPATCH)
          DGEI%XAVG_LE_V_C(JJ) = DGEI%XAVG_LE_V_C(JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XLE_V_C(JJ,JPATCH)
          DGEI%XAVG_LE_G_C(JJ) = DGEI%XAVG_LE_G_C(JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XLE_G_C(JJ,JPATCH)
          DGEI%XAVG_LE_N_C(JJ) = DGEI%XAVG_LE_N_C(JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XLE_N_C(JJ,JPATCH)
          DGEI%XAVG_SWNET_V(JJ) = DGEI%XAVG_SWNET_V(JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XSWNET_V(JJ,JPATCH)
          DGEI%XAVG_SWNET_G(JJ) = DGEI%XAVG_SWNET_G(JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XSWNET_G(JJ,JPATCH)
          DGEI%XAVG_SWNET_N(JJ) = DGEI%XAVG_SWNET_N(JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XSWNET_N(JJ,JPATCH)
          DGEI%XAVG_SWNET_NS(JJ) = DGEI%XAVG_SWNET_NS(JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XSWNET_NS(JJ,JPATCH)
          DGEI%XAVG_LWNET_V(JJ) = DGEI%XAVG_LWNET_V(JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XLWNET_V(JJ,JPATCH)
          DGEI%XAVG_LWNET_G(JJ) = DGEI%XAVG_LWNET_G(JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XLWNET_G(JJ,JPATCH)
          DGEI%XAVG_LWNET_N(JJ) = DGEI%XAVG_LWNET_N(JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XLWNET_N(JJ,JPATCH)
          DGEI%XAVG_SWDOWN_GN(JJ) = DGEI%XAVG_SWDOWN_GN(JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XSWDOWN_GN(JJ,JPATCH)
          DGEI%XAVG_LWDOWN_GN(JJ) = DGEI%XAVG_LWDOWN_GN(JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XLWDOWN_GN(JJ,JPATCH)
          DGEI%XAVG_H_V_C(JJ) = DGEI%XAVG_H_V_C(JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XH_V_C(JJ,JPATCH)
          DGEI%XAVG_H_G_C(JJ) = DGEI%XAVG_H_G_C(JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XH_G_C(JJ,JPATCH)
          DGEI%XAVG_H_C_A(JJ) = DGEI%XAVG_H_C_A(JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XH_C_A(JJ,JPATCH)
          DGEI%XAVG_H_N_C(JJ) = DGEI%XAVG_H_N_C(JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XH_N_C(JJ,JPATCH)
          DGEI%XAVG_SR_GN(JJ) = DGEI%XAVG_SR_GN(JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XSR_GN(JJ,JPATCH)
          DGEI%XAVG_MELTCV(JJ) = DGEI%XAVG_MELTCV(JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XMELTCV(JJ,JPATCH)
          DGEI%XAVG_FRZCV(JJ) = DGEI%XAVG_FRZCV(JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XFRZCV(JJ,JPATCH)
        ENDIF
        !
      ENDIF
    END DO
  ENDDO
!
! Isba water budget and reservoir time tendencies
!
  IF(DGEI%LWATER_BUDGET)THEN
!  
    DGEI%XRAINFALL  (:) = PRAIN(:) * PTSTEP
    DGEI%XSNOWFALL  (:) = PSNOW(:) * PTSTEP
    DGEI%XAVG_DWG   (:) = 0.0
    DGEI%XAVG_DWGI  (:) = 0.0
    DGEI%XAVG_DWR   (:) = 0.0
    DGEI%XAVG_DSWE  (:) = 0.0
    DGEI%XAVG_WATBUD(:) = 0.0
!
    DO JPATCH=1,SIZE(I%XPATCH,2)
!     cdir nodep
      DO JJ=1,SIZE(ZSUMPATCH)
        IF (ZSUMPATCH(JJ) > 0.) THEN
!
           DGEI%XAVG_DWG   (JJ) = DGEI%XAVG_DWG   (JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XDWG   (JJ,JPATCH)
           DGEI%XAVG_DWGI  (JJ) = DGEI%XAVG_DWGI  (JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XDWGI  (JJ,JPATCH)
           DGEI%XAVG_DWR   (JJ) = DGEI%XAVG_DWR   (JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XDWR   (JJ,JPATCH)
           DGEI%XAVG_DSWE  (JJ) = DGEI%XAVG_DSWE  (JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XDSWE  (JJ,JPATCH)
           DGEI%XAVG_WATBUD(JJ) = DGEI%XAVG_WATBUD(JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XWATBUD(JJ,JPATCH)
!
        ENDIF
      ENDDO
    ENDDO
!
  ENDIF
!
END IF
!
!
!       2.     Surface Cumulated Energy fluxes
!              -------------------------------
!
IF (DGEI%LSURF_BUDGETC) THEN
   DGEI%XAVG_RNC        (:) = 0.
   DGEI%XAVG_HC         (:) = 0.
   DGEI%XAVG_LEC        (:) = 0.
   DGEI%XAVG_GFLUXC     (:) = 0.
   DGEI%XAVG_LEIC       (:) = 0.
   DGEI%XAVG_LEGC       (:) = 0.
   DGEI%XAVG_LEGIC      (:) = 0.
   DGEI%XAVG_LEVC       (:) = 0.
   DGEI%XAVG_LESAC      (:) = 0.
   DGEI%XAVG_LESLC      (:) = 0.
   DGEI%XAVG_LERC       (:) = 0.
   DGEI%XAVG_LETRC      (:) = 0.
   DGEI%XAVG_EVAPC      (:) = 0.
   DGEI%XAVG_SUBLC      (:) = 0.
   DGEI%XAVG_SNDRIFTC   (:) = 0.
   DGEI%XAVG_DRAINC     (:) = 0.
   DGEI%XAVG_QSBC       (:) = 0.
   DGEI%XAVG_RUNOFFC    (:) = 0.
   DGEI%XAVG_HORTC      (:) = 0.
   DGEI%XAVG_DRIPC      (:) = 0.
   DGEI%XAVG_RRVEGC     (:) = 0.
   DGEI%XAVG_MELTC      (:) = 0.
   DGEI%XAVG_IFLOODC    (:) = 0.
   DGEI%XAVG_PFLOODC    (:) = 0.
   DGEI%XAVG_LE_FLOODC  (:) = 0.
   DGEI%XAVG_LEI_FLOODC (:) = 0.
   DGEI%XAVG_IRRIG_FLUXC(:) = 0.
   DGEI%XAVG_GPPC       (:) = 0.
   DGEI%XAVG_RESPC_AUTO (:) = 0.
   DGEI%XAVG_RESPC_ECO  (:) = 0.
!
   IF (ISIZE_LMEB_PATCH>0) THEN
        DGEI%XAVG_LEVCVC    (:) = 0.
        DGEI%XAVG_LESCC     (:) = 0.
        DGEI%XAVG_LETRGVC   (:) = 0.
        DGEI%XAVG_LETRCVC   (:) = 0.
        DGEI%XAVG_LERGVC    (:) = 0.
        DGEI%XAVG_LERCVC    (:) = 0.
        DGEI%XAVG_LE_C_AC   (:) = 0.
        DGEI%XAVG_LE_V_CC   (:) = 0.
        DGEI%XAVG_LE_G_CC   (:) = 0.
        DGEI%XAVG_LE_N_CC   (:) = 0.
        DGEI%XAVG_SWNET_VC     (:) = 0.
        DGEI%XAVG_SWNET_GC     (:) = 0.
        DGEI%XAVG_SWNET_NC     (:) = 0.
        DGEI%XAVG_SWNET_NSC    (:) = 0.
        DGEI%XAVG_LWNET_VC     (:) = 0.
        DGEI%XAVG_LWNET_GC     (:) = 0.
        DGEI%XAVG_LWNET_NC     (:) = 0.
        DGEI%XAVG_SWDOWN_GNC   (:) = 0.
        DGEI%XAVG_LWDOWN_GNC   (:) = 0.
        DGEI%XAVG_H_V_CC       (:) = 0.
        DGEI%XAVG_H_G_CC       (:) = 0.
        DGEI%XAVG_H_C_AC       (:) = 0.
        DGEI%XAVG_H_N_CC       (:) = 0.
        DGEI%XAVG_SR_GNC       (:) = 0.
        DGEI%XAVG_MELTCVC      (:) = 0.
        DGEI%XAVG_FRZCVC       (:) = 0.
   ENDIF
!
  DO JPATCH=1,SIZE(I%XPATCH,2)
!cdir nodep
    DO JJ=1,SIZE(ZSUMPATCH)
      IF (ZSUMPATCH(JJ) > 0.) THEN
!
! Net radiation
!
        DGEI%XAVG_RNC(JJ)  = DGEI%XAVG_RNC(JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XRNC(JJ,JPATCH)
!
! Sensible heat flux
!
        DGEI%XAVG_HC(JJ)  = DGEI%XAVG_HC(JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XHC(JJ,JPATCH)
!
! Total latent heat flux
!
        DGEI%XAVG_LEC(JJ)  = DGEI%XAVG_LEC(JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XLEC(JJ,JPATCH)
!
! Storage flux
!
        DGEI%XAVG_GFLUXC(JJ)  = DGEI%XAVG_GFLUXC(JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XGFLUXC(JJ,JPATCH)
!
! Total surface sublimation
!
        DGEI%XAVG_LEIC(JJ)  = DGEI%XAVG_LEIC(JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XLEIC(JJ,JPATCH)
!
! Latent heat of evaporation over the ground
!
        DGEI%XAVG_LEGC(JJ)  = DGEI%XAVG_LEGC(JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XLEGC(JJ,JPATCH)
!
! Surface soil ice sublimation
!
        DGEI%XAVG_LEGIC(JJ)  = DGEI%XAVG_LEGIC(JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XLEGIC(JJ,JPATCH)
!
! Latent heat of evaporation over vegetation
!
        DGEI%XAVG_LEVC(JJ)  = DGEI%XAVG_LEVC(JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XLEVC(JJ,JPATCH)
!
! Latent heat of sublimation over snow
!
        DGEI%XAVG_LESAC(JJ)  = DGEI%XAVG_LESAC(JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XLESAC(JJ,JPATCH)
!
! Latent heat of evaporation of liquid water over snow
!
        DGEI%XAVG_LESLC(JJ)  = DGEI%XAVG_LESLC(JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XLESLC(JJ,JPATCH)
!
! Evaporation from canopy water interception
!
        DGEI%XAVG_LERC(JJ)  = DGEI%XAVG_LERC(JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XLERC(JJ,JPATCH)
!
! Evapotranspiration of the vegetation
!
        DGEI%XAVG_LETRC(JJ)  = DGEI%XAVG_LETRC(JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XLETRC(JJ,JPATCH)
!
! Evapotranspiration
!
        DGEI%XAVG_EVAPC(JJ)  = DGEI%XAVG_EVAPC(JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XEVAPC(JJ,JPATCH)
!
! Sublimation
!
        DGEI%XAVG_SUBLC(JJ)  = DGEI%XAVG_SUBLC(JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XSUBLC(JJ,JPATCH)
!
! Blowing snow sublimation (ES or Crocus)
!
        DGEI%XAVG_SNDRIFTC(JJ)  = DGEI%XAVG_SNDRIFTC(JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XSNDRIFTC(JJ,JPATCH)
!
! Soil drainage flux
!
        DGEI%XAVG_DRAINC(JJ)  = DGEI%XAVG_DRAINC(JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XDRAINC(JJ,JPATCH)
!
! Soil lateral subsurface flux
!
        DGEI%XAVG_QSBC(JJ)  = DGEI%XAVG_QSBC(JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XQSBC(JJ,JPATCH)        
!
! Supersaturation runoff
!
        DGEI%XAVG_RUNOFFC(JJ)  = DGEI%XAVG_RUNOFFC(JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XRUNOFFC(JJ,JPATCH)
!
! Horton runoff
!
        DGEI%XAVG_HORTC(JJ)  = DGEI%XAVG_HORTC(JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XHORTC(JJ,JPATCH)
!
! Vegetation dripping
!
        DGEI%XAVG_DRIPC(JJ)  = DGEI%XAVG_DRIPC(JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XDRIPC(JJ,JPATCH)
!
! precipitation intercepted by the vegetation
!
        DGEI%XAVG_RRVEGC(JJ)  = DGEI%XAVG_RRVEGC(JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XRRVEGC(JJ,JPATCH)
!      
! Snow melt
!
        DGEI%XAVG_MELTC(JJ)  = DGEI%XAVG_MELTC(JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XMELTC(JJ,JPATCH)
!      
! Flood infiltartion
!
        DGEI%XAVG_IFLOODC(JJ) = DGEI%XAVG_IFLOODC(JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XIFLOODC(JJ,JPATCH)
!      
! Precipitation intercepted by the floodplains
!     
        DGEI%XAVG_PFLOODC(JJ) = DGEI%XAVG_PFLOODC(JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XPFLOODC(JJ,JPATCH)
!      
! Floodplains evaporation
!     
        DGEI%XAVG_LE_FLOODC (JJ) = DGEI%XAVG_LE_FLOODC (JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XLE_FLOODC (JJ,JPATCH)
        DGEI%XAVG_LEI_FLOODC(JJ) = DGEI%XAVG_LEI_FLOODC(JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XLEI_FLOODC(JJ,JPATCH)
!      
! irrigation rate (as soil input)
!
        DGEI%XAVG_IRRIG_FLUXC(JJ)  = DGEI%XAVG_IRRIG_FLUXC(JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XIRRIG_FLUXC(JJ,JPATCH)
!
! Gross primary production
!
        DGEI%XAVG_GPPC(JJ) = DGEI%XAVG_GPPC(JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XGPPC(JJ,JPATCH)
!
! Autotrophic respiration
!   
        DGEI%XAVG_RESPC_AUTO(JJ) = DGEI%XAVG_RESPC_AUTO(JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XRESPC_AUTO(JJ,JPATCH)
!
! Ecosystem respiration
!
        DGEI%XAVG_RESPC_ECO(JJ) = DGEI%XAVG_RESPC_ECO(JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XRESPC_ECO(JJ,JPATCH)
!      
        IF (ISIZE_LMEB_PATCH>0) THEN
          DGEI%XAVG_LEVCVC(JJ) = DGEI%XAVG_LEVCVC(JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XLEVCVC(JJ,JPATCH)
          DGEI%XAVG_LESCC(JJ) = DGEI%XAVG_LESCC(JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XLESCC(JJ,JPATCH)
!         DGEI%XAVG_LETRGVC(JJ) = DGEI%XAVG_LETRGVC(JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XLETRGVC(JJ,JPATCH)
          DGEI%XAVG_LETRCVC(JJ) = DGEI%XAVG_LETRCVC(JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XLETRCVC(JJ,JPATCH)
!         DGEI%XAVG_LERGVC(JJ) = DGEI%XAVG_LERGVC(JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XLERGVC(JJ,JPATCH)
          DGEI%XAVG_LERCVC(JJ) = DGEI%XAVG_LERCVC(JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XLERCVC(JJ,JPATCH)
          DGEI%XAVG_LE_C_AC(JJ) = DGEI%XAVG_LE_C_AC(JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XLE_C_AC(JJ,JPATCH)
          DGEI%XAVG_LE_V_CC(JJ) = DGEI%XAVG_LE_V_CC(JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XLE_V_CC(JJ,JPATCH)
          DGEI%XAVG_LE_G_CC(JJ) = DGEI%XAVG_LE_G_CC(JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XLE_G_CC(JJ,JPATCH)
          DGEI%XAVG_LE_N_CC(JJ) = DGEI%XAVG_LE_N_CC(JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XLE_N_CC(JJ,JPATCH)
          DGEI%XAVG_SWNET_VC(JJ) = DGEI%XAVG_SWNET_VC(JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XSWNET_VC(JJ,JPATCH)
          DGEI%XAVG_SWNET_GC(JJ) = DGEI%XAVG_SWNET_GC(JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XSWNET_GC(JJ,JPATCH)
          DGEI%XAVG_SWNET_NC(JJ) = DGEI%XAVG_SWNET_NC(JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XSWNET_NC(JJ,JPATCH)
          DGEI%XAVG_SWNET_NSC(JJ) = DGEI%XAVG_SWNET_NSC(JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XSWNET_NSC(JJ,JPATCH)
          DGEI%XAVG_LWNET_VC(JJ) = DGEI%XAVG_LWNET_VC(JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XLWNET_VC(JJ,JPATCH)
          DGEI%XAVG_LWNET_GC(JJ) = DGEI%XAVG_LWNET_GC(JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XLWNET_GC(JJ,JPATCH)
          DGEI%XAVG_LWNET_NC(JJ) = DGEI%XAVG_LWNET_NC(JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XLWNET_NC(JJ,JPATCH)
          DGEI%XAVG_SWDOWN_GNC(JJ) = DGEI%XAVG_SWDOWN_GNC(JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XSWDOWN_GNC(JJ,JPATCH)
          DGEI%XAVG_LWDOWN_GNC(JJ) = DGEI%XAVG_LWDOWN_GNC(JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XLWDOWN_GNC(JJ,JPATCH)
          DGEI%XAVG_H_V_CC(JJ) = DGEI%XAVG_H_V_CC(JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XH_V_CC(JJ,JPATCH)
          DGEI%XAVG_H_G_CC(JJ) = DGEI%XAVG_H_G_CC(JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XH_G_CC(JJ,JPATCH)
          DGEI%XAVG_H_C_AC(JJ) = DGEI%XAVG_H_C_AC(JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XH_C_AC(JJ,JPATCH)
          DGEI%XAVG_H_N_CC(JJ) = DGEI%XAVG_H_N_CC(JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XH_N_CC(JJ,JPATCH)
          DGEI%XAVG_SR_GNC(JJ) = DGEI%XAVG_SR_GNC(JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XSR_GNC(JJ,JPATCH)
          DGEI%XAVG_MELTCVC(JJ) = DGEI%XAVG_MELTCVC(JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XMELTCVC(JJ,JPATCH)
          DGEI%XAVG_FRZCVC(JJ) = DGEI%XAVG_FRZCVC(JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XFRZCVC(JJ,JPATCH)
        ENDIF
        !
      ENDIF
    ENDDO
  END DO
!
! Isba water budget and reservoir time tendencies
!
  IF(DGEI%LWATER_BUDGET)THEN
!  
    DGEI%XRAINFALLC  (:) = DGEI%XRAINFALLC (:) + PRAIN(:) * PTSTEP
    DGEI%XSNOWFALLC  (:) = DGEI%XSNOWFALLC (:) + PSNOW(:) * PTSTEP
    DGEI%XAVG_DWGC   (:) = 0.0
    DGEI%XAVG_DWGIC  (:) = 0.0
    DGEI%XAVG_DWRC   (:) = 0.0
    DGEI%XAVG_DSWEC  (:) = 0.0
    DGEI%XAVG_WATBUDC(:) = 0.0
!
    DO JPATCH=1,SIZE(I%XPATCH,2)
!     cdir nodep
      DO JJ=1,SIZE(ZSUMPATCH)
        IF (ZSUMPATCH(JJ) > 0.) THEN
!
           DGEI%XAVG_DWGC   (JJ) = DGEI%XAVG_DWGC   (JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XDWGC   (JJ,JPATCH)
           DGEI%XAVG_DWGIC  (JJ) = DGEI%XAVG_DWGIC  (JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XDWGIC  (JJ,JPATCH)
           DGEI%XAVG_DWRC   (JJ) = DGEI%XAVG_DWRC   (JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XDWRC   (JJ,JPATCH)
           DGEI%XAVG_DSWEC  (JJ) = DGEI%XAVG_DSWEC  (JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XDSWEC  (JJ,JPATCH)
           DGEI%XAVG_WATBUDC(JJ) = DGEI%XAVG_WATBUDC(JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XWATBUDC(JJ,JPATCH)
!
        ENDIF
      ENDDO
    ENDDO
!
  ENDIF
!
! Ice calving flux
!  
  IF(I%LGLACIER)THEN 
    DGEI%XAVG_ICEFLUXC(:)= 0.
    DO JPATCH=1,SIZE(I%XPATCH,2)
!     cdir nodep  
      DO JJ=1,SIZE(ZSUMPATCH)
         IF(ZSUMPATCH(JJ) > 0.)THEN
            DGEI%XAVG_ICEFLUXC(JJ) = DGEI%XAVG_ICEFLUXC(JJ) + I%XPATCH(JJ,JPATCH) * DGEI%XICEFLUXC(JJ,JPATCH)      
         ENDIF
      END DO
    END DO
  END IF
!  
END IF
!
IF (LHOOK) CALL DR_HOOK('AVERAGE_DIAG_EVAP_ISBA_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE AVERAGE_DIAG_EVAP_ISBA_n
