!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
      SUBROUTINE HYDRO_SOILDIF(PTSTEP,                                              &
                               PBCOEF,PWSAT,PCONDSAT,PMPOTSAT,PWFC,PDG,PDZG,PDZDIF, &
                               PPG,PLETR,PLEG,PEVAPCOR,PF2WGHT,                     &                              
                               PWG,PWGI,PTG,PPS,PQSAT,PQSATI,PWDRAIN,               &
                               PDRAIN,PHORTON,HKSAT,HSOC,PWWILT,HHORT,PFSAT,        &
                               KWG_LAYER,KMAX_LAYER,KLAYER_HORT                     )
!     ##########################################################################
!
!
!!****  *HYDRO_SOILDIF*  
!
!!    PURPOSE
!!    -------
!     This subroutine solves the 1-D (z) mass conservation equation
!     (mixed-form Richard's equation: tendency in terms of volumetric
!     water content, gradient in terms of matric potential)
!     for liquid water (using Darcy's Law for the vertical flux)
!     together with the Clapp and Hornberger (1978) simplification to
!     the Brooks and Corey (1966) empirical model for relating matric
!     potential and hydraulic conductivity to soil water content.
!     Any set of parameters can be used (eg. Clapp and Hornberger 1978;
!     Cosby et al. 1984; etc.) Modifications to the equations is also
!     made for the Van Genucten model/relationships. The equations
!     also incorporate vapor transfer for dry soils. The soil porosity
!     is modified in the presence of soil ice. Soil ice content
!     is also updated herein due to changes resulting from sublimation.
!     The layer averaged set of equations are time differenced
!     using an implicit time scheme. 
!     The equations/model *assume* a heterogeneous soil texture profile,
!     however, if the soil properties are homogeneous, the equations
!     collapse into the standard homogeneous approach (i.e. give the
!     same results as). The eqs are solved rapidly by taking advantage of the
!     fact that the matrix is tridiagonal. 
!     Note that the exponential profile of hydraulic conductivity with soil
!     depth is applied to interfacial conductivity (or interblock)
!
!     
!!**  METHOD
!!    ------
!
!     Direct calculation
!
!!    EXTERNAL
!!    --------
!
!     None
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    USE MODD_CST
!!    USE MODI_TRIDIAG_GROUND
!!      
!!    REFERENCE
!!    ---------
!!
!!    Boone (2000)
!!    Boone et al. (2000)
!!      
!!    AUTHOR
!!    ------
!!	A. Boone          * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    16/02/00 Boone
!!      Modif       04/2010  B.Decharme: geometric mean for interfacial conductivity
!!                                       Brook and Corey 
!!      Modif       08/2011  B.Decharme: Optimization using global loops
!!                  10/12    B.Decharme: EVAPCOR snow correction in DIF
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_SURF_PAR, ONLY : XUNDEF, NUNDEF
USE MODD_CSTS,     ONLY : XLSTT, XRHOLW, XLVTT
USE MODD_ISBA_PAR, ONLY : XWGMIN
!
USE MODE_HYDRO_DIF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
REAL, INTENT(IN)                    :: PTSTEP ! Model time step (s)
!
REAL, DIMENSION(:), INTENT(IN)      :: PPS, PPG, PLETR, PLEG, PEVAPCOR, PWDRAIN, PFSAT
!                                      PPS    = surface pressure (Pa)
!                                      PPG    = throughfall rate: 
!                                               rate at which water reaches the surface
!                                               of the soil (from snowmelt, rain, canopy
!                                               drip, etc...) (m/s)
!                                      PLETR  = transpiration rate (m/s)
!                                      PLEG   = bare-soil evaporation rate (m/s)
!                                      PEVAPCOR = correction for any excess evaporation 
!                                                from snow as it completely ablates (m/s)
!                                      PWDRAIN= minimum Wg for drainage (m3 m-3)
!                                      PFSAT  = Saturated fraction
!
REAL, DIMENSION(:,:), INTENT(IN)    :: PQSAT,PQSATI
!                                      specific humidity at saturation
!
REAL, DIMENSION(:,:), INTENT(IN)    :: PTG, PDG, PDZG, PDZDIF
!                                      PTG = layer-average soil temperatures (K)
!                                      PDG = soil layer depth       (m)
!                                      PDZG= soil layer thicknesses (m)
!                                      PDZDIF = distance between the midpoints of consecutive layers (m)
!
REAL, DIMENSION(:,:), INTENT(IN)    :: PWSAT, PCONDSAT, PF2WGHT, PWFC
!                                      PWSAT        = porosity profile (m3 m-3)
!                                      PCONDSAT     = hydraulic conductivity at saturation (m s-1)
!                                      PF2WGHT      = root-zone transpiration weights (-)
!                                      PWFC         = field capacity water content (m3 m-3)
!
REAL, DIMENSION(:,:), INTENT(IN)    :: PBCOEF,PMPOTSAT
!                                      PMPOTSAT = matric potential at saturation (m) (BC parameters)
!                                      PBCOEF   = slope of the retention curve (-) (BC parameters)
!
INTEGER, DIMENSION(:), INTENT(IN)   :: KWG_LAYER  
!                                      KWG_LAYER = Number of soil moisture layers
!
INTEGER,               INTENT(IN)   :: KMAX_LAYER  
!                                      KMAX_LAYER = Max number of soil moisture layers (DIF option)
!
INTEGER,               INTENT(IN)   :: KLAYER_HORT! DIF optimization
!
REAL, DIMENSION(:,:), INTENT(INOUT) :: PWG, PWGI
!                                      PWG  = volumetric liquid water content (m3 m-3) 
!                                      PWGI = volumetric ice content (m3 m-3)
!
REAL, DIMENSION(:), INTENT(OUT)     :: PDRAIN, PHORTON
!                                      PDRAIN   = drainage (flux out of model base) (kg m-2 s-1)
!                                      PHORTON  = runoff (due to saturation (lateral) (kg m-2 s-1)
!
 CHARACTER(LEN=*),     INTENT(IN)    :: HHORT    ! Hortonian runoff
!
 CHARACTER(LEN=*),     INTENT(IN)    :: HKSAT    ! soil hydraulic profil option
!                                               ! 'DEF'  = ISBA homogenous soil
!                                               ! 'SGH'  = ksat exponential decay
!
 CHARACTER(LEN=*),     INTENT(IN)    :: HSOC     ! soil organic carbon profil option
!                                               ! 'DEF'  = ISBA homogenous soil
!                                               ! 'SGH'  = SOC profile
!
REAL, DIMENSION(:,:), INTENT(IN)    :: PWWILT
!                                      PWWILT = wilting point volumetric water
!                                             content (m3 m-3)
!
!
!*      0.2    declarations of local variables
!
INTEGER                             :: JJ, JL    ! loop control
!
INTEGER                             :: INI, INL, IDEPTH ! Number of point and grid layers
!
REAL, DIMENSION(SIZE(PDZG,1))       :: ZINFILTMAX, ZINFILTC, ZEXCESS
!                                      ZINFILTMAX = maximum allowable infiltration rate
!                                                   (from Darcy's Law) (m s-1)
!                                      ZEXCESS    = working variable: excess soil water
!                                                   which is used as a constraint 
!                                      
!
REAL, DIMENSION(SIZE(PDZG,1),SIZE(PDZG,2)) :: ZWFLUX, ZDFLUXDT1, ZDFLUXDT2, ZWFLUXN
!                                      ZWFLUX    = vertical soil water flux (+ up) (m s-1)
!                                      ZDFLUXDT  = total vertical flux derrivative
!                                      ZDFLUXDT1 = vertical flux derrivative: dF_j/dw_j
!                                      ZDFLUXDT2 = vertical flux derrivative: dF_j/dw_j+1
!                                      ZWFLUXN   = vertical soil water flux at end of time 
!
REAL, DIMENSION(SIZE(PDZG,1),SIZE(PDZG,2)) :: ZPSI, ZK, ZNU, ZWSAT, ZHEAD, &
                                              ZVAPCOND, ZFRZ, ZKI,         &
                                              ZCAPACITY, ZINFNEG
!                                      ZNU       = interfacial total conductivity (m s-1)
!                                                  at level z_j
!                                      ZK        = hydraulic conductivity (m s-1)
!                                      ZHEAD     = matric potential gradient (-)
!                                      ZWSAT     = ice modified soil porosity (m3 m-3)
!                                      ZFRZ      = diffusion coefficient for freezing (-)
!                                      ZVAPCOND  = vapor conductivity (m s-1) 
!                                      ZKI       = interfacial hydraulic conductivity (m s-1) at level z_j
!                                      ZCAPACITY = simple volumetric water holding capacity estimate for
!                                                  wetting front penetration (-) 
!                                      ZINFNEG   = Negative infiltration (m s-1)
!
REAL, DIMENSION(SIZE(PDZG,1),SIZE(PDZG,2)) :: ZAMTRX, ZBMTRX, ZCMTRX, ZFRC, ZSOL, &
                                              ZSGDRAIN
!                                      ZAMTRX    = leftmost diagonal element of tri-diagonal
!                                                  coefficient matrix 
!                                      ZBMTRX    = center diagonal element (vector)
!                                      ZCMTRX    = rightmost diagonal element (vector)
!                                      ZFRC      = forcing function (vector)
!                                      ZSOL      = solution vector
!                                      ZSGDRAIN  = sub-grid drainge (m s-1): calibration parameter
!
REAL, DIMENSION(SIZE(PDZG,1),SIZE(PDZG,2))  ::ZWWILT, ZINFLAYER
!
REAL, PARAMETER                     :: ZWGHT = 0.5  ! time scheme weight for calculating flux.
!                                                     varies from 0 (explicit time scheme)
!                                                     to 1 (backward difference implicit)
!                                                     Default is 1/2 (Crank-Nicholson)
!
REAL, PARAMETER                     :: ZEICE = 6.0  ! Ice vertical diffusion impedence factor 
!
REAL                                :: ZLOG10, ZS, ZLOG, ZWDRAIN
!
REAL                               :: ZWFC, ZWLIM, ZDKDT1, ZDKDT2, ZDHEADDT1, ZDHEADDT2
!                                     ZWFC      = ice modified soil field capacity (m3 m-3)
!                                     ZDKDT1    = hydraulic conductivity derrivative w/r/t upper layer water content
!                                     ZDKDT2    = "" lower layer water content
!                                     ZDHEADDT1 = matric potential gradient derrivative w/r/t upper layer water content
!                                     ZDHEADDT2 = "" lower layer water content
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
! 0. Initialization:
!    ---------------
!
IF (LHOOK) CALL DR_HOOK('HYDRO_SOILDIF',0,ZHOOK_HANDLE)
!
INI = SIZE(PDZG(:,:),1)
INL = KMAX_LAYER
!
ZLOG10 = LOG(10.0)
!
PDRAIN    (:) = 0.0
PHORTON   (:) = 0.0
ZINFILTC  (:) = 0.0
ZEXCESS   (:) = 0.0
ZINFILTMAX(:) = 0.0
!
ZINFNEG  (:,:) = 0.0
ZINFLAYER(:,:) = 0.0
ZFRC     (:,:) = 0.0
ZFRZ     (:,:) = 0.0
!
ZWSAT    (:,:) = XUNDEF
ZCAPACITY(:,:) = XUNDEF
ZPSI     (:,:) = XUNDEF
ZK       (:,:) = XUNDEF
ZVAPCOND (:,:) = XUNDEF
ZKI      (:,:) = XUNDEF
ZSOL     (:,:) = XUNDEF
ZNU      (:,:) = XUNDEF
ZHEAD    (:,:) = XUNDEF
ZWFLUX   (:,:) = XUNDEF
ZWFLUXN  (:,:) = XUNDEF
ZDFLUXDT1(:,:) = XUNDEF
ZDFLUXDT2(:,:) = XUNDEF
ZAMTRX   (:,:) = XUNDEF
ZBMTRX   (:,:) = XUNDEF
ZCMTRX   (:,:) = XUNDEF
!
ZSGDRAIN (:,:) = 0.0    
ZWWILT   (:,:) = XWGMIN
IF(HKSAT=='SGH'.OR.HSOC=='SGH')THEN
  ZWWILT  (:,:) = PWWILT(:,:)
ENDIF        
!
DO JL=1,INL
   DO JJ=1,INI
!
      IDEPTH=KWG_LAYER(JJ)
      IF(JL<=IDEPTH)THEN
!
!       Modification/addition of frozen soil parameters
!       -----------------------------------------------
!
!       Modify soil porosity as ice assumed to become part
!       of solid soil matrix (with respect to liquid flow):
        ZWSAT (JJ,JL) = MAX(XWGMIN, PWSAT(JJ,JL)-PWGI(JJ,JL))        
!
!       Factor from (Johnsson and Lundin 1991), except here it is normalized so that it
!       goes to zero in the limit as all available pore space is filled up with ice.
!       For now, a simple constant is used for all soils. Further modifications
!       will be made as research warrents.
!       Old : 10.**(-ZEICE*(PWGI(JJ,JL)/(PWGI(JJ,JL)+PWG(JJ,JL))))
        ZFRZ(JJ,JL) = EXP(ZLOG10*(-ZEICE*(PWGI(JJ,JL)/(PWGI(JJ,JL)+PWG(JJ,JL)))))
!
!       Simple volumetric water holding capacity estimate for wetting front penetration
        ZCAPACITY(JJ,JL) = MAX(0.0,ZWSAT(JJ,JL)-PWG(JJ,JL))*PDZG(JJ,JL)

!       Linear (in time) sub-grid drainage term (input in m s-1 herein)
!       ---------------------------------------------------------------
!
!       (very simple way to take into account sub-grid water table effects, pores, etc...):
!       Can be calibrated. Also, make this effect vanish/cease for 
!       extremely dry soil layers. This term/option is OFF if WDRAIN = 0.
!
        IF(PWDRAIN(JJ) > 0.0)THEN
          ZWFC            = PWFC  (JJ,JL)*ZWSAT(JJ,JL)/PWSAT(JJ,JL)
          ZWLIM           = MAX(XWGMIN,ZWWILT(JJ,JL)*ZWSAT(JJ,JL)/PWSAT(JJ,JL))
          ZS              = MIN(1.0,(ZWFC+PWDRAIN(JJ))/ZWSAT(JJ,JL))
          ZLOG            = (2.*PBCOEF(JJ,JL)+3.0)*LOG(ZS)
          ZWDRAIN         = ZFRZ(JJ,JL)*PCONDSAT(JJ,JL) * EXP(ZLOG)
          ZSGDRAIN(JJ,JL) = ZWDRAIN * MAX(0.0, MIN(ZWFC,PWG(JJ,JL))-ZWLIM)/(ZWFC-ZWLIM)
        ENDIF
!
     ENDIF
!
   ENDDO
ENDDO
!
! 1. Infiltration at "t"
!    -------------------
!
! Surface flux term (m s-1): Infiltration (and surface runoff)
! Surface fluxes are limited a Green-Ampt approximation from Abramopoulos et al
! (1988) and Entekhabi and Eagleson (1989).
! Note : when Horton option is used, infiltration already calculated in hydro_sgh
IF(HHORT/='SGH')THEN
!  Green-Ampt approximation for maximum infiltration (derived form)
   ZINFILTMAX(:) = INFMAX_FUNC(PWG,ZWSAT,ZFRZ,PCONDSAT,PMPOTSAT,PBCOEF,PDZG,PDG,KLAYER_HORT)
!  Fast(temporal)-response runoff (surface excess) (m s-1):
   PHORTON   (:) = (1.-PFSAT(:)) * MAX(0.0,PPG(:)-ZINFILTMAX(:))
ENDIF
!
!
! 2. Initialise soil moisture profile according to the sink linear terms at "t"
!    ----------------------------------------------------------------------------
!
!Surface cumulative infiltration  (m)
ZINFILTC(:) = MAX(0.0,PPG(:)-PHORTON(:))*PTSTEP
!
DO JL=1,INL
   DO JJ=1,INI
      IDEPTH=KWG_LAYER(JJ)
      IF(JL<=IDEPTH)THEN
!       Infiltration terms (m) :
        ZINFLAYER(JJ,JL) = MIN(ZINFILTC(JJ),ZCAPACITY(JJ,JL))
!       Soil moisture (m3/m3) :
        PWG (JJ,JL) = PWG(JJ,JL)+ZINFLAYER(JJ,JL)/PDZG(JJ,JL)
!       Put remainding infiltration into the next layer (m)
        ZINFILTC(JJ) = ZINFILTC(JJ) - ZINFLAYER(JJ,JL)
!       Possible negative infiltration  (m s-1)
        ZINFNEG(JJ,JL) = (MIN(0.0,PPG(JJ))+PEVAPCOR(JJ))*PDZG(JJ,JL)/PDG(JJ,IDEPTH)
     ENDIF
   ENDDO
ENDDO
!
!Fast(temporal)-response runoff (surface excess) (kg m2 s-1):
!special case : if infiltration > total soil capacity
PHORTON(:)=(PHORTON(:)+ZINFILTC(:)/PTSTEP)*XRHOLW
!
!
! 3. Initialise matric potential and hydraulic conductivity at "t"
!    -------------------------------------------------------------
!
DO JL=1,INL
   DO JJ=1,INI    
     IDEPTH=KWG_LAYER(JJ)
     IF(JL<=IDEPTH)THEN
!       Matric potential (m) :
!       psi=mpotsat*(w/wsat)**(-bcoef)
        ZS          = MIN(1.0,PWG(JJ,JL)/ZWSAT(JJ,JL))
        ZLOG        = PBCOEF(JJ,JL)*LOG(ZS)
        ZPSI(JJ,JL) = PMPOTSAT(JJ,JL)*EXP(-ZLOG)
!       Hydraulic conductivity from matric potential (m s-1):
!       k=frz*condsat*(psi/mpotsat)**(-2-3/bcoef)
        ZLOG      = -ZLOG*(2.0+3.0/PBCOEF(JJ,JL))
        ZK(JJ,JL) = ZFRZ(JJ,JL)*PCONDSAT(JJ,JL)*EXP(-ZLOG)
     ENDIF
   ENDDO
ENDDO    
!
!
! 4. Vapor diffusion conductivity (m s-1)
!    ------------------------------------
!
ZVAPCOND(:,:) = VAPCONDCF(PTG,PPS,PWG,PWGI,ZPSI,PWSAT,PWFC,PQSAT,PQSATI,KWG_LAYER,INL)
ZVAPCOND(:,:) = ZFRZ(:,:)*ZVAPCOND(:,:)
!
! 5. Linearized water flux: values at "t"
!    ------------------------------------
!    calculate flux at the beginning of the time step:
!
DO JL=1,INL
   DO JJ=1,INI
!
      IDEPTH=KWG_LAYER(JJ)
      IF(JL<IDEPTH)THEN
!
!       Total interfacial conductivity (m s-1) And Potential gradient (dimensionless):
        ZKI  (JJ,JL) = SQRT(ZK(JJ,JL)*ZK(JJ,JL+1))
        ZNU  (JJ,JL) = ZKI(JJ,JL) + SQRT(ZVAPCOND(JJ,JL)*ZVAPCOND(JJ,JL+1))
        ZHEAD(JJ,JL) = (ZPSI(JJ,JL)-ZPSI(JJ,JL+1))/PDZDIF(JJ,JL)
!
!       Total Sub-surface soil water fluxes (m s-1): (+ up, - down) using Darcy's
!       Law with an added linear drainage term:
        ZWFLUX(JJ,JL) = -ZNU(JJ,JL) *ZHEAD(JJ,JL) - ZKI(JJ,JL) - ZSGDRAIN(JJ,JL)
!
      ELSEIF(JL==IDEPTH)THEN !Last layers
!      
!       Total interfacial conductivity (m s-1) And Potential gradient (dimensionless):
        ZKI  (JJ,IDEPTH) = ZK(JJ,IDEPTH)
        ZNU  (JJ,IDEPTH) = 0.0
        ZHEAD(JJ,IDEPTH) = 0.0
!
!       Total Sub-surface soil water fluxes (m s-1): (+ up, - down) using Darcy's
!       Law with an added linear drainage term:
        ZWFLUX(JJ,IDEPTH) = -ZNU(JJ,IDEPTH) *ZHEAD(JJ,IDEPTH) - ZKI(JJ,IDEPTH) - ZSGDRAIN(JJ,IDEPTH)
!
      ENDIF
!
   ENDDO
ENDDO
!
!
! 7. Linearized water flux: values at "t+dt"
!    ---------------------------------------
! Flux Derrivative terms, see A. Boone thesis (Annexe E).
!
DO JL=1,INL
   DO JJ=1,INI
      IDEPTH=KWG_LAYER(JJ)        
      IF(JL<IDEPTH)THEN                
         ZDHEADDT1 = -PBCOEF(JJ,JL  )*ZPSI(JJ,JL  )/(PWG(JJ,JL  )*PDZDIF(JJ,JL))
         ZDHEADDT2 = -PBCOEF(JJ,JL+1)*ZPSI(JJ,JL+1)/(PWG(JJ,JL+1)*PDZDIF(JJ,JL))
         ZDKDT1    = (2.*PBCOEF(JJ,JL  )+3.)*ZKI(JJ,JL)/(2.0*PWG(JJ,JL  ))
         ZDKDT2    = (2.*PBCOEF(JJ,JL+1)+3.)*ZKI(JJ,JL)/(2.0*PWG(JJ,JL+1))
!        Total Flux derrivative terms:
         ZDFLUXDT1(JJ,JL) = -ZDKDT1*ZHEAD(JJ,JL) - ZNU(JJ,JL)*ZDHEADDT1 - ZDKDT1
         ZDFLUXDT2(JJ,JL) = -ZDKDT2*ZHEAD(JJ,JL) + ZNU(JJ,JL)*ZDHEADDT2 - ZDKDT2  
      ELSEIF(JL==IDEPTH)THEN !Last layers
         ZDHEADDT1 = 0.0   
         ZDHEADDT2 = 0.0
         ZDKDT1    = (2.*PBCOEF(JJ,IDEPTH)+3.)*ZK(JJ,IDEPTH)/PWG(JJ,IDEPTH)
         ZDKDT2    = 0.0                
!        Total Flux derrivative terms:
         ZDFLUXDT1(JJ,IDEPTH) = -ZDKDT1*ZHEAD(JJ,IDEPTH) - ZNU(JJ,IDEPTH)*ZDHEADDT1 - ZDKDT1
         ZDFLUXDT2(JJ,IDEPTH) = -ZDKDT2*ZHEAD(JJ,IDEPTH) + ZNU(JJ,IDEPTH)*ZDHEADDT2 - ZDKDT2     
      ENDIF
   ENDDO
ENDDO
!
! 8. Jacobian Matrix coefficients and Forcing function
!    -------------------------------------------------
!     
!surface layer:
ZFRC  (:,1) = ZWFLUX(:,1) - PLEG(:) - PF2WGHT(:,1)*PLETR(:) + ZINFNEG(:,1)
ZAMTRX(:,1) = 0.0
ZBMTRX(:,1) = (PDZG(:,1)/PTSTEP) - ZWGHT*ZDFLUXDT1(:,1)
ZCMTRX(:,1) = -ZWGHT*ZDFLUXDT2(:,1)
!
!Other sub-surface layers:       
DO JL=2,INL
   DO JJ=1,INI   
      IDEPTH=KWG_LAYER(JJ)
      IF(JL<=IDEPTH)THEN
        ZFRC  (JJ,JL) = ZWFLUX (JJ,JL) - ZWFLUX(JJ,JL-1) - PF2WGHT(JJ,JL)*PLETR(JJ) + ZINFNEG(JJ,JL)
        ZAMTRX(JJ,JL) = ZWGHT*ZDFLUXDT1(JJ,JL-1)
        ZBMTRX(JJ,JL) = (PDZG(JJ,JL)/PTSTEP) - ZWGHT*(ZDFLUXDT1(JJ,JL)-ZDFLUXDT2(JJ,JL-1))       
        ZCMTRX(JJ,JL) = -ZWGHT*ZDFLUXDT2(JJ,JL)
      ENDIF
   ENDDO
ENDDO
!
! Solve Matrix Equation: tridiagonal system: solve for soil
! water (volumetric water content) tendencies:
!
 CALL TRIDIAG_DIF(ZAMTRX,ZBMTRX,ZCMTRX,ZFRC,KWG_LAYER,INL,ZSOL)
!
! 9. Final calculations and diagnostics:
!    -----------------------------------
!
!
DO JL=1,INL
   DO JJ=1,INI
!   
      IDEPTH=KWG_LAYER(JJ)
      IF(JL<IDEPTH)THEN
! 
!       Update liquid water content (m3 m-3):
        PWG(JJ,JL)   = PWG(JJ,JL) + ZSOL(JJ,JL)    
!
!       Supersaturated drainage (kg m-2 s-1):
        ZEXCESS(JJ)  = MAX(0.0, PWG(JJ,JL) - ZWSAT(JJ,JL))
        PWG(JJ,JL  ) = MIN(PWG(JJ,JL),ZWSAT(JJ,JL))
        PWG(JJ,JL+1) = PWG(JJ,JL+1) + ZEXCESS(JJ)*(PDZG(JJ,JL)/PDZG(JJ,JL+1))
!
!       final fluxes (at end of time step) (m s-1):
        ZWFLUXN(JJ,JL) = ZWFLUX(JJ,JL) + ZDFLUXDT1(JJ,JL)*ZSOL(JJ,JL) + ZDFLUXDT2(JJ,JL)*ZSOL(JJ,JL+1)
!
      ELSEIF(JL==IDEPTH)THEN
! 
!       Update liquid water content (m3 m-3):
        PWG(JJ,IDEPTH)   = PWG(JJ,IDEPTH) + ZSOL(JJ,IDEPTH)    
!        
!       Supersaturated drainage (kg m-2 s-1):
        ZEXCESS(JJ)    = MAX(0.0, PWG(JJ,IDEPTH) - ZWSAT(JJ,IDEPTH))
        PWG(JJ,IDEPTH) = MIN(PWG(JJ,IDEPTH),ZWSAT(JJ,IDEPTH))   
        PDRAIN (JJ)    = PDRAIN(JJ) + ZEXCESS(JJ)*PDZG(JJ,IDEPTH)*XRHOLW/PTSTEP
!   
!       final fluxes (at end of time step) (m s-1):
        ZWFLUXN(JJ,IDEPTH) = ZWFLUX(JJ,IDEPTH) + ZDFLUXDT1(JJ,IDEPTH)*ZSOL(JJ,IDEPTH)
!   
!       Drainage or baseflow out of bottom of model (slow time response) (kg m-2 s-1):
!       Final fluxes (if needed) over the time step (kg m-2 s-1)
!       would be calculated as (for water budget checks) as F = [ wgt*F(t+dt) + (1.-wgt)*F(t) ]*XRHOLW
        PDRAIN (JJ) = PDRAIN(JJ)-(ZWGHT*ZWFLUXN(JJ,IDEPTH)+(1.-ZWGHT)*ZWFLUX(JJ,IDEPTH))*XRHOLW
!
      ENDIF
!      
   ENDDO
ENDDO
!
! Possible correction/Constraint application: 
!
DO JL=1,INL
   DO JJ=1,INI   
      IDEPTH=KWG_LAYER(JJ)
      IF(JL<IDEPTH)THEN
!       if the soil water happens to fall below the minimum, then
!       extract needed water from the layer below: this should
!       generally be non-existant: but added to ensure conservation
!       even for the most extreme events.              
        ZEXCESS(JJ)  = MAX(0., XWGMIN  - PWG(JJ,JL))
        PWG(JJ,JL)   = PWG(JJ,JL)   + ZEXCESS(JJ) 
        PWG(JJ,JL+1) = PWG(JJ,JL+1) - ZEXCESS(JJ)*PDZG(JJ,JL)/PDZG(JJ,JL+1)
      ELSEIF(JL==IDEPTH.AND.PWG(JJ,IDEPTH)<XWGMIN)THEN
!       NOTE, negative moisture can arise for *completely* dry/dessicated soils 
!       owing to the above check because vertical fluxes
!       can be *very* small but nonzero. Here correct owing to
!       small numerical drainage.
        PDRAIN(JJ)     = PDRAIN(JJ) + MIN(0.0,PWG(JJ,IDEPTH)-XWGMIN)*PDZG(JJ,IDEPTH)*XRHOLW/PTSTEP
        PWG(JJ,IDEPTH) = XWGMIN
      ENDIF
   ENDDO
ENDDO       
!
IF (LHOOK) CALL DR_HOOK('HYDRO_SOILDIF',1,ZHOOK_HANDLE)
!
END SUBROUTINE HYDRO_SOILDIF 






