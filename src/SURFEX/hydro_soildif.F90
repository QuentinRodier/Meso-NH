!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE HYDRO_SOILDIF(IO, KK, PK, PEK, PTSTEP, PPG, PETR, PEG, PEVAPCOR,   &
                               PPS, PQSAT, PQSATI, PDRAIN, PHORTON, KMAX_LAYER, PQSB)
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
!!                  04/13    B.Decharme: Subsurface runoff if SGH (DIF option only)
!!                  07/2013  B.Decharme: Surface / Water table depth coupling
!!                  05/2016  B.Decharme: Bug : no Horton runoff because is already computed in hydro_sgh if required
!!                  10/2018  B.Decharme: Surface / Water table depth coupling into the soil
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
USE MODD_ISBA_n,         ONLY : ISBA_K_t, ISBA_P_t, ISBA_PE_t
!
USE MODD_SURF_PAR, ONLY : XUNDEF, NUNDEF
USE MODD_CSTS,     ONLY : XRHOLW
USE MODD_ISBA_PAR, ONLY : XWGMIN
!
USE MODE_HYDRO_DIF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
TYPE(ISBA_OPTIONS_t), INTENT(INOUT) :: IO
TYPE(ISBA_K_t),       INTENT(INOUT) :: KK
TYPE(ISBA_P_t),       INTENT(INOUT) :: PK
TYPE(ISBA_PE_t),      INTENT(INOUT) :: PEK
!
REAL, INTENT(IN)                    :: PTSTEP ! Model time step (s)
!
REAL, DIMENSION(:), INTENT(IN)      :: PPS, PPG, PEG, PEVAPCOR
!                                      PPS    = surface pressure (Pa)
!                                      PPG    = throughfall rate: 
!                                               rate at which water reaches the surface
!                                               of the soil (from snowmelt, rain, canopy
!                                               drip, etc...) (m/s)
!                                      PEG   = bare-soil evaporation rate (m/s)
!                                      PEVAPCOR = correction for any excess evaporation 
!                                                from snow as it completely ablates (m/s)
!
REAL, DIMENSION(:,:), INTENT(IN)    :: PQSAT,PQSATI
!                                      specific humidity at saturation
!
REAL, DIMENSION(:,:), INTENT(IN)    :: PETR
!                                      PETR      = transpiration rate (in each soil layer)
!
INTEGER,               INTENT(IN)   :: KMAX_LAYER  
!                                      KMAX_LAYER = Max number of soil moisture layers (DIF option)
!
REAL, DIMENSION(:), INTENT(OUT)     :: PDRAIN, PHORTON
!                                      PDRAIN   = drainage (flux out of model base) (kg m-2 s-1)
!
REAL, DIMENSION(:),   INTENT(OUT)   :: PQSB     !Lateral subsurface flow [kg/m²/s]
!
!
!*      0.2    declarations of local parameters
!
!
REAL, PARAMETER                     :: ZWGHT = 0.5  ! time scheme weight for calculating flux.
!                                                     varies from 0 (explicit time scheme)
!                                                     to 1 (backward difference implicit)
!                                                     Default is 1/2 (Crank-Nicholson)
!
REAL, PARAMETER                     :: ZEICE = 6.0  ! Ice vertical diffusion impedence factor 
!
!
!*      0.3    declarations of local variables
!
!
REAL, DIMENSION(SIZE(PK%XDZG,1))                 :: ZINFILTC, ZEXCESS, ZDGN, ZWGTOT, ZWTD
!                                                   ZEXCESS    = working variable: excess soil water
!                                                                which is used as a constraint 
!                                                   ZDGN      = Depth of the last node (m)
!                                                   ZWGTOT    = total soil moisture for ZINFNEG computation
!                                                   ZWTD      = water table depth positive below soil surface (m)
!
REAL, DIMENSION(SIZE(PK%XDZG,1),SIZE(PK%XDZG,2)) :: ZWFLUX, ZDFLUXDT1, ZDFLUXDT2, ZWFLUXN
!                                                   ZWFLUX    = vertical soil water flux (+ up) (m s-1)
!                                                   ZDFLUXDT  = total vertical flux derrivative
!                                                   ZDFLUXDT1 = vertical flux derrivative: dF_j/dw_j
!                                                   ZDFLUXDT2 = vertical flux derrivative: dF_j/dw_j+1
!                                                   ZWFLUXN   = vertical soil water flux at end of time 

REAL, DIMENSION(SIZE(PK%XDZG,1),SIZE(PK%XDZG,2)) :: ZPSI, ZK, ZNU, ZWSAT, ZHEAD, ZVAPCOND, &
                                                    ZFRZ, ZKI, ZCAPACITY, ZINFNEG, ZPSI_EQ,&
                                                    ZFEQ, ZLIMITK
!                                                   ZPSI      = matric potential (m)
!                                                   ZK        = hydraulic conductivity (m s-1)
!                                                   ZNU       = interfacial total conductivity (m s-1) at level z_j
!                                                   ZWSAT     = ice modified soil porosity (m3 m-3)
!                                                   ZHEAD     = matric potential gradient (-)
!                                                   ZVAPCOND  = vapor conductivity (m s-1) 
!                                                   ZFRZ      = diffusion coefficient for freezing (-)
!                                                   ZKI       = interfacial hydraulic conductivity (m s-1) at level z_j
!                                                   ZCAPACITY = simple volumetric water holding capacity estimate for
!                                                               wetting front penetration (-) 
!                                                   ZINFNEG   = Negative infiltration (m s-1)
!                                                   ZPSI_EQ   = matric potential in equilibrium with groudwater WTD (m)
!                                                   ZFEQ      = work array
!                                                   ZLIMITK   = limit drainage in presence of WTD to match analytical 
!                                                               solution of saturated soil
!
REAL, DIMENSION(SIZE(PK%XDZG,1),SIZE(PK%XDZG,2)) :: ZAMTRX, ZBMTRX, ZCMTRX, ZFRC, ZSOL, &
                                                    ZTOPQS
!                                                   ZAMTRX    = leftmost diagonal element of tri-diagonal
!                                                               coefficient matrix 
!                                                   ZBMTRX    = center diagonal element (vector)
!                                                   ZCMTRX    = rightmost diagonal element (vector)
!                                                   ZFRC      = forcing function (vector)
!                                                   ZSOL      = solution vector
!
REAL, DIMENSION(SIZE(PK%XDZG,1),SIZE(PK%XDZG,2))  :: ZINFLAYER
!
!
REAL                                :: ZLOG10, ZS, ZLOG, ZWDRAIN
!
REAL                                :: ZDKDT1, ZDKDT2, ZDHEADDT1, ZDHEADDT2
!                                      ZDKDT1    = hydraulic conductivity derrivative w/r/t upper layer water content
!                                      ZDKDT2    = "" lower layer water content
!                                      ZDHEADDT1 = matric potential gradient derrivative w/r/t upper layer water content
!                                      ZDHEADDT2 = "" lower layer water content
!
!
INTEGER                             :: JI, JL    ! loop control
!
INTEGER                             :: INI, INL, IDEPTH ! Number of point and grid layers
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
! 0. Initialization:
!    ---------------
!
IF (LHOOK) CALL DR_HOOK('HYDRO_SOILDIF',0,ZHOOK_HANDLE)
!
INI = SIZE(PK%XDZG,1)
INL = KMAX_LAYER
!
ZLOG10 = LOG(10.0)
!
PDRAIN    (:) = 0.0
PQSB      (:) = 0.0
PHORTON   (:) = 0.0
ZINFILTC  (:) = 0.0
ZEXCESS   (:) = 0.0
!
ZINFNEG  (:,:) = 0.0
ZINFLAYER(:,:) = 0.0
ZFRC     (:,:) = 0.0
ZFRZ     (:,:) = 0.0
ZFEQ     (:,:) = 0.0
ZLIMITK  (:,:) = 0.0
!
ZWSAT    (:,:) = XUNDEF
ZCAPACITY(:,:) = XUNDEF
ZPSI     (:,:) = XUNDEF
ZPSI_EQ  (:,:) = XUNDEF
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
! Modification/addition of frozen soil parameters
! -----------------------------------------------
!
DO JL=1,INL
  DO JI=1,INI
!
    IDEPTH=PK%NWG_LAYER(JI)
    IF(JL<=IDEPTH)THEN
!
!     Modify soil porosity as ice assumed to become part
!     of solid soil matrix (with respect to liquid flow):
      ZWSAT (JI,JL) = MAX(XWGMIN, KK%XWSAT(JI,JL)-PEK%XWGI(JI,JL))   
!
!     Factor from (Johnsson and Lundin 1991), except here it is normalized so that it
!     goes to zero in the limit as all available pore space is filled up with ice.
!     For now, a simple constant is used for all soils. Further modifications
!     will be made as research warrents.
!     Old : 10.**(-ZEICE*(PEK%XWGI(JI,JL)/(PEK%XWGI(JI,JL)+PEK%XWG(JI,JL))))
      ZFRZ(JI,JL) = EXP(ZLOG10*(-ZEICE*(PEK%XWGI(JI,JL)/(PEK%XWGI(JI,JL)+PEK%XWG(JI,JL)))))
!
    ENDIF
!
  ENDDO
ENDDO
!
! Lateral sub-surface flow (m s-1) if Topmodel
! --------------------------------------------
!
IF(IO%CRUNOFF=='SGH')THEN
  ZTOPQS(:,:)=ZFRZ(:,:)*PK%XTOPQS(:,:)
ELSE
  ZTOPQS(:,:)=0.0
ENDIF
!
! 1. Infiltration at "t"
!    -------------------
!
! Surface flux term (m s-1): Infiltration (and surface runoff)
! Surface fluxes are limited a Green-Ampt approximation from Abramopoulos et al
! (1988) and Entekhabi and Eagleson (1989).
! Note : when Horton option is used, infiltration already calculated in hydro_sgh
!
!Surface cumulative infiltration  (m)
ZINFILTC(:) = MAX(0.0,PPG(:))*PTSTEP

!
! 2. Initialise soil moisture profile according to infiltration terms at "t"
!    ----------------------------------------------------------------------
!
DO JL=1,INL
  DO JI=1,INI
    IDEPTH=PK%NWG_LAYER(JI)
    IF(JL<=IDEPTH)THEN
!     Simple volumetric water holding capacity estimate for wetting front penetration
      ZCAPACITY(JI,JL) = MAX(0.0,ZWSAT(JI,JL)-PEK%XWG(JI,JL))*PK%XDZG(JI,JL)
!     Infiltration terms (m) :
      ZINFLAYER(JI,JL) = MIN(ZINFILTC(JI),ZCAPACITY(JI,JL))
!     Soil moisture (m3/m3) :
      PEK%XWG(JI,JL) = PEK%XWG(JI,JL)+ZINFLAYER(JI,JL)/PK%XDZG(JI,JL)
!     Put remainding infiltration into the next layer (m)
      ZINFILTC(JI) = ZINFILTC(JI) - ZINFLAYER(JI,JL)
    ENDIF
  ENDDO
ENDDO
!
! 3. Compute Fast(temporal)-response runoff and Possible negative infiltration
!    -------------------------------------------------------------------------
!
!Possible negative infiltration  (m s-1)
ZWGTOT(:)=0.0 
DO JL=1,INL
  DO JI=1,INI
    IDEPTH=PK%NWG_LAYER(JI)
    IF(JL<IDEPTH)THEN
      ZINFNEG(JI,JL) = (MIN(0.0,PPG(JI))-PEVAPCOR(JI))*PK%XDZG(JI,JL)*PEK%XWG(JI,JL)
      ZWGTOT (JI   ) = ZWGTOT(JI)+PK%XDZG(JI,JL)*PEK%XWG(JI,JL)
    ENDIF
  ENDDO
ENDDO
DO JL=1,INL
  DO JI=1,INI
    ZINFNEG(JI,JL) = ZINFNEG(JI,JL)/ZWGTOT(JI)
  ENDDO
ENDDO 
!
!Fast(temporal)-response runoff (surface excess) (kg m2 s-1):
!special case : if infiltration > total soil capacity
PHORTON(:)=ZINFILTC(:)*(XRHOLW/PTSTEP)
!
!
! 4. Initialise matric potential and hydraulic conductivity at "t"
!    -------------------------------------------------------------
!
DO JL=1,INL
  DO JI=1,INI    
    IDEPTH=PK%NWG_LAYER(JI)
    IF(JL<=IDEPTH)THEN
!     Matric potential (m) :
!     psi=mpotsat*(w/wsat)**(-bcoef)
      ZS          = MIN(1.0,PEK%XWG(JI,JL)/ZWSAT(JI,JL))
      ZLOG        = KK%XBCOEF(JI,JL)*LOG(ZS)
      ZPSI(JI,JL) = KK%XMPOTSAT(JI,JL)*EXP(-ZLOG)
!     Hydraulic conductivity from matric potential (m s-1):
!     k=frz*condsat*(psi/mpotsat)**(-2-3/bcoef)
      ZLOG      = -ZLOG*(2.0+3.0/KK%XBCOEF(JI,JL))
      ZK(JI,JL) = ZFRZ(JI,JL)*PK%XCONDSAT(JI,JL)*EXP(-ZLOG)
    ENDIF
  ENDDO
ENDDO    
!
! Prepare water table depth coupling
! ----------------------------------
!
WHERE(KK%XWTD(:)/=XUNDEF)
  ZWTD(:) = -KK%XWTD(:)
ELSEWHERE
  ZWTD(:) = XUNDEF
ENDWHERE
!
DO JL=1,INL
   DO JI=1,INI    
      IDEPTH=PK%NWG_LAYER(JI)
      IF(JL<IDEPTH)THEN
!       Equilibrium potential with wtd (if no wtd, ZPSI_EQ = ZPSI)
        ZFEQ   (JI,JL) = MIN(1.0,MAX(0.0,PK%XDG(JI,JL+1)-ZWTD(JI))/(PK%XDG(JI,JL+1)-PK%XDG(JI,JL)))
        ZPSI_EQ(JI,JL) = ZPSI(JI,JL+1) + ZFEQ(JI,JL) * (KK%XMPOTSAT(JI,JL+1)-ZPSI(JI,JL+1))
      ENDIF
   ENDDO
ENDDO
!
ZDGN(:) = XUNDEF
ZWTD(:) = XUNDEF
!
DO JI=1,INI
   IDEPTH=PK%NWG_LAYER(JI)   
!  Depth of the last node
   ZDGN   (JI) = 0.5*(PK%XDG(JI,IDEPTH)+PK%XDG(JI,IDEPTH-1))
   IF(KK%XWTD(JI)/=XUNDEF)THEN  
!    Water table depth
     ZWTD(JI) = MAX(PK%XDG(JI,IDEPTH),-KK%XWTD(JI))
   ENDIF
ENDDO
!
! Limit drainage in presence of WTD to match analytical solution of saturated soil
!
ZFEQ   (:,1) = MIN(1.0,MAX(0.0,PK%XDG(:,1)+KK%XWTD(:))/PK%XDG(:,1))
ZLIMITK(:,1) = (1.0-KK%XFWTD(:)*ZFEQ(:,1))
DO JL=2,INL
   DO JI=1,INI    
      IDEPTH=PK%NWG_LAYER(JI)
      IF(JL<=IDEPTH)THEN
        ZFEQ   (JI,JL) = MIN(1.0,MAX(0.0,PK%XDG(JI,JL)+KK%XWTD(JI))/(PK%XDG(JI,JL)-PK%XDG(JI,JL-1)))
        ZLIMITK(JI,JL) = (1.0-KK%XFWTD(JI)*ZFEQ(JI,JL))
      ENDIF
   ENDDO
ENDDO  
!
! 5. Vapor diffusion conductivity (m s-1)
!    ------------------------------------
!
ZVAPCOND(:,:) = VAPCONDCF(PEK%XTG, PPS, PEK%XWG, PEK%XWGI, ZPSI,&
                          KK%XWSAT, KK%XWFC, PQSAT, PQSATI, PK%NWG_LAYER, INL)
ZVAPCOND(:,:) = ZFRZ(:,:)*ZVAPCOND(:,:)
!
! 6. Linearized water flux: values at "t"
!    ------------------------------------
!    calculate flux at the beginning of the time step:
!
DO JL=1,INL
  DO JI=1,INI
!
    IDEPTH=PK%NWG_LAYER(JI)
!
    IF(JL<IDEPTH)THEN
!
!     Total interfacial conductivity (m s-1) And Potential gradient (dimensionless):
      ZKI  (JI,JL) = SQRT(ZK(JI,JL)*ZK(JI,JL+1))
      ZNU  (JI,JL) = ZKI(JI,JL) + SQRT(ZVAPCOND(JI,JL)*ZVAPCOND(JI,JL+1))
      ZHEAD(JI,JL) = (ZPSI(JI,JL)-(1.0-KK%XFWTD(JI))*ZPSI(JI,JL+1)-KK%XFWTD(JI)*ZPSI_EQ(JI,JL)) &
                   / PK%XDZDIF(JI,JL)
!
!     Total Sub-surface soil water fluxes (m s-1): (+ up, - down) using Darcy's
!     Law with an added linear drainage term:
      ZWFLUX(JI,JL) = -ZNU(JI,JL) * ZHEAD(JI,JL) - ZKI(JI,JL)*ZLIMITK(JI,JL)
!
    ELSEIF(JL==IDEPTH)THEN !Last layers   
!        
!     Total interfacial conductivity (m s-1) And Potential gradient (dimensionless):
      ZKI  (JI,IDEPTH) = ZK(JI,IDEPTH)
      ZNU  (JI,IDEPTH) = ZK(JI,IDEPTH) * KK%XFWTD(JI)
      ZHEAD(JI,IDEPTH) = (ZPSI(JI,IDEPTH)-KK%XMPOTSAT(JI,IDEPTH))/(ZWTD(JI)-ZDGN(JI))
!
!     Total Sub-surface soil water fluxes (m s-1): (+ up, - down) using Darcy's
!     Law with an added linear drainage term:
      ZWFLUX(JI,IDEPTH) = -ZNU(JI,IDEPTH) * ZHEAD(JI,IDEPTH) - ZKI(JI,IDEPTH)*ZLIMITK(JI,IDEPTH)
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
!
  DO JI=1,INI
!
    IDEPTH=PK%NWG_LAYER(JI)        
!
    IF(JL<IDEPTH)THEN                
!
      ZDHEADDT1 = -KK%XBCOEF(JI,JL  )*ZPSI(JI,JL  )/(PEK%XWG(JI,JL  )*PK%XDZDIF(JI,JL))    
      ZDHEADDT2 = -KK%XBCOEF(JI,JL+1)*(ZPSI(JI,JL+1)*(1.0-KK%XFWTD(JI))+KK%XFWTD(JI)*ZPSI_EQ(JI,JL)) &
                / (PEK%XWG(JI,JL+1)*PK%XDZDIF(JI,JL))
!     
      ZDKDT1    = (2.*KK%XBCOEF(JI,JL  )+3.)*ZKI(JI,JL)/(2.0*PEK%XWG(JI,JL  ))
      ZDKDT2    = (2.*KK%XBCOEF(JI,JL+1)+3.)*ZKI(JI,JL)/(2.0*PEK%XWG(JI,JL+1))
!
!     Total Flux derrivative terms:
      ZDFLUXDT1(JI,JL) = -ZDKDT1*ZHEAD(JI,JL) - ZNU(JI,JL)*ZDHEADDT1 - ZDKDT1*ZLIMITK(JI,JL)
      ZDFLUXDT2(JI,JL) = -ZDKDT2*ZHEAD(JI,JL) + ZNU(JI,JL)*ZDHEADDT2 - ZDKDT2*ZLIMITK(JI,JL) 
!
    ELSEIF(JL==IDEPTH)THEN !Last layers
!
      ZDHEADDT1 = -KK%XBCOEF(JI,IDEPTH)*ZPSI       (JI,IDEPTH)/(PEK%XWG  (JI,IDEPTH)*(ZWTD(JI)-ZDGN(JI))) &
                  +KK%XBCOEF(JI,IDEPTH)*KK%XMPOTSAT(JI,IDEPTH)/(ZWSAT(JI,IDEPTH)    *(ZWTD(JI)-ZDGN(JI)))
      ZDHEADDT2 = 0.0
!
      ZDKDT1    = (2.*KK%XBCOEF(JI,IDEPTH)+3.)*ZK(JI,IDEPTH)/PEK%XWG(JI,IDEPTH)
      ZDKDT2    = 0.0                
!
!     Total Flux derrivative terms:
      ZDFLUXDT1(JI,IDEPTH) = -ZDKDT1*ZHEAD(JI,IDEPTH)*KK%XFWTD(JI) - ZNU(JI,IDEPTH)*ZDHEADDT1 - ZDKDT1*ZLIMITK(JI,IDEPTH)
      ZDFLUXDT2(JI,IDEPTH) = 0.0  
!
    ENDIF
  ENDDO
!
ENDDO
!
! 8. Jacobian Matrix coefficients and Forcing function
!    -------------------------------------------------
!     
!surface layer:
ZFRC  (:,1) = ZWFLUX(:,1) - PEG(:) - PETR(:,1) + ZINFNEG(:,1) - ZTOPQS(:,1)
ZAMTRX(:,1) = 0.0
ZBMTRX(:,1) = (PK%XDZG(:,1)/PTSTEP) - ZWGHT*ZDFLUXDT1(:,1)
ZCMTRX(:,1) = -ZWGHT*ZDFLUXDT2(:,1)
!
!Other sub-surface layers:       
DO JL=2,INL
  DO JI=1,INI   
    IDEPTH=PK%NWG_LAYER(JI)
    IF(JL<=IDEPTH)THEN
      ZFRC  (JI,JL) = ZWFLUX (JI,JL) - ZWFLUX(JI,JL-1) - PETR(JI,JL) + ZINFNEG(JI,JL) - ZTOPQS(JI,JL)
      ZAMTRX(JI,JL) = ZWGHT*ZDFLUXDT1(JI,JL-1)
      ZBMTRX(JI,JL) = (PK%XDZG(JI,JL)/PTSTEP) - ZWGHT*(ZDFLUXDT1(JI,JL)-ZDFLUXDT2(JI,JL-1))       
      ZCMTRX(JI,JL) = -ZWGHT*ZDFLUXDT2(JI,JL)
    ENDIF
  ENDDO
ENDDO
!
!Solve Matrix Equation: tridiagonal system: solve for soil
!water (volumetric water content) tendencies:
!
CALL TRIDIAG_DIF(ZAMTRX,ZBMTRX,ZCMTRX,ZFRC,PK%NWG_LAYER(:),INL,ZSOL)
!
! 9. Final calculations and diagnostics:
!    -----------------------------------
!
!
DO JL=1,INL
!
  DO JI=1,INI
!
    IDEPTH=PK%NWG_LAYER(JI)
! 
    IF(JL<IDEPTH)THEN
! 
!     Update liquid water content (m3 m-3):
      PEK%XWG(JI,JL)   = PEK%XWG(JI,JL) + ZSOL(JI,JL)    
!
!     Supersaturated drainage (kg m-2 s-1):
      ZEXCESS(JI)  = MAX(0.0, PEK%XWG(JI,JL) - ZWSAT(JI,JL))
      PEK%XWG(JI,JL  ) = MIN(PEK%XWG(JI,JL),ZWSAT(JI,JL))
      PEK%XWG(JI,JL+1) = PEK%XWG(JI,JL+1) + ZEXCESS(JI)*(PK%XDZG(JI,JL)/PK%XDZG(JI,JL+1))
!
!     final fluxes (at end of time step) (m s-1):
      ZWFLUXN(JI,JL) = ZWFLUX(JI,JL) + ZDFLUXDT1(JI,JL)*ZSOL(JI,JL) + ZDFLUXDT2(JI,JL)*ZSOL(JI,JL+1)
!
!     Total topmodel subsurface flow
      PQSB (JI) = PQSB(JI) + ZTOPQS(JI,JL)*XRHOLW
!
    ELSEIF(JL==IDEPTH)THEN
! 
!     Update liquid water content (m3 m-3):
      PEK%XWG(JI,IDEPTH)   = PEK%XWG(JI,IDEPTH) + ZSOL(JI,IDEPTH)    
!        
!     Supersaturated drainage (kg m-2 s-1):
      ZEXCESS(JI)        = MAX(0.0, PEK%XWG(JI,IDEPTH) - ZWSAT(JI,IDEPTH))
      PEK%XWG(JI,IDEPTH) = MIN(PEK%XWG(JI,IDEPTH),ZWSAT(JI,IDEPTH))   
      PDRAIN (JI)        = PDRAIN(JI) + ZEXCESS(JI)*PK%XDZG(JI,IDEPTH)*XRHOLW/PTSTEP
!   
!     final fluxes (at end of time step) (m s-1):
      ZWFLUXN(JI,IDEPTH) = ZWFLUX(JI,IDEPTH) + ZDFLUXDT1(JI,IDEPTH)*ZSOL(JI,IDEPTH)
!   
!     Drainage or baseflow out of bottom of model (slow time response) (kg m-2 s-1):
!     Final fluxes (if needed) over the time step (kg m-2 s-1)
!     would be calculated as (for water budget checks) as F = [ wgt*F(t+dt) + (1.-wgt)*F(t) ]*XRHOLW
      PDRAIN (JI) = PDRAIN(JI)-(ZWGHT*ZWFLUXN(JI,IDEPTH)+(1.-ZWGHT)*ZWFLUX(JI,IDEPTH))*XRHOLW
!
!     Total topmodel subsurface flow
      PQSB (JI) = PQSB(JI) + ZTOPQS(JI,IDEPTH)*XRHOLW
!
    ENDIF
!
  ENDDO
!
ENDDO
!
! Possible correction/Constraint application: 
!
DO JL=1,INL
  DO JI=1,INI   
     IDEPTH=PK%NWG_LAYER(JI)
     IF(JL<IDEPTH)THEN
!      if the soil water happens to fall below the minimum, then
!      extract needed water from the layer below: this should
!      generally be non-existant: but added to ensure conservation
!      even for the most extreme events.              
       ZEXCESS(JI)  = MAX(0., XWGMIN  - PEK%XWG(JI,JL))
       PEK%XWG(JI,JL)   = PEK%XWG(JI,JL)   + ZEXCESS(JI) 
       PEK%XWG(JI,JL+1) = PEK%XWG(JI,JL+1) - ZEXCESS(JI)*PK%XDZG(JI,JL)/PK%XDZG(JI,JL+1)
     ELSEIF(JL==IDEPTH.AND.PEK%XWG(JI,IDEPTH)<XWGMIN)THEN
!      NOTE, negative moisture can arise for *completely* dry/dessicated soils 
!      owing to the above check because vertical fluxes
!      can be *very* small but nonzero. Here correct owing to
!      small numerical drainage.
       PDRAIN(JI)     = PDRAIN(JI) + MIN(0.0,PEK%XWG(JI,IDEPTH)-XWGMIN)*PK%XDZG(JI,IDEPTH)*XRHOLW/PTSTEP
       PEK%XWG(JI,IDEPTH) = XWGMIN
     ENDIF
  ENDDO
ENDDO
!
IF (LHOOK) CALL DR_HOOK('HYDRO_SOILDIF',1,ZHOOK_HANDLE)
!
END SUBROUTINE HYDRO_SOILDIF 






