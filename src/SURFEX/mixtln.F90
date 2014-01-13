!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
    SUBROUTINE MIXTL_n(PFSOL,PFNSOL,PSFTEAU,PSFU,PSFV,PSEATEMP)
!     #######################################################################
!
!!****  *MIXTLN (1D MODEL)*  
!!
!!    PURPOSE
!!    -------
!     Oceanic 1D model in TKE Closure scheme
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
!!    REFERENCE
!!    ---------
!     Gaspar et al, 1990 : A simple eddy kinetic energy model for simulations of 
!     the oceanic vertical mixing : Tests at station Papa and Long-term upper ocean 
!     study site, JGR, 95,C9, 16,179--16,193. 
!!      
!!    AUTHOR
!!    ------
!!     C. Lebeaupin  *Météo-France* 
!!
!!    MODIFICATIONS
!!    -------------
!!      Original     02/2008
!!     01/2012 : H. Giordani, P. Peyrille
!!                       Add FLAGS: 
!!                       LREL_TS: relaxation on T, S
!!                       LREL_CUR: damping on current
!!                       FOR LREL_CUR: implicit and explicit codinf is made
!!                       for conveniency, DTREL=Ucur term, DSREL=VCURterm
!!                Corrections:
!!                   coriolis terms in current equation, 
!!                   richardson nb in diapycnal mixing, 
!!                   remove threshold value for mixing tendency
!!    07/2012, P. Le Moigne : CMO1D phasing
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_CSTS
USE MODD_OCEAN_CSTS
USE MODD_SEAFLUX_n, ONLY : XSEABATHY
USE MODD_OCEAN_n
USE MODD_OCEAN_GRID_n
!
USE MODD_SURF_PAR,   ONLY : XUNDEF
!
! Module containing relaxation fields
USE MODD_OCEAN_REL_n , ONLY : XSEAT_REL, XSEAS_REL, XTAU_REL, &
                            LREL_CUR, LREL_TS, &
                            XQCORR, LFLX_CORR, LDIAPYCNAL,&
                            XSEAU_REL, XSEAV_REL
!
USE MODD_SEAFLUX_GRID_n, ONLY :  XLAT
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
REAL, DIMENSION(:)  ,INTENT(IN)       :: PFSOL   ! solar flux (W/m2)
REAL, DIMENSION(:)  ,INTENT(IN)       :: PFNSOL  ! non solar flux (W/m2)
REAL, DIMENSION(:)  ,INTENT(IN)       :: PSFTEAU ! fresh water flux(kg/m2/s)
REAL, DIMENSION(:)  ,INTENT(IN)       :: PSFU    ! zonal stress (Pa)
REAL, DIMENSION(:)  ,INTENT(IN)       :: PSFV    ! meridian stress (Pa)
!
REAL, DIMENSION(:)  ,INTENT(OUT)    :: PSEATEMP! sea surface temperature (K)
!*      0.2    declarations of local variables
!
 COMPLEX, DIMENSION(NOCKMIN:NOCKMAX,2) :: ZUC           ! vecteur vent en ecriture complexe
 COMPLEX, DIMENSION(NOCKMIN:NOCKMAX) :: ZAI, ZBI, ZCI     !matrices pour resolution numérique
 COMPLEX, DIMENSION(NOCKMIN:NOCKMAX) :: ZAU, ZBU, ZCU, ZYU !matrices pour resolution numérique
 COMPLEX, DIMENSION(NOCKMIN:NOCKMAX) :: ZOMU,ZWU
!
REAL, DIMENSION(NOCKMIN:NOCKMAX) :: ZSEAT, ZSEAS, ZSEAE, ZSEAV, ZSEAU
REAL, DIMENSION(NOCKMIN:NOCKMAX) :: ZSEAT_REL, ZSEAS_REL, ZSEAV_REL, ZSEAU_REL
!
REAL, DIMENSION(NOCKMIN:NOCKMAX) :: ZA, ZB, ZC, ZA2, ZB2, ZC2, ZYT, ZYS, ZYE        !matrices pour resolution numérique
!
REAL, DIMENSION(NOCKMIN:NOCKMAX) :: ADVT,ADVS !advection horiz. temperature and salinity
REAL, DIMENSION(NOCKMIN:NOCKMAX) :: ADVU,ADVV !advection horiz. of current
REAL, DIMENSION(NOCKMIN:NOCKMAX) :: ADVE      !advection of turbulent kinetic energy
!
REAL, DIMENSION(NOCKMIN:NOCKMAX) :: ZTDTREL  ! Tendancy derived from  relxation (K/s)
REAL, DIMENSION(NOCKMIN:NOCKMAX) :: ZSDTREL  ! ---- salinity   ---------- (%/s)
REAL, DIMENSION(NOCKMIN:NOCKMAX) :: ZUDTREL  ! Tendancy term derived from relaxation on U current (m/s2)
REAL, DIMENSION(NOCKMIN:NOCKMAX) :: ZVDTREL  !  ------------------------------------    V        (m/s2)
!
REAL, DIMENSION(NOCKMIN:NOCKMAX) :: ZLE,ZKMEL,ZKMELM
REAL, DIMENSION(NOCKMIN:NOCKMAX) :: ZKMES,ZKMED,ZKMEWM,ZKMEWS
REAL, DIMENSION(NOCKMIN:NOCKMAX) :: ZPTH,ZPDY
REAL, DIMENSION(NOCKMIN:NOCKMAX) :: ZTENDE,ZDIFFV
REAL, DIMENSION(NOCKMIN:NOCKMAX) :: ZOMT, ZOMS, ZOME !vector for matrix inversion
REAL, DIMENSION(NOCKMIN:NOCKMAX) :: ZWT, ZWS, ZWE
REAL, DIMENSION(NOCKMIN:NOCKMAX) :: ZU,ZV,ZT,ZS,ZE
REAL, DIMENSION(NOCKMIN:NOCKMAX) :: ZZDRHO
REAL, DIMENSION(NOCKMIN:NOCKMAX) :: ZDTFSOL
!
REAL :: ZDTFNSOL
!
REAL :: ZSFU, ZSFV, ZFNSOL, ZFSOL, ZSFTEAU, ZLAT
REAL :: ZSEAHMO, ZSEATEMP
!
REAL :: ZSEUIL,ZEMIN,ZEMAX,ZTEST
!
REAL :: ZF, ZEWS
REAL :: ZALG
REAL :: ZEE,ZPOT,ZXLME,ZXLPE,ZXROD,ZAUX,ZXDL
REAL :: ZDU,ZDV,ZRICH,ZDRHODZ
REAL :: ZT1, ZT2, ZT3, ZS1, ZS2
!
INTEGER :: IUP,IBOT
INTEGER :: J,JJ,JPT,JIN,IKHML
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!----------------------------------------------------------------------------
!       1.     Initializations
!__________________________________________________________________________
!Take account of an horizontale advection

!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('MIXTL_N',0,ZHOOK_HANDLE)
IUP=NOCKMIN+1
IBOT=NOCKMAX
!
ZSEUIL=2.23E-3
ZEMIN=1.E-3
ZEMAX=0.1
!
ZT1=13.5
ZT2=-0.19494
ZT3=-0.49038E-2
ZS1=32.6
ZS2=0.77475
!
ZALG = XG / XRHOSW
!
!-------------------------------------------------------------------------------
!
!iterations on grid points
DO JPT=1,SIZE(PFSOL)
!
  ZTEST=0.
  IKHML=1
!
  !simplified variables inside this loop
  ZLAT    = XLAT   (JPT)
  ZFSOL   = PFSOL  (JPT)
  ZFNSOL  = PFNSOL (JPT)
  ZSFTEAU = PSFTEAU(JPT)
  ZSFU    = PSFU   (JPT)
  ZSFV    = PSFV   (JPT)
  ZEWS    = SQRT(ZSFU**2+ZSFV**2)/XRHOSW
  ZF      = 4.*XPI*SIN(ZLAT*XPI/180.)/86400.

  ZSEAHMO=0.
  DO J=IUP-1,IBOT
    ZSEAT(J) = XSEAT(JPT,J)
    ZSEAS(J) = XSEAS(JPT,J)
    ZSEAU(J) = XSEAU(JPT,J)
    ZSEAV(J) = XSEAV(JPT,J)    
    ZSEAE(J) = XSEAE(JPT,J)
    !
    ZSEAU_REL(J) = XSEAU_REL(JPT,J)
    ZSEAV_REL(J) = XSEAV_REL(JPT,J)
    ZSEAT_REL(J) = XSEAT_REL(JPT,J)
    ZSEAS_REL(J) = XSEAS_REL(JPT,J)
    !
    IF (J>=IUP .AND. ZSEAE(J)>=(ZEMIN*SQRT(2.))) ZSEAHMO = ZSEAHMO-XDZ1(J)
  ENDDO
  XSEAHMO(JPT) = ZSEAHMO

 !precalculation of DRHO
  DO J=IUP-1,IBOT
    ZU(J)=0.
    ZV(J)=0.
    ZT(J)=0.
    ZS(J)=0.
    ZE(J)=0.
    ADVT(J)=0.
    ADVS(J)=0.
    ADVU(J)=0.
    ADVV(J)=0.
    ADVE(J)=0.
    ZZDRHO(J)=(ZSEAT(J)-ZT1)*(ZT2+ZT3*(ZSEAT(J)-ZT1)) + ZS2*(ZSEAS(J)-ZS1)
    ZUDTREL(J)=0.
    ZVDTREL(J)=0.
    ZTDTREL(J)=0.
    ZSDTREL(J)=0.
    ZDTFSOL(J)=0.
  ENDDO
  ZDTFNSOL=0.
!
! Control print
!IF (LREL_CUR)  WRITE(*,*) "WARNING :: Damping on current will be done"
!IF (LREL_TS)  WRITE(*,*) "WARNING :: Relaxation on T, S ocean will be done "
!IF (LDIAPYCNAL)  WRITE(*,*) "WARNING :: diapycnal mixing has been activated"
!IF (LFLX_CORR) WRITE(*,*) "WARNING :: ocean fluxes correctin has been activated"
!
!------------------------------------------------------------------------------
!
!       2.     Oceanic vertical mixing 
!              -----------------------
!!
!!       2.a    Diapycnal mixing
!!              ----------------
!!
  DO J=IBOT-1,IUP,-1
    ZKMES(J) =0.
    ZKMEWM(J)=0.
    ZKMEWS(J)=0.
    ZKMED(J) =0.
    IF ((ZTEST==0.).AND.(ZSEAE(J)>=ZSEUIL)) THEN
      IKHML=J
      ZTEST=1.
    ENDIF
  ENDDO
  
  !! Modif PP - HG : flag diapycnal
  IF (LDIAPYCNAL) THEN
    !
    DO J=IKHML,IBOT-1
      ZDRHODZ=(ZZDRHO(J)-ZZDRHO(J+1))/XDZ1(J)
      ZDU=ZSEAU(J+1)-ZSEAU(J)
      ZDV=ZSEAV(J+1)-ZSEAV(J)
      IF((ZDU*ZDU+ZDV*ZDV).LE.1.E-7) THEN 
        ZRICH = 0.8
      ELSE
        ZRICH = -ZALG*ZDRHODZ/(ZDU**2+ZDV**2)/XK4(J)
      ENDIF  
!coefficient de mélange aux ondes internes
      ZKMEWM(J)=1.E-3
      ZKMEWS(J)=1.E-4
!coefficient de mélange du au cisaillement
      IF(ZRICH>0.7) THEN
        ZKMES(J) = 0.
      ELSEIF(ZRICH>=0.) THEN
        ZKMES(J) = 5.E-3*(1.-(ZRICH/0.7)*(ZRICH/0.7))**3
      ELSE
        ZKMES(J) = 5.E-3
      ENDIF
    ENDDO
    !plm si ldiapycnal=F zkmes non modofie et zrich ne sert a rien !
    !plm ELSE
    !plm  ZRICH=-ZALG*ZDRHODZ / (ZDU**2+ZDV**2) / XK4(J) 
    !  
  ENDIF
!
!       2.b    Mixing length and coefficient 
!              -----------------------------
!
  DO J=IUP,IBOT-1

    ZEE=ZSEAE(J)**2/ZALG
    ZXROD=(ZZDRHO(J)+ ZZDRHO(J+1))*0.5   

    ZXLME=0.
    ZPOT=0.
    JIN=J
    DO JJ=J+1,IBOT
      ZAUX=ZPOT + XDZ2(JJ)*(ZZDRHO(JJ)-ZXROD)
      IF (ZAUX<ZEE) THEN
        JIN=JJ
        ZPOT=ZAUX
        ZXLME=ZXLME+XDZ2(JJ)
      ENDIF
    ENDDO
    IF (JIN==J) THEN
      ZXLME=2.*(XZ2(J)-XZHOC(J+1))/(ZZDRHO(J+1)-ZXROD)/ZALG
      ZXLME=SQRT(ZXLME)*ZSEAE(J)
    ELSE
      IF (JIN/=IBOT) THEN
        ZXDL=(ZEE-ZPOT)/(ZZDRHO(JIN+1)-ZXROD)
        ZXLME=ZXLME+ZXDL
      ENDIF
    ENDIF

    ZXLPE=0.
    ZPOT=0.
    JIN=J
    DO JJ=J-1,NOCKMIN,-1
      ZAUX=ZPOT + XDZ2(JJ+1)*(ZXROD-ZZDRHO(JJ+1))
      IF (ZAUX<ZEE) THEN
        JIN=JJ
        ZPOT=ZAUX
        ZXLPE=ZXLPE+XDZ2(JJ+1)
      ENDIF
    ENDDO
    IF (JIN==J) THEN
      ZXLPE=2.*(XZ2(J)-XZHOC(J))/(ZZDRHO(J)-ZXROD)/ZALG
      ZXLPE=SQRT(ZXLPE)*ZSEAE(J)
    ELSE 
      IF (JIN/=NOCKMIN) THEN
        ZXDL=- (ZEE-ZPOT)/(ZZDRHO(JIN)-ZXROD)
        ZXLPE=ZXLPE+ZXDL
      ENDIF
    ENDIF

    ZLE(J)=SQRT(ZXLME*ZXLPE)
    ZKMEL(J)=XCKL*ZLE(J)*ZSEAE(J)
  
  ENDDO

  ZLE(IBOT)=ZLE(IBOT-1)
  ZKMEL(IBOT)=XCKL*ZLE(IBOT)*ZSEAE(IBOT)

  !first coef at all levels: because needed at j+1 further
  ZKMELM(IUP)=ZKMEL(IUP)
  DO J=IUP+1,IBOT
    ZKMELM(J)= (ZKMEL(J)+ZKMEL(J-1))/2.
  ENDDO

!--------------------------------------------------------------------------------------------------------
!
!!       2.c    Numerical resolution of evolution equations
!!              -------------------------------------------
!
  DO J=IUP,IBOT
    IF (LREL_CUR) THEN
      ZUDTREL(J) =  - (ZSEAU(J)-ZSEAU_REL(J))  / XTAU_REL 
      ZVDTREL(J) =  - (ZSEAV(J)-ZSEAV_REL(J))  / XTAU_REL 
    ENDIF
    ! flux solaire
    ZDTFSOL(J) = XRAY(J)*ZFSOL/XDZ2(J) 
  ENDDO
!
  IF (LREL_TS) THEN
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! RELAXATION IS MADE INSTEAD OF FLUX CORRECTION
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!  
    DO J=IUP,IBOT
      ! flux non solaire
      ZTDTREL(J) =  - (ZSEAT(J)-ZSEAT_REL(J)) / XTAU_REL
      ZSDTREL(J) =  - (ZSEAS(J)-ZSEAS_REL(J)) / XTAU_REL
    ENDDO
  ENDIF
!
  IF (LFLX_CORR) THEN
    ! NO relaxation
    ! Barnier correction on surface fluxes          
    ! flux non solaire corrige
    ZTDTREL(IUP) =  XQCORR *(ZSEAT_REL(IUP)-ZSEAT(IUP)) / (XRHOSW*XCPSW)
    ZFNSOL = ZFNSOL + ZTDTREL(IUP)
  ENDIF
!
! flux non solaire
  ZDTFNSOL = ZFNSOL/XDZ2(IUP) 
!
!!
!loop on levels
  ZA (:) = 0.
  ZAI(:) = 0.
  ZA2(:) = 0.
  ZC (:) = 0.
  ZCI(:) = 0.
  ZC2(:) = 0.
  DO J=IUP,IBOT

    IF (J<IBOT) THEN
      IF (J>IUP) THEN
        ZA (J) = XOCEAN_TSTEP * XK1(J) * (ZKMEL(J-1) + ZKMES(J-1) + ZKMEWS(J-1) + ZKMED(J-1)) 
        ZAI(J) = XOCEAN_TSTEP * XK1(J) * (ZKMEL(J-1) + ZKMES(J-1) + ZKMEWM(J-1) + ZKMED(J-1))
        ZA2(J) = XOCEAN_TSTEP * XK2(J) * ZKMELM(J)
      ENDIF
      ZC (J) = XOCEAN_TSTEP * XK2(J) * (ZKMEL(J)   + ZKMES(J)   + ZKMEWS(J)   + ZKMED(J)) 
      ZCI(J) = XOCEAN_TSTEP * XK2(J) * (ZKMEL(J)   + ZKMES(J)   + ZKMEWM(J)   + ZKMED(J))
      ZC2(J) = XOCEAN_TSTEP * XK3(J) * ZKMELM(J+1)
    ENDIF
  
    ZB (J) = 1. - ZA (J) - ZC (J)
    ZBI(J) = 1. - ZAI(J) - ZCI(J)
    ZB2(J) = 1. - ZA2(J) - ZC2(J) + XOCEAN_TSTEP * ZSEAE(J)/ZLE(J)/XZCE

    ZAU(J)    = ZAI(J) * (1.,0.) 
    ZCU(J)    = ZCI(J) * (1.,0.) 
    ZBU(J)    = ZBI(J) * (1.,0.) + XOCEAN_TSTEP * ZF * XGAMA * (0.,1.)
    

    ZOMT(J)   = 1./(ZB (J) - ZA (J) * ZOMT(J-1) * ZC (J-1)) 
    ZOMS(J)   = 1./(ZB (J) - ZA (J) * ZOMS(J-1) * ZC (J-1))    
    ZOMU(J)   = 1./(ZBU(J) - ZAU(J) * ZOMU(J-1) * ZCU(J-1)) 
    ZOME(J)   = 1./(ZB2(J) - ZA2(J) * ZOME(J-1) * ZC2(J-1))
 

    ZYT(J)    = ZSEAT(J) + XOCEAN_TSTEP * (ZDTFSOL(J) + ADVT(J)) 
    ZYS(J)    = ZSEAS(J) + XOCEAN_TSTEP * (             ADVS(J))  
    IF (LREL_TS) THEN
      IF (.NOT.LFLX_CORR) ZYT(J) = ZYT(J) + XOCEAN_TSTEP * ZTDTREL(J)
      ZYS(J) = ZYS(J) + XOCEAN_TSTEP * ZSDTREL(J)
    ENDIF
    
    ZUC(J,1)  = ZSEAU(J)*(1.,0.) + ZSEAV(J)*(0.,1.)
    ZYU(J)    = ZUC(J,1) + XOCEAN_TSTEP * (ZUC(J,1)*ZF*(1.-XGAMA)*(0.,-1.) + ADVU(J)*(1.,0.) + ADVV(J)*(0.,1.))
    ! damping on current if LREL_CUR=T in explicit scheme
    IF (LREL_CUR) ZYU(J) = ZYU(J) + XOCEAN_TSTEP * (ZUDTREL(J)*(1.,0.) + ZVDTREL(J)*(0.,1.))  

    IF (J<IBOT) THEN
      ZDRHODZ   = (ZZDRHO(J)-ZZDRHO(J+1))/XDZ1(J)
      ZDU       = ZSEAU(J+1)-ZSEAU(J)
      ZDV       = ZSEAV(J+1)-ZSEAV(J)

      ZPTH(J)   = XCKL * ZLE(J) * ZALG * ZDRHODZ
      ZPDY(J)   = XCKL * ZLE(J) * XK4(J) * (ZDU**2+ZDV**2)
    ELSE
      ZPTH(J) = ZPTH(J-1)
      ZPDY(J) = ZPDY(J-1)
    ENDIF

    ZYE(J)    = ZSEAE(J) + XOCEAN_TSTEP * (0.5 * ZSEAE(J)**2/ZLE(J)/XZCE + ADVE(J)) + ZPTH(J) + ZPDY(J)

    IF (J==IUP) THEN
      ZYT(J) = ZYT(J) + XOCEAN_TSTEP * ZDTFNSOL
      ZYS(J) = ZYS(J) + XOCEAN_TSTEP * ZSEAS(IUP) * ZSFTEAU / XDZ2(IUP)
      ZYU(J) = ZYU(J) - XOCEAN_TSTEP * ( ZSFU*(1.,0.) + ZSFV*(0.,1.) ) / XDZ2(IUP) / XRHOSW
      ZYE(J) = ZYE(J) + XOCEAN_TSTEP * ZEWS / XDZ1(IUP)
    ENDIF


    ZWT(J)    = ZOMT(J) * (ZYT(J)- ZA (J)*ZWT(J-1))
    ZWS(J)    = ZOMS(J) * (ZYS(J)- ZA (J)*ZWS(J-1))
    ZWU(J)    = ZOMU(J) * (ZYU(J)- ZAU(J)*ZWU(J-1))
    ZWE(J)    = ZOME(J) * (ZYE(J)- ZA2(J)*ZWE(J-1))

  ENDDO
!
!---------------------------------------------------------------------------------------------------
!
  ZT(IBOT)    = ZWT(IBOT)
  ZS(IBOT)    = ZWS(IBOT)  
  ZUC(IBOT,2) = ZWU(IBOT)
  ZE(IBOT)    = ZWE(IBOT)  
  DO J=IBOT-1,IUP,-1
    ZT (J)   = ZWT(J) - ZC (J) * ZOMT(J) * ZT (J+1)
    ZS (J)   = ZWS(J) - ZC (J) * ZOMS(J) * ZS (J+1)
    ZUC(J,2) = ZWU(J) - ZCU(J) * ZOMU(J) * ZUC(J+1,2)
    ZE (J)   = ZWE(J) - ZC2(J) * ZOME(J) * ZE (J+1)
  ENDDO
!
!
  DO J=IUP,IBOT
    ZU(J)  = REAL (ZUC(J,2))
    ZV(J)  = AIMAG(ZUC(J,2))
    !
    ZE(J)  = MAX(ZEMIN,ZE(J))
  ! Transformation to preserve E <EMAX; secure if mixt crash
    ZE(J)  = MIN(ZE(J),ZEMAX)
  !bilan TKE
    !ZTENDE(J) = (ZE(J)*ZE(J)-ZSEAE(J)**2)/XOCEAN_TSTEP
    !ZDIFFV(J) = ZTENDE(J) - ZSEAE(J)*(ZPDY(J) + ZPTH(J))
    !
    ZSEAT(J)  = ZT(J)
    ZSEAS(J)  = ZS(J)    
    ZSEAU(J)  = ZU(J)
    ZSEAV(J)  = ZV(J)
    ZSEAE(J)  = ZE(J)
  ENDDO
!
!------------------------------------------------------------------------------
!!       3.     New oceanic profiles
!!              --------------------
!!
  IF (LPROGSST) XSEATEND(JPT) = (ZT(IUP)-ZSEAT(IUP)) / XOCEAN_TSTEP
  ZSEAT(NOCKMIN)  = ZT(IUP)
  ZSEAS(NOCKMIN)  = ZS(IUP)  
  ZSEAU(NOCKMIN)  = ZU(IUP)
  ZSEAV(NOCKMIN)  = ZV(IUP)
  ZSEAE(NOCKMIN)  = ZE(IUP)

  !bathymetrie
  DO J=IUP,IBOT
    IF (XSEABATH(JPT,J)==0.) THEN
      ZSEAT(J)  = ZSEAT(J-1) 
      ZSEAS(J)  = ZSEAS(J-1)            
      ZSEAU(J)  = ZSEAU(J-1)
      ZSEAV(J)  = ZSEAV(J-1)
      ZSEAE(J)  = ZSEAE(J-1)
    ENDIF
  ENDDO
!
!SST diagnosticed with 1D oceanic model
  ZSEATEMP = ZSEAT(IUP) + XTT
  PSEATEMP(JPT) = ZSEATEMP
!
  DO J=IUP,IBOT
    XLE   (JPT,J) = ZLE   (J)
    XLK   (JPT,J) = ZLE   (J)
    XKMEL (JPT,J) = ZKMEL (J)
    XKMELM(JPT,J) = ZKMELM(J)
  ENDDO

  DO J=IUP-1,IBOT
    XSEAT(JPT,J) = ZSEAT(J)
    XSEAS(JPT,J) = ZSEAS(J)
    XSEAU(JPT,J) = ZSEAU(J)
    XSEAV(JPT,J) = ZSEAV(J)
    XSEAE(JPT,J) = ZSEAE(J)
    XDTFSOL(JPT,J) = ZDTFSOL(J)
  ENDDO

  XDTFNSOL(JPT) = ZDTFNSOL

ENDDO
!  
IF (LHOOK) CALL DR_HOOK('MIXTL_N',1,ZHOOK_HANDLE)
!
!!-------------------------------------------------------------------------------
!!
!ENDDO !end of iterations on sea surfex grid points
!!------------------------------------------------------------------------------
!
END SUBROUTINE MIXTL_n
