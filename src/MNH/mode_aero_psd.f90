!ORILAM_LIC Copyright 2006-2022 CNRS, Meteo-France and Universite Paul Sabatier
!ORILAM_LIC This is part of the ORILAM software governed by the CeCILL-C licence
!ORILAM_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!ORILAM_LIC for details.
!-----------------------------------------------------------------
!!   ########################
MODULE MODE_AERO_PSD
!!   ########################
!!
!! MODULE DUST PSD (Particle Size Distribution)
!! Purpose: Contains subroutines to convert from transported variables (ppv)
!! to understandable aerosol variables, e.g. #/m3, kg/m3, sigma, R_{n}

USE MODD_CH_AEROSOL
USE MODD_CONF,    ONLY : CPROGRAM
USE MODD_CH_MNHC_n, ONLY : LCH_INIT_FIELD
USE MODD_CST, ONLY :    &
       XPI              & !Definition of pi
      ,XAVOGADRO        & ![molec/mol] avogadros number
      ,XMD                ![kg/mol] molar weight of air
USE MODD_CST, ONLY : XMNH_TINY
USE MODD_CSTS_DUST, ONLY : XDENSITY_DUST
!
IMPLICIT NONE
! 
CONTAINS
! 
!    ############################################################
  SUBROUTINE PPP2AERO(             &
       PSVT                         & !I [ppv] input scalar variables (moment of distribution)
       , PRHODREF                   & !I [kg/m3] density of air       
       , PSIG3D                     & !O [-] standard deviation of aerosol distribution
       , PRG3D                      & !O [um] number median diameter of aerosol distribution
       , PN3D                       & !O [#/m3] number concentration of aerosols
       , PCTOTA                     & !O [ug/m3] mass of each aerosol compounds
       , PM3D                       & !O moments 0, 3 and 6
       , PMI                        & !O [g/mol] molecular weight
       )
!    ############################################################
!
!!
!!    PURPOSE
!!    -------
!!    Translate the three moments M0, M3 and M6 given in ppv into
!!    Values which can be understood more easily (R, sigma, N, M)
!! 
!!    CALLING STRUCTURE NOTE: OPTIONAL VARIABLES
!!    -------
!!    CALL PPP2AEROS(PSVT, PRHODREF, PSIG3D=SIGVAR,  &
!!       PRG3D=RVAR, PN3D=NVAR, PM3D=ZM)
!!
!!    REFERENCE
!!    ---------
!!    none
!!
!!    AUTHOR
!!    ------
!!    Pierre TULET (LA)
!!
!!    MODIFICATIONS
!!    -------------
!!    Alf Grini (CNRM)
!!    M.Leriche 2015 : masse molaire Black carbon à 12 g/mol
!!
!!    EXTERNAL
!!    --------
!!
    IMPLICIT NONE
!!
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
!*      0.1    declarations of arguments
!
REAL,    DIMENSION(:,:,:,:),  INTENT(IN)     :: PSVT     !I [#/molec_{air}] first moment
                                                             !I [molec_{aer}/molec_{air} 3rd moment
                                                             !I [um6/molec_{air}*(cm3/m3)] 6th moment
REAL,    DIMENSION(:,:,:),    INTENT(IN)     :: PRHODREF !I [kg/m3] density of air

REAL,    DIMENSION(:,:,:,:),  OPTIONAL, INTENT(OUT)   :: PSIG3D   !O [-] standard deviation
REAL,    DIMENSION(:,:,:,:),  OPTIONAL, INTENT(OUT)   :: PRG3D    !O [um] number median diameter
REAL,    DIMENSION(:,:,:,:),  OPTIONAL, INTENT(OUT)   :: PN3D     !O [#/m3] number concentration
REAL,    DIMENSION(:,:,:,:,:),OPTIONAL, INTENT(OUT)   :: PCTOTA   !O [ug/m3] mass of each component
REAL,    DIMENSION(:,:,:,:),  OPTIONAL, INTENT(OUT)   :: PM3D     !O moments 0,3 and 6 
REAL,    DIMENSION(:,:,:,:),  OPTIONAL, INTENT(IN)    :: PMI ! molecular weight
!
!*      0.2    declarations local variables
!
REAL, DIMENSION(SIZE(PSVT,1), SIZE(PSVT,2), SIZE(PSVT,3),NSP+NCARB+NSOA):: ZMI                 ! [g/mol] molar weight of aerosol
REAL,DIMENSION(SIZE(PSVT,1), SIZE(PSVT,2), SIZE(PSVT,3),SIZE(PSVT,4)) :: ZSV ! [aerosol concentration]
REAL,DIMENSION(SIZE(PSVT,1), SIZE(PSVT,2), SIZE(PSVT,3)) :: ZSIGMA   ! [-] standard deviation
REAL,DIMENSION(SIZE(PSVT,1),SIZE(PSVT,2),SIZE(PSVT,3),NSP+NCARB+NSOA,JPMODE):: ZCTOTA
REAL,DIMENSION(SIZE(PSVT,1),SIZE(PSVT,2),SIZE(PSVT,3),JPMODE*3) :: ZM
REAL,DIMENSION(SIZE(PSVT,1),SIZE(PSVT,2),SIZE(PSVT,3),JPMODE) :: ZMASK, ZRG

REAL,DIMENSION(NSP+NCARB+NSOA)       :: ZFAC                 ! M3 / mass conversion factor
REAL,DIMENSION(NSP+NCARB+NSOA)       :: ZRHOI                ! aerosol density
REAL                                 :: ZDEN2MOL
REAL,DIMENSION(JPMODE*3)             :: ZPMIN               ! [aerosol units] minimum values for N, sigma, M
INTEGER                              :: JI,JJ,JK,JSV, JN              ! [idx] loop counters
REAL    :: ZINIRADIUSI, ZINIRADIUSJ
!
!-------------------------------------------------------------------------------
!
!        1.    initialisation 

IF (CRGUNIT=="MASS") THEN
  ZINIRADIUSI = XINIRADIUSI * EXP(-3.*(LOG(XINISIGI))**2)
  ZINIRADIUSJ = XINIRADIUSJ * EXP(-3.*(LOG(XINISIGJ))**2)
ELSE
  ZINIRADIUSI = XINIRADIUSI 
  ZINIRADIUSJ = XINIRADIUSJ
END IF
! Moments index
NM0(1) = 1
NM3(1) = 2
NM6(1) = 3
NM0(2) = 4
NM3(2) = 5
NM6(2) = 6

    !Get minimum values possible
    ZPMIN(1) = XN0IMIN
    ZPMIN(2) = ZPMIN(1) * (ZINIRADIUSI**3)*EXP(4.5 * LOG(XINISIGI)**2) 
    ZPMIN(3) = ZPMIN(1) * (ZINIRADIUSI**6)*EXP(18. * LOG(XINISIGI)**2)

    ZPMIN(4) = XN0JMIN
    ZPMIN(5) = ZPMIN(4) * (ZINIRADIUSJ**3)*EXP(4.5 * LOG(XINISIGJ)**2) 
    ZPMIN(6) = ZPMIN(4) * (ZINIRADIUSJ**6)*EXP(18. * LOG(XINISIGJ)**2)

! Cf Ackermann (all to black carbon except water)
!Set molecular weightn g/mol 
ZRHOI(:) = 1.8e3
ZRHOI(JP_AER_H2O) = 1.0e3   ! water
ZRHOI(JP_AER_DST) = XDENSITY_DUST  ! dusts

IF(PRESENT(PMI)) THEN
  ZMI(:,:,:,:) = PMI(:,:,:,:)
ELSE
    ZMI(:,:,:,:) = 250.
    ZMI(:,:,:,JP_AER_SO4)  = 98.
    ZMI(:,:,:,JP_AER_NO3)  = 63.
    ZMI(:,:,:,JP_AER_NH3)  = 17.
    ZMI(:,:,:,JP_AER_H2O)  = 18.
    ZMI(:,:,:,JP_AER_BC)   = 12.
    ZMI(:,:,:,JP_AER_DST)  = 100.
    IF (NSOA .EQ. 10) THEN
    ZMI(:,:,:,JP_AER_SOA1) = 88. 
    ZMI(:,:,:,JP_AER_SOA2) = 180.
    ZMI(:,:,:,JP_AER_SOA3) = 1.5374857E2
    ZMI(:,:,:,JP_AER_SOA4) = 1.9586780E2
    ZMI(:,:,:,JP_AER_SOA5) = 195.
    ZMI(:,:,:,JP_AER_SOA6) = 195.
    ZMI(:,:,:,JP_AER_SOA7) = 165.
    ZMI(:,:,:,JP_AER_SOA8) = 195.
    ZMI(:,:,:,JP_AER_SOA9) = 270.
    ZMI(:,:,:,JP_AER_SOA10) = 210.
    END IF
ENDIF
! conversion into mol.cm-3
ZDEN2MOL = 1E-6 * XAVOGADRO  / XMD
!
DO JJ=1, SIZE(PSVT,4)
  ZSV(:,:,:,JJ) =  PSVT(:,:,:,JJ) * ZDEN2MOL * PRHODREF(:,:,:)
ENDDO
ZSV(:,:,:,:) = MAX(ZSV(:,:,:,:), XMNH_TINY)
!
DO JJ=1,NSP+NCARB+NSOA
  ZFAC(JJ)=(4./3.)*XPI*ZRHOI(JJ)*1.e-9
ENDDO
!
!-------------------------------------------------------------------------------
!
!*       2    transfer aerosol mass from gas to aerosol variables
!               (and conversion of mol.cm-3 --> microgram/m3)
!
ZCTOTA(:,:,:,:,:) = 0.
! aerosol phase
  ZCTOTA(:,:,:,JP_AER_SO4,1) = ZSV(:,:,:,JP_CH_SO4i)*ZMI(:,:,:,JP_AER_SO4)/6.0221367E+11
  ZCTOTA(:,:,:,JP_AER_SO4,2) = ZSV(:,:,:,JP_CH_SO4j)*ZMI(:,:,:,JP_AER_SO4)/6.0221367E+11

  ZCTOTA(:,:,:,JP_AER_NO3,1) = ZSV(:,:,:,JP_CH_NO3i)*ZMI(:,:,:,JP_AER_NO3)/6.0221367E+11
  ZCTOTA(:,:,:,JP_AER_NO3,2) = ZSV(:,:,:,JP_CH_NO3j)*ZMI(:,:,:,JP_AER_NO3)/6.0221367E+11

  ZCTOTA(:,:,:,JP_AER_NH3,1) = ZSV(:,:,:,JP_CH_NH3i)*ZMI(:,:,:,JP_AER_NH3)/6.0221367E+11
  ZCTOTA(:,:,:,JP_AER_NH3,2) = ZSV(:,:,:,JP_CH_NH3j)*ZMI(:,:,:,JP_AER_NH3)/6.0221367E+11
!
! water
  ZCTOTA(:,:,:,JP_AER_H2O,1) = ZSV(:,:,:,JP_CH_H2Oi)*ZMI(:,:,:,JP_AER_H2O)/6.0221367E+11
  ZCTOTA(:,:,:,JP_AER_H2O,2) = ZSV(:,:,:,JP_CH_H2Oj)*ZMI(:,:,:,JP_AER_H2O)/6.0221367E+11
!
! primary organic carbon
  ZCTOTA(:,:,:,JP_AER_OC,1) = ZSV(:,:,:,JP_CH_OCi)*ZMI(:,:,:,JP_AER_OC)/6.0221367E+11
  ZCTOTA(:,:,:,JP_AER_OC,2) = ZSV(:,:,:,JP_CH_OCj)*ZMI(:,:,:,JP_AER_OC)/6.0221367E+11
!
! primary black carbon
  ZCTOTA(:,:,:,JP_AER_BC,1) = ZSV(:,:,:,JP_CH_BCi)*ZMI(:,:,:,JP_AER_BC)/6.0221367E+11
  ZCTOTA(:,:,:,JP_AER_BC,2) = ZSV(:,:,:,JP_CH_BCj)*ZMI(:,:,:,JP_AER_BC)/6.0221367E+11
!dust
 ZCTOTA(:,:,:,JP_AER_DST,1) = ZSV(:,:,:,JP_CH_DSTi)*ZMI(:,:,:,JP_AER_DST)/6.0221367E+11
 ZCTOTA(:,:,:,JP_AER_DST,2) = ZSV(:,:,:,JP_CH_DSTj)*ZMI(:,:,:,JP_AER_DST)/6.0221367E+11
!
IF (NSOA .EQ. 10) THEN
  ZCTOTA(:,:,:,JP_AER_SOA1,1) = ZSV(:,:,:,JP_CH_SOA1i)*ZMI(:,:,:,JP_AER_SOA1)/6.0221367E+11
  ZCTOTA(:,:,:,JP_AER_SOA1,2) = ZSV(:,:,:,JP_CH_SOA1j)*ZMI(:,:,:,JP_AER_SOA1)/6.0221367E+11
  ZCTOTA(:,:,:,JP_AER_SOA2,1) = ZSV(:,:,:,JP_CH_SOA2i)*ZMI(:,:,:,JP_AER_SOA2)/6.0221367E+11
  ZCTOTA(:,:,:,JP_AER_SOA2,2) = ZSV(:,:,:,JP_CH_SOA2j)*ZMI(:,:,:,JP_AER_SOA2)/6.0221367E+11
  ZCTOTA(:,:,:,JP_AER_SOA3,1) = ZSV(:,:,:,JP_CH_SOA3i)*ZMI(:,:,:,JP_AER_SOA3)/6.0221367E+11
  ZCTOTA(:,:,:,JP_AER_SOA3,2) = ZSV(:,:,:,JP_CH_SOA3j)*ZMI(:,:,:,JP_AER_SOA3)/6.0221367E+11
  ZCTOTA(:,:,:,JP_AER_SOA4,1) = ZSV(:,:,:,JP_CH_SOA4i)*ZMI(:,:,:,JP_AER_SOA4)/6.0221367E+11
  ZCTOTA(:,:,:,JP_AER_SOA4,2) = ZSV(:,:,:,JP_CH_SOA4j)*ZMI(:,:,:,JP_AER_SOA4)/6.0221367E+11
  ZCTOTA(:,:,:,JP_AER_SOA5,1) = ZSV(:,:,:,JP_CH_SOA5i)*ZMI(:,:,:,JP_AER_SOA5)/6.0221367E+11
  ZCTOTA(:,:,:,JP_AER_SOA5,2) = ZSV(:,:,:,JP_CH_SOA5j)*ZMI(:,:,:,JP_AER_SOA5)/6.0221367E+11

  ZCTOTA(:,:,:,JP_AER_SOA6,1) = ZSV(:,:,:,JP_CH_SOA6i)*ZMI(:,:,:,JP_AER_SOA6)/6.0221367E+11
  ZCTOTA(:,:,:,JP_AER_SOA6,2) = ZSV(:,:,:,JP_CH_SOA6j)*ZMI(:,:,:,JP_AER_SOA6)/6.0221367E+11
  ZCTOTA(:,:,:,JP_AER_SOA7,1) = ZSV(:,:,:,JP_CH_SOA7i)*ZMI(:,:,:,JP_AER_SOA7)/6.0221367E+11
  ZCTOTA(:,:,:,JP_AER_SOA7,2) = ZSV(:,:,:,JP_CH_SOA7j)*ZMI(:,:,:,JP_AER_SOA7)/6.0221367E+11
  ZCTOTA(:,:,:,JP_AER_SOA8,1) = ZSV(:,:,:,JP_CH_SOA8i)*ZMI(:,:,:,JP_AER_SOA8)/6.0221367E+11
  ZCTOTA(:,:,:,JP_AER_SOA8,2) = ZSV(:,:,:,JP_CH_SOA8j)*ZMI(:,:,:,JP_AER_SOA8)/6.0221367E+11
  ZCTOTA(:,:,:,JP_AER_SOA9,1) = ZSV(:,:,:,JP_CH_SOA9i)*ZMI(:,:,:,JP_AER_SOA9)/6.0221367E+11
  ZCTOTA(:,:,:,JP_AER_SOA9,2) = ZSV(:,:,:,JP_CH_SOA9j)*ZMI(:,:,:,JP_AER_SOA9)/6.0221367E+11
  ZCTOTA(:,:,:,JP_AER_SOA10,1) = ZSV(:,:,:,JP_CH_SOA10i)*ZMI(:,:,:,JP_AER_SOA10)/6.0221367E+11
  ZCTOTA(:,:,:,JP_AER_SOA10,2) = ZSV(:,:,:,JP_CH_SOA10j)*ZMI(:,:,:,JP_AER_SOA10)/6.0221367E+11
END IF
!
!-------------------------------------------------------------------------------
!
!*       3    calculate moment 3 from total aerosol mass
!
ZM(:,:,:,2) = 0.
ZM(:,:,:,5) = 0.
DO JJ = 1,NSP+NCARB+NSOA
    ZM(:,:,:,2) = ZM(:,:,:,2)+ZCTOTA(:,:,:,JJ,1)/ZFAC(JJ) !==>um3_{aer}/m3_{air} (volume ==> 3rd moment)
    ZM(:,:,:,5) = ZM(:,:,:,5)+ZCTOTA(:,:,:,JJ,2)/ZFAC(JJ) !==>um3_{aer}/m3_{air} (volume ==> 3rd moment)
ENDDO
!
!-------------------------------------------------------------------------------
!
!*       4    set  moment 0 
!
IF ((CPROGRAM=="REAL  ").OR.(CPROGRAM=="IDEAL ")) THEN
  ZM(:,:,:,1)= ZM(:,:,:,2) / ((ZINIRADIUSI**3)*EXP(4.5 * LOG(XINISIGI)**2))  
  ZM(:,:,:,4)= ZM(:,:,:,5) / ((ZINIRADIUSJ**3)*EXP(4.5 * LOG(XINISIGJ)**2))
ELSE
  ZM(:,:,:,1)=   MAX(ZSV(:,:,:,JP_CH_M0i) * 1E+6, XMNH_TINY) ! molec_{aer}/m3_{air}
  ZM(:,:,:,4)=   MAX(ZSV(:,:,:,JP_CH_M0j) * 1E+6, XMNH_TINY) ! molec_{aer}/m3_{air}
END IF
!
!-------------------------------------------------------------------------------
!
!*       5    set moment 6  ==> um6_{aer}/m3_{air}
!
IF (LVARSIGI) THEN ! set M6 variable standard deviation
  IF ((CPROGRAM=="REAL  ").OR.(CPROGRAM=="IDEAL ")) THEN
  ZM(:,:,:,3)= ZM(:,:,:,1) * (ZINIRADIUSJ**6)*EXP(18. * LOG(XINISIGJ)**2)
  ELSE
  ZM(:,:,:,3) = MAX(ZSV(:,:,:,JP_CH_M6i), XMNH_TINY)

  ZSIGMA(:,:,:)=ZM(:,:,:,2)**2/(ZM(:,:,:,1)*ZM(:,:,:,3))
  ZSIGMA(:,:,:)=MIN(1-1E-10,ZSIGMA(:,:,:))
  ZSIGMA(:,:,:)=MAX(1E-10,ZSIGMA(:,:,:))
  ZSIGMA(:,:,:)= LOG(ZSIGMA(:,:,:))
  ZSIGMA(:,:,:)= EXP(1./3.*SQRT(-ZSIGMA(:,:,:)))

  ZM(:,:,:,3) = ZM(:,:,:,1) &
          * ( (ZM(:,:,:,2)/ZM(:,:,:,1))**(1./3.)  &
          * exp(-(3./2.)*log(ZSIGMA(:,:,:))**2))**6 &
          * exp(18.*log(ZSIGMA(:,:,:))**2)

  END IF
 IF(PRESENT(PSIG3D)) PSIG3D(:,:,:,1) = ZSIGMA(:,:,:)

ELSE ! fixed standard deviation
 ZM(:,:,:,3) = ZM(:,:,:,1) &
          * ( (ZM(:,:,:,2)/ZM(:,:,:,1))**(1./3.)  &
          * exp(-(3./2.)*log(XINISIGI)**2))**6 &
          * exp(18.*log(XINISIGI)**2)

 IF(PRESENT(PSIG3D)) PSIG3D(:,:,:,1) = XINISIGI
END IF

IF (LVARSIGJ) THEN ! set M6 variable standard deviation
  IF ((CPROGRAM=="REAL  ").OR.(CPROGRAM=="IDEAL ")) THEN
  ZM(:,:,:,6)= ZM(:,:,:,4) * (ZINIRADIUSJ**6)*EXP(18. * LOG(XINISIGJ)**2)
  ELSE
  ZM(:,:,:,6) = MAX(ZSV(:,:,:,JP_CH_M6j), XMNH_TINY)

  ZSIGMA(:,:,:)=ZM(:,:,:,5)**2/(ZM(:,:,:,4)*ZM(:,:,:,6))
  ZSIGMA(:,:,:)=MIN(1-1E-10,ZSIGMA(:,:,:))
  ZSIGMA(:,:,:)=MAX(1E-10,ZSIGMA(:,:,:))
  ZSIGMA(:,:,:)= LOG(ZSIGMA(:,:,:))
  ZSIGMA(:,:,:)= EXP(1./3.*SQRT(-ZSIGMA(:,:,:)))

  ZM(:,:,:,6) = ZM(:,:,:,4) &
          * ( (ZM(:,:,:,5)/ZM(:,:,:,4))**(1./3.)  &
          * exp(-(3./2.)*log(ZSIGMA(:,:,:))**2))**6 &
          * exp(18.*log(ZSIGMA(:,:,:))**2)
  END IF

  IF(PRESENT(PSIG3D)) PSIG3D(:,:,:,2) = ZSIGMA(:,:,:)

ELSE ! fixed standard deviation
  ZM(:,:,:,6) = ZM(:,:,:,4) &
          * ( (ZM(:,:,:,5)/ZM(:,:,:,4))**(1./3.)  &
          * exp(-(3./2.)*log(XINISIGJ)**2))**6 &
          * exp(18.*log(XINISIGJ)**2)

  IF(PRESENT(PSIG3D)) PSIG3D(:,:,:,2) = XINISIGJ
END IF


!-------------------------------------------------------------------------------
!
!*       6    calculate modal parameters from moments
!
DO JN=1,JPMODE
  IF(PRESENT(PN3D)) PN3D(:,:,:,JN) = ZM(:,:,:,NM0(JN))

  ZRG(:,:,:,JN) = (ZM(:,:,:,NM3(JN))**4. &
              / (ZM(:,:,:,NM6(JN))*ZM(:,:,:,NM0(JN))**3.))**(1./6.)


  IF(PRESENT(PRG3D)) PRG3D(:,:,:,JN)=(ZM(:,:,:,NM3(JN))**4. &
              / (ZM(:,:,:,NM6(JN))*ZM(:,:,:,NM0(JN))**3.))**(1./6.)

ENDDO
!
IF(PRESENT(PCTOTA)) PCTOTA(:,:,:,:,:) = ZCTOTA(:,:,:,:,:)
IF(PRESENT(PM3D)) PM3D(:,:,:,:) = ZM(:,:,:,:)
!
!
END SUBROUTINE PPP2AERO
!
!   ############################################################
  SUBROUTINE CON2MIX (PSVT         & !I [µg/m3] O [ppp] input scalar variables (moment of distribution)
                     ,PRHODREF     ) !I [kg/m3] density of air

!
!!    PURPOSE
!!    -------
!!     conversion from µg/m3 to moments (ppp) to init aerosol profile (ch_init_field)
!!
!!    REFERENCE
!!    ---------
!!    none
!!
!!    AUTHOR
!!    ------
!!    Pierre TULET (LA)
!!
!!    EXTERNAL
!!    --------
!!
    IMPLICIT NONE
!!
REAL,       DIMENSION(:,:,:,:),  INTENT(INOUT)     :: PSVT      !I [#/molec_{air}] first moment
                                                                !I [molec_{aer}/molec_{air} 3rd moment
                                                                !I [um6/molec_{air}*(cm3/m3)] 6th moment
REAL,       DIMENSION(:,:,:),  INTENT(IN)      :: PRHODREF !I [kg/m3] density of air

!*      0.2    declarations local variables
!
REAL,DIMENSION(SIZE(PSVT,1), SIZE(PSVT,2), SIZE(PSVT,3), JPMODE*3) :: ZM  ! aerosols moments
REAL,DIMENSION(SIZE(PSVT,1),SIZE(PSVT,2),SIZE(PSVT,3),NSP+NCARB+NSOA,JPMODE):: ZCTOTA

REAL,DIMENSION(NSP+NCARB+NSOA)       :: ZFAC                ! M3 / mass conversion factor
REAL,DIMENSION(NSP+NCARB+NSOA)       :: ZRHOI                ! aerosol density
REAL,DIMENSION(NSP+NCARB+NSOA)       :: ZMI                 ! molecular weight
INTEGER                              :: JJ                  ! [idx] loop counters
REAL                                 :: ZDEN2MOL
REAL    :: ZINIRADIUSI, ZINIRADIUSJ
!
!-------------------------------------------------------------------------------
!
!        1.    initialisation

ZRHOI(:) = 1.8e3
ZRHOI(JP_AER_H2O) = 1.0e3   ! water
ZRHOI(JP_AER_DST) = XDENSITY_DUST  ! dusts
ZMI(:) = 250.
ZMI(JP_AER_SO4)  = 98.
ZMI(JP_AER_NO3)  = 63.
ZMI(JP_AER_NH3)  = 17.
ZMI(JP_AER_H2O)  = 18.
ZMI(JP_AER_BC)   = 12.
ZMI(JP_AER_DST)  = 100.
IF (NSOA .EQ. 10) THEN
 ZMI(JP_AER_SOA1) = 88.
 ZMI(JP_AER_SOA2) = 180.
 ZMI(JP_AER_SOA3) = 1.5374857E2
 ZMI(JP_AER_SOA4) = 1.9586780E2
 ZMI(JP_AER_SOA5) = 195.
 ZMI(JP_AER_SOA6) = 195.
 ZMI(JP_AER_SOA7) = 165.
 ZMI(JP_AER_SOA8) = 195.
 ZMI(JP_AER_SOA9) = 270.
 ZMI(JP_AER_SOA10) = 210.
END IF


IF (CRGUNIT=="MASS") THEN
    ZINIRADIUSI = XINIRADIUSI * EXP(-3.*(LOG(XINISIGI))**2)
    ZINIRADIUSJ = XINIRADIUSJ * EXP(-3.*(LOG(XINISIGJ))**2)
ELSE
    ZINIRADIUSI = XINIRADIUSI
    ZINIRADIUSJ = XINIRADIUSJ
END IF

! conversion into mol.cm-3
ZDEN2MOL = 1E-6 * XAVOGADRO  / XMD

    
DO JJ=1,NSP+NCARB+NSOA
  ZFAC(JJ)=(4./3.)*XPI*ZRHOI(JJ)*1.e-9
ENDDO
!
! inorganic phase
  ZCTOTA(:,:,:,JP_AER_SO4,1) = PSVT(:,:,:,JP_CH_SO4i)
  ZCTOTA(:,:,:,JP_AER_SO4,2) = PSVT(:,:,:,JP_CH_SO4j)
  ZCTOTA(:,:,:,JP_AER_NO3,1) = PSVT(:,:,:,JP_CH_NO3i)
  ZCTOTA(:,:,:,JP_AER_NO3,2) = PSVT(:,:,:,JP_CH_NO3j)
  ZCTOTA(:,:,:,JP_AER_NH3,1) = PSVT(:,:,:,JP_CH_NH3i)
  ZCTOTA(:,:,:,JP_AER_NH3,2) = PSVT(:,:,:,JP_CH_NH3j)
!
! water
  ZCTOTA(:,:,:,JP_AER_H2O,1) = PSVT(:,:,:,JP_CH_H2Oi)
  ZCTOTA(:,:,:,JP_AER_H2O,2) = PSVT(:,:,:,JP_CH_H2Oj)
!
! primary organic carbon
  ZCTOTA(:,:,:,JP_AER_OC,1) = PSVT(:,:,:,JP_CH_OCi)
  ZCTOTA(:,:,:,JP_AER_OC,2) = PSVT(:,:,:,JP_CH_OCj)
!
! primary black carbon
  ZCTOTA(:,:,:,JP_AER_BC,1) = PSVT(:,:,:,JP_CH_BCi)
  ZCTOTA(:,:,:,JP_AER_BC,2) = PSVT(:,:,:,JP_CH_BCj)
!dust
  ZCTOTA(:,:,:,JP_AER_DST,1) = PSVT(:,:,:,JP_CH_DSTi)
  ZCTOTA(:,:,:,JP_AER_DST,2) = PSVT(:,:,:,JP_CH_DSTj)
!
 IF (NSOA .EQ. 10) THEN
  ZCTOTA(:,:,:,JP_AER_SOA1,1) = PSVT(:,:,:,JP_CH_SOA1i)
  ZCTOTA(:,:,:,JP_AER_SOA1,2) = PSVT(:,:,:,JP_CH_SOA1j)
  ZCTOTA(:,:,:,JP_AER_SOA2,1) = PSVT(:,:,:,JP_CH_SOA2i)
  ZCTOTA(:,:,:,JP_AER_SOA2,2) = PSVT(:,:,:,JP_CH_SOA2j)
  ZCTOTA(:,:,:,JP_AER_SOA3,1) = PSVT(:,:,:,JP_CH_SOA3i)
  ZCTOTA(:,:,:,JP_AER_SOA3,2) = PSVT(:,:,:,JP_CH_SOA3j)
  ZCTOTA(:,:,:,JP_AER_SOA4,1) = PSVT(:,:,:,JP_CH_SOA4i)
  ZCTOTA(:,:,:,JP_AER_SOA4,2) = PSVT(:,:,:,JP_CH_SOA4j)
  ZCTOTA(:,:,:,JP_AER_SOA5,1) = PSVT(:,:,:,JP_CH_SOA5i)
  ZCTOTA(:,:,:,JP_AER_SOA5,2) = PSVT(:,:,:,JP_CH_SOA5j)

  ZCTOTA(:,:,:,JP_AER_SOA6,1) = PSVT(:,:,:,JP_CH_SOA6i)
  ZCTOTA(:,:,:,JP_AER_SOA6,2) = PSVT(:,:,:,JP_CH_SOA6j)
  ZCTOTA(:,:,:,JP_AER_SOA7,1) = PSVT(:,:,:,JP_CH_SOA7i)
  ZCTOTA(:,:,:,JP_AER_SOA7,2) = PSVT(:,:,:,JP_CH_SOA7j)
  ZCTOTA(:,:,:,JP_AER_SOA8,1) = PSVT(:,:,:,JP_CH_SOA8i)
  ZCTOTA(:,:,:,JP_AER_SOA8,2) = PSVT(:,:,:,JP_CH_SOA8j)
  ZCTOTA(:,:,:,JP_AER_SOA9,1) = PSVT(:,:,:,JP_CH_SOA9i)
  ZCTOTA(:,:,:,JP_AER_SOA9,2) = PSVT(:,:,:,JP_CH_SOA9j)
  ZCTOTA(:,:,:,JP_AER_SOA10,1) = PSVT(:,:,:,JP_CH_SOA10i)
  ZCTOTA(:,:,:,JP_AER_SOA10,2) = PSVT(:,:,:,JP_CH_SOA10j)
  END IF
  ZCTOTA(:,:,:,:,:) = MAX(ZCTOTA(:,:,:,:,:),XMNH_TINY)

!
!*       3    calculate moment 3 from total aerosol mass
!
ZM(:,:,:,2) = 0.
ZM(:,:,:,5) = 0.
  DO JJ = 1,NSP+NCARB+NSOA
    ZM(:,:,:,2) = ZM(:,:,:,2)+ZCTOTA(:,:,:,JJ,1)/ZFAC(JJ)
    ZM(:,:,:,5) = ZM(:,:,:,5)+ZCTOTA(:,:,:,JJ,2)/ZFAC(JJ)
  ENDDO
!
!
!*       4    calculate moment 0 from dispersion and mean radius
!
ZM(:,:,:,1)= ZM(:,:,:,2)/ &
              ( (ZINIRADIUSI**3)*EXP(4.5 * LOG(XINISIGI)**2) )

ZM(:,:,:,4)= ZM(:,:,:,5)/ &
              ( (ZINIRADIUSJ**3)*EXP(4.5 * LOG(XINISIGJ)**2) )
!

!*       5    calculate moment 6 from dispersion and mean radius
!
ZM(:,:,:,3) = ZM(:,:,:,1)*(ZINIRADIUSI**6) * EXP(18 *(LOG(XINISIGI))**2)
ZM(:,:,:,6) = ZM(:,:,:,4)*(ZINIRADIUSJ**6) * EXP(18 *(LOG(XINISIGJ))**2)

!*       6    return to ppp
! inorganic phase
 PSVT(:,:,:,JP_CH_SO4i) =  ZCTOTA(:,:,:,JP_AER_SO4,1)*6.0221367E+11/ZMI(JP_AER_SO4)
 PSVT(:,:,:,JP_CH_SO4j) =  ZCTOTA(:,:,:,JP_AER_SO4,2)*6.0221367E+11/ZMI(JP_AER_SO4)
 PSVT(:,:,:,JP_CH_NO3i) =  ZCTOTA(:,:,:,JP_AER_NO3,1)*6.0221367E+11/ZMI(JP_AER_NO3)
 PSVT(:,:,:,JP_CH_NO3j) =  ZCTOTA(:,:,:,JP_AER_NO3,2)*6.0221367E+11/ZMI(JP_AER_NO3)
 PSVT(:,:,:,JP_CH_NH3i) =  ZCTOTA(:,:,:,JP_AER_NH3,1)*6.0221367E+11/ZMI(JP_AER_NH3)
 PSVT(:,:,:,JP_CH_NH3j) =  ZCTOTA(:,:,:,JP_AER_NH3,2)*6.0221367E+11/ZMI(JP_AER_NH3)
!
! water
 PSVT(:,:,:,JP_CH_H2Oi) =  ZCTOTA(:,:,:,JP_AER_H2O,1)*6.0221367E+11/ZMI(JP_AER_H2O)
 PSVT(:,:,:,JP_CH_H2Oj) =  ZCTOTA(:,:,:,JP_AER_H2O,2)*6.0221367E+11/ZMI(JP_AER_H2O)
!
! primary organic carbon
 PSVT(:,:,:,JP_CH_OCi) =  ZCTOTA(:,:,:,JP_AER_OC,1)*6.0221367E+11/ZMI(JP_AER_OC)
 PSVT(:,:,:,JP_CH_OCj) =  ZCTOTA(:,:,:,JP_AER_OC,2)*6.0221367E+11/ZMI(JP_AER_OC)
!
! primary black carbon
 PSVT(:,:,:,JP_CH_BCi) =  ZCTOTA(:,:,:,JP_AER_BC,1)*6.0221367E+11/ZMI(JP_AER_BC)
 PSVT(:,:,:,JP_CH_BCj) =  ZCTOTA(:,:,:,JP_AER_BC,2)*6.0221367E+11/ZMI(JP_AER_BC)
!dust
 PSVT(:,:,:,JP_CH_DSTi) =  ZCTOTA(:,:,:,JP_AER_DST,1)*6.0221367E+11/ZMI(JP_AER_DST)
 PSVT(:,:,:,JP_CH_DSTj) =  ZCTOTA(:,:,:,JP_AER_DST,2)*6.0221367E+11/ZMI(JP_AER_DST)
!
 IF (NSOA .EQ. 10) THEN
  PSVT(:,:,:,JP_CH_SOA1i) =  ZCTOTA(:,:,:,JP_AER_SOA1,1)*6.0221367E+11/ZMI(JP_AER_SOA1)
  PSVT(:,:,:,JP_CH_SOA1j) =  ZCTOTA(:,:,:,JP_AER_SOA1,2)*6.0221367E+11/ZMI(JP_AER_SOA1)
  PSVT(:,:,:,JP_CH_SOA2i) =  ZCTOTA(:,:,:,JP_AER_SOA2,1)*6.0221367E+11/ZMI(JP_AER_SOA2)
  PSVT(:,:,:,JP_CH_SOA2j) =  ZCTOTA(:,:,:,JP_AER_SOA2,2)*6.0221367E+11/ZMI(JP_AER_SOA2)
  PSVT(:,:,:,JP_CH_SOA3i) =  ZCTOTA(:,:,:,JP_AER_SOA3,1)*6.0221367E+11/ZMI(JP_AER_SOA3)
  PSVT(:,:,:,JP_CH_SOA3j) =  ZCTOTA(:,:,:,JP_AER_SOA3,2)*6.0221367E+11/ZMI(JP_AER_SOA3)
  PSVT(:,:,:,JP_CH_SOA4i) =  ZCTOTA(:,:,:,JP_AER_SOA4,1)*6.0221367E+11/ZMI(JP_AER_SOA4)
  PSVT(:,:,:,JP_CH_SOA4j) =  ZCTOTA(:,:,:,JP_AER_SOA4,2)*6.0221367E+11/ZMI(JP_AER_SOA4)
  PSVT(:,:,:,JP_CH_SOA5i) =  ZCTOTA(:,:,:,JP_AER_SOA5,1)*6.0221367E+11/ZMI(JP_AER_SOA5)
  PSVT(:,:,:,JP_CH_SOA5j) =  ZCTOTA(:,:,:,JP_AER_SOA5,2)*6.0221367E+11/ZMI(JP_AER_SOA5)
  PSVT(:,:,:,JP_CH_SOA6i) =  ZCTOTA(:,:,:,JP_AER_SOA6,1)*6.0221367E+11/ZMI(JP_AER_SOA6)
  PSVT(:,:,:,JP_CH_SOA6j) =  ZCTOTA(:,:,:,JP_AER_SOA6,2)*6.0221367E+11/ZMI(JP_AER_SOA6)
  PSVT(:,:,:,JP_CH_SOA7i) =  ZCTOTA(:,:,:,JP_AER_SOA7,1)*6.0221367E+11/ZMI(JP_AER_SOA7)
  PSVT(:,:,:,JP_CH_SOA7j) =  ZCTOTA(:,:,:,JP_AER_SOA7,2)*6.0221367E+11/ZMI(JP_AER_SOA7)
  PSVT(:,:,:,JP_CH_SOA8i) =  ZCTOTA(:,:,:,JP_AER_SOA8,1)*6.0221367E+11/ZMI(JP_AER_SOA8)
  PSVT(:,:,:,JP_CH_SOA8j) =  ZCTOTA(:,:,:,JP_AER_SOA8,2)*6.0221367E+11/ZMI(JP_AER_SOA8)
  PSVT(:,:,:,JP_CH_SOA9i) =  ZCTOTA(:,:,:,JP_AER_SOA9,1)*6.0221367E+11/ZMI(JP_AER_SOA9)
  PSVT(:,:,:,JP_CH_SOA9j) =  ZCTOTA(:,:,:,JP_AER_SOA9,2)*6.0221367E+11/ZMI(JP_AER_SOA9)
  PSVT(:,:,:,JP_CH_SOA10i) =  ZCTOTA(:,:,:,JP_AER_SOA10,1)*6.0221367E+11/ZMI(JP_AER_SOA10)
  PSVT(:,:,:,JP_CH_SOA10j) =  ZCTOTA(:,:,:,JP_AER_SOA10,2)*6.0221367E+11/ZMI(JP_AER_SOA10)
  END IF

!
PSVT(:,:,:,JP_CH_M0i) = ZM(:,:,:,1) * 1E-6
PSVT(:,:,:,JP_CH_M0j) = ZM(:,:,:,4) * 1E-6

IF (LVARSIGI) PSVT(:,:,:,JP_CH_M6i) = ZM(:,:,:,3)
IF (LVARSIGJ) PSVT(:,:,:,JP_CH_M6j) = ZM(:,:,:,6)

DO JJ=1,SIZE(PSVT,4)
  PSVT(:,:,:,JJ) =  PSVT(:,:,:,JJ) / (ZDEN2MOL * PRHODREF(:,:,:))
ENDDO


END SUBROUTINE CON2MIX

!   ############################################################
  SUBROUTINE AERO2PPP(             &
       PSVT                         & !IO [ppv] input scalar variables (moment of distribution)
       , PRHODREF                   & !I [kg/m3] density of air       
       , PSIG3D                     & !I [-] standard deviation of aerosol distribution
       , PRG3D                      & !I [um] number median diameter of aerosol distribution
       , PMI                        & !O [g/mol] molecular weight
       )
!!   ############################################################
!
!!
!!    PURPOSE
!!    -------
!!    Translate the aerosol Mass, RG and SIGMA in the  three moments M0, M3 and M6 given in ppv 
!!
!!    REFERENCE
!!    ---------
!!    none
!!
!!    AUTHOR
!!    ------
!!    Pierre TULET (LA)
!!
!!    MODIFICATIONS
!!    -------------
!!    Alf Grini (CNRM)
!!
!!    EXTERNAL
!!    --------
!!
    IMPLICIT NONE
!!
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
!*      0.1    declarations of arguments
!
REAL,       DIMENSION(:,:,:,:),  INTENT(INOUT)     :: PSVT      !I [#/molec_{air}] first moment
                                                                !I [molec_{aer}/molec_{air} 3rd moment
                                                                !I [um6/molec_{air}*(cm3/m3)] 6th moment
REAL,       DIMENSION(:,:,:),  INTENT(IN)      :: PRHODREF !I [kg/m3] density of air

REAL,       DIMENSION(:,:,:,:),  INTENT(IN)     :: PSIG3D   !O [-] standard deviation
REAL,       DIMENSION(:,:,:,:),  INTENT(IN)     :: PRG3D    !O [um] number median diameter
REAL,       DIMENSION(:,:,:,:), OPTIONAL,  INTENT(IN) :: PMI !O molecular weight
!
!
!*      0.2    declarations local variables
!
REAL,DIMENSION(SIZE(PSVT,1),SIZE(PSVT,2),SIZE(PSVT,3),NSP+NCARB+NSOA)  :: ZMI                 ! [g/mol] molar weight of aerosol
REAL,DIMENSION(:,:,:,:), ALLOCATABLE :: ZM                  ! [aerosol units] local array which goes to output later
REAL,DIMENSION(JPMODE*3)             :: ZPMIN               ! [aerosol units] minimum values for N, sigma, M
REAL,DIMENSION(SIZE(PSVT,1),SIZE(PSVT,2),SIZE(PSVT,3),NSP+NCARB+NSOA,JPMODE):: ZCTOTA

REAL,DIMENSION(NSP+NCARB+NSOA)       :: ZFAC                ! M3 / mass conversion factor
REAL,DIMENSION(NSP+NCARB+NSOA)       :: ZRHOI                ! aerosol density
INTEGER                              :: JJ                  ! [idx] loop counters
REAL                                 :: ZDEN2MOL
REAL    :: ZINIRADIUSI, ZINIRADIUSJ
!
!-------------------------------------------------------------------------------
!
!        1.    initialisation 

!Calculations here are for one mode only
IF (CRGUNIT=="MASS") THEN
    ZINIRADIUSI = XINIRADIUSI * EXP(-3.*(LOG(XINISIGI))**2)
    ZINIRADIUSJ = XINIRADIUSJ * EXP(-3.*(LOG(XINISIGJ))**2)
ELSE
    ZINIRADIUSI = XINIRADIUSI 
    ZINIRADIUSJ = XINIRADIUSJ
END IF


!Get minimum values possible
ZPMIN(1) = XN0IMIN
ZPMIN(2) = ZPMIN(1) * (ZINIRADIUSI**3)*EXP(4.5 * LOG(XINISIGI)**2) 
ZPMIN(3) = ZPMIN(1) * (ZINIRADIUSI**6)*EXP(18. * LOG(XINISIGI)**2)
ZPMIN(4) = XN0JMIN
ZPMIN(5) = ZPMIN(4) * (ZINIRADIUSJ**3)*EXP(4.5 * LOG(XINISIGJ)**2) 
ZPMIN(6) = ZPMIN(4) * (ZINIRADIUSJ**6)*EXP(18. * LOG(XINISIGJ)**2)

ALLOCATE (ZM(SIZE(PSVT,1), SIZE(PSVT,2), SIZE(PSVT,3), JPMODE*3))
    
!Set molecular weightn g/mol 
IF(PRESENT(PMI)) THEN
  ZMI(:,:,:,:) = PMI(:,:,:,:)
ELSE
    ZMI(:,:,:,:) = 250.
    ZMI(:,:,:,JP_AER_SO4)  = 98.
    ZMI(:,:,:,JP_AER_NO3)  = 63.
    ZMI(:,:,:,JP_AER_NH3)  = 17.
    ZMI(:,:,:,JP_AER_H2O)  = 18.
    ZMI(:,:,:,JP_AER_BC)   = 12.
    ZMI(:,:,:,JP_AER_DST)  = 100.
    IF (NSOA .EQ. 10) THEN
    ZMI(:,:,:,JP_AER_SOA1) = 88. 
    ZMI(:,:,:,JP_AER_SOA2) = 180.
    ZMI(:,:,:,JP_AER_SOA3) = 1.5374857E2
    ZMI(:,:,:,JP_AER_SOA4) = 1.9586780E2
    ZMI(:,:,:,JP_AER_SOA5) = 195.
    ZMI(:,:,:,JP_AER_SOA6) = 195.
    ZMI(:,:,:,JP_AER_SOA7) = 165.
    ZMI(:,:,:,JP_AER_SOA8) = 195.
    ZMI(:,:,:,JP_AER_SOA9) = 270.
    ZMI(:,:,:,JP_AER_SOA10) = 210.
    END IF
ENDIF

! Cf Ackermann (all to black carbon except water)
!Set molecular weightn g/mol 
ZRHOI(:) = 1.8e3
ZRHOI(JP_AER_H2O) = 1.0e3   ! water
ZRHOI(JP_AER_DST) = XDENSITY_DUST  ! dusts

! conversion into mol.cm-3
ZDEN2MOL = 1E-6 * XAVOGADRO  / XMD
DO JJ=1, SIZE(PSVT, 4)
  PSVT(:,:,:,JJ) =  PSVT(:,:,:,JJ) * ZDEN2MOL * PRHODREF(:,:,:)
ENDDO
!
DO JJ=1,NSP+NCARB+NSOA
  ZFAC(JJ)=(4./3.)*XPI*ZRHOI(JJ)*1.e-9
ENDDO
!
!
!*       2    transfer aerosol mass from gas to aerosol variables
!               (and conversion of mol.cm-3 --> microgram/m3)
!
ZCTOTA(:,:,:,:,:) = 0.
! aerosol phase
  ZCTOTA(:,:,:,JP_AER_SO4,1) = PSVT(:,:,:,JP_CH_SO4i)*ZMI(:,:,:,JP_AER_SO4)/6.0221367E+11
  ZCTOTA(:,:,:,JP_AER_SO4,2) = PSVT(:,:,:,JP_CH_SO4j)*ZMI(:,:,:,JP_AER_SO4)/6.0221367E+11
  ZCTOTA(:,:,:,JP_AER_NO3,1) = PSVT(:,:,:,JP_CH_NO3i)*ZMI(:,:,:,JP_AER_NO3)/6.0221367E+11

  ZCTOTA(:,:,:,JP_AER_NO3,2) = PSVT(:,:,:,JP_CH_NO3j)*ZMI(:,:,:,JP_AER_NO3)/6.0221367E+11
  ZCTOTA(:,:,:,JP_AER_NH3,1) = PSVT(:,:,:,JP_CH_NH3i)*ZMI(:,:,:,JP_AER_NH3)/6.0221367E+11
  ZCTOTA(:,:,:,JP_AER_NH3,2) = PSVT(:,:,:,JP_CH_NH3j)*ZMI(:,:,:,JP_AER_NH3)/6.0221367E+11
!
! water
  ZCTOTA(:,:,:,JP_AER_H2O,1) = PSVT(:,:,:,JP_CH_H2Oi)*ZMI(:,:,:,JP_AER_H2O)/6.0221367E+11
  ZCTOTA(:,:,:,JP_AER_H2O,2) = PSVT(:,:,:,JP_CH_H2Oj)*ZMI(:,:,:,JP_AER_H2O)/6.0221367E+11
!
! primary organic carbon
  ZCTOTA(:,:,:,JP_AER_OC,1) = PSVT(:,:,:,JP_CH_OCi)*ZMI(:,:,:,JP_AER_OC)/6.0221367E+11
  ZCTOTA(:,:,:,JP_AER_OC,2) = PSVT(:,:,:,JP_CH_OCj)*ZMI(:,:,:,JP_AER_OC)/6.0221367E+11
!
! primary black carbon
  ZCTOTA(:,:,:,JP_AER_BC,1) = PSVT(:,:,:,JP_CH_BCi)*ZMI(:,:,:,JP_AER_BC)/6.0221367E+11
  ZCTOTA(:,:,:,JP_AER_BC,2) = PSVT(:,:,:,JP_CH_BCj)*ZMI(:,:,:,JP_AER_BC)/6.0221367E+11
!dust
  ZCTOTA(:,:,:,JP_AER_DST,1) = PSVT(:,:,:,JP_CH_DSTi)*ZMI(:,:,:,JP_AER_DST)/6.0221367E+11
  ZCTOTA(:,:,:,JP_AER_DST,2) = PSVT(:,:,:,JP_CH_DSTj)*ZMI(:,:,:,JP_AER_DST)/6.0221367E+11
!
 IF (NSOA .EQ. 10) THEN
  ZCTOTA(:,:,:,JP_AER_SOA1,1) = PSVT(:,:,:,JP_CH_SOA1i)*ZMI(:,:,:,JP_AER_SOA1)/6.0221367E+11
  ZCTOTA(:,:,:,JP_AER_SOA1,2) = PSVT(:,:,:,JP_CH_SOA1j)*ZMI(:,:,:,JP_AER_SOA1)/6.0221367E+11
  ZCTOTA(:,:,:,JP_AER_SOA2,1) = PSVT(:,:,:,JP_CH_SOA2i)*ZMI(:,:,:,JP_AER_SOA2)/6.0221367E+11
  ZCTOTA(:,:,:,JP_AER_SOA2,2) = PSVT(:,:,:,JP_CH_SOA2j)*ZMI(:,:,:,JP_AER_SOA2)/6.0221367E+11
  ZCTOTA(:,:,:,JP_AER_SOA3,1) = PSVT(:,:,:,JP_CH_SOA3i)*ZMI(:,:,:,JP_AER_SOA3)/6.0221367E+11
  ZCTOTA(:,:,:,JP_AER_SOA3,2) = PSVT(:,:,:,JP_CH_SOA3j)*ZMI(:,:,:,JP_AER_SOA3)/6.0221367E+11
  ZCTOTA(:,:,:,JP_AER_SOA4,1) = PSVT(:,:,:,JP_CH_SOA4i)*ZMI(:,:,:,JP_AER_SOA4)/6.0221367E+11
  ZCTOTA(:,:,:,JP_AER_SOA4,2) = PSVT(:,:,:,JP_CH_SOA4j)*ZMI(:,:,:,JP_AER_SOA4)/6.0221367E+11
  ZCTOTA(:,:,:,JP_AER_SOA5,1) = PSVT(:,:,:,JP_CH_SOA5i)*ZMI(:,:,:,JP_AER_SOA5)/6.0221367E+11
  ZCTOTA(:,:,:,JP_AER_SOA5,2) = PSVT(:,:,:,JP_CH_SOA5j)*ZMI(:,:,:,JP_AER_SOA5)/6.0221367E+11

  ZCTOTA(:,:,:,JP_AER_SOA6,1) = PSVT(:,:,:,JP_CH_SOA6i)*ZMI(:,:,:,JP_AER_SOA6)/6.0221367E+11
  ZCTOTA(:,:,:,JP_AER_SOA6,2) = PSVT(:,:,:,JP_CH_SOA6j)*ZMI(:,:,:,JP_AER_SOA6)/6.0221367E+11
  ZCTOTA(:,:,:,JP_AER_SOA7,1) = PSVT(:,:,:,JP_CH_SOA7i)*ZMI(:,:,:,JP_AER_SOA7)/6.0221367E+11
  ZCTOTA(:,:,:,JP_AER_SOA7,2) = PSVT(:,:,:,JP_CH_SOA7j)*ZMI(:,:,:,JP_AER_SOA7)/6.0221367E+11
  ZCTOTA(:,:,:,JP_AER_SOA8,1) = PSVT(:,:,:,JP_CH_SOA8i)*ZMI(:,:,:,JP_AER_SOA8)/6.0221367E+11
  ZCTOTA(:,:,:,JP_AER_SOA8,2) = PSVT(:,:,:,JP_CH_SOA8j)*ZMI(:,:,:,JP_AER_SOA8)/6.0221367E+11
  ZCTOTA(:,:,:,JP_AER_SOA9,1) = PSVT(:,:,:,JP_CH_SOA9i)*ZMI(:,:,:,JP_AER_SOA9)/6.0221367E+11
  ZCTOTA(:,:,:,JP_AER_SOA9,2) = PSVT(:,:,:,JP_CH_SOA9j)*ZMI(:,:,:,JP_AER_SOA9)/6.0221367E+11
  ZCTOTA(:,:,:,JP_AER_SOA10,1) = PSVT(:,:,:,JP_CH_SOA10i)*ZMI(:,:,:,JP_AER_SOA10)/6.0221367E+11
  ZCTOTA(:,:,:,JP_AER_SOA10,2) = PSVT(:,:,:,JP_CH_SOA10j)*ZMI(:,:,:,JP_AER_SOA10)/6.0221367E+11
  END IF

!
!*       3    calculate moment 3 from total aerosol mass
!
  ZM(:,:,:,2) = 0.
  ZM(:,:,:,5) = 0.
  DO JJ = 1,NSP+NCARB+NSOA
    ZM(:,:,:,2) = ZM(:,:,:,2)+ZCTOTA(:,:,:,JJ,1)/ZFAC(JJ)
    ZM(:,:,:,5) = ZM(:,:,:,5)+ZCTOTA(:,:,:,JJ,2)/ZFAC(JJ)
  ENDDO
!  ZM(:,:,:,2) = MAX(ZM(:,:,:,2), ZPMIN(2))
!  ZM(:,:,:,5) = MAX(ZM(:,:,:,5), ZPMIN(5))
!
!
!*       4    calculate moment 0 from dispersion and mean radius
!
   ZM(:,:,:,1)= ZM(:,:,:,2)/ &
              ( (PRG3D(:,:,:,1)**3)*EXP(4.5 * LOG(PSIG3D(:,:,:,1))**2) )
   ZM(:,:,:,4)= ZM(:,:,:,5)/ &
              ( (PRG3D(:,:,:,2)**3)*EXP(4.5 * LOG(PSIG3D(:,:,:,2))**2) )
!

!*       5    calculate moment 6 from dispersion and mean radius
!
 ZM(:,:,:,3) = ZM(:,:,:,1)*(PRG3D(:,:,:,1)**6) * &
               EXP(18 *(LOG(PSIG3D(:,:,:,1)))**2)
 ZM(:,:,:,6) = ZM(:,:,:,4)*(PRG3D(:,:,:,2)**6) * &
               EXP(18 *(LOG(PSIG3D(:,:,:,2)))**2)

!*       6    return to ppv
!
PSVT(:,:,:,JP_CH_M0i) = ZM(:,:,:,1) * 1E-6 
PSVT(:,:,:,JP_CH_M0j) = ZM(:,:,:,4) * 1E-6

IF (LVARSIGI) PSVT(:,:,:,JP_CH_M6i) = ZM(:,:,:,3) 
IF (LVARSIGJ) PSVT(:,:,:,JP_CH_M6j) = ZM(:,:,:,6)

DO JJ=1,SIZE(PSVT,4)
  PSVT(:,:,:,JJ) =  PSVT(:,:,:,JJ) / (ZDEN2MOL * PRHODREF(:,:,:))
ENDDO

DEALLOCATE(ZM)
!
END SUBROUTINE AERO2PPP
!
!    ############################################################
  SUBROUTINE PPP2AERO1D(            &
       PSVT                         & !I [ppv] input scalar variables (moment of distribution)
       , PRHODREF                   & !I [kg/m3] density of air       
       , PMI                        & !O molecular weight
       , PSIG1D                     & !O [-] standard deviation of aerosol distribution
       , PRG1D                      & !O [um] number median diameter of aerosol distribution
       , PN1D                       & !O [#/m3] number concentration of aerosols
       , PCTOTA                     & !O [ug/m3] mass of each aerosol compounds
       , PM1D                       & !moments 0, 3 and 6
       )
!    ############################################################
!
!!
!!    PURPOSE
!!    -------
!!    Translate the three moments M0, M3 and M6 given in ppv into
!!    Values which can be understood more easily (R, sigma, N, M)
!! 
!!    CALLING STRUCTURE NOTE: OPTIONAL VARIABLES
!!    -------
!!    CALL PPP2AERO1D(PSVT, PRHODREF, PSIG1D=SIGVAR,  &
!!       PRG1D=RVAR, PN1D=NVAR, PM1D=ZM)
!!
!!    REFERENCE
!!    ---------
!!    none
!!
!!    AUTHOR
!!    ------
!!    Pierre TULET (LA)
!!
!!    MODIFICATIONS
!!    -------------
!!    Alf Grini (CNRM)
!!
!!    EXTERNAL
!!    --------
!!
    IMPLICIT NONE
!!
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
!*      0.1    declarations of arguments
!
REAL,    DIMENSION(:,:),  INTENT(IN)     :: PSVT     !I [#/molec_{air}] first moment
                                                             !I [molec_{aer}/molec_{air} 3rd moment
                                                             !I [um6/molec_{air}*(cm3/m3)] 6th moment
REAL,    DIMENSION(:),    INTENT(IN)     :: PRHODREF !I [kg/m3] density of air

REAL,    DIMENSION(:,:),  OPTIONAL, INTENT(IN)    :: PMI ! molecular weight g/mol
REAL,    DIMENSION(:,:),  OPTIONAL, INTENT(OUT)   :: PSIG1D   !O [-] standard deviation
REAL,    DIMENSION(:,:),  OPTIONAL, INTENT(OUT)   :: PRG1D    !O [um] number median diameter
REAL,    DIMENSION(:,:),  OPTIONAL, INTENT(OUT)   :: PN1D     !O [#/m3] number concentration
REAL,    DIMENSION(:,:,:),OPTIONAL, INTENT(OUT)   :: PCTOTA   !O [ug/m3] mass of each component
REAL,    DIMENSION(:,:),  OPTIONAL, INTENT(OUT)   :: PM1D     !O moments 0,3 and 6 
!
!*      0.2    declarations local variables
!
REAL, DIMENSION(SIZE(PSVT,1),NSP+NCARB+NSOA)      :: ZMI      ! [kg/mol] molar weight of aerosol
REAL,DIMENSION(SIZE(PSVT,1), SIZE(PSVT,2)) :: ZSV             ! [aerosol concentration]
REAL,DIMENSION(SIZE(PSVT,1)) :: ZSIGMA                        ! [-] standard deviation
REAL,DIMENSION(SIZE(PSVT,1),JPMODE) :: ZMASK  
REAL,DIMENSION(SIZE(PSVT,1),NSP+NCARB+NSOA,JPMODE):: ZCTOTA
REAL,DIMENSION(SIZE(PSVT,1),JPMODE*3) :: ZM

REAL,DIMENSION(NSP+NCARB+NSOA)       :: ZFAC                  ! M3 / mass conversion factor
REAL,DIMENSION(NSP+NCARB+NSOA)       :: ZRHOI                ! aerosol density
REAL                                 :: ZDEN2MOL
REAL,DIMENSION(JPMODE*3)             :: ZPMIN                 ! [aerosol units] minimum values for N, sigma, M
REAL,DIMENSION(JPMODE)               :: ZRATIOBC, ZRATIOOC 
INTEGER                              :: JJ, JN                ! [idx] loop counters
REAL    :: ZINIRADIUSI, ZINIRADIUSJ
!
!-------------------------------------------------------------------------------
!
!        1.    initialisation 

IF (CRGUNIT=="MASS") THEN
  ZINIRADIUSI = XINIRADIUSI * EXP(-3.*(LOG(XINISIGI))**2)
  ZINIRADIUSJ = XINIRADIUSJ * EXP(-3.*(LOG(XINISIGJ))**2)
ELSE
  ZINIRADIUSI = XINIRADIUSI 
  ZINIRADIUSJ = XINIRADIUSJ
END IF



    !Get minimum values possible
    ZPMIN(1) = XN0IMIN
    ZPMIN(2) = ZPMIN(1) * (ZINIRADIUSI**3)*EXP(4.5 * LOG(XINISIGI)**2) 
    ZPMIN(3) = ZPMIN(1) * (ZINIRADIUSI**6)*EXP(18. * LOG(XINISIGI)**2)

    ZPMIN(4) = XN0JMIN
    ZPMIN(5) = ZPMIN(4) * (ZINIRADIUSJ**3)*EXP(4.5 * LOG(XINISIGJ)**2) 
    ZPMIN(6) = ZPMIN(4) * (ZINIRADIUSJ**6)*EXP(18. * LOG(XINISIGJ)**2)

!Set molecular weightn g/mol 
IF(PRESENT(PMI)) THEN
  ZMI(:,:) = PMI(:,:)
ELSE
    ZMI(:,:) = 250.
    ZMI(:,JP_AER_SO4)  = 98.
    ZMI(:,JP_AER_NO3)  = 63.
    ZMI(:,JP_AER_NH3)  = 17.
    ZMI(:,JP_AER_H2O)  = 18.
    ZMI(:,JP_AER_BC)   = 12.
    IF (NSOA .EQ. 10) THEN
    ZMI(:,JP_AER_SOA1) = 88. 
    ZMI(:,JP_AER_SOA2) = 180.
    ZMI(:,JP_AER_SOA3) = 1.5374857E2
    ZMI(:,JP_AER_SOA4) = 1.9586780E2
    ZMI(:,JP_AER_SOA5) = 195.
    ZMI(:,JP_AER_SOA6) = 195.
    ZMI(:,JP_AER_SOA7) = 165.
    ZMI(:,JP_AER_SOA8) = 195.
    ZMI(:,JP_AER_SOA9) = 270.
    ZMI(:,JP_AER_SOA10) = 210.
    END IF
ENDIF

! Cf Ackermann (all to black carbon except water)
!Set molecular weightn g/mol 
ZRHOI(:) = 1.8e3
ZRHOI(JP_AER_H2O) = 1.0e3   ! water
ZRHOI(JP_AER_DST) = XDENSITY_DUST  ! dusts

! conversion into mol.cm-3
ZDEN2MOL = 1E-6 * XAVOGADRO  / XMD
!
DO JJ=1, SIZE(PSVT,2)
  ZSV(:,JJ) =  PSVT(:,JJ) * ZDEN2MOL * PRHODREF(:)
ENDDO
!
DO JJ=1,NSP+NCARB+NSOA
  ZFAC(JJ)=(4./3.)*XPI*ZRHOI(JJ)*1.e-9
ENDDO
!
!-------------------------------------------------------------------------------
!
!*       2    transfer aerosol mass from gas to aerosol variables
!               (and conversion of mol.cm-3 --> microgram/m3)
!
ZCTOTA(:,:,:) = 0.
! aerosol phase
  ZCTOTA(:,JP_AER_SO4,1) = ZSV(:,JP_CH_SO4i)*ZMI(:,JP_AER_SO4)/6.0221367E+11
  ZCTOTA(:,JP_AER_SO4,2) = ZSV(:,JP_CH_SO4j)*ZMI(:,JP_AER_SO4)/6.0221367E+11

  ZCTOTA(:,JP_AER_NO3,1) = ZSV(:,JP_CH_NO3i)*ZMI(:,JP_AER_NO3)/6.0221367E+11
  ZCTOTA(:,JP_AER_NO3,2) = ZSV(:,JP_CH_NO3j)*ZMI(:,JP_AER_NO3)/6.0221367E+11

  ZCTOTA(:,JP_AER_NH3,1) = ZSV(:,JP_CH_NH3i)*ZMI(:,JP_AER_NH3)/6.0221367E+11
  ZCTOTA(:,JP_AER_NH3,2) = ZSV(:,JP_CH_NH3j)*ZMI(:,JP_AER_NH3)/6.0221367E+11
!
! water
  ZCTOTA(:,JP_AER_H2O,1) = ZSV(:,JP_CH_H2Oi)*ZMI(:,JP_AER_H2O)/6.0221367E+11
  ZCTOTA(:,JP_AER_H2O,2) = ZSV(:,JP_CH_H2Oj)*ZMI(:,JP_AER_H2O)/6.0221367E+11
!
! primary organic carbon
  ZCTOTA(:,JP_AER_OC,1) = ZSV(:,JP_CH_OCi)*ZMI(:,JP_AER_OC)/6.0221367E+11
  ZCTOTA(:,JP_AER_OC,2) = ZSV(:,JP_CH_OCj)*ZMI(:,JP_AER_OC)/6.0221367E+11
!
! primary black carbon
  ZCTOTA(:,JP_AER_BC,1) = ZSV(:,JP_CH_BCi)*ZMI(:,JP_AER_BC)/6.0221367E+11
  ZCTOTA(:,JP_AER_BC,2) = ZSV(:,JP_CH_BCj)*ZMI(:,JP_AER_BC)/6.0221367E+11
!
IF (NSOA .EQ. 10) THEN
  ZCTOTA(:,JP_AER_SOA1,1) = ZSV(:,JP_CH_SOA1i)*ZMI(:,JP_AER_SOA1)/6.0221367E+11
  ZCTOTA(:,JP_AER_SOA1,2) = ZSV(:,JP_CH_SOA1j)*ZMI(:,JP_AER_SOA1)/6.0221367E+11
  ZCTOTA(:,JP_AER_SOA2,1) = ZSV(:,JP_CH_SOA2i)*ZMI(:,JP_AER_SOA2)/6.0221367E+11
  ZCTOTA(:,JP_AER_SOA2,2) = ZSV(:,JP_CH_SOA2j)*ZMI(:,JP_AER_SOA2)/6.0221367E+11
  ZCTOTA(:,JP_AER_SOA3,1) = ZSV(:,JP_CH_SOA3i)*ZMI(:,JP_AER_SOA3)/6.0221367E+11
  ZCTOTA(:,JP_AER_SOA3,2) = ZSV(:,JP_CH_SOA3j)*ZMI(:,JP_AER_SOA3)/6.0221367E+11
  ZCTOTA(:,JP_AER_SOA4,1) = ZSV(:,JP_CH_SOA4i)*ZMI(:,JP_AER_SOA4)/6.0221367E+11
  ZCTOTA(:,JP_AER_SOA4,2) = ZSV(:,JP_CH_SOA4j)*ZMI(:,JP_AER_SOA4)/6.0221367E+11
  ZCTOTA(:,JP_AER_SOA5,1) = ZSV(:,JP_CH_SOA5i)*ZMI(:,JP_AER_SOA5)/6.0221367E+11
  ZCTOTA(:,JP_AER_SOA5,2) = ZSV(:,JP_CH_SOA5j)*ZMI(:,JP_AER_SOA5)/6.0221367E+11

  ZCTOTA(:,JP_AER_SOA6,1) = ZSV(:,JP_CH_SOA6i)*ZMI(:,JP_AER_SOA6)/6.0221367E+11
  ZCTOTA(:,JP_AER_SOA6,2) = ZSV(:,JP_CH_SOA6j)*ZMI(:,JP_AER_SOA6)/6.0221367E+11
  ZCTOTA(:,JP_AER_SOA7,1) = ZSV(:,JP_CH_SOA7i)*ZMI(:,JP_AER_SOA7)/6.0221367E+11
  ZCTOTA(:,JP_AER_SOA7,2) = ZSV(:,JP_CH_SOA7j)*ZMI(:,JP_AER_SOA7)/6.0221367E+11
  ZCTOTA(:,JP_AER_SOA8,1) = ZSV(:,JP_CH_SOA8i)*ZMI(:,JP_AER_SOA8)/6.0221367E+11
  ZCTOTA(:,JP_AER_SOA8,2) = ZSV(:,JP_CH_SOA8j)*ZMI(:,JP_AER_SOA8)/6.0221367E+11
  ZCTOTA(:,JP_AER_SOA9,1) = ZSV(:,JP_CH_SOA9i)*ZMI(:,JP_AER_SOA9)/6.0221367E+11
  ZCTOTA(:,JP_AER_SOA9,2) = ZSV(:,JP_CH_SOA9j)*ZMI(:,JP_AER_SOA9)/6.0221367E+11
  ZCTOTA(:,JP_AER_SOA10,1) = ZSV(:,JP_CH_SOA10i)*ZMI(:,JP_AER_SOA10)/6.0221367E+11
  ZCTOTA(:,JP_AER_SOA10,2) = ZSV(:,JP_CH_SOA10j)*ZMI(:,JP_AER_SOA10)/6.0221367E+11
END IF
!
!-------------------------------------------------------------------------------
!
!*       3    calculate moment 3 from total aerosol mass
!
ZM(:,2) = 0.
ZM(:,5) = 0.
DO JJ = 1,NSP+NCARB+NSOA
    ZM(:,2) = ZM(:,2)+ZCTOTA(:,JJ,1)/ZFAC(JJ) !==>um3_{aer}/m3_{air} (volume ==> 3rd moment)
    ZM(:,5) = ZM(:,5)+ZCTOTA(:,JJ,2)/ZFAC(JJ) !==>um3_{aer}/m3_{air} (volume ==> 3rd moment)
ENDDO
!
!-------------------------------------------------------------------------------
!
!*       4    set  moment 0 
!
   ZM(:,1)=   MAX(ZSV(:,JP_CH_M0i) * 1E+6, XMNH_TINY) ! molec_{aer}/m3_{air}
   ZM(:,4)=   MAX(ZSV(:,JP_CH_M0j) * 1E+6, XMNH_TINY) ! molec_{aer}/m3_{air}
!
!-------------------------------------------------------------------------------
!
!*       5    set moment 6  ==> um6_{aer}/m3_{air}
!
IF (LVARSIGI) THEN ! set M6 variable standard deviation
  ZM(:,3) = MAX(ZSV(:,JP_CH_M6i), XMNH_TINY)

  ZSIGMA(:)=ZM(:,2)**2/(ZM(:,1)*ZM(:,3))
  ZSIGMA(:)=MIN(1-1E-10,ZSIGMA(:))
  ZSIGMA(:)=MAX(1E-10,ZSIGMA(:))
  ZSIGMA(:)= LOG(ZSIGMA(:))
  ZSIGMA(:)= EXP(1./3.*SQRT(-ZSIGMA(:)))
  WHERE (ZSIGMA(:) > XSIGIMAX)
   ZSIGMA(:) =  XSIGIMAX
  END WHERE
  WHERE (ZSIGMA(:) < XSIGIMIN)
   ZSIGMA(:) =  XSIGIMIN
  END WHERE
  ZM(:,3) = ZM(:,1) &
          * ( (ZM(:,2)/ZM(:,1))**(1./3.)  &
          * exp(-(3./2.)*log(ZSIGMA(:))**2))**6 &
          * exp(18.*log(ZSIGMA(:))**2)

 IF(PRESENT(PSIG1D)) PSIG1D(:,1) = ZSIGMA(:)

ELSE ! fixed standard deviation
 ZM(:,3) = ZM(:,1) &
          * ( (ZM(:,2)/ZM(:,1))**(1./3.)  &
          * exp(-(3./2.)*log(XINISIGI)**2))**6 &
          * exp(18.*log(XINISIGI)**2)

 IF(PRESENT(PSIG1D)) PSIG1D(:,1) = XINISIGI
END IF

IF (LVARSIGJ) THEN ! set M6 variable standard deviation
  ZM(:,6) = MAX(ZSV(:,JP_CH_M6j), XMNH_TINY)

  ZSIGMA(:)=ZM(:,5)**2/(ZM(:,4)*ZM(:,6))
  ZSIGMA(:)=MIN(1-1E-10,ZSIGMA(:))
  ZSIGMA(:)=MAX(1E-10,ZSIGMA(:))
  ZSIGMA(:)= LOG(ZSIGMA(:))
  ZSIGMA(:)= EXP(1./3.*SQRT(-ZSIGMA(:)))
  WHERE (ZSIGMA(:) > XSIGJMAX)
    ZSIGMA(:) =  XSIGJMAX
  END WHERE
  WHERE (ZSIGMA(:) < XSIGJMIN)
    ZSIGMA(:) =  XSIGJMIN
  END WHERE

  ZM(:,6) = ZM(:,4) &
          * ( (ZM(:,5)/ZM(:,4))**(1./3.)  &
          * exp(-(3./2.)*log(ZSIGMA(:))**2))**6 &
          * exp(18.*log(ZSIGMA(:))**2)

  IF(PRESENT(PSIG1D)) PSIG1D(:,2) = ZSIGMA(:)

ELSE ! fixed standard deviation
  ZM(:,6) = ZM(:,4) &
          * ( (ZM(:,5)/ZM(:,4))**(1./3.)  &
          * exp(-(3./2.)*log(XINISIGJ)**2))**6 &
          * exp(18.*log(XINISIGJ)**2)

  IF(PRESENT(PSIG1D)) PSIG1D(:,2) = XINISIGJ
END IF


!-------------------------------------------------------------------------------
!
!*       6    calculate modal parameters from moments
!
DO JN=1,JPMODE
!*************************************************************
! Blindages pour valeurs inferieurs au mininmum accepte
!*************************************************************
  ZMASK(:,JN) = 1.
  WHERE ((ZM(:,NM0(JN)) .LT. ZPMIN(NM0(JN))).OR.&
         (ZM(:,NM3(JN)) .LT. ZPMIN(NM3(JN))).OR.&
         (ZM(:,NM6(JN)) .LT. ZPMIN(NM6(JN))))

    ZM(:,NM0(JN)) = ZPMIN(NM0(JN))
    ZM(:,NM3(JN)) = ZPMIN(NM3(JN))
    ZM(:,NM6(JN)) = ZPMIN(NM6(JN))

    ZMASK(:,JN)  = 0.
  END WHERE
  DO JJ=1,NSP+NCARB+NSOA
    ZCTOTA(:,JJ,JN) = ZCTOTA(:,JJ,JN) * ZMASK(:,JN)
  ENDDO
  WHERE (ZMASK(:,JN) == 0.)
    ZCTOTA(:,JP_AER_BC,JN) = 0.5 * ZPMIN(NM3(JN)) * ZFAC(JP_AER_BC)
    ZCTOTA(:,JP_AER_OC,JN) = 0.5 * ZPMIN(NM3(JN)) * ZFAC(JP_AER_OC)
  END WHERE
  !
  IF(PRESENT(PN1D)) PN1D(:,JN) = ZM(:,NM0(JN))

  IF(PRESENT(PRG1D)) PRG1D(:,JN)=(ZM(:,NM3(JN))**4. &
              / (ZM(:,NM6(JN))*ZM(:,NM0(JN))**3.))**(1./6.)

ENDDO
!
IF(PRESENT(PCTOTA)) PCTOTA(:,:,:) = ZCTOTA(:,:,:)
IF(PRESENT(PM1D)) PM1D(:,:) = ZM(:,:)
!
!
END SUBROUTINE PPP2AERO1D
!
!
END MODULE MODE_AERO_PSD
