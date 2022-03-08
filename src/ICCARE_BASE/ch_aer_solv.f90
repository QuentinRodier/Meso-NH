!ORILAM_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!ORILAM_LIC This is part of the ORILAM software governed by the CeCILL-C licence
!ORILAM_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!ORILAM_LIC for details.
!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source: /home/cvsroot/MNH-VX-Y-Z/src/MNH/ch_aer_solv.f90,v $ $Revision: 1.1.2.1.2.1.16.2.2.1.2.1 $ $Date: 2015/12/01 15:26:23 $
!-----------------------------------------------------------------
!-----------------------------------------------------------------
!!   #######################
     MODULE MODI_CH_AER_SOLV
!!   #######################
!!
INTERFACE
!!
SUBROUTINE CH_AER_SOLV(PM, PLNSIG, PRG, PN,PCTOTG, PCTOTA, PCCTOT,              &
                       PDMINTRA,PDMINTER,PDMCOND, PDMNUCL, PDMMERG, PSEDA,PDT,  &
                       PRV, PDENAIR, PPRESSURE, PTEMP, PRC, PTIME,PSOLORG,      &
                       PMBEG,PMINT,PMEND)
IMPLICIT NONE
REAL, DIMENSION(:,:),   INTENT(INOUT) :: PM
REAL, DIMENSION(:,:),   INTENT(INOUT) :: PSEDA
REAL, DIMENSION(:,:),   INTENT(INOUT) :: PDMINTRA
REAL, DIMENSION(:,:),   INTENT(INOUT) :: PDMINTER
REAL, DIMENSION(:,:),   INTENT(INOUT) :: PDMCOND
REAL, DIMENSION(:,:),   INTENT(INOUT) :: PDMNUCL
REAL, DIMENSION(:,:),   INTENT(INOUT) :: PDMMERG
REAL, DIMENSION(:,:),   INTENT(INOUT) :: PLNSIG, PRG, PN
REAL, DIMENSION(:,:),   INTENT(INOUT) :: PCTOTG
REAL, DIMENSION(:,:,:), INTENT(INOUT) :: PCTOTA, PCCTOT
REAL, DIMENSION(:,:),   INTENT(INOUT) :: PSOLORG
REAL, INTENT(IN)                      :: PDT, PTIME
REAL, DIMENSION(:),     INTENT(IN)    :: PRV, PDENAIR, PPRESSURE, PTEMP, PRC
REAL, DIMENSION(:,:),   INTENT(INOUT) :: PMBEG,PMINT,PMEND
END SUBROUTINE CH_AER_SOLV
!!
END INTERFACE
!!
END MODULE MODI_CH_AER_SOLV
!!
!!   ##############################################################################
     SUBROUTINE CH_AER_SOLV(PM, PLNSIG, PRG, PN,PCTOTG, PCTOTA, PCCTOT,           &
                            PDMINTRA,PDMINTER,PDMCOND,PDMNUCL,PDMMERG,PSEDA, PDT, &
                            PRV, PDENAIR, PPRESSURE, PTEMP, PRC, PTIME,PSOLORG,   &
                            PMBEG,PMINT,PMEND)
!!   ##############################################################################
!!
!!   PURPOSE
!!   -------
!!   Time variable solver of the modal aerosol equations
!!
!!    REFERENCE
!!    ---------
!!    none
!!
!!    AUTHOR
!!    ------
!!    Vincent Crassier (LA)
!!
!!    MODIFICATIONS
!!    -------------
!!    P. Tulet for nesting
!!    P. Tulet organic condensation
!!    P. Tulet thermodynamic equilibrium for each mode
!!    P. Tulet add third mode
!!    M. Leriche 2015 correction bug
!!    M. Leriche 08/16 suppress moments index declaration already in modd_aerosol
!!    M. Leriche 08/16 add an other particular case for the M0 resolution to
!!               avoid a division by zero (when ZK = 1)
!!    J. Pianezze : 10/2018 add comments and simplification
!!
!!    EXTERNAL
!!    --------
!!
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_CH_AEROSOL
USE MODD_CST,            ONLY : XMNH_TINY
USE MODD_CONF,           ONLY : NVERB
USE MODI_CH_AER_MINERAL
USE MODI_CH_AER_ORGANIC
USE MODI_CH_AER_MPMPO
!
IMPLICIT NONE
!
!*       0.1   declarations of arguments
!
REAL, DIMENSION(:,:),   INTENT(INOUT) :: PM
REAL, DIMENSION(:,:),   INTENT(INOUT) :: PSEDA
REAL, DIMENSION(:,:),   INTENT(INOUT) :: PDMINTRA
REAL, DIMENSION(:,:),   INTENT(INOUT) :: PDMINTER
REAL, DIMENSION(:,:),   INTENT(INOUT) :: PDMCOND
REAL, DIMENSION(:,:),   INTENT(INOUT) :: PDMNUCL
REAL, DIMENSION(:,:),   INTENT(INOUT) :: PDMMERG
REAL, DIMENSION(:,:),   INTENT(INOUT) :: PLNSIG, PRG, PN
REAL, DIMENSION(:,:),   INTENT(INOUT) :: PCTOTG
REAL, DIMENSION(:,:,:), INTENT(INOUT) :: PCTOTA, PCCTOT
REAL, INTENT(IN)                      :: PDT, PTIME
REAL, DIMENSION(:),     INTENT(IN)    :: PRV, PDENAIR, PPRESSURE, PTEMP, PRC
REAL, DIMENSION(:,:),   INTENT(INOUT) :: PSOLORG
REAL, DIMENSION(:,:),   INTENT(INOUT) :: PMBEG,PMINT,PMEND
!
!*       0.2   declarations of local variables
!
INTEGER                            :: JI,JJ,JK, JN, IDT
REAL, DIMENSION(SIZE(PM,1))        :: ZSUM
REAL, DIMENSION(SIZE(PM,1),JPMODE) :: ZOM,ZMASK
REAL, DIMENSION(SIZE(PM,1))        :: ZSIGMA
!
REAL, DIMENSION(SIZE(PM,1))        :: ZA,ZB,ZC,ZD
REAL, DIMENSION(SIZE(PM,1))        :: ZCONST1,ZCONST2
REAL, DIMENSION(SIZE(PM,1))        :: Z0,ZK,ZKEXP
!
REAL, SAVE,  DIMENSION(JPMODE*3)   :: ZPMIN
REAL, SAVE,  DIMENSION(JPMODE)     :: ZRATIOBC, ZRATIOOC
REAL                               :: ZINIRADIUSI, ZINIRADIUSJ
!
!-------------------------------------------------------------------------------
!
!*       1.     INITIALIZATION
!               --------------
!
PMBEG(:,:)=PM(:,:)
!
IF (CRGUNIT=="MASS") THEN
  ZINIRADIUSI = XINIRADIUSI * EXP(-3.*(LOG(XINISIGI))**2)
  ZINIRADIUSJ = XINIRADIUSJ * EXP(-3.*(LOG(XINISIGJ))**2)
ELSE
  ZINIRADIUSI = XINIRADIUSI
  ZINIRADIUSJ = XINIRADIUSJ
END IF
!
ZPMIN(1) = XN0IMIN
ZPMIN(2) = ZPMIN(1) * (ZINIRADIUSI**3)*EXP(4.5 * LOG(XINISIGI)**2) 
ZPMIN(3) = ZPMIN(1) * (ZINIRADIUSI**6)*EXP(18. * LOG(XINISIGI)**2)
!
ZPMIN(4) = XN0JMIN
ZPMIN(5) = ZPMIN(4) * (ZINIRADIUSJ**3)*EXP(4.5 * LOG(XINISIGJ)**2) 
ZPMIN(6) = ZPMIN(4) * (ZINIRADIUSJ**6)*EXP(18. * LOG(XINISIGJ)**2)
!
!-------------------------------------------------------------------------------
!
!*       2.     SOLVE MOMENT DYNAMIC EQUATIONS
!               ------------------------------
!
DO JI=1,JPMODE
  !
  !*           2.1     MOMENT 0
  ! 
  !*************************************************************
  ! Resolution du moment d'ordre 0: pour cela il faut resoudre
  ! une equation differentielle du type dY/dt=-AY^2-BY+C
  ! these Crassier page 42
  !*************************************************************
  !
  ! Pour la resolution plusieurs cas particuliers seront traites
  ZA(:) = 0.0
  ZB(:) = 0.0
  ZC(:) = 0.0
  ZA(:) = -PDMINTRA(:,NM0(JI)) / (PM(:,NM0(JI))**2.0)
  ZB(:) = -PDMINTER(:,NM0(JI)) /  PM(:,NM0(JI))
  ZC(:) =  PDMCOND (:,NM0(JI)) + PDMNUCL(:,NM0(JI))
  !
  DO JK=1,SIZE(PM,1)
    IF ( (ZA(JK) == 0.) .AND. (ZB(JK) == 0.) ) THEN
      IF (NVERB .GE. 10) WRITE(*,*) '~~~ CH_AER_SOLV 1.1 : IF 2'
      PM(JK,NM0(JI)) = PM(JK,NM0(JI)) + ZC(JK) * PDT
    ELSE IF ((ZB(JK) == 0. .AND. ZC(JK)/PM(JK,NM0(JI)) <= 1.e-10).OR. &
      (ZC(JK) <= 1.e-10 .AND. ZB(JK)/ZA(JK) <= 1.e-3))  THEN
      IF (NVERB .GE. 10) WRITE(*,*) '~~~ CH_AER_SOLV 1.1 : IF 1'
      ! type dY/dt=-AY^2
      Z0(JK)=PM(JK,NM0(JI)) 
      PM(JK,NM0(JI))=Z0(JK)/(1.+ZA(JK)*Z0(JK)*PDT)
    ELSE
      IF (NVERB .GE. 10) WRITE(*,*) '~~~ CH_AER_SOLV 1.1 : IF 3'
      ZCONST1(JK)=ZB(JK)/(2.*ZA(JK))
      Z0(JK)=PM(JK,NM0(JI))+ZCONST1(JK)
      IF (((ZB(JK)**2+4.*ZA(JK)*ZC(JK))) < 0.) THEN
        ZD(JK)=SQRT(ABS(ZB(JK)**2+4.*ZA(JK)*ZC(JK)))
        PM(JK,NM0(JI))=-ZCONST1(JK)+ZD(JK)*TAN(ATAN(Z0(JK)/ZD(JK))-ZA(JK)*ZD(JK)*PDT)
      ELSE
        ZD(JK)=SQRT(ZB(JK)**2+4.*ZA(JK)*ZC(JK))
        ZCONST2(JK)=ZD(JK)/(2.*ABS(ZA(JK)))
        ZKEXP(JK)=EXP(-2.*ZA(JK)*ZCONST2(JK)*PDT)
        ZK(JK)=(Z0(JK)-ZCONST2(JK))/(Z0(JK)+ZCONST2(JK))*ZKEXP(JK)
        PM(JK,NM0(JI))=-ZCONST1(JK)+ZCONST2(JK)*(1.+ZK(JK))/(1.-ZK(JK))
      ENDIF
    ENDIF
  ENDDO
  !
  PM(:,NM0(JI)) = PM(:,NM0(JI)) + (PDMMERG(:,NM0(JI)) + PSEDA(:,NM0(JI))) * PDT
  PM(:,NM0(JI))= MAX(PM(:,NM0(JI)), XMNH_TINY )
  !
  !*************************************************************
  ! Resolution du moment d'ordre 3
  ! eq. diff. de type dY/dt = K
  !*************************************************************
  !
  PM(:,NM3(JI))=PM(:,NM3(JI))+ &
                (PDMINTRA(:,NM3(JI))+PDMINTER(:,NM3(JI))+PDMCOND(:,NM3(JI))+&
                 PDMNUCL(:,NM3(JI))+PDMMERG(:,NM3(JI))+PSEDA(:,NM3(JI)))*PDT
  !
  PM(:,NM3(JI))= MAX(PM(:,NM3(JI)), XMNH_TINY)
  !
  !*************************************************************
  ! Resolution du moment d'ordre 6
  ! eq. diff. de type dY/dt = K
  !*************************************************************
  !
  PM(:,NM6(JI))=PM(:,NM6(JI))+ &
               (PDMINTRA(:,NM6(JI))+PDMINTER(:,NM6(JI))+PDMCOND(:,NM6(JI))+&
                PDMNUCL(:,NM6(JI))+PDMMERG(:,NM6(JI))+PSEDA(:,NM6(JI)) )*PDT
  !
  PM(:,NM6(JI))= MAX(PM(:,NM6(JI)), XMNH_TINY)
  !
ENDDO
!
!-------------------------------------------------------------------------------
!
!*       3.     CHEMICAL EQUILIBRIUM 
!               --------------------
!
!******************************************************************
! Calcul de la variation de concentration des differents
! composes pour trouver le nouveau moment d'ordre 3
!******************************************************************
!
DO JI=1,JPMODE
  !
  ! Coagulation intermodale 
  !-------------------------
  !
  DO JJ=1,NSP+NCARB+NSOA
    !
    PCTOTA(:,JJ,JI)=PCTOTA(:,JJ,JI) &
                +(PCCTOT(:,JJ,1)*PDMINTER(:,NM3(JI)) + PCCTOT(:,JJ,JI)* PDMINTRA(:,NM3(JI))) &
                *XFAC(JJ)*PDT
    !
    ! Sedimentation
    !--------------
    PCTOTA(:,JJ,JI)=  PCTOTA(:,JJ,JI) + PCCTOT(:,JJ,JI)*PSEDA(:,NM3(JI))*XFAC(JJ)*PDT
    PCTOTA(:,JJ,JI)=  MAX(PCTOTA(:,JJ,JI), XMNH_TINY)
    !
  ENDDO
  !
ENDDO
!
! H2SO4 Condensation + Nucleation 
!---------------------------------
!
PCTOTA(:,JP_AER_SO4,1)=PCTOTA(:,JP_AER_SO4,1) &
                      +(PDMCOND(:,NM3(1))+PDMNUCL(:,NM3(1))+PDMMERG(:,NM3(1)))*XFAC(JP_AER_SO4)*PDT
PCTOTA(:,JP_AER_SO4,2)=PCTOTA(:,JP_AER_SO4,2) &
                      +(PDMCOND(:,NM3(2))+PDMNUCL(:,NM3(2))+PDMMERG(:,NM3(2)))*XFAC(JP_AER_SO4)*PDT
!
!*************************************************************
! Calcul de la fraction massique entre les modes
!*************************************************************
ZSUM (:) = 0.
DO JI=1,JPMODE
  DO JJ=1,NSP+NCARB+NSOA
    ZSUM (:) = ZSUM (:) + PCTOTA(:,JJ,JI)
  ENDDO
ENDDO
ZOM(:,:) = 0.
DO JI=1,JPMODE
  DO JJ=1,NSP+NCARB+NSOA
    ZOM(:,JI)  =  ZOM(:,JI) + PCTOTA(:,JJ,JI) / ZSUM (:) 
  ENDDO
ENDDO
!
! Equilibre mineraux
!-------------------
!
IDT = INT(MAX(5.*PDT,1.))
!
IF ((PDT .GT. 0.).AND.( MOD(INT(PTIME) , IDT) .EQ. 0)) THEN
  CALL CH_AER_MINERAL(PCTOTG, PCTOTA,PRV, PDENAIR, PPRESSURE, PTEMP, PRC, ZOM,&
                      PCCTOT)
  !
  ! Equilibre Organiques
  !---------------------
  !
  IF (NSOA .EQ. 10)   CALL CH_AER_ORGANIC(PCTOTG, PCTOTA,PRV, PDENAIR, &
                                          PPRESSURE, PTEMP,&
                                          PRC, ZOM, PCCTOT,PLNSIG, PRG, PDT, PSOLORG)
  !
END IF
!
! Forced mass need to be positive
PCTOTA(:,:,:) = MAX(PCTOTA(:,:,:), 0.0)
PCTOTG(:,:)   = MAX(PCTOTG(:,:)  , 0.0)
!
DO JI=1,JPMODE
  ZSUM(:)=0.
  DO JJ=1,NSP+NCARB+NSOA
    ZSUM(:)=ZSUM(:)+PCTOTA(:,JJ,JI)/XRHOI(JJ)
  ENDDO
  !
  DO JJ=1,NSP+NCARB+NSOA
    PCCTOT(:,JJ,JI)=PCTOTA(:,JJ,JI)/XRHOI(JJ)/ZSUM(:)
  ENDDO
ENDDO
!
PMINT(:,:)=PM(:,:)
!
!
!-------------------------------------------------------------------------------
!
!*       4.     ADJUSTEMENT OF AEROSOL DISTRIBUTION AFTER CHEMICAL EQUILIBRIUM 
!               --------------------------------------------------------------
!
!
!******************************************************************************
! Calcul des nouveaux moments d'ordre 3 et 6
! Le moment d'ordre 3 est recalcule a partir de la composition de chaque mode
! Le moment d'ordre 6 est calcule pour garder sigma constant pendant l'equilibre chimique
!******************************************************************************
!
!              4.1    COMPUTATION OF THE NEW SIGMA
!                     ----------------------------
!
DO JN=1,JPMODE
  !
  IF (JN .EQ. 1) THEN
    !
    IF (LVARSIGI) THEN ! variable dispersion for mode 1
      !
      ZSIGMA(:) = PM(:,NM3(JN))**2./(PM(:,NM0(JN))*PM(:,NM6(JN)))
      ZSIGMA(:) = MIN(1-1E-10,ZSIGMA(:))
      ZSIGMA(:) = MAX(1E-10,ZSIGMA(:))
      ZSIGMA(:) = LOG(ZSIGMA(:))
      ZSIGMA(:) = EXP(1./3.*SQRT(-ZSIGMA(:)))
      !
      WHERE (ZSIGMA(:) > XSIGIMAX)
        ZSIGMA(:) =  XSIGIMAX
      END WHERE
      !
      WHERE (ZSIGMA(:) < XSIGIMIN)
        ZSIGMA(:) =  XSIGIMIN
      END WHERE
      !
    ELSE ! fixed dispersion for mode 1
      ZSIGMA(:) = XINISIGI
    END IF
  END IF
  !
  IF (JN .EQ. 2) THEN
    !
    IF (LVARSIGJ) THEN ! variable dispersion for mode 2
      !
      ZSIGMA(:) = PM(:,NM3(JN))**2./(PM(:,NM0(JN))*PM(:,NM6(JN)))
      ZSIGMA(:) = MIN(1-1E-10,ZSIGMA(:))
      ZSIGMA(:) = MAX(1E-10,ZSIGMA(:))
      ZSIGMA(:) = LOG(ZSIGMA(:))
      ZSIGMA(:) = EXP(1./3.*SQRT(-ZSIGMA(:)))
      !
      WHERE (ZSIGMA(:) > XSIGJMAX)
        ZSIGMA(:) =  XSIGJMAX
      END WHERE
      !
      WHERE (ZSIGMA(:) < XSIGJMIN)
        ZSIGMA(:) =  XSIGJMIN
      END WHERE
      !
    ELSE ! fixed dispersion for mode 2
      ZSIGMA(:) = XINISIGJ
    END IF
  END IF
  !
  PLNSIG(:,JN) = LOG(ZSIGMA(:))
  !
END DO
!
!
!              4.2    COMPUTATION OF THE MOMENT 3 AFTER CHEMICAL EQUILIBRIUM
!                     ------------------------------------------------------
!
DO JN=1,JPMODE
  ZSUM(:)=0.0
  DO JJ=1,NSP+NCARB+NSOA
    ZSUM(:) = ZSUM(:)+PCTOTA(:,JJ,JN)/XFAC(JJ)
  ENDDO
  PM(:,NM3(JN))=ZSUM(:)
END DO
!
!
!              4.2    COMPUTATION OF THE MOMENT 6 AFTER CHEMICAL EQUILIBRIUM
!                     ------------------------------------------------------
!
DO JN=1,JPMODE
  PM(:,NM6(JN)) = PM(:,NM0(JN)) &
          * ( (PM(:,NM3(JN))/PM(:,NM0(JN)))**(1./3.) * EXP(-(3./2.)*PLNSIG(:,JN)**2))**6 &
          * EXP(18.*PLNSIG(:,JN)**2)
ENDDO
!
!-------------------------------------------------------------------------------
!
!*       5.     TO AVOID VALUES BELOW MINIMUM REQUIRED 
!               --------------------------------------
!
!*************************************************************
! Blindages pour valeurs inferieurs au mininmum accepte
!*************************************************************
!
DO JN=1,JPMODE
  ZMASK(:,JN) = 1.
  WHERE ((PM(:,NM0(JN)) .LT. ZPMIN(NM0(JN))).OR.&
         (PM(:,NM3(JN)) .LT. ZPMIN(NM3(JN))).OR.&
         (PM(:,NM6(JN)) .LT. ZPMIN(NM6(JN))))

    PM(:,NM0(JN)) = ZPMIN(NM0(JN))
    PM(:,NM3(JN)) = ZPMIN(NM3(JN))
    PM(:,NM6(JN)) = ZPMIN(NM6(JN))

    ZMASK(:,JN)  = 0.
  END WHERE
  DO JJ=1,NSP+NCARB+NSOA
    PCTOTA(:,JJ,JN) = PCTOTA(:,JJ,JN) * ZMASK(:,JN)
  ENDDO
  WHERE (ZMASK(:,JN) == 0.)
    PCTOTA(:,JP_AER_BC,JN) = 0.5 * ZPMIN(NM3(JN)) * XFAC(JP_AER_BC)
    PCTOTA(:,JP_AER_OC,JN) = 0.5 * ZPMIN(NM3(JN)) * XFAC(JP_AER_OC)
  END WHERE
  !
ENDDO
!
PMEND(:,:)=PM(:,:)
!
END SUBROUTINE CH_AER_SOLV
