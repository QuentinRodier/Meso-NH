!MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!     ######spl
        SUBROUTINE MNH2LPDM_ECH(HFM,KFMTO,HFLOG,KFLOG,KVERB)
!	##################################################
!-----------------------------------------------------------------------
!****	MNH2S2_ECH TRAITEMENT D'UNE ECHEANCE.
!
!	Auteur   : Francois Bonnardot, DP/SERV/ENV
!	Creation : 07.01.2009
!-----------------------------------------------------------------------
!
!*	0.  DECLARATIONS.
!	    -------------
!
!*	0.1 Modules.
!
!
!
USE MODD_DIM_n
USE MODD_TIME_n
USE MODD_GRID_n
!
USE MODD_CST
USE MODD_PARAMETERS
USE MODD_TIME
!
USE MODD_MNH2LPDM
!
USE MODI_IO_LL
USE MODI_FM_LL
USE MODI_FMREAD_LL
USE MODI_INI_CST
!
IMPLICIT NONE
!
!
!*	0.2 Arguments.
!
CHARACTER(LEN=*), INTENT(IN)  :: HFM,HFLOG
INTEGER, INTENT(IN)  :: KVERB,KFMTO
INTEGER, INTENT(IN)  :: KFLOG
!
!
!*	0.3 Variables locales.
!
CHARACTER(LEN=100)   :: YCOM                         ! Commentaire champ FM.
CHARACTER(LEN=100)   :: YFTURB                       ! Stockage champs de turbulence.
INTEGER              :: KFTURB
INTEGER              :: IREP,INBART,IGRID,ILONCOM    ! Variables lecture FM.
INTEGER,DIMENSION(3) :: IDATCUR                      ! Date dans FM.
REAL                 :: ZSECCUR                      ! Heure (sec) dans FM.
INTEGER              :: ICURAA,ICURMM,ICURJJ         ! Date  courante.
INTEGER              :: ICURHH,ICURMN,ICURSS         ! Heure courante.
INTEGER              :: JI,JJ,JK
!
!
!
!
!*	1.  INITIALISATION.
!	    ---------------
!
!*	1.1 Blabla.
!
!
!*	2.  LECTURE DES DONNEES MESO-NH DE BASE.
!	    ------------------------------------
!
!*	2.1 Ouverture du fichier Meso-NH.
!
CALL FMOPEN_LL(HFM,'READ',HFLOG,0,2,KVERB,INBART,IREP)
!
!*	2.2 Date et heure courante.
!
CALL FMREAD(HFM,'DTCUR%TDATE',HFLOG,'--',IDATCUR,IGRID,ILONCOM,YCOM,IREP)
CALL FMREAD(HFM,'DTCUR%TIME', HFLOG,'--',ZSECCUR,IGRID,ILONCOM,YCOM,IREP)
! 
ICURAA=MOD(IDATCUR(1),100)  ! Annee sur 2 caracteres.
ICURMM=IDATCUR(2)
ICURJJ=IDATCUR(3)
ICURSS=NINT(ZSECCUR)
!
ICURMN = NINT( (FLOAT(ICURSS)/60.0)/5.0 )*5   ! Heure arrondie a 5 minutes pres.
ICURSS = 0
ICURHH =ICURMN/60
ICURMN =ICURMN-ICURHH*60
!
print*, '%%% MNH2LPDM2_ECH Date et heure des donnees :'
print 20300, ICURJJ,ICURMM,ICURAA,ICURHH,ICURMN,ICURSS
20300 FORMAT(I2.2,'/',I2.2,'/',I4.4,' ',I2.1,'h',I2.1,'mn',I2.1,'sec')
!
!
!
!*	2.3 Lecture des champs Meso-NH de base.
!
CALL FMREAD(HFM,'UM',HFLOG,'XY',XUM,IGRID,ILONCOM,YCOM,IREP)
CALL FMREAD(HFM,'VM',HFLOG,'XY',XVM,IGRID,ILONCOM,YCOM,IREP)
CALL FMREAD(HFM,'WM',HFLOG,'XY',XWM,IGRID,ILONCOM,YCOM,IREP)
CALL FMREAD(HFM,'THM',HFLOG,'XY',XTHM,IGRID,ILONCOM,YCOM,IREP)
CALL FMREAD(HFM,'TKEM',HFLOG,'XY',XTKEM,IGRID,ILONCOM,YCOM,IREP)
CALL FMREAD(HFM,'LM',HFLOG,'XY',XLM,IGRID,ILONCOM,YCOM,IREP)
CALL FMREAD(HFM,'THW_FLX',HFLOG,'XY',XWPTHP,IGRID,ILONCOM,YCOM,IREP)
CALL FMREAD(HFM,'DISS',HFLOG,'XY',XDISSIP,IGRID,ILONCOM,YCOM,IREP)
CALL FMREAD(HFM,'FMU',HFLOG,'XY',XSFU,IGRID,ILONCOM,YCOM,IREP)
CALL FMREAD(HFM,'FMV',HFLOG,'XY',XSFV,IGRID,ILONCOM,YCOM,IREP)
CALL FMREAD(HFM,'INPRT',HFLOG,'XY',XINRT,IGRID,ILONCOM,YCOM,IREP)
CALL FMREAD(HFM,'RVM',HFLOG,'XY',XRMVM,IGRID,ILONCOM,YCOM,IREP)
CALL FMREAD(HFM,'RCM',HFLOG,'XY',XRMCM,IGRID,ILONCOM,YCOM,IREP)
CALL FMREAD(HFM,'RRM',HFLOG,'XY',XRMRM,IGRID,ILONCOM,YCOM,IREP)
!
!              Lecture des donnees Meso-NH terminee.'
!
!*	2.4 Fermeture du fichier Meso-NH.
!
CALL FMCLOS_LL(HFM,'KEEP',HFLOG,IREP)
!
!
!*	3.  PREPARATION DES DONNEES.
!	    ------------------------
!
!
!*	3.2 Niveaux altitude "hors-sol" (1:NKMAX).
!
XSU(:,:,1:NKMAX) = XUM(NSIB:NSIE,NSJB:NSJE,NKB:NKE)
XSV(:,:,1:NKMAX) = XVM(NSIB:NSIE,NSJB:NSJE,NKB:NKE)
XSW(:,:,1:NKMAX) = XWM(NSIB:NSIE,NSJB:NSJE,NKB:NKE)
XSTH(:,:,1:NKMAX) = XTHM(NSIB:NSIE,NSJB:NSJE,NKB:NKE)
XSTKE(:,:,1:NKMAX) = XTKEM(NSIB:NSIE,NSJB:NSJE,NKB:NKE)
XSLM(:,:,1:NKMAX) = XLM(NSIB:NSIE,NSJB:NSJE,NKB:NKE)
XSDISSIP(:,:,1:NKMAX) = XDISSIP(NSIB:NSIE,NSJB:NSJE,NKB:NKE)
XSINRT(:,:) = XINRT(NSIB:NSIE,NSJB:NSJE)
XSWPTHP(:,:,1:NKMAX) = XWPTHP(NSIB:NSIE,NSJB:NSJE,NKB:NKE)
XSRMV(:,:,1:NKMAX) = XRMVM(NSIB:NSIE,NSJB:NSJE,NKB:NKE)
XSRMC(:,:,1:NKMAX) = XRMCM(NSIB:NSIE,NSJB:NSJE,NKB:NKE)
XSRMR(:,:,1:NKMAX) = XRMRM(NSIB:NSIE,NSJB:NSJE,NKB:NKE)
XSSFU(:,:) = XSFU(NSIB:NSIE,NSJB:NSJE)
XSSFV(:,:) = XSFV(NSIB:NSIE,NSJB:NSJE)
!
!
!*	4.  CALCULS DES TEMPS LAGRANGIENS ET VARIANCES DU VENT POUR LPDM.
!	    ------------------------------------------------------------
!
      XRVSRD  = XRV/XRD
!
      XSUSTAR (:,:)   = XUNDEF
      XSLMO   (:,:)   = XUNDEF
      XSHMIX  (:,:)   = XUNDEF
      XSWSTAR (:,:)   = XUNDEF
      XSSIGU  (:,:,:) = XUNDEF
      XSSIGW  (:,:,:) = XUNDEF
      XSTIMEU (:,:,:) = XUNDEF
      XSTIMEW (:,:,:) = XUNDEF
!
      DO JI=1,NSIMAX ; DO JJ=1,NSJMAX
        !
        !* Temperature potentielle virtuelle.
        !
        XSTHETAV(:)=1.0+XSRMV(JI,JJ,:)+XSRMC(JI,JJ,:)+XSRMR(JI,JJ,:)
        XSTHETAV(:) = XSTH(JI,JJ,:)*(1.0+XSRMV(JI,JJ,:)*XRVSRD)/XSTHETAV(:)
        !
        !* ZHMIX Hauteur de melange.
        !
        XTHSOL       = XSTHETAV(1)+0.5
        XSHMIX(JI,JJ) = 0.0
        DO JK=2,NKMAX
           IF ( XSTHETAV(JK).GT.XTHSOL ) THEN
              XSHMIX(JI,JJ)     = XSHAUT  (JK-1)  &
                +( XSHAUT  (JK) - XSHAUT  (JK-1) )  &
                /( XSTHETAV(JK) - XSTHETAV(JK-1) )  &
                *( XTHSOL      - XSTHETAV(JK-1) )
              EXIT
           ENDIF
        END DO
        XSHMIX(JI,JJ)=MAX(XSHMIX(JI,JJ),50.0)

        !
        !* XSUSTAR Vitesse de frottement.
        !
        XSUSTAR(JI,JJ) = XSSFU(JI,JJ)*XSSFU(JI,JJ) &
                        +XSSFV(JI,JJ)*XSSFV(JI,JJ)
        XSUSTAR(JI,JJ) = SQRT(SQRT(XSUSTAR(JI,JJ)))
        !
        !
        !
        !* XSLMO Longueur de Monin-Obukhov.
        !
        IF (XSWPTHP(JI,JJ,1).NE.0.) THEN
         XSLMO(JI,JJ)= -XSTHETAV(1)*(XSUSTAR(JI,JJ)**3) &
                     / (XKARMAN*XG*XSWPTHP(JI,JJ,1))
        ENDIF
        !
        !
        !* XSWSTAR Vitesse Verticale Convective.
        !
        XSWSTAR(JI,JJ)=XG/XSTHETAV(1)*XSWPTHP(JI,JJ,1)*XSHMIX(JI,JJ)
        XSWSTAR(JI,JJ)=SIGN(1.,XSWSTAR(JI,JJ)) &
                               * ( ABS(XSWSTAR(JI,JJ))**(1./3.))
        !
        !
        IF (CTURBPARAM=="HANNA".OR.CTURBPARAM=="HANNABIS") THEN
        !
        IF ((XSLMO(JI,JJ).GT.0).AND.(XSLMO(JI,JJ).LE.300)) THEN
           !
           !* Conditions stables.
           !
           !*	XSSIGU,XSSIGW <u'2>**0.5, <w'2>**0.5
           DO JK=1,NKMAX
           IF (XSHAUT(JK).LT.XSHMIX(JI,JJ)) THEN
           XSSIGU(JI,JJ,JK) = SQRT( 0.5 *                              &
                ((2.0*(1-XSHAUT(JK)/XSHMIX(JI,JJ))*XSUSTAR(JI,JJ))**2) &
              + ((1.3*(1-XSHAUT(JK)/XSHMIX(JI,JJ))*XSUSTAR(JI,JJ))**2) )
           XSSIGW(JI,JJ,JK) = 1.3*(1-XSHAUT(JK)/XSHMIX(JI,JJ))         &
                             *XSUSTAR(JI,JJ)
           ELSE
           XSSIGU(JI,JJ,JK) = 0.001
           XSSIGW(JI,JJ,JK) = 0.001
           ENDIF
           ENDDO
           ! 
           XSSIGU(JI,JJ,:)=MAX(0.001,XSSIGU(JI,JJ,:))
           XSSIGW(JI,JJ,:)=MAX(0.001,XSSIGW(JI,JJ,:))
           !
           !* Lagrangian time scale
           XSTIMEU(JI,JJ,:) = 0.11*XSHMIX(JI,JJ)/XSSIGU(JI,JJ,:)  &
                             *SQRT( XSHAUT(:)/XSHMIX(JI,JJ) )
           XSTIMEW(JI,JJ,:) = 0.10*XSHMIX(JI,JJ)/XSSIGW(JI,JJ,:)  &
                             *( XSHAUT(:)/XSHMIX(JI,JJ) )**0.8
           !
           !
        ENDIF
        !
        !
        IF (ABS(XSLMO(JI,JJ)).GT.300) THEN
           !
           !* Conditions neutres.
           !
           !* XSSIGU,XSSIGW <u'2>**0.5, <w'2>**0.5
           XSSIGU(JI,JJ,:)=SQRT( 0.5 *                                &
        ((2.0*XSUSTAR(JI,JJ)*EXP(-3*XSCORIOZ(JI,JJ)*XSHAUT(:)/XSUSTAR(JI,JJ)))**2)  &
     +  ((1.3*XSUSTAR(JI,JJ)*EXP(-2*XSCORIOZ(JI,JJ)*XSHAUT(:)/XSUSTAR(JI,JJ)))**2) )
           XSSIGW(JI,JJ,:)=1.3*XSUSTAR(JI,JJ)*EXP(-2*XSCORIOZ(JI,JJ)*XSHAUT(:)/XSUSTAR(JI,JJ))
           XSSIGU(JI,JJ,:)=MAX(0.001,XSSIGU(JI,JJ,:))
           XSSIGW(JI,JJ,:)=MAX(0.001,XSSIGW(JI,JJ,:))
           !
           !* lagrangian time scale
           XSTIMEU(JI,JJ,:) = 0.5*XSHAUT(:)/   &
                 (XSSIGW(JI,JJ,:)*(1.+15.0*XSCORIOZ(JI,JJ)*XSHAUT(:)/XSUSTAR(JI,JJ)))
           XSTIMEW(JI,JJ,:) = XSTIMEU(JI,JJ,:)
           !
        ENDIF 
        !
        ! 
        IF ((XSLMO(JI,JJ).LT.0).AND.(XSLMO(JI,JJ).GE.-300)) THEN
           !
           !* Conditions instables.
           !
           !* XSSIGU,XSSIGW <u'2>**0.5, <w'2>**0.5
           ! 
           IF (CTURBPARAM=="HANNA") THEN
           !
             DO JK=1,NKMAX
             IF (XSHAUT(JK).LE.XSHMIX(JI,JJ)) THEN
             XSSIGU(JI,JJ,JK)=XSUSTAR(JI,JJ)         &
                        * (12+0.5*XSHMIX(JI,JJ)/ABS(XSLMO(JI,JJ)))**(1/3)
             ELSE
             XSSIGU(JI,JJ,JK)=0.001
             ENDIF
             ENDDO
             ! 
             DO JK=1,NKMAX
             !IF (XSHAUT(JK).LE.XSHMIX(JI,JJ)) THEN
             !  XSSIGW(JI,JJ,JK)=SQRT(  1.2*XSWSTAR(JI,JJ)**2 &
             !               *(1-0.9*XSHAUT(JK)/XSHMIX(JI,JJ)) &
             !               *(XSHAUT(JK)/XSHMIX(JI,JJ))**(2/3) &
             !            +   (1.8-1.4*XSHAUT(JK)/XSHMIX(JI,JJ)) &
             !               *XSUSTAR(JI,JJ)**2 )  
             !ELSE
             IF (XSHAUT(JK).LE.0.4*XSHMIX(JI,JJ)) THEN
               XSSIGW(JI,JJ,JK)=0.763*(XSHAUT(JK)/XSHMIX(JI,JJ))**0.175
             ELSE IF (XSHAUT(JK).LE.0.96*XSHMIX(JI,JJ)) THEN
               XSSIGW(JI,JJ,JK)=0.722*XSWSTAR(JI,JJ)* &
                                (1-XSHAUT(JK)/XSHMIX(JI,JJ))**0.207
             ELSE IF (XSHAUT(JK).LE.XSHMIX(JI,JJ)) THEN
               XSSIGW(JI,JJ,JK)=0.37*XSWSTAR(JI,JJ)
             ELSE
               XSSIGW(JI,JJ,JK)=0.001
             ENDIF
             ENDDO
             !
             XSSIGU(JI,JJ,:)=MAX(0.001,XSSIGU(JI,JJ,:))
             XSSIGW(JI,JJ,:)=MAX(0.001,XSSIGW(JI,JJ,:))
             !
             !* Lagrangian time scale
             XSTIMEU(JI,JJ,:) = 0.15*XSHMIX(JI,JJ)/XSSIGU(JI,JJ,:)
             DO JK=1,NKMAX
                 IF (XSHAUT(JK).LE.(0.1*XSHMIX(JI,JJ))) THEN
                   IF ( XSHAUT(JK).LT.(XSZ0(JI,JJ)-XSLMO(JI,JJ)) ) THEN
                     XSTIMEW(JI,JJ,JK) = 0.1*XSHAUT(JK)/XSSIGW(JI,JJ,JK)   &
                      / ( 0.55 - 0.38*(XSHAUT(JK)-XSZ0(JI,JJ))/ABS(XSLMO(JI,JJ)))
                   ELSE
                     XSTIMEW(JI,JJ,JK) = 0.59*XSHAUT(JK)/XSSIGW(JI,JJ,JK)
                   ENDIF
                 ELSE
                     XSTIMEW(JI,JJ,JK) = 0.15*XSHMIX(JI,JJ)/XSSIGW(JI,JJ,JK) &
                   *( 1.-EXP(-5*XSHAUT(JK)/XSHMIX(JI,JJ)) )
                 ENDIF
             END DO
           !
           ELSE IF (CTURBPARAM=="HANNABIS") THEN
             !* sigmas
             XSSIGW(JI,JJ,:) = SQRT(2./3.*XSTKE(JI,JJ,:))
             XSSIGU(JI,JJ,:) = XSSIGW(JI,JJ,:)
             !* Temps Lagrangien
             DO JK=1,NKMAX
               IF (XSHAUT(JK).LE.XSHMIX(JI,JJ)) THEN
               XSTIMEU(JI,JJ,JK)=0.17*XSHMIX(JI,JJ)/XSSIGU(JI,JJ,JK)
               XSTIMEW(JI,JJ,JK)=0.2*XSHMIX(JI,JJ)/XSSIGW(JI,JJ,JK)* &
               (1-EXP(-4*XSHAUT(JK)/XSHMIX(JI,JJ)) &
                 -0.0003*EXP(8*XSHAUT(JK)/XSHMIX(JI,JJ)))
               ELSE IF (XSHAUT(JK).LE.XSHMIX(JI,JJ)*1.2) THEN
               XSTIMEU(JI,JJ,JK)= &
                (1-(XSHAUT(JK)-XSHAUT(JK-1))/(XSHAUT(JK+1)-XSHAUT(JK-1)))* &
                XSTIMEU(JI,JJ,JK-1) &
                +(XSHAUT(JK)-XSHAUT(JK-1))/(XSHAUT(JK+1)-XSHAUT(JK-1))*10000.0
               XSTIMEW(JI,JJ,JK)= &
                (1-(XSHAUT(JK)-XSHAUT(JK-1))/(XSHAUT(JK+1)-XSHAUT(JK-1)))* &
                XSTIMEW(JI,JJ,JK-1)  &
                +(XSHAUT(JK)-XSHAUT(JK-1))/(XSHAUT(JK+1)-XSHAUT(JK-1))*10000.0
               ELSE
               XSTIMEU(JI,JJ,JK)=10000.0
               XSTIMEW(JI,JJ,JK)=10000.0
               ENDIF
             ENDDO
             !
           ENDIF ! CTURBPARAM=HANNA ou HANNABIS
           !  
        ENDIF ! instable
        !
        ELSE ! CTURBPARAM=="ISOTROPE"
          !
          !*	XSSIGU,XSSIGW <u'2>**0.5, <w'2>**0.5
          !
          XSSIGW(JI,JJ,:) = SQRT(2./3.*XSTKE(JI,JJ,:))
          XSSIGU(JI,JJ,:) = XSSIGW(JI,JJ,:)
          !
          !* Lagrangian time scale
          DO JK=1,NKMAX
            IF (XSHAUT(JK).LE.XSHMIX(JI,JJ)) THEN
            XSTIMEU(JI,JJ,JK)=ABS(2*(XSSIGU(JI,JJ,JK)**2)/(3*XSDISSIP(JI,JJ,JK)))
            XSTIMEW(JI,JJ,JK)=ABS(2*(XSSIGW(JI,JJ,JK)**2)/(3*XSDISSIP(JI,JJ,JK)))
            ELSE IF (XSHAUT(JK).LE.XSHMIX(JI,JJ)*1.2) THEN
            XSTIMEU(JI,JJ,JK)= &
            (1-(XSHAUT(JK)-XSHAUT(JK-1))/(XSHAUT(JK+1)-XSHAUT(JK-1)))*XSTIMEU(JI,JJ,JK-1) &
           +(XSHAUT(JK)-XSHAUT(JK-1))/(XSHAUT(JK+1)-XSHAUT(JK-1))*1000.0
            XSTIMEW(JI,JJ,JK)=XSTIMEU(JI,JJ,JK)
            ELSE
            XSTIMEU(JI,JJ,JK)=1000.0
            XSTIMEW(JI,JJ,JK)=1000.0
            ENDIF
          ENDDO
          !
        ENDIF
        !
        !
     END DO
  END DO
  !
     IF (IGRILLE.EQ.2) THEN
     WRITE(YFTURB,'("TURB_LPDM",5I2.2)') ICURAA,ICURMM,ICURJJ,ICURHH,ICURMN
     KFTURB=50
     CALL OPEN_LL(UNIT=KFTURB,FILE=YFTURB,IOSTAT=IREP,FORM='FORMATTED', &
      ACTION='WRITE',MODE='GLOBAL')
     WRITE(UNIT=KFTURB,FMT='(5A12)') "WSTAR       ","USTAR       ", &
                                     "HMIX        ","LMO         ", &
                                     "WPTHP"
     WRITE(UNIT=KFTURB,FMT='(5F12.5)') XSWSTAR(15,15),XSUSTAR(15,15), &
                                       XSHMIX(15,15),XSLMO(15,15),    &
                                       XSWPTHP(15,15,1)


     WRITE(UNIT=KFTURB,FMT='(8A12)') "HAUT          ","TKE           ", &
                                     "DISS          ","THETA         ", &
                                     "SIGU          ","SIGW          ", &
                                     "TIMEU         ","TIMEW         "
     DO JK=1,NKMAX
     WRITE(UNIT=KFTURB,FMT='(6F12.5,2F12.1)') XSHAUT(JK),XSTKE(15,15,JK), &
                                    XSDISSIP(15,15,JK),XSTH(15,15,JK), &
                                    XSSIGU(15,15,JK),XSSIGW(15,15,JK), &
                                    XSTIMEU(15,15,JK),XSTIMEW(15,15,JK)
  
     ENDDO
     CALL CLOSE_LL(YFTURB,IREP,'KEEP')
     ENDIF
!
               

!
!*	5.  ECRITURES FIC MTO.
!	    ------------------
!
!
DO JK = 1,NKMAX
WRITE(KFMTO) XSU(:,:,JK)        ! Composante zonale du vent.
ENDDO
DO JK = 1,NKMAX
WRITE(KFMTO) XSV(:,:,JK)        ! Composante meridienne du vent.
ENDDO
DO JK = 1,NKMAX
WRITE(KFMTO) XSW(:,:,JK)        ! Vitesse verticale.
ENDDO
DO JK = 1,NKMAX
WRITE(KFMTO) XSTH(:,:,JK)     ! Temperature potentielle.
ENDDO
DO JK = 1,NKMAX
WRITE(KFMTO) XSTKE(:,:,JK)       ! Energie cinetique Turbulence
ENDDO
DO JK = 1,NKMAX
WRITE(KFMTO) (XSSIGU(:,:,JK))**2       ! SigmaU
ENDDO
DO JK = 1,NKMAX
WRITE(KFMTO) (XSSIGU(:,:,JK))**2       ! SigmaV
ENDDO
DO JK = 1,NKMAX
WRITE(KFMTO) (XSSIGW(:,:,JK))**2       ! SigmaW
ENDDO
DO JK = 1,NKMAX
WRITE(KFMTO) XSTIMEU(:,:,JK)       ! Temps lagrangien U
ENDDO
DO JK = 1,NKMAX
WRITE(KFMTO) XSTIMEU(:,:,JK)       ! Temps lagrangien V
ENDDO
DO JK = 1,NKMAX
WRITE(KFMTO) XSTIMEW(:,:,JK)       ! Dissipation de TKE
ENDDO
WRITE(KFMTO) XSINRT
!
END SUBROUTINE MNH2LPDM_ECH
