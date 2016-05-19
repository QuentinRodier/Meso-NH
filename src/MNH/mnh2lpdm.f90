!     ######spl
        PROGRAM MNH2LPDM
!	##############
!-----------------------------------------------------------------------------
!****	MNH2DIF COUPLAGE MESO-NH / SPRAY.
!
!	Auteur   :   Michel Bouzom, DP/SERV/ENV
!	Creation :   16.07.2002
!       Modification  : 07.01.2006 (T.LAUVAUX, adaptation LPDM)
!       Modification  : 04.01.2009 (F. BONNARDOT, DP/SER/ENV )
!
!-----------------------------------------------------------------------------
!
!
!
!*	0.  DECLARATIONS.
!	    -------------
!
!*	0.1 Modules.
!
USE MODD_MNH2LPDM
USE MODI_IO_LL
USE MODD_CONF, ONLY : CPROGRAM
!
USE MODI_MNH2LPDM_INI
USE MODI_MNH2LPDM_ECH
!
!
!*	0.2 Variables locales.
!
IMPLICIT NONE
!
CHARACTER(LEN=28) :: YFNML,YFLOG        ! Nom   NAMELIST et LOG.
INTEGER           :: IFNML,IFLOG        ! Unite NAMELIST et LOG.
INTEGER           :: IFMTO,IFGRI,IFDAT  ! Unite METEO et GRILLE.
INTEGER           :: IREP,IVERB,JFIC
!
!
!
!
!*	1.  INITIALISATION.
!	    ---------------
!
!*	1.1 Variables generales.
!
YFLOG    = 'METEO.log'
YFNML    = 'MNH2LPDM1.nam'
IVERB    =  5
 CFMNH(:) = 'VIDE'
IFLOG = 52
IFGRI = 51
IFDAT = 53
!
!
!*	1.2 Initialisation routines LL.
!
CALL INITIO_LL
!
!
!*	1.3 Ouverture du fichier log.
!
CALL OPEN_LL(UNIT=IFLOG,FILE=YFLOG,IOSTAT=IREP,FORM='FORMATTED', &
     ACTION='WRITE',MODE='GLOBAL')
!
!
!*	1.4 Lecture des namelists.
!
CALL OPEN_LL(UNIT=IFNML,FILE=YFNML,IOSTAT=IREP,FORM='FORMATTED', &
     ACTION='READ',MODE='GLOBAL')
print *,'Ouverture fichier Namlist OK'
READ(UNIT=IFNML,NML=NAM_TURB)
READ(UNIT=IFNML,NML=NAM_FIC)
print *,'Lecture de NAM_FIC OK.'
CALL CLOSE_LL(YFNML,IREP,'KEEP')
!
!
!*	1.5 Comptage des FM a traiter.
!
IF (CFMNH(1).NE.'VIDE') THEN
   NBMNH=1
   DO WHILE (CFMNH(NBMNH+1).NE.'VIDE')
      NBMNH=NBMNH+1
   END DO
   print *,NBMNH,' fichiers a traiter.'
ELSE
   STOP
ENDIF   
!
!
!
!
!*	2.  TRAITEMENTS.
!	    ------------
!
!*	2.1 Ouverture des fichiers METEO et GRILLE et DATE.
!
CALL OPEN_LL(UNIT=IFGRI,FILE=CFGRI,IOSTAT=IREP,FORM='FORMATTED', &
  ACTION='WRITE',MODE='GLOBAL')
CALL OPEN_LL(UNIT=IFDAT,FILE=CFDAT,IOSTAT=IREP,FORM='FORMATTED', &
  ACTION='WRITE',MODE='GLOBAL')
!
!
!*	2.2 Preparation du couplage.
!
CALL MNH2LPDM_INI(CFMNH(1),CFMNH(NBMNH),YFLOG,IFLOG,IFGRI,IFDAT,IVERB)
!
!
!*	2.3 Traitement des echeances.
!
DO JFIC=1,NBMNH
   IFMTO = JFIC+60
   print*,"CFMTO(JFIC)=",CFMTO(JFIC)
   CALL OPEN_LL(UNIT=IFMTO,FILE=CFMTO(JFIC),IOSTAT=IREP,FORM='UNFORMATTED', &
      ACTION='WRITE',MODE='GLOBAL',RECL=100000000)
   CALL MNH2LPDM_ECH(CFMNH(JFIC),IFMTO,YFLOG,IFLOG,IVERB)
   print*,"CLOSE_LL(CFMTO(JFIC)"
   CALL CLOSE_LL(CFMTO(JFIC),IREP,'KEEP')
END DO
!
!
!*	2.4 Fermeture des fichiers, METEO, GRILLE et LOG.
!
CALL CLOSE_LL(CFGRI,IREP,'KEEP')
CALL CLOSE_LL(CFDAT,IREP,'KEEP')
CALL CLOSE_LL(YFLOG,IREP,'KEEP')
!
!
!
END PROGRAM MNH2LPDM
