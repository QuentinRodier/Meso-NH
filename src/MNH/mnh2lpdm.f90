!MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
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
USE MODD_IO_ll,ONLY : TPTR2FILE
!
USE MODE_FM
USE MODE_IO_MANAGE_STRUCT, ONLY: IO_FILE_ADD2LIST
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
TYPE(TPTR2FILE),DIMENSION(JPMNHMAX) :: TZFMNH  ! MesoNH files
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
   CALL IO_FILE_ADD2LIST(TZFMNH(1)%TZFILE,TRIM(CFMNH(1)),'UNKNOWN','READ',KLFINPRAR=0,KLFITYPE=2,KLFIVERB=IVERB)
   DO WHILE (CFMNH(NBMNH+1).NE.'VIDE')
      NBMNH=NBMNH+1
      CALL IO_FILE_ADD2LIST(TZFMNH(NBMNH)%TZFILE,TRIM(CFMNH(NBMNH)),'UNKNOWN','READ',KLFINPRAR=0,KLFITYPE=2,KLFIVERB=IVERB)
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
CALL MNH2LPDM_INI(TZFMNH(1)%TZFILE,TZFMNH(NBMNH)%TZFILE,YFLOG,IFLOG,IFGRI,IFDAT)
!
!
!*	2.3 Traitement des echeances.
!
DO JFIC=1,NBMNH
   print*,"CFMTO(JFIC)=",CFMTO(JFIC)
   CALL OPEN_LL(UNIT=IFMTO,FILE=CFMTO(JFIC),IOSTAT=IREP,FORM='UNFORMATTED', &
      ACTION='WRITE',MODE='GLOBAL',RECL=100000000)
   CALL MNH2LPDM_ECH(TZFMNH(JFIC)%TZFILE,IFMTO,YFLOG)
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
