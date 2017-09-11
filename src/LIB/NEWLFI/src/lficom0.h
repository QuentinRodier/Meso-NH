!MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
C
C----- DESCRIPTION DES "PARAMETER" DU LOGICIEL DE FICHIERS INDEXES -----
C-----  (et des variables logiques a charger absolument partout )  -----
C
C     JPNBIM = NOMBRE DE BITS PAR MOT MACHINE
C     JPNBIC = NOMBRE DE BITS PAR CARACTERE
C     JPNCMO = NOMBRE DE CARACTERES PAR MOT MACHINE
C
C     JPNCPN = NOMBRE MAXI. POSSIBLE DE CARACTERES PAR NOM D'ARTICLE
C     JPLARD = LONGUEUR D'ARTICLE "PHYSIQUE" elementaire des Fichiers
C              ( exprimee en mots, DOIT ETRE PAIRE, SUPERIEURE OU EGALE
C                a JPLDOC, JPLARD*JPNCMO DOIT ETRE MULTIPLE DE JPNCPN )
C     JPLARC = Longueur d'article "physique" exprimee en caracteres
C     JPRECL = PARAMETRE "RECL" de base POUR "OPEN" DES FICHIERS
C     JPNXFI = NOMBRE MAXIMUM DE FICHIERS INDEXES OUVERTS SIMULTANEMENT
C              (1 fichier de "multiplicite" N comptant comme N fichiers)
C     JPFACX = FACteur multiplicateur maXimum entre longueur d'article
C              physique effective et elementaire ( de 1 a JPNXFI )
C     JPXUFM = Nombre maXimum d'Unites logiques a Facteur Mul. predefini
C     JPNPIA = NOMBRE DE *PAIRES* DE "PAGES D'INDEX" EN MEMOIRE
C              *PREALLOUEES* PAR UNITE LOGIQUE ( AU MOINS *4* )
C     JPNXPI = NOMBRE TOTAL DE *PAIRES* DE "PAGES D'INDEX" EN MEMOIRE
C              ALLOUABLES ( DOIT ETRE AU MOINS EGAL A JPNPIA*JPNXFI )
C     JPNPIS = NOMBRE DE *PAIRES* DE "PAGES D'INDEX" NON PREALLOUEES
C     JPNXNA = NOMBRE MAXI. DE NOMS D'ARTICLES PAR PAGE/ARTICLE D'INDEX
C     JPNBLP = NOMBRE MAXI. DE COUPLES (LONGUEUR/POSITION)"   "     "
C     JPNAPP = NOMBRE MAXI. UTILE DE NOMS D'ARTICLES PAR PAGE/AR D'INDEX
C     JPLDOC = LONGUEUR (MOTS) DE LA PARTIE DOCUMENTAIRE DU 1ER ARTICLE
C     JPNPDF = NOMBRE DE PAGES DE DONNEES PAR FICHIER OUVERT ( >= 2 )
C     JPNXPR = NOMBRE MAXIMUM DE PAIRES D'ARTICLES D'INDEX RESERVABLES
C     JPNIL  = CODE DE "VALEUR ABSENTE" POUR CERTAINES TABLES D'ENTIERS.
C     JPNMPN = NOMBRE DE MOTS NECESSAIRE AU STOCKAGE D'UN NOM D'ARTICLE
C     JPNAPX = JPNAPP*JPFACX
C     JPLARX = JPLARD*JPFACX = longueur d'article physique maximale
C     JPLFTX = Longueur maximale traitable des noms de fichiers.
C     JPLFIX =    "        "     imprimable "   "   "     "    .
C     JPLSPX =    "        "   des noms des sous-programmes du logiciel.
C     JPLSTX =    "     "  des valeurs du "STATUS" FORTRAN (open/close).
C     JPCFMX = Nombre maximum de ConFigurations pour iMport/eXport.
C     JPIMEX =    "     "  de fichiers imp/exportables "simultanement".
C     JPDEXP = Dimension tableau Descripteurs EXPlicites d'imp/export.
C     JPDIMP =     "        "         "       IMPlicites "  "    "   .
C     JPXDAM = Nombre maXimum noms D'Articles d'imp/export en Memoire.
C     JPXCIE =    "     "     de Caracteres par nom pour Import/Export.
C     JPXMET =    "     "     "      "       "   "  avec METacaracteres.
C     JPXCCF =    "     "     "      "      des noms de ConFig. imp/exp.
C     JPTYMX =    "   de TYpes de variables valides pour Import/Export.
C
C     LPCRAY = VRAI SI L'ON TRAVAILLE SUR CRAY ( "WHENEQ" UTILISABLE )
C     LPRECH = VRAI SI L'ON PEUT UTILISER LA RECHERCHE "RAPIDE" DES NOMS
C
      INTEGER JPNBIM, JPNBIC, JPNCPN, JPLARD, JPNPDF, JPXUFM, JPNXFI
      INTEGER JPNPIA, JPNXPI, JPNXPR, JPLDOC, JPNIL, JPNCMO, JPLARC
      INTEGER JPXMET, JPRECL, JPFACX, JPLFTX, JPLFIX, JPLSPX, JPLSTX
      INTEGER JPIMEX, JPDEXP, JPDIMP, JPXDAM, JPXCIE, JPCFMX, JPXCCF
      INTEGER JPNXNA, JPNBLP, JPNAPP, JPNPIS, JPNAPX, JPNMPN, JPLARX
      INTEGER JPTYMX
C
      LOGICAL LPCRAY, LPRECH
C
      PARAMETER ( JPNCPN=LEN_HREC, JPLARD=512, JPNPDF=20, JPXUFM=100 )
      PARAMETER ( JPNXFI=300, JPFACX=120, JPNPIA=4, JPNXPR=100 )
C
C     Implementation-dependent symbolic constants (except for JPNCMO and
C     JPLARC definitions, which are there to have only one set of
C     "ifdef" in current header).
C
#if defined(RS6K) || defined(VPP) || defined(T3D) || defined(HPPA) || defined(SUN) || defined(O2000) || defined(LINUX) || defined (MNH_SX5)
      PARAMETER ( JPNBIM=64, JPNBIC=8, LPCRAY=.FALSE. )
      PARAMETER ( JPNCMO=JPNBIM/JPNBIC )
      PARAMETER ( JPLARC=JPNCMO*JPLARD )
      PARAMETER ( JPRECL=JPLARC )
#else
#if defined(DEC)
      PARAMETER ( JPNBIM=64, JPNBIC=8, LPCRAY=.FALSE. )
      PARAMETER ( JPNCMO=JPNBIM/JPNBIC )
      PARAMETER ( JPLARC=JPNCMO*JPLARD )
      PARAMETER ( JPRECL=2*JPLARD )
#else 
#if defined(HP)
      PARAMETER ( JPNBIM=32, JPNBIC=8, LPCRAY=.FALSE. )
      PARAMETER ( JPNCMO=JPNBIM/JPNBIC )
      PARAMETER ( JPLARC=JPNCMO*JPLARD )
      PARAMETER ( JPRECL=JPLARC )
#else
#if defined(SX4)
      PARAMETER ( JPNBIM=64, JPNBIC=8, LPCRAY=.FALSE. )
      PARAMETER ( JPNCMO=JPNBIM/JPNBIC )
      PARAMETER ( JPLARC=JPNCMO*JPLARD )
      PARAMETER ( JPRECL=JPLARD )
#else

C     CRAY IS DEFAULT
      PARAMETER ( JPNBIM=64, JPNBIC=8, LPCRAY=.TRUE. )
      PARAMETER ( JPNCMO=JPNBIM/JPNBIC )
      PARAMETER ( JPLARC=JPNCMO*JPLARD )
      PARAMETER ( JPRECL=JPLARC )
#endif
#endif
#endif
#endif
      PARAMETER ( JPLDOC=22, JPNIL=-999, JPXMET=2*JPNCPN, JPCFMX=4 )
      PARAMETER ( JPNXPI=JPNPIA*JPNXFI+2*JPFACX, JPXCIE=2*JPNCPN )
      PARAMETER ( JPLFTX=512, JPLFIX=128, JPLSPX=6, JPLSTX=7, JPTYMX=5 )
      PARAMETER ( JPIMEX=2, JPDEXP=10000, JPDIMP=1000, JPXDAM=1000 )
      PARAMETER ( JPNXNA=(JPLARD*JPNCMO)/JPNCPN, JPNBLP=JPLARD/2 )
      PARAMETER ( JPNAPP=(JPNBLP*(JPNXNA/JPNBLP)+JPNXNA*(JPNBLP/JPNXNA))
     S                  /(JPNXNA/JPNBLP+JPNBLP/JPNXNA), JPXCCF=16 )
      PARAMETER ( JPNPIS=JPNXPI-JPNPIA*JPNXFI, JPNAPX=JPNAPP*JPFACX )
      PARAMETER ( JPNMPN=1+(JPNCPN-1)/JPNCMO, JPLARX=JPLARD*JPFACX )
      PARAMETER ( LPRECH=(JPNCPN.EQ.(JPNMPN*JPNCMO)).AND.LPCRAY )
C
C---------- VARIABLES LOGIQUES A CHARGER ABSOLUMENT PARTOUT ------------
C
C     LMISOP = VRAI SI ON DOIT TRAVAILLER EN MODE MISE AU POINT LOGICIEL
C     LFRANC = Vrai/Faux si la messagerie doit etre en francais/anglais
C
      LOGICAL LMISOP, LFRANC
C
      COMMON /LFIMAP/ LMISOP, LFRANC
C
C-------- DESCRIPTION DE LA PARTIE DOCUMENTAIRE DU 1ER ARTICLE ---------
C
C     MOT  1 ==> LONGUEUR "PHYSIQUE" Effective DES ARTICLES (EN MOTS)
C     MOT  2 ==> LONGUEUR MAXIMUM DES NOMS D'ARTICLES (CARACTERES)
C     MOT  3 ==> "DRAPEAU" SIGNALANT SI LE FICHIER A BIEN ETE FERME
C                APRES LA DERNIERE MODIFICATION
C     MOT  4 ==> LONGUEUR DE LA PARTIE DOCUMENTAIRE DU FICHIER
C     MOT  5 ==> NOMBRE D'ARTICLES "PHYSIQUES" DANS LE FICHIER
C     MOT  6 ==>    "        "      LOGIQUES    "    "    "
C                (Y COMPRIS LES "TROUS" CREES PAR LES REECRITURES
C                 D'ARTICLES PLUS LONGUES QUE PRECEDEMMENT, ET N'AYANT
C                 PAS ENCORE PU ETRE REUTILISES, COMPTES DANS LE MOT 21)
C     MOT  7 ==> LONGUEUR MINI. DES ARTICLES LOGIQUES DE DONNEES (MOTS)
C     MOT  8 ==>    "     MAXI.  "     "         "     "    "      "
C     MOT  9 ==>    "     TOTALE "     "         "     "    "      "
C     MOT 10 ==> NOMBRE DE REECRITURES SUR PLACE (VRAIES)
C     MOT 11 ==>   "     "      "      PLUS COURTES
C     MOT 12 ==>   "     "      "       "   LONGUES
C     MOT 13 ==> NOMBRE MAXIMUM D'ARTICLES PAR PAGE OU ARTICLE D'INDEX
C     MOT 14 ==> DATE DE LA CREATION DU FICHIER (1ERE OUVERTURE)
C     MOT 15 ==> HEURE "  "    "     "     "    (  "      "    )
C     MOT 16 ==> DATE DE LA DERNIERE MODIFICATION GARANTIE (FERMETURE)
C     MOT 17 ==> HEURE "  "    "          "           "    (    "    )
C     MOT 18 ==> DATE DE LA 1ERE MODIFICATION PAS FORCEMENT GARANTIE
C     MOT 19 ==> HEURE "  "    "      "        "      "        "
C       (LES MODIFICATIONS NE SONT GARANTIES QUE SI LE MOT 4 VAUT ZERO)
C     MOT 20 ==> NOMBRE DE PAIRES D'ARTICLES D'INDEX PRERESERVES .
C     MOT 21 ==> NOMBRE DE "TROUS" CORRESP. A DES REECRITURES + LONGUES
C                ( AVANT OUVERTURE )
C     MOT 22 ==> NUMERO D'ARTICLE MAXI. DES ARTICLES PHYSIQ. DE DONNEES
C
C------ "PARAMETER" DECRIVANT LES POSITIONS DES ENTITES CI-DESSUS ------
C
      INTEGER JPLPAR, JPLMNA, JPFEAM, JPLLDO, JPNAPH, JPNALO, JPLNAL
      INTEGER JPLXAL, JPLTAL, JPNRES, JPNREC, JPNREL, JPXAPI, JPDCRE
      INTEGER JPHCRE, JPDDMG, JPHDMG, JPDMNG, JPHMNG, JPNPIR, JPNTRU
      INTEGER JPAXPD
C
      PARAMETER ( JPLPAR=1, JPLMNA=2, JPFEAM=3, JPLLDO=4, JPNAPH=5 )
      PARAMETER ( JPNALO=6, JPLNAL=7, JPLXAL=8, JPLTAL=9, JPNRES=10 )
      PARAMETER ( JPNREC=11, JPNREL=12, JPXAPI=13, JPDCRE=14 )
      PARAMETER ( JPHCRE=15, JPDDMG=16, JPHDMG=17, JPDMNG=18 )
      PARAMETER ( JPHMNG=19, JPNPIR=20, JPNTRU=21, JPAXPD=22 )
C
