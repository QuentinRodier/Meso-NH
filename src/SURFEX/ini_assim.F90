!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
      SUBROUTINE INI_ASSIM 
!     ####################
!
!!****  *INI_ASSIM * - routine to initialize the module MODD_ASSIM
!!
!!    PURPOSE
!!    -------
!       The purpose of this routine is to initialize variables in module MODD_ASSIM.
!      
!
!!**  METHOD
!!    ------
!!      The various constants are set to their numerical values 
!!     
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!      Module MODD_ASSIM     : contains constants for soil assimilation
!!
!! 
!!    AUTHOR
!!    ------
!!  	J.-F. Mahfouf       * Meteo France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    21/05/09 
!!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_ASSIM
USE MODD_CSTS,       ONLY : XRHOLW, XPI 
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
INTEGER :: J
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!  
!**-----------------------------------------------------------------------
!**  - 1 - Initialisations, diagnostics.
!**        ----------------------------

!*   1.1  Constantes
!
! NECHGU : ECHEANCE DU GUESS EN HEURES (0 A 30).
!
 IF (LHOOK) CALL DR_HOOK('INI_ASSIM',0,ZHOOK_HANDLE)
 NECHGU = 6
!
!
!LFGEL   : CLE D'APPEL DU GEL DE L'EAU DU SOL AVEC ISBA (LSOLV)
!        : KEY FOR SOIL FREEZING WITH ISBA (LSOLV)
!          ( ACSOL, ACDROV)
 LFGEL = .TRUE.
 LCLIM = .TRUE.
!
!  Characteristics of ice and sea 
!SODELX(0:9): DISCRETISATION VERTICALE DU SOL (MAXI 10 COUCHES)
!           : SOIL VERTICAL DISCRETIZATION (MAX 10 LAYERS)
!GCONV      : FACTEUR DE CHANGEMENT D'UNITE (MASSE VOL. DE L'EAU LIQUIDE)
!RD1        : EPAISSEUR DU RESERVOIR SUPERFICIEL.
!           : UPPER RESERVOIR DEPTH.
!RTINER     : RAPPORT DES INERTIES THERMIQUES PROFONDEUR/SURFACE.
!           : RATIO OF THE DEEP/SURFACE THERMAL INERTIAS.
!WCRIN      : QUANTITE CRITIQUE POUR LA NEIGE MI-COUVRANTE.
!           : CRITICAL TRANSITION VALUE FOR SNOW DEPTH (HALF-COVER).
!WPMX       : VALEUR MAXIMUN POUR LE CONTENU EN EAU EN PROFONDEUR.
!               : MAXIMUM VALUE FOR DEEP SOIL WATER CONTENT.
!WSMX       : VALEUR MAXIMUN POUR LE CONTENU EN EAU DE SURFACE.
!           : MAXIMUM VALUE FOR SURFACE SOIL WATER CONTENT.
!NTVGLA     : INDICE DE VEGETATION SUR GLACE.
!TMERGL     : TEMPERATURE DE FONTE DE LA GLACE DE MER.
!           : MELTING TEMPERATURE OF FLOATING ICE.
!RZHZ0G     : RAPPORT LONGUEUR DE RUGOSITE THERMIQUE SUR LONGUEUR
!             RUGOSITE DYNAMIQUE DE LA BANQUISE
 SODELX(0)=1.0/SQRT(1.0+2.0*XPI)
 DO J=1,9
   SODELX(J)=SODELX(J-1)*2.0*XPI
 ENDDO
 RD1    = 1.E-2
 RTINER = 5.
 WCRIN  = 10.
 WPMX   = 100.
 WSMX   = 20.
 NTVGLA = 2
 TMERGL = 271.23 
 RZHZ0G = 1.0 
!
!nactex, canali
!  RCLIMCA : coef. de rappel vers la climatologie des champs de surface
!  RCLISST : coef. de rappel vers la climatologie de SST
!  NSEAICE : utilsation limite glace SSM/I (et nb de jours de retard possible)
!  RSNSA   : Coefficient pour l'analyse de neige
!  RSNSB   : Coefficient pour l'analyse de neige
!  RWPIA   : Coefficient pour l'analyse de l'eau gelee
!  RWPIB   : Coefficient pour l'analyse de l'eau gelee
!
!RCLIMCA=0.045
 RCLIMCA = 0. ! no climatology relaxation
!RCLISST=0.05 ! as in the original cacsts
 RCLISST = 0.05
 NSEAICE = 0
 RWPIA   = 0.025
 RWPIB   = 2.  
 RSNSA   = 0.025
 RSNSB   = 1.0 
!
!yomcli
!  SALBN,SALBX,SALBM,SALBG,SALBB,SALBD : albedo
!  SEMIN,SEMIX,SEMIM,SEMIG,SEMIB,SEMID : emissivity
!  (minimum,maximum,sea,ice,sea ice,desert)
!  SZZ0N,SZZ0M,SZZ0B,SZZ0U,SZZ0D : roughness length
!  (minimum,sea,sea-ice,urban areas,desert)
!
 SALBM = 0.07
 SALBB = 0.65
 SEMIM = 0.96
 SEMIB = 0.97
 SZZ0B = 0.001
!
! ecarts-type d'erreurs d'observation
! T2m  -> SIGT2MO
! Hu2m -> SIGH2MO
! LSGOBS : activee si l'une des valeurs ci-dessus differe de la reference
!
! seuils sur la force du vent a 10m , la duree du jour , les precipitations,
! ws/wfc, evaporation en (mm/jour), presence de glace
!   V10MX , MINDJ , SPRECIP , SWFC  , SEVAP , SICE , SMU0
!
! cles et coefficients de reglage
!     LIMVEG : activation de la limitation a wp > veg*wwilt
!     LHUMID : activation de la clef LIMVEG  ou du seuil SWFC limitee
!     LISSEW : activation du lissage des increments de wp (si NLISSEW=3)
!     NLISSEW: nombre de reseaux anterieurs utilisables pour le lissage
!     ANEBUL , NNEBUL : dependance par rapport a la nebulosite
!     SNEIGT , NNEIGT : dependance par rapport a la neige (T)
!     SNEIGW , NNEIGW : dependance par rapport a la neige (w)
! coefficients pour le retrait du biais de T2m et Hu2m
!     SCOEFT , SCOEFH : =0. pas de retrait / =1. increment mis a zero

! champs d'increments pour le lissage (transfert dans GPPBUF de RINCW)
!     RINCW(jrof,j)  j=1/2/3 pour reseau courant - 06h/12h/18h
!     CINCW(j) : nom des champs d'increments dans les fichiers ARPEGE
!
! biais moyen
!     RBIAT(jrof) : biais moyen de temperature }  tableaux supprimes et mis dans
!     RBIAH(jrof) : biais moyen d'humidite     }   GPPBUF (adresse: MCANRI0)
!
! reglage du rappel clim
!     RCLIMTS : coef. multiplicateur de RCLIMCA pour Ts
!     RCLIMTP : coef. multiplicateur de RCLIMCA pour Tp
!     RCLIMWS : coef. multiplicateur de RCLIMCA pour Ws
!     RCLIMWP : coef. multiplicateur de RCLIMCA pour Wp
!     RCLIMN  : impact de la fraction de neige
!     RCLIMV  : coef. multiplicateur sur desert pour Ws, Wp
!
!NACVEG
!
!***
!*** conditions d'analyse
!***  LIMVEG  : si wp >= veg*wwilt
!***  MINDJ   : duree du jour minimale (heure)
!***  V10MX   : seuil sur le module du vent (analyse) a 10m
!***  SPRECIP : seuil sur les precipitations (prevues) en mm
!***  SEVAP   : seuil l'evaporation inst. en (mm/jour)
!***  SICE    : seuil sur la quantite totale de glace (Kg/m2)
!***  SMU0    : seuil utilisant l'angle zenithal solaire
!***  SWFC    : seuil sur ws/wfc (pas d'analyse de ws si ws > SWFC*wfc)
!*** ponderations
!***  LHUMID  : humidification seulement si wp < veg*wwilt
!***          : assechement seulement si ws > SWFC*wfc (pour ws)
!***  LISSEW  : lissage des increments de wp (moyenne glissante sur 24h)
!***  SIGT2MO : ecart-type d'erreur "d'observation" sur T2m
!***  SIGH2MO : ecart-type d'erreur "d'observation" sur Hu2m
!***  ANEBUL  : reduction maximale par la nebulosite
!***  NNEBUL  : puissance de la nebulosite prise en compte
!***            nebulosite moyenne neb <--> poids 1-ANEBUL*neb**NNEBUL
!***  SNEIGT  : seuil sur la fraction de la maille recouverte de neige (T)
!***  NNEIGT  : coefficient de ponderation (T)
!***  SNEIGW  : seuil sur la fraction de la maille recouverte de neige (w)
!***  NNEIGW  : coefficient de ponderation (w)
!*** retrait du biais sur T2m et Hu2m
!***  SCOEFT  : coefficient pour le retrait du biais de temperature
!***  SCOEFH  : coefficient pour le retrait du biais d'humidite
!***            =0 pas de retrait. =1 increment mis a zero
!*** reglage du rappel clim
!***  RCLIMTS : coef. multiplicateur de RCLIMCA pour Ts
!***  RCLIMTP : coef. multiplicateur de RCLIMCA pour Tp
!***  RCLIMWS : coef. multiplicateur de RCLIMCA pour Ws
!***  RCLIMWP : coef. multiplicateur de RCLIMCA pour Wp
!***  RCLIMN  : impact de la fraction de neige
!***  RCLIMV  : coef. multiplicateur sur desert pour Ws, Wp
!*** lissage spatial du SWI et modification de Wp a partir du SWI lisse
!***  L_SM_WP : lissage spatial du SWI (Soil Wetness Index) et modif Wp
!***  NR_SM_WP: nombre d'appel de la routine de lissage spatial du SWI
!***  RA_SM_WP: distance caracteristique du lissage spatial (en metres)
!***
!***---------------------------------------------------------------------
!
 ANEBUL   = 0.75
 LHUMID   = .TRUE.
 LIMVEG   = .TRUE.
 LLDHMT   = .FALSE.
 LISSEW   = .FALSE.
 LOBSWG   = .TRUE.   ! assimilation of WG
 LOBS2M   = .FALSE.  ! assimilation of T2M + RH2M (with WG)
 LAROME   = .TRUE. !  if AROME model 
 LPRINT   = .TRUE. !  for additional prints
 NLISSEW  = 0
 MINDJ    = 6
 NNEBUL   = 1
 NNEIGT   = 0
 NNEIGW   = 1
 RCLIMN   = 0.
 RCLIMTP  = 0.
 RCLIMTS  = 0.
 RCLIMV   = 1.
 RCLIMWP  = 0.1
 RCLIMWS  = 0.
 SCOEFH   = 0.
 SCOEFT   = 0.
 SEVAP    = 0.
 SICE     = 5
 SIGH2MO  = 0.1  ! observation error for HU2m
 SIGT2MO  = 1.0  ! observation error for T2m
 SIGWGO   = 0.06 ! observation error for WG
 SIGWGB   = 0.06 ! background error for WG
 SIGW2B   = 0.03 ! background error for W2
 SNEIGT   = 1.
 SNEIGW   = 0.
 SPRECIP  = .3
 SPRECIP2 = 4.0
 SWFC     = 1.0
 V10MX    = 10.
 SMU0     = 7.
 RTHR_QC  = 3.0
 SIGWGO_MAX = 6.0 ! maximum acceptable WG obs error (%)
 RSCAL_JAC  = 4.0  ! to modify the "effective" assimilation window
!
! PARAMETERS TO SWITCH ON CASMSWI - SPATIAL SMOOTHING OF SWI (SOIL WETNESS INDEX)
! THEN CHANGING OF Wp ( TOTAL SOIL WATER CONTENT) IN CANARI OI. 
! CASMSWI IS CALLED BY CANARI
!
 L_SM_WP  = .FALSE.
 NR_SM_WP = 1
 RA_SM_WP = 5000.0
!
! Standard deviation of background error
!
 SIGHP1 = 15.E-2
!
! Standard deviation of observation errors (referencce)
!
 SIGT2MR = 1.0
 SIGH2MR = 10.E-2
!
! Soil textural properties (reference = loam)
!
 RSABR   = 43.
 RARGR   = 18.
 GWFC    = 89.0467E-3
 EWFC    = 0.35
 GWWILT  = 37.1342E-3
 EWWILT  = 0.5
 G1WSAT  = -1.08E-3
 G2WSAT  = 494.31E-3
 ADWR    = GWFC*(RARGR**EWFC) - GWWILT*(RARGR**EWWILT)
!
! Low threshold values
!
 REPS1   = 1.E-3 
 REPS2   = 1.E-1
 REPS3   = 1.E-13
!
! Astronomical and time constants
!
 REPSM   = 0.409093                ! obliquity
 IDJ     = 12.0                    ! day duration
 RCDTR   = 24./360.
 ! ITRAD (half assimilation window in sec) --> dependant from NECHGU --> set in OI_MAIN
IF (LHOOK) CALL DR_HOOK('INI_ASSIM',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE INI_ASSIM 
