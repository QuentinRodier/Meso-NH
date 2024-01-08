!     #########################
SUBROUTINE READ_CSVDATA_ARCHI_TEB(BDD, HPROGRAM, HFILE)
  !     #########################
  !
  !!
  !!    PURPOSE
  !!    -------
  !!
  !!    METHOD
  !!    ------
  !!
  !!
  !!    EXTERNAL
  !!    --------
  !!
  !!    IMPLICIT ARGUMENTS
  !!    ------------------
  !!
  !!    REFERENCE
  !!    ---------
  !!
  !!    AUTHOR
  !!    ------
  !!
  !!    V. Masson        Meteo-France
  !!
  !!    MODIFICATION
  !!    ------------
  !!
  !!    Original     05/2012 
  !     R. Schoetter 09/2015 Major changes in order to read MApUCE table
  !     M. Goret     08/2017 Add a 20cm mass in all buildings
  !
  !----------------------------------------------------------------------------
  !
  !*    0.     DECLARATIONS
  !            -----------
  !
  USE MODD_BLD_DESCRIPTION_n, ONLY : BLD_DESC_t
  USE MODD_SURF_PAR, ONLY : XUNDEF, NUNDEF
  !
  USE MODI_OPEN_NAMELIST
  USE MODI_CLOSE_NAMELIST
  !
  USE MODI_GET_LUOUT
  USE MODI_ABOR1_SFX
  !
  USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
  USE PARKIND1  ,ONLY : JPRB
  USE MODI_BLDCODE
  !
  IMPLICIT NONE
  !
  !*    0.1    Declaration of arguments
  !            ------------------------
  !
  TYPE(BLD_DESC_t), INTENT(INOUT) :: BDD
  CHARACTER(LEN=6),  INTENT(IN) :: HPROGRAM
  CHARACTER(LEN=28), INTENT(IN) :: HFILE    ! file to read
  !
  !*    0.2    Declaration of local variables
  !      ------------------------------
  !
  INTEGER :: ILUNAM  ! Unit of the file
  INTEGER :: ILUOUT  ! Unit of the output listing file
  INTEGER :: NFIELD  ! Number of fields to be read
  INTEGER :: NENTRY  ! Number of entries to read
  INTEGER :: NCTLVA  ! Control variable
  INTEGER :: NMATTP  ! Number of material types
  INTEGER :: NCMATN  ! Number of material characteristics 
  INTEGER :: NCVITN  ! Number of glazing characteristics 
  INTEGER :: ICTMAT  ! Control count of material types
  INTEGER :: ICTVIT  ! Control count of window types
  !
  INTEGER :: NWINDTP ! Number of window types
  INTEGER :: NSHADTP ! Number of shading device types
  INTEGER :: NGFTYPE ! Number of ground floor types
  INTEGER :: NIFTYPE ! Number of intermediate floor types
  INTEGER :: NWSTRTP ! Number of wall structure types
  INTEGER :: NWISOTP ! Number of wall insulation types
  INTEGER :: NWRINTP ! Number of wall inner coating types
  INTEGER :: NWREXTP ! Number of wall external coating types
  INTEGER :: NRSTRTP ! Number of roof structure types
  INTEGER :: NRISOTP ! Number of roof insulation types
  INTEGER :: NRRINTP ! Number of roof inner coating types
  INTEGER :: NRREXTP ! Number of roof external coating types
  INTEGER :: NCOVRTP ! Number of road cover types
  INTEGER :: NMECHTP ! Number of mecanical ventilation types
  !
  INTEGER :: IPOSNAME ! Position of material name
  INTEGER :: IPOSDENS ! Position of material density
  INTEGER :: IPOSHCON ! Position of material heat conductivity
  INTEGER :: IPOSHCAP ! Position of material heat capcity
  INTEGER :: IPOSALBE ! Position of material albedo
  INTEGER :: IPOSEMIS ! Position of material emissivity
  INTEGER :: IPOSUVAL ! Position of window u-value
  INTEGER :: IPOSSHGC ! Position of window solar heat gain coefficient
  !
  INTEGER :: KK,LL,MM,IL,II ! Loop indices
  INTEGER :: IR          ! Number of lines read
  !
  INTEGER, ALLOCATABLE :: ITEBPOS(:) ! Indices of TEB input data in .csv file
  INTEGER, ALLOCATABLE :: IIDEPOS(:) ! Indices of islet/usage/age/territory identifiers in .csv file
  !
  REAL :: ZAIRTIGHTN ! Airtightness [AC/h at 50 Pa]
  REAL :: ZGLAZFRACT ! Glazing ratio [1]
  REAL :: ZWALLSTRDP ! Thickness of wall structure [m]
  REAL :: ZWALLISODP ! Thickness of wall insulation [m]
  REAL :: ZWALLRINDP ! Thickness of wall internal coating [m]
  REAL :: ZWALLREXDP ! Thickness of wall external coating [m]
  REAL :: ZROOFSTRDP ! Thickness of roof structure [m]
  REAL :: ZROOFISODP ! Thickness of roof insulation [m]
  REAL :: ZROOFRINDP ! Thickness of roof internal coating [m]
  REAL :: ZROOFREXDP ! Thickness of roof external coating [m]
  REAL :: ZROADCOVDP ! Thickness of road external cover [m]
  REAL :: ZMECHRATE  ! Air exchange rate due to mecanical ventilation [1/h]
  !
  REAL :: ZDENSMATRD ! Material density [kg/m^3]
  REAL :: ZHCONMATRD ! Material heat conductivity [W/mK]
  REAL :: ZHCAPMATRD ! Material heat capacity [J/kgK]
  REAL :: ZALBEMATRD ! Material albedo [1]
  REAL :: ZEMISMATRD ! Material emissivity [1]
  REAL :: ZUVALVITRD ! Glazing U-Value [W/m²K]
  REAL :: ZSHGCVITRD ! Glazing solar heat gain coefficient [1]
  !
  REAL :: ZEPRDC     ! Thickness of ground floor layer [m]
  REAL :: ZEPPIN     ! Thickness of intermediate floor layer [m]
  !
  REAL :: ZGREENROOF   ! Fraction of green roofs [1]
  REAL :: ZFRAC_PANEL  ! Fraction of solar panels [1]
  REAL :: ZALB_PANEL   ! Albedo of solar panels [1]
  REAL :: ZEMIS_PANEL  ! Emissivity of solar panels [1]
  REAL :: ZEFF_PANEL   ! Efficiency of solar panels [1]
  !
  REAL :: DUMMY      ! Dummy variable
  !
  REAL, ALLOCATABLE :: ZDNSITY(:) ! Material density vector [kg/m^3]
  REAL, ALLOCATABLE :: ZHEACON(:) ! Material heat conductivity vector [W/mK]
  REAL, ALLOCATABLE :: ZHEACAP(:) ! Material heat capacity vector [J/kgK]
  REAL, ALLOCATABLE :: ZALBEDO(:) ! Material albedo vector [1]
  REAL, ALLOCATABLE :: ZEMISSI(:) ! Material emissivity vector [1]
  REAL, ALLOCATABLE :: ZUVALUE(:) ! Glazing U-Value vector [W/m²K]
  REAL, ALLOCATABLE :: ZSHGCCO(:) ! Glazing solar heat gain coefficient vector [1]
  !
  REAL, ALLOCATABLE :: ZAUAL(:)   ! Auxiliary vector for albedo
  REAL, ALLOCATABLE :: ZAUEM(:)   ! Auxiliary vector for emissivity
  !
  CHARACTER(LEN=2000) :: YSTRING ! One line of .csv file
  CHARACTER(LEN=2000) :: YSTRAUX ! Auxiliairy string
  !
  CHARACTER(LEN=50) :: YPERI ! Period
  CHARACTER(LEN=50) :: YISLE ! Islet
  CHARACTER(LEN=50) :: YUSAG ! Building usage
  CHARACTER(LEN=50) :: YTERR ! Building material territory
  CHARACTER(LEN=50) :: YWIND ! Window type
  CHARACTER(LEN=50) :: YSHAD ! Shading type
  CHARACTER(LEN=50) :: YGRFL ! Ground floor type
  CHARACTER(LEN=50) :: YIMFL ! Intermediate floor type
  CHARACTER(LEN=50) :: YWSTR ! Wall structure type
  CHARACTER(LEN=50) :: YWISO ! Wall insulation type
  CHARACTER(LEN=46) :: YWISA ! Wall insulation type, after remowing information about location  
  CHARACTER(LEN=50) :: YWRIN ! Wall internal coating type
  CHARACTER(LEN=50) :: YWREX ! Wall external coating type
  CHARACTER(LEN=50) :: YRSTR ! Roof structure type
  CHARACTER(LEN=50) :: YRISO ! Roof insulation type
  CHARACTER(LEN=50) :: YRISA ! Roof insulation type, after removing information about position
  CHARACTER(LEN=50) :: YRRIN ! Roof internal coating type
  CHARACTER(LEN=50) :: YRREX ! Roof external coating type
  CHARACTER(LEN=50) :: YCOVR ! Road external cover type
  CHARACTER(LEN=50) :: YMECH ! Mecanical ventilation type
  CHARACTER(LEN=50) :: YSAUX ! Auxiliary string
  !
  CHARACTER(LEN=50) :: YNAMEMATRD ! Material name
  CHARACTER(LEN=50) :: YDENSMATRD ! Material density
  CHARACTER(LEN=50) :: YHCONMATRD ! Material heat conductivity
  CHARACTER(LEN=50) :: YHCAPMATRD ! Material heat capacity
  CHARACTER(LEN=50) :: YALBEMATRD ! Material albedo
  CHARACTER(LEN=50) :: YEMISMATRD ! Material emissivity
  CHARACTER(LEN=50) :: YNAMEVITRD ! Window type
  CHARACTER(LEN=50) :: YUVALVITRD ! Window U-Value
  CHARACTER(LEN=50) :: YSHGCVITRD ! Window solar heat gain coefficient
  !
  CHARACTER(LEN=50), ALLOCATABLE :: YISLETP(:) ! Building types
  CHARACTER(LEN=50), ALLOCATABLE :: YPERITP(:) ! Building construction periods
  CHARACTER(LEN=50), ALLOCATABLE :: YUSAGTP(:) ! Building usage types
  CHARACTER(LEN=50), ALLOCATABLE :: YTERRTP(:) ! Construction material territory types
  !
  CHARACTER(LEN=50), ALLOCATABLE :: YWINDTP(:) ! Window type types
  CHARACTER(LEN=50), ALLOCATABLE :: YSHADTP(:) ! Shading type types
  CHARACTER(LEN=50), ALLOCATABLE :: YGRFLTP(:) ! Ground floor types
  CHARACTER(LEN=50), ALLOCATABLE :: YIMFLTP(:) ! Intermediate floor types
  CHARACTER(LEN=50), ALLOCATABLE :: YWSTRTP(:) ! Wall structure types
  CHARACTER(LEN=50), ALLOCATABLE :: YWISOTP(:) ! Wall insulation types
  CHARACTER(LEN=50), ALLOCATABLE :: YWREXTP(:) ! Wall external coating types
  CHARACTER(LEN=50), ALLOCATABLE :: YWRINTP(:) ! Wall internal cover types
  CHARACTER(LEN=50), ALLOCATABLE :: YRSTRTP(:) ! Roof structure types
  CHARACTER(LEN=50), ALLOCATABLE :: YRISOTP(:) ! Roof insulation types
  CHARACTER(LEN=50), ALLOCATABLE :: YRREXTP(:) ! Roof external coating types
  CHARACTER(LEN=50), ALLOCATABLE :: YRRINTP(:) ! Roof internal cover types
  CHARACTER(LEN=50), ALLOCATABLE :: YCOVRTP(:) ! Road external cover types
  CHARACTER(LEN=50), ALLOCATABLE :: YMECHTP(:) ! Mecanical ventilation types
  !
  CHARACTER(LEN=50), ALLOCATABLE :: YCMATN(:)  ! Names of material characteristics
  CHARACTER(LEN=50), ALLOCATABLE :: YCVITN(:)  ! Names of glazing characteristics
  CHARACTER(LEN=50), ALLOCATABLE :: YMTP(:)    ! Material types
  CHARACTER(LEN=50), ALLOCATABLE :: YNAMES(:)  ! Names of fields to be read
  CHARACTER(LEN=50), ALLOCATABLE :: YNAMEID(:) ! Names of identifiers
  CHARACTER(LEN=50), ALLOCATABLE :: YREADD(:)  ! Data read from .csv file
  CHARACTER(LEN=50), ALLOCATABLE :: YREADID(:) ! Data read from .csv file
  !
  REAL(KIND=JPRB) :: ZHOOK_HANDLE
  !
  ! -------------------------------------------------------------------------------
  !
  IF (LHOOK) CALL DR_HOOK('READ_CSVDATA_ARCHI_TEB',0,ZHOOK_HANDLE)
  !-------------------------------------------------------------------------------
  IF (LEN_TRIM(HFILE)==0) THEN
     IF (LHOOK) CALL DR_HOOK('READ_CSVDATA_ARCHI_TEB',1,ZHOOK_HANDLE)
     RETURN
  END IF
  !
  CALL GET_LUOUT(HPROGRAM,ILUOUT)
  !
  ! ###############################################################################
  ! Read definitions in .csv file
  ! ###############################################################################
  !
  CALL READ_DEF_IN_CSV(HPROGRAM,ILUNAM,HFILE,"LISTE DES ENTREES",NENTRY,YNAMEID)
  ALLOCATE(IIDEPOS(1:NENTRY))
  ALLOCATE(YREADID(1:NENTRY))
  !
  CALL READ_DEF_IN_CSV(HPROGRAM,ILUNAM,HFILE,"LISTE DES CHAMPS A LIRE",NFIELD,YNAMES)
  ALLOCATE(YREADD (1:NFIELD))
  ALLOCATE(ITEBPOS(1:NFIELD))
  !
  CALL READ_DEF_IN_CSV(HPROGRAM,ILUNAM,HFILE,"DEFINITION DES TYPOLOGIES DES ILOTS URBAINS",BDD%NDESC_BLD,YISLETP)
  CALL READ_DEF_IN_CSV(HPROGRAM,ILUNAM,HFILE,"DEFINITION DES USAGES",BDD%NDESC_USE,YUSAGTP)
  CALL READ_DEF_IN_CSV(HPROGRAM,ILUNAM,HFILE,"DEFINITION DES TERRITOIRES",BDD%NDESC_TER,YTERRTP)
  CALL READ_DEF_IN_CSV(HPROGRAM,ILUNAM,HFILE,"DEFINITION DES PERIODES DE CONSTRUCTION",BDD%NDESC_AGE,YPERITP)
  CALL READ_DEF_IN_CSV(HPROGRAM,ILUNAM,HFILE,"DEFINITION DES TYPES DE VITRAGE",NWINDTP,YWINDTP)
  CALL READ_DEF_IN_CSV(HPROGRAM,ILUNAM,HFILE,"DEFINITION DES PROTECTIONS SOLAIRES",NSHADTP,YSHADTP)
  CALL READ_DEF_IN_CSV(HPROGRAM,ILUNAM,HFILE,"DEFINITION DES TYPES DE PLANCHER RDC",NGFTYPE,YGRFLTP)
  CALL READ_DEF_IN_CSV(HPROGRAM,ILUNAM,HFILE,"DEFINITION DES TYPES DE PLANCHER ITM",NIFTYPE,YIMFLTP)
  CALL READ_DEF_IN_CSV(HPROGRAM,ILUNAM,HFILE,"DEFINITION DES TYPES DE COUVERTURE DE ROUTE",NCOVRTP,YCOVRTP)
  CALL READ_DEF_IN_CSV(HPROGRAM,ILUNAM,HFILE,"DEFINITION DES TYPES DE VENTILATION MECANIQUE",NMECHTP,YMECHTP)
  CALL READ_DEF_IN_CSV(HPROGRAM,ILUNAM,HFILE,"DEFINITION DES PORTEURS DU MUR",NWSTRTP,YWSTRTP)
  CALL READ_DEF_IN_CSV(HPROGRAM,ILUNAM,HFILE,"DEFINITION DES PORTEURS DU TOIT",NRSTRTP,YRSTRTP)
  CALL READ_DEF_IN_CSV(HPROGRAM,ILUNAM,HFILE,"DEFINITION DES ISOLANTS DU MUR",NWISOTP,YWISOTP)
  CALL READ_DEF_IN_CSV(HPROGRAM,ILUNAM,HFILE,"DEFINITION DES ISOLANTS DU TOIT",NRISOTP,YRISOTP)
  CALL READ_DEF_IN_CSV(HPROGRAM,ILUNAM,HFILE,"DEFINITION DES REV INT DU MUR",NWRINTP,YWRINTP)
  CALL READ_DEF_IN_CSV(HPROGRAM,ILUNAM,HFILE,"DEFINITION DES REV INT DU TOIT",NRRINTP,YRRINTP)
  CALL READ_DEF_IN_CSV(HPROGRAM,ILUNAM,HFILE,"DEFINITION DES REV EXT DU MUR",NWREXTP,YWREXTP)
  CALL READ_DEF_IN_CSV(HPROGRAM,ILUNAM,HFILE,"DEFINITION DES REV EXT DU TOIT",NRREXTP,YRREXTP)
  CALL READ_DEF_IN_CSV(HPROGRAM,ILUNAM,HFILE,"LISTE DES MATERIAUX",NMATTP,YMTP)
  CALL READ_DEF_IN_CSV(HPROGRAM,ILUNAM,HFILE,"LISTE DES CARACTERISTIQUES DES MATERIAUX",NCMATN,YCMATN)
  CALL READ_DEF_IN_CSV(HPROGRAM,ILUNAM,HFILE,"LISTE DES CARACTERISTIQUES DU VITRAGE",NCVITN,YCVITN)
  !
  ! Write characters containing class names
  !
  ALLOCATE(BDD%YDESC_TYPNAME(BDD%NDESC_BLD))
  ALLOCATE(BDD%YDESC_USENAME(BDD%NDESC_USE))
  ALLOCATE(BDD%YDESC_AGENAME(BDD%NDESC_AGE))
  ALLOCATE(BDD%YDESC_TERNAME(BDD%NDESC_TER))
  !
  BDD%YDESC_TYPNAME=YISLETP
  BDD%YDESC_USENAME=YUSAGTP
  BDD%YDESC_AGENAME=YPERITP
  BDD%YDESC_TERNAME=YTERRTP
  !
  ! Find the positions of MApUCE building types in input data
  !
  DO II=1,BDD%NDESC_BLD
     IF     (TRIM(BDD%YDESC_TYPNAME(II)).EQ."PD") THEN
        BDD%NDESC_POS_TYP_PD   = II
     ELSEIF (TRIM(BDD%YDESC_TYPNAME(II)).EQ."PSC") THEN
        BDD%NDESC_POS_TYP_PSC  = II
     ELSEIF (TRIM(BDD%YDESC_TYPNAME(II)).EQ."PCIO") THEN
        BDD%NDESC_POS_TYP_PCIO = II
     ELSEIF (TRIM(BDD%YDESC_TYPNAME(II)).EQ."PCIF") THEN
        BDD%NDESC_POS_TYP_PCIF = II
     ELSEIF (TRIM(BDD%YDESC_TYPNAME(II)).EQ."ID") THEN
        BDD%NDESC_POS_TYP_ID   = II
     ELSEIF (TRIM(BDD%YDESC_TYPNAME(II)).EQ."ICIO") THEN
        BDD%NDESC_POS_TYP_ICIO = II
     ELSEIF (TRIM(BDD%YDESC_TYPNAME(II)).EQ."ICIF") THEN
        BDD%NDESC_POS_TYP_ICIF = II
     ELSEIF (TRIM(BDD%YDESC_TYPNAME(II)).EQ."BGH") THEN
        BDD%NDESC_POS_TYP_BGH  = II
     ELSEIF (TRIM(BDD%YDESC_TYPNAME(II)).EQ."BA") THEN
        BDD%NDESC_POS_TYP_BA   = II
     ELSEIF (TRIM(BDD%YDESC_TYPNAME(II)).EQ."LOCAL") THEN
        BDD%NDESC_POS_TYP_LOCA = II      
     ELSE
        CALL ABOR1_SFX("Building type not found")
     ENDIF
  ENDDO
  !
  ! Find the positions of MApUCE building uses in input data
  !
  DO II=1,BDD%NDESC_USE
     IF     (TRIM(BDD%YDESC_USENAME(II)).EQ."BATIMENT AGRICOLE") THEN
        BDD%NDESC_POS_USE_AGR = II
     ELSEIF (TRIM(BDD%YDESC_USENAME(II)).EQ."CHATEAU") THEN
        BDD%NDESC_POS_USE_CHA = II
     ELSEIF (TRIM(BDD%YDESC_USENAME(II)).EQ."COMMERCE") THEN
        BDD%NDESC_POS_USE_COM = II
     ELSEIF (TRIM(BDD%YDESC_USENAME(II)).EQ."HABITAT COLLECTIF") THEN
        BDD%NDESC_POS_USE_HAC = II
     ELSEIF (TRIM(BDD%YDESC_USENAME(II)).EQ."HABITAT INDIVIDUEL") THEN
        BDD%NDESC_POS_USE_HAI = II
     ELSEIF (TRIM(BDD%YDESC_USENAME(II)).EQ."BATIMENT INDUSTRIEL") THEN
        BDD%NDESC_POS_USE_IND = II
     ELSEIF (TRIM(BDD%YDESC_USENAME(II)).EQ."LOCAL NON CHAUFFE") THEN
        BDD%NDESC_POS_USE_LNC = II
     ELSEIF (TRIM(BDD%YDESC_USENAME(II)).EQ."BATIMENT RELIGIEUX") THEN
        BDD%NDESC_POS_USE_REL = II
     ELSEIF (TRIM(BDD%YDESC_USENAME(II)).EQ."BATIMENT DE SANTE") THEN
        BDD%NDESC_POS_USE_SAN = II
     ELSEIF (TRIM(BDD%YDESC_USENAME(II)).EQ."BATIMENT D ENSEIGNEMENT") THEN
        BDD%NDESC_POS_USE_ENS = II
     ELSEIF (TRIM(BDD%YDESC_USENAME(II)).EQ."SERRE AGRICOLE") THEN
        BDD%NDESC_POS_USE_SER = II
     ELSEIF (TRIM(BDD%YDESC_USENAME(II)).EQ."BATIMENT SPORTIF") THEN
        BDD%NDESC_POS_USE_SPO = II
     ELSEIF (TRIM(BDD%YDESC_USENAME(II)).EQ."TERTIAIRE") THEN
        BDD%NDESC_POS_USE_TER = II       
     ELSE
        CALL ABOR1_SFX("Building use not found")
     ENDIF
  ENDDO
  !
  ! Find the positions of MApUCE building contruction periods in input data
  !
  DO II=1,BDD%NDESC_AGE
     IF     (TRIM(BDD%YDESC_AGENAME(II)).EQ."P1") THEN
        BDD%NDESC_POS_AGE_P1 = II
     ELSEIF (TRIM(BDD%YDESC_AGENAME(II)).EQ."P2") THEN
        BDD%NDESC_POS_AGE_P2 = II
     ELSEIF (TRIM(BDD%YDESC_AGENAME(II)).EQ."P3") THEN
        BDD%NDESC_POS_AGE_P3 = II
     ELSEIF (TRIM(BDD%YDESC_AGENAME(II)).EQ."P4") THEN
        BDD%NDESC_POS_AGE_P4 = II
     ELSEIF (TRIM(BDD%YDESC_AGENAME(II)).EQ."P5") THEN
        BDD%NDESC_POS_AGE_P5 = II
     ELSEIF (TRIM(BDD%YDESC_AGENAME(II)).EQ."P6") THEN
        BDD%NDESC_POS_AGE_P6 = II
     ELSEIF (TRIM(BDD%YDESC_AGENAME(II)).EQ."P7") THEN
        BDD%NDESC_POS_AGE_P7 = II
     ELSE
        CALL ABOR1_SFX("Building construction period not found")
     ENDIF  
  ENDDO
  !
  ! Find the positions of MApUCE default material territory
  !
  BDD%NDESC_POS_PX_DEFAULT=-NUNDEF
  DO II=1,BDD%NDESC_TER
     IF (TRIM(BDD%YDESC_TERNAME(II)).EQ."FRANCE") THEN
        BDD%NDESC_POS_PX_DEFAULT  = II
     ENDIF  
  ENDDO
  IF (BDD%NDESC_POS_PX_DEFAULT.LT.0) CALL ABOR1_SFX ("Default material territory not found")
  !
  ! #########################################################################
  ! Read material characteristics
  ! #########################################################################
  !
  ALLOCATE(ZDNSITY(1:NMATTP))
  ALLOCATE(ZHEACON(1:NMATTP))
  ALLOCATE(ZHEACAP(1:NMATTP))
  ALLOCATE(ZALBEDO(1:NMATTP))
  ALLOCATE(ZEMISSI(1:NMATTP))
  !
  ZDNSITY(:)=-XUNDEF
  ZHEACON(:)=-XUNDEF
  ZHEACAP(:)=-XUNDEF
  ZALBEDO(:)=-XUNDEF
  ZEMISSI(:)=-XUNDEF
  !
  CALL OPEN_NAMELIST(HPROGRAM,ILUNAM,HFILE)
  !
  NCTLVA=0
  !
  DO LL=1,1000
     !
     READ (ILUNAM,END=9999,FMT='(A2000)') YSTRING
     !
     IF (INDEX(YSTRING,"FAMILLE DE MATERIAUX").GT.0) THEN 
        !
        NCTLVA=1
        !
        ! Find positions of material characteristics in string
        !
        IPOSNAME=-NUNDEF
        IPOSDENS=-NUNDEF
        IPOSHCON=-NUNDEF
        IPOSHCAP=-NUNDEF
        IPOSALBE=-NUNDEF
        IPOSEMIS=-NUNDEF
        !
        DO KK=1,1000
           !
           IF (INDEX(YSTRING,",").EQ.0) EXIT
           !
           IF (YSTRING(1:(INDEX(YSTRING,",")-1)).EQ."MATERIAUX") THEN
              IPOSNAME=KK
           ELSE IF (YSTRING(1:(INDEX(YSTRING,",")-1)).EQ."MASSE VOLUMIQUE SECHE (kg/m^3)") THEN
              IPOSDENS=KK
           ELSE IF (YSTRING(1:(INDEX(YSTRING,",")-1)).EQ."CONDUCTIVITE THERMIQUE UTILE (W/mK)") THEN
              IPOSHCON=KK
           ELSE IF (YSTRING(1:(INDEX(YSTRING,",")-1)).EQ."CAPACITE THERMIQUE MASSIQUE (J/kgK)") THEN
              IPOSHCAP=KK
           ELSE IF (YSTRING(1:(INDEX(YSTRING,",")-1)).EQ."ALBEDO") THEN
              IPOSALBE=KK
           ELSE IF (YSTRING(1:(INDEX(YSTRING,",")-1)).EQ."EMISSIVITE") THEN
              IPOSEMIS=KK
           ENDIF
           !
           YSTRING=YSTRING((INDEX(YSTRING,",")+1):LEN(YSTRING))
           !
        ENDDO
        !
        IF (IPOSNAME.LT.0) CALL ABOR1_SFX('Material name not found')
        IF (IPOSDENS.LT.0) CALL ABOR1_SFX('Material density not found')
        IF (IPOSHCON.LT.0) CALL ABOR1_SFX('Material heat conductivity not found')
        IF (IPOSHCAP.LT.0) CALL ABOR1_SFX('Material heat capacity not found')
        IF (IPOSALBE.LT.0) CALL ABOR1_SFX('Material albedo not found')
        IF (IPOSEMIS.LT.0) CALL ABOR1_SFX('Material emissivity not found')
        !
     ENDIF
     !
     ICTMAT=0
     !
     IF (NCTLVA.EQ.1) THEN
        !
        DO KK=1,1000
           !
           READ (ILUNAM,END=9999,FMT='(A2000)') YSTRING
           !
           DO MM=1,1000
              !
              IF      (MM.EQ.IPOSNAME) THEN
                 YNAMEMATRD=YSTRING(1:(INDEX(YSTRING,",")-1))
              ELSE IF (MM.EQ.IPOSDENS) THEN
                 YDENSMATRD=YSTRING(1:(INDEX(YSTRING,",")-1))
                 READ(YDENSMATRD,*) ZDENSMATRD
              ELSE IF (MM.EQ.IPOSHCON) THEN
                 YHCONMATRD=YSTRING(1:(INDEX(YSTRING,",")-1))
                 READ(YHCONMATRD,*) ZHCONMATRD
              ELSE IF (MM.EQ.IPOSHCAP) THEN
                 YHCAPMATRD=YSTRING(1:(INDEX(YSTRING,",")-1))
                 READ(YHCAPMATRD,*) ZHCAPMATRD
              ELSE IF (MM.EQ.IPOSALBE) THEN
                 YALBEMATRD=YSTRING(1:(INDEX(YSTRING,",")-1))
                 READ(YALBEMATRD,*) ZALBEMATRD
              ELSE IF (MM.EQ.IPOSEMIS) THEN
                 YEMISMATRD=YSTRING(1:(INDEX(YSTRING,",")-1))
                 READ(YEMISMATRD,*) ZEMISMATRD
              ENDIF
              !
              IF (INDEX(YSTRING,",").EQ.0) THEN
                 ICTMAT=ICTMAT+1
                 EXIT
              ENDIF
              YSTRING=YSTRING((INDEX(YSTRING,",")+1):LEN(YSTRING))
              !
           ENDDO
           !
           WHERE(YMTP==YNAMEMATRD)
              ZDNSITY=ZDENSMATRD
              ZHEACON=ZHCONMATRD
              ZHEACAP=ZHCAPMATRD
              ZALBEDO=ZALBEMATRD
              ZEMISSI=ZEMISMATRD
           ENDWHERE
           !
           IF (ICTMAT.EQ.NMATTP) EXIT
           !
        ENDDO
        EXIT
     ENDIF
     !
  ENDDO
  !
  CALL CLOSE_NAMELIST(HPROGRAM,ILUNAM)
  !
  ! Conversion of heat capacity from J/kgK -> J/m^3K
  !
  ZHEACAP(:)=ZHEACAP(:)*ZDNSITY(:)
  ! 
  ! #########################################################################
  ! Read glazing characteristics
  ! #########################################################################
  !
  ALLOCATE(ZUVALUE(1:NWINDTP))
  ALLOCATE(ZSHGCCO(1:NWINDTP))
  !
  ZUVALUE(:)=-XUNDEF
  ZSHGCCO(:)=-XUNDEF
  !
  CALL OPEN_NAMELIST(HPROGRAM,ILUNAM,HFILE)
  !
  NCTLVA=0
  !
  DO LL=1,1000
     !
     READ (ILUNAM,END=9999,FMT='(A2000)') YSTRING
     !
     IF (INDEX(YSTRING,"DEBUT TABLEAU VITRAGE").GT.0) THEN
        !
        NCTLVA=1
        !
        ! Find positions of glazing characteristics in string
        !
        IPOSNAME=-NUNDEF
        IPOSUVAL=-NUNDEF
        IPOSSHGC=-NUNDEF
        !
        DO KK=1,1000
           !
           IF (INDEX(YSTRING,",").EQ.0) EXIT
           !
           IF      (YSTRING(1:(INDEX(YSTRING,",")-1)).EQ."Type de vitrage") THEN
              IPOSNAME=KK
           ELSE IF (YSTRING(1:(INDEX(YSTRING,",")-1)).EQ."U_VALUE (W/m^2K)") THEN
              IPOSUVAL=KK
           ELSE IF (YSTRING(1:(INDEX(YSTRING,",")-1)).EQ."SHGC") THEN
              IPOSSHGC=KK
           ENDIF
           !
           YSTRING=YSTRING((INDEX(YSTRING,",")+1):LEN(YSTRING))
           !
        ENDDO
        !
        IF (IPOSNAME.LT.0) CALL ABOR1_SFX('Glazing name not found')
        IF (IPOSUVAL.LT.0) CALL ABOR1_SFX('Glazing U-value not found')
        IF (IPOSSHGC.LT.0) CALL ABOR1_SFX('Glazing solar heat gain coefficient not found')
        !
     ENDIF
     !
     ICTVIT=0
     !
     IF (NCTLVA.EQ.1) THEN
        !
        DO KK=1,1000
           !
           READ (ILUNAM,END=9999,FMT='(A2000)') YSTRING
           !
           DO MM=1,1000
              !
              IF      (MM.EQ.IPOSNAME) THEN
                 YNAMEVITRD=YSTRING(1:(INDEX(YSTRING,",")-1))
              ELSE IF (MM.EQ.IPOSUVAL) THEN
                 YUVALVITRD=YSTRING(1:(INDEX(YSTRING,",")-1))
                 READ(YUVALVITRD,*) ZUVALVITRD
              ELSE IF (MM.EQ.IPOSSHGC) THEN
                 YSHGCVITRD=YSTRING(1:(INDEX(YSTRING,",")-1))
                 READ(YSHGCVITRD,*) ZSHGCVITRD
              ENDIF
              !
              IF (INDEX(YSTRING,",").EQ.0) THEN
                 ICTVIT=ICTVIT+1
                 EXIT
              ENDIF
              YSTRING=YSTRING((INDEX(YSTRING,",")+1):LEN(YSTRING))
              !
           ENDDO
           !
           WHERE(YWINDTP.EQ.YNAMEVITRD)
              ZUVALUE=ZUVALVITRD
              ZSHGCCO=ZSHGCVITRD
           ENDWHERE
           !
           IF (ICTVIT.EQ.(NWINDTP-1)) EXIT
           !
        ENDDO
        EXIT
     ENDIF
     !
  ENDDO
  !
  CALL CLOSE_NAMELIST(HPROGRAM,ILUNAM)
  !
  ! ############################################################
  ! Read the number of lines containing architectural information
  ! ############################################################
  !
  CALL OPEN_NAMELIST(HPROGRAM,ILUNAM,HFILE)
  READ (ILUNAM,END=9999,FMT='(A2000)') YSTRING
  DO LL=1,1000
     READ (ILUNAM,END=9999,FMT='(A2000)') YSTRING
     IF (INDEX(YSTRING,"NUMEROTATION").GT.0) EXIT
  ENDDO
  CALL CLOSE_NAMELIST(HPROGRAM,ILUNAM)
  !
  BDD%NDESC_CODE=LL-1
  !
  ! ############################################################
  ! Allocation of fields characterising the buildings
  ! ############################################################
  !
  ALLOCATE(BDD%NDESC_BLD_LIST(BDD%NDESC_CODE))
  ALLOCATE(BDD%NDESC_USE_LIST(BDD%NDESC_CODE))
  ALLOCATE(BDD%NDESC_AGE_LIST(BDD%NDESC_CODE))
  ALLOCATE(BDD%NDESC_AGE_DATE(BDD%NDESC_CODE))
  ALLOCATE(BDD%NDESC_TER_LIST(BDD%NDESC_CODE))
  ALLOCATE(BDD%NDESC_CODE_LIST(BDD%NDESC_CODE))
  !
  BDD%NDESC_BLD_LIST(:) = -NUNDEF
  BDD%NDESC_USE_LIST(:) = -NUNDEF
  BDD%NDESC_AGE_LIST(:) = -NUNDEF
  BDD%NDESC_AGE_DATE(:) = -NUNDEF
  BDD%NDESC_TER_LIST(:) = -NUNDEF
  BDD%NDESC_CODE_LIST(:)= -NUNDEF
  !
  BDD%NDESC_ROAD_LAYER = 1
  ALLOCATE(BDD%XDESC_HC_ROAD  (BDD%NDESC_CODE,BDD%NDESC_ROAD_LAYER))
  ALLOCATE(BDD%XDESC_TC_ROAD  (BDD%NDESC_CODE,BDD%NDESC_ROAD_LAYER))
  ALLOCATE(BDD%XDESC_D_ROAD   (BDD%NDESC_CODE,BDD%NDESC_ROAD_LAYER))
  ALLOCATE(BDD%XDESC_ALB_ROAD (BDD%NDESC_CODE))
  ALLOCATE(BDD%XDESC_EMIS_ROAD(BDD%NDESC_CODE))
  BDD%XDESC_HC_ROAD(:,:) = -XUNDEF
  BDD%XDESC_TC_ROAD(:,:) = -XUNDEF
  BDD%XDESC_D_ROAD (:,:) =  0.0
  BDD%XDESC_ALB_ROAD (:) = -XUNDEF
  BDD%XDESC_EMIS_ROAD(:) = -XUNDEF
  !
  BDD%NDESC_WALL_LAYER  = 4
  ALLOCATE(BDD%XDESC_HC_WALL(BDD%NDESC_CODE,BDD%NDESC_WALL_LAYER))
  ALLOCATE(BDD%XDESC_TC_WALL(BDD%NDESC_CODE,BDD%NDESC_WALL_LAYER))
  ALLOCATE(BDD%XDESC_D_WALL (BDD%NDESC_CODE,BDD%NDESC_WALL_LAYER))
  ALLOCATE(BDD%NDESC_ISOWALLPOS(BDD%NDESC_CODE))
  BDD%XDESC_HC_WALL(:,:) = -XUNDEF
  BDD%XDESC_TC_WALL(:,:) = -XUNDEF
  BDD%XDESC_D_WALL (:,:) = 0.0
  BDD%NDESC_ISOWALLPOS(:)= 0
  !
  BDD%NDESC_ROOF_LAYER  = 5
  ALLOCATE(BDD%XDESC_HC_ROOF (BDD%NDESC_CODE,BDD%NDESC_ROOF_LAYER))
  ALLOCATE(BDD%XDESC_TC_ROOF (BDD%NDESC_CODE,BDD%NDESC_ROOF_LAYER))
  ALLOCATE(BDD%XDESC_D_ROOF  (BDD%NDESC_CODE,BDD%NDESC_ROOF_LAYER))
  ALLOCATE(BDD%NDESC_ISOROOFPOS(BDD%NDESC_CODE))
  BDD%XDESC_HC_ROOF (:,:) = -XUNDEF
  BDD%XDESC_TC_ROOF (:,:) = -XUNDEF
  BDD%XDESC_D_ROOF  (:,:) = 0.0
  BDD%NDESC_ISOROOFPOS(:)= 0
  !
  BDD%NDESC_FLOOR_LAYER = 1
  ALLOCATE(BDD%XDESC_HC_FLOOR(BDD%NDESC_CODE,BDD%NDESC_FLOOR_LAYER))
  ALLOCATE(BDD%XDESC_TC_FLOOR(BDD%NDESC_CODE,BDD%NDESC_FLOOR_LAYER))
  ALLOCATE(BDD%XDESC_D_FLOOR (BDD%NDESC_CODE,BDD%NDESC_FLOOR_LAYER))
  BDD%XDESC_HC_FLOOR(:,:) = -XUNDEF
  BDD%XDESC_TC_FLOOR(:,:) = -XUNDEF
  BDD%XDESC_D_FLOOR (:,:) = 0.0
  !
  BDD%NDESC_MASS_LAYER = 2
  ALLOCATE(BDD%XDESC_HC_MASS(BDD%NDESC_CODE,BDD%NDESC_MASS_LAYER))
  ALLOCATE(BDD%XDESC_TC_MASS(BDD%NDESC_CODE,BDD%NDESC_MASS_LAYER))
  ALLOCATE(BDD%XDESC_D_MASS (BDD%NDESC_CODE,BDD%NDESC_MASS_LAYER))
  BDD%XDESC_HC_MASS(:,:) = -XUNDEF
  BDD%XDESC_TC_MASS(:,:) = -XUNDEF
  BDD%XDESC_D_MASS (:,:) = 0.0
  !
  ALLOCATE(BDD%XDESC_ISMASS(BDD%NDESC_CODE))
  BDD%XDESC_ISMASS(:)=1.0
  !
  ALLOCATE(BDD%XDESC_ALB_ROOF (BDD%NDESC_CODE))
  ALLOCATE(BDD%XDESC_ALB_WALL (BDD%NDESC_CODE))
  ALLOCATE(BDD%XDESC_EMIS_ROOF(BDD%NDESC_CODE))
  ALLOCATE(BDD%XDESC_EMIS_WALL(BDD%NDESC_CODE))
  BDD%XDESC_ALB_ROOF (:) = -XUNDEF
  BDD%XDESC_ALB_WALL (:) = -XUNDEF
  BDD%XDESC_EMIS_ROOF(:) = -XUNDEF
  BDD%XDESC_EMIS_WALL(:) = -XUNDEF
  !
  ALLOCATE(BDD%XDESC_SHGC      (BDD%NDESC_CODE))
  ALLOCATE(BDD%XDESC_U_WIN     (BDD%NDESC_CODE))
  ALLOCATE(BDD%XDESC_GR        (BDD%NDESC_CODE))
  ALLOCATE(BDD%XDESC_N50       (BDD%NDESC_CODE))
  ALLOCATE(BDD%XDESC_SHGC_SH   (BDD%NDESC_CODE))
  ALLOCATE(BDD%XDESC_SHADEARCHI(BDD%NDESC_CODE))
  ALLOCATE(BDD%XDESC_ISMECH    (BDD%NDESC_CODE))
  ALLOCATE(BDD%XDESC_MECHRATE  (BDD%NDESC_CODE))
  !
  BDD%XDESC_SHGC       (:) = -XUNDEF
  BDD%XDESC_U_WIN      (:) = -XUNDEF
  BDD%XDESC_GR         (:) = -XUNDEF
  BDD%XDESC_N50        (:) = -XUNDEF
  BDD%XDESC_SHGC_SH    (:) = -XUNDEF
  BDD%XDESC_SHADEARCHI (:) = -XUNDEF
  BDD%XDESC_ISMECH     (:) = -XUNDEF
  BDD%XDESC_MECHRATE   (:) = -XUNDEF
  !
  ALLOCATE(BDD%XDESC_GREENROOF (BDD%NDESC_CODE))
  ALLOCATE(BDD%XDESC_EMIS_PANEL(BDD%NDESC_CODE))
  ALLOCATE(BDD%XDESC_ALB_PANEL (BDD%NDESC_CODE))
  ALLOCATE(BDD%XDESC_EFF_PANEL (BDD%NDESC_CODE))
  ALLOCATE(BDD%XDESC_FRAC_PANEL(BDD%NDESC_CODE))
  !
  BDD%XDESC_GREENROOF(:)  = -XUNDEF
  BDD%XDESC_EMIS_PANEL(:) = -XUNDEF
  BDD%XDESC_ALB_PANEL(:)  = -XUNDEF
  BDD%XDESC_EFF_PANEL(:)  = -XUNDEF
  BDD%XDESC_FRAC_PANEL(:) = -XUNDEF
  !
  ! #######################################################
  ! Find positions of architectural input in .csv file
  ! #######################################################
  !
  ITEBPOS(:) = -NUNDEF
  IIDEPOS(:) = -NUNDEF
  !
  CALL OPEN_NAMELIST(HPROGRAM,ILUNAM,HFILE)
  !
  READ (ILUNAM,END=9999,FMT='(A2000)') YSTRING
  !
  DO KK=1,1000
     !
     IF (INDEX(YSTRING,",").EQ.0) EXIT
     !
     DO LL=1,nfield
        IF (YSTRING(1:(INDEX(YSTRING,",")-1)).EQ.ynames(LL)) itebpos(LL)=KK
     ENDDO
     !
     DO LL=1,4
        IF (YSTRING(1:(INDEX(YSTRING,",")-1)).EQ.YNAMEID(LL)) IIDEPOS(LL)=KK
     ENDDO
     !
     YSTRING=YSTRING((INDEX(YSTRING,",")+1):LEN(YSTRING))
     !
  ENDDO
  !
  DO LL=1,nfield
     IF (itebpos(LL).LT.0) CALL ABOR1_SFX('Requested input not found in table')
  ENDDO
  !
  DO LL=1,4
     IF (IIDEPOS(LL).LT.0) CALL ABOR1_SFX('Requested input not found in table')
  ENDDO
  !
  ! #################################################
  ! Read architectural information in csv file
  ! #################################################
  !
  IR=0
  !
  DO KK=1,BDD%NDESC_CODE
     !
     READ(ILUNAM,END=9999,FMT='(A2000)') YSTRING
     IR=IR+1
     !
     ! Read identifiers for islet, usage, period and territory
     !
     YSTRAUX=YSTRING
     DO LL=1,1000
        IF (INDEX(YSTRING,",").EQ.0) EXIT
        DO MM=1,4
           IF (IIDEPOS(MM)==LL) YREADID(MM)=YSTRING(1:(INDEX(YSTRING,",")-1))
        ENDDO
        YSTRING=YSTRING((INDEX(YSTRING,",")+1):LEN(YSTRING))
     ENDDO
     YSTRING=YSTRAUX
     !
     YISLE = TRIM(YREADID(1))
     YUSAG = TRIM(YREADID(2))
     YPERI = TRIM(YREADID(3))
     YTERR = TRIM(YREADID(4))
     !
     ! Read architectural information
     !
     DO LL=1,1000
        IF (INDEX(YSTRING,",").EQ.0) EXIT
        DO MM=1,nfield
           IF (itebpos(MM)==LL) yreadd(MM) = YSTRING(1:(INDEX(YSTRING,",")-1))
        ENDDO
        YSTRING=YSTRING((INDEX(YSTRING,",")+1):LEN(YSTRING))
     ENDDO
     !
     ! #############################################################
     ! Calculation of building code as a function of islet, usage,
     ! period and territory
     ! #############################################################
     !
     NCTLVA=0
     DO LL=1,SIZE(YISLETP)
        IF (YISLE.EQ.YISLETP(LL)) THEN
           BDD%NDESC_BLD_LIST(IR)=LL
           NCTLVA=NCTLVA+1
        ENDIF
     ENDDO
     IF (NCTLVA.NE.1) CALL ABOR1_SFX('Error in islet number attribution')
     !
     NCTLVA=0
     DO LL=1,SIZE(YUSAGTP)
        IF (YUSAG.EQ.YUSAGTP(LL)) THEN
           BDD%NDESC_USE_LIST(IR)=LL
           NCTLVA=NCTLVA+1
        ENDIF
     ENDDO
     IF (NCTLVA.NE.1) CALL ABOR1_SFX('Error in usage number attribution')
     !
     NCTLVA=0
     DO LL=1,SIZE(YPERITP)
        IF (YPERI.EQ.YPERITP(LL)) THEN
           BDD%NDESC_AGE_LIST(IR)=LL
           NCTLVA=NCTLVA+1
        ENDIF
     ENDDO
     IF (NCTLVA.NE.1) CALL ABOR1_SFX('Error in age class number attribution')
     !
     NCTLVA=0
     DO LL=1,SIZE(YTERRTP)
        IF (YTERR.EQ.YTERRTP(LL)) THEN
           BDD%NDESC_TER_LIST(IR)=LL
           NCTLVA=NCTLVA+1
        ENDIF
     ENDDO
     !
     IF (NCTLVA.NE.1) CALL ABOR1_SFX('Error in territory number attribution')
     !
     CALL BLDCODE(BDD%NDESC_BLD_LIST(IR),BDD%NDESC_AGE_LIST(IR),BDD%NDESC_USE_LIST(IR), &
                  BDD%NDESC_TER_LIST(IR),BDD%NDESC_CODE_LIST(IR))
     !
     ! #############################################################
     ! Check and attribute input concerning the windows and shading
     ! #############################################################
     !
     DO LL=1,NFIELD
        IF     (YNAMES(LL).EQ."TYPE_VITRAGE") THEN
           YWIND=TRIM(YREADD(LL))
        ELSEIF (YNAMES(LL).EQ."POURCENTAGE_VITRAGE") THEN
           YSTRAUX =TRIM(YREADD(LL))
           READ(YSTRAUX,*) ZGLAZFRACT
           ZGLAZFRACT=0.01*ZGLAZFRACT
        ELSEIF (YNAMES(LL).EQ."PROTECTIONS_SOLAIRES") THEN
           YSHAD=TRIM(YREADD(LL))
        ENDIF
     ENDDO
     !
     CALL CHECKINP(YWINDTP,YWIND)
     CALL CHECKINP(YSHADTP,YSHAD)
     !
     IF ((ZGLAZFRACT.LT.0.0).OR.(ZGLAZFRACT.GT.1.0)) CALL ABOR1_SFX('Wrong glazing ratio')
     IF ((YWIND.EQ."NON").AND.(ZGLAZFRACT.GT.0.0))   CALL ABOR1_SFX('Inconsistant window type and glazing fraction')
     IF ((YWIND.EQ."NON").AND.(YSHAD.NE."NON"))      CALL ABOR1_SFX('Inconsistant window type and shading type')
     !
     BDD%XDESC_GR(IR)=ZGLAZFRACT
     !
     NCTLVA=0
     DO LL=1,SIZE(YWINDTP)
        IF (YWINDTP(LL).EQ.YWIND) THEN
           NCTLVA=NCTLVA+1
           BDD%XDESC_SHGC (IR)=ZSHGCCO(LL)
           BDD%XDESC_U_WIN(IR)=ZUVALUE(LL)
        ENDIF
     ENDDO
     IF (NCTLVA.NE.1) CALL ABOR1_SFX('Error in attribution of window characteristics')
     !
     ! For the cases with YWIND = "NON", the characteristics for single
     ! glazing are attributed. However, this will not affect the results since
     ! in this case the glazing ratio is 0.0
     !
     IF (YWIND.EQ."NON") THEN
        DO LL=1,SIZE(YWINDTP)
           IF (YWINDTP(LL).EQ."SIMPLE VITRAGE") THEN
              BDD%XDESC_SHGC (IR)=ZSHGCCO(LL)
              BDD%XDESC_U_WIN(IR)=ZUVALUE(LL)
           ENDIF
        ENDDO
     ENDIF
     !
     ! Check values for window characteristics
     !
     IF ((BDD%XDESC_SHGC (IR).LT.0.0).OR.(BDD%XDESC_SHGC (IR).GT.1.0)) CALL ABOR1_SFX('Wrong value of solar heat gain coefficient')
     IF ((BDD%XDESC_U_WIN(IR).LT.0.0).OR.(BDD%XDESC_U_WIN(IR).GT.10.)) CALL ABOR1_SFX('Wrong value of window U-Value coefficient')
     !
     ! Modify window characteristics according to shading type.
     ! For CASQUETTE and SHED ORIENTATION NORD, the heat gain
     ! coefficient of the windows is reduced and SHADEARCHI is
     ! set to 2.0 (shading always present).
     !
     ! Further modifications of shgc etc. for specific elements
     ! like "EPAISSEUR MUR" or "CASQUETTE" could be made.
     ! However, it needs to be carefully checked how those
     ! interact with the calculations done in window_data etc.
     !
     IF      (YSHAD.EQ."NON") THEN
        BDD%XDESC_SHADEARCHI(IR) = 0.0
        BDD%XDESC_SHGC_SH   (IR) = BDD%XDESC_SHGC(IR)
     ELSE IF (YSHAD.EQ."EPAISSEUR MUR") THEN
        BDD%XDESC_SHADEARCHI(IR) = 0.0
        BDD%XDESC_SHGC(IR)       = BDD%XDESC_SHGC(IR) + 0.5*(1.0-BDD%XDESC_SHGC(IR))
        BDD%XDESC_SHGC_SH(IR)    = BDD%XDESC_SHGC(IR)
     ELSE IF (YSHAD.EQ."EPAISSEUR TOITURE") THEN
        BDD%XDESC_SHADEARCHI(IR) = 0.0
        BDD%XDESC_SHGC(IR)       = BDD%XDESC_SHGC(IR) + 0.5*(1.0-BDD%XDESC_SHGC(IR))
        BDD%XDESC_SHGC_SH   (IR) = BDD%XDESC_SHGC(IR)
     ELSE IF (YSHAD.EQ."STORE INTERIEUR") THEN
        BDD%XDESC_SHADEARCHI(IR) = 0.0
        BDD%XDESC_SHGC_SH   (IR) = BDD%XDESC_SHGC(IR)
     ELSE IF (YSHAD.EQ."CASQUETTE") THEN
        BDD%XDESC_SHADEARCHI(IR) = 2.0
        BDD%XDESC_SHGC_SH   (IR) = 0.5*BDD%XDESC_SHGC(IR)
     ELSE IF (YSHAD.EQ."DIMENSIONNEMENT OUVERTURE") THEN
        BDD%XDESC_SHADEARCHI(IR) = 0.0
        BDD%XDESC_SHGC(IR)       = BDD%XDESC_SHGC(IR) + 0.5*(1.0-BDD%XDESC_SHGC(IR))
        BDD%XDESC_SHGC_SH(IR)    = BDD%XDESC_SHGC(IR)
     ELSE IF (YSHAD.EQ."STORE INTERIEUR_VOLET") THEN
        BDD%XDESC_SHADEARCHI(IR) = 1.0
        BDD%XDESC_SHGC_SH   (IR) = 0.025
     ELSE IF (YSHAD.EQ."EPAISSEUR MUR_VOLET") THEN
        BDD%XDESC_SHADEARCHI(IR) = 1.0
        BDD%XDESC_SHGC(IR)       = BDD%XDESC_SHGC(IR) + 0.5*(1.0-BDD%XDESC_SHGC(IR))
        BDD%XDESC_SHGC_SH   (IR) = 0.025
     ELSE IF (YSHAD.EQ."DEBORD TOITURE_VOLET") THEN
        BDD%XDESC_SHADEARCHI(IR) = 1.0
        BDD%XDESC_SHGC_SH   (IR) = 0.025
     ELSE IF (YSHAD.EQ."SHED ORIENTATION NORD") THEN
        BDD%XDESC_SHADEARCHI(IR) = 2.0
        BDD%XDESC_SHGC_SH   (IR) = 0.5*BDD%XDESC_SHGC(IR)
     ELSE IF (YSHAD.EQ."VENTILATION NATURELLE") THEN
        BDD%XDESC_SHADEARCHI(IR) = 0.0
        BDD%XDESC_SHGC_SH   (IR) = BDD%XDESC_SHGC(IR)
     ELSE IF (YSHAD.EQ."DOUBLE PEAU") THEN
        BDD%XDESC_SHADEARCHI(IR) = 2.0
        BDD%XDESC_SHGC_SH   (IR) = 0.5*BDD%XDESC_SHGC(IR)
     ELSE IF (YSHAD.EQ."CASQUETTE_VOLET") THEN
        BDD%XDESC_SHADEARCHI(IR) = 1.0
        BDD%XDESC_SHGC_SH   (IR) = 0.025
     ELSE
        CALL ABOR1_SFX('This shading type is not implemented')
     ENDIF
     !
     ! ################################################
     ! Check and attribute airtightness
     ! ################################################
     !
     DO LL=1,NFIELD
        IF (YNAMES(LL).EQ."PERMEABILITE") THEN
           YSTRAUX =TRIM(YREADD(LL))
           READ(YSTRAUX,*) ZAIRTIGHTN
        ENDIF
     ENDDO
     !
     IF ((ZAIRTIGHTN.LT.0.0).OR.(ZAIRTIGHTN.GT.25.0)) CALL ABOR1_SFX('Unplausible value for airtightness')
     !
     BDD%XDESC_N50(IR)=ZAIRTIGHTN
     !
     ! ######################################################################
     ! Check and attribute input concerning the ground floor and mass
     ! ######################################################################
     !
     DO LL=1,NFIELD
        IF     (YNAMES(LL).EQ."PLANCHER_RDC") THEN
           YGRFL=TRIM(YREADD(LL))
        ELSEIF (YNAMES(LL).EQ."EP_PLANCHER_RDC") THEN
           YSTRAUX =TRIM(YREADD(LL))
           READ(YSTRAUX,*) ZEPRDC
           ZEPRDC=0.01*ZEPRDC
           IF ((ZEPRDC.LT.0.0).OR.(ZEPRDC.GT.2.0)) CALL ABOR1_SFX('Unplausible value for ground floor thickness')
        ELSEIF (YNAMES(LL).EQ."PLANCHER_INTERMEDIAIRE") THEN
           YIMFL=TRIM(YREADD(LL))
        ELSEIF (YNAMES(LL).EQ."EP_PLANCHER_INTER") THEN
           YSTRAUX =TRIM(YREADD(LL))
           READ(YSTRAUX,*) ZEPPIN
           ZEPPIN=0.01*ZEPPIN
           IF ((ZEPPIN.LT.0.0).OR.(ZEPPIN.GT.2.0)) CALL ABOR1_SFX('Unplausible value for intermediate floor thickness')
        ENDIF
     ENDDO
     !
     CALL CHECKINP(YGRFLTP,YGRFL)
     CALL CHECKINP(YIMFLTP,YIMFL)
     !
     IF (YIMFL.EQ."NON") THEN
        !
        ! There will be cases where the architectural information
        ! indicates that there is no intermediate floor, but the data
        ! for building and floor height will lead to the presence of
        ! an intermediate floor. For these cases an intermediate
        ! floor built of 3 cm wood is assumed here in order to
        ! not perturb the calculations.
        !
        BDD%XDESC_ISMASS(IR)=0.0
        !
        BDD%XDESC_D_MASS(IR,1)=0.03
        YSAUX="BOIS"
        CALL GETPCHAR(YMTP,ZHEACAP,ZHEACON,ZALBEDO,ZEMISSI,YSAUX,BDD%XDESC_HC_MASS(IR,1), &
             BDD%XDESC_TC_MASS(IR,1),DUMMY,DUMMY)
        !
        BDD%XDESC_D_MASS (IR,2)=    0.0
        BDD%XDESC_HC_MASS(IR,2)=-XUNDEF
        BDD%XDESC_TC_MASS(IR,2)=-XUNDEF
        !
     ELSE
        BDD%XDESC_D_MASS (IR,1)=ZEPPIN
        BDD%XDESC_D_MASS (IR,2)=0.20
        BDD%XDESC_HC_MASS(IR,2)=1.0E+6
        BDD%XDESC_TC_MASS(IR,2)=1.0
        CALL GETPCHAR(YMTP,ZHEACAP,ZHEACON,ZALBEDO,ZEMISSI,YIMFL,BDD%XDESC_HC_MASS(IR,1), &
             BDD%XDESC_TC_MASS(IR,1),DUMMY,DUMMY)
     ENDIF
     !
     DO IL=1,BDD%NDESC_MASS_LAYER
        !
        IF (BDD%XDESC_D_MASS(IR,IL).LT.0.0) CALL ABOR1_SFX('Negative mass layer thickness')
        !
        IF (BDD%XDESC_D_MASS(IR,IL).GT.0.0) THEN
           IF (BDD%XDESC_HC_MASS(IR,IL).LT.0.0) CALL ABOR1_SFX('Heat capacity not attributed')
           IF (BDD%XDESC_TC_MASS(IR,IL).LT.0.0) CALL ABOR1_SFX('Thermal conductivity not attributed')
        ENDIF
        !
     ENDDO
     !
     IF (YGRFL.EQ."TERRE BATTUE")                YGRFL = "TERRE CRUE"
     IF (YGRFL.EQ."DALLE BETON SUR TERRE PLEIN") YGRFL = "DALLE BETON"
     IF (YGRFL.EQ."REVETEMENT SUR TERRE PLEIN")  YGRFL = "TERRE CRUE"
     IF (YGRFL.EQ."DALLE BETON SOUS SOL")        YGRFL = "DALLE BETON"
     IF (YGRFL.EQ."VOUTE PIERRE SOUS SOL")       YGRFL = "PIERRE"
     !
     BDD%XDESC_D_FLOOR(IR,1)=ZEPRDC
     CALL GETPCHAR(YMTP,ZHEACAP,ZHEACON,ZALBEDO,ZEMISSI,YGRFL,BDD%XDESC_HC_FLOOR(IR,1), &
          BDD%XDESC_TC_FLOOR(IR,1),DUMMY,DUMMY)
     !
     DO IL=1,BDD%NDESC_FLOOR_LAYER
        !
        IF (BDD%XDESC_D_FLOOR(IR,IL).LT.0.0) CALL ABOR1_SFX('Negative floor layer thickness')
        !
        IF (BDD%XDESC_D_FLOOR(IR,IL).GT.0.0) THEN
           IF (BDD%XDESC_HC_FLOOR(IR,IL).LT.0.0) CALL ABOR1_SFX('Heat capacity not attributed')
           IF (BDD%XDESC_TC_FLOOR(IR,IL).LT.0.0) CALL ABOR1_SFX('Thermal conductivity not attributed')
        ENDIF
     ENDDO
     !
     ! ######################################################################
     ! Check and attribute input concerning the roads
     ! ######################################################################
     !
     DO LL=1,NFIELD
        IF     (YNAMES(LL).EQ."COUVERTURE_ROUTE") THEN
           YCOVR=TRIM(YREADD(LL))
        ELSEIF (YNAMES(LL).EQ."EP_COUVERTURE_ROUTE") THEN
           YSTRAUX=TRIM(YREADD(LL))
           READ(YSTRAUX,*) ZROADCOVDP
           ZROADCOVDP=0.01*ZROADCOVDP
        ENDIF
     ENDDO
     !
     CALL CHECKINP(YCOVRTP,YCOVR)
     IF ((ZROADCOVDP.LT.0.01).OR.(ZROADCOVDP.GT.0.2)) CALL ABOR1_SFX('Unrealistic road cover thickness')
     !
     BDD%XDESC_D_ROAD(IR,1)=ZROADCOVDP
     !
     CALL GETPCHAR(YMTP,ZHEACAP,ZHEACON,ZALBEDO,ZEMISSI,YCOVR,BDD%XDESC_HC_ROAD(IR,1), &
          BDD%XDESC_TC_ROAD(IR,1),BDD%XDESC_ALB_ROAD(IR),BDD%XDESC_EMIS_ROAD(IR))
     !
     ! Check attributed road parameters
     !
     DO IL=1,BDD%NDESC_ROAD_LAYER
        IF (BDD%XDESC_D_ROAD (IR,IL).LT.0.0) CALL ABOR1_SFX('Road thickness not attributed')
        IF (BDD%XDESC_HC_ROAD(IR,IL).LT.0.0) CALL ABOR1_SFX('Road heat capacity not attributed')
        IF (BDD%XDESC_TC_ROAD(IR,IL).LT.0.0) CALL ABOR1_SFX('Road thermal conductivity not attributed')
     ENDDO
     !
     ! ######################################################################
     ! Check and attribute input concerning the mechanical ventilation
     ! ######################################################################
     !
     DO LL=1,NFIELD
        IF     (YNAMES(LL).EQ."TYPE VENTILATION MECANIQUE") THEN
           YMECH=TRIM(YREADD(LL))
        ELSEIF (YNAMES(LL).EQ."TAUX D ECHANGE VENTILATION MECANIQUE [1/h]") THEN
           YSTRAUX=TRIM(YREADD(LL))
           READ(YSTRAUX,*) ZMECHRATE
        ENDIF
     ENDDO
     !
     CALL CHECKINP(YMECHTP,YMECH)
     !
     IF ((ZMECHRATE.LT.0.0).OR.(ZMECHRATE.GT.10.0)) CALL ABOR1_SFX('Unrealistic mechanical ventilation rate')
     IF ((YMECH.EQ."NON") .AND.(ZMECHRATE.GT.0.01)) CALL ABOR1_SFX('Inconsistant input for mecanical ventilation')
     IF ((YMECH.EQ."OUI") .AND.(ZMECHRATE.LT.0.10)) CALL ABOR1_SFX('Inconsistant input for mecanical ventilation')
     !
     IF (YMECH.EQ."NON") BDD%XDESC_ISMECH(IR)=0.0
     IF (YMECH.EQ."OUI") BDD%XDESC_ISMECH(IR)=1.0
     !
     BDD%XDESC_MECHRATE(IR)=ZMECHRATE
     !
     ! ######################################################################
     ! Check and attribute input concerning green roofs and solar panels
     ! ######################################################################
     !
     DO LL=1,NFIELD
        IF     (YNAMES(LL).EQ."FRACTION DE TOITS VEGETALISES") THEN
           YSTRAUX=TRIM(YREADD(LL))
           READ(YSTRAUX,*) ZGREENROOF
        ELSEIF (YNAMES(LL).EQ."FRACTION DE PANNEAUX SOLAIRES") THEN
           YSTRAUX=TRIM(YREADD(LL))
           READ(YSTRAUX,*) ZFRAC_PANEL
        ELSEIF (YNAMES(LL).EQ."ALBEDO DES PANNEAUX SOLAIRES") THEN
           YSTRAUX=TRIM(YREADD(LL))
           READ(YSTRAUX,*) ZALB_PANEL
        ELSEIF (YNAMES(LL).EQ."EMISSIVITE DES PANNEAUX SOLAIRES") THEN
           YSTRAUX=TRIM(YREADD(LL))
           READ(YSTRAUX,*) ZEMIS_PANEL
        ELSEIF (YNAMES(LL).EQ."EFFICACITE DES PANNEAUX SOLAIRES") THEN
           YSTRAUX=TRIM(YREADD(LL))
           READ(YSTRAUX,*) ZEFF_PANEL
        ENDIF
     ENDDO
     !
     IF ((ZGREENROOF .LT.0.0).OR.(ZGREENROOF .GT.1.0)) CALL ABOR1_SFX('Wrong fraction of green roofs')
     IF ((ZFRAC_PANEL.LT.0.0).OR.(ZFRAC_PANEL.GT.1.0)) CALL ABOR1_SFX('Wrong fraction of solar panels')
     IF ((ZALB_PANEL .LT.0.0).OR.(ZALB_PANEL .GT.1.0)) CALL ABOR1_SFX('Wrong fraction of solar panels')
     IF ((ZEMIS_PANEL.LT.0.0).OR.(ZEMIS_PANEL.GT.1.0)) CALL ABOR1_SFX('Wrong fraction of solar panels')
     IF ((ZEFF_PANEL .LT.0.0).OR.(ZEFF_PANEL .GT.1.0)) CALL ABOR1_SFX('Wrong fraction of solar panels')
     !
     BDD%XDESC_GREENROOF(IR)  = ZGREENROOF
     BDD%XDESC_FRAC_PANEL(IR) = ZFRAC_PANEL
     BDD%XDESC_ALB_PANEL(IR)  = ZALB_PANEL
     BDD%XDESC_EMIS_PANEL(IR) = ZEMIS_PANEL
     BDD%XDESC_EFF_PANEL(IR)  = ZEFF_PANEL
     !
     ! #####################################################
     ! Check and attribute input concerning the walls
     ! #####################################################
     !
     DO LL=1,NFIELD
        IF     (YNAMES(LL).EQ."PORTEUR") THEN
           YWSTR=TRIM(YREADD(LL))
        ELSEIF (YNAMES(LL).EQ."EP_PORTEUR") THEN
           YSTRAUX =TRIM(YREADD(LL))
           READ(YSTRAUX,*) ZWALLSTRDP
           ZWALLSTRDP=0.01*ZWALLSTRDP
        ELSEIF (YNAMES(LL).EQ."ISOLANT") THEN
           YWISO=TRIM(YREADD(LL))
        ELSEIF (YNAMES(LL).EQ."EP_ISOLANT") THEN
           YSTRAUX =TRIM(YREADD(LL))
           READ(YSTRAUX,*) ZWALLISODP
           ZWALLISODP=0.01*ZWALLISODP
        ELSEIF (YNAMES(LL).EQ."REVETEMENT_INT") THEN
           YWRIN=TRIM(YREADD(LL))
        ELSEIF (YNAMES(LL).EQ."EP_RI") THEN
           YSTRAUX =TRIM(YREADD(LL))
           READ(YSTRAUX,*) ZWALLRINDP
           ZWALLRINDP=0.01*ZWALLRINDP
        ELSEIF (YNAMES(LL).EQ."REVETEMENT_EXT") THEN
           YWREX=TRIM(YREADD(LL))
        ELSEIF (YNAMES(LL).EQ."EP_RE") THEN
           YSTRAUX =TRIM(YREADD(LL))
           READ(YSTRAUX,*) ZWALLREXDP
           ZWALLREXDP=0.01*ZWALLREXDP
        ENDIF
     ENDDO
     !
     CALL CHECKINP(YWSTRTP,YWSTR)
     CALL CHECKINP(YWISOTP,YWISO)
     CALL CHECKINP(YWRINTP,YWRIN)
     CALL CHECKINP(YWREXTP,YWREX)
     !
     IF ((ZWALLSTRDP.LT.0.0).OR.(ZWALLSTRDP.GT.1.0)) CALL ABOR1_SFX('Unrealistic wall structure thickness')
     IF ((ZWALLISODP.LT.0.0).OR.(ZWALLISODP.GT.1.0)) CALL ABOR1_SFX('Unrealistic wall insulation thickness')
     IF ((ZWALLRINDP.LT.0.0).OR.(ZWALLRINDP.GT.1.0)) CALL ABOR1_SFX('Unrealistic wall internal coating thickness')
     IF ((ZWALLREXDP.LT.0.0).OR.(ZWALLREXDP.GT.1.0)) CALL ABOR1_SFX('Unrealistic wall external coating thickness')
     !
     IF ((INDEX(YWSTR,"OSSATURE").NE.0).AND.(ZWALLSTRDP.NE.0.0)) CALL ABOR1_SFX('Wrong structure thickness')
     !
     BDD%XDESC_D_WALL(IR,1)=ZWALLREXDP
     BDD%XDESC_D_WALL(IR,4)=ZWALLRINDP
     !
     ALLOCATE(ZAUAL(1:BDD%NDESC_WALL_LAYER))
     ALLOCATE(ZAUEM(1:BDD%NDESC_WALL_LAYER))
     ZAUAL(:)=-XUNDEF
     ZAUEM(:)=-XUNDEF
     !
     IF (ZWALLREXDP.GT.0.0) CALL GETPCHAR(YMTP,ZHEACAP,ZHEACON,ZALBEDO,ZEMISSI,YWREX,BDD%XDESC_HC_WALL(IR,1), &
          BDD%XDESC_TC_WALL(IR,1),ZAUAL(1),ZAUEM(1))
     IF (ZWALLRINDP.GT.0.0) CALL GETPCHAR(YMTP,ZHEACAP,ZHEACON,ZALBEDO,ZEMISSI,YWRIN,BDD%XDESC_HC_WALL(IR,4), &
          BDD%XDESC_TC_WALL(IR,4),ZAUAL(4),ZAUEM(4))
     !
     IF (INDEX(YWISO,"ITE").GT.0) THEN
        !
        YWISA=YWISO(5:LEN(TRIM(YWISO)))
        !
        BDD%XDESC_D_WALL(IR,2)=ZWALLISODP
        BDD%XDESC_D_WALL(IR,3)=ZWALLSTRDP
        !
        ! In the case no insulation is present, the parameters of LAINE MINERALE are imposed
        !
        IF (ZWALLISODP.GT.0.0) THEN
           CALL GETPCHAR(YMTP,ZHEACAP,ZHEACON,ZALBEDO,ZEMISSI,YWISA,BDD%XDESC_HC_WALL(IR,2), &
              BDD%XDESC_TC_WALL(IR,2),ZAUAL(2),ZAUEM(2))
        ELSE
           YSAUX="LAINE MINERALE"
           CALL GETPCHAR(YMTP,ZHEACAP,ZHEACON,ZALBEDO,ZEMISSI,YSAUX,BDD%XDESC_HC_WALL(IR,2), &
              BDD%XDESC_TC_WALL(IR,2),ZAUAL(2),ZAUEM(2))
        ENDIF
        !
        IF (ZWALLSTRDP.GT.0.0) CALL GETPCHAR(YMTP,ZHEACAP,ZHEACON,ZALBEDO,ZEMISSI,YWSTR,     &
           BDD%XDESC_HC_WALL(IR,3),BDD%XDESC_TC_WALL(IR,3),ZAUAL(3),ZAUEM(3))
        !
        BDD%NDESC_ISOWALLPOS(IR) = 2
        !
     ELSE
        !
        ! In the default case, the insulation is interior
        !
        YWISA=YWISO(5:LEN(TRIM(YWISO)))
        !
        BDD%XDESC_D_WALL(IR,2)=ZWALLSTRDP
        BDD%XDESC_D_WALL(IR,3)=ZWALLISODP
        !
        IF (ZWALLSTRDP.GT.0.0) CALL GETPCHAR(YMTP,ZHEACAP,ZHEACON,ZALBEDO,ZEMISSI,YWSTR,BDD%XDESC_HC_WALL(IR,2), &
             BDD%XDESC_TC_WALL(IR,2),ZAUAL(2),ZAUEM(2))
        !
        ! In the case no insulation is present, the parameters of LAINE MINERALE are imposed
        !
        IF (ZWALLISODP.GT.0.0) THEN
           CALL GETPCHAR(YMTP,ZHEACAP,ZHEACON,ZALBEDO,ZEMISSI,YWISA,BDD%XDESC_HC_WALL(IR,3), &
              BDD%XDESC_TC_WALL(IR,3),ZAUAL(3),ZAUEM(3))
        ELSE
           YSAUX="LAINE MINERALE"
           CALL GETPCHAR(YMTP,ZHEACAP,ZHEACON,ZALBEDO,ZEMISSI,YSAUX,BDD%XDESC_HC_WALL(IR,3), &
              BDD%XDESC_TC_WALL(IR,3),ZAUAL(3),ZAUEM(3))
        ENDIF
        !
        BDD%NDESC_ISOWALLPOS(IR) = 3
        !
     ENDIF
     !
     ! ##############################################################################
     !
     ! Albedo and emissivity of outermost layer
     !
     NCTLVA=0
     DO IL=1,BDD%NDESC_WALL_LAYER
        IF (BDD%XDESC_HC_WALL(IR,IL).GT.0.0) THEN
           BDD%XDESC_ALB_WALL (IR)=ZAUAL(IL)
           BDD%XDESC_EMIS_WALL(IR)=ZAUEM(IL)
           NCTLVA=NCTLVA+1
           EXIT
        ENDIF
     ENDDO
     !
     IF (NCTLVA.NE.1) CALL ABOR1_SFX('Error in albedo and emissivity attribution')
     IF ((BDD%XDESC_ALB_WALL(IR) .LT.0.0).OR.(BDD%XDESC_ALB_WALL(IR) .GT.1.0)) CALL ABOR1_SFX('Wrong value for albedo')
     IF ((BDD%XDESC_EMIS_WALL(IR).LT.0.0).OR.(BDD%XDESC_EMIS_WALL(IR).GT.1.0)) CALL ABOR1_SFX('Wrong value for emissivity')
     !
     DEALLOCATE(ZAUAL)
     DEALLOCATE(ZAUEM)
     !
     DO IL=1,BDD%NDESC_WALL_LAYER
        !
        IF (BDD%XDESC_D_WALL(IR,IL).LT.0.0) CALL ABOR1_SFX('Negative wall layer thickness')
        !
        IF (BDD%XDESC_D_WALL(IR,IL).GT.0.0) THEN
           IF (BDD%XDESC_HC_WALL(IR,IL).LT.0.0) CALL ABOR1_SFX('Heat capacity not attributed')
           IF (BDD%XDESC_TC_WALL(IR,IL).LT.0.0) CALL ABOR1_SFX('Thermal conductivity not attributed')
        ENDIF
        !
     ENDDO
     !
     ! #######################################################
     ! Check and attribute input concerning the roofs
     ! #######################################################
     !
     DO LL=1,NFIELD
        IF     (YNAMES(LL).EQ."PORTEUR3") THEN
           YRSTR=TRIM(YREADD(LL))
        ELSEIF (YNAMES(LL).EQ."EP_PORTEUR3") THEN
           YSTRAUX =TRIM(YREADD(LL))
           READ(YSTRAUX,*) ZROOFSTRDP
           ZROOFSTRDP=0.01*ZROOFSTRDP
        ELSEIF (YNAMES(LL).EQ."ISOLANT3") THEN
           YRISO=TRIM(YREADD(LL))
        ELSEIF (YNAMES(LL).EQ."EP_ISOLANT3") THEN
           YSTRAUX =TRIM(YREADD(LL))
           READ(YSTRAUX,*) ZROOFISODP
           ZROOFISODP=0.01*ZROOFISODP
        ELSEIF (YNAMES(LL).EQ."REVETEMENT_INT3") THEN
           YRRIN=TRIM(YREADD(LL))
        ELSEIF (YNAMES(LL).EQ."EP_RI3") THEN
           YSTRAUX =TRIM(YREADD(LL))
           READ(YSTRAUX,*) ZROOFRINDP
           ZROOFRINDP=0.01*ZROOFRINDP
        ELSEIF (YNAMES(LL).EQ."REVETEMENT_EXT3") THEN
           YRREX=TRIM(YREADD(LL))
        ELSEIF (YNAMES(LL).EQ."EP_RE3") THEN
           YSTRAUX =TRIM(YREADD(LL))
           READ(YSTRAUX,*) ZROOFREXDP
           ZROOFREXDP=0.01*ZROOFREXDP
        ENDIF
     ENDDO
     !
     CALL CHECKINP(YRSTRTP,YRSTR)
     CALL CHECKINP(YRISOTP,YRISO)
     CALL CHECKINP(YRRINTP,YRRIN)
     CALL CHECKINP(YRREXTP,YRREX)
     !
     IF ((ZROOFSTRDP.LT.0.0).OR.(ZROOFSTRDP.GT.1.0)) CALL ABOR1_SFX('Unrealistic roof structure thickness')
     IF ((ZROOFISODP.LT.0.0).OR.(ZROOFISODP.GT.1.0)) CALL ABOR1_SFX('Unrealistic roof insulation thickness')
     IF ((ZROOFRINDP.LT.0.0).OR.(ZROOFRINDP.GT.1.0)) CALL ABOR1_SFX('Unrealistic roof internal coating thickness')
     IF ((ZROOFREXDP.LT.0.0).OR.(ZROOFREXDP.GT.1.0)) CALL ABOR1_SFX('Unrealistic roof external coating thickness')
     !
     BDD%XDESC_D_ROOF(IR,1)=ZROOFREXDP
     BDD%XDESC_D_ROOF(IR,4)=ZROOFRINDP
     !
     ALLOCATE(ZAUAL(1:BDD%NDESC_ROOF_LAYER))
     ALLOCATE(ZAUEM(1:BDD%NDESC_ROOF_LAYER))
     ZAUAL(:)=-XUNDEF
     ZAUEM(:)=-XUNDEF
     !
     IF (ZROOFREXDP.GT.0.0) CALL GETPCHAR(YMTP,ZHEACAP,ZHEACON,ZALBEDO,ZEMISSI,YRREX,BDD%XDESC_HC_ROOF(IR,1), &
          BDD%XDESC_TC_ROOF(IR,1),ZAUAL(1),ZAUEM(1))
     IF (ZROOFRINDP.GT.0.0) CALL GETPCHAR(YMTP,ZHEACAP,ZHEACON,ZALBEDO,ZEMISSI,YRRIN,BDD%XDESC_HC_ROOF(IR,4), &
          BDD%XDESC_TC_ROOF(IR,4),ZAUAL(4),ZAUEM(4))
     !
     IF (INDEX(YRISO,"ITE").GT.0) THEN
        !
        YRISA=YRISO(5:LEN(TRIM(YRISO)))
        !
        BDD%XDESC_D_ROOF(IR,2)=ZROOFISODP
        BDD%XDESC_D_ROOF(IR,3)=ZROOFSTRDP
        !
        ! If no roof insulation is present, the parameters of LAINE MINERALE are imposed
        !
        IF (ZROOFISODP.GT.0.0) THEN
           CALL GETPCHAR(YMTP,ZHEACAP,ZHEACON,ZALBEDO,ZEMISSI,YRISA,BDD%XDESC_HC_ROOF(IR,2),&
              BDD%XDESC_TC_ROOF(IR,2),ZAUAL(2),ZAUEM(2))
        ELSE
           YSAUX="LAINE MINERALE"
           CALL GETPCHAR(YMTP,ZHEACAP,ZHEACON,ZALBEDO,ZEMISSI,YSAUX,BDD%XDESC_HC_ROOF(IR,2),&
              BDD%XDESC_TC_ROOF(IR,2),ZAUAL(2),ZAUEM(2))
        ENDIF
        !
        IF (ZROOFSTRDP.GT.0.0) CALL GETPCHAR(YMTP,ZHEACAP,ZHEACON,ZALBEDO,ZEMISSI,YRSTR,    &
              BDD%XDESC_HC_ROOF(IR,3),BDD%XDESC_TC_ROOF(IR,3),ZAUAL(3),ZAUEM(3))
        !
        BDD%NDESC_ISOROOFPOS(IR) = 2
        !
     ELSE
        !
        ! In the default case, the insulation is ITI
        !
        YRISA=YRISO(5:LEN(TRIM(YRISO)))
        !
        BDD%XDESC_D_ROOF(IR,2)=ZROOFSTRDP
        BDD%XDESC_D_ROOF(IR,3)=ZROOFISODP
        !
        IF (ZROOFSTRDP.GT.0.0) CALL GETPCHAR(YMTP,ZHEACAP,ZHEACON,ZALBEDO,ZEMISSI,YRSTR, &
           BDD%XDESC_HC_ROOF(IR,2),BDD%XDESC_TC_ROOF(IR,2),ZAUAL(2),ZAUEM(2))
        !
        ! If no roof insulation is present, the parameters of LAINE MINERALE are imposed
        !
        IF (ZROOFISODP.GT.0.0) THEN
           CALL GETPCHAR(YMTP,ZHEACAP,ZHEACON,ZALBEDO,ZEMISSI,YRISA,BDD%XDESC_HC_ROOF(IR,3), &
              BDD%XDESC_TC_ROOF(IR,3),ZAUAL(3),ZAUEM(3))
        ELSE
           YSAUX="LAINE MINERALE"
           CALL GETPCHAR(YMTP,ZHEACAP,ZHEACON,ZALBEDO,ZEMISSI,YSAUX,BDD%XDESC_HC_ROOF(IR,3), &
              BDD%XDESC_TC_ROOF(IR,3),ZAUAL(3),ZAUEM(3))
        ENDIF
        !
        BDD%NDESC_ISOROOFPOS(IR) = 3
        !
     ENDIF
     !
     ! A layer of wood can be placed below the roof in order to consider that
     ! normally the lowest layer of the roof is not in contact with the inhabited rooms
     !
     BDD%XDESC_D_ROOF(IR,5)=0.05
     YSAUX="BOIS"
     CALL GETPCHAR(YMTP,ZHEACAP,ZHEACON,ZALBEDO,ZEMISSI,YSAUX,BDD%XDESC_HC_ROOF(IR,5), &
           BDD%XDESC_TC_ROOF(IR,5),DUMMY,DUMMY)
     !
     ! Albedo and emissivity of outermost layer
     !
     NCTLVA=0
     DO IL=1,BDD%NDESC_ROOF_LAYER
        IF (BDD%XDESC_HC_ROOF(IR,IL).GT.0.0) THEN
           BDD%XDESC_ALB_ROOF (IR)=ZAUAL(IL)
           BDD%XDESC_EMIS_ROOF(IR)=ZAUEM(IL)
           NCTLVA=NCTLVA+1
           EXIT
        ENDIF
     ENDDO
     !
     DEALLOCATE(ZAUAL)
     DEALLOCATE(ZAUEM)
     !
     IF (NCTLVA.NE.1) CALL ABOR1_SFX('Error in attribution of albedo')
     IF ((BDD%XDESC_ALB_ROOF (IR).LT.0.0).OR.(BDD%XDESC_ALB_ROOF (IR).GT.1.0)) CALL ABOR1_SFX('Wrong value of roof albedo')
     IF ((BDD%XDESC_EMIS_ROOF(IR).LT.0.0).OR.(BDD%XDESC_EMIS_ROOF(IR).GT.1.0)) CALL ABOR1_SFX('Wrong value of roof emissivity')
     !
     DO IL=1,BDD%NDESC_ROOF_LAYER
        !
        IF (BDD%XDESC_D_ROOF(IR,IL).LT.0.0) CALL ABOR1_SFX('Negative roof layer thickness')
        !
        IF (BDD%XDESC_D_ROOF(IR,IL).GT.0.0) THEN
           IF (BDD%XDESC_HC_ROOF(IR,IL).LT.0.0) CALL ABOR1_SFX('Heat capacity not attributed')
           IF (BDD%XDESC_TC_ROOF(IR,IL).LT.0.0) CALL ABOR1_SFX('Thermal conductivity not attributed')
        ENDIF
     ENDDO
     !
  ENDDO
  !
  ! Check whether there is a double entry in the .csv table
  !
  DO LL=1,BDD%NDESC_CODE
     DO IR=(LL+1),BDD%NDESC_CODE
        !
        IF (BDD%NDESC_CODE_LIST(LL).EQ.BDD%NDESC_CODE_LIST(IR)) THEN
           CALL ABOR1_SFX('Double entry in building defintion table')
        ENDIF
        !
     ENDDO
  ENDDO
  !
  IF (LHOOK) CALL DR_HOOK('READ_CSVDATA_ARCHI_TEB',1,ZHOOK_HANDLE)
  !
  RETURN
  !
9999 CONTINUE
  !
  CALL ABOR1_SFX('End of file in read of .csv file')
  !
CONTAINS
  !
  SUBROUTINE READ_DEF_IN_CSV(HPROGRAM,ILUNAM,HFILE,YNAME,NTP,YTP)
    !
    ! Read definitions in .csv file
    !
    CHARACTER(LEN=6) , INTENT(IN) :: HPROGRAM
    CHARACTER(LEN=28), INTENT(IN) :: HFILE
    CHARACTER(LEN=*) , INTENT(IN) :: YNAME
    !
    INTEGER, INTENT(OUT) :: NTP
    INTEGER, INTENT(OUT) :: ILUNAM
    CHARACTER(LEN=50), ALLOCATABLE, INTENT(OUT) :: YTP(:)
    !
    CHARACTER(LEN=2000) :: YSTRING
    CHARACTER(LEN=50)   :: YSTREAD
    INTEGER             :: KK,LL
    !
    CALL OPEN_NAMELIST(HPROGRAM,ILUNAM,HFILE)
    !
    DO KK=1,1000
       !
       READ(ILUNAM,END=9999,FMT='(A2000)') YSTRING
       !
       YSTREAD=YSTRING(1:(INDEX(YSTRING,",")-1))
       YSTRING=YSTRING((INDEX(YSTRING,",")+1):LEN(YSTRING))
       !
       IF (YSTREAD.EQ.YNAME) THEN
          !
          YSTREAD=TRIM(YSTRING(1:(INDEX(YSTRING,",")-1)))
          READ(YSTREAD,*) NTP
          ALLOCATE(YTP(1:NTP))
          !
          DO LL=1,NTP
             IF (INDEX(YSTRING,",").EQ.0) CALL ABOR1_SFX('Wrong length of input vector')
             YSTRING=YSTRING((INDEX(YSTRING,",")+1):LEN(YSTRING))
             IF (LL.LT.NTP) THEN
                YTP(LL)=YSTRING(1:(INDEX(YSTRING,",")-1))
             ELSE
                IF (INDEX(YSTRING,",").EQ.0) THEN
                   YTP(LL)=TRIM(YSTRING)
                ELSE
                   YTP(LL)=YSTRING(1:(INDEX(YSTRING,",")-1))
                ENDIF
             ENDIF
          ENDDO
          !
          EXIT
          !
       ENDIF
    ENDDO
    !
    CALL CLOSE_NAMELIST(HPROGRAM,ILUNAM)
    !
    RETURN
    !
9999 CONTINUE
    !
    CALL ABOR1_SFX('Requested input not found in .csv file')
    !
  END SUBROUTINE READ_DEF_IN_CSV
  !
  SUBROUTINE CHECKINP(YTP,YTYPE)
    !
    IMPLICIT NONE
    !
    CHARACTER(LEN=50), DIMENSION(:), INTENT(IN) :: YTP(:)
    CHARACTER(LEN=50), INTENT(IN)               :: YTYPE
    !
    INTEGER :: NCTLVA,KK
    !
    NCTLVA=0
    DO KK=1,SIZE(YTP)
       IF(YTYPE.EQ.YTP(KK)) NCTLVA=NCTLVA+1
    ENDDO
    IF (NCTLVA.NE.1) CALL ABOR1_SFX('Unknown input value')
    !
  END SUBROUTINE CHECKINP
  !
  SUBROUTINE GETPCHAR(YMTP,ZHEACAP,ZHEACON,ZALBEDO,ZEMISSI,YCIN,ZHC,ZTC,ZAL,ZEM)
    !
    IMPLICIT NONE
    !
    CHARACTER (LEN=*), INTENT (INOUT) :: YCIN
    CHARACTER (LEN=*), INTENT (IN)    :: YMTP(:)
    !
    REAL, INTENT(IN) :: ZHEACAP(:)
    REAL, INTENT(IN) :: ZHEACON(:)
    REAL, INTENT(IN) :: ZALBEDO(:)
    REAL, INTENT(IN) :: ZEMISSI(:)
    !
    REAL, INTENT(OUT) :: ZHC
    REAL, INTENT(OUT) :: ZTC
    REAL, INTENT(OUT) :: ZAL
    REAL, INTENT(OUT) :: ZEM
    !
    INTEGER :: NCTLVA
    !
    ! Exception for glass as wall or roof structure type.
    ! In this case the values for steel are taken.
    ! This works only for the walls since there is no roof glazing implemented.
    ! The roof will thus be treated as steel.
    !
    IF ((YCIN.EQ."SIMPLE VITRAGE").OR.(YCIN.EQ."DOUBLE VITRAGE").OR.(YCIN.EQ."Tinted glass")) THEN
       YCIN="BARDAGE METAL (acier)"
    ENDIF
    !
    ! Attribution of material characteristics
    !
    NCTLVA=0
    DO LL=1,SIZE(YMTP)
       IF (YMTP(LL).EQ.YCIN) THEN
          NCTLVA=NCTLVA+1
          ZHC=ZHEACAP(LL)
          ZTC=ZHEACON(LL)
          ZAL=ZALBEDO(LL)
          ZEM=ZEMISSI(LL)
       ENDIF
    ENDDO
    !
    ! Check whether material has been found
    !
    IF (NCTLVA.EQ.0) CALL ABOR1_SFX('Material type not found')
    IF (NCTLVA.GT.1) CALL ABOR1_SFX('More than one material type found')
    !
  END SUBROUTINE GETPCHAR
  !
END SUBROUTINE READ_CSVDATA_ARCHI_TEB
