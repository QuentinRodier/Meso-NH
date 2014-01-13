!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########################
      SUBROUTINE READ_CSVDATA_TEB(HPROGRAM,HFILE)
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
!!    Original    05/2012 
!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_CSTS,     ONLY : XTT
USE MODD_SURF_PAR, ONLY : XUNDEF
USE MODD_TEB_n, ONLY : NROOF_LAYER, NWALL_LAYER, NROAD_LAYER
USE MODD_BEM_n, ONLY : NFLOOR_LAYER
!
USE MODD_BLD_DESCRIPTION
!
USE MODI_OPEN_NAMELIST
USE MODI_CLOSE_NAMELIST
!
USE MODI_GET_LUOUT
USE MODI_ABOR1_SFX
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
 CHARACTER(LEN=6),  INTENT(IN) :: HPROGRAM
 CHARACTER(LEN=28), INTENT(IN) :: HFILE    ! file to read
!
!
!*    0.2    Declaration of local variables
!      ------------------------------
!
INTEGER            :: ILUNAM       ! logical unit of the file
INTEGER            :: ILUOUT       ! logical unit of the output listing file
 CHARACTER(LEN=400) :: YSTRING
 CHARACTER(LEN=80)  :: YSTRING1, YSTRING2, YSTRING3, YSTRING4, &
                      YSTRING5, YSTRING6, YSTRING7, YSTRING8, YSTRING9
 CHARACTER(LEN=30), DIMENSION(:),   ALLOCATABLE :: YUSE_NAME ! building's use name
 CHARACTER(LEN=30), DIMENSION(:),   ALLOCATABLE :: YBLD_NAME ! building name
 CHARACTER(LEN=30), DIMENSION(:),   ALLOCATABLE :: YLAYER    ! name of layer
INTEGER            :: I1
INTEGER            :: I2
INTEGER            :: JBLD                ! loop counter on buildings
INTEGER            :: JAGE                ! loop counter on building's ages
INTEGER            :: JLAYER              ! loop counter on layers
INTEGER            :: IINDEX              ! index in descriptive data arrays
!
INTEGER            :: IALL_HYP            ! number of hypotheses for equipment
INTEGER            :: IHYP                ! kept hypothese for equipment
 CHARACTER(LEN=10)  :: YTYPE_OF_DATA       ! 'STRUCTURE', 'EQUIPMENT'
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('READ_CSVDATA_TEB',0,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
IF (LEN_TRIM(HFILE)==0) THEN
  IF (LHOOK) CALL DR_HOOK('READ_CSVDATA_TEB',1,ZHOOK_HANDLE)
  RETURN
END IF
!
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
!-------------------------------------------------------------------------------
!
!*    1.     Opens the file
!      --------------------
!
 CALL OPEN_NAMELIST(HPROGRAM,ILUNAM,HFILE)
!
!-------------------------------------------------------------------------------
!
!*    2.1    Reads the number of building types and of construction dates
!            ------------------------------------------------------------
!
 CALL READ_CONF_IN_CSVFILE("Nb de types de batiments",NDESC_BLD)
 CALL READ_CONF_IN_CSVFILE("Nb de plages de dates",NDESC_AGE)
 CALL READ_CONF_IN_CSVFILE("Nb de types d_usages",NDESC_USE)
!
NDESC_CODE = NDESC_BLD * NDESC_AGE
!
!*    2.2    Reads the number of layers for description of the surfaces
!            ----------------------------------------------------------
!
 CALL READ_CONF_IN_CSVFILE("Nb de couches MUR",NDESC_WALL_LAYER)
 CALL READ_CONF_IN_CSVFILE("Nb de couches TOITURE",NDESC_ROOF_LAYER)
 CALL READ_CONF_IN_CSVFILE("Nb de couches PLANCHER",NDESC_FLOOR_LAYER)
!
!-------------------------------------------------------------------------------
!
!*    3.     Reads the codes of the building construction (or renovation) dates
!            ------------------------------------------------------------------
!
ALLOCATE(NDESC_AGE_LIST(NDESC_AGE))
ALLOCATE(NDESC_AGE_DATE(NDESC_AGE))
!
DO
  YSTRING1=' '
  YSTRING2=' '
  YSTRING3=' '
  YSTRING4=' '
  YSTRING5=' '
  YSTRING6=' '
!* reads the record
  READ(ILUNAM,END=98,FMT='(A400)') YSTRING
!* analyses if the record has been written in French convention 
  CALL FRENCH_TO_ENGLISH(YSTRING)
!* reads the string
  IF (LEN_TRIM(YSTRING)>0) &
  READ(YSTRING,FMT=*) YSTRING1, YSTRING2, YSTRING3, YSTRING4, YSTRING5, YSTRING6
  IF (YSTRING1=='DATE' .AND. YSTRING2(4:)=='plage de date') THEN
    READ(YSTRING,FMT=*) YSTRING1, YSTRING2, NDESC_AGE_LIST(:)
  END IF
  IF (YSTRING1=='DATE' .AND. YSTRING2(:)=='Date maximum') THEN
    READ(YSTRING,FMT=*) YSTRING1, YSTRING2, NDESC_AGE_DATE(:)
  END IF
END DO
!
98 REWIND(ILUNAM)
!
!
!
!-------------------------------------------------------------------------------
!
!*    3.     Reads the codes of the building and building's use types
!            --------------------------------------------------------
!
ALLOCATE(YBLD_NAME(NDESC_BLD))
ALLOCATE(YUSE_NAME(NDESC_USE))
!
ALLOCATE(NDESC_BLD_LIST(NDESC_BLD))
ALLOCATE(NDESC_CODE_LIST(NDESC_CODE))
ALLOCATE(NDESC_USE_LIST(NDESC_USE))
!
DO
  YSTRING1=' '
  YSTRING2=' '
  YSTRING3=' '
  YSTRING4=' '
  YSTRING5=' '
  YSTRING6=' '
!* reads the record
  READ(ILUNAM,END=99,FMT='(A400)') YSTRING
!* analyses if the record has been written in French convention 
  CALL FRENCH_TO_ENGLISH(YSTRING)
!* reads the string
  IF (LEN_TRIM(YSTRING)>0) &
  READ(YSTRING,FMT=*) YSTRING1, YSTRING2, YSTRING3, YSTRING4, YSTRING5, YSTRING6

  IF (YSTRING1=='TYPES USAGES' .AND. YSTRING4=='TYPES BATIMENTS') THEN
    ! reads both use and building types
    DO JBLD=1,MAX(NDESC_BLD,NDESC_USE)
      READ(ILUNAM,FMT='(A400)') YSTRING
      CALL FRENCH_TO_ENGLISH(YSTRING)
      READ(YSTRING,FMT=*) YSTRING1, I1, YSTRING3, YSTRING4, I2, YSTRING6
    ! updates building types
      IF (JBLD<=NDESC_BLD) THEN
        YBLD_NAME(JBLD) = YSTRING4
        NDESC_BLD_LIST(JBLD) = I2
        DO JAGE=1,NDESC_AGE
          IINDEX = (JBLD-1)*NDESC_AGE + JAGE
          NDESC_CODE_LIST(IINDEX) = BLD_CODE(NDESC_BLD_LIST(JBLD),NDESC_AGE_LIST(JAGE))
        END DO
      END IF
    ! updates building's use types
      IF (JBLD<=NDESC_USE) THEN
        YUSE_NAME     (JBLD) = YSTRING1
        NDESC_USE_LIST(JBLD) = I1
      END IF
    END DO
    EXIT
  END IF
END DO
!
99 REWIND(ILUNAM)
!
!------------------------------------------------------------------------------
!
!*    4.     town parameters depending on building structure descriptions
!      ------------------------------------------------------------------
!
YTYPE_OF_DATA = 'STRUCTURE'
!
!* radiative properties
!
ALLOCATE(XDESC_ALB_ROOF(NDESC_CODE))
 CALL READ_IN_CSVFILE('TOITURE',YBLD_NAME,"Exterieur",'Albedo',XDESC_ALB_ROOF)

ALLOCATE(XDESC_ALB_WALL(NDESC_CODE))
 CALL READ_IN_CSVFILE('MUR',YBLD_NAME,"Couche 1 (Ext)",'Albedo',XDESC_ALB_WALL)

ALLOCATE(XDESC_EMIS_ROOF(NDESC_CODE))
 CALL READ_IN_CSVFILE('TOITURE',YBLD_NAME,"Exterieur",'Emissivite',XDESC_EMIS_ROOF)

ALLOCATE(XDESC_EMIS_WALL(NDESC_CODE))
 CALL READ_IN_CSVFILE('MUR',YBLD_NAME,"Couche 1 (Ext)",'Emissivite',XDESC_EMIS_WALL)
!
!* thermal properties for roof
!
ALLOCATE(YLAYER(NDESC_ROOF_LAYER))
DO JLAYER=1,NDESC_ROOF_LAYER
  IF (JLAYER==1) THEN
    WRITE(YLAYER(JLAYER),FMT='(A)') 'Exterieur '
  ELSEIF (JLAYER==NDESC_ROOF_LAYER) THEN
    WRITE(YLAYER(JLAYER),FMT='(A)') 'Interieur '
  ELSE
    WRITE(YLAYER(JLAYER),FMT='(A)') 'Milieu '
  END IF
END DO
!
ALLOCATE(XDESC_HC_ROOF(NDESC_CODE,NDESC_ROOF_LAYER))
ALLOCATE(XDESC_TC_ROOF(NDESC_CODE,NDESC_ROOF_LAYER))
ALLOCATE(XDESC_D_ROOF (NDESC_CODE,NDESC_ROOF_LAYER))
DO JLAYER=1,NDESC_ROOF_LAYER
  CALL READ_IN_CSVFILE('TOITURE',YBLD_NAME,YLAYER(JLAYER),'Chaleur specifique C',XDESC_HC_ROOF(:,JLAYER))
  CALL READ_IN_CSVFILE('TOITURE',YBLD_NAME,YLAYER(JLAYER),'Conductivite',XDESC_TC_ROOF(:,JLAYER))
  CALL READ_IN_CSVFILE('TOITURE',YBLD_NAME,YLAYER(JLAYER),'Epaisseur d',XDESC_D_ROOF(:,JLAYER))
END DO
!* transformation from kJ.m-3.K-1 to J.m-3.K-1
XDESC_HC_ROOF = XDESC_HC_ROOF * 1000.
DEALLOCATE(YLAYER)
!
!* thermal properties for wall
!
ALLOCATE(YLAYER(NDESC_WALL_LAYER))
DO JLAYER=1,NDESC_WALL_LAYER
  IF (JLAYER==1) THEN
    WRITE(YLAYER(JLAYER),FMT='(A,I1,A)') 'Couche ',JLAYER,' (Ext)'
  ELSEIF (JLAYER==NDESC_WALL_LAYER) THEN
    WRITE(YLAYER(JLAYER),FMT='(A,I1,A)') 'Couche ',JLAYER,' (Int)'
  ELSE
    WRITE(YLAYER(JLAYER),FMT='(A,I1,A)') 'Couche ',JLAYER,' (Milieu)'
  END IF
END DO
!
ALLOCATE(XDESC_HC_WALL(NDESC_CODE,NDESC_WALL_LAYER))
ALLOCATE(XDESC_TC_WALL(NDESC_CODE,NDESC_WALL_LAYER))
ALLOCATE(XDESC_D_WALL (NDESC_CODE,NDESC_WALL_LAYER))
DO JLAYER=1,NDESC_WALL_LAYER
  CALL READ_IN_CSVFILE('MUR',YBLD_NAME,YLAYER(JLAYER),'Chaleur specifique C',XDESC_HC_WALL(:,JLAYER))
  CALL READ_IN_CSVFILE('MUR',YBLD_NAME,YLAYER(JLAYER),'Conductivite',XDESC_TC_WALL(:,JLAYER))
  CALL READ_IN_CSVFILE('MUR',YBLD_NAME,YLAYER(JLAYER),'Epaisseur d',XDESC_D_WALL(:,JLAYER))
END DO
!* transformation from kJ.m-3.K-1 to J.m-3.K-1
XDESC_HC_WALL = XDESC_HC_WALL * 1000.
DEALLOCATE(YLAYER)
!
!
!* thermal properties for floor
!
ALLOCATE(YLAYER(NDESC_FLOOR_LAYER))
DO JLAYER=1,NDESC_FLOOR_LAYER
  IF (JLAYER==1) THEN
    WRITE(YLAYER(JLAYER),FMT='(A)') 'Superieur '
  ELSEIF (JLAYER==NDESC_FLOOR_LAYER) THEN
    WRITE(YLAYER(JLAYER),FMT='(A)') 'Inferieur '
  ELSE
    WRITE(YLAYER(JLAYER),FMT='(A)') 'Milieu '
  END IF
END DO
!
ALLOCATE(XDESC_HC_FLOOR(NDESC_CODE,NDESC_FLOOR_LAYER))
ALLOCATE(XDESC_TC_FLOOR(NDESC_CODE,NDESC_FLOOR_LAYER))
ALLOCATE(XDESC_D_FLOOR (NDESC_CODE,NDESC_FLOOR_LAYER))
DO JLAYER=1,NDESC_FLOOR_LAYER
  CALL READ_IN_CSVFILE('PLANCHER',YBLD_NAME,YLAYER(JLAYER),'Chaleur specifique C',XDESC_HC_FLOOR(:,JLAYER))
  CALL READ_IN_CSVFILE('PLANCHER',YBLD_NAME,YLAYER(JLAYER),'Conductivite',XDESC_TC_FLOOR(:,JLAYER))
  CALL READ_IN_CSVFILE('PLANCHER',YBLD_NAME,YLAYER(JLAYER),'Epaisseur d',XDESC_D_FLOOR(:,JLAYER))
END DO
!* transformation from kJ.m-3.K-1 to J.m-3.K-1
XDESC_HC_FLOOR = XDESC_HC_FLOOR * 1000.
DEALLOCATE(YLAYER)
!
!* windows
!  -------
!
ALLOCATE(XDESC_SHGC(NDESC_CODE))
 CALL READ_IN_CSVFILE('ENVELOPPE',YBLD_NAME,"Vitrage",'Facteur solaire m',XDESC_SHGC)

ALLOCATE(XDESC_U_WIN(NDESC_CODE))
 CALL READ_IN_CSVFILE('ENVELOPPE',YBLD_NAME,"Vitrage",'U-factor',XDESC_U_WIN)

ALLOCATE(XDESC_GR(NDESC_CODE))
 CALL READ_IN_CSVFILE('ENVELOPPE',YBLD_NAME,"Vitrage",'Surface fenetre /surface facade',XDESC_GR)
!
!------------------------------------------------------------------------------
!
!*    5.     town parameters depending on building equipment descriptions
!      ------------------------------------------------------------------
!
YTYPE_OF_DATA = 'EQUIPMENT'
!
 CALL READ_CONF_IN_CSVFILE("Nb d_hypotheses",IALL_HYP)
!
!* Air conditionning systems
!
 CALL READ_HYP_IN_CSVFILE("Climatisation","Taux de rejets en toitures",IHYP)
ALLOCATE(XDESC_F_WASTE_CAN(NDESC_CODE))
 CALL READ_IN_CSVFILE('EQUIPEMENT',YBLD_NAME,"Climatisation",'Taux de rejets en toitures',XDESC_F_WASTE_CAN)
XDESC_F_WASTE_CAN = XDESC_F_WASTE_CAN / 100. ! % => fraction
!
 CALL READ_HYP_IN_CSVFILE("Climatisation","Taux de rejets secs",IHYP)
ALLOCATE(XDESC_F_WATER_COND(NDESC_CODE))
 CALL READ_IN_CSVFILE('EQUIPEMENT',YBLD_NAME,"Climatisation",'Taux de rejets secs',XDESC_F_WATER_COND)
XDESC_F_WATER_COND = XDESC_F_WATER_COND / 100. ! % => fraction
!
 CALL READ_HYP_IN_CSVFILE("Climatisation","Performance (COP)",IHYP)
ALLOCATE(XDESC_COP_RAT(NDESC_CODE))
 CALL READ_IN_CSVFILE('EQUIPEMENT',YBLD_NAME,"Climatisation",'Performance (COP)',XDESC_COP_RAT)
!
!
!* Heating systems
!
!CALL READ_HYP_IN_CSVFILE("Chauffage","Efficacite energetique",IHYP)
ALLOCATE(XDESC_EFF_HEAT(NDESC_CODE))
!CALL READ_IN_CSVFILE('EQUIPEMENT',YBLD_NAME,"Chauffage",'Efficacite energetique',XDESC_EFF_HEAT)
XDESC_EFF_HEAT = 0.9
!
!
!* Sanitary ventilation
 CALL READ_HYP_IN_CSVFILE("Infiltration","Taux de renouvellement d_air",IHYP)
ALLOCATE(XDESC_INF(NDESC_CODE))
 CALL READ_IN_CSVFILE('EQUIPEMENT',YBLD_NAME,"Infiltration","Taux de renouvellement d_air",XDESC_INF)
!
 CALL READ_HYP_IN_CSVFILE("Ventilation Mecanique Controlee","Taux de renouvellement d_air",IHYP)
ALLOCATE(XDESC_V_VENT(NDESC_CODE))
 CALL READ_IN_CSVFILE('EQUIPEMENT',YBLD_NAME,"Ventilation Mecanique Controlee","Taux de renouvellement d_air",XDESC_V_VENT)
!
!* Greenroof fraction
 CALL READ_HYP_IN_CSVFILE("Toits vegetalises","Implantation",IHYP)
ALLOCATE(XDESC_GREENROOF(NDESC_CODE))
 CALL READ_IN_CSVFILE('EQUIPEMENT',YBLD_NAME,"Toits vegetalises","Implantation",XDESC_GREENROOF)
!-------------------------------------------------------------------------------

!------------------------------------------------------------------------------
!
!*    7.     town parameters depending on building's use descriptions
!      --------------------------------------------------------------
!
!
YTYPE_OF_DATA = 'USE'
!
!* Temperature target for air conditionning
 CALL READ_HYP_IN_CSVFILE("Climatisation","Temp. de consigne",IHYP)
ALLOCATE(XDESC_TCOOL_TARGET(NDESC_USE))
 CALL READ_IN_CSVFILE('USAGE',YUSE_NAME,"Climatisation","Temp. de consigne",XDESC_TCOOL_TARGET)
XDESC_TCOOL_TARGET = XDESC_TCOOL_TARGET + XTT ! °C => K
!
!* Temperature target for domestic heating
 CALL READ_HYP_IN_CSVFILE("Chauffage","Temp. de consigne",IHYP)
ALLOCATE(XDESC_THEAT_TARGET(NDESC_USE))
 CALL READ_IN_CSVFILE('USAGE',YUSE_NAME,"Chauffage","Temp. de consigne",XDESC_THEAT_TARGET)
XDESC_THEAT_TARGET = XDESC_THEAT_TARGET + XTT ! °C => K
!
!* Internal gains
 CALL READ_HYP_IN_CSVFILE("Apports internes","Flux",IHYP)
ALLOCATE(XDESC_QIN(NDESC_USE))
 CALL READ_IN_CSVFILE('USAGE',YUSE_NAME,"Apports internes","Flux",XDESC_QIN)
!
!* Latent fraction for internal gains
 CALL READ_HYP_IN_CSVFILE("Apports internes","Fraction latente",IHYP)
ALLOCATE(XDESC_QIN_FLAT(NDESC_USE))
 CALL READ_IN_CSVFILE('USAGE',YUSE_NAME,"Apports internes","Fraction latente",XDESC_QIN_FLAT)
XDESC_QIN_FLAT = XDESC_QIN_FLAT / 100. ! % => fraction
!
!* Solar protections
 CALL READ_HYP_IN_CSVFILE("Protection solaire","Facteur solaire m",IHYP)
ALLOCATE(XDESC_SHGC_SH(NDESC_USE))
 CALL READ_IN_CSVFILE('USAGE',YUSE_NAME,"Protection solaire","Facteur solaire m",XDESC_SHGC_SH)
!
 CALL READ_HYP_IN_CSVFILE("Protection solaire","Active",IHYP)
ALLOCATE(XDESC_SHADE(NDESC_USE))
 CALL READ_IN_CSVFILE('USAGE',YUSE_NAME,"Protection solaire","Active",XDESC_SHADE)
!
!* Extra Natural ventilation (windows open or extra mechanical ventilation)
 CALL READ_HYP_IN_CSVFILE("Sur-ventilation","Type d_ouverture",IHYP)
ALLOCATE(XDESC_NATVENT(NDESC_USE))
 CALL READ_IN_CSVFILE('USAGE',YUSE_NAME,"Sur-ventilation","Type d_ouverture",XDESC_NATVENT)
!
!------------------------------------------------------------------------------
!
!*    8.     town parameters depending on urban structure
!      --------------------------------------------------
!
NDESC_ROAD_LAYER = 3
!
ALLOCATE(XDESC_ALB_ROAD(NDESC_CODE))
XDESC_ALB_ROAD = 0.08
ALLOCATE(XDESC_EMIS_ROAD(NDESC_CODE))
XDESC_EMIS_ROAD = 0.94
ALLOCATE(XDESC_HC_ROAD(NDESC_CODE,NDESC_ROAD_LAYER))
XDESC_HC_ROAD(:,1)  = 1940000.
XDESC_HC_ROAD(:,2:) = 1280000.
ALLOCATE(XDESC_TC_ROAD(NDESC_CODE,NDESC_ROAD_LAYER))
XDESC_TC_ROAD(:,1)  = 0.74
XDESC_TC_ROAD(:,2:) = 0.25
ALLOCATE(XDESC_D_ROAD(NDESC_CODE,NDESC_ROAD_LAYER))
XDESC_D_ROAD(:,1)  = 0.05
XDESC_D_ROAD(:,2)  = 0.1
XDESC_D_ROAD(:,3:) = 1.
!
!-------------------------------------------------------------------------------
!
 CALL CLOSE_NAMELIST(HPROGRAM,ILUNAM)
!
IF (LHOOK) CALL DR_HOOK('READ_CSVDATA_TEB',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
CONTAINS
!-------------------------------------------------------------------------------
!
FUNCTION BLD_CODE(KBLD,KAGE)
INTEGER, INTENT(IN) :: KBLD     ! building type number
INTEGER, INTENT(IN) :: KAGE     ! building construction period number
INTEGER             :: BLD_CODE ! building code combining type and age
BLD_CODE = 100*KBLD+KAGE
END FUNCTION BLD_CODE
!
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------
!
SUBROUTINE READ_CONF_IN_CSVFILE(HCODE1,KDATA)

 CHARACTER(LEN=*), INTENT(IN) :: HCODE1
INTEGER,          INTENT(OUT):: KDATA
 CHARACTER(LEN=80) :: YERROR
!
REWIND(ILUNAM)
DO
  YSTRING1 = ''
  YSTRING2 = ''
!* reads the record
 READ(ILUNAM,END=101,FMT='(A400)') YSTRING
!* analyses if the record has been written in French convention 
  CALL FRENCH_TO_ENGLISH(YSTRING)
!* reads the string
  IF (LEN_TRIM(YSTRING)>0) &
  READ(YSTRING,FMT=*) YSTRING1, YSTRING2

  IF (TRIM(YSTRING1)==TRIM(HCODE1)) THEN
    READ(YSTRING,*) YSTRING1, KDATA
    REWIND(ILUNAM)
    RETURN
  END IF
END DO
!
101 YERROR=TRIM(HCODE1)//' not found in file : '//TRIM(HFILE)
 CALL ABOR1_SFX(YERROR)
!
END SUBROUTINE READ_CONF_IN_CSVFILE
!
SUBROUTINE READ_HYP_IN_CSVFILE(HCODE1,HCODE2,KDATA)

 CHARACTER(LEN=*), INTENT(IN) :: HCODE1
 CHARACTER(LEN=*), INTENT(IN) :: HCODE2
INTEGER,          INTENT(OUT):: KDATA
 CHARACTER(LEN=80) :: YERROR
LOGICAL           :: GCODE2
!
REWIND(ILUNAM)
DO
  YSTRING1 = ''
  YSTRING2 = ''
!* reads the record
  READ(ILUNAM,END=101,FMT='(A400)') YSTRING
!* analyses if the record has been written in French convention 
  CALL FRENCH_TO_ENGLISH(YSTRING)
!* reads the string
  IF (LEN_TRIM(YSTRING)>0) &
  READ(YSTRING,FMT=*) YSTRING1, YSTRING2

  GCODE2 = TRIM(YSTRING2)==TRIM(HCODE2)
  IF (TRIM(YSTRING1)==TRIM(HCODE1) .AND. GCODE2) THEN
    READ(YSTRING,*) YSTRING1, YSTRING2, KDATA
    REWIND(ILUNAM)
    RETURN
  END IF
END DO
!
101 YERROR=TRIM(HCODE1)//' '//TRIM(HCODE2)//' not found in file : '//TRIM(HFILE)
 CALL ABOR1_SFX(YERROR)
!
END SUBROUTINE READ_HYP_IN_CSVFILE
!
SUBROUTINE READ_IN_CSVFILE(HCODE_ELEMENT,HCODE_TYPE,HCODE_ELEMENT2,HCODE_PARAM,PDATA)
!
 CHARACTER(LEN=*),               INTENT(IN) :: HCODE_ELEMENT  ! type of element
 CHARACTER(LEN=*), DIMENSION(:), INTENT(IN) :: HCODE_TYPE     ! building type or
                                                             ! building's use type
 CHARACTER(LEN=*),               INTENT(IN) :: HCODE_ELEMENT2 ! description of element
 CHARACTER(LEN=*),               INTENT(IN) :: HCODE_PARAM    ! name of Parameter
REAL, DIMENSION(:),             INTENT(OUT):: PDATA          ! data read in the csv file
!
REAL, DIMENSION(:), ALLOCATABLE      :: ZDATA        ! data array read in the file
LOGICAL, DIMENSION(SIZE(HCODE_TYPE)) :: GINITIALIZED ! Flag to know if parameter
!                                                    ! has been initialized correctly
LOGICAL                          :: GFOUND ! correct record has been found
 CHARACTER(LEN=80)                :: YTYPE  ! type of building or building's use
!                                          ! in the csv file record
 CHARACTER(LEN=100)               :: YERROR ! Character string for error message
INTEGER                          :: IN1 ! number of building type or use
INTEGER                          :: IN2 ! number of construction dates
!
IF (YTYPE_OF_DATA=='STRUCTURE') THEN
  ALLOCATE(ZDATA(NDESC_AGE))
  IN1=NDESC_BLD
  IN2=NDESC_AGE
ELSE IF (YTYPE_OF_DATA=='EQUIPMENT') THEN
  ALLOCATE(ZDATA(IALL_HYP))
  IN1=NDESC_BLD
  IN2=NDESC_AGE
ELSE IF (YTYPE_OF_DATA=='USE') THEN
  ALLOCATE(ZDATA(IALL_HYP))
  IN1=NDESC_USE
  IN2=1
END IF
!
PDATA = XUNDEF
GINITIALIZED(:)=.FALSE.
DO
  YSTRING1=' '
  YSTRING2=' '
  YSTRING3=' '
  YSTRING4=' '
  YSTRING5=' '
  YSTRING6=' '
  YSTRING7=' '
  YSTRING8=' '
!* reads the record
  READ(ILUNAM,END=100,FMT='(A400)') YSTRING
!* analyses if the record has been written in French convention 
  CALL FRENCH_TO_ENGLISH(YSTRING)
!* reads the string
  IF (LEN_TRIM(YSTRING)>0) &
  READ(YSTRING,FMT=*) YSTRING1, YSTRING2, YSTRING3, YSTRING4, YSTRING5, YSTRING6, YSTRING7
  !
  IF (YTYPE_OF_DATA=='EQUIPMENT' .OR. YTYPE_OF_DATA=='USE') THEN
    GFOUND = TRIM(YSTRING1)==TRIM(HCODE_ELEMENT) .AND. TRIM(YSTRING6)==TRIM(HCODE_ELEMENT2) &
                                                 .AND. TRIM(YSTRING7)==TRIM(HCODE_PARAM)
  ELSE IF (YTYPE_OF_DATA=='STRUCTURE') THEN
    GFOUND = TRIM(YSTRING1)==TRIM(HCODE_ELEMENT) .AND. TRIM(YSTRING4)==TRIM(HCODE_ELEMENT2) &
                                                 .AND. TRIM(YSTRING5)==TRIM(HCODE_PARAM)
  ELSE
    GFOUND = .FALSE.
  END IF

  IF (GFOUND) THEN
!* reads the data in the record
  IF (YTYPE_OF_DATA=='EQUIPMENT' .OR. YTYPE_OF_DATA=='USE') THEN
    READ(YSTRING,FMT=*) YSTRING1, YSTRING2, YSTRING3, YSTRING4, YSTRING5, &
                        YSTRING6, YSTRING7, YSTRING8, YSTRING9, ZDATA(:)
  ELSE IF (YTYPE_OF_DATA=='STRUCTURE') THEN
    READ(YSTRING,FMT=*) YSTRING1, YSTRING2, YSTRING3, YSTRING4, YSTRING5, &
                        YSTRING6, YSTRING7, ZDATA(:)
  END IF
!* in case of EQUIPMENT or USE data, one keeps the chosen hypothesis
    IF (YTYPE_OF_DATA=='EQUIPMENT' .OR. YTYPE_OF_DATA=='USE') ZDATA(:) = ZDATA(IHYP)
!* one finds for which building type or building's use type the data is for
    IF (YTYPE_OF_DATA=='EQUIPMENT') YTYPE = YSTRING2
    IF (YTYPE_OF_DATA=='STRUCTURE') YTYPE = YSTRING2
    IF (YTYPE_OF_DATA=='USE      ') YTYPE = YSTRING4
    !
    DO JBLD=1,IN1
      IF (TRIM(HCODE_TYPE(JBLD))==TRIM(YTYPE) .OR. TRIM(YTYPE)=='Tous batiments') THEN
!* one affects the data for this type of building for each construction dates
        DO JAGE=1,IN2
          IINDEX = (JBLD-1)*IN2 + JAGE
          PDATA(IINDEX) = ZDATA(MIN(JAGE,SIZE(ZDATA)))
        END DO
        GINITIALIZED(JBLD) = .TRUE.
      END IF
    END DO
  END IF
  IF (ALL(GINITIALIZED)) EXIT
END DO
!
100 REWIND(ILUNAM)
DEALLOCATE(ZDATA)
!
!* one checks if the data is available for all building's types
IF (ANY(.NOT. GINITIALIZED)) THEN
  WRITE(ILUOUT,*) '*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*'
  WRITE(ILUOUT,*) 'While reading the csv data file for building parameters specification'
  WRITE(ILUOUT,*) '(file ',TRIM(HFILE),')'
  WRITE(ILUOUT,*) 'The field corresponding to the following '
  WRITE(ILUOUT,*) 'identifiers:',TRIM(HCODE_ELEMENT),' ',TRIM(HCODE_ELEMENT2),' ',TRIM(HCODE_PARAM)
  WRITE(ILUOUT,*) 'has not been completely initialized.'
  WRITE(ILUOUT,*) 'The data for the following building types were not found:'
  IF (YTYPE_OF_DATA=='USE') THEN
    DO JBLD=1,IN1
      IF (.NOT. GINITIALIZED(JBLD)) WRITE(ILUOUT,*) '"',TRIM(YUSE_NAME(JBLD)),'"'
    END DO
  ELSE
    DO JBLD=1,IN1
      IF (.NOT. GINITIALIZED(JBLD)) WRITE(ILUOUT,*) '"',TRIM(YBLD_NAME(JBLD)),'"'
    END DO
  END IF
  WRITE(ILUOUT,*) '*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*'

  YERROR='Initialization not complete for: '//TRIM(HCODE_ELEMENT)//' '//TRIM(HCODE_ELEMENT2)//' '//TRIM(HCODE_PARAM)
  CALL ABOR1_SFX(YERROR)
END IF
!
END SUBROUTINE READ_IN_CSVFILE
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
SUBROUTINE FRENCH_TO_ENGLISH(HSTRING)
 CHARACTER(LEN=400), INTENT(INOUT) :: HSTRING ! csv record
INTEGER :: JL
LOGICAL :: GFRENCH
!
GFRENCH = .FALSE.
!* analyses if the record has been written in French convention 
!     French  convention (separator is ;  decimal symbol is ,) 
!  or English convention (separator is ,  decimal symbol is .)
DO JL=1,400
  IF (HSTRING(JL:JL)==';') GFRENCH=.TRUE.
END DO
!
! If French convention is used in the file, transforms it in English convention
IF (GFRENCH) THEN
  DO JL=1,400
    IF (HSTRING(JL:JL)==',') HSTRING(JL:JL)='.'
    IF (HSTRING(JL:JL)==';') HSTRING(JL:JL)=','
  END DO
END IF
!
END SUBROUTINE FRENCH_TO_ENGLISH
!-------------------------------------------------------------------------------
!
END SUBROUTINE READ_CSVDATA_TEB
