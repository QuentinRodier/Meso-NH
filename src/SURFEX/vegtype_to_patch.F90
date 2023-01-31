!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!#### ########################################################
SUBROUTINE VEGTYPE_TO_PATCH(KVEGTYPE,KNPATCH,KPATCH_NB,OECOSG,HPATCH_NAME)
!#############################################################
!
!!****  *VEGTYPE_TO_PATCH* - routine to attribute patch numbre (and name) with the nvegtype
!!
!!    PURPOSE
!!    -------
!
!  Calculation of patch indices coresponding to different  vegtype
!          according to the  number of patch  (NPATCH).
!             
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!    none
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!      
!!    none
!!
!!    REFERENCE
!!    ---------
!!
!!      
!!    AUTHOR
!!    ------
!    F.Solmon/V.Masson 06/00 
!
!!    MODIFICATIONS
!!    -------------
!    R. Alkama       05/2012 : new vegtypes (19 rather than 12)
!    S. Faroux ?     01/2018 : new vegtypes (20 rather than 19 in ECOCLIMAP-SG)
!    A. DRUEL        10/2018 : - new organisation: 20, 19, 12, 9, 3 & 1 maintained, other (11,10,8,7,6,5,4,2) removed
!                                                13, 10 and 7 add with new logical (physic)
!                              - change function into routine
!                              - add the optionnal name of patch and vegtype into output
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_DATA_COVER_PAR, ONLY : NVT_NO, NVT_ROCK, NVT_SNOW, NVT_TEBD,   & 
                                NVT_BONE, NVT_TRBE, NVT_C3, NVT_C4,     &
                                NVT_IRR, NVT_GRAS, NVT_TROG,NVT_PARK,   &
                                NVT_TRBD, NVT_TEBE, NVT_TENE, NVT_BOBD, &
                                NVT_BOND, NVT_BOGR, NVT_SHRB, NVT_C3W,  &
                                NVT_C3S, NVT_FLTR, NVT_FLGR, NVEGTYPE
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABOR1_SFX
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
INTEGER, INTENT(IN)  :: KVEGTYPE    ! indices of vegetation type           
INTEGER, INTENT(IN)  :: KNPATCH     ! total number of PATCHES used 
!
INTEGER, INTENT(OUT) :: KPATCH_NB   ! PATCH index corresponding to the vegtype KVEGTYPE
!
LOGICAL,           OPTIONAL, INTENT(IN)  :: OECOSG
CHARACTER(LEN=34), OPTIONAL, INTENT(OUT) :: HPATCH_NAME ! Name of patch (dim 1: patch name, dim 2: vegtype inside)
!
!*      0.2    declarations of local variables
!
CHARACTER(LEN=4), DIMENSION(NVEGTYPE) :: YVEG
CHARACTER(LEN=2)                      :: YVEGTYPE
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-----------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('VEGTYPE_TO_PATCH',0,ZHOOK_HANDLE)
!
IF (.NOT.(PRESENT(HPATCH_NAME).EQV.PRESENT(OECOSG))) THEN
   CALL ABOR1_SFX('VEGTYPE_TO_PATCH: THE TWO OPTION ARE LINK AND HAVE TO BE PRESENT TOGETHER!')
ENDIF
!
WRITE(YVEGTYPE,"(I2.2)") NVEGTYPE
!
! 1. Find the NPATCH NUMBER corresponding to NVEGTYPE (and NPATCH NAME if asked)
!
! No differentiation: all nevegtype are merged into 1.
IF (KNPATCH==1) THEN
  KPATCH_NB = 1 ! default case
  IF (PRESENT(HPATCH_NAME)) HPATCH_NAME = 'ALL VEGEGTYPE TOGETHER'
!
!differenciation between low-vegetation and high vegtation, (forest +shrubs)
ELSEIF (KNPATCH==2) THEN
   IF (PRESENT(HPATCH_NAME)) WRITE(*,*) "CAUTION: YOU CHOSE THE PATCH NUMBER = 2 (WITHOUT IRRIGATION)."
   IF (PRESENT(HPATCH_NAME)) WRITE(*,*) "         FOR BIO-PHYSICAL ASPECT WE RECOMMAND TO USE NPATCH = 3!!"
   IF ( KVEGTYPE== NVT_TEBD .OR. KVEGTYPE== NVT_TRBD .OR. KVEGTYPE== NVT_TEBE .OR. &
        KVEGTYPE== NVT_BOBD .OR. KVEGTYPE== NVT_SHRB .OR. KVEGTYPE== NVT_BONE .OR. &
        KVEGTYPE== NVT_TENE .OR. KVEGTYPE== NVT_BOND .OR. KVEGTYPE== NVT_TRBE .OR. &
        KVEGTYPE== NVT_FLTR ) THEN
     KPATCH_NB = 2
     IF (PRESENT(HPATCH_NAME)) HPATCH_NAME = 'HIGH VEGETATION (TREES & SHRUBS)'
   ELSE
     KPATCH_NB = 1
     IF (PRESENT(HPATCH_NAME)) HPATCH_NAME = 'LOW VEGETATION (GRASSES & NO VEG)'
   ENDIF
!
!differenciation between no vegetation, forest (+shrubs) and low vegetation
ELSEIF (KNPATCH==3) THEN
   IF (KVEGTYPE== NVT_NO   .OR. KVEGTYPE== NVT_ROCK .OR. KVEGTYPE== NVT_SNOW ) THEN 
     KPATCH_NB = 1
     IF (PRESENT(HPATCH_NAME)) HPATCH_NAME = 'NO VEGETATION'
   ELSEIF (KVEGTYPE== NVT_TEBD .OR. KVEGTYPE== NVT_TRBD .OR. KVEGTYPE== NVT_TEBE .OR. &
           KVEGTYPE== NVT_BOBD .OR. KVEGTYPE== NVT_SHRB .OR. KVEGTYPE== NVT_BONE .OR. &
           KVEGTYPE== NVT_TENE .OR. KVEGTYPE== NVT_BOND .OR. KVEGTYPE== NVT_TRBE .OR. &
           KVEGTYPE== NVT_FLTR ) THEN
     KPATCH_NB = 2
     IF (PRESENT(HPATCH_NAME)) HPATCH_NAME = 'TREES AND SHRUBS'
   ELSEIF (KVEGTYPE== NVT_GRAS .OR. KVEGTYPE== NVT_BOGR .OR. KVEGTYPE== NVT_TROG .OR. &
           KVEGTYPE== NVT_PARK .OR. KVEGTYPE== NVT_C3   .OR. KVEGTYPE== NVT_C3W  .OR. &
           KVEGTYPE== NVT_C3S  .OR. KVEGTYPE== NVT_C4   .OR. KVEGTYPE== NVT_IRR  .OR. &
           KVEGTYPE== NVT_FLGR ) THEN
     KPATCH_NB = 3
     IF (PRESENT(HPATCH_NAME)) HPATCH_NAME = 'HERBACEOUS'
   ENDIF
!
!differenciation between - No vegetation 
!                        - High vegetation (Broadleafs forest, needleleafs forest and shrubs)
!                        - Crops (+irrigated if old version before ECOSG) and Grassland (+park if old version before ECOSG)
ELSEIF (KNPATCH==5) THEN
   IF (KVEGTYPE== NVT_NO   .OR. KVEGTYPE== NVT_ROCK .OR. KVEGTYPE== NVT_SNOW ) THEN
     KPATCH_NB = 1
     IF (PRESENT(HPATCH_NAME)) HPATCH_NAME = 'NO VEGETATION'
   ELSEIF (KVEGTYPE== NVT_TEBD .OR. KVEGTYPE== NVT_TRBD .OR. KVEGTYPE== NVT_TEBE .OR. &
           KVEGTYPE== NVT_BOBD .OR. KVEGTYPE== NVT_SHRB .OR. KVEGTYPE== NVT_BONE .OR. &
           KVEGTYPE== NVT_TENE .OR. KVEGTYPE== NVT_BOND .OR. KVEGTYPE== NVT_TRBE .OR. &
           KVEGTYPE== NVT_FLTR ) THEN
     KPATCH_NB = 2
     IF (PRESENT(HPATCH_NAME)) HPATCH_NAME = 'TREES AND SHRUBS'
   ELSEIF (KVEGTYPE== NVT_C3  .OR. KVEGTYPE== NVT_C3W  .OR. &
           KVEGTYPE== NVT_C3S .OR. KVEGTYPE== NVT_C4 ) THEN
     KPATCH_NB = 3
     IF (PRESENT(HPATCH_NAME)) HPATCH_NAME = 'CROPS'
   ELSEIF (KVEGTYPE== NVT_IRR  .OR. KVEGTYPE== NVT_PARK .OR. KVEGTYPE== NVT_FLGR ) THEN
     KPATCH_NB = 4
     IF (PRESENT(HPATCH_NAME)) THEN
       IF ( OECOSG ) THEN
         HPATCH_NAME = 'FLOODED TREES'
       ELSE
         HPATCH_NAME = 'C4 CROPS IRRIGATED & PARKS'
       ENDIF
     ENDIF
   ELSEIF (KVEGTYPE== NVT_GRAS .OR. KVEGTYPE== NVT_BOGR .OR. KVEGTYPE== NVT_TROG ) THEN
     KPATCH_NB = 5
     IF (PRESENT(HPATCH_NAME)) HPATCH_NAME = 'GRASSLANDS'
   ENDIF
!
!differenciation between: - Bare fields and permanent snow
!  NEW DISTINCTION        - Broadleafs forest, needleleafs forest and shrubs
! (from KNPATCH==3)       - Crops (+irrigated if old version before ECOSG) and Grassland (+park if old version before ECOSG)
ELSEIF (KNPATCH==7) THEN
   IF (KVEGTYPE== NVT_NO   .OR. KVEGTYPE== NVT_ROCK ) THEN
     KPATCH_NB = 1
     IF (PRESENT(HPATCH_NAME)) HPATCH_NAME = 'BARE FIELD'
   ELSEIF (KVEGTYPE== NVT_SNOW ) THEN
     KPATCH_NB = 2
     IF (PRESENT(HPATCH_NAME)) HPATCH_NAME = 'PERMANENT SNOW'
   ELSEIF (KVEGTYPE== NVT_BOBD .OR. KVEGTYPE== NVT_TEBD .OR. KVEGTYPE== NVT_TRBD .OR. &
           KVEGTYPE== NVT_TEBE .OR. KVEGTYPE== NVT_TRBE .OR. KVEGTYPE== NVT_FLTR ) THEN
     KPATCH_NB = 3
     IF (PRESENT(HPATCH_NAME)) HPATCH_NAME = 'BROADLEAF TREES'
   ELSEIF (KVEGTYPE== NVT_BONE .OR. KVEGTYPE== NVT_TENE .OR. KVEGTYPE== NVT_BOND ) THEN
     KPATCH_NB = 4
     IF (PRESENT(HPATCH_NAME)) HPATCH_NAME = 'NEEDLELEAF TREES'
   ELSEIF (KVEGTYPE== NVT_SHRB ) THEN 
     KPATCH_NB = 5
     IF (PRESENT(HPATCH_NAME)) HPATCH_NAME = 'SHRUBS'
   ELSEIF (KVEGTYPE==  NVT_C3  .OR. KVEGTYPE== NVT_C3W  .OR. KVEGTYPE== NVT_C3S  .OR. &
           KVEGTYPE== NVT_C4   .OR. KVEGTYPE== NVT_IRR  .OR. KVEGTYPE== NVT_PARK ) THEN
     KPATCH_NB = 6
     IF (PRESENT(HPATCH_NAME)) HPATCH_NAME = 'CROPS'
   ELSEIF (KVEGTYPE== NVT_BOGR .OR. KVEGTYPE== NVT_GRAS .OR. KVEGTYPE== NVT_TROG .OR. &
           KVEGTYPE== NVT_FLGR ) THEN
     KPATCH_NB = 7 
     IF (PRESENT(HPATCH_NAME)) HPATCH_NAME = 'GRASSLANDS'
   ENDIF
!
!differenciation between: - Bare fields and permanent snow
!  OLD DISTINCTION        - Broadleafs(+shrubs), needleleafs and flooded tree cover
! (from KNPATCH==3)       - C3 crops, C4 crops(irrigated if old version), Grassland and wetlands 
ELSEIF (KNPATCH==9) THEN
   IF (KVEGTYPE== NVT_NO   .OR. KVEGTYPE== NVT_ROCK ) THEN
     KPATCH_NB = 1
     IF (PRESENT(HPATCH_NAME)) HPATCH_NAME = 'BARE FIELD'
   ELSEIF (KVEGTYPE== NVT_SNOW) THEN
     KPATCH_NB = 2
     IF (PRESENT(HPATCH_NAME)) HPATCH_NAME = 'PERMANENT SNOW'
   ELSEIF (KVEGTYPE== NVT_TEBD .OR. KVEGTYPE== NVT_TRBD .OR. KVEGTYPE== NVT_TEBE .OR. &
           KVEGTYPE== NVT_BOBD .OR. KVEGTYPE== NVT_SHRB .OR. KVEGTYPE== NVT_TRBE ) THEN
     KPATCH_NB = 3
     IF (PRESENT(HPATCH_NAME)) HPATCH_NAME = 'BROADLEAF TREES & SHRUBS'
   ELSEIF (KVEGTYPE== NVT_BONE .OR. KVEGTYPE== NVT_TENE .OR. KVEGTYPE== NVT_BOND ) THEN
     KPATCH_NB = 4
     IF (PRESENT(HPATCH_NAME)) HPATCH_NAME = 'NEEDLELEAF TREES'
   ELSEIF (KVEGTYPE== NVT_C3   .OR. KVEGTYPE== NVT_C3W  .OR. KVEGTYPE== NVT_C3S )  THEN
     KPATCH_NB = 5
     IF (PRESENT(HPATCH_NAME)) HPATCH_NAME = 'C3 CROPS'
   ELSEIF (KVEGTYPE== NVT_C4  ) THEN
     KPATCH_NB = 6
     IF (PRESENT(HPATCH_NAME)) HPATCH_NAME = 'C4 CROPS'
   ELSEIF (KVEGTYPE== NVT_IRR  .OR. KVEGTYPE== NVT_FLTR) THEN
     KPATCH_NB = 7
     IF (PRESENT(HPATCH_NAME)) THEN
       IF ( OECOSG ) THEN
         HPATCH_NAME = 'FLOODED TREES'
       ELSE
         HPATCH_NAME = 'C4 CROPS IRRIGATED'
       ENDIF
     ENDIF
   ELSEIF (KVEGTYPE== NVT_GRAS .OR. KVEGTYPE== NVT_BOGR .OR. KVEGTYPE== NVT_TROG)  THEN
     KPATCH_NB = 8
     IF (PRESENT(HPATCH_NAME)) HPATCH_NAME = 'GRASSLANDS'
   ELSEIF (KVEGTYPE== NVT_PARK .OR. KVEGTYPE== NVT_FLGR) THEN
     KPATCH_NB = 9
     IF (PRESENT(HPATCH_NAME)) THEN
       IF ( OECOSG ) THEN
          HPATCH_NAME = 'FLOODED GRASSES'
       ELSE
         HPATCH_NAME = 'IRRIGATED PARKS GARDEN & PEAT BOGS'
       ENDIF
     ENDIF
   ENDIF
!
!differenciation between: - Bare soil and bare rock
!  NEW DISTINCTION        - Crops C3 and C4 (+irrigated if old version before ECOSG)
! (from KNPATCH==7)       - Grassland C3 and C4 (+park if old version before ECOSG)
ELSEIF (KNPATCH==10) THEN
   IF (KVEGTYPE==  NVT_NO   ) THEN
     KPATCH_NB = 1
     IF (PRESENT(HPATCH_NAME)) HPATCH_NAME = 'BARE SOIL'
   ELSEIF (KVEGTYPE==  NVT_ROCK ) THEN
     KPATCH_NB = 2
     IF (PRESENT(HPATCH_NAME)) HPATCH_NAME = 'BARE ROCK'
   ELSEIF (KVEGTYPE==  NVT_SNOW ) THEN
     KPATCH_NB = 3
     IF (PRESENT(HPATCH_NAME)) HPATCH_NAME = 'PERMANENT SNOW'
   ELSEIF (KVEGTYPE== NVT_BOBD .OR. KVEGTYPE== NVT_TEBD .OR. KVEGTYPE== NVT_TRBD .OR. &
           KVEGTYPE== NVT_TEBE .OR. KVEGTYPE== NVT_TRBE .OR. KVEGTYPE== NVT_FLTR ) THEN
     KPATCH_NB = 4
     IF (PRESENT(HPATCH_NAME)) HPATCH_NAME = 'BROADLEAF TREES'
   ELSEIF (KVEGTYPE== NVT_BONE .OR. KVEGTYPE== NVT_TENE .OR. KVEGTYPE== NVT_BOND ) THEN
     KPATCH_NB = 5
     IF (PRESENT(HPATCH_NAME)) HPATCH_NAME = 'NEEDLELEAF TREES'
   ELSEIF (KVEGTYPE== NVT_SHRB ) THEN
     KPATCH_NB = 6
     IF (PRESENT(HPATCH_NAME)) HPATCH_NAME = 'SHRUBS'
   ELSEIF (KVEGTYPE== NVT_C3   .OR. KVEGTYPE== NVT_C3W .OR. KVEGTYPE== NVT_C3S  ) THEN
     KPATCH_NB = 7
     IF (PRESENT(HPATCH_NAME)) HPATCH_NAME = 'C3 CROPS'
   ELSEIF (KVEGTYPE== NVT_C4   .OR. KVEGTYPE== NVT_IRR .OR. KVEGTYPE== NVT_PARK ) THEN
     KPATCH_NB = 8
     IF (PRESENT(HPATCH_NAME)) THEN
       IF ( OECOSG ) THEN
         HPATCH_NAME = 'C4 CROPS'
       ELSE
         HPATCH_NAME = 'C4 CROPS & PARKS (IRRIG. OR NOT)'
       ENDIF
     ENDIF
   ELSEIF (KVEGTYPE== NVT_BOGR .OR. KVEGTYPE== NVT_GRAS .OR. KVEGTYPE== NVT_FLGR ) THEN
     KPATCH_NB = 9
     IF (PRESENT(HPATCH_NAME)) HPATCH_NAME = 'C3 GRASSLANDS'
   ELSEIF (KVEGTYPE== NVT_TROG ) THEN
     KPATCH_NB = 10
     IF (PRESENT(HPATCH_NAME)) HPATCH_NAME = 'C4 GRASSLANDS'
   ENDIF
!
!differenciation between: - Bare soil and bare rock
!  OLD DISTINCTION        - Grassland (temperate+boreal) and trpical one
! (from KNPATCH==9)       - Broadleafs(+shrubs) and tropical broadleafs evergreen
ELSEIF (KNPATCH==12) THEN
   IF (KVEGTYPE==  NVT_NO ) THEN
     KPATCH_NB = 1
     IF (PRESENT(HPATCH_NAME)) HPATCH_NAME = 'BARE SOIL'
   ELSEIF (KVEGTYPE==  NVT_ROCK ) THEN
     KPATCH_NB = 2
     IF (PRESENT(HPATCH_NAME)) HPATCH_NAME = 'BARE ROCK'
   ELSEIF (KVEGTYPE==  NVT_SNOW ) THEN
     KPATCH_NB = 3
     IF (PRESENT(HPATCH_NAME)) HPATCH_NAME = 'PERMANENT SNOW'
   ELSEIF (KVEGTYPE== NVT_TEBD .OR. KVEGTYPE== NVT_TRBD .OR. KVEGTYPE== NVT_TEBE .OR. &
           KVEGTYPE== NVT_BOBD .OR. KVEGTYPE== NVT_SHRB) THEN
     KPATCH_NB = 4
     IF (PRESENT(HPATCH_NAME)) HPATCH_NAME = 'BROADLEAF TREES & SHRUBS (-TRBE)'
   ELSEIF (KVEGTYPE== NVT_BONE .OR. KVEGTYPE== NVT_TENE .OR. KVEGTYPE== NVT_BOND) THEN
     KPATCH_NB = 5
     IF (PRESENT(HPATCH_NAME)) HPATCH_NAME = 'NEEDLELEAF TREES'
   ELSEIF (KVEGTYPE== NVT_TRBE ) THEN
     KPATCH_NB = 6
     IF (PRESENT(HPATCH_NAME)) HPATCH_NAME = 'TROPICAL BROADLEAF EVERG. TREES'
   ELSEIF (KVEGTYPE== NVT_C3   .OR. KVEGTYPE== NVT_C3W  .OR. KVEGTYPE== NVT_C3S ) THEN
     KPATCH_NB = 7
     IF (PRESENT(HPATCH_NAME)) HPATCH_NAME = 'C3 CROPS'
   ELSEIF (KVEGTYPE== NVT_C4   ) THEN
     KPATCH_NB = 8
     IF (PRESENT(HPATCH_NAME)) HPATCH_NAME = 'C4 CROPS'
   ELSEIF (KVEGTYPE== NVT_IRR  .OR. KVEGTYPE== NVT_FLTR) THEN
     KPATCH_NB = 9
     IF (PRESENT(HPATCH_NAME)) THEN
       IF ( OECOSG ) THEN
         HPATCH_NAME = 'FLOODED TREES'
       ELSE
         HPATCH_NAME = 'C4 CROPS IRRIGATED'
       ENDIF
     ENDIF
   ELSEIF (KVEGTYPE== NVT_GRAS .OR. KVEGTYPE== NVT_BOGR) THEN
     KPATCH_NB = 10
     IF (PRESENT(HPATCH_NAME)) HPATCH_NAME = 'C3 GRASSLANDS'
   ELSEIF (KVEGTYPE== NVT_TROG ) THEN
     KPATCH_NB = 11
     IF (PRESENT(HPATCH_NAME)) HPATCH_NAME = 'C4 GRASSLANDS'
   ELSEIF (KVEGTYPE== NVT_PARK .OR. KVEGTYPE== NVT_FLGR) THEN
     KPATCH_NB = 12
     IF (PRESENT(HPATCH_NAME)) THEN
       IF ( OECOSG ) THEN
          HPATCH_NAME = 'FLOODED GRASSES'
       ELSE
         HPATCH_NAME = 'IRRIGATED PARKS GARDEN & PEAT BOGS'
       ENDIF
     ENDIF
   ENDIF
!
!differenciation between: - Broadleaf decidus and broadleaf evergreen forest 
!  NEW DISTINCTION        - Needleleaf decidus and needleleaf evergreen forest
! (from KNPATCH==7)       - Winter C3 crops and summer C3 crops (C3 crops go in summer C3 in old versions)
ELSEIF (KNPATCH==13) THEN
   IF (KVEGTYPE==  NVT_NO ) THEN
     KPATCH_NB = 1
     IF (PRESENT(HPATCH_NAME)) HPATCH_NAME = 'BARE SOIL'
   ELSEIF (KVEGTYPE== NVT_ROCK ) THEN
     KPATCH_NB = 2
     IF (PRESENT(HPATCH_NAME)) HPATCH_NAME = 'BARE ROCK'
   ELSEIF (KVEGTYPE== NVT_SNOW ) THEN
     KPATCH_NB = 3
     IF (PRESENT(HPATCH_NAME)) HPATCH_NAME = 'PERMANENT SNOW'
   ELSEIF (KVEGTYPE== NVT_BOBD .OR. KVEGTYPE== NVT_TEBD .OR. KVEGTYPE== NVT_TRBD .OR. &
           KVEGTYPE== NVT_FLTR ) THEN
     KPATCH_NB = 4
     IF (PRESENT(HPATCH_NAME)) HPATCH_NAME = 'BROADLEAF DECIDUOUS TREES'
   ELSEIF (KVEGTYPE== NVT_TEBE .OR. KVEGTYPE== NVT_TRBE ) THEN
     KPATCH_NB = 5
     IF (PRESENT(HPATCH_NAME)) HPATCH_NAME = 'BROADLEAF EVERGREEN TREES'
   ELSEIF (KVEGTYPE== NVT_BOND ) THEN
     KPATCH_NB = 6
     IF (PRESENT(HPATCH_NAME)) HPATCH_NAME = 'NEEDLELEAF BOREAL DECIDUOUS TREES'
   ELSEIF (KVEGTYPE== NVT_BONE .OR. KVEGTYPE== NVT_TENE ) THEN
     KPATCH_NB = 7
     IF (PRESENT(HPATCH_NAME)) HPATCH_NAME = 'NEEDLELEAF EVERGREEN TREES'
   ELSEIF (KVEGTYPE== NVT_SHRB ) THEN
     KPATCH_NB = 8
     IF (PRESENT(HPATCH_NAME)) HPATCH_NAME = 'SHRUBS'
   ELSEIF (KVEGTYPE== NVT_C3   .OR. KVEGTYPE== NVT_C3S  ) THEN
     KPATCH_NB = 9
     IF (PRESENT(HPATCH_NAME)) THEN
       IF ( OECOSG ) THEN
         HPATCH_NAME = 'SUMMER C3 CROPS'
       ELSE
         HPATCH_NAME = 'C3 CROPS'
       ENDIF
     ENDIF
   ELSEIF (KVEGTYPE== NVT_C3W .OR. KVEGTYPE== NVT_IRR .OR. KVEGTYPE== NVT_PARK )  THEN
     KPATCH_NB = 10
     IF (PRESENT(HPATCH_NAME)) THEN
       IF ( OECOSG ) THEN
         HPATCH_NAME = 'WINTER C3 CROPS'
       ELSE
         HPATCH_NAME = 'IRRIGATED C4 CROPS, PARKS & GARDEN'
       ENDIF
     ENDIF
   ELSEIF (KVEGTYPE== NVT_C4 ) THEN
     KPATCH_NB = 11
     IF (PRESENT(HPATCH_NAME)) HPATCH_NAME = 'C4 CROPS'
   ELSEIF (KVEGTYPE== NVT_BOGR .OR. KVEGTYPE== NVT_GRAS .OR. KVEGTYPE== NVT_FLGR ) THEN
     KPATCH_NB = 12
     IF (PRESENT(HPATCH_NAME)) HPATCH_NAME = 'C3 GRASSLANDS'
   ELSEIF (KVEGTYPE== NVT_TROG ) THEN
     KPATCH_NB = 13
     IF (PRESENT(HPATCH_NAME)) HPATCH_NAME = 'C4 GRASSLANDS'
   ENDIF
!
! if KNPATCH=NVEGTYPE: no patch merging, each vegetation is computed
ELSEIF (KNPATCH==NVEGTYPE) THEN
  KPATCH_NB = KVEGTYPE
  IF (PRESENT(HPATCH_NAME)) THEN
    IF ( NVEGTYPE == 19 ) THEN
      YVEG(1) =  "NO  "      ! no vegetation (smooth)
      YVEG(2) =  "ROCK"      ! no vegetation (rocks)
      YVEG(3) =  "SNOW"      ! permanent snow and ice
      YVEG(4) =  "TEBD"      ! temperate broadleaf deciduous trees
      YVEG(5) =  "BONE"      ! boreal needleleaf evergreen trees 
      YVEG(6) =  "TRBE"      ! tropical broadleaf evergreen trees
      YVEG(7) =  "C3  "      ! C3 cultures types
      YVEG(8) =  "C4  "      ! C4 cultures types
      YVEG(9) =  "IRR "      ! irrigated crops
      YVEG(10)=  "GRAS"      ! temperate grassland C3
      YVEG(11)=  "TROG"      ! tropical  grassland C4
      YVEG(12)=  "PARK"      ! peat bogs, parks and gardens (irrigated grass)
      YVEG(13)=  "TRBD"      ! tropical  broadleaf  deciduous trees
      YVEG(14)=  "TEBE"      ! temperate broadleaf  evergreen trees
      YVEG(15)=  "TENE"      ! temperate needleleaf evergreen trees
      YVEG(16)=  "BOBD"      ! boreal    broadleaf  deciduous trees
      YVEG(17)=  "BOND"      ! boreal    needleleaf deciduous trees
      YVEG(18)=  "BOGR"      ! boreal grassland C3
      YVEG(19)=  "SHRB"      ! broadleaf shrub
    !
    ELSEIF ( NVEGTYPE == 20 ) THEN
      YVEG(1) =  "NO  "      ! no vegetation (smooth)
      YVEG(2) =  "ROCK"      ! no vegetation (rocks)
      YVEG(3) =  "SNOW"      ! permanent snow and ice
      YVEG(4) =  "BOBD"      ! boreal    broadleaf  deciduous trees
      YVEG(5) =  "TEBD"      ! temperate broadleaf deciduous trees
      YVEG(6) =  "TRBD"      ! tropical  broadleaf  deciduous trees
      YVEG(7) =  "TEBE"      ! temperate broadleaf  evergreen trees
      YVEG(8) =  "TRBE"      ! tropical broadleaf evergreen trees
      YVEG(9) =  "BONE"      ! boreal needleleaf evergreen trees 
      YVEG(10)=  "TENE"      ! temperate needleleaf evergreen trees
      YVEG(11)=  "BOND"      ! boreal    needleleaf deciduous trees
      YVEG(12)=  "SHRB"      ! broadleaf shrub
      YVEG(13)=  "BOGR"      ! boreal grassland C3
      YVEG(14)=  "GRAS"      ! temperate grassland C3
      YVEG(15)=  "TROG"      ! tropical  grassland C4
      YVEG(16)=  "C3W "      ! winter C3 cultures types
      YVEG(17)=  "C3S "      ! summer C3 cultures types
      YVEG(18)=  "C4  "      ! C4 cultures types
      YVEG(19)=  "FLTR"      ! flooded trees
      YVEG(20)=  "FLGR"      ! flooded grassland
    ELSE
      CALL ABOR1_SFX('VEGTYPE_TO_PATCH: NO NAME CORRESPONDANCE FOR NVEGTYPE/=20 or 19. PLEASE UPDATE. NVEGTYPE = '//YVEGTYPE)
    ENDIF
    !
    HPATCH_NAME = 'NVT_'//YVEG(KVEGTYPE)//' (PATCHS = VEGTYPES)'
    !
  ENDIF
!
ELSE
!
  CALL ABOR1_SFX('VEGTYPE_TO_PATCH: NPATCH (OR NPATCH_TREE OR EQ.) MUST BE 1, 2, 3, 5, 7, 9, 10, 12, 13 OR EQUAL TO NVEGTYPE =' &
                 //YVEGTYPE)
!
ENDIF
!
IF (LHOOK) CALL DR_HOOK('VEGTYPE_TO_PATCH',1,ZHOOK_HANDLE)
!
END SUBROUTINE VEGTYPE_TO_PATCH
