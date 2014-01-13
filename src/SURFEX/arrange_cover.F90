!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########################
      SUBROUTINE ARRANGE_COVER(PDATA_NATURE,PDATA_TOWN,PDATA_SEA,PDATA_WATER,PDATA_VEGTYPE, &
                               PDATA_GARDEN, OGARDEN, PDATA_BLD, PDATA_WALL_O_HOR           )
!     #########################
!
!!**** *ARRANGE_COVER*
!!
!!    PURPOSE
!!    -------
!!
!!    change water and intertidal (not lake) to nature and/or town to rock : arrange cover properly
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
!!    B. Decharme        Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    03/2009
!!                04/2013 (V. Masson) Fusion of Arrange_cover & update_data_frac
!!                        to allow distinct cover change options between submodels (_n)
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_SURF_PAR,       ONLY : XUNDEF
!
USE MODD_SURF_ATM_n,     ONLY : LWATER_TO_NATURE, LTOWN_TO_ROCK
!
USE MODD_DATA_COVER_n,   ONLY : XDATA_TOWN, XDATA_NATURE, XDATA_GARDEN,           &
                                XDATA_SEA, XDATA_WATER, XDATA_VEGTYPE, LGARDEN,   &
                                XDATA_BLD, XDATA_WALL_O_HOR
USE MODD_DATA_COVER,     ONLY : XDATA_ROOT_DEPTH, XDATA_GROUND_DEPTH, XDATA_DICE, &
                                XDATA_LAI, XDATA_LAI_ALL_YEARS,                   &
                                XDATA_ALB_VEG_NIR, XDATA_ALB_VEG_VIS,             &
                                XDATA_ALB_SOIL_NIR, XDATA_ALB_SOIL_VIS
!
USE MODD_DATA_COVER_PAR, ONLY : NVEGTYPE, JPCOVER, NROCK, NWATER, NVT_ROCK
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
REAL, DIMENSION(:), INTENT(IN)  :: PDATA_NATURE
REAL, DIMENSION(:), INTENT(IN)  :: PDATA_TOWN
REAL, DIMENSION(:), INTENT(IN)  :: PDATA_SEA
REAL, DIMENSION(:), INTENT(IN)  :: PDATA_WATER
REAL, DIMENSION(:), INTENT(IN)  :: PDATA_GARDEN
REAL, DIMENSION(:,:),INTENT(IN) :: PDATA_VEGTYPE
LOGICAL,            INTENT(IN)  :: OGARDEN
REAL, DIMENSION(:), INTENT(IN)  :: PDATA_BLD
REAL, DIMENSION(:), INTENT(IN)  :: PDATA_WALL_O_HOR
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
REAL     :: ZWORK
!
INTEGER  :: JCOVER, JVEGTYPE, JL
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('ARRANGE_COVER',0,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
! Allocate fields from submodel module
!-------------------------------------------------------------------------------
!
IF (.NOT.ASSOCIATED(XDATA_NATURE)) THEN
  ALLOCATE(XDATA_NATURE    (JPCOVER))
  ALLOCATE(XDATA_TOWN      (JPCOVER))
  ALLOCATE(XDATA_SEA       (JPCOVER))
  ALLOCATE(XDATA_WATER     (JPCOVER))
  ALLOCATE(XDATA_VEGTYPE   (JPCOVER,NVEGTYPE))
  ALLOCATE(XDATA_GARDEN    (JPCOVER))
  ALLOCATE(XDATA_BLD       (JPCOVER))
  ALLOCATE(XDATA_WALL_O_HOR(JPCOVER))
ENDIF
!
!-------------------------------------------------------------------------------
! Default values
!-------------------------------------------------------------------------------
!
LGARDEN = OGARDEN
!
XDATA_NATURE     = PDATA_NATURE
XDATA_TOWN       = PDATA_TOWN
XDATA_SEA        = PDATA_SEA
XDATA_WATER      = PDATA_WATER
XDATA_VEGTYPE    = PDATA_VEGTYPE
XDATA_GARDEN     = PDATA_GARDEN
XDATA_BLD        = PDATA_BLD
XDATA_WALL_O_HOR = PDATA_WALL_O_HOR
!
!-------------------------------------------------------------------------------
! Change water (not lake) to nature
!-------------------------------------------------------------------------------
!
IF(LWATER_TO_NATURE)THEN
  DO JCOVER=1,JPCOVER
     IF(JCOVER/=NWATER(1).AND.JCOVER/=NWATER(2).AND.JCOVER/=NWATER(3).AND.XDATA_WATER(JCOVER)>0.0)THEN
       XDATA_NATURE(JCOVER)=XDATA_NATURE(JCOVER)+XDATA_WATER(JCOVER)
       XDATA_WATER (JCOVER)=0.0
     ENDIF
     !Only cover 242
     IF(XDATA_SEA(JCOVER)>0.0.AND.XDATA_SEA(JCOVER)<1.0)THEN
           XDATA_NATURE(JCOVER)=XDATA_NATURE(JCOVER)+XDATA_SEA(JCOVER)
           XDATA_SEA(JCOVER)=0.0
     ENDIF
  ENDDO
ENDIF
!
!-------------------------------------------------------------------------------
! Change town to rock but keep other natural fraction
!-------------------------------------------------------------------------------
!
IF(LTOWN_TO_ROCK)THEN
!        
  DO JCOVER=1,JPCOVER
     IF(XDATA_TOWN(JCOVER)>0.0.OR.XDATA_GARDEN(JCOVER)>0.0)THEN
!
       XDATA_NATURE(JCOVER) = XDATA_NATURE(JCOVER) + XDATA_GARDEN(JCOVER) * XDATA_TOWN(JCOVER)
       XDATA_TOWN  (JCOVER) = XDATA_TOWN  (JCOVER) * ( 1. - XDATA_GARDEN(JCOVER))
       XDATA_GARDEN(JCOVER) = 0.0
!
       ZWORK=XDATA_NATURE(JCOVER)+XDATA_TOWN(JCOVER)
!       
       DO JVEGTYPE=1,NVEGTYPE
             XDATA_VEGTYPE(JCOVER,JVEGTYPE)=XDATA_VEGTYPE(JCOVER,JVEGTYPE)*XDATA_NATURE(JCOVER)/ZWORK
       ENDDO
!      
       XDATA_VEGTYPE(JCOVER,NVT_ROCK) = XDATA_VEGTYPE(JCOVER,NVT_ROCK)+XDATA_TOWN(JCOVER)/ZWORK
!
       XDATA_NATURE(JCOVER)=XDATA_NATURE(JCOVER)+XDATA_TOWN(JCOVER)
!       
       XDATA_TOWN  (JCOVER)=0.0
!
!      Initialise some variables
       XDATA_LAI          (JCOVER,:,NVT_ROCK) = 0.0
       XDATA_LAI_ALL_YEARS(JCOVER,:,NVT_ROCK) = 0.0
       XDATA_ROOT_DEPTH   (JCOVER,  NVT_ROCK) = 0.2
       XDATA_GROUND_DEPTH (JCOVER,  NVT_ROCK) = 0.2
       XDATA_DICE         (JCOVER,  NVT_ROCK) = 0.2
       XDATA_ALB_VEG_NIR  (JCOVER,:,NVT_ROCK) = 0.3
       XDATA_ALB_VEG_VIS  (JCOVER,:,NVT_ROCK) = 0.1
       XDATA_ALB_SOIL_NIR (JCOVER,:,NVT_ROCK) = 0.3
       XDATA_ALB_SOIL_VIS (JCOVER,:,NVT_ROCK) = 0.1
!
     ENDIF
  ENDDO
!
ELSE
!-------------------------------------------------------------------------------
! Town is kept, but if gardens are not treated specifically, 
! they are included into nature fraction.
!-------------------------------------------------------------------------------
!
  IF (.NOT. OGARDEN) THEN
    XDATA_NATURE     = XDATA_NATURE + XDATA_GARDEN * XDATA_TOWN
    XDATA_TOWN       = XDATA_TOWN   * ( 1. - XDATA_GARDEN)
    XDATA_GARDEN     = 0.
    XDATA_BLD        = XDATA_BLD / (1. - XDATA_GARDEN)
    XDATA_WALL_O_HOR = XDATA_WALL_O_HOR / (1. - XDATA_GARDEN)
  END IF
!
ENDIF
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('ARRANGE_COVER',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE ARRANGE_COVER
