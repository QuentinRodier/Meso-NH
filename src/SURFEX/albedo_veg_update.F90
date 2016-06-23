!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
    SUBROUTINE ALBEDO_VEG_UPDATE (DTCO, DTI, IG, I, &
                                  PTSTEP,TTIME,PCOVER,OCOVER,          &
                       HISBA,OECOCLIMAP,HPHOTO,OAGRIP,OTR_ML,HSFTYPE, &
                       PVEG,PALBNIR,PALBVIS,PALBUV,                   &
                       HALBEDO, PALBNIR_VEG, PALBVIS_VEG, PALBUV_VEG, &
                       PALBNIR_SOIL, PALBVIS_SOIL, PALBUV_SOIL        )  
!   ###############################################################
!!****  *ALBEDO_VEG_UPDATE*
!!
!!    PURPOSE
!!    -------
!
!     performs the time evolution of albedo for vegetation and soil
!     at UTC midnight, with effective change each ten days
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
!!
!!      B. Decharme          * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    02/02/13 
!!
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
!
!
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_DATA_ISBA_n, ONLY : DATA_ISBA_t
USE MODD_ISBA_GRID_n, ONLY : ISBA_GRID_t
USE MODD_ISBA_n, ONLY : ISBA_t
!
USE MODD_TYPE_DATE_SURF
!
USE MODI_INIT_ISBA_MIXPAR
USE MODI_CONVERT_PATCH_ISBA
USE MODI_ALBEDO
USE MODI_UPDATE_DATA_COVER
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(DATA_ISBA_t), INTENT(INOUT) :: DTI
TYPE(ISBA_GRID_t), INTENT(INOUT) :: IG
TYPE(ISBA_t), INTENT(INOUT) :: I
!
REAL,                 INTENT(IN)    :: PTSTEP  ! time step
TYPE(DATE_TIME),      INTENT(IN)    :: TTIME   ! UTC time
REAL,   DIMENSION(:,:), INTENT(IN)  :: PCOVER  ! cover types
LOGICAL, DIMENSION(:), INTENT(IN)   :: OCOVER
 CHARACTER(LEN=*),     INTENT(IN)    :: HISBA   ! type of soil (Force-Restore OR Diffusion)
 CHARACTER(LEN=*),     INTENT(IN)    :: HPHOTO  ! type of photosynthesis
LOGICAL,              INTENT(IN)    :: OAGRIP
LOGICAL,              INTENT(IN)    :: OTR_ML
 CHARACTER(LEN=*),     INTENT(IN)    :: HSFTYPE ! nature / garden
LOGICAL,              INTENT(IN)    :: OECOCLIMAP ! T if ecoclimap is used
!
REAL,   DIMENSION(:,:), INTENT(INOUT) :: PVEG    ! vegetation fraction
REAL,   DIMENSION(:,:), INTENT(INOUT) :: PALBNIR ! snow-free near-infra-red albedo
REAL,   DIMENSION(:,:), INTENT(INOUT) :: PALBVIS ! snow-free visible albedo
REAL,   DIMENSION(:,:), INTENT(INOUT) :: PALBUV  ! snow-free UV albedo
!
 CHARACTER(LEN=4),     INTENT(IN)    :: HALBEDO ! albedo type
!                                              ! 'CM13' 
!                                              ! 'DRY ' 
!                                              ! 'EVOL' 
!                                              ! 'WET ' 
!                                              ! 'USER'
REAL,   DIMENSION(:,:), INTENT(INOUT) :: PALBVIS_VEG ! visible, near infra-red and UV
REAL,   DIMENSION(:,:), INTENT(INOUT) :: PALBNIR_VEG ! albedo of the vegetation
REAL,   DIMENSION(:,:), INTENT(INOUT) :: PALBUV_VEG  !
REAL,   DIMENSION(:,:), INTENT(INOUT) :: PALBVIS_SOIL! visible, near infra-red and UV
REAL,   DIMENSION(:,:), INTENT(INOUT) :: PALBNIR_SOIL! soil albedo
REAL,   DIMENSION(:,:), INTENT(INOUT) :: PALBUV_SOIL !
!
!*      0.2    declarations of local variables
!
INTEGER                                  :: IDECADE, IDECADE2  ! decade of simulation
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-----------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('ALBEDO_VEG_UPDATE',0,ZHOOK_HANDLE)
!
IDECADE = 3 * ( TTIME%TDATE%MONTH - 1 ) + MIN(TTIME%TDATE%DAY-1,29) / 10 + 1
IDECADE2 = IDECADE
!
IF ( MOD(MIN(TTIME%TDATE%DAY,30),10)==1 .AND. TTIME%TIME - PTSTEP < 0.) THEN
    CALL UPDATE_DATA_COVER(DTCO, DTI, IG, I, &
                           TTIME%TDATE%YEAR)  
    CALL INIT_ISBA_MIXPAR(DTCO, DTI, IG, I, &
                          HISBA,IDECADE,IDECADE2,PCOVER,OCOVER,HPHOTO,HSFTYPE)
    CALL CONVERT_PATCH_ISBA(DTCO, DTI, I, &
                            HISBA,IDECADE,IDECADE2,PCOVER,OCOVER,&
                           HPHOTO,OAGRIP,.FALSE.,OTR_ML,HSFTYPE, &
                           PALBNIR_VEG=PALBNIR_VEG,              &
                           PALBVIS_VEG=PALBVIS_VEG,              &
                           PALBUV_VEG=PALBUV_VEG                 ) 
    IF ( HALBEDO=='CM13') THEN
       CALL CONVERT_PATCH_ISBA(DTCO, DTI, I, &
                            HISBA,IDECADE,IDECADE2,PCOVER,OCOVER,&
                              HPHOTO,OAGRIP,.FALSE.,OTR_ML,HSFTYPE, &
                              PALBNIR_SOIL=PALBNIR_SOIL, &
                              PALBVIS_SOIL=PALBVIS_SOIL, &
                              PALBUV_SOIL=PALBUV_SOIL )
    ENDIF                   
    CALL ALBEDO(HALBEDO,                                   &
                PALBVIS_VEG,PALBNIR_VEG,PALBUV_VEG,PVEG,   &
                PALBVIS_SOIL,PALBNIR_SOIL,PALBUV_SOIL,     &
                PALBVIS ,PALBNIR, PALBUV                   )  

END IF
!
IF (LHOOK) CALL DR_HOOK('ALBEDO_VEG_UPDATE',1,ZHOOK_HANDLE)
!
!-----------------------------------------------------------------
!
END SUBROUTINE ALBEDO_VEG_UPDATE
