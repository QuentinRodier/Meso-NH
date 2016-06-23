!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
    SUBROUTINE VEGETATION_UPDATE (DTCO, DTI, DTGD, DTGR, IG, I, TGRO, &
                                  PTSTEP,TTIME,PCOVER,OCOVER,          &
                       HISBA,OECOCLIMAP, HPHOTO, OAGRIP, OTR_ML,      &
                       HSFTYPE, PLAI,PVEG,PZ0,                        &
                       PALBNIR,PALBVIS,PALBUV,PEMIS,                  &
                       PRSMIN,PGAMMA,PWRMAX_CF,                       &
                       PRGL,PCV,                                      &
                       PGMES,PBSLAI,PLAIMIN,PSEFOLD,PGC,PDMAX,        &
                       PF2I,OSTRESS,                                  &
                       PAOSIP,PAOSIM,PAOSJP,PAOSJM,                   &
                       PHO2IP,PHO2IM,PHO2JP,PHO2JM,                   &
                       PZ0EFFIP,PZ0EFFIM,PZ0EFFJP,PZ0EFFJM,           &
                       HALBEDO, PALBNIR_VEG, PALBVIS_VEG, PALBUV_VEG, &
                       PALBNIR_SOIL, PALBVIS_SOIL, PALBUV_SOIL,       &
                       PCE_NITRO, PCF_NITRO, PCNA_NITRO,              &
                       TPSEED, TPREAP, PWATSUP, PIRRIG,               &
                       PGNDLITTER, PRGLGV,PGAMMAGV,                   &
                       PRSMINGV, PWRMAX_CFGV,                         &
                       PH_VEG, PLAIGV, PZ0LITTER,                     &
                       ODUPDATED, OABSENT                             )
!   ###############################################################
!!****  *VEGETATION EVOL*
!!
!!    PURPOSE
!!    -------
!
!     performs the time evolution of vegetation parameters
!       at UTC midnight for prescribed parameters, with effective change each ten days
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
!!      V. Masson          * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/03/03 
!!
!!      P Le Moigne 09/2005 AGS modifs of L. Jarlan
!!      P Samuelsson 10/2014 MEB
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
!
!
!
!
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_DATA_ISBA_n, ONLY : DATA_ISBA_t
USE MODD_DATA_TEB_GARDEN_n, ONLY : DATA_TEB_GARDEN_t
USE MODD_DATA_TEB_GREENROOF_n, ONLY : DATA_TEB_GREENROOF_t
USE MODD_ISBA_GRID_n, ONLY : ISBA_GRID_t
USE MODD_ISBA_n, ONLY : ISBA_t
USE MODD_TEB_GREENROOF_OPTION_n, ONLY : TEB_GREENROOF_OPTIONS_t
!
USE MODD_TYPE_DATE_SURF
!
USE MODI_INIT_ISBA_MIXPAR
USE MODI_CONVERT_PATCH_ISBA
USE MODI_INIT_FROM_DATA_GRDN_n
USE MODI_INIT_FROM_DATA_GREENROOF_n
USE MODI_SUBSCALE_Z0EFF
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
TYPE(DATA_TEB_GARDEN_t), INTENT(INOUT) :: DTGD
TYPE(DATA_TEB_GREENROOF_t), INTENT(INOUT) :: DTGR
TYPE(ISBA_GRID_t), INTENT(INOUT) :: IG
TYPE(ISBA_t), INTENT(INOUT) :: I
TYPE(TEB_GREENROOF_OPTIONS_t), INTENT(INOUT) :: TGRO
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
REAL,   DIMENSION(:,:), INTENT(INOUT) :: PLAI    ! leaf area index (LAI) 
REAL,   DIMENSION(:,:), INTENT(INOUT) :: PVEG    ! vegetation fraction
REAL,   DIMENSION(:,:), INTENT(INOUT) :: PZ0     ! roughness length: momentum
REAL,   DIMENSION(:,:), INTENT(INOUT) :: PALBNIR ! snow-free near-infra-red albedo
REAL,   DIMENSION(:,:), INTENT(INOUT) :: PALBVIS ! snow-free visible albedo
REAL,   DIMENSION(:,:), INTENT(INOUT) :: PALBUV  ! snow-free UV albedo
REAL,   DIMENSION(:,:), INTENT(INOUT) :: PEMIS   ! snow-free emissivity
!
REAL,   DIMENSION(:,:), INTENT(INOUT) :: PRSMIN  ! minimal stomatal resistance
REAL,   DIMENSION(:,:), INTENT(INOUT) :: PGAMMA  ! 
REAL,   DIMENSION(:,:), INTENT(INOUT) :: PWRMAX_CF ! 
REAL,   DIMENSION(:,:), INTENT(INOUT) :: PRGL
REAL,   DIMENSION(:,:), INTENT(INOUT) :: PCV
REAL,   DIMENSION(:,:), INTENT(INOUT) :: PGMES
REAL,   DIMENSION(:,:), INTENT(INOUT) :: PCE_NITRO
REAL,   DIMENSION(:,:), INTENT(INOUT) :: PCF_NITRO
REAL,   DIMENSION(:,:), INTENT(INOUT) :: PCNA_NITRO
REAL,   DIMENSION(:,:), INTENT(INOUT) :: PBSLAI
REAL,   DIMENSION(:,:), INTENT(INOUT) :: PLAIMIN
REAL,   DIMENSION(:,:), INTENT(INOUT) :: PSEFOLD
REAL,   DIMENSION(:,:), INTENT(INOUT) :: PGC
REAL,   DIMENSION(:,:), INTENT(INOUT) :: PF2I
REAL,   DIMENSION(:,:), INTENT(INOUT) :: PDMAX
LOGICAL,DIMENSION(:,:), INTENT(INOUT) :: OSTRESS
!
! MEB stuff
REAL,   DIMENSION(:,:), INTENT(INOUT) :: PGNDLITTER
REAL,   DIMENSION(:,:), INTENT(INOUT) :: PRGLGV
REAL,   DIMENSION(:,:), INTENT(INOUT) :: PGAMMAGV
REAL,   DIMENSION(:,:), INTENT(INOUT) :: PRSMINGV
REAL,   DIMENSION(:,:), INTENT(INOUT) :: PWRMAX_CFGV
REAL,   DIMENSION(:,:), INTENT(INOUT) :: PH_VEG
REAL,   DIMENSION(:,:), INTENT(INOUT) :: PLAIGV
REAL,   DIMENSION(:,:), INTENT(INOUT) :: PZ0LITTER
!
 CHARACTER(LEN=4),     INTENT(IN)    :: HALBEDO ! albedo type
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

REAL, DIMENSION(:), INTENT(IN)  :: PAOSIP  ! A/S for increasing x
REAL, DIMENSION(:), INTENT(IN)  :: PAOSIM  ! A/S for decreasing x
REAL, DIMENSION(:), INTENT(IN)  :: PAOSJP  ! A/S for increasing y
REAL, DIMENSION(:), INTENT(IN)  :: PAOSJM  ! A/S for decreasing y
REAL, DIMENSION(:), INTENT(IN)  :: PHO2IP  ! h/2 for increasing x
REAL, DIMENSION(:), INTENT(IN)  :: PHO2IM  ! h/2 for decreasing x
REAL, DIMENSION(:), INTENT(IN)  :: PHO2JP  ! h/2 for increasing y
REAL, DIMENSION(:), INTENT(IN)  :: PHO2JM  ! h/2 for decreasing y
!
REAL, DIMENSION(:,:), INTENT(INOUT) :: PZ0EFFIP! roughness length for increasing x
REAL, DIMENSION(:,:), INTENT(INOUT) :: PZ0EFFIM! roughness length for decreasing x
REAL, DIMENSION(:,:), INTENT(INOUT) :: PZ0EFFJP! roughness length for increasing y
REAL, DIMENSION(:,:), INTENT(INOUT) :: PZ0EFFJM! roughness length for decreasing y
!
TYPE(DATE_TIME), DIMENSION(:,:), INTENT(INOUT) :: TPSEED   ! seeding date
TYPE(DATE_TIME), DIMENSION(:,:), INTENT(INOUT) :: TPREAP   ! seeding date
!
REAL, DIMENSION(:,:), INTENT(INOUT) :: PWATSUP  ! water supply during irrigation
REAL, DIMENSION(:,:), INTENT(INOUT) :: PIRRIG   ! irrigated fraction
!
LOGICAL,              INTENT(OUT)   :: ODUPDATED  ! T if parameters are being reset
LOGICAL,DIMENSION(:), INTENT(IN), OPTIONAL :: OABSENT ! T where field is not defined
!
!*      0.2    declarations of local variables
!
INTEGER                                  :: IDECADE, IDECADE2  ! decade of simulation
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-----------------------------------------------------------------
!
!*      2.     Non-interactive vegetation
!              --------------------------
!
!*      2.1    Decade
!              ------
!
IF (LHOOK) CALL DR_HOOK('VEGETATION_UPDATE',0,ZHOOK_HANDLE)
IDECADE = 3 * ( TTIME%TDATE%MONTH - 1 ) + MIN(TTIME%TDATE%DAY-1,29) / 10 + 1
IDECADE2 = IDECADE
ODUPDATED=.FALSE.
!
!*      2.2    From ecoclimap
!              --------------
!
!* new decade?
  IF ( MOD(MIN(TTIME%TDATE%DAY,30),10)==1 .AND. TTIME%TIME - PTSTEP < 0.) THEN
    ODUPDATED=.TRUE.
!* time varying parameters
    IF (OECOCLIMAP .OR. HSFTYPE=='NAT') THEN
!* new year ? --> recomputes data LAI and derivated parameters (usefull in case of ecoclimap2)
      CALL UPDATE_DATA_COVER(DTCO, DTI, IG, I, &
                             TTIME%TDATE%YEAR)  
      IF (HSFTYPE=='NAT') CALL INIT_ISBA_MIXPAR(DTCO, DTI, IG, I, &
                                                HISBA,IDECADE,IDECADE2,PCOVER,OCOVER,HPHOTO,HSFTYPE)
      CALL CONVERT_PATCH_ISBA(DTCO, DTI, I, &
                              HISBA,IDECADE,IDECADE2,PCOVER,OCOVER,&
                           HPHOTO,OAGRIP,.FALSE.,OTR_ML,HSFTYPE, &
                           PVEG=PVEG,PLAI=PLAI,PRSMIN=PRSMIN,    &
                           PGAMMA=PGAMMA, PWRMAX_CF=PWRMAX_CF,   &
                           PRGL=PRGL,PCV=PCV,PZ0=PZ0,            &
                           PALBNIR_VEG=PALBNIR_VEG,              &
                           PALBVIS_VEG=PALBVIS_VEG,              &
                           PALBUV_VEG=PALBUV_VEG,                &
                           PEMIS_ECO=PEMIS,PGMES=PGMES,          &
                           PBSLAI=PBSLAI,                        &
                           PLAIMIN=PLAIMIN,PSEFOLD=PSEFOLD,      &
                           PGC=PGC,PF2I=PF2I,                    &
                           OSTRESS=OSTRESS,PCE_NITRO=PCE_NITRO,  &
                           PCF_NITRO=PCF_NITRO,                  &
                           PCNA_NITRO=PCNA_NITRO,                &
                           TPSEED=TPSEED, TPREAP=TPREAP,         &
                           PWATSUP=PWATSUP,PIRRIG=PIRRIG,        &
                           PGNDLITTER=PGNDLITTER,                &
                           PRGLGV=PRGLGV,PGAMMAGV=PGAMMAGV,      &
                           PRSMINGV=PRSMINGV,                    &
                           PWRMAX_CFGV=PWRMAX_CFGV,              &
                           PLAIGV=PLAIGV,PZ0LITTER=PZ0LITTER,    &
                           PH_VEG=PH_VEG                         )
      IF ( HALBEDO=='CM13') THEN
        CALL CONVERT_PATCH_ISBA(DTCO, DTI, I, &
                              HISBA,IDECADE,IDECADE2,PCOVER,OCOVER,&
                              HPHOTO,OAGRIP,.FALSE.,OTR_ML,HSFTYPE, &
                              PALBNIR_SOIL=PALBNIR_SOIL, &
                              PALBVIS_SOIL=PALBVIS_SOIL, &
                              PALBUV_SOIL=PALBUV_SOIL )
      ENDIF
    ELSEIF (HSFTYPE=='GRD') THEN
      CALL INIT_FROM_DATA_GRDN_n(DTGD, &
                                 IDECADE,HPHOTO,                                      &
                       PVEG=PVEG(:,1),PLAI=PLAI(:,1),PZ0=PZ0(:,1),PEMIS=PEMIS(:,1)    )  
     
    ELSEIF (HSFTYPE=='GNR') THEN
      CALL INIT_FROM_DATA_GREENROOF_n(DTGR, TGRO, &
                                      IDECADE,HPHOTO,                                 &
                       PVEG=PVEG(:,1),PLAI=PLAI(:,1),PZ0=PZ0(:,1),PEMIS=PEMIS(:,1)    )  

    ENDIF
!
!* default values to avoid problems in physical routines
!  for points where there is no vegetation or soil to be simulated by ISBA.
    IF (PRESENT(OABSENT)) THEN
        WHERE (OABSENT(:))
          PVEG       (:,1) = 0.
          PLAI       (:,1) = 0.
          PRSMIN     (:,1) = 40.
          PGAMMA     (:,1) = 0.
          PWRMAX_CF  (:,1) = 0.2
          PRGL       (:,1) = 100.
          PCV        (:,1) = 2.E-5
          PZ0        (:,1) = 0.013
          PALBNIR_VEG(:,1) = 0.30
          PALBVIS_VEG(:,1) = 0.30
          PALBUV_VEG (:,1) = 0.06
          PEMIS      (:,1) = 0.94                
        END WHERE
        IF (HPHOTO/='NON') THEN
          WHERE (OABSENT(:))
            PGMES      (:,1) = 0.020
            PBSLAI     (:,1) = 0.36
            PLAIMIN    (:,1) = 0.3
            PSEFOLD    (:,1) = 90*86400.
            PGC        (:,1) = 0.00025                  
          END WHERE
          IF (HPHOTO/='AGS' .AND. HPHOTO/='LAI') THEN
            WHERE (OABSENT(:)) PF2I       (:,1) = 0.3
            IF (HPHOTO=='NIT' .OR. HPHOTO=='NCB') THEN
              WHERE (OABSENT(:))
                PCE_NITRO  (:,1) = 7.68
                PCF_NITRO  (:,1) = -4.33
                PCNA_NITRO (:,1) = 1.3                      
              END WHERE
            ENDIF
          ENDIF
        ENDIF
    ENDIF

    IF (HSFTYPE=='NAT') THEN
!* albedo
      CALL ALBEDO(HALBEDO,                                 &
                  PALBVIS_VEG,PALBNIR_VEG,PALBUV_VEG,PVEG,   &
                  PALBVIS_SOIL,PALBNIR_SOIL,PALBUV_SOIL,     &
                  PALBVIS ,PALBNIR, PALBUV                   )  
!
!* effective roughness length
      CALL SUBSCALE_Z0EFF(PAOSIP,PAOSIM,PAOSJP,PAOSJM,       &
                          PHO2IP,PHO2IM,PHO2JP,PHO2JM,PZ0,     &
                          PZ0EFFIP,PZ0EFFIM,PZ0EFFJP,PZ0EFFJM  )  
    ENDIF

  END IF
IF (LHOOK) CALL DR_HOOK('VEGETATION_UPDATE',1,ZHOOK_HANDLE)
!
!*      2.3    Prescribed vegetation
!              ---------------------
!
!-----------------------------------------------------------------
!
END SUBROUTINE VEGETATION_UPDATE
