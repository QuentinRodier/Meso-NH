!     #########
      SUBROUTINE INIT_FROM_DATA_GREENROOF_n(KDECADE, HPHOTO,                              &
                                            POM_GR, PSAND_GR, PCLAY_GR, PVEG,             &
                                            PLAI,PRSMIN,PGAMMA,PWRMAX_CF,                 &
                                            PRGL,PCV,PDG,PD_ICE,PZ0,PZ0_O_Z0H,            &
                                            PALBNIR_VEG,PALBVIS_VEG,PALBUV_VEG,           &
                                            PEMIS,                                        &
                                            PVEGTYPE,PROOTFRAC,                           &
                                            PGMES,PBSLAI,PLAIMIN,PSEFOLD,PGC,             &
                                            PDMAX, PF2I, OSTRESS,                         &
                                            PH_TREE, PRE25,                               &
                                            PCE_NITRO, PCF_NITRO, PCNA_NITRO,             &
                                            PALBNIR_SOIL,PALBVIS_SOIL,PALBUV_SOIL         )  
!     ##############################################################
!
!!**** *CONVERT_COVER* convert surface cover classes into secondary 
!!                     physiographic variables for ISBA
!!
!!    PURPOSE
!!    -------
!!
!!    METHOD
!!    ------
!!   
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!    Based on init_from_data_grdnn
!!    
!!    AUTHOR
!!    ------
!!
!!    C. de Munck & A. Lemonsu        Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original   08/2011
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_TEB_GREENROOF_n,      ONLY :NTIME_GR
USE MODD_DATA_TEB_GREENROOF_n, ONLY :XPAR_OM_GR,                          &
                                     XPAR_SAND_GR, XPAR_CLAY_GR,          &
                                     XPAR_LAI, XPAR_H_TREE, XPAR_VEGTYPE, &
                                     XPAR_VEG, XPAR_Z0, XPAR_Z0_O_Z0H,    &
                                     XPAR_EMIS, XPAR_GAMMA, XPAR_CV,      &
                                     XPAR_RGL, XPAR_RSMIN, XPAR_DG,       &
                                     XPAR_ALBNIR_VEG, XPAR_ALBVIS_VEG,    &
                                     XPAR_ALBUV_VEG, XPAR_DICE,           &
                                     XPAR_ALBNIR_SOIL, XPAR_ALBVIS_SOIL,  &
                                     XPAR_ALBUV_SOIL,                     &
                                     XPAR_GMES, XPAR_BSLAI, XPAR_LAIMIN,  &
                                     XPAR_SEFOLD, XPAR_GC, XPAR_WRMAX_CF, &
                                     XPAR_ROOTFRAC, LDATA_STRESS,         &
                                     XPAR_DMAX, XPAR_F2I, XPAR_RE25,      &
                                     XPAR_CE_NITRO, XPAR_CF_NITRO,        &
                                     XPAR_CNA_NITRO  

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
INTEGER,                INTENT(IN)    :: KDECADE
 CHARACTER(LEN=*),       INTENT(IN)    :: HPHOTO  ! type of photosynthesis
!
REAL, DIMENSION(:,:),   OPTIONAL, INTENT(OUT)   :: PSAND_GR
REAL, DIMENSION(:,:),   OPTIONAL, INTENT(OUT)   :: PCLAY_GR
REAL, DIMENSION(:,:),   OPTIONAL, INTENT(OUT)   :: POM_GR
REAL, DIMENSION(:),     OPTIONAL, INTENT(OUT)   :: PVEG
REAL, DIMENSION(:),     OPTIONAL, INTENT(OUT)   :: PLAI
REAL, DIMENSION(:),     OPTIONAL, INTENT(OUT)   :: PRSMIN
REAL, DIMENSION(:),     OPTIONAL, INTENT(OUT)   :: PGAMMA
REAL, DIMENSION(:),     OPTIONAL, INTENT(OUT)   :: PWRMAX_CF
REAL, DIMENSION(:),     OPTIONAL, INTENT(OUT)   :: PRGL
REAL, DIMENSION(:),     OPTIONAL, INTENT(OUT)   :: PCV
REAL, DIMENSION(:,:),   OPTIONAL, INTENT(OUT)   :: PDG
REAL, DIMENSION(:),     OPTIONAL, INTENT(OUT)   :: PD_ICE
REAL, DIMENSION(:,:),   OPTIONAL, INTENT(OUT)   :: PROOTFRAC
REAL, DIMENSION(:),     OPTIONAL, INTENT(OUT)   :: PZ0
REAL, DIMENSION(:),     OPTIONAL, INTENT(OUT)   :: PZ0_O_Z0H
REAL, DIMENSION(:),     OPTIONAL, INTENT(OUT)   :: PALBNIR_VEG
REAL, DIMENSION(:),     OPTIONAL, INTENT(OUT)   :: PALBVIS_VEG
REAL, DIMENSION(:),     OPTIONAL, INTENT(OUT)   :: PALBUV_VEG
REAL, DIMENSION(:),     OPTIONAL, INTENT(OUT)   :: PEMIS
!
REAL, DIMENSION(:,:),   OPTIONAL, INTENT(OUT)   :: PVEGTYPE
!
REAL, DIMENSION(:),     OPTIONAL, INTENT(OUT)   :: PGMES
REAL, DIMENSION(:),     OPTIONAL, INTENT(OUT)   :: PRE25
REAL, DIMENSION(:),     OPTIONAL, INTENT(OUT)   :: PBSLAI
REAL, DIMENSION(:),     OPTIONAL, INTENT(OUT)   :: PLAIMIN
REAL, DIMENSION(:),     OPTIONAL, INTENT(OUT)   :: PSEFOLD
REAL, DIMENSION(:),     OPTIONAL, INTENT(OUT)   :: PGC
REAL, DIMENSION(:),     OPTIONAL, INTENT(OUT)   :: PDMAX
REAL, DIMENSION(:),     OPTIONAL, INTENT(OUT)   :: PF2I
LOGICAL, DIMENSION(:),  OPTIONAL, INTENT(OUT)   :: OSTRESS
!
REAL, DIMENSION(:),     OPTIONAL, INTENT(OUT)   :: PH_TREE
!
REAL, DIMENSION(:),     OPTIONAL, INTENT(OUT)   :: PCE_NITRO
REAL, DIMENSION(:),     OPTIONAL, INTENT(OUT)   :: PCF_NITRO
REAL, DIMENSION(:),     OPTIONAL, INTENT(OUT)   :: PCNA_NITRO
REAL, DIMENSION(:),     OPTIONAL, INTENT(OUT)   :: PALBNIR_SOIL
REAL, DIMENSION(:),     OPTIONAL, INTENT(OUT)   :: PALBVIS_SOIL
REAL, DIMENSION(:),     OPTIONAL, INTENT(OUT)   :: PALBUV_SOIL
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER :: ITIME
INTEGER :: ILUOUT
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!*    1.      TIME INITIALIZATION
!             -------------------
!
! data every month
IF (LHOOK) CALL DR_HOOK('INIT_FROM_DATA_GREENROOF_N',0,ZHOOK_HANDLE)
!ITIME = (KDECADE+2)/3      
ITIME = NTIME_GR
!
!*    2.      SECONDARY VARIABLES
!             -------------------
!
!*    2.0     fields for greenroofs
!             ---------------------
!
IF (PRESENT(POM_GR))   POM_GR(:,:)   = XPAR_OM_GR(:,:)
!
IF (PRESENT(PSAND_GR)) PSAND_GR(:,:) = XPAR_SAND_GR(:,:)
!
IF (PRESENT(PCLAY_GR)) PCLAY_GR(:,:) = XPAR_CLAY_GR(:,:)
!
!
!*    2.1     fields on natural surfaces only, taking into account patches/ 
!             -------------------------------
!
IF (PRESENT(PVEGTYPE)) PVEGTYPE = XPAR_VEGTYPE
!
! vegetation fraction
! -------------------
!
IF (PRESENT(PVEG)) PVEG(:) =  XPAR_VEG (:,ITIME)
!
! Leaf Area Index
! ---------------
!
IF (PRESENT(PLAI)) PLAI(:) = XPAR_LAI (:,ITIME)
!
! roughness length
! ----------------
!
IF (PRESENT(PZ0)) PZ0(:) =  XPAR_Z0 (:,ITIME)
!
IF (PRESENT(PZ0_O_Z0H)) PZ0_O_Z0H = XPAR_Z0_O_Z0H
!
!
!emis-eco
!--------
!
IF (PRESENT(PEMIS)) PEMIS(:) =  XPAR_EMIS (:,ITIME)
! 
!---------------------------------------------------------------------------------
! 
!* 1/Rsmin
!
IF (PRESENT(PRSMIN)) THEN
  IF (SIZE(PRSMIN)>0) PRSMIN = XPAR_RSMIN
END IF
!
!* other vegetation parameters
!
IF (PRESENT(PGAMMA)) PGAMMA = XPAR_GAMMA
IF (PRESENT(PWRMAX_CF)) PWRMAX_CF = XPAR_WRMAX_CF
!
!
IF (PRESENT(PRGL)) PRGL = XPAR_RGL
IF (PRESENT(PCV)) PCV = XPAR_CV
!
!---------------------------------------------------------------------------------
!
!* soil layers
!  -----------
!
IF (PRESENT(PDG)) PDG = XPAR_DG
!
!* cumulative root fraction
!
IF (PRESENT(PROOTFRAC)) PROOTFRAC = XPAR_ROOTFRAC
!
!* soil ice for runoff
!
IF (PRESENT(PD_ICE)) PD_ICE = XPAR_DICE
!
!---------------------------------------------------------------------------------
IF (PRESENT(PALBNIR_VEG))   PALBNIR_VEG = XPAR_ALBNIR_VEG
IF (PRESENT(PALBVIS_VEG))   PALBVIS_VEG = XPAR_ALBVIS_VEG
IF (PRESENT(PALBUV_VEG))    PALBUV_VEG  = XPAR_ALBUV_VEG

IF (PRESENT(PALBNIR_SOIL))  PALBNIR_SOIL(:) = XPAR_ALBNIR_SOIL
IF (PRESENT(PALBVIS_SOIL))  PALBVIS_SOIL(:) = XPAR_ALBVIS_SOIL
IF (PRESENT(PALBUV_SOIL))   PALBUV_SOIL (:) = XPAR_ALBUV_SOIL

IF (PRESENT(PGMES)) THEN
  IF (SIZE(PGMES)>0) PGMES = XPAR_GMES
END IF

IF (PRESENT(PBSLAI)) THEN
  IF (SIZE(PBSLAI)>0) PBSLAI = XPAR_BSLAI
END IF

IF (PRESENT(PSEFOLD)) THEN
  IF (SIZE(PSEFOLD)>0) PSEFOLD = XPAR_SEFOLD
END IF

IF (PRESENT(PGC)) THEN
  IF (SIZE(PGC)>0) PGC = XPAR_GC
END IF

IF (PRESENT(PDMAX)) THEN
  IF (SIZE(PDMAX)>0) PDMAX = XPAR_DMAX
END IF

IF (PRESENT(PRE25)) THEN
  IF (SIZE(PRE25)>0) PRE25 = XPAR_RE25
END IF

IF (PRESENT(PLAIMIN)) THEN
  IF (SIZE(PLAIMIN)>0) PLAIMIN = XPAR_LAIMIN
END IF

IF (PRESENT(PCE_NITRO)) THEN
  IF (SIZE(PCE_NITRO)>0) PCE_NITRO = XPAR_CE_NITRO
END IF

IF (PRESENT(PCF_NITRO)) THEN
  IF (SIZE(PCF_NITRO)>0) PCF_NITRO = XPAR_CF_NITRO
END IF

IF (PRESENT(PCNA_NITRO)) THEN
  IF (SIZE(PCNA_NITRO)>0) PCNA_NITRO = XPAR_CNA_NITRO
END IF

IF (PRESENT(PF2I)) THEN
  IF (SIZE(PF2I)>0) PF2I = XPAR_F2I
END IF
!
IF (PRESENT(OSTRESS)) THEN
  IF (SIZE(OSTRESS)>0) OSTRESS = LDATA_STRESS
END IF
IF (LHOOK) CALL DR_HOOK('INIT_FROM_DATA_GREENROOF_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE INIT_FROM_DATA_GREENROOF_n
