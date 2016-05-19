!#############################################################
SUBROUTINE INIT_TEB_VEG_OPTIONS_n(HPROGRAM)
!#############################################################
!
!!****  *INIT_TEB_TEB_VEG_n* - routine to initialize ISBA
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!	A. Lemonsu  *Meteo France*	
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    09/2009
!!      B. Decharme 07/2011 : read pgd+prep
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
USE MODD_READ_NAMELIST,   ONLY : LNAM_READ
!
USE MODD_TYPE_DATE_SURF
USE MODD_TYPE_SNOW
!
USE MODD_TEB_VEG_n,       ONLY: CROUGH,CISBA,CPEDOTF,LTR_ML,CPHOTO,CRUNOFF,CALBEDO,   &
                                CSCOND, CC1DRY, CSOILFRZ, CDIFSFCOND, CSNOWRES,       &
                                CRESPSL,NNBIOMASS, CSOC,                              & 
                                CCPSURF, CHORT, CKSAT, CTOPREG, XCGMAX, XCDRAG
!
USE MODD_TEB_GARDEN_n,    ONLY: NGROUND_LAYER, XSOILGRID

USE MODD_CH_TEB_n,        ONLY: LCH_BIO_FLUX  

USE MODD_DATA_COVER_PAR,  ONLY: NVEGTYPE
USE MODD_SURF_PAR,        ONLY: XUNDEF, NUNDEF
USE MODD_DIAG_MISC_TEB_n, ONLY: LSURF_DIAG_ALBEDO
!
USE MODD_ISBA_PAR,        ONLY : XOPTIMGRID
!
USE MODN_TEB_n,           ONLY : XTSTEP
!
USE MODI_DEFAULT_ISBA
USE MODI_DEFAULT_CH_DEP
USE MODI_DEFAULT_CH_BIO_FLUX
USE MODI_READ_DEFAULT_TEB_VEG_n
USE MODI_READ_TEB_VEG_CONF_n
USE MODI_GET_LUOUT
USE MODI_READ_SURF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
 CHARACTER(LEN=6),                   INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
!
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER           :: IVERSION, IBUGFIX  ! surface version
INTEGER           :: ILUOUT   ! unit of output listing file
INTEGER           :: IRESP    ! Error code after redding
 CHARACTER(LEN=12) :: YRECFM   ! Name of the article to be read
!
!
REAL                              :: ZOUT_TSTEP
 CHARACTER(LEN=3)                  :: YRAIN 
LOGICAL                           :: GCANOPY_DRAG
LOGICAL                           :: GGLACIER
LOGICAL                           :: GTRIP
LOGICAL                           :: GFLOOD
LOGICAL                           :: GVEGUPD
LOGICAL                           :: GSPINUPCARBS
LOGICAL                           :: GSPINUPCARBW
REAL                              :: ZSPINMAXS
REAL                              :: ZSPINMAXW
INTEGER                           :: INBYEARSPINS
INTEGER                           :: INBYEARSPINW
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!               Initialisation for IO
!
IF (LHOOK) CALL DR_HOOK('INIT_TEB_VEG_OPTIONS_N',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
!               Other little things
!
LSURF_DIAG_ALBEDO = .FALSE.
!
IF (LNAM_READ) THEN
 !
 !*       1.     Defaults
 !               --------
 !
 !        1.1. Hard defaults
 !      
 !       Definition of default options for ISBA (in MODD_TEB_VEG_n)
 !       REM - TSTEP, OUT_TSTEP, CANOPY_DRAG are defined as local variables
 !             because they are already in init_teb.f90 (these options are 
 !             forced to the same values for TEB and urban green areas)
 !
 CALL DEFAULT_ISBA(XTSTEP, ZOUT_TSTEP,                         &
                   CROUGH, CRUNOFF, CALBEDO, CSCOND,           &
                   CC1DRY, CSOILFRZ, CDIFSFCOND, CSNOWRES,     &
                   CCPSURF, XCGMAX, XCDRAG, CKSAT, CSOC,       &
                   CTOPREG, YRAIN, CHORT, GFLOOD, GTRIP,       &
                   GGLACIER, GCANOPY_DRAG, GVEGUPD,            &
                   GSPINUPCARBS, GSPINUPCARBW,                 & 
                   ZSPINMAXS, ZSPINMAXW,                       &
                   INBYEARSPINS, INBYEARSPINW                  )
 !
 CALL DEFAULT_CH_BIO_FLUX(LCH_BIO_FLUX)
 !
ENDIF
!        1.2. Defaults from file header
!    
 CALL READ_DEFAULT_TEB_VEG_n(HPROGRAM)
!
 CALL READ_TEB_VEG_CONF_n(HPROGRAM)
!
!-------------------------------------------------------------------------------
CRESPSL = 'DEF'
!-------------------------------------------------------------------------------
!
!         Initialisation for IO
!
!*       2.     Definition of version
!               ---------------------
!
YRECFM='VERSION'
 CALL READ_SURF(HPROGRAM,YRECFM,IVERSION,IRESP)
!
YRECFM='BUG'
 CALL READ_SURF(HPROGRAM,YRECFM,IBUGFIX,IRESP)
!
!*       2.     Initialisation of ISBA options
!               ------------------------------
!
!
YRECFM='TWN_ISBA'
IF (IVERSION>7 .OR. IVERSION==7 .AND. IBUGFIX>=3) YRECFM='GD_ISBA'
 CALL READ_SURF(HPROGRAM,YRECFM,CISBA,IRESP)
!
IF (IVERSION>=7) THEN
  !
  YRECFM='TWN_PEDOTF'
  IF (IVERSION>7 .OR. IVERSION==7 .AND. IBUGFIX>=3) YRECFM='GD_PEDOTF'
  CALL READ_SURF(HPROGRAM,YRECFM,CPEDOTF,IRESP)
  !
ELSE
  CPEDOTF = 'CH78'
ENDIF
!
YRECFM='TWN_PHOTO'
IF (IVERSION>7 .OR. IVERSION==7 .AND. IBUGFIX>=3) YRECFM='GD_PHOTO'
 CALL READ_SURF(HPROGRAM,YRECFM,CPHOTO,IRESP)
!
YRECFM='TWN_LAYER'
IF (IVERSION>7 .OR. IVERSION==7 .AND. IBUGFIX>=3) YRECFM='GD_LAYER'
 CALL READ_SURF(HPROGRAM,YRECFM,NGROUND_LAYER,IRESP)
!
!* new radiative transfert
!
IF (IVERSION>7 .OR. IVERSION==7 .AND. IBUGFIX>=2) THEN
  !
  YRECFM='TWN_TR_ML'
  IF (IVERSION>7 .OR. IVERSION==7 .AND. IBUGFIX>=3) YRECFM='GD_TR_ML'
  CALL READ_SURF(HPROGRAM,YRECFM,LTR_ML,IRESP)
  !
ELSE 
  LTR_ML = .FALSE.
ENDIF
!
!* Reference grid for DIF
!
IF(CISBA=='DIF') THEN
  ALLOCATE(XSOILGRID(NGROUND_LAYER))
  XSOILGRID=XUNDEF
  IF (IVERSION>7 .OR. IVERSION==7 .AND. IBUGFIX>=2) THEN
    YRECFM='TWN_SOILGRID'
    IF (IVERSION>7 .OR. IVERSION==7 .AND. IBUGFIX>=3) YRECFM='GD_SOILGRID'
    CALL READ_SURF(HPROGRAM,YRECFM,XSOILGRID,IRESP,HDIR='-')
  ELSE
    XSOILGRID(1:NGROUND_LAYER)=XOPTIMGRID(1:NGROUND_LAYER)
  ENDIF
ELSE
  ALLOCATE(XSOILGRID(0))
ENDIF
!
!* number of biomass pools
!
NNBIOMASS=1
IF (CPHOTO=='NIT') NNBIOMASS=3
!
!-------------------------------------------------------------------------------
!
!
IF (LHOOK) CALL DR_HOOK('INIT_TEB_VEG_OPTIONS_N',1,ZHOOK_HANDLE)
END SUBROUTINE INIT_TEB_VEG_OPTIONS_n
