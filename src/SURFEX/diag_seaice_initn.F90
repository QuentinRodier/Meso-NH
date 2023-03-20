!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE DIAG_SEAICE_INIT_n (DGO, DI, DIC, DGMSI, OREAD_BUDGETC, S, &
                                      HPROGRAM,KLU,KSW)
!     #####################
!
!!****  *DIAG_SEAFLUX_INIT_n* - routine to initialize SEAFLUX diagnostic variables
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
!!      V. Masson   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2004 
!!      Modified    01/2006 : sea flux parameterization.
!!      Modified    08/2009 : cumulative sea flux 
!!      B. decharme 04/2013 : Add EVAP and SUBL diag
!!      S.Senesi    01/2014 : introduce fractional seaice 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODE_DIAG
!
USE MODD_DIAG_n,             ONLY : DIAG_t, DIAG_OPTIONS_t
USE MODD_DIAG_MISC_SEAICE_n, ONLY : DIAG_MISC_SEAICE_t
USE MODD_SEAFLUX_n,          ONLY : SEAFLUX_t
!
#ifdef SFX_OL
USE MODN_IO_OFFLINE,     ONLY : LRESTART
#endif
USE MODD_SURF_PAR,       ONLY : XUNDEF, LEN_HREC
USE MODD_SFX_OASIS,      ONLY : LCPL_SEA,LCPL_SEAICE
!
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
TYPE(DIAG_OPTIONS_t),     INTENT(INOUT) :: DGO
TYPE(DIAG_t),             INTENT(INOUT) :: DI
TYPE(DIAG_t),             INTENT(INOUT) :: DIC
TYPE(DIAG_MISC_SEAICE_t), INTENT(INOUT) :: DGMSI
TYPE(SEAFLUX_t),          INTENT(INOUT) :: S
!
LOGICAL,          INTENT(IN) :: OREAD_BUDGETC
INTEGER,          INTENT(IN) :: KLU   ! size of arrays
INTEGER,          INTENT(IN) :: KSW   ! number of SW spectral bands
CHARACTER(LEN=6), INTENT(IN) :: HPROGRAM  ! program calling
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER           :: IVERSION
INTEGER           :: IRESP          ! IRESP  : return-code if a problem appears
LOGICAL           :: LWORK
!
CHARACTER(LEN=LEN_HREC) :: YREC           ! Name of the article to be read
!
REAL(KIND=JPRB)   :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!* surface energy budget
!
IF (LHOOK) CALL DR_HOOK('DIAG_SEAICE_INIT_N',0,ZHOOK_HANDLE)
!
CALL ALLOC_BUD(DGO,DI,KLU,KSW)
!
IF (DGO%LSURF_BUDGET.OR.DGO%LSURF_BUDGETC) THEN
  !
  ALLOCATE(DI%XEVAP(KLU))
  ALLOCATE(DI%XSUBL(KLU))
  !
  DI%XEVAP    = XUNDEF
  DI%XSUBL    = XUNDEF  
  !
ELSE
  !
  ALLOCATE(DI%XEVAP(0))
  ALLOCATE(DI%XSUBL(0))  
  ! 
ENDIF
!
!* cumulative surface energy budget
!
#ifdef SFX_OL
LWORK = (DGO%LSURF_BUDGETC .OR. (LRESTART .AND. .NOT.DGO%LRESET_BUDGETC))
#else
LWORK = (DGO%LSURF_BUDGETC .OR. .NOT.DGO%LRESET_BUDGETC)
#endif
!
IF(LWORK.AND.(S%LHANDLE_SIC.OR.LCPL_SEAICE))THEN
  !
  CALL ALLOC_SURF_BUD(DIC,KLU,KLU,KSW)
  ALLOCATE(DIC%XEVAP(KLU))
  ALLOCATE(DIC%XSUBL(KLU))
  !
  CALL INIT_SURF_BUD(DIC,0.)
  DIC%XEVAP = 0.0
  DIC%XSUBL = 0.0
  !
ELSE
  !
  CALL ALLOC_SURF_BUD(DIC,0,0,0)
  ALLOCATE(DIC%XEVAP(0))
  ALLOCATE(DIC%XSUBL(0))  
  !
ENDIF
!
!* Seaice model diagnostics init 
!
IF (DGMSI%LDIAG_MISC_SEAICE) THEN
  ALLOCATE(DGMSI%XSIT(KLU))
  ALLOCATE(DGMSI%XSND(KLU)) 
  ALLOCATE(DGMSI%XMLT(KLU))  
  DGMSI%XSIT=XUNDEF
  DGMSI%XSND=XUNDEF
  DGMSI%XMLT=XUNDEF
ELSE
  ALLOCATE(DGMSI%XSIT  (0))
  ALLOCATE(DGMSI%XSND  (0))
  ALLOCATE(DGMSI%XMLT  (0))
ENDIF
!
IF(S%LHANDLE_SIC.AND.S%CSEAICE_SCHEME /= 'NONE  ')THEN
!        
  ALLOCATE(S%XGLT_SEA_SNET(KLU))
  ALLOCATE(S%XGLT_SEA_HEAT(KLU))
  ALLOCATE(S%XGLT_SEA_EVAP(KLU))
  ALLOCATE(S%XGLT_SEA_RAIN(KLU))
  ALLOCATE(S%XGLT_SEA_SNOW(KLU))
  ALLOCATE(S%XGLT_SEAICE_SNET(KLU))
  ALLOCATE(S%XGLT_SEAICE_HEAT(KLU))
  ALLOCATE(S%XGLT_SEAICE_EVAP(KLU))
  S%XGLT_SEA_SNET(:) = 0.0
  S%XGLT_SEA_HEAT(:) = 0.0
  S%XGLT_SEA_EVAP(:) = 0.0
  S%XGLT_SEA_RAIN(:) = 0.0
  S%XGLT_SEA_SNOW(:) = 0.0
  S%XGLT_SEAICE_SNET(:) = 0.0
  S%XGLT_SEAICE_HEAT(:) = 0.0
  S%XGLT_SEAICE_EVAP(:) = 0.0
!
ELSE
  ALLOCATE(S%XGLT_SEA_SNET(0))
  ALLOCATE(S%XGLT_SEA_HEAT(0))
  ALLOCATE(S%XGLT_SEA_EVAP(0))
  ALLOCATE(S%XGLT_SEA_RAIN(0))
  ALLOCATE(S%XGLT_SEA_SNOW(0))
  ALLOCATE(S%XGLT_SEAICE_SNET(0))
  ALLOCATE(S%XGLT_SEAICE_HEAT(0))
  ALLOCATE(S%XGLT_SEAICE_EVAP(0))      
ENDIF
!
!* Earth system model coupling variables
!
IF(LCPL_SEAICE)THEN
  ALLOCATE(S%XCPL_SEAICE_SNET(KLU))
  ALLOCATE(S%XCPL_SEAICE_HEAT(KLU))
  ALLOCATE(S%XCPL_SEAICE_EVAP(KLU))
  S%XCPL_SEAICE_SNET(:) = 0.0
  S%XCPL_SEAICE_HEAT(:) = 0.0
  S%XCPL_SEAICE_EVAP(:) = 0.0
ELSE
  ALLOCATE(S%XCPL_SEAICE_SNET(0))
  ALLOCATE(S%XCPL_SEAICE_HEAT(0))
  ALLOCATE(S%XCPL_SEAICE_EVAP(0))
ENDIF
!
IF (LHOOK) CALL DR_HOOK('DIAG_SEAICE_INIT_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE DIAG_SEAICE_INIT_n
