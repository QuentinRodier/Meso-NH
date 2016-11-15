!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE DIAG_FLAKE_INIT_n (DGU, DGF, DGMF, F, &
                                    HPROGRAM,KLU,KSW)
!     #####################
!
!!****  *DIAG_FLAKE_INIT_n* - routine to initialize FLAKE diagnostic variables
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
!!       V.Masson   10/2013 Adds min and max 2m parameters
!!      B. Decharme  04/2013 new diag
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
!
!
!
USE MODD_DIAG_FLAKE_n, ONLY : DIAG_FLAKE_t
USE MODD_DIAG_MISC_FLAKE_n, ONLY : DIAG_MISC_FLAKE_t
USE MODD_DIAG_SURF_ATM_n, ONLY : DIAG_SURF_ATM_t
USE MODD_FLAKE_n, ONLY : FLAKE_t
!
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODD_SFX_OASIS,      ONLY : LCPL_LAKE
!
!
!
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
!
!
!
TYPE(DIAG_FLAKE_t), INTENT(INOUT) :: DGF
TYPE(DIAG_MISC_FLAKE_t), INTENT(INOUT) :: DGMF
TYPE(DIAG_SURF_ATM_t), INTENT(INOUT) :: DGU
TYPE(FLAKE_t), INTENT(INOUT) :: F
!
INTEGER, INTENT(IN) :: KLU   ! size of arrays
INTEGER, INTENT(IN) :: KSW   ! number of SW spectral bands
 CHARACTER(LEN=6), INTENT(IN):: HPROGRAM  ! program calling
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER           :: IVERSION
INTEGER           :: IRESP          ! IRESP  : return-code if a problem appears
 CHARACTER(LEN=LEN_HREC) :: YREC           ! Name of the article to be read
REAL(KIND=JPRB)   :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!* surface energy budget
!
IF (LHOOK) CALL DR_HOOK('DIAG_FLAKE_INIT_N',0,ZHOOK_HANDLE)
!
ALLOCATE(DGF%XDIAG_TS(KLU))
DGF%XDIAG_TS = XUNDEF
!
IF (DGF%LSURF_BUDGET.OR.DGF%LSURF_BUDGETC) THEN
  ALLOCATE(DGF%XRN     (KLU))
  ALLOCATE(DGF%XH      (KLU))
  ALLOCATE(DGF%XLE     (KLU))
  ALLOCATE(DGF%XLEI    (KLU))
  ALLOCATE(DGF%XGFLUX  (KLU))
  ALLOCATE(DGF%XEVAP   (KLU))
  ALLOCATE(DGF%XSUBL   (KLU))
  ALLOCATE(DGF%XSWD    (KLU))
  ALLOCATE(DGF%XSWU    (KLU))
  ALLOCATE(DGF%XLWD    (KLU))
  ALLOCATE(DGF%XLWU    (KLU))
  ALLOCATE(DGF%XSWBD   (KLU,KSW))
  ALLOCATE(DGF%XSWBU   (KLU,KSW))
  ALLOCATE(DGF%XFMU    (KLU))
  ALLOCATE(DGF%XFMV    (KLU))
  ALLOCATE(DGF%XALBT   (KLU))
  ALLOCATE(DGF%XSWE    (KLU))
  !
  DGF%XRN      = XUNDEF
  DGF%XH       = XUNDEF
  DGF%XLE      = XUNDEF
  DGF%XLEI     = XUNDEF
  DGF%XGFLUX   = XUNDEF
  DGF%XEVAP    = XUNDEF
  DGF%XSUBL    = XUNDEF  
  DGF%XSWD     = XUNDEF
  DGF%XSWU     = XUNDEF
  DGF%XLWD     = XUNDEF
  DGF%XLWU     = XUNDEF
  DGF%XSWBD    = XUNDEF
  DGF%XSWBU    = XUNDEF
  DGF%XFMU     = XUNDEF
  DGF%XFMV     = XUNDEF
  DGF%XALBT    = XUNDEF
  DGF%XSWE     = XUNDEF
ELSE
  ALLOCATE(DGF%XRN     (0))
  ALLOCATE(DGF%XH      (0))
  ALLOCATE(DGF%XLE     (0))
  ALLOCATE(DGF%XLEI    (0))
  ALLOCATE(DGF%XGFLUX  (0))
  ALLOCATE(DGF%XEVAP   (0))
  ALLOCATE(DGF%XSUBL   (0))  
  ALLOCATE(DGF%XSWD    (0))
  ALLOCATE(DGF%XSWU    (0))
  ALLOCATE(DGF%XLWD    (0))
  ALLOCATE(DGF%XLWU    (0))
  ALLOCATE(DGF%XSWBD   (0,0))
  ALLOCATE(DGF%XSWBU   (0,0))
  ALLOCATE(DGF%XFMU    (0))
  ALLOCATE(DGF%XFMV    (0))
  ALLOCATE(DGF%XALBT   (0))
  ALLOCATE(DGF%XSWE    (0))
END IF
!
!* cumulative surface energy budget
!
IF (DGF%LSURF_BUDGETC) THEN
!    
  ALLOCATE(DGF%XRNC    (KLU))
  ALLOCATE(DGF%XHC     (KLU))
  ALLOCATE(DGF%XLEC    (KLU))
  ALLOCATE(DGF%XLEIC   (KLU))
  ALLOCATE(DGF%XGFLUXC (KLU))
  ALLOCATE(DGF%XEVAPC  (KLU))
  ALLOCATE(DGF%XSUBLC  (KLU))  
  ALLOCATE(DGF%XSWDC   (KLU))
  ALLOCATE(DGF%XSWUC   (KLU))
  ALLOCATE(DGF%XLWDC   (KLU))
  ALLOCATE(DGF%XLWUC   (KLU))
  ALLOCATE(DGF%XFMUC   (KLU))
  ALLOCATE(DGF%XFMVC   (KLU))
!
  IF (.NOT. DGU%LREAD_BUDGETC) THEN        
     DGF%XRNC    = 0.0
     DGF%XHC     = 0.0
     DGF%XLEC    = 0.0
     DGF%XLEIC   = 0.0
     DGF%XGFLUXC = 0.0
     DGF%XEVAPC  = 0.0
     DGF%XSUBLC  = 0.0
     DGF%XSWDC   = 0.0
     DGF%XSWUC   = 0.0
     DGF%XLWDC   = 0.0
     DGF%XLWUC   = 0.0
     DGF%XFMUC   = 0.0
     DGF%XFMVC   = 0.0
  ELSEIF (DGU%LREAD_BUDGETC.AND.DGF%LRESET_BUDGETC) THEN
     DGF%XRNC    = 0.0
     DGF%XHC     = 0.0
     DGF%XLEC    = 0.0
     DGF%XLEIC   = 0.0
     DGF%XGFLUXC = 0.0
     DGF%XEVAPC  = 0.0
     DGF%XSUBLC  = 0.0     
     DGF%XSWDC   = 0.0
     DGF%XSWUC   = 0.0
     DGF%XLWDC   = 0.0
     DGF%XLWUC   = 0.0
     DGF%XFMUC   = 0.0
     DGF%XFMVC   = 0.0
  ELSE
     CALL READ_SURF(&
                    HPROGRAM,'VERSION',IVERSION,IRESP)
     IF (IVERSION<8)THEN
       DGF%XRNC    = 0.0
       DGF%XHC     = 0.0
       DGF%XLEC    = 0.0
       DGF%XLEIC   = 0.0
       DGF%XGFLUXC = 0.0
       DGF%XEVAPC  = 0.0
       DGF%XSUBLC  = 0.0     
       DGF%XSWDC   = 0.0
       DGF%XSWUC   = 0.0
       DGF%XLWDC   = 0.0
       DGF%XLWUC   = 0.0
       DGF%XFMUC   = 0.0
       DGF%XFMVC   = 0.0             
     ELSE
       YREC='RNC_WAT'
       CALL READ_SURF(&
                    HPROGRAM,YREC,DGF%XRNC,IRESP)
       YREC='HC_WAT'
       CALL READ_SURF(&
                    HPROGRAM,YREC,DGF%XHC ,IRESP)
       YREC='LEC_WAT'
       CALL READ_SURF(&
                    HPROGRAM,YREC,DGF%XLEC,IRESP)
       YREC='LEIC_WAT'
       CALL READ_SURF(&
                    HPROGRAM,YREC,DGF%XLEIC,IRESP)     
       YREC='GFLUXC_WAT'
       CALL READ_SURF(&
                    HPROGRAM,YREC,DGF%XGFLUXC,IRESP)
       YREC='SWDC_WAT'
       CALL READ_SURF(&
                    HPROGRAM,YREC,DGF%XSWDC,IRESP)
       YREC='SWUC_WAT'
       CALL READ_SURF(&
                    HPROGRAM,YREC,DGF%XSWUC,IRESP)
       YREC='LWDC_WAT'
       CALL READ_SURF(&
                    HPROGRAM,YREC,DGF%XLWDC,IRESP)
       YREC='LWUC_WAT'
       CALL READ_SURF(&
                    HPROGRAM,YREC,DGF%XLWUC,IRESP)
       YREC='FMUC_WAT'
       CALL READ_SURF(&
                    HPROGRAM,YREC,DGF%XFMUC,IRESP)
       YREC='FMVC_WAT'
       CALL READ_SURF(&
                    HPROGRAM,YREC,DGF%XFMVC,IRESP)
       YREC='EVAPC_WAT'
        CALL READ_SURF(&
                    HPROGRAM,YREC,DGF%XEVAPC,IRESP)
        YREC='SUBLC_WAT'
        CALL READ_SURF(&
                    HPROGRAM,YREC,DGF%XSUBLC,IRESP)              
      ENDIF
!
  ENDIF   
ELSE
  ALLOCATE(DGF%XRNC    (0))
  ALLOCATE(DGF%XHC     (0))
  ALLOCATE(DGF%XLEC    (0))
  ALLOCATE(DGF%XLEIC   (0))
  ALLOCATE(DGF%XGFLUXC (0))
  ALLOCATE(DGF%XEVAPC  (0))
  ALLOCATE(DGF%XSUBLC  (0))  
  ALLOCATE(DGF%XSWDC   (0))
  ALLOCATE(DGF%XSWUC   (0))
  ALLOCATE(DGF%XLWDC   (0))
  ALLOCATE(DGF%XLWUC   (0))
  ALLOCATE(DGF%XFMUC   (0))
  ALLOCATE(DGF%XFMVC   (0))  
ENDIF
!
!* parameters at 2m
!
IF (DGF%N2M>=1) THEN
  ALLOCATE(DGF%XRI     (KLU))
  ALLOCATE(DGF%XT2M    (KLU))
  ALLOCATE(DGF%XT2M_MIN(KLU))
  ALLOCATE(DGF%XT2M_MAX(KLU))
  ALLOCATE(DGF%XQ2M    (KLU))
  ALLOCATE(DGF%XHU2M   (KLU))
  ALLOCATE(DGF%XHU2M_MIN(KLU))
  ALLOCATE(DGF%XHU2M_MAX(KLU))
  ALLOCATE(DGF%XZON10M (KLU))
  ALLOCATE(DGF%XMER10M (KLU))
  ALLOCATE(DGF%XWIND10M (KLU))
  ALLOCATE(DGF%XWIND10M_MAX(KLU))
  !
  DGF%XRI      = XUNDEF
  DGF%XT2M     = XUNDEF
  DGF%XT2M_MIN = XUNDEF
  DGF%XT2M_MAX = 0.0
  DGF%XQ2M     = XUNDEF
  DGF%XHU2M    = XUNDEF
  DGF%XHU2M_MIN= XUNDEF
  DGF%XHU2M_MAX=-XUNDEF
  DGF%XZON10M  = XUNDEF
  DGF%XMER10M  = XUNDEF
  DGF%XWIND10M = XUNDEF
  DGF%XWIND10M_MAX = 0.0
ELSE
  ALLOCATE(DGF%XRI      (0))
  ALLOCATE(DGF%XT2M     (0))
  ALLOCATE(DGF%XT2M_MIN (0))
  ALLOCATE(DGF%XT2M_MAX (0))
  ALLOCATE(DGF%XQ2M     (0))
  ALLOCATE(DGF%XHU2M    (0))
  ALLOCATE(DGF%XHU2M_MIN(0))
  ALLOCATE(DGF%XHU2M_MAX(0))
  ALLOCATE(DGF%XZON10M  (0))
  ALLOCATE(DGF%XMER10M  (0))
  ALLOCATE(DGF%XWIND10M (0))
  ALLOCATE(DGF%XWIND10M_MAX(0))
END IF
!
!* transfer coefficients
!
IF (DGF%LCOEF) THEN
  ALLOCATE(DGF%XCD     (KLU))
  ALLOCATE(DGF%XCH     (KLU))
  ALLOCATE(DGF%XCE     (KLU))
  ALLOCATE(DGF%XZ0     (KLU))
  ALLOCATE(DGF%XZ0H    (KLU))
  !
  DGF%XCD      = XUNDEF
  DGF%XCH      = XUNDEF
  DGF%XCE      = XUNDEF
  DGF%XZ0      = XUNDEF
  DGF%XZ0H     = XUNDEF
ELSE
  ALLOCATE(DGF%XCD     (0))
  ALLOCATE(DGF%XCH     (0))
  ALLOCATE(DGF%XCE     (0))
  ALLOCATE(DGF%XZ0     (0))
  ALLOCATE(DGF%XZ0H    (0))
END IF
!
!* surface humidity
!
IF (DGF%LSURF_VARS) THEN
  ALLOCATE(DGF%XQS     (KLU))
  !
  DGF%XQS      = XUNDEF
ELSE
  ALLOCATE(DGF%XQS     (0))
END IF
!
!* Flake temperature profile
!
IF (DGMF%LWATER_PROFILE) THEN
   ALLOCATE (DGMF%XZW_PROFILE(COUNT(DGMF%XZWAT_PROFILE/= XUNDEF))) 
   ALLOCATE (DGMF%XTW_PROFILE(COUNT(DGMF%XZWAT_PROFILE/= XUNDEF),KLU)) 
   DGMF%XZW_PROFILE=DGMF%XZWAT_PROFILE(:COUNT(DGMF%XZWAT_PROFILE /= XUNDEF))
 ELSE
   ALLOCATE (DGMF%XZW_PROFILE(0)) 
   ALLOCATE (DGMF%XTW_PROFILE(0,0)) 
 END IF
!
!* Coupling field with earth systme model
!
!
IF(LCPL_LAKE)THEN
!
  ALLOCATE(F%XCPL_FLAKE_EVAP(KLU))
  ALLOCATE(F%XCPL_FLAKE_RAIN(KLU))
  ALLOCATE(F%XCPL_FLAKE_SNOW(KLU))
  F%XCPL_FLAKE_EVAP(:) = 0.0
  F%XCPL_FLAKE_RAIN(:) = 0.0
  F%XCPL_FLAKE_SNOW(:) = 0.0
!
ELSE
!
  ALLOCATE(F%XCPL_FLAKE_EVAP(0))
  ALLOCATE(F%XCPL_FLAKE_RAIN(0))
  ALLOCATE(F%XCPL_FLAKE_SNOW(0))
!
ENDIF
!
IF (LHOOK) CALL DR_HOOK('DIAG_FLAKE_INIT_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE DIAG_FLAKE_INIT_n
