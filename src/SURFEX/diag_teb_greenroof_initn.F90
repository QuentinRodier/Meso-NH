!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE DIAG_TEB_GREENROOF_INIT_n (DGMTO, DGGR, TGRO, TVG, &
                                            HPROGRAM,KLU,KSW)
!     #####################
!
!!****  *DIAG_TEB_GREENROOF_INIT_n* - routine to initialize TEB-ISBA diagnostic variables
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!    Based on diag_teb_garden_initn
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
!!      Original                  02/2003 
!!      modified                  11/2003  by P. LeMoigne: surface cumulated energy budget
!!      modified                  10/2004  by P. LeMoigne: surface miscellaneous fields
!!      B. Decharme               2008     New diag for water budget and allow to reset
!!                                         cumulatives variables at the beginning of a run
!       C. de Munck & A. Lemonsu  09/2011  Greenroofs
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
USE MODD_DIAG_MISC_TEB_OPTION_n, ONLY : DIAG_MISC_TEB_OPTIONS_t
USE MODD_DIAG_TEB_GREENROOF_n, ONLY : DIAG_TEB_GREENROOF_t
USE MODD_TEB_GREENROOF_OPTION_n, ONLY : TEB_GREENROOF_OPTIONS_t
USE MODD_TEB_VEG_n, ONLY : TEB_VEG_OPTIONS_t
!
USE MODD_SURF_PAR,             ONLY : XUNDEF
USE MODD_TYPE_DATE_SURF
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
TYPE(DIAG_MISC_TEB_OPTIONS_t), INTENT(INOUT) :: DGMTO
TYPE(DIAG_TEB_GREENROOF_t), INTENT(INOUT) :: DGGR
TYPE(TEB_GREENROOF_OPTIONS_t), INTENT(INOUT) :: TGRO
TYPE(TEB_VEG_OPTIONS_t), INTENT(INOUT) :: TVG
!
INTEGER, INTENT(IN)         :: KLU       ! size of arrays
INTEGER, INTENT(IN)         :: KSW       ! spectral bands
 CHARACTER(LEN=6), INTENT(IN):: HPROGRAM  ! program calling
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER           :: IRESP          ! IRESP  : return-code if a problem appears
 CHARACTER(LEN=LEN_HREC) :: YREC           ! Name of the article to be read
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('DIAG_TEB_GREENROOF_INIT_N',0,ZHOOK_HANDLE)
DGGR%XCG         = XUNDEF
DGGR%XC1         = XUNDEF
DGGR%XC2         = XUNDEF
DGGR%XWGEQ       = XUNDEF
DGGR%XCT         = XUNDEF
DGGR%XRS         = XUNDEF
DGGR%XHU         = XUNDEF
DGGR%XHUG        = XUNDEF
DGGR%XHV         = XUNDEF
DGGR%XRESTORE    = XUNDEF
DGGR%XRI         = XUNDEF
DGGR%XUSTAR      = XUNDEF
DGGR%XRN         = XUNDEF
DGGR%XH          = XUNDEF
DGGR%XGFLUX      = XUNDEF
DGGR%XSNOWTEMP   = XUNDEF
DGGR%XSNOWLIQ    = XUNDEF
DGGR%XSNOWDZ     = XUNDEF
DGGR%XSNOWHMASS  = XUNDEF
DGGR%XMELTADV    = XUNDEF
IF (TVG%CPHOTO/='NON') THEN
  DGGR%XIACAN    = XUNDEF
END IF
DGGR%XCD         = XUNDEF
DGGR%XCDN        = XUNDEF
DGGR%XCH         = XUNDEF
DGGR%XQS         = XUNDEF
DGGR%XLEI        = XUNDEF
DGGR%XLEG        = XUNDEF
DGGR%XLEGI       = XUNDEF
DGGR%XLEV        = XUNDEF
DGGR%XLES        = XUNDEF
DGGR%XLER        = XUNDEF
DGGR%XLETR       = XUNDEF
DGGR%XEVAP       = XUNDEF
DGGR%XDRAIN      = XUNDEF
DGGR%XRUNOFF     = XUNDEF
DGGR%XHORT       = XUNDEF
DGGR%XDRIP       = XUNDEF
DGGR%XRRVEG      = XUNDEF
DGGR%XMELT       = XUNDEF
DGGR%XALBT       = XUNDEF
DGGR%XEMIST      = XUNDEF
!
!* surface energy budget
!
!IF (LSURF_BUDGET) THEN
  !
  ALLOCATE(DGGR%XSWD      (KLU))
  ALLOCATE(DGGR%XSWU      (KLU))
  ALLOCATE(DGGR%XSWBD     (KLU,KSW))
  ALLOCATE(DGGR%XSWBU     (KLU,KSW))
  ALLOCATE(DGGR%XLWD      (KLU))
  ALLOCATE(DGGR%XLWU      (KLU))
  ALLOCATE(DGGR%XFMU      (KLU))
  ALLOCATE(DGGR%XFMV      (KLU))
  !
  DGGR%XSWD     = XUNDEF
  DGGR%XSWU     = XUNDEF
  DGGR%XSWBD    = XUNDEF
  DGGR%XSWBU    = XUNDEF
  DGGR%XLWD     = XUNDEF
  DGGR%XLWU     = XUNDEF
  DGGR%XFMU     = XUNDEF
  DGGR%XFMV     = XUNDEF
  !
!END IF
!
!* surface temperature and parameters at 2m
!
ALLOCATE(DGGR%XTS    (KLU))
DGGR%XTS     = XUNDEF
ALLOCATE(DGGR%XTSRAD (KLU))
DGGR%XTSRAD  = XUNDEF
!
!* miscellaneous surface fields
!
IF (DGMTO%LSURF_MISC_BUDGET) THEN
  !
  ALLOCATE(DGGR%XSWI    (KLU,TGRO%NLAYER_GR))
  ALLOCATE(DGGR%XTSWI   (KLU,TGRO%NLAYER_GR))
  ALLOCATE(DGGR%XTWSNOW (KLU))
  ALLOCATE(DGGR%XTDSNOW (KLU))
  DGGR%XSWI     = XUNDEF
  DGGR%XTSWI    = XUNDEF
  DGGR%XTWSNOW  = XUNDEF
  DGGR%XTDSNOW  = XUNDEF
ENDIF


  ALLOCATE(DGGR%XALBT   (KLU))
  ALLOCATE(DGGR%XGPP    (KLU))
  ALLOCATE(DGGR%XRESP_AUTO  (KLU))
  ALLOCATE(DGGR%XRESP_ECO   (KLU))
  !
  DGGR%XALBT    = XUNDEF
  DGGR%XGPP     = XUNDEF
  DGGR%XRESP_AUTO   = XUNDEF
  DGGR%XRESP_ECO    = XUNDEF  
  !
!END IF
!
!* transfer coefficients
!
!IF (LCOEF) THEN
  !
  ALLOCATE(DGGR%XCE            (KLU))
  ALLOCATE(DGGR%XZ0_WITH_SNOW  (KLU))
  ALLOCATE(DGGR%XZ0H_WITH_SNOW (KLU))
  ALLOCATE(DGGR%XZ0EFF         (KLU))
  !
  DGGR%XCE            = XUNDEF
  DGGR%XZ0_WITH_SNOW  = XUNDEF
  DGGR%XZ0H_WITH_SNOW = XUNDEF
  DGGR%XZ0EFF         = XUNDEF
!END IF
!
!
!* surface humidity
!
!IF (LSURF_VARS) THEN
  ALLOCATE(DGGR%XQS            (KLU))
  !
  DGGR%XQS            = XUNDEF
!END IF
!
IF (LHOOK) CALL DR_HOOK('DIAG_TEB_GREENROOF_INIT_N',1,ZHOOK_HANDLE)
!
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE DIAG_TEB_GREENROOF_INIT_n
