!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     ######spl
      SUBROUTINE DIAG_MISC_TEB_INIT_n(HPROGRAM,KLU,KSW)
!     #####################
!
!!****  *DIAG_MISC_TEB_INIT_n* - routine to initialize TEB diagnostic variables
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
!!	V. Masson   *Meteo France*	
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2004 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_SURF_PAR,   ONLY : XUNDEF
USE MODD_TYPE_DATE_SURF
!
USE MODD_TEB_n,             ONLY : CBEM
USE MODD_DIAG_MISC_TEB_n,   ONLY : LSURF_MISC_BUDGET,                         &
                                     XQF_BLD, XQF_TOWN, XDQS_TOWN, XFLX_BLD,  &
                                     XRN_ROAD, XH_ROAD, XLE_ROAD, XGFLUX_ROAD,&
                                     XRN_WALL_A, XH_WALL_A, XGFLUX_WALL_A,    &
                                     XRN_WALL_B, XH_WALL_B, XGFLUX_WALL_B,    &
                                     XRN_ROOF, XH_ROOF, XLE_ROOF, XGFLUX_ROOF,&
                                     XRN_GARDEN,XH_GARDEN,XLE_GARDEN,         &
                                     XGFLUX_GARDEN,                           &
                                     XRN_STRLROOF, XH_STRLROOF,               &
                                     XLE_STRLROOF, XGFLUX_STRLROOF,           &
                                     XRN_GREENROOF, XH_GREENROOF,             &
                                     XLE_GREENROOF, XGFLUX_GREENROOF,         &
                                     XRUNOFF_GREENROOF, XDRAIN_GREENROOF,     &
                                     XRN_BLT,XH_BLT,XLE_BLT,XGFLUX_BLT,       &
                                     XABS_SW_ROOF ,XABS_SW_SNOW_ROOF,         &
                                     XABS_LW_ROOF ,XABS_LW_SNOW_ROOF,         &
                                     XABS_SW_ROAD ,XABS_SW_SNOW_ROAD,         &
                                     XABS_LW_ROAD ,XABS_LW_SNOW_ROAD,         &
                                     XABS_SW_WALL_A, XABS_SW_WALL_B,          &
                                     XABS_LW_WALL_A, XABS_LW_WALL_B,          &
                                     XABS_SW_GARDEN,XABS_LW_GARDEN,           &
                                     XABS_SW_GREENROOF,XABS_LW_GREENROOF,     &
                                     XG_GREENROOF_ROOF,                       &
                                     XH_BLD_COOL, XT_BLD_COOL,                &
                                     XH_BLD_HEAT, XLE_BLD_COOL, XLE_BLD_HEAT, &
                                     XH_WASTE, XLE_WASTE, XHVAC_COOL,         &
                                     XHVAC_HEAT, XCAP_SYS, XM_SYS, XCOP,      &
                                     XQ_SYS, XT_SYS, XTR_SW_WIN, XFAN_POWER,  &
                                     XABS_SW_WIN, XABS_LW_WIN, XEMIT_LW_GRND, &
                                     XEMIT_LW_FAC, XT_RAD_IND,                &
                                     XREF_SW_GRND, XREF_SW_FAC, XHU_BLD

!
USE MODI_READ_SURF
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
INTEGER, INTENT(IN) :: KLU   ! size of arrays
INTEGER, INTENT(IN) :: KSW   ! spectral bands
 CHARACTER(LEN=6), INTENT(IN):: HPROGRAM  ! program calling
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER           :: IRESP          ! IRESP  : return-code if a problem appears
 CHARACTER(LEN=12) :: YREC           ! Name of the article to be read
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!* surface energy budget
!
IF (LHOOK) CALL DR_HOOK('DIAG_MISC_TEB_INIT_N',0,ZHOOK_HANDLE)
!
!* miscellaneous fields
!
IF (LSURF_MISC_BUDGET) THEN
  ALLOCATE(XQF_BLD           (KLU))
  ALLOCATE(XFLX_BLD          (KLU))
  ALLOCATE(XQF_TOWN          (KLU))
  ALLOCATE(XDQS_TOWN         (KLU))
  ALLOCATE(XRN_ROAD          (KLU))
  ALLOCATE(XH_ROAD           (KLU))
  ALLOCATE(XLE_ROAD          (KLU))
  ALLOCATE(XGFLUX_ROAD       (KLU))
  ALLOCATE(XRN_WALL_A        (KLU))
  ALLOCATE(XH_WALL_A         (KLU))
  ALLOCATE(XGFLUX_WALL_A     (KLU))
  ALLOCATE(XRN_WALL_B        (KLU))
  ALLOCATE(XH_WALL_B         (KLU))
  ALLOCATE(XGFLUX_WALL_B     (KLU))
  ALLOCATE(XRN_ROOF          (KLU))
  ALLOCATE(XH_ROOF           (KLU))
  ALLOCATE(XLE_ROOF          (KLU))
  ALLOCATE(XGFLUX_ROOF       (KLU))
  ALLOCATE(XRN_GARDEN        (KLU))
  ALLOCATE(XH_GARDEN         (KLU))
  ALLOCATE(XLE_GARDEN        (KLU))
  ALLOCATE(XGFLUX_GARDEN     (KLU))
  ALLOCATE(XRN_BLT           (KLU))
  ALLOCATE(XH_BLT            (KLU))
  ALLOCATE(XLE_BLT           (KLU))
  ALLOCATE(XGFLUX_BLT        (KLU))
  ALLOCATE(XRN_STRLROOF      (KLU))
  ALLOCATE(XH_STRLROOF       (KLU))
  ALLOCATE(XLE_STRLROOF      (KLU))
  ALLOCATE(XGFLUX_STRLROOF   (KLU))
  ALLOCATE(XRN_GREENROOF     (KLU))
  ALLOCATE(XH_GREENROOF      (KLU))
  ALLOCATE(XLE_GREENROOF     (KLU))
  ALLOCATE(XGFLUX_GREENROOF  (KLU))
  ALLOCATE(XG_GREENROOF_ROOF (KLU))
  ALLOCATE(XRUNOFF_GREENROOF (KLU))
  ALLOCATE(XDRAIN_GREENROOF  (KLU))
  !
  ALLOCATE(XABS_SW_ROOF      (KLU))
  ALLOCATE(XABS_SW_SNOW_ROOF (KLU))
  ALLOCATE(XABS_LW_ROOF      (KLU))
  ALLOCATE(XABS_LW_SNOW_ROOF (KLU))
  ALLOCATE(XABS_SW_ROAD      (KLU))
  ALLOCATE(XABS_SW_SNOW_ROAD (KLU))
  ALLOCATE(XABS_LW_ROAD      (KLU))
  ALLOCATE(XABS_LW_SNOW_ROAD (KLU))
  ALLOCATE(XABS_SW_WALL_A    (KLU))
  ALLOCATE(XABS_SW_WALL_B    (KLU))
  ALLOCATE(XABS_LW_WALL_A    (KLU))
  ALLOCATE(XABS_LW_WALL_B    (KLU))
  ALLOCATE(XABS_SW_GARDEN    (KLU))
  ALLOCATE(XABS_LW_GARDEN    (KLU))
  ALLOCATE(XABS_SW_GREENROOF (KLU))
  ALLOCATE(XABS_LW_GREENROOF (KLU))
  !
  ALLOCATE(XREF_SW_FAC       (KLU))  
  ALLOCATE(XREF_SW_GRND      (KLU))
  !
  ALLOCATE(XEMIT_LW_FAC      (KLU))
  ALLOCATE(XEMIT_LW_GRND     (KLU))
  !
  IF (CBEM=='BEM') THEN
    ALLOCATE(XH_BLD_COOL    (KLU))
    ALLOCATE(XT_BLD_COOL    (KLU))
    ALLOCATE(XH_BLD_HEAT    (KLU))
    ALLOCATE(XLE_BLD_COOL   (KLU))
    ALLOCATE(XLE_BLD_HEAT   (KLU))
    ALLOCATE(XH_WASTE       (KLU))
    ALLOCATE(XLE_WASTE      (KLU))
    ALLOCATE(XHVAC_COOL     (KLU))
    ALLOCATE(XHVAC_HEAT     (KLU))
    ALLOCATE(XCAP_SYS       (KLU))
    ALLOCATE(XM_SYS         (KLU))
    ALLOCATE(XCOP           (KLU))
    ALLOCATE(XQ_SYS         (KLU))
    ALLOCATE(XT_SYS         (KLU))
    ALLOCATE(XTR_SW_WIN     (KLU))
    ALLOCATE(XFAN_POWER     (KLU))
    ALLOCATE(XT_RAD_IND     (KLU))
    ALLOCATE(XHU_BLD        (KLU))
    ALLOCATE(XABS_SW_WIN    (KLU)) 
    ALLOCATE(XABS_LW_WIN    (KLU))    
  ENDIF
  !
  XQF_BLD            = XUNDEF
  XFLX_BLD           = XUNDEF
  XQF_TOWN           = XUNDEF
  XDQS_TOWN          = XUNDEF
  XRN_ROAD           = XUNDEF
  XH_ROAD            = XUNDEF
  XLE_ROAD           = XUNDEF
  XGFLUX_ROAD        = XUNDEF
  XRN_WALL_A         = XUNDEF
  XH_WALL_A          = XUNDEF
  XGFLUX_WALL_A      = XUNDEF
  XRN_WALL_B         = XUNDEF
  XH_WALL_B          = XUNDEF
  XGFLUX_WALL_B      = XUNDEF
  XRN_ROOF           = XUNDEF
  XH_ROOF            = XUNDEF
  XLE_ROOF           = XUNDEF
  XGFLUX_ROOF        = XUNDEF 
  XRN_GARDEN         = XUNDEF
  XH_GARDEN          = XUNDEF
  XLE_GARDEN         = XUNDEF
  XGFLUX_GARDEN      = XUNDEF  
  XRN_BLT            = XUNDEF
  XH_BLT             = XUNDEF
  XLE_BLT            = XUNDEF
  XGFLUX_BLT         = XUNDEF  
  XRN_STRLROOF       = XUNDEF
  XH_STRLROOF        = XUNDEF
  XLE_STRLROOF       = XUNDEF
  XGFLUX_STRLROOF    = XUNDEF  
  XRN_GREENROOF      = XUNDEF
  XH_GREENROOF       = XUNDEF
  XLE_GREENROOF      = XUNDEF
  XGFLUX_GREENROOF   = XUNDEF  
  XG_GREENROOF_ROOF  = XUNDEF  
  XRUNOFF_GREENROOF  = XUNDEF  
  XDRAIN_GREENROOF   = XUNDEF  
!
  XABS_SW_ROOF       = XUNDEF  
  XABS_SW_SNOW_ROOF  = XUNDEF  
  XABS_LW_ROOF       = XUNDEF  
  XABS_LW_SNOW_ROOF  = XUNDEF  
  XABS_SW_ROAD       = XUNDEF  
  XABS_SW_SNOW_ROAD  = XUNDEF  
  XABS_LW_ROAD       = XUNDEF  
  XABS_LW_SNOW_ROAD  = XUNDEF  
  XABS_SW_WALL_A     = XUNDEF  
  XABS_SW_WALL_B     = XUNDEF  
  XABS_LW_WALL_A     = XUNDEF  
  XABS_LW_WALL_B     = XUNDEF  
  XABS_SW_GARDEN     = XUNDEF  
  XABS_LW_GARDEN     = XUNDEF 
  XABS_SW_GREENROOF  = XUNDEF  
  XABS_LW_GREENROOF  = XUNDEF 
  !
  XREF_SW_FAC        = XUNDEF
  XREF_SW_GRND       = XUNDEF
  !
  XEMIT_LW_FAC       = XUNDEF
  XEMIT_LW_GRND      = XUNDEF
  !
  IF (CBEM=='BEM') THEN
    XH_BLD_COOL     = XUNDEF
    XT_BLD_COOL     = XUNDEF
    XH_BLD_HEAT     = XUNDEF
    XLE_BLD_COOL    = XUNDEF
    XLE_BLD_HEAT    = XUNDEF
    XH_WASTE        = XUNDEF
    XLE_WASTE       = XUNDEF
    XHVAC_COOL      = XUNDEF
    XHVAC_HEAT      = XUNDEF
    XCAP_SYS        = XUNDEF
    XM_SYS          = XUNDEF
    XCOP            = XUNDEF
    XQ_SYS          = XUNDEF
    XT_SYS          = XUNDEF
    XTR_SW_WIN      = XUNDEF
    XFAN_POWER      = XUNDEF
    XT_RAD_IND      = XUNDEF
    XHU_BLD         = XUNDEF
    XABS_SW_WIN     = XUNDEF 
    XABS_LW_WIN     = XUNDEF    
  ENDIF
!  
ENDIF
!
IF (LHOOK) CALL DR_HOOK('DIAG_MISC_TEB_INIT_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE DIAG_MISC_TEB_INIT_n
