!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
      SUBROUTINE FLAG_TEB_GREENROOF_n(KFLAG)
!     ##################################
!
!!****  *FLAG_TEB_GREENROOF_n* - routine to flag ISBA variables where green roofs are
!!                            not present
!!                         
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
!!      Original    10/2011
!!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
USE MODD_CO2V_PAR,       ONLY : XANFMINIT, XCONDCTMIN
USE MODD_TEB_n,          ONLY : XGREENROOF
USE MODD_TEB_VEG_n,      ONLY : CPHOTO, CISBA, CRESPSL
USE MODD_TEB_GREENROOF_n,   ONLY : NLAYER_GR,                      &
                                XTG, XWG, XWGI, XWR, XLAI, TSNOW,   &
                                XRESA, XANFM, XAN, XLE, XANDAY,     &
                                XBSLAI, XBIOMASS, XRESP_BIOMASS,    &
                                XSNOWFREE_ALB, XSNOWFREE_ALB_VEG,   &
                                XSNOWFREE_ALB_SOIL
!                                
USE MODD_SURF_PAR,       ONLY : XUNDEF
!
USE MODI_FLAG_GR_SNOW
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
INTEGER, INTENT(IN) :: KFLAG ! 1 : to put physical values to run ISBA afterwards
!                            ! 2 : to flag with XUNDEF value for points without green roof
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
REAL :: ZWR, ZTG, ZWG, ZRESA, ZANFM, ZDEF
INTEGER :: JL1, JL2 ! loop counter on layers
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!
!* 1D physical dimension
!
IF (LHOOK) CALL DR_HOOK('FLAG_TEB_GREENROOF_N',0,ZHOOK_HANDLE)
!
ZWR = XUNDEF
!
IF (KFLAG==1) THEN
  ZTG   = 300.
  ZWG   = 0.5
  ZRESA = 100.
  ZANFM = XANFMINIT
  ZDEF  = 0.
ELSEIF (KFLAG==2) THEN
  ZTG   = XUNDEF
  ZWG   = XUNDEF
  ZRESA = XUNDEF
  ZANFM = XUNDEF
  ZDEF  = XUNDEF
ENDIF
!
!-------------------------------------------------------------------------------
!     
  !
  DO JL1=1,NLAYER_GR
    WHERE (XGREENROOF(:)==0.) 
      XTG (:,JL1) = ZTG
      XWG (:,JL1) = ZWG
      XWGI(:,JL1) = ZDEF
    END WHERE
  END DO
  !
  WHERE (XGREENROOF(:)==0.) 
    XWR  (:) = ZWR
    XRESA(:) = ZRESA
  END WHERE
  !
  IF (CPHOTO/='NON') THEN
    !
    WHERE (XGREENROOF(:)==0.)
      XANFM (:) = ZANFM              
      XAN   (:) = ZDEF
      XANDAY(:) = ZDEF
      XLE   (:) = ZDEF
    END WHERE
    !
  ELSE IF (CPHOTO=='LAI' .OR. CPHOTO=='LST' .OR. CPHOTO=='NIT' .OR. CPHOTO=='NCB') THEN
    !
    WHERE (XGREENROOF(:)==0.) XLAI(:) = ZDEF
    !
  ELSE IF (CPHOTO=='AGS' .OR. CPHOTO=='AST') THEN
    !
    DO JL1=1,SIZE(XBIOMASS,2)
      WHERE (XGREENROOF(:)==0.)
        XBIOMASS     (:,JL1) = ZDEF
        XRESP_BIOMASS(:,JL1) = ZDEF
      END WHERE
    END DO
      !
  END IF
    !
!ENDIF
  !
!
!-------------------------------------------------------------------------------
!
!* Flag snow characteristics
!
 CALL FLAG_GR_SNOW(KFLAG,XGREENROOF(:)==0.,TSNOW)
!
!
!* snow-free characteristics
!
IF (KFLAG==1) THEN
  WHERE (XGREENROOF==0.) XSNOWFREE_ALB      = 0.2
  WHERE (XGREENROOF==0.) XSNOWFREE_ALB_VEG  = 0.2
  WHERE (XGREENROOF==0.) XSNOWFREE_ALB_SOIL = 0.2
ELSEIF (KFLAG==2) THEN
  WHERE (XGREENROOF==0.) XSNOWFREE_ALB      = XUNDEF
  WHERE (XGREENROOF==0.) XSNOWFREE_ALB_VEG  = XUNDEF
  WHERE (XGREENROOF==0.) XSNOWFREE_ALB_SOIL = XUNDEF
END IF
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('FLAG_TEB_GREENROOF_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE FLAG_TEB_GREENROOF_n
