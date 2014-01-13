!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #######
SUBROUTINE CLI_LAKE
!     ###############
!
!!****  *CLI_LAKE* - prepares input for lake variables from climate data
!!
!!    PURPOSE
!!    -------
!
!!**  METHOD
!!    ------
!!
!!    REFERENCE
!!    ---------
!!      
!!
!!    AUTHOR
!!    ------
!!     E. Kourzeneva
!!
!!    MODIFICATIONS
!!    -------------
!!------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_FLAKE_GRID_n, ONLY : XLAT, XLON
USE MODD_FLAKE_n,      ONLY : XTS, XT_SNOW, XT_ICE, XT_MNW, XT_WML, &
                              XT_BOT, XT_B1, TTIME, &  
                              XCT, XH_SNOW, XH_ICE, XH_ML, XH_B1, &  
                              XWATER_DEPTH
!
USE MODI_START_LAKE_OF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
!*      0.2    declarations of local variables
!
INTEGER :: JI
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------------

IF (LHOOK) CALL DR_HOOK('CLI_LAKE',0,ZHOOK_HANDLE)

DO JI=1,SIZE(XLAT)
  CALL START_LAKE_OF(TTIME%TDATE%DAY,TTIME%TDATE%MONTH,XLON(JI),XLAT(JI),&
        XWATER_DEPTH(JI), XT_SNOW(JI),XT_ICE(JI),XT_MNW(JI),XT_WML(JI), &
        XT_BOT(JI),XT_B1(JI),XCT(JI), &
        XH_SNOW(JI),XH_ICE(JI),XH_ML(JI),XH_B1(JI),XTS(JI))
 
END DO

IF (LHOOK) CALL DR_HOOK('CLI_LAKE',1,ZHOOK_HANDLE)

END SUBROUTINE CLI_LAKE
