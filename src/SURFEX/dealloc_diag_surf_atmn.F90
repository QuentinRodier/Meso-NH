!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #############################################################
      SUBROUTINE DEALLOC_DIAG_SURF_ATM_n
!     #############################################################
!
!!    AUTHOR
!!    ------
!!	V. Masson   *Meteo France*	
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2004
!!      Modified    01/2006 : sea flux parameterization.
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_DIAG_SURF_ATM_n,ONLY : XRN_TILE, XH_TILE, XLE_TILE, XGFLUX_TILE,         &
                                  XRI_TILE, XCD_TILE, XCH_TILE, XCE_TILE,           &
                                  XT2M_TILE, XTS_TILE, XQ2M_TILE, XHU2M_TILE,       &
                                  XZON10M_TILE, XMER10M_TILE, XLEI_TILE, XQS_TILE,  &
                                  XZ0_TILE, XZ0H_TILE, XT2M_MIN_TILE, XT2M_MAX_TILE,&
                                  XAVG_RN, XAVG_H, XAVG_LE, XAVG_LEI, XAVG_GFLUX,   &
                                  XAVG_RI, XAVG_CD, XAVG_CH, XAVG_CE,               &
                                  XAVG_T2M, XAVG_TS, XAVG_Q2M, XAVG_HU2M,           &
                                  XAVG_T2M_MIN_ZS,XAVG_Q2M_MIN_ZS,XAVG_HU2M_MIN_ZS, &
                                  XPS,XRHOA, XDIAG_TRAD, XDIAG_EMIS,                &
                                  XAVG_ZON10M, XAVG_MER10M, XAVG_SFCO2, XAVG_LEIC,  &
                                  XAVG_Z0, XAVG_Z0H, XRW_RAIN, XRW_SNOW,  XAVG_RNC, &
                                  XAVG_HC, XAVG_LEC, XAVG_GFLUXC, XAVG_SWDC,        &
                                  XAVG_SWUC, XAVG_LWDC, XAVG_LWUC, XAVG_FMUC,       &
                                  XAVG_FMVC, XRNC_TILE, XHC_TILE, XLEC_TILE,        &
                                  XGFLUXC_TILE, XSWDC_TILE, XSWUC_TILE, XLWDC_TILE, &
                                  XLWUC_TILE, XFMUC_TILE, XFMVC_TILE, XAVG_T2M_MIN, &
                                  XAVG_T2M_MAX, XLEIC_TILE, XHU2M_MIN_TILE,         &
                                  XAVG_HU2M_MIN, XHU2M_MAX_TILE, XAVG_HU2M_MAX,     &
                                  XWIND10M_TILE, XAVG_WIND10M, XWIND10M_MAX_TILE,   &
                                  XAVG_WIND10M_MAX  
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
!*       0.2   Declarations of local variables
!              -------------------------------
!
!-------------------------------------------------------------------------------
!
!

REAL(KIND=JPRB) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('DEALLOC_DIAG_SURF_ATM_N',0,ZHOOK_HANDLE)
DEALLOCATE(XRI_TILE     )
DEALLOCATE(XCD_TILE     )
DEALLOCATE(XCH_TILE     )
DEALLOCATE(XCE_TILE     )
DEALLOCATE(XQS_TILE     )
DEALLOCATE(XZ0_TILE     )
DEALLOCATE(XZ0H_TILE    )
DEALLOCATE(XRN_TILE     )
DEALLOCATE(XH_TILE      )
DEALLOCATE(XLE_TILE     )
DEALLOCATE(XLEI_TILE    )
DEALLOCATE(XGFLUX_TILE  )
DEALLOCATE(XT2M_TILE    )
DEALLOCATE(XTS_TILE     )
DEALLOCATE(XT2M_MIN_TILE)
DEALLOCATE(XT2M_MAX_TILE)
DEALLOCATE(XQ2M_TILE    )
DEALLOCATE(XHU2M_TILE   )
DEALLOCATE(XZON10M_TILE )
DEALLOCATE(XMER10M_TILE )
DEALLOCATE(XDIAG_TRAD   )
DEALLOCATE(XDIAG_EMIS   )
!
DEALLOCATE(XAVG_RI     )
DEALLOCATE(XAVG_CD     )
DEALLOCATE(XAVG_CH     )
DEALLOCATE(XAVG_CE     )
DEALLOCATE(XAVG_Z0     )
DEALLOCATE(XAVG_Z0H    )
DEALLOCATE(XAVG_RN     )
DEALLOCATE(XAVG_H      )
DEALLOCATE(XAVG_LE     )
DEALLOCATE(XAVG_LEI    )
DEALLOCATE(XAVG_GFLUX  )
DEALLOCATE(XAVG_T2M    )
DEALLOCATE(XAVG_TS     )
DEALLOCATE(XAVG_T2M_MIN)
DEALLOCATE(XAVG_T2M_MAX)
DEALLOCATE(XAVG_Q2M    )
DEALLOCATE(XAVG_HU2M   )
DEALLOCATE(XAVG_ZON10M )
DEALLOCATE(XAVG_MER10M )
DEALLOCATE(XAVG_SFCO2  )
DEALLOCATE(XAVG_T2M_MIN_ZS    )
DEALLOCATE(XAVG_Q2M_MIN_ZS    )
DEALLOCATE(XAVG_HU2M_MIN_ZS   )
DEALLOCATE(XPS                )
DEALLOCATE(XRHOA              )
!
DEALLOCATE(XRNC_TILE     )
DEALLOCATE(XHC_TILE      )
DEALLOCATE(XLEC_TILE     )
DEALLOCATE(XLEIC_TILE    )
DEALLOCATE(XGFLUXC_TILE  )
DEALLOCATE(XSWDC_TILE    )
DEALLOCATE(XSWUC_TILE    )
DEALLOCATE(XLWDC_TILE    )
DEALLOCATE(XLWUC_TILE    )
DEALLOCATE(XFMUC_TILE    )
DEALLOCATE(XFMVC_TILE    )
!
DEALLOCATE(XAVG_RNC     )
DEALLOCATE(XAVG_HC      )
DEALLOCATE(XAVG_LEC     )
DEALLOCATE(XAVG_LEIC    )
DEALLOCATE(XAVG_GFLUXC  )
DEALLOCATE(XAVG_SWDC    )
DEALLOCATE(XAVG_SWUC    )
DEALLOCATE(XAVG_LWDC    )
DEALLOCATE(XAVG_LWUC    )
DEALLOCATE(XAVG_FMUC    )
DEALLOCATE(XAVG_FMVC    )
!
DEALLOCATE(XRW_RAIN   )
DEALLOCATE(XRW_SNOW   )
!
DEALLOCATE(XHU2M_MIN_TILE    )
DEALLOCATE(XAVG_HU2M_MIN     )
DEALLOCATE(XHU2M_MAX_TILE    )
DEALLOCATE(XAVG_HU2M_MAX     )
DEALLOCATE(XWIND10M_TILE     )
DEALLOCATE(XAVG_WIND10M      )
DEALLOCATE(XWIND10M_MAX_TILE )
DEALLOCATE(XAVG_WIND10M_MAX  )
IF (LHOOK) CALL DR_HOOK('DEALLOC_DIAG_SURF_ATM_N',1,ZHOOK_HANDLE)
! 
!-------------------------------------------------------------------------------
!
END SUBROUTINE DEALLOC_DIAG_SURF_ATM_n
