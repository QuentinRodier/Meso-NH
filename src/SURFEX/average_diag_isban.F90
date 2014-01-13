!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
      SUBROUTINE AVERAGE_DIAG_ISBA_n(PHW,PHT,PSFCO2)
!     #######################################
!
!
!!****  *AVERAGE_DIAG_ISBA_n*  
!!
!!    PURPOSE
!!    -------
!      Average the diagnostics from all ISBA tiles
!     
!!**  METHOD
!!    ------
!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------ 
!!
!!      
!!    REFERENCE
!!    ---------
!!      
!!    AUTHOR
!!    ------
!!	S. Belair           * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    10/03/95 
!!      V.Masson    20/03/96  remove abnormal averages and average TS**4 instead
!!                            of TS
!!      (J.Stein)   27/03/96 use only H and LE in the soil scheme
!!      A. Boone    27/11/02 revised to output ALMA variables, and general applications
!!      B. Decharme 17/08/09 cumulative radiatif budget
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
!
USE MODD_SURF_PAR,    ONLY : XUNDEF
USE MODD_ISBA_n,      ONLY : XPATCH, NPATCH, XLE
USE MODD_DIAG_EVAP_ISBA_n, ONLY : LSURF_BUDGETC                            
USE MODD_DIAG_ISBA_n, ONLY : N2M, LSURF_BUDGET, LCOEF, LSURF_VARS,            &
                               XAVG_RN, XAVG_H, XAVG_LE, XAVG_GFLUX, XAVG_RI, &
                               XAVG_CD, XAVG_CH, XAVG_CE, XAVG_T2M, XAVG_Q2M, &
                               XAVG_HU2M, XAVG_T2M_MIN, XAVG_T2M_MAX,         &
                               XAVG_ZON10M, XAVG_MER10M, XAVG_LEI,            &
                               XAVG_Z0, XAVG_Z0H, XAVG_Z0EFF, XAVG_QS,        &
                               XAVG_SWBD, XAVG_SWBU, XAVG_SWD, XAVG_SWU,      &
                               XAVG_LWD, XAVG_LWU, XAVG_FMU, XAVG_FMV,        &
                               XRN, XH, XGFLUX, XLEI, XRI, XCD, XCH, XCE,     &
                               XT2M, XQ2M, XHU2M, XZON10M, XMER10M,           &
                               XZ0_WITH_SNOW, XZ0H_WITH_SNOW, XZ0EFF, XQS,    &
                               XSWBD, XSWBU, XSWD, XSWU, XLWD, XLWU,          &
                               XFMU, XFMV, XSWDC, XSWUC, XLWDC, XLWUC,        &
                               XFMUC, XFMVC, XAVG_SWDC, XAVG_SWUC, XAVG_LWDC, &
                               XAVG_LWUC, XAVG_FMUC, XAVG_FMVC, XTS, XAVG_TS, &
                               XTSRAD, XAVG_TSRAD,                            &
                               XAVG_HU2M_MIN, XAVG_HU2M_MAX, XAVG_WIND10M,    &
                               XAVG_WIND10M_MAX, XWIND10M, XAVG_SFCO2  
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
REAL, DIMENSION(:), INTENT(IN)       :: PHW    ! atmospheric level height for wind
REAL, DIMENSION(:), INTENT(IN)       :: PHT    ! atmospheric level height
REAL, DIMENSION(:), INTENT(IN)       :: PSFCO2 ! CO2 flux
!
!*      0.2    declarations of local variables
!
INTEGER                              :: JPATCH ! tile loop counter
INTEGER                              :: JSWB   ! band loop counter
REAL, DIMENSION(SIZE(XPATCH,1))      :: ZSUMPATCH
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!       0.     Initialization
!              --------------
!
IF (LHOOK) CALL DR_HOOK('AVERAGE_DIAG_ISBA_N',0,ZHOOK_HANDLE)
ZSUMPATCH(:) = 0.
DO JPATCH=1,SIZE(XPATCH,2)
  ZSUMPATCH(:) = ZSUMPATCH(:) + XPATCH(:,JPATCH)
END DO
!
!       1.     Energy fluxes
!              -------------
!
IF (LSURF_BUDGET) THEN
  XAVG_RN(:)     = 0.
  XAVG_H (:)     = 0.
  XAVG_LE(:)     = 0.
  XAVG_LEI(:)    = 0.
  XAVG_GFLUX(:)  = 0.
  XAVG_SWD(:)    = 0.
  XAVG_SWU(:)    = 0.
  XAVG_LWD(:)    = 0.
  XAVG_LWU(:)    = 0.
  XAVG_FMU(:)    = 0.
  XAVG_FMV(:)    = 0.
  XAVG_SWBD(:,:) = 0.
  XAVG_SWBU(:,:) = 0.
  !
  DO JPATCH=1,SIZE(XPATCH,2)
    WHERE (ZSUMPATCH(:) > 0.)
!
! Net radiation
!
      XAVG_RN(:)  = XAVG_RN(:) +XPATCH(:,JPATCH) * XRN(:,JPATCH)
!
! Sensible heat flux
!
      XAVG_H (:)  = XAVG_H (:) +XPATCH(:,JPATCH) * XH (:,JPATCH)
!
! Total latent heat flux
!
      XAVG_LE(:)  = XAVG_LE(:) +XPATCH(:,JPATCH) * XLE(:,JPATCH)
!
! Sublimation latent heat flux
!
      XAVG_LEI(:) = XAVG_LEI(:) +XPATCH(:,JPATCH) * XLEI(:,JPATCH)
!
! Storage flux
!
      XAVG_GFLUX(:)  = XAVG_GFLUX(:) +XPATCH(:,JPATCH) * XGFLUX(:,JPATCH)
!
! Downwards SW radiation
!
      XAVG_SWD(:)  = XAVG_SWD(:) +XPATCH(:,JPATCH) * XSWD(:,JPATCH)
!
! Upwards SW radiation
!
      XAVG_SWU(:)  = XAVG_SWU(:) +XPATCH(:,JPATCH) * XSWU(:,JPATCH)
!
! Downwards LW radiation
!
      XAVG_LWD(:)  = XAVG_LWD(:) +XPATCH(:,JPATCH) * XLWD(:,JPATCH)
!
! Upwards LW radiation
!
      XAVG_LWU(:)  = XAVG_LWU(:) +XPATCH(:,JPATCH) * XLWU(:,JPATCH)
!
! Zonal wind stress
!
      XAVG_FMU(:)  = XAVG_FMU(:) +XPATCH(:,JPATCH) * XFMU(:,JPATCH)
!
! Meridian wind stress
!
      XAVG_FMV(:)  = XAVG_FMV(:) +XPATCH(:,JPATCH) * XFMV(:,JPATCH)
!
    END WHERE
  END DO
!
  DO JPATCH=1,SIZE(XPATCH,2)
    DO JSWB=1,SIZE(XSWBD,2)
      WHERE (ZSUMPATCH(:) > 0.)
!
! Downwards SW radiation for each spectral band
!
        XAVG_SWBD(:,JSWB)  = XAVG_SWBD(:,JSWB) +XPATCH(:,JPATCH) * XSWBD(:,JSWB,JPATCH)
!
! Upwards SW radiation for each spectral band
!
        XAVG_SWBU(:,JSWB)  = XAVG_SWBU(:,JSWB) +XPATCH(:,JPATCH) * XSWBU(:,JSWB,JPATCH)
!
      END WHERE
    END DO
  END DO
END IF
!
IF (LSURF_BUDGETC) THEN
   XAVG_SWDC(:) = 0.
   XAVG_SWUC(:) = 0.
   XAVG_LWDC(:) = 0.
   XAVG_LWUC(:) = 0.
   XAVG_FMUC(:) = 0.
   XAVG_FMVC(:) = 0.
   DO JPATCH=1,SIZE(XPATCH,2)
      WHERE (ZSUMPATCH(:) > 0.)
!
!        Downwards SW radiation
!
         XAVG_SWDC(:) = XAVG_SWDC(:) + XPATCH(:,JPATCH) * XSWDC(:,JPATCH)
!
!        Upwards SW radiation
!
         XAVG_SWUC(:) = XAVG_SWUC(:) + XPATCH(:,JPATCH) * XSWUC(:,JPATCH)
!
!        Downwards LW radiation
!
         XAVG_LWDC(:) = XAVG_LWDC(:) + XPATCH(:,JPATCH) * XLWDC(:,JPATCH)
!
!        Upwards LW radiation
!
         XAVG_LWUC(:) = XAVG_LWUC(:) + XPATCH(:,JPATCH) * XLWUC(:,JPATCH)
!
!        Zonal wind stress
!
         XAVG_FMUC(:) = XAVG_FMUC(:) + XPATCH(:,JPATCH) * XFMUC(:,JPATCH)
!
!        Meridian wind stress
!
         XAVG_FMVC(:) = XAVG_FMVC(:) + XPATCH(:,JPATCH) * XFMVC(:,JPATCH)
!
    END WHERE
  END DO
ENDIF    
!
!
!       2.     surface temperature and 2 meters parameters
!              -------------------------------------------
!
XAVG_TS(:) = 0.0
DO JPATCH=1,SIZE(XPATCH,2)
    WHERE (ZSUMPATCH(:) > 0.)
       XAVG_TS(:)  = XAVG_TS(:) + XPATCH(:,JPATCH) * XTS(:,JPATCH)
    END WHERE
END DO
XAVG_TSRAD(:) = 0.0
DO JPATCH=1,SIZE(XPATCH,2)
    WHERE (ZSUMPATCH(:) > 0.)
       XAVG_TSRAD(:)  = XAVG_TSRAD(:) + XPATCH(:,JPATCH) * XTSRAD(:,JPATCH)
    END WHERE
END DO

!
IF (N2M>=1) THEN

  XAVG_T2M(:)  = 0.
  XAVG_Q2M(:)  = 0.
  XAVG_HU2M(:)  = 0.
  XAVG_RI(:)  = 0.
  !
  XAVG_SFCO2(:)  = PSFCO2(:)
  !
  DO JPATCH=1,SIZE(XPATCH,2)
    WHERE (ZSUMPATCH(:) > 0.)
!
! 2 meters temperature
!
      XAVG_T2M(:)  = XAVG_T2M(:) + XPATCH(:,JPATCH) * XT2M(:,JPATCH)
!
! 2 meters humidity
!
      XAVG_Q2M(:)  = XAVG_Q2M(:) + XPATCH(:,JPATCH) * XQ2M(:,JPATCH)
!
! 2 meters relative humidity
!
      XAVG_HU2M(:)  = XAVG_HU2M(:) + XPATCH(:,JPATCH) * XHU2M(:,JPATCH)
!
! Richardson number
!
      XAVG_RI(:)  = XAVG_RI(:) + XPATCH(:,JPATCH) * XRI(:,JPATCH)
!
    END WHERE
  END DO
!
! 10 meters wind
!
  WHERE(PHW(:)>=10.) 
    XAVG_ZON10M (:)  = 0.
    XAVG_MER10M (:)  = 0.
    XAVG_WIND10M(:)  = 0.
  ELSEWHERE
    XAVG_ZON10M (:)  = XUNDEF
    XAVG_MER10M (:)  = XUNDEF
    XAVG_WIND10M(:)  = XUNDEF
  END WHERE
  DO JPATCH=1,SIZE(XPATCH,2)
    WHERE (ZSUMPATCH(:) > 0. .AND. PHW(:)>=10.)
      XAVG_ZON10M(:)  = XAVG_ZON10M (:) + XPATCH(:,JPATCH) * XZON10M (:,JPATCH)
      XAVG_MER10M(:)  = XAVG_MER10M (:) + XPATCH(:,JPATCH) * XMER10M (:,JPATCH)
      XAVG_WIND10M(:) = XAVG_WIND10M(:) + XPATCH(:,JPATCH) * XWIND10M(:,JPATCH)
    END WHERE
  ENDDO
!
  XAVG_T2M_MIN(:) = MIN(XAVG_T2M_MIN(:),XAVG_T2M(:))
  XAVG_T2M_MAX(:) = MAX(XAVG_T2M_MAX(:),XAVG_T2M(:))
!
  XAVG_HU2M_MIN(:) = MIN(XAVG_HU2M_MIN(:),XAVG_HU2M(:))
  XAVG_HU2M_MAX(:) = MAX(XAVG_HU2M_MAX(:),XAVG_HU2M(:))
!
  XAVG_WIND10M_MAX(:) = MAX(XAVG_WIND10M_MAX(:),XAVG_WIND10M(:))
!
END IF
!
!
!       3.     Transfer coefficients
!              ---------------------
!
IF (LCOEF) THEN
  !
  XAVG_CD   (:) = 0.
  XAVG_CH   (:) = 0.
  XAVG_CE   (:) = 0.
  XAVG_Z0   (:) = 0.
  XAVG_Z0H  (:) = 0.
  XAVG_Z0EFF(:) = 0.
  !
  DO JPATCH=1,SIZE(XPATCH,2)
    WHERE (ZSUMPATCH(:) > 0.)
      !
      XAVG_CD(:)  = XAVG_CD(:) + XPATCH(:,JPATCH) * XCD(:,JPATCH)
      !
      XAVG_CH(:)  = XAVG_CH(:) + XPATCH(:,JPATCH) * XCH(:,JPATCH)
      !
      XAVG_CE(:)  = XAVG_CE(:) + XPATCH(:,JPATCH) * XCE(:,JPATCH)
      !
      !             
      XAVG_Z0(:)    = XAVG_Z0(:)    + XPATCH(:,JPATCH) * 1./(LOG(PHW(:)/XZ0_WITH_SNOW (:,JPATCH)))**2
      !      
      XAVG_Z0H(:)   = XAVG_Z0H(:)   + XPATCH(:,JPATCH) * 1./(LOG(PHT(:)/XZ0H_WITH_SNOW(:,JPATCH)))**2
      !      
      XAVG_Z0EFF(:) = XAVG_Z0EFF(:) + XPATCH(:,JPATCH) * 1./(LOG(PHW(:)/XZ0EFF        (:,JPATCH)))**2
      !      
    END WHERE
  END DO
  !
  XAVG_Z0(:)    = PHW(:) *  EXP( - SQRT(1./XAVG_Z0(:)) )
  !
  XAVG_Z0H(:)   = PHT(:) *  EXP( - SQRT(1./XAVG_Z0H(:)) )
  !
  XAVG_Z0EFF(:) = PHW(:) *  EXP( - SQRT(1./XAVG_Z0EFF(:)) )
  !
END IF
!
IF (LSURF_VARS) THEN
  XAVG_QS(:)  = 0.
  !
  DO JPATCH=1,SIZE(XPATCH,2)
    WHERE (ZSUMPATCH(:) > 0.)
!
! specific humidity at surface
!
      XAVG_QS(:)  = XAVG_QS(:) + XPATCH(:,JPATCH) * XQS(:,JPATCH)
!
    END WHERE
  END DO
END IF
!
IF (LHOOK) CALL DR_HOOK('AVERAGE_DIAG_ISBA_N',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE AVERAGE_DIAG_ISBA_n
