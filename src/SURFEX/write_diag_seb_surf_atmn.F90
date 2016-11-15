!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE WRITE_DIAG_SEB_SURF_ATM_n (DTCO, DGU, U, UG, &
                                            HPROGRAM)
!     #################################
!
!!****  *WRITE_DIAG_SEB_SURF_ATM_n* - writes surface diagnostics
!!
!!    PURPOSE
!!    -------
!!
!!
!!**  METHOD
!!    ------
!!
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
!!      Modified    08/2009 : cumulated diag
!!      Juan        6/12/2011: parallel bug , remove local ANY(XAVG_ZON10M) test
!!      B. Decharme  06/13   Add QS, evap and sublimation diags
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_DIAG_SURF_ATM_n, ONLY : DIAG_SURF_ATM_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
!
USE MODD_SURF_PAR, ONLY : XUNDEF
!
USE MODI_INIT_IO_SURF_n
USE MODI_WRITE_SURF
USE MODI_END_IO_SURF_n
USE MODI_SUM_ON_ALL_PROCS
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
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(DIAG_SURF_ATM_t), INTENT(INOUT) :: DGU
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
!
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! program calling
!
!*       0.2   Declarations of local variables
!              -------------------------------
!

INTEGER           :: IRESP          ! IRESP  : return-code if a problem appears
 CHARACTER(LEN=LEN_HREC) :: YRECFM         ! Name of the article to be read
 CHARACTER(LEN=100):: YCOMMENT       ! Comment string
 CHARACTER(LEN=2)  :: YNUM
!
INTEGER           :: JSW
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!         Initialisation for IO
!
IF (LHOOK) CALL DR_HOOK('WRITE_DIAG_SEB_SURF_ATM_N',0,ZHOOK_HANDLE)
 CALL INIT_IO_SURF_n(DTCO, DGU, U, &
                     HPROGRAM,'FULL  ','SURF  ','WRITE')
!
!
!*       1.     Richardson number :
!               -----------------
!
IF (DGU%N2M>=1) THEN
  !        
  YRECFM='RI'
  YCOMMENT='X_Y_'//YRECFM
  !
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGU%XAVG_RI(:),IRESP,HCOMMENT=YCOMMENT)
  !
ENDIF
!
!*       2.     parameters at surface, 2 and 10 meters :
!               ----------------------------------------
!
IF (DGU%N2M>=1.OR.DGU%LSURF_BUDGET.OR.DGU%LSURF_BUDGETC) THEN
  !
  YRECFM='TS'
  YCOMMENT='X_Y_'//YRECFM//' (K)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGU%XAVG_TS(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='TSRAD'
  YCOMMENT='X_Y_'//YRECFM//' (K)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGU%XDIAG_TRAD(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='EMIS'
  YCOMMENT='X_Y_'//YRECFM//' (-)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGU%XDIAG_EMIS(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='SFCO2'
  YCOMMENT='X_Y_'//YRECFM//' (M.kgCO2.S-1.kgAIR-1)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGU%XAVG_SFCO2(:),IRESP,HCOMMENT=YCOMMENT)
  !
ENDIF
!
IF (DGU%N2M>=1) THEN
  !
  YRECFM='T2M'
  YCOMMENT='X_Y_'//YRECFM//' (K)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGU%XAVG_T2M(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='T2MMIN'
  YCOMMENT='X_Y_'//YRECFM//' (K)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGU%XAVG_T2M_MIN(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='T2MMAX'
  YCOMMENT='X_Y_'//YRECFM//' (K)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGU%XAVG_T2M_MAX(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='Q2M'
  YCOMMENT='X_Y_'//YRECFM//' (KG/KG)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGU%XAVG_Q2M(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='HU2M'
  YCOMMENT='X_Y_'//YRECFM//' (-)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGU%XAVG_HU2M(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='HU2MMIN'
  YCOMMENT='X_Y_'//YRECFM//' (-)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGU%XAVG_HU2M_MIN(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='HU2MMAX'
  YCOMMENT='X_Y_'//YRECFM//' (-)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGU%XAVG_HU2M_MAX(:),IRESP,HCOMMENT=YCOMMENT)
  !
  IF ( SUM_ON_ALL_PROCS(HPROGRAM,UG%CGRID,DGU%XAVG_ZON10M(:)/= XUNDEF) > 0. ) THEN
    !
    YRECFM='ZON10M'
    YCOMMENT='X_Y_'//YRECFM//' (M/S)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGU%XAVG_ZON10M(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='MER10M'
    YCOMMENT='X_Y_'//YRECFM//' (M/S)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGU%XAVG_MER10M(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='W10M'
    YCOMMENT='X_Y_'//YRECFM//' (M/S)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGU%XAVG_WIND10M(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='W10MMAX'
    YCOMMENT='X_Y_'//YRECFM//' (M/S)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGU%XAVG_WIND10M_MAX(:),IRESP,HCOMMENT=YCOMMENT)
    !
  ENDIF
  !
  IF (DGU%L2M_MIN_ZS) THEN
    !
    YRECFM='T2M_MIN_ZS'
    YCOMMENT='X_Y_'//YRECFM//' (K)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGU%XAVG_T2M_MIN_ZS(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='Q2M_MIN_ZS'
    YCOMMENT='X_Y_'//YRECFM//' (KG/KG)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGU%XAVG_Q2M_MIN_ZS(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='HU2M_MIN_ZS'
    YCOMMENT='X_Y_'//YRECFM//' (KG/KG)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGU%XAVG_HU2M_MIN_ZS(:),IRESP,HCOMMENT=YCOMMENT)
    !
  END IF
  !
END IF
!
!*       3.     Energy fluxes :
!               -------------
!
IF (DGU%LSURF_BUDGET) THEN
  !
  YRECFM='RN'
  YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGU%XAVG_RN(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='H'
  YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGU%XAVG_H(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='LE'
  YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGU%XAVG_LE(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='LEI'
  YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGU%XAVG_LEI(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='GFLUX'
  YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGU%XAVG_GFLUX(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='EVAP'
  YCOMMENT='X_Y_'//YRECFM//' (kg/m2/s)'
  !
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGU%XAVG_EVAP(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='SUBL'
  YCOMMENT='X_Y_'//YRECFM//' (kg/m2/s)'
  !
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGU%XAVG_SUBL(:),IRESP,HCOMMENT=YCOMMENT)
  !
  IF (DGU%LRAD_BUDGET) THEN
    !         
    YRECFM='SWD'
    YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGU%XAVG_SWD(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='SWU'
    YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGU%XAVG_SWU(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='LWD'
    YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGU%XAVG_LWD(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='LWU'
    YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGU%XAVG_LWU(:),IRESP,HCOMMENT=YCOMMENT)
    !
    DO JSW=1, SIZE(DGU%XAVG_SWBD,2)
      YNUM=ACHAR(48+JSW)
      !
      YRECFM='SWD_'//YNUM
      YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
      CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGU%XAVG_SWBD(:,JSW),IRESP,HCOMMENT=YCOMMENT)
      !
      YRECFM='SWU_'//YNUM
      YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
      CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGU%XAVG_SWBU(:,JSW),IRESP,HCOMMENT=YCOMMENT)
      !
    ENDDO
    !
  ENDIF
  !
  YRECFM='FMUNOSSO'
  YCOMMENT='X_Y_'//YRECFM//' (kg/ms2)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGU%XAVG_FMU(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='FMVNOSSO'
  YCOMMENT='X_Y_'//YRECFM//' (kg/ms2)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGU%XAVG_FMV(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='FMU'
  YCOMMENT='X_Y_'//YRECFM//' (kg/ms2)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGU%XSSO_FMU(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='FMV'
  YCOMMENT='X_Y_'//YRECFM//' (kg/ms2)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGU%XSSO_FMV(:),IRESP,HCOMMENT=YCOMMENT)
  !
END IF
!
! * Cumulated diag
!
IF (DGU%LSURF_BUDGETC) THEN
  !
  YRECFM='RNC'
  YCOMMENT='X_Y_'//YRECFM//' (J/m2)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGU%XAVG_RNC(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='HC'
  YCOMMENT='X_Y_'//YRECFM//' (J/m2)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGU%XAVG_HC(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='LEC'
  YCOMMENT='X_Y_'//YRECFM//' (J/m2)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGU%XAVG_LEC(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='LEIC'
  YCOMMENT='X_Y_'//YRECFM//' (J/m2)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGU%XAVG_LEIC(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='GFLUXC'
  YCOMMENT='X_Y_'//YRECFM//' (J/m2)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGU%XAVG_GFLUXC(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='EVAPC'
  YCOMMENT='X_Y_'//YRECFM//' (kg/m2)'
  !
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGU%XAVG_EVAPC(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='SUBLC'
  YCOMMENT='X_Y_'//YRECFM//' (kg/m2)'
  !
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGU%XAVG_SUBLC(:),IRESP,HCOMMENT=YCOMMENT)
  !
  IF (DGU%LRAD_BUDGET .OR. (DGU%LSURF_BUDGETC .AND. .NOT.DGU%LRESET_BUDGETC)) THEN
    !        
    YRECFM='SWDC'
    YCOMMENT='X_Y_'//YRECFM//' (J/m2)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGU%XAVG_SWDC(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='SWUC'
    YCOMMENT='X_Y_'//YRECFM//' (J/m2)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGU%XAVG_SWUC(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='LWDC'
    YCOMMENT='X_Y_'//YRECFM//' (J/m2)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGU%XAVG_LWDC(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='LWUC'
    YCOMMENT='X_Y_'//YRECFM//' (J/m2)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGU%XAVG_LWUC(:),IRESP,HCOMMENT=YCOMMENT)
    !
  ENDIF
  !
  YRECFM='FMUC'
  YCOMMENT='X_Y_'//YRECFM//' (kg/ms)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGU%XAVG_FMUC(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='FMVC'
  YCOMMENT='X_Y_'//YRECFM//' (kg/ms)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGU%XAVG_FMVC(:),IRESP,HCOMMENT=YCOMMENT)
  !
END IF
!
!
!*       4.     Transfer coefficients
!               ---------------------
!
IF (DGU%LCOEF) THEN
  !
  YRECFM='CD'
  YCOMMENT='X_Y_'//YRECFM
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGU%XAVG_CD(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='CH'
  YCOMMENT='X_Y_'//YRECFM
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGU%XAVG_CH(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='CE'
  YCOMMENT='X_Y_'//YRECFM
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGU%XAVG_CE(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='Z0'
  YCOMMENT='X_Y_'//YRECFM
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGU%XAVG_Z0(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='Z0H'
  YCOMMENT='X_Y_'//YRECFM
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGU%XAVG_Z0H(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='UREF'
  YCOMMENT='X_Y_'//YRECFM
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGU%XDIAG_UREF(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='ZREF'
  YCOMMENT='X_Y_'//YRECFM
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGU%XDIAG_ZREF(:),IRESP,HCOMMENT=YCOMMENT)
  !
END IF
!
!
!*       5.     Surface humidity
!               ----------------
!
IF (DGU%LSURF_VARS) THEN
!
YRECFM='QS'
YCOMMENT='X_Y_'//YRECFM//' (kg/kg)'
!
 CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGU%XAVG_QS(:),IRESP,HCOMMENT=YCOMMENT)
!
ENDIF
!
!-------------------------------------------------------------------------------
!
!         End of IO
!
 CALL END_IO_SURF_n(HPROGRAM)
IF (LHOOK) CALL DR_HOOK('WRITE_DIAG_SEB_SURF_ATM_N',1,ZHOOK_HANDLE)
!
!
END SUBROUTINE WRITE_DIAG_SEB_SURF_ATM_n
