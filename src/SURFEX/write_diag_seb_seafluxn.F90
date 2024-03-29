!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE WRITE_DIAG_SEB_SEAFLUX_n (DTCO, DUO, U, CHS, DSO, D, DC, &
                                           OHANDLE_SIC,HPROGRAM)
!     #################################
!
!!****  *WRITE_DIAG_SEB_SEAFLUX_n* - write the SEAFLUX diagnostic fields
!!
!!    PURPOSE
!!    -------
!!
!!
!!**  METHOD
!!    ------
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
!!      B. Decharme 06/2013 : Add evap and sublimation diag
!!                            Delete LPROVAR_TO_DIAG here
!!      S.Senesi    01/2014 : add diags on seaice 
!!      S. Belamari 06/2014 : Introduce GRESET to avoid errors due to NBLOCK=0
!!                            when coupled with ARPEGE/ALADIN/AROME
!!      S. Senesi    08/15   Add 2nd dimension name for SW bands to write_surf calls
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_SURF_ATM_n,   ONLY : SURF_ATM_t
USE MODD_CH_SEAFLUX_n, ONLY : CH_SEAFLUX_t
USE MODD_DIAG_n,       ONLY : DIAG_t, DIAG_OPTIONS_t
!
#ifdef SFX_ARO
USE MODD_IO_SURF_ARO,   ONLY : NBLOCK
#endif
!
#ifdef SFX_OL
USE MODD_IO_SURF_OL, ONLY : LRESET_DIAG_ol=>LRESET_DIAG
#endif
!
#ifdef SFX_NC
USE MODD_IO_SURF_NC, ONLY : LRESET_DIAG_nc=>LRESET_DIAG
#endif
!
USE MODD_XIOS, ONLY : LALLOW_ADD_DIM, YSWBAND_DIM_NAME
!
USE MODD_SURF_PAR,      ONLY : XUNDEF, LEN_HREC
!
USE MODI_WRITE_DIAG_2M_10M
USE MODI_INIT_IO_SURF_n
USE MODI_WRITE_SURF
USE MODI_END_IO_SURF_n
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
TYPE(DATA_COVER_t),   INTENT(INOUT) :: DTCO
TYPE(DIAG_OPTIONS_t), INTENT(INOUT) :: DUO
TYPE(SURF_ATM_t),     INTENT(INOUT) :: U
TYPE(CH_SEAFLUX_t),   INTENT(INOUT) :: CHS
TYPE(DIAG_OPTIONS_t), INTENT(INOUT) :: DSO
TYPE(DIAG_t),         INTENT(INOUT) :: D
TYPE(DIAG_t),         INTENT(INOUT) :: DC
!
LOGICAL, INTENT(IN) :: OHANDLE_SIC
!
CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! program calling
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER                 :: IRESP          ! IRESP  : return-code if a problem appears
CHARACTER(LEN=LEN_HREC) :: YRECFM         ! Name of the article to be read
CHARACTER(LEN=100)      :: YCOMMENT       ! Comment string
CHARACTER(LEN=2)        :: YNUM
!
LOGICAL           :: GRESET
INTEGER           :: JSV, JSW
LOGICAL           :: GMISC
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('WRITE_DIAG_SEB_SEAFLUX_N',0,ZHOOK_HANDLE)
!
!         Initialisation for IO
!
GRESET=.TRUE.
#ifdef SFX_ARO
GRESET=(NBLOCK>0)
#endif
#ifdef SFX_OL
IF (.NOT. LRESET_DIAG_ol) GRESET = .FALSE.
#endif
#ifdef SFX_NC
IF (.NOT. LRESET_DIAG_nc) GRESET = .FALSE.
#endif
!
CALL INIT_IO_SURF_n(DTCO, U,HPROGRAM,'SEA   ','SEAFLX','WRITE','SEAFLUX_DIAGNOSTICS.OUT.nc')
!
!
!*       1.     Surface temperature :
!               ---------------------
!
GMISC=(DSO%N2M>=1.OR.DSO%LSURF_BUDGET.OR.DSO%LSURF_BUDGETC)
!
IF (GMISC.AND.OHANDLE_SIC) THEN
    !
    YRECFM='TS_SEA'
    YCOMMENT='X_Y_'//YRECFM//' (K)'
    !
    CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,D%XTS(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='TSRAD_SEA'
    YCOMMENT='X_Y_'//YRECFM//' (K)'
    !
    CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,D%XTSRAD(:),IRESP,HCOMMENT=YCOMMENT)
    !
ENDIF
!
!*       2.     Richardson number :
!               -----------------
!
IF (DSO%N2M>=1) THEN
   !
   YRECFM='RI_SEA'
   YCOMMENT='X_Y_'//YRECFM
   !
   CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,D%XRI(:),IRESP,HCOMMENT=YCOMMENT)
   !
ENDIF
 !
 !*       3.     Energy fluxes :
 !               -------------
 !
IF (DSO%LSURF_BUDGET) THEN
   !
   YRECFM='RN_SEA'
   YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
   !
   CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,D%XRN(:),IRESP,HCOMMENT=YCOMMENT)
   !
   YRECFM='H_SEA'
   YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
   !
   CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,D%XH(:),IRESP,HCOMMENT=YCOMMENT)
   !
   YRECFM='LE_SEA'
   YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
   !
   CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,D%XLE(:),IRESP,HCOMMENT=YCOMMENT)
   !
   YRECFM='LEI_SEA'
   YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
   CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,D%XLEI(:),IRESP,HCOMMENT=YCOMMENT) 
   !
   YRECFM='GFLUX_SEA'
   YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
   !
   CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,D%XGFLUX(:),IRESP,HCOMMENT=YCOMMENT)
   !
   YRECFM='EVAP_SEA'
   YCOMMENT='X_Y_'//YRECFM//' (kg/m2/s)'
   !
   CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,D%XEVAP(:),IRESP,HCOMMENT=YCOMMENT)
   !
   YRECFM='SUBL_SEA'
   YCOMMENT='X_Y_'//YRECFM//' (kg/m2/s)'
   !
   CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,D%XSUBL(:),IRESP,HCOMMENT=YCOMMENT)
   !
   IF (DSO%LRAD_BUDGET) THEN
      !
      YRECFM='SWD_SEA'
      YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
      !
      CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,D%XSWD(:),IRESP,HCOMMENT=YCOMMENT)
      !
      YRECFM='SWU_SEA'
      YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
      !
      CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,D%XSWU(:),IRESP,HCOMMENT=YCOMMENT)
      !
      YRECFM='LWD_SEA'
      YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
      !
      CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,D%XLWD(:),IRESP,HCOMMENT=YCOMMENT)
      !
      YRECFM='LWU_SEA'
      YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
      !
      CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,D%XLWU(:),IRESP,HCOMMENT=YCOMMENT)
      !
      IF (LALLOW_ADD_DIM) THEN
        !
        YRECFM='SWD_SEA_'
        YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
        CALL WRITE_SURF(DUO%CSELECT,&
             HPROGRAM,YRECFM,D%XSWBD(:,:),IRESP,HCOMMENT=YCOMMENT, HNAM_DIM=YSWBAND_DIM_NAME)
        !
        YRECFM='SWU_SEA_'
        YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
        CALL WRITE_SURF(DUO%CSELECT,&
             HPROGRAM,YRECFM,D%XSWBU(:,:),IRESP,HCOMMENT=YCOMMENT, HNAM_DIM=YSWBAND_DIM_NAME)        
        !
      ELSE
        !
        DO JSW=1, SIZE(D%XSWBD,2)
          YNUM=ACHAR(48+JSW)
          !
          YRECFM='SWD_SEA_'//YNUM
          YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
          !
          CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,D%XSWBD(:,JSW),IRESP,HCOMMENT=YCOMMENT)
          !
          YRECFM='SWU_SEA_'//YNUM
          YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
          !
          CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,D%XSWBU(:,JSW),IRESP,HCOMMENT=YCOMMENT)
          !
        ENDDO
        !
      ENDIF
      !
   ENDIF
   !
   YRECFM='FMU_SEA'
   YCOMMENT='X_Y_'//YRECFM//' (kg/ms2)'
   !
   CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,D%XFMU(:),IRESP,HCOMMENT=YCOMMENT)
   !
   YRECFM='FMV_SEA'
   YCOMMENT='X_Y_'//YRECFM//' (kg/ms2)'
   !
   CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,D%XFMV(:),IRESP,HCOMMENT=YCOMMENT)
   !
END IF
!
IF (DSO%LSURF_BUDGET.OR.DSO%LSURF_BUDGETC) THEN
!
  YRECFM='TALB_SEA'
  YCOMMENT='total albedo over tile sea (-)'
  CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,D%XALBT(:),IRESP,HCOMMENT=YCOMMENT)
!        
ENDIF
!
!*       4.     transfer coefficients
!               ---------------------
!
IF (DSO%LCOEF) THEN
   !
   YRECFM='CD_SEA'
   YCOMMENT='X_Y_'//YRECFM//' (W/s2)'
   !
   CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,D%XCD(:),IRESP,HCOMMENT=YCOMMENT)
   !
   YRECFM='CH_SEA'
   YCOMMENT='X_Y_'//YRECFM//' (W/s)'
   !
   CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,D%XCH(:),IRESP,HCOMMENT=YCOMMENT)
   !
   YRECFM='CE_SEA'
   YCOMMENT='X_Y_'//YRECFM//' (W/s/K)'
   !
   CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,D%XCE(:),IRESP,HCOMMENT=YCOMMENT)
   !
   YRECFM='Z0_SEA'
   YCOMMENT='X_Y_'//YRECFM//' (M)'
   !
   CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,D%XZ0(:),IRESP,HCOMMENT=YCOMMENT)
   !
   YRECFM='Z0H_SEA'
   YCOMMENT='X_Y_'//YRECFM//' (M)'
   !
   CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,D%XZ0H(:),IRESP,HCOMMENT=YCOMMENT)
   !
END IF
!
!
!*       5.     Surface humidity
!               ----------------
!
IF (DSO%LSURF_VARS) THEN
   !
   YRECFM='QS_SEA'
   YCOMMENT='X_Y_'//YRECFM//' (KG/KG)'
   !
   CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,D%XQS(:),IRESP,HCOMMENT=YCOMMENT)
   !
ENDIF
!
!
!*       6.     parameters at 2 and 10 meters :
!               -----------------------------
!
IF (DSO%N2M>=1) THEN
   !
   CALL WRITE_DIAG_2M_10M(DUO, DSO, D, '_SEA', HPROGRAM)
   !
END IF
!
!
!*       7.     chemical diagnostics:
!               --------------------
!
IF (CHS%SVS%NBEQ>0 .AND. CHS%CCH_DRY_DEP=="WES89 ") THEN
   DO JSV = 1,SIZE(CHS%CCH_NAMES,1)
      YRECFM='DVSE'//TRIM(CHS%CCH_NAMES(JSV))
      WRITE(YCOMMENT,'(A13,I3.3)')'(m/s) DV_SEA_',JSV
      CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,CHS%XDEP(:,JSV),IRESP,HCOMMENT=YCOMMENT)
   END DO
ENDIF
!
IF (DSO%LSURF_BUDGETC) THEN
   ! 
   CALL END_IO_SURF_n(HPROGRAM)
   CALL INIT_IO_SURF_n(DTCO, U,HPROGRAM,'SEA   ','SEAFLX','WRITE','SEAFLUX_DIAG_CUMUL.OUT.nc')
   !
   YRECFM='RNC_SEA'
   YCOMMENT='X_Y_'//YRECFM//' (J/m2)'
   !
   CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,DC%XRN(:),IRESP,HCOMMENT=YCOMMENT)
   !
   YRECFM='HC_SEA'
   YCOMMENT='X_Y_'//YRECFM//' (J/m2)'
   !
   CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,DC%XH(:),IRESP,HCOMMENT=YCOMMENT)
   !
   YRECFM='LEC_SEA'
   YCOMMENT='X_Y_'//YRECFM//' (J/m2)'
   !
   CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,DC%XLE(:),IRESP,HCOMMENT=YCOMMENT)
   !
   YRECFM='LEIC_SEA'
   YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
   CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,DC%XLEI(:),IRESP,HCOMMENT=YCOMMENT) 
   !
   YRECFM='GFLUXC_SEA'
   YCOMMENT='X_Y_'//YRECFM//' (J/m2)'
   !
   CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,DC%XGFLUX(:),IRESP,HCOMMENT=YCOMMENT)
   !
   YRECFM='EVAPC_SEA'
   YCOMMENT='X_Y_'//YRECFM//' (kg/m2)'
   !
   CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,DC%XEVAP(:),IRESP,HCOMMENT=YCOMMENT)
   !
   YRECFM='SUBLC_SEA'
   YCOMMENT='X_Y_'//YRECFM//' (kg/m2)'
   !
   CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,DC%XSUBL(:),IRESP,HCOMMENT=YCOMMENT)
   !
   IF (DSO%LRAD_BUDGET .OR. (DSO%LSURF_BUDGETC .AND. .NOT.DUO%LRESET_BUDGETC)) THEN
      !
      YRECFM='SWDC_SEA'
      YCOMMENT='X_Y_'//YRECFM//' (J/m2)'
      !
      CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,DC%XSWD(:),IRESP,HCOMMENT=YCOMMENT)
      !
      YRECFM='SWUC_SEA'
      YCOMMENT='X_Y_'//YRECFM//' (J/m2)'
      !
      CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,DC%XSWU(:),IRESP,HCOMMENT=YCOMMENT)
      !
      YRECFM='LWDC_SEA'
      YCOMMENT='X_Y_'//YRECFM//' (J/m2)'
      !
      CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,DC%XLWD(:),IRESP,HCOMMENT=YCOMMENT)
      !
      YRECFM='LWUC_SEA'
      YCOMMENT='X_Y_'//YRECFM//' (J/m2)'
      !
      CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,DC%XLWU(:),IRESP,HCOMMENT=YCOMMENT)
      !
   ENDIF
   !
   YRECFM='FMUC_SEA'
   YCOMMENT='X_Y_'//YRECFM//' (kg/ms)'
   !
   CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,DC%XFMU(:),IRESP,HCOMMENT=YCOMMENT)
   !
   YRECFM='FMVC_SEA'
   YCOMMENT='X_Y_'//YRECFM//' (kg/ms)'
   !
   CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,DC%XFMV(:),IRESP,HCOMMENT=YCOMMENT)
   !
END IF
!
!------------------------------------------------------------------------------
!
!         End of IO
!
 CALL END_IO_SURF_n(HPROGRAM)
!
IF (LHOOK) CALL DR_HOOK('WRITE_DIAG_SEB_SEAFLUX_N',1,ZHOOK_HANDLE)
!
!
END SUBROUTINE WRITE_DIAG_SEB_SEAFLUX_n
