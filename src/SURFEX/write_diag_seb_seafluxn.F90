!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE WRITE_DIAG_SEB_SEAFLUX_n (DTCO, DGU, U, CHS, DGS, S, &
                                           HPROGRAM)
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
!!      B. Decharme 02/2016 : NBLOCK instead of LCOUNTW for compilation in AAA
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_DIAG_SURF_ATM_n, ONLY : DIAG_SURF_ATM_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_CH_SEAFLUX_n, ONLY : CH_SEAFLUX_t
USE MODD_DIAG_SEAFLUX_n, ONLY : DIAG_SEAFLUX_t
USE MODD_SEAFLUX_n, ONLY : SEAFLUX_t
!
!
#ifdef SFX_ARO
USE MODD_IO_SURF_ARO,   ONLY : NBLOCK
#endif
!
!
USE MODD_SURF_PAR,      ONLY : XUNDEF
!
!
!
!                               
!
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
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(DIAG_SURF_ATM_t), INTENT(INOUT) :: DGU
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(CH_SEAFLUX_t), INTENT(INOUT) :: CHS
TYPE(DIAG_SEAFLUX_t), INTENT(INOUT) :: DGS
TYPE(SEAFLUX_t), INTENT(INOUT) :: S
!
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! program calling
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER           :: IRESP          ! IRESP  : return-code if a problem appears
 CHARACTER(LEN=12) :: YRECFM         ! Name of the article to be read
 CHARACTER(LEN=100):: YCOMMENT       ! Comment string
 CHARACTER(LEN=2)  :: YNUM
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
!
 CALL INIT_IO_SURF_n(DTCO, DGU, U, &
                    HPROGRAM,'SEA   ','SEAFLX','WRITE')
!
!
!*       1.     Surface temperature :
!               ---------------------
!
GMISC=(DGS%N2M>=1.OR.DGS%LSURF_BUDGET.OR.DGS%LSURF_BUDGETC)
!
IF (GMISC.AND.S%LHANDLE_SIC) THEN
    !
    YRECFM='TS_SEA'
    YCOMMENT='X_Y_'//YRECFM//' (K)'
    !
    CALL WRITE_SURF(DGU, U, &
                    HPROGRAM,YRECFM,DGS%XTS(:),IRESP,HCOMMENT=YCOMMENT)
    !
    YRECFM='TSRAD_SEA'
    YCOMMENT='X_Y_'//YRECFM//' (K)'
    !
    CALL WRITE_SURF(DGU, U, &
                    HPROGRAM,YRECFM,DGS%XTSRAD(:),IRESP,HCOMMENT=YCOMMENT)
    !
ENDIF
!
!*       2.     Richardson number :
!               -----------------
!
IF (DGS%N2M>=1) THEN
   !
   YRECFM='RI_SEA'
   YCOMMENT='X_Y_'//YRECFM
   !
   CALL WRITE_SURF(DGU, U, &
                    HPROGRAM,YRECFM,DGS%XRI(:),IRESP,HCOMMENT=YCOMMENT)
   !
ENDIF
 !
 !*       3.     Energy fluxes :
 !               -------------
 !
IF (DGS%LSURF_BUDGET) THEN
   !
   YRECFM='RN_SEA'
   YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
   !
   CALL WRITE_SURF(DGU, U, &
                    HPROGRAM,YRECFM,DGS%XRN(:),IRESP,HCOMMENT=YCOMMENT)
   !
   YRECFM='H_SEA'
   YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
   !
   CALL WRITE_SURF(DGU, U, &
                    HPROGRAM,YRECFM,DGS%XH(:),IRESP,HCOMMENT=YCOMMENT)
   !
   YRECFM='LE_SEA'
   YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
   !
   CALL WRITE_SURF(DGU, U, &
                    HPROGRAM,YRECFM,DGS%XLE(:),IRESP,HCOMMENT=YCOMMENT)
   !
   YRECFM='LEI_SEA'
   YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
   CALL WRITE_SURF(DGU, U, &
                    HPROGRAM,YRECFM,DGS%XLE_ICE(:),IRESP,HCOMMENT=YCOMMENT) 
   !
   YRECFM='GFLUX_SEA'
   YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
   !
   CALL WRITE_SURF(DGU, U, &
                    HPROGRAM,YRECFM,DGS%XGFLUX(:),IRESP,HCOMMENT=YCOMMENT)
   !
   YRECFM='EVAP_SEA'
   YCOMMENT='X_Y_'//YRECFM//' (kg/m2/s)'
   !
   CALL WRITE_SURF(DGU, U, &
                    HPROGRAM,YRECFM,DGS%XEVAP(:),IRESP,HCOMMENT=YCOMMENT)
   !
   YRECFM='SUBL_SEA'
   YCOMMENT='X_Y_'//YRECFM//' (kg/m2/s)'
   !
   CALL WRITE_SURF(DGU, U, &
                    HPROGRAM,YRECFM,DGS%XSUBL(:),IRESP,HCOMMENT=YCOMMENT)
   !
   IF (DGS%LRAD_BUDGET) THEN
      !
      YRECFM='SWD_SEA'
      YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
      !
      CALL WRITE_SURF(DGU, U, &
                    HPROGRAM,YRECFM,DGS%XSWD(:),IRESP,HCOMMENT=YCOMMENT)
      !
      YRECFM='SWU_SEA'
      YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
      !
      CALL WRITE_SURF(DGU, U, &
                    HPROGRAM,YRECFM,DGS%XSWU(:),IRESP,HCOMMENT=YCOMMENT)
      !
      YRECFM='LWD_SEA'
      YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
      !
      CALL WRITE_SURF(DGU, U, &
                    HPROGRAM,YRECFM,DGS%XLWD(:),IRESP,HCOMMENT=YCOMMENT)
      !
      YRECFM='LWU_SEA'
      YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
      !
      CALL WRITE_SURF(DGU, U, &
                    HPROGRAM,YRECFM,DGS%XLWU(:),IRESP,HCOMMENT=YCOMMENT)
      !
      DO JSW=1, SIZE(DGS%XSWBD,2)
         YNUM=ACHAR(48+JSW)
         !
         YRECFM='SWD_SEA_'//YNUM
         YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
         !
         CALL WRITE_SURF(DGU, U, &
                    HPROGRAM,YRECFM,DGS%XSWBD(:,JSW),IRESP,HCOMMENT=YCOMMENT)
         !
         YRECFM='SWU_SEA_'//YNUM
         YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
         !
         CALL WRITE_SURF(DGU, U, &
                    HPROGRAM,YRECFM,DGS%XSWBU(:,JSW),IRESP,HCOMMENT=YCOMMENT)
         !
      ENDDO
      !
   ENDIF
   !
   YRECFM='FMU_SEA'
   YCOMMENT='X_Y_'//YRECFM//' (kg/ms2)'
   !
   CALL WRITE_SURF(DGU, U, &
                    HPROGRAM,YRECFM,DGS%XFMU(:),IRESP,HCOMMENT=YCOMMENT)
   !
   YRECFM='FMV_SEA'
   YCOMMENT='X_Y_'//YRECFM//' (kg/ms2)'
   !
   CALL WRITE_SURF(DGU, U, &
                    HPROGRAM,YRECFM,DGS%XFMV(:),IRESP,HCOMMENT=YCOMMENT)
   !
END IF
!
IF (DGS%LSURF_BUDGETC) THEN
   !
   YRECFM='RNC_SEA'
   YCOMMENT='X_Y_'//YRECFM//' (J/m2)'
   !
   CALL WRITE_SURF(DGU, U, &
                    HPROGRAM,YRECFM,DGS%XRNC(:),IRESP,HCOMMENT=YCOMMENT)
   !
   YRECFM='HC_SEA'
   YCOMMENT='X_Y_'//YRECFM//' (J/m2)'
   !
   CALL WRITE_SURF(DGU, U, &
                    HPROGRAM,YRECFM,DGS%XHC(:),IRESP,HCOMMENT=YCOMMENT)
   !
   YRECFM='LEC_SEA'
   YCOMMENT='X_Y_'//YRECFM//' (J/m2)'
   !
   CALL WRITE_SURF(DGU, U, &
                    HPROGRAM,YRECFM,DGS%XLEC(:),IRESP,HCOMMENT=YCOMMENT)
   !
   YRECFM='LEIC_SEA'
   YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
   CALL WRITE_SURF(DGU, U, &
                    HPROGRAM,YRECFM,DGS%XLEC_ICE(:),IRESP,HCOMMENT=YCOMMENT) 
   !
   YRECFM='GFLUXC_SEA'
   YCOMMENT='X_Y_'//YRECFM//' (J/m2)'
   !
   CALL WRITE_SURF(DGU, U, &
                    HPROGRAM,YRECFM,DGS%XGFLUXC(:),IRESP,HCOMMENT=YCOMMENT)
   !
   YRECFM='EVAPC_SEA'
   YCOMMENT='X_Y_'//YRECFM//' (kg/m2)'
   !
   CALL WRITE_SURF(DGU, U, &
                    HPROGRAM,YRECFM,DGS%XEVAPC(:),IRESP,HCOMMENT=YCOMMENT)
   !
   YRECFM='SUBLC_SEA'
   YCOMMENT='X_Y_'//YRECFM//' (kg/m2)'
   !
   CALL WRITE_SURF(DGU, U, &
                    HPROGRAM,YRECFM,DGS%XSUBLC(:),IRESP,HCOMMENT=YCOMMENT)
   !
   IF (DGS%LRAD_BUDGET .OR. (DGS%LSURF_BUDGETC .AND. .NOT.DGU%LRESET_BUDGETC)) THEN
      !
      YRECFM='SWDC_SEA'
      YCOMMENT='X_Y_'//YRECFM//' (J/m2)'
      !
      CALL WRITE_SURF(DGU, U, &
                    HPROGRAM,YRECFM,DGS%XSWDC(:),IRESP,HCOMMENT=YCOMMENT)
      !
      YRECFM='SWUC_SEA'
      YCOMMENT='X_Y_'//YRECFM//' (J/m2)'
      !
      CALL WRITE_SURF(DGU, U, &
                    HPROGRAM,YRECFM,DGS%XSWUC(:),IRESP,HCOMMENT=YCOMMENT)
      !
      YRECFM='LWDC_SEA'
      YCOMMENT='X_Y_'//YRECFM//' (J/m2)'
      !
      CALL WRITE_SURF(DGU, U, &
                    HPROGRAM,YRECFM,DGS%XLWDC(:),IRESP,HCOMMENT=YCOMMENT)
      !
      YRECFM='LWUC_SEA'
      YCOMMENT='X_Y_'//YRECFM//' (J/m2)'
      !
      CALL WRITE_SURF(DGU, U, &
                    HPROGRAM,YRECFM,DGS%XLWUC(:),IRESP,HCOMMENT=YCOMMENT)
      !
   ENDIF
   !
   YRECFM='FMUC_SEA'
   YCOMMENT='X_Y_'//YRECFM//' (kg/ms)'
   !
   CALL WRITE_SURF(DGU, U, &
                    HPROGRAM,YRECFM,DGS%XFMUC(:),IRESP,HCOMMENT=YCOMMENT)
   !
   YRECFM='FMVC_SEA'
   YCOMMENT='X_Y_'//YRECFM//' (kg/ms)'
   !
   CALL WRITE_SURF(DGU, U, &
                    HPROGRAM,YRECFM,DGS%XFMVC(:),IRESP,HCOMMENT=YCOMMENT)
   !
END IF
!
IF (DGS%LSURF_BUDGET.OR.DGS%LSURF_BUDGETC) THEN
!
  YRECFM='TALB_SEA'
  YCOMMENT='total albedo over tile sea (-)'
  CALL WRITE_SURF(DGU, U, &
                    HPROGRAM,YRECFM,DGS%XALBT(:),IRESP,HCOMMENT=YCOMMENT)
!        
ENDIF
!
!*       4.     transfer coefficients
!               ---------------------
!
IF (DGS%LCOEF) THEN
   !
   YRECFM='CD_SEA'
   YCOMMENT='X_Y_'//YRECFM//' (W/s2)'
   !
   CALL WRITE_SURF(DGU, U, &
                    HPROGRAM,YRECFM,DGS%XCD(:),IRESP,HCOMMENT=YCOMMENT)
   !
   YRECFM='CH_SEA'
   YCOMMENT='X_Y_'//YRECFM//' (W/s)'
   !
   CALL WRITE_SURF(DGU, U, &
                    HPROGRAM,YRECFM,DGS%XCH(:),IRESP,HCOMMENT=YCOMMENT)
   !
   YRECFM='CE_SEA'
   YCOMMENT='X_Y_'//YRECFM//' (W/s/K)'
   !
   CALL WRITE_SURF(DGU, U, &
                    HPROGRAM,YRECFM,DGS%XCE(:),IRESP,HCOMMENT=YCOMMENT)
   !
   YRECFM='Z0_SEA'
   YCOMMENT='X_Y_'//YRECFM//' (M)'
   !
   CALL WRITE_SURF(DGU, U, &
                    HPROGRAM,YRECFM,DGS%XZ0(:),IRESP,HCOMMENT=YCOMMENT)
   !
   YRECFM='Z0H_SEA'
   YCOMMENT='X_Y_'//YRECFM//' (M)'
   !
   CALL WRITE_SURF(DGU, U, &
                    HPROGRAM,YRECFM,DGS%XZ0H(:),IRESP,HCOMMENT=YCOMMENT)
   !
END IF
!
!
!*       5.     Surface humidity
!               ----------------
!
IF (DGS%LSURF_VARS) THEN
   !
   YRECFM='QS_SEA'
   YCOMMENT='X_Y_'//YRECFM//' (KG/KG)'
   !
   CALL WRITE_SURF(DGU, U, &
                    HPROGRAM,YRECFM,DGS%XQS(:),IRESP,HCOMMENT=YCOMMENT)
   !
ENDIF
!
!
!*       6.     parameters at 2 and 10 meters :
!               -----------------------------
!
IF (DGS%N2M>=1) THEN
   !
   YRECFM='T2M_SEA'
   YCOMMENT='X_Y_'//YRECFM//' (K)'
   !
   CALL WRITE_SURF(DGU, U, &
                    HPROGRAM,YRECFM,DGS%XT2M(:),IRESP,HCOMMENT=YCOMMENT)
   !
   YRECFM='T2MMIN_SEA'
   YCOMMENT='X_Y_'//YRECFM//' (K)'
   !
   CALL WRITE_SURF(DGU, U, &
                    HPROGRAM,YRECFM,DGS%XT2M_MIN(:),IRESP,HCOMMENT=YCOMMENT)
   IF(GRESET)DGS%XT2M_MIN(:)=XUNDEF
   !
   YRECFM='T2MMAX_SEA'
   YCOMMENT='X_Y_'//YRECFM//' (K)'
   !
   CALL WRITE_SURF(DGU, U, &
                    HPROGRAM,YRECFM,DGS%XT2M_MAX(:),IRESP,HCOMMENT=YCOMMENT)
   IF(GRESET)DGS%XT2M_MAX(:)=0.0
   !
   YRECFM='Q2M_SEA'
   YCOMMENT='X_Y_'//YRECFM//' (KG/KG)'
   !
   CALL WRITE_SURF(DGU, U, &
                    HPROGRAM,YRECFM,DGS%XQ2M(:),IRESP,HCOMMENT=YCOMMENT)
   !
   YRECFM='HU2M_SEA'
   YCOMMENT='X_Y_'//YRECFM//' (-)'
   !
   CALL WRITE_SURF(DGU, U, &
                    HPROGRAM,YRECFM,DGS%XHU2M(:),IRESP,HCOMMENT=YCOMMENT)
   !
   YRECFM='HU2MMIN_SEA'
   YCOMMENT='X_Y_'//YRECFM//' (-)'
   !
   CALL WRITE_SURF(DGU, U, &
                    HPROGRAM,YRECFM,DGS%XHU2M_MIN(:),IRESP,HCOMMENT=YCOMMENT)
   IF(GRESET)DGS%XHU2M_MIN(:)=XUNDEF
   !
   YRECFM='HU2MMAX_SEA'
   YCOMMENT='X_Y_'//YRECFM//' (-)'
   !
   CALL WRITE_SURF(DGU, U, &
                    HPROGRAM,YRECFM,DGS%XHU2M_MAX(:),IRESP,HCOMMENT=YCOMMENT)
   IF(GRESET)DGS%XHU2M_MAX(:)=-XUNDEF
   !
   YRECFM='ZON10M_SEA'
   YCOMMENT='X_Y_'//YRECFM//' (M/S)'
   !
   CALL WRITE_SURF(DGU, U, &
                    HPROGRAM,YRECFM,DGS%XZON10M(:),IRESP,HCOMMENT=YCOMMENT)
   !
   YRECFM='MER10M_SEA'
   YCOMMENT='X_Y_'//YRECFM//' (M/S)'
   !
   CALL WRITE_SURF(DGU, U, &
                    HPROGRAM,YRECFM,DGS%XMER10M(:),IRESP,HCOMMENT=YCOMMENT)
   !
   YRECFM='W10M_SEA'
   YCOMMENT='X_Y_'//YRECFM//' (M/S)'
   !
   CALL WRITE_SURF(DGU, U, &
                    HPROGRAM,YRECFM,DGS%XWIND10M(:),IRESP,HCOMMENT=YCOMMENT)
   !
   YRECFM='W10MMAX_SEA'
   YCOMMENT='X_Y_'//YRECFM//' (M/S)'
   !
   CALL WRITE_SURF(DGU, U, &
                    HPROGRAM,YRECFM,DGS%XWIND10M_MAX(:),IRESP,HCOMMENT=YCOMMENT)
   IF(GRESET)DGS%XWIND10M_MAX(:)=0.0
   !
END IF
!
!
!*       7.     chemical diagnostics:
!               --------------------
!
IF (CHS%SVS%NBEQ>0 .AND. CHS%CCH_DRY_DEP=="WES89 ") THEN
   DO JSV = 1,SIZE(CHS%CCH_NAMES,1)
      YRECFM='DV_SEA_'//TRIM(CHS%CCH_NAMES(JSV))
      WRITE(YCOMMENT,'(A13,I3.3)')'(m/s) DV_SEA_',JSV
      CALL WRITE_SURF(DGU, U, &
                    HPROGRAM,YRECFM,CHS%XDEP(:,JSV),IRESP,HCOMMENT=YCOMMENT)
   END DO
ENDIF
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
