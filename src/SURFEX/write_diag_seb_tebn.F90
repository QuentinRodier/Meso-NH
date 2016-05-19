!     #########
      SUBROUTINE WRITE_DIAG_SEB_TEB_n(HPROGRAM)
!     #################################
!
!!****  *WRITE_DIAG_SEB_TEB_n* - writes TEB diagnostics
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
!!	V. Masson   *Meteo France*	
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2004
!!      Modified    01/2006 : TEB flux parameterization.
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_DIAG_TEB_n,ONLY : N2M, LSURF_BUDGET, LRAD_BUDGET,          &
                             LCOEF, LSURF_VARS,                       &
                             XRN, XH, XLE, XGFLUX,                    &
                             XRI, XCD, XCH, XCE, XZ0, XZ0H,           &
                             XT2M, XQ2M, XHU2M,                       &
                             XZON10M, XMER10M, XSFCO2, XQS,           &
                             XSWD, XSWU, XSWBD, XSWBU,                &
                             XLWD, XLWU, XFMU, XFMV  
USE MODD_DIAG_UTCI_TEB_n, ONLY : LUTCI, XUTCI_IN, XUTCI_OUTSUN,       &
                                 XUTCI_OUTSHADE, XTRAD_SUN, XTRAD_SHADE
                           
USE MODD_CH_TEB_n,  ONLY : XDEP, CCH_DRY_DEP, CCH_NAMES, NBEQ 
!
USE MODI_INIT_IO_SURF_n
USE MODI_WRITE_SURF
USE MODI_END_IO_SURF_n
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
INTEGER           :: JSV, JSW
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!         Initialisation for IO
!
IF (LHOOK) CALL DR_HOOK('WRITE_DIAG_SEB_TEB_N',0,ZHOOK_HANDLE)
 CALL INIT_IO_SURF_n(HPROGRAM,'TOWN  ','TEB   ','WRITE')
!
!
!
!*       2.     Richardson number :
!               -----------------
!
IF (N2M>=1) THEN

YRECFM='RI_TEB'
YCOMMENT='X_Y_'//YRECFM
!
 CALL WRITE_SURF(HPROGRAM,YRECFM,XRI(:),IRESP,HCOMMENT=YCOMMENT)
!
END IF
!
!*       3.     Energy fluxes :
!               -------------
!
IF (LSURF_BUDGET) THEN

YRECFM='RN_TEB'
YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
!
 CALL WRITE_SURF(HPROGRAM,YRECFM,XRN(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='H_TEB'
YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
!
 CALL WRITE_SURF(HPROGRAM,YRECFM,XH(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='LE_TEB'
YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
!
 CALL WRITE_SURF(HPROGRAM,YRECFM,XLE(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='GFLUX_TEB'
YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
!
 CALL WRITE_SURF(HPROGRAM,YRECFM,XGFLUX(:),IRESP,HCOMMENT=YCOMMENT)
!
IF (LRAD_BUDGET) THEN
!        
   YRECFM='SWD_TEB'
   YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
   !
   CALL WRITE_SURF(HPROGRAM,YRECFM,XSWD(:),IRESP,HCOMMENT=YCOMMENT)
   !
   YRECFM='SWU_TEB'
   YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
   !
   CALL WRITE_SURF(HPROGRAM,YRECFM,XSWU(:),IRESP,HCOMMENT=YCOMMENT)
   !
   YRECFM='LWD_TEB'
   YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
   !
   CALL WRITE_SURF(HPROGRAM,YRECFM,XLWD(:),IRESP,HCOMMENT=YCOMMENT)
   !
   YRECFM='LWU_TEB'
   YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
   !
   CALL WRITE_SURF(HPROGRAM,YRECFM,XLWU(:),IRESP,HCOMMENT=YCOMMENT)
   !
   DO JSW=1, SIZE(XSWBD,2)
      YNUM=ACHAR(48+JSW)
      !
      YRECFM='SWD_TEB_'//YNUM
      YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
      !
      CALL WRITE_SURF(HPROGRAM,YRECFM,XSWBD(:,JSW),IRESP,HCOMMENT=YCOMMENT)
      !
      YRECFM='SWU_TEB_'//YNUM
      YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
      !
      CALL WRITE_SURF(HPROGRAM,YRECFM,XSWBU(:,JSW),IRESP,HCOMMENT=YCOMMENT)
      !
   ENDDO
!
ENDIF
!
YRECFM='FMU_TEB'
YCOMMENT='X_Y_'//YRECFM//' (kg/ms2)'
!
 CALL WRITE_SURF(HPROGRAM,YRECFM,XFMU(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='FMV_TEB'
YCOMMENT='X_Y_'//YRECFM//' (kg/ms2)'
!
 CALL WRITE_SURF(HPROGRAM,YRECFM,XFMV(:),IRESP,HCOMMENT=YCOMMENT)
!
END IF
!
!
!
!*       4.     Transfer coefficients
!               ---------------------
!
IF (LCOEF) THEN

YRECFM='CD_TEB'
YCOMMENT='X_Y_'//YRECFM
!
 CALL WRITE_SURF(HPROGRAM,YRECFM,XCD(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='CH_TEB'
YCOMMENT='X_Y_'//YRECFM
!
 CALL WRITE_SURF(HPROGRAM,YRECFM,XCH(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='CE_TEB'
YCOMMENT='X_Y_'//YRECFM
!
 CALL WRITE_SURF(HPROGRAM,YRECFM,XCE(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='Z0_TEB'
YCOMMENT='X_Y_'//YRECFM//' (M)'
!
 CALL WRITE_SURF(HPROGRAM,YRECFM,XZ0(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='Z0H_TEB'
YCOMMENT='X_Y_'//YRECFM//' (M)'
!
 CALL WRITE_SURF(HPROGRAM,YRECFM,XZ0H(:),IRESP,HCOMMENT=YCOMMENT)
!
ENDIF
!
!
!*       5.     Surface humidity
!               ----------------
!
IF (LSURF_VARS) THEN

YRECFM='QS_TEB'
YCOMMENT='X_Y_'//YRECFM//' (KG/KG)'
!
 CALL WRITE_SURF(HPROGRAM,YRECFM,XQS(:),IRESP,HCOMMENT=YCOMMENT)
!
ENDIF

!
!*       5.     parameters at 2 and 10 meters :
!               -----------------------------
!
IF (N2M>=1) THEN

YRECFM='T2M_TEB'
YCOMMENT='X_Y_'//YRECFM//' (K)'
!
 CALL WRITE_SURF(HPROGRAM,YRECFM,XT2M(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='Q2M_TEB'
YCOMMENT='X_Y_'//YRECFM//' (KG/KG)'
!
 CALL WRITE_SURF(HPROGRAM,YRECFM,XQ2M(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='HU2M_TEB'
YCOMMENT='X_Y_'//YRECFM//' (KG/KG)'
!
 CALL WRITE_SURF(HPROGRAM,YRECFM,XHU2M(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='ZON10M_TEB'
YCOMMENT='X_Y_'//YRECFM//' (M/S)'
!
 CALL WRITE_SURF(HPROGRAM,YRECFM,XZON10M(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='MER10M_TEB'
YCOMMENT='X_Y_'//YRECFM//' (M/S)'
!
 CALL WRITE_SURF(HPROGRAM,YRECFM,XMER10M(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='SFCO2_TEB'
YCOMMENT='X_Y_'//YRECFM//' (KG/M2/S)'
!
 CALL WRITE_SURF(HPROGRAM,YRECFM,XSFCO2(:),IRESP,HCOMMENT=YCOMMENT)
!
END IF
!
IF (LUTCI .AND. N2M >0) THEN
  YRECFM='UTCI_IN'
  YCOMMENT='UTCI for person indoor'//' (°C)'
  CALL WRITE_SURF(HPROGRAM,YRECFM,XUTCI_IN(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='UTCI_OUTSUN'
  YCOMMENT='UTCI for person at sun'//' (°C)'
  CALL WRITE_SURF(HPROGRAM,YRECFM,XUTCI_OUTSUN(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='UTCI_OUTSHAD'
  YCOMMENT='UTCI for person in shade'//' (°C)'
  CALL WRITE_SURF(HPROGRAM,YRECFM,XUTCI_OUTSHADE(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='TRAD_SUN'
  YCOMMENT='Mean radiant temperature seen by person at sun'//' (K)'
  CALL WRITE_SURF(HPROGRAM,YRECFM,XTRAD_SUN(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='TRAD_SHADE'
  YCOMMENT='Mean radiant temperature seen by person in shade'//' (K)'
  CALL WRITE_SURF(HPROGRAM,YRECFM,XTRAD_SHADE(:),IRESP,HCOMMENT=YCOMMENT)
END IF
!
!
!*       6.     chemical diagnostics:
!               --------------------
!
IF (NBEQ>0 .AND. CCH_DRY_DEP=="WES89 ") THEN
  DO JSV = 1,SIZE(CCH_NAMES,1)
    YRECFM='DV_TWN_'//TRIM(CCH_NAMES(JSV))
    WRITE(YCOMMENT,'(A13,I3.3)')'(m/s) DV_TWN_',JSV
    CALL WRITE_SURF(HPROGRAM,YRECFM,XDEP(:,JSV),IRESP,HCOMMENT=YCOMMENT)
  END DO
ENDIF
!-------------------------------------------------------------------------------
!
!         End of IO
!
 CALL END_IO_SURF_n(HPROGRAM)
IF (LHOOK) CALL DR_HOOK('WRITE_DIAG_SEB_TEB_N',1,ZHOOK_HANDLE)
!
!
END SUBROUTINE WRITE_DIAG_SEB_TEB_n
