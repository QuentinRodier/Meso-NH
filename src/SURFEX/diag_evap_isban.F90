!     #########
SUBROUTINE DIAG_EVAP_ISBA_n(HPHOTO,PTSTEP,KMASK,KSIZE,KPATCH,PRHOA)
!     ###############################################################################
!
!!****  *DIAG_EVAP-ISBA_n * - additional diagnostics for ISBA
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
!!     P. LeMoigne 
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2004
!!                     2008      New diag
!!      B. Decharme    2012      New snow diag LESL
!!                               Add carbon fluxes diag
!!                               Add isba water budget diag
!!------------------------------------------------------------------
!
USE MODD_ISBA_n,              ONLY : LGLACIER, CPHOTO, TSNOW
!
USE MODD_PACK_ISBA,           ONLY : XP_LE
USE MODD_PACK_DIAG_ISBA,      ONLY : XP_RN, XP_H, XP_GFLUX, XP_LEI,  &
                                       XP_LEG, XP_LEGI, XP_LEV,      &
                                       XP_LES, XP_LER, XP_LETR,      &
                                       XP_EVAP, XP_DRAIN, XP_RUNOFF, &
                                       XP_HORT, XP_MELT, XP_DRIP,    &
                                       XP_IFLOOD, XP_PFLOOD,         &
                                       XP_LE_FLOOD, XP_SWD, XP_SWU,  &
                                       XP_LWD, XP_LWU, XP_FMU,       &
                                       XP_FMV, XP_ICEFLUX, XP_LESL,  &
                                       XP_LEI_FLOOD, XP_RRVEG,       & 
                                       XP_IRRIG_FLUX, XP_GPP,        &
                                       XP_RESP_AUTO, XP_RESP_ECO,    &
                                       XP_DWG,XP_DWGI,XP_DWR,        &
                                       XP_DSWE,XP_WATBUD                                       

USE MODD_DIAG_EVAP_ISBA_n,    ONLY : LSURF_EVAP_BUDGET, LSURF_BUDGETC,    &
                                       LWATER_BUDGET,                     &
                                       XLEG, XLEGI, XLEV, XLES, XLESL,    &
                                       XLER, XLETR, XEVAP, XDRAIN,        &
                                       XRUNOFF, XHORT, XMELT, XDRIP,      &
                                       XRRVEG, XRNC, XHC, XLEC, XGFLUXC,  &
                                       XLEGC, XLEGIC, XLEVC, XLESC,       &
                                       XLESLC, XLERC, XLETRC, XEVAPC,     &
                                       XLEIC, XDRAINC, XRUNOFFC, XHORTC,  &
                                       XMELTC, XDRIPC, XRRVEGC,           &
                                       XIFLOOD, XIFLOODC,                 &
                                       XPFLOOD, XPFLOODC,                 &
                                       XLE_FLOOD, XLE_FLOODC,             &
                                       XLEI_FLOOD, XLEI_FLOODC, XICEFLUXC,&
                                       XIRRIG_FLUX, XIRRIG_FLUXC,         &
                                       XGPP, XRESP_AUTO, XRESP_ECO,       &
                                       XGPPC, XRESPC_AUTO, XRESPC_ECO,    &
                                       XDWG,XDWGI,XDWR,XDSWE,XWATBUD,     &
                                       XDWGC,XDWGIC,XDWRC,XDSWEC,XWATBUDC

!
USE MODD_DIAG_ISBA_n,         ONLY : XSWDC, XSWUC, XLWDC, XLWUC, XFMUC, XFMVC
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
 CHARACTER(LEN=*), INTENT(IN)      :: HPHOTO        ! type of photosynthesis
REAL,    INTENT(IN)               :: PTSTEP        ! time step
INTEGER, INTENT(IN)               :: KSIZE, KPATCH   
!
INTEGER, DIMENSION(:), INTENT(IN) :: KMASK
REAL,    DIMENSION(:), INTENT(IN) :: PRHOA         ! air density for unit change
!
INTEGER :: JJ
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*      0.2    declarations of local variables
!
!-------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('DIAG_EVAP_ISBA_N',0,ZHOOK_HANDLE)
!
IF (LSURF_EVAP_BUDGET) THEN
!cdir nodep
  DO JJ=1,KSIZE
     !
     XLEG       (KMASK(JJ), KPATCH)  =  XP_LEG        (JJ)
     XLEGI      (KMASK(JJ), KPATCH)  =  XP_LEGI       (JJ)
     XLEV       (KMASK(JJ), KPATCH)  =  XP_LEV        (JJ)
     XLES       (KMASK(JJ), KPATCH)  =  XP_LES        (JJ)
     XLER       (KMASK(JJ), KPATCH)  =  XP_LER        (JJ)
     XLETR      (KMASK(JJ), KPATCH)  =  XP_LETR       (JJ)
     XEVAP      (KMASK(JJ), KPATCH)  =  XP_EVAP       (JJ)
     XDRAIN     (KMASK(JJ), KPATCH)  =  XP_DRAIN      (JJ)
     XRUNOFF    (KMASK(JJ), KPATCH)  =  XP_RUNOFF     (JJ)
     XHORT      (KMASK(JJ), KPATCH)  =  XP_HORT       (JJ)
     XDRIP      (KMASK(JJ), KPATCH)  =  XP_DRIP       (JJ)
     XRRVEG     (KMASK(JJ), KPATCH)  =  XP_RRVEG      (JJ)
     XMELT      (KMASK(JJ), KPATCH)  =  XP_MELT       (JJ)
     XIFLOOD    (KMASK(JJ), KPATCH)  =  XP_IFLOOD     (JJ)
     XPFLOOD    (KMASK(JJ), KPATCH)  =  XP_PFLOOD     (JJ)
     XLE_FLOOD  (KMASK(JJ), KPATCH)  =  XP_LE_FLOOD   (JJ)
     XLEI_FLOOD (KMASK(JJ), KPATCH)  =  XP_LEI_FLOOD  (JJ)
     XIRRIG_FLUX(KMASK(JJ), KPATCH)  =  XP_IRRIG_FLUX (JJ)
     !
  END DO
  !
  IF (TSNOW%SCHEME=='3-L' .OR. TSNOW%SCHEME=='CRO') THEN
!cdir nodep
     DO JJ=1,KSIZE
        XLESL    (KMASK(JJ), KPATCH)  =  XP_LESL       (JJ)
     END DO
  END IF
  !
  IF(HPHOTO/='NON')THEN
!cdir nodep
     DO JJ=1,KSIZE
        ! Transform units from kgCO2/kgair m/s to kgCO2/m2/s
        XGPP       (KMASK(JJ), KPATCH)  =  XP_GPP       (JJ) * PRHOA(JJ)
        XRESP_AUTO (KMASK(JJ), KPATCH)  =  XP_RESP_AUTO (JJ) * PRHOA(JJ)
        XRESP_ECO  (KMASK(JJ), KPATCH)  =  XP_RESP_ECO  (JJ) * PRHOA(JJ)
        !
     END DO
  ELSE  
     XGPP      (:,:)=0.0
     XRESP_AUTO(:,:)=0.0
     XRESP_ECO (:,:)=0.0
  ENDIF
  !
  IF(LWATER_BUDGET)THEN
!cdir nodep
     DO JJ=1,KSIZE
        XDWG   (KMASK(JJ), KPATCH)  =  XP_DWG   (JJ)
        XDWGI  (KMASK(JJ), KPATCH)  =  XP_DWGI  (JJ)
        XDWR   (KMASK(JJ), KPATCH)  =  XP_DWR   (JJ)
        XDSWE  (KMASK(JJ), KPATCH)  =  XP_DSWE  (JJ)
        XWATBUD(KMASK(JJ), KPATCH)  =  XP_WATBUD(JJ)
     END DO
  ENDIF
  !
END IF
!
IF (LSURF_BUDGETC) THEN
!cdir nodep
  DO JJ=1,KSIZE
     !
     XRNC        (KMASK(JJ), KPATCH)  =  XRNC        (KMASK(JJ), KPATCH) + XP_RN        (JJ) * PTSTEP
     XHC         (KMASK(JJ), KPATCH)  =  XHC         (KMASK(JJ), KPATCH) + XP_H         (JJ) * PTSTEP
     XLEC        (KMASK(JJ), KPATCH)  =  XLEC        (KMASK(JJ), KPATCH) + XP_LE        (JJ) * PTSTEP
     XLEIC       (KMASK(JJ), KPATCH)  =  XLEIC       (KMASK(JJ), KPATCH) + XP_LEI       (JJ) * PTSTEP
     XGFLUXC     (KMASK(JJ), KPATCH)  =  XGFLUXC     (KMASK(JJ), KPATCH) + XP_GFLUX     (JJ) * PTSTEP
     XLEGC       (KMASK(JJ), KPATCH)  =  XLEGC       (KMASK(JJ), KPATCH) + XP_LEG       (JJ) * PTSTEP
     XLEGIC      (KMASK(JJ), KPATCH)  =  XLEGIC      (KMASK(JJ), KPATCH) + XP_LEGI      (JJ) * PTSTEP
     XLEVC       (KMASK(JJ), KPATCH)  =  XLEVC       (KMASK(JJ), KPATCH) + XP_LEV       (JJ) * PTSTEP
     XLESC       (KMASK(JJ), KPATCH)  =  XLESC       (KMASK(JJ), KPATCH) + XP_LES       (JJ) * PTSTEP
     XLERC       (KMASK(JJ), KPATCH)  =  XLERC       (KMASK(JJ), KPATCH) + XP_LER       (JJ) * PTSTEP
     XLETRC      (KMASK(JJ), KPATCH)  =  XLETRC      (KMASK(JJ), KPATCH) + XP_LETR      (JJ) * PTSTEP
     XEVAPC      (KMASK(JJ), KPATCH)  =  XEVAPC      (KMASK(JJ), KPATCH) + XP_EVAP      (JJ) * PTSTEP
     XDRAINC     (KMASK(JJ), KPATCH)  =  XDRAINC     (KMASK(JJ), KPATCH) + XP_DRAIN     (JJ) * PTSTEP
     XRUNOFFC    (KMASK(JJ), KPATCH)  =  XRUNOFFC    (KMASK(JJ), KPATCH) + XP_RUNOFF    (JJ) * PTSTEP
     XHORTC      (KMASK(JJ), KPATCH)  =  XHORTC      (KMASK(JJ), KPATCH) + XP_HORT      (JJ) * PTSTEP
     XDRIPC      (KMASK(JJ), KPATCH)  =  XDRIPC      (KMASK(JJ), KPATCH) + XP_DRIP      (JJ) * PTSTEP
     XRRVEGC     (KMASK(JJ), KPATCH)  =  XRRVEGC     (KMASK(JJ), KPATCH) + XP_RRVEG     (JJ) * PTSTEP
     XMELTC      (KMASK(JJ), KPATCH)  =  XMELTC      (KMASK(JJ), KPATCH) + XP_MELT      (JJ) * PTSTEP
     XIFLOODC    (KMASK(JJ), KPATCH)  =  XIFLOODC    (KMASK(JJ), KPATCH) + XP_IFLOOD    (JJ) * PTSTEP
     XPFLOODC    (KMASK(JJ), KPATCH)  =  XPFLOODC    (KMASK(JJ), KPATCH) + XP_PFLOOD    (JJ) * PTSTEP
     XLE_FLOODC  (KMASK(JJ), KPATCH)  =  XLE_FLOODC  (KMASK(JJ), KPATCH) + XP_LE_FLOOD  (JJ) * PTSTEP
     XLEI_FLOODC (KMASK(JJ), KPATCH)  =  XLEI_FLOODC (KMASK(JJ), KPATCH) + XP_LEI_FLOOD (JJ) * PTSTEP
     XIRRIG_FLUXC(KMASK(JJ), KPATCH)  =  XIRRIG_FLUXC(KMASK(JJ), KPATCH) + XP_IRRIG_FLUX(JJ) * PTSTEP
     !
     XSWDC(KMASK(JJ), KPATCH)  = XSWDC(KMASK(JJ), KPATCH) + XP_SWD(JJ) * PTSTEP
     XSWUC(KMASK(JJ), KPATCH)  = XSWUC(KMASK(JJ), KPATCH) + XP_SWU(JJ) * PTSTEP
     XLWDC(KMASK(JJ), KPATCH)  = XLWDC(KMASK(JJ), KPATCH) + XP_LWD(JJ) * PTSTEP
     XLWUC(KMASK(JJ), KPATCH)  = XLWUC(KMASK(JJ), KPATCH) + XP_LWU(JJ) * PTSTEP
     XFMUC(KMASK(JJ), KPATCH)  = XFMUC(KMASK(JJ), KPATCH) + XP_FMU(JJ) * PTSTEP
     XFMVC(KMASK(JJ), KPATCH)  = XFMVC(KMASK(JJ), KPATCH) + XP_FMV(JJ) * PTSTEP
     !
  END DO
  !
  IF (TSNOW%SCHEME=='3-L' .OR. TSNOW%SCHEME=='CRO') THEN
!cdir nodep
     DO JJ=1,KSIZE
        XLESLC (KMASK(JJ), KPATCH) = XLESLC (KMASK(JJ), KPATCH) + XP_LESL (JJ) * PTSTEP
     END DO
  END IF
  !
  IF(HPHOTO/='NON')THEN
!cdir nodep
     DO JJ=1,KSIZE
        !Transform units from kgCO2/kgair m/s to kgCO2/m2
        XGPPC       (KMASK(JJ), KPATCH)  =  XGPPC       (KMASK(JJ), KPATCH)+  XP_GPP       (JJ) * PRHOA(JJ) * PTSTEP
        XRESPC_AUTO (KMASK(JJ), KPATCH)  =  XRESPC_AUTO (KMASK(JJ), KPATCH)+  XP_RESP_AUTO (JJ) * PRHOA(JJ) * PTSTEP
        XRESPC_ECO  (KMASK(JJ), KPATCH)  =  XRESPC_ECO  (KMASK(JJ), KPATCH)+  XP_RESP_ECO  (JJ) * PRHOA(JJ) * PTSTEP
     END DO
  ELSE  
     XGPPC      (:,:)=0.0
     XRESPC_AUTO(:,:)=0.0
     XRESPC_ECO (:,:)=0.0       
  ENDIF
  !
  IF(LGLACIER)THEN
!cdir nodep
    DO JJ=1,KSIZE
       XICEFLUXC(KMASK(JJ), KPATCH)  = XICEFLUXC(KMASK(JJ), KPATCH) + XP_ICEFLUX(JJ) * PTSTEP
    END DO  
  END IF
  !  
  IF(LWATER_BUDGET)THEN
!cdir nodep
     DO JJ=1,KSIZE
        XDWGC   (KMASK(JJ), KPATCH)  =  XDWGC   (KMASK(JJ), KPATCH) + XP_DWG   (JJ) * PTSTEP
        XDWGIC  (KMASK(JJ), KPATCH)  =  XDWGIC  (KMASK(JJ), KPATCH) + XP_DWGI  (JJ) * PTSTEP
        XDWRC   (KMASK(JJ), KPATCH)  =  XDWRC   (KMASK(JJ), KPATCH) + XP_DWR   (JJ) * PTSTEP
        XDSWEC  (KMASK(JJ), KPATCH)  =  XDSWEC  (KMASK(JJ), KPATCH) + XP_DSWE  (JJ) * PTSTEP
        XWATBUDC(KMASK(JJ), KPATCH)  =  XWATBUDC(KMASK(JJ), KPATCH) + XP_WATBUD(JJ) * PTSTEP
     END DO
  ENDIF
  !  
END IF
IF (LHOOK) CALL DR_HOOK('DIAG_EVAP_ISBA_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE DIAG_EVAP_ISBA_n
