!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE WRITE_DIAG_SEB_TEB_n (DTCO, DGU, U, CHT, DGT, DGUT, &
                                       HPROGRAM)
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
!!      V. Masson   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2004
!!      Modified    01/2006 : TEB flux parameterization.
!!       V. Masson  10/2013 : Adds heat/cold stress ranges diagnostics
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
USE MODD_CH_TEB_n, ONLY : CH_TEB_t
USE MODD_DIAG_TEB_n, ONLY : DIAG_TEB_t
USE MODD_DIAG_UTCI_TEB_n, ONLY : DIAG_UTCI_TEB_t
!
#ifdef SFX_ARO
USE MODD_IO_SURF_ARO,   ONLY : NBLOCK
#endif
!
USE MODD_SURF_PAR,  ONLY : XUNDEF
USE MODD_UTCI
                           
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
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(DIAG_SURF_ATM_t), INTENT(INOUT) :: DGU
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(CH_TEB_t), INTENT(INOUT) :: CHT
TYPE(DIAG_TEB_t), INTENT(INOUT) :: DGT
TYPE(DIAG_UTCI_TEB_t), INTENT(INOUT) :: DGUT
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
LOGICAL           :: GRESET
INTEGER           :: JSV, JSW
INTEGER           :: JSTRESS         ! loop on heat stress ranges
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!         Initialisation for IO
!
IF (LHOOK) CALL DR_HOOK('WRITE_DIAG_SEB_TEB_N',0,ZHOOK_HANDLE)
!
GRESET=.TRUE.
#ifdef SFX_ARO
GRESET=(NBLOCK>0)
#endif
!
 CALL INIT_IO_SURF_n(DTCO, DGU, U, &
                    HPROGRAM,'TOWN  ','TEB   ','WRITE')
!
!
!
!*       2.     Richardson number :
!               -----------------
!
IF (DGT%N2M>=1) THEN

YRECFM='RI_TEB'
YCOMMENT='X_Y_'//YRECFM
!
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,DGT%XRI(:),IRESP,HCOMMENT=YCOMMENT)
!
END IF
!
!*       3.     Energy fluxes :
!               -------------
!
IF (DGT%LSURF_BUDGET) THEN

YRECFM='RN_TEB'
YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
!
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,DGT%XRN(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='H_TEB'
YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
!
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,DGT%XH(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='LE_TEB'
YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
!
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,DGT%XLE(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='GFLUX_TEB'
YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
!
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,DGT%XGFLUX(:),IRESP,HCOMMENT=YCOMMENT)
!
IF (DGT%LRAD_BUDGET) THEN
!        
   YRECFM='SWD_TEB'
   YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
   !
   CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,DGT%XSWD(:),IRESP,HCOMMENT=YCOMMENT)
   !
   YRECFM='SWU_TEB'
   YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
   !
   CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,DGT%XSWU(:),IRESP,HCOMMENT=YCOMMENT)
   !
   YRECFM='LWD_TEB'
   YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
   !
   CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,DGT%XLWD(:),IRESP,HCOMMENT=YCOMMENT)
   !
   YRECFM='LWU_TEB'
   YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
   !
   CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,DGT%XLWU(:),IRESP,HCOMMENT=YCOMMENT)
   !
   DO JSW=1, SIZE(DGT%XSWBD,2)
      YNUM=ACHAR(48+JSW)
      !
      YRECFM='SWD_TEB_'//YNUM
      YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
      !
      CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,DGT%XSWBD(:,JSW),IRESP,HCOMMENT=YCOMMENT)
      !
      YRECFM='SWU_TEB_'//YNUM
      YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
      !
      CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,DGT%XSWBU(:,JSW),IRESP,HCOMMENT=YCOMMENT)
      !
   ENDDO
!
ENDIF
!
YRECFM='FMU_TEB'
YCOMMENT='X_Y_'//YRECFM//' (kg/ms2)'
!
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,DGT%XFMU(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='FMV_TEB'
YCOMMENT='X_Y_'//YRECFM//' (kg/ms2)'
!
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,DGT%XFMV(:),IRESP,HCOMMENT=YCOMMENT)
!
END IF
!
!
!
!*       4.     Transfer coefficients
!               ---------------------
!
IF (DGT%LCOEF) THEN

YRECFM='CD_TEB'
YCOMMENT='X_Y_'//YRECFM
!
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,DGT%XCD(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='CH_TEB'
YCOMMENT='X_Y_'//YRECFM
!
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,DGT%XCH(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='CE_TEB'
YCOMMENT='X_Y_'//YRECFM
!
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,DGT%XCE(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='Z0_TEB'
YCOMMENT='X_Y_'//YRECFM//' (M)'
!
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,DGT%XZ0(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='Z0H_TEB'
YCOMMENT='X_Y_'//YRECFM//' (M)'
!
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,DGT%XZ0H(:),IRESP,HCOMMENT=YCOMMENT)
!
ENDIF
!
!
!*       5.     Surface humidity
!               ----------------
!
IF (DGT%LSURF_VARS) THEN

YRECFM='QS_TEB'
YCOMMENT='X_Y_'//YRECFM//' (KG/KG)'
!
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,DGT%XQS(:),IRESP,HCOMMENT=YCOMMENT)
!
ENDIF

!
!*       5.     parameters at 2 and 10 meters :
!               -----------------------------
!
IF (DGT%N2M>=1) THEN

YRECFM='T2M_TEB'
YCOMMENT='X_Y_'//YRECFM//' (K)'
!
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,DGT%XT2M(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='T2MMIN_TEB'
YCOMMENT='X_Y_'//YRECFM//' (K)'
!
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,DGT%XT2M_MIN(:),IRESP,HCOMMENT=YCOMMENT)
IF(GRESET)DGT%XT2M_MIN(:)=XUNDEF
!
YRECFM='T2MMAX_TEB'
YCOMMENT='X_Y_'//YRECFM//' (K)'
!
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,DGT%XT2M_MAX(:),IRESP,HCOMMENT=YCOMMENT)
IF(GRESET)DGT%XT2M_MAX(:)=-XUNDEF
!
YRECFM='Q2M_TEB'
YCOMMENT='X_Y_'//YRECFM//' (KG/KG)'
!
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,DGT%XQ2M(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='HU2M_TEB'
YCOMMENT='X_Y_'//YRECFM//' (KG/KG)'
!
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,DGT%XHU2M(:),IRESP,HCOMMENT=YCOMMENT)
 !
YRECFM='HU2MMIN_TEB'
YCOMMENT='X_Y_'//YRECFM//' (-)'
!
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,DGT%XHU2M_MIN(:),IRESP,HCOMMENT=YCOMMENT)
IF(GRESET)DGT%XHU2M_MIN(:)=XUNDEF
!
YRECFM='HU2MMAX_TEB'
YCOMMENT='X_Y_'//YRECFM//' (-)'
!
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,DGT%XHU2M_MAX(:),IRESP,HCOMMENT=YCOMMENT)
IF(GRESET)DGT%XHU2M_MAX(:)=-XUNDEF
!
YRECFM='ZON10M_TEB'
YCOMMENT='X_Y_'//YRECFM//' (M/S)'
!
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,DGT%XZON10M(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='MER10M_TEB'
YCOMMENT='X_Y_'//YRECFM//' (M/S)'
!
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,DGT%XMER10M(:),IRESP,HCOMMENT=YCOMMENT)
 !
YRECFM='W10M_TEB'
YCOMMENT='X_Y_'//YRECFM//' (M/S)'
!
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,DGT%XWIND10M(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='W10MMAX_TEB'
YCOMMENT='X_Y_'//YRECFM//' (M/S)'
!
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,DGT%XWIND10M_MAX(:),IRESP,HCOMMENT=YCOMMENT)
IF(GRESET)DGT%XWIND10M_MAX(:)=0.0
!
YRECFM='SFCO2_TEB'
YCOMMENT='X_Y_'//YRECFM//' (M.kgCO2.S-1.kgAIR-1)'
!
 CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,DGT%XSFCO2(:),IRESP,HCOMMENT=YCOMMENT)
!
END IF
!
IF (DGUT%LUTCI .AND. DGT%N2M >0) THEN
  YRECFM='UTCI_IN'
!RJ: extended ascii should be avoided in I/O
  YCOMMENT='UTCI for person indoor'//' (°C)'
  CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,DGUT%XUTCI_IN(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='UTCI_OUTSUN'
!RJ: extended ascii should be avoided in I/O
  YCOMMENT='UTCI for person at sun'//' (°C)'
  CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,DGUT%XUTCI_OUTSUN(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='UTCI_OUTSHAD'
!RJ: extended ascii should be avoided in I/O
  YCOMMENT='UTCI for person in shade'//' (°C)'
  CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,DGUT%XUTCI_OUTSHADE(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='TRAD_SUN'
  YCOMMENT='Mean radiant temperature seen by person at sun'//' (K)'
  CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,DGUT%XTRAD_SUN(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='TRAD_SHADE'
  YCOMMENT='Mean radiant temperature seen by person in shade'//' (K)'
  CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,DGUT%XTRAD_SHADE(:),IRESP,HCOMMENT=YCOMMENT)
  !
  DO JSTRESS=1,NUTCI_STRESS
    YRECFM='UTCIC_IN_'//CUTCI_STRESS_NAMES(JSTRESS)
    YCOMMENT='Cumulated time spent in '//CUTCI_STRESS_NAMES(JSTRESS)//' stress range for person indoor'//' (s)'
    CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,DGUT%XUTCIC_IN(:,JSTRESS),IRESP,HCOMMENT=YCOMMENT)
  END DO
  !
  DO JSTRESS=1,NUTCI_STRESS
    YRECFM='UTCIC_SU_'//CUTCI_STRESS_NAMES(JSTRESS)
    YCOMMENT='Cumulated time spent in '//CUTCI_STRESS_NAMES(JSTRESS)//' stress range for person at sun'//' (s)'
    CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,DGUT%XUTCIC_OUTSUN(:,JSTRESS),IRESP,HCOMMENT=YCOMMENT)
  END DO
  !
  DO JSTRESS=1,NUTCI_STRESS
    YRECFM='UTCIC_SH_'//CUTCI_STRESS_NAMES(JSTRESS)
    YCOMMENT='Cumulated time spent in '//CUTCI_STRESS_NAMES(JSTRESS)//' stress range for person in shade'//' (s)'
    CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,DGUT%XUTCIC_OUTSHADE(:,JSTRESS),IRESP,HCOMMENT=YCOMMENT)
  END DO
END IF
!
!
!*       6.     chemical diagnostics:
!               --------------------
!
IF (CHT%SVT%NBEQ>0 .AND. CHT%CCH_DRY_DEP=="WES89 ") THEN
  DO JSV = 1,SIZE(CHT%CCH_NAMES,1)
    YRECFM='DV_TWN_'//TRIM(CHT%CCH_NAMES(JSV))
    WRITE(YCOMMENT,'(A13,I3.3)')'(m/s) DV_TWN_',JSV
    CALL WRITE_SURF(DGU, U, &
                 HPROGRAM,YRECFM,CHT%XDEP(:,JSV),IRESP,HCOMMENT=YCOMMENT)
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
