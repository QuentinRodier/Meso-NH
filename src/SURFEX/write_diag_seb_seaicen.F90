!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE WRITE_DIAG_SEB_SEAICE_n (DTCO, DGU, U, DGS, DGSI, S, &
                                          HPROGRAM)
!     #################################
!
!!****  *WRITE_DIAG_SEB_SEAICE_n* - write the seaice diagnostic fields
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
!!      S.Senesi                *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2014
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_DIAG_SURF_ATM_n, ONLY : DIAG_SURF_ATM_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_DIAG_SEAFLUX_n, ONLY : DIAG_SEAFLUX_t
USE MODD_DIAG_SEAICE_n, ONLY : DIAG_SEAICE_t
USE MODD_SEAFLUX_n, ONLY : SEAFLUX_t
!
USE MODD_SFX_OASIS,      ONLY : LCPL_SEAICE
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
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(DIAG_SURF_ATM_t), INTENT(INOUT) :: DGU
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(DIAG_SEAFLUX_t), INTENT(INOUT) :: DGS
TYPE(DIAG_SEAICE_t), INTENT(INOUT) :: DGSI
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
INTEGER           :: JSV, JSW
!
REAL(KIND=JPRB)   :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!         Initialisation for IO
!
IF (LHOOK) CALL DR_HOOK('WRITE_DIAG_SEB_SEAICE_N',0,ZHOOK_HANDLE)
!
 CALL INIT_IO_SURF_n(DTCO, DGU, U, &
                    HPROGRAM,'SEA   ','SEAFLX','WRITE')
!
IF(LCPL_SEAICE.OR.S%LHANDLE_SIC)THEN      
!
  YCOMMENT='Sea-ice temperature (K)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,'TSICE',S%XTICE(:),IRESP,YCOMMENT)
!
  YCOMMENT='Sea-ice albedo (-)'
  CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,'IALB',S%XICE_ALB(:),IRESP,YCOMMENT)
!
ENDIF
!
IF (TRIM(S%CSEAICE_SCHEME) == 'GELATO') THEN 
    YCOMMENT='Sea-ice thickness (m)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,'SIT',DGSI%XSIT(:),IRESP,YCOMMENT)
    !
    YCOMMENT='Sea-ice snow depth (m)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,'SND',DGSI%XSND(:),IRESP,YCOMMENT)
    !
    YCOMMENT='Sea mixed layer temp for Glt (K)'
    CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,'SIMLT',DGSI%XMLT(:),IRESP,YCOMMENT)
    !
ENDIF
!
!
!*       8.2.     Richardson number :
!               -----------------
IF (DGS%N2M>=1) THEN
   !
   YRECFM='RI_SEAICE'
   YCOMMENT='X_Y_'//YRECFM
   !
   CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGS%XRI_ICE(:),IRESP,HCOMMENT=YCOMMENT)
   !
END IF
!
!*       8.3     Energy fluxes :
!               -------------
!
IF (DGS%LSURF_BUDGET) THEN

   YRECFM='RN_SEAICE'
   YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
   !
   CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGS%XRN_ICE(:),IRESP,HCOMMENT=YCOMMENT)
   !
   YRECFM='H_SEAICE'
   YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
   !
   CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGS%XH_ICE(:),IRESP,HCOMMENT=YCOMMENT)
   !
   YRECFM='LE_SEAICE'
   YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
   !
   CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGS%XLE_ICE(:),IRESP,HCOMMENT=YCOMMENT)
   !
   YRECFM='GFLX_SEAICE'
   YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
   !
   CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGS%XGFLUX_ICE(:),IRESP,HCOMMENT=YCOMMENT)
   !
   IF (DGS%LRAD_BUDGET) THEN
      !
      YRECFM='SWU_SEAICE'
      YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
      !
      CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGS%XSWU_ICE(:),IRESP,HCOMMENT=YCOMMENT)
      !
      YRECFM='LWU_SEAICE'
      YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
      !
      CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGS%XLWU_ICE(:),IRESP,HCOMMENT=YCOMMENT)
      !
      DO JSW=1, SIZE(DGS%XSWBU_ICE,2)
         YNUM=ACHAR(48+JSW)
         !
         YRECFM='SWU_SEAICE_'//YNUM
         YCOMMENT='X_Y_'//YRECFM//' (W/m2)'
         !
         CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGS%XSWBU_ICE(:,JSW),IRESP,HCOMMENT=YCOMMENT)
         !
      ENDDO
      !
   ENDIF
   !
   YRECFM='FMU_SEAICE'
   YCOMMENT='X_Y_'//YRECFM//' (kg/ms2)'
   !
   CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGS%XFMU_ICE(:),IRESP,HCOMMENT=YCOMMENT)
   !
   YRECFM='FMV_SEAICE'
   YCOMMENT='X_Y_'//YRECFM//' (kg/ms2)'
   !
   CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGS%XFMV_ICE(:),IRESP,HCOMMENT=YCOMMENT)
   !
END IF
!
IF (DGS%LSURF_BUDGETC) THEN
   !
   YRECFM='RNC_SEAICE'
   YCOMMENT='X_Y_'//YRECFM//' (J/m2)'
   !
   CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGS%XRNC_ICE(:),IRESP,HCOMMENT=YCOMMENT)
   !
   YRECFM='HC_SEAICE'
   YCOMMENT='X_Y_'//YRECFM//' (J/m2)'
   !
   CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGS%XHC_ICE(:),IRESP,HCOMMENT=YCOMMENT)
   !
   YRECFM='LEC_SEAICE'
   YCOMMENT='X_Y_'//YRECFM//' (J/m2)'
   !
   CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGS%XLEC_ICE(:),IRESP,HCOMMENT=YCOMMENT)
   !
   YRECFM='GFLXC_SEAICE'
   YCOMMENT='X_Y_'//YRECFM//' (J/m2)'
   !
   CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGS%XGFLUXC_ICE(:),IRESP,HCOMMENT=YCOMMENT)
   IF (DGS%LRAD_BUDGET .OR. (DGS%LSURF_BUDGETC .AND. .NOT.DGU%LRESET_BUDGETC)) THEN
      !
      YRECFM='SWUC_SEAICE'
      YCOMMENT='X_Y_'//YRECFM//' (J/m2)'
      !
      CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGS%XSWUC_ICE(:),IRESP,HCOMMENT=YCOMMENT)
      !
      YRECFM='LWUC_SEAICE'
      YCOMMENT='X_Y_'//YRECFM//' (J/m2)'
      !
      CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGS%XLWUC_ICE(:),IRESP,HCOMMENT=YCOMMENT)
      !
   ENDIF
   !
   YRECFM='FMUC_SEAICE'
   YCOMMENT='X_Y_'//YRECFM//' (kg/ms)'
   !
   CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGS%XFMUC_ICE(:),IRESP,HCOMMENT=YCOMMENT)
   !
   YRECFM='FMVC_SEAICE'
   YCOMMENT='X_Y_'//YRECFM//' (kg/ms)'
   !
   CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGS%XFMVC_ICE(:),IRESP,HCOMMENT=YCOMMENT)
   !
END IF
!
!*       8.4     transfer coefficients
!               ---------------------
!
IF (DGS%LCOEF) THEN
   !
   YRECFM='CD_SEAICE'
   YCOMMENT='X_Y_'//YRECFM//' (W/s2)'
   !
   CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGS%XCD_ICE(:),IRESP,HCOMMENT=YCOMMENT)
   !
   YRECFM='CH_SEAICE'
   YCOMMENT='X_Y_'//YRECFM//' (W/s)'
   !
   CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGS%XCH_ICE(:),IRESP,HCOMMENT=YCOMMENT)
   !
   YRECFM='Z0_SEAICE'
   YCOMMENT='X_Y_'//YRECFM//' (M)'
   !
   CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGS%XZ0_ICE(:),IRESP,HCOMMENT=YCOMMENT)
   !
   YRECFM='Z0H_SEAICE'
   YCOMMENT='X_Y_'//YRECFM//' (M)'
   !
   CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGS%XZ0H_ICE(:),IRESP,HCOMMENT=YCOMMENT)
   !
END IF
!
!
!*       8.5     Surface humidity
!               ----------------
!
IF (DGS%LSURF_VARS) THEN
   YRECFM='QS_SEAICE'
   YCOMMENT='X_Y_'//YRECFM//' (KG/KG)'
   !
   CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGS%XQS_ICE(:),IRESP,HCOMMENT=YCOMMENT)
   !
ENDIF
!

!
!*       8.6.     parameters at 2 and 10 meters :
!               -----------------------------
!
IF (DGS%N2M>=1) THEN
   !
   YRECFM='T2M_SEAICE'
   YCOMMENT='X_Y_'//YRECFM//' (K)'
   !
   CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGS%XT2M_ICE(:),IRESP,HCOMMENT=YCOMMENT)
   !
   YRECFM='Q2M_SEAICE'
   YCOMMENT='X_Y_'//YRECFM//' (KG/KG)'
   !
   CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGS%XQ2M_ICE(:),IRESP,HCOMMENT=YCOMMENT)
   !
   YRECFM='HU2M_SEAICE'
   YCOMMENT='X_Y_'//YRECFM//' (-)'
   !
   CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGS%XHU2M_ICE(:),IRESP,HCOMMENT=YCOMMENT)
   !
   YRECFM='ZON10M_SEAICE'
   YCOMMENT='X_Y_'//YRECFM//' (M/S)'
   !
   CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGS%XZON10M_ICE(:),IRESP,HCOMMENT=YCOMMENT)
   !
   YRECFM='MER10M_SEAICE'
   YCOMMENT='X_Y_'//YRECFM//' (M/S)'
   !
   CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGS%XMER10M_ICE(:),IRESP,HCOMMENT=YCOMMENT)
   !
   YRECFM='W10M_SEAICE'
   YCOMMENT='X_Y_'//YRECFM//' (M/S)'
   !
   CALL WRITE_SURF(DGU, U, &
                  HPROGRAM,YRECFM,DGS%XWIND10M_ICE(:),IRESP,HCOMMENT=YCOMMENT)
   !
END IF
!
!         End of IO
!
 CALL END_IO_SURF_n(HPROGRAM)

IF (LHOOK) CALL DR_HOOK('WRITE_DIAG_SEB_SEAICE_N',1,ZHOOK_HANDLE)
!
!
END SUBROUTINE WRITE_DIAG_SEB_SEAICE_n
