!     #########
      SUBROUTINE WRITE_DIAG_SEB_FLAKE_n(HPROGRAM)
!     #################################
!
!!****  *WRITE_DIAG_SEB_FLAKE_n* - writes FLAKE diagnostics
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
USE MODD_DIAG_FLAKE_n,  ONLY : N2M, LSURF_BUDGET, LRAD_BUDGET,  LCOEF,   &
                                 LSURF_VARS, XRN, XH, XLE, XLEI, XGFLUX,   &
                                 XRI, XCD, XCH, XCE, XZ0, XZ0H,            &
                                 XT2M, XQ2M, XHU2M,                        &
                                 XZON10M, XMER10M, XQS,                    &
                                 XSWD, XSWU, XLWD, XLWU, XSWBD, XSWBU,     &
                                 XFMU, XFMV  

USE MODD_CH_WATFLUX_n,  ONLY : XDEP, CCH_DRY_DEP, CCH_NAMES, NBEQ
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
IF (LHOOK) CALL DR_HOOK('WRITE_DIAG_SEB_FLAKE_N',0,ZHOOK_HANDLE)
 CALL INIT_IO_SURF_n(HPROGRAM,'WATER ','FLAKE ','WRITE')
!
!
!*       2.     Richardson number :
!               -----------------
!
IF (N2M>=1) THEN

YRECFM='RI_WAT'
YCOMMENT='Bulk-Richardson number for water'
!
 CALL WRITE_SURF(HPROGRAM,YRECFM,XRI(:),IRESP,HCOMMENT=YCOMMENT)
!
END IF
!
!*       3.     Energy fluxes :
!               -------------
!
IF (LSURF_BUDGET) THEN

YRECFM='RN_WAT'
YCOMMENT='net radiation for water'//' (W/m2)'
!
 CALL WRITE_SURF(HPROGRAM,YRECFM,XRN(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='H_WAT'
YCOMMENT='sensible heat flux for water'//' (W/m2)'
!
 CALL WRITE_SURF(HPROGRAM,YRECFM,XH(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='LE_WAT'
YCOMMENT='total latent heat flux for water'//' (W/m2)'
!
 CALL WRITE_SURF(HPROGRAM,YRECFM,XLE(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='LEI_WAT'
YCOMMENT='sublimation latent heat flux for water-ice'//' (W/m2)'
!
 CALL WRITE_SURF(HPROGRAM,YRECFM,XLEI(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='GFLUX_WAT'
YCOMMENT='conduction flux for water'//' (W/m2)'
!
 CALL WRITE_SURF(HPROGRAM,YRECFM,XGFLUX(:),IRESP,HCOMMENT=YCOMMENT)
!
IF (LRAD_BUDGET) THEN
!
   YRECFM='SWD_WAT'
   YCOMMENT='short wave downward radiation for water'//' (W/m2)'
   !
   CALL WRITE_SURF(HPROGRAM,YRECFM,XSWD(:),IRESP,HCOMMENT=YCOMMENT)
   !
   YRECFM='SWU_WAT'
   YCOMMENT='short wave upward radiation for water'//' (W/m2)'
   !
   CALL WRITE_SURF(HPROGRAM,YRECFM,XSWU(:),IRESP,HCOMMENT=YCOMMENT)
   !
   YRECFM='LWD_WAT'
   YCOMMENT='downward long wave radiation'//' (W/m2)'
   !
   CALL WRITE_SURF(HPROGRAM,YRECFM,XLWD(:),IRESP,HCOMMENT=YCOMMENT)
   !
   YRECFM='LWU_WAT'
   YCOMMENT='upward long wave radiation'//' (W/m2)'
   !
   CALL WRITE_SURF(HPROGRAM,YRECFM,XLWU(:),IRESP,HCOMMENT=YCOMMENT)
   !       
   DO JSW=1, SIZE(XSWBD,2)
      YNUM=ACHAR(48+JSW)
      !
      YRECFM='SWD_WAT_'//YNUM
      YCOMMENT='downward short wave radiation by spectral band '//' (W/m2)'
      !
      CALL WRITE_SURF(HPROGRAM,YRECFM,XSWBD(:,JSW),IRESP,HCOMMENT=YCOMMENT)
      !
      YRECFM='SWU_WAT_'//YNUM
      YCOMMENT='upward short wave radiation by spectral band'//' (W/m2)'
      !
      CALL WRITE_SURF(HPROGRAM,YRECFM,XSWBU(:,JSW),IRESP,HCOMMENT=YCOMMENT)
      !
   ENDDO
!
ENDIF
!
YRECFM='FMU_WAT'
YCOMMENT='u-component of momentum flux for water'//' (kg/ms2)'
!
 CALL WRITE_SURF(HPROGRAM,YRECFM,XFMU(:),IRESP,HCOMMENT=YCOMMENT)
YRECFM='FMV_WAT'
YCOMMENT='v-component of momentum flux for water'//' (kg/ms2)'
!
 CALL WRITE_SURF(HPROGRAM,YRECFM,XFMV(:),IRESP,HCOMMENT=YCOMMENT)
!
END IF
!
!
!*       4.     Transfer coefficients
!               ---------------------
!
IF (LCOEF) THEN

YRECFM='CD_WAT'
YCOMMENT='drag coefficient for wind over water (W/s2)'
!
 CALL WRITE_SURF(HPROGRAM,YRECFM,XCD(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='CH_WAT'
YCOMMENT='drag coefficient for heat (W/s)'
!
 CALL WRITE_SURF(HPROGRAM,YRECFM,XCH(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='CE_WAT'
YCOMMENT='drag coefficient for vapor (W/s/K)'
!
 CALL WRITE_SURF(HPROGRAM,YRECFM,XCE(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='Z0_WAT'
YCOMMENT='roughness length over water (m)'

 CALL WRITE_SURF(HPROGRAM,YRECFM,XZ0(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='Z0H_WAT'
YCOMMENT='thermal roughness length over water (m)'
!
 CALL WRITE_SURF(HPROGRAM,YRECFM,XZ0H(:),IRESP,HCOMMENT=YCOMMENT)
!
END IF
!
!
!*       5.     Surface humidity
!               ----------------
!
IF (LSURF_VARS) THEN

YRECFM='QS_WAT'
YCOMMENT='specific humidity over water'//' (KG/KG)'
!
 CALL WRITE_SURF(HPROGRAM,YRECFM,XQS(:),IRESP,HCOMMENT=YCOMMENT)
!
ENDIF
!

!
!*       6.     parameters at 2 and 10 meters :
!               -----------------------------
!
IF (N2M>=1) THEN

YRECFM='T2M_WAT'
YCOMMENT='2 meters temperature'//' (K)'
!
 CALL WRITE_SURF(HPROGRAM,YRECFM,XT2M(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='Q2M_WAT'
YCOMMENT='2 meters specific humidity'//' (KG/KG)'
!
 CALL WRITE_SURF(HPROGRAM,YRECFM,XQ2M(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='HU2M_WAT'
YCOMMENT='2 meters relative humidity'//' (KG/KG)'
!
 CALL WRITE_SURF(HPROGRAM,YRECFM,XHU2M(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='ZON10M_WAT'
YCOMMENT='10 meters zonal wind'//' (M/S)'
!
 CALL WRITE_SURF(HPROGRAM,YRECFM,XZON10M(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='MER10M_WAT'
YCOMMENT='10 meters meridian wind'//' (M/S)'
!
 CALL WRITE_SURF(HPROGRAM,YRECFM,XMER10M(:),IRESP,HCOMMENT=YCOMMENT)
!
END IF
!
!
!*       7.     chemical diagnostics:
!               --------------------
!
IF (NBEQ>0 .AND. CCH_DRY_DEP=="WES89 ") THEN
  DO JSV = 1,SIZE(CCH_NAMES,1)
    YRECFM='DV_WAT_'//TRIM(CCH_NAMES(JSV))
    WRITE(YCOMMENT,'(A26)')'final dry deposition (m/s)'
    CALL WRITE_SURF(HPROGRAM,YRECFM,XDEP(:,JSV),IRESP,HCOMMENT=YCOMMENT)
  END DO
ENDIF
!
!-------------------------------------------------------------------------------
!
!         End of IO
!
 CALL END_IO_SURF_n(HPROGRAM)
IF (LHOOK) CALL DR_HOOK('WRITE_DIAG_SEB_FLAKE_N',1,ZHOOK_HANDLE)
!
!
END SUBROUTINE WRITE_DIAG_SEB_FLAKE_n
