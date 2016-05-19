!     #########
SUBROUTINE READ_NAMELISTS_TEB_n(HPROGRAM, HINIT)
!     #######################################################
!
!---------------------------------------------------------------------------
!
USE MODN_TEB_n                          
USE MODN_TEB_VEG_n,            ONLY: CROUGH,CRUNOFF,CALBEDO,CSCOND,                &
                                     CC1DRY, CSOILFRZ, CDIFSFCOND, CSNOWRES,       &
                                     CCPSURF, XCGMAX, CKSAT, CTOPREG,              &
                                     CRAIN, CHORT, LFLOOD, LTRIP , LGLACIER,       &
                                     LCANOPY_DRAG, LVEGUPD
USE MODN_TEB_GREENROOF_n,      ONLY: CRUNOFF_GR,CSCOND_GR,CKSAT_GR,CHORT_GR
!
USE MODI_DEFAULT_TEB
USE MODI_DEFAULT_TEB_VEG
USE MODI_DEFAULT_GREENROOF
USE MODI_DEFAULT_CH_DEP
USE MODI_DEFAULT_DIAG_TEB
USE MODI_READ_DEFAULT_TEB_n
USE MODI_READ_TEB_CONF_n
USE MODI_READ_TEB_VEG_CONF_n
!
USE MODI_READ_NAM_PREP_TEB_n
USE MODI_READ_NAM_PREP_GARDEN_n
USE MODI_READ_NAM_PREP_GREENROOF_n
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_READ_TEB_CONF_n
IMPLICIT NONE
!
 CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
 CHARACTER(LEN=3),   INTENT(IN)  :: HINIT     ! choice of fields to initialize
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('READ_NAMELISTS_TEB_N',0,ZHOOK_HANDLE)
 CALL DEFAULT_TEB(CZ0H,XTSTEP,XOUT_TSTEP, CCH_BEM)
!
 CALL DEFAULT_TEB_VEG(CROUGH,CRUNOFF,CALBEDO,CSCOND,            &
                     CC1DRY, CSOILFRZ, CDIFSFCOND, CSNOWRES,   &
                     CCPSURF, XCGMAX, CKSAT, CTOPREG,          &
                     CRAIN, CHORT, LFLOOD, LTRIP , LGLACIER,   &
                     LCANOPY_DRAG, LVEGUPD                     )
!
 CALL DEFAULT_GREENROOF(CRUNOFF_GR,CSCOND_GR,                   &
                       CKSAT_GR,CHORT_GR)
!
 CALL DEFAULT_CH_DEP(CCH_DRY_DEP)
!
 CALL DEFAULT_DIAG_TEB(N2M,LSURF_BUDGET,L2M_MIN_ZS,LRAD_BUDGET, &
                      LCOEF,LSURF_VARS,LSURF_MISC_BUDGET,LUTCI,&
                      LPGD,LPGD_FIX,XDIAG_TSTEP)   
!               
 CALL READ_DEFAULT_TEB_n(HPROGRAM)
!
 CALL READ_TEB_CONF_n(HPROGRAM) 
!  
 CALL READ_TEB_VEG_CONF_n(HPROGRAM) 
!
IF (HINIT=='PRE') THEN
        CALL READ_NAM_PREP_TEB_n(HPROGRAM)
        CALL READ_NAM_PREP_GARDEN_n(HPROGRAM)
        CALL READ_NAM_PREP_GREENROOF_n(HPROGRAM)
ENDIF
!
IF (LHOOK) CALL DR_HOOK('READ_NAMELISTS_TEB_N',1,ZHOOK_HANDLE)
!
!------------------------------------
!
END SUBROUTINE READ_NAMELISTS_TEB_n
