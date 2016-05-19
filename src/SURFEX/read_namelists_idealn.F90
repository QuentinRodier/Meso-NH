!     #########
SUBROUTINE READ_NAMELISTS_IDEAL_n(HPROGRAM)
!     #######################################################
!
!--------------------------------------------------------------------------
!
USE MODN_IDEAL_n
!
USE MODI_DEFAULT_DIAG_IDEAL
USE MODI_READ_DEFAULT_IDEAL_n
USE MODI_READ_IDEAL_CONF_n
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
 CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!----------------------------------------------------------
!ideal: reprend essentiellement la namelist NAM_DIAG_SURF
IF (LHOOK) CALL DR_HOOK('READ_NAMELISTS_IDEAL_N',0,ZHOOK_HANDLE)
 CALL DEFAULT_DIAG_IDEAL(N2M,LSURF_BUDGET,L2M_MIN_ZS,LRAD_BUDGET,LCOEF,LSURF_VARS,&
                         LSURF_BUDGETC,LRESET_BUDGETC,XDIAG_TSTEP           )
!                        
 CALL READ_DEFAULT_IDEAL_n(HPROGRAM)
!
 CALL READ_IDEAL_CONF_n(HPROGRAM)   
IF (LHOOK) CALL DR_HOOK('READ_NAMELISTS_IDEAL_N',1,ZHOOK_HANDLE)
!
END SUBROUTINE READ_NAMELISTS_IDEAL_n
