!     #########
SUBROUTINE READ_NAMELISTS_TRIP_n(HPROGRAM)
!     #######################################################
!
!----------------------------------------------
!
USE MODN_TRIP_n
!
USE MODI_DEFAULT_TRIP
!
USE MODI_READ_TRIP_CONF_n
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
!--------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('READ_NAMELISTS_TRIP_N',0,ZHOOK_HANDLE)
 CALL DEFAULT_TRIP(CGROUNDW,CVIT,LFLOODT,LDIAG_CPL,LTRIP_DIAG_MISC,   &
                    LNCPRINT,LPRINT_TRIP,XTSTEP_COUPLING,XTRIP_TSTEP,&
                    XDATA_TAUG,XCVEL,XRATMED                         )  
!
 CALL READ_TRIP_CONF_n(HPROGRAM)
!
IF (LHOOK) CALL DR_HOOK('READ_NAMELISTS_TRIP_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------
!
END SUBROUTINE READ_NAMELISTS_TRIP_n
