!##################
MODULE MODN_TRIP_n
!##################
!
!!****  *MODN_TRIP_n* - declaration of namelist NAM_TRIP
!!
!!    PURPOSE
!!    -------
!       The purpose of this module is to specify  the namelist NAM_TRIP
!     which concern the river routing model configuration configuration.
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!       
!!    AUTHOR
!!    ------
!!	B. Decharme    *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    05/2008                    
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
USE MODD_TRIP_n, ONLY:                         &
           CVIT_n => CVIT,                       &
           CGROUNDW_n => CGROUNDW,               &
           LFLOODT_n => LFLOODT,                 &
           LTRIP_DIAG_MISC_n => LTRIP_DIAG_MISC, &
           LDIAG_CPL_n => LDIAG_CPL,             &
           LNCPRINT_n => LNCPRINT,               &
           LPRINT_TRIP_n => LPRINT_TRIP,         &
           XDATA_TAUG_n => XDATA_TAUG,           &
           XCVEL_n => XCVEL,                     &
           XRATMED_n => XRATMED,                 &
           XTRIP_TSTEP_n => XTRIP_TSTEP,         &
           XTSTEP_COUPLING_n => XTSTEP_COUPLING  
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
 CHARACTER(LEN=3)  :: CVIT
 CHARACTER(LEN=3)  :: CGROUNDW
LOGICAL  :: LFLOODT
LOGICAL  :: LTRIP_DIAG_MISC
LOGICAL  :: LDIAG_CPL
LOGICAL  :: LNCPRINT
LOGICAL  :: LPRINT_TRIP
REAL     :: XTSTEP_COUPLING
REAL     :: XDATA_TAUG
REAL     :: XCVEL
REAL     :: XRATMED
REAL     :: XTRIP_TSTEP
!
NAMELIST/NAM_TRIPn/CVIT,CGROUNDW,LFLOODT,LTRIP_DIAG_MISC,LDIAG_CPL, &
                     LNCPRINT,LPRINT_TRIP,XTSTEP_COUPLING,XTRIP_TSTEP,&
                     XDATA_TAUG,XRATMED,XCVEL  
!
CONTAINS
!
SUBROUTINE INIT_NAM_TRIPn

  REAL(KIND=JPRB) :: ZHOOK_HANDLE

  IF (LHOOK) CALL DR_HOOK('MODN_TRIP_N:INIT_NAM_TRIPN',0,ZHOOK_HANDLE)
  CVIT             = CVIT_n
  CGROUNDW        = CGROUNDW_n
  LFLOODT         = LFLOODT_n
  LTRIP_DIAG_MISC = LTRIP_DIAG_MISC_n
  LDIAG_CPL       = LDIAG_CPL_n
  LNCPRINT        = LNCPRINT_n
  LPRINT_TRIP     = LPRINT_TRIP_n
  XDATA_TAUG      = XDATA_TAUG_n
  XCVEL           = XCVEL_n
  XRATMED         = XRATMED_n
  XTRIP_TSTEP     = XTRIP_TSTEP_n
  XTSTEP_COUPLING = XTSTEP_COUPLING_n
IF (LHOOK) CALL DR_HOOK('MODN_TRIP_N:INIT_NAM_TRIPN',1,ZHOOK_HANDLE)
END SUBROUTINE INIT_NAM_TRIPn
!
SUBROUTINE UPDATE_NAM_TRIPn

  REAL(KIND=JPRB) :: ZHOOK_HANDLE

  IF (LHOOK) CALL DR_HOOK('MODN_TRIP_N:UPDATE_NAM_TRIPN',0,ZHOOK_HANDLE)
  CVIT_n            = CVIT
  CGROUNDW_n        = CGROUNDW
  LFLOODT_n         = LFLOODT
  LTRIP_DIAG_MISC_n = LTRIP_DIAG_MISC
  LDIAG_CPL_n       = LDIAG_CPL
  LNCPRINT_n        = LNCPRINT
  LPRINT_TRIP_n     = LPRINT_TRIP
  XDATA_TAUG_n      = XDATA_TAUG
  XCVEL_n           = XCVEL
  XRATMED_n         = XRATMED
  XTRIP_TSTEP_n     = XTRIP_TSTEP
  XTSTEP_COUPLING_n = XTSTEP_COUPLING
IF (LHOOK) CALL DR_HOOK('MODN_TRIP_N:UPDATE_NAM_TRIPN',1,ZHOOK_HANDLE)
END SUBROUTINE UPDATE_NAM_TRIPn
!
END MODULE MODN_TRIP_n
