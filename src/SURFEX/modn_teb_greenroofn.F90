!     ##################
      MODULE MODN_TEB_GREENROOF_n
!     ##################
!
!!****  *MODN_TEB_GREENROOF_n* - declaration of namelist NAM_TEBn
!!
!!    PURPOSE
!!    -------
!       The purpose of this module is to specify the namelist NAM_TEB_GREENROOFn
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!    Based on modn_tebn
!!       
!!    AUTHOR
!!    ------
!!	C. de Munck & A. Lemonsu    *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    07/2011                   
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
USE MODD_TEB_GREENROOF_n, ONLY:                 &
           CRUNOFF_GR_n => CRUNOFF_GR,          &
           CSCOND_GR_n  => CSCOND_GR,           & 
           CKSAT_GR_n   => CKSAT_GR,            &
           CSOC_GR_n    => CSOC_GR,             &
           CHORT_GR_n   => CHORT_GR
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
 CHARACTER(LEN=4)  :: CSCOND_GR
 CHARACTER(LEN=4)  :: CRUNOFF_GR
 CHARACTER(LEN=3)  :: CKSAT_GR
 CHARACTER(LEN=3)  :: CSOC_GR
 CHARACTER(LEN=3)  :: CHORT_GR         
!
NAMELIST/NAM_TEB_GREENROOFn/CRUNOFF_GR,CSCOND_GR,CKSAT_GR,CSOC_GR,CHORT_GR
!
CONTAINS
!
! subroutine INIT !
SUBROUTINE INIT_NAM_TEB_GREENROOFn

  REAL(KIND=JPRB) :: ZHOOK_HANDLE

  IF (LHOOK) CALL DR_HOOK('MODN_TEB_GREENROOF_N:INIT_NAM_TEB_GREENROOFN',0,ZHOOK_HANDLE)
  CSCOND_GR     = CSCOND_GR_n
  CRUNOFF_GR    = CRUNOFF_GR_n
  CKSAT_GR      = CKSAT_GR_n
  CSOC_GR       = CSOC_GR_n
  CHORT_GR      = CHORT_GR_n
IF (LHOOK) CALL DR_HOOK('MODN_TEB_GREENROOF_N:INIT_NAM_TEB_GREENROOFN',1,ZHOOK_HANDLE)
END SUBROUTINE INIT_NAM_TEB_GREENROOFn

! subroutine UPDATE !
SUBROUTINE UPDATE_NAM_TEB_GREENROOFn

  REAL(KIND=JPRB) :: ZHOOK_HANDLE

  IF (LHOOK) CALL DR_HOOK('MODN_TEB_GREENROOF_N:UPDATE_NAM_TEB_GREENROOFN',0,ZHOOK_HANDLE)
  CSCOND_GR_n    = CSCOND_GR
  CRUNOFF_GR_n   = CRUNOFF_GR
  CKSAT_GR_n     = CKSAT_GR
  CSOC_GR_n      = CSOC_GR
  CHORT_GR_n     = CHORT_GR
IF (LHOOK) CALL DR_HOOK('MODN_TEB_GREENROOF_N:UPDATE_NAM_TEB_GREENROOFN',1,ZHOOK_HANDLE)
END SUBROUTINE UPDATE_NAM_TEB_GREENROOFn

END MODULE MODN_TEB_GREENROOF_n
