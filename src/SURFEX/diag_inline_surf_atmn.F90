!     #########
       SUBROUTINE DIAG_INLINE_SURF_ATM_n (PHW, PHT, PPS, PRHOA, PTRAD, PEMIS, PSFU, PSFV, PSFCO2)
!     ###############################################################################!
!!****  *DIAG_INLINE_SURF_ATM_n * - Computes diagnostics during SURF_ATM time-step
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
!!      Original    02/2006
!!------------------------------------------------------------------
!

!
!
USE MODD_DIAG_SURF_ATM_n, ONLY : LCOEF, XDIAG_UREF, XDIAG_ZREF, &
                                 XPS, XRHOA, XDIAG_TRAD, XDIAG_EMIS,&
                                 XSSO_FMU, XSSO_FMV, XAVG_SFCO2
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, DIMENSION(:), INTENT(IN)       :: PHW    ! atmospheric level height for wind
REAL, DIMENSION(:), INTENT(IN)       :: PHT    ! atmospheric level height
REAL, DIMENSION(:), INTENT(IN)       :: PPS    ! surface pressure
REAL, DIMENSION(:), INTENT(IN)       :: PRHOA  ! air density
REAL, DIMENSION(:), INTENT(IN)       :: PTRAD  ! radiative temperature at t (K)
REAL, DIMENSION(:), INTENT(IN)       :: PEMIS  ! emissivity at t (-)
REAL, DIMENSION(:), INTENT(IN)       :: PSFU   ! zonal momentum flux                   (Pa)
REAL, DIMENSION(:), INTENT(IN)       :: PSFV   ! meridian momentum flux                (Pa)
REAL, DIMENSION(:), INTENT(IN)       :: PSFCO2 ! CO2 flux                              (kg/m2/s)
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*      0.2    declarations of local variables
!
!-------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('DIAG_INLINE_SURF_ATM_N',0,ZHOOK_HANDLE)
IF (LCOEF) THEN
  XDIAG_UREF = PHW
  XDIAG_ZREF = PHT
END IF
!
XRHOA = PRHOA
XPS   = PPS
XDIAG_TRAD = PTRAD
XDIAG_EMIS = PEMIS
!
XSSO_FMU   = PSFU
XSSO_FMV   = PSFV
!
XAVG_SFCO2 = PSFCO2
!
IF (LHOOK) CALL DR_HOOK('DIAG_INLINE_SURF_ATM_N',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------------
!
END SUBROUTINE DIAG_INLINE_SURF_ATM_n
