!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
       SUBROUTINE DIAG_INLINE_SURF_ATM_n (DGU, &
                                           PHW, PHT, PPS, PRHOA, PTRAD, PEMIS, PSFU, PSFV, PSFCO2)
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
!
!
USE MODD_DIAG_SURF_ATM_n, ONLY : DIAG_SURF_ATM_t
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
TYPE(DIAG_SURF_ATM_t), INTENT(INOUT) :: DGU
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
IF (DGU%LCOEF) THEN
  DGU%XDIAG_UREF = PHW
  DGU%XDIAG_ZREF = PHT
END IF
!
DGU%XRHOA = PRHOA
DGU%XPS   = PPS
DGU%XDIAG_TRAD = PTRAD
DGU%XDIAG_EMIS = PEMIS
!
DGU%XSSO_FMU   = PSFU
DGU%XSSO_FMV   = PSFV
!
DGU%XAVG_SFCO2 = PSFCO2
!
IF (LHOOK) CALL DR_HOOK('DIAG_INLINE_SURF_ATM_N',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------------
!
END SUBROUTINE DIAG_INLINE_SURF_ATM_n
