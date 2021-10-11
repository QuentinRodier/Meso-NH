!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ###########################
      MODULE MODD_DIAG_UTCI_n
!     ############################
!
!!****  *MODD_DIAG_UTCI - declaration of confort from TEB & ISBA schemes
!!
!!    PURPOSE
!!    -------
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      None 
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!      P. Le Moigne   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original       07/10/04
!!      R. Schoetter   29/03/2017   Add time averaged comfort indices
!
!
!*       0.   DECLARATIONS
!             ------------
!
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE

TYPE DIAG_UTCI_t
!------------------------------------------------------------------------------
!
  LOGICAL :: LUTCI                                       ! flag to compute UTCI quantities
  INTEGER :: NCOUNT_UTCI_STEP                            ! Counter for UTCI
  REAL, POINTER, DIMENSION(:,:)   :: XUTCI_IN            ! UTCI for person indoor
  REAL, POINTER, DIMENSION(:)     :: XUTCI_OUTSUN        ! UTCI for person outdoor at sun
  REAL, POINTER, DIMENSION(:)     :: XUTCI_OUTSHADE      ! UTCI for person outdoor at shade
  REAL, POINTER, DIMENSION(:)     :: XUTCI_OUTAGG        ! UTCI outdoor weighted according to sun/shade fractions (only for TEB) 
  REAL, POINTER, DIMENSION(:)     :: XUTCI_OUTSUN_MEAN   ! Mean UTCI for person outdoor at sun
  REAL, POINTER, DIMENSION(:)     :: XUTCI_OUTSHADE_MEAN ! Mean UTCI for person outdoor at shade
  REAL, POINTER, DIMENSION(:)     :: XTRAD_SUN           ! Mean radiant temperature seen by person at sun (K)
  REAL, POINTER, DIMENSION(:)     :: XTRAD_SHADE         ! Mean radiant temperature seen by person in shade (K)
  REAL, POINTER, DIMENSION(:)     :: XTRAD_AGG           ! Mean radiant temperature weighted according to sun/shade fractions
  REAL, POINTER, DIMENSION(:)     :: XTRAD_SUN_MEAN      ! Mean Mean radiant temperature seen by person at sun (K)
  REAL, POINTER, DIMENSION(:)     :: XTRAD_SHADE_MEAN    ! Mean Mean radiant temperature seen by person in shade (K)
  REAL, POINTER, DIMENSION(:,:,:) :: XUTCIC_IN           ! Cumulated UTCI stress for person indoor
  REAL, POINTER, DIMENSION(:,:)   :: XUTCIC_OUTSUN       ! Cumulated UTCI stress for person outdoor at sun
  REAL, POINTER, DIMENSION(:,:)   :: XUTCIC_OUTSHADE     ! Cumulated UTCI stress for person outdoor at shade
  REAL, POINTER, DIMENSION(:,:)   :: XUTCIC_OUTAGG       ! Cumulated UTCI stress for person outdoor aggregation sun shade UTCI
!
END TYPE DIAG_UTCI_t
!



CONTAINS

!




SUBROUTINE DIAG_UTCI_INIT(YDIAG_UTCI)
TYPE(DIAG_UTCI_t), INTENT(INOUT) :: YDIAG_UTCI
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK("MODD_DIAG_UTCI_N:DIAG_UTCI_INIT",0,ZHOOK_HANDLE)
YDIAG_UTCI%LUTCI=.FALSE.
YDIAG_UTCI%NCOUNT_UTCI_STEP=0
  NULLIFY(YDIAG_UTCI%XUTCI_IN)
  NULLIFY(YDIAG_UTCI%XUTCI_OUTSUN)
  NULLIFY(YDIAG_UTCI%XUTCI_OUTSHADE)
  NULLIFY(YDIAG_UTCI%XUTCI_OUTAGG) 
  NULLIFY(YDIAG_UTCI%XUTCI_OUTSUN_MEAN)
  NULLIFY(YDIAG_UTCI%XUTCI_OUTSHADE_MEAN)
  NULLIFY(YDIAG_UTCI%XTRAD_SUN)
  NULLIFY(YDIAG_UTCI%XTRAD_SHADE)
  NULLIFY(YDIAG_UTCI%XTRAD_AGG)
  NULLIFY(YDIAG_UTCI%XTRAD_SUN_MEAN)
  NULLIFY(YDIAG_UTCI%XTRAD_SHADE_MEAN)
  NULLIFY(YDIAG_UTCI%XUTCIC_IN)
  NULLIFY(YDIAG_UTCI%XUTCIC_OUTSUN)
  NULLIFY(YDIAG_UTCI%XUTCIC_OUTSHADE)
  NULLIFY(YDIAG_UTCI%XUTCIC_OUTAGG)
IF (LHOOK) CALL DR_HOOK("MODD_DIAG_UTCI_N:DIAG_UTCI_INIT",1,ZHOOK_HANDLE)
END SUBROUTINE DIAG_UTCI_INIT



END MODULE MODD_DIAG_UTCI_n
