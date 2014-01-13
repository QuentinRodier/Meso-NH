!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
      SUBROUTINE DEFAULT_SEAFLUX(PTSTEP,POUT_TSTEP,HSEA_ALB,HSEA_FLUX, &
                                   OPWG, OPRECIP, OPWEBB, KGRVWAVES,     &
                                   OPROGSST, KTIME_COUPLING,             &
                                   PICHCE, HINTERPOL_SST       )  
!     ########################################################################
!
!!****  *DEFAULT_SEAFLUX* - routine to set default values for the configuration for SEAFLUX scheme
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
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
USE MODD_SURF_PAR,   ONLY : XUNDEF
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
!
REAL,              INTENT(OUT) :: PTSTEP        ! time step for run
REAL,              INTENT(OUT) :: POUT_TSTEP    ! time step for writing
 CHARACTER(LEN=6),  INTENT(OUT) :: HSEA_FLUX     ! type of sea scheme
 CHARACTER(LEN=4),  INTENT(OUT) :: HSEA_ALB      ! type of sea albedo
LOGICAL,           INTENT(OUT) :: OPWG          ! gustiness impact
LOGICAL,           INTENT(OUT) :: OPRECIP       ! precipitation correction
LOGICAL,           INTENT(OUT) :: OPWEBB        ! Webb correction
INTEGER,           INTENT(OUT) :: KGRVWAVES     ! Wave gravity in roughness length
LOGICAL,           INTENT(OUT) :: OPROGSST      !two-way coupling
INTEGER,           INTENT(OUT) :: KTIME_COUPLING!coupling frequency
REAL,              INTENT(OUT) :: PICHCE        !CE coef calculation for ECUME
 CHARACTER(LEN=6),  INTENT(OUT) :: HINTERPOL_SST ! Quadratic interpolation of monthly SST
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('DEFAULT_SEAFLUX',0,ZHOOK_HANDLE)
PTSTEP     = XUNDEF
POUT_TSTEP = XUNDEF
!
HSEA_FLUX = "ECUME "
HSEA_ALB  = "TA96"
!
OPWG    = .FALSE.
OPRECIP = .FALSE. 
OPWEBB  = .FALSE.
!
KGRVWAVES = 0
!
OPROGSST = .FALSE.
KTIME_COUPLING = 300
!
PICHCE = 0.0
!
HINTERPOL_SST = "NONE"
IF (LHOOK) CALL DR_HOOK('DEFAULT_SEAFLUX',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE DEFAULT_SEAFLUX
