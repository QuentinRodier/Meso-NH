!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
      SUBROUTINE PREP_CTRL_WATFLUX(K2M,OSURF_BUDGET,O2M_MIN_ZS,ORAD_BUDGET,OCOEF,OSURF_VARS,&
                                  KLUOUT,OSURF_BUDGETC)  
!     #################################################################################################################
!
!!****  *PREP_CTRL_WATFLUX* - routine to check that diagnostics are switched off
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
!!	P. Le Moigne   *Meteo France*	
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    04/2007 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
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
INTEGER,  INTENT(INOUT) :: K2M                ! flag for 2m parameters
LOGICAL,  INTENT(INOUT) :: OSURF_BUDGET       ! flag for surface budget
LOGICAL,  INTENT(INOUT) :: O2M_MIN_ZS         ! flag for 2m parameters at min zs
LOGICAL,  INTENT(INOUT) :: ORAD_BUDGET        ! flag for radiative budget
LOGICAL,  INTENT(INOUT) :: OCOEF              ! flag for turbulent coefficients
LOGICAL,  INTENT(INOUT) :: OSURF_VARS         ! flag for other surface variables
INTEGER,  INTENT(IN)    :: KLUOUT             ! unit number
LOGICAL,  INTENT(INOUT) :: OSURF_BUDGETC      ! flag for cumulated surface budget
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('PREP_CTRL_WATFLUX',0,ZHOOK_HANDLE)
K2M = 0
!
OSURF_BUDGET  = .FALSE.
O2M_MIN_ZS    = .FALSE.
ORAD_BUDGET   = .FALSE.
OCOEF         = .FALSE.
OSURF_VARS    = .FALSE.
OSURF_BUDGETC = .FALSE.
!
WRITE(KLUOUT,*)'WATFLUX DIAGNOSTICS DESACTIVATED'
IF (LHOOK) CALL DR_HOOK('PREP_CTRL_WATFLUX',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE PREP_CTRL_WATFLUX
