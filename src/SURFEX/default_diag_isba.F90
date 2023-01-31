!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE DEFAULT_DIAG_ISBA(K2M,OSURF_BUDGET,O2M_MIN_ZS,ORAD_BUDGET, &
                                   OCOEF,OSURF_VARS,OSURF_EVAP_BUDGET,      &
                                   OSURF_MISC_BUDGET,OSURF_DIAG_ALBEDO,     &
                                   OSURF_BUDGETC,OSURF_MISC_DIF,            &
                                   OPATCH_BUDGET,OUTCI, OPGD,ORESET_BUDGETC,&
                                   OWATER_BUDGET,OENERGY_BUDGET,OPROSNOW,   &
                                   OPROBANDS,OVOLUMETRIC_SNOWLIQ,PDIAG_TSTEP,&
                                   OLUTILES_BUDGET  )
!     #################################################################################################################
!
!!****  *DEFAULT_DIAG_ISBA* - routine to set default values for the choice of diagnostics
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
!!      V. Masson   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2004 
!!      Modified by P. Le Moigne, 11/2004: add budget switch 
!!      Modified by B. Decharme , 06/2009: add patch budget switch 
!!      Modified by A.L. Gibelin, 04/2009: add carbon spinup
!!      Modified by B. Decharme , 05/2012: add miscellaneous field key for dif
!!                                         add isba water budget key
!!      Modif M. Lafaysse 09/2015: OPROSNOW, OVOLUMETRIC_SNOWLIQ
!!      Modif M. Dumont 11/15 : OPROBANDS
!!      Modified by B. Decharme , 08/2016: add isba energy and land use diag key  
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
INTEGER,  INTENT(OUT) :: K2M                ! flag for operational 2m quantities
LOGICAL,  INTENT(OUT) :: OSURF_BUDGET       ! flag for surface budget
LOGICAL,  INTENT(OUT) :: O2M_MIN_ZS
LOGICAL,  INTENT(OUT) :: ORAD_BUDGET        ! flag for radiative budget
LOGICAL,  INTENT(OUT) :: OCOEF
LOGICAL,  INTENT(OUT) :: OSURF_VARS
LOGICAL,  INTENT(OUT) :: OSURF_EVAP_BUDGET  ! flag for surface evaporation budget
LOGICAL,  INTENT(OUT) :: OSURF_MISC_BUDGET  ! flag for surface miscellaneous budget
LOGICAL,  INTENT(OUT) :: OSURF_DIAG_ALBEDO  ! flag for albedo
LOGICAL,  INTENT(OUT) :: OSURF_BUDGETC      ! flag for cumulated surface budget
LOGICAL,  INTENT(OUT) :: OSURF_MISC_DIF     ! flag for surface miscellaneous dif variables
LOGICAL,  INTENT(OUT) :: OPATCH_BUDGET      ! flag for patch output
LOGICAL,  INTENT(OUT) :: OLUTILES_BUDGET    ! flag for land use tiles diag
LOGICAL,  INTENT(OUT) :: OUTCI              ! flag for UTCI fields
LOGICAL,  INTENT(OUT) :: OPGD               ! flag for PGD fields
LOGICAL,  INTENT(OUT) :: ORESET_BUDGETC     ! flag for cumulated surface budget
LOGICAL,  INTENT(OUT) :: OWATER_BUDGET      ! flag for isba water budget
LOGICAL,  INTENT(OUT) :: OPROSNOW           ! flag for Crocus-MEPRA diagnostics
LOGICAL, INTENT(OUT)  :: OPROBANDS          ! flag for Crocus spectral output
LOGICAL,  INTENT(OUT) :: OVOLUMETRIC_SNOWLIQ ! volumetric snow liquid water content (kg m-3)
LOGICAL,  INTENT(OUT) :: OENERGY_BUDGET     ! flag for isba energy budget
REAL,     INTENT(OUT) :: PDIAG_TSTEP        ! time-step for writing
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('DEFAULT_DIAG_ISBA',0,ZHOOK_HANDLE)
K2M               = 0
OSURF_BUDGET      = .FALSE.
!
O2M_MIN_ZS        = .FALSE.
ORAD_BUDGET       = .FALSE.
!
OCOEF             = .FALSE.
OSURF_VARS        = .FALSE.
!
OSURF_EVAP_BUDGET = .FALSE.
OSURF_MISC_BUDGET = .FALSE.
OSURF_MISC_DIF    = .FALSE.
!
OSURF_DIAG_ALBEDO = .FALSE.
!
OSURF_BUDGETC     = .FALSE.
!
OPATCH_BUDGET     = .TRUE.
!
OUTCI             = .FALSE.
!
OPGD              = .FALSE.
ORESET_BUDGETC    = .FALSE.
!
OWATER_BUDGET     = .FALSE.
OENERGY_BUDGET    = .FALSE.
!
OPROSNOW          = .FALSE.
OPROBANDS         = .FALSE.
!
OVOLUMETRIC_SNOWLIQ = .FALSE.
!
OLUTILES_BUDGET   = .FALSE.
!
PDIAG_TSTEP       = XUNDEF
!
IF (LHOOK) CALL DR_HOOK('DEFAULT_DIAG_ISBA',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE DEFAULT_DIAG_ISBA
