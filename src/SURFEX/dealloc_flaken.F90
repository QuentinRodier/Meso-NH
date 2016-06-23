!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #################################################################################
SUBROUTINE DEALLOC_FLAKE_n (CHF, FG, F)
!     #################################################################################
!
!!****  *DEALLOC_FLAKE_n * - Deallocate all arrays
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
!!     V. Masson 
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2004
!!      Modified    04/2013, P. Le Moigne: FLake chemistry and XZ0
!!------------------------------------------------------------------
!

!


!
!
USE MODD_CH_FLAKE_n, ONLY : CH_FLAKE_t
USE MODD_FLAKE_GRID_n, ONLY : FLAKE_GRID_t
USE MODD_FLAKE_n, ONLY : FLAKE_t
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
!*      0.2    declarations of local variables
!
!-------------------------------------------------------------------------------------
!

!
TYPE(CH_FLAKE_t), INTENT(INOUT) :: CHF
TYPE(FLAKE_GRID_t), INTENT(INOUT) :: FG
TYPE(FLAKE_t), INTENT(INOUT) :: F
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('DEALLOC_FLAKE_N',0,ZHOOK_HANDLE)
IF (ASSOCIATED(F%LCOVER ))  DEALLOCATE(F%LCOVER )
IF (ASSOCIATED(F%XCOVER ))  DEALLOCATE(F%XCOVER )
IF (ASSOCIATED(F%XZS    ))  DEALLOCATE(F%XZS    )
IF (ASSOCIATED(F%XEMIS         ))  DEALLOCATE(F%XEMIS         )
IF (ASSOCIATED(F%XWATER_DEPTH  ))  DEALLOCATE(F%XWATER_DEPTH  )
IF (ASSOCIATED(F%XWATER_FETCH  ))  DEALLOCATE(F%XWATER_FETCH  )
IF (ASSOCIATED(F%XT_BS         ))  DEALLOCATE(F%XT_BS         )
IF (ASSOCIATED(F%XDEPTH_BS     ))  DEALLOCATE(F%XDEPTH_BS     )
IF (ASSOCIATED(F%XCORIO        ))  DEALLOCATE(F%XCORIO        )
IF (ASSOCIATED(F%XDIR_ALB      ))  DEALLOCATE(F%XDIR_ALB      )
IF (ASSOCIATED(F%XSCA_ALB      ))  DEALLOCATE(F%XSCA_ALB      )
IF (ASSOCIATED(F%XICE_ALB      ))  DEALLOCATE(F%XICE_ALB      )
IF (ASSOCIATED(F%XSNOW_ALB     ))  DEALLOCATE(F%XSNOW_ALB     )
IF (ASSOCIATED(F%XEXTCOEF_WATER))  DEALLOCATE(F%XEXTCOEF_WATER)
IF (ASSOCIATED(F%XEXTCOEF_ICE  ))  DEALLOCATE(F%XEXTCOEF_ICE  )
IF (ASSOCIATED(F%XEXTCOEF_SNOW ))  DEALLOCATE(F%XEXTCOEF_SNOW )
IF (ASSOCIATED(F%XT_SNOW       ))  DEALLOCATE(F%XT_SNOW       )
IF (ASSOCIATED(F%XT_ICE        ))  DEALLOCATE(F%XT_ICE        )
IF (ASSOCIATED(F%XT_MNW        ))  DEALLOCATE(F%XT_MNW        )
IF (ASSOCIATED(F%XT_WML        ))  DEALLOCATE(F%XT_WML        )
IF (ASSOCIATED(F%XT_BOT        ))  DEALLOCATE(F%XT_BOT        )
IF (ASSOCIATED(F%XT_B1         ))  DEALLOCATE(F%XT_B1         )
IF (ASSOCIATED(F%XCT           ))  DEALLOCATE(F%XCT           )
IF (ASSOCIATED(F%XH_SNOW       ))  DEALLOCATE(F%XH_SNOW       )
IF (ASSOCIATED(F%XH_ICE        ))  DEALLOCATE(F%XH_ICE        )
IF (ASSOCIATED(F%XH_ML         ))  DEALLOCATE(F%XH_ML         )
IF (ASSOCIATED(F%XH_B1         ))  DEALLOCATE(F%XH_B1         )
IF (ASSOCIATED(F%XTS           ))  DEALLOCATE(F%XTS           )
IF (ASSOCIATED(F%XZ0           ))  DEALLOCATE(F%XZ0           )
!
!-------------------------------------------------------------------------------------
!
IF (ASSOCIATED(FG%XGRID_PAR )) DEALLOCATE(FG%XGRID_PAR )
IF (ASSOCIATED(FG%XLAT      )) DEALLOCATE(FG%XLAT      )
IF (ASSOCIATED(FG%XLON      )) DEALLOCATE(FG%XLON      )
IF (ASSOCIATED(FG%XMESH_SIZE)) DEALLOCATE(FG%XMESH_SIZE)
!
!-------------------------------------------------------------------------------------
!
IF(ASSOCIATED(CHF%XDEP))      DEALLOCATE(CHF%XDEP)
IF(ASSOCIATED(CHF%CCH_NAMES)) DEALLOCATE(CHF%CCH_NAMES)
IF(ASSOCIATED(CHF%SVF%CSV))       DEALLOCATE(CHF%SVF%CSV)
IF (LHOOK) CALL DR_HOOK('DEALLOC_FLAKE_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE DEALLOC_FLAKE_n


