!     #################################################################################
SUBROUTINE DEALLOC_FLAKE_n
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
!!------------------------------------------------------------------
!

!
USE MODD_FLAKE_n,      ONLY : LCOVER          , XCOVER        , &
                                XZS           , XEMIS         , &
                                XWATER_DEPTH  , XWATER_FETCH  , &
                                XT_BS         , XDEPTH_BS     , &
                                XCORIO        , XDIR_ALB      , &
                                XSCA_ALB      , XICE_ALB      , &
                                XSNOW_ALB     , XEXTCOEF_WATER, &
                                XEXTCOEF_ICE  , XEXTCOEF_SNOW , &
                                XT_SNOW       , XT_ICE        , &
                                XT_MNW        , XT_WML        , &
                                XT_BOT        , XT_B1         , &
                                XCT           , XH_SNOW       , &
                                XH_ICE        , XH_ML         , &
                                XH_B1         , XTS             
USE MODD_FLAKE_GRID_n, ONLY : XGRID_PAR, XLAT, XLON, XMESH_SIZE
USE MODD_CH_WATFLUX_n,   ONLY : XDEP, CCH_NAMES, CSV


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

REAL(KIND=JPRB) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('DEALLOC_FLAKE_N',0,ZHOOK_HANDLE)
IF (ASSOCIATED(LCOVER ))  DEALLOCATE(LCOVER )
IF (ASSOCIATED(XCOVER ))  DEALLOCATE(XCOVER )
IF (ASSOCIATED(XZS    ))  DEALLOCATE(XZS    )
IF (ASSOCIATED(XEMIS         ))  DEALLOCATE(XEMIS         )
IF (ASSOCIATED(XWATER_DEPTH  ))  DEALLOCATE(XWATER_DEPTH  )
IF (ASSOCIATED(XWATER_FETCH  ))  DEALLOCATE(XWATER_FETCH  )
IF (ASSOCIATED(XT_BS         ))  DEALLOCATE(XT_BS         )
IF (ASSOCIATED(XDEPTH_BS     ))  DEALLOCATE(XDEPTH_BS     )
IF (ASSOCIATED(XCORIO        ))  DEALLOCATE(XCORIO        )
IF (ASSOCIATED(XDIR_ALB      ))  DEALLOCATE(XDIR_ALB      )
IF (ASSOCIATED(XSCA_ALB      ))  DEALLOCATE(XSCA_ALB      )
IF (ASSOCIATED(XICE_ALB      ))  DEALLOCATE(XICE_ALB      )
IF (ASSOCIATED(XSNOW_ALB     ))  DEALLOCATE(XSNOW_ALB     )
IF (ASSOCIATED(XEXTCOEF_WATER))  DEALLOCATE(XEXTCOEF_WATER)
IF (ASSOCIATED(XEXTCOEF_ICE  ))  DEALLOCATE(XEXTCOEF_ICE  )
IF (ASSOCIATED(XEXTCOEF_SNOW ))  DEALLOCATE(XEXTCOEF_SNOW )
IF (ASSOCIATED(XT_SNOW       ))  DEALLOCATE(XT_SNOW       )
IF (ASSOCIATED(XT_ICE        ))  DEALLOCATE(XT_ICE        )
IF (ASSOCIATED(XT_MNW        ))  DEALLOCATE(XT_MNW        )
IF (ASSOCIATED(XT_WML        ))  DEALLOCATE(XT_WML        )
IF (ASSOCIATED(XT_BOT        ))  DEALLOCATE(XT_BOT        )
IF (ASSOCIATED(XT_B1         ))  DEALLOCATE(XT_B1         )
IF (ASSOCIATED(XCT           ))  DEALLOCATE(XCT           )
IF (ASSOCIATED(XH_SNOW       ))  DEALLOCATE(XH_SNOW       )
IF (ASSOCIATED(XH_ICE        ))  DEALLOCATE(XH_ICE        )
IF (ASSOCIATED(XH_ML         ))  DEALLOCATE(XH_ML         )
IF (ASSOCIATED(XH_B1         ))  DEALLOCATE(XH_B1         )
IF (ASSOCIATED(XTS           ))  DEALLOCATE(XTS           )
!
!-------------------------------------------------------------------------------------
!
IF (ASSOCIATED(XGRID_PAR )) DEALLOCATE(XGRID_PAR )
IF (ASSOCIATED(XLAT      )) DEALLOCATE(XLAT      )
IF (ASSOCIATED(XLON      )) DEALLOCATE(XLON      )
IF (ASSOCIATED(XMESH_SIZE)) DEALLOCATE(XMESH_SIZE)
!
!-------------------------------------------------------------------------------------
!
IF(ASSOCIATED(XDEP))      DEALLOCATE(XDEP)
IF(ASSOCIATED(CCH_NAMES)) DEALLOCATE(CCH_NAMES)
IF(ASSOCIATED(CSV))       DEALLOCATE(CSV)
IF (LHOOK) CALL DR_HOOK('DEALLOC_FLAKE_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE DEALLOC_FLAKE_n


