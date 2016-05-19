!     #################################################################################
SUBROUTINE DEALLOC_WATFLUX_n
!     #################################################################################
!
!!****  *DEALLOC_WATFLUX_n * - Deallocate all arrays
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
USE MODD_WATFLUX_n,      ONLY : LCOVER, XCOVER, XZS, XTS, XZ0, &
                                  XEMIS, XDIR_ALB, XSCA_ALB,     &
                                  XCPL_WATER_WIND,               &
                                  XCPL_WATER_FWSU,               &
                                  XCPL_WATER_FWSV,               &
                                  XCPL_WATER_SNET,               &
                                  XCPL_WATER_HEAT,               &
                                  XCPL_WATER_EVAP,               &
                                  XCPL_WATER_RAIN,               &
                                  XCPL_WATER_SNOW  
USE MODD_WATFLUX_GRID_n, ONLY : XGRID_PAR, XLAT, XLON, XMESH_SIZE
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

IF (LHOOK) CALL DR_HOOK('DEALLOC_WATFLUX_N',0,ZHOOK_HANDLE)
IF (ASSOCIATED(LCOVER ))  DEALLOCATE(LCOVER )
IF (ASSOCIATED(XCOVER ))  DEALLOCATE(XCOVER )
IF (ASSOCIATED(XZS    ))  DEALLOCATE(XZS    )
IF (ASSOCIATED(XTS    ))  DEALLOCATE(XTS    )
IF (ASSOCIATED(XZ0    ))  DEALLOCATE(XZ0    )
IF (ASSOCIATED(XEMIS  ))  DEALLOCATE(XEMIS  )
!
IF (ASSOCIATED(XDIR_ALB))  DEALLOCATE(XDIR_ALB)
IF (ASSOCIATED(XSCA_ALB))  DEALLOCATE(XSCA_ALB)
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
!
!-------------------------------------------------------------------------------------
!
IF(ASSOCIATED(XCPL_WATER_WIND))      DEALLOCATE(XCPL_WATER_WIND)
IF(ASSOCIATED(XCPL_WATER_FWSU))      DEALLOCATE(XCPL_WATER_FWSU)
IF(ASSOCIATED(XCPL_WATER_FWSV))      DEALLOCATE(XCPL_WATER_FWSV)
IF(ASSOCIATED(XCPL_WATER_SNET))      DEALLOCATE(XCPL_WATER_SNET)
IF(ASSOCIATED(XCPL_WATER_HEAT))      DEALLOCATE(XCPL_WATER_HEAT)
IF(ASSOCIATED(XCPL_WATER_EVAP))      DEALLOCATE(XCPL_WATER_EVAP)
IF(ASSOCIATED(XCPL_WATER_RAIN))      DEALLOCATE(XCPL_WATER_RAIN)
IF(ASSOCIATED(XCPL_WATER_SNOW))      DEALLOCATE(XCPL_WATER_SNOW)
IF (LHOOK) CALL DR_HOOK('DEALLOC_WATFLUX_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE DEALLOC_WATFLUX_n


