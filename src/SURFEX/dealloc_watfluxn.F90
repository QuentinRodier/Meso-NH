!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #################################################################################
SUBROUTINE DEALLOC_WATFLUX_n (CHW, WG, W)
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


!
!
USE MODD_CH_WATFLUX_n, ONLY : CH_WATFLUX_t
USE MODD_WATFLUX_GRID_n, ONLY : WATFLUX_GRID_t
USE MODD_WATFLUX_n, ONLY : WATFLUX_t
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
TYPE(CH_WATFLUX_t), INTENT(INOUT) :: CHW
TYPE(WATFLUX_GRID_t), INTENT(INOUT) :: WG
TYPE(WATFLUX_t), INTENT(INOUT) :: W
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('DEALLOC_WATFLUX_N',0,ZHOOK_HANDLE)
IF (ASSOCIATED(W%LCOVER ))  DEALLOCATE(W%LCOVER )
IF (ASSOCIATED(W%XCOVER ))  DEALLOCATE(W%XCOVER )
IF (ASSOCIATED(W%XZS    ))  DEALLOCATE(W%XZS    )
IF (ASSOCIATED(W%XTS    ))  DEALLOCATE(W%XTS    )
IF (ASSOCIATED(W%XZ0    ))  DEALLOCATE(W%XZ0    )
IF (ASSOCIATED(W%XEMIS  ))  DEALLOCATE(W%XEMIS  )
!
IF (ASSOCIATED(W%XDIR_ALB))  DEALLOCATE(W%XDIR_ALB)
IF (ASSOCIATED(W%XSCA_ALB))  DEALLOCATE(W%XSCA_ALB)
!
!-------------------------------------------------------------------------------------
!
IF (ASSOCIATED(WG%XGRID_PAR )) DEALLOCATE(WG%XGRID_PAR )
IF (ASSOCIATED(WG%XLAT      )) DEALLOCATE(WG%XLAT      )
IF (ASSOCIATED(WG%XLON      )) DEALLOCATE(WG%XLON      )
IF (ASSOCIATED(WG%XMESH_SIZE)) DEALLOCATE(WG%XMESH_SIZE)
!
!-------------------------------------------------------------------------------------
!
IF(ASSOCIATED(CHW%XDEP))      DEALLOCATE(CHW%XDEP)
IF(ASSOCIATED(CHW%CCH_NAMES)) DEALLOCATE(CHW%CCH_NAMES)
IF(ASSOCIATED(CHW%SVW%CSV))       DEALLOCATE(CHW%SVW%CSV)
!
!-------------------------------------------------------------------------------------
!
IF(ASSOCIATED(W%XCPL_WATER_WIND))      DEALLOCATE(W%XCPL_WATER_WIND)
IF(ASSOCIATED(W%XCPL_WATER_FWSU))      DEALLOCATE(W%XCPL_WATER_FWSU)
IF(ASSOCIATED(W%XCPL_WATER_FWSV))      DEALLOCATE(W%XCPL_WATER_FWSV)
IF(ASSOCIATED(W%XCPL_WATER_SNET))      DEALLOCATE(W%XCPL_WATER_SNET)
IF(ASSOCIATED(W%XCPL_WATER_HEAT))      DEALLOCATE(W%XCPL_WATER_HEAT)
IF(ASSOCIATED(W%XCPL_WATER_EVAP))      DEALLOCATE(W%XCPL_WATER_EVAP)
IF(ASSOCIATED(W%XCPL_WATER_RAIN))      DEALLOCATE(W%XCPL_WATER_RAIN)
IF(ASSOCIATED(W%XCPL_WATER_SNOW))      DEALLOCATE(W%XCPL_WATER_SNOW)
IF (LHOOK) CALL DR_HOOK('DEALLOC_WATFLUX_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE DEALLOC_WATFLUX_n


