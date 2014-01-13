!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #################################################################################
SUBROUTINE DEALLOC_SURF_ATM_n
!     #################################################################################
!
!!****  *DEALLOC_SURF_ATM_n * - Deallocate all arrays
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
USE MODD_SURF_ATM_n,          ONLY : XNATURE, XTOWN, XSEA, XWATER,                &
                                       LCOVER, XCOVER, XZS,                         &
                                       NR_NATURE, NR_SEA, NR_TOWN, NR_WATER,        &
                                       NDIM_SEA, NDIM_WATER, NDIM_NATURE, NDIM_TOWN  
!
USE MODD_SURF_ATM_GRID_n,     ONLY : XGRID_PAR, XLAT, XLON, XMESH_SIZE, XJPDIR
USE MODD_SURF_ATM_SSO_n,      ONLY : XAOSIP, XAOSIM, XAOSJP, XAOSJM,      &
                                       XHO2IP, XHO2IM, XHO2JP, XHO2JM,      &
                                       XZ0REL, XSSO_SLOPE, XSSO_ANIS,       &
                                       XSSO_DIR, XSSO_STDEV,                &
                                       XAVG_ZS, XSIL_ZS, XMAX_ZS, XMIN_ZS  
USE MODD_CH_EMIS_FIELD_n,     ONLY : CEMIS_NAME, CEMIS_AREA, NEMIS_TIME, &
                                       TSEMISS, XEMIS_FIELDS, CEMIS_COMMENT  
USE MODD_DUMMY_SURF_FIELDS_n, ONLY : XDUMMY_FIELDS
USE MODD_CH_SURF_n,           ONLY : CCH_NAMES
USE MODD_SV_n,                ONLY : CSV
!
USE MODI_DEALLOC_SEA_n
USE MODI_DEALLOC_INLAND_WATER_n
USE MODI_DEALLOC_NATURE_n
USE MODI_DEALLOC_TOWN_n
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

IF (LHOOK) CALL DR_HOOK('DEALLOC_SURF_ATM_N',0,ZHOOK_HANDLE)
IF (ASSOCIATED(XNATURE)) DEALLOCATE(XNATURE)
IF (ASSOCIATED(XTOWN  )) DEALLOCATE(XTOWN  )
IF (ASSOCIATED(XWATER )) DEALLOCATE(XWATER )
IF (ASSOCIATED(XSEA   )) DEALLOCATE(XSEA   )
!
IF (ASSOCIATED(LCOVER )) DEALLOCATE(LCOVER )
IF (ASSOCIATED(XCOVER )) DEALLOCATE(XCOVER )
IF (ASSOCIATED(XZS    )) DEALLOCATE(XZS    )
!
IF (ASSOCIATED(NR_NATURE)) DEALLOCATE(NR_NATURE)
IF (ASSOCIATED(NR_TOWN  )) DEALLOCATE(NR_TOWN  )
IF (ASSOCIATED(NR_WATER )) DEALLOCATE(NR_WATER )
IF (ASSOCIATED(NR_SEA   )) DEALLOCATE(NR_SEA   )
!
!-------------------------------------------------------------------------------------
!
IF (ASSOCIATED(XGRID_PAR )) DEALLOCATE(XGRID_PAR )
IF (ASSOCIATED(XLAT      )) DEALLOCATE(XLAT      )
IF (ASSOCIATED(XLON      )) DEALLOCATE(XLON      )
IF (ASSOCIATED(XMESH_SIZE)) DEALLOCATE(XMESH_SIZE)
IF (ASSOCIATED(XJPDIR    )) DEALLOCATE(XJPDIR    )
!
!-------------------------------------------------------------------------------------
!
IF (ASSOCIATED(XAOSIP))     DEALLOCATE( XAOSIP)
IF (ASSOCIATED(XAOSIM))     DEALLOCATE( XAOSIM)
IF (ASSOCIATED(XAOSJP))     DEALLOCATE( XAOSJP)
IF (ASSOCIATED(XAOSJM))     DEALLOCATE( XAOSJM)
IF (ASSOCIATED(XHO2IP))     DEALLOCATE( XHO2IP)
IF (ASSOCIATED(XHO2IM))     DEALLOCATE( XHO2IM)
IF (ASSOCIATED(XHO2JP))     DEALLOCATE( XHO2JP)
IF (ASSOCIATED(XHO2JM))     DEALLOCATE( XHO2JM)
IF (ASSOCIATED(XZ0REL))     DEALLOCATE( XZ0REL)
IF (ASSOCIATED(XSSO_SLOPE)) DEALLOCATE( XSSO_SLOPE)
IF (ASSOCIATED(XSSO_ANIS))  DEALLOCATE( XSSO_ANIS)
IF (ASSOCIATED(XSSO_DIR))   DEALLOCATE( XSSO_DIR)
IF (ASSOCIATED(XSSO_STDEV)) DEALLOCATE( XSSO_STDEV)
IF (ASSOCIATED(XAVG_ZS))    DEALLOCATE( XAVG_ZS)
IF (ASSOCIATED(XSIL_ZS))    DEALLOCATE( XSIL_ZS)
IF (ASSOCIATED(XMAX_ZS))    DEALLOCATE( XMAX_ZS)
IF (ASSOCIATED(XMIN_ZS))    DEALLOCATE( XMIN_ZS)
!
!-------------------------------------------------------------------------------------
!
IF (ASSOCIATED(CEMIS_AREA))   DEALLOCATE(CEMIS_AREA)
IF (ASSOCIATED(CEMIS_COMMENT))DEALLOCATE(CEMIS_COMMENT)
IF (ASSOCIATED(CEMIS_NAME))   DEALLOCATE(CEMIS_NAME)
IF (ASSOCIATED(NEMIS_TIME))   DEALLOCATE(NEMIS_TIME)
IF (ASSOCIATED(XEMIS_FIELDS)) DEALLOCATE(XEMIS_FIELDS)
IF (ASSOCIATED(TSEMISS))      DEALLOCATE(TSEMISS)
!
!-------------------------------------------------------------------------------------
!
IF (ASSOCIATED(XDUMMY_FIELDS)) DEALLOCATE( XDUMMY_FIELDS)
!
!-------------------------------------------------------------------------------------
!
IF (ASSOCIATED(CSV)) DEALLOCATE(CSV)
!
!-------------------------------------------------------------------------------------
!
IF (ASSOCIATED(CCH_NAMES))  DEALLOCATE(CCH_NAMES)
IF (ASSOCIATED(CEMIS_NAME)) DEALLOCATE(CEMIS_NAME)
IF (ASSOCIATED(CEMIS_AREA)) DEALLOCATE(CEMIS_AREA)
IF (ASSOCIATED(NEMIS_TIME)) DEALLOCATE(NEMIS_TIME )
IF (ASSOCIATED(TSEMISS))    DEALLOCATE(TSEMISS)
!
!-------------------------------------------------------------------------------------
!
IF (NDIM_SEA    >0) CALL DEALLOC_SEA_n
IF (NDIM_WATER  >0) CALL DEALLOC_INLAND_WATER_n
IF (NDIM_NATURE >0) CALL DEALLOC_NATURE_n
IF (NDIM_TOWN   >0) CALL DEALLOC_TOWN_n
IF (LHOOK) CALL DR_HOOK('DEALLOC_SURF_ATM_N',1,ZHOOK_HANDLE)
!
!--------------------------------------------------------------------------------------
!
END SUBROUTINE DEALLOC_SURF_ATM_n
