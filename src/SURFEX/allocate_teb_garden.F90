!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
    SUBROUTINE ALLOCATE_TEB_GARDEN(KLU,KGROUND_LAYER)  
!   ##########################################################################
!
USE MODD_TEB_VEG_n, ONLY : NNBIOMASS
USE MODD_TEB_GARDEN_n
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
INTEGER, INTENT(IN) :: KLU
INTEGER, INTENT(IN) :: KGROUND_LAYER
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
! Mask and number of grid elements containing patches/tiles:
!
IF (LHOOK) CALL DR_HOOK('ALLOCATE_TEB_GARDEN',0,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
! Averaged Surface radiative parameters:
!
ALLOCATE(XSNOWFREE_ALB           (KLU))
ALLOCATE(XSNOWFREE_ALB_VEG       (KLU))
ALLOCATE(XSNOWFREE_ALB_SOIL      (KLU))
!
!-------------------------------------------------------------------------------
!
! Prognostic variables:
!
!
! - Soil and vegetation heat and water:
!
ALLOCATE(XWR                     (KLU                    )) 
ALLOCATE(XTG                     (KLU,KGROUND_LAYER      )) 
ALLOCATE(XWG                     (KLU,KGROUND_LAYER      )) 
ALLOCATE(XWGI                    (KLU,KGROUND_LAYER      )) 
ALLOCATE(XRESA                   (KLU                    )) 
!
! - Vegetation: Ags Prognostic (YPHOTO = 'LAI', 'LST', 'AGS' or 'LST')
!
ALLOCATE(XAN                     (KLU                    )) 
ALLOCATE(XANDAY                  (KLU                    )) 
ALLOCATE(XANFM                   (KLU                    )) 
ALLOCATE(XLE                     (KLU                    ))
!
! - Vegetation (Ags 'NIT' 'NCB' option):
!
ALLOCATE(XBIOMASS                (KLU,NNBIOMASS          ))
ALLOCATE(XRESP_BIOMASS           (KLU,NNBIOMASS          ))
!
IF (LHOOK) CALL DR_HOOK('ALLOCATE_TEB_GARDEN',1,ZHOOK_HANDLE)
!
END SUBROUTINE ALLOCATE_TEB_GARDEN
