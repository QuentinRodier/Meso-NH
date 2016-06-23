!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
    SUBROUTINE ALLOCATE_TEB_GARDEN (TGD, TVG, &
                                    KLU,KGROUND_LAYER)  
!   ##########################################################################
!
!
!
!
USE MODD_TEB_GARDEN_n, ONLY : TEB_GARDEN_t
USE MODD_TEB_VEG_n, ONLY : TEB_VEG_OPTIONS_t
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!
TYPE(TEB_GARDEN_t), INTENT(INOUT) :: TGD
TYPE(TEB_VEG_OPTIONS_t), INTENT(INOUT) :: TVG
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
ALLOCATE(TGD%CUR%XSNOWFREE_ALB           (KLU))
ALLOCATE(TGD%CUR%XSNOWFREE_ALB_VEG       (KLU))
ALLOCATE(TGD%CUR%XSNOWFREE_ALB_SOIL      (KLU))
!
!-------------------------------------------------------------------------------
!
! Prognostic variables:
!
!
! - Soil and vegetation heat and water:
!
ALLOCATE(TGD%CUR%XWR                     (KLU                    )) 
ALLOCATE(TGD%CUR%XTG                     (KLU,KGROUND_LAYER      )) 
ALLOCATE(TGD%CUR%XWG                     (KLU,KGROUND_LAYER      )) 
ALLOCATE(TGD%CUR%XWGI                    (KLU,KGROUND_LAYER      )) 
ALLOCATE(TGD%CUR%XRESA                   (KLU                    )) 
!
! - Vegetation: Ags Prognostic (YPHOTO = 'LAI', 'LST', 'AGS' or 'LST')
!
ALLOCATE(TGD%CUR%XAN                     (KLU                    )) 
ALLOCATE(TGD%CUR%XANDAY                  (KLU                    )) 
ALLOCATE(TGD%CUR%XANFM                   (KLU                    )) 
ALLOCATE(TGD%CUR%XLE                     (KLU                    ))
!
! - Vegetation (Ags 'NIT' 'NCB' option):
!
ALLOCATE(TGD%CUR%XBIOMASS                (KLU,TVG%NNBIOMASS          ))
ALLOCATE(TGD%CUR%XRESP_BIOMASS           (KLU,TVG%NNBIOMASS          ))
!
IF (LHOOK) CALL DR_HOOK('ALLOCATE_TEB_GARDEN',1,ZHOOK_HANDLE)
!
END SUBROUTINE ALLOCATE_TEB_GARDEN
