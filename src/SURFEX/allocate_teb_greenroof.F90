!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
    SUBROUTINE ALLOCATE_TEB_GREENROOF (TGR, TVG, &
                                       KLU,KLAYER_GR)
!   ##########################################################################
!
!
!
!
USE MODD_TEB_GREENROOF_n, ONLY : TEB_GREENROOF_t
USE MODD_TEB_VEG_n, ONLY : TEB_VEG_OPTIONS_t
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!
TYPE(TEB_GREENROOF_t), INTENT(INOUT) :: TGR
TYPE(TEB_VEG_OPTIONS_t), INTENT(INOUT) :: TVG
!
INTEGER, INTENT(IN) :: KLU
INTEGER, INTENT(IN) :: KLAYER_GR
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
! Mask and number of grid elements containing patches/tiles:
!
IF (LHOOK) CALL DR_HOOK('ALLOCATE_TEB_GREENROOF',0,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
! Averaged Surface radiative parameters:
!
ALLOCATE(TGR%CUR%XSNOWFREE_ALB           (KLU))
ALLOCATE(TGR%CUR%XSNOWFREE_ALB_VEG       (KLU))
ALLOCATE(TGR%CUR%XSNOWFREE_ALB_SOIL      (KLU))
!
!-------------------------------------------------------------------------------
!
! Prognostic variables:
!
! - Soil and vegetation heat and water:
!
ALLOCATE(TGR%CUR%XWR                     (KLU                     )) 
ALLOCATE(TGR%CUR%XTG                     (KLU,KLAYER_GR       )) 
ALLOCATE(TGR%CUR%XWG                     (KLU,KLAYER_GR       )) 
ALLOCATE(TGR%CUR%XWGI                    (KLU,KLAYER_GR       )) 
ALLOCATE(TGR%CUR%XRESA                   (KLU                     )) 
!
! - Vegetation: Ags Prognostic (YPHOTO = 'LAI', 'LST', 'AGS' or 'LST')
!
ALLOCATE(TGR%CUR%XAN                     (KLU                     )) 
ALLOCATE(TGR%CUR%XANDAY                  (KLU                     )) 
ALLOCATE(TGR%CUR%XANFM                   (KLU                     )) 
ALLOCATE(TGR%CUR%XLE                     (KLU                     ))
!
! - Vegetation (Ags 'NIT' 'NCB' option):
!
ALLOCATE(TGR%CUR%XBIOMASS                (KLU,TVG%NNBIOMASS           ))
ALLOCATE(TGR%CUR%XRESP_BIOMASS           (KLU,TVG%NNBIOMASS           ))
!
IF (LHOOK) CALL DR_HOOK('ALLOCATE_TEB_GREENROOF',1,ZHOOK_HANDLE)
!
END SUBROUTINE ALLOCATE_TEB_GREENROOF
