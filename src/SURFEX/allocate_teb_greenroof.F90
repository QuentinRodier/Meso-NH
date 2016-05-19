!     #########
    SUBROUTINE ALLOCATE_TEB_GREENROOF(KLU,KLAYER_GR)
!   ##########################################################################
!
USE MODD_TEB_VEG_n, ONLY : NNBIOMASS
USE MODD_TEB_GREENROOF_n
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
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
ALLOCATE(XSNOWFREE_ALB           (KLU))
ALLOCATE(XSNOWFREE_ALB_VEG       (KLU))
ALLOCATE(XSNOWFREE_ALB_SOIL      (KLU))
!
!-------------------------------------------------------------------------------
!
! Prognostic variables:
!
! - Soil and vegetation heat and water:
!
ALLOCATE(XWR                     (KLU                     )) 
ALLOCATE(XTG                     (KLU,KLAYER_GR       )) 
ALLOCATE(XWG                     (KLU,KLAYER_GR       )) 
ALLOCATE(XWGI                    (KLU,KLAYER_GR       )) 
ALLOCATE(XRESA                   (KLU                     )) 
!
! - Vegetation: Ags Prognostic (YPHOTO = 'LAI', 'LST', 'AGS' or 'LST')
!
ALLOCATE(XAN                     (KLU                     )) 
ALLOCATE(XANDAY                  (KLU                     )) 
ALLOCATE(XANFM                   (KLU                     )) 
ALLOCATE(XLE                     (KLU                     ))
!
! - Vegetation (Ags 'NIT' 'NCB' option):
!
ALLOCATE(XBIOMASS                (KLU,NNBIOMASS           ))
ALLOCATE(XRESP_BIOMASS           (KLU,NNBIOMASS           ))
!
IF (LHOOK) CALL DR_HOOK('ALLOCATE_TEB_GREENROOF',1,ZHOOK_HANDLE)
!
END SUBROUTINE ALLOCATE_TEB_GREENROOF
