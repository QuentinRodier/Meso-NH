!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
    SUBROUTINE ALLOCATE_TEB_GARDEN_PGD (TGDPE, TGDP, &
                                        OALLOC,KLU,KVEGTYPE,KGROUND_LAYER, KDIMTAB)  
!   ##########################################################################
!
!
!
!
USE MODD_TEB_GARDEN_PGD_EVOL_n, ONLY : TEB_GARDEN_PGD_EVOL_t
USE MODD_TEB_GARDEN_PGD_n, ONLY : TEB_GARDEN_PGD_t
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!
TYPE(TEB_GARDEN_PGD_EVOL_t), INTENT(INOUT) :: TGDPE
TYPE(TEB_GARDEN_PGD_t), INTENT(INOUT) :: TGDP
!
LOGICAL, INTENT(IN) :: OALLOC ! True if constant PGD fields must be allocated
INTEGER, INTENT(IN) :: KLU
INTEGER, INTENT(IN) :: KVEGTYPE
INTEGER, INTENT(IN) :: KGROUND_LAYER
INTEGER, INTENT(IN) :: KDIMTAB
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!
IF (LHOOK) CALL DR_HOOK('ALLOCATE_TEB_GARDEN_PGD',0,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
! - Physiographic field that can evolve prognostically
!
ALLOCATE(TGDPE%CUR%XLAI                    (KLU                     ))
ALLOCATE(TGDPE%CUR%XVEG                    (KLU                     )) 
ALLOCATE(TGDPE%CUR%XEMIS                   (KLU                     )) 
ALLOCATE(TGDPE%CUR%XZ0                     (KLU                     )) 
!
! - vegetation:
!
ALLOCATE(TGDP%XALBNIR_VEG             (KLU                     )) 
ALLOCATE(TGDP%XALBVIS_VEG             (KLU                     )) 
ALLOCATE(TGDP%XALBUV_VEG              (KLU                     )) 
!
IF (.NOT. OALLOC) THEN
  IF (LHOOK) CALL DR_HOOK('ALLOCATE_TEB_GARDEN_PGD',1,ZHOOK_HANDLE)
  RETURN
END IF
!-------------------------------------------------------------------------------
!
! Mask and number of grid elements containing tiles:
ALLOCATE(TGDP%XVEGTYPE                (KLU,KVEGTYPE            ))
!
!-------------------------------------------------------------------------------
!
! Input Parameters:
!
! - vegetation + bare soil:
!
ALLOCATE(TGDP%XZ0_O_Z0H               (KLU                     )) 

!
! - vegetation: default option (Jarvis) and general parameters:
!
ALLOCATE(TGDP%XWRMAX_CF               (KLU                     )) 
ALLOCATE(TGDP%XGAMMA                  (KLU                     )) 
ALLOCATE(TGDP%XCV                     (KLU                     )) 
ALLOCATE(TGDP%XRGL                    (KLU                     )) 
ALLOCATE(TGDP%XRSMIN                  (KLU                     )) 
ALLOCATE(TGDP%XROOTFRAC               (KLU,KGROUND_LAYER       ))
ALLOCATE(TGDP%NWG_LAYER               (KLU                     ))
ALLOCATE(TGDP%XDROOT                  (KLU                     ))
ALLOCATE(TGDP%XDG2                    (KLU                     ))
!
!-------------------------------------------------------------------------------
!
! - vegetation: Ags parameters ('AGS', 'LAI', 'AST', 'LST', 'NIT' options)
!
ALLOCATE(TGDP%XBSLAI                  (KLU                     )) 
ALLOCATE(TGDP%XLAIMIN                 (KLU                     )) 
ALLOCATE(TGDP%XSEFOLD                 (KLU                     )) 
ALLOCATE(TGDP%XH_TREE                 (KLU                     )) 
ALLOCATE(TGDP%XANF                    (KLU                     )) 
ALLOCATE(TGDP%XGMES                   (KLU                     ))
ALLOCATE(TGDP%XRE25                   (KLU                     ))
!
!-------------------------------------------------------------------------------
!
! - vegetation: Ags Stress parameters ('AST', 'LST', 'NIT' options)
!
ALLOCATE(TGDP%LSTRESS                 (KLU                     )) 
ALLOCATE(TGDP%XF2I                    (KLU                     )) 
ALLOCATE(TGDP%XGC                     (KLU                     )) 
ALLOCATE(TGDP%XAH                     (KLU                     )) 
ALLOCATE(TGDP%XBH                     (KLU                     )) 
ALLOCATE(TGDP%XDMAX                   (KLU                     )) 
!
!-------------------------------------------------------------------------------
!
! - vegetation: Ags Nitrogen-model parameters ('NIT' option)
!
ALLOCATE(TGDP%XCE_NITRO               (KLU                     )) 
ALLOCATE(TGDP%XCF_NITRO               (KLU                     )) 
ALLOCATE(TGDP%XCNA_NITRO              (KLU                     )) 
!
!-------------------------------------------------------------------------------
!
! - soil: primary parameters
!
ALLOCATE(TGDP%XSAND                   (KLU,KGROUND_LAYER       )) 
ALLOCATE(TGDP%XCLAY                   (KLU,KGROUND_LAYER       )) 
ALLOCATE(TGDP%XRUNOFFB                (KLU                     )) 
ALLOCATE(TGDP%XWDRAIN                 (KLU                     )) 
ALLOCATE(TGDP%XGAMMAT                 (KLU                     )) 
ALLOCATE(TGDP%XDG                     (KLU,KGROUND_LAYER       )) 
ALLOCATE(TGDP%XRUNOFFD                (KLU                     )) 
!
!-------------------------------------------------------------------------------
!
! - SGH scheme
!                                   
ALLOCATE(TGDP%XD_ICE                  (KLU                     )) 
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('ALLOCATE_TEB_GARDEN_PGD',1,ZHOOK_HANDLE)
!
END SUBROUTINE ALLOCATE_TEB_GARDEN_PGD
