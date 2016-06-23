!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
    SUBROUTINE ALLOCATE_TEB_GREENROOF_PGD (TGRPE, TGRP, &
                                           OALLOC,KLU,KVEGTYPE,KLAYER_GR, KDIMTAB)  
!   ##########################################################################
!
!
!
!
USE MODD_TEB_GREENROOF_PGD_EVOL_n, ONLY : TEB_GREENROOF_PGD_EVOL_t
USE MODD_TEB_GREENROOF_PGD_n, ONLY : TEB_GREENROOF_PGD_t
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!
TYPE(TEB_GREENROOF_PGD_EVOL_t), INTENT(INOUT) :: TGRPE
TYPE(TEB_GREENROOF_PGD_t), INTENT(INOUT) :: TGRP
!
LOGICAL, INTENT(IN) :: OALLOC ! True if constant PGD fields must be allocated
INTEGER, INTENT(IN) :: KLU
INTEGER, INTENT(IN) :: KVEGTYPE
INTEGER, INTENT(IN) :: KLAYER_GR
INTEGER, INTENT(IN) :: KDIMTAB
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('ALLOCATE_TEB_GREENROOF_PGD',0,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
! - Physiographic field that can evolve prognostically
!
ALLOCATE(TGRPE%CUR%XLAI                    (KLU                     ))
ALLOCATE(TGRPE%CUR%XVEG                    (KLU                     )) 
ALLOCATE(TGRPE%CUR%XEMIS                   (KLU                     )) 
ALLOCATE(TGRPE%CUR%XZ0                     (KLU                     )) 
!
! - vegetation:
!
ALLOCATE(TGRP%XALBNIR_VEG             (KLU                     )) 
ALLOCATE(TGRP%XALBVIS_VEG             (KLU                     )) 
ALLOCATE(TGRP%XALBUV_VEG              (KLU                     )) 
!
IF (.NOT. OALLOC) THEN
  IF (LHOOK) CALL DR_HOOK('ALLOCATE_TEB_GARDEN_PGD',1,ZHOOK_HANDLE)
  RETURN
END IF
!-------------------------------------------------------------------------------
! Mask and number of grid elements containing tiles:
!
ALLOCATE(TGRP%XVEGTYPE                (KLU,KVEGTYPE            ))
!
!-------------------------------------------------------------------------------
!
! Input Parameters:
!
! - vegetation + bare soil:
!
ALLOCATE(TGRP%XZ0_O_Z0H               (KLU                     )) 
!
! - vegetation: default option (Jarvis) and general parameters:
!
ALLOCATE(TGRP%XWRMAX_CF               (KLU                     )) 
ALLOCATE(TGRP%XGAMMA                  (KLU                     )) 
ALLOCATE(TGRP%XCV                     (KLU                     )) 
ALLOCATE(TGRP%XRGL                    (KLU                     )) 
ALLOCATE(TGRP%XRSMIN                  (KLU                     )) 
ALLOCATE(TGRP%XROOTFRAC               (KLU,KLAYER_GR           ))
ALLOCATE(TGRP%NWG_LAYER               (KLU                     ))
ALLOCATE(TGRP%XDROOT                  (KLU                     ))
ALLOCATE(TGRP%XDG2                    (KLU                     ))
!
!-------------------------------------------------------------------------------
!
! - vegetation: Ags parameters ('AGS', 'LAI', 'AST', 'LST', 'NIT' options)
!
ALLOCATE(TGRP%XBSLAI                  (KLU                     )) 
ALLOCATE(TGRP%XLAIMIN                 (KLU                     )) 
ALLOCATE(TGRP%XSEFOLD                 (KLU                     )) 
ALLOCATE(TGRP%XH_TREE                 (KLU                     )) 
ALLOCATE(TGRP%XANF                    (KLU                     ))
ALLOCATE(TGRP%XGMES                   (KLU                     ))
ALLOCATE(TGRP%XRE25                   (KLU                     ))
!
!-------------------------------------------------------------------------------
!
! - vegetation: Ags Stress parameters ('AST', 'LST', 'NIT' options)
!
ALLOCATE(TGRP%LSTRESS                 (KLU                     )) 
ALLOCATE(TGRP%XF2I                    (KLU                     )) 
ALLOCATE(TGRP%XGC                     (KLU                     )) 
ALLOCATE(TGRP%XAH                     (KLU                     )) 
ALLOCATE(TGRP%XBH                     (KLU                     )) 
ALLOCATE(TGRP%XDMAX                   (KLU                     )) 
!
!-------------------------------------------------------------------------------
!
! - vegetation: Ags Nitrogen-model parameters ('NIT' option)
!
ALLOCATE(TGRP%XCE_NITRO               (KLU                     )) 
ALLOCATE(TGRP%XCF_NITRO               (KLU                     )) 
ALLOCATE(TGRP%XCNA_NITRO              (KLU                     )) 
!
!-------------------------------------------------------------------------------
!
! - soil: primary parameters
!
ALLOCATE(TGRP%XOM_GR                  (KLU,KLAYER_GR           ))  
ALLOCATE(TGRP%XSAND_GR                (KLU,KLAYER_GR           ))  
ALLOCATE(TGRP%XCLAY_GR                (KLU,KLAYER_GR           ))  
ALLOCATE(TGRP%XTAUICE                 (KLU                     )) 
ALLOCATE(TGRP%XGAMMAT                 (KLU                     )) 
ALLOCATE(TGRP%XDG                     (KLU,KLAYER_GR           )) 
ALLOCATE(TGRP%XRUNOFFD                (KLU                     )) 
!
!-------------------------------------------------------------------------------
!
! - SGH scheme
!                                   
ALLOCATE(TGRP%XD_ICE                  (KLU                     )) 
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('ALLOCATE_TEB_GREENROOF_PGD',1,ZHOOK_HANDLE)
!
END SUBROUTINE ALLOCATE_TEB_GREENROOF_PGD
