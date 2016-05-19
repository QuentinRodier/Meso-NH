!     #########
    SUBROUTINE ALLOCATE_TEB_GARDEN_PGD(OALLOC,KLU,KVEGTYPE,KGROUND_LAYER, KDIMTAB)  
!   ##########################################################################
!
USE MODD_TEB_GARDEN_n
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
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
ALLOCATE(XLAI                    (KLU                     ))
ALLOCATE(XVEG                    (KLU                     )) 
ALLOCATE(XEMIS                   (KLU                     )) 
ALLOCATE(XZ0                     (KLU                     )) 
!
! - vegetation:
!
ALLOCATE(XALBNIR_VEG             (KLU                     )) 
ALLOCATE(XALBVIS_VEG             (KLU                     )) 
ALLOCATE(XALBUV_VEG              (KLU                     )) 
!
IF (.NOT. OALLOC) THEN
  IF (LHOOK) CALL DR_HOOK('ALLOCATE_TEB_GARDEN_PGD',1,ZHOOK_HANDLE)
  RETURN
END IF
!-------------------------------------------------------------------------------
!
! Mask and number of grid elements containing tiles:
ALLOCATE(XVEGTYPE                (KLU,KVEGTYPE            ))
!
!-------------------------------------------------------------------------------
!
! Input Parameters:
!
! - vegetation + bare soil:
!
ALLOCATE(XZ0_O_Z0H               (KLU                     )) 

!
! - vegetation: default option (Jarvis) and general parameters:
!
ALLOCATE(XWRMAX_CF               (KLU                     )) 
ALLOCATE(XGAMMA                  (KLU                     )) 
ALLOCATE(XCV                     (KLU                     )) 
ALLOCATE(XRGL                    (KLU                     )) 
ALLOCATE(XRSMIN                  (KLU                     )) 
ALLOCATE(XROOTFRAC               (KLU,KGROUND_LAYER       ))
ALLOCATE(NWG_LAYER               (KLU                     ))
ALLOCATE(XDROOT                  (KLU                     ))
ALLOCATE(XDG2                    (KLU                     ))
!
!-------------------------------------------------------------------------------
!
! - vegetation: Ags parameters ('AGS', 'LAI', 'AST', 'LST', 'NIT' options)
!
ALLOCATE(XBSLAI                  (KLU                     )) 
ALLOCATE(XLAIMIN                 (KLU                     )) 
ALLOCATE(XSEFOLD                 (KLU                     )) 
ALLOCATE(XH_TREE                 (KLU                     )) 
ALLOCATE(XANF                    (KLU                     )) 
ALLOCATE(XGMES                   (KLU                     ))
ALLOCATE(XRE25                   (KLU                     ))
!
!-------------------------------------------------------------------------------
!
! - vegetation: Ags Stress parameters ('AST', 'LST', 'NIT' options)
!
ALLOCATE(LSTRESS                 (KLU                     )) 
ALLOCATE(XF2I                    (KLU                     )) 
ALLOCATE(XGC                     (KLU                     )) 
ALLOCATE(XAH                     (KLU                     )) 
ALLOCATE(XBH                     (KLU                     )) 
ALLOCATE(XDMAX                   (KLU                     )) 
!
!-------------------------------------------------------------------------------
!
! - vegetation: Ags Nitrogen-model parameters ('NIT' option)
!
ALLOCATE(XCE_NITRO               (KLU                     )) 
ALLOCATE(XCF_NITRO               (KLU                     )) 
ALLOCATE(XCNA_NITRO              (KLU                     )) 
!
!-------------------------------------------------------------------------------
!
! - soil: primary parameters
!
ALLOCATE(XSAND                   (KLU,KGROUND_LAYER       )) 
ALLOCATE(XCLAY                   (KLU,KGROUND_LAYER       )) 
ALLOCATE(XRUNOFFB                (KLU                     )) 
ALLOCATE(XWDRAIN                 (KLU                     )) 
ALLOCATE(XGAMMAT                 (KLU                     )) 
ALLOCATE(XDG                     (KLU,KGROUND_LAYER       )) 
ALLOCATE(XRUNOFFD                (KLU                     )) 
!
!-------------------------------------------------------------------------------
!
! - SGH scheme
!                                   
ALLOCATE(XD_ICE                  (KLU                     )) 
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('ALLOCATE_TEB_GARDEN_PGD',1,ZHOOK_HANDLE)
!
END SUBROUTINE ALLOCATE_TEB_GARDEN_PGD
