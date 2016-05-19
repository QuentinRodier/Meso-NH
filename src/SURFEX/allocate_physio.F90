!     #########
    SUBROUTINE ALLOCATE_PHYSIO(HPHOTO, HISBA, KLU, KVEGTYPE, KGROUND_LAYER, KPATCH, &
                               PVEGTYPE, PLAI, PVEG, PZ0, PEMIS, PDG, PD_ICE, &
                               PRSMIN, PGAMMA, PWRMAX_CF, PRGL, PCV, &
                               PZ0_O_Z0H, PALBNIR_VEG, PALBVIS_VEG, PALBUV_VEG, &
                               PH_TREE, PRE25, PLAIMIN, PBSLAI, PSEFOLD, &
                               PGMES, PGC, PF2I, PDMAX, OSTRESS, &
                               PCE_NITRO, PCF_NITRO, PCNA_NITRO, &
                               PTSEED, PTREAP, PWATSUP, PIRRIG, &
                               PROOTFRAC, KWG_LAYER, PDROOT, PDG2 )  
!   ##########################################################################
!
USE MODD_TYPE_DATE_SURF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
 CHARACTER(LEN=3),INTENT(IN)  :: HPHOTO
 CHARACTER(LEN=3),INTENT(IN)  :: HISBA
!
INTEGER, INTENT(IN) :: KLU
INTEGER, INTENT(IN) :: KVEGTYPE
INTEGER, INTENT(IN) :: KGROUND_LAYER
INTEGER, INTENT(IN) :: KPATCH
!
REAL, DIMENSION(:,:), POINTER :: PVEGTYPE
!
REAL, DIMENSION(:,:), POINTER :: PLAI
REAL, DIMENSION(:,:), POINTER :: PVEG
REAL, DIMENSION(:,:), POINTER :: PZ0
REAL, DIMENSION(:,:), POINTER :: PEMIS
!
REAL, DIMENSION(:,:,:), POINTER :: PDG
REAL, DIMENSION(:,:)  , POINTER :: PD_ICE
!
REAL, DIMENSION(:,:), POINTER :: PRSMIN
REAL, DIMENSION(:,:), POINTER :: PGAMMA
REAL, DIMENSION(:,:), POINTER :: PWRMAX_CF
REAL, DIMENSION(:,:), POINTER :: PRGL
REAL, DIMENSION(:,:), POINTER :: PCV
REAL, DIMENSION(:,:), POINTER :: PZ0_O_Z0H
REAL, DIMENSION(:,:), POINTER :: PALBNIR_VEG
REAL, DIMENSION(:,:), POINTER :: PALBVIS_VEG
REAL, DIMENSION(:,:), POINTER :: PALBUV_VEG
!
REAL, DIMENSION(:,:), POINTER :: PH_TREE
REAL, DIMENSION(:,:), POINTER :: PRE25
REAL, DIMENSION(:,:), POINTER :: PLAIMIN
REAL, DIMENSION(:,:), POINTER :: PBSLAI
REAL, DIMENSION(:,:), POINTER :: PSEFOLD
REAL, DIMENSION(:,:), POINTER :: PGMES
REAL, DIMENSION(:,:), POINTER :: PGC
REAL, DIMENSION(:,:), POINTER :: PF2I
REAL, DIMENSION(:,:), POINTER :: PDMAX
LOGICAL, DIMENSION(:,:), POINTER :: OSTRESS
REAL, DIMENSION(:,:), POINTER :: PCE_NITRO
REAL, DIMENSION(:,:), POINTER :: PCF_NITRO
REAL, DIMENSION(:,:), POINTER :: PCNA_NITRO
!
TYPE(DATE_TIME), DIMENSION(:,:), POINTER :: PTSEED
TYPE(DATE_TIME), DIMENSION(:,:), POINTER :: PTREAP
REAL, DIMENSION(:,:), POINTER :: PWATSUP
REAL, DIMENSION(:,:), POINTER :: PIRRIG
!
REAL, DIMENSION(:,:,:), POINTER :: PROOTFRAC
INTEGER, DIMENSION(:,:), POINTER :: KWG_LAYER
REAL, DIMENSION(:,:), POINTER :: PDROOT
REAL, DIMENSION(:,:), POINTER :: PDG2
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
! Mask and number of grid elements containing patches/tiles:
!
IF (LHOOK) CALL DR_HOOK('ALLOCATE_PHYSIO',0,ZHOOK_HANDLE)
ALLOCATE(PVEGTYPE                (KLU,KVEGTYPE            ))
!
ALLOCATE(PLAI                    (KLU,KPATCH              )) 
ALLOCATE(PVEG                    (KLU,KPATCH              )) 
ALLOCATE(PZ0                     (KLU,KPATCH              )) 
ALLOCATE(PEMIS                   (KLU,KPATCH              )) 
!
ALLOCATE(PDG                     (KLU,KGROUND_LAYER,KPATCH)) 
ALLOCATE(PD_ICE                  (KLU,KPATCH              )) 
!
ALLOCATE(PRSMIN                  (KLU,KPATCH              )) 
ALLOCATE(PGAMMA                  (KLU,KPATCH              )) 
ALLOCATE(PWRMAX_CF               (KLU,KPATCH              )) 
ALLOCATE(PRGL                    (KLU,KPATCH              )) 
ALLOCATE(PCV                     (KLU,KPATCH              )) 
ALLOCATE(PZ0_O_Z0H               (KLU,KPATCH              )) 
ALLOCATE(PALBNIR_VEG             (KLU,KPATCH              )) 
ALLOCATE(PALBVIS_VEG             (KLU,KPATCH              )) 
ALLOCATE(PALBUV_VEG              (KLU,KPATCH              )) 
!
! - vegetation: Ags parameters ('AGS', 'LAI', 'AST', 'LST', 'NIT' options)
!
IF (HPHOTO/='NON') THEN
  ALLOCATE(PH_TREE                 (KLU,KPATCH              )) 
  ALLOCATE(PRE25                   (KLU,KPATCH              )) 
  ALLOCATE(PLAIMIN                 (KLU,KPATCH              )) 
  ALLOCATE(PBSLAI                  (KLU,KPATCH              )) 
  ALLOCATE(PSEFOLD                 (KLU,KPATCH              )) 
  ALLOCATE(PGMES                   (KLU,KPATCH              )) 
  ALLOCATE(PGC                     (KLU,KPATCH              )) 
  ALLOCATE(PDMAX                   (KLU,KPATCH              ))
  IF (HPHOTO/='AGS' .AND. HPHOTO/='LAI') THEN
    ALLOCATE(PF2I                    (KLU,KPATCH              ))
    ALLOCATE(OSTRESS                 (KLU,KPATCH              )) 
    IF (HPHOTO=='NIT' .OR. HPHOTO=='NCB') THEN
      ALLOCATE(PCE_NITRO               (KLU,KPATCH              )) 
      ALLOCATE(PCF_NITRO               (KLU,KPATCH              )) 
      ALLOCATE(PCNA_NITRO              (KLU,KPATCH              ))  
    ELSE
      ALLOCATE(PCE_NITRO    (0,0))
      ALLOCATE(PCF_NITRO    (0,0))
      ALLOCATE(PCNA_NITRO   (0,0))
 
    ENDIF
  ELSE
    ALLOCATE(PF2I   (0,0))
    ALLOCATE(OSTRESS(0,0))
    ALLOCATE(PCE_NITRO    (0,0))
    ALLOCATE(PCF_NITRO    (0,0))
    ALLOCATE(PCNA_NITRO   (0,0))
  ENDIF
ELSE
  ALLOCATE(PH_TREE    (0,0)) 
  ALLOCATE(PRE25      (0,0))
  ALLOCATE(PLAIMIN    (0,0))
  ALLOCATE(PBSLAI     (0,0))  
  ALLOCATE(PSEFOLD    (0,0))  
  ALLOCATE(PGMES      (0,0))
  ALLOCATE(PGC        (0,0))
  ALLOCATE(PF2I   (0,0))
  ALLOCATE(PDMAX  (0,0))
  ALLOCATE(OSTRESS(0,0))
  ALLOCATE(PCE_NITRO    (0,0))
  ALLOCATE(PCF_NITRO    (0,0))
  ALLOCATE(PCNA_NITRO   (0,0))
ENDIF  
!
! - Irrigation, seeding and reaping
!
IF (HPHOTO == 'LAI' .OR. HPHOTO == 'LST' .OR. HPHOTO == 'NIT' .OR. HPHOTO == 'NCB')  THEN
  ALLOCATE(PTSEED                  (KLU,KPATCH              )) 
  ALLOCATE(PTREAP                  (KLU,KPATCH              )) 
  ALLOCATE(PWATSUP                 (KLU,KPATCH              )) 
  ALLOCATE(PIRRIG                  (KLU,KPATCH              ))
ELSE
  ALLOCATE(PTSEED     (0,0))
  ALLOCATE(PTREAP     (0,0))
  ALLOCATE(PWATSUP    (0,0))
  ALLOCATE(PIRRIG     (0,0))        
ENDIF
!
! - ISBA-DF scheme
!
IF(HISBA=='DIF')THEN
  ALLOCATE(PROOTFRAC  (KLU,KGROUND_LAYER,KPATCH))
  ALLOCATE(KWG_LAYER  (KLU,KPATCH))
  ALLOCATE(PDROOT     (KLU,KPATCH))
  ALLOCATE(PDG2       (KLU,KPATCH))
ELSE  
  ALLOCATE(PROOTFRAC  (0,0,0))
  ALLOCATE(KWG_LAYER  (0,0)  )
  ALLOCATE(PDROOT     (0,0)  )        
  ALLOCATE(PDG2       (0,0)  )        
ENDIF
!
!
IF (LHOOK) CALL DR_HOOK('ALLOCATE_PHYSIO',1,ZHOOK_HANDLE)
!
END SUBROUTINE ALLOCATE_PHYSIO
