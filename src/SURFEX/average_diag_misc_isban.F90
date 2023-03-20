!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #############################
      SUBROUTINE AVERAGE_DIAG_MISC_ISBA_n (DM, NDM, IO, NK, NP, NPE)
!     #############################
!
!
!!****  *AVERAGE_DIAG_MISC_ISBA_n*  
!!
!!    PURPOSE
!!    -------
!      Average the cumulated diagnostics from all ISBA tiles
!     
!!**  METHOD
!!    ------
!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------ 
!!
!!      
!!    REFERENCE
!!    ---------
!!      
!!    AUTHOR
!!    ------
!!      P. Le Moigne           * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    10/2004
!!      B. Decharme  2008    New diag Total albedo, Total SWI, & Flood
!!      B. Decharme 09/2009  New diag Total soil SWI
!!      B. Decharme  2012    Averaged LAI
!!      B. Decharme  2012    New diag for DIF:
!!                           F2 stress
!!                           Root zone swi, wg and wgi
!!                           swi, wg and wgi comparable to ISBA-FR-DG2 and DG3 layers
!!                           active layer thickness over permafrost
!!                           frozen layer thickness over non-permafrost
!!      J. Colin     2018    Nudging diag
!!      B. Decharme  2018    Many diags, SWI exact comptutation, etc.
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
!
USE MODD_DIAG_MISC_ISBA_n, ONLY : DIAG_MISC_ISBA_t, DIAG_MISC_ISBA_NP_t
USE MODD_ISBA_OPTIONS_n,   ONLY : ISBA_OPTIONS_t
USE MODD_ISBA_n,           ONLY : ISBA_K_t, ISBA_P_t, ISBA_PE_t, ISBA_NK_t, ISBA_NP_t, ISBA_NPE_t
!
USE MODD_SURF_PAR, ONLY : XUNDEF, NUNDEF
USE MODD_CSTS,     ONLY : XRHOLW
!
USE MODI_COMPUT_COLD_LAYERS_THICK
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
TYPE(DIAG_MISC_ISBA_t),    INTENT(INOUT) :: DM
TYPE(DIAG_MISC_ISBA_NP_t), INTENT(INOUT) :: NDM
TYPE(ISBA_OPTIONS_t),      INTENT(INOUT) :: IO
TYPE(ISBA_NK_t),           INTENT(INOUT) :: NK
TYPE(ISBA_NP_t),           INTENT(INOUT) :: NP
TYPE(ISBA_NPE_t),          INTENT(INOUT) :: NPE
!
!*      0.2    declarations of local variables
!
TYPE(ISBA_K_t),         POINTER :: KK
TYPE(ISBA_P_t),         POINTER :: PK
TYPE(ISBA_PE_t),        POINTER :: PEK
TYPE(DIAG_MISC_ISBA_t), POINTER :: DMK
!
REAL, DIMENSION(SIZE(DM%XHV)) :: ZSUMDG, ZSUMDG2
REAL, DIMENSION(SIZE(DM%XHV)) :: ZVEG
REAL, DIMENSION(SIZE(DM%XHV)) :: ZSNOW
REAL, DIMENSION(SIZE(DM%XHV)) :: ZSUMFRD2
REAL, DIMENSION(SIZE(DM%XHV)) :: ZSUMFRD3
REAL, DIMENSION(SIZE(DM%XHV)) :: ZPONDSWI
REAL, DIMENSION(SIZE(DM%XHV)) :: ZPONDSWID2
REAL, DIMENSION(SIZE(DM%XHV)) :: ZPONDSWID3
!
REAL, DIMENSION(SIZE(DM%XHV),IO%NGROUND_LAYER) :: ZPONDL, ZTG, ZDG, ZPONDVEG
REAL, DIMENSION(SIZE(DM%XHV),IO%NNSOILCARB)    :: ZCSOILTOT
!
LOGICAL                         :: GPROSNOW
!
REAL                            :: ZWORK, ZTSWI, ZSWI, ZTAW
!
INTEGER                         :: JI    ! grid-cell loop counter
INTEGER                         :: JP    ! tile loop counter
INTEGER                         :: JL    ! layer loop counter
INTEGER                         :: INI,INP,INL
INTEGER                         :: IDEPTH,IMASK
INTEGER                         :: ISIZE_LMEB_PATCH   ! Number of patches where multi-energy balance should be applied
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('AVERAGE_DIAG_MISC_ISBA_N',0,ZHOOK_HANDLE)
!
!       0.     Initialization
!              --------------
!
INI=SIZE(DM%XHV)
INL=IO%NGROUND_LAYER
INP=IO%NPATCH
!
ISIZE_LMEB_PATCH = COUNT(IO%LMEB_PATCH(:))
!
GPROSNOW = (DM%LPROSNOW .AND. NPE%AL(1)%TSNOW%SCHEME=="CRO")
!
!-------------------------------------------------------------------------------
!
!       1.     Surface Miscellaneous terms
!              ---------------------------
!
DM%XWR  (:) = 0.
DM%XWRVN(:) = 0.
DM%XHV  (:) = 0.
DM%XPSNG(:) = 0.
DM%XPSNV(:) = 0.
DM%XPSN (:) = 0.
DM%XFSAT(:) = 0.
DM%XFFG (:) = 0.
DM%XFFV (:) = 0.
DM%XFF  (:) = 0.
!
DM%XLAI(:) = 0.
DM%XF2 (:) = 0.
!
DM%XTWSNOW   (:) = 0.
DM%XTDSNOW   (:) = 0.  
DM%XTTSNOW   (:) = 0.
DM%XTSNOWAGE (:) = 0.
DM%XTSNOWLIQ (:) = 0.
DM%XGFLUXSNOW(:) = 0.
!
DM%XSOIL_TWG (:) = 0.
DM%XSOIL_TWGI(:) = 0.
DM%XROOT_TWG (:) = 0.
DM%XSURF_TWG (:) = 0.
!
ZSNOW(:)=0.0
ZVEG (:)=0.0
!   
DO JP=1,INP
   !
   PK => NP%AL(JP)
   PEK => NPE%AL(JP)
   DMK => NDM%AL(JP)
   !     
   DO JI=1,PK%NSIZE_P
      !
      IMASK = PK%NR_P(JI)
      !
      !liquid water retained on canopŷ
      DM%XWR(IMASK) = DM%XWR(IMASK) + PK%XPATCH(JI) * PEK%XWR(JI)
      !
      ! Snow retained on canopy with MEB
      IF(ISIZE_LMEB_PATCH>0)THEN
        DM%XWRVN(IMASK) = DM%XWRVN(IMASK) + PK%XPATCH(JI) * PEK%XWRVN(JI)
      ENDIF
      !
      ! Halstead coefficient
      DM%XHV(IMASK) = DM%XHV(IMASK) + PK%XPATCH(JI) * DMK%XHV(JI)
      !
      ! Snow fractions
      DM%XPSNG(IMASK) = DM%XPSNG(IMASK) + PK%XPATCH(JI) * DMK%XPSNG(JI)
      DM%XPSNV(IMASK) = DM%XPSNV(IMASK) + PK%XPATCH(JI) * DMK%XPSNV(JI)
      DM%XPSN (IMASK) = DM%XPSN (IMASK) + PK%XPATCH(JI) * DMK%XPSN (JI)
      !
      ! Saturated fraction
      DM%XFSAT(IMASK) = DM%XFSAT(IMASK) + PK%XPATCH(JI) * DMK%XFSAT(JI)
      !
      ! Flood fractions
      DM%XFFG(IMASK) = DM%XFFG(IMASK) + PK%XPATCH(JI) * DMK%XFFG(JI)
      DM%XFFV(IMASK) = DM%XFFV(IMASK) + PK%XPATCH(JI) * DMK%XFFV(JI)
      DM%XFF (IMASK) = DM%XFF (IMASK) + PK%XPATCH(JI) * DMK%XFF (JI)
      !
      ! Total LAI and vegetation soil stress
      IF (PEK%XLAI(JI)/=XUNDEF) THEN
         DM%XLAI(IMASK) = DM%XLAI(IMASK) + PK%XPATCH(JI) * PEK%XLAI(JI)
         DM%XF2 (IMASK) = DM%XF2 (IMASK) + PK%XPATCH(JI) * DMK%XF2 (JI)
         ZVEG   (IMASK) = ZVEG   (IMASK) + PK%XPATCH(JI)
      ENDIF
      !
      ! Total soil moisture
      DM%XSOIL_TWG (IMASK) = DM%XSOIL_TWG (IMASK) + PK%XPATCH(JI) * DMK%XSOIL_TWG (JI)
      DM%XSOIL_TWGI(IMASK) = DM%XSOIL_TWGI(IMASK) + PK%XPATCH(JI) * DMK%XSOIL_TWGI(JI)
      DM%XROOT_TWG (IMASK) = DM%XROOT_TWG (IMASK) + PK%XPATCH(JI) * DMK%XROOT_TWG (JI)
      DM%XSURF_TWG (IMASK) = DM%XSURF_TWG (IMASK) + PK%XPATCH(JI) * DMK%XSURF_TWG (JI)
      !
      ! Snow total outputs
      DM%XTWSNOW   (IMASK) = DM%XTWSNOW(IMASK)    + PK%XPATCH(JI) * DMK%XTWSNOW   (JI)
      DM%XTDSNOW   (IMASK) = DM%XTDSNOW(IMASK)    + PK%XPATCH(JI) * DMK%XTDSNOW   (JI)
      DM%XTSNOWLIQ (IMASK) = DM%XTSNOWLIQ (IMASK) + PK%XPATCH(JI) * DMK%XTSNOWLIQ (JI)
      DM%XGFLUXSNOW(IMASK) = DM%XGFLUXSNOW(IMASK) + PK%XPATCH(JI) * DMK%XGFLUXSNOW(JI)
      !      
      IF (DMK%XTWSNOW(JI)>0.0) THEN
         DM%XTTSNOW  (IMASK) = DM%XTTSNOW  (IMASK) + PK%XPATCH(JI) * DMK%XTTSNOW  (JI)
         DM%XTSNOWAGE(IMASK) = DM%XTSNOWAGE(IMASK) + PK%XPATCH(JI) * DMK%XTSNOWAGE(JI)
         ZSNOW       (IMASK) = ZSNOW       (IMASK) + PK%XPATCH(JI)
      ENDIF
      !      
   ENDDO
   !
ENDDO
!
! Total vegetation soil stress final
!
WHERE(ZVEG(:)>0.0)
     DM%XF2(:) = DM%XF2(:)/ZVEG(:)
ELSEWHERE
     DM%XF2(:) = XUNDEF
ENDWHERE
!
! Total Snow temperature and age final
!
WHERE(ZSNOW(:)>0.0)
     DM%XTTSNOW  (:) = DM%XTTSNOW  (:)/ZSNOW(:)
     DM%XTSNOWAGE(:) = DM%XTSNOWAGE(:)/ZSNOW(:)
ELSEWHERE
     DM%XTTSNOW  (:) = XUNDEF
     DM%XTSNOWAGE(:) = XUNDEF
ENDWHERE
!
!-------------------------------------------------------------------------------
!
!       2.     Specific treatement following CISBA option
!              ------------------------------------------
!
!   Soil Wetness Index profile, Total Soil Wetness Index and 
!   Total Soil Water Content (Liquid+Solid) and Total Frozen Content
!
DM%XF2WGHT(:,:) = 0.
!
DM%XSWI (:,:) = 0.
DM%XTSWI(:,:) = 0.
!   
DM%XSOIL_SWI (:) = 0.
DM%XSOIL_TSWI(:) = 0.
DM%XSOIL_WG  (:) = 0.
DM%XSOIL_WGI (:) = 0.
!
ZSUMDG  (:)=0.0
ZSUMDG2 (:)=0.0
ZSUMFRD2(:)=0.0
ZSUMFRD3(:)=0.0
!
!---------------------------------
IF(IO%CISBA=='DIF')THEN ! DIF case
  !-------------------------------
  !
  ! Permafrost, Active and Frozen layers thickness
  !
  DM%XPLT(:) = 0. 
  DM%XALT(:) = 0. 
  DM%XFLT(:) = 0. 
  !
  ZTG(:,:)=0.0
  ZDG(:,:)=0.0
  DO JP=1,INP
    PK => NP%AL(JP)
    PEK => NPE%AL(JP)
    DO JL=1,INL
      DO JI=1,PK%NSIZE_P
        IMASK = PK%NR_P(JI)
        ZTG(IMASK,JL) = ZTG(IMASK,JL) + PK%XPATCH(JI) * PEK%XTG(JI,JL)
        ZDG(IMASK,JL) = ZDG(IMASK,JL) + PK%XPATCH(JI) * PK%XDG (JI,JL)
        ENDDO
     ENDDO
  ENDDO
  !
  CALL COMPUT_COLD_LAYERS_THICK(ZDG,ZTG,DM%XPLT,DM%XALT,DM%XFLT)
  !
  ! Soil water content and soil wetness index
  !  
  ZPONDSWI(:  )=0.0
  ZPONDL  (:,:)=0.0
  ZPONDVEG(:,:)=0.0
  !  
  DO JP=1,INP   
     KK => NK%AL(JP)
     PK => NP%AL(JP)
     PEK => NPE%AL(JP)
     DMK => NDM%AL(JP)
     DO JL = 1,INL
        DO JI=1,PK%NSIZE_P
           IMASK = PK%NR_P(JI)
           IDEPTH = PK%NWG_LAYER(JI)
           IF(JL<=IDEPTH.AND.IDEPTH/=NUNDEF)THEN
             !Total soil depth
             ZSUMDG(IMASK) = ZSUMDG(IMASK) + PK%XDZG(JI,JL) * PK%XPATCH(JI)
             !Total available water (m)
             ZSWI  = PEK%XWG(JI,JL)                 - KK%XWWILT(JI,JL)
             ZTSWI = PEK%XWG(JI,JL)+PEK%XWGI(JI,JL) - KK%XWWILT(JI,JL)
             ZTAW  = KK%XWFC(JI,JL)                 - KK%XWWILT(JI,JL)
             !Soil Wetness Index profile
             DM%XSWI (IMASK,JL) = DM%XSWI (IMASK,JL) + PK%XDZG(JI,JL) * PK%XPATCH(JI) * ZSWI
             DM%XTSWI(IMASK,JL) = DM%XTSWI(IMASK,JL) + PK%XDZG(JI,JL) * PK%XPATCH(JI) * ZTSWI
             ZPONDL  (IMASK,JL) = ZPONDL  (IMASK,JL) + PK%XDZG(JI,JL) * PK%XPATCH(JI) * ZTAW
             !Total soil wetness index
             DM%XSOIL_SWI (IMASK) = DM%XSOIL_SWI (IMASK) + PK%XDZG(JI,JL) * PK%XPATCH(JI) * ZSWI
             DM%XSOIL_TSWI(IMASK) = DM%XSOIL_TSWI(IMASK) + PK%XDZG(JI,JL) * PK%XPATCH(JI) * ZTSWI
             ZPONDSWI     (IMASK) = ZPONDSWI     (IMASK) + PK%XDZG(JI,JL) * PK%XPATCH(JI) * ZTAW
           ENDIF
           IF(PEK%XLAI(JI)/=XUNDEF.AND.JL<=IDEPTH.AND.IDEPTH/=NUNDEF)THEN
             !Vegetation soil stress by layer
             DM%XF2WGHT(IMASK,JL) = DM%XF2WGHT(IMASK,JL) + PK%XDZG(JI,JL) * PK%XPATCH(JI) * DMK%XF2WGHT(JI,JL)
             ZPONDVEG  (IMASK,JL) = ZPONDVEG  (IMASK,JL) + PK%XDZG(JI,JL) * PK%XPATCH(JI)
           ENDIF
        ENDDO
     ENDDO
  ENDDO
  !  
  WHERE(ZPONDVEG(:,:)> 0.)
    DM%XF2WGHT(:,:) = DM%XF2WGHT(:,:) / ZPONDVEG(:,:)
  ELSEWHERE
    DM%XF2WGHT(:,:) = XUNDEF
  ENDWHERE
  !   
  WHERE(ZPONDL(:,:)> 0.)
    DM%XSWI (:,:) = DM%XSWI (:,:) / ZPONDL(:,:)
    DM%XTSWI(:,:) = DM%XTSWI(:,:) / ZPONDL(:,:)
  ELSEWHERE
    DM%XSWI (:,:) = XUNDEF
    DM%XTSWI(:,:) = XUNDEF
  ENDWHERE
  !  
  WHERE(ZPONDSWI(:)> 0.)
        DM%XSOIL_SWI (:) = DM%XSOIL_SWI (:) / ZPONDSWI(:)
        DM%XSOIL_TSWI(:) = DM%XSOIL_TSWI(:) / ZPONDSWI(:)
  ELSEWHERE
        DM%XSOIL_SWI (:) = XUNDEF
        DM%XSOIL_TSWI(:) = XUNDEF
  ENDWHERE
  !
  !Soil Water Content (m3.m-3)
  !
  WHERE(ZSUMDG(:)>0.0)
        DM%XSOIL_WG (:) = DM%XSOIL_TWG (:)/(ZSUMDG(:)*XRHOLW)
        DM%XSOIL_WGI(:) = DM%XSOIL_TWGI(:)/(ZSUMDG(:)*XRHOLW)
  ENDWHERE
  !
  !----------------------------------------------
  IF(DM%LSURF_MISC_DIF)THEN ! LSURF_MISC_DIF case
    !--------------------------------------------
    !
    ZPONDSWID2(:)=0.0
    ZPONDSWID3(:)=0.0
    !
    DM%XFRD2_TSWI (:) = 0.
    DM%XFRD2_TWG  (:) = 0.
    DM%XFRD2_TWGI (:) = 0.
    !
    DM%XFRD3_TSWI (:) = 0.
    DM%XFRD3_TWG  (:) = 0.
    DM%XFRD3_TWGI (:) = 0.
    !
    DO JP=1,INP
       KK => NK%AL(JP)
       PK => NP%AL(JP)
       PEK => NPE%AL(JP)
       DMK => NDM%AL(JP)
       DO JL = 1,INL
          DO JI=1,PK%NSIZE_P
             IMASK = PK%NR_P(JI)    
             IDEPTH= PK%NWG_LAYER(JI)
             !
             IF(JL<=IDEPTH.AND.IDEPTH/=NUNDEF)THEN
               !
               ! Total available water (m)
               !
               ZTSWI = PEK%XWG(JI,JL)+PEK%XWGI(JI,JL) - KK%XWWILT(JI,JL)
               ZTAW  = KK%XWFC(JI,JL)                 - KK%XWWILT(JI,JL)
               !
               ! ISBA-FR-DG2 comparable soil wetness index, liquid water and ice contents
               !
               ZWORK = MIN(PK%XDZG(JI,JL),MAX(0.0,PK%XDG2(JI)-PK%XDG(JI,JL)+PK%XDZG(JI,JL)))
               !
               DM%XFRD2_TSWI (IMASK) = DM%XFRD2_TSWI (IMASK) + ZWORK * PK%XPATCH(JI) * ZTSWI
               ZPONDSWID2    (IMASK) = ZPONDSWID2    (IMASK) + ZWORK * PK%XPATCH(JI) * ZTAW
               !
               DM%XFRD2_TWG  (IMASK) = DM%XFRD2_TWG  (IMASK) + ZWORK * PK%XPATCH(JI) * PEK%XWG  (JI,JL)
               DM%XFRD2_TWGI (IMASK) = DM%XFRD2_TWGI (IMASK) + ZWORK * PK%XPATCH(JI) * PEK%XWGI (JI,JL)
               ZSUMFRD2      (IMASK) = ZSUMFRD2      (IMASK) + ZWORK * PK%XPATCH(JI)
               !
               ! ISBA-FR-DG3 comparable soil wetness index, liquid water and ice contents
               !
               ZWORK  =MIN(PK%XDZG(JI,JL),MAX(0.0,PK%XDG(JI,JL)-PK%XDG2(JI)))
               !
               DM%XFRD3_TSWI (IMASK) = DM%XFRD3_TSWI (IMASK) + ZWORK * PK%XPATCH(JI) * ZTSWI
               ZPONDSWID3    (IMASK) = ZPONDSWID3    (IMASK) + ZWORK * PK%XPATCH(JI) * ZTAW
               !
               DM%XFRD3_TWG  (IMASK) = DM%XFRD3_TWG  (IMASK) + ZWORK * PK%XPATCH(JI) * PEK%XWG  (JI,JL)
               DM%XFRD3_TWGI (IMASK) = DM%XFRD3_TWGI (IMASK) + ZWORK * PK%XPATCH(JI) * PEK%XWGI (JI,JL)
               ZSUMFRD3      (IMASK) = ZSUMFRD3      (IMASK) + ZWORK * PK%XPATCH(JI)
               !
             ENDIF
             !
          ENDDO
       ENDDO
    ENDDO
!    
    WHERE(ZSUMFRD2(:)>0.0) 
          DM%XFRD2_TWG (:) = DM%XFRD2_TWG (:) / ZSUMFRD2(:)
          DM%XFRD2_TWGI(:) = DM%XFRD2_TWGI(:) / ZSUMFRD2(:)    
    ENDWHERE    
    WHERE(ZPONDSWID2(:)>0.0) 
          DM%XFRD2_TSWI(:) = DM%XFRD2_TSWI(:) / ZPONDSWID2(:)
    ELSEWHERE
          DM%XFRD2_TSWI(:) = XUNDEF
    ENDWHERE    
    WHERE(ZSUMFRD3(:)>0.0) 
          DM%XFRD3_TWG (:) = DM%XFRD3_TWG (:) / ZSUMFRD3(:)
          DM%XFRD3_TWGI(:) = DM%XFRD3_TWGI(:) / ZSUMFRD3(:) 
    ENDWHERE  
    WHERE(ZPONDSWID3(:)>0.0) 
          DM%XFRD3_TSWI(:) = DM%XFRD3_TSWI(:) / ZPONDSWID3(:)
    ELSEWHERE
          DM%XFRD3_TSWI(:) = XUNDEF
    ENDWHERE
    !
    ! ---------------------------
  ENDIF ! End LSURF_MISC_DIF case
  ! -----------------------------
  !
  !----------------------
ELSE ! Force-restore case
  !----------------------
  ! 
  DO JP=1,INP
    PK => NP%AL(JP)
    PEK => NPE%AL(JP)
    DMK => NDM%AL(JP)
    DO JI=1,PK%NSIZE_P
       IMASK = PK%NR_P(JI)    
       !Total soil depth
       ZSUMDG (IMASK) = ZSUMDG (IMASK) + PK%XPATCH(JI) * PK%XDG(JI,2)        
       ZSUMDG2(IMASK) = ZSUMDG2(IMASK) + PK%XPATCH(JI) * PK%XDG(JI,2)        
       !Soil Wetness Index profile
       DM%XSWI (IMASK,1) = DM%XSWI (IMASK,1) + PK%XPATCH(JI) * DMK%XSWI (JI,1)
       DM%XSWI (IMASK,2) = DM%XSWI (IMASK,2) + PK%XPATCH(JI) * DMK%XSWI (JI,2)
       DM%XTSWI(IMASK,1) = DM%XTSWI(IMASK,1) + PK%XPATCH(JI) * DMK%XTSWI(JI,1)
       DM%XTSWI(IMASK,2) = DM%XTSWI(IMASK,2) + PK%XPATCH(JI) * DMK%XTSWI(JI,2)
       !With isba-fr, wwilt and wwfc are homogeneous (the average is direct)
       DM%XSOIL_SWI (IMASK) = DM%XSOIL_SWI (IMASK) + PK%XPATCH(JI) * DMK%XSOIL_SWI( JI)
       DM%XSOIL_TSWI(IMASK) = DM%XSOIL_TSWI(IMASK) + PK%XPATCH(JI) * DMK%XSOIL_TSWI(JI)         
    ENDDO
  ENDDO     
  !     
  IF(IO%CISBA=='3-L')THEN          
    !swi of  third layer
    DO JP=1,INP
       DMK => NDM%AL(JP)
       PK => NP%AL(JP)
       PEK => NPE%AL(JP)    
       DO JI=1,PK%NSIZE_P
          IMASK = PK%NR_P(JI)
          !Total soil depth
          ZSUMDG(IMASK) = ZSUMDG(IMASK) + PK%XPATCH(JI) * (PK%XDG(JI,3)-PK%XDG(JI,2))
          !Soil Wetness Index profile (Remenber: no ice in the third layer of 3-L)
          DM%XSWI (IMASK,3) = DM%XSWI (IMASK,3) + PK%XPATCH(JI) * DMK%XSWI (JI,3)
          DM%XTSWI(IMASK,3) = DM%XTSWI(IMASK,3) + PK%XPATCH(JI) * DMK%XTSWI(JI,3)
       ENDDO
    ENDDO
  ENDIF
  !
  !Soil Water Content (m3.m-3)
  !
  WHERE(ZSUMDG(:)>0.0)
        DM%XSOIL_WG (:) = DM%XSOIL_TWG (:)/(ZSUMDG (:)*XRHOLW)
        DM%XSOIL_WGI(:) = DM%XSOIL_TWGI(:)/(ZSUMDG2(:)*XRHOLW) ! Ice content only in the root zone
  ENDWHERE
  !
  !------------------------------
ENDIF ! End ISBA soil scheme case   
!--------------------------------
!
!-------------------------------------------------------------------------------
!
!       3.     Specific treatement for Crocus
!              ------------------------------
!
IF (GPROSNOW) THEN
   !
   DM%XSNDPT_1DY     (:) = 0.
   DM%XSNDPT_3DY     (:) = 0.
   DM%XSNDPT_5DY     (:) = 0.
   DM%XSNDPT_7DY     (:) = 0.
   DM%XSNSWE_1DY     (:) = 0.
   DM%XSNSWE_3DY     (:) = 0.
   DM%XSNSWE_5DY     (:) = 0.
   DM%XSNSWE_7DY     (:) = 0.
   DM%XSNRAM_SONDE   (:) = 0.
   DM%XSN_WETTHCKN   (:) = 0.
   DM%XSN_REFRZNTHCKN(:) = 0.
   DM%XDEP_HIG       (:) = 0.
   DM%XDEP_MOD       (:) = 0.
   DM%XACC_LEV       (:) = 0.
   DM%XPRO_INF_TYP   (:) = 0.
   !
   IF (DM%LPROBANDS) THEN
      DM%XSPEC_ALB(:,:)=0.
      DM%XDIFF_RATIO(:,:)=0.
   ENDIF 
   !
   DO JP=1,INP
      !
      PK => NP%AL(JP)
      PEK => NPE%AL(JP)
      DMK => NDM%AL(JP)
      !     
      DO JI=1,PK%NSIZE_P
         !
         IMASK = PK%NR_P(JI)
         !
         DM%XSNDPT_1DY     (IMASK) = DM%XSNDPT_1DY     (IMASK) + PK%XPATCH(JI) * DMK%XSNDPT_1DY     (JI)
         DM%XSNDPT_3DY     (IMASK) = DM%XSNDPT_3DY     (IMASK) + PK%XPATCH(JI) * DMK%XSNDPT_3DY     (JI)
         DM%XSNDPT_5DY     (IMASK) = DM%XSNDPT_5DY     (IMASK) + PK%XPATCH(JI) * DMK%XSNDPT_5DY     (JI)
         DM%XSNDPT_7DY     (IMASK) = DM%XSNDPT_7DY     (IMASK) + PK%XPATCH(JI) * DMK%XSNDPT_7DY     (JI)
         DM%XSNSWE_1DY     (IMASK) = DM%XSNSWE_1DY(     IMASK) + PK%XPATCH(JI) * DMK%XSNSWE_1DY     (JI)
         DM%XSNSWE_3DY     (IMASK) = DM%XSNSWE_3DY     (IMASK) + PK%XPATCH(JI) * DMK%XSNSWE_3DY     (JI)
         DM%XSNSWE_5DY     (IMASK) = DM%XSNSWE_5DY     (IMASK) + PK%XPATCH(JI) * DMK%XSNSWE_5DY     (JI)
         DM%XSNSWE_7DY     (IMASK) = DM%XSNSWE_7DY     (IMASK) + PK%XPATCH(JI) * DMK%XSNSWE_7DY     (JI)
         DM%XSNRAM_SONDE   (IMASK) = DM%XSNRAM_SONDE   (IMASK) + PK%XPATCH(JI) * DMK%XSNRAM_SONDE   (JI)
         DM%XSN_WETTHCKN   (IMASK) = DM%XSN_WETTHCKN   (IMASK) + PK%XPATCH(JI) * DMK%XSN_WETTHCKN   (JI)
         DM%XSN_REFRZNTHCKN(IMASK) = DM%XSN_REFRZNTHCKN(IMASK) + PK%XPATCH(JI) * DMK%XSN_REFRZNTHCKN(JI)
         !The following does not make really sense
         DM%XDEP_HIG       (IMASK) = DM%XDEP_HIG       (IMASK) + PK%XPATCH(JI) * DMK%XDEP_HIG       (JI)
         DM%XDEP_MOD       (IMASK) = DM%XDEP_MOD       (IMASK) + PK%XPATCH(JI) * DMK%XDEP_MOD       (JI)
         DM%XACC_LEV       (IMASK) = DM%XACC_LEV       (IMASK) + PK%XPATCH(JI) * DMK%XACC_LEV       (JI)
         DM%XPRO_INF_TYP   (IMASK) = DM%XPRO_INF_TYP   (IMASK) + PK%XPATCH(JI) * DMK%XPRO_INF_TYP   (JI)
         !        
         ! Ugly way to keep XUNDEF instead of 0 in case of no snow
         IF ((DMK%XTWSNOW(JI)<=0.0).AND.GPROSNOW) THEN
            DM%XDEP_HIG       (IMASK) = XUNDEF
            DM%XDEP_MOD       (IMASK) = XUNDEF
            DM%XACC_LEV       (IMASK) = 4
            DM%XPRO_INF_TYP   (IMASK) = 6
         ENDIF
         !
      ENDDO
      !
   ENDDO
   !
ENDIF
!
IF(NPE%AL(1)%TSNOW%SCHEME=="CRO") THEN

  IF ( IO%LSNOWSYTRON ) THEN

    DM%XSYTMASS(:) = 0.
    DM%XSYTMASSC(:) = 0.

    DO JP=1,INP
      PK => NP%AL(JP)
      DMK => NDM%AL(JP)
      !   
      DO JI=1,PK%NSIZE_P
        IMASK = PK%NR_P(JI)
        !
        !     SYTRON total outputs
        DM%XSYTMASS(IMASK) = DM%XSYTMASS(IMASK) + PK%XPATCH(JI) * DMK%XSYTMASS(JI)
        DM%XSYTMASSC(IMASK) = DM%XSYTMASSC(IMASK) + PK%XPATCH(JI) * DMK%XSYTMASSC(JI)
        !
      ENDDO
      !
    ENDDO

  ENDIF

  IF ( IO%LSNOWMAK_BOOL ) THEN

    DM%XPRODCOUNT(:) = 0.

    DO JP=1,INP
      PK => NP%AL(JP)
      DMK => NDM%AL(JP)
      !   
      DO JI=1,PK%NSIZE_P
        IMASK = PK%NR_P(JI)
        !
        !     Snow production counter output
        DM%XPRODCOUNT(IMASK) = DM%XPRODCOUNT(IMASK) + PK%XPATCH(JI) * DMK%XPRODCOUNT(JI)
        !
      ENDDO
      !
    ENDDO
    !
  ENDIF

ENDIF
!
!
!-------------------------------------------------------------------------------
!
!       4.     Specific ISBA-CC
!              ----------------
!
!* soil turn-over time (s)
!
IF(IO%CRESPSL=='CNT'.OR.IO%CRESPSL=='DIF')THEN
  !
  ZCSOILTOT    (:,:) = 0.0
  DM%XTSOILPOOL(:,:) = 0.0
  ! 
  DO JP=1,INP
     PK => NP%AL(JP)
     PEK => NPE%AL(JP)
     DMK => NDM%AL(JP)
     DO JI=1,PK%NSIZE_P
        IMASK = PK%NR_P(JI) 
        DO JL=1,IO%NNSOILCARB
           ZCSOILTOT    (IMASK,JL) = ZCSOILTOT    (IMASK,JL) + PK%XPATCH(JI) * PEK%XSOILCARB(JI,JL)
           DM%XTSOILPOOL(IMASK,JL) = DM%XTSOILPOOL(IMASK,JL) + PK%XPATCH(JI) * PEK%XSOILCARB(JI,JL) * DMK%XTSOILPOOL(JI,JL)
        ENDDO
      ENDDO
  ENDDO
  !
  WHERE(ZCSOILTOT(:,:)>0.0)
       DM%XTSOILPOOL(:,:) = DM%XTSOILPOOL(:,:) / ZCSOILTOT(:,:)
  ENDWHERE
  !
ENDIF
!
!* Fire scheme
!
IF(IO%LFIRE)THEN
  !
  DM%XFIREFRA(:) = 0.0
  !
  DO JP=1,INP
     PK => NP%AL(JP)
     DMK => NDM%AL(JP)
     DO JI=1,PK%NSIZE_P
        IMASK = PK%NR_P(JI)  
        DM%XFIREFRA(IMASK) = DM%XFIREFRA(IMASK) + PK%XPATCH(JI) * DMK%XFIREFRA(JI)
      ENDDO
  ENDDO
  !
ENDIF        
!
!* Soil gas scheme
!
IF (IO%LSOILGAS) THEN
   !
   DM%XSOILO2  (:,:) = 0.
   DM%XSOILCO2 (:,:) = 0.
   DM%XSOILCH4 (:,:) = 0.
   DM%XOXIC_O2 (:,:) = 0.
   DM%XOXIC_CO2(:,:) = 0.
   DM%XMG_CH4  (:,:) = 0.
   DM%XMT_CH4  (:,:) = 0.
   !
   DO JP=1,INP
      PK => NP%AL(JP)
      DMK => NDM%AL(JP)
      DO JL=1,INL
         DO JI=1,PK%NSIZE_P
            IMASK  = PK%NR_P(JI) 
            IDEPTH = PK%NWG_LAYER(JI)
            IF(JL<=IDEPTH.AND.IDEPTH/=NUNDEF)THEN
              !                  
              DM%XSOILO2  (IMASK,JL) = DM%XSOILO2  (IMASK,JL) + PK%XPATCH(JI) * DMK%XSOILO2  (JI,JL)
              DM%XSOILCO2 (IMASK,JL) = DM%XSOILCO2 (IMASK,JL) + PK%XPATCH(JI) * DMK%XSOILCO2 (JI,JL)
              DM%XSOILCH4 (IMASK,JL) = DM%XSOILCH4 (IMASK,JL) + PK%XPATCH(JI) * DMK%XSOILCH4 (JI,JL)
              !
              DM%XOXIC_O2 (IMASK,JL) = DM%XOXIC_O2 (IMASK,JL) + PK%XPATCH(JI) * DMK%XOXIC_O2 (JI,JL)
              DM%XOXIC_CO2(IMASK,JL) = DM%XOXIC_CO2(IMASK,JL) + PK%XPATCH(JI) * DMK%XOXIC_CO2(JI,JL)
              DM%XMG_CH4  (IMASK,JL) = DM%XMG_CH4  (IMASK,JL) + PK%XPATCH(JI) * DMK%XMG_CH4  (JI,JL)
              DM%XMT_CH4  (IMASK,JL) = DM%XMT_CH4  (IMASK,JL) + PK%XPATCH(JI) * DMK%XMT_CH4  (JI,JL)
              !              
            ENDIF
         ENDDO
      ENDDO
   ENDDO
   !
ENDIF
!
!-------------------------------------------------------------------------------
!
!       5.     Nudging Increment of water and snow (kg.m-2)
!              --------------------------------------------
!
!
IF(IO%CNUDG_WG/='DEF')THEN
  !
  DM%XNUDGINCSM (:  ) = 0.0
  DM%XNUDGINCSML(:,:) = 0.0
  !
  DO JP=1,INP
     PK => NP%AL(JP)
     DMK => NDM%AL(JP)
     DO JI=1,PK%NSIZE_P
        IMASK = PK%NR_P(JI) 
        DM%XNUDGINCSM(IMASK) = DM%XNUDGINCSM(IMASK) + PK%XPATCH(JI) * DMK%XNUDGINCSM(JI)
     ENDDO
  ENDDO
  !
  DO JP=1,INP
     PK => NP%AL(JP)
     DMK => NDM%AL(JP)
     DO JL=1,INL
        DO JI=1,PK%NSIZE_P
           IMASK = PK%NR_P(JI) 
           DM%XNUDGINCSML(IMASK,JL) = DM%XNUDGINCSML(IMASK,JL) + PK%XPATCH(JI) * DMK%XNUDGINCSML(JI,JL)
        ENDDO
     ENDDO
  ENDDO  
  !
ENDIF
!
IF(IO%LNUDG_SWE)THEN
  !
  DM%XNUDGINCSWE(:  ) = 0.0
  !  
  DO JP=1,INP
     PK => NP%AL(JP)
     DMK => NDM%AL(JP)
     DO JI=1,PK%NSIZE_P
        IMASK = PK%NR_P(JI) 
        DM%XNUDGINCSWE(IMASK) = DM%XNUDGINCSWE(IMASK) + PK%XPATCH(JI) * DMK%XNUDGINCSWE(JI)
     ENDDO
  ENDDO
  !
ENDIF
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('AVERAGE_DIAG_MISC_ISBA_N',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE AVERAGE_DIAG_MISC_ISBA_n
