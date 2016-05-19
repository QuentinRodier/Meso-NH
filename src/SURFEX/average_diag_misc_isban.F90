!     #############################
      SUBROUTINE AVERAGE_DIAG_MISC_ISBA_n
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
!!	P. Le Moigne           * Meteo-France *
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
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_SURF_PAR,         ONLY : XUNDEF, NUNDEF
!
USE MODD_CSTS,             ONLY : XRHOLW
!
USE MODD_ISBA_n,           ONLY : CISBA, XPATCH, NGROUND_LAYER, &
                                  XDG, XDZG, XWG, XWGI,         &
                                  NSIZE_NATURE_P, NWG_LAYER,    &
                                  XDG2, XLAI, XROOTFRAC, XDROOT
!
USE MODD_DIAG_MISC_ISBA_n, ONLY : LSURF_MISC_BUDGET,      &
                                  LSURF_MISC_DIF,         &
                                  XHV  , XAVG_HV   ,      &
                                  XSWI , XAVG_SWI  ,      &
                                  XTSWI, XAVG_TSWI ,      &
                                  XDPSNG, XAVG_PSNG ,     &
                                  XDPSNV, XAVG_PSNV ,     &
                                  XDPSN , XAVG_PSN  ,     &
                                  XALBT, XAVG_ALBT ,      &
                                  XDFFG , XAVG_FFG  ,     &
                                  XDFFV , XAVG_FFV  ,     &
                                  XDFF  , XAVG_FF   ,     &
                                  XDFSAT, XAVG_FSAT   ,   &
                                  XTWSNOW, XAVG_TWSNOW ,  &
                                  XTDSNOW, XAVG_TDSNOW ,  &
                                  XTTSNOW, XAVG_TTSNOW ,  &
                                  XSOIL_TSWI, XSOIL_TWG,  &
                                  XSOIL_TWGI, XSURF_TSWI, &
                                  XSURF_TWG, XSURF_TWGI,  &
                                  XROOT_TSWI, XROOT_TWG,  &
                                  XROOT_TWGI, XFRD2_TSWI, &
                                  XFRD2_TWG, XFRD2_TWGI,  &
                                  XFRD3_TSWI, XFRD3_TWG,  &
                                  XFRD3_TWGI,             &
                                  XALT, XAVG_ALT,         &
                                  XFLT, XAVG_FLT,         &
                                  XAVG_LAI
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
INTEGER                         :: JJ        ! grid-cell loop counter
INTEGER                         :: JPATCH    ! tile loop counter
INTEGER                         :: JLAYER    ! layer loop counter
REAL, DIMENSION(SIZE(XPATCH,1)) :: ZSUMPATCH
REAL, DIMENSION(SIZE(XPATCH,1)) :: ZSUMDG, ZSNOW, ZSUMSURF, ZSUMROOT, &
                                   ZSUMFRD2, ZSUMFRD3, ZPONDF2
REAL, DIMENSION(SIZE(XPATCH,1),SIZE(XPATCH,2))       :: ZLAI, ZDROOT
REAL, DIMENSION(SIZE(XDG,1),SIZE(XDG,2),SIZE(XDG,3)) :: ZROOTFRAC
REAL                            :: ZWORK
INTEGER                         :: INI,INP,IDEPTH,IWORK
!
REAL, DIMENSION(SIZE(XDG,1),SIZE(XDG,2)) :: ZPOND
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!       0.     Initialization
!              --------------
!
IF (LHOOK) CALL DR_HOOK('AVERAGE_DIAG_MISC_ISBA_N',0,ZHOOK_HANDLE)
!
IF (.NOT.LSURF_MISC_BUDGET) THEN
   IF (LHOOK) CALL DR_HOOK('AVERAGE_DIAG_MISC_ISBA_N',1,ZHOOK_HANDLE)
   RETURN
ENDIF
!
INI=SIZE(XPATCH,1)
INP=SIZE(XPATCH,2)
!
ZSUMPATCH(:) = 0.0
DO JPATCH=1,INP
   DO JJ=1,INI
      ZSUMPATCH(JJ) = ZSUMPATCH(JJ) + XPATCH(JJ,JPATCH)
   END DO
END DO
!
ZSUMSURF(:)=0.0
ZSUMROOT(:)=0.0
ZSUMFRD2(:)=0.0
ZSUMFRD3(:)=0.0
ZSUMDG  (:)=0.0
ZSNOW   (:)=0.0
ZPONDF2 (:)=0.0
!
WHERE(XLAI(:,:)/=XUNDEF)
      ZLAI(:,:)=XLAI(:,:)
ELSEWHERE
      ZLAI(:,:)=0.0
ENDWHERE
!
!-------------------------------------------------------------------------------
!
!       1.     Surface Miscellaneous terms
!              ---------------------------
!
XAVG_HV  (:)   = 0.
XAVG_PSNG(:)   = 0.
XAVG_PSNV(:)   = 0.
XAVG_PSN (:)   = 0.
XAVG_ALBT(:)   = 0.
XAVG_SWI (:,:) = 0.
XAVG_TSWI(:,:) = 0.
XAVG_FSAT(:)   = 0.
XAVG_FFG (:)   = 0.
XAVG_FFV (:)   = 0.
XAVG_FF  (:)   = 0.
XAVG_TWSNOW(:) = 0.
XAVG_TDSNOW(:) = 0.  
XAVG_TTSNOW(:) = 0.
XAVG_LAI   (:) = 0.
!   
XSOIL_TSWI(:)  = 0.
XSOIL_TWG  (:) = 0.
XSOIL_TWGI (:) = 0.
! 
IF(CISBA=='DIF')THEN
!        
  XAVG_ALT   (:) = 0. 
  XAVG_FLT   (:) = 0. 
!
ENDIF

IF(CISBA=='DIF'.AND.LSURF_MISC_DIF)THEN
!
  WHERE(XDROOT(:,:)/=XUNDEF)
        ZDROOT(:,:)=XDROOT(:,:)
  ELSEWHERE
        ZDROOT(:,:)=0.0
  ENDWHERE
!
  XSURF_TSWI (:) = 0.
  XSURF_TWG  (:) = 0.
  XSURF_TWGI (:) = 0.
!  
  XROOT_TSWI (:) = 0.
  XROOT_TWG  (:) = 0.
  XROOT_TWGI (:) = 0.
!   
  XFRD2_TSWI (:) = 0.
  XFRD2_TWG  (:) = 0.
  XFRD2_TWGI (:) = 0.
!   
  XFRD3_TSWI (:) = 0.
  XFRD3_TWG  (:) = 0.
  XFRD3_TWGI (:) = 0.
!  
ENDIF
!
DO JPATCH=1,INP
!
!cdir nodep
  DO JJ=1,INI
!
    IF (ZSUMPATCH(JJ) > 0.) THEN
!
!     Halstead coefficient
      XAVG_HV(JJ) = XAVG_HV(JJ) + XPATCH(JJ,JPATCH) * XHV(JJ,JPATCH)
!
!     Snow fractions
      XAVG_PSNG(JJ) = XAVG_PSNG(JJ) + XPATCH(JJ,JPATCH) * XDPSNG(JJ,JPATCH)
      XAVG_PSNV(JJ) = XAVG_PSNV(JJ) + XPATCH(JJ,JPATCH) * XDPSNV(JJ,JPATCH)
      XAVG_PSN (JJ) = XAVG_PSN (JJ) + XPATCH(JJ,JPATCH) * XDPSN (JJ,JPATCH)
!
!     Saturated fraction
      XAVG_FSAT (JJ) = XAVG_FSAT (JJ) + XPATCH(JJ,JPATCH) * XDFSAT (JJ,JPATCH)
!
!     Flood fractions
      XAVG_FFG(JJ) = XAVG_FFG(JJ) + XPATCH(JJ,JPATCH) * XDFFG(JJ,JPATCH)
      XAVG_FFV(JJ) = XAVG_FFV(JJ) + XPATCH(JJ,JPATCH) * XDFFV(JJ,JPATCH)
      XAVG_FF (JJ) = XAVG_FF (JJ) + XPATCH(JJ,JPATCH) * XDFF (JJ,JPATCH)
!
!     Total albedo
      XAVG_ALBT(JJ) = XAVG_ALBT(JJ) + XPATCH(JJ,JPATCH) * XALBT (JJ,JPATCH)
!
!     Total LAI
      XAVG_LAI (JJ) = XAVG_LAI(JJ)  + XPATCH(JJ,JPATCH) * ZLAI (JJ,JPATCH)
!      
!     Snow total outputs
      XAVG_TWSNOW(JJ) = XAVG_TWSNOW(JJ) + XPATCH(JJ,JPATCH) * XTWSNOW(JJ,JPATCH)
      XAVG_TDSNOW(JJ) = XAVG_TDSNOW(JJ) + XPATCH(JJ,JPATCH) * XTDSNOW(JJ,JPATCH)
!      
      IF (XTWSNOW(JJ,JPATCH)>0.0) THEN
         XAVG_TTSNOW(JJ) = XAVG_TTSNOW(JJ) + XPATCH(JJ,JPATCH) * XTTSNOW(JJ,JPATCH)
         ZSNOW      (JJ) = ZSNOW      (JJ) + XPATCH(JJ,JPATCH)
      ENDIF
!
    ENDIF
!
  ENDDO
!
ENDDO
!
!-------------------------------------------------------------------------------
!
!       2.     Specific treatement following CISBA option
!              ------------------------------------------
!
!   Soil Wetness Index profile, Total Soil Wetness Index and 
!   Total Soil Water Content (Liquid+Solid) and Total Frozen Content
!
!---------------------------------------------
IF(CISBA=='DIF')THEN ! DIF case
!---------------------------------------------
!
! Active and Frozen layers thickness
  DO JPATCH=1,INP
     DO JJ=1,INI
        IF (ZSUMPATCH(JJ) > 0.) THEN
           XAVG_ALT(JJ) = XAVG_ALT (JJ) + XPATCH(JJ,JPATCH) * XALT(JJ,JPATCH)
           XAVG_FLT(JJ) = XAVG_FLT (JJ) + XPATCH(JJ,JPATCH) * XFLT(JJ,JPATCH)
        ENDIF
     ENDDO
  ENDDO
!    
  ZPOND(:,:)=0.0
  DO JPATCH=1,INP      
     IF(NSIZE_NATURE_P(JPATCH) > 0 )THEN
       DO JLAYER = 1,NGROUND_LAYER
!         cdir nodep 
          DO JJ=1,INI
             IDEPTH=NWG_LAYER(JJ,JPATCH)
             IF(JLAYER<=IDEPTH.AND.IDEPTH/=NUNDEF)THEN
               ZWORK=XDZG(JJ,JLAYER,JPATCH)
               !Soil Wetness Index profile
               XAVG_SWI (JJ,JLAYER) = XAVG_SWI (JJ,JLAYER)+ZWORK*XPATCH(JJ,JPATCH)*XSWI (JJ,JLAYER,JPATCH) 
               XAVG_TSWI(JJ,JLAYER) = XAVG_TSWI(JJ,JLAYER)+ZWORK*XPATCH(JJ,JPATCH)*XTSWI(JJ,JLAYER,JPATCH)
               ZPOND    (JJ,JLAYER) = ZPOND    (JJ,JLAYER)+ZWORK*XPATCH(JJ,JPATCH)
               !Total soil wetness index, total water and ice contents
               XSOIL_TSWI(JJ) = XSOIL_TSWI(JJ) + ZWORK * XPATCH(JJ,JPATCH) * XTSWI(JJ,JLAYER,JPATCH)
               ZSUMDG    (JJ) = ZSUMDG    (JJ) + ZWORK * XPATCH(JJ,JPATCH)
               XSOIL_TWG (JJ) = XSOIL_TWG (JJ) + ZWORK * XPATCH(JJ,JPATCH) * (XWG(JJ,JLAYER,JPATCH)+XWGI(JJ,JLAYER,JPATCH))
               XSOIL_TWGI(JJ) = XSOIL_TWGI(JJ) + ZWORK * XPATCH(JJ,JPATCH) * XWGI(JJ,JLAYER,JPATCH)                     
             ENDIF
          ENDDO
       ENDDO
     ENDIF
  ENDDO
!  
  WHERE(ZPOND(:,:)> 0.)
        XAVG_SWI (:,:) = XAVG_SWI (:,:) / ZPOND(:,:)
        XAVG_TSWI(:,:) = XAVG_TSWI(:,:) / ZPOND(:,:)
  ELSEWHERE
        XAVG_SWI (:,:) = XUNDEF
        XAVG_TSWI(:,:) = XUNDEF
  ENDWHERE
!
! ---------------------------------------------
  IF(LSURF_MISC_DIF)THEN ! LSURF_MISC_DIF case
! ---------------------------------------------
!
    ZROOTFRAC(:,1,:)=XROOTFRAC(:,1,:)
    DO JPATCH=1,INP      
      IF(NSIZE_NATURE_P(JPATCH) > 0 )THEN
        DO JLAYER = 2,NGROUND_LAYER
!          cdir nodep 
           DO JJ=1,INI
              ZROOTFRAC(JJ,JLAYER,JPATCH) = XROOTFRAC(JJ,JLAYER,JPATCH) - XROOTFRAC(JJ,JLAYER-1,JPATCH)            
           ENDDO
        ENDDO
      ENDIF
    ENDDO
!
!   Surface soil wetness index, liquid water and ice contents
    DO JPATCH=1,INP
       DO JJ=1,INI
          IF(ZSUMPATCH(JJ) > 0.)THEN
            XSURF_TSWI(JJ) = XSURF_TSWI(JJ) + XPATCH(JJ,JPATCH) * XDG(JJ,1,JPATCH) * XTSWI(JJ,1,JPATCH)
            XSURF_TWG (JJ) = XSURF_TWG (JJ) + XPATCH(JJ,JPATCH) * XDG(JJ,1,JPATCH) * XWG (JJ,1,JPATCH)
            XSURF_TWGI(JJ) = XSURF_TWGI(JJ) + XPATCH(JJ,JPATCH) * XDG(JJ,1,JPATCH) * XWGI(JJ,1,JPATCH)
            ZSUMSURF  (JJ) = ZSUMSURF  (JJ) + XPATCH(JJ,JPATCH) * XDG(JJ,1,JPATCH)
          ENDIF
       ENDDO
    ENDDO
!    
    DO JPATCH=1,INP
!  
      IF (NSIZE_NATURE_P(JPATCH) == 0 ) CYCLE
!     
      DO JLAYER = 1,NGROUND_LAYER
!       cdir nodep 
        DO JJ=1,INI
          IDEPTH=NWG_LAYER(JJ,JPATCH)
          IF(JLAYER<=IDEPTH.AND.IDEPTH/=NUNDEF)THEN
            !
            ! Root zone soil wetness index,  Total water and ice contents
            ZWORK=MIN(XDZG(JJ,JLAYER,JPATCH),MAX(0.0,ZDROOT(JJ,JPATCH)-XDG(JJ,JLAYER,JPATCH)+XDZG(JJ,JLAYER,JPATCH)))
            IF(ZDROOT(JJ,JPATCH)>0.0)THEN
              XROOT_TSWI (JJ) = XROOT_TSWI (JJ) + ZWORK * XPATCH(JJ,JPATCH) * XTSWI(JJ,JLAYER,JPATCH)
              ZSUMROOT   (JJ) = ZSUMROOT   (JJ) + ZWORK * XPATCH(JJ,JPATCH)
            ENDIF
            XROOT_TWG  (JJ) = XROOT_TWG  (JJ) + ZWORK * XPATCH(JJ,JPATCH) * (XWG (JJ,JLAYER,JPATCH)+XWGI(JJ,JLAYER,JPATCH))
            XROOT_TWGI (JJ) = XROOT_TWGI (JJ) + ZWORK * XPATCH(JJ,JPATCH) *  XWGI(JJ,JLAYER,JPATCH)
            !
            ! ISBA-FR-DG2 comparable soil wetness index, liquid water and ice contents
            ZWORK=MIN(XDZG(JJ,JLAYER,JPATCH),MAX(0.0,XDG2(JJ,JPATCH)-XDG(JJ,JLAYER,JPATCH)+XDZG(JJ,JLAYER,JPATCH)))
            XFRD2_TSWI (JJ) = XFRD2_TSWI (JJ) + ZWORK * XPATCH(JJ,JPATCH) * XTSWI(JJ,JLAYER,JPATCH)
            XFRD2_TWG  (JJ) = XFRD2_TWG  (JJ) + ZWORK * XPATCH(JJ,JPATCH) * XWG  (JJ,JLAYER,JPATCH)
            XFRD2_TWGI (JJ) = XFRD2_TWGI (JJ) + ZWORK * XPATCH(JJ,JPATCH) * XWGI (JJ,JLAYER,JPATCH)
            ZSUMFRD2   (JJ) = ZSUMFRD2   (JJ) + ZWORK * XPATCH(JJ,JPATCH)
            !
            ! ISBA-FR-DG3 comparable soil wetness index, liquid water and ice contents
            ZWORK=MIN(XDZG(JJ,JLAYER,JPATCH),MAX(0.0,XDG(JJ,JLAYER,JPATCH)-XDG2(JJ,JPATCH)))
            XFRD3_TSWI (JJ) = XFRD3_TSWI (JJ) + ZWORK * XPATCH(JJ,JPATCH) * XTSWI(JJ,JLAYER,JPATCH)
            XFRD3_TWG  (JJ) = XFRD3_TWG  (JJ) + ZWORK * XPATCH(JJ,JPATCH) * XWG  (JJ,JLAYER,JPATCH)
            XFRD3_TWGI (JJ) = XFRD3_TWGI (JJ) + ZWORK * XPATCH(JJ,JPATCH) * XWGI (JJ,JLAYER,JPATCH)
            ZSUMFRD3   (JJ) = ZSUMFRD3   (JJ) + ZWORK * XPATCH(JJ,JPATCH)
            !
          ENDIF
        ENDDO
      ENDDO
!
    ENDDO
!
    WHERE(ZSUMSURF(:)>0.0) 
          XSURF_TSWI (:) = XSURF_TSWI (:) / ZSUMSURF(:)
          XSURF_TWG  (:) = XSURF_TWG  (:) / ZSUMSURF(:)
          XSURF_TWGI (:) = XSURF_TWGI (:) / ZSUMSURF(:)
    ELSEWHERE
          XSURF_TSWI (:) = XUNDEF
          XSURF_TWG  (:) = XUNDEF
          XSURF_TWGI (:) = XUNDEF
    ENDWHERE 
!    
    WHERE(ZSUMFRD2(:)>0.0) 
          XFRD2_TSWI (:) = XFRD2_TSWI (:) / ZSUMFRD2(:)
          XFRD2_TWG  (:) = XFRD2_TWG  (:) / ZSUMFRD2(:)
          XFRD2_TWGI (:) = XFRD2_TWGI (:) / ZSUMFRD2(:)          
    ELSEWHERE
          XFRD2_TSWI (:) = XUNDEF
          XFRD2_TWG  (:) = XUNDEF
          XFRD2_TWGI (:) = XUNDEF 
    ENDWHERE 
!    
    WHERE(ZSUMFRD3(:)>0.0) 
          XFRD3_TSWI (:) = XFRD3_TSWI (:) / ZSUMFRD3(:)
          XFRD3_TWG  (:) = XFRD3_TWG  (:) / ZSUMFRD3(:)
          XFRD3_TWGI (:) = XFRD3_TWGI (:) / ZSUMFRD3(:) 
    ELSEWHERE
          XFRD3_TSWI (:) = XUNDEF
          XFRD3_TWG  (:) = XUNDEF
          XFRD3_TWGI (:) = XUNDEF
    ENDWHERE
!
    WHERE(ZSUMROOT(:)>0.0) 
          XROOT_TSWI (:) = XROOT_TSWI (:) / ZSUMROOT(:)
    ELSEWHERE
          XROOT_TSWI (:) = XUNDEF
    ENDWHERE
!
    XROOT_TWG  (:) = XROOT_TWG  (:) * XRHOLW
    XROOT_TWGI (:) = XROOT_TWGI (:) * XRHOLW  
!
! ---------------------------------------------
  ENDIF ! End LSURF_MISC_DIF case
! ---------------------------------------------
!
!---------------------------------------------
ELSE ! Force-restore case
!---------------------------------------------
! 
  DO JPATCH=1,INP
     DO JJ=1,INI     
        IF(ZSUMPATCH(JJ) > 0.)THEN
!
          XAVG_SWI (JJ,1) = XAVG_SWI (JJ,1) + XPATCH(JJ,JPATCH) * XSWI (JJ,1,JPATCH)
          XAVG_SWI (JJ,2) = XAVG_SWI (JJ,2) + XPATCH(JJ,JPATCH) * XSWI (JJ,2,JPATCH)
          XAVG_TSWI(JJ,1) = XAVG_TSWI(JJ,1) + XPATCH(JJ,JPATCH) * XTSWI(JJ,1,JPATCH)
          XAVG_TSWI(JJ,2) = XAVG_TSWI(JJ,2) + XPATCH(JJ,JPATCH) * XTSWI(JJ,2,JPATCH)
!
          XSOIL_TSWI(JJ) = XSOIL_TSWI(JJ) + XPATCH(JJ,JPATCH) * XDG (JJ,2,JPATCH) * XTSWI(JJ,2,JPATCH)
          XSOIL_TWG (JJ) = XSOIL_TWG (JJ) + XPATCH(JJ,JPATCH) * XDG (JJ,2,JPATCH) * (XWG(JJ,2,JPATCH)+XWGI(JJ,2,JPATCH))
          XSOIL_TWGI(JJ) = XSOIL_TWGI(JJ) + XPATCH(JJ,JPATCH) * XDG (JJ,2,JPATCH) * XWGI(JJ,2,JPATCH) 
! 
          ZSUMDG    (JJ) = ZSUMDG    (JJ) + XPATCH(JJ,JPATCH) * XDG(JJ,NGROUND_LAYER,JPATCH)        
!          
       ENDIF
     ENDDO
  ENDDO     
!     
  IF(CISBA=='3-L')THEN
!          
    ZPOND(:,:)=0.0
    DO JPATCH=1,INP
       DO JJ=1,SIZE(XPATCH,1)        
          IF(ZSUMPATCH(JJ) > 0.)THEN
!
            ZWORK=MAX(0.0,XDG(JJ,3,JPATCH)-XDG(JJ,2,JPATCH))
!
!           Remenber: no ice in the third layer of 3-L
            ZPOND     (JJ,3) = ZPOND     (JJ,3) + XPATCH(JJ,JPATCH) * ZWORK
            XAVG_SWI  (JJ,3) = XAVG_SWI  (JJ,3) + XPATCH(JJ,JPATCH) * ZWORK * XSWI (JJ,3,JPATCH)
            XAVG_TSWI (JJ,3) = XAVG_TSWI (JJ,3) + XPATCH(JJ,JPATCH) * ZWORK * XTSWI(JJ,3,JPATCH)
            XSOIL_TSWI(JJ  ) = XSOIL_TSWI(JJ  ) + XPATCH(JJ,JPATCH) * ZWORK * XTSWI(JJ,3,JPATCH)  
            XSOIL_TWG (JJ  ) = XSOIL_TWG (JJ  ) + XPATCH(JJ,JPATCH) * ZWORK * XWG  (JJ,3,JPATCH)  
!
          ENDIF
       ENDDO
    ENDDO
!
    WHERE(ZPOND(:,3)>0.0)
          XAVG_SWI (:,3) = XAVG_SWI (:,3) / ZPOND(:,3)
          XAVG_TSWI(:,3) = XAVG_TSWI(:,3) / ZPOND(:,3)
    ELSEWHERE
          XAVG_SWI (:,3) = XUNDEF
          XAVG_TSWI(:,3) = XUNDEF
    ENDWHERE
!
  ENDIF
  
!
!---------------------------------------------
ENDIF ! End ISBA soil scheme case   
!---------------------------------------------
!
!       3.     Final computation for grid-cell diag
!              ------------------------------------
!
!Total Soil Wetness Index
WHERE(ZSUMDG(:)>0.0)XSOIL_TSWI(:) = XSOIL_TSWI(:)/ZSUMDG(:)
        !Total Soil Water Content (Liquid+Solid) and Total Frozen Content (kg/m2)
XSOIL_TWG (:)= XSOIL_TWG (:) * XRHOLW
XSOIL_TWGI(:)= XSOIL_TWGI(:) * XRHOLW
!
! Snow temperature  
WHERE(ZSNOW(:)>0.0)
      XAVG_TTSNOW(:) = XAVG_TTSNOW(:)/ZSNOW(:)
ELSEWHERE
      XAVG_TTSNOW(:) = XUNDEF
ENDWHERE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('AVERAGE_DIAG_MISC_ISBA_N',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE AVERAGE_DIAG_MISC_ISBA_n
