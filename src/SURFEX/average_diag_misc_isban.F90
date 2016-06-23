!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #############################
      SUBROUTINE AVERAGE_DIAG_MISC_ISBA_n (DGMI, I)
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
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
!
USE MODD_DIAG_MISC_ISBA_n, ONLY : DIAG_MISC_ISBA_t
USE MODD_ISBA_n, ONLY : ISBA_t
!
USE MODD_SURF_PAR,         ONLY : XUNDEF, NUNDEF
!
USE MODD_CSTS,             ONLY : XRHOLW
!
!
!
USE MODI_COMPUT_COLD_LAYERS_THICK
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!
TYPE(DIAG_MISC_ISBA_t), INTENT(INOUT) :: DGMI
TYPE(ISBA_t), INTENT(INOUT) :: I
!
INTEGER                         :: JJ        ! grid-cell loop counter
INTEGER                         :: JPATCH    ! tile loop counter
INTEGER                         :: JLAYER    ! layer loop counter
REAL, DIMENSION(SIZE(I%XPATCH,1)) :: ZSUMPATCH
REAL, DIMENSION(SIZE(I%XPATCH,1)) :: ZSUMDG, ZSNOW, ZSUMFRD2, ZSUMFRD3, ZPONDF2
REAL, DIMENSION(SIZE(I%XPATCH,1),SIZE(I%XPATCH,2)) :: ZLAI
REAL                            :: ZWORK
INTEGER                         :: INI,INP,IDEPTH,IWORK
!
REAL, DIMENSION(SIZE(I%XDG,1),SIZE(I%XDG,2)) :: ZPOND, ZTG, ZDG
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
IF (.NOT.DGMI%LSURF_MISC_BUDGET) THEN
   IF (LHOOK) CALL DR_HOOK('AVERAGE_DIAG_MISC_ISBA_N',1,ZHOOK_HANDLE)
   RETURN
ENDIF
!
INI=SIZE(I%XPATCH,1)
INP=SIZE(I%XPATCH,2)
!
ZSUMPATCH(:) = 0.0
DO JPATCH=1,INP
   DO JJ=1,INI
      ZSUMPATCH(JJ) = ZSUMPATCH(JJ) + I%XPATCH(JJ,JPATCH)
   END DO
END DO
!
ZSUMFRD2(:)=0.0
ZSUMFRD3(:)=0.0
ZSUMDG  (:)=0.0
ZSNOW   (:)=0.0
ZPONDF2 (:)=0.0
!
WHERE(I%XLAI(:,:)/=XUNDEF)
      ZLAI(:,:)=I%XLAI(:,:)
ELSEWHERE
      ZLAI(:,:)=0.0
ENDWHERE
!
!-------------------------------------------------------------------------------
!
!       1.     Surface Miscellaneous terms
!              ---------------------------
!
DGMI%XAVG_HV  (:)   = 0.
DGMI%XAVG_PSNG(:)   = 0.
DGMI%XAVG_PSNV(:)   = 0.
DGMI%XAVG_PSN (:)   = 0.
DGMI%XAVG_ALBT(:)   = 0.
DGMI%XAVG_SWI (:,:) = 0.
DGMI%XAVG_TSWI(:,:) = 0.
DGMI%XAVG_FSAT(:)   = 0.
DGMI%XAVG_FFG (:)   = 0.
DGMI%XAVG_FFV (:)   = 0.
DGMI%XAVG_FF  (:)   = 0.
DGMI%XAVG_TWSNOW(:) = 0.
DGMI%XAVG_TDSNOW(:) = 0.  
DGMI%XAVG_TTSNOW(:) = 0.
DGMI%XAVG_LAI   (:) = 0.
!   
DGMI%XSOIL_SWI  (:)  = 0.
DGMI%XSOIL_TSWI (:)  = 0.
DGMI%XSOIL_TWG  (:) = 0.
DGMI%XSOIL_TWGI (:) = 0.
DGMI%XSOIL_WG   (:) = 0.
DGMI%XSOIL_WGI  (:) = 0.
! 
IF(I%CISBA=='DIF')THEN
!        
  DGMI%XAVG_ALT   (:) = 0. 
  DGMI%XAVG_FLT   (:) = 0. 
!
ENDIF

IF(I%CISBA=='DIF'.AND.DGMI%LSURF_MISC_DIF)THEN
!   
  DGMI%XFRD2_TSWI (:) = 0.
  DGMI%XFRD2_TWG  (:) = 0.
  DGMI%XFRD2_TWGI (:) = 0.
!   
  DGMI%XFRD3_TSWI (:) = 0.
  DGMI%XFRD3_TWG  (:) = 0.
  DGMI%XFRD3_TWGI (:) = 0.
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
      DGMI%XAVG_HV(JJ) = DGMI%XAVG_HV(JJ) + I%XPATCH(JJ,JPATCH) * DGMI%XHV(JJ,JPATCH)
!
!     Snow fractions
      DGMI%XAVG_PSNG(JJ) = DGMI%XAVG_PSNG(JJ) + I%XPATCH(JJ,JPATCH) * DGMI%XDPSNG(JJ,JPATCH)
      DGMI%XAVG_PSNV(JJ) = DGMI%XAVG_PSNV(JJ) + I%XPATCH(JJ,JPATCH) * DGMI%XDPSNV(JJ,JPATCH)
      DGMI%XAVG_PSN (JJ) = DGMI%XAVG_PSN (JJ) + I%XPATCH(JJ,JPATCH) * DGMI%XDPSN (JJ,JPATCH)
!
!     Saturated fraction
      DGMI%XAVG_FSAT (JJ) = DGMI%XAVG_FSAT (JJ) + I%XPATCH(JJ,JPATCH) * DGMI%XDFSAT (JJ,JPATCH)
!
!     Flood fractions
      DGMI%XAVG_FFG(JJ) = DGMI%XAVG_FFG(JJ) + I%XPATCH(JJ,JPATCH) * DGMI%XDFFG(JJ,JPATCH)
      DGMI%XAVG_FFV(JJ) = DGMI%XAVG_FFV(JJ) + I%XPATCH(JJ,JPATCH) * DGMI%XDFFV(JJ,JPATCH)
      DGMI%XAVG_FF (JJ) = DGMI%XAVG_FF (JJ) + I%XPATCH(JJ,JPATCH) * DGMI%XDFF (JJ,JPATCH)
!
!     Total albedo
      DGMI%XAVG_ALBT(JJ) = DGMI%XAVG_ALBT(JJ) + I%XPATCH(JJ,JPATCH) * DGMI%XALBT (JJ,JPATCH)
!
!     Total LAI
      DGMI%XAVG_LAI (JJ) = DGMI%XAVG_LAI(JJ)  + I%XPATCH(JJ,JPATCH) * ZLAI (JJ,JPATCH)
!      
!     Snow total outputs
      DGMI%XAVG_TWSNOW(JJ) = DGMI%XAVG_TWSNOW(JJ) + I%XPATCH(JJ,JPATCH) * DGMI%XTWSNOW(JJ,JPATCH)
      DGMI%XAVG_TDSNOW(JJ) = DGMI%XAVG_TDSNOW(JJ) + I%XPATCH(JJ,JPATCH) * DGMI%XTDSNOW(JJ,JPATCH)
!      
      IF (DGMI%XTWSNOW(JJ,JPATCH)>0.0) THEN
         DGMI%XAVG_TTSNOW(JJ) = DGMI%XAVG_TTSNOW(JJ) + I%XPATCH(JJ,JPATCH) * DGMI%XTTSNOW(JJ,JPATCH)
         ZSNOW      (JJ) = ZSNOW      (JJ) + I%XPATCH(JJ,JPATCH)
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
IF(I%CISBA=='DIF')THEN ! DIF case
!---------------------------------------------
!
! Active and Frozen layers thickness
  ZTG(:,:)=0.0
  ZDG(:,:)=0.0
  DO JPATCH=1,INP
     DO JLAYER=1,I%NGROUND_LAYER
        DO JJ=1,INI 
           ZTG(JJ,JLAYER) = ZTG(JJ,JLAYER) + I%XPATCH(JJ,JPATCH) * I%XTG(JJ,JLAYER,JPATCH)
           ZDG(JJ,JLAYER) = ZDG(JJ,JLAYER) + I%XPATCH(JJ,JPATCH) * I%XDG(JJ,JLAYER,JPATCH)
        ENDDO
     ENDDO
  ENDDO
  CALL COMPUT_COLD_LAYERS_THICK(ZDG,ZTG,DGMI%XAVG_ALT,DGMI%XAVG_FLT)
!    
  ZPOND(:,:)=0.0
  DO JPATCH=1,INP      
     IF(I%NSIZE_NATURE_P(JPATCH) > 0 )THEN
       DO JLAYER = 1,I%NGROUND_LAYER
!         cdir nodep 
          DO JJ=1,INI
             IDEPTH=I%NWG_LAYER(JJ,JPATCH)
             IF(JLAYER<=IDEPTH.AND.IDEPTH/=NUNDEF)THEN
               ZWORK=I%XDZG(JJ,JLAYER,JPATCH)
               !Soil Wetness Index profile
               DGMI%XAVG_SWI (JJ,JLAYER) = DGMI%XAVG_SWI (JJ,JLAYER)+ZWORK*I%XPATCH(JJ,JPATCH)*DGMI%XSWI (JJ,JLAYER,JPATCH) 
               DGMI%XAVG_TSWI(JJ,JLAYER) = DGMI%XAVG_TSWI(JJ,JLAYER)+ZWORK*I%XPATCH(JJ,JPATCH)*DGMI%XTSWI(JJ,JLAYER,JPATCH)
               ZPOND    (JJ,JLAYER) = ZPOND    (JJ,JLAYER)+ZWORK*I%XPATCH(JJ,JPATCH)
               !Total soil wetness index, total water and ice contents
               DGMI%XSOIL_SWI (JJ) = DGMI%XSOIL_SWI (JJ) + ZWORK * I%XPATCH(JJ,JPATCH) * DGMI%XSWI (JJ,JLAYER,JPATCH)
               DGMI%XSOIL_TSWI(JJ) = DGMI%XSOIL_TSWI(JJ) + ZWORK * I%XPATCH(JJ,JPATCH) * DGMI%XTSWI(JJ,JLAYER,JPATCH)
               ZSUMDG    (JJ) = ZSUMDG    (JJ) + ZWORK * I%XPATCH(JJ,JPATCH)
               DGMI%XSOIL_TWG (JJ) = DGMI%XSOIL_TWG (JJ) + ZWORK * I%XPATCH(JJ,JPATCH) * (I%XWG(JJ,JLAYER,JPATCH) &
                                               + I%XWGI(JJ,JLAYER,JPATCH))
               DGMI%XSOIL_TWGI(JJ) = DGMI%XSOIL_TWGI(JJ) + ZWORK * I%XPATCH(JJ,JPATCH) * I%XWGI(JJ,JLAYER,JPATCH)
             ENDIF
          ENDDO
       ENDDO
     ENDIF
  ENDDO
!  
  WHERE(ZPOND(:,:)> 0.)
        DGMI%XAVG_SWI (:,:) = DGMI%XAVG_SWI (:,:) / ZPOND(:,:)
        DGMI%XAVG_TSWI(:,:) = DGMI%XAVG_TSWI(:,:) / ZPOND(:,:)
  ELSEWHERE
        DGMI%XAVG_SWI (:,:) = XUNDEF
        DGMI%XAVG_TSWI(:,:) = XUNDEF
  ENDWHERE
!
! ---------------------------------------------
  IF(DGMI%LSURF_MISC_DIF)THEN ! LSURF_MISC_DIF case
! ---------------------------------------------
!    
    DO JPATCH=1,INP
!  
      IF (I%NSIZE_NATURE_P(JPATCH) == 0 ) CYCLE
!     
      DO JLAYER = 1,I%NGROUND_LAYER
!       cdir nodep 
        DO JJ=1,INI
          IDEPTH=I%NWG_LAYER(JJ,JPATCH)
          IF(JLAYER<=IDEPTH.AND.IDEPTH/=NUNDEF)THEN
            !
            ! ISBA-FR-DG2 comparable soil wetness index, liquid water and ice contents
            ZWORK=MIN(I%XDZG(JJ,JLAYER,JPATCH),MAX(0.0,I%XDG2(JJ,JPATCH)-I%XDG(JJ,JLAYER,JPATCH)+I%XDZG(JJ,JLAYER,JPATCH)))
            DGMI%XFRD2_TSWI (JJ) = DGMI%XFRD2_TSWI (JJ) + ZWORK * I%XPATCH(JJ,JPATCH) * DGMI%XTSWI(JJ,JLAYER,JPATCH)
            DGMI%XFRD2_TWG  (JJ) = DGMI%XFRD2_TWG  (JJ) + ZWORK * I%XPATCH(JJ,JPATCH) * I%XWG  (JJ,JLAYER,JPATCH)
            DGMI%XFRD2_TWGI (JJ) = DGMI%XFRD2_TWGI (JJ) + ZWORK * I%XPATCH(JJ,JPATCH) * I%XWGI (JJ,JLAYER,JPATCH)
            ZSUMFRD2   (JJ) = ZSUMFRD2   (JJ) + ZWORK * I%XPATCH(JJ,JPATCH)
            !
            ! ISBA-FR-DG3 comparable soil wetness index, liquid water and ice contents
            ZWORK=MIN(I%XDZG(JJ,JLAYER,JPATCH),MAX(0.0,I%XDG(JJ,JLAYER,JPATCH)-I%XDG2(JJ,JPATCH)))
            DGMI%XFRD3_TSWI (JJ) = DGMI%XFRD3_TSWI (JJ) + ZWORK * I%XPATCH(JJ,JPATCH) * DGMI%XTSWI(JJ,JLAYER,JPATCH)
            DGMI%XFRD3_TWG  (JJ) = DGMI%XFRD3_TWG  (JJ) + ZWORK * I%XPATCH(JJ,JPATCH) * I%XWG  (JJ,JLAYER,JPATCH)
            DGMI%XFRD3_TWGI (JJ) = DGMI%XFRD3_TWGI (JJ) + ZWORK * I%XPATCH(JJ,JPATCH) * I%XWGI (JJ,JLAYER,JPATCH)
            ZSUMFRD3   (JJ) = ZSUMFRD3   (JJ) + ZWORK * I%XPATCH(JJ,JPATCH)
            !
          ENDIF
        ENDDO
      ENDDO
!
    ENDDO
!    
    WHERE(ZSUMFRD2(:)>0.0) 
          DGMI%XFRD2_TSWI (:) = DGMI%XFRD2_TSWI (:) / ZSUMFRD2(:)
          DGMI%XFRD2_TWG  (:) = DGMI%XFRD2_TWG  (:) / ZSUMFRD2(:)
          DGMI%XFRD2_TWGI (:) = DGMI%XFRD2_TWGI (:) / ZSUMFRD2(:)          
    ELSEWHERE
          DGMI%XFRD2_TSWI (:) = XUNDEF
    ENDWHERE 
!    
    WHERE(ZSUMFRD3(:)>0.0) 
          DGMI%XFRD3_TSWI (:) = DGMI%XFRD3_TSWI (:) / ZSUMFRD3(:)
          DGMI%XFRD3_TWG  (:) = DGMI%XFRD3_TWG  (:) / ZSUMFRD3(:)
          DGMI%XFRD3_TWGI (:) = DGMI%XFRD3_TWGI (:) / ZSUMFRD3(:) 
    ELSEWHERE
          DGMI%XFRD3_TSWI (:) = XUNDEF
    ENDWHERE
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
          DGMI%XAVG_SWI (JJ,1) = DGMI%XAVG_SWI (JJ,1) + I%XPATCH(JJ,JPATCH) * DGMI%XSWI (JJ,1,JPATCH)
          DGMI%XAVG_SWI (JJ,2) = DGMI%XAVG_SWI (JJ,2) + I%XPATCH(JJ,JPATCH) * DGMI%XSWI (JJ,2,JPATCH)
          DGMI%XAVG_TSWI(JJ,1) = DGMI%XAVG_TSWI(JJ,1) + I%XPATCH(JJ,JPATCH) * DGMI%XTSWI(JJ,1,JPATCH)
          DGMI%XAVG_TSWI(JJ,2) = DGMI%XAVG_TSWI(JJ,2) + I%XPATCH(JJ,JPATCH) * DGMI%XTSWI(JJ,2,JPATCH)
!
          DGMI%XSOIL_SWI (JJ) = DGMI%XSOIL_SWI (JJ) + I%XPATCH(JJ,JPATCH) * I%XDG (JJ,2,JPATCH) * DGMI%XSWI (JJ,2,JPATCH)
          DGMI%XSOIL_TSWI(JJ) = DGMI%XSOIL_TSWI(JJ) + I%XPATCH(JJ,JPATCH) * I%XDG (JJ,2,JPATCH) * DGMI%XTSWI(JJ,2,JPATCH)
          DGMI%XSOIL_TWG (JJ) = DGMI%XSOIL_TWG (JJ) + I%XPATCH(JJ,JPATCH) * I%XDG (JJ,2,JPATCH) * (I%XWG(JJ,2,JPATCH) &
                                          + I%XWGI(JJ,2,JPATCH))
          DGMI%XSOIL_TWGI(JJ) = DGMI%XSOIL_TWGI(JJ) + I%XPATCH(JJ,JPATCH) * I%XDG (JJ,2,JPATCH) * I%XWGI(JJ,2,JPATCH) 
! 
          ZSUMDG    (JJ) = ZSUMDG    (JJ) + I%XPATCH(JJ,JPATCH) * I%XDG(JJ,I%NGROUND_LAYER,JPATCH)        
!          
       ENDIF
     ENDDO
  ENDDO     
!     
  IF(I%CISBA=='3-L')THEN
!          
    ZPOND(:,:)=0.0
    DO JPATCH=1,INP
       DO JJ=1,SIZE(I%XPATCH,1)        
          IF(ZSUMPATCH(JJ) > 0.)THEN
!
            ZWORK=MAX(0.0,I%XDG(JJ,3,JPATCH)-I%XDG(JJ,2,JPATCH))
!
!           Remenber: no ice in the third layer of 3-L
            ZPOND     (JJ,3) = ZPOND     (JJ,3) + I%XPATCH(JJ,JPATCH) * ZWORK
            DGMI%XAVG_SWI  (JJ,3) = DGMI%XAVG_SWI  (JJ,3) + I%XPATCH(JJ,JPATCH) * ZWORK * DGMI%XSWI (JJ,3,JPATCH)
            DGMI%XSOIL_SWI (JJ  ) = DGMI%XSOIL_SWI (JJ  ) + I%XPATCH(JJ,JPATCH) * ZWORK * DGMI%XSWI (JJ,3,JPATCH)  
            DGMI%XAVG_TSWI (JJ,3) = DGMI%XAVG_TSWI (JJ,3) + I%XPATCH(JJ,JPATCH) * ZWORK * DGMI%XTSWI(JJ,3,JPATCH)
            DGMI%XSOIL_TSWI(JJ  ) = DGMI%XSOIL_TSWI(JJ  ) + I%XPATCH(JJ,JPATCH) * ZWORK * DGMI%XTSWI(JJ,3,JPATCH)  
            DGMI%XSOIL_TWG (JJ  ) = DGMI%XSOIL_TWG (JJ  ) + I%XPATCH(JJ,JPATCH) * ZWORK * I%XWG  (JJ,3,JPATCH)  
!
          ENDIF
       ENDDO
    ENDDO
!
    WHERE(ZPOND(:,3)>0.0)
          DGMI%XAVG_SWI (:,3) = DGMI%XAVG_SWI (:,3) / ZPOND(:,3)
          DGMI%XAVG_TSWI(:,3) = DGMI%XAVG_TSWI(:,3) / ZPOND(:,3)
    ELSEWHERE
          DGMI%XAVG_SWI (:,3) = XUNDEF
          DGMI%XAVG_TSWI(:,3) = XUNDEF
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
!Total Soil Wetness Index and Soil Water Content (m3.m-3)
WHERE(ZSUMDG(:)>0.0)
      DGMI%XSOIL_SWI (:) = DGMI%XSOIL_SWI (:)/ZSUMDG(:)
      DGMI%XSOIL_TSWI(:) = DGMI%XSOIL_TSWI(:)/ZSUMDG(:)
      DGMI%XSOIL_WG  (:) = DGMI%XSOIL_TWG (:)/ZSUMDG(:)
      DGMI%XSOIL_WGI (:) = DGMI%XSOIL_TWGI(:)/ZSUMDG(:)
ENDWHERE
!       
!Total Soil Water Content (Liquid+Solid) and Total Frozen Content (kg/m2)
DGMI%XSOIL_TWG (:)= DGMI%XSOIL_TWG (:) * XRHOLW
DGMI%XSOIL_TWGI(:)= DGMI%XSOIL_TWGI(:) * XRHOLW
!
! Snow temperature  
WHERE(ZSNOW(:)>0.0)
      DGMI%XAVG_TTSNOW(:) = DGMI%XAVG_TTSNOW(:)/ZSNOW(:)
ELSEWHERE
      DGMI%XAVG_TTSNOW(:) = XUNDEF
ENDWHERE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('AVERAGE_DIAG_MISC_ISBA_N',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE AVERAGE_DIAG_MISC_ISBA_n
