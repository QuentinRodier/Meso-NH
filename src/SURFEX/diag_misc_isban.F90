!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
SUBROUTINE DIAG_MISC_ISBA_n(PTSTEP, HISBA, HPHOTO, HSNOW, OAGRIP, OTR_ML, &
                            PTIME, KSIZE, KPATCH, KMASK, PSEUIL,          &
                            PPSN, PPSNG, PPSNV, PFF, PFFG, PFFV,          &
                            PWG, PWGI, PWFC, PWWILT, PWSNOW, PRSNOW,      &
                            PFAPARC, PFAPIRC, PLAI_EFFC, PMUS, PFSAT,     &
                            PDG, PTG                                      )  
!     ###############################################################################
!
!!****  *DIAG_MISC-ISBA_n * - additional diagnostics for ISBA
!!
!!    PURPOSE
!!    -------
!
!!**  METHOD
!!    ------
!!
!!    REFERENCE
!!    ---------
!!      
!!
!!    AUTHOR
!!    ------
!!     P. Le Moigne 
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    10/2004
!!      Modified    10/2004 by P. Le Moigne: Halstead coefficient
!!      B. Decharme   2008    Do not limit the SWI to 1
!!                            Add total SWI
!!      S. Lafont    03/2009 : change unit of carbon output in kg/m2/s
!!      A.L. Gibelin 04/2009 : Add respiration diagnostics
!!      A.L. Gibelin 07/2009 : Suppress RDK and transform GPP as a diagnostic
!!        S. Lafont  01/2011 : accumulate carbon variable between 2 outputs
!!       B. Decharme 05/2012 : Carbon fluxes in diag_evap
!!       B. Decharme 05/2012 : Active and frozen layers thickness for dif
!!       B. Decharme 06/2013 : Snow temp for EBA scheme (XP_SNOWTEMP not allocated)
!!
!!------------------------------------------------------------------
!
USE MODD_CSTS,       ONLY : XTT
USE MODD_SURF_PAR,   ONLY : XUNDEF
!
USE MODD_PACK_DIAG_ISBA,      ONLY : XP_HV, XP_SWI, XP_ALBT, XP_TSWI,     &
                                     XP_TWSNOW, XP_TDSNOW, XP_SNOWTEMP,   &
                                     XP_SNOWLIQ,                          &
                                     XP_FAPAR, XP_FAPIR, XP_FAPAR_BS,     &
                                     XP_FAPIR_BS 
!                                     
USE MODD_DIAG_MISC_ISBA_n,    ONLY : LSURF_MISC_BUDGET,                       &
                                     XHV, XSWI, XDPSNG, XDPSNV, XDPSN, XSEUIL,&
                                     XALBT, XTSWI, XDFF, XDFFG,               &
                                     XDFFV, XTWSNOW, XTDSNOW, XTTSNOW,        &
                                     XSNOWLIQ, XSNOWTEMP, XDLAI_EFFC,         &
                                     XFAPAR, XFAPIR, XDFAPARC, XDFAPIRC,      &
                                     XFAPAR_BS, XFAPIR_BS, XDFSAT, XALT,      &
                                     XFLT
USE MODD_TYPE_SNOW
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL,               INTENT(IN)    :: PTSTEP        ! timestep for  accumulated values 
 CHARACTER(LEN=*), INTENT(IN)      :: HISBA         ! ISBA scheme
 CHARACTER(LEN=*), INTENT(IN)      :: HPHOTO        ! type of photosynthesis
 CHARACTER(LEN=*), INTENT(IN)      :: HSNOW         ! snow scheme
LOGICAL, INTENT(IN)               :: OAGRIP
LOGICAL, INTENT(IN)               :: OTR_ML
REAL,    INTENT(IN)               :: PTIME   ! current time since midnight
INTEGER, INTENT(IN)               :: KSIZE, KPATCH
INTEGER, DIMENSION(:), INTENT(IN) :: KMASK
REAL, DIMENSION(:), INTENT(IN)    :: PSEUIL
!    
!Snow/flood fraction at t
REAL, DIMENSION(:), INTENT(IN)    :: PPSN
REAL, DIMENSION(:), INTENT(IN)    :: PPSNG
REAL, DIMENSION(:), INTENT(IN)    :: PPSNV
REAL, DIMENSION(:), INTENT(IN)    :: PFF
REAL, DIMENSION(:), INTENT(IN)    :: PFFG
REAL, DIMENSION(:), INTENT(IN)    :: PFFV
!
REAL, DIMENSION(:,:),  INTENT(IN) :: PWG           ! soil water content profile (m3/m3)
REAL, DIMENSION(:,:),  INTENT(IN) :: PWGI          ! soil solid water content profile (m3/m3)
REAL, DIMENSION(:,:),  INTENT(IN) :: PWFC          ! field capacity profile (m3/m3)
REAL, DIMENSION(:,:),  INTENT(IN) :: PWWILT        ! wilting point  profile (m3/m3)
REAL, DIMENSION(:,:),  INTENT(IN) :: PWSNOW        ! snow reservoir (kg/m2)
REAL, DIMENSION(:,:),  INTENT(IN) :: PRSNOW        ! snow density (kg/m3)
!
REAL, DIMENSION(:,:),  INTENT(IN) :: PDG           ! soil layer depth
REAL, DIMENSION(:,:),  INTENT(IN) :: PTG           ! soil temperature
!
REAL, DIMENSION(:), INTENT(INOUT) :: PFAPARC
REAL, DIMENSION(:), INTENT(INOUT) :: PFAPIRC
REAL, DIMENSION(:), INTENT(INOUT) :: PLAI_EFFC
REAL, DIMENSION(:), INTENT(INOUT) :: PMUS
!
REAL, DIMENSION(:), INTENT(IN)    :: PFSAT
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(SIZE(PPSN))    :: ZSNOWTEMP
REAL, DIMENSION(SIZE(PWSNOW,1),SIZE(PWSNOW,2)) :: ZWORK
REAL, DIMENSION(SIZE(PWSNOW,1),SIZE(PWSNOW,2)) :: ZWORKTEMP
!
LOGICAL :: GMASK
INTEGER :: JJ, JI, JK
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('DIAG_MISC_ISBA_N',0,ZHOOK_HANDLE)
IF (LSURF_MISC_BUDGET) THEN
  !
  XP_SWI (:,:)=XUNDEF
  XP_TSWI(:,:)=XUNDEF  
  DO JJ=1,SIZE(PWG,2)
    DO JI=1,SIZE(PWG,1)
      IF(PWG (JI,JJ)/=XUNDEF)THEN    
        XP_SWI (JI,JJ) = (PWG (JI,JJ)               - PWWILT(JI,JJ)) / (PWFC(JI,JJ) - PWWILT(JI,JJ))
        XP_TSWI(JI,JJ) = (PWG (JI,JJ) + PWGI(JI,JJ) - PWWILT(JI,JJ)) / (PWFC(JI,JJ) - PWWILT(JI,JJ))
      ENDIF
    ENDDO
  ENDDO
  !
  DO JK=1,SIZE(XP_SWI,2)
!cdir nodep
    DO JJ=1,KSIZE
      JI                      =  KMASK         (JJ)
      !
      XSWI     (JI,JK,KPATCH)  =  XP_SWI        (JJ,JK)
      XTSWI    (JI,JK,KPATCH)  =  XP_TSWI       (JJ,JK)
      !
    END DO
  ENDDO  
  !
  DO JI = 1,SIZE(PWSNOW,2)
!cdir nodep 
    DO JJ = 1,SIZE(PWSNOW,1)
      ZWORK(JJ,JI)  = PWSNOW(JJ,JI) / PRSNOW(JJ,JI)
    ENDDO
  ENDDO
  !
  XP_TWSNOW=0.
  XP_TDSNOW=0.
  ZSNOWTEMP=0.  
  !
  IF (HSNOW/='EBA')THEN
     ZWORKTEMP(:,:) = XP_SNOWTEMP(:,:)
  ELSE
     ZWORKTEMP(:,1) = MIN(PTG(:,1),XTT)
  ENDIF
  !
  DO JI = 1,SIZE(PWSNOW,2)
!cdir nodep 
    DO JJ = 1,SIZE(PWSNOW,1)
      XP_TWSNOW(JJ) = XP_TWSNOW(JJ) + PWSNOW(JJ,JI)      
      XP_TDSNOW(JJ) = XP_TDSNOW(JJ) + ZWORK (JJ,JI)
      ZSNOWTEMP(JJ) = ZSNOWTEMP(JJ) + ZWORKTEMP(JJ,JI) * ZWORK(JJ,JI)
    ENDDO
  ENDDO
  !
  WHERE(XP_TDSNOW(:)>0.0)
        ZSNOWTEMP(:)=ZSNOWTEMP(:)/XP_TDSNOW(:)
  ELSEWHERE
        ZSNOWTEMP(:)=XUNDEF
  ENDWHERE
  !
!cdir nodep
  DO JJ=1,KSIZE
     JI                     =  KMASK       (JJ)
     !
     XHV      (JI, KPATCH)  =  XP_HV       (JJ)
     XDPSNG   (JI, KPATCH)  =  PPSNG       (JJ)
     XDPSNV   (JI, KPATCH)  =  PPSNV       (JJ)
     XDPSN    (JI, KPATCH)  =  PPSN        (JJ)     
     XALBT    (JI, KPATCH)  =  XP_ALBT     (JJ)
     XDFF     (JI, KPATCH)  =  PFF         (JJ)
     XDFFG    (JI, KPATCH)  =  PFFG        (JJ)
     XDFFV    (JI, KPATCH)  =  PFFV        (JJ)     
     XTWSNOW  (JI, KPATCH)  =  XP_TWSNOW   (JJ)
     XTDSNOW  (JI, KPATCH)  =  XP_TDSNOW   (JJ)
     XTTSNOW  (JI, KPATCH)  =  ZSNOWTEMP   (JJ)
     XDFSAT   (JI, KPATCH)  =  PFSAT       (JJ)     
     !
  END DO
!
  IF (HSNOW=='3-L' .OR. HSNOW=='CRO') THEN
     !
    DO JK=1,SIZE(XP_SNOWLIQ,2)
!cdir nodep
      DO JJ=1,KSIZE
        JI                      =  KMASK         (JJ)
        !
        XSNOWLIQ (JI,JK,KPATCH)  =  XP_SNOWLIQ    (JJ,JK)
        XSNOWTEMP(JI,JK,KPATCH)  =  XP_SNOWTEMP   (JJ,JK)
        !
      END DO
    ENDDO
     !
  ENDIF
!
! cosine of solar zenith angle 
!

  IF (HPHOTO/='NON'.AND.OTR_ML) THEN
       !
!cdir nodep
       DO JJ=1,KSIZE
         JI = KMASK(JJ)
         !
         XFAPAR      (JI, KPATCH) = XP_FAPAR      (JJ)
         XFAPIR      (JI, KPATCH) = XP_FAPIR      (JJ)
         XFAPAR_BS   (JI, KPATCH) = XP_FAPAR_BS   (JJ)
         XFAPIR_BS   (JI, KPATCH) = XP_FAPIR_BS   (JJ)
         !
       ENDDO
       !
       ! Mask where vegetation evolution is performed (just before solar midnight)
       GMASK = ( PTIME - PTSTEP < 0. ) .AND. ( PTIME >= 0. )
       IF (GMASK) THEN
!cdir nodep
         DO JJ=1,KSIZE
           JI = KMASK(JJ)
           !
           IF (PMUS(JJ).NE.0.) THEN
             XDFAPARC   (JI, KPATCH) = PFAPARC   (JJ) / PMUS(JJ) 
             XDFAPIRC   (JI, KPATCH) = PFAPIRC   (JJ) / PMUS(JJ)
             XDLAI_EFFC (JI, KPATCH) = PLAI_EFFC (JJ) / PMUS(JJ)
           ENDIF
           !
         ENDDO
!cdir nodep         
         DO JJ=1,KSIZE   
           PFAPARC(JJ)   = 0.
           PFAPIRC(JJ)   = 0.
           PLAI_EFFC(JJ) = 0.
           PMUS(JJ)      = 0.
         ENDDO
       ENDIF
       !
  ENDIF
  !
  IF(HISBA=='DIF')THEN
    CALL COMPUT_COLD_LAYERS_THICK
  ENDIF
  !
END IF
!
IF (OAGRIP) THEN
  !
!cdir nodep
  DO JJ=1,KSIZE
     JI                     =  KMASK         (JJ)
     !
     XSEUIL   (JI, KPATCH)  =  PSEUIL (JJ)
     !
  END DO
!
END IF
IF (LHOOK) CALL DR_HOOK('DIAG_MISC_ISBA_N',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------------
!
CONTAINS
!
SUBROUTINE COMPUT_COLD_LAYERS_THICK
!
! Comput active layer (ALT) and frozen layer (FLT) theaknesses 
! using linear interpolation between two nodes :
!       ALT = depth to zero centigrade isotherm in permafrost
!       FLT = depth to zero centigrade isotherm in non-permafrost
!
USE MODD_SURF_PAR, ONLY : NUNDEF
!
IMPLICIT NONE
!
REAL, DIMENSION(KSIZE,SIZE(PDG,2)) :: ZNODE
INTEGER, DIMENSION(KSIZE)          :: IUP_ALT, IDOWN_ALT
INTEGER, DIMENSION(KSIZE)          :: IUP_FLT, IDOWN_FLT
!
REAL    :: ZTG_UP, ZTG_DOWN
REAL    :: ZUP, ZDOWN
REAL    :: ZALT, ZFLT, ZSLOPE
!
INTEGER :: JJ, JI, JL, INL
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('DIAG_MISC_ISBA_N:COMPUT_COLD_LAYERS_THICK',0,ZHOOK_HANDLE)
!
INL=SIZE(PDG,2)
!
IUP_ALT  (:)=0
IDOWN_ALT(:)=0
IUP_FLT  (:)=0
IDOWN_FLT(:)=0
!
!Surface soil layer
!
ZNODE(:,1)=0.5*PDG(:,1)
WHERE(PTG(:,1)>XTT.AND.PTG(:,2)<=XTT.AND.PTG(:,3)<=XTT)
      IUP_ALT  (:)=1
      IDOWN_ALT(:)=2
ENDWHERE
WHERE(PTG(:,1)<XTT.AND.PTG(:,2)>=XTT.AND.PTG(:,3)>=XTT)
      IUP_FLT  (:)=1
      IDOWN_FLT(:)=2
ENDWHERE
!
!Middle soil layer
!
DO JL=2,INL-1
   DO JJ=1,KSIZE 
      ZNODE(JJ,JL)=0.5*(PDG(JJ,JL)+PDG(JJ,JL-1))
      IF(PTG(JJ,JL-1)>XTT.AND.PTG(JJ,JL)>XTT.AND.PTG(JJ,JL+1)<=XTT)THEN
        IUP_ALT  (JJ)=JL
        IDOWN_ALT(JJ)=JL+1
      ENDIF
      IF(PTG(JJ,JL-1)<XTT.AND.PTG(JJ,JL)<XTT.AND.PTG(JJ,JL+1)>=XTT)THEN
        IUP_FLT  (JJ)=JL
        IDOWN_FLT(JJ)=JL+1
      ENDIF      
   ENDDO
ENDDO
!
!Last soil layer
!
ZNODE(:,INL)=0.5*(PDG(:,INL)+PDG(:,INL-1))
WHERE(PTG(:,INL)>XTT)IDOWN_ALT(:)=NUNDEF
WHERE(PTG(:,INL)<XTT)IDOWN_FLT(:)=NUNDEF
!
DO JJ=1,KSIZE
!
   ZALT  =0.0
   IF(IDOWN_ALT(JJ)>0.AND.IDOWN_ALT(JJ)<=INL)THEN
     ZTG_UP    = PTG  (JJ,IUP_ALT  (JJ))
     ZTG_DOWN  = PTG  (JJ,IDOWN_ALT(JJ))
     ZUP       = ZNODE(JJ,IUP_ALT  (JJ))
     ZDOWN     = ZNODE(JJ,IDOWN_ALT(JJ))
     ZSLOPE    = (ZUP-ZDOWN)/(ZTG_UP-ZTG_DOWN)
     ZALT      = ZDOWN+(XTT-ZTG_DOWN)*ZSLOPE
   ENDIF
!
   ZFLT  =0.0
   IF(IDOWN_FLT(JJ)>0.AND.IDOWN_FLT(JJ)<=INL)THEN
     ZTG_UP    = PTG  (JJ,IUP_FLT  (JJ))
     ZTG_DOWN  = PTG  (JJ,IDOWN_FLT(JJ))
     ZUP       = ZNODE(JJ,IUP_FLT  (JJ))
     ZDOWN     = ZNODE(JJ,IDOWN_FLT(JJ))
     ZSLOPE    = (ZUP-ZDOWN)/(ZTG_UP-ZTG_DOWN)
     ZFLT      = ZDOWN+(XTT-ZTG_DOWN)*ZSLOPE
   ENDIF
!
   JI              =  KMASK(JJ)
   XALT(JI,KPATCH) =  ZALT 
   XFLT(JI,KPATCH) =  ZFLT 
!
ENDDO
!
IF (LHOOK) CALL DR_HOOK('DIAG_MISC_ISBA_N:COMPUT_COLD_LAYERS_THICK',1,ZHOOK_HANDLE)
!
END SUBROUTINE COMPUT_COLD_LAYERS_THICK
!-------------------------------------------------------------------------------------
!
END SUBROUTINE DIAG_MISC_ISBA_n
