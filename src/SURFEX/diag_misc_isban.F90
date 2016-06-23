!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE DIAG_MISC_ISBA_n (DGMI, PKDI, &
                             PTSTEP, HISBA, HPHOTO, HSNOW, OAGRIP, OTR_ML, &
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
!
USE MODD_DIAG_MISC_ISBA_n, ONLY : DIAG_MISC_ISBA_t
USE MODD_PACK_DIAG_ISBA, ONLY : PACK_DIAG_ISBA_t
!
USE MODD_CSTS,       ONLY : XTT
USE MODD_SURF_PAR,   ONLY : XUNDEF
!
!                                     
USE MODD_TYPE_SNOW
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
TYPE(DIAG_MISC_ISBA_t), INTENT(INOUT) :: DGMI
TYPE(PACK_DIAG_ISBA_t), INTENT(INOUT) :: PKDI
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
REAL, DIMENSION(KSIZE) :: ZALT, ZFLT
!
LOGICAL :: GMASK
INTEGER :: JJ, JI, JK
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('DIAG_MISC_ISBA_N',0,ZHOOK_HANDLE)
!
IF (DGMI%LSURF_MISC_BUDGET) THEN
  !
  PKDI%XP_SWI (:,:)=XUNDEF
  PKDI%XP_TSWI(:,:)=XUNDEF  
  DO JJ=1,SIZE(PWG,2)
    DO JI=1,SIZE(PWG,1)
      IF(PWG (JI,JJ)/=XUNDEF)THEN    
        PKDI%XP_SWI (JI,JJ) = (PWG (JI,JJ) - PWWILT(JI,JJ)) / (PWFC(JI,JJ) - PWWILT(JI,JJ))
        PKDI%XP_TSWI(JI,JJ) = (PWG (JI,JJ) - PWWILT(JI,JJ)) / (PWFC(JI,JJ) - PWWILT(JI,JJ))
      ENDIF
      IF(PWGI (JI,JJ)/=XUNDEF)THEN    
        PKDI%XP_TSWI(JI,JJ) = PKDI%XP_TSWI(JI,JJ) +  PWGI(JI,JJ) / (PWFC(JI,JJ) - PWWILT(JI,JJ))
      ENDIF
    ENDDO
  ENDDO
  !
  DO JK=1,SIZE(PKDI%XP_SWI,2)
!cdir nodep
    DO JJ=1,KSIZE
      JI                      =  KMASK         (JJ)
      !
      DGMI%XSWI     (JI,JK,KPATCH)  =  PKDI%XP_SWI        (JJ,JK)
      DGMI%XTSWI    (JI,JK,KPATCH)  =  PKDI%XP_TSWI       (JJ,JK)
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
  PKDI%XP_TWSNOW=0.
  PKDI%XP_TDSNOW=0.
  ZSNOWTEMP=0.  
  !
  IF (HSNOW/='EBA')THEN
     ZWORKTEMP(:,:) = PKDI%XP_SNOWTEMP(:,:)
  ELSE
     ZWORKTEMP(:,1) = MIN(PTG(:,1),XTT)
  ENDIF
  !
  DO JI = 1,SIZE(PWSNOW,2)
!cdir nodep 
    DO JJ = 1,SIZE(PWSNOW,1)
      PKDI%XP_TWSNOW(JJ) = PKDI%XP_TWSNOW(JJ) + PWSNOW(JJ,JI)      
      PKDI%XP_TDSNOW(JJ) = PKDI%XP_TDSNOW(JJ) + ZWORK (JJ,JI)
      ZSNOWTEMP(JJ) = ZSNOWTEMP(JJ) + ZWORKTEMP(JJ,JI) * ZWORK(JJ,JI)
    ENDDO
  ENDDO
  !
  WHERE(PKDI%XP_TDSNOW(:)>0.0)
        ZSNOWTEMP(:)=ZSNOWTEMP(:)/PKDI%XP_TDSNOW(:)
  ELSEWHERE
        ZSNOWTEMP(:)=XUNDEF
  ENDWHERE
  !
!cdir nodep
  DO JJ=1,KSIZE
     JI                     =  KMASK       (JJ)
     !
     DGMI%XHV      (JI, KPATCH)  =  PKDI%XP_HV       (JJ)
     DGMI%XDPSNG   (JI, KPATCH)  =  PPSNG       (JJ)
     DGMI%XDPSNV   (JI, KPATCH)  =  PPSNV       (JJ)
     DGMI%XDPSN    (JI, KPATCH)  =  PPSN        (JJ)     
     DGMI%XALBT    (JI, KPATCH)  =  PKDI%XP_ALBT     (JJ)
     DGMI%XDFF     (JI, KPATCH)  =  PFF         (JJ)
     DGMI%XDFFG    (JI, KPATCH)  =  PFFG        (JJ)
     DGMI%XDFFV    (JI, KPATCH)  =  PFFV        (JJ)     
     DGMI%XTWSNOW  (JI, KPATCH)  =  PKDI%XP_TWSNOW   (JJ)
     DGMI%XTDSNOW  (JI, KPATCH)  =  PKDI%XP_TDSNOW   (JJ)
     DGMI%XTTSNOW  (JI, KPATCH)  =  ZSNOWTEMP   (JJ)
     DGMI%XDFSAT   (JI, KPATCH)  =  PFSAT       (JJ)     
     !
  END DO
!
  IF (HSNOW=='3-L' .OR. HSNOW=='CRO') THEN
     !
    DO JK=1,SIZE(PKDI%XP_SNOWLIQ,2)
!cdir nodep
      DO JJ=1,KSIZE
        JI                      =  KMASK         (JJ)
        !
        DGMI%XSNOWLIQ (JI,JK,KPATCH)  =  PKDI%XP_SNOWLIQ    (JJ,JK)
        DGMI%XSNOWTEMP(JI,JK,KPATCH)  =  PKDI%XP_SNOWTEMP   (JJ,JK)
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
         DGMI%XFAPAR      (JI, KPATCH) = PKDI%XP_FAPAR      (JJ)
         DGMI%XFAPIR      (JI, KPATCH) = PKDI%XP_FAPIR      (JJ)
         DGMI%XFAPAR_BS   (JI, KPATCH) = PKDI%XP_FAPAR_BS   (JJ)
         DGMI%XFAPIR_BS   (JI, KPATCH) = PKDI%XP_FAPIR_BS   (JJ)
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
             DGMI%XDFAPARC   (JI, KPATCH) = PFAPARC   (JJ) / PMUS(JJ) 
             DGMI%XDFAPIRC   (JI, KPATCH) = PFAPIRC   (JJ) / PMUS(JJ)
             DGMI%XDLAI_EFFC (JI, KPATCH) = PLAI_EFFC (JJ) / PMUS(JJ)
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
    ZALT(:)=0.0
    ZFLT(:)=0.0
    CALL COMPUT_COLD_LAYERS_THICK(PDG,PTG,ZALT,ZFLT)
    DO JJ=1,KSIZE
       JI              =  KMASK(JJ)
       DGMI%XALT(JI,KPATCH) =  ZALT(JJ) 
       DGMI%XFLT(JI,KPATCH) =  ZFLT(JJ)  
    ENDDO
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
     DGMI%XSEUIL   (JI, KPATCH)  =  PSEUIL (JJ)
     !
  END DO
!
END IF
IF (LHOOK) CALL DR_HOOK('DIAG_MISC_ISBA_N',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------------
!
END SUBROUTINE DIAG_MISC_ISBA_n
