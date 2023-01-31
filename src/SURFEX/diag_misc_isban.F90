!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE DIAG_MISC_ISBA_n (DMK, KK, PK, PEK, AGK, IO, OVOLUMETRIC_SNOWLIQ, &
                             PTSTEP, PTIME)  
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
!!       M. Lafaysse 09/2015 : new Crocus-MEPRA outputs
!!       A. Druel    02/2019 : Remove XSEUIL and associated LARGIP flag (for irrigation)
!!       B. Decharme 02/2019 : Compute all wg and swi diag properly
!!
!!------------------------------------------------------------------
!
!
USE MODD_DIAG_MISC_ISBA_n, ONLY : DIAG_MISC_ISBA_t
USE MODD_ISBA_n,           ONLY : ISBA_K_t, ISBA_P_t, ISBA_PE_t
USE MODD_AGRI_n,           ONLY : AGRI_t
USE MODD_ISBA_OPTIONS_n,   ONLY : ISBA_OPTIONS_t
!
USE MODD_CSTS,             ONLY : XTT, XRHOLW
USE MODD_SURF_PAR,         ONLY : XUNDEF
!
!                                     
USE MODD_TYPE_SNOW
USE MODD_PREP_SNOW,        ONLY : NIMPUR
!
USE MODI_COMPUT_COLD_LAYERS_THICK
!
USE YOMHOOK   ,            ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,            ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
TYPE(DIAG_MISC_ISBA_t), INTENT(INOUT) :: DMK
TYPE(ISBA_K_t),         INTENT(INOUT) :: KK
TYPE(ISBA_P_t),         INTENT(INOUT) :: PK
TYPE(ISBA_PE_t),        INTENT(INOUT) :: PEK
TYPE(AGRI_t),           INTENT(INOUT) :: AGK
TYPE(ISBA_OPTIONS_t),   INTENT(INOUT) :: IO
!
LOGICAL,                INTENT(IN)    :: OVOLUMETRIC_SNOWLIQ
REAL,                   INTENT(IN)    :: PTSTEP  ! timestep for  accumulated values 
REAL,                   INTENT(IN)    :: PTIME   ! current time since midnight
!
!
!*      0.2    declarations of local parameters
!
REAL, PARAMETER                       :: ZDGSFC = 0.10 !(m)
!
!*      0.3    declarations of local variables
!
REAL, DIMENSION(SIZE(PEK%XPSN)) :: ZSNOWTEMP
REAL, DIMENSION(SIZE(PEK%XPSN)) :: ZTWSNOW
REAL, DIMENSION(SIZE(PEK%XPSN)) :: ZTDSNOW
REAL, DIMENSION(SIZE(PEK%XPSN)) :: ZTSNOWLIQ
REAL, DIMENSION(SIZE(PEK%XPSN)) :: ZTSNOWAGE

REAL, DIMENSION(SIZE(PEK%XPSN)) :: ZALT
REAL, DIMENSION(SIZE(PEK%XPSN)) :: ZFLT
REAL, DIMENSION(SIZE(PEK%XPSN)) :: ZPLT
REAL, DIMENSION(SIZE(PEK%XPSN)) :: ZWORKSWI
REAL, DIMENSION(SIZE(PEK%XPSN)) :: ZSUMDG
!
REAL, DIMENSION(SIZE(PEK%XWG,1),SIZE(PEK%XWG,2)) :: ZWGI
!
REAL, DIMENSION(SIZE(PEK%TSNOW%WSNOW,1),SIZE(PEK%TSNOW%WSNOW,2)) :: ZWORKDSN
REAL, DIMENSION(SIZE(PEK%TSNOW%WSNOW,1),SIZE(PEK%TSNOW%WSNOW,2)) :: ZWORKSNTEMP
!
REAL    :: ZWORK
LOGICAL :: GMASK
INTEGER :: INI, INL, INS
INTEGER :: JL, JI
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('DIAG_MISC_ISBA_N',0,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------------
!
!*       1.     Initialization:
!               ---------------
!
INI=SIZE(PEK%XWG,1)
INL=SIZE(PEK%XWG,2)
INS=SIZE(PEK%TSNOW%WSNOW,2)
!
DMK%XFF  (:) = KK%XFF  (:)
DMK%XFFG (:) = KK%XFFG (:)
DMK%XFFV (:) = KK%XFFV (:)
DMK%XFSAT(:) = KK%XFSAT(:)
!
WHERE(PEK%XLAI(:)==XUNDEF)
     DMK%XF2(:)=XUNDEF
ENDWHERE
!
IF (IO%CISBA=='DIF') THEN
   DO JL=1,INL
      DO JI=1,INI
         IF(PEK%XLAI(JI)==XUNDEF)THEN
           DMK%XF2WGHT(JI,JL)=XUNDEF
         ENDIF
      ENDDO
   ENDDO
ENDIF
!
!-------------------------------------------------------------------------------------
!
!*       2.     Soil Wetness Index per layer:
!               -----------------------------
!
ZWGI(:,:) = PEK%XWGI(:,:)
IF(IO%CISBA=='3-L')THEN
  ZWGI(:,3)=0.0
ENDIF
!
DMK%XSWI (:,:)=XUNDEF
DMK%XTSWI(:,:)=XUNDEF  
DO JL=1,INL
  DO JI=1,INI
    IF(PEK%XWG (JI,JL)/=XUNDEF)THEN    
       DMK%XSWI (JI,JL) = (PEK%XWG(JI,JL) - KK%XWWILT(JI,JL)) / (KK%XWFC(JI,JL) - KK%XWWILT(JI,JL))
       DMK%XTSWI(JI,JL) = (PEK%XWG(JI,JL) - KK%XWWILT(JI,JL)) / (KK%XWFC(JI,JL) - KK%XWWILT(JI,JL))
    ENDIF
    IF(PEK%XWGI (JI,JL)/=XUNDEF)THEN    
       DMK%XTSWI(JI,JL) = DMK%XTSWI(JI,JL) + ZWGI(JI,JL) / (KK%XWFC(JI,JL) - KK%XWWILT(JI,JL))
    ENDIF
  ENDDO
ENDDO
!
!-------------------------------------------------------------------------------------
!
!*       3.     Total soil moisture (kg/m2) and Total SWI :
!               -------------------------------------------
!
DMK%XSOIL_SWI (:) = 0.0
DMK%XSOIL_TSWI(:) = 0.0
!
DMK%XSOIL_TWG (:) = 0.0
DMK%XSOIL_TWGI(:) = 0.0
DMK%XSOIL_WG  (:) = 0.0
DMK%XSOIL_WGI (:) = 0.0
!
DMK%XROOT_TWG (:) = 0.0
DMK%XSURF_TWG (:) = 0.0
!
ZWORKSWI(:) = 0.0  
ZSUMDG  (:) = 0.0
!
IF (IO%CISBA=='DIF') THEN
   !
   ! Soil can be vertically heterogeneous
   !
   DO JL=1,INL
      DO JI=1,INI
         IF(JL<=PK%NWG_LAYER(JI))THEN
           !Total soil depth
           ZSUMDG(JI) = ZSUMDG(JI) + PK%XDZG(JI,JL)               
           !surface mositure 10cm (kg/m2)
           ZWORK             = MIN(PK%XDZG(JI,JL),MAX(0.0,ZDGSFC-PK%XDG(JI,JL)+PK%XDZG(JI,JL)))*XRHOLW
           DMK%XSURF_TWG(JI) = DMK%XSURF_TWG(JI)+ZWORK*(PEK%XWG(JI,JL)+PEK%XWGI(JI,JL))
           !root mositure (kg/m2)
           ZWORK             = MIN(PK%XDZG(JI,JL),MAX(0.0,PK%XDG2(JI)-PK%XDG(JI,JL)+PK%XDZG(JI,JL)))*XRHOLW
           DMK%XROOT_TWG(JI) = DMK%XROOT_TWG(JI)+ZWORK*(PEK%XWG(JI,JL)+PEK%XWGI(JI,JL))
           !total soil mositure (kg/m2)
           DMK%XSOIL_TWG (JI)=DMK%XSOIL_TWG (JI)+PK%XDZG(JI,JL)*(PEK%XWG(JI,JL)+PEK%XWGI(JI,JL))*XRHOLW
           DMK%XSOIL_TWGI(JI)=DMK%XSOIL_TWGI(JI)+PK%XDZG(JI,JL)*PEK%XWGI(JI,JL)*XRHOLW
           !total SWI
           DMK%XSOIL_SWI (JI)=DMK%XSOIL_SWI (JI)+PK%XDZG(JI,JL)*(PEK%XWG(JI,JL)                -KK%XWWILT(JI,JL))
           DMK%XSOIL_TSWI(JI)=DMK%XSOIL_TSWI(JI)+PK%XDZG(JI,JL)*(PEK%XWG(JI,JL)+PEK%XWGI(JI,JL)-KK%XWWILT(JI,JL))
           ZWORKSWI      (JI)=ZWORKSWI      (JI)+PK%XDZG(JI,JL)*(KK%XWFC(JI,JL)                -KK%XWWILT(JI,JL))          
         ENDIF
      ENDDO
   ENDDO
   !
   WHERE(ZWORKSWI(:)>0.0)
         DMK%XSOIL_SWI (:)=DMK%XSOIL_SWI (:)/ZWORKSWI(:)
         DMK%XSOIL_TSWI(:)=DMK%XSOIL_TSWI(:)/ZWORKSWI(:)
   ELSEWHERE
         DMK%XSOIL_SWI (:)=XUNDEF
         DMK%XSOIL_TSWI(:)=XUNDEF
   ENDWHERE
   !
   !Soil Water Content (m3.m-3)
   !
   WHERE(ZSUMDG(:)>0)
         DMK%XSOIL_WG (:)=DMK%XSOIL_TWG (:)/(ZSUMDG(:)*XRHOLW)
         DMK%XSOIL_WGI(:)=DMK%XSOIL_TWGI(:)/(ZSUMDG(:)*XRHOLW)
   ELSEWHERE
         DMK%XSOIL_WG (:)=XUNDEF
         DMK%XSOIL_WGI(:)=XUNDEF
   ENDWHERE
   !
ELSE 
   !
   ! Force-restore
   !
   DO JI=1,INI
       !Total soil depth
       ZSUMDG(JI) = PK%XDG(JI,2)
       !surface mositure 1cm (kg/m2)
       DMK%XSURF_TWG(JI)=(PEK%XWG(JI,1)+PEK%XWGI(JI,1))*PK%XDG(JI,1)*XRHOLW
       !root mositure (kg/m2)
       DMK%XROOT_TWG(JI)=(PEK%XWG(JI,2)+PEK%XWGI(JI,2))*PK%XDG(JI,2)*XRHOLW
       !total soil mositure (kg/m2)
       DMK%XSOIL_TWG (JI)=(PEK%XWG(JI,2)+PEK%XWGI(JI,2))*PK%XDG(JI,2)*XRHOLW
       DMK%XSOIL_TWGI(JI)=               PEK%XWGI(JI,2) *PK%XDG(JI,2)*XRHOLW
       !total SWI
       DMK%XSOIL_SWI (JI)=DMK%XSWI (JI,2)
       DMK%XSOIL_TSWI(JI)=DMK%XTSWI(JI,2)
   ENDDO
   !
   IF(IO%CISBA=='3-L')THEN
      DO JI=1,INI
         !Total soil depth
         ZSUMDG(JI) = PK%XDG(JI,3)
         !total soil mositure (kg/m2)
         DMK%XSOIL_TWG(JI)=DMK%XSOIL_TWG(JI)+PEK%XWG(JI,3)*(PK%XDG(JI,3)-PK%XDG(JI,2))*XRHOLW
         !total SWI (With isba-fr, wwilt and wwfc are homogeneous so the average is direct)
         DMK%XSOIL_SWI (JI)=(DMK%XSWI (JI,2)*PK%XDG(JI,2)+DMK%XSWI (JI,3)*(PK%XDG(JI,3)-PK%XDG(JI,2)))/PK%XDG(JI,3)
         DMK%XSOIL_TSWI(JI)=(DMK%XTSWI(JI,2)*PK%XDG(JI,2)+DMK%XTSWI(JI,3)*(PK%XDG(JI,3)-PK%XDG(JI,2)))/PK%XDG(JI,3)
      ENDDO
   ENDIF
   !
   !Soil Water Content (m3.m-3)
   !
   WHERE(ZSUMDG(:)>0)
         DMK%XSOIL_WG (:)=DMK%XSOIL_TWG(:)/(ZSUMDG(:)*XRHOLW)
   ELSEWHERE
         DMK%XSOIL_WG (:)=XUNDEF
   ENDWHERE
   !
   WHERE(PK%XDG(:,2)>0)
         DMK%XSOIL_WGI(:)=DMK%XSOIL_TWGI(:)/(PK%XDG(:,2)*XRHOLW) ! Ice content only in the root zone
   ELSEWHERE
         DMK%XSOIL_WGI(:)=XUNDEF
   ENDWHERE
   !
ENDIF
!
!-------------------------------------------------------------------------------------
!
!*       4.     Snow properties :
!               -----------------
!
! Snow fractions
!
DMK%XPSNG  (:) = PEK%XPSNG(:)
DMK%XPSNV  (:) = PEK%XPSNV(:)
DMK%XPSN   (:) = PEK%XPSN (:)
!
! Snow total mass, snow total depth, snow total temperature
!
ZWORKDSN(:,:)=0.0
DO JL = 1,INS
   DO JI = 1,INI
      ZWORKDSN(JI,JL) = PEK%TSNOW%WSNOW(JI,JL) / PEK%TSNOW%RHO(JI,JL)
   ENDDO
ENDDO
!
ZTWSNOW  (:) = 0.
ZTDSNOW  (:) = 0.
ZTSNOWLIQ(:) = 0.
ZTSNOWAGE(:) = 0.
ZSNOWTEMP(:) = 0.  
!
IF (PEK%TSNOW%SCHEME/='EBA')THEN
   ZWORKSNTEMP(:,:) = DMK%XSNOWTEMP(:,:)
ELSE
   ZWORKSNTEMP(:,1) = MIN(PEK%XTG(:,1),XTT)
ENDIF
!
DO JL = 1,INS
  DO JI = 1,INI
    ZTWSNOW  (JI) = ZTWSNOW  (JI) + PEK%TSNOW%WSNOW(JI,JL)
    ZTDSNOW  (JI) = ZTDSNOW  (JI) + ZWORKDSN       (JI,JL)
    ZSNOWTEMP(JI) = ZSNOWTEMP(JI) + ZWORKDSN       (JI,JL) * ZWORKSNTEMP(JI,JL)
  ENDDO
ENDDO
!
WHERE(ZTDSNOW(:)>0.0)
      ZSNOWTEMP(:)=ZSNOWTEMP(:)/ZTDSNOW(:)
ELSEWHERE
      ZSNOWTEMP(:)=XUNDEF
ENDWHERE
!
DMK%XTWSNOW(:) =  ZTWSNOW  (:)
DMK%XTDSNOW(:) =  ZTDSNOW  (:)
DMK%XTTSNOW(:) =  ZSNOWTEMP(:)
!
! Snow liquid content, snow age
!
IF (PEK%TSNOW%SCHEME=='3-L' .OR. PEK%TSNOW%SCHEME=='CRO') THEN
   !
   DO JL = 1,INS
      DO JI = 1,INI
          ZTSNOWLIQ(JI) = ZTSNOWLIQ(JI) + DMK%XSNOWLIQ (JI,JL) * XRHOLW
          ZTSNOWAGE(JI) = ZTSNOWAGE(JI) + PEK%TSNOW%AGE(JI,JL) * ZWORKDSN(JI,JL)
      ENDDO
   ENDDO
   !
   IF(OVOLUMETRIC_SNOWLIQ)THEN
     !
     WHERE(ZWORKDSN(:,:)>0.0)
           DMK%XSNOWLIQ(:,:) = DMK%XSNOWLIQ(:,:) * XRHOLW / ZWORKDSN(:,:)     
     ELSEWHERE
           DMK%XSNOWLIQ(:,:) = XUNDEF
     ENDWHERE
     !
     WHERE(ZTDSNOW(:)>0.0)
          ZTSNOWLIQ(:)=ZTSNOWLIQ(:)/ZTDSNOW(:)
     ELSEWHERE
          ZTSNOWLIQ(:)=XUNDEF
     ENDWHERE
     !
   ENDIF     
   !
   WHERE(ZTDSNOW(:)>0.0)
        ZTSNOWAGE(:)=ZTSNOWAGE(:)/ZTDSNOW(:)
   ELSEWHERE
        ZTSNOWAGE(:)=XUNDEF
   ENDWHERE
   !
   DMK%XTSNOWLIQ(:) =  ZTSNOWLIQ(:)
   DMK%XTSNOWAGE(:) =  ZTSNOWAGE(:)
!
ENDIF
!
IF (PEK%TSNOW%SCHEME=='CRO' .AND. IO%LSNOWSYTRON) THEN
    DMK%XSYTMASSC(:)  =  DMK%XSYTMASSC(:) * PTSTEP
ENDIF
!
!-------------------------------------------------------------------------------------
!
!*       5.     Interactive vegetation :
!               ------------------------
!
IF (IO%CPHOTO/='NON'.AND.IO%LTR_ML) THEN
   !
   ! Mask where vegetation evolution is performed (just before solar midnight)
   GMASK = ( PTIME - PTSTEP < 0. ) .AND. ( PTIME >= 0. )
   !
   IF (GMASK) THEN
      !
      DO JI=1,INI
         !
         IF (PEK%XMUS(JI)/=0.) THEN
           DMK%XDFAPARC   (JI) = PEK%XFAPARC   (JI) / PEK%XMUS(JI) 
           DMK%XDFAPIRC   (JI) = PEK%XFAPIRC   (JI) / PEK%XMUS(JI)
           DMK%XDLAI_EFFC (JI) = PEK%XLAI_EFFC (JI) / PEK%XMUS(JI)
         ENDIF
         !
      ENDDO
      !
      DO JI=1,INI   
         PEK%XFAPARC(JI)   = 0.
         PEK%XFAPIRC(JI)   = 0.
         PEK%XLAI_EFFC(JI) = 0.
         PEK%XMUS(JI)      = 0.
      ENDDO
      !
   ENDIF
   !
ENDIF
!
!-------------------------------------------------------------------------------------
!
!*       6.     Comput active layer, frozen layer and permafrost thicknesses :
!               --------------------------------------------------------------
!
IF(IO%CISBA=='DIF')THEN
  ZPLT(:)=0.0
  ZALT(:)=0.0
  ZFLT(:)=0.0
  CALL COMPUT_COLD_LAYERS_THICK(PK%XDG,PEK%XTG,ZPLT,ZALT,ZFLT)
  DO JI=1,INI
    DMK%XPLT(JI) =  ZPLT(JI) 
    DMK%XALT(JI) =  ZALT(JI) 
    DMK%XFLT(JI) =  ZFLT(JI)  
  ENDDO
ENDIF
!
!-------------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('DIAG_MISC_ISBA_N',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------------
!
END SUBROUTINE DIAG_MISC_ISBA_n
