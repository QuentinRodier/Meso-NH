!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
SUBROUTINE PREP_PERM_SNOW(TPSNOW,PTG,PPERM_SNOW_FRAC,KSNOW)
!          ################################################
!
!
!!****  *PREP_PERM_SNOW* - takes into account permanent snow into prognostic snow
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
!!     V. Masson 
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2004
!!      B. Decharme 03/2009: Consistency with Arpege permanent
!!                                          snow/ice treatment
!!      B. Decharme 07/2012: 3-L or Crocus adjustments
!!      M. Lafaysse 09/2012: adaptation with new snow age in Crocus
!!------------------------------------------------------------------
!

USE MODD_TYPE_SNOW
USE MODD_CSTS,           ONLY : XTT
USE MODD_DATA_COVER_PAR, ONLY : NVT_SNOW
USE MODD_SNOW_PAR,       ONLY : XRHOSMAX, XANSMAX, XANSMIN, &
                                XAGLAMAX, XAGLAMIN, XHGLA,  &
                                XRHOSMAX_ES
USE MODD_SURF_PAR,       ONLY : XUNDEF
!
USE MODD_ISBA_PAR,       ONLY : XWGMIN
USE MODD_ISBA_n,         ONLY : CISBA,XWG,XWGI,XWSAT,   &
                                NGROUND_LAYER,LGLACIER, &
                                TTIME
!
USE MODI_SNOW_HEAT_TO_T_WLIQ
USE MODI_SNOW_T_WLIQ_TO_HEAT
USE MODI_MKFLAG_SNOW
USE MODE_SURF_SNOW_FRAC
USE MODE_SNOW3L
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
TYPE(SURF_SNOW), INTENT(INOUT) :: TPSNOW            ! snow mantel characteristics
REAL, DIMENSION(:,:),  INTENT(IN):: PTG             ! soil temperature for patch KSNOW
REAL, DIMENSION(:,:),  INTENT(IN):: PPERM_SNOW_FRAC ! fraction of permanent snow for patch KSNOW
INTEGER,               INTENT(IN):: KSNOW           ! patch number where permanent snow is
!
!*      0.2    declarations of local variables
!
INTEGER                             :: JLAYER      ! loop counter on snow layers
REAL, DIMENSION(:),   ALLOCATABLE   :: ZWSNOW_PERM ! snow total reservoir due to perm. snow
REAL, DIMENSION(:),   ALLOCATABLE   :: ZWSNOW      ! initial snow total reservoir
REAL, DIMENSION(:),   ALLOCATABLE   :: ZD          ! new snow total depth
REAL, DIMENSION(:,:), ALLOCATABLE   :: ZDEPTH      ! depth of each layer
REAL, DIMENSION(:,:,:), ALLOCATABLE :: ZT          ! new snow temperature profile
REAL, DIMENSION(:,:,:), ALLOCATABLE :: ZWLIQ       ! liquid water in the snow
REAL, DIMENSION(:),   ALLOCATABLE   :: ZPSN        ! permanent snow fraction
!
LOGICAL, DIMENSION(:),  ALLOCATABLE :: LWORK
INTEGER                             :: IWORK
!
REAL              ::ZRHOSMAX
REAL              ::ZAGE_NOW
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------
!
!*       1.    Snow where permanent snow is
!              ----------------------------
!
!* snow fraction must be at least equal to permanent snow fraction
!  The snow fraction is computed as Wsnow/(Wsnow+XWCRN)
!
!
IF (LHOOK) CALL DR_HOOK('PREP_PERM_SNOW',0,ZHOOK_HANDLE)
!
ZRHOSMAX=XRHOSMAX
IF(TPSNOW%SCHEME=='3-L'.OR.TPSNOW%SCHEME=='CRO')THEN
  ZRHOSMAX=XRHOSMAX_ES
ENDIF
!
ALLOCATE(ZPSN(SIZE(PTG,1)))
ZPSN(:) = MIN ( PPERM_SNOW_FRAC(:,NVT_SNOW) , 0.9999 )
!
!* if no permanent snow present
!
IF (ALL(ZPSN(:)==0.)) THEN
  DEALLOCATE(ZPSN) 
  IF (LHOOK) CALL DR_HOOK('PREP_PERM_SNOW',1,ZHOOK_HANDLE)
  RETURN
END IF
!
!* total snow amount due to permanent snow
!
ALLOCATE(ZWSNOW_PERM(SIZE(PTG,1)))
ZWSNOW_PERM(:) = WSNOW_FROM_SNOW_FRAC_GROUND(ZPSN)
!
!* limitation of maximum snow amount
!
IF(LGLACIER)THEN
!  limited to 33.3 meters of aged snow
   ZWSNOW_PERM(:) = MIN(ZWSNOW_PERM(:),XHGLA * ZRHOSMAX )
ELSE
!  limited to 2. meters of aged snow
   ZWSNOW_PERM(:) = MIN(ZWSNOW_PERM(:),2.0 * ZRHOSMAX )
ENDIF
!
!* permanent snow can be added only if deep soil temperature is below 5 C
!  (glaciers on subgrid mountains tops that are contained in the grid mesh are neglected)
!
IF(.NOT.LGLACIER)THEN
  WHERE(PTG(:,SIZE(PTG,2))>XTT+5.) ZWSNOW_PERM(:) = 0.
ENDIF
!
!-------------------------------------------------------------------------------------
!
!*       2.    Other parameters of new snow, except temperature
!              ------------------------------------------------
!
!* rho must be defined for snow 3-L before temperature and heat computations
!
!* rho
!
ALLOCATE(LWORK(SIZE(PTG,1)))
!
DO JLAYER=1,TPSNOW%NLAYER
!
  LWORK(:)=.FALSE.
!
  IF(LGLACIER)THEN
      WHERE(ZWSNOW_PERM(:)>0.)LWORK(:)=.TRUE.
  ELSE
      WHERE(ZWSNOW_PERM(:)>0..AND.TPSNOW%WSNOW(:,JLAYER,KSNOW)==0.)LWORK(:)=.TRUE.
  ENDIF
!
  WHERE(LWORK(:))
    TPSNOW%RHO(:,JLAYER,KSNOW) = ZRHOSMAX
  END WHERE
!
!* albedo
!
  IF(LGLACIER)THEN
    WHERE(LWORK(:))
         TPSNOW%ALB(:,KSNOW) = (XAGLAMAX+XAGLAMIN)/2.0
    END WHERE
  ELSE
    WHERE(LWORK(:))
         TPSNOW%ALB(:,KSNOW) = (XANSMAX+XANSMIN)/2.0
    END WHERE
  ENDIF
!
END DO
!
IF (TPSNOW%SCHEME=='CRO') THEN

DO JLAYER=1,TPSNOW%NLAYER/4
  WHERE(LWORK(:))
            !TPSNOW%RHO(:,JLAYER,KSNOW) = ZRHOSMAX*         &
            !      (1.+ FLOAT(JLAYER)/FLOAT(TPSNOW%NLAYER)) 
           TPSNOW%GRAN1(:,JLAYER,KSNOW) = MIN(-1.,-99.*     &
                  (1.-4*FLOAT(JLAYER)/FLOAT(TPSNOW%NLAYER))) 
           TPSNOW%GRAN2(:,JLAYER,KSNOW) = 50. 
           TPSNOW%HIST(:,JLAYER,KSNOW) = 0 
           TPSNOW%AGE(:,JLAYER,KSNOW) = 365.*FLOAT(JLAYER-1)/ &
                                        FLOAT(TPSNOW%NLAYER)
  END WHERE
END DO
DO JLAYER=1+TPSNOW%NLAYER/4,TPSNOW%NLAYER
  WHERE(LWORK(:))
           !TPSNOW%RHO(:,JLAYER,KSNOW) = ZRHOSMAX*         &
           !       (1.+ FLOAT(JLAYER)/FLOAT(TPSNOW%NLAYER)) 
           TPSNOW%GRAN1(:,JLAYER,KSNOW) = 99. 
           TPSNOW%GRAN2(:,JLAYER,KSNOW) = 0.0003 
           TPSNOW%HIST(:,JLAYER,KSNOW) = 0 
           TPSNOW%AGE(:,JLAYER,KSNOW) = 3650.*FLOAT(JLAYER-1)/ &
                                        FLOAT(TPSNOW%NLAYER) 
  END WHERE
END DO
END IF
!
!-------------------------------------------------------------------------------------
!
!*       3.    Modification of snow reservoir profile
!              --------------------------------------
!
!* initial snow content
!
ALLOCATE(ZWSNOW(SIZE(PTG,1)))
ZWSNOW(:) = 0.
DO JLAYER=1,TPSNOW%NLAYER
  ZWSNOW(:) = ZWSNOW(:) + TPSNOW%WSNOW(:,JLAYER,KSNOW) 
END DO
!
!* new total snow content
!
ZWSNOW_PERM(:) = MAX(ZWSNOW_PERM(:),ZWSNOW(:))
!
!* new total snow depth
!
ALLOCATE(ZD(SIZE(PTG,1)))
ZD(:) = 0.
DO JLAYER=1,TPSNOW%NLAYER
  ZD(:) = ZD(:) + TPSNOW%WSNOW(:,JLAYER,KSNOW)/TPSNOW%RHO(:,JLAYER,KSNOW)
END DO
ZD(:) = ZD(:) + (ZWSNOW_PERM(:)-ZWSNOW(:))/ZRHOSMAX
!
!* modified snow content profile
!
SELECT CASE(TPSNOW%SCHEME)
  CASE('D95','1-L','EBA')
    LWORK(:)=.FALSE.
    IF(LGLACIER)THEN
       WHERE(ZWSNOW(:)>=0..AND.TPSNOW%WSNOW(:,1,KSNOW)/=XUNDEF)LWORK(:)=.TRUE.
    ELSE
       WHERE(ZWSNOW(:)==0..AND.TPSNOW%WSNOW(:,1,KSNOW)/=XUNDEF)LWORK(:)=.TRUE.
    ENDIF
    WHERE(LWORK(:))
      TPSNOW%WSNOW(:,1,KSNOW) = ZWSNOW_PERM(:)
    END WHERE
  CASE('3-L','CRO')
    !* grid
    ALLOCATE(ZDEPTH(SIZE(PTG,1),TPSNOW%NLAYER))
    CALL SNOW3LGRID(ZDEPTH,ZD)
    DO JLAYER=1,TPSNOW%NLAYER
      WHERE(ZWSNOW(:)> 0. .AND. TPSNOW%WSNOW(:,JLAYER,KSNOW)/=XUNDEF)
        TPSNOW%WSNOW(:,JLAYER,KSNOW) = ZDEPTH(:,JLAYER) * TPSNOW%RHO(:,JLAYER,KSNOW)
      END WHERE
      WHERE(ZWSNOW(:)==0. .AND. TPSNOW%WSNOW(:,JLAYER,KSNOW)/=XUNDEF)
        TPSNOW%WSNOW(:,JLAYER,KSNOW) = ZDEPTH(:,JLAYER) * ZRHOSMAX
      END WHERE
   END DO
   DEALLOCATE(ZDEPTH)

END SELECT
!
DEALLOCATE(ZD)
DEALLOCATE(LWORK)
!-------------------------------------------------------------------------------------
!
!*       4.    Temperature of new snow
!              -----------------------
!
ALLOCATE(ZT   (SIZE(TPSNOW%WSNOW,1),SIZE(TPSNOW%WSNOW,2),SIZE(TPSNOW%WSNOW,3)))
ALLOCATE(ZWLIQ(SIZE(TPSNOW%WSNOW,1),SIZE(TPSNOW%WSNOW,2),SIZE(TPSNOW%WSNOW,3)))
!
SELECT CASE(TPSNOW%SCHEME)
  CASE('1-L')
    ZT(:,:,:) = TPSNOW%T (:,:,:)
  CASE('3-L','CRO')
    CALL SNOW_HEAT_TO_T_WLIQ(TPSNOW%HEAT,TPSNOW%RHO,ZT,ZWLIQ)
END SELECT
!
!* new snow is set to deep ground temperature
!
DO JLAYER=1,TPSNOW%NLAYER
  WHERE(ZWSNOW_PERM(:)>0. .AND. ZWSNOW(:)==0.)
    ZT(:,JLAYER,KSNOW) = MIN(PTG(:,SIZE(PTG,2)), XTT)
  END WHERE
END DO
!
SELECT CASE(TPSNOW%SCHEME)
  CASE('1-L')
    TPSNOW%T (:,:,:) = ZT(:,:,:)
  CASE('3-L','CRO')
    CALL SNOW_T_WLIQ_TO_HEAT(TPSNOW%HEAT,TPSNOW%RHO,ZT,ZWLIQ)
END SELECT
!
DEALLOCATE(ZT   )
DEALLOCATE(ZWLIQ)
!
!
!-------------------------------------------------------------------------------------
!
!*       5.    Soil ice initialization for LGLACIER
!              -----------------------
!
IF(LGLACIER)THEN
!
  IF (CISBA == 'DIF') THEN
      IWORK=NGROUND_LAYER
  ELSE
      IWORK=2
  ENDIF
!
  DO JLAYER=1,IWORK
     WHERE(PPERM_SNOW_FRAC(:,NVT_SNOW)>0.0)
           XWGI(:,JLAYER,KSNOW) = MAX(XWGI(:,JLAYER,KSNOW),XWSAT(:,JLAYER)*ZPSN(:))
           XWG (:,JLAYER,KSNOW) = MIN(XWG (:,JLAYER,KSNOW),MAX(XWSAT(:,JLAYER)-XWGI(:,JLAYER,KSNOW),XWGMIN))
     END WHERE
     WHERE(XWG(:,JLAYER,KSNOW) /= XUNDEF .AND. (XWG(:,JLAYER,KSNOW) + XWGI(:,JLAYER,KSNOW)) > XWSAT(:,JLAYER) )
           XWGI(:,JLAYER,KSNOW) = XWSAT(:,JLAYER)-XWG (:,JLAYER,KSNOW) !WGT<=WSAT
     END WHERE
  ENDDO
!
ENDIF
!
DEALLOCATE(ZPSN)
!
!-------------------------------------------------------------------------------------
!
!*       6.    Masking where there is no snow
!              ------------------------------
!
 CALL MKFLAG_SNOW(TPSNOW)
IF (LHOOK) CALL DR_HOOK('PREP_PERM_SNOW',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE PREP_PERM_SNOW
