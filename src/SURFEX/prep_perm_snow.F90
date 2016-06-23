!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE PREP_PERM_SNOW (I, &
                           TPSNOW,PTG,PPERM_SNOW_FRAC,KSNOW)
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

!
USE MODD_ISBA_n, ONLY : ISBA_t
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
!
TYPE(ISBA_t), INTENT(INOUT) :: I
!
TYPE(SURF_SNOW), INTENT(INOUT) :: TPSNOW            ! snow mantel characteristics
REAL, DIMENSION(:,:),  INTENT(IN):: PTG             ! soil temperature for patch KSNOW
REAL, DIMENSION(:,:),  INTENT(IN):: PPERM_SNOW_FRAC ! fraction of permanent snow for patch KSNOW
INTEGER,               INTENT(IN):: KSNOW           ! patch number where permanent snow is
!
!*      0.2    declarations of local parameter
!
REAL, PARAMETER :: ZRHOL1 = 150.
!
!*      0.3    declarations of local variables
!
INTEGER                             :: JLAYER      ! loop counter on snow layers
REAL, DIMENSION(:),   ALLOCATABLE   :: ZWSNOW_PERM ! snow total reservoir due to perm. snow
REAL, DIMENSION(:),   ALLOCATABLE   :: ZWSNOW      ! initial snow total reservoir
REAL, DIMENSION(:),   ALLOCATABLE   :: ZD          ! new snow total depth
REAL, DIMENSION(:,:), ALLOCATABLE   :: ZDEPTH      ! depth of each layer
REAL, DIMENSION(:,:,:), ALLOCATABLE :: ZT          ! new snow temperature profile
REAL, DIMENSION(:),   ALLOCATABLE   :: ZPSN        ! permanent snow fraction
REAL, DIMENSION(:,:), ALLOCATABLE   :: ZWAT        ! 
!
LOGICAL, DIMENSION(:,:), ALLOCATABLE :: GWORK
INTEGER                              :: IWORK
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
IF(I%LGLACIER)THEN
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
IF(.NOT.I%LGLACIER)THEN
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
ALLOCATE(GWORK(SIZE(PTG,1),TPSNOW%NLAYER))
!
DO JLAYER=1,TPSNOW%NLAYER
!
  GWORK(:,JLAYER)=.FALSE.
!
  IF(I%LGLACIER)THEN
      WHERE(ZWSNOW_PERM(:)>0.)GWORK(:,JLAYER)=.TRUE.
  ELSE
      WHERE(ZWSNOW_PERM(:)>0..AND.TPSNOW%WSNOW(:,JLAYER,KSNOW)==0.)GWORK(:,JLAYER)=.TRUE.
  ENDIF
!
!* rho
!
  WHERE(GWORK(:,JLAYER))
    TPSNOW%RHO(:,JLAYER,KSNOW) = ZRHOSMAX
  END WHERE
!
!* albedo
!
  IF(I%LGLACIER)THEN
    WHERE(GWORK(:,JLAYER))
         TPSNOW%ALB(:,KSNOW) = (XAGLAMAX+XAGLAMIN)/2.0
    END WHERE
  ELSE
    WHERE(GWORK(:,JLAYER))
         TPSNOW%ALB(:,KSNOW) = (XANSMAX+XANSMIN)/2.0
    END WHERE
  ENDIF
!
END DO
!
IF (TPSNOW%SCHEME=='3-L'.OR.TPSNOW%SCHEME=='CRO') THEN
!
! * optimized rho perm snow profile
!
  IF(I%LGLACIER.AND.TPSNOW%NLAYER>=6)THEN
    WHERE(GWORK(:,1))
      TPSNOW%RHO(:,1,KSNOW) = ZRHOL1
    END WHERE 
    IF(TPSNOW%NLAYER>=6.AND.TPSNOW%NLAYER<12)THEN
      WHERE(GWORK(:,2))
       TPSNOW%RHO(:,2,KSNOW) = ZRHOL1 + 100.
      END WHERE 
      WHERE(GWORK(:,3))
       TPSNOW%RHO(:,3,KSNOW) = ZRHOL1 + 250.
      END WHERE 
    ELSE
      DO JLAYER=2,TPSNOW%NLAYER
         WHERE(GWORK(:,JLAYER))
              TPSNOW%RHO(:,JLAYER,KSNOW) = MIN(ZRHOSMAX,TPSNOW%RHO(:,JLAYER-1,KSNOW)+100.)
         END WHERE     
      ENDDO
    ENDIF
  ENDIF
!
! * Snow age profile
!
  DO JLAYER=1,TPSNOW%NLAYER/4
    WHERE(GWORK(:,JLAYER))
             TPSNOW%AGE(:,JLAYER,KSNOW) = 365.0*FLOAT(JLAYER-1)/ &
                                          FLOAT(TPSNOW%NLAYER)
    END WHERE
  END DO
  DO JLAYER=1+TPSNOW%NLAYER/4,TPSNOW%NLAYER
    WHERE(GWORK(:,JLAYER))
             TPSNOW%AGE(:,JLAYER,KSNOW) = 3650.*FLOAT(JLAYER-1)/ &
                                          FLOAT(TPSNOW%NLAYER) 
    END WHERE
  END DO
!
  IF(I%LGLACIER)THEN
    WHERE(GWORK(:,:))TPSNOW%AGE(:,:,KSNOW) = 0.0
  ENDIF
!
END IF
!
IF (TPSNOW%SCHEME=='CRO') THEN
DO JLAYER=1,TPSNOW%NLAYER/4
  WHERE(GWORK(:,JLAYER))
           TPSNOW%GRAN1(:,JLAYER,KSNOW) = MIN(-1.,-99.*     &
                  (1.-4*FLOAT(JLAYER)/FLOAT(TPSNOW%NLAYER))) 
           TPSNOW%GRAN2(:,JLAYER,KSNOW) = 50. 
           TPSNOW%HIST(:,JLAYER,KSNOW) = 0 
  END WHERE
END DO
DO JLAYER=1+TPSNOW%NLAYER/4,TPSNOW%NLAYER
  WHERE(GWORK(:,JLAYER))
           TPSNOW%GRAN1(:,JLAYER,KSNOW) = 99. 
           TPSNOW%GRAN2(:,JLAYER,KSNOW) = 0.0003 
           TPSNOW%HIST(:,JLAYER,KSNOW) = 0 
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
    GWORK(:,1)=.FALSE.
    IF(I%LGLACIER)THEN
       WHERE(ZWSNOW(:)>=0..AND.TPSNOW%WSNOW(:,1,KSNOW)/=XUNDEF)GWORK(:,1)=.TRUE.
    ELSE
       WHERE(ZWSNOW(:)==0..AND.TPSNOW%WSNOW(:,1,KSNOW)/=XUNDEF)GWORK(:,1)=.TRUE.
    ENDIF
    WHERE(GWORK(:,1))
      TPSNOW%WSNOW(:,1,KSNOW) = ZWSNOW_PERM(:)
    END WHERE
  CASE('3-L','CRO')
    !* grid
    ALLOCATE(ZDEPTH(SIZE(PTG,1),TPSNOW%NLAYER))
    CALL SNOW3LGRID(ZDEPTH,ZD)
    DO JLAYER=1,TPSNOW%NLAYER
      WHERE(ZWSNOW(:)>= 0. .AND. TPSNOW%WSNOW(:,JLAYER,KSNOW)/=XUNDEF)
        TPSNOW%WSNOW(:,JLAYER,KSNOW) = ZDEPTH(:,JLAYER) * TPSNOW%RHO(:,JLAYER,KSNOW)
      END WHERE
   END DO
   DEALLOCATE(ZDEPTH)

END SELECT
!
DEALLOCATE(ZD)
!-------------------------------------------------------------------------------------
!
!*       4.    Temperature of new snow
!              -----------------------
!
ALLOCATE(ZT   (SIZE(TPSNOW%WSNOW,1),SIZE(TPSNOW%WSNOW,2),SIZE(TPSNOW%WSNOW,3)))
!       
SELECT CASE(TPSNOW%SCHEME)
  CASE('1-L')
    ZT(:,:,:) = TPSNOW%T (:,:,:)
  CASE('3-L','CRO')
    CALL SNOW_HEAT_TO_T_WLIQ(TPSNOW%HEAT,TPSNOW%RHO,ZT)
END SELECT
!
!* new snow is set to deep ground temperature
!
DO JLAYER=1,TPSNOW%NLAYER
!
  GWORK(:,JLAYER)=.FALSE.
!
  IF(I%LGLACIER)THEN
      WHERE(ZWSNOW_PERM(:)>0.)GWORK(:,JLAYER)=.TRUE.
  ELSE
      WHERE(ZWSNOW_PERM(:)>0. .AND. ZWSNOW(:)==0)GWORK(:,JLAYER)=.TRUE.
  ENDIF
!  
  WHERE(GWORK(:,JLAYER))
      ZT(:,JLAYER,KSNOW) = MIN(PTG(:,SIZE(PTG,2)),XTT)
  END WHERE
!
END DO
!
!
SELECT CASE(TPSNOW%SCHEME)
  CASE('1-L')
    TPSNOW%T (:,:,:) = ZT(:,:,:)
  CASE('3-L','CRO')
    CALL SNOW_T_WLIQ_TO_HEAT(TPSNOW%HEAT,TPSNOW%RHO,ZT)
END SELECT
!
DEALLOCATE(ZT   )
DEALLOCATE(GWORK)
!
!
!-------------------------------------------------------------------------------------
!
!*       5.    Soil ice initialization for LGLACIER
!              -----------------------
!
ALLOCATE(ZWAT(SIZE(PTG,1),SIZE(PTG,2)))
!
IF(I%LGLACIER)THEN
!
  IF (I%CISBA == 'DIF') THEN
      IWORK=I%NGROUND_LAYER
      ZWAT(:,:)=I%XWFC(:,:)
  ELSE
      IWORK=2
      ZWAT(:,:)=I%XWSAT(:,:)
  ENDIF
!
  DO JLAYER=1,IWORK
     WHERE(PPERM_SNOW_FRAC(:,NVT_SNOW)>0.0)
           I%XWGI(:,JLAYER,KSNOW) = MAX(I%XWGI(:,JLAYER,KSNOW),ZWAT(:,JLAYER)*ZPSN(:))
           I%XWG (:,JLAYER,KSNOW) = MIN(I%XWG (:,JLAYER,KSNOW),MAX(I%XWSAT(:,JLAYER)-I%XWGI(:,JLAYER,KSNOW),XWGMIN))
     END WHERE
     WHERE(I%XWG(:,JLAYER,KSNOW) /= XUNDEF .AND. (I%XWG(:,JLAYER,KSNOW) + I%XWGI(:,JLAYER,KSNOW)) > I%XWSAT(:,JLAYER) )
           I%XWGI(:,JLAYER,KSNOW) = I%XWSAT(:,JLAYER)-I%XWG (:,JLAYER,KSNOW) !WGT<=WSAT
     END WHERE
  ENDDO
!
ENDIF
!
DEALLOCATE(ZWAT)
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
