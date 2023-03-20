!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE PREP_PERM_SNOW (IO, KK, PK, PEK, NPAR_VEG_IRR_USE, OWGI_ADJUST)
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
!!      M. dumont   02/2016: snow impurity content
!!      A. Druel    02/2019: adapt the code to be compatible with irrigation (and new patches)
!!      B. Decharme 07/2016: 3-L or Crocus adjustments
!!
!!------------------------------------------------------------------
!
USE MODD_SURFEX_MPI,      ONLY : NCOMM, NPROC
!
USE MODD_ISBA_OPTIONS_n,  ONLY : ISBA_OPTIONS_t
USE MODD_ISBA_n,          ONLY : ISBA_K_t, ISBA_P_t, ISBA_PE_t
!
USE MODD_TYPE_SNOW
USE MODD_CSTS,            ONLY : XTT
USE MODD_DATA_COVER_PAR,  ONLY : NVT_SNOW, NVEGTYPE
USE MODD_SNOW_PAR,        ONLY : XRHOSMAX, XANSMAX, XANSMIN, &
                                 XAGLAMAX, XAGLAMIN, XHGLA,  &
                                 XRHOSMAX_ES
USE MODD_SURF_PAR,        ONLY : XUNDEF
USE MODD_PREP_SNOW,       ONLY : NIMPUR
!
USE MODD_ISBA_PAR,        ONLY : XWGMIN
!
USE MODI_VEGTYPE_TO_PATCH
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
#ifdef SFX_MPI
INCLUDE "mpif.h"
#endif
!
!*      0.1    declarations of arguments
!
!
TYPE(ISBA_OPTIONS_t),  INTENT(INOUT) :: IO
TYPE(ISBA_K_t),        INTENT(INOUT) :: KK
TYPE(ISBA_P_t),        INTENT(INOUT) :: PK
TYPE(ISBA_PE_t),       INTENT(INOUT) :: PEK
!
INTEGER, DIMENSION(:), INTENT(IN)    :: NPAR_VEG_IRR_USE ! vegtype with irrigation
LOGICAL, DIMENSION(:), INTENT(INOUT) :: OWGI_ADJUST      ! Adjust wgi if new snow perm
!
!*      0.2    declarations of local parameter
!
REAL, DIMENSION(5), PARAMETER :: ZRHOL = (/100.,150.,250.,350.,500./)
!
!*      0.3    declarations of local variables
!
INTEGER                             :: JL          ! loop counter on snow layers
INTEGER                             :: JIMP        ! loop counter on impurity types
REAL, DIMENSION(:),   ALLOCATABLE   :: ZWSNOW_PERM ! snow total reservoir due to perm. snow
REAL, DIMENSION(:),   ALLOCATABLE   :: ZWSNOW      ! initial snow total reservoir
REAL, DIMENSION(:),   ALLOCATABLE   :: ZD          ! new snow total depth
REAL, DIMENSION(:,:), ALLOCATABLE   :: ZDEPTH      ! depth of each layer
REAL, DIMENSION(:,:), ALLOCATABLE   :: ZT          ! new snow temperature profile
REAL, DIMENSION(:),   ALLOCATABLE   :: ZPSN        ! permanent snow fraction
REAL, DIMENSION(:,:), ALLOCATABLE   :: ZWAT        ! 
REAL, DIMENSION(:,:), ALLOCATABLE   :: ZSNOWDZ_OLD
!
LOGICAL, DIMENSION(:,:), ALLOCATABLE :: GWORK
INTEGER                              :: IWORK
!
INTEGER :: INFOMPI
!
REAL, DIMENSION(SIZE(PK%XVEGTYPE_PATCH,1)):: ZVEG_SNOW
INTEGER                                   :: JTYPE, JTYPE2
!
REAL, DIMENSION(0:NPROC-1) :: ZPSN0
REAL :: ZSUM_PSN
REAL ::ZRHOSMAX
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
IF(PEK%TSNOW%SCHEME=='3-L'.OR.PEK%TSNOW%SCHEME=='CRO')THEN
  ZRHOSMAX=XRHOSMAX_ES
ENDIF
!
ALLOCATE(ZPSN(SIZE(PEK%XTG,1)))
!
ZVEG_SNOW(:) = 0.
DO JTYPE = 1, SIZE(PK%XVEGTYPE_PATCH,2)
  JTYPE2 = JTYPE
  IF (JTYPE > NVEGTYPE) JTYPE2 = NPAR_VEG_IRR_USE( JTYPE - NVEGTYPE )
  IF ( JTYPE2 == NVT_SNOW ) ZVEG_SNOW(:) = ZVEG_SNOW(:) + PK%XVEGTYPE_PATCH(:,JTYPE)
ENDDO
!
ZPSN(:) = MIN(ZVEG_SNOW(:),0.999999)
!
!* if no permanent snow present
!
ZSUM_PSN = SUM(ZPSN(:))
IF (NPROC>1) THEN
#ifdef SFX_MPI
  CALL MPI_ALLGATHER(ZSUM_PSN,KIND(ZSUM_PSN)/4,MPI_REAL,&
                     ZPSN0,KIND(ZPSN0)/4,MPI_REAL,NCOMM,INFOMPI)
#endif
ELSE
  ZPSN0(:) = ZSUM_PSN
ENDIF

IF (ALL(ZPSN0(:)==0.)) THEN
  DEALLOCATE(ZPSN) 
  IF (LHOOK) CALL DR_HOOK('PREP_PERM_SNOW',1,ZHOOK_HANDLE)
  RETURN
END IF
!
!* total snow amount due to permanent snow
!
ALLOCATE(ZWSNOW_PERM(SIZE(PEK%XTG,1)))
ZWSNOW_PERM(:) = WSNOW_FROM_SNOW_FRAC_GROUND(ZPSN)
!
!* limitation of maximum snow amount
!
IF(IO%LGLACIER)THEN
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
WHERE(PEK%XTG(:,SIZE(PEK%XTG,2))>XTT+5.) ZWSNOW_PERM(:) = 0.
!
!* initial snow content
!
ALLOCATE(ZWSNOW(SIZE(PEK%XTG,1)))
ZWSNOW(:) = 0.
DO JL=1,PEK%TSNOW%NLAYER
  ZWSNOW(:) = ZWSNOW(:) + PEK%TSNOW%WSNOW(:,JL) 
END DO
!
!-------------------------------------------------------------------------------------
!
!*       2.    Other parameters of new snow, except temperature
!              ------------------------------------------------
!
ALLOCATE(GWORK(SIZE(PEK%XTG,1),PEK%TSNOW%NLAYER))
!
DO JL=1,PEK%TSNOW%NLAYER
!
  GWORK(:,JL)=.FALSE.
!
  IF(IO%LGLACIER)THEN
    WHERE(ZWSNOW_PERM(:)>0.)GWORK(:,JL)=.TRUE.
  ELSE
    WHERE(ZWSNOW_PERM(:)>0..AND.ZWSNOW(:)==0.)GWORK(:,JL)=.TRUE.
  ENDIF
!
!* albedo
!
  IF(IO%LGLACIER)THEN
    WHERE(GWORK(:,JL))
      PEK%TSNOW%ALB(:) = (XAGLAMAX+XAGLAMIN)/2.0
    END WHERE
  ELSE
    WHERE(GWORK(:,JL))
      PEK%TSNOW%ALB(:) = (XANSMAX+XANSMIN)/2.0
    END WHERE
  ENDIF
!
ENDDO
!
!* rho must be defined for snow 3-L before temperature and heat computations
!
DO JL=1,PEK%TSNOW%NLAYER
   WHERE(ZWSNOW_PERM(:)>0..AND.ZWSNOW(:)==0.)
     PEK%TSNOW%RHO(:,JL) = ZRHOSMAX
   END WHERE
ENDDO
!
!* Optimizations
!
IF (PEK%TSNOW%SCHEME=='3-L'.OR.PEK%TSNOW%SCHEME=='CRO') THEN
!
! * optimized rho perm snow profile
!
  IF(IO%LGLACIER.AND.PEK%TSNOW%NLAYER>=6)THEN
    WHERE(ZWSNOW_PERM(:)>0.)
      PEK%TSNOW%RHO(:,1) = MIN(ZRHOL(1),PEK%TSNOW%RHO(:,1))
      PEK%TSNOW%RHO(:,2) = MIN(ZRHOL(2),PEK%TSNOW%RHO(:,2))
      PEK%TSNOW%RHO(:,3) = MIN(ZRHOL(3),PEK%TSNOW%RHO(:,3))
    END WHERE    
  ENDIF
!
  IF(IO%LGLACIER.AND.PEK%TSNOW%NLAYER>=12)THEN
    WHERE(ZWSNOW_PERM(:)>0.)
      PEK%TSNOW%RHO(:,4) = MIN(ZRHOL(4),PEK%TSNOW%RHO(:,4))
      PEK%TSNOW%RHO(:,5) = MIN(ZRHOL(5),PEK%TSNOW%RHO(:,5))
    END WHERE    
    DO JL=6,PEK%TSNOW%NLAYER
       WHERE(GWORK(:,JL))
             PEK%TSNOW%RHO(:,JL) = ZRHOSMAX
       END WHERE     
    ENDDO
  ENDIF
!
! * Snow age profile
!
  DO JL=1,PEK%TSNOW%NLAYER/4
    WHERE(GWORK(:,JL))
      PEK%TSNOW%AGE(:,JL) = 365.0*FLOAT(JL-1)/ FLOAT(PEK%TSNOW%NLAYER)
    END WHERE
  END DO
  DO JL=1+PEK%TSNOW%NLAYER/4,PEK%TSNOW%NLAYER
    WHERE(GWORK(:,JL))
      PEK%TSNOW%AGE(:,JL) = 3650.*FLOAT(JL-1)/ FLOAT(PEK%TSNOW%NLAYER) 
    END WHERE
  END DO
!
  IF(IO%LGLACIER)THEN
    WHERE(GWORK(:,:))PEK%TSNOW%AGE(:,:) = 0.0
  ENDIF
!
END IF
!
IF (PEK%TSNOW%SCHEME=='CRO') THEN
DO JL=1,PEK%TSNOW%NLAYER/4
  WHERE(GWORK(:,JL))
    PEK%TSNOW%GRAN1(:,JL) = MIN(-1.,-99.* (1.-4*FLOAT(JL)/FLOAT(PEK%TSNOW%NLAYER))) 
    PEK%TSNOW%GRAN2(:,JL) = 50. 
    PEK%TSNOW%HIST(:,JL) = 0 
  END WHERE
END DO
DO JL=1+PEK%TSNOW%NLAYER/4,PEK%TSNOW%NLAYER
  WHERE(GWORK(:,JL))
    PEK%TSNOW%GRAN1(:,JL) = 99. 
    PEK%TSNOW%GRAN2(:,JL) = 0.0003 
    PEK%TSNOW%HIST(:,JL) = 0 
  END WHERE
END DO
END IF
!
IF (PEK%TSNOW%SCHEME=='CRO') THEN
  DO JIMP=1,NIMPUR
    DO JL=1,PEK%TSNOW%NLAYER/4
      WHERE(GWORK(:,JL))
        PEK%TSNOW%IMPUR(:,JL,JIMP)=0.
      END WHERE
    END DO
    DO JL=1+PEK%TSNOW%NLAYER/4,PEK%TSNOW%NLAYER
      WHERE(GWORK(:,JL))
        PEK%TSNOW%IMPUR(:,JL,JIMP)=0.
      END WHERE
    END DO
  ENDDO
END IF
!
!-------------------------------------------------------------------------------------
!
!*       3.    Modification of snow reservoir profile
!              --------------------------------------
!
!* new total snow content
!
ZWSNOW_PERM(:) = MAX(ZWSNOW_PERM(:),ZWSNOW(:))
!
!* new total snow depth
!
ALLOCATE(ZD(SIZE(PEK%XTG,1)))
ZD(:) = 0.
DO JL=1,PEK%TSNOW%NLAYER
  ZD(:) = ZD(:) + PEK%TSNOW%WSNOW(:,JL)/PEK%TSNOW%RHO(:,JL)
END DO
ZD(:) = ZD(:) + (ZWSNOW_PERM(:)-ZWSNOW(:))/ZRHOSMAX
!
!* modified snow content profile
!
SELECT CASE(PEK%TSNOW%SCHEME)
!
  CASE('D95','1-L','EBA')
    GWORK(:,1)=.FALSE.
    IF(IO%LGLACIER)THEN
       WHERE(ZWSNOW(:)>=0..AND.PEK%TSNOW%WSNOW(:,1)/=XUNDEF)GWORK(:,1)=.TRUE.
    ELSE
       WHERE(ZWSNOW(:)==0..AND.PEK%TSNOW%WSNOW(:,1)/=XUNDEF)GWORK(:,1)=.TRUE.
    ENDIF
    WHERE(GWORK(:,1))
      PEK%TSNOW%WSNOW(:,1) = ZWSNOW_PERM(:)
    END WHERE
!
!* grid
!
  CASE('3-L','CRO')
!
    ALLOCATE(ZSNOWDZ_OLD(SIZE(PEK%XTG,1),PEK%TSNOW%NLAYER))
    DO JL=1,PEK%TSNOW%NLAYER
       ZSNOWDZ_OLD(:,JL)=ZD(:)/PEK%TSNOW%NLAYER
    ENDDO
!
! Robert: Creation of auxiliary variable for old snow layer depth
    ALLOCATE(ZDEPTH(SIZE(PEK%XTG,1),PEK%TSNOW%NLAYER))
    CALL SNOW3LGRID(ZDEPTH,ZD,ZSNOWDZ_OLD)
    DEALLOCATE(ZSNOWDZ_OLD)
!
    DO JL=1,PEK%TSNOW%NLAYER
      WHERE(ZWSNOW(:)>= 0. .AND. PEK%TSNOW%WSNOW(:,JL)/=XUNDEF)
        PEK%TSNOW%WSNOW(:,JL) = ZDEPTH(:,JL) * PEK%TSNOW%RHO(:,JL)
      END WHERE
    END DO
    DEALLOCATE(ZDEPTH)
!
END SELECT
!
DEALLOCATE(ZD)
!
!-------------------------------------------------------------------------------------
!
!*       4.    Temperature of new snow
!              -----------------------
!
ALLOCATE(ZT(SIZE(PEK%TSNOW%WSNOW,1),SIZE(PEK%TSNOW%WSNOW,2)))
!       
SELECT CASE(PEK%TSNOW%SCHEME)
  CASE('1-L')
    ZT(:,:) = PEK%TSNOW%T (:,:)
  CASE('3-L','CRO')
    CALL SNOW_HEAT_TO_T_WLIQ(PEK%TSNOW%HEAT,PEK%TSNOW%RHO,ZT)
END SELECT
!
!* new snow is set to deep ground temperature
!
DO JL=1,PEK%TSNOW%NLAYER
!
  GWORK(:,JL)=.FALSE.
!
  IF(IO%LGLACIER)THEN
      WHERE(ZWSNOW_PERM(:)>0.)GWORK(:,JL)=.TRUE.
  ELSE
      WHERE(ZWSNOW_PERM(:)>0. .AND. ZWSNOW(:)==0)GWORK(:,JL)=.TRUE.
  ENDIF
!  
  WHERE(GWORK(:,JL))
      ZT(:,JL) = MIN(PEK%XTG(:,SIZE(PEK%XTG,2)),XTT)
  END WHERE
!
END DO
!
!
SELECT CASE(PEK%TSNOW%SCHEME)
  CASE('1-L')
    PEK%TSNOW%T (:,:) = ZT(:,:)
  CASE('3-L','CRO')
    CALL SNOW_T_WLIQ_TO_HEAT(PEK%TSNOW%HEAT,PEK%TSNOW%RHO,ZT)
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
ALLOCATE(ZWAT(SIZE(PEK%XTG,1),SIZE(PEK%XTG,2)))
!
IF(IO%LGLACIER)THEN
!
  IF (IO%CISBA == 'DIF') THEN
!
     WHERE(ZWSNOW_PERM(:)>0. .AND. ZWSNOW(:)==0)
           OWGI_ADJUST(:) = .TRUE.
     END WHERE
!
  ELSE
!          
    IWORK=2
    ZWAT(:,:)=KK%XWSAT(:,:)
    DO JL=1,IWORK
       WHERE(ZVEG_SNOW>0.0)
         PEK%XWGI(:,JL) = MAX(PEK%XWGI(:,JL),ZWAT(:,JL)*ZPSN(:))
         PEK%XWG (:,JL) = MIN(PEK%XWG (:,JL), MAX(KK%XWSAT(:,JL)-PEK%XWGI(:,JL),XWGMIN))
       END WHERE
       WHERE(PEK%XWG(:,JL) /= XUNDEF .AND. (PEK%XWG(:,JL) + PEK%XWGI(:,JL)) > KK%XWSAT(:,JL) )
         PEK%XWGI(:,JL) = KK%XWSAT(:,JL)-PEK%XWG (:,JL) !WGT<=WSAT
       END WHERE
    ENDDO    
!
  ENDIF
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
CALL MKFLAG_SNOW(PEK%TSNOW)
!
IF (LHOOK) CALL DR_HOOK('PREP_PERM_SNOW',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE PREP_PERM_SNOW
