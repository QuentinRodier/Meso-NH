!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
SUBROUTINE PREP_VER_SNOW(TPSNOW,PZS_LS,PZS,PTG_LS,PTG,KDEEP_SOIL)
!          ###########################################
!
!
!!****  *PREP_VER_SNOW* - change in snow variables due to altitude change
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
!!       02/2014    E. Martin vertical correction applied to snow cover and not by layers
!!------------------------------------------------------------------
!

USE MODD_TYPE_SNOW
USE MODD_CSTS,       ONLY : XTT
USE MODD_PREP,       ONLY : XT_CLIM_GRAD
USE MODD_PREP_SNOW,  ONLY : XWSNOW_CLIM_GRAD
USE MODD_SURF_PAR,   ONLY : XUNDEF
!
USE MODI_SNOW_HEAT_TO_T_WLIQ
USE MODI_SNOW_T_WLIQ_TO_HEAT
USE MODI_MKFLAG_SNOW
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
TYPE(SURF_SNOW), INTENT(INOUT) :: TPSNOW ! snow mantel characteristics
REAL, DIMENSION(:), INTENT(IN) :: PZS_LS ! initial orography
REAL, DIMENSION(:), INTENT(IN) :: PZS    ! final   orography
REAL, DIMENSION(:,:,:),INTENT(IN),OPTIONAL:: PTG_LS ! soil temperature on initial orography
REAL, DIMENSION(:,:,:),INTENT(IN),OPTIONAL:: PTG    ! soil temperature on final   orography
INTEGER,               INTENT(IN),OPTIONAL:: KDEEP_SOIL ! index of deep soil temperature
!
!*      0.2    declarations of local variables
!
INTEGER                             :: IPATCH    ! number of patches
INTEGER                             :: JPATCH    ! loop counter on patches
INTEGER                             :: JLAYER    ! loop counter on snow layers
REAL, DIMENSION(:,:,:), ALLOCATABLE :: ZWSNOW_LS ! snow reservoir   on initial orography
REAL, DIMENSION(:,:,:), ALLOCATABLE :: ZTSNOW_LS ! snow temperature on initial orography
REAL, DIMENSION(:,:,:), ALLOCATABLE :: ZWSNOW    ! snow content     on final   orography
REAL, DIMENSION(:,:,:), ALLOCATABLE :: ZTSNOW    ! snow temperature on final   orography
REAL, DIMENSION(:,:,:), ALLOCATABLE :: ZWSNOW2   ! snow content     on final   orography
REAL, DIMENSION(:,:,:), ALLOCATABLE :: ZTSNOW2   ! snow temperature on final   orography
REAL, DIMENSION(:,:,:), ALLOCATABLE :: ZWLIQ     ! snow liquid water content
REAL, DIMENSION(:,:),   ALLOCATABLE :: ZZSFREEZE ! altitude where deep soil freezes
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('PREP_VER_SNOW',0,ZHOOK_HANDLE)
IPATCH = SIZE(TPSNOW%WSNOW,3)
!
!*       1.    Snow reservoir on initial orography
!              -----------------------------------
!
ALLOCATE(ZWSNOW_LS(SIZE(TPSNOW%WSNOW,1),SIZE(TPSNOW%WSNOW,2),IPATCH))
ZWSNOW_LS(:,:,:) =  TPSNOW%WSNOW(:,:,:)
!
!-------------------------------------------------------------------------------------
!
!*       2.    temperature of snow on initial orography
!              ----------------------------------------
!
ALLOCATE(ZTSNOW_LS(SIZE(TPSNOW%WSNOW,1),SIZE(TPSNOW%WSNOW,2),IPATCH))
SELECT CASE(TPSNOW%SCHEME)
  CASE ('D95','EBA')
    IF (PRESENT(PTG_LS)) THEN
      DO JPATCH=1,IPATCH
        ZTSNOW_LS(:,1,JPATCH) =  MIN(PTG_LS(:,1,JPATCH),XTT)
      END DO
    ELSE
      ZTSNOW_LS = XUNDEF
    END IF
  CASE ('1-L')
    ZTSNOW_LS(:,:,:) =  TPSNOW%T(:,:,:)
  CASE ('3-L','CRO')
    CALL SNOW_HEAT_TO_T_WLIQ(TPSNOW%HEAT(:,:,:),TPSNOW%RHO(:,:,:),ZTSNOW_LS(:,:,:))
END SELECT
!
!-------------------------------------------------------------------------------------
!
!*       3.    vertical shift of temperature
!              -----------------------------
!
ALLOCATE(ZTSNOW(SIZE(TPSNOW%WSNOW,1),SIZE(TPSNOW%WSNOW,2),IPATCH))
DO JPATCH=1,IPATCH
  DO JLAYER=1,TPSNOW%NLAYER
    ZTSNOW(:,JLAYER,JPATCH) = ZTSNOW_LS(:,JLAYER,JPATCH) + XT_CLIM_GRAD  * (PZS(:) - PZS_LS(:))  
  END DO
END DO
!
!-------------------------------------------------------------------------------------
!
!*       4.    vertical shift of snow content where snow already exists
!              ------------------------------
!
!* use of climatological snow content gradient
!
ALLOCATE(ZWSNOW(SIZE(TPSNOW%WSNOW,1),SIZE(TPSNOW%WSNOW,2),IPATCH))
!
ZWSNOW(:,:,:) = ZWSNOW_LS(:,:,:) 
!
IF (PRESENT(PTG)) THEN
  DO JPATCH=1,IPATCH
    DO JLAYER=1,TPSNOW%NLAYER
      WHERE(ZWSNOW_LS(:,JLAYER,JPATCH)>0..AND.((PTG(:,KDEEP_SOIL,JPATCH)-XTT >= 2.).OR.(PZS(:) > PZS_LS(:))))
        ZWSNOW(:,JLAYER,JPATCH) = ZWSNOW_LS(:,JLAYER,JPATCH) + ( XWSNOW_CLIM_GRAD  * (PZS(:) - PZS_LS(:))/TPSNOW%NLAYER)  
        ZWSNOW(:,JLAYER,JPATCH) = MAX(ZWSNOW(:,JLAYER,JPATCH),0.)
      END WHERE
    END DO
  END DO
ELSE
  DO JPATCH=1,IPATCH
    DO JLAYER=1,TPSNOW%NLAYER
      WHERE(ZWSNOW_LS(:,JLAYER,JPATCH)>0.)
        ZWSNOW(:,JLAYER,JPATCH) = ZWSNOW_LS(:,JLAYER,JPATCH) + ( XWSNOW_CLIM_GRAD  * (PZS(:) - PZS_LS(:))/TPSNOW%NLAYER)
        ZWSNOW(:,JLAYER,JPATCH) = MAX(ZWSNOW(:,JLAYER,JPATCH),0.)
      END WHERE
    END DO
  END DO
ENDIF
!
WHERE(TPSNOW%WSNOW(:,:,:)/=XUNDEF) TPSNOW%WSNOW = ZWSNOW
!
!-------------------------------------------------------------------------------------
!
!        5.    Where snow did not exist on initial orography
!              ---------------------------------------------
!
!* in this case, new snow can appear only if orography differences in larger
!  than 1000m, and starts at an altitude where the deep soil temperature becomes negative
!
!* the same climatological gradient is used, but the value zero for the snow
!  content is defined as the altitude where deep soil freezes.
!
!*       5.1   Altitude where deep soil freezes (only if soil temperatures are provided)
!              --------------------------------
!
IF (PRESENT(PTG)) THEN
  ALLOCATE(ZZSFREEZE(SIZE(TPSNOW%WSNOW,1),IPATCH))
  DO JPATCH=1,IPATCH
    ZZSFREEZE(:,JPATCH) = PZS &
                          + (XTT - PTG(:,KDEEP_SOIL,JPATCH)) / XT_CLIM_GRAD  
  END DO
!
!*       5.2   Amount and Temperature of new snow (only if soil temperatures are provided)
!              ----------------------------------
!
!* Snow temperature is then defined as the deep soil temperature at the final
!  altitude.
!
  ALLOCATE(ZWSNOW2(SIZE(TPSNOW%WSNOW,1),TPSNOW%NLAYER,IPATCH))
  ALLOCATE(ZTSNOW2(SIZE(TPSNOW%WSNOW,1),TPSNOW%NLAYER,IPATCH))
  DO JPATCH=1,IPATCH
    DO JLAYER=1,TPSNOW%NLAYER
      ZWSNOW2(:,JLAYER,JPATCH) =  XWSNOW_CLIM_GRAD  * (PZS(:) - ZZSFREEZE(:,JPATCH))/TPSNOW%NLAYER
      ZWSNOW2(:,JLAYER,JPATCH) = MAX(ZWSNOW2(:,JLAYER,JPATCH),0.)
      ZTSNOW2      (:,JLAYER,JPATCH) = PTG(:,KDEEP_SOIL,JPATCH)
    END DO
  END DO
!
!*       5.3   Apply maximum between this value and the shifted one
!              ----------------------------------------------------
!
  DO JPATCH=1,IPATCH
    DO JLAYER=1,TPSNOW%NLAYER
      WHERE(TPSNOW%WSNOW(:,JLAYER,JPATCH)/=XUNDEF .AND. ZWSNOW_LS(:,JLAYER,JPATCH)==0. &
                                                    .AND. (PZS(:)-PZS_LS(:))>1000. )  
        TPSNOW%WSNOW(:,JLAYER,JPATCH) = ZWSNOW2(:,JLAYER,JPATCH)
        ZTSNOW      (:,JLAYER,JPATCH) = ZTSNOW2(:,JLAYER,JPATCH)
      END WHERE
    END DO
  END DO

  DEALLOCATE(ZZSFREEZE)
  DEALLOCATE(ZWSNOW2  )
  DEALLOCATE(ZTSNOW2  )
END IF
!
!-------------------------------------------------------------------------------------
!
!*       6.    Coherence between temperature and snow content
!              ----------------------------------------------
!
SELECT CASE(TPSNOW%SCHEME)
  CASE('1-L')
    !* snow temperature cannot be larger than 0 C
    TPSNOW%T (:,:,:) = MIN ( ZTSNOW(:,:,:), XTT ) 
  CASE('3-L','CRO')
    ALLOCATE(ZWLIQ(SIZE(TPSNOW%WSNOW,1),SIZE(TPSNOW%WSNOW,2),IPATCH))
    CALL SNOW_T_WLIQ_TO_HEAT(TPSNOW%HEAT,TPSNOW%RHO,ZTSNOW)
    CALL SNOW_HEAT_TO_T_WLIQ(TPSNOW%HEAT,TPSNOW%RHO,ZTSNOW,ZWLIQ)
    CALL SNOW_T_WLIQ_TO_HEAT(TPSNOW%HEAT,TPSNOW%RHO,ZTSNOW,ZWLIQ)
    DEALLOCATE(ZWLIQ)
END SELECT
!
!-------------------------------------------------------------------------------------
!
!*       7.    Masking where there is no snow
!              ------------------------------
!
 CALL MKFLAG_SNOW(TPSNOW)
!
!-------------------------------------------------------------------------------------
DEALLOCATE(ZWSNOW_LS)
DEALLOCATE(ZTSNOW_LS)
DEALLOCATE(ZWSNOW   )
DEALLOCATE(ZTSNOW   )
IF (LHOOK) CALL DR_HOOK('PREP_VER_SNOW',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------------
!
END SUBROUTINE PREP_VER_SNOW
