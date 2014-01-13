!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
SUBROUTINE PREP_HOR_SNOW_FIELD( HPROGRAM,                       &
                                HFILE,HFILETYPE,                &
                                HFILEPGD,HFILEPGDTYPE,          &
                                KLUOUT,OUNIF,HSNSURF,KPATCH,    &
                                KL,TPSNOW, TPTIME,              &
                                PUNIF_WSNOW, PUNIF_RSNOW,       &
                                PUNIF_TSNOW, PUNIF_ASNOW,       &
                                OSNOW_IDEAL,                    &
                                PUNIF_SG1SNOW, PUNIF_SG2SNOW,   &
                                PUNIF_HISTSNOW,PUNIF_AGESNOW,   &                                
                                PF,PDEPTH,PVEGTYPE_PATCH,PPATCH   )
!     #######################################################
!
!!****  *PREP_HOR_SNOW_FIELD* - reads, interpolates and prepares a snow field
!!
!!    PURPOSE
!!    -------
!!
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
!!      P. Le Moigne 10/2005, Phasage Arome
!!------------------------------------------------------------------
!
USE MODD_TYPE_SNOW
USE MODD_TYPE_DATE_SURF, ONLY : DATE_TIME
!
USE MODD_CSTS,           ONLY : XTT
USE MODD_PREP_SNOW,      ONLY : XGRID_SNOW
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODD_DATA_COVER_PAR, ONLY : NVEGTYPE
USE MODD_PREP,           ONLY : LINTERP
!
USE MODI_PREP_SNOW_GRIB
USE MODI_PREP_SNOW_UNIF
USE MODI_PREP_SNOW_EXTERN
USE MODI_PREP_SNOW_BUFFER
USE MODI_HOR_INTERPOL
USE MODI_VEGTYPE_GRID_TO_PATCH_GRID
USE MODI_SNOW_HEAT_TO_T_WLIQ
USE MODI_VEGTYPE_TO_PATCH
!
USE MODI_ABOR1_SFX
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
 CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
 CHARACTER(LEN=28),  INTENT(IN)  :: HFILE     ! file name
 CHARACTER(LEN=6),   INTENT(IN)  :: HFILETYPE ! file type
 CHARACTER(LEN=28),  INTENT(IN)  :: HFILEPGD     ! file name
 CHARACTER(LEN=6),   INTENT(IN)  :: HFILEPGDTYPE ! file type
INTEGER,            INTENT(IN)  :: KLUOUT    ! logical unit of output listing
LOGICAL,            INTENT(IN)  :: OUNIF     ! flag for prescribed uniform field
 CHARACTER(LEN=10)               :: HSNSURF   ! type of field
INTEGER,            INTENT(IN)  :: KPATCH    ! patch number for output scheme
INTEGER,            INTENT(IN)  :: KL        ! number of points
TYPE(SURF_SNOW)                 :: TPSNOW    ! snow fields
TYPE(DATE_TIME),    INTENT(IN)  :: TPTIME    ! date and time
REAL, DIMENSION(:), INTENT(IN)  :: PUNIF_WSNOW ! prescribed snow content (kg/m2)
REAL, DIMENSION(:), INTENT(IN)  :: PUNIF_RSNOW ! prescribed density (kg/m3)
REAL, DIMENSION(:), INTENT(IN)  :: PUNIF_TSNOW ! prescribed temperature (K)
REAL,               INTENT(IN)  :: PUNIF_ASNOW ! prescribed albedo (-)
LOGICAL,            INTENT(IN)  :: OSNOW_IDEAL
REAL, DIMENSION(:), INTENT(IN)  :: PUNIF_SG1SNOW ! 
REAL, DIMENSION(:), INTENT(IN)  :: PUNIF_SG2SNOW ! 
REAL, DIMENSION(:), INTENT(IN)  :: PUNIF_HISTSNOW ! 
REAL, DIMENSION(:), INTENT(IN)  :: PUNIF_AGESNOW ! 

REAL,DIMENSION(:,:,:),  INTENT(OUT),OPTIONAL :: PF     ! output field (x,kpatch)
REAL,DIMENSION(:,:,:),INTENT(IN), OPTIONAL :: PDEPTH ! thickness of each snow layer
REAL,DIMENSION(:,:,:),  INTENT(IN), OPTIONAL :: PVEGTYPE_PATCH ! fraction of each patch
REAL,DIMENSION(:,:),  INTENT(IN), OPTIONAL :: PPATCH ! fraction of each patch
!
!
!*      0.2    declarations of local variables
!
REAL, POINTER, DIMENSION(:,:,:)     :: ZFIELDIN  ! field to interpolate horizontally
REAL, POINTER, DIMENSION(:,:)       :: ZFIELD ! field to interpolate horizontally
REAL, ALLOCATABLE, DIMENSION(:,:,:) :: ZFIELDOUT ! field interpolated   horizontally
REAL, ALLOCATABLE, DIMENSION(:,:)   :: ZD        ! snow depth (x, kpatch)
REAL, ALLOCATABLE, DIMENSION(:,:,:) :: ZW        ! work array (x, fine   snow grid, kpatch)
REAL, ALLOCATABLE, DIMENSION(:,:,:) :: ZHEAT     ! work array (x, output snow grid, kpatch)
REAL, ALLOCATABLE, DIMENSION(:,:,:) :: ZGRID     ! grid array (x, output snow grid, kpatch)
!
LOGICAL                       :: GSNOW_IDEAL
INTEGER                       :: JPATCH    ! loop on patches
INTEGER                       :: JVEGTYPE  ! loop on vegtypes
INTEGER                       :: JLAYER    ! loop on layers
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!----------------------------------------------------------------------------
!
!*      1.     Does the field exist?
!
!
IF (LHOOK) CALL DR_HOOK('PREP_HOR_SNOW_FIELD',0,ZHOOK_HANDLE)
IF (HSNSURF(1:3)=='HEA' .AND. TPSNOW%SCHEME=='D95' .AND. LHOOK) CALL DR_HOOK('PREP_HOR_SNOW_FIELD',1,ZHOOK_HANDLE)
IF (HSNSURF(1:3)=='HEA' .AND. TPSNOW%SCHEME=='D95') RETURN
!
GSNOW_IDEAL = .FALSE.
!
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
!*      2.     Reading of input  configuration (Grid and interpolation type)
!
IF (OUNIF) THEN
  GSNOW_IDEAL = OSNOW_IDEAL
  CALL PREP_SNOW_UNIF(KLUOUT,HSNSURF,ZFIELDIN, TPTIME, GSNOW_IDEAL,       &
                      PUNIF_WSNOW, PUNIF_RSNOW, PUNIF_TSNOW,              &
                      PUNIF_ASNOW, PUNIF_SG1SNOW,                         &
                      PUNIF_SG2SNOW, PUNIF_HISTSNOW, PUNIF_AGESNOW        )
ELSE IF (HFILETYPE=='GRIB  ') THEN
  CALL PREP_SNOW_GRIB(HPROGRAM,HSNSURF,HFILE,KLUOUT,ZFIELDIN)
ELSE IF (HFILETYPE=='MESONH' .OR. HFILETYPE=='ASCII ' .OR. HFILETYPE=='LFI   ') THEN
  GSNOW_IDEAL = OSNOW_IDEAL
  CALL PREP_SNOW_EXTERN(HPROGRAM,HSNSURF,HFILE,HFILETYPE,HFILEPGD,HFILEPGDTYPE,&
                        KLUOUT,ZFIELDIN,GSNOW_IDEAL,TPSNOW%NLAYER)
ELSE IF (HFILETYPE=='BUFFER') THEN
  CALL PREP_SNOW_BUFFER(HPROGRAM,HSNSURF,KLUOUT,ZFIELDIN)
ELSE
  CALL ABOR1_SFX('PREP_HOR_SNOW_FIELD: data file type not supported : '//HFILETYPE)
END IF
!
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
!*      3.     Horizontal interpolation
!
ALLOCATE(ZFIELDOUT(KL,SIZE(ZFIELDIN,2),SIZE(ZFIELDIN,3)))
ALLOCATE(ZFIELD(SIZE(ZFIELDIN,1),SIZE(ZFIELDIN,2)))
!
!
DO JVEGTYPE = 1, SIZE(ZFIELDIN,3)
  JPATCH = 1
  IF (KPATCH>1) JPATCH = VEGTYPE_TO_PATCH(JVEGTYPE,KPATCH)
  IF (PRESENT(PDEPTH)) THEN
    !* does not interpolates snow caracteristics on points without snow
    LINTERP(:) = ( PDEPTH(:,1,JPATCH) /= 0. .AND. PDEPTH(:,1,JPATCH) /= XUNDEF )
    IF (PRESENT(PPATCH)) LINTERP(:) = LINTERP(:) .AND. (PPATCH(:,JPATCH)>0.)
  ELSEIF (PRESENT(PPATCH))THEN
    LINTERP(:) = (PPATCH(:,JPATCH)>0.)
  ENDIF
  !* horizontal interpolation
  ZFIELD=ZFIELDIN(:,:,JVEGTYPE)
  CALL HOR_INTERPOL(KLUOUT,ZFIELD,ZFIELDOUT(:,:,JVEGTYPE))
  !
  LINTERP(:) = .TRUE.
END DO
!
DEALLOCATE(ZFIELD)
!
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
!*      4.     Transformation from vegtype grid to patch grid, if any
!
ALLOCATE(ZW (SIZE(ZFIELDOUT,1),SIZE(ZFIELDOUT,2),KPATCH))
!
ZW = 0.
IF (SIZE(ZFIELDOUT,3)==NVEGTYPE) THEN
  CALL VEGTYPE_GRID_TO_PATCH_GRID(KPATCH,PVEGTYPE_PATCH,PPATCH,ZFIELDOUT,ZW)
ELSE
  DO JPATCH=1,KPATCH
    ZW(:,:,JPATCH) = ZFIELDOUT(:,:,1)
  END DO
END IF
!
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
!*      5.     Defines normalized output grid, if depths of snow layers are present
!
IF (PRESENT(PDEPTH) .AND. .NOT.GSNOW_IDEAL) THEN
!
!* total snow depth
!
  ALLOCATE(ZD(SIZE(TPSNOW%WSNOW,1),KPATCH))
  ZD(:,:)=0.
  DO JPATCH=1,KPATCH
    DO JLAYER=1,TPSNOW%NLAYER
      WHERE (PDEPTH(:,JLAYER,JPATCH)/=XUNDEF) ZD(:,JPATCH) = ZD(:,JPATCH) + PDEPTH(:,JLAYER,JPATCH)
    END DO
  END DO
!
!* grid at center of layers
!
  ALLOCATE(ZGRID(SIZE(ZW,1),TPSNOW%NLAYER,KPATCH))
  DO JPATCH=1,KPATCH
    ZGRID(:,1,JPATCH) = 0.5 * PDEPTH(:,1,JPATCH)
    DO JLAYER=2,TPSNOW%NLAYER
      ZGRID(:,JLAYER,JPATCH) = ZGRID(:,JLAYER-1,JPATCH) + 0.5 * PDEPTH(:,JLAYER-1,JPATCH) &
                                                        + 0.5 * PDEPTH(:,JLAYER  ,JPATCH)
    END DO
  END DO
!
!* normalized grid
!
  DO JPATCH=1,KPATCH
    DO JLAYER=1,TPSNOW%NLAYER
      WHERE (ZD(:,JPATCH)/=0.)
        ZGRID(:,JLAYER,JPATCH) = ZGRID(:,JLAYER,JPATCH) / ZD(:,JPATCH)
      ELSEWHERE
        ZGRID(:,JLAYER,JPATCH) = 0.5
      END WHERE
    END DO
  END DO
!
  DEALLOCATE(ZD)
!
ELSEIF (.NOT.GSNOW_IDEAL) THEN
  IF (HSNSURF(1:3)=='RHO' .OR. HSNSURF(1:3)=='HEA') THEN
    WRITE(KLUOUT,*) 'when interpolation profiles of snow pack quantities,'
    WRITE(KLUOUT,*) 'depth of snow layers must be given'
    CALL ABOR1_SFX('PREP_HOR_SNOW_FIELD: DEPTH OF SNOW LAYERS NEEDED')
  END IF
END IF
!
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
!*      6.     Return to historical variable
!
SELECT CASE (HSNSURF(1:3))
  !
  CASE('DEP','WWW')  ! total snow depth or snow content
    !
    DO JPATCH=1,KPATCH
      IF (GSNOW_IDEAL) THEN
        PF(:,:,JPATCH) = ZW(:,:,JPATCH)
      ELSE
        DO JLAYER=1,SIZE(PF,2)
          PF(:,JLAYER,JPATCH) = ZW(:,1,JPATCH)
        ENDDO
      ENDIF
    END DO
    !
    IF (PRESENT(PPATCH)) THEN
      DO JLAYER = 1,TPSNOW%NLAYER
        WHERE(PPATCH(:,:)==0.)
          PF(:,JLAYER,:) = XUNDEF
        END WHERE
      ENDDO
    ENDIF
  !
  !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !
  CASE('RHO') 
    !
    IF (GSNOW_IDEAL) THEN
      TPSNOW%RHO = ZW
    ELSE
      !* interpolation on snow levels
      CALL INIT_FROM_REF_GRID(XGRID_SNOW,ZW,ZGRID,TPSNOW%RHO)
    ENDIF
    !
    !* mask for areas where there is no snow
    DO JPATCH=1,KPATCH
      DO JLAYER=1,TPSNOW%NLAYER
        WHERE(PDEPTH(:,1,JPATCH)==0. .OR. PDEPTH(:,1,JPATCH)==XUNDEF) TPSNOW%RHO(:,JLAYER,JPATCH) = XUNDEF
      END DO
    END DO
  !
  !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !
  CASE('ALB')
    !
    DO JPATCH=1,KPATCH
      TPSNOW%ALB(:,JPATCH) = ZW(:,1,JPATCH)
    END DO
    !
    !* mask for areas where there is no snow
    DO JPATCH=1,KPATCH
      WHERE(PDEPTH(:,1,JPATCH)==0. .OR. PDEPTH(:,1,JPATCH)==XUNDEF)  TPSNOW%ALB(:,JPATCH) = XUNDEF
    END DO
  !
  !- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
  !
  CASE('HEA') 
    !
    IF (TPSNOW%SCHEME=='3-L' .OR. TPSNOW%SCHEME=='CRO') THEN
      !
      IF (GSNOW_IDEAL) THEN
        TPSNOW%HEAT = ZW
      ELSE
        !* interpolation of heat on snow levels
        CALL INIT_FROM_REF_GRID(XGRID_SNOW,ZW,ZGRID,TPSNOW%HEAT)
      ENDIF
      !
      !* mask for areas where there is no snow
      DO JPATCH=1,KPATCH
        DO JLAYER=1,TPSNOW%NLAYER
          WHERE(PDEPTH(:,1,JPATCH)==0. .OR. PDEPTH(:,1,JPATCH)==XUNDEF) TPSNOW%HEAT(:,JLAYER,JPATCH) = XUNDEF
        END DO
      END DO
      !
    ELSE IF (TPSNOW%SCHEME=='1-L') THEN
      !* interpolation of heat on snow levels
      ALLOCATE(ZHEAT(SIZE(ZFIELDOUT,1),TPSNOW%NLAYER,KPATCH))
      !
      IF (GSNOW_IDEAL) THEN
        ZHEAT = ZW
      ELSE
        CALL INIT_FROM_REF_GRID(XGRID_SNOW,ZW,ZGRID,ZHEAT)
      ENDIF
      !
      !* transformation from heat to temperature
      CALL SNOW_HEAT_TO_T_WLIQ(ZHEAT,TPSNOW%RHO,TPSNOW%T)
      WHERE (TPSNOW%T>XTT) TPSNOW%T = XTT
      DEALLOCATE(ZHEAT)
      !
      !* mask for areas where there is no snow
      DO JPATCH=1,KPATCH
        DO JLAYER=1,TPSNOW%NLAYER
          WHERE(PDEPTH(:,1,JPATCH)==0. .OR. PDEPTH(:,1,JPATCH)==XUNDEF) TPSNOW%T(:,JLAYER,JPATCH) = XUNDEF
        END DO
      END DO
      !
    END IF
  !
  !
  CASE('SG1')
    !
    IF (GSNOW_IDEAL) THEN
      TPSNOW%GRAN1 = ZW
    ELSE
      !* interpolation of heat on snow levels
      CALL INIT_FROM_REF_GRID(XGRID_SNOW,ZW,ZGRID,TPSNOW%GRAN1)
    ENDIF
    !
    !* mask for areas where there is no snow
    DO JPATCH=1,KPATCH
      DO JLAYER=1,TPSNOW%NLAYER
        WHERE(PDEPTH(:,1,JPATCH)==0. .OR. PDEPTH(:,1,JPATCH)==XUNDEF) TPSNOW%GRAN1(:,JLAYER,JPATCH) = XUNDEF
      END DO
    END DO
    !
  CASE('SG2')
    !
    IF (GSNOW_IDEAL) THEN
      TPSNOW%GRAN2 = ZW
    ELSE
      !* interpolation of heat on snow levels
      CALL INIT_FROM_REF_GRID(XGRID_SNOW,ZW,ZGRID,TPSNOW%GRAN2)
    ENDIF
    !
    !* mask for areas where there is no snow
    DO JPATCH=1,KPATCH
      DO JLAYER=1,TPSNOW%NLAYER
        WHERE(PDEPTH(:,1,JPATCH)==0. .OR. PDEPTH(:,1,JPATCH)==XUNDEF) TPSNOW%GRAN2(:,JLAYER,JPATCH) = XUNDEF
      END DO
    END DO
    !
  CASE('HIS')
    !
    IF (GSNOW_IDEAL) THEN
      TPSNOW%HIST = ZW
    ELSE
      !* interpolation of heat on snow levels
      CALL INIT_FROM_REF_GRID(XGRID_SNOW,ZW,ZGRID,TPSNOW%HIST)
    ENDIF
    !
    !* mask for areas where there is no snow
    DO JPATCH=1,KPATCH
      DO JLAYER=1,TPSNOW%NLAYER
        WHERE(PDEPTH(:,1,JPATCH)==0. .OR. PDEPTH(:,1,JPATCH)==XUNDEF) TPSNOW%HIST(:,JLAYER,JPATCH) = XUNDEF
      END DO
    END DO
    !
  CASE('AGE')
    !
    IF (GSNOW_IDEAL) THEN
      TPSNOW%AGE = ZW
    ELSE
      !* interpolation of heat on snow levels
      CALL INIT_FROM_REF_GRID(XGRID_SNOW,ZW,ZGRID,TPSNOW%AGE)
    ENDIF
    !
    !* mask for areas where there is no snow
    DO JPATCH=1,KPATCH
      DO JLAYER=1,TPSNOW%NLAYER
        WHERE(PDEPTH(:,1,JPATCH)==0. .OR. PDEPTH(:,1,JPATCH)==XUNDEF) TPSNOW%AGE(:,JLAYER,JPATCH) = XUNDEF
      END DO
    END DO
    !
END SELECT
!
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
!*      7.     Deallocations
!
DEALLOCATE(ZFIELDIN )
DEALLOCATE(ZFIELDOUT)
IF (PRESENT(PDEPTH) .AND. .NOT.GSNOW_IDEAL) DEALLOCATE(ZGRID    )
DEALLOCATE(ZW       )
IF (LHOOK) CALL DR_HOOK('PREP_HOR_SNOW_FIELD',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
CONTAINS
!
!-------------------------------------------------------------------------------------
!
SUBROUTINE INIT_FROM_REF_GRID(PGRID1,PT1,PD2,PT2)
!
USE MODI_INTERP_GRID
!
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PT1    ! variable profile
REAL, DIMENSION(:),     INTENT(IN)  :: PGRID1 ! normalized grid
REAL, DIMENSION(:,:,:), INTENT(IN)  :: PD2    ! output layer thickness
REAL, DIMENSION(:,:,:), INTENT(OUT) :: PT2    ! variable profile
!
INTEGER                                  :: JL  ! loop counter
REAL, DIMENSION(SIZE(PT1,1),SIZE(PT1,2)) :: ZD1 ! input grid
REAL, DIMENSION(SIZE(PD2,1),SIZE(PD2,2)) :: ZD2 ! output grid
REAL, DIMENSION(SIZE(PD2,1))             :: ZDT  ! output total thickness
INTEGER                       :: JPATCH    ! loop on patches
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
IF (LHOOK) CALL DR_HOOK('INIT_FROM_REF_GRID',0,ZHOOK_HANDLE)
DO JPATCH=1,KPATCH
  ZD2(:,:) = 0.
  ZDT (:)   = 0.
  !
  DO JL=1,SIZE(ZD2,2)
    ZD2(:,JL) = ZDT(:) + PD2(:,JL,JPATCH)/2.
    ZDT (:)    = ZDT(:) + PD2(:,JL,JPATCH)
  END DO
  !
  DO JL=1,SIZE(PT1,2)
    ZD1(:,JL) = PGRID1(JL) * ZDT(:)
  END DO
  !
  CALL INTERP_GRID(ZD1,PT1(:,:,JPATCH),ZD2,PT2(:,:,JPATCH))
END DO
IF (LHOOK) CALL DR_HOOK('INIT_FROM_REF_GRID',1,ZHOOK_HANDLE)
!
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
END SUBROUTINE INIT_FROM_REF_GRID
!-------------------------------------------------------------------------------------
!
END SUBROUTINE PREP_HOR_SNOW_FIELD
