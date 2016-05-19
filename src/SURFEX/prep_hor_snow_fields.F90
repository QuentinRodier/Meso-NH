!     #########
SUBROUTINE PREP_HOR_SNOW_FIELDS(HPROGRAM,HSURF,              &
                                HFILE,HFILETYPE,             &
                                HFILEPGD,HFILEPGDTYPE,       &
                                KLUOUT,OUNIF,KPATCH,         &
                                KL,TPSNOW, TPTIME,           &
                                PUNIF_WSNOW, PUNIF_RSNOW,    &
                                PUNIF_TSNOW, PUNIF_ASNOW,    &
                                OSNOW_IDEAL,                 &
                                PUNIF_SG1SNOW, PUNIF_SG2SNOW,&
                                PUNIF_HISTSNOW,PUNIF_AGESNOW,&
                                PVEGTYPE_PATCH, PPATCH       )  
!     #######################################################
!
!
!!****  *PREP_HOR_SNOW_FIELDS* - prepares all snow fields for one surface scheme.
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
!!------------------------------------------------------------------
!
USE MODD_TYPE_SNOW
USE MODD_TYPE_DATE_SURF, ONLY : DATE_TIME
!
USE MODD_SURF_PAR,       ONLY : XUNDEF
!
USE MODI_ALLOCATE_GR_SNOW
USE MODI_PREP_HOR_SNOW_FIELD
USE MODE_SNOW3L
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
 CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
 CHARACTER(LEN=7),   INTENT(IN)  :: HSURF     ! type of field
 CHARACTER(LEN=28),  INTENT(IN)  :: HFILE     ! file name
 CHARACTER(LEN=6),   INTENT(IN)  :: HFILETYPE ! file type
 CHARACTER(LEN=28),  INTENT(IN)  :: HFILEPGD     ! file name
 CHARACTER(LEN=6),   INTENT(IN)  :: HFILEPGDTYPE ! file type
INTEGER,            INTENT(IN)  :: KLUOUT    ! logical unit of output listing
LOGICAL,            INTENT(IN)  :: OUNIF     ! flag for prescribed uniform field
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

REAL,DIMENSION(:,:,:),  INTENT(IN), OPTIONAL :: PVEGTYPE_PATCH ! fraction of each patch
REAL,DIMENSION(:,:),INTENT(IN), OPTIONAL  :: PPATCH ! fraction of each patch
!
!
!*      0.2    declarations of local variables
!
 CHARACTER(LEN=10)                   :: YSNSURF   ! type of field
REAL,ALLOCATABLE,DIMENSION(:,:,:)   :: ZW        ! total snow content
REAL,ALLOCATABLE,DIMENSION(:,:)     :: ZWRHO     ! total snow content from rho profile alone
REAL,ALLOCATABLE,DIMENSION(:,:,:)   :: ZD        ! total snow depth
REAL,ALLOCATABLE,DIMENSION(:,:,:)   :: ZDEPTH    ! snow depth of each layer
REAL,DIMENSION(KL,KPATCH)           :: ZPATCH    ! fraction of each patch
REAL,DIMENSION(:,:,:), ALLOCATABLE  :: ZVEGTYPE_PATCH    ! fraction of each patch
!
INTEGER                             :: JPATCH    ! loop counter on patches
INTEGER                             :: JLAYER    ! loop counter on layers
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!---------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('PREP_HOR_SNOW_FIELDS',0,ZHOOK_HANDLE)
IF (PRESENT(PPATCH)) THEN
   ZPATCH = PPATCH
ELSE
   ZPATCH = 1.
ENDIF
IF (PRESENT(PVEGTYPE_PATCH)) THEN
  ALLOCATE(ZVEGTYPE_PATCH(KL,SIZE(PVEGTYPE_PATCH,2),KPATCH))
  ZVEGTYPE_PATCH = PVEGTYPE_PATCH
ELSE
  ALLOCATE(ZVEGTYPE_PATCH(KL,1,KPATCH))
  ZVEGTYPE_PATCH = 1.
ENDIF
!
!*      1.     Allocation of output field
!
 CALL ALLOCATE_GR_SNOW(TPSNOW,KL,KPATCH)
!
!---------------------------------------------------------------------------
!
!*      3.     Treatment of total snow content (kg/m2)
!
ALLOCATE(ZW(KL,TPSNOW%NLAYER,KPATCH))
!
YSNSURF='WWW'//HSURF
 CALL PREP_HOR_SNOW_FIELD(HPROGRAM, HFILE, HFILETYPE, HFILEPGD, HFILEPGDTYPE, &
                         KLUOUT, OUNIF, YSNSURF, KPATCH, KL, TPSNOW, TPTIME, &
                         PUNIF_WSNOW, PUNIF_RSNOW, PUNIF_TSNOW, PUNIF_ASNOW, &
                         OSNOW_IDEAL, PUNIF_SG1SNOW,                         &
                         PUNIF_SG2SNOW, PUNIF_HISTSNOW,PUNIF_AGESNOW,        &                      
                         PF=ZW,PVEGTYPE_PATCH=ZVEGTYPE_PATCH,PPATCH=ZPATCH   )  
!
!----------------------------------------------------------------------------
!
!*      4.     Treatment of total snow depth
!
ALLOCATE(ZD(KL,TPSNOW%NLAYER,KPATCH))
!
YSNSURF='DEP'//HSURF
 CALL PREP_HOR_SNOW_FIELD(HPROGRAM, HFILE, HFILETYPE, HFILEPGD, HFILEPGDTYPE, &
                         KLUOUT, OUNIF, YSNSURF, KPATCH, KL, TPSNOW, TPTIME, &
                         PUNIF_WSNOW, PUNIF_RSNOW, PUNIF_TSNOW, PUNIF_ASNOW, &
                         OSNOW_IDEAL, PUNIF_SG1SNOW,                         &
                         PUNIF_SG2SNOW, PUNIF_HISTSNOW,PUNIF_AGESNOW,        &
                         PF=ZD,PVEGTYPE_PATCH=ZVEGTYPE_PATCH,PPATCH=ZPATCH   )
!
!* snow layer thickness definition
!
ALLOCATE(ZDEPTH(SIZE(TPSNOW%WSNOW,1),TPSNOW%NLAYER,KPATCH))
!
IF (OSNOW_IDEAL) THEN
  ZDEPTH(:,:,:) = ZD(:,:,:)
ELSE
  IF (TPSNOW%NLAYER==1) THEN
    DO JPATCH=1,KPATCH
      ZDEPTH(:,1,JPATCH) = ZD(:,1,JPATCH)
    END DO
  ELSEIF (TPSNOW%SCHEME=='3-L' .OR. TPSNOW%SCHEME=='CRO') THEN
    DO JPATCH=1,KPATCH
      CALL SNOW3LGRID(ZDEPTH(:,:,JPATCH),ZD(:,1,JPATCH))
    END DO
  ENDIF
ENDIF
!
!----------------------------------------------------------------------------
!
!*      4.     Snow density profile
!              --------------------
!
!* density profile
YSNSURF='RHO'//HSURF
 CALL PREP_HOR_SNOW_FIELD(HPROGRAM,HFILE,HFILETYPE,HFILEPGD,HFILEPGDTYPE,     &
                         KLUOUT,OUNIF,YSNSURF, KPATCH, KL, TPSNOW, TPTIME,   &
                         PUNIF_WSNOW, PUNIF_RSNOW, PUNIF_TSNOW, PUNIF_ASNOW, &
                         OSNOW_IDEAL, PUNIF_SG1SNOW,                         &
                         PUNIF_SG2SNOW, PUNIF_HISTSNOW,PUNIF_AGESNOW,        &
                         PDEPTH=ZDEPTH,PVEGTYPE_PATCH=ZVEGTYPE_PATCH,PPATCH=ZPATCH )  
!
!----------------------------------------------------------------------------
!
!*      5.     Snow water content profile
!              --------------------------

IF (OSNOW_IDEAL) THEN
  !
  TPSNOW%WSNOW(:,:,:) = ZW(:,:,:)
  !
ELSE
  !
  ALLOCATE(ZWRHO(SIZE(TPSNOW%WSNOW,1),KPATCH))
  ZWRHO(:,:) = 0.
  !
  !* snow depth estimated from rho profile
  DO JPATCH=1,KPATCH
    DO JLAYER=1,TPSNOW%NLAYER
      WHERE (ZPATCH(:,JPATCH)>0. .AND. TPSNOW%RHO(:,JLAYER,JPATCH)/=XUNDEF)
        ZWRHO(:,JPATCH) = ZWRHO(:,JPATCH) + TPSNOW%RHO(:,JLAYER,JPATCH) * ZDEPTH(:,JLAYER,JPATCH)
      ELSEWHERE
        ZWRHO(:,JPATCH) = XUNDEF
      END WHERE
    END DO
  END DO
  !
  !* modification of rho: coherence between rho profile, total snow and total depth
  DO JPATCH=1,KPATCH
    DO JLAYER=1,TPSNOW%NLAYER
      WHERE(ZPATCH(:,JPATCH)>0. .AND. ZWRHO(:,JPATCH)/=0. .AND. ZWRHO(:,JPATCH)/=XUNDEF)
        TPSNOW%RHO(:,JLAYER,JPATCH) = TPSNOW%RHO(:,JLAYER,JPATCH) * ZW(:,1,JPATCH) / ZWRHO(:,JPATCH)
      ELSEWHERE
        TPSNOW%RHO(:,JLAYER,JPATCH) = XUNDEF
      END WHERE
    END DO
  END DO
  !
  !* snow content profile for each grid level
  DO JPATCH=1,KPATCH
    DO JLAYER=1,TPSNOW%NLAYER
      WHERE(ZPATCH(:,JPATCH)>0.)
        TPSNOW%WSNOW(:,JLAYER,JPATCH) = TPSNOW%RHO(:,JLAYER,JPATCH) * ZDEPTH(:,JLAYER,JPATCH)
      ELSEWHERE
        TPSNOW%WSNOW(:,JLAYER,JPATCH) = XUNDEF
      END WHERE
    END DO
  END DO
  !
  DEALLOCATE(ZWRHO)
  !
ENDIF
!
!----------------------------------------------------------------------------
!
!*      6.     Albedo and snow heat content
!              ----------------------------
!
!* albedo
YSNSURF='ALB'//HSURF
 CALL PREP_HOR_SNOW_FIELD(HPROGRAM,HFILE,HFILETYPE,HFILEPGD,HFILEPGDTYPE,     &
                         KLUOUT,OUNIF,YSNSURF, KPATCH, KL, TPSNOW, TPTIME,   &
                         PUNIF_WSNOW, PUNIF_RSNOW, PUNIF_TSNOW, PUNIF_ASNOW, &
                         OSNOW_IDEAL, PUNIF_SG1SNOW,                         &
                         PUNIF_SG2SNOW, PUNIF_HISTSNOW,PUNIF_AGESNOW,        &
                         PDEPTH=ZDEPTH,PVEGTYPE_PATCH=ZVEGTYPE_PATCH,PPATCH=ZPATCH) 
!
!* heat in snowpack profile
YSNSURF='HEA'//HSURF
 CALL PREP_HOR_SNOW_FIELD(HPROGRAM,HFILE,HFILETYPE,HFILEPGD,HFILEPGDTYPE,     &
                         KLUOUT,OUNIF,YSNSURF, KPATCH, KL, TPSNOW, TPTIME,   &
                         PUNIF_WSNOW, PUNIF_RSNOW, PUNIF_TSNOW, PUNIF_ASNOW, &
                         OSNOW_IDEAL, PUNIF_SG1SNOW,                         &
                         PUNIF_SG2SNOW, PUNIF_HISTSNOW,PUNIF_AGESNOW,        &
                         PDEPTH=ZDEPTH,PVEGTYPE_PATCH=ZVEGTYPE_PATCH,PPATCH=ZPATCH)  
!
!----------------------------------------------------------------------------
!
!*      7.     Crocus specific parameters
!              --------------------------
!
IF (TPSNOW%SCHEME=='CRO') THEN
  !
  YSNSURF='SG1'//HSURF
  CALL PREP_HOR_SNOW_FIELD(HPROGRAM,HFILE,HFILETYPE,HFILEPGD,HFILEPGDTYPE,   &
                         KLUOUT,OUNIF,YSNSURF, KPATCH, KL, TPSNOW, TPTIME,   &
                         PUNIF_WSNOW, PUNIF_RSNOW, PUNIF_TSNOW, PUNIF_ASNOW, &
                         OSNOW_IDEAL, PUNIF_SG1SNOW,                         &
                         PUNIF_SG2SNOW, PUNIF_HISTSNOW,PUNIF_AGESNOW,        &
                         PDEPTH=ZDEPTH,PVEGTYPE_PATCH=ZVEGTYPE_PATCH,PPATCH=ZPATCH)   
  !
  YSNSURF='SG2'//HSURF
  CALL PREP_HOR_SNOW_FIELD(HPROGRAM,HFILE,HFILETYPE,HFILEPGD,HFILEPGDTYPE,   &
                         KLUOUT,OUNIF,YSNSURF, KPATCH, KL, TPSNOW, TPTIME,   &
                         PUNIF_WSNOW, PUNIF_RSNOW, PUNIF_TSNOW, PUNIF_ASNOW, &
                         OSNOW_IDEAL, PUNIF_SG1SNOW,                         &
                         PUNIF_SG2SNOW, PUNIF_HISTSNOW,PUNIF_AGESNOW,        &
                         PDEPTH=ZDEPTH,PVEGTYPE_PATCH=ZVEGTYPE_PATCH,PPATCH=ZPATCH)   
  !
  YSNSURF='HIS'//HSURF
  CALL PREP_HOR_SNOW_FIELD(HPROGRAM,HFILE,HFILETYPE,HFILEPGD,HFILEPGDTYPE,   &
                         KLUOUT,OUNIF,YSNSURF, KPATCH, KL, TPSNOW, TPTIME,   &
                         PUNIF_WSNOW, PUNIF_RSNOW, PUNIF_TSNOW, PUNIF_ASNOW, &
                         OSNOW_IDEAL, PUNIF_SG1SNOW,                         &
                         PUNIF_SG2SNOW, PUNIF_HISTSNOW,PUNIF_AGESNOW,        &
                         PDEPTH=ZDEPTH,PVEGTYPE_PATCH=ZVEGTYPE_PATCH,PPATCH=ZPATCH)   
  !
  YSNSURF='AGE'//HSURF
  CALL PREP_HOR_SNOW_FIELD(HPROGRAM,HFILE,HFILETYPE,HFILEPGD,HFILEPGDTYPE,   &
                         KLUOUT,OUNIF,YSNSURF, KPATCH, KL, TPSNOW, TPTIME,   &
                         PUNIF_WSNOW, PUNIF_RSNOW, PUNIF_TSNOW, PUNIF_ASNOW, &
                         OSNOW_IDEAL, PUNIF_SG1SNOW,                         &
                         PUNIF_SG2SNOW, PUNIF_HISTSNOW,PUNIF_AGESNOW,        &
                         PDEPTH=ZDEPTH,PVEGTYPE_PATCH=ZVEGTYPE_PATCH,PPATCH=ZPATCH)   
  !  
ENDIF
!
!*      8.     Deallocations
!
DEALLOCATE(ZD      )
DEALLOCATE(ZW      )
DEALLOCATE(ZDEPTH  )
DEALLOCATE(ZVEGTYPE_PATCH)
IF (LHOOK) CALL DR_HOOK('PREP_HOR_SNOW_FIELDS',1,ZHOOK_HANDLE)
!
!----------------------------------------------------------------------------
!
END SUBROUTINE PREP_HOR_SNOW_FIELDS
