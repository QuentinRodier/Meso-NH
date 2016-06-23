!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE PREP_HOR_SNOW_FIELDS (DTCO, &
                                  IG, U,GCP, &
                                 HPROGRAM,HSURF,              &
                                HFILE,HFILETYPE,             &
                                HFILEPGD,HFILEPGDTYPE,       &
                                KLUOUT,OUNIF,KPATCH,         &
                                KTEB_PATCH, &
                                KL,TPSNOW, TPTIME,           &
                                PUNIF_WSNOW, PUNIF_RSNOW,    &
                                PUNIF_TSNOW, PUNIF_LWCSNOW,  &
                                PUNIF_ASNOW, OSNOW_IDEAL,    &
                                PUNIF_SG1SNOW, PUNIF_SG2SNOW,&
                                PUNIF_HISTSNOW,PUNIF_AGESNOW,&
                                PVEGTYPE, PVEGTYPE_PATCH,    &
                                PPATCH, OKEY                 )  
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
!!      B. Decharme 10/2013, Phasage Arpege-Climat
!!      B. Decharme 04/2014, Init permsnow
!!------------------------------------------------------------------
!
!
!
!
!
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
!
USE MODD_ISBA_GRID_n, ONLY : ISBA_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_GRID_CONF_PROJ, ONLY : GRID_CONF_PROJ_t
!
USE MODD_TYPE_SNOW
USE MODD_TYPE_DATE_SURF, ONLY : DATE_TIME
!
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODD_SNOW_PAR,       ONLY : XAGLAMIN, XAGLAMAX
!
USE MODD_DATA_COVER_PAR, ONLY : NVEGTYPE
!
USE MODI_ALLOCATE_GR_SNOW
USE MODI_PREP_HOR_SNOW_FIELD
USE MODE_SNOW3L
USE MODI_OPEN_AUX_IO_SURF
USE MODI_READ_SURF
USE MODI_CLOSE_AUX_IO_SURF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
!
TYPE(ISBA_GRID_t), INTENT(INOUT) :: IG
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(GRID_CONF_PROJ_t),INTENT(INOUT) :: GCP
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
INTEGER,            INTENT(IN) :: KTEB_PATCH
INTEGER,            INTENT(IN)  :: KL        ! number of points
TYPE(SURF_SNOW)                 :: TPSNOW    ! snow fields
TYPE(DATE_TIME),    INTENT(IN)  :: TPTIME    ! date and time
REAL, DIMENSION(:), INTENT(IN)  :: PUNIF_WSNOW ! prescribed snow content (kg/m2)
REAL, DIMENSION(:), INTENT(IN)  :: PUNIF_RSNOW ! prescribed density (kg/m3)
REAL, DIMENSION(:), INTENT(IN)  :: PUNIF_TSNOW ! prescribed temperature (K)
REAL, DIMENSION(:), INTENT(IN)  :: PUNIF_LWCSNOW ! prescribed snow liquid water content (kg/m3)
REAL,               INTENT(IN)  :: PUNIF_ASNOW ! prescribed albedo (-)
LOGICAL,            INTENT(IN)  :: OSNOW_IDEAL
REAL, DIMENSION(:), INTENT(IN)  :: PUNIF_SG1SNOW ! 
REAL, DIMENSION(:), INTENT(IN)  :: PUNIF_SG2SNOW ! 
REAL, DIMENSION(:), INTENT(IN)  :: PUNIF_HISTSNOW ! 
REAL, DIMENSION(:), INTENT(IN)  :: PUNIF_AGESNOW ! 

REAL,DIMENSION(:,:),    INTENT(IN ), OPTIONAL :: PVEGTYPE       ! fraction of each vegtype
REAL,DIMENSION(:,:,:),  INTENT(IN ), OPTIONAL :: PVEGTYPE_PATCH ! fraction of each vegtype per patch
REAL,DIMENSION(:,:),    INTENT(IN ), OPTIONAL :: PPATCH         ! fraction of each patch
LOGICAL,                INTENT(OUT), OPTIONAL :: OKEY
!
!
!*      0.2    declarations of local variables
!
 CHARACTER(LEN=10)                   :: YSNSURF   ! type of field
REAL,ALLOCATABLE,DIMENSION(:,:,:)   :: ZW        ! total snow content
REAL,ALLOCATABLE,DIMENSION(:,:)     :: ZWRHO     ! total snow content from rho profile alone
REAL,ALLOCATABLE,DIMENSION(:,:,:)   :: ZD        ! total snow depth
REAL,ALLOCATABLE,DIMENSION(:,:,:)   :: ZDEPTH    ! snow depth of each layer
REAL,ALLOCATABLE,DIMENSION(:,:)     :: ZDTOT     ! total snow depth
REAL,DIMENSION(KL,KPATCH)           :: ZPATCH    ! fraction of each patch
REAL,DIMENSION(:,:),   ALLOCATABLE  :: ZVEGTYPE          ! fraction of each patch
REAL,DIMENSION(:,:,:), ALLOCATABLE  :: ZVEGTYPE_PATCH    ! fraction of each vegtype per patch
!
INTEGER                             :: JPATCH    ! loop counter on patches
INTEGER                             :: JLAYER    ! loop counter on layers
INTEGER                             :: IVERSION  ! surface version
 CHARACTER(LEN=16)                   :: YRECFM    ! record name
INTEGER                             :: IRESP     ! error return code
LOGICAL                             :: GGLACIER
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!---------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('PREP_HOR_SNOW_FIELDS',0,ZHOOK_HANDLE)
!
IF (PRESENT(PPATCH)) THEN
   ZPATCH = PPATCH
ELSE
   ZPATCH = 1.
ENDIF
IF (PRESENT(PVEGTYPE)) THEN
  ALLOCATE(ZVEGTYPE(KL,SIZE(PVEGTYPE,2)))
  ZVEGTYPE = PVEGTYPE
ELSE
  ALLOCATE(ZVEGTYPE(KL,NVEGTYPE))
  ZVEGTYPE = 1.
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
!*      2.     Find if PERMSNOW must be done
!
IF(PRESENT(OKEY))THEN
!  
  IF ( (HFILETYPE=='MESONH' .OR. HFILETYPE=='ASCII ' .OR. HFILETYPE=='LFI   '.OR. HFILETYPE=='FA    ') &
       .AND. (HSURF=='SN_VEG ')  ) THEN
!       
     CALL OPEN_AUX_IO_SURF(&
                           HFILE,HFILETYPE,'FULL  ')
     YRECFM='VERSION'
     CALL READ_SURF(&
                    HFILETYPE,YRECFM,IVERSION,IRESP)    
     CALL CLOSE_AUX_IO_SURF(HFILE,HFILETYPE)
!  
     IF(IVERSION>7)THEN       
       CALL OPEN_AUX_IO_SURF(&
                           HFILE,HFILETYPE,'NATURE')
       YRECFM='GLACIER'
       CALL READ_SURF(&
                    HFILETYPE,YRECFM,GGLACIER,IRESP)    
       CALL CLOSE_AUX_IO_SURF(HFILE,HFILETYPE)  
       IF(GGLACIER)OKEY=.FALSE.
     ENDIF
!
  ENDIF
!  
  IF(OSNOW_IDEAL)OKEY=.FALSE.
!
ENDIF
!
!---------------------------------------------------------------------------
!
!*      3.     Treatment of total snow content (kg/m2)
!
ALLOCATE(ZW(KL,TPSNOW%NLAYER,KPATCH))
!
YSNSURF='WWW'//HSURF
 CALL PREP_HOR_SNOW_FIELD(DTCO, &
                         IG, U,GCP, &
                         HPROGRAM, HFILE, HFILETYPE, HFILEPGD, HFILEPGDTYPE,  &
                         KLUOUT, OUNIF, YSNSURF, KPATCH, KTEB_PATCH, KL, TPSNOW, TPTIME,  &
                         PUNIF_WSNOW, PUNIF_RSNOW, PUNIF_TSNOW, PUNIF_LWCSNOW,&
                         PUNIF_ASNOW, OSNOW_IDEAL, PUNIF_SG1SNOW,             &
                         PUNIF_SG2SNOW, PUNIF_HISTSNOW,PUNIF_AGESNOW,         &                      
                         PF=ZW, PVEGTYPE=ZVEGTYPE,                            &
                         PVEGTYPE_PATCH=ZVEGTYPE_PATCH, PPATCH=ZPATCH         )  
!
!----------------------------------------------------------------------------
!
!*      4.     Treatment of total snow depth
!
ALLOCATE(ZD(KL,TPSNOW%NLAYER,KPATCH))
!
YSNSURF='DEP'//HSURF
 CALL PREP_HOR_SNOW_FIELD(DTCO, &
                         IG, U,GCP, &
                         HPROGRAM, HFILE, HFILETYPE, HFILEPGD, HFILEPGDTYPE,  &
                         KLUOUT, OUNIF, YSNSURF, KPATCH, KTEB_PATCH, KL, TPSNOW, TPTIME,  &
                         PUNIF_WSNOW, PUNIF_RSNOW, PUNIF_TSNOW, PUNIF_LWCSNOW,&
                         PUNIF_ASNOW, OSNOW_IDEAL, PUNIF_SG1SNOW,             &
                         PUNIF_SG2SNOW, PUNIF_HISTSNOW,PUNIF_AGESNOW,         &
                         PF=ZD, PVEGTYPE=ZVEGTYPE,                            &
                         PVEGTYPE_PATCH=ZVEGTYPE_PATCH, PPATCH=ZPATCH         )
!
!* snow layer thickness definition
!
ALLOCATE(ZDEPTH(KL,TPSNOW%NLAYER,KPATCH))
!
IF (OSNOW_IDEAL) THEN
  ZDEPTH(:,:,:) = ZD(:,:,:)
ELSE
  IF (TPSNOW%NLAYER==1) THEN
    DO JPATCH=1,KPATCH
      ZDEPTH(:,1,JPATCH) = ZD(:,1,JPATCH)
    END DO
  ELSEIF (TPSNOW%SCHEME=='3-L') THEN
      ZDEPTH(:,:,:)=ZD(:,:,:)
  ELSEIF (TPSNOW%SCHEME=='CRO') THEN
    ALLOCATE(ZDTOT(KL,KPATCH))
    ZDTOT(:,:)=0.0
    DO JLAYER=1,TPSNOW%NLAYER
       ZDTOT(:,:)=ZDTOT(:,:)+ZD(:,JLAYER,:)
    END DO
    DO JPATCH=1,KPATCH
       CALL SNOW3LGRID(ZDEPTH(:,:,JPATCH),ZDTOT(:,JPATCH))
    END DO
    DEALLOCATE(ZDTOT)
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
 CALL PREP_HOR_SNOW_FIELD(DTCO, &
                         IG, U,GCP, &
                         HPROGRAM,HFILE,HFILETYPE,HFILEPGD,HFILEPGDTYPE,           &
                         KLUOUT,OUNIF,YSNSURF, KPATCH, KTEB_PATCH, KL, TPSNOW, TPTIME,         &
                         PUNIF_WSNOW, PUNIF_RSNOW, PUNIF_TSNOW, PUNIF_LWCSNOW,     &
                         PUNIF_ASNOW, OSNOW_IDEAL, PUNIF_SG1SNOW,                  &
                         PUNIF_SG2SNOW, PUNIF_HISTSNOW,PUNIF_AGESNOW,              &
                         PDEPTH=ZDEPTH, PVEGTYPE=ZVEGTYPE,                         &
                         PVEGTYPE_PATCH=ZVEGTYPE_PATCH, PPATCH=ZPATCH              )  
!
!----------------------------------------------------------------------------
!
!*      5.     Snow water content profile
!              --------------------------
!
IF (OSNOW_IDEAL) THEN
  !
  TPSNOW%WSNOW(:,:,:) = ZW(:,:,:)
  !
ELSE
  !
  ALLOCATE(ZWRHO(SIZE(TPSNOW%WSNOW,1),KPATCH))
  ALLOCATE(ZDTOT(SIZE(TPSNOW%WSNOW,1),KPATCH))
  ZWRHO(:,:) = 0.0
  ZDTOT(:,:) = 0.0
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
  !* modification of snow depth: coherence between rho profile, total snow and total depth
  DO JPATCH=1,KPATCH
    DO JLAYER=1,TPSNOW%NLAYER
      WHERE(ZPATCH(:,JPATCH)>0. .AND. ZWRHO(:,JPATCH)/=0. .AND. ZWRHO(:,JPATCH)/=XUNDEF .AND. ZW(:,1,JPATCH)>0.0)
        ZDTOT(:,JPATCH) = ZDTOT(:,JPATCH) + ZDEPTH(:,JLAYER,JPATCH) * ZW(:,1,JPATCH) / ZWRHO(:,JPATCH)
      ENDWHERE
    END DO
    CALL SNOW3LGRID(ZDEPTH(:,:,JPATCH),ZDTOT(:,JPATCH))
  END DO
  !
  !* snow content profile for each grid level
  DO JPATCH=1,KPATCH
    DO JLAYER=1,TPSNOW%NLAYER
      WHERE(ZPATCH(:,JPATCH)>0..AND.TPSNOW%RHO(:,JLAYER,JPATCH)/=XUNDEF.AND.ZDTOT(:,JPATCH)>0.)
        TPSNOW%WSNOW(:,JLAYER,JPATCH) = TPSNOW%RHO(:,JLAYER,JPATCH) * ZDEPTH(:,JLAYER,JPATCH)
      ELSEWHERE(ZPATCH(:,JPATCH)>0..AND.(TPSNOW%RHO(:,JLAYER,JPATCH)==XUNDEF.OR.ZDTOT(:,JPATCH)==0.0))
        TPSNOW%WSNOW(:,JLAYER,JPATCH) = 0.0
      ELSEWHERE
        TPSNOW%WSNOW(:,JLAYER,JPATCH) = XUNDEF
      END WHERE
    END DO
  END DO
  !
  DEALLOCATE(ZWRHO)
  DEALLOCATE(ZDTOT)
  !
ENDIF
!
!----------------------------------------------------------------------------
!
!*      6.     Albedo, snow heat content, and age
!              ----------------------------------
!
!* albedo
YSNSURF='ALB'//HSURF
 CALL PREP_HOR_SNOW_FIELD(DTCO, &
                         IG, U,GCP, &
                         HPROGRAM,HFILE,HFILETYPE,HFILEPGD,HFILEPGDTYPE,         &
                         KLUOUT,OUNIF,YSNSURF, KPATCH, KTEB_PATCH, KL, TPSNOW, TPTIME,        &
                         PUNIF_WSNOW, PUNIF_RSNOW, PUNIF_TSNOW, PUNIF_LWCSNOW,    &
                         PUNIF_ASNOW, OSNOW_IDEAL, PUNIF_SG1SNOW,                 &
                         PUNIF_SG2SNOW, PUNIF_HISTSNOW,PUNIF_AGESNOW,             &
                         PDEPTH=ZDEPTH, PVEGTYPE=ZVEGTYPE,                        &
                         PVEGTYPE_PATCH=ZVEGTYPE_PATCH, PPATCH=ZPATCH             ) 
!
IF (TPSNOW%SCHEME/='D95') THEN
  !
  !* heat in snowpack profile
  YSNSURF='HEA'//HSURF
  CALL PREP_HOR_SNOW_FIELD(DTCO, &
                         IG, U,GCP, &
                         HPROGRAM,HFILE,HFILETYPE,HFILEPGD,HFILEPGDTYPE,          &
                           KLUOUT,OUNIF,YSNSURF, KPATCH, KTEB_PATCH, KL, TPSNOW, TPTIME,        &
                           PUNIF_WSNOW, PUNIF_RSNOW, PUNIF_TSNOW, PUNIF_LWCSNOW,    &
                           PUNIF_ASNOW, OSNOW_IDEAL, PUNIF_SG1SNOW,                 &
                           PUNIF_SG2SNOW, PUNIF_HISTSNOW,PUNIF_AGESNOW,             &
                           PDEPTH=ZDEPTH, PVEGTYPE=ZVEGTYPE,                        &
                           PVEGTYPE_PATCH=ZVEGTYPE_PATCH, PPATCH=ZPATCH             )
  !
ENDIF
!
IF (TPSNOW%SCHEME=='CRO'.OR. TPSNOW%SCHEME=='3-L') THEN
  !
  !* age in snowpack profile
  YSNSURF='AGE'//HSURF
  CALL PREP_HOR_SNOW_FIELD(DTCO, &
                         IG, U,GCP, &
                         HPROGRAM,HFILE,HFILETYPE,HFILEPGD,HFILEPGDTYPE,        &
                         KLUOUT,OUNIF,YSNSURF, KPATCH, KTEB_PATCH, KL, TPSNOW, TPTIME,        &
                         PUNIF_WSNOW, PUNIF_RSNOW, PUNIF_TSNOW, PUNIF_LWCSNOW,    &
                         PUNIF_ASNOW, OSNOW_IDEAL, PUNIF_SG1SNOW,                 &
                         PUNIF_SG2SNOW, PUNIF_HISTSNOW,PUNIF_AGESNOW,             &
                         PDEPTH=ZDEPTH, PVEGTYPE=ZVEGTYPE,                        &
                         PVEGTYPE_PATCH=ZVEGTYPE_PATCH, PPATCH=ZPATCH             )   
  !
  WHERE(TPSNOW%WSNOW(:,1,:)>0.0.AND.TPSNOW%WSNOW(:,1,:)/=XUNDEF.AND. &
        TPSNOW%AGE(:,1,:)==0.0.AND.TPSNOW%ALB(:,:)<XAGLAMIN)
        TPSNOW%ALB(:,:)=(XAGLAMIN+XAGLAMAX)/2.0
  ENDWHERE
  !
ENDIF
!
!----------------------------------------------------------------------------
!
!*      7.     Crocus specific parameters
!              --------------------------
!
IF (TPSNOW%SCHEME=='CRO') THEN
  !
  YSNSURF='SG1'//HSURF
  CALL PREP_HOR_SNOW_FIELD(DTCO, &
                         IG, U,GCP, &
                         HPROGRAM,HFILE,HFILETYPE,HFILEPGD,HFILEPGDTYPE,        &
                         KLUOUT,OUNIF,YSNSURF, KPATCH, KTEB_PATCH, KL, TPSNOW, TPTIME,        &
                         PUNIF_WSNOW, PUNIF_RSNOW, PUNIF_TSNOW, PUNIF_LWCSNOW,    &
                         PUNIF_ASNOW, OSNOW_IDEAL, PUNIF_SG1SNOW,                 &
                         PUNIF_SG2SNOW, PUNIF_HISTSNOW,PUNIF_AGESNOW,             &
                         PDEPTH=ZDEPTH, PVEGTYPE=ZVEGTYPE,                        &
                         PVEGTYPE_PATCH=ZVEGTYPE_PATCH, PPATCH=ZPATCH             )   
  !
  YSNSURF='SG2'//HSURF
  CALL PREP_HOR_SNOW_FIELD(DTCO, &
                         IG, U,GCP, &
                         HPROGRAM,HFILE,HFILETYPE,HFILEPGD,HFILEPGDTYPE,        &
                         KLUOUT,OUNIF,YSNSURF, KPATCH, KTEB_PATCH, KL, TPSNOW, TPTIME,        &
                         PUNIF_WSNOW, PUNIF_RSNOW, PUNIF_TSNOW, PUNIF_LWCSNOW,    &
                         PUNIF_ASNOW, OSNOW_IDEAL, PUNIF_SG1SNOW,                 &
                         PUNIF_SG2SNOW, PUNIF_HISTSNOW,PUNIF_AGESNOW,             &
                         PDEPTH=ZDEPTH, PVEGTYPE=ZVEGTYPE,                        &
                         PVEGTYPE_PATCH=ZVEGTYPE_PATCH, PPATCH=ZPATCH             )   
  !
  YSNSURF='HIS'//HSURF
  CALL PREP_HOR_SNOW_FIELD(DTCO, &
                         IG, U,GCP, &
                         HPROGRAM,HFILE,HFILETYPE,HFILEPGD,HFILEPGDTYPE,        &
                         KLUOUT,OUNIF,YSNSURF, KPATCH, KTEB_PATCH, KL, TPSNOW, TPTIME,        &
                         PUNIF_WSNOW, PUNIF_RSNOW, PUNIF_TSNOW, PUNIF_LWCSNOW,    &
                         PUNIF_ASNOW, OSNOW_IDEAL, PUNIF_SG1SNOW,                 &
                         PUNIF_SG2SNOW, PUNIF_HISTSNOW,PUNIF_AGESNOW,             &
                         PDEPTH=ZDEPTH, PVEGTYPE=ZVEGTYPE,                        &
                         PVEGTYPE_PATCH=ZVEGTYPE_PATCH, PPATCH=ZPATCH             )   
  !
ENDIF
!
!*      8.     Deallocations
!
DEALLOCATE(ZD      )
DEALLOCATE(ZW      )
DEALLOCATE(ZDEPTH  )
DEALLOCATE(ZVEGTYPE)
DEALLOCATE(ZVEGTYPE_PATCH)
!
IF (LHOOK) CALL DR_HOOK('PREP_HOR_SNOW_FIELDS',1,ZHOOK_HANDLE)
!
!----------------------------------------------------------------------------
!
END SUBROUTINE PREP_HOR_SNOW_FIELDS
