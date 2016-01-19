!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
! Modifications :
! P.Marguinaud : 11-09-2012 : shorten field name
! G.Delautier : 24-06-2015 : bug for arome compressed files
! M.Moge    01/2016  using READ_SURF_FIELD2D/3D for 2D/3D surfex fields reads
!     #####################
MODULE MODE_READ_EXTERN
!     #####################
!-------------------------------------------------------------------
!
USE MODI_READ_LECOCLIMAP
!
USE MODI_PUT_ON_ALL_VEGTYPES
USE MODI_OLD_NAME
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
CONTAINS
!
!---------------------------------------------------------------------------------------
!
!     #######################
      SUBROUTINE READ_EXTERN_DEPTH(HPROGRAM,KLUOUT,HISBA,HNAT,HFIELD,KNI,KLAYER, &
                                   KPATCH,PSOILGRID,PDEPTH,KVERSION  )
!     #######################
!
USE MODD_SURF_PAR,       ONLY : NUNDEF, XUNDEF
USE MODD_DATA_COVER_PAR, ONLY : JPCOVER, NVEGTYPE
!
USE MODI_READ_SURF_ISBA_PAR_n
USE MODI_READ_SURF_FIELD3D
USE MODI_READ_SURF_FIELD2D
USE MODI_READ_SURF
USE MODI_CONVERT_COVER_ISBA
USE MODI_GARDEN_SOIL_DEPTH

!
IMPLICIT NONE
!
!* dummy arguments
!  ---------------
!
 CHARACTER(LEN=6),     INTENT(IN)    :: HPROGRAM  ! type of input file
INTEGER,              INTENT(IN)    :: KLUOUT    ! logical unit of output listing
 CHARACTER(LEN=3),     INTENT(IN)    :: HISBA     ! type of ISBA soil scheme
 CHARACTER(LEN=3),     INTENT(IN)    :: HNAT      ! type of surface (nature, gardens)
 CHARACTER(LEN=7),     INTENT(IN)    :: HFIELD    ! field name
INTEGER,              INTENT(IN)    :: KNI       ! number of points
INTEGER,           INTENT(INOUT)    :: KLAYER    ! number of layers
INTEGER,              INTENT(IN)    :: KPATCH    ! number of patch
INTEGER,              INTENT(IN)    :: KVERSION  ! surface version
REAL, DIMENSION(:),   INTENT(IN)    :: PSOILGRID
REAL, DIMENSION(:,:,:), POINTER     :: PDEPTH    ! middle depth of each layer
!
!* local variables
!  ---------------
!
 CHARACTER(LEN=12) :: YRECFM         ! Name of the article to be read
 CHARACTER(LEN=16) :: YRECFM2
 CHARACTER(LEN=100):: YCOMMENT       ! Comment string
INTEGER           :: IRESP          ! reading return code
INTEGER           :: ILAYER         ! number of soil layers
INTEGER           :: JLAYER         ! loop counter
INTEGER           :: JPATCH         ! loop counter
INTEGER           :: JJ
INTEGER           :: IVERSION
INTEGER           :: IBUGFIX
!
LOGICAL, DIMENSION(JPCOVER)          :: GCOVER ! flag to read the covers
REAL,    DIMENSION(:,:), ALLOCATABLE :: ZCOVER ! cover fractions
REAL,    DIMENSION(:,:), ALLOCATABLE :: ZGROUND_DEPTH ! cover fractions
INTEGER, DIMENSION(:,:), ALLOCATABLE :: IWG_LAYER
REAL,  DIMENSION(:,:,:), ALLOCATABLE :: ZD     ! depth of each inter-layer
REAL,  DIMENSION(:,:,:), ALLOCATABLE :: ZDG    ! depth of each inter-layer
REAL,  DIMENSION(:,:,:), ALLOCATABLE :: ZDEPTH ! middle of each layer for each patch
REAL,  DIMENSION(:,:),   ALLOCATABLE :: ZWORK  ! work array
REAL,  DIMENSION(KNI)                :: ZHVEG  ! high vegetation fraction
REAL,  DIMENSION(KNI)                :: ZLVEG  ! low  vegetation fraction
REAL,  DIMENSION(KNI)                :: ZNVEG  ! no   vegetation fraction
 CHARACTER(LEN=4)                     :: YHVEG  ! type of high vegetation
 CHARACTER(LEN=4)                     :: YLVEG  ! type of low  vegetation
 CHARACTER(LEN=4)                     :: YNVEG  ! type of no   vegetation
LOGICAL                              :: GECOCLIMAP ! T if ecoclimap is used
LOGICAL                              :: GPAR_GARDEN! T if garden data are used
LOGICAL                              :: GDATA_DG
LOGICAL                              :: GDATA_GROUND_DEPTH
INTEGER                              :: IHYDRO_LAYER
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!
!------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_EXTERN:READ_EXTERN_DEPTH',0,ZHOOK_HANDLE)
!
IF (HNAT=='NAT') THEN
  CALL READ_LECOCLIMAP(HPROGRAM,GECOCLIMAP)
ELSE
  CALL READ_SURF(HPROGRAM,'PAR_GARDEN',GPAR_GARDEN,IRESP)
  GECOCLIMAP = .NOT. GPAR_GARDEN
END IF
!
!
YRECFM='VERSION'
 CALL READ_SURF(HPROGRAM,YRECFM,IVERSION,IRESP)
!
YRECFM='BUG'
 CALL READ_SURF(HPROGRAM,YRECFM,IBUGFIX,IRESP)
!
!------------------------------------------------------------------------------
!
ALLOCATE(ZDG   (KNI,KLAYER,KPATCH))
ALLOCATE(IWG_LAYER   (KNI,KPATCH))
IWG_LAYER(:,:) = NUNDEF
IHYDRO_LAYER = KLAYER
!
IF (GECOCLIMAP) THEN

 IF (IVERSION<7 .OR. IVERSION==7 .AND. IBUGFIX<=3) THEN
  !
  !* reading of the cover to obtain the depth of inter-layers
  !
  CALL OLD_NAME(HPROGRAM,'COVER_LIST      ',YRECFM)
  CALL READ_SURF(HPROGRAM,YRECFM,GCOVER(:),IRESP,HDIR='-')
  !
  ALLOCATE(ZCOVER(KNI,JPCOVER))
  YRECFM='COVER'
  CALL READ_SURF(HPROGRAM,YRECFM,ZCOVER(:,:),GCOVER(:),IRESP,HDIR='A')  
  !
  !* computes soil layers
  !  
  CALL CONVERT_COVER_ISBA(HISBA,NUNDEF,ZCOVER,'   ',HNAT,PSOILGRID=PSOILGRID,PDG=ZDG,KWG_LAYER=IWG_LAYER)
  !
  DEALLOCATE(ZCOVER)
 ELSE
print*, '-----------------------------------------------'
print*, '-----------------------------------------------'
print*, '-----------------------------------------------'
print*, '-----------------------------------------------'
print*, 'MODE_READ_EXTERN : ==> ON NE LIT PAS LES COVERS'
print*, '-----------------------------------------------'
print*, '-----------------------------------------------'
print*, '-----------------------------------------------'
print*, '-----------------------------------------------'
  IF (HNAT=='NAT') THEN
    YRECFM='ECO_DG'
  ELSE
    YRECFM='GD_ECO_DG'
  END IF
  CALL READ_SURF_FIELD3D(HPROGRAM,ZDG,1,SIZE(ZDG,2),YRECFM,HDIR='A')
  !
  IF (HISBA=='DIF') THEN
    YRECFM='ECO_WG_L'
    IF (HNAT=='GRD') YRECFM='GD_ECO_WG_L'
    ALLOCATE(ZWORK(KNI,KPATCH)) 
    CALL READ_SURF_FIELD2D(HPROGRAM,ZWORK(:,:),YRECFM,HDIR='A')
    WHERE (ZWORK==XUNDEF) ZWORK=NUNDEF
    IWG_LAYER=NINT(ZWORK)
    DEALLOCATE(ZWORK)
  END IF
 END IF
  !
  IF (HISBA=='DIF') IHYDRO_LAYER = MAXVAL(IWG_LAYER(:,:),IWG_LAYER(:,:)/=NUNDEF)
ENDIF

!-------------------------------------------------------------------
IF (HNAT=='NAT' .AND. (IVERSION>=7 .OR. .NOT.GECOCLIMAP)) THEN
  !
  !* directly read soil layers in the file for nature ISBA soil layers
  !
  GDATA_DG = .TRUE.
  IF (IVERSION>=7) THEN
    YRECFM='L_DG'
    YCOMMENT=YRECFM
    CALL READ_SURF(HPROGRAM,YRECFM,GDATA_DG,IRESP,HCOMMENT=YCOMMENT)
  ENDIF
  !
  IF (GDATA_DG) THEN
    !
    ALLOCATE(ZWORK(KNI,KPATCH))
    DO JLAYER=1,KLAYER
      IF (JLAYER<10)  WRITE(YRECFM,FMT='(A4,I1.1)') 'D_DG',JLAYER
      IF (JLAYER>=10) WRITE(YRECFM,FMT='(A4,I2.2)') 'D_DG',JLAYER
      CALL READ_SURF_ISBA_PAR_n(HPROGRAM,YRECFM,KLUOUT,KNI,ZWORK,IRESP,IVERSION,HDIR='A')
      DO JPATCH=1,KPATCH
        ZDG(:,JLAYER,JPATCH) = ZWORK(:,JPATCH)
      END DO
    END DO
    DEALLOCATE(ZWORK)
    !
  ENDIF
  !
    GDATA_GROUND_DEPTH=.FALSE.
  IF (IVERSION>7 .OR. IVERSION==7 .AND. IBUGFIX>=2) THEN
    !
    YRECFM2='L_GROUND_DEPTH'
    IF (IVERSION>7 .OR. IVERSION==7 .AND. IBUGFIX>=3) YRECFM2='L_GROUND_DPT'
    YCOMMENT=YRECFM2
    CALL READ_SURF(HPROGRAM,YRECFM2,GDATA_GROUND_DEPTH,IRESP,HCOMMENT=YCOMMENT)
    !
    IF (GDATA_GROUND_DEPTH) THEN
      !
      YRECFM2='D_GROUND_DETPH'
      IF (IVERSION>7 .OR. IVERSION==7 .AND. IBUGFIX>=3) YRECFM2='D_GROUND_DPT'
      ALLOCATE(ZGROUND_DEPTH(KNI,KPATCH))
      CALL READ_SURF_ISBA_PAR_n(HPROGRAM,YRECFM2,KLUOUT,KNI,ZGROUND_DEPTH(:,:),IRESP,IVERSION,HDIR='A')
      !
      DO JPATCH=1,KPATCH
        DO JJ=1,KNI
          DO JLAYER=1,KLAYER
            IF ( ZDG(JJ,JLAYER,JPATCH) <= ZGROUND_DEPTH(JJ,JPATCH) .AND. ZGROUND_DEPTH(JJ,JPATCH) < XUNDEF ) &
                IWG_LAYER(JJ,JPATCH) = JLAYER
          ENDDO
        ENDDO
      ENDDO
      DEALLOCATE(ZGROUND_DEPTH)
      !
      IF (HISBA=='DIF') IHYDRO_LAYER = MAXVAL(IWG_LAYER(:,:),IWG_LAYER(:,:)/=NUNDEF)
      !
    ENDIF
    !
  ENDIF
  !
ELSE IF (HNAT=='GRD' .AND. .NOT.GECOCLIMAP) THEN
  !
  !* computes soil layers from vegetation fractions read in the file
  !
  CALL READ_SURF(HPROGRAM,'D_TYPE_HVEG',YHVEG,IRESP)
  CALL READ_SURF(HPROGRAM,'D_TYPE_LVEG',YLVEG,IRESP)
  CALL READ_SURF(HPROGRAM,'D_TYPE_NVEG',YNVEG,IRESP)
  CALL READ_SURF(HPROGRAM,'D_FRAC_HVEG',ZHVEG,IRESP,HDIR='A')
  CALL READ_SURF(HPROGRAM,'D_FRAC_LVEG',ZLVEG,IRESP,HDIR='A')
  CALL READ_SURF(HPROGRAM,'D_FRAC_NVEG',ZNVEG,IRESP,HDIR='A')
  ! Ground layers
  CALL GARDEN_SOIL_DEPTH(YNVEG,YLVEG,YHVEG,ZNVEG,ZLVEG,ZHVEG,ZDG)
  !
END IF
!
DEALLOCATE(IWG_LAYER)
!
IF (HFIELD=='WG    ' .OR. HFIELD=='WGI   ' .OR. HFIELD=='TWN_WG  ' .OR. HFIELD=='TWN_WGI ' .OR. &
      HFIELD=='GD_WG  ' .OR. HFIELD=='GD_WGI ') THEN
  KLAYER = IHYDRO_LAYER
ENDIF
!
!-------------------------------------------------------------------
!
!* In force-restore ISBA, adds a layer at bottom of surface layer and a layer
!  between root and deep layers.
!
IF (HISBA=='2-L' .OR. HISBA=='3-L') THEN
  ILAYER = KLAYER + 1
  IF (HISBA=='3-L') ILAYER = ILAYER + 1
  ALLOCATE(ZD    (KNI,ILAYER,KPATCH))
  DO JPATCH=1,KPATCH
    ! for interpolations, middle of surface layer must be at least at 1cm
    ZD(:,1,JPATCH) = MIN(3.*ZDG(:,1,JPATCH),MAX(ZDG(:,1,JPATCH),0.02))
    ! new layer below surface layer. This layer will be at root depth layer humidity
    ZD(:,2,JPATCH) = MIN(4.*ZDG(:,1,JPATCH),0.5*(ZDG(:,1,JPATCH)+ZDG(:,2,JPATCH)))
    ! root layer
    ZD(:,3,JPATCH) = ZDG(:,2,JPATCH)
    IF (HISBA=='3-L') THEN
      ! between root and deep layers. This layer will have deep soil humidity.
      WHERE (ZDG(:,2,JPATCH)<ZDG(:,3,JPATCH))
        ZD(:,4,JPATCH) = 0.75 * ZDG(:,2,JPATCH) + 0.25 * ZDG(:,3,JPATCH)
      ELSEWHERE
        ZD(:,4,JPATCH) = ZDG(:,3,JPATCH)
      END WHERE
      ! deep layer
      ZD(:,5,JPATCH) = ZDG(:,3,JPATCH)
    END IF
  END DO
ELSE
  ILAYER = KLAYER
  ALLOCATE(ZD    (KNI,ILAYER,KPATCH))
  ZD(:,:,:) = ZDG(:,1:KLAYER,:)
END IF
!
DEALLOCATE(ZDG)
!
!-------------------------------------------------------------------
!* recovers middle layer depth (from the surface)
ALLOCATE(ZDEPTH    (KNI,ILAYER,KPATCH))
ZDEPTH = XUNDEF
DO JPATCH=1,KPATCH
  WHERE(ZD(:,1,JPATCH)/=XUNDEF) &
    ZDEPTH    (:,1,JPATCH)=ZD(:,1,JPATCH)/2.  
  DO JLAYER=2,ILAYER
    WHERE(ZD(:,1,JPATCH)/=XUNDEF) &
      ZDEPTH    (:,JLAYER,JPATCH) = (ZD(:,JLAYER-1,JPATCH) + ZD(:,JLAYER,JPATCH))/2.  
  END DO
END DO
DEALLOCATE(ZD)
!-------------------------------------------------------------------
!
ALLOCATE(PDEPTH    (KNI,ILAYER,NVEGTYPE))
 CALL PUT_ON_ALL_VEGTYPES(KNI,ILAYER,KPATCH,NVEGTYPE,ZDEPTH,PDEPTH)
DEALLOCATE(ZDEPTH)

IF (LHOOK) CALL DR_HOOK('MODE_READ_EXTERN:READ_EXTERN_DEPTH',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------
!
END SUBROUTINE READ_EXTERN_DEPTH
!
!
!-------------------------------------------------------------------
!---------------------------------------------------------------------------------------
!
!     #######################
      SUBROUTINE READ_EXTERN_ISBA(HFILE,HFILETYPE,HFILEPGD,HFILEPGDTYPE,&
                                  KLUOUT,KNI,HFIELD,HNAME,PFIELD,PDEPTH)
!     #######################
!
USE MODD_DATA_COVER_PAR, ONLY : NVEGTYPE
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODD_ISBA_PAR,    ONLY : XOPTIMGRID
!
USE MODI_OPEN_AUX_IO_SURF
USE MODI_CLOSE_AUX_IO_SURF
USE MODI_READ_SURF
USE MODI_READ_SURF_FIELD3D
USE MODE_SOIL
!
IMPLICIT NONE
!
!* dummy arguments
!  ---------------
!
 CHARACTER(LEN=28),  INTENT(IN)  :: HFILE     ! name of file
 CHARACTER(LEN=6),   INTENT(IN)  :: HFILETYPE ! type of input file
 CHARACTER(LEN=28),  INTENT(IN)  :: HFILEPGD     ! name of file
 CHARACTER(LEN=6),   INTENT(IN)  :: HFILEPGDTYPE ! type of input file
INTEGER,              INTENT(IN)    :: KLUOUT    ! logical unit of output listing
INTEGER,              INTENT(IN)    :: KNI       ! number of points
 CHARACTER(LEN=7),     INTENT(IN)    :: HFIELD    ! field name
 CHARACTER(LEN=*),     INTENT(IN)    :: HNAME     ! field name in the file
REAL, DIMENSION(:,:,:), POINTER       :: PFIELD    ! field to initialize
REAL, DIMENSION(:,:,:), POINTER       :: PDEPTH    ! middle depth of each layer
!
!
!* local variables
!  ---------------
!
 CHARACTER(LEN=12) :: YRECFM         ! Name of the article to be read
 CHARACTER(LEN=4)  :: YLVL
#ifdef MNH_PARALLEL
 CHARACTER(LEN=8)  :: YPATCH
#endif
 CHARACTER(LEN=3)  :: YISBA          ! type of ISBA soil scheme
 CHARACTER(LEN=3)  :: YNAT           ! type of surface (nature, garden)
 CHARACTER(LEN=4)  :: YPEDOTF        ! type of pedo-transfert function
INTEGER           :: IRESP          ! reading return code
INTEGER           :: ILAYER         ! number of layers
INTEGER           :: JLAYER         ! loop counter
INTEGER           :: IPATCH         ! number of patch
INTEGER           :: JPATCH         ! loop counter
INTEGER           :: JVEGTYPE       ! loop counter
LOGICAL           :: GTEB           ! TEB field
!
REAL,  DIMENSION(:,:,:), ALLOCATABLE :: ZFIELD ! field read, one level, all patches
REAL,  DIMENSION(:,:),   ALLOCATABLE :: ZWORK  ! field read, one level, all patches
!
REAL,  DIMENSION(:,:,:), ALLOCATABLE :: ZVAR      ! profile of physical variable
REAL,  DIMENSION(:),   ALLOCATABLE   :: ZCLAY     ! clay fraction
REAL,  DIMENSION(:),   ALLOCATABLE   :: ZSAND     ! sand fraction
REAL,  DIMENSION(:),   ALLOCATABLE   :: ZWWILT    ! wilting point
REAL,  DIMENSION(:),   ALLOCATABLE   :: ZWFC      ! field capacity
REAL,  DIMENSION(:),   ALLOCATABLE   :: ZWSAT     ! saturation
REAL,  DIMENSION(:),   ALLOCATABLE   :: ZSOILGRID
REAL,  DIMENSION(:),   ALLOCATABLE   :: ZNAT      ! natural surface fraction 
!
INTEGER :: IVERSION   ! surface version
INTEGER :: IBUGFIX
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('MODE_READ_EXTERN:READ_EXTERN_ISBA',0,ZHOOK_HANDLE)
WRITE  (KLUOUT,*) ' | Reading ',HFIELD,' in externalized file'
!
GTEB = (HNAME(1:3)=='TWN' .OR. HNAME(1:3)=='GD_' .OR. HNAME(1:3)=='GR_' &
        .OR. HNAME(4:6)=='GD_' .OR. HNAME(4:6)=='GR_')
!
!------------------------------------------------------------------------------
!
IF (GTEB) THEN
  CALL OPEN_AUX_IO_SURF(HFILEPGD,HFILEPGDTYPE,'TOWN  ')
ELSE
  CALL OPEN_AUX_IO_SURF(HFILEPGD,HFILEPGDTYPE,'NATURE')
ENDIF
!
YRECFM='VERSION'
 CALL READ_SURF(HFILEPGDTYPE,YRECFM,IVERSION,IRESP)
!
YRECFM='BUG'
 CALL READ_SURF(HFILEPGDTYPE,YRECFM,IBUGFIX,IRESP)
!
!* Read number of soil layers
!
YRECFM='GROUND_LAYER'
IF (GTEB) THEN 
  YRECFM='TWN_LAYER'
  IF (IVERSION>7 .OR. IVERSION==7 .AND. IBUGFIX>=3) YRECFM='GD_LAYER'
ENDIF
 CALL READ_SURF(HFILEPGDTYPE,YRECFM,ILAYER,IRESP)
!
!* number of tiles
!
IPATCH=1
IF (.NOT. GTEB) THEN
  YRECFM='PATCH_NUMBER'
  CALL READ_SURF(HFILEPGDTYPE,YRECFM,IPATCH,IRESP)
END IF
!
!* soil scheme
!
YRECFM='ISBA'
IF (GTEB) THEN 
  YRECFM='TWN_ISBA'
  IF (IVERSION>7 .OR. IVERSION==7 .AND. IBUGFIX>=3) YRECFM='GD_ISBA'
ENDIF
 CALL READ_SURF(HFILEPGDTYPE,YRECFM,YISBA,IRESP)
!
IF (IVERSION>=7) THEN
  !
  !* Pedo-transfert function
  !
  YRECFM='PEDOTF'
  IF (GTEB) THEN 
    YRECFM='TWN_PEDOTF'
    IF (IVERSION>7 .OR. IVERSION==7 .AND. IBUGFIX>=3) YRECFM='GD_PEDOTF'
  ENDIF
  CALL READ_SURF(HFILEPGDTYPE,YRECFM,YPEDOTF,IRESP)
  !
ELSE
  YPEDOTF = 'CH78'
ENDIF
!
!Only Brook and Corey with Force-Restore scheme
IF(YISBA/='DIF')THEN
  YPEDOTF='CH78'
ENDIF
!
!-------------------------------------------------------------------------------
!
! *.  Read clay fraction
!     ------------------
!
ALLOCATE(ZCLAY(KNI))
YRECFM='CLAY'
IF (GTEB) THEN 
  YRECFM='TWN_CLAY'
  IF (IVERSION>7 .OR. IVERSION==7 .AND. IBUGFIX>=3) YRECFM='GD_CLAY'
ENDIF
 CALL READ_SURF(HFILEPGDTYPE,YRECFM,ZCLAY(:),IRESP,HDIR='A')
!
!-------------------------------------------------------------------------------
!
! *.  Read sand fraction
!     ------------------
!
ALLOCATE(ZSAND(KNI))
YRECFM='SAND'
IF (GTEB) THEN 
  YRECFM='TWN_SAND'
  IF (IVERSION>7 .OR. IVERSION==7 .AND. IBUGFIX>=3) YRECFM='GD_SAND'
ENDIF
 CALL READ_SURF(HFILEPGDTYPE,YRECFM,ZSAND(:),IRESP,HDIR='A')
!
!-------------------------------------------------------------------------------
!
! *.  Read soil grid
!     --------------
!
!* Reference grid for DIF
!
IF(YISBA=='DIF') THEN
  ALLOCATE(ZSOILGRID(ILAYER))
  ZSOILGRID=XUNDEF
  IF (IVERSION>7 .OR. IVERSION==7 .AND. IBUGFIX>=2) THEN
    YRECFM='SOILGRID'
    IF (GTEB) THEN 
      YRECFM='TWN_SOILGRID'
      IF (IVERSION>7 .OR. IVERSION==7 .AND. IBUGFIX>=3) YRECFM='GD_SOILGRID'
    ENDIF
    CALL READ_SURF(HFILEPGDTYPE,YRECFM,ZSOILGRID,IRESP,HDIR='-')
  ELSE
    ZSOILGRID(1:ILAYER) = XOPTIMGRID(1:ILAYER)
  ENDIF
ELSE
  ALLOCATE(ZSOILGRID(0))
ENDIF
!
IF ((HFIELD=='TG    ') .AND. (YISBA=='2-L' .OR. YISBA=='3-L')) THEN
  ALLOCATE(PDEPTH    (KNI,ILAYER,NVEGTYPE))
  DO JVEGTYPE=1,NVEGTYPE
    PDEPTH(:,1,JVEGTYPE) = 0.
    PDEPTH(:,2,JVEGTYPE) = 0.2
    IF (ILAYER==3) PDEPTH(:,3,JVEGTYPE) = 3.
  END DO
ELSE
  YNAT='NAT'
  IF (GTEB) YNAT='GRD'
  CALL READ_EXTERN_DEPTH(HFILEPGDTYPE,KLUOUT,YISBA,YNAT,HFIELD,KNI,ILAYER,IPATCH,&
                         ZSOILGRID,PDEPTH,IVERSION)
END IF
!
DEALLOCATE(ZSOILGRID)
!
! *.  Read fraction of nature
!     --------------
!
ALLOCATE(ZNAT(KNI))
IF (IVERSION>=7) THEN
  CALL READ_SURF(HFILEPGDTYPE,'FRAC_NATURE',ZNAT,IRESP,HDIR='A')
ELSE
  ZNAT=1.0  
ENDIF

!
 CALL CLOSE_AUX_IO_SURF(HFILEPGD,HFILEPGDTYPE)
!
!* Allocate soil variable profile
!  ------------------------------
!
!
ALLOCATE(ZVAR(KNI,ILAYER,IPATCH))
ALLOCATE(ZWORK(KNI,IPATCH))
ZWORK(:,:) = XUNDEF
!
! *.  Read soil variable profile
!     --------------------------
!
IF (GTEB) THEN
  CALL OPEN_AUX_IO_SURF(HFILE,HFILETYPE,'TOWN  ')
ELSE
  CALL OPEN_AUX_IO_SURF(HFILE,HFILETYPE,'NATURE')
ENDIF
!
  YRECFM=TRIM(HNAME)
  CALL READ_SURF_FIELD3D(HFILETYPE,ZVAR,1,ILAYER,YRECFM,HDIR='A')
!
 CALL CLOSE_AUX_IO_SURF(HFILE,HFILETYPE)
!
DEALLOCATE(ZWORK)
!
!
! *.  Compute relative humidity from units kg/m^2 (SWI)
!     ------------------------------------------------
!
!* In case of force-restore ISBA, adds one layer at bottom of surface layer
IF ((HFIELD=='WG    ' .OR. HFIELD=='WGI   ') .AND. (YISBA=='2-L' .OR. YISBA=='3-L')) THEN
  ALLOCATE(ZFIELD(KNI,ILAYER,IPATCH))
  ZFIELD(:,:,:) = ZVAR(:,:,:)
  DEALLOCATE(ZVAR)
  !
  ILAYER = ILAYER + 1
  IF ( YISBA=='3-L' ) ILAYER = ILAYER + 1
  ALLOCATE(ZVAR(KNI,ILAYER,IPATCH))
  DO JPATCH=1,IPATCH
    ZVAR(:,1,JPATCH)=ZFIELD(:,1,JPATCH)
    ZVAR(:,2,JPATCH)=ZFIELD(:,2,JPATCH)  ! new layer at root layer humidity but below surface layer
    ZVAR(:,3,JPATCH)=ZFIELD(:,2,JPATCH)
    IF ( YISBA=='3-L' ) THEN
      ZVAR(:,4,JPATCH)=ZFIELD(:,3,JPATCH)
      ZVAR(:,5,JPATCH)=ZFIELD(:,3,JPATCH)
    END IF
  END DO
  DEALLOCATE(ZFIELD)
END IF
!
ALLOCATE(ZFIELD(KNI,ILAYER,IPATCH))
ZFIELD = ZVAR
!
IF (HFIELD=='WG    ' .OR. HFIELD=='WGI   ') THEN
  !
  ! Compute ISBA model constants
  !
  ALLOCATE (ZWFC  (KNI))
  ALLOCATE (ZWWILT(KNI))
  ALLOCATE (ZWSAT (KNI))
  !
  ZWSAT (:) = WSAT_FUNC (ZCLAY(:),ZSAND(:),YPEDOTF)
  ZWWILT(:) = WWILT_FUNC(ZCLAY(:),ZSAND(:),YPEDOTF)
  ZWFC  (:) = WFC_FUNC  (ZCLAY(:),ZSAND(:),YPEDOTF)
  !
  DEALLOCATE (ZSAND)
  DEALLOCATE (ZCLAY)

  ZFIELD(:,:,:) = XUNDEF
  !
  IF (HFIELD=='WG    ') THEN
    DO JPATCH=1,IPATCH
      DO JLAYER=1,ILAYER
        WHERE(ZNAT(:)>0.0 .AND. ZVAR(:,JLAYER,JPATCH)/=XUNDEF)
          ZVAR(:,JLAYER,JPATCH) = MAX(MIN(ZVAR(:,JLAYER,JPATCH),ZWSAT(:)),0.)
          !
          ZFIELD(:,JLAYER,JPATCH) = (ZVAR(:,JLAYER,JPATCH) - ZWWILT(:)) / (ZWFC(:) - ZWWILT(:))
        END WHERE
      END DO
    END DO
  ELSE IF (HFIELD=='WGI   ') THEN
    DO JPATCH=1,IPATCH
      DO JLAYER=1,ILAYER
        WHERE(ZNAT(:)>0.0 .AND. ZVAR(:,JLAYER,JPATCH)/=XUNDEF)
          ZFIELD(:,JLAYER,JPATCH) = ZVAR(:,JLAYER,JPATCH) / ZWSAT(:)  
        END WHERE 
      END DO
    END DO
  END IF
!
  DEALLOCATE (ZNAT)
  DEALLOCATE (ZWSAT)
  DEALLOCATE (ZWWILT)
  DEALLOCATE (ZWFC)
!
!
END IF
!
DEALLOCATE(ZVAR)
!-------------------------------------------------------------------------------
!
! *.  Set the field on all vegtypes
!     -----------------------------
!
ALLOCATE(PFIELD(KNI,ILAYER,NVEGTYPE))
 CALL PUT_ON_ALL_VEGTYPES(KNI,ILAYER,IPATCH,NVEGTYPE,ZFIELD,PFIELD)
DEALLOCATE(ZFIELD)
IF (LHOOK) CALL DR_HOOK('MODE_READ_EXTERN:READ_EXTERN_ISBA',1,ZHOOK_HANDLE)
!
!------------------------------------------------------------------------------
!
END SUBROUTINE READ_EXTERN_ISBA
!
!------------------------------------------------------------------------------
!
END MODULE MODE_READ_EXTERN                       
