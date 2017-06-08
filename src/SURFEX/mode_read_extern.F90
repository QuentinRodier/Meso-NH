!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #####################
MODULE MODE_READ_EXTERN
!     #####################
!-------------------------------------------------------------------
!
USE MODD_SURF_PAR,       ONLY : NUNDEF, XUNDEF
USE MODD_DATA_COVER_PAR, ONLY : JPCOVER, NVEGTYPE
!
USE MODE_READ_SURF_COV, ONLY : READ_SURF_COV
!
USE MODI_READ_LECOCLIMAP
!
USE MODI_OLD_NAME
USE MODI_OPEN_AUX_IO_SURF
USE MODI_CLOSE_AUX_IO_SURF
USE MODI_READ_SURF
USE MODI_ABOR1_SFX
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
 CONTAINS
!
!---------------------------------------------------------------------------------------
!
!     #######################
      SUBROUTINE READ_EXTERN_DEPTH (U, &
                                     DTCO, I,GCP, &
                                    HFILE,HPROGRAM,KLUOUT,HISBA,HNAT,HFIELD,KNI,KLAYER, &
                                   KPATCH,PSOILGRID,PDEPTH,KVERSION,KWG_LAYER          )
!     #######################
!
!
!
!
!
!
!
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_ISBA_n, ONLY : ISBA_t
USE MODD_GRID_CONF_PROJ, ONLY : GRID_CONF_PROJ_t
!
USE MODI_READ_SURF_ISBA_PAR_n
USE MODI_READ_SURF_FIELD3D
USE MODI_READ_SURF_FIELD2D
USE MODI_CONVERT_COVER_ISBA
USE MODI_GARDEN_SOIL_DEPTH
!
! Modifications :
! P.Marguinaud : 11-09-2012 : shorten field name
! G.Delautier : 24-06-2015 : bug for arome compressed files
! M.Moge    01/2016  using READ_SURF_FIELD2D/3D for 2D/3D surfex fields reads
!
IMPLICIT NONE
!
!* dummy arguments
!  ---------------
!
!
!
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(GRID_CONF_PROJ_t),INTENT(INOUT) :: GCP
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(ISBA_t), INTENT(INOUT) :: I
!
 CHARACTER(LEN=28),  INTENT(IN)      :: HFILE     ! name of file!
 CHARACTER(LEN=6),     INTENT(IN)    :: HPROGRAM  ! type of input file
INTEGER,              INTENT(IN)    :: KLUOUT    ! logical unit of output listing
 CHARACTER(LEN=3),     INTENT(IN)    :: HISBA     ! type of ISBA soil scheme
 CHARACTER(LEN=3),     INTENT(IN)    :: HNAT      ! type of surface (nature, gardens)
 CHARACTER(LEN=7),     INTENT(IN)    :: HFIELD    ! field name
INTEGER,              INTENT(IN)    :: KNI       ! number of points
INTEGER,              INTENT(IN)    :: KLAYER    ! number of layers
INTEGER,              INTENT(IN)    :: KPATCH    ! number of patch
INTEGER,              INTENT(IN)    :: KVERSION  ! surface version
REAL, DIMENSION(:),   INTENT(IN)    :: PSOILGRID !
REAL, DIMENSION(:,:,:), POINTER     :: PDEPTH    ! depth of each layer over each patches
INTEGER, DIMENSION(:,:), INTENT(OUT):: KWG_LAYER
!
!* local variables
!  ---------------
!
 CHARACTER(LEN=4 ) :: YLVL
 CHARACTER(LEN=LEN_HREC) :: YRECFM         ! Name of the article to be read
 CHARACTER(LEN=LEN_HREC) :: YRECFM2
 CHARACTER(LEN=100):: YCOMMENT       ! Comment string
INTEGER           :: IRESP          ! reading return code
INTEGER           :: JLAYER         ! loop counter
INTEGER           :: JPATCH         ! loop counter
INTEGER           :: JJ
INTEGER           :: IVERSION
INTEGER           :: IBUGFIX
!
LOGICAL, DIMENSION(JPCOVER)          :: GCOVER ! flag to read the covers
REAL,    DIMENSION(:,:), ALLOCATABLE :: ZCOVER ! cover fractions
REAL,    DIMENSION(:,:), ALLOCATABLE :: ZGROUND_DEPTH ! cover fractions
REAL,  DIMENSION(:,:),   ALLOCATABLE :: ZWORK  ! work array
INTEGER, DIMENSION(:), ALLOCATABLE   :: ZSOILGRID
REAL,  DIMENSION(KNI)                :: ZHVEG  ! high vegetation fraction
REAL,  DIMENSION(KNI)                :: ZLVEG  ! low  vegetation fraction
REAL,  DIMENSION(KNI)                :: ZNVEG  ! no   vegetation fraction
REAL,  DIMENSION(KNI)                :: ZPERM  ! permafrost distribution
 CHARACTER(LEN=4)                     :: YHVEG  ! type of high vegetation
 CHARACTER(LEN=4)                     :: YLVEG  ! type of low  vegetation
 CHARACTER(LEN=4)                     :: YNVEG  ! type of no   vegetation
LOGICAL                              :: GECOCLIMAP ! T if ecoclimap is used
LOGICAL                              :: GPAR_GARDEN! T if garden data are used
LOGICAL                              :: GDATA_DG
LOGICAL                              :: GDATA_GROUND_DEPTH, GDATA_ROOT_DEPTH
LOGICAL                              :: GPERM
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!
!------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_EXTERN:READ_EXTERN_DEPTH',0,ZHOOK_HANDLE)
!
 CALL OPEN_AUX_IO_SURF(&
                      HFILE,HPROGRAM,'FULL  ')
YRECFM='VERSION'
 CALL READ_SURF(&
               HPROGRAM,YRECFM,IVERSION,IRESP)
YRECFM='BUG'
 CALL READ_SURF(&
               HPROGRAM,YRECFM,IBUGFIX,IRESP)
 CALL CLOSE_AUX_IO_SURF(HFILE,HPROGRAM)
!
IF (HNAT=='NAT') THEN
  CALL OPEN_AUX_IO_SURF(&
                      HFILE,HPROGRAM,'FULL  ')
  CALL READ_LECOCLIMAP(&
                       HPROGRAM,GECOCLIMAP)
  CALL CLOSE_AUX_IO_SURF(HFILE,HPROGRAM)
ELSE
  CALL OPEN_AUX_IO_SURF(&
                      HFILE,HPROGRAM,'TOWN  ')
  CALL READ_SURF(&
               HPROGRAM,'PAR_GARDEN',GPAR_GARDEN,IRESP)
  CALL CLOSE_AUX_IO_SURF(HFILE,HPROGRAM)
  GECOCLIMAP = .NOT. GPAR_GARDEN
END IF
!
!------------------------------------------------------------------------------
!
!* permafrost distribution for soil depth
!
GPERM   =.FALSE.
ZPERM(:)=0.0
!
IF (HNAT=='NAT'.AND.(IVERSION>7 .OR. (IVERSION==7 .AND. IBUGFIX>3)))THEN
   CALL OPEN_AUX_IO_SURF(&
                      HFILE,HPROGRAM,'NATURE')
   YRECFM='PERMAFROST'
   CALL READ_SURF(&
               HPROGRAM,YRECFM,GPERM,IRESP)
   IF(GPERM)THEN
     YRECFM='PERM'
     CALL READ_SURF(&
               HPROGRAM,YRECFM,ZPERM(:),IRESP,HDIR='A')           
   ENDIF
   CALL CLOSE_AUX_IO_SURF(HFILE,HPROGRAM)
ENDIF
!
ALLOCATE(PDEPTH(KNI,KLAYER,KPATCH))
PDEPTH(:,:,:) = XUNDEF
!
KWG_LAYER(:,:) = NUNDEF
!
IF (GECOCLIMAP) THEN
  !
 IF (IVERSION<8 .OR. IVERSION==8 .AND. IBUGFIX<=1) THEN  !
  CALL OPEN_AUX_IO_SURF(&
                      HFILE,HPROGRAM,'FULL  ')
  !
  !* reading of the cover to obtain the depth of inter-layers
  !
  CALL OLD_NAME(&
                HPROGRAM,'COVER_LIST      ',YRECFM)
  CALL READ_SURF(&
               HPROGRAM,YRECFM,GCOVER(:),IRESP,HDIR='-')
  !
  ALLOCATE(ZCOVER(KNI,COUNT(GCOVER)))
  YRECFM='COVER'
  CALL READ_SURF_COV(&
                     HPROGRAM,YRECFM,ZCOVER(:,:),GCOVER(:),IRESP,HDIR='A')
  !
  !* computes soil layers
  !  
  CALL CONVERT_COVER_ISBA(DTCO, I, &
                          HISBA,1,ZCOVER,GCOVER,'   ',HNAT,PSOILGRID=PSOILGRID, &
                          PPERM=ZPERM,PDG=PDEPTH,KWG_LAYER=KWG_LAYER             )
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
    CALL OPEN_AUX_IO_SURF(&
                      HFILE,HPROGRAM,'NATURE')    
    YRECFM='ECO_DG'
  ELSE
    CALL OPEN_AUX_IO_SURF(&
                      HFILE,HPROGRAM,'TOWN  ')    
    YRECFM='GD_ECO_DG'
  END IF
  CALL READ_SURF_FIELD3D(HPROGRAM,PDEPTH,1,SIZE(PDEPTH,2),YRECFM,HDIR='A')
  !
  IF (HISBA=='DIF') THEN
    YRECFM='ECO_WG_L'
    IF (HNAT=='GRD') YRECFM='GD_ECO_WG_L'
    ALLOCATE(ZWORK(KNI,KPATCH)) 
    CALL READ_SURF_FIELD2D(HPROGRAM,ZWORK(:,:),YRECFM,HDIR='A')
    WHERE (ZWORK==XUNDEF) ZWORK=NUNDEF
    KWG_LAYER=NINT(ZWORK)
    DEALLOCATE(ZWORK)
  END IF
 END IF
  !
  CALL CLOSE_AUX_IO_SURF(HFILE,HPROGRAM)
  !
ENDIF
!
IF (HNAT=='GRD') THEN
  CALL OPEN_AUX_IO_SURF(&
                      HFILE,HPROGRAM,'TOWN  ')
ELSE
  CALL OPEN_AUX_IO_SURF(&
                      HFILE,HPROGRAM,'NATURE')
ENDIF
!
!-------------------------------------------------------------------
IF (HNAT=='NAT' .AND. (IVERSION>=7 .OR. .NOT.GECOCLIMAP)) THEN
  !
  !* directly read soil layers in the file for nature ISBA soil layers
  !
  GDATA_DG = .TRUE.
  IF (IVERSION>=7) THEN
    YRECFM='L_DG'
    YCOMMENT=YRECFM
    CALL READ_SURF(&
               HPROGRAM,YRECFM,GDATA_DG,IRESP,HCOMMENT=YCOMMENT)
  ENDIF
  !
  IF (GDATA_DG) THEN
    !
    ALLOCATE(ZWORK(KNI,KPATCH))
    DO JLAYER=1,KLAYER
      IF (JLAYER<10)  WRITE(YRECFM,FMT='(A4,I1.1)') 'D_DG',JLAYER
      IF (JLAYER>=10) WRITE(YRECFM,FMT='(A4,I2.2)') 'D_DG',JLAYER
      CALL READ_SURF_ISBA_PAR_n(DTCO, U, I,GCP, &
                                HPROGRAM,YRECFM,KLUOUT,KNI,ZWORK,IRESP,IVERSION,HDIR='A')
      DO JPATCH=1,KPATCH
        PDEPTH(:,JLAYER,JPATCH) = ZWORK(:,JPATCH)
      END DO
    END DO
    DEALLOCATE(ZWORK)
    !
  ENDIF
  !
  IF (IVERSION>7 .OR. IVERSION==7 .AND. IBUGFIX>=2) THEN
    !
    !cas when root_depth and ground_depth were extrapolated in extrapol_field
    !during pgd step
    IF (.NOT.GDATA_DG .AND. HISBA=="3-L") THEN
      !
      YRECFM2='L_ROOT_DEPTH'
      YCOMMENT=YRECFM2
      CALL READ_SURF(HPROGRAM,YRECFM2,GDATA_ROOT_DEPTH,IRESP,HCOMMENT=YCOMMENT)
      !
      IF (GDATA_ROOT_DEPTH) THEN
        YRECFM2='D_ROOT_DEPTH'
        CALL READ_SURF_ISBA_PAR_n(DTCO, U, I,GCP, &
                                  HPROGRAM,YRECFM2,KLUOUT,KNI,PDEPTH(:,2,:),IRESP,IVERSION,HDIR='A')
      ENDIF
      !
    ENDIF
    !    
    YRECFM2='L_GROUND_DEPTH'
    IF (IVERSION>7 .OR. IVERSION==7 .AND. IBUGFIX>=3) YRECFM2='L_GROUND_DPT'
    YCOMMENT=YRECFM2
    CALL READ_SURF(&
               HPROGRAM,YRECFM2,GDATA_GROUND_DEPTH,IRESP,HCOMMENT=YCOMMENT)
    IF (IRESP/=0 .AND. YRECFM2=='L_GROUND_DEPTH' ) THEN
     WRITE  (KLUOUT,*) ' | ERROR Reading L_GROUND_DEPTH try with L_GROUND_DPT'

      YRECFM2='L_GROUND_DPT'
      YCOMMENT=YRECFM2
      CALL READ_SURF(&
               HPROGRAM,YRECFM2,GDATA_GROUND_DEPTH,IRESP,HCOMMENT=YCOMMENT)
    ENDIF  
    !
    IF (GDATA_GROUND_DEPTH) THEN
      !
      YRECFM2='D_GROUND_DETPH'
      IF (IVERSION>7 .OR. IVERSION==7 .AND. IBUGFIX>=3) YRECFM2='D_GROUND_DPT'
      ALLOCATE(ZGROUND_DEPTH(KNI,KPATCH))
      CALL READ_SURF_ISBA_PAR_n(DTCO, U, I,GCP, &
                                HPROGRAM,YRECFM2,KLUOUT,KNI,ZGROUND_DEPTH(:,:),IRESP,IVERSION,HDIR='A')
      !
      IF (.NOT.GDATA_DG) THEN
        !
        IF (HISBA=="2-L") THEN
          !
          PDEPTH(:,2,:) = ZGROUND_DEPTH(:,:)
          PDEPTH(:,1,:) = XUNDEF
          WHERE (ZGROUND_DEPTH(:,:)/=XUNDEF) PDEPTH(:,1,:) = 0.01
          !
        ELSEIF (HISBA=="3-L") THEN
          !
          PDEPTH(:,3,:) = ZGROUND_DEPTH(:,:)
          PDEPTH(:,1,:) = XUNDEF
          WHERE (ZGROUND_DEPTH(:,:)/=XUNDEF) PDEPTH(:,1,:) = 0.01
          !
        ELSEIF (HISBA=="DIF") THEN
          !
          ALLOCATE(ZSOILGRID(KLAYER))
          DO JLAYER=1,KLAYER
            WRITE(YLVL,'(I4)') JLAYER
            YRECFM2='SOILGRID'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
            CALL READ_SURF(HPROGRAM,YRECFM,ZSOILGRID(JLAYER),IRESP)
            PDEPTH(:,JLAYER,:) = ZSOILGRID(JLAYER)
          ENDDO
          DEALLOCATE(ZSOILGRID)
          !
        ENDIF
      ENDIF
      !      
      DO JPATCH=1,KPATCH
        DO JJ=1,KNI
          DO JLAYER=1,KLAYER
            IF ( PDEPTH(JJ,JLAYER,JPATCH) <= ZGROUND_DEPTH(JJ,JPATCH) .AND. ZGROUND_DEPTH(JJ,JPATCH) < XUNDEF ) &
                KWG_LAYER(JJ,JPATCH) = JLAYER
          ENDDO
        ENDDO
      ENDDO
      DEALLOCATE(ZGROUND_DEPTH)
      !
    ENDIF
    !
  ENDIF
  !
ELSE IF (HNAT=='GRD' .AND. .NOT.GECOCLIMAP) THEN
  !
  !* computes soil layers from vegetation fractions read in the file
  !
  CALL READ_SURF(&
               HPROGRAM,'D_TYPE_HVEG',YHVEG,IRESP)
  CALL READ_SURF(&
               HPROGRAM,'D_TYPE_LVEG',YLVEG,IRESP)
  CALL READ_SURF(&
               HPROGRAM,'D_TYPE_NVEG',YNVEG,IRESP)
  CALL READ_SURF(&
               HPROGRAM,'D_FRAC_HVEG',ZHVEG,IRESP,HDIR='A')
  CALL READ_SURF(&
               HPROGRAM,'D_FRAC_LVEG',ZLVEG,IRESP,HDIR='A')
  CALL READ_SURF(&
               HPROGRAM,'D_FRAC_NVEG',ZNVEG,IRESP,HDIR='A')
  ! Ground layers
  CALL GARDEN_SOIL_DEPTH(YNVEG,YLVEG,YHVEG,ZNVEG,ZLVEG,ZHVEG,PDEPTH)
  !
END IF
!
 CALL CLOSE_AUX_IO_SURF(HFILE,HPROGRAM)
!-------------------------------------------------------------------
!
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
      SUBROUTINE READ_EXTERN_ISBA (U, &
                                    DTCO, I,GCP, &
                                   HFILE,HFILETYPE,HFILEPGD,HFILEPGDTYPE,&
                                  KLUOUT,KNI,HFIELD,HNAME,PFIELD,PDEPTH,OKEY)
!     #######################
!
!
!
!
!
!
!
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_ISBA_n, ONLY : ISBA_t
USE MODD_GRID_CONF_PROJ, ONLY : GRID_CONF_PROJ_t
!
USE MODD_ISBA_PAR,    ONLY : XOPTIMGRID
!
USE MODE_SOIL
USE MODI_ISBA_SOC_PARAMETERS
USE MODI_READ_SURF
USE MODI_READ_SURF_FIELD3D
!
IMPLICIT NONE
!
!* dummy arguments
!  ---------------
!
!
!
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(ISBA_t), INTENT(INOUT) :: I
TYPE(GRID_CONF_PROJ_t),INTENT(INOUT) :: GCP
!
 CHARACTER(LEN=28),    INTENT(IN)  :: HFILE     ! name of file
 CHARACTER(LEN=6),     INTENT(IN)  :: HFILETYPE ! type of input file
 CHARACTER(LEN=28),    INTENT(IN)  :: HFILEPGD     ! name of file
 CHARACTER(LEN=6),     INTENT(IN)  :: HFILEPGDTYPE ! type of input file
INTEGER,              INTENT(IN)  :: KLUOUT    ! logical unit of output listing
INTEGER,              INTENT(IN)  :: KNI       ! number of points
 CHARACTER(LEN=7),     INTENT(IN)  :: HFIELD    ! field name
 CHARACTER(LEN=*),     INTENT(IN)  :: HNAME     ! field name in the file
REAL, DIMENSION(:,:,:), POINTER   :: PFIELD    ! field to initialize
REAL, DIMENSION(:,:,:), POINTER   :: PDEPTH    ! depth of each inter-layer
LOGICAL, OPTIONAL,  INTENT(INOUT) :: OKEY
!
!
!* local variables
!  ---------------
!
 CHARACTER(LEN=LEN_HREC) :: YRECFM         ! Name of the article to be read
 CHARACTER(LEN=4)  :: YLVL
 CHARACTER(LEN=3)  :: YISBA          ! type of ISBA soil scheme
#ifdef MNH_PARALLEL
 CHARACTER(LEN=8)  :: YPATCH
#endif
 CHARACTER(LEN=3)  :: YNAT           ! type of surface (nature, garden)
 CHARACTER(LEN=4)  :: YPEDOTF        ! type of pedo-transfert function
INTEGER           :: IRESP          ! reading return code
INTEGER           :: ILAYER         ! number of layers
INTEGER           :: JLAYER         ! loop counter
INTEGER           :: IPATCH         ! number of patch
INTEGER           :: JPATCH         ! loop counter
INTEGER           :: JVEGTYPE       ! loop counter
LOGICAL           :: GTEB           ! TEB field
INTEGER           :: IWORK          ! work integer
INTEGER           :: JI
!
REAL,  DIMENSION(:,:),   ALLOCATABLE :: ZWORK  ! field read, one level, all patches
!
REAL,  DIMENSION(:,:,:), ALLOCATABLE :: ZVAR      ! profile of physical variable
REAL,  DIMENSION(:),   ALLOCATABLE   :: ZCLAY     ! clay fraction
REAL,  DIMENSION(:),   ALLOCATABLE   :: ZSAND     ! sand fraction
REAL,  DIMENSION(:),   ALLOCATABLE   :: ZSOILGRID
REAL,  DIMENSION(:),   ALLOCATABLE   :: ZNAT      ! natural surface fraction
!
REAL,  DIMENSION(:,:),   ALLOCATABLE :: ZWWILT    ! wilting point
REAL,  DIMENSION(:,:),   ALLOCATABLE :: ZWFC      ! field capacity
REAL,  DIMENSION(:,:),   ALLOCATABLE :: ZWSAT     ! saturation
REAL,  DIMENSION(:,:),   ALLOCATABLE :: ZPATCH
REAL,  DIMENSION(:,:),   ALLOCATABLE :: ZBCOEF
REAL,  DIMENSION(:,:),   ALLOCATABLE :: ZMPOTSAT
REAL,  DIMENSION(:,:),   ALLOCATABLE :: ZHCAPSOIL
REAL,  DIMENSION(:,:),   ALLOCATABLE :: ZCONDDRY
REAL,  DIMENSION(:,:),   ALLOCATABLE :: ZCONDSLD
REAL,  DIMENSION(:,:),   ALLOCATABLE :: ZWD0
REAL,  DIMENSION(:,:),   ALLOCATABLE :: ZANISO
REAL,  DIMENSION(:,:),   ALLOCATABLE :: ZFRACSOC
REAL,  DIMENSION(:,:,:), ALLOCATABLE :: ZCONDSAT
!
REAL,  DIMENSION(KNI,2)              :: ZSOC
!
INTEGER, DIMENSION(:,:), ALLOCATABLE :: IWG_LAYER
!
LOGICAL                              :: GTEMP_ARP ! Arpege soil temperature profile
LOGICAL                              :: GSOC_DATA ! Soil organic carbon (data in pgd)
LOGICAL                              :: GSOC      ! Soil organic carbon (physical option)
!
INTEGER :: IVERSION   ! surface version
INTEGER :: IBUGFIX
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('MODE_READ_EXTERN:READ_EXTERN_ISBA',0,ZHOOK_HANDLE)
WRITE  (KLUOUT,*) ' | Reading ',HFIELD,' in externalized file'
!
!------------------------------------------------------------------------------
! Init
!
GTEB = (HNAME(1:3)=='TWN' .OR. HNAME(1:3)=='GD_' .OR. HNAME(1:3)=='GR_' &
        .OR. HNAME(4:6)=='GD_' .OR. HNAME(4:6)=='GR_')
!
GTEMP_ARP = .FALSE.
GSOC      = .FALSE.
GSOC_DATA = .FALSE.
!
!------------------------------------------------------------------------------
!
 CALL OPEN_AUX_IO_SURF(&
                      HFILEPGD,HFILEPGDTYPE,'FULL  ')
YRECFM='VERSION'
 CALL READ_SURF(&
               HFILEPGDTYPE,YRECFM,IVERSION,IRESP)
YRECFM='BUG'
 CALL READ_SURF(&
               HFILEPGDTYPE,YRECFM,IBUGFIX,IRESP)
 CALL CLOSE_AUX_IO_SURF(HFILEPGD,HFILEPGDTYPE)
!
IF (GTEB) THEN
  CALL OPEN_AUX_IO_SURF(&
                      HFILEPGD,HFILEPGDTYPE,'TOWN  ')
ELSE
  CALL OPEN_AUX_IO_SURF(&
                      HFILEPGD,HFILEPGDTYPE,'NATURE')
ENDIF
!
!* Read number of soil layers
!
YRECFM='GROUND_LAYER'
IF (GTEB) THEN 
  YRECFM='TWN_LAYER'
  IF (IVERSION>7 .OR. IVERSION==7 .AND. IBUGFIX>=3) YRECFM='GD_LAYER'
ENDIF
 CALL READ_SURF(&
               HFILEPGDTYPE,YRECFM,ILAYER,IRESP)
!
!* number of tiles
!
IPATCH=1
IF (.NOT. GTEB) THEN
  YRECFM='PATCH_NUMBER'
  CALL READ_SURF(&
               HFILEPGDTYPE,YRECFM,IPATCH,IRESP)
END IF
!
!* soil scheme
!
YRECFM='ISBA'
IF (GTEB) THEN 
  YRECFM='TWN_ISBA'
  IF (IVERSION>7 .OR. IVERSION==7 .AND. IBUGFIX>=3) YRECFM='GD_ISBA'
ENDIF
 CALL READ_SURF(&
               HFILEPGDTYPE,YRECFM,YISBA,IRESP)
IF(YISBA=='DIF'.AND.PRESENT(OKEY))THEN
  OKEY=.FALSE.
ENDIF
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
  CALL READ_SURF(&
               HFILEPGDTYPE,YRECFM,YPEDOTF,IRESP)
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
 CALL READ_SURF(&
               HFILEPGDTYPE,YRECFM,ZCLAY(:),IRESP,HDIR='A')
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
 CALL READ_SURF(&
               HFILEPGDTYPE,YRECFM,ZSAND(:),IRESP,HDIR='A')
!
!-------------------------------------------------------------------------------
!
!
! *.  Soil organic carbon profile
!     ---------------------------
!
IF ( (.NOT.GTEB).AND.(IVERSION>7.OR.(IVERSION==7.AND.IBUGFIX>3)) &
     .AND.(YISBA=='DIF').AND.(HFIELD=='WG    '.OR.HFIELD=='WGI   ') ) THEN
   YRECFM='SOCP'
   CALL READ_SURF(&
               HFILEPGDTYPE,YRECFM,GSOC_DATA,IRESP)
   IF(GSOC_DATA)THEN
     YRECFM='SOC_TOP'
     CALL READ_SURF(&
               HFILEPGDTYPE,YRECFM,ZSOC(:,1),IRESP,HDIR='A')
     YRECFM='SOC_SUB'
     CALL READ_SURF(&
               HFILEPGDTYPE,YRECFM,ZSOC(:,2),IRESP,HDIR='A')
     WHERE(ZSOC(:,:)==XUNDEF)ZSOC(:,:)=0.0
   ENDIF
ENDIF
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
  IF (IVERSION>=8) THEN
     DO JLAYER=1,ILAYER
        WRITE(YLVL,'(I4)') JLAYER
        YRECFM='SOILGRID'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
        IF (GTEB) THEN 
           YRECFM='GD_SGRID'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
        ENDIF
        CALL READ_SURF(&
               HFILEPGDTYPE,YRECFM,ZSOILGRID(JLAYER),IRESP)
     ENDDO
  ELSEIF (IVERSION==7 .AND. IBUGFIX>=2) THEN
    YRECFM='SOILGRID'
    IF (GTEB) THEN 
      YRECFM='TWN_SOILGRID'
      IF (IVERSION>7 .OR. IVERSION==7 .AND. IBUGFIX>=3) YRECFM='GD_SOILGRID'
    ENDIF
    CALL READ_SURF(&
               HFILEPGDTYPE,YRECFM,ZSOILGRID,IRESP,HDIR='-')
  ELSE
    ZSOILGRID(1:ILAYER) = XOPTIMGRID(1:ILAYER)
  ENDIF
ELSE
  ALLOCATE(ZSOILGRID(0))
ENDIF
!
ALLOCATE(IWG_LAYER(KNI,IPATCH))
IWG_LAYER(:,:) = NUNDEF        
!
! *.  Read fraction of nature
!     --------------
!
ALLOCATE(ZNAT(KNI))
IF (IVERSION>=7) THEN
  IF (GTEB) THEN
    CALL READ_SURF(HFILEPGDTYPE,'FRAC_TOWN',ZNAT,IRESP,HDIR='A')
  ELSE
    CALL READ_SURF(HFILEPGDTYPE,'FRAC_NATURE',ZNAT,IRESP,HDIR='A')
  ENDIF
ELSE
  ZNAT=1.0
ENDIF
!
 CALL CLOSE_AUX_IO_SURF(HFILEPGD,HFILEPGDTYPE)
!
IF (.NOT.GTEB .AND. HFIELD=='TG    ' .AND. (YISBA=='2-L' .OR. YISBA=='3-L') ) THEN
   IF (IVERSION>7) THEN
     YRECFM='TEMPARP'
     CALL OPEN_AUX_IO_SURF(&
                      HFILE,HFILETYPE,'NATURE')
     CALL READ_SURF(&
               HFILEPGDTYPE,YRECFM,GTEMP_ARP,IRESP)
     IF(GTEMP_ARP)THEN
       YRECFM = 'NTEMPLARP'
       CALL READ_SURF(&
               HFILEPGDTYPE,YRECFM,ILAYER,IRESP)     
     ENDIF
     CALL CLOSE_AUX_IO_SURF(HFILE,HFILETYPE)
   ENDIF
ENDIF  
!
IF ((HFIELD=='TG    ') .AND. (YISBA=='2-L' .OR. YISBA=='3-L')) THEN
  ALLOCATE(PDEPTH    (KNI,ILAYER,NVEGTYPE))
  DO JVEGTYPE=1,NVEGTYPE
    PDEPTH(:,1,JVEGTYPE) = 0.01
    PDEPTH(:,2,JVEGTYPE) = 0.40
    IF (ILAYER==3) PDEPTH(:,3,JVEGTYPE) = 5.00
!   GTEMP_ARP case
    IF (GTEMP_ARP) THEN
       PDEPTH(:,3,JVEGTYPE) = 1.0
       DO JLAYER=4,ILAYER
          PDEPTH(:,JLAYER,JVEGTYPE) = PDEPTH(:,JLAYER-1,JVEGTYPE)+1.
       ENDDO
    ENDIF    
  END DO
ELSE
  YNAT='NAT'
  IF (GTEB) YNAT='GRD'
  CALL READ_EXTERN_DEPTH(U, &
                         DTCO, I,GCP, &
                         HFILEPGD,HFILEPGDTYPE,KLUOUT,YISBA,YNAT,HFIELD,KNI,  &
                         ILAYER,IPATCH,ZSOILGRID,PDEPTH,IVERSION,IWG_LAYER)
END IF
!
DEALLOCATE(ZSOILGRID)
!
!
!* Allocate soil variable profile
!  ------------------------------
!
!
ALLOCATE(ZVAR(KNI,ILAYER,IPATCH))
ALLOCATE(ZWORK(KNI,IPATCH))
ZWORK(:,:  ) = XUNDEF
ZVAR (:,:,:) = 0.0
!
! *.  Read soil variable profile
!     --------------------------
!
IF (GTEB) THEN
  CALL OPEN_AUX_IO_SURF(&
                      HFILE,HFILETYPE,'TOWN  ')
ELSE
  CALL OPEN_AUX_IO_SURF(&
                      HFILE,HFILETYPE,'NATURE')
ENDIF
!
IWORK=ILAYER
IF(YISBA=='2-L'.OR.YISBA=='3-L') THEN
  SELECT CASE(HFIELD)
         CASE('TG    ')
             IF(GTEMP_ARP)THEN
               IWORK=ILAYER
             ELSE
               IWORK=2
             ENDIF
         CASE('WGI   ')
             IWORK=2
  END SELECT
ENDIF
!
YRECFM=TRIM(HNAME)
 CALL READ_SURF_FIELD3D(HFILETYPE,ZVAR,1,IWORK,YRECFM,HDIR='A')
DO JI=1,KNI
  IF (ZNAT(JI)==0.) ZVAR(JI,:,:) = XUNDEF
END DO
!
DEALLOCATE (ZNAT)
!
IF(YISBA=='3-L') THEN
  SELECT CASE(HFIELD)
         CASE('TG    ')
         IF(.NOT.GTEMP_ARP)ZVAR(:,3,:)=ZVAR(:,2,:)
         CASE('WGI   ')       
         ZVAR(:,3,:)=ZVAR(:,2,:)
  END SELECT
ENDIF
!
 CALL CLOSE_AUX_IO_SURF(HFILE,HFILETYPE)
!
DEALLOCATE(ZWORK)
!
!
! *.  Compute relative humidity from units kg/m^2 (SWI)
!     ------------------------------------------------
!
ALLOCATE(PFIELD(KNI,ILAYER,IPATCH))
!
PFIELD(:,:,:) = ZVAR(:,:,:)
!
IF (HFIELD=='WG    ' .OR. HFIELD=='WGI   ') THEN
  !
  ! Compute ISBA model constants
  !
  ALLOCATE (ZWFC  (KNI,ILAYER))
  ALLOCATE (ZWWILT(KNI,ILAYER))
  ALLOCATE (ZWSAT (KNI,ILAYER))
  !
  ZWSAT (:,1) = WSAT_FUNC (ZCLAY(:),ZSAND(:),YPEDOTF)
  ZWWILT(:,1) = WWILT_FUNC(ZCLAY(:),ZSAND(:),YPEDOTF)
  IF(YISBA=='DIF')THEN
    ZWFC(:,1) = W33_FUNC(ZCLAY(:),ZSAND(:),YPEDOTF)
  ELSE
    ZWFC(:,1) = WFC_FUNC(ZCLAY(:),ZSAND(:),YPEDOTF)
  ENDIF
  DO JLAYER=2,ILAYER
     ZWSAT (:,JLAYER) = ZWSAT (:,1)
     ZWFC  (:,JLAYER) = ZWFC  (:,1)
     ZWWILT(:,JLAYER) = ZWWILT(:,1)
  ENDDO
  !
  DEALLOCATE (ZSAND)
  DEALLOCATE (ZCLAY)
  !
  IF(GSOC_DATA)THEN
    !
    ALLOCATE(ZPATCH(KNI,IPATCH))
    !
    CALL OPEN_AUX_IO_SURF(&
                      HFILE,HFILETYPE,'NATURE')
    YRECFM='SOC'
    CALL READ_SURF(&
               HFILETYPE,YRECFM,GSOC,IRESP)
    YRECFM='PATCH'
    CALL READ_SURF(&
               HFILETYPE,YRECFM,ZPATCH(:,:),IRESP,HDIR='A')
    WHERE(ZPATCH(:,:)==XUNDEF)ZPATCH(:,:)=0.0
    CALL CLOSE_AUX_IO_SURF(HFILE,HFILETYPE)    
    !
    IF(GSOC)THEN
      ALLOCATE(ZBCOEF   (KNI,ILAYER))
      ALLOCATE(ZMPOTSAT (KNI,ILAYER))
      ALLOCATE(ZHCAPSOIL(KNI,ILAYER))
      ALLOCATE(ZCONDDRY (KNI,ILAYER))
      ALLOCATE(ZCONDSLD (KNI,ILAYER))
      ALLOCATE(ZWD0     (KNI,ILAYER))
      ALLOCATE(ZANISO   (KNI,ILAYER))
      ALLOCATE(ZFRACSOC (KNI,ILAYER))
      ALLOCATE(ZCONDSAT (KNI,ILAYER,IPATCH))
      ZBCOEF   (:,:)=0.0
      ZMPOTSAT (:,:)=0.0
      ZHCAPSOIL(:,:)=0.0
      ZCONDDRY (:,:)=XUNDEF
      ZCONDSLD (:,:)=XUNDEF
      ZWD0     (:,:)=0.0
      ZANISO   (:,:)=0.0
      ZFRACSOC (:,:)=0.0
      ZCONDSAT (:,:,:)=0.0
      CALL ISBA_SOC_PARAMETERS ('NONE' ,ZPATCH,PDEPTH,ZSOC,ZBCOEF,ZMPOTSAT,&
                                ZCONDSAT,ZWSAT,ZHCAPSOIL,ZCONDDRY,ZCONDSLD,&
                                ZWFC,ZWWILT,ZWD0,ZANISO,ZFRACSOC           )
      DEALLOCATE(ZBCOEF)
      DEALLOCATE(ZMPOTSAT)
      DEALLOCATE(ZCONDSAT)
      DEALLOCATE(ZHCAPSOIL)
      DEALLOCATE(ZCONDDRY)
      DEALLOCATE(ZCONDSLD)
      DEALLOCATE(ZWD0)
      DEALLOCATE(ZANISO)
      DEALLOCATE(ZFRACSOC)
    ENDIF
    !
    DEALLOCATE(ZPATCH)
    !
  ENDIF
  !
  PFIELD(:,:,:) = XUNDEF
  !
  IF(YISBA=='DIF')THEN
  !     
  ! extrapolation of deep layers
    DO JPATCH=1,IPATCH
       DO JI=1,KNI
         IWORK=IWG_LAYER(JI,JPATCH)
         IF(IWORK<ILAYER)THEN
           DO JLAYER=IWORK+1,ILAYER
              ZVAR(JI,JLAYER,JPATCH)=ZVAR(JI,IWORK,JPATCH)
           ENDDO
         ENDIF
      ENDDO
    ENDDO
  ENDIF
  !
  IF (HFIELD=='WG    ') THEN
    DO JPATCH=1,IPATCH
      DO JLAYER=1,ILAYER
       DO JI=1,KNI
       IF(ZVAR(JI,JLAYER,JPATCH)/=XUNDEF)THEN
          ZVAR(JI,JLAYER,JPATCH) = MAX(MIN(ZVAR(JI,JLAYER,JPATCH),ZWSAT(JI,JLAYER)),0.)
          !
          PFIELD(JI,JLAYER,JPATCH) = (ZVAR(JI,JLAYER,JPATCH) - ZWWILT(JI,JLAYER)) / (ZWFC(JI,JLAYER) - ZWWILT(JI,JLAYER))
        ENDIF
      END DO
      END DO
    END DO
  ELSE IF (HFIELD=='WGI   ') THEN
    DO JPATCH=1,IPATCH
      DO JLAYER=1,ILAYER
        WHERE(ZVAR(:,JLAYER,JPATCH)/=XUNDEF)
          PFIELD(:,JLAYER,JPATCH) = ZVAR(:,JLAYER,JPATCH) / ZWSAT(:,JLAYER) 
        END WHERE
      END DO
    END DO
  END IF
!
  DEALLOCATE (ZWSAT)
  DEALLOCATE (ZWWILT)
  DEALLOCATE (ZWFC)
!
END IF
!
DEALLOCATE(ZVAR)
DEALLOCATE(IWG_LAYER)
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('MODE_READ_EXTERN:READ_EXTERN_ISBA',1,ZHOOK_HANDLE)
!
!------------------------------------------------------------------------------
!
END SUBROUTINE READ_EXTERN_ISBA
!
!------------------------------------------------------------------------------
!
END MODULE MODE_READ_EXTERN                       
