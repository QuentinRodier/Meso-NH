!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE PREP_SNOW_EXTERN (GCP,&
                             HPROGRAM,HSURF,HFILE,HFILETYPE,HFILEPGD,HFILEPGDTYPE,&
                            KLUOUT,PFIELD,OSNOW_IDEAL,KLAYER,KTEB_PATCH)
!     #################################################################################
!
!
!!****  *PREP_SNOW_EXTERN*  
!!
!!    PURPOSE
!!    -------
!       Read and prepare initial snow fields from external files
!     
!!**  METHOD
!!    ------
!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------ 
!!
!!      
!!    REFERENCE
!!    ---------
!!
!!    
!!    AUTHOR
!!    ------
!!         * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    ?
!!       02/2014 E. Martin : cor. for passing from from multilayer to a single layer
!!      B. Decharme  04/2014, external init with FA files
!!                            improve vertical interpolation
!-------------------------------------------------------------------------------
!
!
!
!
!
USE MODD_GRID_CONF_PROJ, ONLY : GRID_CONF_PROJ_t
USE MODD_TYPE_SNOW
USE MODD_PREP,           ONLY : CINGRID_TYPE, CINTERP_TYPE
USE MODD_PREP_SNOW,      ONLY : XGRID_SNOW, NGRID_LEVEL
USE MODD_DATA_COVER_PAR, ONLY : NVEGTYPE
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODD_CSTS,           ONLY : XTT
!
USE MODE_SNOW3L
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_TOWN_PRESENCE
USE MODI_PUT_ON_ALL_VEGTYPES
USE MODI_ABOR1_SFX
USE MODI_PREP_GRID_EXTERN
USE MODI_OPEN_AUX_IO_SURF
USE MODI_CLOSE_AUX_IO_SURF
USE MODI_ALLOCATE_GR_SNOW
USE MODI_INTERP_GRID_NAT
USE MODI_READ_GR_SNOW
USE MODI_READ_SURF
USE MODI_SNOW_T_WLIQ_TO_HEAT
USE MODI_READ_TEB_PATCH
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
!
TYPE(GRID_CONF_PROJ_t),INTENT(INOUT) :: GCP
 CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
 CHARACTER(LEN=10),  INTENT(IN)  :: HSURF     ! type of field
 CHARACTER(LEN=28),  INTENT(IN)  :: HFILE     ! name of file
 CHARACTER(LEN=6),   INTENT(IN)  :: HFILETYPE ! type of file
 CHARACTER(LEN=28),  INTENT(IN)  :: HFILEPGD     ! name of file
 CHARACTER(LEN=6),   INTENT(IN)  :: HFILEPGDTYPE ! type of file
INTEGER,            INTENT(IN)  :: KLUOUT    ! logical unit of output listing
REAL,DIMENSION(:,:,:), POINTER  :: PFIELD    ! field to interpolate horizontally
LOGICAL,            INTENT(IN)  :: OSNOW_IDEAL
INTEGER,            INTENT(IN)  :: KLAYER    ! Number of layer of output snow scheme
INTEGER,            INTENT(IN) :: KTEB_PATCH
!
!*      0.2    declarations of local variables
!
TYPE(SURF_SNOW)                    :: TZSNOW ! snow characteristics

REAL, DIMENSION(:,:,:), ALLOCATABLE :: ZFIELD       ! work field on input snow grid
REAL, DIMENSION(:,:,:), ALLOCATABLE :: ZFIELD_FINE  ! work field on fine snow grid
REAL, DIMENSION(:,:,:), ALLOCATABLE :: ZTEMP        ! snow temperature
REAL, DIMENSION(:,:,:), ALLOCATABLE :: ZWLIQ        ! liquid water snow pack content
REAL, DIMENSION(:,:),   ALLOCATABLE :: ZD           ! total snow depth
REAL, DIMENSION(:,:,:), ALLOCATABLE :: ZDEPTH       ! thickness of each layer (m)
REAL, DIMENSION(:,:,:), ALLOCATABLE :: ZGRID        ! normalized input grid
!
LOGICAL                           :: GTOWN          ! town variables written in the file
 CHARACTER(LEN=12)                 :: YRECFM         ! record name
INTEGER                           :: IRESP          ! error return code
INTEGER                           :: IVERSION       ! SURFEX version
LOGICAL                           :: GOLD_NAME      ! old name flag 
INTEGER                           :: IBUGFIX        ! SURFEX bug version
INTEGER                           :: IVEGTYPE       ! actual number of vegtypes
INTEGER                           :: JLAYER         ! loop on snow vertical grids
INTEGER                           :: JI             ! loop on pts
INTEGER                           :: INI
 CHARACTER(LEN=8)                  :: YAREA          ! area treated ('ROOF','ROAD','VEG ')
 CHARACTER(LEN=3)                  :: YPREFIX        ! prefix to identify patch
INTEGER                           :: IPATCH         ! number of input patch
INTEGER                           :: ITEB_PATCH     ! number of input patch for TEB
INTEGER                           :: JPATCH         ! loop on patch
 CHARACTER(LEN=6)                  :: YMASK          ! type of tile mask
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------
!
!*      3.     Area being treated
!              ------------------
!
IF (LHOOK) CALL DR_HOOK('PREP_SNOW_EXTERN',0,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
YAREA='        '
YAREA(1:4) = HSURF(7:10)
!
IF (YAREA(1:4)=='VEG ') THEN
  IVEGTYPE = NVEGTYPE
  YMASK = 'NATURE'
  YPREFIX = '   '  
ELSE
  IVEGTYPE = 1
  YMASK    = 'TOWN  '
  IPATCH   = 1
  YPREFIX = '   '  
END IF
!
!*      1.     Preparation of IO for reading in the file
!              -----------------------------------------
!
!* Note that all points are read, even those without physical meaning.
!  These points will not be used during the horizontal interpolation step.
!  Their value must be defined as XUNDEF.
!
!* reading of version of the file being read
 CALL OPEN_AUX_IO_SURF(&
                      HFILEPGD,HFILEPGDTYPE,'FULL  ')
 CALL READ_SURF(&
               HFILEPGDTYPE,'VERSION',IVERSION,IRESP)
 CALL READ_SURF(&
               HFILEPGDTYPE,'BUG',IBUGFIX,IRESP)
 CALL CLOSE_AUX_IO_SURF(HFILEPGD,HFILEPGDTYPE)
!
GOLD_NAME=(IVERSION<7 .OR. (IVERSION==7 .AND. IBUGFIX<3))
!
 CALL OPEN_AUX_IO_SURF(&
                      HFILEPGD,HFILEPGDTYPE,YMASK)
!
IF (YAREA(1:4)=='VEG ') THEN
  YRECFM = 'PATCH_NUMBER'
  CALL READ_SURF(&
               HFILEPGDTYPE,YRECFM,IPATCH,IRESP)
  CALL CLOSE_AUX_IO_SURF(HFILEPGD,HFILEPGDTYPE)
ELSE
  IF (.NOT.GOLD_NAME) THEN
     IF (YAREA(1:4)=='ROOF') YAREA(1:4) = 'RF  '
     IF (YAREA(1:4)=='ROAD') YAREA(1:4) = 'RD  '
   ENDIF
  CALL CLOSE_AUX_IO_SURF(HFILEPGD,HFILEPGDTYPE)
  CALL READ_TEB_PATCH(&
                      HFILEPGD,HFILEPGDTYPE,ITEB_PATCH)
  IF (ITEB_PATCH>1) THEN
    WRITE(YPREFIX,FMT='(A,I1,A)') 'T',MIN(KTEB_PATCH,ITEB_PATCH),'_'
  END IF  
END IF
!
!
!-------------------------------------------------------------------------------------
!
!*      2.     Reading of grid
!              ---------------
!
 CALL OPEN_AUX_IO_SURF(&
                      HFILEPGD,HFILEPGDTYPE,'FULL  ')
!
 CALL PREP_GRID_EXTERN(GCP,&
                      HFILEPGDTYPE,KLUOUT,CINGRID_TYPE,CINTERP_TYPE,INI)
!
!-------------------------------------------------------------------------------------
!
!*      4.     Reading of snow data
!              ---------------------
!
IF (YAREA(1:2)=='RO' .OR. YAREA(1:2)=='GA' .OR. YAREA(1:2)=='RF' .OR. YAREA(1:2)=='RD') THEN
  CALL TOWN_PRESENCE(&
                     HFILEPGDTYPE,GTOWN)
  CALL CLOSE_AUX_IO_SURF(HFILEPGD,HFILEPGDTYPE)
  IF (.NOT. GTOWN) THEN
    TZSNOW%SCHEME='1-L'
    TZSNOW%NLAYER=1
    CALL ALLOCATE_GR_SNOW(TZSNOW,INI,IPATCH)
  ELSE
    CALL OPEN_AUX_IO_SURF(&
                      HFILE,HFILETYPE,YMASK)
    CALL READ_GR_SNOW(&
                      HFILETYPE,TRIM(YAREA),YPREFIX,INI,IPATCH,TZSNOW, &
                      HDIR='A',KVERSION=IVERSION,KBUGFIX=IBUGFIX)
    CALL CLOSE_AUX_IO_SURF(HFILE,HFILETYPE)
  ENDIF
ELSE
  CALL CLOSE_AUX_IO_SURF(HFILEPGD,HFILEPGDTYPE)
  CALL OPEN_AUX_IO_SURF(&
                      HFILE,HFILETYPE,YMASK)
  CALL READ_GR_SNOW(&
                      HFILETYPE,TRIM(YAREA),YPREFIX,INI,IPATCH,TZSNOW, &
                    HDIR='A',KVERSION=IVERSION,KBUGFIX=IBUGFIX)
  CALL CLOSE_AUX_IO_SURF(HFILE,HFILETYPE)
ENDIF
!
!-------------------------------------------------------------------------------------
!
!*      5.     Total snow content
!              ------------------
!
SELECT CASE (HSURF(1:3))
  CASE ('WWW')
    IF (OSNOW_IDEAL) THEN
      ALLOCATE(ZFIELD(INI,KLAYER,IPATCH))
      ZFIELD(:,:,:) = TZSNOW%WSNOW(:,1:KLAYER,:)
      ALLOCATE(PFIELD(INI,KLAYER,IVEGTYPE))
      CALL PUT_ON_ALL_VEGTYPES(INI,KLAYER,IPATCH,IVEGTYPE,ZFIELD,PFIELD)
    ELSE
      ALLOCATE(ZFIELD(INI,1,IPATCH))
      ZFIELD(:,1,:) = 0.0
      DO JLAYER=1,TZSNOW%NLAYER
        ZFIELD(:,1,:) = ZFIELD(:,1,:) + TZSNOW%WSNOW(:,JLAYER,:)
      END DO 
      WHERE ( ZFIELD(:,1,:)>XUNDEF ) ZFIELD(:,1,:)=XUNDEF
      ALLOCATE(PFIELD(INI,1,IVEGTYPE))
      CALL PUT_ON_ALL_VEGTYPES(INI,1,IPATCH,IVEGTYPE,ZFIELD,PFIELD)
    ENDIF
    DEALLOCATE(ZFIELD)
!
!-------------------------------------------------------------------------------------
!
!*      6.     Albedo
!              ------
!
  CASE ('ALB')
    ALLOCATE(ZFIELD(INI,1,IPATCH))
    ZFIELD(:,1,:) = TZSNOW%ALB(:,:)
    !
    ALLOCATE(PFIELD(INI,1,IVEGTYPE))
    CALL PUT_ON_ALL_VEGTYPES(INI,1,IPATCH,IVEGTYPE,ZFIELD,PFIELD)
    DEALLOCATE(ZFIELD)
!
!-------------------------------------------------------------------------------------
!
!*      7.     Total depth to snow grid
!              ------------------------
!
  CASE ('DEP')
    ALLOCATE(ZFIELD_FINE(INI,KLAYER,IPATCH))
    IF (OSNOW_IDEAL) THEN
      ZFIELD_FINE(:,:,:) = TZSNOW%WSNOW(:,1:KLAYER,:)/TZSNOW%RHO(:,1:KLAYER,:)
      WHERE(TZSNOW%WSNOW(:,1:KLAYER,:)==XUNDEF) ZFIELD_FINE(:,:,:)=XUNDEF
    ELSE          
      ALLOCATE(ZDEPTH(INI,TZSNOW%NLAYER,IPATCH))
      ZDEPTH(:,:,:) = TZSNOW%WSNOW(:,:,:)/TZSNOW%RHO(:,:,:)
      WHERE(TZSNOW%WSNOW(:,:,:)==XUNDEF) ZDEPTH(:,:,:)=XUNDEF
      IF(TZSNOW%NLAYER/=KLAYER)THEN
        !* total depth
        ALLOCATE(ZD(INI,IPATCH))
        ZD(:,:) = 0.0
        DO JPATCH=1,IPATCH
           DO JLAYER=1,TZSNOW%NLAYER
              DO JI=1,INI
                 IF(ZDEPTH(JI,JLAYER,JPATCH)/=XUNDEF)THEN
                   ZD(JI,JPATCH) = ZD(JI,JPATCH) + ZDEPTH(JI,JLAYER,JPATCH)
                 ENDIF
              ENDDO
           ENDDO
        ENDDO
        !* fine grid
        DO JPATCH=1,IPATCH
           CALL SNOW3LGRID(ZFIELD_FINE(:,:,JPATCH),ZD(:,JPATCH))
        ENDDO  
        DEALLOCATE(ZD)
      ELSE
        ZFIELD_FINE(:,:,:)=ZDEPTH(:,:,:)
      ENDIF
      DEALLOCATE(ZDEPTH)
    ENDIF
    ALLOCATE(PFIELD(INI,KLAYER,IVEGTYPE))
    CALL PUT_ON_ALL_VEGTYPES(INI,KLAYER,IPATCH,IVEGTYPE,ZFIELD_FINE,PFIELD)
    DEALLOCATE(ZFIELD_FINE)
!
!-------------------------------------------------------------------------------------
!
!*      8.     Density or heat profile
!              -----------------------
!
  CASE ('RHO','HEA','SG1','SG2','HIS','AGE')
    ALLOCATE(ZFIELD(INI,TZSNOW%NLAYER,IPATCH))
!
    SELECT CASE (TZSNOW%SCHEME)
      CASE ('D95','1-L','EBA')
        ALLOCATE(ZFIELD_FINE(INI,NGRID_LEVEL,IPATCH))      
        !* computes output physical variable
        IF (HSURF(1:3)=='RHO') ZFIELD(:,1,:) = TZSNOW%RHO(:,1,:)
        IF (HSURF(1:3)=='HEA') THEN
          ALLOCATE(ZTEMP(INI,TZSNOW%NLAYER,IPATCH))
          ALLOCATE(ZWLIQ(INI,TZSNOW%NLAYER,IPATCH))
          IF (TZSNOW%SCHEME=='D95'.OR.TZSNOW%SCHEME=='EBA') ZTEMP(:,1,:) = XTT-2.
          IF (TZSNOW%SCHEME=='1-L') ZTEMP(:,1,:) = TZSNOW%T(:,1,:)
          ZWLIQ(:,:,:) = 0.0
          CALL SNOW_T_WLIQ_TO_HEAT(ZFIELD,TZSNOW%RHO,ZTEMP,ZWLIQ)
          DEALLOCATE(ZTEMP)
          DEALLOCATE(ZWLIQ)
        END IF
        IF (HSURF(1:3)=='SG1') ZFIELD(:,1,:) = -20.0
        IF (HSURF(1:3)=='SG2') ZFIELD(:,1,:) = 80.0
        IF (HSURF(1:3)=='HIS') ZFIELD(:,1,:) = 0.0
        IF (HSURF(1:3)=='AGE') ZFIELD(:,1,:) = 3.0
        !* put profile on fine snow grid
        DO JLAYER=1,NGRID_LEVEL
          ZFIELD_FINE(:,JLAYER,:) = ZFIELD(:,1,:)
        END DO
        ALLOCATE(PFIELD(INI,NGRID_LEVEL,IVEGTYPE))
        CALL PUT_ON_ALL_VEGTYPES(INI,NGRID_LEVEL,IPATCH,IVEGTYPE,ZFIELD_FINE,PFIELD)

      CASE ('3-L','CRO')
        !* input physical variable
        IF (HSURF(1:3)=='RHO') ZFIELD(:,:,:) = TZSNOW%RHO (:,1:TZSNOW%NLAYER,:)
        IF (HSURF(1:3)=='HEA') ZFIELD(:,:,:) = TZSNOW%HEAT(:,1:TZSNOW%NLAYER,:)
        IF (HSURF(1:3)=='AGE') ZFIELD(:,:,:) = TZSNOW%AGE (:,1:TZSNOW%NLAYER,:)
        IF (TZSNOW%SCHEME=='CRO')THEN
           IF (HSURF(1:3)=='SG1') ZFIELD(:,:,:) = TZSNOW%GRAN1(:,1:TZSNOW%NLAYER,:)
           IF (HSURF(1:3)=='SG2') ZFIELD(:,:,:) = TZSNOW%GRAN2(:,1:TZSNOW%NLAYER,:)
           IF (HSURF(1:3)=='HIS') ZFIELD(:,:,:) = TZSNOW%HIST (:,1:TZSNOW%NLAYER,:)
        ELSE
           IF (HSURF(1:3)=='SG1') ZFIELD(:,:,:) = -20.0
           IF (HSURF(1:3)=='SG2') ZFIELD(:,:,:) = 80.0
           IF (HSURF(1:3)=='HIS') ZFIELD(:,:,:) = 0.0
        ENDIF
        !
        IF (OSNOW_IDEAL) THEN
          ALLOCATE(ZFIELD_FINE(INI,KLAYER,IPATCH))
          ZFIELD_FINE(:,:,:) = ZFIELD(:,:,:)
          ALLOCATE(PFIELD(INI,KLAYER,IVEGTYPE))
          CALL PUT_ON_ALL_VEGTYPES(INI,KLAYER,IPATCH,IVEGTYPE,ZFIELD_FINE,PFIELD)
        ELSE
          ALLOCATE(ZFIELD_FINE(INI,NGRID_LEVEL,IPATCH))  
          !
          !* input snow layer thickness
          ALLOCATE(ZDEPTH(INI,TZSNOW%NLAYER,IPATCH))
          DO JPATCH=1,IPATCH
              ZDEPTH(:,:,JPATCH) = TZSNOW%WSNOW(:,:,JPATCH)/TZSNOW%RHO(:,:,JPATCH)
          END DO
          !
          !* total depth
          ALLOCATE(ZD(INI,IPATCH))
          ZD(:,:) = 0.
          DO JLAYER=1,TZSNOW%NLAYER
            ZD(:,:) = ZD(:,:) + ZDEPTH(:,JLAYER,:)
          ENDDO
          !
          !* input normalized grid
          ALLOCATE(ZGRID(INI,TZSNOW%NLAYER,IPATCH))
          DO JPATCH=1,IPATCH
             DO JI=1,INI
                IF(ZD(JI,JPATCH)==0.0)THEN
                   DO JLAYER = 1,TZSNOW%NLAYER
                      ZGRID(JI,JLAYER,JPATCH)=REAL(JLAYER)/REAL(TZSNOW%NLAYER)
                   ENDDO
                ELSE
                   DO JLAYER = 1,TZSNOW%NLAYER
                      IF(JLAYER==1)THEN
                        ZGRID(JI,JLAYER,JPATCH)=ZDEPTH(JI,JLAYER,JPATCH)/ ZD(JI,JPATCH)
                      ELSE
                        ZGRID(JI,JLAYER,JPATCH) = ZGRID(JI,JLAYER-1,JPATCH) + ZDEPTH(JI,JLAYER,JPATCH)/ZD(JI,JPATCH)
                      ENDIF
                   ENDDO
                ENDIF
             ENDDO
          ENDDO
          DEALLOCATE(ZDEPTH)
          DEALLOCATE(ZD)
          !    
          !* interpolation of profile onto fine normalized snow grid
          DO JPATCH=1,IPATCH
            CALL INTERP_GRID_NAT(ZGRID(:,:,JPATCH),ZFIELD(:,:,JPATCH),    &
                             XGRID_SNOW(:), ZFIELD_FINE(:,:,JPATCH))
          END DO
          DEALLOCATE(ZGRID)
          ALLOCATE(PFIELD(INI,NGRID_LEVEL,IVEGTYPE))
          CALL PUT_ON_ALL_VEGTYPES(INI,NGRID_LEVEL,IPATCH,IVEGTYPE,ZFIELD_FINE,PFIELD)
        ENDIF
      END SELECT
    !
    !* put field form patch to all vegtypes
    DEALLOCATE(ZFIELD)
    DEALLOCATE(ZFIELD_FINE)
!
END SELECT
!
!-------------------------------------------------------------------------------------
!
!*      9.     End of IO
!              ---------
!
IF (LHOOK) CALL DR_HOOK('PREP_SNOW_EXTERN',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
END SUBROUTINE PREP_SNOW_EXTERN
