!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
SUBROUTINE PREP_SNOW_EXTERN(HPROGRAM,HSURF,HFILE,HFILETYPE,HFILEPGD,HFILEPGDTYPE,&
                            KLUOUT,PFIELD,OSNOW_IDEAL,KLAYER)

!!     ##########################################################################
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
!!	   * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    ?
!!       02/2014 E. Martin : cor. for passing from from multilayer to a single layer
!-------------------------------------------------------------------------------
!
!
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
USE MODI_INTERP_GRID
USE MODI_READ_GR_SNOW
USE MODI_READ_SURF
USE MODI_SNOW_T_WLIQ_TO_HEAT
USE MODI_GET_CURRENT_TEB_PATCH
USE MODI_READ_TEB_PATCH
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
 CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
 CHARACTER(LEN=10),  INTENT(IN)  :: HSURF     ! type of field
 CHARACTER(LEN=28),  INTENT(IN)  :: HFILE     ! name of file
 CHARACTER(LEN=6),   INTENT(IN)  :: HFILETYPE ! type of file
 CHARACTER(LEN=28),  INTENT(IN)  :: HFILEPGD     ! name of file
 CHARACTER(LEN=6),   INTENT(IN)  :: HFILEPGDTYPE ! type of file
INTEGER,            INTENT(IN)  :: KLUOUT    ! logical unit of output listing
REAL,DIMENSION(:,:,:), POINTER  :: PFIELD    ! field to interpolate horizontally
LOGICAL,            INTENT(IN)  :: OSNOW_IDEAL
INTEGER,            INTENT(IN)  :: KLAYER
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
INTEGER                           :: INI
 CHARACTER(LEN=8)                  :: YAREA          ! area treated ('ROOF','ROAD','VEG ')
 CHARACTER(LEN=3)                  :: YPREFIX        ! prefix to identify patch
INTEGER                           :: IPATCH         ! number of input patch
INTEGER                           :: ITEB_PATCH     ! number of input patch for TEB
INTEGER                           :: ICURRENT_TEB_PATCH ! current patch for TEB
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
 CALL OPEN_AUX_IO_SURF(HFILEPGD,HFILEPGDTYPE,YMASK)
!
!* reading of version of the file being read
 CALL READ_SURF(HFILEPGDTYPE,'VERSION',IVERSION,IRESP)
 CALL READ_SURF(HFILEPGDTYPE,'BUG',IBUGFIX,IRESP)
GOLD_NAME=(IVERSION<7 .OR. (IVERSION==7 .AND. IBUGFIX<3))
!
IF (YAREA(1:4)=='VEG ') THEN
  YRECFM = 'PATCH_NUMBER'
  CALL READ_SURF(HFILEPGDTYPE,YRECFM,IPATCH,IRESP)
ELSE
  IF (.NOT.GOLD_NAME) THEN
     IF (YAREA(1:4)=='ROOF') YAREA(1:4) = 'RF  '
     IF (YAREA(1:4)=='ROAD') YAREA(1:4) = 'RD  '
   ENDIF
  CALL READ_TEB_PATCH(HFILEPGDTYPE,ITEB_PATCH)
  IF (ITEB_PATCH>1) THEN
    CALL GET_CURRENT_TEB_PATCH(ICURRENT_TEB_PATCH)
    WRITE(YPREFIX,FMT='(A,I1,A)') 'T',MIN(ICURRENT_TEB_PATCH,ITEB_PATCH),'_'
  END IF  
END IF
!
!-------------------------------------------------------------------------------------
!
!*      2.     Reading of grid
!              ---------------
!
 CALL PREP_GRID_EXTERN(HFILEPGDTYPE,KLUOUT,CINGRID_TYPE,CINTERP_TYPE,INI)
!
!-------------------------------------------------------------------------------------
!
!*      4.     Reading of snow data
!              ---------------------
!
IF (YAREA(1:2)=='RO' .OR. YAREA(1:2)=='GA' .OR. YAREA(1:2)=='RF' .OR. YAREA(1:2)=='RD') THEN
  CALL TOWN_PRESENCE(HFILEPGDTYPE,GTOWN)
  CALL CLOSE_AUX_IO_SURF(HFILEPGD,HFILEPGDTYPE)
  IF (.NOT. GTOWN) THEN
    TZSNOW%SCHEME='1-L'
    CALL ALLOCATE_GR_SNOW(TZSNOW,INI,IPATCH)
  ELSE
    CALL OPEN_AUX_IO_SURF(HFILE,HFILETYPE,YMASK)
    CALL READ_GR_SNOW(HFILETYPE,TRIM(YAREA),YPREFIX,INI,IPATCH,TZSNOW,HDIR='A')
    CALL CLOSE_AUX_IO_SURF(HFILE,HFILETYPE)
  ENDIF
ELSE
  CALL CLOSE_AUX_IO_SURF(HFILEPGD,HFILEPGDTYPE)
  CALL OPEN_AUX_IO_SURF(HFILE,HFILETYPE,YMASK)
  CALL READ_GR_SNOW(HFILETYPE,TRIM(YAREA),YPREFIX,INI,IPATCH,TZSNOW,HDIR='A')
  CALL CLOSE_AUX_IO_SURF(HFILE,HFILETYPE)
ENDIF
!
IF (TZSNOW%NLAYER.LT.KLAYER) THEN
  CALL ABOR1_SFX("PREP_SNOW_EXTERN: SNOW NLAYER IN EXTERN FILE MUST BE HIGHER THAN CURRENT NLAYER")
ENDIF
!
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
      ZFIELD = 0.
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
!*      7.     Total depth
!              -----------
!
  CASE ('DEP')
    IF (OSNOW_IDEAL) THEN
      ALLOCATE(ZDEPTH(INI,KLAYER,IPATCH))
      ZDEPTH(:,:,:) = TZSNOW%WSNOW(:,1:KLAYER,:)/TZSNOW%RHO(:,1:KLAYER,:)
      WHERE(TZSNOW%WSNOW(:,1:KLAYER,:)==XUNDEF) ZDEPTH(:,:,:)=XUNDEF
      ALLOCATE(PFIELD(INI,KLAYER,IVEGTYPE))
      CALL PUT_ON_ALL_VEGTYPES(INI,KLAYER,IPATCH,IVEGTYPE,ZDEPTH,PFIELD)
    ELSE
      ALLOCATE(ZDEPTH(INI,1,IPATCH))
      ZDEPTH = 0.
      DO JPATCH=1,IPATCH
        DO JLAYER=1,TZSNOW%NLAYER
          ZDEPTH(:,1,JPATCH) = ZDEPTH(:,1,JPATCH) + &
                               TZSNOW%WSNOW(:,JLAYER,JPATCH)/TZSNOW%RHO(:,JLAYER,JPATCH)
        END DO
        WHERE(TZSNOW%WSNOW(:,1,JPATCH)==XUNDEF) ZDEPTH(:,1,JPATCH)=XUNDEF
      END DO
      ALLOCATE(PFIELD(INI,1,IVEGTYPE))
      CALL PUT_ON_ALL_VEGTYPES(INI,1,IPATCH,IVEGTYPE,ZDEPTH,PFIELD)
    ENDIF
    DEALLOCATE(ZDEPTH)
!
!-------------------------------------------------------------------------------------
!
!*      8.     Density or heat profile
!              -----------------------
!
  CASE ('RHO','HEA')
    ALLOCATE(ZFIELD     (INI,TZSNOW%NLAYER,IPATCH))

    SELECT CASE (TZSNOW%SCHEME)
      CASE ('D95','1-L','EBA')
        ALLOCATE(ZFIELD_FINE(INI,NGRID_LEVEL,IPATCH))      
        !* computes output physical variable
        IF (HSURF(1:1)=='R') ZFIELD(:,1,:) = TZSNOW%RHO(:,1,:)
        IF (HSURF(1:1)=='H') THEN
          ALLOCATE(ZTEMP(INI,TZSNOW%NLAYER,IPATCH))
          ALLOCATE(ZWLIQ(INI,TZSNOW%NLAYER,IPATCH))
          IF (TZSNOW%SCHEME=='D95'.OR.TZSNOW%SCHEME=='EBA') ZTEMP(:,1,:) = XTT
          IF (TZSNOW%SCHEME=='1-L') ZTEMP(:,1,:) = TZSNOW%T(:,1,:)
          ZWLIQ(:,:,:) = 0.
          CALL SNOW_T_WLIQ_TO_HEAT(ZFIELD,TZSNOW%RHO,ZTEMP,ZWLIQ)
          DEALLOCATE(ZTEMP)
          DEALLOCATE(ZWLIQ)
        END IF
        !* put profile on fine snow grid
        DO JLAYER=1,NGRID_LEVEL
          ZFIELD_FINE(:,JLAYER,:) = ZFIELD(:,1,:)
        END DO
        ALLOCATE(PFIELD(INI,NGRID_LEVEL,IVEGTYPE))
        CALL PUT_ON_ALL_VEGTYPES(INI,NGRID_LEVEL,IPATCH,IVEGTYPE,ZFIELD_FINE,PFIELD)

      CASE ('3-L','CRO')
        !* input physical variable
        IF (HSURF(1:1)=='R') ZFIELD(:,:,:) = TZSNOW%RHO (:,1:TZSNOW%NLAYER,:)
        IF (HSURF(1:1)=='H') ZFIELD(:,:,:) = TZSNOW%HEAT(:,1:TZSNOW%NLAYER,:)
        !
        IF (OSNOW_IDEAL) THEN
          ALLOCATE(ZFIELD_FINE(INI,KLAYER,IPATCH))
          IF (HSURF(1:1)=='R') ZFIELD_FINE(:,:,:) = ZFIELD(:,:,:)
          IF (HSURF(1:1)=='H') ZFIELD_FINE(:,:,:) = ZFIELD(:,:,:)
          ALLOCATE(PFIELD(INI,KLAYER,IVEGTYPE))
          CALL PUT_ON_ALL_VEGTYPES(INI,KLAYER,IPATCH,IVEGTYPE,ZFIELD_FINE,PFIELD)
        ELSE
          ALLOCATE(ZFIELD_FINE(INI,NGRID_LEVEL,IPATCH))      
          !* total depth
          ALLOCATE(ZD(INI,IPATCH))
          ZD = 0.
          DO JLAYER=1,TZSNOW%NLAYER
            ZD(:,:) = ZD(:,:) + TZSNOW%WSNOW(:,JLAYER,:)/TZSNOW%RHO(:,JLAYER,:)
          END DO
          !* input snow layer thickness
          ALLOCATE(ZDEPTH(INI,TZSNOW%NLAYER,IPATCH))
          DO JPATCH=1,IPATCH
            IF (KLAYER>=3) THEN
              CALL SNOW3LGRID(ZDEPTH(:,:,JPATCH),ZD(:,JPATCH))
            ELSEIF (KLAYER==1) THEN
              ZDEPTH(:,1,JPATCH) = ZD(:,JPATCH)
            ENDIF
          END DO
          !* input normalized grid
          ALLOCATE(ZGRID(INI,TZSNOW%NLAYER,IPATCH))
          WHERE(ZD(:,:)>0.)
            ZGRID(:,1,:) = 0.5 * ZDEPTH(:,1,:) / ZD(:,:)
          ELSEWHERE
            ZGRID(:,1,:) = 0.5 / FLOAT(KLAYER)
          END WHERE
          DO JLAYER = 2,TZSNOW%NLAYER
            WHERE (ZD(:,:)>0.)
              ZGRID(:,JLAYER,:) = ZGRID(:,JLAYER-1,:)                     &
                                 + 0.5 * ZDEPTH(:,JLAYER-1,:) / ZD(:,:)   &
                                 + 0.5 * ZDEPTH(:,JLAYER  ,:) / ZD(:,:)
            ELSEWHERE
              ZGRID(:,JLAYER,:) = (FLOAT(JLAYER)-0.5) / FLOAT(KLAYER)
            END WHERE
          END DO
          DEALLOCATE(ZD)
          DEALLOCATE(ZDEPTH)
          !* interpolation of profile onto fine normalized snow grid
          DO JPATCH=1,IPATCH
            CALL INTERP_GRID(ZGRID(:,:,JPATCH),ZFIELD(:,:,JPATCH),    &
                             XGRID_SNOW(:),    ZFIELD_FINE(:,:,JPATCH))
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
!*      9.     Crocus specific fields
!              -----------------------
!
  CASE ('SG1','SG2','HIS','AGE')
    ALLOCATE(ZFIELD(INI,KLAYER,IPATCH))
    IF (HSURF(1:3)=='SG1') ZFIELD(:,:,:) = TZSNOW%GRAN1(:,1:KLAYER,:)
    IF (HSURF(1:3)=='SG2') ZFIELD(:,:,:) = TZSNOW%GRAN2(:,1:KLAYER,:)
    IF (HSURF(1:3)=='HIS') ZFIELD(:,:,:) = TZSNOW%HIST(:,1:KLAYER,:)
    IF (HSURF(1:3)=='AGE') ZFIELD(:,:,:) = TZSNOW%AGE(:,1:KLAYER,:)
    ALLOCATE(PFIELD(INI,KLAYER,IVEGTYPE))
    CALL PUT_ON_ALL_VEGTYPES(INI,KLAYER,IPATCH,IVEGTYPE,ZFIELD,PFIELD)
    DEALLOCATE(ZFIELD)
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
