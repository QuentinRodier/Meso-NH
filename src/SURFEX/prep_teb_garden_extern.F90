!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE PREP_TEB_GARDEN_EXTERN (DTCO, I, U,GCP, &
                                   HPROGRAM,HSURF,HFILE,HFILETYPE,HFILEPGD,HFILEPGDTYPE,KLUOUT,KPATCH,PFIELD)
!     #################################################################################
!
!!****  *PREP_TEB_GARDEN_EXTERN* - initializes ISBA fields from operational GRIB
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
!!      M.Moge    01/2016  using READ_SURF_FIELD2D/3D for 2D/3D surfex fields reads
!!------------------------------------------------------------------
!

!
!
!
!
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_ISBA_n, ONLY : ISBA_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_GRID_CONF_PROJ, ONLY : GRID_CONF_PROJ_t
!
USE MODE_READ_EXTERN
!
USE MODD_TYPE_DATE_SURF
!
USE MODI_PREP_GRID_EXTERN
USE MODI_READ_SURF
USE MODI_INTERP_GRID_NAT
USE MODI_OPEN_AUX_IO_SURF
USE MODI_CLOSE_AUX_IO_SURF
USE MODI_READ_TEB_PATCH
USE MODI_TOWN_PRESENCE
!
USE MODD_PREP,           ONLY : CINGRID_TYPE, CINTERP_TYPE
USE MODD_PREP_TEB_GARDEN,ONLY : XGRID_SOIL, XWR_DEF
USE MODD_DATA_COVER_PAR, ONLY : NVEGTYPE
USE MODD_SURF_PAR,       ONLY : XUNDEF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_PUT_ON_ALL_VEGTYPES
USE MODI_READ_SURF_FIELD2D
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(ISBA_t), INTENT(INOUT) :: I
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(GRID_CONF_PROJ_t),INTENT(INOUT) :: GCP
!
 CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
 CHARACTER(LEN=7),   INTENT(IN)  :: HSURF     ! type of field
 CHARACTER(LEN=28),  INTENT(IN)  :: HFILE     ! name of file
 CHARACTER(LEN=6),   INTENT(IN)  :: HFILETYPE ! type of input file
 CHARACTER(LEN=28),  INTENT(IN)  :: HFILEPGD     ! name of file
 CHARACTER(LEN=6),   INTENT(IN)  :: HFILEPGDTYPE ! type of input file
INTEGER,            INTENT(IN)  :: KLUOUT    ! logical unit of output listing
INTEGER,            INTENT(IN)  :: KPATCH
REAL,DIMENSION(:,:,:), POINTER  :: PFIELD    ! field to interpolate horizontally (on final soil grid)
!
!*      0.2    declarations of local variables
!
 CHARACTER(LEN=12) :: YRECFM         ! Name of the article to be read
INTEGER           :: IRESP          ! reading return code
INTEGER           :: INI            ! total 1D dimension
INTEGER           :: IPATCH         ! number of patch
!
REAL, DIMENSION(:,:,:), POINTER     :: ZFIELD         ! field read on initial MNH vertical soil grid, all patches
REAL, DIMENSION(:,:),   POINTER     :: ZFIELD1        ! field read on initial MNH vertical soil grid, one patch
REAL, DIMENSION(:,:,:), POINTER     :: ZD             ! depth of field in the soil
REAL, DIMENSION(:,:), POINTER       :: ZD1            ! depth of field in the soil, one patch
REAL, DIMENSION(:,:), ALLOCATABLE   :: ZOUT           !
INTEGER                             :: JPATCH         ! loop counter for patch
INTEGER                             :: ITEB_PATCH     ! number of TEB patches in file
INTEGER                             :: IVERSION       ! SURFEX version
INTEGER                             :: IBUGFIX        ! SURFEX bug version
LOGICAL                             :: GOLD_NAME      ! old name flag for temperatures
 CHARACTER(LEN=12)                   :: YSURF     ! type of field
 CHARACTER(LEN=3)                    :: YPATCH    ! indentificator for TEB patch
LOGICAL                         :: GTEB      ! flag if TEB fields are present
LOGICAL                         :: GGARDEN   ! T if gardens are present in the file
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!------------------------------------------------------------------------------
!
!*      1.     Preparation of IO for reading in the file
!              -----------------------------------------
!
!* Note that all points are read, even those without physical meaning.
!  These points will not be used during the horizontal interpolation step.
!  Their value must be defined as XUNDEF.
!
IF (LHOOK) CALL DR_HOOK('PREP_TEB_GARDEN_EXTERN',0,ZHOOK_HANDLE)
!
!------------------------------------------------------------------------------
!
!*      2.     Reading of grid
!              ---------------
!
 CALL OPEN_AUX_IO_SURF(&
                      HFILEPGD,HFILEPGDTYPE,'FULL  ')
!
!* reading of version of the file being read
 CALL READ_SURF(&
               HFILEPGDTYPE,'VERSION',IVERSION,IRESP)
 CALL READ_SURF(&
               HFILEPGDTYPE,'BUG',IBUGFIX,IRESP)
GOLD_NAME=(IVERSION<7 .OR. (IVERSION==7 .AND. IBUGFIX<3))
!
 CALL PREP_GRID_EXTERN(GCP,&
                      HFILEPGDTYPE,KLUOUT,CINGRID_TYPE,CINTERP_TYPE,INI)
!
!* reads if TEB fields exist in the input file
 CALL TOWN_PRESENCE(&
                   HFILEPGDTYPE,GTEB)
!
IF (GTEB) THEN
  CALL CLOSE_AUX_IO_SURF(HFILEPGD,HFILEPGDTYPE)
  CALL READ_TEB_PATCH(&
                      HFILEPGD,HFILEPGDTYPE,ITEB_PATCH)
  CALL OPEN_AUX_IO_SURF(&
                      HFILEPGD,HFILEPGDTYPE,'FULL  ')  
  YPATCH='   '
  IF (ITEB_PATCH>1) THEN
    WRITE(YPATCH,FMT='(A,I1,A)') 'T',MIN(KPATCH,ITEB_PATCH),'_'
  END IF
END IF
!
!---------------------------------------------------------------------------------------
!
!*      3.     Transformation into physical quantity to be interpolated
!              --------------------------------------------------------
!
SELECT CASE(HSURF)
!
!*     3.      Orography
!              ---------
!
  CASE('ZS     ')
    ALLOCATE(PFIELD(INI,1,1))
    YRECFM='ZS'
    CALL READ_SURF(&
               HFILEPGDTYPE,YRECFM,PFIELD(:,1,1),IRESP,HDIR='A')
    CALL CLOSE_AUX_IO_SURF(HFILEPGD,HFILEPGDTYPE)
!
!--------------------------------------------------------------------------
!
!
!*      3.1    Profile of temperature, water or ice in the soil
!
  CASE('TG    ','WG    ','WGI   ')
!* choice if one reads garden fields (if present) or ISBA fields
    GGARDEN = .FALSE.
    IF (GTEB) CALL READ_SURF(&
               HFILEPGDTYPE,'GARDEN',GGARDEN,IRESP)
    CALL CLOSE_AUX_IO_SURF(HFILEPGD,HFILEPGDTYPE)
    IF (GGARDEN) THEN
      YSURF = 'GD_'//HSURF(1:3)
      IF (GOLD_NAME) YSURF = 'TWN_'//HSURF(1:3)
      YSURF = YPATCH//YSURF
    ELSE
      YSURF = HSURF
    END IF
    YSURF=ADJUSTL(YSURF)  
!* reading of the profile and its depth definition
     CALL READ_EXTERN_ISBA(U, &
                           DTCO, I,GCP, &
                           HFILE,HFILETYPE,HFILEPGD,HFILEPGDTYPE,KLUOUT,INI,&
                HSURF,YSURF,ZFIELD,ZD)
! 
     ALLOCATE(ZFIELD1(SIZE(ZFIELD,1),SIZE(ZFIELD,2)))
     ALLOCATE(ZD1(SIZE(ZFIELD,1),SIZE(ZFIELD,2)))
     ALLOCATE(ZOUT(SIZE(ZFIELD,1),SIZE(XGRID_SOIL)))
     ALLOCATE(PFIELD(SIZE(ZFIELD,1),SIZE(XGRID_SOIL),SIZE(ZFIELD,3)))
!
     DO JPATCH=1,SIZE(ZFIELD,3)
        ZFIELD1(:,:)=ZFIELD(:,:,JPATCH)
        ZD1(:,:)=ZD(:,:,JPATCH)
        CALL INTERP_GRID_NAT(ZD1,ZFIELD1,XGRID_SOIL,ZOUT)
        PFIELD(:,:,JPATCH)=ZOUT(:,:)
     END DO
!
     DEALLOCATE(ZFIELD)
     DEALLOCATE(ZOUT)
     DEALLOCATE(ZFIELD1)
     DEALLOCATE(ZD)
!
!--------------------------------------------------------------------------
!
!*      3.4    Water content intercepted on leaves, LAI
!
  CASE('WR     ')
     ALLOCATE(PFIELD(INI,1,NVEGTYPE))
     !* choice if one reads garden fields (if present) or ISBA fields    
     GGARDEN = .FALSE.
     IF (GTEB) CALL READ_SURF(&
               HFILEPGDTYPE,'GARDEN',GGARDEN,IRESP)
     CALL CLOSE_AUX_IO_SURF(HFILEPGD,HFILEPGDTYPE)
     IF (GGARDEN) THEN
       IPATCH = 1    
       YRECFM = 'GD_WR'
       IF (GOLD_NAME) YRECFM = 'TWN_WR'
       YRECFM = YPATCH//YRECFM
       CALL OPEN_AUX_IO_SURF(&
                      HFILE,HFILETYPE,'TOWN  ')
     ELSE            
       YRECFM = 'PATCH_NUMBER'
       CALL OPEN_AUX_IO_SURF(&
                      HFILEPGD,HFILEPGDTYPE,'NATURE')
       CALL READ_SURF(&
               HFILEPGDTYPE,YRECFM,IPATCH,IRESP)
       CALL CLOSE_AUX_IO_SURF(HFILEPGD,HFILEPGDTYPE)
       CALL OPEN_AUX_IO_SURF(&
                      HFILE,HFILETYPE,'NATURE')
       YRECFM = 'WR'
     END IF
     YRECFM=ADJUSTL(YRECFM)
     
     ALLOCATE(ZFIELD(INI,1,IPATCH))
     CALL READ_SURF_FIELD2D(&
               HFILETYPE,ZFIELD(:,1,:),YRECFM,HDIR='A')
     CALL CLOSE_AUX_IO_SURF(HFILE,HFILETYPE)
     CALL PUT_ON_ALL_VEGTYPES(INI,1,1,NVEGTYPE,ZFIELD,PFIELD)
     DEALLOCATE(ZFIELD)
!
  CASE('LAI    ')
     CALL CLOSE_AUX_IO_SURF(HFILEPGD,HFILEPGDTYPE)
     ALLOCATE(PFIELD(INI,1,NVEGTYPE))
     PFIELD(:,:,:) = XUNDEF
!
END SELECT
!
!
!---------------------------------------------------------------------------
!
!*      6.     End of IO
!              ---------
!
IF (LHOOK) CALL DR_HOOK('PREP_TEB_GARDEN_EXTERN',1,ZHOOK_HANDLE)
!
!---------------------------------------------------------------------------
!---------------------------------------------------------------------------
END SUBROUTINE PREP_TEB_GARDEN_EXTERN
