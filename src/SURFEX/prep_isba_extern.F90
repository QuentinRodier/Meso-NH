!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
SUBROUTINE PREP_ISBA_EXTERN(HPROGRAM,HSURF,HFILE,HFILETYPE,HFILEPGD,HFILEPGDTYPE,KLUOUT,PFIELD)
!     #################################################################################
!
!!****  *PREP_ISBA_EXTERN* - initializes ISBA fields from operational GRIB
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
!!        M.Moge    08/2015  reading 'WR' one patch at a time for Z-parallel splitting with MNH
!!------------------------------------------------------------------
!

!
USE MODE_READ_EXTERN
!
USE MODD_TYPE_DATE_SURF
!
USE MODI_PREP_GRID_EXTERN
USE MODI_READ_SURF
USE MODI_INTERP_GRID
USE MODI_OPEN_AUX_IO_SURF
USE MODI_CLOSE_AUX_IO_SURF
!
USE MODD_PREP,           ONLY : CINGRID_TYPE, CINTERP_TYPE
USE MODD_PREP_ISBA,      ONLY : XGRID_SOIL, XWR_DEF
USE MODD_DATA_COVER_PAR, ONLY : NVEGTYPE
USE MODD_SURF_PAR,       ONLY : XUNDEF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_PUT_ON_ALL_VEGTYPES
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
 CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
 CHARACTER(LEN=7),   INTENT(IN)  :: HSURF     ! type of field
 CHARACTER(LEN=28),  INTENT(IN)  :: HFILE     ! name of file
 CHARACTER(LEN=6),   INTENT(IN)  :: HFILETYPE ! type of input file
 CHARACTER(LEN=28),  INTENT(IN)  :: HFILEPGD     ! name of file
 CHARACTER(LEN=6),   INTENT(IN)  :: HFILEPGDTYPE ! type of input file
INTEGER,            INTENT(IN)  :: KLUOUT    ! logical unit of output listing
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
REAL, DIMENSION(:,:), POINTER     :: ZD1            ! depth of field in the soil, one patch
REAL, DIMENSION(:,:), ALLOCATABLE   :: ZOUT         !
INTEGER                             :: JPATCH, JVEGTYPE        ! loop counter for patch
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
IF (LHOOK) CALL DR_HOOK('PREP_ISBA_EXTERN',0,ZHOOK_HANDLE)
!
!------------------------------------------------------------------------------
!
!*      2.     Reading of grid
!              ---------------
!
 CALL OPEN_AUX_IO_SURF(HFILEPGD,HFILEPGDTYPE,'NATURE')
!
 CALL PREP_GRID_EXTERN(HFILEPGDTYPE,KLUOUT,CINGRID_TYPE,CINTERP_TYPE,INI)
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
    CALL READ_SURF(HFILEPGDTYPE,YRECFM,PFIELD(:,1,1),IRESP,HDIR='A')
    CALL CLOSE_AUX_IO_SURF(HFILEPGD,HFILEPGDTYPE)
!
!--------------------------------------------------------------------------
!
!
!*      3.1    Profile of temperature, water or ice in the soil
!
  CASE('TG    ','WG    ','WGI   ')
     CALL CLOSE_AUX_IO_SURF(HFILEPGD,HFILEPGDTYPE)
!* reading of the profile and its depth definition
     CALL READ_EXTERN_ISBA(HFILE,HFILETYPE,HFILEPGD,HFILEPGDTYPE,&
                           KLUOUT,INI,HSURF,HSURF,ZFIELD,ZD)
! 
     ALLOCATE(ZFIELD1(SIZE(ZFIELD,1),SIZE(ZFIELD,2)))
     ALLOCATE(ZD1(SIZE(ZFIELD,1),SIZE(ZFIELD,2)))
     ALLOCATE(ZOUT(SIZE(ZFIELD,1),SIZE(XGRID_SOIL)))
     ALLOCATE(PFIELD(SIZE(ZFIELD,1),SIZE(XGRID_SOIL),SIZE(ZFIELD,3)))
!
     DO JVEGTYPE=1,SIZE(ZFIELD,3)
        ZFIELD1(:,:)=ZFIELD(:,:,JVEGTYPE)
        ZD1(:,:)=ZD(:,:,JVEGTYPE)
        CALL INTERP_GRID(ZD1,ZFIELD1,XGRID_SOIL,ZOUT)
        PFIELD(:,:,JVEGTYPE)=ZOUT(:,:)
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
     !* number of tiles
     YRECFM='PATCH_NUMBER'
     CALL READ_SURF(HFILEPGDTYPE,YRECFM,IPATCH,IRESP)
     CALL CLOSE_AUX_IO_SURF(HFILEPGD,HFILEPGDTYPE)
     ALLOCATE(ZFIELD(INI,1,IPATCH))
     YRECFM = 'WR'
     CALL OPEN_AUX_IO_SURF(HFILE,HFILETYPE,'NATURE')
#ifdef MNH_PARALLEL
     DO JPATCH=1,IPATCH
       WRITE(YRECFM,'(A2,I4.4)') 'WR',JPATCH
       CALL READ_SURF(HFILETYPE,YRECFM,ZFIELD(:,1,JPATCH),IRESP,HDIR='A')
     END DO
#else
     CALL READ_SURF(HFILETYPE,YRECFM,ZFIELD(:,1,:),IRESP,HDIR='A')
#endif
     CALL CLOSE_AUX_IO_SURF(HFILE,HFILETYPE)
     CALL PUT_ON_ALL_VEGTYPES(INI,1,IPATCH,NVEGTYPE,ZFIELD,PFIELD)
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
IF (LHOOK) CALL DR_HOOK('PREP_ISBA_EXTERN',1,ZHOOK_HANDLE)
!
!---------------------------------------------------------------------------
!---------------------------------------------------------------------------
END SUBROUTINE PREP_ISBA_EXTERN
