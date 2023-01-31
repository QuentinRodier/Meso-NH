!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!#########
SUBROUTINE SFX_OASIS_PREP (IO, S, UG, U, HPROGRAM, KNPTS, KPARAL)
!###################################################
!
!!****  *SFX_OASIS_PREP* - Prepare grid areas and mask file for SFX-OASIS coupling
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!      B. Decharme   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    10/2013
!!    10/2016 B. Decharme : bug surface/groundwater coupling 
!!    10/2016 S. Senesi : Good traitement for 2D structured grid   
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
USE MODD_ISBA_n, ONLY : ISBA_S_t
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
!
USE MODD_SURF_PAR,       ONLY : XUNDEF
!
USE MODD_SURFEX_MPI, ONLY : NRANK, NPIO, NPROC
!
USE MODN_SFX_OASIS
USE MODD_SFX_OASIS
!
USE MODI_GATHER_AND_WRITE_MPI
USE MODI_GET_LUOUT
USE MODI_ABOR1_SFX
USE MODI_GET_MESH_CORNER
USE MODI_UNPACK_SAME_RANK
USE MODI_SFX_OASIS_CHECK
!
USE MODE_GRIDTYPE_CARTESIAN
USE MODE_GRIDTYPE_CONF_PROJ
USE MODE_GRIDTYPE_IGN
USE MODE_GRIDTYPE_LONLAT_REG
USE MODE_GRIDTYPE_LONLAT_ROT
!
#ifdef CPLOASIS
USE MOD_OASIS
#endif
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
TYPE(ISBA_OPTIONS_t), INTENT(INOUT) :: IO
TYPE(ISBA_S_t), INTENT(INOUT) :: S
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
!
CHARACTER(LEN=6),        INTENT(IN) :: HPROGRAM    ! program calling surf. schemes
INTEGER,                 INTENT(IN) :: KNPTS  ! Number of grid point on this proc
INTEGER, DIMENSION(:),   INTENT(IN) :: KPARAL
!
!*       0.2   Declarations of local parameter
!              -------------------------------
!
INTEGER,           PARAMETER  :: INC = 4    ! Number of grid-cell corners
!
CHARACTER(LEN=4),  PARAMETER  :: YSFX_LAND = 'slan'
CHARACTER(LEN=4),  PARAMETER  :: YSFX_SEA  = 'ssea'
CHARACTER(LEN=4),  PARAMETER  :: YSFX_LAKE = 'slak'
!
!*       0.3   Declarations of local variables
!              -------------------------------
!
REAL,    DIMENSION(U%NSIZE_FULL)       :: ZMASK_LAND ! land-sea mask for rrm coupling
REAL,    DIMENSION(U%NSIZE_FULL)       :: ZMASK_LAKE ! lake mask for ogcm coupling
REAL,    DIMENSION(U%NSIZE_FULL)       :: ZMASK_SEA  ! sea-land mask for ogcm coupling
!
REAL,    DIMENSION(U%NSIZE_FULL,1)     :: ZLON
REAL,    DIMENSION(U%NSIZE_FULL,1)     :: ZLAT
REAL,    DIMENSION(U%NSIZE_FULL,1)     :: ZAREA
INTEGER, DIMENSION(U%NSIZE_FULL,1)     :: IMASK
!
REAL,    DIMENSION(U%NSIZE_FULL,1,INC) :: ZCORNER_LON
REAL,    DIMENSION(U%NSIZE_FULL,1,INC) :: ZCORNER_LAT
!
REAL,    DIMENSION(U%NDIM_FULL)       :: ZMASK_LAND_TOT ! land-sea mask for rrm coupling
REAL,    DIMENSION(U%NDIM_FULL)       :: ZMASK_LAKE_TOT ! lake mask for ogcm coupling
REAL,    DIMENSION(U%NDIM_FULL)       :: ZMASK_SEA_TOT  ! sea-land mask for ogcm coupling
!
REAL,    DIMENSION(U%NDIM_FULL,1)     :: ZLON_TOT
REAL,    DIMENSION(U%NDIM_FULL,1)     :: ZLAT_TOT
REAL,    DIMENSION(U%NDIM_FULL,1)     :: ZAREA_TOT
INTEGER, DIMENSION(U%NDIM_FULL,1)     :: IMASK_TOT
!
REAL,    DIMENSION(U%NDIM_FULL,1,INC) :: ZCORNER_LON_TOT
REAL,    DIMENSION(U%NDIM_FULL,1,INC) :: ZCORNER_LAT_TOT
!
REAL,    DIMENSION(:,:), ALLOCATABLE     :: ZLON_2D, ZLAT_2D, ZAREA_2D
INTEGER, DIMENSION(:,:), ALLOCATABLE     :: IMASK_2D
!
REAL,    DIMENSION(:,:,:), ALLOCATABLE :: ZCORNER_LON_2D, ZCORNER_LAT_2D
!
INTEGER, DIMENSION(2)          :: IVAR_SHAPE  ! indexes for the coupling field local dimension
!
INTEGER                        :: IPART_ID ! Local partition ID
INTEGER                        :: IERR     ! Error info
!
INTEGER                        :: ILUOUT, IFLAG
!
INTEGER                        :: JI, JC, IX, IY
!
LOGICAL                        :: G2D_GRID
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('SFX_OASIS_PREP',0,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
#ifdef CPLOASIS
!-------------------------------------------------------------------------------
!
!
!*       0.     Initialize :
!               ------------
!
CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
CALL SFX_OASIS_CHECK(IO, U, ILUOUT)
!
!-------------------------------------------------------------------------------
!
!*       1.     Define parallel partitions:
!               ---------------------------
!
CALL OASIS_DEF_PARTITION(IPART_ID,KPARAL(:),IERR)
!
IF(IERR/=OASIS_OK)THEN
   WRITE(ILUOUT,*)'SFX_OASIS_PREP: OASIS def partition problem, err = ',IERR
   CALL ABOR1_SFX('SFX_OASIS_PREP: OASIS def partition problem')
ENDIF
!
!-------------------------------------------------------------------------------
!
!*       2.     Get grid definition :
!               ---------------------
!
CALL GET_MESH_CORNER(UG, ILUOUT,ZCORNER_LAT(:,1,:),ZCORNER_LON(:,1,:))
!
ZLON(:,1)=UG%G%XLON(:)
ZLAT(:,1)=UG%G%XLAT(:)
!
!-------------------------------------------------------------------------------
!
!*       3.     Comput masks :
!               --------------
!
ZMASK_LAND(:) = U%XNATURE(:)+U%XTOWN(:)
ZMASK_SEA (:) = U%XSEA   (:)
IF(U%CWATER=='FLAKE ')THEN
  ZMASK_LAKE(:) = U%XWATER (:)
ELSE
  ZMASK_LAKE(:) = XUNDEF
ENDIF
IF(LCPL_SEA.AND.LWATER)THEN
  ZMASK_SEA (:) = U%XSEA (:)+U%XWATER(:)
ENDIF
!
!-------------------------------------------------------------------------------
!
!*       4.     Grid definition :
!               -----------------
!
G2D_GRID=.FALSE.
IF (UG%G%CGRID=='CONF PROJ' .OR. &
    UG%G%CGRID=='CARTESIAN' .OR. &
    UG%G%CGRID=='IGN'       .OR. &
    UG%G%CGRID=='LONLAT REG'.OR. &
    UG%G%CGRID=='LONLAT ROT') THEN
!
    G2D_GRID=.TRUE.
    IF(NPROC>1)THEN
!     Check PARALLELIZation
      WRITE(ILUOUT,*)'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
      WRITE(ILUOUT,*)'SFX_OASIS_PREP: 2D structured grid is PARALLELIZED but perhaps the resulting oasis files are wrong'
      WRITE(ILUOUT,*)'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!'
    ENDIF
!
ENDIF
!
IF(G2D_GRID.AND.NRANK==NPIO)THEN
!
   SELECT CASE (UG%G%CGRID)
     CASE('CARTESIAN ')
       CALL GET_GRIDTYPE_CARTESIAN(UG%XGRID_FULL_PAR,KIMAX=IX,KJMAX=IY)
     CASE('CONF PROJ ')
       CALL GET_GRIDTYPE_CONF_PROJ(UG%XGRID_FULL_PAR,KIMAX=IX,KJMAX=IY)
     CASE('IGN       ')
       CALL GET_GRIDTYPE_IGN(UG%XGRID_FULL_PAR,KDIMX=IX,KDIMY=IY)
     CASE('LONLAT REG')
       CALL GET_GRIDTYPE_LONLAT_REG(UG%XGRID_FULL_PAR,KLON=IX,KLAT=IY)
     CASE('LONLAT ROT')
       CALL GET_GRIDTYPE_LONLAT_ROT(UG%XGRID_FULL_PAR,KLON=IX,KLAT=IY)
   END SELECT
!
   ALLOCATE(ZLON_2D(IX,IY))
   ALLOCATE(ZLAT_2D(IX,IY))
   ALLOCATE(ZAREA_2D(IX,IY))
   ALLOCATE(IMASK_2D(IX,IY))
   ALLOCATE(ZCORNER_LON_2D(IX,IY,INC))
   ALLOCATE(ZCORNER_LAT_2D(IX,IY,INC))
!
ENDIF
!
!
IF(NRANK==NPIO)THEN
  CALL OASIS_START_GRIDS_WRITING(IFLAG)
ENDIF
!
!*       4.1    Grid definition for Land surface :
!               ----------------------------------
!
IF(LCPL_LAND)THEN  
!
  ZAREA(:,1) = UG%G%XMESH_SIZE(:) * ZMASK_LAND(:)
! 0 = not masked ; 1 = masked
  WHERE(ZAREA(:,1)>0.0)
        IMASK(:,1) = 0
  ELSEWHERE
        IMASK(:,1) = 1
  ENDWHERE
!
  CALL GATHER_AND_WRITE_MPI(ZLON,ZLON_TOT)
  CALL GATHER_AND_WRITE_MPI(ZLAT,ZLAT_TOT)
  CALL GATHER_AND_WRITE_MPI(ZCORNER_LON,ZCORNER_LON_TOT)
  CALL GATHER_AND_WRITE_MPI(ZCORNER_LAT,ZCORNER_LAT_TOT)
  CALL GATHER_AND_WRITE_MPI(ZAREA,ZAREA_TOT)
  CALL GATHER_AND_WRITE_MPI(IMASK,IMASK_TOT)
!
  IF(G2D_GRID.AND.NRANK==NPIO)THEN
!
    ZLON_2D(:,:) = RESHAPE(ZLON_TOT, (/IX,IY/))
    ZLAT_2D(:,:) = RESHAPE(ZLAT_TOT, (/IX,IY/))
    ZCORNER_LON_2D(:,:,:) = RESHAPE(ZCORNER_LON_TOT, (/IX,IY,INC/))
    ZCORNER_LAT_2D(:,:,:) = RESHAPE(ZCORNER_LAT_TOT, (/IX,IY,INC/))
    IMASK_2D(:,:) = RESHAPE(IMASK_TOT, (/IX,IY/))
    ZAREA_2D(:,:) = RESHAPE(ZAREA_TOT, (/IX,IY/))
!
    CALL OASIS_WRITE_GRID  (YSFX_LAND,IX,IY,ZLON_2D(:,:),ZLAT_2D(:,:))  
    CALL OASIS_WRITE_CORNER(YSFX_LAND,IX,IY,INC,ZCORNER_LON_2D(:,:,:),ZCORNER_LAT_2D(:,:,:))
    CALL OASIS_WRITE_AREA  (YSFX_LAND,IX,IY,ZAREA_2D(:,:))
    CALL OASIS_WRITE_MASK  (YSFX_LAND,IX,IY,IMASK_2D(:,:))
!
  ELSEIF(NRANK==NPIO)THEN
!
    CALL OASIS_WRITE_GRID  (YSFX_LAND,U%NDIM_FULL,1,ZLON_TOT(:,:),ZLAT_TOT(:,:))  
    CALL OASIS_WRITE_CORNER(YSFX_LAND,U%NDIM_FULL,1,INC,ZCORNER_LON_TOT(:,:,:),ZCORNER_LAT_TOT(:,:,:))
    CALL OASIS_WRITE_AREA  (YSFX_LAND,U%NDIM_FULL,1,ZAREA_TOT(:,:))
    CALL OASIS_WRITE_MASK  (YSFX_LAND,U%NDIM_FULL,1,IMASK_TOT(:,:))
!
  ENDIF
!
ENDIF
!
!*       4.2    Grid definition for lake surface :
!               ----------------------------------
!
IF(LCPL_LAKE)THEN
!
  ZAREA(:,1) = UG%G%XMESH_SIZE(:) * ZMASK_LAKE(:)
! 0 = not masked ; 1 = masked
  WHERE(ZAREA(:,1)>0.0)
        IMASK(:,1) = 0
  ELSEWHERE
        IMASK(:,1) = 1
  ENDWHERE
!
  CALL GATHER_AND_WRITE_MPI(ZLON,ZLON_TOT)
  CALL GATHER_AND_WRITE_MPI(ZLAT,ZLAT_TOT)
  CALL GATHER_AND_WRITE_MPI(ZCORNER_LON,ZCORNER_LON_TOT)
  CALL GATHER_AND_WRITE_MPI(ZCORNER_LAT,ZCORNER_LAT_TOT)
  CALL GATHER_AND_WRITE_MPI(ZAREA,ZAREA_TOT)
  CALL GATHER_AND_WRITE_MPI(IMASK,IMASK_TOT)
!
  IF(G2D_GRID.AND.NRANK==NPIO)THEN
!
    ZLON_2D(:,:) = RESHAPE(ZLON_TOT, (/IX,IY/))
    ZLAT_2D(:,:) = RESHAPE(ZLAT_TOT, (/IX,IY/))
    ZCORNER_LON_2D(:,:,:) = RESHAPE(ZCORNER_LON_TOT, (/IX,IY,INC/))
    ZCORNER_LAT_2D(:,:,:) = RESHAPE(ZCORNER_LAT_TOT, (/IX,IY,INC/))
    IMASK_2D(:,:) = RESHAPE(IMASK_TOT, (/IX,IY/))
    ZAREA_2D(:,:) = RESHAPE(ZAREA_TOT, (/IX,IY/))
!
    CALL OASIS_WRITE_GRID  (YSFX_LAKE,IX,IY,ZLON_2D(:,:),ZLAT_2D(:,:))  
    CALL OASIS_WRITE_CORNER(YSFX_LAKE,IX,IY,INC,ZCORNER_LON_2D(:,:,:),ZCORNER_LAT_2D(:,:,:))
    CALL OASIS_WRITE_AREA  (YSFX_LAKE,IX,IY,ZAREA_2D(:,:))
    CALL OASIS_WRITE_MASK  (YSFX_LAKE,IX,IY,IMASK_2D(:,:))
!
  ELSEIF(NRANK==NPIO)THEN
!
    CALL OASIS_WRITE_GRID  (YSFX_LAKE,U%NDIM_FULL,1,ZLON_TOT(:,:),ZLAT_TOT(:,:))  
    CALL OASIS_WRITE_CORNER(YSFX_LAKE,U%NDIM_FULL,1,INC,ZCORNER_LON_TOT(:,:,:),ZCORNER_LAT_TOT(:,:,:))
    CALL OASIS_WRITE_AREA  (YSFX_LAKE,U%NDIM_FULL,1,ZAREA_TOT(:,:))
    CALL OASIS_WRITE_MASK  (YSFX_LAKE,U%NDIM_FULL,1,IMASK_TOT(:,:))
!
  ENDIF
!
ENDIF
!
!*       4.3    Grid definition for sea/water :
!               -------------------------------
!
IF(LCPL_SEA)THEN    
!
  ZAREA(:,1) = UG%G%XMESH_SIZE(:) * ZMASK_SEA(:)
! 0 = not masked ; 1 = masked
  WHERE(ZAREA(:,1)>0.0)
        IMASK(:,1) = 0
  ELSEWHERE
        IMASK(:,1) = 1
  ENDWHERE
!
  CALL GATHER_AND_WRITE_MPI(ZLON,ZLON_TOT)
  CALL GATHER_AND_WRITE_MPI(ZLAT,ZLAT_TOT)
  CALL GATHER_AND_WRITE_MPI(ZCORNER_LON,ZCORNER_LON_TOT)
  CALL GATHER_AND_WRITE_MPI(ZCORNER_LAT,ZCORNER_LAT_TOT)
  CALL GATHER_AND_WRITE_MPI(ZAREA,ZAREA_TOT)
  CALL GATHER_AND_WRITE_MPI(IMASK,IMASK_TOT)
!
  IF(G2D_GRID.AND.NRANK==NPIO)THEN
!
    ZLON_2D(:,:) = RESHAPE(ZLON_TOT, (/IX,IY/))
    ZLAT_2D(:,:) = RESHAPE(ZLAT_TOT, (/IX,IY/))
    ZCORNER_LON_2D(:,:,:) = RESHAPE(ZCORNER_LON_TOT, (/IX,IY,INC/))
    ZCORNER_LAT_2D(:,:,:) = RESHAPE(ZCORNER_LAT_TOT, (/IX,IY,INC/))
    IMASK_2D(:,:) = RESHAPE(IMASK_TOT, (/IX,IY/))
    ZAREA_2D(:,:) = RESHAPE(ZAREA_TOT, (/IX,IY/))
!
    CALL OASIS_WRITE_GRID  (YSFX_SEA,IX,IY,ZLON_2D(:,:),ZLAT_2D(:,:))  
    CALL OASIS_WRITE_CORNER(YSFX_SEA,IX,IY,INC,ZCORNER_LON_2D(:,:,:),ZCORNER_LAT_2D(:,:,:))
    CALL OASIS_WRITE_AREA  (YSFX_SEA,IX,IY,ZAREA_2D(:,:))
    CALL OASIS_WRITE_MASK  (YSFX_SEA,IX,IY,IMASK_2D(:,:))
!
  ELSEIF(NRANK==NPIO)THEN
!
    CALL OASIS_WRITE_GRID  (YSFX_SEA,U%NDIM_FULL,1,ZLON_TOT(:,:),ZLAT_TOT(:,:))  
    CALL OASIS_WRITE_CORNER(YSFX_SEA,U%NDIM_FULL,1,INC,ZCORNER_LON_TOT(:,:,:),ZCORNER_LAT_TOT(:,:,:))
    CALL OASIS_WRITE_AREA  (YSFX_SEA,U%NDIM_FULL,1,ZAREA_TOT(:,:))
    CALL OASIS_WRITE_MASK  (YSFX_SEA,U%NDIM_FULL,1,IMASK_TOT(:,:))
!
  ENDIF
!
ENDIF
!
IF(NRANK==NPIO)THEN
  CALL OASIS_TERMINATE_GRIDS_WRITING()
ENDIF
!
CALL OASIS_ENDDEF(IERR)
!
IF(IERR/=OASIS_OK)THEN
   WRITE(ILUOUT,*)'SFX_OASIS_PREP: OASIS enddef problem, err = ',IERR
   CALL ABOR1_SFX('SFX_OASIS_PREP: OASIS enddef problem')
ENDIF
!
!-------------------------------------------------------------------------------
#endif
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('SFX_OASIS_PREP',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE SFX_OASIS_PREP
