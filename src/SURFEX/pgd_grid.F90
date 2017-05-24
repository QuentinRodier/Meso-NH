!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE PGD_GRID (&
                            UG, U,GCP, &
                           HPROGRAM,HFILE,HFILETYPE,OGRID,HGRID,KGRID_PAR,PGRID_PAR)
!     ##########################################################
!!
!!    PURPOSE
!!    -------
!!   Reads in namelist the grid type and parameters.
!!
!!    METHOD
!!    ------
!!   
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!
!!    V. Masson                   Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original     01/2004
!!    E. Martin    10/2007 IGN grid
!!    P. Samuelsson  12/2012 Rotated lonlat
!!    M. Moge      05/02/2015 parallelization (using local sizes, GET_MEAN_OF_COORD_SQRT_ll, SET_NAM_GRID_CONF_PROJ_LOCAL) + MPPDB_CHECK
!!    M. Moge      01/03/2015 call SPLIT_GRID if CPROGRAM == 'PGD   ' + remove SET_NAM_GRID_CONF_PROJ_LOCAL
!!    M. Moge      01/03/2015 change in the input arguments of PGD_GRID_IO_INIT : passing IDXRATIO, IDYRATIO
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
!
!
!
!
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_GRID_CONF_PROJ, ONLY :GRID_CONF_PROJ_t 
!
USE MODD_SURFEX_MPI,     ONLY : NSIZE, NINDEX, NPIO, NRANK
USE MODD_SURFEX_OMP,     ONLY : NINDX2SFX, NWORK, NWORK2, XWORK, XWORK2, XWORK3, &
                                NWORK_FULL, NWORK2_FULL, XWORK_FULL, XWORK2_FULL
!
USE MODD_PGD_GRID,       ONLY : NL, XGRID_PAR, NGRID_PAR, XMESHLENGTH
USE MODN_PGD_GRID
USE MODD_CSTS,           ONLY : XPI, XRADIUS
!
USE MODI_DEFAULT_GRID
USE MODI_GRID_FROM_FILE
USE MODI_OPEN_NAMELIST
USE MODI_TEST_NAM_VAR_SURF
USE MODI_CLOSE_NAMELIST
USE MODI_GET_LUOUT
USE MODI_READ_NAM_GRIDTYPE
USE MODI_LATLON_GRID
!
USE MODE_POS_SURF
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABOR1_SFX
!
USE MODI_PGD_GRID_IO_INIT
#ifdef MNH_PARALLEL
USE MODE_TOOLS_ll, ONLY : GET_MEAN_OF_COORD_SQRT_ll
!
USE MODI_GET_SIZE_FULL_n
#ifdef MNH_PARALLEL
USE MODE_MPPDB
#endif
USE MODI_SPLIT_GRID
USE MODD_CONF, ONLY : CPROGRAM
#endif
!
IMPLICIT NONE
!
!*    0.1    Declaration of dummy arguments
!            ------------------------------
!
!
!
!
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(GRID_CONF_PROJ_t),INTENT(INOUT) :: GCP
!
 CHARACTER(LEN=6),  INTENT(IN)   :: HPROGRAM   ! program calling the surface
 CHARACTER(LEN=28), INTENT(IN)   :: HFILE      ! atmospheric file name
 CHARACTER(LEN=6),  INTENT(IN)   :: HFILETYPE  ! atmospheric file type
LOGICAL,           INTENT(IN)   :: OGRID      ! .true. if grid is imposed by atm. model
 CHARACTER(LEN=10), INTENT(OUT)  :: HGRID      ! grid type
INTEGER,           INTENT(OUT)  :: KGRID_PAR  ! size of PGRID_PAR
REAL, DIMENSION(:), POINTER     :: PGRID_PAR  ! parameters defining this grid
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER           :: ILUOUT ! output listing logical unit
INTEGER           :: ILUNAM ! namelist file  logical unit
LOGICAL           :: GFOUND ! Flag true if namelist is present
REAL(KIND=JPRB) :: ZHOOK_HANDLE
INTEGER :: IIMAX_ll, IJMAX_ll ! global size of son model
LOGICAL :: GRECT
!
 INTEGER :: IXOR = 1            ! position of modified bottom left point
 INTEGER :: IYOR = 1            ! according to initial grid
 INTEGER :: IXSIZE = -999       ! number of grid meshes in initial grid to be
 INTEGER :: IYSIZE = -999       ! covered by the modified grid
 INTEGER :: IDXRATIO = 1        ! resolution ratio between modified grid
 INTEGER :: IDYRATIO = 1        ! and initial grid
NAMELIST/NAM_INIFILE_CONF_PROJ/IXOR,IYOR,IXSIZE,IYSIZE,IDXRATIO,IDYRATIO
!
!*    0.3    Declaration of namelists
!            ------------------------
!
!------------------------------------------------------------------------------
!
!*    1.      Defaults
!             --------
!
IF (LHOOK) CALL DR_HOOK('PGD_GRID',0,ZHOOK_HANDLE)
 CALL DEFAULT_GRID(HPROGRAM,CGRID)
!
YINIFILE  = '                         '
YINIFILETYPE = '      '
!
IF (OGRID) THEN
  YINIFILE  = HFILE
  YINIFILETYPE = HFILETYPE
END IF
!
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!------------------------------------------------------------------------------
!
!*    2.      Open namelist
!             -------------
!
IF (.NOT. OGRID) THEN
  CALL OPEN_NAMELIST(HPROGRAM,ILUNAM)
!
!------------------------------------------------------------------------------
!
!*    3.      Read grid type
!             --------------
!
  CALL POSNAM(ILUNAM,'NAM_PGD_GRID',GFOUND,ILUOUT)
  IF (GFOUND) READ(UNIT=ILUNAM,NML=NAM_PGD_GRID)
!
!------------------------------------------------------------------------------
!
!*    5.      Close namelist
!             --------------
!
  CALL CLOSE_NAMELIST(HPROGRAM,ILUNAM)
!
END IF
!-------------------------------------------------------------------------------
!
!*       4.    check of grid and input file types
!              ----------------------------------
! 
 CALL TEST_NAM_VAR_SURF(ILUOUT,'CGRID',CGRID,'CONF PROJ ','NONE      ','LONLAT REG','CARTESIAN ','GAUSS     ',&
          'IGN       ','LONLATVAL ','LONLAT ROT')  
 CALL TEST_NAM_VAR_SURF(ILUOUT,'YINIFILETYPE',YINIFILETYPE,'      ','MESONH','LFI   ','ASCII ','NC    ')
!
!
!------------------------------------------------------------------------------
!
!*    5.      Initializes grid characteristics
!             --------------------------------
!
!*    5.1     From another file
!             -----------------
!
IF (LEN_TRIM(YINIFILETYPE)>0 .AND. LEN_TRIM(YINIFILE)>0 ) THEN
  IF (YINIFILETYPE=='MESONH' .OR. YINIFILETYPE=='LFI   ' .OR. YINIFILETYPE=='ASCII ' .OR. YINIFILETYPE=='NC    ') THEN
    CALL GRID_FROM_FILE(&
                        HPROGRAM,YINIFILE,YINIFILETYPE,OGRID,CGRID,NGRID_PAR,XGRID_PAR,NL,U)
    HGRID     = CGRID
    IF ( HGRID == "IGN       " .OR. HGRID == "GAUSS     " .OR. HGRID == "NONE      " ) THEN
      GRECT = .FALSE.
    ELSE
      GRECT = .TRUE.
    ENDIF
    ! on lit la taille globale du modele fils dans la namelist
    CALL OPEN_NAMELIST(HPROGRAM,ILUNAM)
    CALL POSNAM(ILUNAM,'NAM_INIFILE_CONF_PROJ',GFOUND,ILUOUT)
    IF (GFOUND) THEN 
      READ(UNIT=ILUNAM,NML=NAM_INIFILE_CONF_PROJ)
      IIMAX_ll = IXSIZE*IDXRATIO
      IJMAX_ll = IYSIZE*IDYRATIO
    ENDIF
    CALL CLOSE_NAMELIST(HPROGRAM,ILUNAM)

    !*    3.      Additional actions for I/O
    !
    IF (GFOUND) THEN 
#ifdef MNH_PARALLEL
      CALL PGD_GRID_IO_INIT(HPROGRAM,NGRID_PAR,XGRID_PAR, HGRID, GRECT, IIMAX_ll, IJMAX_ll, IDXRATIO, IDYRATIO)
#else
      CALL PGD_GRID_IO_INIT(HPROGRAM)
#endif
      U%NDIM_FULL = NL
    ELSE
#ifdef MNH_PARALLEL
      CALL PGD_GRID_IO_INIT(HPROGRAM,NGRID_PAR,XGRID_PAR, HGRID, GRECT)
#else
      CALL PGD_GRID_IO_INIT(HPROGRAM)
#endif
    ENDIF
    NSIZE     = U%NDIM_FULL
#ifdef MNH_PARALLEL
    CALL GET_SIZE_FULL_n(U,HPROGRAM,U%NDIM_FULL,U%NSIZE_FULL)
    NL = U%NSIZE_FULL
#else
    U%NSIZE_FULL = NL
#endif
                
  ELSE
    CALL ABOR1_SFX('PGD_GRID: FILE TYPE NOT SUPPORTED '//HFILETYPE//' FOR FILE '//HFILE)
  END IF
!
ELSE
!
!*    5.2     Grid not initialized
!             --------------------
!
  IF (CGRID=='NONE      ' .OR. CGRID=='          ') THEN
    CALL ABOR1_SFX('PGD_GRID: GRID TYPE NOT INITIALIZED, CGRID='//CGRID)

!
!*    5.3     Grid initialized
!             ----------------
!
  ELSE
!
    CALL READ_NAM_GRIDTYPE(HPROGRAM,CGRID,NGRID_PAR,XGRID_PAR,NL,GCP)
    HGRID     = CGRID
    !*    3.      Additional actions for I/O
    !
#ifdef MNH_PARALLEL
    CALL PGD_GRID_IO_INIT(HPROGRAM,NGRID_PAR,XGRID_PAR)
#else
    CALL PGD_GRID_IO_INIT(HPROGRAM)
#endif
    U%NDIM_FULL = NL
    NSIZE     = U%NDIM_FULL
#ifdef MNH_PARALLEL
    CALL GET_SIZE_FULL_n(U,HPROGRAM,U%NDIM_FULL,U%NSIZE_FULL)
    NL = U%NSIZE_FULL
#else
    U%NSIZE_FULL = NL
#endif
  END IF
#ifdef MNH_PARALLEL
  ! IF we are in PREP_PGD, we need to split the grid. Otherwise, the grid was read in parallel and is already splitted
  IF ( CPROGRAM == 'PGD   ') THEN
    CALL SPLIT_GRID(UG,U,'MESONH',NGRID_PAR,XGRID_PAR)
  ENDIF
#endif

END IF
!
!
IF (.NOT.ALLOCATED(NINDEX)) THEN
  ALLOCATE(NINDEX(U%NDIM_FULL))
  NINDEX(:) = 0
ENDIF
NINDX2SFX    = U%NDIM_FULL
ALLOCATE(NWORK(U%NDIM_FULL))
ALLOCATE(XWORK(U%NDIM_FULL))
ALLOCATE(NWORK2(U%NDIM_FULL,10))
ALLOCATE(XWORK2(U%NDIM_FULL,10))
ALLOCATE(XWORK3(U%NDIM_FULL,10,10))
IF (NRANK==NPIO) THEN
  ALLOCATE(NWORK_FULL(U%NDIM_FULL))
  ALLOCATE(XWORK_FULL(U%NDIM_FULL))
  ALLOCATE(NWORK2_FULL(U%NDIM_FULL,10))
  ALLOCATE(XWORK2_FULL(U%NDIM_FULL,10))
ELSE
  ALLOCATE(NWORK_FULL(0))
  ALLOCATE(XWORK_FULL(0))
  ALLOCATE(NWORK2_FULL(0,0))
  ALLOCATE(XWORK2_FULL(0,0))
ENDIF
!
KGRID_PAR = NGRID_PAR
ALLOCATE(PGRID_PAR(KGRID_PAR))
PGRID_PAR = XGRID_PAR
!
!------------------------------------------------------------------------------
!
!*    6.      Latitude and longitude
!             ----------------------
!
ALLOCATE(UG%XLAT       (U%NSIZE_FULL))
ALLOCATE(UG%XLON       (U%NSIZE_FULL))
ALLOCATE(UG%XMESH_SIZE (U%NSIZE_FULL))
ALLOCATE(UG%XJPDIR     (U%NSIZE_FULL))
 CALL LATLON_GRID(CGRID,NGRID_PAR,U%NSIZE_FULL,ILUOUT,XGRID_PAR,UG%XLAT,UG%XLON,UG%XMESH_SIZE,UG%XJPDIR)
#ifdef MNH_PARALLEL
 CALL MPPDB_CHECK_SURFEX2D(UG%XLAT,"PGD_GRID after LATLON_GRID:XLAT",PRECISION,ILUOUT)
 CALL MPPDB_CHECK_SURFEX2D(UG%XLON,"PGD_GRID after LATLON_GRID:XLON",PRECISION,ILUOUT)
 CALL MPPDB_CHECK_SURFEX2D(UG%XMESH_SIZE,"PGD_GRID after LATLON_GRID:XMESH_SIZE",PRECISION,ILUOUT)
#endif
!
!------------------------------------------------------------------------------
!
!*    7.      Average grid length (in degrees)
!             --------------------------------
!
!* in meters
#ifdef MNH_PARALLEL
CALL GET_MEAN_OF_COORD_SQRT_ll(UG%XMESH_SIZE,U%NSIZE_FULL,U%NDIM_FULL,XMESHLENGTH)
#else
XMESHLENGTH = SUM ( SQRT(UG%XMESH_SIZE) ) / NL
#endif
!
!* in degrees (of latitude)
XMESHLENGTH = XMESHLENGTH *180. / XPI / XRADIUS
IF (LHOOK) CALL DR_HOOK('PGD_GRID',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE PGD_GRID
