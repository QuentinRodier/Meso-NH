!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
      SUBROUTINE PGD_GRID(HPROGRAM,HFILE,HFILETYPE,OGRID,HGRID,KGRID_PAR,PGRID_PAR)
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
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_SURFEX_MPI,     ONLY : NSIZE, NINDEX, NPIO, NRANK
USE MODD_SURFEX_OMP,     ONLY : NINDX2, NWORK, XWORK, XWORK2, XWORK3, &
                                NWORK_FULL, XWORK_FULL, XWORK2_FULL
!
USE MODD_PGD_GRID,       ONLY : NL, XGRID_PAR, NGRID_PAR, XMESHLENGTH
USE MODN_PGD_GRID
USE MODD_SURF_ATM_GRID_n, ONLY : XLAT, XLON, XMESH_SIZE, XJPDIR
USE MODD_SURF_ATM_n,     ONLY : NDIM_FULL
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
IMPLICIT NONE
!
!*    0.1    Declaration of dummy arguments
!            ------------------------------
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
YFILETYPE = '      '
!
IF (OGRID) THEN
  YINIFILE  = HFILE
  YFILETYPE = HFILETYPE
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
          'IGN       ','LONLATVAL ')  
 CALL TEST_NAM_VAR_SURF(ILUOUT,'YFILETYPE',YFILETYPE,'      ','MESONH','LFI   ','ASCII ')
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
IF (LEN_TRIM(YFILETYPE)>0 .AND. LEN_TRIM(YINIFILE)>0 ) THEN
  IF (YFILETYPE=='MESONH' .OR. YFILETYPE=='LFI   ' .OR. YFILETYPE=='ASCII ') THEN
    CALL GRID_FROM_FILE(HPROGRAM,YINIFILE,YFILETYPE,OGRID,CGRID,NGRID_PAR,XGRID_PAR,NL)
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
    CALL READ_NAM_GRIDTYPE(HPROGRAM,CGRID,NGRID_PAR,XGRID_PAR,NL)
!
  END IF

END IF
!
HGRID     = CGRID
NDIM_FULL = NL
NSIZE     = NDIM_FULL
IF (.NOT.ALLOCATED(NINDEX)) THEN
  ALLOCATE(NINDEX(NDIM_FULL))
  NINDEX(:) = 0
ENDIF
NINDX2    = NDIM_FULL
ALLOCATE(NWORK(NDIM_FULL))
ALLOCATE(XWORK(NDIM_FULL))
ALLOCATE(XWORK2(NDIM_FULL,10))
ALLOCATE(XWORK3(NDIM_FULL,10,10))
IF (NRANK==NPIO) THEN
  ALLOCATE(NWORK_FULL(NDIM_FULL))
  ALLOCATE(XWORK_FULL(NDIM_FULL))
  ALLOCATE(XWORK2_FULL(NDIM_FULL,10))
ELSE
  ALLOCATE(NWORK_FULL(0))
  ALLOCATE(XWORK_FULL(0))
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
ALLOCATE(XLAT       (NL))
ALLOCATE(XLON       (NL))
ALLOCATE(XMESH_SIZE (NL))
ALLOCATE(XJPDIR     (NL))
 CALL LATLON_GRID(CGRID,NGRID_PAR,NL,ILUOUT,XGRID_PAR,XLAT,XLON,XMESH_SIZE,XJPDIR)
!
!------------------------------------------------------------------------------
!
!*    7.      Average grid length (in degrees)
!             --------------------------------
!
!* in meters
XMESHLENGTH = SUM ( SQRT(XMESH_SIZE) ) / NL
!
!* in degrees (of latitude)
XMESHLENGTH = XMESHLENGTH *180. / XPI / XRADIUS
IF (LHOOK) CALL DR_HOOK('PGD_GRID',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE PGD_GRID
