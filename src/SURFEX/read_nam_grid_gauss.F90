!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!################################################################
SUBROUTINE READ_NAM_GRID_GAUSS(HPROGRAM,KGRID_PAR,KL,PGRID_PAR)
!################################################################
!
!!****  *READ_NAM_GRID_GAUSS* - routine to read in namelist the horizontal grid
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
!!      V. Masson   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2004
!!      B. Decharme    2008  Comput and save the Mesh size
!!                     2013  Bug lat and lon for non rotat-strech grid
!!                           Grid corner (used with oasis)
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_CSTS,     ONLY : XPI
USE MODD_SURF_PAR, ONLY : XUNDEF
! 
USE MODE_POS_SURF
!
USE MODI_OPEN_NAMELIST
USE MODI_CLOSE_NAMELIST
USE MODI_GET_LUOUT
!
USE MODE_GRIDTYPE_GAUSS
!
USE EGGANGLES , ONLY : P_ASIN
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABOR1_SFX
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
 CHARACTER(LEN=6),           INTENT(IN)    :: HPROGRAM   ! calling program
INTEGER,                    INTENT(INOUT) :: KGRID_PAR  ! size of PGRID_PAR
INTEGER,                    INTENT(OUT)   :: KL         ! number of points
REAL, DIMENSION(KGRID_PAR), INTENT(OUT)   :: PGRID_PAR  ! parameters defining this grid
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER :: ILUOUT ! output listing logical unit
INTEGER :: ILUNAM ! namelist file  logical unit
REAL,    DIMENSION(:),   ALLOCATABLE :: ZLAT_XY     ! pseudo-latitudes
REAL,    DIMENSION(:),   ALLOCATABLE :: ZLON_XY     ! pseudo-longitudes
REAL,    DIMENSION(:),   ALLOCATABLE :: ZLAT        ! latitudes
REAL,    DIMENSION(:),   ALLOCATABLE :: ZLON        ! longitudes
REAL,    DIMENSION(:),   ALLOCATABLE :: ZMESH_SIZE  ! Mesh size
!                                                                              _____ Sup
REAL,    DIMENSION(:),   ALLOCATABLE :: ZLATSUP     ! Grid corner Latitude    |     |
REAL,    DIMENSION(:),   ALLOCATABLE :: ZLONSUP     ! Grid corner Longitude   |     |
REAL,    DIMENSION(:),   ALLOCATABLE :: ZLATINF     ! Grid corner Latitude    |_____|
REAL,    DIMENSION(:),   ALLOCATABLE :: ZLONINF     ! Grid corner Longitude  Inf
!
REAL,    DIMENSION(:),   ALLOCATABLE :: ZXINF    ! pseudo-longitude western limit of grid mesh
REAL,    DIMENSION(:),   ALLOCATABLE :: ZXSUP    ! pseudo-longitude eastern limit of grid mesh
REAL,    DIMENSION(:),   ALLOCATABLE :: ZYINF    ! pseudo-latitude southern limit of grid mesh
REAL,    DIMENSION(:),   ALLOCATABLE :: ZYSUP    ! pseudo-latitude northern limit of grid mesh
!
!*       0.3   Declarations of namelist
!              ------------------------
!
INTEGER :: NDGLG    ! number of pseudo-latitudes
REAL    :: RMUCEN   ! sine of the latitude of the rotated pole
REAL    :: RLOCEN   ! longitude of the rotated pole (radian)
REAL    :: RSTRET   ! stretching factor (must be greater than or equal to 1)
INTEGER, DIMENSION(1000) :: NRGRI ! number of pseudo-longitudes on each
                                  ! pseudo-latitude circle on pseau
                                  ! northern hemisphere (starting from
                                  ! the rotated pole)
!
REAL    :: ZLAPO    ! latitude  of the rotated pole (deg)
REAL    :: ZLOPO    ! longitude of the rotated pole (deg)
REAL    :: ZCODIL   ! stretching factor (must be greater than or equal to 1)
INTEGER                            :: ITYP   ! type of transform (0 --> no rotation, 1 otherwise)
INTEGER                            :: INLATI ! number of latitudes
INTEGER, DIMENSION(:), ALLOCATABLE :: INLOPA ! number of pseudo-longitudes on each
                                             ! pseudo-latitude circle

INTEGER :: JSTGLO

!
REAL, DIMENSION(:), POINTER :: ZGRID_PAR
!
LOGICAL :: GFOUND
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
NAMELIST/NAMDIM/NDGLG
NAMELIST/NAMGEM/RMUCEN, RLOCEN, RSTRET
NAMELIST/NAMRGRI/NRGRI
!
!------------------------------------------------------------------------------
!
!*       1.    Default values
! 
IF (LHOOK) CALL DR_HOOK('READ_NAM_GRID_GAUSS',0,ZHOOK_HANDLE)
NDGLG = 0
RMUCEN = 1.
RLOCEN = XPI
RSTRET = 1.
!
NRGRI(:) = 0
!------------------------------------------------------------------------------
!
!*       2.    opening of namelist
! 
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
 CALL OPEN_NAMELIST(HPROGRAM,ILUNAM)
!
!---------------------------------------------------------------------------
!
!*       3.    Reading of projection parameters
!              --------------------------------
!
 CALL POSNAM(ILUNAM,'NAMGEM',GFOUND,ILUOUT)
IF (GFOUND) READ(UNIT=ILUNAM,NML=NAMGEM)
!
IF (RSTRET<1.) THEN
  WRITE(ILUOUT,*) '****************************************************'
  WRITE(ILUOUT,*) 'stretching factor RSTRET for the Gaussian grid'
  WRITE(ILUOUT,*) 'definition must be greater than or equal to 1'
  WRITE(ILUOUT,*) 'You have set RSTRET=', RSTRET
  WRITE(ILUOUT,*) 'Please modify the value of RSTRET in namelist NAMGEM'
  WRITE(ILUOUT,*) '****************************************************'
  CALL ABOR1_SFX('READ_NAM_GRID_GAUSS: STRETCHING FACTOR MUST BE >= 1.')
END IF
!
ZLAPO = 180. / XPI * P_ASIN(RMUCEN)
ZLOPO = 180. / XPI * RLOCEN
!
ZCODIL = RSTRET
!
!---------------------------------------------------------------------------
!
!*       4.    Reading parameters of the grid
!              ------------------------------
!
 CALL POSNAM(ILUNAM,'NAMDIM',GFOUND,ILUOUT)
IF (GFOUND) READ(UNIT=ILUNAM,NML=NAMDIM)
 CALL POSNAM(ILUNAM,'NAMRGRI',GFOUND,ILUOUT)
IF (GFOUND) READ(UNIT=ILUNAM,NML=NAMRGRI)
!
INLATI = NDGLG
ALLOCATE(INLOPA(INLATI))
INLOPA(1:INLATI/2) = NRGRI(1:INLATI/2)
INLOPA(INLATI/2+1:INLATI) = NRGRI(INLATI/2:1:-1)
!
!---------------------------------------------------------------------------
 CALL CLOSE_NAMELIST(HPROGRAM,ILUNAM)
!---------------------------------------------------------------------------
!
!*       5.    Computes pseudo-latitudes and pseudo-longitudes of all points
!              -------------------------------------------------------------
!
!* number of points
KL = SUM(INLOPA)

!
!* type of transform
IF (ZLAPO>89.99 .AND. ABS(ZLOPO)<0.00001) THEN
  ITYP=0
ELSE
  ITYP=1
ENDIF
!
ALLOCATE(ZLAT_XY(KL))
ALLOCATE(ZLON_XY(KL))
ZLAT_XY(:) = XUNDEF
ZLON_XY(:) = XUNDEF
!
 CALL COMP_GRIDTYPE_GAUSS(INLATI,INLOPA,KL,ITYP,ZLAT_XY,ZLON_XY)
!
!---------------------------------------------------------------------------
!
!*       6.    Computes latitudes and longitudes
!              ---------------------------------
!
!* all points are used
ALLOCATE(ZLAT(KL))
ALLOCATE(ZLON(KL))
!
ZLAT(:) = XUNDEF
ZLON(:) = XUNDEF
!
IF(ZCODIL==1.0.AND.ITYP==0)THEN
  ZLON(:)=ZLON_XY(:)
  ZLAT(:)=ZLAT_XY(:)
ELSE
  CALL LATLON_GAUSS(ZLON_XY,ZLAT_XY,KL,ZLOPO,ZLAPO,ZCODIL,ZLON,ZLAT)
ENDIF
!
!---------------------------------------------------------------------------
!
!*       7.    Computes grid corner latitudes and longitudes
!              ---------------------------------------------
!
ALLOCATE(ZXINF(KL))
ALLOCATE(ZYINF(KL))
ALLOCATE(ZXSUP(KL))
ALLOCATE(ZYSUP(KL))
!
ALLOCATE(ZLONINF(KL))
ALLOCATE(ZLATINF(KL))
ALLOCATE(ZLONSUP(KL))
ALLOCATE(ZLATSUP(KL))
!
ZXINF  (:) = XUNDEF
ZYINF  (:) = XUNDEF
ZXSUP  (:) = XUNDEF
ZYSUP  (:) = XUNDEF
ZLONINF(:) = XUNDEF
ZLATINF(:) = XUNDEF
ZLONSUP(:) = XUNDEF
ZLATSUP(:) = XUNDEF
!
 CALL GAUSS_GRID_LIMITS(INLATI,INLOPA,ZXINF,ZXSUP,ZYINF,ZYSUP)
!
IF(ZCODIL==1.0.AND.ITYP==0)THEN
  ZLONINF(:) = ZXINF(:)
  ZLATINF(:) = ZYINF(:)
  ZLONSUP(:) = ZXSUP(:)
  ZLATSUP(:) = ZYSUP(:)
ELSE        
  CALL LATLON_GAUSS(ZXINF,ZYINF,KL,ZLOPO,ZLAPO,ZCODIL,ZLONINF,ZLATINF)  
  CALL LATLON_GAUSS(ZXSUP,ZYSUP,KL,ZLOPO,ZLAPO,ZCODIL,ZLONSUP,ZLATSUP)  
ENDIF
!
!---------------------------------------------------------------------------
!
!*       8.    Computes mesh size
!              ---------------------------------
!
ALLOCATE(ZMESH_SIZE(KL))
ZMESH_SIZE(:) = XUNDEF
!
 CALL MESH_SIZE_GAUSS(KL,INLATI,INLOPA,ZLAPO,ZLOPO,ZCODIL,&
                               ZLAT_XY,ZLON,ZLAT,ZMESH_SIZE)  
!
!---------------------------------------------------------------------------
!
!*       9.    All this information stored into pointer PGRID_PAR
!              --------------------------------------------------
!
 CALL PUT_GRIDTYPE_GAUSS(ZGRID_PAR,INLATI,ZLAPO,ZLOPO,ZCODIL,INLOPA, &
                          KL,ZLAT,ZLON,ZLAT_XY,ZLON_XY,ZMESH_SIZE,   &
                          ZLONINF,ZLATINF,ZLONSUP,ZLATSUP            )  
!
!---------------------------------------------------------------------------
!
!*       9.    All this information stored into pointer PGRID_PAR
!              --------------------------------------------------
!
DEALLOCATE(ZLAT)
DEALLOCATE(ZLON)
DEALLOCATE(ZLAT_XY)
DEALLOCATE(ZLON_XY)
DEALLOCATE(INLOPA)
DEALLOCATE(ZMESH_SIZE)
DEALLOCATE(ZLATINF)
DEALLOCATE(ZLONINF)
DEALLOCATE(ZLATSUP)
DEALLOCATE(ZLONSUP)
DEALLOCATE(ZXINF)
DEALLOCATE(ZYINF)
DEALLOCATE(ZXSUP)
DEALLOCATE(ZYSUP)
!
!---------------------------------------------------------------------------
!
!* 1st call : initializes dimension
!
IF (KGRID_PAR==0) THEN
  KGRID_PAR = SIZE(ZGRID_PAR)
!
ELSE
!
!* 2nd call : initializes grid array
!
  PGRID_PAR(:) = 0.
  PGRID_PAR(:) = ZGRID_PAR
END IF
!
DEALLOCATE(ZGRID_PAR)
IF (LHOOK) CALL DR_HOOK('READ_NAM_GRID_GAUSS',1,ZHOOK_HANDLE)

!
!---------------------------------------------------------------------------
!
END SUBROUTINE READ_NAM_GRID_GAUSS
