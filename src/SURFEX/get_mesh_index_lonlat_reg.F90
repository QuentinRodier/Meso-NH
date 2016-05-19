!     ###############################################################
      SUBROUTINE GET_MESH_INDEX_LONLAT_REG(KGRID_PAR,KL,PGRID_PAR,PLAT,PLON,KINDEX,KSSO,KISSOX,KISSOY)
!     ###############################################################
!
!!**** *GET_MESH_INDEX_LONLAT_REG* get the grid mesh where point (lat,lon) is located
!!
!!    PURPOSE
!!    -------
!!
!!    AUTHOR
!!    ------
!!
!!    V. Masson         Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    12/09/95
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_GET_MESH_INDEX_LONLAT_REG, ONLY : XLONLIM, XLATLIM, NLAT, NLON, XLON0
USE MODE_GRIDTYPE_LONLAT_REG
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
INTEGER,                       INTENT(IN)    :: KGRID_PAR ! size of PGRID_PAR
INTEGER,                       INTENT(IN)    :: KL        ! number of points
REAL,    DIMENSION(KGRID_PAR), INTENT(IN)    :: PGRID_PAR ! grid parameters
REAL,    DIMENSION(KL),        INTENT(IN)    :: PLAT      ! latitude of the point
REAL,    DIMENSION(KL),        INTENT(IN)    :: PLON      ! longitude of the point
INTEGER, DIMENSION(KL),        INTENT(OUT)   :: KINDEX    ! index of the grid mesh where the point is
INTEGER,                       INTENT(IN)    :: KSSO      ! number of subgrid mesh in each direction
INTEGER, DIMENSION(KL),        INTENT(OUT)   :: KISSOX    ! X index of the subgrid mesh
INTEGER, DIMENSION(KL),        INTENT(OUT)   :: KISSOY    ! Y index of the subgrid mesh
!
!*    0.2    Declaration of other local variables
!            ------------------------------------
!
INTEGER                           :: JI       ! loop counter in x
INTEGER                           :: JJ       ! loop counter in y
INTEGER                           :: JL       ! loop counter on input points
!
REAL    :: ZLONMIN ! minimum longitude (degrees)
REAL    :: ZLONMAX ! maximum longitude (degrees)
REAL    :: ZLATMIN ! minimum latitude  (degrees)
REAL    :: ZLATMAX ! maximum latitude  (degrees)
REAL    :: ZDLON   ! longitude grid size
REAL    :: ZDLAT   ! latitude  grid size
!
REAL, DIMENSION(SIZE(PLON)) :: ZLON
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!----------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('GET_MESH_INDEX_LONLAT_REG',0,ZHOOK_HANDLE)
IF (.NOT. ALLOCATED(XLATLIM)) THEN
!
!*    1.     Uncode parameters of the grid
!            -----------------------------
!
  CALL GET_GRIDTYPE_LONLAT_REG(PGRID_PAR,ZLONMIN,ZLONMAX, &
                                 ZLATMIN,ZLATMAX,NLON,NLAT  )  
!
!----------------------------------------------------------------------------
!
!*    2.     Limits of grid meshes
!            ---------------------
!
  ZDLON = (ZLONMAX-ZLONMIN) / FLOAT(NLON)
  ZDLAT = (ZLATMAX-ZLATMIN) / FLOAT(NLAT)
!
  ALLOCATE(XLONLIM(NLON+1))
  DO JI=1,NLON+1
    XLONLIM(JI) = ZLONMIN + FLOAT(JI-1)*ZDLON
  END DO

  ALLOCATE(XLATLIM(NLAT+1))
  DO JI=1,NLAT+1
    XLATLIM(JI) = ZLATMIN + FLOAT(JI-1)*ZDLAT
  END DO
!
  XLON0 = 0.5*(ZLONMIN+ZLONMAX)
!
END IF
!----------------------------------------------------------------------------
!
!*    3.     Reshifts the longitudes with respect to projection reference point
!            ------------------------------------------------------------------
!
!
ZLON(:) = PLON(:)+NINT((XLON0-PLON(:))/360.)*360.
!
!----------------------------------------------------------------------------
!
!*    4.     Localisation of the data points on (x,y) grid
!            ---------------------------------------------
!
IF (KL/=NLON*NLAT) THEN
  KINDEX = 0
  KISSOX = 0
  KISSOY = 0
END IF
!
!
DO JL=1,SIZE(PLAT)
  IF (     ZLON(JL)<XLONLIM(1) .OR. ZLON(JL)>=XLONLIM(NLON+1) &
        .OR. PLAT(JL)<XLATLIM(1) .OR. PLAT(JL)>=XLATLIM(NLAT+1) ) THEN  
    KINDEX(JL) = 0
    IF (KSSO/=0) THEN
      KISSOX(JL) = 0
      KISSOY(JL) = 0
    END IF
    CYCLE
  END IF
  JI = COUNT (ZLON(JL)>=XLONLIM(:))
  JJ = COUNT (PLAT(JL)>=XLATLIM(:))
  KINDEX(JL) = (JJ-1) * NLON + JI
!
!
!*    6.     Localisation of the data points on in the subgrid of this mesh
!            --------------------------------------------------------------
!
  IF (KSSO/=0) THEN
    KISSOX(JL) = 1 + INT( FLOAT(KSSO) * (ZLON(JL)-XLONLIM(JI))/(XLONLIM(JI+1)-XLONLIM(JI)) )
    KISSOY(JL) = 1 + INT( FLOAT(KSSO) * (PLAT(JL)-XLATLIM(JJ))/(XLATLIM(JJ+1)-XLATLIM(JJ)) )
  END IF
END DO
IF (LHOOK) CALL DR_HOOK('GET_MESH_INDEX_LONLAT_REG',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE GET_MESH_INDEX_LONLAT_REG
