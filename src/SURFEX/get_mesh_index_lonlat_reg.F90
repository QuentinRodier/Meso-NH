!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ###############################################################
      SUBROUTINE GET_MESH_INDEX_LONLAT_REG(KGRID_PAR,KSSO,PGRID_PAR,PLAT,PLON,&
                        KINDEX,KISSOX,KISSOY,PVALUE,PNODATA)
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
INTEGER,                       INTENT(IN)    :: KSSO      ! number of subgrid mesh in each direction
REAL,    DIMENSION(:),         INTENT(IN)    :: PGRID_PAR ! grid parameters
REAL,    DIMENSION(:),         INTENT(IN)    :: PLAT      ! latitude of the point
REAL,    DIMENSION(:),         INTENT(IN)    :: PLON      ! longitude of the point
INTEGER, DIMENSION(:,:),       INTENT(OUT)   :: KINDEX    ! index of the grid mesh where the point is
INTEGER, DIMENSION(:,:),       INTENT(OUT)   :: KISSOX    ! X index of the subgrid mesh
INTEGER, DIMENSION(:,:),       INTENT(OUT)   :: KISSOY    ! Y index of the subgrid mesh
!
REAL, DIMENSION(:), OPTIONAL, INTENT(IN)    :: PVALUE  ! value of the point to add
REAL, OPTIONAL, INTENT(IN) :: PNODATA
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
REAL :: ZNODATA
!
REAL, DIMENSION(SIZE(PLAT))       :: ZVALUE
!
REAL, DIMENSION(SIZE(PLON)) :: ZLON
!
INTEGER, DIMENSION(SIZE(PLAT))    :: ICI, ICJ
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!----------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('GET_MESH_INDEX_LONLAT_REG_1',0,ZHOOK_HANDLE)
!
IF (PRESENT(PVALUE) .AND. PRESENT(PNODATA)) THEN
  ZVALUE(:) = PVALUE(:)
  ZNODATA = PNODATA
ELSE
  ZVALUE(:) = 1
  ZNODATA = 0
ENDIF
!
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
IF (LHOOK) CALL DR_HOOK('GET_MESH_INDEX_LONLAT_REG_1',1,ZHOOK_HANDLE)
IF (LHOOK) CALL DR_HOOK('GET_MESH_INDEX_LONLAT_REG_2',0,ZHOOK_HANDLE)
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
IF (SIZE(PLAT)/=NLON*NLAT) THEN
  KINDEX = 0
  KISSOX = 0
  KISSOY = 0
END IF
!
IF (LHOOK) CALL DR_HOOK('GET_MESH_INDEX_LONLAT_REG_2',1,ZHOOK_HANDLE)
IF (LHOOK) CALL DR_HOOK('GET_MESH_INDEX_LONLAT_REG_3',0,ZHOOK_HANDLE)
!
ICI(:) = 0
ICJ(:) = 0
!$OMP PARALLEL DO PRIVATE(JL,JJ)
DO JL=1,SIZE(PLAT)
  !
  IF (ZVALUE(JL)==ZNODATA) CYCLE
  ! 
  DO JJ=SIZE(XLONLIM),1,-1
    IF (XLONLIM(JJ)<=ZLON(JL)) THEN
      ICI(JL) = JJ
      EXIT
    ENDIF
  ENDDO
  DO JJ=SIZE(XLATLIM),1,-1
    IF (XLATLIM(JJ)<=PLAT(JL)) THEN
      ICJ(JL) = JJ
      EXIT
    ENDIF
  ENDDO
  !
ENDDO
!$OMP END PARALLEL DO
!
IF (LHOOK) CALL DR_HOOK('GET_MESH_INDEX_LONLAT_REG_3',1,ZHOOK_HANDLE)
IF (LHOOK) CALL DR_HOOK('GET_MESH_INDEX_LONLAT_REG_4',0,ZHOOK_HANDLE)
!
KINDEX(:,:) = 0

DO JL=1,SIZE(PLAT)
  !
  IF (ZVALUE(JL)==ZNODATA) CYCLE
  ! 
  IF (     ZLON(JL)<XLONLIM(1) .OR. ZLON(JL)>=XLONLIM(NLON+1) &
        .OR. PLAT(JL)<XLATLIM(1) .OR. PLAT(JL)>=XLATLIM(NLAT+1) ) THEN

    IF (KSSO/=0) THEN
      KISSOX(1,JL) = 0
      KISSOY(1,JL) = 0
    END IF
    CYCLE

  END IF

  JI = ICI(JL)
  JJ = ICJ(JL)
  KINDEX(1,JL) = (JJ-1) * NLON + JI
!
!
!*    6.     Localisation of the data points on in the subgrid of this mesh
!            --------------------------------------------------------------
!
  IF (KSSO/=0) THEN
    KISSOX(1,JL) = 1 + INT( FLOAT(KSSO) * (ZLON(JL)-XLONLIM(JI))/(XLONLIM(JI+1)-XLONLIM(JI)) )
    KISSOY(1,JL) = 1 + INT( FLOAT(KSSO) * (PLAT(JL)-XLATLIM(JJ))/(XLATLIM(JJ+1)-XLATLIM(JJ)) )
  END IF
END DO
IF (LHOOK) CALL DR_HOOK('GET_MESH_INDEX_LONLAT_REG_4',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE GET_MESH_INDEX_LONLAT_REG
