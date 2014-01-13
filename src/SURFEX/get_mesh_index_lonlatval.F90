!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     ###############################################################
      SUBROUTINE GET_MESH_INDEX_LONLATVAL(KGRID_PAR,KL,PGRID_PAR,PLAT,PLON,KINDEX,KSSO,KISSOX,KISSOY)
!     ###############################################################
!
!!**** *GET_MESH_INDEX_LONLATVAL* get the grid mesh where point (lat,lon) is located
!!
!!    PURPOSE
!!    -------
!!
!!    AUTHOR
!!    ------
!!
!!    E. Martin         Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    10/2007  
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_GET_MESH_INDEX_LONLATVAL, ONLY : XXLIM, XYLIM, XX_MIN, XX_MAX, XY_MIN, &
                                      XY_MAX, XDX, XDY  
USE MODE_GRIDTYPE_LONLATVAL
USE MODD_POINT_OVERLAY
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
INTEGER                           :: ILAMBERT ! Lambert type
!
REAL, DIMENSION(:), ALLOCATABLE   :: ZX       ! X Lambert   coordinate
REAL, DIMENSION(:), ALLOCATABLE   :: ZY       ! Y Lambert   coordinate
!
INTEGER                           :: IVAR
INTEGER                           :: IL       ! Grid dimension
INTEGER                           :: JL       ! loop counter in lambert grid
INTEGER                           :: JI       ! loop counter on input points
REAL, DIMENSION(SIZE(PLON)) :: ZLON
REAL :: XLON0
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!----------------------------------------------------------------------------
!
!
IF (LHOOK) CALL DR_HOOK('GET_MESH_INDEX_LONLATVAL',0,ZHOOK_HANDLE)
IF (.NOT. ALLOCATED(XXLIM)) THEN
!
!*    1.     Uncode parameters of the grid
!            -----------------------------
!
  CALL GET_GRIDTYPE_LONLATVAL(PGRID_PAR,KL=IL)
!
  ALLOCATE(ZX (IL))
  ALLOCATE(ZY (IL))
  ALLOCATE(XDX(IL))
  ALLOCATE(XDY(IL))
  ALLOCATE(XXLIM(IL))
  ALLOCATE(XYLIM(IL))
!
  CALL GET_GRIDTYPE_LONLATVAL(PGRID_PAR,PX=ZX,PY=ZY,PDX=XDX,PDY=XDY)
!
!*    2.     Limits of grid meshes in x and y
!            --------------------------------
!
  XXLIM(:)=ZX(:)-XDX(:)/2.
  XYLIM(:)=ZY(:)-XDY(:)/2.

  XX_MIN = MINVAL(XXLIM)
  XX_MAX = MAXVAL(XXLIM+XDX)
  XY_MIN = MINVAL(XYLIM)
  XY_MAX = MAXVAL(XYLIM+XDY)
  DEALLOCATE(ZX )
  DEALLOCATE(ZY )
  

END IF
!
XLON0 = 0.5*(XX_MIN+XX_MAX)
!
!*    3.     Projection
!            ----------
!
ALLOCATE(ZX (SIZE(PLAT)))
ALLOCATE(ZY (SIZE(PLAT)))
!
  CALL GET_GRIDTYPE_LONLATVAL(PGRID_PAR)
!
  ZLON(:) = PLON(:)+NINT((XLON0-PLON(:))/360.)*360.
!
!*    5.     Localisation of the data points on (x,y) grid
!            ---------------------------------------------
!
KINDEX(:)=0.
!
DO JI=1,SIZE(PLON)

  IF (     ZLON(JI)<XX_MIN .OR. ZLON(JI)>XX_MAX        &
        .OR. PLAT(JI)<XY_MIN .OR. PLAT(JI)>XY_MAX ) THEN  
     KINDEX(JI) = 0
    IF (KSSO/=0) THEN
      KISSOX(JI) = 0
      KISSOY(JI) = 0
    END IF
    XNUM(JI)=0
    CYCLE
  END IF
!
  IVAR=XNUM(JI)
!
  IF (IVAR.NE.0) THEN
          
    DO JL=IVAR,SIZE(XXLIM)
      IF( ZLON(JI)> XXLIM(JL) .AND. ZLON(JI) < XXLIM(JL)+XDX(JL)   &
       .AND. PLAT(JI) > XYLIM(JL) .AND. PLAT(JI) < XYLIM(JL)+XDY(JL) ) THEN  
!
        KINDEX(JI) = JL
        IVAR = JL+1
!
!*    6.     Localisation of the data points in the subgrid of this mesh
!            -----------------------------------------------------------
!
        IF (KSSO/=0) THEN
          KISSOX(JI) = 1 + INT( FLOAT(KSSO) *             &
                           (ZLON(JI)-XXLIM(KINDEX(JI)))/XDX(KINDEX(JI)) )  
          KISSOY(JI) = 1 + INT( FLOAT(KSSO) *             &
                           (PLAT(JI)-XYLIM(KINDEX(JI)))/XDY(KINDEX(JI)) )  
        END IF
        EXIT
      ENDIF
    ENDDO

    IF (IVAR.NE.XNUM(JI)) THEN
      XNUM(JI)=IVAR
    ELSE
      XNUM(JI)=0
    ENDIF
 
  ENDIF 
    
END DO
!
!-------------------------------------------------------------------------------
DEALLOCATE(ZX )
DEALLOCATE(ZY )
IF (LHOOK) CALL DR_HOOK('GET_MESH_INDEX_LONLATVAL',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE GET_MESH_INDEX_LONLATVAL
