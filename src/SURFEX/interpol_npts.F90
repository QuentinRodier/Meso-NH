!     #########
      SUBROUTINE INTERPOL_NPTS(HPROGRAM,KLUOUT,KNPTS,KCODE,PX,PY,PFIELD)
!     #########################################################
!
!!**** *INTERPOL_NPTS* interpolates with ###ine f77 programs a 2D field
!!                           from all grid points valid values
!!
!!    PURPOSE
!!    -------
!!
!!    The points are all on only one grid (defined with the coordinates
!!    of all the points). The code to apply for each point is:
!!
!!    KCODE>0 : data point (with field valid for interpolation)
!!    KCODE=-1: point to ignore
!!    KCODE=0 : point to interpolate
!!
!!
!!
!!    METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!
!!    V. Masson          Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    03/2004
!!    Modification
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODD_SURF_ATM_GRID_n,  ONLY : CGRID, XGRID_PAR, NGRID_PAR, NNEAR
USE MODD_SURF_ATM_n,       ONLY : NSIZE_FULL, NDIM_FULL
!
USE MODI_GET_INTERP_HALO
USE MODI_GET_NEAR_MESHES
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
 CHARACTER(LEN=6),      INTENT(IN)     :: HPROGRAM ! host program
INTEGER,               INTENT(IN)     :: KLUOUT   ! output listing
INTEGER,               INTENT(IN)     :: KNPTS    ! number of points to interpolate with
INTEGER,DIMENSION(:),  INTENT(INOUT)  :: KCODE    ! code for each point
                                                  ! >0 point used for interpolation
                                                  !  0 point to interpolate
                                                  ! -1 point not used
                                                  ! -2 point not used
!                                                 ! -3 if spline is no computed
!                                                 ! for this point
REAL,   DIMENSION(:),  INTENT(IN)     :: PX       ! x of each grid mesh.
REAL,   DIMENSION(:),  INTENT(IN)     :: PY       ! y of each grid mesh.
REAL,   DIMENSION(:,:),INTENT(INOUT)  :: PFIELD   ! pgd field on grid mesh.
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER                                     :: IL ! number of points
INTEGER                                     :: JD ! data point index
INTEGER                                     :: JS ! loop counter on data points
INTEGER                                     :: JL ! loop counter on points to initialize
INTEGER                                     :: JP, JPP ! loops counter on KNPTS
REAL :: ZDIST ! square distance between two interpolating and interpolated points
REAL, DIMENSION(0:KNPTS)                :: ZNDIST ! 3 nearest square distances
REAL, DIMENSION(0:KNPTS,SIZE(PFIELD,2)) :: ZNVAL  ! 3 corresponding field values
REAL, DIMENSION(SIZE(PFIELD,2))         :: ZSUM
!
INTEGER                          :: INEAR_NBR      ! number of points to scan
INTEGER                          :: JLIST          ! loop counter on points to interpolate
INTEGER                          :: ICOUNT         ! counter
INTEGER                          :: INPTS
INTEGER                          :: ISCAN          ! number of points to scan
INTEGER, DIMENSION(:), ALLOCATABLE :: IINDEX       ! list of index to scan
INTEGER                            :: IHALO        ! halo available
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('INTERPOL_NPTS',0,ZHOOK_HANDLE)
IL = SIZE(PFIELD,1)
!
 CALL GET_INTERP_HALO(HPROGRAM,CGRID,IHALO)
!
INEAR_NBR = (2*IHALO+1)**2
!
!
ALLOCATE(IINDEX(IL))
IINDEX(:) = 0
!
!
IF (.NOT.ASSOCIATED(NNEAR)) THEN
  ALLOCATE(NNEAR(IL,INEAR_NBR))
  NNEAR(:,:) = 0
  CALL GET_NEAR_MESHES(CGRID,NGRID_PAR,NSIZE_FULL,XGRID_PAR,INEAR_NBR,NNEAR)
ENDIF
!
DO JL=1,IL

  IF (KCODE(JL)/=0) CYCLE

  ZNDIST (1:KNPTS) = 1.E20
  ZNDIST (0) = 0.
  ZNVAL(0:KNPTS,:) = 0.
  !
  ICOUNT = 0
  DO JD=1,INEAR_NBR
    IF (NNEAR(JL,JD)>0) THEN
      IF (KCODE(NNEAR(JL,JD))>0) THEN  
        ICOUNT = ICOUNT+1
        IINDEX(ICOUNT) = NNEAR(JL,JD)
      END IF
    END IF
  END DO
  !
  IF (ICOUNT>=1) THEN
    ISCAN = ICOUNT
    INPTS = MIN(ICOUNT,KNPTS)
  ELSE
    KCODE(JL) = -4
    CYCLE
  END IF
  !
  !
  DO JS=1,ISCAN
    !
    JD = IINDEX(JS)
    !
    ZDIST=  ( ( PX(JD)-PX(JL) ) ** 2 ) + ( ( PY(JD)-PY(JL) ) ** 2 )
    !
    IF ( ZDIST>ZNDIST(INPTS) ) CYCLE
    !
    DO JP = INPTS,1,-1
      !
      IF ( ZDIST>ZNDIST(JP-1) ) THEN
        !
        IF ( JP<INPTS ) THEN
          DO JPP = INPTS,JP+1,-1
            ZNDIST(JPP)  = ZNDIST(JPP-1)
            ZNVAL(JPP,:) = ZNVAL(JPP-1,:)
          ENDDO
        ENDIF
        !
        ZNDIST(JP)  = ZDIST
        ZNVAL(JP,:) = PFIELD(JD,:)
        !
        EXIT
        !
      ENDIF
      !
    ENDDO
    !
  ENDDO
  !
  ZNDIST(:) = SQRT(ZNDIST(:))
  !
  PFIELD(JL,:) = 0.
  ZSUM(:) = 0.
  DO JP = 1, INPTS
    PFIELD(JL,:) = PFIELD(JL,:) + ZNVAL(JP,:)/ZNDIST(JP)
    ZSUM(:) = ZSUM(:) + 1./ZNDIST(JP)
  ENDDO
  PFIELD(JL,:) = PFIELD(JL,:) / ZSUM(:)
  !
END DO
!
DEALLOCATE(IINDEX    )
IF (LHOOK) CALL DR_HOOK('INTERPOL_NPTS',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE INTERPOL_NPTS
