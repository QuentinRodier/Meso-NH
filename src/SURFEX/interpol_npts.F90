!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE INTERPOL_NPTS (UG, U, &
                                HPROGRAM,KLUOUT,KNPTS,KCODE,PX,PY,PFIELD,KNEAR_NBR)
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
!!    B. Decharme  2014  scan all point case if gaussien grid or NHALO = 0
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
!
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
!
USE MODD_SURF_PAR,       ONLY : XUNDEF
!
USE MODI_GET_INTERP_HALO
USE MODI_GET_NEAR_MESHES
USE MODI_SUM_ON_ALL_PROCS
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
!
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
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
INTEGER, INTENT(IN) :: KNEAR_NBR
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
INTEGER :: IHALO
INTEGER                            :: JLIST          ! loop counter on points to interpolate
INTEGER                            :: ICOUNT         ! counter
INTEGER                            :: INPTS
INTEGER                            :: ISCAN          ! number of points to scan
INTEGER                            :: ISCAN_ALL      ! number of data points
INTEGER, DIMENSION(SIZE(PFIELD,1)) :: IINDEX       ! list of index to scan
INTEGER, DIMENSION(SIZE(PFIELD,1)) :: IINDEX_ALL   ! list of all data points
INTEGER, DIMENSION(SIZE(KCODE))    :: ISIZE
INTEGER                            :: ISIZE0
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('INTERPOL_NPTS',0,ZHOOK_HANDLE)
!
IL = SIZE(PFIELD,1)
!
IINDEX    (:) = 0
!
 CALL GET_INTERP_HALO(HPROGRAM,UG%CGRID,IHALO)
!
IF(UG%CGRID=='GAUSS'.OR.IHALO==0)THEN
!
  ISIZE(:) = 1.
  ISIZE0 = SUM_ON_ALL_PROCS(HPROGRAM,UG%CGRID,ISIZE(:)==1)
!
  IINDEX_ALL(:) = 0
!        
  ISCAN_ALL = COUNT(KCODE(:)>0)
!
  JS = 0
  DO JD=1,IL
    IF (KCODE(JD)>0) THEN
      JS = JS+1
      IINDEX_ALL(JS) = JD
    END IF
  END DO 
!
ELSEIF (.NOT.ASSOCIATED(UG%NNEAR)) THEN
  ALLOCATE(UG%NNEAR(IL,KNEAR_NBR))
  UG%NNEAR(:,:) = 0
  CALL GET_NEAR_MESHES(UG%CGRID,UG%NGRID_PAR,U%NSIZE_FULL,UG%XGRID_PAR,KNEAR_NBR,UG%NNEAR)
ENDIF
!
DO JL=1,IL

  IF (KCODE(JL)/=0) CYCLE

  ZNDIST (1:KNPTS) = 1.E20
  ZNDIST (0) = 0.
  ZNVAL(0:KNPTS,:) = 0.
  !
  IF(UG%CGRID=='GAUSS'.OR.IHALO==0)THEN
    !
    IF (U%NDIM_FULL/=ISIZE0) THEN
      ! point can not be interpolated further than halo in multiprocessor run
      KCODE(JL) = -4
      CYCLE
    END IF
    INPTS     = KNPTS
    ISCAN     = ISCAN_ALL
    ICOUNT    = ISCAN
    IINDEX(:) = IINDEX_ALL(:)
    !
  ELSE
    !
    ICOUNT = 0
    DO JD=1,KNEAR_NBR
      IF (UG%NNEAR(JL,JD)>0) THEN
        IF (KCODE(UG%NNEAR(JL,JD))>0) THEN  
          ICOUNT = ICOUNT+1
          IINDEX(ICOUNT) = UG%NNEAR(JL,JD)
        END IF
      END IF
    END DO
    !
  ENDIF
  !
  !IF (ICOUNT>=1) THEN
  IF (ICOUNT>=KNPTS) THEN
    ISCAN = ICOUNT
    !INPTS = MIN(ICOUNT,KNPTS)
    INPTS = KNPTS
  ELSEIF (KNEAR_NBR>=U%NDIM_FULL .AND. ICOUNT>=1) THEN
    ISCAN = ICOUNT
    INPTS = ICOUNT      
  ELSE
    KCODE(JL) = -4
    CYCLE
  END IF
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
IF (LHOOK) CALL DR_HOOK('INTERPOL_NPTS',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE INTERPOL_NPTS
