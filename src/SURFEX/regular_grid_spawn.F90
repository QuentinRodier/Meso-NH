!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     ################################################################
      SUBROUTINE REGULAR_GRID_SPAWN(KLUOUT,                               &
                                      KL1, KIMAX1,KJMAX1,PX1,PY1,PDX1,PDY1, &
                                      KXOR, KYOR, KDXRATIO, KDYRATIO,       &
                                      KXSIZE, KYSIZE,                       &
                                      KL2, KIMAX2,KJMAX2,PX2,PY2,PDX2,PDY2  )  
!     ################################################################
!
!!****  *REGULAR_GRID_SPAWN* - routine to read in namelist the horizontal grid
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
!!	V. Masson   *Meteo France*	
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2004 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_SURF_PAR, ONLY : NUNDEF
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
INTEGER,                      INTENT(IN)    :: KLUOUT     ! output listing logical unit
INTEGER,                      INTENT(IN)    :: KL1        ! total number of points KIMAX1 * KJMAX1
INTEGER,                      INTENT(IN)    :: KIMAX1     ! number of points in x direction
INTEGER,                      INTENT(IN)    :: KJMAX1     ! number of points in y direction
REAL, DIMENSION(KL1),         INTENT(IN)    :: PX1        ! X coordinate of all points
REAL, DIMENSION(KL1),         INTENT(IN)    :: PY1        ! Y coordinate of all points
REAL, DIMENSION(KL1),         INTENT(IN)    :: PDX1       ! X mesh size of all points
REAL, DIMENSION(KL1),         INTENT(IN)    :: PDY1       ! Y mesh size of all points
INTEGER,                      INTENT(IN)    :: KXOR       ! position of modified bottom left point
INTEGER,                      INTENT(IN)    :: KYOR       ! according to initial grid
INTEGER,                      INTENT(IN)    :: KXSIZE     ! number of grid meshes in initial grid to be
INTEGER,                      INTENT(IN)    :: KYSIZE     ! covered by the modified grid
INTEGER,                      INTENT(IN)    :: KDXRATIO   ! resolution ratio between modified grid
INTEGER,                      INTENT(IN)    :: KDYRATIO   ! and initial grid
INTEGER,                      INTENT(IN)    :: KL2        ! total number of points KIMAX2 * KJMAX2
INTEGER,                      INTENT(IN)    :: KIMAX2     ! number of points in x direction
INTEGER,                      INTENT(IN)    :: KJMAX2     ! number of points in y direction
REAL, DIMENSION(KL2),         INTENT(OUT)   :: PX2        ! X coordinate of all points
REAL, DIMENSION(KL2),         INTENT(OUT)   :: PY2        ! Y coordinate of all points
REAL, DIMENSION(KL2),         INTENT(OUT)   :: PDX2       ! X mesh size of all points
REAL, DIMENSION(KL2),         INTENT(OUT)   :: PDY2       ! Y mesh size of all points
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
!* initial grid
!
REAL, DIMENSION(:),   ALLOCATABLE :: ZXM1     ! X coordinate of center of mesh (IIMAX1   points)
REAL, DIMENSION(:),   ALLOCATABLE :: ZYM1     ! Y coordinate of center of mesh (IJMAX1   points)
REAL, DIMENSION(:),   ALLOCATABLE :: ZXHAT1   ! X coordinate of left side      (IIMAX1+1 points)
REAL, DIMENSION(:),   ALLOCATABLE :: ZYHAT1   ! Y coordinate of bottom side    (IJMAX1+1 points)
!
!* new grid
!
REAL, DIMENSION(:),   ALLOCATABLE :: ZXHAT2   ! X coordinate of left side      (IIMAX2 points)
REAL, DIMENSION(:),   ALLOCATABLE :: ZYHAT2   ! Y coordinate of bottom side    (IJMAX2 points)
!
!* other variables
!
INTEGER     :: JL            ! loop counter
INTEGER     :: JI,JJ         ! loop controls relatively to modified grid
INTEGER     :: JIBOX,JJBOX   ! grid mesh relatively to initial grid
REAL        :: ZCOEF         ! ponderation coefficient for linear interpolation
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!------------------------------------------------------------------------------
!
!*       1.    Coherence tests
!              ---------------
!
!* tests
!
IF (LHOOK) CALL DR_HOOK('REGULAR_GRID_SPAWN',0,ZHOOK_HANDLE)
IF ( KXOR+KXSIZE-1 > KIMAX1 ) THEN
  WRITE(KLUOUT,*) 'spawned domain is not contained in the input domain'
  WRITE(KLUOUT,*) 'IXOR = ', KXOR, ' IXSIZE = ', KXSIZE,&
                    ' with NIMAX(file) = ', KIMAX1  
  CALL ABOR1_SFX('REGULAR_GRID_SPAWN: (1) SPAWNED DOMAIN NOT CONTAINED IN INPUT DOMAIN')
END IF
IF ( KYOR+KYSIZE-1 > KJMAX1 ) THEN
  WRITE(KLUOUT,*) 'spawned domain is not contained in the input domain'
  WRITE(KLUOUT,*) 'IYOR = ', KYOR, ' IYSIZE = ', KYSIZE,&
                    ' with NJMAX(file) = ', KJMAX1  
  CALL ABOR1_SFX('REGULAR_GRID_SPAWN: (2) SPAWNED DOMAIN NOT CONTAINED IN INPUT DOMAIN')
END IF
!
!------------------------------------------------------------------------------
!
!*       2.    Center of mesh coordinate arrays for each direction separately
!              --------------------------------------------------------------
!
ALLOCATE(ZXM1  (KIMAX1))
ALLOCATE(ZYM1  (KJMAX1))
ALLOCATE(ZXHAT1(KIMAX1+1))
ALLOCATE(ZYHAT1(KJMAX1+1))
ALLOCATE(ZXHAT2(KIMAX2+1))
ALLOCATE(ZYHAT2(KJMAX2+1))
!
ZXM1(:) = PX1(1:KIMAX1)
DO JL=1,KL1
  IF (MOD(JL,KIMAX1)==0) ZYM1(JL/KIMAX1) = PY1(JL)
END DO
!
!------------------------------------------------------------------------------
!
!*       3.    side of mesh coordinate arrays for each direction separately
!              ------------------------------------------------------------
!
!
IF (KIMAX1==1) THEN
  ZXHAT1(1) = ZXM1(1) - 0.5 * PDX1(1)
  ZXHAT1(2) = ZXM1(1) + 0.5 * PDX1(1)
ELSE
  ZXHAT1(1) = 1.5 * ZXM1(1) - 0.5 * ZXM1(2)
  DO JI=2,KIMAX1
    ZXHAT1(JI) = 0.5 * ZXM1(JI-1) + 0.5 * ZXM1(JI)
  END DO
  ZXHAT1(KIMAX1+1) = 1.5 * ZXM1(KIMAX1) - 0.5 * ZXM1(KIMAX1-1)
END IF
!
IF (KJMAX1==1) THEN
  ZYHAT1(1) = ZYM1(1) - 0.5 * PDY1(1)
  ZYHAT1(2) = ZYM1(1) + 0.5 * PDY1(1)
ELSE
  ZYHAT1(1) = 1.5 * ZYM1(1) - 0.5 * ZYM1(2)
  DO JJ=2,KJMAX1
    ZYHAT1(JJ) = 0.5 * ZYM1(JJ-1) + 0.5 * ZYM1(JJ)
  END DO
  ZYHAT1(KJMAX1+1) = 1.5 * ZYM1(KJMAX1) - 0.5 * ZYM1(KJMAX1-1)
END IF
!
!------------------------------------------------------------------------------
!
!*       5.    Interpolation of coordinate arrays for each direction separately
!              ----------------------------------------------------------------
!
!* X coordinate array
!
DO JI=1,KIMAX2
  JIBOX=(JI-1)/KDXRATIO + KXOR
  ZCOEF= FLOAT(MOD(JI-1,KDXRATIO))/FLOAT(KDXRATIO)
  ZXHAT2(JI)=(1.-ZCOEF)*ZXHAT1(JIBOX)+ZCOEF*ZXHAT1(JIBOX+1)
END DO
IF (KIMAX2==1) THEN
  ZXHAT2(KIMAX2+1) = ZXHAT2(KIMAX2) + ZXHAT1(JIBOX+1) - ZXHAT1(JIBOX)
ELSE
  ZXHAT2(KIMAX2+1) = 2. * ZXHAT2(KIMAX2) - ZXHAT2(KIMAX2-1)
END IF
!
!
!* Y coordinate array
!
DO JJ=1,KJMAX2
  JJBOX=(JJ-1)/KDYRATIO + KYOR
  ZCOEF= FLOAT(MOD(JJ-1,KDYRATIO))/FLOAT(KDYRATIO)
  ZYHAT2(JJ)=(1.-ZCOEF)*ZYHAT1(JJBOX)+ZCOEF*ZYHAT1(JJBOX+1)
END DO
IF (KJMAX2==1) THEN
  ZYHAT2(KJMAX2+1) = ZYHAT2(KJMAX2) + ZYHAT1(JJBOX+1) - ZYHAT1(JJBOX)
ELSE
  ZYHAT2(KJMAX2+1) = 2. * ZYHAT2(KJMAX2) - ZYHAT2(KJMAX2-1)
END IF
!---------------------------------------------------------------------------
DEALLOCATE(ZXM1)
DEALLOCATE(ZYM1)
DEALLOCATE(ZXHAT1)
DEALLOCATE(ZYHAT1)
!------------------------------------------------------------------------------
!
!*       5.    Coordinate arrays of all points
!              -------------------------------
!
DO JJ=1,KJMAX2
  DO JI=1,KIMAX2
    JL = (JJ-1) * KIMAX2 + JI
      PX2 (JL) = 0.5 * ZXHAT2(JI) + 0.5 * ZXHAT2(JI+1)
      PDX2(JL) = ZXHAT2(JI+1) - ZXHAT2(JI)
      PY2 (JL) = 0.5 * ZYHAT2(JJ) + 0.5 * ZYHAT2(JJ+1)
      PDY2(JL) = ZYHAT2(JJ+1) - ZYHAT2(JJ)
  END DO
END DO
!
!---------------------------------------------------------------------------
DEALLOCATE(ZXHAT2)
DEALLOCATE(ZYHAT2)
IF (LHOOK) CALL DR_HOOK('REGULAR_GRID_SPAWN',1,ZHOOK_HANDLE)
!---------------------------------------------------------------------------
!
END SUBROUTINE REGULAR_GRID_SPAWN
