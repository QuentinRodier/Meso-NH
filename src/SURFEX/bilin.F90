!     #########
     SUBROUTINE BILIN (KLUOUT,PX1,PY1,PFIELD1,PX2,PY2,PFIELD2,OINTERP)
!     #########################################################################
!
!!****  *BILINEAR * - subroutine to interpolate surface FIELD
!!
!!    PURPOSE
!!    -------
!!
!!
!!**  METHOD
!!    ------
!!
!!     Interpolation is bilinear, and uses 9 grid points, located in the
!!   center of model 1 grid mesh, and at the boundaries of this grid
!!   mesh (2 X limits, 2 Y limits and 4 corners).
!!     This implies that the grid mesh values located around the model 1
!!   grid mesh are not used directly. The values at the boundaries of the
!!   grid mesh are defined by the average between the middle point
!!   (this grid mesh value), and the one in the considered direction.
!!   So the eight grid meshes around the considered grid mesh are used
!!   equally.
!!     This is important to note that these average values are erased
!!   and replaced by zero if they are at the limit of any grid
!!   mesh with the zero value. This allows to insure zero value in model 2
!!   grid meshes where there was not the considered class in corresponding
!!   model 1 grid mesh, and to insure continuity of the FIELD type
!!   at such boundaries.
!!
!!
!!    The arrays and array index are defined on the following (model1) grid:
!!
!!
!!        XFIELD                    XFIELD                    XFIELD
!!          *                         *                         *
!!       i-1,j+1                    i,j+1                    i+1,j+1
!!
!!
!!
!!                   ZFIELD_XY     ZFIELD_Y    ZFIELD_XY
!!                       *            *            *
!!                     i,j+1        i,j+1       i+1,j+1
!!
!!
!!
!!        XFIELD      ZFIELD_X      XFIELD      ZFIELD_X      XFIELD
!!          *            *            *            *            *
!!        i-1,j         i,j          i,j         i+1,j        i+1,j
!!
!!
!!
!!                    ZFIELD_XY     ZFIELD_Y    ZFIELD_XY
!!                       *            *            *
!!                      i,j          i,j         i+1,j
!!
!!
!!
!!        XFIELD                    XFIELD                    XFIELD
!!          *                         *                         *
!!       i-1,j-1                    i,j-1                    i+1,j-1
!!
!!
!!
!!
!!
!!    AUTHOR
!!    ------
!!
!!       V. Masson     * METEO-FRANCE *
!!
!!    MODIFICATIONS
!!    -------------
!!
!!      Original     01/2004
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODI_HOR_EXTRAPOL_SURF
!
USE MODD_SURF_PAR,   ONLY : XUNDEF
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of dummy arguments :
!
INTEGER,              INTENT(IN)  :: KLUOUT  ! output listing logical unit
REAL, DIMENSION(:),   INTENT(IN)  :: PX1     ! X coordinate of the regular input grid
REAL, DIMENSION(:),   INTENT(IN)  :: PY1     ! Y coordinate of the regular input grid
REAL, DIMENSION(:,:), INTENT(IN)  :: PFIELD1 ! FIELD on regular input grid
REAL, DIMENSION(:),   INTENT(IN)  :: PX2     ! X coordinate of all points of output grid
REAL, DIMENSION(:),   INTENT(IN)  :: PY2     ! Y coordinate of all points of output grid
REAL, DIMENSION(:),   INTENT(OUT) :: PFIELD2 ! FIELD on model 2
LOGICAL, DIMENSION(:),INTENT(IN)  :: OINTERP ! .true. where physical value is needed
!
!
!*       0.2    Declarations of local variables for print on FM file
!
! 
REAL, DIMENSION (:,:), ALLOCATABLE   :: ZW       ! weight. 1 if defined, 
                                                 !         0 if FIELD=XUNDEF
REAL, DIMENSION (:,:), ALLOCATABLE   :: ZW_FIELD ! weight. * FIELD
REAL, DIMENSION (:,:), ALLOCATABLE   :: ZFIELD_X ! FIELD at mesh interface
REAL, DIMENSION (:,:), ALLOCATABLE   :: ZFIELD_Y ! FIELD at mesh interface
REAL, DIMENSION (:,:), ALLOCATABLE   :: ZFIELD_XY! FIELD at mesh corner
REAL, DIMENSION (SIZE(PX1)+1)        :: ZX       ! X coordinate of left   limit of input meshes
REAL, DIMENSION (SIZE(PY1)+1)        :: ZY       ! Y coordinate of bottom limit of input meshes
REAL, DIMENSION (:),   ALLOCATABLE   :: ZX1      ! input X coordinate of all points
REAL, DIMENSION (:),   ALLOCATABLE   :: ZY1      ! input Y coordinate of all points
REAL, DIMENSION (:),   ALLOCATABLE   :: ZFIELD1  ! input field of all points
!
REAL                                 :: ZC1_X    ! coefficient for left   points
REAL                                 :: ZC2_X    ! coefficient for middle points
REAL                                 :: ZC3_X    ! coefficient for right  points
REAL                                 :: ZC1_Y    ! coefficient for bottom points
REAL                                 :: ZC2_Y    ! coefficient for middle points
REAL                                 :: ZC3_Y    ! coefficient for top    points
!
INTEGER                              :: IIU       ! model 1 X size
INTEGER                              :: IJU       ! model 1 Y size
!
INTEGER                              :: JI        ! grid 1 x index
INTEGER                              :: JJ        ! grid 1 y index
INTEGER                              :: IIN       ! loop counter on all input points
!
INTEGER                              :: JL        ! grid 2 index
!
REAL                                 :: ZEPS=1.E-3
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('BILIN',0,ZHOOK_HANDLE)
IIU=SIZE(PFIELD1,1)
IJU=SIZE(PFIELD1,2)
!
!* weighting factor
!
ALLOCATE(ZW(IIU,IJU))
WHERE (PFIELD1/=XUNDEF)
  ZW=1.
ELSEWHERE
  ZW=0.
END WHERE
!
!* weighted FIELD
!
ALLOCATE(ZW_FIELD(IIU,IJU))
ZW_FIELD=ZW*PFIELD1
!
!-------------------------------------------------------------------------------
!
!*       1.     FIELD type at grid mesh interfaces (in X directions)
!               ----------------------------------
!
ALLOCATE(ZFIELD_X(IIU+1,IJU))
!
!*       1.1    Standard case
!               -------------
!
IF (IIU>1)                                                      &
ZFIELD_X(2:IIU  ,:) =  (ZW_FIELD(1:IIU-1,:)+ZW_FIELD(2:IIU,:))  &
                    / MAX(ZW      (1:IIU-1,:)+ZW      (2:IIU,:),ZEPS)  
ZFIELD_X(1      ,:) =         ZW_FIELD(1  ,:)
ZFIELD_X(  IIU+1,:) =         ZW_FIELD(IIU,:)
!
!
!*       1.2    FIELD type value is 0 in the grid mesh
!               --------------------------------------
!
WHERE ( PFIELD1(1:IIU,:)==0.)
  ZFIELD_X(1:IIU,:)  = 0.
END WHERE
!
WHERE ( PFIELD1(1:IIU,:)==0.)
  ZFIELD_X(2:IIU+1,:)  = 0.
END WHERE
!
!-------------------------------------------------------------------------------
!
!*       2.     FIELD type at grid mesh interfaces (in X directions)
!               ----------------------------------
!
ALLOCATE(ZFIELD_Y(IIU,IJU+1))
!
!*       2.1    Standard case
!               -------------
!
IF (IJU>1)                                                     &
ZFIELD_Y(:,2:IJU  ) =  (ZW_FIELD(:,1:IJU-1)+ZW_FIELD(:,2:IJU)) &
                    / MAX(ZW      (:,1:IJU-1)+ZW      (:,2:IJU),ZEPS)  
ZFIELD_Y(:,1      ) =       ZW_FIELD(:,1      )
ZFIELD_Y(:,  IJU+1) =       ZW_FIELD(:,  IJU  )
!
!
!*       2.3    FIELD type value is 0 in the grid mesh
!               --------------------------------------
!
WHERE ( PFIELD1(:,1:IJU)==0.)
  ZFIELD_Y(:,1:IJU)  = 0.
END WHERE
!
WHERE ( PFIELD1(:,1:IJU)==0.)
  ZFIELD_Y(:,2:IJU+1)  = 0.
END WHERE
!
!-------------------------------------------------------------------------------
!
!*       3.     FIELD type at grid mesh corners
!               -------------------------------
!
ALLOCATE(ZFIELD_XY(IIU+1,IJU+1))
!
!*       3.1    Standard case
!               -------------
!

IF (IIU>1 .AND. IJU>1)                                       &
ZFIELD_XY(2:IIU  ,2:IJU  ) =  (  ZW_FIELD(1:IIU-1,1:IJU-1)   &
                                 + ZW_FIELD(1:IIU-1,2:IJU  )   &
                                 + ZW_FIELD(2:IIU  ,1:IJU-1)   &
                                 + ZW_FIELD(2:IIU  ,2:IJU  ) ) &
                           / MAX(  ZW      (1:IIU-1,1:IJU-1)   &
                                 + ZW      (1:IIU-1,2:IJU  )   &
                                 + ZW      (2:IIU  ,1:IJU-1)   &
                                 + ZW      (2:IIU  ,2:IJU  ) , ZEPS)  
!
IF (IJU>1)                                                   &
ZFIELD_XY(1      ,2:IJU  ) = (  ZW_FIELD(1       ,1:IJU-1)   &
                                + ZW_FIELD(1       ,2:IJU  ) ) &
                          / MAX(  ZW      (1       ,1:IJU-1)   &
                                + ZW      (1       ,2:IJU  ) , ZEPS)  
!
IF (IJU>1)                                                   &
ZFIELD_XY(  IIU+1,2:IJU  ) = (  ZW_FIELD(IIU     ,1:IJU-1)   &
                                + ZW_FIELD(IIU     ,2:IJU  ) ) &
                          / MAX(  ZW      (IIU     ,1:IJU-1)   &
                                + ZW      (IIU     ,2:IJU  ) , ZEPS)  
!
IF (IIU>1)                                                   &
ZFIELD_XY(2:IIU  ,1      ) = (   ZW_FIELD(1:IIU-1,1      )   &
                                 + ZW_FIELD(2:IIU  ,1      ) ) &
                          / MAX(   ZW      (1:IIU-1,1      )   &
                                 + ZW      (2:IIU  ,1      ) , ZEPS)  
!
IF (IIU>1)                                                   &
ZFIELD_XY(2:IIU  ,IJU+1  ) = (   ZW_FIELD(1:IIU-1,IJU    )   &
                                 + ZW_FIELD(2:IIU  ,IJU    ) ) &
                          / MAX(   ZW      (1:IIU-1,IJU    )   &
                                 + ZW      (2:IIU  ,IJU    ) , ZEPS)  
!
ZFIELD_XY(1      ,1      ) =       (   PFIELD1(1      ,1      ) )
ZFIELD_XY(IIU+1  ,1      ) =       (   PFIELD1(IIU    ,1      ) )
ZFIELD_XY(1      ,IJU+1  ) =       (   PFIELD1(1      ,IJU    ) )
ZFIELD_XY(IIU+1  ,IJU+1  ) =       (   PFIELD1(IIU    ,IJU    ) )
!
!*       3.2    FIELD type value is 0 in one grid mesh
!               --------------------------------------
!
WHERE ( PFIELD1(1:IIU,1:IJU)==0.)
  ZFIELD_XY(1:IIU,1:IJU)  = 0.
END WHERE
!
WHERE ( PFIELD1(1:IIU,1:IJU)==0.)
  ZFIELD_XY(1:IIU,2:IJU+1)  = 0.
END WHERE
!
WHERE ( PFIELD1(1:IIU,1:IJU)==0.)
  ZFIELD_XY(2:IIU+1,1:IJU)  = 0.
END WHERE
!
WHERE ( PFIELD1(1:IIU,1:IJU)==0.)
  ZFIELD_XY(2:IIU+1,2:IJU+1)  = 0.
END WHERE
!
!-------------------------------------------------------------------------------
!
!*       4.     Coordinates of points between input grid points
!               -----------------------------------------------
!
IF (IIU>1) THEN
  ZX(IIU+1) = 1.5*PX1(IIU)    -0.5*PX1(IIU-1)
  ZX(2:IIU) = 0.5*PX1(1:IIU-1)+0.5*PX1(2:IIU)
  ZX(1)     = 1.5*PX1(1)      -0.5*PX1(2)
ELSE
  ZX(1)     = PX1(1) - 1.E6 ! uniform field in X direction if only 1 point is
  ZX(2)     = PX1(1) + 1.E6 ! available. Arbitrary mesh length of 2000km assumed
END IF
!
IF (IJU>1) THEN
  ZY(IJU+1) = 1.5*PY1(IJU)    -0.5*PY1(IJU-1)
  ZY(2:IJU) = 0.5*PY1(1:IJU-1)+0.5*PY1(2:IJU)
  ZY(1)     = 1.5*PY1(1)      -0.5*PY1(2)
ELSE
  ZY(1)     = PY1(1) - 1.E6 ! uniform field in Y direction if only 1 point is
  ZY(2)     = PY1(1) + 1.E6 ! available. Arbitrary mesh length of 2000km assumed
END IF
!
!-------------------------------------------------------------------------------
!
!*       5.     Interpolation
!               -------------
!
PFIELD2(:) = XUNDEF
!
!* loop on all output grid points
!
DO JL=1,SIZE(PFIELD2,1)
!
!* interpolation weights in X direction
!
  JI=COUNT(ZX(:)<=PX2(JL))
  JI=MAX(MIN(JI,IIU),0)
  IF (PX1(JI)<=PX2(JL)) THEN
    ZC2_X = (PX2(JL)-ZX(JI+1))/(PX1(JI)-ZX(JI+1))
    ZC2_X = MAX(MIN(ZC2_X,1.),0.)
    ZC3_X = 1. - ZC2_X
    ZC1_X = 0.
  ELSE
    ZC2_X = (PX2(JL)-ZX(JI))/(PX1(JI)-ZX(JI))
    ZC2_X = MAX(MIN(ZC2_X,1.),0.)
    ZC1_X = 1. - ZC2_X
    ZC3_X = 0.
  END IF
!
!* interpolation weights in Y direction
!
  JJ=COUNT(ZY(:)<=PY2(JL))
  JJ=MAX(MIN(JJ,IJU),0)
  IF (PY1(JJ)<=PY2(JL)) THEN
    ZC2_Y = (PY2(JL)-ZY(JJ+1))/(PY1(JJ)-ZY(JJ+1))
    ZC2_Y = MAX(MIN(ZC2_Y,1.),0.)
    ZC3_Y = 1. - ZC2_Y
    ZC1_Y = 0.
  ELSE
    ZC2_Y = (PY2(JL)-ZY(JJ))/(PY1(JJ)-ZY(JJ))
    ZC2_Y = MAX(MIN(ZC2_Y,1.),0.)
    ZC1_Y = 1. - ZC2_Y
    ZC3_Y = 0.
  END IF
!
!* interpolation
!
  IF(PFIELD1(JI,JJ) /= XUNDEF)                       &
       PFIELD2(JL) =                                   &
            ZC1_Y * (   ZC1_X * ZFIELD_XY(JI  ,JJ  )   &
                      + ZC2_X * ZFIELD_Y (JI  ,JJ  )   &
                      + ZC3_X * ZFIELD_XY(JI+1,JJ  ) ) &
          + ZC2_Y * (   ZC1_X * ZFIELD_X (JI  ,JJ  )   &
                      + ZC2_X * PFIELD1  (JI  ,JJ  )   &
                      + ZC3_X * ZFIELD_X (JI+1,JJ  ) ) &
          + ZC3_Y * (   ZC1_X * ZFIELD_XY(JI  ,JJ+1)   &
                      + ZC2_X * ZFIELD_Y (JI  ,JJ+1)   &
                      + ZC3_X * ZFIELD_XY(JI+1,JJ+1) )  

END DO
!
!-------------------------------------------------------------------------------
!
DEALLOCATE(ZW)
DEALLOCATE(ZW_FIELD)
DEALLOCATE(ZFIELD_X )
DEALLOCATE(ZFIELD_Y )
DEALLOCATE(ZFIELD_XY)
!
!-------------------------------------------------------------------------------
!
WHERE(ABS(PFIELD2-XUNDEF)<1.E-6) PFIELD2=XUNDEF
!
!------------------------------------------------------------------------------
!
!*     6.   EXTRAPOLATIONS IF SOME POINTS WERE NOT INTERPOLATED
!           ---------------------------------------------------
!
!* no missing point
IF (COUNT(PFIELD2(:)==XUNDEF .AND. OINTERP(:))==0 .AND. LHOOK) CALL DR_HOOK('BILIN',1,ZHOOK_HANDLE)
IF (COUNT(PFIELD2(:)==XUNDEF .AND. OINTERP(:))==0) RETURN
!
!* no data point
IF (COUNT(PFIELD1(:,:)/=XUNDEF)==0 .AND. LHOOK) CALL DR_HOOK('BILIN',1,ZHOOK_HANDLE)
IF (COUNT(PFIELD1(:,:)/=XUNDEF)==0) RETURN
!
WRITE(KLUOUT,*) ' Remaining horizontal extrapolations'
WRITE(KLUOUT,*) ' Total number of input data     : ',COUNT(PFIELD1(:,:)/=XUNDEF)
WRITE(KLUOUT,*) ' Number of points to interpolate: ',COUNT(PFIELD2(:)==XUNDEF .AND. OINTERP(:))
!
!* input grid coordinates
!
ALLOCATE(ZX1(IIU*IJU))
ALLOCATE(ZY1(IIU*IJU))
ALLOCATE(ZFIELD1(IIU*IJU))
IIN=0
DO JJ=1,IJU
  DO JI=1,IIU
    IIN = IIN +1
    ZX1    (IIN) = PX1    (JI)
    ZY1    (IIN) = PY1       (JJ)
    ZFIELD1(IIN) = PFIELD1(JI,JJ)
  END DO
END DO
!
 CALL HOR_EXTRAPOL_SURF(KLUOUT,'XY  ',ZY1,ZX1,ZFIELD1,PY2,PX2,PFIELD2,OINTERP)
!
DEALLOCATE(ZX1)
DEALLOCATE(ZY1)
DEALLOCATE(ZFIELD1)
IF (LHOOK) CALL DR_HOOK('BILIN',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE BILIN
