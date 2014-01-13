!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     ################################################
      SUBROUTINE INTERPOL_FIELD2D(HPROGRAM,KLUOUT,KCODE,PFIELD,HFIELD,PDEF,KNPTS)
!     ################################################
!
!!**** *INTERPOL_FIELD* initializes coordinate system for spline interpolation
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
!!    AUTHOR
!!    ------
!!
!!    V. Masson          Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    01/2004
!!    Modification
!!      A. Alias        07/2013 add MODI_ABOR1_SFX
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_SURF_PAR,  ONLY : XUNDEF
USE MODD_SURF_ATM_n, ONLY : NDIM_FULL, NSIZE_FULL
USE MODD_SURF_ATM_GRID_n, ONLY : CGRID
!
USE MODI_GET_GRID_COORD
USE MODI_INTERPOL_NPTS
USE MODI_SUM_ON_ALL_PROCS
USE MODI_ABOR1_SFX
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
 CHARACTER(LEN=6),        INTENT(IN)   :: HPROGRAM ! host program
INTEGER,                 INTENT(IN)   :: KLUOUT   ! output listing
INTEGER,DIMENSION(:),  INTENT(INOUT)  :: KCODE    ! code for each point
                                                  ! >0 point used for interpolation
                                                  !  0 point to interpolate
                                                  ! -1 point not used
                                                  ! -2 point not used
!                                                 ! -3 if spline is no computed
!                                                 ! for this point
REAL,   DIMENSION(:,:),INTENT(INOUT)  :: PFIELD   ! pgd field on grid mesh.
 CHARACTER(LEN=*),        INTENT(IN)   :: HFIELD   ! name of the field for prints
REAL,DIMENSION(:),OPTIONAL, INTENT(IN):: PDEF     ! default value if not enough data
INTEGER, OPTIONAL,       INTENT(IN)   :: KNPTS    ! number of points to interpolate with

!
!*    0.2    Declaration of local variables
!            ------------------------------
!
REAL, DIMENSION(SIZE(KCODE))   :: ZX             ! coordinate used for
REAL, DIMENSION(SIZE(KCODE))   :: ZY             ! splines interpolation
REAL, DIMENSION(SIZE(PFIELD,2)):: ZDEF           ! default value for field
INTEGER                        :: INPTS          ! number of points to interpolate with

!
INTEGER                        :: JLOOP          ! loop counter
!
INTEGER                        :: IERR1          ! number of points interpolated
INTEGER                        :: IERR2          ! number of points not interpolated in the end
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('INTERPOL_FIELD:INTERPOL_FIELD2D',0,ZHOOK_HANDLE)
!
INPTS = 3
IF (PRESENT(KNPTS)) INPTS = KNPTS
!
ZDEF = XUNDEF
IF (PRESENT(PDEF)) ZDEF = PDEF
!
!*    2.     Miscellaneous Initializations
!            -----------------------------
!
 CALL GET_GRID_COORD(KLUOUT,PX=ZX,PY=ZY)
!
!-------------------------------------------------------------------------------
!
!*    5.     Interpolation with 3 nearest points
!            -----------------------------------
!
 CALL INTERPOL_NPTS(HPROGRAM,KLUOUT,INPTS,KCODE,ZX,ZY,PFIELD)
!
!-------------------------------------------------------------------------------
!
!*    6.     Final check
!            -----------
!
IERR1 = SUM_ON_ALL_PROCS(HPROGRAM,CGRID,KCODE(:)==0)
IERR2 = SUM_ON_ALL_PROCS(HPROGRAM,CGRID,KCODE(:)==-4)
!
IF (IERR1>0 .OR. IERR2>0) THEN
  !
  WRITE(KLUOUT,*) ' '
  WRITE(KLUOUT,*) ' Interpolation of field : ',HFIELD
  WRITE(KLUOUT,*) ' ----------------------'
  WRITE(KLUOUT,*) ' '
  WRITE(KLUOUT,*) ' Number of points interpolated with ',INPTS,' nearest points: ', &
                    IERR1
  !
  !
  IF (IERR2>0) THEN
    WRITE(KLUOUT,*) ' Number of points that could not be interpolated : ', &
                      IERR2
    IF (PRESENT(PDEF)) THEN
      DO JLOOP=1,SIZE(PFIELD,2)
        WHERE(KCODE(:)==-4)
          PFIELD(:,JLOOP)=PDEF(JLOOP)
        END WHERE
        WRITE(KLUOUT,*) ' For these points, the default value (',PDEF(JLOOP),') is set.'
      END DO
    ELSE
      WRITE(KLUOUT,*) ' Please provide data with better resolution'
      WRITE(KLUOUT,*) ' Or define a higher halo value             '
      CALL ABOR1_SFX('Some points lack data and are too far away from other points')
    END IF
  END IF
!
END IF
!
IF (LHOOK) CALL DR_HOOK('INTERPOL_FIELD:INTERPOL_FIELD2D',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE INTERPOL_FIELD2D
