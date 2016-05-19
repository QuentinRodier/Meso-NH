!     #########
SUBROUTINE READ_NAM_GRID_TRIP(HPROGRAM)
!######################################
!
!!****  *READ_NAM_GRID_TRIP* - routine to read in namelist the TRIP horizontal grid
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
!!	B. Decharme   *Meteo France*	
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    05/2008 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODE_POS_SURF
USE MODD_TRIP_GRID_n, ONLY : XGRID_TRIP
!
USE MODI_OPEN_NAMELIST
USE MODI_CLOSE_NAMELIST
USE MODI_GET_LUOUT
!
USE MODE_GRID_TRIP
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
 CHARACTER(LEN=6),   INTENT(IN)    :: HPROGRAM   ! calling program
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER :: ILUOUT ! output listing logical unit
INTEGER :: ILUNAM ! namelist file  logical unit
INTEGER :: I
LOGICAL :: GFOUND
REAL    :: ZWORK
!
!
!*       0.3   Declarations of namelist
!              ------------------------
!
REAL    :: TLONMIN  ! minimum longitude (degrees)
REAL    :: TLONMAX  ! maximum longitude (degrees)
REAL    :: TLATMIN  ! minimum latitude  (degrees)
REAL    :: TLATMAX  ! maximum latitude  (degrees)
REAL    :: TRES     ! 1° or 0.5° resolution
!
REAL, DIMENSION(:), ALLOCATABLE ::  ZLON
REAL, DIMENSION(:), ALLOCATABLE ::  ZLAT
!
INTEGER :: ILON,ILAT
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
NAMELIST/NAM_GRID_TRIP/TLONMIN, TLONMAX, TLATMIN, TLATMAX, TRES
!
!------------------------------------------------------------------------------
!
!*       1.    opening of namelist
! 
IF (LHOOK) CALL DR_HOOK('READ_NAM_GRID_TRIP',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
 CALL OPEN_NAMELIST(HPROGRAM,ILUNAM)
!
!---------------------------------------------------------------------------
!
!*       2.    Reading of projection parameters
!              --------------------------------
!
 CALL POSNAM(ILUNAM,'NAM_GRID_TRIP',GFOUND,ILUOUT)
IF (GFOUND) READ(UNIT=ILUNAM,NML=NAM_GRID_TRIP)
!
 CALL CLOSE_NAMELIST(HPROGRAM,ILUNAM)
!
IF(TRES/=0.5.AND.TRES/=1.0.AND.TRES/=0.1)THEN
  WRITE(ILUOUT,*)'Error : The resolution of the TRIP grid must be 1° or 0.5°'
  WRITE(ILUOUT,*)'        or 0.1° over France                               '
  WRITE(ILUOUT,*)'        In NAM_GRID_TRIP, TRES should be 0.1 or 0.5 or 1. '
  STOP
ENDIF
!
!---------------------------------------------------------------------------
!
!*       3.    Number of lattitude and longitude
!
ILON = INT((TLONMAX-TLONMIN)/TRES)
ILAT = INT((TLATMAX-TLATMIN)/TRES)
!
!---------------------------------------------------------------------------
!
!*       4.    lattitude and longitude values
!
ALLOCATE(ZLON(ILON))
ALLOCATE(ZLAT(ILAT))
!
ZWORK = TLONMIN-(TRES/2.)
DO I=1,ILON
   ZWORK   = ZWORK + TRES
   ZLON(I) = ZWORK
ENDDO
!
ZWORK =TLATMIN-(TRES/2.)
DO I=1,ILAT
   ZWORK   = ZWORK + TRES
   ZLAT(I) = ZWORK
ENDDO
!
!---------------------------------------------------------------------------
!
!*       5.    All this information stored into PGRID_TRIP
!              -------------------------------------------
!
ALLOCATE(XGRID_TRIP(7+ILON+ILAT))
XGRID_TRIP(:) = 0.0
!
 CALL PUT_GRID_TRIP(XGRID_TRIP,TLONMIN,TLONMAX,TLATMIN,TLATMAX,TRES,ILON,ILAT,ZLON,ZLAT)
!
DEALLOCATE(ZLON)
DEALLOCATE(ZLAT)
IF (LHOOK) CALL DR_HOOK('READ_NAM_GRID_TRIP',1,ZHOOK_HANDLE)
!
!---------------------------------------------------------------------------
!
END SUBROUTINE READ_NAM_GRID_TRIP
