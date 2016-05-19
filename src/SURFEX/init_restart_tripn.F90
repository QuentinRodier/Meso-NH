!     #########
      SUBROUTINE INIT_RESTART_TRIP_n (KLUOUT,HFILE,KLON,KLAT,HTITLE,HTIMEUNIT,OTIME )
!     #######################################################################
!
!!****  *INIT_RESTART_TRIP_n*  
!!
!!    PURPOSE
!!    -------
!
!     Define the name and unit of each trip restart variables.
!     
!!    REFERENCE
!!    ---------
!!      
!!    AUTHOR
!!    ------
!!	B. Decharme     
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    27/05/08 
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODE_TRIP_NETCDF
!
USE MODD_TRIP_n,   ONLY : CGROUNDW, LFLOODT, LNCPRINT
USE MODD_TRIP_PAR, ONLY : XTRIP_UNDEF
!
USE MODD_DIAG_TRIP_n
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_GET_LONLAT_TRIP
!
IMPLICIT NONE
!
include 'netcdf.inc'
!
!*      0.1    declarations of arguments
!
!
 CHARACTER(LEN=*), INTENT(IN) :: HFILE, HTITLE, HTIMEUNIT
!
INTEGER, INTENT(IN)          :: KLUOUT, KLON, KLAT
!
LOGICAL, INTENT(IN)          :: OTIME
!
!*      0.2    declarations of restart variables
!
 CHARACTER(LEN=nf_max_name), DIMENSION(:), ALLOCATABLE :: YVNAME  !Name of each restart variable
 CHARACTER(LEN=nf_max_name), DIMENSION(:), ALLOCATABLE :: YVLNAME !Long name of each restart variables
 CHARACTER(LEN=nf_max_name), DIMENSION(:), ALLOCATABLE :: YUNIT   !Unit of each restart variable
!
 CHARACTER(LEN=nf_max_name) :: YFILE,YTITLE,YTIMEUNIT
!
REAL, DIMENSION(:), ALLOCATABLE ::  ZLON
REAL, DIMENSION(:), ALLOCATABLE ::  ZLAT
!
INTEGER :: IND, INCID, INUM
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!
! * Number of restart variable
!
IF (LHOOK) CALL DR_HOOK('INIT_RESTART_TRIP_N',0,ZHOOK_HANDLE)
IND = 1
IF(CGROUNDW/='DEF') IND = IND + 1
IF(LFLOODT)IND = IND + 3
!
! * Allocate netcdf file attributs
!
ALLOCATE(YVNAME  (IND))
ALLOCATE(YVLNAME (IND))
ALLOCATE(YUNIT   (IND))
!
ALLOCATE(ZLON(KLON))
ALLOCATE(ZLAT(KLAT))
!
! * Initialyse netcdf file attributs
!
YVNAME (1) = 'SURF_STO                  '
YVLNAME(1) = 'River storage             '
YUNIT  (1) = 'kg                        '
!
INUM = 1
!
IF(CGROUNDW/='DEF')THEN
!        
INUM = INUM + 1
YVNAME (INUM) = 'GROUND_STO                '
YVLNAME(INUM) = 'Groundwater storage       '
YUNIT  (INUM) = 'kg                        '
!
ENDIF
!
IF(LFLOODT)THEN
!
INUM = INUM + 1
YVNAME (INUM) = 'FLOOD_STO                 '
YVLNAME(INUM) = 'Floodplain storage        '
YUNIT  (INUM) = 'kg                        '

INUM = INUM + 1
YVNAME (INUM) = 'FFLOOD_T                  '
YVLNAME(INUM) = 'TRIP flooded fraction     '
YUNIT  (INUM) = '-                         '
!
INUM = INUM + 1
YVNAME (INUM) = 'HFLOOD_T                  '
YVLNAME(INUM) = 'Flood depth               '
YUNIT  (INUM) = 'm                         '
!
ENDIF
!
! * Create netcdf file
!
YFILE     = HFILE(1:LEN_TRIM(HFILE))
YTITLE    = HTITLE(1:LEN_TRIM(HTITLE))
YTIMEUNIT = HTIMEUNIT(1:LEN_TRIM(HTIMEUNIT))
!
 CALL GET_LONLAT_TRIP(KLON,KLAT,ZLON,ZLAT)
!
 CALL NCCREATE(KLUOUT,YFILE,YTITLE,YTIMEUNIT,YVNAME,YVLNAME,YUNIT,ZLON,ZLAT,XTRIP_UNDEF,LNCPRINT,INCID,OTIME)
!
 CALL NCCLOSE(KLUOUT,LNCPRINT,YFILE,INCID)
!
! * Deallocate netcdf file attributs
!
DEALLOCATE(YVNAME  )
DEALLOCATE(YVLNAME )
DEALLOCATE(YUNIT   )
DEALLOCATE(ZLON    )
DEALLOCATE(ZLAT    )
IF (LHOOK) CALL DR_HOOK('INIT_RESTART_TRIP_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
END SUBROUTINE INIT_RESTART_TRIP_n
