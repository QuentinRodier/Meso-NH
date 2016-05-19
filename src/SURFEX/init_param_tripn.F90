!     #########
      SUBROUTINE INIT_PARAM_TRIP_n (KLUOUT,HFILE,KLON,KLAT,HTITLE,HTIMEUNIT)
!     #######################################################################
!
!!****  *INIT_PARAM_TRIP_n*  
!!
!!    PURPOSE
!!    -------
!
!     Define the name and unit of each trip parameter.
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
USE MODD_TRIP_n,   ONLY : CGROUNDW, CVIT, LFLOODT, LNCPRINT
USE MODD_TRIP_PAR, ONLY : XTRIP_UNDEF, NTRIPTAB
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
!*      0.2    declarations of output variables
!
 CHARACTER(LEN=nf_max_name), DIMENSION(:), ALLOCATABLE :: YVNAME  !Name of each output variable
 CHARACTER(LEN=nf_max_name), DIMENSION(:), ALLOCATABLE :: YVLNAME !Long name of each output variables
 CHARACTER(LEN=nf_max_name), DIMENSION(:), ALLOCATABLE :: YUNIT   !Unit of each output variable
!
 CHARACTER(LEN=nf_max_name) :: YFILE,YTITLE,YTIMEUNIT
!
LOGICAL, DIMENSION(:), ALLOCATABLE ::  LZLEN
!
REAL, DIMENSION(:), ALLOCATABLE ::  ZLON
REAL, DIMENSION(:), ALLOCATABLE ::  ZLAT
!
INTEGER :: INPARAM, INCID, INUM
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!
! * Number of output variable
!
IF (LHOOK) CALL DR_HOOK('INIT_PARAM_TRIP_N',0,ZHOOK_HANDLE)
INPARAM = 6
IF(CGROUNDW/='DEF') INPARAM = INPARAM + 1
IF(CVIT=='VAR')INPARAM = INPARAM + 3
IF(LFLOODT)INPARAM = INPARAM + 5
!
! * Allocate netcdf file attributs
!
ALLOCATE(YVNAME  (INPARAM))
ALLOCATE(YVLNAME (INPARAM))
ALLOCATE(YUNIT   (INPARAM))
ALLOCATE(LZLEN   (INPARAM))
!
ALLOCATE(ZLON(KLON))
ALLOCATE(ZLAT(KLAT))
!
! * Initialyse netcdf file attributs
!
YVNAME (1) = 'FLOWDIR                    '
YVLNAME(1) = 'Flow direction             '
YUNIT  (1) = '-                          '
LZLEN  (1) = .FALSE.
!
YVNAME (2) = 'RIVSEQ                     '
YVLNAME(2) = 'River sequence             '
YUNIT  (2) = '-                          '
LZLEN  (2) = .FALSE.
!
YVNAME (3) = 'RIVLEN                     '
YVLNAME(3) = 'River length               '
YUNIT  (3) = 'm                          '
LZLEN  (3) = .FALSE.
!
YVNAME (4) = 'NUM_BAS                    '
YVLNAME(4) = 'Trip basin reference number'
YUNIT  (4) = '-                          '
LZLEN  (4) = .FALSE.
!
YVNAME (5) = 'ELEV                       '
YVLNAME(5) = 'Surface elevation along the river channel'
YUNIT  (5) = 'm                          '
LZLEN  (5) = .FALSE.
!
YVNAME (6) = 'DR_AREA                    '
YVLNAME(6) = 'Trip drainage area         '
YUNIT  (6) = 'm2                         '
LZLEN  (6) = .FALSE.
!
INUM = 6
!
IF(CGROUNDW/='DEF')THEN
!   
INUM = INUM + 1
!
YVNAME (INUM) = 'TAUG                      '
YVLNAME(INUM) = 'Groundwater transfert time (0=permafrost area)'
YUNIT  (INUM) = 'days                      '
LZLEN  (INUM) = .FALSE.
!
ENDIF
!
IF(CVIT=='VAR')THEN
!
INUM = INUM + 1
YVNAME (INUM) = 'N_RIV                     '
YVLNAME(INUM) = 'Manning coefficient       '
YUNIT  (INUM) = '-                         '
LZLEN  (INUM) = .FALSE.
!
INUM = INUM + 1
YVNAME (INUM) = 'WIDTHRIV                  '
YVLNAME(INUM) = 'Stream river width        '
YUNIT  (INUM) = 'm                         '
LZLEN  (INUM) = .FALSE.
!
INUM = INUM + 1
YVNAME (INUM) = 'SLOPERIV                  '
YVLNAME(INUM) = 'Stream River slope        '
YUNIT  (INUM) = 'm/m                       '
LZLEN  (INUM) = .FALSE.
!
ENDIF
!
IF(LFLOODT)THEN
!
INUM = INUM + 1
YVNAME (INUM) = 'RIVDEPTH                  '
YVLNAME(INUM) = 'Stream River Depth (Hc)   '
YUNIT  (INUM) = 'm                         '
LZLEN  (INUM) = .FALSE.
!
INUM = INUM + 1
YVNAME (INUM) = 'NFLOOD                    '
YVLNAME(INUM) = 'Manning coef for flood    '
YUNIT  (INUM) = '-                         '
LZLEN  (INUM) = .FALSE.
!
INUM = INUM + 1
YVNAME (INUM) = 'TABF                      '
YVLNAME(INUM) = 'Potential flood fraction  '
YUNIT  (INUM) = '-                         '
LZLEN  (INUM) = .TRUE.
!
INUM = INUM + 1
YVNAME (INUM)= 'TABH                      '
YVLNAME(INUM)= 'Topographic height        '
YUNIT  (INUM)= 'm                         '
LZLEN  (INUM)= .TRUE.
!
INUM = INUM + 1
YVNAME (INUM)= 'TABVF                     '
YVLNAME(INUM)= 'Potential flood volume    '
YUNIT  (INUM)= 'kg                        '
LZLEN  (INUM)= .TRUE.
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
IF(ALL(.NOT.LZLEN(:)))THEN
   CALL NCCREATE(KLUOUT,YFILE,YTITLE,YTIMEUNIT,YVNAME,YVLNAME,YUNIT,   &
                   ZLON,ZLAT,XTRIP_UNDEF,LNCPRINT,INCID,.FALSE.)  
ELSE
   CALL NCCREATE(KLUOUT,YFILE,YTITLE,YTIMEUNIT,YVNAME,YVLNAME,YUNIT,   &
                   ZLON,ZLAT,XTRIP_UNDEF,LNCPRINT,INCID,.FALSE.,NTRIPTAB,LZLEN)  
ENDIF
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
IF (LHOOK) CALL DR_HOOK('INIT_PARAM_TRIP_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
END SUBROUTINE INIT_PARAM_TRIP_n
