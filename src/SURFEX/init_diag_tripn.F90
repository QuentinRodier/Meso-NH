!     #########
      SUBROUTINE INIT_DIAG_TRIP_n (KLUOUT,HFILE,KLON,KLAT,HTITLE,HTIMEUNIT,OTIME)
!     #######################################################################
!
!!****  *INIT_DIAG_TRIP_n*  
!!
!!    PURPOSE
!!    -------
!
!     Define the name and unit of each trip output variable.
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
USE MODD_TRIP_n,   ONLY : CGROUNDW, CVIT, LFLOODT, LNCPRINT, LTRIP_DIAG_MISC
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
!*      0.2    declarations of output variables
!
 CHARACTER(LEN=nf_max_name), DIMENSION(:), ALLOCATABLE :: YVNAME  !Name of each output variable
 CHARACTER(LEN=nf_max_name), DIMENSION(:), ALLOCATABLE :: YVLNAME !Long name of each output variables
 CHARACTER(LEN=nf_max_name), DIMENSION(:), ALLOCATABLE :: YUNIT   !Unit of each output variable
!
 CHARACTER(LEN=nf_max_name) :: YFILE,YTITLE,YTIMEUNIT
!
REAL, DIMENSION(:), ALLOCATABLE ::  ZLON
REAL, DIMENSION(:), ALLOCATABLE ::  ZLAT
!
INTEGER :: INDIAG, INCID, INUM
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!
! * Number of output variable
!
IF (LHOOK) CALL DR_HOOK('INIT_DIAG_TRIP_N',0,ZHOOK_HANDLE)
INUM   = 0
INDIAG = 2
IF(LTRIP_DIAG_MISC) INDIAG = INDIAG + 1
IF(CGROUNDW/='DEF') INDIAG = INDIAG + 2
IF(CVIT=='VAR')     INDIAG = INDIAG + 2
IF(LFLOODT)THEN
  INDIAG = INDIAG + 3
  IF(LTRIP_DIAG_MISC) INDIAG = INDIAG + 8
ENDIF
!
! * Allocate netcdf file attributs
!
ALLOCATE(YVNAME  (INDIAG))
ALLOCATE(YVLNAME (INDIAG))
ALLOCATE(YUNIT   (INDIAG))
!
ALLOCATE(ZLON(KLON))
ALLOCATE(ZLAT(KLAT))
!
! * Initialyse netcdf file attributs
!
INUM = INUM + 1
YVNAME (INUM) = 'SURF_STO                  '
YVLNAME(INUM) = 'River storage             '
YUNIT  (INUM) = 'kg m-2                    '
!
INUM = INUM + 1
YVNAME (INUM) = 'QDIS                      '
YVLNAME(INUM) = 'Discharge                 '
YUNIT  (INUM) = 'm3 s-1                    '
!
IF(LTRIP_DIAG_MISC)THEN
  INUM = INUM + 1
  YVNAME (INUM) = 'QSIN                      '
  YVLNAME(INUM) = 'Inflow to the river       '
  YUNIT  (INUM) = 'm3 s-1                    '
ENDIF
!
IF(CGROUNDW/='DEF')THEN
! 
  INUM = INUM + 1
  YVNAME (INUM) = 'GROUND_STO                '
  YVLNAME(INUM) = 'Groundwater storage       '
  YUNIT  (INUM) = 'kg m-2                    '
!     
  INUM = INUM + 1
  YVNAME (INUM) = 'QGF'
  YVLNAME(INUM) = 'Groundwater flow to river '
  YUNIT  (INUM) = 'm3 s-1                    '
!
ENDIF
!
IF(CVIT=='VAR')THEN
!
  INUM = INUM + 1
  YVNAME (INUM) = 'VEL                       '
  YVLNAME(INUM) = 'Stream flow velocity      '
  YUNIT  (INUM) = 'm s-1                     '
!
  INUM = INUM + 1
  YVNAME (INUM) = 'HSTREAM                   '
  YVLNAME(INUM) = 'Stream river depth        '
  YUNIT  (INUM) = 'm                         '
!
ENDIF
!
IF(LFLOODT)THEN
!        
  INUM = INUM + 1
  YVNAME (INUM) = 'FLOOD_STO                 '
  YVLNAME(INUM) = 'Floodplain storage        '
  YUNIT  (INUM) = 'kg m-2                    '
!
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
  IF(LTRIP_DIAG_MISC)THEN
!
    INUM = INUM + 1
    YVNAME (INUM)= 'FSOURCE                      '
    YVLNAME(INUM)= 'Floodplains source (Pf-Ef-If)'
    YUNIT  (INUM)= 'kg m-2                       '
!
    INUM = INUM + 1
    YVNAME (INUM)= 'VFIN                      '
    YVLNAME(INUM)= 'River to flood velocity   '
    YUNIT  (INUM)= 'm s-1                     '
!
    INUM = INUM + 1
    YVNAME (INUM)= 'QRF                       '
    YVLNAME(INUM)= 'River flow to floodplain  '
    YUNIT  (INUM)= 'm3 s-1                    '
!
    INUM = INUM + 1
    YVNAME (INUM)= 'VFOUT                     '
    YVLNAME(INUM)= 'Flood to river velocity   '
    YUNIT  (INUM)= 'm s-1                     '
!
    INUM = INUM + 1
    YVNAME (INUM)= 'QFR                       '
    YVLNAME(INUM)= 'Flood flow to river       '
    YUNIT  (INUM)= 'm3 s-1                    '
!
    INUM = INUM + 1
    YVNAME (INUM)= 'HSF                         '
    YVLNAME(INUM)= 'River-Flood depth comparison'
    YUNIT  (INUM)= 'm                           '
!
    INUM = INUM + 1
    YVNAME (INUM)= 'WF                          '
    YVLNAME(INUM)= 'Flood width during dt       '
    YUNIT  (INUM)= 'm                           '
!
    INUM = INUM + 1
    YVNAME (INUM)= 'LF                          '
    YVLNAME(INUM)= 'Flood lenght during dt      '
    YUNIT  (INUM)= 'm                           '
!
  ENDIF
!
ENDIF
!
! * Allocate and initialyse diagnostic variables
!
ALLOCATE(XDIAG_SURF_STO(KLON,KLAT))
ALLOCATE(XDIAG_QDIS    (KLON,KLAT))
XDIAG_SURF_STO(:,:) = 0.0
XDIAG_QDIS    (:,:) = 0.0
!
IF(LTRIP_DIAG_MISC)THEN
  ALLOCATE(XDIAG_QIN(KLON,KLAT))
  XDIAG_QIN(:,:) = 0.0
ELSE
  ALLOCATE(XDIAG_QIN(0,0))
ENDIF
!
IF(CGROUNDW/='DEF')THEN
  ALLOCATE(XDIAG_GROUND_STO (KLON,KLAT))
  ALLOCATE(XDIAG_QGF        (KLON,KLAT))
  XDIAG_GROUND_STO (:,:) = 0.0
  XDIAG_QGF        (:,:) = 0.0
ELSE
  ALLOCATE(XDIAG_GROUND_STO (0,0))
  ALLOCATE(XDIAG_QGF        (0,0))
ENDIF
!
IF(CVIT=='VAR')THEN
  ALLOCATE(XDIAG_VEL(KLON,KLAT))
  ALLOCATE(XDIAG_HS (KLON,KLAT))
  XDIAG_VEL(:,:) = 0.0
  XDIAG_HS (:,:) = 0.0
ELSE
  ALLOCATE(XDIAG_VEL(0,0))
  ALLOCATE(XDIAG_HS (0,0))
ENDIF        
!
IF(LFLOODT)THEN
  ALLOCATE(XDIAG_FLOOD_STO (KLON,KLAT))
  ALLOCATE(XDIAG_FF        (KLON,KLAT))
  ALLOCATE(XDIAG_HF        (KLON,KLAT))
  XDIAG_FLOOD_STO (:,:) = 0.0
  XDIAG_FF        (:,:) = 0.0
  XDIAG_HF        (:,:) = 0.0
  IF(LTRIP_DIAG_MISC)THEN
    ALLOCATE(XDIAG_QFR   (KLON,KLAT))
    ALLOCATE(XDIAG_QRF   (KLON,KLAT))
    ALLOCATE(XDIAG_VFIN  (KLON,KLAT))
    ALLOCATE(XDIAG_VFOUT (KLON,KLAT))
    ALLOCATE(XDIAG_WF    (KLON,KLAT))
    ALLOCATE(XDIAG_LF    (KLON,KLAT))
    ALLOCATE(XDIAG_HSF   (KLON,KLAT))
    ALLOCATE(XDIAG_SOURCE(KLON,KLAT))
    XDIAG_QFR   (:,:) = 0.0
    XDIAG_QRF   (:,:) = 0.0
    XDIAG_VFIN  (:,:) = 0.0
    XDIAG_VFOUT (:,:) = 0.0
    XDIAG_WF    (:,:) = 0.0
    XDIAG_LF    (:,:) = 0.0
    XDIAG_HSF   (:,:) = 0.0
    XDIAG_SOURCE(:,:) = 0.0
  ELSE
    ALLOCATE(XDIAG_QFR   (0,0))
    ALLOCATE(XDIAG_QRF   (0,0))
    ALLOCATE(XDIAG_VFIN  (0,0))
    ALLOCATE(XDIAG_VFOUT (0,0))
    ALLOCATE(XDIAG_WF    (0,0))
    ALLOCATE(XDIAG_LF    (0,0))
    ALLOCATE(XDIAG_HSF   (0,0))
    ALLOCATE(XDIAG_SOURCE(0,0))
  ENDIF
ELSE
  ALLOCATE(XDIAG_FLOOD_STO (0,0))
  ALLOCATE(XDIAG_HF        (0,0))
  ALLOCATE(XDIAG_FF        (0,0))
  ALLOCATE(XDIAG_QFR       (0,0))
  ALLOCATE(XDIAG_QRF       (0,0))
  ALLOCATE(XDIAG_VFIN      (0,0))
  ALLOCATE(XDIAG_VFOUT     (0,0))
  ALLOCATE(XDIAG_WF        (0,0))
  ALLOCATE(XDIAG_LF        (0,0))
  ALLOCATE(XDIAG_HSF       (0,0))
  ALLOCATE(XDIAG_SOURCE    (0,0))
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
IF (LHOOK) CALL DR_HOOK('INIT_DIAG_TRIP_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
END SUBROUTINE INIT_DIAG_TRIP_n
