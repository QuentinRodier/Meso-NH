!MNH_LIC Copyright 2002-2022 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
! Author:
!  P. Tulet    15/02/2002
!
!  Modifications
!  P. Wautelet: 05/2016-04/2018: new data structures and calls for I/O
!  P. Wautelet 13/09/2019: budget: simplify and modernize date/time management
!  P. Wautelet 09/10/2020: Write_diachro: use new datatype tpfields
!  P. Wautelet 03/03/2021: budgets: add tbudiachrometadata type (useful to pass more information to Write_diachro)
!  P. Wautelet 04/02/2022: use TSVLIST to manage metadata of scalar variables
!  P. Wautelet    04/2022: restructure stations for better performance, reduce memory usage and correct some problems/bugs
! --------------------------------------------------------------------------
!      ###########################
MODULE MODE_WRITE_STATION_n
!      ###########################

use modd_parameters, only: NCOMMENTLGTMAX, NMNHNAMELGTMAX, NUNITLGTMAX

implicit none

private

public :: WRITE_STATION_n

CHARACTER(LEN=NCOMMENTLGTMAX), DIMENSION(:), ALLOCATABLE :: CCOMMENT ! comment string
CHARACTER(LEN=NMNHNAMELGTMAX), DIMENSION(:), ALLOCATABLE :: CTITLE   ! title
CHARACTER(LEN=NUNITLGTMAX),    DIMENSION(:), ALLOCATABLE :: CUNIT    ! physical unit

REAL, DIMENSION(:,:,:,:,:,:), ALLOCATABLE :: XWORK6   ! contains temporal serie

contains
!
! #####################################
SUBROUTINE WRITE_STATION_n( TPDIAFILE )
! #####################################
!
!
!****  *WRITE_STATION* - write the stations records in the diachronic file
!
!*      0. DECLARATIONS
!          ------------
!
USE MODD_ALLSTATION_n,    ONLY: LDIAG_SURFRAD
USE MODD_CONF_n,          ONLY: NRR
USE MODD_IO,              ONLY: ISNPROC, ISP, TFILEDATA
USE MODD_MPIF
USE MODD_NSV,             ONLY: nsv
USE MODD_PARAM_n,         ONLY: CRAD, CSURF, CTURB
USE MODD_PRECISION,       ONLY: MNHINT_MPI, MNHREAL_MPI
USE MODD_STATION_n,       only: NUMBSTAT_LOC, TSTATIONS, tstations_time
USE MODD_TYPE_STATPROF,   ONLY: TSTATIONDATA
!
USE MODE_MSG
USE MODE_STATPROF_TOOLS,  ONLY: STATION_ALLOCATE
!
IMPLICIT NONE
!
!
!*      0.1  declarations of arguments
!
TYPE(TFILEDATA),  INTENT(IN) :: TPDIAFILE ! diachronic file to write
!
!-------------------------------------------------------------------------------
!
!       0.2  declaration of local variables
!
INTEGER, PARAMETER :: ITAG = 100
INTEGER :: IERR
INTEGER :: JP, JS
INTEGER :: IDX
INTEGER :: INUMSTAT  ! Total number of stations (for the current model)
INTEGER :: IPACKSIZE ! Size of the ZPACK buffer
INTEGER :: IPOS      ! Position in the ZPACK buffer
INTEGER :: ISTORE
INTEGER, DIMENSION(:), ALLOCATABLE :: INSTATPRC    ! Array to store the number of stations per process (for the current model)
INTEGER, DIMENSION(:), ALLOCATABLE :: ISTATIDS     ! Intermediate array for MPI communication
INTEGER, DIMENSION(:), ALLOCATABLE :: ISTATPRCRANK ! Array to store the ranks of the processes where the stations are
INTEGER, DIMENSION(:), ALLOCATABLE :: IDS          ! Array to store the station number to send
INTEGER, DIMENSION(:), ALLOCATABLE :: IDISP        ! Array to store the displacements for MPI communications
REAL,    DIMENSION(:), ALLOCATABLE :: ZPACK        ! Buffer to store raw data of a station (used for MPI communication)
TYPE(TSTATIONDATA) :: TZSTATION
!
!----------------------------------------------------------------------------

ALLOCATE( INSTATPRC(ISNPROC) )
ALLOCATE( IDS(NUMBSTAT_LOC) )

!Gather number of station present on each process
CALL MPI_ALLGATHER( NUMBSTAT_LOC, 1, MNHINT_MPI, INSTATPRC, 1, MNHINT_MPI, TPDIAFILE%NMPICOMM, IERR )

!Store the identification number of local stations (these numbers are globals)
DO JS = 1, NUMBSTAT_LOC
  IDS(JS) = TSTATIONS(JS)%NID
END DO

ALLOCATE( IDISP(ISNPROC) )
IDISP(1) = 0
DO JP = 2, ISNPROC
  IDISP(JP) = IDISP(JP-1) + INSTATPRC(JP-1)
END DO

INUMSTAT = SUM( INSTATPRC(:) )
ALLOCATE( ISTATIDS(INUMSTAT) )
ALLOCATE( ISTATPRCRANK(INUMSTAT) )

!Gather the list of all the stations of all processes
CALL MPI_ALLGATHERV( IDS(:), NUMBSTAT_LOC, MNHINT_MPI, ISTATIDS(:), INSTATPRC(:), &
                     IDISP(:), MNHINT_MPI, TPDIAFILE%NMPICOMM, IERR )

!Store the rank of each process corresponding to a given station
IDX = 1
ISTATPRCRANK(:) = -1
DO JP = 1, ISNPROC
  DO JS = 1, INSTATPRC(JP)
    ISTATPRCRANK(ISTATIDS(IDX)) = JP
    IDX = IDX + 1
  END DO
END DO

CALL STATION_ALLOCATE( TZSTATION, SIZE( tstations_time%tpdates ) )

!Determine the size of the ZPACK buffer used to transfer station data in 1 MPI communication
IF ( ISNPROC > 1 ) THEN
  ISTORE = SIZE( TSTATIONS_TIME%TPDATES )
  IPACKSIZE = 7
  IPACKSIZE = IPACKSIZE + ISTORE * ( 5 + NRR + NSV )
  IF ( CTURB == 'TKEL') IPACKSIZE = IPACKSIZE + ISTORE !Tke term
  IF ( CRAD /= 'NONE' ) IPACKSIZE = IPACKSIZE + ISTORE !XTSRAD term
  IF ( LDIAG_SURFRAD ) THEN
    IF ( CSURF == 'EXTE' ) IPACKSIZE = IPACKSIZE + ISTORE * 10
    IF ( CRAD /= 'NONE' )  IPACKSIZE = IPACKSIZE + ISTORE * 7
    IPACKSIZE = IPACKSIZE + ISTORE !XSFCO2 term
  END IF

  ALLOCATE( ZPACK(IPACKSIZE) )
END IF

IDX = 1

STATION: DO JS = 1, INUMSTAT
  IF ( ISTATPRCRANK(JS) == TPDIAFILE%NMASTER_RANK ) THEN
    !No communication necessary, the station data is already on the writer process
    IF ( ISP == TPDIAFILE%NMASTER_RANK ) THEN
      TZSTATION = TSTATIONS(IDX)
      IDX = IDX + 1
    END IF
  ELSE
    !The station data is not on the writer process
    IF ( ISP == ISTATPRCRANK(JS) ) THEN
      ! This process has the data and needs to send it to the writer process
      IPOS = 1
      ZPACK(IPOS) = TSTATIONS(IDX)%NID;  IPOS = IPOS + 1
      ZPACK(IPOS) = TSTATIONS(IDX)%XX;   IPOS = IPOS + 1
      ZPACK(IPOS) = TSTATIONS(IDX)%XY;   IPOS = IPOS + 1
      ZPACK(IPOS) = TSTATIONS(IDX)%XZ;   IPOS = IPOS + 1
      ZPACK(IPOS) = TSTATIONS(IDX)%XLON; IPOS = IPOS + 1
      ZPACK(IPOS) = TSTATIONS(IDX)%XLAT; IPOS = IPOS + 1
      ZPACK(IPOS) = TSTATIONS(IDX)%XZS;  IPOS = IPOS + 1
      ZPACK(IPOS:IPOS+ISTORE-1) = TSTATIONS(IDX)%XZON(:); IPOS = IPOS + ISTORE
      ZPACK(IPOS:IPOS+ISTORE-1) = TSTATIONS(IDX)%XMER(:); IPOS = IPOS + ISTORE
      ZPACK(IPOS:IPOS+ISTORE-1) = TSTATIONS(IDX)%XW(:);   IPOS = IPOS + ISTORE
      ZPACK(IPOS:IPOS+ISTORE-1) = TSTATIONS(IDX)%XP(:);   IPOS = IPOS + ISTORE
      IF ( CTURB == 'TKEL') THEN
        ZPACK(IPOS:IPOS+ISTORE-1) = TSTATIONS(IDX)%XTKE(:); IPOS = IPOS + ISTORE
      END IF
      ZPACK(IPOS:IPOS+ISTORE-1) = TSTATIONS(IDX)%XTH(:);  IPOS = IPOS + ISTORE
      ZPACK(IPOS:IPOS+ISTORE*NRR-1) = RESHAPE( TSTATIONS(IDX)%XR(:,:),  [ISTORE*NRR] ); IPOS = IPOS + ISTORE * NRR
      ZPACK(IPOS:IPOS+ISTORE*NSV-1) = RESHAPE( TSTATIONS(IDX)%XSV(:,:), [ISTORE*NSV] ); IPOS = IPOS + ISTORE * NSV
      IF ( CRAD /= 'NONE' ) THEN
        ZPACK(IPOS:IPOS+ISTORE-1) = TSTATIONS(IDX)%XTSRAD(:); IPOS = IPOS + ISTORE
      END IF
      IF ( LDIAG_SURFRAD ) THEN
        IF ( CSURF == 'EXTE') THEN
          ZPACK(IPOS:IPOS+ISTORE-1) = TSTATIONS(IDX)%XT2M;    IPOS = IPOS + ISTORE
          ZPACK(IPOS:IPOS+ISTORE-1) = TSTATIONS(IDX)%XQ2M;    IPOS = IPOS + ISTORE
          ZPACK(IPOS:IPOS+ISTORE-1) = TSTATIONS(IDX)%XHU2M;   IPOS = IPOS + ISTORE
          ZPACK(IPOS:IPOS+ISTORE-1) = TSTATIONS(IDX)%XZON10M; IPOS = IPOS + ISTORE
          ZPACK(IPOS:IPOS+ISTORE-1) = TSTATIONS(IDX)%XMER10M; IPOS = IPOS + ISTORE
          ZPACK(IPOS:IPOS+ISTORE-1) = TSTATIONS(IDX)%XRN;     IPOS = IPOS + ISTORE
          ZPACK(IPOS:IPOS+ISTORE-1) = TSTATIONS(IDX)%XH;      IPOS = IPOS + ISTORE
          ZPACK(IPOS:IPOS+ISTORE-1) = TSTATIONS(IDX)%XLE;     IPOS = IPOS + ISTORE
          ZPACK(IPOS:IPOS+ISTORE-1) = TSTATIONS(IDX)%XGFLUX;  IPOS = IPOS + ISTORE
          ZPACK(IPOS:IPOS+ISTORE-1) = TSTATIONS(IDX)%XLEI;    IPOS = IPOS + ISTORE
        END IF
        IF ( CRAD /= 'NONE' ) THEN
          ZPACK(IPOS:IPOS+ISTORE-1) = TSTATIONS(IDX)%XSWD;    IPOS = IPOS + ISTORE
          ZPACK(IPOS:IPOS+ISTORE-1) = TSTATIONS(IDX)%XSWU;    IPOS = IPOS + ISTORE
          ZPACK(IPOS:IPOS+ISTORE-1) = TSTATIONS(IDX)%XLWD;    IPOS = IPOS + ISTORE
          ZPACK(IPOS:IPOS+ISTORE-1) = TSTATIONS(IDX)%XLWU;    IPOS = IPOS + ISTORE
          ZPACK(IPOS:IPOS+ISTORE-1) = TSTATIONS(IDX)%XSWDIR;  IPOS = IPOS + ISTORE
          ZPACK(IPOS:IPOS+ISTORE-1) = TSTATIONS(IDX)%XSWDIFF; IPOS = IPOS + ISTORE
          ZPACK(IPOS:IPOS+ISTORE-1) = TSTATIONS(IDX)%XDSTAOD; IPOS = IPOS + ISTORE
        END IF
        ZPACK(IPOS:IPOS+ISTORE-1) = TSTATIONS(IDX)%XSFCO2;    IPOS = IPOS + ISTORE
      END IF

      IF ( IPOS /= IPACKSIZE ) &
        call Print_msg( NVERB_WARNING, 'IO', 'WRITE_STATION_n', 'IPOS /= IPACKSIZE (sender side)', OLOCAL = .TRUE. )

      CALL MPI_SEND( TSTATIONS(IDX)%CNAME, LEN(TSTATIONS(IDX)%CNAME), MPI_CHARACTER, TPDIAFILE%NMASTER_RANK - 1, &
                     ITAG, TPDIAFILE%NMPICOMM, IERR )
      CALL MPI_SEND( ZPACK, IPACKSIZE, MNHREAL_MPI, TPDIAFILE%NMASTER_RANK - 1, ITAG, TPDIAFILE%NMPICOMM, IERR )

      IDX = IDX + 1

    ELSE IF ( ISP == TPDIAFILE%NMASTER_RANK ) THEN
      ! This process is the writer and will receive the station data from its owner
      CALL MPI_RECV( TZSTATION%CNAME, LEN(TZSTATION%CNAME), MPI_CHARACTER, &
                                                    ISTATPRCRANK(JS) - 1, ITAG, TPDIAFILE%NMPICOMM, MPI_STATUS_IGNORE, IERR )
      CALL MPI_RECV( ZPACK, IPACKSIZE, MNHREAL_MPI, ISTATPRCRANK(JS) - 1, ITAG, TPDIAFILE%NMPICOMM, MPI_STATUS_IGNORE, IERR )

      IPOS = 1
      TZSTATION%NID  = NINT( ZPACK(IPOS) ); IPOS = IPOS + 1
      TZSTATION%XX   = ZPACK(IPOS);         IPOS = IPOS + 1
      TZSTATION%XY   = ZPACK(IPOS);         IPOS = IPOS + 1
      TZSTATION%XZ   = ZPACK(IPOS);         IPOS = IPOS + 1
      TZSTATION%XLON = ZPACK(IPOS);         IPOS = IPOS + 1
      TZSTATION%XLAT = ZPACK(IPOS);         IPOS = IPOS + 1
      TZSTATION%XZS  = ZPACK(IPOS);         IPOS = IPOS + 1
      TZSTATION%XZON(:) = ZPACK(IPOS:IPOS+ISTORE-1); IPOS = IPOS + ISTORE
      TZSTATION%XMER(:) = ZPACK(IPOS:IPOS+ISTORE-1); IPOS = IPOS + ISTORE
      TZSTATION%XW(:)   = ZPACK(IPOS:IPOS+ISTORE-1); IPOS = IPOS + ISTORE
      TZSTATION%XP(:)   = ZPACK(IPOS:IPOS+ISTORE-1); IPOS = IPOS + ISTORE
      IF ( CTURB == 'TKEL') THEN
        TZSTATION%XTKE(:) = ZPACK(IPOS:IPOS+ISTORE-1); IPOS = IPOS + ISTORE
      END IF
      TZSTATION%XTH(:) = ZPACK(IPOS:IPOS+ISTORE-1);  IPOS = IPOS + ISTORE
      TZSTATION%XR(:,:)  = RESHAPE( ZPACK(IPOS:IPOS+ISTORE*NRR-1), [ ISTORE, NRR ] ); IPOS = IPOS + ISTORE * NRR
      TZSTATION%XSV(:,:) = RESHAPE( ZPACK(IPOS:IPOS+ISTORE*NSV-1), [ ISTORE, NSV ] ); IPOS = IPOS + ISTORE * NSV
      IF ( CRAD /= 'NONE' ) THEN
        TZSTATION%XTSRAD(:) = ZPACK(IPOS:IPOS+ISTORE-1); IPOS = IPOS + ISTORE
      END IF
      IF ( LDIAG_SURFRAD ) THEN
        IF ( CSURF == 'EXTE' ) THEN
          TZSTATION%XT2M    = ZPACK(IPOS:IPOS+ISTORE-1); IPOS = IPOS + ISTORE
          TZSTATION%XQ2M    = ZPACK(IPOS:IPOS+ISTORE-1); IPOS = IPOS + ISTORE
          TZSTATION%XHU2M   = ZPACK(IPOS:IPOS+ISTORE-1); IPOS = IPOS + ISTORE
          TZSTATION%XZON10M = ZPACK(IPOS:IPOS+ISTORE-1); IPOS = IPOS + ISTORE
          TZSTATION%XMER10M = ZPACK(IPOS:IPOS+ISTORE-1); IPOS = IPOS + ISTORE
          TZSTATION%XRN     = ZPACK(IPOS:IPOS+ISTORE-1); IPOS = IPOS + ISTORE
          TZSTATION%XH      = ZPACK(IPOS:IPOS+ISTORE-1); IPOS = IPOS + ISTORE
          TZSTATION%XLE     = ZPACK(IPOS:IPOS+ISTORE-1); IPOS = IPOS + ISTORE
          TZSTATION%XGFLUX  = ZPACK(IPOS:IPOS+ISTORE-1); IPOS = IPOS + ISTORE
          TZSTATION%XLEI    = ZPACK(IPOS:IPOS+ISTORE-1); IPOS = IPOS + ISTORE
        END IF
        IF ( CRAD /= 'NONE' ) THEN
          TZSTATION%XSWD    = ZPACK(IPOS:IPOS+ISTORE-1); IPOS = IPOS + ISTORE
          TZSTATION%XSWU    = ZPACK(IPOS:IPOS+ISTORE-1); IPOS = IPOS + ISTORE
          TZSTATION%XLWD    = ZPACK(IPOS:IPOS+ISTORE-1); IPOS = IPOS + ISTORE
          TZSTATION%XLWU    = ZPACK(IPOS:IPOS+ISTORE-1); IPOS = IPOS + ISTORE
          TZSTATION%XSWDIR  = ZPACK(IPOS:IPOS+ISTORE-1); IPOS = IPOS + ISTORE
          TZSTATION%XSWDIFF = ZPACK(IPOS:IPOS+ISTORE-1); IPOS = IPOS + ISTORE
          TZSTATION%XDSTAOD = ZPACK(IPOS:IPOS+ISTORE-1); IPOS = IPOS + ISTORE
        END IF
        TZSTATION%XSFCO2 =    ZPACK(IPOS:IPOS+ISTORE-1); IPOS = IPOS + ISTORE
      END IF

      IF ( IPOS /= IPACKSIZE ) &
        call Print_msg( NVERB_WARNING, 'IO', 'WRITE_STATION_n', 'IPOS /= IPACKSIZE (receiver side)', OLOCAL = .TRUE. )
    END IF
  END IF

  CALL STATION_DIACHRO_n( TPDIAFILE, TZSTATION )

END DO STATION

END SUBROUTINE WRITE_STATION_n

! ##################################################
SUBROUTINE STATION_DIACHRO_n( TPDIAFILE, TPSTATION )
! ##################################################

USE MODD_ALLSTATION_n,  ONLY: LDIAG_SURFRAD
use modd_budget,        only: NLVL_CATEGORY, NLVL_SUBCATEGORY, NLVL_GROUP, NLVL_SHAPE, NLVL_TIMEAVG, NLVL_NORM, NLVL_MASK, &
                              tbudiachrometadata
USE MODD_CONF,          ONLY: LCARTESIAN
USE MODD_CST,           ONLY: XRV
use modd_field,         only: NMNHDIM_STATION_TIME, NMNHDIM_STATION_PROC, NMNHDIM_UNUSED, &
                              tfieldmetadata_base, TYPEREAL
USE MODD_IO,            ONLY: TFILEDATA
USE MODD_NSV,           ONLY: nsv, nsv_aer, nsv_aerbeg, nsv_aerend, &
                              nsv_dst, nsv_dstbeg, nsv_dstend, nsv_slt, nsv_sltbeg, nsv_sltend, &
                              tsvlist
USE MODD_PARAM_n,       ONLY: CRAD, CSURF, CTURB
use modd_station_n,     only: tstations_time
use modd_type_statprof, only: tstationdata

USE MODE_AERO_PSD
USE MODE_DUST_PSD
USE MODE_SALT_PSD
use MODE_WRITE_DIACHRO, ONLY: Write_diachro

TYPE(TFILEDATA),    INTENT(IN) :: TPDIAFILE ! diachronic file to write
TYPE(TSTATIONDATA), INTENT(IN) :: TPSTATION
!
!*      0.2  declaration of local variables for diachro
!
REAL, DIMENSION(:,:,:,:,:,:), ALLOCATABLE :: XWORK6 ! contains temporal series
REAL, DIMENSION(:,:,:,:),     ALLOCATABLE :: ZSV, ZN0, ZSIG, ZRG
REAL, DIMENSION(:,:,:,:,:),   ALLOCATABLE :: ZPTOTA
REAL, DIMENSION(:,:,:),       ALLOCATABLE :: ZRHO
!
CHARACTER(LEN=NCOMMENTLGTMAX)     :: YCOMMENT ! comment string
CHARACTER(LEN=NMNHNAMELGTMAX)     :: YTITLE   ! title
!
!!! do not forget to increment the IPROC value if you add diagnostic !!!
INTEGER :: IPROC    ! number of variables records
!!! do not forget to increment the JPROC value if you add diagnostic !!!
INTEGER :: ISTORE
INTEGER :: JPROC    ! loop counter
INTEGER :: JRR      ! loop counter
INTEGER :: JSV      ! loop counter
type(tbudiachrometadata)                             :: tzbudiachro
type(tfieldmetadata_base), dimension(:), allocatable :: tzfields
!
!----------------------------------------------------------------------------
!
IPROC = 8 + SIZE(TPSTATION%XR,2) + SIZE(TPSTATION%XSV,2)

IF ( CTURB == 'TKEL' ) IPROC = IPROC + 1
IF (LDIAG_SURFRAD) THEN
  IF(CSURF=="EXTE") IPROC = IPROC + 10
  IF(CRAD/="NONE")  IPROC = IPROC + 7
  IPROC = IPROC + 1 ! XSFCO2 term
END IF
IF (LORILAM) IPROC = IPROC + JPMODE*(3+NSOA+NCARB+NSP)
IF (LDUST) IPROC = IPROC + NMODE_DST*3
IF (LSALT) IPROC = IPROC + NMODE_SLT*3
IF ( CRAD /= 'NONE' )  IPROC = IPROC + 1
!
ISTORE = SIZE( TSTATIONS_TIME%TPDATES )

ALLOCATE( XWORK6(1, 1, 1, ISTORE, 1, IPROC) )
ALLOCATE( CCOMMENT(IPROC) )
ALLOCATE( CTITLE  (IPROC) )
ALLOCATE( CUNIT   (IPROC) )
!
JPROC = 0
!
!----------------------------------------------------------------------------
!
call Add_point( 'ZS', 'Orography', 'm',  SPREAD( tpstation%xzs, 1, istore ) )
call Add_point( 'P',  'Pressure',  'Pa', tpstation%xp(:) )
! call Add_point( 'Z', 'Z Pos', 'm', SPREAD( tpstation%xz, 1, istore ) )

if ( lcartesian ) then
  call Add_point( 'X', 'X Pos', 'm', SPREAD( tpstation%xx, 1, istore ) )
  call Add_point( 'Y', 'Y Pos', 'm', SPREAD( tpstation%xy, 1, istore ) )
  call Add_point( 'U', 'Axial velocity',       'm s-1', tpstation%xzon(:) )
  call Add_point( 'V', 'Transversal velocity', 'm s-1', tpstation%xmer(:) )
else
  call Add_point( 'LON', 'Longitude', 'degree', SPREAD( tpstation%xlon, 1, istore ) )
  call Add_point( 'LAT', 'Latitude',  'degree', SPREAD( tpstation%xlat, 1, istore ) )
  call Add_point( 'ZON_WIND', 'Zonal wind',      'm s-1', tpstation%xzon(:) )
  call Add_point( 'MER_WIND', 'Meridional wind', 'm s-1', tpstation%xmer(:) )
end if

call Add_point( 'W',  'Air vertical speed',    'm s-1', tpstation%xw(:)  )
call Add_point( 'Th', 'Potential temperature', 'K',     tpstation%xth(:) )

if ( ldiag_surfrad ) then
  if ( csurf == "EXTE" ) then
    call Add_point( 'T2m',    '2-m temperature',        'K',       tpstation%xt2m(:)    )
    call Add_point( 'Q2m',    '2-m humidity',           'kg kg-1', tpstation%xq2m(:)    )
    call Add_point( 'HU2m',   '2-m relative humidity',  'percent', tpstation%xhu2m(:)   )
    call Add_point( 'zon10m', '10-m zonal wind',        'm s-1',   tpstation%xzon10m(:) )
    call Add_point( 'mer10m', '10-m meridian wind',     'm s-1',   tpstation%xmer10m(:) )
    call Add_point( 'RN',     'Net radiation',          'W m-2',   tpstation%xrn(:)     )
    call Add_point( 'H',      'Sensible heat flux',     'W m-2',   tpstation%xh(:)      )
    call Add_point( 'LE',     'Total Latent heat flux', 'W m-2',   tpstation%xle(:)     )
    call Add_point( 'G',      'Storage heat flux',      'W m-2',   tpstation%xgflux(:)  )
    call Add_point( 'LEI',    'Solid Latent heat flux', 'W m-2',   tpstation%xlei(:)    )
  end if
  if ( crad /= 'NONE' ) then
    call Add_point( 'SWD',    'Downward short-wave radiation',         'W m-2', tpstation%xswd(:)    )
    call Add_point( 'SWU',    'Upward short-wave radiation',           'W m-2', tpstation%xswu(:)    )
    call Add_point( 'LWD',    'Downward long-wave radiation',          'W m-2', tpstation%xlwd(:)    )
    call Add_point( 'LWU',    'Upward long-wave radiation',            'W m-2', tpstation%xlwu(:)    )
    call Add_point( 'SWDIR',  'Downward direct short-wave radiation',  'W m-2', tpstation%xswdir(:)  )
    call Add_point( 'SWDIFF', 'Downward diffuse short-wave radiation', 'W m-2', tpstation%xswdiff(:) )
    call Add_point( 'DSTAOD', 'Dust aerosol optical depth',            'm',     tpstation%xdstaod(:) )
  end if
end if

do jrr = 1, SIZE( tpstation%xr, 2 )
  select case( jrr )
    case (1)
      call Add_point( 'Rv', 'Water vapor mixing ratio',        'kg kg-1', tpstation%xr(:,jrr) )
    case (2)
      call Add_point( 'Rc', 'Liquid cloud water mixing ratio', 'kg kg-1', tpstation%xr(:,jrr) )
    case (3)
      call Add_point( 'Rr', 'Rain water mixing ratio',         'kg kg-1', tpstation%xr(:,jrr) )
    case (4)
      call Add_point( 'Ri', 'Ice cloud water mixing ratio',    'kg kg-1', tpstation%xr(:,jrr) )
    case (5)
      call Add_point( 'Rs', 'Snow mixing ratio',               'kg kg-1', tpstation%xr(:,jrr) )
    case (6)
      call Add_point( 'Rg', 'Graupel mixing ratio',            'kg kg-1', tpstation%xr(:,jrr) )
    case (7)
      call Add_point( 'Rh', 'Hail mixing ratio',               'kg kg-1', tpstation%xr(:,jrr) )
  end select
end do

if ( cturb == 'TKEL' ) call Add_point( 'Tke', 'Turbulent kinetic energy', 'm2 s-2', tpstation%xtke(:) )

if ( nsv > 0 ) then
  ! Scalar variables
  DO JSV = 1, NSV
    IF ( TRIM( TSVLIST(JSV)%CUNITS ) == 'ppv' ) THEN
      !*1e9 for conversion ppv->ppb
      call Add_point( TRIM( TSVLIST(JSV)%CMNHNAME ), '', 'ppb', TPSTATION%XSV(:,JSV) * 1.e9 )
    ELSE
      call Add_point( TRIM( TSVLIST(JSV)%CMNHNAME ), '', TSVLIST(JSV)%CUNITS, TPSTATION%XSV(:,JSV) )
    END IF
  END DO

  IF ((LORILAM).AND. .NOT.(ANY(TPSTATION%XP(:) == 0.))) THEN
    ALLOCATE (ZSV(1,1,ISTORE,NSV_AER))
    ALLOCATE (ZRHO(1,1,ISTORE))
    ALLOCATE (ZN0(1,1,ISTORE,JPMODE))
    ALLOCATE (ZRG(1,1,ISTORE,JPMODE))
    ALLOCATE (ZSIG(1,1,ISTORE,JPMODE))
    ALLOCATE (ZPTOTA(1,1,ISTORE,NSP+NCARB+NSOA,JPMODE))
    ZSV(1,1,:,1:NSV_AER) = TPSTATION%XSV(:,NSV_AERBEG:NSV_AEREND)
    IF (SIZE(TPSTATION%XR,2) >0) THEN
      ZRHO(1,1,:) = 0.
      DO JRR=1,SIZE(TPSTATION%XR,2)
        ZRHO(1,1,:) = ZRHO(1,1,:) + TPSTATION%XR(:,JRR)
      ENDDO
      ZRHO(1,1,:) = TPSTATION%XTH(:) * ( 1. + XRV/XRD*TPSTATION%XR(:,1) )  &
                                      / ( 1. + ZRHO(1,1,:)                )
    ELSE
      ZRHO(1,1,:) = TPSTATION%XTH(:)
    ENDIF
    ZRHO(1,1,:) =  TPSTATION%XP(:) / &
                  (XRD *ZRHO(1,1,:) *((TPSTATION%XP(:)/XP00)**(XRD/XCPD)) )

    CALL PPP2AERO(ZSV,ZRHO, PSIG3D=ZSIG, PRG3D=ZRG, PN3D=ZN0,PCTOTA=ZPTOTA)

    DO JSV=1,JPMODE
      ! mean radius
      WRITE(YTITLE,'(A6,I1)')'AERRGA',JSV
      WRITE(YCOMMENT,'(A18,I1)')'RG (nb) AERO MODE ',JSV
      call Add_point( ytitle, ycomment, 'um', ZRG(1,1,:,JSV) )

      ! standard deviation
      WRITE(YTITLE,'(A7,I1)')'AERSIGA',JSV
      WRITE(YCOMMENT,'(A16,I1)')'SIGMA AERO MODE ',JSV
      call Add_point( ytitle, ycomment, '',ZSIG(1,1,:,JSV) )

      ! particles number
      WRITE(YTITLE,'(A6,I1)')'AERN0A',JSV
      WRITE(YCOMMENT,'(A13,I1)')'N0 AERO MODE ',JSV
      call Add_point( ytitle, ycomment, 'm-3', ZN0(1,1,:,JSV) )

      WRITE(YTITLE,'(A5,I1)')'MOC  ',JSV
      WRITE(CCOMMENT,'(A23,I1)')'MASS OC   AEROSOL MODE ',JSV
      call Add_point( ytitle, ycomment, 'ug m-3', ZPTOTA(1,1,:,JP_AER_OC,JSV) )

      WRITE(YTITLE,'(A5,I1)')'MBC  ',JSV
      WRITE(CCOMMENT,'(A23,I1)')'MASS BC   AEROSOL MODE ',JSV
      call Add_point( ytitle, ycomment, 'ug m-3', ZPTOTA(1,1,:,JP_AER_BC,JSV) )

      WRITE(YTITLE,'(A5,I1)')'MDST  ',JSV
      WRITE(CCOMMENT,'(A23,I1)')'MASS DST   AEROSOL MODE ',JSV
      call Add_point( ytitle, ycomment, 'ug m-3', ZPTOTA(1,1,:,JP_AER_DST,JSV) )

      WRITE(YTITLE,'(A5,I1)')'MSO4 ',JSV
      WRITE(CCOMMENT,'(A23,I1)')'MASS SO4  AEROSOL MODE ',JSV
      call Add_point( ytitle, ycomment, 'ug m-3', ZPTOTA(1,1,:,JP_AER_SO4,JSV) )

      WRITE(YTITLE,'(A5,I1)')'MNO3 ',JSV
      WRITE(CCOMMENT,'(A23,I1)')'MASS NO3  AEROSOL MODE ',JSV
      call Add_point( ytitle, ycomment, 'ug m-3', ZPTOTA(1,1,:,JP_AER_NO3,JSV) )

      WRITE(YTITLE,'(A5,I1)')'MH2O ',JSV
      WRITE(CCOMMENT,'(A23,I1)')'MASS H2O  AEROSOL MODE ',JSV
      call Add_point( ytitle, ycomment, 'ug m-3', ZPTOTA(1,1,:,JP_AER_H2O,JSV) )

      WRITE(YTITLE,'(A5,I1)')'MNH3 ',JSV
      WRITE(CCOMMENT,'(A23,I1)')'MASS NH3  AEROSOL MODE ',JSV
      call Add_point( ytitle, ycomment, 'ug m-3', ZPTOTA(1,1,:,JP_AER_NH3,JSV) )

      IF ( NSOA == 10 ) THEN
        WRITE(YTITLE,'(A5,I1)')'MSOA1',JSV
        WRITE(CCOMMENT,'(A23,I1)')'MASS SOA1 AEROSOL MODE ',JSV
        call Add_point( ytitle, ycomment, 'ug m-3', ZPTOTA(1,1,:,JP_AER_SOA1,JSV) )

        WRITE(YTITLE,'(A5,I1)')'MSOA2',JSV
        WRITE(CCOMMENT,'(A23,I1)')'MASS SOA2 AEROSOL MODE ',JSV
        call Add_point( ytitle, ycomment, 'ug m-3', ZPTOTA(1,1,:,JP_AER_SOA2,JSV) )

        WRITE(YTITLE,'(A5,I1)')'MSOA3',JSV
        WRITE(CCOMMENT,'(A23,I1)')'MASS SOA3 AEROSOL MODE ',JSV
        call Add_point( ytitle, ycomment, 'ug m-3', ZPTOTA(1,1,:,JP_AER_SOA3,JSV) )

        WRITE(YTITLE,'(A5,I1)')'MSOA4',JSV
        WRITE(CCOMMENT,'(A23,I1)')'MASS SOA4 AEROSOL MODE ',JSV
        call Add_point( ytitle, ycomment, 'ug m-3', ZPTOTA(1,1,:,JP_AER_SOA4,JSV) )

        WRITE(YTITLE,'(A5,I1)')'MSOA5',JSV
        WRITE(CCOMMENT,'(A23,I1)')'MASS SOA5 AEROSOL MODE ',JSV
        call Add_point( ytitle, ycomment, 'ug m-3', ZPTOTA(1,1,:,JP_AER_SOA5,JSV) )

        WRITE(YTITLE,'(A5,I1)')'MSOA6',JSV
        WRITE(CCOMMENT,'(A23,I1)')'MASS SOA6 AEROSOL MODE ',JSV
        call Add_point( ytitle, ycomment, 'ug m-3', ZPTOTA(1,1,:,JP_AER_SOA6,JSV) )

        WRITE(YTITLE,'(A5,I1)')'MSOA7',JSV
        WRITE(CCOMMENT,'(A23,I1)')'MASS SOA7 AEROSOL MODE ',JSV
        call Add_point( ytitle, ycomment, 'ug m-3', ZPTOTA(1,1,:,JP_AER_SOA7,JSV) )

        WRITE(YTITLE,'(A5,I1)')'MSOA8',JSV
        WRITE(CCOMMENT,'(A23,I1)')'MASS SOA8 AEROSOL MODE ',JSV
        call Add_point( ytitle, ycomment, 'ug m-3', ZPTOTA(1,1,:,JP_AER_SOA8,JSV) )

        WRITE(YTITLE,'(A5,I1)')'MSOA9',JSV
        WRITE(CCOMMENT,'(A23,I1)')'MASS SOA9 AEROSOL MODE ',JSV
        call Add_point( ytitle, ycomment, 'ug m-3', ZPTOTA(1,1,:,JP_AER_SOA9,JSV) )

        WRITE(YTITLE,'(A6,I1)')'MSOA10',JSV
        WRITE(CCOMMENT,'(A24,I1)')'MASS SOA10 AEROSOL MODE ',JSV
        call Add_point( ytitle, ycomment, 'ug m-3', ZPTOTA(1,1,:,JP_AER_SOA10,JSV) )
      END IF
    END DO

    DEALLOCATE (ZSV,ZRHO)
    DEALLOCATE (ZN0,ZRG,ZSIG)
  END IF

  IF ((LDUST).AND. .NOT.(ANY(TPSTATION%XP(:) == 0.))) THEN
    ALLOCATE (ZSV(1,1,ISTORE,NSV_DST))
    ALLOCATE (ZRHO(1,1,ISTORE))
    ALLOCATE (ZN0(1,1,ISTORE,NMODE_DST))
    ALLOCATE (ZRG(1,1,ISTORE,NMODE_DST))
    ALLOCATE (ZSIG(1,1,ISTORE,NMODE_DST))
    ZSV(1,1,:,1:NSV_DST) = TPSTATION%XSV(:,NSV_DSTBEG:NSV_DSTEND)
    IF (SIZE(TPSTATION%XR,2) >0) THEN
      ZRHO(1,1,:) = 0.
      DO JRR=1,SIZE(TPSTATION%XR,2)
        ZRHO(1,1,:) = ZRHO(1,1,:) + TPSTATION%XR(:,JRR)
      ENDDO
      ZRHO(1,1,:) = TPSTATION%XTH(:) * ( 1. + XRV/XRD*TPSTATION%XR(:,1) )  &
                                      / ( 1. + ZRHO(1,1,:)                )
    ELSE
      ZRHO(1,1,:) = TPSTATION%XTH(:)
    ENDIF
    ZRHO(1,1,:) =  TPSTATION%XP(:) / &
                  (XRD *ZRHO(1,1,:) *((TPSTATION%XP(:)/XP00)**(XRD/XCPD)) )
    CALL PPP2DUST(ZSV,ZRHO, PSIG3D=ZSIG, PRG3D=ZRG, PN3D=ZN0)
    DO JSV=1,NMODE_DST
      ! mean radius
      JPROC = JPROC+1
      WRITE(CTITLE(JPROC),'(A6,I1)')'DSTRGA',JSV
      CUNIT    (JPROC) = 'um'
      WRITE(CCOMMENT(JPROC),'(A18,I1)')'RG (nb) DUST MODE ',JSV
      XWORK6 (1,1,1,:,1,JPROC) = ZRG(1,1,:,JSV)
      ! standard deviation
      JPROC = JPROC+1
      WRITE(CTITLE(JPROC),'(A7,I1)')'DSTSIGA',JSV
      CUNIT    (JPROC) = '  '
      WRITE(CCOMMENT(JPROC),'(A16,I1)')'SIGMA DUST MODE ',JSV
      XWORK6 (1,1,1,:,1,JPROC) = ZSIG(1,1,:,JSV)
      ! particles number
      JPROC = JPROC+1
      WRITE(CTITLE(JPROC),'(A6,I1)')'DSTN0A',JSV
      CUNIT    (JPROC) = 'm-3'
      WRITE(CCOMMENT(JPROC),'(A13,I1)')'N0 DUST MODE ',JSV
      XWORK6 (1,1,1,:,1,JPROC) = ZN0(1,1,:,JSV)
    ENDDO
    DEALLOCATE (ZSV,ZRHO)
    DEALLOCATE (ZN0,ZRG,ZSIG)
  END IF

  IF ((LSALT).AND. .NOT.(ANY(TPSTATION%XP(:) == 0.))) THEN
    ALLOCATE (ZSV(1,1,ISTORE,NSV_SLT))
    ALLOCATE (ZRHO(1,1,ISTORE))
    ALLOCATE (ZN0(1,1,ISTORE,NMODE_SLT))
    ALLOCATE (ZRG(1,1,ISTORE,NMODE_SLT))
    ALLOCATE (ZSIG(1,1,ISTORE,NMODE_SLT))
    ZSV(1,1,:,1:NSV_SLT) = TPSTATION%XSV(:,NSV_SLTBEG:NSV_SLTEND)
    IF (SIZE(TPSTATION%XR,2) >0) THEN
      ZRHO(1,1,:) = 0.
      DO JRR=1,SIZE(TPSTATION%XR,2)
        ZRHO(1,1,:) = ZRHO(1,1,:) + TPSTATION%XR(:,JRR)
      ENDDO
      ZRHO(1,1,:) = TPSTATION%XTH(:) * ( 1. + XRV/XRD*TPSTATION%XR(:,1) )  &
                                      / ( 1. + ZRHO(1,1,:)                )
    ELSE
      ZRHO(1,1,:) = TPSTATION%XTH(:)
    ENDIF
    ZRHO(1,1,:) =  TPSTATION%XP(:) / &
                  (XRD *ZRHO(1,1,:) *((TPSTATION%XP(:)/XP00)**(XRD/XCPD)) )
    CALL PPP2SALT(ZSV,ZRHO, PSIG3D=ZSIG, PRG3D=ZRG, PN3D=ZN0)
    DO JSV=1,NMODE_SLT
      ! mean radius
      WRITE(CTITLE(JPROC),'(A6,I1)')'SLTRGA',JSV
      WRITE(CCOMMENT(JPROC),'(A18,I1)')'RG (nb) SALT MODE ',JSV
      call Add_point( ytitle, ycomment, 'um', ZRG(1,1,:,JSV) )

      ! standard deviation
      WRITE(CTITLE(JPROC),'(A7,I1)')'SLTSIGA',JSV
      WRITE(CCOMMENT(JPROC),'(A16,I1)')'SIGMA DUST MODE ',JSV
      call Add_point( ytitle, ycomment, '',ZSIG(1,1,:,JSV) )

      ! particles number
      WRITE(CTITLE(JPROC),'(A6,I1)')'SLTN0A',JSV
      WRITE(CCOMMENT(JPROC),'(A13,I1)')'N0 DUST MODE ',JSV
      call Add_point( ytitle, ycomment, 'm-3', ZN0(1,1,:,JSV) )
    ENDDO
    DEALLOCATE (ZSV,ZRHO)
    DEALLOCATE (ZN0,ZRG,ZSIG)
  END IF
end if

if ( crad /= 'NONE' ) call Add_point( 'Tsrad', 'Radiative Surface Temperature', 'K', tpstation%xtsrad(:) )

if ( ldiag_surfrad ) call Add_point( 'SFCO2', 'CO2 Surface Flux', 'mg m-2 s-1', tpstation%xsfco2(:) )
!
!----------------------------------------------------------------------------
!
!
allocate( tzfields( jproc ) )

tzfields(:)%cmnhname  = ctitle(1 : jproc)
tzfields(:)%cstdname  = ''
tzfields(:)%clongname = ctitle(1 : jproc)
tzfields(:)%cunits    = cunit(1 : jproc)
tzfields(:)%ccomment  = ccomment(1 : jproc)
tzfields(:)%ngrid     = 0
tzfields(:)%ntype     = TYPEREAL
tzfields(:)%ndims     = 2
tzfields(:)%ndimlist(1) = NMNHDIM_UNUSED
tzfields(:)%ndimlist(2) = NMNHDIM_UNUSED
tzfields(:)%ndimlist(3) = NMNHDIM_UNUSED
tzfields(:)%ndimlist(4) = NMNHDIM_STATION_TIME
tzfields(:)%ndimlist(5) = NMNHDIM_UNUSED
tzfields(:)%ndimlist(6) = NMNHDIM_STATION_PROC

tzbudiachro%lleveluse(NLVL_CATEGORY)    = .true.
tzbudiachro%clevels  (NLVL_CATEGORY)    = 'Stations'
tzbudiachro%ccomments(NLVL_CATEGORY)    = 'Level for the different stations'

tzbudiachro%lleveluse(NLVL_SUBCATEGORY) = .false.
tzbudiachro%clevels  (NLVL_SUBCATEGORY) = ''
tzbudiachro%ccomments(NLVL_SUBCATEGORY) = ''

tzbudiachro%lleveluse(NLVL_GROUP)       = .true.
tzbudiachro%clevels  (NLVL_GROUP)       = tpstation%cname
tzbudiachro%ccomments(NLVL_GROUP)       = 'Values at position of station ' // Trim( tpstation%cname )

tzbudiachro%lleveluse(NLVL_SHAPE)       = .false.
tzbudiachro%clevels  (NLVL_SHAPE)       = 'Point'
tzbudiachro%ccomments(NLVL_SHAPE)       = 'Values at position of station ' // Trim( tpstation%cname )

tzbudiachro%lleveluse(NLVL_TIMEAVG)     = .false.
tzbudiachro%clevels  (NLVL_TIMEAVG)     = 'Not_time_averaged'
tzbudiachro%ccomments(NLVL_TIMEAVG)     = 'Values are not time averaged'

tzbudiachro%lleveluse(NLVL_NORM)        = .false.
tzbudiachro%clevels  (NLVL_NORM)        = 'Not_normalized'
tzbudiachro%ccomments(NLVL_NORM)        = 'Values are not normalized'

tzbudiachro%lleveluse(NLVL_MASK)        = .false.
tzbudiachro%clevels  (NLVL_MASK)        = ''
tzbudiachro%ccomments(NLVL_MASK)        = ''

tzbudiachro%lmobile    = .false.
!Compression does not make sense here
!Keep these values for backward compatibility of LFI files
tzbudiachro%licompress = .true.
tzbudiachro%ljcompress = .true.
tzbudiachro%lkcompress = .false.
tzbudiachro%ltcompress = .false.
tzbudiachro%lnorm      = .false.
!Boundaries in physical domain does not make sense here
!These values are not written in the netCDF files
!These values are written in the LFI files. Kept for backward compatibility of LFI files
tzbudiachro%nil        = 1
tzbudiachro%nih        = 1
tzbudiachro%njl        = 1
tzbudiachro%njh        = 1
tzbudiachro%nkl        = 1
tzbudiachro%nkh        = 1

call Write_diachro( tpdiafile, tzbudiachro, tzfields, tstations_time%tpdates, xwork6(:,:,:,:,:,:jproc) )

deallocate( tzfields )

!Necessary because global variables (private inside module)
Deallocate( xwork6  )
Deallocate (ccomment)
Deallocate (ctitle  )
Deallocate (cunit   )

!----------------------------------------------------------------------------

contains

! ######################################################
subroutine Add_point( htitle, hcomment, hunits, pfield )
! ######################################################

use mode_msg

character(len=*),   intent(in) :: htitle
character(len=*),   intent(in) :: hcomment
character(len=*),   intent(in) :: hunits
real, dimension(:), intent(in) :: pfield

integer :: jk

jproc = jproc + 1

if ( jproc > iproc ) call Print_msg( NVERB_FATAL, 'IO', 'Add_point', 'more processes than expected' )

ctitle(jproc)   = Trim( htitle)
ccomment(jproc) = Trim( hcomment )
cunit(jproc)    = Trim( hunits )

xwork6(1, 1, 1, :, 1, jproc) = pfield(:)

end subroutine Add_point

END SUBROUTINE STATION_DIACHRO_n

END MODULE MODE_WRITE_STATION_n
