!MNH_LIC Copyright 2002-2023 CNRS, Meteo-France and Universite Paul Sabatier
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

implicit none

private

public :: STATION_DIACHRO_n

contains

! ##################################################
SUBROUTINE STATION_DIACHRO_n( TPDIAFILE, TPSTATION )
! ##################################################

USE MODD_ALLSTATION_n,  ONLY: LDIAG_SURFRAD_STAT
use modd_budget,        only: NLVL_CATEGORY, NLVL_SUBCATEGORY, NLVL_GROUP, NLVL_SHAPE, NLVL_TIMEAVG, NLVL_NORM, NLVL_MASK, &
                              tbudiachrometadata
USE MODD_CH_AEROSOL,    ONLY: JPMODE, LORILAM, NCARB, NSOA, NSP
USE MODD_CONF,          ONLY: LCARTESIAN
USE MODD_DUST,          ONLY: LDUST, NMODE_DST
use modd_field,         only: NMNHDIM_STATION_TIME, NMNHDIM_STATION_PROC, NMNHDIM_UNUSED, &
                              tfieldmetadata_base, TYPEREAL
USE MODD_IO,            ONLY: TFILEDATA
USE MODD_NSV,           ONLY: nsv, tsvlist
USE MODD_PARAM_n,       ONLY: CRAD, CSURF, CTURB
USE MODD_SALT,          ONLY: LSALT, NMODE_SLT
use modd_station_n,     only: tstations_time
use modd_type_statprof, only: tstationdata

use mode_sensor,         only: Add_dust_data, Add_fixpoint, Add_orilam_data, Add_point, Add_salt_data, &
                               Sensor_current_processes_number_get, &
                               ccomment, ctitle, cunit, xwork6, &
                               Sensor_write_workarrays_allocate, Sensor_write_workarrays_deallocate
use mode_statprof_tools, only: Add_diag_surfrad_data
use MODE_WRITE_DIACHRO,  ONLY: Write_diachro

TYPE(TFILEDATA),    INTENT(IN) :: TPDIAFILE ! diachronic file to write
TYPE(TSTATIONDATA), INTENT(IN) :: TPSTATION
!
!*      0.2  declaration of local variables for diachro
!
!!! do not forget to increment the IPROC value if you add diagnostic !!!
INTEGER :: IPROC    ! number of variables records
!!! do not forget to increment the JPROC value if you add diagnostic !!!
INTEGER :: ISTORE
INTEGER :: JPROC    ! loop counter
INTEGER :: JRR      ! loop counter
INTEGER :: JSV      ! loop counter
real    :: zalt_meas ! True altitude for the station measurements
type(tbudiachrometadata)                             :: tzbudiachro
type(tfieldmetadata_base), dimension(:), allocatable :: tzfields
!
!----------------------------------------------------------------------------
!
IPROC = 5 + SIZE(TPSTATION%XR,3) + SIZE(TPSTATION%XSV,3)

IF ( CTURB == 'TKEL' ) IPROC = IPROC + 1
IF (LDIAG_SURFRAD_STAT) THEN
  IF(CSURF=="EXTE") IPROC = IPROC + 10
  IF(CRAD/="NONE")  IPROC = IPROC + 8
  IPROC = IPROC + 1 ! XSFCO2 term
END IF
IF ( LORILAM ) IPROC = IPROC + JPMODE * ( 3 + NSOA + NCARB + NSP )
IF ( LDUST )   IPROC = IPROC + NMODE_DST * 3
IF ( LSALT )   IPROC = IPROC + NMODE_SLT * 3
IF ( CRAD /= 'NONE' )  IPROC = IPROC + 1

ISTORE = SIZE( TSTATIONS_TIME%TPDATES )

call Sensor_write_workarrays_allocate( 1, istore, iproc )
!
!----------------------------------------------------------------------------
!
call Add_point( 'P',  'Pressure',  'Pa', tpstation%xp(1,:) )

if ( lcartesian ) then
  call Add_point( 'U', 'Axial velocity',       'm s-1', tpstation%xzon(1,:) )
  call Add_point( 'V', 'Transversal velocity', 'm s-1', tpstation%xmer(1,:) )
else
  call Add_point( 'ZON_WIND', 'Zonal wind',      'm s-1', tpstation%xzon(1,:) )
  call Add_point( 'MER_WIND', 'Meridional wind', 'm s-1', tpstation%xmer(1,:) )
end if

call Add_point( 'W',  'Air vertical speed',    'm s-1', tpstation%xw(1,:)  )
call Add_point( 'Th', 'Potential temperature', 'K',     tpstation%xth(1,:) )

if ( ldiag_surfrad_stat ) call Add_diag_surfrad_data( tpstation )

do jrr = 1, SIZE( tpstation%xr, 3 )
  select case( jrr )
    case (1)
      call Add_point( 'Rv', 'Water vapor mixing ratio',        'kg kg-1', tpstation%xr(1,:,jrr) )
    case (2)
      call Add_point( 'Rc', 'Liquid cloud water mixing ratio', 'kg kg-1', tpstation%xr(1,:,jrr) )
    case (3)
      call Add_point( 'Rr', 'Rain water mixing ratio',         'kg kg-1', tpstation%xr(1,:,jrr) )
    case (4)
      call Add_point( 'Ri', 'Ice cloud water mixing ratio',    'kg kg-1', tpstation%xr(1,:,jrr) )
    case (5)
      call Add_point( 'Rs', 'Snow mixing ratio',               'kg kg-1', tpstation%xr(1,:,jrr) )
    case (6)
      call Add_point( 'Rg', 'Graupel mixing ratio',            'kg kg-1', tpstation%xr(1,:,jrr) )
    case (7)
      call Add_point( 'Rh', 'Hail mixing ratio',               'kg kg-1', tpstation%xr(1,:,jrr) )
  end select
end do

if ( cturb == 'TKEL' ) call Add_point( 'Tke', 'Turbulent kinetic energy', 'm2 s-2', tpstation%xtke(1,:) )

if ( nsv > 0 ) then
  ! Scalar variables
  DO JSV = 1, NSV
    IF ( TRIM( TSVLIST(JSV)%CUNITS ) == 'ppv' ) THEN
      !*1e9 for conversion ppv->ppb
      call Add_point( TRIM( TSVLIST(JSV)%CMNHNAME ), '', 'ppb', TPSTATION%XSV(1,:,JSV) * 1.e9 )
    ELSE
      call Add_point( TRIM( TSVLIST(JSV)%CMNHNAME ), '', TSVLIST(JSV)%CUNITS, TPSTATION%XSV(1,:,JSV) )
    END IF
  END DO

  if ( lorilam ) call Add_orilam_data( tpstation, 1, istore )
  if ( ldust   ) call Add_dust_data  ( tpstation, 1, istore )
  if ( lsalt   ) call Add_salt_data  ( tpstation, 1, istore )
end if

if ( crad /= 'NONE' ) call Add_point( 'Tsrad', 'Radiative Surface Temperature', 'K', tpstation%xtsrad(:) )
!
!----------------------------------------------------------------------------
!
!
jproc = Sensor_current_processes_number_get()

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
call Sensor_write_workarrays_deallocate( )

!----------------------------------------------------------------------------
!Treat position and fix values (not changing during simulation)

IPROC = 6

call Sensor_write_workarrays_allocate( 1, 1, iproc )

if ( lcartesian ) then
  call Add_fixpoint( 'X', 'X position', 'm', TPSTATION%XX_CUR )
  call Add_fixpoint( 'Y', 'Y position', 'm', TPSTATION%XY_CUR )
else
  call Add_fixpoint( 'LON', 'longitude', 'degree', TPSTATION%XLON_CUR )
  call Add_fixpoint( 'LAT', 'latitude',  'degree', TPSTATION%XLAT_CUR )
end if

call Add_fixpoint( 'Z',     'altitude',  'm', TPSTATION%XZ_CUR )
call Add_fixpoint( 'ZS',    'orography', 'm', TPSTATION%XZS )
call Add_fixpoint( 'Zmeas', 'interpolated altitude used for measurements', 'm', TPSTATION%XZMEAS )
call Add_fixpoint( 'K',     'vertical model level used for computations',  '1', REAL( TPSTATION%NK ) )

jproc = Sensor_current_processes_number_get()

Allocate( tzfields( jproc ) )

tzfields(:)%cmnhname  = ctitle(1 : jproc)
tzfields(:)%cstdname  = ''
tzfields(:)%clongname = ctitle(1 : jproc)
tzfields(:)%cunits    = cunit(1 : jproc)
tzfields(:)%ccomment  = ccomment(1 : jproc)
tzfields(:)%ngrid     = 0
tzfields(:)%ntype     = TYPEREAL
tzfields(:)%ndims     = 1
tzfields(:)%ndimlist(1) = NMNHDIM_UNUSED
tzfields(:)%ndimlist(2) = NMNHDIM_UNUSED
tzfields(:)%ndimlist(3) = NMNHDIM_UNUSED
tzfields(:)%ndimlist(4) = NMNHDIM_UNUSED
tzfields(:)%ndimlist(5) = NMNHDIM_UNUSED
tzfields(:)%ndimlist(6) = NMNHDIM_STATION_PROC

call Write_diachro( tpdiafile, tzbudiachro, tzfields, tstations_time%tpdates, xwork6(:,:,:,:,:,:jproc) )

deallocate( tzfields )
call Sensor_write_workarrays_deallocate( )

END SUBROUTINE STATION_DIACHRO_n

END MODULE MODE_WRITE_STATION_n
