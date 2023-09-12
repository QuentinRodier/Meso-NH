!MNH_LIC Copyright 2002-2023 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
! Author:
!  P. Tulet    15/02/2002
!
!  Modifications
!  G. Delautier      2016: LIMA
!  C. Lac         10/2016: add visibility diagnostics for fog
!  P. Wautelet 05/2016-04/2018: new data structures and calls for I/O
!  P. Wautelet 13/09/2019: budget: simplify and modernize date/time management
!  P. Wautelet 09/10/2020: Write_diachro: use new datatype tpfields
!  P. Wautelet 03/03/2021: budgets: add tbudiachrometadata type (useful to pass more information to Write_diachro)
!  P. Wautelet 11/03/2021: bugfix: correct name for NSV_LIMA_IMM_NUCL
!  P. Wautelet 05/07/2021: reorganisation to store point values correctly (not in vertical profiles)
!  M. Taufour     07/2021: modify RARE for hydrometeors containing ice and add bright band calculation for RARE
!  P. Wautelet 01/09/2021: fix: correct vertical dimension for ALT and W
!  P. Wautelet 19/11/2021: bugfix in units for LIMA variables
!  P. Wautelet 04/02/2022: use TSVLIST to manage metadata of scalar variables
!  P. Wautelet    04/2022: restructure profilers for better performance, reduce memory usage and correct some problems/bugs
!-----------------------------------------------------------------
!      ###########################
MODULE MODE_WRITE_PROFILER_n
!      ###########################

use modd_parameters, only: NMNHNAMELGTMAX, NUNITLGTMAX

implicit none

private

public :: PROFILER_DIACHRO_n

contains

! ####################################################
SUBROUTINE PROFILER_DIACHRO_n( TPDIAFILE, TPPROFILER )
! ####################################################

USE MODD_ALLPROFILER_n,   ONLY: LDIAG_SURFRAD_PROF
use modd_budget,          only: NLVL_CATEGORY, NLVL_SUBCATEGORY, NLVL_GROUP, NLVL_SHAPE, NLVL_TIMEAVG, NLVL_NORM, NLVL_MASK, &
                                tbudiachrometadata
USE MODD_CH_AEROSOL,      ONLY: JPMODE, LORILAM, NCARB, NSOA, NSP
USE MODD_CONF,            ONLY: LCARTESIAN
USE MODD_CONF_n,          ONLY: NRR
USE MODD_CST,             ONLY: XRV
USE MODD_DUST,            ONLY: LDUST, NMODE_DST
USE MODD_DIM_n,           ONLY: NKMAX
use modd_field,           only: NMNHDIM_LEVEL, NMNHDIM_LEVEL_W, NMNHDIM_PROFILER_TIME, NMNHDIM_PROFILER_PROC, NMNHDIM_UNUSED, &
                                tfieldmetadata_base, TYPEREAL
USE MODD_IO,              ONLY: TFILEDATA
USE MODD_NSV,             ONLY: tsvlist, nsv
USE MODD_PARAMETERS,      ONLY: JPVEXT, XUNDEF
USE MODD_PARAM_n,         ONLY: CCLOUD, CRAD, CTURB
USE MODD_PROFILER_n
USE MODD_RADIATIONS_n,    ONLY: NAER
USE MODD_SALT,            ONLY: LSALT, NMODE_SLT
USE MODD_TYPE_STATPROF
!
use mode_sensor,          only: Add_dust_data, Add_fixpoint, Add_orilam_data, Add_point, Add_profile, Add_salt_data, &
                                Sensor_current_processes_number_get, &
                                ccomment, ctitle, cunit, xwork6, &
                                Sensor_write_workarrays_allocate, Sensor_write_workarrays_deallocate
use mode_statprof_tools,  only: Add_diag_surfrad_data
use mode_write_diachro,   only: Write_diachro
!
TYPE(TFILEDATA),     INTENT(IN) :: TPDIAFILE ! diachronic file to write
TYPE(TPROFILERDATA), INTENT(IN) :: TPPROFILER
!
!*      0.2  declaration of local variables for diachro
!
character(len=NMNHNAMELGTMAX)                        :: ytitle
character(len=NUNITLGTMAX)                           :: yunit
INTEGER                                              :: IKU
INTEGER                                              :: IPROC    ! number of variables records
INTEGER                                              :: JPROC
integer                                              :: jproc_alt, jproc_w
INTEGER                                              :: JRR      ! loop counter
INTEGER                                              :: JSV      ! loop counter
INTEGER                                              :: ISTORE
REAL, DIMENSION(:,:,:),                  ALLOCATABLE :: ZRHO
REAL, DIMENSION(:,:),                    ALLOCATABLE :: ZWORK
type(tbudiachrometadata)                             :: tzbudiachro
type(tfieldmetadata_base), dimension(:), allocatable :: tzfields
!
!----------------------------------------------------------------------------

IKU = NKMAX + 2 * JPVEXT !Number of vertical levels

IPROC = 13 + NRR + NSV
if ( ccloud == 'C2R2' .or. ccloud == 'KHKO' )  IPROC = IPROC + 1
if ( ccloud /= 'NONE' .and. ccloud /= 'REVE' ) IPROC = IPROC + 1
if ( ccloud == 'ICE3' .or. ccloud == 'ICE4' )  IPROC = IPROC + 1
if ( ccloud == 'LIMA' )  IPROC = IPROC + 3
IF ( LORILAM ) IPROC = IPROC + JPMODE * ( 3 + NSOA + NCARB + NSP )
IF ( LDUST )   IPROC = IPROC + NMODE_DST * 3
IF ( LSALT )   IPROC = IPROC + NMODE_SLT * 3
IF (LDUST .OR. LORILAM .OR. LSALT) IPROC=IPROC+NAER
IF ( CTURB == 'TKEL' ) IPROC = IPROC + 1

ISTORE = SIZE( TPROFILERS_TIME%TPDATES )

call Sensor_write_workarrays_allocate( iku, istore, iproc )
!
!----------------------------------------------------------------------------
!Treat vertical profiles

call Add_profile( 'Th',       'Potential temperature',         'K',      tpprofiler%xth        )
call Add_profile( 'Thv',      'Virtual Potential temperature', 'K',      tpprofiler%xthv       )
if ( ccloud == 'C2R2' .or. ccloud == 'KHKO' ) &
  call Add_profile( 'VISIGUL', 'Visibility Gultepe',           'km',     tpprofiler%xvisigul   )
if ( ccloud /= 'NONE' .and. ccloud /= 'REVE' ) &
  call Add_profile( 'VISIKUN', 'Visibility Kunkel',            'km',     tpprofiler%xvisikun   )
call Add_profile( 'RARE',     'Radar reflectivity',            'dBZ',    tpprofiler%xcrare     )
call Add_profile( 'RAREatt',  'Radar attenuated reflectivity', 'dBZ',    tpprofiler%xcrare_att )
call Add_profile( 'P',        'Pressure',                      'Pa',     tpprofiler%xp         )
call Add_profile( 'ALT',      'Altitude',                      'm',      tpprofiler%xzz        )
!Store position of ALT in the field list. Useful because it is not computed on the same Arakawa-grid points
jproc_alt = Sensor_current_processes_number_get()
call Add_profile( 'ZON_WIND', 'Zonal wind',                    'm s-1',  tpprofiler%xzon       )
call Add_profile( 'MER_WIND', 'Meridian wind',                 'm s-1',  tpprofiler%xmer       )
call Add_profile( 'FF',       'Wind intensity',                'm s-1',  tpprofiler%xff        )
call Add_profile( 'DD',       'Wind direction',                'degree', tpprofiler%xdd        )
call Add_profile( 'W',        'Air vertical speed',            'm s-1',  tpprofiler%xw         )
!Store position of W in the field list. Useful because it is not computed on the same Arakawa-grid points
jproc_w = Sensor_current_processes_number_get()

call Add_profile( 'TKE_DISS', 'TKE dissipation rate', 'm2 s-2', tpprofiler%xtke_diss )

if ( ccloud == 'LIMA' ) then
  call Add_profile( 'CCLOUDT', 'liquid cloud concentration', 'kg-1', tpprofiler%xccz(:,:) )
  call Add_profile( 'CRAINT',  'Rain concentration',         'kg-1', tpprofiler%xcrz(:,:) )
  call Add_profile( 'CICET',   'Ice concentration',          'kg-1', tpprofiler%xciz(:,:) )
else if ( ccloud == 'ICE3' .or. ccloud == 'ICE4' ) then
  call Add_profile( 'CIT',     'Ice concentration',           'm-3', tpprofiler%xciz(:,:) )
end if

if ( nrr >= 1 ) call Add_profile( 'Rv', 'Water vapor mixing ratio',        'kg kg-1', tpprofiler%xr(:,:,1) )
if ( nrr >= 2 ) call Add_profile( 'Rc', 'Liquid cloud water mixing ratio', 'kg kg-1', tpprofiler%xr(:,:,2) )
if ( nrr >= 3 ) call Add_profile( 'Rr', 'Rain water mixing ratio',         'kg kg-1', tpprofiler%xr(:,:,3) )
if ( nrr >= 4 ) call Add_profile( 'Ri', 'Ice cloud water mixing ratio',    'kg kg-1', tpprofiler%xr(:,:,4) )
if ( nrr >= 5 ) call Add_profile( 'Rs', 'Snow mixing ratio',               'kg kg-1', tpprofiler%xr(:,:,5) )
if ( nrr >= 6 ) call Add_profile( 'Rg', 'Graupel mixing ratio',            'kg kg-1', tpprofiler%xr(:,:,6) )
if ( nrr >= 7 ) call Add_profile( 'Rh', 'Hail mixing ratio',               'kg kg-1', tpprofiler%xr(:,:,7) )

call Add_profile( 'Rhod', 'Density of dry air', 'kg m-3', tpprofiler%xrhod )
if ( cturb == 'TKEL') &
  call Add_profile( 'Tke', 'Turbulent kinetic energy', 'm2 s-2', tpprofiler%xtke )

if ( nsv > 0  ) then
  ! Scalar variables
  Allocate( zwork, mold = tpprofiler%xsv(:,:,1) )
  do jsv = 1, nsv
    if ( Trim( tsvlist(jsv)%cunits ) == 'ppv' ) then
      yunit = 'ppb'
      zwork = tpprofiler%xsv(:,:,jsv) * 1.e9 !*1e9 for conversion ppv->ppb
    else
      yunit = Trim( tsvlist(jsv)%cunits )
      zwork = tpprofiler%xsv(:,:,jsv)
    end if
    call Add_profile( tsvlist(jsv)%cmnhname, '', yunit, zwork )
  end do
  Deallocate( zwork )

  if ( lorilam ) call Add_orilam_data( tpprofiler, iku, istore )
  if ( ldust   ) call Add_dust_data  ( tpprofiler, iku, istore )
  if ( lsalt   ) call Add_salt_data  ( tpprofiler, iku, istore )

  if ( ldust .or. lorilam .or. lsalt ) then
    do jsv = 1, naer
      Write( ytitle, '( a, i1 )' ) 'AEREXT', jsv
      call Add_profile( ytitle, 'Aerosol Extinction', '1', tpprofiler%xaer(:,:,jsv) )
    end do
  end if
end if

jproc = Sensor_current_processes_number_get()

allocate( tzfields( jproc ) )

tzfields(:)%cmnhname  = ctitle(1 : jproc)
tzfields(:)%cstdname  = ''
tzfields(:)%clongname = ctitle(1 : jproc)
tzfields(:)%cunits    = cunit(1 : jproc)
tzfields(:)%ccomment  = ccomment(1 : jproc)
tzfields(:)%ngrid     = 0
tzfields(:)%ntype     = TYPEREAL
tzfields(:)%ndims     = 3
tzfields(:)%ndimlist(1) = NMNHDIM_UNUSED
tzfields(:)%ndimlist(2) = NMNHDIM_UNUSED
tzfields(:)%ndimlist(3) = NMNHDIM_LEVEL
tzfields(jproc_alt)%ndimlist(3) = NMNHDIM_LEVEL_W
tzfields(jproc_w)%ndimlist(3)   = NMNHDIM_LEVEL_W
tzfields(:)%ndimlist(4) = NMNHDIM_PROFILER_TIME
tzfields(:)%ndimlist(5) = NMNHDIM_UNUSED
tzfields(:)%ndimlist(6) = NMNHDIM_PROFILER_PROC

tzbudiachro%lleveluse(NLVL_CATEGORY)    = .true.
tzbudiachro%clevels  (NLVL_CATEGORY)    = 'Profilers'
tzbudiachro%ccomments(NLVL_CATEGORY)    = 'Level for the different vertical profilers'

tzbudiachro%lleveluse(NLVL_SUBCATEGORY) = .false.
tzbudiachro%clevels  (NLVL_SUBCATEGORY) = ''
tzbudiachro%ccomments(NLVL_SUBCATEGORY) = ''

tzbudiachro%lleveluse(NLVL_GROUP)       = .true.
tzbudiachro%clevels  (NLVL_GROUP)       = tpprofiler%cname
tzbudiachro%ccomments(NLVL_GROUP)       = 'Data at position of profiler ' // Trim( tpprofiler%cname )

tzbudiachro%lleveluse(NLVL_SHAPE)       = .true.
tzbudiachro%clevels  (NLVL_SHAPE)       = 'Vertical_profile'
tzbudiachro%ccomments(NLVL_SHAPE)       = 'Vertical profiles at position of profiler ' // Trim( tpprofiler%cname )

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
!Horizontal boundaries in physical domain does not make sense here (but flyer position does)
!These values are not written in the netCDF files
!These values are written in the LFI files. They are kept for backward compatibility (and not set to default values)
tzbudiachro%nil        = 1
tzbudiachro%nih        = 1
tzbudiachro%njl        = 1
tzbudiachro%njh        = 1
!1->iku includes non-physical levels (IKU=NKMAX+2*JPVEXT)
!This does not conform to documentation (limits are in the physical domain)
!These values are not written in the netCDF files
!These values are written in the LFI files. They are kept for backward compatibility (and not set to default values)
tzbudiachro%nkl        = 1
tzbudiachro%nkh        = iku

call Write_diachro( tpdiafile, tzbudiachro, tzfields, tprofilers_time%tpdates, xwork6(:,:,:,:,:,:jproc) )

Deallocate( tzfields )
call Sensor_write_workarrays_deallocate( )

!----------------------------------------------------------------------------
!Treat point values

IPROC = 4
IF (LDIAG_SURFRAD_PROF) THEN
  IPROC = IPROC + 10
  IF(CRAD/="NONE")  IPROC = IPROC + 8
  IPROC = IPROC + 1 ! XSFCO2 term
END IF
IF( CRAD /= 'NONE' )  IPROC = IPROC + 1 !Tsrad term

call Sensor_write_workarrays_allocate( 1, istore, iproc )

! xrhod_sensor NOT computed => not written
! if needed, please add its computation in STATION_n
! call Add_point( 'Rhod', 'Density of dry air', 'kg m-3', tpstation%xrhod_sensor )

if ( ldiag_surfrad_prof ) call Add_diag_surfrad_data( tpprofiler )

call Add_point( 'IWV', 'Integrated Water Vapour',   'kg m-2', tpprofiler%xiwv )
call Add_point( 'ZTD', 'Zenith Tropospheric Delay', 'm',      tpprofiler%xztd )
call Add_point( 'ZWD', 'Zenith Wet Delay',          'm',      tpprofiler%xzwd )
call Add_point( 'ZHD', 'Zenith Hydrostatic Delay',  'm',      tpprofiler%xzhd )

if ( crad /= 'NONE' ) call Add_point( 'Tsrad', 'Radiative Surface Temperature', 'K', tpprofiler%xtsrad(:) )

jproc = Sensor_current_processes_number_get()

Allocate( tzfields( jproc ) )

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
tzfields(:)%ndimlist(4) = NMNHDIM_PROFILER_TIME
tzfields(:)%ndimlist(5) = NMNHDIM_UNUSED
tzfields(:)%ndimlist(6) = NMNHDIM_PROFILER_PROC

tzbudiachro%lleveluse(NLVL_CATEGORY)    = .true.
tzbudiachro%clevels  (NLVL_CATEGORY)    = 'Profilers'
tzbudiachro%ccomments(NLVL_CATEGORY)    = 'Level for the different vertical profilers'

tzbudiachro%lleveluse(NLVL_SUBCATEGORY) = .false.
tzbudiachro%clevels  (NLVL_SUBCATEGORY) = ''
tzbudiachro%ccomments(NLVL_SUBCATEGORY) = ''

tzbudiachro%lleveluse(NLVL_GROUP)       = .true.
tzbudiachro%clevels  (NLVL_GROUP)       = tpprofiler%cname
tzbudiachro%ccomments(NLVL_GROUP)       = 'Data at position of profiler ' // Trim( tpprofiler%cname )

tzbudiachro%lleveluse(NLVL_SHAPE)       = .true.
tzbudiachro%clevels  (NLVL_SHAPE)       = 'Point'
tzbudiachro%ccomments(NLVL_SHAPE)       = 'Values at position of profiler ' // Trim( tpprofiler%cname )

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
!Horizontal boundaries in physical domain does not make sense here (but flyer position does)
!These values are not written in the netCDF files
!These values are written in the LFI files. They are kept for backward compatibility (and not set to default values)
tzbudiachro%nil        = 1
tzbudiachro%nih        = 1
tzbudiachro%njl        = 1
tzbudiachro%njh        = 1
tzbudiachro%nkl        = 1
tzbudiachro%nkh        = 1

call Write_diachro( tpdiafile, tzbudiachro, tzfields, tprofilers_time%tpdates, xwork6(:,:,:,:,:,:jproc) )

Deallocate( tzfields )
call Sensor_write_workarrays_deallocate( )

!----------------------------------------------------------------------------
!Treat position

IPROC = 3

call Sensor_write_workarrays_allocate( 1, 1, iproc )

if ( lcartesian ) then
  call Add_fixpoint( 'X', 'X position', 'm', TPPROFILER%XX_CUR )
  call Add_fixpoint( 'Y', 'Y position', 'm', TPPROFILER%XY_CUR )
else
  call Add_fixpoint( 'LON', 'longitude', 'degree', TPPROFILER%XLON_CUR )
  call Add_fixpoint( 'LAT', 'latitude',  'degree', TPPROFILER%XLAT_CUR )
end if

call Add_fixpoint( 'Z', 'altitude', 'm', TPPROFILER%XZ_CUR )

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
tzfields(:)%ndimlist(6) = NMNHDIM_PROFILER_PROC

call Write_diachro( tpdiafile, tzbudiachro, tzfields, tprofilers_time%tpdates, xwork6(:,:,:,:,:,:jproc) )

Deallocate( tzfields )
call Sensor_write_workarrays_deallocate( )

END SUBROUTINE PROFILER_DIACHRO_n

END MODULE MODE_WRITE_PROFILER_n
