!MNH_LIC Copyright 2012-2017 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!     ################################
      MODULE MODI_READ_LIMA_DATA_NETCDF_CASE
!     #################################
INTERFACE
SUBROUTINE READ_LIMA_DATA_NETCDF_CASE(TPPRE_REAL1,HFILE,TPPGDFILE, &
                                      PTIME_HORI,KVERB,ODUMMY_REAL ) 
!
USE MODD_IO, ONLY: TFILEDATA
!
TYPE(TFILEDATA),POINTER,INTENT(IN) :: TPPRE_REAL1 ! PRE_REAL1 file
CHARACTER(LEN=28),  INTENT(IN)    :: HFILE      ! name of the NETCDF file
TYPE(TFILEDATA),    INTENT(IN)    :: TPPGDFILE  ! physiographic data file
REAL,               INTENT(INOUT) :: PTIME_HORI ! time spent in hor. interpolations
INTEGER,            INTENT(IN)    :: KVERB      ! verbosity level
LOGICAL,            INTENT(IN)    :: ODUMMY_REAL! flag to interpolate dummy fields
END SUBROUTINE READ_LIMA_DATA_NETCDF_CASE
!
END INTERFACE
END MODULE MODI_READ_LIMA_DATA_NETCDF_CASE
!     ####################################################################
      SUBROUTINE READ_LIMA_DATA_NETCDF_CASE(TPPRE_REAL1,HFILE,TPPGDFILE, &
                                            PTIME_HORI,KVERB,ODUMMY_REAL ) 
!     ####################################################################
!
!!****  *READ_LIMA_DATA_NETCDF_CASE* - reads data for the initialization of real cases.
!!
!!    PURPOSE
!!    -------
!     This routine reads the two input files :
!       The PGD which is closed after reading
!       The NETCDF file
!     Projection is read in READ_LFIFM_PGD (MODD_GRID).
!     Grid and definition of large domain are read in PGD file and 
!           NETCDF files.
!     The PGD files are also read in READ_LFIFM_PGD.
!     The PGD file is closed.
!     Vertical grid is defined in READ_VER_GRID.
!     PGD fields are stored on MESO-NH domain (in TRUNC_PGD).
!!
!!**  METHOD
!!    ------
!!  0. Declarations
!!    1. Declaration of arguments
!!    2. Declaration of local variables
!!  1. Read PGD file
!!    1. Domain restriction
!!    2. Coordinate conversion to lat,lon system
!!  2. Read Netcdf fields
!!  3. Vertical grid
!!  4. Free all temporary allocations
!!
!!    EXTERNAL
!!    --------
!!    subroutine READ_LFIFM_PGD    : to read PGD file
!!    subroutine READ_VER_GRID     : to read the vertical grid in namelist file.
!!    subroutine HORIBL            : horizontal bilinear interpolation
!!    subroutine XYTOLATLON        : projection from conformal to lat,lon
!!
!!    Module     MODI_READ_VER_GRID     : interface for subroutine READ_VER_GRID
!!    Module     MODI_HORIBL            : interface for subroutine HORIBL
!!    Module     MODI_XYTOLATLON        : interface for subroutine XYTOLATLON
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!      Module MODD_CONF      : contains configuration variables for all models.
!!         NVERB : verbosity level for output-listing
!!      Module MODD_LUNIT     : contains logical unit names for all models
!!         CLUOUT0 : name of output-listing
!!      Module MODD_PGDDIM    : contains dimension of PGD fields
!!         NPGDIMAX: dimension along x (no external point)
!!         NPGDJMAX: dimension along y (no external point)
!!      Module MODD_PARAMETERS
!!         JPHEXT
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    23/01/12 (C. Mari) 
!!      P. Wautelet 30/10/17 use F90 module for netCDF
!-------------------------------------------------------------------------------
!
!*      0. DECLARATIONS
!          ------------
!
USE MODD_BLANK_n
USE MODD_CH_AEROSOL, ONLY: CORGANIC, NCARB, NSOA, NSP, LORILAM,&
                           JPMODE, LVARSIGI, LVARSIGJ,CAERONAMES
USE MODD_CH_M9_n,    ONLY: NEQ ,  CNAMES
USE MODD_CH_MNHC_n,  ONLY: LUSECHEM,LUSECHAQ,LUSECHIC,LCH_PH
USE MODD_CONF
USE MODD_CONF_n
USE MODD_CST
USE MODD_DIM_n
USE MODD_GRID
USE MODD_GRID_n
USE MODD_IO,      ONLY: TFILEDATA
USE MODD_LUNIT,      ONLY: TLUOUT0
USE MODE_MODELN_HANDLER
USE MODD_NETCDF,     ONLY:CDFINT
USE MODD_NSV  
USE MODD_PARAMETERS
USE MODD_PARAM_n,    ONLY : CTURB
USE MODD_PREP_REAL
USE MODD_TIME
USE MODD_TIME_n
!
!UPG*PT
!USE MODE_FM
!USE MODE_IO_ll
USE MODE_IO
USE MODE_TOOLS_ll
!UPG*PT
USE MODE_MPPDB
USE MODE_THERMO
USE MODE_TIME
!
USE MODI_CH_AER_INIT_SOA
USE MODI_CH_INIT_SCHEME_n
USE MODI_CH_OPEN_INPUT  
USE MODI_HORIBL
USE MODI_INI_NSV
USE MODI_READ_HGRID_n
USE MODI_READ_VER_GRID
USE MODI_XYTOLATLON
!
USE NETCDF
!
USE MODD_PARAM_n,    ONLY : CCLOUD
USE MODD_PARAM_LIMA, ONLY : NMOD_CCN, LSCAV, LAERO_MASS, HINI_CCN, HTYPE_CCN, &
                            NMOD_IFN, NMOD_IMM, LHHONI, NINDICE_CCN_IMM
!
IMPLICIT NONE
!
!* 0.1. Declaration of arguments
!       ------------------------
!
TYPE(TFILEDATA),POINTER,INTENT(IN) :: TPPRE_REAL1 ! PRE_REAL1 file
CHARACTER(LEN=28),  INTENT(IN)    :: HFILE      ! name of the NETCDF file
TYPE(TFILEDATA),    INTENT(IN)    :: TPPGDFILE  ! physiographic data file
REAL,               INTENT(INOUT) :: PTIME_HORI ! time spent in hor. interpolations
INTEGER,            INTENT(IN)    :: KVERB      ! verbosity level
LOGICAL,            INTENT(IN)    :: ODUMMY_REAL! flag to interpolate dummy fields
!
!* 0.2 Declaration of local variables
!      ------------------------------
! General purpose variables
INTEGER                            :: ILUOUT0       ! Unit used for output msg.
INTEGER                            :: JI,JJ,JK      ! Dummy counters
INTEGER                            :: JLOOP1
! Variables used by the PGD reader
CHARACTER(LEN=28)                  :: YPGD_NAME     ! not used - dummy argument
CHARACTER(LEN=28)                  :: YPGD_DAD_NAME ! not used - dummy argument
CHARACTER(LEN=2)                   :: YPGD_TYPE     ! not used - dummy argument
! PGD Grib definition variables
INTEGER                            :: INO           ! Number of points of the grid
INTEGER                            :: IIU           ! Number of points along X
INTEGER                            :: IJU           ! Number of points along Y
REAL, DIMENSION(:), ALLOCATABLE    :: ZLONOUT       ! mapping PGD -> Grib (lon.)
REAL, DIMENSION(:), ALLOCATABLE    :: ZLATOUT       ! mapping PGD -> Grib (lat.)
REAL, DIMENSION(:,:), ALLOCATABLE  :: ZXM           ! X of PGD mass points
REAL, DIMENSION(:,:), ALLOCATABLE  :: ZYM           ! Y of PGD mass points
REAL, DIMENSION(:,:), ALLOCATABLE  :: ZLATM         ! Lat of PGD mass points
REAL, DIMENSION(:,:), ALLOCATABLE  :: ZLONM         ! Lon of PGD mass points
! Variable involved in the task of reading the netcdf  file
REAL,DIMENSION(:,:),ALLOCATABLE    :: ZVALUE        ! Intermediate array
REAL,DIMENSION(:),ALLOCATABLE      :: ZVALUE1D        ! Intermediate array
REAL,DIMENSION(:,:),ALLOCATABLE    :: ZOUT          ! Intermediate arrays
REAL,DIMENSION(:),ALLOCATABLE      :: ZOUT1D          ! Intermediate arrays
! model indice
INTEGER                           :: IMI
TYPE(TFILEDATA),POINTER                       :: TZFILE
!
! For netcdf 
!
integer(kind=CDFINT) :: status, ncid, varid
integer(kind=CDFINT) :: lat_varid, lon_varid, lev_varid, time_varid 
integer(kind=CDFINT) :: a_varid, b_varid, p0_varid, ps_varid, t_varid, q_varid 
integer(kind=CDFINT) :: mmr_dust1_varid, mmr_dust2_varid, mmr_dust3_varid
integer(kind=CDFINT) :: mmr_seasalt1_varid, mmr_seasalt2_varid, mmr_seasalt3_varid
integer(kind=CDFINT) :: mmr_bc_hydrophilic_varid, mmr_bc_hydrophobic_varid
integer(kind=CDFINT) :: mmr_oc_hydrophilic_varid, mmr_oc_hydrophobic_varid
integer(kind=CDFINT) :: mmr_sulfaer_varid
integer(kind=CDFINT) :: recid, latid, lonid, levid, timeid
integer(kind=CDFINT) :: latlen, lonlen, levlen, nrecs,timelen
integer(kind=CDFINT) :: KILEN
CHARACTER(LEN=40)                     :: recname
REAL, DIMENSION(:), ALLOCATABLE       :: lats
REAL, DIMENSION(:), ALLOCATABLE       :: lons 
REAL, DIMENSION(:), ALLOCATABLE       :: levs 
INTEGER, DIMENSION(:), ALLOCATABLE    :: count3d, start3d
INTEGER, DIMENSION(:), ALLOCATABLE    :: count2d, start2d 
REAL, DIMENSION(:), ALLOCATABLE       :: time, a, b 
REAL                                  :: p0 
INTEGER, DIMENSION(:), ALLOCATABLE    :: kinlo 
REAL, DIMENSION(:,:,:), ALLOCATABLE   :: mmr_dust1, mmr_dust2, mmr_dust3
REAL, DIMENSION(:,:,:), ALLOCATABLE   :: mmr_seasalt1, mmr_seasalt2, mmr_seasalt3
REAL, DIMENSION(:,:,:), ALLOCATABLE   :: mmr_bc_hydrophilic, mmr_bc_hydrophobic
REAL, DIMENSION(:,:,:), ALLOCATABLE   :: mmr_oc_hydrophilic, mmr_oc_hydrophobic
REAL, DIMENSION(:,:,:), ALLOCATABLE   :: mmr_sulfaer
REAL, DIMENSION(:,:,:), ALLOCATABLE   :: ZWORK
!REAL, DIMENSION(:,:,:), ALLOCATABLE   :: TMOZ, QMOZ, PSMOZ
REAL, DIMENSION(:,:,:), ALLOCATABLE   :: ZTCAM, ZQCAM
REAL, DIMENSION(:,:), ALLOCATABLE     :: ZPSCAM
REAL                                  :: scale, offset
! for reverse altitude
REAL, DIMENSION(:), ALLOCATABLE       :: TMP1, TMP2
REAL, DIMENSION(:,:,:), ALLOCATABLE   :: TMP3
REAL, DIMENSION(:,:,:,:), ALLOCATABLE :: TMP4,TMP5
!----------------------------------------------------------------------
TZFILE => NULL()
!
IMI = GET_CURRENT_MODEL_INDEX()
!
!--------------------------------------------------------------
!
!* 1. READ PGD FILE
!     -------------
!
ILUOUT0 = TLUOUT0%NLU
CALL READ_HGRID_n(TPPGDFILE,YPGD_NAME,YPGD_DAD_NAME,YPGD_TYPE)
!
! 1.1 Domain restriction
!
CALL GET_DIM_EXT_ll('B',IIU,IJU)
INO = IIU * IJU
!
!
! 1.2 Coordinate conversion to lat,lon system
!
ALLOCATE (ZXM(IIU,IJU))
ALLOCATE (ZYM(IIU,IJU))
ALLOCATE (ZLONM(IIU,IJU))
ALLOCATE (ZLATM(IIU,IJU))
ZXM(1:IIU-1,1) = (XXHAT(1:IIU-1) + XXHAT(2:IIU) ) / 2.
ZXM(IIU,1)     = XXHAT(IIU) - XXHAT(IIU-1) + ZXM(IIU-1,1)
ZXM(:,2:IJU)   = SPREAD(ZXM(:,1),2,IJU-1)
ZYM(1,1:IJU-1) = (XYHAT(1:IJU-1) + XYHAT(2:IJU)) / 2.
ZYM(1,IJU)     = XYHAT(IJU) - XYHAT(IJU-1) + ZYM(1,IJU-1)
ZYM(2:IIU,:)   = SPREAD(ZYM(1,:),1,IIU-1)
CALL SM_XYTOLATLON_A (XLAT0,XLON0,XRPK,XLATORI,XLONORI,ZXM,ZYM,ZLATM,ZLONM, &
                      IIU,IJU)
ALLOCATE (ZLONOUT(INO))
ALLOCATE (ZLATOUT(INO))
JLOOP1 = 0
DO JJ = 1, IJU
  ZLONOUT(JLOOP1+1:JLOOP1+IIU) = ZLONM(1:IIU,JJ)
  ZLATOUT(JLOOP1+1:JLOOP1+IIU) = ZLATM(1:IIU,JJ)
  JLOOP1 = JLOOP1 + IIU
ENDDO
DEALLOCATE (ZYM)
DEALLOCATE (ZXM)
!
!--------------------------------------------------------------
!
!* 2. READ NETCDF FIELDS
!     ------------------
!
! 2.1 Open netcdf files
!
status = nf90_open(HFILE, nf90_nowrite, ncid) 
if (status /= nf90_noerr) call handle_err(status)
!
! 2.2 Read netcdf files
!
! get dimension IDs
!
!* get dimension ID of unlimited variable in netcdf file
status = nf90_inquire(ncid, unlimitedDimId = recid)
!status = nf90_inq_dimid(ncid, "time", timeid)
if (status /= nf90_noerr) call handle_err(status)
status = nf90_inq_dimid(ncid, "latitude", latid)
if (status /= nf90_noerr) call handle_err(status)
status = nf90_inq_dimid(ncid, "longitude", lonid)
if (status /= nf90_noerr) call handle_err(status)
status = nf90_inq_dimid(ncid, "level", levid)
if (status /= nf90_noerr) call handle_err(status)
!
! get dimensions
!
!* get dimension and name of unlimited variable in netcdf file
status = nf90_inquire_dimension(ncid, recid, name=recname, len=nrecs)
!status = nf90_inquire_dimension(ncid, timeid, len=nrecs)
if (status /= nf90_noerr) call handle_err(status)
status = nf90_inquire_dimension(ncid, latid, len=latlen)
if (status /= nf90_noerr) call handle_err(status)
status = nf90_inquire_dimension(ncid, lonid, len=lonlen)
if (status /= nf90_noerr) call handle_err(status)
status = nf90_inquire_dimension(ncid, levid, len=levlen)
if (status /= nf90_noerr) call handle_err(status)
!
! get variable IDs
!
status = nf90_inq_varid(ncid, "latitude", lat_varid)
if (status /= nf90_noerr) call handle_err(status)
status = nf90_inq_varid(ncid, "longitude", lon_varid)
if (status /= nf90_noerr) call handle_err(status)
status = nf90_inq_varid(ncid, "level", lev_varid)
if (status /= nf90_noerr) call handle_err(status)
status = nf90_inq_varid(ncid, "time", time_varid)
if (status /= nf90_noerr) call handle_err(status)
!
!!! status = nf90_inq_varid(ncid, "a", a_varid)
!!! if (status /= nf90_noerr) call handle_err(status)
!!! status = nf90_inq_varid(ncid, "b", b_varid)
!!! if (status /= nf90_noerr) call handle_err(status)
!
status = nf90_inq_varid(ncid, "aermr04", mmr_dust1_varid)
if (status /= nf90_noerr) call handle_err(status)
status = nf90_inq_varid(ncid, "aermr05", mmr_dust2_varid)
if (status /= nf90_noerr) call handle_err(status)
status = nf90_inq_varid(ncid, "aermr06", mmr_dust3_varid)
if (status /= nf90_noerr) call handle_err(status)
!
status = nf90_inq_varid(ncid, "aermr01", mmr_seasalt1_varid)
if (status /= nf90_noerr) call handle_err(status)
status = nf90_inq_varid(ncid, "aermr02", mmr_seasalt2_varid)
if (status /= nf90_noerr) call handle_err(status)
status = nf90_inq_varid(ncid, "aermr03", mmr_seasalt3_varid)
if (status /= nf90_noerr) call handle_err(status)
!
status = nf90_inq_varid(ncid, "aermr10", mmr_bc_hydrophilic_varid)
if (status /= nf90_noerr) call handle_err(status)
status = nf90_inq_varid(ncid, "aermr09", mmr_bc_hydrophobic_varid)
if (status /= nf90_noerr) call handle_err(status)
!
status = nf90_inq_varid(ncid, "aermr08", mmr_oc_hydrophilic_varid)
if (status /= nf90_noerr) call handle_err(status)
status = nf90_inq_varid(ncid, "aermr07", mmr_oc_hydrophobic_varid)
if (status /= nf90_noerr) call handle_err(status)
!
status = nf90_inq_varid(ncid, "aermr11", mmr_sulfaer_varid)
if (status /= nf90_noerr) call handle_err(status)
!
!!! status = nf90_inq_varid(ncid, "p0", p0_varid)
!!! if (status /= nf90_noerr) call handle_err(status)
status = nf90_inq_varid(ncid, "sp", ps_varid)
if (status /= nf90_noerr) call handle_err(status)
status = nf90_inq_varid(ncid, "t", t_varid)
if (status /= nf90_noerr) call handle_err(status)
status = nf90_inq_varid(ncid, "q", q_varid)
if (status /= nf90_noerr) call handle_err(status)
!

KILEN = latlen * lonlen
!
! 2.3 Read data.
!
ALLOCATE (count3d(4))
ALLOCATE (start3d(4))
ALLOCATE (count2d(3))
ALLOCATE (start2d(3))
ALLOCATE (lats(latlen))
ALLOCATE (lons(lonlen))
ALLOCATE (levs(levlen))
ALLOCATE (kinlo(latlen))
kinlo(:) = lonlen
!ALLOCATE (time(nrecs))
!ALLOCATE (a(levlen))
!ALLOCATE (b(levlen))
! T, Q, Ps :
ALLOCATE (ZTCAM(lonlen,latlen,levlen))
ALLOCATE (ZQCAM(lonlen,latlen,levlen))
!ALLOCATE (ZPSCAM(lonlen,latlen,levlen))
ALLOCATE (ZPSCAM(lonlen,latlen))
! transformed a, b :
ALLOCATE (XA_SV_LS(levlen))
ALLOCATE (XB_SV_LS(levlen))
! meteo var
ALLOCATE (XT_SV_LS(IIU,IJU,levlen))
ALLOCATE (XQ_SV_LS(IIU,IJU,levlen,1))
ALLOCATE (XPS_SV_LS(IIU,IJU))
ALLOCATE (XZS_SV_LS(IIU,IJU))
! take the orography from ECMWF
XZS_SV_LS(:,:) = XZS_LS(:,:)
! aerosol mr from CAMS or MACC
ALLOCATE (mmr_dust1(lonlen,latlen,levlen))
ALLOCATE (mmr_dust2(lonlen,latlen,levlen))
ALLOCATE (mmr_dust3(lonlen,latlen,levlen))
!
ALLOCATE (mmr_seasalt1(lonlen,latlen,levlen))
ALLOCATE (mmr_seasalt2(lonlen,latlen,levlen))
ALLOCATE (mmr_seasalt3(lonlen,latlen,levlen))
!
ALLOCATE (mmr_bc_hydrophilic(lonlen,latlen,levlen))
ALLOCATE (mmr_bc_hydrophobic(lonlen,latlen,levlen))
!
ALLOCATE (mmr_oc_hydrophilic(lonlen,latlen,levlen))
ALLOCATE (mmr_oc_hydrophobic(lonlen,latlen,levlen))
!
ALLOCATE (mmr_sulfaer(lonlen,latlen,levlen))
!
ALLOCATE (ZWORK(lonlen,latlen,levlen))
!
! get values of variables
!
status = nf90_get_var(ncid, lat_varid, lats(:))
if (status /= nf90_noerr) call handle_err(status)
status = nf90_get_var(ncid, lon_varid, lons(:))
if (status /= nf90_noerr) call handle_err(status)
status = nf90_get_var(ncid, lev_varid, levs(:))
if (status /= nf90_noerr) call handle_err(status)
!!! status = nf90_get_var(ncid, time_varid, time(:))
!!! if (status /= nf90_noerr) call handle_err(status)
!!! status = nf90_get_var(ncid, a_varid, a(:))
!!! if (status /= nf90_noerr) call handle_err(status)
!!! status = nf90_get_var(ncid, b_varid, b(:))
!!! if (status /= nf90_noerr) call handle_err(status)
!!! status = nf90_get_var(ncid, p0_varid, p0)
!!! if (status /= nf90_noerr) call handle_err(status)
!
! Reference pressure (needed for the vertical interpolation)
!
!!! XP00_SV_LS = p0
XP00_SV_LS = 101325.0
!
! a and b coefficients (needed for the vertical interpolation)
!
XA_SV_LS(:) = (/ 20.000000000, 38.425343000, 63.647804000, 95.636963000, 134.48330700, &
                 180.58435100, 234.77905300, 298.49578900, 373.97192400, 464.61813400, &
                 575.65100100, 713.21807900, 883.66052200, 1094.8347170, 1356.4746090, &
                 1680.6402590, 2082.2739260, 2579.8886720, 3196.4216310, 3960.2915040, &
                 4906.7084960, 6018.0195310, 7306.6313480, 8765.0537110, 10376.126953, &
                 12077.446289, 13775.325195, 15379.805664, 16819.474609, 18045.183594, &
                 19027.695313, 19755.109375, 20222.205078, 20429.863281, 20384.480469, &
                 20097.402344, 19584.330078, 18864.750000, 17961.357422, 16899.468750, &
                 15706.447266, 14411.124023, 13043.218750, 11632.758789, 10209.500977, &
                 8802.3564450, 7438.8032230, 6144.3149410, 4941.7783200, 3850.9133300, &
                 2887.6965330, 2063.7797850, 1385.9125980, 855.36175500, 467.33358800, &
                 210.39389000, 65.889244000, 7.3677430000, 0.0000000000, 0.0000000000  /)

XA_SV_LS(:) = XA_SV_LS(:) / XP00_SV_LS

XB_SV_LS(:) = (/ 0.00000000, 0.00000000, 0.00000000, 0.00000000, 0.00000000, &
                 0.00000000, 0.00000000, 0.00000000, 0.00000000, 0.00000000, &
                 0.00000000, 0.00000000, 0.00000000, 0.00000000, 0.00000000, &
                 0.00000000, 0.00000000, 0.00000000, 0.00000000, 0.00000000, &
                 0.00000000, 0.00000000, 0.00000000, 0.00007582, 0.00046139, &
                 0.00181516, 0.00508112, 0.01114291, 0.02067788, 0.03412116, &
                 0.05169041, 0.07353383, 0.09967469, 0.13002251, 0.16438432, &
                 0.20247594, 0.24393314, 0.28832296, 0.33515489, 0.38389215, &
                 0.43396294, 0.48477158, 0.53570992, 0.58616841, 0.63554746, &
                 0.68326861, 0.72878581, 0.77159661, 0.81125343, 0.84737492, &
                 0.87965691, 0.90788388, 0.93194032, 0.95182151, 0.96764523, &
                 0.97966272, 0.98827010, 0.99401945, 0.99763012, 1.00000000  /)
!
!     Read 1 record of lon*lat values, starting at the
!     beginning of the record (the (1, 1, rec=time) element in the netCDF
!     file).
count2d(1) = lonlen
count2d(2) = latlen
count2d(3) = 1
start2d(1) = 1
start2d(2) = 1
start2d(3) = 1
!
!     Read 1 record of lon*lat*lev values, starting at the
!     beginning of the record (the (1, 1, 1, rec=time) element in the netCDF
!     file).
count3d(1) = lonlen
count3d(2) = latlen
count3d(3) = levlen
count3d(4) = 1
start3d(1) = 1
start3d(2) = 1
start3d(3) = 1
start3d(4) = 1
!
! Temperature and spec. hum. (needed for the vertical interpolation)
!
status = nf90_get_var(ncid, t_varid, ZTCAM(:,:,:), start=start3d, count=count3d)
if (status /= nf90_noerr) call handle_err(status)
status = nf90_get_att(ncid, t_varid, "scale_factor", scale) 
status = nf90_get_att(ncid, t_varid, "add_offset", offset) 
ZTCAM(:,:,:) = offset + scale * ZTCAM(:,:,:)
!
status = nf90_get_var(ncid, q_varid, ZQCAM(:,:,:), start=start3d, count=count3d)
if (status /= nf90_noerr) call handle_err(status)
status = nf90_get_att(ncid, q_varid, "scale_factor", scale) 
status = nf90_get_att(ncid, q_varid, "add_offset", offset) 
ZQCAM(:,:,:) = offset + scale * ZQCAM(:,:,:)
!
status = nf90_get_var(ncid, ps_varid, ZPSCAM(:,:), start=start2d, count=count2d)
if (status /= nf90_noerr) call handle_err(status)
status = nf90_get_att(ncid, ps_varid, "scale_factor", scale) 
status = nf90_get_att(ncid, ps_varid, "add_offset", offset) 
ZPSCAM(:,:) = offset + scale * ZPSCAM(:,:)
!ZPSCAM(:,:) = EXP( ZPSCAM(:,:) )
!
! Aerosol concentrations
!
status = nf90_get_var(ncid, mmr_dust1_varid, mmr_dust1(:,:,:), start=start3d, count=count3d)
if (status /= nf90_noerr) call handle_err(status)
status = nf90_get_att(ncid, mmr_dust1_varid, "scale_factor", scale) 
status = nf90_get_att(ncid, mmr_dust1_varid, "add_offset", offset) 
mmr_dust1(:,:,:) = offset + scale * mmr_dust1(:,:,:)
!
status = nf90_get_var(ncid, mmr_dust2_varid, mmr_dust2(:,:,:), start=start3d, count=count3d)
if (status /= nf90_noerr) call handle_err(status)
status = nf90_get_att(ncid, mmr_dust2_varid, "scale_factor", scale) 
status = nf90_get_att(ncid, mmr_dust2_varid, "add_offset", offset) 
mmr_dust2(:,:,:) = offset + scale * mmr_dust2(:,:,:)
!
status = nf90_get_var(ncid, mmr_dust3_varid, mmr_dust3(:,:,:), start=start3d, count=count3d)
if (status /= nf90_noerr) call handle_err(status)
status = nf90_get_att(ncid, mmr_dust3_varid, "scale_factor", scale) 
status = nf90_get_att(ncid, mmr_dust3_varid, "add_offset", offset) 
mmr_dust3(:,:,:) = offset + scale * mmr_dust3(:,:,:)
!
!
status = nf90_get_var(ncid, mmr_seasalt1_varid, mmr_seasalt1(:,:,:), start=start3d, count=count3d)
if (status /= nf90_noerr) call handle_err(status)
status = nf90_get_att(ncid, mmr_seasalt1_varid, "scale_factor", scale) 
status = nf90_get_att(ncid, mmr_seasalt1_varid, "add_offset", offset) 
mmr_seasalt1(:,:,:) = offset + scale * mmr_seasalt1(:,:,:)
!
status = nf90_get_var(ncid, mmr_seasalt2_varid, mmr_seasalt2(:,:,:), start=start3d, count=count3d)
if (status /= nf90_noerr) call handle_err(status)
status = nf90_get_att(ncid, mmr_seasalt2_varid, "scale_factor", scale) 
status = nf90_get_att(ncid, mmr_seasalt2_varid, "add_offset", offset) 
mmr_seasalt2(:,:,:) = offset + scale * mmr_seasalt2(:,:,:)
!
status = nf90_get_var(ncid, mmr_seasalt3_varid, mmr_seasalt3(:,:,:), start=start3d, count=count3d)
if (status /= nf90_noerr) call handle_err(status)
status = nf90_get_att(ncid, mmr_seasalt3_varid, "scale_factor", scale) 
status = nf90_get_att(ncid, mmr_seasalt3_varid, "add_offset", offset) 
mmr_seasalt3(:,:,:) = offset + scale * mmr_seasalt3(:,:,:)
!
!
status = nf90_get_var(ncid, mmr_bc_hydrophilic_varid, mmr_bc_hydrophilic(:,:,:), start=start3d, count=count3d)
if (status /= nf90_noerr) call handle_err(status)
status = nf90_get_att(ncid, mmr_bc_hydrophilic_varid, "scale_factor", scale) 
status = nf90_get_att(ncid, mmr_bc_hydrophilic_varid, "add_offset", offset) 
mmr_bc_hydrophilic(:,:,:) = offset + scale * mmr_bc_hydrophilic(:,:,:)
!
status = nf90_get_var(ncid, mmr_bc_hydrophobic_varid, mmr_bc_hydrophobic(:,:,:), start=start3d, count=count3d)
if (status /= nf90_noerr) call handle_err(status)
status = nf90_get_att(ncid, mmr_bc_hydrophobic_varid, "scale_factor", scale) 
status = nf90_get_att(ncid, mmr_bc_hydrophobic_varid, "add_offset", offset) 
mmr_bc_hydrophobic(:,:,:) = offset + scale * mmr_bc_hydrophobic(:,:,:)
!
!
status = nf90_get_var(ncid, mmr_oc_hydrophilic_varid, mmr_oc_hydrophilic(:,:,:), start=start3d, count=count3d)
if (status /= nf90_noerr) call handle_err(status)
status = nf90_get_att(ncid, mmr_oc_hydrophilic_varid, "scale_factor", scale) 
status = nf90_get_att(ncid, mmr_oc_hydrophilic_varid, "add_offset", offset) 
mmr_oc_hydrophilic(:,:,:) = offset + scale * mmr_oc_hydrophilic(:,:,:)
!
status = nf90_get_var(ncid, mmr_oc_hydrophobic_varid, mmr_oc_hydrophobic(:,:,:), start=start3d, count=count3d)
if (status /= nf90_noerr) call handle_err(status)
status = nf90_get_att(ncid, mmr_oc_hydrophobic_varid, "scale_factor", scale) 
status = nf90_get_att(ncid, mmr_oc_hydrophobic_varid, "add_offset", offset) 
mmr_oc_hydrophobic(:,:,:) = offset + scale * mmr_oc_hydrophobic(:,:,:)
!
!
status = nf90_get_var(ncid, mmr_sulfaer_varid, mmr_sulfaer(:,:,:), start=start3d, count=count3d)
if (status /= nf90_noerr) call handle_err(status)
status = nf90_get_att(ncid, mmr_sulfaer_varid, "scale_factor", scale) 
status = nf90_get_att(ncid, mmr_sulfaer_varid, "add_offset", offset) 
mmr_sulfaer(:,:,:) = offset + scale * mmr_sulfaer(:,:,:)
!
!--------------------------------------------------------------
!
!*  3 Conversion of MACC or CAMS variables into LIMA variables
!     ------------------------------------------------
!
! initialise NSV_* variables
! cas simple : 3 modes de CCN (dont 1 actif par immersion), 2 modes IFN
! CCN1 : seasalt
! CCN2 : sulfates
! CCN3 (IMM) : hydrophilic OM and BC
! IFN1 : dust
! IFN2 : hydrophobic OM and BC
!
! XSV : Nc, Nr, 3 CCN free, 3 CCN activés, Ni, 2 IN free, 2 IN activé = 11 variables
!
! Concentrations en nombre par kilo !
!
CCLOUD='LIMA'
NMOD_CCN=3
LSCAV=.FALSE.
LAERO_MASS=.FALSE.
NMOD_IFN=2
NMOD_IMM=1
LHHONI=.FALSE.
HINI_CCN='AER'
HTYPE_CCN(1)='M'
HTYPE_CCN(2)='C'
HTYPE_CCN(3)='C'
!
!   3.1 initialize lima sv var. 
!
! Always initialize chemical scheme variables before INI_NSV call !
CALL CH_INIT_SCHEME_n(IMI,LUSECHAQ,LUSECHIC,LCH_PH,ILUOUT0,KVERB)
IF (LORILAM) THEN
   CORGANIC = "MPMPO"
   LVARSIGI = .TRUE.
   LVARSIGJ = .TRUE.
   CALL CH_AER_INIT_SOA(ILUOUT0, KVERB)
END IF
!
CALL INI_NSV(1)
DEALLOCATE(XSV_LS_LIMA)
ALLOCATE (XSV_LS_LIMA(IIU,IJU,levlen,NSV))
XSV_LS_LIMA(:,:,:,:) = 0.
!
ALLOCATE(NINDICE_CCN_IMM(1))
NINDICE_CCN_IMM(1)=3
!
! Define work arrays
!
ALLOCATE(ZVALUE(levlen,KILEN))
ALLOCATE(ZVALUE1D(KILEN))
ALLOCATE(ZOUT(levlen,INO))
ALLOCATE(ZOUT1D(INO))
!
where (ZLONOUT(:) < 0.) ZLONOUT(:) = ZLONOUT(:) + 360. ! correct longitudes
!
!
!  3.2 Select CAMS/MACC mixing ratios and perform the horizontal interpolation
!
! Free CCN concentration (mode 1)
!
ZWORK(:,:,:)=mmr_seasalt1(:,:,:)+mmr_seasalt2(:,:,:)+mmr_seasalt3(:,:,:)
!!! ZWORK(:,:,:)=mmr_seasalt2(:,:,:)
!!!JPP ZWORK(:,:,:)=ZWORK(:,:,:)*1.E18/3620.
DO JK = 1, levlen
   JLOOP1 = 0
   DO JJ = 1, latlen
      ZVALUE(JK,JLOOP1+1:JLOOP1+lonlen) = ZWORK(1:lonlen,JJ,JK)
      JLOOP1 = JLOOP1 + lonlen
   ENDDO
   CALL HORIBL(lats(1),lons(1),lats(latlen),lons(lonlen), &
        latlen,kinlo,KILEN,                                &
        ZVALUE(JK,:),INO,ZLONOUT,ZLATOUT,                  &
        ZOUT(JK,:),.FALSE.,PTIME_HORI,.TRUE. )
   CALL ARRAY_1D_TO_2D(INO,ZOUT(JK,:),IIU,IJU,XSV_LS_LIMA(:,:,JK,NSV_LIMA_CCN_FREE))
ENDDO
!
! Free CCN concentration (mode 2)
!
!!!JPP ZWORK(:,:,:)=mmr_sulfaer(:,:,:)*1.E18/345
ZWORK(:,:,:)=mmr_sulfaer(:,:,:)
DO JK = 1, levlen
   JLOOP1 = 0
   DO JJ = 1, latlen
      ZVALUE(JK,JLOOP1+1:JLOOP1+lonlen) = ZWORK(1:lonlen,JJ,JK)
      JLOOP1 = JLOOP1 + lonlen
   ENDDO
   CALL HORIBL(lats(1),lons(1),lats(latlen),lons(lonlen), &
        latlen,kinlo,KILEN,                                &
        ZVALUE(JK,:),INO,ZLONOUT,ZLATOUT,                  &
        ZOUT(JK,:),.FALSE.,PTIME_HORI,.TRUE. )
   CALL ARRAY_1D_TO_2D(INO,ZOUT(JK,:),IIU,IJU,XSV_LS_LIMA(:,:,JK,NSV_LIMA_CCN_FREE + 1))
ENDDO
!
! Free CCN concentration (mode 3, IMM)
!
!!!JPP ZWORK(:,:,:)=mmr_bc_hydrophilic(:,:,:)*1.E18/20.
!!!JPP ZWORK(:,:,:)=ZWORK(:,:,:) + mmr_oc_hydrophilic(:,:,:)*1.E18/16.
ZWORK(:,:,:)=mmr_bc_hydrophilic(:,:,:)+mmr_oc_hydrophilic(:,:,:)
DO JK = 1, levlen
   JLOOP1 = 0
   DO JJ = 1, latlen
      ZVALUE(JK,JLOOP1+1:JLOOP1+lonlen) = ZWORK(1:lonlen,JJ,JK)
      JLOOP1 = JLOOP1 + lonlen
   ENDDO
   CALL HORIBL(lats(1),lons(1),lats(latlen),lons(lonlen), &
        latlen,kinlo,KILEN,                                &
        ZVALUE(JK,:),INO,ZLONOUT,ZLATOUT,                  &
        ZOUT(JK,:),.FALSE.,PTIME_HORI,.TRUE. )
   CALL ARRAY_1D_TO_2D(INO,ZOUT(JK,:),IIU,IJU,XSV_LS_LIMA(:,:,JK,NSV_LIMA_CCN_FREE + 2))
ENDDO
!
! Free IFN concentration (mode 1)
!
!!!JPP ZWORK(:,:,:)=mmr_dust2(:,:,:)*1.E18/(1204.*0.58)
!!!JPP ZWORK2(:,:,:)=max(0.,(mmr_dust3(:,:,:)*1.E18/1204.-2.4*ZWORK(:,:,:))/70.)
ZWORK(:,:,:)=mmr_dust1(:,:,:) + mmr_dust2(:,:,:) + mmr_dust3(:,:,:)
DO JK = 1, levlen
   JLOOP1 = 0
   DO JJ = 1, latlen
      ZVALUE(JK,JLOOP1+1:JLOOP1+lonlen) = ZWORK(1:lonlen,JJ,JK)
      JLOOP1 = JLOOP1 + lonlen
   ENDDO
   CALL HORIBL(lats(1),lons(1),lats(latlen),lons(lonlen), &
        latlen,kinlo,KILEN,                                &
        ZVALUE(JK,:),INO,ZLONOUT,ZLATOUT,                  &
        ZOUT(JK,:),.FALSE.,PTIME_HORI,.TRUE. )
   CALL ARRAY_1D_TO_2D(INO,ZOUT(JK,:),IIU,IJU,XSV_LS_LIMA(:,:,JK,NSV_LIMA_IFN_FREE))
ENDDO
!
! Free IFN concentration (mode 2)
!
!!!JPP ZWORK(:,:,:)=mmr_bc_hydrophobic(:,:,:)*1.E18/20.
!!!JPP ZWORK(:,:,:)=ZWORK(:,:,:) + mmr_oc_hydrophobic(:,:,:)*1.E18/16.
ZWORK(:,:,:)=mmr_bc_hydrophobic(:,:,:)+mmr_oc_hydrophobic(:,:,:)
DO JK = 1, levlen
   JLOOP1 = 0
   DO JJ = 1, latlen
      ZVALUE(JK,JLOOP1+1:JLOOP1+lonlen) = ZWORK(1:lonlen,JJ,JK)
      JLOOP1 = JLOOP1 + lonlen
   ENDDO
   CALL HORIBL(lats(1),lons(1),lats(latlen),lons(lonlen), &
        latlen,kinlo,KILEN,                                &
        ZVALUE(JK,:),INO,ZLONOUT,ZLATOUT,                  &
        ZOUT(JK,:),.FALSE.,PTIME_HORI,.TRUE. )
   CALL ARRAY_1D_TO_2D(INO,ZOUT(JK,:),IIU,IJU,XSV_LS_LIMA(:,:,JK,NSV_LIMA_IFN_FREE + 1))
ENDDO
!
!  3.3 Meteo ver. perform the horizontal interpolation
!
! Temperature (needed for the vertical interpolation) 
!
DO JK = 1, levlen
   JLOOP1 = 0
   DO JJ = 1, latlen
      ZVALUE(JK,JLOOP1+1:JLOOP1+lonlen) = ZTCAM(1:lonlen,JJ,JK)
      JLOOP1 = JLOOP1 + lonlen
   ENDDO
   CALL HORIBL(lats(1),lons(1),lats(latlen),lons(lonlen), &
        latlen,kinlo,KILEN,                                &
        ZVALUE(JK,:),INO,ZLONOUT,ZLATOUT,                  &
        ZOUT(JK,:),.FALSE.,PTIME_HORI,.TRUE. )
   CALL ARRAY_1D_TO_2D(INO,ZOUT(JK,:),IIU,IJU,XT_SV_LS(:,:,JK))
ENDDO  ! levlen
!
! Spec. Humidity (needed for the vertical interpolation) 
!
DO JK = 1, levlen
   JLOOP1 = 0
   DO JJ = 1, latlen
      ZVALUE(JK,JLOOP1+1:JLOOP1+lonlen) = ZQCAM(1:lonlen,JJ,JK)
      JLOOP1 = JLOOP1 + lonlen
   ENDDO
   CALL HORIBL(lats(1),lons(1),lats(latlen),lons(lonlen), &
        latlen,kinlo,KILEN,                                &
        ZVALUE(JK,:),INO,ZLONOUT,ZLATOUT,                  &
        ZOUT(JK,:),.FALSE.,PTIME_HORI,.TRUE. )
   CALL ARRAY_1D_TO_2D(INO,ZOUT(JK,:),IIU,IJU,XQ_SV_LS(:,:,JK,1))
ENDDO  ! levlen
!
! Surface pressure (needed for the vertical interpolation) 
!
JLOOP1 = 0
DO JJ = 1, latlen
   ZVALUE1D(JLOOP1+1:JLOOP1+lonlen) = ZPSCAM(1:lonlen,JJ)
   JLOOP1 = JLOOP1 + lonlen
ENDDO
CALL HORIBL(lats(1),lons(1),lats(latlen),lons(lonlen), &
     latlen,kinlo,KILEN,                                &
     ZVALUE1D(:),INO,ZLONOUT,ZLATOUT,                  &
     ZOUT1D(:),.FALSE.,PTIME_HORI,.TRUE. )
CALL ARRAY_1D_TO_2D(INO,ZOUT1D(:),IIU,IJU,XPS_SV_LS(:,:))
!
!  3.4 Correct negative values produced by the horizontal interpolations
!
XSV_LS_LIMA(:,:,:,:) = MAX(XSV_LS_LIMA(:,:,:,:),0.)
XPS_SV_LS(:,:)  = MAX(XPS_SV_LS(:,:),0.)
XT_SV_LS(:,:,:) = MAX(XT_SV_LS(:,:,:),0.)
XQ_SV_LS(:,:,:,1) = MAX(XQ_SV_LS(:,:,:,1),0.)
!
!  3.5 If Netcdf vertical levels have to be reversed :
!
ALLOCATE(TMP1(levlen))
ALLOCATE(TMP2(levlen))
ALLOCATE(TMP3(IIU,IJU,levlen))
ALLOCATE(TMP4(IIU,IJU,levlen,NRR))
ALLOCATE(TMP5(IIU,IJU,levlen,NSV))
DO JJ=1,levlen
   ! inv. lev
   TMP1(JJ)       = XA_SV_LS(levlen+1-JJ)
   TMP2(JJ)       = XB_SV_LS(levlen+1-JJ)
   TMP3(:,:,JJ)   = XT_SV_LS(:,:,levlen+1-JJ)
   TMP4(:,:,JJ,:) = XQ_SV_LS(:,:,levlen+1-JJ,:)
   TMP5(:,:,JJ,:)   = XSV_LS(:,:,levlen+1-JJ,:)
ENDDO
XA_SV_LS(:)       = TMP1(:)
XB_SV_LS(:)       = TMP2(:)
XT_SV_LS(:,:,:)   = TMP3(:,:,:)
XQ_SV_LS(:,:,:,:) = TMP4(:,:,:,:)
XSV_LS(:,:,:,:)   = TMP5(:,:,:,:)
DEALLOCATE(TMP1)
DEALLOCATE(TMP2)
DEALLOCATE(TMP3)
DEALLOCATE(TMP4)
DEALLOCATE(TMP5)
!
!  3.6 close the netcdf file
!
status = nf90_close(ncid) 
if (status /= nf90_noerr) call handle_err(status)
!
DEALLOCATE (ZVALUE)
DEALLOCATE (ZOUT)
!!
!-------------------------------------------------------------
!
!* 4. VERTICAL GRID
!     -------------      
!
!  4.1 Read VERTICAL GRID
!
WRITE (ILUOUT0,'(A)') ' | Reading of vertical grid in progress'
CALL READ_VER_GRID(TPPRE_REAL1)
!
!--------------------------------------------------------------
!
!  5. Free all temporary allocations
!     ------------------------------
!
DEALLOCATE (ZLATOUT)
DEALLOCATE (ZLONOUT)
DEALLOCATE (count3d)
DEALLOCATE (start3d)
DEALLOCATE (count2d)
DEALLOCATE (start2d)
!
DEALLOCATE (lats)
DEALLOCATE (lons)
DEALLOCATE (levs)
!DEALLOCATE (time)
!DEALLOCATE (a)
!DEALLOCATE (b)
! ps, T, Q :
DEALLOCATE (ZPSCAM)
DEALLOCATE (ZTCAM)
DEALLOCATE (ZQCAM)
!
DEALLOCATE (mmr_dust1)
DEALLOCATE (mmr_dust2)
DEALLOCATE (mmr_dust3)
!
DEALLOCATE (mmr_seasalt1)
DEALLOCATE (mmr_seasalt2)
DEALLOCATE (mmr_seasalt3)
!
DEALLOCATE (mmr_bc_hydrophilic)
DEALLOCATE (mmr_bc_hydrophobic)
!
DEALLOCATE (mmr_oc_hydrophilic)
DEALLOCATE (mmr_oc_hydrophobic)
!
DEALLOCATE (mmr_sulfaer)
!
DEALLOCATE (ZWORK)
!
WRITE (ILUOUT0,'(A,A4,A)') ' -- netcdf decoder for ',HFILE,' file ended successfully'
WRITE (ILUOUT0,'(A,A4,A)') 'MACC mixing ratios are interpolated horizontally'
!
!
CONTAINS
!
!     #############################
      SUBROUTINE HANDLE_ERR(STATUS)
!     #############################
     INTEGER(KIND=CDFINT) STATUS
     IF (STATUS .NE. NF90_NOERR) THEN
        PRINT *, NF90_STRERROR(STATUS)
     STOP 'Stopped'
     ENDIF
     END SUBROUTINE HANDLE_ERR
!
!
!     #############################################
      SUBROUTINE ARRAY_1D_TO_2D (KN1,P1,KL1,KL2,P2)
!     #############################################
!
!       Small routine used to store a linear array into a 2 dimension array
!
USE MODE_MSG
IMPLICIT NONE
INTEGER,                INTENT(IN)  :: KN1
REAL,DIMENSION(KN1),    INTENT(IN)  :: P1
INTEGER,                INTENT(IN)  :: KL1
INTEGER,                INTENT(IN)  :: KL2
REAL,DIMENSION(KL1,KL2),INTENT(OUT) :: P2
INTEGER                 :: JLOOP1_A1T2
INTEGER                 :: JLOOP2_A1T2
INTEGER                 :: JPOS_A1T2
!
IF (KN1 < KL1*KL2) THEN
  CALL PRINT_MSG(NVERB_FATAL,'GEN','ARRAY_1D_TO_2D','sizes do not match')
END IF
JPOS_A1T2 = 1
DO JLOOP2_A1T2 = 1, KL2
  DO JLOOP1_A1T2 = 1, KL1
    P2(JLOOP1_A1T2,JLOOP2_A1T2) = P1(JPOS_A1T2)
    JPOS_A1T2 = JPOS_A1T2 + 1
  END DO
END DO
END SUBROUTINE ARRAY_1D_TO_2D
!
END SUBROUTINE READ_LIMA_DATA_NETCDF_CASE
