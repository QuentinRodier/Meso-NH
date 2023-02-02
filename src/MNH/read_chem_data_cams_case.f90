!iMNH_LIC Copyright 2012-2023 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!     ####################################
      MODULE MODI_READ_CHEM_DATA_CAMS_CASE
!     ####################################
INTERFACE
SUBROUTINE READ_CHEM_DATA_CAMS_CASE(TPPRE_REAL1,HFILE,TPPGDFILE,PTIME_HORI, &
                                    KVERB,ODUMMY_REAL,OUSECHEM              )
!
USE MODD_IO, ONLY: TFILEDATA
!
TYPE(TFILEDATA),POINTER,INTENT(IN) :: TPPRE_REAL1 ! PRE_REAL1 file
CHARACTER(LEN=28),  INTENT(IN)    :: HFILE      ! name of the NETCDF file
TYPE(TFILEDATA),    INTENT(IN)    :: TPPGDFILE  ! physiographic data file
REAL,               INTENT(INOUT) :: PTIME_HORI ! time spent in hor. interpolations
INTEGER,            INTENT(IN)    :: KVERB      ! verbosity level
LOGICAL,            INTENT(IN)    :: ODUMMY_REAL! flag to interpolate dummy fields
LOGICAL,            INTENT(IN)    :: OUSECHEM   ! flag to initialize chemistry
END SUBROUTINE READ_CHEM_DATA_CAMS_CASE
!
END INTERFACE
END MODULE MODI_READ_CHEM_DATA_CAMS_CASE
!     #############################################################################
      SUBROUTINE READ_CHEM_DATA_CAMS_CASE(TPPRE_REAL1,HFILE,TPPGDFILE,PTIME_HORI, &
                                    KVERB,ODUMMY_REAL,OUSECHEM                    ) 
!     #############################################################################
!
!!****  *READ_CHEM_DATA_CAMS_CASE* - reads data for the initialization of real cases.
!!
!!    PURPOSE
!!    -------
!     This routine reads the two input files :
!       The PGD which is closed after reading
!       The CAMS file
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
!!  2. Read Netcdf fields and transfer CAMS var. in MNH var.
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
!!      A. Berger   20/03/12 adapt whatever the chemical mechanism in BASIC
!!      P. Wautelet 30/10/17 use F90 module for netCDF
!!      J.Pianezzej 13/02/2019 : correction for use of MEGAN
!!      M. Leriche 26/01/2021 : adapt to CAMS reanalysis file
!!      M. Leriche 26/02/2021 : add initialization for dust and sea salt
!!      P. Tulet   01/02/2022 : unit conversion for aerosols (SALTCAMn, AEROCAMn, DUSTCAMn)
!!      M. Leriche 02/02/2022 : compute air density from CAMS
!  P. Wautelet 02/02/2023: correct support of 64bit integers (MNH_INT=8)
!-------------------------------------------------------------------------------
!
!*       0. DECLARATIONS
!           ------------
!
USE MODD_BLANK_n
USE MODD_CH_AEROSOL, ONLY: CORGANIC, NCARB, NSOA, NSP, LORILAM,&
                           JPMODE, LVARSIGI, LVARSIGJ,CAERONAMES,LAERINIT
USE MODD_CH_M9_n,    ONLY: NEQ ,  CNAMES
USE MODD_CH_MNHC_n,  ONLY: LUSECHEM,LUSECHAQ,LUSECHIC,LCH_PH
USE MODD_DUST, ONLY : LDUST, LDSTCAMS
USE MODD_SALT, ONLY : LSALT, LSLTCAMS
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
USE MODD_PREP_REAL
USE MODD_TIME
USE MODD_TIME_n
!
!UPG*PT
!USE MODE_FM
!USE MODE_IO
USE MODE_TOOLS, ONLY: UPCASE
USE MODE_TOOLS_ll
USE MODE_IO_FILE,    only: IO_File_close
!UPG*PT
USE MODE_MPPDB
USE MODE_THERMO
USE MODE_TIME
!
USE MODI_CH_AER_INIT_SOA
USE MODI_CH_INIT_SCHEME_n
USE MODI_CH_OPEN_INPUT  
USE MODI_DUSTCAMS_n
USE MODI_HORIBL
USE MODI_INI_NSV
USE MODI_READ_HGRID_n
USE MODI_READ_VER_GRID
USE MODI_SALTCAMS_n
USE MODI_XYTOLATLON
USE MODI_AEROCAMS_n
!
USE NETCDF
!
IMPLICIT NONE
!
!*       0.1. Declaration of arguments
!             ------------------------
!
TYPE(TFILEDATA),POINTER,INTENT(IN) :: TPPRE_REAL1 ! PRE_REAL1 file
CHARACTER(LEN=28),  INTENT(IN)    :: HFILE      ! name of the NETCDF file
TYPE(TFILEDATA),    INTENT(IN)    :: TPPGDFILE  ! physiographic data file
REAL,               INTENT(INOUT) :: PTIME_HORI ! time spent in hor. interpolations
INTEGER,            INTENT(IN)    :: KVERB      ! verbosity level
LOGICAL,            INTENT(IN)    :: ODUMMY_REAL! flag to interpolate dummy fields
LOGICAL,            INTENT(IN)    :: OUSECHEM   ! flag to initialize chemistry
!
!* 0.2 Declaration of local variables
!      ------------------------------
! General purpose variables
INTEGER                            :: ILUOUT0       ! Unit used for output msg.
INTEGER                            :: IRET          ! Return code from subroutines
INTEGER                            :: JI,JJ,JK      ! Dummy counters
INTEGER                            :: JLOOP1        !  
INTEGER                            :: JN            ! conter of dust/SS modes
INTEGER                            :: JNCHEM, JNAER ! conters of chemical species in BASIC
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
REAL,DIMENSION(:),ALLOCATABLE      :: ZVALUE1D      ! Intermediate array
REAL,DIMENSION(:,:),ALLOCATABLE    :: ZOUT          ! Intermediate arrays
REAL,DIMENSION(:),ALLOCATABLE      :: ZOUT1D        ! Intermediate arrays
INTEGER(kind=CDFINT)               :: ind_netcdf    ! Indice for netcdf var.
!chemistry field infile CAM1.nam
INTEGER                                       :: ICHANNEL
CHARACTER(LEN=8)                              :: YCAM="CAM1.nam"
integer                                       :: ICAM
CHARACTER(LEN=100)                            :: YFORMAT
CHARACTER(LEN=40), DIMENSION(:), ALLOCATABLE  :: YSPCMNH
integer, dimension(:), ALLOCATABLE            :: ISPCCAM
CHARACTER(LEN=9)                              :: YA
REAL,DIMENSION(:,:),ALLOCATABLE               :: ZCOEFCAMSEU
REAL,DIMENSION(:,:),ALLOCATABLE               :: ZMASMOLCAMSEU
CHARACTER(LEN=18),dimension(:,:),ALLOCATABLE  :: YSPCCAMSEU
type TZCAM
real                                          :: ZCOEFCAM, ZMASMOLCAM
character(16)                                 :: YSPCCAM
end type TZCAM
type(TZCAM), DIMENSION(:,:),ALLOCATABLE       :: TZSTOC
! model indice
INTEGER                           :: IMI
TYPE(TFILEDATA),POINTER                       :: TZFILE
! for dust and sea salt
REAL, DIMENSION(:,:,:,:), ALLOCATABLE  :: ZMASS1, ZMASS2
!
! For netcdf 
!
integer(kind=CDFINT) :: status, ncid, varid
integer(kind=CDFINT) :: lat_varid, lon_varid, lev_varid 
integer(kind=CDFINT) :: t_varid, q_varid, ps_varid 
integer(kind=CDFINT) :: recid, latid, lonid, levid
integer(kind=CDFINT) :: latlen, lonlen, levlen
integer :: KILEN
integer(kind=CDFINT) :: mmr_dust1_varid, mmr_dust2_varid, mmr_dust3_varid ! for init. dust
integer(kind=CDFINT) :: mmr_seasalt1_varid, mmr_seasalt2_varid, mmr_seasalt3_varid ! for init sea salt
CHARACTER(LEN=40)                     :: recname
REAL, DIMENSION(:), ALLOCATABLE       :: lats
REAL, DIMENSION(:), ALLOCATABLE       :: lons 
REAL, DIMENSION(:), ALLOCATABLE       :: levs 
INTEGER(kind=CDFINT), DIMENSION(:), ALLOCATABLE :: count3d, start3d
INTEGER(kind=CDFINT), DIMENSION(:), ALLOCATABLE :: count2d, start2d 
INTEGER, DIMENSION(:), ALLOCATABLE    :: kinlo 
REAL, DIMENSION(:,:,:), ALLOCATABLE   :: vartemp3d,vartemp3dbis,vartemp3dter 
REAL, DIMENSION(:,:,:), ALLOCATABLE   :: vartemp3dquater 
REAL, DIMENSION(:,:,:), ALLOCATABLE   :: ZCHEMCAM, ZTCAM, ZQCAM, ZPRESSCAM
REAL, DIMENSION(:,:), ALLOCATABLE     :: ZPSCAM 
REAL, DIMENSION(:,:,:), ALLOCATABLE   :: mmr_dust1, mmr_dust2, mmr_dust3
REAL, DIMENSION(:,:,:), ALLOCATABLE   :: mmr_seasalt1, mmr_seasalt2, mmr_seasalt3
REAL :: scale, offset
! for reverse altitude
REAL, DIMENSION(:), ALLOCATABLE       :: TMP1, TMP2
REAL, DIMENSION(:,:,:), ALLOCATABLE   :: TMP3
REAL, DIMENSION(:,:,:,:), ALLOCATABLE :: TMP4,TMP5
REAL, DIMENSION(:,:,:), ALLOCATABLE   :: ZPRESS_SV_LS, ZRHO_SV_LS
!
!----------------------------------------------------------------------
TZFILE => NULL()
!
IMI = GET_CURRENT_MODEL_INDEX()
!
!*       1. READ PGD FILE
!           -------------
!
ILUOUT0 = TLUOUT0%NLU
CALL READ_HGRID_n(TPPGDFILE,YPGD_NAME,YPGD_DAD_NAME,YPGD_TYPE)
!
!*       1.1 Domain restriction
!
CALL GET_DIM_EXT_ll('B',IIU,IJU)
INO = IIU * IJU
!
!*       1.2 Coordinate conversion to lat,lon system
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
DEALLOCATE (ZLONM)
DEALLOCATE (ZLATM)
!
!
!*       2. READ NETCDF FIELDS
!           ------------------
!
!*       2.1 Open netcdf files
!
status = nf90_open(HFILE, nf90_nowrite, ncid) 
if (status /= nf90_noerr) call handle_err(status)
!
!*       2.2 Read netcdf files
!
! get dimension IDs
!
!* get dimension ID of unlimited variable in netcdf file
!status = nf90_inquire(ncid, unlimitedDimId = recid)
!if (status /= nf90_noerr) call handle_err(status)
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
!status = nf90_inquire_dimension(ncid, recid, name=recname, len=nrecs)
!if (status /= nf90_noerr) call handle_err(status)
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
status = nf90_inq_varid(ncid, "t", t_varid)
if (status /= nf90_noerr) call handle_err(status)
status = nf90_inq_varid(ncid, "q", q_varid)
if (status /= nf90_noerr) call handle_err(status)
status = nf90_inq_varid(ncid, "sp", ps_varid)
if (status /= nf90_noerr) call handle_err(status)
IF (LDUST .AND. LDSTCAMS) THEN
  status = nf90_inq_varid(ncid, "aermr04", mmr_dust1_varid)
  if (status /= nf90_noerr) call handle_err(status)
  status = nf90_inq_varid(ncid, "aermr05", mmr_dust2_varid)
  if (status /= nf90_noerr) call handle_err(status)
  status = nf90_inq_varid(ncid, "aermr06", mmr_dust3_varid)
  if (status /= nf90_noerr) call handle_err(status)
ENDIF
IF (LSALT .AND. LSLTCAMS) THEN
  status = nf90_inq_varid(ncid, "aermr01", mmr_seasalt1_varid)
  if (status /= nf90_noerr) call handle_err(status)
  status = nf90_inq_varid(ncid, "aermr02", mmr_seasalt2_varid)
  if (status /= nf90_noerr) call handle_err(status)
  status = nf90_inq_varid(ncid, "aermr03", mmr_seasalt3_varid)
  if (status /= nf90_noerr) call handle_err(status)
ENDIF  

!
KILEN = latlen * lonlen
!
!*       2.3 Read data.
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
IF (OUSECHEM) THEN ! chem and possibly orilam
  ALLOCATE (vartemp3d(lonlen,latlen,levlen))
  ALLOCATE (vartemp3dbis(lonlen,latlen,levlen))
  ALLOCATE (vartemp3dter(lonlen,latlen,levlen))
  ALLOCATE (vartemp3dquater(lonlen,latlen,levlen))
  ALLOCATE (ZCHEMCAM(lonlen,latlen,levlen))
ENDIF
IF (LDUST .AND. LDSTCAMS) THEN
  ALLOCATE (mmr_dust1(lonlen,latlen,levlen))
  ALLOCATE (mmr_dust2(lonlen,latlen,levlen))
  ALLOCATE (mmr_dust3(lonlen,latlen,levlen))
ENDIF  
IF (LSALT .AND. LSLTCAMS) THEN
  ALLOCATE (mmr_seasalt1(lonlen,latlen,levlen))
  ALLOCATE (mmr_seasalt2(lonlen,latlen,levlen))
  ALLOCATE (mmr_seasalt3(lonlen,latlen,levlen))
ENDIF
ALLOCATE (ZTCAM(lonlen,latlen,levlen))
ALLOCATE (ZQCAM(lonlen,latlen,levlen))
ALLOCATE (ZPSCAM(lonlen,latlen))
ALLOCATE (ZPRESSCAM(lonlen,latlen,levlen))
ALLOCATE (XA_SV_LS(levlen))
ALLOCATE (XB_SV_LS(levlen))
ALLOCATE (XT_SV_LS(IIU,IJU,levlen))
ALLOCATE (XQ_SV_LS(IIU,IJU,levlen,1))
ALLOCATE (XPS_SV_LS(IIU,IJU))
ALLOCATE (XZS_SV_LS(IIU,IJU))
ALLOCATE (ZPRESS_SV_LS(IIU,IJU,levlen))
ALLOCATE (ZRHO_SV_LS(IIU,IJU,levlen))
! take the orography from ECMWF
XZS_SV_LS(:,:) = XZS_LS(:,:)
!
! get values of variables
!
status = nf90_get_var(ncid, lat_varid, lats(:))
if (status /= nf90_noerr) call handle_err(status)
status = nf90_get_var(ncid, lon_varid, lons(:))
if (status /= nf90_noerr) call handle_err(status)
status = nf90_get_var(ncid, lev_varid, levs(:))
if (status /= nf90_noerr) call handle_err(status)
!
!
! Reference pressure (needed for the vertical interpolation)
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

XB_SV_LS(:) = (/ 0.00000000, 0.00000000, 0.00000000, 0.00000000, 0.00000000, &
                 0.00000000, 0.00000000, 0.00000000, 0.00000000, 0.00000000, &
                 0.00000000, 0.00000000, 0.00000000, 0.00000000, 0.00000000, &
                 0.00000000, 0.00000000, 0.00000000, 0.00000000, 0.00000000, &
                 0.00000000, 0.00000000, 0.00000000, 0.00007600, 0.00046100, &
                 0.00181500, 0.00508100, 0.01114300, 0.02067800, 0.03412100, &
                 0.05169000, 0.07353400, 0.09967500, 0.13002300, 0.16438400, &
                 0.20247600, 0.24393300, 0.28832300, 0.33515500, 0.38389200, &
                 0.43396300, 0.48477200, 0.53571000, 0.58616800, 0.63554700, &
                 0.68326900, 0.72878600, 0.77159700, 0.81125300, 0.84737500, &
                 0.87965700, 0.90788400, 0.93194000, 0.95182200, 0.96764500, &
                 0.97966300, 0.98827000, 0.99401900, 0.99763000, 1.00000000  /)
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
!     Read 1 record of lon*lat*lev values, starting at the
!     beginning of the record (the (1, 1, 1, rec) element in the netCDF
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
!
ALLOCATE(ZVALUE(levlen,KILEN))
ALLOCATE(ZOUT(levlen,INO))
ALLOCATE(ZVALUE1D(KILEN))
ALLOCATE(ZOUT1D(INO))
!
!*      2.3.1 read meteo veriables
!             temperature, spec. hum. and surface pressure
!             needed for the vertical interpolation
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
!
DO JK = 1, levlen
  IF (JK.EQ.1) THEN
    ZPRESSCAM(:,:,JK) = (XA_SV_LS(JK) + XB_SV_LS(JK)*ZPSCAM(:,:)) ! ZPRESCAM = 0. for n=0
  ELSE
    ZPRESSCAM(:,:,JK) = ( XA_SV_LS(JK) + XA_SV_LS(JK-1) +      &
                        ( XB_SV_LS(JK) + XB_SV_LS(JK-1))*ZPSCAM(:,:)) / 2.
  ENDIF
END DO

!
where (ZLONOUT(:) < 0.) ZLONOUT(:) = ZLONOUT(:) + 360.  ! correct longitudes
!        
!*       2.3.2 meteo. variables horizontal interpolation
!
DO JK = 1, levlen
  JLOOP1 = 0
  DO JJ = 1, latlen
    ZVALUE(JK,JLOOP1+1:JLOOP1+lonlen) = ZTCAM(1:lonlen,JJ,JK)
    JLOOP1 = JLOOP1 + lonlen
  ENDDO
  CALL HORIBL(lats(1),lons(1),lats(latlen),lons(lonlen), &
              int(latlen,kind=kind(1)),kinlo,KILEN,      &
              ZVALUE(JK,:),INO,ZLONOUT,ZLATOUT,          &
              ZOUT(JK,:),.FALSE.,PTIME_HORI,.FALSE.)
!
  CALL ARRAY_1D_TO_2D(INO,ZOUT(JK,:),IIU,IJU, &
                      XT_SV_LS(:,:,JK))
ENDDO 
!
DO JK = 1, levlen
  JLOOP1 = 0
  DO JJ = 1, latlen
    ZVALUE(JK,JLOOP1+1:JLOOP1+lonlen) = ZQCAM(1:lonlen,JJ,JK)
    JLOOP1 = JLOOP1 + lonlen
  ENDDO
  CALL HORIBL(lats(1),lons(1),lats(latlen),lons(lonlen), &
              int(latlen,kind=kind(1)),kinlo,KILEN,      &
              ZVALUE(JK,:),INO,ZLONOUT,ZLATOUT,          &
              ZOUT(JK,:),.FALSE.,PTIME_HORI,.FALSE.)
!
   CALL ARRAY_1D_TO_2D(INO,ZOUT(JK,:),IIU,IJU,                    &
                       XQ_SV_LS(:,:,JK,1))
ENDDO 
!
DO JK = 1, levlen
  JLOOP1 = 0
  DO JJ = 1, latlen
    ZVALUE(JK,JLOOP1+1:JLOOP1+lonlen) = ZPRESSCAM(1:lonlen,JJ,JK)
    JLOOP1 = JLOOP1 + lonlen
  ENDDO
  CALL HORIBL(lats(1),lons(1),lats(latlen),lons(lonlen), &
              int(latlen,kind=kind(1)),kinlo,KILEN,      &
              ZVALUE(JK,:),INO,ZLONOUT,ZLATOUT,          &
              ZOUT(JK,:),.FALSE.,PTIME_HORI,.FALSE.)
!
   CALL ARRAY_1D_TO_2D(INO,ZOUT(JK,:),IIU,IJU,                    &
                       ZPRESS_SV_LS(:,:,JK))
ENDDO 
!
JLOOP1 = 0
DO JJ = 1, latlen
  ZVALUE1D(JLOOP1+1:JLOOP1+lonlen) = ZPSCAM(1:lonlen,JJ)
  JLOOP1 = JLOOP1 + lonlen
ENDDO
CALL HORIBL(lats(1),lons(1),lats(latlen),lons(lonlen), &
            int(latlen,kind=kind(1)),kinlo,KILEN,      &
            ZVALUE1D(:),INO,ZLONOUT,ZLATOUT,           &
            ZOUT1D(:),.FALSE.,PTIME_HORI,.FALSE.)
!
CALL ARRAY_1D_TO_2D(INO,ZOUT1D(:),IIU,IJU,                    &
                    XPS_SV_LS(:,:))
!
! air density in kg/m3 RHO=PM/RT
ZRHO_SV_LS(:,:,:) = (ZPRESS_SV_LS(:,:,:))/(XRD*XT_SV_LS(:,:,:))

!
!*       2.3.3 correct negative values produced by the horizontal interpolations
!
XPS_SV_LS(:,:)  = MAX(XPS_SV_LS(:,:),0.)
XT_SV_LS(:,:,:) = MAX(XT_SV_LS(:,:,:),0.)
XQ_SV_LS(:,:,:,1) = MAX(XQ_SV_LS(:,:,:,1),0.)
ZRHO_SV_LS(:,:,:) = MAX(ZRHO_SV_LS(:,:,:),0.)
!
!
!*       2.4 initialize NSV variables
!
! Always initialize chemical scheme variables before INI_NSV call !
CALL CH_INIT_SCHEME_n(IMI,LUSECHAQ,LUSECHIC,LCH_PH,ILUOUT0,KVERB)
IF (LORILAM) THEN
  CORGANIC = "MPMPO"
  LVARSIGI = .TRUE.
  LVARSIGJ = .TRUE.
  CALL CH_AER_INIT_SOA(ILUOUT0, KVERB)
END IF
IF (OUSECHEM) LUSECHEM = .TRUE.
! initialise NSV_* variables
CALL INI_NSV(1)
IF (ALLOCATED(XSV_LS)) DEALLOCATE(XSV_LS)
ALLOCATE (XSV_LS(IIU,IJU,levlen,NSV))
XSV_LS(:,:,:,:) = 0.
!
!*       2.5 read chem. variables and convert them into MNH variables
!
IF (OUSECHEM) THEN
  WRITE (ILUOUT0,'(A,A4,A)') ' | Reading CAMS species (ppp) from ',HFILE,'file'
!
! read CAMS species from the file CAM1.nam
!
! open input file
  CALL CH_OPEN_INPUT(YCAM,"CAM2MESONH",TZFILE,ILUOUT0,KVERB)
  ICHANNEL = TZFILE%NLU
!
!read number of cams species to transfer into mesonh
  READ(ICHANNEL, *) ICAM
  IF (KVERB >= 5) WRITE (ILUOUT0,*) "number of cams species to transfer into &
  & mesonh : ", ICAM
!
!read data input format
  READ(ICHANNEL,"(A)") YFORMAT
  YFORMAT=UPCASE(YFORMAT)
  IF (KVERB >= 5) WRITE (ILUOUT0,*) "input format is: ", YFORMAT
!
!allocate fields
  ALLOCATE(YSPCMNH(ICAM))      !MESONH species
  ALLOCATE(TZSTOC(ICAM,4))     !CAMS coefficient and CAMS species associated
  ALLOCATE(ISPCCAM(ICAM))      !number of CAMS species into each MESONH species
  ALLOCATE(ZCOEFCAMSEU(ICAM,4))!Coef stoich of each CAMS species
  ALLOCATE(ZMASMOLCAMSEU(ICAM,4))!molar mass of  each CAMS species
  ALLOCATE(YSPCCAMSEU(ICAM,4))    !CAMS species name
!read MESONH variable names and CAMS variable names associated 
  DO JI = 1,ICAM               !for every MNH species existing in CAM1.nam                              
    READ(ICHANNEL,YFORMAT) YSPCMNH(JI), ISPCCAM(JI),           & !reading line by line
                 TZSTOC(JI,1)%ZCOEFCAM, TZSTOC(JI,1)%YSPCCAM, TZSTOC(JI,1)%ZMASMOLCAM, &
                 TZSTOC(JI,2)%ZCOEFCAM, TZSTOC(JI,2)%YSPCCAM, TZSTOC(JI,2)%ZMASMOLCAM, &
                 TZSTOC(JI,3)%ZCOEFCAM, TZSTOC(JI,3)%YSPCCAM, TZSTOC(JI,3)%ZMASMOLCAM, &
                 TZSTOC(JI,4)%ZCOEFCAM, TZSTOC(JI,4)%YSPCCAM, TZSTOC(JI,4)%ZMASMOLCAM
    WRITE(ILUOUT0,YFORMAT) YSPCMNH(JI), ISPCCAM(JI),&
!writing in arrays
                TZSTOC(JI,1)%ZCOEFCAM, TZSTOC(JI,1)%YSPCCAM, TZSTOC(JI,1)%ZMASMOLCAM, &
                TZSTOC(JI,2)%ZCOEFCAM, TZSTOC(JI,2)%YSPCCAM, TZSTOC(JI,2)%ZMASMOLCAM, &
                TZSTOC(JI,3)%ZCOEFCAM, TZSTOC(JI,3)%YSPCCAM, TZSTOC(JI,3)%ZMASMOLCAM, &
                TZSTOC(JI,4)%ZCOEFCAM, TZSTOC(JI,4)%YSPCCAM, TZSTOC(JI,4)%ZMASMOLCAM
!
    ZCOEFCAMSEU(JI,1) =  (TZSTOC(JI,1)%ZCOEFCAM) !coef stoich of each CAMS species set into an array 
    ZCOEFCAMSEU(JI,2) =  (TZSTOC(JI,2)%ZCOEFCAM) 
    ZCOEFCAMSEU(JI,3) =  (TZSTOC(JI,3)%ZCOEFCAM)
    ZCOEFCAMSEU(JI,4) =  (TZSTOC(JI,4)%ZCOEFCAM)
! 
    YSPCCAMSEU(JI,1)=trim(TZSTOC(JI,1)%YSPCCAM) !specie name of each CAMS specie set into an array
    YSPCCAMSEU(JI,2)=trim(TZSTOC(JI,2)%YSPCCAM) 
    YSPCCAMSEU(JI,3)=trim(TZSTOC(JI,3)%YSPCCAM)
    YSPCCAMSEU(JI,4)=trim(TZSTOC(JI,4)%YSPCCAM)
!
    ZMASMOLCAMSEU(JI,1)= (TZSTOC(JI,1)%ZMASMOLCAM) ! molar mass to convert kg/kg to ppp
    ZMASMOLCAMSEU(JI,2)= (TZSTOC(JI,2)%ZMASMOLCAM)
    ZMASMOLCAMSEU(JI,3)= (TZSTOC(JI,3)%ZMASMOLCAM)
    ZMASMOLCAMSEU(JI,4)= (TZSTOC(JI,4)%ZMASMOLCAM)
!
! read chem. variables and exchange CAMS values onto prognostic variables XSV_LS
! convert CAMS fields to 2D for use in horizontal interpolation routine HORIBL.f90
!
    DO JNCHEM = NSV_CHEMBEG, NSV_CHEMEND  !loop on all MNH species
      IF (trim(CNAMES(JNCHEM-NSV_CHEMBEG+1))==trim(YSPCMNH(JI))) THEN !MNH mechanism species
        IF (ISPCCAM(JI)==1) THEN
          status = nf90_inq_varid(ncid, trim(YSPCCAMSEU(JI,1)), ind_netcdf)
          if (status /= nf90_noerr) call handle_err(status)
          status = nf90_get_var(ncid, ind_netcdf, vartemp3d, start=start3d, count=count3d)
          if (status /= nf90_noerr) call  handle_err(status)
          status = nf90_get_att(ncid, ind_netcdf, "scale_factor", scale) 
          status = nf90_get_att(ncid, ind_netcdf, "add_offset", offset)
          vartemp3d(:,:,:)=offset + scale * vartemp3d(:,:,:)
          ZCHEMCAM(:,:,:)=ZCOEFCAMSEU(JI,1)*vartemp3d(:,:,:)*XMD*1E3/ZMASMOLCAMSEU(JI,1)
        ELSE IF (ISPCCAM(JI)==2) THEN
          status = nf90_inq_varid(ncid, trim(YSPCCAMSEU(JI,1)), ind_netcdf)
          if (status /= nf90_noerr) call handle_err(status)
          status = nf90_get_var(ncid, ind_netcdf, vartemp3d, start=start3d, count=count3d)
          if (status /= nf90_noerr) call  handle_err(status)
          status = nf90_get_att(ncid, ind_netcdf, "scale_factor", scale) 
          status = nf90_get_att(ncid, ind_netcdf, "add_offset", offset)
          vartemp3d(:,:,:)=offset + scale*vartemp3d(:,:,:)
          status = nf90_inq_varid(ncid, trim(YSPCCAMSEU(JI,2)), ind_netcdf)
          if (status /= nf90_noerr) call handle_err(status)
          status = nf90_get_var(ncid, ind_netcdf, vartemp3dbis, start=start3d, count=count3d)
          if (status /= nf90_noerr) call  handle_err(status)
          status = nf90_get_att(ncid, ind_netcdf, "scale_factor", scale) 
          status = nf90_get_att(ncid, ind_netcdf, "add_offset", offset)
          vartemp3dbis(:,:,:)=offset + scale*vartemp3dbis(:,:,:)
          ZCHEMCAM(:,:,:)=ZCOEFCAMSEU(JI,1)*vartemp3d(:,:,:)*XMD*1E3/ZMASMOLCAMSEU(JI,1) + &
                          ZCOEFCAMSEU(JI,2)*vartemp3dbis(:,:,:)*XMD*1E3/ZMASMOLCAMSEU(JI,2) 
        ELSE IF (ISPCCAM(JI)==3) THEN
          status = nf90_inq_varid(ncid, trim(YSPCCAMSEU(JI,1)), ind_netcdf)
          if (status /= nf90_noerr) call handle_err(status)
          status = nf90_get_var(ncid, ind_netcdf, vartemp3d, start=start3d, count=count3d)
          if (status /= nf90_noerr) call  handle_err(status)
          status = nf90_get_att(ncid, ind_netcdf, "scale_factor", scale) 
          status = nf90_get_att(ncid, ind_netcdf, "add_offset", offset)
          vartemp3d(:,:,:)=offset + scale*vartemp3d(:,:,:)
          status = nf90_inq_varid(ncid, trim(YSPCCAMSEU(JI,2)), ind_netcdf)
          if (status /= nf90_noerr) call handle_err(status)
          status = nf90_get_var(ncid, ind_netcdf, vartemp3dbis, start=start3d, count=count3d)
          if (status /= nf90_noerr) call  handle_err(status)
          status = nf90_get_att(ncid, ind_netcdf, "scale_factor", scale) 
          status = nf90_get_att(ncid, ind_netcdf, "add_offset", offset)
          vartemp3dbis(:,:,:)=offset + scale*vartemp3dbis(:,:,:)
          status = nf90_inq_varid(ncid, trim(YSPCCAMSEU(JI,3)), ind_netcdf)
          if (status /= nf90_noerr) call handle_err(status)
          status = nf90_get_var(ncid, ind_netcdf, vartemp3dter, start=start3d, count=count3d)
          if (status /= nf90_noerr) call  handle_err(status)
          status = nf90_get_att(ncid, ind_netcdf, "scale_factor", scale) 
          status = nf90_get_att(ncid, ind_netcdf, "add_offset", offset)
          vartemp3dter(:,:,:)=offset + scale*vartemp3dter(:,:,:)
          ZCHEMCAM(:,:,:)=ZCOEFCAMSEU(JI,1)*vartemp3d(:,:,:)*XMD*1E3/ZMASMOLCAMSEU(JI,1) +&
                          ZCOEFCAMSEU(JI,2)*vartemp3dbis(:,:,:)*XMD*1E3/ZMASMOLCAMSEU(JI,2) +&
                          ZCOEFCAMSEU(JI,3)*vartemp3dter(:,:,:)*XMD*1E3/ZMASMOLCAMSEU(JI,3) 
        ELSE IF (ISPCCAM(JI)==4) THEN
          status = nf90_inq_varid(ncid, trim(YSPCCAMSEU(JI,1)), ind_netcdf)
          if (status /= nf90_noerr) call handle_err(status)
          status = nf90_get_var(ncid, ind_netcdf, vartemp3d, start=start3d, count=count3d)
          if (status /= nf90_noerr) call  handle_err(status)
          status = nf90_get_att(ncid, ind_netcdf, "scale_factor", scale) 
          status = nf90_get_att(ncid, ind_netcdf, "add_offset", offset)
          vartemp3d(:,:,:)=offset + scale*vartemp3d(:,:,:)
          status = nf90_inq_varid(ncid, trim(YSPCCAMSEU(JI,2)), ind_netcdf)
          if (status /= nf90_noerr) call handle_err(status)
          status = nf90_get_var(ncid, ind_netcdf, vartemp3dbis, start=start3d, count=count3d)
          if (status /= nf90_noerr) call  handle_err(status)
          status = nf90_get_att(ncid, ind_netcdf, "scale_factor", scale) 
          status = nf90_get_att(ncid, ind_netcdf, "add_offset", offset)
          vartemp3dbis(:,:,:)=offset + scale*vartemp3dbis(:,:,:)
          status = nf90_inq_varid(ncid, trim(YSPCCAMSEU(JI,3)), ind_netcdf)
          if (status /= nf90_noerr) call handle_err(status)
          status = nf90_get_var(ncid, ind_netcdf, vartemp3dter, start=start3d, count=count3d)
          if (status /= nf90_noerr) call  handle_err(status)
          status = nf90_get_att(ncid, ind_netcdf, "scale_factor", scale) 
          status = nf90_get_att(ncid, ind_netcdf, "add_offset", offset)
          vartemp3dter(:,:,:)=offset + scale*vartemp3dter(:,:,:)
          status = nf90_inq_varid(ncid, trim(YSPCCAMSEU(JI,4)), ind_netcdf)
          if (status /= nf90_noerr) call handle_err(status)
          status = nf90_get_var(ncid, ind_netcdf, vartemp3dquater, start=start3d, count=count3d)
          if (status /= nf90_noerr) call  handle_err(status)
          status = nf90_get_att(ncid, ind_netcdf, "scale_factor", scale) 
          status = nf90_get_att(ncid, ind_netcdf, "add_offset", offset)
          vartemp3dquater(:,:,:)=offset + scale*vartemp3dquater(:,:,:)
          ZCHEMCAM(:,:,:)=ZCOEFCAMSEU(JI,1)*vartemp3d(:,:,:)*XMD*1E3/ZMASMOLCAMSEU(JI,1)+&
                          ZCOEFCAMSEU(JI,2)*vartemp3dbis(:,:,:)*XMD*1E3/ZMASMOLCAMSEU(JI,2)+&
                          ZCOEFCAMSEU(JI,3)*vartemp3dter(:,:,:)*XMD*1E3/ZMASMOLCAMSEU(JI,3)+&
                          ZCOEFCAMSEU(JI,4)*vartemp3dquater(:,:,:)*XMD*1E3/ZMASMOLCAMSEU(JI,4)
        ENDIF
        DO JK = 1, levlen
          JLOOP1 = 0
          DO JJ = 1, latlen
            ZVALUE(JK,JLOOP1+1:JLOOP1+lonlen) = ZCHEMCAM(1:lonlen,JJ,JK)
            JLOOP1 = JLOOP1+lonlen
          ENDDO                                                                                           
          CALL HORIBL(lats(1),lons(1),lats(latlen),lons(lonlen), &
                      int(latlen,kind=kind(1)),kinlo,KILEN,      &
                      ZVALUE(JK,:),INO,ZLONOUT,ZLATOUT,          &
                      ZOUT(JK,:),.FALSE.,PTIME_HORI,.TRUE.)
          CALL ARRAY_1D_TO_2D(INO,ZOUT(JK,:),IIU,IJU, &
                              XSV_LS(:,:,JK,JNCHEM)   )
        ENDDO  ! levlen
      ENDIF      
    XSV_LS(:,:,:,JNCHEM) = MAX(XSV_LS(:,:,:,JNCHEM), 0.)
    ENDDO ! JNCHEM
!  
    DO JNAER = NSV_AERBEG, NSV_AEREND ! no need to convert to ppp
      IF (trim(CAERONAMES(JNAER-NSV_AERBEG+1))==trim(YSPCMNH(JI))) THEN !MNH mechanism species

        IF (ISPCCAM(JI)==1) THEN
          status = nf90_inq_varid(ncid, trim(YSPCCAMSEU(JI,1)), ind_netcdf)            
          if (status /= nf90_noerr) call handle_err(status)
          status = nf90_get_var(ncid, ind_netcdf, vartemp3d, start=start3d, count=count3d)
          if (status /= nf90_noerr) call  handle_err(status)
          status = nf90_get_att(ncid, ind_netcdf, "scale_factor", scale) 
          status = nf90_get_att(ncid, ind_netcdf, "add_offset", offset)
          ZCHEMCAM(:,:,:)=ZCOEFCAMSEU(JI,1)*(offset + scale*vartemp3d(:,:,:))
        ELSE IF (ISPCCAM(JI)==2) THEN
          status = nf90_inq_varid(ncid, trim(YSPCCAMSEU(JI,1)), ind_netcdf)
          if (status /= nf90_noerr) call handle_err(status)
          status = nf90_get_var(ncid, ind_netcdf, vartemp3d, start=start3d, count=count3d)
          if (status /= nf90_noerr) call  handle_err(status)
          status = nf90_get_att(ncid, ind_netcdf, "scale_factor", scale) 
          status = nf90_get_att(ncid, ind_netcdf, "add_offset", offset)
          vartemp3d(:,:,:)=offset + scale*vartemp3d(:,:,:)
          status = nf90_inq_varid(ncid, trim(YSPCCAMSEU(JI,2)), ind_netcdf)
          if (status /= nf90_noerr) call handle_err(status)
          status = nf90_get_var(ncid, ind_netcdf, vartemp3dbis, start=start3d, count=count3d)
          if (status /= nf90_noerr) call  handle_err(status)
          status = nf90_get_att(ncid, ind_netcdf, "scale_factor", scale) 
          status = nf90_get_att(ncid, ind_netcdf, "add_offset", offset)
          vartemp3dbis(:,:,:)=offset + scale*vartemp3dbis(:,:,:)
          ZCHEMCAM(:,:,:)=ZCOEFCAMSEU(JI,1)*vartemp3d(:,:,:) + &
                                ZCOEFCAMSEU(JI,2)*vartemp3dbis(:,:,:) 
        ELSE IF (ISPCCAM(JI)==3) THEN
          status = nf90_inq_varid(ncid, trim(YSPCCAMSEU(JI,1)), ind_netcdf)
          if (status /= nf90_noerr) call handle_err(status)
          status = nf90_get_var(ncid, ind_netcdf, vartemp3d, start=start3d, count=count3d)
          if (status /= nf90_noerr) call  handle_err(status)
          status = nf90_get_att(ncid, ind_netcdf, "scale_factor", scale) 
          status = nf90_get_att(ncid, ind_netcdf, "add_offset", offset)
          vartemp3d(:,:,:)=offset + scale*vartemp3d(:,:,:)
          status = nf90_inq_varid(ncid, trim(YSPCCAMSEU(JI,2)), ind_netcdf)
          if (status /= nf90_noerr) call handle_err(status)
          status = nf90_get_var(ncid, ind_netcdf, vartemp3dbis, start=start3d, count=count3d)
          if (status /= nf90_noerr) call  handle_err(status)
          status = nf90_get_att(ncid, ind_netcdf, "scale_factor", scale) 
          status = nf90_get_att(ncid, ind_netcdf, "add_offset", offset)
          vartemp3dbis(:,:,:)=offset + scale*vartemp3dbis(:,:,:)
          status = nf90_inq_varid(ncid, trim(YSPCCAMSEU(JI,3)), ind_netcdf)
          if (status /= nf90_noerr) call handle_err(status)
          status = nf90_get_var(ncid, ind_netcdf, vartemp3dter, start=start3d, count=count3d)
          if (status /= nf90_noerr) call  handle_err(status)
          status = nf90_get_att(ncid, ind_netcdf, "scale_factor", scale) 
          status = nf90_get_att(ncid, ind_netcdf, "add_offset", offset)
          vartemp3dter(:,:,:)=offset + scale*vartemp3dter(:,:,:)
          ZCHEMCAM(:,:,:)=ZCOEFCAMSEU(JI,1)*vartemp3d(:,:,:)+&
                          ZCOEFCAMSEU(JI,2)*vartemp3dbis(:,:,:)+&
                          ZCOEFCAMSEU(JI,3)*vartemp3dter(:,:,:)
        ELSE IF (ISPCCAM(JI)==4) THEN
          status = nf90_inq_varid(ncid, trim(YSPCCAMSEU(JI,1)), ind_netcdf)               
          if (status /= nf90_noerr) call handle_err(status)
          status = nf90_get_var(ncid, ind_netcdf, vartemp3d, start=start3d, count=count3d)
          if (status /= nf90_noerr) call  handle_err(status)
          status = nf90_get_att(ncid, ind_netcdf, "scale_factor", scale) 
          status = nf90_get_att(ncid, ind_netcdf, "add_offset", offset)
          vartemp3d(:,:,:)=offset + scale*vartemp3d(:,:,:)
          status = nf90_inq_varid(ncid, trim(YSPCCAMSEU(JI,2)), ind_netcdf)
          if (status /= nf90_noerr) call handle_err(status)
          status = nf90_get_var(ncid, ind_netcdf, vartemp3dbis, start=start3d, count=count3d)
          if (status /= nf90_noerr) call  handle_err(status)
          status = nf90_get_att(ncid, ind_netcdf, "scale_factor", scale) 
          status = nf90_get_att(ncid, ind_netcdf, "add_offset", offset)
          vartemp3dbis(:,:,:)=offset + scale*vartemp3dbis(:,:,:)
          status = nf90_inq_varid(ncid, trim(YSPCCAMSEU(JI,3)), ind_netcdf)
          if (status /= nf90_noerr) call handle_err(status)
          status = nf90_get_var(ncid, ind_netcdf, vartemp3dter, start=start3d, count=count3d)
          if (status /= nf90_noerr) call  handle_err(status)
          status = nf90_get_att(ncid, ind_netcdf, "scale_factor", scale) 
          status = nf90_get_att(ncid, ind_netcdf, "add_offset", offset)
          vartemp3dter(:,:,:)=offset + scale*vartemp3dter(:,:,:)
          status = nf90_inq_varid(ncid, trim(YSPCCAMSEU(JI,4)), ind_netcdf)
          if (status /= nf90_noerr) call handle_err(status)
          status = nf90_get_var(ncid, ind_netcdf, vartemp3dquater, start=start3d, count=count3d)
          if (status /= nf90_noerr) call  handle_err(status)
          status = nf90_get_att(ncid, ind_netcdf, "scale_factor", scale) 
          status = nf90_get_att(ncid, ind_netcdf, "add_offset", offset)
          vartemp3dquater(:,:,:)=offset + scale*vartemp3dquater(:,:,:)
          ZCHEMCAM(:,:,:)=ZCOEFCAMSEU(JI,1)*vartemp3d(:,:,:)+&
                           ZCOEFCAMSEU(JI,2)*vartemp3dbis(:,:,:)+&
                           ZCOEFCAMSEU(JI,3)*vartemp3dter(:,:,:)+&
                           ZCOEFCAMSEU(JI,4)*vartemp3dquater(:,:,:)
        ENDIF
        DO JK = 1, levlen
          JLOOP1 = 0
          DO JJ = 1, latlen
            ZVALUE(JK,JLOOP1+1:JLOOP1+lonlen) = ZCHEMCAM(1:lonlen,JJ,JK)
            JLOOP1 = JLOOP1+lonlen
          ENDDO                                                                                           
          CALL HORIBL(lats(1),lons(1),lats(latlen),lons(lonlen), &
                      int(latlen,kind=kind(1)),kinlo,KILEN,      &
                      ZVALUE(JK,:),INO,ZLONOUT,ZLATOUT,          &
                      ZOUT(JK,:),.FALSE.,PTIME_HORI,.TRUE.)
          CALL ARRAY_1D_TO_2D(INO,ZOUT(JK,:),IIU,IJU, &
                              XSV_LS(:,:,JK,JNAER)   )
        ENDDO  ! levlen
      ENDIF         
    XSV_LS(:,:,:,JNAER) = MAX(XSV_LS(:,:,:,JNAER), 1E-40)
    ENDDO ! JNAER
  ENDDO  ! ICAM loop on MNH species in CAM1.nam
  DEALLOCATE(YSPCMNH) 
  DEALLOCATE(TZSTOC)
  DEALLOCATE(ISPCCAM) 
  DEALLOCATE(ZCOEFCAMSEU)
  DEALLOCATE(ZMASMOLCAMSEU)
  DEALLOCATE(YSPCCAMSEU)
!
  IF (LORILAM) THEN ! convert kg/kg into ppv and moments
    CALL AEROCAMS_n(XSV_LS(:,:,:,NSV_AERBEG:NSV_AEREND), ZRHO_SV_LS)
    LAERINIT = .FALSE.  ! to avoid enter in the routine ch_reallfin
  ENDIF
ENDIF ! OUSECHEM
!
!*       2.6 read dust variables and convert them into MNH variables
!
IF (LDUST .AND. LDSTCAMS)  THEN
  WRITE (ILUOUT0,'(A)') ' | Reading CAMS dust (kg/kg)'
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
  ALLOCATE (ZMASS1(lonlen,latlen,levlen,3))
  ALLOCATE (ZMASS2(SIZE(XSV_LS,1), SIZE(XSV_LS,2), SIZE(XSV_LS,3),3))
!
  ZMASS1(:,:,:,1) = mmr_dust1(:,:,:)
  ZMASS1(:,:,:,2) = mmr_dust2(:,:,:)
  ZMASS1(:,:,:,3) = mmr_dust3(:,:,:)

  ZMASS1(:,:,:,:) = MAX(ZMASS1(:,:,:,:),1E-40)

  DO JN=1,3
    DO JK = 1, levlen
      JLOOP1 = 0
      DO JJ = 1, latlen
        ZVALUE(JK,JLOOP1+1:JLOOP1+lonlen) = ZMASS1(1:lonlen,JJ,JK,JN)
        JLOOP1 = JLOOP1 + lonlen
      ENDDO
      CALL HORIBL(lats(1),lons(1),lats(latlen),lons(lonlen), &
                  int(latlen,kind=kind(1)),kinlo,KILEN,      &
                  ZVALUE(JK,:),INO,ZLONOUT,ZLATOUT,          &
                  ZOUT(JK,:),.FALSE.,PTIME_HORI,.TRUE. )
      CALL ARRAY_1D_TO_2D(INO,ZOUT(JK,:),IIU,IJU,ZMASS2(:,:,JK,JN))
    ENDDO
  ENDDO
!
  ! conversion kg/kg into moment units (ppv)
  CALL DUSTCAMS_n(XSV_LS(:,:,:,NSV_DSTBEG:NSV_DSTEND), ZMASS2(:,:,:,:), ZRHO_SV_LS(:,:,:))

  DEALLOCATE (ZMASS1)
  DEALLOCATE (ZMASS2)
END IF
!
!*       2.7 read sea salt variables and convert them into MNH variables
!
IF (LSALT .AND. LSLTCAMS)  THEN
  WRITE (ILUOUT0,'(A)') ' | Reading CAMS sea salt (kg/kg)'
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
  ALLOCATE (ZMASS1(lonlen,latlen,levlen,3))
  ALLOCATE (ZMASS2(SIZE(XSV_LS,1), SIZE(XSV_LS,2), SIZE(XSV_LS,3),3))
!
  ZMASS1(:,:,:,1) = mmr_seasalt1(:,:,:)
  ZMASS1(:,:,:,2) = mmr_seasalt2(:,:,:)
  ZMASS1(:,:,:,3) = mmr_seasalt3(:,:,:)
  ZMASS1(:,:,:,:) = MAX(ZMASS1(:,:,:,:),1E-40)
  DO JN=1,3
    DO JK = 1, levlen
      JLOOP1 = 0
      DO JJ = 1, latlen
        ZVALUE(JK,JLOOP1+1:JLOOP1+lonlen) = ZMASS1(1:lonlen,JJ,JK,JN)
        JLOOP1 = JLOOP1 + lonlen
      ENDDO
      CALL HORIBL(lats(1),lons(1),lats(latlen),lons(lonlen), &
                  int(latlen,kind=kind(1)),kinlo,KILEN,      &
                  ZVALUE(JK,:),INO,ZLONOUT,ZLATOUT,          &
                  ZOUT(JK,:),.FALSE.,PTIME_HORI,.TRUE. )
      CALL ARRAY_1D_TO_2D(INO,ZOUT(JK,:),IIU,IJU,ZMASS2(:,:,JK,JN))
    ENDDO
  ENDDO
!
  ! conversion kg/kg into moment units (ppv)
  CALL SALTCAMS_n(XSV_LS(:,:,:,NSV_SLTBEG:NSV_SLTEND),ZMASS2(:,:,:,:), ZRHO_SV_LS(:,:,:))
  !
  DEALLOCATE (ZMASS1)
  DEALLOCATE (ZMASS2)
ENDIF
!
! 
!*       3. If netcdf vertical levels have to be reversed
!
ALLOCATE(TMP1(levlen))
ALLOCATE(TMP2(levlen))
ALLOCATE(TMP3(IIU,IJU,levlen))
ALLOCATE(TMP4(IIU,IJU,levlen,NRR))
ALLOCATE(TMP5(IIU,IJU,levlen,NSV))
!
XA_SV_LS(:) = XA_SV_LS(:) / XP00_SV_LS
!
DO JJ=1,levlen
! inv. lev
  TMP1(JJ)       = XA_SV_LS(levlen+1-JJ)
  TMP2(JJ)       = XB_SV_LS(levlen+1-JJ)
  TMP3(:,:,JJ)   = XT_SV_LS(:,:,levlen+1-JJ)
  TMP4(:,:,JJ,:) = XQ_SV_LS(:,:,levlen+1-JJ,:)
  TMP5(:,:,JJ,:) = XSV_LS(:,:,levlen+1-JJ,:)
ENDDO
!
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
!*       4 close the netcdf file
!
status = nf90_close(ncid) 
if (status /= nf90_noerr) call handle_err(status)
!
DEALLOCATE(ZVALUE)
DEALLOCATE(ZOUT)
IF (ALLOCATED(ZVALUE1D)) DEALLOCATE(ZVALUE1D) 
IF (ALLOCATED(ZOUT1D)) DEALLOCATE(ZOUT1D)
!
! close
! file
IF (OUSECHEM) CALL IO_FILE_CLOSE(TZFILE)
!
!
!-------------------------------------------------------------
!
!*       5. VERTICAL GRID
!           -------------
!
!*       5.1 Read VERTICAL GRID
!
WRITE (ILUOUT0,'(A)') ' | Reading of vertical grid in progress'
CALL READ_VER_GRID(TPPRE_REAL1)
!
!--------------------------------------------------------------
!
!*       6. Free all temporary allocations
!           ------------------------------
!
DEALLOCATE (count3d)
DEALLOCATE (count2d)
DEALLOCATE (start3d)
DEALLOCATE (start2d)
DEALLOCATE (lats)
DEALLOCATE (lons)
DEALLOCATE (levs)
DEALLOCATE (kinlo)
DEALLOCATE (ZLATOUT)
DEALLOCATE (ZLONOUT)
DEALLOCATE (ZTCAM)
DEALLOCATE (ZQCAM)
DEALLOCATE (ZPSCAM)
DEALLOCATE (ZPRESSCAM)
DEALLOCATE (ZPRESS_SV_LS)
DEALLOCATE (ZRHO_SV_LS)
IF (ALLOCATED(ZCHEMCAM)) DEALLOCATE(ZCHEMCAM)
IF (ALLOCATED(vartemp3d)) DEALLOCATE(vartemp3d)
IF (ALLOCATED(vartemp3dbis)) DEALLOCATE(vartemp3dbis)
IF (ALLOCATED(vartemp3dter)) DEALLOCATE(vartemp3dter)
IF (ALLOCATED(vartemp3dquater)) DEALLOCATE(vartemp3dquater)
IF (ALLOCATED(mmr_dust1)) DEALLOCATE(mmr_dust1)
IF (ALLOCATED(mmr_dust2)) DEALLOCATE(mmr_dust2)
IF (ALLOCATED(mmr_dust3)) DEALLOCATE(mmr_dust3)
IF (ALLOCATED(mmr_seasalt1)) DEALLOCATE(mmr_seasalt1)
IF (ALLOCATED(mmr_seasalt2)) DEALLOCATE(mmr_seasalt2)
IF (ALLOCATED(mmr_seasalt3)) DEALLOCATE(mmr_seasalt3)
!
WRITE (ILUOUT0,'(A,A4,A)') ' -- netcdf decoder for ',HFILE,' file ended successfully'
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
END SUBROUTINE READ_CHEM_DATA_CAMS_CASE
