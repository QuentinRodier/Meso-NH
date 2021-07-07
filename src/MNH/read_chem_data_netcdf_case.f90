!MNH_LIC Copyright 2012-2021 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!     ################################
      MODULE MODI_READ_CHEM_DATA_NETCDF_CASE
!     #################################
INTERFACE
SUBROUTINE READ_CHEM_DATA_NETCDF_CASE(TPPRE_REAL1,HFILE,TPPGDFILE, &
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
END SUBROUTINE READ_CHEM_DATA_NETCDF_CASE
!
END INTERFACE
END MODULE MODI_READ_CHEM_DATA_NETCDF_CASE
!     ####################################################################
      SUBROUTINE READ_CHEM_DATA_NETCDF_CASE(TPPRE_REAL1,HFILE,TPPGDFILE, &
                                            PTIME_HORI,KVERB,ODUMMY_REAL ) 
!     ####################################################################
!
!!****  *READ_CHEM_DATA_NETCDF_CASE* - reads data for the initialization of real cases.
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
!!         TLUOUT0 : name of output-listing
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
!  P. Wautelet 10/04/2019: replace ABORT and STOP calls by Print_msg
!  P. Wautelet 18/09/2019: correct support of 64bit integers (MNH_INT=8)
!  P. Wautelet 09/03/2021: move some chemistry initializations to ini_nsv
!-------------------------------------------------------------------------------
!
!*      0. DECLARATIONS
!------------
!
USE MODD_BLANK_n,    ONLY: CDUMMY1
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
USE MODD_IO,         ONLY: TFILEDATA
USE MODD_LUNIT,      ONLY: TLUOUT0
USE MODE_MODELN_HANDLER
USE MODD_NSV  
USE MODD_PARAMETERS
USE MODD_PARAM_n,    ONLY: CTURB
USE MODD_PRECISION,  ONLY: CDFINT
USE MODD_PREP_REAL
USE MODD_TIME
USE MODD_TIME_n
!
USE MODE_IO_FILE,    only: IO_File_close
USE MODE_MPPDB
USE MODE_THERMO
USE MODE_TIME
USE MODE_TOOLS,      ONLY: UPCASE
use mode_tools_ll,   only: GET_DIM_EXT_ll
!
USE MODI_CH_OPEN_INPUT
USE MODI_HORIBL
USE MODI_INI_NSV
USE MODI_READ_HGRID_n
USE MODI_READ_VER_GRID
USE MODI_XYTOLATLON
!
USE NETCDF
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
INTEGER                            :: IRET          ! Return code from subroutines
INTEGER                            :: JI,JJ,JK      ! Dummy counters
INTEGER                            :: JLOOP1        !  |
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
REAL,DIMENSION(:),ALLOCATABLE      :: ZVALUE1D        ! Intermediate array
REAL,DIMENSION(:,:),ALLOCATABLE    :: ZOUT          ! Intermediate arrays
REAL,DIMENSION(:),ALLOCATABLE      :: ZOUT1D          ! Intermediate arrays
INTEGER(kind=CDFINT)               :: ind_netcdf    ! Indice for netcdf var.
!chemistry field infile MOZ1.nam
INTEGER                                       :: ICHANNEL
CHARACTER(LEN=8)                              :: YMOZ="MOZ1.nam"
integer                                       :: IMOZ
CHARACTER(LEN=68)                             :: YFORMAT
CHARACTER(LEN=40), DIMENSION(:), ALLOCATABLE  :: YSPCMNH
integer, dimension(:), ALLOCATABLE            :: ISPCMOZ
CHARACTER(LEN=9)                              :: YA
REAL,DIMENSION(:,:),ALLOCATABLE               :: ZCOEFMOZART
CHARACTER(LEN=18),dimension(:,:),ALLOCATABLE  :: YCHANGE
type TZMOZ
real                                          :: ZCOEFMOZ
character(16)                                 :: YSPCMOZ
end type TZMOZ
type(TZMOZ), DIMENSION(:,:),ALLOCATABLE       :: TZSTOC
! model indice
INTEGER                           :: IMI
TYPE(TFILEDATA),POINTER                       :: TZFILE
!
! For netcdf 
!
CHARACTER(LEN=40)                     :: yrecname
integer              :: IKILEN
integer(kind=CDFINT) :: istatus, incid, ivarid
integer(kind=CDFINT) :: ilat_varid, ilon_varid, ilev_varid, itime_varid
integer(kind=CDFINT) :: ihyam_varid, ihybm_varid, ip0_varid, it_varid, iq_varid, ips_varid
integer(kind=CDFINT) :: irecid, ilatid, ilonid, ilevid, itimeid
integer(kind=CDFINT) :: ilatlen, ilonlen, ilevlen, inrecs, itimelen
integer(kind=CDFINT) :: itimeindex
INTEGER, DIMENSION(:), ALLOCATABLE    :: ikinlo
INTEGER(kind=CDFINT), DIMENSION(:), ALLOCATABLE :: icount3d, istart3d
INTEGER(kind=CDFINT), DIMENSION(:), ALLOCATABLE :: icount2d, istart2d
REAL                                  :: zp0
REAL, DIMENSION(:), ALLOCATABLE       :: zlats
REAL, DIMENSION(:), ALLOCATABLE       :: zlons
REAL, DIMENSION(:), ALLOCATABLE       :: zlevs
REAL, DIMENSION(:), ALLOCATABLE       :: ztime, zhyam, zhybm
REAL, DIMENSION(:,:), ALLOCATABLE     :: ZPSMOZ
REAL, DIMENSION(:,:,:), ALLOCATABLE   :: zvartemp3d, zvartemp3dbis, zvartemp3dter
REAL, DIMENSION(:,:,:), ALLOCATABLE   :: zvartemp3dquater
REAL, DIMENSION(:,:,:), ALLOCATABLE   :: ZCHEMMOZ, ZTMOZ, ZQMOZ

real ::a,b

!----------------------------------------------------------------------
TZFILE => NULL()
!
IMI = GET_CURRENT_MODEL_INDEX()
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
!
!* 2. READ NETCDF FIELDS
!     ------------------
!
! 2.1 Open netcdf files
!print*,'Open netcdf files:',HFILE
!
istatus = nf90_open(HFILE, nf90_nowrite, incid)
if (istatus /= nf90_noerr) call handle_err(istatus)
!
! 2.2 Read netcdf files
!
! get dimension IDs
!
!* get dimension ID of unlimited variable in netcdf file
istatus = nf90_inquire(incid, unlimitedDimId = irecid)
if (istatus /= nf90_noerr) call handle_err(istatus)
istatus = nf90_inq_dimid(incid, "lat", ilatid)
if (istatus /= nf90_noerr) call handle_err(istatus)
istatus = nf90_inq_dimid(incid, "lon", ilonid)
if (istatus /= nf90_noerr) call handle_err(istatus)
istatus = nf90_inq_dimid(incid, "lev", ilevid)
if (istatus /= nf90_noerr) call handle_err(istatus)
!
! get dimensions
!
!* get dimension and name of unlimited variable in netcdf file
istatus = nf90_inquire_dimension(incid, irecid, name=yrecname, len=inrecs)
if (istatus /= nf90_noerr) call handle_err(istatus)
istatus = nf90_inquire_dimension(incid, ilatid, len=ilatlen)
if (istatus /= nf90_noerr) call handle_err(istatus)
istatus = nf90_inquire_dimension(incid, ilonid, len=ilonlen)
if (istatus /= nf90_noerr) call handle_err(istatus)
istatus = nf90_inquire_dimension(incid, ilevid, len=ilevlen)
if (istatus /= nf90_noerr) call handle_err(istatus)
!print*, ilatlen, ilonlen, ilevlen, inrecs
!
! get variable IDs
!
istatus = nf90_inq_varid(incid, "lat", ilat_varid)
if (istatus /= nf90_noerr) call handle_err(istatus)
istatus = nf90_inq_varid(incid, "lon", ilon_varid)
if (istatus /= nf90_noerr) call handle_err(istatus)
istatus = nf90_inq_varid(incid, "lev", ilev_varid)
if (istatus /= nf90_noerr) call handle_err(istatus)
istatus = nf90_inq_varid(incid, "time", itime_varid)
if (istatus /= nf90_noerr) call handle_err(istatus)
istatus = nf90_inq_varid(incid, "P0", ip0_varid)
if (istatus /= nf90_noerr) call handle_err(istatus)
istatus = nf90_inq_varid(incid, "hyam", ihyam_varid)
if (istatus /= nf90_noerr) call handle_err(istatus)
istatus = nf90_inq_varid(incid, "hybm", ihybm_varid)
if (istatus /= nf90_noerr) call handle_err(istatus)
istatus = nf90_inq_varid(incid, "T", it_varid)
if (istatus /= nf90_noerr) call handle_err(istatus)
istatus = nf90_inq_varid(incid, "Q", iq_varid)
if (istatus /= nf90_noerr) call handle_err(istatus)
istatus = nf90_inq_varid(incid, "PS", ips_varid)
if (istatus /= nf90_noerr) call handle_err(istatus)
!
IKILEN = ilatlen * ilonlen
!
! 2.3 Read data.
!
ALLOCATE (icount3d(4))
ALLOCATE (istart3d(4))
ALLOCATE (icount2d(3))
ALLOCATE (istart2d(3))
ALLOCATE (zlats(ilatlen))
ALLOCATE (zlons(ilonlen))
ALLOCATE (zlevs(ilevlen))
ALLOCATE (ztime(inrecs))
ALLOCATE (ikinlo(ilatlen))
ikinlo(:) = ilonlen
ALLOCATE (zvartemp3d(ilonlen,ilatlen,ilevlen))
ALLOCATE (zvartemp3dbis(ilonlen,ilatlen,ilevlen))
ALLOCATE (zvartemp3dter(ilonlen,ilatlen,ilevlen))
ALLOCATE (zvartemp3dquater(ilonlen,ilatlen,ilevlen))
ALLOCATE (ZCHEMMOZ(ilonlen,ilatlen,ilevlen))
ALLOCATE (ZTMOZ(ilonlen,ilatlen,ilevlen))
ALLOCATE (ZQMOZ(ilonlen,ilatlen,ilevlen))
ALLOCATE (ZPSMOZ(ilonlen,ilatlen))
ALLOCATE (XA_SV_LS(ilevlen))
ALLOCATE (zhyam(ilevlen))
ALLOCATE (XB_SV_LS(ilevlen))
ALLOCATE (zhybm(ilevlen))
ALLOCATE (XT_SV_LS(IIU,IJU,ilevlen))
ALLOCATE (XQ_SV_LS(IIU,IJU,ilevlen,1))
ALLOCATE (XPS_SV_LS(IIU,IJU))
ALLOCATE (XZS_SV_LS(IIU,IJU))
! take the orography from ECMWF
XZS_SV_LS(:,:) = XZS_LS(:,:)
!
! get values of variables
!
istatus = nf90_get_var(incid, ilat_varid, zlats(:))
if (istatus /= nf90_noerr) call handle_err(istatus)
istatus = nf90_get_var(incid, ilon_varid, zlons(:))
if (istatus /= nf90_noerr) call handle_err(istatus)
istatus = nf90_get_var(incid, ilev_varid, zlevs(:))
if (istatus /= nf90_noerr) call handle_err(istatus)
istatus = nf90_get_var(incid, itime_varid, ztime(:))
if (istatus /= nf90_noerr) call handle_err(istatus)
istatus = nf90_get_var(incid, ihyam_varid, zhyam)
if (istatus /= nf90_noerr) call handle_err(istatus)
istatus = nf90_get_var(incid, ihybm_varid, zhybm)
if (istatus /= nf90_noerr) call handle_err(istatus)
istatus = nf90_get_var(incid, ip0_varid, zp0)
if (istatus /= nf90_noerr) call handle_err(istatus)
XP00_SV_LS = zp0
!
! hyam and hybm coefficients for pressure calculations have to be reversed 
! from top-bottom to bottom-up direction
do JJ = 1, ilevlen
  XA_SV_LS(JJ) = zhyam(ilevlen+1-JJ)
  XB_SV_LS(JJ) = zhybm(ilevlen+1-JJ)
end do
!
!
!     Read 1 record of lon*lat*lev values, starting at the
!     beginning of the record (the (1, 1, 1, rec) element in the netCDF
!     file).
 icount3d(1) = ilonlen
 icount3d(2) = ilatlen
 icount3d(3) = ilevlen
 icount3d(4) = 1
 istart3d(1) = 1
 istart3d(2) = 1
 istart3d(3) = 1
! Choose time index according to the chosen time in namelist
! 1 for 06h - 2 for 12h - 3 for 18h - 4 for 24h
IF (CDUMMY1=="06") THEN
       itimeindex=1
ELSEIF (CDUMMY1=="12") THEN
       itimeindex=2
ELSEIF (CDUMMY1=="18") THEN
       itimeindex=3
ELSEIF ((CDUMMY1=="24").OR.(CDUMMY1=="00")) THEN
       itimeindex=4
ENDIF
 istart3d(4) = itimeindex
!
  istatus = nf90_get_var(incid, it_varid, zvartemp3d, start=istart3d, count=icount3d)
  if (istatus /= nf90_noerr) call handle_err(istatus)
!
do JJ=1,ilevlen
! lev, lat, lon
 ZTMOZ(:,:,JJ) = zvartemp3d(:,:,ilevlen+1-JJ)
enddo
!
  istatus = nf90_get_var(incid, iq_varid, zvartemp3d, start=istart3d, count=icount3d)
  if (istatus /= nf90_noerr) call handle_err(istatus)
!
do JJ=1,ilevlen
! lev, lat, lon
 ZQMOZ(:,:,JJ) = zvartemp3d(:,:,ilevlen+1-JJ)
enddo
!
 icount2d(1) = ilonlen
 icount2d(2) = ilatlen
 icount2d(3) = 1
 istart2d(1) = 1
 istart2d(2) = 1
 istart2d(3) = itimeindex
  istatus = nf90_get_var(incid, ips_varid, ZPSMOZ(:,:), start=istart2d, count=icount2d)
  if (istatus /= nf90_noerr) call handle_err(istatus)

  
!------------------------------------------------------------------------
!* 3 Interpolation of MOZART variable
!---------------------------------------------------------------------
  LUSECHEM = .TRUE.
  IF (LORILAM) THEN
    CORGANIC = "MPMPO"
    LVARSIGI = .TRUE.
    LVARSIGJ = .TRUE.
  END IF
  ! initialise NSV_* variables
  CALL INI_NSV(IMI)
    DEALLOCATE(XSV_LS)
    ALLOCATE (XSV_LS(IIU,IJU,ilevlen,NSV))
   XSV_LS(:,:,:,:) = 0.
!
  WRITE (ILUOUT0,'(A,A4,A)') ' | Reading MOZART species (ppp) from ',HFILE,' file'

where (ZLONOUT(:) < 0.) ZLONOUT(:) = ZLONOUT(:) + 360.
!
ALLOCATE(ZVALUE(ilevlen,IKILEN))
ALLOCATE(ZOUT(ilevlen,INO))
ALLOCATE(ZVALUE1D(IKILEN))
ALLOCATE(ZOUT1D(INO))

!
!*       2.6.1  read MOZART species from the file MOZ1.nam
!
! open input file
CALL CH_OPEN_INPUT(YMOZ,"MOZ2MESONH",TZFILE,ILUOUT0,KVERB)
ICHANNEL = TZFILE%NLU
!
!read number of mocage species to transfer into mesonh
READ(ICHANNEL, *) IMOZ
IF (KVERB >= 5) WRITE (ILUOUT0,*) "number of mozart species to transfer into &
& mesonh : ", IMOZ
!
!read data input format
READ(ICHANNEL,"(A)") YFORMAT
YFORMAT=UPCASE(YFORMAT)
IF (KVERB >= 5) WRITE (ILUOUT0,*) "input format is: ", YFORMAT
!
!allocate fields
ALLOCATE(YSPCMNH(IMOZ))      !MESONH species
ALLOCATE(TZSTOC(IMOZ,4))     !MOZART coefficient and MOZART species associated
ALLOCATE(ISPCMOZ(IMOZ))      !MOZART species number into MESONH species
ALLOCATE(ZCOEFMOZART(IMOZ,4))!Coef stoich of each MOZART species
ALLOCATE(YCHANGE(IMOZ,4))    !MOZART species with _VMR_inst
!read MESONH variable names and MOZART variable names associated 
DO JI = 1,IMOZ               !for every MNH species existing in MOZ1.nam                              
  READ(ICHANNEL,YFORMAT) YSPCMNH(JI), ISPCMOZ(JI), TZSTOC(JI,1)%ZCOEFMOZ,& !reading line by line
                 TZSTOC(JI,1)%YSPCMOZ, TZSTOC(JI,2)%ZCOEFMOZ,&             !of string
                 TZSTOC(JI,2)%YSPCMOZ, TZSTOC(JI,3)%ZCOEFMOZ,&
                 TZSTOC(JI,3)%YSPCMOZ, TZSTOC(JI,4)%ZCOEFMOZ,&
                 TZSTOC(JI,4)%YSPCMOZ
  WRITE(ILUOUT0,YFORMAT) YSPCMNH(JI), ISPCMOZ(JI),&                        !writing in arrays
                TZSTOC(JI,1)%ZCOEFMOZ, TZSTOC(JI,1)%YSPCMOZ,&
                TZSTOC(JI,2)%ZCOEFMOZ, TZSTOC(JI,2)%YSPCMOZ,&
                TZSTOC(JI,3)%ZCOEFMOZ, TZSTOC(JI,3)%YSPCMOZ,&
                TZSTOC(JI,4)%ZCOEFMOZ, TZSTOC(JI,4)%YSPCMOZ
!
  ZCOEFMOZART(JI,1) =  (TZSTOC(JI,1)%ZCOEFMOZ) !coef stoich of each MOZART species set into an array 
  ZCOEFMOZART(JI,2) =  (TZSTOC(JI,2)%ZCOEFMOZ) 
  ZCOEFMOZART(JI,3) =  (TZSTOC(JI,3)%ZCOEFMOZ)
  ZCOEFMOZART(JI,4) =  (TZSTOC(JI,4)%ZCOEFMOZ)
! 
  YA="_VMR_inst"
  YCHANGE(JI,1)=trim(TZSTOC(JI,1)%YSPCMOZ)//YA !set into an array MOZART species with _VMR_inst
  YCHANGE(JI,2)=trim(TZSTOC(JI,2)%YSPCMOZ)//YA 
  YCHANGE(JI,3)=trim(TZSTOC(JI,3)%YSPCMOZ)//YA
  YCHANGE(JI,4)=trim(TZSTOC(JI,4)%YSPCMOZ)//YA
!
!* exchange mozart values onto prognostic variables XSV_LS
! and convert MOZART fields to 2D for use in horizontal interpolation 
! routine HORIBL.f90
!
  DO JNCHEM = NSV_CHEMBEG, NSV_CHEMEND  !loop on all MNH species
    IF (trim(CNAMES(JNCHEM-NSV_CHEMBEG+1))==trim(YSPCMNH(JI))) THEN !MNH mechanism species
       IF (ISPCMOZ(JI)==1) THEN
         istatus = nf90_inq_varid(incid, trim(YCHANGE(JI,1)), ind_netcdf)
         if (istatus /= nf90_noerr) call handle_err(istatus)
         istatus = nf90_get_var(incid, ind_netcdf, zvartemp3d, start=istart3d, count=icount3d)
         if (istatus /= nf90_noerr) call  handle_err(istatus)
         DO JJ=1,ilevlen ! lev, lat, lon
           ZCHEMMOZ(:,:,JJ)=ZCOEFMOZART(JI,1)*zvartemp3d(:,:,ilevlen+1-JJ)
         ENDDO
       ELSE IF (ISPCMOZ(JI)==2) THEN
         istatus = nf90_inq_varid(incid, trim(YCHANGE(JI,1)), ind_netcdf)
         if (istatus /= nf90_noerr) call handle_err(istatus)
         istatus = nf90_get_var(incid, ind_netcdf, zvartemp3d, start=istart3d, count=icount3d)
         if (istatus /= nf90_noerr) call  handle_err(istatus)
         istatus = nf90_inq_varid(incid, trim(YCHANGE(JI,2)), ind_netcdf)
         if (istatus /= nf90_noerr) call handle_err(istatus)
         istatus = nf90_get_var(incid, ind_netcdf, zvartemp3dbis, start=istart3d, count=icount3d)
         if (istatus /= nf90_noerr) call  handle_err(istatus)
         DO JJ=1,ilevlen ! lev, lat, lon
           ZCHEMMOZ(:,:,JJ)=ZCOEFMOZART(JI,1)*zvartemp3d(:,:,ilevlen+1-JJ) + &
                               ZCOEFMOZART(JI,2)*zvartemp3dbis(:,:,ilevlen+1-JJ)
         ENDDO
       ELSE IF (ISPCMOZ(JI)==3) THEN
         istatus = nf90_inq_varid(incid, trim(YCHANGE(JI,1)), ind_netcdf)
         if (istatus /= nf90_noerr) call handle_err(istatus)
         istatus = nf90_get_var(incid, ind_netcdf, zvartemp3d, start=istart3d, count=icount3d)
         if (istatus /= nf90_noerr) call  handle_err(istatus)
         istatus = nf90_inq_varid(incid, trim(YCHANGE(JI,2)), ind_netcdf)
         if (istatus /= nf90_noerr) call handle_err(istatus)
         istatus = nf90_get_var(incid, ind_netcdf, zvartemp3dbis, start=istart3d, count=icount3d)
         if (istatus /= nf90_noerr) call  handle_err(istatus)
         istatus = nf90_inq_varid(incid, trim(YCHANGE(JI,3)), ind_netcdf)
         if (istatus /= nf90_noerr) call handle_err(istatus)
         istatus = nf90_get_var(incid, ind_netcdf, zvartemp3dter, start=istart3d, count=icount3d)
         if (istatus /= nf90_noerr) call  handle_err(istatus)
         DO JJ=1,ilevlen ! lev, lat, lon
           ZCHEMMOZ(:,:,JJ)=ZCOEFMOZART(JI,1)*zvartemp3d(:,:,ilevlen+1-JJ)+&
                            ZCOEFMOZART(JI,2)*zvartemp3dbis(:,:,ilevlen+1-JJ)+&
                            ZCOEFMOZART(JI,3)*zvartemp3dter(:,:,ilevlen+1-JJ)
         ENDDO
       ELSE IF (ISPCMOZ(JI)==4) THEN
         istatus = nf90_inq_varid(incid, trim(YCHANGE(JI,1)), ind_netcdf)
         if (istatus /= nf90_noerr) call handle_err(istatus)
         istatus = nf90_get_var(incid, ind_netcdf, zvartemp3d, start=istart3d, count=icount3d)
         if (istatus /= nf90_noerr) call  handle_err(istatus)
         istatus = nf90_inq_varid(incid, trim(YCHANGE(JI,2)), ind_netcdf)
         if (istatus /= nf90_noerr) call handle_err(istatus)
         istatus = nf90_get_var(incid, ind_netcdf, zvartemp3dbis, start=istart3d, count=icount3d)
         if (istatus /= nf90_noerr) call  handle_err(istatus)
         istatus = nf90_inq_varid(incid, trim(YCHANGE(JI,3)), ind_netcdf)
         if (istatus /= nf90_noerr) call handle_err(istatus)
         istatus = nf90_get_var(incid, ind_netcdf, zvartemp3dter, start=istart3d, count=icount3d)
         if (istatus /= nf90_noerr) call  handle_err(istatus)
         istatus = nf90_inq_varid(incid, trim(YCHANGE(JI,4)), ind_netcdf)
         if (istatus /= nf90_noerr) call handle_err(istatus)
         istatus = nf90_get_var(incid, ind_netcdf, zvartemp3dquater, start=istart3d, count=icount3d)
         if (istatus /= nf90_noerr) call  handle_err(istatus)
         DO JJ=1,ilevlen ! lev, lat, lon
           ZCHEMMOZ(:,:,JJ)=ZCOEFMOZART(JI,1)*zvartemp3d(:,:,ilevlen+1-JJ)+&
                               ZCOEFMOZART(JI,2)*zvartemp3dbis(:,:,ilevlen+1-JJ)+&
                               ZCOEFMOZART(JI,3)*zvartemp3dter(:,:,ilevlen+1-JJ)+&
                               ZCOEFMOZART(JI,4)*zvartemp3dquater(:,:,ilevlen+1-JJ)
         ENDDO
       ENDIF
       DO JK = 1, ilevlen
         JLOOP1 = 0
         DO JJ = 1, ilatlen
           ZVALUE(JK,JLOOP1+1:JLOOP1+ilonlen) = ZCHEMMOZ(1:ilonlen,JJ,JK)
           JLOOP1 = JLOOP1+ilonlen
         ENDDO                                                                                           
         CALL HORIBL(zlats(1),zlons(1),zlats(ilatlen),zlons(ilonlen), &
                     int(ilatlen,kind=kind(1)),ikinlo,IKILEN,      &
                     ZVALUE(JK,:),INO,ZLONOUT,ZLATOUT,          &
                     ZOUT(JK,:),.FALSE.,PTIME_HORI,.TRUE.)
         CALL ARRAY_1D_TO_2D(INO,ZOUT(JK,:),IIU,IJU, &
                             XSV_LS(:,:,JK,JNCHEM)   )
       ENDDO  ! ilevlen
    ENDIF      

  ENDDO ! JNCHEM
  DO JNAER = NSV_AERBEG, NSV_AEREND 
    IF (trim(CAERONAMES(JNAER-NSV_AERBEG+1))==trim(YSPCMNH(JI))) THEN !MNH mechanism species
       IF (ISPCMOZ(JI)==1) THEN
         istatus = nf90_inq_varid(incid, trim(YCHANGE(JI,1)), ind_netcdf)
         if (istatus /= nf90_noerr) call handle_err(istatus)
         istatus = nf90_get_var(incid, ind_netcdf, zvartemp3d, start=istart3d, count=icount3d)
         if (istatus /= nf90_noerr) call  handle_err(istatus)
         DO JJ=1,ilevlen ! lev, lat, lon
           ZCHEMMOZ(:,:,JJ)=ZCOEFMOZART(JI,1)*zvartemp3d(:,:,ilevlen+1-JJ)
         ENDDO
       ELSE IF (ISPCMOZ(JI)==2) THEN
         istatus = nf90_inq_varid(incid, trim(YCHANGE(JI,1)), ind_netcdf)
         if (istatus /= nf90_noerr) call handle_err(istatus)
         istatus = nf90_get_var(incid, ind_netcdf, zvartemp3d, start=istart3d, count=icount3d)
         if (istatus /= nf90_noerr) call  handle_err(istatus)
         istatus = nf90_inq_varid(incid, trim(YCHANGE(JI,2)), ind_netcdf)
         if (istatus /= nf90_noerr) call handle_err(istatus)
         istatus = nf90_get_var(incid, ind_netcdf, zvartemp3dbis, start=istart3d, count=icount3d)
         if (istatus /= nf90_noerr) call  handle_err(istatus)
         DO JJ=1,ilevlen ! lev, lat, lon
           ZCHEMMOZ(:,:,JJ)=ZCOEFMOZART(JI,1)*zvartemp3d(:,:,ilevlen+1-JJ) + &
                               ZCOEFMOZART(JI,2)*zvartemp3dbis(:,:,ilevlen+1-JJ)
         ENDDO
       ELSE IF (ISPCMOZ(JI)==3) THEN
         istatus = nf90_inq_varid(incid, trim(YCHANGE(JI,1)), ind_netcdf)
         if (istatus /= nf90_noerr) call handle_err(istatus)
         istatus = nf90_get_var(incid, ind_netcdf, zvartemp3d, start=istart3d, count=icount3d)
         if (istatus /= nf90_noerr) call  handle_err(istatus)
         istatus = nf90_inq_varid(incid, trim(YCHANGE(JI,2)), ind_netcdf)
         if (istatus /= nf90_noerr) call handle_err(istatus)
         istatus = nf90_get_var(incid, ind_netcdf, zvartemp3dbis, start=istart3d, count=icount3d)
         if (istatus /= nf90_noerr) call  handle_err(istatus)
         istatus = nf90_inq_varid(incid, trim(YCHANGE(JI,3)), ind_netcdf)
         if (istatus /= nf90_noerr) call handle_err(istatus)
         istatus = nf90_get_var(incid, ind_netcdf, zvartemp3dter, start=istart3d, count=icount3d)
         if (istatus /= nf90_noerr) call  handle_err(istatus)
         DO JJ=1,ilevlen ! lev, lat, lon
           ZCHEMMOZ(:,:,JJ)=ZCOEFMOZART(JI,1)*zvartemp3d(:,:,ilevlen+1-JJ)+&
                            ZCOEFMOZART(JI,2)*zvartemp3dbis(:,:,ilevlen+1-JJ)+&
                            ZCOEFMOZART(JI,3)*zvartemp3dter(:,:,ilevlen+1-JJ)
         ENDDO
       ELSE IF (ISPCMOZ(JI)==4) THEN
         istatus = nf90_inq_varid(incid, trim(YCHANGE(JI,1)), ind_netcdf)
         if (istatus /= nf90_noerr) call handle_err(istatus)
         istatus = nf90_get_var(incid, ind_netcdf, zvartemp3d, start=istart3d, count=icount3d)
         if (istatus /= nf90_noerr) call  handle_err(istatus)
         istatus = nf90_inq_varid(incid, trim(YCHANGE(JI,2)), ind_netcdf)
         if (istatus /= nf90_noerr) call handle_err(istatus)
         istatus = nf90_get_var(incid, ind_netcdf, zvartemp3dbis, start=istart3d, count=icount3d)
         if (istatus /= nf90_noerr) call  handle_err(istatus)
         istatus = nf90_inq_varid(incid, trim(YCHANGE(JI,3)), ind_netcdf)
         if (istatus /= nf90_noerr) call handle_err(istatus)
         istatus = nf90_get_var(incid, ind_netcdf, zvartemp3dter, start=istart3d, count=icount3d)
         if (istatus /= nf90_noerr) call  handle_err(istatus)
         istatus = nf90_inq_varid(incid, trim(YCHANGE(JI,4)), ind_netcdf)
         if (istatus /= nf90_noerr) call handle_err(istatus)
         istatus = nf90_get_var(incid, ind_netcdf, zvartemp3dquater, start=istart3d, count=icount3d)
         if (istatus /= nf90_noerr) call  handle_err(istatus)
         DO JJ=1,ilevlen ! lev, lat, lon
           ZCHEMMOZ(:,:,JJ)=ZCOEFMOZART(JI,1)*zvartemp3d(:,:,ilevlen+1-JJ)+&
                               ZCOEFMOZART(JI,2)*zvartemp3dbis(:,:,ilevlen+1-JJ)+&
                               ZCOEFMOZART(JI,3)*zvartemp3dter(:,:,ilevlen+1-JJ)+&
                               ZCOEFMOZART(JI,4)*zvartemp3dquater(:,:,ilevlen+1-JJ)
         ENDDO
       ENDIF
       DO JK = 1, ilevlen
         JLOOP1 = 0
         DO JJ = 1, ilatlen
           ZVALUE(JK,JLOOP1+1:JLOOP1+ilonlen) = ZCHEMMOZ(1:ilonlen,JJ,JK)
           JLOOP1 = JLOOP1+ilonlen
         ENDDO                                                                                           
         CALL HORIBL(zlats(1),zlons(1),zlats(ilatlen),zlons(ilonlen), &
                     int(ilatlen,kind=kind(1)),ikinlo,IKILEN,      &
                     ZVALUE(JK,:),INO,ZLONOUT,ZLATOUT,          &
                     ZOUT(JK,:),.FALSE.,PTIME_HORI,.TRUE.)
         CALL ARRAY_1D_TO_2D(INO,ZOUT(JK,:),IIU,IJU, &
                             XSV_LS(:,:,JK,JNAER)   )
       ENDDO  ! ilevlen
    ENDIF         
  ENDDO ! JNAER
ENDDO  ! JIDO JNCHEM = NSV_CHEMBEG, NSV_CHEMEND  !loop on all MNH species
DEALLOCATE(YSPCMNH) 
DEALLOCATE(TZSTOC)
DEALLOCATE(ISPCMOZ) 
DEALLOCATE(ZCOEFMOZART)
DEALLOCATE(YCHANGE)
!
XSV_LS(:,:,:,:) = MAX(XSV_LS(:,:,:,:),0.)
!
DO JK = 1, ilevlen
  JLOOP1 = 0
  DO JJ = 1, ilatlen
    ZVALUE(JK,JLOOP1+1:JLOOP1+ilonlen) = ZTMOZ(1:ilonlen,JJ,JK)
    JLOOP1 = JLOOP1 + ilonlen
  ENDDO
  CALL HORIBL(zlats(1),zlons(1),zlats(ilatlen),zlons(ilonlen), &
              int(ilatlen,kind=kind(1)),ikinlo,IKILEN,      &
              ZVALUE(JK,:),INO,ZLONOUT,ZLATOUT,          &
              ZOUT(JK,:),.FALSE.,PTIME_HORI,.FALSE.)
!
  CALL ARRAY_1D_TO_2D(INO,ZOUT(JK,:),IIU,IJU, &
                      XT_SV_LS(:,:,JK))
ENDDO 
XT_SV_LS(:,:,:) = MAX(XT_SV_LS(:,:,:),0.)
!
DO JK = 1, ilevlen
  JLOOP1 = 0
  DO JJ = 1, ilatlen
    ZVALUE(JK,JLOOP1+1:JLOOP1+ilonlen) = ZQMOZ(1:ilonlen,JJ,JK)
    JLOOP1 = JLOOP1 + ilonlen
  ENDDO
  CALL HORIBL(zlats(1),zlons(1),zlats(ilatlen),zlons(ilonlen), &
              int(ilatlen,kind=kind(1)),ikinlo,IKILEN,      &
              ZVALUE(JK,:),INO,ZLONOUT,ZLATOUT,                  &
              ZOUT(JK,:),.FALSE.,PTIME_HORI,.FALSE.)
!
   CALL ARRAY_1D_TO_2D(INO,ZOUT(JK,:),IIU,IJU,                    &
                       XQ_SV_LS(:,:,JK,1))
ENDDO 
XQ_SV_LS(:,:,:,1) = MAX(XQ_SV_LS(:,:,:,1),0.)
!
JLOOP1 = 0
DO JJ = 1, ilatlen
  ZVALUE1D(JLOOP1+1:JLOOP1+ilonlen) = ZPSMOZ(1:ilonlen,JJ)
  JLOOP1 = JLOOP1 + ilonlen
ENDDO
CALL HORIBL(zlats(1),zlons(1),zlats(ilatlen),zlons(ilonlen), &
            int(ilatlen,kind=kind(1)),ikinlo,IKILEN,      &
            ZVALUE1D(:),INO,ZLONOUT,ZLATOUT,                  &
            ZOUT1D(:),.FALSE.,PTIME_HORI,.FALSE.)
!
CALL ARRAY_1D_TO_2D(INO,ZOUT1D(:),IIU,IJU,                    &
                    XPS_SV_LS(:,:))
XPS_SV_LS(:,:) = MAX(XPS_SV_LS(:,:),0.)
!
!
!
! close the netcdf file
istatus = nf90_close(incid)
if (istatus /= nf90_noerr) call handle_err(istatus)
!
  DEALLOCATE (ZVALUE)
  DEALLOCATE (ZOUT)
  DEALLOCATE (ZVALUE1D) 
  DEALLOCATE (ZOUT1D)
!!

! close
! file
CALL IO_File_close(TZFILE)


!-------------------------------------------------------------
!
!* 4. VERTICAL GRID
!
!* 4.1 Read VERTICAL GRID
!
WRITE (ILUOUT0,'(A)') ' | Reading of vertical grid in progress'
CALL READ_VER_GRID(TPPRE_REAL1)
!
!--------------------------------------------------------------
!
!* 4.2 Interpolate on Meso-NH VERTICAL GRID
!
!* 4.3 Free all temporary allocations
!
DEALLOCATE (ZLATOUT)
DEALLOCATE (ZLONOUT)
DEALLOCATE (zhyam)
DEALLOCATE (zhybm)
DEALLOCATE (zvartemp3d)
DEALLOCATE (zvartemp3dbis)
DEALLOCATE (zvartemp3dter)
DEALLOCATE (zvartemp3dquater)
!
WRITE (ILUOUT0,'(A,A4,A)') ' -- netcdf decoder for ',HFILE,' file ended successfully'
!
!
CONTAINS
!
! #############################
  subroutine handle_err(istatus)
! #############################
    use mode_msg

    integer(kind=CDFINT) istatus

    if ( istatus /= NF90_NOERR ) then
      call Print_msg( NVERB_FATAL, 'IO', 'HANDLE_ERR', NF90_STRERROR(istatus) )
    end if

  end subroutine handle_err
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
END SUBROUTINE READ_CHEM_DATA_NETCDF_CASE
