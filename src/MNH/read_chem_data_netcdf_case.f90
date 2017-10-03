!     ################################
      MODULE MODI_READ_CHEM_DATA_NETCDF_CASE
!     #################################
INTERFACE
SUBROUTINE READ_CHEM_DATA_NETCDF_CASE(HPRE_REAL1,HFILE,HPGDFILE,      &
                    PTIME_HORI,KVERB,ODUMMY_REAL                         ) 
!
CHARACTER(LEN=28),  INTENT(IN) :: HPRE_REAL1 ! name of the PRE_REAL1 file
CHARACTER(LEN=28),  INTENT(IN) :: HFILE    ! name of the NETCDF file
CHARACTER(LEN=28),  INTENT(IN) :: HPGDFILE ! name of the physiographic data file
INTEGER,           INTENT(IN)  :: KVERB    ! verbosity level
LOGICAL,           INTENT(IN)  :: ODUMMY_REAL! flag to interpolate dummy fields
REAL,           INTENT(INOUT)  :: PTIME_HORI ! time spent in hor. interpolations
END SUBROUTINE READ_CHEM_DATA_NETCDF_CASE
!
END INTERFACE
END MODULE MODI_READ_CHEM_DATA_NETCDF_CASE
!     ##########################################################################
      SUBROUTINE READ_CHEM_DATA_NETCDF_CASE(HPRE_REAL1,HFILE,HPGDFILE,      &
                       PTIME_HORI,KVERB,ODUMMY_REAL                            )
!     ##########################################################################
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
!!    function   FMLOOK_ll         : to retrieve the logical unit associated with a file
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
!-------------------------------------------------------------------------------
!
!*      0. DECLARATIONS
!------------
USE MODE_FM
USE MODE_IO_ll
USE MODE_TIME
!
USE MODI_READ_HGRID_n
USE MODI_READ_VER_GRID
USE MODI_XYTOLATLON
USE MODI_HORIBL
USE MODI_INI_NSV
USE MODI_CH_INIT_SCHEME_n
USE MODI_CH_AER_INIT_SOA
!
USE MODD_CONF
USE MODD_CONF_n
USE MODD_CST
USE MODD_LUNIT
USE MODD_PARAMETERS
USE MODD_GRID
USE MODD_GRID_n
USE MODD_DIM_n
USE MODD_PARAM_n, ONLY : CTURB
USE MODD_TIME
USE MODD_TIME_n
USE MODD_CH_MNHC_n, ONLY : LUSECHEM,LUSECHAQ,LUSECHIC,LCH_PH
USE MODD_CH_M9_n,   ONLY : NEQ ,  CNAMES
USE MODD_CH_AEROSOL, ONLY: CORGANIC, NCARB, NSOA, NSP, LORILAM,&
                           JPMODE, LVARSIGI, LVARSIGJ,CAERONAMES
USE MODD_NSV  
USE MODD_PREP_REAL
USE MODE_MODELN_HANDLER
!JUAN REALZ
USE MODE_MPPDB
!JUAN REALZ
USE MODI_CH_OPEN_INPUT  
USE MODE_THERMO
!
USE MODD_BLANK
!
IMPLICIT NONE
!
include 'netcdf.inc'
!
!* 0.1. Declaration of arguments
!       ------------------------
!
CHARACTER(LEN=28),      INTENT(IN) :: HPRE_REAL1 ! name of the PRE_REAL1 file
CHARACTER(LEN=28),      INTENT(IN) :: HFILE      ! name of the GRIB file
CHARACTER(LEN=28),      INTENT(IN) :: HPGDFILE   ! name of the physiographic data file
INTEGER,               INTENT(IN)  :: KVERB    ! verbosity level
LOGICAL,               INTENT(IN)  :: ODUMMY_REAL! flag to interpolate dummy fields
REAL,                INTENT(INOUT) :: PTIME_HORI ! time spent in hor. interpolations
!
!* 0.2 Declaration of local variables
!      ------------------------------
! General purpose variables
INTEGER                            :: ILUOUT0       ! Unit used for output msg.
INTEGER                            :: IRESP   ! Return code of FM-routines
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
INTEGER                            :: ind_netcdf    ! Indice for netcdf var.
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
!
! For netcdf 
!
integer                               :: status, ncid, varid
integer :: lat_varid, lon_varid, lev_varid, time_varid 
integer :: hyam_varid, hybm_varid, p0_varid, t_varid, q_varid, ps_varid 
integer :: recid, latid, lonid, levid, timeid
integer :: latlen, lonlen, levlen, nrecs,timelen
integer :: itimeindex, KILEN, jrec
CHARACTER(LEN=40)                     :: recname
REAL, DIMENSION(:), ALLOCATABLE       :: lats
REAL, DIMENSION(:), ALLOCATABLE       :: lons 
REAL, DIMENSION(:), ALLOCATABLE       :: levs 
INTEGER, DIMENSION(:), ALLOCATABLE    :: count3d, start3d
INTEGER, DIMENSION(:), ALLOCATABLE    :: count2d, start2d 
REAL, DIMENSION(:), ALLOCATABLE       :: time, hyam, hybm 
REAL                                  :: p0 
INTEGER, DIMENSION(:), ALLOCATABLE    :: kinlo 
REAL, DIMENSION(:,:,:), ALLOCATABLE   :: vartemp3d,vartemp3dbis,vartemp3dter 
REAL, DIMENSION(:,:,:), ALLOCATABLE   :: vartemp3dquater 
REAL, DIMENSION(:,:,:), ALLOCATABLE   :: ZCHEMMOZ, TMOZ, QMOZ
REAL, DIMENSION(:,:), ALLOCATABLE     :: PSMOZ 

real ::a,b

!----------------------------------------------------------------------
!
IMI = GET_CURRENT_MODEL_INDEX()
!
!* 1. READ PGD FILE
!     -------------
!
CALL FMLOOK_ll(CLUOUT0,CLUOUT0,ILUOUT0,IRET)
CALL READ_HGRID_n(HPGDFILE,YPGD_NAME,YPGD_DAD_NAME,YPGD_TYPE)
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
status = nf_open(HFILE, nf_nowrite, ncid) 
if (status /= nf_noerr) call handle_err(status)
!
! 2.2 Read netcdf files
!
! get dimension IDs
!
!* get dimension ID of unlimited variable in netcdf file
status = nf_inq_unlimdim(ncid, recid)
if (status /= nf_noerr) call handle_err(status)
status = nf_inq_dimid(ncid, "lat", latid)
if (status /= nf_noerr) call handle_err(status)
status = nf_inq_dimid(ncid, "lon", lonid)
if (status /= nf_noerr) call handle_err(status)
status = nf_inq_dimid(ncid, "lev", levid)
if (status /= nf_noerr) call handle_err(status)
!
! get dimensions
!
!* get dimension and name of unlimited variable in netcdf file
status = nf_inq_dim(ncid, recid, recname, nrecs)
if (status /= nf_noerr) call handle_err(status)
status = nf_inq_dimlen(ncid, latid, latlen)
if (status /= nf_noerr) call handle_err(status)
status = nf_inq_dimlen(ncid, lonid, lonlen)
if (status /= nf_noerr) call handle_err(status)
status = nf_inq_dimlen(ncid, levid, levlen)
if (status /= nf_noerr) call handle_err(status)
!print*, latlen, lonlen, levlen, nrecs
!
! get variable IDs
!
status = nf_inq_varid(ncid, "lat", lat_varid)
if (status /= nf_noerr) call handle_err(status)
status = nf_inq_varid(ncid, "lon", lon_varid)
if (status /= nf_noerr) call handle_err(status)
status = nf_inq_varid(ncid, "lev", lev_varid)
if (status /= nf_noerr) call handle_err(status)
status = nf_inq_varid(ncid, "time", time_varid)
if (status /= nf_noerr) call handle_err(status)
status = nf_inq_varid(ncid, "P0", p0_varid)
if (status /= nf_noerr) call handle_err(status)
status = nf_inq_varid(ncid, "hyam", hyam_varid)
if (status /= nf_noerr) call handle_err(status)
status = nf_inq_varid(ncid, "hybm", hybm_varid)
if (status /= nf_noerr) call handle_err(status)
status = nf_inq_varid(ncid, "T", t_varid)
if (status /= nf_noerr) call handle_err(status)
status = nf_inq_varid(ncid, "Q", q_varid)
if (status /= nf_noerr) call handle_err(status)
status = nf_inq_varid(ncid, "PS", ps_varid)
if (status /= nf_noerr) call handle_err(status)
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
ALLOCATE (time(nrecs))
ALLOCATE (kinlo(latlen))
kinlo(:) = lonlen
ALLOCATE (vartemp3d(lonlen,latlen,levlen))
ALLOCATE (vartemp3dbis(lonlen,latlen,levlen))
ALLOCATE (vartemp3dter(lonlen,latlen,levlen))
ALLOCATE (vartemp3dquater(lonlen,latlen,levlen))
ALLOCATE (ZCHEMMOZ(lonlen,latlen,levlen))
ALLOCATE (TMOZ(lonlen,latlen,levlen))
ALLOCATE (QMOZ(lonlen,latlen,levlen))
ALLOCATE (PSMOZ(lonlen,latlen))
ALLOCATE (XA_SV_LS(levlen))
ALLOCATE (hyam(levlen))
ALLOCATE (XB_SV_LS(levlen))
ALLOCATE (hybm(levlen))
ALLOCATE (XT_SV_LS(IIU,IJU,levlen))
ALLOCATE (XQ_SV_LS(IIU,IJU,levlen,1))
ALLOCATE (XPS_SV_LS(IIU,IJU))
ALLOCATE (XZS_SV_LS(IIU,IJU))
! take the orography from ECMWF
XZS_SV_LS(:,:) = XZS_LS(:,:)
!
! get values of variables
!
status = nf_get_var_double(ncid, lat_varid, lats(:))
if (status /= nf_noerr) call handle_err(status)
status = nf_get_var_double(ncid, lon_varid, lons(:))
if (status /= nf_noerr) call handle_err(status)
status = nf_get_var_double(ncid, lev_varid, levs(:))
if (status /= nf_noerr) call handle_err(status)
status = nf_get_var_double(ncid, time_varid, time(:))
if (status /= nf_noerr) call handle_err(status)
status = nf_get_var_double(ncid, hyam_varid, hyam)
if (status /= nf_noerr) call handle_err(status)
status = nf_get_var_double(ncid, hybm_varid, hybm)
if (status /= nf_noerr) call handle_err(status)
status = nf_get_var_double(ncid, p0_varid, p0)
if (status /= nf_noerr) call handle_err(status)
XP00_SV_LS = p0
!
! hyam and hybm coefficients for pressure calculations have to be reversed 
! from top-bottom to bottom-up direction
do JJ = 1, levlen
  XA_SV_LS(JJ) = hyam(levlen+1-JJ)
  XB_SV_LS(JJ) = hybm(levlen+1-JJ)
end do
!
!
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
! Choose time index according to the chosen time in namelist
! 1 for 00 - 2 for 06 - 3 for 12 - 4 for 18
IF (CDUMMY1=="00") THEN 
        itimeindex=1
ELSEIF (CDUMMY1=="06") THEN
        itimeindex=2
ELSEIF (CDUMMY1=="12") THEN
        itimeindex=3
ELSEIF (CDUMMY1=="18") THEN
        itimeindex=4
ENDIF
 start3d(4) = itimeindex
!
  status = nf_get_vara_double(ncid, t_varid, start3d, count3d, &
           vartemp3d)
  if (status /= nf_noerr) call handle_err(status)
!
do JJ=1,levlen
! lev, lat, lon
 TMOZ(:,:,JJ) = vartemp3d(:,:,levlen+1-JJ)
enddo
!
  status = nf_get_vara_double(ncid, q_varid, start3d, count3d, &
           vartemp3d)
  if (status /= nf_noerr) call handle_err(status)
!
do JJ=1,levlen
! lev, lat, lon
 QMOZ(:,:,JJ) = vartemp3d(:,:,levlen+1-JJ)
enddo
!
 count2d(1) = lonlen
 count2d(2) = latlen
 count2d(3) = 1
 start2d(1) = 1
 start2d(2) = 1
 start2d(3) = itimeindex
  status = nf_get_vara_double(ncid, ps_varid, start2d, count2d, PSMOZ(:,:))
  if (status /= nf_noerr) call handle_err(status)

  
!------------------------------------------------------------------------
!* 3 Interpolation of MOZART variable
!---------------------------------------------------------------------
  ! Always initialize chemical scheme variables before INI_NSV call !
  CALL CH_INIT_SCHEME_n(IMI,LUSECHAQ,LUSECHIC,LCH_PH,ILUOUT0,KVERB)
  LUSECHEM = .TRUE.
  IF (LORILAM) THEN
    CORGANIC = "MPMPO"
    LVARSIGI = .TRUE.
    LVARSIGJ = .TRUE.
    CALL CH_AER_INIT_SOA(ILUOUT0, KVERB)
  END IF
  ! initialise NSV_* variables
  CALL INI_NSV(1)
    DEALLOCATE(XSV_LS)
    ALLOCATE (XSV_LS(IIU,IJU,levlen,NSV))
   XSV_LS(:,:,:,:) = 0.
!
  WRITE (ILUOUT0,'(A,A4,A)') ' | Reading MOZART species (ppp) from ',HFILE,' file'

where (ZLONOUT(:) < 0.) ZLONOUT(:) = ZLONOUT(:) + 360.
!
ALLOCATE(ZVALUE(levlen,KILEN))
ALLOCATE(ZOUT(levlen,INO))
ALLOCATE(ZVALUE1D(KILEN))
ALLOCATE(ZOUT1D(INO))

!
!*       2.6.1  read MOZART species from the file MOZ1.nam
!
! open input file
CALL CH_OPEN_INPUT(YMOZ,"MOZ2MESONH",ICHANNEL,ILUOUT0,KVERB)
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
         status = nf_inq_varid(ncid, trim(YCHANGE(JI,1)), ind_netcdf)
         if (status /= nf_noerr) call handle_err(status)
         status = nf_get_vara_double(ncid,ind_netcdf, start3d, count3d, &
                                                            vartemp3d)
         if (status /= nf_noerr) call  handle_err(status)
         DO JJ=1,levlen ! lev, lat, lon
           ZCHEMMOZ(:,:,JJ)=ZCOEFMOZART(JI,1)*vartemp3d(:,:,levlen+1-JJ)
         ENDDO
       ELSE IF (ISPCMOZ(JI)==2) THEN
         status = nf_inq_varid(ncid, trim(YCHANGE(JI,1)), ind_netcdf)
         if (status /= nf_noerr) call handle_err(status)
         status = nf_get_vara_double(ncid,ind_netcdf, start3d, count3d, &
                                                               vartemp3d)
         if (status /= nf_noerr) call  handle_err(status)
         status = nf_inq_varid(ncid, trim(YCHANGE(JI,2)), ind_netcdf)
         if (status /= nf_noerr) call handle_err(status)
         status = nf_get_vara_double(ncid,ind_netcdf, start3d, count3d, &
                                                            vartemp3dbis)
         if (status /= nf_noerr) call  handle_err(status)
         DO JJ=1,levlen ! lev, lat, lon
           ZCHEMMOZ(:,:,JJ)=ZCOEFMOZART(JI,1)*vartemp3d(:,:,levlen+1-JJ) + &
                               ZCOEFMOZART(JI,2)*vartemp3dbis(:,:,levlen+1-JJ) 
         ENDDO
       ELSE IF (ISPCMOZ(JI)==3) THEN
         status = nf_inq_varid(ncid, trim(YCHANGE(JI,1)), ind_netcdf)
         if (status /= nf_noerr) call handle_err(status)
         status = nf_get_vara_double(ncid,ind_netcdf, start3d, count3d, &
                                                               vartemp3d)
         if (status /= nf_noerr) call  handle_err(status)
         status = nf_inq_varid(ncid, trim(YCHANGE(JI,2)), ind_netcdf)
         if (status /= nf_noerr) call handle_err(status)
         status = nf_get_vara_double(ncid,ind_netcdf, start3d, count3d, &
                                                            vartemp3dbis)
         if (status /= nf_noerr) call  handle_err(status)
         status = nf_inq_varid(ncid, trim(YCHANGE(JI,3)), ind_netcdf)
         if (status /= nf_noerr) call handle_err(status)
         status = nf_get_vara_double(ncid,ind_netcdf, start3d, count3d, &
                                                            vartemp3dter)
         if (status /= nf_noerr) call  handle_err(status)
         DO JJ=1,levlen ! lev, lat, lon
           ZCHEMMOZ(:,:,JJ)=ZCOEFMOZART(JI,1)*vartemp3d(:,:,levlen+1-JJ)+&
                            ZCOEFMOZART(JI,2)*vartemp3dbis(:,:,levlen+1-JJ)+&
                            ZCOEFMOZART(JI,3)*vartemp3dter(:,:,levlen+1-JJ)
         ENDDO
       ELSE IF (ISPCMOZ(JI)==4) THEN
         status = nf_inq_varid(ncid, trim(YCHANGE(JI,1)), ind_netcdf)
         if (status /= nf_noerr) call handle_err(status)
         status = nf_get_vara_double(ncid,ind_netcdf, start3d, count3d, &
                                                               vartemp3d)
         if (status /= nf_noerr) call  handle_err(status)
         status = nf_inq_varid(ncid, trim(YCHANGE(JI,2)), ind_netcdf)
         if (status /= nf_noerr) call handle_err(status)
         status = nf_get_vara_double(ncid,ind_netcdf, start3d, count3d, &
                                                            vartemp3dbis)
         if (status /= nf_noerr) call  handle_err(status)
         status = nf_inq_varid(ncid, trim(YCHANGE(JI,3)), ind_netcdf)
         if (status /= nf_noerr) call handle_err(status)
         status = nf_get_vara_double(ncid,ind_netcdf, start3d, count3d, &
                                                            vartemp3dter)
         if (status /= nf_noerr) call  handle_err(status)
         status = nf_inq_varid(ncid, trim(YCHANGE(JI,4)), ind_netcdf)
         if (status /= nf_noerr) call handle_err(status)
         status = nf_get_vara_double(ncid,ind_netcdf, start3d, count3d, &
                                                         vartemp3dquater)
         if (status /= nf_noerr) call  handle_err(status)
         DO JJ=1,levlen ! lev, lat, lon
           ZCHEMMOZ(:,:,JJ)=ZCOEFMOZART(JI,1)*vartemp3d(:,:,levlen+1-JJ)+&
                               ZCOEFMOZART(JI,2)*vartemp3dbis(:,:,levlen+1-JJ)+&
                               ZCOEFMOZART(JI,3)*vartemp3dter(:,:,levlen+1-JJ)+&
                               ZCOEFMOZART(JI,4)*vartemp3dquater(:,:,levlen+1-JJ)
         ENDDO
       ENDIF
       DO JK = 1, levlen
         JLOOP1 = 0
         DO JJ = 1, latlen
           ZVALUE(JK,JLOOP1+1:JLOOP1+lonlen) = ZCHEMMOZ(1:lonlen,JJ,JK)
           JLOOP1 = JLOOP1+lonlen
         ENDDO                                                                                           
         CALL HORIBL(lats(1),lons(1),lats(latlen),lons(lonlen), &
                     latlen,kinlo,KILEN,                        &
                     ZVALUE(JK,:),INO,ZLONOUT,ZLATOUT,          &
                     ZOUT(JK,:),.FALSE.,PTIME_HORI,.TRUE.)
         CALL ARRAY_1D_TO_2D(INO,ZOUT(JK,:),IIU,IJU, &
                             XSV_LS(:,:,JK,JNCHEM)   )
       ENDDO  ! levlen
    ENDIF      

  ENDDO ! JNCHEM
  DO JNAER = NSV_AERBEG, NSV_AEREND 
    IF (trim(CAERONAMES(JNAER-NSV_AERBEG+1))==trim(YSPCMNH(JI))) THEN !MNH mechanism species
       IF (ISPCMOZ(JI)==1) THEN
         status = nf_inq_varid(ncid, trim(YCHANGE(JI,1)), ind_netcdf)
         if (status /= nf_noerr) call handle_err(status)
         status = nf_get_vara_double(ncid,ind_netcdf, start3d, count3d, &
                                                            vartemp3d)
         if (status /= nf_noerr) call  handle_err(status)
         DO JJ=1,levlen ! lev, lat, lon
           ZCHEMMOZ(:,:,JJ)=ZCOEFMOZART(JI,1)*vartemp3d(:,:,levlen+1-JJ)
         ENDDO
       ELSE IF (ISPCMOZ(JI)==2) THEN
         status = nf_inq_varid(ncid, trim(YCHANGE(JI,1)), ind_netcdf)
         if (status /= nf_noerr) call handle_err(status)
         status = nf_get_vara_double(ncid,ind_netcdf, start3d, count3d, &
                                                               vartemp3d)
         if (status /= nf_noerr) call  handle_err(status)
         status = nf_inq_varid(ncid, trim(YCHANGE(JI,2)), ind_netcdf)
         if (status /= nf_noerr) call handle_err(status)
         status = nf_get_vara_double(ncid,ind_netcdf, start3d, count3d, &
                                                            vartemp3dbis)
         if (status /= nf_noerr) call  handle_err(status)
         DO JJ=1,levlen ! lev, lat, lon
           ZCHEMMOZ(:,:,JJ)=ZCOEFMOZART(JI,1)*vartemp3d(:,:,levlen+1-JJ) + &
                               ZCOEFMOZART(JI,2)*vartemp3dbis(:,:,levlen+1-JJ) 
         ENDDO
       ELSE IF (ISPCMOZ(JI)==3) THEN
         status = nf_inq_varid(ncid, trim(YCHANGE(JI,1)), ind_netcdf)
         if (status /= nf_noerr) call handle_err(status)
         status = nf_get_vara_double(ncid,ind_netcdf, start3d, count3d, &
                                                               vartemp3d)
         if (status /= nf_noerr) call  handle_err(status)
         status = nf_inq_varid(ncid, trim(YCHANGE(JI,2)), ind_netcdf)
         if (status /= nf_noerr) call handle_err(status)
         status = nf_get_vara_double(ncid,ind_netcdf, start3d, count3d, &
                                                            vartemp3dbis)
         if (status /= nf_noerr) call  handle_err(status)
         status = nf_inq_varid(ncid, trim(YCHANGE(JI,3)), ind_netcdf)
         if (status /= nf_noerr) call handle_err(status)
         status = nf_get_vara_double(ncid,ind_netcdf, start3d, count3d, &
                                                            vartemp3dter)
         if (status /= nf_noerr) call  handle_err(status)
         DO JJ=1,levlen ! lev, lat, lon
           ZCHEMMOZ(:,:,JJ)=ZCOEFMOZART(JI,1)*vartemp3d(:,:,levlen+1-JJ)+&
                            ZCOEFMOZART(JI,2)*vartemp3dbis(:,:,levlen+1-JJ)+&
                            ZCOEFMOZART(JI,3)*vartemp3dter(:,:,levlen+1-JJ)
         ENDDO
       ELSE IF (ISPCMOZ(JI)==4) THEN
         status = nf_inq_varid(ncid, trim(YCHANGE(JI,1)), ind_netcdf)
         if (status /= nf_noerr) call handle_err(status)
         status = nf_get_vara_double(ncid,ind_netcdf, start3d, count3d, &
                                                               vartemp3d)
         if (status /= nf_noerr) call  handle_err(status)
         status = nf_inq_varid(ncid, trim(YCHANGE(JI,2)), ind_netcdf)
         if (status /= nf_noerr) call handle_err(status)
         status = nf_get_vara_double(ncid,ind_netcdf, start3d, count3d, &
                                                            vartemp3dbis)
         if (status /= nf_noerr) call  handle_err(status)
         status = nf_inq_varid(ncid, trim(YCHANGE(JI,3)), ind_netcdf)
         if (status /= nf_noerr) call handle_err(status)
         status = nf_get_vara_double(ncid,ind_netcdf, start3d, count3d, &
                                                            vartemp3dter)
         if (status /= nf_noerr) call  handle_err(status)
         status = nf_inq_varid(ncid, trim(YCHANGE(JI,4)), ind_netcdf)
         if (status /= nf_noerr) call handle_err(status)
         status = nf_get_vara_double(ncid,ind_netcdf, start3d, count3d, &
                                                         vartemp3dquater)
         if (status /= nf_noerr) call  handle_err(status)
         DO JJ=1,levlen ! lev, lat, lon
           ZCHEMMOZ(:,:,JJ)=ZCOEFMOZART(JI,1)*vartemp3d(:,:,levlen+1-JJ)+&
                               ZCOEFMOZART(JI,2)*vartemp3dbis(:,:,levlen+1-JJ)+&
                               ZCOEFMOZART(JI,3)*vartemp3dter(:,:,levlen+1-JJ)+&
                               ZCOEFMOZART(JI,4)*vartemp3dquater(:,:,levlen+1-JJ)
         ENDDO
       ENDIF
       DO JK = 1, levlen
         JLOOP1 = 0
         DO JJ = 1, latlen
           ZVALUE(JK,JLOOP1+1:JLOOP1+lonlen) = ZCHEMMOZ(1:lonlen,JJ,JK)
           JLOOP1 = JLOOP1+lonlen
         ENDDO                                                                                           
         CALL HORIBL(lats(1),lons(1),lats(latlen),lons(lonlen), &
                     latlen,kinlo,KILEN,                        &
                     ZVALUE(JK,:),INO,ZLONOUT,ZLATOUT,          &
                     ZOUT(JK,:),.FALSE.,PTIME_HORI,.TRUE.)
         CALL ARRAY_1D_TO_2D(INO,ZOUT(JK,:),IIU,IJU, &
                             XSV_LS(:,:,JK,JNAER)   )
       ENDDO  ! levlen
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
DO JK = 1, levlen
  JLOOP1 = 0
  DO JJ = 1, latlen
    ZVALUE(JK,JLOOP1+1:JLOOP1+lonlen) = TMOZ(1:lonlen,JJ,JK)
    JLOOP1 = JLOOP1 + lonlen
  ENDDO
  CALL HORIBL(lats(1),lons(1),lats(latlen),lons(lonlen), &
              latlen,kinlo,KILEN,                        &
              ZVALUE(JK,:),INO,ZLONOUT,ZLATOUT,          &
              ZOUT(JK,:),.FALSE.,PTIME_HORI,.FALSE.)
!
  CALL ARRAY_1D_TO_2D(INO,ZOUT(JK,:),IIU,IJU, &
                      XT_SV_LS(:,:,JK))
ENDDO 
XT_SV_LS(:,:,:) = MAX(XT_SV_LS(:,:,:),0.)
!
DO JK = 1, levlen
  JLOOP1 = 0
  DO JJ = 1, latlen
    ZVALUE(JK,JLOOP1+1:JLOOP1+lonlen) = QMOZ(1:lonlen,JJ,JK)
    JLOOP1 = JLOOP1 + lonlen
  ENDDO
  CALL HORIBL(lats(1),lons(1),lats(latlen),lons(lonlen), &
              latlen,kinlo,KILEN,                                &
              ZVALUE(JK,:),INO,ZLONOUT,ZLATOUT,                  &
              ZOUT(JK,:),.FALSE.,PTIME_HORI,.FALSE.)
!
   CALL ARRAY_1D_TO_2D(INO,ZOUT(JK,:),IIU,IJU,                    &
                       XQ_SV_LS(:,:,JK,1))
ENDDO 
XQ_SV_LS(:,:,:,1) = MAX(XQ_SV_LS(:,:,:,1),0.)
!
JLOOP1 = 0
DO JJ = 1, latlen
  ZVALUE1D(JLOOP1+1:JLOOP1+lonlen) = PSMOZ(1:lonlen,JJ)
  JLOOP1 = JLOOP1 + lonlen
ENDDO
CALL HORIBL(lats(1),lons(1),lats(latlen),lons(lonlen), &
            latlen,kinlo,KILEN,                                &
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
!nf_close
status = nf_close(ncid) 
if (status /= nf_noerr) call handle_err(status)
!
  DEALLOCATE (ZVALUE)
  DEALLOCATE (ZOUT)
  DEALLOCATE (ZVALUE1D) 
  DEALLOCATE (ZOUT1D)
!!

! close
! file
CALL CLOSE_ll(YMOZ,IOSTAT=IRET)


!-------------------------------------------------------------
!
!* 4. VERTICAL GRID
!
!* 4.1 Read VERTICAL GRID
!
WRITE (ILUOUT0,'(A)') ' | Reading of vertical grid in progress'
CALL READ_VER_GRID(HPRE_REAL1)
!
!--------------------------------------------------------------
!
!* 4.2 Interpolate on Meso-NH VERTICAL GRID
!
!* 4.3 Free all temporary allocations
!
DEALLOCATE (ZLATOUT)
DEALLOCATE (ZLONOUT)
DEALLOCATE (hyam)
DEALLOCATE (hybm)
DEALLOCATE (vartemp3d)
DEALLOCATE (vartemp3dbis)
DEALLOCATE (vartemp3dter)
DEALLOCATE (vartemp3dquater)
!
WRITE (ILUOUT0,'(A,A4,A)') ' -- netcdf decoder for ',HFILE,' file ended successfully'
!
!
CONTAINS
!
!     #############################
      SUBROUTINE HANDLE_ERR(STATUS)
!     #############################
     INTEGER STATUS
     IF (STATUS .NE. NF_NOERR) THEN
        PRINT *, NF_STRERROR(STATUS)
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
  WRITE (ILUOUT0,'(A)') ' | Error in "ARRAY_1D_TO_2D", sizes do not match - abort'
 !callabortstop
  CALL CLOSE_ll(CLUOUT0,IOSTAT=IRESP)
  CALL ABORT
  STOP
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
