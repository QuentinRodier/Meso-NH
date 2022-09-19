!MNH_LIC Copyright 2012-2021 CNRS, Meteo-France and Universite Paul Sabatier
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
!!****  *READ_CAMS_DATA_NETCDF_CASE* - reads data for the initialization of real cases.
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
!!      Original    06/2021 forked from read_chem_data_netcdf_case.f90

!-------------------------------------------------------------------------------
!
!*      0. DECLARATIONS
!------------
!
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
USE MODD_NETCDF,     ONLY:CDFINT
USE MODD_NSV  
USE MODD_PARAMETERS
USE MODD_PARAM_n,    ONLY : CTURB
USE MODD_PRECISION, ONLY:CDFINT
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
                            NMOD_IFN, NMOD_IMM, LHHONI, NINDICE_CCN_IMM,CCCN_MODES,&
                            CIFN_SPECIES
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
INTEGER                            :: JJ            ! Dummy counters
INTEGER                            :: JLOOP1
! Variables used by the PGD reader
CHARACTER(LEN=28)                  :: YPGD_NAME     ! not used - dummy argument
CHARACTER(LEN=28)                  :: YPGD_DAD_NAME ! not used - dummy argument
CHARACTER(LEN=2)                   :: YPGD_TYPE     ! not used - dummy argument
! PGD Grib definition variables
INTEGER                            :: INO           ! Number of points of the grid
INTEGER                            :: IIU           ! Number of points along X
INTEGER                            :: IJU           ! Number of points along Y
integer                            :: ilatlen, ilonlen, ilevlen
REAL, DIMENSION(:), ALLOCATABLE    :: ZLONOUT       ! mapping PGD -> Grib (lon.)
REAL, DIMENSION(:), ALLOCATABLE    :: ZLATOUT       ! mapping PGD -> Grib (lat.)
REAL, DIMENSION(:,:), ALLOCATABLE  :: ZXM           ! X of PGD mass points
REAL, DIMENSION(:,:), ALLOCATABLE  :: ZYM           ! Y of PGD mass points
REAL, DIMENSION(:,:), ALLOCATABLE  :: ZLATM         ! Lat of PGD mass points
REAL, DIMENSION(:,:), ALLOCATABLE  :: ZLONM         ! Lon of PGD mass points
INTEGER                           :: IMI
!
! For netcdf 
!
integer(kind=CDFINT) :: istatus, incid
integer(kind=CDFINT) :: itimeindex
INTEGER(kind=CDFINT)               :: ind_netcdf    ! Indice for netcdf var.
REAL, DIMENSION(:), ALLOCATABLE       :: zlats
REAL, DIMENSION(:), ALLOCATABLE       :: zlons 
REAL, DIMENSION(:), ALLOCATABLE       :: zlevs 
REAL, DIMENSION(:,:,:), ALLOCATABLE   :: zmmr_dust1, zmmr_dust2, zmmr_dust3
REAL, DIMENSION(:,:,:), ALLOCATABLE   :: zmmr_seasalt1, zmmr_seasalt2, zmmr_seasalt3
REAL, DIMENSION(:,:,:), ALLOCATABLE   :: zmmr_bc_hydrophilic, zmmr_bc_hydrophobic
REAL, DIMENSION(:,:,:), ALLOCATABLE   :: zmmr_oc_hydrophilic, zmmr_oc_hydrophobic
REAL, DIMENSION(:,:,:), ALLOCATABLE   :: zmmr_sulfaer
REAL, DIMENSION(:,:,:), ALLOCATABLE   :: ZWORK
REAL, DIMENSION(:,:,:), ALLOCATABLE   :: ZTCAMS, ZQCAMS
REAL, DIMENSION(:,:), ALLOCATABLE     :: ZPSCAMS
REAL, DIMENSION(:), ALLOCATABLE       :: ZTMP1, ZTMP2
REAL, DIMENSION(:,:,:), ALLOCATABLE   :: ZTMP3
REAL, DIMENSION(:,:,:,:), ALLOCATABLE :: ZTMP4,ZTMP5
!----------------------------------------------------------------------
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
DEALLOCATE (ZLONM)
DEALLOCATE (ZLATM)
!
!
!* 2. READ NETCDF FIELDS
!     ------------------
!
! 2.1 Open netcdf files
!
istatus = nf90_open(HFILE, nf90_nowrite, incid) 
if (istatus /= nf90_noerr) call handle_err(istatus)
!
! 2.2 Read netcdf files
!
! get dimensions
!
CALL READ_DIM(incid,"latitude",ilatlen)
CALL READ_DIM(incid,"longitude",ilonlen)
CALL READ_DIM(incid,"level",ilevlen)
!
! 2.3 Read data.
!
ALLOCATE (zlats(ilatlen))
ALLOCATE (zlons(ilonlen))
ALLOCATE (zlevs(ilevlen))
! T, Q, Ps :
ALLOCATE (ZTCAMS(ilonlen,ilatlen,ilevlen))
ALLOCATE (ZQCAMS(ilonlen,ilatlen,ilevlen))
ALLOCATE (ZPSCAMS(ilonlen,ilatlen))
! transformed a, b :
ALLOCATE (XA_SV_LS(ilevlen))
ALLOCATE (XB_SV_LS(ilevlen))
! meteo var
ALLOCATE (XT_SV_LS(IIU,IJU,ilevlen))
ALLOCATE (XQ_SV_LS(IIU,IJU,ilevlen,NRR))
ALLOCATE (XPS_SV_LS(IIU,IJU))
ALLOCATE (XZS_SV_LS(IIU,IJU))
! take the orography from ECMWF
XZS_SV_LS(:,:) = XZS_LS(:,:)
! aerosol mr from CAMS or MACC
ALLOCATE (zmmr_dust1(ilonlen,ilatlen,ilevlen))
ALLOCATE (zmmr_dust2(ilonlen,ilatlen,ilevlen))
ALLOCATE (zmmr_dust3(ilonlen,ilatlen,ilevlen))
!
ALLOCATE (zmmr_seasalt1(ilonlen,ilatlen,ilevlen))
ALLOCATE (zmmr_seasalt2(ilonlen,ilatlen,ilevlen))
ALLOCATE (zmmr_seasalt3(ilonlen,ilatlen,ilevlen))
!
ALLOCATE (zmmr_bc_hydrophilic(ilonlen,ilatlen,ilevlen))
ALLOCATE (zmmr_bc_hydrophobic(ilonlen,ilatlen,ilevlen))
!
ALLOCATE (zmmr_oc_hydrophilic(ilonlen,ilatlen,ilevlen))
ALLOCATE (zmmr_oc_hydrophobic(ilonlen,ilatlen,ilevlen))
!
ALLOCATE (zmmr_sulfaer(ilonlen,ilatlen,ilevlen))
!
ALLOCATE (ZWORK(ilonlen,ilatlen,ilevlen))
!
! get values of variables
!
!
! Reference pressure (needed for the vertical interpolation)
!
XP00_SV_LS = 101325.0
!
! a and b coefficients (needed for the vertical interpolation)
!
IF (ilevlen .eq. 60) THEN
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

ELSE IF (ilevlen .eq. 137) THEN

XA_SV_LS(:) = (/ &
2.000365 , 3.102241 , 4.666084 , 6.827977 , 9.746966 , 13.605424 , 18.608931 , 24.985718 , &
32.985710 , 42.879242 , 54.955463 , 69.520576 , 86.895882 , 107.415741 , 131.425507 , 159.279404 , &
191.338562 , 227.968948 , 269.539581 , 316.420746 , 368.982361 , 427.592499 , 492.616028 , 564.413452 , &
643.339905 , 729.744141 , 823.967834 , 926.344910 , 1037.201172 , 1156.853638 , 1285.610352 , 1423.770142 , &
1571.622925 , 1729.448975 , 1897.519287 , 2076.095947 , 2265.431641 , 2465.770508 , 2677.348145 , 2900.391357 , &
3135.119385 , 3381.743652 , 3640.468262 , 3911.490479 , 4194.930664 , 4490.817383 , 4799.149414 , 5119.895020 , &
5452.990723 , 5798.344727 , 6156.074219 , 6526.946777 , 6911.870605 , 7311.869141 , 7727.412109 , 8159.354004 , &
8608.525391 , 9076.400391 , 9562.682617 , 10065.978516 , 10584.631836 , 11116.662109 , 11660.067383 , 12211.547852 , &
12766.873047 , 13324.668945 , 13881.331055 , 14432.139648 , 14975.615234 , 15508.256836 , 16026.115234 , 16527.322266 , &
17008.789063 , 17467.613281 , 17901.621094 , 18308.433594 , 18685.718750 , 19031.289063 , 19343.511719 , 19620.042969 , &
19859.390625 , 20059.931641 , 20219.664063 , 20337.863281 , 20412.308594 , 20442.078125 , 20425.718750 , 20361.816406 , &
20249.511719 , 20087.085938 , 19874.025391 , 19608.572266 , 19290.226563 , 18917.460938 , 18489.707031 , 18006.925781 , &
17471.839844 , 16888.687500 , 16262.046875 , 15596.695313 , 14898.453125 , 14173.324219 , 13427.769531 , 12668.257813 , &
11901.339844 , 11133.304688 , 10370.175781 , 9617.515625 , 8880.453125 , 8163.375000 , 7470.343750 , 6804.421875 , &
6168.531250 , 5564.382813 , 4993.796875 , 4457.375000 , 3955.960938 , 3489.234375 , 3057.265625 , 2659.140625 , &
2294.242188 , 1961.500000 , 1659.476563 , 1387.546875 , 1143.250000 , 926.507813 , 734.992188 , 568.062500 , &
424.414063 , 302.476563 , 202.484375 , 122.101563 , 62.781250 , 22.835938 , 3.757813 , 0.000000 , 0.000000  /)

XA_SV_LS(:) = XA_SV_LS(:) / XP00_SV_LS

XB_SV_LS(:) = (/ &
0.000000 , 0.000000 , 0.000000 , 0.000000 , 0.000000 , 0.000000 , 0.000000 , 0.000000 , &
0.000000 , 0.000000 , 0.000000 , 0.000000 , 0.000000 , 0.000000 , 0.000000 , 0.000000 , &
0.000000 , 0.000000 , 0.000000 , 0.000000 , 0.000000 , 0.000000 , 0.000000 , 0.000000 , &
0.000000 , 0.000000 , 0.000000 , 0.000000 , 0.000000 , 0.000000 , 0.000000 , 0.000000 , &
0.000000 , 0.000000 , 0.000000 , 0.000000 , 0.000000 , 0.000000 , 0.000000 , 0.000000 , &
0.000000 , 0.000000 , 0.000000 , 0.000000 , 0.000000 , 0.000000 , 0.000000 , 0.000000 , &
0.000000 , 0.000000 , 0.000000 , 0.000000 , 0.000000 , 0.000000 , 0.000007 , 0.000024 , &
0.000059 , 0.000112 , 0.000199 , 0.000340 , 0.000562 , 0.000890 , 0.001353 , 0.001992 , &
0.002857 , 0.003971 , 0.005378 , 0.007133 , 0.009261 , 0.011806 , 0.014816 , 0.018318 , &
0.022355 , 0.026964 , 0.032176 , 0.038026 , 0.044548 , 0.051773 , 0.059728 , 0.068448 , &
0.077958 , 0.088286 , 0.099462 , 0.111505 , 0.124448 , 0.138313 , 0.153125 , 0.168910 , &
0.185689 , 0.203491 , 0.222333 , 0.242244 , 0.263242 , 0.285354 , 0.308598 , 0.332939 , &
0.358254 , 0.384363 , 0.411125 , 0.438391 , 0.466003 , 0.493800 , 0.521619 , 0.549301 , &
0.576692 , 0.603648 , 0.630036 , 0.655736 , 0.680643 , 0.704669 , 0.727739 , 0.749797 , &
0.770798 , 0.790717 , 0.809536 , 0.827256 , 0.843881 , 0.859432 , 0.873929 , 0.887408 , &
0.899900 , 0.911448 , 0.922096 , 0.931881 , 0.940860 , 0.949064 , 0.956550 , 0.963352 , &
0.969513 , 0.975078 , 0.980072 , 0.984542 , 0.988500 , 0.991984 , 0.995003 , 0.997630 , 1.000000 /)

END IF

CALL READ_VAR_1D(incid,"latitude",ilatlen,zlats)
CALL READ_VAR_1D(incid,"longitude",ilonlen,zlons)
CALL READ_VAR_1D(incid,"level",ilevlen,zlevs)

CALL READ_VAR_2D(incid,"sp",ilonlen,ilatlen,ZPSCAMS)

CALL READ_VAR_3D(incid,"t",ilonlen,ilatlen,ilevlen,ZTCAMS)
CALL READ_VAR_3D(incid,"q",ilonlen,ilatlen,ilevlen,ZQCAMS)

CALL READ_VAR_3D(incid,"aermr01",ilonlen,ilatlen,ilevlen,zmmr_seasalt1)
CALL READ_VAR_3D(incid,"aermr02",ilonlen,ilatlen,ilevlen,zmmr_seasalt2)
CALL READ_VAR_3D(incid,"aermr03",ilonlen,ilatlen,ilevlen,zmmr_seasalt3)
CALL READ_VAR_3D(incid,"aermr04",ilonlen,ilatlen,ilevlen,zmmr_dust1)
CALL READ_VAR_3D(incid,"aermr05",ilonlen,ilatlen,ilevlen,zmmr_dust2)
CALL READ_VAR_3D(incid,"aermr06",ilonlen,ilatlen,ilevlen,zmmr_dust3)
CALL READ_VAR_3D(incid,"aermr07",ilonlen,ilatlen,ilevlen,zmmr_oc_hydrophobic)
CALL READ_VAR_3D(incid,"aermr08",ilonlen,ilatlen,ilevlen,zmmr_oc_hydrophilic)
CALL READ_VAR_3D(incid,"aermr09",ilonlen,ilatlen,ilevlen,zmmr_bc_hydrophobic)
CALL READ_VAR_3D(incid,"aermr10",ilonlen,ilatlen,ilevlen,zmmr_bc_hydrophilic)
CALL READ_VAR_3D(incid,"aermr11",ilonlen,ilatlen,ilevlen,zmmr_sulfaer)
!
!------------------------------------------------------------------------
!* 3 Conversion of CAMS variables into LIMA variables
!---------------------------------------------------------------------
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
CCCN_MODES='CAMS_AIT'
CIFN_SPECIES='CAMS_AIT'
!
! Always initialize chemical scheme variables before INI_NSV call !
!
CALL CH_INIT_SCHEME_n(IMI,LUSECHAQ,LUSECHIC,LCH_PH,ILUOUT0,KVERB)
IF (LORILAM) THEN
   CORGANIC = "MPMPO"
   LVARSIGI = .TRUE.
   LVARSIGJ = .TRUE.
   CALL CH_AER_INIT_SOA(ILUOUT0, KVERB)
END IF
!
CALL INI_NSV(IMI)
ALLOCATE (XSV_LS_LIMA(IIU,IJU,ilevlen,NSV))
XSV_LS_LIMA(:,:,:,:) = 0.
!
NINDICE_CCN_IMM(1)=3
!
! Define work arrays
!
where (ZLONOUT(:) < 0.) ZLONOUT(:) = ZLONOUT(:) + 360. ! correct longitudes
!
!
! Select CAMS mixing ratios
! and perform the horizontal interpolation
!
! Free CCN concentration (mode 1)
!
ZWORK(:,:,:)=zmmr_seasalt1(:,:,:)+zmmr_seasalt2(:,:,:)+zmmr_seasalt3(:,:,:)
CALL INTERP_3D (ilonlen,ilatlen,ilevlen,ZWORK,zlats,zlons,IIU,IJU,ZLATOUT,ZLONOUT,PTIME_HORI, &
     XSV_LS_LIMA(:,:,:,NSV_LIMA_CCN_FREE))
!
! Free CCN concentration (mode 2)
!
ZWORK(:,:,:)=zmmr_sulfaer(:,:,:)
CALL INTERP_3D (ilonlen,ilatlen,ilevlen,ZWORK,zlats,zlons,IIU,IJU,ZLATOUT,ZLONOUT,PTIME_HORI, &
     XSV_LS_LIMA(:,:,:,NSV_LIMA_CCN_FREE + 1))
!
! Free CCN concentration (mode 3, IMM)
!
ZWORK(:,:,:)=zmmr_bc_hydrophilic(:,:,:)+zmmr_oc_hydrophilic(:,:,:)
CALL INTERP_3D (ilonlen,ilatlen,ilevlen,ZWORK,zlats,zlons,IIU,IJU,ZLATOUT,ZLONOUT,PTIME_HORI, &
     XSV_LS_LIMA(:,:,:,NSV_LIMA_CCN_FREE + 2))
!
! Free IFN concentration (mode 1)
!
ZWORK(:,:,:)=zmmr_dust1(:,:,:) + zmmr_dust2(:,:,:) + zmmr_dust3(:,:,:)
CALL INTERP_3D (ilonlen,ilatlen,ilevlen,ZWORK,zlats,zlons,IIU,IJU,ZLATOUT,ZLONOUT,PTIME_HORI, &
     XSV_LS_LIMA(:,:,:,NSV_LIMA_IFN_FREE))
!
! Free IFN concentration (mode 2)
!
ZWORK(:,:,:)=zmmr_bc_hydrophobic(:,:,:)+zmmr_oc_hydrophobic(:,:,:)
CALL INTERP_3D (ilonlen,ilatlen,ilevlen,ZWORK,zlats,zlons,IIU,IJU,ZLATOUT,ZLONOUT,PTIME_HORI, &
     XSV_LS_LIMA(:,:,:,NSV_LIMA_IFN_FREE + 1))
!
! Temperature (needed for the vertical interpolation) 
!
CALL INTERP_3D (ilonlen,ilatlen,ilevlen,ZTCAMS,zlats,zlons,IIU,IJU,ZLATOUT,ZLONOUT,PTIME_HORI,XT_SV_LS)
!
! Spec. Humidity (needed for the vertical interpolation) 
!
CALL INTERP_3D (ilonlen,ilatlen,ilevlen,ZQCAMS,zlats,zlons,IIU,IJU,ZLATOUT,ZLONOUT,PTIME_HORI,XQ_SV_LS(:,:,:,1))
!
! Surface pressure (needed for the vertical interpolation) 
!
CALL INTERP_2D (ilonlen,ilatlen,ZPSCAMS,zlats,zlons,IIU,IJU,ZLATOUT,ZLONOUT,PTIME_HORI,XPS_SV_LS)
!
! Correct negative values produced by the horizontal interpolations
!
XSV_LS_LIMA(:,:,:,:) = MAX(XSV_LS_LIMA(:,:,:,:),0.)
XPS_SV_LS(:,:)  = MAX(XPS_SV_LS(:,:),0.)
XT_SV_LS(:,:,:) = MAX(XT_SV_LS(:,:,:),0.)
XQ_SV_LS(:,:,:,1) = MAX(XQ_SV_LS(:,:,:,1),0.)
!
! If Netcdf vertical levels have to be reversed :
!
ALLOCATE(ZTMP1(ilevlen))
ALLOCATE(ZTMP2(ilevlen))
ALLOCATE(ZTMP3(IIU,IJU,ilevlen))
ALLOCATE(ZTMP4(IIU,IJU,ilevlen,NRR))
ALLOCATE(ZTMP5(IIU,IJU,ilevlen,NSV))
DO JJ=1,ilevlen
   ! inv. lev
   ZTMP1(JJ)       = XA_SV_LS(ilevlen+1-JJ)
   ZTMP2(JJ)       = XB_SV_LS(ilevlen+1-JJ)
   ZTMP3(:,:,JJ)   = XT_SV_LS(:,:,ilevlen+1-JJ)
   ZTMP4(:,:,JJ,1) = XQ_SV_LS(:,:,ilevlen+1-JJ,1)
   ZTMP5(:,:,JJ,:) = XSV_LS_LIMA(:,:,ilevlen+1-JJ,:)
ENDDO
XA_SV_LS(:)          = ZTMP1(:)
XB_SV_LS(:)          = ZTMP2(:)
XT_SV_LS(:,:,:)      = ZTMP3(:,:,:)
XQ_SV_LS(:,:,:,1)    = ZTMP4(:,:,:,1)
XSV_LS_LIMA(:,:,:,:) = ZTMP5(:,:,:,:)
DEALLOCATE(ZTMP1)
DEALLOCATE(ZTMP2)
DEALLOCATE(ZTMP3)
DEALLOCATE(ZTMP4)
DEALLOCATE(ZTMP5)
!
! close the netcdf file
istatus = nf90_close(incid) 
if (istatus /= nf90_noerr) call handle_err(istatus)
!
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
!* Free all temporary allocations
!
DEALLOCATE (ZLATOUT)
DEALLOCATE (ZLONOUT)
!
DEALLOCATE (zlats)
DEALLOCATE (zlons)
DEALLOCATE (zlevs)
! ps, T, Q :
DEALLOCATE (ZPSCAMS)
DEALLOCATE (ZTCAMS)
DEALLOCATE (ZQCAMS)
!
DEALLOCATE (zmmr_dust1)
DEALLOCATE (zmmr_dust2)
DEALLOCATE (zmmr_dust3)
!
DEALLOCATE (zmmr_seasalt1)
DEALLOCATE (zmmr_seasalt2)
DEALLOCATE (zmmr_seasalt3)
!
DEALLOCATE (zmmr_bc_hydrophilic)
DEALLOCATE (zmmr_bc_hydrophobic)
!
DEALLOCATE (zmmr_oc_hydrophilic)
DEALLOCATE (zmmr_oc_hydrophobic)
!
DEALLOCATE (zmmr_sulfaer)
!
DEALLOCATE (ZWORK)
!
WRITE (ILUOUT0,'(A,A4,A)') ' -- netcdf decoder for ',HFILE,' file ended successfully'
WRITE (ILUOUT0,'(A,A4,A)') 'CAMS mixing ratios are interpolated horizontally'
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
!     #############################################
      SUBROUTINE READ_DIM (file,name,output)
!     #############################################
!
!       Small routine used to store a linear array into a 2 dimension array
!
IMPLICIT NONE
INTEGER(kind=CDFINT),   INTENT(IN)  :: file
CHARACTER(*),           INTENT(IN)  :: name
INTEGER,                INTENT(OUT) :: output
!
INTEGER(kind=CDFINT) :: ilen
INTEGER(kind=CDFINT) :: istatus, index
!
istatus = nf90_inq_dimid(file, name, index)
if (istatus /= nf90_noerr) call handle_err(istatus)
istatus = nf90_inquire_dimension(file, index, len=ilen)
if (istatus /= nf90_noerr) call handle_err(istatus)
!
output = ilen
!
END SUBROUTINE READ_DIM
!
!     #############################################
      SUBROUTINE READ_VAR_1D (file,name,size,output)
!     #############################################
!
!       Small routine used to store a linear array into a 2 dimension array
!
IMPLICIT NONE
INTEGER(kind=CDFINT),   INTENT(IN)  :: file
CHARACTER(*),           INTENT(IN)  :: name
INTEGER,                INTENT(IN)  :: size
REAL, DIMENSION(size),  INTENT(INOUT) :: output
!
INTEGER(kind=CDFINT) :: istatus, index
!
istatus = nf90_inq_varid(file, name, index)
if (istatus /= nf90_noerr) call handle_err(istatus)
istatus = nf90_get_var(file, index, output)
if (istatus /= nf90_noerr) call handle_err(istatus)
!
END SUBROUTINE READ_VAR_1D
!
!     #############################################
      SUBROUTINE READ_VAR_2D (file,name,size_lon,size_lat,output)
!     #############################################
!
!       Small routine used to store a linear array into a 2 dimension array
!
IMPLICIT NONE
INTEGER(kind=CDFINT),   INTENT(IN)  :: file
CHARACTER(*),           INTENT(IN)  :: name
INTEGER,                INTENT(IN)  :: size_lon
INTEGER,                INTENT(IN)  :: size_lat
REAL, DIMENSION(size_lon,size_lat),      INTENT(INOUT) :: output
!
INTEGER(kind=CDFINT) :: istatus, index
REAL :: scale, offset
INTEGER,DIMENSION(4) :: s, c
!
s(:)=1
c(1)=size_lon
c(2)=size_lat
c(3)=1
c(4)=1
istatus = nf90_inq_varid(file, name, index)
if (istatus /= nf90_noerr) call handle_err(istatus)
istatus = nf90_get_var(file, index, output)
if (istatus /= nf90_noerr) call handle_err(istatus)
istatus = nf90_get_att(file, index, "scale_factor", scale) 
istatus = nf90_get_att(file, index, "add_offset", offset)
output = offset + scale * output
!
END SUBROUTINE READ_VAR_2D
!
!     #############################################
      SUBROUTINE READ_VAR_3D (file,name,size_lon,size_lat,size_lev,output)
!     #############################################
!
!       Small routine used to store a linear array into a 2 dimension array
!
IMPLICIT NONE
INTEGER(kind=CDFINT),   INTENT(IN)  :: file
CHARACTER(*),           INTENT(IN)  :: name
INTEGER,                INTENT(IN)  :: size_lon
INTEGER,                INTENT(IN)  :: size_lat
INTEGER,                INTENT(IN)  :: size_lev
REAL, DIMENSION(size_lon,size_lat,size_lev),      INTENT(INOUT) :: output
!
INTEGER(kind=CDFINT) :: istatus, index
REAL :: scale, offset
INTEGER(kind=CDFINT),DIMENSION(4) :: s, c
!
s(:)=1
c(1)=size_lon
c(2)=size_lat
c(3)=size_lev
c(4)=1
istatus = nf90_inq_varid(file, name, index)
if (istatus /= nf90_noerr) call handle_err(istatus)
istatus = nf90_get_var(file, index, output,start=s,count=c)
if (istatus /= nf90_noerr) call handle_err(istatus)
istatus = nf90_get_att(file, index, "scale_factor", scale) 
istatus = nf90_get_att(file, index, "add_offset", offset)
output = offset + scale * output
!
END SUBROUTINE READ_VAR_3D
!
!     #############################################
      SUBROUTINE INTERP_2D (size_lon,size_lat,input,zlats,zlons,IIU,IJU,PLATOUT,PLONOUT,PTIME_HORI,output)
!     #############################################
!
!       Small routine used to store a linear array into a 2 dimension array
!
IMPLICIT NONE
!
INTEGER,                INTENT(IN)  :: size_lon
INTEGER,                INTENT(IN)  :: size_lat
REAL, DIMENSION(size_lon,size_lat),      INTENT(IN) :: input
REAL, DIMENSION(size_lat),      INTENT(IN) :: zlats
REAL, DIMENSION(size_lon),      INTENT(IN) :: zlons
INTEGER,                INTENT(IN) :: IIU
INTEGER,                INTENT(IN) :: IJU
REAL, DIMENSION(IIU*IJU),      INTENT(IN) :: PLATOUT
REAL, DIMENSION(IIU*IJU),      INTENT(IN) :: PLONOUT
REAL,               INTENT(INOUT) :: PTIME_HORI
REAL, DIMENSION(IIU,IJU),      INTENT(INOUT) :: output
!
INTEGER :: JLOOP1, JJ, INO
REAL, DIMENSION(size_lat*size_lon) :: ZVALUE
REAL, DIMENSION(IIU*IJU) :: ZOUT
INTEGER, DIMENSION(size_lat) :: kinlo
INTEGER :: KILEN
!
kinlo(:)=size_lon
KILEN=size_lat*size_lon
INO=IIU*IJU
JLOOP1 = 0
DO JJ = 1, size_lat
   ZVALUE(JLOOP1+1:JLOOP1+size_lon) = input(1:size_lon,JJ)
   JLOOP1 = JLOOP1 + size_lon
ENDDO
CALL HORIBL(zlats(1),zlons(1),zlats(size_lat),zlons(size_lon), &
     size_lat,kinlo,KILEN,                                &
     ZVALUE(:),INO,PLONOUT,PLATOUT,                  &
     ZOUT(:),.FALSE.,PTIME_HORI,.TRUE. )
CALL ARRAY_1D_TO_2D(INO,ZOUT(:),IIU,IJU,output(:,:))
!
END SUBROUTINE INTERP_2D
!
!     #############################################
      SUBROUTINE INTERP_3D (size_lon,size_lat,size_lev,input,zlats,zlons,IIU,IJU,PLATOUT,PLONOUT,PTIME_HORI,output)
!     #############################################
!
!       Small routine used to store a linear array into a 2 dimension array
!
IMPLICIT NONE
!
INTEGER,                INTENT(IN)  :: size_lon
INTEGER,                INTENT(IN)  :: size_lat
INTEGER,                INTENT(IN)  :: size_lev
REAL, DIMENSION(size_lon,size_lat,size_lev),      INTENT(IN) :: input
REAL, DIMENSION(size_lat),      INTENT(IN) :: zlats
REAL, DIMENSION(size_lon),      INTENT(IN) :: zlons
INTEGER,                INTENT(IN) :: IIU
INTEGER,                INTENT(IN) :: IJU
REAL, DIMENSION(IIU*IJU),      INTENT(IN) :: PLATOUT
REAL, DIMENSION(IIU*IJU),      INTENT(IN) :: PLONOUT
REAL,               INTENT(INOUT) :: PTIME_HORI
REAL, DIMENSION(IIU,IJU,size_lev),      INTENT(INOUT) :: output
!
INTEGER :: JLOOP1, JJ, JK, INO
REAL, DIMENSION(size_lev,size_lat*size_lon) :: ZVALUE
REAL, DIMENSION(size_lev,IIU*IJU) :: ZOUT
INTEGER, DIMENSION(size_lat) :: kinlo
INTEGER :: KILEN
!
kinlo(:)=size_lon
KILEN=size_lat*size_lon
INO=IIU*IJU
DO JK = 1, ilevlen
   JLOOP1 = 0
   DO JJ = 1, size_lat
      ZVALUE(JK,JLOOP1+1:JLOOP1+size_lon) = input(1:size_lon,JJ,JK)
      JLOOP1 = JLOOP1 + size_lon
   ENDDO
   CALL HORIBL(zlats(1),zlons(1),zlats(size_lat),zlons(size_lon), &
        size_lat,kinlo,KILEN,                                &
        ZVALUE(JK,:),INO,PLONOUT,PLATOUT,                  &
        ZOUT(JK,:),.FALSE.,PTIME_HORI,.TRUE. )
   CALL ARRAY_1D_TO_2D(INO,ZOUT(JK,:),IIU,IJU,output(:,:,JK))
ENDDO
!
END SUBROUTINE INTERP_3D
!
END SUBROUTINE READ_LIMA_DATA_NETCDF_CASE
