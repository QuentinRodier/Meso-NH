!MNH_LIC Copyright 2003-2024 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!    ########################
     MODULE MODI_CALL_RTTOV13
!    ########################
INTERFACE
!
     SUBROUTINE CALL_RTTOV13(KDLON, KFLEV, PEMIS, PTSRAD,   &
                PTHT, PRT, PPABST, PZZ, PMFCONV, PCLDFR, PULVLKB, PVLVLKB,  &
                OUSERI, KRTTOVINFO, TPFILE    )
!
USE MODD_IO, ONLY: TFILEDATA
!
INTEGER, INTENT(IN)   :: KDLON !number of columns where the
                               !radiation calculations are performed
INTEGER, INTENT(IN)   :: KFLEV !number of vertical levels where the
                               !radiation calculations are performed
!
!
REAL, DIMENSION(:,:),     INTENT(IN) :: PEMIS  !Surface IR EMISsivity
REAL, DIMENSION(:,:),     INTENT(IN) :: PTSRAD !RADiative Surface Temperature
!
REAL, DIMENSION(:,:,:),   INTENT(IN) :: PTHT   !THeta at t
REAL, DIMENSION(:,:,:,:), INTENT(IN) :: PRT    !moist variables at t
REAL, DIMENSION(:,:,:),   INTENT(IN) :: PPABST !pressure at t
REAL, DIMENSION(:,:,:),   INTENT(IN) :: PZZ    !Model level heights
!
REAL, DIMENSION(:,:,:),   INTENT(IN) :: PMFCONV! convective mass flux (kg /s m^2)
REAL, DIMENSION(:,:,:),   INTENT(IN) :: PCLDFR  ! cloud fraction
REAL, DIMENSION(:,:),     INTENT(IN) :: PULVLKB ! U-wind at KB level
REAL, DIMENSION(:,:),     INTENT(IN) :: PVLVLKB ! V-wind at KB level
!
LOGICAL, INTENT(IN)                  :: OUSERI ! logical switch to compute both
                                               ! liquid and solid condensate (OUSERI=.TRUE.)
                                               ! or only liquid condensate (OUSERI=.FALSE.)
!
INTEGER, DIMENSION(:,:), INTENT(IN) :: KRTTOVINFO ! platform, satellite, sensor,
                                                  ! and selection calculations
TYPE(TFILEDATA),   INTENT(IN) :: TPFILE ! File characteristics
!
END SUBROUTINE CALL_RTTOV13
END INTERFACE
END MODULE MODI_CALL_RTTOV13
!    #####################################################################
SUBROUTINE CALL_RTTOV13(KDLON, KFLEV, PEMIS, PTSRAD,     &
           PTHT, PRT, PPABST, PZZ, PMFCONV, PCLDFR, PULVLKB, PVLVLKB,  &
           OUSERI, KRTTOVINFO, TPFILE    )
!    #####################################################################
!!
!!****  *CALL_RTTOV* - 
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!    See Chaboureau and Pinty, 2006
!!    Validation of a cirrus parameterization with Meteosat Second Generation
!!    observations. Geophys. Res. Let., doi:10.1029/2005GL024725
!!
!!    AUTHOR
!!    ------
!!      J.-P. Chaboureau       *L.A.*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    11/12/03
!!      JP Chaboureau 27/03/2008 Vectorization
!!      JP Chaboureau 02/11/2009 move GANGL deallocation outside the sensor loop
!!      J.Escobar     15/09/2015 WENO5 & JPHEXT <> 1 
!!      JP Chaboureau 09/04/2021 adapt to call RTTOV13
!!      JP Chaboureau 10/11/2022 set opts_scatt % lusercfrac to false for MW
!!----------------------------------------------------------------------------
!!
!!*       0.    DECLARATIONS
!!              ------------
!!
USE MODD_CST
USE MODD_PARAMETERS
USE MODD_GRID_n
USE MODD_IO, ONLY: TFILEDATA
USE MODD_FIELD, ONLY: TFIELDMETADATA, TYPEREAL, XDEFFILLVALUE
USE MODD_LUNIT_n
USE MODD_LBC_n
USE MODD_DEEP_CONVECTION_n
USE MODD_REF_n
USE MODD_RADIATIONS_n, ONLY : XSEA, XZENITH
USE MODD_TIME_n, ONLY: TDTCUR      ! Current Time and Date
!
USE MODN_CONF
!                                
USE MODI_SUNPOS_n
USE MODI_DETER_ANGLE
USE MODI_PINTER
!
USE MODE_IO_FIELD_WRITE, ONLY: IO_Field_write
USE MODE_ll
USE MODE_TOOLS_ll
USE MODE_MSG
USE MODE_POS
!
#ifdef MNH_RTTOV_13
USE rttov_const, ONLY :  errorstatus_success, &
       & sensor_id, sensor_id_ir, sensor_id_hi, sensor_id_mw, inst_name, &
       & platform_name, gas_unit_specconc, tmin, tmax, qmin, qmax, pmin, pmax, &
       & rad2deg, zenmaxv9, min_reflectivity
USE rttov_types
USE mod_rttov_brdf_atlas, ONLY : rttov_brdf_atlas_data
USE parkind1, ONLY: jpim, jprb, jplm
!
IMPLICIT NONE
!
! -----------------------------------------------------------------------------
#include "rttov_direct.interface"
#include "rttov_read_coefs.interface"
#include "rttov_alloc_transmission.interface"
#include "rttov_dealloc_coefs.interface"
#include "rttov_alloc_direct.interface"
#include "rttov_read_scattcoeffs.interface"
#include "rttov_dealloc_scattcoeffs.interface"
#include "rttov_scatt_setupindex.interface"
#include "rttov_scatt.interface"
#include "rttov_scatt_ad.interface"
#include "rttov_alloc_rad.interface"
#include "rttov_init_rad.interface"
#include "rttov_alloc_prof.interface"
#include "rttov_alloc_scatt_prof.interface"
#include "rttov_alloc_reflectivity.interface"
! Use BRDF atlas
#include "rttov_setup_brdf_atlas.interface"
#include "rttov_get_brdf.interface"
#include "rttov_deallocate_brdf_atlas.interface"
#endif
!!!
!!!*       0.1   DECLARATIONS OF DUMMY ARGUMENTS :
!!!
INTEGER, INTENT(IN)   :: KDLON   !number of columns where the
! radiation calculations are performed
INTEGER, INTENT(IN)   :: KFLEV   !number of vertical levels where the
! radiation calculations are performed
!!!
REAL, DIMENSION(:,:),     INTENT(IN) :: PEMIS  !Surface IR EMISsivity
REAL, DIMENSION(:,:),     INTENT(IN) :: PTSRAD !RADiative Surface Temperature
                                !
REAL, DIMENSION(:,:,:),   INTENT(IN) :: PTHT   !THeta at t
REAL, DIMENSION(:,:,:,:), INTENT(IN) :: PRT    !moist variables at t
REAL, DIMENSION(:,:,:),   INTENT(IN) :: PPABST !pressure at t
REAL, DIMENSION(:,:,:),   INTENT(IN) :: PZZ    !Model level heights
!!!
!!!
REAL, DIMENSION(:,:,:),   INTENT(IN) :: PMFCONV ! convective mass flux (kg /s m^2)
REAL, DIMENSION(:,:,:),   INTENT(IN) :: PCLDFR  ! cloud fraction
REAL, DIMENSION(:,:),     INTENT(IN) :: PULVLKB ! U-wind at KB level
REAL, DIMENSION(:,:),     INTENT(IN) :: PVLVLKB ! V-wind at KB level
!!!
LOGICAL, INTENT(IN)                  :: OUSERI ! logical switch to compute both
! liquid and solid condensate (OUSERI=.TRUE.)
! or only liquid condensate (OUSERI=.FALSE.)
!!!
INTEGER, DIMENSION(:,:), INTENT(IN) :: KRTTOVINFO ! platform, satellite, sensor,
                                                  ! and selection calculations
TYPE(TFILEDATA),   INTENT(IN) :: TPFILE ! File characteristics
!
#ifdef MNH_RTTOV_13
!!!
!!!*       0.2   DECLARATIONS OF LOCAL VARIABLES
!!!
!!!
LOGICAL(KIND=jplm)  :: thermal, solar, radar

INTEGER(KIND=jpim), PARAMETER :: nhydro_frac = 1 ! a single profile of cloud cover (for MW)
!
INTEGER :: JI,JJ,JK,JK1,JK2,JKRAD,JKF,JSAT,JC ! loop indexes
!
INTEGER :: IJSAT        ! number of columns/=NUNDEF which 
                        ! have to be treated in the table KRTTOVINFO(:,:)
INTEGER :: IIB,IIE      ! I index value of the first/last inner mass point
INTEGER :: IJB,IJE      ! J index value of the first/last inner mass point
INTEGER :: IKB,IKE      ! K index value of the first/last inner mass point
INTEGER :: IIU          ! array size for the first  index
INTEGER :: IJU          ! array size for the second index
INTEGER :: IKU          ! array size for the third  index
INTEGER :: IKR          ! real array size for the third  index
INTEGER (Kind=jpim) :: iwp_levels ! equal to IKR (call to rttov_scatt)
INTEGER :: IIJ          ! reformatted array index
INTEGER :: IKSTAE       ! level number of the STAndard atmosphere array
INTEGER :: IKUP         ! vertical level above which STAndard atmosphere data

REAL, DIMENSION(:,:,:,:), ALLOCATABLE   ::  ZREF
REAL, DIMENSION(:,:,:), ALLOCATABLE   ::  ZOUT
REAL, DIMENSION(:,:), ALLOCATABLE :: ZANTMP, ZUTH
REAL :: ZZH, zdeg_to_rad, zrad_to_deg, zbeta, zalpha

! Other arrays for zenithal solar angle 
LOGICAL                             :: LCOSZENOUT
REAL, DIMENSION(:,:),   ALLOCATABLE :: ZCOSZEN, ZSINZEN, ZAZIMSOL

! -----------------------------------------------------------------------------
REAL, DIMENSION(1) :: ZANGL, ZLON, ZLAT   !Satellite zenith angle, longitude, latitude (deg)
! -----------------------------------------------------------------------------
INTEGER, DIMENSION(:), ALLOCATABLE :: IMSURF   !Surface type index
                                
INTEGER :: IKFBOT, IKFTOP, INDEX, ISUM, JLEV, JCH, IWATER, ICAN
!  at the open of the file LFI routines 
CHARACTER(LEN=8)  :: YINST  
CHARACTER(LEN=5)  :: YBEG, YEND
CHARACTER(LEN=2)  :: YCHAN, YTWO   
CHARACTER(LEN=1)  :: YONE   
                               
INTEGER, PARAMETER :: JPPLAT=24

CHARACTER(LEN=3), DIMENSION(JPPLAT) :: YPLAT= (/ &
     'N  ','D  ','MET','GO ','GMS','FY2','TRM','ERS', &
     'EOS','MTP','ENV','MSG','FY1','ADS','MTS','CRL', &
     'JPS','GFT','STL','MTR','KAL','MOR','FY3','CMS' /)
CHARACTER(LEN=2), DIMENSION(2) :: YLBL_MVIRI = (/ 'WV', 'IR'/)
CHARACTER(LEN=3), DIMENSION(7) :: YLBL_SSMI = (/ &
     '19V','19H','22V','37V','37H','85V','85H'/)
CHARACTER(LEN=3), DIMENSION(9) :: YLBL_TMI = (/ &
     '10V','10H','19V','19H','22V','37V','37H','85V','85H'/)
CHARACTER(LEN=4), DIMENSION(5) :: YLBL_MHS = (/ &
     '8900','1570','1831','1833','1903'/)
CHARACTER(LEN=5), DIMENSION(12) :: YLBL_SEVIRI = (/ &
     'VIS06','VIS08','NIR16','IR039','WV062','WV073', &
     'IR087','IR097','IR108','IR120','IR134','HRV  '/)
CHARACTER(LEN=5), DIMENSION(19) :: YLBL_ABI = (/ &
     'VIS04','VIS06','VIS08','VIS14','VIS16','VIS22', &
     'IR039','IR062','IR069','IR073','IR085','IR096','IR103','IR112','IR123','IR133', &
     'HRV04','HRV08','HRV16'/)
CHARACTER(LEN=3), DIMENSION(4) :: YLBL_GOESI = (/ &
     '039', '067','107','120'/)
CHARACTER(LEN=2), DIMENSION(6) :: YLBL_SAPHIR = (/ &
     'S1','S2','S3','S4','S5','S6'/)
CHARACTER(LEN=4), DIMENSION(13) :: YLBL_ICI = (/ &
     '1837','1833','1832','243V','243H','3259','3253','3251','4487','4483','4481','664V','664H'/)
CHARACTER(LEN=4), DIMENSION(2) :: YLBL_DPR = (/ '13', '35' /)
CHARACTER(LEN=4), DIMENSION(1) :: YLBL_CPR = (/ '94' /)
CHARACTER(LEN=4), DIMENSION(13) :: YLBL_GMI = (/ &
     '10V','10H','18V','18H','23V','36V','36H','89V','89H','166V','166H','1833','1837'/)

! -----------------------------------------------------------------------------
LOGICAL (kind=jplm)       , ALLOCATABLE :: calcemis    (:) 
LOGICAL(KIND=jplm)        , ALLOCATABLE :: use_chan  (:,:)  ! Flags to specify channels to simulate
INTEGER (kind=jpim)       , ALLOCATABLE :: frequencies (:) 
TYPE (rttov_chanprof)     , ALLOCATABLE :: chanprof    (:)  ! Channel and profile indices
TYPE (rttov_profile)      , ALLOCATABLE :: profiles    (:)
TYPE (rttov_profile_cloud), ALLOCATABLE :: cld_profiles(:)
TYPE(rttov_emissivity)    , ALLOCATABLE :: emissivity  (:)  ! Input/output surface emissivity
TYPE(rttov_reflectivity)  , ALLOCATABLE :: reflectivity
LOGICAL(KIND=jplm)        , ALLOCATABLE :: calcrefl    (:)  ! Flag to indicate calculation of BRDF within RTTOV
TYPE(rttov_reflectance)   , ALLOCATABLE :: reflectance (:)  ! Input/output surface BRDF
TYPE(rttov_transmission)                :: transmission   ! Output transmittances
INTEGER(KIND=jpim) :: asw
INTEGER(jpim) :: run_gas_units = gas_unit_specconc ! mass mixing ratio [kg/kg] over wet air

integer (kind=jpim)        :: errorstatus
type (rttov_radiance)      :: radiance, radiance_k  
type (rttov_options)       :: opts     ! Defaults to everything optional switched off
type (rttov_options_scatt) :: opts_scatt
type (rttov_coefs     )    :: coefs
type (rttov_scatt_coef)    :: coef_scatt

TYPE(rttov_brdf_atlas_data)      :: brdf_atlas               ! Data structure for BRDF atlas

integer (kind=jpim) :: instrument (3)
integer (kind=jpim) :: ilev, iprof, ichan, nprof, nchannels, nlevels, nchanprof
real    (kind=jprb) :: zenangle
integer (kind=jpim), parameter :: fin = 10
character (len=256) :: outstring
! -----------------------------------------------------------------------------
CHARACTER(LEN=:), ALLOCATABLE :: YMNHNAME, YUNITS, YCOMMENT
REAL, DIMENSION(SIZE(PTHT,1),SIZE(PTHT,2),SIZE(PTHT,3)) :: ZTEMP
TYPE(TFIELDMETADATA) :: TZFIELD
!-------------------------------------------------------------------------------
!
!*       0.     ARRAYS BOUNDS INITIALIZATION
!
IIU=SIZE(PTHT,1)
IJU=SIZE(PTHT,2)
IKU=SIZE(PTHT,3)
CALL GET_INDICE_ll (IIB,IJB,IIE,IJE)
IKB=1+JPVEXT
IKE=IKU-JPVEXT

errorstatus = 0
nlevels=IKE-IKB+1
nprof=1
ZTEMP = PTHT * ( PPABST/XP00 ) ** (XRD/XCPD)                   
DO JSAT=1,SIZE(KRTTOVINFO,2)
  IF (KRTTOVINFO(1,JSAT) /= NUNDEF) THEN
    IJSAT = JSAT
  END IF
END DO

opts % interpolation % addinterp  = .TRUE.  ! Allow interpolation of input profile
opts % interpolation % interp_mode = 1      ! Set interpolation method
opts % config % do_checkinput = .TRUE.  
opts % config % verbose       = .FALSE.  ! Enable printing of warnings
opts_scatt % config % verbose = .FALSE. ! Disable printing of warnings
opts_scatt % lusercfrac = .FALSE.

LCOSZENOUT=.TRUE.
ALLOCATE(ZCOSZEN(IIU,IJU))
ALLOCATE(ZSINZEN(IIU,IJU))
ALLOCATE(ZAZIMSOL(IIU,IJU))
CALL SUNPOS_n ( XZENITH, ZCOSZEN, ZSINZEN, ZAZIMSOL )

! -----------------------------------------------------------------------------
!              *** LOOP OVER SENSORS ***
! -----------------------------------------------------------------------------
DO JSAT=1,IJSAT ! loop over sensors
 
  instrument(1)=KRTTOVINFO(1,JSAT)
  instrument(2)=KRTTOVINFO(2,JSAT)
  instrument(3)=KRTTOVINFO(3,JSAT)

  radar = .FALSE.
  IF( sensor_id( instrument(3) ) /= sensor_id_mw) THEN
    opts % rt_ir % addsolar         = .FALSE. ! Do not include solar radiation
    IF (KRTTOVINFO(4,JSAT)==1) THEN
      opts % rt_ir % addsolar       = .TRUE.  ! Include solar radiation
    END IF
    opts % rt_ir % addaerosl        = .FALSE. ! Do not include aerosol effects
    opts % rt_ir % addclouds        = .TRUE.  ! Include cloud effects
    opts % rt_ir % ir_scatt_model   = 2       ! Scattering model for emission source term:
                                              !   1 => DOM; 2 => Chou-scaling
    opts % rt_ir % vis_scatt_model  = 1       ! Scattering model for solar source term:
                                              !   1 => DOM; 2 => single-scattering; 3 => MFASIS
    opts % rt_ir % dom_nstreams     = 8       ! Number of streams for Discrete Ordinates (DOM)
    opts % rt_all % addrefrac       = .TRUE.  ! Include refraction in path calc
    opts % rt_all % ozone_data      = .FALSE. ! Set the relevant flag to .TRUE.
    opts % rt_all % co2_data        = .FALSE. !   when supplying a profile of the
    opts % rt_all % n2o_data        = .FALSE. !   given trace gas (ensure the
    opts % rt_all % ch4_data        = .FALSE. !   coef file supports the gas)
    opts % rt_all % co_data         = .FALSE. !
    opts % rt_all % so2_data        = .FALSE. !

    opts % rt_ir % user_cld_opt_param   = .FALSE.
  ELSE
    opts % rt_all % addrefrac       = .FALSE. ! Do not include refraction in path calc
    opts % rt_ir % addsolar         = .FALSE. ! Do not include solar radiation
    opts % rt_ir % addaerosl        = .FALSE. ! Do not include aerosol effects
    opts % rt_ir % addclouds        = .FALSE. ! Do not include cloud effects
    opts % rt_mw % clw_data         = .FALSE. ! Do not include cloud liquid water
    IF (KRTTOVINFO(3,JSAT).EQ.105.OR.KRTTOVINFO(3,JSAT).EQ.107) radar = .TRUE.
  END IF

! Read and initialise coefficients
! -----------------------------------------------------------------------------
  CALL rttov_read_coefs (errorstatus, coefs, opts, instrument=instrument)
  IF (errorstatus /= errorstatus_success) THEN
    WRITE(*,*) 'platform=',instrument(1),' sat_id=',instrument(2),' inst=',instrument(3)
    CALL PRINT_MSG(NVERB_FATAL,'GEN','CALL_RTTOV13','error rttov_readcoeffs')
  END IF
  
  IF (coefs%coef%id_sensor == sensor_id_mw) THEN
    CALL rttov_read_scattcoeffs (errorstatus, opts_scatt, coefs, coef_scatt, &
                                 file_coef='hydrotable_'//                   &
                                 TRIM(platform_name(instrument(1)))//'_'//   &
                                 TRIM(inst_name(instrument(3)))//'.dat')
    IF (errorstatus /= errorstatus_success) THEN
      WRITE(*,*) 'error rttov_readcoeffs :',errorstatus
      CALL PRINT_MSG(NVERB_FATAL,'GEN','CALL_RTTOV13','error rttov_read_scattcoeffs')
    END IF
  END IF

  IF (opts % rt_ir % addsolar) THEN
    ! Initialise the RTTOV BRDF atlas
    CALL rttov_setup_brdf_atlas(        &
                errorstatus,            &
                opts,                   &
                TDTCUR%nmonth,          &
                brdf_atlas,             &
                path='brdf_data',       & 
                coefs = coefs) ! If supplied the BRDF atlas is initialised for this sensor and
                               ! this makes the atlas much faster to access
    IF (errorstatus /= errorstatus_success) THEN
      WRITE(*,*) 'error initialising BRDF atlas'
      CALL PRINT_MSG(NVERB_FATAL,'GEN','CALL_RTTOV13','error rttov_setup_brdf_atlas')
    END IF
  END IF

  nchannels = coefs%coef%fmv_chn   ! number of channels on instrument
  nchanprof = nprof * nchannels    ! total channels to simulate

  IF (.NOT.radar) THEN
    ALLOCATE(ZOUT(IIU,IJU,nchanprof))
    ZOUT(:,:,:)=XDEFFILLVALUE
  ELSE
    ALLOCATE(ZREF(IIU,IJU,IKU,nchanprof))
    ZREF(:,:,:,:)=min_reflectivity
  END IF

  ALLOCATE (chanprof     (nchanprof))
  ALLOCATE (frequencies  (nchanprof))
  ALLOCATE (emissivity   (nchanprof))
  ALLOCATE (calcemis    (nchanprof))
  ALLOCATE (profiles     (nprof))
  IF (coefs%coef% id_sensor == sensor_id_mw) THEN
    ALLOCATE (cld_profiles (nprof))
  END IF

  IF (coefs%coef% id_sensor /= sensor_id_mw) THEN
    calcemis = .FALSE.
    ! Allocate arrays for surface reflectance
    ALLOCATE(calcrefl(nchanprof))
    ALLOCATE(reflectance(nchanprof))
    calcrefl = .TRUE.
    reflectance % refl_in = 0.0_JPRB
    ! Use default cloud top BRDF for simple cloud in VIS/NIR channels
    reflectance % refl_cloud_top = 0._jprb
    ! Let RTTOV provide diffuse surface reflectances
    reflectance % diffuse_refl_in = 0._jprb
  ELSE
    ! Request RTTOV / FASTEM to calculate surface emissivity
    calcemis = .TRUE.
    emissivity % emis_in = 0.0_JPRB
  END IF

  ! --------------------------------------------------------------------------
  ! 4. Build the list of profile/channel indices in chanprof
  ! --------------------------------------------------------------------------

  IF (coefs%coef% id_sensor /= sensor_id_mw) THEN
    DO JCH=1,nchanprof
      chanprof(JCH)%prof = 1
      chanprof(JCH)%chan = JCH
    END DO
  ELSE
    ALLOCATE(use_chan(nprof,coefs%coef%fmv_chn))
    use_chan(:,:) = .TRUE._jplm
    CALL rttov_scatt_setupindex ( &
          errorstatus,        &
          nprof,              &
          coefs%coef%fmv_chn, &
          coefs,              &
          coef_scatt,         &
          nchanprof,          &
          chanprof,           &
          frequencies,        &
          use_chan)
    IF (errorstatus /= errorstatus_success) THEN
      WRITE(*,*) 'error finding channels, frequencies and polarisations'
      CALL PRINT_MSG(NVERB_FATAL,'GEN','CALL_RTTOV13','error rttov_scatt_setupindex')
    END IF
  END IF

  asw = 1_jpim ! Switch for allocation passed into RTTOV subroutines

! Allocate profiles (input) and radiance (output) structures
  CALL rttov_alloc_prof(errorstatus, nprof, profiles, nlevels, opts, asw, coefs, init = .TRUE._jplm)
  IF (coefs%coef% id_sensor == sensor_id_mw) THEN
    cld_profiles(1)%nhydro = 5
    CALL rttov_alloc_scatt_prof(errorstatus, nprof, cld_profiles, nlevels, &
            cld_profiles(1)%nhydro, nhydro_frac, 1_jpim, init = .TRUE._jplm)
    IF (radar) THEN
       ALLOCATE(reflectivity)
       CALL rttov_alloc_reflectivity(errorstatus, nchanprof, reflectivity, &
                                        nlevels, 1_jpim, init = .TRUE._jplm)
    END IF
  END IF

  CALL rttov_alloc_rad       (errorstatus, nchannels, radiance, nlevels-1_jpim,asw)
!    WRITE(*,*) 'error rttov_alloc_rad :',errorstatus
  ! Allocate transmittance structure
  CALL rttov_alloc_transmission( &
      & errorstatus,             &
      & transmission,            &
      & nlevels-1_jpim,          &
      & nchannels,               &
      & asw,                     &
      & init=.TRUE.)

  profiles(1) % zenangle    = 0. ! zenith
  profiles(1) % skin % fastem(:) = &
! RTTOV 8.5 example
!        (/ 3.0_JPRB, 5.0_JPRB, 15.0_JPRB, 0.1_JPRB, 0.3_JPRB /)
! Bare soil see Table 3 svr rttov7)
       (/ 2.3_JPRB, 1.9_JPRB, 21.8_JPRB, 0.0_JPRB, 0.5_JPRB /)

  profiles(1) % nlevels =  nlevels
  profiles(1) % nlayers =  nlevels-1

 ! Ensure the options and coefficients are consistent
  CALL rttov_user_options_checkinput(errorstatus, opts, coefs)
  IF (errorstatus /= 0) THEN
    WRITE(*,*) 'error in rttov options'
    CALL PRINT_MSG( NVERB_FATAL, 'GEN', 'CALL_RTTOV13', 'rttov_user_options_checkinput' )
  ENDIF

  profiles(1) % date(1) = TDTCUR%nyear
  profiles(1) % date(2) = TDTCUR%nmonth
  profiles(1) % date(3) = TDTCUR%nday
! profiles(1) % ctp = 500.0_JPRB   ! Not used but still required by RTTOV
! profiles(1) % cfraction = 0.0_JPRB
  profiles(1) % clwde     = 0.0_JPRB

  DO JI=IIB,IIE
    DO JJ=IJB,IJE      

      ZANGL = XUNDEF
      ZLON  = XLON(JI,JJ)
      ZLAT  = XLAT(JI,JJ)
      IF (KRTTOVINFO(1,JSAT) == 2) THEN ! DMSP PLATFORM
        ZANGL=53.1 ! see Saunders, 2002, RTTOV7 - science/validation rep, page 8
      ELSEIF (KRTTOVINFO(1,JSAT) == 3) THEN ! METEOSAT PLATFORM
        CALL DETER_ANGLE(5, 1, ZLAT, ZLON, ZANGL)
        WHERE (ZANGL /= XUNDEF .AND. ZANGL /=0.) ZANGL=ACOS(1./ZANGL)*rad2deg
      ELSEIF (KRTTOVINFO(1,JSAT) == 12) THEN ! MSG PLATFORM
        CALL DETER_ANGLE(6, 1, ZLAT, ZLON, ZANGL)
        WHERE (ZANGL /= XUNDEF .AND. ZANGL /=0.) ZANGL=ACOS(1./ZANGL)*rad2deg
      ELSEIF (KRTTOVINFO(1,JSAT) == 4) THEN ! GOES-E PLATFORM
        CALL DETER_ANGLE(1, 1, ZLAT, ZLON, ZANGL)
        WHERE (ZANGL /= XUNDEF .AND. ZANGL /=0.) ZANGL=ACOS(1./ZANGL)*rad2deg
      ELSEIF (KRTTOVINFO(1,JSAT) == 7) THEN ! TRMM PLATFORM
        ZANGL=52.3
      ELSE
        ZANGL=0.
      ENDIF

      WHERE (ZANGL == XUNDEF) ZANGL=0.
      profiles(1) % zenangle = MIN(ZANGL(1),zenmaxv9)
      profiles(1) % azangle = 0.
      profiles(1) % sunzenangle = XZENITH(JI,JJ) *rad2deg
      profiles(1) % sunazangle  = ZAZIMSOL(JI,JJ)*rad2deg

      DO JK=IKB,IKE ! nlevels
        JKRAD = nlevels-JK+2 !INVERSION OF VERTICAL LEVELS!
        profiles(1) % p(JKRAD) = PPABST(JI,JJ,JK)*0.01
        profiles(1) % t(JKRAD) = MIN(tmax,MAX(tmin,ZTEMP(JI,JJ,JK)))
        profiles(1) % q(JKRAD) = MIN(qmax,MAX(qmin,PRT(JI,JJ,JK,1)/(1.+PRT(JI,JJ,JK,1))))
      END DO
      profiles(1) % elevation = 0.5*( PZZ(JI,JJ,1)+PZZ(JI,JJ,IKB) )*0.001
      profiles(1) % skin % t = MIN(tmax,MAX(tmin,PTSRAD(JI,JJ)))
      profiles(1) % s2m % t = MIN(tmax,MAX(tmin,ZTEMP(JI,JJ,IKB)))
      profiles(1) % s2m % q = MIN(qmax,MAX(qmin,PRT(JI,JJ,IKB,1)/(1.+PRT(JI,JJ,IKB,1))))
      profiles(1) % s2m % u = PULVLKB(JI,JJ) ! 2m wind speed u (m/s)
      profiles(1) % s2m % v = PVLVLKB(JI,JJ) ! 2m wind speed v (m/s)
      profiles(1) % s2m % p = PPABST(JI,JJ,IKB)*0.01
      profiles(1) % s2m % wfetc = 100000. ! typical value for open ocean (m)
      IF (NINT(XSEA(JI,JJ)).EQ.0.) THEN
        profiles(1) % skin % surftype = 0 ! Surface Mask 0=land, 1=sea, 2=sea-ice
      ELSE
        profiles(1) % skin % surftype = 1
        profiles(1) % skin % watertype = 1 ! Ocean water
      END IF
      IF( coefs%coef% id_sensor /= sensor_id_mw) THEN
!        profiles(1) % clw_scheme =  1 ! OPAC CLW properties
         profiles(1) % clw_scheme =  2 ! “Deff” CLW properties
         profiles(1) % clwde_param = 1
         profiles(1) % ice_scheme =  1 ! Baum/SSEC ice properties
!        profiles(1) % ice_scheme =  2 ! Baran2014 ice properties
         profiles(1) % icede_param = 4 ! McFarquar et al (2003)

!        profiles(1) % clw_scheme = coefs % coef_mfasis_cld % clw_scheme
!        profiles(1) % ice_scheme = coefs % coef_mfasis_cld % ice_scheme

        DO JK=IKB+1,IKE-1 ! nlayers
          JKRAD = nlevels-JK+1 !INVERSION OF VERTICAL LEVELS!
          profiles(1) %cfrac(JKRAD) = PCLDFR(JI,JJ,JK)
          profiles(1) %cloud(1,JKRAD) = PRT(JI,JJ,JK,2)
          IF (OUSERI) THEN
            profiles(1) %cloud(6,JKRAD) = (PRT(JI,JJ,JK,4)+PRT(JI,JJ,JK,5))
          END IF
        END DO
      ELSE
        DO JK=IKB,IKE
          JKRAD = nlevels-JK+2 !INVERSION OF VERTICAL LEVELS!
          cld_profiles(1) %hydro_frac(JKRAD,1) = 1.
          cld_profiles(1) % ph (JKRAD) = 0.5*( PPABST(JI,JJ,JK) + PPABST(JI,JJ,JK+1) )*0.01
          cld_profiles(1) %hydro(JKRAD,4) = PRT(JI,JJ,JK,2) ! liquid water
          cld_profiles(1) %hydro(JKRAD,1) = PRT(JI,JJ,JK,3) ! rain
          IF (OUSERI) THEN
            cld_profiles(1) %hydro(JKRAD,5) = PRT(JI,JJ,JK,4) ! ice water
            cld_profiles(1) %hydro(JKRAD,2) = PRT(JI,JJ,JK,5) ! snow
            cld_profiles(1) %hydro(JKRAD,3) = PRT(JI,JJ,JK,6) ! graupel
          END IF
        END DO
        cld_profiles (1) % ph (nlevels+1) =   profiles (1) % s2m % p
      END IF

      DO JCH=1,nchanprof
        IF (.NOT.calcemis(JCH)) emissivity(JCH)%emis_in = PEMIS(JI,JJ)
      END DO
      IF (opts % rt_ir % addsolar) THEN
        ! Use BRDF atlas
        CALL rttov_get_brdf(              &
                  errorstatus,            &
                  opts,                   &
                  chanprof,               &
                  profiles,               &
                  coefs,                  &
                  brdf_atlas,             &
                  reflectance(:) % refl_in)
        IF (errorstatus /= errorstatus_success) THEN
          WRITE(*,*) 'error reading BRDF atlas'
          CALL PRINT_MSG(NVERB_FATAL,'GEN','CALL_RTTOV13','error rttov_get_brdf')
        END IF
        ! Calculate BRDF within RTTOV where the atlas BRDF value is zero or less
        calcrefl(:) = (reflectance(:) % refl_in <= 0._jprb)
      END IF
      IF (coefs%coef% id_sensor /= sensor_id_mw) THEN
        CALL rttov_direct(               &
             & errorstatus,              &! out   error flag
             & chanprof,                 &! in    channel and profile index structure
             & opts,                     &! in    options structure
             & profiles,                 &! in    profile array
             & coefs,                    &! in    coefficients strucutre
             & transmission,             &! inout computed transmittances
             & radiance,                 &! inout computed radiances
             & calcemis    = calcemis,   &! in    flag for internal emissivity calcs
             & emissivity  = emissivity, &! inout input/output emissivities per channel
             & calcrefl    = calcrefl,   &! in    flag for internal BRDF calcs
             & reflectance = reflectance) ! inout input/output BRDFs per channel
      ELSEIF (radar) THEN
        CALL rttov_scatt ( &
             & errorstatus,  opts_scatt,   &
             & nlevels,      chanprof,     &
             & frequencies,  profiles,     &
             & cld_profiles, coefs,        &
             & coef_scatt,   calcemis,     &
             & emissivity,   radiance,     &
             & reflectivity = reflectivity)
      ELSE
        CALL rttov_scatt ( &
             & errorstatus,  opts_scatt,   &
             & nlevels,      chanprof,     &
             & frequencies,  profiles,     &
             & cld_profiles, coefs,        &
             & coef_scatt,   calcemis,     &
             & emissivity,   radiance)
      END IF
      DO JCH=1,nchanprof
        ichan = chanprof(JCH)%chan
        thermal = coefs%coef%ss_val_chn(ichan) < 2
!       solar   = coefs%coef%ss_val_chn(ichan) > 0
        IF (.NOT.radar) THEN
          IF (thermal) THEN
            ZOUT(JI,JJ,JCH)= radiance % bt(JCH)
          ELSE
            ZOUT(JI,JJ,JCH)= radiance % refl(JCH)
          END IF
        ELSE
          DO JK=IKB,IKE
            JKRAD = nlevels-JK+2 !INVERSION OF VERTICAL LEVELS!
            ZREF(JI,JJ,JK,JCH)= reflectivity % azef(JKRAD,JCH)
          END DO
        END IF
      END DO
    END DO
  END DO
! -----------------------------------------------------------------------------
! LATERAL BOUNDARY FILLING
  IF (.NOT.radar) THEN
    IF (LWEST_ll() .AND.CLBCX(1)/='CYCL') ZOUT(IIB-1,:,:) = ZOUT(IIB,:,:)
    IF (LEAST_ll() .AND.CLBCX(1)/='CYCL') ZOUT(IIE+1,:,:) = ZOUT(IIE,:,:)
    IF (LSOUTH_ll().AND.CLBCY(1)/='CYCL') ZOUT(:,IJB-1,:) = ZOUT(:,IJB,:)
    IF (LNORTH_ll().AND.CLBCY(1)/='CYCL') ZOUT(:,IJE+1,:) = ZOUT(:,IJE,:)
  ELSE
    IF (LWEST_ll() .AND.CLBCX(1)/='CYCL') ZREF(IIB-1,:,:,:) = ZREF(IIB,:,:,:)
    IF (LEAST_ll() .AND.CLBCX(1)/='CYCL') ZREF(IIE+1,:,:,:) = ZREF(IIE,:,:,:)
    IF (LSOUTH_ll().AND.CLBCY(1)/='CYCL') ZREF(:,IJB-1,:,:) = ZREF(:,IJB,:,:)
    IF (LNORTH_ll().AND.CLBCY(1)/='CYCL') ZREF(:,IJE+1,:,:) = ZREF(:,IJE,:,:)
  END IF
! -----------------------------------------------------------------------------
  YBEG='    '
  IF (KRTTOVINFO(1,JSAT) <= 2) THEN ! NOAA
    WRITE(YTWO,'(I2.2)') KRTTOVINFO(2,JSAT)
    YBEG=TRIM(YPLAT(KRTTOVINFO(1,JSAT)))//YTWO
  ELSEIF (KRTTOVINFO(1,JSAT) <= JPPLAT) THEN
    WRITE(YONE,'(I1.1)') KRTTOVINFO(2,JSAT)
    YBEG=TRIM(YPLAT(KRTTOVINFO(1,JSAT)))//YONE
  ELSE
    YBEG='XXXXX'
  END IF
  IF (KRTTOVINFO(1,JSAT) == 4) THEN
    YBEG='GOES'
  ELSEIF  (KRTTOVINFO(1,JSAT) == 57) THEN
    IF  (KRTTOVINFO(3,JSAT) == 108) YBEG='b325'
    IF  (KRTTOVINFO(3,JSAT) == 110) YBEG='b448'
    IF  (KRTTOVINFO(3,JSAT) == 111) YBEG='b183'
    YTWO='ch'
  ELSEIF  (KRTTOVINFO(1,JSAT) == 68) THEN
    YBEG='aws'
    YTWO='ch'
  ELSEIF  (KRTTOVINFO(1,JSAT) == 28) THEN
    IF  (KRTTOVINFO(2,JSAT) == 1) YBEG='aos1'
    IF  (KRTTOVINFO(2,JSAT) == 2) YBEG='aos2'
    IF  (KRTTOVINFO(2,JSAT) == 3) YBEG='aos3'
    YTWO='ch'
  ELSE
    WRITE(YTWO,'(I2.2)') KRTTOVINFO(3,JSAT)
  END IF

  DO JCH=1,nchanprof
    YEND='    '
    WRITE(YCHAN,'(I2.2)') JCH
    IF (KRTTOVINFO(3,JSAT) == 0) THEN ! HIRS
      YEND='H'//YCHAN
    ELSEIF (KRTTOVINFO(3,JSAT) == 3) THEN ! AMSU-A
      YEND='A'//YCHAN
    ELSEIF (KRTTOVINFO(3,JSAT) == 4) THEN ! AMSU-B
      YEND='B'//YCHAN
    ELSEIF (KRTTOVINFO(3,JSAT) == 6) THEN ! SSMI
      YEND=YLBL_SSMI(JCH)
    ELSEIF (KRTTOVINFO(3,JSAT) == 9) THEN ! TMI
      YEND=YLBL_TMI(JCH)
    ELSEIF (KRTTOVINFO(3,JSAT) == 15) THEN ! MHS
      YEND=YLBL_MHS(JCH)
    ELSEIF (KRTTOVINFO(3,JSAT) == 20) THEN ! MVIRI
      YEND=YLBL_MVIRI(JCH)
    ELSEIF (KRTTOVINFO(3,JSAT) == 21) THEN ! SEVIRI
      IF (opts % rt_ir % addsolar) THEN
        YEND=YLBL_SEVIRI(JCH)
      ELSE
        YEND=YLBL_SEVIRI(JCH+3)
      END IF
    ELSEIF (KRTTOVINFO(3,JSAT) == 22) THEN ! GOES-I
      IF (opts % rt_ir % addsolar) THEN
        YEND=YLBL_GOESI(JCH)
      ELSE
        YEND=YLBL_GOESI(JCH+6)
      END IF
    ELSEIF (KRTTOVINFO(3,JSAT) == 34) THEN ! SAPHIR
      YEND=YLBL_SAPHIR(JCH)
    ELSEIF (KRTTOVINFO(3,JSAT) == 44) THEN ! ABI
      IF (opts % rt_ir % addsolar) THEN
        YEND=YLBL_ABI(JCH)
      ELSE
        YEND=YLBL_ABI(JCH+6)
      END IF
    ELSEIF (KRTTOVINFO(3,JSAT) == 70) THEN ! ICI
      YBEG='ici'
      YEND=YLBL_ICI(JCH)
    ELSEIF (KRTTOVINFO(3,JSAT) == 71) THEN ! GMI
      YBEG='gmi'
      YEND=YLBL_GMI(JCH)
    ELSEIF (KRTTOVINFO(3,JSAT) == 105) THEN ! DPR
      YBEG='dpr'
      YEND=YLBL_DPR(JCH)
    ELSEIF (KRTTOVINFO(3,JSAT) == 107) THEN ! CPR
      YBEG='cpr'
      YEND=YLBL_CPR(JCH)
    ELSE
      YEND=YTWO//YCHAN
    END IF

    ichan = chanprof(JCH)%chan
    thermal = coefs%coef%ss_val_chn(ichan) < 2
!   solar   = coefs%coef%ss_val_chn(ichan) > 0
    IF (.NOT.radar) THEN
      YMNHNAME   = TRIM(YBEG)//'_'//TRIM(YEND)
      IF (thermal) THEN
        IF (KRTTOVINFO(3,JSAT) /= 21 .AND. KRTTOVINFO(3,JSAT) /= 44) &
        YMNHNAME   = TRIM(YBEG)//'_'//TRIM(YEND)//'BT'
        YUNITS     = 'K'
        YCOMMENT   = TRIM(YBEG)//'_'//TRIM(YEND)//' brightness temperature'
      ELSE
        YMNHNAME   = TRIM(YBEG)//'_'//TRIM(YEND)//'refl'
        YUNITS     = '-'
        YCOMMENT   = TRIM(YBEG)//'_'//TRIM(YEND)//' bidirectional reflectance factor'
      END IF
      TZFIELD = TFIELDMETADATA(                    &
      CMNHNAME   = TRIM( YMNHNAME ),               &
      CSTDNAME   = '',                             &
      CLONGNAME  = 'MesoNH: ' // TRIM( YMNHNAME ), &
      CUNITS     = TRIM( YUNITS ),                 &
      CDIR       = 'XY',                           &
      CCOMMENT   = TRIM( YCOMMENT ),               &
      NGRID      = 1,                              &
      NTYPE      = TYPEREAL,                       &
      NDIMS      = 2,                              &
      LTIMEDEP   = .TRUE.                          )
      CALL IO_Field_write(TPFILE,TZFIELD,ZOUT(:,:,JCH))
      IF (opts%rt_ir%addsolar .AND. LCOSZENOUT) THEN
        YMNHNAME   = 'COSZEN'
        YUNITS     = '-'
        YCOMMENT   = 'Cosine of solar zenith angle'
        TZFIELD = TFIELDMETADATA(                    &
        CMNHNAME   = TRIM( YMNHNAME ),               &
        CSTDNAME   = '',                             &
        CLONGNAME  = 'MesoNH: ' // TRIM( YMNHNAME ), &
        CUNITS     = TRIM( YUNITS ),                 &
        CDIR       = 'XY',                           &
        CCOMMENT   = TRIM( YCOMMENT ),               &
        NGRID      = 1,                              &
        NTYPE      = TYPEREAL,                       &
        NDIMS      = 2,                              &
        LTIMEDEP   = .TRUE.                          )
        CALL IO_Field_write(TPFILE,TZFIELD,ZCOSZEN)
        LCOSZENOUT=.FALSE.
      END IF
    ELSE
      YMNHNAME   = TRIM(YBEG)//'_'//TRIM(YEND)//'refl'
      YUNITS     = 'dBZ'
      YCOMMENT   = TRIM(YBEG)//'_'//TRIM(YEND)//' radar reflectivity'
      TZFIELD = TFIELDMETADATA(                    &
      CMNHNAME   = TRIM( YMNHNAME ),               &
      CSTDNAME   = '',                             &
      CLONGNAME  = 'MesoNH: ' // TRIM( YMNHNAME ), &
      CUNITS     = TRIM( YUNITS ),                 &
      CDIR       = 'XY',                           &
      CCOMMENT   = TRIM( YCOMMENT ),               &
      NGRID      = 1,                              &
      NTYPE      = TYPEREAL,                       &
      NDIMS      = 3,                              &
      LTIMEDEP   = .TRUE.                          )
      WHERE(ZREF(:,:,:,JCH)==min_reflectivity) ZREF(:,:,:,JCH)=XDEFFILLVALUE
      CALL IO_Field_write(TPFILE,TZFIELD,ZREF(:,:,:,JCH))
    END IF
  END DO
  DEALLOCATE(chanprof,frequencies,emissivity,calcemis,profiles)

  IF (.NOT.radar) THEN
    DEALLOCATE(ZOUT)
  ELSE
    DEALLOCATE(ZREF)
  END IF
  IF( coefs%coef% id_sensor == sensor_id_mw) THEN
    CALL rttov_alloc_scatt_prof(errorstatus, nprof, cld_profiles, nlevels, &
                                cld_profiles(1)%nhydro, nhydro_frac, 0_jpim)
    CALL rttov_dealloc_scattcoeffs(coef_scatt)
    IF (radar) CALL rttov_alloc_reflectivity(errorstatus, nchanprof,       &
                                             reflectivity, nlevels, 0_jpim)
    DEALLOCATE(cld_profiles)
    DEALLOCATE(use_chan)
  ELSE
    DEALLOCATE(reflectance,calcrefl)
    IF (opts % rt_ir % addsolar) CALL rttov_deallocate_brdf_atlas(brdf_atlas)
  END IF
  CALL rttov_dealloc_coefs(errorstatus, coefs)
END DO

#else
PRINT *, "RTTOV 13.0 LIBRARY NOT AVAILABLE = ###CALL_RTTOV13####"
#endif
!
END SUBROUTINE CALL_RTTOV13
