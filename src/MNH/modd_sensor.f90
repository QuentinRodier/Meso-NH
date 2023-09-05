!MNH_LIC Copyright 2023-2023 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
! Original version:
!  P. Wautelet: 27/04/2023
! Modifications:
!-----------------------------------------------------------------
MODULE MODD_SENSOR
  USE MODD_PARAMETERS, ONLY: NSENSORNAMELGTMAX, NNEGUNDEF, XNEGUNDEF, XUNDEF
  USE MODD_TYPE_DATE,  ONLY: DATE_TIME
  USE MODD_SURF_PAR,   ONLY: XUNDEF_SFX => XUNDEF

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: TSENSOR
  PUBLIC :: TSENSORTIME

  INTEGER, PARAMETER :: NTAG_NCUR = 145
  INTEGER, PARAMETER :: NTAG_PACK = 245
  TYPE :: TSENSORTIME
      INTEGER                                    :: N_CUR   = 0       ! current step of storage
      REAL                                       :: XTSTEP  = 60.     ! storage time step (default reset later)
      TYPE(DATE_TIME), DIMENSION(:), ALLOCATABLE :: TPDATES           ! dates(n) (n: recording instants)

    CONTAINS
      PROCEDURE :: STORESTEP_CHECK_AND_SET
  END TYPE TSENSORTIME

  TYPE, ABSTRACT :: TSENSOR
      CHARACTER(LEN=NSENSORNAMELGTMAX) :: CNAME = '' ! Title or name of the sensor
      CHARACTER(LEN=NSENSORNAMELGTMAX) :: CTYPE = '' ! Sensor type:
        ! 'AIRCRAFT' : aircraft
        ! 'RADIOS' : radiosounding balloon, 'ISODEN' : iso-density balloon, 'CVBALL' : Constant Volume balloon
        ! 'STATION', 'PROFILER',...
      INTEGER :: NID = 0 ! Identification number of the sensor (from 1 to total number,
                         ! separate numbering for separate sensor types)
      INTEGER :: NSTORE_CUR = 0  ! Current store instant
      INTEGER :: NSTORE_MAX = -1 ! Maximum number of store instants (negative if arrays not allocated)

      INTEGER :: NBUFFER_FIXSIZE = 43 + 2 * NSENSORNAMELGTMAX ! Memory size required for exchange buffer (fixed part)
      INTEGER :: NBUFFER_VARSIZE = 0 ! Memory size required for exchange buffer (part per store instant)

      LOGICAL :: LFIX ! true if sensor is fix (can not move)

      ! Current position of sensor
      REAL :: XX_CUR   = XNEGUNDEF  ! x position
      REAL :: XY_CUR   = XNEGUNDEF  ! y position
      REAL :: XZ_CUR   = XNEGUNDEF  ! z position
      REAL :: XLAT_CUR = XNEGUNDEF  ! latitude
      REAL :: XLON_CUR = XNEGUNDEF  ! longitude

      ! Position in the mesh
      INTEGER :: NI_M = NNEGUNDEF ! X position for mass-point axis (between this one and the next one)
      INTEGER :: NJ_M = NNEGUNDEF ! Y position for mass-point axis (between this one and the next one)
      INTEGER :: NI_U = NNEGUNDEF ! X position for u-point axis (between this one and the next one)
      INTEGER :: NJ_V = NNEGUNDEF ! Y position for v-point axis (between this one and the next one)

      INTEGER :: NK00 = NNEGUNDEF ! Z position for ni_m  , nj_m
      INTEGER :: NK01 = NNEGUNDEF ! Z position for ni_m  , nj_m+1
      INTEGER :: NK10 = NNEGUNDEF ! Z position for ni_m+1, nj_m
      INTEGER :: NK11 = NNEGUNDEF ! Z position for ni_m+1, nj_m+1
      INTEGER :: NU00 = NNEGUNDEF ! Z position for ni_u  , nj_m
      INTEGER :: NU01 = NNEGUNDEF ! Z position for ni_u  , nj_m+1
      INTEGER :: NU10 = NNEGUNDEF ! Z position for ni_u+1, nj_m
      INTEGER :: NU11 = NNEGUNDEF ! Z position for ni_u+1, nj_m+1
      INTEGER :: NV00 = NNEGUNDEF ! Z position for ni_m  , nj_v
      INTEGER :: NV01 = NNEGUNDEF ! Z position for ni_m  , nj_v+1
      INTEGER :: NV10 = NNEGUNDEF ! Z position for ni_m+1, nj_v
      INTEGER :: NV11 = NNEGUNDEF ! Z position for ni_m+1, nj_v+1
      ! Coefficient to interpolate values (sensors are usually not exactly on mesh points)
      REAL :: XXMCOEF = XUNDEF ! Interpolation coefficient for X (mass-point)
      REAL :: XYMCOEF = XUNDEF ! Interpolation coefficient for Y (mass-point)
      REAL :: XXUCOEF = XUNDEF ! Interpolation coefficient for X (U-point)
      REAL :: XYVCOEF = XUNDEF ! Interpolation coefficient for Y (V-point)

      ! Coefficient to interpolate vertically
      REAL :: XZCOEF00 = XUNDEF ! Interpolation coefficient in Z direction for ni_m  , nj_m
      REAL :: XZCOEF01 = XUNDEF ! Interpolation coefficient in Z direction for ni_m  , nj_m+1
      REAL :: XZCOEF10 = XUNDEF ! Interpolation coefficient in Z direction for ni_m+1, nj_m
      REAL :: XZCOEF11 = XUNDEF ! Interpolation coefficient in Z direction for ni_m+1, nj_m+1
      REAL :: XUCOEF00 = XUNDEF ! Interpolation coefficient in Z direction for ni_u  , nj_m
      REAL :: XUCOEF01 = XUNDEF ! Interpolation coefficient in Z direction for ni_u  , nj_m+1
      REAL :: XUCOEF10 = XUNDEF ! Interpolation coefficient in Z direction for ni_u+1, nj_m
      REAL :: XUCOEF11 = XUNDEF ! Interpolation coefficient in Z direction for ni_u+1, nj_m+1
      REAL :: XVCOEF00 = XUNDEF ! Interpolation coefficient in Z direction for ni_m  , nj_v
      REAL :: XVCOEF01 = XUNDEF ! Interpolation coefficient in Z direction for ni_m  , nj_v+1
      REAL :: XVCOEF10 = XUNDEF ! Interpolation coefficient in Z direction for ni_m+1, nj_v
      REAL :: XVCOEF11 = XUNDEF ! Interpolation coefficient in Z direction for ni_m+1, nj_v+1

      ! Data records (at recording instants)
      REAL, DIMENSION(:,:),   ALLOCATABLE :: XZON    ! zonal wind(n)
      REAL, DIMENSION(:,:),   ALLOCATABLE :: XMER    ! meridian wind(n)
      REAL, DIMENSION(:,:),   ALLOCATABLE :: XW      ! w(n)  (air vertical speed)
      REAL, DIMENSION(:,:),   ALLOCATABLE :: XP      ! p(n)
      REAL, DIMENSION(:,:),   ALLOCATABLE :: XTKE    ! tke(n)
      REAL, DIMENSION(:,:),   ALLOCATABLE :: XTH     ! th(n)
      REAL, DIMENSION(:,:,:), ALLOCATABLE :: XR      ! r*(n)
      REAL, DIMENSION(:,:,:), ALLOCATABLE :: XSV     ! Sv*(n)
      REAL, DIMENSION(:),     ALLOCATABLE :: XTSRAD  ! surface temperature Ts(n)

      REAL, DIMENSION(:,:),   ALLOCATABLE :: XCIZ       ! Ice concentration
      REAL, DIMENSION(:,:),   ALLOCATABLE :: XCCZ       ! Cloud concentration (LIMA)
      REAL, DIMENSION(:,:),   ALLOCATABLE :: XCRZ       ! Rain concentration (LIMA)
      REAL, DIMENSION(:,:),   ALLOCATABLE :: XIWCZ      ! ice water content
      REAL, DIMENSION(:,:),   ALLOCATABLE :: XLWCZ      ! liquid water content
      REAL, DIMENSION(:,:),   ALLOCATABLE :: XCRARE     ! cloud radar reflectivity
      REAL, DIMENSION(:,:),   ALLOCATABLE :: XCRARE_ATT ! attenuated (= more realistic) cloud radar reflectivity


    CONTAINS
      PROCEDURE(TSENSOR_ALLOCATION),   DEFERRED :: DATA_ARRAYS_ALLOCATE
      PROCEDURE(TSENSOR_DEALLOCATION), DEFERRED :: DATA_ARRAYS_DEALLOCATE
      ! Remark: data_arrays_(de)allocate_sensor do not point to data_arrays_(de)allocate to allow other dummy arguments
      PROCEDURE, NON_OVERRIDABLE                :: DATA_ARRAYS_ALLOCATE_SENSOR
      PROCEDURE, NON_OVERRIDABLE                :: DATA_ARRAYS_DEALLOCATE_SENSOR

      PROCEDURE                                 :: COMPUTE_VERTICAL_INTERP_COEFF
      PROCEDURE                                 :: INTERP_FROM_MASSPOINT
      PROCEDURE                                 :: INTERP_FROM_UPOINT
      PROCEDURE                                 :: INTERP_FROM_VPOINT
      PROCEDURE                                 :: INTERP_HOR_FROM_MASSPOINT_0D
      PROCEDURE                                 :: INTERP_HOR_FROM_UPOINT_0D
      PROCEDURE                                 :: INTERP_HOR_FROM_VPOINT_0D
      PROCEDURE                                 :: INTERP_HOR_FROM_MASSPOINT_1D
      PROCEDURE                                 :: INTERP_HOR_FROM_UPOINT_1D
      PROCEDURE                                 :: INTERP_HOR_FROM_VPOINT_1D

      PROCEDURE, NON_OVERRIDABLE               :: BUFFER_SIZE_COMPUTE
      ! 2 procedures pointing to the same one: necessary to allow overload for extended types (limitation of Fortran standard)
      PROCEDURE, NON_OVERRIDABLE               :: BUFFER_PACK_SENSOR
      PROCEDURE                                :: BUFFER_PACK => BUFFER_PACK_SENSOR
      PROCEDURE, NON_OVERRIDABLE               :: BUFFER_UNPACK_SENSOR
      PROCEDURE                                :: BUFFER_UNPACK => BUFFER_UNPACK_SENSOR
      PROCEDURE, NON_OVERRIDABLE               :: BUFFER_SIZE_SEND
      PROCEDURE, NON_OVERRIDABLE               :: BUFFER_SIZE_RECV
      PROCEDURE, NON_OVERRIDABLE               :: BUFFER_SEND
      PROCEDURE, NON_OVERRIDABLE               :: BUFFER_RECV
      PROCEDURE, NON_OVERRIDABLE               :: SEND            => SENSOR_COMM_SEND
      PROCEDURE, NON_OVERRIDABLE               :: SEND_DEALLOCATE => SENSOR_COMM_SEND_DEALLOCATE
      PROCEDURE, NON_OVERRIDABLE               :: RECV_ALLOCATE   => SENSOR_COMM_RECV_ALLOCATE

      GENERIC :: INTERP_HOR_FROM_MASSPOINT => INTERP_HOR_FROM_MASSPOINT_0D, INTERP_HOR_FROM_MASSPOINT_1D
      GENERIC :: INTERP_HOR_FROM_UPOINT    => INTERP_HOR_FROM_UPOINT_0D,    INTERP_HOR_FROM_UPOINT_1D
      GENERIC :: INTERP_HOR_FROM_VPOINT    => INTERP_HOR_FROM_VPOINT_0D,    INTERP_HOR_FROM_VPOINT_1D

  END TYPE TSENSOR

  ABSTRACT INTERFACE
    SUBROUTINE TSENSOR_ALLOCATION( TPSENSOR, KSTORE )
      IMPORT TSENSOR
      CLASS(TSENSOR),    INTENT(INOUT) :: TPSENSOR
      INTEGER, OPTIONAL, INTENT(IN)    :: KSTORE
    END SUBROUTINE

    SUBROUTINE TSENSOR_DEALLOCATION( TPSENSOR )
      IMPORT TSENSOR
      CLASS(TSENSOR),    INTENT(INOUT) :: TPSENSOR
    END SUBROUTINE
  END INTERFACE


  CONTAINS
    ! ############################################################################
    SUBROUTINE DATA_ARRAYS_ALLOCATE_SENSOR( TPSENSOR, OVERTPROF, KLEVELS, KSTORE )
    ! ############################################################################

      USE MODD_CONF_N,           ONLY: NRR
      USE MODD_DIM_N,            ONLY: NKMAX
      USE MODD_NSV,              ONLY: NSV
      USE MODD_PARAMETERS,       ONLY: JPVEXT
      USE MODD_PARAM_N,          ONLY: CCLOUD, CRAD, CTURB

      USE MODE_MSG

      CLASS(TSENSOR), INTENT(INOUT) :: TPSENSOR
      LOGICAL,        INTENT(IN)    :: OVERTPROF ! vertical profile or not
      INTEGER,        INTENT(IN)    :: KLEVELS   ! number of vertical levels
      INTEGER,        INTENT(IN)    :: KSTORE    ! number of storage instants

      INTEGER :: IKU ! number of vertical levels for profile
      INTEGER :: IVARSIZE ! total allocated size per store

      CALL PRINT_MSG( NVERB_DEBUG, 'GEN', 'Data_arrays_allocate_sensor', 'sensor: ' // TRIM(TPSENSOR%CNAME), OLOCAL = .TRUE. )

      IKU = NKMAX + 2 * JPVEXT
      IVARSIZE = 0

      IF ( TPSENSOR%NSTORE_MAX >= 0 ) THEN
        CALL PRINT_MSG( NVERB_ERROR, 'GEN', 'Data_arrays_allocate_sensor', 'sensor: ' // TRIM(TPSENSOR%CNAME) &
                                            // ' already allocated', OLOCAL = .TRUE. )
        RETURN
      END IF

      TPSENSOR%NSTORE_MAX = KSTORE

      ALLOCATE( TPSENSOR%XZON (KLEVELS, KSTORE) ) ; IVARSIZE = IVARSIZE + KLEVELS
      ALLOCATE( TPSENSOR%XMER (KLEVELS, KSTORE) ) ; IVARSIZE = IVARSIZE + KLEVELS
      ALLOCATE( TPSENSOR%XW   (KLEVELS, KSTORE) ) ; IVARSIZE = IVARSIZE + KLEVELS
      ALLOCATE( TPSENSOR%XP   (KLEVELS, KSTORE) ) ; IVARSIZE = IVARSIZE + KLEVELS
      IF ( CTURB == 'TKEL' ) THEN
        ALLOCATE( TPSENSOR%XTKE(KLEVELS, KSTORE) ) ; IVARSIZE = IVARSIZE + KLEVELS
      ELSE
        ALLOCATE( TPSENSOR%XTKE(0, 0) )
      END IF
      ALLOCATE( TPSENSOR%XTH  (KLEVELS, KSTORE) )      ; IVARSIZE = IVARSIZE + KLEVELS
      ALLOCATE( TPSENSOR%XR   (KLEVELS, KSTORE, NRR) ) ; IVARSIZE = IVARSIZE + KLEVELS * NRR
      ALLOCATE( TPSENSOR%XSV  (KLEVELS, KSTORE, NSV) ) ; IVARSIZE = IVARSIZE + KLEVELS * NSV
      IF ( CRAD /= 'NONE' ) THEN
        ALLOCATE( TPSENSOR%XTSRAD(KSTORE) ) ; IVARSIZE = IVARSIZE + 1
      ELSE
        ALLOCATE( TPSENSOR%XTSRAD(0) )
      END IF

      IF ( OVERTPROF ) THEN
        IF ( CCLOUD == 'LIMA') THEN
          ALLOCATE( TPSENSOR%XCIZ    (IKU, KSTORE) ) ; IVARSIZE = IVARSIZE + IKU
          ALLOCATE( TPSENSOR%XCCZ    (IKU, KSTORE) ) ; IVARSIZE = IVARSIZE + IKU
          ALLOCATE( TPSENSOR%XCRZ    (IKU, KSTORE) ) ; IVARSIZE = IVARSIZE + IKU
        ELSE IF ( CCLOUD(1:3) == 'ICE') THEN
          ALLOCATE( TPSENSOR%XCIZ    (IKU, KSTORE) ) ; IVARSIZE = IVARSIZE + IKU
          ALLOCATE( TPSENSOR%XCCZ    (0, 0) )
          ALLOCATE( TPSENSOR%XCRZ    (0, 0) )
        ELSE
          ALLOCATE( TPSENSOR%XCIZ    (0, 0) )
          ALLOCATE( TPSENSOR%XCCZ    (0, 0) )
          ALLOCATE( TPSENSOR%XCRZ    (0, 0) )
        END IF

        ALLOCATE( TPSENSOR%XIWCZ     (IKU, KSTORE) ) ; IVARSIZE = IVARSIZE + IKU
        ALLOCATE( TPSENSOR%XLWCZ     (IKU, KSTORE) ) ; IVARSIZE = IVARSIZE + IKU
        ALLOCATE( TPSENSOR%XCRARE    (IKU, KSTORE) ) ; IVARSIZE = IVARSIZE + IKU
        ALLOCATE( TPSENSOR%XCRARE_ATT(IKU, KSTORE) ) ; IVARSIZE = IVARSIZE + IKU
      ELSE
        ALLOCATE( TPSENSOR%XCIZ    (0, 0) )
        ALLOCATE( TPSENSOR%XCCZ    (0, 0) )
        ALLOCATE( TPSENSOR%XCRZ    (0, 0) )
        ALLOCATE( TPSENSOR%XIWCZ     (0, 0) )
        ALLOCATE( TPSENSOR%XLWCZ     (0, 0) )
        ALLOCATE( TPSENSOR%XCRARE    (0, 0) )
        ALLOCATE( TPSENSOR%XCRARE_ATT(0, 0) )
      END IF

      TPSENSOR%NBUFFER_VARSIZE = TPSENSOR%NBUFFER_VARSIZE + IVARSIZE

      TPSENSOR%XZON      (:,:)   = XUNDEF
      TPSENSOR%XMER      (:,:)   = XUNDEF
      TPSENSOR%XW        (:,:)   = XUNDEF
      TPSENSOR%XP        (:,:)   = XUNDEF
      TPSENSOR%XTKE      (:,:)   = XUNDEF
      TPSENSOR%XTH       (:,:)   = XUNDEF
      TPSENSOR%XR        (:,:,:) = XUNDEF
      TPSENSOR%XSV       (:,:,:) = XUNDEF
      TPSENSOR%XTSRAD    (:)     = XUNDEF_SFX
      TPSENSOR%XCIZ      (:,:)   = XUNDEF
      TPSENSOR%XCCZ      (:,:)   = XUNDEF
      TPSENSOR%XCRZ      (:,:)   = XUNDEF
      TPSENSOR%XIWCZ     (:,:)   = XUNDEF
      TPSENSOR%XLWCZ     (:,:)   = XUNDEF
      TPSENSOR%XCRARE    (:,:)   = XUNDEF
      TPSENSOR%XCRARE_ATT(:,:)   = XUNDEF

    END SUBROUTINE DATA_ARRAYS_ALLOCATE_SENSOR


    ! ##################################################
    SUBROUTINE DATA_ARRAYS_DEALLOCATE_SENSOR( TPSENSOR )
    ! ##################################################

      USE MODE_MSG

      CLASS(TSENSOR), INTENT(INOUT) :: TPSENSOR

      CALL PRINT_MSG( NVERB_DEBUG, 'GEN', 'Data_arrays_deallocate_sensor', 'sensor: ' // TRIM(TPSENSOR%CNAME), OLOCAL = .TRUE. )

      ! nstore_max set to negative value to inform that arrays are not allocated
      TPSENSOR%NSTORE_MAX = -1

      DEALLOCATE( TPSENSOR%XZON       )
      DEALLOCATE( TPSENSOR%XMER       )
      DEALLOCATE( TPSENSOR%XW         )
      DEALLOCATE( TPSENSOR%XP         )
      DEALLOCATE( TPSENSOR%XTKE       )
      DEALLOCATE( TPSENSOR%XTH        )
      DEALLOCATE( TPSENSOR%XR         )
      DEALLOCATE( TPSENSOR%XSV        )
      DEALLOCATE( TPSENSOR%XTSRAD     )
      DEALLOCATE( TPSENSOR%XCIZ       )
      DEALLOCATE( TPSENSOR%XCCZ       )
      DEALLOCATE( TPSENSOR%XCRZ       )
      DEALLOCATE( TPSENSOR%XIWCZ      )
      DEALLOCATE( TPSENSOR%XLWCZ      )
      DEALLOCATE( TPSENSOR%XCRARE     )
      DEALLOCATE( TPSENSOR%XCRARE_ATT )

    END SUBROUTINE DATA_ARRAYS_DEALLOCATE_SENSOR


    ! ###########################################################################
    FUNCTION STORESTEP_CHECK_AND_SET( TPSENSOR_TIME, KSTORE_ID ) RESULT( OSTORE )
    ! ###########################################################################

      USE MODD_TIME_N, ONLY: TDTCUR

      USE MODE_DATETIME
      USE MODE_MSG

      CLASS(TSENSORTIME), INTENT(INOUT) :: TPSENSOR_TIME
      INTEGER,            INTENT(OUT)   :: KSTORE_ID ! current step of storage
      LOGICAL                           :: OSTORE

      OSTORE = .FALSE.

      IF ( .NOT.ALLOCATED( TPSENSOR_TIME%TPDATES ) ) &
        CALL PRINT_MSG( NVERB_FATAL, 'GEN', 'Storestep_check_and_set', 'tpdates not allocated for tpsensor_time' )

      IF ( TPSENSOR_TIME%N_CUR == 0 ) THEN
        IF ( SIZE( TPSENSOR_TIME%TPDATES ) < 1 ) &
          CALL PRINT_MSG( NVERB_FATAL, 'GEN', 'Storestep_check_and_set', 'tpdates too small' )

        ! First store
        TPSENSOR_TIME%N_CUR = 1
        TPSENSOR_TIME%TPDATES(1) = TDTCUR
        KSTORE_ID = 1
        OSTORE = .TRUE.
      ELSE IF ( TPSENSOR_TIME%N_CUR > 0 ) THEN
        IF ( TDTCUR - TPSENSOR_TIME%TPDATES(TPSENSOR_TIME%N_CUR) >= TPSENSOR_TIME%XTSTEP - 1.E-6 ) THEN
          TPSENSOR_TIME%N_CUR = TPSENSOR_TIME%N_CUR + 1
          KSTORE_ID = TPSENSOR_TIME%N_CUR

          IF ( KSTORE_ID < 1 .OR. KSTORE_ID > SIZE( TPSENSOR_TIME%TPDATES ) ) THEN
            CALL PRINT_MSG( NVERB_ERROR, 'GEN', 'Storestep_check_and_set', 'problem with step of storage' )
            KSTORE_ID = -2
          ELSE
            TPSENSOR_TIME%TPDATES(KSTORE_ID) = TDTCUR
            OSTORE = .TRUE.
          END IF
        ELSE
          ! Return an invalid step number
          ! This is not an instant to do a store
          KSTORE_ID = -1
        END IF
      ELSE
        CALL PRINT_MSG( NVERB_ERROR, 'GEN', 'Storestep_check_and_set', 'n_cur cannot be negative' )
        KSTORE_ID = -3
      END IF

    END FUNCTION STORESTEP_CHECK_AND_SET


    ! ##############################################################################################
    SUBROUTINE COMPUTE_VERTICAL_INTERP_COEFF( TPSENSOR, HPOS, PALT, PZ, OLOW, OHIGH, ODONOLOWCRASH )
    ! ##############################################################################################
      USE MODD_PARAMETERS, ONLY: JPVEXT
      USE MODD_TIME_N,     ONLY: TDTCUR

      USE MODE_MSG

      CLASS(TSENSOR),         INTENT(INOUT) :: TPSENSOR
      CHARACTER(LEN=*),       INTENT(IN)    :: HPOS  ! Compute interpolation coefficients for Arakawa points ('MASS', 'U' or 'V')
      REAL,                   INTENT(IN)    :: PALT  ! Altitude to which compute interpolation coefficients
      REAL, DIMENSION(:,:,:), INTENT(IN)    :: PZ    ! Altitudes around point
      LOGICAL,                INTENT(OUT)   :: OLOW  ! true if sensor is too low
      LOGICAL,                INTENT(OUT)   :: OHIGH ! true if sensor is too high
      LOGICAL, OPTIONAL,      INTENT(IN)    :: ODONOLOWCRASH ! if true, force position to ground when sensor is below it

      INTEGER :: IK00, IK01, IK10, IK11
      INTEGER :: IKB, IKE, IKU
      INTEGER :: JI, JJ
      LOGICAL :: GCHANGE ! set to true if at least an index has been forced to change
      LOGICAL :: GDONE   ! set to true if coefficient computation has been done
      LOGICAL :: GDONOLOWCRASH
      REAL    :: ZZCOEF00, ZZCOEF01, ZZCOEF10, ZZCOEF11

      OLOW  = .FALSE.
      OHIGH = .FALSE.

      GCHANGE = .FALSE.
      GDONE   = .FALSE.

      IKB = 1 + JPVEXT
      IKE = SIZE( PZ, 3 ) - JPVEXT
      IKU = SIZE( PZ, 3 )

      IF ( PRESENT( ODONOLOWCRASH ) ) THEN
        GDONOLOWCRASH = ODONOLOWCRASH
      ELSE
        GDONOLOWCRASH = .FALSE.
      END IF
      ! Interpolation coefficients for the 4 suroundings verticals

      ! Determine ik?? coefficients depending on the level ordering
      ! (altitudes can also be based on pressure that decreaze with heigth)
      IF ( PZ(1,1,2)  > PZ(1,1,1) ) THEN
        IK00 = MAX( COUNT (PALT >= PZ(1,1,:)), 1)
        IK01 = MAX( COUNT (PALT >= PZ(1,2,:)), 1)
        IK10 = MAX( COUNT (PALT >= PZ(2,1,:)), 1)
        IK11 = MAX( COUNT (PALT >= PZ(2,2,:)), 1)
      ELSE
        IK00 = MAX( COUNT (PALT <= PZ(1,1,:)), 1)
        IK01 = MAX( COUNT (PALT <= PZ(1,2,:)), 1)
        IK10 = MAX( COUNT (PALT <= PZ(2,1,:)), 1)
        IK11 = MAX( COUNT (PALT <= PZ(2,2,:)), 1)
      END IF

      IF ( ANY( [ IK00, IK01, IK10, IK11 ] < IKB ) ) THEN
          ! Sensor is low (too near the ground or below it)
        OLOW = .TRUE.
        IF ( GDONOLOWCRASH ) THEN
          ! Do not allow crash on the ground: set position on the ground if too low
          GCHANGE = .TRUE.
          !Minimum altitude is on the ground at ikb (no crash if too low)
          IK00 = MAX ( IK00, IKB )
          IK01 = MAX ( IK01, IKB )
          IK10 = MAX ( IK10, IKB )
          IK11 = MAX ( IK11, IKB )

          ZZCOEF00 = ( PALT - PZ(1,1,IK00) ) / ( PZ(1,1,IK00+1) - PZ(1,1,IK00) )
          ZZCOEF01 = ( PALT - PZ(1,2,IK01) ) / ( PZ(1,2,IK01+1) - PZ(1,2,IK01) )
          ZZCOEF10 = ( PALT - PZ(2,1,IK10) ) / ( PZ(2,1,IK10+1) - PZ(2,1,IK10) )
          ZZCOEF11 = ( PALT - PZ(2,2,IK11) ) / ( PZ(2,2,IK11+1) - PZ(2,2,IK11) )

          CMNHMSG(1) = 'sensor ' // TRIM( TPSENSOR%CNAME ) // ' is near the ground'
          WRITE( CMNHMSG(2), "( 'at ', I2, '/', I2, '/', I4, ' ', F18.12, 's' )" ) &
                 TDTCUR%NDAY, TDTCUR%NMONTH, TDTCUR%NYEAR, TDTCUR%XTIME
          CALL PRINT_MSG( NVERB_INFO, 'GEN', 'Compute_vertical_interp_coeff', OLOCAL = .TRUE. )
        ELSE
          CALL PRINT_MSG( NVERB_WARNING, 'GEN', 'Compute_vertical_interp_coeff', &
                          'sensor ' // TRIM( TPSENSOR%CNAME ) // ' is too low', OLOCAL = .TRUE. )
          ZZCOEF00 = XUNDEF
          ZZCOEF01 = XUNDEF
          ZZCOEF10 = XUNDEF
          ZZCOEF11 = XUNDEF
        END IF
        GDONE = .TRUE.
      END IF

      ! In a separate if (compared to olow detection) to allow olow and ohigh=true simultaneaously (very rare event!)
      IF ( ANY( [ IK00, IK01, IK10, IK11 ] >= IKE ) ) THEN
        ! Sensor is high (above physical domain)
        OHIGH = .TRUE.

        ! Limit ik?? indices to prevent out of bound accesses
        IF ( IK00 > IKU-1) THEN
          IK00 = IKU-1
          GCHANGE = .TRUE.
        END IF
        IF ( IK01 > IKU-1) THEN
          IK01 = IKU-1
          GCHANGE = .TRUE.
        END IF
        IF ( IK10 > IKU-1) THEN
          IK10 = IKU-1
          GCHANGE = .TRUE.
        END IF
        IF ( IK11 > IKU-1) THEN
          IK11 = IKU-1
          GCHANGE = .TRUE.
        END IF

        CALL PRINT_MSG( NVERB_WARNING, 'GEN', 'Compute_vertical_interp_coeff', &
                        'sensor ' // TRIM( TPSENSOR%CNAME ) // ' is too high', OLOCAL = .TRUE. )
      END IF

      IF ( .NOT. GDONE ) THEN
        ZZCOEF00 = ( PALT - PZ(1,1,IK00) ) / ( PZ(1,1,IK00+1) - PZ(1,1,IK00) )
        ZZCOEF01 = ( PALT - PZ(1,2,IK01) ) / ( PZ(1,2,IK01+1) - PZ(1,2,IK01) )
        ZZCOEF10 = ( PALT - PZ(2,1,IK10) ) / ( PZ(2,1,IK10+1) - PZ(2,1,IK10) )
        ZZCOEF11 = ( PALT - PZ(2,2,IK11) ) / ( PZ(2,2,IK11+1) - PZ(2,2,IK11) )
      END IF


      SELECT CASE ( HPOS )
        CASE ( 'MASS' )
          TPSENSOR%NK00 = IK00
          TPSENSOR%NK01 = IK01
          TPSENSOR%NK10 = IK10
          TPSENSOR%NK11 = IK11

          TPSENSOR%XZCOEF00 = ZZCOEF00
          TPSENSOR%XZCOEF01 = ZZCOEF01
          TPSENSOR%XZCOEF10 = ZZCOEF10
          TPSENSOR%XZCOEF11 = ZZCOEF11

        CASE ( 'U' )
          TPSENSOR%NU00 = IK00
          TPSENSOR%NU01 = IK01
          TPSENSOR%NU10 = IK10
          TPSENSOR%NU11 = IK11

          TPSENSOR%XUCOEF00 = ZZCOEF00
          TPSENSOR%XUCOEF01 = ZZCOEF01
          TPSENSOR%XUCOEF10 = ZZCOEF10
          TPSENSOR%XUCOEF11 = ZZCOEF11

        CASE ( 'V' )
          TPSENSOR%NV00 = IK00
          TPSENSOR%NV01 = IK01
          TPSENSOR%NV10 = IK10
          TPSENSOR%NV11 = IK11

          TPSENSOR%XVCOEF00 = ZZCOEF00
          TPSENSOR%XVCOEF01 = ZZCOEF01
          TPSENSOR%XVCOEF10 = ZZCOEF10
          TPSENSOR%XVCOEF11 = ZZCOEF11

        CASE DEFAULT
          CALL PRINT_MSG( NVERB_ERROR, 'GEN', 'Compute_vertical_interp_coeff', 'invalid hpos', OLOCAL = .TRUE. )

      END SELECT
    END SUBROUTINE COMPUTE_VERTICAL_INTERP_COEFF


    ! #######################################################
    FUNCTION INTERP_FROM_MASSPOINT( TPSENSOR, PA ) RESULT( PB )
    ! #######################################################
      USE MODE_MSG

      CLASS(TSENSOR),         INTENT(IN) :: TPSENSOR
      REAL, DIMENSION(:,:,:), INTENT(IN) :: PA
      REAL                               :: PB

      INTEGER :: JI, JJ
      INTEGER :: IK00, IK01, IK10, IK11
      REAL    :: ZXMCOEF, ZYMCOEF
      REAL    :: ZZCOEF00, ZZCOEF01, ZZCOEF10, ZZCOEF11

      IF ( SIZE( PA, 1 ) == 2 ) THEN
        JI = 1
        JJ = 1
      ELSE
        JI = TPSENSOR%NI_M
        JJ = TPSENSOR%NJ_M
      END IF

      IK00 = TPSENSOR%NK00
      IK01 = TPSENSOR%NK01
      IK10 = TPSENSOR%NK10
      IK11 = TPSENSOR%NK11

      ZXMCOEF = TPSENSOR%XXMCOEF
      ZYMCOEF = TPSENSOR%XYMCOEF

      ZZCOEF00 = TPSENSOR%XZCOEF00
      ZZCOEF01 = TPSENSOR%XZCOEF01
      ZZCOEF10 = TPSENSOR%XZCOEF10
      ZZCOEF11 = TPSENSOR%XZCOEF11

      IF (       JI >= 1 .AND. JI < SIZE( PA, 1 ) &
           .AND. JJ >= 1 .AND. JJ < SIZE( PA, 2 ) &
           .AND. ALL( [IK00,IK01,IK10,IK11] >= 1 ) .AND. ALL( [IK00,IK01,IK10,IK11] < SIZE( PA, 3 ) ) ) THEN
        PB = (1.-ZXMCOEF) *  (1.-ZYMCOEF) * ( (1.-ZZCOEF00) * PA(JI  ,JJ  ,IK00) + ZZCOEF00 * PA(JI  ,JJ  ,IK00+1) ) + &
             (   ZXMCOEF) *  (1.-ZYMCOEF) * ( (1.-ZZCOEF10) * PA(JI+1,JJ  ,IK10) + ZZCOEF10 * PA(JI+1,JJ  ,IK10+1) ) + &
             (1.-ZXMCOEF) *  (   ZYMCOEF) * ( (1.-ZZCOEF01) * PA(JI  ,JJ+1,IK01) + ZZCOEF01 * PA(JI  ,JJ+1,IK01+1) ) + &
             (   ZXMCOEF) *  (   ZYMCOEF) * ( (1.-ZZCOEF11) * PA(JI+1,JJ+1,IK11) + ZZCOEF11 * PA(JI+1,JJ+1,IK11+1) )
      ELSE
        CALL PRINT_MSG( NVERB_ERROR, 'GEN', 'Interp_from_masspoint', 'value can not be interpolated', OLOCAL = .TRUE. )
        PB = XUNDEF
      END IF

    END FUNCTION INTERP_FROM_MASSPOINT


    ! ######################################################
    FUNCTION INTERP_FROM_UPOINT( TPSENSOR, PA ) RESULT( PB )
    ! ######################################################
      USE MODE_MSG

      CLASS(TSENSOR),         INTENT(IN) :: TPSENSOR
      REAL, DIMENSION(:,:,:), INTENT(IN) :: PA
      REAL                               :: PB

      INTEGER :: JI, JJ
      INTEGER :: IU00, IU01, IU10, IU11
      REAL    :: ZXUCOEF, ZYUCOEF
      REAL    :: ZUCOEF00, ZUCOEF01, ZUCOEF10, ZUCOEF11

      IF ( SIZE( PA, 1 ) == 2 ) THEN
        JI = 1
        JJ = 1
      ELSE
        JI = TPSENSOR%NI_U
        JJ = TPSENSOR%NJ_M
      END IF

      IU00 = TPSENSOR%NU00
      IU01 = TPSENSOR%NU01
      IU10 = TPSENSOR%NU10
      IU11 = TPSENSOR%NU11

      ZXUCOEF = TPSENSOR%XXUCOEF
      ZYUCOEF = TPSENSOR%XYMCOEF

      ZUCOEF00 = TPSENSOR%XUCOEF00
      ZUCOEF01 = TPSENSOR%XUCOEF01
      ZUCOEF10 = TPSENSOR%XUCOEF10
      ZUCOEF11 = TPSENSOR%XUCOEF11

      IF (       JI >= 1 .AND. JI < SIZE( PA, 1 ) &
           .AND. JJ >= 1 .AND. JJ < SIZE( PA, 2 ) &
           .AND. ALL( [IU00,IU01,IU10,IU11] >= 1 ) .AND. ALL( [IU00,IU01,IU10,IU11] < SIZE( PA, 3 ) ) ) THEN
        PB = (1.-ZXUCOEF) *  (1.-ZYUCOEF) * ( (1.-ZUCOEF00) * PA(JI  ,JJ  ,IU00) + ZUCOEF00 * PA(JI  ,JJ  ,IU00+1) ) + &
             (   ZXUCOEF) *  (1.-ZYUCOEF) * ( (1.-ZUCOEF10) * PA(JI+1,JJ  ,IU10) + ZUCOEF10 * PA(JI+1,JJ  ,IU10+1) ) + &
             (1.-ZXUCOEF) *  (   ZYUCOEF) * ( (1.-ZUCOEF01) * PA(JI  ,JJ+1,IU01) + ZUCOEF01 * PA(JI  ,JJ+1,IU01+1) ) + &
             (   ZXUCOEF) *  (   ZYUCOEF) * ( (1.-ZUCOEF11) * PA(JI+1,JJ+1,IU11) + ZUCOEF11 * PA(JI+1,JJ+1,IU11+1) )
      ELSE
        CALL PRINT_MSG( NVERB_ERROR, 'GEN', 'Interp_from_upoint', 'value can not be interpolated', OLOCAL = .TRUE. )
        PB = XUNDEF
      END IF

    END FUNCTION INTERP_FROM_UPOINT


    ! ######################################################
    FUNCTION INTERP_FROM_VPOINT( TPSENSOR, PA ) RESULT( PB )
    ! ######################################################
      USE MODE_MSG

      CLASS(TSENSOR),         INTENT(IN) :: TPSENSOR
      REAL, DIMENSION(:,:,:), INTENT(IN) :: PA
      REAL                               :: PB

      INTEGER :: JI, JJ
      INTEGER :: IV00, IV01, IV10, IV11
      REAL    :: ZXVCOEF, ZYVCOEF
      REAL    :: ZVCOEF00, ZVCOEF01, ZVCOEF10, ZVCOEF11

      IF ( SIZE( PA, 1 ) == 2 ) THEN
        JI = 1
        JJ = 1
      ELSE
        JI = TPSENSOR%NI_M
        JJ = TPSENSOR%NJ_V
      END IF

      IV00 = TPSENSOR%NV00
      IV01 = TPSENSOR%NV01
      IV10 = TPSENSOR%NV10
      IV11 = TPSENSOR%NV11

      ZXVCOEF = TPSENSOR%XXMCOEF
      ZYVCOEF = TPSENSOR%XYVCOEF

      ZVCOEF00 = TPSENSOR%XVCOEF00
      ZVCOEF01 = TPSENSOR%XVCOEF01
      ZVCOEF10 = TPSENSOR%XVCOEF10
      ZVCOEF11 = TPSENSOR%XVCOEF11

      IF (       JI >= 1 .AND. JI < SIZE( PA, 1 ) &
           .AND. JJ >= 1 .AND. JJ < SIZE( PA, 2 ) &
           .AND. ALL( [IV00,IV01,IV10,IV11] >= 1 ) .AND. ALL( [IV00,IV01,IV10,IV11] < SIZE( PA, 3 ) ) ) THEN
        PB = (1.-ZXVCOEF) *  (1.-ZYVCOEF) * ( (1.-ZVCOEF00) * PA(JI  ,JJ  ,IV00) + ZVCOEF00 * PA(JI  ,JJ  ,IV00+1) ) + &
             (   ZXVCOEF) *  (1.-ZYVCOEF) * ( (1.-ZVCOEF10) * PA(JI+1,JJ  ,IV10) + ZVCOEF10 * PA(JI+1,JJ  ,IV10+1) ) + &
             (1.-ZXVCOEF) *  (   ZYVCOEF) * ( (1.-ZVCOEF01) * PA(JI  ,JJ+1,IV01) + ZVCOEF01 * PA(JI  ,JJ+1,IV01+1) ) + &
             (   ZXVCOEF) *  (   ZYVCOEF) * ( (1.-ZVCOEF11) * PA(JI+1,JJ+1,IV11) + ZVCOEF11 * PA(JI+1,JJ+1,IV11+1) )
      ELSE
        CALL PRINT_MSG( NVERB_ERROR, 'GEN', 'Interp_from_vpoint', 'value can not be interpolated', OLOCAL = .TRUE. )
        PB = XUNDEF
      END IF

    END FUNCTION INTERP_FROM_VPOINT


    ! ################################################################
    FUNCTION INTERP_HOR_FROM_MASSPOINT_0D( TPSENSOR, PA ) RESULT( PB )
    ! ################################################################
      USE MODD_CONF,       ONLY: L1D
      USE MODD_PARAMETERS, ONLY: XUNDEF

      USE MODE_MSG

      IMPLICIT NONE

      CLASS(TSENSOR),       INTENT(IN) :: TPSENSOR
      REAL, DIMENSION(:,:), INTENT(IN) :: PA
      REAL                             :: PB

      REAL, DIMENSION(1) :: ZB

      ZB = INTERP_HOR_FROM_MASSPOINT_1D( TPSENSOR, RESHAPE( PA, [ SIZE(PA,1), SIZE(PA,2), 1 ] ) )
      PB = ZB(1)

    END FUNCTION INTERP_HOR_FROM_MASSPOINT_0D


    ! #############################################################
    FUNCTION INTERP_HOR_FROM_UPOINT_0D( TPSENSOR, PA ) RESULT( PB )
    ! #############################################################
      USE MODD_CONF,       ONLY: L1D

      USE MODE_MSG

      IMPLICIT NONE

      CLASS(TSENSOR),       INTENT(IN) :: TPSENSOR
      REAL, DIMENSION(:,:), INTENT(IN) :: PA
      REAL                             :: PB

      REAL, DIMENSION(1) :: ZB

      ZB = INTERP_HOR_FROM_UPOINT_1D( TPSENSOR, RESHAPE( PA, [ SIZE(PA,1), SIZE(PA,2), 1 ] ) )
      PB = ZB(1)

    END FUNCTION INTERP_HOR_FROM_UPOINT_0D


    ! #############################################################
    FUNCTION INTERP_HOR_FROM_VPOINT_0D( TPSENSOR, PA ) RESULT( PB )
    ! #############################################################
      USE MODD_CONF,       ONLY: L1D

      USE MODE_MSG

      IMPLICIT NONE

      CLASS(TSENSOR),       INTENT(IN) :: TPSENSOR
      REAL, DIMENSION(:,:), INTENT(IN) :: PA
      REAL                             :: PB

      REAL, DIMENSION(1) :: ZB

      ZB = INTERP_HOR_FROM_VPOINT_1D( TPSENSOR, RESHAPE( PA, [ SIZE(PA,1), SIZE(PA,2), 1 ] ) )
      PB = ZB(1)

    END FUNCTION INTERP_HOR_FROM_VPOINT_0D


    ! ################################################################
    FUNCTION INTERP_HOR_FROM_MASSPOINT_1D( TPSENSOR, PA ) RESULT( PB )
    ! ################################################################
      USE MODD_CONF,       ONLY: L1D

      USE MODE_MSG

      CLASS(TSENSOR),         INTENT(IN) :: TPSENSOR
      REAL, DIMENSION(:,:,:), INTENT(IN) :: PA
      REAL, DIMENSION( SIZE( PA, 3 ) )   :: PB

      INTEGER :: JI, JJ, JK

      IF ( SIZE( PA, 1 ) == 2 ) THEN
        JI = 1
        JJ = 1
      ELSE IF ( L1D ) THEN
        JI = 2
        JJ = 2
      ELSE
        JI = TPSENSOR%NI_M
        JJ = TPSENSOR%NJ_M
      END IF

      IF (       JI >= 1 .AND. JI < SIZE( PA, 1 ) &
           .AND. JJ >= 1 .AND. JJ < SIZE( PA, 2 ) ) THEN
        DO JK = 1, SIZE( PA, 3 )
          IF ( PA(JI, JJ,   JK) /= XUNDEF .AND. PA(JI+1, JJ,   JK) /= XUNDEF .AND. &
               PA(JI, JJ+1, JK) /= XUNDEF .AND. PA(JI+1, JJ+1, JK) /= XUNDEF       ) THEN
            PB(JK) = (1.-TPSENSOR%XXMCOEF) *  (1.-TPSENSOR%XYMCOEF) * PA(JI,   JJ,   JK) + &
                     (   TPSENSOR%XXMCOEF) *  (1.-TPSENSOR%XYMCOEF) * PA(JI+1, JJ,   JK) + &
                     (1.-TPSENSOR%XXMCOEF) *  (   TPSENSOR%XYMCOEF) * PA(JI,   JJ+1, JK) + &
                     (   TPSENSOR%XXMCOEF) *  (   TPSENSOR%XYMCOEF) * PA(JI+1, JJ+1, JK)
          ELSE
            PB(JK) = XUNDEF
          END IF
        END DO
      ELSE
        CALL PRINT_MSG( NVERB_ERROR, 'GEN', 'Interp_hor_from_masspoint', 'value can not be interpolated', OLOCAL = .TRUE. )
        PB(:) = XUNDEF
      END IF

    END FUNCTION INTERP_HOR_FROM_MASSPOINT_1D


    ! #############################################################
    FUNCTION INTERP_HOR_FROM_UPOINT_1D( TPSENSOR, PA ) RESULT( PB )
    ! #############################################################
      USE MODD_CONF,       ONLY: L1D

      USE MODE_MSG

      IMPLICIT NONE

      CLASS(TSENSOR),         INTENT(IN) :: TPSENSOR
      REAL, DIMENSION(:,:,:), INTENT(IN) :: PA
      REAL, DIMENSION( SIZE( PA, 3 ) )   :: PB

      INTEGER :: JI, JJ

      IF ( SIZE( PA, 1 ) == 2 ) THEN
        JI = 1
        JJ = 1
      ELSE IF ( L1D ) THEN
        JI = 2
        JJ = 2
      ELSE
        JI = TPSENSOR%NI_U
        JJ = TPSENSOR%NJ_M
      END IF

      IF (       JI >= 1 .AND. JI < SIZE( PA, 1 ) &
           .AND. JJ >= 1 .AND. JJ < SIZE( PA, 2 ) ) THEN
        PB(:) = (1.-TPSENSOR%XXUCOEF) *  (1.-TPSENSOR%XYMCOEF) * PA(JI,   JJ,   :) + &
                (   TPSENSOR%XXUCOEF) *  (1.-TPSENSOR%XYMCOEF) * PA(JI+1, JJ,   :) + &
                (1.-TPSENSOR%XXUCOEF) *  (   TPSENSOR%XYMCOEF) * PA(JI,   JJ+1, :) + &
                (   TPSENSOR%XXUCOEF) *  (   TPSENSOR%XYMCOEF) * PA(JI+1, JJ+1, :)
      ELSE
        CALL PRINT_MSG( NVERB_ERROR, 'GEN', 'Interp_hor_from_upoint', 'value can not be interpolated', OLOCAL = .TRUE. )
        PB(:) = XUNDEF
      END IF

    END FUNCTION INTERP_HOR_FROM_UPOINT_1D


    ! #############################################################
    FUNCTION INTERP_HOR_FROM_VPOINT_1D( TPSENSOR, PA ) RESULT( PB )
    ! #############################################################
      USE MODD_CONF,       ONLY: L1D

      USE MODE_MSG

      IMPLICIT NONE

      CLASS(TSENSOR),         INTENT(IN) :: TPSENSOR
      REAL, DIMENSION(:,:,:), INTENT(IN) :: PA
      REAL, DIMENSION( SIZE( PA, 3 ) )   :: PB

      INTEGER :: JI, JJ

      IF ( SIZE( PA, 1 ) == 2 ) THEN
        JI = 1
        JJ = 1
      ELSE IF ( L1D ) THEN
        JI = 2
        JJ = 2
      ELSE
        JI = TPSENSOR%NI_M
        JJ = TPSENSOR%NJ_V
      END IF
      IF (       JI >= 1 .AND. JI < SIZE( PA, 1 ) &
           .AND. JJ >= 1 .AND. JJ < SIZE( PA, 2 ) ) THEN
        PB(:) = (1.-TPSENSOR%XXMCOEF) *  (1.-TPSENSOR%XYVCOEF) * PA(JI,   JJ,   :) + &
                (   TPSENSOR%XXMCOEF) *  (1.-TPSENSOR%XYVCOEF) * PA(JI+1, JJ,   :) + &
                (1.-TPSENSOR%XXMCOEF) *  (   TPSENSOR%XYVCOEF) * PA(JI,   JJ+1, :) + &
                (   TPSENSOR%XXMCOEF) *  (   TPSENSOR%XYVCOEF) * PA(JI+1, JJ+1, :)
      ELSE
        CALL PRINT_MSG( NVERB_ERROR, 'GEN', 'Interp_hor_from_vpoint', 'value can not be interpolated', OLOCAL = .TRUE. )
        PB(:) = XUNDEF
      END IF

    END FUNCTION INTERP_HOR_FROM_VPOINT_1D

    ! ######################################################################
    FUNCTION BUFFER_SIZE_COMPUTE( TPSENSOR, KSTORE_CURRENT ) RESULT( KSIZE )
    ! ######################################################################

      USE MODE_MSG

      CLASS(TSENSOR),           INTENT(IN) :: TPSENSOR
      INTEGER,        OPTIONAL, INTENT(IN) :: KSTORE_CURRENT ! Current number of stored instants
      INTEGER                              :: KSIZE

      INTEGER :: ISTORES

      IF ( PRESENT( KSTORE_CURRENT ) ) THEN
        ISTORES = KSTORE_CURRENT
        IF ( ISTORES > TPSENSOR%NSTORE_MAX ) THEN
          CALL PRINT_MSG( NVERB_ERROR, 'GEN', 'Buffer_size_compute', 'sensor: ' // TRIM(TPSENSOR%CNAME) &
                           // ': kstore_current>nstore_max', OLOCAL = .TRUE. )
          ISTORES = TPSENSOR%NSTORE_MAX
        END IF
      ELSE
        ISTORES = TPSENSOR%NSTORE_MAX
      END IF

      KSIZE = TPSENSOR%NBUFFER_FIXSIZE + ISTORES * TPSENSOR%NBUFFER_VARSIZE

    END FUNCTION BUFFER_SIZE_COMPUTE


    ! ######################################################################
    SUBROUTINE BUFFER_PACK_SENSOR( TPSENSOR, PBUFFER, KPOS, KSTORE_CURRENT )
    ! ######################################################################

      USE MODD_CONF_N,     ONLY: NRR
      USE MODD_DIM_N,      ONLY: NKMAX
      USE MODD_NSV,        ONLY: NSV
      USE MODD_PARAMETERS, ONLY: JPVEXT
      USE MODD_PARAM_N,    ONLY: CCLOUD, CRAD, CTURB

      USE MODE_MSG

      CLASS(TSENSOR),               INTENT(IN)    :: TPSENSOR
      REAL, DIMENSION(:),           INTENT(INOUT) :: PBUFFER        ! Buffer to pack
      INTEGER,                      INTENT(INOUT) :: KPOS           ! Position in the buffer
      INTEGER,            OPTIONAL, INTENT(IN)    :: KSTORE_CURRENT ! Current number of stored instants

      INTEGER :: IKU     ! number of vertical levels for profile
      INTEGER :: ILEVELS
      INTEGER :: ISTORES
      INTEGER :: ILVST   ! =ilevels*istores
      INTEGER :: JI

      IKU = NKMAX + 2 * JPVEXT
      ILEVELS = SIZE( TPSENSOR%XZON, 1 )

      IF ( PRESENT( KSTORE_CURRENT ) ) THEN
        ISTORES = KSTORE_CURRENT
        IF ( ISTORES > TPSENSOR%NSTORE_MAX ) THEN
          CALL PRINT_MSG( NVERB_ERROR, 'GEN', 'Buffer_pack_sensor', 'sensor: ' // TRIM(TPSENSOR%CNAME) &
                          // ': kstore_current>nstore_max', OLOCAL = .TRUE. )
          ISTORES = TPSENSOR%NSTORE_MAX
        END IF
      ELSE
        ISTORES = TPSENSOR%NSTORE_MAX
      END IF

      IF ( KPOS /= 1 )                                                                                 &
        CALL PRINT_MSG( NVERB_WARNING, 'GEN', 'Buffer_pack_sensor', 'sensor: ' // TRIM(TPSENSOR%CNAME) &
                        // ': initial position of buffer not at its beginning', OLOCAL = .TRUE. )

      ILVST = ILEVELS * ISTORES

      ! Convert title characters to integers
      DO JI = 1, LEN( TPSENSOR%CNAME )
        PBUFFER(KPOS) = ICHAR( TPSENSOR%CNAME(JI:JI) )
        KPOS = KPOS + 1
      END DO

      DO JI = 1, LEN( TPSENSOR%CTYPE )
        PBUFFER(KPOS) = ICHAR( TPSENSOR%CTYPE(JI:JI) )
        KPOS = KPOS + 1
      END DO

      PBUFFER(KPOS) = TPSENSOR%NID        ; KPOS = KPOS + 1
      PBUFFER(KPOS) = TPSENSOR%NSTORE_CUR ; KPOS = KPOS + 1
      PBUFFER(KPOS) = TPSENSOR%NSTORE_MAX ; KPOS = KPOS + 1

      PBUFFER(KPOS) = TPSENSOR%NBUFFER_FIXSIZE ; KPOS = KPOS + 1
      PBUFFER(KPOS) = TPSENSOR%NBUFFER_VARSIZE ; KPOS = KPOS + 1

      IF ( TPSENSOR%LFIX ) THEN
        PBUFFER(KPOS) = 1.D0
      ELSE
        PBUFFER(KPOS) = 0.D0
      END IF
      KPOS = KPOS + 1

      PBUFFER(KPOS) = TPSENSOR%XX_CUR   ; KPOS = KPOS + 1
      PBUFFER(KPOS) = TPSENSOR%XY_CUR   ; KPOS = KPOS + 1
      PBUFFER(KPOS) = TPSENSOR%XZ_CUR   ; KPOS = KPOS + 1
      PBUFFER(KPOS) = TPSENSOR%XLAT_CUR ; KPOS = KPOS + 1
      PBUFFER(KPOS) = TPSENSOR%XLON_CUR ; KPOS = KPOS + 1

      PBUFFER(KPOS) = TPSENSOR%NI_M ; KPOS = KPOS + 1
      PBUFFER(KPOS) = TPSENSOR%NJ_M ; KPOS = KPOS + 1
      PBUFFER(KPOS) = TPSENSOR%NI_U ; KPOS = KPOS + 1
      PBUFFER(KPOS) = TPSENSOR%NJ_V ; KPOS = KPOS + 1

      PBUFFER(KPOS) = TPSENSOR%NK00 ; KPOS = KPOS + 1
      PBUFFER(KPOS) = TPSENSOR%NK01 ; KPOS = KPOS + 1
      PBUFFER(KPOS) = TPSENSOR%NK10 ; KPOS = KPOS + 1
      PBUFFER(KPOS) = TPSENSOR%NK11 ; KPOS = KPOS + 1
      PBUFFER(KPOS) = TPSENSOR%NU00 ; KPOS = KPOS + 1
      PBUFFER(KPOS) = TPSENSOR%NU01 ; KPOS = KPOS + 1
      PBUFFER(KPOS) = TPSENSOR%NU10 ; KPOS = KPOS + 1
      PBUFFER(KPOS) = TPSENSOR%NU11 ; KPOS = KPOS + 1
      PBUFFER(KPOS) = TPSENSOR%NV00 ; KPOS = KPOS + 1
      PBUFFER(KPOS) = TPSENSOR%NV01 ; KPOS = KPOS + 1
      PBUFFER(KPOS) = TPSENSOR%NV10 ; KPOS = KPOS + 1
      PBUFFER(KPOS) = TPSENSOR%NV11 ; KPOS = KPOS + 1

      PBUFFER(KPOS) = TPSENSOR%XXMCOEF ; KPOS = KPOS + 1
      PBUFFER(KPOS) = TPSENSOR%XYMCOEF ; KPOS = KPOS + 1
      PBUFFER(KPOS) = TPSENSOR%XXUCOEF ; KPOS = KPOS + 1
      PBUFFER(KPOS) = TPSENSOR%XYVCOEF ; KPOS = KPOS + 1

      PBUFFER(KPOS) = TPSENSOR%XZCOEF00 ; KPOS = KPOS + 1
      PBUFFER(KPOS) = TPSENSOR%XZCOEF01 ; KPOS = KPOS + 1
      PBUFFER(KPOS) = TPSENSOR%XZCOEF10 ; KPOS = KPOS + 1
      PBUFFER(KPOS) = TPSENSOR%XZCOEF11 ; KPOS = KPOS + 1
      PBUFFER(KPOS) = TPSENSOR%XUCOEF00 ; KPOS = KPOS + 1
      PBUFFER(KPOS) = TPSENSOR%XUCOEF01 ; KPOS = KPOS + 1
      PBUFFER(KPOS) = TPSENSOR%XUCOEF10 ; KPOS = KPOS + 1
      PBUFFER(KPOS) = TPSENSOR%XUCOEF11 ; KPOS = KPOS + 1
      PBUFFER(KPOS) = TPSENSOR%XVCOEF00 ; KPOS = KPOS + 1
      PBUFFER(KPOS) = TPSENSOR%XVCOEF01 ; KPOS = KPOS + 1
      PBUFFER(KPOS) = TPSENSOR%XVCOEF10 ; KPOS = KPOS + 1
      PBUFFER(KPOS) = TPSENSOR%XVCOEF11 ; KPOS = KPOS + 1

      PBUFFER(KPOS:KPOS+ILVST-1) = RESHAPE( TPSENSOR%XZON(:,1:ISTORES), [ILVST] ) ; KPOS = KPOS + ILVST
      PBUFFER(KPOS:KPOS+ILVST-1) = RESHAPE( TPSENSOR%XMER(:,1:ISTORES), [ILVST] ) ; KPOS = KPOS + ILVST
      PBUFFER(KPOS:KPOS+ILVST-1) = RESHAPE( TPSENSOR%XW  (:,1:ISTORES), [ILVST] ) ; KPOS = KPOS + ILVST
      PBUFFER(KPOS:KPOS+ILVST-1) = RESHAPE( TPSENSOR%XP  (:,1:ISTORES), [ILVST] ) ; KPOS = KPOS + ILVST
      IF ( CTURB == 'TKEL') THEN
        PBUFFER(KPOS:KPOS+ILVST-1) = RESHAPE( TPSENSOR%XTKE(:,1:ISTORES), [ILVST] ) ; KPOS = KPOS + ILVST
      END IF
      PBUFFER(KPOS:KPOS+ILVST-1)     = RESHAPE( TPSENSOR%XTH(:,1:ISTORES),   [ILVST] )      ; KPOS = KPOS + ILVST
      PBUFFER(KPOS:KPOS+ILVST*NRR-1) = RESHAPE( TPSENSOR%XR (:,1:ISTORES,:), [ILVST*NRR]  ) ; KPOS = KPOS + ILVST * NRR
      PBUFFER(KPOS:KPOS+ILVST*NSV-1) = RESHAPE( TPSENSOR%XSV(:,1:ISTORES,:), [ILVST*NSV]  ) ; KPOS = KPOS + ILVST * NSV
      IF ( CRAD /= 'NONE' ) THEN
        PBUFFER(KPOS:KPOS+ISTORES-1) = TPSENSOR%XTSRAD(1:ISTORES) ; KPOS = KPOS + ISTORES
      END IF

      IF ( SIZE( TPSENSOR%XIWCZ ) > 0 ) THEN
        IF ( CCLOUD(1:3) == 'ICE') THEN
          PBUFFER(KPOS:KPOS+IKU*ISTORES-1) = RESHAPE( TPSENSOR%XCIZ    (:,1:ISTORES), [IKU*ISTORES] ) ; KPOS = KPOS + IKU * ISTORES
        END IF
        IF ( CCLOUD == 'LIMA') THEN
          PBUFFER(KPOS:KPOS+IKU*ISTORES-1) = RESHAPE( TPSENSOR%XCIZ    (:,1:ISTORES), [IKU*ISTORES] ) ; KPOS = KPOS + IKU * ISTORES
          PBUFFER(KPOS:KPOS+IKU*ISTORES-1) = RESHAPE( TPSENSOR%XCCZ    (:,1:ISTORES), [IKU*ISTORES] ) ; KPOS = KPOS + IKU * ISTORES
          PBUFFER(KPOS:KPOS+IKU*ISTORES-1) = RESHAPE( TPSENSOR%XCRZ    (:,1:ISTORES), [IKU*ISTORES] ) ; KPOS = KPOS + IKU * ISTORES
        END IF
        PBUFFER(KPOS:KPOS+IKU*ISTORES-1) = RESHAPE( TPSENSOR%XIWCZ     (:,1:ISTORES), [IKU*ISTORES] ) ; KPOS = KPOS + IKU * ISTORES
        PBUFFER(KPOS:KPOS+IKU*ISTORES-1) = RESHAPE( TPSENSOR%XLWCZ     (:,1:ISTORES), [IKU*ISTORES] ) ; KPOS = KPOS + IKU * ISTORES
        PBUFFER(KPOS:KPOS+IKU*ISTORES-1) = RESHAPE( TPSENSOR%XCRARE    (:,1:ISTORES), [IKU*ISTORES] ) ; KPOS = KPOS + IKU * ISTORES
        PBUFFER(KPOS:KPOS+IKU*ISTORES-1) = RESHAPE( TPSENSOR%XCRARE_ATT(:,1:ISTORES), [IKU*ISTORES] ) ; KPOS = KPOS + IKU * ISTORES
      END IF

    END SUBROUTINE BUFFER_PACK_SENSOR


    ! ################################################################
    SUBROUTINE BUFFER_UNPACK_SENSOR( TPSENSOR, PBUFFER, KPOS, KSTORE )
    ! ################################################################

      USE MODD_CONF_N,     ONLY: NRR
      USE MODD_DIM_N,      ONLY: NKMAX
      USE MODD_NSV,        ONLY: NSV
      USE MODD_PARAMETERS, ONLY: JPVEXT
      USE MODD_PARAM_N,    ONLY: CCLOUD, CRAD, CTURB

      USE MODE_MSG

      CLASS(TSENSOR),     INTENT(INOUT) :: TPSENSOR
      REAL, DIMENSION(:), INTENT(IN)    :: PBUFFER  ! Buffer to unpack
      INTEGER,            INTENT(INOUT) :: KPOS     ! Position in the buffer
      INTEGER,            INTENT(IN)    :: KSTORE   ! Current number of stored instants

      INTEGER :: IKU     ! number of vertical levels for profile
      INTEGER :: ILEVELS
      INTEGER :: ILVST   ! =ilevels*kstore_cur
      INTEGER :: JI

      IKU = NKMAX + 2 * JPVEXT
      ILEVELS = SIZE( TPSENSOR%XZON, 1 )
      ILVST = ILEVELS * KSTORE

      ! Convert integers to characters for title
      DO JI = 1, LEN( TPSENSOR%CNAME )
        TPSENSOR%CNAME(JI:JI) = ACHAR( NINT( PBUFFER(KPOS) ) )
        KPOS = KPOS + 1
      END DO

      DO JI = 1, LEN( TPSENSOR%CTYPE )
        TPSENSOR%CTYPE(JI:JI) = ACHAR( NINT( PBUFFER(KPOS) ) )
        KPOS = KPOS + 1
      END DO

      TPSENSOR%NID        = NINT( PBUFFER(KPOS) ) ; KPOS = KPOS + 1
      TPSENSOR%NSTORE_CUR = NINT( PBUFFER(KPOS) ) ; KPOS = KPOS + 1
      TPSENSOR%NSTORE_MAX = NINT( PBUFFER(KPOS) ) ; KPOS = KPOS + 1

      TPSENSOR%NBUFFER_FIXSIZE = NINT( PBUFFER(KPOS) ) ; KPOS = KPOS + 1
      TPSENSOR%NBUFFER_VARSIZE = NINT( PBUFFER(KPOS) ) ; KPOS = KPOS + 1

      IF ( NINT( PBUFFER(KPOS) ) == 0 ) THEN
        TPSENSOR%LFIX = .FALSE.
      ELSE
        TPSENSOR%LFIX = .TRUE.
      END IF
      KPOS = KPOS + 1

      TPSENSOR%XX_CUR   = PBUFFER(KPOS) ; KPOS = KPOS + 1
      TPSENSOR%XY_CUR   = PBUFFER(KPOS) ; KPOS = KPOS + 1
      TPSENSOR%XZ_CUR   = PBUFFER(KPOS) ; KPOS = KPOS + 1
      TPSENSOR%XLAT_CUR = PBUFFER(KPOS) ; KPOS = KPOS + 1
      TPSENSOR%XLON_CUR = PBUFFER(KPOS) ; KPOS = KPOS + 1

      TPSENSOR%NI_M = NINT( PBUFFER(KPOS) ) ; KPOS = KPOS + 1
      TPSENSOR%NJ_M = NINT( PBUFFER(KPOS) ) ; KPOS = KPOS + 1
      TPSENSOR%NI_U = NINT( PBUFFER(KPOS) ) ; KPOS = KPOS + 1
      TPSENSOR%NJ_V = NINT( PBUFFER(KPOS) ) ; KPOS = KPOS + 1

      TPSENSOR%NK00 = NINT( PBUFFER(KPOS) ) ; KPOS = KPOS + 1
      TPSENSOR%NK01 = NINT( PBUFFER(KPOS) ) ; KPOS = KPOS + 1
      TPSENSOR%NK10 = NINT( PBUFFER(KPOS) ) ; KPOS = KPOS + 1
      TPSENSOR%NK11 = NINT( PBUFFER(KPOS) ) ; KPOS = KPOS + 1
      TPSENSOR%NU00 = NINT( PBUFFER(KPOS) ) ; KPOS = KPOS + 1
      TPSENSOR%NU01 = NINT( PBUFFER(KPOS) ) ; KPOS = KPOS + 1
      TPSENSOR%NU10 = NINT( PBUFFER(KPOS) ) ; KPOS = KPOS + 1
      TPSENSOR%NU11 = NINT( PBUFFER(KPOS) ) ; KPOS = KPOS + 1
      TPSENSOR%NV00 = NINT( PBUFFER(KPOS) ) ; KPOS = KPOS + 1
      TPSENSOR%NV01 = NINT( PBUFFER(KPOS) ) ; KPOS = KPOS + 1
      TPSENSOR%NV10 = NINT( PBUFFER(KPOS) ) ; KPOS = KPOS + 1
      TPSENSOR%NV11 = NINT( PBUFFER(KPOS) ) ; KPOS = KPOS + 1

      TPSENSOR%XXMCOEF = PBUFFER(KPOS) ; KPOS = KPOS + 1
      TPSENSOR%XYMCOEF = PBUFFER(KPOS) ; KPOS = KPOS + 1
      TPSENSOR%XXUCOEF = PBUFFER(KPOS) ; KPOS = KPOS + 1
      TPSENSOR%XYVCOEF = PBUFFER(KPOS) ; KPOS = KPOS + 1

      TPSENSOR%XZCOEF00 = PBUFFER(KPOS) ; KPOS = KPOS + 1
      TPSENSOR%XZCOEF01 = PBUFFER(KPOS) ; KPOS = KPOS + 1
      TPSENSOR%XZCOEF10 = PBUFFER(KPOS) ; KPOS = KPOS + 1
      TPSENSOR%XZCOEF11 = PBUFFER(KPOS) ; KPOS = KPOS + 1
      TPSENSOR%XUCOEF00 = PBUFFER(KPOS) ; KPOS = KPOS + 1
      TPSENSOR%XUCOEF01 = PBUFFER(KPOS) ; KPOS = KPOS + 1
      TPSENSOR%XUCOEF10 = PBUFFER(KPOS) ; KPOS = KPOS + 1
      TPSENSOR%XUCOEF11 = PBUFFER(KPOS) ; KPOS = KPOS + 1
      TPSENSOR%XVCOEF00 = PBUFFER(KPOS) ; KPOS = KPOS + 1
      TPSENSOR%XVCOEF01 = PBUFFER(KPOS) ; KPOS = KPOS + 1
      TPSENSOR%XVCOEF10 = PBUFFER(KPOS) ; KPOS = KPOS + 1
      TPSENSOR%XVCOEF11 = PBUFFER(KPOS) ; KPOS = KPOS + 1

      TPSENSOR%XZON(:,1:KSTORE) = RESHAPE( PBUFFER(KPOS:KPOS+ILVST-1), [ ILEVELS, KSTORE ] ) ; KPOS = KPOS + ILVST
      TPSENSOR%XMER(:,1:KSTORE) = RESHAPE( PBUFFER(KPOS:KPOS+ILVST-1), [ ILEVELS, KSTORE ] ) ; KPOS = KPOS + ILVST
      TPSENSOR%XW  (:,1:KSTORE) = RESHAPE( PBUFFER(KPOS:KPOS+ILVST-1), [ ILEVELS, KSTORE ] ) ; KPOS = KPOS + ILVST
      TPSENSOR%XP  (:,1:KSTORE) = RESHAPE( PBUFFER(KPOS:KPOS+ILVST-1), [ ILEVELS, KSTORE ] ) ; KPOS = KPOS + ILVST
      IF ( CTURB == 'TKEL') THEN
        TPSENSOR%XTKE(:,1:KSTORE) = RESHAPE( PBUFFER(KPOS:KPOS+ILVST-1), [ ILEVELS, KSTORE ] ) ; KPOS = KPOS + ILVST
      END IF
      TPSENSOR%XTH(:,1:KSTORE)   = RESHAPE( PBUFFER(KPOS:KPOS+ILVST-1),     [ ILEVELS, KSTORE ] )      ; KPOS = KPOS + ILVST
      TPSENSOR%XR (:,1:KSTORE,:) = RESHAPE( PBUFFER(KPOS:KPOS+ILVST*NRR-1), [ ILEVELS, KSTORE, NRR ] ) ; KPOS = KPOS + ILVST * NRR
      TPSENSOR%XSV(:,1:KSTORE,:) = RESHAPE( PBUFFER(KPOS:KPOS+ILVST*NSV-1), [ ILEVELS, KSTORE, NSV ] ) ; KPOS = KPOS + ILVST * NSV
      IF ( CRAD /= 'NONE' ) THEN
        TPSENSOR%XTSRAD(1:KSTORE) = PBUFFER(KPOS:KPOS+KSTORE-1) ; KPOS = KPOS + KSTORE
      END IF

      IF ( SIZE( TPSENSOR%XIWCZ ) > 0 ) THEN
        IF ( CCLOUD(1:3) == 'ICE' ) THEN
          TPSENSOR%XCIZ    (:,1:KSTORE) = RESHAPE( PBUFFER(KPOS:KPOS+IKU*KSTORE-1), [ IKU, KSTORE ] ) ; KPOS = KPOS + IKU * KSTORE
        END IF
        IF ( CCLOUD == 'LIMA' ) THEN
          TPSENSOR%XCIZ    (:,1:KSTORE) = RESHAPE( PBUFFER(KPOS:KPOS+IKU*KSTORE-1), [ IKU, KSTORE ] ) ; KPOS = KPOS + IKU * KSTORE
          TPSENSOR%XCCZ    (:,1:KSTORE) = RESHAPE( PBUFFER(KPOS:KPOS+IKU*KSTORE-1), [ IKU, KSTORE ] ) ; KPOS = KPOS + IKU * KSTORE
          TPSENSOR%XCRZ    (:,1:KSTORE) = RESHAPE( PBUFFER(KPOS:KPOS+IKU*KSTORE-1), [ IKU, KSTORE ] ) ; KPOS = KPOS + IKU * KSTORE
        END IF
        TPSENSOR%XIWCZ     (:,1:KSTORE) = RESHAPE( PBUFFER(KPOS:KPOS+IKU*KSTORE-1), [ IKU, KSTORE ] ) ; KPOS = KPOS + IKU * KSTORE
        TPSENSOR%XLWCZ     (:,1:KSTORE) = RESHAPE( PBUFFER(KPOS:KPOS+IKU*KSTORE-1), [ IKU, KSTORE ] ) ; KPOS = KPOS + IKU * KSTORE
        TPSENSOR%XCRARE    (:,1:KSTORE) = RESHAPE( PBUFFER(KPOS:KPOS+IKU*KSTORE-1), [ IKU, KSTORE ] ) ; KPOS = KPOS + IKU * KSTORE
        TPSENSOR%XCRARE_ATT(:,1:KSTORE) = RESHAPE( PBUFFER(KPOS:KPOS+IKU*KSTORE-1), [ IKU, KSTORE ] ) ; KPOS = KPOS + IKU * KSTORE
      END IF

    END SUBROUTINE BUFFER_UNPACK_SENSOR


    ! #################################################################
    SUBROUTINE BUFFER_SIZE_SEND( TPSENSOR, KSTORE_CUR, KPACKSIZE, KTO )
    ! #################################################################

      !Workaround problem with Intel MPI module (do not use only for MPI subroutines)
      ! use modd_mpif,      only: MPI_SEND
      USE MODD_MPIF
      USE MODD_PRECISION, ONLY: MNHINT_MPI
      USE MODD_VAR_LL,    ONLY: NMNH_COMM_WORLD

      CLASS(TSENSOR), INTENT(IN) :: TPSENSOR
      INTEGER,        INTENT(IN) :: KSTORE_CUR
      INTEGER,        INTENT(IN) :: KPACKSIZE
      INTEGER,        INTENT(IN) :: KTO         ! Process to which to send sensor data

      INTEGER               :: IERR
      INTEGER, DIMENSION(3) :: ISTORES

      ISTORES(1) = KSTORE_CUR          ! Number of currently used store-positions
      ISTORES(2) = TPSENSOR%NSTORE_MAX ! Total number of store positions
      ISTORES(3) = KPACKSIZE           ! Total size of the buffer

      CALL MPI_SEND( ISTORES, 3, MNHINT_MPI, KTO-1, NTAG_NCUR, NMNH_COMM_WORLD, IERR )

    END SUBROUTINE BUFFER_SIZE_SEND


    ! ###############################################################################
    SUBROUTINE BUFFER_SIZE_RECV( TPSENSOR, KSTORE_CUR, KSTORE_TOT, KPACKSIZE, KFROM )
    ! ###############################################################################

      !Workaround problem with Intel MPI module (do not use only for MPI subroutines)
      ! USE MODD_MPIF,      ONLY: MPI_RECV, MPI_STATUS_IGNORE
      USE MODD_MPIF
      USE MODD_PRECISION, ONLY: MNHINT_MPI
      USE MODD_VAR_LL,    ONLY: NMNH_COMM_WORLD

      CLASS(TSENSOR), INTENT(IN)  :: TPSENSOR
      INTEGER,        INTENT(OUT) :: KSTORE_CUR
      INTEGER,        INTENT(OUT) :: KSTORE_TOT
      INTEGER,        INTENT(OUT) :: KPACKSIZE
      INTEGER,        INTENT(IN)  :: KFROM     ! Process from which to receive sensor data

      INTEGER               :: IERR
      INTEGER, DIMENSION(3) :: ISTORES

      CALL MPI_RECV( ISTORES, 3, MNHINT_MPI, KFROM-1, NTAG_NCUR, NMNH_COMM_WORLD, MPI_STATUS_IGNORE, IERR )

      KSTORE_CUR = ISTORES(1)
      KSTORE_TOT = ISTORES(2)
      KPACKSIZE  = ISTORES(3)

    END SUBROUTINE BUFFER_SIZE_RECV


    ! ##############################################
    SUBROUTINE BUFFER_SEND( TPSENSOR, PBUFFER, KTO )
    ! ##############################################

      !Workaround problem with Intel MPI module (do not use only for MPI subroutines)
      ! USE MODD_MPIF,      ONLY: MPI_SEND
      USE MODD_MPIF
      USE MODD_PRECISION, ONLY: MNHREAL_MPI
      USE MODD_VAR_LL,    ONLY: NMNH_COMM_WORLD

      CLASS(TSENSOR),     INTENT(IN) :: TPSENSOR
      REAL, DIMENSION(:), INTENT(IN) :: PBUFFER
      INTEGER,            INTENT(IN) :: KTO      ! Process to which to send buffer

      INTEGER :: IERR

      ! Send packed data
      CALL MPI_SEND( PBUFFER, SIZE( PBUFFER ), MNHREAL_MPI, KTO-1, NTAG_PACK, NMNH_COMM_WORLD, IERR )

    END SUBROUTINE BUFFER_SEND


    ! ################################################
    SUBROUTINE BUFFER_RECV( TPSENSOR, PBUFFER, KFROM )
    ! ################################################

      !Workaround problem with Intel MPI module (do not use only for MPI subroutines)
      ! USE MODD_MPIF,      ONLY: MPI_RECV, MPI_STATUS_IGNORE
      USE MODD_MPIF
      USE MODD_PRECISION, ONLY: MNHREAL_MPI
      USE MODD_VAR_LL,    ONLY: NMNH_COMM_WORLD

      CLASS(TSENSOR),     INTENT(IN)  :: TPSENSOR
      REAL, DIMENSION(:), INTENT(OUT) :: PBUFFER
      INTEGER,            INTENT(IN)  :: KFROM    ! Process from which to receive buffer

      INTEGER :: IERR

      ! Receive packed data
      CALL MPI_RECV( PBUFFER, SIZE( PBUFFER ), MNHREAL_MPI, KFROM-1, NTAG_PACK, NMNH_COMM_WORLD, MPI_STATUS_IGNORE, IERR )

    END SUBROUTINE BUFFER_RECV

    ! ##################################################################
    SUBROUTINE SENSOR_COMM_SEND( TPSENSOR, KTO, OSEND_SIZE_TO_RECEIVER )
    ! ##################################################################

      USE MODD_IO, ONLY: ISP

      USE MODE_MSG

      CLASS(TSENSOR),           INTENT(INOUT) :: TPSENSOR
      INTEGER,                  INTENT(IN)    :: KTO                    ! Process to which to send data
      LOGICAL,        OPTIONAL, INTENT(IN)    :: OSEND_SIZE_TO_RECEIVER ! If the buffer size has to be send to the receiver

      CHARACTER(LEN=10) :: YFROM, YTO
      INTEGER           :: IPACKSIZE
      INTEGER           :: IPOS
      LOGICAL           :: GSEND_SIZE_TO_RECEIVER
      REAL,    DIMENSION(:), ALLOCATABLE :: ZPACK ! buffer to store raw data of the sensor

      WRITE( YFROM, '( i10 )' ) ISP
      WRITE( YTO,   '( i10 )' ) KTO
      CALL PRINT_MSG( NVERB_DEBUG, 'GEN', 'Sensor_comm_send', &
                      'send sensor ' // TRIM(TPSENSOR%CNAME) // ': ' // TRIM(YFROM) // '->' // TRIM(YTO), OLOCAL = .TRUE. )

      IF ( PRESENT( OSEND_SIZE_TO_RECEIVER ) ) THEN
        GSEND_SIZE_TO_RECEIVER = OSEND_SIZE_TO_RECEIVER
      ELSE
        GSEND_SIZE_TO_RECEIVER = .FALSE.
      END IF

      IPACKSIZE = TPSENSOR%BUFFER_SIZE_COMPUTE( TPSENSOR%NSTORE_CUR )

      IF ( GSEND_SIZE_TO_RECEIVER ) CALL TPSENSOR%BUFFER_SIZE_SEND( TPSENSOR%NSTORE_CUR, IPACKSIZE, KTO )

      ALLOCATE( ZPACK(IPACKSIZE) )

      IPOS = 1
      CALL TPSENSOR%BUFFER_PACK( ZPACK, IPOS, TPSENSOR%NSTORE_CUR )

      IF ( IPOS-1 /= IPACKSIZE ) &
        CALL PRINT_MSG( NVERB_ERROR, 'GEN', 'Sensor_comm_send', 'IPOS-1 /= IPACKSIZE', OLOCAL = .TRUE. )

      CALL TPSENSOR%BUFFER_SEND( ZPACK, KTO )

      DEALLOCATE( ZPACK )

    END SUBROUTINE SENSOR_COMM_SEND

    ! #############################################################################
    SUBROUTINE SENSOR_COMM_SEND_DEALLOCATE( TPSENSOR, KTO, OSEND_SIZE_TO_RECEIVER )
    ! #############################################################################

      CLASS(TSENSOR),           INTENT(INOUT) :: TPSENSOR
      INTEGER,                  INTENT(IN)    :: KTO                    ! Process to which to send data
      LOGICAL,        OPTIONAL, INTENT(IN)    :: OSEND_SIZE_TO_RECEIVER ! If the buffer size has to be send to the receiver

      CALL SENSOR_COMM_SEND( TPSENSOR, KTO, OSEND_SIZE_TO_RECEIVER )

      ! Deallocate sensor data once not needed anymore
      IF ( TPSENSOR%NSTORE_MAX >= 0 ) CALL TPSENSOR%DATA_ARRAYS_DEALLOCATE( )

    END SUBROUTINE SENSOR_COMM_SEND_DEALLOCATE

    ! ####################################################################################################
    SUBROUTINE SENSOR_COMM_RECV_ALLOCATE( TPSENSOR, KFROM, KSTORE_CUR, KSTORE_MAX, ORECV_SIZE_FROM_OWNER )
    ! ####################################################################################################

      USE MODD_IO, ONLY: ISP

      USE MODE_MSG

      CLASS(TSENSOR),           INTENT(INOUT) :: TPSENSOR
      INTEGER,                  INTENT(IN)    :: KFROM    ! Process from which to receive data
      INTEGER,        OPTIONAL, INTENT(IN)    :: KSTORE_CUR ! Number of storage steps to receive
      INTEGER,        OPTIONAL, INTENT(IN)    :: KSTORE_MAX ! Maximum number of storage steps to store in sensor
                                                            ! (if not provided, kstore_* size must be given by the sender)
      LOGICAL,        OPTIONAL, INTENT(IN)    :: ORECV_SIZE_FROM_OWNER ! If the buffer size has to be send to the receiver

      CHARACTER(LEN=10) :: YFROM, YTO
      INTEGER           :: IPACKSIZE
      INTEGER           :: IPOS
      INTEGER           :: ISTORE_CUR
      INTEGER           :: ISTORE_MAX
      LOGICAL           :: GRECV_SIZE_FROM_OWNER
      REAL,    DIMENSION(:), ALLOCATABLE :: ZPACK ! buffer to store raw data of the sensor

      WRITE( YFROM, '( i10 )' ) KFROM
      WRITE( YTO,   '( i10 )' ) ISP
      CALL PRINT_MSG( NVERB_DEBUG, 'GEN', 'Sensor_comm_recv_allocate', &
                      'receive sensor (name not yet known): ' // TRIM(YFROM) // '->' // TRIM(YTO), OLOCAL = .TRUE. )

      IF ( PRESENT( ORECV_SIZE_FROM_OWNER ) ) THEN
        GRECV_SIZE_FROM_OWNER = ORECV_SIZE_FROM_OWNER
      ELSE
        GRECV_SIZE_FROM_OWNER = .FALSE.
       END IF

      IF ( PRESENT( KSTORE_CUR ) ) THEN
        IF ( GRECV_SIZE_FROM_OWNER )                                                                       &
          CALL PRINT_MSG( NVERB_ERROR, 'GEN', 'Sensor_comm_recv_allocate',                                 &
                          'kstore_cur may not be provided if size is received from owner', OLOCAL = .TRUE. )
        ISTORE_CUR = KSTORE_CUR
      ELSE
        IF ( .NOT. GRECV_SIZE_FROM_OWNER )                                                                  &
          CALL PRINT_MSG( NVERB_ERROR, 'GEN', 'Sensor_comm_recv_allocate',                                  &
                          'kstore_cur must be provided if size is not received from owner', OLOCAL = .TRUE. )
        ! istore_cur will be received from owner
        ISTORE_CUR = 0
      END IF

      IF ( PRESENT( KSTORE_MAX ) ) THEN
        IF ( GRECV_SIZE_FROM_OWNER )                                                                       &
          CALL PRINT_MSG( NVERB_ERROR, 'GEN', 'Sensor_comm_recv_allocate',                                 &
                          'kstore_max may not be provided if size is received from owner', OLOCAL = .TRUE. )
        ISTORE_MAX = KSTORE_MAX
      ELSE
        IF ( .NOT. GRECV_SIZE_FROM_OWNER )                                                                  &
          CALL PRINT_MSG( NVERB_ERROR, 'GEN', 'Sensor_comm_recv_allocate',                                  &
                          'kstore_max must be provided if size is not received from owner', OLOCAL = .TRUE. )

        ! istore_max will be received from owner
        ISTORE_MAX = 0
      END IF

      IF ( GRECV_SIZE_FROM_OWNER ) THEN
        CALL TPSENSOR%BUFFER_SIZE_RECV( ISTORE_CUR, ISTORE_MAX, IPACKSIZE, KFROM )
      ELSE
        IPACKSIZE = TPSENSOR%BUFFER_SIZE_COMPUTE( ISTORE_CUR )
      END IF

      ! Allocate receive buffer
      ALLOCATE( ZPACK(IPACKSIZE) )

      ! Allocation of sensor must be done only once the total number of stores is known (and only if not yet allocated)
      IF (TPSENSOR%NSTORE_MAX < 0 ) CALL TPSENSOR%DATA_ARRAYS_ALLOCATE( ISTORE_MAX )

      CALL TPSENSOR%BUFFER_RECV( ZPACK, KFROM )

      IPOS = 1
      CALL TPSENSOR%BUFFER_UNPACK( ZPACK, IPOS, ISTORE_CUR )

      IF ( IPOS-1 /= IPACKSIZE ) &
        CALL PRINT_MSG( NVERB_ERROR, 'GEN', 'Sensor_comm_recv_allocate', 'IPOS-1 /= IPACKSIZE', OLOCAL = .TRUE. )

    END SUBROUTINE SENSOR_COMM_RECV_ALLOCATE

END MODULE MODD_SENSOR
