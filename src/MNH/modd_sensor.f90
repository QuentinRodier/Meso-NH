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

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: TSENSOR

  TYPE, ABSTRACT :: TSENSOR
      CHARACTER(LEN=NSENSORNAMELGTMAX) :: CNAME = '' ! Title or name of the sensor

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
    CONTAINS
      PROCEDURE                               :: COMPUTE_VERTICAL_INTERP_COEFF
      PROCEDURE                               :: INTERP_FROM_MASSPOINT
      PROCEDURE                               :: INTERP_FROM_UPOINT
      PROCEDURE                               :: INTERP_FROM_VPOINT
      PROCEDURE                               :: INTERP_HOR_FROM_MASSPOINT_0D
      PROCEDURE                               :: INTERP_HOR_FROM_UPOINT_0D
      PROCEDURE                               :: INTERP_HOR_FROM_VPOINT_0D
      PROCEDURE                               :: INTERP_HOR_FROM_MASSPOINT_1D
      PROCEDURE                               :: INTERP_HOR_FROM_UPOINT_1D
      PROCEDURE                               :: INTERP_HOR_FROM_VPOINT_1D

      GENERIC :: INTERP_HOR_FROM_MASSPOINT => INTERP_HOR_FROM_MASSPOINT_0D, INTERP_HOR_FROM_MASSPOINT_1D
      GENERIC :: INTERP_HOR_FROM_UPOINT    => INTERP_HOR_FROM_UPOINT_0D,    INTERP_HOR_FROM_UPOINT_1D
      GENERIC :: INTERP_HOR_FROM_VPOINT    => INTERP_HOR_FROM_VPOINT_0D,    INTERP_HOR_FROM_VPOINT_1D

  END TYPE TSENSOR



  CONTAINS
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

END MODULE MODD_SENSOR
