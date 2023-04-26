!MNH_LIC Copyright 2000-2023 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
MODULE MODE_INI_BALLOON

IMPLICIT NONE

PRIVATE

PUBLIC :: INI_BALLOON

CONTAINS

!     ######################
      SUBROUTINE INI_BALLOON
!     ######################
!
!
!!****  *INI_BALLOON* - user initializes the balloon characteristics
!!
!!    PURPOSE
!!    -------
!
!
!!**  METHOD
!!    ------
!!    
!!    For constant volume Balloon, horizontal advection using horizontal wind
!!        vertical speed of the balloon calculated using the balloon equation
!!        (Koffi et AL 2000, JAS vol 57 P.2007-2021)
!!
!!   Must be defined (for each balloon):
!!   ---------------
!!
!!  No default exist for these variables.
!!  ************************************
!!
!!  1) the model in which the balloon will evolve
!!     if NOT initialized, the balloon is NOT used.
!!  1.1) the possibility to switch from a model to its dad or kid
!!       'FIX' : NMODEL used during the run
!!       'MOB' : best resolution model used. NMODEL=1 is used at the beginning
!!
!!  2) the type of balloon
!!
!!     'RADIOS' for radiosounding balloon
!!     'ISODEN' for iso-density balloon
!!     'CVBALL' for constant volume Balloon
!!
!!  3) the launching date and time
!!
!!  4) the latitude of the launching site
!!
!!  5) the longitude of the launching site
!!
!!  6) the altitude of the launching site (for 'RADIOS')
!!
!!                      OR
!!
!!     the altitude OR pressure of balloon at start of the leveled flight
!!     (for 'ISODEN'). In this case, the density of this level will be computed,
!!     and the balloon will evolve at this density level.
!!
!!
!!
!!   Can be defined  (for each balloon):
!!   --------------
!!
!!  7) the ascentional vertical speed of the ballon (in calm air) (for 'RADIOS')
!!     default is 5m/s
!!
!!  8) the time step for data storage.
!!    default is 60s
!!
!!  9) the name or title describing the balloon (8 characters)
!!     default is the balloon type (6 characters) + the balloon numbers (2 characters)
!!
!!  10) for 'CVBALL' the aerodynamic drag coefficient of the balloon
!!
!!  11) for 'CVBALL' the induced drag coefficient (i.e. air shifted by the balloon)
!!
!!  12) for 'CVBALL' the volume of the balloon
!!
!!  13) for 'CVBALL' the mass of the balloon
!!
!!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!      Valery Masson             * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!     Original 15/05/2000
!!              Apr,19, 2001 (G.Jaubert) add CVBALL type and switch in models
!  P. Wautelet 13/07/2022: give balloons characteristics in namelist instead of hardcoded
!! --------------------------------------------------------------------------
USE MODD_AIRCRAFT_BALLOON
USE MODD_CONF,             ONLY: NMODEL_NEST => NMODEL
USE MODD_CST,              ONLY: XPI
USE MODD_PARAMETERS,       ONLY: XNEGUNDEF, XUNDEF

USE MODE_MSG

USE MODN_BALLOONS

IMPLICIT NONE

INTEGER :: JI
TYPE(TBALLOONDATA), POINTER :: TZBALLOON

!Treat balloon data read in namelist
DO JI = 1, NBALLOONS
  ALLOCATE( TBALLOONS(JI)%TBALLOON )
  TZBALLOON => TBALLOONS(JI)%TBALLOON

  TZBALLOON%NID = JI

  IF ( CTITLE(JI) == '' ) THEN
    WRITE( CTITLE(JI), FMT = '( A, I3.3) ') TRIM( CTYPE(JI) ), JI

    WRITE( CMNHMSG(1), FMT = '( A, I4 )' ) 'no title given to balloon number ', JI
    CMNHMSG(2) = 'title set to ' // TRIM( CTITLE(JI) )
    CALL PRINT_MSG( NVERB_INFO, 'GEN', 'INI_BALLOON', OLOCAL = .TRUE. )
  END IF
  TZBALLOON%CNAME = CTITLE(JI)

  IF ( CMODEL(JI) == 'FIX' ) THEN
    IF ( NMODEL(JI) < 1 .OR. NMODEL(JI) > NMODEL_NEST ) THEN
      CMNHMSG(1) = 'invalid NMODEL balloon ' // TRIM( CTITLE(JI) )
      CMNHMSG(2) = 'NMODEL must be between 1 and the last nested model number'
      CALL PRINT_MSG( NVERB_ERROR, 'GEN', 'INI_BALLOON', OLOCAL = .TRUE. )
      NMODEL(JI) = 1
    END IF
  ELSE IF ( CMODEL(JI) == 'MOB' ) THEN
    IF ( NMODEL(JI) /= 0 .AND. NMODEL(JI) /= 1 ) THEN
      CALL PRINT_MSG( NVERB_WARNING, 'GEN', 'INI_BALLOON', &
                      'NMODEL is set to 1 at start for a CMODEL="MOB" balloon (balloon ' // TRIM( CTITLE(JI) ) // ')', &
                       OLOCAL = .TRUE.)
    END IF
    IF ( NMODEL_NEST == 1 ) CMODEL(JI) = 'FIX' ! If only one model, FIX and MOB are the same
    ! NMODEL set temporarily to 1. Will be set to the launch model in INI_LAUNCH
    NMODEL(JI) = 1
  ELSE
    CMNHMSG(1) = 'invalid CMODEL (' // TRIM( CMODEL(JI) ) // ') for balloon ' // TRIM( CTITLE(JI) )
    CMNHMSG(2) = 'CMODEL must be FIX or MOB (default="FIX")'
    CALL PRINT_MSG( NVERB_ERROR, 'GEN', 'INI_BALLOON', OLOCAL = .TRUE. )
    CMODEL(JI) = 'FIX'
    NMODEL(JI) = 1
  END IF
  TZBALLOON%CMODEL = CMODEL(JI)
  TZBALLOON%NMODEL = NMODEL(JI)

  TZBALLOON%CTYPE = CTYPE(JI)

  IF ( .NOT. TLAUNCH(JI)%CHECK( TRIM( CTITLE(JI) ) ) ) &
        CALL PRINT_MSG( NVERB_ERROR, 'GEN', 'INI_BALLOON', &
                        'problem with TLAUNCH (not set or incorrect values) for balloon ' // TRIM( CTITLE(JI) ), OLOCAL = .TRUE. )
  TZBALLOON%TLAUNCH  = TLAUNCH(JI)

  IF ( XLATLAUNCH(JI) == XUNDEF ) &
    CALL PRINT_MSG( NVERB_ERROR, 'GEN', 'INI_BALLOON', 'XLATLAUNCH not provided for balloon ' // TRIM( CTITLE(JI) ), &
                    OLOCAL = .TRUE. )
  TZBALLOON%XLATLAUNCH = XLATLAUNCH(JI)

  IF ( XLONLAUNCH(JI) == XUNDEF ) &
    CALL PRINT_MSG( NVERB_ERROR, 'GEN', 'INI_BALLOON', 'XLONLAUNCH not provided for balloon ' // TRIM( CTITLE(JI) ), &
                    OLOCAL = .TRUE. )
  TZBALLOON%XLONLAUNCH = XLONLAUNCH(JI)

  IF ( XTSTEP(JI) == XNEGUNDEF ) THEN
    CALL PRINT_MSG( NVERB_INFO, 'GEN', 'INI_BALLOON', &
                    'data storage frequency not provided for balloon ' // TRIM( CTITLE(JI) ) // ' => set to 60s', OLOCAL = .TRUE. )
    XTSTEP(JI) = 60.
  ELSE IF ( XTSTEP(JI) <=0. ) THEN
    CALL PRINT_MSG( NVERB_ERROR, 'GEN', 'INI_BALLOON', 'invalid data storage frequency for balloon ' // TRIM( CTITLE(JI) ), &
                    OLOCAL = .TRUE. )
    XTSTEP(JI) = 60.
  END IF
  TZBALLOON%TFLYER_TIME%XTSTEP = XTSTEP(JI)

  SELECT CASE ( CTYPE(JI) )
    CASE ( 'CVBALL' )
      IF ( XALTLAUNCH(JI) == XNEGUNDEF .AND. XPRES(JI) == XNEGUNDEF ) THEN
        CMNHMSG(1) = 'altitude or pressure at launch not provided for CVBALL balloon ' // TRIM( CTITLE(JI) )
        CMNHMSG(2) = 'altitude with same air density than balloon will be used for the launch position'
        CALL PRINT_MSG( NVERB_INFO, 'GEN', 'INI_BALLOON' , OLOCAL = .TRUE.)
      END IF
      IF ( XALTLAUNCH(JI) /= XNEGUNDEF .AND. XPRES(JI) /= XNEGUNDEF ) &
        CALL PRINT_MSG( NVERB_ERROR, 'GEN', 'INI_BALLOON', &
                        'altitude or pressure at launch (not both) must be provided for ISODEN balloon ' // TRIM( CTITLE(JI) ), &
                        OLOCAL = .TRUE. )
      TZBALLOON%XALTLAUNCH = XALTLAUNCH(JI)
      TZBALLOON%XPRES      = XPRES(JI)

      IF ( XWASCENT(JI) == XNEGUNDEF ) THEN
        CALL PRINT_MSG( NVERB_INFO, 'GEN', 'INI_BALLOON', &
                        'initial vertical speed not provided for CVBALL balloon ' // TRIM( CTITLE(JI) ) // ' => set to 0.' , &
                        OLOCAL = .TRUE.)
        XWASCENT(JI) = 0.
      END IF
      TZBALLOON%XWASCENT = XWASCENT(JI)


      IF ( XAERODRAG(JI) == XNEGUNDEF ) THEN
        CALL PRINT_MSG( NVERB_INFO, 'GEN', 'INI_BALLOON', &
                        'aerodynamic drag coefficient not provided for CVBALL balloon ' // TRIM( CTITLE(JI) ) &
                        // ' => set to 0.44', OLOCAL = .TRUE.)
        XAERODRAG(JI) = 0.44
      END IF
      TZBALLOON%XAERODRAG = XAERODRAG(JI)

      IF ( XINDDRAG(JI) == XNEGUNDEF ) THEN
        CALL PRINT_MSG( NVERB_INFO, 'GEN', 'INI_BALLOON', &
                        'induced drag coefficient not provided for CVBALL balloon ' // TRIM( CTITLE(JI) ) // ' => set to 0.014', &
                        OLOCAL = .TRUE.)
        XINDDRAG(JI) = 0.014
      END IF
      TZBALLOON%XINDDRAG = XINDDRAG(JI)

      IF ( XMASS(JI) == XNEGUNDEF ) &
        CALL PRINT_MSG( NVERB_FATAL, 'GEN', 'INI_BALLOON', 'mass not provided for CVBALL balloon ' // TRIM( CTITLE(JI) ), &
                        OLOCAL = .TRUE. )
      TZBALLOON%XMASS = XMASS(JI)

      IF ( XDIAMETER(JI) <= 0. .AND. XVOLUME(JI) <= 0. ) &
        CALL PRINT_MSG( NVERB_FATAL, 'GEN', 'INI_BALLOON', &
                        'diameter or volume not provided for CVBALL balloon ' // TRIM( CTITLE(JI) ), OLOCAL = .TRUE. )

      IF ( XDIAMETER(JI) <= 0. ) THEN
        TZBALLOON%XVOLUME          = XVOLUME(JI)
        TZBALLOON%XDIAMETER        = ( (3. * XVOLUME(JI) ) / ( 4. * XPI ) ) ** ( 1. / 3. )
      ELSE IF ( XVOLUME(JI) <= 0 ) THEN
        TZBALLOON%XDIAMETER        = XDIAMETER(JI)
        TZBALLOON%XVOLUME          = XPI / 6 * XDIAMETER(JI)**3
      ELSE
        CALL PRINT_MSG( NVERB_ERROR, 'GEN', 'INI_BALLOON', &
                        'diameter or volume (not both) must be provided for CVBALL balloon ' // TRIM( CTITLE(JI) ), &
                        OLOCAL = .TRUE. )
      END IF


    CASE ( 'ISODEN' )
      IF ( XALTLAUNCH(JI) == XNEGUNDEF .AND. XPRES(JI) == XNEGUNDEF ) &
        CALL PRINT_MSG( NVERB_FATAL, 'GEN', 'INI_BALLOON', &
                        'altitude or pressure at launch must be provided for ISODEN balloon ' // TRIM( CTITLE(JI) ), &
                        OLOCAL = .TRUE. )
      IF ( XALTLAUNCH(JI) /= XNEGUNDEF .AND. XPRES(JI) /= XNEGUNDEF ) &
        CALL PRINT_MSG( NVERB_ERROR, 'GEN', 'INI_BALLOON', &
                        'altitude or pressure at launch (not both) must be provided for ISODEN balloon ' // TRIM( CTITLE(JI) ), &
                        OLOCAL = .TRUE.)
      TZBALLOON%XALTLAUNCH = XALTLAUNCH(JI)
      TZBALLOON%XPRES      = XPRES(JI)

      IF ( XWASCENT(JI) /= XNEGUNDEF ) THEN
        CALL PRINT_MSG( NVERB_WARNING, 'GEN', 'INI_BALLOON', &
                        'initial vertical speed is not needed for ISODEN balloon ' // TRIM( CTITLE(JI) ) // ' => ignored', &
                        OLOCAL = .TRUE. )
        XWASCENT(JI) = XNEGUNDEF
      END IF
      TZBALLOON%XWASCENT = XWASCENT(JI)


      IF ( XAERODRAG(JI) /= XNEGUNDEF ) THEN
        CALL PRINT_MSG( NVERB_WARNING, 'GEN', 'INI_BALLOON', &
                        'aerodynamic drag coefficient is not needed for ISODEN balloon ' // TRIM( CTITLE(JI) ) // ' => ignored', &
                        OLOCAL = .TRUE. )
        XAERODRAG(JI) = XNEGUNDEF
      END IF
      TZBALLOON%XAERODRAG = XAERODRAG(JI)

      IF ( XINDDRAG(JI) /= XNEGUNDEF ) THEN
        CALL PRINT_MSG( NVERB_WARNING, 'GEN', 'INI_BALLOON', &
                        'induced drag coefficient is not needed for ISODEN balloon ' // TRIM( CTITLE(JI) ) // ' => ignored' , &
                        OLOCAL = .TRUE.)
        XINDDRAG(JI) = XNEGUNDEF
      END IF
      TZBALLOON%XINDDRAG = XINDDRAG(JI)

      IF ( XMASS(JI) /= XNEGUNDEF ) THEN
        CALL PRINT_MSG( NVERB_WARNING, 'GEN', 'INI_BALLOON', &
                        'mass is not needed for ISODEN balloon ' // TRIM( CTITLE(JI) ) // ' => ignored', OLOCAL = .TRUE. )
        XMASS(JI) = XNEGUNDEF
      END IF
      TZBALLOON%XMASS = XMASS(JI)

      IF ( XDIAMETER(JI) /= XNEGUNDEF ) THEN
        CALL PRINT_MSG( NVERB_WARNING, 'GEN', 'INI_BALLOON', &
                        'diameter is not needed for ISODEN balloon ' // TRIM( CTITLE(JI) ) // ' => ignored', OLOCAL = .TRUE. )
        XDIAMETER(JI) = XNEGUNDEF
      END IF
      TZBALLOON%XDIAMETER = XDIAMETER(JI)

      IF ( XVOLUME(JI) /= XNEGUNDEF ) THEN
        CALL PRINT_MSG( NVERB_WARNING, 'GEN', 'INI_BALLOON', &
                        'volume is not needed for ISODEN balloon ' // TRIM( CTITLE(JI) ) // ' => ignored', OLOCAL = .TRUE. )
        XVOLUME(JI) = XNEGUNDEF
      END IF
      TZBALLOON%XVOLUME = XVOLUME(JI)


    CASE ( 'RADIOS' )
      IF ( XALTLAUNCH(JI) == XNEGUNDEF ) &
        CALL PRINT_MSG( NVERB_FATAL, 'GEN', 'INI_BALLOON', &
                        'altitude of launch must be provided for radiosounding balloon ' // TRIM( CTITLE(JI) ), OLOCAL = .TRUE. )
      TZBALLOON%XALTLAUNCH = XALTLAUNCH(JI)

      IF ( XWASCENT(JI) == XNEGUNDEF ) THEN
        CALL PRINT_MSG( NVERB_INFO, 'GEN', 'INI_BALLOON', &
                        'initial vertical speed not provided for balloon ' // TRIM( CTITLE(JI) ) // ' => set to 5.', &
                        OLOCAL = .TRUE. )
        XWASCENT(JI) = 5.
      END IF
      TZBALLOON%XWASCENT = XWASCENT(JI)

      IF ( XPRES(JI) /= XNEGUNDEF ) THEN
        CALL PRINT_MSG( NVERB_WARNING, 'GEN', 'INI_BALLOON',                        &
                        'initial pressure is not needed for radiosounding balloon ' &
                        // TRIM( CTITLE(JI) ) // ' => ignored', OLOCAL = .TRUE. )
        XPRES(JI) = XNEGUNDEF
      END IF
      TZBALLOON%XAERODRAG = XAERODRAG(JI)

      IF ( XAERODRAG(JI) /= XNEGUNDEF ) THEN
        CALL PRINT_MSG( NVERB_WARNING, 'GEN', 'INI_BALLOON',                                    &
                        'aerodynamic drag coefficient is not needed for radiosounding balloon ' &
                        // TRIM( CTITLE(JI) ) // ' => ignored', OLOCAL = .TRUE. )
        XAERODRAG(JI) = XNEGUNDEF
      END IF
      TZBALLOON%XAERODRAG = XAERODRAG(JI)

      IF ( XINDDRAG(JI) /= XNEGUNDEF ) THEN
        CALL PRINT_MSG( NVERB_WARNING, 'GEN', 'INI_BALLOON',                                &
                        'induced drag coefficient is not needed for radiosounding balloon ' &
                        // TRIM( CTITLE(JI) ) // ' => ignored', OLOCAL = .TRUE. )
        XINDDRAG(JI) = XNEGUNDEF
      END IF
      TZBALLOON%XINDDRAG = XINDDRAG(JI)

      IF ( XMASS(JI) /= XNEGUNDEF ) THEN
        CALL PRINT_MSG( NVERB_WARNING, 'GEN', 'INI_BALLOON', &
                        'mass is not needed for radiosounding balloon ' // TRIM( CTITLE(JI) ) // ' => ignored', OLOCAL = .TRUE. )
        XMASS(JI) = XNEGUNDEF
      END IF
      TZBALLOON%XMASS = XMASS(JI)

      IF ( XDIAMETER(JI) /= XNEGUNDEF ) THEN
        CALL PRINT_MSG( NVERB_WARNING, 'GEN', 'INI_BALLOON', &
                        'diameter is not needed for radiosounding balloon ' // TRIM( CTITLE(JI) ) // ' => ignored', &
                        OLOCAL = .TRUE. )
        XDIAMETER(JI) = XNEGUNDEF
      END IF
      TZBALLOON%XDIAMETER = XDIAMETER(JI)

      IF ( XVOLUME(JI) /= XNEGUNDEF ) THEN
        CALL PRINT_MSG( NVERB_WARNING, 'GEN', 'INI_BALLOON', &
                        'volume is not needed for radiosounding balloon ' // TRIM( CTITLE(JI) ) // ' => ignored', OLOCAL = .TRUE. )
        XVOLUME(JI) = XNEGUNDEF
      END IF
      TZBALLOON%XVOLUME = XVOLUME(JI)


    CASE DEFAULT
      CALL PRINT_MSG( NVERB_FATAL, 'GEN', 'INI_BALLOON', 'invalid balloon type (CTYPE=' &
                      // TRIM( CTYPE(JI ) ) // ') for balloon ' // TRIM( CTITLE(JI) ), OLOCAL = .TRUE. )

  END SELECT
END DO

IF ( NBALLOONS > 0 ) CALL BALLOONS_NML_DEALLOCATE()

!----------------------------------------------------------------------------
!
END SUBROUTINE INI_BALLOON

END MODULE MODE_INI_BALLOON
