!MNH_LIC Copyright 2002-2022 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
! Authors:
!  Misc: some of the code was taken from older subroutines/functions for stations
!  P. Wautelet 08/04/2022
!-----------------------------------------------------------------
! Modifications:
!-----------------------------------------------------------------
!      ##################
MODULE MODE_STATION_TOOLS
!      ##################

USE MODD_TYPE_STATION, ONLY: TSTATIONDATA

IMPLICIT NONE

PRIVATE

PUBLIC :: STATION_ALLOCATE
PUBLIC :: STATION_INI_INTERP
PUBLIC :: STATION_POSITION
PUBLIC :: STATION_ADD
PUBLIC :: STATION_INTERP_2D, STATION_INTERP_2D_U, STATION_INTERP_2D_V

CONTAINS

! ##############################################
SUBROUTINE STATION_ALLOCATE( TPSTATION, KSTORE )
! ##############################################

  USE MODD_ALLSTATION_n, ONLY: LDIAG_SURFRAD
  USE MODD_CONF_n,       ONLY: NRR
  USE MODD_NSV,          ONLY: NSV
  USE MODD_PARAMETERS,   ONLY: XUNDEF
  USE MODD_PARAM_n,      ONLY: CRAD, CTURB

  IMPLICIT NONE

  TYPE(TSTATIONDATA), INTENT(INOUT) :: TPSTATION
  INTEGER,            INTENT(IN)    :: KSTORE  ! number of moments to store

  ALLOCATE( TPSTATION%XZON(KSTORE) )
  ALLOCATE( TPSTATION%XMER(KSTORE) )
  ALLOCATE( TPSTATION%XW  (KSTORE) )
  ALLOCATE( TPSTATION%XP  (KSTORE) )
  IF ( CTURB == 'TKEL' ) THEN
    ALLOCATE( TPSTATION%XTKE(KSTORE) )
  ELSE
    ALLOCATE( TPSTATION%XTKE(0) )
  END IF
  ALLOCATE( TPSTATION%XTH(KSTORE) )
  ALLOCATE( TPSTATION%XR (KSTORE, NRR) )
  ALLOCATE( TPSTATION%XSV(KSTORE, NSV) )
  IF ( CRAD /= 'NONE' ) THEN
    ALLOCATE( TPSTATION%XTSRAD(KSTORE) )
  ELSE
    ALLOCATE( TPSTATION%XTSRAD(0) )
  END IF
  IF ( LDIAG_SURFRAD ) THEN
    ALLOCATE( TPSTATION%XT2M   (KSTORE) )
    ALLOCATE( TPSTATION%XQ2M   (KSTORE) )
    ALLOCATE( TPSTATION%XHU2M  (KSTORE) )
    ALLOCATE( TPSTATION%XZON10M(KSTORE) )
    ALLOCATE( TPSTATION%XMER10M(KSTORE) )
    ALLOCATE( TPSTATION%XRN    (KSTORE) )
    ALLOCATE( TPSTATION%XH     (KSTORE) )
    ALLOCATE( TPSTATION%XLE    (KSTORE) )
    ALLOCATE( TPSTATION%XLEI   (KSTORE) )
    ALLOCATE( TPSTATION%XGFLUX (KSTORE) )
    IF ( CRAD /= 'NONE' ) THEN
      ALLOCATE( TPSTATION%XSWD   (KSTORE) )
      ALLOCATE( TPSTATION%XSWU   (KSTORE) )
      ALLOCATE( TPSTATION%XLWD   (KSTORE) )
      ALLOCATE( TPSTATION%XLWU   (KSTORE) )
      ALLOCATE( TPSTATION%XSWDIR (KSTORE) )
      ALLOCATE( TPSTATION%XSWDIFF(KSTORE) )
      ALLOCATE( TPSTATION%XDSTAOD(KSTORE) )
    END IF
    ALLOCATE( TPSTATION%XSFCO2(KSTORE) )
  END IF

  TPSTATION%XZON(:)    = XUNDEF
  TPSTATION%XMER(:)    = XUNDEF
  TPSTATION%XW(:)      = XUNDEF
  TPSTATION%XP(:)      = XUNDEF
  TPSTATION%XTKE(:)    = XUNDEF
  TPSTATION%XTH(:)     = XUNDEF
  TPSTATION%XR(:,:)    = XUNDEF
  TPSTATION%XSV(:,:)   = XUNDEF
  TPSTATION%XTSRAD(:)  = XUNDEF
  IF ( LDIAG_SURFRAD ) THEN
    TPSTATION%XT2M(:)    = XUNDEF
    TPSTATION%XQ2M(:)    = XUNDEF
    TPSTATION%XHU2M(:)   = XUNDEF
    TPSTATION%XZON10M(:) = XUNDEF
    TPSTATION%XMER10M(:) = XUNDEF
    TPSTATION%XRN(:)     = XUNDEF
    TPSTATION%XH(:)      = XUNDEF
    TPSTATION%XLE(:)     = XUNDEF
    TPSTATION%XLEI(:)    = XUNDEF
    TPSTATION%XGFLUX(:)  = XUNDEF
    IF ( CRAD /= 'NONE' ) THEN
      TPSTATION%XSWD(:)    = XUNDEF
      TPSTATION%XSWU(:)    = XUNDEF
      TPSTATION%XLWD(:)    = XUNDEF
      TPSTATION%XLWU(:)    = XUNDEF
      TPSTATION%XSWDIR(:)  = XUNDEF
      TPSTATION%XSWDIFF(:) = XUNDEF
      TPSTATION%XDSTAOD(:) = XUNDEF
    END IF
    TPSTATION%XSFCO2(:)  = XUNDEF
  END IF

END SUBROUTINE STATION_ALLOCATE

! ########################################
SUBROUTINE STATION_INI_INTERP( TPSTATION )
! ########################################

  USE MODD_GRID,         ONLY: XLATORI, XLONORI
  USE MODD_PARAMETERS,   ONLY: XUNDEF

  USE MODE_GRIDPROJ,     ONLY: SM_XYHAT
  USE MODE_MSG

  IMPLICIT NONE

  TYPE(TSTATIONDATA), INTENT(INOUT) :: TPSTATION

  IF ( TPSTATION%XLAT == XUNDEF .OR. TPSTATION%XLON == XUNDEF ) THEN
    CMNHMSG(1) = 'Error in station position'
    CMNHMSG(2) = 'either LATitude or LONgitude segment'
    CMNHMSG(3) = 'or I and J segment'
    CMNHMSG(4) = 'definition is not complete.'
    CALL PRINT_MSG( NVERB_FATAL, 'GEN', 'STATION_INI_INTERP' )
  END IF

  CALL SM_XYHAT( XLATORI,        XLONORI,        &
                 TPSTATION%XLAT, TPSTATION%XLON, &
                 TPSTATION%XX,   TPSTATION%XY    )

END SUBROUTINE STATION_INI_INTERP

! ###############################################################################################
SUBROUTINE STATION_POSITION( TPSTATION, PXHAT_GLOB, PYHAT_GLOB, PXHATM, PYHATM,                 &
                             PXHATM_PHYS_MIN, PXHATM_PHYS_MAX,PYHATM_PHYS_MIN, PYHATM_PHYS_MAX, &
                             OINSIDE, OPRESENT                                                  )
! ###############################################################################################
! Subroutine to determine the position of a station on the model grid
! and set the useful coefficients for data interpolation

  USE MODD_CONF,           ONLY: L1D
  USE MODD_GRID_n,         ONLY: XXHAT, XYHAT, XZZ
  USE MODD_PARAMETERS,     ONLY: JPHEXT, JPVEXT

  USE MODE_MSG
  USE MODE_NEST_LL,        ONLY: GET_MODEL_NUMBER_ll
  USE MODE_TOOLS_ll,       ONLY: GET_INDICE_ll

  IMPLICIT NONE

  TYPE(TSTATIONDATA), INTENT(INOUT) :: TPSTATION
  REAL, DIMENSION(:), INTENT(IN)    :: PXHAT_GLOB
  REAL, DIMENSION(:), INTENT(IN)    :: PYHAT_GLOB
  REAL, DIMENSION(:), INTENT(IN)    :: PXHATM ! mass point coordinates
  REAL, DIMENSION(:), INTENT(IN)    :: PYHATM ! mass point coordinates
  REAL,               INTENT(IN)    :: PXHATM_PHYS_MIN, PYHATM_PHYS_MIN  ! Minimum X coordinate of mass points in the physical domain
  REAL,               INTENT(IN)    :: PXHATM_PHYS_MAX, PYHATM_PHYS_MAX  ! Minimum X coordinate of mass points in the physical domain
  LOGICAL,            INTENT(OUT)   :: OINSIDE  ! True if station is inside physical domain of model
  LOGICAL,            INTENT(OUT)   :: OPRESENT ! True if station is present on the current process

  INTEGER :: IIB ! domain sizes of current process
  INTEGER :: IJB !
  INTEGER :: IIE !
  INTEGER :: IJE !
  INTEGER :: IMI
  INTEGER :: JK
  REAL    :: ZLOW, ZHIGH

  OPRESENT = .FALSE.
  OINSIDE  = .FALSE.

  CALL GET_INDICE_ll( IIB, IJB, IIE, IJE )

  IF (       TPSTATION%XX >= PXHAT_GLOB(JPHEXT+1) .AND. TPSTATION%XX <= PXHAT_GLOB(UBOUND(PXHAT_GLOB,1)-JPHEXT+1) &
       .AND. TPSTATION%XY >= PYHAT_GLOB(JPHEXT+1) .AND. TPSTATION%XY <= PYHAT_GLOB(UBOUND(PYHAT_GLOB,1)-JPHEXT+1) ) THEN
    OINSIDE = .TRUE.
  ELSE
    CALL GET_MODEL_NUMBER_ll(IMI)
    WRITE( CMNHMSG(1), "( 'station ', A, ' is outside of physical domain of model', I3 )" ) TRIM(TPSTATION%CNAME), IMI
    CALL PRINT_MSG( NVERB_WARNING, 'GEN', 'STATION_POSITION' )
 END IF

  ! X position
  TPSTATION%NI_U = COUNT( XXHAT (:) <= TPSTATION%XX )
  TPSTATION%NI_M = COUNT( PXHATM(:) <= TPSTATION%XX )

  ! Y position
  TPSTATION%NJ_V = COUNT( XYHAT (:) <= TPSTATION%XY )
  TPSTATION%NJ_M = COUNT( PYHATM(:) <= TPSTATION%XY )

  ! Position of station according to process
  IF (       TPSTATION%NI_U >= IIB .AND. TPSTATION%NI_U <= IIE &
       .AND. TPSTATION%NJ_V >= IJB .AND. TPSTATION%NJ_V <= IJE ) OPRESENT = .TRUE.
  IF ( L1D ) OPRESENT = .TRUE.

  ! Check if station is too near of physical domain border (outside of physical domain for mass points)
  IF ( OINSIDE .AND. .NOT. L1D ) THEN
    IF (      TPSTATION%XX < PXHATM_PHYS_MIN .OR. TPSTATION%XX > PXHATM_PHYS_MAX &
         .OR. TPSTATION%XY < PYHATM_PHYS_MIN .OR. TPSTATION%XY > PYHATM_PHYS_MAX ) THEN
      CALL GET_MODEL_NUMBER_ll(IMI)
      WRITE( CMNHMSG(1), "( 'station ', A, ' is outside of mass-points physical domain of model', I3 )" ) &
             TRIM(TPSTATION%CNAME), IMI
      CMNHMSG(2) = 'but is inside of flux-points physical domain.'
      CMNHMSG(3) = 'Meaning: station is too close to the boundaries of physical domain.'
      CMNHMSG(4) = '=> station disabled (not computed)'
      CALL PRINT_MSG( NVERB_WARNING, 'GEN', 'STATION_POSITION' )
      OPRESENT = .FALSE.
      OINSIDE = .FALSE.
    END IF
  END IF

  ! Computations only on correct process
  IF ( OPRESENT .AND. .NOT. L1D ) THEN
    ! Interpolation coefficient for X (mass-point)
    TPSTATION%XXMCOEF = ( TPSTATION%XX - PXHATM(TPSTATION%NI_M) ) / ( PXHATM(TPSTATION%NI_M+1) - PXHATM(TPSTATION%NI_M) )
    ! Interpolation coefficient for Y (mass-point)
    TPSTATION%XYMCOEF = ( TPSTATION%XY - PYHATM(TPSTATION%NJ_M) ) / ( PYHATM(TPSTATION%NJ_M+1) - PYHATM(TPSTATION%NJ_M) )
    ! Interpolation coefficient for X (U-point)
    TPSTATION%XXUCOEF = ( TPSTATION%XX - XXHAT(TPSTATION%NI_U) )  / ( XXHAT(TPSTATION%NI_U+1)  - XXHAT(TPSTATION%NI_U) )
    ! Interpolation coefficient for Y (V-point)
    TPSTATION%XYVCOEF = ( TPSTATION%XY - XYHAT(TPSTATION%NJ_V) )  / ( XYHAT(TPSTATION%NJ_V+1)  - XYHAT(TPSTATION%NJ_V) )
  END IF

  IF ( OPRESENT ) THEN
    ! The closest K-level to the station altitude is chosen
    JK = JPVEXT + 1
    DO WHILE ( ( STATION_INTERP_2D( TPSTATION, XZZ(:,:,JK) ) - STATION_INTERP_2D( TPSTATION, XZZ(:,:,JPVEXT+1) ) ) < TPSTATION%XZ)
      JK = JK + 1
    END DO
    ZLOW  = STATION_INTERP_2D( TPSTATION, XZZ(:,:,JK-1) ) - STATION_INTERP_2D( TPSTATION, XZZ(:,:,JPVEXT+1) )
    ZHIGH = STATION_INTERP_2D( TPSTATION, XZZ(:,:,JK  ) ) - STATION_INTERP_2D( TPSTATION, XZZ(:,:,JPVEXT+1) )
    !If the station is nearer from the lower level, select it
    IF ( ( ZHIGH - TPSTATION%XZ ) > ( TPSTATION%XZ - ZLOW ) ) JK = JK - 1
    TPSTATION%NK = JK
  END IF

END SUBROUTINE STATION_POSITION

! #################################
SUBROUTINE STATION_ADD( TPSTATION )
! #################################
! Subroutine to add a station to the local list of stations
  USE MODD_STATION_n, ONLY: NUMBSTAT_LOC, TSTATIONS

  IMPLICIT NONE

  TYPE(TSTATIONDATA), INTENT(IN) :: TPSTATION

  INTEGER :: JS
  TYPE(TSTATIONDATA), DIMENSION(:), POINTER :: TZSTATIONS

  NUMBSTAT_LOC = NUMBSTAT_LOC + 1

  ALLOCATE( TZSTATIONS( NUMBSTAT_LOC ) )
  DO JS = 1, NUMBSTAT_LOC - 1
    TZSTATIONS(JS) = TSTATIONS(JS)
  END DO
  TZSTATIONS(NUMBSTAT_LOC) = TPSTATION
  IF ( ASSOCIATED( TSTATIONS ) ) DEALLOCATE( TSTATIONS ) !Can be done without memory leak because allocatable arrays were
                                                         !not yet allocated (will be done in STATION_ALLOCATE)
  TSTATIONS => TZSTATIONS

END SUBROUTINE STATION_ADD
!
! ######################################################
FUNCTION STATION_INTERP_2D( TPSTATION, PA ) RESULT( PB )
! ######################################################
  USE MODD_CONF, ONLY: L1D

  USE MODE_MSG

  IMPLICIT NONE

  TYPE(TSTATIONDATA),   INTENT(IN) :: TPSTATION
  REAL, DIMENSION(:,:), INTENT(IN) :: PA
  REAL                             :: PB

  INTEGER :: JI, JJ

  IF ( SIZE( PA, 1 ) == 2 ) THEN
    JI = 1
    JJ = 1
  ELSE IF ( L1D ) THEN
    JI = 2
    JJ = 2
  ELSE
    JI = TPSTATION%NI_M
    JJ = TPSTATION%NJ_M
  END IF

  IF (       JI >= 1 .AND. JI < SIZE( PA, 1 ) &
       .AND. JJ >= 1 .AND. JJ < SIZE( PA, 2 ) ) THEN
    PB = (1.-TPSTATION%XXMCOEF) *  (1.-TPSTATION%XYMCOEF) * PA(JI,JJ)    + &
         (   TPSTATION%XXMCOEF) *  (1.-TPSTATION%XYMCOEF) * PA(JI+1,JJ)  + &
         (1.-TPSTATION%XXMCOEF) *  (   TPSTATION%XYMCOEF) * PA(JI,JJ+1)  + &
         (   TPSTATION%XXMCOEF) *  (   TPSTATION%XYMCOEF) * PA(JI+1,JJ+1)
  ELSE
    CALL PRINT_MSG( NVERB_ERROR, 'GEN', 'STATION_INTERP_2D', 'value can not be interpolated' )
  END IF

END FUNCTION STATION_INTERP_2D

! ########################################################
FUNCTION STATION_INTERP_2D_U( TPSTATION, PA ) RESULT( PB )
! ########################################################
  USE MODD_CONF, ONLY: L1D

  USE MODE_MSG

  IMPLICIT NONE

  TYPE(TSTATIONDATA),   INTENT(IN) :: TPSTATION
  REAL, DIMENSION(:,:), INTENT(IN) :: PA
  REAL                             :: PB

  INTEGER :: JI, JJ

  IF ( SIZE( PA, 1 ) == 2 ) THEN
    JI = 1
    JJ = 1
  ELSE IF ( L1D ) THEN
    JI = 2
    JJ = 2
  ELSE
    JI = TPSTATION%NI_U
    JJ = TPSTATION%NJ_M
  END IF

  IF (       JI >= 1 .AND. JI < SIZE( PA, 1 ) &
       .AND. JJ >= 1 .AND. JJ < SIZE( PA, 2 ) ) THEN
    PB = (1.-TPSTATION%XXUCOEF) *  (1.-TPSTATION%XYMCOEF) * PA(JI,JJ)    + &
         (   TPSTATION%XXUCOEF) *  (1.-TPSTATION%XYMCOEF) * PA(JI+1,JJ)  + &
         (1.-TPSTATION%XXUCOEF) *  (   TPSTATION%XYMCOEF) * PA(JI,JJ+1)  + &
         (   TPSTATION%XXUCOEF) *  (   TPSTATION%XYMCOEF) * PA(JI+1,JJ+1)
  ELSE
    CALL PRINT_MSG( NVERB_ERROR, 'GEN', 'STATION_INTERP_2D_U', 'value can not be interpolated' )
  END IF

END FUNCTION STATION_INTERP_2D_U

! ########################################################
FUNCTION STATION_INTERP_2D_V( TPSTATION, PA ) RESULT( PB )
! ########################################################
  USE MODD_CONF, ONLY: L1D

  USE MODE_MSG

  IMPLICIT NONE

  TYPE(TSTATIONDATA),   INTENT(IN) :: TPSTATION
  REAL, DIMENSION(:,:), INTENT(IN) :: PA
  REAL                             :: PB

  INTEGER :: JI, JJ

  IF ( SIZE( PA, 1 ) == 2 ) THEN
    JI = 1
    JJ = 1
  ELSE IF ( L1D ) THEN
    JI = 2
    JJ = 2
  ELSE
    JI = TPSTATION%NI_M
    JJ = TPSTATION%NJ_V
  END IF

  IF (       JI >= 1 .AND. JI < SIZE( PA, 1 ) &
       .AND. JJ >= 1 .AND. JJ < SIZE( PA, 2 ) ) THEN
    PB = (1.-TPSTATION%XXMCOEF) *  (1.-TPSTATION%XYVCOEF) * PA(JI,JJ)    + &
         (   TPSTATION%XXMCOEF) *  (1.-TPSTATION%XYVCOEF) * PA(JI+1,JJ)  + &
         (1.-TPSTATION%XXMCOEF) *  (   TPSTATION%XYVCOEF) * PA(JI,JJ+1)  + &
         (   TPSTATION%XXMCOEF) *  (   TPSTATION%XYVCOEF) * PA(JI+1,JJ+1)
  ELSE
    CALL PRINT_MSG( NVERB_ERROR, 'GEN', 'STATION_INTERP_2D_V', 'value can not be interpolated' )
  END IF

END FUNCTION STATION_INTERP_2D_V

END MODULE MODE_STATION_TOOLS
