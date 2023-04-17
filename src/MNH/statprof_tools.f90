!MNH_LIC Copyright 2002-2023 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
! Authors:
!  Misc: some of the code was taken from older subroutines/functions for stations
!  P. Wautelet 08/04/2022
!-----------------------------------------------------------------
! Modifications:
!  P. Wautelet 30/09/2022: bugfix: use XUNDEF from SURFEX for surface variables computed by SURFEX
!  P. Wautelet 25/11/2022: rewrite STATPROF_INSTANT algorithm (does not depends on model timestep anymore => independent of model)
!-----------------------------------------------------------------
!      ###################
MODULE MODE_STATPROF_TOOLS
!      ###################

USE MODD_TYPE_STATPROF, ONLY: TPROFILERDATA, TSTATIONDATA, TSTATPROFDATA, TSTATPROFTIME

IMPLICIT NONE

PRIVATE

PUBLIC :: PROFILER_ALLOCATE, STATION_ALLOCATE
PUBLIC :: STATPROF_INI_INTERP
PUBLIC :: STATPROF_POSITION
PUBLIC :: PROFILER_ADD, STATION_ADD
PUBLIC :: STATPROF_INTERP_2D, STATPROF_INTERP_2D_U, STATPROF_INTERP_2D_V
PUBLIC :: STATPROF_INTERP_3D, STATPROF_INTERP_3D_U, STATPROF_INTERP_3D_V
PUBLIC :: STATPROF_INSTANT

CONTAINS

! ################################################
SUBROUTINE PROFILER_ALLOCATE( TPPROFILER, KSTORE )
! ################################################

!   USE MODD_ALLSTATION_n, ONLY: LDIAG_SURFRAD
  USE MODD_CONF_n,       ONLY: NRR
  USE MODD_DIAG_IN_RUN,  ONLY: LDIAG_IN_RUN
  USE MODD_DIM_n,        ONLY: NKMAX
  USE MODD_NSV,          ONLY: NSV
  USE MODD_PARAMETERS,   ONLY: JPVEXT, XUNDEF
  USE MODD_PARAM_n,      ONLY: CCLOUD, CRAD, CTURB
  USE MODD_RADIATIONS_n, ONLY: NAER
  USE MODD_SURF_PAR,     ONLY: XUNDEF_SFX => XUNDEF

  IMPLICIT NONE

  TYPE(TPROFILERDATA), INTENT(INOUT) :: TPPROFILER
  INTEGER,             INTENT(IN)    :: KSTORE  ! number of moments to store

  INTEGER :: IKU

  IKU = NKMAX + 2 * JPVEXT
  ALLOCATE( TPPROFILER%XZON      (KSTORE, IKU) )
  ALLOCATE( TPPROFILER%XMER      (KSTORE, IKU) )
  ALLOCATE( TPPROFILER%XFF       (KSTORE, IKU) )
  ALLOCATE( TPPROFILER%XDD       (KSTORE, IKU) )
  ALLOCATE( TPPROFILER%XW        (KSTORE, IKU) )
  ALLOCATE( TPPROFILER%XP        (KSTORE, IKU) )
  ALLOCATE( TPPROFILER%XZZ       (KSTORE, IKU) )
  IF ( CTURB == 'TKEL' ) THEN
    ALLOCATE( TPPROFILER%XTKE    (KSTORE, IKU) )
  ELSE
    ALLOCATE( TPPROFILER%XTKE    (0, 0) )
  END IF
  ALLOCATE( TPPROFILER%XTH       (KSTORE, IKU) )
  ALLOCATE( TPPROFILER%XTHV      (KSTORE, IKU) )
  IF ( CCLOUD == 'C2R2' .OR. CCLOUD == 'KHKO' ) THEN
    ALLOCATE( TPPROFILER%XVISIGUL  (KSTORE, IKU) )
  ELSE
    ALLOCATE( TPPROFILER%XVISIGUL  (0, 0) )
  END IF
  IF ( CCLOUD /= 'NONE' .AND. CCLOUD /= 'REVE' ) THEN
    ALLOCATE( TPPROFILER%XVISIKUN  (KSTORE, IKU) )
  ELSE
    ALLOCATE( TPPROFILER%XVISIKUN  (0, 0) )
  END IF
  ALLOCATE( TPPROFILER%XCRARE    (KSTORE, IKU) )
  ALLOCATE( TPPROFILER%XCRARE_ATT(KSTORE, IKU) )
  IF ( CCLOUD == 'ICE3' .OR. CCLOUD == 'ICE4' ) THEN
    ALLOCATE( TPPROFILER%XCIZ    (KSTORE, IKU) )
  ELSE
    ALLOCATE( TPPROFILER%XCIZ    (0, 0) )
  END IF
  ALLOCATE( TPPROFILER%XLWCZ     (KSTORE, IKU) )
  ALLOCATE( TPPROFILER%XIWCZ     (KSTORE, IKU) )
  ALLOCATE( TPPROFILER%XRHOD     (KSTORE, IKU) )
  ALLOCATE( TPPROFILER%XR        (KSTORE, IKU, NRR) )
  ALLOCATE( TPPROFILER%XSV       (KSTORE, IKU, NSV) )
  ALLOCATE( TPPROFILER%XAER      (KSTORE, IKU, NAER) )

  ALLOCATE( TPPROFILER%XIWV(KSTORE) )
  ALLOCATE( TPPROFILER%XZTD(KSTORE) )
  ALLOCATE( TPPROFILER%XZWD(KSTORE) )
  ALLOCATE( TPPROFILER%XZHD(KSTORE) )

!   IF ( LDIAG_IN_RUN ) THEN
    ALLOCATE( TPPROFILER%XT2M   (KSTORE) )
    ALLOCATE( TPPROFILER%XQ2M   (KSTORE) )
    ALLOCATE( TPPROFILER%XHU2M  (KSTORE) )
    ALLOCATE( TPPROFILER%XZON10M(KSTORE) )
    ALLOCATE( TPPROFILER%XMER10M(KSTORE) )
    ALLOCATE( TPPROFILER%XRN    (KSTORE) )
    ALLOCATE( TPPROFILER%XH     (KSTORE) )
    ALLOCATE( TPPROFILER%XLE    (KSTORE) )
    ALLOCATE( TPPROFILER%XLEI   (KSTORE) )
    ALLOCATE( TPPROFILER%XGFLUX (KSTORE) )
    IF ( CRAD /= 'NONE' ) THEN
      ALLOCATE( TPPROFILER%XSWD   (KSTORE) )
      ALLOCATE( TPPROFILER%XSWU   (KSTORE) )
      ALLOCATE( TPPROFILER%XLWD   (KSTORE) )
      ALLOCATE( TPPROFILER%XLWU   (KSTORE) )
      ALLOCATE( TPPROFILER%XSWDIR (KSTORE) )
      ALLOCATE( TPPROFILER%XSWDIFF(KSTORE) )
      ALLOCATE( TPPROFILER%XDSTAOD(KSTORE) )
      ALLOCATE( TPPROFILER%XSLTAOD(KSTORE) )
    END IF
    ALLOCATE( TPPROFILER%XTKE_DISS(KSTORE, IKU) )
    ALLOCATE( TPPROFILER%XSFCO2   (KSTORE) )
  END IF

  TPPROFILER%XZON      (:,:) = XUNDEF
  TPPROFILER%XMER      (:,:) = XUNDEF
  TPPROFILER%XFF       (:,:) = XUNDEF
  TPPROFILER%XDD       (:,:) = XUNDEF
  TPPROFILER%XW        (:,:) = XUNDEF
  TPPROFILER%XP        (:,:) = XUNDEF
  TPPROFILER%XZZ       (:,:) = XUNDEF
  IF ( CTURB == 'TKEL' ) TPPROFILER%XTKE(:,:) = XUNDEF
  TPPROFILER%XTH       (:,:) = XUNDEF
  TPPROFILER%XTHV      (:,:) = XUNDEF
  IF ( CCLOUD == 'C2R2' .OR. CCLOUD == 'KHKO' )  TPPROFILER%XVISIGUL(:,:) = XUNDEF
  IF ( CCLOUD /= 'NONE' .AND. CCLOUD /= 'REVE' ) TPPROFILER%XVISIKUN(:,:) = XUNDEF
  TPPROFILER%XCRARE    (:,:) = XUNDEF
  TPPROFILER%XCRARE_ATT(:,:) = XUNDEF
  IF ( CCLOUD == 'ICE3' .OR. CCLOUD == 'ICE4' ) TPPROFILER%XCIZ      (:,:) = XUNDEF
  TPPROFILER%XLWCZ     (:,:) = XUNDEF
  TPPROFILER%XIWCZ     (:,:) = XUNDEF
  TPPROFILER%XRHOD     (:,:) = XUNDEF
  TPPROFILER%XR        (:,:,:) = XUNDEF
  TPPROFILER%XSV       (:,:,:) = XUNDEF
  TPPROFILER%XAER      (:,:,:) = XUNDEF

  TPPROFILER%XIWV(:) = XUNDEF
  TPPROFILER%XZTD(:) = XUNDEF
  TPPROFILER%XZWD(:) = XUNDEF
  TPPROFILER%XZHD(:) = XUNDEF

!   IF ( LDIAG_IN_RUN ) THEN
    TPPROFILER%XT2M   (:) = XUNDEF_SFX
    TPPROFILER%XQ2M   (:) = XUNDEF_SFX
    TPPROFILER%XHU2M  (:) = XUNDEF_SFX
    TPPROFILER%XZON10M(:) = XUNDEF_SFX
    TPPROFILER%XMER10M(:) = XUNDEF_SFX
    TPPROFILER%XRN    (:) = XUNDEF_SFX
    TPPROFILER%XH     (:) = XUNDEF_SFX
    TPPROFILER%XLE    (:) = XUNDEF_SFX
    TPPROFILER%XLEI   (:) = XUNDEF_SFX
    TPPROFILER%XGFLUX (:) = XUNDEF_SFX
    IF ( CRAD /= 'NONE' ) THEN
      TPPROFILER%XSWD   (:) = XUNDEF
      TPPROFILER%XSWU   (:) = XUNDEF
      TPPROFILER%XLWD   (:) = XUNDEF
      TPPROFILER%XLWU   (:) = XUNDEF
      TPPROFILER%XSWDIR (:) = XUNDEF
      TPPROFILER%XSWDIFF(:) = XUNDEF
      TPPROFILER%XDSTAOD(:) = XUNDEF
      TPPROFILER%XSLTAOD(:) = XUNDEF
    END IF
    TPPROFILER%XTKE_DISS(:,:) = XUNDEF
    TPPROFILER%XSFCO2(:)      = XUNDEF
  END IF

END SUBROUTINE PROFILER_ALLOCATE

! ##############################################
SUBROUTINE STATION_ALLOCATE( TPSTATION, KSTORE )
! ##############################################

  USE MODD_ALLSTATION_n, ONLY: LDIAG_SURFRAD
  USE MODD_CONF_n,       ONLY: NRR
  USE MODD_NSV,          ONLY: NSV
  USE MODD_PARAMETERS,   ONLY: XUNDEF
  USE MODD_PARAM_n,      ONLY: CRAD, CTURB
  USE MODD_SURF_PAR,     ONLY: XUNDEF_SFX => XUNDEF

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
      ALLOCATE( TPSTATION%XSLTAOD(KSTORE) )
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
    TPSTATION%XT2M(:)    = XUNDEF_SFX
    TPSTATION%XQ2M(:)    = XUNDEF_SFX
    TPSTATION%XHU2M(:)   = XUNDEF_SFX
    TPSTATION%XZON10M(:) = XUNDEF_SFX
    TPSTATION%XMER10M(:) = XUNDEF_SFX
    TPSTATION%XRN(:)     = XUNDEF_SFX
    TPSTATION%XH(:)      = XUNDEF_SFX
    TPSTATION%XLE(:)     = XUNDEF_SFX
    TPSTATION%XLEI(:)    = XUNDEF_SFX
    TPSTATION%XGFLUX(:)  = XUNDEF_SFX
    IF ( CRAD /= 'NONE' ) THEN
      TPSTATION%XSWD(:)    = XUNDEF
      TPSTATION%XSWU(:)    = XUNDEF
      TPSTATION%XLWD(:)    = XUNDEF
      TPSTATION%XLWU(:)    = XUNDEF
      TPSTATION%XSWDIR(:)  = XUNDEF
      TPSTATION%XSWDIFF(:) = XUNDEF
      TPSTATION%XDSTAOD(:) = XUNDEF
      TPSTATION%XSLTAOD(:) = XUNDEF
    END IF
    TPSTATION%XSFCO2(:)  = XUNDEF_SFX
  END IF

END SUBROUTINE STATION_ALLOCATE

! ##########################################
SUBROUTINE STATPROF_INI_INTERP( TPSTATPROF )
! ##########################################

  USE MODD_GRID,         ONLY: XLATORI, XLONORI
  USE MODD_PARAMETERS,   ONLY: XUNDEF

  USE MODE_GRIDPROJ,     ONLY: SM_XYHAT
  USE MODE_MSG

  IMPLICIT NONE

  CLASS(TSTATPROFDATA), INTENT(INOUT) :: TPSTATPROF

  IF ( TPSTATPROF%XLAT == XUNDEF .OR. TPSTATPROF%XLON == XUNDEF ) THEN
    CMNHMSG(1) = 'Error in station or profiler position'
    CMNHMSG(2) = 'either LATitude or LONgitude segment'
    CMNHMSG(3) = 'or I and J segment'
    CMNHMSG(4) = 'definition is not complete.'
    CALL PRINT_MSG( NVERB_FATAL, 'GEN', 'STATPROF_INI_INTERP' )
  END IF

  CALL SM_XYHAT( XLATORI,         XLONORI,         &
                 TPSTATPROF%XLAT, TPSTATPROF%XLON, &
                 TPSTATPROF%XX,   TPSTATPROF%XY    )

END SUBROUTINE STATPROF_INI_INTERP

! ###########################################################
SUBROUTINE STATPROF_POSITION( TPSTATPROF, OINSIDE, OPRESENT )
! ###########################################################
! Subroutine to determine the position of a station/profiler on the model grid
! and set the useful coefficients for data interpolation

  USE MODD_CONF,           ONLY: L1D
  USE MODD_GRID_n,         ONLY: NPHYS_XMIN, NPHYS_XMAX, NPHYS_YMIN, NPHYS_YMAX, XHAT_BOUND, XHATM_BOUND, &
                                 XXHAT, XYHAT, XXHATM, XYHATM, XZZ
  USE MODD_PARAMETERS,     ONLY: JPHEXT, JPVEXT

  USE MODE_MSG
  USE MODE_NEST_LL,        ONLY: GET_MODEL_NUMBER_ll
  USE MODE_TOOLS_ll,       ONLY: GET_INDICE_ll

  IMPLICIT NONE

  CLASS(TSTATPROFDATA), INTENT(INOUT) :: TPSTATPROF
  LOGICAL,              INTENT(OUT)   :: OINSIDE  ! True if station/profiler is inside physical domain of model
  LOGICAL,              INTENT(OUT)   :: OPRESENT ! True if station/profiler is present on the current process

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

  IF (       TPSTATPROF%XX >= XHAT_BOUND(NPHYS_XMIN) .AND. TPSTATPROF%XX <= XHAT_BOUND(NPHYS_XMAX) &
       .AND. TPSTATPROF%XY >= XHAT_BOUND(NPHYS_YMIN) .AND. TPSTATPROF%XY <= XHAT_BOUND(NPHYS_YMAX) ) THEN
    OINSIDE = .TRUE.
  ELSE
    CALL GET_MODEL_NUMBER_ll(IMI)
    WRITE( CMNHMSG(1), "( 'station or profiler ', A, ' is outside of physical domain of model', I3 )" ) TRIM(TPSTATPROF%CNAME), IMI
    CALL PRINT_MSG( NVERB_WARNING, 'GEN', 'STATPROF_POSITION' )
 END IF

  ! X position
  TPSTATPROF%NI_U = COUNT( XXHAT (:) <= TPSTATPROF%XX )
  TPSTATPROF%NI_M = COUNT( XXHATM(:) <= TPSTATPROF%XX )

  ! Y position
  TPSTATPROF%NJ_V = COUNT( XYHAT (:) <= TPSTATPROF%XY )
  TPSTATPROF%NJ_M = COUNT( XYHATM(:) <= TPSTATPROF%XY )

  ! Position of station/profiler according to process
  IF (       TPSTATPROF%NI_U >= IIB .AND. TPSTATPROF%NI_U <= IIE &
       .AND. TPSTATPROF%NJ_V >= IJB .AND. TPSTATPROF%NJ_V <= IJE ) OPRESENT = .TRUE.
  IF ( L1D ) OPRESENT = .TRUE.

  ! Check if station/profiler is too near of physical domain border (outside of physical domain for mass points)
  IF ( OINSIDE .AND. .NOT. L1D ) THEN
    IF (      TPSTATPROF%XX < XHATM_BOUND(NPHYS_XMIN) .OR. TPSTATPROF%XX > XHATM_BOUND(NPHYS_XMAX) &
         .OR. TPSTATPROF%XY < XHATM_BOUND(NPHYS_YMIN) .OR. TPSTATPROF%XY > XHATM_BOUND(NPHYS_YMAX) ) THEN
      CALL GET_MODEL_NUMBER_ll(IMI)
      WRITE( CMNHMSG(1), "( 'station or profiler ', A, ' is outside of mass-points physical domain of model', I3 )" ) &
             TRIM(TPSTATPROF%CNAME), IMI
      CMNHMSG(2) = 'but is inside of flux-points physical domain.'
      CMNHMSG(3) = 'Meaning: station or profiler is too close to the boundaries of physical domain.'
      CMNHMSG(4) = '=> station or profiler disabled (not computed)'
      CALL PRINT_MSG( NVERB_WARNING, 'GEN', 'STATPROF_POSITION' )
      OPRESENT = .FALSE.
      OINSIDE = .FALSE.
    END IF
  END IF

  ! Computations only on correct process
  IF ( OPRESENT .AND. .NOT. L1D ) THEN
    ! Interpolation coefficient for X (mass-point)
    TPSTATPROF%XXMCOEF = ( TPSTATPROF%XX - XXHATM(TPSTATPROF%NI_M) ) / ( XXHATM(TPSTATPROF%NI_M+1) - XXHATM(TPSTATPROF%NI_M) )
    ! Interpolation coefficient for Y (mass-point)
    TPSTATPROF%XYMCOEF = ( TPSTATPROF%XY - XYHATM(TPSTATPROF%NJ_M) ) / ( XYHATM(TPSTATPROF%NJ_M+1) - XYHATM(TPSTATPROF%NJ_M) )
    ! Interpolation coefficient for X (U-point)
    TPSTATPROF%XXUCOEF = ( TPSTATPROF%XX - XXHAT(TPSTATPROF%NI_U) )  / ( XXHAT(TPSTATPROF%NI_U+1)  - XXHAT(TPSTATPROF%NI_U) )
    ! Interpolation coefficient for Y (V-point)
    TPSTATPROF%XYVCOEF = ( TPSTATPROF%XY - XYHAT(TPSTATPROF%NJ_V) )  / ( XYHAT(TPSTATPROF%NJ_V+1)  - XYHAT(TPSTATPROF%NJ_V) )
  END IF

  IF ( OPRESENT ) THEN
    SELECT TYPE( TPSTATPROF )
      TYPE IS( TPROFILERDATA )
        ! Nothing to do

      TYPE IS( TSTATIONDATA )
        ! The closest K-level to the station altitude is chosen
        JK = JPVEXT + 1
        DO WHILE ( ( STATPROF_INTERP_2D( TPSTATPROF, XZZ(:,:,JK) ) - STATPROF_INTERP_2D( TPSTATPROF, XZZ(:,:,JPVEXT+1) ) ) &
                   < TPSTATPROF%XZ)
          JK = JK + 1
        END DO
        ZLOW  = STATPROF_INTERP_2D( TPSTATPROF, XZZ(:,:,JK-1) ) - STATPROF_INTERP_2D( TPSTATPROF, XZZ(:,:,JPVEXT+1) )
        ZHIGH = STATPROF_INTERP_2D( TPSTATPROF, XZZ(:,:,JK  ) ) - STATPROF_INTERP_2D( TPSTATPROF, XZZ(:,:,JPVEXT+1) )
        !If the station/profiler is nearer from the lower level, select it
        IF ( ( ZHIGH - TPSTATPROF%XZ ) > ( TPSTATPROF%XZ - ZLOW ) ) JK = JK - 1
        TPSTATPROF%NK = JK
        TPSTATPROF%XZMEAS = STATPROF_INTERP_2D( TPSTATPROF, XZZ(:,:,JK) )

      CLASS DEFAULT
        CALL PRINT_MSG( NVERB_ERROR, 'GEN', 'STATPROF_POSITION', 'unknown type for TPSTATPROF', OLOCAL = .TRUE. )
    END SELECT
  END IF

END SUBROUTINE STATPROF_POSITION

! ###################################
SUBROUTINE PROFILER_ADD( TPPROFILER )
! ###################################
! Subroutine to add a station to the local list of profilers
  USE MODD_PROFILER_n, ONLY: NUMBPROFILER_LOC, TPROFILERS

  IMPLICIT NONE

  CLASS(TPROFILERDATA), INTENT(IN) :: TPPROFILER

  INTEGER :: JS
  TYPE(TPROFILERDATA), DIMENSION(:), POINTER :: TZPROFILERS

  NUMBPROFILER_LOC = NUMBPROFILER_LOC + 1

  ALLOCATE( TZPROFILERS( NUMBPROFILER_LOC ) )
  DO JS = 1, NUMBPROFILER_LOC - 1
    TZPROFILERS(JS) = TPROFILERS(JS)
  END DO

  !Copy fields available in TSTATPROFDATA
  !other fields are not yet set
  TZPROFILERS(NUMBPROFILER_LOC)%CNAME   = TPPROFILER%CNAME
  TZPROFILERS(NUMBPROFILER_LOC)%NID     = TPPROFILER%NID
  TZPROFILERS(NUMBPROFILER_LOC)%XX      = TPPROFILER%XX
  TZPROFILERS(NUMBPROFILER_LOC)%XY      = TPPROFILER%XY
  TZPROFILERS(NUMBPROFILER_LOC)%XZ      = TPPROFILER%XZ
  TZPROFILERS(NUMBPROFILER_LOC)%XLON    = TPPROFILER%XLON
  TZPROFILERS(NUMBPROFILER_LOC)%XLAT    = TPPROFILER%XLAT
  TZPROFILERS(NUMBPROFILER_LOC)%NI_M    = TPPROFILER%NI_M
  TZPROFILERS(NUMBPROFILER_LOC)%NJ_M    = TPPROFILER%NJ_M
  TZPROFILERS(NUMBPROFILER_LOC)%NI_U    = TPPROFILER%NI_U
  TZPROFILERS(NUMBPROFILER_LOC)%NJ_V    = TPPROFILER%NJ_V
  TZPROFILERS(NUMBPROFILER_LOC)%XXMCOEF = TPPROFILER%XXMCOEF
  TZPROFILERS(NUMBPROFILER_LOC)%XYMCOEF = TPPROFILER%XYMCOEF
  TZPROFILERS(NUMBPROFILER_LOC)%XXUCOEF = TPPROFILER%XXUCOEF
  TZPROFILERS(NUMBPROFILER_LOC)%XYVCOEF = TPPROFILER%XYVCOEF
  TZPROFILERS(NUMBPROFILER_LOC)%CTYPE   = TPPROFILER%CTYPE

  IF ( ASSOCIATED( TPROFILERS ) ) DEALLOCATE( TPROFILERS ) !Can be done without memory leak because allocatable arrays were
                                                           !not yet allocated (will be done in PROFILER_ALLOCATE)
  TPROFILERS => TZPROFILERS

END SUBROUTINE PROFILER_ADD

! #################################
SUBROUTINE STATION_ADD( TPSTATION )
! #################################
! Subroutine to add a station to the local list of stations
  USE MODD_STATION_n, ONLY: NUMBSTAT_LOC, TSTATIONS

  IMPLICIT NONE

  CLASS(TSTATIONDATA), INTENT(IN) :: TPSTATION

  INTEGER :: JS
  TYPE(TSTATIONDATA), DIMENSION(:), POINTER :: TZSTATIONS

  NUMBSTAT_LOC = NUMBSTAT_LOC + 1

  ALLOCATE( TZSTATIONS( NUMBSTAT_LOC ) )
  DO JS = 1, NUMBSTAT_LOC - 1
    TZSTATIONS(JS) = TSTATIONS(JS)
  END DO

  !Copy fields available in TSTATPROFDATA
  !other fields are not yet set
  TZSTATIONS(NUMBSTAT_LOC)%CNAME   = TPSTATION%CNAME
  TZSTATIONS(NUMBSTAT_LOC)%NID     = TPSTATION%NID
  TZSTATIONS(NUMBSTAT_LOC)%XX      = TPSTATION%XX
  TZSTATIONS(NUMBSTAT_LOC)%XY      = TPSTATION%XY
  TZSTATIONS(NUMBSTAT_LOC)%XZ      = TPSTATION%XZ
  TZSTATIONS(NUMBSTAT_LOC)%XLON    = TPSTATION%XLON
  TZSTATIONS(NUMBSTAT_LOC)%XLAT    = TPSTATION%XLAT
  TZSTATIONS(NUMBSTAT_LOC)%NI_M    = TPSTATION%NI_M
  TZSTATIONS(NUMBSTAT_LOC)%NJ_M    = TPSTATION%NJ_M
  TZSTATIONS(NUMBSTAT_LOC)%NI_U    = TPSTATION%NI_U
  TZSTATIONS(NUMBSTAT_LOC)%NJ_V    = TPSTATION%NJ_V
  TZSTATIONS(NUMBSTAT_LOC)%XXMCOEF = TPSTATION%XXMCOEF
  TZSTATIONS(NUMBSTAT_LOC)%XYMCOEF = TPSTATION%XYMCOEF
  TZSTATIONS(NUMBSTAT_LOC)%XXUCOEF = TPSTATION%XXUCOEF
  TZSTATIONS(NUMBSTAT_LOC)%XYVCOEF = TPSTATION%XYVCOEF
  TZSTATIONS(NUMBSTAT_LOC)%NK      = TPSTATION%NK
  TZSTATIONS(NUMBSTAT_LOC)%XZMEAS  = TPSTATION%XZMEAS

  IF ( ASSOCIATED( TSTATIONS ) ) DEALLOCATE( TSTATIONS ) !Can be done without memory leak because allocatable arrays were
                                                         !not yet allocated (will be done in STATION_ALLOCATE)
  TSTATIONS => TZSTATIONS

END SUBROUTINE STATION_ADD

! ########################################################
FUNCTION STATPROF_INTERP_2D( TPSTATPROF, PA ) RESULT( PB )
! ########################################################
  USE MODD_CONF,       ONLY: L1D
  USE MODD_PARAMETERS, ONLY: XUNDEF

  USE MODE_MSG

  IMPLICIT NONE

  CLASS(TSTATPROFDATA), INTENT(IN) :: TPSTATPROF
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
    JI = TPSTATPROF%NI_M
    JJ = TPSTATPROF%NJ_M
  END IF

  IF (       JI >= 1 .AND. JI < SIZE( PA, 1 ) &
       .AND. JJ >= 1 .AND. JJ < SIZE( PA, 2 ) ) THEN
    PB = (1.-TPSTATPROF%XXMCOEF) *  (1.-TPSTATPROF%XYMCOEF) * PA(JI,   JJ  ) + &
         (   TPSTATPROF%XXMCOEF) *  (1.-TPSTATPROF%XYMCOEF) * PA(JI+1, JJ  ) + &
         (1.-TPSTATPROF%XXMCOEF) *  (   TPSTATPROF%XYMCOEF) * PA(JI,   JJ+1) + &
         (   TPSTATPROF%XXMCOEF) *  (   TPSTATPROF%XYMCOEF) * PA(JI+1, JJ+1)
  ELSE
    CALL PRINT_MSG( NVERB_ERROR, 'GEN', 'STATPROF_INTERP_2D', 'value can not be interpolated', OLOCAL = .TRUE. )
    PB = XUNDEF
  END IF

END FUNCTION STATPROF_INTERP_2D

! ##########################################################
FUNCTION STATPROF_INTERP_2D_U( TPSTATPROF, PA ) RESULT( PB )
! ##########################################################
  USE MODD_CONF,       ONLY: L1D
  USE MODD_PARAMETERS, ONLY: XUNDEF

  USE MODE_MSG

  IMPLICIT NONE

  CLASS(TSTATPROFDATA), INTENT(IN) :: TPSTATPROF
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
    JI = TPSTATPROF%NI_U
    JJ = TPSTATPROF%NJ_M
  END IF

  IF (       JI >= 1 .AND. JI < SIZE( PA, 1 ) &
       .AND. JJ >= 1 .AND. JJ < SIZE( PA, 2 ) ) THEN
    PB = (1.-TPSTATPROF%XXUCOEF) *  (1.-TPSTATPROF%XYMCOEF) * PA(JI,   JJ  ) + &
         (   TPSTATPROF%XXUCOEF) *  (1.-TPSTATPROF%XYMCOEF) * PA(JI+1, JJ  ) + &
         (1.-TPSTATPROF%XXUCOEF) *  (   TPSTATPROF%XYMCOEF) * PA(JI,   JJ+1) + &
         (   TPSTATPROF%XXUCOEF) *  (   TPSTATPROF%XYMCOEF) * PA(JI+1, JJ+1)
  ELSE
    CALL PRINT_MSG( NVERB_ERROR, 'GEN', 'STATPROF_INTERP_2D_U', 'value can not be interpolated', OLOCAL = .TRUE. )
    PB = XUNDEF
  END IF

END FUNCTION STATPROF_INTERP_2D_U

! ##########################################################
FUNCTION STATPROF_INTERP_2D_V( TPSTATPROF, PA ) RESULT( PB )
! ##########################################################
  USE MODD_CONF,       ONLY: L1D
  USE MODD_PARAMETERS, ONLY: XUNDEF

  USE MODE_MSG

  IMPLICIT NONE

  CLASS(TSTATPROFDATA), INTENT(IN) :: TPSTATPROF
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
    JI = TPSTATPROF%NI_M
    JJ = TPSTATPROF%NJ_V
  END IF

  IF (       JI >= 1 .AND. JI < SIZE( PA, 1 ) &
       .AND. JJ >= 1 .AND. JJ < SIZE( PA, 2 ) ) THEN
    PB = (1.-TPSTATPROF%XXMCOEF) *  (1.-TPSTATPROF%XYVCOEF) * PA(JI,   JJ  ) + &
         (   TPSTATPROF%XXMCOEF) *  (1.-TPSTATPROF%XYVCOEF) * PA(JI+1, JJ  ) + &
         (1.-TPSTATPROF%XXMCOEF) *  (   TPSTATPROF%XYVCOEF) * PA(JI,   JJ+1) + &
         (   TPSTATPROF%XXMCOEF) *  (   TPSTATPROF%XYVCOEF) * PA(JI+1, JJ+1)
  ELSE
    CALL PRINT_MSG( NVERB_ERROR, 'GEN', 'STATPROF_INTERP_2D_V', 'value can not be interpolated', OLOCAL = .TRUE. )
    PB = XUNDEF
  END IF

END FUNCTION STATPROF_INTERP_2D_V

! ########################################################
FUNCTION STATPROF_INTERP_3D( TPSTATPROF, PA ) RESULT( PB )
! ########################################################
  USE MODD_CONF,       ONLY: L1D
  USE MODD_PARAMETERS, ONLY: XUNDEF

  USE MODE_MSG

  IMPLICIT NONE

  CLASS(TSTATPROFDATA),   INTENT(IN) :: TPSTATPROF
  REAL, DIMENSION(:,:,:), INTENT(IN) :: PA
  REAL, DIMENSION(SIZE(PA,3))        :: PB

  INTEGER :: JI, JJ, JK

  IF ( SIZE( PA, 1 ) == 2 ) THEN
    JI = 1
    JJ = 1
  ELSE IF ( L1D ) THEN
    JI = 2
    JJ = 2
  ELSE
    JI = TPSTATPROF%NI_M
    JJ = TPSTATPROF%NJ_M
  END IF

  IF (       JI >= 1 .AND. JI < SIZE( PA, 1 ) &
       .AND. JJ >= 1 .AND. JJ < SIZE( PA, 2 ) ) THEN
    DO JK = 1, SIZE( PA, 3 )
      IF ( PA(JI, JJ,   JK) /= XUNDEF .AND. PA(JI+1, JJ,   JK) /= XUNDEF .AND. &
           PA(JI, JJ+1, JK) /= XUNDEF .AND. PA(JI+1, JJ+1, JK) /= XUNDEF       ) THEN
        PB(JK) = (1.-TPSTATPROF%XXMCOEF) *  (1.-TPSTATPROF%XYMCOEF) * PA(JI,   JJ,   JK) + &
                 (   TPSTATPROF%XXMCOEF) *  (1.-TPSTATPROF%XYMCOEF) * PA(JI+1, JJ,   JK) + &
                 (1.-TPSTATPROF%XXMCOEF) *  (   TPSTATPROF%XYMCOEF) * PA(JI,   JJ+1, JK) + &
                 (   TPSTATPROF%XXMCOEF) *  (   TPSTATPROF%XYMCOEF) * PA(JI+1, JJ+1, JK)
      ELSE
        PB(JK) = XUNDEF
      END IF
    END DO
  ELSE
    CALL PRINT_MSG( NVERB_ERROR, 'GEN', 'STATPROF_INTERP_3D', 'value can not be interpolated', OLOCAL = .TRUE. )
    PB(:) = XUNDEF
  END IF

END FUNCTION STATPROF_INTERP_3D

! ##########################################################
FUNCTION STATPROF_INTERP_3D_U( TPSTATPROF, PA ) RESULT( PB )
! ##########################################################
  USE MODD_CONF,       ONLY: L1D
  USE MODD_PARAMETERS, ONLY: XUNDEF

  USE MODE_MSG

  IMPLICIT NONE

  CLASS(TSTATPROFDATA),   INTENT(IN) :: TPSTATPROF
  REAL, DIMENSION(:,:,:), INTENT(IN) :: PA
  REAL, DIMENSION(SIZE(PA,3))        :: PB

  INTEGER :: JI, JJ

  IF ( SIZE( PA, 1 ) == 2 ) THEN
    JI = 1
    JJ = 1
  ELSE IF ( L1D ) THEN
    JI = 2
    JJ = 2
  ELSE
    JI = TPSTATPROF%NI_U
    JJ = TPSTATPROF%NJ_M
  END IF

  IF (       JI >= 1 .AND. JI < SIZE( PA, 1 ) &
       .AND. JJ >= 1 .AND. JJ < SIZE( PA, 2 ) ) THEN
    PB(:) = (1.-TPSTATPROF%XXUCOEF) *  (1.-TPSTATPROF%XYMCOEF) * PA(JI,   JJ,   :) + &
            (   TPSTATPROF%XXUCOEF) *  (1.-TPSTATPROF%XYMCOEF) * PA(JI+1, JJ,   :) + &
            (1.-TPSTATPROF%XXUCOEF) *  (   TPSTATPROF%XYMCOEF) * PA(JI,   JJ+1, :) + &
            (   TPSTATPROF%XXUCOEF) *  (   TPSTATPROF%XYMCOEF) * PA(JI+1, JJ+1, :)
  ELSE
    CALL PRINT_MSG( NVERB_ERROR, 'GEN', 'STATPROF_INTERP_3D_U', 'value can not be interpolated', OLOCAL = .TRUE. )
    PB = XUNDEF
  END IF

END FUNCTION STATPROF_INTERP_3D_U

! ##########################################################
FUNCTION STATPROF_INTERP_3D_V( TPSTATPROF, PA ) RESULT( PB )
! ##########################################################
  USE MODD_CONF,       ONLY: L1D
  USE MODD_PARAMETERS, ONLY: XUNDEF

  USE MODE_MSG

  IMPLICIT NONE

  CLASS(TSTATPROFDATA),   INTENT(IN) :: TPSTATPROF
  REAL, DIMENSION(:,:,:), INTENT(IN) :: PA
  REAL, DIMENSION(SIZE(PA,3))        :: PB

  INTEGER :: JI, JJ

  IF ( SIZE( PA, 1 ) == 2 ) THEN
    JI = 1
    JJ = 1
  ELSE IF ( L1D ) THEN
    JI = 2
    JJ = 2
  ELSE
    JI = TPSTATPROF%NI_M
    JJ = TPSTATPROF%NJ_V
  END IF

  IF (       JI >= 1 .AND. JI < SIZE( PA, 1 ) &
       .AND. JJ >= 1 .AND. JJ < SIZE( PA, 2 ) ) THEN
    PB(:) = (1.-TPSTATPROF%XXMCOEF) *  (1.-TPSTATPROF%XYVCOEF) * PA(JI,   JJ,   :) + &
            (   TPSTATPROF%XXMCOEF) *  (1.-TPSTATPROF%XYVCOEF) * PA(JI+1, JJ,   :) + &
            (1.-TPSTATPROF%XXMCOEF) *  (   TPSTATPROF%XYVCOEF) * PA(JI,   JJ+1, :) + &
            (   TPSTATPROF%XXMCOEF) *  (   TPSTATPROF%XYVCOEF) * PA(JI+1, JJ+1, :)
  ELSE
    CALL PRINT_MSG( NVERB_ERROR, 'GEN', 'STATPROF_INTERP_3D_V', 'value can not be interpolated', OLOCAL = .TRUE. )
    PB = XUNDEF
  END IF

END FUNCTION STATPROF_INTERP_3D_V

! #################################################
SUBROUTINE STATPROF_INSTANT( TPSTATPROF_TIME, KIN )
! #################################################
  USE MODD_TIME_n,     ONLY: TDTCUR

  USE MODE_DATETIME
  USE MODE_MSG

  IMPLICIT NONE

  TYPE(TSTATPROFTIME), INTENT(INOUT) :: TPSTATPROF_TIME
  INTEGER,             INTENT(OUT)   :: KIN ! Current step of storage

  IF ( TPSTATPROF_TIME%N_CUR == 0 ) THEN
    ! First store
    TPSTATPROF_TIME%N_CUR = 1
    TPSTATPROF_TIME%TPDATES(1) = TDTCUR
    KIN = 1
  ELSE
    IF ( TDTCUR - TPSTATPROF_TIME%TPDATES(TPSTATPROF_TIME%N_CUR) >= TPSTATPROF_TIME%XTSTEP - 1.E-6 ) THEN
      TPSTATPROF_TIME%N_CUR = TPSTATPROF_TIME%N_CUR + 1
      KIN = TPSTATPROF_TIME%N_CUR

      IF ( KIN < 1 .OR. KIN > SIZE( TPSTATPROF_TIME%TPDATES ) ) THEN
        CALL PRINT_MSG( NVERB_ERROR, 'GEN', 'STATPROF_INSTANT', 'problem with step of storage' )
        KIN = -2
      ELSE
        TPSTATPROF_TIME%TPDATES(KIN) = TDTCUR
      END IF
    ELSE
      ! Return an invalid step number
      KIN = -1
    END IF
  END IF

END SUBROUTINE STATPROF_INSTANT

END MODULE MODE_STATPROF_TOOLS
