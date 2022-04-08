!MNH_LIC Copyright 2022-2022 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
! Author:
!  P. Wautelet 08/04/2022
!-----------------------------------------------------------------
! Modifications:
!-----------------------------------------------------------------
!      ##################
MODULE MODE_STATION_TOOLS
!      ##################

IMPLICIT NONE

PRIVATE

PUBLIC :: STATION_POSITION

CONTAINS

! ######################################
SUBROUTINE STATION_POSITION( TPSTATION )
! ######################################
! Subroutine to determine the position of a station on the model grid
! and set the useful coefficient for data interpolation

  USE MODD_CONF,         ONLY: L1D
  USE MODD_GRID_n,       ONLY: XXHAT, XYHAT
  USE MODD_TYPE_STATION, ONLY: TSTATIONDATA

  USE MODE_TOOLS_ll,     ONLY: GET_INDICE_ll, LWEST_ll, LEAST_ll, LNORTH_ll, LSOUTH_ll

  IMPLICIT NONE

  TYPE(TSTATIONDATA), INTENT(INOUT) :: TPSTATION

  INTEGER :: IIB ! domain sizes of current process
  INTEGER :: IJB !
  INTEGER :: IIE !
  INTEGER :: IJE !
  INTEGER :: IIU !
  INTEGER :: IJU !
  REAL, DIMENSION(SIZE(XXHAT)) :: ZXHATM ! mass point coordinates
  REAL, DIMENSION(SIZE(XYHAT)) :: ZYHATM ! mass point coordinates

  IIU = SIZE( XXHAT )
  IJU = SIZE( XYHAT )

  CALL GET_INDICE_ll (IIB, IJB, IIE, IJE )

  ! Interpolations of model variables to mass points
  ZXHATM(1:IIU-1) = 0.5 * XXHAT(1:IIU-1) + 0.5 * XXHAT(2:IIU  )
  ZXHATM(  IIU  ) = 1.5 * XXHAT(  IIU  ) - 0.5 * XXHAT(  IIU-1)

  ZYHATM(1:IJU-1) = 0.5 * XYHAT(1:IJU-1) + 0.5 * XYHAT(2:IJU  )
  ZYHATM(  IJU  ) = 1.5 * XYHAT(  IJU  ) - 0.5 * XYHAT(  IJU-1)

  TPSTATION%LPRESENT = .FALSE.

  ! X position
  TPSTATION%NI_U = COUNT( XXHAT (:) <= TPSTATION%XX )
  TPSTATION%NI_M = COUNT( ZXHATM(:) <= TPSTATION%XX )

  IF ( TPSTATION%NI_M<=IIB-1 .AND. LWEST_ll() .AND. .NOT. L1D ) TPSTATION%LERROR = .TRUE.
  IF ( TPSTATION%NI_M>=IIE   .AND. LEAST_ll() .AND. .NOT. L1D ) TPSTATION%LERROR = .TRUE.

  ! Y position
  TPSTATION%NJ_V = COUNT( XYHAT (:) <= TPSTATION%XY )
  TPSTATION%NJ_M = COUNT( ZYHATM(:) <= TPSTATION%XY )

  IF ( TPSTATION%NJ_M<=IJB-1 .AND. LSOUTH_ll() .AND. .NOT. L1D ) TPSTATION%LERROR = .TRUE.
  IF ( TPSTATION%NJ_M>=IJE   .AND. LNORTH_ll() .AND. .NOT. L1D ) TPSTATION%LERROR = .TRUE.

  ! Position of station according to processes
  IF (       TPSTATION%NI_U >= IIB .AND. TPSTATION%NI_U <= IIE &
       .AND. TPSTATION%NJ_V >= IJB .AND. TPSTATION%NJ_V <= IJE ) TPSTATION%LPRESENT = .TRUE.
  IF ( L1D ) TPSTATION%LPRESENT = .TRUE.

  ! Computations only on correct process
  IF ( TPSTATION%LPRESENT .AND. .NOT. L1D ) THEN
    ! Interpolation coefficient for X (mass-point)
    TPSTATION%XXMCOEF = ( TPSTATION%XX - ZXHATM(TPSTATION%NI_M) ) / ( ZXHATM(TPSTATION%NI_M+1) - ZXHATM(TPSTATION%NI_M) )
    ! Interpolation coefficient for Y (mass-point)
    TPSTATION%XYMCOEF = ( TPSTATION%XY - ZYHATM(TPSTATION%NJ_M) ) / ( ZYHATM(TPSTATION%NJ_M+1) - ZYHATM(TPSTATION%NJ_M) )
    ! Interpolation coefficient for X (U-point)
    TPSTATION%XXUCOEF = ( TPSTATION%XX - XXHAT(TPSTATION%NI_U) )  / ( XXHAT(TPSTATION%NI_U+1)  - XXHAT(TPSTATION%NI_U) )
    ! Interpolation coefficient for Y (V-point)
    TPSTATION%XYVCOEF = ( TPSTATION%XY - XYHAT(TPSTATION%NJ_V) )  / ( XYHAT(TPSTATION%NJ_V+1)  - XYHAT(TPSTATION%NJ_V) )
  END IF

END SUBROUTINE STATION_POSITION

END MODULE MODE_STATION_TOOLS
