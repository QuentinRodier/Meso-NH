!MNH_LIC Copyright 2022-2022 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
! Author:
!  P. Wautelet 25/11/2022
! Modifications:
!-----------------------------------------------------------------
!      ###################
MODULE MODE_POSITION_TOOLS
!      ###################

USE MODE_MSG

IMPLICIT NONE

PRIVATE

PUBLIC :: FIND_PROCESS_AND_MODEL_FROM_XY_POS

CONTAINS

!----------------------------------------------------------------------------
SUBROUTINE FIND_PROCESS_FROM_XY_POS( PX, PY, TPMODEL, KRANK, GINSIDE )
  ! Find the rank of the process with the given position
  USE MODD_FIELD,        ONLY: TFIELDLIST
  USE MODD_IO,           ONLY: ISNPROC
  USE MODD_PARAMETERS,   ONLY: NNEGUNDEF
  USE MODD_STRUCTURE_ll, ONLY: PROCONF_ll

  USE MODE_FIELD, ONLY: FIND_FIELD_ID_FROM_MNHNAME

  IMPLICIT NONE

  REAL,             INTENT(IN)  :: PX
  REAL,             INTENT(IN)  :: PY
  TYPE(PROCONF_ll), INTENT(IN)  :: TPMODEL
  INTEGER,          INTENT(OUT) :: KRANK
  LOGICAL,          INTENT(OUT) :: GINSIDE

  INTEGER :: IID
  INTEGER :: IRESP
  INTEGER :: IXPOS
  INTEGER :: IYPOS
  INTEGER :: JP
  REAL, DIMENSION(:), POINTER :: ZXHAT_ll
  REAL, DIMENSION(:), POINTER :: ZYHAT_ll

  GINSIDE = .FALSE.
  KRANK = NNEGUNDEF

  ZXHAT_ll => NULL()
  ZYHAT_ll => NULL()

  call Find_field_id_from_mnhname( 'XHAT_ll', iid, iresp )
  ZXHAT_ll => tfieldlist(iid)%tfield_x1d(TPMODEL%NUMBER)%data
  call Find_field_id_from_mnhname( 'YHAT_ll', iid, iresp )
  ZYHAT_ll => tfieldlist(iid)%tfield_x1d(TPMODEL%NUMBER)%data

  IXPOS = COUNT( ZXHAT_ll(:) <= PX )
  IYPOS = COUNT( ZYHAT_ll(:) <= PY )

  DO JP = 1, ISNPROC
    IF (       IXPOS >= TPMODEL%TSPLITS_B(JP)%NXORP .AND. IXPOS <= TPMODEL%TSPLITS_B(JP)%NXENDP &
         .AND. IYPOS >= TPMODEL%TSPLITS_B(JP)%NYORP .AND. IYPOS <= TPMODEL%TSPLITS_B(JP)%NYENDP ) THEN
      GINSIDE = .TRUE.
      KRANK = JP
      EXIT
    END IF
  END DO

END SUBROUTINE FIND_PROCESS_FROM_XY_POS
!----------------------------------------------------------------------------

!----------------------------------------------------------------------------
SUBROUTINE FIND_PROCESS_AND_MODEL_FROM_XY_POS( PX, PY, KRANK, KMODEL )
  ! Find the rank of the process with the given position
  ! on the most refined model (except if model number is forced)

  USE MODD_IO,           ONLY: ISNPROC
  USE MODD_PARAMETERS,   ONLY: NNEGUNDEF
  USE MODD_STRUCTURE_ll, ONLY: PROCONF_ll
  USE MODD_VAR_ll,       ONLY: TCRRT_PROCONF

  IMPLICIT NONE

  REAL,    INTENT(IN)    :: PX
  REAL,    INTENT(IN)    :: PY
  INTEGER, INTENT(OUT)   :: KRANK  ! If < 1, position is outside domain(s)
  INTEGER, INTENT(INOUT) :: KMODEL ! If > 0 at entry, model is fixed, else it is set by subroutine

  CHARACTER(LEN=3) :: YMODEL
  LOGICAL          :: GFOUND
  LOGICAL          :: OINSIDE
  TYPE(PROCONF_ll), POINTER :: TZMODEL

  KRANK = NNEGUNDEF

  TZMODEL => TCRRT_PROCONF

  ! Go back to the root model
  DO WHILE( ASSOCIATED( TZMODEL%TPARENT ) )
    TZMODEL => TZMODEL%TPARENT
  END DO

  IF ( KMODEL > 0 ) THEN
    ! Find the configuration corresponding to KMODEL
    ! based on GO_TOMODEL_ll

    ! Find the model configuration
    CALL FIND_MODEL( TZMODEL, KMODEL, GFOUND )

    IF ( .NOT. GFOUND ) THEN
      WRITE( YMODEL, '( I3 )' ) KMODEL
      CALL PRINT_MSG( NVERB_FATAL, 'GEN', 'FIND_PROCESS_AND_MODEL_FROM_XY_POS', 'model ' // YMODEL // ' not found' )
    END IF

    ! Find the rank of the process where the position is
    CALL FIND_PROCESS_FROM_XY_POS( PX, PY, TZMODEL, KRANK, OINSIDE )
  ELSE
    ! Model number is not fixed => find the finer model corresponding to the position
    CALL FIND_FINER_MODEL_WITH_XY_POS( PX, PY, TZMODEL, KMODEL, KRANK, OINSIDE )
  END IF

END SUBROUTINE FIND_PROCESS_AND_MODEL_FROM_XY_POS
!----------------------------------------------------------------------------


!----------------------------------------------------------------------------
RECURSIVE SUBROUTINE FIND_FINER_MODEL_WITH_XY_POS( PX, PY, TPMODEL, KMODEL, KRANK, OINSIDE )
  USE MODD_PARAMETERS,   ONLY: NNEGUNDEF
  USE MODD_STRUCTURE_ll, ONLY: LPROCONF_ll, PROCONF_ll

  IMPLICIT NONE

  REAL,                      INTENT(IN)    :: PX
  REAL,                      INTENT(IN)    :: PY
  TYPE(PROCONF_ll), POINTER, INTENT(INOUT) :: TPMODEL
  INTEGER,                   INTENT(INOUT) :: KMODEL
  INTEGER,                   INTENT(OUT)   :: KRANK
  LOGICAL,                   INTENT(OUT)   :: OINSIDE

  INTEGER :: IRANK
  LOGICAL :: GINSIDE
  TYPE(PROCONF_ll),  POINTER :: TZCHILD
  TYPE(LPROCONF_ll), POINTER :: TZMODELS

  IRANK = NNEGUNDEF
  GINSIDE = .FALSE.

  CALL FIND_PROCESS_FROM_XY_POS( PX, PY, TPMODEL, IRANK, GINSIDE )
  IF ( GINSIDE ) THEN
    KMODEL = TPMODEL%NUMBER
    KRANK = IRANK
  END IF
  OINSIDE = GINSIDE

  IF ( .NOT. GINSIDE ) RETURN

  !If the coordinates are inside the current model, look at its children
  TZMODELS => TPMODEL%TCHILDREN
  DO WHILE(ASSOCIATED(TZMODELS))
    TZCHILD => TZMODELS%TELT

    CALL FIND_FINER_MODEL_WITH_XY_POS( PX, PY, TZCHILD, KMODEL, IRANK, GINSIDE )

    IF ( .NOT. GINSIDE ) THEN
      TZMODELS => TZMODELS%TNEXT
    ELSE
      TPMODEL => TZCHILD
      KMODEL = TPMODEL%NUMBER
      KRANK = IRANK
      OINSIDE = GINSIDE
      RETURN
    END IF
  END DO

END SUBROUTINE FIND_FINER_MODEL_WITH_XY_POS
!----------------------------------------------------------------------------


!----------------------------------------------------------------------------
RECURSIVE SUBROUTINE FIND_MODEL( TPMODEL, KMODEL, OFOUND )
  USE MODD_STRUCTURE_ll, ONLY: LPROCONF_ll, PROCONF_ll

  IMPLICIT NONE

  TYPE(PROCONF_ll), POINTER, INTENT(INOUT) :: TPMODEL
  INTEGER,                   INTENT(IN)    :: KMODEL
  LOGICAL,                   INTENT(OUT)   :: OFOUND

  TYPE(PROCONF_ll),  POINTER :: TZCHILD
  TYPE(LPROCONF_ll), POINTER :: TZMODELS

  OFOUND = .FALSE.

  ! Is the current model the searched one?
  IF (TPMODEL%NUMBER == KMODEL) THEN
    OFOUND = .TRUE.
    RETURN
  ENDIF

  ! no => explore all the children model of the current model
  TZMODELS => TPMODEL%TCHILDREN
  DO WHILE(ASSOCIATED(TZMODELS))

    IF (TZMODELS%TELT%NUMBER == KMODEL) THEN
      OFOUND = .TRUE.
      TPMODEL => TZMODELS%TELT
      RETURN
    END IF

    TZCHILD => TZMODELS%TELT

    CALL FIND_MODEL( TZCHILD, KMODEL, OFOUND )

    IF ( .NOT. OFOUND ) THEN
      TZMODELS => TZMODELS%TNEXT
    ELSE
      TPMODEL => TZCHILD
      RETURN
    END IF

  END DO

END SUBROUTINE FIND_MODEL
!----------------------------------------------------------------------------

END MODULE MODE_POSITION_TOOLS
