!MNH_LIC Copyright 2000-2023 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
! Author: Valery Masson (Meteo-France)
!     Original 15/05/2000
! Modifications:
!  P. Lacarrere   03/2008: add 3D fluxes
!  P. Wautelet 05/2016-04/2018: new data structures and calls for I/O
!  P. Wautelet 13/09/2019: budget: simplify and modernize date/time management
!  P. Wautelet    06/2022: reorganize flyers
!-----------------------------------------------------------------
!      #####################
MODULE MODE_AIRCRAFT_BALLOON
!      #####################

USE MODE_MSG

IMPLICIT NONE

PRIVATE

PUBLIC :: AIRCRAFT_BALLOON

PUBLIC :: AIRCRAFT_BALLOON_LONGTYPE_GET

CONTAINS
!
!     #################################################################
      SUBROUTINE AIRCRAFT_BALLOON(PTSTEP, PZ,                         &
                                  PMAP, PLONOR, PLATOR,               &
                                  PU, PV, PW, PP, PTH, PR, PSV, PTKE, &
                                  PTS, PRHODREF, PCIT, PSEA           )
!     #################################################################
! *AIRCRAFT_BALLOON* - monitor for balloons and aircrafts

USE MODD_AIRCRAFT_BALLOON
USE MODD_TURB_FLUX_AIRCRAFT_BALLOON

USE MODE_AIRCRAFT_BALLOON_EVOL,      ONLY: AIRCRAFT_BALLOON_EVOL

IMPLICIT NONE
!
!*      0.1  declarations of arguments
!
!
REAL,                     INTENT(IN)     :: PTSTEP ! time step
REAL, DIMENSION(:,:,:),   INTENT(IN)     :: PZ     ! z array
REAL, DIMENSION(:,:),     INTENT(IN)     :: PMAP   ! map factor
REAL,                     INTENT(IN)     :: PLONOR ! origine longitude
REAL,                     INTENT(IN)     :: PLATOR ! origine latitude
REAL, DIMENSION(:,:,:),   INTENT(IN)     :: PU     ! horizontal wind X component
REAL, DIMENSION(:,:,:),   INTENT(IN)     :: PV     ! horizontal wind Y component
REAL, DIMENSION(:,:,:),   INTENT(IN)     :: PW     ! vertical wind
REAL, DIMENSION(:,:,:),   INTENT(IN)     :: PP     ! pressure
REAL, DIMENSION(:,:,:),   INTENT(IN)     :: PTH    ! potential temperature
REAL, DIMENSION(:,:,:,:), INTENT(IN)     :: PR     ! water mixing ratios
REAL, DIMENSION(:,:,:,:), INTENT(IN)     :: PSV    ! Scalar variables
REAL, DIMENSION(:,:,:),   INTENT(IN)     :: PTKE   ! turbulent kinetic energy
REAL, DIMENSION(:,:),     INTENT(IN)     :: PTS    ! surface temperature
REAL, DIMENSION(:,:,:),   INTENT(IN)     :: PRHODREF ! dry air density of the reference state
REAL, DIMENSION(:,:,:),   INTENT(IN)     :: PCIT     ! pristine ice concentration
REAL, DIMENSION(:,:), OPTIONAL, INTENT(IN) :: PSEA
!
!-------------------------------------------------------------------------------
!
!       0.2  declaration of local variables
!
INTEGER                      :: JI
LOGICAL, SAVE                :: GFIRSTCALL = .TRUE.
!----------------------------------------------------------------------------

CALL PRINT_MSG( NVERB_DEBUG, 'GEN', 'AIRCRAFT_BALLOON', 'called' )

IF(.NOT. ALLOCATED(XTHW_FLUX)) ALLOCATE(XTHW_FLUX(SIZE(PTH,1),SIZE(PTH,2),SIZE(PTH,3)))
IF(.NOT. ALLOCATED(XRCW_FLUX)) ALLOCATE(XRCW_FLUX(SIZE(PTH,1),SIZE(PTH,2),SIZE(PTH,3)))
IF(.NOT. ALLOCATED(XSVW_FLUX)) ALLOCATE(XSVW_FLUX(SIZE(PSV,1),SIZE(PSV,2),SIZE(PSV,3),SIZE(PSV,4)))

IF ( NBALLOONS > 0 ) THEN
  IF ( GFIRSTCALL ) CALL BALLOONS_INIT_POSITIONS()
  NRANKCUR_BALLOON(:) = NRANKNXT_BALLOON(:)
  NRANKNXT_BALLOON(:) = 0

  DO JI = 1, NBALLOONS
    IF ( ASSOCIATED( TBALLOONS(JI)%TBALLOON ) ) THEN
      CALL AIRCRAFT_BALLOON_EVOL( PTSTEP, PZ, PMAP, PLONOR, PLATOR,                                   &
                                  PU, PV, PW, PP, PTH, PR, PSV, PTKE, PTS, PRHODREF, PCIT,            &
                                  TBALLOONS(JI)%TBALLOON, NRANKCUR_BALLOON(JI), NRANKNXT_BALLOON(JI), &
                                  PSEA )
    END IF
  END DO

  CALL BALLOONS_MOVE_TO_NEW_RANKS()

END IF
!
IF ( NAIRCRAFTS > 0 ) THEN
  IF ( GFIRSTCALL ) CALL AIRCRAFTS_INIT_POSITIONS()
  NRANKCUR_AIRCRAFT(:) = NRANKNXT_AIRCRAFT(:)
  NRANKNXT_AIRCRAFT(:) = 0

  DO JI = 1, NAIRCRAFTS
    IF ( ASSOCIATED( TAIRCRAFTS(JI)%TAIRCRAFT ) ) THEN
      CALL AIRCRAFT_BALLOON_EVOL( PTSTEP, PZ, PMAP, PLONOR, PLATOR,                                       &
                                  PU, PV, PW, PP, PTH, PR, PSV, PTKE, PTS, PRHODREF, PCIT,                &
                                  TAIRCRAFTS(JI)%TAIRCRAFT, NRANKCUR_AIRCRAFT(JI), NRANKNXT_AIRCRAFT(JI), &
                                  PSEA )
    END IF
  END DO

  CALL AIRCRAFTS_MOVE_TO_NEW_RANKS()
END IF

GFIRSTCALL = .FALSE.

CONTAINS

!----------------------------------------------------------------------------
SUBROUTINE AIRCRAFTS_INIT_POSITIONS()

USE MODD_DYN_n,                      ONLY: DYN_MODEL
USE MODD_IO,                         ONLY: ISP
USE MODD_TIME_n,                     ONLY: TDTCUR

USE MODE_AIRCRAFT_BALLOON_EVOL,      ONLY: AIRCRAFT_COMPUTE_POSITION, FLYER_GET_RANK_MODEL_ISCRASHED
USE MODE_DATETIME

INTEGER                      :: IMODEL
REAL                         :: ZDELTATIME
TYPE(DATE_TIME)              :: TZDATE
TYPE(TAIRCRAFTDATA), POINTER :: TZAIRCRAFT

! Set next rank to 0 (necessary for MPI_ALLREDUCE)
NRANKNXT_AIRCRAFT(:) = 0

IF ( ISP == NFLYER_DEFAULT_RANK ) THEN
  DO JI = 1, NAIRCRAFTS
    IF ( .NOT. ASSOCIATED( TAIRCRAFTS(JI)%TAIRCRAFT ) ) &
      CALL PRINT_MSG( NVERB_FATAL, 'GEN', 'AIRCRAFT_BALLOON', 'aircraft structure not associated' )

    ! Compute position at take-off (or at first timestep in flight)
    TZAIRCRAFT => TAIRCRAFTS(JI)%TAIRCRAFT

    ! Determine moment of the first positioning
    ! This is done at first call of this subroutine and therefore not necessarily on the correct model
    IF ( TDTCUR < TZAIRCRAFT%TLAUNCH ) THEN
      ! Moment is the first timestep since launch date
      ZDELTATIME = TZAIRCRAFT%TLAUNCH - TDTCUR + 1.E-8
      IF ( TZAIRCRAFT%CMODEL == 'FIX' ) THEN
        IMODEL = TZAIRCRAFT%NMODEL
      ELSE ! 'MOB'
        IMODEL = 1
      END IF
      TZDATE = TDTCUR + INT( ZDELTATIME / DYN_MODEL(IMODEL)%XTSTEP ) * DYN_MODEL(IMODEL)%XTSTEP
    ELSE IF ( TDTCUR > TZAIRCRAFT%TLAND ) THEN
      ! Nothing to do
      ! Aircraft will never be in flight in this run. Data will remain on the initial process.
    ELSE
      ! Aircraft is already in flight at the beginning of the run
      TZDATE = TDTCUR
    END IF

    CALL AIRCRAFT_COMPUTE_POSITION( TZDATE, TZAIRCRAFT )

    ! Get rank of the process where the aircraft is at this moment and the model number
    CALL FLYER_GET_RANK_MODEL_ISCRASHED( TZAIRCRAFT )

    NRANKNXT_AIRCRAFT(JI) = TZAIRCRAFT%NRANK_CUR
  END DO
END IF

CALL AIRCRAFTS_MOVE_TO_NEW_RANKS()

END SUBROUTINE AIRCRAFTS_INIT_POSITIONS
!----------------------------------------------------------------------------
!----------------------------------------------------------------------------
SUBROUTINE AIRCRAFTS_MOVE_TO_NEW_RANKS()

USE MODD_IO,                         ONLY: ISP
USE MODD_MPIF
USE MODD_PRECISION,                  ONLY: MNHINT_MPI
USE MODD_VAR_ll,                     ONLY: NMNH_COMM_WORLD

INTEGER :: IERR
INTEGER, DIMENSION(:), ALLOCATABLE :: IRANKNXT_AIRCRAFT_TMP

#if 0
CALL MPI_ALLREDUCE( MPI_IN_PLACE, NRANKNXT_AIRCRAFT, NAIRCRAFTS, MNHINT_MPI, MPI_MAX, NMNH_COMM_WORLD, IERR )
#else
!Do this to not use MPI_IN_PLACE (not yet implemented in MPIVIDE)
ALLOCATE( IRANKNXT_AIRCRAFT_TMP, MOLD = NRANKNXT_AIRCRAFT )
CALL MPI_ALLREDUCE( NRANKNXT_AIRCRAFT, IRANKNXT_AIRCRAFT_TMP, NAIRCRAFTS, MNHINT_MPI, MPI_MAX, NMNH_COMM_WORLD, IERR )
NRANKNXT_AIRCRAFT = IRANKNXT_AIRCRAFT_TMP
DEALLOCATE( IRANKNXT_AIRCRAFT_TMP )
#endif

DO JI = 1, NAIRCRAFTS
  IF ( NRANKNXT_AIRCRAFT(JI) /= NRANKCUR_AIRCRAFT(JI) ) THEN
    IF ( ISP == NRANKCUR_AIRCRAFT(JI) ) THEN
      CALL TAIRCRAFTS(JI)%TAIRCRAFT%SEND_DEALLOCATE( KTO = NRANKNXT_AIRCRAFT(JI), OSEND_SIZE_TO_RECEIVER = .TRUE. )
      DEALLOCATE( TAIRCRAFTS(JI)%TAIRCRAFT )
    ELSE IF ( ISP == NRANKNXT_AIRCRAFT(JI) ) THEN
      IF ( ASSOCIATED( TAIRCRAFTS(JI)%TAIRCRAFT ) ) &
        call Print_msg( NVERB_FATAL, 'GEN', 'AIRCRAFT_BALLOON', 'aircraft already associated' )
      ALLOCATE( TAIRCRAFTS(JI)%TAIRCRAFT )
      CALL TAIRCRAFTS(JI)%TAIRCRAFT%RECV_ALLOCATE( KFROM = NRANKCUR_AIRCRAFT(JI), ORECV_SIZE_FROM_OWNER = .TRUE. )
    END IF
  END IF
END DO

END SUBROUTINE AIRCRAFTS_MOVE_TO_NEW_RANKS
!----------------------------------------------------------------------------
!----------------------------------------------------------------------------
SUBROUTINE BALLOONS_INIT_POSITIONS()

USE MODD_IO,                         ONLY: ISP

USE MODE_AIRCRAFT_BALLOON_EVOL,      ONLY: FLYER_GET_RANK_MODEL_ISCRASHED

TYPE(TBALLOONDATA),  POINTER :: TZBALLOON

! Set next rank to 0 (necessary for MPI_ALLREDUCE)
NRANKNXT_BALLOON(:) = 0

IF ( ISP == NFLYER_DEFAULT_RANK ) THEN
  DO JI = 1, NBALLOONS
    IF ( .NOT. ASSOCIATED( TBALLOONS(JI)%TBALLOON ) ) &
      CALL PRINT_MSG( NVERB_FATAL, 'GEN', 'AIRCRAFT_BALLOON', 'balloon structure not associated' )

    TZBALLOON => TBALLOONS(JI)%TBALLOON

    ! Initialize model number (and rank)
    ! This is not done in initialisation phase because some data is not yet available at this early stage
    ! (XXHAT_ll of all models are needed by FIND_PROCESS_AND_MODEL_FROM_XY_POS)
    IF ( .NOT. TZBALLOON%LPOSITION_INIT ) THEN
      TZBALLOON%LPOSITION_INIT = .TRUE.
      ! Get rank of the process where the balloon is and the model number
      IF ( TZBALLOON%LFLY ) THEN
        ! In this case, we are in a restart and the balloon position was read in the restart file
        CALL FLYER_GET_RANK_MODEL_ISCRASHED( TZBALLOON )
      ELSE
        CALL FLYER_GET_RANK_MODEL_ISCRASHED( TZBALLOON, PX = TZBALLOON%XXLAUNCH, PY = TZBALLOON%XYLAUNCH )
      END IF
      IF ( TZBALLOON%LCRASH ) THEN
        CALL PRINT_MSG( NVERB_WARNING, 'GEN', 'AIRCRAFT_BALLOON', 'balloon ' // TRIM( TZBALLOON%CNAME ) &
                        // ': launch coordinates are outside of horizontal physical domain' )
      END IF
    ELSE
      CALL PRINT_MSG( NVERB_ERROR, 'GEN', 'AIRCRAFT_BALLOON', 'balloon ' // TRIM( TZBALLOON%CNAME ) &
                      // ': position has already been initialized' )
    END IF

    NRANKNXT_BALLOON(JI) = TZBALLOON%NRANK_CUR
  END DO
END IF

CALL BALLOONS_MOVE_TO_NEW_RANKS()

END SUBROUTINE BALLOONS_INIT_POSITIONS
!----------------------------------------------------------------------------
!----------------------------------------------------------------------------
SUBROUTINE BALLOONS_MOVE_TO_NEW_RANKS()

USE MODD_IO,                         ONLY: ISP
USE MODD_MPIF
USE MODD_PRECISION,                  ONLY: MNHINT_MPI
USE MODD_VAR_ll,                     ONLY: NMNH_COMM_WORLD

INTEGER :: IERR
INTEGER, DIMENSION(:), ALLOCATABLE :: IRANKNXT_BALLOON_TMP

#if 0
CALL MPI_ALLREDUCE( MPI_IN_PLACE, NRANKNXT_BALLOON, NBALLOONS, MNHINT_MPI, MPI_MAX, NMNH_COMM_WORLD, IERR )
#else
!Do this to not use MPI_IN_PLACE (not yet implemented in MPIVIDE)
ALLOCATE( IRANKNXT_BALLOON_TMP, MOLD = NRANKNXT_BALLOON )
CALL MPI_ALLREDUCE( NRANKNXT_BALLOON, IRANKNXT_BALLOON_TMP, NBALLOONS, MNHINT_MPI, MPI_MAX, NMNH_COMM_WORLD, IERR )
NRANKNXT_BALLOON = IRANKNXT_BALLOON_TMP
DEALLOCATE( IRANKNXT_BALLOON_TMP )
#endif

DO JI = 1, NBALLOONS
  IF ( NRANKNXT_BALLOON(JI) /= NRANKCUR_BALLOON(JI) ) THEN
    IF ( ISP == NRANKCUR_BALLOON(JI) ) THEN
      CALL TBALLOONS(JI)%TBALLOON%SEND_DEALLOCATE( KTO = NRANKNXT_BALLOON(JI), OSEND_SIZE_TO_RECEIVER = .TRUE. )
      DEALLOCATE( TBALLOONS(JI)%TBALLOON )
    ELSE IF ( ISP == NRANKNXT_BALLOON(JI) ) THEN
      ALLOCATE( TBALLOONS(JI)%TBALLOON )
      CALL TBALLOONS(JI)%TBALLOON%RECV_ALLOCATE( KFROM = NRANKCUR_BALLOON(JI), ORECV_SIZE_FROM_OWNER = .TRUE. )
    END IF
  END IF
END DO

END SUBROUTINE BALLOONS_MOVE_TO_NEW_RANKS
!----------------------------------------------------------------------------
END SUBROUTINE AIRCRAFT_BALLOON
!----------------------------------------------------------------------------
!----------------------------------------------------------------------------
SUBROUTINE AIRCRAFT_BALLOON_LONGTYPE_GET( TPFLYER, HLONGTYPE )
USE MODD_AIRCRAFT_BALLOON, ONLY: taircraftdata, tballoondata, TFLYERDATA

IMPLICIT NONE

CLASS(TFLYERDATA), INTENT(IN)  :: TPFLYER
CHARACTER(LEN=*),  INTENT(OUT) :: HLONGTYPE

character(len=:), allocatable :: ytype

select type ( tpflyer )
  class is ( taircraftdata )
    ytype = 'Aircrafts'

  class is ( tballoondata )
    if ( Trim( TPFLYER%CTYPE ) == 'RADIOS' ) then
      ytype = 'Radiosonde_balloons'
    else if ( Trim( TPFLYER%CTYPE ) == 'ISODEN' ) then
      ytype = 'Isodensity_balloons'
    else if ( Trim( TPFLYER%CTYPE ) == 'CVBALL' ) then
      ytype = 'Constant_volume_balloons'
    else
      call Print_msg( NVERB_ERROR, 'GEN', 'AIRCRAFT_BALLOON_LONGTYPE_GET', 'unknown category for flyer ' // Trim( tpflyer%cname ) )
      ytype = 'Unknown'
    end if

  class default
    call Print_msg( NVERB_ERROR, 'GEN', 'AIRCRAFT_BALLOON_LONGTYPE_GET', 'unknown class for flyer ' // Trim( tpflyer%cname ) )
    ytype = 'Unknown'

end select

if ( Len_trim( ytype ) > Len( HLONGTYPE ) ) &
  call Print_msg( NVERB_WARNING, 'GEN', 'AIRCRAFT_BALLOON_LONGTYPE_GET', &
                  'HLONGTYPE truncated for flyer ' // Trim( tpflyer%cname ) )
HLONGTYPE = Trim( ytype )

END SUBROUTINE AIRCRAFT_BALLOON_LONGTYPE_GET
!----------------------------------------------------------------------------
!----------------------------------------------------------------------------

END MODULE MODE_AIRCRAFT_BALLOON
