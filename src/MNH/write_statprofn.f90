!MNH_LIC Copyright 2002-2023 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!  Module to initiate the writing for stations and profilers
!  It was initially created to deduplicate code from the WRITE_STATION_n and WRITE_PROFILER_n subroutines.
! Author:
!  P. Wautelet 12/07/2023
!
!  Modifications
! --------------------------------------------------------------------------
!      #####################
MODULE MODE_WRITE_STATPROF_n
!      #####################

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: WRITE_STATPROF_n

contains
  !
  ! ##################################################
  SUBROUTINE WRITE_STATPROF_n( TPDIAFILE, TPSTATPROF )
  ! ##################################################
  !
  !
  !****  *WRITE_STATPROF_n* - write the stations or profilers records in the diachronic file
  !
  !*      0. DECLARATIONS
  !          ------------
  !
  USE MODD_IO,              ONLY: ISNPROC, ISP, TFILEDATA
  USE MODD_MPIF
  USE MODD_PRECISION,       ONLY: MNHINT_MPI
  USE MODD_PROFILER_n,      only: NUMBPROFILER_LOC, TPROFILERS, TPROFILERS_TIME
  USE MODD_STATION_n,       ONLY: NUMBSTAT_LOC, TSTATIONS, TSTATIONS_TIME
  USE MODD_TYPE_STATPROF,   ONLY: TSTATIONDATA, TSTATPROFDATA, TPROFILERDATA
  !
  USE MODE_MSG
  USE MODE_WRITE_PROFILER_n, ONLY: PROFILER_DIACHRO_n
  USE MODE_WRITE_STATION_n,  ONLY: STATION_DIACHRO_n
  !
  IMPLICIT NONE
  !
  !
  !*      0.1  declarations of arguments
  !
  TYPE(TFILEDATA),                    INTENT(IN) :: TPDIAFILE  ! Diachronic file to write
  CLASS(TSTATPROFDATA), DIMENSION(:), INTENT(IN) :: TPSTATPROF ! Stations/profilers to write
  !
  !-------------------------------------------------------------------------------
  !
  !       0.2  declaration of local variables
  !
  INTEGER :: IERR
  INTEGER :: JP, JS
  INTEGER :: IDX
  INTEGER :: INUMSTATPROF      ! Total number of stations/profilers (for the current model)
  INTEGER :: INUMSTATPROF_LOC  ! Number of local stations/profilers (for the current model)
  INTEGER :: ISTORE
  INTEGER, DIMENSION(:), ALLOCATABLE :: INSTATPROFPRC    ! Array to store the number of statprof per process (for the current model)
  INTEGER, DIMENSION(:), ALLOCATABLE :: ISTATPROFIDS     ! Intermediate array for MPI communication
  INTEGER, DIMENSION(:), ALLOCATABLE :: ISTATPROFPRCRANK ! Array to store the ranks of the processes where the statprof are
  INTEGER, DIMENSION(:), ALLOCATABLE :: IDS              ! Array to store the statprof number to send
  INTEGER, DIMENSION(:), ALLOCATABLE :: IDISP            ! Array to store the displacements for MPI communications
  CLASS(TSTATPROFDATA), ALLOCATABLE :: TZSTATPROF
  CLASS(TSTATPROFDATA), DIMENSION(:), POINTER :: TZSTATPROFLIST
    !
  !----------------------------------------------------------------------------

  SELECT TYPE ( TPSTATPROF )
    TYPE IS ( TPROFILERDATA )
      INUMSTATPROF_LOC = NUMBPROFILER_LOC
      ISTORE = SIZE( TPROFILERS_TIME%TPDATES )
      ALLOCATE( TPROFILERDATA :: TZSTATPROF )
      TZSTATPROFLIST => TPROFILERS

    TYPE IS ( TSTATIONDATA )
      INUMSTATPROF_LOC = NUMBSTAT_LOC
      ISTORE = SIZE( TSTATIONS_TIME%TPDATES )
      ALLOCATE( TSTATIONDATA :: TZSTATPROF )
      TZSTATPROFLIST => TSTATIONS

    CLASS DEFAULT
      CALL PRINT_MSG( NVERB_ERROR, 'IO', 'WRITE_STATPROF_n', 'unknown type' )
  END SELECT

  ALLOCATE( INSTATPROFPRC(ISNPROC) )
  ALLOCATE( IDS(INUMSTATPROF_LOC) )

  !Gather number of stations/profilers present on each process
  CALL MPI_ALLGATHER( INUMSTATPROF_LOC, 1, MNHINT_MPI, INSTATPROFPRC, 1, MNHINT_MPI, TPDIAFILE%NMPICOMM, IERR )

  !Store the identification number of local stations (these numbers are globals)
  DO JS = 1, INUMSTATPROF_LOC
    IDS(JS) = TPSTATPROF(JS)%NID
  END DO

  ALLOCATE( IDISP(ISNPROC) )
  IDISP(1) = 0
  DO JP = 2, ISNPROC
    IDISP(JP) = IDISP(JP-1) + INSTATPROFPRC(JP-1)
  END DO

  INUMSTATPROF = SUM( INSTATPROFPRC(:) )
  ALLOCATE( ISTATPROFIDS(INUMSTATPROF) )
  ALLOCATE( ISTATPROFPRCRANK(INUMSTATPROF) )

  !Gather the list of all the stations of all processes
  CALL MPI_ALLGATHERV( IDS(:), INUMSTATPROF_LOC, MNHINT_MPI, ISTATPROFIDS(:), INSTATPROFPRC(:), &
                       IDISP(:), MNHINT_MPI, TPDIAFILE%NMPICOMM, IERR )

  !Store the rank of each process corresponding to a given station/profiler
  IDX = 1
  ISTATPROFPRCRANK(:) = -1
  DO JP = 1, ISNPROC
    DO JS = 1, INSTATPROFPRC(JP)
      ISTATPROFPRCRANK(ISTATPROFIDS(IDX)) = JP
      IDX = IDX + 1
    END DO
  END DO

  CALL TZSTATPROF%DATA_ARRAYS_ALLOCATE( ISTORE )

  IDX = 1

  STATPROF: DO JS = 1, INUMSTATPROF
    IF ( ISTATPROFPRCRANK(JS) == TPDIAFILE%NMASTER_RANK ) THEN
      !No communication necessary, the station data is already on the writer process
      IF ( ISP == TPDIAFILE%NMASTER_RANK ) THEN
        TZSTATPROF = TZSTATPROFLIST(IDX)
        IDX = IDX + 1
      END IF
    ELSE
      !The station data is not on the writer process
      IF ( ISP == ISTATPROFPRCRANK(JS) ) THEN
        ! This process has the data and needs to send it to the writer process
        CALL TZSTATPROFLIST(IDX)%SEND( KTO = TPDIAFILE%NMASTER_RANK, OSEND_SIZE_TO_RECEIVER = .FALSE. )

        IDX = IDX + 1
      ELSE IF ( ISP == TPDIAFILE%NMASTER_RANK ) THEN
        ! This process is the writer and will receive the station data from its owner
        ! Remark: allocation is already done and will be skipped in RECV_ALLOCATE
        CALL TZSTATPROF%RECV_ALLOCATE( KFROM = ISTATPROFPRCRANK(JS), KSTORE_CUR = ISTORE, KSTORE_MAX = ISTORE )
      END IF
    END IF

    SELECT TYPE ( TZSTATPROF )
      TYPE IS ( TPROFILERDATA )
        CALL PROFILER_DIACHRO_n( TPDIAFILE, TZSTATPROF )

      TYPE IS ( TSTATIONDATA )
        CALL STATION_DIACHRO_n( TPDIAFILE, TZSTATPROF )

      CLASS DEFAULT
        CALL PRINT_MSG( NVERB_ERROR, 'IO', 'WRITE_STATPROF_n', 'unknown type' )
    END SELECT

  END DO STATPROF

  ! Deallocate arrays (if still allocated)
  IF ( TZSTATPROF%NSTORE_MAX >= 0 ) CALL TZSTATPROF%DATA_ARRAYS_DEALLOCATE( )

  END SUBROUTINE WRITE_STATPROF_n

END MODULE MODE_WRITE_STATPROF_N
