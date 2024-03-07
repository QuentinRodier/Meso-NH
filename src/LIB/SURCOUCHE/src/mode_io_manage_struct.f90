!MNH_LIC Copyright 2016-2024 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
! Author(s)
!  P. Wautelet 2016
! Modifications:
!  P. Wautelet 05/2016-04/2018: new data structures and calls for I/O
!  P. Wautelet 21/01/2019: add LIO_ALLOW_NO_BACKUP and LIO_NO_WRITE to modd_io_ll
!                          to allow to disable writes (for bench purposes)
!  P. Wautelet 06/02/2019: simplify OPEN_ll and do somme assignments at a more logical place
!  P. Wautelet 07/02/2019: force TYPE to a known value for IO_File_add2list
!  P. Wautelet 07/02/2019: remove OPARALLELIO argument from open and close files subroutines
!                          (nsubfiles_ioz is now determined in IO_File_add2list)
!  P. Wautelet 18/02/2019: bugfixes for nsubfiles_ioz
!  P. Wautelet 05/03/2019: rename IO subroutines and modules
!  P. Wautelet 12/03/2019: add TMAINFILE field in TFILEDATA
!  P. Wautelet 11/02/2020: bugfix: TDADFILE was wrongly constructed for output files
!  S. Donnier  28/02/2020: type STREAM needed for use of ECOCLIMAP SG
!  P. Wautelet 08/01/2021: allow output files with empty variable list (useful if IO_Field_user_write is not empty)
!  P. Wautelet 18/03/2022: minor bugfix in ISTEP_MAX computation + adapt diagnostics messages
!                          (change verbosity level and remove some unnecessary warnings)
!  P. Wautelet 13/01/2023: set NMODEL field for backup and output files
!  P. Wautelet 14/12/2023: add lossy compression for output files
!  P. Wautelet 17/01/2024: add IO_File_remove_from_list subroutine
!  P. Wautelet 02/02/2024: restructure backups/outputs lists
!  P. Wautelet 07/02/2024: add compression for all netCDF files
!-----------------------------------------------------------------
MODULE MODE_IO_MANAGE_STRUCT
!
USE MODD_IO
use modd_precision, only: LFIINT
!
USE MODE_MSG
!
IMPLICIT NONE
!
private
!
public :: IO_Bakout_struct_prepare, IO_File_find_byname, IO_Filelist_print
public :: IO_File_add2list, IO_File_remove_from_list
public :: IO_Is_backup_time, IO_Is_output_time, IO_BakOut_file_create
!
! Integers for file stats
INTEGER, SAVE :: NFILE_STAT_NADD    = 0 ! Number of files added to file list
INTEGER, SAVE :: NFILE_STAT_NREM    = 0 ! Number of files removed from file list
INTEGER, SAVE :: NFILE_STAT_CURSIZE = 0 ! Current number of files in file list
INTEGER, SAVE :: NFILE_STAT_MAXSIZE = 0 ! Maximum number of files in file list
!
CONTAINS
!
!###################################################
SUBROUTINE IO_Bakout_struct_prepare( KSUP, PSEGLEN )
!###################################################
!
USE MODD_BAKOUT
USE MODD_CONF,        ONLY: NMODEL
USE MODD_DYN,         ONLY: XSEGLEN
USE MODD_DYN_n,       ONLY: DYN_MODEL
USE MODD_FIELD,       ONLY: TFIELDLIST
USE MODD_NESTING,     ONLY: NDAD
USE MODD_SUB_MODEL_N, ONLY: NFILE_BACKUP_CURRENT, NFILE_OUTPUT_CURRENT, SUB_MODEL_MODEL
USE MODD_OUT_n,       ONLY: OUT_GOTO_MODEL, OUT_MODEL
USE MODD_VAR_ll,      ONLY: IP

use mode_field, only: Find_field_id_from_mnhname

USE MODN_BACKUP, ONLY: BACKUP_NML_DEALLOCATE
USE MODN_OUTPUT, ONLY: OUTPUT_NML_DEALLOCATE

IMPLICIT NONE
!
INTEGER, INTENT(IN) :: KSUP    ! supp. time steps
REAL,    INTENT(IN) :: PSEGLEN ! segment duration (in seconds)
!
INTEGER           :: IBAK_NUM
INTEGER           :: IOUT_NUM
INTEGER           :: IMI              ! Model number for loop
INTEGER           :: IERR_LVL         ! Level of error message
INTEGER           :: IVAR             ! Number of variables
INTEGER           :: ISTEP
INTEGER           :: ISTEP_MAX        ! Number of timesteps
INTEGER           :: IPOS,IFIELD      ! Indices
INTEGER           :: JOUT,IDX         ! Loop indices
INTEGER           :: IRESP
INTEGER           :: ISTEPDADFIRST
INTEGER, DIMENSION(:,:), ALLOCATABLE :: IBAK_STEPLIST
INTEGER, DIMENSION(:,:), ALLOCATABLE :: IOUT_STEPLIST
! Arrays to store list of backup/output steps (intermediate array)
REAL              :: ZTSTEP_RND
!
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Bakout_struct_prepare','called')
!
! Special case if writes are forced to NO
IF (LIO_NO_WRITE) THEN

  DO IMI = 1, NMODEL
    OUT_MODEL(IMI)%NBAK_NUMB = 0
    OUT_MODEL(IMI)%NOUT_NUMB = 0
  END DO
  RETURN
END IF

! Copy NBAK_STEP into IBAK_STEPLIST. All backup steps will be stored in it (except regular series)
ALLOCATE( IBAK_STEPLIST, SOURCE=NBAK_STEP )
ALLOCATE( IOUT_STEPLIST, SOURCE=NOUT_STEP )

! Treat regular series for all models before next loop on models
! This is necessary to have a first version of them before synchronizing them to the nested submodels
DO IMI = 1, NMODEL
  ! Backup regular series is provided with intervals in seconds
  IF ( XBAK_TIME_FREQ(IMI) > 0. ) THEN
    IF ( NBAK_STEP_FREQ(IMI) > 0 )                                   &
      CALL PRINT_MSG( NVERB_ERROR, 'IO', 'IO_Bakout_struct_prepare', &
                      'XBAK_TIME_FREQ and NBAK_STEP_FREQ can not be provided simultaneously' )

    ! Check that frequency is at least equals to the model time step
    IF ( XBAK_TIME_FREQ(IMI) < DYN_MODEL(IMI)%XTSTEP - 1.E-6 ) THEN
      CALL PRINT_MSG( NVERB_ERROR, 'IO', 'IO_Bakout_struct_prepare', 'XBAK_TIME_FREQ smaller than model timestep' )
      XBAK_TIME_FREQ(IMI) = DYN_MODEL(IMI)%XTSTEP
    END IF

    ! Check that the frequency is a multiple of the model time step
    ZTSTEP_RND = NINT( XBAK_TIME_FREQ(IMI) / DYN_MODEL(IMI)%XTSTEP ) * DYN_MODEL(IMI)%XTSTEP
    IF ( ABS( ZTSTEP_RND - XBAK_TIME_FREQ(IMI) ) > 1.E-6 ) THEN
      CALL PRINT_MSG( NVERB_ERROR, 'IO', 'IO_Bakout_struct_prepare', 'XBAK_TIME_FREQ is not a multiple of the model timestep' )
      XBAK_TIME_FREQ(IMI) = ZTSTEP_RND
    END IF

    IF ( XBAK_TIME_FREQ_FIRST(IMI) > 0. ) THEN
      ! Check that the first write time of the series is a multiple of the model time step
      ZTSTEP_RND = NINT( XBAK_TIME_FREQ_FIRST(IMI) / DYN_MODEL(IMI)%XTSTEP ) * DYN_MODEL(IMI)%XTSTEP
      IF ( ABS( ZTSTEP_RND - XBAK_TIME_FREQ_FIRST(IMI) ) > 1.E-6 ) THEN
        CALL PRINT_MSG( NVERB_ERROR, 'IO', 'IO_Bakout_struct_prepare', &
                        'XBAK_TIME_FREQ_FIRST is not a multiple of the model timestep' )
        XBAK_TIME_FREQ_FIRST(IMI) = ZTSTEP_RND
      END IF
    END IF

    ! Check that the first write time of the series is not after the end of the segment
    IF ( XBAK_TIME_FREQ_FIRST(IMI) > XSEGLEN + 1.E-6 ) THEN
      CALL PRINT_MSG( NVERB_ERROR, 'IO', 'IO_Bakout_struct_prepare', &
                      'XBAK_TIME_FREQ_FIRST is after the end of the simulation segment' )
    END IF

    ! Do not mix NBAK_STEP_FREQ_FIRST with XBAK_TIME_FREQ
    IF ( NBAK_STEP_FREQ_FIRST(IMI) > 0 ) THEN
      CMNHMSG(1) = 'NBAK_STEP_FREQ_FIRST is not allowed with XBAK_TIME_FREQ'
      CMNHMSG(2) = 'use XBAK_TIME_FREQ_FIRST instead'
      CALL PRINT_MSG( NVERB_ERROR, 'IO', 'IO_Bakout_struct_prepare' )
    END IF

    ! Set the backup frequency in timesteps
    OUT_MODEL(IMI)%NBAK_STEPFREQ         = NINT( XBAK_TIME_FREQ(IMI)       / DYN_MODEL(IMI)%XTSTEP )

    IF ( XBAK_TIME_FREQ_FIRST(IMI) > 0. ) THEN
      OUT_MODEL(IMI)%NBAK_STEPFREQFIRST = NINT( XBAK_TIME_FREQ_FIRST(IMI) / DYN_MODEL(IMI)%XTSTEP ) + 1
    ELSE
      ! Set first backup to frequency
      OUT_MODEL(IMI)%NBAK_STEPFREQFIRST = OUT_MODEL(IMI)%NBAK_STEPFREQ + 1
    END IF
  END IF

  ! Backup regular series is provided with intervals in timesteps
  IF ( NBAK_STEP_FREQ(IMI) > 0 ) THEN
    OUT_MODEL(IMI)%NBAK_STEPFREQ = NBAK_STEP_FREQ(IMI)

    ! Do not mix XBAK_TIME_FREQ_FIRST with NBAK_STEP_FREQ
    IF ( XBAK_TIME_FREQ_FIRST(IMI) > 0 ) THEN
      CMNHMSG(1) = 'XBAK_TIME_FREQ_FIRST is not allowed with NBAK_STEP_FREQ'
      CMNHMSG(2) = 'use NBAK_STEP_FREQ_FIRST instead'
      CALL PRINT_MSG( NVERB_ERROR, 'IO', 'IO_Bakout_struct_prepare' )
    END IF

    IF ( NBAK_STEP_FREQ_FIRST(IMI) > 0 ) THEN
      OUT_MODEL(IMI)%NBAK_STEPFREQFIRST = NBAK_STEP_FREQ_FIRST(IMI)
    ELSE
      ! Set first backup to frequency
      OUT_MODEL(IMI)%NBAK_STEPFREQFIRST = OUT_MODEL(IMI)%NBAK_STEPFREQ + 1
    END IF
  END IF

  ! Output regular series is provided with intervals in seconds
  IF ( XOUT_TIME_FREQ(IMI) > 0. ) THEN
    IF ( NOUT_STEP_FREQ(IMI) > 0 )                                   &
      CALL PRINT_MSG( NVERB_ERROR, 'IO', 'IO_Bakout_struct_prepare', &
                      'XOUT_TIME_FREQ and NOUT_STEP_FREQ can not be provided simultaneously' )

    ! Check that frequency is at least equals to the model time step
    IF ( XOUT_TIME_FREQ(IMI) < DYN_MODEL(IMI)%XTSTEP - 1.E-6 ) THEN
      CALL PRINT_MSG( NVERB_ERROR, 'IO', 'IO_Bakout_struct_prepare', 'XOUT_TIME_FREQ smaller than model timestep' )
      XOUT_TIME_FREQ(IMI) = DYN_MODEL(IMI)%XTSTEP
    END IF

    ! Check that the frequency is a multiple of the model time step
    ZTSTEP_RND = NINT( XOUT_TIME_FREQ(IMI) / DYN_MODEL(IMI)%XTSTEP ) * DYN_MODEL(IMI)%XTSTEP
    IF ( ABS( ZTSTEP_RND - XOUT_TIME_FREQ(IMI) ) > 1.E-6 ) THEN
      CALL PRINT_MSG( NVERB_ERROR, 'IO', 'IO_Bakout_struct_prepare', 'XOUT_TIME_FREQ is not a multiple of the model timestep' )
      XOUT_TIME_FREQ(IMI) = ZTSTEP_RND
    END IF

    IF ( XOUT_TIME_FREQ_FIRST(IMI) > 0. ) THEN
      ! Check that the first write time of the series is a multiple of the model time step
      ZTSTEP_RND = NINT( XOUT_TIME_FREQ_FIRST(IMI) / DYN_MODEL(IMI)%XTSTEP ) * DYN_MODEL(IMI)%XTSTEP
      IF ( ABS( ZTSTEP_RND - XOUT_TIME_FREQ_FIRST(IMI) ) > 1.E-6 ) THEN
        CALL PRINT_MSG( NVERB_ERROR, 'IO', 'IO_Bakout_struct_prepare', &
                        'XOUT_TIME_FREQ_FIRST is not a multiple of the model timestep' )
        XOUT_TIME_FREQ_FIRST(IMI) = ZTSTEP_RND
      END IF
    END IF

    ! Check that the first write time of the series is not after the end of the segment
    IF ( XOUT_TIME_FREQ_FIRST(IMI) > XSEGLEN + 1.E-6 ) THEN
      CALL PRINT_MSG( NVERB_ERROR, 'IO', 'IO_Bakout_struct_prepare', &
                      'XOUT_TIME_FREQ_FIRST is after the end of the simulation segment' )
    END IF

    ! Do not mix NOUT_STEP_FREQ_FIRST with XOUT_TIME_FREQ
    IF ( NOUT_STEP_FREQ_FIRST(IMI) > 0 ) THEN
      CMNHMSG(1) = 'NOUT_STEP_FREQ_FIRST is not allowed with XOUT_TIME_FREQ'
      CMNHMSG(2) = 'use XOUT_TIME_FREQ_FIRST instead'
      CALL PRINT_MSG( NVERB_ERROR, 'IO', 'IO_Bakout_struct_prepare' )
    END IF

    ! Set the output frequency in timesteps
    OUT_MODEL(IMI)%NOUT_STEPFREQ         = NINT( XOUT_TIME_FREQ(IMI)       / DYN_MODEL(IMI)%XTSTEP )

    IF ( XOUT_TIME_FREQ_FIRST(IMI) > 0. ) THEN
      OUT_MODEL(IMI)%NOUT_STEPFREQFIRST = NINT( XOUT_TIME_FREQ_FIRST(IMI) / DYN_MODEL(IMI)%XTSTEP ) + 1
    ELSE
      ! Set first output to frequency
      OUT_MODEL(IMI)%NOUT_STEPFREQFIRST = OUT_MODEL(IMI)%NOUT_STEPFREQ + 1
    END IF
  END IF

  ! Backup regular series is provided with intervals in timesteps
  IF ( NOUT_STEP_FREQ(IMI) > 0 ) THEN
    OUT_MODEL(IMI)%NOUT_STEPFREQ = NOUT_STEP_FREQ(IMI)

    ! Do not mix XOUT_TIME_FREQ_FIRST with NOUT_STEP_FREQ
    IF ( XOUT_TIME_FREQ_FIRST(IMI) > 0 ) THEN
      CMNHMSG(1) = 'XOUT_TIME_FREQ_FIRST is not allowed with NOUT_STEP_FREQ'
      CMNHMSG(2) = 'use NOUT_STEP_FREQ_FIRST instead'
      CALL PRINT_MSG( NVERB_ERROR, 'IO', 'IO_Bakout_struct_prepare' )
    END IF

    IF ( NOUT_STEP_FREQ_FIRST(IMI) > 0 ) THEN
      OUT_MODEL(IMI)%NOUT_STEPFREQFIRST = NOUT_STEP_FREQ_FIRST(IMI)
    ELSE
      ! Set first output to frequency
      OUT_MODEL(IMI)%NOUT_STEPFREQFIRST = OUT_MODEL(IMI)%NOUT_STEPFREQ + 1
    END IF
  END IF
END DO

! Synchronize regular series to nested models
DO IMI = 1, NMODEL
  ! Synchronize regular backup series to nested models
  IF ( OUT_MODEL(IMI)%NBAK_STEPFREQ > 0 ) THEN
    DO IDX = IMI+1, NMODEL
      ISTEP = OUT_MODEL(IDX-1)%NBAK_STEPFREQ * NINT( DYN_MODEL(IDX-1)%XTSTEP / DYN_MODEL(IDX)%XTSTEP )
      IF ( OUT_MODEL(IDX)%NBAK_STEPFREQ > 0 ) THEN
        IF ( MOD(ISTEP,OUT_MODEL(IDX)%NBAK_STEPFREQ) /= 0 ) THEN
          CALL PRINT_MSG( NVERB_ERROR, 'IO', 'IO_Bakout_struct_prepare', &
                          'backup frequency for parent model must be a multiple of child' )
          OUT_MODEL(IDX)%NBAK_STEPFREQ = ISTEP
        ELSE
          ! Nothing to do. We keep the model frequency
        END IF
      ELSE
        ! Propagate to child
        OUT_MODEL(IDX)%NBAK_STEPFREQ = ISTEP
      END IF

      IF ( OUT_MODEL(IDX)%NBAK_STEPFREQFIRST > 0 ) THEN
        IF ( OUT_MODEL(IDX-1)%NBAK_STEPFREQFIRST > 0 ) THEN
          ! Compute first step of dad in number of timesteps for THIS model
          ISTEPDADFIRST = ( OUT_MODEL(IDX-1)%NBAK_STEPFREQFIRST - 1 ) * NINT( DYN_MODEL(IDX-1)%XTSTEP / DYN_MODEL(IDX)%XTSTEP ) + 1
          ! The first backup of a child model must be before or at the same time than its parent
          IF ( OUT_MODEL(IDX)%NBAK_STEPFREQFIRST > ISTEPDADFIRST ) THEN
            CALL PRINT_MSG( NVERB_ERROR, 'IO', 'IO_Bakout_struct_prepare', &
                            'the first backup of a child model must be before or at the same time than its parent' )
            OUT_MODEL(IDX)%NBAK_STEPFREQFIRST = ISTEPDADFIRST
          END IF
          ! The backup times must be aligned with the one of the parent model (if it does also regular series)
          IF ( MOD( ISTEPDADFIRST - OUT_MODEL(IDX)%NBAK_STEPFREQFIRST, OUT_MODEL(IDX)%NBAK_STEPFREQ ) /= 0 ) THEN
            CMNHMSG(1) = 'times of series of backups must be aligned with the time of its parent'
            CMNHMSG(2) = 'check that XBAK_TIME_FREQ_FIRST or NBAK_STEP_FREQ_FIRST are set correctly for all submodels'
            CALL PRINT_MSG( NVERB_ERROR, 'IO', 'IO_Bakout_struct_prepare' )
            OUT_MODEL(IDX)%NBAK_STEPFREQFIRST = ( OUT_MODEL(IDX-1)%NBAK_STEPFREQFIRST - 1 ) &
                                                * NINT( DYN_MODEL(IDX-1)%XTSTEP / DYN_MODEL(IDX)%XTSTEP ) + 1
          END IF
        ELSE
          ! Nothing to do (the parent does not do regular series)
        END IF
      ELSE
        ! Propagate first time (in timesteps)
        OUT_MODEL(IDX)%NBAK_STEPFREQFIRST = ( OUT_MODEL(IDX-1)%NBAK_STEPFREQFIRST - 1 ) &
                                            * NINT( DYN_MODEL(IDX-1)%XTSTEP / DYN_MODEL(IDX)%XTSTEP ) + 1
      END IF
    END DO
  END IF

  ! Synchronize regular output series to nested models
  IF ( OUT_MODEL(IMI)%NOUT_STEPFREQ > 0 ) THEN
    DO IDX = IMI+1, NMODEL
      ISTEP = OUT_MODEL(IDX-1)%NOUT_STEPFREQ * NINT( DYN_MODEL(IDX-1)%XTSTEP / DYN_MODEL(IDX)%XTSTEP )
      IF ( OUT_MODEL(IDX)%NOUT_STEPFREQ > 0 ) THEN
        IF ( MOD(ISTEP,OUT_MODEL(IDX)%NOUT_STEPFREQ) /= 0 ) THEN
          CALL PRINT_MSG( NVERB_ERROR, 'IO', 'IO_Bakout_struct_prepare', &
                          'output frequency for parent model must be a multiple of child' )
          OUT_MODEL(IDX)%NOUT_STEPFREQ = ISTEP
        ELSE
          ! Nothing to do. We keep the model frequency
        END IF
      ELSE
        ! Propagate to child
        OUT_MODEL(IDX)%NOUT_STEPFREQ = ISTEP
      END IF

      IF ( OUT_MODEL(IDX)%NOUT_STEPFREQFIRST > 0 ) THEN
        IF ( OUT_MODEL(IDX-1)%NOUT_STEPFREQFIRST > 0 ) THEN
          ! Compute first step of dad in number of timesteps for THIS model
          ISTEPDADFIRST = ( OUT_MODEL(IDX-1)%NOUT_STEPFREQFIRST - 1 ) * NINT( DYN_MODEL(IDX-1)%XTSTEP / DYN_MODEL(IDX)%XTSTEP ) + 1
          ! The first output of a child model must be before or at the same time than its parent
          IF ( OUT_MODEL(IDX)%NOUT_STEPFREQFIRST > ISTEPDADFIRST ) THEN
            CALL PRINT_MSG( NVERB_ERROR, 'IO', 'IO_Bakout_struct_prepare', &
                            'the first output of a child model must be before or at the same time than its parent' )
            OUT_MODEL(IDX)%NOUT_STEPFREQFIRST = ISTEPDADFIRST
          END IF
          ! The output times must be aligned with the one of the parent model (if it does also regular series)
          IF ( MOD( ISTEPDADFIRST - OUT_MODEL(IDX)%NOUT_STEPFREQFIRST, OUT_MODEL(IDX)%NOUT_STEPFREQ ) /= 0 ) THEN
            CMNHMSG(1) = 'times of series of outputs must be aligned with the time of its parent'
            CMNHMSG(2) = 'check that XOUT_TIME_FREQ_FIRST or NOUT_STEP_FREQ_FIRST are set correctly for all submodels'
            CALL PRINT_MSG( NVERB_ERROR, 'IO', 'IO_Bakout_struct_prepare' )
            OUT_MODEL(IDX)%NOUT_STEPFREQFIRST = ( OUT_MODEL(IDX-1)%NOUT_STEPFREQFIRST - 1 ) &
                                                * NINT( DYN_MODEL(IDX-1)%XTSTEP / DYN_MODEL(IDX)%XTSTEP ) + 1
          END IF
        ELSE
          ! Nothing to do (the parent does not do regular series)
        END IF
      ELSE
        ! Propagate first time (in timesteps)
        OUT_MODEL(IDX)%NOUT_STEPFREQFIRST = ( OUT_MODEL(IDX-1)%NOUT_STEPFREQFIRST - 1 ) &
                                            * NINT( DYN_MODEL(IDX-1)%XTSTEP / DYN_MODEL(IDX)%XTSTEP ) + 1
      END IF
    END DO
  END IF
END DO

! Treat irregular backups/outputs
DO IMI = 1, NMODEL
  IBAK_NUM = 0
  IOUT_NUM = 0

  !Reduce XSEGLEN by time added to XSEGLEN for 1st domain (see set_grid subroutine)
  ISTEP_MAX = NINT( ( XSEGLEN - KSUP * DYN_MODEL(1)%XTSTEP ) / DYN_MODEL(IMI)%XTSTEP ) + 1

  ! Check that provided times are multiples of model timestep and not after end of segment
  ! After that, insert them in the lists (in timesteps instead of seconds)
  DO JOUT = 1, NFILE_NUM_MAX
    IF ( XBAK_TIME(IMI,JOUT) >= 0.) THEN
      ZTSTEP_RND = NINT( XBAK_TIME(IMI,JOUT) / DYN_MODEL(IMI)%XTSTEP ) * DYN_MODEL(IMI)%XTSTEP
      IF ( ABS( ZTSTEP_RND - XBAK_TIME(IMI,JOUT) ) > 1.E-6 ) THEN
        CALL PRINT_MSG( NVERB_ERROR, 'IO', 'IO_Bakout_struct_prepare', 'XBAK_TIME is not a multiple of the model timestep' )
        XBAK_TIME(IMI,JOUT) = ZTSTEP_RND
      END IF
      IF ( XBAK_TIME(IMI,JOUT) > XSEGLEN + 1.E-6 ) THEN
        WRITE( CMNHMSG(1), '( "XBAK_TIME ", EN12.3 , " after end of simulation time segment => ignored" )' ) XBAK_TIME(IMI,JOUT)
        CALL PRINT_MSG( NVERB_WARNING, 'IO', 'IO_Bakout_struct_prepare' )
        XBAK_TIME(IMI,JOUT) = XNEGUNDEF
      END IF

      IF ( XBAK_TIME(IMI,JOUT) > 0.) THEN ! Check again because its value could have been modified just before if ignored
        ! Insert XBAK_TIME into IBAK_STEPLIST after conversion in timestep number (use insert because the list may be non-empty)
        IBAK_NUM = IBAK_NUM + 1
        CALL IO_INSERT_INT( IBAK_NUM, IBAK_STEPLIST(IMI,:), NINT( XBAK_TIME(IMI,JOUT) / DYN_MODEL(IMI)%XTSTEP ) + 1 )
      END IF
    END IF

    IF ( XOUT_TIME(IMI,JOUT) >= 0.) THEN
      ZTSTEP_RND = NINT( XOUT_TIME(IMI,JOUT) / DYN_MODEL(IMI)%XTSTEP ) * DYN_MODEL(IMI)%XTSTEP
      IF ( ABS( ZTSTEP_RND - XOUT_TIME(IMI,JOUT) ) > 1.E-6 ) THEN
        CALL PRINT_MSG( NVERB_ERROR, 'IO', 'IO_Bakout_struct_prepare', 'XOUT_TIME is not a multiple of the model timestep' )
        XOUT_TIME(IMI,JOUT) = ZTSTEP_RND
      END IF
      IF ( XOUT_TIME(IMI,JOUT) > XSEGLEN + 1.E-6 ) THEN
        WRITE( CMNHMSG(1), '( "XOUT_TIME ", EN12.3 , " after end of simulation time segment => ignored" )' ) XOUT_TIME(IMI,JOUT)
        CALL PRINT_MSG( NVERB_WARNING, 'IO', 'IO_Bakout_struct_prepare' )
        XOUT_TIME(IMI,JOUT) = XNEGUNDEF
      END IF

      IF ( XOUT_TIME(IMI,JOUT) > 0.) THEN ! Check again because its value could have been modified just before if ignored
        ! Insert XOUT_TIME into IOUT_STEPLIST after conversion in timestep number (use insert because the list may be non-empty)
        IOUT_NUM = IOUT_NUM + 1
        CALL IO_INSERT_INT( IOUT_NUM, IOUT_STEPLIST(IMI,:), NINT( XOUT_TIME(IMI,JOUT) / DYN_MODEL(IMI)%XTSTEP ) + 1 )
      END IF
    END IF
  END DO
  !
  !* Synchronization between nested models through IBAK_STEPLIST/IOUT_STEPLIST arrays
  !
  CALL IO_SYNC_MODELS_INT(IBAK_NUM,IBAK_STEPLIST(:,:))
  CALL IO_SYNC_MODELS_INT(IOUT_NUM,IOUT_STEPLIST(:,:))

  IF ( LBAK_BEG ) THEN
    IBAK_NUM = IBAK_NUM + 1
    CALL IO_INSERT_INT( IBAK_NUM, IBAK_STEPLIST(IMI,:), 1 ) ! 1 is the 1st step number
  END IF
  IF ( LOUT_BEG ) THEN
    IOUT_NUM = IOUT_NUM + 1
    CALL IO_INSERT_INT( IOUT_NUM, IOUT_STEPLIST(IMI,:), 1 ) ! 1 is the 1st step number
  END IF

  IF ( LBAK_END ) THEN
    IBAK_NUM = IBAK_NUM + 1
    CALL IO_INSERT_INT( IBAK_NUM, IBAK_STEPLIST(IMI,:), ISTEP_MAX )
  END IF
  IF ( LOUT_END ) THEN
    IOUT_NUM = IOUT_NUM + 1
    CALL IO_INSERT_INT( IOUT_NUM, IOUT_STEPLIST(IMI,:), ISTEP_MAX )
  END IF
  !
  !* Find and remove duplicated entries
  !
  CALL FIND_REMOVE_DUPLICATES( IBAK_NUM, IBAK_STEPLIST(IMI,:) )
  CALL FIND_REMOVE_DUPLICATES( IOUT_NUM, IOUT_STEPLIST(IMI,:) )
  !
  !* Find and remove out of time range entries
  !
  CALL FIND_REMOVE_OUTOFTIMERANGE( IBAK_NUM, IBAK_STEPLIST(IMI,:) )
  CALL FIND_REMOVE_OUTOFTIMERANGE( IOUT_NUM, IOUT_STEPLIST(IMI,:) )

  ! Remove entries in list if they are at the same time than regular entries
  DO JOUT = OUT_MODEL(IMI)%NBAK_STEPFREQFIRST, ISTEP_MAX, OUT_MODEL(IMI)%NBAK_STEPFREQ
    DO IDX = 1, IBAK_NUM
      IF ( IBAK_STEPLIST(IMI,IDX) == JOUT ) THEN
        CALL PRINT_MSG(NVERB_DEBUG,'IO','FIND_REMOVE_REGULAR','found duplicated backup step (removed extra one)')
        IBAK_STEPLIST(IMI,IDX) = NNEGUNDEF
      END IF
    END DO
  END DO
  DO JOUT = OUT_MODEL(IMI)%NOUT_STEPFREQFIRST, ISTEP_MAX, OUT_MODEL(IMI)%NOUT_STEPFREQ
    DO IDX = 1, IOUT_NUM
      IF ( IOUT_STEPLIST(IMI,IDX) == JOUT ) THEN
        CALL PRINT_MSG(NVERB_DEBUG,'IO','FIND_REMOVE_REGULAR','found duplicated output step (removed extra one)')
        IOUT_STEPLIST(IMI,IDX) = NNEGUNDEF
      END IF
    END DO
  END DO
  !
  !* Sort entries
  !
  CALL SORT_ENTRIES( IBAK_NUM, IBAK_STEPLIST(IMI,:) )
  CALL SORT_ENTRIES( IOUT_NUM, IOUT_STEPLIST(IMI,:) )
  !
  !* Count the number of backups/outputs of model IMI and compact the list
  !
  IBAK_NUM = 0
  DO JOUT = 1, SIZE( IBAK_STEPLIST(IMI,:) )
    IF ( IBAK_STEPLIST(IMI,JOUT) >= 0 ) THEN
      IBAK_NUM = IBAK_NUM + 1
    END IF
  END DO
  ALLOCATE( OUT_MODEL(IMI)%NBAK_STEPLIST(IBAK_NUM) )
  OUT_MODEL(IMI)%NBAK_STEPLIST(:) = IBAK_STEPLIST(IMI,1:IBAK_NUM)
  OUT_MODEL(IMI)%NBAK_NUMB = IBAK_NUM

  IOUT_NUM = 0
  DO JOUT = 1, SIZE( IOUT_STEPLIST(IMI,:) )
    IF ( IOUT_STEPLIST(IMI,JOUT) >= 0 ) THEN
      IOUT_NUM = IOUT_NUM + 1
    END IF
  END DO
  ALLOCATE( OUT_MODEL(IMI)%NOUT_STEPLIST(IOUT_NUM) )
  OUT_MODEL(IMI)%NOUT_STEPLIST(:) = IOUT_STEPLIST(IMI,1:IOUT_NUM)
  OUT_MODEL(IMI)%NOUT_NUMB = IOUT_NUM

  ! Count the number of regular backups/outputs
  IBAK_NUM = MAX( ( ISTEP_MAX - OUT_MODEL(IMI)%NBAK_STEPFREQFIRST ) / OUT_MODEL(IMI)%NBAK_STEPFREQ + 1, 0 )
  OUT_MODEL(IMI)%NBAK_NUMB = OUT_MODEL(IMI)%NBAK_NUMB + IBAK_NUM

  IOUT_NUM = MAX( ( ISTEP_MAX - OUT_MODEL(IMI)%NOUT_STEPFREQFIRST ) / OUT_MODEL(IMI)%NOUT_STEPFREQ + 1, 0 )
  OUT_MODEL(IMI)%NOUT_NUMB = OUT_MODEL(IMI)%NOUT_NUMB + IOUT_NUM

  ! Print message if there are no backups
  IF ( OUT_MODEL(IMI)%NBAK_NUMB == 0 ) THEN
    IF( LIO_ALLOW_NO_BACKUP ) THEN
      IERR_LVL = NVERB_WARNING
    ELSE
      IERR_LVL = NVERB_ERROR
    END IF
    CALL PRINT_MSG( IERR_LVL, 'IO', 'IO_Bakout_struct_prepare', 'no (valid) backup time' )
  END IF

  !Determine the list of the fields to write in each output
  IF ( OUT_MODEL(IMI)%NOUT_NUMB > 0 ) THEN
    !Count the number of fields to output
    IVAR = 0
    DO IPOS = 1,JPOUTVARMAX
      IF (COUT_VAR(IMI,IPOS)/='') IVAR = IVAR + 1
    END DO
    IF (IVAR==0) CALL PRINT_MSG(NVERB_WARNING,'IO','IO_Bakout_struct_prepare','no fields chosen for output')
    ALLOCATE( OUT_MODEL(IMI)%NOUT_FIELDLIST(IVAR) )

    IF ( IVAR > 0 ) THEN
      !Determine the list of the outputs to do (by field number)
      IVAR = 0
      DO IPOS = 1,JPOUTVARMAX
        IF (COUT_VAR(IMI,IPOS)/='') THEN
          IVAR=IVAR+1
          CALL FIND_FIELD_ID_FROM_MNHNAME(COUT_VAR(IMI,IPOS),IFIELD,IRESP)
          OUT_MODEL(IMI)%NOUT_FIELDLIST(IVAR) = IFIELD
          IF (IRESP/=0) THEN
            CALL PRINT_MSG(NVERB_FATAL,'IO','IO_Bakout_struct_prepare','unknown field for output: '//TRIM(COUT_VAR(IMI,IPOS)))
            !MNH is killed to prevent problems with wrong values in NOUT_FIELDLIST
          END IF
          !
        END IF
      END DO
    END IF
  END IF
  !
  IF ( IP == 1 ) THEN
    ! Backup information
    WRITE( *, '( "-------------------------------------------------" )' )
    WRITE( *, '( "Model number:      ", I9 )' ) IMI
    WRITE( *, '( "Number of backups: ", I9 )' ) OUT_MODEL(IMI)%NBAK_NUMB
    IF (OUT_MODEL(IMI)%NBAK_STEPFREQ > 0 ) THEN
      WRITE( *, '( "  Regular:         ", I9 )' ) &
             ( ISTEP_MAX - OUT_MODEL(IMI)%NBAK_STEPFREQFIRST ) / OUT_MODEL(IMI)%NBAK_STEPFREQ + 1
      WRITE( *, '( "   Frequency: ", I9, " timesteps (", F12.3, "s)" )' ) &
             OUT_MODEL(IMI)%NBAK_STEPFREQ, OUT_MODEL(IMI)%NBAK_STEPFREQ * DYN_MODEL(IMI)%XTSTEP
      WRITE( *, '( "   First:     ", I9, " timesteps (", F12.3, "s)" )' ) &
             OUT_MODEL(IMI)%NBAK_STEPFREQFIRST, ( OUT_MODEL(IMI)%NBAK_STEPFREQFIRST - 1 ) * DYN_MODEL(IMI)%XTSTEP
    ELSE
      WRITE( *, '( "  Regular:         ", I9 )' ) 0
    END IF
    WRITE( *, '( "  Iregular:        ", I9 )' ) SIZE( OUT_MODEL(IMI)%NBAK_STEPLIST )
    IF ( SIZE( OUT_MODEL(IMI)%NBAK_STEPLIST ) > 0 ) THEN
      WRITE( *, '( "   Timestep        Time" )' )
      DO JOUT = 1, SIZE( OUT_MODEL(IMI)%NBAK_STEPLIST )
        WRITE(*,'( "  ", I9,F12.3 )'  ) OUT_MODEL(IMI)%NBAK_STEPLIST(JOUT), &
                                        ( OUT_MODEL(IMI)%NBAK_STEPLIST(JOUT) - 1 ) * DYN_MODEL(IMI)%XTSTEP
      END DO
    END IF

    ! Output information
    WRITE( *, '( "-------------------------------------------------" )' )
    WRITE( *, '( "Model number:      ", I9 )' ) IMI
    WRITE( *, '( "Number of outputs: ", I9 )' ) OUT_MODEL(IMI)%NOUT_NUMB
    IF (OUT_MODEL(IMI)%NOUT_STEPFREQ > 0 ) THEN
      WRITE( *, '( "  Regular:         ", I9 )' ) &
             ( ISTEP_MAX - OUT_MODEL(IMI)%NOUT_STEPFREQFIRST ) / OUT_MODEL(IMI)%NOUT_STEPFREQ + 1
      WRITE( *, '( "   Frequency: ", I9, " timesteps (", F12.3, "s)" )' ) &
             OUT_MODEL(IMI)%NOUT_STEPFREQ, OUT_MODEL(IMI)%NOUT_STEPFREQ * DYN_MODEL(IMI)%XTSTEP
      WRITE( *, '( "   First:     ", I9, " timesteps (", F12.3, "s)" )' ) &
             OUT_MODEL(IMI)%NOUT_STEPFREQFIRST, ( OUT_MODEL(IMI)%NOUT_STEPFREQFIRST - 1 ) * DYN_MODEL(IMI)%XTSTEP
    ELSE
      WRITE( *, '( "  Regular:         ", I9 )' ) 0
    END IF
    WRITE( *, '( "  Iregular:        ", I9 )' ) SIZE( OUT_MODEL(IMI)%NOUT_STEPLIST )
    IF ( SIZE( OUT_MODEL(IMI)%NOUT_STEPLIST ) > 0 ) THEN
      WRITE( *, '( "   Timestep        Time" )' )
      DO JOUT = 1, SIZE( OUT_MODEL(IMI)%NOUT_STEPLIST )
        WRITE(*,'( "  ", I9,F12.3 )'  ) OUT_MODEL(IMI)%NOUT_STEPLIST(JOUT), &
                                        ( OUT_MODEL(IMI)%NOUT_STEPLIST(JOUT) - 1 ) * DYN_MODEL(IMI)%XTSTEP
      END DO
    END IF

    IF ( OUT_MODEL(IMI)%NOUT_NUMB > 0 ) THEN
      IF ( SIZE( OUT_MODEL(IMI)%NOUT_FIELDLIST ) > 0 ) THEN
        WRITE( *, '( "List of fields:" )' )
        DO JOUT = 1, SIZE( OUT_MODEL(IMI)%NOUT_FIELDLIST )
          IDX = OUT_MODEL(IMI)%NOUT_FIELDLIST(JOUT)
          WRITE(*, '( "  ", A )' ) TRIM(TFIELDLIST(IDX)%CMNHNAME)
        END DO
      END IF
    END IF

    WRITE( *, '( "-------------------------------------------------" )' )
  END IF
    !
END DO ! IMI=1,NMODEL
!
CALL BACKUP_NML_DEALLOCATE()
CALL OUTPUT_NML_DEALLOCATE()
!
! Set/initialize the pointers (necessary to use them now without OUT_MODEL(1)%...)
CALL OUT_GOTO_MODEL( 1, 1 )
NFILE_BACKUP_CURRENT => SUB_MODEL_MODEL(1)%NFILE_BACKUP_CURRENT
NFILE_OUTPUT_CURRENT => SUB_MODEL_MODEL(1)%NFILE_OUTPUT_CURRENT
!
CONTAINS
!
!#########################################################################
SUBROUTINE IO_INSERT_INT( KPOS, KSTEPS, KVAL )
!#########################################################################
  !
  INTEGER,              INTENT(INOUT) :: KPOS   ! First position to try to insert KVAL
  INTEGER,DIMENSION(:), INTENT(INOUT) :: KSTEPS ! Array in which to store KVAL
  INTEGER,              INTENT(IN)    :: KVAL   ! Value to store
  !
  CALL FIND_NEXT_AVAIL_SLOT_INT( KSTEPS, KPOS )
  KSTEPS(KPOS) = KVAL
  !
END SUBROUTINE IO_INSERT_INT
!
!#########################################################################
SUBROUTINE IO_INSERT_REGULAR_INT(KFIRST,KFREQ,KSTEPS)
!#########################################################################
  !
  INTEGER,              INTENT(IN)    :: KFIRST,KFREQ
  INTEGER,DIMENSION(:), INTENT(INOUT) :: KSTEPS
  !
  IDX = 1
  DO JOUT = KFIRST, ISTEP_MAX, KFREQ
    CALL FIND_NEXT_AVAIL_SLOT_INT(KSTEPS,IDX)
    KSTEPS(IDX) = JOUT
  END DO
END SUBROUTINE IO_INSERT_REGULAR_INT
!
!#########################################################################
SUBROUTINE IO_SYNC_MODELS_INT(KNUMB,KSTEPS)
!#########################################################################
  !
  INTEGER,                INTENT(INOUT) :: KNUMB
  INTEGER,DIMENSION(:,:), INTENT(INOUT) :: KSTEPS
  !
  INTEGER :: JKLOOP ! Loop index
  !
  DO JOUT = 1, NFILE_NUM_MAX
    IF (KSTEPS(IMI,JOUT) > 0) THEN
      KNUMB = KNUMB + 1
      !Output/backup time is propagated to nested models (with higher numbers)
      DO JKLOOP = IMI+1,NMODEL
        IDX = 1
        CALL FIND_NEXT_AVAIL_SLOT_INT(KSTEPS(JKLOOP,:),IDX)
        ! Use of NINT and real to prevent rounding errors
        ! (STEP-1)* ... +1 because step numbers begin at 1
        KSTEPS(JKLOOP,IDX) = (KSTEPS(IMI,JOUT)-1) * NINT( DYN_MODEL(IMI)%XTSTEP/DYN_MODEL(JKLOOP)%XTSTEP ) + 1
      END DO
    END IF
  END DO
END SUBROUTINE IO_SYNC_MODELS_INT
!
!#########################################################################
SUBROUTINE FIND_NEXT_AVAIL_SLOT_INT(KSTEPS,KIDX)
!#########################################################################
  !
  INTEGER,DIMENSION(:), INTENT(IN)    :: KSTEPS
  INTEGER,              INTENT(INOUT) :: KIDX
  !
  !Find next (starting from KIDX) non 'allocated' element
  DO WHILE ( KSTEPS(KIDX) >= 0 )
    KIDX = KIDX + 1
    IF (KIDX > NFILE_NUM_MAX) CALL PRINT_MSG(NVERB_FATAL,'IO','FIND_NEXT_AVAIL_SLOT_INT','NFILE_NUM_MAX too small')
  END DO
END SUBROUTINE FIND_NEXT_AVAIL_SLOT_INT
!
!#########################################################################
SUBROUTINE FIND_REMOVE_DUPLICATES(KNUMB,KSTEPS)
!#########################################################################
  !
  INTEGER,              INTENT(IN)    :: KNUMB
  INTEGER,DIMENSION(:), INTENT(INOUT) :: KSTEPS
  !
  INTEGER :: JKLOOP ! Loop index
  !
  DO JOUT = 1,KNUMB
    DO JKLOOP = JOUT+1,KNUMB
      IF ( KSTEPS(JKLOOP) == KSTEPS(JOUT) .AND. KSTEPS(JKLOOP) > 0 ) THEN
        CALL PRINT_MSG(NVERB_DEBUG,'IO','FIND_REMOVE_DUPLICATES','found duplicated backup/output step (removed extra one)')
        KSTEPS(JKLOOP) = NNEGUNDEF
      END IF
    END DO
  END DO
END SUBROUTINE FIND_REMOVE_DUPLICATES
!
!#########################################################################
SUBROUTINE FIND_REMOVE_OUTOFTIMERANGE(KNUMB,KSTEPS)
!#########################################################################
  !
  INTEGER,              INTENT(IN)    :: KNUMB
  INTEGER,DIMENSION(:), INTENT(INOUT) :: KSTEPS
  !
  DO JOUT = 1,KNUMB
    IF ( KSTEPS(JOUT) < 1 .OR. KSTEPS(JOUT) > ISTEP_MAX ) THEN
      IF ( KSTEPS(JOUT) /= NNEGUNDEF ) &
        CALL PRINT_MSG(NVERB_WARNING,'IO','FIND_REMOVE_OUTOFTIMERANGE','found backup/output step outside of time range')
      KSTEPS(JOUT) = NNEGUNDEF
    END IF
  END DO
END SUBROUTINE FIND_REMOVE_OUTOFTIMERANGE
!
!#########################################################################
SUBROUTINE SORT_ENTRIES(KNUMB,KSTEPS)
!#########################################################################
  !
  INTEGER,              INTENT(IN)    :: KNUMB
  INTEGER,DIMENSION(:), INTENT(INOUT) :: KSTEPS
  !
  INTEGER :: ITEMP  ! Intermediate variable
  INTEGER :: JKLOOP ! Loop index
  !
  DO JOUT = 1,KNUMB
    ITEMP = KSTEPS(JOUT)
    IF (ITEMP<=0) ITEMP = HUGE(ITEMP)
    IPOS = -1
    DO JKLOOP = JOUT+1,KNUMB
      IF ( KSTEPS(JKLOOP) < ITEMP .AND. KSTEPS(JKLOOP) >= 0 ) THEN
        ITEMP = KSTEPS(JKLOOP)
        IPOS = JKLOOP
      END IF
    END DO
    IF (IPOS >= JOUT) THEN
      KSTEPS(IPOS) = KSTEPS(JOUT)
      KSTEPS(JOUT) = ITEMP
    END IF
  END DO
END SUBROUTINE SORT_ENTRIES
!
END SUBROUTINE IO_Bakout_struct_prepare


FUNCTION IO_Is_backup_time( KMI, KTCOUNT, KNUMBAK ) RESULT( OBAK )
  USE MODD_OUT_n
  USE MODD_SUB_MODEL_N, ONLY: NFILE_BACKUP_CURRENT

  ! Determine if it is a step when a backup is needed
  INTEGER, INTENT(IN)    :: KMI     ! Model number
  INTEGER, INTENT(IN)    :: KTCOUNT ! Timestep
  INTEGER, INTENT(INOUT) :: KNUMBAK ! Number of the backup
  LOGICAL :: OBAK    ! Result

  INTEGER :: JI

  WRITE( CMNHMSG(1), '( "called for timestep ", I0, " on model ", I0 )' ) KTCOUNT, KMI
  CALL PRINT_MSG( NVERB_DEBUG, 'IO', 'IO_Is_backup_time' )

  OBAK = .FALSE.
  KNUMBAK = NNEGUNDEF

  ! No more backups to do
  IF ( NFILE_BACKUP_CURRENT >= NBAK_NUMB ) RETURN

  ! Check if it is time for a regular backup
  IF ( NBAK_STEPFREQ > 0 .AND. KTCOUNT >= NBAK_STEPFREQFIRST ) THEN
    IF ( MOD( KTCOUNT - NBAK_STEPFREQFIRST, NBAK_STEPFREQ ) == 0 ) OBAK = .TRUE.
  END IF

  ! Check if it is time for an irregular backup
  IF ( .NOT. OBAK ) THEN
    DO JI = 1, SIZE( NBAK_STEPLIST )
      IF ( NBAK_STEPLIST(JI) == KTCOUNT ) THEN
        OBAK = .TRUE.
        EXIT
      END IF
    END DO
  END IF

  IF ( OBAK ) THEN
    NFILE_BACKUP_CURRENT = NFILE_BACKUP_CURRENT + 1
    KNUMBAK = NFILE_BACKUP_CURRENT
  END IF

END FUNCTION IO_Is_backup_time


FUNCTION IO_Is_output_time( KMI, KTCOUNT, KNUMOUT ) RESULT( OOUT )
  USE MODD_OUT_n
  USE MODD_SUB_MODEL_N, ONLY: NFILE_OUTPUT_CURRENT

  ! Determine if it is a step when a output is needed
  INTEGER, INTENT(IN)    :: KMI     ! Model number
  INTEGER, INTENT(IN)    :: KTCOUNT ! Timestep
  INTEGER, INTENT(INOUT) :: KNUMOUT ! Number of the output
  LOGICAL :: OOUT    ! Result

  INTEGER :: JI

  WRITE( CMNHMSG(1), '( "called for timestep ", I0, " on model ", I0 )' ) KTCOUNT, KMI
  CALL PRINT_MSG( NVERB_DEBUG, 'IO', 'IO_Is_output_time' )

  OOUT = .FALSE.
  KNUMOUT = NNEGUNDEF

  ! No more outputs to do
  IF ( NFILE_OUTPUT_CURRENT >= NOUT_NUMB ) RETURN

  ! Check if it is time for a regular output
  IF ( NOUT_STEPFREQ > 0 .AND. KTCOUNT >= NOUT_STEPFREQFIRST ) THEN
    IF ( MOD( KTCOUNT - NOUT_STEPFREQFIRST, NOUT_STEPFREQ ) == 0 ) OOUT = .TRUE.
  END IF

  ! Check if it is time for an irregular output
  IF ( .NOT. OOUT ) THEN
    DO JI = 1, SIZE( NOUT_STEPLIST )
      IF ( NOUT_STEPLIST(JI) == KTCOUNT ) THEN
        OOUT = .TRUE.
        EXIT
      END IF
    END DO
  END IF

  IF ( OOUT ) THEN
    NFILE_OUTPUT_CURRENT = NFILE_OUTPUT_CURRENT + 1
    KNUMOUT = NFILE_OUTPUT_CURRENT
  END IF

END FUNCTION IO_Is_output_time


SUBROUTINE IO_BakOut_file_create( TPFILE, HTYPE, KMI, KSTEP, KNUMBAK )
  USE MODD_BAKOUT,      ONLY: CBAK_DIR, COUT_DIR
  USE MODD_CONF,        ONLY: CEXP, CSEG, NMODEL, NVERB
  USE MODD_CONF_n,      ONLY: NRR
  USE MODD_NSV,         ONLY: NSV
  USE MODD_PARAMETERS,  ONLY: NMODELNUMLGTMAX

  TYPE(TFILEDATA), POINTER, INTENT(INOUT) :: TPFILE  ! File structure to return
  CHARACTER(LEN=*),         INTENT(IN)    :: HTYPE   ! File type
  INTEGER,                  INTENT(IN)    :: KMI     ! Model number
  INTEGER,                  INTENT(IN)    :: KSTEP   ! Timestep number
  INTEGER,                  INTENT(IN)    :: KNUMBAK ! Number of the backup

  CHARACTER(LEN=:), ALLOCATABLE :: YDIRNAME
  CHARACTER(LEN=:), ALLOCATABLE :: YFORMAT
  CHARACTER(LEN=:), ALLOCATABLE :: YNAME, YNAMEPRE
  CHARACTER(LEN=:), ALLOCATABLE :: YNUMBER ! Character string for the file number
  INTEGER                       :: ILEN
  INTEGER(KIND=LFIINT)          :: ILFINPRAR

  CALL PRINT_MSG( NVERB_DEBUG, 'IO', 'IO_BakOut_file_create', 'called' )

  IF ( HTYPE /= 'MNHBACKUP' .AND. HTYPE /= 'MNHOUTPUT' ) &
    CALL PRINT_MSG( NVERB_FATAL, 'IO', 'IO_BakOut_file_create', 'invalid HTYPE' )

  IF ( NFILE_NUM_MAX < 1000 ) THEN
    ALLOCATE( CHARACTER(LEN=3) :: YNUMBER )
    WRITE ( YNUMBER, FMT = "(I3.3)" ) KNUMBAK
  ELSE IF ( NFILE_NUM_MAX < 10000 ) THEN
    ALLOCATE( CHARACTER(LEN=4) :: YNUMBER )
    WRITE ( YNUMBER, FMT = "(I4.4)" ) KNUMBAK
  ELSE IF ( NFILE_NUM_MAX < 100000 ) THEN
    ALLOCATE( CHARACTER(LEN=5) :: YNUMBER )
    WRITE ( YNUMBER, FMT = "(I5.5)" ) KNUMBAK
  ELSE IF ( NFILE_NUM_MAX < 1000000 ) THEN
    ALLOCATE( CHARACTER(LEN=6) :: YNUMBER )
    WRITE ( YNUMBER, FMT = "(I6.6)" ) KNUMBAK
  ELSE
    CALL PRINT_MSG( NVERB_FATAL, 'IO', 'IO_BakOut_file_create', 'NFILE_NUM_MAX is too large' )
  END IF

  ILEN = LEN_TRIM(CEXP) + 1 + NMODELNUMLGTMAX + 1 + LEN_TRIM(CSEG)
  ALLOCATE( CHARACTER(LEN=ILEN) :: YNAMEPRE )
  IF ( NMODELNUMLGTMAX == 1 ) THEN
    IF ( NMODEL > 9 ) CALL PRINT_MSG( NVERB_FATAL, 'GEN', 'IO_BakOut_file_create', 'NMODEL>9 and NMODELNUMLGTMAX=1' )
    WRITE( YNAMEPRE, '( A, ".", I1, ".", A) ' ) TRIM(CEXP), KMI, TRIM(CSEG)
  ELSE IF ( NMODELNUMLGTMAX == 2 ) THEN
    IF ( NMODEL > 99 ) CALL PRINT_MSG( NVERB_FATAL, 'GEN', 'IO_BakOut_file_create', 'NMODEL>99 and NMODELNUMLGTMAX=2' )
    WRITE( YNAMEPRE, '( A, ".", I2.2, ".", A) ' ) TRIM(CEXP), KMI, TRIM(CSEG)
  ELSE
    CALL PRINT_MSG( NVERB_FATAL, 'GEN', 'IO_BakOut_file_create', 'NMODELNUMLGTMAX>2 not implemented' )
  END IF

  IF ( HTYPE == 'MNHOUTPUT' ) THEN
    ! Add a "OUT" suffix for output files
    YNAME = YNAMEPRE // '.OUT.' // YNUMBER

     !Set output directory
    IF (LEN_TRIM(COUT_DIR)>0) THEN
      YDIRNAME = TRIM(COUT_DIR)
    ELSE IF (LEN_TRIM(CIO_DIR)>0) THEN
      YDIRNAME = TRIM(CIO_DIR)
    ELSE
      YDIRNAME = ''
    END IF
  ELSE IF ( HTYPE == 'MNHBACKUP' ) THEN
    YNAME = YNAMEPRE // '.' // YNUMBER

    IF (LEN_TRIM(CBAK_DIR)>0) THEN
      YDIRNAME = TRIM(CBAK_DIR)
    ELSE IF (LEN_TRIM(CIO_DIR)>0) THEN
      YDIRNAME = TRIM(CIO_DIR)
    ELSE
      YDIRNAME = ''
    END IF
  END IF

  IF ( LIOCDF4 ) THEN
    IF ( .NOT.LLFIOUT ) THEN
      YFORMAT = 'NETCDF4'
    ELSE
      YFORMAT = 'LFICDF4'
      IF ( HTYPE == 'MNHBACKUP' ) ILFINPRAR = 22+2*(4+NRR+NSV)
    END IF
  ELSE IF ( LLFIOUT ) THEN
    YFORMAT = 'LFI'
    IF ( HTYPE == 'MNHBACKUP') ILFINPRAR = 22+2*(4+NRR+NSV)
  ELSE
    CALL PRINT_MSG( NVERB_FATAL, 'IO', 'IO_BakOut_file_create', 'unknown backup/output fileformat' )
  ENDIF

  CALL IO_File_add2list( TPFILE, HNAME=YNAME, HTYPE=HTYPE, HMODE='WRITE', HFORMAT=YFORMAT, HDIRNAME=YDIRNAME, &
                         KLFINPRAR=ILFINPRAR, KLFITYPE=1, KLFIVERB=NVERB, KMODEL=KMI, KSTEP=KSTEP )

END SUBROUTINE IO_BakOut_file_create


SUBROUTINE IO_File_add2list( TPFILE, HNAME, HTYPE, HMODE,                         &
                             HFORM, HACCESS, HFORMAT, HDIRNAME,                   &
                             KLFINPRAR, KLFITYPE, KLFIVERB, KRECL, KMODEL, KSTEP, &
                             TPDADFILE, TPDATAFILE, TPMAINFILE, OOLD, OSPLIT_IOZ  )
!
#ifdef MNH_IOCDF4
  USE NETCDF, ONLY: NF90_QUANTIZE_BITGROOM, NF90_QUANTIZE_BITROUND, NF90_QUANTIZE_GRANULARBR
!
#endif
USE MODD_BAKOUT,         ONLY: LBAK_COMPRESS, NBAK_COMPRESS_LEVEL, LBAK_REDUCE_FLOAT_PRECISION, &
                               LOUT_COMPRESS, NOUT_COMPRESS_LEVEL, LOUT_REDUCE_FLOAT_PRECISION, &
                               COUT_COMPRESS_LOSSY_ALGO, LOUT_COMPRESS_LOSSY, NOUT_COMPRESS_LOSSY_NSD
USE MODD_CONF,           ONLY: CPROGRAM
USE MODD_CONFZ,          ONLY: NB_PROCIO_R, NB_PROCIO_W
USE MODD_DYN_n,          ONLY: DYN_MODEL
USE MODD_IO,             ONLY: LDIAG_REDUCE_FLOAT_PRECISION, LIO_COMPRESS, NIO_COMPRESS_LEVEL
USE MODD_NESTING,        ONLY: NDAD
!
USE MODE_MODELN_HANDLER, ONLY: GET_CURRENT_MODEL_INDEX
USE MODE_TOOLS,          ONLY: UPCASE
!
TYPE(TFILEDATA),POINTER,         INTENT(INOUT) :: TPFILE    !File structure to return
CHARACTER(LEN=*),                INTENT(IN)    :: HNAME     !Filename
CHARACTER(LEN=*),                INTENT(IN)    :: HTYPE     !Filetype (backup, output, prepidealcase...)
CHARACTER(LEN=*),                INTENT(IN)    :: HMODE     !Opening mode (read, write...)
CHARACTER(LEN=*),       OPTIONAL,INTENT(IN)    :: HFORM     !Formatted/unformatted
CHARACTER(LEN=*),       OPTIONAL,INTENT(IN)    :: HACCESS   !Direct/sequential/stream
CHARACTER(LEN=*),       OPTIONAL,INTENT(IN)    :: HFORMAT   !Fileformat (NETCDF4, LFI, LFICDF4...)
CHARACTER(LEN=*),       OPTIONAL,INTENT(IN)    :: HDIRNAME  !File directory
INTEGER(KIND=LFIINT),   OPTIONAL,INTENT(IN)    :: KLFINPRAR !Number of predicted articles of the LFI file (non crucial)
INTEGER,                OPTIONAL,INTENT(IN)    :: KLFITYPE  !Type of the file (used to generate list of files to transfers)
INTEGER,                OPTIONAL,INTENT(IN)    :: KLFIVERB  !LFI verbosity level
INTEGER,                OPTIONAL,INTENT(IN)    :: KRECL     !Record length
INTEGER,                OPTIONAL,INTENT(IN)    :: KMODEL    !Model number
INTEGER,                OPTIONAL,INTENT(IN)    :: KSTEP     !Timestep number
TYPE(TFILEDATA),POINTER,OPTIONAL,INTENT(IN)    :: TPDADFILE !Corresponding dad file
TYPE(TFILEDATA),POINTER,OPTIONAL,INTENT(IN)    :: TPDATAFILE!Corresponding data file (used only for DES files)
TYPE(TFILEDATA),POINTER,OPTIONAL,INTENT(IN)    :: TPMAINFILE!Corresponding main file (for subfiles)
LOGICAL,                OPTIONAL,INTENT(IN)    :: OOLD      !FALSE if new file (should not be found)
                                                            !TRUE if the file could already be in the list
                                                            !     (add it only if not yet present)
logical,                optional,intent(in)    :: osplit_ioz !Is the file split vertically
!
INTEGER :: IMI
INTEGER :: IRESP
INTEGER :: ILFITYPE
INTEGER :: ILFIVERB
INTEGER :: IMULT
INTEGER(KIND=LFIINT) :: ILFINPRAR
LOGICAL :: GOLD
logical :: gsplit_ioz
TYPE(TFILEDATA), POINTER :: TZFILE
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_File_add2list','called for '//TRIM(HNAME))
!
IMI = GET_CURRENT_MODEL_INDEX()
!
IF (PRESENT(OOLD)) THEN
  GOLD = OOLD
ELSE
  GOLD = .FALSE. !By default, we assume file is not yet in list
END IF
!
IF (ASSOCIATED(TPFILE)) THEN
  IF (GOLD) THEN
    CALL PRINT_MSG(NVERB_INFO,'IO','IO_File_add2list','file '//TRIM(HNAME)//' already associated. Pointer will be overwritten')
    TPFILE => NULL()
  ELSE
    CALL PRINT_MSG(NVERB_FATAL,'IO','IO_File_add2list','file '//TRIM(HNAME)//' already associated')
  END IF
END IF
!
CALL IO_File_find_byname(HNAME,TPFILE,IRESP,OOLD=GOLD)
IF (IRESP==0) THEN
  !File has been found
  !Check if really same one (LFI vs netCDF)
  IF (PRESENT(HFORMAT)) THEN
    IF ( (HFORMAT=='LFI' .AND. TPFILE%CFORMAT/='NETCDF4') .OR. (HFORMAT=='NETCDF4' .AND. TPFILE%CFORMAT/='LFI') ) THEN
      CALL PRINT_MSG(NVERB_FATAL,'IO','IO_File_add2list','file '//TRIM(HNAME)//' already in filelist')
    END IF
  ELSE
    IF (.NOT.GOLD) THEN
      CALL PRINT_MSG(NVERB_ERROR,'IO','IO_File_add2list','file '//TRIM(HNAME)//' already in filelist')
    ELSE
      CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_File_add2list','file '//TRIM(HNAME)//' already in filelist (not unexpected)')
    END IF
    RETURN
  END IF
END IF
!
IF(     PRESENT(HFORM) .AND. TRIM(HTYPE)/='SURFACE_DATA') &
    CALL PRINT_MSG(NVERB_WARNING,'IO','IO_File_add2list','optional argument HFORM is not used by '//TRIM(HTYPE)//' files')
IF(.NOT.PRESENT(HFORM) .AND. TRIM(HTYPE)=='SURFACE_DATA') &
    CALL PRINT_MSG(NVERB_ERROR,'IO','IO_File_add2list','optional argument HFORM is necessary for '//TRIM(HTYPE)//' files')
IF(PRESENT(HFORM)) THEN
  IF(HFORM/='FORMATTED' .AND. HFORM/='UNFORMATTED') &
    CALL PRINT_MSG(NVERB_ERROR,'IO','IO_File_add2list','HFORM should be FORMATTED or UNFORMATTED and not '//TRIM(HFORM))
END IF
!
IF(     PRESENT(HACCESS) .AND. TRIM(HTYPE)/='SURFACE_DATA') &
    CALL PRINT_MSG(NVERB_WARNING,'IO','IO_File_add2list','optional argument HACCESS is not used by '//TRIM(HTYPE)//' files')
IF(.NOT.PRESENT(HACCESS) .AND. TRIM(HTYPE)=='SURFACE_DATA') &
    CALL PRINT_MSG(NVERB_ERROR,'IO','IO_File_add2list','optional argument HACCESS is necessary for '//TRIM(HTYPE)//' files')
IF(PRESENT(HACCESS)) THEN
  IF(HACCESS/='DIRECT' .AND. HACCESS/='SEQUENTIAL' .AND. HACCESS/='STREAM') &
    CALL PRINT_MSG(NVERB_ERROR,'IO','IO_File_add2list','HACCESS should be DIRECT, SEQUENTIAL or STREAM and not '//TRIM(HACCESS))
END IF
!
IF (PRESENT(HFORMAT)) THEN
  IF(CPROGRAM=='LFICDF') THEN
    IF (HFORMAT/='LFI' .AND. HFORMAT/='NETCDF4') &
      CALL PRINT_MSG(NVERB_ERROR,'IO','IO_File_add2list','invalid HFORMAT ('//TRIM(HFORMAT)//')')
  END IF
ELSE
  IF(CPROGRAM=='LFICDF') &
    CALL PRINT_MSG(NVERB_FATAL,'IO','IO_File_add2list','optional argument HFORMAT is necessary for CPROGRAM='//TRIM(CPROGRAM))
END IF
!
IF(PRESENT(KLFINPRAR)) THEN
  ILFINPRAR = KLFINPRAR
ELSE
  ILFINPRAR = 0
END IF
!
IF(PRESENT(KLFITYPE)) THEN
  ILFITYPE = KLFITYPE
ELSE
  ILFITYPE = -1
END IF
!
IF(PRESENT(KLFIVERB)) THEN
  ILFIVERB = KLFIVERB
ELSE
  ILFIVERB = -1
END IF
!
IF(     PRESENT(KRECL) .AND. TRIM(HTYPE)/='SURFACE_DATA' .AND. TRIM(HTYPE)/='TXT') &
    CALL PRINT_MSG(NVERB_WARNING,'IO','IO_File_add2list','optional argument KRECL is not used by '//TRIM(HTYPE)//' files')
IF(.NOT.PRESENT(KRECL) .AND. TRIM(HTYPE)=='SURFACE_DATA') THEN
    IF(TRIM(HACCESS)=='DIRECT') &
      CALL PRINT_MSG(NVERB_ERROR,'IO','IO_File_add2list','optional argument KRECL is necessary for '//TRIM(HTYPE)// &
                                                         ' files in DIRECT access')
END IF
!
IF (PRESENT(TPDATAFILE) .AND. TRIM(HTYPE)/='DES') &
    CALL PRINT_MSG(NVERB_WARNING,'IO','IO_File_add2list','optional argument TPDATAFILE is not used by '//TRIM(HTYPE)//' files')
!
IF ( PRESENT( TPMAINFILE) ) THEN
  IF ( LEN(HTYPE) >= 3 ) THEN
    IF ( HTYPE(1:3) /= 'MNH' ) &
      CALL PRINT_MSG(NVERB_WARNING,'IO','IO_File_add2list','optional argument TPMAINFILE is not used by '//TRIM(HTYPE)//' files')
  ELSE
    CALL PRINT_MSG(NVERB_WARNING,'IO','IO_File_add2list','optional argument TPMAINFILE is not used by '//TRIM(HTYPE)//' files')
  END IF
END IF
!
IF (.NOT.ASSOCIATED(TFILE_LAST)) THEN
  ALLOCATE(TFILE_LAST)
  TFILE_FIRST => TFILE_LAST
ELSE
  ALLOCATE(TFILE_LAST%TFILE_NEXT)
  TFILE_LAST%TFILE_NEXT%TFILE_PREV => TFILE_LAST
  TFILE_LAST => TFILE_LAST%TFILE_NEXT
END IF
!
TPFILE => TFILE_LAST
!
TPFILE%CNAME = HNAME
TPFILE%CTYPE = HTYPE
!
IF (PRESENT(HDIRNAME)) THEN
  IF (LEN_TRIM(HDIRNAME)>0) TPFILE%CDIRNAME=TRIM(HDIRNAME)
END IF
!
IF (TRIM(HMODE)/='READ' .AND. TRIM(HMODE)/='WRITE') THEN
  CALL PRINT_MSG(NVERB_FATAL,'IO','IO_File_add2list','unknown mode ('//TRIM(HMODE)//') for file '//TRIM(HNAME))
END IF
!
TPFILE%CMODE = HMODE
!
IF( PRESENT( KMODEL ) ) TPFILE%NMODEL = KMODEL
!
IF( PRESENT( KSTEP )  ) TPFILE%NSTEP  = KSTEP
!
if ( present(osplit_ioz) ) then
  gsplit_ioz = osplit_ioz
else
  gsplit_ioz = .false.
  if ( len_trim(htype) >= 3 ) then
    if ( htype(1:3) == 'MNH' ) then
      ! MNH/MNHBACKUP/MNHOUTPUT
      !Remark: 'MNH' is more general than MNHBACKUP and could be in fact a MNHBACKUP file
      gsplit_ioz = .true.
    end if
  end if
end if

if ( gsplit_ioz ) then
  select case (hmode)
    case('READ')
      tpfile%nsubfiles_ioz = nb_procio_r
    case('WRITE')
      tpfile%nsubfiles_ioz = nb_procio_w
  end select
  if (tpfile%nsubfiles_ioz == 1) tpfile%nsubfiles_ioz = 0
else
  tpfile%nsubfiles_ioz = 0
end if

IF ( PRESENT(TPMAINFILE) ) THEN
  IF ( .NOT. ASSOCIATED( TPMAINFILE ) ) &
    CALL PRINT_MSG( NVERB_ERROR, 'IO', 'IO_File_add2list', 'TPMAINFILE is not associated for file ' // TRIM(HNAME) )
  TPFILE%TMAINFILE => TPMAINFILE
ELSE
  TPFILE%TMAINFILE => NULL()
END IF

SELECT CASE(TPFILE%CTYPE)
  !Chemistry input files
  CASE('CHEMINPUT')
    IF (TRIM(HMODE)/='READ') & !Invalid because not (yet) necessary
      CALL PRINT_MSG(NVERB_ERROR,'IO','IO_File_add2list','invalid mode '//TRIM(HMODE)//' for file '//TRIM(HNAME))
    TPFILE%CACCESS = 'SEQUENTIAL'
    TPFILE%CFORM   = 'FORMATTED'
    TPFILE%CFORMAT = 'TEXT'


  !Chemistry tabulation files
  CASE('CHEMTAB')
    IF (TRIM(HMODE)/='READ') & !Invalid because not (yet) necessary
      CALL PRINT_MSG(NVERB_ERROR,'IO','IO_File_add2list','invalid mode '//TRIM(HMODE)//' for file '//TRIM(HNAME))
    TPFILE%CACCESS = 'SEQUENTIAL'
    TPFILE%CFORM   = 'FORMATTED'
    TPFILE%CFORMAT = 'TEXT'


  !DES files
  CASE('DES')
    TPFILE%CACCESS = 'SEQUENTIAL'
    TPFILE%CFORM   = 'FORMATTED'
    TPFILE%CFORMAT = 'TEXT'
    TPFILE%NRECL   = 8*1024
    IF (.NOT.PRESENT(TPDATAFILE)) THEN
      CALL PRINT_MSG(NVERB_ERROR,'IO','IO_File_add2list','missing TPDATAFILE argument for DES file '//TRIM(HNAME))
    ELSE
      IF (.NOT.ASSOCIATED(TPDATAFILE)) &
        CALL PRINT_MSG(NVERB_ERROR,'IO','IO_File_add2list','TPDATAFILE is not associated for DES file '//TRIM(HNAME))
      TPFILE%TDATAFILE => TPDATAFILE
      TPDATAFILE%TDESFILE => TPFILE
      IF (PRESENT(HDIRNAME)) &
        CALL PRINT_MSG(NVERB_WARNING,'IO','IO_File_add2list','HDIRNAME argument ignored for DES file '//TRIM(HNAME))
      IF (ALLOCATED(TPDATAFILE%CDIRNAME)) TPFILE%CDIRNAME = TPDATAFILE%CDIRNAME
    END IF


  !GPS files
  CASE('GPS')
    IF (TRIM(HMODE)/='WRITE') & !Invalid because not (yet) necessary
      CALL PRINT_MSG(NVERB_ERROR,'IO','IO_File_add2list','invalid mode '//TRIM(HMODE)//' for file '//TRIM(HNAME))
    TPFILE%CACCESS = 'SEQUENTIAL'
    TPFILE%CFORM   = 'FORMATTED'
    TPFILE%CFORMAT = 'TEXT'


  !Meteo files
  CASE('METEO')
    IF (TRIM(HMODE)/='WRITE') & !Invalid because not (yet) necessary
      CALL PRINT_MSG(NVERB_ERROR,'IO','IO_File_add2list','invalid mode '//TRIM(HMODE)//' for file '//TRIM(HNAME))
    TPFILE%CACCESS = 'SEQUENTIAL'
    TPFILE%CFORM   = 'UNFORMATTED'
    TPFILE%CFORMAT = 'BINARY'
    TPFILE%NRECL   = 100000000


  !Namelist files
  CASE('NML')
    IF (TRIM(HMODE)/='READ') & !Invalid because not (yet) necessary
      CALL PRINT_MSG(NVERB_ERROR,'IO','IO_File_add2list','invalid mode '//TRIM(HMODE)//' for file '//TRIM(HNAME))
    TPFILE%CACCESS = 'SEQUENTIAL'
    TPFILE%CFORM   = 'FORMATTED'
    TPFILE%CFORMAT = 'TEXT'


  !OUTPUTLISTING files
  CASE('OUTPUTLISTING')
    IF (TRIM(HMODE)/='WRITE') & !Invalid because not (yet) necessary
      CALL PRINT_MSG(NVERB_ERROR,'IO','IO_File_add2list','invalid mode '//TRIM(HMODE)//' for file '//TRIM(HNAME))
    TPFILE%CACCESS = 'SEQUENTIAL'
    TPFILE%CFORM   = 'FORMATTED'
    TPFILE%CFORMAT = 'TEXT'


  !SURFACE_DATA files
  CASE('SURFACE_DATA')
    IF (TRIM(HMODE)/='READ') & !Invalid because not (yet) necessary
      CALL PRINT_MSG(NVERB_ERROR,'IO','IO_File_add2list','invalid mode '//TRIM(HMODE)//' for file '//TRIM(HNAME))
    TPFILE%CFORMAT = 'SURFACE_DATA'
    TPFILE%CFORM   = HFORM
    TPFILE%CACCESS = HACCESS
    IF(TRIM(HACCESS)=='DIRECT') TPFILE%NRECL = KRECL


  !Text files
  CASE('TXT')
    TPFILE%CACCESS = 'SEQUENTIAL'
    TPFILE%CFORM   = 'FORMATTED'
    TPFILE%CFORMAT = 'TEXT'
    IF(PRESENT(KRECL)) TPFILE%NRECL = KRECL


  !MesoNH files
  !Remark: 'MNH' is more general than MNHBACKUP and could be in fact a MNHBACKUP file
  CASE ('MNH', 'MNHBACKUP', 'MNHDIACHRONIC', 'MNHDIAG', 'MNHOUTPUT', 'PGD')
    IF (TRIM(HMODE)=='READ') THEN
      IF (PRESENT(HFORMAT)) THEN
        TPFILE%CFORMAT = TRIM(HFORMAT)
      ELSE IF (LLFIREAD) THEN
        TPFILE%CFORMAT = 'LFI'
        TPFILE%NLFINPRAR = ILFINPRAR
      ELSE IF (LIOCDF4) THEN
        TPFILE%CFORMAT = 'NETCDF4'
      ELSE
        CALL PRINT_MSG(NVERB_FATAL,'IO','IO_File_add2list','invalid format for file '//TRIM(HNAME))
      END IF
    ELSE IF (TRIM(HMODE)=='WRITE') THEN
      IF (PRESENT(HFORMAT)) THEN
        TPFILE%CFORMAT = TRIM(HFORMAT)
      ELSE IF (LLFIOUT .AND. LIOCDF4) THEN
        TPFILE%CFORMAT = 'LFICDF4'
        TPFILE%NLFINPRAR = ILFINPRAR
      ELSE IF (LIOCDF4) THEN
        TPFILE%CFORMAT = 'NETCDF4'
      ELSE IF (LLFIOUT) THEN
        TPFILE%CFORMAT = 'LFI'
        TPFILE%NLFINPRAR = ILFINPRAR
      ELSE
        CALL PRINT_MSG(NVERB_FATAL,'IO','IO_File_add2list','invalid format for file '//TRIM(HNAME))
      END IF
    END IF
    !
    TPFILE%NLFITYPE = ILFITYPE
    TPFILE%NLFIVERB = ILFIVERB
    !
    ! Apply compression to all HTYPE='MNH*' files (if asked)
    IF ( LIO_COMPRESS ) THEN
      TPFILE%LNCCOMPRESS       = LIO_COMPRESS
      IF ( NIO_COMPRESS_LEVEL < 0 .OR. NIO_COMPRESS_LEVEL > 9 ) THEN
        CALL PRINT_MSG( NVERB_ERROR, 'IO', 'IO_File_add2list', &
                        'NIO_COMPRESS_LEVEL must be in the [0..9] range' )
        NIO_COMPRESS_LEVEL = 4
      END IF
      TPFILE%NNCCOMPRESS_LEVEL = NIO_COMPRESS_LEVEL
    END IF
    !
    IF (TRIM(HTYPE)=='MNHOUTPUT') THEN
#ifdef MNH_IOCDF4
      TPFILE%LNCREDUCE_FLOAT_PRECISION = LOUT_REDUCE_FLOAT_PRECISION(IMI)
      ! Apply compression to output files if not already forced for all
      IF ( .NOT. LIO_COMPRESS ) THEN
        TPFILE%LNCCOMPRESS             = LOUT_COMPRESS(IMI)
        IF ( NOUT_COMPRESS_LEVEL(IMI) < 0 .OR. NOUT_COMPRESS_LEVEL(IMI) > 9 ) THEN
          CALL PRINT_MSG( NVERB_ERROR, 'IO', 'IO_File_add2list', &
                          'NOUT_COMPRESS_LEVEL must be in the [0..9] range' )
          NOUT_COMPRESS_LEVEL(IMI) = 4
        END IF
        TPFILE%NNCCOMPRESS_LEVEL       = NOUT_COMPRESS_LEVEL(IMI)
      END IF

      !Set lossy compression
      TPFILE%LNCCOMPRESS_LOSSY = LOUT_COMPRESS_LOSSY(IMI)
      IF ( LOUT_COMPRESS_LOSSY(IMI) ) THEN
        !Force compression if lossy compression is enabled
        TPFILE%LNCCOMPRESS = .TRUE.

        !Set lossy compression algorithm
        SELECT CASE ( UPCASE( COUT_COMPRESS_LOSSY_ALGO(IMI) ) )
          CASE ( 'BITGROOM' )
            TPFILE%NNCCOMPRESS_LOSSY_ALGO = NF90_QUANTIZE_BITGROOM
          CASE ( 'GRANULARBR' )
            TPFILE%NNCCOMPRESS_LOSSY_ALGO = NF90_QUANTIZE_GRANULARBR
          CASE ( 'BITROUND' )
            TPFILE%NNCCOMPRESS_LOSSY_ALGO = NF90_QUANTIZE_BITROUND
          CASE DEFAULT
            CMNHMSG(1) = 'invalid COUT_COMPRESS_LOSSY_ALGO'
            CMNHMSG(2) = 'Accepted algorithms: BITGROOM, GRANULARBR (default choice), BITROUND'
            CALL PRINT_MSG( NVERB_ERROR, 'IO', 'IO_File_add2list' )
            TPFILE%NNCCOMPRESS_LOSSY_ALGO = NF90_QUANTIZE_GRANULARBR
        END SELECT

        !Set number of significant digits/bits for lossy compression algorithm
#if (MNH_REAL == 4)
        SELECT CASE ( TPFILE%NNCCOMPRESS_LOSSY_ALGO )
          CASE ( NF90_QUANTIZE_BITROUND )
            ! For 32 bit reals, number of significant bits must be in the 1 to 23 range
            IF ( NOUT_COMPRESS_LOSSY_NSD(IMI) < 1 .OR. NOUT_COMPRESS_LOSSY_NSD(IMI) > 23 ) THEN
              CALL PRINT_MSG( NVERB_ERROR, 'IO', 'IO_File_add2list', &
                              'NOUT_COMPRESS_LOSSY_NSD must be in the [1..23] range' )
              NOUT_COMPRESS_LOSSY_NSD(IMI) = 7
            END IF
          CASE ( NF90_QUANTIZE_BITGROOM, NF90_QUANTIZE_GRANULARBR )
            ! For 32 bit reals, number of significant digits must be in the 1 to 7 range
            IF ( NOUT_COMPRESS_LOSSY_NSD(IMI) < 1 .OR. NOUT_COMPRESS_LOSSY_NSD(IMI) > 7 ) THEN
              CALL PRINT_MSG( NVERB_ERROR, 'IO', 'IO_File_add2list', &
                              'NOUT_COMPRESS_LOSSY_NSD must be in the [1..7] range' )
              NOUT_COMPRESS_LOSSY_NSD(IMI) = 3
            END IF
          CASE DEFAULT
            CALL PRINT_MSG( NVERB_FATAL, 'IO', 'IO_File_add2list', 'invalid NNCCOMPRESS_LOSSY_ALGO (internal fatal error)' )
        END SELECT
#elif (MNH_REAL == 8)
        IF ( TPFILE%LNCREDUCE_FLOAT_PRECISION ) THEN
          SELECT CASE ( TPFILE%NNCCOMPRESS_LOSSY_ALGO )
            CASE ( NF90_QUANTIZE_BITROUND )
              ! For 32 bit reals, number of significant bits must be in the 1 to 23 range
              IF ( NOUT_COMPRESS_LOSSY_NSD(IMI) < 1 .OR. NOUT_COMPRESS_LOSSY_NSD(IMI) > 23 ) THEN
                CALL PRINT_MSG( NVERB_ERROR, 'IO', 'IO_File_add2list', &
                                'NOUT_COMPRESS_LOSSY_NSD must be in the [1..23] range' )
                NOUT_COMPRESS_LOSSY_NSD(IMI) = 7
              END IF
            CASE ( NF90_QUANTIZE_BITGROOM, NF90_QUANTIZE_GRANULARBR )
              ! For 32 bit reals, number of significant digits must be in the 1 to 7 range
              IF ( NOUT_COMPRESS_LOSSY_NSD(IMI) < 1 .OR. NOUT_COMPRESS_LOSSY_NSD(IMI) > 7 ) THEN
                CALL PRINT_MSG( NVERB_ERROR, 'IO', 'IO_File_add2list', &
                                'NOUT_COMPRESS_LOSSY_NSD must be in the [1..7] range' )
                NOUT_COMPRESS_LOSSY_NSD(IMI) = 3
              END IF
            CASE DEFAULT
              CALL PRINT_MSG( NVERB_FATAL, 'IO', 'IO_File_add2list', 'invalid NNCCOMPRESS_LOSSY_ALGO (internal fatal error)' )
          END SELECT
        ELSE
          SELECT CASE ( TPFILE%NNCCOMPRESS_LOSSY_ALGO )
            CASE ( NF90_QUANTIZE_BITROUND )
              ! For 64 bit reals, number of significant bits must be in the 1 to 52 range
              IF ( NOUT_COMPRESS_LOSSY_NSD(IMI) < 1 .OR. NOUT_COMPRESS_LOSSY_NSD(IMI) > 52 ) THEN
                CALL PRINT_MSG( NVERB_ERROR, 'IO', 'IO_File_add2list', &
                                'NOUT_COMPRESS_LOSSY_NSD must be in the [1..52] range' )
                NOUT_COMPRESS_LOSSY_NSD(IMI) = 7
              END IF
            CASE ( NF90_QUANTIZE_BITGROOM, NF90_QUANTIZE_GRANULARBR )
              ! For 64 bit reals, number of significant digits must be in the 1 to 15 range
              IF ( NOUT_COMPRESS_LOSSY_NSD(IMI) < 1 .OR. NOUT_COMPRESS_LOSSY_NSD(IMI) > 15 ) THEN
                CALL PRINT_MSG( NVERB_ERROR, 'IO', 'IO_File_add2list', &
                                'NOUT_COMPRESS_LOSSY_NSD must be in the [1..15] range')
                NOUT_COMPRESS_LOSSY_NSD(IMI) = 3
              END IF
            CASE DEFAULT
              CALL PRINT_MSG( NVERB_FATAL, 'IO', 'IO_File_add2list', 'invalid NNCCOMPRESS_LOSSY_ALGO (internal fatal error)' )
          END SELECT
        END IF
#else
#error "Invalid MNH_REAL"
#endif
        TPFILE%NNCCOMPRESS_LOSSY_NSD = NOUT_COMPRESS_LOSSY_NSD(IMI)
      END IF
#endif
    ELSE IF (TRIM(HTYPE)=='MNHBACKUP' .OR. TRIM(HTYPE)=='MNHDIACHRONIC') THEN
      TPFILE%LNCREDUCE_FLOAT_PRECISION = LBAK_REDUCE_FLOAT_PRECISION(IMI)
      IF ( LBAK_REDUCE_FLOAT_PRECISION(IMI) ) THEN
        IF ( .NOT. LIO_ALLOW_REDUCED_PRECISION_BACKUP ) THEN
          cmnhmsg(1) = 'LBAK_REDUCE_FLOAT_PRECISION=T is dangerous'
          cmnhmsg(2) = 'if needed, it must be forced with LIO_ALLOW_REDUCED_PRECISION_BACKUP=T in NAM_CONFIO'
          CALL PRINT_MSG( NVERB_ERROR, 'IO', 'IO_File_add2list' )
          TPFILE%LNCREDUCE_FLOAT_PRECISION = .FALSE.
        ELSE
          IF ( .NOT. ASSOCIATED( TPFILE%TMAINFILE ) ) THEN
            ! Do not print warning for subfiles
            CALL PRINT_MSG( NVERB_WARNING, 'IO', 'IO_File_add2list', trim(tpfile%cname) // &
                            ' LBAK_REDUCE_FLOAT_PRECISION=T dangerous (forced by LIO_ALLOW_REDUCED_PRECISION_BACKUP=T)' )
          END IF
        END IF
      END IF

      ! Apply compression to backup files if not already forced for all
      IF ( .NOT. LIO_COMPRESS ) THEN
        TPFILE%LNCCOMPRESS       = LBAK_COMPRESS(IMI)
        IF ( NBAK_COMPRESS_LEVEL(IMI) < 0 .OR. NBAK_COMPRESS_LEVEL(IMI) > 9 ) THEN
          CALL PRINT_MSG( NVERB_ERROR, 'IO', 'IO_File_add2list', &
                          'NBAK_COMPRESS_LEVEL must be in the [0..9] range' )
          NBAK_COMPRESS_LEVEL(IMI) = 4
        END IF
        TPFILE%NNCCOMPRESS_LEVEL = NBAK_COMPRESS_LEVEL(IMI)
      END IF
    ELSE IF ( TRIM(HTYPE) == 'MNHDIAG' ) THEN
      TPFILE%LNCREDUCE_FLOAT_PRECISION = LDIAG_REDUCE_FLOAT_PRECISION
    END IF
    !
    IF ( TRIM(HTYPE) == 'MNHBACKUP' .OR. TRIM(HTYPE) == 'MNHOUTPUT' ) THEN
      IF( PRESENT(TPDADFILE) ) THEN
        CALL PRINT_MSG( NVERB_FATAL, 'IO', 'IO_File_add2list', &
                         'TPDADFILE should not be provided for backup or output file ' // TRIM(HNAME) )
      END IF

      ! Find dad file (not for the subfiles)
      IF ( .NOT. ASSOCIATED(TPFILE%TMAINFILE) ) THEN
        TPFILE%TDADFILE => NULL()
        ! Security check (if it happens, this part of the code should be exported outside of the IMI loop)
        IF ( NDAD(IMI) > IMI ) CALL PRINT_MSG( NVERB_FATAL, 'IO', 'IO_File_add2list', 'NDAD(IMI)>IMI' )
        IF ( NDAD(IMI) == IMI .OR.  IMI == 1 ) THEN
          TPFILE%TDADFILE => TPFILE !Points to itself
        ELSE
          ! Try to find the dad file: it must be of the same type (MNHBACKUP/MNHOUTPUT), to the dad and at the same time
          IMULT = NINT( DYN_MODEL(NDAD(IMI))%XTSTEP / DYN_MODEL(IMI)%XTSTEP )
          TZFILE => TFILE_FIRST
          DO WHILE ( ASSOCIATED(TZFILE) )
            IF ( TZFILE%CTYPE == TPFILE%CTYPE .AND. TZFILE%NMODEL == NDAD(IMI) ) THEN
              ! Check if at same time
              IF ( TPFILE%NSTEP == ( TZFILE%NSTEP - 1 ) * IMULT + 1 ) THEN
                TPFILE%TDADFILE => TZFILE
                EXIT
              END IF
            END IF
            TZFILE => TZFILE%TFILE_NEXT
          END DO
        END IF
      ELSE
        ! Subfile
        TPFILE%TDADFILE => NULL()
      END IF
    ELSE
      IF(PRESENT(TPDADFILE)) THEN
        IF (.NOT.ASSOCIATED(TPDADFILE)) CALL PRINT_MSG(NVERB_WARNING,'IO','IO_File_add2list', &
                                                       'TPDADFILE provided but not associated for file '//TRIM(HNAME))
        TPFILE%TDADFILE => TPDADFILE
      ELSE
        TPFILE%TDADFILE => NULL()
      END IF
    END IF


  CASE default
    call print_msg(NVERB_FATAL,'IO','IO_File_add2list','invalid type '//trim(tpfile%ctype)//' for file '//trim(hname))
END SELECT
!
TPFILE%LOPENED = .FALSE.
TPFILE%NOPEN   = 0
TPFILE%NCLOSE  = 0

NFILE_STAT_NADD    = NFILE_STAT_NADD    + 1
NFILE_STAT_CURSIZE = NFILE_STAT_CURSIZE + 1
NFILE_STAT_MAXSIZE = MAX( NFILE_STAT_MAXSIZE, NFILE_STAT_CURSIZE )

END SUBROUTINE IO_File_add2list


RECURSIVE SUBROUTINE IO_File_remove_from_list( TPFILE )
  ! Remove a file from the file list and free its ressources

#ifdef MNH_IOCDF4
  USE MODE_IO_TOOLS_NC4, ONLY: IO_Iocdf_dealloc_nc4
#endif

  TYPE(TFILEDATA), POINTER, INTENT(INOUT) :: TPFILE    !File structure to return

  INTEGER :: JF

  IF ( .NOT.ASSOCIATED(TPFILE) ) THEN
    CALL PRINT_MSG( NVERB_ERROR, 'IO', 'IO_File_remove_from_list', 'trying to remove a non existing file' )
    RETURN
  END IF

  CALL PRINT_MSG( NVERB_DEBUG, 'IO', 'IO_File_remove_from_list', 'called for ' // TRIM(TPFILE%CNAME) )

  ! Check if the file is opened. If it is, print an error
  ! Do not do the close here, because there will be a circular dependency with the MODE_IO_FILE module
  IF ( TPFILE%LOPENED ) THEN
    CALL PRINT_MSG( NVERB_ERROR, 'IO', 'IO_File_remove_from_list', TRIM(TPFILE%CNAME) // &
                    ': removing an opened file is not allowed' )
    RETURN
  END IF

  ! Print a warning if a file has never been opened
  IF ( TPFILE%NOPEN == 0 ) CALL PRINT_MSG( NVERB_WARNING, 'IO', 'IO_File_remove_from_list', TRIM(TPFILE%CNAME) // &
                                          ': never been opened' )

  ! Are there sub-files? If yes, remove them first
  ! Do it only if not already removed (ie in IO_File_close)
  IF ( TPFILE%NSUBFILES_IOZ > 0 .AND. ALLOCATED( TPFILE%TFILES_IOZ ) ) THEN
    DO JF = 1, TPFILE%NSUBFILES_IOZ
      CALL IO_File_remove_from_list( TPFILE%TFILES_IOZ(JF)%TFILE )
    END DO
  END IF

  ! Remove corresponding .des file
  IF ( ASSOCIATED( TPFILE%TDESFILE) ) CALL IO_File_remove_from_list( TPFILE%TDESFILE )

  ! Remove file from list
  IF ( ASSOCIATED(TPFILE%TFILE_PREV) ) THEN
    TPFILE%TFILE_PREV%TFILE_NEXT => TPFILE%TFILE_NEXT
  ELSE
    ! File was first in the list
    TFILE_FIRST => TPFILE%TFILE_NEXT
  END IF
  IF ( ASSOCIATED(TPFILE%TFILE_NEXT) ) THEN
    TPFILE%TFILE_NEXT%TFILE_PREV => TPFILE%TFILE_PREV
  ELSE
    ! File was last in the list
    TFILE_LAST => TPFILE%TFILE_PREV
  END IF

#ifdef MNH_IOCDF4
  IF ( ASSOCIATED( TPFILE%TNCDIMS ) ) THEN
    CALL PRINT_MSG( NVERB_WARNING, 'IO', 'IO_File_remove_from_list', TRIM(TPFILE%CNAME) // &
                    ': TNCDIMS should not be associated at this point' )
    CALL IO_Iocdf_dealloc_nc4( TPFILE%TNCDIMS )
  END IF
#endif

  ! Free file ressources
  DEALLOCATE( TPFILE )
  TPFILE => NULL()

  NFILE_STAT_NREM    = NFILE_STAT_NREM    + 1
  NFILE_STAT_CURSIZE = NFILE_STAT_CURSIZE - 1

END SUBROUTINE IO_File_remove_from_list


SUBROUTINE IO_File_find_byname(HNAME,TPFILE,KRESP,OOLD)
!
USE MODD_PARAMETERS, ONLY: NFILENAMELGTMAX
!
CHARACTER(LEN=*),       INTENT(IN)  :: HNAME  ! Name of the file to find
TYPE(TFILEDATA),POINTER,INTENT(OUT) :: TPFILE ! File structure to return
INTEGER,                INTENT(OUT) :: KRESP  ! Return value
LOGICAL, OPTIONAL,      INTENT(IN)  :: OOLD   ! FALSE if new file (should not be found)
                                              ! TRUE if file may be in the list
!
TYPE(TFILEDATA),POINTER :: TZFILE ! File structure
LOGICAL                 :: GOLD
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_File_find_byname','looking for '//TRIM(HNAME))
!
NULLIFY(TPFILE)
KRESP = 0
!
IF (PRESENT(OOLD)) THEN
  GOLD = OOLD
ELSE
  GOLD = .TRUE.
END IF
!
IF (LEN_TRIM(HNAME)>NFILENAMELGTMAX) &
  CALL PRINT_MSG(NVERB_WARNING,'IO','IO_File_find_byname','HNAME length is bigger than NFILENAMELGTMAX for '//TRIM(HNAME))
!
IF (.NOT.ASSOCIATED(TFILE_FIRST)) THEN
  IF (GOLD) CALL PRINT_MSG(NVERB_WARNING,'IO','IO_File_find_byname','filelist is empty')
ELSE
  !
  TZFILE => TFILE_FIRST
  !
  DO
    IF (TRIM(TZFILE%CNAME) == TRIM(HNAME(1:MIN(NFILENAMELGTMAX,LEN(HNAME)))) ) THEN
      TPFILE => TZFILE
      EXIT
    END IF
    IF (.NOT.ASSOCIATED(TZFILE%TFILE_NEXT)) EXIT
    TZFILE => TZFILE%TFILE_NEXT
  END DO
END IF
!
IF (.NOT.ASSOCIATED(TPFILE)) THEN
  CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_File_find_byname','file '//TRIM(HNAME)//' not found in list')
  KRESP = -1 !File not found
ELSE
  IF (GOLD) THEN
    CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_File_find_byname',TRIM(HNAME)//' was found')
  ELSE !File should not be found
    CALL PRINT_MSG(NVERB_ERROR,'IO','IO_File_find_byname',TRIM(HNAME)//' was found (unexpected)')
  END IF
END IF
!
END SUBROUTINE IO_File_find_byname
!
SUBROUTINE IO_Filelist_print(TPFILE_FIRST)
!
USE MODD_VAR_ll, ONLY : IP
!
TYPE(TFILEDATA),POINTER,OPTIONAL,INTENT(IN) :: TPFILE_FIRST
!
TYPE(TFILEDATA),POINTER :: TZFILE ! File structure
!
IF (IP/=1 .AND. .NOT.LVERB_ALLPRC) RETURN
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Filelist_print','called')
!
IF (PRESENT(TPFILE_FIRST)) THEN
  IF (.NOT.ASSOCIATED(TPFILE_FIRST)) RETURN
  TZFILE => TPFILE_FIRST
ELSE
  IF (.NOT.ASSOCIATED(TFILE_FIRST)) RETURN
  TZFILE => TFILE_FIRST
END IF
!
WRITE( *, '( /, "Filelist statistics:" )' )
WRITE( *, '( "  Number of files added:   ", I0 )' ) NFILE_STAT_NADD
WRITE( *, '( "  Number of files removed: ", I0 )' ) NFILE_STAT_NREM
WRITE( *, '( "  Current list size:       ", I0 )' ) NFILE_STAT_CURSIZE
WRITE( *, '( "  Maximum list size:       ", I0 )' ) NFILE_STAT_MAXSIZE
WRITE( *, '( /, "Current filelist" )' )
WRITE( *, '( A28," ",A13," ",A7," ",A7," ",A7," ",A7," ",A6," ",A6," ",A5," ",A6," ",A13," ",A13)' ) &
      'CNAME                       ', &
      'CTYPE        ','CFORMAT','CMODE  ','LOPENED','NLFIFLU','NNCID','NLU','NOPEN','NCLOSE','NOPEN_CURRENT','NSUBFILES_IOZ'
WRITE (*,'( A,A )') '--------------------------------------------------------------------------------------------------------', &
                    '------------------------'
WRITE (*,'(A28," ",A13," ",A7," ",A7," ",L7," ",I7," ",I6," ",I6," ",I5," ",I6," ",I13," ",I13)' ) &
      TZFILE%CNAME,TZFILE%CTYPE,TZFILE%CFORMAT,&
      TZFILE%CMODE,TZFILE%LOPENED,TZFILE%NLFIFLU,TZFILE%NNCID,TZFILE%NLU,TZFILE%NOPEN,TZFILE%NCLOSE,TZFILE%NOPEN_CURRENT,&
      TZFILE%NSUBFILES_IOZ
!
DO WHILE (ASSOCIATED(TZFILE%TFILE_NEXT))
  TZFILE => TZFILE%TFILE_NEXT
  WRITE (*,'(A28," ",A13," ",A7," ",A7," ",L7," ",I7," ",I6," ",I6," ",I5," ",I6," ",I13," ",I13)' ) &
        TZFILE%CNAME,TZFILE%CTYPE,TZFILE%CFORMAT,&
        TZFILE%CMODE,TZFILE%LOPENED,TZFILE%NLFIFLU,TZFILE%NNCID,TZFILE%NLU,TZFILE%NOPEN,TZFILE%NCLOSE,TZFILE%NOPEN_CURRENT,&
        TZFILE%NSUBFILES_IOZ
END DO
WRITE (*,'(/)')
!
END SUBROUTINE IO_Filelist_print
!
END MODULE MODE_IO_MANAGE_STRUCT
