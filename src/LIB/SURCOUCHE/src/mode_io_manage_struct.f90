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
!
! Integers for file stats
INTEGER, SAVE :: NFILE_STAT_NADD    = 0 ! Number of files added to file list
INTEGER, SAVE :: NFILE_STAT_NREM    = 0 ! Number of files removed from file list
INTEGER, SAVE :: NFILE_STAT_CURSIZE = 0 ! Current number of files in file list
INTEGER, SAVE :: NFILE_STAT_MAXSIZE = 0 ! Maximum number of files in file list
!
CONTAINS
!
!#########################################################################
SUBROUTINE IO_Bakout_struct_prepare(KSUP,PTSTEP,PSEGLEN)
!#########################################################################
!
USE MODD_BAKOUT
USE MODD_CONF
USE MODD_CONF_n
USE MODD_DYN,        ONLY : XSEGLEN
USE MODD_DYN_n,      ONLY : DYN_MODEL
use modd_field,      only: tfieldlist
USE MODD_IO_SURF_MNH,ONLY : IO_SURF_MNH_MODEL
USE MODD_NESTING,    ONLY : NDAD
USE MODD_NSV,        ONLY: NSV
USE MODD_OUT_n,      ONLY : OUT_MODEL
USE MODD_VAR_ll,     ONLY : IP

use mode_field, only: Find_field_id_from_mnhname

IMPLICIT NONE
!
INTEGER, INTENT(IN) :: KSUP    ! supp. time steps
REAL,    INTENT(IN) :: PTSTEP  ! time step of model KMI
REAL,    INTENT(IN) :: PSEGLEN ! segment duration (in seconds)
!
INTEGER           :: IMI              ! Model number for loop
INTEGER           :: IBAK_NUMB, IOUT_NUMB ! Number of backups/outputs
INTEGER           :: IERR_LVL         ! Level of error message
INTEGER           :: IVAR             ! Number of variables
INTEGER           :: ISTEP_MAX        ! Number of timesteps
INTEGER           :: IPOS,IFIELD      ! Indices
INTEGER           :: JOUT,IDX         ! Loop indices
INTEGER           :: IRESP
INTEGER, DIMENSION(:), ALLOCATABLE :: IBAK_STEP, IOUT_STEP
! Arrays to store list of backup/output steps (intermediate array)
CHARACTER (LEN=4) :: YDADNUMBER       ! Character string for the DAD model file number
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
!
DO IMI = 1, NMODEL
  IBAK_NUMB = 0
  IOUT_NUMB = 0
  !Reduce XSEGLEN by time added to XSEGLEN for 1st domain (see set_grid subroutine)
  ISTEP_MAX = NINT( ( XSEGLEN - KSUP * DYN_MODEL(1)%XTSTEP ) / DYN_MODEL(IMI)%XTSTEP ) + 1

  ! Check that provided times are multiples of model timestep and not after end of segment
  DO JOUT = 1, NFILE_NUM_MAX
    IF ( XBAK_TIME(IMI,JOUT) >= 0.) THEN
      ZTSTEP_RND = NINT( XBAK_TIME(IMI,JOUT) / PTSTEP ) * PTSTEP
      IF ( ABS( ZTSTEP_RND - XBAK_TIME(IMI,JOUT) ) > 1.E-6 ) THEN
        CALL PRINT_MSG( NVERB_ERROR, 'IO', 'IO_Bakout_struct_prepare', 'XBAK_TIME is not a multiple of the model timestep' )
        XBAK_TIME(IMI,JOUT) = ZTSTEP_RND
      END IF
      IF ( XBAK_TIME(IMI,JOUT) > XSEGLEN + 1.E-6 ) THEN
        CALL PRINT_MSG( NVERB_WARNING, 'IO', 'IO_Bakout_struct_prepare', &
                        'XBAK_TIME after end of simulation time segment => ignored' )
        XBAK_TIME(IMI,JOUT) = XNEGUNDEF
      END IF
    END IF
    IF ( XOUT_TIME(IMI,JOUT) >= 0.) THEN
      ZTSTEP_RND = NINT( XOUT_TIME(IMI,JOUT) / PTSTEP ) * PTSTEP
      IF ( ABS( ZTSTEP_RND - XOUT_TIME(IMI,JOUT) ) > 1.E-6 ) THEN
        CALL PRINT_MSG( NVERB_ERROR, 'IO', 'IO_Bakout_struct_prepare', 'XOUT_TIME is not a multiple of the model timestep' )
        XOUT_TIME(IMI,JOUT) = ZTSTEP_RND
      END IF
      IF ( XOUT_TIME(IMI,JOUT) > XSEGLEN + 1.E-6 ) THEN
        CALL PRINT_MSG( NVERB_WARNING, 'IO', 'IO_Bakout_struct_prepare', &
                        'XOUT_TIME after end of simulation time segment => ignored' )
        XOUT_TIME(IMI,JOUT) = XNEGUNDEF
      END IF
    END IF
  END DO
  !
  !* Insert regular backups/outputs into XBAK_TIME/XOUT_TIME arrays
  !
  IF ( XBAK_TIME_FREQ(IMI) > 0. ) THEN
    IF ( NBAK_STEP_FREQ(IMI) > 0 )                                   &
      CALL PRINT_MSG( NVERB_ERROR, 'IO', 'IO_Bakout_struct_prepare', &
                      'XBAK_TIME_FREQ and NBAK_STEP_FREQ can not be provided simultaneously' )

    ! Check that frequency is at least equals to the model time step
    IF ( XBAK_TIME_FREQ(IMI) < PTSTEP - 1.E-6 ) THEN
      CALL PRINT_MSG( NVERB_ERROR, 'IO', 'IO_Bakout_struct_prepare', 'XBAK_TIME_FREQ smaller than model timestep' )
      XBAK_TIME_FREQ(IMI) = PTSTEP
    END IF

    ! Check that the frequency is a multiple of the model time step
    ZTSTEP_RND = NINT( XBAK_TIME_FREQ(IMI) / PTSTEP ) * PTSTEP
    IF ( ABS( ZTSTEP_RND - XBAK_TIME_FREQ(IMI) ) > 1.E-6 ) THEN
      CALL PRINT_MSG( NVERB_ERROR, 'IO', 'IO_Bakout_struct_prepare', 'XBAK_TIME_FREQ is not a multiple of the model timestep' )
      XBAK_TIME_FREQ(IMI) = ZTSTEP_RND
    END IF

    IF ( XBAK_TIME_FREQ_FIRST(IMI) > 0. ) THEN
      ! Check that the first write time of the series is a multiple of the model time step
      ZTSTEP_RND = NINT( XBAK_TIME_FREQ_FIRST(IMI) / PTSTEP ) * PTSTEP
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

    CALL IO_INSERT_REGULAR_FLOAT(XBAK_TIME_FREQ_FIRST(IMI),XBAK_TIME_FREQ(IMI),XBAK_TIME(IMI,:))
  END IF

  IF ( XOUT_TIME_FREQ(IMI) > 0. ) THEN
    IF ( NOUT_STEP_FREQ(IMI) > 0 )                                   &
      CALL PRINT_MSG( NVERB_ERROR, 'IO', 'IO_Bakout_struct_prepare', &
                      'XOUT_TIME_FREQ and NOUT_STEP_FREQ can not be provided simultaneously' )

    ! Check that frequency is at least equals to the model time step
    IF ( XOUT_TIME_FREQ(IMI) < PTSTEP - 1.E-6 ) THEN
      CALL PRINT_MSG( NVERB_ERROR, 'IO', 'IO_Bakout_struct_prepare', 'XOUT_TIME_FREQ smaller than model timestep' )
      XOUT_TIME_FREQ(IMI) = PTSTEP
    END IF

    ! Check that the frequency is a multiple of the model time step
    ZTSTEP_RND = NINT( XOUT_TIME_FREQ(IMI) / PTSTEP ) * PTSTEP
    IF ( ABS( ZTSTEP_RND - XOUT_TIME_FREQ(IMI) ) > 1.E-6 ) THEN
      CALL PRINT_MSG( NVERB_ERROR, 'IO', 'IO_Bakout_struct_prepare', 'XOUT_TIME_FREQ is not a multiple of the model timestep' )
      XOUT_TIME_FREQ(IMI) = ZTSTEP_RND
    END IF

    IF ( XOUT_TIME_FREQ_FIRST(IMI) > 0. ) THEN
      ! Check that the first write time of the series is a multiple of the model time step
      ZTSTEP_RND = NINT( XOUT_TIME_FREQ_FIRST(IMI) / PTSTEP ) * PTSTEP
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

    CALL IO_INSERT_REGULAR_FLOAT(XOUT_TIME_FREQ_FIRST(IMI),XOUT_TIME_FREQ(IMI),XOUT_TIME(IMI,:))
  END IF
  !
  !* Synchronization between nested models through XBAK_TIME/XOUT_TIME arrays
  !
  CALL IO_SYNC_MODELS_FLOAT(IBAK_NUMB,XBAK_TIME)
  CALL IO_SYNC_MODELS_FLOAT(IOUT_NUMB,XOUT_TIME)
  !
  !* Insert regular backups/outputs into NBAK_STEP/NOUT_STEP arrays
  !
  IF (NBAK_STEP_FREQ(IMI)>0) CALL IO_INSERT_REGULAR_INT(NBAK_STEP_FREQ_FIRST(IMI),NBAK_STEP_FREQ(IMI),NBAK_STEP(IMI,:))
  IF (NOUT_STEP_FREQ(IMI)>0) CALL IO_INSERT_REGULAR_INT(NOUT_STEP_FREQ_FIRST(IMI),NOUT_STEP_FREQ(IMI),NOUT_STEP(IMI,:))
  !
  !* Synchronization between nested models through NBAK_STEP/NOUT_STEP arrays
  !
  CALL IO_SYNC_MODELS_INT(IBAK_NUMB,NBAK_STEP)
  CALL IO_SYNC_MODELS_INT(IOUT_NUMB,NOUT_STEP)
  !
  !* Group all backups/outputs in a common form and add backups/outputs at beginning and end if requested
  !
  IF (LBAK_BEG) IBAK_NUMB = IBAK_NUMB + 1
  IF (LBAK_END) IBAK_NUMB = IBAK_NUMB + 1
  IF (LOUT_BEG) IOUT_NUMB = IOUT_NUMB + 1
  IF (LOUT_END) IOUT_NUMB = IOUT_NUMB + 1
  !
  ALLOCATE(IBAK_STEP(IBAK_NUMB))
  IBAK_STEP(:) = NNEGUNDEF
  ALLOCATE(IOUT_STEP(IOUT_NUMB))
  IOUT_STEP(:) = NNEGUNDEF
  !
  IBAK_NUMB = 0
  IOUT_NUMB = 0
  !
  IF (LBAK_BEG) THEN
    IBAK_NUMB = IBAK_NUMB + 1
    IBAK_STEP(IBAK_NUMB) = 1 ! 1 is the 1st step number
  END IF
  IF (LOUT_BEG) THEN
    IOUT_NUMB = IOUT_NUMB + 1
    IOUT_STEP(IOUT_NUMB) = 1 ! 1 is the 1st step number
  END IF
  !
  DO JOUT = 1, NFILE_NUM_MAX
    IF (XBAK_TIME(IMI,JOUT) >= 0.) THEN
      IBAK_NUMB = IBAK_NUMB + 1
      IBAK_STEP(IBAK_NUMB) = NINT(XBAK_TIME(IMI,JOUT)/DYN_MODEL(IMI)%XTSTEP) + 1
    END IF
    IF (XOUT_TIME(IMI,JOUT) >= 0.) THEN
      IOUT_NUMB = IOUT_NUMB + 1
      IOUT_STEP(IOUT_NUMB) = NINT(XOUT_TIME(IMI,JOUT)/DYN_MODEL(IMI)%XTSTEP) + 1
    END IF
  END DO
  !
  DO JOUT = 1, NFILE_NUM_MAX
    IF (NBAK_STEP(IMI,JOUT) > 0) THEN
      IBAK_NUMB = IBAK_NUMB + 1
      IBAK_STEP(IBAK_NUMB) = NBAK_STEP(IMI,JOUT)
    END IF
    IF (NOUT_STEP(IMI,JOUT) > 0) THEN
      IOUT_NUMB = IOUT_NUMB + 1
      IOUT_STEP(IOUT_NUMB) = NOUT_STEP(IMI,JOUT)
    END IF
  END DO
  !
  IF (LBAK_END) THEN
    IBAK_NUMB = IBAK_NUMB + 1
    IBAK_STEP(IBAK_NUMB) = ISTEP_MAX
  END IF
  IF (LOUT_END) THEN
    IOUT_NUMB = IOUT_NUMB + 1
    IOUT_STEP(IOUT_NUMB) = ISTEP_MAX
  END IF
  !
  !* Find and remove duplicated entries
  !
  CALL FIND_REMOVE_DUPLICATES(IBAK_NUMB,IBAK_STEP)
  CALL FIND_REMOVE_DUPLICATES(IOUT_NUMB,IOUT_STEP)
  !
  !* Find and remove out of time range entries
  !
  CALL FIND_REMOVE_OUTOFTIMERANGE(IBAK_NUMB,IBAK_STEP)
  CALL FIND_REMOVE_OUTOFTIMERANGE(IOUT_NUMB,IOUT_STEP)
  !
  !* Sort entries
  !
  CALL SORT_ENTRIES(IBAK_NUMB,IBAK_STEP)
  CALL SORT_ENTRIES(IOUT_NUMB,IOUT_STEP)
  !
  !* Count the number of backups/outputs of model IMI
  !
  IBAK_NUMB = 0
  DO JOUT = 1,SIZE(IBAK_STEP)
    IF (IBAK_STEP(JOUT) >= 0) THEN
      IBAK_NUMB = IBAK_NUMB + 1
    END IF
  END DO
  IF (IBAK_NUMB==0) THEN
    IF(LIO_ALLOW_NO_BACKUP) THEN
      IERR_LVL = NVERB_WARNING
    ELSE
      IERR_LVL = NVERB_ERROR
    END IF
    CALL PRINT_MSG(IERR_LVL,'IO','IO_Bakout_struct_prepare','no (valid) backup time')
  END IF
  !
  IOUT_NUMB = 0
  DO JOUT = 1,SIZE(IOUT_STEP)
    IF (IOUT_STEP(JOUT) >= 0) THEN
      IOUT_NUMB = IOUT_NUMB + 1
    END IF
  END DO
  !
  OUT_MODEL(IMI)%NBAK_NUMB = IBAK_NUMB
  OUT_MODEL(IMI)%NOUT_NUMB = IOUT_NUMB
  !
  !* Populate the backup/output data structures
  !
  ALLOCATE(OUT_MODEL(IMI)%TBACKUPN(IBAK_NUMB))
  ALLOCATE(OUT_MODEL(IMI)%TOUTPUTN(IOUT_NUMB))
  !
  CALL POPULATE_STRUCT(TFILE_FIRST,TFILE_LAST,IBAK_STEP,"MNHBACKUP",OUT_MODEL(IMI)%TBACKUPN,IMI)
  CALL POPULATE_STRUCT(TFILE_FIRST,TFILE_LAST,IOUT_STEP,"MNHOUTPUT",OUT_MODEL(IMI)%TOUTPUTN,IMI)
  !
  !* Find dad output number
  !
  !Security check (if it happens, this part of the code should be exported outside of the IMI loop)
  IF (NDAD(IMI)>IMI) CALL PRINT_MSG(NVERB_FATAL,'IO','IO_Bakout_struct_prepare','NDAD(IMI)>IMI')
  IF (NDAD(IMI) == IMI .OR.  IMI == 1) THEN
    OUT_MODEL(IMI)%TBACKUPN(:)%NOUTDAD = 0
    DO IPOS = 1,OUT_MODEL(IMI)%NBAK_NUMB
      OUT_MODEL(IMI)%TBACKUPN(IPOS)%TFILE%TDADFILE => OUT_MODEL(IMI)%TBACKUPN(IPOS)%TFILE !Points to itself
    END DO
    OUT_MODEL(IMI)%TOUTPUTN(:)%NOUTDAD = 0
    DO IPOS = 1,OUT_MODEL(IMI)%NOUT_NUMB
      OUT_MODEL(IMI)%TOUTPUTN(IPOS)%TFILE%TDADFILE => OUT_MODEL(IMI)%TOUTPUTN(IPOS)%TFILE !Points to itself
    END DO
  ELSE
    DO IPOS = 1,OUT_MODEL(IMI)%NBAK_NUMB
      IDX = 0
      DO JOUT = 1,OUT_MODEL(NDAD(IMI))%NBAK_NUMB
        IF ( OUT_MODEL(NDAD(IMI))%TBACKUPN(JOUT)%XTIME <= OUT_MODEL(IMI)%TBACKUPN(IPOS)%XTIME+1.E-6 ) THEN
          IDX = JOUT
        ELSE
          EXIT
        END IF
      END DO
      IF (IDX>0) THEN
        OUT_MODEL(IMI)%TBACKUPN(IPOS)%NOUTDAD = IDX
        WRITE (YDADNUMBER,FMT="('.',I3.3)") OUT_MODEL(IMI)%TBACKUPN(IPOS)%NOUTDAD
        OUT_MODEL(IMI)%TBACKUPN(IPOS)%TFILE%TDADFILE => OUT_MODEL(NDAD(IMI))%TBACKUPN(IDX)%TFILE
      ELSE
        OUT_MODEL(IMI)%TBACKUPN(IPOS)%NOUTDAD = -1
        NULLIFY(OUT_MODEL(IMI)%TBACKUPN(IPOS)%TFILE%TDADFILE) !No dad file
      END IF
    END DO
    DO IPOS = 1,OUT_MODEL(IMI)%NOUT_NUMB
      IDX = 0
      DO JOUT = 1,OUT_MODEL(NDAD(IMI))%NOUT_NUMB
        IF ( OUT_MODEL(NDAD(IMI))%TOUTPUTN(JOUT)%XTIME <= OUT_MODEL(IMI)%TOUTPUTN(IPOS)%XTIME+1.E-6 ) THEN
          IDX = JOUT
        ELSE
          EXIT
        END IF
      END DO
      IF (IDX>0) THEN
        OUT_MODEL(IMI)%TOUTPUTN(IPOS)%NOUTDAD = IDX
        WRITE (YDADNUMBER,FMT="('.',I3.3)") OUT_MODEL(IMI)%TOUTPUTN(IPOS)%NOUTDAD
        OUT_MODEL(IMI)%TOUTPUTN(IPOS)%TFILE%TDADFILE => OUT_MODEL(NDAD(IMI))%TOUTPUTN(IDX)%TFILE
      ELSE
        OUT_MODEL(IMI)%TOUTPUTN(IPOS)%NOUTDAD = -1
        NULLIFY(OUT_MODEL(IMI)%TOUTPUTN(IPOS)%TFILE%TDADFILE) !No dad file
      END IF
    END DO
  END IF
  !
  !Determine the list of the fields to write in each output
  IF (IOUT_NUMB>0) THEN
    !Count the number of fields to output
    IVAR = 0
    DO IPOS = 1,JPOUTVARMAX
      IF (COUT_VAR(IMI,IPOS)/='') IVAR = IVAR + 1
    END DO
    IF (IVAR==0) CALL PRINT_MSG(NVERB_WARNING,'IO','IO_Bakout_struct_prepare','no fields chosen for output')
    ALLOCATE(OUT_MODEL(IMI)%TOUTPUTN(1)%NFIELDLIST(IVAR))

    if ( ivar > 0 ) then
      !Determine the list of the outputs to do (by field number)
      !Set the NFIELDLIST for the 1st output
      !All the others will use the same list (for the moment)
      IVAR = 0
      DO IPOS = 1,JPOUTVARMAX
        IF (COUT_VAR(IMI,IPOS)/='') THEN
          IVAR=IVAR+1
          CALL FIND_FIELD_ID_FROM_MNHNAME(COUT_VAR(IMI,IPOS),IFIELD,IRESP)
          OUT_MODEL(IMI)%TOUTPUTN(1)%NFIELDLIST(IVAR) = IFIELD
          IF (IRESP/=0) THEN
            CALL PRINT_MSG(NVERB_FATAL,'IO','IO_Bakout_struct_prepare','unknown field for output: '//TRIM(COUT_VAR(IMI,IPOS)))
            !MNH is killed to prevent problems with wrong values in NFIELDLIST
          END IF
          !
        END IF
      END DO
    end if

    !All the outputs use the same field list (for the moment)
    DO IPOS = 2,IOUT_NUMB
      OUT_MODEL(IMI)%TOUTPUTN(IPOS)%NFIELDLIST => OUT_MODEL(IMI)%TOUTPUTN(1)%NFIELDLIST
    END DO
  END IF  
  !
  DEALLOCATE(IBAK_STEP)
  DEALLOCATE(IOUT_STEP)
  !
  IF (IP==1) THEN
  PRINT *,'-------------------------------'
  PRINT *,'Model number:      ',IMI
  PRINT *,'Number of backups: ',IBAK_NUMB
  if ( ibak_numb > 0 ) then
    PRINT *,'Timestep     Time'
    DO JOUT = 1,IBAK_NUMB
      WRITE(*,'( I9,F12.3 )'  ) OUT_MODEL(IMI)%TBACKUPN(JOUT)%NSTEP,OUT_MODEL(IMI)%TBACKUPN(JOUT)%XTIME
    END DO
  end if
  PRINT *,'-------------------------------'
  PRINT *,'Model number:      ',IMI
  PRINT *,'Number of outputs: ',IOUT_NUMB
  if ( iout_numb > 0 ) then
    PRINT *,'Timestep     Time'
    DO JOUT = 1,IOUT_NUMB
      WRITE(*,'( I9,F12.3 )'  ) OUT_MODEL(IMI)%TOUTPUTN(JOUT)%NSTEP,OUT_MODEL(IMI)%TOUTPUTN(JOUT)%XTIME
    END DO
  end if
  !
  IF ( IOUT_NUMB>0 ) THEN
    IF ( SIZE(OUT_MODEL(IMI)%TOUTPUTN(1)%NFIELDLIST)>0 ) THEN
      PRINT *,'List of fields:'
      DO JOUT = 1,SIZE(OUT_MODEL(IMI)%TOUTPUTN(1)%NFIELDLIST)
        IDX = OUT_MODEL(IMI)%TOUTPUTN(1)%NFIELDLIST(JOUT)
        PRINT *,'  ',TRIM(TFIELDLIST(IDX)%CMNHNAME)
      END DO
    END IF
  END IF
  !
  PRINT *,'-------------------------------'
  END IF
  !
END DO ! IMI=1,NMODEL
!
DEALLOCATE(NBAK_STEP)
DEALLOCATE(NOUT_STEP)
DEALLOCATE(XBAK_TIME)
DEALLOCATE(XOUT_TIME)
DEALLOCATE(COUT_VAR)
!
CONTAINS
!
!#########################################################################
SUBROUTINE IO_INSERT_REGULAR_FLOAT(PFIRST,PFREQ,PTIMES)
!#########################################################################
  !
  REAL,              INTENT(IN)    :: PFIRST,PFREQ
  REAL,DIMENSION(:), INTENT(INOUT) :: PTIMES
  !
  REAL              :: ZOUT, ZOUTMAX    ! Time of output/backup
  !
  IDX = 1
  ZOUT = PFIRST
  ZOUTMAX = PSEGLEN - PTSTEP*KSUP
  DO WHILE ( ZOUT <= ZOUTMAX )
    CALL FIND_NEXT_AVAIL_SLOT_FLOAT(PTIMES,IDX)
    PTIMES(IDX) = ZOUT
    ZOUT = ZOUT + PFREQ
  END DO
END SUBROUTINE IO_INSERT_REGULAR_FLOAT
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
SUBROUTINE IO_SYNC_MODELS_FLOAT(KNUMB,PTIMES)
!#########################################################################
  !
  INTEGER,             INTENT(INOUT) :: KNUMB
  REAL,DIMENSION(:,:), INTENT(INOUT) :: PTIMES
  !
  INTEGER :: JKLOOP ! Loop index
  !
  DO JOUT = 1, NFILE_NUM_MAX
    IF (PTIMES(IMI,JOUT) >= 0.) THEN
      KNUMB = KNUMB + 1
      !Value is rounded to nearest timestep
      PTIMES(IMI,JOUT) = NINT(PTIMES(IMI,JOUT)/DYN_MODEL(IMI)%XTSTEP) * DYN_MODEL(IMI)%XTSTEP
      !Output/backup time is propagated to nested models (with higher numbers)
      DO JKLOOP = IMI+1,NMODEL
        IDX = 1
        CALL FIND_NEXT_AVAIL_SLOT_FLOAT(PTIMES(JKLOOP,:),IDX)
        PTIMES(JKLOOP,IDX) = PTIMES(IMI,JOUT)
      END DO
    END IF
  END DO
END SUBROUTINE IO_SYNC_MODELS_FLOAT
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
        KSTEPS(JKLOOP,IDX) = (KSTEPS(IMI,JOUT)-1) * NINT( DYN_MODEL(JKLOOP)%XTSTEP/DYN_MODEL(IMI)%XTSTEP ) + 1
      END DO
    END IF
  END DO
END SUBROUTINE IO_SYNC_MODELS_INT
!
!#########################################################################
SUBROUTINE FIND_NEXT_AVAIL_SLOT_FLOAT(PTIMES,kIDX)
!#########################################################################
  !
  REAL,DIMENSION(:), INTENT(IN)    :: PTIMES
  INTEGER,           INTENT(INOUT) :: KIDX
  !
  !Find next (starting from KIDX) non 'allocated' element
  DO WHILE ( PTIMES(KIDX) >= 0. )
    KIDX = KIDX + 1
    IF (KIDX > NFILE_NUM_MAX) CALL PRINT_MSG(NVERB_FATAL,'IO','FIND_NEXT_AVAIL_SLOT_FLOAT','NFILE_NUM_MAX too small')
  END DO
END SUBROUTINE FIND_NEXT_AVAIL_SLOT_FLOAT
!
!#########################################################################
SUBROUTINE FIND_NEXT_AVAIL_SLOT_INT(KSTEPS,KIDX)
!#########################################################################
  !
  INTEGER,DIMENSION(:), INTENT(IN)    :: KSTEPS
  INTEGER,              INTENT(INOUT) :: KIDX
  !
  !Find next (starting from KIDX) non 'allocated' element
  DO WHILE ( KSTEPS(IDX) >= 0 )
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
!#########################################################################
SUBROUTINE POPULATE_STRUCT(TPFILE_FIRST,TPFILE_LAST,KSTEPS,HFILETYPE,TPBAKOUTN,KMI)
!#########################################################################
  !
#ifdef MNH_IOCDF4
  USE NETCDF, ONLY: NF90_QUANTIZE_BITGROOM, NF90_QUANTIZE_BITROUND, NF90_QUANTIZE_GRANULARBR
#endif
  !
  USE MODD_CONFZ,      ONLY: NB_PROCIO_W
  !
  USE MODE_TOOLS,      ONLY: UPCASE
  !
  TYPE(TFILEDATA),           POINTER,INTENT(INOUT) :: TPFILE_FIRST,TPFILE_LAST
  INTEGER,DIMENSION(:),              INTENT(IN)    :: KSTEPS
  CHARACTER(LEN=*),                  INTENT(IN)    :: HFILETYPE
  TYPE(TOUTBAK),DIMENSION(:),POINTER,INTENT(IN)    :: TPBAKOUTN
  INTEGER,                           INTENT(IN)    :: KMI ! Model number
  !
  CHARACTER (LEN=:), ALLOCATABLE :: YNUMBER ! Character string for the file number
  INTEGER :: JI

  IF ( NFILE_NUM_MAX < 1000 ) THEN
    ALLOCATE( CHARACTER(LEN=3) :: YNUMBER )
  ELSE IF ( NFILE_NUM_MAX < 10000 ) THEN
    ALLOCATE( CHARACTER(LEN=4) :: YNUMBER )
  ELSE IF ( NFILE_NUM_MAX < 100000 ) THEN
    ALLOCATE( CHARACTER(LEN=5) :: YNUMBER )
  ELSE IF ( NFILE_NUM_MAX < 1000000 ) THEN
    ALLOCATE( CHARACTER(LEN=6) :: YNUMBER )
  ELSE
    CALL PRINT_MSG( NVERB_FATAL, 'IO', 'POPULATE_STRUCT', 'NFILE_NUM_MAX is too large' )
  END IF

  IPOS = 0
  DO JOUT = 1,SIZE(KSTEPS)
    IF (KSTEPS(JOUT) >= 0) THEN
        NFILE_STAT_NADD    = NFILE_STAT_NADD    + 1
        NFILE_STAT_CURSIZE = NFILE_STAT_CURSIZE + 1
        IPOS = IPOS + 1
        TPBAKOUTN(IPOS)%NID = IPOS
        TPBAKOUTN(IPOS)%NSTEP = KSTEPS(JOUT)
        TPBAKOUTN(IPOS)%XTIME = (KSTEPS(JOUT)-1)*DYN_MODEL(IMI)%XTSTEP
        IF (.NOT.ASSOCIATED(TPFILE_FIRST)) THEN
          ALLOCATE(TPFILE_FIRST)
          TPFILE_LAST => TPFILE_FIRST
        ELSE
          ALLOCATE(TPFILE_LAST%TFILE_NEXT)
          TPFILE_LAST%TFILE_NEXT%TFILE_PREV => TPFILE_LAST
          TPFILE_LAST => TPFILE_LAST%TFILE_NEXT
        ENDIF
        TPBAKOUTN(IPOS)%TFILE => TPFILE_LAST
        TPBAKOUTN(IPOS)%TFILE%CTYPE=HFILETYPE
        TPBAKOUTN(IPOS)%TFILE%CMODE="WRITE"
        IF ( NFILE_NUM_MAX  < 1000 ) THEN
          WRITE ( YNUMBER, FMT = "(I3.3)" ) IPOS
        ELSE IF ( NFILE_NUM_MAX  < 10000 ) THEN
          WRITE ( YNUMBER, FMT = "(I4.4)" ) IPOS
        ELSE IF ( NFILE_NUM_MAX  < 100000 ) THEN
          WRITE ( YNUMBER, FMT = "(I5.5)" ) IPOS
        ELSE IF ( NFILE_NUM_MAX  < 1000000 ) THEN
          WRITE ( YNUMBER, FMT = "(I6.6)" ) IPOS
        ELSE
          CALL PRINT_MSG( NVERB_FATAL, 'IO', 'POPULATE_STRUCT', 'NFILE_NUM_MAX is too large' )
        END IF

        IF (TRIM(HFILETYPE)=='MNHOUTPUT') THEN
          ! Add a "OUT" suffix for output files
          TPBAKOUTN(IPOS)%TFILE%CNAME=ADJUSTL(ADJUSTR(IO_SURF_MNH_MODEL(IMI)%COUTFILE)//'.OUT.'//YNUMBER)

#ifdef MNH_IOCDF4
          !Reduce the float precision if asked
          TPBAKOUTN(IPOS)%TFILE%LNCREDUCE_FLOAT_PRECISION = LOUT_REDUCE_FLOAT_PRECISION(IMI)

          !Set compression if asked
          TPBAKOUTN(IPOS)%TFILE%LNCCOMPRESS = LOUT_COMPRESS(IMI)
          IF ( NOUT_COMPRESS_LEVEL(IMI)<0 .OR. NOUT_COMPRESS_LEVEL(IMI)>9 ) THEN
            CALL PRINT_MSG( NVERB_ERROR, 'IO', 'POPULATE_STRUCT', &
                            'NOUT_COMPRESS_LEVEL must be in the [0..9] range' )
            NOUT_COMPRESS_LEVEL(IMI) = 4
          END IF
          TPBAKOUTN(IPOS)%TFILE%NNCCOMPRESS_LEVEL = NOUT_COMPRESS_LEVEL(IMI)

          !Set lossy compression
          TPBAKOUTN(IPOS)%TFILE%LNCCOMPRESS_LOSSY = LOUT_COMPRESS_LOSSY(IMI)
          IF ( LOUT_COMPRESS_LOSSY(IMI) ) THEN
            !Force compression if lossy compression is enabled
            TPBAKOUTN(IPOS)%TFILE%LNCCOMPRESS = .TRUE.

            !Set lossy compression algorithm
            SELECT CASE ( UPCASE( COUT_COMPRESS_LOSSY_ALGO(IMI) ) )
              CASE ( 'BITGROOM' )
                TPBAKOUTN(IPOS)%TFILE%NNCCOMPRESS_LOSSY_ALGO = NF90_QUANTIZE_BITGROOM
              CASE ( 'GRANULARBR' )
                TPBAKOUTN(IPOS)%TFILE%NNCCOMPRESS_LOSSY_ALGO = NF90_QUANTIZE_GRANULARBR
              CASE ( 'BITROUND' )
                TPBAKOUTN(IPOS)%TFILE%NNCCOMPRESS_LOSSY_ALGO = NF90_QUANTIZE_BITROUND
              CASE DEFAULT
                CMNHMSG(1) = 'invalid COUT_COMPRESS_LOSSY_ALGO'
                CMNHMSG(2) = 'Accepted algorithms: BITGROOM, GRANULARBR (default choice), BITROUND'
                CALL PRINT_MSG( NVERB_ERROR, 'IO', 'POPULATE_STRUCT' )
                TPBAKOUTN(IPOS)%TFILE%NNCCOMPRESS_LOSSY_ALGO = NF90_QUANTIZE_GRANULARBR
            END SELECT

            !Set number of significant digits/bits for lossy compression algorithm
#if (MNH_REAL == 4)
            SELECT CASE ( TPBAKOUTN(IPOS)%TFILE%NNCCOMPRESS_LOSSY_ALGO )
              CASE ( NF90_QUANTIZE_BITROUND )
                ! For 32 bit reals, number of significant bits must be in the 1 to 23 range
                IF ( NOUT_COMPRESS_LOSSY_NSD(IMI) < 1 .OR. NOUT_COMPRESS_LOSSY_NSD(IMI) > 23 ) THEN
                  CALL PRINT_MSG( NVERB_ERROR, 'IO', 'POPULATE_STRUCT', &
                                  'NOUT_COMPRESS_LOSSY_NSD must be in the [1..23] range' )
                  NOUT_COMPRESS_LOSSY_NSD(IMI) = 7
                END IF
              CASE ( NF90_QUANTIZE_BITGROOM, NF90_QUANTIZE_GRANULARBR )
                ! For 32 bit reals, number of significant digits must be in the 1 to 7 range
                IF ( NOUT_COMPRESS_LOSSY_NSD(IMI) < 1 .OR. NOUT_COMPRESS_LOSSY_NSD(IMI) > 7 ) THEN
                  CALL PRINT_MSG( NVERB_ERROR, 'IO', 'POPULATE_STRUCT', &
                                  'NOUT_COMPRESS_LOSSY_NSD must be in the [1..7] range' )
                  NOUT_COMPRESS_LOSSY_NSD(IMI) = 3
                END IF
              CASE DEFAULT
                CALL PRINT_MSG( NVERB_FATAL, 'IO', 'POPULATE_STRUCT', 'invalid NNCCOMPRESS_LOSSY_ALGO (internal fatal error)' )
            END SELECT
#elif (MNH_REAL == 8)
            IF ( TPBAKOUTN(IPOS)%TFILE%LNCREDUCE_FLOAT_PRECISION ) THEN
              SELECT CASE ( TPBAKOUTN(IPOS)%TFILE%NNCCOMPRESS_LOSSY_ALGO )
                CASE ( NF90_QUANTIZE_BITROUND )
                  ! For 32 bit reals, number of significant bits must be in the 1 to 23 range
                  IF ( NOUT_COMPRESS_LOSSY_NSD(IMI) < 1 .OR. NOUT_COMPRESS_LOSSY_NSD(IMI) > 23 ) THEN
                    CALL PRINT_MSG( NVERB_ERROR, 'IO', 'POPULATE_STRUCT', &
                                    'NOUT_COMPRESS_LOSSY_NSD must be in the [1..23] range' )
                    NOUT_COMPRESS_LOSSY_NSD(IMI) = 7
                  END IF
                CASE ( NF90_QUANTIZE_BITGROOM, NF90_QUANTIZE_GRANULARBR )
                  ! For 32 bit reals, number of significant digits must be in the 1 to 7 range
                  IF ( NOUT_COMPRESS_LOSSY_NSD(IMI) < 1 .OR. NOUT_COMPRESS_LOSSY_NSD(IMI) > 7 ) THEN
                    CALL PRINT_MSG( NVERB_ERROR, 'IO', 'POPULATE_STRUCT', &
                                    'NOUT_COMPRESS_LOSSY_NSD must be in the [1..7] range' )
                    NOUT_COMPRESS_LOSSY_NSD(IMI) = 3
                  END IF
                CASE DEFAULT
                  CALL PRINT_MSG( NVERB_FATAL, 'IO', 'POPULATE_STRUCT', 'invalid NNCCOMPRESS_LOSSY_ALGO (internal fatal error)' )
              END SELECT
            ELSE
              SELECT CASE ( TPBAKOUTN(IPOS)%TFILE%NNCCOMPRESS_LOSSY_ALGO )
                CASE ( NF90_QUANTIZE_BITROUND )
                  ! For 64 bit reals, number of significant bits must be in the 1 to 52 range
                  IF ( NOUT_COMPRESS_LOSSY_NSD(IMI) < 1 .OR. NOUT_COMPRESS_LOSSY_NSD(IMI) > 52 ) THEN
                    CALL PRINT_MSG( NVERB_ERROR, 'IO', 'POPULATE_STRUCT', &
                                    'NOUT_COMPRESS_LOSSY_NSD must be in the [1..52] range' )
                    NOUT_COMPRESS_LOSSY_NSD(IMI) = 7
                  END IF
                CASE ( NF90_QUANTIZE_BITGROOM, NF90_QUANTIZE_GRANULARBR )
                  ! For 64 bit reals, number of significant digits must be in the 1 to 15 range
                  IF ( NOUT_COMPRESS_LOSSY_NSD(IMI) < 1 .OR. NOUT_COMPRESS_LOSSY_NSD(IMI) > 15 ) THEN
                    CALL PRINT_MSG( NVERB_ERROR, 'IO', 'POPULATE_STRUCT', &
                                    'NOUT_COMPRESS_LOSSY_NSD must be in the [1..15] range')
                    NOUT_COMPRESS_LOSSY_NSD(IMI) = 3
                  END IF
                CASE DEFAULT
                  CALL PRINT_MSG( NVERB_FATAL, 'IO', 'POPULATE_STRUCT', 'invalid NNCCOMPRESS_LOSSY_ALGO (internal fatal error)' )
              END SELECT
            END IF
#else
#error "Invalid MNH_REAL"
#endif
            TPBAKOUTN(IPOS)%TFILE%NNCCOMPRESS_LOSSY_NSD = NOUT_COMPRESS_LOSSY_NSD(IMI)
          END IF
#endif

          !Set output directory
          IF (LEN_TRIM(COUT_DIR)>0) THEN
            TPBAKOUTN(IPOS)%TFILE%CDIRNAME=TRIM(COUT_DIR)
          ELSE IF (LEN_TRIM(CIO_DIR)>0) THEN
            TPBAKOUTN(IPOS)%TFILE%CDIRNAME=TRIM(CIO_DIR)
          END IF
        ELSE IF (TRIM(HFILETYPE)=='MNHBACKUP') THEN
          TPBAKOUTN(IPOS)%TFILE%CNAME=ADJUSTL(ADJUSTR(IO_SURF_MNH_MODEL(IMI)%COUTFILE)//'.'//YNUMBER)
          IF (LEN_TRIM(CBAK_DIR)>0) THEN
            TPBAKOUTN(IPOS)%TFILE%CDIRNAME=TRIM(CBAK_DIR)
          ELSE IF (LEN_TRIM(CIO_DIR)>0) THEN
            TPBAKOUTN(IPOS)%TFILE%CDIRNAME=TRIM(CIO_DIR)
          END IF
        ELSE
          CALL PRINT_MSG(NVERB_FATAL,'IO','POPULATE_STRUCT','unknown filetype ('//TRIM(HFILETYPE)//')')
        ENDIF
        TPBAKOUTN(IPOS)%TFILE%NLFITYPE=1 !1: to be transferred
        TPBAKOUTN(IPOS)%TFILE%NLFIVERB=NVERB
        IF (LIOCDF4) THEN
          IF (.NOT.LLFIOUT) THEN
            TPBAKOUTN(IPOS)%TFILE%CFORMAT='NETCDF4'
          ELSE
            TPBAKOUTN(IPOS)%TFILE%CFORMAT='LFICDF4'
            IF (TRIM(HFILETYPE)=='MNHBACKUP') TPBAKOUTN(IPOS)%TFILE%NLFINPRAR= 22+2*(4+NRR+NSV)
          END IF
        ELSE IF (LLFIOUT) THEN
          TPBAKOUTN(IPOS)%TFILE%CFORMAT='LFI'
          IF (TRIM(HFILETYPE)=='MNHBACKUP') TPBAKOUTN(IPOS)%TFILE%NLFINPRAR= 22+2*(4+NRR+NSV)
        ELSE
          CALL PRINT_MSG(NVERB_FATAL,'IO','POPULATE_STRUCT','unknown backup/output fileformat')
        ENDIF
        !
        TPBAKOUTN(IPOS)%TFILE%NMODEL = KMI
        !
        IF (NB_PROCIO_W>1) THEN
          TPBAKOUTN(IPOS)%TFILE%NSUBFILES_IOZ = NB_PROCIO_W
          !Remark: sub-files are automatically added/removed when the backup/output file is opened/closed
          !Therefore, no need to do this here
        ELSE
          TPBAKOUTN(IPOS)%TFILE%NSUBFILES_IOZ = 0
        END IF
        !
    END IF
  END DO
  NFILE_STAT_MAXSIZE = MAX( NFILE_STAT_MAXSIZE, NFILE_STAT_CURSIZE )
END SUBROUTINE POPULATE_STRUCT
!
END SUBROUTINE IO_Bakout_struct_prepare
!
SUBROUTINE IO_File_add2list(TPFILE,HNAME,HTYPE,HMODE,                 &
                            HFORM,HACCESS,HFORMAT,HDIRNAME,           &
                            KLFINPRAR,KLFITYPE,KLFIVERB,KRECL,KMODEL, &
                            TPDADFILE,TPDATAFILE,OOLD,OSPLIT_IOZ)
!
USE MODD_BAKOUT,         ONLY: LOUT_COMPRESS,LOUT_REDUCE_FLOAT_PRECISION,NOUT_COMPRESS_LEVEL
USE MODD_CONF,           ONLY: CPROGRAM
use modd_confz,          only: nb_procio_r,nb_procio_w
!
USE MODE_MODELN_HANDLER, ONLY: GET_CURRENT_MODEL_INDEX
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
TYPE(TFILEDATA),POINTER,OPTIONAL,INTENT(IN)    :: TPDADFILE !Corresponding dad file
TYPE(TFILEDATA),POINTER,OPTIONAL,INTENT(IN)    :: TPDATAFILE!Corresponding data file (used only for DES files)
LOGICAL,                OPTIONAL,INTENT(IN)    :: OOLD      !FALSE if new file (should not be found)
                                                            !TRUE if the file could already be in the list
                                                            !     (add it only if not yet present)
logical,                optional,intent(in)    :: osplit_ioz !Is the file split vertically
!
INTEGER :: IMI,IRESP
INTEGER(KIND=LFIINT) :: ILFINPRAR
INTEGER :: ILFITYPE
INTEGER :: ILFIVERB
LOGICAL :: GOLD
logical :: gsplit_ioz
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
    IF (TRIM(HTYPE)=='MNHOUTPUT') THEN
      TPFILE%LNCREDUCE_FLOAT_PRECISION = LOUT_REDUCE_FLOAT_PRECISION(IMI)
      TPFILE%LNCCOMPRESS               = LOUT_COMPRESS(IMI)
      TPFILE%NNCCOMPRESS_LEVEL         = NOUT_COMPRESS_LEVEL(IMI)
    END IF
    !
    IF(PRESENT(TPDADFILE)) THEN
      IF (.NOT.ASSOCIATED(TPDADFILE)) CALL PRINT_MSG(NVERB_WARNING,'IO','IO_File_add2list', &
                                                     'TPDADFILE provided but not associated for file '//TRIM(HNAME))
      TPFILE%TDADFILE => TPDADFILE
    ELSE
      TPFILE%TDADFILE => NULL()
    END IF


  CASE default
    call print_msg(NVERB_FATAL,'IO','IO_File_add2list','invalid type '//trim(tpfile%ctype)//' for file '//trim(hname))
END SELECT
!
IF(PRESENT(KMODEL)) TPFILE%NMODEL = KMODEL
!
TPFILE%LOPENED = .FALSE.
TPFILE%NOPEN   = 0
TPFILE%NCLOSE  = 0
!
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
