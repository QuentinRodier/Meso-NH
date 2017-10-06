!MNH_LIC Copyright 1994-2016 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!-----------------------------------------------------------------
!!    Authors
!!    -------
!
!     P. Wautelet : 2016: original version
!
MODULE MODE_IO_MANAGE_STRUCT
!
USE MODD_IO_ll
USE MODE_MSG
!
IMPLICIT NONE
!
CONTAINS
!
!#########################################################################
SUBROUTINE IO_PREPARE_BAKOUT_STRUCT(KSUP,PTSTEP,PSEGLEN)
!#########################################################################
!
USE MODD_BAKOUT
USE MODD_CONF
USE MODD_CONF_n
USE MODD_DYN,        ONLY : XSEGLEN
USE MODD_DYN_n,      ONLY : DYN_MODEL
USE MODD_IO_SURF_MNH,ONLY : IO_SURF_MNH_MODEL
USE MODD_NESTING,    ONLY : CDAD_NAME,NDAD
USE MODD_NSV,        ONLY: NSV
USE MODD_OUT_n,      ONLY : OUT_MODEL
USE MODD_VAR_ll,     ONLY : IP
USE MODE_FIELD
!
IMPLICIT NONE
!
INTEGER, INTENT(IN) :: KSUP    ! supp. time steps
REAL,    INTENT(IN) :: PTSTEP  ! time step of model KMI
REAL,    INTENT(IN) :: PSEGLEN ! segment duration (in seconds)
!
INTEGER           :: IMI              ! Model number for loop
INTEGER           :: IBAK_NUMB, IOUT_NUMB ! Number of backups/outputs
INTEGER           :: IVAR             ! Number of variables
INTEGER           :: ISTEP_MAX        ! Number of timesteps
INTEGER           :: IPOS,IFIELD      ! Indices
INTEGER           :: JOUT,IDX         ! Loop indices
INTEGER           :: IRESP
INTEGER, DIMENSION(:), ALLOCATABLE :: IBAK_STEP, IOUT_STEP
! Arrays to store list of backup/output steps (intermediate array)
CHARACTER (LEN=4) :: YDADNUMBER       ! Character string for the DAD model file number
!
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_PREPARE_BAKOUT_STRUCT','called')
!
DO IMI = 1, NMODEL
  IBAK_NUMB = 0
  IOUT_NUMB = 0
  ISTEP_MAX = NINT(XSEGLEN/DYN_MODEL(IMI)%XTSTEP)+1
  IF (IMI == 1) ISTEP_MAX = ISTEP_MAX - KSUP
  !
  !* Insert regular backups/outputs into XBAK_TIME/XOUT_TIME arrays
  !
  IF (XBAK_TIME_FREQ(IMI)>0.) CALL IO_INSERT_REGULAR_FLOAT(XBAK_TIME_FREQ_FIRST(IMI),XBAK_TIME_FREQ(IMI),XBAK_TIME(IMI,:))
  IF (XOUT_TIME_FREQ(IMI)>0.) CALL IO_INSERT_REGULAR_FLOAT(XOUT_TIME_FREQ_FIRST(IMI),XOUT_TIME_FREQ(IMI),XOUT_TIME(IMI,:))
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
  DO JOUT = 1,JPOUTMAX
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
  DO JOUT = 1,JPOUTMAX
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
  CALL POPULATE_STRUCT(TFILE_FIRST,TFILE_LAST,IBAK_STEP,"BACKUP",OUT_MODEL(IMI)%TBACKUPN)
  CALL POPULATE_STRUCT(TFILE_FIRST,TFILE_LAST,IOUT_STEP,"OUTPUT",OUT_MODEL(IMI)%TOUTPUTN)
  !
  !* Find dad output number
  !
  !Security check (if it happens, this part of the code should be exported outside of the IMI loop)
  IF (NDAD(IMI)>IMI) CALL PRINT_MSG(NVERB_FATAL,'IO','IO_PREPARE_BAKOUT_STRUCT','NDAD(IMI)>IMI')
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
        OUT_MODEL(IMI)%TOUTPUTN(IPOS)%TFILE%TDADFILE => OUT_MODEL(NDAD(IMI))%TBACKUPN(IDX)%TFILE
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
    IF (IVAR==0) CALL PRINT_MSG(NVERB_ERROR,'IO','IO_PREPARE_BAKOUT_STRUCT','no fields chosen for output')
    ALLOCATE(OUT_MODEL(IMI)%TOUTPUTN(1)%NFIELDLIST(IVAR))
    !Determine the list of the outputs to do (by field number)
    IVAR = 1
    !Set the NFIELDLIST for the 1st output
    !All the others will use the same list (for the moment)
    DO IPOS = 1,JPOUTVARMAX
      IF (COUT_VAR(IMI,IPOS)/='') THEN
        CALL FIND_FIELD_ID_FROM_MNHNAME(COUT_VAR(IMI,IPOS),IFIELD,IRESP)
        OUT_MODEL(IMI)%TOUTPUTN(1)%NFIELDLIST(IVAR) = IFIELD
        IF (IRESP/=0) THEN
          CALL PRINT_MSG(NVERB_FATAL,'IO','IO_PREPARE_BAKOUT_STRUCT','unknown field for output: '//TRIM(COUT_VAR(IMI,IPOS)))
          !MNH is killed to prevent problems with wrong values in NFIELDLIST
        END IF
        !
        IVAR=IVAR+1
      END IF
    END DO
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
  PRINT *,'-------------------------'
  PRINT *,'Model number:      ',IMI
  PRINT *,'Number of backups: ',IBAK_NUMB
  PRINT *,'Timestep     Time'
  DO JOUT = 1,IBAK_NUMB
    WRITE(*,'( I9,F12.3 )'  ) OUT_MODEL(IMI)%TBACKUPN(JOUT)%NSTEP,OUT_MODEL(IMI)%TBACKUPN(JOUT)%XTIME
  END DO
  PRINT *,'-------------------------'
  PRINT *,'Model number:      ',IMI
  PRINT *,'Number of outputs: ',IOUT_NUMB
  PRINT *,'Timestep     Time'
  DO JOUT = 1,IOUT_NUMB
    WRITE(*,'( I9,F12.3 )'  ) OUT_MODEL(IMI)%TOUTPUTN(JOUT)%NSTEP,OUT_MODEL(IMI)%TOUTPUTN(JOUT)%XTIME
  END DO
  !
  IF (IOUT_NUMB>0) THEN
    PRINT *,'Field list:'
    DO JOUT = 1,SIZE(OUT_MODEL(IMI)%TOUTPUTN(1)%NFIELDLIST)
      IDX=OUT_MODEL(IMI)%TOUTPUTN(1)%NFIELDLIST(JOUT)
      PRINT *,'  ',TRIM(TFIELDLIST(IDX)%CMNHNAME)
    END DO
  END IF
  !
  PRINT *,'-------------------------'
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
  DO JOUT = 1,JPOUTMAX
    IF (PTIMES(IMI,JOUT) >= 0.) THEN
      KNUMB = KNUMB + 1
      !Value is rounded to nearest timestep
      PTIMES(IMI,JOUT) = NINT(PTIMES(IMI,JOUT)/DYN_MODEL(IMI)%XTSTEP) * DYN_MODEL(IMI)%XTSTEP
      !Output/backup time is propagated to nested models (with higher numbers)
      !PW: TODO: BUG?: what happens if 2 dissociated models? Use NSON(:) array?
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
  DO JOUT = 1,JPOUTMAX
    IF (KSTEPS(IMI,JOUT) > 0) THEN
      KNUMB = KNUMB + 1
      !Output/backup time is propagated to nested models (with higher numbers)
      !PW: TODO: BUG?: what happens if 2 dissociated models? Use NSON(:) array?
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
    IF (KIDX > JPOUTMAX) CALL PRINT_MSG(NVERB_FATAL,'IO','FIND_NEXT_AVAIL_SLOT_FLOAT','JPOUTMAX too small')
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
    IF (KIDX > JPOUTMAX) CALL PRINT_MSG(NVERB_FATAL,'IO','FIND_NEXT_AVAIL_SLOT_INT','JPOUTMAX too small')
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
        CALL PRINT_MSG(NVERB_INFO,'IO','FIND_REMOVE_DUPLICATES','found duplicated backup/output step (removed extra one)')
        KSTEPS(JKLOOP) = NNEGUNDEF
      END IF
    END DO
  END DO
END SUBROUTINE FIND_REMOVE_DUPLICATES
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
SUBROUTINE POPULATE_STRUCT(TPFILE_FIRST,TPFILE_LAST,KSTEPS,HFILETYPE,TPBAKOUTN)
!#########################################################################
  !
  USE MODD_CONFZ, ONLY: NB_PROCIO_W
  !
  TYPE(TFILEDATA),           POINTER,INTENT(INOUT) :: TPFILE_FIRST,TPFILE_LAST
  INTEGER,DIMENSION(:),              INTENT(IN)    :: KSTEPS
  CHARACTER(LEN=*),                  INTENT(IN)    :: HFILETYPE
  TYPE(TOUTBAK),DIMENSION(:),POINTER,INTENT(OUT)   :: TPBAKOUTN
  !
  CHARACTER (LEN=3) :: YNUMBER          ! Character string for the file number
  INTEGER :: JI
  !
  IPOS = 0
  DO JOUT = 1,SIZE(KSTEPS)
    IF (KSTEPS(JOUT) >= 0) THEN
        IPOS = IPOS + 1
        TPBAKOUTN(IPOS)%NID = IPOS
        TPBAKOUTN(IPOS)%NSTEP = KSTEPS(JOUT)
        TPBAKOUTN(IPOS)%XTIME = (KSTEPS(JOUT)-1)*DYN_MODEL(IMI)%XTSTEP
        IF (IPOS>999) THEN
          CALL PRINT_MSG(NVERB_FATAL,'IO','POPULATE_STRUCT','more than 999 backups/outputs')
        END IF
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
        WRITE (YNUMBER,FMT="(I3.3)") IPOS
        IF (TRIM(HFILETYPE)=='OUTPUT') THEN
          ! Add a "OUT" suffix for output files
          TPBAKOUTN(IPOS)%TFILE%CNAME=ADJUSTL(ADJUSTR(IO_SURF_MNH_MODEL(IMI)%COUTFILE)//'.OUT.'//YNUMBER)
          !Reduce the float precision if asked
          TPBAKOUTN(IPOS)%TFILE%LNCREDUCE_FLOAT_PRECISION = LOUT_REDUCE_FLOAT_PRECISION(IMI)
          !Set compression if asked
          TPBAKOUTN(IPOS)%TFILE%LNCCOMPRESS = LOUT_COMPRESS(IMI)
          IF ( NOUT_COMPRESS_LEVEL(IMI)<0 .OR. NOUT_COMPRESS_LEVEL(IMI)>9 ) THEN
            CALL PRINT_MSG(NVERB_WARNING,'IO','POPULATE_STRUCT',&
                           'NOUT_COMPRESS_LEVEL must be in the [0..9] range. Value forced to 4')
            NOUT_COMPRESS_LEVEL(IMI) = 4
          END IF
          TPBAKOUTN(IPOS)%TFILE%NNCCOMPRESS_LEVEL = NOUT_COMPRESS_LEVEL(IMI)
        ELSE IF (TRIM(HFILETYPE)=='BACKUP') THEN
          TPBAKOUTN(IPOS)%TFILE%CNAME=ADJUSTL(ADJUSTR(IO_SURF_MNH_MODEL(IMI)%COUTFILE)//'.'//YNUMBER)
        ELSE
          CALL PRINT_MSG(NVERB_FATAL,'IO','POPULATE_STRUCT','unknown filetype ('//TRIM(HFILETYPE)//')')
        ENDIF
        TPBAKOUTN(IPOS)%TFILE%NLFITYPE=1 !1: to be transfered
!PW: TODO: set NLFIVERB only when useful (only if LFI file...)
        TPBAKOUTN(IPOS)%TFILE%NLFIVERB=NVERB
        IF (LIOCDF4) THEN
          IF (.NOT.LLFIOUT) THEN
            TPBAKOUTN(IPOS)%TFILE%CFORMAT='NETCDF4'
          ELSE
            TPBAKOUTN(IPOS)%TFILE%CFORMAT='LFICDF4'
            IF (TRIM(HFILETYPE)=='BACKUP') TPBAKOUTN(IPOS)%TFILE%NLFINPRAR= 22+2*(4+NRR+NSV)
          END IF
        ELSE IF (LLFIOUT) THEN
          TPBAKOUTN(IPOS)%TFILE%CFORMAT='LFI'
          IF (TRIM(HFILETYPE)=='BACKUP') TPBAKOUTN(IPOS)%TFILE%NLFINPRAR= 22+2*(4+NRR+NSV)
        ELSE
          CALL PRINT_MSG(NVERB_FATAL,'IO','POPULATE_STRUCT','unknown backup/output fileformat')
        ENDIF
        !
        !Create file structures if Z-splitted files
        IF (NB_PROCIO_W>1) THEN
          ALLOCATE(TPBAKOUTN(IPOS)%TFILE_IOZ(NB_PROCIO_W))
          IF (NB_PROCIO_W>999) THEN
            CALL PRINT_MSG(NVERB_FATAL,'IO','POPULATE_STRUCT','more than 999 z-levels')
          END IF
          DO JI = 1,NB_PROCIO_W
            ALLOCATE(TPFILE_LAST%TFILE_NEXT)
            TPFILE_LAST%TFILE_NEXT%TFILE_PREV => TPFILE_LAST
            TPFILE_LAST => TPFILE_LAST%TFILE_NEXT
            TPBAKOUTN(IPOS)%TFILE_IOZ(JI)%TFILE => TPFILE_LAST
            TPBAKOUTN(IPOS)%TFILE_IOZ(JI)%TFILE%CTYPE=HFILETYPE
            TPBAKOUTN(IPOS)%TFILE_IOZ(JI)%TFILE%CMODE="WRITE"
            WRITE (YNUMBER,FMT="(I3.3)") JI
            TPBAKOUTN(IPOS)%TFILE_IOZ(JI)%TFILE%CNAME = TRIM(TPBAKOUTN(IPOS)%TFILE%CNAME)//'.Z'//YNUMBER
            IF (TRIM(HFILETYPE)=='OUTPUT') THEN
              !Reduce the float precision if asked
              TPBAKOUTN(IPOS)%TFILE_IOZ(JI)%TFILE%LNCREDUCE_FLOAT_PRECISION = LOUT_REDUCE_FLOAT_PRECISION(IMI)
              !Set compression if asked
              TPBAKOUTN(IPOS)%TFILE_IOZ(JI)%TFILE%LNCCOMPRESS = LOUT_COMPRESS(IMI)
              IF ( NOUT_COMPRESS_LEVEL(IMI)<0 .OR. NOUT_COMPRESS_LEVEL(IMI)>9 ) THEN
                PRINT *,'ERROR: NOUT_COMPRESS_LEVEL must be in the [0..9] range. Value forced to 4'
                NOUT_COMPRESS_LEVEL(IMI) = 4
              END IF
              TPBAKOUTN(IPOS)%TFILE_IOZ(JI)%TFILE%NNCCOMPRESS_LEVEL = NOUT_COMPRESS_LEVEL(IMI)
            END IF
            TPBAKOUTN(IPOS)%TFILE_IOZ(JI)%TFILE%NLFITYPE=1 !1: to be transfered
!PW: TODO: set NLFIVERB only when useful (only if LFI file...)
            TPBAKOUTN(IPOS)%TFILE_IOZ(JI)%TFILE%NLFIVERB=NVERB
            IF (LIOCDF4) THEN
              IF (.NOT.LLFIOUT) THEN
                TPBAKOUTN(IPOS)%TFILE_IOZ(JI)%TFILE%CFORMAT='NETCDF4'
              ELSE
                TPBAKOUTN(IPOS)%TFILE_IOZ(JI)%TFILE%CFORMAT='LFICDF4'
              END IF
            ELSE IF (LLFIOUT) THEN
              TPBAKOUTN(IPOS)%TFILE_IOZ(JI)%TFILE%CFORMAT='LFI'
              !TPBAKOUTN(IPOS)%TFILE_IOZ(JI)%TFILE%NLFINPRAR= 0
            ELSE
              CALL PRINT_MSG(NVERB_FATAL,'IO','POPULATE_STRUCT','unknown backup/output fileformat')
            ENDIF
          END DO
        END IF
        !
    END IF
  END DO
END SUBROUTINE POPULATE_STRUCT
!
END SUBROUTINE IO_PREPARE_BAKOUT_STRUCT
!
SUBROUTINE IO_FILE_ADD2LIST(TPFILE,HNAME,HTYPE,HMODE,KLFINPRAR,KLFITYPE,KLFIVERB,TPDADFILE,OOLD)
!
USE MODD_BAKOUT,         ONLY : LOUT_COMPRESS,LOUT_REDUCE_FLOAT_PRECISION,NOUT_COMPRESS_LEVEL
USE MODE_MODELN_HANDLER, ONLY : GET_CURRENT_MODEL_INDEX
!
TYPE(TFILEDATA),POINTER,         INTENT(INOUT) :: TPFILE    !File structure to return
CHARACTER(LEN=*),                INTENT(IN)    :: HNAME     !Filename
CHARACTER(LEN=*),                INTENT(IN)    :: HTYPE     !Filetype (backup, output, prepidealcase...)
CHARACTER(LEN=*),                INTENT(IN)    :: HMODE     !Opening mode (read, write...)
INTEGER,                OPTIONAL,INTENT(IN)    :: KLFINPRAR !Number of predicted articles of the LFI file (non crucial)
INTEGER,                OPTIONAL,INTENT(IN)    :: KLFITYPE  !Type of the file (used to generate list of files to transfers)
INTEGER,                OPTIONAL,INTENT(IN)    :: KLFIVERB  !LFI verbosity level
TYPE(TFILEDATA),POINTER,OPTIONAL,INTENT(IN)    :: TPDADFILE !Corresponding dad file
LOGICAL,                OPTIONAL,INTENT(IN)    :: OOLD      !FALSE if new file (should not be found)
                                                            !TRUE if the file could already be in the list
                                                            !     (add it only if not yet present)
!
INTEGER :: IMI,IRESP
INTEGER :: ILFINPRAR
INTEGER :: ILFITYPE
INTEGER :: ILFIVERB
LOGICAL :: GOLD
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_FILE_ADD2LIST','called for '//TRIM(HNAME))
!
IF (PRESENT(OOLD)) THEN
  GOLD = OOLD
ELSE
  GOLD = .FALSE. !By default, we assume file is not yet in list
END IF
!
IF (ASSOCIATED(TPFILE)) THEN
  IF (GOLD) THEN
    CALL PRINT_MSG(NVERB_INFO,'IO','IO_FILE_ADD2LIST','file '//TRIM(HNAME)//' already associated. Pointer will be overwritten')
    TPFILE => NULL()
  ELSE
    CALL PRINT_MSG(NVERB_FATAL,'IO','IO_FILE_ADD2LIST','file '//TRIM(HNAME)//' already associated')
  END IF
END IF
!
CALL IO_FILE_FIND_BYNAME(HNAME,TPFILE,IRESP,OOLD=GOLD)
IF (IRESP==0) THEN
  IF (.NOT.GOLD) THEN
    CALL PRINT_MSG(NVERB_ERROR,'IO','IO_FILE_ADD2LIST','file '//TRIM(HNAME)//' already in filelist')
  ELSE
    CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_FILE_ADD2LIST','file '//TRIM(HNAME)//' already in filelist (not unexpected)')
  END IF
  RETURN
END IF
!
IMI = GET_CURRENT_MODEL_INDEX()
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
IF (TRIM(HMODE)/='READ' .AND. TRIM(HMODE)/='WRITE') THEN
  CALL PRINT_MSG(NVERB_FATAL,'IO','IO_FILE_ADD2LIST','unknown mode ('//TRIM(HMODE)//') for file '//TRIM(HNAME))
END IF
!
TPFILE%CMODE = HMODE
!
IF (TRIM(HTYPE)/='OUTPUTLISTING') THEN
  IF (TRIM(HMODE)=='READ') THEN
    IF (LLFIREAD) THEN
      TPFILE%CFORMAT = 'LFI'
      TPFILE%NLFINPRAR = ILFINPRAR
    ELSE IF (LIOCDF4) THEN
      TPFILE%CFORMAT = 'NETCDF4'
    ELSE
      CALL PRINT_MSG(NVERB_FATAL,'IO','IO_FILE_ADD2LIST','invalid format for file '//TRIM(HNAME))
    END IF
  ELSE IF (TRIM(HMODE)=='WRITE') THEN
    IF (LLFIOUT .AND. LIOCDF4) THEN
      TPFILE%CFORMAT = 'LFICDF4'
      TPFILE%NLFINPRAR = ILFINPRAR
    ELSE IF (LIOCDF4) THEN
      TPFILE%CFORMAT = 'NETCDF4'
    ELSE IF (LLFIOUT) THEN
      TPFILE%CFORMAT = 'LFI'
      TPFILE%NLFINPRAR = ILFINPRAR
    ELSE
      CALL PRINT_MSG(NVERB_FATAL,'IO','IO_FILE_ADD2LIST','invalid format for file '//TRIM(HNAME))
    END IF
  END IF
  !
  TPFILE%NLFITYPE = ILFITYPE
  TPFILE%NLFIVERB = ILFIVERB
  !
  IF (TRIM(HTYPE)=='OUTPUT') THEN
    TPFILE%LNCREDUCE_FLOAT_PRECISION = LOUT_REDUCE_FLOAT_PRECISION(IMI)
    TPFILE%LNCCOMPRESS               = LOUT_COMPRESS(IMI)
    TPFILE%NNCCOMPRESS_LEVEL         = NOUT_COMPRESS_LEVEL(IMI)
  END IF
  !
  IF(PRESENT(TPDADFILE)) THEN
    IF (.NOT.ASSOCIATED(TPDADFILE)) CALL PRINT_MSG(NVERB_WARNING,'IO','IO_FILE_ADD2LIST','TPDADFILE provided but not associated')
    TPFILE%TDADFILE => TPDADFILE
  ELSE
    TPFILE%TDADFILE => NULL()
  END IF
  !
ELSE
  !OUTPUTLISTING files
  IF (TRIM(HMODE)/='WRITE') &
    CALL PRINT_MSG(NVERB_ERROR,'IO','IO_FILE_ADD2LIST','invalid mode '//TRIM(HMODE)//' for file '//TRIM(HNAME))
  TPFILE%CFORMAT = 'OUTPUTLISTING'
END IF
!
TPFILE%LOPENED = .FALSE.
TPFILE%NOPEN   = 0
TPFILE%NCLOSE  = 0
!
END SUBROUTINE IO_FILE_ADD2LIST
!
SUBROUTINE IO_FILE_FIND_BYNAME(HNAME,TPFILE,KRESP,OOLD)
!
CHARACTER(LEN=*),       INTENT(IN)  :: HNAME  ! Name of the file to find
TYPE(TFILEDATA),POINTER,INTENT(OUT) :: TPFILE ! File structure to return
INTEGER,                INTENT(OUT) :: KRESP  ! Return value
LOGICAL, OPTIONAL,      INTENT(IN)  :: OOLD   ! FALSE if new file (should not be found)
!
TYPE(TFILEDATA),POINTER :: TZFILE ! File structure
LOGICAL                 :: GOLD
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_FILE_FIND_BYNAME','looking for: '//TRIM(HNAME))
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
IF (.NOT.ASSOCIATED(TFILE_FIRST)) THEN
  IF (GOLD) CALL PRINT_MSG(NVERB_WARNING,'IO','IO_FILE_FIND_BYNAME','filelist is empty')
ELSE
  !
  TZFILE => TFILE_FIRST
  !
  DO
    IF (TRIM(TZFILE%CNAME) == TRIM(HNAME) ) THEN
      TPFILE => TZFILE
      EXIT
    END IF
    IF (.NOT.ASSOCIATED(TZFILE%TFILE_NEXT)) EXIT
    TZFILE => TZFILE%TFILE_NEXT
  END DO
END IF
!
IF (.NOT.ASSOCIATED(TPFILE)) THEN
  IF (GOLD) CALL PRINT_MSG(NVERB_WARNING,'IO','IO_FILE_FIND_BYNAME','file '//TRIM(HNAME)//' not found in list')
  KRESP = -1 !File not found
ELSE
  CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_FILE_FIND_BYNAME',TRIM(HNAME)//' was found')
END IF  
!
END SUBROUTINE IO_FILE_FIND_BYNAME
!
SUBROUTINE IO_FILE_PRINT_LIST(TPFILE_FIRST)
!
TYPE(TFILEDATA),POINTER,OPTIONAL,INTENT(IN) :: TPFILE_FIRST
!
TYPE(TFILEDATA),POINTER :: TZFILE ! File structure
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_FILE_PRINT_LIST','called')
!
IF (PRESENT(TPFILE_FIRST)) THEN
  IF (.NOT.ASSOCIATED(TPFILE_FIRST)) RETURN
  TZFILE => TPFILE_FIRST
ELSE
  IF (.NOT.ASSOCIATED(TFILE_FIRST)) RETURN
  TZFILE => TFILE_FIRST
END IF
!
WRITE (*,'(A28," ",A13," ",A7," ",A7," ",L1," ",I6," ",I6," ",I6," ",I3," ",I3)' ) TZFILE%CNAME,TZFILE%CTYPE,TZFILE%CFORMAT,&
      TZFILE%CMODE,TZFILE%LOPENED,TZFILE%NLFIFLU,TZFILE%NNCID,TZFILE%NLU,TZFILE%NOPEN,TZFILE%NCLOSE
!
DO WHILE (ASSOCIATED(TZFILE%TFILE_NEXT))
  TZFILE => TZFILE%TFILE_NEXT
  WRITE (*,'(A28," ",A13," ",A7," ",A7," ",L1," ",I6," ",I6," ",I6," ",I3," ",I3)' ) TZFILE%CNAME,TZFILE%CTYPE,TZFILE%CFORMAT,&
        TZFILE%CMODE,TZFILE%LOPENED,TZFILE%NLFIFLU,TZFILE%NNCID,TZFILE%NLU,TZFILE%NOPEN,TZFILE%NCLOSE
END DO
!
END SUBROUTINE IO_FILE_PRINT_LIST
!
END MODULE MODE_IO_MANAGE_STRUCT
