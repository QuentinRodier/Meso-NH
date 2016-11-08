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
  IMPLICIT NONE
!
CONTAINS
!
!#########################################################################
SUBROUTINE IO_PREPARE_BAKOUT_STRUCT(KSUP,PTSTEP,PSEGLEN)
!#########################################################################
!
USE MODD_CONF
USE MODD_CONF_n
USE MODD_DYN,        ONLY : XSEGLEN
USE MODD_DYN_n,      ONLY : DYN_MODEL
USE MODD_FMOUT
USE MODD_IO_ll
USE MODD_IO_SURF_MNH,ONLY : IO_SURF_MNH_MODEL
USE MODD_NESTING,    ONLY : CDAD_NAME,NDAD
USE MODD_NSV,        ONLY: NSV
USE MODD_OUT_n,      ONLY : OUT_MODEL
USE MODD_VAR_ll,     ONLY : IP
!
IMPLICIT NONE
!
INTEGER, INTENT(IN) :: KSUP    ! supp. time steps
REAL,    INTENT(IN) :: PTSTEP  ! time step of model KMI
REAL,    INTENT(IN) :: PSEGLEN ! segment duration (in seconds)
!
INTEGER           :: IMI              ! Model number for loop
INTEGER           :: IBAK_NUMB, IOUT_NUMB ! Number of backups/outputs
INTEGER           :: ISTEP_MAX        ! Number of timesteps
INTEGER           :: IPOS             ! Index
INTEGER           :: JOUT,IDX         ! Loop indices
INTEGER, DIMENSION(:), ALLOCATABLE :: IBAK_STEP, IOUT_STEP
! Arrays to store list of backup/output steps (intermediate array)
CHARACTER (LEN=4) :: YDADNUMBER       ! Character string for the DAD model file number
!
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
  CALL POPULATE_STRUCT(TFILE_BAK_FIRST,TFILE_BAK_LAST,IBAK_STEP,"BACKUP",OUT_MODEL(IMI)%TBACKUPN)
  CALL POPULATE_STRUCT(TFILE_OUT_FIRST,TFILE_OUT_LAST,IOUT_STEP,"OUTPUT",OUT_MODEL(IMI)%TOUTPUTN)
  !
  !* Find dad output number
  !
  !Security check (if it happens, this part of the code should be exported outside of the IMI loop)
  IF (NDAD(IMI)>IMI) THEN
    print *,'ERROR in SET_GRID'
    STOP
  END IF
  IF (NDAD(IMI) == IMI .OR.  IMI == 1) THEN
    OUT_MODEL(IMI)%TBACKUPN(:)%NOUTDAD = 0
    DO IPOS = 1,OUT_MODEL(IMI)%NBAK_NUMB
      OUT_MODEL(IMI)%TBACKUPN(IPOS)%CDADFILENAME = OUT_MODEL(IMI)%TBACKUPN(IPOS)%TFILE%CNAME
    END DO
    OUT_MODEL(IMI)%TOUTPUTN(:)%NOUTDAD = 0
    DO IPOS = 1,OUT_MODEL(IMI)%NOUT_NUMB
      OUT_MODEL(IMI)%TOUTPUTN(IPOS)%CDADFILENAME = OUT_MODEL(IMI)%TOUTPUTN(IPOS)%TFILE%CNAME
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
        OUT_MODEL(IMI)%TBACKUPN(IPOS)%CDADFILENAME = ADJUSTL(ADJUSTR(CDAD_NAME(IMI))//YDADNUMBER)
      ELSE
        OUT_MODEL(IMI)%TBACKUPN(IPOS)%NOUTDAD = -1
        WRITE ( OUT_MODEL(IMI)%TBACKUPN(IPOS)%CDADFILENAME , FMT="('NO_DAD_FILE')" )
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
        OUT_MODEL(IMI)%TOUTPUTN(IPOS)%CDADFILENAME = ADJUSTL(ADJUSTR(CDAD_NAME(IMI))//YDADNUMBER)
      ELSE
        OUT_MODEL(IMI)%TOUTPUTN(IPOS)%NOUTDAD = -1
        WRITE ( OUT_MODEL(IMI)%TOUTPUTN(IPOS)%CDADFILENAME , FMT="('NO_DAD_FILE')" )
      END IF
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
    WRITE(*,'( I9 F12.3 )'  ) OUT_MODEL(IMI)%TBACKUPN(JOUT)%NSTEP,OUT_MODEL(IMI)%TBACKUPN(JOUT)%XTIME
  END DO
  PRINT *,'-------------------------'
  PRINT *,'Model number:      ',IMI
  PRINT *,'Number of outputs: ',IOUT_NUMB
  PRINT *,'Timestep     Time'
  DO JOUT = 1,IOUT_NUMB
    WRITE(*,'( I9 F12.3 )'  ) OUT_MODEL(IMI)%TOUTPUTN(JOUT)%NSTEP,OUT_MODEL(IMI)%TOUTPUTN(JOUT)%XTIME
  END DO
  PRINT *,'-------------------------'
  END IF
  !
END DO ! IMI=1,NMODEL
!
DEALLOCATE(NBAK_STEP)
DEALLOCATE(NOUT_STEP)
DEALLOCATE(XBAK_TIME)
DEALLOCATE(XOUT_TIME)
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
    IF (KIDX > JPOUTMAX) THEN
      PRINT *,'Error in SET_GRID when treating backup/output list (JPOUTMAX too small)'
      CALL ABORT
      STOP
    END IF
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
    IF (KIDX > JPOUTMAX) THEN
      PRINT *,'Error in SET_GRID when treating backup/output list (JPOUTMAX too small)'
      CALL ABORT
      STOP
    END IF
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
        print *,'WARNING: found duplicated backup/output step (removed extra one)'
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
  TYPE(TFILEDATA),           POINTER,INTENT(INOUT) :: TPFILE_FIRST,TPFILE_LAST
  INTEGER,DIMENSION(:),              INTENT(IN)    :: KSTEPS
  CHARACTER(LEN=*),                  INTENT(IN)    :: HFILETYPE
  TYPE(TOUTBAK),DIMENSION(:),POINTER,INTENT(OUT)   :: TPBAKOUTN
  !
  CHARACTER (LEN=4) :: YNUMBER          ! Character string for the file number
  !
  IPOS = 0
  DO JOUT = 1,SIZE(KSTEPS)
    IF (KSTEPS(JOUT) >= 0) THEN
        IPOS = IPOS + 1
        TPBAKOUTN(IPOS)%NID = IPOS
        TPBAKOUTN(IPOS)%NSTEP = KSTEPS(JOUT)
        TPBAKOUTN(IPOS)%XTIME = (KSTEPS(JOUT)-1)*DYN_MODEL(IMI)%XTSTEP
        IF (IPOS>999) THEN
          print *,'ERROR in SET_GRID: more than 999 backups/outputs'
          STOP
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
        WRITE (YNUMBER,FMT="('.',I3.3)") IPOS
        TPBAKOUTN(IPOS)%TFILE%CNAME=ADJUSTL(ADJUSTR(IO_SURF_MNH_MODEL(IMI)%COUTFILE)//YNUMBER)
        TPBAKOUTN(IPOS)%TFILE%NLFITYPE=1 !1: to be transfered
!PW: TODO: set NLFIVERB only when useful (only if LFI file...)
        TPBAKOUTN(IPOS)%TFILE%NLFIVERB=NVERB
        IF (LIOCDF4) THEN
          TPBAKOUTN(IPOS)%TFILE%CTYPE='NETCDF4'
          IF (LLFIOUT) THEN
            PRINT *,'Warning: LLFIOUT + LIOCDF4 = .TRUE. not yet implemented with new IO data structures'
            TPBAKOUTN(IPOS)%TFILE%NLFINPRAR= 22+2*(4+NRR+NSV)
          END IF
        ELSE IF (LLFIOUT) THEN
          TPBAKOUTN(IPOS)%TFILE%CTYPE='LFI'
          TPBAKOUTN(IPOS)%TFILE%NLFINPRAR= 22+2*(4+NRR+NSV)
        ELSE
          PRINT *,'Error: unknown backup/output fileformat'
          CALL ABORT
        ENDIF
    END IF
  END DO
END SUBROUTINE POPULATE_STRUCT
!
END SUBROUTINE IO_PREPARE_BAKOUT_STRUCT
!
END MODULE MODE_IO_MANAGE_STRUCT
