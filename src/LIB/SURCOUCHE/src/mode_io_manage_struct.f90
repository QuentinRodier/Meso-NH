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
INTEGER           :: IBAK_NUMB        ! Number of outputs
INTEGER           :: ISTEP_MAX        ! Number of timesteps
INTEGER           :: ITEMP            ! Intermediate variable
INTEGER           :: IPOS             ! Index
INTEGER           :: JKLOOP,JOUT,IDX  ! Loop indices
INTEGER, DIMENSION(:), ALLOCATABLE :: IBAK_STEP ! Array to store list of backup steps (intermediate array)
CHARACTER (LEN=4) :: YNUMBER          ! Character string for the file number
CHARACTER (LEN=4) :: YDADNUMBER       ! Character string for the DAD model file number
REAL              :: ZOUT, ZOUTMAX    ! Time of output/backup
!
!
DO IMI = 1, NMODEL
  IBAK_NUMB = 0
  ISTEP_MAX = NINT(XSEGLEN/DYN_MODEL(IMI)%XTSTEP)+1
  IF (IMI == 1) ISTEP_MAX = ISTEP_MAX - KSUP
  !
  !* Insert regular backups into XBAK_TIME array
  !
  IF (XBAK_TIME_FREQ(IMI)>0.) THEN
    IDX = 1
    ZOUT = XBAK_TIME_FREQ_FIRST(IMI)
    ZOUTMAX = PSEGLEN - PTSTEP*KSUP
    DO WHILE ( ZOUT <= ZOUTMAX )
      !Find first non 'allocated' element
      DO WHILE ( XBAK_TIME(IMI,IDX) >= 0. )
        IDX = IDX + 1
        IF (IDX > JPOUTMAX) THEN
          PRINT *,'Error in SET_GRID when treating output list (JPOUTMAX too small)'
          CALL ABORT
          STOP
        END IF
      END DO
      XBAK_TIME(IMI,IDX) = ZOUT
      ZOUT = ZOUT + XBAK_TIME_FREQ(IMI)
    END DO
  END IF
  !
  !* Synchronization between nested models through XBAK_TIME arrays (MODD_FMOUT)
  !
  DO JOUT = 1,JPOUTMAX
    IF (XBAK_TIME(IMI,JOUT) >= 0.) THEN
      IBAK_NUMB = IBAK_NUMB + 1
      !Value is rounded to nearest timestep
      XBAK_TIME(IMI,JOUT) = NINT(XBAK_TIME(IMI,JOUT)/DYN_MODEL(IMI)%XTSTEP) * DYN_MODEL(IMI)%XTSTEP
      !Output/backup time is propagated to nested models (with higher numbers)
      !PW: TODO: BUG?: what happens if 2 dissociated models? Use NSON(:) array?
      DO JKLOOP = IMI+1,NMODEL
        IDX = 1
        !Find first non 'allocated' element
        DO WHILE ( XBAK_TIME(JKLOOP,IDX) >= 0. )
          IDX = IDX + 1
          IF (IDX > JPOUTMAX) THEN
            PRINT *,'Error in SET_GRID when treating output list (JPOUTMAX too small)'
            CALL ABORT
            STOP
          END IF
        END DO
        XBAK_TIME(JKLOOP,IDX) = XBAK_TIME(IMI,JOUT)
      END DO
    END IF
  END DO
  !
  !* Insert regular backups into NBAK_STEP array
  !
  IF (NBAK_STEP_FREQ(IMI)>0) THEN
    IDX = 1
    DO JOUT = NBAK_STEP_FREQ_FIRST(IMI), ISTEP_MAX, NBAK_STEP_FREQ(IMI)
      !Find first non 'allocated' element
      DO WHILE ( NBAK_STEP(IMI,IDX) >= 0 )
        IDX = IDX + 1
        IF (IDX > JPOUTMAX) THEN
          PRINT *,'Error in SET_GRID when treating output list (JPOUTMAX too small)'
          CALL ABORT
          STOP
        END IF
      END DO
      NBAK_STEP(IMI,IDX) = JOUT
    END DO
  END IF
  !
  !* Synchronization between nested models through NBAK_STEP arrays (MODD_FMOUT)
  !
  DO JOUT = 1,JPOUTMAX
    IF (NBAK_STEP(IMI,JOUT) > 0) THEN
      IBAK_NUMB = IBAK_NUMB + 1
      !Output/backup time is propagated to nested models (with higher numbers)
      !PW: TODO: BUG?: what happens if 2 dissociated models? Use NSON(:) array?
      DO JKLOOP = IMI+1,NMODEL
        IDX = 1
        !Find first non 'allocated' element
        DO WHILE ( NBAK_STEP(JKLOOP,IDX) >= 0 )
          IDX = IDX + 1
        END DO
        IF (IDX > JPOUTMAX) THEN
          PRINT *,'Error in SET_GRID when treating output list (JPOUTMAX too small)'
          CALL ABORT
          STOP
        END IF
        ! Use of NINT and real to prevent rounding errors
        ! (STEP-1)* ... +1 because step numbers begin at 1
        NBAK_STEP(JKLOOP,IDX) = (NBAK_STEP(IMI,JOUT)-1) * NINT( DYN_MODEL(JKLOOP)%XTSTEP/DYN_MODEL(IMI)%XTSTEP ) + 1
      END DO
    END IF
  END DO
  !
  !* Group all backups in a common form and add backups at beginning and end if requested
  !
  IF (LBAK_BEG) IBAK_NUMB = IBAK_NUMB + 1
  IF (LBAK_END) IBAK_NUMB = IBAK_NUMB + 1
  !
  ALLOCATE(IBAK_STEP(IBAK_NUMB))
  IBAK_STEP(:) = NNEGUNDEF
  !
  IBAK_NUMB = 0
  !
  IF (LBAK_BEG) THEN
    IBAK_NUMB = IBAK_NUMB + 1
    IBAK_STEP(IBAK_NUMB) = 1 ! 1 is the 1st step number
  END IF
  !
  DO JOUT = 1,JPOUTMAX
    IF (XBAK_TIME(IMI,JOUT) >= 0.) THEN
      IBAK_NUMB = IBAK_NUMB + 1
      IBAK_STEP(IBAK_NUMB) = NINT(XBAK_TIME(IMI,JOUT)/DYN_MODEL(IMI)%XTSTEP) + 1
    END IF
  END DO
  !
  DO JOUT = 1,JPOUTMAX
    IF (NBAK_STEP(IMI,JOUT) > 0) THEN
      IBAK_NUMB = IBAK_NUMB + 1
      IBAK_STEP(IBAK_NUMB) = NBAK_STEP(IMI,JOUT)
    END IF
  END DO
  !
  IF (LBAK_END) THEN
    IBAK_NUMB = IBAK_NUMB + 1
    IBAK_STEP(IBAK_NUMB) = ISTEP_MAX
  END IF
  !
  !* Find and remove duplicated entries
  !
  DO JOUT = 1,IBAK_NUMB
    DO JKLOOP = JOUT+1,IBAK_NUMB
      IF ( IBAK_STEP(JKLOOP) == IBAK_STEP(JOUT) .AND. IBAK_STEP(JKLOOP) > 0 ) THEN
        print *,'WARNING: found duplicated backup step (removed extra one)'
        IBAK_STEP(JKLOOP) = NNEGUNDEF
      END IF
    END DO
  END DO
  !
  !* Sort entries
  !
  DO JOUT = 1,IBAK_NUMB
    ITEMP = IBAK_STEP(JOUT)
    IF (ITEMP<=0) ITEMP = HUGE(ITEMP)
    IPOS = -1
    DO JKLOOP = JOUT+1,IBAK_NUMB
      IF ( IBAK_STEP(JKLOOP) < ITEMP .AND. IBAK_STEP(JKLOOP) >= 0 ) THEN
        ITEMP = IBAK_STEP(JKLOOP)
        IPOS = JKLOOP
      END IF
    END DO
    IF (IPOS >= JOUT) THEN
      IBAK_STEP(IPOS) = IBAK_STEP(JOUT)
      IBAK_STEP(JOUT) = ITEMP
    END IF
  END DO
  !
  !* Count the number of backups of model IMI
  !
  IBAK_NUMB = 0
  DO JOUT = 1,SIZE(IBAK_STEP)
    IF (IBAK_STEP(JOUT) >= 0) THEN
      IBAK_NUMB = IBAK_NUMB + 1
    END IF
  END DO
  !
  OUT_MODEL(IMI)%NOUT_NUMB = IBAK_NUMB
  ALLOCATE(OUT_MODEL(IMI)%TOUTBAKN(IBAK_NUMB))
  !
  !* Populate the backup data structures
  !
  IPOS = 0
  DO JOUT = 1,SIZE(IBAK_STEP)
    IF (IBAK_STEP(JOUT) >= 0) THEN
        IPOS = IPOS + 1
        OUT_MODEL(IMI)%TOUTBAKN(IPOS)%NBAKID = IPOS
        OUT_MODEL(IMI)%TOUTBAKN(IPOS)%NOUTID = -1
        OUT_MODEL(IMI)%TOUTBAKN(IPOS)%NSTEP = IBAK_STEP(JOUT)
        OUT_MODEL(IMI)%TOUTBAKN(IPOS)%XTIME = (IBAK_STEP(JOUT)-1)*DYN_MODEL(IMI)%XTSTEP
        IF (IPOS>999) THEN
          print *,'ERROR in SET_GRID: more than 999 backups'
          STOP
        END IF
        IF (.NOT.ASSOCIATED(TFILE_FIRST)) THEN
          ALLOCATE(TFILE_FIRST)
          TFILE_LAST => TFILE_FIRST
        ELSE
          ALLOCATE(TFILE_LAST%TFILE_NEXT)
          TFILE_LAST%TFILE_NEXT%TFILE_PREV => TFILE_LAST
          TFILE_LAST => TFILE_LAST%TFILE_NEXT
        ENDIF
        OUT_MODEL(IMI)%TOUTBAKN(IPOS)%TFILE => TFILE_LAST
        OUT_MODEL(IMI)%TOUTBAKN(IPOS)%TFILE%CTYPE="BACKUP"
        OUT_MODEL(IMI)%TOUTBAKN(IPOS)%TFILE%CMODE="WRITE"
        WRITE (YNUMBER,FMT="('.',I3.3)") IPOS
        OUT_MODEL(IMI)%TOUTBAKN(IPOS)%TFILE%CNAME=ADJUSTL(ADJUSTR(IO_SURF_MNH_MODEL(IMI)%COUTFILE)//YNUMBER)
        OUT_MODEL(IMI)%TOUTBAKN(IPOS)%TFILE%NLFITYPE=1 !1: to be transfered
!PW: TODO: set NLFIVERB only when useful (only if LFI file...)
        OUT_MODEL(IMI)%TOUTBAKN(IPOS)%TFILE%NLFIVERB=NVERB
        IF (LIOCDF4) THEN
          OUT_MODEL(IMI)%TOUTBAKN(IPOS)%TFILE%CTYPE='NETCDF4'
          IF (LLFIOUT) THEN
            PRINT *,'Warning: LLFIOUT + LIOCDF4 = .TRUE. not yet implemented with new IO data structures'
            OUT_MODEL(IMI)%TOUTBAKN(IPOS)%TFILE%NLFINPRAR= 22+2*(4+NRR+NSV)
          END IF
        ELSE IF (LLFIOUT) THEN
          OUT_MODEL(IMI)%TOUTBAKN(IPOS)%TFILE%CTYPE='LFI'
          OUT_MODEL(IMI)%TOUTBAKN(IPOS)%TFILE%NLFINPRAR= 22+2*(4+NRR+NSV)
        ELSE
          PRINT *,'Error: unknown backup fileformat'
          CALL ABORT
        ENDIF
    END IF
  END DO
  !
  !* Find dad output number
  !
  !Security check (if it happens, this part of the code should be exported outside of the IMI loop)
  IF (NDAD(IMI)>IMI) THEN
    print *,'ERROR in SET_GRID'
    STOP
  END IF
  IF (NDAD(IMI) == IMI .OR.  IMI == 1) THEN
    OUT_MODEL(IMI)%TOUTBAKN(:)%NOUTDAD = 0
    !Check IPOS>0 because TOUTBAKN(0) does not exist (IPOS=0 only if no backups)
    IF(IPOS>0) OUT_MODEL(IMI)%TOUTBAKN(IPOS)%CDADFILENAME = OUT_MODEL(IMI)%TOUTBAKN(IPOS)%TFILE%CNAME
  ELSE
    DO IPOS = 1,OUT_MODEL(IMI)%NOUT_NUMB
      IDX = 0
      DO JOUT = 1,OUT_MODEL(NDAD(IMI))%NOUT_NUMB
        IF ( OUT_MODEL(NDAD(IMI))%TOUTBAKN(JOUT)%XTIME <= OUT_MODEL(IMI)%TOUTBAKN(IPOS)%XTIME+1.E-6 ) THEN
          IDX = JOUT
        ELSE
          EXIT
        END IF
      END DO
      IF (IDX>0) THEN
        OUT_MODEL(IMI)%TOUTBAKN(IPOS)%NOUTDAD = IDX
        WRITE (YDADNUMBER,FMT="('.',I3.3)") OUT_MODEL(IMI)%TOUTBAKN(IPOS)%NOUTDAD
        OUT_MODEL(IMI)%TOUTBAKN(IPOS)%CDADFILENAME = ADJUSTL(ADJUSTR(CDAD_NAME(IMI))//YDADNUMBER)
      ELSE
        OUT_MODEL(IMI)%TOUTBAKN(IPOS)%NOUTDAD = -1
        WRITE ( OUT_MODEL(IMI)%TOUTBAKN(IPOS)%CDADFILENAME , FMT="('NO_DAD_FILE')" )
      END IF
    END DO
  END IF
  !
  DEALLOCATE(IBAK_STEP)
  !
  IF (IP==1) THEN
  PRINT *,'-------------------------'
  PRINT *,'Model number:      ',IMI
  PRINT *,'Number of backups: ',IBAK_NUMB
  PRINT *,'Timestep     Time'
  DO JOUT = 1,IBAK_NUMB
    WRITE(*,'( I9 F12.3 )'  ) OUT_MODEL(IMI)%TOUTBAKN(JOUT)%NSTEP,OUT_MODEL(IMI)%TOUTBAKN(JOUT)%XTIME
  END DO
  PRINT *,'-------------------------'
  END IF
  !
END DO ! IMI=1,NMODEL
!
DEALLOCATE(NBAK_STEP)
DEALLOCATE(XBAK_TIME)
!
END SUBROUTINE IO_PREPARE_BAKOUT_STRUCT
!
END MODULE MODE_IO_MANAGE_STRUCT
