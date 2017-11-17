!MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!--------------- special set of characters for CVS information
!-----------------------------------------------------------------
! $Source$
! $Name$ 
! $Revision$ 
! $Date$
!-----------------------------------------------------------------
!-----------------------------------------------------------------
!!    Authors
!!    -------
!
!     D. Gazen
!     Juan 19/08/2005: bug argument optinonel ACCESS --> YACCESS 
!     Juan 22/05/2008: bug mode SPECIFIC in OPEN_ll 
!     Juan 05/11/2009: allow JPMAX_UNIT=48 open files 
!     J.Escobar   18/10/10   bug with PGI compiler on ADJUSTL
!     P. Wautelet 04/02/2016: bug with DELIM='NONE' and GCC 5.2/5.3
!     D.Gazen   : avril 2016 change error message 
!     P. Wautelet : may 2016: use NetCDF Fortran module
!     P. Wautelet : July 2016: added type OUTBAK
!
MODULE MODE_IO_ll

  USE MODD_ERRCODES
  USE MODE_FD_ll
  USE MODD_MPIF
  !JUANZ
  USE MODD_VAR_ll, ONLY : NMNH_COMM_WORLD
  !JUANZ
  USE MODE_MSG

  IMPLICIT NONE 

  PRIVATE

  !INCLUDE 'mpif.h'

  INTEGER, PARAMETER :: JPFNULL = 9       !! /dev/null fortran unit
  INTEGER, PARAMETER :: JPRESERVED_UNIT   = 11
  INTEGER, PARAMETER :: JPMAX_UNIT_NUMBER = JPRESERVED_UNIT+300
  ! 
  LOGICAL,SAVE :: GALLOC(JPRESERVED_UNIT:JPMAX_UNIT_NUMBER) = .FALSE.
  !
  CHARACTER(LEN=*),PARAMETER      :: CFILENULL="/dev/null"
  !
  LOGICAL,SAVE :: GCONFIO = .FALSE. ! Turn TRUE when SET_CONFIO_ll is called.

  PUBLIC IONEWFLU,UPCASE,INITIO_ll,OPEN_ll,CLOSE_ll,FLUSH_ll
  PUBLIC SET_CONFIO_ll,GCONFIO
  !JUANZ
  PUBLIC  io_file,io_rank
  !JUANZ

CONTAINS 

  FUNCTION IONEWFLU()

    INTEGER :: IONEWFLU

    INTEGER :: JI
    INTEGER :: IOS
    LOGICAL :: GEXISTS, GOPENED, GFOUND

    GFOUND = .FALSE.

    DO JI=JPRESERVED_UNIT, JPMAX_UNIT_NUMBER
       IF (GALLOC(JI)) CYCLE
       INQUIRE(UNIT=JI, EXIST=GEXISTS, OPENED=GOPENED, IOSTAT=IOS)
       IF (GEXISTS .AND. .NOT. GOPENED .AND. IOS == 0) THEN
          IONEWFLU   = JI
          GFOUND     = .TRUE.
          GALLOC(JI) = .TRUE.
          EXIT
       END IF
    END DO

    IF (.NOT. GFOUND) IONEWFLU = NOSLOTLEFT

  END FUNCTION IONEWFLU

  SUBROUTINE IOFREEFLU(KOFLU)
    
    INTEGER :: KOFLU
    
    IF ((KOFLU .GE. JPRESERVED_UNIT) .AND. (KOFLU .LE. JPMAX_UNIT_NUMBER )) THEN 
       GALLOC(KOFLU) = .FALSE.
    ELSE
       print*,"mode_io.f90: IOFREEFLU BAD IUNIT=",KOFLU
       STOP "mode_io.f90: IOFREEFLU BAD IUNIT"
    END IF

  END SUBROUTINE IOFREEFLU

  FUNCTION UPCASE(HSTRING)
    CHARACTER(LEN=*)            :: HSTRING
    CHARACTER(LEN=LEN(HSTRING)) :: UPCASE

    INTEGER :: JC
    INTEGER, PARAMETER :: IAMIN = IACHAR("a")
    INTEGER, PARAMETER :: IAMAJ = IACHAR("A")

    DO JC=1,LEN(HSTRING)
       IF (HSTRING(JC:JC) >= "a" .AND. HSTRING(JC:JC) <= "z") THEN
          UPCASE(JC:JC) = ACHAR(IACHAR(HSTRING(JC:JC)) - IAMIN + IAMAJ)
       ELSE
          UPCASE(JC:JC) = HSTRING(JC:JC)
       END IF
    END DO

  END FUNCTION UPCASE

  SUBROUTINE SET_CONFIO_ll()
    USE MODN_CONFIO

    !Use MODN_CONFIO namelist variables
    CALL SET_CONFIO_INTERN_ll(LCDF4, LLFIOUT, LLFIREAD)
  END SUBROUTINE SET_CONFIO_ll

  SUBROUTINE SET_CONFIO_INTERN_ll(OIOCDF4, OLFIOUT, OLFIREAD)
    USE MODD_IO_ll, ONLY : LIOCDF4, LLFIOUT, LLFIREAD
    LOGICAL, INTENT(IN) :: OIOCDF4, OLFIOUT, OLFIREAD

    CALL PRINT_MSG(NVERB_DEBUG,'IO','SET_CONFIO_ll','called')

    IF (GCONFIO) THEN
      CALL PRINT_MSG(NVERB_WARNING,'IO','SET_CONFIO_ll','already called (ignoring this call)')
    ELSE
#if defined(MNH_IOCDF4)
      LIOCDF4  = OIOCDF4
      LLFIOUT  = OLFIOUT
      LLFIREAD = OLFIREAD

      IF (.NOT.LIOCDF4 .AND. .NOT.LLFIOUT) THEN
        CALL PRINT_MSG(NVERB_WARNING,'IO','SET_CONFIO_ll','output format forced to netCDF')
        LIOCDF4 = .TRUE.
      END IF
#else
      LIOCDF4  = .FALSE.
      LLFIOUT  = .TRUE.
      LLFIREAD = .TRUE.
#endif       
      GCONFIO = .TRUE.
    END IF
    
  END SUBROUTINE SET_CONFIO_INTERN_ll
  
  SUBROUTINE INITIO_ll()
    USE  MODE_MNH_WORLD , ONLY :  INIT_NMNH_COMM_WORLD
    USE MODD_IO_ll
    USE MODE_FIELD
    IMPLICIT NONE

    INTEGER :: IERR, IOS
    LOGICAL :: GISINIT

    CALL PRINT_MSG(NVERB_DEBUG,'IO','INITIO_ll','called')

    ISTDERR = 0

    CALL MPI_INITIALIZED(GISINIT, IERR)
    IF (.NOT. GISINIT) THEN
       !CALL MPI_INIT(IERR)
       CALL INIT_NMNH_COMM_WORLD(IERR)
       if (IERR .NE.0) CALL PRINT_MSG(NVERB_FATAL,'IO','SET_CONFIO_ll','problem with remapping of NMNH_COMM_WORLD')
    END IF
    !! Now MPI is initialized for sure

    CALL INITFD()

    !! Default number for Processor I/O
    ISIOP = 1

    !! Get number of allocated processors
    CALL MPI_COMM_SIZE(NMNH_COMM_WORLD, ISNPROC,IERR)
    IF (ISNPROC==1) GSMONOPROC = .TRUE.

    !! Store proc number
    CALL MPI_COMM_RANK(NMNH_COMM_WORLD, ISP, IERR)
    ISP = ISP + 1

    !! Open /dev/null for GLOBAL mode
#if defined(DEV_NULL)
    OPEN(UNIT=JPFNULL,FILE=CFILENULL  ,ACTION='WRITE',IOSTAT=IOS)
#else
    OPEN(UNIT=JPFNULL,STATUS='SCRATCH',ACTION='WRITE',IOSTAT=IOS)
#endif
    IF (IOS > 0) THEN
       WRITE(ISTDERR,*) 'Error OPENING /dev/null...'
       CALL MPI_ABORT(NMNH_COMM_WORLD, IOS, IERR)
    END IF

    !! Init STDOUT and PIPE
    IF (ISP == ISIOP) THEN
       ISTDOUT = 6
    ELSE
       ISTDOUT = JPFNULL
    END IF

  END SUBROUTINE INITIO_ll

  SUBROUTINE OPEN_ll(&
       TPFILE,  &
       MODE,    &
       LFIPAR,  &
       COMM,    &
       STATUS,  &
       ACCESS,  &
       IOSTAT,  &
       FORM,    &
       RECL,    &
       BLANK,   &
       POSITION,&
       DELIM,    &
       PAD,      &
       KNB_PROCIO,& 
       KMELEV,&
       OPARALLELIO)
#if defined(MNH_IOCDF4)
  USE MODD_NETCDF, ONLY:IDCDF_KIND
  USE MODE_NETCDF
#endif
  USE MODD_IO_ll
  USE MODE_IO_MANAGE_STRUCT, ONLY: IO_FILE_ADD2LIST, IO_FILE_FIND_BYNAME

    TYPE(TFILEDATA), INTENT(INOUT)         :: TPFILE
    CHARACTER(len=*),INTENT(IN),  OPTIONAL :: MODE
    TYPE(LFIPARAM),  POINTER,     OPTIONAL :: LFIPAR
    CHARACTER(len=*),INTENT(IN),  OPTIONAL :: STATUS
    CHARACTER(len=*),INTENT(IN),  OPTIONAL :: ACCESS
    INTEGER,         INTENT(OUT)           :: IOSTAT
    CHARACTER(len=*),INTENT(IN),  OPTIONAL :: FORM
    INTEGER,         INTENT(IN),  OPTIONAL :: RECL
    CHARACTER(len=*),INTENT(IN),  OPTIONAL :: BLANK
    CHARACTER(len=*),INTENT(IN),  OPTIONAL :: POSITION
    CHARACTER(len=*),INTENT(IN),  OPTIONAL :: DELIM
    CHARACTER(len=*),INTENT(IN),  OPTIONAL :: PAD
    INTEGER,         INTENT(IN),  OPTIONAL :: COMM
    !JUANZ
    INTEGER,         INTENT(IN),  OPTIONAL :: KNB_PROCIO
    INTEGER(KIND=LFI_INT), INTENT(IN),  OPTIONAL :: KMELEV    
    LOGICAL,         INTENT(IN),  OPTIONAL :: OPARALLELIO
    !JUANZ
    !
    ! local var
    !
    !JUANZ
    CHARACTER(len=5)                      :: cfile
    INTEGER                               :: ifile, irank_procio   
    TYPE(FD_ll), POINTER                  :: TZFD_IOZ  
    CHARACTER(len=128)                    :: YFILE_IOZ
    INTEGER(KIND=LFI_INT)                 :: IRESOU,IMELEV,INPRAR
    INTEGER(KIND=LFI_INT)                 :: INUMBR8,ININAR8    
    LOGICAL(KIND=LFI_INT)                 :: GNAMFI8,GFATER8,GSTATS8 
    !JUANZ

#if defined(MNH_SX5) || defined(MNH_SP4) || defined(NAGf95) || defined(MNH_LINUX)
    CHARACTER(len=20)    :: YSTATUS
    CHARACTER(len=20)    :: YACCESS
    CHARACTER(len=20)    :: YFORM
    INTEGER              :: YRECL
    INTEGER ,PARAMETER   :: RECL_DEF = 10000
    CHARACTER(len=20)    :: YBLANK
    CHARACTER(len=20)    :: YPOSITION
    CHARACTER(len=20)    :: YDELIM
    CHARACTER(len=20)    :: YPAD
    !JUAN
#endif
    CHARACTER(len=20)    :: YACTION
    CHARACTER(len=20)    :: YMODE
    INTEGER              :: IOS,IERR,IRESP
    INTEGER(KIND=IDCDF_KIND) :: IOSCDF
    INTEGER              :: ICOMM
    INTEGER              :: ICMPRES
    TYPE(FD_ll), POINTER :: TZFD, TZFDTEMP
    ! didier
    LOGICAL :: GEXISTS,GOPENED
    INTEGER :: IUNIT
    ! didier
    !JUAN SX5 : probleme function retournant un pointer
    TYPE(FD_ll), POINTER :: TZJUAN
    LOGICAL               :: GPARALLELIO
    TYPE(TFILEDATA),POINTER :: TZSPLITFILE

    CALL PRINT_MSG(NVERB_DEBUG,'IO','OPEN_ll','opening '//TRIM(TPFILE%CNAME)//' for '//TRIM(TPFILE%CMODE))
    !
    IF ( PRESENT(OPARALLELIO) ) THEN
      GPARALLELIO = OPARALLELIO
    ELSE  !par defaut on active les IO paralleles en Z si possible
      GPARALLELIO = .TRUE.
    ENDIF

#ifdef MNH_VPP
    !! BUG Fuji avec RECL non fourni en argument de MYOPEN
    INTEGER :: IRECSIZE     
    IF (PRESENT(RECL)) THEN
       IRECSIZE = RECL
    ELSE 
       IRECSIZE = 2147483647  ! Default value for FUJI RECL
    END IF
#endif

    IOS = 0
    IF (PRESENT(COMM)) THEN 
       ICOMM = COMM
    ELSE
       ICOMM = NMNH_COMM_WORLD ! Default communicator
    END IF

    IF (PRESENT(MODE)) THEN 
       YMODE = MODE
       YMODE = UPCASE(TRIM(ADJUSTL(YMODE)))
    ELSE 
       YMODE = 'GLOBAL'         ! Default Mode
    END IF

    YACTION = TPFILE%CMODE
    YACTION = UPCASE(TRIM(ADJUSTL(YACTION)))
    IF (YACTION /= "READ" .AND. YACTION /= "WRITE") THEN
       IOSTAT = 99
       TPFILE%NLU = -1
       WRITE(ISTDERR,*) 'Erreur OPEN_ll : ACTION=',YACTION,' non supportee'
       RETURN
    END IF

    IF (.NOT. ANY(YMODE == (/'GLOBAL     ','SPECIFIC   ','DISTRIBUTED' , 'IO_ZSPLIT  '/))) THEN
       IOSTAT = 99
       TPFILE%NLU = -1
       WRITE(ISTDERR,*) 'OPEN_ll error : MODE UNKNOWN'
       RETURN
    END IF

    !JUAN SX5 : probleme function retournant un pointer
    !IF (.NOT. ASSOCIATED(GETFD(TPFILE%CNAME))) THEN
    TZJUAN=>GETFD(TPFILE%CNAME)
    IF (.NOT. ASSOCIATED(TZJUAN)) THEN 
       !JUAN SX5 : probleme function retournant un pointer
       !! File is not already opened : GOOD
       !! Add a new FD element
       TZFD=>NEWFD()
    ELSE 
       !! Error : File already opened
       IOSTAT = 99
       TPFILE%NLU = -1
       WRITE(ISTDERR,*) 'OPEN_ll error : File', TPFILE%CNAME, 'already opened'
       RETURN
    END IF

!!$    CALL MPI_ALLREDUCE(ILOCALERR, IGLOBALERR, 1, MPI_INTEGER, MPI_BOR,&
!!$         & ICOMM, IERR)
!!$    IF (IGLOBALERR /= NOERROR) THEN 
!!$       IOSTAT = GLOBALERR
!!$       TPFILE%NLU = -1
!!$       RETURN 
!!$    END IF



    TZFD%NAME = TPFILE%CNAME
    TZFD%MODE = YMODE
    NULLIFY(TZFD%PARAM)

#if defined(MNH_SX5) || defined(MNH_SP4) || defined(NAGf95) || defined(MNH_LINUX)
    !JUAN
    IF (PRESENT(STATUS)) THEN
       YSTATUS=STATUS
    ELSE
       YSTATUS='UNKNOWN'
    ENDIF
    IF (PRESENT(ACCESS)) THEN
       YACCESS=ACCESS
    ELSE
       YACCESS='SEQUENTIAL'
    ENDIF
    IF (PRESENT(FORM)) THEN
       YFORM=FORM
    ELSE
       YFORM='FORMATTED'
    ENDIF
    IF (PRESENT(RECL)) THEN
       YRECL=RECL
    ELSE
       YRECL=RECL_DEF
    ENDIF
    IF (PRESENT(BLANK)) THEN
       YBLANK=BLANK
    ELSE
       YBLANK='NULL'
    ENDIF
    IF (PRESENT(POSITION)) THEN
       YPOSITION=POSITION
    ELSE
       YPOSITION='ASIS'
    ENDIF
    IF (PRESENT(DELIM)) THEN
       YDELIM=DELIM
    ELSE
       YDELIM='NONE'
    ENDIF
    IF (PRESENT(PAD)) THEN
       YPAD=PAD
    ELSE
       YPAD='YES'
    ENDIF
#endif

    SELECT CASE(YMODE)

    CASE('GLOBAL')
       IF (YACTION == 'READ') THEN
          TZFD%OWNER = ISP
          TPFILE%NMASTER_RANK  = -1
          TPFILE%LMASTER       = .TRUE. !Every process read the file
          TPFILE%LMULTIMASTERS = .TRUE.
       ELSE
          TZFD%OWNER = ISIOP
          IF (TPFILE%CTYPE=='OUTPUTLISTING') THEN
            TZFD%OWNER = ISP
            TPFILE%NMASTER_RANK  = -1
            TPFILE%LMASTER       = .TRUE. !Every process may write in the file
            TPFILE%LMULTIMASTERS = .TRUE.
          ELSE
            TPFILE%NMASTER_RANK  = ISIOP
            TPFILE%LMASTER       = (ISP == ISIOP)
            TPFILE%LMULTIMASTERS = .FALSE.
          END IF
       END IF

       IF (ISP == TZFD%OWNER) THEN 
          !! I/O processor case

          TZFD%FLU = IONEWFLU()
#ifdef MNH_VPP
          OPEN(UNIT=TZFD%FLU,       &
               FILE=TRIM(TZFD%NAME),&
               STATUS=STATUS,       &
               ACCESS=ACCESS,       &
               IOSTAT=IOS,          &
               FORM=FORM,           &
               RECL=IRECSIZE,       &
               BLANK=BLANK,         &
               POSITION=POSITION,   &
               ACTION=YACTION,      &
               DELIM=DELIM,         &
               PAD=PAD)

#else
#if defined(MNH_SX5) || defined(MNH_SP4) || defined(NAGf95) || defined(MNH_LINUX)
          !JUAN : 31/03/2000 modif pour acces direct
          IF (YACCESS=='DIRECT') THEN
             OPEN(UNIT=TZFD%FLU,       &
                  FILE=TRIM(TZFD%NAME),&
                  STATUS=YSTATUS,       &
                  ACCESS=YACCESS,       &
                  IOSTAT=IOS,          &
                  FORM=YFORM,           &
                  RECL=YRECL,           &
                  ACTION=YACTION)
          ELSE
             IF (YFORM=="FORMATTED") THEN
               IF (YACTION=='READ') THEN
                OPEN(UNIT=TZFD%FLU,       &
                     FILE=TRIM(TZFD%NAME),&
                     STATUS=YSTATUS,       &
                     ACCESS=YACCESS,       &
                     IOSTAT=IOS,          &
                     FORM=YFORM,           &
                     RECL=YRECL,           &
                     BLANK=YBLANK,         &
                     POSITION=YPOSITION,   &
                     ACTION=YACTION,      &
                     !DELIM=YDELIM,         & !Philippe: commented because bug with GCC 5.X
                     PAD=YPAD)
               ELSE
                OPEN(UNIT=TZFD%FLU,       &
                     FILE=TRIM(TZFD%NAME),&
                     STATUS=YSTATUS,       &
                     ACCESS=YACCESS,       &
                     IOSTAT=IOS,          &
                     FORM=YFORM,           &
                     RECL=YRECL,           &
                     BLANK=YBLANK,         &
                     POSITION=YPOSITION,   &
                     ACTION=YACTION,      &
                     DELIM=YDELIM,         &
                     PAD=YPAD)
               ENDIF
             ELSE
                OPEN(UNIT=TZFD%FLU,       &
                     FILE=TRIM(TZFD%NAME),&
                     STATUS=YSTATUS,       &
                     ACCESS=YACCESS,       &
                     IOSTAT=IOS,          &
                     FORM=YFORM,           &
                     RECL=YRECL,           &
                     POSITION=YPOSITION,   &
                     ACTION=YACTION)
             ENDIF
          ENDIF


          !print*,' OPEN_ll'
          !print*,' OPEN(UNIT=',TZFD%FLU       
          !print*,' FILE=',TRIM(TZFD%NAME)
          !print*,' STATUS=',YSTATUS       
          !print*,' ACCESS=',YACCESS
          !print*,' IOSTAT=',IOS
          !print*,' FORM=',YFORM
          !print*,' RECL=',YRECL
          !print*,' BLANK=',YBLANK
          !print*,' POSITION=',YPOSITION
          !print*,' ACTION=',YACTION
          !print*,' DELIM=',YDELIM
          !print*,' PAD=',YPAD
#else
          OPEN(UNIT=TZFD%FLU,       &
               FILE=TRIM(TZFD%NAME),&
               STATUS=STATUS,       &
               ACCESS=ACCESS,       &
               IOSTAT=IOS,          &
               FORM=FORM,           &
               RECL=RECL,           &
               BLANK=BLANK,         &
               POSITION=POSITION,   &
               ACTION=YACTION,      &
               DELIM=DELIM,         &
               PAD=PAD)
#endif

#endif

       ELSE 
          !! NON I/O processors case
          IOS = 0
          TZFD%FLU = JPFNULL
       END IF

    CASE('SPECIFIC')
       TZFD%OWNER = ISP
       TZFD%FLU = IONEWFLU()
       TPFILE%NMASTER_RANK  = -1
       TPFILE%LMASTER       = .TRUE. !Every process use the file
       TPFILE%LMULTIMASTERS = .TRUE.

#ifdef MNH_VPP
       OPEN(UNIT=TZFD%FLU,                      &
            FILE=TRIM(TZFD%NAME)//SUFFIX(".P"), &
            STATUS=STATUS,                         &
            ACCESS=ACCESS,                         &
            IOSTAT=IOS,                            &
            FORM=FORM,                             &
            RECL=IRECSIZE,                         &
            BLANK=BLANK,                           &
            POSITION=POSITION,                     &
            ACTION=YACTION,                        &
            DELIM=DELIM,                           &
            PAD=PAD)

#else
#if defined(MNH_SX5) || defined(MNH_SP4) || defined(NAGf95) || defined(MNH_LINUX)
       IF (ACCESS=='DIRECT') THEN
          OPEN(UNIT=TZFD%FLU,       &
               FILE=TRIM(TZFD%NAME)//SUFFIX(".P"), &
               STATUS=YSTATUS,       &
               ACCESS=YACCESS,       &
               IOSTAT=IOS,          &
               FORM=YFORM,           &
               RECL=YRECL,           &
               ACTION=YACTION)
       ELSE
        IF (YACTION=='READ') THEN
          OPEN(UNIT=TZFD%FLU,                      &
               FILE=TRIM(TZFD%NAME)//SUFFIX(".P"), &
               STATUS=YSTATUS,                         &
               ACCESS=YACCESS,                         &
               IOSTAT=IOS,                             &
               FORM=YFORM,                             &
               RECL=YRECL,                             &
               BLANK=YBLANK,                           &
               POSITION=YPOSITION,                     &
               ACTION=YACTION,                         &
               !DELIM=YDELIM,         & !Philippe: commented because bug with GCC 5.X
               PAD=YPAD)
         ELSE
          OPEN(UNIT=TZFD%FLU,                      &
               FILE=TRIM(TZFD%NAME)//SUFFIX(".P"), &
               STATUS=YSTATUS,                         &
               ACCESS=YACCESS,                         &
               IOSTAT=IOS,                             &
               FORM=YFORM,                             &
               RECL=YRECL,                             &
               BLANK=YBLANK,                           &
               POSITION=YPOSITION,                     &
               ACTION=YACTION,                         &
               DELIM=YDELIM,                           &
               PAD=YPAD)
         ENDIF
       ENDIF
#else
       OPEN(UNIT=TZFD%FLU,                      &
            FILE=TRIM(TZFD%NAME)//SUFFIX(".P"), &
            STATUS=STATUS,                         &
            ACCESS=ACCESS,                         &
            IOSTAT=IOS,                            &
            FORM=FORM,                             &
            RECL=RECL,                             &
            BLANK=BLANK,                           &
            POSITION=POSITION,                     &
            ACTION=YACTION,                        &
            DELIM=DELIM,                           &
            PAD=PAD)
#endif

#endif

    CASE('DISTRIBUTED')
       TZFD%OWNER = ISIOP
       IF (.NOT. PRESENT(LFIPAR)) THEN
          PRINT *,"ERROR OPEN_ll : LFI non present"
          RETURN
       END IF
       TZFD%PARAM=>LFIPAR

       TPFILE%NMASTER_RANK  = ISIOP
       TPFILE%LMASTER       = (ISP == ISIOP)
       TPFILE%LMULTIMASTERS = .FALSE.

       IF (ISP == TZFD%OWNER) THEN 
          TZFD%FLU = IONEWFLU()
       ELSE 
          !! NON I/O processors case
          IOS = 0
          TZFD%FLU = -1
       END IF


    CASE('IO_ZSPLIT')
       TPFILE%NMASTER_RANK  = ISIOP
       TPFILE%LMASTER       = (ISP == ISIOP)
       TPFILE%LMULTIMASTERS = .FALSE.

       TZFD%OWNER = ISIOP
       TZFD%NAME  = TRIM(TPFILE%CNAME)//".lfi"
       IF (PRESENT(KNB_PROCIO)) THEN
          TZFD%NB_PROCIO = KNB_PROCIO
       ELSE
          TZFD%NB_PROCIO = 1
       ENDIF
       IF( .NOT. GPARALLELIO ) THEN
         TZFD%NB_PROCIO = 1
       ENDIF
       TZFD%COMM = NMNH_COMM_WORLD
       TZFD%PARAM     =>LFIPAR
#if defined(MNH_IOCDF4)
       IF (ISP == TZFD%OWNER .AND. (.NOT. LIOCDF4 .OR. (YACTION=='WRITE' .AND. LLFIOUT) &
            &                                     .OR. (YACTION=='READ'  .AND. LLFIREAD))) THEN
#else
       IF (ISP == TZFD%OWNER) THEN
#endif
             TZFD%FLU = IONEWFLU()
       ELSE 
          !! NON I/O processors OR NetCDF read case 
          IOS = 0
          TZFD%FLU = -1
       END IF
       IF (TZFD%NB_PROCIO .GT. 1 ) THEN
          DO ifile=0,TZFD%NB_PROCIO-1
             irank_procio = 1 + io_rank(ifile,ISNPROC,TZFD%NB_PROCIO)
             write(cfile ,'(".Z",i3.3)') ifile+1
             YFILE_IOZ           = TRIM(TPFILE%CNAME)//cfile//".lfi"
             TZFD_IOZ           =>NEWFD()
             TZFD_IOZ%NAME      = YFILE_IOZ
             TZFD_IOZ%MODE      = 'IO_ZSPLIT'
             TZFD_IOZ%OWNER     = irank_procio
             TZFD_IOZ%COMM      = NMNH_COMM_WORLD
             TZFD_IOZ%NB_PROCIO = TZFD%NB_PROCIO
             TZFD_IOZ%FLU       = -1
             TZFD_IOZ%PARAM     =>LFIPAR

             CALL IO_FILE_FIND_BYNAME(TRIM(TPFILE%CNAME)//TRIM(CFILE),TZSPLITFILE,IRESP,OOLD=.FALSE.)

             IF (IRESP/=0) THEN !File not yet in filelist => add it (nothing to do if already in list)
               CALL IO_FILE_ADD2LIST(TZSPLITFILE,TRIM(TPFILE%CNAME)//TRIM(CFILE),TPFILE%CTYPE,TPFILE%CMODE, &
                                     KLFINPRAR=TPFILE%NLFINPRAR,KLFITYPE=TPFILE%NLFITYPE,KLFIVERB=TPFILE%NLFIVERB)
             END IF
             !Done outside of the previous IF to prevent problems with .OUT files
             TZSPLITFILE%NMPICOMM      = ICOMM
             TZSPLITFILE%NMASTER_RANK  = irank_procio
             TZSPLITFILE%LMASTER       = (ISP == irank_procio)
             TZSPLITFILE%LMULTIMASTERS = .FALSE.

             IF ( irank_procio .EQ. ISP ) THEN
#if defined(MNH_IOCDF4)                   
                IF (LIOCDF4) THEN
                   IF (YACTION == 'READ' .AND. .NOT. LLFIREAD) THEN
                      ! Open NetCDF File for reading
                      TZFD_IOZ%CDF => NEWIOCDF()
                      CALL PRINT_MSG(NVERB_DEBUG,'IO','OPEN_ll','NF90_OPEN(IO_ZSPLIT) for '//TRIM(TPFILE%CNAME)//cfile//'.nc')
                      IOSCDF = NF90_OPEN(TRIM(TPFILE%CNAME)//cfile//".nc", NF90_NOWRITE, TZFD_IOZ%CDF%NCID)
                      IF (IOSCDF /= NF90_NOERR) THEN
   PRINT *, 'Error in opening (NF90_OPEN) ', TRIM(TPFILE%CNAME)//cfile//'.nc', ' : ', NF90_STRERROR(IOSCDF)
                         STOP
                      ELSE
                         IOS = 0
                      END IF
                   END IF
                   
                   IF (YACTION == 'WRITE') THEN
                      ! YACTION == 'WRITE'
                      ! Create NetCDF File for writing
                      TZFD_IOZ%CDF => NEWIOCDF()
                      CALL PRINT_MSG(NVERB_DEBUG,'IO','OPEN_ll','NF90_CREATE(IO_ZSPLIT) for '//TRIM(TPFILE%CNAME)//cfile//'.nc')
                      IOSCDF = NF90_CREATE(TRIM(TPFILE%CNAME)//cfile//".nc", &
                           &IOR(NF90_CLOBBER,NF90_NETCDF4), TZFD_IOZ%CDF%NCID)
                      IF (IOSCDF /= NF90_NOERR) THEN
                         PRINT *, 'Error in opening (NF90_CREATE) ', TRIM(TPFILE%CNAME)//cfile//'.nc', ' : ', NF90_STRERROR(IOSCDF)
                         STOP
                      ELSE
                         IOS = 0
                      END IF
                   END IF
                END IF
#endif
                IF (.NOT. LIOCDF4 .OR. (YACTION=='WRITE' .AND. LLFIOUT)&
                     &            .OR. (YACTION=='READ'  .AND. LLFIREAD)) THEN
                   ! LFI case
                   ! Open LFI File for reading
                   !this proc must write on this file open it ...    
                   TZFD_IOZ%FLU       = IONEWFLU()
                   !! LFI-File case
                   IRESOU = 0
                   GNAMFI8 = .TRUE.
                   GFATER8 = .TRUE.
                   GSTATS8 = .FALSE.
                   IF (PRESENT(KMELEV)) THEN
                      IMELEV = KMELEV
                   ELSE
                      IMELEV = 0
                   ENDIF
                   INPRAR = 49
                   !
                   ! JUAN open lfi file temporary modif
                   !
                   INUMBR8 = TZFD_IOZ%FLU
                   CALL LFIOUV(IRESOU,     &
                        INUMBR8,           &
                        GNAMFI8,           &
                        TZFD_IOZ%NAME,     &
                        "UNKNOWN",         &
                        GFATER8,           &
                        GSTATS8,           &
                        IMELEV,            &
                        INPRAR,            &
                        ININAR8)
                   !KNINAR = ININAR8
                END IF

                CALL UPDATE_METADATA(TZSPLITFILE)

             ENDIF
          ENDDO
       END IF


    END SELECT
print *,'PW: TPFILE%CNAME=',TPFILE%CNAME,'master,multimasters=',TPFILE%LMASTER,TPFILE%LMULTIMASTERS

!PW: not done here because TZFDLFI%CDF not yet set
!    CALL UPDATE_METADATA(TPFILE)

    ! Recherche d'un communicateur a reutiliser
    ! TZFD is the first element

    TPFILE%NMPICOMM = ICOMM
    TZFD%COMM = ICOMM
!!$    TZFD%COMM = MPI_COMM_NULL

!!$    TZFDTEMP=>TZFD%NEXT
!!$    DO WHILE(ASSOCIATED(TZFDTEMP))
!!$       CALL MPI_COMM_COMPARE(ICOMM,TZFDTEMP%COMM,ICMPRES,IERR)
!!$       IF (ICMPRES == MPI_CONGRUENT) THEN
!!$          TZFD%COMM = TZFDTEMP%COMM
!!$          EXIT
!!$       END IF
!!$       TZFDTEMP=>TZFDTEMP%NEXT
!!$    END DO
!!$
!!$    IF (TZFD%COMM == MPI_COMM_NULL) THEN
!!$       ! Pas de communicateur equivalent -> on duplique
!!$       !
!!$       CALL MPI_COMM_DUP(ICOMM, TZFD%COMM, IERR)
!!$       !       WRITE(ISTDOUT,*) 'FILE = ',TZFD%NAME,', comm ',TZFD%COMM&
!!$       !            & , ' cree par duplication de comm ', ICOMM
!!$    END IF

    IOSTAT = IOS
    TPFILE%NLU = TZFD%FLU

  CONTAINS
    FUNCTION SUFFIX(HEXT)

      CHARACTER(len=*)             :: HEXT
      CHARACTER(len=LEN(HEXT)+3)   :: SUFFIX

      WRITE(SUFFIX,'(A,i3.3)') TRIM(HEXT), ISP

    END FUNCTION SUFFIX

    SUBROUTINE UPDATE_METADATA(TPFILEMD)
      TYPE(TFILEDATA), INTENT(INOUT), OPTIONAL :: TPFILEMD

      TYPE(FD_ll), POINTER  :: TZFDLFI

      IF(.NOT.PRESENT(TPFILEMD)) RETURN

      TPFILEMD%LOPENED = .TRUE.
      TPFILEMD%NOPEN   = TPFILEMD%NOPEN + 1

      NULLIFY(TZFDLFI)

      TZFDLFI=>GETFD(ADJUSTL(TRIM(TPFILEMD%CNAME)//'.lfi'))

      IF(.NOT.ASSOCIATED(TZFDLFI)) &
        CALL PRINT_MSG(NVERB_FATAL,'IO','OPEN_ll::UPDATE_METADATA','TZFDLFI not found for '&
                                               //TRIM(TPFILEMD%CNAME))

      !TZFDLFI%CDF exists only if ISP == TZFDLFI%OWNER
      IF (TRIM(TPFILEMD%CMODE) == 'READ' .AND. ISP == TPFILEMD%NMASTER_RANK) THEN
        IF (LIOCDF4 .AND. .NOT.LLFIREAD) THEN
          TPFILEMD%NNCID = TZFDLFI%CDF%NCID
          IF (TPFILEMD%NNCID<0) CALL PRINT_MSG(NVERB_FATAL,'IO','OPEN_ll::UPDATE_METADATA','invalid NNCID for '&
                                               //TRIM(TPFILEMD%CNAME))
        ELSE
          TPFILEMD%NLFIFLU = TZFDLFI%FLU
          IF (TPFILEMD%NLFIFLU<0) CALL PRINT_MSG(NVERB_FATAL,'IO','OPEN_ll::UPDATE_METADATA','invalid NLFIFLU for '&
                                                //TRIM(TPFILEMD%CNAME))
        ENDIF
      ELSE IF (TRIM(TPFILEMD%CMODE) == 'WRITE' .AND. ISP == TPFILEMD%NMASTER_RANK) THEN
        IF (LIOCDF4) THEN
          TPFILEMD%NNCID = TZFDLFI%CDF%NCID
          IF (TPFILEMD%NNCID<0) CALL PRINT_MSG(NVERB_FATAL,'IO','OPEN_ll::UPDATE_METADATA','invalid NNCID for '&
                                               //TRIM(TPFILEMD%CNAME))
        END IF
        IF (.NOT.LIOCDF4 .OR. LLFIOUT) THEN
          TPFILEMD%NLFIFLU = TZFDLFI%FLU
          IF (TPFILEMD%NLFIFLU<0) CALL PRINT_MSG(NVERB_FATAL,'IO','OPEN_ll::UPDATE_METADATA','invalid NLFIFLU for '&
                                                 //TRIM(TPFILEMD%CNAME))
        END IF
      ELSE IF (TRIM(TPFILEMD%CMODE) /= 'READ' .AND. TRIM(TPFILEMD%CMODE) /= 'WRITE') THEN
        CALL PRINT_MSG(NVERB_FATAL,'IO','OPEN_ll::UPDATE_METADATA','unknown opening mode ('//TRIM(TPFILEMD%CMODE)//') for '&
                                                          //TRIM(TPFILEMD%CNAME))
      END IF

    END SUBROUTINE UPDATE_METADATA
  END SUBROUTINE OPEN_ll

  SUBROUTINE CLOSE_ll(HFILE,IOSTAT,STATUS,OPARALLELIO)
  USE MODD_IO_ll
#if defined(MNH_IOCDF4)
  USE MODE_NETCDF
#endif
    CHARACTER(LEN=*), INTENT(IN)            :: HFILE
    INTEGER,          INTENT(OUT), OPTIONAL :: IOSTAT
    CHARACTER(LEN=*), INTENT(IN),  OPTIONAL :: STATUS
    LOGICAL,          INTENT(IN),  OPTIONAL :: OPARALLELIO

    TYPE(FD_ll), POINTER :: TZFD
    INTEGER :: OLDCOMM

    INTEGER :: IERR, IGLOBALERR, IRESP

    CHARACTER(LEN=100)                      :: STATUSL
    !JUANZ
    CHARACTER(len=5)                      :: yfile
    INTEGER                               :: ifile, irank_procio,ilen   
    TYPE(FD_ll), POINTER                  :: TZFD_IOZ  
    CHARACTER(len=128)                    :: YFILE_IOZ
    INTEGER(KIND=LFI_INT)                 :: IRESP8,INUM8
    CHARACTER(LEN=7)                      :: YSTATU  
    LOGICAL                               :: GPARALLELIO

    CALL PRINT_MSG(NVERB_DEBUG,'IO','CLOSE_ll','closing '//TRIM(HFILE))

    IF ( PRESENT(OPARALLELIO) ) THEN
      GPARALLELIO = OPARALLELIO
    ELSE  !par defaut on active les IO paralleles en Z si possible
      GPARALLELIO = .TRUE.
    ENDIF
    !JUANZ

    TZFD=>GETFD(HFILE)

    IF (.NOT. ASSOCIATED(TZFD)) THEN
       WRITE(ISTDOUT,*) 'Erreur CLOSE_ll : Fichier : ', HFILE, ' non&
            & present...'
       IF (PRESENT(IOSTAT)) IOSTAT = BADVALUE
       RETURN
    END IF

    IRESP      = 0
    IGLOBALERR = 0
    IF (PRESENT(STATUS))  THEN
       STATUSL = STATUS
    ELSE
       STATUSL = "KEEP"
    ENDIF

    SELECT CASE(TZFD%MODE)
    CASE('GLOBAL','SPECIFIC')
       IF (TZFD%OWNER == ISP) THEN
          CLOSE(UNIT=TZFD%FLU, IOSTAT=IRESP,STATUS=STATUSL)
          CALL IOFREEFLU(TZFD%FLU)
       END IF
       CALL MPI_ALLREDUCE(IRESP,IGLOBALERR,1,MPI_INTEGER,MPI_BOR,TZFD&
            & %COMM,IERR)
    CASE('DISTRIBUTED')
       ! nothing to close with FM-Files

    CASE('IO_ZSPLIT')
       !
       ! close LFI file in the different PROC
       !
       IF( .NOT. GPARALLELIO ) THEN
         TZFD%NB_PROCIO = 1
       ENDIF
       IF (TZFD%NB_PROCIO .GT. 1 ) THEN
          DO ifile=0,TZFD%NB_PROCIO-1
             irank_procio = 1 + io_rank(ifile,ISNPROC,TZFD%NB_PROCIO)
             write(yfile ,'(".Z",i3.3)') ifile+1
             ilen = len_trim(TZFD%NAME)
             YFILE_IOZ   = TRIM(TZFD%NAME(1:ilen-4))//yfile//".lfi"
             TZFD_IOZ => GETFD(YFILE_IOZ)
             IF (ISP == TZFD_IOZ%OWNER) THEN
                IF (TZFD_IOZ%FLU > 0) THEN
                   INUM8=TZFD_IOZ%FLU
                   CALL LFIFER(IRESP8,INUM8,YSTATU)
                   CALL IOFREEFLU(TZFD_IOZ%FLU)
                   IRESP = IRESP8
                END IF
                IF (ASSOCIATED(TZFD_IOZ%CDF)) CALL CLEANIOCDF(TZFD_IOZ%CDF)
             END IF
          END DO
       END IF
    END SELECT

    OLDCOMM = TZFD%COMM   !! Recopie dans var. temporaire

    CALL DELFD(TZFD)

!!$    IF (IRESP == IGLOBALERR) THEN
!!$
!!$       ! liberation du communicateur
!!$       !
!!$       TZFD=>GETFD(OLDCOMM)
!!$
!!$       IF (.NOT. ASSOCIATED(TZFD)) THEN
!!$          CALL MPI_COMM_FREE(OLDCOMM, IERR)
!!$       END IF
!!$    END IF

    IF (PRESENT(IOSTAT)) IOSTAT = IGLOBALERR

  END SUBROUTINE CLOSE_ll

  SUBROUTINE FLUSH_ll(HFILE,IRESP)
#if defined(NAGf95)
    USE F90_UNIX
#endif
    USE MODD_IO_ll
    CHARACTER(LEN=*), INTENT(IN)            :: HFILE
    INTEGER,          INTENT(OUT), OPTIONAL :: IRESP

    TYPE(FD_ll), POINTER :: TZFD
    INTEGER              :: IUNIT

    IRESP=0
    TZFD=>GETFD(HFILE)
    IF (.NOT. ASSOCIATED(TZFD)) THEN
       WRITE(ISTDOUT,*) 'Error in FLUSH_ll : file ',TRIM(HFILE),&
            &' not present !'
       IF (PRESENT(IRESP)) IRESP = BADVALUE
       RETURN
    END IF

    IUNIT=TZFD%FLU
    IF (TZFD%OWNER == ISP .AND. TZFD%MODE /= 'DISTRIBUTED') THEN
#if defined(MNH_SP4)
       CALL FLUSH(IUNIT)
#else
       CALL FLUSH(IUNIT)
#endif
    END IF

  END SUBROUTINE FLUSH_ll

  FUNCTION io_file(k,nb_proc_io)
    !
    ! return the file number where to write the K level of data
    !
    IMPLICIT NONE
    INTEGER(kind=MNH_MPI_RANK_KIND)                   :: k,nb_proc_io
    INTEGER(kind=MNH_MPI_RANK_KIND)                   :: io_file

    io_file = MOD ((k-1) , nb_proc_io )

  END FUNCTION io_file

  FUNCTION io_rank(ifile,nb_proc,nb_proc_io,offset_rank)
    !
    ! return the proc number which must write the 'ifile' file
    !
    IMPLICIT NONE
    INTEGER(kind=MNH_MPI_RANK_KIND)                  :: ifile,nb_proc,nb_proc_io
    INTEGER(kind=MNH_MPI_RANK_KIND),OPTIONAL         :: offset_rank

    INTEGER(kind=MNH_MPI_RANK_KIND)                  :: io_rank

    INTEGER(kind=MNH_MPI_RANK_KIND)                  :: ipas,irest

    ipas  =        nb_proc / nb_proc_io
    irest =  MOD ( nb_proc , nb_proc_io )

    IF  (ipas /= 0 ) THEN
       io_rank=ipas * ifile + MIN(ifile , irest )
    ELSE
       io_rank=MOD(ifile , nb_proc )
    ENDIF

    !
    ! optional rank to shift for read test
    !
    IF (PRESENT(offset_rank)) THEN
       IF ( offset_rank .GT.0 ) io_rank=MOD(io_rank+offset_rank,nb_proc)
       IF ( offset_rank .LT.0 ) io_rank=MOD(nb_proc-io_rank+offset_rank,nb_proc)
    ENDIF

  END FUNCTION io_rank
  !
  !
END MODULE MODE_IO_ll


MODULE MODE_MSG
!
USE MODD_IO_ll, ONLY : NVERB_FATAL,NVERB_ERROR,NVERB_WARNING,NVERB_INFO,NVERB_DEBUG
!
IMPLICIT NONE
!
CONTAINS
!
SUBROUTINE PRINT_MSG(KVERB,HDOMAIN,HSUBR,HMSG)
!
USE ISO_FORTRAN_ENV, ONLY : ERROR_UNIT, OUTPUT_UNIT
!
USE MODD_CONF,   ONLY : CPROGRAM
USE MODD_IO_ll,  ONLY : NIO_VERB,NIO_ABORT_LEVEL,NGEN_VERB,NGEN_ABORT_LEVEL, &
                        LVERB_OUTLST, LVERB_STDOUT, LVERB_ALLPRC, TFILE_OUTPUTLISTING
USE MODD_LUNIT,  ONLY : TLUOUT0
USE MODD_VAR_ll, ONLY : IP, NMNH_COMM_WORLD
!USE MODE_FM,     ONLY : IO_FILE_CLOSE_ll
!
INTEGER,         INTENT(IN) :: KVERB   !Verbosity level
CHARACTER(LEN=*),INTENT(IN) :: HDOMAIN !Domain/category of message
CHARACTER(LEN=*),INTENT(IN) :: HSUBR   !Subroutine/function name
CHARACTER(LEN=*),INTENT(IN) :: HMSG    !Message
!
CHARACTER(LEN=8)  :: YPRC
CHARACTER(LEN=9)  :: YPRE
CHARACTER(LEN=30) :: YSUBR
INTEGER :: IERR, IMAXVERB,IABORTLEVEL
INTEGER :: ILU
LOGICAL :: GWRITE_OUTLST,GWRITE_STDOUT
!
!Determine if the process will write
GWRITE_OUTLST = .FALSE.
GWRITE_STDOUT = .FALSE.
IF (IP == 1 .OR. LVERB_ALLPRC) THEN
  IF (LVERB_OUTLST) GWRITE_OUTLST = .TRUE.
  IF (LVERB_STDOUT) GWRITE_STDOUT = .TRUE.
END IF
!
YPRC=''
IF (LVERB_ALLPRC) WRITE(YPRC,'( I8 )') IP-1
!
!Check if the output file is available
ILU = -1
IF (ASSOCIATED(TFILE_OUTPUTLISTING)) THEN
  IF (TFILE_OUTPUTLISTING%LOPENED) THEN
    ILU = TFILE_OUTPUTLISTING%NLU
  ELSE
    GWRITE_OUTLST = .FALSE.
    IF (GWRITE_STDOUT) WRITE(UNIT=OUTPUT_UNIT,FMT=*) 'TFILE_OUTPUTLISTING not opened'
  END IF
ELSE
!PW: TODO?: temporary to detect non-initialisation
! should disappear except at the beginning of a run
  GWRITE_OUTLST = .FALSE.
  IF (GWRITE_STDOUT) WRITE(UNIT=OUTPUT_UNIT,FMT=*) 'TFILE_OUTPUTLISTING not associated'
END IF
!
SELECT CASE(HDOMAIN)
  CASE('IO')
    IMAXVERB    = NIO_VERB
    IABORTLEVEL = NIO_ABORT_LEVEL
  CASE ('GEN')
    IMAXVERB    = NGEN_VERB
    IABORTLEVEL = NGEN_ABORT_LEVEL
  CASE DEFAULT
    IF (GWRITE_STDOUT) WRITE(UNIT=OUTPUT_UNIT,FMT=*) 'ERROR: PRINT_MSG: wrong message category (',TRIM(HDOMAIN),')'
    IF (GWRITE_OUTLST) WRITE(UNIT=ILU,        FMT=*) 'ERROR: PRINT_MSG: wrong message category (',TRIM(HDOMAIN),')'
    RETURN
END SELECT
!
IF (KVERB>IMAXVERB) RETURN
!
SELECT CASE(KVERB)
  CASE(NVERB_FATAL)
    YPRE='FATAL:   '
  CASE(NVERB_ERROR)
    YPRE='ERROR:   '
  CASE(NVERB_WARNING)
    YPRE='WARNING: '
  CASE(NVERB_INFO)
    YPRE='INFO:    '
  CASE(NVERB_DEBUG)
    YPRE='DEBUG:   '
  CASE DEFAULT
    IF (GWRITE_STDOUT) WRITE(UNIT=OUTPUT_UNIT,FMT=*) 'ERROR: PRINT_MSG: wrong verbosity level'
    IF (GWRITE_OUTLST) WRITE(UNIT=ILU,        FMT=*) 'ERROR: PRINT_MSG: wrong verbosity level'
END SELECT
!
YSUBR=TRIM(HSUBR)//':'
IF (LVERB_ALLPRC) THEN
  IF (GWRITE_STDOUT) WRITE(UNIT=OUTPUT_UNIT,FMT="(A8,': ',A9,A30,A)") ADJUSTL(YPRC),YPRE,YSUBR,HMSG
  IF (GWRITE_OUTLST) WRITE(UNIT=ILU,        FMT="(A8,': ',A9,A30,A)") ADJUSTL(YPRC),YPRE,YSUBR,HMSG
ELSE
  IF (GWRITE_STDOUT) WRITE(UNIT=OUTPUT_UNIT,FMT="(A9,A30,A)") YPRE,YSUBR,HMSG
  IF (GWRITE_OUTLST) WRITE(UNIT=ILU,        FMT="(A9,A30,A)") YPRE,YSUBR,HMSG
END IF
!
IF (KVERB<=IABORTLEVEL) THEN
  IF (IP==1) WRITE(UNIT=ERROR_UNIT,FMT=*) 'ABORT asked by application '//TRIM(CPROGRAM)
  IF (GWRITE_STDOUT) WRITE(UNIT=OUTPUT_UNIT,FMT=*) 'ABORT asked by application '//TRIM(CPROGRAM)
  IF (GWRITE_OUTLST) WRITE(UNIT=ILU,        FMT=*) 'ABORT asked by application '//TRIM(CPROGRAM)
#if 0
  !Problem: loop dependency between MODE_MSG and MODE_FM (IO_FILE_CLOSE_ll call PRINT_MSG)
  NIO_VERB = 0 !To not get further messages (ABORT should be the last for readability)
  IF (ILU>0) CALL IO_FILE_CLOSE_ll(TFILE_OUTPUTLISTING) !To flush it
#else
  IF (ILU>0) FLUSH(UNIT=ILU) !OK in F2003
  IF (ASSOCIATED(TLUOUT0)) FLUSH(UNIT=TLUOUT0%NLU)
#endif
  CALL MPI_ABORT(NMNH_COMM_WORLD, -10, IERR)
  CALL ABORT
END IF
!
END SUBROUTINE PRINT_MSG
!
END MODULE MODE_MSG
