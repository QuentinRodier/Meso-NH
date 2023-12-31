! (C) Copyright 2014- ECMWF.
!
! This software is licensed under the terms of the Apache Licence Version 2.0
! which can be obtained at http://www.apache.org/licenses/LICENSE-2.0.
!
! In applying this licence, ECMWF does not waive the privileges and immunities
! granted to it by virtue of its status as an intergovernmental organisation
! nor does it submit to any jurisdiction.

MODULE EC_MEMINFO_MOD

CONTAINS

SUBROUTINE EC_MEMINFO(KU,CDSTRING,KCOMM,KBARR,KIOTASK,KCALL)

USE PARKIND1, ONLY : JPIM, JPIB, JPRD
USE MPL_MPIF

IMPLICIT NONE

!-- EC_MEMINFO:
!   Author   : Peter Towers (ECMWF)  : 2015-2016
!   Modified : Sami Saarinen (ECMWF) : 21-SEP-2016 : Added getenv EC_MEMINFO -- export EC_MEMINFO=0 disables any EC_MEMINFO output
!              Sami Saarinen (ECMWF) : 02-MAR-2017 : Enabled flexible number of sockets & lots of tidying
!              Sami Saarinen (ECMWF) : 09-MAR-2017 : Power monitoring added (via EC_PMON) -- works at least on Cray systems
!              Sami Saarinen (ECMWF) : 12-MAR-2017 : Gather core affinities via call to ec_coreid()
!              Sami Saarinen (ECMWF) : 12-DEC-2017 : Obtain MPI & OpenMP version information

!#include "ec_pmon.intfb.h"

INTEGER(KIND=JPIM), INTENT(IN) :: KU,KCOMM,KBARR,KIOTASK,KCALL
CHARACTER(LEN=*), INTENT(IN) :: CDSTRING
INTEGER(KIND=JPIM), PARAMETER :: ITAG = 98765
INTEGER(KIND=JPIM) :: ID,KULOUT
INTEGER(KIND=JPIM) :: II,JJ,I,J,K,MYPROC,NPROC,LEN,ERROR,NODENUM,JID,IDX
INTEGER(KIND=JPIB) :: TASKSMALL,NODEHUGE,MEMFREE,CACHED,NFREE
INTEGER(KIND=JPIB),SAVE :: NODEHUGE_CACHED
INTEGER(KIND=JPIM), PARAMETER :: MAXNUMA_DEF = 4 ! Max number of "sockets" supported by default
INTEGER(KIND=JPIM), SAVE :: MAXNUMA = 0 ! Max number of "sockets" supported -- initialized to zero to enforce updated value (env EC_MAXNUMA)
INTEGER(KIND=JPIM) :: NNUMA ! Actual number of "sockets" (can be 0 ob systems that do not have /proc/buddyinfo, e.g. WSL)
!INTEGER(KIND=JPIB),DIMENSION(0:MAXNUMA-1) :: SMALLPAGE,HUGEPAGE
INTEGER(KIND=JPIB),DIMENSION(:),ALLOCATABLE,SAVE :: SMALLPAGE,HUGEPAGE
INTEGER(KIND=JPIB) :: GETMAXRSS,GETMAXHWM
INTEGER(KIND=JPIB) :: HEAP_SIZE
INTEGER(KIND=JPIB), PARAMETER :: ONEMEGA = 1024_JPIB * 1024_JPIB
INTEGER(KIND=JPIB) :: ENERGY, POWER
INTEGER(KIND=JPIB) :: TOT_ENERGY, MAXPOWER, AVGPOWER
INTEGER(KIND=JPIM),SAVE :: PAGESIZE = 0
INTEGER(KIND=JPIM),SAVE :: MAXTH = 0
INTEGER(KIND=JPIM),SAVE :: MAXTH_COMP = 0
INTEGER(KIND=JPIM),SAVE :: MAXTH_IO = 0
INTEGER(KIND=JPIM),PARAMETER :: MAXCOLS = 18 ! Max numerical columns in /proc/buddyinfo (often just 11, but Cray has 18 entries)
INTEGER(KIND=JPIM) :: N18
!INTEGER(KIND=JPIB),DIMENSION(0:MAXCOLS-1,0:MAXNUMA-1) :: NODE, BUCKET
!INTEGER(KIND=JPIB),DIMENSION(7+2*MAXNUMA) :: SENDBUF,RECVBUF
INTEGER(KIND=JPIB),DIMENSION(:,:),ALLOCATABLE,SAVE :: NODE, BUCKET
INTEGER(KIND=JPIB),DIMENSION(:),ALLOCATABLE,SAVE :: SENDBUF,RECVBUF
REAL(KIND=JPRD) :: PERCENT_USED(2)
CHARACTER(LEN=256) :: CLSTR
CHARACTER(LEN=512) :: TMPDIR
CHARACTER(LEN=512), SAVE :: PROGRAM = ' '
CHARACTER(LEN=20)  :: NODENAME,LASTNODE,CLMAXNODE
CHARACTER(LEN=12)  :: VAL
CHARACTER(LEN=1)   :: M
CHARACTER(LEN=160) ::LINE
CHARACTER(LEN=56) :: FILENAME
CHARACTER(LEN=1) :: CLEC_MEMINFO
CHARACTER(LEN=5) :: CSTAR
CHARACTER(LEN=LEN(CSTAR)+1+LEN(CDSTRING)) :: ID_STRING
CHARACTER(LEN=10) ::  CLDATEOD,CLTIMEOD,CLZONEOD
CHARACTER(LEN=3), PARAMETER :: CLMON(1:12) = (/ &
     'Jan','Feb','Mar','Apr','May','Jun', &
     'Jul','Aug','Sep','Oct','Nov','Dec' /)
INTEGER(KIND=JPIM) :: IVALUES(8), IMON
INTEGER(KIND=JPIM) :: IRECV_STATUS(MPI_STATUS_SIZE)
LOGICAL :: LLNOCOMM, LLNOHDR
INTEGER(KIND=JPIM), SAVE :: IAM_NODEMASTER = 0
LOGICAL, SAVE :: LLFIRST_TIME = .TRUE.
TYPE RANKNODE_T
   INTEGER(KIND=JPIM) :: NODENUM
   INTEGER(KIND=JPIM) :: RANK_WORLD
   INTEGER(KIND=JPIM) :: RANK
   INTEGER(KIND=JPIM) :: IORANK
   INTEGER(KIND=JPIM) :: NODEMASTER
   INTEGER(KIND=JPIM) :: NUMTH
   INTEGER(KIND=JPIM), ALLOCATABLE :: COREIDS(:)
   CHARACTER(LEN=LEN(NODENAME)) :: NODE
   CHARACTER(LEN=LEN(CLSTR)) :: STR
END TYPE
TYPE (RANKNODE_T), ALLOCATABLE, SAVE :: RN(:)
INTEGER(KIND=JPIM), ALLOCATABLE :: COREIDS(:)
LOGICAL, ALLOCATABLE :: DONE(:)
INTEGER(KIND=JPIM), SAVE :: NUMNODES = 0
INTEGER(KIND=JPIM) :: NN
INTEGER(KIND=JPIM), SAVE :: IOTASKS = 0
INTEGER(KIND=JPIM) :: IORANK, NSEND, NRECV
LOGICAL :: FILE_EXISTS
REAL(KIND=JPRD), EXTERNAL :: UTIL_WALLTIME
REAL(KIND=JPRD), SAVE :: WT0
REAL(KIND=JPRD) :: WT
CHARACTER(LEN=64) :: CLPFX
CHARACTER(LEN=3) :: ZUM
INTEGER(KIND=JPIM) :: IPFXLEN, NUMTH, MYTH
INTEGER(KIND=JPIM) :: NCOMM_MEMINFO = 0
COMMON /cmn_meminfo/ NCOMM_MEMINFO
INTEGER OMP_GET_MAX_THREADS, OMP_GET_THREAD_NUM
#ifdef _OPENMP
EXTERNAL OMP_GET_MAX_THREADS, OMP_GET_THREAD_NUM
#else
OMP_GET_MAX_THREADS() = 1
OMP_GET_THREAD_NUM() = 0
#endif

CALL GET_ENVIRONMENT_VARIABLE('EC_MEMINFO',CLEC_MEMINFO)
IF (CLEC_MEMINFO == '0') RETURN

IF (LLFIRST_TIME) WT0 = UTIL_WALLTIME()
IF (MAXTH == 0) MAXTH = OMP_GET_MAX_THREADS()

LLNOCOMM = (KCOMM == -1 .or. KCOMM == -2)
LLNOHDR = (KCOMM == -2)

IF (LLNOCOMM) THEN
   ! Direct call to obtain EC_meminfo -output
   ERROR = 0
   MYPROC = 0
   NPROC = 1
   CLPFX = CDSTRING
   IPFXLEN = LEN_TRIM(CLPFX)
   ZUM = 'tsk'
ELSE
   CLPFX = ' '
   IPFXLEN = 0
   ZUM = 'sum'
   CALL MPI_COMM_RANK(KCOMM,MYPROC,ERROR)
   CALL CHECK_ERROR("from MPI_COMM_RANK",__FILE__,__LINE__)

   CALL MPI_COMM_SIZE(KCOMM,NPROC,ERROR)
   CALL CHECK_ERROR("from MPI_COMM_SIZE",__FILE__,__LINE__)

   IF (KCALL == 0) THEN
      CALL CONDBARR()
      CALL CHECK_ERROR("from MPI_BARRIER(at start)",__FILE__,__LINE__)
   ENDIF
ENDIF

IF (LLFIRST_TIME) THEN ! The *very* first time
   CALL EC_PMON(ENERGY,POWER)

   !-- Neither of these two may stop working when linking with C++ (like in OOPS) ...
   ! CALL GETARG(0,PROGRAM)
   ! CALL GET_COMMAND_ARGUMENT(0,PROGRAM)
   !... so using the old saviour from ifsaux/support/cargs.c:
   CALL GETARG_C(0,PROGRAM)

   CALL GET_ENVIRONMENT_VARIABLE("HUGETLB_DEFAULT_PAGE_SIZE",VAL)
   I=INDEX(VAL,"M")
   IF(I > 0) THEN
      READ(VAL(1:I-1),*) PAGESIZE
      PAGESIZE=PAGESIZE*1024
   ELSE
      PAGESIZE=0
   ENDIF

   NODEHUGE=0
   
   IF(PAGESIZE > 0) THEN
      !WRITE(FILENAME,'(a,i0,a)') "/sys/kernel/mm/hugepages/hugepages-", &
      !     PAGESIZE,"kB/nr_hugepages"
      FILENAME='/proc/sys/vm/nr_hugepages' ! more generic; contents the same as in /sys/kernel/mm/hugepages/hugepages-2048kB/nr_hugepages
      INQUIRE(FILE=FILENAME, EXIST=FILE_EXISTS)
      IF( FILE_EXISTS ) THEN
        OPEN(502,FILE=FILENAME,STATUS="old",ACTION="read",ERR=999)
        READ(502,*,ERR=998,END=998) NODEHUGE
998     continue
        CLOSE(502)
      ENDIF
999   continue
   ENDIF

   NODEHUGE=NODEHUGE*PAGESIZE
   NODEHUGE=NODEHUGE/1024
   NODEHUGE_CACHED = NODEHUGE
ENDIF

NODEHUGE=NODEHUGE_CACHED

CALL EC_GETHOSTNAME(NODENAME) ! from support/env.c

IF (MAXNUMA == 0) THEN
   CALL GET_ENVIRONMENT_VARIABLE("EC_MAXNUMA",VAL) ! Note: *not* export EC_MEMINFO_MAXNUMA=<value>, but EC_MAXNUMA=<value>
   IF (VAL /= "") READ(VAL,*) MAXNUMA
   IF (MAXNUMA < 1) MAXNUMA = MAXNUMA_DEF
   ALLOCATE(SMALLPAGE(0:MAXNUMA-1))
   ALLOCATE(HUGEPAGE(0:MAXNUMA-1))
   ALLOCATE(NODE(0:MAXCOLS-1,0:MAXNUMA-1))
   ALLOCATE(BUCKET(0:MAXCOLS-1,0:MAXNUMA-1))
   ALLOCATE(SENDBUF(7+2*MAXNUMA))
   ALLOCATE(RECVBUF(7+2*MAXNUMA))
ENDIF

IF (MYPROC == 0) THEN 
!
! Use already open file for output or $EC_MEMINFO_TMPDIR/meminfo
! We do not use $TMPDIR as it may have been inherited from mother superiour (MOMS) node
!
   IF(KU == -1) THEN
      CALL GET_ENVIRONMENT_VARIABLE('EC_MEMINFO_TMPDIR',TMPDIR)
      IF (TMPDIR == ' ') TMPDIR = '.'
      !    write(0,*) '## EC_MEMINFO: KCOMM=',KCOMM
      !    CALL LINUX_TRBK()
      KULOUT=501
      OPEN(UNIT=KULOUT,FILE=TRIM(TMPDIR)//"/"//"meminfo.txt",STATUS='unknown', &
           ACTION='write',POSITION='append')
   ELSE
      KULOUT=KU
   ENDIF
ENDIF

IF (LLFIRST_TIME .and. .not. LLNOCOMM) THEN
! Fetch affinities (over OpenMP threads)
! Note: I/O-tasks may now have different number of threads than on computational tasks
   ALLOCATE(COREIDS(0:MAXTH-1))
#ifdef _OPENMP
!$OMP PARALLEL NUM_THREADS(MAXTH) SHARED(COREIDS) PRIVATE(MYTH)
#endif
   MYTH = OMP_GET_THREAD_NUM()
   CALL EC_COREID(COREIDS(MYTH))
#ifdef _OPENMP
!$OMP END PARALLEL
#endif

! Store the communicator we are in upon entering EC_MEMINFO for the first time -- to be used in the EC_MPI_FINALIZE
   NCOMM_MEMINFO = KCOMM
! Fetch node names & numbers per task
   IORANK = 0
   IF (KIOTASK > 0) IORANK = 1
   IF (MYPROC == 0) THEN
      CALL SLASH_PROC
      ALLOCATE(RN(0:NPROC-1))
      DO I=0,NPROC-1
         RN(I)%NODENUM = -1
         IF (I > 0) THEN ! Receive in the MPI-rank order of KCOMM (i.e. may not be the same as MPI_COMM_WORLD -order)
            CALL MPI_RECV(LASTNODE,LEN(LASTNODE),MPI_BYTE,I,ITAG,KCOMM,IRECV_STATUS,ERROR)
            CALL CHECK_ERROR("from MPI_RECV(LASTNODE)",__FILE__,__LINE__)
            CALL MPI_RECV(IORANK,1,MPI_INTEGER4,I,ITAG+1,KCOMM,IRECV_STATUS,ERROR)
            CALL CHECK_ERROR("from MPI_RECV(IORANK)",__FILE__,__LINE__)
            CALL MPI_RECV(K,1,MPI_INTEGER4,I,ITAG+2,KCOMM,IRECV_STATUS,ERROR)
            CALL CHECK_ERROR("from MPI_RECV(RANK_WORLD)",__FILE__,__LINE__)
            CALL MPI_RECV(NUMTH,1,MPI_INTEGER4,I,ITAG+3,KCOMM,IRECV_STATUS,ERROR)
            CALL CHECK_ERROR("from MPI_RECV(NUMTH)",__FILE__,__LINE__)
            CALL MPI_RECV(CLSTR,LEN(CLSTR),MPI_BYTE,I,ITAG+4,KCOMM,IRECV_STATUS,ERROR)
            CALL CHECK_ERROR("from MPI_RECV(CLSTR)",__FILE__,__LINE__)
            RN(I)%RANK = I
            RN(I)%STR = CLSTR
         ELSE
            LASTNODE=NODENAME
            NUMTH = MAXTH
            CALL MPI_COMM_RANK(MPI_COMM_WORLD,K,ERROR)
            RN(I)%RANK = 0 ! Itself
            RN(I)%STR = CDSTRING
         ENDIF
         RN(I)%RANK_WORLD = K
         RN(I)%IORANK = IORANK
         RN(I)%NODEMASTER = 0
         RN(I)%NODE = LASTNODE
         ! Affinities
         RN(I)%NUMTH = NUMTH
         ALLOCATE(RN(I)%COREIDS(0:NUMTH-1))
         IF (I > 0) THEN ! Receive in MPI-rank order
            CALL MPI_RECV(RN(I)%COREIDS,NUMTH,MPI_INTEGER4,I,ITAG+5,KCOMM,IRECV_STATUS,ERROR)
            CALL CHECK_ERROR("from MPI_RECV(COREIDS)",__FILE__,__LINE__)
         ELSE
            RN(I)%COREIDS = COREIDS
         ENDIF
         IF (IORANK == 0) THEN
            MAXTH_COMP = MAX(MAXTH_COMP,NUMTH)
         ELSE
            MAXTH_IO = MAX(MAXTH_IO,NUMTH)
         ENDIF
      ENDDO
      
      CALL RNSORT(KULOUT) ! Output now goes to "meminfo.txt"

      IAM_NODEMASTER = RN(0)%NODEMASTER ! Itself
      DO I=1,NPROC-1
         CALL MPI_SEND(RN(I)%NODEMASTER,1,MPI_INTEGER4,I,ITAG+6,KCOMM,ERROR)
         CALL CHECK_ERROR("from MPI_SEND(IAM_NODEMASTER)",__FILE__,__LINE__)
      ENDDO
   ELSE
      CALL MPI_SEND(NODENAME,LEN(NODENAME),MPI_BYTE,0,ITAG,KCOMM,ERROR)
      CALL CHECK_ERROR("from MPI_SEND(NODENAME)",__FILE__,__LINE__)
      CALL MPI_SEND(IORANK,1,MPI_INTEGER4,0,ITAG+1,KCOMM,ERROR)
      CALL CHECK_ERROR("from MPI_SEND(IORANK)",__FILE__,__LINE__)
      CALL MPI_COMM_RANK(MPI_COMM_WORLD,K,ERROR)
      CALL MPI_SEND(K,1,MPI_INTEGER4,0,ITAG+2,KCOMM,ERROR)
      CALL CHECK_ERROR("from MPI_SEND(RANK_WORLD)",__FILE__,__LINE__)
      CALL MPI_SEND(MAXTH,1,MPI_INTEGER4,0,ITAG+3,KCOMM,ERROR)
      CALL CHECK_ERROR("from MPI_SEND(MAXTH)",__FILE__,__LINE__)
      CLSTR = CDSTRING
      CALL MPI_SEND(CLSTR,LEN(CLSTR),MPI_BYTE,0,ITAG+4,KCOMM,ERROR)
      CALL CHECK_ERROR("from MPI_SEND(CLSTR)",__FILE__,__LINE__)
      CALL MPI_SEND(COREIDS,MAXTH,MPI_INTEGER4,0,ITAG+5,KCOMM,ERROR)
      CALL CHECK_ERROR("from MPI_SEND(COREIDS)",__FILE__,__LINE__)
      CALL MPI_RECV(IAM_NODEMASTER,1,MPI_INTEGER4,0,ITAG+6,KCOMM,IRECV_STATUS,ERROR)
      CALL CHECK_ERROR("from MPI_RECV(IAM_NODEMASTER)",__FILE__,__LINE__)
   ENDIF
   DEALLOCATE(COREIDS)
   LLFIRST_TIME = .FALSE.
   CALL CONDBARR()
   CALL CHECK_ERROR("from MPI_BARRIER near LLFIRST_TIME=.FALSE.",__FILE__,__LINE__)
ENDIF

IF (MYPROC == 0 .or. IAM_NODEMASTER == 1) CALL SLASH_PROC

HEAP_SIZE=GETMAXHWM()/ONEMEGA
TASKSMALL=GETMAXRSS()/ONEMEGA

IF (MYPROC == 0) THEN
   CALL DATE_AND_TIME(CLDATEOD,CLTIMEOD,CLZONEOD,IVALUES)
   READ(CLDATEOD(5:6),'(I2)') IMON
   IF (.not.LLNOCOMM .AND. KCALL /= 1) CALL PRT_DETAIL(KULOUT)
   IF (.not.LLNOHDR)  CALL PRT_HDR(KULOUT)
   IF(KU == -1) THEN
      IF (KCALL /= 1) CALL PRT_DETAIL(0)
      CALL PRT_HDR(0)
   ENDIF

   ! Note: MYPROC == 0 is always at the RN(0) i.e. at the first NODENUM
   TOT_ENERGY = ENERGY
   MAXPOWER = POWER
   AVGPOWER = POWER
   CLMAXNODE = NODENAME
   LASTNODE = NODENAME

   NN = NUMNODES
   IF (LLNOCOMM) NN=1

   IF (NPROC > 1) THEN
      ALLOCATE(DONE(1:NPROC-1))
      DONE(:) = .FALSE.
   ENDIF

   DO NODENUM=1,NN
      JID = 0
      DO II=1,NPROC-1
         IF (.NOT.DONE(II)) THEN
            J = II ! Used to be REF(II) -- don't know why ?!
            IF (RN(J)%NODENUM == NODENUM) THEN
               I = RN(J)%RANK
               IF (RN(J)%NODEMASTER == 1) THEN ! Always the first task on particular NODENUM
                  LASTNODE = RN(J)%NODE
                  NRECV = SIZE(RECVBUF)
                  JID = J ! Always >= 1
               ELSE
                  NRECV = 2
               ENDIF
               CALL MPI_RECV(RECVBUF,NRECV,MPI_INTEGER8,I,ITAG+5,KCOMM,IRECV_STATUS,ERROR)
               CALL CHECK_ERROR("from MPI_RECV(RECVBUF)",__FILE__,__LINE__)
               IF (NRECV > 2) THEN
                  HEAP_SIZE=RECVBUF(1)
                  TASKSMALL=RECVBUF(2)
                  ENERGY=RECVBUF(3)
                  POWER=RECVBUF(4)
                  NODEHUGE=RECVBUF(5)
                  MEMFREE=RECVBUF(6)
                  CACHED=RECVBUF(7)
                  DO K=0,MAXNUMA-1
                     SMALLPAGE(K) = RECVBUF(7+2*K+1)
                     HUGEPAGE(K) = RECVBUF(7+2*K+2)
                  ENDDO
                  TOT_ENERGY = TOT_ENERGY + ENERGY
                  IF (POWER > MAXPOWER) THEN
                     MAXPOWER = POWER
                     CLMAXNODE = LASTNODE
                  ENDIF
                  AVGPOWER = AVGPOWER + POWER
               ELSE
                  HEAP_SIZE=HEAP_SIZE+RECVBUF(1)
                  TASKSMALL=TASKSMALL+RECVBUF(2)
               ENDIF
               DONE(II) = .TRUE.
            ENDIF
         ENDIF
      ENDDO
      
      PERCENT_USED(2) = 0
      IF (NODEHUGE == 0 .or. HEAP_SIZE >= NODEHUGE) THEN
         ! running with small pages
         IF (TASKSMALL+NODEHUGE+MEMFREE+CACHED > 0) THEN
            PERCENT_USED(1) = 100.0*(TASKSMALL+NODEHUGE)/(TASKSMALL+NODEHUGE+MEMFREE+CACHED)
         ELSE
            PERCENT_USED(1) = 0
         ENDIF
         CSTAR = " Sm/p"
      ELSE
         ! running with huge pages
         PERCENT_USED(1) = 100.0*(HEAP_SIZE+TASKSMALL)/(TASKSMALL+NODEHUGE+MEMFREE+CACHED)
         NFREE = 0
         IF (NNUMA > 0) NFREE = SUM(HUGEPAGE(0:NNUMA-1))
         PERCENT_USED(2) = (100.0*(NODEHUGE - NFREE))/NODEHUGE
         IF (PERCENT_USED(2) < 0) PERCENT_USED(2) = 0
         IF (PERCENT_USED(2) > 100) PERCENT_USED(2) = 100
         CSTAR = " Hg/p"
      ENDIF
   
      IF (LLNOCOMM) THEN
         ID_STRING = CSTAR
      ELSE IF (KCALL == 0 .AND. JID > 0) THEN
         ! This should signify the compute & I/O nodes (if they are separate)
         CLSTR = RN(JID)%STR
         ID_STRING = CSTAR//":"//TRIM(CLSTR)
      ELSE
         ID_STRING = CSTAR//":"//CDSTRING
      ENDIF

      CALL PRT_DATA(KULOUT)
      IF (KU == -1) THEN
         CALL PRT_DATA(0)
         IF (NODENUM == NN) THEN
            AVGPOWER = NINT(REAL(AVGPOWER)/REAL(NN))
            CALL PRT_TOTAL_ENERGIES(0)
            CALL PRT_TOTAL_ENERGIES(KULOUT)
            IF (KCALL == 1) THEN
               CALL DATE_AND_TIME(CLDATEOD,CLTIMEOD,CLZONEOD,IVALUES)
               READ(CLDATEOD(5:6),'(I2)') IMON
               CALL PRT_DETAIL(0)
               CALL PRT_DETAIL(KULOUT)
            ENDIF
            CALL PRT_EMPTY(KULOUT,1)
            CLOSE(KULOUT)
         ENDIF
      ENDIF
   ENDDO ! DO NODENUM=1,NN
   IF (ALLOCATED(DONE)) DEALLOCATE(DONE)
ELSE
    SENDBUF(1)=HEAP_SIZE
    SENDBUF(2)=TASKSMALL
    IF (IAM_NODEMASTER == 1) THEN
       SENDBUF(3)=ENERGY
       SENDBUF(4)=POWER
       SENDBUF(5)=NODEHUGE
       SENDBUF(6)=MEMFREE
       SENDBUF(7)=CACHED
       DO K=0,MAXNUMA-1
          SENDBUF(7+2*K+1)=SMALLPAGE(K)
          SENDBUF(7+2*K+2)=HUGEPAGE(K)
       ENDDO
       NSEND = SIZE(SENDBUF)
    ELSE
       NSEND = 2
    ENDIF
    CALL MPI_SEND(SENDBUF,NSEND,MPI_INTEGER8,0,ITAG+5,KCOMM,ERROR)
    CALL CHECK_ERROR("from MPI_SEND(SENDBUF)",__FILE__,__LINE__)
ENDIF

IF (.not.LLNOCOMM) THEN
   CALL CONDBARR()
   CALL CHECK_ERROR("from MPI_BARRIER(at end)",__FILE__,__LINE__)
ENDIF

CONTAINS

SUBROUTINE SLASH_PROC
  IMPLICIT NONE
  CALL EC_PMON(ENERGY,POWER)

  N18 = 0 ! number of buddy columns (up to MAXCOLS)
  NNUMA = 0 ! number of NUMA-nodes (up to MAXNUMA)

  OPEN(FILE="/proc/buddyinfo",UNIT=502,STATUS="old",ACTION="read",ERR=97)
  
  READ(502,'(a)',END=99) LINE
  READ(502,'(a)',END=99) LINE
  READ(502,'(a)',END=99) LINE
  NODE(:,0)=-1
  READ(LINE(22:),*,END=98) NODE(:,0)
98 CONTINUE
  N18 = COUNT(NODE(:,0) >= 0)
  NNUMA = 1
  DO K=1,MAXNUMA-1
     NODE(:,K)=0
     READ(502,'(a)',END=99) LINE
     READ(LINE(22:),*) NODE(0:N18-1,K)
     NNUMA = NNUMA + 1
  ENDDO
  
99 CONTINUE
  CLOSE(502)
97 CONTINUE
  
  SMALLPAGE(:) = 0
  HUGEPAGE(:) = 0
  DO K=0,NNUMA-1
     BUCKET(:,K) = 0
     DO J=0,N18-1
        BUCKET(J,K) = 4096_JPIB * NODE(J,K) * (2_JPIB ** J)
     ENDDO
     SMALLPAGE(K) = SUM(BUCKET(0:8,K))/ONEMEGA
     HUGEPAGE(K) = SUM(BUCKET(9:N18-1,K))/ONEMEGA
  ENDDO
  
  MEMFREE = 0
  CACHED = 0
  
  INQUIRE(FILE="/proc/meminfo", EXIST=FILE_EXISTS)
  IF( FILE_EXISTS ) THEN
    OPEN(FILE="/proc/meminfo",UNIT=502,STATUS="old",ACTION="read",ERR=977)
    DO I=1,10
      READ(502,'(a)',ERR=988,END=988) LINE
      IF(LINE(1:7) == "MemFree") THEN
        READ(LINE(9:80),*) MEMFREE 
      ELSEIF(LINE(1:6) == "Cached") THEN
        READ(LINE(8:80),*) CACHED
      ENDIF
    ENDDO
988 continue
    CLOSE(502)
977 continue
  
    MEMFREE=MEMFREE/1024
    CACHED=CACHED/1024
  ENDIF

END SUBROUTINE SLASH_PROC

SUBROUTINE PRT_EMPTY(KUN,KOUNT)
IMPLICIT NONE
INTEGER(KIND=JPIM), INTENT(IN) :: KUN,KOUNT
INTEGER(KIND=JPIM) :: JJ
DO JJ=1,KOUNT
   WRITE(KUN,'(a)')  CLPFX(1:IPFXLEN)//"## EC_MEMINFO "
ENDDO
END SUBROUTINE PRT_EMPTY

FUNCTION KWH(JOULES)
IMPLICIT NONE
INTEGER(KIND=JPIB), INTENT(IN) :: JOULES
REAL(KIND=JPRD) KWH
KWH = REAL(JOULES,JPRD) / 3600000.0_JPRD
END FUNCTION KWH

SUBROUTINE PRT_TOTAL_ENERGIES(KUN)
IMPLICIT NONE
INTEGER(KIND=JPIM), INTENT(IN) :: KUN
IF (KCALL == 1) THEN ! last call
   WT = UTIL_WALLTIME() - WT0
   CALL PRT_EMPTY(KUN,2)
   WRITE(KUN,'(a,a,f12.3,a,i0,a)')  CLPFX(1:IPFXLEN)//"## EC_MEMINFO ",&
        & " Total energy consumed : ",KWH(TOT_ENERGY), " kWh (",TOT_ENERGY," J)"
!-- Peak power below is misleading since based on values at sample points
!   WRITE(KUN,'(a,a,i0,a)')  CLPFX(1:IPFXLEN)//"## EC_MEMINFO ",&
!        & " Peak power            : ",MAXPOWER," W (node "//trim(CLMAXNODE)//")"
!-- Avg power must be calculated based on total Joules divided by wall time and num nodes
   AVGPOWER = TOT_ENERGY / WT / NUMNODES
   WRITE(KUN,'(a,a,i0,a,i0,a)')  CLPFX(1:IPFXLEN)//"## EC_MEMINFO ",&
        & " Avg. power / node     : ",AVGPOWER," W across ",NUMNODES," nodes"
   CALL PRT_EMPTY(KUN,1)
ENDIF
END SUBROUTINE PRT_TOTAL_ENERGIES

SUBROUTINE PRT_DETAIL(KUN)
IMPLICIT NONE
INTEGER(KIND=JPIM), INTENT(IN) :: KUN
CHARACTER(LEN=128) :: JOBNAME
CHARACTER(LEN=128) :: JOBID
CALL GET_ENVIRONMENT_VARIABLE('EC_JOB_NAME',JOBNAME)
IF (JOBNAME == '') CALL GET_ENVIRONMENT_VARIABLE('PBS_JOBNAME',JOBNAME)
IF (JOBNAME == '') CALL GET_ENVIRONMENT_VARIABLE('SLURM_JOB_NAME',JOBNAME)
IF (JOBNAME == '') CALL GET_ENVIRONMENT_VARIABLE('EC_MEMINFO_JOBNAME',JOBNAME)
CALL GET_ENVIRONMENT_VARIABLE('PBS_JOBID',JOBID)
IF (JOBID == '') CALL GET_ENVIRONMENT_VARIABLE('SLURM_JOB_ID',JOBID)
IF (JOBID == '') CALL GET_ENVIRONMENT_VARIABLE('EC_MEMINFO_JOBID',JOBID)
CALL PRT_EMPTY(KUN,1)
WT = UTIL_WALLTIME() - WT0
WRITE(KUN,'(4a,f10.3,a)') CLPFX(1:IPFXLEN)//"## EC_MEMINFO Detailed memory information ", &
     "for program ",TRIM(PROGRAM)," -- wall-time : ",WT,"s"
WRITE(KUN,'(a,i0,a,i0,a,i0,a,i0,a,i0,a,i0,a,a,":",a,":",a,a,a,"-",a,"-",a)') &
     CLPFX(1:IPFXLEN)//"## EC_MEMINFO Running on ",NUMNODES," nodes (",NNUMA,&
     "-numa) with ",NPROC-IOTASKS, &
     " compute + ",IOTASKS," I/O-tasks and ", MAXTH_COMP, "+", MAXTH_IO, " threads at ", &
     CLTIMEOD(1:2),CLTIMEOD(3:4),CLTIMEOD(5:10), &
     " on ",CLDATEOD(7:8),CLMON(IMON),CLDATEOD(1:4)
WRITE(KUN,'(4a)') CLPFX(1:IPFXLEN)//"## EC_MEMINFO The Job Name is ",TRIM(JOBNAME), &
     " and the Job ID is ",TRIM(JOBID)
CALL PRT_EMPTY(KUN,1)
END SUBROUTINE PRT_DETAIL

SUBROUTINE PRT_HDR(KUN)
IMPLICIT NONE
INTEGER(KIND=JPIM), INTENT(IN) :: KUN
INTEGER(KIND=JPIM) :: INUMA, ILEN
CHARACTER(LEN=4096) :: CLBUF
INUMA = NNUMA

ILEN = 0
WRITE(CLBUF(ILEN+1:),'(A)') &
     CLPFX(1:IPFXLEN)//"## EC_MEMINFO                           | TC    | MEMORY USED(MB) "
ILEN = LEN_TRIM(CLBUF)
DO K=0,INUMA-1
   IF (K == 0) THEN
      WRITE(CLBUF(ILEN+1:),'(A)') " | MEMORY FREE(MB)"
      ILEN = LEN_TRIM(CLBUF)
   ELSE
      WRITE(CLBUF(ILEN+1:),'(A)') "  -------------  "
      ILEN = LEN_TRIM(CLBUF) + 2
   ENDIF
ENDDO
IF (NNUMA > 0) THEN
   WRITE(CLBUF(ILEN+1:),'(A)') " INCLUDING CACHED|  %USED %HUGE  | Energy  Power"
ELSE
   WRITE(CLBUF(ILEN+1:),'(A)') "  MEMORY FREE(MB) |  %USED %HUGE  | Energy  Power"
ENDIF
WRITE(KUN,'(A)') TRIM(CLBUF)

ILEN=0
WRITE(CLBUF(ILEN+1:),'(A)') &
     CLPFX(1:IPFXLEN)//"## EC_MEMINFO                           | Malloc| Inc Heap        |"
ILEN = LEN_TRIM(CLBUF)
DO K=0,INUMA-1
   WRITE(CLBUF(ILEN+1:),'(A,I2,A)') " Numa region ",K," |"
   ILEN = LEN_TRIM(CLBUF)
ENDDO
WRITE(CLBUF(ILEN+1:),'(A)')  "                |               |    (J)    (W)"
WRITE(KUN,'(A)') TRIM(CLBUF)

ILEN=0
WRITE(CLBUF(ILEN+1:),'(A)') &
     CLPFX(1:IPFXLEN)//"## EC_MEMINFO Node Name                 | Heap  | RSS("//zum//")        |"
ILEN = LEN_TRIM(CLBUF)
DO K=0,INUMA-1
   WRITE(CLBUF(ILEN+1:),'(A)') " Small  Huge or |"
   ILEN = LEN_TRIM(CLBUF)
ENDDO
WRITE(CLBUF(ILEN+1:),'(A)') " Total          |"
WRITE(KUN,'(A)') TRIM(CLBUF)

ILEN=0
WRITE(CLBUF(ILEN+1:),'(A)') &
     CLPFX(1:IPFXLEN)//"## EC_MEMINFO                           | (sum) | Small    Huge   |"
ILEN = LEN_TRIM(CLBUF)
DO K=0,INUMA-1
   WRITE(CLBUF(ILEN+1:),'(A)') "  Only   Small  |"
   ILEN = LEN_TRIM(CLBUF)
ENDDO
WRITE(CLBUF(ILEN+1:),'(A)') " Memfree+Cached |"
WRITE(KUN,'(A)') TRIM(CLBUF)
END SUBROUTINE PRT_HDR

SUBROUTINE PRT_DATA(KUN)
IMPLICIT NONE
INTEGER(KIND=JPIM), INTENT(IN) :: KUN
INTEGER(KIND=JPIM) :: INUMA,ILEN
CHARACTER(LEN=4096) :: CLBUF
INUMA = NNUMA

ILEN=0
WRITE(CLBUF(ILEN+1:),'(a,i4,1x,a,3i8,1x)') &
     CLPFX(1:IPFXLEN)//"## EC_MEMINFO ", &
     NODENUM-1,LASTNODE,HEAP_SIZE,TASKSMALL,NODEHUGE
ILEN = LEN_TRIM(CLBUF) + 1
DO K=0,INUMA-1
   WRITE(CLBUF(ILEN+1:),'(1x,2i8)') SMALLPAGE(K),HUGEPAGE(K)
   ILEN = LEN_TRIM(CLBUF)
ENDDO
WRITE(CLBUF(ILEN+1:),'(2x,2i8,3x,2f6.1,1x,i9,1x,i6,1x,a)') &
     MEMFREE,CACHED, &
     PERCENT_USED,&
     ENERGY,POWER,&
     trim(ID_STRING)
WRITE(KUN,'(A)') TRIM(CLBUF)
END SUBROUTINE PRT_DATA

SUBROUTINE CONDBARR()
IF (NPROC > 1 .and. KBARR /= 0) THEN
   CALL MPI_BARRIER(KCOMM,ERROR)
ELSE
   ERROR = 0
ENDIF
END SUBROUTINE CONDBARR

SUBROUTINE CHECK_ERROR(CLWHAT,SRCFILE,SRCLINE)
IMPLICIT NONE
CHARACTER(LEN=*), INTENT(IN) :: CLWHAT, SRCFILE
INTEGER(KIND=JPIM), INTENT(IN) :: SRCLINE
IF (ERROR /= 0) THEN
   WRITE(0,'(A,I0,1X,A,1X,"(",A,":",I0,")")') &
        & CLPFX(1:IPFXLEN)//"## EC_MEMINFO error code =",ERROR,CLWHAT,SRCFILE,SRCLINE
   CALL MPI_ABORT(KCOMM,-1,ERROR)
ENDIF
ERROR = 0
END SUBROUTINE CHECK_ERROR

SUBROUTINE RNSORT(KUN)
IMPLICIT NONE
INTEGER(KIND=JPIM), INTENT(IN) :: KUN
INTEGER(KIND=JPIM) :: ILEN
CHARACTER(LEN=1) :: CLAST
CHARACTER(LEN=4) :: CLMASTER
CHARACTER(LEN=4096) :: CLBUF
INTEGER(KIND=JPIM) :: impi_vers, impi_subvers, ilibrary_version_len
INTEGER(KIND=JPIM) :: iomp_vers, iomp_subvers, iopenmp
CHARACTER(LEN=4096) :: clibrary_version
LOGICAL :: LLDONE(0:NPROC-1)
INTEGER(KIND=JPIM) :: REF(0:NPROC-1) ! Keep list of the order tasks been added
LLDONE(:) = .FALSE.
IOTASKS = 0
K = 0
NODENUM = 0
DO I=0,NPROC-1
   IF (RN(I)%NODENUM == -1) THEN
      IF (RN(I)%IORANK == 1) THEN
         IOTASKS = IOTASKS + 1
         RN(I)%IORANK = IOTASKS
      ELSE
         RN(I)%IORANK = 0
      ENDIF
      NODENUM = NODENUM + 1
      RN(I)%NODENUM = NODENUM
      RN(I)%NODEMASTER = 1
      LLDONE(I) = .TRUE.
      ! NB: Adjacent REF-elements allow us to operate with particular node's tasks that follow their the node-master
      REF(K) = I
      K = K + 1
      LASTNODE = RN(I)%NODE
!      DO J=I+1,NPROC-1 ! not valid anymore since ranks might have been reordered -- need to run through the whole list -- LLNODE speeds up
      DO J=0,NPROC-1
         IF (.NOT.LLDONE(J)) THEN
            IF (RN(J)%NODENUM == -1) THEN
               IF (RN(J)%NODE == LASTNODE) THEN
                  RN(J)%NODENUM = NODENUM
                  IF (RN(J)%IORANK == 1) THEN
                     IOTASKS = IOTASKS + 1
                     RN(J)%IORANK = IOTASKS
                  ELSE
                     RN(J)%IORANK = 0
                  ENDIF
                  RN(J)%NODEMASTER = 0
                  LLDONE(J) = .TRUE.
                  REF(K) = J
                  K = K + 1
               ENDIF
            ENDIF
         ENDIF
      ENDDO
   ENDIF
ENDDO
NUMNODES = NODENUM
CALL ecmpi_version(impi_vers, impi_subvers, clibrary_version, ilibrary_version_len)
call ecomp_version(iomp_vers, iomp_subvers, iopenmp)
CALL PRT_EMPTY(KUN,1)
WRITE(KUN,'(a,i0,".",i0)') &
     & CLPFX(1:IPFXLEN)//&
     & "## EC_MEMINFO : MPI-version ",impi_vers, impi_subvers
WRITE(KUN,'(a)')  &
     & CLPFX(1:IPFXLEN)//&
     & "## EC_MEMINFO : Start of MPI-library version"
WRITE(KUN,'(a)') trim(clibrary_version) ! This is could be a multiline, very long string
WRITE(KUN,'(a)')  &
     & CLPFX(1:IPFXLEN)//&
     & "## EC_MEMINFO : End of MPI-library version"
WRITE(KUN,'(a,i0,".",i0,".",i6.6)') &
     & CLPFX(1:IPFXLEN)//&
     & "## EC_MEMINFO : OpenMP-version ",iomp_vers, iomp_subvers, iopenmp
CALL PRT_EMPTY(KUN,2)
WRITE(KUN,1003) &
     & CLPFX(1:IPFXLEN)//&
     &"## EC_MEMINFO ********************************************************************************",&
     & CLPFX(1:IPFXLEN)//&
     &"## EC_MEMINFO *** Mapping of MPI & I/O-tasks to nodes and tasks' thread-to-core affinities ***", &
     & CLPFX(1:IPFXLEN)//&
     &"## EC_MEMINFO ********************************************************************************"
1003 FORMAT((A))
CALL PRT_EMPTY(KUN,1)
WRITE(KUN,'(a,i0,a,i0,a,i0,a,i0,a,i0,a,i0,a)') &
     & CLPFX(1:IPFXLEN)//"## EC_MEMINFO Running on ",NUMNODES," nodes (",NNUMA,&
     & "-numa) with ",NPROC-IOTASKS, &
     & " compute + ",IOTASKS," I/O-tasks and ", MAXTH_COMP, "+", MAXTH_IO, " threads"
CALL PRT_EMPTY(KUN,1)
WRITE(KUN,1000) CLPFX(1:IPFXLEN)//"## EC_MEMINFO ",&
     & "#","NODE#","NODENAME","MPI#","WORLD#","I/O#","MASTER","REF#","OMP#","Core affinities"
WRITE(KUN,1000) CLPFX(1:IPFXLEN)//"## EC_MEMINFO ",&
     & "=","=====","========","====","======","====","======","====","====","==============="
1000 FORMAT(A,2(1X,A5),1X,A20,6(1X,A6),2X,A)
CALL PRT_EMPTY(KUN,1)
DO K=0,NPROC-1 ! Loop over the task as they have been added (see few lines earlier how REF(K) has been getting its values I or J)
   ILEN = 0
   ! A formidable trick ? No need for a nested loop over 0:NPROC-1 to keep tasks within the same node together in the output
   I = REF(K)
   NUMTH = RN(I)%NUMTH
   CLMASTER = '[No]'
   IF (RN(I)%NODEMASTER == 1) CLMASTER = ' Yes'
   IF (RN(I)%IORANK > 0) THEN
      WRITE(CLBUF(ILEN+1:),1001) &
           & CLPFX(1:IPFXLEN)//"## EC_MEMINFO ",&
           & K,RN(I)%NODENUM-1,TRIM(ADJUSTL(RN(I)%NODE)),RN(I)%RANK,RN(I)%RANK_WORLD,RN(I)%IORANK-1,&
           & CLMASTER,I,NUMTH,"{"
1001  FORMAT(A,2(1X,I5),1X,A20,3(1X,I6),1X,A6,2(1X,I6),2X,A)
   ELSE
      WRITE(CLBUF(ILEN+1:),1002) &
           & CLPFX(1:IPFXLEN)//"## EC_MEMINFO ",&
           & K,RN(I)%NODENUM-1,TRIM(ADJUSTL(RN(I)%NODE)),RN(I)%RANK,RN(I)%RANK_WORLD,"[No]",&
           & CLMASTER,I,NUMTH,"{"
1002  FORMAT(A,2(1X,I5),1X,A20,2(1X,I6),2(1X,A6),2(1X,I6),2X,A)
   ENDIF
   ILEN = LEN_TRIM(CLBUF)
   CLAST = ','
   DO J=0,NUMTH-1
      IF (J == NUMTH-1) CLAST = '}'
      WRITE(CLBUF(ILEN+1:),'(I0,A1)') RN(I)%COREIDS(J),CLAST
      ILEN = LEN_TRIM(CLBUF)
   ENDDO
   WRITE(KUN,'(A,1X)') TRIM(CLBUF)
ENDDO
CALL PRT_EMPTY(KUN,1)
CALL FLUSH(KUN)
END SUBROUTINE RNSORT

END SUBROUTINE EC_MEMINFO

END MODULE EC_MEMINFO_MOD

