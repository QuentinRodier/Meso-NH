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

#ifdef MNH_MPI_DOUBLE_PRECISION
#define MPI_FLOAT MPI_DOUBLE_PRECISION
#else
#define MPI_FLOAT MPI_REAL
#endif

MODULE MODE_FMREAD
!
!Correction :
!  J.Escobar : 22/08/2005 : BUG : manque un "GOTO 1000" si champs
!              lue non trouvé !!!
!  J.Escobar : 13/01/2015 : remove comment on BCAST(IRESP in FMREADX2_ll
!  J.Escobar : 15/09/2015 : WENO5 & JPHEXT <> 1 
!
USE MODD_IO_ll, ONLY : NVERB_FATAL,NVERB_ERROR,NVERB_WARNING,NVERB_INFO,NVERB_DEBUG,TFILEDATA
USE MODD_MPIF
!
USE MODE_FIELD
#if defined(MNH_IOCDF4)
USE MODE_NETCDF
#endif
USE MODE_MSG
USE MODE_READWRITE_LFI

IMPLICIT NONE 

PRIVATE

INTERFACE IO_READ_FIELD
   MODULE PROCEDURE IO_READ_FIELD_BYNAME_N0,  &
                    IO_READ_FIELD_BYNAME_L0,  &
                    IO_READ_FIELD_BYNAME_C0,  &
                    IO_READ_FIELD_BYFIELD_N0, &
                    IO_READ_FIELD_BYFIELD_L0, &
                    IO_READ_FIELD_BYFIELD_C0
!                       IO_READ_FIELD_BYNAME_X0, IO_READ_FIELD_BYNAME_X1,  &
!                       IO_READ_FIELD_BYNAME_X2, IO_READ_FIELD_BYNAME_X3,  &
!                       IO_READ_FIELD_BYNAME_X4, IO_READ_FIELD_BYNAME_X5,  &
!                       IO_READ_FIELD_BYNAME_X6,                            &
!                       IO_READ_FIELD_BYNAME_N1,  &
!                       IO_READ_FIELD_BYNAME_N2, IO_READ_FIELD_BYNAME_N3,  &
!                       IO_READ_FIELD_BYNAME_L1,  &
!                       IO_READ_FIELD_BYNAME_C1,  &
!                       IO_READ_FIELD_BYNAME_T0,                            &
!                       IO_READ_FIELD_BYFIELD_X0,IO_READ_FIELD_BYFIELD_X1, &
!                       IO_READ_FIELD_BYFIELD_X2,IO_READ_FIELD_BYFIELD_X3, &
!                       IO_READ_FIELD_BYFIELD_X4,IO_READ_FIELD_BYFIELD_X5, &
!                       IO_READ_FIELD_BYFIELD_X6,                           &
!                       IO_READ_FIELD_BYFIELD_N1, &
!                       IO_READ_FIELD_BYFIELD_N2,IO_READ_FIELD_BYFIELD_N3, &
!                       IO_READ_FIELD_BYFIELD_L1, &
!                       IO_READ_FIELD_BYFIELD_C1, &
!                       IO_READ_FIELD_BYFIELD_T0
END INTERFACE

INTERFACE FMREAD
  MODULE PROCEDURE FMREADX0_ll,FMREADX1_ll,FMREADX2_ll,FMREADX3_ll,&
       & FMREADX4_ll,FMREADX5_ll,FMREADX6_ll,&
       & FMREADN0_ll,FMREADN1_ll,FMREADN2_ll,&
       & FMREADL0_ll,FMREADL1_ll,FMREADC0_ll,FMREADT0_ll
END INTERFACE
!

PUBLIC FMREAD_LB,FMREAD,FMREADX0_ll,FMREADX1_ll,FMREADX2_ll,FMREADX3_ll,&
       & FMREADX4_ll,FMREADX5_ll,FMREADX6_ll,&
       & FMREADN0_ll,FMREADN1_ll,FMREADN2_ll,&
       & FMREADL0_ll,FMREADL1_ll,FMREADC0_ll,FMREADT0_ll
PUBLIC IO_READ_FIELD

CONTAINS 
SUBROUTINE FM_READ_ERR(HFUNC,HFILEM,HFIPRI,HRECFM,HDIR,KRESP)
USE MODE_FM, ONLY : FMLOOK_ll

CHARACTER(LEN=*) :: HFUNC 
CHARACTER(LEN=*) :: HFILEM
CHARACTER(LEN=*) :: HFIPRI
CHARACTER(LEN=*) :: HRECFM
CHARACTER(LEN=*) :: HDIR
INTEGER          :: KRESP

INTEGER          :: ILUPRI
INTEGER          :: IRESP

CALL FMLOOK_ll(HFIPRI,HFIPRI,ILUPRI,IRESP)
WRITE (ILUPRI,*) ' exit from ',HFUNC, ' with RESP:',KRESP
!STOP "fmread_ll.f90:: FM_READ_ERR"

WRITE (ILUPRI,*) '   | HFILEM = ',HFILEM
WRITE (ILUPRI,*) '   | HRECFM = ',HRECFM
WRITE (ILUPRI,*) '   | HDIR  = ',HDIR

END SUBROUTINE FM_READ_ERR


SUBROUTINE BCAST_HEADER(TPFD,TPFMH)
USE MODE_FD_ll, ONLY : FD_ll
USE MODD_FM
TYPE(FD_ll),     POINTER      :: TPFD
TYPE(FMHEADER), INTENT(INOUT) :: TPFMH

INTEGER :: ierr 

CALL MPI_BCAST(TPFMH%GRID,1,MPI_INTEGER,TPFD%OWNER-1,TPFD%COMM,IERR)
CALL MPI_BCAST(TPFMH%COMLEN,1,MPI_INTEGER,TPFD%OWNER-1,TPFD%COMM,IERR)
CALL MPI_BCAST(TPFMH%COMMENT,TPFMH%COMLEN,MPI_CHARACTER,TPFD%OWNER-1,TPFD%COMM,IERR)

END SUBROUTINE BCAST_HEADER

SUBROUTINE IO_BCAST_FIELD_METADATA(TPFD,TPFIELD)
USE MODE_FD_ll, ONLY : FD_ll
TYPE(FD_ll), POINTER, INTENT(IN)    :: TPFD
TYPE(TFIELDDATA),     INTENT(INOUT) :: TPFIELD
!
INTEGER :: IERR
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_BCAST_FIELD_METADATA','called for '//TRIM(TPFIELD%CMNHNAME))
!
CALL MPI_BCAST(TPFIELD%CMNHNAME, LEN(TPFIELD%CMNHNAME), MPI_CHARACTER,TPFD%OWNER-1,TPFD%COMM,IERR)
CALL MPI_BCAST(TPFIELD%CSTDNAME, LEN(TPFIELD%CSTDNAME), MPI_CHARACTER,TPFD%OWNER-1,TPFD%COMM,IERR)
CALL MPI_BCAST(TPFIELD%CLONGNAME,LEN(TPFIELD%CLONGNAME),MPI_CHARACTER,TPFD%OWNER-1,TPFD%COMM,IERR)
CALL MPI_BCAST(TPFIELD%CUNITS,   LEN(TPFIELD%CUNITS),   MPI_CHARACTER,TPFD%OWNER-1,TPFD%COMM,IERR)
CALL MPI_BCAST(TPFIELD%CDIR,     LEN(TPFIELD%CDIR),     MPI_CHARACTER,TPFD%OWNER-1,TPFD%COMM,IERR)
CALL MPI_BCAST(TPFIELD%CLBTYPE,  LEN(TPFIELD%CLBTYPE),  MPI_CHARACTER,TPFD%OWNER-1,TPFD%COMM,IERR)
CALL MPI_BCAST(TPFIELD%CCOMMENT, LEN(TPFIELD%CCOMMENT), MPI_CHARACTER,TPFD%OWNER-1,TPFD%COMM,IERR)
CALL MPI_BCAST(TPFIELD%NGRID,    1,                     MPI_INTEGER,  TPFD%OWNER-1,TPFD%COMM,IERR)
CALL MPI_BCAST(TPFIELD%NTYPE,    1,                     MPI_INTEGER,  TPFD%OWNER-1,TPFD%COMM,IERR)
CALL MPI_BCAST(TPFIELD%NDIMS,    1,                     MPI_INTEGER,  TPFD%OWNER-1,TPFD%COMM,IERR)
!
END SUBROUTINE IO_BCAST_FIELD_METADATA

SUBROUTINE FMREADX0_ll(HFILEM,HRECFM,HFIPRI,HDIR,PFIELD,KGRID,&
     KLENCH,HCOMMENT,KRESP)
USE MODD_IO_ll, ONLY : ISP,GSMONOPROC 
USE MODD_FM
USE MODE_FD_ll, ONLY : GETFD,JPFINL,FD_LL
!
!*      0.    DECLARATIONS
!             ------------
!
!
!*      0.1   Declarations of arguments
!
CHARACTER(LEN=*),             INTENT(IN) ::HFILEM ! FM-file name
CHARACTER(LEN=*),             INTENT(IN) ::HRECFM ! name of the article to read
CHARACTER(LEN=*),             INTENT(IN) ::HFIPRI ! output file for error messages
CHARACTER(LEN=*),             INTENT(IN) ::HDIR   ! field form
REAL,                         INTENT(INOUT)::PFIELD ! array containing the data field 
INTEGER,                      INTENT(INOUT)::KGRID  ! C-grid indicator (u,v,w,T)
INTEGER,                      INTENT(INOUT)::KLENCH ! length of comment string
CHARACTER(LEN=*),             INTENT(INOUT)::HCOMMENT ! comment string
INTEGER,                      INTENT(INOUT)::KRESP    ! return-code
!
!*      0.2   Declarations of local variables
!
!----------------------------------------------------------------
CHARACTER(LEN=JPFINL)        :: YFNLFI
INTEGER                      :: IERR
TYPE(FD_ll), POINTER         :: TZFD
INTEGER                      :: IRESP
TYPE(FMHEADER)               :: TZFMH
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','FMREADX0_ll','reading '//TRIM(HRECFM))
!
!*      1.1   THE NAME OF LFIFM
!
IRESP = 0
YFNLFI=TRIM(ADJUSTL(HFILEM))//'.lfi'
!------------------------------------------------------------------
TZFD=>GETFD(YFNLFI)
IF (ASSOCIATED(TZFD)) THEN
  IF (GSMONOPROC) THEN ! sequential execution
    IF (ASSOCIATED(TZFD%CDF)) THEN
       CALL NCREAD(TZFD%CDF%NCID,HRECFM,PFIELD,TZFMH,IRESP)
    ELSE
       CALL FM_READ_ll(TZFD%FLU,HRECFM,.TRUE.,1,PFIELD,TZFMH,IRESP)
    END IF
    IF (IRESP /= 0) GOTO 1000
  ELSE ! multiprocessor execution
    IF (ISP == TZFD%OWNER)  THEN
      IF (ASSOCIATED(TZFD%CDF)) THEN
         CALL NCREAD(TZFD%CDF%NCID,HRECFM,PFIELD,TZFMH,IRESP)
      ELSE
         CALL FM_READ_ll(TZFD%FLU,HRECFM,.TRUE.,1,PFIELD,TZFMH,IRESP)
      END IF
    END IF
    !
    CALL MPI_BCAST(IRESP,1,MPI_INTEGER,TZFD%OWNER-1,TZFD%COMM,IERR)
    IF (IRESP /= 0) GOTO 1000
    !
    CALL BCAST_HEADER(TZFD,TZFMH)
    !
    CALL MPI_BCAST(PFIELD,1,MPI_FLOAT,TZFD%OWNER-1,TZFD%COMM,IERR)
  END IF
  KGRID  = TZFMH%GRID
  KLENCH = TZFMH%COMLEN
  HCOMMENT = TZFMH%COMMENT(1:TZFMH%COMLEN)
ELSE 
  IRESP = -61
END IF
!----------------------------------------------------------------
1000 CONTINUE
!! Error handler
IF (IRESP.NE.0) THEN
  CALL FM_READ_ERR("FMREADX0_ll",HFILEM,HFIPRI,HRECFM,HDIR,IRESP)
ENDIF
KRESP = IRESP
RETURN
    
END SUBROUTINE FMREADX0_ll

SUBROUTINE FMREADX1_ll(HFILEM,HRECFM,HFIPRI,HDIR,PFIELD,KGRID,&
     KLENCH,HCOMMENT,KRESP, KIMAX_ll, KJMAX_ll, TPSPLITTING)
USE MODD_IO_ll, ONLY : ISP,GSMONOPROC, ISNPROC
USE MODD_FM
USE MODE_FD_ll, ONLY : GETFD,JPFINL,FD_LL
USE MODE_SCATTER_ll
USE MODE_ALLOCBUFFER_ll
USE MODD_STRUCTURE_ll, ONLY : ZONE_ll
!
!*      0.    DECLARATIONS
!             ------------
!
!
!*      0.1   Declarations of arguments
!
CHARACTER(LEN=*),        INTENT(IN) ::HFILEM   ! FM-file name
CHARACTER(LEN=*),        INTENT(IN) ::HRECFM   ! name of the article to read
CHARACTER(LEN=*),        INTENT(IN) ::HFIPRI   ! output file for error messages
CHARACTER(LEN=*),        INTENT(IN) ::HDIR     ! Field form
REAL,DIMENSION(:),TARGET,INTENT(INOUT)::PFIELD   ! array containing the data field 
INTEGER,                 INTENT(INOUT)::KGRID    ! C-grid indicator (u,v,w,T)
INTEGER,                 INTENT(INOUT)::KLENCH   ! length of comment string
CHARACTER(LEN=*),        INTENT(INOUT)::HCOMMENT ! comment string
INTEGER,                 INTENT(INOUT)::KRESP    ! return-code
INTEGER, OPTIONAL, INTENT(IN) ::KIMAX_ll
INTEGER, OPTIONAL, INTENT(IN) ::KJMAX_ll
TYPE(ZONE_ll), DIMENSION(ISNPROC), OPTIONAL :: TPSPLITTING  ! splitting of the domain
!
!*      0.2   Declarations of local variables
!
!----------------------------------------------------------------
CHARACTER(LEN=JPFINL)     :: YFNLFI
INTEGER                   :: IERR
TYPE(FD_ll), POINTER      :: TZFD
INTEGER                   :: IRESP
REAL,DIMENSION(:),POINTER :: ZFIELDP
LOGICAL                   :: GALLOC
TYPE(FMHEADER)            :: TZFMH
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','FMREADX1_ll','reading '//TRIM(HRECFM))
!
!*      1.1   THE NAME OF LFIFM
!
GALLOC = .FALSE.
IRESP = 0
YFNLFI=TRIM(ADJUSTL(HFILEM))//'.lfi'
!------------------------------------------------------------------
TZFD=>GETFD(YFNLFI)
IF (ASSOCIATED(TZFD)) THEN
  IF (GSMONOPROC) THEN ! sequential execution
    IF (ASSOCIATED(TZFD%CDF)) THEN
       CALL NCREAD(TZFD%CDF%NCID,HRECFM,PFIELD,TZFMH,IRESP)
    ELSE
       CALL FM_READ_ll(TZFD%FLU,HRECFM,.TRUE.,SIZE(PFIELD),PFIELD,TZFMH,IRESP)
    END IF
    IF (IRESP /= 0) GOTO 1000
  ELSE ! multiprocessor execution
    IF (ISP == TZFD%OWNER)  THEN
      IF( PRESENT(KIMAX_ll) .AND. PRESENT(KJMAX_ll) ) THEN
        CALL ALLOCBUFFER_ll(ZFIELDP,PFIELD,HDIR,GALLOC, KIMAX_ll, KJMAX_ll)
      ELSE
        CALL ALLOCBUFFER_ll(ZFIELDP,PFIELD,HDIR,GALLOC)
      ENDIF
      IF (ASSOCIATED(TZFD%CDF)) THEN
         CALL NCREAD(TZFD%CDF%NCID,HRECFM,ZFIELDP,TZFMH,IRESP)
      ELSE
         CALL FM_READ_ll(TZFD%FLU,HRECFM,.TRUE.,SIZE(ZFIELDP),ZFIELDP,TZFMH&
              & ,IRESP)
      END IF
    ELSE
      ALLOCATE(ZFIELDP(0))
      GALLOC = .TRUE.
    END IF
      
    !  
    CALL MPI_BCAST(IRESP,1,MPI_INTEGER,TZFD%OWNER-1,TZFD%COMM,IERR)
    IF (IRESP /= 0) GOTO 1000
    !
    CALL BCAST_HEADER(TZFD,TZFMH)
    !
    IF (HDIR /= 'XX' .AND. HDIR /='YY') THEN
      ! Broadcast Field
      CALL MPI_BCAST(PFIELD,SIZE(PFIELD),MPI_FLOAT,TZFD%OWNER-1,TZFD%COMM,IERR)
    ELSE 
      !Scatter Field
      IF( PRESENT(TPSPLITTING) ) THEN
        CALL SCATTER_XXFIELD(HDIR,ZFIELDP,PFIELD,TZFD%OWNER,TZFD%COMM,TPSPLITTING)
      ELSE
        CALL SCATTER_XXFIELD(HDIR,ZFIELDP,PFIELD,TZFD%OWNER,TZFD%COMM)
      ENDIF
    END IF
  END IF !(GSMONOPROC)
  
  KGRID  = TZFMH%GRID
  KLENCH = TZFMH%COMLEN
  HCOMMENT = TZFMH%COMMENT(1:TZFMH%COMLEN)
ELSE 
  IRESP = -61
END IF
!----------------------------------------------------------------
1000 CONTINUE
!! Error handler
IF (IRESP.NE.0) THEN
  CALL FM_READ_ERR("FMREADX1_ll",HFILEM,HFIPRI,HRECFM,HDIR,IRESP)
ENDIF

IF (GALLOC) DEALLOCATE (ZFIELDP)
KRESP = IRESP
RETURN
!------------------------------------------------------------------
END SUBROUTINE FMREADX1_ll

SUBROUTINE FMREADX2_ll(HFILEM,HRECFM,HFIPRI,HDIR,PFIELD,KGRID,&
     KLENCH,HCOMMENT,KRESP, KIMAX_ll, KJMAX_ll, TPSPLITTING)
USE MODD_IO_ll, ONLY : ISP,GSMONOPROC,LPACK,L1D,L2D , ISNPROC
USE MODD_PARAMETERS_ll,ONLY : JPHEXT
USE MODD_FM
USE MODE_FD_ll, ONLY : GETFD,JPFINL,FD_LL
USE MODE_SCATTER_ll
USE MODE_ALLOCBUFFER_ll
!JUANZ
USE MODD_TIMEZ, ONLY : TIMEZ
USE MODE_MNH_TIMING, ONLY : SECOND_MNH2
!JUANZ 
USE MODD_STRUCTURE_ll, ONLY : ZONE_ll
#ifdef MNH_GA
    USE MODE_GA
#endif

IMPLICIT NONE

CHARACTER(LEN=*),           INTENT(IN) ::HFILEM   ! FM-file name
CHARACTER(LEN=*),           INTENT(IN) ::HRECFM   ! name of the article to read
CHARACTER(LEN=*),           INTENT(IN) ::HFIPRI   ! output file for error messages
CHARACTER(LEN=*),           INTENT(IN) ::HDIR     ! field form
REAL,DIMENSION(:,:),TARGET, INTENT(INOUT)::PFIELD   ! array containing the data field
INTEGER,                    INTENT(INOUT)::KGRID    ! C-grid indicator (u,v,w,T)
INTEGER,                    INTENT(INOUT)::KLENCH   ! length of comment string
CHARACTER(LEN=*),           INTENT(INOUT)::HCOMMENT ! comment string
INTEGER,                   INTENT(INOUT)::KRESP     ! return-code
INTEGER, OPTIONAL, INTENT(IN) ::KIMAX_ll
INTEGER, OPTIONAL, INTENT(IN) ::KJMAX_ll
TYPE(ZONE_ll), DIMENSION(ISNPROC), OPTIONAL :: TPSPLITTING  ! splitting of the domain
!
!
!*      0.2   Declarations of local variables
!
CHARACTER(LEN=JPFINL)        :: YFNLFI

INTEGER                      :: IERR
TYPE(FD_ll), POINTER         :: TZFD
INTEGER                      :: IRESP
REAL,DIMENSION(:,:), POINTER :: ZFIELDP
LOGICAL                      :: GALLOC
TYPE(FMHEADER)               :: TZFMH
!JUANZ
REAL*8,DIMENSION(2) :: T0,T1,T2
REAL*8,DIMENSION(2) :: T11,T22
!JUANZ
#ifdef MNH_GA
REAL,DIMENSION(:,:),POINTER    :: ZFIELD_GA
#endif
INTEGER                      :: IHEXTOT
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','FMREADX2_ll','reading '//TRIM(HRECFM))
!
!*      1.1   THE NAME OF LFIFM
!
CALL SECOND_MNH2(T11)
GALLOC = .FALSE.
IRESP = 0
YFNLFI=TRIM(ADJUSTL(HFILEM))//'.lfi'

!------------------------------------------------------------------
IHEXTOT = 2*JPHEXT+1
TZFD=>GETFD(YFNLFI)
IF (ASSOCIATED(TZFD)) THEN
  IF (GSMONOPROC) THEN ! sequential execution
!    IF (LPACK .AND. L1D .AND. HDIR=='XY') THEN 
    IF (LPACK .AND. L1D .AND. SIZE(PFIELD,1)==IHEXTOT .AND. SIZE(PFIELD,2)==IHEXTOT) THEN
      ZFIELDP=>PFIELD(JPHEXT+1:JPHEXT+1,JPHEXT+1:JPHEXT+1)
      IF (ASSOCIATED(TZFD%CDF)) THEN
         CALL NCREAD(TZFD%CDF%NCID,HRECFM,ZFIELDP,TZFMH,IRESP)
      ELSE
         CALL FM_READ_ll(TZFD%FLU,HRECFM,.TRUE.,SIZE(ZFIELDP),ZFIELDP,TZFMH,IRESP)
      END IF
      PFIELD(:,:)=SPREAD(SPREAD(PFIELD(JPHEXT+1,JPHEXT+1),DIM=1,NCOPIES=IHEXTOT),DIM=2,NCOPIES=IHEXTOT)
!    ELSE IF (LPACK .AND. L2D .AND. HDIR=='XY') THEN
    ELSE IF (LPACK .AND. L2D .AND. SIZE(PFIELD,2)==IHEXTOT) THEN
      ZFIELDP=>PFIELD(:,JPHEXT+1:JPHEXT+1)
      IF (ASSOCIATED(TZFD%CDF)) THEN
         CALL NCREAD(TZFD%CDF%NCID,HRECFM,ZFIELDP,TZFMH,IRESP)
      ELSE
         CALL FM_READ_ll(TZFD%FLU,HRECFM,.TRUE.,SIZE(ZFIELDP),ZFIELDP,TZFMH,IRESP)
      END IF
      PFIELD(:,:)=SPREAD(PFIELD(:,JPHEXT+1),DIM=2,NCOPIES=IHEXTOT)
    ELSE
      IF (ASSOCIATED(TZFD%CDF)) THEN
         CALL NCREAD(TZFD%CDF%NCID,HRECFM,PFIELD,TZFMH,IRESP)
      ELSE
         CALL FM_READ_ll(TZFD%FLU,HRECFM,.TRUE.,SIZE(PFIELD),PFIELD,TZFMH,IRESP)
      END IF
    END IF
    IF (IRESP /= 0) GOTO 1000
  ELSE ! multiprocessor execution
     CALL SECOND_MNH2(T0)
    IF (ISP == TZFD%OWNER)  THEN
      ! I/O processor case
      IF( PRESENT(KIMAX_ll) .AND. PRESENT(KJMAX_ll) ) THEN
        CALL ALLOCBUFFER_ll(ZFIELDP,PFIELD,HDIR,GALLOC, KIMAX_ll, KJMAX_ll)
      ELSE
        CALL ALLOCBUFFER_ll(ZFIELDP,PFIELD,HDIR,GALLOC)
      ENDIF
      IF (ASSOCIATED(TZFD%CDF)) THEN
         CALL NCREAD(TZFD%CDF%NCID,HRECFM,ZFIELDP,TZFMH,IRESP)
      ELSE
         CALL FM_READ_ll(TZFD%FLU,HRECFM,.TRUE.,SIZE(ZFIELDP),ZFIELDP,TZFMH&
           & ,IRESP)
      END IF
    ELSE
      ALLOCATE(ZFIELDP(0,0))
      GALLOC = .TRUE.
    END IF
    CALL SECOND_MNH2(T1)
    TIMEZ%T_READ2D_READ=TIMEZ%T_READ2D_READ + T1 - T0
    !
    CALL MPI_BCAST(IRESP,1,MPI_INTEGER,TZFD%OWNER-1,TZFD%COMM,IERR)
    IF (IRESP /= 0) GOTO 1000
    !
    CALL BCAST_HEADER(TZFD,TZFMH)
    !
    IF (HDIR == 'XX' .OR. HDIR =='YY') THEN
      ! XX or YY Scatter Field
      IF( PRESENT(TPSPLITTING) ) THEN
        CALL SCATTER_XXFIELD(HDIR,ZFIELDP,PFIELD,TZFD%OWNER,TZFD%COMM,TPSPLITTING)
      ELSE
        CALL SCATTER_XXFIELD(HDIR,ZFIELDP,PFIELD,TZFD%OWNER,TZFD%COMM)
      ENDIF
    ELSE IF (HDIR == 'XY') THEN
      IF (LPACK .AND. L2D) THEN
        ! 2D compact case
      IF( PRESENT(TPSPLITTING) ) THEN
        CALL SCATTER_XXFIELD('XX',ZFIELDP(:,1),PFIELD(:,JPHEXT+1),TZFD%OWNER,TZFD%COMM,TPSPLITTING)
      ELSE
        CALL SCATTER_XXFIELD('XX',ZFIELDP(:,1),PFIELD(:,JPHEXT+1),TZFD%OWNER,TZFD%COMM)
      ENDIF
        PFIELD(:,:) = SPREAD(PFIELD(:,JPHEXT+1),DIM=2,NCOPIES=IHEXTOT)
      ELSE
#ifdef MNH_GA
         !
         ! init/create the ga , dim3 = 1
         !
         CALL MNH_INIT_GA(SIZE(PFIELD,1),SIZE(PFIELD,2),1,HRECFM,"READ")
         IF (ISP == TZFD%OWNER)  THEN
            !
            ! put the data in the g_a , this proc get this 1 slide
            !
            lo_zplan(JPIZ) = 1
            hi_zplan(JPIZ) = 1
            call nga_put(g_a, lo_zplan, hi_zplan,ZFIELDP, ld_zplan)
         END IF
         call ga_sync
         !
         ! get the columun data in this proc
         !
         ! temp buf to avoid problem with none stride PFIELDS buffer  with HALO 
         ALLOCATE (ZFIELD_GA (SIZE(PFIELD,1),SIZE(PFIELD,2)))
         call nga_get(g_a, lo_col, hi_col,ZFIELD_GA(1,1) , ld_col)
         PFIELD = ZFIELD_GA
         DEALLOCATE(ZFIELD_GA)
#else
        ! XY Scatter Field
        CALL SCATTER_XYFIELD(ZFIELDP,PFIELD,TZFD%OWNER,TZFD%COMM)
#endif
      END IF
    ELSE
      CALL MPI_BCAST(PFIELD,SIZE(PFIELD),MPI_FLOAT,TZFD%OWNER-1,TZFD%COMM,IERR)
    END IF
    CALL SECOND_MNH2(T2)
    TIMEZ%T_READ2D_SCAT=TIMEZ%T_READ2D_SCAT + T2 - T1    
  END IF !(GSMONOPROC)
  
  KGRID  = TZFMH%GRID
  KLENCH = TZFMH%COMLEN
  HCOMMENT = TZFMH%COMMENT(1:TZFMH%COMLEN)
ELSE 
  IRESP = -61
END IF
!----------------------------------------------------------------
1000 CONTINUE
!! Error handler
IF (IRESP.NE.0) THEN
  CALL FM_READ_ERR("FMREADX2_ll",HFILEM,HFIPRI,HRECFM,HDIR,IRESP)
ENDIF
IF (GALLOC) DEALLOCATE (ZFIELDP)
KRESP = IRESP
!------------------------------------------------------------------

CALL SECOND_MNH2(T22)
TIMEZ%T_READ2D_ALL=TIMEZ%T_READ2D_ALL + T22 - T11

END SUBROUTINE FMREADX2_ll

SUBROUTINE FMREADX3_ll(HFILEM,HRECFM,HFIPRI,HDIR,PFIELD,KGRID,&
     KLENCH,HCOMMENT,KRESP)
USE MODD_IO_ll, ONLY : ISP,GSMONOPROC,LPACK,L1D,L2D 
USE MODD_PARAMETERS_ll,ONLY : JPHEXT
USE MODD_FM
USE MODE_FD_ll, ONLY : GETFD,JPFINL,FD_LL
USE MODE_SCATTER_ll
USE MODE_ALLOCBUFFER_ll
!JUANZ
USE MODD_IO_ll, ONLY : ISNPROC
USE MODE_IO_ll, ONLY : io_file,io_rank
USE MODD_TIMEZ, ONLY : TIMEZ
USE MODE_MNH_TIMING, ONLY : SECOND_MNH2
!JUANZ
#ifdef MNH_GA
    USE MODE_GA
#endif
USE MODD_VAR_ll, ONLY : MNH_STATUSES_IGNORE

IMPLICIT NONE

CHARACTER(LEN=*),             INTENT(IN) ::HFILEM ! FM-file name
CHARACTER(LEN=*),             INTENT(IN) ::HRECFM ! name of the article to read
CHARACTER(LEN=*),             INTENT(IN) ::HFIPRI ! output file for error messages
CHARACTER(LEN=*),             INTENT(IN) ::HDIR   ! field form
REAL, DIMENSION(:,:,:),TARGET,INTENT(INOUT)::PFIELD ! array containing the data field
INTEGER,                      INTENT(INOUT)::KGRID  ! C-grid indicator (u,v,w,T)
INTEGER,                      INTENT(INOUT)::KLENCH ! length of comment string
CHARACTER(LEN=*),             INTENT(INOUT)::HCOMMENT ! comment string
INTEGER,                      INTENT(INOUT)::KRESP    ! return-code
!
#ifdef MNH_GA
REAL,DIMENSION(:,:,:),POINTER              :: ZFIELD_GA
#endif
!
!
!*      0.2   Declarations of local variables
!
CHARACTER(LEN=JPFINL)                    :: YFNLFI
INTEGER                                  :: IERR
TYPE(FD_ll), POINTER                     :: TZFD
INTEGER                                  :: IRESP
REAL,DIMENSION(:,:,:),POINTER            :: ZFIELDP
LOGICAL                                  :: GALLOC
TYPE(FMHEADER)                           :: TZFMH
!JUAN
INTEGER                                  :: JK,JKK
CHARACTER(LEN=LEN(HRECFM))               :: YK,YRECZSLIDE
REAL,DIMENSION(:,:),POINTER              :: ZSLIDE_ll,ZSLIDE
INTEGER                                  :: IK_FILE,IK_rank,inb_proc_real,JK_MAX
CHARACTER(len=5)                         :: YK_FILE  
CHARACTER(len=128)                       :: YFILE_IOZ  
TYPE(FD_ll), POINTER                     :: TZFD_IOZ 
INTEGER                                  :: JI,IXO,IXE,IYO,IYE
REAL,DIMENSION(:,:),POINTER              :: TX2DP
INTEGER, DIMENSION(MPI_STATUS_SIZE)      :: STATUS
LOGICAL                                  :: GALLOC_ll

INTEGER,ALLOCATABLE,DIMENSION(:)    :: REQ_TAB
INTEGER                           :: NB_REQ
TYPE TX_2DP
   REAL,DIMENSION(:,:), POINTER    :: X
END TYPE TX_2DP
TYPE(TX_2DP),ALLOCATABLE,DIMENSION(:) :: T_TX2DP
REAL*8,DIMENSION(2) :: T0,T1,T2
REAL*8,DIMENSION(2) :: T11,T22
INTEGER             :: IHEXTOT
!JUAN
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','FMREADX3_ll','reading '//TRIM(HRECFM))
!
!*      1.1   THE NAME OF LFIFM
!
CALL SECOND_MNH2(T11)
GALLOC    = .FALSE.
GALLOC_ll = .FALSE.
IRESP  = 0
YFNLFI = TRIM(ADJUSTL(HFILEM))//'.lfi'
!------------------------------------------------------------------
IHEXTOT = 2*JPHEXT+1
TZFD=>GETFD(YFNLFI)
IF (ASSOCIATED(TZFD)) THEN
  IF (GSMONOPROC  .AND.  (TZFD%nb_procio.eq.1) ) THEN ! sequential execution
!    IF (LPACK .AND. L1D .AND. HDIR=='XY') THEN 
    IF (LPACK .AND. L1D  .AND. SIZE(PFIELD,1)==IHEXTOT .AND. SIZE(PFIELD,2)==IHEXTOT) THEN
      ZFIELDP=>PFIELD(JPHEXT+1:JPHEXT+1,JPHEXT+1:JPHEXT+1,:)
      IF (ASSOCIATED(TZFD%CDF)) THEN
         CALL NCREAD(TZFD%CDF%NCID,HRECFM,ZFIELDP,TZFMH,IRESP)
      ELSE
         CALL FM_READ_ll(TZFD%FLU,HRECFM,.TRUE.,SIZE(ZFIELDP),ZFIELDP,TZFMH,IRESP)
      END IF
      PFIELD(:,:,:)=SPREAD(SPREAD(PFIELD(JPHEXT+1,JPHEXT+1,:),DIM=1,NCOPIES=IHEXTOT),DIM=2,NCOPIES=IHEXTOT)
!    ELSE IF (LPACK .AND. L2D .AND. HDIR=='XY') THEN
    ELSE IF (LPACK .AND. L2D .AND. SIZE(PFIELD,2)==IHEXTOT) THEN
      ALLOCATE (ZFIELDP(SIZE(PFIELD,1),1,SIZE(PFIELD,3)))
      GALLOC = .TRUE.
      IF (ASSOCIATED(TZFD%CDF)) THEN
         CALL NCREAD(TZFD%CDF%NCID,HRECFM,ZFIELDP,TZFMH,IRESP)
      ELSE
         CALL FM_READ_ll(TZFD%FLU,HRECFM,.TRUE.,SIZE(ZFIELDP),ZFIELDP,TZFMH,IRESP)
      END IF
      PFIELD(:,:,:)=SPREAD(ZFIELDP(:,1,:),DIM=2,NCOPIES=IHEXTOT)
    ELSE
      IF (ASSOCIATED(TZFD%CDF)) THEN
         CALL NCREAD(TZFD%CDF%NCID,HRECFM,PFIELD,TZFMH,IRESP)
      ELSE
         CALL FM_READ_ll(TZFD%FLU,HRECFM,.TRUE.,SIZE(PFIELD),PFIELD,TZFMH,IRESP)
      END IF
    END IF
    IF (IRESP /= 0) GOTO 1000
  ELSEIF ( (TZFD%nb_procio .eq. 1 ) .OR.  ( HDIR == '--' )  ) THEN ! multiprocessor execution & 1 IO proc 
  ! read 3D field for graphique
    IF (ISP == TZFD%OWNER)  THEN
      CALL ALLOCBUFFER_ll(ZFIELDP,PFIELD,HDIR,GALLOC)
      IF (ASSOCIATED(TZFD%CDF)) THEN
         CALL NCREAD(TZFD%CDF%NCID,HRECFM,ZFIELDP,TZFMH,IRESP)
      ELSE
         CALL FM_READ_ll(TZFD%FLU,HRECFM,.TRUE.,SIZE(ZFIELDP),ZFIELDP,TZFMH&
           & ,IRESP)
      END IF
    ELSE 
      ALLOCATE(ZFIELDP(0,0,0))
      GALLOC = .TRUE. 
    END IF
    !
    CALL MPI_BCAST(IRESP,1,MPI_INTEGER,TZFD%OWNER-1,TZFD%COMM,IERR)
    IF (IRESP /= 0) GOTO 1000
    !
    CALL BCAST_HEADER(TZFD,TZFMH)
    !
    IF (HDIR == 'XX' .OR. HDIR =='YY') THEN
      ! XX or YY Scatter Field
      CALL SCATTER_XXFIELD(HDIR,ZFIELDP,PFIELD,TZFD%OWNER,TZFD%COMM) 
    ELSE IF (HDIR == 'XY') THEN
      IF (LPACK .AND. L2D) THEN
        ! 2D compact case
        CALL SCATTER_XXFIELD('XX',ZFIELDP(:,1,:),PFIELD(:,JPHEXT+1,:),TZFD%OWNER,TZFD%COMM)
        PFIELD(:,:,:) = SPREAD(PFIELD(:,JPHEXT+1,:),DIM=2,NCOPIES=IHEXTOT)
      ELSE
        ! XY Scatter Field
        CALL SCATTER_XYFIELD(ZFIELDP,PFIELD,TZFD%OWNER,TZFD%COMM)
      END IF
    ELSE
      ! Broadcast Field
      CALL MPI_BCAST(PFIELD,SIZE(PFIELD),MPI_FLOAT,TZFD%OWNER-1,TZFD%COMM,IERR)
    END IF
  ELSE  ! multiprocessor execution & // IO  
!
!JUAN BG Z SLIDE 
!
#ifdef MNH_GA
          !
          ! init/create the ga
          !
          CALL SECOND_MNH2(T0)
          CALL MNH_INIT_GA(SIZE(PFIELD,1),SIZE(PFIELD,2),SIZE(PFIELD,3),HRECFM,"READ")
         !
         ! read the data
         !
         ALLOCATE(ZSLIDE_ll(0,0)) ! to avoid bug on test of size
         GALLOC_ll = .TRUE.
         DO JKK=1,IKU_ll
            IK_FILE   =  io_file(JKK,TZFD%nb_procio)
            write(YK_FILE ,'(".Z",i3.3)') IK_FILE+1
            YFILE_IOZ =  TRIM(HFILEM)//YK_FILE//".lfi"
            TZFD_IOZ => GETFD(YFILE_IOZ)
            !
            IK_RANK   =  TZFD_IOZ%OWNER
            !
            IF (ISP == IK_RANK )  THEN
               IF ( SIZE(ZSLIDE_ll) .EQ. 0 ) THEN
                  DEALLOCATE(ZSLIDE_ll)
                  CALL ALLOCBUFFER_ll(ZSLIDE_ll,ZSLIDE,HDIR,GALLOC_ll)
               END IF
               !    
               CALL SECOND_MNH2(T0)
               WRITE(YK,'(I4.4)')  JKK
               YRECZSLIDE = TRIM(HRECFM)//YK
               IF (ASSOCIATED(TZFD_IOZ%CDF)) THEN
                  CALL NCREAD(TZFD_IOZ%CDF%NCID,YRECZSLIDE,ZSLIDE_ll,TZFMH,IRESP)
               ELSE
                  CALL FM_READ_ll(TZFD_IOZ%FLU,YRECZSLIDE,.TRUE.,SIZE(ZSLIDE_ll),ZSLIDE_ll,TZFMH&
                       & ,IRESP)
               END IF
               CALL SECOND_MNH2(T1)
               TIMEZ%T_READ3D_READ=TIMEZ%T_READ3D_READ + T1 - T0
               !
               ! put the data in the g_a , this proc get this JKK slide
               !
               lo_zplan(JPIZ) = JKK
               hi_zplan(JPIZ) = JKK
               call nga_put(g_a, lo_zplan, hi_zplan,ZSLIDE_ll, ld_zplan)
            END IF
         END DO
         call ga_sync
         !
         ! get the columun data in this proc
         !
         ! temp buf to avoid problem with none stride PFIELDS buffer  with HALO 
         ALLOCATE (ZFIELD_GA (SIZE(PFIELD,1),SIZE(PFIELD,2),SIZE(PFIELD,3)))
         call nga_get(g_a, lo_col, hi_col,ZFIELD_GA(1,1,1) , ld_col)
         PFIELD = ZFIELD_GA
         DEALLOCATE(ZFIELD_GA)
#else
     ALLOCATE(ZSLIDE_ll(0,0))
     GALLOC_ll = .TRUE.
     inb_proc_real = min(TZFD%nb_procio,ISNPROC)
     Z_SLIDE: DO JK=1,SIZE(PFIELD,3),inb_proc_real
        !
        ! read the data
        !
        JK_MAX=min(SIZE(PFIELD,3),JK+inb_proc_real-1)
        !
         NB_REQ=0
         ALLOCATE(REQ_TAB(ISNPROC-1))
         ALLOCATE(T_TX2DP(ISNPROC-1))        
        DO JKK=JK,JK_MAX
           IF (TZFD%NB_PROCIO .GT. 1 ) THEN
              IK_FILE   =  io_file(JKK,TZFD%nb_procio)
              write(YK_FILE ,'(".Z",i3.3)') IK_FILE+1
              YFILE_IOZ =  TRIM(HFILEM)//YK_FILE//".lfi"
              TZFD_IOZ => GETFD(YFILE_IOZ)
           ELSE
              TZFD_IOZ => TZFD
           ENDIF
           IK_RANK   =  TZFD_IOZ%OWNER
           IF (ISP == IK_RANK )  THEN
              IF ( SIZE(ZSLIDE_ll) .EQ. 0 ) THEN
                 DEALLOCATE(ZSLIDE_ll)
                 CALL ALLOCBUFFER_ll(ZSLIDE_ll,ZSLIDE,HDIR,GALLOC_ll)
              END IF
              !JUAN
               CALL SECOND_MNH2(T0)
              WRITE(YK,'(I4.4)')  JKK
              YRECZSLIDE = TRIM(HRECFM)//YK
              IF (ASSOCIATED(TZFD_IOZ%CDF)) THEN
                 CALL NCREAD(TZFD_IOZ%CDF%NCID,YRECZSLIDE,ZSLIDE_ll,TZFMH,IRESP)
              ELSE
                 CALL FM_READ_ll(TZFD_IOZ%FLU,YRECZSLIDE,.TRUE.,SIZE(ZSLIDE_ll),ZSLIDE_ll,TZFMH&
                   & ,IRESP)
              END IF
              !JUANIOZ
               CALL SECOND_MNH2(T1)
               TIMEZ%T_READ3D_READ=TIMEZ%T_READ3D_READ + T1 - T0
              DO JI = 1,ISNPROC
                 CALL GET_DOMREAD_ll(JI,IXO,IXE,IYO,IYE)
                 TX2DP=>ZSLIDE_ll(IXO:IXE,IYO:IYE)
                 IF (ISP /= JI) THEN 
                     NB_REQ = NB_REQ + 1
                     ALLOCATE(T_TX2DP(NB_REQ)%X(IXO:IXE,IYO:IYE))
                     T_TX2DP(NB_REQ)%X=TX2DP
                     CALL MPI_ISEND(T_TX2DP(NB_REQ)%X,SIZE(TX2DP),MPI_FLOAT,JI-1,199+IK_RANK &
                          & ,TZFD_IOZ%COMM,REQ_TAB(NB_REQ),IERR)
                     !CALL MPI_BSEND(TX2DP,SIZE(TX2DP),MPI_FLOAT,JI-1,199+IK_RANK,TZFD_IOZ%COMM,IERR)
                 ELSE 
                    PFIELD(:,:,JKK) = TX2DP(:,:)
                 END IF
              END DO
               CALL SECOND_MNH2(T2)
               TIMEZ%T_READ3D_SEND=TIMEZ%T_READ3D_SEND + T2 - T1
              !JUANIOZ
           END IF
        END DO
        !
        ! brodcast the data
        !
    IF (HDIR == 'XX' .OR. HDIR =='YY') THEN
      ! XX or YY Scatter Field
       STOP " XX ou YY NON PREVU SUR BG POUR LE MOMENT "
      CALL SCATTER_XXFIELD(HDIR,ZFIELDP,PFIELD,TZFD%OWNER,TZFD%COMM) 
    ELSE IF (HDIR == 'XY') THEN
       IF (LPACK .AND. L2D) THEN
          ! 2D compact case
          STOP " L2D NON PREVU SUR BG POUR LE MOMENT "
          CALL SCATTER_XXFIELD('XX',ZFIELDP(:,1,:),PFIELD(:,JPHEXT+1,:),TZFD%OWNER,TZFD%COMM)
          PFIELD(:,:,:) = SPREAD(PFIELD(:,JPHEXT+1,:),DIM=2,NCOPIES=IHEXTOT)
       ELSE
          !
          ! XY Scatter Field
          !
               CALL SECOND_MNH2(T0)
          DO JKK=JK,JK_MAX
             !
             ! get the file & rank 
             !
             IF (TZFD%NB_PROCIO .GT. 1 ) THEN
               IK_FILE   =  io_file(JKK,TZFD%nb_procio)
               write(YK_FILE ,'(".Z",i3.3)') IK_FILE+1
               YFILE_IOZ =  TRIM(HFILEM)//YK_FILE//".lfi"
               TZFD_IOZ => GETFD(YFILE_IOZ)
            ELSE
               TZFD_IOZ => TZFD
            END IF
            !
            !IK_RANK   =  1 + io_rank(IK_FILE,ISNPROC,TZFD%nb_procio)
            IK_RANK    =  TZFD_IOZ%OWNER
            !
            ZSLIDE => PFIELD(:,:,JKK)
!JUANIOZ
            !CALL SCATTER_XYFIELD(ZSLIDE_ll,ZSLIDE,TZFD_IOZ%OWNER,TZFD_IOZ%COMM)
            IF (ISP .NE. IK_RANK) THEN
               CALL MPI_RECV(ZSLIDE,SIZE(ZSLIDE),MPI_FLOAT,IK_RANK-1,199+IK_RANK,TZFD_IOZ%COMM&
                    & ,STATUS,IERR)
            END IF
!JUAN IOZ
         END DO
               CALL SECOND_MNH2(T1)
               TIMEZ%T_READ3D_RECV=TIMEZ%T_READ3D_RECV + T1 - T0               
      END IF
    ELSE
      ! Broadcast Field
       STOP "  Broadcast Field NON PREVU SUR BG POUR LE MOMENT "
      CALL MPI_BCAST(PFIELD,SIZE(PFIELD),MPI_FLOAT,TZFD%OWNER-1,TZFD%COMM,IERR)
    END IF
         CALL SECOND_MNH2(T0) 
         IF (NB_REQ .GT.0 ) THEN
            CALL MPI_WAITALL(NB_REQ,REQ_TAB,MNH_STATUSES_IGNORE,IERR)
            DO JI=1,NB_REQ ;  DEALLOCATE(T_TX2DP(JI)%X) ; ENDDO
         END IF
         DEALLOCATE(T_TX2DP)
         DEALLOCATE(REQ_TAB)
         CALL SECOND_MNH2(T1) 
         TIMEZ%T_READ3D_WAIT=TIMEZ%T_READ3D_WAIT + T1 - T0
 END DO Z_SLIDE
 !
 CALL BCAST_HEADER(TZFD,TZFMH)
 !
#endif
!JUAN BG Z SLIDE  
  END IF !(GSMONOPROC) 
  
  KGRID    = TZFMH%GRID
  KLENCH   = TZFMH%COMLEN
  HCOMMENT = TZFMH%COMMENT(1:TZFMH%COMLEN)
ELSE 
  IRESP = -61          
END IF
!----------------------------------------------------------------
1000 CONTINUE
!! Error handler
IF (IRESP.NE.0) THEN
  CALL FM_READ_ERR("FMREADX3_ll",HFILEM,HFIPRI,HRECFM,HDIR,IRESP)
ENDIF
IF (GALLOC) DEALLOCATE (ZFIELDP)
IF (GALLOC_ll) DEALLOCATE (ZSLIDE_ll)
!IF (ASSOCIATED(ZSLIDE_ll)) DEALLOCATE (ZSLIDE_ll)
KRESP = IRESP
CALL MPI_BARRIER(TZFD%COMM,IERR)
CALL SECOND_MNH2(T22)
TIMEZ%T_READ3D_ALL=TIMEZ%T_READ3D_ALL + T22 - T11

!------------------------------------------------------------------
END SUBROUTINE FMREADX3_ll

SUBROUTINE FMREADX4_ll(HFILEM,HRECFM,HFIPRI,HDIR,PFIELD,KGRID,&
     KLENCH,HCOMMENT,KRESP)
USE MODD_IO_ll, ONLY : ISP,GSMONOPROC,LPACK,L1D,L2D 
USE MODD_PARAMETERS_ll,ONLY : JPHEXT
USE MODD_FM
USE MODE_FD_ll, ONLY : GETFD,JPFINL,FD_LL
USE MODE_SCATTER_ll
USE MODE_ALLOCBUFFER_ll

CHARACTER(LEN=*),              INTENT(IN) ::HFILEM   ! FM-file name
CHARACTER(LEN=*),              INTENT(IN) ::HRECFM   ! name of the article to read
CHARACTER(LEN=*),              INTENT(IN) ::HFIPRI   ! output file for error messages
CHARACTER(LEN=*),              INTENT(IN) ::HDIR     ! field form
REAL,DIMENSION(:,:,:,:),TARGET,INTENT(INOUT)::PFIELD   ! array containing the data field
INTEGER,                       INTENT(INOUT)::KGRID    ! C-grid indicator (u,v,w,T)
INTEGER,                       INTENT(INOUT)::KLENCH   ! length of comment string
CHARACTER(LEN=*),              INTENT(INOUT)::HCOMMENT ! comment string
INTEGER,                       INTENT(INOUT)::KRESP  ! return-code if
!
!
!*      0.2   Declarations of local variables
!
CHARACTER(LEN=JPFINL)           :: YFNLFI
INTEGER                         :: IERR
TYPE(FD_ll), POINTER            :: TZFD
INTEGER                         :: IRESP
REAL,DIMENSION(:,:,:,:),POINTER :: ZFIELDP
LOGICAL                         :: GALLOC
TYPE(FMHEADER)                  :: TZFMH
INTEGER                         :: IHEXTOT
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','FMREADX4_ll','reading '//TRIM(HRECFM))
!
!*      1.1   THE NAME OF LFIFM
!
GALLOC = .FALSE.
IRESP = 0
YFNLFI=TRIM(ADJUSTL(HFILEM))//'.lfi'
!------------------------------------------------------------------
IHEXTOT = 2*JPHEXT+1
TZFD=>GETFD(YFNLFI)
IF (ASSOCIATED(TZFD)) THEN
  IF (GSMONOPROC) THEN ! sequential execution
!    IF (LPACK .AND. L1D .AND. HDIR=='XY') THEN 
    IF (LPACK .AND. L1D .AND. SIZE(PFIELD,1)==IHEXTOT .AND. SIZE(PFIELD,2)==IHEXTOT) THEN
      ZFIELDP=>PFIELD(JPHEXT+1:JPHEXT+1,JPHEXT+1:JPHEXT+1,:,:)
      IF (ASSOCIATED(TZFD%CDF)) THEN
         CALL NCREAD(TZFD%CDF%NCID,HRECFM,ZFIELDP,TZFMH,IRESP)
      ELSE
         CALL FM_READ_ll(TZFD%FLU,HRECFM,.TRUE.,SIZE(ZFIELDP),ZFIELDP,TZFMH,IRESP)
      END IF
      PFIELD(:,:,:,:)=SPREAD(SPREAD(PFIELD(JPHEXT+1,JPHEXT+1,:,:),DIM=1,NCOPIES=IHEXTOT),DIM=2,NCOPIES=IHEXTOT)
!    ELSE IF (LPACK .AND. L2D .AND. HDIR=='XY') THEN
    ELSE IF (LPACK .AND. L2D .AND. SIZE(PFIELD,2)==IHEXTOT) THEN
      ZFIELDP=>PFIELD(:,JPHEXT+1:JPHEXT+1,:,:)
      IF (ASSOCIATED(TZFD%CDF)) THEN
         CALL NCREAD(TZFD%CDF%NCID,HRECFM,ZFIELDP,TZFMH,IRESP)
      ELSE
         CALL FM_READ_ll(TZFD%FLU,HRECFM,.TRUE.,SIZE(ZFIELDP),ZFIELDP,TZFMH,IRESP)
      END IF
      PFIELD(:,:,:,:)=SPREAD(PFIELD(:,JPHEXT+1,:,:),DIM=2,NCOPIES=IHEXTOT)
    ELSE
      IF (ASSOCIATED(TZFD%CDF)) THEN
         CALL NCREAD(TZFD%CDF%NCID,HRECFM,PFIELD,TZFMH,IRESP)
      ELSE
         CALL FM_READ_ll(TZFD%FLU,HRECFM,.TRUE.,SIZE(PFIELD),PFIELD,TZFMH,IRESP)
      END IF
    END IF
    IF (IRESP /= 0) GOTO 1000
  ELSE
    IF (ISP == TZFD%OWNER)  THEN
      CALL ALLOCBUFFER_ll(ZFIELDP,PFIELD,HDIR,GALLOC)
      IF (ASSOCIATED(TZFD%CDF)) THEN
         CALL NCREAD(TZFD%CDF%NCID,HRECFM,ZFIELDP,TZFMH,IRESP)
      ELSE
         CALL FM_READ_ll(TZFD%FLU,HRECFM,.TRUE.,SIZE(ZFIELDP),ZFIELDP,TZFMH&
           & ,IRESP)
      END IF
    ELSE
      ALLOCATE(ZFIELDP(0,0,0,0))
      GALLOC = .TRUE.
    END IF
    !
    CALL MPI_BCAST(IRESP,1,MPI_INTEGER,TZFD%OWNER-1,TZFD%COMM,IERR)
    IF (IRESP /= 0) GOTO 1000
    !
    CALL BCAST_HEADER(TZFD,TZFMH)
    !
    IF (HDIR == 'XX' .OR. HDIR =='YY') THEN
      ! XX or YY Scatter Field
      CALL SCATTER_XXFIELD(HDIR,ZFIELDP,PFIELD,TZFD%OWNER,TZFD%COMM) 
    ELSE IF (HDIR == 'XY') THEN
      IF (LPACK .AND. L2D) THEN
        ! 2D compact case
        CALL SCATTER_XXFIELD('XX',ZFIELDP(:,1,:,:),PFIELD(:,JPHEXT+1,:,:),TZFD%OWNER,TZFD%COMM)
        PFIELD(:,:,:,:) = SPREAD(PFIELD(:,JPHEXT+1,:,:),DIM=2,NCOPIES=IHEXTOT)
      ELSE
        ! XY Scatter Field
        CALL SCATTER_XYFIELD(ZFIELDP,PFIELD,TZFD%OWNER,TZFD%COMM)
      END IF
    ELSE
      ! Broadcast Field
      CALL MPI_BCAST(PFIELD,SIZE(PFIELD),MPI_FLOAT,TZFD%OWNER-1,TZFD%COMM,IERR)
    END IF
  END IF
  KGRID  = TZFMH%GRID
  KLENCH = TZFMH%COMLEN
  HCOMMENT = TZFMH%COMMENT(1:TZFMH%COMLEN)
ELSE 
  IRESP = -61
END IF
!----------------------------------------------------------------
1000 CONTINUE
!! Error handler
IF (IRESP.NE.0) THEN
  CALL FM_READ_ERR("FMREADX4_ll",HFILEM,HFIPRI,HRECFM,HDIR,IRESP)
ENDIF

IF (GALLOC) DEALLOCATE (ZFIELDP)
KRESP = IRESP
RETURN
!------------------------------------------------------------------
END SUBROUTINE FMREADX4_ll

SUBROUTINE FMREADX5_ll(HFILEM,HRECFM,HFIPRI,HDIR,PFIELD,KGRID,&
     KLENCH,HCOMMENT,KRESP)
USE MODD_IO_ll, ONLY : ISP,GSMONOPROC,LPACK,L1D,L2D 
USE MODD_PARAMETERS_ll,ONLY : JPHEXT
USE MODD_FM
USE MODE_FD_ll, ONLY : GETFD,JPFINL,FD_LL
USE MODE_SCATTER_ll
USE MODE_ALLOCBUFFER_ll

CHARACTER(LEN=*),                INTENT(IN) ::HFILEM ! FM-file name
CHARACTER(LEN=*),                INTENT(IN) ::HRECFM ! name of the article to read
CHARACTER(LEN=*),                INTENT(IN) ::HFIPRI ! output file for error messages
CHARACTER(LEN=*),                INTENT(IN) ::HDIR   ! field form
REAL,DIMENSION(:,:,:,:,:),TARGET,INTENT(INOUT)::PFIELD ! array containing the data field
INTEGER,                         INTENT(INOUT)::KGRID  ! C-grid indicator (u,v,w,T)
INTEGER,                         INTENT(INOUT)::KLENCH ! length of comment string
CHARACTER(LEN=*),                INTENT(INOUT)::HCOMMENT ! comment string
INTEGER,                         INTENT(INOUT)::KRESP  ! return-code
!
!
!*      0.2   Declarations of local variables
!
CHARACTER(LEN=JPFINL)             :: YFNLFI
INTEGER                           :: IERR
TYPE(FD_ll), POINTER              :: TZFD
INTEGER                           :: IRESP
REAL,DIMENSION(:,:,:,:,:),POINTER :: ZFIELDP
LOGICAL                           :: GALLOC
TYPE(FMHEADER)                    :: TZFMH
INTEGER                           :: IHEXTOT
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','FMREADX5_ll','reading '//TRIM(HRECFM))
!
!*      1.1   THE NAME OF LFIFM
!
GALLOC = .FALSE.
IRESP = 0
YFNLFI=TRIM(ADJUSTL(HFILEM))//'.lfi'
!------------------------------------------------------------------
IHEXTOT = 2*JPHEXT+1
TZFD=>GETFD(YFNLFI)
IF (ASSOCIATED(TZFD)) THEN
  IF (GSMONOPROC) THEN ! sequential execution
!    IF (LPACK .AND. L1D .AND. HDIR=='XY') THEN 
    IF (LPACK .AND. L1D .AND. SIZE(PFIELD,1)==IHEXTOT .AND. SIZE(PFIELD,2)==IHEXTOT) THEN
      ZFIELDP=>PFIELD(JPHEXT+1:JPHEXT+1,JPHEXT+1:JPHEXT+1,:,:,:)
      IF (ASSOCIATED(TZFD%CDF)) THEN
         CALL NCREAD(TZFD%CDF%NCID,HRECFM,ZFIELDP,TZFMH,IRESP)
      ELSE
         CALL FM_READ_ll(TZFD%FLU,HRECFM,.TRUE.,SIZE(ZFIELDP),ZFIELDP,TZFMH,IRESP)
      END IF
      PFIELD(:,:,:,:,:)=SPREAD(SPREAD(PFIELD(JPHEXT+1,JPHEXT+1,:,:,:),DIM=1,NCOPIES=IHEXTOT),DIM=2,NCOPIES=IHEXTOT)
!    ELSE IF (LPACK .AND. L2D .AND. HDIR=='XY') THEN
    ELSE IF (LPACK .AND. L2D .AND. SIZE(PFIELD,2)==IHEXTOT) THEN
      ZFIELDP=>PFIELD(:,JPHEXT+1:JPHEXT+1,:,:,:)
      IF (ASSOCIATED(TZFD%CDF)) THEN
         CALL NCREAD(TZFD%CDF%NCID,HRECFM,ZFIELDP,TZFMH,IRESP)
      ELSE
         CALL FM_READ_ll(TZFD%FLU,HRECFM,.TRUE.,SIZE(ZFIELDP),ZFIELDP,TZFMH,IRESP)
      END IF
      PFIELD(:,:,:,:,:)=SPREAD(PFIELD(:,JPHEXT+1,:,:,:),DIM=2,NCOPIES=IHEXTOT)
    ELSE
      IF (ASSOCIATED(TZFD%CDF)) THEN
         CALL NCREAD(TZFD%CDF%NCID,HRECFM,PFIELD,TZFMH,IRESP)
      ELSE
         CALL FM_READ_ll(TZFD%FLU,HRECFM,.TRUE.,SIZE(PFIELD),PFIELD,TZFMH,IRESP)
      END IF
    END IF  
    IF (IRESP /= 0) GOTO 1000
  ELSE ! multiprocessor execution
    IF (ISP == TZFD%OWNER)  THEN
      CALL ALLOCBUFFER_ll(ZFIELDP,PFIELD,HDIR,GALLOC)
      IF (ASSOCIATED(TZFD%CDF)) THEN
         CALL NCREAD(TZFD%CDF%NCID,HRECFM,ZFIELDP,TZFMH,IRESP)
      ELSE
         CALL FM_READ_ll(TZFD%FLU,HRECFM,.TRUE.,SIZE(ZFIELDP),ZFIELDP,TZFMH&
           & ,IRESP)
      END IF
    ELSE
      ALLOCATE(ZFIELDP(0,0,0,0,0))
      GALLOC = .TRUE.
    END IF
    !
    CALL MPI_BCAST(IRESP,1,MPI_INTEGER,TZFD%OWNER-1,TZFD%COMM,IERR)
    IF (IRESP /= 0) GOTO 1000
    !
    CALL BCAST_HEADER(TZFD,TZFMH)
    !
    IF (HDIR == 'XX' .OR. HDIR =='YY') THEN
      ! XX or YY Scatter Field
      CALL SCATTER_XXFIELD(HDIR,ZFIELDP,PFIELD,TZFD%OWNER,TZFD%COMM) 
    ELSE IF (HDIR == 'XY') THEN
      IF (LPACK .AND. L2D) THEN
        ! 2D compact case
        CALL SCATTER_XXFIELD('XX',ZFIELDP(:,1,:,:,:),PFIELD(:,JPHEXT+1,:,:,:),&
             & TZFD%OWNER,TZFD%COMM)
        PFIELD(:,:,:,:,:) = SPREAD(PFIELD(:,JPHEXT+1,:,:,:),DIM=2,NCOPIES=IHEXTOT)
      ELSE
        ! XY Scatter Field
        CALL SCATTER_XYFIELD(ZFIELDP,PFIELD,TZFD%OWNER,TZFD%COMM)
      END IF
    ELSE
      ! Broadcast Field
      CALL MPI_BCAST(PFIELD,SIZE(PFIELD),MPI_FLOAT,TZFD%OWNER-1,TZFD%COMM,IERR)
    END IF
  END IF
  KGRID  = TZFMH%GRID
  KLENCH = TZFMH%COMLEN
  HCOMMENT = TZFMH%COMMENT(1:TZFMH%COMLEN)
ELSE 
  IRESP = -61
END IF
!----------------------------------------------------------------
1000 CONTINUE
!! Error handler
IF (IRESP.NE.0) THEN
  CALL FM_READ_ERR("FMREADX5_ll",HFILEM,HFIPRI,HRECFM,HDIR,IRESP)
ENDIF
IF (GALLOC) DEALLOCATE (ZFIELDP)
KRESP = IRESP
RETURN
!------------------------------------------------------------------
END SUBROUTINE FMREADX5_ll

SUBROUTINE FMREADX6_ll(HFILEM,HRECFM,HFIPRI,HDIR,PFIELD,KGRID,&
     KLENCH,HCOMMENT,KRESP)
USE MODD_IO_ll, ONLY : ISP,GSMONOPROC 
USE MODD_FM
USE MODE_FD_ll, ONLY : GETFD,JPFINL,FD_LL
USE MODE_SCATTER_ll
USE MODE_ALLOCBUFFER_ll

CHARACTER(LEN=*),                  INTENT(IN) ::HFILEM ! FM-file name
CHARACTER(LEN=*),                  INTENT(IN) ::HRECFM ! name of the article to read
CHARACTER(LEN=*),                  INTENT(IN) ::HFIPRI ! output file for error messages
CHARACTER(LEN=*),                  INTENT(IN) ::HDIR   ! field form
REAL,DIMENSION(:,:,:,:,:,:),TARGET,INTENT(INOUT)::PFIELD ! array containing the data field
INTEGER,                           INTENT(INOUT)::KGRID  ! C-grid indicator (u,v,w,T)
INTEGER,                           INTENT(INOUT)::KLENCH ! length of comment string
CHARACTER(LEN=*),                  INTENT(INOUT)::HCOMMENT ! comment string
INTEGER,                           INTENT(INOUT)::KRESP  ! return-code
!
!
!*      0.2   Declarations of local variables
!
CHARACTER(LEN=JPFINL)               :: YFNLFI
INTEGER                             :: IERR
TYPE(FD_ll), POINTER                :: TZFD
INTEGER                             :: IRESP
REAL,DIMENSION(:,:,:,:,:,:),POINTER :: ZFIELDP
LOGICAL                             :: GALLOC
TYPE(FMHEADER)                      :: TZFMH
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','FMREADX6_ll','reading '//TRIM(HRECFM))
!
!*      1.1   THE NAME OF LFIFM
!
GALLOC = .FALSE.
IRESP = 0
YFNLFI=TRIM(ADJUSTL(HFILEM))//'.lfi'
!------------------------------------------------------------------
TZFD=>GETFD(YFNLFI)
IF (ASSOCIATED(TZFD)) THEN
  IF (GSMONOPROC) THEN ! sequential execution
      IF (ASSOCIATED(TZFD%CDF)) THEN
         CALL NCREAD(TZFD%CDF%NCID,HRECFM,PFIELD,TZFMH,IRESP)
      ELSE
         CALL FM_READ_ll(TZFD%FLU,HRECFM,.TRUE.,SIZE(PFIELD),PFIELD,TZFMH,IRESP)
      END IF
    IF (IRESP /= 0) GOTO 1000
  ELSE ! multiprocessor execution
    IF (ISP == TZFD%OWNER)  THEN
      CALL ALLOCBUFFER_ll(ZFIELDP,PFIELD,HDIR,GALLOC)
      IF (ASSOCIATED(TZFD%CDF)) THEN
         CALL NCREAD(TZFD%CDF%NCID,HRECFM,ZFIELDP,TZFMH,IRESP)
      ELSE
         CALL FM_READ_ll(TZFD%FLU,HRECFM,.TRUE.,SIZE(ZFIELDP),ZFIELDP,TZFMH&
           & ,IRESP)
      END IF
    ELSE
      ALLOCATE(ZFIELDP(0,0,0,0,0,0))
      GALLOC = .TRUE.
    END IF
    !
    CALL MPI_BCAST(IRESP,1,MPI_INTEGER,TZFD%OWNER-1,TZFD%COMM,IERR)
    IF (IRESP /= 0) GOTO 1000
    !
    CALL BCAST_HEADER(TZFD,TZFMH)
    !
    IF (HDIR == 'XX' .OR. HDIR =='YY') THEN
      ! XX or YY Scatter Field
      CALL SCATTER_XXFIELD(HDIR,ZFIELDP,PFIELD,TZFD%OWNER,TZFD%COMM) 
    ELSE IF (HDIR == 'XY') THEN
      ! XY Scatter Field
      CALL SCATTER_XYFIELD(ZFIELDP,PFIELD,TZFD%OWNER,TZFD%COMM)
    ELSE
      ! Broadcast Field
      CALL MPI_BCAST(PFIELD,SIZE(PFIELD),MPI_FLOAT,TZFD%OWNER-1,TZFD%COMM,IERR)
    END IF
  END IF
  KGRID  = TZFMH%GRID
  KLENCH = TZFMH%COMLEN
  HCOMMENT = TZFMH%COMMENT(1:TZFMH%COMLEN)
ELSE 
  IRESP = -61
END IF
!----------------------------------------------------------------
1000 CONTINUE
!! Error handler
IF (IRESP.NE.0) THEN
  CALL FM_READ_ERR("FMREADX6_ll",HFILEM,HFIPRI,HRECFM,HDIR,IRESP)
ENDIF
IF (GALLOC) DEALLOCATE (ZFIELDP)
KRESP = IRESP
RETURN
!------------------------------------------------------------------
END SUBROUTINE FMREADX6_ll

SUBROUTINE FMREADN0_ll(HFILEM,HRECFM,HFIPRI,HDIR,KFIELD,KGRID,&
     KLENCH,HCOMMENT,KRESP)
USE MODD_IO_ll, ONLY : ISP,GSMONOPROC 
USE MODD_FM
USE MODE_FD_ll, ONLY : GETFD,JPFINL,FD_LL

!
!*      0.    DECLARATIONS
!             ------------
!
!
!*      0.1   Declarations of arguments
!
CHARACTER(LEN=*),          INTENT(IN) ::HFILEM   ! FM-file name
CHARACTER(LEN=*),          INTENT(IN) ::HRECFM   ! name of the article to read
CHARACTER(LEN=*),          INTENT(IN) ::HFIPRI   ! output file for error messages
CHARACTER(LEN=*),          INTENT(IN) ::HDIR     ! Field form
INTEGER,                   INTENT(INOUT)::KFIELD ! array containing the data field     
INTEGER,                   INTENT(INOUT)::KGRID    ! C-grid indicator (u,v,w,T)
INTEGER,                   INTENT(INOUT)::KLENCH   ! length of comment string
CHARACTER(LEN=*),          INTENT(INOUT)::HCOMMENT ! comment string
INTEGER,                   INTENT(INOUT)::KRESP    ! return-code
!
!*      0.2   Declarations of local variables
!
CHARACTER(LEN=JPFINL)        :: YFNLFI
INTEGER                      :: IERR
TYPE(FD_ll), POINTER         :: TZFD
INTEGER                      :: IRESP
TYPE(FMHEADER)               :: TZFMH
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','FMREADN0_ll','reading '//TRIM(HRECFM))
!
!*      1.1   THE NAME OF LFIFM
!
IRESP = 0
YFNLFI=TRIM(ADJUSTL(HFILEM))//'.lfi'
!  
TZFD=>GETFD(YFNLFI)
IF (ASSOCIATED(TZFD)) THEN
  IF (GSMONOPROC) THEN ! sequential execution
      IF (ASSOCIATED(TZFD%CDF)) THEN
         CALL NCREAD(TZFD%CDF%NCID,HRECFM,KFIELD,TZFMH,IRESP)
      ELSE
         CALL FM_READ_ll(TZFD%FLU,HRECFM,.FALSE.,1,KFIELD,TZFMH,IRESP)
      END IF
    IF (IRESP /= 0) GOTO 1000
  ELSE
    IF (ISP == TZFD%OWNER)  THEN
      IF (ASSOCIATED(TZFD%CDF)) THEN
         CALL NCREAD(TZFD%CDF%NCID,HRECFM,KFIELD,TZFMH,IRESP)
      ELSE
         CALL FM_READ_ll(TZFD%FLU,HRECFM,.FALSE.,1,KFIELD,TZFMH,IRESP)
      END IF
    END IF
    !
    CALL MPI_BCAST(IRESP,1,MPI_INTEGER,TZFD%OWNER-1,TZFD%COMM,IERR)
    IF (IRESP /= 0) GOTO 1000
    !
    CALL BCAST_HEADER(TZFD,TZFMH)       
    !
    CALL MPI_BCAST(KFIELD,1,MPI_INTEGER,TZFD%OWNER-1,TZFD%COMM,IERR)
  END IF
  KGRID  = TZFMH%GRID
  KLENCH = TZFMH%COMLEN
  HCOMMENT = TZFMH%COMMENT(1:TZFMH%COMLEN)
ELSE 
  IRESP = -61
END IF
!----------------------------------------------------------------
1000 CONTINUE
!! Error handler
IF (IRESP.NE.0) THEN
  CALL FM_READ_ERR("FMREADN0_ll",HFILEM,HFIPRI,HRECFM,HDIR,IRESP)
ENDIF
KRESP = IRESP
RETURN

END SUBROUTINE FMREADN0_ll

SUBROUTINE IO_READ_FIELD_BYNAME_N0(TPFILE,HNAME,KFIELD,KRESP)
!
TYPE(TFILEDATA),  INTENT(IN)    :: TPFILE
CHARACTER(LEN=*), INTENT(IN)    :: HNAME    ! name of the field to write
INTEGER,          INTENT(INOUT) :: KFIELD   ! array containing the data field
INTEGER,OPTIONAL, INTENT(OUT)   :: KRESP    ! return-code
!
INTEGER :: ID ! Index of the field
INTEGER :: IRESP ! return_code
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_READ_FIELD_BYNAME_N0',TRIM(TPFILE%CNAME)//': reading '//TRIM(HNAME))
!
CALL FIND_FIELD_ID_FROM_MNHNAME(HNAME,ID,IRESP)
!
IF(IRESP==0) CALL IO_READ_FIELD(TPFILE,TFIELDLIST(ID),KFIELD,IRESP)
!
IF (PRESENT(KRESP)) KRESP = IRESP
!
END SUBROUTINE IO_READ_FIELD_BYNAME_N0

SUBROUTINE IO_READ_FIELD_BYFIELD_N0(TPFILE,TPFIELD,KFIELD,KRESP)
!
USE MODD_IO_ll, ONLY : ISP,GSMONOPROC
USE MODE_FD_ll, ONLY : GETFD,FD_LL
!
TYPE(TFILEDATA),  INTENT(IN)    :: TPFILE
TYPE(TFIELDDATA), INTENT(INOUT) :: TPFIELD
INTEGER,          INTENT(INOUT) :: KFIELD   ! array containing the data field
INTEGER,OPTIONAL, INTENT(OUT)   :: KRESP    ! return-code
!
INTEGER                      :: IERR
TYPE(FD_ll), POINTER         :: TZFD
INTEGER                      :: IRESP
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_READ_FIELD_BYFIELD_N0',TRIM(TPFILE%CNAME)//': reading '//TRIM(TPFIELD%CMNHNAME))
!
IRESP = 0
!
TZFD=>GETFD(TRIM(ADJUSTL(TPFILE%CNAME))//'.lfi')
IF (ASSOCIATED(TZFD)) THEN
  IF (GSMONOPROC) THEN ! sequential execution
      IF (TPFILE%CFORMAT=='NETCDF4') THEN
         CALL IO_READ_FIELD_NC4(TPFILE,TPFIELD,KFIELD,IRESP)
      ELSE IF (TPFILE%CFORMAT=='LFI') THEN
         CALL IO_READ_FIELD_LFI(TPFILE,TPFIELD,KFIELD,IRESP)
      ELSE
         CALL PRINT_MSG(NVERB_FATAL,'IO','IO_READ_FIELD_BYFIELD_N0',&
                        TRIM(TPFILE%CNAME)//': invalid fileformat ('//TRIM(TPFILE%CFORMAT)//')')
      END IF
  ELSE
    IF (ISP == TZFD%OWNER)  THEN
      IF (TPFILE%CFORMAT=='NETCDF4') THEN
         CALL IO_READ_FIELD_NC4(TPFILE,TPFIELD,KFIELD,IRESP)
      ELSE IF (TPFILE%CFORMAT=='LFI') THEN
         CALL IO_READ_FIELD_LFI(TPFILE,TPFIELD,KFIELD,IRESP)
      ELSE
         CALL PRINT_MSG(NVERB_FATAL,'IO','IO_READ_FIELD_BYFIELD_N0',&
                        TRIM(TPFILE%CNAME)//': invalid fileformat ('//TRIM(TPFILE%CFORMAT)//')')
      END IF
    END IF
    !
    CALL MPI_BCAST(IRESP,1,MPI_INTEGER,TZFD%OWNER-1,TZFD%COMM,IERR)
    !
    !Broadcast header only if IRESP==-111
    !because metadata of field has been modified in IO_READ_FIELD_xxx
    IF (IRESP==-111) CALL IO_BCAST_FIELD_METADATA(TZFD,TPFIELD)
    !
    CALL MPI_BCAST(KFIELD,1,MPI_INTEGER,TZFD%OWNER-1,TZFD%COMM,IERR)
  END IF
ELSE
  IRESP = -61
  CALL PRINT_MSG(NVERB_ERROR,'IO','IO_READ_FIELD_BYFIELD_N0','file '//TRIM(TPFILE%CNAME)//' not found')
END IF
!
IF (PRESENT(KRESP)) KRESP = IRESP
!
END SUBROUTINE IO_READ_FIELD_BYFIELD_N0


SUBROUTINE FMREADN1_ll(HFILEM,HRECFM,HFIPRI,HDIR,KFIELD,KGRID,&
     KLENCH,HCOMMENT,KRESP)
USE MODD_IO_ll, ONLY : ISP,GSMONOPROC
USE MODD_FM
USE MODE_FD_ll, ONLY : GETFD,JPFINL,FD_LL
USE MODE_SCATTER_ll
USE MODE_ALLOCBUFFER_ll

!*      0.    DECLARATIONS
!             ------------
!
!
!*      0.1   Declarations of arguments
!
CHARACTER(LEN=*),           INTENT(IN) ::HFILEM   ! FM-file name
CHARACTER(LEN=*),           INTENT(IN) ::HRECFM   ! name of the article to read
CHARACTER(LEN=*),           INTENT(IN) ::HFIPRI   ! output file for error messages
CHARACTER(LEN=*),           INTENT(IN) ::HDIR     ! Field form
INTEGER,DIMENSION(:),TARGET,INTENT(INOUT)::KFIELD ! array containing the data field     
INTEGER,                    INTENT(INOUT)::KGRID    ! C-grid indicator (u,v,w,T)
INTEGER,                    INTENT(INOUT)::KLENCH   ! length of comment string
CHARACTER(LEN=*),           INTENT(INOUT)::HCOMMENT ! comment string
INTEGER,                    INTENT(INOUT)::KRESP    ! return-code
!
!*      0.2   Declarations of local variables
!
CHARACTER(LEN=JPFINL)            :: YFNLFI
INTEGER                          :: IERR
TYPE(FD_ll), POINTER             :: TZFD
INTEGER                          :: IRESP
INTEGER,DIMENSION(:),POINTER     :: IFIELDP
LOGICAL                          :: GALLOC
TYPE(FMHEADER)                   :: TZFMH
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','FMREADN1_ll','reading '//TRIM(HRECFM))
!
!*      1.1   THE NAME OF LFIFM
!
GALLOC = .FALSE.
IRESP = 0
YFNLFI=TRIM(ADJUSTL(HFILEM))//'.lfi'
!------------------------------------------------------------------
TZFD=>GETFD(YFNLFI)
IF (ASSOCIATED(TZFD)) THEN
  IF (GSMONOPROC) THEN ! sequential execution
    IF (ASSOCIATED(TZFD%CDF)) THEN
       CALL NCREAD(TZFD%CDF%NCID,HRECFM,KFIELD,TZFMH,IRESP)
    ELSE
       CALL FM_READ_ll(TZFD%FLU,HRECFM,.FALSE.,SIZE(KFIELD),KFIELD,TZFMH,IRESP)
    END IF
    IF (IRESP /= 0) GOTO 1000
  ELSE
    IF (ISP == TZFD%OWNER)  THEN
      CALL ALLOCBUFFER_ll(IFIELDP,KFIELD,HDIR,GALLOC)
      IF (ASSOCIATED(TZFD%CDF)) THEN
         CALL NCREAD(TZFD%CDF%NCID,HRECFM,IFIELDP,TZFMH,IRESP)
      ELSE
         CALL FM_READ_ll(TZFD%FLU,HRECFM,.FALSE.,SIZE(IFIELDP),IFIELDP,TZFMH&
           & ,IRESP)
      END IF
    ELSE
      ALLOCATE(IFIELDP(0))
      GALLOC = .TRUE.
    END IF
    !
    CALL MPI_BCAST(IRESP,1,MPI_INTEGER,TZFD%OWNER-1,TZFD%COMM,IERR)
    IF (IRESP /= 0) GOTO 1000
    !
    CALL BCAST_HEADER(TZFD,TZFMH)
    !
    IF (HDIR /= 'XX' .AND. HDIR /='YY') THEN
      ! Broadcast Field
      CALL MPI_BCAST(KFIELD,SIZE(KFIELD),MPI_INTEGER,TZFD%OWNER-1,TZFD%COMM,IERR)
    ELSE 
      !Scatter Field
      CALL SCATTER_XXFIELD(HDIR,IFIELDP,KFIELD,TZFD%OWNER,TZFD%COMM) 
    END IF
  END IF
  KGRID  = TZFMH%GRID
  KLENCH = TZFMH%COMLEN
  HCOMMENT = TZFMH%COMMENT(1:TZFMH%COMLEN)
ELSE 
  IRESP = -61
END IF
!----------------------------------------------------------------
1000 CONTINUE
!! Error handler
IF (IRESP.NE.0) THEN
  CALL FM_READ_ERR("FMREADN1_ll",HFILEM,HFIPRI,HRECFM,HDIR,IRESP)
ENDIF
IF (GALLOC) DEALLOCATE (IFIELDP)
KRESP = IRESP
RETURN
  
END SUBROUTINE FMREADN1_ll

SUBROUTINE FMREADN2_ll(HFILEM,HRECFM,HFIPRI,HDIR,KFIELD,KGRID,&
     KLENCH,HCOMMENT,KRESP)
USE MODD_IO_ll, ONLY : ISP,GSMONOPROC,LPACK,L1D,L2D 
USE MODD_PARAMETERS_ll,ONLY : JPHEXT
USE MODD_FM
USE MODE_FD_ll, ONLY : GETFD,JPFINL,FD_LL
USE MODE_SCATTER_ll
USE MODE_ALLOCBUFFER_ll

CHARACTER(LEN=*),              INTENT(IN) ::HFILEM ! FM-file name
CHARACTER(LEN=*),              INTENT(IN) ::HRECFM ! name of the article to read
CHARACTER(LEN=*),              INTENT(IN) ::HFIPRI ! output file for error messages
CHARACTER(LEN=*),              INTENT(IN) ::HDIR   ! field form
INTEGER, DIMENSION(:,:),TARGET,INTENT(INOUT)::KFIELD ! array containing the data field
INTEGER,                       INTENT(INOUT)::KGRID  ! C-grid indicator (u,v,w,T)
INTEGER,                       INTENT(INOUT)::KLENCH ! length of comment string
CHARACTER(LEN=*),              INTENT(INOUT)::HCOMMENT ! comment string
INTEGER,                       INTENT(INOUT)::KRESP  ! return-code
!
!
!*      0.2   Declarations of local variables
!
CHARACTER(LEN=JPFINL)          :: YFNLFI
INTEGER                        :: IERR
TYPE(FD_ll), POINTER           :: TZFD
INTEGER                        :: IRESP
INTEGER,DIMENSION(:,:),POINTER :: IFIELDP
LOGICAL                        :: GALLOC
TYPE(FMHEADER)                 :: TZFMH
INTEGER                        :: IHEXTOT
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','FMREADN2_ll','reading '//TRIM(HRECFM))
!
!*      1.1   THE NAME OF LFIFM
!
GALLOC = .FALSE.
IRESP = 0
YFNLFI=TRIM(ADJUSTL(HFILEM))//'.lfi'
!------------------------------------------------------------------
IHEXTOT = 2*JPHEXT+1
TZFD=>GETFD(YFNLFI)
IF (ASSOCIATED(TZFD)) THEN
  IF (GSMONOPROC) THEN ! sequential execution
!    IF (LPACK .AND. L1D .AND. HDIR=='XY') THEN 
    IF (LPACK .AND. L1D .AND. SIZE(KFIELD,1)==IHEXTOT .AND. SIZE(KFIELD,2)==IHEXTOT) THEN
      IFIELDP=>KFIELD(JPHEXT+1:JPHEXT+1,JPHEXT+1:JPHEXT+1)
      IF (ASSOCIATED(TZFD%CDF)) THEN
         CALL NCREAD(TZFD%CDF%NCID,HRECFM,IFIELDP,TZFMH,IRESP)
      ELSE
         CALL FM_READ_ll(TZFD%FLU,HRECFM,.FALSE.,SIZE(IFIELDP),IFIELDP,TZFMH,IRESP)
      END IF
      KFIELD(:,:)=SPREAD(SPREAD(KFIELD(JPHEXT+1,JPHEXT+1),DIM=1,NCOPIES=IHEXTOT),DIM=2,NCOPIES=IHEXTOT)
!    ELSE IF (LPACK .AND. L2D .AND. HDIR=='XY') THEN
    ELSE IF (LPACK .AND. L2D .AND. SIZE(KFIELD,2)==IHEXTOT) THEN
      IFIELDP=>KFIELD(:,JPHEXT+1:JPHEXT+1)
      IF (ASSOCIATED(TZFD%CDF)) THEN
         CALL NCREAD(TZFD%CDF%NCID,HRECFM,IFIELDP,TZFMH,IRESP)
      ELSE
         CALL FM_READ_ll(TZFD%FLU,HRECFM,.FALSE.,SIZE(IFIELDP),IFIELDP,TZFMH,IRESP)
      END IF
      KFIELD(:,:)=SPREAD(KFIELD(:,JPHEXT+1),DIM=2,NCOPIES=IHEXTOT)
    ELSE
      IF (ASSOCIATED(TZFD%CDF)) THEN
         CALL NCREAD(TZFD%CDF%NCID,HRECFM,KFIELD,TZFMH,IRESP)
      ELSE
         CALL FM_READ_ll(TZFD%FLU,HRECFM,.FALSE.,SIZE(KFIELD),KFIELD,TZFMH,IRESP)
      END IF
    END IF
    IF (IRESP /= 0) GOTO 1000
  ELSE
    IF (ISP == TZFD%OWNER)  THEN
      CALL ALLOCBUFFER_ll(IFIELDP,KFIELD,HDIR,GALLOC)
      IF (ASSOCIATED(TZFD%CDF)) THEN
         CALL NCREAD(TZFD%CDF%NCID,HRECFM,IFIELDP,TZFMH,IRESP)
      ELSE
         CALL FM_READ_ll(TZFD%FLU,HRECFM,.FALSE.,SIZE(IFIELDP),IFIELDP&
           & ,TZFMH,IRESP)
      END IF
    ELSE
      ALLOCATE(IFIELDP(0,0))
      GALLOC = .TRUE.
    END IF
    !
    CALL MPI_BCAST(IRESP,1,MPI_INTEGER,TZFD%OWNER-1,TZFD%COMM,IERR)
    IF (IRESP /= 0) GOTO 1000
    !
    CALL BCAST_HEADER(TZFD,TZFMH)
    !
    IF (HDIR == 'XX' .OR. HDIR =='YY') THEN
      ! XX or YY Scatter Field
      CALL SCATTER_XXFIELD(HDIR,IFIELDP,KFIELD,TZFD%OWNER,TZFD&
           & %COMM) 
    ELSE IF (HDIR == 'XY') THEN
      IF (LPACK .AND. L2D) THEN
        ! 2D compact case
        CALL SCATTER_XXFIELD('XX',IFIELDP(:,1),KFIELD(:,JPHEXT+1),TZFD%OWNER,TZFD%COMM)
        KFIELD(:,:) = SPREAD(KFIELD(:,JPHEXT+1),DIM=2,NCOPIES=IHEXTOT)
      ELSE
        ! XY Scatter Field
        CALL SCATTER_XYFIELD(IFIELDP,KFIELD,TZFD%OWNER,TZFD%COMM)
      END IF
    ELSE
      ! Broadcast Field
      IF (ISP == TZFD%OWNER) KFIELD = IFIELDP
      CALL MPI_BCAST(KFIELD,SIZE(KFIELD),MPI_INTEGER,TZFD%OWNER-1&
           & ,TZFD%COMM,IERR)
    END IF
  END IF
  KGRID  = TZFMH%GRID
  KLENCH = TZFMH%COMLEN
  HCOMMENT = TZFMH%COMMENT(1:TZFMH%COMLEN)
ELSE 
  IRESP = -61
END IF
!----------------------------------------------------------------
1000 CONTINUE
!! Error handler
IF (IRESP.NE.0) THEN
  CALL FM_READ_ERR("FMREADN2_ll",HFILEM,HFIPRI,HRECFM,HDIR,IRESP)
ENDIF
!
IF (GALLOC) DEALLOCATE (IFIELDP)
KRESP = IRESP
RETURN
!------------------------------------------------------------------
END SUBROUTINE FMREADN2_ll


SUBROUTINE FMREADL0_ll(HFILEM,HRECFM,HFIPRI,HDIR,OFIELD,KGRID,&
     KLENCH,HCOMMENT,KRESP)
USE MODD_IO_ll, ONLY : ISP,GSMONOPROC 
USE MODD_FM
USE MODE_FD_ll, ONLY : GETFD,JPFINL,FD_LL

!*      0.    DECLARATIONS
!             ------------
!
!
!*      0.1   Declarations of arguments
!
CHARACTER(LEN=*),          INTENT(IN) ::HFILEM ! FM-file name
CHARACTER(LEN=*),          INTENT(IN) ::HRECFM ! name of the article to read
CHARACTER(LEN=*),          INTENT(IN) ::HFIPRI ! output file for error messages
CHARACTER(LEN=*),          INTENT(IN) ::HDIR   ! field form
LOGICAL,                   INTENT(INOUT)::OFIELD ! array containing the data field
INTEGER,                   INTENT(INOUT)::KGRID  ! C-grid indicator (u,v,w,T)
INTEGER,                   INTENT(INOUT)::KLENCH ! length of comment string
CHARACTER(LEN=*),          INTENT(INOUT)::HCOMMENT ! comment string
INTEGER,                   INTENT(INOUT)::KRESP    ! return-code
!
!*      0.2   Declarations of local variables
!
CHARACTER(LEN=JPFINL)        :: YFNLFI
INTEGER                      :: IERR
TYPE(FD_ll), POINTER         :: TZFD
INTEGER                      :: IRESP
INTEGER                      :: IFIELD
TYPE(FMHEADER)               :: TZFMH
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','FMREADL0_ll','reading '//TRIM(HRECFM))
!
!*      1.1   THE NAME OF LFIFM
!
IRESP = 0
YFNLFI=TRIM(ADJUSTL(HFILEM))//'.lfi'
!------------------------------------------------------------------
TZFD=>GETFD(YFNLFI)
IF (ASSOCIATED(TZFD)) THEN
  IF (GSMONOPROC) THEN ! sequential execution
    IF (ASSOCIATED(TZFD%CDF)) THEN
       CALL NCREAD(TZFD%CDF%NCID,HRECFM,IFIELD,TZFMH,IRESP)
    ELSE
       CALL FM_READ_ll(TZFD%FLU,HRECFM,.FALSE.,1,IFIELD,TZFMH,IRESP)
    END IF
    IF (IRESP /= 0) GOTO 1000
  ELSE
    IF (ISP == TZFD%OWNER)  THEN
      IF (ASSOCIATED(TZFD%CDF)) THEN
         CALL NCREAD(TZFD%CDF%NCID,HRECFM,IFIELD,TZFMH,IRESP)
      ELSE
         CALL FM_READ_ll(TZFD%FLU,HRECFM,.FALSE.,1,IFIELD,TZFMH,IRESP)
      END IF
    END IF
    !
    CALL MPI_BCAST(IRESP,1,MPI_INTEGER,TZFD%OWNER-1,TZFD%COMM,IERR)
    IF (IRESP /= 0) GOTO 1000
    !
    CALL BCAST_HEADER(TZFD,TZFMH)
    !
    CALL MPI_BCAST(IFIELD,1,MPI_INTEGER,TZFD%OWNER-1,TZFD%COMM,&
         & IERR)
  END IF
  IF (IFIELD==1) THEN
    OFIELD=.TRUE.
  ELSE
    OFIELD=.FALSE.
  END IF
  KGRID  = TZFMH%GRID
  KLENCH = TZFMH%COMLEN
  HCOMMENT = TZFMH%COMMENT(1:TZFMH%COMLEN)
ELSE 
  IRESP = -61
END IF
!----------------------------------------------------------------
1000 CONTINUE
!! Error handler
IF (IRESP.NE.0) THEN
  CALL FM_READ_ERR("FMREADL0_ll",HFILEM,HFIPRI,HRECFM,HDIR,IRESP)
ENDIF
KRESP = IRESP
RETURN

END SUBROUTINE FMREADL0_ll

SUBROUTINE IO_READ_FIELD_BYNAME_L0(TPFILE,HNAME,OFIELD,KRESP)
!
TYPE(TFILEDATA),  INTENT(IN)    :: TPFILE
CHARACTER(LEN=*), INTENT(IN)    :: HNAME    ! name of the field to write
LOGICAL,          INTENT(INOUT) :: OFIELD   ! array containing the data field
INTEGER,OPTIONAL, INTENT(OUT)   :: KRESP    ! return-code
!
INTEGER :: ID ! Index of the field
INTEGER :: IRESP ! return_code
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_READ_FIELD_BYNAME_L0',TRIM(TPFILE%CNAME)//': reading '//TRIM(HNAME))
!
CALL FIND_FIELD_ID_FROM_MNHNAME(HNAME,ID,IRESP)
!
IF(IRESP==0) CALL IO_READ_FIELD(TPFILE,TFIELDLIST(ID),OFIELD,IRESP)
!
IF (PRESENT(KRESP)) KRESP = IRESP
!
END SUBROUTINE IO_READ_FIELD_BYNAME_L0

SUBROUTINE IO_READ_FIELD_BYFIELD_L0(TPFILE,TPFIELD,OFIELD,KRESP)
!
USE MODD_IO_ll, ONLY : ISP,GSMONOPROC
USE MODE_FD_ll, ONLY : GETFD,FD_LL
!
TYPE(TFILEDATA),  INTENT(IN)    :: TPFILE
TYPE(TFIELDDATA), INTENT(INOUT) :: TPFIELD
LOGICAL,          INTENT(INOUT) :: OFIELD   ! array containing the data field
INTEGER,OPTIONAL, INTENT(OUT)   :: KRESP    ! return-code
!
INTEGER                      :: IERR
TYPE(FD_ll), POINTER         :: TZFD
INTEGER                      :: IRESP
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_READ_FIELD_BYFIELD_L0',TRIM(TPFILE%CNAME)//': reading '//TRIM(TPFIELD%CMNHNAME))
!
IRESP = 0
!
TZFD=>GETFD(TRIM(ADJUSTL(TPFILE%CNAME))//'.lfi')
IF (ASSOCIATED(TZFD)) THEN
  IF (GSMONOPROC) THEN ! sequential execution
      IF (TPFILE%CFORMAT=='NETCDF4') THEN
         CALL IO_READ_FIELD_NC4(TPFILE,TPFIELD,OFIELD,IRESP)
      ELSE IF (TPFILE%CFORMAT=='LFI') THEN
         CALL IO_READ_FIELD_LFI(TPFILE,TPFIELD,OFIELD,IRESP)
      ELSE
         CALL PRINT_MSG(NVERB_FATAL,'IO','IO_READ_FIELD_BYFIELD_L0',&
                        TRIM(TPFILE%CNAME)//': invalid fileformat ('//TRIM(TPFILE%CFORMAT)//')')
      END IF
  ELSE
    IF (ISP == TZFD%OWNER)  THEN
      IF (TPFILE%CFORMAT=='NETCDF4') THEN
         CALL IO_READ_FIELD_NC4(TPFILE,TPFIELD,OFIELD,IRESP)
      ELSE IF (TPFILE%CFORMAT=='LFI') THEN
         CALL IO_READ_FIELD_LFI(TPFILE,TPFIELD,OFIELD,IRESP)
      ELSE
         CALL PRINT_MSG(NVERB_FATAL,'IO','IO_READ_FIELD_BYFIELD_L0',&
                        TRIM(TPFILE%CNAME)//': invalid fileformat ('//TRIM(TPFILE%CFORMAT)//')')
      END IF
    END IF
    !
    CALL MPI_BCAST(IRESP,1,MPI_INTEGER,TZFD%OWNER-1,TZFD%COMM,IERR)
    !
    !Broadcast header only if IRESP==-111
    !because metadata of field has been modified in IO_READ_FIELD_xxx
    IF (IRESP==-111) CALL IO_BCAST_FIELD_METADATA(TZFD,TPFIELD)
    !
    CALL MPI_BCAST(OFIELD,1,MPI_LOGICAL,TZFD%OWNER-1,TZFD%COMM,IERR)
  END IF
ELSE
  IRESP = -61
  CALL PRINT_MSG(NVERB_ERROR,'IO','IO_READ_FIELD_BYFIELD_L0','file '//TRIM(TPFILE%CNAME)//' not found')
END IF
!
IF (PRESENT(KRESP)) KRESP = IRESP
!
END SUBROUTINE IO_READ_FIELD_BYFIELD_L0


SUBROUTINE FMREADL1_ll(HFILEM,HRECFM,HFIPRI,HDIR,OFIELD,KGRID,&
     KLENCH,HCOMMENT,KRESP)
USE MODD_IO_ll, ONLY : ISP,GSMONOPROC 
USE MODD_FM
USE MODE_FD_ll, ONLY : GETFD,JPFINL,FD_LL
!
!*      0.    DECLARATIONS
!             ------------
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
CHARACTER(LEN=*),          INTENT(IN) ::HFILEM  ! FM-file name
CHARACTER(LEN=*),          INTENT(IN) ::HRECFM  ! name of the article to read
CHARACTER(LEN=*),          INTENT(IN) ::HFIPRI  ! output file for error messages
CHARACTER(LEN=*),          INTENT(IN) ::HDIR    ! Field form
LOGICAL, DIMENSION(:),     INTENT(INOUT)::OFIELD  ! array containing the data field
INTEGER,                   INTENT(INOUT)::KGRID    ! C-grid indicator (u,v,w,T)
INTEGER,                   INTENT(INOUT)::KLENCH   ! length of comment string
CHARACTER(LEN=*),          INTENT(INOUT)::HCOMMENT ! comment string
INTEGER,                   INTENT(INOUT)::KRESP    ! return-code
!
!*      0.2   Declarations of local variables
!

CHARACTER(LEN=JPFINL)            :: YFNLFI
INTEGER                          :: IERR
TYPE(FD_ll), POINTER             :: TZFD
INTEGER                          :: IRESP
INTEGER, DIMENSION(SIZE(OFIELD)) :: IFIELD
TYPE(FMHEADER)                   :: TZFMH
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','FMREADL1_ll','reading '//TRIM(HRECFM))
!
!*      1.1   THE NAME OF LFIFM
!
IRESP = 0
YFNLFI=TRIM(ADJUSTL(HFILEM))//'.lfi'
!
TZFD=>GETFD(YFNLFI)
IF (ASSOCIATED(TZFD)) THEN
  IF (GSMONOPROC) THEN ! sequential execution
      IF (ASSOCIATED(TZFD%CDF)) THEN
         CALL NCREAD(TZFD%CDF%NCID,HRECFM,IFIELD,TZFMH,IRESP)
      ELSE
         CALL FM_READ_ll(TZFD%FLU,HRECFM,.FALSE.,SIZE(IFIELD),IFIELD,TZFMH&
         & ,IRESP)
      END IF
    IF (IRESP /= 0) GOTO 1000
  ELSE
    IF (ISP == TZFD%OWNER)  THEN
      IF (ASSOCIATED(TZFD%CDF)) THEN
         CALL NCREAD(TZFD%CDF%NCID,HRECFM,IFIELD,TZFMH,IRESP)
      ELSE
         CALL FM_READ_ll(TZFD%FLU,HRECFM,.FALSE.,SIZE(IFIELD),IFIELD,TZFMH&
           & ,IRESP)
      END IF
    END IF
    !
    CALL MPI_BCAST(IRESP,1,MPI_INTEGER,TZFD%OWNER-1,TZFD%COMM,IERR)
    IF (IRESP /= 0) GOTO 1000
    !
    CALL BCAST_HEADER(TZFD,TZFMH)
    !
    CALL MPI_BCAST(IFIELD,SIZE(IFIELD),MPI_INTEGER,TZFD%OWNER-1,TZFD&
       & %COMM,IERR)
  END IF
  WHERE (IFIELD==1)
    OFIELD=.TRUE.
  ELSEWHERE
    OFIELD=.FALSE.
  END WHERE
  KGRID  = TZFMH%GRID
  KLENCH = TZFMH%COMLEN
  HCOMMENT = TZFMH%COMMENT(1:TZFMH%COMLEN)
ELSE 
  IRESP = -61
END IF
!----------------------------------------------------------------
1000 CONTINUE
!! Error handler
IF (IRESP.NE.0) THEN
  CALL FM_READ_ERR("FMREADL1_ll",HFILEM,HFIPRI,HRECFM,HDIR,IRESP)
ENDIF
KRESP = IRESP
RETURN

END SUBROUTINE FMREADL1_ll

SUBROUTINE FMREADC0_ll(HFILEM,HRECFM,HFIPRI,HDIR,HFIELD,KGRID,&
     KLENCH,HCOMMENT,KRESP)
USE MODD_IO_ll, ONLY : ISP,GSMONOPROC,LIOCDF4,LLFIREAD
USE MODD_FM
USE MODE_FD_ll, ONLY : GETFD,JPFINL,FD_LL
!
!*      0.    DECLARATIONS
!             ------------
!
!
!*      0.1   Declarations of arguments
!
CHARACTER(LEN=*),          INTENT(IN) ::HFILEM   ! FM-file name
CHARACTER(LEN=*),          INTENT(IN) ::HRECFM   ! name of the article to read
CHARACTER(LEN=*),          INTENT(IN) ::HFIPRI   ! output file for error messages
CHARACTER(LEN=*),          INTENT(IN) ::HDIR     ! Field form
CHARACTER(LEN=*),          INTENT(INOUT)::HFIELD   ! array containing the data field    
INTEGER,                   INTENT(INOUT)::KGRID    ! C-grid indicator (u,v,w,T)
INTEGER,                   INTENT(INOUT)::KLENCH   ! length of comment string
CHARACTER(LEN=*),          INTENT(INOUT)::HCOMMENT ! comment string
INTEGER,                   INTENT(INOUT)::KRESP    ! return-code
!
!*      0.2   Declarations of local variables
!
CHARACTER(LEN=JPFINL)             :: YFNLFI
INTEGER                           :: IERR
TYPE(FD_ll), POINTER              :: TZFD
INTEGER                           :: IRESP
INTEGER                           :: JLOOP
INTEGER, DIMENSION(LEN(HFIELD))   :: IFIELD
CHARACTER(LEN(HFIELD))            :: YFIELD
INTEGER                           :: ILENG
TYPE(FMHEADER)                    :: TZFMH
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','FMREADC0_ll','reading '//TRIM(HRECFM))
!
!*      1.1   THE NAME OF LFIFM
!
IRESP = 0
YFNLFI=TRIM(ADJUSTL(HFILEM))//'.lfi'
ILENG=LEN(HFIELD)
!
TZFD=>GETFD(YFNLFI)
IF (ASSOCIATED(TZFD)) THEN
  IF (GSMONOPROC) THEN  ! sequential execution
      IF (ASSOCIATED(TZFD%CDF)) THEN
         CALL NCREAD(TZFD%CDF%NCID,HRECFM,YFIELD,TZFMH,IRESP)
      ELSE
         CALL FM_READ_ll(TZFD%FLU,HRECFM,.FALSE.,ILENG,IFIELD,TZFMH,IRESP)
      END IF
    IF (IRESP /= 0) GOTO 1000
  ELSE ! parallel execution
    IF (ISP == TZFD%OWNER)  THEN
      IF (ASSOCIATED(TZFD%CDF)) THEN
         CALL NCREAD(TZFD%CDF%NCID,HRECFM,YFIELD,TZFMH,IRESP)
      ELSE
         CALL FM_READ_ll(TZFD%FLU,HRECFM,.FALSE.,ILENG,IFIELD,TZFMH,IRESP)
      END IF
    END IF
    !
    CALL MPI_BCAST(IRESP,1,MPI_INTEGER,TZFD%OWNER-1,TZFD%COMM,IERR)
    IF (IRESP /= 0) GOTO 1000
    !
    CALL BCAST_HEADER(TZFD,TZFMH)
    !
    IF (LIOCDF4 .AND. .NOT. LLFIREAD) THEN
       ! NetCDF
       CALL MPI_BCAST(YFIELD,ILENG,MPI_CHARACTER,TZFD%OWNER-1,TZFD%COMM,&
            &IERR)
    ELSE 
       ! LFI
       CALL MPI_BCAST(IFIELD,ILENG,MPI_INTEGER,TZFD%OWNER-1,TZFD%COMM,&
            & IERR)
    END IF
  END IF ! parallel execution
  !
  IF (LIOCDF4 .AND. .NOT. LLFIREAD) THEN
     ! NetCDF
     HFIELD = YFIELD
  ELSE
     ! LFI Case
     DO JLOOP=1,ILENG
        HFIELD(JLOOP:JLOOP)=ACHAR(IFIELD(JLOOP))
     END DO
  END IF
  KGRID  = TZFMH%GRID
  KLENCH = TZFMH%COMLEN
  HCOMMENT = TZFMH%COMMENT(1:TZFMH%COMLEN)
ELSE 
  IRESP = -61
END IF
!----------------------------------------------------------------
1000 CONTINUE
!! Error handler
IF (IRESP.NE.0) THEN
  CALL FM_READ_ERR("FMREADC0_ll",HFILEM,HFIPRI,HRECFM,HDIR,IRESP)
ENDIF
KRESP = IRESP
RETURN

END SUBROUTINE FMREADC0_ll

SUBROUTINE IO_READ_FIELD_BYNAME_C0(TPFILE,HNAME,HFIELD,KRESP)
!
TYPE(TFILEDATA),  INTENT(IN)    :: TPFILE
CHARACTER(LEN=*), INTENT(IN)    :: HNAME    ! name of the field to write
CHARACTER(LEN=*), INTENT(INOUT) :: HFIELD   ! array containing the data field
INTEGER,OPTIONAL, INTENT(OUT)   :: KRESP    ! return-code
!
INTEGER :: ID ! Index of the field
INTEGER :: IRESP ! return_code
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_READ_FIELD_BYNAME_C0',TRIM(TPFILE%CNAME)//': reading '//TRIM(HNAME))
!
CALL FIND_FIELD_ID_FROM_MNHNAME(HNAME,ID,IRESP)
!
IF(IRESP==0) CALL IO_READ_FIELD(TPFILE,TFIELDLIST(ID),HFIELD,IRESP)
!
IF (PRESENT(KRESP)) KRESP = IRESP
!
END SUBROUTINE IO_READ_FIELD_BYNAME_C0

SUBROUTINE IO_READ_FIELD_BYFIELD_C0(TPFILE,TPFIELD,HFIELD,KRESP)
!
USE MODD_IO_ll, ONLY : ISP,GSMONOPROC
USE MODE_FD_ll, ONLY : GETFD,FD_LL
!
TYPE(TFILEDATA),  INTENT(IN)    :: TPFILE
TYPE(TFIELDDATA), INTENT(INOUT) :: TPFIELD
CHARACTER(LEN=*), INTENT(INOUT) :: HFIELD   ! array containing the data field
INTEGER,OPTIONAL, INTENT(OUT)   :: KRESP    ! return-code
!
INTEGER                      :: IERR
TYPE(FD_ll), POINTER         :: TZFD
INTEGER                      :: IRESP
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_READ_FIELD_BYFIELD_C0',TRIM(TPFILE%CNAME)//': reading '//TRIM(TPFIELD%CMNHNAME))
!
IRESP = 0
!
TZFD=>GETFD(TRIM(ADJUSTL(TPFILE%CNAME))//'.lfi')
IF (ASSOCIATED(TZFD)) THEN
  IF (GSMONOPROC) THEN ! sequential execution
      IF (TPFILE%CFORMAT=='NETCDF4') THEN
         CALL IO_READ_FIELD_NC4(TPFILE,TPFIELD,HFIELD,IRESP)
      ELSE IF (TPFILE%CFORMAT=='LFI') THEN
         CALL IO_READ_FIELD_LFI(TPFILE,TPFIELD,HFIELD,IRESP)
      ELSE
         CALL PRINT_MSG(NVERB_FATAL,'IO','IO_READ_FIELD_BYFIELD_C0',&
                        TRIM(TPFILE%CNAME)//': invalid fileformat ('//TRIM(TPFILE%CFORMAT)//')')
      END IF
  ELSE
    IF (ISP == TZFD%OWNER)  THEN
      IF (TPFILE%CFORMAT=='NETCDF4') THEN
         CALL IO_READ_FIELD_NC4(TPFILE,TPFIELD,HFIELD,IRESP)
      ELSE IF (TPFILE%CFORMAT=='LFI') THEN
         CALL IO_READ_FIELD_LFI(TPFILE,TPFIELD,HFIELD,IRESP)
      ELSE
         CALL PRINT_MSG(NVERB_FATAL,'IO','IO_READ_FIELD_BYFIELD_C0',&
                        TRIM(TPFILE%CNAME)//': invalid fileformat ('//TRIM(TPFILE%CFORMAT)//')')
      END IF
    END IF
    !
    CALL MPI_BCAST(IRESP,1,MPI_INTEGER,TZFD%OWNER-1,TZFD%COMM,IERR)
    !
    !Broadcast header only if IRESP==-111
    !because metadata of field has been modified in IO_READ_FIELD_xxx
    IF (IRESP==-111) CALL IO_BCAST_FIELD_METADATA(TZFD,TPFIELD)
    !
    CALL MPI_BCAST(HFIELD,LEN(HFIELD),MPI_CHARACTER,TZFD%OWNER-1,TZFD%COMM,IERR)
  END IF
ELSE
  IRESP = -61
  CALL PRINT_MSG(NVERB_ERROR,'IO','IO_READ_FIELD_BYFIELD_C0','file '//TRIM(TPFILE%CNAME)//' not found')
END IF
!
IF (PRESENT(KRESP)) KRESP = IRESP
!
END SUBROUTINE IO_READ_FIELD_BYFIELD_C0


SUBROUTINE FMREADT0_ll(HFILEM,HRECFM,HFIPRI,HDIR,TFIELD,KGRID,&
                           KLENCH,HCOMMENT,KRESP)
!*      0.    DECLARATIONS
!             ------------
!
USE MODD_IO_ll, ONLY : ISP,GSMONOPROC 
USE MODD_TYPE_DATE
USE MODD_FM
USE MODE_FD_ll, ONLY : GETFD,JPFINL,FD_LL
!
!*      0.1   Declarations of arguments
!
CHARACTER(LEN=*),          INTENT(IN) ::HFILEM   ! FM-file name
CHARACTER(LEN=*),          INTENT(IN) ::HRECFM   ! name of the article to read
CHARACTER(LEN=*),          INTENT(IN) ::HFIPRI   ! output file for error messages
CHARACTER(LEN=*),          INTENT(IN) ::HDIR     ! Field form
TYPE (DATE_TIME),          INTENT(INOUT)::TFIELD ! array containing the data field
INTEGER,                   INTENT(INOUT)::KGRID    ! C-grid indicator (u,v,w,T)
INTEGER,                   INTENT(INOUT)::KLENCH   ! length of comment string
CHARACTER(LEN=*),          INTENT(INOUT)::HCOMMENT ! comment string
INTEGER,                   INTENT(INOUT)::KRESP    ! return-code
!
!
!*      0.2   Declarations of local variables
!
!-------------------------------------------------------------------------------


CHARACTER(LEN=JPFINL)        :: YFNLFI
INTEGER                      :: IERR
TYPE(FD_ll), POINTER         :: TZFD
INTEGER                      :: IRESP
INTEGER,DIMENSION(3)         :: ITDATE
REAL                         :: ZTIME
TYPE(FMHEADER)               :: TZFMH
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','FMREADT0_ll','reading '//TRIM(HRECFM))
!
!*      1.1   THE NAME OF LFIFM
!
IRESP = 0

YFNLFI=TRIM(ADJUSTL(HFILEM))//'.lfi'

TZFD=>GETFD(YFNLFI)
IF (ASSOCIATED(TZFD)) THEN
  IF (GSMONOPROC) THEN ! sequential execution
    IF (ASSOCIATED(TZFD%CDF)) THEN
       CALL NCREAD(TZFD%CDF%NCID,TRIM(HRECFM)//'%TDATE',ITDATE,TZFMH,IRESP)
       CALL NCREAD(TZFD%CDF%NCID,TRIM(HRECFM)//'%TIME',ZTIME,TZFMH,IRESP)
    ELSE
       CALL FM_READ_ll(TZFD%FLU,TRIM(HRECFM)//'%TDATE',.FALSE.,3,ITDATE&
       & ,TZFMH,IRESP)
       CALL FM_READ_ll(TZFD%FLU,TRIM(HRECFM)//'%TIME',.TRUE.,1,ZTIME&
       & ,TZFMH,IRESP)
    END IF
    IF (IRESP /= 0) GOTO 1000
  ELSE
    IF (ISP == TZFD%OWNER)  THEN
       IF (ASSOCIATED(TZFD%CDF)) THEN
          CALL NCREAD(TZFD%CDF%NCID,TRIM(HRECFM)//'%TDATE',ITDATE,TZFMH,IRESP)
          CALL NCREAD(TZFD%CDF%NCID,TRIM(HRECFM)//'%TIME',ZTIME,TZFMH,IRESP)
       ELSE
          CALL FM_READ_ll(TZFD%FLU,TRIM(HRECFM)//'%TDATE',.FALSE.,3,ITDATE&
               & ,TZFMH,IRESP)
          CALL FM_READ_ll(TZFD%FLU,TRIM(HRECFM)//'%TIME',.TRUE.,1,ZTIME&
               & ,TZFMH,IRESP)

       END IF
    END IF
    !
    CALL MPI_BCAST(IRESP,1,MPI_INTEGER,TZFD%OWNER-1,TZFD%COMM,IERR)
    IF (IRESP /= 0) GOTO 1000
    ! Last header is significant
    CALL BCAST_HEADER(TZFD,TZFMH)      
    !
    CALL MPI_BCAST(ITDATE,3,MPI_INTEGER,TZFD%OWNER-1,TZFD%COMM,IERR)
    CALL MPI_BCAST(ZTIME,1,MPI_FLOAT,TZFD%OWNER-1,TZFD%COMM,IERR)
  END IF
  TFIELD%TDATE = DATE(ITDATE(1),ITDATE(2),ITDATE(3))
  TFIELD%TIME = ZTIME
  KGRID  = TZFMH%GRID
  KLENCH = TZFMH%COMLEN
  HCOMMENT = TZFMH%COMMENT(1:TZFMH%COMLEN)
ELSE 
  IRESP = -61
END IF
!----------------------------------------------------------------
1000 CONTINUE
!! Error handler
IF (IRESP.NE.0) THEN
  CALL FM_READ_ERR("FMREADT0_ll",HFILEM,HFIPRI,HRECFM,HDIR,IRESP)
ENDIF
KRESP = IRESP
RETURN

END SUBROUTINE FMREADT0_ll

SUBROUTINE FMREAD_LB(HFILEM,HRECFM,HFIPRI,HLBTYPE,PLB,KRIM,KL3D,&
     & KGRID,KLENCH,HCOMMENT,KRESP)
USE MODD_FM
USE MODD_IO_ll,        ONLY : ISP,ISNPROC,GSMONOPROC,LPACK,L2D 
USE MODD_PARAMETERS_ll,ONLY : JPHEXT
USE MODE_DISTRIB_LB
USE MODE_TOOLS_ll,     ONLY : GET_GLOBALDIMS_ll
USE MODE_FD_ll,        ONLY : GETFD,JPFINL,FD_LL
!JUANZ
USE MODD_TIMEZ, ONLY : TIMEZ
USE MODE_MNH_TIMING, ONLY : SECOND_MNH2
!JUANZ
USE MODD_VAR_ll, ONLY : MNH_STATUSES_IGNORE

CHARACTER(LEN=*),     INTENT(IN) ::HFILEM   ! file name
CHARACTER(LEN=*),     INTENT(IN) ::HRECFM   ! name of the article to be written
CHARACTER(LEN=*),     INTENT(IN) ::HFIPRI   ! file for prints
CHARACTER(LEN=*),     INTENT(IN) ::HLBTYPE  ! 'LBX','LBXU','LBY' or 'LBYV'
REAL, DIMENSION(:,:,:),TARGET, INTENT(INOUT)::PLB ! array containing the LB field
INTEGER,              INTENT(IN) :: KRIM  ! size of the LB area
INTEGER,              INTENT(IN) :: KL3D  ! size of the LB array in FM
INTEGER,              INTENT(INOUT)::KGRID  ! C-grid indicator (u,v,w,T)
INTEGER,              INTENT(INOUT)::KLENCH ! length of comment string
CHARACTER(LEN=*),     INTENT(INOUT)::HCOMMENT ! comment string
INTEGER,              INTENT(INOUT)::KRESP  ! return-code 
!
!*      0.2   Declarations of local variables
!
CHARACTER(LEN=JPFINL)        :: YFNLFI
INTEGER                      :: IERR
TYPE(FD_ll), POINTER         :: TZFD
INTEGER                      :: IRESP
REAL,DIMENSION(:,:,:),ALLOCATABLE,TARGET :: Z3D
REAL,DIMENSION(:,:,:), POINTER           :: TX3DP
TYPE(FMHEADER)               :: TZFMH
INTEGER :: IIMAX_ll,IJMAX_ll
INTEGER :: IIB,IIE,IJB,IJE
INTEGER :: JI
INTEGER, DIMENSION(MPI_STATUS_SIZE) :: STATUS
INTEGER, ALLOCATABLE,DIMENSION(:,:)   :: STATUSES
!JUANZIO
!JUAN INTEGER,SAVE,DIMENSION(100000)    :: REQ_TAB
INTEGER,ALLOCATABLE,DIMENSION(:)    :: REQ_TAB
INTEGER                           :: NB_REQ,IKU
TYPE TX_3DP
REAL,DIMENSION(:,:,:), POINTER    :: X
END TYPE
TYPE(TX_3DP),ALLOCATABLE,DIMENSION(:) :: T_TX3DP
REAL*8,DIMENSION(2) :: T0,T1,T2,T3
REAL*8,DIMENSION(2) :: T11,T22
!JUANZIO
INTEGER             :: IHEXTOT
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','FMREAD_LB','reading '//TRIM(HRECFM))
!
!*      1.1   THE NAME OF LFIFM
!
CALL SECOND_MNH2(T11)
IRESP = 0
YFNLFI=TRIM(ADJUSTL(HFILEM))//'.lfi'
!------------------------------------------------------------------
IHEXTOT = 2*JPHEXT+1
TZFD=>GETFD(YFNLFI)
IF (ASSOCIATED(TZFD)) THEN
  IF (GSMONOPROC) THEN ! sequential execution
    IF (HLBTYPE == 'LBX' .OR. HLBTYPE == 'LBXU') THEN 
      ALLOCATE(Z3D(KL3D,SIZE(PLB,2),SIZE(PLB,3)))
      Z3D = 0.0
      IF (LPACK .AND. L2D) THEN
        TX3DP=>Z3D(:,JPHEXT+1:JPHEXT+1,:)
        IF (ASSOCIATED(TZFD%CDF)) THEN
           CALL NCREAD(TZFD%CDF%NCID,HRECFM,TX3DP,TZFMH,IRESP)
        ELSE
           CALL FM_READ_ll(TZFD%FLU,HRECFM,.TRUE.,SIZE(TX3DP),TX3DP,TZFMH,IRESP)
        END IF
        Z3D(:,:,:) = SPREAD(Z3D(:,JPHEXT+1,:),DIM=2,NCOPIES=IHEXTOT)
      ELSE
         IF (ASSOCIATED(TZFD%CDF)) THEN
            CALL NCREAD(TZFD%CDF%NCID,HRECFM,Z3D,TZFMH,IRESP)
         ELSE
            CALL FM_READ_ll(TZFD%FLU,HRECFM,.TRUE.,SIZE(Z3D),Z3D,TZFMH,IRESP)
         END IF
      END IF
      PLB(1:KRIM+JPHEXT,:,:)          = Z3D(1:KRIM+JPHEXT,:,:)
      PLB(KRIM+JPHEXT+1:2*(KRIM+JPHEXT),:,:) = Z3D(KL3D-KRIM-JPHEXT+1:KL3D,:,:)
    ELSE !(HLBTYPE == 'LBY' .OR. HLBTYPE == 'LBYV') 
      ALLOCATE(Z3D(SIZE(PLB,1),KL3D,SIZE(PLB,3)))
      Z3D = 0.0
      IF (ASSOCIATED(TZFD%CDF)) THEN
         CALL NCREAD(TZFD%CDF%NCID,HRECFM,Z3D,TZFMH,IRESP)
      ELSE
         CALL FM_READ_ll(TZFD%FLU,HRECFM,.TRUE.,SIZE(Z3D),Z3D,TZFMH,IRESP)
      END IF
      PLB(:,1:KRIM+JPHEXT,:)          = Z3D(:,1:KRIM+JPHEXT,:)
      PLB(:,KRIM+JPHEXT+1:2*(KRIM+JPHEXT),:) = Z3D(:,KL3D-KRIM-JPHEXT+1:KL3D,:)
    END IF
    IF (IRESP /= 0) GOTO 1000
  ELSE                 ! multiprocessor execution
    IF (ISP == TZFD%OWNER)  THEN
      CALL SECOND_MNH2(T0)
      CALL GET_GLOBALDIMS_ll(IIMAX_ll,IJMAX_ll)
      IF (HLBTYPE == 'LBX' .OR. HLBTYPE == 'LBXU') THEN 
        ALLOCATE(Z3D(KL3D,IJMAX_ll+2*JPHEXT,SIZE(PLB,3)))
        Z3D = 0.0
        IF (LPACK .AND. L2D) THEN
          TX3DP=>Z3D(:,JPHEXT+1:JPHEXT+1,:)
          IF (ASSOCIATED(TZFD%CDF)) THEN
             CALL NCREAD(TZFD%CDF%NCID,HRECFM,TX3DP,TZFMH,IRESP)
          ELSE
             CALL FM_READ_ll(TZFD%FLU,HRECFM,.TRUE.,SIZE(TX3DP),TX3DP,TZFMH,IRESP)
          END IF
          Z3D(:,:,:) = SPREAD(Z3D(:,JPHEXT+1,:),DIM=2,NCOPIES=IHEXTOT)
        ELSE
           IF (ASSOCIATED(TZFD%CDF)) THEN
              CALL NCREAD(TZFD%CDF%NCID,HRECFM,Z3D,TZFMH,IRESP)
           ELSE
              CALL FM_READ_ll(TZFD%FLU,HRECFM,.TRUE.,SIZE(Z3D),Z3D,TZFMH,IRESP)
           END IF
        END IF
        ! erase gap in LB field
        Z3D(KRIM+JPHEXT+1:2*(KRIM+JPHEXT),:,:) = Z3D(KL3D-KRIM-JPHEXT+1:KL3D,:,:)
      ELSE !(HLBTYPE == 'LBY' .OR. HLBTYPE == 'LBYV') 
        ALLOCATE(Z3D(IIMAX_ll+2*JPHEXT,KL3D,SIZE(PLB,3)))
        Z3D = 0.0
        IF (ASSOCIATED(TZFD%CDF)) THEN
           CALL NCREAD(TZFD%CDF%NCID,HRECFM,Z3D,TZFMH,IRESP)
        ELSE
           CALL FM_READ_ll(TZFD%FLU,HRECFM,.TRUE.,SIZE(Z3D),Z3D,TZFMH,IRESP)
        END IF
        ! erase gap in LB field
        Z3D(:,KRIM+JPHEXT+1:2*(KRIM+JPHEXT),:) = Z3D(:,KL3D-KRIM-JPHEXT+1:KL3D,:)
      END IF
      CALL SECOND_MNH2(T1)
      TIMEZ%T_READLB_READ=TIMEZ%T_READLB_READ + T1 - T0
    END IF
    !  
    CALL MPI_BCAST(IRESP,1,MPI_INTEGER,TZFD%OWNER-1,TZFD%COMM,IERR)
    IF (IRESP /= 0) GOTO 1000
    !  
    CALL BCAST_HEADER(TZFD,TZFMH)
    ! 
    NB_REQ=0
    ALLOCATE(REQ_TAB(ISNPROC-1))
    !REQ_TAB=MPI_REQUEST_NULL
    IF (ISP == TZFD%OWNER)  THEN
       CALL SECOND_MNH2(T1)
      !ALLOCATE(REQ_TAB(ISNPROC-1))
      !REQ_TAB=MPI_REQUEST_NULL
      ALLOCATE(T_TX3DP(ISNPROC-1))
      IKU = SIZE(Z3D,3)
      DO JI = 1,ISNPROC
        CALL GET_DISTRIB_LB(HLBTYPE,JI,'FM','READ',KRIM,IIB,IIE,IJB,IJE)
        IF (IIB /= 0) THEN
          TX3DP=>Z3D(IIB:IIE,IJB:IJE,:)
          IF (ISP /= JI) THEN 
            NB_REQ = NB_REQ + 1
            ALLOCATE(T_TX3DP(NB_REQ)%X(IIB:IIE,IJB:IJE,IKU))           
            T_TX3DP(NB_REQ)%X=Z3D(IIB:IIE,IJB:IJE,:)
            CALL MPI_ISEND(T_TX3DP(NB_REQ)%X,SIZE(TX3DP),MPI_FLOAT,JI-1,99,TZFD%COMM,REQ_TAB(NB_REQ),IERR)
            !CALL MPI_BSEND(T_TX3DP(NB_REQ)%X,SIZE(TX3DP),MPI_FLOAT,JI-1,99,TZFD%COMM,IERR)
          ELSE
            CALL GET_DISTRIB_LB(HLBTYPE,JI,'LOC','READ',KRIM,IIB,IIE,IJB,IJE)
            PLB(IIB:IIE,IJB:IJE,:) = TX3DP(:,:,:)
          END IF
        END IF
      END DO
      CALL SECOND_MNH2(T2)
      TIMEZ%T_READLB_SEND=TIMEZ%T_READLB_SEND + T2 - T1      
      IF (NB_REQ .GT.0 ) THEN
         !ALLOCATE(STATUSES(MPI_STATUS_SIZE,NB_REQ))
         !CALL MPI_WAITALL(NB_REQ,REQ_TAB,STATUSES,IERR)
         CALL MPI_WAITALL(NB_REQ,REQ_TAB,MNH_STATUSES_IGNORE,IERR)
         !DEALLOCATE(STATUSES)
         DO JI=1,NB_REQ ;  DEALLOCATE(T_TX3DP(JI)%X) ; ENDDO
      END IF
      DEALLOCATE(T_TX3DP)
      !DEALLOCATE(REQ_TAB)
      CALL SECOND_MNH2(T3)
      TIMEZ%T_READLB_WAIT=TIMEZ%T_READLB_WAIT + T3 - T2
    ELSE
       CALL SECOND_MNH2(T0)
      !ALLOCATE(REQ_TAB(1))
      !REQ_TAB=MPI_REQUEST_NULL
      CALL GET_DISTRIB_LB(HLBTYPE,ISP,'LOC','READ',KRIM,IIB,IIE,IJB,IJE)
      IF (IIB /= 0) THEN
        TX3DP=>PLB(IIB:IIE,IJB:IJE,:)
        CALL MPI_RECV(TX3DP,SIZE(TX3DP),MPI_FLOAT,TZFD%OWNER-1,99,TZFD%COMM,STATUS,IERR)
        !NB_REQ = NB_REQ + 1
        !CALL MPI_IRECV(TX3DP,SIZE(TX3DP),MPI_FLOAT,TZFD%OWNER-1,99,TZFD%COMM,REQ_TAB(NB_REQ),IERR)
        !IF (NB_REQ .GT.0 ) CALL MPI_WAITALL(NB_REQ,REQ_TAB,MNH_STATUSES_IGNORE,IERR)
      END IF
      CALL SECOND_MNH2(T1)
      TIMEZ%T_READLB_RECV=TIMEZ%T_READLB_RECV + T1 - T0 
    END IF
    DEALLOCATE(REQ_TAB)
  END IF !(GSMONOPROC)
  KGRID  = TZFMH%GRID
  KLENCH = TZFMH%COMLEN
  HCOMMENT = TZFMH%COMMENT(1:TZFMH%COMLEN)
ELSE 
  IRESP = -61          
END IF
!----------------------------------------------------------------
1000 CONTINUE
!! Error handler
IF (IRESP.NE.0) THEN
  CALL FM_READ_ERR("FMREAD_LB",HFILEM,HFIPRI,HRECFM,HLBTYPE,IRESP)
ENDIF
!
IF (ALLOCATED(Z3D)) DEALLOCATE (Z3D)
KRESP = IRESP
!
!CALL MPI_BARRIER(TZFD%COMM,IERR)
CALL SECOND_MNH2(T22)
TIMEZ%T_READLB_ALL=TIMEZ%T_READLB_ALL + T22 - T11
END SUBROUTINE FMREAD_LB

END MODULE MODE_FMREAD
