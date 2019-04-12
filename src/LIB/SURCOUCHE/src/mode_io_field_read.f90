!MNH_LIC Copyright 1994-2019 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
! Modifications:
!  J. Escobar  22/08/2005: BUG : missing "GOTO 1000" if read field not found
!  J. Escobar  13/01/2015: remove comment on BCAST(IRESP in FMREADX2_ll
!  J. Escobar  15/09/2015: WENO5 & JPHEXT <> 1
!  P. Wautelet 05/2016-04/2018: new data structures and calls for I/O
!  J. Escobar  17/07/2018: reintroduce needed MPI_BARRIER in IO_Field_read_byfield_X3
!  P. Wautelet 29/01/2019: small bug correction in time measurement in IO_Field_read_byfield_X2
!  P. Wautelet 05/03/2019: rename IO subroutines and modules
!  P. Wautelet 10/04/2019: replace ABORT and STOP calls by Print_msg
!  P. Wautelet 12/04/2019: use MNHTIME for time measurement variables
!-----------------------------------------------------------------

MODULE MODE_IO_FIELD_READ
!
USE MODD_IO, ONLY : NVERB_FATAL,NVERB_ERROR,NVERB_WARNING,NVERB_INFO,NVERB_DEBUG,TFILEDATA
USE MODD_MPIF
use modd_precision, only: MNHREAL_MPI, MNHTIME
!
USE MODE_FIELD
USE MODE_IO_READ_LFI
#if defined(MNH_IOCDF4)
USE MODE_IO_READ_NC4
#endif
USE MODE_MSG

IMPLICIT NONE 

PRIVATE

public :: IO_Field_read, IO_Field_read_lb

INTERFACE IO_Field_read
   MODULE PROCEDURE IO_Field_read_byname_X0, IO_Field_read_byname_X1,  &
                    IO_Field_read_byname_X2, IO_Field_read_byname_X3,  &
                    IO_Field_read_byname_X4, IO_Field_read_byname_X5,  &
                    IO_Field_read_byname_X6,                           &
                    IO_Field_read_byname_N0, IO_Field_read_byname_N1,  &
                    IO_Field_read_byname_N2,                           &
                    IO_Field_read_byname_L0, IO_Field_read_byname_L1,  &
                    IO_Field_read_byname_C0,                           &
                    IO_Field_read_byname_T0,                           &
                    IO_Field_read_byfield_X0,IO_Field_read_byfield_X1, &
                    IO_Field_read_byfield_X2,IO_Field_read_byfield_X3, &
                    IO_Field_read_byfield_X4,IO_Field_read_byfield_X5, &
                    IO_Field_read_byfield_X6,                          &
                    IO_Field_read_byfield_N0,IO_Field_read_byfield_N1, &
                    IO_Field_read_byfield_N2,                          &
                    IO_Field_read_byfield_L0,IO_Field_read_byfield_L1, &
                    IO_Field_read_byfield_C0,                          &
                    IO_Field_read_byfield_T0
END INTERFACE

INTERFACE IO_Field_read_lb
   MODULE PROCEDURE IO_Field_read_byname_lb, IO_Field_read_byfield_lb
END INTERFACE

CONTAINS 

SUBROUTINE IO_File_read_check(TPFILE,HSUBR,KRESP)
TYPE(TFILEDATA),  INTENT(IN)  :: TPFILE
CHARACTER(LEN=*), INTENT(IN)  :: HSUBR
INTEGER,          INTENT(OUT) :: KRESP
!
KRESP = 0
!
!Check if file is opened
IF (.NOT.TPFILE%LOPENED) THEN
  CALL PRINT_MSG(NVERB_ERROR,'IO',HSUBR,TRIM(TPFILE%CNAME)//' is not opened')
  KRESP = -201
  RETURN
END IF
!
!Check if file is in the right opening mode
IF (TPFILE%CMODE/='READ') THEN
  CALL PRINT_MSG(NVERB_WARNING,'IO',HSUBR,&
                 TRIM(TPFILE%CNAME)//': reading in a file opened in '//TRIM(TPFILE%CMODE)//' mode')
END IF
!
!Check fileformat
IF (TPFILE%CFORMAT/='NETCDF4' .AND. TPFILE%CFORMAT/='LFI' .AND. TPFILE%CFORMAT/='LFICDF4') THEN
  CALL PRINT_MSG(NVERB_FATAL,'IO',HSUBR,&
                 TRIM(TPFILE%CNAME)//': invalid fileformat ('//TRIM(TPFILE%CFORMAT)//')')
  KRESP = -202
  RETURN
END IF
!
END SUBROUTINE IO_File_read_check


SUBROUTINE IO_Field_metadata_bcast(TPFILE,TPFIELD)
TYPE(TFILEDATA),  INTENT(IN)    :: TPFILE
TYPE(TFIELDDATA), INTENT(INOUT) :: TPFIELD
!
INTEGER :: IERR
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_metadata_bcast','called for '//TRIM(TPFIELD%CMNHNAME))
!
CALL MPI_BCAST(TPFIELD%CMNHNAME, LEN(TPFIELD%CMNHNAME), MPI_CHARACTER,TPFILE%NMASTER_RANK-1,TPFILE%NMPICOMM,IERR)
CALL MPI_BCAST(TPFIELD%CSTDNAME, LEN(TPFIELD%CSTDNAME), MPI_CHARACTER,TPFILE%NMASTER_RANK-1,TPFILE%NMPICOMM,IERR)
CALL MPI_BCAST(TPFIELD%CLONGNAME,LEN(TPFIELD%CLONGNAME),MPI_CHARACTER,TPFILE%NMASTER_RANK-1,TPFILE%NMPICOMM,IERR)
CALL MPI_BCAST(TPFIELD%CUNITS,   LEN(TPFIELD%CUNITS),   MPI_CHARACTER,TPFILE%NMASTER_RANK-1,TPFILE%NMPICOMM,IERR)
CALL MPI_BCAST(TPFIELD%CDIR,     LEN(TPFIELD%CDIR),     MPI_CHARACTER,TPFILE%NMASTER_RANK-1,TPFILE%NMPICOMM,IERR)
CALL MPI_BCAST(TPFIELD%CLBTYPE,  LEN(TPFIELD%CLBTYPE),  MPI_CHARACTER,TPFILE%NMASTER_RANK-1,TPFILE%NMPICOMM,IERR)
CALL MPI_BCAST(TPFIELD%CCOMMENT, LEN(TPFIELD%CCOMMENT), MPI_CHARACTER,TPFILE%NMASTER_RANK-1,TPFILE%NMPICOMM,IERR)
CALL MPI_BCAST(TPFIELD%NGRID,    1,                     MPI_INTEGER,  TPFILE%NMASTER_RANK-1,TPFILE%NMPICOMM,IERR)
CALL MPI_BCAST(TPFIELD%NTYPE,    1,                     MPI_INTEGER,  TPFILE%NMASTER_RANK-1,TPFILE%NMPICOMM,IERR)
CALL MPI_BCAST(TPFIELD%NDIMS,    1,                     MPI_INTEGER,  TPFILE%NMASTER_RANK-1,TPFILE%NMPICOMM,IERR)
!
END SUBROUTINE IO_Field_metadata_bcast


SUBROUTINE IO_Field_read_byname_X0(TPFILE,HNAME,PFIELD,KRESP)
!
TYPE(TFILEDATA),  INTENT(IN)    :: TPFILE
CHARACTER(LEN=*), INTENT(IN)    :: HNAME    ! name of the field to write
REAL,             INTENT(INOUT) :: PFIELD   ! data field
INTEGER,OPTIONAL, INTENT(OUT)   :: KRESP    ! return-code
!
INTEGER :: ID ! Index of the field
INTEGER :: IRESP ! return_code
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_read_byname_X0',TRIM(TPFILE%CNAME)//': reading '//TRIM(HNAME))
!
CALL FIND_FIELD_ID_FROM_MNHNAME(HNAME,ID,IRESP)
!
IF(IRESP==0) CALL IO_Field_read(TPFILE,TFIELDLIST(ID),PFIELD,IRESP)
!
IF (PRESENT(KRESP)) KRESP = IRESP
!
END SUBROUTINE IO_Field_read_byname_X0

SUBROUTINE IO_Field_read_byfield_X0(TPFILE,TPFIELD,PFIELD,KRESP)
!
USE MODD_IO,        ONLY: ISP,GSMONOPROC
!
TYPE(TFILEDATA),  INTENT(IN)    :: TPFILE
TYPE(TFIELDDATA), INTENT(INOUT) :: TPFIELD
REAL,             INTENT(INOUT) :: PFIELD   ! data field
INTEGER,OPTIONAL, INTENT(OUT)   :: KRESP    ! return-code
!
INTEGER                      :: IERR
INTEGER                      :: IRESP
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_read_byfield_X0',TRIM(TPFILE%CNAME)//': reading '//TRIM(TPFIELD%CMNHNAME))
!
IRESP = 0
!
CALL IO_File_read_check(TPFILE,'IO_Field_read_byfield_X0',IRESP)
!
IF (IRESP==0) THEN
  IF (GSMONOPROC) THEN ! sequential execution
      IF (TPFILE%CFORMAT=='NETCDF4') THEN
         CALL IO_Field_read_nc4(TPFILE,TPFIELD,PFIELD,IRESP)
      ELSE IF (TPFILE%CFORMAT=='LFI') THEN
         CALL IO_Field_read_lfi(TPFILE,TPFIELD,PFIELD,IRESP)
      ELSE IF (TPFILE%CFORMAT=='LFICDF4') THEN
        CALL IO_Field_read_nc4(TPFILE,TPFIELD,PFIELD,IRESP)
      END IF
  ELSE
    IF (ISP == TPFILE%NMASTER_RANK)  THEN
      IF (TPFILE%CFORMAT=='NETCDF4') THEN
         CALL IO_Field_read_nc4(TPFILE,TPFIELD,PFIELD,IRESP)
      ELSE IF (TPFILE%CFORMAT=='LFI') THEN
         CALL IO_Field_read_lfi(TPFILE,TPFIELD,PFIELD,IRESP)
      ELSE IF (TPFILE%CFORMAT=='LFICDF4') THEN
        CALL IO_Field_read_nc4(TPFILE,TPFIELD,PFIELD,IRESP)
      END IF
    END IF
    !
    CALL MPI_BCAST(IRESP,1,MPI_INTEGER,TPFILE%NMASTER_RANK-1,TPFILE%NMPICOMM,IERR)
    !
    !Broadcast header only if IRESP==-111
    !because metadata of field has been modified in IO_Field_read_xxx
    IF (IRESP==-111) CALL IO_Field_metadata_bcast(TPFILE,TPFIELD)
    !
    ! Broadcast Field
    CALL MPI_BCAST(PFIELD,1,MNHREAL_MPI,TPFILE%NMASTER_RANK-1,TPFILE%NMPICOMM,IERR)
  END IF
END IF
!
IF (IRESP==-111) IRESP = 0 !-111 is not really an error (metadata has changed)
!
IF (PRESENT(KRESP)) KRESP = IRESP
!
END SUBROUTINE IO_Field_read_byfield_X0


SUBROUTINE IO_Field_read_byname_X1(TPFILE,HNAME,PFIELD,KRESP,KIMAX_ll,KJMAX_ll,TPSPLITTING)
!
USE MODD_IO,           ONLY: ISNPROC
USE MODD_STRUCTURE_ll, ONLY: ZONE_ll
!
TYPE(TFILEDATA),  INTENT(IN)    :: TPFILE
CHARACTER(LEN=*), INTENT(IN)    :: HNAME    ! name of the field to write
REAL,DIMENSION(:),INTENT(INOUT) :: PFIELD   ! array containing the data field
INTEGER,OPTIONAL, INTENT(OUT)   :: KRESP    ! return-code
INTEGER,OPTIONAL, INTENT(IN)    :: KIMAX_ll
INTEGER,OPTIONAL, INTENT(IN)    :: KJMAX_ll
TYPE(ZONE_ll),DIMENSION(ISNPROC),OPTIONAL,INTENT(IN) :: TPSPLITTING  ! splitting of the domain
!
INTEGER :: ID ! Index of the field
INTEGER :: IRESP ! return_code
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_read_byname_X1',TRIM(TPFILE%CNAME)//': reading '//TRIM(HNAME))
!
CALL FIND_FIELD_ID_FROM_MNHNAME(HNAME,ID,IRESP)
!
IF(IRESP==0) CALL IO_Field_read(TPFILE,TFIELDLIST(ID),PFIELD,IRESP,KIMAX_ll,KJMAX_ll,TPSPLITTING)
!
IF (PRESENT(KRESP)) KRESP = IRESP
!
END SUBROUTINE IO_Field_read_byname_X1

SUBROUTINE IO_Field_read_byfield_X1(TPFILE,TPFIELD,PFIELD,KRESP,KIMAX_ll,KJMAX_ll,TPSPLITTING)
!
USE MODD_IO,           ONLY: ISP, GSMONOPROC, ISNPROC
USE MODD_STRUCTURE_ll, ONLY: ZONE_ll
!
USE MODE_SCATTER_ll
USE MODE_ALLOCBUFFER_ll
!
TYPE(TFILEDATA),  INTENT(IN)    :: TPFILE
TYPE(TFIELDDATA), INTENT(INOUT) :: TPFIELD
REAL,DIMENSION(:),INTENT(INOUT) :: PFIELD   ! array containing the data field
INTEGER,OPTIONAL, INTENT(OUT)   :: KRESP    ! return-code
INTEGER,OPTIONAL, INTENT(IN)    :: KIMAX_ll
INTEGER,OPTIONAL, INTENT(IN)    :: KJMAX_ll
TYPE(ZONE_ll),DIMENSION(ISNPROC),OPTIONAL,INTENT(IN) :: TPSPLITTING  ! splitting of the domain
!
INTEGER                      :: IERR
REAL,DIMENSION(:),POINTER    :: ZFIELDP
LOGICAL                      :: GALLOC
INTEGER                      :: IRESP
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_read_byfield_X1',TRIM(TPFILE%CNAME)//': reading '//TRIM(TPFIELD%CMNHNAME))
!
GALLOC = .FALSE.
IRESP = 0
ZFIELDP => NULL()
!
CALL IO_File_read_check(TPFILE,'IO_Field_read_byfield_X1',IRESP)
!
IF (IRESP==0) THEN
  IF (GSMONOPROC) THEN ! sequential execution
      IF (TPFILE%CFORMAT=='NETCDF4') THEN
         CALL IO_Field_read_nc4(TPFILE,TPFIELD,PFIELD,IRESP)
      ELSE IF (TPFILE%CFORMAT=='LFI') THEN
         CALL IO_Field_read_lfi(TPFILE,TPFIELD,PFIELD,IRESP)
      ELSE IF (TPFILE%CFORMAT=='LFICDF4') THEN
        CALL IO_Field_read_nc4(TPFILE,TPFIELD,PFIELD,IRESP)
      END IF
  ELSE
    IF (ISP == TPFILE%NMASTER_RANK)  THEN
      CALL ALLOCBUFFER_ll(ZFIELDP,PFIELD,TPFIELD%CDIR,GALLOC, KIMAX_ll, KJMAX_ll)
      IF (TPFILE%CFORMAT=='NETCDF4') THEN
         CALL IO_Field_read_nc4(TPFILE,TPFIELD,ZFIELDP,IRESP)
      ELSE IF (TPFILE%CFORMAT=='LFI') THEN
         CALL IO_Field_read_lfi(TPFILE,TPFIELD,ZFIELDP,IRESP)
      ELSE IF (TPFILE%CFORMAT=='LFICDF4') THEN
        CALL IO_Field_read_nc4(TPFILE,TPFIELD,ZFIELDP,IRESP)
      END IF
    ELSE
      !Not really necessary but useful to suppress alerts with Valgrind
      ALLOCATE(ZFIELDP(0))
      GALLOC = .TRUE.
    END IF
    !
    CALL MPI_BCAST(IRESP,1,MPI_INTEGER,TPFILE%NMASTER_RANK-1,TPFILE%NMPICOMM,IERR)
    !
    !Broadcast header only if IRESP==-111
    !because metadata of field has been modified in IO_Field_read_xxx
    IF (IRESP==-111) CALL IO_Field_metadata_bcast(TPFILE,TPFIELD)
    !
    IF (TPFIELD%CDIR /= 'XX' .AND. TPFIELD%CDIR /='YY') THEN
      ! Broadcast Field
      CALL MPI_BCAST(PFIELD,SIZE(PFIELD),MNHREAL_MPI,TPFILE%NMASTER_RANK-1,TPFILE%NMPICOMM,IERR)
    ELSE
      !Scatter Field
      CALL SCATTER_XXFIELD(TPFIELD%CDIR,ZFIELDP,PFIELD,TPFILE%NMASTER_RANK,TPFILE%NMPICOMM,TPSPLITTING)
    END IF
  END IF
END IF
!
IF (GALLOC) DEALLOCATE (ZFIELDP)
!
IF (IRESP==-111) IRESP = 0 !-111 is not really an error (metadata has changed)
!
IF (PRESENT(KRESP)) KRESP = IRESP
!
END SUBROUTINE IO_Field_read_byfield_X1


SUBROUTINE IO_Field_read_byname_X2(TPFILE,HNAME,PFIELD,KRESP,KIMAX_ll,KJMAX_ll,TPSPLITTING)
!
USE MODD_IO,           ONLY: ISNPROC
USE MODD_STRUCTURE_ll, ONLY: ZONE_ll
!
!
TYPE(TFILEDATA),    INTENT(IN)    :: TPFILE
CHARACTER(LEN=*),   INTENT(IN)    :: HNAME    ! name of the field to write
REAL,DIMENSION(:,:),INTENT(INOUT) :: PFIELD   ! array containing the data field
INTEGER,OPTIONAL,   INTENT(OUT)   :: KRESP    ! return-code
INTEGER,OPTIONAL,   INTENT(IN)    :: KIMAX_ll
INTEGER,OPTIONAL,   INTENT(IN)    :: KJMAX_ll
TYPE(ZONE_ll),DIMENSION(ISNPROC),OPTIONAL,INTENT(IN) :: TPSPLITTING  ! splitting of the domain
!
INTEGER :: ID ! Index of the field
INTEGER :: IRESP ! return_code
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_read_byname_X2',TRIM(TPFILE%CNAME)//': reading '//TRIM(HNAME))
!
CALL FIND_FIELD_ID_FROM_MNHNAME(HNAME,ID,IRESP)
!
IF(IRESP==0) CALL IO_Field_read(TPFILE,TFIELDLIST(ID),PFIELD,IRESP,KIMAX_ll,KJMAX_ll,TPSPLITTING)
!
IF (PRESENT(KRESP)) KRESP = IRESP
!
END SUBROUTINE IO_Field_read_byname_X2

SUBROUTINE IO_Field_read_byfield_X2(TPFILE,TPFIELD,PFIELD,KRESP,KIMAX_ll,KJMAX_ll,TPSPLITTING)
!
USE MODD_IO,           ONLY: GSMONOPROC, ISP, ISNPROC, LPACK, L1D, L2D
USE MODD_STRUCTURE_ll, ONLY: ZONE_ll
USE MODD_TIMEZ,        ONLY: TIMEZ
!
USE MODE_ALLOCBUFFER_ll
#ifdef MNH_GA
USE MODE_GA
#endif
USE MODE_MNH_TIMING,   ONLY: SECOND_MNH2
USE MODE_SCATTER_ll
!
TYPE(TFILEDATA),           INTENT(IN)    :: TPFILE
TYPE(TFIELDDATA),          INTENT(INOUT) :: TPFIELD
REAL,DIMENSION(:,:),TARGET,INTENT(INOUT) :: PFIELD   ! array containing the data field
INTEGER, OPTIONAL,         INTENT(OUT)   :: KRESP    ! return-code
INTEGER, OPTIONAL,         INTENT(IN)    :: KIMAX_ll
INTEGER, OPTIONAL,         INTENT(IN)    :: KJMAX_ll
TYPE(ZONE_ll),DIMENSION(ISNPROC),OPTIONAL,INTENT(IN) :: TPSPLITTING  ! splitting of the domain
!
INTEGER                      :: IERR
REAL,DIMENSION(:,:),POINTER  :: ZFIELDP
LOGICAL                      :: GALLOC
INTEGER                      :: IRESP
INTEGER                      :: IHEXTOT
REAL(kind=MNHTIME), DIMENSION(2) :: T0, T1, T2
REAL(kind=MNHTIME), DIMENSION(2) :: T11, T22
#ifdef MNH_GA
REAL,DIMENSION(:,:),POINTER    :: ZFIELD_GA
#endif
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_read_byfield_X2',TRIM(TPFILE%CNAME)//': reading '//TRIM(TPFIELD%CMNHNAME))
!
CALL SECOND_MNH2(T11)
GALLOC = .FALSE.
IRESP = 0
ZFIELDP => NULL()
!
IHEXTOT = 2*JPHEXT+1
CALL IO_File_read_check(TPFILE,'IO_Field_read_byfield_X2',IRESP)
!
IF (IRESP==0) THEN
  IF (GSMONOPROC) THEN ! sequential execution
    IF (LPACK .AND. L1D .AND. SIZE(PFIELD,1)==IHEXTOT .AND. SIZE(PFIELD,2)==IHEXTOT) THEN
      ZFIELDP=>PFIELD(JPHEXT+1:JPHEXT+1,JPHEXT+1:JPHEXT+1)
    ELSE IF (LPACK .AND. L2D .AND. SIZE(PFIELD,2)==IHEXTOT) THEN
      ZFIELDP=>PFIELD(:,JPHEXT+1:JPHEXT+1)
    ELSE
      ZFIELDP=>PFIELD(:,:)
    END IF
    IF (TPFILE%CFORMAT=='NETCDF4') THEN
      CALL IO_Field_read_nc4(TPFILE,TPFIELD,ZFIELDP,IRESP)
    ELSE IF (TPFILE%CFORMAT=='LFI') THEN
      CALL IO_Field_read_lfi(TPFILE,TPFIELD,ZFIELDP,IRESP)
    ELSE IF (TPFILE%CFORMAT=='LFICDF4') THEN
      CALL IO_Field_read_nc4(TPFILE,TPFIELD,ZFIELDP,IRESP)
    END IF
    IF (LPACK .AND. L1D .AND. SIZE(PFIELD,1)==IHEXTOT .AND. SIZE(PFIELD,2)==IHEXTOT) THEN
      PFIELD(:,:)=SPREAD(SPREAD(PFIELD(JPHEXT+1,JPHEXT+1),DIM=1,NCOPIES=IHEXTOT),DIM=2,NCOPIES=IHEXTOT)
    ELSE IF (LPACK .AND. L2D .AND. SIZE(PFIELD,2)==IHEXTOT) THEN
      PFIELD(:,:)=SPREAD(PFIELD(:,JPHEXT+1),DIM=2,NCOPIES=IHEXTOT)
    END IF
  ELSE
    CALL SECOND_MNH2(T0)
    IF (ISP == TPFILE%NMASTER_RANK)  THEN
      ! I/O process case
      CALL ALLOCBUFFER_ll(ZFIELDP,PFIELD,TPFIELD%CDIR,GALLOC, KIMAX_ll, KJMAX_ll)
      IF (TPFILE%CFORMAT=='NETCDF4') THEN
         CALL IO_Field_read_nc4(TPFILE,TPFIELD,ZFIELDP,IRESP)
      ELSE IF (TPFILE%CFORMAT=='LFI') THEN
         CALL IO_Field_read_lfi(TPFILE,TPFIELD,ZFIELDP,IRESP)
      ELSE IF (TPFILE%CFORMAT=='LFICDF4') THEN
        CALL IO_Field_read_nc4(TPFILE,TPFIELD,ZFIELDP,IRESP)
      END IF
    ELSE
      !Not really necessary but useful to suppress alerts with Valgrind
      ALLOCATE(ZFIELDP(0,0))
      GALLOC = .TRUE.
    END IF
    CALL SECOND_MNH2(T1)
    TIMEZ%T_READ2D_READ=TIMEZ%T_READ2D_READ + T1 - T0
    !
    CALL MPI_BCAST(IRESP,1,MPI_INTEGER,TPFILE%NMASTER_RANK-1,TPFILE%NMPICOMM,IERR)
    !
    !Broadcast header only if IRESP==-111
    !because metadata of field has been modified in IO_Field_read_xxx
    IF (IRESP==-111) CALL IO_Field_metadata_bcast(TPFILE,TPFIELD)
    !
    IF (TPFIELD%CDIR == 'XX' .OR. TPFIELD%CDIR == 'YY') THEN
      ! XX or YY Scatter Field
      CALL SCATTER_XXFIELD(TPFIELD%CDIR,ZFIELDP,PFIELD,TPFILE%NMASTER_RANK,TPFILE%NMPICOMM,TPSPLITTING)
    ELSE IF (TPFIELD%CDIR == 'XY') THEN
      IF (LPACK .AND. L2D) THEN
        ! 2D compact case
        CALL SCATTER_XXFIELD('XX',ZFIELDP(:,1),PFIELD(:,JPHEXT+1),TPFILE%NMASTER_RANK,TPFILE%NMPICOMM,TPSPLITTING)
        PFIELD(:,:) = SPREAD(PFIELD(:,JPHEXT+1),DIM=2,NCOPIES=IHEXTOT)
      ELSE
#ifdef MNH_GA
        !
        ! init/create the ga , dim3 = 1
        !
        CALL MNH_INIT_GA(SIZE(PFIELD,1),SIZE(PFIELD,2),1,TPFIELD%CMNHNAME,"READ")
        IF (ISP == TPFILE%NMASTER_RANK)  THEN
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
        CALL SCATTER_XYFIELD(ZFIELDP,PFIELD,TPFILE%NMASTER_RANK,TPFILE%NMPICOMM)
#endif
      END IF
    ELSE
      CALL MPI_BCAST(PFIELD,SIZE(PFIELD),MNHREAL_MPI,TPFILE%NMASTER_RANK-1,TPFILE%NMPICOMM,IERR)
    END IF
    CALL SECOND_MNH2(T2)
    TIMEZ%T_READ2D_SCAT=TIMEZ%T_READ2D_SCAT + T2 - T1
  END IF
END IF
!
IF (GALLOC) DEALLOCATE (ZFIELDP)
!
IF (IRESP==-111) IRESP = 0 !-111 is not really an error (metadata has changed)
!
IF (PRESENT(KRESP)) KRESP = IRESP
!
CALL SECOND_MNH2(T22)
TIMEZ%T_READ2D_ALL=TIMEZ%T_READ2D_ALL + T22 - T11
!
END SUBROUTINE IO_Field_read_byfield_X2


SUBROUTINE IO_Field_read_byname_X3(TPFILE,HNAME,PFIELD,KRESP)
!
TYPE(TFILEDATA),      INTENT(IN)    :: TPFILE
CHARACTER(LEN=*),     INTENT(IN)    :: HNAME    ! name of the field to write
REAL,DIMENSION(:,:,:),INTENT(INOUT) :: PFIELD   ! array containing the data field
INTEGER,OPTIONAL,     INTENT(OUT)   :: KRESP    ! return-code
!
INTEGER :: ID ! Index of the field
INTEGER :: IRESP ! return_code
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_read_byname_X3',TRIM(TPFILE%CNAME)//': reading '//TRIM(HNAME))
!
CALL FIND_FIELD_ID_FROM_MNHNAME(HNAME,ID,IRESP)
!
IF(IRESP==0) CALL IO_Field_read(TPFILE,TFIELDLIST(ID),PFIELD,IRESP)
!
IF (PRESENT(KRESP)) KRESP = IRESP
!
END SUBROUTINE IO_Field_read_byname_X3

SUBROUTINE IO_Field_read_byfield_X3(TPFILE,TPFIELD,PFIELD,KRESP)
!
USE MODD_IO,               ONLY: GSMONOPROC, ISP, ISNPROC, LPACK, L1D, L2D
USE MODD_TIMEZ,            ONLY: TIMEZ
USE MODD_VAR_ll,           ONLY: MNH_STATUSES_IGNORE
!
USE MODE_ALLOCBUFFER_ll
#ifdef MNH_GA
USE MODE_GA
#endif
USE MODE_IO_TOOLS,         ONLY: IO_Level2filenumber_get
USE MODE_IO_MANAGE_STRUCT, ONLY: IO_File_find_byname
USE MODE_MNH_TIMING,       ONLY: SECOND_MNH2
USE MODE_SCATTER_ll
!
TYPE(TFILEDATA),TARGET,      INTENT(IN)    :: TPFILE
TYPE(TFIELDDATA),            INTENT(INOUT) :: TPFIELD
REAL,DIMENSION(:,:,:),TARGET,INTENT(INOUT) :: PFIELD   ! array containing the data field
INTEGER, OPTIONAL,           INTENT(OUT)   :: KRESP    ! return-code
!
TYPE TX_2DP
   REAL,DIMENSION(:,:), POINTER :: X
END TYPE TX_2DP
!
INTEGER                               :: IERR,IRESP,IRESP_TMP
INTEGER                               :: IHEXTOT
INTEGER                               :: IK_FILE,IK_RANK,INB_PROC_REAL,JK_MAX
INTEGER                               :: JI,IXO,IXE,IYO,IYE
INTEGER                               :: JK,JKK
INTEGER                               :: NB_REQ
INTEGER,ALLOCATABLE,DIMENSION(:)      :: REQ_TAB
INTEGER, DIMENSION(MPI_STATUS_SIZE)   :: STATUS
LOGICAL                               :: GALLOC, GALLOC_ll
REAL,DIMENSION(:,:),POINTER           :: TX2DP
REAL,DIMENSION(:,:),POINTER           :: ZSLICE_ll,ZSLICE
REAL,DIMENSION(:,:,:),POINTER         :: ZFIELDP
REAL(kind=MNHTIME), DIMENSION(2)      :: T0, T1, T2
REAL(kind=MNHTIME), DIMENSION(2)      :: T11, T22
CHARACTER(LEN=2)                      :: YDIR
CHARACTER(LEN=4)                      :: YK
CHARACTER(LEN=NMNHNAMELGTMAX+4)       :: YRECZSLICE
CHARACTER(LEN=4)                      :: YSUFFIX
TYPE(TFILEDATA),POINTER               :: TZFILE
TYPE(TFIELDDATA)                      :: TZFIELD
TYPE(TX_2DP),ALLOCATABLE,DIMENSION(:) :: T_TX2DP
#ifdef MNH_GA
REAL,DIMENSION(:,:,:),POINTER              :: ZFIELD_GA
#endif
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_read_byfield_X3',TRIM(TPFILE%CNAME)//': reading '//TRIM(TPFIELD%CMNHNAME))
!
CALL SECOND_MNH2(T11)
!
TZFILE => NULL()
GALLOC    = .FALSE.
GALLOC_ll = .FALSE.
IRESP = 0
ZFIELDP => NULL()
YDIR = TPFIELD%CDIR
!
IHEXTOT = 2*JPHEXT+1
!
CALL IO_File_read_check(TPFILE,'IO_Field_read_byfield_X3',IRESP)
!
IF (IRESP==0) THEN
  IF (GSMONOPROC  .AND. TPFILE%NSUBFILES_IOZ==0 ) THEN ! sequential execution
    IF (LPACK .AND. L1D .AND. SIZE(PFIELD,1)==IHEXTOT .AND. SIZE(PFIELD,2)==IHEXTOT) THEN
      ZFIELDP=>PFIELD(JPHEXT+1:JPHEXT+1,JPHEXT+1:JPHEXT+1,:)
    ELSE IF (LPACK .AND. L2D .AND. SIZE(PFIELD,2)==IHEXTOT) THEN
      ALLOCATE (ZFIELDP(SIZE(PFIELD,1),1,SIZE(PFIELD,3)))
      GALLOC = .TRUE.
    ELSE
      ZFIELDP=>PFIELD(:,:,:)
    END IF
    IF (TPFILE%CFORMAT=='NETCDF4') THEN
      CALL IO_Field_read_nc4(TPFILE,TPFIELD,ZFIELDP,IRESP)
    ELSE IF (TPFILE%CFORMAT=='LFI') THEN
      CALL IO_Field_read_lfi(TPFILE,TPFIELD,ZFIELDP,IRESP)
    ELSE IF (TPFILE%CFORMAT=='LFICDF4') THEN
      CALL IO_Field_read_nc4(TPFILE,TPFIELD,ZFIELDP,IRESP)
    END IF
    IF (LPACK .AND. L1D .AND. SIZE(PFIELD,1)==IHEXTOT .AND. SIZE(PFIELD,2)==IHEXTOT) THEN
      PFIELD(:,:,:)=SPREAD(SPREAD(PFIELD(JPHEXT+1,JPHEXT+1,:),DIM=1,NCOPIES=IHEXTOT),DIM=2,NCOPIES=IHEXTOT)
    ELSE IF (LPACK .AND. L2D .AND. SIZE(PFIELD,2)==IHEXTOT) THEN
      PFIELD(:,:,:)=SPREAD(ZFIELDP(:,1,:),DIM=2,NCOPIES=IHEXTOT)
    END IF
  ELSE IF ( TPFILE%NSUBFILES_IOZ==0 .OR. YDIR == '--' ) THEN ! multiprocesses execution & 1 IO proc
    IF (ISP == TPFILE%NMASTER_RANK)  THEN
      ! I/O process case
      CALL ALLOCBUFFER_ll(ZFIELDP,PFIELD,YDIR,GALLOC)
      IF (TPFILE%CFORMAT=='NETCDF4') THEN
        CALL IO_Field_read_nc4(TPFILE,TPFIELD,ZFIELDP,IRESP)
      ELSE IF (TPFILE%CFORMAT=='LFI') THEN
        CALL IO_Field_read_lfi(TPFILE,TPFIELD,ZFIELDP,IRESP)
      ELSE IF (TPFILE%CFORMAT=='LFICDF4') THEN
        CALL IO_Field_read_nc4(TPFILE,TPFIELD,ZFIELDP,IRESP)
      END IF
    ELSE
      !Not really necessary but useful to suppress alerts with Valgrind
      ALLOCATE(ZFIELDP(0,0,0))
      GALLOC = .TRUE.
    END IF
    !
    CALL MPI_BCAST(IRESP,1,MPI_INTEGER,TPFILE%NMASTER_RANK-1,TPFILE%NMPICOMM,IERR)
    !
    !Broadcast header only if IRESP==-111
    !because metadata of field has been modified in IO_Field_read_xxx
    IF (IRESP==-111) CALL IO_Field_metadata_bcast(TPFILE,TPFIELD)
    !
    IF (YDIR == 'XX' .OR. YDIR =='YY') THEN
      ! XX or YY Scatter Field
      CALL SCATTER_XXFIELD(YDIR,ZFIELDP,PFIELD,TPFILE%NMASTER_RANK,TPFILE%NMPICOMM)
    ELSE IF (YDIR == 'XY') THEN
      IF (LPACK .AND. L2D) THEN
        ! 2D compact case
        CALL SCATTER_XXFIELD('XX',ZFIELDP(:,1,:),PFIELD(:,JPHEXT+1,:),TPFILE%NMASTER_RANK,TPFILE%NMPICOMM)
        PFIELD(:,:,:) = SPREAD(PFIELD(:,JPHEXT+1,:),DIM=2,NCOPIES=IHEXTOT)
      ELSE
        ! XY Scatter Field
        CALL SCATTER_XYFIELD(ZFIELDP,PFIELD,TPFILE%NMASTER_RANK,TPFILE%NMPICOMM)
      END IF
    ELSE
      ! Broadcast Field
      CALL MPI_BCAST(PFIELD,SIZE(PFIELD),MNHREAL_MPI,TPFILE%NMASTER_RANK-1,TPFILE%NMPICOMM,IERR)
    END IF
  ELSE  ! multiprocesses execution & // IO
!
!JUAN BG Z SLICE
!
#ifdef MNH_GA
    !
    ! init/create the ga
    !
    CALL SECOND_MNH2(T0)
    CALL MNH_INIT_GA(SIZE(PFIELD,1),SIZE(PFIELD,2),SIZE(PFIELD,3),TPFIELD%CMNHNAME,"READ")
    !
    ! read the data
    !
    ALLOCATE(ZSLICE_ll(0,0)) ! to avoid bug on test of size
    GALLOC_ll = .TRUE.
    DO JKK=1,IKU_ll
      IK_FILE = IO_Level2filenumber_get(JKK,TPFILE%NSUBFILES_IOZ)
      TZFILE => TPFILE%TFILES_IOZ(IK_FILE+1)%TFILE
      TZFIELD = TPFIELD
      WRITE(YSUFFIX,'(I4.4)') JKK
      TZFIELD%CMNHNAME = TRIM(TPFIELD%CMNHNAME)//TRIM(YSUFFIX)
      IF (LEN_TRIM(TZFIELD%CSTDNAME)>0)  TZFIELD%CSTDNAME  = TRIM(TZFIELD%CSTDNAME)//'_at_level_'//YSUFFIX
      IF (LEN_TRIM(TZFIELD%CLONGNAME)>0) TZFIELD%CLONGNAME = TRIM(TZFIELD%CLONGNAME)//' at level '//YSUFFIX
      TZFIELD%NDIMS = 2
      !
      IK_RANK = TZFILE%NMASTER_RANK
      !
      IF (ISP == IK_RANK )  THEN
        IF ( SIZE(ZSLICE_ll) .EQ. 0 ) THEN
          DEALLOCATE(ZSLICE_ll)
          CALL ALLOCBUFFER_ll(ZSLICE_ll,ZSLICE,YDIR,GALLOC_ll)
        END IF
        !
        CALL SECOND_MNH2(T0)
        WRITE(YK,'(I4.4)')  JKK
        YRECZSLICE = TRIM(TPFIELD%CMNHNAME)//YK
        IF (TPFILE%CFORMAT=='NETCDF4') THEN
          CALL IO_Field_read_nc4(TZFILE,TZFIELD,ZSLICE_ll,IRESP_TMP)
        ELSE IF (TPFILE%CFORMAT=='LFI') THEN
          CALL IO_Field_read_lfi(TZFILE,TZFIELD,ZSLICE_ll,IRESP_TMP)
        ELSE IF (TPFILE%CFORMAT=='LFICDF4') THEN
          CALL IO_Field_read_nc4(TZFILE,TZFIELD,ZSLICE_ll,IRESP_TMP)
        END IF
        CALL SECOND_MNH2(T1)
        TIMEZ%T_READ3D_READ=TIMEZ%T_READ3D_READ + T1 - T0
        !
        ! put the data in the g_a , this proc get this JKK slide
        !
        LO_ZPLAN(JPIZ) = JKK
        HI_ZPLAN(JPIZ) = JKK
        CALL NGA_PUT(G_A, LO_ZPLAN, HI_ZPLAN,ZSLICE_LL, LD_ZPLAN)
      END IF
      TZFILE => NULL()
    END DO
    CALL GA_SYNC
    !
    CALL MPI_BCAST(IRESP_TMP,1,MPI_INTEGER,IK_RANK-1,TZFILE%NMPICOMM,IERR)
    IF (IRESP_TMP/=0) IRESP = IRESP_TMP !Keep last "error"
    !
    ! get the columun data in this proc
    !
    ! temp buf to avoid problem with none stride PFIELDS buffer  with HALO
    ALLOCATE (ZFIELD_GA (SIZE(PFIELD,1),SIZE(PFIELD,2),SIZE(PFIELD,3)))
    CALL NGA_GET(G_A, LO_COL, HI_COL,ZFIELD_GA(1,1,1) , LD_COL)
    PFIELD = ZFIELD_GA
    DEALLOCATE(ZFIELD_GA)
#else
    ALLOCATE(ZSLICE_ll(0,0))
    GALLOC_ll = .TRUE.
    INB_PROC_REAL = MIN(TPFILE%NSUBFILES_IOZ,ISNPROC)
    ALLOCATE(REQ_TAB((ISNPROC-1)*INB_PROC_REAL))
    ALLOCATE(T_TX2DP((ISNPROC-1)*INB_PROC_REAL))
    Z_SLICE: DO JK=1,SIZE(PFIELD,3),INB_PROC_REAL
      !
      ! read the data
      !
      JK_MAX=MIN(SIZE(PFIELD,3),JK+INB_PROC_REAL-1)
      !
      NB_REQ=0
      DO JKK=JK,JK_MAX
        IF (TPFILE%NSUBFILES_IOZ .GT. 1 ) THEN
          IK_FILE = IO_Level2filenumber_get(JKK,TPFILE%NSUBFILES_IOZ)
          TZFILE => TPFILE%TFILES_IOZ(IK_FILE+1)%TFILE
          TZFIELD = TPFIELD
          WRITE(YSUFFIX,'(I4.4)') JKK
          TZFIELD%CMNHNAME = TRIM(TPFIELD%CMNHNAME)//TRIM(YSUFFIX)
          IF (LEN_TRIM(TZFIELD%CSTDNAME)>0)  TZFIELD%CSTDNAME  = TRIM(TZFIELD%CSTDNAME)//'_at_level_'//YSUFFIX
          IF (LEN_TRIM(TZFIELD%CLONGNAME)>0) TZFIELD%CLONGNAME = TRIM(TZFIELD%CLONGNAME)//' at level '//YSUFFIX
          TZFIELD%NDIMS = 2
        ELSE
          TZFILE => TPFILE
          TZFIELD = TPFIELD
        END IF
        IK_RANK = TZFILE%NMASTER_RANK
        IF (ISP == IK_RANK )  THEN
          IF ( SIZE(ZSLICE_ll) .EQ. 0 ) THEN
            DEALLOCATE(ZSLICE_ll)
            CALL ALLOCBUFFER_ll(ZSLICE_ll,ZSLICE,YDIR,GALLOC_ll)
          END IF
          CALL SECOND_MNH2(T0)
          WRITE(YK,'(I4.4)')  JKK
          YRECZSLICE = TRIM(TPFIELD%CMNHNAME)//YK
          IF (TZFILE%CFORMAT=='NETCDF4') THEN
            CALL IO_Field_read_nc4(TZFILE,TZFIELD,ZSLICE_ll,IRESP_TMP)
          ELSE IF (TZFILE%CFORMAT=='LFI') THEN
            CALL IO_Field_read_lfi(TZFILE,TZFIELD,ZSLICE_ll,IRESP_TMP)
          ELSE IF (TZFILE%CFORMAT=='LFICDF4') THEN
            CALL IO_Field_read_nc4(TZFILE,TZFIELD,ZSLICE_ll,IRESP_TMP)
          END IF
          CALL SECOND_MNH2(T1)
          TIMEZ%T_READ3D_READ=TIMEZ%T_READ3D_READ + T1 - T0
          DO JI = 1,ISNPROC
            CALL GET_DOMREAD_ll(JI,IXO,IXE,IYO,IYE)
            TX2DP=>ZSLICE_ll(IXO:IXE,IYO:IYE)
            IF (ISP /= JI) THEN
              NB_REQ = NB_REQ + 1
              ALLOCATE(T_TX2DP(NB_REQ)%X(IXO:IXE,IYO:IYE))
              T_TX2DP(NB_REQ)%X=TX2DP
              CALL MPI_ISEND(T_TX2DP(NB_REQ)%X,SIZE(TX2DP),MNHREAL_MPI,JI-1,199+IK_RANK, &
                             TZFILE%NMPICOMM,REQ_TAB(NB_REQ),IERR)
              !CALL MPI_BSEND(TX2DP,SIZE(TX2DP),MNHREAL_MPI,JI-1,199+IK_RANK,TZFILE%NMPICOMM,IERR)
            ELSE
              PFIELD(:,:,JKK) = TX2DP(:,:)
            END IF
          END DO
          CALL SECOND_MNH2(T2)
          TIMEZ%T_READ3D_SEND=TIMEZ%T_READ3D_SEND + T2 - T1
        END IF
        !
        CALL MPI_BCAST(IRESP_TMP,1,MPI_INTEGER,IK_RANK-1,TZFILE%NMPICOMM,IERR)
        IF (IRESP_TMP/=0) IRESP = IRESP_TMP !Keep last "error"
        TZFILE => NULL()
      END DO
      !
      ! broadcast the data
      !
      IF (YDIR == 'XX' .OR. YDIR =='YY') THEN
        ! XX or YY Scatter Field
        call Print_msg( NVERB_FATAL, 'GEN', 'IO_Field_read_byfield_X3', 'XX or YY not yet planned on Blue Gene' )
        CALL SCATTER_XXFIELD(YDIR,ZFIELDP,PFIELD,TPFILE%NMASTER_RANK,TPFILE%NMPICOMM)
      ELSE IF (YDIR == 'XY') THEN
        IF (LPACK .AND. L2D) THEN
          ! 2D compact case
          call Print_msg( NVERB_FATAL, 'GEN', 'IO_Field_read_byfield_X3', 'L2D not yet planned on Blue Gene' )
          CALL SCATTER_XXFIELD('XX',ZFIELDP(:,1,:),PFIELD(:,JPHEXT+1,:),TPFILE%NMASTER_RANK,TPFILE%NMPICOMM)
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
            IF (TPFILE%NSUBFILES_IOZ .GT. 1 ) THEN
               IK_FILE = IO_Level2filenumber_get(JKK,TPFILE%NSUBFILES_IOZ)
               TZFILE => TPFILE%TFILES_IOZ(IK_FILE+1)%TFILE
            ELSE
              TZFILE => TPFILE
            END IF
            !
            IK_RANK = TZFILE%NMASTER_RANK
            !
            ZSLICE => PFIELD(:,:,JKK)
            !CALL SCATTER_XYFIELD(ZSLICE_ll,ZSLICE,TZFILE%NMASTER_RANK,TZFILE%NMPICOMM)
            IF (ISP .NE. IK_RANK) THEN
              CALL MPI_RECV(ZSLICE,SIZE(ZSLICE),MNHREAL_MPI,IK_RANK-1,199+IK_RANK, &
                            TZFILE%NMPICOMM,STATUS,IERR)
            END IF
            TZFILE => NULL()
          END DO
          CALL SECOND_MNH2(T1)
          TIMEZ%T_READ3D_RECV=TIMEZ%T_READ3D_RECV + T1 - T0
        END IF
      ELSE
        ! Broadcast Field
        call Print_msg( NVERB_FATAL, 'GEN', 'IO_Field_read_byfield_X3', 'broadcast field not yet planned on Blue Gene' )
        CALL MPI_BCAST(PFIELD,SIZE(PFIELD),MNHREAL_MPI,TPFILE%NMASTER_RANK-1,TPFILE%NMPICOMM,IERR)
      END IF
      CALL SECOND_MNH2(T0)
      IF (NB_REQ .GT.0 ) THEN
        CALL MPI_WAITALL(NB_REQ,REQ_TAB,MNH_STATUSES_IGNORE,IERR)
        DO JI=1,NB_REQ ;  DEALLOCATE(T_TX2DP(JI)%X) ; ENDDO
      END IF
      CALL SECOND_MNH2(T1)
      TIMEZ%T_READ3D_WAIT=TIMEZ%T_READ3D_WAIT + T1 - T0
    END DO Z_SLICE
    !
    DEALLOCATE(T_TX2DP)
    DEALLOCATE(REQ_TAB)
    !
    !Broadcast header only if IRESP==-111
    !because metadata of field has been modified in IO_Field_read_xxx
    IF (IRESP==-111) CALL IO_Field_metadata_bcast(TPFILE,TPFIELD)
    !
#endif
!JUAN BG Z SLICE
  END IF !(GSMONOPROC)
END IF
!
IF (GALLOC)    DEALLOCATE (ZFIELDP)
IF (GALLOC_ll) DEALLOCATE (ZSLICE_ll)
!
IF (IRESP==-111) IRESP = 0 !-111 is not really an error (metadata has changed)
!
IF (PRESENT(KRESP)) KRESP = IRESP
!
CALL MPI_BARRIER(TPFILE%NMPICOMM,IERR)
CALL SECOND_MNH2(T22)
TIMEZ%T_READ3D_ALL=TIMEZ%T_READ3D_ALL + T22 - T11
!
END SUBROUTINE IO_Field_read_byfield_X3


SUBROUTINE IO_Field_read_byname_X4(TPFILE,HNAME,PFIELD,KRESP)
!
USE MODD_IO,           ONLY: ISNPROC
USE MODD_STRUCTURE_ll, ONLY: ZONE_ll
!
!
TYPE(TFILEDATA),        INTENT(IN)    :: TPFILE
CHARACTER(LEN=*),       INTENT(IN)    :: HNAME    ! name of the field to write
REAL,DIMENSION(:,:,:,:),INTENT(INOUT) :: PFIELD   ! array containing the data field
INTEGER,OPTIONAL,       INTENT(OUT)   :: KRESP    ! return-code
!
INTEGER :: ID ! Index of the field
INTEGER :: IRESP ! return_code
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_read_byname_X4',TRIM(TPFILE%CNAME)//': reading '//TRIM(HNAME))
!
CALL FIND_FIELD_ID_FROM_MNHNAME(HNAME,ID,IRESP)
!
IF(IRESP==0) CALL IO_Field_read(TPFILE,TFIELDLIST(ID),PFIELD,IRESP)
!
IF (PRESENT(KRESP)) KRESP = IRESP
!
END SUBROUTINE IO_Field_read_byname_X4

SUBROUTINE IO_Field_read_byfield_X4(TPFILE,TPFIELD,PFIELD,KRESP)
!
USE MODD_IO,             ONLY: GSMONOPROC, ISP, ISNPROC, LPACK, L1D, L2D
USE MODD_STRUCTURE_ll,   ONLY: ZONE_ll
USE MODD_TIMEZ,          ONLY: TIMEZ
!
USE MODE_ALLOCBUFFER_ll
USE MODE_MNH_TIMING,     ONLY: SECOND_MNH2
USE MODE_SCATTER_ll
!
TYPE(TFILEDATA),               INTENT(IN)    :: TPFILE
TYPE(TFIELDDATA),              INTENT(INOUT) :: TPFIELD
REAL,DIMENSION(:,:,:,:),TARGET,INTENT(INOUT) :: PFIELD   ! array containing the data field
INTEGER, OPTIONAL,             INTENT(OUT)   :: KRESP    ! return-code
!
INTEGER                          :: IERR
REAL,DIMENSION(:,:,:,:),POINTER  :: ZFIELDP
LOGICAL                          :: GALLOC
INTEGER                          :: IRESP
INTEGER                          :: IHEXTOT
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_read_byfield_X4',TRIM(TPFILE%CNAME)//': reading '//TRIM(TPFIELD%CMNHNAME))
!
GALLOC = .FALSE.
IRESP = 0
ZFIELDP => NULL()
!
IHEXTOT = 2*JPHEXT+1
CALL IO_File_read_check(TPFILE,'IO_Field_read_byfield_X4',IRESP)
!
IF (IRESP==0) THEN
  IF (GSMONOPROC) THEN ! sequential execution
    IF (LPACK .AND. L1D .AND. SIZE(PFIELD,1)==IHEXTOT .AND. SIZE(PFIELD,2)==IHEXTOT) THEN
      ZFIELDP=>PFIELD(JPHEXT+1:JPHEXT+1,JPHEXT+1:JPHEXT+1,:,:)
    ELSE IF (LPACK .AND. L2D .AND. SIZE(PFIELD,2)==IHEXTOT) THEN
      ZFIELDP=>PFIELD(:,JPHEXT+1:JPHEXT+1,:,:)
    ELSE
      ZFIELDP=>PFIELD(:,:,:,:)
    END IF
    IF (TPFILE%CFORMAT=='NETCDF4') THEN
      CALL IO_Field_read_nc4(TPFILE,TPFIELD,ZFIELDP,IRESP)
    ELSE IF (TPFILE%CFORMAT=='LFI') THEN
      CALL IO_Field_read_lfi(TPFILE,TPFIELD,ZFIELDP,IRESP)
    ELSE IF (TPFILE%CFORMAT=='LFICDF4') THEN
      CALL IO_Field_read_nc4(TPFILE,TPFIELD,ZFIELDP,IRESP)
    END IF
    IF (LPACK .AND. L1D .AND. SIZE(PFIELD,1)==IHEXTOT .AND. SIZE(PFIELD,2)==IHEXTOT) THEN
      PFIELD(:,:,:,:)=SPREAD(SPREAD(PFIELD(JPHEXT+1,JPHEXT+1,:,:),DIM=1,NCOPIES=IHEXTOT),DIM=2,NCOPIES=IHEXTOT)
    ELSE IF (LPACK .AND. L2D .AND. SIZE(PFIELD,2)==IHEXTOT) THEN
      PFIELD(:,:,:,:)=SPREAD(PFIELD(:,JPHEXT+1,:,:),DIM=2,NCOPIES=IHEXTOT)
    END IF
  ELSE
    IF (ISP == TPFILE%NMASTER_RANK)  THEN
      ! I/O process case
      CALL ALLOCBUFFER_ll(ZFIELDP,PFIELD,TPFIELD%CDIR,GALLOC)
      IF (TPFILE%CFORMAT=='NETCDF4') THEN
         CALL IO_Field_read_nc4(TPFILE,TPFIELD,ZFIELDP,IRESP)
      ELSE IF (TPFILE%CFORMAT=='LFI') THEN
         CALL IO_Field_read_lfi(TPFILE,TPFIELD,ZFIELDP,IRESP)
      ELSE IF (TPFILE%CFORMAT=='LFICDF4') THEN
        CALL IO_Field_read_nc4(TPFILE,TPFIELD,ZFIELDP,IRESP)
      END IF
    ELSE
      !Not really necessary but useful to suppress alerts with Valgrind
      ALLOCATE(ZFIELDP(0,0,0,0))
      GALLOC = .TRUE.
    END IF
    !
    CALL MPI_BCAST(IRESP,1,MPI_INTEGER,TPFILE%NMASTER_RANK-1,TPFILE%NMPICOMM,IERR)
    !
    !Broadcast header only if IRESP==-111
    !because metadata of field has been modified in IO_Field_read_xxx
    IF (IRESP==-111) CALL IO_Field_metadata_bcast(TPFILE,TPFIELD)
    !
    IF (TPFIELD%CDIR == 'XX' .OR. TPFIELD%CDIR == 'YY') THEN
      ! XX or YY Scatter Field
      CALL SCATTER_XXFIELD(TPFIELD%CDIR,ZFIELDP,PFIELD,TPFILE%NMASTER_RANK,TPFILE%NMPICOMM)
    ELSE IF (TPFIELD%CDIR == 'XY') THEN
      IF (LPACK .AND. L2D) THEN
        ! 2D compact case
        CALL SCATTER_XXFIELD('XX',ZFIELDP(:,1,:,:),PFIELD(:,JPHEXT+1,:,:),TPFILE%NMASTER_RANK,TPFILE%NMPICOMM)
        PFIELD(:,:,:,:) = SPREAD(PFIELD(:,JPHEXT+1,:,:),DIM=2,NCOPIES=IHEXTOT)
      ELSE
        ! XY Scatter Field
        CALL SCATTER_XYFIELD(ZFIELDP,PFIELD,TPFILE%NMASTER_RANK,TPFILE%NMPICOMM)
      END IF
    ELSE
      CALL MPI_BCAST(PFIELD,SIZE(PFIELD),MNHREAL_MPI,TPFILE%NMASTER_RANK-1,TPFILE%NMPICOMM,IERR)
    END IF
  END IF
END IF
!
IF (GALLOC) DEALLOCATE (ZFIELDP)
!
IF (IRESP==-111) IRESP = 0 !-111 is not really an error (metadata has changed)
!
IF (PRESENT(KRESP)) KRESP = IRESP
!
END SUBROUTINE IO_Field_read_byfield_X4


SUBROUTINE IO_Field_read_byname_X5(TPFILE,HNAME,PFIELD,KRESP)
!
USE MODD_IO,           ONLY: ISNPROC
USE MODD_STRUCTURE_ll, ONLY: ZONE_ll
!
!
TYPE(TFILEDATA),          INTENT(IN)    :: TPFILE
CHARACTER(LEN=*),         INTENT(IN)    :: HNAME    ! name of the field to write
REAL,DIMENSION(:,:,:,:,:),INTENT(INOUT) :: PFIELD   ! array containing the data field
INTEGER,OPTIONAL,         INTENT(OUT)   :: KRESP    ! return-code
!
INTEGER :: ID ! Index of the field
INTEGER :: IRESP ! return_code
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_read_byname_X5',TRIM(TPFILE%CNAME)//': reading '//TRIM(HNAME))
!
CALL FIND_FIELD_ID_FROM_MNHNAME(HNAME,ID,IRESP)
!
IF(IRESP==0) CALL IO_Field_read(TPFILE,TFIELDLIST(ID),PFIELD,IRESP)
!
IF (PRESENT(KRESP)) KRESP = IRESP
!
END SUBROUTINE IO_Field_read_byname_X5

SUBROUTINE IO_Field_read_byfield_X5(TPFILE,TPFIELD,PFIELD,KRESP)
!
USE MODD_IO,             ONLY: GSMONOPROC, ISP, ISNPROC, LPACK, L1D, L2D
USE MODD_STRUCTURE_ll,   ONLY: ZONE_ll
USE MODD_TIMEZ,          ONLY: TIMEZ
!
USE MODE_ALLOCBUFFER_ll
USE MODE_MNH_TIMING,     ONLY: SECOND_MNH2
USE MODE_SCATTER_ll
!
TYPE(TFILEDATA),                 INTENT(IN)    :: TPFILE
TYPE(TFIELDDATA),                INTENT(INOUT) :: TPFIELD
REAL,DIMENSION(:,:,:,:,:),TARGET,INTENT(INOUT) :: PFIELD   ! array containing the data field
INTEGER, OPTIONAL,               INTENT(OUT)   :: KRESP    ! return-code
!
INTEGER                            :: IERR
REAL,DIMENSION(:,:,:,:,:),POINTER  :: ZFIELDP
LOGICAL                            :: GALLOC
INTEGER                            :: IRESP
INTEGER                            :: IHEXTOT
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_read_byfield_X5',TRIM(TPFILE%CNAME)//': reading '//TRIM(TPFIELD%CMNHNAME))
!
GALLOC = .FALSE.
IRESP = 0
ZFIELDP => NULL()
!
IHEXTOT = 2*JPHEXT+1
CALL IO_File_read_check(TPFILE,'IO_Field_read_byfield_X5',IRESP)
!
IF (IRESP==0) THEN
  IF (GSMONOPROC) THEN ! sequential execution
    IF (LPACK .AND. L1D .AND. SIZE(PFIELD,1)==IHEXTOT .AND. SIZE(PFIELD,2)==IHEXTOT) THEN
      ZFIELDP=>PFIELD(JPHEXT+1:JPHEXT+1,JPHEXT+1:JPHEXT+1,:,:,:)
    ELSE IF (LPACK .AND. L2D .AND. SIZE(PFIELD,2)==IHEXTOT) THEN
      ZFIELDP=>PFIELD(:,JPHEXT+1:JPHEXT+1,:,:,:)
    ELSE
      ZFIELDP=>PFIELD(:,:,:,:,:)
    END IF
    IF (TPFILE%CFORMAT=='NETCDF4') THEN
      CALL IO_Field_read_nc4(TPFILE,TPFIELD,ZFIELDP,IRESP)
    ELSE IF (TPFILE%CFORMAT=='LFI') THEN
      CALL IO_Field_read_lfi(TPFILE,TPFIELD,ZFIELDP,IRESP)
    ELSE IF (TPFILE%CFORMAT=='LFICDF4') THEN
      CALL IO_Field_read_nc4(TPFILE,TPFIELD,ZFIELDP,IRESP)
    END IF
    IF (LPACK .AND. L1D .AND. SIZE(PFIELD,1)==IHEXTOT .AND. SIZE(PFIELD,2)==IHEXTOT) THEN
      PFIELD(:,:,:,:,:)=SPREAD(SPREAD(PFIELD(JPHEXT+1,JPHEXT+1,:,:,:),DIM=1,NCOPIES=IHEXTOT),DIM=2,NCOPIES=IHEXTOT)
    ELSE IF (LPACK .AND. L2D .AND. SIZE(PFIELD,2)==IHEXTOT) THEN
      PFIELD(:,:,:,:,:)=SPREAD(PFIELD(:,JPHEXT+1,:,:,:),DIM=2,NCOPIES=IHEXTOT)
    END IF
  ELSE
    IF (ISP == TPFILE%NMASTER_RANK)  THEN
      ! I/O process case
      CALL ALLOCBUFFER_ll(ZFIELDP,PFIELD,TPFIELD%CDIR,GALLOC)
      IF (TPFILE%CFORMAT=='NETCDF4') THEN
         CALL IO_Field_read_nc4(TPFILE,TPFIELD,ZFIELDP,IRESP)
      ELSE IF (TPFILE%CFORMAT=='LFI') THEN
         CALL IO_Field_read_lfi(TPFILE,TPFIELD,ZFIELDP,IRESP)
      ELSE IF (TPFILE%CFORMAT=='LFICDF4') THEN
        CALL IO_Field_read_nc4(TPFILE,TPFIELD,ZFIELDP,IRESP)
      END IF
    ELSE
      !Not really necessary but useful to suppress alerts with Valgrind
      ALLOCATE(ZFIELDP(0,0,0,0,0))
      GALLOC = .TRUE.
    END IF
    !
    CALL MPI_BCAST(IRESP,1,MPI_INTEGER,TPFILE%NMASTER_RANK-1,TPFILE%NMPICOMM,IERR)
    !
    !Broadcast header only if IRESP==-111
    !because metadata of field has been modified in IO_Field_read_xxx
    IF (IRESP==-111) CALL IO_Field_metadata_bcast(TPFILE,TPFIELD)
    !
    IF (TPFIELD%CDIR == 'XX' .OR. TPFIELD%CDIR == 'YY') THEN
      ! XX or YY Scatter Field
      CALL SCATTER_XXFIELD(TPFIELD%CDIR,ZFIELDP,PFIELD,TPFILE%NMASTER_RANK,TPFILE%NMPICOMM)
    ELSE IF (TPFIELD%CDIR == 'XY') THEN
      IF (LPACK .AND. L2D) THEN
        ! 2D compact case
        CALL SCATTER_XXFIELD('XX',ZFIELDP(:,1,:,:,:),PFIELD(:,JPHEXT+1,:,:,:),TPFILE%NMASTER_RANK,TPFILE%NMPICOMM)
        PFIELD(:,:,:,:,:) = SPREAD(PFIELD(:,JPHEXT+1,:,:,:),DIM=2,NCOPIES=IHEXTOT)
      ELSE
        ! XY Scatter Field
        CALL SCATTER_XYFIELD(ZFIELDP,PFIELD,TPFILE%NMASTER_RANK,TPFILE%NMPICOMM)
      END IF
    ELSE
      CALL MPI_BCAST(PFIELD,SIZE(PFIELD),MNHREAL_MPI,TPFILE%NMASTER_RANK-1,TPFILE%NMPICOMM,IERR)
    END IF
  END IF
END IF
!
IF (GALLOC) DEALLOCATE (ZFIELDP)
!
IF (IRESP==-111) IRESP = 0 !-111 is not really an error (metadata has changed)
!
IF (PRESENT(KRESP)) KRESP = IRESP
!
END SUBROUTINE IO_Field_read_byfield_X5


SUBROUTINE IO_Field_read_byname_X6(TPFILE,HNAME,PFIELD,KRESP)
!
USE MODD_IO,           ONLY: ISNPROC
USE MODD_STRUCTURE_ll, ONLY: ZONE_ll
!
!
TYPE(TFILEDATA),            INTENT(IN)    :: TPFILE
CHARACTER(LEN=*),           INTENT(IN)    :: HNAME    ! name of the field to write
REAL,DIMENSION(:,:,:,:,:,:),INTENT(INOUT) :: PFIELD   ! array containing the data field
INTEGER,OPTIONAL,           INTENT(OUT)   :: KRESP    ! return-code
!
INTEGER :: ID ! Index of the field
INTEGER :: IRESP ! return_code
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_read_byname_X6',TRIM(TPFILE%CNAME)//': reading '//TRIM(HNAME))
!
CALL FIND_FIELD_ID_FROM_MNHNAME(HNAME,ID,IRESP)
!
IF(IRESP==0) CALL IO_Field_read(TPFILE,TFIELDLIST(ID),PFIELD,IRESP)
!
IF (PRESENT(KRESP)) KRESP = IRESP
!
END SUBROUTINE IO_Field_read_byname_X6

SUBROUTINE IO_Field_read_byfield_X6(TPFILE,TPFIELD,PFIELD,KRESP)
!
USE MODD_IO,             ONLY: GSMONOPROC,ISP,ISNPROC,LPACK,L1D,L2D
USE MODD_STRUCTURE_ll,   ONLY: ZONE_ll
USE MODD_TIMEZ,          ONLY: TIMEZ
!
USE MODE_ALLOCBUFFER_ll
USE MODE_MNH_TIMING,     ONLY: SECOND_MNH2
USE MODE_SCATTER_ll
!
TYPE(TFILEDATA),                   INTENT(IN)    :: TPFILE
TYPE(TFIELDDATA),                  INTENT(INOUT) :: TPFIELD
REAL,DIMENSION(:,:,:,:,:,:),TARGET,INTENT(INOUT) :: PFIELD   ! array containing the data field
INTEGER, OPTIONAL,                 INTENT(OUT)   :: KRESP    ! return-code
!
INTEGER                              :: IERR
REAL,DIMENSION(:,:,:,:,:,:),POINTER  :: ZFIELDP
LOGICAL                              :: GALLOC
INTEGER                              :: IRESP
INTEGER                              :: IHEXTOT
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_read_byfield_X6',TRIM(TPFILE%CNAME)//': reading '//TRIM(TPFIELD%CMNHNAME))
!
GALLOC = .FALSE.
IRESP = 0
ZFIELDP => NULL()
!
IHEXTOT = 2*JPHEXT+1
CALL IO_File_read_check(TPFILE,'IO_Field_read_byfield_X6',IRESP)
!
IF (IRESP==0) THEN
  IF (GSMONOPROC) THEN ! sequential execution
    IF (TPFILE%CFORMAT=='NETCDF4') THEN
      CALL IO_Field_read_nc4(TPFILE,TPFIELD,PFIELD,IRESP)
    ELSE IF (TPFILE%CFORMAT=='LFI') THEN
      CALL IO_Field_read_lfi(TPFILE,TPFIELD,PFIELD,IRESP)
    ELSE IF (TPFILE%CFORMAT=='LFICDF4') THEN
      CALL IO_Field_read_nc4(TPFILE,TPFIELD,PFIELD,IRESP)
    END IF
  ELSE
    IF (ISP == TPFILE%NMASTER_RANK)  THEN
      ! I/O process case
      CALL ALLOCBUFFER_ll(ZFIELDP,PFIELD,TPFIELD%CDIR,GALLOC)
      IF (TPFILE%CFORMAT=='NETCDF4') THEN
         CALL IO_Field_read_nc4(TPFILE,TPFIELD,ZFIELDP,IRESP)
      ELSE IF (TPFILE%CFORMAT=='LFI') THEN
         CALL IO_Field_read_lfi(TPFILE,TPFIELD,ZFIELDP,IRESP)
      ELSE IF (TPFILE%CFORMAT=='LFICDF4') THEN
        CALL IO_Field_read_nc4(TPFILE,TPFIELD,ZFIELDP,IRESP)
      END IF
    ELSE
      !Not really necessary but useful to suppress alerts with Valgrind
      ALLOCATE(ZFIELDP(0,0,0,0,0,0))
      GALLOC = .TRUE.
    END IF
    !
    CALL MPI_BCAST(IRESP,1,MPI_INTEGER,TPFILE%NMASTER_RANK-1,TPFILE%NMPICOMM,IERR)
    !
    !Broadcast header only if IRESP==-111
    !because metadata of field has been modified in IO_Field_read_xxx
    IF (IRESP==-111) CALL IO_Field_metadata_bcast(TPFILE,TPFIELD)
    !
    IF (TPFIELD%CDIR == 'XX' .OR. TPFIELD%CDIR == 'YY') THEN
      ! XX or YY Scatter Field
      CALL SCATTER_XXFIELD(TPFIELD%CDIR,ZFIELDP,PFIELD,TPFILE%NMASTER_RANK,TPFILE%NMPICOMM)
    ELSE IF (TPFIELD%CDIR == 'XY') THEN
      ! XY Scatter Field
      CALL SCATTER_XYFIELD(ZFIELDP,PFIELD,TPFILE%NMASTER_RANK,TPFILE%NMPICOMM)
    ELSE
      CALL MPI_BCAST(PFIELD,SIZE(PFIELD),MNHREAL_MPI,TPFILE%NMASTER_RANK-1,TPFILE%NMPICOMM,IERR)
    END IF
  END IF
END IF
!
IF (GALLOC) DEALLOCATE (ZFIELDP)
!
IF (IRESP==-111) IRESP = 0 !-111 is not really an error (metadata has changed)
!
IF (PRESENT(KRESP)) KRESP = IRESP
!
END SUBROUTINE IO_Field_read_byfield_X6


SUBROUTINE IO_Field_read_byname_N0(TPFILE,HNAME,KFIELD,KRESP)
!
TYPE(TFILEDATA),  INTENT(IN)    :: TPFILE
CHARACTER(LEN=*), INTENT(IN)    :: HNAME    ! name of the field to write
INTEGER,          INTENT(INOUT) :: KFIELD   ! array containing the data field
INTEGER,OPTIONAL, INTENT(OUT)   :: KRESP    ! return-code
!
INTEGER :: ID ! Index of the field
INTEGER :: IRESP ! return_code
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_read_byname_N0',TRIM(TPFILE%CNAME)//': reading '//TRIM(HNAME))
!
CALL FIND_FIELD_ID_FROM_MNHNAME(HNAME,ID,IRESP)
!
IF(IRESP==0) CALL IO_Field_read(TPFILE,TFIELDLIST(ID),KFIELD,IRESP)
!
IF (PRESENT(KRESP)) KRESP = IRESP
!
END SUBROUTINE IO_Field_read_byname_N0

SUBROUTINE IO_Field_read_byfield_N0(TPFILE,TPFIELD,KFIELD,KRESP)
!
USE MODD_IO, ONLY: ISP,GSMONOPROC
!
TYPE(TFILEDATA),  INTENT(IN)    :: TPFILE
TYPE(TFIELDDATA), INTENT(INOUT) :: TPFIELD
INTEGER,          INTENT(INOUT) :: KFIELD   ! array containing the data field
INTEGER,OPTIONAL, INTENT(OUT)   :: KRESP    ! return-code
!
INTEGER                      :: IERR
INTEGER                      :: IRESP
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_read_byfield_N0',TRIM(TPFILE%CNAME)//': reading '//TRIM(TPFIELD%CMNHNAME))
!
IRESP = 0
!
CALL IO_File_read_check(TPFILE,'IO_Field_read_byfield_N0',IRESP)
!
IF (IRESP==0) THEN
  IF (GSMONOPROC) THEN ! sequential execution
      IF (TPFILE%CFORMAT=='NETCDF4') THEN
         CALL IO_Field_read_nc4(TPFILE,TPFIELD,KFIELD,IRESP)
      ELSE IF (TPFILE%CFORMAT=='LFI') THEN
         CALL IO_Field_read_lfi(TPFILE,TPFIELD,KFIELD,IRESP)
      ELSE IF (TPFILE%CFORMAT=='LFICDF4') THEN
        CALL IO_Field_read_nc4(TPFILE,TPFIELD,KFIELD,IRESP)
      END IF
  ELSE
    IF (ISP == TPFILE%NMASTER_RANK)  THEN
      IF (TPFILE%CFORMAT=='NETCDF4') THEN
         CALL IO_Field_read_nc4(TPFILE,TPFIELD,KFIELD,IRESP)
      ELSE IF (TPFILE%CFORMAT=='LFI') THEN
         CALL IO_Field_read_lfi(TPFILE,TPFIELD,KFIELD,IRESP)
      ELSE IF (TPFILE%CFORMAT=='LFICDF4') THEN
        CALL IO_Field_read_nc4(TPFILE,TPFIELD,KFIELD,IRESP)
      END IF
    END IF
    !
    CALL MPI_BCAST(IRESP,1,MPI_INTEGER,TPFILE%NMASTER_RANK-1,TPFILE%NMPICOMM,IERR)
    !
    !Broadcast header only if IRESP==-111
    !because metadata of field has been modified in IO_Field_read_xxx
    IF (IRESP==-111) CALL IO_Field_metadata_bcast(TPFILE,TPFIELD)
    !
    CALL MPI_BCAST(KFIELD,1,MPI_INTEGER,TPFILE%NMASTER_RANK-1,TPFILE%NMPICOMM,IERR)
  END IF
END IF
!
IF (IRESP==-111) IRESP = 0 !-111 is not really an error (metadata has changed)
!
IF (PRESENT(KRESP)) KRESP = IRESP
!
END SUBROUTINE IO_Field_read_byfield_N0


SUBROUTINE IO_Field_read_byname_N1(TPFILE,HNAME,KFIELD,KRESP)
!
TYPE(TFILEDATA),     INTENT(IN)    :: TPFILE
CHARACTER(LEN=*),    INTENT(IN)    :: HNAME    ! name of the field to write
INTEGER,DIMENSION(:),INTENT(INOUT) :: KFIELD   ! array containing the data field
INTEGER,OPTIONAL,    INTENT(OUT)   :: KRESP    ! return-code
!
INTEGER :: ID ! Index of the field
INTEGER :: IRESP ! return_code
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_read_byname_N1',TRIM(TPFILE%CNAME)//': reading '//TRIM(HNAME))
!
CALL FIND_FIELD_ID_FROM_MNHNAME(HNAME,ID,IRESP)
!
IF(IRESP==0) CALL IO_Field_read(TPFILE,TFIELDLIST(ID),KFIELD,IRESP)
!
IF (PRESENT(KRESP)) KRESP = IRESP
!
END SUBROUTINE IO_Field_read_byname_N1

SUBROUTINE IO_Field_read_byfield_N1(TPFILE,TPFIELD,KFIELD,KRESP)
!
USE MODD_IO, ONLY: ISP, GSMONOPROC
!
USE MODE_ALLOCBUFFER_ll
USE MODE_SCATTER_ll
!
TYPE(TFILEDATA),     INTENT(IN)    :: TPFILE
TYPE(TFIELDDATA),    INTENT(INOUT) :: TPFIELD
INTEGER,DIMENSION(:),INTENT(INOUT) :: KFIELD   ! array containing the data field
INTEGER,OPTIONAL,    INTENT(OUT)   :: KRESP    ! return-code
!
INTEGER                      :: IERR
INTEGER                      :: IRESP
INTEGER,DIMENSION(:),POINTER :: IFIELDP
LOGICAL                      :: GALLOC
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_read_byfield_N1',TRIM(TPFILE%CNAME)//': reading '//TRIM(TPFIELD%CMNHNAME))
!
GALLOC = .FALSE.
IRESP = 0
IFIELDP => NULL()
!
CALL IO_File_read_check(TPFILE,'IO_Field_read_byfield_N1',IRESP)
!
IF (IRESP==0) THEN
  IF (GSMONOPROC) THEN ! sequential execution
      IF (TPFILE%CFORMAT=='NETCDF4') THEN
         CALL IO_Field_read_nc4(TPFILE,TPFIELD,KFIELD,IRESP)
      ELSE IF (TPFILE%CFORMAT=='LFI') THEN
         CALL IO_Field_read_lfi(TPFILE,TPFIELD,KFIELD,IRESP)
      ELSE IF (TPFILE%CFORMAT=='LFICDF4') THEN
        CALL IO_Field_read_nc4(TPFILE,TPFIELD,KFIELD,IRESP)
      END IF
  ELSE
    IF (ISP == TPFILE%NMASTER_RANK)  THEN
      CALL ALLOCBUFFER_ll(IFIELDP,KFIELD,TPFIELD%CDIR,GALLOC)
      IF (TPFILE%CFORMAT=='NETCDF4') THEN
         CALL IO_Field_read_nc4(TPFILE,TPFIELD,IFIELDP,IRESP)
      ELSE IF (TPFILE%CFORMAT=='LFI') THEN
         CALL IO_Field_read_lfi(TPFILE,TPFIELD,IFIELDP,IRESP)
      ELSE IF (TPFILE%CFORMAT=='LFICDF4') THEN
        CALL IO_Field_read_nc4(TPFILE,TPFIELD,IFIELDP,IRESP)
      END IF
    ELSE
      !Not really necessary but useful to suppress alerts with Valgrind
      ALLOCATE(IFIELDP(0))
      GALLOC = .TRUE.
    END IF
    !
    CALL MPI_BCAST(IRESP,1,MPI_INTEGER,TPFILE%NMASTER_RANK-1,TPFILE%NMPICOMM,IERR)
    !
    !Broadcast header only if IRESP==-111
    !because metadata of field has been modified in IO_Field_read_xxx
    IF (IRESP==-111) CALL IO_Field_metadata_bcast(TPFILE,TPFIELD)
    !
    IF (TPFIELD%CDIR /= 'XX' .AND. TPFIELD%CDIR /='YY') THEN
      ! Broadcast Field
      CALL MPI_BCAST(KFIELD,SIZE(KFIELD),MPI_INTEGER,TPFILE%NMASTER_RANK-1,TPFILE%NMPICOMM,IERR)
    ELSE
      !Scatter Field
      CALL SCATTER_XXFIELD(TPFIELD%CDIR,IFIELDP,KFIELD,TPFILE%NMASTER_RANK,TPFILE%NMPICOMM)
    END IF
  END IF
END IF
!
IF (GALLOC) DEALLOCATE (IFIELDP)
!
IF (IRESP==-111) IRESP = 0 !-111 is not really an error (metadata has changed)
!
IF (PRESENT(KRESP)) KRESP = IRESP
!
END SUBROUTINE IO_Field_read_byfield_N1


SUBROUTINE IO_Field_read_byname_N2(TPFILE,HNAME,KFIELD,KRESP)
!
USE MODD_IO,           ONLY: ISNPROC
USE MODD_STRUCTURE_ll, ONLY: ZONE_ll
!
!
TYPE(TFILEDATA),       INTENT(IN)    :: TPFILE
CHARACTER(LEN=*),      INTENT(IN)    :: HNAME    ! name of the field to write
INTEGER,DIMENSION(:,:),INTENT(INOUT) :: KFIELD   ! array containing the data field
INTEGER,OPTIONAL,      INTENT(OUT)   :: KRESP    ! return-code
!
INTEGER :: ID    ! Index of the field
INTEGER :: IRESP ! return_code
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_read_byname_N2',TRIM(TPFILE%CNAME)//': reading '//TRIM(HNAME))
!
CALL FIND_FIELD_ID_FROM_MNHNAME(HNAME,ID,IRESP)
!
IF(IRESP==0) CALL IO_Field_read(TPFILE,TFIELDLIST(ID),KFIELD,IRESP)
!
IF (PRESENT(KRESP)) KRESP = IRESP
!
END SUBROUTINE IO_Field_read_byname_N2

SUBROUTINE IO_Field_read_byfield_N2(TPFILE,TPFIELD,KFIELD,KRESP)
!
USE MODD_IO,           ONLY: GSMONOPROC, ISP, ISNPROC, LPACK, L1D, L2D
USE MODD_STRUCTURE_ll, ONLY: ZONE_ll
USE MODD_TIMEZ,        ONLY: TIMEZ
!
USE MODE_ALLOCBUFFER_ll
USE MODE_SCATTER_ll
!
TYPE(TFILEDATA),              INTENT(IN)    :: TPFILE
TYPE(TFIELDDATA),             INTENT(INOUT) :: TPFIELD
INTEGER,DIMENSION(:,:),TARGET,INTENT(INOUT) :: KFIELD   ! array containing the data field
INTEGER, OPTIONAL,            INTENT(OUT)   :: KRESP    ! return-code
!
INTEGER                         :: IERR
INTEGER,DIMENSION(:,:),POINTER  :: IFIELDP
LOGICAL                         :: GALLOC
INTEGER                         :: IRESP
INTEGER                         :: IHEXTOT
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_read_byfield_N2',TRIM(TPFILE%CNAME)//': reading '//TRIM(TPFIELD%CMNHNAME))
!
GALLOC = .FALSE.
IRESP = 0
IFIELDP => NULL()
!
IHEXTOT = 2*JPHEXT+1
CALL IO_File_read_check(TPFILE,'IO_Field_read_byfield_N2',IRESP)
!
IF (IRESP==0) THEN
  IF (GSMONOPROC) THEN ! sequential execution
    IF (LPACK .AND. L1D .AND. SIZE(KFIELD,1)==IHEXTOT .AND. SIZE(KFIELD,2)==IHEXTOT) THEN
      IFIELDP=>KFIELD(JPHEXT+1:JPHEXT+1,JPHEXT+1:JPHEXT+1)
    ELSE IF (LPACK .AND. L2D .AND. SIZE(KFIELD,2)==IHEXTOT) THEN
      IFIELDP=>KFIELD(:,JPHEXT+1:JPHEXT+1)
    ELSE
      IFIELDP=>KFIELD(:,:)
    END IF
    IF (TPFILE%CFORMAT=='NETCDF4') THEN
      CALL IO_Field_read_nc4(TPFILE,TPFIELD,IFIELDP,IRESP)
    ELSE IF (TPFILE%CFORMAT=='LFI') THEN
      CALL IO_Field_read_lfi(TPFILE,TPFIELD,IFIELDP,IRESP)
    ELSE IF (TPFILE%CFORMAT=='LFICDF4') THEN
      CALL IO_Field_read_nc4(TPFILE,TPFIELD,IFIELDP,IRESP)
    END IF
    IF (LPACK .AND. L1D .AND. SIZE(KFIELD,1)==IHEXTOT .AND. SIZE(KFIELD,2)==IHEXTOT) THEN
      KFIELD(:,:)=SPREAD(SPREAD(KFIELD(JPHEXT+1,JPHEXT+1),DIM=1,NCOPIES=IHEXTOT),DIM=2,NCOPIES=IHEXTOT)
    ELSE IF (LPACK .AND. L2D .AND. SIZE(KFIELD,2)==IHEXTOT) THEN
      KFIELD(:,:)=SPREAD(KFIELD(:,JPHEXT+1),DIM=2,NCOPIES=IHEXTOT)
    END IF
  ELSE
    IF (ISP == TPFILE%NMASTER_RANK)  THEN
      ! I/O process case
      CALL ALLOCBUFFER_ll(IFIELDP,KFIELD,TPFIELD%CDIR,GALLOC)
      IF (TPFILE%CFORMAT=='NETCDF4') THEN
         CALL IO_Field_read_nc4(TPFILE,TPFIELD,IFIELDP,IRESP)
      ELSE IF (TPFILE%CFORMAT=='LFI') THEN
         CALL IO_Field_read_lfi(TPFILE,TPFIELD,IFIELDP,IRESP)
      ELSE IF (TPFILE%CFORMAT=='LFICDF4') THEN
        CALL IO_Field_read_nc4(TPFILE,TPFIELD,IFIELDP,IRESP)
      END IF
    ELSE
      !Not really necessary but useful to suppress alerts with Valgrind
      ALLOCATE(IFIELDP(0,0))
      GALLOC = .TRUE.
    END IF
    !
    CALL MPI_BCAST(IRESP,1,MPI_INTEGER,TPFILE%NMASTER_RANK-1,TPFILE%NMPICOMM,IERR)
    !
    !Broadcast header only if IRESP==-111
    !because metadata of field has been modified in IO_Field_read_xxx
    IF (IRESP==-111) CALL IO_Field_metadata_bcast(TPFILE,TPFIELD)
    !
    IF (TPFIELD%CDIR == 'XX' .OR. TPFIELD%CDIR == 'YY') THEN
      ! XX or YY Scatter Field
      CALL SCATTER_XXFIELD(TPFIELD%CDIR,IFIELDP,KFIELD,TPFILE%NMASTER_RANK,TPFILE%NMPICOMM)
      ! Broadcast Field
      CALL MPI_BCAST(KFIELD,SIZE(KFIELD),MNHREAL_MPI,TPFILE%NMASTER_RANK-1,TPFILE%NMPICOMM,IERR)
    ELSE IF (TPFIELD%CDIR == 'XY') THEN
      IF (LPACK .AND. L2D) THEN
        ! 2D compact case
        CALL SCATTER_XXFIELD('XX',IFIELDP(:,1),KFIELD(:,JPHEXT+1),TPFILE%NMASTER_RANK,TPFILE%NMPICOMM)
        KFIELD(:,:) = SPREAD(KFIELD(:,JPHEXT+1),DIM=2,NCOPIES=IHEXTOT)
      ELSE
        ! XY Scatter Field
        CALL SCATTER_XYFIELD(IFIELDP,KFIELD,TPFILE%NMASTER_RANK,TPFILE%NMPICOMM)
      END IF
    ELSE
      IF (ISP == TPFILE%NMASTER_RANK) KFIELD = IFIELDP
      CALL MPI_BCAST(KFIELD,SIZE(KFIELD),MPI_INTEGER,TPFILE%NMASTER_RANK-1,TPFILE%NMPICOMM,IERR)
    END IF
  END IF
END IF
!
IF (GALLOC) DEALLOCATE (IFIELDP)
!
IF (IRESP==-111) IRESP = 0 !-111 is not really an error (metadata has changed)
!
IF (PRESENT(KRESP)) KRESP = IRESP
!
END SUBROUTINE IO_Field_read_byfield_N2


SUBROUTINE IO_Field_read_byname_L0(TPFILE,HNAME,OFIELD,KRESP)
!
TYPE(TFILEDATA),  INTENT(IN)    :: TPFILE
CHARACTER(LEN=*), INTENT(IN)    :: HNAME    ! name of the field to write
LOGICAL,          INTENT(INOUT) :: OFIELD   ! array containing the data field
INTEGER,OPTIONAL, INTENT(OUT)   :: KRESP    ! return-code
!
INTEGER :: ID ! Index of the field
INTEGER :: IRESP ! return_code
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_read_byname_L0',TRIM(TPFILE%CNAME)//': reading '//TRIM(HNAME))
!
CALL FIND_FIELD_ID_FROM_MNHNAME(HNAME,ID,IRESP)
!
IF(IRESP==0) CALL IO_Field_read(TPFILE,TFIELDLIST(ID),OFIELD,IRESP)
!
IF (PRESENT(KRESP)) KRESP = IRESP
!
END SUBROUTINE IO_Field_read_byname_L0

SUBROUTINE IO_Field_read_byfield_L0(TPFILE,TPFIELD,OFIELD,KRESP)
!
USE MODD_IO, ONLY: ISP, GSMONOPROC
!
TYPE(TFILEDATA),  INTENT(IN)    :: TPFILE
TYPE(TFIELDDATA), INTENT(INOUT) :: TPFIELD
LOGICAL,          INTENT(INOUT) :: OFIELD   ! array containing the data field
INTEGER,OPTIONAL, INTENT(OUT)   :: KRESP    ! return-code
!
INTEGER                      :: IERR
INTEGER                      :: IRESP
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_read_byfield_L0',TRIM(TPFILE%CNAME)//': reading '//TRIM(TPFIELD%CMNHNAME))
!
IRESP = 0
!
CALL IO_File_read_check(TPFILE,'IO_Field_read_byfield_L0',IRESP)
!
IF (IRESP==0) THEN
  IF (GSMONOPROC) THEN ! sequential execution
      IF (TPFILE%CFORMAT=='NETCDF4') THEN
         CALL IO_Field_read_nc4(TPFILE,TPFIELD,OFIELD,IRESP)
      ELSE IF (TPFILE%CFORMAT=='LFI') THEN
         CALL IO_Field_read_lfi(TPFILE,TPFIELD,OFIELD,IRESP)
      ELSE IF (TPFILE%CFORMAT=='LFICDF4') THEN
        CALL IO_Field_read_nc4(TPFILE,TPFIELD,OFIELD,IRESP)
      END IF
  ELSE
    IF (ISP == TPFILE%NMASTER_RANK)  THEN
      IF (TPFILE%CFORMAT=='NETCDF4') THEN
         CALL IO_Field_read_nc4(TPFILE,TPFIELD,OFIELD,IRESP)
      ELSE IF (TPFILE%CFORMAT=='LFI') THEN
         CALL IO_Field_read_lfi(TPFILE,TPFIELD,OFIELD,IRESP)
      ELSE IF (TPFILE%CFORMAT=='LFICDF4') THEN
        CALL IO_Field_read_nc4(TPFILE,TPFIELD,OFIELD,IRESP)
      END IF
    END IF
    !
    CALL MPI_BCAST(IRESP,1,MPI_INTEGER,TPFILE%NMASTER_RANK-1,TPFILE%NMPICOMM,IERR)
    !
    !Broadcast header only if IRESP==-111
    !because metadata of field has been modified in IO_Field_read_xxx
    IF (IRESP==-111) CALL IO_Field_metadata_bcast(TPFILE,TPFIELD)
    !
    CALL MPI_BCAST(OFIELD,1,MPI_LOGICAL,TPFILE%NMASTER_RANK-1,TPFILE%NMPICOMM,IERR)
  END IF
END IF
!
IF (IRESP==-111) IRESP = 0 !-111 is not really an error (metadata has changed)
!
IF (PRESENT(KRESP)) KRESP = IRESP
!
END SUBROUTINE IO_Field_read_byfield_L0


SUBROUTINE IO_Field_read_byname_L1(TPFILE,HNAME,OFIELD,KRESP)
!
TYPE(TFILEDATA),     INTENT(IN)    :: TPFILE
CHARACTER(LEN=*),    INTENT(IN)    :: HNAME    ! name of the field to write
LOGICAL,DIMENSION(:),INTENT(INOUT) :: OFIELD   ! array containing the data field
INTEGER,OPTIONAL,    INTENT(OUT)   :: KRESP    ! return-code
!
INTEGER :: ID ! Index of the field
INTEGER :: IRESP ! return_code
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_read_byname_L1',TRIM(TPFILE%CNAME)//': reading '//TRIM(HNAME))
!
CALL FIND_FIELD_ID_FROM_MNHNAME(HNAME,ID,IRESP)
!
IF(IRESP==0) CALL IO_Field_read(TPFILE,TFIELDLIST(ID),OFIELD,IRESP)
!
IF (PRESENT(KRESP)) KRESP = IRESP
!
END SUBROUTINE IO_Field_read_byname_L1

SUBROUTINE IO_Field_read_byfield_L1(TPFILE,TPFIELD,OFIELD,KRESP)
!
USE MODD_IO, ONLY: ISP, GSMONOPROC
!
TYPE(TFILEDATA),     INTENT(IN)    :: TPFILE
TYPE(TFIELDDATA),    INTENT(INOUT) :: TPFIELD
LOGICAL,DIMENSION(:),INTENT(INOUT) :: OFIELD   ! array containing the data field
INTEGER,OPTIONAL,    INTENT(OUT)   :: KRESP    ! return-code
!
INTEGER                      :: IERR
INTEGER                      :: IRESP
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_read_byfield_L1',TRIM(TPFILE%CNAME)//': reading '//TRIM(TPFIELD%CMNHNAME))
!
IRESP = 0
!
CALL IO_File_read_check(TPFILE,'IO_Field_read_byfield_L1',IRESP)
!
IF (IRESP==0) THEN
  IF (GSMONOPROC) THEN ! sequential execution
      IF (TPFILE%CFORMAT=='NETCDF4') THEN
         CALL IO_Field_read_nc4(TPFILE,TPFIELD,OFIELD,IRESP)
      ELSE IF (TPFILE%CFORMAT=='LFI') THEN
         CALL IO_Field_read_lfi(TPFILE,TPFIELD,OFIELD,IRESP)
      ELSE IF (TPFILE%CFORMAT=='LFICDF4') THEN
        CALL IO_Field_read_nc4(TPFILE,TPFIELD,OFIELD,IRESP)
      END IF
  ELSE
    IF (ISP == TPFILE%NMASTER_RANK)  THEN
      IF (TPFILE%CFORMAT=='NETCDF4') THEN
         CALL IO_Field_read_nc4(TPFILE,TPFIELD,OFIELD,IRESP)
      ELSE IF (TPFILE%CFORMAT=='LFI') THEN
         CALL IO_Field_read_lfi(TPFILE,TPFIELD,OFIELD,IRESP)
      ELSE IF (TPFILE%CFORMAT=='LFICDF4') THEN
        CALL IO_Field_read_nc4(TPFILE,TPFIELD,OFIELD,IRESP)
      END IF
    END IF
    !
    CALL MPI_BCAST(IRESP,1,MPI_INTEGER,TPFILE%NMASTER_RANK-1,TPFILE%NMPICOMM,IERR)
    !
    !Broadcast header only if IRESP==-111
    !because metadata of field has been modified in IO_Field_read_xxx
    IF (IRESP==-111) CALL IO_Field_metadata_bcast(TPFILE,TPFIELD)
    !
    CALL MPI_BCAST(OFIELD,SIZE(OFIELD),MPI_LOGICAL,TPFILE%NMASTER_RANK-1,TPFILE%NMPICOMM,IERR)
  END IF
END IF
!
IF (IRESP==-111) IRESP = 0 !-111 is not really an error (metadata has changed)
!
IF (PRESENT(KRESP)) KRESP = IRESP
!
END SUBROUTINE IO_Field_read_byfield_L1


SUBROUTINE IO_Field_read_byname_C0(TPFILE,HNAME,HFIELD,KRESP)
!
TYPE(TFILEDATA),  INTENT(IN)    :: TPFILE
CHARACTER(LEN=*), INTENT(IN)    :: HNAME    ! name of the field to write
CHARACTER(LEN=*), INTENT(INOUT) :: HFIELD   ! array containing the data field
INTEGER,OPTIONAL, INTENT(OUT)   :: KRESP    ! return-code
!
INTEGER :: ID ! Index of the field
INTEGER :: IRESP ! return_code
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_read_byname_C0',TRIM(TPFILE%CNAME)//': reading '//TRIM(HNAME))
!
CALL FIND_FIELD_ID_FROM_MNHNAME(HNAME,ID,IRESP)
!
IF(IRESP==0) CALL IO_Field_read(TPFILE,TFIELDLIST(ID),HFIELD,IRESP)
!
IF (PRESENT(KRESP)) KRESP = IRESP
!
END SUBROUTINE IO_Field_read_byname_C0

SUBROUTINE IO_Field_read_byfield_C0(TPFILE,TPFIELD,HFIELD,KRESP)
!
USE MODD_IO, ONLY: ISP, GSMONOPROC
!
TYPE(TFILEDATA),  INTENT(IN)    :: TPFILE
TYPE(TFIELDDATA), INTENT(INOUT) :: TPFIELD
CHARACTER(LEN=*), INTENT(INOUT) :: HFIELD   ! array containing the data field
INTEGER,OPTIONAL, INTENT(OUT)   :: KRESP    ! return-code
!
INTEGER                      :: IERR
INTEGER                      :: IRESP
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_read_byfield_C0',TRIM(TPFILE%CNAME)//': reading '//TRIM(TPFIELD%CMNHNAME))
!
IRESP = 0
!
CALL IO_File_read_check(TPFILE,'IO_Field_read_byfield_C0',IRESP)
!
IF (IRESP==0) THEN
  IF (GSMONOPROC) THEN ! sequential execution
      IF (TPFILE%CFORMAT=='NETCDF4') THEN
         CALL IO_Field_read_nc4(TPFILE,TPFIELD,HFIELD,IRESP)
      ELSE IF (TPFILE%CFORMAT=='LFI') THEN
         CALL IO_Field_read_lfi(TPFILE,TPFIELD,HFIELD,IRESP)
      ELSE IF (TPFILE%CFORMAT=='LFICDF4') THEN
        CALL IO_Field_read_nc4(TPFILE,TPFIELD,HFIELD,IRESP)
      END IF
  ELSE
    IF (ISP == TPFILE%NMASTER_RANK)  THEN
      IF (TPFILE%CFORMAT=='NETCDF4') THEN
         CALL IO_Field_read_nc4(TPFILE,TPFIELD,HFIELD,IRESP)
      ELSE IF (TPFILE%CFORMAT=='LFI') THEN
         CALL IO_Field_read_lfi(TPFILE,TPFIELD,HFIELD,IRESP)
      ELSE IF (TPFILE%CFORMAT=='LFICDF4') THEN
        CALL IO_Field_read_nc4(TPFILE,TPFIELD,HFIELD,IRESP)
      END IF
    END IF
    !
    CALL MPI_BCAST(IRESP,1,MPI_INTEGER,TPFILE%NMASTER_RANK-1,TPFILE%NMPICOMM,IERR)
    !
    !Broadcast header only if IRESP==-111
    !because metadata of field has been modified in IO_Field_read_xxx
    IF (IRESP==-111) CALL IO_Field_metadata_bcast(TPFILE,TPFIELD)
    !
    CALL MPI_BCAST(HFIELD,LEN(HFIELD),MPI_CHARACTER,TPFILE%NMASTER_RANK-1,TPFILE%NMPICOMM,IERR)
  END IF
END IF
!
IF (IRESP==-111) IRESP = 0 !-111 is not really an error (metadata has changed)
!
IF (PRESENT(KRESP)) KRESP = IRESP
!
END SUBROUTINE IO_Field_read_byfield_C0


SUBROUTINE IO_Field_read_byname_T0(TPFILE,HNAME,TPDATA,KRESP)
!
TYPE(TFILEDATA),  INTENT(IN)    :: TPFILE
CHARACTER(LEN=*), INTENT(IN)    :: HNAME    ! name of the field to write
TYPE (DATE_TIME), INTENT(INOUT) :: TPDATA   ! array containing the data field
INTEGER,OPTIONAL, INTENT(OUT)   :: KRESP    ! return-code
!
INTEGER :: ID ! Index of the field
INTEGER :: IRESP ! return_code
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_read_byname_T0',TRIM(TPFILE%CNAME)//': reading '//TRIM(HNAME))
!
CALL FIND_FIELD_ID_FROM_MNHNAME(HNAME,ID,IRESP)
!
IF(IRESP==0) CALL IO_Field_read(TPFILE,TFIELDLIST(ID),TPDATA,IRESP)
!
IF (PRESENT(KRESP)) KRESP = IRESP
!
END SUBROUTINE IO_Field_read_byname_T0

SUBROUTINE IO_Field_read_byfield_T0(TPFILE,TPFIELD,TPDATA,KRESP)
!
USE MODD_IO, ONLY: ISP, GSMONOPROC
!
TYPE(TFILEDATA),  INTENT(IN)    :: TPFILE
TYPE(TFIELDDATA), INTENT(INOUT) :: TPFIELD
TYPE (DATE_TIME), INTENT(INOUT) :: TPDATA   ! array containing the data field
INTEGER,OPTIONAL, INTENT(OUT)   :: KRESP    ! return-code
!
INTEGER                      :: IERR
INTEGER                      :: IRESP
INTEGER,DIMENSION(3)         :: ITDATE
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_read_byfield_T0',TRIM(TPFILE%CNAME)//': reading '//TRIM(TPFIELD%CMNHNAME))
!
IRESP = 0
!
CALL IO_File_read_check(TPFILE,'IO_Field_read_byfield_T0',IRESP)
!
IF (IRESP==0) THEN
  IF (GSMONOPROC) THEN ! sequential execution
      IF (TPFILE%CFORMAT=='NETCDF4') THEN
         CALL IO_Field_read_nc4(TPFILE,TPFIELD,TPDATA,IRESP)
      ELSE IF (TPFILE%CFORMAT=='LFI') THEN
         CALL IO_Field_read_lfi(TPFILE,TPFIELD,TPDATA,IRESP)
      ELSE IF (TPFILE%CFORMAT=='LFICDF4') THEN
        CALL IO_Field_read_nc4(TPFILE,TPFIELD,TPDATA,IRESP)
      END IF
  ELSE
    IF (ISP == TPFILE%NMASTER_RANK)  THEN
      IF (TPFILE%CFORMAT=='NETCDF4') THEN
         CALL IO_Field_read_nc4(TPFILE,TPFIELD,TPDATA,IRESP)
      ELSE IF (TPFILE%CFORMAT=='LFI') THEN
         CALL IO_Field_read_lfi(TPFILE,TPFIELD,TPDATA,IRESP)
      ELSE IF (TPFILE%CFORMAT=='LFICDF4') THEN
        CALL IO_Field_read_nc4(TPFILE,TPFIELD,TPDATA,IRESP)
      END IF
      ITDATE(1) = TPDATA%TDATE%YEAR
      ITDATE(2) = TPDATA%TDATE%MONTH
      ITDATE(3) = TPDATA%TDATE%DAY
    END IF
    !
    CALL MPI_BCAST(IRESP,1,MPI_INTEGER,TPFILE%NMASTER_RANK-1,TPFILE%NMPICOMM,IERR)
    !
    !Broadcast header only if IRESP==-111
    !because metadata of field has been modified in IO_Field_read_xxx
    IF (IRESP==-111) CALL IO_Field_metadata_bcast(TPFILE,TPFIELD)
    !
    CALL MPI_BCAST(ITDATE,     3,MPI_INTEGER,TPFILE%NMASTER_RANK-1,TPFILE%NMPICOMM,IERR)
    CALL MPI_BCAST(TPDATA%TIME,1,MNHREAL_MPI,  TPFILE%NMASTER_RANK-1,TPFILE%NMPICOMM,IERR)
    TPDATA%TDATE%YEAR  = ITDATE(1)
    TPDATA%TDATE%MONTH = ITDATE(2)
    TPDATA%TDATE%DAY   = ITDATE(3)
  END IF
END IF
!
IF (IRESP==-111) IRESP = 0 !-111 is not really an error (metadata has changed)
!
IF (PRESENT(KRESP)) KRESP = IRESP
!
END SUBROUTINE IO_Field_read_byfield_T0


SUBROUTINE IO_Field_read_byname_lb(TPFILE,HNAME,KL3D,KRIM,PLB,KRESP)
!
TYPE(TFILEDATA),               INTENT(IN)    :: TPFILE
CHARACTER(LEN=*),              INTENT(IN)    :: HNAME   ! name of the field to write
INTEGER,                       INTENT(IN)    :: KL3D    ! size of the LB array in FM
INTEGER,                       INTENT(IN)    :: KRIM    ! size of the LB area
REAL, DIMENSION(:,:,:),TARGET, INTENT(INOUT) :: PLB     ! array containing the LB field
INTEGER,OPTIONAL,              INTENT(OUT)   :: KRESP   ! return-code

INTEGER :: ID ! Index of the field
INTEGER :: IRESP ! return_code
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_read_byname_lb',TRIM(TPFILE%CNAME)//': reading '//TRIM(HNAME))
!
CALL FIND_FIELD_ID_FROM_MNHNAME(HNAME,ID,IRESP)
!
IF(IRESP==0) CALL IO_Field_read_lb(TPFILE,TFIELDLIST(ID),KL3D,KRIM,PLB,IRESP)
!
IF (PRESENT(KRESP)) KRESP = IRESP
!
END SUBROUTINE IO_Field_read_byname_lb

SUBROUTINE IO_Field_read_byfield_lb(TPFILE,TPFIELD,KL3D,KRIM,PLB,KRESP)
!
USE MODD_IO,            ONLY: GSMONOPROC, ISP, ISNPROC, LPACK, L2D
USE MODD_PARAMETERS_ll, ONLY: JPHEXT
USE MODD_TIMEZ,         ONLY: TIMEZ
USE MODD_VAR_ll,        ONLY: MNH_STATUSES_IGNORE
!
USE MODE_DISTRIB_lb
USE MODE_MNH_TIMING,    ONLY: SECOND_MNH2
USE MODE_TOOLS_ll,      ONLY: GET_GLOBALDIMS_ll
!
TYPE(TFILEDATA),               INTENT(IN)    :: TPFILE
TYPE(TFIELDDATA),              INTENT(INOUT) :: TPFIELD
INTEGER,                       INTENT(IN)    :: KL3D    ! size of the LB array in FM
INTEGER,                       INTENT(IN)    :: KRIM    ! size of the LB area
REAL, DIMENSION(:,:,:),TARGET, INTENT(INOUT) :: PLB     ! array containing the LB field
INTEGER,OPTIONAL,              INTENT(OUT)   :: KRESP   ! return-code
!
!*      0.2   Declarations of local variables
!
TYPE TX_3DP
  REAL,DIMENSION(:,:,:), POINTER    :: X
END TYPE
!
CHARACTER(LEN=4)                         :: YLBTYPE  ! 'LBX','LBXU','LBY' or 'LBYV'
INTEGER                                  :: IERR,IRESP
INTEGER                                  :: IHEXTOT
INTEGER                                  :: IIMAX_ll,IJMAX_ll
INTEGER                                  :: IIB,IIE,IJB,IJE
INTEGER                                  :: JI
INTEGER                                  :: NB_REQ,IKU
INTEGER, DIMENSION(MPI_STATUS_SIZE)      :: STATUS
INTEGER, ALLOCATABLE,DIMENSION(:,:)      :: STATUSES
INTEGER,ALLOCATABLE,DIMENSION(:)         :: REQ_TAB
REAL,DIMENSION(:,:,:),ALLOCATABLE,TARGET :: Z3D
REAL,DIMENSION(:,:,:), POINTER           :: TX3DP
REAL(kind=MNHTIME), DIMENSION(2)         :: T0, T1, T2, T3
REAL(kind=MNHTIME), DIMENSION(2)         :: T11, T22
TYPE(TX_3DP),ALLOCATABLE,DIMENSION(:)    :: T_TX3DP
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_read_byfield_lb','reading '//TRIM(TPFIELD%CMNHNAME))
!
YLBTYPE  = TPFIELD%CLBTYPE
!
IF (YLBTYPE/='LBX' .AND. YLBTYPE/='LBXU' .AND. YLBTYPE/='LBY' .AND. YLBTYPE/='LBYV') THEN
  CALL PRINT_MSG(NVERB_ERROR,'IO','IO_Field_read_byfield_lb',TRIM(TPFILE%CNAME)//': invalid CLBTYPE (' &
                 //TRIM(TPFIELD%CLBTYPE)//') for '//TRIM(TPFIELD%CMNHNAME))
  RETURN
END IF
!
!*      1.1   THE NAME OF LFIFM
!
CALL SECOND_MNH2(T11)
IRESP = 0
!------------------------------------------------------------------
IHEXTOT = 2*JPHEXT+1
CALL IO_File_read_check(TPFILE,'IO_Field_read_byfield_lb',IRESP)
!
IF (IRESP==0) THEN
  IF (GSMONOPROC) THEN ! sequential execution
    IF (YLBTYPE == 'LBX' .OR. YLBTYPE == 'LBXU') THEN
      ALLOCATE(Z3D(KL3D,SIZE(PLB,2),SIZE(PLB,3)))
      Z3D = 0.0
      IF (LPACK .AND. L2D) THEN
        TX3DP=>Z3D(:,JPHEXT+1:JPHEXT+1,:)
      ELSE
        TX3DP => Z3D(:,:,:)
      END IF
    ELSE !(YLBTYPE == 'LBY' .OR. YLBTYPE == 'LBYV')
      ALLOCATE(Z3D(SIZE(PLB,1),KL3D,SIZE(PLB,3)))
      Z3D = 0.0
      TX3DP => Z3D(:,:,:)
    END IF
    IF (TPFILE%CFORMAT=='NETCDF4') THEN
      CALL IO_Field_read_nc4(TPFILE,TPFIELD,TX3DP,IRESP)
    ELSE IF (TPFILE%CFORMAT=='LFI') THEN
      CALL IO_Field_read_lfi(TPFILE,TPFIELD,TX3DP,IRESP)
    ELSE IF (TPFILE%CFORMAT=='LFICDF4') THEN
      CALL IO_Field_read_nc4(TPFILE,TPFIELD,TX3DP,IRESP)
    END IF
    IF (YLBTYPE == 'LBX' .OR. YLBTYPE == 'LBXU') THEN
      IF (LPACK .AND. L2D) Z3D(:,:,:) = SPREAD(Z3D(:,JPHEXT+1,:),DIM=2,NCOPIES=IHEXTOT)
      PLB(1:KRIM+JPHEXT,:,:)          = Z3D(1:KRIM+JPHEXT,:,:)
      PLB(KRIM+JPHEXT+1:2*(KRIM+JPHEXT),:,:) = Z3D(KL3D-KRIM-JPHEXT+1:KL3D,:,:)
    ELSE !(YLBTYPE == 'LBY' .OR. YLBTYPE == 'LBYV')
      PLB(:,1:KRIM+JPHEXT,:)                 = Z3D(:,1:KRIM+JPHEXT,:)
      PLB(:,KRIM+JPHEXT+1:2*(KRIM+JPHEXT),:) = Z3D(:,KL3D-KRIM-JPHEXT+1:KL3D,:)
    END IF
  ELSE                 ! multiprocesses execution
    IF (ISP == TPFILE%NMASTER_RANK)  THEN
      CALL SECOND_MNH2(T0)
      CALL GET_GLOBALDIMS_ll(IIMAX_ll,IJMAX_ll)
      IF (YLBTYPE == 'LBX' .OR. YLBTYPE == 'LBXU') THEN
        ALLOCATE(Z3D(KL3D,IJMAX_ll+2*JPHEXT,SIZE(PLB,3)))
        Z3D = 0.0
        IF (LPACK .AND. L2D) THEN
          TX3DP=>Z3D(:,JPHEXT+1:JPHEXT+1,:)
        ELSE
          TX3DP => Z3D(:,:,:)
        END IF
      ELSE !(YLBTYPE == 'LBY' .OR. YLBTYPE == 'LBYV')
        ALLOCATE(Z3D(IIMAX_ll+2*JPHEXT,KL3D,SIZE(PLB,3)))
        Z3D = 0.0
        TX3DP => Z3D(:,:,:)
      END IF
      IF (TPFILE%CFORMAT=='NETCDF4') THEN
        CALL IO_Field_read_nc4(TPFILE,TPFIELD,TX3DP,IRESP)
      ELSE IF (TPFILE%CFORMAT=='LFI') THEN
        CALL IO_Field_read_lfi(TPFILE,TPFIELD,TX3DP,IRESP)
      ELSE IF (TPFILE%CFORMAT=='LFICDF4') THEN
        CALL IO_Field_read_nc4(TPFILE,TPFIELD,TX3DP,IRESP)
      END IF
      IF (YLBTYPE == 'LBX' .OR. YLBTYPE == 'LBXU') THEN
        IF (LPACK .AND. L2D) Z3D(:,:,:) = SPREAD(Z3D(:,JPHEXT+1,:),DIM=2,NCOPIES=IHEXTOT)
        ! erase gap in LB field
        Z3D(KRIM+JPHEXT+1:2*(KRIM+JPHEXT),:,:) = Z3D(KL3D-KRIM-JPHEXT+1:KL3D,:,:)
      ELSE !(YLBTYPE == 'LBY' .OR. YLBTYPE == 'LBYV')
        ! erase gap in LB field
        Z3D(:,KRIM+JPHEXT+1:2*(KRIM+JPHEXT),:) = Z3D(:,KL3D-KRIM-JPHEXT+1:KL3D,:)
      END IF
      CALL SECOND_MNH2(T1)
      TIMEZ%T_READLB_READ=TIMEZ%T_READLB_READ + T1 - T0
    END IF
    !
    CALL MPI_BCAST(IRESP,1,MPI_INTEGER,TPFILE%NMASTER_RANK-1,TPFILE%NMPICOMM,IERR)
    !
    !Broadcast header only if IRESP==-111
    !because metadata of field has been modified in IO_Field_read_xxx
    IF (IRESP==-111) CALL IO_Field_metadata_bcast(TPFILE,TPFIELD)
    !
    NB_REQ=0
    ALLOCATE(REQ_TAB(ISNPROC-1))
    !REQ_TAB=MPI_REQUEST_NULL
    IF (ISP == TPFILE%NMASTER_RANK)  THEN
       CALL SECOND_MNH2(T1)
      !ALLOCATE(REQ_TAB(ISNPROC-1))
      !REQ_TAB=MPI_REQUEST_NULL
      ALLOCATE(T_TX3DP(ISNPROC-1))
      IKU = SIZE(Z3D,3)
      DO JI = 1,ISNPROC
        CALL GET_DISTRIB_lb(YLBTYPE,JI,'FM','READ',KRIM,IIB,IIE,IJB,IJE)
        IF (IIB /= 0) THEN
          TX3DP=>Z3D(IIB:IIE,IJB:IJE,:)
          IF (ISP /= JI) THEN
            NB_REQ = NB_REQ + 1
            ALLOCATE(T_TX3DP(NB_REQ)%X(IIB:IIE,IJB:IJE,IKU))
            T_TX3DP(NB_REQ)%X=Z3D(IIB:IIE,IJB:IJE,:)
            CALL MPI_ISEND(T_TX3DP(NB_REQ)%X,SIZE(TX3DP),MNHREAL_MPI,JI-1,99,TPFILE%NMPICOMM,REQ_TAB(NB_REQ),IERR)
            !CALL MPI_BSEND(T_TX3DP(NB_REQ)%X,SIZE(TX3DP),MNHREAL_MPI,JI-1,99,TPFILE%NMPICOMM,IERR)
          ELSE
            CALL GET_DISTRIB_lb(YLBTYPE,JI,'LOC','READ',KRIM,IIB,IIE,IJB,IJE)
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
      CALL GET_DISTRIB_lb(YLBTYPE,ISP,'LOC','READ',KRIM,IIB,IIE,IJB,IJE)
      IF (IIB /= 0) THEN
        TX3DP=>PLB(IIB:IIE,IJB:IJE,:)
        CALL MPI_RECV(TX3DP,SIZE(TX3DP),MNHREAL_MPI,TPFILE%NMASTER_RANK-1,99,TPFILE%NMPICOMM,STATUS,IERR)
        !NB_REQ = NB_REQ + 1
        !CALL MPI_IRECV(TX3DP,SIZE(TX3DP),MNHREAL_MPI,TPFILE%NMASTER_RANK-1,99,TPFILE%NMPICOMM,REQ_TAB(NB_REQ),IERR)
        !IF (NB_REQ .GT.0 ) CALL MPI_WAITALL(NB_REQ,REQ_TAB,MNH_STATUSES_IGNORE,IERR)
      END IF
      CALL SECOND_MNH2(T1)
      TIMEZ%T_READLB_RECV=TIMEZ%T_READLB_RECV + T1 - T0
    END IF
    DEALLOCATE(REQ_TAB)
  END IF !(GSMONOPROC)
END IF
!----------------------------------------------------------------
!
IF (ALLOCATED(Z3D)) DEALLOCATE (Z3D)
!
IF (IRESP==-111) IRESP = 0 !-111 is not really an error (metadata has changed)
!
IF (PRESENT(KRESP)) KRESP = IRESP
!
CALL SECOND_MNH2(T22)
TIMEZ%T_READLB_ALL=TIMEZ%T_READLB_ALL + T22 - T11
!
END SUBROUTINE IO_Field_read_byfield_lb

END MODULE MODE_IO_FIELD_READ
