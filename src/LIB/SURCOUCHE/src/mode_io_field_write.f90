!MNH_LIC Copyright 1994-2019 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
! Modifications:
!  J. Escobar  15/09/2015: WENO5 & JPHEXT <> 1
!  P. Wautelet 05/2016-04/2018: new data structures and calls for I/O
!  P. Wautelet 10/01/2019: do not write scalars in Z-split files
!  P. Wautelet 10/01/2019: write header also for Z-split files
!  P. Wautelet 05/03/2019: rename IO subroutines and modules
!  P. Wautelet 10/04/2019: replace ABORT and STOP calls by Print_msg
!  P. Wautelet 12/04/2019: added pointers for C1D, L1D, N1D, X5D and X6D structures in TFIELDDATA
!  P. Wautelet 12/04/2019: use MNHTIME for time measurement variables
!  P. Wautelet 12/07/2019: add support for 1D array of dates
!-----------------------------------------------------------------

#define MNH_SCALARS_IN_SPLITFILES 0

MODULE MODE_IO_FIELD_WRITE

  USE MODD_IO,         ONLY: TFILEDATA, TOUTBAK
  USE MODD_MPIF
  use modd_parameters, only: NMNHNAMELGTMAX
  use modd_precision,  only: MNHINT_MPI, MNHREAL_MPI, MNHTIME

  USE MODE_FIELD
  USE MODE_IO_WRITE_LFI
#if defined(MNH_IOCDF4)
  USE MODE_IO_WRITE_NC4
#endif
  use mode_msg

  IMPLICIT NONE 

  PRIVATE

  public :: IO_Field_write, IO_Field_write_box, IO_Field_write_lb
  public :: IO_Header_write
  public :: IO_Fieldlist_write, IO_Field_user_write

  INTERFACE IO_Field_write
     MODULE PROCEDURE IO_Field_write_byname_X0, IO_Field_write_byname_X1,  &
                      IO_Field_write_byname_X2, IO_Field_write_byname_X3,  &
                      IO_Field_write_byname_X4, IO_Field_write_byname_X5,  &
                      IO_Field_write_byname_X6,                            &
                      IO_Field_write_byname_N0, IO_Field_write_byname_N1,  &
                      IO_Field_write_byname_N2, IO_Field_write_byname_N3,  &
                      IO_Field_write_byname_L0, IO_Field_write_byname_L1,  &
                      IO_Field_write_byname_C0, IO_Field_write_byname_C1,  &
                      IO_Field_write_byname_T0, IO_Field_write_byname_T1,  &
                      IO_Field_write_byfield_X0,IO_Field_write_byfield_X1, &
                      IO_Field_write_byfield_X2,IO_Field_write_byfield_X3, &
                      IO_Field_write_byfield_X4,IO_Field_write_byfield_X5, &
                      IO_Field_write_byfield_X6,                           &
                      IO_Field_write_byfield_N0,IO_Field_write_byfield_N1, &
                      IO_Field_write_byfield_N2,IO_Field_write_byfield_N3, &
                      IO_Field_write_byfield_L0,IO_Field_write_byfield_L1, &
                      IO_Field_write_byfield_C0,IO_Field_write_byfield_C1, &
                      IO_Field_write_byfield_T0,IO_Field_write_byfield_T1
  END INTERFACE

  INTERFACE IO_Field_write_box
     MODULE PROCEDURE IO_Field_write_box_byfield_X5
  END INTERFACE

  INTERFACE IO_Field_write_lb
     MODULE PROCEDURE IO_Field_write_byname_lb, IO_Field_write_byfield_lb
  END INTERFACE

CONTAINS 

  SUBROUTINE IO_Field_metadata_check(TPFIELD,KTYPE,KDIMS,HCALLER)
    TYPE(TFIELDDATA), INTENT(IN) :: TPFIELD ! Field to check
    INTEGER,          INTENT(IN) :: KTYPE   ! Expected datatype
    INTEGER,          INTENT(IN) :: KDIMS   ! Expected number of dimensions
    CHARACTER(LEN=*), INTENT(IN) :: HCALLER ! name of the calling subroutine
    !
    CHARACTER(LEN=2) :: YDIMOK,YDIMKO
    CHARACTER(LEN=8) :: YTYPEOK,YTYPEKO
    !
    IF (TPFIELD%NGRID<0 .OR. TPFIELD%NGRID>8) THEN
      CALL PRINT_MSG(NVERB_WARNING,'IO',HCALLER,'TPFIELD%NGRID is invalid for '//TRIM(TPFIELD%CMNHNAME))
    END IF
    IF (TPFIELD%NTYPE/=KTYPE) THEN
      CALL TYPE_WRITE(KTYPE,YTYPEOK)
      CALL TYPE_WRITE(TPFIELD%NTYPE,YTYPEKO)
      CALL PRINT_MSG(NVERB_WARNING,'IO',HCALLER,&
                     'TPFIELD%NTYPE should be '//YTYPEOK//' instead of '//YTYPEKO//' for '//TRIM(TPFIELD%CMNHNAME))
    END IF
    IF (TPFIELD%NDIMS/=KDIMS) THEN
      WRITE (YDIMOK,'(I2)') KDIMS
      WRITE (YDIMKO,'(I2)') TPFIELD%NDIMS
      CALL PRINT_MSG(NVERB_WARNING,'IO',HCALLER,&
                     'TPFIELD%NDIMS should be '//YDIMOK//' instead of '//YDIMKO//' for '//TRIM(TPFIELD%CMNHNAME))
    END IF
    !
    CONTAINS
    SUBROUTINE TYPE_WRITE(KTYPEINT,HTYPE)
      INTEGER,         INTENT(IN)  :: KTYPEINT
      CHARACTER(LEN=8),INTENT(OUT) :: HTYPE
      !
      SELECT CASE(KTYPEINT)
        CASE(TYPEINT)
          HTYPE = 'TYPEINT'
        CASE(TYPELOG)
          HTYPE = 'TYPELOG'
        CASE(TYPEREAL)
          HTYPE = 'TYPEREAL'
        CASE(TYPECHAR)
          HTYPE = 'TYPECHAR'
        CASE(TYPEDATE)
          HTYPE = 'TYPEDATE'
        CASE DEFAULT
          HTYPE = 'UNKNOWN'
      END SELECT
      !
    END SUBROUTINE TYPE_WRITE
  END SUBROUTINE IO_Field_metadata_check


  SUBROUTINE IO_File_write_check(TPFILE,HSUBR,KRESP)
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
    IF (TPFILE%CMODE/='WRITE') THEN
      CALL PRINT_MSG(NVERB_WARNING,'IO',HSUBR,&
                    TRIM(TPFILE%CNAME)//': writing in a file opened in '//TRIM(TPFILE%CMODE)//' mode')
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
  END SUBROUTINE IO_File_write_check


  SUBROUTINE IO_Format_write_select(TPFILE,OLFI,ONC4)
    TYPE(TFILEDATA), INTENT(IN)  :: TPFILE ! File structure
    LOGICAL,         INTENT(OUT) :: OLFI   ! Write in LFI format?
    LOGICAL,         INTENT(OUT) :: ONC4   ! Write in netCDF format?

    OLFI = .FALSE.
    ONC4 = .FALSE.
    IF (TPFILE%CFORMAT=='LFI'     .OR. TPFILE%CFORMAT=='LFICDF4') OLFI = .TRUE.
    IF (TPFILE%CFORMAT=='NETCDF4' .OR. TPFILE%CFORMAT=='LFICDF4') ONC4 = .TRUE.
  END SUBROUTINE IO_Format_write_select


  SUBROUTINE IO_Header_write(TPFILE,HDAD_NAME)
    TYPE(TFILEDATA),          INTENT(IN)  :: TPFILE   ! File structure
    CHARACTER(LEN=*),OPTIONAL,INTENT(IN)  :: HDAD_NAME

    integer :: ifile

    CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Header_write_FILE','called for file '//TRIM(TPFILE%CNAME))

    CALL IO_Header_onefile_write(TPFILE,HDAD_NAME)

    !Write header also for the Z-split files
    DO IFILE=1,TPFILE%NSUBFILES_IOZ
      CALL IO_Header_onefile_write(TPFILE%TFILES_IOZ(IFILE)%TFILE,HDAD_NAME)
    END DO
  END SUBROUTINE IO_Header_write


  SUBROUTINE IO_Header_onefile_write(TPFILE,HDAD_NAME)
    !
    USE MODD_CONF
    USE MODD_CONF_n,     ONLY: CSTORAGE_TYPE
    USE MODD_PARAMETERS, ONLY: NFILENAMELGTMAXLFI
    !
    TYPE(TFILEDATA),          INTENT(IN)  :: TPFILE   ! File structure
    CHARACTER(LEN=*),OPTIONAL,INTENT(IN)  :: HDAD_NAME
    !
    CHARACTER(LEN=:),ALLOCATABLE :: YDAD_NAME
    INTEGER                      :: ILEN,ILEN2
    !
    CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Header_onefile_write','called for file '//TRIM(TPFILE%CNAME))
    !
    IF ( ASSOCIATED(TPFILE%TDADFILE) .AND. PRESENT(HDAD_NAME) ) THEN
      IF ( TRIM(TPFILE%TDADFILE%CNAME) /= TRIM(HDAD_NAME) ) THEN
        CALL PRINT_MSG(NVERB_ERROR,'IO','IO_Header_onefile_write','TPFILE%TDADFILE%CNAME /= HDAD_NAME')
      END IF
    END IF
    !
    CALL IO_Header_write_nc4(TPFILE)
    !
    CALL IO_Field_write(TPFILE,'MNHVERSION',  NMNHVERSION)
    CALL IO_Field_write(TPFILE,'MASDEV',      NMASDEV)
    CALL IO_Field_write(TPFILE,'BUGFIX',      NBUGFIX)
    CALL IO_Field_write(TPFILE,'BIBUSER',     CBIBUSER)
    CALL IO_Field_write(TPFILE,'PROGRAM',     CPROGRAM)
    CALL IO_Field_write(TPFILE,'STORAGE_TYPE',CSTORAGE_TYPE)
    CALL IO_Field_write(TPFILE,'MY_NAME',     TPFILE%CNAME)
    !
    IF ( ASSOCIATED(TPFILE%TDADFILE) ) THEN
      ILEN  = LEN_TRIM(TPFILE%TDADFILE%CNAME)
      ILEN2 = MAX(NFILENAMELGTMAXLFI,ILEN)
      ALLOCATE(CHARACTER(LEN=ILEN2) :: YDAD_NAME)
      IF(ILEN>0) THEN
        YDAD_NAME(1:ILEN) = TPFILE%TDADFILE%CNAME(1:ILEN)
        YDAD_NAME(ILEN+1:ILEN2) = ' '
      ELSE
        YDAD_NAME(:) = ' '
      END IF
    ELSE IF (PRESENT(HDAD_NAME)) THEN
      ILEN  = LEN_TRIM(HDAD_NAME)
      ILEN2 = MAX(NFILENAMELGTMAXLFI,ILEN)
      ALLOCATE(CHARACTER(LEN=ILEN2) :: YDAD_NAME)
      IF(ILEN>0) THEN
        YDAD_NAME(1:ILEN) = HDAD_NAME(1:ILEN)
        YDAD_NAME(ILEN+1:ILEN2) = ' '
      ELSE
        YDAD_NAME(:) = ' '
      END IF
    ELSE
      CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Header_onefile_write',TRIM(TPFILE%CNAME)// &
                     ': TPFILE%TDADFILE not associated and HDAD_NAME not provided')
      ALLOCATE(CHARACTER(LEN=NFILENAMELGTMAXLFI) :: YDAD_NAME)
      YDAD_NAME(:) = ' '
    ENDIF
    CALL IO_Field_write(TPFILE,'DAD_NAME',YDAD_NAME)
    DEALLOCATE(YDAD_NAME)
    !
  END SUBROUTINE IO_Header_onefile_write


  SUBROUTINE IO_Field_write_byname_X0(TPFILE,HNAME,PFIELD,KRESP)
    !
    !*      0.1   Declarations of arguments
    !
    TYPE(TFILEDATA),           INTENT(IN) :: TPFILE
    CHARACTER(LEN=*),          INTENT(IN) :: HNAME    ! name of the field to write
    REAL,                      INTENT(IN) :: PFIELD   ! array containing the data field
    INTEGER,OPTIONAL,          INTENT(OUT):: KRESP    ! return-code 
    !
    !*      0.2   Declarations of local variables
    !
    INTEGER :: ID ! Index of the field
    INTEGER :: IRESP ! return_code
    !
    CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_write_byname_X0',TRIM(TPFILE%CNAME)//': writing '//TRIM(HNAME))
    !
    CALL FIND_FIELD_ID_FROM_MNHNAME(HNAME,ID,IRESP)
    !
    IF(IRESP==0) CALL IO_Field_write(TPFILE,TFIELDLIST(ID),PFIELD,IRESP)
    !
    IF (PRESENT(KRESP)) KRESP = IRESP
    !
  END SUBROUTINE IO_Field_write_byname_X0


  SUBROUTINE IO_Field_write_byfield_X0(TPFILE,TPFIELD,PFIELD,KRESP)
    USE MODD_IO,               ONLY: GSMONOPROC, ISP
    !
    USE MODE_IO_MANAGE_STRUCT, ONLY: IO_File_find_byname
    !
    IMPLICIT NONE
    !
    !*      0.1   Declarations of arguments
    !
    TYPE(TFILEDATA),             INTENT(IN) :: TPFILE
    TYPE(TFIELDDATA),            INTENT(IN) :: TPFIELD
    REAL,TARGET,                 INTENT(IN) :: PFIELD   ! array containing the data field
    INTEGER,OPTIONAL,            INTENT(OUT):: KRESP    ! return-code 
    !
    !*      0.2   Declarations of local variables
    !
    CHARACTER(LEN=28)                        :: YFILEM   ! FM-file name
    CHARACTER(LEN=NMNHNAMELGTMAX)            :: YRECFM   ! name of the article to write
    CHARACTER(LEN=2)                         :: YDIR     ! field form
    INTEGER                                  :: IERR
    INTEGER                                  :: IRESP
    !
    INTEGER                                  :: IK_FILE
    TYPE(TFILEDATA),POINTER                  :: TZFILE
    LOGICAL                                  :: GLFI, GNC4
    CHARACTER(LEN=:),ALLOCATABLE             :: YMSG
    CHARACTER(LEN=6)                         :: YRESP
    !
    YFILEM   = TPFILE%CNAME
    YRECFM   = TPFIELD%CMNHNAME
    YDIR     = TPFIELD%CDIR
    !
    IRESP = 0
    TZFILE => NULL()
    !
    CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_write_byfield_X0',TRIM(YFILEM)//': writing '//TRIM(YRECFM))
    !
    CALL IO_Field_metadata_check(TPFIELD,TYPEREAL,0,'IO_Field_write_byfield_X0')
    !
    CALL IO_File_write_check(TPFILE,'IO_Field_write_byfield_X0',IRESP)
    !
    CALL IO_Format_write_select(TPFILE,GLFI,GNC4)
    !
    IF (IRESP==0) THEN
       IF (GSMONOPROC) THEN ! sequential execution
          IF (GLFI) CALL IO_Field_write_lfi(TPFILE,TPFIELD,PFIELD,IRESP)
          IF (GNC4) CALL IO_Field_write_nc4(TPFILE,TPFIELD,PFIELD,IRESP)
       ELSE ! multiprocesses execution
          IF (ISP == TPFILE%NMASTER_RANK)  THEN
             IF (GLFI) CALL IO_Field_write_lfi(TPFILE,TPFIELD,PFIELD,IRESP)
             IF (GNC4) CALL IO_Field_write_nc4(TPFILE,TPFIELD,PFIELD,IRESP)
          END IF
          !
          CALL MPI_BCAST(IRESP,1,MNHINT_MPI,TPFILE%NMASTER_RANK-1,TPFILE%NMPICOMM,IERR)
       END IF ! multiprocesses execution
#if MNH_SCALARS_IN_SPLITFILES
       IF (TPFILE%NSUBFILES_IOZ>0) THEN
          ! write the data in all Z files
          DO IK_FILE=1,TPFILE%NSUBFILES_IOZ
             TZFILE => TPFILE%TFILES_IOZ(IK_FILE)%TFILE
             IF ( ISP == TZFILE%NMASTER_RANK )  THEN
                IF (GLFI) CALL IO_Field_write_lfi(TZFILE,TPFIELD,PFIELD,IRESP)
                IF (GNC4) CALL IO_Field_write_nc4(TZFILE,TPFIELD,PFIELD,IRESP)
             END IF
          END DO
       ENDIF
#endif
    END IF
    !
    IF (IRESP.NE.0) THEN
      WRITE(YRESP, '( I6 )') IRESP
      YMSG = 'RESP='//YRESP//' when writing '//TRIM(YRECFM)//' in '//TRIM(TPFILE%CNAME)
      CALL PRINT_MSG(NVERB_ERROR,'IO','IO_Field_write_byfield_X0',YMSG)
    END IF
    IF (PRESENT(KRESP)) KRESP = IRESP
  END SUBROUTINE IO_Field_write_byfield_X0


  SUBROUTINE IO_Field_write_byname_X1(TPFILE,HNAME,PFIELD,KRESP)
    !
    !*      0.1   Declarations of arguments
    !
    TYPE(TFILEDATA),           INTENT(IN) :: TPFILE
    CHARACTER(LEN=*),          INTENT(IN) :: HNAME    ! name of the field to write
    REAL,DIMENSION(:),         INTENT(IN) :: PFIELD   ! array containing the data field
    INTEGER,OPTIONAL,          INTENT(OUT):: KRESP    ! return-code 
    !
    !*      0.2   Declarations of local variables
    !
    INTEGER :: ID ! Index of the field
    INTEGER :: IRESP ! return-code 
    !
    CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_write_byname_X1',TRIM(TPFILE%CNAME)//': writing '//TRIM(HNAME))
    !
    CALL FIND_FIELD_ID_FROM_MNHNAME(HNAME,ID,IRESP)
    !
    IF(IRESP==0) CALL IO_Field_write(TPFILE,TFIELDLIST(ID),PFIELD,IRESP)
    !
    IF (PRESENT(KRESP)) KRESP = IRESP
    !
  END SUBROUTINE IO_Field_write_byname_X1


  SUBROUTINE IO_Field_write_byfield_X1(TPFILE,TPFIELD,PFIELD,KRESP)
    USE MODD_IO,               ONLY: GSMONOPROC, ISP
    !
    USE MODE_ALLOCBUFFER_ll
    USE MODE_GATHER_ll
    USE MODE_IO_MANAGE_STRUCT, ONLY: IO_File_find_byname
    !
    IMPLICIT NONE
    !
    !*      0.1   Declarations of arguments
    !
    TYPE(TFILEDATA),             INTENT(IN) :: TPFILE
    TYPE(TFIELDDATA),            INTENT(IN) :: TPFIELD
    REAL,DIMENSION(:),TARGET,    INTENT(IN) :: PFIELD   ! array containing the data field
    INTEGER,OPTIONAL,            INTENT(OUT):: KRESP    ! return-code 
    !
    !*      0.2   Declarations of local variables
    !
    CHARACTER(LEN=28)                        :: YFILEM   ! FM-file name
    CHARACTER(LEN=NMNHNAMELGTMAX)            :: YRECFM   ! name of the article to write
    CHARACTER(LEN=2)                         :: YDIR     ! field form
    INTEGER                                  :: IERR
    INTEGER                                  :: IRESP
    INTEGER                                  :: ISIZEMAX
    REAL,DIMENSION(:),POINTER                :: ZFIELDP
    LOGICAL                                  :: GALLOC
    LOGICAL                                  :: GLFI, GNC4
    !
    CHARACTER(LEN=:),ALLOCATABLE             :: YMSG
    CHARACTER(LEN=6)                         :: YRESP
    !
    IRESP = 0
    GALLOC = .FALSE.
    !
    YFILEM   = TPFILE%CNAME
    YRECFM   = TPFIELD%CMNHNAME
    YDIR     = TPFIELD%CDIR
    !
    CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_write_byfield_X1',TRIM(YFILEM)//': writing '//TRIM(YRECFM))
    !
    CALL IO_Field_metadata_check(TPFIELD,TYPEREAL,1,'IO_Field_write_byfield_X1')
    !
    CALL IO_File_write_check(TPFILE,'IO_Field_write_byfield_X1',IRESP)
    !
    CALL IO_Format_write_select(TPFILE,GLFI,GNC4)
    !
    IF (IRESP==0) THEN
       IF (GSMONOPROC) THEN ! sequential execution
          IF (GLFI) CALL IO_Field_write_lfi(TPFILE,TPFIELD,PFIELD,IRESP)
          IF (GNC4) CALL IO_Field_write_nc4(TPFILE,TPFIELD,PFIELD,IRESP)
       ELSE ! multiprocesses execution
          CALL MPI_ALLREDUCE(SIZE(PFIELD),ISIZEMAX,1,MNHINT_MPI,MPI_MAX,TPFILE%NMPICOMM,IRESP)
          IF (ISIZEMAX==0) THEN
             CALL PRINT_MSG(NVERB_INFO,'IO','IO_Field_write_byfield_X1','ignoring variable with a zero size ('//TRIM(YRECFM)//')')
             IF (PRESENT(KRESP)) KRESP=0
             RETURN
          END IF

          IF (ISP == TPFILE%NMASTER_RANK)  THEN
             CALL ALLOCBUFFER_ll(ZFIELDP,PFIELD,YDIR,GALLOC)
          ELSE
             ALLOCATE(ZFIELDP(0))
             GALLOC = .TRUE.
          END IF
          !
          IF (YDIR == 'XX' .OR. YDIR =='YY') THEN
             CALL GATHER_XXFIELD(YDIR,PFIELD,ZFIELDP,TPFILE%NMASTER_RANK,TPFILE%NMPICOMM)
          END IF
          !
          IF (ISP == TPFILE%NMASTER_RANK)  THEN
             IF (GLFI) CALL IO_Field_write_lfi(TPFILE,TPFIELD,ZFIELDP,IRESP)
             IF (GNC4) CALL IO_Field_write_nc4(TPFILE,TPFIELD,ZFIELDP,IRESP)
          END IF
          !
          CALL MPI_BCAST(IRESP,1,MNHINT_MPI,TPFILE%NMASTER_RANK-1,TPFILE%NMPICOMM,IERR)
       END IF ! multiprocesses execution
    END IF
    !
    IF (IRESP.NE.0) THEN
      WRITE(YRESP, '( I6 )') IRESP
      YMSG = 'RESP='//YRESP//' when writing '//TRIM(YRECFM)//' in '//TRIM(TPFILE%CNAME)
      CALL PRINT_MSG(NVERB_ERROR,'IO','IO_Field_write_byfield_X1',YMSG)
    END IF
    IF (GALLOC) DEALLOCATE(ZFIELDP)
    IF (PRESENT(KRESP)) KRESP = IRESP
  END SUBROUTINE IO_Field_write_byfield_X1


  SUBROUTINE IO_Field_write_byname_X2(TPFILE,HNAME,PFIELD,KRESP)
    !
    !*      0.1   Declarations of arguments
    !
    TYPE(TFILEDATA),           INTENT(IN) :: TPFILE
    CHARACTER(LEN=*),          INTENT(IN) :: HNAME    ! name of the field to write
    REAL,DIMENSION(:,:),       INTENT(IN) :: PFIELD   ! array containing the data field
    INTEGER,OPTIONAL,          INTENT(OUT):: KRESP    ! return-code 
    !
    !*      0.2   Declarations of local variables
    !
    INTEGER :: ID ! Index of the field
    INTEGER :: IRESP ! return-code 
    !
    CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_write_byname_X2',TRIM(TPFILE%CNAME)//': writing '//TRIM(HNAME))
    !
    CALL FIND_FIELD_ID_FROM_MNHNAME(HNAME,ID,IRESP)
    !
    IF(IRESP==0) CALL IO_Field_write(TPFILE,TFIELDLIST(ID),PFIELD,IRESP)
    !
    IF (PRESENT(KRESP)) KRESP = IRESP
    !
  END SUBROUTINE IO_Field_write_byname_X2


  SUBROUTINE IO_Field_write_byfield_X2(TPFILE,TPFIELD,PFIELD,KRESP)
    USE MODD_IO,            ONLY: GSMONOPROC,ISP,L1D,L2D,LPACK
    USE MODD_PARAMETERS_ll, ONLY: JPHEXT
    USE MODD_TIMEZ,         ONLY: TIMEZ
    !
    USE MODE_ALLOCBUFFER_ll
#ifdef MNH_GA
    USE MODE_GA
#endif 
    USE MODE_GATHER_ll
    USE MODE_MNH_TIMING,    ONLY: SECOND_MNH2
    !
    IMPLICIT NONE
    !
    !*      0.1   Declarations of arguments
    !
    TYPE(TFILEDATA),             INTENT(IN) :: TPFILE
    TYPE(TFIELDDATA),            INTENT(IN) :: TPFIELD
    REAL,DIMENSION(:,:),TARGET,  INTENT(IN) :: PFIELD   ! array containing the data field
    INTEGER,OPTIONAL,            INTENT(OUT):: KRESP    ! return-code 
    !
    !*      0.2   Declarations of local variables
    !
    CHARACTER(LEN=28)                        :: YFILEM   ! FM-file name
    CHARACTER(LEN=NMNHNAMELGTMAX)            :: YRECFM   ! name of the article to write
    CHARACTER(LEN=2)                         :: YDIR     ! field form
    INTEGER                                  :: IERR
    INTEGER                                  :: ISIZEMAX
    INTEGER                                  :: IRESP
    REAL,DIMENSION(:,:),POINTER              :: ZFIELDP
    LOGICAL                                  :: GALLOC
    LOGICAL                                  :: GLFI, GNC4
    !
    REAL(kind=MNHTIME), DIMENSION(2)         :: T0, T1, T2
    REAL(kind=MNHTIME), DIMENSION(2)         :: T11, T22
#ifdef MNH_GA
    REAL,DIMENSION(:,:),POINTER              :: ZFIELD_GA
#endif
    INTEGER                                  :: IHEXTOT
    CHARACTER(LEN=:),ALLOCATABLE             :: YMSG
    CHARACTER(LEN=6)                         :: YRESP
    !
    YFILEM   = TPFILE%CNAME
    YRECFM   = TPFIELD%CMNHNAME
    YDIR     = TPFIELD%CDIR
    !
    IRESP = 0
    GALLOC = .FALSE.
    IHEXTOT = 2*JPHEXT+1
    !
    CALL SECOND_MNH2(T11)
    !
    CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_write_byfield_X2',TRIM(YFILEM)//': writing '//TRIM(YRECFM))
    !
    CALL IO_Field_metadata_check(TPFIELD,TYPEREAL,2,'IO_Field_write_byfield_X2')
    !
    CALL IO_File_write_check(TPFILE,'IO_Field_write_byfield_X2',IRESP)
    !
    CALL IO_Format_write_select(TPFILE,GLFI,GNC4)
    !
    IF (IRESP==0) THEN
       IF (GSMONOPROC) THEN ! sequential execution
          !    IF (LPACK .AND. L1D .AND. YDIR=='XY') THEN 
          IF (LPACK .AND. L1D .AND. SIZE(PFIELD,1)==IHEXTOT .AND. SIZE(PFIELD,2)==IHEXTOT) THEN 
             ZFIELDP=>PFIELD(JPHEXT+1:JPHEXT+1,JPHEXT+1:JPHEXT+1)
             IF (GLFI) CALL IO_Field_write_lfi(TPFILE,TPFIELD,ZFIELDP,IRESP)
             IF (GNC4) CALL IO_Field_write_nc4(TPFILE,TPFIELD,ZFIELDP,IRESP)
             !    ELSE IF (LPACK .AND. L2D .AND. YDIR=='XY') THEN
          ELSEIF (LPACK .AND. L2D .AND. SIZE(PFIELD,2)==IHEXTOT) THEN
             ZFIELDP=>PFIELD(:,JPHEXT+1:JPHEXT+1)
             IF (GLFI) CALL IO_Field_write_lfi(TPFILE,TPFIELD,ZFIELDP,IRESP)
             IF (GNC4) CALL IO_Field_write_nc4(TPFILE,TPFIELD,ZFIELDP,IRESP)
          ELSE
             IF (GLFI) CALL IO_Field_write_lfi(TPFILE,TPFIELD,PFIELD,IRESP)
             IF (GNC4) CALL IO_Field_write_nc4(TPFILE,TPFIELD,PFIELD,IRESP)
          END IF
       ELSE ! multiprocesses execution
          CALL SECOND_MNH2(T0)
          CALL MPI_ALLREDUCE(SIZE(PFIELD),ISIZEMAX,1,MNHINT_MPI,MPI_MAX,TPFILE%NMPICOMM,IRESP)
          IF (ISIZEMAX==0) THEN
             CALL PRINT_MSG(NVERB_INFO,'IO','IO_Field_write_byfield_X2','ignoring variable with a zero size ('//TRIM(YRECFM)//')')
             IF (PRESENT(KRESP)) KRESP=0
             RETURN
          END IF

          IF (ISP == TPFILE%NMASTER_RANK)  THEN
             ! I/O process case
             CALL ALLOCBUFFER_ll(ZFIELDP,PFIELD,YDIR,GALLOC)
          ELSE
             ALLOCATE(ZFIELDP(0,0))
             GALLOC = .TRUE.
          END IF
          !   
          IF (YDIR == 'XX' .OR. YDIR =='YY') THEN
             CALL GATHER_XXFIELD(YDIR,PFIELD,ZFIELDP,TPFILE%NMASTER_RANK,TPFILE%NMPICOMM)
          ELSEIF (YDIR == 'XY') THEN
             IF (LPACK .AND. L2D) THEN
                CALL GATHER_XXFIELD('XX',PFIELD(:,JPHEXT+1),ZFIELDP(:,1),TPFILE%NMASTER_RANK,TPFILE%NMPICOMM)
             ELSE
#ifdef MNH_GA
          !
          ! init/create the ga , dim3 = 1
          !
          CALL MNH_INIT_GA(SIZE(PFIELD,1),SIZE(PFIELD,2),1,YRECFM,"WRITE")
         !
         !   copy columun data to global arrays g_a 
         !
         ALLOCATE (ZFIELD_GA (SIZE(PFIELD,1),SIZE(PFIELD,2)))
         ZFIELD_GA = PFIELD
         call nga_put(g_a, lo_col, hi_col,ZFIELD_GA(NIXO_L,NIYO_L) , ld_col)  
         call ga_sync
         DEALLOCATE (ZFIELD_GA)
         IF (ISP == TPFILE%NMASTER_RANK) THEN
            !
            ! this proc get the  Z slide to write
            !
            lo_zplan(JPIZ) = 1
            hi_zplan(JPIZ) = 1
            call nga_get(g_a, lo_zplan, hi_zplan,ZFIELDP, ld_zplan)
         END IF
#else
         CALL GATHER_XYFIELD(PFIELD,ZFIELDP,TPFILE%NMASTER_RANK,TPFILE%NMPICOMM)
#endif
             END IF
          END IF
          CALL SECOND_MNH2(T1)
          TIMEZ%T_WRIT2D_GATH=TIMEZ%T_WRIT2D_GATH + T1 - T0
          !
          IF (ISP == TPFILE%NMASTER_RANK) THEN
             IF (GLFI) CALL IO_Field_write_lfi(TPFILE,TPFIELD,ZFIELDP,IRESP)
             IF (GNC4) CALL IO_Field_write_nc4(TPFILE,TPFIELD,ZFIELDP,IRESP)
          END IF
#ifdef MNH_GA
         call ga_sync
#endif     
          CALL SECOND_MNH2(T2)
          TIMEZ%T_WRIT2D_WRIT=TIMEZ%T_WRIT2D_WRIT + T2 - T1
          !
          CALL MPI_BCAST(IRESP,1,MNHINT_MPI,TPFILE%NMASTER_RANK-1,TPFILE%NMPICOMM,IERR)
       END IF
    END IF
    !
    IF (IRESP.NE.0) THEN
      WRITE(YRESP, '( I6 )') IRESP
      YMSG = 'RESP='//YRESP//' when writing '//TRIM(YRECFM)//' in '//TRIM(TPFILE%CNAME)
      CALL PRINT_MSG(NVERB_ERROR,'IO','IO_Field_write_byfield_X2',YMSG)
    END IF
    IF (GALLOC) DEALLOCATE(ZFIELDP)
    IF (PRESENT(KRESP)) KRESP = IRESP
    CALL SECOND_MNH2(T22)
    TIMEZ%T_WRIT2D_ALL=TIMEZ%T_WRIT2D_ALL + T22 - T11
  END SUBROUTINE IO_Field_write_byfield_X2


  SUBROUTINE IO_Field_write_byname_X3(TPFILE,HNAME,PFIELD,KRESP)
    !
    !*      0.1   Declarations of arguments
    !
    TYPE(TFILEDATA),             INTENT(IN) :: TPFILE
    CHARACTER(LEN=*),            INTENT(IN) :: HNAME    ! name of the field to write
    REAL,DIMENSION(:,:,:),       INTENT(IN) :: PFIELD   ! array containing the data field
    INTEGER,OPTIONAL,            INTENT(OUT):: KRESP    ! return-code 
    !
    !*      0.2   Declarations of local variables
    !
    INTEGER :: ID ! Index of the field
    INTEGER :: IRESP ! return_code
    !
    CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_write_byname_X3',TRIM(TPFILE%CNAME)//': writing '//TRIM(HNAME))
    !
    CALL FIND_FIELD_ID_FROM_MNHNAME(HNAME,ID,IRESP)
    !
    IF(IRESP==0) CALL IO_Field_write(TPFILE,TFIELDLIST(ID),PFIELD,IRESP)
    !
    IF (PRESENT(KRESP)) KRESP = IRESP
    !
  END SUBROUTINE IO_Field_write_byname_X3


  SUBROUTINE IO_Field_write_byfield_X3(TPFILE,TPFIELD,PFIELD,KRESP)
    USE MODD_IO,               ONLY: GSMONOPROC, ISNPROC, ISP, L1D, L2D, LPACK
    USE MODD_PARAMETERS_ll,    ONLY: JPHEXT
    USE MODD_TIMEZ,            ONLY: TIMEZ
    USE MODD_VAR_ll,           ONLY: MNH_STATUSES_IGNORE
    !
    USE MODE_ALLOCBUFFER_ll
#ifdef MNH_GA
    USE MODE_GA
#endif
    USE MODE_GATHER_ll
    USE MODE_IO_MANAGE_STRUCT, ONLY: IO_File_find_byname
    USE MODE_IO_TOOLS,         ONLY: IO_Level2filenumber_get
    USE MODE_MNH_TIMING,       ONLY: SECOND_MNH2
    !
    !
    !*      0.1   Declarations of arguments
    !
    TYPE(TFILEDATA),TARGET,      INTENT(IN) :: TPFILE
    TYPE(TFIELDDATA),            INTENT(IN) :: TPFIELD
    REAL,DIMENSION(:,:,:),TARGET,INTENT(IN) :: PFIELD   ! array containing the data field
    INTEGER,OPTIONAL,            INTENT(OUT):: KRESP    ! return-code 
    !
    !*      0.2   Declarations of local variables
    !
    CHARACTER(LEN=28)                        :: YFILEM   ! FM-file name
    CHARACTER(LEN=NMNHNAMELGTMAX)            :: YRECFM   ! name of the article to write
    CHARACTER(LEN=2)                         :: YDIR     ! field form
    INTEGER                                  :: IERR
    INTEGER                                  :: ISIZEMAX
    INTEGER                                  :: IRESP
    REAL,DIMENSION(:,:,:),POINTER            :: ZFIELDP
    LOGICAL                                  :: GALLOC
    LOGICAL                                  :: GLFI, GNC4
    INTEGER                                  :: JK,JKK
    REAL,DIMENSION(:,:),POINTER              :: ZSLICE_ll,ZSLICE
    INTEGER                                  :: IK_FILE,IK_RANK,INB_PROC_REAL,JK_MAX
    INTEGER                                  :: JI,IXO,IXE,IYO,IYE
    REAL,DIMENSION(:,:),POINTER              :: TX2DP
    INTEGER, DIMENSION(MPI_STATUS_SIZE)      :: STATUS
    LOGICAL                                  :: GALLOC_ll
    INTEGER,ALLOCATABLE,DIMENSION(:)         :: REQ_TAB
    INTEGER                                  :: NB_REQ
    TYPE TX_2DP
       REAL, DIMENSION(:,:), POINTER :: X
    END TYPE TX_2DP
    TYPE(TX_2DP),ALLOCATABLE,DIMENSION(:)    :: T_TX2DP
    REAL(kind=MNHTIME), DIMENSION(2)         :: T0, T1, T2
    REAL(kind=MNHTIME), DIMENSION(2)         :: T11, T22
#ifdef MNH_GA
    REAL,DIMENSION(:,:,:),POINTER            :: ZFIELD_GA
#endif
    INTEGER                                  :: IHEXTOT
    CHARACTER(LEN=:),ALLOCATABLE             :: YMSG
    CHARACTER(LEN=6)                         :: YRESP
    TYPE(TFILEDATA),POINTER                  :: TZFILE
    !
    TZFILE => NULL()
    !
    ZSLICE    => NULL()
    ZSLICE_ll => NULL()
    !
    YFILEM   = TPFILE%CNAME
    YRECFM   = TPFIELD%CMNHNAME
    YDIR     = TPFIELD%CDIR
    !
    IRESP = 0
    GALLOC    = .FALSE.
    GALLOC_ll = .FALSE.
    IHEXTOT = 2*JPHEXT+1
    !
    CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_write_byfield_X3',TRIM(YFILEM)//': writing '//TRIM(YRECFM))
    !
    CALL SECOND_MNH2(T11)
    !
    CALL IO_Field_metadata_check(TPFIELD,TYPEREAL,3,'IO_Field_write_byfield_X3')
    !
    CALL IO_File_write_check(TPFILE,'IO_Field_write_byfield_X3',IRESP)
    !
    CALL IO_Format_write_select(TPFILE,GLFI,GNC4)
    !
    IF (IRESP==0) THEN
       IF (GSMONOPROC .AND. TPFILE%NSUBFILES_IOZ==0 ) THEN ! sequential execution
          !    IF (LPACK .AND. L1D .AND. YDIR=='XY') THEN 
          IF (LPACK .AND. L1D .AND. SIZE(PFIELD,1)==IHEXTOT .AND. SIZE(PFIELD,2)==IHEXTOT) THEN 
             ZFIELDP=>PFIELD(JPHEXT+1:JPHEXT+1,JPHEXT+1:JPHEXT+1,:)
             IF (GLFI) CALL IO_Field_write_lfi(TPFILE,TPFIELD,ZFIELDP,IRESP)
             IF (GNC4) CALL IO_Field_write_nc4(TPFILE,TPFIELD,ZFIELDP,IRESP)
             !    ELSE IF (LPACK .AND. L2D .AND. YDIR=='XY') THEN
          ELSEIF (LPACK .AND. L2D .AND. SIZE(PFIELD,2)==IHEXTOT) THEN
             ZFIELDP=>PFIELD(:,JPHEXT+1:JPHEXT+1,:)
             IF (GLFI) CALL IO_Field_write_lfi(TPFILE,TPFIELD,ZFIELDP,IRESP)
             IF (GNC4) CALL IO_Field_write_nc4(TPFILE,TPFIELD,ZFIELDP,IRESP)
          ELSE
             IF (GLFI) CALL IO_Field_write_lfi(TPFILE,TPFIELD,PFIELD,IRESP)
             IF (GNC4) CALL IO_Field_write_nc4(TPFILE,TPFIELD,PFIELD,IRESP)
          END IF
       ELSEIF ( TPFILE%NSUBFILES_IOZ==0 .OR. YDIR=='--' ) THEN  ! multiprocesses execution & 1 proc IO
          CALL MPI_ALLREDUCE(SIZE(PFIELD),ISIZEMAX,1,MNHINT_MPI,MPI_MAX,TPFILE%NMPICOMM,IRESP)
          IF (ISIZEMAX==0) THEN
             CALL PRINT_MSG(NVERB_INFO,'IO','IO_Field_write_byfield_X3','ignoring variable with a zero size ('//TRIM(YRECFM)//')')
             IF (PRESENT(KRESP)) KRESP=0
             RETURN
          END IF

          ! write 3D field in 1 time = output for graphique
          IF (ISP == TPFILE%NMASTER_RANK)  THEN
             CALL ALLOCBUFFER_ll(ZFIELDP,PFIELD,YDIR,GALLOC)
          ELSE
             ALLOCATE(ZFIELDP(0,0,0))
             GALLOC = .TRUE.
          END IF
          !
          IF (YDIR == 'XX' .OR. YDIR =='YY') THEN
             CALL GATHER_XXFIELD(YDIR,PFIELD,ZFIELDP,TPFILE%NMASTER_RANK,TPFILE%NMPICOMM)
          ELSEIF (YDIR == 'XY') THEN
             IF (LPACK .AND. L2D) THEN
                CALL GATHER_XXFIELD('XX',PFIELD(:,JPHEXT+1,:),ZFIELDP(:,1,:),TPFILE%NMASTER_RANK,TPFILE%NMPICOMM)
             ELSE
                CALL GATHER_XYFIELD(PFIELD,ZFIELDP,TPFILE%NMASTER_RANK,TPFILE%NMPICOMM)
             END IF
          END IF
          !
          IF (ISP == TPFILE%NMASTER_RANK)  THEN
             IF (GLFI) CALL IO_Field_write_lfi(TPFILE,TPFIELD,ZFIELDP,IRESP)
             IF (GNC4) CALL IO_Field_write_nc4(TPFILE,TPFIELD,ZFIELDP,IRESP)
          END IF
          !
          CALL MPI_BCAST(IRESP,1,MNHINT_MPI,TPFILE%NMASTER_RANK-1,TPFILE%NMPICOMM,IERR)
          !
       ELSE ! multiprocesses execution & // IO
          CALL MPI_ALLREDUCE(SIZE(PFIELD),ISIZEMAX,1,MNHINT_MPI,MPI_MAX,TPFILE%NMPICOMM,IRESP)
          IF (ISIZEMAX==0) THEN
             CALL PRINT_MSG(NVERB_INFO,'IO','IO_Field_write_byfield_X3','ignoring variable with a zero size ('//TRIM(YRECFM)//')')
             IF (PRESENT(KRESP)) KRESP=0
             RETURN
          END IF
          !
          !JUAN BG Z SLICE
          !
          !
#ifdef MNH_GA
          !
          ! init/create the ga
          !
          CALL SECOND_MNH2(T0)
          CALL MNH_INIT_GA(SIZE(PFIELD,1),SIZE(PFIELD,2),SIZE(PFIELD,3),YRECFM,"WRITE")
         !
         !   copy columun data to global arrays g_a 
         !
         ALLOCATE (ZFIELD_GA (SIZE(PFIELD,1),SIZE(PFIELD,2),SIZE(PFIELD,3)))
         ZFIELD_GA = PFIELD
         call nga_put(g_a, lo_col, hi_col,ZFIELD_GA(NIXO_L,NIYO_L,1) , ld_col)  
         DEALLOCATE(ZFIELD_GA)
         call ga_sync
         CALL SECOND_MNH2(T1)
         TIMEZ%T_WRIT3D_SEND=TIMEZ%T_WRIT3D_SEND + T1 - T0
         !
         ! Write the variable attributes in the non-split file
         !
         if ( tpfile%nmaster_rank==isp .and. gnc4 ) &
           call IO_Write_field_header_split_nc4( tpfile, tpfield, size( pfield, 3 ) )
         !
         ! write the data
         !
         ALLOCATE(ZSLICE_ll(0,0)) ! to avoid bug on test of size
         GALLOC_ll = .TRUE.
         !
         DO JKK=1,IKU_ll
            !
            IK_FILE = IO_Level2filenumber_get(JKK,TPFILE%NSUBFILES_IOZ)
            TZFILE => TPFILE%TFILES_IOZ(IK_FILE+1)%TFILE
            !
            IK_RANK = TZFILE%NMASTER_RANK
            !
            IF (ISP == IK_RANK )  THEN 
               CALL SECOND_MNH2(T0)
               !
               IF ( SIZE(ZSLICE_ll) .EQ. 0 ) THEN
                  DEALLOCATE(ZSLICE_ll)
                  CALL ALLOCBUFFER_ll(ZSLICE_ll,ZSLICE,YDIR,GALLOC_ll)
               END IF
               !
               ! this proc get this JKK slide
               !
               lo_zplan(JPIZ) = JKK
               hi_zplan(JPIZ) = JKK
               call nga_get(g_a, lo_zplan, hi_zplan,ZSLICE_ll, ld_zplan)
               CALL SECOND_MNH2(T1)
               TIMEZ%T_WRIT3D_RECV=TIMEZ%T_WRIT3D_RECV + T1 - T0
               !
               IF (GLFI) CALL IO_Field_write_lfi(TPFILE,TPFIELD,ZSLICE_ll,IRESP,KVERTLEVEL=JKK,KZFILE=IK_FILE+1)
               IF (GNC4) CALL IO_Field_write_nc4(TPFILE,TPFIELD,ZSLICE_ll,IRESP,KVERTLEVEL=JKK,KZFILE=IK_FILE+1)
               CALL SECOND_MNH2(T2)
               TIMEZ%T_WRIT3D_WRIT=TIMEZ%T_WRIT3D_WRIT + T2 - T1
            END IF
         END DO
         !
         CALL SECOND_MNH2(T0) 
         call ga_sync
         CALL SECOND_MNH2(T1) 
         TIMEZ%T_WRIT3D_WAIT=TIMEZ%T_WRIT3D_WAIT + T1 - T0     
#else
          !
          ALLOCATE(ZSLICE_ll(0,0))
          GALLOC_ll = .TRUE.
          INB_PROC_REAL = MIN(TPFILE%NSUBFILES_IOZ,ISNPROC)
          Z_SLICE: DO JK=1,SIZE(PFIELD,3),INB_PROC_REAL
             !
             ! collect the data
             !
             JK_MAX=MIN(SIZE(PFIELD,3),JK+INB_PROC_REAL-1)
             !
             NB_REQ=0
             ALLOCATE(REQ_TAB(INB_PROC_REAL))
             ALLOCATE(T_TX2DP(INB_PROC_REAL))
             DO JKK=JK,JK_MAX
                !
                ! get the file & rank to write this level
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
                IF (YDIR == 'XX' .OR. YDIR =='YY') THEN
                   call Print_msg( NVERB_FATAL, 'GEN', 'IO_Field_write_byfield_X3', 'XX not yet planned on Blue Gene' )
                   CALL GATHER_XXFIELD(YDIR,PFIELD,ZFIELDP,TPFILE%NMASTER_RANK,TPFILE%NMPICOMM)
                ELSEIF (YDIR == 'XY') THEN
                   IF (LPACK .AND. L2D) THEN
                      call Print_msg( NVERB_FATAL, 'GEN', 'IO_Field_write_byfield_X3', 'L2D not yet planned on Blue Gene' )
                      CALL GATHER_XXFIELD('XX',PFIELD(:,JPHEXT+1,:),ZFIELDP(:,1,:),TPFILE%NMASTER_RANK,TPFILE%NMPICOMM)
                   ELSE
                      CALL SECOND_MNH2(T0)
                      IF ( ISP /= IK_RANK )  THEN
                         ! Other processes
                         CALL GET_DOMWRITE_ll(ISP,'local',IXO,IXE,IYO,IYE)
                         IF (IXO /= 0) THEN ! intersection is not empty
                            NB_REQ = NB_REQ + 1
                            ALLOCATE(T_TX2DP(NB_REQ)%X(IXO:IXE,IYO:IYE))
                            ZSLICE => PFIELD(:,:,JKK)
                            TX2DP=>ZSLICE(IXO:IXE,IYO:IYE)
                            T_TX2DP(NB_REQ)%X=ZSLICE(IXO:IXE,IYO:IYE)
                            CALL MPI_ISEND(T_TX2DP(NB_REQ)%X,SIZE(TX2DP),MNHREAL_MPI,IK_RANK-1,99+IK_RANK &
                                          & ,TZFILE%NMPICOMM,REQ_TAB(NB_REQ),IERR)
                            !CALL MPI_BSEND(TX2DP,SIZE(TX2DP),MNHREAL_MPI,IK_RANK-1,99+IK_RANK,TZFILE%NMPICOMM,IERR)
                         END IF
                      END IF
                      CALL SECOND_MNH2(T1)
                      TIMEZ%T_WRIT3D_SEND=TIMEZ%T_WRIT3D_SEND + T1 - T0
                   END IF
                END IF
             END DO
             !
             ! Write the variable attributes in the non-split file
             !
             if ( tpfile%nmaster_rank==isp .and. gnc4 ) &
               call IO_Field_header_split_write_nc4( tpfile, tpfield, size( pfield, 3 ) )
             !
             ! write the data
             !
             DO JKK=JK,JK_MAX
                IF (TPFILE%NSUBFILES_IOZ .GT. 1 ) THEN
                   IK_FILE = IO_Level2filenumber_get(JKK,TPFILE%NSUBFILES_IOZ)
                   TZFILE => TPFILE%TFILES_IOZ(IK_FILE+1)%TFILE
                ELSE
                   TZFILE => TPFILE
                ENDIF
                IK_RANK = TZFILE%NMASTER_RANK
                !
                IF (ISP == IK_RANK )  THEN
                   CALL SECOND_MNH2(T0)
                   ! I/O proc case
                   IF ( SIZE(ZSLICE_ll) .EQ. 0 ) THEN
                      DEALLOCATE(ZSLICE_ll)
                      CALL ALLOCBUFFER_ll(ZSLICE_ll,ZSLICE,YDIR,GALLOC_ll)
                   END IF
                   DO JI=1,ISNPROC
                      CALL GET_DOMWRITE_ll(JI,'global',IXO,IXE,IYO,IYE)
                      IF (IXO /= 0) THEN ! intersection is not empty
                         TX2DP=>ZSLICE_ll(IXO:IXE,IYO:IYE)
                         IF (ISP == JI) THEN 
                            CALL GET_DOMWRITE_ll(JI,'local',IXO,IXE,IYO,IYE)
                            ZSLICE => PFIELD(:,:,JKK)
                            TX2DP = ZSLICE(IXO:IXE,IYO:IYE)
                         ELSE 
                            CALL MPI_RECV(TX2DP,SIZE(TX2DP),MNHREAL_MPI,JI-1,99+IK_RANK,TZFILE%NMPICOMM,STATUS,IERR)
                         END IF
                      END IF
                   END DO
                   CALL SECOND_MNH2(T1)
                   TIMEZ%T_WRIT3D_RECV=TIMEZ%T_WRIT3D_RECV + T1 - T0
                   IF (GLFI) CALL IO_Field_write_lfi(TPFILE,TPFIELD,ZSLICE_ll,IRESP,KVERTLEVEL=JKK,KZFILE=IK_FILE+1)
                   IF (GNC4) CALL IO_Field_write_nc4(TPFILE,TPFIELD,ZSLICE_ll,IRESP,KVERTLEVEL=JKK,KZFILE=IK_FILE+1)
                   CALL SECOND_MNH2(T2)
                   TIMEZ%T_WRIT3D_WRIT=TIMEZ%T_WRIT3D_WRIT + T2 - T1
                END IF
             END DO
             !
             CALL SECOND_MNH2(T0) 
             IF (NB_REQ .GT.0 ) THEN
                CALL MPI_WAITALL(NB_REQ,REQ_TAB,MNH_STATUSES_IGNORE,IERR)
                DO JI=1,NB_REQ ;  DEALLOCATE(T_TX2DP(JI)%X) ; ENDDO
             END IF
             DEALLOCATE(T_TX2DP)
             DEALLOCATE(REQ_TAB)
             CALL SECOND_MNH2(T1) 
             TIMEZ%T_WRIT3D_WAIT=TIMEZ%T_WRIT3D_WAIT + T1 - T0
          END DO Z_SLICE
          !JUAN BG Z SLICE
! end of MNH_GA
#endif
       END IF ! multiprocesses execution
    END IF
    !
    IF (IRESP.NE.0) THEN
      WRITE(YRESP, '( I6 )') IRESP
      YMSG = 'RESP='//YRESP//' when writing '//TRIM(YRECFM)//' in '//TRIM(TPFILE%CNAME)
      CALL PRINT_MSG(NVERB_ERROR,'IO','IO_Field_write_byfield_X3',YMSG)
    END IF
    IF (GALLOC)    DEALLOCATE(ZFIELDP)
    IF (GALLOC_ll) DEALLOCATE(ZSLICE_ll)
    IF (PRESENT(KRESP)) KRESP = IRESP
    CALL SECOND_MNH2(T22)
    TIMEZ%T_WRIT3D_ALL=TIMEZ%T_WRIT3D_ALL + T22 - T11
  END SUBROUTINE IO_Field_write_byfield_X3


  SUBROUTINE IO_Field_write_byname_X4(TPFILE,HNAME,PFIELD,KRESP)
    !
    !*      0.1   Declarations of arguments
    !
    TYPE(TFILEDATA),             INTENT(IN) :: TPFILE
    CHARACTER(LEN=*),            INTENT(IN) :: HNAME    ! name of the field to write
    REAL,DIMENSION(:,:,:,:),     INTENT(IN) :: PFIELD   ! array containing the data field
    INTEGER,OPTIONAL,            INTENT(OUT):: KRESP    ! return-code 
    !
    !*      0.2   Declarations of local variables
    !
    INTEGER :: ID ! Index of the field
    INTEGER :: IRESP ! return_code
    !
    CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_write_byname_X4',TRIM(TPFILE%CNAME)//': writing '//TRIM(HNAME))
    !
    CALL FIND_FIELD_ID_FROM_MNHNAME(HNAME,ID,IRESP)
    !
    IF(IRESP==0) CALL IO_Field_write(TPFILE,TFIELDLIST(ID),PFIELD,IRESP)
    !
    IF (PRESENT(KRESP)) KRESP = IRESP
    !
  END SUBROUTINE IO_Field_write_byname_X4


  SUBROUTINE IO_Field_write_byfield_X4(TPFILE,TPFIELD,PFIELD,KRESP)
    USE MODD_IO,            ONLY: GSMONOPROC, ISP, L1D, L2D, LPACK
    USE MODD_PARAMETERS_ll, ONLY: JPHEXT
    USE MODD_TIMEZ,         ONLY: TIMEZ
    !
    USE MODE_ALLOCBUFFER_ll
    USE MODE_GATHER_ll
    USE MODE_IO_TOOLS,      ONLY: IO_Level2filenumber_get
    USE MODE_MNH_TIMING,    ONLY: SECOND_MNH2
    USE MODD_VAR_ll,        ONLY: MNH_STATUSES_IGNORE
    !
    !
    !*      0.1   Declarations of arguments
    !
    TYPE(TFILEDATA),                 INTENT(IN) :: TPFILE
    TYPE(TFIELDDATA),                INTENT(IN) :: TPFIELD
    REAL,DIMENSION(:,:,:,:),TARGET,  INTENT(IN) :: PFIELD   ! array containing the data field
    INTEGER,OPTIONAL,                INTENT(OUT):: KRESP    ! return-code 
    !
    !*      0.2   Declarations of local variables
    !
    CHARACTER(LEN=28)                        :: YFILEM   ! FM-file name
    CHARACTER(LEN=NMNHNAMELGTMAX)            :: YRECFM   ! name of the article to write
    CHARACTER(LEN=2)                         :: YDIR     ! field form
    INTEGER                                  :: IERR
    INTEGER                                  :: ISIZEMAX
    INTEGER                                  :: IRESP
    REAL,DIMENSION(:,:,:,:),POINTER          :: ZFIELDP
    LOGICAL                                  :: GALLOC
    LOGICAL                                  :: GLFI, GNC4
    INTEGER                                  :: IHEXTOT
    CHARACTER(LEN=:),ALLOCATABLE             :: YMSG
    CHARACTER(LEN=6)                         :: YRESP
    !
    YFILEM   = TPFILE%CNAME
    YRECFM   = TPFIELD%CMNHNAME
    YDIR     = TPFIELD%CDIR
    !
    IRESP = 0
    GALLOC    = .FALSE.
    !
    IHEXTOT = 2*JPHEXT+1
    !
    CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_write_byfield_X4',TRIM(YFILEM)//': writing '//TRIM(YRECFM))
    !
    CALL IO_Field_metadata_check(TPFIELD,TYPEREAL,4,'IO_Field_write_byfield_X4')
    !
    CALL IO_File_write_check(TPFILE,'IO_Field_write_byfield_X4',IRESP)
    !
    CALL IO_Format_write_select(TPFILE,GLFI,GNC4)
    !
    IF (IRESP==0) THEN
       IF (GSMONOPROC) THEN ! sequential execution
          !    IF (LPACK .AND. L1D .AND. YDIR=='XY') THEN 
          IF (LPACK .AND. L1D .AND. SIZE(PFIELD,1)==IHEXTOT .AND. SIZE(PFIELD,2)==IHEXTOT) THEN 
             ZFIELDP=>PFIELD(JPHEXT+1:JPHEXT+1,JPHEXT+1:JPHEXT+1,:,:)
             !    ELSE IF (LPACK .AND. L2D .AND. YDIR=='XY') THEN
          ELSEIF (LPACK .AND. L2D .AND. SIZE(PFIELD,2)==IHEXTOT) THEN
             ZFIELDP=>PFIELD(:,JPHEXT+1:JPHEXT+1,:,:)
             IF (GLFI) CALL IO_Field_write_lfi(TPFILE,TPFIELD,ZFIELDP,IRESP)
             IF (GNC4) CALL IO_Field_write_nc4(TPFILE,TPFIELD,ZFIELDP,IRESP)
          ELSE
             IF (GLFI) CALL IO_Field_write_lfi(TPFILE,TPFIELD,PFIELD,IRESP)
             IF (GNC4) CALL IO_Field_write_nc4(TPFILE,TPFIELD,PFIELD,IRESP)
          END IF
       ELSE
          CALL MPI_ALLREDUCE(SIZE(PFIELD),ISIZEMAX,1,MNHINT_MPI,MPI_MAX,TPFILE%NMPICOMM,IRESP)
          IF (ISIZEMAX==0) THEN
             CALL PRINT_MSG(NVERB_INFO,'IO','IO_Field_write_byfield_X4','ignoring variable with a zero size ('//TRIM(YRECFM)//')')
             IF (PRESENT(KRESP)) KRESP=0
             RETURN
          END IF

          IF (ISP == TPFILE%NMASTER_RANK)  THEN
             CALL ALLOCBUFFER_ll(ZFIELDP,PFIELD,YDIR,GALLOC)
          ELSE
             ALLOCATE(ZFIELDP(0,0,0,0))
             GALLOC = .TRUE.
          END IF
          !
          IF (YDIR == 'XX' .OR. YDIR =='YY') THEN
             CALL GATHER_XXFIELD(YDIR,PFIELD,ZFIELDP,TPFILE%NMASTER_RANK,TPFILE%NMPICOMM)
          ELSEIF (YDIR == 'XY') THEN
             IF (LPACK .AND. L2D) THEN
                CALL GATHER_XXFIELD('XX',PFIELD(:,JPHEXT+1,:,:),ZFIELDP(:,1,:,:),TPFILE%NMASTER_RANK,TPFILE%NMPICOMM)
             ELSE
                CALL GATHER_XYFIELD(PFIELD,ZFIELDP,TPFILE%NMASTER_RANK,TPFILE%NMPICOMM)
             END IF
          END IF
          !
          IF (ISP == TPFILE%NMASTER_RANK)  THEN
             IF (GLFI) CALL IO_Field_write_lfi(TPFILE,TPFIELD,ZFIELDP,IRESP)
             IF (GNC4) CALL IO_Field_write_nc4(TPFILE,TPFIELD,ZFIELDP,IRESP)
          END IF
          !
          CALL MPI_BCAST(IRESP,1,MNHINT_MPI,TPFILE%NMASTER_RANK-1,TPFILE%NMPICOMM,IERR)
       END IF ! multiprocess execution
    END IF
    !
    IF (IRESP.NE.0) THEN
      WRITE(YRESP, '( I6 )') IRESP
      YMSG = 'RESP='//YRESP//' when writing '//TRIM(YRECFM)//' in '//TRIM(TPFILE%CNAME)
      CALL PRINT_MSG(NVERB_ERROR,'IO','IO_Field_write_byfield_X4',YMSG)
    END IF
    IF (GALLOC) DEALLOCATE(ZFIELDP)
    IF (PRESENT(KRESP)) KRESP = IRESP
  END SUBROUTINE IO_Field_write_byfield_X4


  SUBROUTINE IO_Field_write_byname_X5(TPFILE,HNAME,PFIELD,KRESP)
    !
    !*      0.1   Declarations of arguments
    !
    TYPE(TFILEDATA),             INTENT(IN) :: TPFILE
    CHARACTER(LEN=*),            INTENT(IN) :: HNAME    ! name of the field to write
    REAL,DIMENSION(:,:,:,:,:),   INTENT(IN) :: PFIELD   ! array containing the data field
    INTEGER,OPTIONAL,            INTENT(OUT):: KRESP    ! return-code 
    !
    !*      0.2   Declarations of local variables
    !
    INTEGER :: ID ! Index of the field
    INTEGER :: IRESP ! return_code
    !
    CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_write_byname_X5',TRIM(TPFILE%CNAME)//': writing '//TRIM(HNAME))
    !
    CALL FIND_FIELD_ID_FROM_MNHNAME(HNAME,ID,IRESP)
    !
    IF(IRESP==0) CALL IO_Field_write(TPFILE,TFIELDLIST(ID),PFIELD,IRESP)
    !
    IF (PRESENT(KRESP)) KRESP = IRESP
    !
  END SUBROUTINE IO_Field_write_byname_X5


  SUBROUTINE IO_Field_write_byfield_X5(TPFILE,TPFIELD,PFIELD,KRESP)
    USE MODD_IO,            ONLY: GSMONOPROC, ISP, L1D, L2D, LPACK
    USE MODD_PARAMETERS_ll, ONLY: JPHEXT
    USE MODD_TIMEZ,         ONLY: TIMEZ
    !
    USE MODE_ALLOCBUFFER_ll
    USE MODE_GATHER_ll
    USE MODE_IO_TOOLS,      ONLY: IO_Level2filenumber_get
    USE MODE_MNH_TIMING,    ONLY: SECOND_MNH2
    USE MODD_VAR_ll,        ONLY: MNH_STATUSES_IGNORE
    !
    !
    !*      0.1   Declarations of arguments
    !
    TYPE(TFILEDATA),                 INTENT(IN) :: TPFILE
    TYPE(TFIELDDATA),                INTENT(IN) :: TPFIELD
    REAL,DIMENSION(:,:,:,:,:),TARGET,INTENT(IN) :: PFIELD   ! array containing the data field
    INTEGER,OPTIONAL,                INTENT(OUT):: KRESP    ! return-code 
    !
    !*      0.2   Declarations of local variables
    !
    CHARACTER(LEN=28)                        :: YFILEM   ! FM-file name
    CHARACTER(LEN=NMNHNAMELGTMAX)            :: YRECFM   ! name of the article to write
    CHARACTER(LEN=2)                         :: YDIR     ! field form
    INTEGER                                  :: IERR
    INTEGER                                  :: ISIZEMAX
    INTEGER                                  :: IRESP
    REAL,DIMENSION(:,:,:,:,:),POINTER        :: ZFIELDP
    LOGICAL                                  :: GALLOC
    LOGICAL                                  :: GLFI, GNC4
    INTEGER                                  :: IHEXTOT
    CHARACTER(LEN=:),ALLOCATABLE             :: YMSG
    CHARACTER(LEN=6)                         :: YRESP
    !
    YFILEM   = TPFILE%CNAME
    YRECFM   = TPFIELD%CMNHNAME
    YDIR     = TPFIELD%CDIR
    !
    IRESP = 0
    GALLOC    = .FALSE.
    !
    IHEXTOT = 2*JPHEXT+1
    !
    CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_write_byfield_X5',TRIM(YFILEM)//': writing '//TRIM(YRECFM))
    !
    CALL IO_Field_metadata_check(TPFIELD,TYPEREAL,5,'IO_Field_write_byfield_X5')
    !
    CALL IO_File_write_check(TPFILE,'IO_Field_write_byfield_X5',IRESP)
    !
    CALL IO_Format_write_select(TPFILE,GLFI,GNC4)
    !
    IF (IRESP==0) THEN
       IF (GSMONOPROC) THEN ! sequential execution
          !    IF (LPACK .AND. L1D .AND. YDIR=='XY') THEN 
          IF (LPACK .AND. L1D .AND. SIZE(PFIELD,1)==IHEXTOT .AND. SIZE(PFIELD,2)==IHEXTOT) THEN 
             ZFIELDP=>PFIELD(JPHEXT+1:JPHEXT+1,JPHEXT+1:JPHEXT+1,:,:,:)
             !    ELSE IF (LPACK .AND. L2D .AND. YDIR=='XY') THEN
          ELSEIF (LPACK .AND. L2D .AND. SIZE(PFIELD,2)==IHEXTOT) THEN
             ZFIELDP=>PFIELD(:,JPHEXT+1:JPHEXT+1,:,:,:)
             IF (GLFI) CALL IO_Field_write_lfi(TPFILE,TPFIELD,ZFIELDP,IRESP)
             IF (GNC4) CALL IO_Field_write_nc4(TPFILE,TPFIELD,ZFIELDP,IRESP)
          ELSE
             IF (GLFI) CALL IO_Field_write_lfi(TPFILE,TPFIELD,PFIELD,IRESP)
             IF (GNC4) CALL IO_Field_write_nc4(TPFILE,TPFIELD,PFIELD,IRESP)
          END IF
       ELSE
          CALL MPI_ALLREDUCE(SIZE(PFIELD),ISIZEMAX,1,MNHINT_MPI,MPI_MAX,TPFILE%NMPICOMM,IRESP)
          IF (ISIZEMAX==0) THEN
             CALL PRINT_MSG(NVERB_INFO,'IO','IO_Field_write_byfield_X5','ignoring variable with a zero size ('//TRIM(YRECFM)//')')
             IF (PRESENT(KRESP)) KRESP=0
             RETURN
          END IF

          IF (ISP == TPFILE%NMASTER_RANK)  THEN
             CALL ALLOCBUFFER_ll(ZFIELDP,PFIELD,YDIR,GALLOC)
          ELSE
             ALLOCATE(ZFIELDP(0,0,0,0,0))
             GALLOC = .TRUE.
          END IF
          !
          IF (YDIR == 'XX' .OR. YDIR =='YY') THEN
             CALL GATHER_XXFIELD(YDIR,PFIELD,ZFIELDP,TPFILE%NMASTER_RANK,TPFILE%NMPICOMM)
          ELSEIF (YDIR == 'XY') THEN
             IF (LPACK .AND. L2D) THEN
                CALL GATHER_XXFIELD('XX',PFIELD(:,JPHEXT+1,:,:,:),ZFIELDP(:,1,:,:,:),&
                     & TPFILE%NMASTER_RANK,TPFILE%NMPICOMM)
             ELSE
                CALL GATHER_XYFIELD(PFIELD,ZFIELDP,TPFILE%NMASTER_RANK,TPFILE%NMPICOMM)
             END IF
          END IF
          !
          IF (ISP == TPFILE%NMASTER_RANK)  THEN
             IF (GLFI) CALL IO_Field_write_lfi(TPFILE,TPFIELD,ZFIELDP,IRESP)
             IF (GNC4) CALL IO_Field_write_nc4(TPFILE,TPFIELD,ZFIELDP,IRESP)
          END IF
          !
          CALL MPI_BCAST(IRESP,1,MNHINT_MPI,TPFILE%NMASTER_RANK-1,TPFILE%NMPICOMM,IERR)
       END IF ! multiprocess execution
    END IF
    !
    IF (IRESP.NE.0) THEN
      WRITE(YRESP, '( I6 )') IRESP
      YMSG = 'RESP='//YRESP//' when writing '//TRIM(YRECFM)//' in '//TRIM(TPFILE%CNAME)
      CALL PRINT_MSG(NVERB_ERROR,'IO','IO_Field_write_byfield_X5',YMSG)
    END IF
    IF (GALLOC) DEALLOCATE(ZFIELDP)
    IF (PRESENT(KRESP)) KRESP = IRESP
  END SUBROUTINE IO_Field_write_byfield_X5


  SUBROUTINE IO_Field_write_byname_X6(TPFILE,HNAME,PFIELD,KRESP)
    !
    !*      0.1   Declarations of arguments
    !
    TYPE(TFILEDATA),             INTENT(IN) :: TPFILE
    CHARACTER(LEN=*),            INTENT(IN) :: HNAME    ! name of the field to write
    REAL,DIMENSION(:,:,:,:,:,:), INTENT(IN) :: PFIELD   ! array containing the data field
    INTEGER,OPTIONAL,            INTENT(OUT):: KRESP    ! return-code 
    !
    !*      0.2   Declarations of local variables
    !
    INTEGER :: ID ! Index of the field
    INTEGER :: IRESP ! return_code
    !
    CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_write_byname_X6',TRIM(TPFILE%CNAME)//': writing '//TRIM(HNAME))
    !
    CALL FIND_FIELD_ID_FROM_MNHNAME(HNAME,ID,IRESP)
    !
    IF(IRESP==0) CALL IO_Field_write(TPFILE,TFIELDLIST(ID),PFIELD,IRESP)
    !
    IF (PRESENT(KRESP)) KRESP = IRESP
    !
  END SUBROUTINE IO_Field_write_byname_X6

  SUBROUTINE IO_Field_write_byfield_X6(TPFILE,TPFIELD,PFIELD,KRESP)
    USE MODD_IO,            ONLY: GSMONOPROC, ISP
    USE MODD_PARAMETERS_ll, ONLY: JPHEXT
    USE MODD_TIMEZ,         ONLY: TIMEZ
    !
    USE MODE_ALLOCBUFFER_ll
    USE MODE_GATHER_ll
    USE MODE_IO_TOOLS,      ONLY: IO_Level2filenumber_get
    USE MODE_MNH_TIMING,    ONLY: SECOND_MNH2
    USE MODD_VAR_ll,        ONLY: MNH_STATUSES_IGNORE
    !
    !
    !*      0.1   Declarations of arguments
    !
    TYPE(TFILEDATA),                   INTENT(IN) :: TPFILE
    TYPE(TFIELDDATA),                  INTENT(IN) :: TPFIELD
    REAL,DIMENSION(:,:,:,:,:,:),TARGET,INTENT(IN) :: PFIELD   ! array containing the data field
    INTEGER,OPTIONAL,                  INTENT(OUT):: KRESP    ! return-code 
    !
    !*      0.2   Declarations of local variables
    !
    CHARACTER(LEN=28)                        :: YFILEM   ! FM-file name
    CHARACTER(LEN=NMNHNAMELGTMAX)            :: YRECFM   ! name of the article to write
    CHARACTER(LEN=2)                         :: YDIR     ! field form
    INTEGER                                  :: IERR
    INTEGER                                  :: ISIZEMAX
    INTEGER                                  :: IRESP
    REAL,DIMENSION(:,:,:,:,:,:),POINTER      :: ZFIELDP
    LOGICAL                                  :: GLFI, GNC4
    LOGICAL                                  :: GALLOC
    INTEGER                                  :: IHEXTOT
    CHARACTER(LEN=:),ALLOCATABLE             :: YMSG
    CHARACTER(LEN=6)                         :: YRESP
    !
    YFILEM   = TPFILE%CNAME
    YRECFM   = TPFIELD%CMNHNAME
    YDIR     = TPFIELD%CDIR
    !
    IRESP = 0
    GALLOC    = .FALSE.
    !
    IHEXTOT = 2*JPHEXT+1
    !
    CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_write_byfield_X6',TRIM(YFILEM)//': writing '//TRIM(YRECFM))
    !
    CALL IO_Field_metadata_check(TPFIELD,TYPEREAL,6,'IO_Field_write_byfield_X6')
    !
    CALL IO_File_write_check(TPFILE,'IO_Field_write_byfield_X6',IRESP)
    !
    CALL IO_Format_write_select(TPFILE,GLFI,GNC4)
    !
    IF (IRESP==0) THEN
       IF (GSMONOPROC) THEN ! sequential execution
          IF (GLFI) CALL IO_Field_write_lfi(TPFILE,TPFIELD,PFIELD,IRESP)
          IF (GNC4) CALL IO_Field_write_nc4(TPFILE,TPFIELD,PFIELD,IRESP)
       ELSE
          CALL MPI_ALLREDUCE(SIZE(PFIELD),ISIZEMAX,1,MNHINT_MPI,MPI_MAX,TPFILE%NMPICOMM,IRESP)
          IF (ISIZEMAX==0) THEN
             CALL PRINT_MSG(NVERB_INFO,'IO','IO_Field_write_byfield_X6','ignoring variable with a zero size ('//TRIM(YRECFM)//')')
             IF (PRESENT(KRESP)) KRESP=0
             RETURN
          END IF

          IF (ISP == TPFILE%NMASTER_RANK)  THEN
             CALL ALLOCBUFFER_ll(ZFIELDP,PFIELD,YDIR,GALLOC)
          ELSE
             ALLOCATE(ZFIELDP(0,0,0,0,0,0))
             GALLOC = .TRUE.
          END IF
          !
          IF (YDIR == 'XX' .OR. YDIR =='YY') THEN
             CALL GATHER_XXFIELD(YDIR,PFIELD,ZFIELDP,TPFILE%NMASTER_RANK,TPFILE%NMPICOMM)
          ELSEIF (YDIR == 'XY') THEN
             CALL GATHER_XYFIELD(PFIELD,ZFIELDP,TPFILE%NMASTER_RANK,TPFILE%NMPICOMM)
          END IF
          !
          IF (ISP == TPFILE%NMASTER_RANK)  THEN
             IF (GLFI) CALL IO_Field_write_lfi(TPFILE,TPFIELD,ZFIELDP,IRESP)
             IF (GNC4) CALL IO_Field_write_nc4(TPFILE,TPFIELD,ZFIELDP,IRESP)
          END IF
          !
          CALL MPI_BCAST(IRESP,1,MNHINT_MPI,TPFILE%NMASTER_RANK-1,TPFILE%NMPICOMM,IERR)
       END IF ! multiprocess execution
    END IF
    !
    IF (IRESP.NE.0) THEN
      WRITE(YRESP, '( I6 )') IRESP
      YMSG = 'RESP='//YRESP//' when writing '//TRIM(YRECFM)//' in '//TRIM(TPFILE%CNAME)
      CALL PRINT_MSG(NVERB_ERROR,'IO','IO_Field_write_byfield_X6',YMSG)
    END IF
    IF (GALLOC) DEALLOCATE(ZFIELDP)
    IF (PRESENT(KRESP)) KRESP = IRESP
  END SUBROUTINE IO_Field_write_byfield_X6


  SUBROUTINE IO_Field_write_byname_N0(TPFILE,HNAME,KFIELD,KRESP)
    !
    !*      0.1   Declarations of arguments
    !
    TYPE(TFILEDATA),             INTENT(IN) :: TPFILE
    CHARACTER(LEN=*),            INTENT(IN) :: HNAME    ! name of the field to write
    INTEGER,                     INTENT(IN) :: KFIELD   ! array containing the data field
    INTEGER,OPTIONAL,            INTENT(OUT):: KRESP    ! return-code 
    !
    !*      0.2   Declarations of local variables
    !
    INTEGER :: ID ! Index of the field
    INTEGER :: IRESP ! return_code
    !
    CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_write_byname_N0',TRIM(TPFILE%CNAME)//': writing '//TRIM(HNAME))
    !
    CALL FIND_FIELD_ID_FROM_MNHNAME(HNAME,ID,IRESP)
    !
    IF(IRESP==0) CALL IO_Field_write(TPFILE,TFIELDLIST(ID),KFIELD,IRESP)
    !
    IF (PRESENT(KRESP)) KRESP = IRESP
    !
  END SUBROUTINE IO_Field_write_byname_N0


  SUBROUTINE IO_Field_write_byfield_N0(TPFILE,TPFIELD,KFIELD,KRESP)
    USE MODD_IO, ONLY: GSMONOPROC, ISP
    !*      0.    DECLARATIONS
    !             ------------
    !
    !
    !*      0.1   Declarations of arguments
    !
    TYPE(TFILEDATA),             INTENT(IN) :: TPFILE
    TYPE(TFIELDDATA),            INTENT(IN) :: TPFIELD
    INTEGER,                     INTENT(IN) :: KFIELD   ! array containing the data field
    INTEGER,OPTIONAL,            INTENT(OUT):: KRESP    ! return-code 
    !
    !*      0.2   Declarations of local variables
    !
    INTEGER                      :: IERR
    INTEGER                      :: IRESP
    INTEGER                      :: IK_FILE
    TYPE(TFILEDATA),POINTER      :: TZFILE
    LOGICAL                      :: GLFI, GNC4
    CHARACTER(LEN=:),ALLOCATABLE :: YMSG
    CHARACTER(LEN=6)             :: YRESP
    !
    IRESP = 0
    TZFILE => NULL()
    !
    CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_write_byfield_N0',TRIM(TPFILE%CNAME)//': writing '//TRIM(TPFIELD%CMNHNAME))
    !
    CALL IO_Field_metadata_check(TPFIELD,TYPEINT,0,'IO_Field_write_byfield_N0')
    !
    CALL IO_File_write_check(TPFILE,'IO_Field_write_byfield_N0',IRESP)
    !
    CALL IO_Format_write_select(TPFILE,GLFI,GNC4)
    !
    IF (IRESP==0) THEN
       IF (GSMONOPROC) THEN ! sequential execution
          IF (GLFI) CALL IO_Field_write_lfi(TPFILE,TPFIELD,KFIELD,IRESP)
          IF (GNC4) CALL IO_Field_write_nc4(TPFILE,TPFIELD,KFIELD,IRESP)
       ELSE 
          IF (ISP == TPFILE%NMASTER_RANK)  THEN
             IF (GLFI) CALL IO_Field_write_lfi(TPFILE,TPFIELD,KFIELD,IRESP)
             IF (GNC4) CALL IO_Field_write_nc4(TPFILE,TPFIELD,KFIELD,IRESP)
          END IF
          !
          CALL MPI_BCAST(IRESP,1,MNHINT_MPI,TPFILE%NMASTER_RANK-1,TPFILE%NMPICOMM,IERR)
       END IF ! multiprocess execution
#if MNH_SCALARS_IN_SPLITFILES
       IF (TPFILE%NSUBFILES_IOZ>0) THEN
          ! write the data in all Z files
          DO IK_FILE=1,TPFILE%NSUBFILES_IOZ
             TZFILE => TPFILE%TFILES_IOZ(IK_FILE)%TFILE
             IF ( ISP == TZFILE%NMASTER_RANK )  THEN
                IF (GLFI) CALL IO_Field_write_lfi(TZFILE,TPFIELD,KFIELD,IRESP)
                IF (GNC4) CALL IO_Field_write_nc4(TZFILE,TPFIELD,KFIELD,IRESP)
             END IF
          END DO
       ENDIF
#endif
    END IF
    !
    IF (IRESP.NE.0) THEN
      WRITE(YRESP, '( I6 )') IRESP
      YMSG = 'RESP='//YRESP//' when writing '//TRIM(TPFIELD%CMNHNAME)//' in '//TRIM(TPFILE%CNAME)
      CALL PRINT_MSG(NVERB_ERROR,'IO','IO_Field_write_byfield_N0',YMSG)
    END IF
    IF (PRESENT(KRESP)) KRESP = IRESP
  END SUBROUTINE IO_Field_write_byfield_N0


  SUBROUTINE IO_Field_write_byname_N1(TPFILE,HNAME,KFIELD,KRESP)
    !
    !*      0.1   Declarations of arguments
    !
    TYPE(TFILEDATA),             INTENT(IN) :: TPFILE
    CHARACTER(LEN=*),            INTENT(IN) :: HNAME    ! name of the field to write
    INTEGER,DIMENSION(:),        INTENT(IN) :: KFIELD   ! array containing the data field
    INTEGER,OPTIONAL,            INTENT(OUT):: KRESP    ! return-code 
    !
    !*      0.2   Declarations of local variables
    !
    INTEGER :: ID ! Index of the field
    INTEGER :: IRESP ! return_code
    !
    CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_write_byname_N1',TRIM(TPFILE%CNAME)//': writing '//TRIM(HNAME))
    !
    CALL FIND_FIELD_ID_FROM_MNHNAME(HNAME,ID,IRESP)
    !
    IF(IRESP==0) CALL IO_Field_write(TPFILE,TFIELDLIST(ID),KFIELD,IRESP)
    !
    IF (PRESENT(KRESP)) KRESP = IRESP
    !
  END SUBROUTINE IO_Field_write_byname_N1


  SUBROUTINE IO_Field_write_byfield_N1(TPFILE,TPFIELD,KFIELD,KRESP)
    !
    USE MODD_IO, ONLY: ISP,GSMONOPROC
    !
    USE MODE_ALLOCBUFFER_ll
    USE MODE_GATHER_ll
    !
    IMPLICIT NONE
    !
    !*      0.1   Declarations of arguments
    !
    TYPE(TFILEDATA),              INTENT(IN) :: TPFILE
    TYPE(TFIELDDATA),             INTENT(IN) :: TPFIELD
    INTEGER,DIMENSION(:),TARGET,  INTENT(IN) :: KFIELD   ! array containing the data field
    INTEGER,OPTIONAL,             INTENT(OUT):: KRESP    ! return-code 
    !
    !*      0.2   Declarations of local variables
    !
    CHARACTER(LEN=28)                        :: YFILEM   ! FM-file name
    CHARACTER(LEN=NMNHNAMELGTMAX)            :: YRECFM   ! name of the article to write
    CHARACTER(LEN=2)                         :: YDIR     ! field form
    INTEGER                                  :: IERR
    INTEGER                                  :: ISIZEMAX
    INTEGER                                  :: IRESP
    INTEGER,DIMENSION(:),POINTER             :: IFIELDP
    LOGICAL                                  :: GALLOC
    LOGICAL                                  :: GLFI, GNC4
    CHARACTER(LEN=:),ALLOCATABLE             :: YMSG
    CHARACTER(LEN=6)                         :: YRESP
    !
    YFILEM   = TPFILE%CNAME
    YRECFM   = TPFIELD%CMNHNAME
    YDIR     = TPFIELD%CDIR
    !
    IRESP = 0
    GALLOC = .FALSE.
    !
    CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_write_byfield_N1',TRIM(YFILEM)//': writing '//TRIM(YRECFM))
    !
    CALL IO_Field_metadata_check(TPFIELD,TYPEINT,1,'IO_Field_write_byfield_N1')
    !
    CALL IO_File_write_check(TPFILE,'IO_Field_write_byfield_N1',IRESP)
    !
    CALL IO_Format_write_select(TPFILE,GLFI,GNC4)
    !
    IF (IRESP==0) THEN
       IF (GSMONOPROC) THEN ! sequential execution
          IF (GLFI) CALL IO_Field_write_lfi(TPFILE,TPFIELD,KFIELD,IRESP)
          IF (GNC4) CALL IO_Field_write_nc4(TPFILE,TPFIELD,KFIELD,IRESP)
       ELSE ! multiprocesses execution
          CALL MPI_ALLREDUCE(SIZE(KFIELD),ISIZEMAX,1,MNHINT_MPI,MPI_MAX,TPFILE%NMPICOMM,IRESP)
          IF (ISIZEMAX==0) THEN
             CALL PRINT_MSG(NVERB_INFO,'IO','IO_Field_write_byfield_N1','ignoring variable with a zero size ('//TRIM(YRECFM)//')')
             IF (PRESENT(KRESP)) KRESP=0
             RETURN
          END IF

          IF (ISP == TPFILE%NMASTER_RANK)  THEN
             CALL ALLOCBUFFER_ll(IFIELDP,KFIELD,YDIR,GALLOC)
          ELSE
             ALLOCATE(IFIELDP(0))
             GALLOC = .TRUE.
          END IF
          !
          IF (YDIR == 'XX' .OR. YDIR =='YY') THEN
             CALL GATHER_XXFIELD(YDIR,KFIELD,IFIELDP,TPFILE%NMASTER_RANK,TPFILE%NMPICOMM)
          END IF
          !
          IF (ISP == TPFILE%NMASTER_RANK)  THEN
             IF (GLFI) CALL IO_Field_write_lfi(TPFILE,TPFIELD,IFIELDP,IRESP)
             IF (GNC4) CALL IO_Field_write_nc4(TPFILE,TPFIELD,IFIELDP,IRESP)
          END IF
          !
          CALL MPI_BCAST(IRESP,1,MNHINT_MPI,TPFILE%NMASTER_RANK-1,TPFILE%NMPICOMM,IERR)
       END IF
    END IF
    !
    IF (IRESP.NE.0) THEN
      WRITE(YRESP, '( I6 )') IRESP
      YMSG = 'RESP='//YRESP//' when writing '//TRIM(YRECFM)//' in '//TRIM(TPFILE%CNAME)
      CALL PRINT_MSG(NVERB_ERROR,'IO','IO_Field_write_byfield_N1',YMSG)
    END IF
    IF (GALLOC) DEALLOCATE(IFIELDP)
    IF (PRESENT(KRESP)) KRESP = IRESP
    !
  END SUBROUTINE IO_Field_write_byfield_N1

  
  SUBROUTINE IO_Field_write_byname_N2(TPFILE,HNAME,KFIELD,KRESP)
    !
    !*      0.1   Declarations of arguments
    !
    TYPE(TFILEDATA),             INTENT(IN) :: TPFILE
    CHARACTER(LEN=*),            INTENT(IN) :: HNAME    ! name of the field to write
    INTEGER,DIMENSION(:,:),      INTENT(IN) :: KFIELD   ! array containing the data field
    INTEGER,OPTIONAL,            INTENT(OUT):: KRESP    ! return-code 
    !
    !*      0.2   Declarations of local variables
    !
    INTEGER :: ID ! Index of the field
    INTEGER :: IRESP ! return_code
    !
    CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_write_byname_N2',TRIM(TPFILE%CNAME)//': writing '//TRIM(HNAME))
    !
    CALL FIND_FIELD_ID_FROM_MNHNAME(HNAME,ID,IRESP)
    !
    IF(IRESP==0) CALL IO_Field_write(TPFILE,TFIELDLIST(ID),KFIELD,IRESP)
    !
    IF (PRESENT(KRESP)) KRESP = IRESP
    !
  END SUBROUTINE IO_Field_write_byname_N2


  SUBROUTINE IO_Field_write_byfield_N2(TPFILE,TPFIELD,KFIELD,KRESP)
    USE MODD_IO,            ONLY: GSMONOPROC, ISP, L1D, L2D, LPACK
    USE MODD_PARAMETERS_ll, ONLY: JPHEXT
    USE MODD_TIMEZ,         ONLY: TIMEZ
    !
    USE MODE_ALLOCBUFFER_ll
    USE MODE_GATHER_ll
    USE MODE_MNH_TIMING,    ONLY: SECOND_MNH2
    !
    IMPLICIT NONE
    !
    !*      0.1   Declarations of arguments
    !
    TYPE(TFILEDATA),              INTENT(IN) :: TPFILE
    TYPE(TFIELDDATA),             INTENT(IN) :: TPFIELD
    INTEGER,DIMENSION(:,:),TARGET,INTENT(IN) :: KFIELD   ! array containing the data field
    INTEGER,OPTIONAL,             INTENT(OUT):: KRESP    ! return-code 
    !
    !*      0.2   Declarations of local variables
    !
    CHARACTER(LEN=28)                        :: YFILEM   ! FM-file name
    CHARACTER(LEN=NMNHNAMELGTMAX)            :: YRECFM   ! name of the article to write
    CHARACTER(LEN=2)                         :: YDIR     ! field form
    INTEGER                                  :: IERR
    INTEGER                                  :: ISIZEMAX
    INTEGER                                  :: IRESP
    INTEGER,DIMENSION(:,:),POINTER           :: IFIELDP
    LOGICAL                                  :: GALLOC
    LOGICAL                                  :: GLFI, GNC4
    !
    REAL(kind=MNHTIME), DIMENSION(2)         :: T0, T1, T2
    REAL(kind=MNHTIME), DIMENSION(2)         :: T11, T22
    INTEGER                                  :: IHEXTOT
    CHARACTER(LEN=:),ALLOCATABLE             :: YMSG
    CHARACTER(LEN=6)                         :: YRESP
    !
    YFILEM   = TPFILE%CNAME
    YRECFM   = TPFIELD%CMNHNAME
    YDIR     = TPFIELD%CDIR
    !
    IRESP = 0
    GALLOC = .FALSE.
    !
    IHEXTOT = 2*JPHEXT+1
    !
    CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_write_byfield_N2',TRIM(YFILEM)//': writing '//TRIM(YRECFM))
    !
    CALL SECOND_MNH2(T11)
    !
    CALL IO_Field_metadata_check(TPFIELD,TYPEINT,2,'IO_Field_write_byfield_N2')
    !
    CALL IO_File_write_check(TPFILE,'IO_Field_write_byfield_N2',IRESP)
    !
    CALL IO_Format_write_select(TPFILE,GLFI,GNC4)
    !
    IF (IRESP==0) THEN
       IF (GSMONOPROC) THEN ! sequential execution
          IF (LPACK .AND. L1D .AND. SIZE(KFIELD,1)==IHEXTOT .AND. SIZE(KFIELD,2)==IHEXTOT) THEN 
             IFIELDP=>KFIELD(JPHEXT+1:JPHEXT+1,JPHEXT+1:JPHEXT+1)
             IF (GLFI) CALL IO_Field_write_lfi(TPFILE,TPFIELD,IFIELDP,IRESP)
             IF (GNC4) CALL IO_Field_write_nc4(TPFILE,TPFIELD,IFIELDP,IRESP)
             !    ELSE IF (LPACK .AND. L2D .AND. YDIR=='XY') THEN
          ELSEIF (LPACK .AND. L2D .AND. SIZE(KFIELD,2)==IHEXTOT) THEN
             IFIELDP=>KFIELD(:,JPHEXT+1:JPHEXT+1)
             IF (GLFI) CALL IO_Field_write_lfi(TPFILE,TPFIELD,IFIELDP,IRESP)
             IF (GNC4) CALL IO_Field_write_nc4(TPFILE,TPFIELD,IFIELDP,IRESP)
          ELSE
             IF (GLFI) CALL IO_Field_write_lfi(TPFILE,TPFIELD,KFIELD,IRESP)
             IF (GNC4) CALL IO_Field_write_nc4(TPFILE,TPFIELD,KFIELD,IRESP)
          END IF
       ELSE ! multiprocesses execution
          CALL MPI_ALLREDUCE(SIZE(KFIELD),ISIZEMAX,1,MNHINT_MPI,MPI_MAX,TPFILE%NMPICOMM,IRESP)
          IF (ISIZEMAX==0) THEN
             CALL PRINT_MSG(NVERB_INFO,'IO','IO_Field_write_byfield_N2','ignoring variable with a zero size ('//TRIM(YRECFM)//')')
             IF (PRESENT(KRESP)) KRESP=0
             RETURN
          END IF

          CALL SECOND_MNH2(T0)
          IF (ISP == TPFILE%NMASTER_RANK) THEN
             ! I/O process case
             CALL ALLOCBUFFER_ll(IFIELDP,KFIELD,YDIR,GALLOC)
          ELSE
             ALLOCATE(IFIELDP(0,0))
             GALLOC = .TRUE.
          END IF
          !   
          IF (YDIR == 'XX' .OR. YDIR =='YY') THEN
             CALL GATHER_XXFIELD(YDIR,KFIELD,IFIELDP,TPFILE%NMASTER_RANK,TPFILE%NMPICOMM)
          ELSEIF (YDIR == 'XY') THEN
             IF (LPACK .AND. L2D) THEN
                CALL GATHER_XXFIELD('XX',KFIELD(:,JPHEXT+1),IFIELDP(:,1),TPFILE%NMASTER_RANK,TPFILE%NMPICOMM)
             ELSE
                CALL GATHER_XYFIELD(KFIELD,IFIELDP,TPFILE%NMASTER_RANK,TPFILE%NMPICOMM)
             END IF
          END IF
          CALL SECOND_MNH2(T1)
          TIMEZ%T_WRIT2D_GATH=TIMEZ%T_WRIT2D_GATH + T1 - T0
          !
          IF (ISP == TPFILE%NMASTER_RANK) THEN
             IF (GLFI) CALL IO_Field_write_lfi(TPFILE,TPFIELD,IFIELDP,IRESP)
             IF (GNC4) CALL IO_Field_write_nc4(TPFILE,TPFIELD,IFIELDP,IRESP)
          END IF
          CALL SECOND_MNH2(T2)
          TIMEZ%T_WRIT2D_WRIT=TIMEZ%T_WRIT2D_WRIT + T2 - T1
          !
          CALL MPI_BCAST(IRESP,1,MNHINT_MPI,TPFILE%NMASTER_RANK-1,TPFILE%NMPICOMM,IERR)
       END IF
    END IF
    !
    IF (IRESP.NE.0) THEN
      WRITE(YRESP, '( I6 )') IRESP
      YMSG = 'RESP='//YRESP//' when writing '//TRIM(YRECFM)//' in '//TRIM(TPFILE%CNAME)
      CALL PRINT_MSG(NVERB_ERROR,'IO','IO_Field_write_byfield_N2',YMSG)
    END IF
    IF (GALLOC) DEALLOCATE(IFIELDP)
    IF (PRESENT(KRESP)) KRESP = IRESP
    CALL SECOND_MNH2(T22)
    TIMEZ%T_WRIT2D_ALL=TIMEZ%T_WRIT2D_ALL + T22 - T11
    !
  END SUBROUTINE IO_Field_write_byfield_N2


  SUBROUTINE IO_Field_write_byname_N3(TPFILE,HNAME,KFIELD,KRESP)
    !
    !*      0.1   Declarations of arguments
    !
    TYPE(TFILEDATA),             INTENT(IN) :: TPFILE
    CHARACTER(LEN=*),            INTENT(IN) :: HNAME    ! name of the field to write
    INTEGER,DIMENSION(:,:,:),    INTENT(IN) :: KFIELD   ! array containing the data field
    INTEGER,OPTIONAL,            INTENT(OUT):: KRESP    ! return-code 
    !
    !*      0.2   Declarations of local variables
    !
    INTEGER :: ID ! Index of the field
    INTEGER :: IRESP ! return_code
    !
    CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_write_byname_N3',TRIM(TPFILE%CNAME)//': writing '//TRIM(HNAME))
    !
    CALL FIND_FIELD_ID_FROM_MNHNAME(HNAME,ID,IRESP)
    !
    IF(IRESP==0) CALL IO_Field_write(TPFILE,TFIELDLIST(ID),KFIELD,IRESP)
    !
    IF (PRESENT(KRESP)) KRESP = IRESP
    !
  END SUBROUTINE IO_Field_write_byname_N3

  SUBROUTINE IO_Field_write_byfield_N3(TPFILE,TPFIELD,KFIELD,KRESP)
    USE MODD_IO,            ONLY: GSMONOPROC, ISP, L1D, L2D, LPACK
    USE MODD_PARAMETERS_ll, ONLY: JPHEXT
    USE MODD_TIMEZ,         ONLY: TIMEZ
    !
    USE MODE_ALLOCBUFFER_ll
    USE MODE_GATHER_ll
    USE MODE_MNH_TIMING,    ONLY: SECOND_MNH2
    !
    IMPLICIT NONE
    !
    !*      0.1   Declarations of arguments
    !
    TYPE(TFILEDATA),                INTENT(IN) :: TPFILE
    TYPE(TFIELDDATA),               INTENT(IN) :: TPFIELD
    INTEGER,DIMENSION(:,:,:),TARGET,INTENT(IN) :: KFIELD   ! array containing the data field
    INTEGER,OPTIONAL,               INTENT(OUT):: KRESP    ! return-code 
    !
    !*      0.2   Declarations of local variables
    !
    CHARACTER(LEN=28)                        :: YFILEM   ! FM-file name
    CHARACTER(LEN=NMNHNAMELGTMAX)            :: YRECFM   ! name of the article to write
    CHARACTER(LEN=2)                         :: YDIR     ! field form
    INTEGER                                  :: IERR
    INTEGER                                  :: ISIZEMAX
    INTEGER                                  :: IRESP
    INTEGER,DIMENSION(:,:,:),POINTER         :: IFIELDP
    LOGICAL                                  :: GALLOC
    LOGICAL                                  :: GLFI, GNC4
    !
    REAL(kind=MNHTIME), DIMENSION(2)         :: T11, T22
    INTEGER                                  :: IHEXTOT
    CHARACTER(LEN=:),ALLOCATABLE             :: YMSG
    CHARACTER(LEN=6)                         :: YRESP
    !
    YFILEM   = TPFILE%CNAME
    YRECFM   = TPFIELD%CMNHNAME
    YDIR     = TPFIELD%CDIR
    !
    IRESP = 0
    GALLOC = .FALSE.
    !
    IHEXTOT = 2*JPHEXT+1
    !
    CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_write_byfield_N3',TRIM(YFILEM)//': writing '//TRIM(YRECFM))
    !
    CALL SECOND_MNH2(T11)
    !
    CALL IO_Field_metadata_check(TPFIELD,TYPEINT,3,'IO_Field_write_byfield_N3')
    !
    CALL IO_File_write_check(TPFILE,'IO_Field_write_byfield_N3',IRESP)
    !
    CALL IO_Format_write_select(TPFILE,GLFI,GNC4)
    !
    IF (IRESP==0) THEN
       IF (GSMONOPROC) THEN ! sequential execution
          IF (LPACK .AND. L1D .AND. SIZE(KFIELD,1)==IHEXTOT .AND. SIZE(KFIELD,2)==IHEXTOT) THEN 
             IFIELDP=>KFIELD(JPHEXT+1:JPHEXT+1,JPHEXT+1:JPHEXT+1,:)
             IF (GLFI) CALL IO_Field_write_lfi(TPFILE,TPFIELD,IFIELDP,IRESP)
             IF (GNC4) CALL IO_Field_write_nc4(TPFILE,TPFIELD,IFIELDP,IRESP)
             !    ELSE IF (LPACK .AND. L2D .AND. YDIR=='XY') THEN
          ELSEIF (LPACK .AND. L2D .AND. SIZE(KFIELD,2)==IHEXTOT) THEN
             IFIELDP=>KFIELD(:,JPHEXT+1:JPHEXT+1,:)
             IF (GLFI) CALL IO_Field_write_lfi(TPFILE,TPFIELD,IFIELDP,IRESP)
             IF (GNC4) CALL IO_Field_write_nc4(TPFILE,TPFIELD,IFIELDP,IRESP)
          ELSE
             IF (GLFI) CALL IO_Field_write_lfi(TPFILE,TPFIELD,KFIELD,IRESP)
             IF (GNC4) CALL IO_Field_write_nc4(TPFILE,TPFIELD,KFIELD,IRESP)
          END IF
       ELSE ! multiprocesses execution
          CALL MPI_ALLREDUCE(SIZE(KFIELD),ISIZEMAX,1,MNHINT_MPI,MPI_MAX,TPFILE%NMPICOMM,IRESP)
          IF (ISIZEMAX==0) THEN
             CALL PRINT_MSG(NVERB_INFO,'IO','IO_Field_write_byfield_N3','ignoring variable with a zero size ('//TRIM(YRECFM)//')')
             IF (PRESENT(KRESP)) KRESP=0
             RETURN
          END IF

          IF (ISP == TPFILE%NMASTER_RANK) THEN
             ! I/O process case
             CALL ALLOCBUFFER_ll(IFIELDP,KFIELD,YDIR,GALLOC)
          ELSE
             ALLOCATE(IFIELDP(0,0,0))
             GALLOC = .TRUE.
          END IF
          !   
          IF (YDIR == 'XX' .OR. YDIR =='YY') THEN
             CALL GATHER_XXFIELD(YDIR,KFIELD,IFIELDP,TPFILE%NMASTER_RANK,TPFILE%NMPICOMM)
          ELSEIF (YDIR == 'XY') THEN
             IF (LPACK .AND. L2D) THEN
                CALL GATHER_XXFIELD('XX',KFIELD(:,JPHEXT+1,:),IFIELDP(:,1,:),TPFILE%NMASTER_RANK,TPFILE%NMPICOMM)
             ELSE
                CALL GATHER_XYFIELD(KFIELD,IFIELDP,TPFILE%NMASTER_RANK,TPFILE%NMPICOMM)
             END IF
          END IF
          !
          IF (ISP == TPFILE%NMASTER_RANK) THEN
             IF (GLFI) CALL IO_Field_write_lfi(TPFILE,TPFIELD,IFIELDP,IRESP)
             IF (GNC4) CALL IO_Field_write_nc4(TPFILE,TPFIELD,IFIELDP,IRESP)
          END IF
          !
          CALL MPI_BCAST(IRESP,1,MNHINT_MPI,TPFILE%NMASTER_RANK-1,TPFILE%NMPICOMM,IERR)
       END IF
    END IF
    !
    IF (IRESP.NE.0) THEN
      WRITE(YRESP, '( I6 )') IRESP
      YMSG = 'RESP='//YRESP//' when writing '//TRIM(YRECFM)//' in '//TRIM(TPFILE%CNAME)
      CALL PRINT_MSG(NVERB_ERROR,'IO','IO_Field_write_byfield_N3',YMSG)
    END IF
    IF (GALLOC) DEALLOCATE(IFIELDP)
    IF (PRESENT(KRESP)) KRESP = IRESP
    CALL SECOND_MNH2(T22)
    TIMEZ%T_WRIT3D_ALL=TIMEZ%T_WRIT3D_ALL + T22 - T11
    !
  END SUBROUTINE IO_Field_write_byfield_N3


  SUBROUTINE IO_Field_write_byname_L0(TPFILE,HNAME,OFIELD,KRESP)
    !
    !*      0.1   Declarations of arguments
    !
    TYPE(TFILEDATA),             INTENT(IN) :: TPFILE
    CHARACTER(LEN=*),            INTENT(IN) :: HNAME    ! name of the field to write
    LOGICAL,                     INTENT(IN) :: OFIELD   ! array containing the data field
    INTEGER,OPTIONAL,            INTENT(OUT):: KRESP    ! return-code 
    !
    !*      0.2   Declarations of local variables
    !
    INTEGER :: ID ! Index of the field
    INTEGER :: IRESP ! return_code
    !
    CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_write_byname_L0',TRIM(TPFILE%CNAME)//': writing '//TRIM(HNAME))
    !
    CALL FIND_FIELD_ID_FROM_MNHNAME(HNAME,ID,IRESP)
    !
    IF(IRESP==0) CALL IO_Field_write(TPFILE,TFIELDLIST(ID),OFIELD,IRESP)
    !
    IF (PRESENT(KRESP)) KRESP = IRESP
    !
  END SUBROUTINE IO_Field_write_byname_L0

  SUBROUTINE IO_Field_write_byfield_L0(TPFILE,TPFIELD,OFIELD,KRESP)
    USE MODD_IO,               ONLY: GSMONOPROC, ISP
    !
    USE MODE_IO_MANAGE_STRUCT, ONLY: IO_File_find_byname
    !*      0.    DECLARATIONS
    !             ------------
    !
    !
    !*      0.1   Declarations of arguments
    !
    TYPE(TFILEDATA),             INTENT(IN) :: TPFILE
    TYPE(TFIELDDATA),            INTENT(IN) :: TPFIELD
    LOGICAL,                     INTENT(IN) :: OFIELD   ! array containing the data field
    INTEGER,OPTIONAL,            INTENT(OUT):: KRESP    ! return-code 
    !
    !*      0.2   Declarations of local variables
    !
    INTEGER                      :: IERR
    INTEGER                      :: IRESP
    INTEGER                      :: IK_FILE
    LOGICAL                      :: GLFI, GNC4
    TYPE(TFILEDATA),POINTER      :: TZFILE
    CHARACTER(LEN=:),ALLOCATABLE :: YMSG
    CHARACTER(LEN=6)             :: YRESP
    !
    IRESP = 0
    TZFILE => NULL()
    !
    CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_write_byfield_L0',TRIM(TPFILE%CNAME)//': writing '//TRIM(TPFIELD%CMNHNAME))
    !
    CALL IO_Field_metadata_check(TPFIELD,TYPELOG,0,'IO_Field_write_byfield_L0')
    !
    CALL IO_File_write_check(TPFILE,'IO_Field_write_byfield_L0',IRESP)
    !
    CALL IO_Format_write_select(TPFILE,GLFI,GNC4)
    !
    IF (IRESP==0) THEN
       IF (GSMONOPROC) THEN ! sequential execution
          IF (GLFI) CALL IO_Field_write_lfi(TPFILE,TPFIELD,OFIELD,IRESP)
          IF (GNC4) CALL IO_Field_write_nc4(TPFILE,TPFIELD,OFIELD,IRESP)
       ELSE
          IF (ISP == TPFILE%NMASTER_RANK)  THEN
             IF (GLFI) CALL IO_Field_write_lfi(TPFILE,TPFIELD,OFIELD,IRESP)
             IF (GNC4) CALL IO_Field_write_nc4(TPFILE,TPFIELD,OFIELD,IRESP)
          END IF
          !
          CALL MPI_BCAST(IRESP,1,MNHINT_MPI,TPFILE%NMASTER_RANK-1,TPFILE%NMPICOMM,IERR)
       END IF ! multiprocesses execution
#if MNH_SCALARS_IN_SPLITFILES
       IF (TPFILE%NSUBFILES_IOZ>0) THEN
          ! write the data in all Z files
          DO IK_FILE=1,TPFILE%NSUBFILES_IOZ
             TZFILE => TPFILE%TFILES_IOZ(IK_FILE)%TFILE
             IF ( ISP == TZFILE%NMASTER_RANK )  THEN
                IF (GLFI) CALL IO_Field_write_lfi(TZFILE,TPFIELD,OFIELD,IRESP)
                IF (GNC4) CALL IO_Field_write_nc4(TZFILE,TPFIELD,OFIELD,IRESP)
             END IF
          END DO
       ENDIF
#endif
    END IF
    !
    IF (IRESP.NE.0) THEN
      WRITE(YRESP, '( I6 )') IRESP
      YMSG = 'RESP='//YRESP//' when writing '//TRIM(TPFIELD%CMNHNAME)//' in '//TRIM(TPFILE%CNAME)
      CALL PRINT_MSG(NVERB_ERROR,'IO','IO_Field_write_byfield_L0',YMSG)
    END IF
    IF (PRESENT(KRESP)) KRESP = IRESP
  END SUBROUTINE IO_Field_write_byfield_L0


  SUBROUTINE IO_Field_write_byname_L1(TPFILE,HNAME,OFIELD,KRESP)
    !
    !*      0.1   Declarations of arguments
    !
    TYPE(TFILEDATA),             INTENT(IN) :: TPFILE
    CHARACTER(LEN=*),            INTENT(IN) :: HNAME    ! name of the field to write
    LOGICAL,DIMENSION(:),        INTENT(IN) :: OFIELD   ! array containing the data field
    INTEGER,OPTIONAL,            INTENT(OUT):: KRESP    ! return-code 
    !
    !*      0.2   Declarations of local variables
    !
    INTEGER :: ID ! Index of the field
    INTEGER :: IRESP ! return_code
    !
    CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_write_byname_L1',TRIM(TPFILE%CNAME)//': writing '//TRIM(HNAME))
    !
    CALL FIND_FIELD_ID_FROM_MNHNAME(HNAME,ID,IRESP)
    !
    IF(IRESP==0) CALL IO_Field_write(TPFILE,TFIELDLIST(ID),OFIELD,IRESP)
    !
    IF (PRESENT(KRESP)) KRESP = IRESP
    !
  END SUBROUTINE IO_Field_write_byname_L1


  SUBROUTINE IO_Field_write_byfield_L1(TPFILE,TPFIELD,OFIELD,KRESP)
    !
    USE MODD_IO, ONLY: ISP, GSMONOPROC
    !
    USE MODE_ALLOCBUFFER_ll
    USE MODE_GATHER_ll
    !
    IMPLICIT NONE
    !
    !*      0.1   Declarations of arguments
    !
    TYPE(TFILEDATA),              INTENT(IN) :: TPFILE
    TYPE(TFIELDDATA),             INTENT(IN) :: TPFIELD
    LOGICAL,DIMENSION(:),TARGET,  INTENT(IN) :: OFIELD   ! array containing the data field
    INTEGER,OPTIONAL,             INTENT(OUT):: KRESP    ! return-code 
    !
    !*      0.2   Declarations of local variables
    !
    CHARACTER(LEN=28)                        :: YFILEM   ! FM-file name
    CHARACTER(LEN=NMNHNAMELGTMAX)            :: YRECFM   ! name of the article to write
    CHARACTER(LEN=2)                         :: YDIR     ! field form
    INTEGER                                  :: IERR
    INTEGER                                  :: ISIZEMAX
    INTEGER                                  :: IRESP
    LOGICAL,DIMENSION(:),POINTER             :: GFIELDP
    LOGICAL                                  :: GALLOC
    LOGICAL                                  :: GLFI, GNC4
    CHARACTER(LEN=:),ALLOCATABLE             :: YMSG
    CHARACTER(LEN=6)                         :: YRESP
    !
    YFILEM   = TPFILE%CNAME
    YRECFM   = TPFIELD%CMNHNAME
    YDIR     = TPFIELD%CDIR
    !
    IRESP = 0
    GALLOC = .FALSE.
    !
    CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_write_byfield_L1',TRIM(YFILEM)//': writing '//TRIM(YRECFM))
    !
    CALL IO_Field_metadata_check(TPFIELD,TYPELOG,1,'IO_Field_write_byfield_L1')
    !
    CALL IO_File_write_check(TPFILE,'IO_Field_write_byfield_L1',IRESP)
    !
    CALL IO_Format_write_select(TPFILE,GLFI,GNC4)
    !
    IF (IRESP==0) THEN
       IF (GSMONOPROC) THEN ! sequential execution
          IF (GLFI) CALL IO_Field_write_lfi(TPFILE,TPFIELD,OFIELD,IRESP)
          IF (GNC4) CALL IO_Field_write_nc4(TPFILE,TPFIELD,OFIELD,IRESP)
       ELSE ! multiprocesses execution
          CALL MPI_ALLREDUCE(SIZE(OFIELD),ISIZEMAX,1,MNHINT_MPI,MPI_MAX,TPFILE%NMPICOMM,IRESP)
          IF (ISIZEMAX==0) THEN
             CALL PRINT_MSG(NVERB_INFO,'IO','IO_Field_write_byfield_L1','ignoring variable with a zero size ('//TRIM(YRECFM)//')')
             IF (PRESENT(KRESP)) KRESP=0
             RETURN
          END IF

          IF (ISP == TPFILE%NMASTER_RANK)  THEN
             CALL ALLOCBUFFER_ll(GFIELDP,OFIELD,YDIR,GALLOC)
          ELSE
             ALLOCATE(GFIELDP(0))
             GALLOC = .TRUE.
          END IF
          !
          IF (YDIR == 'XX' .OR. YDIR =='YY') THEN
             CALL GATHER_XXFIELD(YDIR,OFIELD,GFIELDP,TPFILE%NMASTER_RANK,TPFILE%NMPICOMM)
          END IF
          !
          IF (ISP == TPFILE%NMASTER_RANK)  THEN
             IF (GLFI) CALL IO_Field_write_lfi(TPFILE,TPFIELD,GFIELDP,IRESP)
             IF (GNC4) CALL IO_Field_write_nc4(TPFILE,TPFIELD,GFIELDP,IRESP)
          END IF
          !
          CALL MPI_BCAST(IRESP,1,MNHINT_MPI,TPFILE%NMASTER_RANK-1,TPFILE%NMPICOMM,IERR)
       END IF
    END IF
    !
    IF (IRESP.NE.0) THEN
      WRITE(YRESP, '( I6 )') IRESP
      YMSG = 'RESP='//YRESP//' when writing '//TRIM(YRECFM)//' in '//TRIM(TPFILE%CNAME)
      CALL PRINT_MSG(NVERB_ERROR,'IO','IO_Field_write_byfield_L1',YMSG)
    END IF
    IF (GALLOC) DEALLOCATE(GFIELDP)
    IF (PRESENT(KRESP)) KRESP = IRESP
    !
  END SUBROUTINE IO_Field_write_byfield_L1


  SUBROUTINE IO_Field_write_byname_C0(TPFILE,HNAME,HFIELD,KRESP)
    !
    !*      0.1   Declarations of arguments
    !
    TYPE(TFILEDATA),             INTENT(IN) :: TPFILE
    CHARACTER(LEN=*),            INTENT(IN) :: HNAME    ! name of the field to write
    CHARACTER(LEN=*),            INTENT(IN) :: HFIELD   ! array containing the data field
    INTEGER,OPTIONAL,            INTENT(OUT):: KRESP    ! return-code 
    !
    !*      0.2   Declarations of local variables
    !
    INTEGER :: ID ! Index of the field
    INTEGER :: IRESP ! return_code
    !
    CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_write_byname_C0',TRIM(TPFILE%CNAME)//': writing '//TRIM(HNAME))
    !
    CALL FIND_FIELD_ID_FROM_MNHNAME(HNAME,ID,IRESP)
    !
    IF(IRESP==0) CALL IO_Field_write(TPFILE,TFIELDLIST(ID),HFIELD,IRESP)
    !
    IF (PRESENT(KRESP)) KRESP = IRESP
    !
  END SUBROUTINE IO_Field_write_byname_C0


  SUBROUTINE IO_Field_write_byfield_C0(TPFILE,TPFIELD,HFIELD,KRESP)
    USE MODD_IO, ONLY: GSMONOPROC, ISP
    !
    !*      0.    DECLARATIONS
    !             ------------
    !
    !
    !*      0.1   Declarations of arguments
    !
    TYPE(TFILEDATA),             INTENT(IN) :: TPFILE
    TYPE(TFIELDDATA),            INTENT(IN) :: TPFIELD
    CHARACTER(LEN=*),            INTENT(IN) :: HFIELD   ! array containing the data field
    INTEGER,OPTIONAL,            INTENT(OUT):: KRESP    ! return-code 
    !
    !*      0.2   Declarations of local variables
    !
    INTEGER                      :: IERR
    INTEGER                      :: IRESP
    LOGICAL                      :: GLFI, GNC4
    CHARACTER(LEN=:),ALLOCATABLE :: YMSG
    CHARACTER(LEN=6)             :: YRESP
    !
    IRESP = 0
    !
    CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_write_byfield_C0',TRIM(TPFILE%CNAME)//': writing '//TRIM(TPFIELD%CMNHNAME))
    !
    CALL IO_Field_metadata_check(TPFIELD,TYPECHAR,0,'IO_Field_write_byfield_C0')
    !
    CALL IO_Format_write_select(TPFILE,GLFI,GNC4)
    !
    IF (LEN(HFIELD)==0 .AND. GLFI) THEN
      CALL PRINT_MSG(NVERB_ERROR,'IO','IO_Field_write_byfield_C0',&
                     'zero-size string not allowed if LFI output for '//TRIM(TPFIELD%CMNHNAME))
    END IF
    !
    CALL IO_File_write_check(TPFILE,'IO_Field_write_byfield_C0',IRESP)
    !
    IF (IRESP==0) THEN
       IF (GSMONOPROC) THEN ! sequential execution
          IF (GLFI) CALL IO_Field_write_lfi(TPFILE,TPFIELD,HFIELD,IRESP)
          IF (GNC4) CALL IO_Field_write_nc4(TPFILE,TPFIELD,HFIELD,IRESP)
       ELSE 
          IF (ISP == TPFILE%NMASTER_RANK)  THEN
             IF (GLFI) CALL IO_Field_write_lfi(TPFILE,TPFIELD,HFIELD,IRESP)
             IF (GNC4) CALL IO_Field_write_nc4(TPFILE,TPFIELD,HFIELD,IRESP)
          END IF
          !
          CALL MPI_BCAST(IRESP,1,MNHINT_MPI,TPFILE%NMASTER_RANK-1,TPFILE%NMPICOMM,IERR)
       END IF
    END IF
    !
    IF (IRESP.NE.0) THEN
      WRITE(YRESP, '( I6 )') IRESP
      YMSG = 'RESP='//YRESP//' when writing '//TRIM(TPFIELD%CMNHNAME)//' in '//TRIM(TPFILE%CNAME)
      CALL PRINT_MSG(NVERB_ERROR,'IO','IO_Field_write_byfield_C0',YMSG)
    END IF
    IF (PRESENT(KRESP)) KRESP = IRESP
  END SUBROUTINE IO_Field_write_byfield_C0


  SUBROUTINE IO_Field_write_byname_C1(TPFILE,HNAME,HFIELD,KRESP)
    !
    !*      0.1   Declarations of arguments
    !
    TYPE(TFILEDATA),              INTENT(IN) :: TPFILE
    CHARACTER(LEN=*),             INTENT(IN) :: HNAME    ! name of the field to write
    CHARACTER(LEN=*),DIMENSION(:),INTENT(IN) :: HFIELD   ! array containing the data field
    INTEGER,OPTIONAL,             INTENT(OUT):: KRESP    ! return-code 
    !
    !*      0.2   Declarations of local variables
    !
    INTEGER :: ID ! Index of the field
    INTEGER :: IRESP ! return_code
    !
    CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_write_byname_C1',TRIM(TPFILE%CNAME)//': writing '//TRIM(HNAME))
    !
    CALL FIND_FIELD_ID_FROM_MNHNAME(HNAME,ID,IRESP)
    !
    IF(IRESP==0) CALL IO_Field_write(TPFILE,TFIELDLIST(ID),HFIELD,IRESP)
    !
    IF (PRESENT(KRESP)) KRESP = IRESP
    !
  END SUBROUTINE IO_Field_write_byname_C1


  SUBROUTINE IO_Field_write_byfield_C1(TPFILE,TPFIELD,HFIELD,KRESP)
    USE MODD_IO, ONLY: GSMONOPROC, ISP
    !
    !*      0.    DECLARATIONS
    !             ------------
    !
    !
    !*      0.1   Declarations of arguments
    !
    TYPE(TFILEDATA),              INTENT(IN) :: TPFILE
    TYPE(TFIELDDATA),             INTENT(IN) :: TPFIELD
    CHARACTER(LEN=*),DIMENSION(:),INTENT(IN) :: HFIELD   ! array containing the data field
    INTEGER,OPTIONAL,             INTENT(OUT):: KRESP    ! return-code 
    !
    !*      0.2   Declarations of local variables
    !
    INTEGER                          :: IERR
    INTEGER                          :: IRESP
    INTEGER                          :: J,JJ
    INTEGER                          :: ILE, IP
    INTEGER,DIMENSION(:),ALLOCATABLE :: IFIELD
    INTEGER                          :: ILENG
    LOGICAL                          :: GLFI, GNC4
    CHARACTER(LEN=:),ALLOCATABLE     :: YMSG
    CHARACTER(LEN=6)                 :: YRESP
    !
    CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_write_byfield_C1',TRIM(TPFILE%CNAME)//': writing '//TRIM(TPFIELD%CMNHNAME))
    !
    CALL IO_Field_metadata_check(TPFIELD,TYPECHAR,1,'IO_Field_write_byfield_C1')
    !
    IRESP = 0
    !
    CALL IO_Format_write_select(TPFILE,GLFI,GNC4)
    !
    IF(GLFI) THEN
      ILE=LEN(HFIELD)
      IP=SIZE(HFIELD)
      ILENG=ILE*IP
      !
      IF (ILENG==0) THEN
        IP=1
        ILE=1
        ILENG=1
        ALLOCATE(IFIELD(1))
        IFIELD(1)=IACHAR(' ')
      ELSE
        ALLOCATE(IFIELD(ILENG))
        DO JJ=1,IP
          DO J=1,ILE
            IFIELD(ILE*(JJ-1)+J)=IACHAR(HFIELD(JJ)(J:J))
          END DO
        END DO
      END IF
    END IF
    !
    CALL IO_File_write_check(TPFILE,'IO_Field_write_byfield_C1',IRESP)
    !
    IF (IRESP==0) THEN
       IF (GSMONOPROC) THEN ! sequential execution
          IF (GLFI) CALL IO_Field_write_lfi(TPFILE,TPFIELD,IFIELD,IRESP)
          IF (GNC4) CALL IO_Field_write_nc4(TPFILE,TPFIELD,HFIELD,IRESP)
       ELSE 
          IF (ISP == TPFILE%NMASTER_RANK)  THEN
             IF (GLFI) CALL IO_Field_write_lfi(TPFILE,TPFIELD,IFIELD,IRESP)
             IF (GNC4) CALL IO_Field_write_nc4(TPFILE,TPFIELD,HFIELD,IRESP)
          END IF
          !
          CALL MPI_BCAST(IRESP,1,MNHINT_MPI,TPFILE%NMASTER_RANK-1,TPFILE%NMPICOMM,IERR)
       END IF
    END IF
    !
    IF (IRESP.NE.0) THEN
      WRITE(YRESP, '( I6 )') IRESP
      YMSG = 'RESP='//YRESP//' when writing '//TRIM(TPFIELD%CMNHNAME)//' in '//TRIM(TPFILE%CNAME)
      CALL PRINT_MSG(NVERB_ERROR,'IO','IO_Field_write_byfield_C1',YMSG)
    END IF
    IF (ALLOCATED(IFIELD)) DEALLOCATE(IFIELD)
    IF (PRESENT(KRESP)) KRESP = IRESP
  END SUBROUTINE IO_Field_write_byfield_C1


  SUBROUTINE IO_Field_write_byname_T0(TPFILE,HNAME,TFIELD,KRESP)
    USE MODD_TYPE_DATE, only: DATE_TIME
    !
    !*      0.1   Declarations of arguments
    !
    TYPE(TFILEDATA),             INTENT(IN) :: TPFILE
    CHARACTER(LEN=*),            INTENT(IN) :: HNAME    ! name of the field to write
    TYPE (DATE_TIME),            INTENT(IN) :: TFIELD   ! array containing the data field
    INTEGER,OPTIONAL,            INTENT(OUT):: KRESP    ! return-code 
    !
    !*      0.2   Declarations of local variables
    !
    INTEGER :: ID ! Index of the field
    INTEGER :: IRESP ! return_code
    !
    CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_write_byname_T0',TRIM(TPFILE%CNAME)//': writing '//TRIM(HNAME))
    !
    CALL FIND_FIELD_ID_FROM_MNHNAME(HNAME,ID,IRESP)
    !
    IF(IRESP==0) CALL IO_Field_write(TPFILE,TFIELDLIST(ID),TFIELD,IRESP)
    !
    IF (PRESENT(KRESP)) KRESP = IRESP
    !
  END SUBROUTINE IO_Field_write_byname_T0


  SUBROUTINE IO_Field_write_byfield_T0(TPFILE,TPFIELD,TFIELD,KRESP)
    USE MODD_IO, ONLY: GSMONOPROC, ISP
    USE MODD_TYPE_DATE, only: DATE_TIME
    !
    !*      0.    DECLARATIONS
    !             ------------
    !
    !
    !*      0.1   Declarations of arguments
    !
    TYPE(TFILEDATA),             INTENT(IN) :: TPFILE
    TYPE(TFIELDDATA),            INTENT(IN) :: TPFIELD
    TYPE (DATE_TIME),            INTENT(IN) :: TFIELD   ! array containing the data field
    INTEGER,OPTIONAL,            INTENT(OUT):: KRESP    ! return-code 
    !
    !*      0.2   Declarations of local variables
    !
    INTEGER                      :: IERR
    INTEGER                      :: IRESP
    LOGICAL                      :: GLFI, GNC4
    CHARACTER(LEN=:),ALLOCATABLE :: YMSG
    CHARACTER(LEN=6)             :: YRESP
    !
    CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_write_byfield_T0',TRIM(TPFILE%CNAME)//': writing '//TRIM(TPFIELD%CMNHNAME))
    !
    CALL IO_Field_metadata_check(TPFIELD,TYPEDATE,0,'IO_Field_write_byfield_T0')
    !
    IRESP = 0
    !
    CALL IO_File_write_check(TPFILE,'IO_Field_write_byfield_T0',IRESP)
    !
    CALL IO_Format_write_select(TPFILE,GLFI,GNC4)
    !
    IF (IRESP==0) THEN
       IF (GSMONOPROC) THEN ! sequential execution
          IF (GLFI) CALL IO_Field_write_lfi(TPFILE,TPFIELD,TFIELD,IRESP)
          IF (GNC4) CALL IO_Field_write_nc4(TPFILE,TPFIELD,TFIELD,IRESP)
       ELSE 
          IF (ISP == TPFILE%NMASTER_RANK)  THEN
             IF (GLFI) CALL IO_Field_write_lfi(TPFILE,TPFIELD,TFIELD,IRESP)
             IF (GNC4) CALL IO_Field_write_nc4(TPFILE,TPFIELD,TFIELD,IRESP)
          END IF
          !
          CALL MPI_BCAST(IRESP,1,MNHINT_MPI,TPFILE%NMASTER_RANK-1,TPFILE%NMPICOMM,IERR)
       END IF
    END IF
    !
    IF (IRESP.NE.0) THEN
      WRITE(YRESP, '( I6 )') IRESP
      YMSG = 'RESP='//YRESP//' when writing '//TRIM(TPFIELD%CMNHNAME)//' in '//TRIM(TPFILE%CNAME)
      CALL PRINT_MSG(NVERB_ERROR,'IO','IO_Field_write_byfield_T0',YMSG)
    END IF
    IF (PRESENT(KRESP)) KRESP = IRESP
  END SUBROUTINE IO_Field_write_byfield_T0


  SUBROUTINE IO_Field_write_byname_T1(TPFILE,HNAME,TFIELD,KRESP)
    USE MODD_TYPE_DATE, only: DATE_TIME
    !
    !*      0.1   Declarations of arguments
    !
    TYPE(TFILEDATA),               INTENT(IN) :: TPFILE
    CHARACTER(LEN=*),              INTENT(IN) :: HNAME    ! name of the field to write
    TYPE (DATE_TIME),DIMENSION(:), INTENT(IN) :: TFIELD   ! array containing the data field
    INTEGER,OPTIONAL,              INTENT(OUT):: KRESP    ! return-code
    !
    !*      0.2   Declarations of local variables
    !
    INTEGER :: ID ! Index of the field
    INTEGER :: IRESP ! return_code
    !
    CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_write_byname_T1',TRIM(TPFILE%CNAME)//': writing '//TRIM(HNAME))
    !
    CALL FIND_FIELD_ID_FROM_MNHNAME(HNAME,ID,IRESP)
    !
    IF(IRESP==0) CALL IO_Field_write(TPFILE,TFIELDLIST(ID),TFIELD,IRESP)
    !
    IF (PRESENT(KRESP)) KRESP = IRESP
    !
  END SUBROUTINE IO_Field_write_byname_T1


  SUBROUTINE IO_Field_write_byfield_T1(TPFILE,TPFIELD,TFIELD,KRESP)
    USE MODD_IO, ONLY: GSMONOPROC, ISP
    USE MODD_TYPE_DATE, only: DATE_TIME
    !
    !*      0.    DECLARATIONS
    !             ------------
    !
    !
    !*      0.1   Declarations of arguments
    !
    TYPE(TFILEDATA),               INTENT(IN) :: TPFILE
    TYPE(TFIELDDATA),              INTENT(IN) :: TPFIELD
    TYPE (DATE_TIME),DIMENSION(:), INTENT(IN) :: TFIELD   ! array containing the data field
    INTEGER,OPTIONAL,              INTENT(OUT):: KRESP    ! return-code
    !
    !*      0.2   Declarations of local variables
    !
    INTEGER                      :: IERR
    INTEGER                      :: IRESP
    LOGICAL                      :: GLFI, GNC4
    CHARACTER(LEN=:),ALLOCATABLE :: YMSG
    CHARACTER(LEN=6)             :: YRESP
    !
    CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_write_byfield_T1',TRIM(TPFILE%CNAME)//': writing '//TRIM(TPFIELD%CMNHNAME))
    !
    CALL IO_Field_metadata_check(TPFIELD,TYPEDATE,1,'IO_Field_write_byfield_T1')
    !
    IRESP = 0
    !
    CALL IO_File_write_check(TPFILE,'IO_Field_write_byfield_T1',IRESP)
    !
    CALL IO_Format_write_select(TPFILE,GLFI,GNC4)
    !
    IF (IRESP==0) THEN
       IF (GSMONOPROC) THEN ! sequential execution
          IF (GLFI) CALL IO_Field_write_lfi(TPFILE,TPFIELD,TFIELD,IRESP)
          IF (GNC4) CALL IO_Field_write_nc4(TPFILE,TPFIELD,TFIELD,IRESP)
       ELSE
          IF (ISP == TPFILE%NMASTER_RANK)  THEN
             IF (GLFI) CALL IO_Field_write_lfi(TPFILE,TPFIELD,TFIELD,IRESP)
             IF (GNC4) CALL IO_Field_write_nc4(TPFILE,TPFIELD,TFIELD,IRESP)
          END IF
          !
          CALL MPI_BCAST(IRESP,1,MNHINT_MPI,TPFILE%NMASTER_RANK-1,TPFILE%NMPICOMM,IERR)
       END IF
    END IF
    !
    IF (IRESP.NE.0) THEN
      WRITE(YRESP, '( I6 )') IRESP
      YMSG = 'RESP='//YRESP//' when writing '//TRIM(TPFIELD%CMNHNAME)//' in '//TRIM(TPFILE%CNAME)
      CALL PRINT_MSG(NVERB_ERROR,'IO','IO_Field_write_byfield_T1',YMSG)
    END IF
    IF (PRESENT(KRESP)) KRESP = IRESP
  END SUBROUTINE IO_Field_write_byfield_T1


  SUBROUTINE IO_Field_write_byname_lb(TPFILE,HNAME,KL3D,PLB,KRESP)
    !
    !*      0.1   Declarations of arguments
    !
    TYPE(TFILEDATA),             INTENT(IN) :: TPFILE
    CHARACTER(LEN=*),            INTENT(IN) :: HNAME    ! name of the field to write
    INTEGER,                     INTENT(IN) :: KL3D     ! size of the LB array in FM
    REAL,DIMENSION(:,:,:),       INTENT(IN) :: PLB      ! array containing the LB field
    INTEGER,OPTIONAL,            INTENT(OUT):: KRESP    ! return-code
    !
    !*      0.2   Declarations of local variables
    !
    INTEGER :: ID ! Index of the field
    INTEGER :: IRESP ! return_code
    !
    CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_write_byname_lb',TRIM(TPFILE%CNAME)//': writing '//TRIM(HNAME))
    !
    CALL FIND_FIELD_ID_FROM_MNHNAME(HNAME,ID,IRESP)
    !
    IF(IRESP==0) CALL IO_Field_write_lb(TPFILE,TFIELDLIST(ID),KL3D,PLB,IRESP)
    !
    IF (PRESENT(KRESP)) KRESP = IRESP
    !
  END SUBROUTINE IO_Field_write_byname_lb


  SUBROUTINE IO_Field_write_byfield_lb(TPFILE,TPFIELD,KL3D,PLB,KRESP)
    !
    USE MODD_IO,            ONLY: GSMONOPROC, ISNPROC, ISP, L1D, L2D, LPACK
    USE MODD_PARAMETERS_ll, ONLY: JPHEXT
    USE MODD_VAR_ll,        ONLY: MNH_STATUSES_IGNORE
    !
    USE MODE_DISTRIB_lb
    USE MODE_TOOLS_ll,      ONLY: GET_GLOBALDIMS_ll
    !
    !
    !*      0.1   Declarations of arguments
    !
    TYPE(TFILEDATA),             INTENT(IN)    :: TPFILE
    TYPE(TFIELDDATA),            INTENT(INOUT) :: TPFIELD
    INTEGER,                     INTENT(IN)    :: KL3D   ! size of the LB array in FM
    REAL,DIMENSION(:,:,:),TARGET,INTENT(IN)    :: PLB    ! array containing the LB field
    INTEGER,OPTIONAL,            INTENT(OUT)   :: KRESP  ! return-code 
    !
    !*      0.2   Declarations of local variables
    !
    CHARACTER(LEN=28)                        :: YFILEM   ! FM-file name
    CHARACTER(LEN=NMNHNAMELGTMAX)            :: YRECFM   ! name of the article to write
    CHARACTER(LEN=4)                         :: YLBTYPE  ! 'LBX','LBXU','LBY' or 'LBYV'
    INTEGER                                  :: IRIM     ! size of the LB area
    INTEGER                                  :: IERR
    INTEGER                                  :: IRESP
    REAL,DIMENSION(:,:,:),ALLOCATABLE,TARGET :: Z3D
    REAL,DIMENSION(:,:,:), POINTER           :: TX3DP
    INTEGER                                  :: IIMAX_ll,IJMAX_ll
    INTEGER                                  :: JI
    INTEGER                                  :: IIB,IIE,IJB,IJE
    INTEGER, DIMENSION(MPI_STATUS_SIZE)      :: STATUS
    INTEGER,ALLOCATABLE,DIMENSION(:)         :: REQ_TAB
    INTEGER                                  :: NB_REQ,IKU
    LOGICAL                                  :: GLFI, GNC4
    TYPE TX_3DP
       REAL,DIMENSION(:,:,:), POINTER    :: X
    END TYPE TX_3DP
    TYPE(TX_3DP),ALLOCATABLE,DIMENSION(:)    :: T_TX3DP
    CHARACTER(LEN=:),ALLOCATABLE             :: YMSG
    CHARACTER(LEN=6)                         :: YRESP
    !
    YFILEM   = TPFILE%CNAME
    YRECFM   = TPFIELD%CMNHNAME
    YLBTYPE  = TPFIELD%CLBTYPE
    !
    IRESP = 0
    !
    CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_write_byfield_lb',TRIM(YFILEM)//': writing '//TRIM(YRECFM))
    !
    IF (YLBTYPE/='LBX' .AND. YLBTYPE/='LBXU' .AND. YLBTYPE/='LBY' .AND. YLBTYPE/='LBYV') THEN
      CALL PRINT_MSG(NVERB_ERROR,'IO','IO_Field_write_byfield_lb','unknown LBTYPE ('//YLBTYPE//')')
      RETURN
    END IF
    !
    IF (TPFIELD%CDIR/='') THEN
      CALL PRINT_MSG(NVERB_WARNING,'IO','IO_Field_write_byfield_lb','CDIR was set for '//TRIM(YRECFM))
      TPFIELD%CDIR=''
    END IF
    !
    IRIM = (KL3D-2*JPHEXT)/2
    IF (KL3D /= 2*(IRIM+JPHEXT)) THEN
       IRESP = -30
       GOTO 1000
    END IF
    !
    CALL IO_File_write_check(TPFILE,'IO_Field_write_byfield_lb',IRESP)
    !
    CALL IO_Format_write_select(TPFILE,GLFI,GNC4)
    !
    IF (IRESP==0) THEN
       IF (GSMONOPROC) THEN  ! sequential execution
          IF (LPACK .AND. L2D) THEN
             TX3DP=>PLB(:,JPHEXT+1:JPHEXT+1,:)
             IF (GLFI) CALL IO_Field_write_lfi(TPFILE,TPFIELD,TX3DP,IRESP)
             IF (GNC4) CALL IO_Field_write_nc4(TPFILE,TPFIELD,TX3DP,IRESP)
          ELSE
             IF (GLFI) CALL IO_Field_write_lfi(TPFILE,TPFIELD,PLB,IRESP)
             IF (GNC4) CALL IO_Field_write_nc4(TPFILE,TPFIELD,PLB,IRESP)
          END IF
       ELSE
          IF (ISP == TPFILE%NMASTER_RANK)  THEN
             ! I/O proc case
             CALL GET_GLOBALDIMS_ll(IIMAX_ll,IJMAX_ll)
             IF (YLBTYPE == 'LBX' .OR. YLBTYPE == 'LBXU') THEN 
                ALLOCATE(Z3D((IRIM+JPHEXT)*2,IJMAX_ll+2*JPHEXT,SIZE(PLB,3)))
             ELSE ! YLBTYPE == 'LBY' .OR. YLBTYPE == 'LBYV' 
                ALLOCATE(Z3D(IIMAX_ll+2*JPHEXT,(IRIM+JPHEXT)*2,SIZE(PLB,3)))
             END IF
             DO JI = 1,ISNPROC
                CALL GET_DISTRIB_lb(YLBTYPE,JI,'FM','WRITE',IRIM,IIB,IIE,IJB,IJE)
                IF (IIB /= 0) THEN
                   TX3DP=>Z3D(IIB:IIE,IJB:IJE,:)
                   IF (ISP /= JI) THEN
                      CALL MPI_RECV(TX3DP,SIZE(TX3DP),MNHREAL_MPI,JI-1,99,TPFILE%NMPICOMM,STATUS,IERR)
                   ELSE
                      CALL GET_DISTRIB_lb(YLBTYPE,JI,'LOC','WRITE',IRIM,IIB,IIE,IJB,IJE)
                      TX3DP = PLB(IIB:IIE,IJB:IJE,:)
                   END IF
                END IF
             END DO
             IF (LPACK .AND. L2D) THEN
                TX3DP=>Z3D(:,JPHEXT+1:JPHEXT+1,:)
             ELSE
                TX3DP=>Z3D
             END IF
             IF (GLFI) CALL IO_Field_write_lfi(TPFILE,TPFIELD,TX3DP,IRESP)
             IF (GNC4) CALL IO_Field_write_nc4(TPFILE,TPFIELD,TX3DP,IRESP)
          ELSE
             NB_REQ=0
             ALLOCATE(REQ_TAB(1))
             ALLOCATE(T_TX3DP(1))
             IKU = SIZE(PLB,3)
             ! Other processes
             CALL GET_DISTRIB_lb(YLBTYPE,ISP,'LOC','WRITE',IRIM,IIB,IIE,IJB,IJE)
             IF (IIB /= 0) THEN
                TX3DP=>PLB(IIB:IIE,IJB:IJE,:)
                NB_REQ = NB_REQ + 1
                ALLOCATE(T_TX3DP(NB_REQ)%X(IIB:IIE,IJB:IJE,IKU))  
                T_TX3DP(NB_REQ)%X=PLB(IIB:IIE,IJB:IJE,:)
                CALL MPI_ISEND(T_TX3DP(NB_REQ)%X,SIZE(TX3DP),MNHREAL_MPI,TPFILE%NMASTER_RANK-1,99, &
                               TPFILE%NMPICOMM,REQ_TAB(NB_REQ),IERR)
                !CALL MPI_BSEND(TX3DP,SIZE(TX3DP),MNHREAL_MPI,TPFILE%NMASTER_RANK-1,99,TPFILE%NMPICOMM,IERR)
             END IF
             IF (NB_REQ .GT.0 ) THEN
                CALL MPI_WAITALL(NB_REQ,REQ_TAB,MNH_STATUSES_IGNORE,IERR)
                DEALLOCATE(T_TX3DP(1)%X) 
             END IF
             DEALLOCATE(T_TX3DP,REQ_TAB)
          END IF
          !
          CALL MPI_BCAST(IRESP,1,MNHINT_MPI,TPFILE%NMASTER_RANK-1,TPFILE%NMPICOMM,IERR)
       END IF
    END IF
    !
1000 CONTINUE
    IF (IRESP.NE.0) THEN
      WRITE(YRESP, '( I6 )') IRESP
      YMSG = 'RESP='//YRESP//' when writing '//TRIM(YRECFM)//' in '//TRIM(TPFILE%CNAME)
      CALL PRINT_MSG(NVERB_ERROR,'IO','IO_Field_write_byfield_lb',YMSG)
    END IF
    !
    IF (ALLOCATED(Z3D)) DEALLOCATE(Z3D)
    IF (PRESENT(KRESP)) KRESP = IRESP
  END SUBROUTINE IO_Field_write_byfield_lb


  SUBROUTINE IO_Field_write_box_byfield_X5(TPFILE,TPFIELD,HBUDGET,PFIELD,KXOBOX,KXEBOX,KYOBOX,KYEBOX,KRESP)
    !
    USE MODD_IO, ONLY: GSMONOPROC, ISP
    !
    USE MODE_GATHER_ll
    !
    !
    !*      0.1   Declarations of arguments
    !
    TYPE(TFILEDATA),                 INTENT(IN) :: TPFILE
    TYPE(TFIELDDATA),                INTENT(IN) :: TPFIELD
    CHARACTER(LEN=*),                INTENT(IN) :: HBUDGET  ! 'BUDGET' (budget)  or 'OTHER' (MesoNH field)
    REAL,DIMENSION(:,:,:,:,:),TARGET,INTENT(IN) :: PFIELD   ! array containing the data field
    INTEGER,                         INTENT(IN) :: KXOBOX   ! 
    INTEGER,                         INTENT(IN) :: KXEBOX   ! Global coordinates of the box
    INTEGER,                         INTENT(IN) :: KYOBOX   ! 
    INTEGER,                         INTENT(IN) :: KYEBOX   ! 
    INTEGER,OPTIONAL,                INTENT(OUT):: KRESP    ! return-code 
    !
    !*      0.2   Declarations of local variables
    !
    INTEGER                             :: IERR
    INTEGER                             :: IRESP
    REAL,DIMENSION(:,:,:,:,:),POINTER   :: ZFIELDP
    LOGICAL                             :: GALLOC
    LOGICAL                             :: GLFI, GNC4
    CHARACTER(LEN=:),ALLOCATABLE        :: YMSG
    CHARACTER(LEN=6)                    :: YRESP
    !
    CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_write_box_byfield_X5',TRIM(TPFILE%CNAME)//': writing '//TRIM(TPFIELD%CMNHNAME))
    !
    IRESP = 0
    GALLOC = .FALSE.
    !
    CALL IO_File_write_check(TPFILE,'IO_Field_write_box_byfield_X5',IRESP)
    !
    CALL IO_Format_write_select(TPFILE,GLFI,GNC4)
    !
    IF (IRESP==0) THEN
       IF (GSMONOPROC) THEN ! sequential execution
          IF (HBUDGET /= 'BUDGET') THEN
             ! take the sub-section of PFIELD defined by the box
             ZFIELDP=>PFIELD(KXOBOX:KXEBOX,KYOBOX:KYEBOX,:,:,:)
          ELSE
             ! take the field as a budget
             ZFIELDP=>PFIELD
          END IF
          IF (GLFI) CALL IO_Field_write_lfi(TPFILE,TPFIELD,ZFIELDP,IRESP)
          IF (GNC4) CALL IO_Field_write_nc4(TPFILE,TPFIELD,ZFIELDP,IRESP)
       ELSE ! multiprocesses execution
          IF (ISP == TPFILE%NMASTER_RANK)  THEN
             ! Allocate the box
             ALLOCATE(ZFIELDP(KXEBOX-KXOBOX+1,KYEBOX-KYOBOX+1,SIZE(PFIELD,3),&
                  & SIZE(PFIELD,4),SIZE(PFIELD,5)))
             GALLOC = .TRUE.
          ELSE
             ALLOCATE(ZFIELDP(0,0,0,0,0))
             GALLOC = .TRUE.
          END IF
          !
          CALL GATHER_XYFIELD(PFIELD,ZFIELDP,TPFILE%NMASTER_RANK,TPFILE%NMPICOMM,&
               & KXOBOX,KXEBOX,KYOBOX,KYEBOX,HBUDGET)
          !
          IF (ISP == TPFILE%NMASTER_RANK)  THEN
             IF (GLFI) CALL IO_Field_write_lfi(TPFILE,TPFIELD,ZFIELDP,IRESP)
             IF (GNC4) CALL IO_Field_write_nc4(TPFILE,TPFIELD,ZFIELDP,IRESP)
          END IF
          !
          CALL MPI_BCAST(IRESP,1,MNHINT_MPI,TPFILE%NMASTER_RANK-1,TPFILE%NMPICOMM,IERR)
       END IF ! multiprocesses execution
    END IF
    !
    IF (IRESP.NE.0) THEN
      WRITE(YRESP, '( I6 )') IRESP
      YMSG = 'RESP='//YRESP//' when writing '//TRIM(TPFIELD%CMNHNAME)//' in '//TRIM(TPFILE%CNAME)
      CALL PRINT_MSG(NVERB_ERROR,'IO','IO_Field_write_box_byfield_X5',YMSG)
    END IF
    IF (GALLOC) DEALLOCATE(ZFIELDP)
    IF (PRESENT(KRESP)) KRESP = IRESP
  END SUBROUTINE IO_Field_write_box_byfield_X5


SUBROUTINE IO_Fieldlist_write(TPOUTPUT)
!
USE MODE_MODELN_HANDLER, ONLY: GET_CURRENT_MODEL_INDEX
!
IMPLICIT NONE
!
TYPE(TOUTBAK),    INTENT(IN)  :: TPOUTPUT !Output structure
!
INTEGER :: IDX
INTEGER :: IMI
INTEGER :: JI
!
IMI = GET_CURRENT_MODEL_INDEX()
!
DO JI = 1,SIZE(TPOUTPUT%NFIELDLIST)
  IDX = TPOUTPUT%NFIELDLIST(JI)
  NDIMS: SELECT CASE (TFIELDLIST(IDX)%NDIMS)
    !
    !0D output
    !
    CASE (0)
      NTYPE0D: SELECT CASE (TFIELDLIST(IDX)%NTYPE)
        !
        !0D real
        !
        CASE (TYPEREAL)
          IF ( .NOT.ALLOCATED(TFIELDLIST(IDX)%TFIELD_X0D) ) THEN
            call Print_msg( NVERB_ERROR, 'IO', 'IO_Fieldlist_write', trim(tfieldlist(idx)%cmnhname)// &
                            ': TFIELD_X0D is NOT allocated ' )
          END IF
          IF ( .NOT.ASSOCIATED(TFIELDLIST(IDX)%TFIELD_X0D(IMI)%DATA) ) THEN
            call Print_msg( NVERB_ERROR, 'IO', 'IO_Fieldlist_write', trim(tfieldlist(idx)%cmnhname)// &
                            ': TFIELD_X0D%DATA is NOT associated' )
          END IF
          IF ( TFIELDLIST(IDX)%CLBTYPE == 'NONE' ) THEN
            CALL IO_Field_write(TPOUTPUT%TFILE,TFIELDLIST(IDX),TFIELDLIST(IDX)%TFIELD_X0D(IMI)%DATA)
          ELSE
            call Print_msg( NVERB_ERROR, 'IO', 'IO_Fieldlist_write', trim(tfieldlist(idx)%cmnhname)// &
                            ': CLBTYPE/=NONE not allowed for 0D real fields' )
          END IF
        !
        !0D integer
        !
        CASE (TYPEINT)
          IF ( .NOT.ALLOCATED(TFIELDLIST(IDX)%TFIELD_N0D) ) THEN
            call Print_msg( NVERB_ERROR, 'IO', 'IO_Fieldlist_write', trim(tfieldlist(idx)%cmnhname)// &
                            ': TFIELD_N0D is NOT allocated ' )
          END IF
          IF ( .NOT.ASSOCIATED(TFIELDLIST(IDX)%TFIELD_N0D(IMI)%DATA) ) THEN
            call Print_msg( NVERB_ERROR, 'IO', 'IO_Fieldlist_write', trim(tfieldlist(idx)%cmnhname)// &
                            ': TFIELD_N0D%DATA is NOT associated' )
          END IF
          IF ( TFIELDLIST(IDX)%CLBTYPE == 'NONE' ) THEN
            CALL IO_Field_write(TPOUTPUT%TFILE,TFIELDLIST(IDX),TFIELDLIST(IDX)%TFIELD_N0D(IMI)%DATA)
          ELSE
            call Print_msg( NVERB_ERROR, 'IO', 'IO_Fieldlist_write', trim(tfieldlist(idx)%cmnhname)// &
                            ': CLBTYPE/=NONE not allowed for 0D integer fields' )
          END IF
        !
        !0D logical
        !
        CASE (TYPELOG)
          IF ( .NOT.ALLOCATED(TFIELDLIST(IDX)%TFIELD_L0D) ) THEN
            call Print_msg( NVERB_ERROR, 'IO', 'IO_Fieldlist_write', trim(tfieldlist(idx)%cmnhname)// &
                            ': TFIELD_L0D is NOT allocated ' )
          END IF
          IF ( .NOT.ASSOCIATED(TFIELDLIST(IDX)%TFIELD_L0D(IMI)%DATA) ) THEN
            call Print_msg( NVERB_ERROR, 'IO', 'IO_Fieldlist_write', trim(tfieldlist(idx)%cmnhname)// &
                            ': TFIELD_L0D%DATA is NOT associated' )
          END IF
          IF ( TFIELDLIST(IDX)%CLBTYPE == 'NONE' ) THEN
            CALL IO_Field_write(TPOUTPUT%TFILE,TFIELDLIST(IDX),TFIELDLIST(IDX)%TFIELD_L0D(IMI)%DATA)
          ELSE
            call Print_msg( NVERB_ERROR, 'IO', 'IO_Fieldlist_write', trim(tfieldlist(idx)%cmnhname)// &
                            ': CLBTYPE/=NONE not allowed for 0D logical fields' )
          END IF
        !
        !0D string
        !
        CASE (TYPECHAR)
          IF ( .NOT.ALLOCATED(TFIELDLIST(IDX)%TFIELD_C0D) ) THEN
            call Print_msg( NVERB_ERROR, 'IO', 'IO_Fieldlist_write', trim(tfieldlist(idx)%cmnhname)// &
                            ': TFIELD_C0D is NOT allocated ' )
          END IF
          IF ( .NOT.ASSOCIATED(TFIELDLIST(IDX)%TFIELD_C0D(IMI)%DATA) ) THEN
            call Print_msg( NVERB_ERROR, 'IO', 'IO_Fieldlist_write', trim(tfieldlist(idx)%cmnhname)// &
                            ': TFIELD_C0D%DATA is NOT associated' )
          END IF
          IF ( TFIELDLIST(IDX)%CLBTYPE == 'NONE' ) THEN
            CALL IO_Field_write(TPOUTPUT%TFILE,TFIELDLIST(IDX),TFIELDLIST(IDX)%TFIELD_C0D(IMI)%DATA)
          ELSE
            call Print_msg( NVERB_ERROR, 'IO', 'IO_Fieldlist_write', trim(tfieldlist(idx)%cmnhname)// &
                            ': CLBTYPE/=NONE not allowed for 0D character fields' )
          END IF
        !
        !0D date/time
        !
        CASE (TYPEDATE)
          IF ( .NOT.ALLOCATED(TFIELDLIST(IDX)%TFIELD_T0D) ) THEN
            call Print_msg( NVERB_ERROR, 'IO', 'IO_Fieldlist_write', trim(tfieldlist(idx)%cmnhname)// &
                            ': TFIELD_T0D is NOT allocated ' )
          END IF
          IF ( .NOT.ASSOCIATED(TFIELDLIST(IDX)%TFIELD_T0D(IMI)%DATA) ) THEN
            call Print_msg( NVERB_ERROR, 'IO', 'IO_Fieldlist_write', trim(tfieldlist(idx)%cmnhname)// &
                            ': TFIELD_T0D%DATA is NOT associated' )
          END IF
          IF ( TFIELDLIST(IDX)%CLBTYPE == 'NONE' ) THEN
            CALL IO_Field_write(TPOUTPUT%TFILE,TFIELDLIST(IDX),TFIELDLIST(IDX)%TFIELD_T0D(IMI)%DATA)
          ELSE
            call Print_msg( NVERB_ERROR, 'IO', 'IO_Fieldlist_write', trim(tfieldlist(idx)%cmnhname)// &
                            ': CLBTYPE/=NONE not allowed for 0D date/time fields' )
          END IF
        !
        !0D other types
        !
        CASE DEFAULT
          call Print_msg( NVERB_ERROR, 'IO', 'IO_Fieldlist_write', trim(tfieldlist(idx)%cmnhname)// &
                          ': type not yet supported for 0D output' )
      END SELECT NTYPE0D
    !
    !1D output
    !
    CASE (1)
      NTYPE1D: SELECT CASE (TFIELDLIST(IDX)%NTYPE)
        !
        !1D real
        !
        CASE (TYPEREAL)
          IF ( .NOT.ALLOCATED(TFIELDLIST(IDX)%TFIELD_X1D) ) THEN
            call Print_msg( NVERB_ERROR, 'IO', 'IO_Fieldlist_write', trim(tfieldlist(idx)%cmnhname)// &
                            ': TFIELD_X1D is NOT allocated ' )
          END IF
          IF ( .NOT.ASSOCIATED(TFIELDLIST(IDX)%TFIELD_X1D(IMI)%DATA) ) THEN
            call Print_msg( NVERB_ERROR, 'IO', 'IO_Fieldlist_write', trim(tfieldlist(idx)%cmnhname)// &
                            ': TFIELD_X1D%DATA is NOT associated' )
          END IF
          IF ( TFIELDLIST(IDX)%CLBTYPE == 'NONE' ) THEN
            CALL IO_Field_write(TPOUTPUT%TFILE,TFIELDLIST(IDX),TFIELDLIST(IDX)%TFIELD_X1D(IMI)%DATA)
          ELSE
            call Print_msg( NVERB_ERROR, 'IO', 'IO_Fieldlist_write', trim(tfieldlist(idx)%cmnhname)// &
                            ': CLBTYPE/=NONE not allowed for 1D real fields' )
          END IF
        !
        !1D integer
        !
        CASE (TYPEINT)
          IF ( .NOT.ALLOCATED(TFIELDLIST(IDX)%TFIELD_N1D) ) THEN
            call Print_msg( NVERB_ERROR, 'IO', 'IO_Fieldlist_write', trim(tfieldlist(idx)%cmnhname)// &
                            ': TFIELD_N1D is NOT allocated ' )
          END IF
          IF ( .NOT.ASSOCIATED(TFIELDLIST(IDX)%TFIELD_N1D(IMI)%DATA) ) THEN
            call Print_msg( NVERB_ERROR, 'IO', 'IO_Fieldlist_write', trim(tfieldlist(idx)%cmnhname)// &
                            ': TFIELD_N1D%DATA is NOT associated' )
          END IF
          IF ( TFIELDLIST(IDX)%CLBTYPE == 'NONE' ) THEN
            CALL IO_Field_write(TPOUTPUT%TFILE,TFIELDLIST(IDX),TFIELDLIST(IDX)%TFIELD_N1D(IMI)%DATA)
          ELSE
            call Print_msg( NVERB_ERROR, 'IO', 'IO_Fieldlist_write', trim(tfieldlist(idx)%cmnhname)// &
                            ': CLBTYPE/=NONE not allowed for 1D integer fields' )
          END IF
        !
        !1D logical
        !
        CASE (TYPELOG)
          IF ( .NOT.ALLOCATED(TFIELDLIST(IDX)%TFIELD_L1D) ) THEN
            call Print_msg( NVERB_ERROR, 'IO', 'IO_Fieldlist_write', trim(tfieldlist(idx)%cmnhname)// &
                            ': TFIELD_L1D is NOT allocated ' )
          END IF
          IF ( .NOT.ASSOCIATED(TFIELDLIST(IDX)%TFIELD_L1D(IMI)%DATA) ) THEN
            call Print_msg( NVERB_ERROR, 'IO', 'IO_Fieldlist_write', trim(tfieldlist(idx)%cmnhname)// &
                            ': TFIELD_L1D%DATA is NOT associated' )
          END IF
          IF ( TFIELDLIST(IDX)%CLBTYPE == 'NONE' ) THEN
            CALL IO_Field_write(TPOUTPUT%TFILE,TFIELDLIST(IDX),TFIELDLIST(IDX)%TFIELD_L1D(IMI)%DATA)
          ELSE
            call Print_msg( NVERB_ERROR, 'IO', 'IO_Fieldlist_write', trim(tfieldlist(idx)%cmnhname)// &
                            ': CLBTYPE/=NONE not allowed for 1D logical fields' )
          END IF
        !
        !1D string
        !
        CASE (TYPECHAR)
          IF ( .NOT.ALLOCATED(TFIELDLIST(IDX)%TFIELD_C1D) ) THEN
            call Print_msg( NVERB_ERROR, 'IO', 'IO_Fieldlist_write', trim(tfieldlist(idx)%cmnhname)// &
                            ': TFIELD_C1D is NOT allocated ' )
          END IF
          IF ( .NOT.ASSOCIATED(TFIELDLIST(IDX)%TFIELD_C1D(IMI)%DATA) ) THEN
            call Print_msg( NVERB_ERROR, 'IO', 'IO_Fieldlist_write', trim(tfieldlist(idx)%cmnhname)// &
                            ': TFIELD_C1D%DATA is NOT associated' )
          END IF
          IF ( TFIELDLIST(IDX)%CLBTYPE == 'NONE' ) THEN
            CALL IO_Field_write(TPOUTPUT%TFILE,TFIELDLIST(IDX),TFIELDLIST(IDX)%TFIELD_C1D(IMI)%DATA)
          ELSE
            call Print_msg( NVERB_ERROR, 'IO', 'IO_Fieldlist_write', trim(tfieldlist(idx)%cmnhname)// &
                            ': CLBTYPE/=NONE not allowed for 1D character fields' )
          END IF
        !
        !1D date/time
        !
        CASE (TYPEDATE)
          IF ( .NOT.ALLOCATED(TFIELDLIST(IDX)%TFIELD_T1D) ) THEN
            call Print_msg( NVERB_ERROR, 'IO', 'IO_Fieldlist_write', trim(tfieldlist(idx)%cmnhname)// &
                            ': TFIELD_T1D is NOT allocated ' )
          END IF
          IF ( .NOT.ASSOCIATED(TFIELDLIST(IDX)%TFIELD_T1D(IMI)%DATA) ) THEN
            call Print_msg( NVERB_ERROR, 'IO', 'IO_Fieldlist_write', trim(tfieldlist(idx)%cmnhname)// &
                            ': TFIELD_T1D%DATA is NOT associated' )
          END IF
          IF ( TFIELDLIST(IDX)%CLBTYPE == 'NONE' ) THEN
            CALL IO_Field_write(TPOUTPUT%TFILE,TFIELDLIST(IDX),TFIELDLIST(IDX)%TFIELD_T1D(IMI)%DATA)
          ELSE
            call Print_msg( NVERB_ERROR, 'IO', 'IO_Fieldlist_write', trim(tfieldlist(idx)%cmnhname)// &
                            ': CLBTYPE/=NONE not allowed for 1D date/time fields' )
          END IF
        !
        !1D other types
        !
        CASE DEFAULT
          call Print_msg( NVERB_ERROR, 'IO', 'IO_Fieldlist_write', trim(tfieldlist(idx)%cmnhname)// &
                          ': type not yet supported for 1D output' )
      END SELECT NTYPE1D
    !
    !2D output
    !
    CASE (2)
      NTYPE2D: SELECT CASE (TFIELDLIST(IDX)%NTYPE)
        !
        !2D real
        !
        CASE (TYPEREAL)
          IF ( .NOT.ALLOCATED(TFIELDLIST(IDX)%TFIELD_X2D) ) THEN
            call Print_msg( NVERB_ERROR, 'IO', 'IO_Fieldlist_write', trim(tfieldlist(idx)%cmnhname)// &
                            ': TFIELD_X2D is NOT allocated ' )
          END IF
          IF ( .NOT.ASSOCIATED(TFIELDLIST(IDX)%TFIELD_X2D(IMI)%DATA) ) THEN
            call Print_msg( NVERB_ERROR, 'IO', 'IO_Fieldlist_write', trim(tfieldlist(idx)%cmnhname)// &
                            ': TFIELD_X2D%DATA is NOT associated' )
          END IF
          IF ( TFIELDLIST(IDX)%CLBTYPE == 'NONE' ) THEN
            CALL IO_Field_write(TPOUTPUT%TFILE,TFIELDLIST(IDX),TFIELDLIST(IDX)%TFIELD_X2D(IMI)%DATA)
          ELSE
            call Print_msg( NVERB_ERROR, 'IO', 'IO_Fieldlist_write', trim(tfieldlist(idx)%cmnhname)// &
                            ': CLBTYPE/=NONE not allowed for 2D real fields' )
          END IF
        !
        !2D integer
        !
        CASE (TYPEINT)
          IF ( .NOT.ALLOCATED(TFIELDLIST(IDX)%TFIELD_N2D) ) THEN
            call Print_msg( NVERB_ERROR, 'IO', 'IO_Fieldlist_write', trim(tfieldlist(idx)%cmnhname)// &
                            ': TFIELD_N2D is NOT allocated ' )
          END IF
          IF ( .NOT.ASSOCIATED(TFIELDLIST(IDX)%TFIELD_N2D(IMI)%DATA) ) THEN
            call Print_msg( NVERB_ERROR, 'IO', 'IO_Fieldlist_write', trim(tfieldlist(idx)%cmnhname)// &
                            ': TFIELD_N2D%DATA is NOT associated' )
          END IF
          IF ( TFIELDLIST(IDX)%CLBTYPE == 'NONE' ) THEN
            CALL IO_Field_write(TPOUTPUT%TFILE,TFIELDLIST(IDX),TFIELDLIST(IDX)%TFIELD_N2D(IMI)%DATA)
          ELSE
            call Print_msg( NVERB_ERROR, 'IO', 'IO_Fieldlist_write', trim(tfieldlist(idx)%cmnhname)// &
                            ': CLBTYPE/=NONE not allowed for 2D integer fields' )
          END IF
        !
        !2D other types
        !
        CASE DEFAULT
          call Print_msg( NVERB_ERROR, 'IO', 'IO_Fieldlist_write', trim(tfieldlist(idx)%cmnhname)// &
                          ': type not yet supported for 2D output' )
      END SELECT NTYPE2D
    !
    !3D output
    !
    CASE (3)
      NTYPE3D: SELECT CASE (TFIELDLIST(IDX)%NTYPE)
        !
        !3D real
        !
        CASE (TYPEREAL)
          IF ( .NOT.ALLOCATED(TFIELDLIST(IDX)%TFIELD_X3D) ) THEN
            call Print_msg( NVERB_ERROR, 'IO', 'IO_Fieldlist_write', trim(tfieldlist(idx)%cmnhname)// &
                            ': TFIELD_X3D is NOT allocated ' )
          END IF
          IF ( .NOT.ASSOCIATED(TFIELDLIST(IDX)%TFIELD_X3D(IMI)%DATA) ) THEN
            call Print_msg( NVERB_ERROR, 'IO', 'IO_Fieldlist_write', trim(tfieldlist(idx)%cmnhname)// &
                            ': TFIELD_X3D%DATA is NOT associated' )
          END IF
          IF ( TFIELDLIST(IDX)%CLBTYPE == 'NONE' ) THEN
            CALL IO_Field_write(TPOUTPUT%TFILE,TFIELDLIST(IDX),TFIELDLIST(IDX)%TFIELD_X3D(IMI)%DATA)
          ELSE
            call Print_msg( NVERB_ERROR, 'IO', 'IO_Fieldlist_write', trim(tfieldlist(idx)%cmnhname)// &
                            ': CLBTYPE/=NONE not (yet) allowed for 3D real fields' )
            !PW: TODO?: add missing field in TFIELDLIST?
            !CALL IO_Field_write_lb(TPOUTPUT%TFILE,TFIELDLIST(IDX),***,TFIELDLIST(IDX)%TFIELD_X3D(IMI)%DATA)
          END IF
        !
        !3D integer
        !
        CASE (TYPEINT)
          IF ( .NOT.ALLOCATED(TFIELDLIST(IDX)%TFIELD_N3D) ) THEN
            call Print_msg( NVERB_ERROR, 'IO', 'IO_Fieldlist_write', trim(tfieldlist(idx)%cmnhname)// &
                            ': TFIELD_N3D is NOT allocated ' )
          END IF
          IF ( .NOT.ASSOCIATED(TFIELDLIST(IDX)%TFIELD_N3D(IMI)%DATA) ) THEN
            call Print_msg( NVERB_ERROR, 'IO', 'IO_Fieldlist_write', trim(tfieldlist(idx)%cmnhname)// &
                            ': TFIELD_N3D%DATA is NOT associated' )
          END IF
          IF ( TFIELDLIST(IDX)%CLBTYPE == 'NONE' ) THEN
            CALL IO_Field_write(TPOUTPUT%TFILE,TFIELDLIST(IDX),TFIELDLIST(IDX)%TFIELD_N3D(IMI)%DATA)
          ELSE
            call Print_msg( NVERB_ERROR, 'IO', 'IO_Fieldlist_write', trim(tfieldlist(idx)%cmnhname)// &
                            ': CLBTYPE/=NONE not (yet) allowed for 3D integer fields' )
            !PW: TODO?: add missing field in TFIELDLIST?
            !CALL IO_Field_write_lb(TPOUTPUT%TFILE,TFIELDLIST(IDX),***,TFIELDLIST(IDX)%TFIELD_N3D(IMI)%DATA)
          END IF
        !
        !3D other types
        !
        CASE DEFAULT
          call Print_msg( NVERB_ERROR, 'IO', 'IO_Fieldlist_write', trim(tfieldlist(idx)%cmnhname)// &
                          ': type not yet supported for 3D output' )
      END SELECT NTYPE3D
    !
    !4D output
    !
    CASE (4)
      NTYPE4D: SELECT CASE (TFIELDLIST(IDX)%NTYPE)
        !
        !4D real
        !
        CASE (TYPEREAL)
          IF ( .NOT.ALLOCATED(TFIELDLIST(IDX)%TFIELD_X4D) ) THEN
            call Print_msg( NVERB_ERROR, 'IO', 'IO_Fieldlist_write', trim(tfieldlist(idx)%cmnhname)// &
                            ': TFIELD_X4D is NOT allocated ' )
          END IF
          IF ( .NOT.ASSOCIATED(TFIELDLIST(IDX)%TFIELD_X4D(IMI)%DATA) ) THEN
            call Print_msg( NVERB_ERROR, 'IO', 'IO_Fieldlist_write', trim(tfieldlist(idx)%cmnhname)// &
                            ': TFIELD_X4D%DATA is NOT associated' )
          END IF
          IF ( TFIELDLIST(IDX)%CLBTYPE == 'NONE' ) THEN
            CALL IO_Field_write(TPOUTPUT%TFILE,TFIELDLIST(IDX),TFIELDLIST(IDX)%TFIELD_X4D(IMI)%DATA)
          ELSE
            call Print_msg( NVERB_ERROR, 'IO', 'IO_Fieldlist_write', trim(tfieldlist(idx)%cmnhname)// &
                            ': CLBTYPE/=NONE not (yet) allowed for 4D real fields' )
            !PW: TODO?: add missing field in TFIELDLIST?
            !CALL IO_Field_write_lb(TPOUTPUT%TFILE,TFIELDLIST(IDX),***,TFIELDLIST(IDX)%TFIELD_X4D(IMI)%DATA)
          END IF
        !
        !4D other types
        !
        CASE DEFAULT
          call Print_msg( NVERB_ERROR, 'IO', 'IO_Fieldlist_write', trim(tfieldlist(idx)%cmnhname)// &
                          ': type not yet supported for 4D output' )
      END SELECT NTYPE4D
    !
    !5D output
    !
    CASE (5)
      NTYPE5D: SELECT CASE (TFIELDLIST(IDX)%NTYPE)
        !
        !5D real
        !
        CASE (TYPEREAL)
          IF ( .NOT.ALLOCATED(TFIELDLIST(IDX)%TFIELD_X5D) ) THEN
            call Print_msg( NVERB_ERROR, 'IO', 'IO_Fieldlist_write', trim(tfieldlist(idx)%cmnhname)// &
                            ': TFIELD_X5D is NOT allocated ' )
          END IF
          IF ( .NOT.ASSOCIATED(TFIELDLIST(IDX)%TFIELD_X5D(IMI)%DATA) ) THEN
            call Print_msg( NVERB_ERROR, 'IO', 'IO_Fieldlist_write', trim(tfieldlist(idx)%cmnhname)// &
                            ': TFIELD_X5D%DATA is NOT associated' )
          END IF
          IF ( TFIELDLIST(IDX)%CLBTYPE == 'NONE' ) THEN
            CALL IO_Field_write(TPOUTPUT%TFILE,TFIELDLIST(IDX),TFIELDLIST(IDX)%TFIELD_X5D(IMI)%DATA)
          ELSE
            call Print_msg( NVERB_ERROR, 'IO', 'IO_Fieldlist_write', trim(tfieldlist(idx)%cmnhname)// &
                            ': CLBTYPE/=NONE not (yet) allowed for 5D real fields' )
            !PW: TODO?: add missing field in TFIELDLIST?
            !CALL IO_Field_write_lb(TPOUTPUT%TFILE,TFIELDLIST(IDX),***,TFIELDLIST(IDX)%TFIELD_X5D(IMI)%DATA)
          END IF
        !
        !5D other types
        !
        CASE DEFAULT
          call Print_msg( NVERB_ERROR, 'IO', 'IO_Fieldlist_write', trim(tfieldlist(idx)%cmnhname)// &
                          ': type not yet supported for 5D output' )
      END SELECT NTYPE5D
    !
    !6D output
    !
    CASE (6)
      NTYPE6D: SELECT CASE (TFIELDLIST(IDX)%NTYPE)
        !
        !6D real
        !
        CASE (TYPEREAL)
          IF ( .NOT.ALLOCATED(TFIELDLIST(IDX)%TFIELD_X6D) ) THEN
            call Print_msg( NVERB_ERROR, 'IO', 'IO_Fieldlist_write', trim(tfieldlist(idx)%cmnhname)// &
                            ': TFIELD_X6D is NOT allocated ' )
          END IF
          IF ( .NOT.ASSOCIATED(TFIELDLIST(IDX)%TFIELD_X6D(IMI)%DATA) ) THEN
            call Print_msg( NVERB_ERROR, 'IO', 'IO_Fieldlist_write', trim(tfieldlist(idx)%cmnhname)// &
                            ': TFIELD_X6D%DATA is NOT associated' )
          END IF
          IF ( TFIELDLIST(IDX)%CLBTYPE == 'NONE' ) THEN
            CALL IO_Field_write(TPOUTPUT%TFILE,TFIELDLIST(IDX),TFIELDLIST(IDX)%TFIELD_X6D(IMI)%DATA)
          ELSE
            call Print_msg( NVERB_ERROR, 'IO', 'IO_Fieldlist_write', trim(tfieldlist(idx)%cmnhname)// &
                            ': CLBTYPE/=NONE not (yet) allowed for 6D real fields' )
            !PW: TODO?: add missing field in TFIELDLIST?
            !CALL IO_Field_write_lb(TPOUTPUT%TFILE,TFIELDLIST(IDX),***,TFIELDLIST(IDX)%TFIELD_X6D(IMI)%DATA)
          END IF
        !
        !6D other types
        !
        CASE DEFAULT
          call Print_msg( NVERB_ERROR, 'IO', 'IO_Fieldlist_write', trim(tfieldlist(idx)%cmnhname)// &
                          ': type not yet supported for 6D output' )
      END SELECT NTYPE6D
    !
    !Other number of dimensions
    !
    CASE DEFAULT
      call Print_msg( NVERB_ERROR, 'IO', 'IO_Fieldlist_write', trim(tfieldlist(idx)%cmnhname)// &
                          ': number of dimensions not yet supported' )
  END SELECT NDIMS
END DO
!
END SUBROUTINE IO_Fieldlist_write


SUBROUTINE IO_Field_user_write(TPOUTPUT)
!
#if 0
USE MODD_DYN_n,      ONLY: XTSTEP
USE MODD_FIELD_n,    ONLY: XUT, XVT, XRT, XTHT
USE MODD_PARAMETERS, ONLY: JPVEXT
USE MODD_PRECIP_n,   ONLY: XINPRR
#endif
!
IMPLICIT NONE
!
TYPE(TOUTBAK),    INTENT(IN)  :: TPOUTPUT !Output structure
!
TYPE(TFIELDDATA) :: TZFIELD
!
#if 0
INTEGER          :: IKB
!
IKB=JPVEXT+1
!
TZFIELD%CMNHNAME   = 'UTLOW'
TZFIELD%CSTDNAME   = 'x_wind'
TZFIELD%CLONGNAME  = ''
TZFIELD%CUNITS     = 'm s-1'
TZFIELD%CDIR       = 'XY'
TZFIELD%CCOMMENT   = 'X_Y_Z_U component of wind at lowest physical level'
TZFIELD%NGRID      = 2
TZFIELD%NTYPE      = TYPEREAL
TZFIELD%NDIMS      = 2
TZFIELD%LTIMEDEP   = .TRUE.
CALL IO_Field_write(TPOUTPUT%TFILE,TZFIELD,XUT(:,:,IKB))
!
TZFIELD%CMNHNAME   = 'VTLOW'
TZFIELD%CSTDNAME   = 'y_wind'
TZFIELD%CLONGNAME  = ''
TZFIELD%CUNITS     = 'm s-1'
TZFIELD%CDIR       = 'XY'
TZFIELD%CCOMMENT   = 'X_Y_Z_V component of wind at lowest physical level'
TZFIELD%NGRID      = 3
TZFIELD%NTYPE      = TYPEREAL
TZFIELD%NDIMS      = 2
TZFIELD%LTIMEDEP   = .TRUE.
CALL IO_Field_write(TPOUTPUT%TFILE,TZFIELD,XVT(:,:,IKB))
!
TZFIELD%CMNHNAME   = 'THTLOW'
TZFIELD%CSTDNAME   = 'air_potential_temperature'
TZFIELD%CLONGNAME  = ''
TZFIELD%CUNITS     = 'K'
TZFIELD%CDIR       = 'XY'
TZFIELD%CCOMMENT   = 'X_Y_Z_potential temperature at lowest physical level'
TZFIELD%NGRID      = 1
TZFIELD%NTYPE      = TYPEREAL
TZFIELD%NDIMS      = 2
TZFIELD%LTIMEDEP   = .TRUE.
CALL IO_Field_write(TPOUTPUT%TFILE,TZFIELD,XTHT(:,:,IKB))
!
TZFIELD%CMNHNAME   = 'RVTLOW'
!TZFIELD%CSTDNAME   = 'humidity_mixing_ratio' !ratio of the mass of water vapor to the mass of dry air
TZFIELD%CSTDNAME   = 'specific_humidity'     !mass fraction of water vapor in (moist) air
TZFIELD%CLONGNAME  = ''
TZFIELD%CUNITS     = 'kg kg-1'
TZFIELD%CDIR       = 'XY'
TZFIELD%CCOMMENT   = 'X_Y_Z_Vapor mixing Ratio at lowest physical level'
TZFIELD%NGRID      = 1
TZFIELD%NTYPE      = TYPEREAL
TZFIELD%NDIMS      = 2
TZFIELD%LTIMEDEP   = .TRUE.
CALL IO_Field_write(TPOUTPUT%TFILE,TZFIELD,XRT(:,:,IKB,1))
!
TZFIELD%CMNHNAME   = 'ACPRRSTEP'
TZFIELD%CSTDNAME   = 'rainfall_amount'
TZFIELD%CLONGNAME  = ''
TZFIELD%CUNITS     = 'kg m-2'
TZFIELD%CDIR       = ''
TZFIELD%CCOMMENT   = 'X_Y_ACcumulated Precipitation Rain Rate during timestep'
TZFIELD%NGRID      = 1
TZFIELD%NTYPE      = TYPEREAL
TZFIELD%NDIMS      = 2
TZFIELD%LTIMEDEP   = .TRUE.
!XACPRR is multiplied by 1000. to convert from m to kg m-2 (water density is assumed to be 1000 kg m-3)
CALL IO_Field_write(TPOUTPUT%TFILE,TZFIELD,XINPRR*XTSTEP*1.0E3)
#endif
!
END SUBROUTINE IO_Field_user_write

END MODULE MODE_IO_FIELD_WRITE

