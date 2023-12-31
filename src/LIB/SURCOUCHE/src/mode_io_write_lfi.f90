!MNH_LIC Copyright 1994-2021 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
! Modifications:
!  P. Wautelet 05/2016-04/2018: new data structures and calls for I/O
!  P. Wautelet 21/06/2018: read and write correctly if MNH_REAL=4
!  P. Wautelet 14/12/2018: split fmreadwrit.f90
!  P. Wautelet 11/01/2019: do not write variables with a zero size
!  P. Wautelet 05/03/2019: rename IO subroutines and modules
!  P. Wautelet 12/07/2019: add support for 1D array of dates
!  P. Wautelet 14/01/2021: add IO_Field_write_lfi_N4 subroutine
!-----------------------------------------------------------------
module mode_io_write_lfi
!
use modd_field,      only: tfieldmetadata_base
USE MODD_IO
USE MODD_PARAMETERS, ONLY: NLFIMAXCOMMENTLENGTH
use modd_precision,  only: LFIINT, MNHINT64, MNHREAL64
!
USE MODE_MSG
!
IMPLICIT NONE
!
PRIVATE
!
public :: IO_Field_write_lfi
!
INTEGER, PARAMETER :: JPXKRK = NLFIMAXCOMMENTLENGTH
INTEGER, PARAMETER :: JPXFIE = 1.5E8
!
INTERFACE IO_Field_write_lfi
   MODULE PROCEDURE IO_Field_write_lfi_X0,IO_Field_write_lfi_X1, &
                    IO_Field_write_lfi_X2,IO_Field_write_lfi_X3, &
                    IO_Field_write_lfi_X4,IO_Field_write_lfi_X5, &
                    IO_Field_write_lfi_X6,                       &
                    IO_Field_write_lfi_N0,IO_Field_write_lfi_N1, &
                    IO_Field_write_lfi_N2,IO_Field_write_lfi_N3, &
                    IO_Field_write_lfi_N4,                       &
                    IO_Field_write_lfi_L0,IO_Field_write_lfi_L1, &
                    IO_Field_write_lfi_C0,                       &
                    IO_Field_write_lfi_T0,IO_Field_write_lfi_T1
END INTERFACE IO_Field_write_lfi
!
CONTAINS
!
!
SUBROUTINE IO_Field_write_lfi_X0(TPFILE,TPFIELD,PFIELD,KRESP)
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
TYPE(TFILEDATA),       INTENT(IN) :: TPFILE
CLASS(tfieldmetadata_base), INTENT(IN) :: TPFIELD
REAL,                  INTENT(IN) :: PFIELD ! array containing the data field
INTEGER,               INTENT(OUT):: KRESP  ! return-code if problems araised
!
!*      0.2   Declarations of local variables
!
INTEGER                                         :: ILENG
INTEGER(KIND=LFIINT)                            :: IRESP, ITOTAL
INTEGER(KIND=MNHINT64),DIMENSION(:),ALLOCATABLE :: IWORK
CHARACTER(LEN=LEN_HREC)                         :: YRECFM
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_write_lfi_X0','writing '//TRIM(TPFIELD%CMNHNAME))
!
ILENG = 1
!
CALL WRITE_PREPARE(TPFIELD,ILENG,IWORK,ITOTAL,IRESP)
!
IF (IRESP==0) THEN
  CALL TRANSFER_R_I8( (/PFIELD/) , IWORK(LEN(TPFIELD%CCOMMENT)+3:) )
  YRECFM=TRIM(TPFIELD%CMNHNAME)
  IF( LEN_TRIM(TPFIELD%CMNHNAME) > LEN(YRECFM) ) &
    CALL PRINT_MSG(NVERB_WARNING,'IO','IO_Field_write_lfi_X0','field name was truncated to '&
                   //YRECFM//' for '//TRIM(TPFIELD%CMNHNAME))
  CALL LFIECR(IRESP,TPFILE%NLFIFLU,TRIM(TPFIELD%CMNHNAME),IWORK,ITOTAL)
ENDIF
!
KRESP=IRESP
!
IF (ALLOCATED(IWORK)) DEALLOCATE(IWORK)
!
END SUBROUTINE IO_Field_write_lfi_X0
!
SUBROUTINE IO_Field_write_lfi_X1(TPFILE,TPFIELD,PFIELD,KRESP)
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
TYPE(TFILEDATA),       INTENT(IN) :: TPFILE
CLASS(tfieldmetadata_base), INTENT(IN) :: TPFIELD
REAL,DIMENSION(:),     INTENT(IN) :: PFIELD ! array containing the data field
INTEGER,               INTENT(OUT):: KRESP  ! return-code if problems araised
!
!*      0.2   Declarations of local variables
!
INTEGER                                         :: ILENG
INTEGER(kind=LFIINT)                            :: IRESP, ITOTAL
INTEGER(KIND=MNHINT64),DIMENSION(:),ALLOCATABLE :: IWORK
CHARACTER(LEN=LEN_HREC)                         :: YRECFM
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_write_lfi_X1','writing '//TRIM(TPFIELD%CMNHNAME))
!
ILENG = SIZE(PFIELD)
!
IF ( ILENG==0 ) THEN
  CALL PRINT_MSG(NVERB_WARNING,'IO','IO_Field_write_lfi_X1','ignoring variable with a zero size ('//TRIM(TPFIELD%CMNHNAME)//')')
  KRESP = 0
  RETURN
END IF
!
CALL WRITE_PREPARE(TPFIELD,ILENG,IWORK,ITOTAL,IRESP)
!
IF (IRESP==0) THEN
  CALL TRANSFER_R_I8(PFIELD,IWORK(LEN(TPFIELD%CCOMMENT)+3:))
  YRECFM=TRIM(TPFIELD%CMNHNAME)
  IF( LEN_TRIM(TPFIELD%CMNHNAME) > LEN(YRECFM) ) &
    CALL PRINT_MSG(NVERB_WARNING,'IO','IO_Field_write_lfi_X1','field name was truncated to '&
                   //YRECFM//' for '//TRIM(TPFIELD%CMNHNAME))
  CALL LFIECR(IRESP,TPFILE%NLFIFLU,YRECFM,IWORK,ITOTAL)
ENDIF
!
KRESP=IRESP
!
IF (ALLOCATED(IWORK)) DEALLOCATE(IWORK)
!
END SUBROUTINE IO_Field_write_lfi_X1
!
SUBROUTINE IO_Field_write_lfi_X2(TPFILE,TPFIELD,PFIELD,KRESP,KVERTLEVEL,KZFILE)
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
TYPE(TFILEDATA),TARGET,INTENT(IN) :: TPFILE
CLASS(tfieldmetadata_base), INTENT(IN) :: TPFIELD
REAL,DIMENSION(:,:),   INTENT(IN) :: PFIELD ! array containing the data field
INTEGER,               INTENT(OUT):: KRESP  ! return-code if problems araised
INTEGER,OPTIONAL,      INTENT(IN) :: KVERTLEVEL ! Number of the vertical level (needed for Z-level split files)
INTEGER,OPTIONAL,      INTENT(IN) :: KZFILE     ! Number of the Z-level split file
!
!*      0.2   Declarations of local variables
!
INTEGER                                         :: ILENG
INTEGER(kind=LFIINT)                            :: IRESP, ITOTAL
INTEGER(KIND=MNHINT64),DIMENSION(:),ALLOCATABLE :: IWORK
CHARACTER(LEN=4)                                :: YSUFFIX
CHARACTER(LEN=LEN(TPFIELD%CMNHNAME)+4)          :: YVARNAME
CHARACTER(LEN=LEN_HREC)                         :: YRECFM
TYPE(TFILEDATA),POINTER                         :: TZFILE
!
IRESP=0
!
ILENG = SIZE(PFIELD)
!
IF ( ILENG==0 ) THEN
  CALL PRINT_MSG(NVERB_WARNING,'IO','IO_Field_write_lfi_X2','ignoring variable with a zero size ('//TRIM(TPFIELD%CMNHNAME)//')')
  KRESP = 0
  RETURN
END IF
!
IF (PRESENT(KVERTLEVEL)) THEN
  IF (.NOT.PRESENT(KZFILE)) THEN
    CALL PRINT_MSG(NVERB_ERROR,'IO','IO_Field_write_lfi_X2','KZFILE argument not provided')
    RETURN
  END IF
  WRITE(YSUFFIX,'(I4.4)') KVERTLEVEL
  YVARNAME = TRIM(TPFIELD%CMNHNAME)//YSUFFIX
  IF (KZFILE>TPFILE%NSUBFILES_IOZ) CALL PRINT_MSG(NVERB_FATAL,'IO','IO_Field_write_lfi_X2','KZFILE value too high')
  TZFILE => TPFILE%TFILES_IOZ(KZFILE)%TFILE
ELSE
  YVARNAME = TRIM(TPFIELD%CMNHNAME)
  TZFILE => TPFILE
ENDIF
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_write_lfi_X2','writing '//TRIM(YVARNAME))
!
CALL WRITE_PREPARE(TPFIELD,ILENG,IWORK,ITOTAL,IRESP)
!
IF (IRESP==0) THEN
  CALL TRANSFER_R_I8(RESHAPE(PFIELD,(/ILENG/)),IWORK(LEN(TPFIELD%CCOMMENT)+3:))
  YRECFM=TRIM(YVARNAME)
  IF( LEN_TRIM(YVARNAME) > LEN(YRECFM) ) &
    CALL PRINT_MSG(NVERB_WARNING,'IO','IO_Field_write_lfi_X2','field name was truncated to '&
                   //YRECFM//' for '//TRIM(YVARNAME))
  CALL LFIECR(IRESP,TZFILE%NLFIFLU,YRECFM,IWORK,ITOTAL)
ENDIF
!
KRESP=IRESP
!
IF (ALLOCATED(IWORK)) DEALLOCATE(IWORK)
!
END SUBROUTINE IO_Field_write_lfi_X2
!
SUBROUTINE IO_Field_write_lfi_X3(TPFILE,TPFIELD,PFIELD,KRESP)
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
TYPE(TFILEDATA),         INTENT(IN) :: TPFILE
CLASS(tfieldmetadata_base), INTENT(IN) :: TPFIELD
REAL,DIMENSION(:,:,:),   INTENT(IN) :: PFIELD ! array containing the data field
INTEGER,                 INTENT(OUT):: KRESP  ! return-code if problems araised
!
!*      0.2   Declarations of local variables
!
INTEGER                                         :: ILENG
INTEGER(kind=LFIINT)                            :: IRESP, ITOTAL
INTEGER(KIND=MNHINT64),DIMENSION(:),ALLOCATABLE :: IWORK
CHARACTER(LEN=LEN_HREC)                         :: YRECFM
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_write_lfi_X3','writing '//TRIM(TPFIELD%CMNHNAME))
!
ILENG = SIZE(PFIELD)
!
IF ( ILENG==0 ) THEN
  CALL PRINT_MSG(NVERB_WARNING,'IO','IO_Field_write_lfi_X3','ignoring variable with a zero size ('//TRIM(TPFIELD%CMNHNAME)//')')
  KRESP = 0
  RETURN
END IF
!
CALL WRITE_PREPARE(TPFIELD,ILENG,IWORK,ITOTAL,IRESP)
!
IF (IRESP==0) THEN
  CALL TRANSFER_R_I8(RESHAPE(PFIELD,(/ILENG/)),IWORK(LEN(TPFIELD%CCOMMENT)+3:))
  YRECFM=TRIM(TPFIELD%CMNHNAME)
  IF( LEN_TRIM(TPFIELD%CMNHNAME) > LEN(YRECFM) ) &
    CALL PRINT_MSG(NVERB_WARNING,'IO','IO_Field_write_lfi_X3','field name was truncated to '&
                   //YRECFM//' for '//TRIM(TPFIELD%CMNHNAME))
  CALL LFIECR(IRESP,TPFILE%NLFIFLU,YRECFM,IWORK,ITOTAL)
ENDIF
!
KRESP=IRESP
!
IF (ALLOCATED(IWORK)) DEALLOCATE(IWORK)
!
END SUBROUTINE IO_Field_write_lfi_X3
!
SUBROUTINE IO_Field_write_lfi_X4(TPFILE,TPFIELD,PFIELD,KRESP)
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
TYPE(TFILEDATA),          INTENT(IN) :: TPFILE
CLASS(tfieldmetadata_base), INTENT(IN) :: TPFIELD
REAL,DIMENSION(:,:,:,:),  INTENT(IN) :: PFIELD ! array containing the data field
INTEGER,                  INTENT(OUT):: KRESP  ! return-code if problems araised
!
!*      0.2   Declarations of local variables
!
INTEGER                                         :: ILENG
INTEGER(kind=LFIINT)                            :: IRESP, ITOTAL
INTEGER(KIND=MNHINT64),DIMENSION(:),ALLOCATABLE :: IWORK
CHARACTER(LEN=LEN_HREC)                         :: YRECFM
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_write_lfi_X4','writing '//TRIM(TPFIELD%CMNHNAME))
!
ILENG = SIZE(PFIELD)
!
IF ( ILENG==0 ) THEN
  CALL PRINT_MSG(NVERB_WARNING,'IO','IO_Field_write_lfi_X4','ignoring variable with a zero size ('//TRIM(TPFIELD%CMNHNAME)//')')
  KRESP = 0
  RETURN
END IF
!
CALL WRITE_PREPARE(TPFIELD,ILENG,IWORK,ITOTAL,IRESP)
!
IF (IRESP==0) THEN
  CALL TRANSFER_R_I8(RESHAPE(PFIELD,(/ILENG/)),IWORK(LEN(TPFIELD%CCOMMENT)+3:))
  YRECFM=TRIM(TPFIELD%CMNHNAME)
  IF( LEN_TRIM(TPFIELD%CMNHNAME) > LEN(YRECFM) ) &
    CALL PRINT_MSG(NVERB_WARNING,'IO','IO_Field_write_lfi_X4','field name was truncated to '&
                   //YRECFM//' for '//TRIM(TPFIELD%CMNHNAME))
  CALL LFIECR(IRESP,TPFILE%NLFIFLU,YRECFM,IWORK,ITOTAL)
ENDIF
!
KRESP=IRESP
!
IF (ALLOCATED(IWORK)) DEALLOCATE(IWORK)
!
END SUBROUTINE IO_Field_write_lfi_X4
!
SUBROUTINE IO_Field_write_lfi_X5(TPFILE,TPFIELD,PFIELD,KRESP)
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
TYPE(TFILEDATA),          INTENT(IN) :: TPFILE
CLASS(tfieldmetadata_base), INTENT(IN) :: TPFIELD
REAL,DIMENSION(:,:,:,:,:),INTENT(IN) :: PFIELD ! array containing the data field
INTEGER,                  INTENT(OUT):: KRESP  ! return-code if problems araised
!
!*      0.2   Declarations of local variables
!
INTEGER                                         :: ILENG
INTEGER(kind=LFIINT)                            :: IRESP, ITOTAL
INTEGER(KIND=MNHINT64),DIMENSION(:),ALLOCATABLE :: IWORK
CHARACTER(LEN=LEN_HREC)                         :: YRECFM
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_write_lfi_X5','writing '//TRIM(TPFIELD%CMNHNAME))
!
ILENG = SIZE(PFIELD)
!
IF ( ILENG==0 ) THEN
  CALL PRINT_MSG(NVERB_WARNING,'IO','IO_Field_write_lfi_X5','ignoring variable with a zero size ('//TRIM(TPFIELD%CMNHNAME)//')')
  KRESP = 0
  RETURN
END IF
!
CALL WRITE_PREPARE(TPFIELD,ILENG,IWORK,ITOTAL,IRESP)
!
IF (IRESP==0) THEN
  CALL TRANSFER_R_I8(RESHAPE(PFIELD,(/ILENG/)),IWORK(LEN(TPFIELD%CCOMMENT)+3:))
  YRECFM=TRIM(TPFIELD%CMNHNAME)
  IF( LEN_TRIM(TPFIELD%CMNHNAME) > LEN(YRECFM) ) &
    CALL PRINT_MSG(NVERB_WARNING,'IO','IO_Field_write_lfi_X5','field name was truncated to '&
                   //YRECFM//' for '//TRIM(TPFIELD%CMNHNAME))
  CALL LFIECR(IRESP,TPFILE%NLFIFLU,YRECFM,IWORK,ITOTAL)
ENDIF
!
KRESP=IRESP
!
IF (ALLOCATED(IWORK)) DEALLOCATE(IWORK)
!
END SUBROUTINE IO_Field_write_lfi_X5
!
SUBROUTINE IO_Field_write_lfi_X6(TPFILE,TPFIELD,PFIELD,KRESP)
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
TYPE(TFILEDATA),            INTENT(IN) :: TPFILE
CLASS(tfieldmetadata_base), INTENT(IN) :: TPFIELD
REAL,DIMENSION(:,:,:,:,:,:),INTENT(IN) :: PFIELD ! array containing the data field
INTEGER,                    INTENT(OUT):: KRESP  ! return-code if problems araised
!
!*      0.2   Declarations of local variables
!
INTEGER                                         :: ILENG
INTEGER(kind=LFIINT)                            :: IRESP, ITOTAL
INTEGER(KIND=MNHINT64),DIMENSION(:),ALLOCATABLE :: IWORK
CHARACTER(LEN=LEN_HREC)                         :: YRECFM
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_write_lfi_X6','writing '//TRIM(TPFIELD%CMNHNAME))
!
ILENG = SIZE(PFIELD)
!
IF ( ILENG==0 ) THEN
  CALL PRINT_MSG(NVERB_WARNING,'IO','IO_Field_write_lfi_X6','ignoring variable with a zero size ('//TRIM(TPFIELD%CMNHNAME)//')')
  KRESP = 0
  RETURN
END IF
!
CALL WRITE_PREPARE(TPFIELD,ILENG,IWORK,ITOTAL,IRESP)
!
IF (IRESP==0) THEN
  CALL TRANSFER_R_I8(RESHAPE(PFIELD,(/ILENG/)),IWORK(LEN(TPFIELD%CCOMMENT)+3:))
  YRECFM=TRIM(TPFIELD%CMNHNAME)
  IF( LEN_TRIM(TPFIELD%CMNHNAME) > LEN(YRECFM) ) &
    CALL PRINT_MSG(NVERB_WARNING,'IO','IO_Field_write_lfi_X6','field name was truncated to '&
                   //YRECFM//' for '//TRIM(TPFIELD%CMNHNAME))
  CALL LFIECR(IRESP,TPFILE%NLFIFLU,YRECFM,IWORK,ITOTAL)
ENDIF
!
KRESP=IRESP
!
IF (ALLOCATED(IWORK)) DEALLOCATE(IWORK)
!
END SUBROUTINE IO_Field_write_lfi_X6
!
SUBROUTINE IO_Field_write_lfi_N0(TPFILE,TPFIELD,KFIELD,KRESP)
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
TYPE(TFILEDATA),         INTENT(IN) :: TPFILE
CLASS(tfieldmetadata_base), INTENT(IN) :: TPFIELD
INTEGER,                 INTENT(IN) :: KFIELD ! array containing the data field
INTEGER,                 INTENT(OUT):: KRESP  ! return-code if problems araised
!
!*      0.2   Declarations of local variables
!
INTEGER                                         :: ILENG
INTEGER(kind=LFIINT)                            :: IRESP, ITOTAL
INTEGER(KIND=MNHINT64),DIMENSION(:),ALLOCATABLE :: IWORK
CHARACTER(LEN=LEN_HREC)                         :: YRECFM
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_write_lfi_N0','writing '//TRIM(TPFIELD%CMNHNAME))
!
ILENG = 1
!
CALL WRITE_PREPARE(TPFIELD,ILENG,IWORK,ITOTAL,IRESP)
!
IF (IRESP==0) THEN
  IWORK(LEN(TPFIELD%CCOMMENT)+3)=KFIELD
  YRECFM=TRIM(TPFIELD%CMNHNAME)
  IF( LEN_TRIM(TPFIELD%CMNHNAME) > LEN(YRECFM) ) &
    CALL PRINT_MSG(NVERB_WARNING,'IO','IO_Field_write_lfi_N0','field name was truncated to '&
                   //YRECFM//' for '//TRIM(TPFIELD%CMNHNAME))
  CALL LFIECR(IRESP,TPFILE%NLFIFLU,YRECFM,IWORK,ITOTAL)
ENDIF
!
KRESP=IRESP
!
IF (ALLOCATED(IWORK)) DEALLOCATE(IWORK)
!
END SUBROUTINE IO_Field_write_lfi_N0
!
SUBROUTINE IO_Field_write_lfi_N1(TPFILE,TPFIELD,KFIELD,KRESP)
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
TYPE(TFILEDATA),         INTENT(IN) :: TPFILE
CLASS(tfieldmetadata_base), INTENT(IN) :: TPFIELD
INTEGER,DIMENSION(:),    INTENT(IN) :: KFIELD ! array containing the data field
INTEGER,                 INTENT(OUT):: KRESP  ! return-code if problems araised
!
!*      0.2   Declarations of local variables
!
INTEGER                                         :: ILENG
INTEGER(kind=LFIINT)                            :: IRESP, ITOTAL
INTEGER(KIND=MNHINT64),DIMENSION(:),ALLOCATABLE :: IWORK
CHARACTER(LEN=LEN_HREC)                         :: YRECFM
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_write_lfi_N1','writing '//TRIM(TPFIELD%CMNHNAME))
!
ILENG = SIZE(KFIELD)
!
IF ( ILENG==0 ) THEN
  CALL PRINT_MSG(NVERB_WARNING,'IO','IO_Field_write_lfi_N1','ignoring variable with a zero size ('//TRIM(TPFIELD%CMNHNAME)//')')
  KRESP = 0
  RETURN
END IF
!
CALL WRITE_PREPARE(TPFIELD,ILENG,IWORK,ITOTAL,IRESP)
!
IF (IRESP==0) THEN
  IWORK(LEN(TPFIELD%CCOMMENT)+3:) = KFIELD(:)
  YRECFM=TRIM(TPFIELD%CMNHNAME)
  IF( LEN_TRIM(TPFIELD%CMNHNAME) > LEN(YRECFM) ) &
    CALL PRINT_MSG(NVERB_WARNING,'IO','IO_Field_write_lfi_N1','field name was truncated to '&
                   //YRECFM//' for '//TRIM(TPFIELD%CMNHNAME))
  CALL LFIECR(IRESP,TPFILE%NLFIFLU,YRECFM,IWORK,ITOTAL)
ENDIF
!
KRESP=IRESP
!
IF (ALLOCATED(IWORK)) DEALLOCATE(IWORK)
!
END SUBROUTINE IO_Field_write_lfi_N1
!
SUBROUTINE IO_Field_write_lfi_N2(TPFILE,TPFIELD,KFIELD,KRESP)
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
TYPE(TFILEDATA),       INTENT(IN) :: TPFILE
CLASS(tfieldmetadata_base), INTENT(IN) :: TPFIELD
INTEGER,DIMENSION(:,:),INTENT(IN) :: KFIELD ! array containing the data field
INTEGER,               INTENT(OUT):: KRESP  ! return-code if problems araised
!
!*      0.2   Declarations of local variables
!
INTEGER                                         :: ILENG
INTEGER(kind=LFIINT)                            :: IRESP, ITOTAL
INTEGER(KIND=MNHINT64),DIMENSION(:),ALLOCATABLE :: IWORK
CHARACTER(LEN=LEN_HREC)                         :: YRECFM
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_write_lfi_N2','writing '//TRIM(TPFIELD%CMNHNAME))
!
ILENG = SIZE(KFIELD)
!
IF ( ILENG==0 ) THEN
  CALL PRINT_MSG(NVERB_WARNING,'IO','IO_Field_write_lfi_N2','ignoring variable with a zero size ('//TRIM(TPFIELD%CMNHNAME)//')')
  KRESP = 0
  RETURN
END IF
!
CALL WRITE_PREPARE(TPFIELD,ILENG,IWORK,ITOTAL,IRESP)
!
IF (IRESP==0) THEN
  IWORK(LEN(TPFIELD%CCOMMENT)+3:) = RESHAPE( KFIELD(:,:) , (/ SIZE(KFIELD) /) )
  YRECFM=TRIM(TPFIELD%CMNHNAME)
  IF( LEN_TRIM(TPFIELD%CMNHNAME) > LEN(YRECFM) ) &
    CALL PRINT_MSG(NVERB_WARNING,'IO','IO_Field_write_lfi_N2','field name was truncated to '&
                   //YRECFM//' for '//TRIM(TPFIELD%CMNHNAME))
  CALL LFIECR(IRESP,TPFILE%NLFIFLU,YRECFM,IWORK,ITOTAL)
ENDIF
!
KRESP=IRESP
!
IF (ALLOCATED(IWORK)) DEALLOCATE(IWORK)
!
END SUBROUTINE IO_Field_write_lfi_N2
!
SUBROUTINE IO_Field_write_lfi_N3(TPFILE,TPFIELD,KFIELD,KRESP)
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
TYPE(TFILEDATA),         INTENT(IN) :: TPFILE
CLASS(tfieldmetadata_base), INTENT(IN) :: TPFIELD
INTEGER,DIMENSION(:,:,:),INTENT(IN) :: KFIELD ! array containing the data field
INTEGER,                 INTENT(OUT):: KRESP  ! return-code if problems araised
!
!*      0.2   Declarations of local variables
!
INTEGER                                         :: ILENG
INTEGER(kind=LFIINT)                            :: IRESP, ITOTAL
INTEGER(KIND=MNHINT64),DIMENSION(:),ALLOCATABLE :: IWORK
CHARACTER(LEN=LEN_HREC)                         :: YRECFM
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_write_lfi_N3','writing '//TRIM(TPFIELD%CMNHNAME))
!
ILENG = SIZE(KFIELD)
!
IF ( ILENG==0 ) THEN
  CALL PRINT_MSG(NVERB_WARNING,'IO','IO_Field_write_lfi_N3','ignoring variable with a zero size ('//TRIM(TPFIELD%CMNHNAME)//')')
  KRESP = 0
  RETURN
END IF
!
CALL WRITE_PREPARE(TPFIELD,ILENG,IWORK,ITOTAL,IRESP)
!
IF (IRESP==0) THEN
  IWORK(LEN(TPFIELD%CCOMMENT)+3:) = RESHAPE( KFIELD(:,:,:) , (/ SIZE(KFIELD) /) )
  YRECFM=TRIM(TPFIELD%CMNHNAME)
  IF( LEN_TRIM(TPFIELD%CMNHNAME) > LEN(YRECFM) ) &
    CALL PRINT_MSG(NVERB_WARNING,'IO','IO_Field_write_lfi_N3','field name was truncated to '&
                   //YRECFM//' for '//TRIM(TPFIELD%CMNHNAME))
  CALL LFIECR(IRESP,TPFILE%NLFIFLU,YRECFM,IWORK,ITOTAL)
ENDIF
!
KRESP=IRESP
!
IF (ALLOCATED(IWORK)) DEALLOCATE(IWORK)
!
END SUBROUTINE IO_Field_write_lfi_N3


SUBROUTINE IO_Field_write_lfi_N4(TPFILE,TPFIELD,KFIELD,KRESP)
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
TYPE(TFILEDATA),           INTENT(IN) :: TPFILE
CLASS(tfieldmetadata_base), INTENT(IN) :: TPFIELD
INTEGER,DIMENSION(:,:,:,:),INTENT(IN) :: KFIELD ! array containing the data field
INTEGER,                   INTENT(OUT):: KRESP  ! return-code if problems araised
!
!*      0.2   Declarations of local variables
!
INTEGER                                         :: ILENG
INTEGER(kind=LFIINT)                            :: IRESP, ITOTAL
INTEGER(KIND=MNHINT64),DIMENSION(:),ALLOCATABLE :: IWORK
CHARACTER(LEN=LEN_HREC)                         :: YRECFM
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_write_lfi_N4','writing '//TRIM(TPFIELD%CMNHNAME))
!
ILENG = SIZE(KFIELD)
!
IF ( ILENG==0 ) THEN
  CALL PRINT_MSG(NVERB_WARNING,'IO','IO_Field_write_lfi_N4','ignoring variable with a zero size ('//TRIM(TPFIELD%CMNHNAME)//')')
  KRESP = 0
  RETURN
END IF
!
CALL WRITE_PREPARE(TPFIELD,ILENG,IWORK,ITOTAL,IRESP)
!
IF (IRESP==0) THEN
  IWORK(LEN(TPFIELD%CCOMMENT)+3:) = RESHAPE( KFIELD(:,:,:,:) , (/ SIZE(KFIELD) /) )
  YRECFM=TRIM(TPFIELD%CMNHNAME)
  IF( LEN_TRIM(TPFIELD%CMNHNAME) > LEN(YRECFM) ) &
    CALL PRINT_MSG(NVERB_WARNING,'IO','IO_Field_write_lfi_N4','field name was truncated to '&
                   //YRECFM//' for '//TRIM(TPFIELD%CMNHNAME))
  CALL LFIECR(IRESP,TPFILE%NLFIFLU,YRECFM,IWORK,ITOTAL)
ENDIF
!
KRESP=IRESP
!
IF (ALLOCATED(IWORK)) DEALLOCATE(IWORK)
!
END SUBROUTINE IO_Field_write_lfi_N4


SUBROUTINE IO_Field_write_lfi_L0(TPFILE,TPFIELD,OFIELD,KRESP)
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
TYPE(TFILEDATA),         INTENT(IN) :: TPFILE
CLASS(tfieldmetadata_base), INTENT(IN) :: TPFIELD
LOGICAL,                 INTENT(IN) :: OFIELD ! array containing the data field
INTEGER,                 INTENT(OUT):: KRESP  ! return-code if problems araised
!
!*      0.2   Declarations of local variables
!
INTEGER                                         :: IFIELD
INTEGER                                         :: ILENG
INTEGER(kind=LFIINT)                            :: IRESP, ITOTAL
INTEGER(KIND=MNHINT64),DIMENSION(:),ALLOCATABLE :: IWORK
CHARACTER(LEN=LEN_HREC)                         :: YRECFM
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_write_lfi_L0','writing '//TRIM(TPFIELD%CMNHNAME))
!
ILENG = 1
!
!Convert LOGICAL to INTEGER (LOGICAL format not supported by LFI files)
IF (OFIELD) THEN
  IFIELD = 1
ELSE
  IFIELD = 0
END IF
!
CALL WRITE_PREPARE(TPFIELD,ILENG,IWORK,ITOTAL,IRESP)
!
IF (IRESP==0) THEN
  IWORK(LEN(TPFIELD%CCOMMENT)+3)=IFIELD
  YRECFM=TRIM(TPFIELD%CMNHNAME)
  IF( LEN_TRIM(TPFIELD%CMNHNAME) > LEN(YRECFM) ) &
    CALL PRINT_MSG(NVERB_WARNING,'IO','IO_Field_write_lfi_L0','field name was truncated to '&
                   //YRECFM//' for '//TRIM(TPFIELD%CMNHNAME))
  CALL LFIECR(IRESP,TPFILE%NLFIFLU,YRECFM,IWORK,ITOTAL)
ENDIF
!
KRESP=IRESP
!
IF (ALLOCATED(IWORK)) DEALLOCATE(IWORK)
!
END SUBROUTINE IO_Field_write_lfi_L0
!
SUBROUTINE IO_Field_write_lfi_L1(TPFILE,TPFIELD,OFIELD,KRESP)
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
TYPE(TFILEDATA),         INTENT(IN) :: TPFILE
CLASS(tfieldmetadata_base), INTENT(IN) :: TPFIELD
LOGICAL,DIMENSION(:),    INTENT(IN) :: OFIELD ! array containing the data field
INTEGER,                 INTENT(OUT):: KRESP  ! return-code if problems araised
!
!*      0.2   Declarations of local variables
!
INTEGER, DIMENSION(SIZE(OFIELD))                :: IFIELD
INTEGER                                         :: ILENG
INTEGER(kind=LFIINT)                            :: IRESP, ITOTAL
INTEGER(KIND=MNHINT64),DIMENSION(:),ALLOCATABLE :: IWORK
CHARACTER(LEN=LEN_HREC)                         :: YRECFM
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_write_lfi_L1','writing '//TRIM(TPFIELD%CMNHNAME))
!
ILENG = SIZE(OFIELD)
!
IF ( ILENG==0 ) THEN
  CALL PRINT_MSG(NVERB_WARNING,'IO','IO_Field_write_lfi_L1','ignoring variable with a zero size ('//TRIM(TPFIELD%CMNHNAME)//')')
  KRESP = 0
  RETURN
END IF
!
!Convert LOGICAL to INTEGER (LOGICAL format not supported by LFI files)
WHERE (OFIELD)
  IFIELD = 1
ELSEWHERE
  IFIELD = 0
END WHERE
!
CALL WRITE_PREPARE(TPFIELD,ILENG,IWORK,ITOTAL,IRESP)
!
IF (IRESP==0) THEN
  IWORK(LEN(TPFIELD%CCOMMENT)+3:) = IFIELD(:)
  YRECFM=TRIM(TPFIELD%CMNHNAME)
  IF( LEN_TRIM(TPFIELD%CMNHNAME) > LEN(YRECFM) ) &
    CALL PRINT_MSG(NVERB_WARNING,'IO','IO_Field_write_lfi_L1','field name was truncated to '&
                   //YRECFM//' for '//TRIM(TPFIELD%CMNHNAME))
  CALL LFIECR(IRESP,TPFILE%NLFIFLU,YRECFM,IWORK,ITOTAL)
ENDIF
!
KRESP=IRESP
!
IF (ALLOCATED(IWORK)) DEALLOCATE(IWORK)
!
END SUBROUTINE IO_Field_write_lfi_L1
!
SUBROUTINE IO_Field_write_lfi_C0(TPFILE,TPFIELD,HFIELD,KRESP)
!
USE MODD_PARAMETERS, ONLY: NFILENAMELGTMAXLFI
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
TYPE(TFILEDATA),         INTENT(IN) :: TPFILE
CLASS(tfieldmetadata_base), INTENT(IN) :: TPFIELD
CHARACTER(LEN=*),        INTENT(IN) :: HFIELD ! array containing the data field
INTEGER,                 INTENT(OUT):: KRESP  ! return-code if problems araised
!
!*      0.2   Declarations of local variables
!
INTEGER                                         :: ILENG, ILENGMAX, JLOOP
INTEGER(kind=LFIINT)                            :: IRESP, ITOTAL
INTEGER(KIND=MNHINT64),DIMENSION(:),ALLOCATABLE :: IWORK
CHARACTER(LEN=LEN_HREC)                         :: YRECFM
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_write_lfi_C0','writing '//TRIM(TPFIELD%CMNHNAME))
!
ILENG=LEN(HFIELD)
ILENGMAX = ILENG
IF (ILENG==0) ILENGMAX=1
!
!Special treatment for MY_NAME and DAD_NAME fields (for backward compatibility)
IF (TPFIELD%CMNHNAME=='MY_NAME' .OR. TPFIELD%CMNHNAME=='DAD_NAME') THEN
  ILENG = MIN(LEN(HFIELD),NFILENAMELGTMAXLFI)
  ILENGMAX = NFILENAMELGTMAXLFI
  IF (LEN_TRIM(HFIELD)>ILENGMAX) &
        CALL PRINT_MSG(NVERB_WARNING,'IO','IO_Field_write_lfi_C0',TRIM(TPFILE%CNAME)// &
                      ': MY_NAME was truncated from '//TRIM(HFIELD)//' to '//HFIELD(1:NFILENAMELGTMAXLFI))
END IF
!
CALL WRITE_PREPARE(TPFIELD,ILENGMAX,IWORK,ITOTAL,IRESP)
!
IF (IRESP==0) THEN
  DO JLOOP=1,ILENG
    IWORK(LEN(TPFIELD%CCOMMENT)+2+JLOOP)=IACHAR(HFIELD(JLOOP:JLOOP))
  END DO
  !Pad with blank characters
  DO JLOOP=ILENG+1,ILENGMAX
      IWORK(LEN(TPFIELD%CCOMMENT)+2+JLOOP)=IACHAR(' ')
  END DO
  YRECFM=TRIM(TPFIELD%CMNHNAME)
  IF( LEN_TRIM(TPFIELD%CMNHNAME) > LEN(YRECFM) ) &
    CALL PRINT_MSG(NVERB_WARNING,'IO','IO_Field_write_lfi_C0','field name was truncated to '&
                   //YRECFM//' for '//TRIM(TPFIELD%CMNHNAME))
  CALL LFIECR(IRESP,TPFILE%NLFIFLU,YRECFM,IWORK,ITOTAL)
ENDIF
!
KRESP=IRESP
!
IF (ALLOCATED(IWORK)) DEALLOCATE(IWORK)
!
END SUBROUTINE IO_Field_write_lfi_C0
!
SUBROUTINE IO_Field_write_lfi_T0(TPFILE,TPFIELD,TPDATA,KRESP)
!
USE MODD_TYPE_DATE
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
TYPE(TFILEDATA),         INTENT(IN) :: TPFILE
CLASS(tfieldmetadata_base), INTENT(IN) :: TPFIELD
TYPE (DATE_TIME),        INTENT(IN) :: TPDATA ! array containing the data field
INTEGER,                 INTENT(OUT):: KRESP  ! return-code if problems araised
!
!*      0.2   Declarations of local variables
!
INTEGER                                         :: ILENG
INTEGER(kind=LFIINT)                            :: IRESP, ITOTAL
CLASS(tfieldmetadata_base), ALLOCATABLE         :: TZFIELD
INTEGER, DIMENSION(3)                           :: ITDATE    ! date array
INTEGER(KIND=MNHINT64),DIMENSION(:),ALLOCATABLE :: IWORK
CHARACTER(LEN=LEN_HREC)                         :: YRECFM
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_write_lfi_T0','writing '//TRIM(TPFIELD%CMNHNAME))
!
Allocate( TZFIELD, source = TPFIELD )
!
! Write date
!
TZFIELD%CMNHNAME = TRIM(TPFIELD%CMNHNAME)//'%TDATE'
TZFIELD%CCOMMENT = 'YYYYMMDD'
ITDATE(1)=TPDATA%nyear
ITDATE(2)=TPDATA%nmonth
ITDATE(3)=TPDATA%nday
ILENG=SIZE(ITDATE)
!
CALL WRITE_PREPARE(TZFIELD,ILENG,IWORK,ITOTAL,IRESP)
!
IF (IRESP==0) THEN
  IWORK(LEN(TZFIELD%CCOMMENT)+3:)=ITDATE(:)
  YRECFM=TRIM(TZFIELD%CMNHNAME)
  IF( LEN_TRIM(TZFIELD%CMNHNAME) > LEN(YRECFM) ) &
    CALL PRINT_MSG(NVERB_WARNING,'IO','IO_Field_write_lfi_T0','field name was truncated to '&
                   //YRECFM//' for '//TRIM(TZFIELD%CMNHNAME))
  CALL LFIECR(IRESP,TPFILE%NLFIFLU,YRECFM,IWORK,ITOTAL)
ENDIF
!
IF (ALLOCATED(IWORK)) DEALLOCATE(IWORK)
!
IF (IRESP/=0) THEN
  KRESP = IRESP
  RETURN
END IF
!
! Write time
!
TZFIELD%CMNHNAME = TRIM(TPFIELD%CMNHNAME)//'%TIME'
TZFIELD%CCOMMENT = 'SECONDS'
ILENG=1
!
CALL WRITE_PREPARE(TZFIELD,ILENG,IWORK,ITOTAL,IRESP)
!
IF (IRESP==0) THEN
  IWORK(LEN(TZFIELD%CCOMMENT)+3) = TRANSFER(TPDATA%xtime,IWORK(1))
  YRECFM=TRIM(TZFIELD%CMNHNAME)
  IF( LEN_TRIM(TZFIELD%CMNHNAME) > LEN(YRECFM) ) &
    CALL PRINT_MSG(NVERB_WARNING,'IO','IO_Field_write_lfi_T0','field name was truncated to '&
                   //YRECFM//' for '//TRIM(TZFIELD%CMNHNAME))
  CALL LFIECR(IRESP,TPFILE%NLFIFLU,YRECFM,IWORK,ITOTAL)
ENDIF
!
KRESP=IRESP
!
IF (ALLOCATED(IWORK)) DEALLOCATE(IWORK)
!
END SUBROUTINE IO_Field_write_lfi_T0
!
SUBROUTINE IO_Field_write_lfi_T1(TPFILE,TPFIELD,TPDATA,KRESP)
!
USE MODD_TYPE_DATE
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
TYPE(TFILEDATA),                INTENT(IN) :: TPFILE
CLASS(tfieldmetadata_base), INTENT(IN) :: TPFIELD
TYPE (DATE_TIME), DIMENSION(:), INTENT(IN) :: TPDATA ! array containing the data field
INTEGER,                        INTENT(OUT):: KRESP  ! return-code if problems araised
!
!*      0.2   Declarations of local variables
!
INTEGER                                         :: ILENG, IPOS
INTEGER                                         :: JI
INTEGER(kind=LFIINT)                            :: IRESP, ITOTAL
CLASS(tfieldmetadata_base), ALLOCATABLE         :: TZFIELD
INTEGER, DIMENSION(:), ALLOCATABLE              :: ITDATE    ! date array
INTEGER(KIND=MNHINT64),DIMENSION(:),ALLOCATABLE :: IWORK
CHARACTER(LEN=LEN_HREC)                         :: YRECFM
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_write_lfi_T1','writing '//TRIM(TPFIELD%CMNHNAME))
!
ILENG = 3 * SIZE( TPDATA )
!
IF ( ILENG==0 ) THEN
  CALL PRINT_MSG(NVERB_WARNING,'IO','IO_Field_write_lfi_T1','ignoring variable with a zero size ('//TRIM(TPFIELD%CMNHNAME)//')')
  KRESP = 0
  RETURN
END IF
!
ALLOCATE( ITDATE( ILENG ) )
!
Allocate( TZFIELD, source = TPFIELD )
!
! Write date
!
TZFIELD%CMNHNAME = TRIM(TPFIELD%CMNHNAME)//'%TDATE'
TZFIELD%CCOMMENT = 'YYYYMMDD'
!
DO JI = 1, SIZE( TPDATA )
  IPOS = 1 + 3 * ( JI - 1 )
  ITDATE(IPOS )     = TPDATA(JI)%nyear
  ITDATE(IPOS + 1 ) = TPDATA(JI)%nmonth
  ITDATE(IPOS + 2 ) = TPDATA(JI)%nday
END DO
!
CALL WRITE_PREPARE(TZFIELD,ILENG,IWORK,ITOTAL,IRESP)
!
IF (IRESP==0) THEN
  IWORK(LEN(TZFIELD%CCOMMENT)+3:)=ITDATE(:)
  YRECFM=TRIM(TZFIELD%CMNHNAME)
  IF( LEN_TRIM(TZFIELD%CMNHNAME) > LEN(YRECFM) ) &
    CALL PRINT_MSG(NVERB_WARNING,'IO','IO_Field_write_lfi_T1','field name was truncated to '&
                   //YRECFM//' for '//TRIM(TZFIELD%CMNHNAME))
  CALL LFIECR(IRESP,TPFILE%NLFIFLU,YRECFM,IWORK,ITOTAL)
ENDIF
!
IF (ALLOCATED(IWORK)) DEALLOCATE(IWORK)
!
IF (IRESP/=0) THEN
  KRESP = IRESP
  RETURN
END IF
!
! Write time
!
TZFIELD%CMNHNAME = TRIM(TPFIELD%CMNHNAME)//'%TIME'
TZFIELD%CCOMMENT = 'SECONDS'
ILENG = SIZE( TPDATA )
!
CALL WRITE_PREPARE(TZFIELD,ILENG,IWORK,ITOTAL,IRESP)
!
IF (IRESP==0) THEN
  CALL TRANSFER_R_I8(TPDATA(:)%xtime,IWORK(LEN(TPFIELD%CCOMMENT)+3:))
  YRECFM=TRIM(TZFIELD%CMNHNAME)
  IF( LEN_TRIM(TZFIELD%CMNHNAME) > LEN(YRECFM) ) &
    CALL PRINT_MSG(NVERB_WARNING,'IO','IO_Field_write_lfi_T1','field name was truncated to '&
                   //YRECFM//' for '//TRIM(TZFIELD%CMNHNAME))
  CALL LFIECR(IRESP,TPFILE%NLFIFLU,YRECFM,IWORK,ITOTAL)
ENDIF
!
KRESP=IRESP
!
IF (ALLOCATED(IWORK)) DEALLOCATE(IWORK)
DEALLOCATE( ITDATE )
!
END SUBROUTINE IO_Field_write_lfi_T1
!
SUBROUTINE WRITE_PREPARE(TPFIELD,KLENG,KWORK,KTOTAL,KRESP)
!
CLASS(tfieldmetadata_base),                     INTENT(IN)    :: TPFIELD
INTEGER,                                        INTENT(IN)    :: KLENG
INTEGER(KIND=MNHINT64),DIMENSION(:),ALLOCATABLE,INTENT(INOUT) :: KWORK
INTEGER(kind=LFIINT),                           INTENT(OUT)   :: KTOTAL
INTEGER(kind=LFIINT),                           INTENT(OUT)   :: KRESP
!
INTEGER                   :: ICOMLEN
INTEGER                   :: J
INTEGER,DIMENSION(JPXKRK) :: ICOMMENT
!
ICOMLEN = LEN(TPFIELD%CCOMMENT)
KRESP = 0
!
IF (KLENG.LE.0) THEN
  KRESP=-40
  RETURN
ELSEIF (KLENG.GT.JPXFIE) THEN
  KRESP=-43
  RETURN
ELSEIF ((TPFIELD%NGRID.LT.0).OR.(TPFIELD%NGRID.GT.8)) THEN
  KRESP=-46
  RETURN
ENDIF
!
KTOTAL=KLENG+1+ICOMLEN+1
ALLOCATE(KWORK(KTOTAL))
!
KWORK(1)=TPFIELD%NGRID
!
SELECT CASE (ICOMLEN)
CASE(0)
  KWORK(2)=ICOMLEN
CASE(1:JPXKRK)
  DO J=1,ICOMLEN
    ICOMMENT(J)=ICHAR(TPFIELD%CCOMMENT(J:J))
  ENDDO
  KWORK(2)=ICOMLEN
  KWORK(3:ICOMLEN+2)=ICOMMENT(1:ICOMLEN)
CASE(JPXKRK+1:)
  CALL PRINT_MSG(NVERB_WARNING,'IO','WRITE_PREPARE','comment is too long')
  KRESP = -57
END SELECT
!
END SUBROUTINE WRITE_PREPARE
!
SUBROUTINE TRANSFER_R_I8(PFIELDIN,KFIELDOUT)
!
REAL,DIMENSION(:),                  INTENT(IN)  :: PFIELDIN
INTEGER(KIND=MNHINT64),DIMENSION(:),INTENT(OUT) :: KFIELDOUT
!
INTEGER :: ILENG
#if (MNH_REAL == 4)
REAL(KIND=MNHREAL64),DIMENSION(:),ALLOCATABLE :: ZFIELD8
#endif
!
ILENG = SIZE(PFIELDIN)
!
#if (MNH_REAL == 8)
  KFIELDOUT(:) = TRANSFER(PFIELDIN,KFIELDOUT(1),ILENG)
#else
  ALLOCATE(ZFIELD8(ILENG))
  ZFIELD8(:) = REAL(PFIELDIN(:),KIND=MNHREAL64)
  KFIELDOUT(:) = TRANSFER(ZFIELD8,KFIELDOUT(1),ILENG)
  DEALLOCATE(ZFIELD8)
#endif
!
END SUBROUTINE TRANSFER_R_I8


end module mode_io_write_lfi
