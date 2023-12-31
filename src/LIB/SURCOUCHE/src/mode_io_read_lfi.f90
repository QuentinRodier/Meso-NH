!MNH_LIC Copyright 1994-2023 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
! Modifications:
!  P. Wautelet 05/2016-04/2018: new data structures and calls for I/O
!  P. Wautelet 21/06/2018: read and write correctly if MNH_REAL=4
!  P. Wautelet 14/12/2018: split fmreadwrit.f90
!  P. Wautelet 21/02/2019: bugfix: intent of read fields: OUT->INOUT to keep initial value if not found in file
!  P. Wautelet 05/03/2019: rename IO subroutines and modules
!  P. Wautelet 25/06/2019: added IO_Field_read for 3D integer arrays (IO_Field_read_lfi_N3)
!-----------------------------------------------------------------
module mode_io_read_lfi
!
use modd_field,      only: tfieldmetadata_base
USE MODD_IO
USE MODD_PARAMETERS, ONLY: NLFIMAXCOMMENTLENGTH
use modd_precision,  only: LFIINT, MNHINT64, MNHREAL32, MNHREAL64
!
USE MODE_MSG
!
IMPLICIT NONE
!
PRIVATE
!
public :: IO_Field_read_lfi
!
INTEGER, PARAMETER :: JPXKRK = NLFIMAXCOMMENTLENGTH
INTEGER, PARAMETER :: JPXFIE = 1.5E8
!
INTERFACE IO_Field_read_lfi
   MODULE PROCEDURE IO_Field_read_lfi_X0, IO_Field_read_lfi_X1, &
                    IO_Field_read_lfi_X2, IO_Field_read_lfi_X3, &
                    IO_Field_read_lfi_X4, IO_Field_read_lfi_X5, &
                    IO_Field_read_lfi_X6,                       &
                    IO_Field_read_lfi_N0, IO_Field_read_lfi_N1, &
                    IO_Field_read_lfi_N2, IO_Field_read_lfi_N3, &
                    IO_Field_read_lfi_L0, IO_Field_read_lfi_L1, &
                    IO_Field_read_lfi_C0,                       &
                    IO_Field_read_lfi_T0
END INTERFACE IO_Field_read_lfi
!
CONTAINS
!
SUBROUTINE IO_Field_read_lfi_X0(TPFILE,TPFIELD,PFIELD,KRESP)
USE MODE_MSG
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
TYPE(TFILEDATA),            INTENT(IN)    :: TPFILE
CLASS(tfieldmetadata_base), INTENT(INOUT) :: TPFIELD
REAL,                       INTENT(INOUT) :: PFIELD  ! array containing the data field
INTEGER,                    INTENT(OUT)   :: KRESP   ! return-code if problems occured
!
!*      0.2   Declarations of local variables
!
INTEGER(KIND=LFIINT)                            :: IRESP,ITOTAL
INTEGER                                         :: ILENG
INTEGER(KIND=MNHINT64),DIMENSION(:),ALLOCATABLE :: IWORK
LOGICAL                                         :: GGOOD
REAL,DIMENSION(1)                               :: ZFIELD
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_read_lfi_X0',TRIM(TPFILE%CNAME)//': reading '//TRIM(TPFIELD%CMNHNAME))
!
ILENG = 1
!
CALL IO_Field_read_check_lfi(TPFILE,TPFIELD,ILENG,IWORK,ITOTAL,IRESP,GGOOD)
!
IF (GGOOD) THEN
  !TRANSFER_I8_R works with 1D arrays
  ZFIELD = TRANSFER_I8_R( (/ IWORK(IWORK(2)+3) /) )
  PFIELD = ZFIELD(1)
END IF
!
KRESP=IRESP
!
IF (ALLOCATED(IWORK)) DEALLOCATE(IWORK)
!
END SUBROUTINE IO_Field_read_lfi_X0
!
!
SUBROUTINE IO_Field_read_lfi_X1(TPFILE,TPFIELD,PFIELD,KRESP)
USE MODE_MSG
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
TYPE(TFILEDATA),            INTENT(IN)    :: TPFILE
CLASS(tfieldmetadata_base), INTENT(INOUT) :: TPFIELD
REAL, DIMENSION(:),         INTENT(INOUT) :: PFIELD  ! array containing the data field
INTEGER,                    INTENT(OUT)   :: KRESP   ! return-code if problems occured
!
!*      0.2   Declarations of local variables
!
INTEGER(KIND=LFIINT)                            :: IRESP,ITOTAL
INTEGER                                         :: ILENG
INTEGER(KIND=MNHINT64),DIMENSION(:),ALLOCATABLE :: IWORK
LOGICAL                                         :: GGOOD
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_read_lfi_X1',TRIM(TPFILE%CNAME)//': reading '//TRIM(TPFIELD%CMNHNAME))
!
ILENG = SIZE(PFIELD)
!
CALL IO_Field_read_check_lfi(TPFILE,TPFIELD,ILENG,IWORK,ITOTAL,IRESP,GGOOD)
!
IF (GGOOD) PFIELD = TRANSFER_I8_R(IWORK(IWORK(2)+3:))
!
KRESP=IRESP
!
IF (ALLOCATED(IWORK)) DEALLOCATE(IWORK)
!
END SUBROUTINE IO_Field_read_lfi_X1
!
!
SUBROUTINE IO_Field_read_lfi_X2(TPFILE,TPFIELD,PFIELD,KRESP)
USE MODE_MSG
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
TYPE(TFILEDATA),            INTENT(IN)    :: TPFILE
CLASS(tfieldmetadata_base), INTENT(INOUT) :: TPFIELD
REAL, DIMENSION(:,:),       INTENT(INOUT) :: PFIELD  ! array containing the data field
INTEGER,                    INTENT(OUT)   :: KRESP   ! return-code if problems occured
!
!*      0.2   Declarations of local variables
!
INTEGER(KIND=LFIINT)                            :: IRESP,ITOTAL
INTEGER                                         :: ILENG
INTEGER(KIND=MNHINT64),DIMENSION(:),ALLOCATABLE :: IWORK
LOGICAL                                         :: GGOOD
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_read_lfi_X2',TRIM(TPFILE%CNAME)//': reading '//TRIM(TPFIELD%CMNHNAME))
!
ILENG = SIZE(PFIELD)
!
CALL IO_Field_read_check_lfi(TPFILE,TPFIELD,ILENG,IWORK,ITOTAL,IRESP,GGOOD)
!
IF (GGOOD) PFIELD = RESHAPE( TRANSFER_I8_R(IWORK(IWORK(2)+3:)) , SHAPE(PFIELD) )
!
KRESP=IRESP
!
IF (ALLOCATED(IWORK)) DEALLOCATE(IWORK)
!
END SUBROUTINE IO_Field_read_lfi_X2
!
!
SUBROUTINE IO_Field_read_lfi_X3(TPFILE,TPFIELD,PFIELD,KRESP)
USE MODE_MSG
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
TYPE(TFILEDATA),            INTENT(IN)    :: TPFILE
CLASS(tfieldmetadata_base), INTENT(INOUT) :: TPFIELD
REAL, DIMENSION(:,:,:),     INTENT(INOUT) :: PFIELD  ! array containing the data field
INTEGER,                    INTENT(OUT)   :: KRESP   ! return-code if problems occured
!
!*      0.2   Declarations of local variables
!
INTEGER(KIND=LFIINT)                            :: IRESP,ITOTAL
INTEGER                                         :: ILENG
INTEGER(KIND=MNHINT64),DIMENSION(:),ALLOCATABLE :: IWORK
LOGICAL                                         :: GGOOD
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_read_lfi_X3',TRIM(TPFILE%CNAME)//': reading '//TRIM(TPFIELD%CMNHNAME))
!
ILENG = SIZE(PFIELD)
!
CALL IO_Field_read_check_lfi(TPFILE,TPFIELD,ILENG,IWORK,ITOTAL,IRESP,GGOOD)
!
IF (GGOOD) PFIELD = RESHAPE( TRANSFER_I8_R(IWORK(IWORK(2)+3:)) , SHAPE(PFIELD) )
!
KRESP=IRESP
!
IF (ALLOCATED(IWORK)) DEALLOCATE(IWORK)
!
END SUBROUTINE IO_Field_read_lfi_X3
!
!
SUBROUTINE IO_Field_read_lfi_X4(TPFILE,TPFIELD,PFIELD,KRESP)
USE MODE_MSG
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
TYPE(TFILEDATA),            INTENT(IN)    :: TPFILE
CLASS(tfieldmetadata_base), INTENT(INOUT) :: TPFIELD
REAL, DIMENSION(:,:,:,:),   INTENT(INOUT) :: PFIELD  ! array containing the data field
INTEGER,                    INTENT(OUT)   :: KRESP   ! return-code if problems occured
!
!*      0.2   Declarations of local variables
!
INTEGER(KIND=LFIINT)                            :: IRESP,ITOTAL
INTEGER                                         :: ILENG
INTEGER(KIND=MNHINT64),DIMENSION(:),ALLOCATABLE :: IWORK
LOGICAL                                         :: GGOOD
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_read_lfi_X4',TRIM(TPFILE%CNAME)//': reading '//TRIM(TPFIELD%CMNHNAME))
!
ILENG = SIZE(PFIELD)
!
CALL IO_Field_read_check_lfi(TPFILE,TPFIELD,ILENG,IWORK,ITOTAL,IRESP,GGOOD)
!
IF (GGOOD) PFIELD = RESHAPE( TRANSFER_I8_R(IWORK(IWORK(2)+3:)) , SHAPE(PFIELD) )
!
KRESP=IRESP
!
IF (ALLOCATED(IWORK)) DEALLOCATE(IWORK)
!
END SUBROUTINE IO_Field_read_lfi_X4
!
!
SUBROUTINE IO_Field_read_lfi_X5(TPFILE,TPFIELD,PFIELD,KRESP)
USE MODE_MSG
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
TYPE(TFILEDATA),            INTENT(IN)    :: TPFILE
CLASS(tfieldmetadata_base), INTENT(INOUT) :: TPFIELD
REAL ,DIMENSION(:,:,:,:,:), INTENT(INOUT) :: PFIELD  ! array containing the data field
INTEGER,                    INTENT(OUT)   :: KRESP   ! return-code if problems occured
!
!*      0.2   Declarations of local variables
!
INTEGER(KIND=LFIINT)                            :: IRESP,ITOTAL
INTEGER                                         :: ILENG
INTEGER(KIND=MNHINT64),DIMENSION(:),ALLOCATABLE :: IWORK
LOGICAL                                         :: GGOOD
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_read_lfi_X5',TRIM(TPFILE%CNAME)//': reading '//TRIM(TPFIELD%CMNHNAME))
!
ILENG = SIZE(PFIELD)
!
CALL IO_Field_read_check_lfi(TPFILE,TPFIELD,ILENG,IWORK,ITOTAL,IRESP,GGOOD)
!
IF (GGOOD) PFIELD = RESHAPE( TRANSFER_I8_R(IWORK(IWORK(2)+3:)) , SHAPE(PFIELD) )
!
KRESP=IRESP
!
IF (ALLOCATED(IWORK)) DEALLOCATE(IWORK)
!
END SUBROUTINE IO_Field_read_lfi_X5
!
!
SUBROUTINE IO_Field_read_lfi_X6(TPFILE,TPFIELD,PFIELD,KRESP)
USE MODE_MSG
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
TYPE(TFILEDATA),              INTENT(IN)    :: TPFILE
CLASS(tfieldmetadata_base),   INTENT(INOUT) :: TPFIELD
REAL, DIMENSION(:,:,:,:,:,:), INTENT(INOUT) :: PFIELD  ! array containing the data field
INTEGER,                      INTENT(OUT)   :: KRESP   ! return-code if problems occured
!
!*      0.2   Declarations of local variables
!
INTEGER(KIND=LFIINT)                            :: IRESP,ITOTAL
INTEGER                                         :: ILENG
INTEGER(KIND=MNHINT64),DIMENSION(:),ALLOCATABLE :: IWORK
LOGICAL                                         :: GGOOD
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_read_lfi_X6',TRIM(TPFILE%CNAME)//': reading '//TRIM(TPFIELD%CMNHNAME))
!
ILENG = SIZE(PFIELD)
!
CALL IO_Field_read_check_lfi(TPFILE,TPFIELD,ILENG,IWORK,ITOTAL,IRESP,GGOOD)
!
IF (GGOOD) PFIELD = RESHAPE( TRANSFER_I8_R(IWORK(IWORK(2)+3:)) , SHAPE(PFIELD) )
!
KRESP=IRESP
!
IF (ALLOCATED(IWORK)) DEALLOCATE(IWORK)
!
END SUBROUTINE IO_Field_read_lfi_X6
!
!
SUBROUTINE IO_Field_read_lfi_N0(TPFILE,TPFIELD,KFIELD,KRESP)
USE MODE_MSG
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
TYPE(TFILEDATA),            INTENT(IN)    :: TPFILE
CLASS(tfieldmetadata_base), INTENT(INOUT) :: TPFIELD
INTEGER,                    INTENT(INOUT) :: KFIELD  ! array containing the data field
INTEGER,                    INTENT(OUT)   :: KRESP   ! return-code if problems occured
!
!*      0.2   Declarations of local variables
!
INTEGER(KIND=LFIINT)                            :: IRESP,ITOTAL
INTEGER                                         :: ILENG
INTEGER(KIND=MNHINT64),DIMENSION(:),ALLOCATABLE :: IWORK
LOGICAL                                         :: GGOOD
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_read_lfi_N0',TRIM(TPFILE%CNAME)//': reading '//TRIM(TPFIELD%CMNHNAME))
!
ILENG = 1
!
CALL IO_Field_read_check_lfi(TPFILE,TPFIELD,ILENG,IWORK,ITOTAL,IRESP,GGOOD)
!
IF (GGOOD) KFIELD = IWORK(IWORK(2)+3)
!
KRESP=IRESP
!
IF (ALLOCATED(IWORK)) DEALLOCATE(IWORK)
!
END SUBROUTINE IO_Field_read_lfi_N0
!
!
SUBROUTINE IO_Field_read_lfi_N1(TPFILE,TPFIELD,KFIELD,KRESP)
USE MODE_MSG
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
TYPE(TFILEDATA),            INTENT(IN)    :: TPFILE
CLASS(tfieldmetadata_base), INTENT(INOUT) :: TPFIELD
INTEGER, DIMENSION(:),      INTENT(INOUT) :: KFIELD  ! array containing the data field
INTEGER,                    INTENT(OUT)   :: KRESP   ! return-code if problems occured
!
!*      0.2   Declarations of local variables
!
INTEGER(KIND=LFIINT)                            :: IRESP,ITOTAL
INTEGER                                         :: ILENG
INTEGER(KIND=MNHINT64),DIMENSION(:),ALLOCATABLE :: IWORK
LOGICAL                                         :: GGOOD
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_read_lfi_N1',TRIM(TPFILE%CNAME)//': reading '//TRIM(TPFIELD%CMNHNAME))
!
ILENG = SIZE(KFIELD)
!
CALL IO_Field_read_check_lfi(TPFILE,TPFIELD,ILENG,IWORK,ITOTAL,IRESP,GGOOD)
!
IF (GGOOD) KFIELD(:) = IWORK(IWORK(2)+3:)
!
KRESP=IRESP
!
IF (ALLOCATED(IWORK)) DEALLOCATE(IWORK)
!
END SUBROUTINE IO_Field_read_lfi_N1
!
!
SUBROUTINE IO_Field_read_lfi_N2(TPFILE,TPFIELD,KFIELD,KRESP)
USE MODE_MSG
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
TYPE(TFILEDATA),            INTENT(IN)    :: TPFILE
CLASS(tfieldmetadata_base), INTENT(INOUT) :: TPFIELD
INTEGER, DIMENSION(:,:),    INTENT(INOUT) :: KFIELD  ! array containing the data field
INTEGER,                    INTENT(OUT)   :: KRESP   ! return-code if problems occured
!
!*      0.2   Declarations of local variables
!
INTEGER(KIND=LFIINT)                            :: IRESP,ITOTAL
INTEGER                                         :: ILENG
INTEGER(KIND=MNHINT64),DIMENSION(:),ALLOCATABLE :: IWORK
LOGICAL                                         :: GGOOD
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_read_lfi_N2',TRIM(TPFILE%CNAME)//': reading '//TRIM(TPFIELD%CMNHNAME))
!
ILENG = SIZE(KFIELD)
!
CALL IO_Field_read_check_lfi(TPFILE,TPFIELD,ILENG,IWORK,ITOTAL,IRESP,GGOOD)
!
IF (GGOOD) KFIELD(:,:) = RESHAPE(IWORK(IWORK(2)+3:),SHAPE(KFIELD))
!
KRESP=IRESP
!
IF (ALLOCATED(IWORK)) DEALLOCATE(IWORK)
!
END SUBROUTINE IO_Field_read_lfi_N2
!
!
SUBROUTINE IO_Field_read_lfi_N3(TPFILE,TPFIELD,KFIELD,KRESP)
USE MODE_MSG
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
TYPE(TFILEDATA),            INTENT(IN)    :: TPFILE
CLASS(tfieldmetadata_base), INTENT(INOUT) :: TPFIELD
INTEGER, DIMENSION(:,:,:),  INTENT(INOUT) :: KFIELD  ! array containing the data field
INTEGER,                    INTENT(OUT)   :: KRESP   ! return-code if problems occured
!
!*      0.2   Declarations of local variables
!
INTEGER(KIND=LFIINT)                            :: IRESP,ITOTAL
INTEGER                                         :: ILENG
INTEGER(KIND=MNHINT64),DIMENSION(:),ALLOCATABLE :: IWORK
LOGICAL                                         :: GGOOD
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_read_lfi_N3',TRIM(TPFILE%CNAME)//': reading '//TRIM(TPFIELD%CMNHNAME))
!
ILENG = SIZE(KFIELD)
!
CALL IO_Field_read_check_lfi(TPFILE,TPFIELD,ILENG,IWORK,ITOTAL,IRESP,GGOOD)
!
IF (GGOOD) KFIELD(:,:,:) = RESHAPE(IWORK(IWORK(2)+3:),SHAPE(KFIELD))
!
KRESP=IRESP
!
IF (ALLOCATED(IWORK)) DEALLOCATE(IWORK)
!
END SUBROUTINE IO_Field_read_lfi_N3
!
!
SUBROUTINE IO_Field_read_lfi_L0(TPFILE,TPFIELD,OFIELD,KRESP)
USE MODE_MSG
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
TYPE(TFILEDATA),            INTENT(IN)    :: TPFILE
CLASS(tfieldmetadata_base), INTENT(INOUT) :: TPFIELD
LOGICAL,                    INTENT(INOUT) :: OFIELD  ! array containing the data field
INTEGER,                    INTENT(OUT)   :: KRESP   ! return-code if problems occured
!
!*      0.2   Declarations of local variables
!
INTEGER(KIND=LFIINT)                            :: IRESP,ITOTAL
INTEGER                                         :: ILENG
INTEGER                                         :: IFIELD
INTEGER(KIND=MNHINT64),DIMENSION(:),ALLOCATABLE :: IWORK
LOGICAL                                         :: GGOOD
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_read_lfi_L0',TRIM(TPFILE%CNAME)//': reading '//TRIM(TPFIELD%CMNHNAME))
!
ILENG = 1
!
CALL IO_Field_read_check_lfi(TPFILE,TPFIELD,ILENG,IWORK,ITOTAL,IRESP,GGOOD)
!
IF (GGOOD) THEN
  IFIELD = IWORK(IWORK(2)+3)
  IF (IFIELD==0) THEN
    OFIELD = .FALSE.
  ELSE IF (IFIELD==1) THEN
    OFIELD = .TRUE.
  ELSE
    CALL PRINT_MSG(NVERB_ERROR,'IO','IO_Field_read_lfi_L0',TRIM(TPFILE%CNAME)//': invalid value in file for ' &
                                                           //TRIM(TPFIELD%CMNHNAME))
    OFIELD = .TRUE.
    IRESP = -112
  END IF
END IF
!
KRESP=IRESP
!
IF (ALLOCATED(IWORK)) DEALLOCATE(IWORK)
!
END SUBROUTINE IO_Field_read_lfi_L0
!
!
SUBROUTINE IO_Field_read_lfi_L1(TPFILE,TPFIELD,OFIELD,KRESP)
USE MODE_MSG
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
TYPE(TFILEDATA),            INTENT(IN)    :: TPFILE
CLASS(tfieldmetadata_base), INTENT(INOUT) :: TPFIELD
LOGICAL, DIMENSION(:),      INTENT(INOUT) :: OFIELD  ! array containing the data field
INTEGER,                    INTENT(OUT)   :: KRESP   ! return-code if problems occured
!
!*      0.2   Declarations of local variables
!
INTEGER(KIND=LFIINT)                            :: IRESP,ITOTAL
INTEGER                                         :: ILENG
INTEGER                                         :: JI
INTEGER, DIMENSION(SIZE(OFIELD))                :: IFIELD
INTEGER(KIND=MNHINT64),DIMENSION(:),ALLOCATABLE :: IWORK
LOGICAL                                         :: GGOOD
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_read_lfi_L1',TRIM(TPFILE%CNAME)//': reading '//TRIM(TPFIELD%CMNHNAME))
!
ILENG = SIZE(OFIELD)
!
CALL IO_Field_read_check_lfi(TPFILE,TPFIELD,ILENG,IWORK,ITOTAL,IRESP,GGOOD)
!
IF (GGOOD) THEN
  IFIELD(:) = IWORK(IWORK(2)+3:)
  DO JI=1,ILENG
    IF (IFIELD(JI)==0) THEN
      OFIELD(JI) = .FALSE.
    ELSE IF (IFIELD(JI)==1) THEN
      OFIELD(JI) = .TRUE.
    ELSE
      OFIELD(JI) = .TRUE.
      IRESP = -112
    END IF
  END DO
  IF (IRESP==-112) THEN
    CALL PRINT_MSG(NVERB_ERROR,'IO','IO_Field_read_lfi_L1',TRIM(TPFILE%CNAME)//': invalid value(s) in file for ' &
                                                           //TRIM(TPFIELD%CMNHNAME))
  END IF
END IF
!
KRESP=IRESP
!
IF (ALLOCATED(IWORK)) DEALLOCATE(IWORK)
!
END SUBROUTINE IO_Field_read_lfi_L1
!
!
SUBROUTINE IO_Field_read_lfi_C0(TPFILE,TPFIELD,HFIELD,KRESP)
!
USE MODD_PARAMETERS, ONLY: NFILENAMELGTMAXLFI
!
USE MODE_MSG
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
TYPE(TFILEDATA),            INTENT(IN)    :: TPFILE
CLASS(tfieldmetadata_base), INTENT(INOUT) :: TPFIELD
CHARACTER(LEN=*),           INTENT(INOUT) :: HFIELD  ! array containing the data field
INTEGER,                    INTENT(OUT)   :: KRESP   ! return-code if problems occured
!
!*      0.2   Declarations of local variables
!
INTEGER(KIND=LFIINT)                            :: IRESP,ITOTAL
INTEGER                                         :: ILENG, ILENGMAX, JLOOP
INTEGER(KIND=MNHINT64),DIMENSION(:),ALLOCATABLE :: IWORK
LOGICAL                                         :: GGOOD
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_read_lfi_C0',TRIM(TPFILE%CNAME)//': reading '//TRIM(TPFIELD%CMNHNAME))
!
ILENG = LEN(HFIELD)
ILENGMAX = ILENG
!
!Special treatment for MY_NAME and DAD_NAME fields (for backward compatibility)
IF (TPFIELD%CMNHNAME=='MY_NAME' .OR. TPFIELD%CMNHNAME=='DAD_NAME') THEN
  ILENG = MIN(LEN(HFIELD),NFILENAMELGTMAXLFI)
  ILENGMAX = NFILENAMELGTMAXLFI
  IF (LEN(HFIELD)<NFILENAMELGTMAXLFI) &
        CALL PRINT_MSG(NVERB_WARNING,'IO','IO_Field_read_lfi_C0',TRIM(TPFILE%CNAME)// &
                      ': LEN(HFIELD)<NFILENAMELGTMAXLFI')
END IF
!
CALL IO_Field_read_check_lfi(TPFILE,TPFIELD,ILENGMAX,IWORK,ITOTAL,IRESP,GGOOD)
!
IF (GGOOD) THEN
  DO JLOOP=1,ILENG
    HFIELD(JLOOP:JLOOP)=ACHAR(IWORK(IWORK(2)+2+JLOOP))
  END DO
END IF
!
KRESP=IRESP
!
IF (ALLOCATED(IWORK)) DEALLOCATE(IWORK)
!
END SUBROUTINE IO_Field_read_lfi_C0
!
!
SUBROUTINE IO_Field_read_lfi_T0(TPFILE,TPFIELD,TPDATA,KRESP)
!
USE MODE_MSG
USE MODD_TYPE_DATE
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
TYPE(TFILEDATA),            INTENT(IN)    :: TPFILE
CLASS(tfieldmetadata_base), INTENT(INOUT) :: TPFIELD
TYPE (DATE_TIME),           INTENT(INOUT) :: TPDATA  ! array containing the data field
INTEGER,                    INTENT(OUT)   :: KRESP   ! return-code if problems occured
!
!*      0.2   Declarations of local variables
!
INTEGER(KIND=LFIINT)                            :: IRESP, ITOTAL
INTEGER                                         :: ILENG
INTEGER(KIND=MNHINT64),DIMENSION(:),ALLOCATABLE :: IWORK
LOGICAL                                         :: GGOOD
CLASS(tfieldmetadata_base), ALLOCATABLE         :: TZFIELD
INTEGER, DIMENSION(3)                           :: ITDATE    ! date array
REAL,DIMENSION(1)                               :: ZTIME
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_read_lfi_T0',TRIM(TPFILE%CNAME)//': reading '//TRIM(TPFIELD%CMNHNAME))
!
ALLOCATE( TZFIELD, SOURCE = TPFIELD ) ! TZFIELD = TPFIELD (sourced allocation, TZFIELD is allocated to a clone of TPFIELD)
!
! Read date
!
TZFIELD%CMNHNAME = TRIM(TPFIELD%CMNHNAME)//'%TDATE'
TZFIELD%CCOMMENT = 'YYYYMMDD'
!
ILENG=SIZE(ITDATE)
!
CALL IO_Field_read_check_lfi(TPFILE,TZFIELD,ILENG,IWORK,ITOTAL,IRESP,GGOOD)
!
IF (GGOOD) THEN
  TPDATA%nyear  = IWORK(IWORK(2)+2+1)
  TPDATA%nmonth = IWORK(IWORK(2)+2+2)
  TPDATA%nday   = IWORK(IWORK(2)+2+3)
END IF
!
IF (ALLOCATED(IWORK)) DEALLOCATE(IWORK)
!
IF (.NOT.GGOOD) THEN
  KRESP = IRESP
  RETURN
END IF
!
! Read time
!
TZFIELD%CMNHNAME = TRIM(TPFIELD%CMNHNAME)//'%TIME'
TZFIELD%CCOMMENT = 'SECONDS'
!
ILENG=1
!
CALL IO_Field_read_check_lfi(TPFILE,TZFIELD,ILENG,IWORK,ITOTAL,IRESP,GGOOD)
!
IF (GGOOD) THEN
  !TRANSFER_I8_R works with 1D arrays
  ZTIME = TRANSFER_I8_R( (/ IWORK(IWORK(2)+3) /) )
  TPDATA%xtime = ZTIME(1)
END IF
!
IF (ALLOCATED(IWORK)) DEALLOCATE(IWORK)
!
KRESP = IRESP
!
END SUBROUTINE IO_Field_read_lfi_T0
!
!
SUBROUTINE IO_Field_read_check_lfi(TPFILE,TPFIELD,KLENG,KWORK,KTOTAL,KRESP,OGOOD)
!
USE MODD_PARAMETERS, ONLY: NGRIDUNKNOWN
!
TYPE(TFILEDATA),                                INTENT(IN)    :: TPFILE
CLASS(tfieldmetadata_base),                     INTENT(INOUT) :: TPFIELD
INTEGER,                                        INTENT(IN)    :: KLENG
INTEGER(KIND=MNHINT64),DIMENSION(:),ALLOCATABLE,INTENT(OUT)   :: KWORK
INTEGER(KIND=LFIINT),                           INTENT(OUT)   :: KTOTAL
INTEGER(KIND=LFIINT),                           INTENT(OUT)   :: KRESP
LOGICAL,                                        INTENT(OUT)   :: OGOOD
!
INTEGER                      :: IERRLEVEL,IROW,J
INTEGER,DIMENSION(JPXKRK)    :: ICOMMENT
INTEGER(KIND=LFIINT)         :: ICOMLEN,INUMBR,IPOSEX
CHARACTER(LEN=:),ALLOCATABLE :: YMSG
CHARACTER(LEN=12)            :: YRECLENGTH_FILE, YRECLENGTH_MEM
CHARACTER(LEN=12)            :: YVAL_FILE, YVAL_MEM
CHARACTER(LEN=JPXKRK)        :: YCOMMENT
CHARACTER(LEN=12)            :: YRESP
CHARACTER(LEN=LEN_HREC)      :: YRECFM
LOGICAL                      :: GOLDMNH !if old version of MesoNH (<5.4, old files without complete and correct metadata)
!
OGOOD = .TRUE.
!
GOLDMNH = TPFILE%NMNHVERSION(1)<5 .OR. (TPFILE%NMNHVERSION(1)==5 .AND. TPFILE%NMNHVERSION(2)<4)
!
YRECFM=TRIM(TPFIELD%CMNHNAME)
IF( LEN_TRIM(TPFIELD%CMNHNAME) > LEN(YRECFM) ) &
  CALL PRINT_MSG(NVERB_WARNING,'IO','IO_Field_read_check_lfi','field name was truncated to '&
                 //YRECFM//' for '//TRIM(TPFIELD%CMNHNAME))
!
!*      2.a   LET'S GET SOME INFORMATION ON THE DESIRED ARTICLE
!
INUMBR = TPFILE%NLFIFLU
CALL LFINFO(KRESP,INUMBR,YRECFM,KTOTAL,IPOSEX)
!
IF (KRESP.NE.0) THEN
  WRITE(YRESP, '( I12 )') KRESP
  YMSG = 'RESP='//TRIM(ADJUSTL(YRESP))//' in call to LFINFO when reading '//TRIM(TPFIELD%CMNHNAME)//' in '//TRIM(TPFILE%CNAME)
  CALL PRINT_MSG(NVERB_ERROR,'IO','IO_Field_read_check_lfi',YMSG)
  OGOOD = .FALSE.
  RETURN
ELSEIF (KTOTAL.EQ.0) THEN
  CALL PRINT_MSG(NVERB_WARNING,'IO','IO_Field_read_check_lfi',TRIM(TPFILE%CNAME)//': record length is zero for ' &
                                                                  //TRIM(TPFIELD%CMNHNAME))
  KRESP=-47
  OGOOD = .FALSE.
  RETURN
ELSEIF (KTOTAL.GT.JPXFIE) THEN
  CALL PRINT_MSG(NVERB_WARNING,'IO','IO_Field_read_check_lfi',TRIM(TPFILE%CNAME)// &
                               ': record length exceeds the maximum value in FM for '//TRIM(TPFIELD%CMNHNAME))
  KRESP=-48
  OGOOD = .FALSE.
  RETURN
ENDIF
!
!*      2.b   UNFORMATTED DIRECT ACCESS READ OPERATION
!
ALLOCATE(KWORK(KTOTAL))
!
CALL LFILEC(KRESP,INUMBR,YRECFM,KWORK,KTOTAL)
IF (KRESP.NE.0) THEN
  WRITE(YRESP, '( I12 )') KRESP
  YMSG = 'RESP='//TRIM(ADJUSTL(YRESP))//' in call to LFILEC when reading '//TRIM(TPFIELD%CMNHNAME)//' in '//TRIM(TPFILE%CNAME)
  CALL PRINT_MSG(NVERB_ERROR,'IO','IO_Field_read_check_lfi',YMSG)
  OGOOD = .FALSE.
  RETURN
ENDIF
!
!*      2.c   THE GRID INDICATOR AND THE COMMENT STRING
!*            ARE SEPARATED FROM THE DATA
!
ICOMLEN = KWORK(2)
IROW=KLENG+ICOMLEN+2
IF (KTOTAL.NE.IROW) THEN
  WRITE(YRECLENGTH_FILE,'(I12)') KTOTAL-2-ICOMLEN
  WRITE(YRECLENGTH_MEM, '(I12)') KLENG
  CALL PRINT_MSG(NVERB_ERROR,'IO','IO_Field_read_check_lfi','wrong field size for '//TRIM(TPFIELD%CMNHNAME) &
                                     //' (expected: '//TRIM(ADJUSTL(YRECLENGTH_MEM))//                            &
                                     ', in file: '   //TRIM(ADJUSTL(YRECLENGTH_FILE))//')')
  KRESP=-63
  OGOOD = .FALSE.
  RETURN
ENDIF
!
IF(TPFIELD%NGRID==NGRIDUNKNOWN) TPFIELD%NGRID=KWORK(1)
!
IF (KWORK(1)/=TPFIELD%NGRID) THEN
  WRITE(YVAL_FILE,'(I12)') KWORK(1)
  WRITE(YVAL_MEM, '(I12)') TPFIELD%NGRID
  IF (TPFIELD%NDIMS==0 .OR. GOLDMNH) THEN
    IERRLEVEL = NVERB_WARNING
  ELSE
    IERRLEVEL = NVERB_ERROR
  END IF
  CALL PRINT_MSG(IERRLEVEL,'IO','IO_Field_read_check_lfi','expected GRID value ('//TRIM(ADJUSTL(YVAL_MEM))// &
                 ') is different than found in file ('//TRIM(ADJUSTL(YVAL_FILE))//') for variable '//TRIM(TPFIELD%CMNHNAME))
  IF(.NOT.GOLDMNH) THEN !Do not modify probably incorrect grid number (to prevent problems later with other correct files)
    TPFIELD%NGRID = KWORK(1)
    KRESP = -111 !Used later to broadcast modified metadata
  END IF
ELSE
  CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_read_check_lfi','expected GRID    found in file for field ' &
                                                            //TRIM(TPFIELD%CMNHNAME))
ENDIF
!
YCOMMENT=''
SELECT CASE (ICOMLEN)
CASE(:-1)
  CALL PRINT_MSG(NVERB_ERROR,'IO','IO_Field_read_check_lfi',TRIM(TPFILE%CNAME)//': comment length is negative for ' &
                                                                  //TRIM(TPFIELD%CMNHNAME))
  KRESP=-58
  OGOOD = .FALSE.
  RETURN
CASE(0)
  KRESP = 0
CASE(1:JPXKRK)
  ICOMMENT(1:ICOMLEN)=KWORK(3:ICOMLEN+2)
  DO J=1,ICOMLEN
    YCOMMENT(J:J)=CHAR(ICOMMENT(J))
  ENDDO
CASE(JPXKRK+1:)
  CALL PRINT_MSG(NVERB_WARNING,'IO','IO_Field_read_check_lfi',TRIM(TPFILE%CNAME)//': comment is too long in file for ' &
                                                                  //TRIM(TPFIELD%CMNHNAME))
  KRESP=-56
  RETURN
END SELECT
!
IF (TRIM(YCOMMENT)/=TRIM(TPFIELD%CCOMMENT)) THEN
  CALL PRINT_MSG(NVERB_INFO,'IO','IO_Field_read_check_lfi','expected COMMENT ('//TRIM(TPFIELD%CCOMMENT)// &
                 ') is different than found ('//TRIM(YCOMMENT)//') in file for field '//TRIM(TPFIELD%CMNHNAME))
  TPFIELD%CCOMMENT=TRIM(YCOMMENT)
  KRESP = -111 !Used later to broadcast modified metadata
ELSE
  CALL PRINT_MSG(NVERB_DEBUG,'IO','IO_Field_read_check_lfi','expected COMMENT found in file for field ' &
                                                            //TRIM(TPFIELD%CMNHNAME))
END IF
!
END SUBROUTINE IO_Field_read_check_lfi
!
!
FUNCTION TRANSFER_I8_R(KFIELDIN) RESULT(PFIELDOUT)
!
INTEGER(KIND=MNHINT64),DIMENSION(:),INTENT(IN) :: KFIELDIN
REAL,DIMENSION(SIZE(KFIELDIN))           :: PFIELDOUT
!
INTEGER :: ILENG
#if (MNH_REAL == 4)
REAL(KIND=MNHREAL64),DIMENSION(:),ALLOCATABLE :: ZFIELD8
#endif
!
ILENG = SIZE(PFIELDOUT)
!
#if (MNH_REAL == 8)
  PFIELDOUT(:) = TRANSFER(KFIELDIN,PFIELDOUT(1),ILENG)
#else
  ALLOCATE(ZFIELD8(ILENG))
  ZFIELD8(:) = TRANSFER(KFIELDIN,ZFIELD8(1),ILENG)
  PFIELDOUT(:) = REAL(ZFIELD8(:),KIND=MNHREAL32)
  DEALLOCATE(ZFIELD8)
#endif
!
END FUNCTION TRANSFER_I8_R


end module mode_io_read_lfi
