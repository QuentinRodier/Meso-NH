!MNH_LIC Copyright 1994-2019 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
! Author(s)
!  D. Gazen
! Modifications:
!  P. Wautelet 01/03/2019: move OPEN_ll to mode_io_file.f90 and SET_FMPACK_ll to here from mode_fm.f90
!
!-----------------------------------------------------------------
MODULE MODE_IO_ll

  USE MODD_MPIF
  USE MODD_VAR_ll, ONLY : NMNH_COMM_WORLD

  USE MODE_MSG

  IMPLICIT NONE 

  PRIVATE

  LOGICAL,SAVE :: GCONFIO = .FALSE. ! Turn TRUE when SET_CONFIO_ll is called.

  public :: GCONFIO
  public :: INITIO_ll, SET_CONFIO_ll
  public :: SET_FMPACK_ll

CONTAINS 

  SUBROUTINE SET_CONFIO_ll()
    USE MODN_CONFIO, only: LCDF4, LLFIOUT, LLFIREAD

    !Use MODN_CONFIO namelist variables
    CALL SET_CONFIO_INTERN_ll(LCDF4, LLFIOUT, LLFIREAD)
  END SUBROUTINE SET_CONFIO_ll

  SUBROUTINE SET_CONFIO_INTERN_ll(OIOCDF4, OLFIOUT, OLFIREAD)
    USE MODD_IO_ll, ONLY : LIOCDF4, LLFIOUT, LLFIREAD, LIO_ALLOW_NO_BACKUP, LIO_NO_WRITE

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

      ! Set LIO_ALLOW_NO_BACKUP=.true. if writes are disabled (to be coherent)
      IF (LIO_NO_WRITE) THEN
        CALL PRINT_MSG(NVERB_WARNING,'IO','SET_CONFIO_ll','file writes are disabled')
        LIO_ALLOW_NO_BACKUP = .true.
      END IF
    END IF

  END SUBROUTINE SET_CONFIO_INTERN_ll

  SUBROUTINE INITIO_ll()
    USE MODD_IO_ll,     only: CNULLFILE, GSMONOPROC, nio_rank, ISNPROC, ISP, NNULLUNIT

    USE MODE_MNH_WORLD, ONLY: INIT_NMNH_COMM_WORLD

    IMPLICIT NONE

    INTEGER :: IERR, IOS
    character(len=256) :: yioerrmsg

    CALL PRINT_MSG(NVERB_DEBUG,'IO','INITIO_ll','called')

    CALL INIT_NMNH_COMM_WORLD(IERR)
    IF (IERR .NE.0) CALL PRINT_MSG(NVERB_FATAL,'IO','INITIO_ll','problem with remapping of NMNH_COMM_WORLD')

    !! Now MPI is initialized for sure

    !! Default number for Processor I/O
    nio_rank = 1

    !! Get number of allocated processors
    CALL MPI_COMM_SIZE(NMNH_COMM_WORLD, ISNPROC,IERR)
    IF (ISNPROC==1) GSMONOPROC = .TRUE.

    !! Store proc number
    CALL MPI_COMM_RANK(NMNH_COMM_WORLD, ISP, IERR)
    ISP = ISP + 1

    !! Open /dev/null for GLOBAL mode
#if defined(DEV_NULL)
    OPEN(NEWUNIT=NNULLUNIT,FILE=CNULLFILE  ,ACTION='WRITE',IOSTAT=IOS, IOMSG=yioerrmsg)
#else
    OPEN(NEWUNIT=NNULLUNIT,STATUS='SCRATCH',ACTION='WRITE',IOSTAT=IOS, IOMSG=yioerrmsg)
#endif
    IF (IOS /= 0) THEN
       CALL PRINT_MSG(NVERB_FATAL,'IO','INITIO_ll','problem opening /dev/null :'//trim(yioerrmsg))
    END IF
  END SUBROUTINE INITIO_ll


SUBROUTINE SET_FMPACK_ll(O1D,O2D,OPACK)
USE MODD_IO_ll,  ONLY: LPACK, L1D, L2D
USE MODD_VAR_ll, ONLY: IP

IMPLICIT NONE

LOGICAL, INTENT(IN) :: O1D,O2D,OPACK

LPACK = OPACK
L1D   = O1D
L2D   = O2D

IF ( IP == 1 ) PRINT *,'INIT L1D,L2D,LPACK = ',L1D,L2D,LPACK

END SUBROUTINE SET_FMPACK_ll

END MODULE MODE_IO_ll
