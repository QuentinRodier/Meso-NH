!MNH_LIC Copyright 2018-2019 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
! Author
!  P. Wautelet 14/12/2018
!
!  Remarks: some of the code comes from mode_fm.f90 and mode_io.f90
!           (was duplicated in the 2 files)
!
! Modifications:
!  P. Wautelet 10/01/2019: use NEWUNIT argument of OPEN
!                          + move IO_Flu_dealloc and IO_Flu_alloc to mode_io_file_lfi.f90
!                          + move management of NNCID and NLFIFLU to the nc4 and lfi subroutines
!  P. Wautelet 05/03/2019: rename IO subroutines and modules
!
!-----------------------------------------------------------------
module mode_io_file_lfi

use modd_io,     only: tfiledata

use mode_msg

implicit none

private

public :: IO_File_create_lfi, IO_File_close_lfi, IO_File_open_lfi

integer, parameter :: JPRESERVED_UNIT   = 11
integer, parameter :: JPMAX_UNIT_NUMBER = JPRESERVED_UNIT + 300

logical,save :: galloc(JPRESERVED_UNIT:JPMAX_UNIT_NUMBER) = .false.

contains

subroutine IO_File_create_lfi(tpfile, kstatus)
  use mode_io_tools,            only: IO_Filename_construct
  use mode_io_tools_mnhversion, only: IO_Mnhversion_set
  use mode_io_tools_lfi,        only: IO_Verbosity_prepare_lfi

  type(tfiledata), intent(inout) :: tpfile
  integer,         intent(inout) :: kstatus

  character(len=:), allocatable :: yfilem        ! name of the file
  character(len=:), allocatable :: yforstatus    ! Status for open of a file (for LFI) ('OLD','NEW','UNKNOWN','SCRATCH','REPLACE')
  integer(kind=LFI_INT)         :: iresou, inumbr
  integer(kind=LFI_INT)         :: imelev, inprar
  integer(kind=LFI_INT)         :: ininar        ! Number of articles present in LFI file
  logical                       :: gnewfi
  logical                       :: gnamfi, gfater, gstats

  call print_msg(NVERB_DEBUG,'IO','IO_File_create_lfi','called for '//trim(tpfile%cname))

  kstatus = 0

  if (tpfile%lmaster) then
    call IO_Filename_construct(tpfile, yfilem)

    iresou = 0
    if ( tpfile%nlfiflu /= -1 ) call print_msg(NVERB_ERROR,'IO', &
                                               'IO_File_create_lfi','file '//trim(yfilem)//'.lfi has already a unit number')
    tpfile%nlfiflu = IO_Flu_alloc()
    gnamfi = .true.
    yforstatus = 'REPLACE'
    gfater = .true.

    call IO_Verbosity_prepare_lfi(tpfile, imelev, gstats)

    inumbr = tpfile%nlfiflu
    inprar = tpfile%nlfinprar
    call lfiouv(iresou, inumbr, gnamfi, trim(yfilem)//'.lfi', yforstatus, gfater, gstats, imelev, inprar, ininar)

    tpfile%nlfininar = ininar

    if (iresou/=0) kstatus = int(iresou, kind=kind(kstatus))

    !test if file is newly defined
    gnewfi = (ininar==0) .or. (imelev<2)
    if (.not.gnewfi) then
      call print_msg(NVERB_INFO,'IO','IO_File_create_lfi','file '//trim(yfilem)//'.lfi previously created with LFI')
    endif
  end if
  call IO_Mnhversion_set(tpfile)
end subroutine IO_File_create_lfi


subroutine IO_File_close_lfi(tpfile, kstatus)
  type(tfiledata),   intent(inout)  :: tpfile
  integer, optional, intent(out)    :: kstatus

  character(len=*), parameter :: YSTATUS = 'KEEP'

  integer(kind=LFI_INT) :: istatus

  call print_msg(NVERB_DEBUG,'IO','IO_File_close_lfi','called for '//trim(tpfile%cname))

  istatus = 0

  if (tpfile%lmaster) then
    if ( tpfile%nlfiflu /= -1 ) then
      call lfifer(istatus, tpfile%nlfiflu, YSTATUS)
      call IO_Flu_dealloc(int(tpfile%nlfiflu))
      tpfile%nlfiflu = -1
    else
      istatus = -1
      call print_msg(NVERB_WARNING, 'IO', 'IO_File_close_lfi', 'file '//trim(tpfile%cname)//'.lfi is not opened')
    end if
  end if

  if (present(kstatus)) kstatus = int(istatus,kind=kind(kstatus))
end subroutine IO_File_close_lfi


subroutine IO_File_open_lfi(tpfile, kstatus)
  use mode_io_tools,            only: IO_Filename_construct
  use mode_io_tools_mnhversion, only: IO_Mnhversion_get
  use mode_io_tools_lfi,        only: IO_Verbosity_prepare_lfi

  type(tfiledata), intent(inout) :: tpfile
  integer,         intent(inout) :: kstatus

  character(len=:),allocatable :: yfilem        ! name of the file
  character(len=:),allocatable :: yforstatus    ! Status for open of a file (for LFI) ('OLD','NEW','UNKNOWN','SCRATCH','REPLACE')
  integer                      :: istatus
  integer(kind=LFI_INT)        :: iresou, inumbr
  integer(kind=LFI_INT)        :: imelev, inprar
  integer(kind=LFI_INT)        :: ininar        ! Number of articles present in LFI file
  logical                      :: gnewfi
  logical                      :: gnamfi, gfater, gstats

  call print_msg(NVERB_DEBUG,'IO','IO_File_open_lfi','called for '//trim(tpfile%cname))

  kstatus = 0

  if (tpfile%lmaster) then
    call IO_Filename_construct(tpfile, yfilem)

    iresou = 0
    if ( tpfile%nlfiflu /= -1 ) call print_msg(NVERB_ERROR,'IO', &
                                               'IO_File_open_lfi','file '//trim(yfilem)//'.lfi has already a unit number')
    tpfile%nlfiflu = IO_Flu_alloc()
    gnamfi = .true.
    yforstatus = 'OLD'
    gfater = .true.

    call IO_Verbosity_prepare_lfi(tpfile, imelev, gstats)

    inumbr = tpfile%nlfiflu
    inprar = tpfile%nlfinprar

    call lfiouv(iresou, inumbr, gnamfi, trim(yfilem)//'.lfi', yforstatus, gfater, gstats, imelev, inprar, ininar)

    tpfile%nlfininar = ininar

    if (iresou/=0) kstatus = int(iresou, kind=kind(kstatus))
  end if
  call IO_Mnhversion_get(tpfile)
end subroutine IO_File_open_lfi


function IO_Flu_alloc()
  use modd_io, only: nnullunit

  integer :: IO_Flu_alloc

  integer :: ji
  integer :: ios
  logical :: gexists, gopened, gfound

  gfound = .false.

  do ji = JPRESERVED_UNIT, JPMAX_UNIT_NUMBER
    if ( galloc(ji) ) cycle
    inquire(unit=ji, exist=gexists, opened=gopened, iostat=ios)
    if (gexists .and. .not. gopened .and. ios == 0) then
      IO_Flu_alloc   = ji
      gfound     = .true.
      galloc(ji) = .true.
      exit
    end if
  end do

  if (.not. gfound) then
    call print_msg(NVERB_ERROR,'IO','IO_Flu_alloc','wrong unit number')
    IO_Flu_alloc = nnullunit !/dev/null Fortran unit
  end if
end function IO_Flu_alloc


subroutine IO_Flu_dealloc(koflu)
  integer :: koflu

  if ( (koflu >= JPRESERVED_UNIT) .and. (koflu <= JPMAX_UNIT_NUMBER) ) then
    galloc(koflu) = .false.
  else
    call print_msg(NVERB_ERROR,'IO','IO_Flu_dealloc','wrong unit number')
  end if
end subroutine IO_Flu_dealloc


end module mode_io_file_lfi
