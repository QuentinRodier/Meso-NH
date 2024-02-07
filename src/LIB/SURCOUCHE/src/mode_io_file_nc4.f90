!MNH_LIC Copyright 2018-2024 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
! Author
!  P. Wautelet 13/12/2018
!
!  Remarks: some of the code comes from mode_fm.f90 and mode_io.f90
!           (was duplicated in the 2 files)
!
! Modifications:
!  P. Wautelet 10/01/2019: use NEWUNIT argument of OPEN
!                          + move IOFREEFLU and IONEWFLU to mode_io_file_lfi.f90
!                          + move management of NNCID and NLFIFLU to the nc4 and lfi subroutines
!  P. Wautelet 10/01/2019: replace handle_err by IO_Err_handle_nc4 for better netCDF error messages
!  P. Wautelet 05/03/2019: rename IO subroutines and modules
!  P. Wautelet 07/03/2019: bugfix: io_set_mnhversion must be called by all the processes
!  P. Wautelet 18/09/2019: correct support of 64bit integers (MNH_INT=8)
!  P. Wautelet 22/09/2020: use ldimreduced to allow reduction in the number of dimensions of fields (used by 2D simulations)
!-----------------------------------------------------------------
#ifdef MNH_IOCDF4
module mode_io_file_nc4

use modd_io,           only: tfiledata
use modd_precision,    only: CDFINT

use mode_io_tools_nc4, only: IO_Err_handle_nc4, IO_Knowndims_set_nc4, IO_Iocdf_alloc_nc4
use mode_msg

use NETCDF,            only: NF90_CLOBBER, NF90_GLOBAL, NF90_NETCDF4, NF90_NOERR, NF90_NOWRITE,  &
                             NF90_CLOSE, NF90_CREATE, NF90_GET_ATT, NF90_INQUIRE, NF90_INQUIRE_ATTRIBUTE, &
                             NF90_OPEN, NF90_PUT_ATT, NF90_STRERROR

implicit none

private

public :: IO_File_create_nc4, IO_File_close_nc4, IO_File_open_nc4

contains

subroutine IO_File_create_nc4(tpfile,hprogram_orig)
  use mode_io_tools,            only: IO_Filename_construct
  use mode_io_tools_mnhversion, only: IO_Mnhversion_set

  type(tfiledata),           intent(inout) :: tpfile
  character(len=*),optional, intent(in)    :: hprogram_orig !to emulate a file coming from this program

  character(len=:),allocatable :: yfilem  ! name of the file
  integer(kind=CDFINT)         :: istatus

  call print_msg(NVERB_DEBUG,'IO','IO_File_create_nc4','called for '//trim(tpfile%cname))

  if (tpfile%lmaster) then
    call IO_Filename_construct(tpfile, yfilem)

    tpfile%tncdims => IO_Iocdf_alloc_nc4()
    istatus = NF90_CREATE(adjustl(trim(yfilem))//".nc", ior(NF90_CLOBBER,NF90_NETCDF4), tpfile%nncid)
    if (istatus /= NF90_NOERR) then
      call print_msg(NVERB_FATAL,'IO','IO_File_create_nc4','NF90_CREATE for '//trim(yfilem)//'.nc: '//NF90_STRERROR(istatus))
    end if
    call IO_Not_cleanly_closed_set_nc4(tpfile)
    call IO_Knowndims_set_nc4(tpfile, hprogram_orig=hprogram_orig)
  end if
  call IO_Mnhversion_set(tpfile)
end subroutine IO_File_create_nc4


subroutine IO_File_close_nc4(tpfile,kstatus)
  use mode_io_tools_nc4, only: IO_Iocdf_dealloc_nc4

  type(tfiledata),           intent(inout) :: tpfile
  integer,         optional, intent(out)   :: kstatus

  integer(kind=CDFINT) :: istatus

  call print_msg(NVERB_DEBUG,'IO','IO_File_close_nc4','called for '//trim(tpfile%cname))

  istatus = 0

  if (tpfile%lmaster ) then
    if (tpfile%nncid == -1) then
      call print_msg(NVERB_WARNING, 'IO', 'IO_File_close_nc4', 'file '//trim(tpfile%cname)//'.nc is not opened')
    else
      if (trim(tpfile%cmode) == 'WRITE') call IO_Cleanly_closed_set_nc4(tpfile)
      istatus = NF90_CLOSE(tpfile%nncid)
      if (istatus /= NF90_NOERR) then
        call print_msg(NVERB_WARNING, 'IO', 'IO_File_close_nc4', 'NF90_CLOSE error: '//trim(NF90_STRERROR(istatus)))
      end if
      tpfile%nncid = -1
      if (associated(tpfile%tncdims)) call IO_Iocdf_dealloc_nc4(tpfile%tncdims)
    end if
  end if

  if (present(kstatus)) kstatus = istatus
end subroutine IO_File_close_nc4


subroutine IO_File_open_nc4(tpfile)
  use mode_io_tools,            only: IO_Filename_construct
  use mode_io_tools_mnhversion, only: IO_Mnhversion_get

  type(tfiledata), intent(inout) :: tpfile

  character(len=:),allocatable :: yfilem  ! name of the file
  integer(kind=CDFINT)         :: istatus

  call print_msg(NVERB_DEBUG,'IO','IO_File_open_nc4','called for '//trim(tpfile%cname))

  if (tpfile%lmaster) then
    call IO_Filename_construct(tpfile, yfilem)

    tpfile%tncdims => IO_Iocdf_alloc_nc4()
    istatus = NF90_OPEN(adjustl(trim(yfilem))//".nc", NF90_NOWRITE, tpfile%nncid)
    if (istatus /= NF90_NOERR) then
      call print_msg(NVERB_FATAL, 'IO', 'IO_File_open_nc4', 'NF90_OPEN for '//trim(yfilem)//'.nc: '//NF90_STRERROR(istatus))
    end if

    istatus = NF90_INQUIRE(tpfile%nncid, nvariables=tpfile%nncnar)
    if (istatus /= NF90_NOERR) then
      call print_msg(NVERB_FATAL,'IO','IO_File_open_nc4','NF90_INQUIRE for '//trim(yfilem)//'.nc: '//NF90_STRERROR(istatus))
    end if
  end if

  if (trim(tpfile%cmode) == 'READ') then
    call IO_Mnhversion_get(tpfile)
    if (tpfile%lmaster) then
      call IO_Cleanly_closed_check_nc4( tpfile )
      ! Do not check precision loss on subfiles (parent has all the information needed)
      if ( .not. associated( tpfile%tmainfile ) ) call IO_Check_precision_loss_nc4( tpfile )
    end if
    call IO_Are_dimension_reduced(tpfile)
  end if

end subroutine IO_File_open_nc4


subroutine IO_Are_dimension_reduced(tpfile)
  use modd_io,        only: isp
  use modd_mpif
  use modd_precision, only: MNHLOG_MPI

  type(tfiledata), intent(inout) :: tpfile

  integer              :: ierr
  integer(kind=CDFINT) :: istatus
  character(len=1)     :: ydimred

  call print_msg(NVERB_DEBUG,'IO','IO_Are_dimension_reduced','called for '//trim(tpfile%cname))

  if ( tpfile%nmnhversion(1) < 5 .or. ( tpfile%nmnhversion(1) == 5 .and. tpfile%nmnhversion(2) < 5 ) ) then
    call Print_msg( NVERB_DEBUG, 'IO', 'IO_Are_dimension_reduced', 'ldimreduced set to false (created with MesoNH < 5.5.0)' )
    tpfile%ldimreduced = .false.
  else
    if ( isp == tpfile%nmaster_rank ) then
      istatus = NF90_GET_ATT( tpfile%nncid, NF90_GLOBAL, 'MNH_REDUCE_DIMENSIONS_IN_FILES', ydimred )
      if ( istatus == NF90_NOERR ) then
        if ( ydimred == '0' ) then
          tpfile%ldimreduced = .false.
        else if ( ydimred == '1' ) then
          tpfile%ldimreduced = .true.
        else
          call Print_msg( NVERB_ERROR, 'IO', 'IO_Are_dimension_reduced', &
                          'invalid value for MNH_REDUCE_DIMENSIONS_IN_FILES attribute' )
        end if
      else !attribute not found
        call Print_msg( NVERB_ERROR, 'IO', 'IO_Are_dimension_reduced', 'MNH_REDUCE_DIMENSIONS_IN_FILES attribute not found' )
      end if
    end if

    call MPI_BCAST( tpfile%ldimreduced, 1, MNHLOG_MPI, tpfile%nmaster_rank - 1, tpfile%nmpicomm, ierr )
  end if

end subroutine IO_Are_dimension_reduced


subroutine IO_Check_precision_loss_nc4( tpfile )
  use modd_io, only: lio_allow_reduced_precision_backup

  type(tfiledata), intent(in) :: tpfile

  character(len=:), allocatable :: yatt
  integer                       :: ierrlvl
  integer(kind=CDFINT)          :: ilen, istatus
  integer, dimension(3)         :: imnhversion

  call Print_msg( NVERB_DEBUG, 'IO', 'IO_Check_precision_loss_nc4', 'called for ' // trim(tpfile%cname) )

  imnhversion = tpfile%nmnhversion
  if ( imnhversion(1)<5                           .OR. &
      (imnhversion(1)==5 .AND. imnhversion(2)<4)       ) then
    call Print_msg( NVERB_DEBUG, 'IO', 'IO_Check_precision_loss_nc4', &
                    'file ' // trim(tpfile%cname) // ' is too old (before MNH 5.4.0) to check if precision loss', olocal = .true. )
    return
  end if

  if ( lio_allow_reduced_precision_backup ) then
    ierrlvl = NVERB_WARNING
  else
    ierrlvl = NVERB_ERROR
  end if

  ! Check MNH_INT attribute
  istatus = NF90_INQUIRE_ATTRIBUTE( tpfile%nncid, NF90_GLOBAL, 'MNH_INT', len = ilen )
  if ( istatus /= NF90_NOERR ) then
    call Print_msg( NVERB_ERROR, 'IO', 'IO_Check_precision_loss_nc4', &
                    'MNH_INT attribute not found in file ' // trim(tpfile%cname), olocal = .true. )
  else
    allocate( character(len=ilen) :: yatt )
    istatus = NF90_GET_ATT( tpfile%nncid, NF90_GLOBAL, 'MNH_INT', yatt )
    if ( istatus /= NF90_NOERR ) then
      call Print_msg( NVERB_WARNING, 'IO', 'IO_Check_precision_loss_nc4', &
                     'MNH_INT attribute not found in file ' // trim(tpfile%cname), olocal = .true. )
    else
#if ( MNH_INT == 4 )
      if ( yatt == '4' ) then
        !Nothing to do
      else if ( yatt == '8' ) then
        call Print_msg( NVERB_WARNING, 'IO', 'IO_Check_precision_loss_nc4', &
                        'integer precision is higher in file ' // trim(tpfile%cname) // ' than in Meso-NH', olocal = .true. )
#elif ( MNH_INT == 8 )
      if ( yatt == '4' ) then
        call Print_msg( ierrlvl, 'IO', 'IO_Check_precision_loss_nc4', &
                        'integer precision is lower in file ' // trim(tpfile%cname) // ' than in Meso-NH', olocal = .true. )
      else if ( yatt == '8' ) then
        !Nothing to do
#else
#error "Invalid MNH_INT"
#endif
      else
        call Print_msg( NVERB_ERROR, 'IO', 'IO_Check_precision_loss_nc4', &
                        'unknown value for MNH_INT (' // trim(yatt) // ') in file ' // trim(tpfile%cname), olocal = .true. )
      end if
    end if
    deallocate( yatt )
  end if

  ! Check MNH_REAL attribute
  istatus = NF90_INQUIRE_ATTRIBUTE( tpfile%nncid, NF90_GLOBAL, 'MNH_REAL', len = ilen )
  if ( istatus /= NF90_NOERR ) then
    call Print_msg( NVERB_ERROR, 'IO', 'IO_Check_precision_loss_nc4', &
                    'MNH_REAL attribute not found in file ' // trim(tpfile%cname), olocal = .true. )
  else
    allocate( character(len=ilen) :: yatt )
    istatus = NF90_GET_ATT( tpfile%nncid, NF90_GLOBAL, 'MNH_REAL', yatt )
    if ( istatus /= NF90_NOERR ) then
      call Print_msg( NVERB_WARNING, 'IO', 'IO_Check_precision_loss_nc4', &
                     'MNH_REAL attribute not found in file ' // trim(tpfile%cname), olocal = .true. )
    else
#if ( MNH_REAL == 4 )
      if ( yatt == '4' ) then
        !Nothing to do
      else if ( yatt == '8' ) then
        call Print_msg( NVERB_WARNING, 'IO', 'IO_Check_precision_loss_nc4', &
                        'float precision is higher in file ' // trim(tpfile%cname) // ' than in Meso-NH', olocal = .true. )
#elif ( MNH_REAL == 8 )
      if ( yatt == '4' ) then
        call Print_msg( ierrlvl, 'IO', 'IO_Check_precision_loss_nc4', &
                        'float precision is lower in file ' // trim(tpfile%cname) // ' than in Meso-NH', olocal = .true. )
      else if ( yatt == '8' ) then
        !Nothing to do
#else
#error "Invalid MNH_REAL"
#endif
      else
        call Print_msg( NVERB_ERROR, 'IO', 'IO_Check_precision_loss_nc4', &
                        'unknown value for MNH_REAL (' // trim(yatt) // ') in file ' // trim(tpfile%cname), olocal = .true. )
      end if
    end if
    deallocate( yatt )
  end if

  imnhversion = tpfile%nmnhversion
  if ( imnhversion(1)<5                                                 .OR. &
      (imnhversion(1)==5 .AND. imnhversion(2)<7)                        .OR. &
      (imnhversion(1)==5 .AND. imnhversion(2)==7 .AND. imnhversion(3)<1)     ) then
    call Print_msg( NVERB_DEBUG, 'IO', 'IO_Check_precision_loss_nc4', &
                    'file ' // trim(tpfile%cname) // ' is too old (before MNH 5.7.1) to check fully loss of precision' )
    return
  end if

  ! Check MNH_REDUCE_FLOAT_PRECISION attribute
  istatus = NF90_INQUIRE_ATTRIBUTE( tpfile%nncid, NF90_GLOBAL, 'MNH_REDUCE_FLOAT_PRECISION', len = ilen )
  if ( istatus /= NF90_NOERR ) then
    call Print_msg( NVERB_ERROR, 'IO', 'IO_Check_precision_loss_nc4', &
                    'MNH_REDUCE_FLOAT_PRECISION attribute not found in file ' // trim(tpfile%cname), olocal = .true. )
  else
    allocate( character(len=ilen) :: yatt )
    istatus = NF90_GET_ATT( tpfile%nncid, NF90_GLOBAL, 'MNH_REDUCE_FLOAT_PRECISION', yatt )
    if ( istatus /= NF90_NOERR ) then
      call Print_msg( NVERB_WARNING, 'IO', 'IO_Check_precision_loss_nc4', &
                     'MNH_REDUCE_FLOAT_PRECISION attribute not found in file ' // trim(tpfile%cname), olocal = .true. )
    else
      if ( yatt == '0' ) then
        !Nothing to do
      else if ( yatt == '1' ) then
        write( cmnhmsg(1), '( "reduced float precision in file ", A )' )  trim(tpfile%cname)
        if ( .not. lio_allow_reduced_precision_backup ) &
          cmnhmsg(2) = 'read can be forced by setting LIO_ALLOW_REDUCED_PRECISION_BACKUP=T in NAM_CONFIO'
        call Print_msg( ierrlvl, 'IO', 'IO_Check_precision_loss_nc4', olocal = .true. )
      else
        call Print_msg( NVERB_ERROR, 'IO', 'IO_Check_precision_loss_nc4',                                                     &
                        'unknown value for MNH_REDUCE_FLOAT_PRECISION (' // trim(yatt) // ') in file ' // trim(tpfile%cname), &
                         olocal = .true. )
      end if
    end if
    deallocate( yatt )
  end if

  ! Check MNH_COMPRESS_LOSSY attribute
  istatus = NF90_INQUIRE_ATTRIBUTE( tpfile%nncid, NF90_GLOBAL, 'MNH_COMPRESS_LOSSY', len = ilen )
  if ( istatus /= NF90_NOERR ) then
    call Print_msg( NVERB_ERROR, 'IO', 'IO_Check_precision_loss_nc4', &
                    'MNH_COMPRESS_LOSSY attribute not found in file ' // trim(tpfile%cname), olocal = .true. )
  else
    allocate( character(len=ilen) :: yatt )
    istatus = NF90_GET_ATT( tpfile%nncid, NF90_GLOBAL, 'MNH_COMPRESS_LOSSY', yatt )
    if ( istatus /= NF90_NOERR ) then
      call Print_msg( NVERB_WARNING, 'IO', 'IO_Check_precision_loss_nc4', &
                     'MNH_COMPRESS_LOSSY attribute not found in file ' // trim(tpfile%cname), olocal = .true. )
    else
      if ( yatt == '0' ) then
        !Nothing to do
      else if ( yatt == '1' ) then
        write( cmnhmsg(1), '( "lossy compression in file ", A )' )  trim(tpfile%cname)
        if ( .not. lio_allow_reduced_precision_backup ) &
          cmnhmsg(2) = 'read can be forced by setting LIO_ALLOW_REDUCED_PRECISION_BACKUP=T in NAM_CONFIO'
        call Print_msg( ierrlvl, 'IO', 'IO_Check_precision_loss_nc4', olocal = .true. )
      else
        call Print_msg( NVERB_ERROR, 'IO', 'IO_Check_precision_loss_nc4',                                                     &
                        'unknown value for MNH_COMPRESS_LOSSY (' // trim(yatt) // ') in file ' // trim(tpfile%cname), &
                         olocal = .true. )
      end if
    end if
    deallocate( yatt )
  end if
end subroutine IO_Check_precision_loss_nc4


subroutine IO_Cleanly_closed_check_nc4(tpfile)
  type(tfiledata), intent(in) :: tpfile

  character(len=:), allocatable :: yclean
  integer(kind=CDFINT)          :: ilen, istatus
  integer, dimension(3)         :: imnhversion

  call print_msg(NVERB_DEBUG,'IO','IO_Cleanly_closed_check_nc4','called for '//trim(tpfile%cname))

  imnhversion = tpfile%nmnhversion
  if ( imnhversion(1)<5                                                 .OR. &
      (imnhversion(1)==5 .AND. imnhversion(2)<4)                        .OR. &
      (imnhversion(1)==5 .AND. imnhversion(2)==4 .AND. imnhversion(3)<2)     ) then
    call print_msg(NVERB_DEBUG,'IO','IO_Cleanly_closed_check_nc4', &
                   'file '//trim(tpfile%cname)//' is too old (before MNH 5.4.2) to check if cleanly closed')
    return
  end if

  istatus = NF90_INQUIRE_ATTRIBUTE(tpfile%nncid, NF90_GLOBAL, 'MNH_cleanly_closed', len = ilen)
  if (istatus /= NF90_NOERR) then
    call print_msg(NVERB_ERROR,'IO','IO_Cleanly_closed_check_nc4', &
                   'MNH_cleanly_closed attribute not found in file '//trim(tpfile%cname))
  else
    allocate( character(len=ilen) :: yclean )
    istatus = NF90_GET_ATT(tpfile%nncid, NF90_GLOBAL, 'MNH_cleanly_closed', yclean)
    if (istatus /= NF90_NOERR) then
      call print_msg(NVERB_WARNING,'IO','IO_Cleanly_closed_check_nc4', &
                    'MNH_cleanly_closed attribute not found in file '//trim(tpfile%cname))
    else
      if (yclean == 'yes') then
        call print_msg(NVERB_DEBUG,'IO','IO_Cleanly_closed_check_nc4', &
                      'file '//trim(tpfile%cname)//' was cleanly closed before opening')
      else if (yclean == 'no') then
        call print_msg(NVERB_ERROR,'IO','IO_Cleanly_closed_check_nc4', &
                      'file '//trim(tpfile%cname)//' was not cleanly closed before opening')
      else
        call print_msg(NVERB_ERROR,'IO','IO_Cleanly_closed_check_nc4', &
                      'invalid MNH_cleanly_closed attribute for file '//trim(tpfile%cname))
      end if
    end if
  end if
end subroutine IO_Cleanly_closed_check_nc4


subroutine IO_Cleanly_closed_set_nc4(tpfile)
  type(tfiledata), intent(in) :: tpfile

  integer(kind=CDFINT) :: istatus

  call print_msg(NVERB_DEBUG,'IO','IO_Cleanly_closed_set_nc4','called for '//trim(tpfile%cname))

  istatus = NF90_PUT_ATT(tpfile%nncid, NF90_GLOBAL, 'MNH_cleanly_closed', 'yes')
  if (istatus /= NF90_NOERR) call IO_Err_handle_nc4(istatus,'IO_Cleanly_closed_set_nc4','NF90_PUT_ATT','MNH_cleanly_closed')
end subroutine IO_Cleanly_closed_set_nc4


subroutine IO_Not_cleanly_closed_set_nc4(tpfile)
  type(tfiledata), intent(in) :: tpfile

  integer(kind=CDFINT) :: istatus

  call print_msg(NVERB_DEBUG,'IO','IO_Not_cleanly_closed_set_nc4','called for '//trim(tpfile%cname))

  istatus = NF90_PUT_ATT(tpfile%nncid, NF90_GLOBAL, 'MNH_cleanly_closed', 'no')
  if (istatus /= NF90_NOERR) call IO_Err_handle_nc4(istatus,'IO_Not_cleanly_closed_set_nc4','NF90_PUT_ATT','MNH_cleanly_closed')
end subroutine IO_Not_cleanly_closed_set_nc4

end module mode_io_file_nc4
#else
!
! External dummy subroutines
!
subroutine IO_File_create_nc4(a, b)
use mode_msg
integer :: a, b
CALL PRINT_MSG(NVERB_ERROR,'IO','IO_File_create_nc4','empty call. Compile with -DMNH_IOCDF4 flag to enable NetCDF')
end subroutine IO_File_create_nc4
!
subroutine IO_File_close_nc4(a)
use mode_msg
integer :: a
CALL PRINT_MSG(NVERB_ERROR,'IO','IO_File_close_nc4','empty call. Compile with -DMNH_IOCDF4 flag to enable NetCDF')
end subroutine IO_File_close_nc4
!
subroutine IO_File_open_nc4(a)
use mode_msg
integer :: a
CALL PRINT_MSG(NVERB_ERROR,'IO','IO_File_open_nc4','empty call. Compile with -DMNH_IOCDF4 flag to enable NetCDF')
end subroutine IO_File_open_nc4
!
#endif
