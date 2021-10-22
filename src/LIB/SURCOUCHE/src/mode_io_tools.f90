!MNH_LIC Copyright 1994-2021 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
! Modifications:
!  P. Wautelet 13/12/2018: extracted from mode_io.f90
!  P. Wautelet 14/12/2018: added IO_Filename_construct
!  P. Wautelet 05/03/2019: rename IO subroutines and modules
!  P. Wautelet 05/09/2019: IO_Mnhversion_get: Z-split files: to prevent serialization between files,
!                          nmnhversion is taken from the main file
!-----------------------------------------------------------------
module mode_io_tools

use modd_io, only: tfiledata

implicit none

private

public :: IO_Level2filenumber_get, IO_Rank_master_get, IO_Filename_construct

contains

FUNCTION IO_Level2filenumber_get(k,nb_proc_io)
  !
  ! return the file number where to write the K level of data
  !
  IMPLICIT NONE
  INTEGER :: k,nb_proc_io
  INTEGER :: IO_Level2filenumber_get

  IO_Level2filenumber_get = MOD ((k-1) , nb_proc_io )

END FUNCTION IO_Level2filenumber_get

FUNCTION IO_Rank_master_get(IFILE,nb_proc,nb_proc_io,offset_rank)
  !
  ! return the proc number which must write the 'IFILE' file
  !
  IMPLICIT NONE
  INTEGER,           INTENT(IN) :: IFILE, nb_proc, nb_proc_io
  INTEGER, OPTIONAL, INTENT(IN) :: offset_rank

  INTEGER                       :: IO_Rank_master_get

  INTEGER                       :: ipas, irest

  ipas  =        nb_proc / nb_proc_io
  irest =  MOD ( nb_proc , nb_proc_io )

  IF  (ipas /= 0 ) THEN
     IO_Rank_master_get=ipas * IFILE + MIN(IFILE , irest )
  ELSE
     IO_Rank_master_get=MOD(IFILE , nb_proc )
  ENDIF

  !
  ! optional rank to shift for read test
  !
  IF (PRESENT(offset_rank)) THEN
     IF ( offset_rank .GT.0 ) IO_Rank_master_get=MOD(IO_Rank_master_get+offset_rank,nb_proc)
     IF ( offset_rank .LT.0 ) IO_Rank_master_get=MOD(nb_proc-IO_Rank_master_get+offset_rank,nb_proc)
  ENDIF

END FUNCTION IO_Rank_master_get


subroutine IO_Filename_construct(tpfile,hfilem)
  type(tfiledata),               intent(inout) :: tpfile
  character(len=:), allocatable, intent(out)   :: hfilem

  if (allocated(tpfile%cdirname)) then
    if(len_trim(tpfile%cdirname)>0) then
      hfilem = trim(tpfile%cdirname)//'/'//trim(tpfile%cname)
    else
      hfilem = trim(tpfile%cname)
    end if
  else
    hfilem = trim(tpfile%cname)
  end if

end subroutine IO_Filename_construct

end module mode_io_tools



module mode_io_tools_mnhversion

use modd_io, only: tfiledata

use mode_msg

implicit none

private

public :: IO_Mnhversion_get, IO_Mnhversion_set

contains

subroutine IO_Mnhversion_get(tpfile)
!Compare MNHVERSION of file with current version and store it in file metadata
  use modd_conf,          only: nmnhversion
  use modd_field,         only: tfielddata, TYPEINT

  use mode_io_field_read, only: IO_Field_read

  type(tfiledata), intent(inout) :: tpfile

  character(len=12)       :: ymnhversion_file,ymnhversion_curr
  integer :: imasdev,ibugfix
  integer :: iresp
  integer,dimension(3)    :: imnhversion
  type(tfielddata)        :: tzfield

  call print_msg(NVERB_DEBUG,'IO','IO_Mnhversion_get','called for '//trim(tpfile%cname))

  if ( trim(tpfile%cmode) /= 'READ' ) &
    call print_msg(NVERB_FATAL,'IO','IO_Mnhversion_get',trim(tpfile%cname)// 'not opened in read mode')

  if ( .not. associated( tpfile%tmainfile ) ) then
    imnhversion(:) = 0
    !use tzfield because tfieldlist could be not initialised
    tzfield = tfielddata(            &
      cmnhname   = 'MNHVERSION',     &
      cstdname   = '',               &
      clongname  = 'MesoNH version', &
      cunits     = '',               &
      cdir       = '--',             &
      ccomment   = '',               &
      ngrid      = 0,                &
      ntype      = TYPEINT,          &
      ndims      = 1,                &
      ltimedep   = .false.           )
    call IO_Field_read(tpfile,tzfield,imnhversion,iresp)
    if (iresp/=0) then
      tzfield%cmnhname   = 'MASDEV'
      tzfield%clongname  = 'MesoNH version (without bugfix)'
      tzfield%ndims      = 0
      call IO_Field_read(tpfile,tzfield,imasdev,iresp)
      if (iresp/=0) then
        call print_msg(NVERB_WARNING,'IO','IO_Mnhversion_get','unknown MASDEV version for '//trim(tpfile%cname))
      else
        if (imasdev<100) then
          imnhversion(1)=imasdev/10
          imnhversion(2)=mod(imasdev,10)
        else !for example for mnh 4.10
          imnhversion(1)=imasdev/100
          imnhversion(2)=mod(imasdev,100)
        end if
      end if
      !
      tzfield%cmnhname   = 'BUGFIX'
      tzfield%clongname  = 'MesoNH bugfix number'
      call IO_Field_read(tpfile,tzfield,ibugfix,iresp)
      if (iresp/=0) then
        call print_msg(NVERB_WARNING,'IO','IO_Mnhversion_get','unknown BUGFIX version for '//trim(tpfile%cname))
      else
        imnhversion(3)=ibugfix
      end if
    end if
    !
    write(ymnhversion_file,"( I0,'.',I0,'.',I0 )" ) imnhversion(1),imnhversion(2),imnhversion(3)
    write(ymnhversion_curr,"( I0,'.',I0,'.',I0 )" ) nmnhversion(1),nmnhversion(2),nmnhversion(3)
    !
    if ( imnhversion(1)==0 .and. imnhversion(2)==0 .and. imnhversion(3)==0 ) then
      call print_msg(NVERB_WARNING,'IO','IO_Mnhversion_get','file '//trim(tpfile%cname)//&
                    ' was written with an unknown version of MesoNH')
    else if (  imnhversion(1)< nmnhversion(1) .or. &
              (imnhversion(1)==nmnhversion(1) .and. imnhversion(2)< nmnhversion(2)) .or. &
              (imnhversion(1)==nmnhversion(1) .and. imnhversion(2)==nmnhversion(2) .and. imnhversion(3)<nmnhversion(3)) ) then
      call print_msg(NVERB_WARNING,'IO','IO_Mnhversion_get','file '//trim(tpfile%cname)//&
                      ' was written with an older version of MesoNH ('//trim(ymnhversion_file)//&
                      ' instead of '//trim(ymnhversion_curr)//')')
    else if (  imnhversion(1)> nmnhversion(1) .or. &
              (imnhversion(1)==nmnhversion(1) .and. imnhversion(2)> nmnhversion(2)) .or. &
              (imnhversion(1)==nmnhversion(1) .and. imnhversion(2)==nmnhversion(2) .and. imnhversion(3)>nmnhversion(3)) ) then
      call print_msg(NVERB_WARNING,'IO','IO_Mnhversion_get','file '//trim(tpfile%cname)//&
                    ' was written with a more recent version of MesoNH ('//trim(ymnhversion_file)//&
                    ' instead of '//trim(ymnhversion_curr)//')')
    else
      call print_msg(NVERB_DEBUG,'IO','IO_Mnhversion_get','file '//trim(tpfile%cname)//&
                      ' was written with the same version of MesoNH ('//trim(ymnhversion_curr)//')')
    end if
    !
    tpfile%nmnhversion(:) = imnhversion(:)
  else ! associated( tpfile%tmainfile )
    if ( .not. tpfile%tmainfile%lopened ) &
      call Print_msg( NVERB_FATAL, 'IO', 'IO_Mnhversion_get', 'tmainfile should be opened' )
    tpfile%nmnhversion(:) = tpfile%tmainfile%nmnhversion(:)
  end if
end subroutine IO_Mnhversion_get


subroutine IO_Mnhversion_set(tpfile)
  use modd_conf,  only: nmnhversion

  type(tfiledata), intent(inout) :: tpfile

  call print_msg(NVERB_DEBUG,'IO','IO_Mnhversion_set','called for '//trim(tpfile%cname))

  if ( trim(tpfile%cmode) /= 'WRITE' ) &
    call print_msg(NVERB_FATAL,'IO','IO_Mnhversion_set',trim(tpfile%cname)// 'not opened in write mode')

  tpfile%nmnhversion(:) = nmnhversion(:)
end subroutine IO_Mnhversion_set

end module mode_io_tools_mnhversion
