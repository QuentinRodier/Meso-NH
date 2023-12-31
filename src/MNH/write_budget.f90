!MNH_LIC Copyright 1995-2022 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
! Author:
!  J. Nicolau (Meteo-France) 27/02/1995
! Modifications:
!  J. Stein    09/09/1996: add the writings in the diachronic file
!  J.-P. Pinty 18/12/1996: clarify the coding
!  J.-P. Pinty 18/03/1997: correction for the SVx
!  V. Gouget M. Chong J.-P. Lafore 10/02/1998: add the BURHODJ, TSTEP and BULEN and writes in physical units
!  V. Ducrocq  07/06/1999: //
!  N. Asencio  18/06/1999: // budget with MASK case
!                         delete ZTORE arrays no longer used, so delete
!                         KIU,KJU,KKU arguments
!                         the mask is written once with a FMWRIT call outside
!                         write_diachro: its name is MASK_(value of NBUTSHIFT).MASK
!                         MENU_DIACHRO must be called after FMWRIT to be read in
!                         read_diachro.
!                         NBUTSHIFT is incremented at the beginning of the routine
!                         The dimensions of the XBUR.. arrays are : first one
!                         is the dimension along K, second one is the time, the
!                         third one is the number of the masks.
!  G. Tanguy      10/2009: add ILENCH=LEN(YCOMMENT) after change of YCOMMENT
!  J. Escobar  24/03/2014: misplaced deallocate in RSV budget
!  C. Lac      11/09/2015: orrection due to FIT temporal scheme
!  P. Wautelet 28/03/2018: Replace TEMPORAL_DIST by DATETIME_DISTANCE
!  P. Wautelet 05/2016-04/2018: new data structures and calls for I/O
!  P. Wautelet 13/09/2019: budget: simplify and modernize date/time management
!  P. Wautelet 14/10/2019: complete restructuration and deduplication of code
!  P. Wautelet 10/03/2020: use the new data structures and subroutines for budgets
!  P. Wautelet 09/10/2020: Write_diachro: use new datatype tpfields
!  P. Wautelet 08/12/2020: budgets: merge budgets terms with different nbutshift
!-----------------------------------------------------------------

!#######################
module mode_write_budget
!#######################

use mode_msg

implicit none

private

public :: Write_budget

character(len=*), parameter :: CMASK_VARNAME = 'MASKS'

contains

!#########################################################
subroutine Write_budget( tpdiafile, tpdtcur, ptstep, ksv )
!#########################################################
!
!!****  *WRITE_BUDGET* - routine to write a budget file
!!                           
!!
!!    PURPOSE
!!    -------
!        The purpose of this routine is to write an initial LFIFM File 
!     of name YFILEDIA//'.lfi' with the FM routines. This routine is 
!     temporary because the budget terms had to be stored in the diachronic
!     MesoNH-files, not yet developped. 
!
!!**  METHOD
!!    ------
!!      The data are written in the LFIFM file :
!!        - dimensions
!!        - budget arrays
!!        - tracer array in mask case
!!
!!      The localization on the model grid is also indicated :
!!
!!        IGRID = 1 for mass grid point
!!        IGRID = 2 for U grid point
!!        IGRID = 3 for V grid point
!!        IGRID = 4 for w grid point
!!        IGRID = 0 for meaningless case
!!
!!
!!
!!    EXTERNAL
!!    --------
!!       NONE
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!       Module MODD_BUDGET
!!
!!
!!    REFERENCE
!!    ---------
!!      Book2 of MESO-NH documentation (routine WRITE_BUDGET)
!!
!-------------------------------------------------------------------------------

  use modd_budget,         only: cbutype, nbumask, nbusurf, nbutshift, nbustep, nbusubwrite, xbulen,                              &
                                 lbu_icp, lbu_jcp,                                                                                &
                                 lbu_ru, lbu_rv, lbu_rw, lbu_rth, lbu_rtke, lbu_rrv, lbu_rrc, lbu_rrr,                            &
                                 lbu_rri, lbu_rrs, lbu_rrg, lbu_rrh, lbu_rsv,                                                     &
                                 NBUDGET_RHO, NBUDGET_U, NBUDGET_V, NBUDGET_W, NBUDGET_TH, NBUDGET_TKE,                           &
                                 NBUDGET_RV, NBUDGET_RC, NBUDGET_RR, NBUDGET_RI, NBUDGET_RS, NBUDGET_RG, NBUDGET_RH, NBUDGET_SV1, &
                                 tbudgets, tburhodj
  use modd_field,          only: NMNHDIM_ONE, NMNHDIM_NI, NMNHDIM_NJ,                              &
                                 NMNHDIM_BUDGET_TIME, NMNHDIM_BUDGET_MASK_NBUMASK, NMNHDIM_UNUSED, &
                                 tfieldmetadata, TYPEINT, TYPEREAL
  use modd_io,             only: tfiledata
  use modd_lunit_n,        only: tluout
  use modd_parameters,     only: NCOMMENTLGTMAX, NMNHNAMELGTMAX
  use modd_type_date,      only: date_time

  use mode_datetime,       only: datetime_distance
  use mode_io_field_write, only: IO_Field_create, IO_Field_write
#ifdef MNH_IOLFI
  use mode_menu_diachro,   only: Menu_diachro
#endif
  use mode_msg
  use mode_time,           only: tdtexp

  implicit none

  type(tfiledata), intent(in) :: tpdiafile    ! file to write
  type(date_time), intent(in) :: tpdtcur      ! current date and time
  real,            intent(in) :: ptstep       ! time step
  integer,         intent(in) :: ksv          ! number of scalar variables

  character(len=NCOMMENTLGTMAX)                        :: ycomment
  character(len=NMNHNAMELGTMAX)                        :: ymnhname
  character(len=NMNHNAMELGTMAX)                        :: yrecfm        ! name of the article to be written
  integer                                              :: jt, jmask
  integer                                              :: jsv           ! loop index over the ksv svx
  logical                                              :: gnocompress   ! true: no compression along x and y direction (cart option)
  real,            dimension(:),           allocatable :: zworktemp
  real,            dimension(:,:,:,:,:,:), allocatable :: zrhodjn, zworkmask
  type(date_time), dimension(:),           allocatable :: tzdates
  type(tfieldmetadata) :: tzfield
  type(tfiledata)      :: tzfile
  !
  !-------------------------------------------------------------------------------
  !
  call Print_msg( NVERB_DEBUG, 'BUD', 'Write_budget', 'called' )

  gnocompress = .true.
  !
  !* Write TSTEP and BULEN
  !  ---------------------
  !
  TZFIELD = TFIELDMETADATA(   &
    CMNHNAME   = 'TSTEP',     &
    CSTDNAME   = '',          &
    CLONGNAME  = 'TSTEP',     &
    CUNITS     = 's',         &
    CDIR       = '--',        &
    CCOMMENT   = 'Time step', &
    NGRID      = 0,           &
    NTYPE      = TYPEREAL,    &
    NDIMS      = 0,           &
    LTIMEDEP   = .FALSE.      )
  CALL IO_Field_write(TPDIAFILE,TZFIELD,PTSTEP)
  !
  TZFIELD = TFIELDMETADATA(   &
    CMNHNAME   = 'BULEN',     &
    CSTDNAME   = '',          &
    CLONGNAME  = 'BULEN',     &
    CUNITS     = 's',         &
    CDIR       = '--',        &
    CCOMMENT   = 'Length of the budget temporal average', &
    NGRID      = 0,           &
    NTYPE      = TYPEREAL,    &
    NDIMS      = 0,           &
    LTIMEDEP   = .FALSE.      )
  CALL IO_Field_write(TPDIAFILE,TZFIELD,XBULEN)
  !
  ! Initialize NBUTSHIFT
  NBUTSHIFT = NBUTSHIFT+1
  !
  !
  SELECT CASE (CBUTYPE)
  !
  !-------------------------------------------------------------------------------
  !
  !* 2.     'CART' CASE
  !         -----------
  !
    CASE('CART','SKIP')
      GNOCOMPRESS=(.NOT.LBU_ICP .AND. .NOT.LBU_JCP)
  !
  !* 2.1    Initialization
  !
      ALLOCATE( ZWORKTEMP(1) )
      !Note: tzdates are used only in LFI files; for netCDF files, dates are written in the coordinates
      allocate( tzdates(1) )
  !
      !Compute time at the middle of the temporally-averaged budget timestep
      !This time is computed from the beginning of the experiment
      CALL DATETIME_DISTANCE(TDTEXP,TPDTCUR,ZWORKTEMP(1))
  !
      ZWORKTEMP(1)=ZWORKTEMP(1)+(1.-NBUSTEP*0.5)*PTSTEP
  !
      tzdates(1)%nyear  = tdtexp%nyear
      tzdates(1)%nmonth = tdtexp%nmonth
      tzdates(1)%nday   = tdtexp%nday
      tzdates(1)%xtime  = tdtexp%xtime + zworktemp(1)

      DEALLOCATE ( ZWORKTEMP )
  !
  !-------------------------------------------------------------------------------
  !
  !* 3.     'MASK' CASE
  !         -----------
  !
    CASE('MASK')
      ALLOCATE(ZWORKTEMP(nbusubwrite))
      !Note: tzdates are used only in LFI files; for netCDF files, dates are written in the coordinates
      allocate( tzdates(nbusubwrite) )
  !
      CALL DATETIME_DISTANCE(TDTEXP,TPDTCUR,ZWORKTEMP(nbusubwrite))
  !
      ZWORKTEMP(nbusubwrite)=ZWORKTEMP(nbusubwrite)+(1.-NBUSTEP*0.5)*PTSTEP
  !
      tzdates(nbusubwrite)%nyear  = tdtexp%nyear
      tzdates(nbusubwrite)%nmonth = tdtexp%nmonth
      tzdates(nbusubwrite)%nday   = tdtexp%nday
      tzdates(nbusubwrite)%xtime  = tdtexp%xtime + zworktemp(nbusubwrite)
      DO JT=1,nbusubwrite-1
        ZWORKTEMP(JT) = ZWORKTEMP(nbusubwrite)-NBUSTEP*PTSTEP*(nbusubwrite-JT)
        tzdates(jt)%nyear  = tdtexp%nyear
        tzdates(jt)%nmonth = tdtexp%nmonth
        tzdates(jt)%nday   = tdtexp%nday
        tzdates(jt)%xtime  = tdtexp%xtime + zworktemp(jt)
      END DO

      DEALLOCATE( ZWORKTEMP )
  !
  !*     3.1    storage of the masks  array
  !
#ifdef MNH_IOLFI
      if ( Trim( tpdiafile%cformat ) == 'LFI' .or. Trim( tpdiafile%cformat ) == 'LFICDF4' ) then
        Allocate( zworkmask(Size( nbusurf, 1 ), Size( nbusurf, 2 ), 1, nbusubwrite, nbumask,1) )
        ! local array
        do jmask = 1, nbumask
          do jt = 1, nbusubwrite
            zworkmask(:, :, 1, jt, jmask, 1) = Real( nbusurf(:, :, jmask, jt), kind = Kind( zworkmask ) )
          end do
        end do

        tzfile = tpdiafile
        tzfile%cformat = 'LFI'

        Write( ymnhname, fmt = "( 'MASK_', i4.4, '.MASK' )" ) nbutshift
        Write( ycomment, fmt = "( 'X_Y_MASK', i4.4 )" ) nbutshift
        tzfield = tfieldmetadata(         &
          cmnhname   = Trim( ymnhname ),  &
          cstdname   = '',                &
          clongname  = Trim( ymnhname ),  &
          cunits     = '',                &
          cdir       = 'XY',              &
          ccomment   = Trim ( ycomment ), &
          ngrid      = 1,                 &
          ntype      = TYPEREAL,          &
          ndims      = 6,                 &
          ndimlist = [ NMNHDIM_NI, NMNHDIM_NJ, NMNHDIM_ONE, NMNHDIM_BUDGET_TIME, NMNHDIM_BUDGET_MASK_NBUMASK, NMNHDIM_ONE ], &
          ltimedep   = .FALSE.            )
        call IO_Field_write( tzfile, tzfield, zworkmask(:, :, :, :, :, :) )

        Write( yrecfm, fmt = "( 'MASK_', i4.4 )" ) nbutshift
        call Menu_diachro( tzfile, yrecfm )

        Deallocate( zworkmask )
      end if
#endif

      if ( Trim( tpdiafile%cformat ) == 'LFICDF4' .or. Trim( tpdiafile%cformat ) == 'NETCDF4' ) then
        tzfile = tpdiafile
        tzfile%cformat = 'NETCDF4'

        tzfield = tfieldmetadata(                &
          cmnhname   = CMASK_VARNAME,            &
          cstdname   = '',                       &
          clongname  = CMASK_VARNAME,            &
          cunits     = '1',                      &
          cdir       = 'XY',                     &
          ccomment   = 'Masks for budget areas', &
          ngrid      = 1,                        &
          ntype      = TYPEINT,                  &
          ndims      = 4,                        &
          ndimlist = [ NMNHDIM_NI, NMNHDIM_NJ, NMNHDIM_BUDGET_MASK_NBUMASK, NMNHDIM_BUDGET_TIME ], &
          ltimedep   = .false.                   ) !The time dependance is in the NMNHDIM_BUDGET_TIME dimension

        !Create the metadata of the field (has to be done only once)
        if ( nbutshift == 1 ) call IO_Field_create( tzfile, tzfield )

        !Write the data (partial write of the field with the given offset)
        call IO_Field_write( tzfile, tzfield, nbusurf(:,:,:,:), koffset= [ 0, 0, 0, ( nbutshift - 1 ) * nbusubwrite ] )

        if ( nbutshift == 1 ) call Menu_diachro( tzfile, CMASK_VARNAME )
      end if
  !
  END SELECT
  !
  if ( cbutype == 'CART' .or. cbutype == 'SKIP' .or. cbutype == 'MASK' ) then
  !
  !* Storage of the budgets array
  !
  !* RU budgets
  !
    IF (LBU_RU) THEN
      call Store_one_budget_rho( tpdiafile, tzdates, tbudgets(NBUDGET_U)%trhodj,   gnocompress, zrhodjn )
      call Store_one_budget    ( tpdiafile, tzdates, tbudgets(NBUDGET_U), zrhodjn, gnocompress, ptstep  )
    END IF
  !
  !* RV budgets
  !
    IF (LBU_RV) THEN
      call Store_one_budget_rho( tpdiafile, tzdates, tbudgets(NBUDGET_V)%trhodj,   gnocompress, zrhodjn )
      call Store_one_budget    ( tpdiafile, tzdates, tbudgets(NBUDGET_V), zrhodjn, gnocompress, ptstep  )
    END IF
  !
  !* RW budgets
  !
    IF (LBU_RW) THEN
      call Store_one_budget_rho( tpdiafile, tzdates, tbudgets(NBUDGET_W)%trhodj,   gnocompress, zrhodjn )
      call Store_one_budget    ( tpdiafile, tzdates, tbudgets(NBUDGET_W), zrhodjn, gnocompress, ptstep  )
    END IF
  !
  !* RHODJ storage for Scalars
  !
    IF (LBU_RTH .OR. LBU_RTKE .OR. LBU_RRV .OR. LBU_RRC .OR. LBU_RRR .OR. &
        LBU_RRI .OR. LBU_RRS  .OR. LBU_RRG .OR. LBU_RRH .OR. LBU_RSV      ) THEN
      if ( .not. associated( tburhodj ) ) call Print_msg( NVERB_FATAL, 'BUD', 'Write_budget', 'tburhodj not associated' )
      call Store_one_budget_rho( tpdiafile, tzdates, tburhodj, gnocompress, zrhodjn )
    ENDIF
  !
  !* RTH budget
  !
    IF (LBU_RTH) THEN
      call Store_one_budget( tpdiafile, tzdates, tbudgets(NBUDGET_TH), zrhodjn, gnocompress, ptstep  )
    END IF
  !
  !* RTKE budget
  !
    IF (LBU_RTKE) THEN
      call Store_one_budget( tpdiafile, tzdates, tbudgets(NBUDGET_TKE), zrhodjn, gnocompress, ptstep  )
    END IF
  !
  !* RRV budget
  !
    IF (LBU_RRV) THEN
      call Store_one_budget( tpdiafile, tzdates, tbudgets(NBUDGET_RV), zrhodjn, gnocompress, ptstep  )
    END IF
  !
  !* RRC budget
  !
    IF (LBU_RRC) THEN
      call Store_one_budget( tpdiafile, tzdates, tbudgets(NBUDGET_RC), zrhodjn, gnocompress, ptstep  )
    END IF
  !
  !* RRR budget
  !
    IF (LBU_RRR) THEN
      call Store_one_budget( tpdiafile, tzdates, tbudgets(NBUDGET_RR), zrhodjn, gnocompress, ptstep  )
    END IF
  !
  !* RRI budget
  !
    IF (LBU_RRI) THEN
      call Store_one_budget( tpdiafile, tzdates, tbudgets(NBUDGET_RI), zrhodjn, gnocompress, ptstep  )
    END IF
  !
  !* RRS budget
  !
    IF (LBU_RRS) THEN
      call Store_one_budget( tpdiafile, tzdates, tbudgets(NBUDGET_RS), zrhodjn, gnocompress, ptstep  )
    END IF
  !
  !* RRG budget
  !
    IF (LBU_RRG) THEN
      call Store_one_budget( tpdiafile, tzdates, tbudgets(NBUDGET_RG), zrhodjn, gnocompress, ptstep  )
    END IF
  !
  !* RRH budget
  !
    IF (LBU_RRH) THEN
      call Store_one_budget( tpdiafile, tzdates, tbudgets(NBUDGET_RH), zrhodjn, gnocompress, ptstep  )
    END IF
  !
  !* RSV budgets
  !
    IF (LBU_RSV) THEN
      do jsv = nbudget_sv1, nbudget_sv1 - 1 + ksv
        call Store_one_budget( tpdiafile, tzdates, tbudgets(jsv), zrhodjn, gnocompress, ptstep  )
      end do
    END IF
  end if

end subroutine Write_budget


subroutine Store_one_budget_rho( tpdiafile, tpdates, tprhodj, knocompress, prhodjn )
  use modd_budget,            only: cbutype,                                                                                     &
                                    lbu_icp, lbu_jcp, lbu_kcp,                                                                   &
                                    nbuil, nbuih, nbujl, nbujh, nbukl, nbukh,                                                    &
                                    nbuimax, nbuimax_ll, nbujmax, nbujmax_ll, nbukmax, nbutshift,                                &
                                    nbumask, nbusubwrite,                                                                        &
                                    NLVL_CATEGORY, NLVL_SUBCATEGORY, NLVL_GROUP, NLVL_SHAPE, NLVL_TIMEAVG, NLVL_NORM, NLVL_MASK, &
                                    tbudiachrometadata, tburhodata
  use modd_field,             only: NMNHDIM_BUDGET_CART_NI,    NMNHDIM_BUDGET_CART_NJ,   NMNHDIM_BUDGET_CART_NI_U, &
                                    NMNHDIM_BUDGET_CART_NJ_U,  NMNHDIM_BUDGET_CART_NI_V, NMNHDIM_BUDGET_CART_NJ_V, &
                                    NMNHDIM_BUDGET_CART_LEVEL, NMNHDIM_BUDGET_CART_LEVEL_W,                        &
                                    NMNHDIM_BUDGET_MASK_LEVEL, NMNHDIM_BUDGET_MASK_LEVEL_W,                        &
                                    NMNHDIM_BUDGET_MASK_NBUMASK, NMNHDIM_BUDGET_TIME,                              &
                                    NMNHDIM_UNUSED, NMNHDIM_UNKNOWN
  use modd_io,                only: tfiledata
  use modd_lunit_n,           only: tluout
  use modd_parameters,        only: XNEGUNDEF
  use modd_type_date,         only: date_time

  use mode_msg
  use mode_write_diachro,     only: Write_diachro

  use modi_end_cart_compress, only: End_cart_compress
  use modi_end_mask_compress, only: End_mask_compress

  implicit none

  type(tfiledata),                                      intent(in)  :: tpdiafile   ! file to write
  type(date_time), dimension(:),                        intent(in)  :: tpdates
  type(tburhodata),                                     intent(in)  :: tprhodj     ! rhodj datastructure
  logical,                                              intent(in)  :: knocompress ! compression for the cart option
  real,            dimension(:,:,:,:,:,:), allocatable, intent(out) :: prhodjn

  character(len=4)              :: ybutype
  type(tbudiachrometadata)      :: tzbudiachro
  type(tburhodata)              :: tzfield

  call Print_msg( NVERB_DEBUG, 'BUD', 'Store_one_budget_rho', 'called for '//trim( tprhodj%cmnhname ) )

  !if ( allocated( prhodjn ) ) deallocate( prhodjn ) !Not necessary: if intent(out) => automatically deallocated

  ! pburhodj storage
  select case ( cbutype )
    case( 'CART', 'SKIP' )
      !Set to CART for all processes even if has no data(='SKIP')
      !Necessary to do the call and the collective write later (if knocompress)
      ybutype = 'CART'
      if ( knocompress ) then
        allocate( prhodjn(nbuimax, nbujmax, nbukmax, 1, 1, 1) ) ! local budget of RHODJU
        prhodjn(:, :, :, 1, 1, 1) = tprhodj%xdata(:, :, :)
      else
        allocate( prhodjn(nbuimax_ll, nbujmax_ll, nbukmax, 1, 1, 1) ) ! global budget of RhodjU
        prhodjn(:,:,:,1,1,1)=End_cart_compress( tprhodj%xdata(:,:,:) )
      end if
    case('MASK')
      ybutype = 'MASK'
      allocate( prhodjn(1, 1, nbukmax, nbusubwrite, nbumask, 1) )
      prhodjn(1, 1, :, :, :, 1) = End_mask_compress( tprhodj%xdata(:, :, :) )
      where  ( prhodjn(1, 1, :, :, :, 1) <= 0. )
        prhodjn(1, 1, :, :, :, 1) = XNEGUNDEF
      end where

    case default
      call Print_msg( NVERB_ERROR, 'BUD', 'Store_one_budget_rho', 'unknown CBUTYPE' )
  end select

  !Copy all fields from tprhodj
  tzfield = tprhodj

  !Modify metadata coming from tprhodj%tgroups
  !ndims and ndimlist are adapted for Write_diachro
  if ( tzfield%ngrid < 1 .or. tzfield%ngrid > 4 ) &
    call Print_msg( NVERB_FATAL, 'BUD', 'Store_one_budget_rho', 'invalid grid' )

  if ( ybutype == 'CART' ) then
    if ( .not. lbu_icp ) then
      select case ( tzfield%ngrid )
        case ( 1, 4 )
          tzfield%ndimlist(1)  = NMNHDIM_BUDGET_CART_NI
        case ( 2 )
          tzfield%ndimlist(1)  = NMNHDIM_BUDGET_CART_NI_U
        case ( 3 )
          tzfield%ndimlist(1)  = NMNHDIM_BUDGET_CART_NI_V
      end select
    else
      tzfield%ndims = tzfield%ndims - 1
      tzfield%ndimlist(1)  = NMNHDIM_UNUSED
    end if

    if ( .not. lbu_jcp ) then
      select case ( tzfield%ngrid )
        case ( 1, 4 )
          tzfield%ndimlist(2)  = NMNHDIM_BUDGET_CART_NJ
        case ( 2 )
          tzfield%ndimlist(2)  = NMNHDIM_BUDGET_CART_NJ_U
        case ( 3 )
          tzfield%ndimlist(2)  = NMNHDIM_BUDGET_CART_NJ_V
      end select
    else
      tzfield%ndims = tzfield%ndims - 1
      tzfield%ndimlist(2)  = NMNHDIM_UNUSED
    end if

    if ( .not. lbu_kcp ) then
      select case ( tzfield%ngrid )
        case ( 1, 2, 3 )
          tzfield%ndimlist(3)  = NMNHDIM_BUDGET_CART_LEVEL
        case ( 4 )
          tzfield%ndimlist(3)  = NMNHDIM_BUDGET_CART_LEVEL_W
      end select
    else
      tzfield%ndims = tzfield%ndims - 1
      tzfield%ndimlist(3)  = NMNHDIM_UNUSED
    end if
    tzfield%ndimlist(4:) = NMNHDIM_UNUSED

  else if ( ybutype == 'MASK' ) then
    tzfield%ndimlist(1) = NMNHDIM_UNUSED
    tzfield%ndimlist(2) = NMNHDIM_UNUSED
    if ( .not. lbu_kcp ) then
      select case ( tzfield%ngrid )
        case ( 1, 2, 3 )
          tzfield%ndimlist(3)  = NMNHDIM_BUDGET_MASK_LEVEL
        case ( 4 )
          tzfield%ndimlist(3)  = NMNHDIM_BUDGET_MASK_LEVEL_W
      end select
    else
      tzfield%ndims = tzfield%ndims - 1
      tzfield%ndimlist(3) = NMNHDIM_UNUSED
    end if
    tzfield%ndimlist(4) = NMNHDIM_BUDGET_TIME
    tzfield%ndimlist(5) = NMNHDIM_BUDGET_MASK_NBUMASK
    tzfield%ndimlist(6) = NMNHDIM_UNUSED

  else
    tzfield%ndimlist(:) = NMNHDIM_UNKNOWN
  end if

  tzbudiachro%lleveluse(NLVL_CATEGORY)    = .true.
  tzbudiachro%clevels  (NLVL_CATEGORY)    = 'Budgets'
  tzbudiachro%ccomments(NLVL_CATEGORY)    = 'Level for the different budgets'

  tzbudiachro%lleveluse(NLVL_SUBCATEGORY) = .false.
  tzbudiachro%clevels  (NLVL_SUBCATEGORY) = ''
  tzbudiachro%ccomments(NLVL_SUBCATEGORY) = ''

  tzbudiachro%lleveluse(NLVL_GROUP)       = .true.
  tzbudiachro%clevels  (NLVL_GROUP)       = 'RhodJ'
  tzbudiachro%ccomments(NLVL_GROUP)       = 'mass of dry air contained in the mesh cells'

  tzbudiachro%lleveluse(NLVL_SHAPE)       = .false.
  if ( ybutype == 'CART' ) then
    tzbudiachro%clevels  (NLVL_SHAPE)     = 'Cartesian'
    tzbudiachro%ccomments(NLVL_SHAPE)     = 'cartesian domain'
  else
    tzbudiachro%clevels  (NLVL_SHAPE)     = 'Mask'
    tzbudiachro%ccomments(NLVL_SHAPE)     = 'masked domain'
  end if

  tzbudiachro%lleveluse(NLVL_TIMEAVG)     = .false.
  tzbudiachro%clevels  (NLVL_TIMEAVG)     = 'Time_averaged'
  tzbudiachro%ccomments(NLVL_TIMEAVG)     = 'Values are time averaged'

  tzbudiachro%lleveluse(NLVL_NORM)        = .false.
  tzbudiachro%clevels  (NLVL_NORM)        = 'Not_normalized'
  tzbudiachro%ccomments(NLVL_NORM)        = 'Values are not normalized'

  tzbudiachro%lleveluse(NLVL_MASK)        = .false.
  if ( ybutype == 'MASK' ) then
    tzbudiachro%clevels  (NLVL_MASK)      = CMASK_VARNAME
    tzbudiachro%ccomments(NLVL_MASK)      = ''
  else
    tzbudiachro%clevels  (NLVL_MASK)      = ''
    tzbudiachro%ccomments(NLVL_MASK)      = ''
  end if

  if ( ybutype == 'CART' ) then
    tzbudiachro%lmobile  = .false.
  else
    !Masks are updated at each timestep (therefore the studied domains change during execution)
    tzbudiachro%lmobile  = .true.
  end if
  tzbudiachro%licompress = lbu_icp
  tzbudiachro%ljcompress = lbu_jcp
  tzbudiachro%lkcompress = lbu_kcp
  tzbudiachro%ltcompress = .true. !Data is temporally averaged
  tzbudiachro%lnorm      = .false.
  !Boundaries in physical domain does not make sense here if 'MASK'
  !In that case, these values are not written in the netCDF files
  !But they are always written in the LFI files. They are kept (in the MASK case) for backward compatibility.
  tzbudiachro%nil        = nbuil
  tzbudiachro%nih        = nbuih
  tzbudiachro%njl        = nbujl
  tzbudiachro%njh        = nbujh
  tzbudiachro%nkl        = nbukl
  tzbudiachro%nkh        = nbukh

  call Write_diachro( tpdiafile, tzbudiachro, [ tzfield ], tpdates, prhodjn, osplit = .true. )

end subroutine Store_one_budget_rho


subroutine Store_one_budget( tpdiafile, tpdates, tpbudget, prhodjn, knocompress, ptstep )
  use modd_budget,            only: cbutype,                                                                                      &
                                    lbu_icp, lbu_jcp, lbu_kcp,                                                                    &
                                    nbuil, nbuih, nbujl, nbujh, nbukl, nbukh,                                                     &
                                    nbuimax, nbuimax_ll, nbujmax, nbujmax_ll, nbukmax, nbustep, nbutshift,                        &
                                    nbumask, nbusubwrite,                                                                         &
                                    NBUDGET_U, NBUDGET_V, NBUDGET_W, NBUDGET_TH, NBUDGET_TKE, NBUDGET_RV, NBUDGET_RC, NBUDGET_RR, &
                                    NBUDGET_RI, NBUDGET_RS, NBUDGET_RG, NBUDGET_RH, NBUDGET_SV1,                                  &
                                    NLVL_CATEGORY, NLVL_SUBCATEGORY, NLVL_GROUP, NLVL_SHAPE, NLVL_TIMEAVG, NLVL_NORM, NLVL_MASK,  &
                                    tbudgetdata, tbudiachrometadata, tbugroupdata
  use modd_field,             only: NMNHDIM_BUDGET_CART_NI,    NMNHDIM_BUDGET_CART_NJ,   NMNHDIM_BUDGET_CART_NI_U, &
                                    NMNHDIM_BUDGET_CART_NJ_U,  NMNHDIM_BUDGET_CART_NI_V, NMNHDIM_BUDGET_CART_NJ_V, &
                                    NMNHDIM_BUDGET_CART_LEVEL, NMNHDIM_BUDGET_CART_LEVEL_W,                        &
                                    NMNHDIM_BUDGET_MASK_LEVEL, NMNHDIM_BUDGET_MASK_LEVEL_W,                        &
                                    NMNHDIM_BUDGET_MASK_NBUMASK, NMNHDIM_BUDGET_TIME,                              &
                                    NMNHDIM_BUDGET_NGROUPS,    NMNHDIM_UNUSED, NMNHDIM_UNKNOWN,                    &
                                    TYPEREAL
  use modd_io,                only: tfiledata
  use modd_lunit_n,           only: tluout
  use modd_parameters,        only: NBUNAMELGTMAX
  use modd_type_date,         only: date_time

  use mode_msg
  use mode_write_diachro,     only: Write_diachro

  use modi_end_cart_compress, only: End_cart_compress
  use modi_end_mask_compress, only: End_mask_compress

  implicit none

  type(tfiledata),                                      intent(in) :: tpdiafile   ! file to write
  type(date_time), dimension(:),                        intent(in) :: tpdates
  type(tbudgetdata),                                    intent(in) :: tpbudget ! Budget datastructure
  real,            dimension(:,:,:,:,:,:), allocatable, intent(in) :: prhodjn
  logical,                                              intent(in) :: knocompress ! compression for the cart option
  real,                                                 intent(in) :: ptstep      ! time step

  character(len=4)                                        :: ybutype
  integer                                                 :: igroups
  integer                                                 :: jproc
  integer                                                 :: jsv
  real,               dimension(:),           allocatable :: zconvert   ! unit conversion coefficient
  real,               dimension(:,:,:,:,:,:), allocatable :: zworkt
  type(tbudiachrometadata)                                :: tzbudiachro
  type(tbugroupdata), dimension(:),           allocatable :: tzfields

  call Print_msg( NVERB_DEBUG, 'BUD', 'Store_one_budget', 'called for '//trim( tpbudget%cname ) )

  if( .not. allocated( prhodjn ) ) then
    call Print_msg( NVERB_ERROR, 'BUD', 'Store_one_budget', 'prhodjn not allocated' )
    return
  end if

  igroups = tpbudget%ngroups

  if ( igroups == 0 ) return

  ! unit conversion for  ru budgets
  allocate( zconvert( igroups ) )
  do jproc = 1, igroups
    if (      tpbudget%tgroups(jproc)%cmnhname == 'INIF' &
         .or. tpbudget%tgroups(jproc)%cmnhname == 'ENDF' &
         .or. tpbudget%tgroups(jproc)%cmnhname == 'AVEF' ) then
      zconvert(jproc) = ptstep * Real( nbustep )
    else
      zconvert(jproc) = 1.
    end if
  end do

  select case ( cbutype )
    case( 'CART', 'SKIP' )
      ybutype = 'CART'
      if ( knocompress ) then
        allocate( zworkt(nbuimax, nbujmax, nbukmax, 1, 1, igroups ) ) ! local budget of ru
        do jproc = 1, igroups
          zworkt(:, :, :, 1, 1, jproc) = tpbudget%tgroups(jproc)%xdata(:, :, :) &
                                         * zconvert(jproc) / prhodjn(:, :, :, 1, 1, 1)
        end do
      else
        allocate( zworkt(nbuimax_ll, nbujmax_ll, nbukmax, 1, 1, igroups ) ) ! global budget of ru

        do jproc = 1, igroups
          zworkt(:, :, :, 1, 1, jproc) = End_cart_compress( tpbudget%tgroups(jproc)%xdata(:, :, :) )
          zworkt(:, :, :, 1, 1, jproc) = zworkt(:, :, :, 1, 1, jproc) * zconvert(jproc) / prhodjn(:, :, :, 1, 1, 1)
        end do
      endif
    case('MASK')
      ybutype = 'MASK'
      allocate( zworkt(1, 1, nbukmax, nbusubwrite, nbumask, igroups ) )
      do jproc = 1, igroups
        zworkt(1, 1, :, :, :, jproc) = End_mask_compress( tpbudget%tgroups(jproc)%xdata(:, :, :) ) &
                                       * zconvert(jproc) / prhodjn(1, 1, :, :, :, 1)
      end do

    case default
      call Print_msg( NVERB_ERROR, 'BUD', 'Store_one_budget', 'unknown CBUTYPE' )
  end select

  deallocate(zconvert)

  allocate( tzfields( igroups ) )

  !Copy all fields from tpbudget%tgroups
  tzfields(:) = tpbudget%tgroups(:)

  !Modify metadata coming from tpbudget%tgroups
  !ndims and ndimlist are adapted for Write_diachro
  do jproc = 1, igroups
    tzfields(jproc)%ndims = 4

    if ( tzfields(jproc)%ngrid < 1 .or. tzfields(jproc)%ngrid > 4 ) &
      call Print_msg( NVERB_FATAL, 'BUD', 'Store_one_budget_rho', 'invalid grid' )

    if ( ybutype == 'CART' ) then
      if ( .not. lbu_icp ) then
        select case ( tzfields(jproc)%ngrid )
          case ( 1, 4 )
            tzfields(jproc)%ndimlist(1)  = NMNHDIM_BUDGET_CART_NI
          case ( 2 )
            tzfields(jproc)%ndimlist(1)  = NMNHDIM_BUDGET_CART_NI_U
          case ( 3 )
            tzfields(jproc)%ndimlist(1)  = NMNHDIM_BUDGET_CART_NI_V
        end select
      else
        tzfields(jproc)%ndims = tzfields(jproc)%ndims - 1
        tzfields(jproc)%ndimlist(1)  = NMNHDIM_UNUSED
      end if

      if ( .not. lbu_jcp ) then
        select case ( tzfields(jproc)%ngrid )
          case ( 1, 4 )
            tzfields(jproc)%ndimlist(2)  = NMNHDIM_BUDGET_CART_NJ
          case ( 2 )
            tzfields(jproc)%ndimlist(2)  = NMNHDIM_BUDGET_CART_NJ_U
          case ( 3 )
            tzfields(jproc)%ndimlist(2)  = NMNHDIM_BUDGET_CART_NJ_V
        end select
      else
        tzfields(jproc)%ndims = tzfields(jproc)%ndims - 1
        tzfields(jproc)%ndimlist(2)  = NMNHDIM_UNUSED
      end if

      if ( .not. lbu_kcp ) then
        select case ( tzfields(jproc)%ngrid )
          case ( 1, 2, 3 )
            tzfields(jproc)%ndimlist(3)  = NMNHDIM_BUDGET_CART_LEVEL
          case ( 4 )
            tzfields(jproc)%ndimlist(3)  = NMNHDIM_BUDGET_CART_LEVEL_W
        end select
      else
        tzfields(jproc)%ndims = tzfields(jproc)%ndims - 1
        tzfields(jproc)%ndimlist(3)  = NMNHDIM_UNUSED
      end if
      tzfields(jproc)%ndimlist(4) = NMNHDIM_UNUSED
      tzfields(jproc)%ndimlist(5) = NMNHDIM_UNUSED
      tzfields(jproc)%ndimlist(6) = NMNHDIM_BUDGET_NGROUPS

    else if ( ybutype == 'MASK' ) then
      tzfields(jproc)%ndimlist(1) = NMNHDIM_UNUSED
      tzfields(jproc)%ndimlist(2) = NMNHDIM_UNUSED
      if ( .not. lbu_kcp ) then
        select case ( tzfields(jproc)%ngrid )
          case ( 1, 2, 3 )
            tzfields(jproc)%ndimlist(3)  = NMNHDIM_BUDGET_MASK_LEVEL
          case ( 4 )
            tzfields(jproc)%ndimlist(3)  = NMNHDIM_BUDGET_MASK_LEVEL_W
        end select
      else
        tzfields(jproc)%ndims = tzfields(jproc)%ndims - 1
        tzfields(jproc)%ndimlist(3) = NMNHDIM_UNUSED
      end if
      tzfields(jproc)%ndimlist(4) = NMNHDIM_BUDGET_TIME
      tzfields(jproc)%ndimlist(5) = NMNHDIM_BUDGET_MASK_NBUMASK
      tzfields(jproc)%ndimlist(6) = NMNHDIM_BUDGET_NGROUPS

    else
      tzfields(jproc)%ndimlist(:) = NMNHDIM_UNKNOWN
    end if
  end do

  tzbudiachro%lleveluse(NLVL_CATEGORY)    = .true.
  tzbudiachro%clevels  (NLVL_CATEGORY)    = 'Budgets'
  tzbudiachro%ccomments(NLVL_CATEGORY)    = 'Level for the different budgets'

  tzbudiachro%lleveluse(NLVL_SUBCATEGORY) = .false.
  tzbudiachro%clevels  (NLVL_SUBCATEGORY) = ''
  tzbudiachro%ccomments(NLVL_SUBCATEGORY) = ''

  tzbudiachro%lleveluse(NLVL_GROUP)       = .true.
  tzbudiachro%clevels  (NLVL_GROUP)       = Trim( tpbudget%cname )
  tzbudiachro%ccomments(NLVL_GROUP)       = Trim( tpbudget%ccomment )

  tzbudiachro%lleveluse(NLVL_SHAPE)       = .false.
  if ( ybutype == 'CART' ) then
    tzbudiachro%clevels  (NLVL_SHAPE)     = 'Cartesian'
    tzbudiachro%ccomments(NLVL_SHAPE)     = 'Cartesian domain'
  else
    tzbudiachro%clevels  (NLVL_SHAPE)     = 'Mask'
    tzbudiachro%ccomments(NLVL_SHAPE)     = 'Masked domain'
  end if

  tzbudiachro%lleveluse(NLVL_TIMEAVG)     = .false.
  tzbudiachro%clevels  (NLVL_TIMEAVG)     = 'Time_averaged'
  tzbudiachro%ccomments(NLVL_TIMEAVG)     = 'Values are time averaged'

  tzbudiachro%lleveluse(NLVL_NORM)        = .false.
  tzbudiachro%clevels  (NLVL_NORM)        = 'Not_normalized'
  tzbudiachro%ccomments(NLVL_NORM)        = 'Values are not normalized'

  tzbudiachro%lleveluse(NLVL_MASK)        = .false.
  if ( ybutype == 'MASK' ) then
    tzbudiachro%clevels  (NLVL_MASK)      = CMASK_VARNAME
    tzbudiachro%ccomments(NLVL_MASK)      = ''
  else
    tzbudiachro%clevels  (NLVL_MASK)      = ''
    tzbudiachro%ccomments(NLVL_MASK)      = ''
  end if

  if ( ybutype == 'CART' ) then
    tzbudiachro%lmobile  = .false.
  else
    !Masks are updated at each timestep (therefore the studied domains change during execution)
    tzbudiachro%lmobile  = .true.
  end if
  tzbudiachro%licompress = lbu_icp
  tzbudiachro%ljcompress = lbu_jcp
  tzbudiachro%lkcompress = lbu_kcp
  !Remark: ltcompress should be false for INIF and ENDF fields
  !        but if set to false these fields should be separated and stored somewhere else
  tzbudiachro%ltcompress = .true. !Data is temporally averaged
  tzbudiachro%lnorm      = .false.
  !Boundaries in physical domain does not make sense here if 'MASK'
  !In that case, these values are not written in the netCDF files
  !But they are always written in the LFI files. They are kept (in the MASK case) for backward compatibility.
  tzbudiachro%nil        = nbuil
  tzbudiachro%nih        = nbuih
  tzbudiachro%njl        = nbujl
  tzbudiachro%njh        = nbujh
  tzbudiachro%nkl        = nbukl
  tzbudiachro%nkh        = nbukh
  if ( tpbudget%nid > NBUDGET_SV1 ) then
    jsv = tpbudget%nid - NBUDGET_SV1 + 1
  else
    jsv = -1
  end if
  tzbudiachro%nsv        = jsv

  call Write_diachro( tpdiafile, tzbudiachro, tzfields, tpdates, zworkt, osplit = .true. )

end subroutine Store_one_budget

end module mode_write_budget
