!MNH_LIC Copyright 1995-2019 CNRS, Meteo-France and Universite Paul Sabatier
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
!-----------------------------------------------------------------

!#######################
module mode_write_budget
!#######################

use mode_msg

implicit none

private

public :: Write_budget

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
!!         CBUTYPE     : Budget type (CART,MASK,SKIP or NONE)
!!         CBURECORD   : name of output recording files for the budgets
!!         CBUCOMMENT  : name of a process for a budget
!!         NBUPROCNBR  : number of processes for each variable
!!         NBUTIME     : number of the budget time intervals ('MASK' case)
!!         NBUWRNB      : number of budget steps when the budget is written
!!         XBURU       : budget array of the variable RU
!!         XBURV       : budget array of the variable RV
!!         XBURW       : budget array of the variable RW
!!         XBURTH      : budget array of the variable RTH
!!         XBURTKE     : budget array of the variable RTKE
!!         XBURRV      : budget array of the variable RRV
!!         XBURRC      : budget array of the variable RRC
!!         XBURRR      : budget array of the variable RRR
!!         XBURRI      : budget array of the variable RRI
!!         XBURRS      : budget array of the variable RRS
!!         XBURRG      : budget array of the variable RRG
!!         XBURRH      : budget array of the variable RRH
!!         XBURSV      : budget array of the variable RSVx
!!
!!
!!    REFERENCE
!!    ---------
!!      Book2 of MESO-NH documentation (routine WRITE_BUDGET)
!!
!-------------------------------------------------------------------------------

  use modd_budget,         only: cbutype, nbumask, nbutshift, nbustep, nbuwrnb, xbulen, xbusurf,                                  &
                                 lbu_icp, lbu_jcp,                                                                                &
                                 lbu_ru, lbu_rv, lbu_rw, lbu_rth, lbu_rtke, lbu_rrv, lbu_rrc, lbu_rrr,                            &
                                 lbu_rri, lbu_rrs, lbu_rrg, lbu_rrh, lbu_rsv,                                                     &
                                 NBUDGET_RHO, NBUDGET_U, NBUDGET_V, NBUDGET_W, NBUDGET_TH, NBUDGET_TKE,                           &
                                 NBUDGET_RV, NBUDGET_RC, NBUDGET_RR, NBUDGET_RI, NBUDGET_RS, NBUDGET_RG, NBUDGET_RH, NBUDGET_SV1, &
                                 xburhodj, xburhodju, xburhodjv, xburhodjw,                                                       &
                                 xburu, xburv, xburw, xburth, xburtke,                                                            &
                                 xburrv, xburrc, xburrr, xburri, xburrs, xburrg, xburrh, xbursv
  use modd_io,             only: tfiledata
  use modd_lunit_n,        only: tluout
  use modd_parameters,     only: NMNHNAMELGTMAX
  use modd_type_date,      only: date_time

  use mode_datetime,       only: datetime_distance
  use mode_field,          only: tfielddata, TYPEREAL
  use mode_io_field_write, only: IO_Field_write
  use mode_menu_diachro,   only: Menu_diachro
  use mode_time,           only: tdtexp

  implicit none

  type(tfiledata), intent(in) :: tpdiafile    ! file to write
  type(date_time), intent(in) :: tpdtcur      ! current date and time
  real,            intent(in) :: ptstep       ! time step
  integer,         intent(in) :: ksv          ! number of scalar variables

  character(len=NMNHNAMELGTMAX)                        :: yrecfm        ! name of the article to be written
  integer                                              :: jt, jmask
  integer                                              :: jsv           ! loop index over the ksv svx
  logical                                              :: gnocompress   ! true: no compression along x and y direction (cart option)
  real,            dimension(:),           allocatable :: zworktemp
  real,            dimension(:,:,:,:,:,:), allocatable :: zrhodjn, zworkmask
  type(date_time), dimension(:),           allocatable :: tzdates
  type(tfielddata) :: tzfield
  !
  !-------------------------------------------------------------------------------
  !
  gnocompress = .true.
  !
  !* Write TSTEP and BULEN
  !  ---------------------
  !
  TZFIELD%CMNHNAME   = 'TSTEP'
  TZFIELD%CSTDNAME   = ''
  TZFIELD%CLONGNAME  = 'TSTEP'
  TZFIELD%CUNITS     = 's'
  TZFIELD%CDIR       = '--'
  TZFIELD%CCOMMENT   = 'Time step'
  TZFIELD%NGRID      = 0
  TZFIELD%NTYPE      = TYPEREAL
  TZFIELD%NDIMS      = 0
  TZFIELD%LTIMEDEP   = .FALSE.
  CALL IO_Field_write(TPDIAFILE,TZFIELD,PTSTEP)
  !
  TZFIELD%CMNHNAME   = 'BULEN'
  TZFIELD%CSTDNAME   = ''
  TZFIELD%CLONGNAME  = 'BULEN'
  TZFIELD%CUNITS     = 's'
  TZFIELD%CDIR       = '--'
  TZFIELD%CCOMMENT   = 'Time step'
  TZFIELD%NGRID      = 0
  TZFIELD%NTYPE      = TYPEREAL
  TZFIELD%NDIMS      = 0
  TZFIELD%LTIMEDEP   = .FALSE.
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
      ALLOCATE( ZWORKTEMP( 1 ) )
      allocate( tzdates( 1 ) )
  !
      !Compute time at the middle of the temporally-averaged budget timestep
      !This time is computed from the beginning of the experiment
      CALL DATETIME_DISTANCE(TDTEXP,TPDTCUR,ZWORKTEMP(1))
  !
      ZWORKTEMP(1)=ZWORKTEMP(1)+(1.-NBUSTEP*0.5)*PTSTEP
  !
      tzdates(1)%tdate%year  = tdtexp%tdate%year
      tzdates(1)%tdate%month = tdtexp%tdate%month
      tzdates(1)%tdate%day   = tdtexp%tdate%day
      tzdates(1)%time        = tdtexp%time + zworktemp(1)

      DEALLOCATE ( ZWORKTEMP )
  !
  !-------------------------------------------------------------------------------
  !
  !* 3.     'MASK' CASE
  !         -----------
  !
    CASE('MASK')
      ALLOCATE(ZWORKTEMP(NBUWRNB))
      allocate( tzdates( NBUWRNB ) )
      ALLOCATE(ZWORKMASK(SIZE(XBUSURF,1),SIZE(XBUSURF,2),1,NBUWRNB,NBUMASK,1))
  !
  ! local array
      DO JMASK=1,NBUMASK
        DO JT=1,NBUWRNB
          ZWORKMASK(:,:,1,JT,JMASK,1) = XBUSURF(:,:,JMASK,JT)
        END DO
      END DO
  !
      CALL DATETIME_DISTANCE(TDTEXP,TPDTCUR,ZWORKTEMP(NBUWRNB))
  !
      ZWORKTEMP(NBUWRNB)=ZWORKTEMP(NBUWRNB)+(1.-NBUSTEP*0.5)*PTSTEP
  !
      tzdates(NBUWRNB )%tdate%year  = tdtexp%tdate%year
      tzdates(NBUWRNB )%tdate%month = tdtexp%tdate%month
      tzdates(NBUWRNB )%tdate%day   = tdtexp%tdate%day
      tzdates(NBUWRNB )%time        = tdtexp%time + zworktemp(NBUWRNB )
      DO JT=1,NBUWRNB-1
        ZWORKTEMP(JT) = ZWORKTEMP(NBUWRNB)-NBUSTEP*PTSTEP*(NBUWRNB-JT)
        tzdates(jt )%tdate%year  = tdtexp%tdate%year
        tzdates(jt )%tdate%month = tdtexp%tdate%month
        tzdates(jt )%tdate%day   = tdtexp%tdate%day
        tzdates(jt )%time        = tdtexp%time + zworktemp(jt )
      END DO

      DEALLOCATE( ZWORKTEMP )
  !
  !*     3.1    storage of the masks  array
  !
      WRITE(TZFIELD%CMNHNAME,FMT="('MASK_',I4.4,'.MASK')" ) nbutshift
      TZFIELD%CSTDNAME   = ''
      TZFIELD%CLONGNAME  = TRIM(TZFIELD%CMNHNAME)
      TZFIELD%CUNITS     = ''
      TZFIELD%CDIR       = 'XY'
      WRITE(TZFIELD%CCOMMENT,FMT="('X_Y_MASK',I4.4)" ) nbutshift
      TZFIELD%NGRID      = 1
      TZFIELD%NTYPE      = TYPEREAL
      TZFIELD%NDIMS      = 6
      TZFIELD%LTIMEDEP   = .FALSE.
      CALL IO_Field_write(TPDIAFILE,TZFIELD,ZWORKMASK(:,:,:,:,:,:))
      WRITE(YRECFM,FMT="('MASK_',I4.4)" ) nbutshift
      CALL MENU_DIACHRO(TPDIAFILE,YRECFM)
      DEALLOCATE(ZWORKMASK)
  !
  END SELECT
  !
  if ( cbutype == 'CART' .or. cbutype == 'SKIP' .or. cbutype == 'MASK' ) then
  !
  !* Storage of the budgets array
  !
  !* XBURHODJU and RU budgets
  !
    IF (LBU_RU) THEN
      call Store_one_budget_rho( tpdiafile, tzdates, xburhodju, NBUDGET_U, gnocompress, zrhodjn )
      call Store_one_budget( tpdiafile, tzdates, xburu, zrhodjn, NBUDGET_U, gnocompress, ptstep )
    END IF
  !
  !* XBURHODJV and RV budgets
  !
    IF (LBU_RV) THEN
      call Store_one_budget_rho( tpdiafile, tzdates, xburhodjv, NBUDGET_V, gnocompress, zrhodjn )
      call Store_one_budget( tpdiafile, tzdates, xburv, zrhodjn, NBUDGET_V, gnocompress, ptstep )
    END IF
  !
  !* XBURHODJW and RW budgets
  !
    IF (LBU_RW) THEN
      call Store_one_budget_rho( tpdiafile, tzdates, xburhodjw, NBUDGET_W, gnocompress, zrhodjn )
      call Store_one_budget( tpdiafile, tzdates, xburw, zrhodjn, NBUDGET_W, gnocompress, ptstep )
    END IF
  !
  !* XBURHODJ storage for Scalars
  !
    IF (LBU_RTH .OR. LBU_RTKE .OR. LBU_RRV .OR. LBU_RRC .OR. LBU_RRR .OR. &
        LBU_RRI .OR. LBU_RRS  .OR. LBU_RRG .OR. LBU_RRH .OR. LBU_RSV      ) THEN
      call Store_one_budget_rho( tpdiafile, tzdates, xburhodj, NBUDGET_RHO, gnocompress, zrhodjn )
    ENDIF
  !
  !* RTH budget
  !
    IF (LBU_RTH) THEN
      call Store_one_budget( tpdiafile, tzdates, xburth, zrhodjn, NBUDGET_TH, gnocompress, ptstep )
    END IF
  !
  !* RTKE budget
  !
    IF (LBU_RTKE) THEN
      call Store_one_budget( tpdiafile, tzdates, xburtke, zrhodjn, NBUDGET_TKE, gnocompress, ptstep )
    END IF
  !
  !* RRV budget
  !
    IF (LBU_RRV) THEN
      call Store_one_budget( tpdiafile, tzdates, xburrv, zrhodjn, NBUDGET_RV, gnocompress, ptstep )
    END IF
  !
  !* RRC budget
  !
    IF (LBU_RRC) THEN
      call Store_one_budget( tpdiafile, tzdates, xburrc, zrhodjn, NBUDGET_RC, gnocompress, ptstep )
    END IF
  !
  !* RRR budget
  !
    IF (LBU_RRR) THEN
      call Store_one_budget( tpdiafile, tzdates, xburrr, zrhodjn, NBUDGET_RR, gnocompress, ptstep )
    END IF
  !
  !* RRI budget
  !
    IF (LBU_RRI) THEN
      call Store_one_budget( tpdiafile, tzdates, xburri, zrhodjn, NBUDGET_RI, gnocompress, ptstep )
    END IF
  !
  !* RRS budget
  !
    IF (LBU_RRS) THEN
      call Store_one_budget( tpdiafile, tzdates, xburrs, zrhodjn, NBUDGET_RS, gnocompress, ptstep )
    END IF
  !
  !* RRG budget
  !
    IF (LBU_RRG) THEN
      call Store_one_budget( tpdiafile, tzdates, xburrg, zrhodjn, NBUDGET_RG, gnocompress, ptstep )
    END IF
  !
  !* RRH budget
  !
    IF (LBU_RRH) THEN
      call Store_one_budget( tpdiafile, tzdates, xburrh, zrhodjn, NBUDGET_RH, gnocompress, ptstep )
    END IF
  !
  !* RSV budgets
  !
    IF (LBU_RSV) THEN
      DO JSV = 1,KSV
        call Store_one_budget( tpdiafile, tzdates, xbursv(:, :, :, :, jsv ), zrhodjn, &
                               NBUDGET_SV1 + jsv - 1, gnocompress, ptstep )
      END DO
    END IF
  end if

end subroutine Write_budget


subroutine Store_one_budget_rho( tpdiafile, tpdates, pburhodj, kp, knocompress, prhodjn )
  use modd_budget,            only: cbutype,                                                      &
                                    lbu_icp, lbu_jcp, lbu_kcp,                                    &
                                    nbuil, nbuih, nbujl, nbujh, nbukl, nbukh,                     &
                                    nbuimax, nbuimax_ll, nbujmax, nbujmax_ll, nbukmax, nbutshift, &
                                    nbumask, nbuwrnb,                                             &
                                    NBUDGET_RHO, NBUDGET_U, NBUDGET_V, NBUDGET_W
  use modd_io,                only: tfiledata
  use modd_lunit_n,           only: tluout
  use modd_parameters,        only: XNEGUNDEF
  use modd_type_date,         only: date_time

  use mode_write_diachro,     only: Write_diachro

  use modi_end_cart_compress, only: End_cart_compress
  use modi_end_mask_compress, only: End_mask_compress

  implicit none

  type(tfiledata),                                      intent(in)  :: tpdiafile   ! file to write
  type(date_time), dimension(:),                        intent(in)  :: tpdates
  real,            dimension(:,:,:),                    intent(in)  :: pburhodj    ! budget arrays for rhodj
  integer,                                              intent(in)  :: kp          ! reference number of budget
  logical,                                              intent(in)  :: knocompress ! compression for the cart option
  real,            dimension(:,:,:,:,:,:), allocatable, intent(out) :: prhodjn

  character(len=4)                               :: ybutype
  character(len=9)                               :: ygroup_name   ! group name
  character(len=99),  dimension(:), allocatable  :: ybucomment    ! comment
  character(len=100), dimension(:), allocatable  :: yworkcomment  ! comment
  character(len=100), dimension(:), allocatable  :: yworkunit     ! comment
  integer,            dimension(:), allocatable  :: iworkgrid     ! grid label

  if ( allocated( prhodjn ) ) deallocate( prhodjn )

  ! pburhodj storage
  select case ( cbutype )
    case( 'CART', 'SKIP' )
      ybutype = 'CART'
        if ( knocompress ) then
          allocate( prhodjn(nbuimax, nbujmax, nbukmax, 1, 1, 1 ) ) ! local budget of RHODJU
          prhodjn(:, :, :, 1, 1, 1 ) = pburhodj(:, :, : )
        else
          allocate( prhodjn(nbuimax_ll, nbujmax_ll, nbukmax, 1, 1, 1 ) ) ! global budget of RhodjU
          prhodjn(:,:,:,1,1,1)=end_cart_compress(pburhodj(:,:,:))
        end if
    case('MASK')
      ybutype = 'MASK'
        allocate( prhodjn(1, 1, nbukmax, nbuwrnb, nbumask, 1 ) )
        prhodjn(1, 1, :, :, :, 1 ) = End_mask_compress( pburhodj(:, :, : ) )
        where  ( prhodjn(1, 1, :, :, :, 1) <= 0. )
            prhodjn(1, 1, :, :, :, 1 ) = XNEGUNDEF
        end where

    case default
      call Print_msg( NVERB_ERROR, 'GEN', 'Store_one_budget_rho', 'unknown CBUTYPE' )
  end select

  allocate( ybucomment(1 ) )
  allocate( yworkunit(1 ) )
  allocate( yworkcomment(1 ) )
  allocate( iworkgrid(1 ) )

  select case( kp )
    case( NBUDGET_RHO )
      ybucomment(1)      = 'RhodJS'
      yworkunit(1)       = 'kg'
      yworkcomment(1)    = 'RhodJ for Scalars variables'
      iworkgrid(1)       = 1
      write( ygroup_name, fmt = "('RJS__',I4.4)" ) nbutshift

    case( NBUDGET_U )
      ybucomment(1)      = 'RhodJX'
      yworkunit(1)       = 'kg'
      yworkcomment(1)    = 'RhodJ for momentum along X axis'
      iworkgrid(1)       = 2
      write( ygroup_name, fmt = "('RJX__',I4.4)" ) nbutshift

    case( NBUDGET_V )
      ybucomment(1)      = 'RhodJY'
      yworkunit(1)       = 'kg'
      yworkcomment(1)    = 'RhodJ for momentum along Y axis'
      iworkgrid(1)       = 3
      write( ygroup_name, fmt = "('RJX__',I4.4)" ) nbutshift

    case( NBUDGET_W )
      ybucomment(1)      = 'RhodJZ'
      yworkunit(1)       = 'kg'
      yworkcomment(1)    = 'RhodJ for momentum along Z axis'
      iworkgrid(1)       = 4
      write( ygroup_name, fmt = "('RJZ__',I4.4)" ) nbutshift

    case default
      call Print_msg( NVERB_ERROR, 'GEN', 'Store_one_budget_rho', 'unknown budget type' )
  end select

  call Write_diachro( tpdiafile, tluout, ygroup_name, ybutype, iworkgrid,                          &
                      tpdates, prhodjn, ybucomment,                                                &
                      yworkunit, yworkcomment,                                                     &
                      oicp = lbu_icp, ojcp = lbu_jcp, okcp = lbu_kcp,                              &
                      kil = nbuil, kih = nbuih, kjl = nbujl, kjh = nbujh, kkl = nbukl, kkh = nbukh )
  deallocate( ybucomment, yworkunit, yworkcomment, iworkgrid )

end subroutine Store_one_budget_rho


subroutine Store_one_budget( tpdiafile, tpdates, pbudarray, prhodjn, kp, knocompress, ptstep )
  use modd_budget,            only: cbucomment, cbutype,                                                                          &
                                    lbu_icp, lbu_jcp, lbu_kcp,                                                                    &
                                    nbuil, nbuih, nbujl, nbujh, nbukl, nbukh,                                                     &
                                    nbuimax, nbuimax_ll, nbujmax, nbujmax_ll, nbukmax, nbuprocnbr, nbustep, nbutshift,            &
                                    nbumask, nbuwrnb,                                                                             &
                                    NBUDGET_U, NBUDGET_V, NBUDGET_W, NBUDGET_TH, NBUDGET_TKE, NBUDGET_RV, NBUDGET_RC, NBUDGET_RR, &
                                    NBUDGET_RI, NBUDGET_RS, NBUDGET_RG, NBUDGET_RH, NBUDGET_SV1
  use modd_io,                only: tfiledata
  use modd_lunit_n,           only: tluout
  use modd_type_date,         only: date_time

  use mode_write_diachro,     only: Write_diachro

  use modi_end_cart_compress, only: End_cart_compress
  use modi_end_mask_compress, only: End_mask_compress

  implicit none

  type(tfiledata),                                      intent(in) :: tpdiafile   ! file to write
  type(date_time), dimension(:),                        intent(in) :: tpdates
  real,            dimension(:,:,:,:),                  intent(in) :: pbudarray   ! budget array
  real,            dimension(:,:,:,:,:,:), allocatable, intent(in) :: prhodjn
  integer,                                              intent(in) :: kp          ! reference number of budget
  logical,                                              intent(in) :: knocompress ! compression for the cart option
  real,                                                 intent(in) :: ptstep      ! time step

  character(len=4)                                        :: ybutype
  character(len=9)                                        :: ygroup_name
  character(len=100), dimension(:),           allocatable :: yworkcomment
  character(len=100), dimension(:),           allocatable :: yworkunit
  integer                                                 :: jproc
  integer                                                 :: jsv
  integer                                                 :: jt
  integer,            dimension(:),           allocatable :: iworkgrid  ! grid label
  real,               dimension(:),           allocatable :: zconvert   ! unit conversion coefficient
  real,               dimension(:,:,:,:,:,:), allocatable :: zworkt

  if( .not. allocated( prhodjn ) ) then
    call Print_msg( NVERB_ERROR, 'GEN', 'Store_one_budget', 'prhodjn not allocated' )
    return
  end if

  ! unit conversion for  ru budgets
  allocate( zconvert( nbuprocnbr( kp ) ) )
  zconvert(1 : 2 )                = ptstep * Real( nbustep )
  zconvert(3 )                    = ptstep * Real( nbustep )
  zconvert(4 : nbuprocnbr( kp ) ) = 1.

  select case ( cbutype )
    case( 'CART', 'SKIP' )
      ybutype = 'CART'
        if ( knocompress ) then
          allocate( zworkt(nbuimax, nbujmax, nbukmax, 1, 1, nbuprocnbr(kp ) ) ) ! local budget of ru
          do jproc = 1, nbuprocnbr(kp )
            zworkt(:, :, :, 1, 1, jproc ) = pbudarray(:, :, :, jproc ) * zconvert(jproc ) / prhodjn(:, :, :, 1, 1, 1 )
          end do
        else
          allocate( zworkt(nbuimax_ll, nbujmax_ll, nbukmax, 1, 1, nbuprocnbr(kp ) ) ) ! global budget of ru
  !
          do jproc = 1, nbuprocnbr(kp )
            zworkt(:, :, :, 1, 1, jproc ) = End_cart_compress( pbudarray(:, :, :, jproc ) )
            zworkt(:, :, :, 1, 1, jproc ) = zworkt(:, :, :, 1, 1, jproc ) * zconvert(jproc ) / prhodjn(:, :, :, 1, 1, 1 )
          end do
        endif
    case('MASK')
      ybutype = 'MASK'
        allocate( zworkt(1, 1, nbukmax, nbuwrnb, nbumask, nbuprocnbr(kp ) ) )
        do jproc = 1, nbuprocnbr(kp )
          zworkt(1, 1, :, :, :, jproc ) = End_mask_compress( pbudarray(:, :, :, jproc ) ) &
                                          * zconvert(jproc ) / prhodjn(1, 1, :, :, :, 1 )
        end do

    case default
      call Print_msg( NVERB_ERROR, 'GEN', 'Store_one_budget', 'unknown CBUTYPE' )
  end select

  deallocate(zconvert)
!
!  RU budgets storage
  allocate( yworkunit( nbuprocnbr(kp ) ) )
  allocate( yworkcomment( nbuprocnbr(kp ) ) )
  allocate( iworkgrid( nbuprocnbr(kp ) ) )
!
  select case( kp )
    case ( NBUDGET_U )
      yworkunit(:)       = 'm s-2'; yworkunit(1:3) = 'm s-1'
      yworkcomment(:)    = 'Budget of momentum along X axis'
      iworkgrid(:)       = 2
      write( ygroup_name, fmt = "('UU___',I4.4)" ) nbutshift

    case ( NBUDGET_V )
      yworkunit(:)       = 'm s-2'; yworkunit(1:3) = 'm s-1'
      yworkcomment(:)    = 'Budget of momentum along Y axis'
      iworkgrid(:)       = 3
      write( ygroup_name, fmt = "('VV___',I4.4)" ) nbutshift

    case ( NBUDGET_W )
      yworkunit(:)       = 'm s-2'; yworkunit(1:3) = 'm s-1'
      yworkcomment(:)    = 'Budget of momentum along Z axis'
      iworkgrid(:)       = 4
      write( ygroup_name, fmt = "('WW___',I4.4)" ) nbutshift

    case ( NBUDGET_TH )
      yworkunit(:)       = 'K s-1' ; yworkunit(1:3) = 'K'
      yworkcomment(:)    = 'Budget of potential temperature'
      iworkgrid(:)       = 1
      write( ygroup_name, fmt = "('TH___',I4.4)" ) nbutshift

    case ( NBUDGET_TKE )
      yworkunit(:)       = 'm2 s-3' ; yworkunit(1:3) = 'm2 s-1'
      yworkcomment(:)    = 'Budget of turbulent kinetic energy'
      iworkgrid(:)       = 1
      write( ygroup_name, fmt = "('TK___',I4.4)" ) nbutshift

    case ( NBUDGET_RV )
      yworkunit(:)       = 's-1' ;  yworkunit(1:3) = 'kg kg-1'
      yworkcomment(:)    = 'Budget of water vapor mixing ratio'
      iworkgrid(:)       = 1
      write( ygroup_name, fmt = "('RV___',I4.4)" ) nbutshift

    case ( NBUDGET_RC )
      yworkunit(:)       = 's-1' ;  yworkunit(1:3) = 'kg kg-1'
      yworkcomment(:)    = 'Budget of cloud water mixing ratio'
      iworkgrid(:)       = 1
      write( ygroup_name, fmt = "('RC___',I4.4)" ) nbutshift

    case ( NBUDGET_RR )
      yworkunit(:)       = 's-1' ;  yworkunit(1:3) = 'kg kg-1'
      yworkcomment(:)    = 'Budget of rain water mixing ratio'
      iworkgrid(:)       = 1
      write( ygroup_name, fmt = "('RR___',I4.4)" ) nbutshift

    case ( NBUDGET_RI )
      yworkunit(:)       = 's-1' ;  yworkunit(1:3) = 'kg kg-1'
      yworkcomment(:)    = 'Budget of cloud ice mixing ratio'
      iworkgrid(:)       = 1
      write( ygroup_name, fmt = "('RI___',I4.4)" ) nbutshift

    case ( NBUDGET_RS )
      yworkunit(:)       = 's-1' ;  yworkunit(1:3) = 'kg kg-1'
      yworkcomment(:)    = 'Budget of snow/aggregate mixing ratio'
      iworkgrid(:)       = 1
      write( ygroup_name, fmt = "('RS___',I4.4)" ) nbutshift

    case ( NBUDGET_RG )
      yworkunit(:)       = 's-1' ;  yworkunit(1:3) = 'kg kg-1'
      yworkcomment(:)    = 'Budget of graupel mixing ratio'
      iworkgrid(:)       = 1
      write( ygroup_name, fmt = "('RG___',I4.4)" ) nbutshift

    case ( NBUDGET_RH )
      yworkunit(:)       = 's-1' ;  yworkunit(1:3) = 'kg kg-1'
      yworkcomment(:)    = 'Budget of hail mixing ratio'
      iworkgrid(:)       = 1
      write( ygroup_name, fmt = "('RH___',I4.4)" ) nbutshift

    case ( NBUDGET_SV1 : )
      jsv = kp - NBUDGET_SV1 + 1
      yworkunit(:)       = 's-1' ;  yworkunit(1:3) = '  '
      DO JT = 1,nbuprocnbr(kp)
        WRITE(yworkcomment(JT),FMT="('Budget of SVx=',I3.3)") jsv
      END DO
      iworkgrid(:)       = 1
      write( ygroup_name, fmt = "('SV',I3.3,I4.4)") jsv, nbutshift

    case default
      call Print_msg( NVERB_ERROR, 'GEN', 'Store_one_budget', 'unknown budget type' )
  end select

  CALL Write_diachro( tpdiafile, tluout, ygroup_name, ybutype, iworkgrid,                              &
                          tpdates, zworkt, cbucomment(kp, :),                                          &
                          yworkunit, yworkcomment,                                                     &
                          oicp = lbu_icp, ojcp = lbu_jcp, okcp = lbu_kcp,                              &
                          kil = nbuil, kih = nbuih, kjl = nbujl, kjh = nbujh, kkl = nbukl, kkh = nbukh )

  deallocate( zworkt, yworkunit, yworkcomment, iworkgrid )

end subroutine Store_one_budget

end module mode_write_budget
