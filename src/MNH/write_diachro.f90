!MNH_LIC Copyright 1996-2023 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
module mode_write_diachro

use mode_msg

implicit none

private

public :: Write_diachro

interface Att_write
   procedure Att_write_c0, Att_write_i0, Att_write_x0
end interface
contains

! #################################################################
subroutine Write_diachro( tpdiafile, tpbudiachro, tpfields,       &
                          tpdates, pvar, osplit, tpflyer )
! #################################################################
!
!!****  *WRITE_DIACHRO* - Ecriture d'un enregistrement dans un fichier
!!                        diachronique (de nom de base HGROUP)
!!
!!    PURPOSE
!!    -------
!
!
!!**  METHOD
!!    ------
!!      En fait pour un groupe donne HGROUP, on ecrit systematiquement
!       plusieurs enregistrements :
!       - 1: HGROUP.TYPE          (type d'informations a enregistrer)
!       - 2: HGROUP.DIM           (dimensions de toutes les matrices a
!                                  enregistrer)
!       - 3: HGROUP.TITRE         (Nom des processus)
!       - 4: HGROUP.UNITE         (Unites pour chaque processus)
!       - 5: HGROUP.COMMENT       (Champ commentaire pour chaque processus)
!       - 6: HGROUP.TRAJT         (Temps)
!       - 7: HGROUP.PROCx         (Champ traite . 1 enr./ 1 processus)
!       - 8: HGROUP.DATIM         (Les differentes dates du modele)
!       et pour certains types d'informations on enregistre egalement
!       des coordonnees (HGROUP.TRAJX, HGROUP.TRAJY, HGROUP.TRAJZ)
!!
!!    EXTERNAL
!!    --------
!!      None
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!      Module
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!      J. Duron    * Laboratoire d'Aerologie *
!!
!!
!!    MODIFICATIONS
!!    -------------
!!      Original       08/01/96
!!      Updated   PM
!!      Modification (N. Asencio) 18/06/99  : the two first dimensions of PMASK
!!                   are linked to the horizontal grid, FMWRIT is called with 'XY' argument.
!!                   In standard configuration of the budgets, the mask is written once
!!                   outside this routine with FMWRIT call. Its record name is 'MASK_nnnn.MASK'
!!                   So optional PMASK is not used .
!!      Modification (J. Duron)   24/06/99  : add logical GPACK to disable the pack option,
!!                                            add the initialization of the dimensions of
!!                                          MASK array in MASK case with write outside the
!!                                          routine.
!!      J.Escobar       02/10/2015 modif for JPHEXT(JPVEXT) variable
!!      D.Gazen+ G.Delautier 06/2016 modif for ncl files
!!      P. Wautelet     09/06/2017: name of the variable added to the name of the written field
!!                                  and better comment (true comment + units)
!!  Philippe Wautelet: 05/2016-04/2018: new data structures and calls for I/O
!  P. Wautelet 13/09/2019: budget: simplify and modernize date/time management
!  P. Wautelet 13/09/2019: remove never used PMASK optional dummy-argument
!  P. Wautelet 28/08/2020: remove TPLUOUTDIA dummy argument
!  P. Wautelet 09/10/2020: use new data type tpfields
!  P. Wautelet 08/12/2020: merge budgets terms with different nbutshift in same group variables
!  P. Wautelet 03/03/2021: add tbudiachrometadata type (useful to pass more information to Write_diachro)
!  P. Wautelet 11/03/2021: remove ptrajx/y/z optional dummy arguments of Write_diachro
!                          + get the trajectory data for LFI files differently
!  P. Wautelet 01/09/2021: allow NMNHDIM_LEVEL and NMNHDIM_LEVEL_W simultaneously
!  P. Wautelet    06/2022: reorganize flyers
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
use modd_aircraft_balloon, only: tflyerdata
use modd_budget,           only: tbudiachrometadata
use modd_conf,             only: lpack
use modd_field,            only: tfieldmetadata_base
use modd_io,               only: gsmonoproc, tfiledata
use modd_type_date,        only: date_time
!
IMPLICIT NONE
!
!*       0.1   Dummy arguments
!              ---------------
TYPE(TFILEDATA),                                    INTENT(IN)           :: TPDIAFILE    ! file to write
type(tbudiachrometadata),                           intent(in)           :: tpbudiachro
class(tfieldmetadata_base), dimension(:),           intent(in)           :: tpfields
type(date_time),            dimension(:),           intent(in)           :: tpdates  !Used only for LFI files
REAL,                       DIMENSION(:,:,:,:,:,:), INTENT(IN)           :: PVAR
logical,                                            intent(in), optional :: osplit
class(tflyerdata),                                  intent(in), optional :: tpflyer
!
!*       0.1   Local variables
!              ---------------
logical :: omonoproc_save ! Copy of true value of gsmonoproc
logical :: gpack
!------------------------------------------------------------------------------

call Print_msg( NVERB_DEBUG, 'BUD', 'Write_diachro', 'called' )

gpack = lpack
lpack = .false.

if ( present( tpflyer ) ) then
  ! Save gsmonoproc value
  omonoproc_save = gsmonoproc

  ! Force gsmonoproc to true to allow IO_Field_write on only 1 process! (not very clean hack)
  ! This is necessary for flyers because their data is local to 1 one process (and has been copied on the master rank of the file)
  gsmonoproc = .true.
end if

#ifdef MNH_IOLFI
if ( tpdiafile%cformat == 'LFI' .or. tpdiafile%cformat == 'LFICDF4' ) &
  call Write_diachro_lfi( tpdiafile, tpbudiachro, tpfields, tpdates, pvar,         tpflyer )
#endif

#ifdef MNH_IOCDF4
if ( tpdiafile%cformat == 'NETCDF4' .or. tpdiafile%cformat == 'LFICDF4' ) &
  call Write_diachro_nc4( tpdiafile, tpbudiachro, tpfields,          pvar, osplit, tpflyer )
#endif

lpack = gpack

if ( present( tpflyer ) ) then
  ! Restore correct value of gsmonoproc
  gsmonoproc = omonoproc_save
end if

! end subroutine Write_diachro_1
end subroutine Write_diachro

#ifdef MNH_IOLFI
!-----------------------------------------------------------------------------
subroutine Write_diachro_lfi( tpdiafile, tpbudiachro, tpfields, tpdates, pvar, tpflyer )

use modd_aircraft_balloon, only: tflyerdata
use modd_budget,         only: NLVL_CATEGORY, NLVL_GROUP, NLVL_SHAPE, nbumask, nbutshift, nbusubwrite, tbudiachrometadata
use modd_field,          only: NMNHDIM_ONE, NMNHDIM_UNKNOWN, NMNHDIM_BUDGET_LES_MASK, &
                               NMNHDIM_FLYER_TIME, NMNHDIM_NOTLISTED, NMNHDIM_UNUSED, &
                               TYPECHAR, TYPEINT, TYPEREAL,                           &
                               tfieldmetadata_base, tfieldmetadata
use modd_io,             only: tfiledata
use modd_les,            only: nles_current_iinf, nles_current_isup, nles_current_jinf, nles_current_jsup, &
                               nles_k, xles_current_z
use modd_parameters,     only: jphext
use modd_time,           only: tdtexp, tdtseg
use modd_time_n,         only: tdtmod
use modd_type_date,      only: date_time

use mode_datetime,       only: Datetime_distance
use mode_io_field_write, only: IO_Field_write, IO_Field_write_box
use mode_menu_diachro,   only: Menu_diachro
use mode_tools_ll,       only: Get_globaldims_ll


type(tfiledata),                                    intent(in)           :: tpdiafile        ! File to write
type(tbudiachrometadata),                           intent(in)           :: tpbudiachro
class(tfieldmetadata_base), dimension(:),           intent(in)           :: tpfields
type(date_time),            dimension(:),           intent(in)           :: tpdates
real,                       dimension(:,:,:,:,:,:), intent(in)           :: pvar
class(tflyerdata),                                  intent(in), optional :: tpflyer

integer, parameter :: LFITITLELGT = 100
integer, parameter :: LFIUNITLGT = 100
integer, parameter :: LFICOMMENTLGT = 100

character(len=:), allocatable :: ycategory
character(len=:), allocatable :: yshape
character(len=:), allocatable :: ytype
CHARACTER(LEN=20) :: YCOMMENT
CHARACTER(LEN=3)  :: YJ
character(len=:),                           allocatable :: ygroup
character(len=LFITITLELGT),   dimension(:), allocatable :: ytitles   !Used to respect LFI fileformat
character(len=LFIUNITLGT),    dimension(:), allocatable :: yunits    !Used to respect LFI fileformat
character(len=LFICOMMENTLGT), dimension(:), allocatable :: ycomments !Used to respect LFI fileformat
INTEGER   ::   ILENG, ILENTITRE, ILENUNITE, ILENCOMMENT
integer   :: iil, iih, ijl, ijh, ikl, ikh
integer   :: idx
INTEGER   ::   II, IJ, IK, IT, IN, IP, J, JJ
INTEGER   ::   INTRAJT, IKTRAJX, IKTRAJY, IKTRAJZ
INTEGER   ::   ITTRAJX, ITTRAJY, ITTRAJZ
INTEGER   ::   INTRAJX, INTRAJY, INTRAJZ
INTEGER   ::   IIMASK, IJMASK, IKMASK, ITMASK, INMASK, IPMASK
INTEGER   ::   IIMAX_ll, IJMAX_ll ! size of the physical global domain
integer   ::   ji
INTEGER,DIMENSION(:),ALLOCATABLE :: ITABCHAR
logical   :: gdistributed
real, dimension(:,:), allocatable :: ztimes
real, dimension(:,:), allocatable :: zdatime
real, dimension(:,:,:), allocatable :: ztrajz
TYPE(TFIELDMETADATA) :: TZFIELD
type(tfiledata)  :: tzfile

call Print_msg( NVERB_DEBUG, 'BUD', 'Write_diachro_lfi', 'called' )

tzfile = tpdiafile

iil = tpbudiachro%nil
iih = tpbudiachro%nih
ijl = tpbudiachro%njl
ijh = tpbudiachro%njh
ikl = tpbudiachro%nkl
ikh = tpbudiachro%nkh

ycategory = Trim( tpbudiachro%clevels(NLVL_CATEGORY) )
yshape    = Trim( tpbudiachro%clevels(NLVL_SHAPE) )

!For backward compatibility of LFI files
if ( tpbudiachro%cdirection == 'I' ) then
  ijl = 1
  ijh = 1
else if ( tpbudiachro%cdirection == 'J' ) then
  iil = 1
  iih = 1
end if

!Write only in LFI files
tzfile%cformat = 'LFI'

YCOMMENT='NOTHING'

!Set ygroup to preserve backward compatibility of LFI files
if (      Any( tpbudiachro%clevels(NLVL_GROUP) == [ 'UU', 'VV', 'WW', 'TH', 'TK', 'RV', 'RC', 'RR', 'RI', 'RS', 'RG', 'RH' ] ) &
     .or.    ( tpbudiachro%clevels(NLVL_GROUP)(1:2) == 'SV' .and. Len_trim( tpbudiachro%clevels(NLVL_GROUP) ) == 5 )         ) then
  Allocate( character(len=9) :: ygroup )
  ygroup(:) = Trim( tpbudiachro%clevels(NLVL_GROUP) )
  do ji = Len_trim( tpbudiachro%clevels(NLVL_GROUP) ) + 1, 5
    ygroup(ji : ji) = '_'
  end do
  Write( ygroup(6:9), '( i4.4 )' ) nbutshift
else if ( tpbudiachro%clevels(NLVL_GROUP) == 'RhodJ' ) then
  Allocate( character(len=9) :: ygroup )

  if ( tpfields(1)%cmnhname == 'RhodJX' ) then
    ygroup(1:3) = 'RJX'
  else if ( tpfields(1)%cmnhname == 'RhodJY' ) then
    ygroup(1:3) = 'RJY'
  else if ( tpfields(1)%cmnhname == 'RhodJZ' ) then
    ygroup(1:3) = 'RJZ'
  else if ( tpfields(1)%cmnhname == 'RhodJS' ) then
    ygroup(1:3) = 'RJS'
  else
    call Print_msg( NVERB_ERROR, 'IO', 'Write_diachro_lfi', 'unknown variable ' // Trim( tpfields(1)%cmnhname ) // &
                    ' for group ' // Trim( tpbudiachro%clevels(NLVL_GROUP) ) )
  end if

  ygroup(4:5) = '__'
  Write( ygroup(6:9), '( i4.4 )' ) nbutshift
else if ( tpbudiachro%nsv > 0 ) then
  Allocate( character(len=9) :: ygroup )
  Write( ygroup, '( "SV", i3.3, i4.4 )' ) tpbudiachro%nsv, nbutshift
else if ( tpbudiachro%clevels(NLVL_CATEGORY) == 'LES_budgets' .and. tpbudiachro%clevels(NLVL_GROUP)(1:3)/='BU_' ) then
  if ( tpfields(1)%ndimlist(6) == NMNHDIM_BUDGET_LES_MASK ) then
    !Remove the name of the mask (different for each 'process') to get the common name for the group
    idx = Index( tpfields(1)%cmnhname, ' ' )
    if ( idx > 0 ) then
      ygroup = tpfields(1)%cmnhname(1:idx- 1)
    else
      ygroup = Trim( tpfields(1)%cmnhname )
    end if
  else
    ygroup = Trim( tpfields(1)%cmnhname )
  end if
else
  ygroup = Trim( tpbudiachro%clevels(NLVL_GROUP) )
end if

!For backward compatibility
if (       Trim( tpbudiachro%clevels(NLVL_CATEGORY) ) == 'Flyers'           &
     .and. Trim( tpbudiachro%clevels(NLVL_SHAPE) )    == 'Vertical_profile' ) then
  ygroup = Trim( ygroup ) // 'Z'
end if

if (       Trim( tpbudiachro%clevels(NLVL_CATEGORY) ) == 'LES_budgets' &
     .and. Trim( tpbudiachro%clevels(NLVL_SHAPE) )    == 'Cartesian'   ) then
  if ( tpbudiachro%ltcompress ) then
    if ( tpbudiachro%lnorm ) then
      ygroup = 'H_' // Trim( ygroup )
    else
      ygroup = 'A_' // Trim( ygroup )
    end if
  else
    if ( tpbudiachro%lnorm ) then
      ygroup = 'E_' // Trim( ygroup )
    else
      !Nothing to do
    end if
  end if
  !Limit to 10 characters (backward compatibility again...)
  if ( Len_trim( ygroup )  > 10 ) ygroup = ygroup(1:10)
end if

if (       Trim( tpbudiachro%clevels(NLVL_CATEGORY) ) == 'LES_budgets' &
     .and. Trim( tpbudiachro%clevels(NLVL_GROUP) )    == 'Spectrum'    ) then
  if ( tpbudiachro%ltcompress ) then
    ygroup = 'T_' // Trim( ygroup )
    !Limit to 10 characters (backward compatibility again...)
    if ( Len_trim( ygroup )  > 10 ) ygroup = ygroup(1:10)
  end if
end if

!Recompute old TYPE for backward compatibility
if ( ycategory == 'Budgets' ) then
  if ( yshape == 'Cartesian' ) then
    ytype = 'CART'
  else
    ytype = 'MASK'
  end if
else if ( ycategory == 'LES_budgets' ) then
  if ( yshape == 'Cartesian' ) then
    ytype = 'SSOL'
  else
    ytype = 'SPXY'
  end if
else if ( ycategory == 'Flyers' ) then
  if ( yshape == 'Point' ) then
    ytype = 'RSPL'
  else
    ytype = 'CART'
  end if
else if ( ycategory == 'Profilers' .or. ycategory == 'Stations' ) then
  ytype = 'CART'
else if ( ycategory == 'Time_series'  ) then
  if ( tpbudiachro%licompress ) then
    ytype = 'CART'
  else
    ytype = 'SSOL'
  end if
else
  call Print_msg( NVERB_ERROR, 'IO', 'Write_diachro_lfi', &
                  'unknown classification for type of variable '//trim(tpfields(1)%cmnhname) )
  ytype = 'UNKN'
end if

II = SIZE(PVAR,1)
IJ = SIZE(PVAR,2)
if ( ycategory == 'Budgets' .and. tpbudiachro%clevels(NLVL_SHAPE) == 'Cartesian' &
     .and. .not. tpbudiachro%licompress .and. .not. tpbudiachro%ljcompress       ) then
  II=iih-iil+1
  IJ=ijh-ijl+1
  gdistributed = .true.
else
  !By default data is already collected on the write process for budgets
  gdistributed = .false.
end if
IK = SIZE(PVAR,3)
IT = SIZE(PVAR,4)
IN = SIZE(PVAR,5)
IP = SIZE(PVAR,6)

INTRAJT=SIZE(tpdates)

IKTRAJX=0; IKTRAJY=0; IKTRAJZ=0
ITTRAJX=0; ITTRAJY=0; ITTRAJZ=0
INTRAJX=0; INTRAJY=0; INTRAJZ=0
IF ( PRESENT( tpflyer ) ) THEN
  IKTRAJX = 1
  ITTRAJX = SIZE( tpflyer%xx )
  INTRAJX = 1
ELSE IF ( ycategory == 'LES_budgets' .and.  tpbudiachro%clevels(NLVL_SHAPE) == 'Cartesian' ) THEN
  IKTRAJX = 1
  ITTRAJX = 1
  INTRAJX = IN
ENDIF
IF ( PRESENT( tpflyer ) ) THEN
  IKTRAJY = 1
  ITTRAJY = SIZE( tpflyer%xy )
  INTRAJY = 1
ELSE IF ( ycategory == 'LES_budgets' .and.  tpbudiachro%clevels(NLVL_SHAPE) == 'Cartesian' ) THEN
  IKTRAJY = 1
  ITTRAJY = 1
  INTRAJY = IN
ENDIF
IF ( PRESENT( tpflyer ) ) THEN
  IKTRAJZ = 1
  ITTRAJZ = SIZE( tpflyer%xz )
  INTRAJZ = 1
ELSE IF ( ycategory == 'LES_budgets' .and.  tpbudiachro%clevels(NLVL_SHAPE) == 'Cartesian' ) THEN
  IKTRAJZ = IK
  ITTRAJZ = 1
  INTRAJZ = IN
ENDIF

IIMASK=0; IJMASK=0; IKMASK=0; ITMASK=0; INMASK=0; IPMASK=0
IF ( tpbudiachro%clevels(NLVL_SHAPE) == 'Mask' ) THEN
!     MASK is written outside this routine but the dimensions must be initialized
!     the mask is defined on the extended domain
  CALL GET_GLOBALDIMS_ll (IIMAX_ll,IJMAX_ll)
  IIMASK=IIMAX_ll + 2 * JPHEXT
  IJMASK=IJMAX_ll + 2 * JPHEXT
  IKMASK=1
  ITMASK=nbusubwrite
  INMASK=NBUMASK
  IPMASK=1
ENDIF

ILENTITRE   = LFITITLELGT
ILENUNITE   = LFIUNITLGT
ILENCOMMENT = LFICOMMENTLGT
!
! 1er enregistrement TYPE
!
TZFIELD = TFIELDMETADATA(             &
  CMNHNAME   = TRIM(ygroup)//'.TYPE', &
  CSTDNAME   = '',                    &
  CLONGNAME  = TRIM(ygroup)//'.TYPE', &
  CUNITS     = '',                    &
  CDIR       = '--',                  &
  CCOMMENT   = TRIM(YCOMMENT),        &
  NGRID      = tpfields(1)%ngrid,     &
  NTYPE      = TYPECHAR,              &
  NDIMS      = 0,                     &
  LTIMEDEP   = .FALSE.                )
CALL IO_Field_write(tzfile,TZFIELD,YTYPE)
!
! 2eme  enregistrement DIMENSIONS des MATRICES et LONGUEUR des TABLEAUX de CARACTERES et FLAGS de COMPRESSION sur les DIFFERENTS AXES
!
TZFIELD = TFIELDMETADATA(            &
  CMNHNAME   = TRIM(ygroup)//'.DIM', &
  CSTDNAME   = '',                   &
  CLONGNAME  = TRIM(ygroup)//'.DIM', &
  CUNITS     = '',                   &
  CDIR       = '--',                 &
  CCOMMENT   = TRIM(YCOMMENT),       &
  NGRID      = tpfields(1)%ngrid,    &
  NTYPE      = TYPEINT,              &
  NDIMS      = 1,                    &
  LTIMEDEP   = .FALSE.               )
SELECT CASE(YTYPE)
  CASE('CART','MASK','SPXY')
    if ( iil < 0 .or. iih < 0 .or. ijl < 0 .or. ijh < 0 .or. ikl < 0 .or. ikh < 0 ) then
      call Print_msg( NVERB_FATAL, 'IO', 'Write_diachro_lfi', &
                      'nil, nih, njl, njh, nkl or nkh not set in tpbudiachro for variable ' // Trim( tpfields(1)%cmnhname ) )
    end if
    ILENG = 34
    ALLOCATE(ITABCHAR(ILENG))
    ITABCHAR(1)=ILENTITRE; ITABCHAR(2)=ILENUNITE
    ITABCHAR(3)=ILENCOMMENT; ITABCHAR(4)=II
    ITABCHAR(5)=IJ; ITABCHAR(6)=IK
    ITABCHAR(7)=IT; ITABCHAR(8)=IN
    ITABCHAR(9)=IP; ITABCHAR(10)=iil
    ITABCHAR(11)=ijl; ITABCHAR(12)=ikl
    ITABCHAR(13)=iih; ITABCHAR(14)=ijh
    ITABCHAR(15)=ikh
    ITABCHAR(16)=Merge( 1, 0, tpbudiachro%licompress )
    ITABCHAR(17)=Merge( 1, 0, tpbudiachro%ljcompress )
    ITABCHAR(18)=Merge( 1, 0, tpbudiachro%lkcompress )
    IF( tpbudiachro%clevels(NLVL_SHAPE) == 'Mask' )THEN
!     ITABCHAR(10)=1; ITABCHAR(11)=1
!     ITABCHAR(13)=1; ITABCHAR(14)=1
      ITABCHAR(16)=1; ITABCHAR(17)=1
    ENDIF
    ITABCHAR(19)=INTRAJT; ITABCHAR(20)=IKTRAJX
    ITABCHAR(21)=IKTRAJY; ITABCHAR(22)=IKTRAJZ
    ITABCHAR(23)=ITTRAJX; ITABCHAR(24)=ITTRAJY
    ITABCHAR(25)=ITTRAJZ; ITABCHAR(26)=INTRAJX
    ITABCHAR(27)=INTRAJY; ITABCHAR(28)=INTRAJZ
    ITABCHAR(29)=IIMASK; ITABCHAR(30)=IJMASK
    ITABCHAR(31)=IKMASK; ITABCHAR(32)=ITMASK
    ITABCHAR(33)=INMASK; ITABCHAR(34)=IPMASK
    CALL IO_Field_write(tzfile,TZFIELD,ITABCHAR)
    DEALLOCATE(ITABCHAR)
  CASE DEFAULT
    ILENG = 25
    ALLOCATE(ITABCHAR(ILENG))
    ITABCHAR(1)=ILENTITRE; ITABCHAR(2)=ILENUNITE
    ITABCHAR(3)=ILENCOMMENT; ITABCHAR(4)=II
    ITABCHAR(5)=IJ; ITABCHAR(6)=IK
    ITABCHAR(7)=IT; ITABCHAR(8)=IN
    ITABCHAR(9)=IP
    ITABCHAR(10)=INTRAJT; ITABCHAR(11)=IKTRAJX
    ITABCHAR(12)=IKTRAJY; ITABCHAR(13)=IKTRAJZ
    ITABCHAR(14)=ITTRAJX; ITABCHAR(15)=ITTRAJY
    ITABCHAR(16)=ITTRAJZ; ITABCHAR(17)=INTRAJX
    ITABCHAR(18)=INTRAJY; ITABCHAR(19)=INTRAJZ
    ITABCHAR(20)=IIMASK; ITABCHAR(21)=IJMASK
    ITABCHAR(22)=IKMASK; ITABCHAR(23)=ITMASK
    ITABCHAR(24)=INMASK; ITABCHAR(25)=IPMASK
    CALL IO_Field_write(tzfile,TZFIELD,ITABCHAR)
    DEALLOCATE(ITABCHAR)
END SELECT
!
! 3eme enregistrement TITRE
!
TZFIELD = TFIELDMETADATA(              &
  CMNHNAME   = TRIM(ygroup)//'.TITRE', &
  CSTDNAME   = '',                     &
  CLONGNAME  = TRIM(ygroup)//'.TITRE', &
  CUNITS     = '',                     &
  CDIR       = '--',                   &
  CCOMMENT   = TRIM(YCOMMENT),         &
  NGRID      = tpfields(1)%ngrid,      &
  NTYPE      = TYPECHAR,               &
  NDIMS      = 1,                      &
  LTIMEDEP   = .FALSE.                 )
allocate( ytitles( ip ) )
ytitles(:) = tpfields(1 : ip)%cmnhname
CALL IO_Field_write(tzfile,TZFIELD,ytitles(:))
deallocate( ytitles )
!
! 4eme enregistrement UNITE
!
TZFIELD = TFIELDMETADATA(              &
  CMNHNAME   = TRIM(ygroup)//'.UNITE', &
  CSTDNAME   = '',                     &
  CLONGNAME  = TRIM(ygroup)//'.UNITE', &
  CUNITS     = '',                     &
  CDIR       = '--',                   &
  CCOMMENT   = TRIM(YCOMMENT),         &
  NGRID      = tpfields(1)%ngrid,      &
  NTYPE      = TYPECHAR,               &
  NDIMS      = 1,                      &
  LTIMEDEP   = .FALSE.                 )
allocate( yunits( ip ) )
yunits(:) = tpfields(1 : ip)%cunits
CALL IO_Field_write(tzfile,TZFIELD,yunits(:))
deallocate( yunits )
!
! 5eme enregistrement COMMENT
!
TZFIELD = TFIELDMETADATA(                &
  CMNHNAME   = TRIM(ygroup)//'.COMMENT', &
  CSTDNAME   = '',                       &
  CLONGNAME  = TRIM(ygroup)//'.COMMENT', &
  CUNITS     = '',                       &
  CDIR       = '--',                     &
  CCOMMENT   = TRIM(YCOMMENT),           &
  NGRID      = tpfields(1)%ngrid,        &
  NTYPE      = TYPECHAR,                 &
  NDIMS      = 1,                        &
  LTIMEDEP   = .FALSE.                   )
allocate( ycomments( ip ) )
ycomments(:) = tpfields(1 : ip)%ccomment
CALL IO_Field_write(tzfile,TZFIELD,ycomments(:))
deallocate( ycomments )
!
! 6eme enregistrement PVAR
!
! Dans la mesure ou cette matrice risque d'etre tres volumineuse, on ecrira un
! enregistrement par processus
DO J = 1,IP
  if ( All( tpfields(1)%ndimlist(:) /= NMNHDIM_UNKNOWN ) ) then
    tzfield%ndimlist(1:5) = tpfields(j)%ndimlist(1:5)
    do jj = 1, 5
      if ( tzfield%ndimlist(jj) == NMNHDIM_UNUSED ) then
        tzfield%ndimlist(jj) = NMNHDIM_ONE
      end if
    end do
    if ( tzfield%ndimlist(4) == NMNHDIM_FLYER_TIME ) tzfield%ndimlist(4) = NMNHDIM_NOTLISTED
    tzfield%ndimlist(6:)   = NMNHDIM_UNUSED
  else
    call Print_msg( NVERB_ERROR, 'IO', 'Write_diachro_lfi', &
                    'some dimensions are unknown for variable '//trim(tpfields(1)%cmnhname) )
  end if

  YJ = '   '
  IF(J < 10)WRITE(YJ,'(I1)')J ; YJ = ADJUSTL(YJ)
  IF(J >= 10 .AND. J < 100) THEN
          WRITE(YJ,'(I2)')J ; YJ = ADJUSTL(YJ)
  ELSE IF(J >= 100 .AND. J < 1000) THEN
          WRITE(YJ,'(I3)')J
  ENDIF
  IF ( gdistributed ) THEN
    TZFIELD = TFIELDMETADATA(                 &
      CMNHNAME   = TRIM(ygroup)//'.PROC'//YJ, &
      CSTDNAME   = '',                        &
      CLONGNAME  = TRIM(ygroup)//'.PROC'//YJ, &
      CUNITS     = tpfields(j)%cunits,        &
      CDIR       = 'XY',                      &
      CCOMMENT   = TRIM(tpfields(j)%cmnhname)//' - '//TRIM(tpfields(j)%ccomment)//' ('// Trim( tpfields(j)%cunits ) //')', &
      NGRID      = tpfields(j)%ngrid,         &
      NTYPE      = TYPEREAL,                  &
      NDIMS      = 5,                         &
      LTIMEDEP   = .FALSE.                    )

    CALL IO_Field_write_BOX(tzfile,TZFIELD,'BUDGET',PVAR(:,:,:,:,:,J), &
                            iil+JPHEXT,iih+JPHEXT,ijl+JPHEXT,ijh+JPHEXT)
  ELSE
    TZFIELD = TFIELDMETADATA(                 &
      CMNHNAME   = TRIM(ygroup)//'.PROC'//YJ, &
      CSTDNAME   = '',                        &
      CLONGNAME  = TRIM(ygroup)//'.PROC'//YJ, &
      CUNITS     = tpfields(j)%cunits,        &
      CDIR       = '--',                      &
      CCOMMENT   = TRIM(tpfields(j)%cmnhname)//' - '//TRIM(tpfields(j)%ccomment)//' ('// Trim( tpfields(j)%cunits ) //')', &
      NGRID      = tpfields(j)%ngrid,         &
      NTYPE      = TYPEREAL,                  &
      NDIMS      = 5,                         &
      LTIMEDEP   = .FALSE.                    )

    CALL IO_Field_write(tzfile,TZFIELD,PVAR(:,:,:,:,:,J))
  ENDIF
  tzfield%ndimlist(:)   = NMNHDIM_UNKNOWN
ENDDO
!
! 7eme enregistrement TRAJT
!
TZFIELD = TFIELDMETADATA(              &
  CMNHNAME   = TRIM(ygroup)//'.TRAJT', &
  CSTDNAME   = '',                     &
  CLONGNAME  = TRIM(ygroup)//'.TRAJT', &
  CUNITS     = '',                     &
  CDIR       = '--',                   &
  CCOMMENT   = TRIM(YCOMMENT),         &
  NGRID      = tpfields(1)%ngrid,      &
  NTYPE      = TYPEREAL,               &
  NDIMS      = 2,                      &
  LTIMEDEP   = .FALSE.                 )

!NMNHDIM_FLYER_TIME excluded because created only in netCDF/HDF groups (local to each flyer)
if ( tpfields(1)%ndimlist(4) /= NMNHDIM_UNKNOWN .and. tpfields(1)%ndimlist(4) /= NMNHDIM_UNUSED &
     .and. tpfields(1)%ndimlist(4) /= NMNHDIM_FLYER_TIME ) then
  tzfield%ndimlist(1)  = tpfields(1)%ndimlist(4)
  tzfield%ndimlist(2)  = NMNHDIM_ONE
  tzfield%ndimlist(3:) = NMNHDIM_UNUSED
end if

!Reconstitute old diachro format
allocate( ztimes( size( tpdates ), 1 ) )

do ji=1,size(tpdates)
  call Datetime_distance( tdtexp, tpdates(ji ), ztimes(ji, 1 ) )
end do

call IO_Field_write( tzfile, tzfield, ztimes )

!Reset ndimlist
tzfield%ndimlist(:) = NMNHDIM_UNKNOWN

deallocate( ztimes )
!
! Dans certains cas
!
!
! 8eme enregistrement TRAJX
!
IF(PRESENT(tpflyer))THEN
  TZFIELD = TFIELDMETADATA(              &
    CMNHNAME   = TRIM(ygroup)//'.TRAJX', &
    CSTDNAME   = '',                     &
    CLONGNAME  = TRIM(ygroup)//'.TRAJX', &
    CUNITS     = '',                     &
    CDIR       = '--',                   &
    CCOMMENT   = TRIM(YCOMMENT),         &
    NGRID      = tpfields(1)%ngrid,      &
    NTYPE      = TYPEREAL,               &
    NDIMS      = 3,                      &
    LTIMEDEP   = .FALSE.                 )
  CALL IO_Field_write(tzfile,TZFIELD, Reshape( tpflyer%xx, [1, Size( tpflyer%xx), 1] ) )
ELSE IF ( ycategory == 'LES_budgets' .and.  tpbudiachro%clevels(NLVL_SHAPE) == 'Cartesian' ) THEN
  TZFIELD = TFIELDMETADATA(              &
    CMNHNAME   = TRIM(ygroup)//'.TRAJX', &
    CSTDNAME   = '',                     &
    CLONGNAME  = TRIM(ygroup)//'.TRAJX', &
    CUNITS     = '',                     &
    CDIR       = '--',                   &
    CCOMMENT   = TRIM(YCOMMENT),         &
    NGRID      = tpfields(1)%ngrid,      &
    NTYPE      = TYPEREAL,               &
    NDIMS      = 3,                      &
    LTIMEDEP   = .FALSE.                 )
  !TRAJX is given in extended domain coordinates (=> +jphext) for backward compatibility
  CALL IO_Field_write(tzfile,TZFIELD, Real( Reshape( &
                       Spread( source = ( nles_current_iinf + nles_current_isup) / 2 + jphext, dim = 1, ncopies = IN ), &
                       [1, 1, IN] ) ) )
ENDIF
!
! 9eme enregistrement TRAJY
!
IF(PRESENT(tpflyer))THEN
  TZFIELD = TFIELDMETADATA(              &
    CMNHNAME   = TRIM(ygroup)//'.TRAJY', &
    CSTDNAME   = '',                     &
    CLONGNAME  = TRIM(ygroup)//'.TRAJY', &
    CUNITS     = '',                     &
    CDIR       = '--',                   &
    CCOMMENT   = TRIM(YCOMMENT),         &
    NGRID      = tpfields(1)%ngrid,      &
    NTYPE      = TYPEREAL,               &
    NDIMS      = 3,                      &
    LTIMEDEP   = .FALSE.                 )
  CALL IO_Field_write(tzfile,TZFIELD, Reshape( tpflyer%xy, [1, Size( tpflyer%xy), 1] ) )
ELSE IF ( ycategory == 'LES_budgets' .and.  tpbudiachro%clevels(NLVL_SHAPE) == 'Cartesian' ) THEN
  TZFIELD = TFIELDMETADATA(              &
    CMNHNAME   = TRIM(ygroup)//'.TRAJY', &
    CSTDNAME   = '',                     &
    CLONGNAME  = TRIM(ygroup)//'.TRAJY', &
    CUNITS     = '',                     &
    CDIR       = '--',                   &
    CCOMMENT   = TRIM(YCOMMENT),         &
    NGRID      = tpfields(1)%ngrid,      &
    NTYPE      = TYPEREAL,               &
    NDIMS      = 3,                      &
    LTIMEDEP   = .FALSE.                 )
  !TRAJY is given in extended domain coordinates (=> +jphext) for backward compatibility
  CALL IO_Field_write(tzfile,TZFIELD, Real( Reshape( &
                       Spread( source = ( nles_current_jinf + nles_current_jsup) / 2 + jphext, dim = 1, ncopies = IN ), &
                       [1, 1, IN] ) ) )
ENDIF
!
! 10eme enregistrement TRAJZ
!
IF(PRESENT(tpflyer))THEN
  TZFIELD = TFIELDMETADATA(              &
    CMNHNAME   = TRIM(ygroup)//'.TRAJZ', &
    CSTDNAME   = '',                     &
    CLONGNAME  = TRIM(ygroup)//'.TRAJZ', &
    CUNITS     = '',                     &
    CDIR       = '--',                   &
    CCOMMENT   = TRIM(YCOMMENT),         &
    NGRID      = tpfields(1)%ngrid,      &
    NTYPE      = TYPEREAL,               &
    NDIMS      = 3,                      &
    LTIMEDEP   = .FALSE.                 )
  CALL IO_Field_write(tzfile,TZFIELD, Reshape( tpflyer%xz, [1, Size( tpflyer%xz), 1] ) )
ELSE IF ( ycategory == 'LES_budgets' .and.  tpbudiachro%clevels(NLVL_SHAPE) == 'Cartesian' ) THEN
  TZFIELD = TFIELDMETADATA(              &
    CMNHNAME   = TRIM(ygroup)//'.TRAJZ', &
    CSTDNAME   = '',                     &
    CLONGNAME  = TRIM(ygroup)//'.TRAJZ', &
    CUNITS     = '',                     &
    CDIR       = '--',                   &
    CCOMMENT   = TRIM(YCOMMENT),         &
    NGRID      = tpfields(1)%ngrid,      &
    NTYPE      = TYPEREAL,               &
    NDIMS      = 3,                      &
    LTIMEDEP   = .FALSE.                 )

  Allocate( ztrajz(IK, 1, IN) )
  do jj = 1, IK
    ztrajz(jj, :, :) = xles_current_z(jj)
  end do
  CALL IO_Field_write(tzfile,TZFIELD,ztrajz)
  Deallocate( ztrajz )
ENDIF
!
! 11eme enregistrement PDATIME
!
TZFIELD = TFIELDMETADATA(              &
  CMNHNAME   = TRIM(ygroup)//'.DATIM', &
  CSTDNAME   = '',                     &
  CLONGNAME  = TRIM(ygroup)//'.DATIM', &
  CUNITS     = '',                     &
  CDIR       = '--',                   &
  CCOMMENT   = TRIM(YCOMMENT),         &
  NGRID      = tpfields(1)%ngrid,      &
  NTYPE      = TYPEREAL,               &
  NDIMS      = 2,                      &
  LTIMEDEP   = .FALSE.                 )

!Reconstitute old diachro format
allocate( zdatime( 16, size(tpdates) ) )

zdatime(1,  : ) = tdtexp%nyear
zdatime(2,  : ) = tdtexp%nmonth
zdatime(3,  : ) = tdtexp%nday
zdatime(4,  : ) = tdtexp%xtime
zdatime(5,  : ) = tdtseg%nyear
zdatime(6,  : ) = tdtseg%nmonth
zdatime(7,  : ) = tdtseg%nday
zdatime(8,  : ) = tdtseg%xtime
zdatime(9,  : ) = tdtmod%nyear
zdatime(10, : ) = tdtmod%nmonth
zdatime(11, : ) = tdtmod%nday
zdatime(12, : ) = tdtmod%xtime
zdatime(13, : ) = tpdates(:)%nyear
zdatime(14, : ) = tpdates(:)%nmonth
zdatime(15, : ) = tpdates(:)%nday
zdatime(16, : ) = tpdates(:)%xtime

call IO_Field_write( tzfile, tzfield, zdatime )

deallocate( zdatime )

call Menu_diachro( tzfile, ygroup )

end subroutine Write_diachro_lfi
#endif

#ifdef MNH_IOCDF4
!-----------------------------------------------------------------------------
subroutine Write_diachro_nc4( tpdiafile, tpbudiachro, tpfields, pvar, osplit, tpflyer )

use NETCDF,                only: NF90_DEF_DIM, NF90_INQ_DIMID, NF90_INQUIRE_DIMENSION, NF90_NOERR

use modd_aircraft_balloon, only: tflyerdata
use modd_budget,           only: CNCGROUPNAMES,                                                      &
                                 NMAXLEVELS, NLVL_ROOT, NLVL_CATEGORY, NLVL_SUBCATEGORY, NLVL_GROUP, &
                                 NLVL_SHAPE, NLVL_TIMEAVG, NLVL_NORM, NLVL_MASK,                     &
                                 nbutshift, nbusubwrite, tbudiachrometadata
use modd_conf,             only: lcartesian
use modd_field
use modd_io,               only: isp, tfiledata
use modd_les,              only: cbl_height_def, cles_norm_type, nles_masks, xles_temp_sampling
use modd_parameters,       only: jphext, NBUNAMELGTMAX, NCOMMENTLGTMAX
use modd_precision,        only: CDFINT, MNHREAL_NF90
use modd_type_date,        only: date_time

use mode_io_field_write,   only: IO_Field_create, IO_Field_write, IO_Field_write_box
use mode_io_tools_nc4,     only: IO_Err_handle_nc4

type(tfiledata),                                    intent(in)           :: tpdiafile        ! File to write
type(tbudiachrometadata),                           intent(in)           :: tpbudiachro
class(tfieldmetadata_base), dimension(:),           intent(in)           :: tpfields
real,                       dimension(:,:,:,:,:,:), intent(in)           :: pvar
logical,                                            intent(in), optional :: osplit
class(tflyerdata),                                  intent(in), optional :: tpflyer

character(len=:), allocatable :: ycategory
character(len=:), allocatable :: ylevelname
character(len=:), allocatable :: ylevels
character(len=:), allocatable :: yshape
character(len=:), allocatable :: ystdnameprefix
integer                                       :: iil, iih, ijl, ijh, ikl, ikh
integer                                       :: idims
integer                                       :: icount
integer                                       :: icorr
integer                                       :: ji
integer                                       :: jl
integer                                       :: jp
integer(kind=CDFINT)                          :: idimid
integer(kind=CDFINT)                          :: ilen
integer(kind=CDFINT)                          :: istatus
integer(kind=CDFINT)                          :: ilevelid
integer(kind=CDFINT), dimension(0:NMAXLEVELS) :: ilevelids ! ids of the different groups/levels in the netCDF file
logical                                       :: gdistributed
logical                                       :: gsplit
logical,              dimension(0:NMAXLEVELS) :: gleveldefined ! Are the different groups/levels already defined in the netCDF file
type(tfieldmetadata)                          :: tzfield
type(tfiledata)                               :: tzfile

call Print_msg( NVERB_DEBUG, 'BUD', 'Write_diachro_nc4', 'called' )

tzfile = tpdiafile

!Write only in netCDF files
tzfile%cformat = 'NETCDF4'

ycategory = Trim( tpbudiachro%clevels(NLVL_CATEGORY)  )
yshape    = Trim( tpbudiachro%clevels(NLVL_SHAPE) )

iil = tpbudiachro%nil
iih = tpbudiachro%nih
ijl = tpbudiachro%njl
ijh = tpbudiachro%njh
ikl = tpbudiachro%nkl
ikh = tpbudiachro%nkh

if ( ycategory == 'Budgets' .and. yshape == 'Cartesian' &
     .and. .not. tpbudiachro%licompress .and. .not. tpbudiachro%ljcompress ) then
  gdistributed = .true.
else
  !By default data is already collected on the write process for budgets
  gdistributed = .false.
end if

if ( Present( osplit ) ) then
  gsplit = osplit
else
  gsplit = .false.
end if

MASTER: if ( isp == tzfile%nmaster_rank) then
  ilevelids(NLVL_ROOT) = tzfile%nncid

  gleveldefined(:) = .false.

  do jl = 1, NMAXLEVELS
    call Move_to_next_level( ilevelids(jl-1), gleveldefined(jl-1), tpbudiachro%lleveluse(jl), &
                           tpbudiachro%clevels(jl), gleveldefined(jl), ilevelids(jl) )
  end do

  tzfile%nncid = ilevelids(NLVL_MASK)

  ylevels = ''

  do jl = NMAXLEVELS, 1, -1
    ylevels = Trim( CNCGROUPNAMES(jl) ) // ' ' // ylevels
    if ( tpbudiachro%lleveluse(jl) ) then
      call Att_write( tpbudiachro%clevels(jl), ilevelids(jl), 'levels', Trim( ylevels ) )
      ylevels = ''
    end if
  end do

  if ( .not. gleveldefined(NLVL_CATEGORY) ) then
    ylevelname = tpbudiachro%clevels(NLVL_CATEGORY)
    ilevelid   = ilevelids  (NLVL_CATEGORY)

    call Att_write( ylevelname, ilevelid, 'category', ylevelname )
    if ( tpbudiachro%lleveluse(NLVL_CATEGORY) .and. Len_trim( tpbudiachro%ccomments(NLVL_CATEGORY) ) > 0 ) &
    call Att_write( ylevelname, ilevelid, 'comment',  tpbudiachro%ccomments(NLVL_CATEGORY) )

    if ( ycategory == 'LES_budgets' ) &
    call Att_write( ylevelname, ilevelid, 'temporal_sampling_frequency', xles_temp_sampling )
  end if

  if ( .not. gleveldefined(NLVL_SUBCATEGORY) ) then
    ylevelname = tpbudiachro%clevels(NLVL_SUBCATEGORY)
    ilevelid   = ilevelids  (NLVL_SUBCATEGORY)

    call Att_write( ylevelname, ilevelid, 'subcategory', ylevelname )
    if ( tpbudiachro%lleveluse(NLVL_SUBCATEGORY) .and. Len_trim( tpbudiachro%ccomments(NLVL_SUBCATEGORY) ) > 0 ) &
    call Att_write( ylevelname, ilevelid, 'comment',     tpbudiachro%ccomments(NLVL_SUBCATEGORY) )
  end if

  if ( .not. gleveldefined(NLVL_GROUP) ) then
    ylevelname = tpbudiachro%clevels(NLVL_GROUP)
    ilevelid   = ilevelids  (NLVL_GROUP)

    call Att_write( ylevelname, ilevelid, 'group',   ylevelname )
    if ( tpbudiachro%lleveluse(NLVL_GROUP) .and. Len_trim( tpbudiachro%ccomments(NLVL_GROUP) ) > 0 ) &
    call Att_write( ylevelname, ilevelid, 'comment', tpbudiachro%ccomments(NLVL_GROUP) )
  end if

  if ( .not. gleveldefined(NLVL_SHAPE) ) then
    ylevelname = tpbudiachro%clevels(NLVL_SHAPE)
    ilevelid   = ilevelids  (NLVL_SHAPE)

    call Att_write( ylevelname, ilevelid, 'shape',   ylevelname )
    if ( tpbudiachro%lleveluse(NLVL_SHAPE) .and. Len_trim( tpbudiachro%ccomments(NLVL_SHAPE) ) > 0 ) &
    call Att_write( ylevelname, ilevelid, 'comment', tpbudiachro%ccomments(NLVL_SHAPE) )

    call Att_write( ylevelname, ilevelid, 'moving', Merge( 'yes', 'no ', tpbudiachro%lmobile ) )

    if (      ( ycategory == 'Budgets' .and. yshape == 'Cartesian' )             &
         .or. ycategory == 'LES_budgets'                                         &
         .or. tpbudiachro%clevels(NLVL_GROUP)      == 'TSERIES'                  &
         .or. tpbudiachro%clevels(NLVL_GROUP)      == 'ZTSERIES'                 &
         .or. tpbudiachro%clevels(NLVL_GROUP)(1:8) == 'XTSERIES'                 ) then
      call Att_write( ylevelname, ilevelid, 'min_I_index_in_physical_domain', iil )
      call Att_write( ylevelname, ilevelid, 'max_I_index_in_physical_domain', iih )
      call Att_write( ylevelname, ilevelid, 'min_J_index_in_physical_domain', ijl )
      call Att_write( ylevelname, ilevelid, 'max_J_index_in_physical_domain', ijh )
    end if

    if (      ( ycategory == 'Budgets' .and. yshape == 'Cartesian' )           &
         .or. tpbudiachro%clevels(NLVL_GROUP)      == 'TSERIES'                &
         .or. tpbudiachro%clevels(NLVL_GROUP)      == 'ZTSERIES'               &
         .or. tpbudiachro%clevels(NLVL_GROUP)(1:8) == 'XTSERIES'               ) then
      !Disabled for LES_budgets because no real meaning on that case (vertical levels are stored in the level_les variable)
      call Att_write( ylevelname, ilevelid, 'min_K_index_in_physical_domain', ikl )
      call Att_write( ylevelname, ilevelid, 'max_K_index_in_physical_domain', ikh )
    end if


    if (      ( ycategory == 'Budgets' .and. yshape == 'Cartesian' )           &
         .or. ( ycategory == 'LES_budgets'    .and. yshape == 'Cartesian' )    &
         .or. tpbudiachro%clevels(NLVL_GROUP)      == 'TSERIES'                &
         .or. tpbudiachro%clevels(NLVL_GROUP)      == 'ZTSERIES'               &
         .or. tpbudiachro%clevels(NLVL_GROUP)(1:8) == 'XTSERIES'               ) then
      call Att_write( ylevelname, ilevelid, &
                      'averaged_in_the_I_direction', Merge( 'yes', 'no ', tpbudiachro%licompress ) )
      call Att_write( ylevelname, ilevelid, &
                      'averaged_in_the_J_direction', Merge( 'yes', 'no ', tpbudiachro%ljcompress ) )
      call Att_write( ylevelname, ilevelid, &
                      'averaged_in_the_K_direction', Merge( 'yes', 'no ', tpbudiachro%lkcompress ) )
    end if
  end if

  if ( .not. gleveldefined(NLVL_TIMEAVG) ) then
    ylevelname = tpbudiachro%clevels(NLVL_TIMEAVG)
    ilevelid   = ilevelids  (NLVL_TIMEAVG)

    if ( tpbudiachro%lleveluse(NLVL_TIMEAVG) .and. Len_trim( tpbudiachro%ccomments(NLVL_TIMEAVG) ) > 0 ) &
    call Att_write( ylevelname, ilevelid, 'comment',        tpbudiachro%ccomments(NLVL_TIMEAVG) )

    call Att_write( ylevelname, ilevelid, 'time_averaged', Merge( 'yes', 'no ', tpbudiachro%ltcompress ) )
  end if

  if ( .not. gleveldefined(NLVL_NORM) ) then
    ylevelname = tpbudiachro%clevels(NLVL_NORM)
    ilevelid   = ilevelids  (NLVL_NORM)

    if ( tpbudiachro%lleveluse(NLVL_NORM) .and. Len_trim( tpbudiachro%ccomments(NLVL_NORM) ) > 0 ) &
    call Att_write( ylevelname, ilevelid, 'comment',   tpbudiachro%ccomments(NLVL_NORM) )

    call Att_write( ylevelname, ilevelid, 'normalized', Merge( 'yes', 'no ', tpbudiachro%lnorm ) )

    if ( ycategory == 'LES_budgets' .and. yshape == 'Cartesian' ) then
      if ( tpbudiachro%lnorm ) then
        if ( cles_norm_type == 'NONE' ) then
          call Att_write( ylevelname, ilevelid, 'normalization', 'none' )
        else if ( cles_norm_type == 'CONV' ) then
          call Att_write( ylevelname, ilevelid, 'normalization', 'convective' )
          ! cbl_height_def determines how the boundary layer height is computed, which is used in this normalization
          call Att_write( ylevelname, ilevelid, 'definition_of_boundary_layer_height', cbl_height_def )
        else if ( cles_norm_type == 'EKMA' ) then
          call Att_write( ylevelname, ilevelid, 'normalization', 'Ekman' )
          ! cbl_height_def determines how the boundary layer height is computed, which is used in this normalization
          call Att_write( ylevelname, ilevelid, 'definition_of_boundary_layer_height', cbl_height_def )
        else if ( cles_norm_type == 'MOBU' ) then
          call Att_write( ylevelname, ilevelid, 'normalization', 'Monin-Obukhov' )
        else
          call Print_msg( NVERB_WARNING, 'IO', 'Write_diachro_nc4', Trim( tzfile%cname ) // &
                          ': group ' // Trim( tpbudiachro%clevels(NLVL_GROUP) ) // ': unknown normalization' )
          call Att_write( ylevelname, ilevelid, 'normalization', 'unknown' )
        end if
      else
      call Att_write( ylevelname, ilevelid, 'normalization', 'none' )
      end if
    end if
  end if

  if ( .not. gleveldefined(NLVL_MASK) ) then
    ylevelname = tpbudiachro%clevels(NLVL_MASK)
    ilevelid   = ilevelids  (NLVL_MASK)

    call Att_write( ylevelname, ilevelid, 'mask',    ylevelname )
    if ( tpbudiachro%lleveluse(NLVL_MASK) .and. Len_trim( tpbudiachro%ccomments(NLVL_MASK) ) > 0 ) &
    call Att_write( ylevelname, ilevelid, 'comment', tpbudiachro%ccomments(NLVL_MASK) )

    if ( ycategory == 'Budgets' .and. yshape == 'Mask' ) &
    call Att_write( ylevelname, ilevelid, 'masks_are_stored_in_variable', tpbudiachro%clevels(NLVL_MASK) )
  end if

end if MASTER

!Determine the number of dimensions and do some verifications
do jp = 1, Size( tpfields )
  if ( Any( tpfields(jp)%ndimlist(:) == NMNHDIM_UNKNOWN ) ) &
    call Print_msg( NVERB_ERROR, 'IO', 'Write_diachro_nc4', &
                    'some dimensions are unknown for variable '//trim(tpfields(jp)%cmnhname) )

  icount = Count( tpfields(jp)%ndimlist(:) /= NMNHDIM_UNUSED )

  if ( tpfields(jp)%ndims /= icount ) &
    call Print_msg( NVERB_ERROR, 'IO', 'Write_diachro_nc4', &
                    'ndims is not coherent with ndimlist for variable '//trim(tpfields(jp)%cmnhname) )

  if ( jp == 1 ) then
    idims = icount
  else
    if ( idims /= icount ) &
      call Print_msg( NVERB_ERROR, 'IO', 'Write_diachro_nc4', &
                      'number of dimensions not the same for all tpfields for variable '//trim(tpfields(jp)%cmnhname) )
  end if
end do

!The dimension list should be the same for all tpfields entries
do jp = 2, Size( tpfields )
  do ji = 1, NMNHMAXDIMS
    if ( tpfields(jp)%ndimlist(ji) /= tpfields(1)%ndimlist(ji) ) then
      !For SERIES: it is possible to have NMNHDIM_NI and NMNHDIM_NI_U in the different tpfields
      !For SERIES: it is possible to have NMNHDIM_SERIES_LEVEL and NMNHDIM_SERIES_LEVEL_W in the different tpfields
      !For profiles: it is possible to have NMNHDIM_LEVEL and NMNHDIM_LEVEL_W in the different tpfields
      !This check is not perfect but should catch most problems
      if ( tpfields(jp)%ndimlist(ji) /= NMNHDIM_NI           .and. tpfields(jp)%ndimlist(ji) /= NMNHDIM_NI_U    .and.  &
           tpfields(jp)%ndimlist(ji) /= NMNHDIM_LEVEL        .and. tpfields(jp)%ndimlist(ji) /= NMNHDIM_LEVEL_W .and.  &
           tpfields(jp)%ndimlist(ji) /= NMNHDIM_SERIES_LEVEL .and. tpfields(jp)%ndimlist(ji) /= NMNHDIM_SERIES_LEVEL_W ) then
        call Print_msg( NVERB_ERROR, 'IO', 'Write_diachro_nc4', &
                        'some dimensions are not the same for all tpfields entries for variable '//trim(tpfields(jp)%cmnhname) )
      end if
    end if
  end do
end do

!Check that if cartesian and no horizontal compression, parameters are as expected
if ( yshape == 'Cartesian' .and. .not. tpbudiachro%licompress .and. .not. tpbudiachro%ljcompress ) then
  icorr = Merge( 1, 0, tpbudiachro%lkcompress )
  if ( ( idims + icorr ) /= 3 .and. ( idims + icorr ) /= 4 ) then
    call Print_msg( NVERB_ERROR, 'IO', 'Write_diachro_nc4',                                                            &
                    'unexpected number of dimensions for cartesian shape without horizontal compression for variable ' &
                    // Trim( tpfields(1)%cmnhname ) )
  end if

  if (      (       tpfields(1)%ndimlist(1) /= NMNHDIM_BUDGET_CART_NI              &
              .and. tpfields(1)%ndimlist(1) /= NMNHDIM_BUDGET_CART_NI_U            &
              .and. tpfields(1)%ndimlist(1) /= NMNHDIM_BUDGET_CART_NI_V          ) &
       .or. (       tpfields(1)%ndimlist(2) /= NMNHDIM_BUDGET_CART_NJ              &
              .and. tpfields(1)%ndimlist(2) /= NMNHDIM_BUDGET_CART_NJ_U            &
              .and. tpfields(1)%ndimlist(2) /= NMNHDIM_BUDGET_CART_NJ_V          ) &
       .or. (       .not. tpbudiachro%lkcompress                                                     &
              .and. tpfields(1)%ndimlist(3) /= NMNHDIM_BUDGET_CART_LEVEL           &
              .and. tpfields(1)%ndimlist(3) /= NMNHDIM_BUDGET_CART_LEVEL_W       ) &
       .or. ( ( idims + icorr ) == 4 .and. tpfields(1)%ndimlist(6) /= NMNHDIM_BUDGET_NGROUPS ) ) then
    call Print_msg( NVERB_ERROR, 'IO', 'Write_diachro_nc4',                                       &
                    'unexpected dimensions for CART without horizontal compression for variable ' &
                    // Trim( tpfields(1)%cmnhname ) )
  end if
end if


select case ( idims )
  case (0)
     !Remark: [ integer:: ] is a constructor for a zero-size array of integers, [] is not allowed (type can not be determined)
      call Diachro_one_field_write_nc4( tzfile, tpbudiachro, tpfields(1), pvar, [ integer:: ], gsplit, gdistributed )

  case (1)

    if ( Any ( tpfields(1)%ndimlist(4) == [ NMNHDIM_BUDGET_LES_TIME, NMNHDIM_BUDGET_LES_AVG_TIME, NMNHDIM_SERIES_TIME ] ) ) then
      if ( Size( tpfields ) /= 1 ) call Print_msg( NVERB_FATAL, 'IO', 'Write_diachro_nc4', &
                                                   'wrong size of tpfields (variable '//trim(tpfields(1)%cmnhname)//')' )

      call Diachro_one_field_write_nc4( tzfile, tpbudiachro, tpfields(1), pvar, [ 4 ], gsplit, gdistributed )
    else if ( Any( tpfields(1)%ndimlist(1) == [ NMNHDIM_NI, NMNHDIM_NI_U, NMNHDIM_NI_V, NMNHDIM_BUDGET_CART_NI,     &
                                                NMNHDIM_BUDGET_CART_NI_U, NMNHDIM_BUDGET_CART_NI_V              ] ) ) then
      if ( Size( tpfields ) /= 1 ) call Print_msg( NVERB_FATAL, 'IO', 'Write_diachro_nc4', &
                                                   'wrong size of tpfields (variable '//trim(tpfields(1)%cmnhname)//')' )

      call Diachro_one_field_write_nc4( tzfile, tpbudiachro, tpfields(1), pvar, [ 1 ], gsplit, gdistributed )

    else if ( Any( tpfields(1)%ndimlist(2) == [ NMNHDIM_NJ, NMNHDIM_NJ_U, NMNHDIM_NJ_V, NMNHDIM_BUDGET_CART_NJ,     &
                                                NMNHDIM_BUDGET_CART_NJ_U, NMNHDIM_BUDGET_CART_NJ_V              ] ) ) then
      if ( Size( tpfields ) /= 1 ) call Print_msg( NVERB_FATAL, 'IO', 'Write_diachro_nc4', &
                                                   'wrong size of tpfields (variable '//trim(tpfields(1)%cmnhname)//')' )
      call Diachro_one_field_write_nc4( tzfile, tpbudiachro, tpfields(1), pvar, [ 2 ], gsplit, gdistributed )
    else if ( Any( tpfields(1)%ndimlist(3) == [ NMNHDIM_LEVEL, NMNHDIM_LEVEL_W,                            &
                                                NMNHDIM_BUDGET_CART_LEVEL, NMNHDIM_BUDGET_CART_LEVEL_W ] ) ) then
      if ( Size( tpfields ) /= 1 ) call Print_msg( NVERB_FATAL, 'IO', 'Write_diachro_nc4', &
                                                   'wrong size of tpfields (variable '//trim(tpfields(1)%cmnhname)//')' )
      call Diachro_one_field_write_nc4( tzfile, tpbudiachro, tpfields(1), pvar, [ 3 ], gsplit, gdistributed )
    else if ( tpfields(1)%ndimlist(6) == NMNHDIM_BUDGET_NGROUPS .or. tpfields(1)%ndimlist(6) == NMNHDIM_PROFILER_PROC ) then
      do ji = 1, Size( pvar, 6 )
        !Remark: [ integer:: ] is a constructor for a zero-size array of integers, [] is not allowed (type can not be determined)
        call Diachro_one_field_write_nc4( tzfile, tpbudiachro, tpfields(ji), pvar(:,:,:,:,:,ji:ji), [ integer:: ], &
                                          gsplit, gdistributed )
      end do
    else
      call Print_msg( NVERB_FATAL, 'IO', 'Write_diachro_nc4', &
                      'case not yet implemented (1D variable '//trim(tpfields(1)%cmnhname)//')' )
    end if


  case (2)

    if (       Any( tpfields(1)%ndimlist(1) == [ NMNHDIM_NI, NMNHDIM_NI_U, NMNHDIM_NI_V, NMNHDIM_BUDGET_CART_NI, &
                                                 NMNHDIM_BUDGET_CART_NI_U, NMNHDIM_BUDGET_CART_NI_V ] )          &
         .and. Any( tpfields(1)%ndimlist(2) == [ NMNHDIM_NJ, NMNHDIM_NJ_U, NMNHDIM_NJ_V, NMNHDIM_BUDGET_CART_NJ, &
                                                 NMNHDIM_BUDGET_CART_NJ_U, NMNHDIM_BUDGET_CART_NJ_V ] )          ) then
      if ( Size( tpfields ) /= 1 ) call Print_msg( NVERB_FATAL, 'IO', 'Write_diachro_nc4', &
                                                   'wrong size of tpfields (variable '//trim(tpfields(1)%cmnhname)//')' )
      call Diachro_one_field_write_nc4( tzfile, tpbudiachro, tpfields(1), pvar, [ 1, 2 ], gsplit, gdistributed, &
                                        iil, iih, ijl, ijh, ikl, ikh )
    else if (       Any( tpfields(1)%ndimlist(1) == [ NMNHDIM_NI, NMNHDIM_NI_U, NMNHDIM_NI_V, NMNHDIM_BUDGET_CART_NI, &
                                                      NMNHDIM_BUDGET_CART_NI_U, NMNHDIM_BUDGET_CART_NI_V ] )          &
              .and. Any( tpfields(1)%ndimlist(3) == [ NMNHDIM_LEVEL, NMNHDIM_LEVEL_W,                                 &
                                                      NMNHDIM_BUDGET_CART_LEVEL, NMNHDIM_BUDGET_CART_LEVEL_W ] )      ) then
      if ( Size( tpfields ) /= 1 ) call Print_msg( NVERB_FATAL, 'IO', 'Write_diachro_nc4', &
                                                   'wrong size of tpfields (variable '//trim(tpfields(1)%cmnhname)//')' )
      call Diachro_one_field_write_nc4( tzfile, tpbudiachro, tpfields(1), pvar, [ 1, 3 ], gsplit, gdistributed )
    else if (       Any( tpfields(1)%ndimlist(2) == [ NMNHDIM_NJ, NMNHDIM_NJ_U, NMNHDIM_NJ_V, NMNHDIM_BUDGET_CART_NJ, &
                                                      NMNHDIM_BUDGET_CART_NJ_U, NMNHDIM_BUDGET_CART_NJ_V ] )          &
              .and. Any( tpfields(1)%ndimlist(3) == [ NMNHDIM_LEVEL, NMNHDIM_LEVEL_W,                                 &
                                                      NMNHDIM_BUDGET_CART_LEVEL, NMNHDIM_BUDGET_CART_LEVEL_W ] )      ) then
      if ( Size( tpfields ) /= 1 ) call Print_msg( NVERB_FATAL, 'IO', 'Write_diachro_nc4', &
                                                   'wrong size of tpfields (variable '//trim(tpfields(1)%cmnhname)//')' )
      call Diachro_one_field_write_nc4( tzfile, tpbudiachro, tpfields(1), pvar, [ 2, 3 ], gsplit, gdistributed )
    else if (  Any( tpfields(1)%ndimlist(1) == [ NMNHDIM_BUDGET_CART_NI, NMNHDIM_BUDGET_CART_NI_U, NMNHDIM_BUDGET_CART_NI_V ] ) &
              .and. tpfields(1)%ndimlist(6) == NMNHDIM_BUDGET_NGROUPS ) then
      ! Loop on the processes
      do ji = 1, Size( pvar, 6 )
        call Diachro_one_field_write_nc4( tzfile, tpbudiachro, tpfields(ji), pvar(:,:,:,:,:,ji:ji), [ 1 ], gsplit, gdistributed )
      end do
    else if (  Any( tpfields(1)%ndimlist(2) == [ NMNHDIM_BUDGET_CART_NJ, NMNHDIM_BUDGET_CART_NJ_U, NMNHDIM_BUDGET_CART_NJ_V ] ) &
              .and. tpfields(1)%ndimlist(6) == NMNHDIM_BUDGET_NGROUPS ) then
      ! Loop on the processes
      do ji = 1, Size( pvar, 6 )
        call Diachro_one_field_write_nc4( tzfile, tpbudiachro, tpfields(ji), pvar(:,:,:,:,:,ji:ji), [ 2 ], gsplit, gdistributed )
      end do
    else if (       tpfields(1)%ndimlist(3) == NMNHDIM_BUDGET_LES_LEVEL     &
       .and. (      tpfields(1)%ndimlist(4) == NMNHDIM_BUDGET_LES_TIME &
               .or. tpfields(1)%ndimlist(4) == NMNHDIM_BUDGET_LES_AVG_TIME ) ) then
      if ( Size( tpfields ) /= 1 ) call Print_msg( NVERB_FATAL, 'IO', 'Write_diachro_nc4', &
                                                   'wrong size of tpfields (variable '//trim(tpfields(1)%cmnhname)//')' )
      call Diachro_one_field_write_nc4( tzfile, tpbudiachro, tpfields(1), pvar, [ 3, 4 ], gsplit, gdistributed )
    else if ( Any( tpfields(1)%ndimlist(3) == [ NMNHDIM_BUDGET_CART_LEVEL, NMNHDIM_BUDGET_CART_LEVEL_W ] ) &
              .and. tpfields(1)%ndimlist(6) == NMNHDIM_BUDGET_NGROUPS ) then
      ! Loop on the processes
      do ji = 1, Size( pvar, 6 )
        call Diachro_one_field_write_nc4( tzfile, tpbudiachro, tpfields(ji), pvar(:,:,:,:,:,ji:ji), [ 3 ], gsplit, gdistributed )
      end do
    else if (  tpfields(1)%ndimlist(4) == NMNHDIM_BUDGET_TIME .and. tpfields(1)%ndimlist(5) == NMNHDIM_BUDGET_MASK_NBUMASK ) then
      if ( Size( tpfields ) /= 1 ) call Print_msg( NVERB_FATAL, 'IO', 'Write_diachro_nc4', &
                                                   'wrong size of tpfields (variable '//trim(tpfields(1)%cmnhname)//')' )
      call Diachro_one_field_write_nc4( tzfile, tpbudiachro, tpfields(1), pvar, [ 4, 5 ], gsplit, gdistributed )
    else if (  (      tpfields(1)%ndimlist(4) == NMNHDIM_BUDGET_LES_TIME &
                 .or. tpfields(1)%ndimlist(4) == NMNHDIM_BUDGET_LES_AVG_TIME ) &
         .and. tpfields(1)%ndimlist(5) == NMNHDIM_BUDGET_LES_SV       ) then
      if ( Size( tpfields ) /= 1 ) call Print_msg( NVERB_FATAL, 'IO', 'Write_diachro_nc4', &
                                                   'wrong size of tpfields (variable '//trim(tpfields(1)%cmnhname)//')' )
      call Diachro_one_field_write_nc4( tzfile, tpbudiachro, tpfields(1), pvar, [ 4, 5 ], gsplit, gdistributed )
    else if (  tpfields(1)%ndimlist(4) == NMNHDIM_FLYER_TIME &
         .and. tpfields(1)%ndimlist(6) == NMNHDIM_FLYER_PROC ) then
      !Correspond to FLYER_DIACHRO
      !Create local time dimension
      if ( isp == tzfile%nmaster_rank) then
        istatus = NF90_INQ_DIMID( ilevelids(NLVL_GROUP), 'time_flyer', idimid )
        if ( istatus == NF90_NOERR ) then
          !Dimension already exists, check that it is not changed
          istatus = NF90_INQUIRE_DIMENSION( ilevelids(NLVL_GROUP), idimid, len = ilen )
          if ( ilen /= Int( Size( pvar, 4), kind = CDFINT ) ) &
            call Print_msg( NVERB_FATAL, 'IO', 'Write_diachro_nc4', 'time_flyer dimension has changed' )
        else
          !Dimension does not exist yet, create it
          istatus = NF90_DEF_DIM( ilevelids(NLVL_GROUP), 'time_flyer', Int( Size( pvar, 4), kind = CDFINT ), idimid )
          if ( istatus /= NF90_NOERR ) &
            call IO_Err_handle_nc4( istatus, 'Write_diachro_nc4', 'NF90_DEF_DIM', Trim( tpfields(1)%cmnhname ) )
        end if
      end if

      ! Loop on the processes
      do ji = 1, Size( pvar, 6 )
        call Diachro_one_field_write_nc4( tzfile, tpbudiachro, tpfields(ji), pvar(:,:,:,:,:,ji:ji), [ 4 ], gsplit, gdistributed )
      end do
    else if (  tpfields(1)%ndimlist(4) == NMNHDIM_PROFILER_TIME &
         .and. tpfields(1)%ndimlist(6) == NMNHDIM_PROFILER_PROC ) then
      !Correspond to WRITE_PROFILER_n
      ! Loop on the processes
      do ji = 1, Size( pvar, 6 )
        call Diachro_one_field_write_nc4( tzfile, tpbudiachro, tpfields(ji), pvar(:,:,:,:,:,ji:ji), [ 4 ], gsplit, gdistributed )
      end do
    else if (  tpfields(1)%ndimlist(4) == NMNHDIM_SERIES_TIME &
         .and. tpfields(1)%ndimlist(6) == NMNHDIM_SERIES_PROC ) then
      !Correspond to WRITE_SERIES_n
      ! Loop on the processes
      do ji = 1, Size( pvar, 6 )
        call Diachro_one_field_write_nc4( tzfile, tpbudiachro, tpfields(ji), pvar(:,:,:,:,:,ji:ji), [ 4 ], gsplit, gdistributed )
      end do
    else if (  ( tpfields(1)%ndimlist(3) == NMNHDIM_SERIES_LEVEL .or. tpfields(1)%ndimlist(3) == NMNHDIM_SERIES_LEVEL_W ) &
         .and. tpfields(1)%ndimlist(4) == NMNHDIM_SERIES_TIME ) then
      !Correspond to WRITE_SERIES_n
      if ( Size( tpfields ) /= 1 ) call Print_msg( NVERB_FATAL, 'IO', 'Write_diachro_nc4', &
                                                   'wrong size of tpfields (variable '//trim(tpfields(1)%cmnhname)//')' )
      call Diachro_one_field_write_nc4( tzfile, tpbudiachro, tpfields(1), pvar(:,:,:,:,:,:), [ 3, 4 ], gsplit, gdistributed )
    else if (  tpfields(1)%ndimlist(4) == NMNHDIM_STATION_TIME &
         .and. tpfields(1)%ndimlist(6) == NMNHDIM_STATION_PROC ) then
      !Correspond to WRITE_STATION_n
      ! Loop on the processes
      do ji = 1, Size( pvar, 6 )
        call Diachro_one_field_write_nc4( tzfile, tpbudiachro, tpfields(ji), pvar(:,:,:,:,:,ji:ji), [ 4 ], gsplit, gdistributed )
      end do
    else
      call Print_msg( NVERB_FATAL, 'IO', 'Write_diachro_nc4', &
                      'case not yet implemented (2D variable '//trim(tpfields(1)%cmnhname)//')' )
    end if


  case (3)

    if (       Any( tpfields(1)%ndimlist(1) == [ NMNHDIM_NI, NMNHDIM_NI_U, NMNHDIM_NI_V, NMNHDIM_BUDGET_CART_NI, &
                                                 NMNHDIM_BUDGET_CART_NI_U, NMNHDIM_BUDGET_CART_NI_V ] )          &
         .and. Any( tpfields(1)%ndimlist(2) == [ NMNHDIM_NJ, NMNHDIM_NJ_U, NMNHDIM_NJ_V, NMNHDIM_BUDGET_CART_NJ, &
                                                 NMNHDIM_BUDGET_CART_NJ_U, NMNHDIM_BUDGET_CART_NJ_V ] )          &
         .and. Any( tpfields(1)%ndimlist(3) == [ NMNHDIM_LEVEL, NMNHDIM_LEVEL_W,                                 &
                                                 NMNHDIM_BUDGET_CART_LEVEL, NMNHDIM_BUDGET_CART_LEVEL_W ] )      ) then
      if ( Size( tpfields ) /= 1 ) call Print_msg( NVERB_FATAL, 'IO', 'Write_diachro_nc4', &
                                                   'wrong size of tpfields (variable '//trim(tpfields(1)%cmnhname)//')' )
      call Diachro_one_field_write_nc4( tzfile, tpbudiachro, tpfields(1), pvar, [ 1, 2, 3 ], gsplit, gdistributed, &
                                        iil, iih, ijl, ijh, ikl, ikh )
    else if (       Any(tpfields(1)%ndimlist(1) == [ NMNHDIM_BUDGET_CART_NI, NMNHDIM_BUDGET_CART_NI_U, NMNHDIM_BUDGET_CART_NI_V ]) &
              .and. Any(tpfields(1)%ndimlist(2) == [ NMNHDIM_BUDGET_CART_NJ, NMNHDIM_BUDGET_CART_NJ_U, NMNHDIM_BUDGET_CART_NJ_V ]) &
              .and.     tpfields(1)%ndimlist(6) ==   NMNHDIM_BUDGET_NGROUPS                                                   ) then
      ! Loop on the processes
      do ji = 1, Size( pvar, 6 )
        call Diachro_one_field_write_nc4( tzfile, tpbudiachro, tpfields(ji), pvar(:,:,:,:,:,ji:ji), [ 1, 2 ], &
                                          gsplit, gdistributed, iil, iih, ijl, ijh, ikl, ikh )
      end do
    else if (       Any ( tpfields(1)%ndimlist(1) == [ NMNHDIM_BUDGET_CART_NI, NMNHDIM_BUDGET_CART_NI_U,          &
                                                       NMNHDIM_BUDGET_CART_NI_V ] )                               &
              .and. Any ( tpfields(1)%ndimlist(3) == [ NMNHDIM_BUDGET_CART_LEVEL, NMNHDIM_BUDGET_CART_LEVEL_W ] ) &
              .and.       tpfields(1)%ndimlist(6) ==   NMNHDIM_BUDGET_NGROUPS                                     ) then
      ! Loop on the processes
      do ji = 1, Size( pvar, 6 )
        call Diachro_one_field_write_nc4( tzfile, tpbudiachro, tpfields(ji), pvar(:,:,:,:,:,ji:ji), [ 1, 3 ], gsplit, gdistributed )
      end do
    else if (       Any ( tpfields(1)%ndimlist(2) == [ NMNHDIM_BUDGET_CART_NJ, NMNHDIM_BUDGET_CART_NJ_U,          &
                                                       NMNHDIM_BUDGET_CART_NJ_V ] )                               &
              .and. Any ( tpfields(1)%ndimlist(3) == [ NMNHDIM_BUDGET_CART_LEVEL, NMNHDIM_BUDGET_CART_LEVEL_W ] ) &
              .and.       tpfields(1)%ndimlist(6) ==   NMNHDIM_BUDGET_NGROUPS                                     ) then
      ! Loop on the processes
      do ji = 1, Size( pvar, 6 )
        call Diachro_one_field_write_nc4( tzfile, tpbudiachro, tpfields(ji), pvar(:,:,:,:,:,ji:ji), [ 2, 3 ], gsplit, gdistributed )
      end do
    else if (         (      tpfields(1)%ndimlist(3) == NMNHDIM_BUDGET_MASK_LEVEL     &
                        .or. tpfields(1)%ndimlist(3) == NMNHDIM_BUDGET_MASK_LEVEL_W ) &
               .and.         tpfields(1)%ndimlist(4) == NMNHDIM_BUDGET_TIME           &
               .and.         tpfields(1)%ndimlist(5) == NMNHDIM_BUDGET_MASK_NBUMASK   ) then
      !Correspond to Store_one_budget_rho (MASK)
      if ( Size( tpfields ) /= 1 ) call Print_msg( NVERB_FATAL, 'IO', 'Write_diachro_nc4', &
                                                   'wrong size of tpfields (variable '//trim(tpfields(1)%cmnhname)//')' )
      call Diachro_one_field_write_nc4( tzfile, tpbudiachro, tpfields(1), pvar, [ 3, 4, 5 ], gsplit, gdistributed, &
                                        iil, iih, ijl, ijh, ikl, ikh )
    else if (              tpfields(1)%ndimlist(3) == NMNHDIM_BUDGET_LES_LEVEL      &
              .and. (      tpfields(1)%ndimlist(4) == NMNHDIM_BUDGET_LES_TIME       &
                      .or. tpfields(1)%ndimlist(4) == NMNHDIM_BUDGET_LES_AVG_TIME ) &
              .and.        tpfields(1)%ndimlist(6) == NMNHDIM_BUDGET_LES_MASK       ) then
      if ( nles_masks /= Size( pvar, 6 ) ) &
        call Print_msg( NVERB_FATAL, 'IO', 'Write_diachro_nc4',                             &
                        'last dimension size of pvar is not equal to nles_masks (variable ' &
                        // Trim( tpfields(1)%cmnhname ) // ')' )

      ! Loop on the processes
      do ji = 1, Size( pvar, 6 )
        call Diachro_one_field_write_nc4( tzfile, tpbudiachro, tpfields(ji), pvar(:,:,:,:,:,ji:ji), [ 3, 4 ], gsplit, gdistributed )
      end do
    else if (       tpfields(1)%ndimlist(3) == NMNHDIM_BUDGET_LES_LEVEL     &
       .and. (      tpfields(1)%ndimlist(4) == NMNHDIM_BUDGET_LES_TIME &
               .or. tpfields(1)%ndimlist(4) == NMNHDIM_BUDGET_LES_AVG_TIME ) &
       .and. tpfields(1)%ndimlist(6) == NMNHDIM_BUDGET_TERM      ) then
      ! Loop on the processes
      do ji = 1, Size( pvar, 6 )
        call Diachro_one_field_write_nc4( tzfile, tpbudiachro, tpfields(ji), pvar(:,:,:,:,:,ji:ji), [ 3, 4 ], gsplit, gdistributed )
      end do
    else if (       tpfields(1)%ndimlist(3) == NMNHDIM_BUDGET_LES_LEVEL      &
       .and. (      tpfields(1)%ndimlist(4) == NMNHDIM_BUDGET_LES_TIME       &
               .or. tpfields(1)%ndimlist(4) == NMNHDIM_BUDGET_LES_AVG_TIME ) &
              .and. tpfields(1)%ndimlist(5) == NMNHDIM_BUDGET_LES_PDF      ) then
      if ( Size( tpfields ) /= 1 ) call Print_msg( NVERB_FATAL, 'IO', 'Write_diachro_nc4', &
                                                   'wrong size of tpfields (variable '//trim(tpfields(1)%cmnhname)//')' )
      call Diachro_one_field_write_nc4( tzfile, tpbudiachro, tpfields(1), pvar, [ 3, 4, 5 ], gsplit, gdistributed )
    else if (              tpfields(1)%ndimlist(3) == NMNHDIM_BUDGET_LES_LEVEL      &
              .and. (      tpfields(1)%ndimlist(4) == NMNHDIM_BUDGET_LES_TIME       &
                      .or. tpfields(1)%ndimlist(4) == NMNHDIM_BUDGET_LES_AVG_TIME ) &
              .and.        tpfields(1)%ndimlist(5) == NMNHDIM_BUDGET_LES_SV         ) then
      if ( Size( tpfields ) /= 1 ) call Print_msg( NVERB_FATAL, 'IO', 'Write_diachro_nc4', &
                                                   'wrong size of tpfields (variable '//trim(tpfields(1)%cmnhname)//')' )
      call Diachro_one_field_write_nc4( tzfile, tpbudiachro, tpfields(1), pvar, [ 3, 4, 5 ], gsplit, gdistributed, &
                                        iil, iih, ijl, ijh, ikl, ikh )
    else if (              tpfields(1)%ndimlist(1) == NMNHDIM_SPECTRA_2PTS_NI       &
              .and.        tpfields(1)%ndimlist(3) == NMNHDIM_SPECTRA_LEVEL         &
              .and. (      tpfields(1)%ndimlist(4) == NMNHDIM_BUDGET_LES_TIME       &
                      .or. tpfields(1)%ndimlist(4) == NMNHDIM_BUDGET_LES_AVG_TIME ) ) then
      if ( Size( tpfields ) /= 1 ) call Print_msg( NVERB_FATAL, 'IO', 'Write_diachro_nc4', &
                                                   'wrong size of tpfields (variable '//trim(tpfields(1)%cmnhname)//')' )
      call Diachro_one_field_write_nc4( tzfile, tpbudiachro, tpfields(1), pvar, [ 1, 3, 4 ], gsplit, gdistributed, &
                                        iil, iih, ijl, ijh, ikl, ikh )
    else if (       tpfields(1)%ndimlist(2) == NMNHDIM_SPECTRA_2PTS_NJ                   &
              .and. tpfields(1)%ndimlist(3) == NMNHDIM_SPECTRA_LEVEL                &
              .and. (      tpfields(1)%ndimlist(4) == NMNHDIM_BUDGET_LES_TIME       &
                      .or. tpfields(1)%ndimlist(4) == NMNHDIM_BUDGET_LES_AVG_TIME ) ) then
      if ( Size( tpfields ) /= 1 ) call Print_msg( NVERB_FATAL, 'IO', 'Write_diachro_nc4', &
                                                   'wrong size of tpfields (variable '//trim(tpfields(1)%cmnhname)//')' )
      call Diachro_one_field_write_nc4( tzfile, tpbudiachro, tpfields(1), pvar, [ 2, 3, 4 ], gsplit, gdistributed, &
                                        iil, iih, ijl, ijh, ikl, ikh )
    else if ( ( tpfields(1)%ndimlist(3) == NMNHDIM_LEVEL .or. tpfields(1)%ndimlist(3) == NMNHDIM_LEVEL_W ) &
         .and. tpfields(1)%ndimlist(4) == NMNHDIM_FLYER_TIME &
         .and. tpfields(1)%ndimlist(6) == NMNHDIM_FLYER_PROC ) then
      !Correspond to FLYER_DIACHRO
      !Create local time dimension
      if ( isp == tzfile%nmaster_rank) then
        istatus = NF90_INQ_DIMID( ilevelids(NLVL_GROUP), 'time_flyer', idimid )
        if ( istatus == NF90_NOERR ) then
          !Dimension already exists, check that it is not changed
          istatus = NF90_INQUIRE_DIMENSION( ilevelids(NLVL_GROUP), idimid, len = ilen )
          if ( ilen /= Int( Size( pvar, 4), kind = CDFINT ) ) &
            call Print_msg( NVERB_FATAL, 'IO', 'Write_diachro_nc4', 'time_flyer dimension has changed' )
        else
          !Dimension does not exist yet, create it
          istatus = NF90_DEF_DIM( ilevelids(NLVL_GROUP), 'time_flyer', Int( Size( pvar, 4), kind = CDFINT ), idimid )
          if ( istatus /= NF90_NOERR ) &
            call IO_Err_handle_nc4( istatus, 'Write_diachro_nc4', 'NF90_DEF_DIM', Trim( tpfields(1)%cmnhname ) )
        end if
      end if

      ! Loop on the processes
      do ji = 1, Size( pvar, 6 )
        call Diachro_one_field_write_nc4( tzfile, tpbudiachro, tpfields(ji), pvar(:,:,:,:,:,ji:ji), [ 3, 4 ], gsplit, gdistributed )
      end do
    else if (  ( tpfields(1)%ndimlist(3) == NMNHDIM_LEVEL .or. tpfields(1)%ndimlist(3) == NMNHDIM_LEVEL_W ) &
         .and. tpfields(1)%ndimlist(4) == NMNHDIM_PROFILER_TIME &
         .and. tpfields(1)%ndimlist(6) == NMNHDIM_PROFILER_PROC ) then
      !Correspond to PROFILER_DIACHRO_n
      ! Loop on the processes
      do ji = 1, Size( pvar, 6 )
        call Diachro_one_field_write_nc4( tzfile, tpbudiachro, tpfields(ji), pvar(:,:,:,:,:,ji:ji), [ 3, 4 ], gsplit, gdistributed )
      end do
    else if (  ( tpfields(1)%ndimlist(3) == NMNHDIM_SERIES_LEVEL .or. tpfields(1)%ndimlist(3) == NMNHDIM_SERIES_LEVEL_W ) &
         .and. tpfields(1)%ndimlist(4) == NMNHDIM_SERIES_TIME &
         .and. tpfields(1)%ndimlist(6) == NMNHDIM_SERIES_PROC ) then
      !Correspond to PROFILER_DIACHRO_n
      ! Loop on the processes
      do ji = 1, Size( pvar, 6 )
        call Diachro_one_field_write_nc4( tzfile, tpbudiachro, tpfields(ji), pvar(:,:,:,:,:,ji:ji), [ 3, 4 ], gsplit, gdistributed )
      end do
    else if (  ( tpfields(1)%ndimlist(1) == NMNHDIM_NI .or. tpfields(1)%ndimlist(1) == NMNHDIM_NI_U )      &
         .and. tpfields(1)%ndimlist(4) == NMNHDIM_SERIES_TIME &
         .and. tpfields(1)%ndimlist(6) == NMNHDIM_SERIES_PROC ) then
      !Correspond to PROFILER_DIACHRO_n
      ! Loop on the processes
      do ji = 1, Size( pvar, 6 )
        call Diachro_one_field_write_nc4( tzfile, tpbudiachro, tpfields(ji), pvar(:,:,:,:,:,ji:ji), [ 1, 4 ], gsplit, gdistributed )
      end do
    else if (  ( tpfields(1)%ndimlist(2) == NMNHDIM_NJ .or. tpfields(1)%ndimlist(2) == NMNHDIM_NJ_U )      &
         .and. tpfields(1)%ndimlist(4) == NMNHDIM_SERIES_TIME &
         .and. tpfields(1)%ndimlist(6) == NMNHDIM_SERIES_PROC ) then
      ! Loop on the processes
      do ji = 1, Size( pvar, 6 )
        call Diachro_one_field_write_nc4( tzfile, tpbudiachro, tpfields(ji), pvar(:,:,:,:,:,ji:ji), [ 2, 4 ], gsplit, gdistributed )
      end do
    else if (       tpfields(1)%ndimlist(4) == NMNHDIM_BUDGET_TIME         &
              .and. tpfields(1)%ndimlist(5) == NMNHDIM_BUDGET_MASK_NBUMASK &
              .and. tpfields(1)%ndimlist(6) == NMNHDIM_BUDGET_NGROUPS      ) then
      ! Loop on the processes
      do ji = 1, Size( pvar, 6 )
        call Diachro_one_field_write_nc4( tzfile, tpbudiachro, tpfields(ji), pvar(:,:,:,:,:,ji:ji), [ 4, 5 ], gsplit, gdistributed )
      end do
    else
      call Print_msg( NVERB_FATAL, 'IO', 'Write_diachro_nc4', &
                      'case not yet implemented (3D variable '//trim(tpfields(1)%cmnhname)//')' )
    end if

  case (4)

    if (       Any( tpfields(1)%ndimlist(1) == [ NMNHDIM_BUDGET_CART_NI, NMNHDIM_BUDGET_CART_NI_U, NMNHDIM_BUDGET_CART_NI_V ] ) &
         .and. Any( tpfields(1)%ndimlist(2) == [ NMNHDIM_BUDGET_CART_NJ, NMNHDIM_BUDGET_CART_NJ_U, NMNHDIM_BUDGET_CART_NJ_V ] ) &
         .and. Any( tpfields(1)%ndimlist(3) == [ NMNHDIM_BUDGET_CART_LEVEL,NMNHDIM_BUDGET_CART_LEVEL_W ] )                      &
         .and.      tpfields(1)%ndimlist(6) == NMNHDIM_BUDGET_NGROUPS                                                        ) then
      !Correspond to Store_one_budget (CART)
      ! Loop on the processes
      do ji = 1, Size( pvar, 6 )
        call Diachro_one_field_write_nc4( tzfile, tpbudiachro, tpfields(ji), pvar(:,:,:,:,:,ji:ji), [ 1, 2, 3 ], &
                                          gsplit, gdistributed, iil, iih, ijl, ijh, ikl, ikh )
      end do
    elseif (  (        tpfields(1)%ndimlist(3) == NMNHDIM_BUDGET_MASK_LEVEL     &
                  .or. tpfields(1)%ndimlist(3) == NMNHDIM_BUDGET_MASK_LEVEL_W ) &
       .and.           tpfields(1)%ndimlist(4) == NMNHDIM_BUDGET_TIME           &
       .and.           tpfields(1)%ndimlist(5) == NMNHDIM_BUDGET_MASK_NBUMASK   &
       .and.           tpfields(1)%ndimlist(6) == NMNHDIM_BUDGET_NGROUPS        ) then
      !Correspond to Store_one_budget (MASK)
      ! Loop on the processes
      do ji = 1, Size( pvar, 6 )
        call Diachro_one_field_write_nc4( tzfile, tpbudiachro, tpfields(ji), pvar(:,:,:,:,:,ji:ji), [ 3, 4, 5 ], &
                                          gsplit, gdistributed, iil, iih, ijl, ijh, ikl, ikh )
      end do
    else if (              tpfields(1)%ndimlist(3) == NMNHDIM_BUDGET_LES_LEVEL      &
              .and. (      tpfields(1)%ndimlist(4) == NMNHDIM_BUDGET_LES_TIME       &
                      .or. tpfields(1)%ndimlist(4) == NMNHDIM_BUDGET_LES_AVG_TIME ) &
       .and.               tpfields(1)%ndimlist(5) == NMNHDIM_BUDGET_LES_SV         &
       .and.               tpfields(1)%ndimlist(6) == NMNHDIM_BUDGET_LES_MASK       ) then
      if ( nles_masks /= Size( pvar, 6 ) ) &
        call Print_msg( NVERB_FATAL, 'IO', 'Write_diachro_nc4',                             &
                        'last dimension size of pvar is not equal to nles_masks (variable ' &
                        // Trim( tpfields(1)%cmnhname ) // ')' )

      ! Loop on the processes
      do ji = 1, Size( pvar, 6 )
        call Diachro_one_field_write_nc4( tzfile, tpbudiachro, tpfields(ji), pvar(:,:,:,:,:,ji:ji), [ 3, 4, 5 ], &
                                          gsplit, gdistributed, iil, iih, ijl, ijh, ikl, ikh )
      end do
    else if (              tpfields(1)%ndimlist(3) == NMNHDIM_BUDGET_LES_LEVEL      &
              .and. (      tpfields(1)%ndimlist(4) == NMNHDIM_BUDGET_LES_TIME       &
                      .or. tpfields(1)%ndimlist(4) == NMNHDIM_BUDGET_LES_AVG_TIME ) &
       .and.               tpfields(1)%ndimlist(5) == NMNHDIM_BUDGET_LES_SV         &
       .and.               tpfields(1)%ndimlist(6) == NMNHDIM_BUDGET_TERM           ) then
      ! Loop on the processes
      do ji = 1, Size( pvar, 6 )
        call Diachro_one_field_write_nc4( tzfile, tpbudiachro, tpfields(ji), pvar(:,:,:,:,:,ji:ji), [ 3, 4, 5 ], &
                                          gsplit, gdistributed, iil, iih, ijl, ijh, ikl, ikh )
      end do
    else if (             tpfields(1)%ndimlist(1) == NMNHDIM_SPECTRA_SPEC_NI       &
             .and.        tpfields(1)%ndimlist(3) == NMNHDIM_SPECTRA_LEVEL         &
             .and. (      tpfields(1)%ndimlist(4) == NMNHDIM_BUDGET_LES_TIME       &
                     .or. tpfields(1)%ndimlist(4) == NMNHDIM_BUDGET_LES_AVG_TIME ) &
             .and.        tpfields(1)%ndimlist(5) == NMNHDIM_COMPLEX               ) then
      !Correspond to LES_DIACHRO_SPEC
      if ( Size( tpfields ) /= 1 ) call Print_msg( NVERB_FATAL, 'IO', 'Write_diachro_nc4', &
                                                   'wrong size of tpfields (variable '//trim(tpfields(1)%cmnhname)//')' )
      call Diachro_one_field_write_nc4( tzfile, tpbudiachro, tpfields(1), pvar, [ 1, 3, 4, 5 ], gsplit, gdistributed )
    else if (              tpfields(1)%ndimlist(2) == NMNHDIM_SPECTRA_SPEC_NJ       &
              .and.        tpfields(1)%ndimlist(3) == NMNHDIM_SPECTRA_LEVEL         &
              .and. (      tpfields(1)%ndimlist(4) == NMNHDIM_BUDGET_LES_TIME       &
                      .or. tpfields(1)%ndimlist(4) == NMNHDIM_BUDGET_LES_AVG_TIME ) &
              .and.        tpfields(1)%ndimlist(5) == NMNHDIM_COMPLEX               ) then
      !Correspond to LES_DIACHRO_SPEC
      if ( Size( tpfields ) /= 1 ) call Print_msg( NVERB_FATAL, 'IO', 'Write_diachro_nc4', &
                                                   'wrong size of tpfields (variable '//trim(tpfields(1)%cmnhname)//')' )
      call Diachro_one_field_write_nc4( tzfile, tpbudiachro, tpfields(1), pvar, [ 2, 3, 4, 5 ], gsplit, gdistributed )
    else
      call Print_msg( NVERB_FATAL, 'IO', 'Write_diachro_nc4', &
                      'case not yet implemented (4D variable '//trim(tpfields(1)%cmnhname)//')' )
    end if

!   case (5)

!   case (6)

  case default
    do ji = 1, Size( pvar, 6 )
      call Diachro_one_field_write_nc4( tzfile, tpbudiachro, tpfields(ji), pvar(:,:,:,:,:,ji:ji), [ 1, 2, 3, 4, 5 ], &
                                        gsplit, gdistributed )
    end do

end select

!Write X and Y position of the flyer
if ( Present( tpflyer ) .and. yshape == 'Point' ) then
  if ( lcartesian ) then
    ystdnameprefix = 'plane'
  else
    ystdnameprefix = 'projection'
  endif

  tzfield = tfieldmetadata(                                 &
    cmnhname   = 'X',                                       &
    cstdname   = Trim( ystdnameprefix ) // '_x_coordinate', &
    clongname  = 'x-position of the flyer',                 &
    cunits     = 'm',                                       &
    cdir       = '--',                                      &
    ccomment   = '',                                        &
    ngrid      = 0,                                         &
    ntype      = TYPEREAL,                                  &
    ndims      = 1,                                         &
    ndimlist   = [ NMNHDIM_FLYER_TIME ],                    &
    ltimedep   = .false.                                    )

  call IO_Field_write( tzfile, tzfield, tpflyer%xx )

  tzfield%cmnhname   = 'Y'
  tzfield%cstdname   = Trim( ystdnameprefix ) // '_y_coordinate'
  tzfield%clongname  = 'y-position of the flyer'

  call IO_Field_write( tzfile, tzfield, tpflyer%xy )
end if

end  subroutine Write_diachro_nc4


subroutine Diachro_one_field_write_nc4( tpfile, tpbudiachro, tpfield, pvar, kdims, osplit, odistributed, &
                                        kil, kih, kjl, kjh, kkl, kkh )
use modd_budget,      only: NLVL_CATEGORY, NLVL_GROUP, NLVL_SHAPE, nbutshift, nbusubwrite, tbudiachrometadata
use modd_field,       only: tfieldmetadata, tfieldmetadata_base
use modd_io,          only: isp, tfiledata
use modd_parameters,  only: jphext

use mode_io_field_write, only: IO_Field_create, IO_Field_write, IO_Field_write_box

type(tfiledata),                                    intent(in)  :: tpfile        !File to write
type(tbudiachrometadata),                           intent(in)  :: tpbudiachro
class(tfieldmetadata_base),                         intent(in)  :: tpfield
real,                       dimension(:,:,:,:,:,:), intent(in)  :: pvar
integer, dimension(:),                              intent(in)  :: kdims        !List of indices of dimensions to use
logical,                                            intent(in)  :: osplit
logical,                                            intent(in)  :: odistributed !.T. if data is distributed among all processes
integer,                                            intent(in), optional :: kil, kih
integer,                                            intent(in), optional :: kjl, kjh
integer,                                            intent(in), optional :: kkl, kkh

integer                                                    :: idims
integer                                                    :: ibutimepos
integer                                                    :: ji
integer,          dimension(size(shape(pvar)))             :: isizes_alldims
integer,          dimension(:),                allocatable :: ioffset
integer,          dimension(:),                allocatable :: isizes
real                                                       :: zdata0d
real,             dimension(:),                allocatable :: zdata1d
real,             dimension(:,:),              allocatable :: zdata2d
real,             dimension(:,:,:),            allocatable :: zdata3d
real,             dimension(:,:,:,:),          allocatable :: zdata4d
real,             dimension(:,:,:,:,:),        allocatable :: zdata5d
type(tfieldmetadata)                                       :: tzfield

idims = Size( kdims )

if ( odistributed ) then
  if ( idims /= 2 .and. idims /= 3 )                                                                 &
    call Print_msg( NVERB_FATAL, 'IO', 'Diachro_one_field_write_nc4',                                &
                   'odistributed=.true. not allowed for dims/=3, field: ' //Trim( tpfield%cmnhname ) )

  if ( tpbudiachro%clevels(NLVL_SHAPE) /= 'Cartesian' )                                                       &
    call Print_msg( NVERB_FATAL, 'IO', 'Diachro_one_field_write_nc4',                                         &
                   'odistributed=.true. not allowed for shape/=cartesian, field: ' //Trim( tpfield%cmnhname ) )
end if

if ( osplit ) then
  if ( idims > 3 )                                                                                          &
    call Print_msg( NVERB_FATAL, 'IO', 'Diachro_one_field_write_nc4',                                       &
                                 'osplit=.true. not allowed for dims>3, field: ' //Trim( tpfield%cmnhname ) )

  if ( tpbudiachro%clevels(NLVL_CATEGORY) /= 'Budgets' )                                                 &
    call Print_msg( NVERB_FATAL, 'IO', 'Diachro_one_field_write_nc4',                                    &
                    'osplit=.true. not allowed for category/=budget, field: ' //Trim( tpfield%cmnhname ) )
end if

Allocate( isizes(idims) )
isizes_alldims = 1
do ji = 1, idims
  isizes(ji) = Size( pvar, kdims(ji) )
  isizes_alldims(kdims(ji)) = isizes(ji)
end do

call Prepare_diachro_write( tpfield, tzfield, kdims, osplit, odistributed, ibutimepos )

NDIMS: select case( idims )
  case ( 0 ) NDIMS
    zdata0d = pvar(1, 1, 1, 1, 1, 1)

    if ( osplit ) then
      !Create the metadata of the field (has to be done only once)
      if ( nbutshift == 1 ) call IO_Field_create( tpfile, tzfield )

      call IO_Field_write( tpfile, tzfield, [ zdata0d ], koffset= [ ( nbutshift - 1 ) * nbusubwrite ] )
    else
      call IO_Field_write( tpfile, tzfield, zdata0d )
    end if


  case ( 1 ) NDIMS
    ! Copy selected dimensions into zdata (+ auto-allocate it)
    zdata1d = Reshape ( pvar(1:isizes_alldims(1), 1:isizes_alldims(2), 1:isizes_alldims(3),  &
                             1:isizes_alldims(4), 1:isizes_alldims(5), 1:isizes_alldims(6)), &
                             isizes(1:1) )

    if ( osplit ) then
      !Create the metadata of the field (has to be done only once)
      if ( nbutshift == 1 ) call IO_Field_create( tpfile, tzfield )

      Allocate( ioffset( tzfield%ndims ) )
      ioffset(:) = 0
      ioffset(ibutimepos) = ( nbutshift - 1 ) * nbusubwrite

      if ( tzfield%ndims == idims ) then
        !No time dimension was added in Prepare_diachro_write
        call IO_Field_write( tpfile, tzfield, zdata1d(:), koffset = ioffset )
      else if ( tzfield%ndims == ( idims + 1 ) ) then
        !A time dimension was added in Prepare_diachro_write
        call IO_Field_write( tpfile, tzfield, Reshape( zdata1d, [ Size(zdata1d,1), 1 ] ), &
                              koffset = ioffset )
      else
        call Print_msg( NVERB_FATAL, 'IO', 'Diachro_one_field_write_nc4', &
                                         'probable bug for ' //Trim( tzfield%cmnhname ) )
      end if
    else !.not. osplit
      call IO_Field_write( tpfile, tzfield, zdata1d )
    end if


  case ( 2 ) NDIMS
    ! Copy selected dimensions into zdata (+ auto-allocate it)
    zdata2d = Reshape ( pvar(1:isizes_alldims(1), 1:isizes_alldims(2), 1:isizes_alldims(3),  &
                             1:isizes_alldims(4), 1:isizes_alldims(5), 1:isizes_alldims(6)), &
                             isizes(1:2) )

    if ( osplit ) then
      !Create the metadata of the field (has to be done only once)
      if ( nbutshift == 1 ) call IO_Field_create( tpfile, tzfield )

      Allocate( ioffset( tzfield%ndims ) )
      ioffset(:) = 0
      ioffset(ibutimepos) = ( nbutshift - 1 ) * nbusubwrite

      if ( odistributed ) then
        if ( tzfield%ndims == idims ) then
          !No time dimension was added in Prepare_diachro_write
          call IO_Field_write_box( tpfile, tzfield, 'BUDGET',                              &
                                   zdata2d,                                                &
                                   kil + jphext, kih + jphext, kjl + jphext, kjh + jphext, &
                                   koffset = ioffset                                       )
        else if ( tzfield%ndims == ( idims + 1 ) ) then
          !A time dimension was added in Prepare_diachro_write
          call IO_Field_write_box( tpfile, tzfield, 'BUDGET',                                   &
                                   Reshape( zdata2d, [ Size(zdata2d,1), Size(zdata2d,2), 1 ] ), &
                                   kil + jphext, kih + jphext, kjl + jphext, kjh + jphext,      &
                                   koffset = ioffset                                            )
        else
          call Print_msg( NVERB_FATAL, 'IO', 'Diachro_one_field_write_nc4', &
                                             'probable bug for ' //Trim( tzfield%cmnhname ) )
        end if
      else
        !Data is already collected on the master process
        if ( tzfield%ndims == idims ) then
          !No time dimension was added in Prepare_diachro_write
          call IO_Field_write( tpfile, tzfield, zdata2d(:,:), koffset = ioffset )
        else if ( tzfield%ndims == ( idims + 1 ) ) then
          !A time dimension was added in Prepare_diachro_write
          call IO_Field_write( tpfile, tzfield, Reshape( zdata2d, [ Size(zdata2d,1), Size(zdata2d,2), 1 ] ), &
                                koffset = ioffset )
        else
          call Print_msg( NVERB_FATAL, 'IO', 'Diachro_one_field_write_nc4', &
                                             'probable bug for ' //Trim( tzfield%cmnhname ) )
        end if
      end if
    else !.not. osplit
      if ( odistributed ) then
        call IO_Field_write_box( tpfile, tzfield, 'BUDGET', zdata2d, &
                                 kil + jphext, kih + jphext, kjl + jphext, kjh + jphext )
      else
        !Data is already collected on the master process
        call IO_Field_write( tpfile, tzfield, zdata2d )
      end if
    end if


  case ( 3 ) NDIMS
    ! Copy selected dimensions into zdata (+ auto-allocate it)
    zdata3d = Reshape ( pvar(1:isizes_alldims(1), 1:isizes_alldims(2), 1:isizes_alldims(3),  &
                             1:isizes_alldims(4), 1:isizes_alldims(5), 1:isizes_alldims(6)), &
                             isizes(1:3) )

    if ( osplit ) then
      !Create the metadata of the field (has to be done only once)
      if ( nbutshift == 1 ) call IO_Field_create( tpfile, tzfield )

      Allocate( ioffset( tzfield%ndims ) )
      ioffset(:) = 0
      ioffset(ibutimepos) = ( nbutshift - 1 ) * nbusubwrite

      if ( odistributed ) then
        if ( tzfield%ndims == idims ) then
          !No time dimension was added in Prepare_diachro_write
          call IO_Field_write_box( tpfile, tzfield, 'BUDGET',                              &
                                   zdata3d,                                                &
                                   kil + jphext, kih + jphext, kjl + jphext, kjh + jphext, &
                                   koffset = ioffset                                       )
        else if ( tzfield%ndims == ( idims + 1 ) ) then
          !A time dimension was added in Prepare_diachro_write
          call IO_Field_write_box( tpfile, tzfield, 'BUDGET',                                                    &
                                   Reshape( zdata3d, [ Size(zdata3d,1), Size(zdata3d,2), Size(zdata3d,3), 1 ] ), &
                                   kil + jphext, kih + jphext, kjl + jphext, kjh + jphext,                       &
                                   koffset = ioffset                                                             )
        else
          call Print_msg( NVERB_FATAL, 'IO', 'Diachro_one_field_write_nc4', &
                                             'probable bug for ' //Trim( tzfield%cmnhname ) )
        end if
      else
        !Data is already collected on the master process
        if ( tzfield%ndims == idims ) then
          !No time dimension was added in Prepare_diachro_write
          call IO_Field_write( tpfile, tzfield, zdata3d(:,:,:), koffset = ioffset )
        else if ( tzfield%ndims == ( idims + 1 ) ) then
          !A time dimension was added in Prepare_diachro_write
          call IO_Field_write( tpfile, tzfield, Reshape( zdata3d, [ Size(zdata3d,1), Size(zdata3d,2), Size(zdata3d,3), 1 ] ), &
                                koffset = ioffset )
        else
          call Print_msg( NVERB_FATAL, 'IO', 'Diachro_one_field_write_nc4', &
                                             'probable bug for ' //Trim( tzfield%cmnhname ) )
        end if
      end if
    else !.not. osplit
      if ( odistributed ) then
        call IO_Field_write_box( tpfile, tzfield, 'BUDGET', zdata3d, &
                                 kil + jphext, kih + jphext, kjl + jphext, kjh + jphext )
      else
        !Data is already collected on the master process
        call IO_Field_write( tpfile, tzfield, zdata3d )
      end if
    end if


  case ( 4 ) NDIMS
    ! Copy selected dimensions into zdata (+ auto-allocate it)
    zdata4d = Reshape ( pvar(1:isizes_alldims(1), 1:isizes_alldims(2), 1:isizes_alldims(3),  &
                             1:isizes_alldims(4), 1:isizes_alldims(5), 1:isizes_alldims(6)), &
                             isizes(1:4) )

    call IO_Field_write( tpfile, tzfield, zdata4d )


  case ( 5 ) NDIMS
    ! Copy selected dimensions into zdata (+ auto-allocate it)
    zdata5d = Reshape ( pvar(1:isizes_alldims(1), 1:isizes_alldims(2), 1:isizes_alldims(3),  &
                             1:isizes_alldims(4), 1:isizes_alldims(5), 1:isizes_alldims(6)), &
                             isizes(1:5) )

    call IO_Field_write( tpfile, tzfield, zdata5d )


  case default NDIMS
    call Print_msg( NVERB_ERROR, 'IO', 'Diachro_one_field_write_nc4', Trim( tpfile%cname ) // &
                          ': unsupported number of dimensions' )
    return

end select NDIMS

end subroutine Diachro_one_field_write_nc4


subroutine Prepare_diachro_write( tpfieldin, tpfieldout, kdims, osplit, odistributed, kbutimepos )
use modd_field, only: NMNHDIM_BUDGET_TIME, NMNHDIM_UNUSED, NMNHMAXDIMS, tfieldmetadata, tfieldmetadata_base

class(tfieldmetadata_base), intent(in)  :: tpfieldin
type(tfieldmetadata),       intent(out) :: tpfieldout
integer, dimension(:),      intent(in)  :: kdims ! List of indices of dimensions to use
logical,                    intent(in)  :: osplit
logical,                    intent(in)  :: odistributed ! .true. if data is distributed among all the processes
integer,                    intent(out) :: kbutimepos

integer :: idims
integer :: jdim

idims = Size( kdims )

if ( idims > NMNHMAXDIMS ) call Print_msg( NVERB_FATAL, 'IO', 'Prepare_diachro_write', &
                                           'kdims is too big for ' //Trim( tpfieldin%cmnhname ) )

tpfieldout%cmnhname   = tpfieldin%cmnhname
tpfieldout%cstdname   = tpfieldin%cstdname
tpfieldout%clongname  = tpfieldin%clongname
tpfieldout%cunits     = tpfieldin%cunits
if ( .not. odistributed ) then
  tpfieldout%cdir       = '--'
else
  tpfieldout%cdir       = 'XY'
end if
tpfieldout%ccomment   = tpfieldin%ccomment
tpfieldout%ngrid      = tpfieldin%ngrid
tpfieldout%ntype      = tpfieldin%ntype
tpfieldout%ltimedep   = .false.

tpfieldout%ndims      = idims

do jdim = 1, idims
  tpfieldout%ndimlist(jdim)  = tpfieldin%ndimlist(kdims(jdim))
end do
tpfieldout%ndimlist(idims + 1:) = NMNHDIM_UNUSED

kbutimepos = -1

!Add budget time dimension if required
if ( osplit ) then
  do jdim = 1, idims
    if ( tpfieldout%ndimlist(jdim) == NMNHDIM_BUDGET_TIME ) then
      kbutimepos = jdim
      exit
    end if
  end do

  !budget time dimension was not found => add it
  if ( kbutimepos == -1 ) then
    idims = idims + 1
    if ( idims > NMNHMAXDIMS ) call Print_msg( NVERB_FATAL, 'IO', 'Prepare_diachro_write',  &
                                               'impossible to add NMNHDIM_BUDGET_TIME dimension for ' //Trim( tpfieldin%cmnhname ) )
    kbutimepos = idims
    tpfieldout%ndims = idims
    tpfieldout%ndimlist(idims) = NMNHDIM_BUDGET_TIME
  end if
end if

end subroutine Prepare_diachro_write


subroutine Att_write_c0( hlevel, kgrpid, hattname, hdata )
use NETCDF,            only: NF90_GET_ATT, NF90_INQUIRE_ATTRIBUTE, NF90_PUT_ATT, NF90_CHAR, NF90_GLOBAL, NF90_NOERR

use modd_precision,    only: CDFINT

use mode_io_tools_nc4, only: IO_Err_handle_nc4, IO_Mnhname_clean

character(len=*),     intent(in) :: hlevel
integer(kind=CDFINT), intent(in) :: kgrpid
character(len=*),     intent(in) :: hattname
character(len=*),     intent(in) :: hdata

character(len=Len(hattname))  :: yattname
character(len=:), allocatable :: yatt
integer(kind=CDFINT)          :: ilen
integer(kind=CDFINT)          :: istatus
integer(kind=CDFINT)          :: itype

call IO_Mnhname_clean( hattname, yattname )

istatus = NF90_INQUIRE_ATTRIBUTE( kgrpid, NF90_GLOBAL, yattname, xtype = itype, len = ilen )
if (istatus == NF90_NOERR ) then
  call Print_msg( NVERB_DEBUG, 'IO', 'Write_diachro_nc4', 'attribute ' // yattname // ' already exists for ' // Trim( hlevel ) )

  if ( itype /= NF90_CHAR ) then
    call Print_msg( NVERB_ERROR, 'IO', 'Write_diachro_nc4', 'type for attribute ' // yattname // &
                    ' has changed for ' // Trim( hlevel ) )
    return
  end if

  Allocate( character(len=ilen) :: yatt )
  istatus = NF90_GET_ATT( kgrpid, NF90_GLOBAL, yattname, yatt )
  if ( yatt == Trim( hdata ) ) then
    call Print_msg( NVERB_DEBUG, 'IO', 'Write_diachro_nc4', 'attribute ' // yattname // ' is unchanged for ' // Trim( hlevel ) )
    !If unchanged, no need to write it again => return
    return
  else
    cmnhmsg(1) = 'attribute ' // yattname // ' has changed for ' // Trim( hlevel )
    cmnhmsg(2) = yatt // ' -> ' // Trim( hdata )
    call Print_msg( NVERB_WARNING, 'IO', 'Write_diachro_nc4' )
  end if

end if

istatus = NF90_PUT_ATT( kgrpid, NF90_GLOBAL, yattname, Trim( hdata ) )
if (istatus /= NF90_NOERR ) &
 call IO_Err_handle_nc4( istatus, 'Write_diachro_nc4', 'NF90_PUT_ATT', Trim( yattname ) // ' for '// Trim( hlevel ) // ' group' )

end subroutine Att_write_c0


subroutine Att_write_i0( hlevel, kgrpid, hattname, kdata )
use NETCDF,            only: NF90_GET_ATT, NF90_INQUIRE_ATTRIBUTE, NF90_PUT_ATT, NF90_GLOBAL, NF90_NOERR

use modd_precision,    only: CDFINT, MNHINT_NF90

use mode_io_tools_nc4, only: IO_Err_handle_nc4, IO_Mnhname_clean

character(len=*),     intent(in) :: hlevel
integer(kind=CDFINT), intent(in) :: kgrpid
character(len=*),     intent(in) :: hattname
integer,              intent(in) :: kdata

character(len=Len(hattname)) :: yattname
integer              :: iatt
integer(kind=CDFINT) :: ilen
integer(kind=CDFINT) :: istatus
integer(kind=CDFINT) :: itype

call IO_Mnhname_clean( hattname, yattname )

istatus = NF90_INQUIRE_ATTRIBUTE( kgrpid, NF90_GLOBAL, yattname, xtype = itype, len = ilen )
if (istatus == NF90_NOERR ) then
  call Print_msg( NVERB_DEBUG, 'IO', 'Write_diachro_nc4', 'attribute ' // yattname // ' already exists for ' // Trim( hlevel ) )

  if ( itype /= MNHINT_NF90 ) then
    call Print_msg( NVERB_ERROR, 'IO', 'Write_diachro_nc4', 'type for attribute ' // yattname // &
                    ' has changed for ' // Trim( hlevel ) )
    return
  end if

  if ( ilen /= 1 ) then
    call Print_msg( NVERB_ERROR, 'IO', 'Write_diachro_nc4', 'size of attribute ' // yattname // &
                    ' has changed for ' // Trim( hlevel ) )
    return
  end if

  istatus = NF90_GET_ATT( kgrpid, NF90_GLOBAL, yattname, iatt )
  if ( iatt == kdata ) then
    call Print_msg( NVERB_DEBUG, 'IO', 'Write_diachro_nc4', 'attribute ' // yattname // ' is unchanged for ' // Trim( hlevel ) )
    !If unchanged, no need to write it again => return
    return
  else
    cmnhmsg(1) = 'attribute ' // yattname // ' has changed for ' // Trim( hlevel )
    Write( cmnhmsg(2), '( I0, " -> ", I0 )' ) iatt, kdata
    call Print_msg( NVERB_WARNING, 'IO', 'Write_diachro_nc4' )
  end if

end if

istatus = NF90_PUT_ATT( kgrpid, NF90_GLOBAL, yattname, kdata )
if (istatus /= NF90_NOERR ) &
 call IO_Err_handle_nc4( istatus, 'Write_diachro_nc4', 'NF90_PUT_ATT', Trim( yattname ) // ' for '// Trim( hlevel ) // ' group' )

end subroutine Att_write_i0


subroutine Att_write_x0( hlevel, kgrpid, hattname, pdata )
use NETCDF,            only: NF90_GET_ATT, NF90_INQUIRE_ATTRIBUTE, NF90_PUT_ATT, NF90_GLOBAL, NF90_NOERR

use modd_precision,    only: CDFINT, MNHREAL_NF90

use mode_io_tools_nc4, only: IO_Err_handle_nc4, IO_Mnhname_clean

character(len=*),     intent(in) :: hlevel
integer(kind=CDFINT), intent(in) :: kgrpid
character(len=*),     intent(in) :: hattname
real,                 intent(in) :: pdata

character(len=Len(hattname)) :: yattname
integer(kind=CDFINT) :: ilen
integer(kind=CDFINT) :: istatus
integer(kind=CDFINT) :: itype
real                 :: zatt

call IO_Mnhname_clean( hattname, yattname )

istatus = NF90_INQUIRE_ATTRIBUTE( kgrpid, NF90_GLOBAL, yattname, xtype = itype, len = ilen )
if (istatus == NF90_NOERR ) then
  call Print_msg( NVERB_DEBUG, 'IO', 'Write_diachro_nc4', 'attribute ' // yattname // ' already exists for ' // Trim( hlevel ) )

  if ( itype /= MNHREAL_NF90 ) then
    call Print_msg( NVERB_ERROR, 'IO', 'Write_diachro_nc4', 'type for attribute ' // yattname // &
                    ' has changed for ' // Trim( hlevel ) )
    return
  end if

  if ( ilen /= 1 ) then
    call Print_msg( NVERB_ERROR, 'IO', 'Write_diachro_nc4', 'size of attribute ' // yattname // &
                    ' has changed for ' // Trim( hlevel ) )
    return
  end if

  istatus = NF90_GET_ATT( kgrpid, NF90_GLOBAL, yattname, zatt )
  if ( zatt == pdata ) then
    call Print_msg( NVERB_DEBUG, 'IO', 'Write_diachro_nc4', 'attribute ' // yattname // ' is unchanged for ' // Trim( hlevel ) )
    !If unchanged, no need to write it again => return
    return
  else
    cmnhmsg(1) = 'attribute ' // yattname // ' has changed for ' // Trim( hlevel )
    Write( cmnhmsg(2), '( F15.7, " -> ", F15.7 )' ) zatt, pdata
    call Print_msg( NVERB_WARNING, 'IO', 'Write_diachro_nc4' )
  end if

end if

istatus = NF90_PUT_ATT( kgrpid, NF90_GLOBAL, yattname, pdata )
if (istatus /= NF90_NOERR ) &
 call IO_Err_handle_nc4( istatus, 'Write_diachro_nc4', 'NF90_PUT_ATT', Trim( yattname ) // ' for '// Trim( hlevel ) // ' group' )

end subroutine Att_write_x0


subroutine Move_to_next_level( kpreviouslevelid, gpreviousleveldefined, oleveluse, hlevelname, gleveldefined, klevelid )
use NETCDF,            only: NF90_DEF_GRP, NF90_INQ_NCID, NF90_NOERR

use modd_precision,    only: CDFINT

use mode_io_tools_nc4, only: IO_Err_handle_nc4, IO_Mnhname_clean

integer(kind=CDFINT), intent(in)    :: kpreviouslevelid
logical,              intent(in)    :: gpreviousleveldefined
logical,              intent(in)    :: oleveluse
! character(len=*),     intent(inout) :: hlevelname
character(len=*),     intent(in)    :: hlevelname
logical,              intent(out)   :: gleveldefined
integer(kind=CDFINT), intent(out)   :: klevelid

character(len=Len(hlevelname)) :: ylevelname
integer(kind=CDFINT) :: istatus

call IO_Mnhname_clean( hlevelname, ylevelname )

if ( oleveluse ) then
  istatus = NF90_INQ_NCID( kpreviouslevelid, Trim( ylevelname ), klevelid )
  if ( istatus == NF90_NOERR ) then
    gleveldefined = .true.
  else
    gleveldefined = .false.
    istatus = NF90_DEF_GRP( kpreviouslevelid, Trim( ylevelname ), klevelid )
    if ( istatus /= NF90_NOERR ) &
      call IO_Err_handle_nc4( istatus, 'Move_to_next_level', 'NF90_DEF_GRP', 'for ' // Trim( ylevelname ) )
  end if
else
  gleveldefined = gpreviousleveldefined
  klevelid = kpreviouslevelid
end if

end subroutine Move_to_next_level
#endif

end module mode_write_diachro
