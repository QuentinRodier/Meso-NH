!MNH_LIC Copyright 1996-2021 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
module mode_write_diachro

use mode_msg

implicit none

private

public :: Write_diachro

contains

! #########################################################################
subroutine Write_diachro( tpdiafile, tpfields, hgroup, htype,             &
                          tpdates, pvar,                                  &
                          oicp, ojcp, okcp, kil, kih, kjl, kjh, kkl, kkh, &
                          ptrajx, ptrajy, ptrajz, osplit )
! #########################################################################
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
!  P. Wautelet 08/12/2020: budgets: merge budgets terms with different nbutshift in same group variables
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
use modd_conf,           only: lpack
use modd_field,          only: tfield_metadata_base
use modd_io,             only: tfiledata
use modd_type_date,      only: date_time
!
IMPLICIT NONE
!
!*       0.1   Dummy arguments
!              ---------------
TYPE(TFILEDATA),                                     INTENT(IN)           :: TPDIAFILE    ! file to write
class(tfield_metadata_base), dimension(:),           intent(in)           :: tpfields
CHARACTER(LEN=*),                                    INTENT(IN)           :: HGROUP, HTYPE
type(date_time),             dimension(:),           intent(in)           :: tpdates
REAL,                        DIMENSION(:,:,:,:,:,:), INTENT(IN)           :: PVAR
LOGICAL,                                             INTENT(IN), OPTIONAL :: OICP, OJCP, OKCP
INTEGER,                                             INTENT(IN), OPTIONAL :: KIL, KIH
INTEGER,                                             INTENT(IN), OPTIONAL :: KJL, KJH
INTEGER,                                             INTENT(IN), OPTIONAL :: KKL, KKH
REAL,DIMENSION(:,:,:),                               INTENT(IN), OPTIONAL :: PTRAJX
REAL,DIMENSION(:,:,:),                               INTENT(IN), OPTIONAL :: PTRAJY
REAL,DIMENSION(:,:,:),                               INTENT(IN), OPTIONAL :: PTRAJZ
logical,                                             intent(in), optional :: osplit
!
!*       0.1   Local variables
!              ---------------
logical :: gicp, gjcp, gkcp
logical :: gpack
!------------------------------------------------------------------------------

call Print_msg( NVERB_DEBUG, 'BUD', 'Write_diachro', 'called' )

if ( present( oicp ) ) then
  gicp = oicp
else
  gicp = .false.
end if

if ( present( ojcp ) ) then
  gjcp = ojcp
else
  gjcp = .false.
end if

if ( present( okcp ) ) then
  gkcp = okcp
else
  gkcp = .false.
end if

gpack = lpack
lpack = .false.

#ifdef MNH_IOLFI
if ( tpdiafile%cformat == 'LFI' .or. tpdiafile%cformat == 'LFICDF4' ) &
  call Write_diachro_lfi( tpdiafile, tpfields, hgroup, htype, tpdates, pvar, gicp, gjcp, gkcp, kil, kih, kjl, kjh, kkl, kkh, &
                          ptrajx, ptrajy, ptrajz )
#endif

#ifdef MNH_IOCDF4
if ( tpdiafile%cformat == 'NETCDF4' .or. tpdiafile%cformat == 'LFICDF4' ) &
  call Write_diachro_nc4( tpdiafile, tpfields, hgroup, htype, tpdates, pvar, gicp, gjcp, gkcp, kil, kih, kjl, kjh, kkl, kkh, &
                          osplit )
#endif

lpack = gpack

end subroutine Write_diachro

#ifdef MNH_IOLFI
!-----------------------------------------------------------------------------
subroutine Write_diachro_lfi( tpdiafile, tpfields, hgroup, htype, tpdates, pvar, oicp, ojcp, okcp, kil, kih, kjl, kjh, kkl, kkh, &
                              ptrajx, ptrajy, ptrajz )

use modd_budget,         only: nbumask, nbutshift, nbusubwrite
use modd_field,          only: NMNHDIM_ONE, NMNHDIM_UNKNOWN, NMNHDIM_FLYER_TIME, NMNHDIM_NOTLISTED, NMNHDIM_UNUSED, &
                               TYPECHAR, TYPEINT, TYPEREAL,                                                         &
                               tfield_metadata_base, tfielddata
use modd_io,             only: tfiledata
use modd_parameters,     only: jphext
use modd_time,           only: tdtexp, tdtseg
use modd_time_n,         only: tdtmod
use modd_type_date,      only: date_time

use mode_datetime,       only: Datetime_distance
use mode_io_field_write, only: IO_Field_write, IO_Field_write_box
use mode_menu_diachro,   only: Menu_diachro
use mode_tools_ll,       only: Get_globaldims_ll


type(tfiledata),                                     intent(in)           :: tpdiafile        ! File to write
class(tfield_metadata_base), dimension(:),           intent(in)           :: tpfields
character(len=*),                                    intent(in)           :: hgroup, htype
type(date_time),             dimension(:),           intent(in)           :: tpdates
real,                        dimension(:,:,:,:,:,:), intent(in)           :: pvar
logical,                                             intent(in)           :: oicp, ojcp, okcp
integer,                                             intent(in), optional :: kil, kih
integer,                                             intent(in), optional :: kjl, kjh
integer,                                             intent(in), optional :: kkl, kkh
REAL,DIMENSION(:,:,:),                               INTENT(IN), OPTIONAL :: PTRAJX
REAL,DIMENSION(:,:,:),                               INTENT(IN), OPTIONAL :: PTRAJY
REAL,DIMENSION(:,:,:),                               INTENT(IN), OPTIONAL :: PTRAJZ

integer, parameter :: LFITITLELGT = 100
integer, parameter :: LFIUNITLGT = 100
integer, parameter :: LFICOMMENTLGT = 100

CHARACTER(LEN=20) :: YCOMMENT
CHARACTER(LEN=3)  :: YJ
character(len=:),                           allocatable :: ygroup
character(len=LFITITLELGT),   dimension(:), allocatable :: ytitles   !Used to respect LFI fileformat
character(len=LFIUNITLGT),    dimension(:), allocatable :: yunits    !Used to respect LFI fileformat
character(len=LFICOMMENTLGT), dimension(:), allocatable :: ycomments !Used to respect LFI fileformat
INTEGER   ::   ILENG, ILENTITRE, ILENUNITE, ILENCOMMENT
INTEGER   ::   II, IJ, IK, IT, IN, IP, J, JJ
INTEGER   ::   INTRAJT, IKTRAJX, IKTRAJY, IKTRAJZ
INTEGER   ::   ITTRAJX, ITTRAJY, ITTRAJZ
INTEGER   ::   INTRAJX, INTRAJY, INTRAJZ
INTEGER   ::   IIMASK, IJMASK, IKMASK, ITMASK, INMASK, IPMASK
INTEGER   ::   ICOMPX, ICOMPY, ICOMPZ
INTEGER   ::   IIMAX_ll, IJMAX_ll ! size of the physical global domain
integer   ::   ji
INTEGER,DIMENSION(:),ALLOCATABLE :: ITABCHAR
real, dimension(:,:), allocatable :: ztimes
real, dimension(:,:), allocatable :: zdatime
TYPE(TFIELDDATA) :: TZFIELD
type(tfiledata)  :: tzfile

call Print_msg( NVERB_DEBUG, 'BUD', 'Write_diachro_lfi', 'called' )

tzfile = tpdiafile

!Write only in LFI files
tzfile%cformat = 'LFI'

YCOMMENT='NOTHING'

if (      Any( hgroup == [ 'RJS', 'RJX', 'RJY', 'RJZ'] )                                              &
     .or. Any( hgroup == [ 'UU', 'VV', 'WW', 'TH', 'TK', 'RV', 'RC', 'RR', 'RI', 'RS', 'RG', 'RH' ] ) &
     .or.  ( hgroup(1:2) == 'SV' .and. Len( hgroup ) == 5 )                                           ) then
  Allocate( character(len=9) :: ygroup )
  ygroup(:) = hgroup
  do ji = Len_trim( hgroup ) + 1, 5
    ygroup(ji : ji) = '_'
  end do
  Write( ygroup(6:9), '( i4.4 )' ) nbutshift
else
  ygroup = hgroup
end if

II = SIZE(PVAR,1)
IJ = SIZE(PVAR,2)
IF(HTYPE == 'CART' .AND. .NOT. OICP .AND. .NOT. OJCP) THEN
                              !for parallel execution, PVAR is distributed on several proc
  II=KIH-KIL+1
  IJ=KJH-KJL+1
ENDIF
IK = SIZE(PVAR,3)
IT = SIZE(PVAR,4)
IN = SIZE(PVAR,5)
IP = SIZE(PVAR,6)

INTRAJT=SIZE(tpdates)

IKTRAJX=0; IKTRAJY=0; IKTRAJZ=0
ITTRAJX=0; ITTRAJY=0; ITTRAJZ=0
INTRAJX=0; INTRAJY=0; INTRAJZ=0
IF(PRESENT(PTRAJX))THEN
  IKTRAJX=SIZE(PTRAJX,1)
  ITTRAJX=SIZE(PTRAJX,2)
  INTRAJX=SIZE(PTRAJX,3)
ENDIF
IF(PRESENT(PTRAJY))THEN
  IKTRAJY=SIZE(PTRAJY,1)
  ITTRAJY=SIZE(PTRAJY,2)
  INTRAJY=SIZE(PTRAJY,3)
ENDIF
IF(PRESENT(PTRAJZ))THEN
  IKTRAJZ=SIZE(PTRAJZ,1)
  ITTRAJZ=SIZE(PTRAJZ,2)
  INTRAJZ=SIZE(PTRAJZ,3)
ENDIF

IIMASK=0; IJMASK=0; IKMASK=0; ITMASK=0; INMASK=0; IPMASK=0
IF(HTYPE == 'MASK')THEN
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

ICOMPX=0; ICOMPY=0; ICOMPZ=0
IF ( OICP ) THEN
  ICOMPX = 1
ELSE
  ICOMPX = 0
ENDIF
IF ( OJCP ) THEN
  ICOMPY = 1
ELSE
  ICOMPY = 0
ENDIF
IF ( OKCP ) THEN
  ICOMPZ=1
ELSE
  ICOMPZ = 0
ENDIF
!
! 1er enregistrement TYPE
!
TZFIELD%CMNHNAME   = TRIM(ygroup)//'.TYPE'
TZFIELD%CSTDNAME   = ''
TZFIELD%CLONGNAME  = TRIM(ygroup)//'.TYPE'
TZFIELD%CUNITS     = ''
TZFIELD%CDIR       = '--'
TZFIELD%CCOMMENT   = TRIM(YCOMMENT)
TZFIELD%NGRID      = tpfields(1)%ngrid
TZFIELD%NTYPE      = TYPECHAR
TZFIELD%NDIMS      = 0
TZFIELD%LTIMEDEP   = .FALSE.
CALL IO_Field_write(tzfile,TZFIELD,HTYPE)
!
! 2eme  enregistrement DIMENSIONS des MATRICES et LONGUEUR des TABLEAUX de CARACTERES et FLAGS de COMPRESSION sur les DIFFERENTS AXES
!
TZFIELD%CMNHNAME   = TRIM(ygroup)//'.DIM'
TZFIELD%CSTDNAME   = ''
TZFIELD%CLONGNAME  = TRIM(ygroup)//'.DIM'
TZFIELD%CUNITS     = ''
TZFIELD%CDIR       = '--'
TZFIELD%CCOMMENT   = TRIM(YCOMMENT)
TZFIELD%NGRID      = tpfields(1)%ngrid
TZFIELD%NTYPE      = TYPEINT
TZFIELD%NDIMS      = 1
TZFIELD%LTIMEDEP   = .FALSE.
SELECT CASE(HTYPE)
  CASE('CART','MASK','SPXY')
    if (      .not. Present( kil ) .or. .not. Present( kih ) .or. .not. Present( kjl ) .or. .not. Present( kjh )  &
         .or. .not. Present( kkl ) .or. .not. Present( kkh ) ) then
      call Print_msg( NVERB_FATAL, 'IO', 'Write_diachro', &
                      'kil, kih, kjl, kjh, kkl or kkh not provided for variable ' // Trim( tpfields(1)%cmnhname ) )
    end if
    ILENG = 34
    ALLOCATE(ITABCHAR(ILENG))
    ITABCHAR(1)=ILENTITRE; ITABCHAR(2)=ILENUNITE
    ITABCHAR(3)=ILENCOMMENT; ITABCHAR(4)=II
    ITABCHAR(5)=IJ; ITABCHAR(6)=IK
    ITABCHAR(7)=IT; ITABCHAR(8)=IN
    ITABCHAR(9)=IP; ITABCHAR(10)=KIL
    ITABCHAR(11)=KJL; ITABCHAR(12)=KKL
    ITABCHAR(13)=KIH; ITABCHAR(14)=KJH
    ITABCHAR(15)=KKH; ITABCHAR(16)=ICOMPX
    ITABCHAR(17)=ICOMPY; ITABCHAR(18)=ICOMPZ
    IF(HTYPE == 'MASK')THEN
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
TZFIELD%CMNHNAME   = TRIM(ygroup)//'.TITRE'
TZFIELD%CSTDNAME   = ''
TZFIELD%CLONGNAME  = TRIM(ygroup)//'.TITRE'
TZFIELD%CUNITS     = ''
TZFIELD%CDIR       = '--'
TZFIELD%CCOMMENT   = TRIM(YCOMMENT)
TZFIELD%NGRID      = tpfields(1)%ngrid
TZFIELD%NTYPE      = TYPECHAR
TZFIELD%NDIMS      = 1
TZFIELD%LTIMEDEP   = .FALSE.
allocate( ytitles( ip ) )
ytitles(:) = tpfields(1 : ip)%cmnhname
CALL IO_Field_write(tzfile,TZFIELD,ytitles(:))
deallocate( ytitles )
!
! 4eme enregistrement UNITE
!
TZFIELD%CMNHNAME   = TRIM(ygroup)//'.UNITE'
TZFIELD%CSTDNAME   = ''
TZFIELD%CLONGNAME  = TRIM(ygroup)//'.UNITE'
TZFIELD%CUNITS     = ''
TZFIELD%CDIR       = '--'
TZFIELD%CCOMMENT   = TRIM(YCOMMENT)
TZFIELD%NGRID      = tpfields(1)%ngrid
TZFIELD%NTYPE      = TYPECHAR
TZFIELD%NDIMS      = 1
TZFIELD%LTIMEDEP   = .FALSE.
allocate( yunits( ip ) )
yunits(:) = tpfields(1 : ip)%cunits
CALL IO_Field_write(tzfile,TZFIELD,yunits(:))
deallocate( yunits )
!
! 5eme enregistrement COMMENT
!
TZFIELD%CMNHNAME   = TRIM(ygroup)//'.COMMENT'
TZFIELD%CSTDNAME   = ''
TZFIELD%CLONGNAME  = TRIM(ygroup)//'.COMMENT'
TZFIELD%CUNITS     = ''
TZFIELD%CDIR       = '--'
TZFIELD%CCOMMENT   = TRIM(YCOMMENT)
TZFIELD%NGRID      = tpfields(1)%ngrid
TZFIELD%NTYPE      = TYPECHAR
TZFIELD%NDIMS      = 1
TZFIELD%LTIMEDEP   = .FALSE.
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
  IF(HTYPE == 'CART' .AND. .NOT. oicp .AND. .NOT. ojcp) THEN
    TZFIELD%CMNHNAME   = TRIM(ygroup)//'.PROC'//YJ
    TZFIELD%CSTDNAME   = ''
    TZFIELD%CLONGNAME  = TRIM(TZFIELD%CMNHNAME)
    TZFIELD%CUNITS     = tpfields(j)%cunits
    TZFIELD%CDIR       = 'XY'
    TZFIELD%CCOMMENT   = TRIM(tpfields(j)%cmnhname)//' - '//TRIM(tpfields(j)%ccomment)//' ('// Trim( tpfields(j)%cunits ) //')'
    TZFIELD%NGRID      = tpfields(j)%ngrid
    TZFIELD%NTYPE      = TYPEREAL
    TZFIELD%NDIMS      = 5
    TZFIELD%LTIMEDEP   = .FALSE.

    CALL IO_Field_write_BOX(tzfile,TZFIELD,'BUDGET',PVAR(:,:,:,:,:,J), &
                            KIL+JPHEXT,KIH+JPHEXT,KJL+JPHEXT,KJH+JPHEXT)
  ELSE
    TZFIELD%CMNHNAME   = TRIM(ygroup)//'.PROC'//YJ
    TZFIELD%CSTDNAME   = ''
    TZFIELD%CLONGNAME  = TRIM(TZFIELD%CMNHNAME)
    TZFIELD%CUNITS     = tpfields(j)%cunits
    TZFIELD%CDIR       = '--'
    TZFIELD%CCOMMENT   = TRIM(tpfields(j)%cmnhname)//' - '//TRIM(tpfields(j)%ccomment)//' ('// Trim( tpfields(j)%cunits ) //')'
    TZFIELD%NGRID      = tpfields(j)%ngrid
    TZFIELD%NTYPE      = TYPEREAL
    TZFIELD%NDIMS      = 5
    TZFIELD%LTIMEDEP   = .FALSE.

    CALL IO_Field_write(tzfile,TZFIELD,PVAR(:,:,:,:,:,J))
  ENDIF
  tzfield%ndimlist(:)   = NMNHDIM_UNKNOWN
ENDDO
!
! 7eme enregistrement TRAJT
!
TZFIELD%CMNHNAME   = TRIM(ygroup)//'.TRAJT'
TZFIELD%CSTDNAME   = ''
TZFIELD%CLONGNAME  = TRIM(ygroup)//'.TRAJT'
TZFIELD%CUNITS     = ''
TZFIELD%CDIR       = '--'
TZFIELD%CCOMMENT   = TRIM(YCOMMENT)
TZFIELD%NGRID      = tpfields(1)%ngrid
TZFIELD%NTYPE      = TYPEREAL
TZFIELD%NDIMS      = 2
TZFIELD%LTIMEDEP   = .FALSE.

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
IF(PRESENT(PTRAJX))THEN
  TZFIELD%CMNHNAME   = TRIM(ygroup)//'.TRAJX'
  TZFIELD%CSTDNAME   = ''
  TZFIELD%CLONGNAME  = TRIM(ygroup)//'.TRAJX'
  TZFIELD%CUNITS     = ''
  TZFIELD%CDIR       = '--'
  TZFIELD%CCOMMENT   = TRIM(YCOMMENT)
  TZFIELD%NGRID      = tpfields(1)%ngrid
  TZFIELD%NTYPE      = TYPEREAL
  TZFIELD%NDIMS      = 3
  TZFIELD%LTIMEDEP   = .FALSE.
  CALL IO_Field_write(tzfile,TZFIELD,PTRAJX)
ENDIF
!
! 9eme enregistrement TRAJY
!
IF(PRESENT(PTRAJY))THEN
  TZFIELD%CMNHNAME   = TRIM(ygroup)//'.TRAJY'
  TZFIELD%CSTDNAME   = ''
  TZFIELD%CLONGNAME  = TRIM(ygroup)//'.TRAJY'
  TZFIELD%CUNITS     = ''
  TZFIELD%CDIR       = '--'
  TZFIELD%CCOMMENT   = TRIM(YCOMMENT)
  TZFIELD%NGRID      = tpfields(1)%ngrid
  TZFIELD%NTYPE      = TYPEREAL
  TZFIELD%NDIMS      = 3
  TZFIELD%LTIMEDEP   = .FALSE.
  CALL IO_Field_write(tzfile,TZFIELD,PTRAJY)
ENDIF
!
! 10eme enregistrement TRAJZ
!
IF(PRESENT(PTRAJZ))THEN
  TZFIELD%CMNHNAME   = TRIM(ygroup)//'.TRAJZ'
  TZFIELD%CSTDNAME   = ''
  TZFIELD%CLONGNAME  = TRIM(ygroup)//'.TRAJZ'
  TZFIELD%CUNITS     = ''
  TZFIELD%CDIR       = '--'
  TZFIELD%CCOMMENT   = TRIM(YCOMMENT)
  TZFIELD%NGRID      = tpfields(1)%ngrid
  TZFIELD%NTYPE      = TYPEREAL
  TZFIELD%NDIMS      = 3
  TZFIELD%LTIMEDEP   = .FALSE.
  CALL IO_Field_write(tzfile,TZFIELD,PTRAJZ)
ENDIF
!
! 11eme enregistrement PDATIME
!
TZFIELD%CMNHNAME   = TRIM(ygroup)//'.DATIM'
TZFIELD%CSTDNAME   = ''
TZFIELD%CLONGNAME  = TRIM(ygroup)//'.DATIM'
TZFIELD%CUNITS     = ''
TZFIELD%CDIR       = '--'
TZFIELD%CCOMMENT   = TRIM(YCOMMENT)
TZFIELD%NGRID      = tpfields(1)%ngrid
TZFIELD%NTYPE      = TYPEREAL
TZFIELD%NDIMS      = 2
TZFIELD%LTIMEDEP   = .FALSE.

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
subroutine Write_diachro_nc4( tpdiafile, tpfields, hgroup, htype, tpdates, pvar, oicp, ojcp, okcp, kil, kih, kjl, kjh, kkl, kkh, &
                              osplit )

use NETCDF,            only: NF90_DEF_DIM, NF90_DEF_GRP, NF90_DEF_VAR, NF90_INQ_NCID, NF90_PUT_ATT, NF90_PUT_VAR, &
                             NF90_GLOBAL, NF90_NOERR, NF90_STRERROR

use modd_budget,       only: nbutshift, nbusubwrite
use modd_field
use modd_io,           only: isp, tfiledata
use modd_les,          only: nles_masks
use modd_parameters,   only: jphext
use modd_precision,    only: CDFINT, MNHREAL_NF90
use modd_type_date,    only: date_time

use mode_io_field_write, only: IO_Field_create, IO_Field_write, IO_Field_write_box
use mode_io_tools_nc4,   only: IO_Err_handle_nc4

type(tfiledata),                                     intent(in)           :: tpdiafile        ! File to write
class(tfield_metadata_base), dimension(:),           intent(in)           :: tpfields
character(len=*),                                    intent(in)           :: hgroup, htype
type(date_time),             dimension(:),           intent(in)           :: tpdates
real,                        dimension(:,:,:,:,:,:), intent(in)           :: pvar
logical,                                             intent(in)           :: oicp, ojcp, okcp
integer,                                             intent(in), optional :: kil, kih
integer,                                             intent(in), optional :: kjl, kjh
integer,                                             intent(in), optional :: kkl, kkh
logical,                                             intent(in), optional :: osplit

integer              :: icompx, icompy, icompz
integer              :: idims
integer              :: icount
integer              :: icorr
integer              :: ji
integer              :: jp
integer(kind=CDFINT) :: isavencid
integer(kind=CDFINT) :: idimid
integer(kind=CDFINT) :: igrpid
integer(kind=CDFINT) :: istatus
logical              :: gdistributed
logical              :: gsplit
type(tfiledata)      :: tzfile

if ( trim ( htype ) == 'CART' .or. trim ( htype ) == 'MASK' .or. trim ( htype ) == 'SPXY') then
  if (        .not. Present( kil ) .or. .not. Present( kih ) &
         .or. .not. Present( kjl ) .or. .not. Present( kjh ) &
         .or. .not. Present( kkl ) .or. .not. Present( kkh ) ) then
    call Print_msg( NVERB_FATAL, 'IO', 'Write_diachro_nc4', &
                      'kil, kih, kjl, kjh, kkl or kkh not provided for variable ' // Trim( tpfields(1)%cmnhname ) )
  end if
end if

tzfile = tpdiafile

!Write only in netCDF files
tzfile%cformat = 'NETCDF4'

if ( oicp ) then
  icompx = 1
else
  icompx = 0
endif
if ( ojcp ) then
  icompy = 1
else
  icompy = 0
endif
if ( okcp ) then
  icompz = 1
else
  icompz = 0
endif

if ( Trim( htype ) == 'CART' .and. .not. oicp .and. .not. ojcp ) then
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
  istatus = NF90_INQ_NCID( tzfile%nncid, trim( hgroup ), igrpid )
  if ( istatus == NF90_NOERR ) then
    call Print_msg( NVERB_WARNING, 'IO', 'Write_diachro_nc4', trim(tzfile%cname)//': group '//trim(hgroup)//' already defined' )
  else
    istatus = NF90_DEF_GRP( tzfile%nncid, trim( hgroup ), igrpid )
    if ( istatus /= NF90_NOERR ) &
      call IO_Err_handle_nc4( istatus, 'Write_diachro_nc4', 'NF90_DEF_GRP', 'for '//trim(hgroup)//' group' )
  end if

  !Save id of the file root group ('/' group)
  isavencid = tzfile%nncid
  tzfile%nncid = igrpid

  istatus = NF90_PUT_ATT( igrpid, NF90_GLOBAL, 'type', trim( htype ) )
  if (istatus /= NF90_NOERR ) &
    call IO_Err_handle_nc4( istatus, 'Write_diachro_nc4', 'NF90_PUT_ATT', 'type for '//trim(hgroup)//' group' )

  if ( trim ( htype ) == 'CART' .or. trim ( htype ) == 'MASK' .or. trim ( htype ) == 'SPXY') then
    istatus = NF90_PUT_ATT( igrpid, NF90_GLOBAL, 'min x index', kil )
    if (istatus /= NF90_NOERR ) &
      call IO_Err_handle_nc4( istatus, 'Write_diachro_nc4', 'NF90_PUT_ATT', 'min x index for '//trim(hgroup)//' group' )

    istatus = NF90_PUT_ATT( igrpid, NF90_GLOBAL, 'max x index', kih )
    if (istatus /= NF90_NOERR ) &
      call IO_Err_handle_nc4( istatus, 'Write_diachro_nc4', 'NF90_PUT_ATT', 'max x index for '//trim(hgroup)//' group' )

    istatus = NF90_PUT_ATT( igrpid, NF90_GLOBAL, 'min y index', kjl )
    if (istatus /= NF90_NOERR ) &
      call IO_Err_handle_nc4( istatus, 'Write_diachro_nc4', 'NF90_PUT_ATT', 'min y index for '//trim(hgroup)//' group' )

    istatus = NF90_PUT_ATT( igrpid, NF90_GLOBAL, 'max y index', kjh )
    if (istatus /= NF90_NOERR ) &
      call IO_Err_handle_nc4( istatus, 'Write_diachro_nc4', 'NF90_PUT_ATT', 'max y index for '//trim(hgroup)//' group' )

    istatus = NF90_PUT_ATT( igrpid, NF90_GLOBAL, 'min z index', kkl )
    if (istatus /= NF90_NOERR ) &
      call IO_Err_handle_nc4( istatus, 'Write_diachro_nc4', 'NF90_PUT_ATT', 'min z index for '//trim(hgroup)//' group' )

    istatus = NF90_PUT_ATT( igrpid, NF90_GLOBAL, 'max z index', kkh )
    if (istatus /= NF90_NOERR ) &
      call IO_Err_handle_nc4( istatus, 'Write_diachro_nc4', 'NF90_PUT_ATT', 'max z index for '//trim(hgroup)//' group' )
  end if

  if ( trim ( htype ) == 'CART' ) then
    istatus = NF90_PUT_ATT( igrpid, NF90_GLOBAL, 'averaged on x dimension', icompx )
    if (istatus /= NF90_NOERR ) &
      call IO_Err_handle_nc4( istatus, 'Write_diachro_nc4', 'NF90_PUT_ATT', 'averaged on x dimension '//trim(hgroup)//' group' )

    istatus = NF90_PUT_ATT( igrpid, NF90_GLOBAL, 'averaged on y dimension', icompy )
    if (istatus /= NF90_NOERR ) &
      call IO_Err_handle_nc4( istatus, 'Write_diachro_nc4', 'NF90_PUT_ATT', 'averaged on y dimension '//trim(hgroup)//' group' )
  end if

  if ( trim ( htype ) == 'CART' .or. trim ( htype ) == 'MASK' .or. trim ( htype ) == 'SPXY') then
    istatus = NF90_PUT_ATT( igrpid, NF90_GLOBAL, 'averaged on z dimension', icompz )
    if (istatus /= NF90_NOERR ) &
      call IO_Err_handle_nc4( istatus, 'Write_diachro_nc4', 'NF90_PUT_ATT', 'averaged on z dimension '//trim(hgroup)//' group' )
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
    if ( tpfields(jp)%ndimlist(ji) /= tpfields(1)%ndimlist(ji) ) &
      call Print_msg( NVERB_ERROR, 'IO', 'Write_diachro_nc4', &
                      'some dimensions are not the same for all tpfields entries for variable '//trim(tpfields(jp)%cmnhname) )
  end do
end do

!Check that if 'CART' and no horizontal compression, parameters are as expected
if ( htype == 'CART' .and. .not. oicp .and. .not. ojcp ) then
  if ( okcp ) then
    icorr = 1
  else
    icorr = 0
  end if
  if ( ( idims + icorr ) /= 3 .and. ( idims + icorr ) /= 4 ) then
    call Print_msg( NVERB_ERROR, 'IO', 'Write_diachro_nc4',                                                 &
                    'unexpected number of dimensions for CART without horizontal compression for variable ' &
                    // Trim( tpfields(1)%cmnhname ) )
  end if

  if (      (       tpfields(1)%ndimlist(1) /= NMNHDIM_BUDGET_CART_NI              &
              .and. tpfields(1)%ndimlist(1) /= NMNHDIM_BUDGET_CART_NI_U            &
              .and. tpfields(1)%ndimlist(1) /= NMNHDIM_BUDGET_CART_NI_V          ) &
       .or. (       tpfields(1)%ndimlist(2) /= NMNHDIM_BUDGET_CART_NJ              &
              .and. tpfields(1)%ndimlist(2) /= NMNHDIM_BUDGET_CART_NJ_U            &
              .and. tpfields(1)%ndimlist(2) /= NMNHDIM_BUDGET_CART_NJ_V          ) &
       .or. (       .not. okcp                                                     &
              .and. tpfields(1)%ndimlist(3) /= NMNHDIM_BUDGET_CART_LEVEL           &
              .and. tpfields(1)%ndimlist(3) /= NMNHDIM_BUDGET_CART_LEVEL_W       ) &
       .or. ( idims == 4 .and. tpfields(1)%ndimlist(6) /= NMNHDIM_BUDGET_NGROUPS ) ) then
    call Print_msg( NVERB_ERROR, 'IO', 'Write_diachro_nc4',                                       &
                    'unexpected dimensions for CART without horizontal compression for variable ' &
                    // Trim( tpfields(1)%cmnhname ) )
  end if

  if ( .not. Present( kil ) .or. .not. Present( kih ) .or. .not. Present( kjl ) .or. .not. Present( kjh ) ) then
    call Print_msg( NVERB_FATAL, 'IO', 'Write_diachro_nc4',                                                &
                    'kil or kih or kjl or kjh not provided for variable ' // Trim( tpfields(1)%cmnhname ) )
  end if
end if


select case ( idims )
  case (0)
     !Remark: [ integer:: ] is a constructor for a zero-size array of integers, [] is not allowed (type can not be determined)
      call Diachro_one_field_write_nc4( tzfile, tpfields(1), htype, pvar, [ integer:: ], gsplit, gdistributed )

  case (1)

    if ( tpfields(1)%ndimlist(4) == NMNHDIM_BUDGET_LES_TIME .or. tpfields(1)%ndimlist(4) == NMNHDIM_BUDGET_LES_AVG_TIME ) then
      if ( Size( tpfields ) /= 1 ) call Print_msg( NVERB_FATAL, 'IO', 'Write_diachro_nc4', &
                                                   'wrong size of tpfields (variable '//trim(tpfields(1)%cmnhname)//')' )

      call Diachro_one_field_write_nc4( tzfile, tpfields(1), htype, pvar, [ 4 ], gsplit, gdistributed )
    else if ( Any( tpfields(1)%ndimlist(1) == [ NMNHDIM_NI, NMNHDIM_NI_U, NMNHDIM_NI_V, NMNHDIM_BUDGET_CART_NI,     &
                                                NMNHDIM_BUDGET_CART_NI_U, NMNHDIM_BUDGET_CART_NI_V              ] ) ) then
      if ( Size( tpfields ) /= 1 ) call Print_msg( NVERB_FATAL, 'IO', 'Write_diachro_nc4', &
                                                   'wrong size of tpfields (variable '//trim(tpfields(1)%cmnhname)//')' )

      call Diachro_one_field_write_nc4( tzfile, tpfields(1), htype, pvar, [ 1 ], gsplit, gdistributed )

    else if ( Any( tpfields(1)%ndimlist(2) == [ NMNHDIM_NJ, NMNHDIM_NJ_U, NMNHDIM_NJ_V, NMNHDIM_BUDGET_CART_NJ,     &
                                                NMNHDIM_BUDGET_CART_NJ_U, NMNHDIM_BUDGET_CART_NJ_V              ] ) ) then
      if ( Size( tpfields ) /= 1 ) call Print_msg( NVERB_FATAL, 'IO', 'Write_diachro_nc4', &
                                                   'wrong size of tpfields (variable '//trim(tpfields(1)%cmnhname)//')' )
      call Diachro_one_field_write_nc4( tzfile, tpfields(1), htype, pvar, [ 2 ], gsplit, gdistributed )
    else if ( Any( tpfields(1)%ndimlist(3) == [ NMNHDIM_LEVEL, NMNHDIM_LEVEL_W,                            &
                                                NMNHDIM_BUDGET_CART_LEVEL, NMNHDIM_BUDGET_CART_LEVEL_W ] ) ) then
      if ( Size( tpfields ) /= 1 ) call Print_msg( NVERB_FATAL, 'IO', 'Write_diachro_nc4', &
                                                   'wrong size of tpfields (variable '//trim(tpfields(1)%cmnhname)//')' )
      call Diachro_one_field_write_nc4( tzfile, tpfields(1), htype, pvar, [ 3 ], gsplit, gdistributed )
    else if ( tpfields(1)%ndimlist(6) == NMNHDIM_BUDGET_NGROUPS ) then
      do ji = 1, Size( pvar, 6 )
        !Remark: [ integer:: ] is a constructor for a zero-size array of integers, [] is not allowed (type can not be determined)
        call Diachro_one_field_write_nc4( tzfile, tpfields(ji), htype, pvar(:,:,:,:,:,ji:ji), [ integer:: ], gsplit, gdistributed )
      end do
    else
      call Print_msg( NVERB_FATAL, 'IO', 'Write_diachro_nc4', &
                      'case not yet implemented (variable '//trim(tpfields(1)%cmnhname)//')' )
    end if


  case (2)

    if (       Any( tpfields(1)%ndimlist(1) == [ NMNHDIM_NI, NMNHDIM_NI_U, NMNHDIM_NI_V, NMNHDIM_BUDGET_CART_NI, &
                                                 NMNHDIM_BUDGET_CART_NI_U, NMNHDIM_BUDGET_CART_NI_V ] )          &
         .and. Any( tpfields(1)%ndimlist(2) == [ NMNHDIM_NJ, NMNHDIM_NJ_U, NMNHDIM_NJ_V, NMNHDIM_BUDGET_CART_NJ, &
                                                 NMNHDIM_BUDGET_CART_NJ_U, NMNHDIM_BUDGET_CART_NJ_V ] )          ) then
      if ( Size( tpfields ) /= 1 ) call Print_msg( NVERB_FATAL, 'IO', 'Write_diachro_nc4', &
                                                   'wrong size of tpfields (variable '//trim(tpfields(1)%cmnhname)//')' )
      call Diachro_one_field_write_nc4( tzfile, tpfields(1), htype, pvar, [ 1, 2 ], gsplit, gdistributed )
    else if (       Any( tpfields(1)%ndimlist(1) == [ NMNHDIM_NI, NMNHDIM_NI_U, NMNHDIM_NI_V, NMNHDIM_BUDGET_CART_NI, &
                                                      NMNHDIM_BUDGET_CART_NI_U, NMNHDIM_BUDGET_CART_NI_V ] )          &
              .and. Any( tpfields(1)%ndimlist(3) == [ NMNHDIM_LEVEL, NMNHDIM_LEVEL_W,                                 &
                                                      NMNHDIM_BUDGET_CART_LEVEL, NMNHDIM_BUDGET_CART_LEVEL_W ] )      ) then
      if ( Size( tpfields ) /= 1 ) call Print_msg( NVERB_FATAL, 'IO', 'Write_diachro_nc4', &
                                                   'wrong size of tpfields (variable '//trim(tpfields(1)%cmnhname)//')' )
      call Diachro_one_field_write_nc4( tzfile, tpfields(1), htype, pvar, [ 1, 3 ], gsplit, gdistributed )
    else if (       Any( tpfields(1)%ndimlist(2) == [ NMNHDIM_NJ, NMNHDIM_NJ_U, NMNHDIM_NJ_V, NMNHDIM_BUDGET_CART_NJ, &
                                                      NMNHDIM_BUDGET_CART_NJ_U, NMNHDIM_BUDGET_CART_NJ_V ] )          &
              .and. Any( tpfields(1)%ndimlist(3) == [ NMNHDIM_LEVEL, NMNHDIM_LEVEL_W,                                 &
                                                      NMNHDIM_BUDGET_CART_LEVEL, NMNHDIM_BUDGET_CART_LEVEL_W ] )      ) then
      if ( Size( tpfields ) /= 1 ) call Print_msg( NVERB_FATAL, 'IO', 'Write_diachro_nc4', &
                                                   'wrong size of tpfields (variable '//trim(tpfields(1)%cmnhname)//')' )
      call Diachro_one_field_write_nc4( tzfile, tpfields(1), htype, pvar, [ 2, 3 ], gsplit, gdistributed )
    else if (  Any( tpfields(1)%ndimlist(1) == [ NMNHDIM_BUDGET_CART_NI, NMNHDIM_BUDGET_CART_NI_U, NMNHDIM_BUDGET_CART_NI_V ] ) &
              .and. tpfields(1)%ndimlist(6) == NMNHDIM_BUDGET_NGROUPS ) then
      ! Loop on the processes
      do ji = 1, Size( pvar, 6 )
        call Diachro_one_field_write_nc4( tzfile, tpfields(ji), htype, pvar(:,:,:,:,:,ji:ji), [ 1 ], gsplit, gdistributed )
      end do
    else if (  Any( tpfields(1)%ndimlist(2) == [ NMNHDIM_BUDGET_CART_NJ, NMNHDIM_BUDGET_CART_NJ_U, NMNHDIM_BUDGET_CART_NJ_V ] ) &
              .and. tpfields(1)%ndimlist(6) == NMNHDIM_BUDGET_NGROUPS ) then
      ! Loop on the processes
      do ji = 1, Size( pvar, 6 )
        call Diachro_one_field_write_nc4( tzfile, tpfields(ji), htype, pvar(:,:,:,:,:,ji:ji), [ 2 ], gsplit, gdistributed )
      end do
    else if (       tpfields(1)%ndimlist(3) == NMNHDIM_BUDGET_LES_LEVEL     &
       .and. (      tpfields(1)%ndimlist(4) == NMNHDIM_BUDGET_LES_TIME &
               .or. tpfields(1)%ndimlist(4) == NMNHDIM_BUDGET_LES_AVG_TIME ) ) then
      if ( Size( tpfields ) /= 1 ) call Print_msg( NVERB_FATAL, 'IO', 'Write_diachro_nc4', &
                                                   'wrong size of tpfields (variable '//trim(tpfields(1)%cmnhname)//')' )
      call Diachro_one_field_write_nc4( tzfile, tpfields(1), htype, pvar, [ 3, 4 ], gsplit, gdistributed )
    else if ( Any( tpfields(1)%ndimlist(3) == [ NMNHDIM_BUDGET_CART_LEVEL, NMNHDIM_BUDGET_CART_LEVEL_W ] ) &
              .and. tpfields(1)%ndimlist(6) == NMNHDIM_BUDGET_NGROUPS ) then
      ! Loop on the processes
      do ji = 1, Size( pvar, 6 )
        call Diachro_one_field_write_nc4( tzfile, tpfields(ji), htype, pvar(:,:,:,:,:,ji:ji), [ 3 ], gsplit, gdistributed )
      end do
    else if (  tpfields(1)%ndimlist(4) == NMNHDIM_BUDGET_TIME .and. tpfields(1)%ndimlist(5) == NMNHDIM_BUDGET_MASK_NBUMASK ) then
      if ( Size( tpfields ) /= 1 ) call Print_msg( NVERB_FATAL, 'IO', 'Write_diachro_nc4', &
                                                   'wrong size of tpfields (variable '//trim(tpfields(1)%cmnhname)//')' )
      call Diachro_one_field_write_nc4( tzfile, tpfields(1), htype, pvar, [ 4, 5 ], gsplit, gdistributed )
    else if (  (      tpfields(1)%ndimlist(4) == NMNHDIM_BUDGET_LES_TIME &
                 .or. tpfields(1)%ndimlist(4) == NMNHDIM_BUDGET_LES_AVG_TIME ) &
         .and. tpfields(1)%ndimlist(5) == NMNHDIM_BUDGET_LES_SV       ) then
      if ( Size( tpfields ) /= 1 ) call Print_msg( NVERB_FATAL, 'IO', 'Write_diachro_nc4', &
                                                   'wrong size of tpfields (variable '//trim(tpfields(1)%cmnhname)//')' )
      call Diachro_one_field_write_nc4( tzfile, tpfields(1), htype, pvar, [ 4, 5 ], gsplit, gdistributed )
    else if (  tpfields(1)%ndimlist(4) == NMNHDIM_FLYER_TIME &
         .and. tpfields(1)%ndimlist(6) == NMNHDIM_FLYER_PROC ) then
      !Correspond to FLYER_DIACHRO
      !Create local time dimension
      if ( isp == tzfile%nmaster_rank) then
        istatus = NF90_DEF_DIM( igrpid, 'time_flyer', Size( pvar, 4), idimid )
        if ( istatus /= NF90_NOERR ) &
          call IO_Err_handle_nc4( istatus, 'Write_diachro_nc4', 'NF90_DEF_DIM', Trim( tpfields(1)%cmnhname ) )
      end if

      ! Loop on the processes
      do ji = 1, Size( pvar, 6 )
        call Diachro_one_field_write_nc4( tzfile, tpfields(ji), htype, pvar(:,:,:,:,:,ji:ji), [ 4 ], gsplit, gdistributed )
      end do
    else if (  tpfields(1)%ndimlist(4) == NMNHDIM_SERIES_TIME &
         .and. tpfields(1)%ndimlist(6) == NMNHDIM_SERIES_PROC ) then
      !Correspond to WRITE_SERIES_n
      ! Loop on the processes
      do ji = 1, Size( pvar, 6 )
        call Diachro_one_field_write_nc4( tzfile, tpfields(ji), htype, pvar(:,:,:,:,:,ji:ji), [ 4 ], gsplit, gdistributed )
      end do
    else
      call Print_msg( NVERB_FATAL, 'IO', 'Write_diachro_nc4', &
                      'case not yet implemented (variable '//trim(tpfields(1)%cmnhname)//')' )
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
      call Diachro_one_field_write_nc4( tzfile, tpfields(1), htype, pvar, [ 1, 2, 3 ], gsplit, gdistributed, &
                                        kil, kih, kjl, kjh, kkl, kkh )
    else if (       Any(tpfields(1)%ndimlist(1) == [ NMNHDIM_BUDGET_CART_NI, NMNHDIM_BUDGET_CART_NI_U, NMNHDIM_BUDGET_CART_NI_V ]) &
              .and. Any(tpfields(1)%ndimlist(2) == [ NMNHDIM_BUDGET_CART_NJ, NMNHDIM_BUDGET_CART_NJ_U, NMNHDIM_BUDGET_CART_NJ_V ]) &
              .and.     tpfields(1)%ndimlist(6) ==   NMNHDIM_BUDGET_NGROUPS                                                   ) then
      ! Loop on the processes
      do ji = 1, Size( pvar, 6 )
        call Diachro_one_field_write_nc4( tzfile, tpfields(ji), htype, pvar(:,:,:,:,:,ji:ji), [ 1, 2 ], gsplit, gdistributed )
      end do
    else if ( Any ( tpfields(1)%ndimlist(1) == [ NMNHDIM_BUDGET_CART_NI, NMNHDIM_BUDGET_CART_NI_U, NMNHDIM_BUDGET_CART_NI_V ] ) &
              .and. tpfields(1)%ndimlist(3) ==   NMNHDIM_BUDGET_CART_LEVEL                                                      &
              .and. tpfields(1)%ndimlist(6) ==   NMNHDIM_BUDGET_NGROUPS                                                      ) then
      ! Loop on the processes
      do ji = 1, Size( pvar, 6 )
        call Diachro_one_field_write_nc4( tzfile, tpfields(ji), htype, pvar(:,:,:,:,:,ji:ji), [ 1, 3 ], gsplit, gdistributed )
      end do
    else if ( Any ( tpfields(1)%ndimlist(2) == [ NMNHDIM_BUDGET_CART_NJ, NMNHDIM_BUDGET_CART_NJ_U, NMNHDIM_BUDGET_CART_NJ_V ] ) &
              .and. tpfields(1)%ndimlist(3) ==   NMNHDIM_BUDGET_CART_LEVEL                                                      &
              .and. tpfields(1)%ndimlist(6) ==   NMNHDIM_BUDGET_NGROUPS                                                      ) then
      ! Loop on the processes
      do ji = 1, Size( pvar, 6 )
        call Diachro_one_field_write_nc4( tzfile, tpfields(ji), htype, pvar(:,:,:,:,:,ji:ji), [ 2, 3 ], gsplit, gdistributed )
      end do
    else if (         (      tpfields(1)%ndimlist(3) == NMNHDIM_BUDGET_MASK_LEVEL     &
                        .or. tpfields(1)%ndimlist(3) == NMNHDIM_BUDGET_MASK_LEVEL_W ) &
               .and.         tpfields(1)%ndimlist(4) == NMNHDIM_BUDGET_TIME           &
               .and.         tpfields(1)%ndimlist(5) == NMNHDIM_BUDGET_MASK_NBUMASK   ) then
      !Correspond to Store_one_budget_rho (MASK)
      if ( Size( tpfields ) /= 1 ) call Print_msg( NVERB_FATAL, 'IO', 'Write_diachro_nc4', &
                                                   'wrong size of tpfields (variable '//trim(tpfields(1)%cmnhname)//')' )
      call Diachro_one_field_write_nc4( tzfile, tpfields(1), htype, pvar, [ 3, 4, 5 ], gsplit, gdistributed, &
                                        kil, kih, kjl, kjh, kkl, kkh )
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
        call Diachro_one_field_write_nc4( tzfile, tpfields(ji), htype, pvar(:,:,:,:,:,ji:ji), [ 3, 4 ], gsplit, gdistributed )
      end do
    else if (       tpfields(1)%ndimlist(3) == NMNHDIM_BUDGET_LES_LEVEL     &
       .and. (      tpfields(1)%ndimlist(4) == NMNHDIM_BUDGET_LES_TIME &
               .or. tpfields(1)%ndimlist(4) == NMNHDIM_BUDGET_LES_AVG_TIME ) &
       .and. tpfields(1)%ndimlist(6) == NMNHDIM_BUDGET_TERM      ) then
      ! Loop on the processes
      do ji = 1, Size( pvar, 6 )
        call Diachro_one_field_write_nc4( tzfile, tpfields(ji), htype, pvar(:,:,:,:,:,ji:ji), [ 3, 4 ], gsplit, gdistributed )
      end do
    else if (              tpfields(1)%ndimlist(3) == NMNHDIM_BUDGET_LES_LEVEL      &
              .and. (      tpfields(1)%ndimlist(4) == NMNHDIM_BUDGET_LES_TIME       &
                      .or. tpfields(1)%ndimlist(4) == NMNHDIM_BUDGET_LES_AVG_TIME ) &
              .and.        tpfields(1)%ndimlist(5) == NMNHDIM_BUDGET_LES_SV         ) then
      if ( Size( tpfields ) /= 1 ) call Print_msg( NVERB_FATAL, 'IO', 'Write_diachro_nc4', &
                                                   'wrong size of tpfields (variable '//trim(tpfields(1)%cmnhname)//')' )
      call Diachro_one_field_write_nc4( tzfile, tpfields(1), htype, pvar, [ 3, 4, 5 ], gsplit, gdistributed, &
                                        kil, kih, kjl, kjh, kkl, kkh )
    else if (              tpfields(1)%ndimlist(1) == NMNHDIM_SPECTRA_2PTS_NI       &
              .and.        tpfields(1)%ndimlist(3) == NMNHDIM_SPECTRA_LEVEL         &
              .and. (      tpfields(1)%ndimlist(4) == NMNHDIM_BUDGET_LES_TIME       &
                      .or. tpfields(1)%ndimlist(4) == NMNHDIM_BUDGET_LES_AVG_TIME ) ) then
      if ( Size( tpfields ) /= 1 ) call Print_msg( NVERB_FATAL, 'IO', 'Write_diachro_nc4', &
                                                   'wrong size of tpfields (variable '//trim(tpfields(1)%cmnhname)//')' )
      call Diachro_one_field_write_nc4( tzfile, tpfields(1), htype, pvar, [ 1, 3, 4 ], gsplit, gdistributed, &
                                        kil, kih, kjl, kjh, kkl, kkh )
    else if (       tpfields(1)%ndimlist(2) == NMNHDIM_SPECTRA_2PTS_NJ                   &
              .and. tpfields(1)%ndimlist(3) == NMNHDIM_SPECTRA_LEVEL                &
              .and. (      tpfields(1)%ndimlist(4) == NMNHDIM_BUDGET_LES_TIME       &
                      .or. tpfields(1)%ndimlist(4) == NMNHDIM_BUDGET_LES_AVG_TIME ) ) then
      if ( Size( tpfields ) /= 1 ) call Print_msg( NVERB_FATAL, 'IO', 'Write_diachro_nc4', &
                                                   'wrong size of tpfields (variable '//trim(tpfields(1)%cmnhname)//')' )
      call Diachro_one_field_write_nc4( tzfile, tpfields(1), htype, pvar, [ 2, 3, 4 ], gsplit, gdistributed, &
                                        kil, kih, kjl, kjh, kkl, kkh )
    else if (  tpfields(1)%ndimlist(3) == NMNHDIM_LEVEL      &
         .and. tpfields(1)%ndimlist(4) == NMNHDIM_FLYER_TIME &
         .and. tpfields(1)%ndimlist(6) == NMNHDIM_FLYER_PROC ) then
      !Correspond to FLYER_DIACHRO
      !Create local time dimension
      if ( isp == tzfile%nmaster_rank) then
        istatus = NF90_DEF_DIM( igrpid, 'time_flyer', Size( pvar, 4), idimid )
        if ( istatus /= NF90_NOERR ) &
          call IO_Err_handle_nc4( istatus, 'Write_diachro_nc4', 'NF90_DEF_DIM', Trim( tpfields(1)%cmnhname ) )
      end if

      ! Loop on the processes
      do ji = 1, Size( pvar, 6 )
        call Diachro_one_field_write_nc4( tzfile, tpfields(ji), htype, pvar(:,:,:,:,:,ji:ji), [ 3, 4 ], gsplit, gdistributed )
      end do
    else if (  tpfields(1)%ndimlist(3) == NMNHDIM_LEVEL      &
         .and. tpfields(1)%ndimlist(4) == NMNHDIM_PROFILER_TIME &
         .and. tpfields(1)%ndimlist(6) == NMNHDIM_PROFILER_PROC ) then
      !Correspond to PROFILER_DIACHRO_n
      ! Loop on the processes
      do ji = 1, Size( pvar, 6 )
        call Diachro_one_field_write_nc4( tzfile, tpfields(ji), htype, pvar(:,:,:,:,:,ji:ji), [ 3, 4 ], gsplit, gdistributed )
      end do
    else if (  ( tpfields(1)%ndimlist(3) == NMNHDIM_SERIES_LEVEL .or. tpfields(1)%ndimlist(3) == NMNHDIM_SERIES_LEVEL_W ) &
         .and. tpfields(1)%ndimlist(4) == NMNHDIM_SERIES_TIME &
         .and. tpfields(1)%ndimlist(6) == NMNHDIM_SERIES_PROC ) then
      !Correspond to PROFILER_DIACHRO_n
      ! Loop on the processes
      do ji = 1, Size( pvar, 6 )
        call Diachro_one_field_write_nc4( tzfile, tpfields(ji), htype, pvar(:,:,:,:,:,ji:ji), [ 3, 4 ], gsplit, gdistributed )
      end do
    else if (  ( tpfields(1)%ndimlist(1) == NMNHDIM_NI .or. tpfields(1)%ndimlist(1) == NMNHDIM_NI_U )      &
         .and. tpfields(1)%ndimlist(4) == NMNHDIM_SERIES_TIME &
         .and. tpfields(1)%ndimlist(6) == NMNHDIM_SERIES_PROC ) then
      !Correspond to PROFILER_DIACHRO_n
      ! Loop on the processes
      do ji = 1, Size( pvar, 6 )
        call Diachro_one_field_write_nc4( tzfile, tpfields(ji), htype, pvar(:,:,:,:,:,ji:ji), [ 1, 4 ], gsplit, gdistributed )
      end do
    else if (       tpfields(1)%ndimlist(4) == NMNHDIM_BUDGET_TIME         &
              .and. tpfields(1)%ndimlist(5) == NMNHDIM_BUDGET_MASK_NBUMASK &
              .and. tpfields(1)%ndimlist(6) == NMNHDIM_BUDGET_NGROUPS      ) then
      ! Loop on the processes
      do ji = 1, Size( pvar, 6 )
        call Diachro_one_field_write_nc4( tzfile, tpfields(ji), htype, pvar(:,:,:,:,:,ji:ji), [ 4, 5 ], gsplit, gdistributed )
      end do
    else
      call Print_msg( NVERB_FATAL, 'IO', 'Write_diachro_nc4', &
                      'case not yet implemented (variable '//trim(tpfields(1)%cmnhname)//')' )
    end if

  case (4)

    if (       Any( tpfields(1)%ndimlist(1) == [ NMNHDIM_BUDGET_CART_NI, NMNHDIM_BUDGET_CART_NI_U, NMNHDIM_BUDGET_CART_NI_V ] ) &
         .and. Any( tpfields(1)%ndimlist(2) == [ NMNHDIM_BUDGET_CART_NJ, NMNHDIM_BUDGET_CART_NJ_U, NMNHDIM_BUDGET_CART_NJ_V ] ) &
         .and. Any( tpfields(1)%ndimlist(3) == [ NMNHDIM_BUDGET_CART_LEVEL,NMNHDIM_BUDGET_CART_LEVEL_W ] )                      &
         .and.      tpfields(1)%ndimlist(6) == NMNHDIM_BUDGET_NGROUPS                                                        ) then
      !Correspond to Store_one_budget (CART)
      ! Loop on the processes
      do ji = 1, Size( pvar, 6 )
        call Diachro_one_field_write_nc4( tzfile, tpfields(ji), htype, pvar(:,:,:,:,:,ji:ji), [ 1, 2, 3 ], gsplit, gdistributed, &
                                          kil, kih, kjl, kjh, kkl, kkh )
      end do
    elseif (  (        tpfields(1)%ndimlist(3) == NMNHDIM_BUDGET_MASK_LEVEL     &
                  .or. tpfields(1)%ndimlist(3) == NMNHDIM_BUDGET_MASK_LEVEL_W ) &
       .and.           tpfields(1)%ndimlist(4) == NMNHDIM_BUDGET_TIME           &
       .and.           tpfields(1)%ndimlist(5) == NMNHDIM_BUDGET_MASK_NBUMASK   &
       .and.           tpfields(1)%ndimlist(6) == NMNHDIM_BUDGET_NGROUPS        ) then
      !Correspond to Store_one_budget (MASK)
      ! Loop on the processes
      do ji = 1, Size( pvar, 6 )
        call Diachro_one_field_write_nc4( tzfile, tpfields(ji), htype, pvar(:,:,:,:,:,ji:ji), [ 3, 4, 5 ], gsplit, gdistributed, &
                                          kil, kih, kjl, kjh, kkl, kkh )
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
        call Diachro_one_field_write_nc4( tzfile, tpfields(ji), htype, pvar(:,:,:,:,:,ji:ji), [ 3, 4, 5 ], gsplit, gdistributed, &
                                          kil, kih, kjl, kjh, kkl, kkh )
      end do
    else if (              tpfields(1)%ndimlist(3) == NMNHDIM_BUDGET_LES_LEVEL      &
              .and. (      tpfields(1)%ndimlist(4) == NMNHDIM_BUDGET_LES_TIME       &
                      .or. tpfields(1)%ndimlist(4) == NMNHDIM_BUDGET_LES_AVG_TIME ) &
       .and.               tpfields(1)%ndimlist(5) == NMNHDIM_BUDGET_LES_SV         &
       .and.               tpfields(1)%ndimlist(6) == NMNHDIM_BUDGET_TERM           ) then
      ! Loop on the processes
      do ji = 1, Size( pvar, 6 )
        call Diachro_one_field_write_nc4( tzfile, tpfields(ji), htype, pvar(:,:,:,:,:,ji:ji), [ 3, 4, 5 ], gsplit, gdistributed, &
                                          kil, kih, kjl, kjh, kkl, kkh )
      end do
    else if (             tpfields(1)%ndimlist(1) == NMNHDIM_SPECTRA_SPEC_NI       &
             .and.        tpfields(1)%ndimlist(3) == NMNHDIM_SPECTRA_LEVEL         &
             .and. (      tpfields(1)%ndimlist(4) == NMNHDIM_BUDGET_LES_TIME       &
                     .or. tpfields(1)%ndimlist(4) == NMNHDIM_BUDGET_LES_AVG_TIME ) &
             .and.        tpfields(1)%ndimlist(5) == NMNHDIM_COMPLEX               ) then
      !Correspond to LES_DIACHRO_SPEC
      if ( Size( tpfields ) /= 1 ) call Print_msg( NVERB_FATAL, 'IO', 'Write_diachro_nc4', &
                                                   'wrong size of tpfields (variable '//trim(tpfields(1)%cmnhname)//')' )
      call Diachro_one_field_write_nc4( tzfile, tpfields(1), htype, pvar, [ 1, 3, 4, 5 ], gsplit, gdistributed )
    else if (              tpfields(1)%ndimlist(2) == NMNHDIM_SPECTRA_SPEC_NJ       &
              .and.        tpfields(1)%ndimlist(3) == NMNHDIM_SPECTRA_LEVEL         &
              .and. (      tpfields(1)%ndimlist(4) == NMNHDIM_BUDGET_LES_TIME       &
                      .or. tpfields(1)%ndimlist(4) == NMNHDIM_BUDGET_LES_AVG_TIME ) &
              .and.        tpfields(1)%ndimlist(5) == NMNHDIM_COMPLEX               ) then
      !Correspond to LES_DIACHRO_SPEC
      if ( Size( tpfields ) /= 1 ) call Print_msg( NVERB_FATAL, 'IO', 'Write_diachro_nc4', &
                                                   'wrong size of tpfields (variable '//trim(tpfields(1)%cmnhname)//')' )
      call Diachro_one_field_write_nc4( tzfile, tpfields(1), htype, pvar, [ 2, 3, 4, 5 ], gsplit, gdistributed )
    else
      call Print_msg( NVERB_FATAL, 'IO', 'Write_diachro_nc4', &
                      'case not yet implemented (variable '//trim(tpfields(1)%cmnhname)//')' )
    end if

!   case (5)

!   case (6)

  case default
    do ji = 1, Size( pvar, 6 )
      call Diachro_one_field_write_nc4( tzfile, tpfields(ji), htype, pvar(:,:,:,:,:,ji:ji), [ 1, 2, 3, 4, 5 ], &
                                        gsplit, gdistributed )
    end do

end select






!

!Restore id of the file root group ('/' group)
tzfile%nncid = isavencid

end  subroutine Write_diachro_nc4

subroutine Diachro_one_field_write_nc4( tpfile, tpfield, htype, pvar, kdims, osplit, odistributed, kil, kih, kjl, kjh, kkl, kkh )
use modd_budget,      only: nbutshift, nbusubwrite
use modd_field,       only: tfielddata, tfield_metadata_base
use modd_io,          only: isp, tfiledata
use modd_parameters,  only: jphext

use mode_io_field_write, only: IO_Field_create, IO_Field_write, IO_Field_write_box

type(tfiledata),                                     intent(in)  :: tpfile        !File to write
class(tfield_metadata_base),                         intent(in)  :: tpfield
character(len=*),                                    intent(in)  :: htype
real,                        dimension(:,:,:,:,:,:), intent(in)  :: pvar
integer, dimension(:),                               intent(in)  :: kdims        !List of indices of dimensions to use
logical,                                             intent(in)  :: osplit
logical,                                             intent(in)  :: odistributed !.T. if data is distributed among all processes
integer,                                             intent(in), optional :: kil, kih
integer,                                             intent(in), optional :: kjl, kjh
integer,                                             intent(in), optional :: kkl, kkh

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
type(tfielddata) :: tzfield

idims = Size( kdims )

if ( odistributed ) then
  if ( idims /= 3 )                                                                                  &
    call Print_msg( NVERB_FATAL, 'IO', 'Diachro_one_field_write_nc4',                                &
                   'odistributed=.true. not allowed for dims/=3, field: ' //Trim( tzfield%cmnhname ) )

  if ( htype /= 'CART' )                                                                                 &
    call Print_msg( NVERB_FATAL, 'IO', 'Diachro_one_field_write_nc4',                                    &
                   'odistributed=.true. not allowed for htype/=CART, field: ' //Trim( tzfield%cmnhname ) )
end if

if ( osplit ) then
  if ( idims > 3 )                                                                                          &
    call Print_msg( NVERB_FATAL, 'IO', 'Diachro_one_field_write_nc4',                                       &
                                 'osplit=.true. not allowed for dims>3, field: ' //Trim( tzfield%cmnhname ) )

  if ( htype /= 'CART' .and. htype /= 'MASK' )                                                                 &
    call Print_msg( NVERB_FATAL, 'IO', 'Diachro_one_field_write_nc4',                                          &
                    'osplit=.true. not allowed for htype/=CART and /=MASK, field: ' //Trim( tzfield%cmnhname ) )
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
    else !.not. osplit
      call IO_Field_write( tpfile, tzfield, zdata2d )
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
          call IO_Field_write_box( tpfile, tzfield, 'BUDGET',                                                     &
                                 zdata3d, &
                                 kil + jphext, kih + jphext, kjl + jphext, kjh + jphext,                        &
                                 koffset = ioffset                                                              )
        else if ( tzfield%ndims == ( idims + 1 ) ) then
          !A time dimension was added in Prepare_diachro_write
          call IO_Field_write_box( tpfile, tzfield, 'BUDGET',                                                     &
                                 Reshape( zdata3d, [ Size(zdata3d,1), Size(zdata3d,2), Size(zdata3d,3), 1 ] ) , &
                                 kil + jphext, kih + jphext, kjl + jphext, kjh + jphext,                        &
                                 koffset = ioffset                                                              )
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
use modd_field, only: NMNHDIM_BUDGET_TIME, NMNHDIM_UNUSED, NMNHMAXDIMS, tfielddata, tfield_metadata_base

class(tfield_metadata_base), intent(in)  :: tpfieldin
type(tfielddata),            intent(out) :: tpfieldout
integer, dimension(:),       intent(in)  :: kdims ! List of indices of dimensions to use
logical,                     intent(in)  :: osplit
logical,                     intent(in)  :: odistributed ! .true. if data is distributed among all the processes
integer,                     intent(out) :: kbutimepos

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
#endif

end module mode_write_diachro
