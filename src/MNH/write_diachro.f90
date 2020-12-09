!MNH_LIC Copyright 1996-2020 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
module mode_write_diachro

implicit none

private

public :: Write_diachro

contains

! #########################################################################
subroutine Write_diachro( tpdiafile, tpfields, hgroup, htype,             &
                          tpdates, pvar,                                  &
                          oicp, ojcp, okcp, kil, kih, kjl, kjh, kkl, kkh, &
                          ptrajx, ptrajy, ptrajz )
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
use mode_msg
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

if ( tpdiafile%cformat == 'LFI' .or. tpdiafile%cformat == 'LFICDF4' ) &
  call Write_diachro_lfi( tpdiafile, tpfields, hgroup, htype, tpdates, pvar, gicp, gjcp, gkcp, kil, kih, kjl, kjh, kkl, kkh, &
                          ptrajx, ptrajy, ptrajz )

#ifdef MNH_IOCDF4
if ( tpdiafile%cformat == 'NETCDF4' .or. tpdiafile%cformat == 'LFICDF4' ) &
  call Write_diachro_nc4( tpdiafile, tpfields, hgroup, htype, tpdates, pvar, gicp, gjcp, gkcp, kil, kih, kjl, kjh, kkl, kkh, &
                          osplit )
#endif

lpack = gpack

end subroutine Write_diachro

!-----------------------------------------------------------------------------
subroutine Write_diachro_lfi( tpdiafile, tpfields, hgroup, htype, tpdates, pvar, oicp, ojcp, okcp, kil, kih, kjl, kjh, kkl, kkh, &
                              ptrajx, ptrajy, ptrajz )

use modd_budget,         only: nbumask, nbutshift, nbuwrnb
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
use mode_msg
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
  ITMASK=NBUWRNB
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

zdatime(1,  : ) = tdtexp%tdate%year
zdatime(2,  : ) = tdtexp%tdate%month
zdatime(3,  : ) = tdtexp%tdate%day
zdatime(4,  : ) = tdtexp%time
zdatime(5,  : ) = tdtseg%tdate%year
zdatime(6,  : ) = tdtseg%tdate%month
zdatime(7,  : ) = tdtseg%tdate%day
zdatime(8,  : ) = tdtseg%time
zdatime(9,  : ) = tdtmod%tdate%year
zdatime(10, : ) = tdtmod%tdate%month
zdatime(11, : ) = tdtmod%tdate%day
zdatime(12, : ) = tdtmod%time
zdatime(13, : ) = tpdates(:)%tdate%year
zdatime(14, : ) = tpdates(:)%tdate%month
zdatime(15, : ) = tpdates(:)%tdate%day
zdatime(16, : ) = tpdates(:)%time

call IO_Field_write( tzfile, tzfield, zdatime )

deallocate( zdatime )

call Menu_diachro( tzfile, ygroup )

end subroutine Write_diachro_lfi
!-----------------------------------------------------------------------------
#ifdef MNH_IOCDF4
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
use mode_menu_diachro,   only: Menu_diachro
use mode_msg

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

character(len=3)                          :: ynum
integer                                   :: icompx, icompy, icompz
integer                                   :: idims
integer                                   :: icount
integer                                   :: ji
integer                                   :: jp
integer(kind=CDFINT)                      :: isavencid
integer(kind=CDFINT)                      :: idimid
integer(kind=CDFINT)                      :: igrpid
integer(kind=CDFINT)                      :: istatus
integer(kind=CDFINT)                      :: idimtimeid
logical                                   :: gsplit
real                                      :: zdata0d
real, dimension(:),           allocatable :: zdata1d
real, dimension(:,:),         allocatable :: zdata2d
real, dimension(:,:,:),       allocatable :: zdata3d
real, dimension(:,:,:,:),     allocatable :: zdata4d
real, dimension(:,:,:,:,:),   allocatable :: zdata5d
real, dimension(:,:,:,:,:,:), allocatable :: zdata6d
type(tfielddata)                          :: tzfield
type(tfiledata)                           :: tzfile

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
    zdata0d = pvar(1, 1, 1, 1, 1, 1)

    TZFIELD%CMNHNAME   = tpfields(1)%cmnhname
    TZFIELD%CSTDNAME   = tpfields(1)%cstdname
    TZFIELD%CLONGNAME  = tpfields(1)%clongname
    TZFIELD%CUNITS     = tpfields(1)%cunits
    TZFIELD%CDIR       = '--'
    TZFIELD%CCOMMENT   = tpfields(1)%ccomment
    TZFIELD%NGRID      = tpfields(1)%ngrid
    TZFIELD%NTYPE      = tpfields(1)%ntype
    TZFIELD%NDIMS      = 0
    TZFIELD%LTIMEDEP   = .FALSE.

    if ( gsplit ) then
      if ( htype /= 'CART' ) call Print_msg( NVERB_ERROR, 'IO', 'Write_diachro_nc4', Trim( tzfile%cname ) // &
                  ': group ' // Trim( hgroup ) //': gsplit=T not implemented for these dimensions and htype/=CART' )

      !Add budget time dimension
      tzfield%ndims = 1
      tzfield%ndimlist(1)  = NMNHDIM_BUDGET_TIME
      tzfield%ndimlist(2:) = NMNHDIM_UNUSED

      !Create the metadata of the field (has to be done only once)
      if ( nbutshift == 1 ) call IO_Field_create( tzfile, tzfield )

      call IO_Field_write( tzfile, tzfield, [ zdata0d ], koffset= [ ( nbutshift - 1 ) * nbusubwrite ] )
    else
      call IO_Field_write( tzfile, tzfield, zdata0d )
    end if

  case (1)

    if ( tpfields(1)%ndimlist(4) == NMNHDIM_BUDGET_LES_TIME .or. tpfields(1)%ndimlist(4) == NMNHDIM_BUDGET_LES_AVG_TIME ) then
      if ( gsplit ) then
        call Print_msg( NVERB_ERROR, 'IO', 'Write_diachro_nc4', Trim( tzfile%cname ) // &
                        ': group ' // Trim( hgroup ) //' gsplit=T not implemented' )
      end if

      if ( Size( tpfields ) /= 1 ) call Print_msg( NVERB_FATAL, 'IO', 'Write_diachro_nc4', &
                                                   'wrong size of tpfields (variable '//trim(tpfields(1)%cmnhname)//')' )

      tzfield%ndimlist(1)  = tpfields(1)%ndimlist(4)
      tzfield%ndimlist(2:) = NMNHDIM_UNUSED

      allocate( zdata1d( size(pvar,4) ) )

      zdata1d(:) = pvar(1, 1, 1, :, 1, 1)

      TZFIELD%CMNHNAME   = tpfields(1)%cmnhname
      TZFIELD%CSTDNAME   = tpfields(1)%cstdname
      TZFIELD%CLONGNAME  = tpfields(1)%clongname
      TZFIELD%CUNITS     = tpfields(1)%cunits
      TZFIELD%CDIR       = '--'
      TZFIELD%CCOMMENT   = tpfields(1)%ccomment
      TZFIELD%NGRID      = tpfields(1)%ngrid
      TZFIELD%NTYPE      = tpfields(1)%ntype
      TZFIELD%NDIMS      = 1
      TZFIELD%LTIMEDEP   = .FALSE.
      CALL IO_Field_write( tzfile, tzfield, zdata1d )

      deallocate( zdata1d )
    else if (      tpfields(1)%ndimlist(1) == NMNHDIM_NI   &
              .or. tpfields(1)%ndimlist(1) == NMNHDIM_NI_U   &
              .or. tpfields(1)%ndimlist(1) == NMNHDIM_NI_V   &
              .or. tpfields(1)%ndimlist(1) == NMNHDIM_BUDGET_CART_NI   &
              .or. tpfields(1)%ndimlist(1) == NMNHDIM_BUDGET_CART_NI_U &
              .or. tpfields(1)%ndimlist(1) == NMNHDIM_BUDGET_CART_NI_V ) then
      if ( Size( tpfields ) /= 1 ) call Print_msg( NVERB_FATAL, 'IO', 'Write_diachro_nc4', &
                                                   'wrong size of tpfields (variable '//trim(tpfields(1)%cmnhname)//')' )

      tzfield%ndimlist(1) = tpfields(1)%ndimlist(1)
      tzfield%ndimlist(2:) = NMNHDIM_UNUSED

      allocate( zdata1d( size(pvar,1) ) )

      zdata1d(:) = pvar(:, 1, 1, 1, 1, 1)

      TZFIELD%CMNHNAME   = tpfields(1)%cmnhname
      TZFIELD%CSTDNAME   = tpfields(1)%cstdname
      TZFIELD%CLONGNAME  = tpfields(1)%clongname
      TZFIELD%CUNITS     = tpfields(1)%cunits
      TZFIELD%CDIR       = '--'
      TZFIELD%CCOMMENT   = tpfields(1)%ccomment
      TZFIELD%NGRID      = tpfields(1)%ngrid
      TZFIELD%NTYPE      = tpfields(1)%ntype
      TZFIELD%NDIMS      = 1
      TZFIELD%LTIMEDEP   = .FALSE.

      if ( gsplit ) then
        if ( htype /= 'CART' ) call Print_msg( NVERB_ERROR, 'IO', 'Write_diachro_nc4', Trim( tzfile%cname ) // &
                    ': group ' // Trim( hgroup ) //': gsplit=T not implemented for these dimensions and htype/=CART' )

        !Add budget time dimension
        tzfield%ndims = 2
        tzfield%ndimlist(2) = NMNHDIM_BUDGET_TIME

        !Create the metadata of the field (has to be done only once)
        if ( nbutshift == 1 ) call IO_Field_create( tzfile, tzfield )

        call IO_Field_write( tzfile, tzfield, Reshape( zdata1d, [ Size(zdata1d,1), 1 ] ), &
                             koffset= [ 0, ( nbutshift - 1 ) * nbusubwrite ] )
      else
        call IO_Field_write( tzfile, tzfield, zdata1d )
      end if

      deallocate( zdata1d )
    else if (      tpfields(1)%ndimlist(2) == NMNHDIM_NJ   &
              .or. tpfields(1)%ndimlist(2) == NMNHDIM_NJ_U   &
              .or. tpfields(1)%ndimlist(2) == NMNHDIM_NJ_V   &
              .or. tpfields(1)%ndimlist(2) == NMNHDIM_BUDGET_CART_NJ   &
              .or. tpfields(1)%ndimlist(2) == NMNHDIM_BUDGET_CART_NJ_U &
              .or. tpfields(1)%ndimlist(2) == NMNHDIM_BUDGET_CART_NJ_V ) then
      if ( Size( tpfields ) /= 1 ) call Print_msg( NVERB_FATAL, 'IO', 'Write_diachro_nc4', &
                                                   'wrong size of tpfields (variable '//trim(tpfields(1)%cmnhname)//')' )

      tzfield%ndimlist(1)  = tpfields(1)%ndimlist(2)
      tzfield%ndimlist(2:) = NMNHDIM_UNUSED

      allocate( zdata1d( size(pvar,2) ) )

      zdata1d(:) = pvar(1, :, 1, 1, 1, 1)

      TZFIELD%CMNHNAME   = tpfields(1)%cmnhname
      TZFIELD%CSTDNAME   = tpfields(1)%cstdname
      TZFIELD%CLONGNAME  = tpfields(1)%clongname
      TZFIELD%CUNITS     = tpfields(1)%cunits
      TZFIELD%CDIR       = '--'
      TZFIELD%CCOMMENT   = tpfields(1)%ccomment
      TZFIELD%NGRID      = tpfields(1)%ngrid
      TZFIELD%NTYPE      = tpfields(1)%ntype
      TZFIELD%NDIMS      = 1
      TZFIELD%LTIMEDEP   = .FALSE.

      if ( gsplit ) then
        if ( htype /= 'CART' ) call Print_msg( NVERB_ERROR, 'IO', 'Write_diachro_nc4', Trim( tzfile%cname ) // &
                    ': group ' // Trim( hgroup ) //': gsplit=T not implemented for these dimensions and htype/=CART' )

        !Add budget time dimension
        tzfield%ndims = 2
        tzfield%ndimlist(2) = NMNHDIM_BUDGET_TIME

        !Create the metadata of the field (has to be done only once)
        if ( nbutshift == 1 ) call IO_Field_create( tzfile, tzfield )

        call IO_Field_write( tzfile, tzfield, Reshape( zdata1d, [ Size(zdata1d,1), 1 ] ), &
                             koffset= [ 0, ( nbutshift - 1 ) * nbusubwrite ] )
      else
        call IO_Field_write( tzfile, tzfield, zdata1d )
      end if

      deallocate( zdata1d )
    else if (      tpfields(1)%ndimlist(3) == NMNHDIM_LEVEL   &
              .or. tpfields(1)%ndimlist(3) == NMNHDIM_LEVEL_W   &
              .or. tpfields(1)%ndimlist(3) == NMNHDIM_BUDGET_CART_LEVEL   &
              .or. tpfields(1)%ndimlist(3) == NMNHDIM_BUDGET_CART_LEVEL_W ) then
      if ( Size( tpfields ) /= 1 ) call Print_msg( NVERB_FATAL, 'IO', 'Write_diachro_nc4', &
                                                   'wrong size of tpfields (variable '//trim(tpfields(1)%cmnhname)//')' )

      tzfield%ndimlist(1)  = tpfields(1)%ndimlist(3)
      tzfield%ndimlist(2:) = NMNHDIM_UNUSED

      allocate( zdata1d( size(pvar,3) ) )

      zdata1d(:) = pvar(1, 1, :, 1, 1, 1)

      TZFIELD%CMNHNAME   = tpfields(1)%cmnhname
      TZFIELD%CSTDNAME   = tpfields(1)%cstdname
      TZFIELD%CLONGNAME  = tpfields(1)%clongname
      TZFIELD%CUNITS     = tpfields(1)%cunits
      TZFIELD%CDIR       = '--'
      TZFIELD%CCOMMENT   = tpfields(1)%ccomment
      TZFIELD%NGRID      = tpfields(1)%ngrid
      TZFIELD%NTYPE      = tpfields(1)%ntype
      TZFIELD%NDIMS      = 1
      TZFIELD%LTIMEDEP   = .FALSE.

      if ( gsplit ) then
        if ( htype /= 'CART' ) call Print_msg( NVERB_ERROR, 'IO', 'Write_diachro_nc4', Trim( tzfile%cname ) // &
                    ': group ' // Trim( hgroup ) //': gsplit=T not implemented for these dimensions and htype/=CART' )

        !Add budget time dimension
        tzfield%ndims = 2
        tzfield%ndimlist(2) = NMNHDIM_BUDGET_TIME

        !Create the metadata of the field (has to be done only once)
        if ( nbutshift == 1 ) call IO_Field_create( tzfile, tzfield )

        call IO_Field_write( tzfile, tzfield, Reshape( zdata1d, [ Size(zdata1d,1), 1 ] ), &
                             koffset= [ 0, ( nbutshift - 1 ) * nbusubwrite ] )
      else
        call IO_Field_write( tzfile, tzfield, zdata1d )
      end if

      deallocate( zdata1d )

    else if ( tpfields(1)%ndimlist(6) == NMNHDIM_BUDGET_NGROUPS ) then
      tzfield%ndimlist(:) = NMNHDIM_UNUSED

      do ji = 1, Size( pvar, 6 )
        zdata0d = pvar(1, 1, 1, 1, 1, ji)

        tzfield%cmnhname   = tpfields(ji)%cmnhname
        tzfield%cstdname   = tpfields(ji)%cstdname
        tzfield%clongname  = tpfields(ji)%clongname
        tzfield%cunits     = tpfields(ji)%cunits
        tzfield%cdir       = '--'
        tzfield%ccomment   = tpfields(ji)%ccomment
        tzfield%ngrid      = tpfields(ji)%ngrid
        tzfield%ntype      = tpfields(ji)%ntype
        tzfield%ndims      = 2
        tzfield%ltimedep   = .false.

        if ( gsplit ) then
          !Add budget time dimension
          tzfield%ndims = 1
          tzfield%ndimlist(1) = NMNHDIM_BUDGET_TIME

          !Create the metadata of the field (has to be done only once)
          if ( nbutshift == 1 ) call IO_Field_create( tzfile, tzfield )

          call IO_Field_write( tzfile, tzfield, [ zdata0d ], koffset= [ ( nbutshift - 1 ) * nbusubwrite ] )
        else
          call IO_Field_write( tzfile, tzfield, zdata0d )
        end if
      end do

    else
      call Print_msg( NVERB_FATAL, 'IO', 'Write_diachro_nc4', &
                      'case not yet implemented (variable '//trim(tpfields(1)%cmnhname)//')' )

      if ( gsplit ) then
        call Print_msg( NVERB_ERROR, 'IO', 'Write_diachro_nc4', Trim( tzfile%cname ) // &
                        ': group ' // Trim( hgroup ) //' gsplit=T not implemented' )
      end if

    end if


  case (2)

    if (       ( tpfields(1)%ndimlist(1) == NMNHDIM_NI .or. tpfields(1)%ndimlist(1) == NMNHDIM_NI_U .or. &
                 tpfields(1)%ndimlist(1) == NMNHDIM_NI_V .or. tpfields(1)%ndimlist(1) == NMNHDIM_BUDGET_CART_NI &
                 .or. tpfields(1)%ndimlist(1) == NMNHDIM_BUDGET_CART_NI_U &
                 .or. tpfields(1)%ndimlist(1) == NMNHDIM_BUDGET_CART_NI_V )     &
         .and. ( tpfields(1)%ndimlist(2) == NMNHDIM_NJ .or. tpfields(1)%ndimlist(2) == NMNHDIM_NJ_U .or. &
                  tpfields(1)%ndimlist(2) == NMNHDIM_NJ_V .or. tpfields(1)%ndimlist(2) == NMNHDIM_BUDGET_CART_NJ .or. &
                  tpfields(1)%ndimlist(2) == NMNHDIM_BUDGET_CART_NJ_U &
                 .or. tpfields(1)%ndimlist(2) == NMNHDIM_BUDGET_CART_NJ_V ) ) then
      if ( Size( tpfields ) /= 1 ) call Print_msg( NVERB_FATAL, 'IO', 'Write_diachro_nc4', &
                                                   'wrong size of tpfields (variable '//trim(tpfields(1)%cmnhname)//')' )

      tzfield%ndimlist(1)  = tpfields(1)%ndimlist(1)
      tzfield%ndimlist(2)  = tpfields(1)%ndimlist(2)
      tzfield%ndimlist(3:) = NMNHDIM_UNUSED

      allocate( zdata2d( size(pvar,1), size(pvar,2) ) )

      zdata2d(:,:) = pvar(:, :, 1, 1, 1, 1)

      TZFIELD%CMNHNAME   = tpfields(1)%cmnhname
      TZFIELD%CSTDNAME   = tpfields(1)%cstdname
      TZFIELD%CLONGNAME  = tpfields(1)%clongname
      TZFIELD%CUNITS     = tpfields(1)%cunits
      TZFIELD%CDIR       = '--'
      TZFIELD%CCOMMENT   = tpfields(1)%ccomment
      TZFIELD%NGRID      = tpfields(1)%ngrid
      TZFIELD%NTYPE      = tpfields(1)%ntype
      TZFIELD%NDIMS      = 2
      TZFIELD%LTIMEDEP   = .FALSE.

      if ( gsplit ) then
        if ( htype /= 'CART' ) call Print_msg( NVERB_ERROR, 'IO', 'Write_diachro_nc4', Trim( tzfile%cname ) // &
                    ': group ' // Trim( hgroup ) //': gsplit=T not implemented for these dimensions and htype/=CART' )

        !Add budget time dimension
        tzfield%ndims = 3
        tzfield%ndimlist(3) = NMNHDIM_BUDGET_TIME

        !Create the metadata of the field (has to be done only once)
        if ( nbutshift == 1 ) call IO_Field_create( tzfile, tzfield )

        call IO_Field_write( tzfile, tzfield, Reshape( zdata2d, [ Size(zdata2d,1), Size(zdata2d,2), 1 ] ), &
                             koffset= [ 0, 0, ( nbutshift - 1 ) * nbusubwrite ] )
      else
        call IO_Field_write( tzfile, tzfield, zdata2d )
      end if

      deallocate( zdata2d )

    else if (       ( tpfields(1)%ndimlist(1) == NMNHDIM_NI .or. tpfields(1)%ndimlist(1) == NMNHDIM_NI_U &
                      .or. tpfields(1)%ndimlist(1) == NMNHDIM_NI_V .or. tpfields(1)%ndimlist(1) == NMNHDIM_BUDGET_CART_NI &
                      .or. tpfields(1)%ndimlist(1) == NMNHDIM_BUDGET_CART_NI_U &
                 .or. tpfields(1)%ndimlist(1) == NMNHDIM_BUDGET_CART_NI_V )     &
       .and.  ( tpfields(1)%ndimlist(3) == NMNHDIM_LEVEL .or. tpfields(1)%ndimlist(3) == NMNHDIM_LEVEL_W &
           .or. tpfields(1)%ndimlist(3) == NMNHDIM_BUDGET_CART_LEVEL &
           .or. tpfields(1)%ndimlist(3) == NMNHDIM_BUDGET_CART_LEVEL_W ) ) then

      if ( Size( tpfields ) /= 1 ) call Print_msg( NVERB_FATAL, 'IO', 'Write_diachro_nc4', &
                                                   'wrong size of tpfields (variable '//trim(tpfields(1)%cmnhname)//')' )

      tzfield%ndimlist(1)  = tpfields(1)%ndimlist(1)
      tzfield%ndimlist(2)  = tpfields(1)%ndimlist(3)
      tzfield%ndimlist(3:) = NMNHDIM_UNUSED

      allocate( zdata2d( size(pvar,1), size(pvar,3) ) )

      zdata2d(:,:) = pvar(:, 1, :, 1, 1, 1)

      TZFIELD%CMNHNAME   = tpfields(1)%cmnhname
      TZFIELD%CSTDNAME   = tpfields(1)%cstdname
      TZFIELD%CLONGNAME  = tpfields(1)%clongname
      TZFIELD%CUNITS     = tpfields(1)%cunits
      TZFIELD%CDIR       = '--'
      TZFIELD%CCOMMENT   = tpfields(1)%ccomment
      TZFIELD%NGRID      = tpfields(1)%ngrid
      TZFIELD%NTYPE      = tpfields(1)%ntype
      TZFIELD%NDIMS      = 2
      TZFIELD%LTIMEDEP   = .FALSE.

      if ( gsplit ) then
        if ( htype /= 'CART' ) call Print_msg( NVERB_ERROR, 'IO', 'Write_diachro_nc4', Trim( tzfile%cname ) // &
                    ': group ' // Trim( hgroup ) //': gsplit=T not implemented for these dimensions and htype/=CART' )

        !Add budget time dimension
        tzfield%ndims = 3
        tzfield%ndimlist(3) = NMNHDIM_BUDGET_TIME

        !Create the metadata of the field (has to be done only once)
        if ( nbutshift == 1 ) call IO_Field_create( tzfile, tzfield )

        call IO_Field_write( tzfile, tzfield, Reshape( zdata2d, [ Size(zdata2d,1), Size(zdata2d,2), 1 ] ), &
                             koffset= [ 0, 0, ( nbutshift - 1 ) * nbusubwrite ] )
      else
        call IO_Field_write( tzfile, tzfield, zdata2d )
      end if

      deallocate( zdata2d )

    else if (       ( tpfields(1)%ndimlist(2) == NMNHDIM_NJ .or. tpfields(1)%ndimlist(2) == NMNHDIM_NJ_U &
                     .or. tpfields(1)%ndimlist(2) == NMNHDIM_NJ_V .or. tpfields(1)%ndimlist(2) == NMNHDIM_BUDGET_CART_NJ &
                     .or. tpfields(1)%ndimlist(2) == NMNHDIM_BUDGET_CART_NJ_U &
                 .or. tpfields(1)%ndimlist(2) == NMNHDIM_BUDGET_CART_NJ_V )     &
       .and.  ( tpfields(1)%ndimlist(3) == NMNHDIM_LEVEL .or. tpfields(1)%ndimlist(3) == NMNHDIM_LEVEL_W &
           .or. tpfields(1)%ndimlist(3) == NMNHDIM_BUDGET_CART_LEVEL &
           .or. tpfields(1)%ndimlist(3) == NMNHDIM_BUDGET_CART_LEVEL_W ) ) then

      if ( Size( tpfields ) /= 1 ) call Print_msg( NVERB_FATAL, 'IO', 'Write_diachro_nc4', &
                                                   'wrong size of tpfields (variable '//trim(tpfields(1)%cmnhname)//')' )

      tzfield%ndimlist(1)  = tpfields(1)%ndimlist(2)
      tzfield%ndimlist(2)  = tpfields(1)%ndimlist(3)
      tzfield%ndimlist(3:) = NMNHDIM_UNUSED

      allocate( zdata2d( size(pvar,2), size(pvar,3) ) )

      zdata2d(:,:) = pvar(1, :, :, 1, 1, 1)

      TZFIELD%CMNHNAME   = tpfields(1)%cmnhname
      TZFIELD%CSTDNAME   = tpfields(1)%cstdname
      TZFIELD%CLONGNAME  = tpfields(1)%clongname
      TZFIELD%CUNITS     = tpfields(1)%cunits
      TZFIELD%CDIR       = '--'
      TZFIELD%CCOMMENT   = tpfields(1)%ccomment
      TZFIELD%NGRID      = tpfields(1)%ngrid
      TZFIELD%NTYPE      = tpfields(1)%ntype
      TZFIELD%NDIMS      = 2
      TZFIELD%LTIMEDEP   = .FALSE.

      if ( gsplit ) then
        if ( htype /= 'CART' ) call Print_msg( NVERB_ERROR, 'IO', 'Write_diachro_nc4', Trim( tzfile%cname ) // &
                    ': group ' // Trim( hgroup ) //': gsplit=T not implemented for these dimensions and htype/=CART' )

        !Add budget time dimension
        tzfield%ndims = 3
        tzfield%ndimlist(3) = NMNHDIM_BUDGET_TIME

        !Create the metadata of the field (has to be done only once)
        if ( nbutshift == 1 ) call IO_Field_create( tzfile, tzfield )

        call IO_Field_write( tzfile, tzfield, Reshape( zdata2d, [ Size(zdata2d,1), Size(zdata2d,2), 1 ] ), &
                             koffset= [ 0, 0, ( nbutshift - 1 ) * nbusubwrite ] )
      else
        call IO_Field_write( tzfile, tzfield, zdata2d )
      end if

      deallocate( zdata2d )

    else if (  Any( tpfields(1)%ndimlist(1) == [ NMNHDIM_BUDGET_CART_NI, NMNHDIM_BUDGET_CART_NI_U, NMNHDIM_BUDGET_CART_NI_V ] ) &
              .and. tpfields(1)%ndimlist(6) == NMNHDIM_BUDGET_NGROUPS ) then
      tzfield%ndimlist(1)  = tpfields(1)%ndimlist(1)
      tzfield%ndimlist(2:) = NMNHDIM_UNUSED

      Allocate( zdata1d(Size( pvar, 1 )) )

      do ji = 1, Size( pvar, 6 )
        zdata1d(:) = pvar(:, 1, 1, 1, 1, ji)

        tzfield%cmnhname   = tpfields(ji)%cmnhname
        tzfield%cstdname   = tpfields(ji)%cstdname
        tzfield%clongname  = tpfields(ji)%clongname
        tzfield%cunits     = tpfields(ji)%cunits
        tzfield%cdir       = '--'
        tzfield%ccomment   = tpfields(ji)%ccomment
        tzfield%ngrid      = tpfields(ji)%ngrid
        tzfield%ntype      = tpfields(ji)%ntype
        tzfield%ndims      = 2
        tzfield%ltimedep   = .false.

        if ( gsplit ) then
          !Add budget time dimension
          tzfield%ndims = 2
          tzfield%ndimlist(2) = NMNHDIM_BUDGET_TIME

          !Create the metadata of the field (has to be done only once)
          if ( nbutshift == 1 ) call IO_Field_create( tzfile, tzfield )

          call IO_Field_write( tzfile, tzfield, Reshape( zdata1d, [ Size(zdata1d,1), 1 ] ), &
                               koffset= [ 0, ( nbutshift - 1 ) * nbusubwrite ] )
        else
          call IO_Field_write( tzfile, tzfield, zdata1d )
        end if
      end do

      Deallocate( zdata1d )

    else if (  Any( tpfields(1)%ndimlist(2) == [ NMNHDIM_BUDGET_CART_NJ, NMNHDIM_BUDGET_CART_NJ_U, NMNHDIM_BUDGET_CART_NJ_V ] ) &
              .and. tpfields(1)%ndimlist(6) == NMNHDIM_BUDGET_NGROUPS ) then
      tzfield%ndimlist(1)  = tpfields(1)%ndimlist(2)
      tzfield%ndimlist(2:) = NMNHDIM_UNUSED

      Allocate( zdata1d(Size( pvar, 2 )) )

      do ji = 1, Size( pvar, 6 )
        zdata1d(:) = pvar(1, :, 1, 1, 1, ji)

        tzfield%cmnhname   = tpfields(ji)%cmnhname
        tzfield%cstdname   = tpfields(ji)%cstdname
        tzfield%clongname  = tpfields(ji)%clongname
        tzfield%cunits     = tpfields(ji)%cunits
        tzfield%cdir       = '--'
        tzfield%ccomment   = tpfields(ji)%ccomment
        tzfield%ngrid      = tpfields(ji)%ngrid
        tzfield%ntype      = tpfields(ji)%ntype
        tzfield%ndims      = 2
        tzfield%ltimedep   = .false.

        if ( gsplit ) then
          !Add budget time dimension
          tzfield%ndims = 2
          tzfield%ndimlist(2) = NMNHDIM_BUDGET_TIME

          !Create the metadata of the field (has to be done only once)
          if ( nbutshift == 1 ) call IO_Field_create( tzfile, tzfield )

          call IO_Field_write( tzfile, tzfield, Reshape( zdata1d, [ Size(zdata1d,1), 1 ] ), &
                               koffset= [ 0, ( nbutshift - 1 ) * nbusubwrite ] )
        else
          call IO_Field_write( tzfile, tzfield, zdata1d )
        end if
      end do

      Deallocate( zdata1d )

    else if (       tpfields(1)%ndimlist(3) == NMNHDIM_BUDGET_LES_LEVEL     &
       .and. (      tpfields(1)%ndimlist(4) == NMNHDIM_BUDGET_LES_TIME &
               .or. tpfields(1)%ndimlist(4) == NMNHDIM_BUDGET_LES_AVG_TIME ) ) then
      if ( gsplit ) then
          call Print_msg( NVERB_ERROR, 'IO', 'Write_diachro_nc4', Trim( tzfile%cname ) // &
                          ': group ' // Trim( hgroup ) //' gsplit=T not implemented' )
      end if

      if ( Size( tpfields ) /= 1 ) call Print_msg( NVERB_FATAL, 'IO', 'Write_diachro_nc4', &
                                                   'wrong size of tpfields (variable '//trim(tpfields(1)%cmnhname)//')' )

      tzfield%ndimlist(1)  = tpfields(1)%ndimlist(3)
      tzfield%ndimlist(2)  = tpfields(1)%ndimlist(4)
      tzfield%ndimlist(3:) = NMNHDIM_UNUSED

      allocate( zdata2d( size(pvar,3), size(pvar,4) ) )

      zdata2d(:,:) = pvar(1, 1, :, :, 1, 1)

      TZFIELD%CMNHNAME   = tpfields(1)%cmnhname
      TZFIELD%CSTDNAME   = tpfields(1)%cstdname
      TZFIELD%CLONGNAME  = tpfields(1)%clongname
      TZFIELD%CUNITS     = tpfields(1)%cunits
      TZFIELD%CDIR       = '--'
      TZFIELD%CCOMMENT   = tpfields(1)%ccomment
      TZFIELD%NGRID      = tpfields(1)%ngrid
      TZFIELD%NTYPE      = tpfields(1)%ntype
      TZFIELD%NDIMS      = 2
      TZFIELD%LTIMEDEP   = .FALSE.
      CALL IO_Field_write( tzfile, tzfield, zdata2d )

      deallocate( zdata2d )

    else if ( Any( tpfields(1)%ndimlist(3) == [ NMNHDIM_BUDGET_CART_LEVEL, NMNHDIM_BUDGET_CART_LEVEL_W ] ) &
              .and. tpfields(1)%ndimlist(6) == NMNHDIM_BUDGET_NGROUPS ) then
      tzfield%ndimlist(1)  = tpfields(1)%ndimlist(3)
      tzfield%ndimlist(2:) = NMNHDIM_UNUSED

      allocate( zdata1d( size(pvar,3) ) )

      ! Loop on the processes (1 written variable per process)
      do ji = 1, Size( pvar, 6 )
        zdata1d(:) = pvar(1, 1, :, 1, 1, ji)

        tzfield%cmnhname   = tpfields(ji)%cmnhname
        tzfield%cstdname   = tpfields(ji)%cstdname
        tzfield%clongname  = tpfields(ji)%clongname
        tzfield%cunits     = tpfields(ji)%cunits
        tzfield%cdir       = '--'
        tzfield%ccomment   = tpfields(ji)%ccomment
        tzfield%ngrid      = tpfields(ji)%ngrid
        tzfield%ntype      = tpfields(ji)%ntype
        tzfield%ndims      = 1
        tzfield%ltimedep   = .FALSE.

        if ( gsplit ) then
          !Add budget time dimension
          tzfield%ndims = 2
          tzfield%ndimlist(2) = NMNHDIM_BUDGET_TIME

          !Create the metadata of the field (has to be done only once)
          if ( nbutshift == 1 ) call IO_Field_create( tzfile, tzfield )

          call IO_Field_write( tzfile, tzfield, Reshape( zdata1d, [ Size(zdata1d,1), 1 ] ), &
                               koffset= [ 0, ( nbutshift - 1 ) * nbusubwrite ] )
        else
          call IO_Field_write( tzfile, tzfield, zdata1d )
        end if
      end do

      deallocate( zdata1d )

    else if (  (      tpfields(1)%ndimlist(4) == NMNHDIM_BUDGET_LES_TIME &
               .or. tpfields(1)%ndimlist(4) == NMNHDIM_BUDGET_LES_AVG_TIME ) &
         .and. tpfields(1)%ndimlist(5) == NMNHDIM_BUDGET_LES_SV       ) then
      if ( gsplit ) then
          call Print_msg( NVERB_ERROR, 'IO', 'Write_diachro_nc4', Trim( tzfile%cname ) // &
                          ': group ' // Trim( hgroup ) //' gsplit=T not implemented' )
      end if

      if ( Size( tpfields ) /= 1 ) call Print_msg( NVERB_FATAL, 'IO', 'Write_diachro_nc4', &
                                                   'wrong size of tpfields (variable '//trim(tpfields(1)%cmnhname)//')' )

      tzfield%ndimlist(1)  = tpfields(1)%ndimlist(4)
      tzfield%ndimlist(2)  = tpfields(1)%ndimlist(5)
      tzfield%ndimlist(3:) = NMNHDIM_UNUSED

      allocate( zdata2d( size(pvar,4), size(pvar,5) ) )

      zdata2d(:,:) = pvar(1, 1, 1, :, :, 1)

      TZFIELD%CMNHNAME   = tpfields(1)%cmnhname
      TZFIELD%CSTDNAME   = tpfields(1)%cstdname
      TZFIELD%CLONGNAME  = tpfields(1)%clongname
      TZFIELD%CUNITS     = tpfields(1)%cunits
      TZFIELD%CDIR       = '--'
      TZFIELD%CCOMMENT   = tpfields(1)%ccomment
      TZFIELD%NGRID      = tpfields(1)%ngrid
      TZFIELD%NTYPE      = tpfields(1)%ntype
      TZFIELD%NDIMS      = 2
      TZFIELD%LTIMEDEP   = .FALSE.
      CALL IO_Field_write( tzfile, tzfield, zdata2d )

      deallocate( zdata2d )
    else if (  tpfields(1)%ndimlist(4) == NMNHDIM_FLYER_TIME &
         .and. tpfields(1)%ndimlist(6) == NMNHDIM_FLYER_PROC ) then
      !Correspond to FLYER_DIACHRO
      if ( gsplit ) then
          call Print_msg( NVERB_ERROR, 'IO', 'Write_diachro_nc4', Trim( tzfile%cname ) // &
                          ': group ' // Trim( hgroup ) //' gsplit=T not implemented' )
      end if

      !Create local time dimension
      if ( isp == tzfile%nmaster_rank) then
        istatus = NF90_DEF_DIM( igrpid, 'time_flyer', Size( pvar, 4), idimid )
        if ( istatus /= NF90_NOERR ) &
          call IO_Err_handle_nc4( istatus, 'Write_diachro_nc4', 'NF90_DEF_DIM', Trim( tpfields(1)%cmnhname ) )
      end if

      tzfield%ndimlist(1)  = tpfields(1)%ndimlist(4)
      tzfield%ndimlist(2:) = NMNHDIM_UNUSED

      allocate( zdata1d( size(pvar,4) ) )

      ! Loop on the processes (1 written variable per process)
      do ji = 1, size(pvar,6)
        zdata1d(:) = pvar(1, 1, 1, :, 1, ji)

        TZFIELD%CMNHNAME   = tpfields(ji)%cmnhname
        TZFIELD%CSTDNAME   = tpfields(ji)%cstdname
        TZFIELD%CLONGNAME  = tpfields(ji)%clongname
        TZFIELD%CUNITS     = tpfields(ji)%cunits
        TZFIELD%CDIR       = '--'
        TZFIELD%CCOMMENT   = tpfields(ji)%ccomment
        TZFIELD%NGRID      = tpfields(ji)%ngrid
        TZFIELD%NTYPE      = tpfields(ji)%ntype
        TZFIELD%NDIMS      = 1
        TZFIELD%LTIMEDEP   = .FALSE.
        CALL IO_Field_write( tzfile, tzfield, zdata1d )
      end do

      deallocate( zdata1d )
    else if (  tpfields(1)%ndimlist(4) == NMNHDIM_STATION_TIME &
         .and. tpfields(1)%ndimlist(6) == NMNHDIM_STATION_PROC ) then
      !Correspond to STATION_DIACHRO_n
      if ( gsplit ) then
          call Print_msg( NVERB_ERROR, 'IO', 'Write_diachro_nc4', Trim( tzfile%cname ) // &
                          ': group ' // Trim( hgroup ) //' gsplit=T not implemented' )
      end if

      tzfield%ndimlist(1)  = tpfields(1)%ndimlist(4)
      tzfield%ndimlist(2:) = NMNHDIM_UNUSED

      allocate( zdata1d( size(pvar,4) ) )

      ! Loop on the processes (1 written variable per process)
      do ji = 1, size(pvar,6)
        zdata1d(:) = pvar(1, 1, 1, :, 1, ji)

        TZFIELD%CMNHNAME   = tpfields(ji)%cmnhname
        TZFIELD%CSTDNAME   = tpfields(ji)%cstdname
        TZFIELD%CLONGNAME  = tpfields(ji)%clongname
        TZFIELD%CUNITS     = tpfields(ji)%cunits
        TZFIELD%CDIR       = '--'
        TZFIELD%CCOMMENT   = tpfields(ji)%ccomment
        TZFIELD%NGRID      = tpfields(ji)%ngrid
        TZFIELD%NTYPE      = tpfields(ji)%ntype
        TZFIELD%NDIMS      = 1
        TZFIELD%LTIMEDEP   = .FALSE.
        CALL IO_Field_write( tzfile, tzfield, zdata1d )
      end do

      deallocate( zdata1d )
    else if (  tpfields(1)%ndimlist(4) == NMNHDIM_SERIES_TIME &
         .and. tpfields(1)%ndimlist(6) == NMNHDIM_SERIES_PROC ) then
      !Correspond to WRITE_SERIES_n
      if ( gsplit ) then
          call Print_msg( NVERB_ERROR, 'IO', 'Write_diachro_nc4', Trim( tzfile%cname ) // &
                          ': group ' // Trim( hgroup ) //' gsplit=T not implemented' )
      end if

      tzfield%ndimlist(1)  = tpfields(1)%ndimlist(4)
      tzfield%ndimlist(2:) = NMNHDIM_UNUSED

      allocate( zdata1d( size(pvar,4) ) )

      ! Loop on the processes (1 written variable per process)
      do ji = 1, size(pvar,6)
        zdata1d(:) = pvar(1, 1, 1, :, 1, ji)

        TZFIELD%CMNHNAME   = tpfields(ji)%cmnhname
        TZFIELD%CSTDNAME   = tpfields(ji)%cstdname
        TZFIELD%CLONGNAME  = tpfields(ji)%clongname
        TZFIELD%CUNITS     = tpfields(ji)%cunits
        TZFIELD%CDIR       = '--'
        TZFIELD%CCOMMENT   = tpfields(ji)%ccomment
        TZFIELD%NGRID      = tpfields(ji)%ngrid
        TZFIELD%NTYPE      = tpfields(ji)%ntype
        TZFIELD%NDIMS      = 1
        TZFIELD%LTIMEDEP   = .FALSE.
        CALL IO_Field_write( tzfile, tzfield, zdata1d )
      end do

      deallocate( zdata1d )
    else
      call Print_msg( NVERB_FATAL, 'IO', 'Write_diachro_nc4', &
                      'case not yet implemented (variable '//trim(tpfields(1)%cmnhname)//')' )

      if ( gsplit ) then
          call Print_msg( NVERB_ERROR, 'IO', 'Write_diachro_nc4', Trim( tzfile%cname ) // &
                          ': group ' // Trim( hgroup ) //' gsplit=T not implemented' )
      end if

    end if


  case (3)
    if (       ( tpfields(1)%ndimlist(1) == NMNHDIM_NI .or. tpfields(1)%ndimlist(1) == NMNHDIM_NI_U .or. &
                 tpfields(1)%ndimlist(1) == NMNHDIM_NI_V .or. tpfields(1)%ndimlist(1) == NMNHDIM_BUDGET_CART_NI &
                 .or. tpfields(1)%ndimlist(1) == NMNHDIM_BUDGET_CART_NI_U &
                 .or. tpfields(1)%ndimlist(1) == NMNHDIM_BUDGET_CART_NI_V )     &
         .and. ( tpfields(1)%ndimlist(2) == NMNHDIM_NJ .or. tpfields(1)%ndimlist(2) == NMNHDIM_NJ_U .or. &
                  tpfields(1)%ndimlist(2) == NMNHDIM_NJ_V .or. tpfields(1)%ndimlist(2) == NMNHDIM_BUDGET_CART_NJ .or. &
                  tpfields(1)%ndimlist(2) == NMNHDIM_BUDGET_CART_NJ_U &
                 .or. tpfields(1)%ndimlist(2) == NMNHDIM_BUDGET_CART_NJ_V ) &
       .and.  ( tpfields(1)%ndimlist(3) == NMNHDIM_LEVEL .or. tpfields(1)%ndimlist(3) == NMNHDIM_LEVEL_W &
           .or. tpfields(1)%ndimlist(3) == NMNHDIM_BUDGET_CART_LEVEL &
           .or. tpfields(1)%ndimlist(3) == NMNHDIM_BUDGET_CART_LEVEL_W ) ) then
      if ( Size( tpfields ) /= 1 ) call Print_msg( NVERB_FATAL, 'IO', 'Write_diachro_nc4', &
                                                   'wrong size of tpfields (variable '//trim(tpfields(1)%cmnhname)//')' )

      tzfield%ndimlist(1)  = tpfields(1)%ndimlist(1)
      tzfield%ndimlist(2)  = tpfields(1)%ndimlist(2)
      tzfield%ndimlist(3)  = tpfields(1)%ndimlist(3)
      tzfield%ndimlist(4:) = NMNHDIM_UNUSED

      allocate( zdata3d( size(pvar,1), size(pvar,2), size(pvar,3) ) )

      zdata3d(:,:,:) = pvar(:, :, :, 1, 1, 1)

      TZFIELD%CMNHNAME   = tpfields(1)%cmnhname
      TZFIELD%CSTDNAME   = tpfields(1)%cstdname
      TZFIELD%CLONGNAME  = tpfields(1)%clongname
      TZFIELD%CUNITS     = tpfields(1)%cunits
      TZFIELD%CCOMMENT   = tpfields(1)%ccomment
      TZFIELD%NGRID      = tpfields(1)%ngrid
      TZFIELD%NTYPE      = tpfields(1)%ntype
      TZFIELD%NDIMS      = 3
      TZFIELD%LTIMEDEP   = .FALSE.

      if ( gsplit ) then
        if ( htype /= 'CART' ) call Print_msg( NVERB_ERROR, 'IO', 'Write_diachro_nc4', Trim( tzfile%cname ) // &
                    ': group ' // Trim( hgroup ) //': gsplit=T not implemented for these dimensions and htype/=CART' )

        if ( htype == 'CART' .and. .not. oicp .and. .not. ojcp ) then
          !Data is distributed between all the processes
          tzfield%cdir = 'XY'

          !Add budget time dimension
          tzfield%ndims = 4
          tzfield%ndimlist(4) = NMNHDIM_BUDGET_TIME

          !Create the metadata of the field (has to be done only once)
          if ( nbutshift == 1 ) call IO_Field_create( tzfile, tzfield )

          call IO_Field_write_box( tzfile, tzfield, 'BUDGET',                                                     &
                                   Reshape( zdata3d, [ Size(zdata3d,1), Size(zdata3d,2), Size(zdata3d,3), 1 ] ) , &
                                   kil + jphext, kih + jphext, kjl + jphext, kjh + jphext,                        &
                                   koffset= [ 0, 0, 0, ( nbutshift - 1 ) * nbusubwrite ]                          )
        else
          call Print_msg( NVERB_ERROR, 'IO', 'Write_diachro_nc4', Trim( tzfile%cname ) // &
                          ': group ' // Trim( hgroup ) //' gsplit=T not implemented' )
        end if
      else
        if ( htype == 'CART' .and. .not. oicp .and. .not. ojcp ) then
          !Data is distributed between all the processes
          tzfield%cdir = 'XY'
          call IO_Field_write_box( tzfile, tzfield, 'BUDGET', zdata3d, &
                                  kil + jphext, kih + jphext, kjl + jphext, kjh + jphext )
        else
          !Data is already collected on the master process
          tzfield%cdir = '--'
          call IO_Field_write( tzfile, tzfield, zdata3d )
        end if
      end if

      deallocate( zdata3d )

    else if (       Any(tpfields(1)%ndimlist(1) == [ NMNHDIM_BUDGET_CART_NI, NMNHDIM_BUDGET_CART_NI_U, NMNHDIM_BUDGET_CART_NI_V ]) &
              .and. Any(tpfields(1)%ndimlist(2) == [ NMNHDIM_BUDGET_CART_NJ, NMNHDIM_BUDGET_CART_NJ_U, NMNHDIM_BUDGET_CART_NJ_V ]) &
              .and. tpfields(1)%ndimlist(6) == NMNHDIM_BUDGET_NGROUPS    ) then
      tzfield%ndimlist(1)  = tpfields(1)%ndimlist(1)
      tzfield%ndimlist(2)  = tpfields(1)%ndimlist(2)
      tzfield%ndimlist(3:) = NMNHDIM_UNUSED

      Allocate( zdata2d(Size( pvar, 1 ), Size( pvar, 2 )) )

      ! Loop on the processes
      do ji = 1, Size( pvar, 6 )
        zdata2d(:, :) = pvar(:, :, 1, 1, 1, ji)

        tzfield%cmnhname   = tpfields(ji)%cmnhname
        tzfield%cstdname   = tpfields(ji)%cstdname
        tzfield%clongname  = tpfields(ji)%clongname
        tzfield%cunits     = tpfields(ji)%cunits
        tzfield%cdir       = '--'
        tzfield%ccomment   = tpfields(ji)%ccomment
        tzfield%ngrid      = tpfields(ji)%ngrid
        tzfield%ntype      = tpfields(ji)%ntype
        tzfield%ndims      = 2
        tzfield%ltimedep   = .false.

        if ( gsplit ) then
          !Add budget time dimension
          tzfield%ndims = 3
          tzfield%ndimlist(3) = NMNHDIM_BUDGET_TIME

          !Create the metadata of the field (has to be done only once)
          if ( nbutshift == 1 ) call IO_Field_create( tzfile, tzfield )

          call IO_Field_write( tzfile, tzfield,                                             &
                               Reshape( zdata2d, [ Size(zdata2d,1), Size(zdata2d,2), 1 ] ), &
                               koffset= [ 0, 0, ( nbutshift - 1 ) * nbusubwrite ]           )
        else
          call IO_Field_write( tzfile, tzfield, zdata2d )
        end if
      end do

      Deallocate( zdata2d )

    else if ( Any ( tpfields(1)%ndimlist(1) == [ NMNHDIM_BUDGET_CART_NI, NMNHDIM_BUDGET_CART_NI_U, NMNHDIM_BUDGET_CART_NI_V ] ) &
              .and. tpfields(1)%ndimlist(3) == NMNHDIM_BUDGET_CART_LEVEL &
              .and. tpfields(1)%ndimlist(6) == NMNHDIM_BUDGET_NGROUPS    ) then
      tzfield%ndimlist(1)  = tpfields(1)%ndimlist(1)
      tzfield%ndimlist(2)  = tpfields(1)%ndimlist(3)
      tzfield%ndimlist(3:) = NMNHDIM_UNUSED

      Allocate( zdata2d(Size( pvar, 1 ), Size( pvar, 3 )) )

      ! Loop on the processes
      do ji = 1, Size( pvar, 6 )
        zdata2d(:, :) = pvar(:, 1, :, 1, 1, ji)

        tzfield%cmnhname   = tpfields(ji)%cmnhname
        tzfield%cstdname   = tpfields(ji)%cstdname
        tzfield%clongname  = tpfields(ji)%clongname
        tzfield%cunits     = tpfields(ji)%cunits
        tzfield%cdir       = '--'
        tzfield%ccomment   = tpfields(ji)%ccomment
        tzfield%ngrid      = tpfields(ji)%ngrid
        tzfield%ntype      = tpfields(ji)%ntype
        tzfield%ndims      = 2
        tzfield%ltimedep   = .false.
        if ( gsplit ) then
          !Add budget time dimension
          tzfield%ndims = 3
          tzfield%ndimlist(3) = NMNHDIM_BUDGET_TIME

          !Create the metadata of the field (has to be done only once)
          if ( nbutshift == 1 ) call IO_Field_create( tzfile, tzfield )

          call IO_Field_write( tzfile, tzfield,                                             &
                               Reshape( zdata2d, [ Size(zdata2d,1), Size(zdata2d,2), 1 ] ), &
                               koffset= [ 0, 0, ( nbutshift - 1 ) * nbusubwrite ]           )
        else
          call IO_Field_write( tzfile, tzfield, zdata2d )
        end if
      end do

      Deallocate( zdata2d )
    else if ( Any ( tpfields(1)%ndimlist(2) == [ NMNHDIM_BUDGET_CART_NJ, NMNHDIM_BUDGET_CART_NJ_U, NMNHDIM_BUDGET_CART_NJ_V ] ) &
              .and. tpfields(1)%ndimlist(3) == NMNHDIM_BUDGET_CART_LEVEL &
              .and. tpfields(1)%ndimlist(6) == NMNHDIM_BUDGET_NGROUPS    ) then
      tzfield%ndimlist(1)  = tpfields(1)%ndimlist(2)
      tzfield%ndimlist(2)  = tpfields(1)%ndimlist(3)
      tzfield%ndimlist(3:) = NMNHDIM_UNUSED

      Allocate( zdata2d(Size( pvar, 2 ), Size( pvar, 3 )) )

      ! Loop on the processes
      do ji = 1, Size( pvar, 6 )
        zdata2d(:, :) = pvar(1, :, :, 1, 1, ji)

        tzfield%cmnhname   = tpfields(ji)%cmnhname
        tzfield%cstdname   = tpfields(ji)%cstdname
        tzfield%clongname  = tpfields(ji)%clongname
        tzfield%cunits     = tpfields(ji)%cunits
        tzfield%cdir       = '--'
        tzfield%ccomment   = tpfields(ji)%ccomment
        tzfield%ngrid      = tpfields(ji)%ngrid
        tzfield%ntype      = tpfields(ji)%ntype
        tzfield%ndims      = 2
        tzfield%ltimedep   = .false.
        if ( gsplit ) then
          !Add budget time dimension
          tzfield%ndims = 3
          tzfield%ndimlist(3) = NMNHDIM_BUDGET_TIME

          !Create the metadata of the field (has to be done only once)
          if ( nbutshift == 1 ) call IO_Field_create( tzfile, tzfield )

          call IO_Field_write( tzfile, tzfield,                                             &
                               Reshape( zdata2d, [ Size(zdata2d,1), Size(zdata2d,2), 1 ] ), &
                               koffset= [ 0, 0, ( nbutshift - 1 ) * nbusubwrite ]           )
        else
          call IO_Field_write( tzfile, tzfield, zdata2d )
        end if
      end do

      Deallocate( zdata2d )
    else if (  ( tpfields(1)%ndimlist(3) == NMNHDIM_BUDGET_MASK_LEVEL &
                  .or. tpfields(1)%ndimlist(3) == NMNHDIM_BUDGET_MASK_LEVEL_W ) &
       .and.    tpfields(1)%ndimlist(4) == NMNHDIM_BUDGET_TIME &
       .and. tpfields(1)%ndimlist(5) == NMNHDIM_BUDGET_MASK_NBUMASK      ) then
      !Correspond to Store_one_budget_rho (MASK)
      if ( Size( tpfields ) /= 1 ) call Print_msg( NVERB_FATAL, 'IO', 'Write_diachro_nc4', &
                                                   'wrong size of tpfields (variable '//trim(tpfields(1)%cmnhname)//')' )

      tzfield%ndimlist(1)  = tpfields(1)%ndimlist(3)
      tzfield%ndimlist(2)  = tpfields(1)%ndimlist(4)
      tzfield%ndimlist(3)  = tpfields(1)%ndimlist(5)
      tzfield%ndimlist(4:) = NMNHDIM_UNUSED

      allocate( zdata3d( size(pvar,3), size(pvar,4), size(pvar,5) ) )

      zdata3d(:,:,:) = pvar(1, 1, :, :, :, 1)

      TZFIELD%CMNHNAME   = tpfields(1)%cmnhname
      TZFIELD%CSTDNAME   = tpfields(1)%cstdname
      TZFIELD%CLONGNAME  = tpfields(1)%clongname
      TZFIELD%CUNITS     = tpfields(1)%cunits
      TZFIELD%CDIR       = '--'
      TZFIELD%CCOMMENT   = tpfields(1)%ccomment
      TZFIELD%NGRID      = tpfields(1)%ngrid
      TZFIELD%NTYPE      = tpfields(1)%ntype
      TZFIELD%NDIMS      = 3
      TZFIELD%LTIMEDEP   = .FALSE.
      if ( gsplit ) then
        !Create the metadata of the field (has to be done only once)
        if ( nbutshift == 1 ) call IO_Field_create( tzfile, tzfield )
        call IO_Field_write( tzfile, tzfield, zdata3d(:,:,:), koffset= [ 0, ( nbutshift - 1 ) * nbusubwrite, 0 ] )
      else
        call IO_Field_write( tzfile, tzfield, zdata3d )
      end if

      deallocate( zdata3d )
    else if (       tpfields(1)%ndimlist(3) == NMNHDIM_BUDGET_LES_LEVEL     &
       .and. (      tpfields(1)%ndimlist(4) == NMNHDIM_BUDGET_LES_TIME &
               .or. tpfields(1)%ndimlist(4) == NMNHDIM_BUDGET_LES_AVG_TIME ) &
       .and. tpfields(1)%ndimlist(6) == NMNHDIM_BUDGET_LES_MASK      ) then
      if ( gsplit ) then
          call Print_msg( NVERB_ERROR, 'IO', 'Write_diachro_nc4', Trim( tzfile%cname ) // &
                          ': group ' // Trim( hgroup ) //' gsplit=T not implemented' )
      end if

      if ( nles_masks /= Size( pvar, 6 ) ) &
        call Print_msg( NVERB_FATAL, 'IO', 'Write_diachro_nc4',                             &
                        'last dimension size of pvar is not equal to nles_masks (variable ' &
                        // Trim( tpfields(1)%cmnhname ) // ')' )

      tzfield%ndimlist(1)  = tpfields(1)%ndimlist(3)
      tzfield%ndimlist(2)  = tpfields(1)%ndimlist(4)
      tzfield%ndimlist(3:) = NMNHDIM_UNUSED

      allocate( zdata2d( size(pvar,3), size(pvar,4) ) )

      ! Loop on the masks (1 written variable per mask)
      do ji = 1, size(pvar,6)
        zdata2d(:,:) = pvar(1, 1, :, :, 1, ji)

        TZFIELD%CMNHNAME   = tpfields(ji)%cmnhname
        TZFIELD%CSTDNAME   = tpfields(ji)%cstdname
        TZFIELD%CLONGNAME  = tpfields(ji)%clongname
        TZFIELD%CUNITS     = tpfields(ji)%cunits
        TZFIELD%CDIR       = '--'
        TZFIELD%CCOMMENT   = tpfields(ji)%ccomment
        TZFIELD%NGRID      = tpfields(ji)%ngrid
        TZFIELD%NTYPE      = tpfields(ji)%ntype
        TZFIELD%NDIMS      = 2
        TZFIELD%LTIMEDEP   = .FALSE.
        CALL IO_Field_write( tzfile, tzfield, zdata2d )
      end do

      deallocate( zdata2d )
    else if (       tpfields(1)%ndimlist(3) == NMNHDIM_BUDGET_LES_LEVEL     &
       .and. (      tpfields(1)%ndimlist(4) == NMNHDIM_BUDGET_LES_TIME &
               .or. tpfields(1)%ndimlist(4) == NMNHDIM_BUDGET_LES_AVG_TIME ) &
       .and. tpfields(1)%ndimlist(6) == NMNHDIM_BUDGET_TERM      ) then
      if ( gsplit ) then
          call Print_msg( NVERB_ERROR, 'IO', 'Write_diachro_nc4', Trim( tzfile%cname ) // &
                          ': group ' // Trim( hgroup ) //' gsplit=T not implemented' )
      end if

      tzfield%ndimlist(1)  = tpfields(1)%ndimlist(3)
      tzfield%ndimlist(2)  = tpfields(1)%ndimlist(4)
      tzfield%ndimlist(3:) = NMNHDIM_UNUSED

      allocate( zdata2d( size(pvar,3), size(pvar,4) ) )

      ! Loop on the masks (1 written variable per mask)
      do ji = 1, size(pvar,6)
        zdata2d(:,:) = pvar(1, 1, :, :, 1, ji)

        TZFIELD%CMNHNAME   = tpfields(ji)%cmnhname
        TZFIELD%CSTDNAME   = tpfields(ji)%cstdname
        TZFIELD%CLONGNAME  = tpfields(ji)%clongname
        TZFIELD%CUNITS     = tpfields(ji)%cunits
        TZFIELD%CDIR       = '--'
        TZFIELD%CCOMMENT   = tpfields(ji)%ccomment
        TZFIELD%NGRID      = tpfields(ji)%ngrid
        TZFIELD%NTYPE      = tpfields(ji)%ntype
        TZFIELD%NDIMS      = 2
        TZFIELD%LTIMEDEP   = .FALSE.
        CALL IO_Field_write( tzfile, tzfield, zdata2d )
      end do

      deallocate( zdata2d )
    else if (       tpfields(1)%ndimlist(3) == NMNHDIM_BUDGET_LES_LEVEL     &
       .and. (      tpfields(1)%ndimlist(4) == NMNHDIM_BUDGET_LES_TIME &
               .or. tpfields(1)%ndimlist(4) == NMNHDIM_BUDGET_LES_AVG_TIME ) &
       .and. tpfields(1)%ndimlist(5) == NMNHDIM_BUDGET_LES_SV      ) then
      if ( gsplit ) then
          call Print_msg( NVERB_ERROR, 'IO', 'Write_diachro_nc4', Trim( tzfile%cname ) // &
                          ': group ' // Trim( hgroup ) //' gsplit=T not implemented' )
      end if

      if ( Size( tpfields ) /= 1 ) call Print_msg( NVERB_FATAL, 'IO', 'Write_diachro_nc4', &
                                                   'wrong size of tpfields (variable '//trim(tpfields(1)%cmnhname)//')' )

      !Correspond to Les_diachro_sv_new
      tzfield%ndimlist(1)  = tpfields(1)%ndimlist(3)
      tzfield%ndimlist(2)  = tpfields(1)%ndimlist(4)
      tzfield%ndimlist(3)  = tpfields(1)%ndimlist(5)
      tzfield%ndimlist(4:) = NMNHDIM_UNUSED

      allocate( zdata3d( size(pvar,3), size(pvar,4), size(pvar,5) ) )

      zdata3d(:,:,:) = pvar(1, 1, :, :, :, 1)

      TZFIELD%CMNHNAME   = tpfields(1)%cmnhname
      TZFIELD%CSTDNAME   = tpfields(1)%cstdname
      TZFIELD%CLONGNAME  = tpfields(1)%clongname
      TZFIELD%CUNITS     = tpfields(1)%cunits
      TZFIELD%CDIR       = '--'
      TZFIELD%CCOMMENT   = tpfields(1)%ccomment
      TZFIELD%NGRID      = tpfields(1)%ngrid
      TZFIELD%NTYPE      = tpfields(1)%ntype
      TZFIELD%NDIMS      = 3
      TZFIELD%LTIMEDEP   = .FALSE.
      CALL IO_Field_write( tzfile, tzfield, zdata3d )

      deallocate( zdata3d )
    else if (       tpfields(1)%ndimlist(1) == NMNHDIM_SPECTRA_2PTS_NI                   &
              .and. tpfields(1)%ndimlist(3) == NMNHDIM_SPECTRA_LEVEL                &
              .and. (      tpfields(1)%ndimlist(4) == NMNHDIM_BUDGET_LES_TIME       &
                      .or. tpfields(1)%ndimlist(4) == NMNHDIM_BUDGET_LES_AVG_TIME ) ) then
      if ( gsplit ) then
          call Print_msg( NVERB_ERROR, 'IO', 'Write_diachro_nc4', Trim( tzfile%cname ) // &
                          ': group ' // Trim( hgroup ) //' gsplit=T not implemented' )
      end if

      if ( Size( tpfields ) /= 1 ) call Print_msg( NVERB_FATAL, 'IO', 'Write_diachro_nc4', &
                                                   'wrong size of tpfields (variable '//trim(tpfields(1)%cmnhname)//')' )

      tzfield%ndimlist(1)  = tpfields(1)%ndimlist(1)
      tzfield%ndimlist(2)  = tpfields(1)%ndimlist(3)
      tzfield%ndimlist(3)  = tpfields(1)%ndimlist(4)
      tzfield%ndimlist(4:) = NMNHDIM_UNUSED

      allocate( zdata3d( size(pvar,1), size(pvar,3), size(pvar,4) ) )

      zdata3d(:,:,:) = pvar(:, 1, :, :, 1, 1)

      TZFIELD%CMNHNAME   = tpfields(1)%cmnhname
      TZFIELD%CSTDNAME   = tpfields(1)%cstdname
      TZFIELD%CLONGNAME  = tpfields(1)%clongname
      TZFIELD%CUNITS     = tpfields(1)%cunits
      TZFIELD%CDIR       = '--'
      TZFIELD%CCOMMENT   = tpfields(1)%ccomment
      TZFIELD%NGRID      = tpfields(1)%ngrid
      TZFIELD%NTYPE      = tpfields(1)%ntype
      TZFIELD%NDIMS      = 3
      TZFIELD%LTIMEDEP   = .FALSE.
      CALL IO_Field_write( tzfile, tzfield, zdata3d )

      deallocate( zdata3d )
    else if (       tpfields(1)%ndimlist(2) == NMNHDIM_SPECTRA_2PTS_NJ                   &
              .and. tpfields(1)%ndimlist(3) == NMNHDIM_SPECTRA_LEVEL                &
              .and. (      tpfields(1)%ndimlist(4) == NMNHDIM_BUDGET_LES_TIME       &
                      .or. tpfields(1)%ndimlist(4) == NMNHDIM_BUDGET_LES_AVG_TIME ) ) then
      if ( gsplit ) then
          call Print_msg( NVERB_ERROR, 'IO', 'Write_diachro_nc4', Trim( tzfile%cname ) // &
                          ': group ' // Trim( hgroup ) //' gsplit=T not implemented' )
      end if

      if ( Size( tpfields ) /= 1 ) call Print_msg( NVERB_FATAL, 'IO', 'Write_diachro_nc4', &
                                                   'wrong size of tpfields (variable '//trim(tpfields(1)%cmnhname)//')' )

      tzfield%ndimlist(1)  = tpfields(1)%ndimlist(2)
      tzfield%ndimlist(2)  = tpfields(1)%ndimlist(3)
      tzfield%ndimlist(3)  = tpfields(1)%ndimlist(4)
      tzfield%ndimlist(4:) = NMNHDIM_UNUSED

      allocate( zdata3d( size(pvar,2), size(pvar,3), size(pvar,4) ) )

      zdata3d(:,:,:) = pvar(1, :, :, :, 1, 1)

      TZFIELD%CMNHNAME   = tpfields(1)%cmnhname
      TZFIELD%CSTDNAME   = tpfields(1)%cstdname
      TZFIELD%CLONGNAME  = tpfields(1)%clongname
      TZFIELD%CUNITS     = tpfields(1)%cunits
      TZFIELD%CDIR       = '--'
      TZFIELD%CCOMMENT   = tpfields(1)%ccomment
      TZFIELD%NGRID      = tpfields(1)%ngrid
      TZFIELD%NTYPE      = tpfields(1)%ntype
      TZFIELD%NDIMS      = 3
      TZFIELD%LTIMEDEP   = .FALSE.
      CALL IO_Field_write( tzfile, tzfield, zdata3d )

      deallocate( zdata3d )
    else if (  tpfields(1)%ndimlist(3) == NMNHDIM_LEVEL      &
         .and. tpfields(1)%ndimlist(4) == NMNHDIM_FLYER_TIME &
         .and. tpfields(1)%ndimlist(6) == NMNHDIM_FLYER_PROC ) then
      !Correspond to FLYER_DIACHRO
      if ( gsplit ) then
          call Print_msg( NVERB_ERROR, 'IO', 'Write_diachro_nc4', Trim( tzfile%cname ) // &
                          ': group ' // Trim( hgroup ) //' gsplit=T not implemented' )
      end if

      !Create local time dimension
      if ( isp == tzfile%nmaster_rank) then
        istatus = NF90_DEF_DIM( igrpid, 'time_flyer', Size( pvar, 4), idimid )
        if ( istatus /= NF90_NOERR ) &
          call IO_Err_handle_nc4( istatus, 'Write_diachro_nc4', 'NF90_DEF_DIM', Trim( tpfields(1)%cmnhname ) )
      end if

      tzfield%ndimlist(1)  = tpfields(1)%ndimlist(3)
      tzfield%ndimlist(2)  = tpfields(1)%ndimlist(4)
      tzfield%ndimlist(3:) = NMNHDIM_UNUSED

      allocate( zdata2d( size(pvar,3), size(pvar,4) ) )

      ! Loop on the processes (1 written variable per process)
      do ji = 1, size(pvar,6)
        zdata2d(:, :) = pvar(1, 1, :, :, 1, ji)

        TZFIELD%CMNHNAME   = tpfields(ji)%cmnhname
        TZFIELD%CSTDNAME   = tpfields(ji)%cstdname
        TZFIELD%CLONGNAME  = tpfields(ji)%clongname
        TZFIELD%CUNITS     = tpfields(ji)%cunits
        TZFIELD%CDIR       = '--'
        TZFIELD%CCOMMENT   = tpfields(ji)%ccomment
        TZFIELD%NGRID      = tpfields(ji)%ngrid
        TZFIELD%NTYPE      = tpfields(ji)%ntype
        TZFIELD%NDIMS      = 2
        TZFIELD%LTIMEDEP   = .FALSE.
        CALL IO_Field_write( tzfile, tzfield, zdata2d )
      end do

      deallocate( zdata2d )
    else if (  tpfields(1)%ndimlist(3) == NMNHDIM_LEVEL      &
         .and. tpfields(1)%ndimlist(4) == NMNHDIM_PROFILER_TIME &
         .and. tpfields(1)%ndimlist(6) == NMNHDIM_PROFILER_PROC ) then
      !Correspond to PROFILER_DIACHRO_n
      if ( gsplit ) then
          call Print_msg( NVERB_ERROR, 'IO', 'Write_diachro_nc4', Trim( tzfile%cname ) // &
                          ': group ' // Trim( hgroup ) //' gsplit=T not implemented' )
      end if

      tzfield%ndimlist(1)  = tpfields(1)%ndimlist(3)
      tzfield%ndimlist(2)  = tpfields(1)%ndimlist(4)
      tzfield%ndimlist(3:) = NMNHDIM_UNUSED

      allocate( zdata2d( size(pvar,3), size(pvar,4) ) )

      ! Loop on the processes (1 written variable per process)
      do ji = 1, size(pvar,6)
        zdata2d(:, :) = pvar(1, 1, :, :, 1, ji)

        TZFIELD%CMNHNAME   = tpfields(ji)%cmnhname
        TZFIELD%CSTDNAME   = tpfields(ji)%cstdname
        TZFIELD%CLONGNAME  = tpfields(ji)%clongname
        TZFIELD%CUNITS     = tpfields(ji)%cunits
        TZFIELD%CDIR       = '--'
        TZFIELD%CCOMMENT   = tpfields(ji)%ccomment
        TZFIELD%NGRID      = tpfields(ji)%ngrid
        TZFIELD%NTYPE      = tpfields(ji)%ntype
        TZFIELD%NDIMS      = 2
        TZFIELD%LTIMEDEP   = .FALSE.
        CALL IO_Field_write( tzfile, tzfield, zdata2d )
      end do

      deallocate( zdata2d )
    else if (  ( tpfields(1)%ndimlist(3) == NMNHDIM_SERIES_LEVEL .or. tpfields(1)%ndimlist(3) == NMNHDIM_SERIES_LEVEL_W )      &
         .and. tpfields(1)%ndimlist(4) == NMNHDIM_SERIES_TIME &
         .and. tpfields(1)%ndimlist(6) == NMNHDIM_SERIES_PROC ) then
      !Correspond to PROFILER_DIACHRO_n
      if ( gsplit ) then
          call Print_msg( NVERB_ERROR, 'IO', 'Write_diachro_nc4', Trim( tzfile%cname ) // &
                          ': group ' // Trim( hgroup ) //' gsplit=T not implemented' )
      end if

      tzfield%ndimlist(1)  = tpfields(1)%ndimlist(3)
      tzfield%ndimlist(2)  = tpfields(1)%ndimlist(4)
      tzfield%ndimlist(3:) = NMNHDIM_UNUSED

      allocate( zdata2d( size(pvar,3), size(pvar,4) ) )

      ! Loop on the processes (1 written variable per process)
      do ji = 1, size(pvar,6)
        zdata2d(:, :) = pvar(1, 1, :, :, 1, ji)

        TZFIELD%CMNHNAME   = tpfields(ji)%cmnhname
        TZFIELD%CSTDNAME   = tpfields(ji)%cstdname
        TZFIELD%CLONGNAME  = tpfields(ji)%clongname
        TZFIELD%CUNITS     = tpfields(ji)%cunits
        TZFIELD%CDIR       = '--'
        TZFIELD%CCOMMENT   = tpfields(ji)%ccomment
        TZFIELD%NGRID      = tpfields(ji)%ngrid
        TZFIELD%NTYPE      = tpfields(ji)%ntype
        TZFIELD%NDIMS      = 2
        TZFIELD%LTIMEDEP   = .FALSE.
        CALL IO_Field_write( tzfile, tzfield, zdata2d )
      end do

      deallocate( zdata2d )
    else if (  ( tpfields(1)%ndimlist(1) == NMNHDIM_NI .or. tpfields(1)%ndimlist(1) == NMNHDIM_NI_U )      &
         .and. tpfields(1)%ndimlist(4) == NMNHDIM_SERIES_TIME &
         .and. tpfields(1)%ndimlist(6) == NMNHDIM_SERIES_PROC ) then
      !Correspond to PROFILER_DIACHRO_n
      if ( gsplit ) then
          call Print_msg( NVERB_ERROR, 'IO', 'Write_diachro_nc4', Trim( tzfile%cname ) // &
                          ': group ' // Trim( hgroup ) //' gsplit=T not implemented' )
      end if

      tzfield%ndimlist(1)  = tpfields(1)%ndimlist(1)
      tzfield%ndimlist(2)  = tpfields(1)%ndimlist(4)
      tzfield%ndimlist(3:) = NMNHDIM_UNUSED

      allocate( zdata2d( size(pvar,1), size(pvar,4) ) )

      ! Loop on the processes (1 written variable per process)
      do ji = 1, size(pvar,6)
        zdata2d(:, :) = pvar(:, 1, 1, :, 1, ji)

        TZFIELD%CMNHNAME   = tpfields(ji)%cmnhname
        TZFIELD%CSTDNAME   = tpfields(ji)%cstdname
        TZFIELD%CLONGNAME  = tpfields(ji)%clongname
        TZFIELD%CUNITS     = tpfields(ji)%cunits
        TZFIELD%CDIR       = '--'
        TZFIELD%CCOMMENT   = tpfields(ji)%ccomment
        TZFIELD%NGRID      = tpfields(ji)%ngrid
        TZFIELD%NTYPE      = tpfields(ji)%ntype
        TZFIELD%NDIMS      = 2
        TZFIELD%LTIMEDEP   = .FALSE.
        CALL IO_Field_write( tzfile, tzfield, zdata2d )
      end do

      deallocate( zdata2d )
    else
      call Print_msg( NVERB_FATAL, 'IO', 'Write_diachro_nc4', &
                      'case not yet implemented (variable '//trim(tpfields(1)%cmnhname)//')' )
    end if

  case (4)
    if (       ( tpfields(1)%ndimlist(1) == NMNHDIM_BUDGET_CART_NI .or. tpfields(1)%ndimlist(1) == NMNHDIM_BUDGET_CART_NI_U &
                 .or. tpfields(1)%ndimlist(1) == NMNHDIM_BUDGET_CART_NI_V )     &
         .and. ( tpfields(1)%ndimlist(2) == NMNHDIM_BUDGET_CART_NJ .or. tpfields(1)%ndimlist(2) == NMNHDIM_BUDGET_CART_NJ_U &
                 .or. tpfields(1)%ndimlist(2) == NMNHDIM_BUDGET_CART_NJ_V ) &
       .and.  ( tpfields(1)%ndimlist(3) == NMNHDIM_BUDGET_CART_LEVEL &
                  .or. tpfields(1)%ndimlist(3) == NMNHDIM_BUDGET_CART_LEVEL_W ) &
       .and. tpfields(1)%ndimlist(6) == NMNHDIM_BUDGET_NGROUPS      ) then
      !Correspond to Store_one_budget (CART)
      tzfield%ndimlist(1)  = tpfields(1)%ndimlist(1)
      tzfield%ndimlist(2)  = tpfields(1)%ndimlist(2)
      tzfield%ndimlist(3)  = tpfields(1)%ndimlist(3)
      tzfield%ndimlist(4:) = NMNHDIM_UNUSED

      allocate( zdata3d( size(pvar,1), size(pvar,2), size(pvar,3) ) )

      ! Loop on the processes
      do ji = 1, size(pvar,6)
        zdata3d(:,:,:) = pvar(:, :, :, 1, 1, ji)

        TZFIELD%CMNHNAME   = tpfields(ji)%cmnhname
        TZFIELD%CSTDNAME   = tpfields(ji)%cstdname
        TZFIELD%CLONGNAME  = tpfields(ji)%clongname
        TZFIELD%CUNITS     = tpfields(ji)%cunits
        TZFIELD%CCOMMENT   = tpfields(ji)%ccomment
        TZFIELD%NGRID      = tpfields(ji)%ngrid
        TZFIELD%NTYPE      = tpfields(ji)%ntype
        TZFIELD%NDIMS      = 3
        TZFIELD%LTIMEDEP   = .FALSE.

        if ( gsplit ) then
          if ( htype /= 'CART' ) call Print_msg( NVERB_ERROR, 'IO', 'Write_diachro_nc4', Trim( tzfile%cname ) // &
                      ': group ' // Trim( hgroup ) //' gsplit=T not implemented for these dimensions and htype/=CART' )

          if ( htype == 'CART' .and. .not. oicp .and. .not. ojcp ) then
            !Data is distributed between all the processes
            tzfield%cdir = 'XY'

            !Add budget time dimension
            tzfield%ndims = 4
            tzfield%ndimlist(4) = NMNHDIM_BUDGET_TIME

            !Create the metadata of the field (has to be done only once)
            if ( nbutshift == 1 ) call IO_Field_create( tzfile, tzfield )

            call IO_Field_write_box( tzfile, tzfield, 'BUDGET',                                                     &
                                     Reshape( zdata3d, [ Size(zdata3d,1), Size(zdata3d,2), Size(zdata3d,3), 1 ] ) , &
                                     kil + jphext, kih + jphext, kjl + jphext, kjh + jphext,                        &
                                     koffset= [ 0, 0, 0, ( nbutshift - 1 ) * nbusubwrite ]                          )
          else
            call Print_msg( NVERB_ERROR, 'IO', 'Write_diachro_nc4', Trim( tzfile%cname ) // &
                            ': group ' // Trim( hgroup ) //' gsplit=T not implemented' )
          end if
        else
          if ( htype == 'CART' .and. .not. oicp .and. .not. ojcp ) then
            !Data is distributed between all the processes
            tzfield%cdir = 'XY'
            call IO_Field_write_box( tzfile, tzfield, 'BUDGET', zdata3d, &
                                    kil + jphext, kih + jphext, kjl + jphext, kjh + jphext )
          else
            !Data is already collected on the master process
            tzfield%cdir = '--'
            call IO_Field_write( tzfile, tzfield, zdata3d )
          end if
        end if
      end do

      deallocate( zdata3d )

    elseif (  ( tpfields(1)%ndimlist(3) == NMNHDIM_BUDGET_MASK_LEVEL &
                  .or. tpfields(1)%ndimlist(3) == NMNHDIM_BUDGET_MASK_LEVEL_W ) &
       .and.    tpfields(1)%ndimlist(4) == NMNHDIM_BUDGET_TIME &
       .and. tpfields(1)%ndimlist(5) == NMNHDIM_BUDGET_MASK_NBUMASK &
       .and. tpfields(1)%ndimlist(6) == NMNHDIM_BUDGET_NGROUPS      ) then
      !Correspond to Store_one_budget (MASK)
      tzfield%ndimlist(1)  = tpfields(1)%ndimlist(3)
      tzfield%ndimlist(2)  = tpfields(1)%ndimlist(4)
      tzfield%ndimlist(3)  = tpfields(1)%ndimlist(5)
      tzfield%ndimlist(4:) = NMNHDIM_UNUSED

      allocate( zdata3d( size(pvar,3), size(pvar,4), size(pvar,5) ) )

      ! Loop on the processes
      do ji = 1, size(pvar,6)
        zdata3d(:,:,:) = pvar(1, 1, :, :, :, ji)

        TZFIELD%CMNHNAME   = tpfields(ji)%cmnhname
        TZFIELD%CSTDNAME   = tpfields(ji)%cstdname
        TZFIELD%CLONGNAME  = tpfields(ji)%clongname
        TZFIELD%CUNITS     = tpfields(ji)%cunits
        TZFIELD%CDIR       = '--'
        TZFIELD%CCOMMENT   = tpfields(ji)%ccomment
        TZFIELD%NGRID      = tpfields(ji)%ngrid
        TZFIELD%NTYPE      = tpfields(ji)%ntype
        TZFIELD%NDIMS      = 3
        TZFIELD%LTIMEDEP   = .FALSE.
        if ( gsplit ) then
        !Create the metadata of the field (has to be done only once)
          if ( nbutshift == 1 ) call IO_Field_create( tzfile, tzfield )
!           call IO_Field_partial_write( tzfile, tzfield, zdata3d(:,:,:), koffset= [ 0, ( nbutshift - 1 ) * nbusubwrite, 0 ] )
          call IO_Field_write( tzfile, tzfield, zdata3d(:,:,:), koffset= [ 0, ( nbutshift - 1 ) * nbusubwrite, 0 ] )
        else
          call IO_Field_write( tzfile, tzfield, zdata3d )
        end if
      end do

      deallocate( zdata3d )
    else if (       tpfields(1)%ndimlist(3) == NMNHDIM_BUDGET_LES_LEVEL     &
       .and. (      tpfields(1)%ndimlist(4) == NMNHDIM_BUDGET_LES_TIME &
               .or. tpfields(1)%ndimlist(4) == NMNHDIM_BUDGET_LES_AVG_TIME ) &
       .and. tpfields(1)%ndimlist(5) == NMNHDIM_BUDGET_LES_SV      &
       .and. tpfields(1)%ndimlist(6) == NMNHDIM_BUDGET_LES_MASK      ) then
      if ( gsplit ) then
          call Print_msg( NVERB_ERROR, 'IO', 'Write_diachro_nc4', Trim( tzfile%cname ) // &
                          ': group ' // Trim( hgroup ) //' gsplit=T not implemented' )
      end if

      if ( nles_masks /= Size( pvar, 6 ) ) &
        call Print_msg( NVERB_FATAL, 'IO', 'Write_diachro_nc4',                             &
                        'last dimension size of pvar is not equal to nles_masks (variable ' &
                        // Trim( tpfields(1)%cmnhname ) // ')' )

      tzfield%ndimlist(1)  = tpfields(1)%ndimlist(3)
      tzfield%ndimlist(2)  = tpfields(1)%ndimlist(4)
      tzfield%ndimlist(3)  = tpfields(1)%ndimlist(5)
      tzfield%ndimlist(4:) = NMNHDIM_UNUSED

      allocate( zdata3d( size(pvar,3), size(pvar,4), size(pvar,5) ) )

      ! Loop on the masks (1 written variable per mask)
      do ji = 1, size(pvar,6)
        zdata3d(:,:,:) = pvar(1, 1, :, :, :, ji)

        TZFIELD%CMNHNAME   = tpfields(ji)%cmnhname
        TZFIELD%CSTDNAME   = tpfields(ji)%cstdname
        TZFIELD%CLONGNAME  = tpfields(ji)%clongname
        TZFIELD%CUNITS     = tpfields(ji)%cunits
        TZFIELD%CDIR       = '--'
        TZFIELD%CCOMMENT   = tpfields(ji)%ccomment
        TZFIELD%NGRID      = tpfields(ji)%ngrid
        TZFIELD%NTYPE      = tpfields(ji)%ntype
        TZFIELD%NDIMS      = 3
        TZFIELD%LTIMEDEP   = .FALSE.
        CALL IO_Field_write( tzfile, tzfield, zdata3d )
      end do

      deallocate( zdata3d )
    else if (       tpfields(1)%ndimlist(3) == NMNHDIM_BUDGET_LES_LEVEL     &
       .and. (      tpfields(1)%ndimlist(4) == NMNHDIM_BUDGET_LES_TIME &
               .or. tpfields(1)%ndimlist(4) == NMNHDIM_BUDGET_LES_AVG_TIME ) &
       .and. tpfields(1)%ndimlist(5) == NMNHDIM_BUDGET_LES_SV      &
       .and. tpfields(1)%ndimlist(6) == NMNHDIM_BUDGET_TERM      ) then
      if ( gsplit ) then
          call Print_msg( NVERB_ERROR, 'IO', 'Write_diachro_nc4', Trim( tzfile%cname ) // &
                          ': group ' // Trim( hgroup ) //' gsplit=T not implemented' )
      end if

      tzfield%ndimlist(1)  = tpfields(1)%ndimlist(3)
      tzfield%ndimlist(2)  = tpfields(1)%ndimlist(4)
      tzfield%ndimlist(3)  = tpfields(1)%ndimlist(5)
      tzfield%ndimlist(4:) = NMNHDIM_UNUSED

      allocate( zdata3d( size(pvar,3), size(pvar,4), size(pvar,5) ) )

      ! Loop on the masks (1 written variable per mask)
      do ji = 1, size(pvar,6)
        zdata3d(:,:,:) = pvar(1, 1, :, :, :, ji)

        TZFIELD%CMNHNAME   = tpfields(ji)%cmnhname
        TZFIELD%CSTDNAME   = tpfields(ji)%cstdname
        TZFIELD%CLONGNAME  = tpfields(ji)%clongname
        TZFIELD%CUNITS     = tpfields(ji)%cunits
        TZFIELD%CDIR       = '--'
        TZFIELD%CCOMMENT   = tpfields(ji)%ccomment
        TZFIELD%NGRID      = tpfields(ji)%ngrid
        TZFIELD%NTYPE      = tpfields(ji)%ntype
        TZFIELD%NDIMS      = 3
        TZFIELD%LTIMEDEP   = .FALSE.
        CALL IO_Field_write( tzfile, tzfield, zdata3d )
      end do

      deallocate( zdata3d )
    else if (       tpfields(1)%ndimlist(1) == NMNHDIM_SPECTRA_SPEC_NI     &
       .and.        tpfields(1)%ndimlist(3) == NMNHDIM_SPECTRA_LEVEL &
       .and. (      tpfields(1)%ndimlist(4) == NMNHDIM_BUDGET_LES_TIME &
               .or. tpfields(1)%ndimlist(4) == NMNHDIM_BUDGET_LES_AVG_TIME ) &
       .and. tpfields(1)%ndimlist(5) == NMNHDIM_COMPLEX      ) then
      !Correspond to LES_DIACHRO_SPEC
      if ( gsplit ) then
          call Print_msg( NVERB_ERROR, 'IO', 'Write_diachro_nc4', Trim( tzfile%cname ) // &
                          ': group ' // Trim( hgroup ) //' gsplit=T not implemented' )
      end if

      if ( Size( tpfields ) /= 1 ) call Print_msg( NVERB_FATAL, 'IO', 'Write_diachro_nc4', &
                                                   'wrong size of tpfields (variable '//trim(tpfields(1)%cmnhname)//')' )

      tzfield%ndimlist(1)  = tpfields(1)%ndimlist(1)
      tzfield%ndimlist(2)  = tpfields(1)%ndimlist(3)
      tzfield%ndimlist(3)  = tpfields(1)%ndimlist(4)
      tzfield%ndimlist(4)  = tpfields(1)%ndimlist(5)
      tzfield%ndimlist(5:) = NMNHDIM_UNUSED

      allocate( zdata4d( size(pvar,1), size(pvar,3), size(pvar,4), size(pvar,5) ) )

      zdata4d(:,:,:,:) = pvar(:, 1, :, :, :, 1)

      TZFIELD%CMNHNAME   = tpfields(1)%cmnhname
      TZFIELD%CSTDNAME   = tpfields(1)%cstdname
      TZFIELD%CLONGNAME  = tpfields(1)%clongname
      TZFIELD%CUNITS     = tpfields(1)%cunits
      TZFIELD%CDIR       = '--'
      TZFIELD%CCOMMENT   = tpfields(1)%ccomment
      TZFIELD%NGRID      = tpfields(1)%ngrid
      TZFIELD%NTYPE      = tpfields(1)%ntype
      TZFIELD%NDIMS      = 4
      TZFIELD%LTIMEDEP   = .FALSE.
      CALL IO_Field_write( tzfile, tzfield, zdata4d )

      deallocate( zdata4d )
    else if (       tpfields(1)%ndimlist(2) == NMNHDIM_SPECTRA_SPEC_NJ     &
       .and.        tpfields(1)%ndimlist(3) == NMNHDIM_SPECTRA_LEVEL &
       .and. (      tpfields(1)%ndimlist(4) == NMNHDIM_BUDGET_LES_TIME &
               .or. tpfields(1)%ndimlist(4) == NMNHDIM_BUDGET_LES_AVG_TIME ) &
       .and. tpfields(1)%ndimlist(5) == NMNHDIM_COMPLEX      ) then
      !Correspond to LES_DIACHRO_SPEC
      if ( gsplit ) then
          call Print_msg( NVERB_ERROR, 'IO', 'Write_diachro_nc4', Trim( tzfile%cname ) // &
                          ': group ' // Trim( hgroup ) //' gsplit=T not implemented' )
      end if

      if ( Size( tpfields ) /= 1 ) call Print_msg( NVERB_FATAL, 'IO', 'Write_diachro_nc4', &
                                                   'wrong size of tpfields (variable '//trim(tpfields(1)%cmnhname)//')' )

      tzfield%ndimlist(1)  = tpfields(1)%ndimlist(2)
      tzfield%ndimlist(2)  = tpfields(1)%ndimlist(3)
      tzfield%ndimlist(3)  = tpfields(1)%ndimlist(4)
      tzfield%ndimlist(4)  = tpfields(1)%ndimlist(5)
      tzfield%ndimlist(5:) = NMNHDIM_UNUSED

      allocate( zdata4d( size(pvar,2), size(pvar,3), size(pvar,4), size(pvar,5) ) )

      zdata4d(:,:,:,:) = pvar(1, :, :, :, :, 1)

      TZFIELD%CMNHNAME   = tpfields(1)%cmnhname
      TZFIELD%CSTDNAME   = tpfields(1)%cstdname
      TZFIELD%CLONGNAME  = tpfields(1)%clongname
      TZFIELD%CUNITS     = tpfields(1)%cunits
      TZFIELD%CDIR       = '--'
      TZFIELD%CCOMMENT   = tpfields(1)%ccomment
      TZFIELD%NGRID      = tpfields(1)%ngrid
      TZFIELD%NTYPE      = tpfields(1)%ntype
      TZFIELD%NDIMS      = 4
      TZFIELD%LTIMEDEP   = .FALSE.
      CALL IO_Field_write( tzfile, tzfield, zdata4d )

      deallocate( zdata4d )
    else
      call Print_msg( NVERB_FATAL, 'IO', 'Write_diachro_nc4', &
                      'case not yet implemented (variable '//trim(tpfields(1)%cmnhname)//')' )
    end if

!   case (5)

!   case (6)

  case default
    if ( gsplit ) then
        call Print_msg( NVERB_ERROR, 'IO', 'Write_diachro_nc4', Trim( tzfile%cname ) // &
                        ': group ' // Trim( hgroup ) //' gsplit=T not implemented' )
    end if

    if ( All( tpfields(1)%ndimlist(:) /= NMNHDIM_UNKNOWN ) ) then
      tzfield%ndimlist(1)  = tpfields(1)%ndimlist(1)
      tzfield%ndimlist(2)  = tpfields(1)%ndimlist(2)
      tzfield%ndimlist(3)  = tpfields(1)%ndimlist(3)
      tzfield%ndimlist(4)  = tpfields(1)%ndimlist(4)
      tzfield%ndimlist(5)  = tpfields(1)%ndimlist(5)
      tzfield%ndimlist(6:) = NMNHDIM_UNUSED
    end if

    do ji = 1, Size( pvar, 6 )
      TZFIELD%CMNHNAME   = tpfields(ji)%cmnhname
      TZFIELD%CSTDNAME   = tpfields(ji)%cstdname
      TZFIELD%CLONGNAME  = tpfields(ji)%clongname
      TZFIELD%CUNITS     = tpfields(ji)%cunits
      TZFIELD%CDIR       = '--'
      TZFIELD%CCOMMENT   = tpfields(ji)%ccomment
      TZFIELD%NGRID      = tpfields(ji)%ngrid
      TZFIELD%NTYPE      = tpfields(ji)%ntype
      TZFIELD%NDIMS      = 5
      TZFIELD%LTIMEDEP   = .FALSE.
      CALL IO_Field_write( tzfile, tzfield, pvar(:, :, :, :, :, ji ) )
    end do
end select

!Reset ndimlist (to prevent problems later)
tzfield%ndimlist(:) = NMNHDIM_UNKNOWN

TZFIELD%CMNHNAME   = 'dates'
TZFIELD%CSTDNAME   = ''
TZFIELD%CLONGNAME  = 'dates'
TZFIELD%CUNITS     = 'seconds since YYYY-MM-DD HH:MM:SS.S'
TZFIELD%CDIR       = '--'
TZFIELD%CCOMMENT   = 'Dates at the middle of the budget timesteps'
TZFIELD%NGRID      = 0
TZFIELD%NTYPE      = TYPEDATE
TZFIELD%NDIMS      = 1
TZFIELD%LTIMEDEP   = .FALSE.

if ( tpfields(1)%ndimlist(4) /= NMNHDIM_UNKNOWN .and. tpfields(1)%ndimlist(4) /= NMNHDIM_UNUSED) then
  tzfield%ndimlist(1) = tpfields(1)%ndimlist(4)
  tzfield%ndimlist(2:) = NMNHDIM_UNUSED
end if

CALL IO_Field_write( tzfile, tzfield, tpdates(:) )

!Reset ndimlist
tzfield%ndimlist(:) = NMNHDIM_UNKNOWN

!

!Restore id of the file root group ('/' group)
tzfile%nncid = isavencid

call Menu_diachro( tzfile, hgroup )

end  subroutine Write_diachro_nc4
#endif

end module mode_write_diachro
