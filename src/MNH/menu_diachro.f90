!MNH_LIC Copyright 1996-2021 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
#ifdef MNH_IOLFI
module mode_menu_diachro

implicit none

private

public :: Menu_diachro

contains
!     ##################################################
      SUBROUTINE MENU_DIACHRO(TPDIAFILE,HGROUP)
!     ##################################################
!
!!****  *MENU_DIACHRO* - Creation, ecriture (eventuellement lecture) de
!          l'enregistrement MENU_BUDGET  dans un fichier diachronique
!!
!!    PURPOSE
!!    -------
!
!!**  METHOD
!!    ------
!!
!      A chaque ecriture d'un enregistrement dans un fichier diachronique,
!      cette routine est appelee pour memoriser le nom du groupe correspon-
!      -dant (passe en argument dans HGROUP)
!     Au terme des ecritures, elle est appelee avec HGROUP='END' qui
!     a pour effet d'ecrire dans le fichier diachronique le tableau contenant
!     le nom des groupes avec l'identificateur de record : MENU_BUDGET
!     Quand HGROUP='READ', l'enregistrement MENU_BUDGET est lu et la
!     liste des groupes enregistres est imprimee
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
!!      Updated   JD 24/06/99 : add GPACK to disable pack option
!  P. Wautelet 05/2016-04/2018: new data structures and calls for I/O
!  P. Wautelet 15/12/2020: more intelligent ygroup allocation (waste less memory + no more risk of out of range accesses)
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_CONF
use modd_field,          only: tfielddata, TYPEINT
USE MODD_IO,             only: TFILEDATA
use modd_parameters,     only: NMNHNAMELGTMAX
!
USE MODE_IO_FIELD_READ,  only: IO_Field_read
USE MODE_IO_FIELD_WRITE, only: IO_Field_write
!
IMPLICIT NONE
!
!*       0.1   Dummy arguments
!              ---------------

TYPE(TFILEDATA),  INTENT(IN) :: TPDIAFILE    ! file to write
CHARACTER(LEN=*), INTENT(IN) :: HGROUP
!
!*       0.1   Local variables
!              ---------------

integer, parameter :: NALLOCSTEP = 50

character(len=NMNHNAMELGTMAX), dimension(:), allocatable, save :: ygroup
character(len=NMNHNAMELGTMAX), dimension(:), allocatable       :: ygroup_tmp
INTEGER   ::   ILENG, J, JJ, IALREADY
INTEGER   ::   IRESPDIA
INTEGER,SAVE   ::   IGROUP=0
INTEGER,DIMENSION(:),ALLOCATABLE :: ITABCHAR
LOGICAL   ::   GPACK
TYPE(TFIELDDATA)  :: TZFIELD
type(tfiledata) :: tzfile
!------------------------------------------------------------------------------
!
if ( tpdiafile%cformat == 'NETCDF4' ) return

GPACK=LPACK
LPACK=.FALSE.
!
IF(HGROUP == 'END')THEN

  IF(IGROUP == 0)THEN
!   print *,' No record for the diachronic file' mettre les prints dans le fichier LISTING
    LPACK=GPACK
    RETURN
  ENDIF

  !Write only in LFI files
  tzfile = tpdiafile
  tzfile%cformat = 'LFI'

  ILENG=NMNHNAMELGTMAX*IGROUP

  TZFIELD%CMNHNAME   = 'MENU_BUDGET.DIM'
  TZFIELD%CSTDNAME   = ''
  TZFIELD%CLONGNAME  = 'MENU_BUDGET.DIM'
  TZFIELD%CUNITS     = ''
  TZFIELD%CDIR       = '--'
  TZFIELD%CCOMMENT   = ''
  TZFIELD%NGRID      = 0
  TZFIELD%NTYPE      = TYPEINT
  TZFIELD%NDIMS      = 0
  TZFIELD%LTIMEDEP   = .FALSE.
  CALL IO_Field_write(tzfile,TZFIELD,ILENG)

  ALLOCATE(ITABCHAR(ILENG))
  DO JJ=1,IGROUP
    DO J = 1,NMNHNAMELGTMAX
      ITABCHAR(NMNHNAMELGTMAX*(JJ-1)+J) = ICHAR(YGROUP(JJ)(J:J))
    ENDDO
  ENDDO

  TZFIELD%CMNHNAME   = 'MENU_BUDGET'
  TZFIELD%CSTDNAME   = ''
  TZFIELD%CLONGNAME  = 'MENU_BUDGET'
  TZFIELD%CUNITS     = ''
  TZFIELD%CDIR       = '--'
  TZFIELD%CCOMMENT   = ''
  TZFIELD%NGRID      = 0
  TZFIELD%NTYPE      = TYPEINT
  TZFIELD%NDIMS      = 1
  TZFIELD%LTIMEDEP   = .FALSE.
  CALL IO_Field_write(tzfile,TZFIELD,ITABCHAR)

  DEALLOCATE(ITABCHAR)

ELSE IF(HGROUP == 'READ')THEN

  !Read only in LFI files
  tzfile = tpdiafile
  tzfile%cformat = 'LFI'

  TZFIELD%CMNHNAME   = 'MENU_BUDGET.DIM'
  TZFIELD%CSTDNAME   = ''
  TZFIELD%CLONGNAME  = 'MENU_BUDGET.DIM'
  TZFIELD%CUNITS     = ''
  TZFIELD%CDIR       = '--'
  TZFIELD%CCOMMENT   = ''
  TZFIELD%NGRID      = 0
  TZFIELD%NTYPE      = TYPEINT
  TZFIELD%NDIMS      = 0
  TZFIELD%LTIMEDEP   = .FALSE.
  CALL IO_Field_read(tzfile,TZFIELD,ILENG,IRESPDIA)
  IF(IRESPDIA == -47)THEN
!   print *,' No record MENU_BUDGET '
    LPACK=GPACK
    RETURN
  ENDIF

  ALLOCATE(ITABCHAR(ILENG))
  TZFIELD%CMNHNAME   = 'MENU_BUDGET'
  TZFIELD%CSTDNAME   = ''
  TZFIELD%CLONGNAME  = 'MENU_BUDGET'
  TZFIELD%CUNITS     = ''
  TZFIELD%CDIR       = '--'
  TZFIELD%CCOMMENT   = ''
  TZFIELD%NGRID      = 0
  TZFIELD%NTYPE      = TYPEINT
  TZFIELD%NDIMS      = 1
  TZFIELD%LTIMEDEP   = .FALSE.
  CALL IO_Field_read(tzfile,TZFIELD,ITABCHAR)
  IGROUP=ILENG/NMNHNAMELGTMAX
  DO JJ=1,IGROUP
    DO J = 1,NMNHNAMELGTMAX
      YGROUP(JJ)(J:J)=CHAR(ITABCHAR(NMNHNAMELGTMAX*(JJ-1)+J))
    ENDDO
  ENDDO
  DO JJ=1,IGROUP
!   print *,' ******** YGROUP :  ',YGROUP(JJ)
  ENDDO
  DEALLOCATE(ITABCHAR)

ELSE

  IALREADY=0
  IF(IGROUP > 1)THEN
!!!!!!!!!!!!!!!!  FUJI  compiler directive !!!!!!!!!!
!ocl scalar
!!!!!!!!!!!!!!!!  FUJI  compiler directive !!!!!!!!!!
    DO JJ=1,IGROUP
      IF(ADJUSTL(HGROUP) == YGROUP(JJ))IALREADY=1
    ENDDO
  ENDIF
  IF(IALREADY == 0)THEN
    IGROUP=IGROUP+1

    !Reallocate ygroup if too small
    if ( Modulo( igroup, NALLOCSTEP ) == 1 ) then
      Allocate( ygroup_tmp( NALLOCSTEP * ( igroup / NALLOCSTEP + 1 ) ) )
      ygroup_tmp( 1 : igroup - 1 ) = ygroup( 1 : igroup - 1 )
      call Move_alloc( from = ygroup_tmp, to = ygroup )
    end if

    YGROUP(IGROUP)=ADJUSTL(HGROUP)
  ENDIF
ENDIF

LPACK=GPACK

END SUBROUTINE MENU_DIACHRO

end module mode_menu_diachro
#endif
