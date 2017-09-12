!MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! MASDEV4_7 diachro 2006/05/18 13:07:25
!-----------------------------------------------------------------
!     ##################################################
      SUBROUTINE MENU_DIACHRO(TPDIAFILE,HLUOUTDIA,HGROUP)
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
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_CONF
USE MODD_IO_ll, ONLY: TFILEDATA
!
USE MODE_FIELD
USE MODE_FMREAD
USE MODE_FMWRIT
!
IMPLICIT NONE
!
!*       0.1   Dummy arguments
!              ---------------

TYPE(TFILEDATA),  INTENT(IN) :: TPDIAFILE    ! file to write
CHARACTER(LEN=*), INTENT(IN) :: HLUOUTDIA
CHARACTER(LEN=*), INTENT(IN) :: HGROUP
!
!*       0.1   Local variables
!              ---------------

!
CHARACTER(LEN=16),DIMENSION(1500),SAVE    :: YGROUP
INTEGER   ::   ILENG, J, JJ, IALREADY
INTEGER   ::   IRESPDIA
INTEGER,SAVE   ::   IGROUP=0
INTEGER,DIMENSION(:),ALLOCATABLE :: ITABCHAR
LOGICAL   ::   GPACK
TYPE(TFIELDDATA)  :: TZFIELD
!------------------------------------------------------------------------------
!
GPACK=LPACK
LPACK=.FALSE.
!
IF(HGROUP == 'END')THEN

  IF(IGROUP == 0)THEN
!   print *,' No record for the diachronic file' mettre les prints dans le fichier LISTING
    LPACK=GPACK
    RETURN
  ENDIF
  ILENG=16*IGROUP

  TZFIELD%CMNHNAME   = 'MENU_BUDGET.DIM'
  TZFIELD%CSTDNAME   = ''
  TZFIELD%CLONGNAME  = 'MesoNH: MENU_BUDGET.DIM'
  TZFIELD%CUNITS     = ''
  TZFIELD%CDIR       = '--'
  TZFIELD%CCOMMENT   = ''
  TZFIELD%NGRID      = 0
  TZFIELD%NTYPE      = TYPEINT
  TZFIELD%NDIMS      = 0
  CALL IO_WRITE_FIELD(TPDIAFILE,TZFIELD,ILENG)

  ALLOCATE(ITABCHAR(ILENG))
  DO JJ=1,IGROUP
    DO J = 1,16
      ITABCHAR(16*(JJ-1)+J) = ICHAR(YGROUP(JJ)(J:J))
    ENDDO
  ENDDO

  TZFIELD%CMNHNAME   = 'MENU_BUDGET'
  TZFIELD%CSTDNAME   = ''
  TZFIELD%CLONGNAME  = 'MesoNH: MENU_BUDGET'
  TZFIELD%CUNITS     = ''
  TZFIELD%CDIR       = '--'
  TZFIELD%CCOMMENT   = ''
  TZFIELD%NGRID      = 0
  TZFIELD%NTYPE      = TYPEINT
  TZFIELD%NDIMS      = 1
  CALL IO_WRITE_FIELD(TPDIAFILE,TZFIELD,ITABCHAR)

  DEALLOCATE(ITABCHAR)

ELSE IF(HGROUP == 'READ')THEN

  TZFIELD%CMNHNAME   = 'MENU_BUDGET.DIM'
  TZFIELD%CSTDNAME   = ''
  TZFIELD%CLONGNAME  = 'MesoNH: MENU_BUDGET.DIM'
  TZFIELD%CUNITS     = ''
  TZFIELD%CDIR       = '--'
  TZFIELD%CCOMMENT   = ''
  TZFIELD%NGRID      = 0
  TZFIELD%NTYPE      = TYPEINT
  TZFIELD%NDIMS      = 0
  CALL IO_READ_FIELD(TPDIAFILE,TZFIELD,ILENG,IRESPDIA)
  IF(IRESPDIA == -47)THEN
!   print *,' No record MENU_BUDGET '
    LPACK=GPACK
    RETURN
  ENDIF

  ALLOCATE(ITABCHAR(ILENG))
  IGROUP=ILENG/16
! print *,' ILENG ILENCH IGROUP ',ILENG,ILENCH,IGROUP
  TZFIELD%CMNHNAME   = 'MENU_BUDGET'
  TZFIELD%CSTDNAME   = ''
  TZFIELD%CLONGNAME  = 'MesoNH: MENU_BUDGET'
  TZFIELD%CUNITS     = ''
  TZFIELD%CDIR       = '--'
  TZFIELD%CCOMMENT   = ''
  TZFIELD%NGRID      = 0
  TZFIELD%NTYPE      = TYPEINT
  TZFIELD%NDIMS      = 1
  CALL IO_READ_FIELD(TPDIAFILE,TZFIELD,ITABCHAR)
  DO JJ=1,IGROUP
    DO J = 1,16
      YGROUP(JJ)(J:J)=CHAR(ITABCHAR(16*(JJ-1)+J))
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
    YGROUP(IGROUP)=ADJUSTL(HGROUP)
  ENDIF
ENDIF

LPACK=GPACK
!
!-----------------------------------------------------------------------------
!
!*       2.       EXITS
!                 -----
!
RETURN
END SUBROUTINE MENU_DIACHRO
