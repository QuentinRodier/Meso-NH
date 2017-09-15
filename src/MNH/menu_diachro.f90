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
      SUBROUTINE MENU_DIACHRO(HFILEDIA,HLUOUTDIA,HGROUP)
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

USE MODE_FMREAD
USE MODE_FMWRIT
USE MODD_CONF

IMPLICIT NONE
!
!*       0.1   Dummy arguments
!              ---------------

CHARACTER(LEN=*) :: HGROUP
CHARACTER(LEN=*) :: HFILEDIA, HLUOUTDIA
!
!*       0.1   Local variables
!              ---------------

!
CHARACTER(LEN=LEN_HREC) :: YRECFM
CHARACTER(LEN=20) :: YCOMMENT
CHARACTER(LEN=LEN_HREC),DIMENSION(1500),SAVE    :: YGROUP
INTEGER   ::   ILENG, ILENCH, IGRID, J, JJ, IALREADY
INTEGER   ::   IRESPDIA
INTEGER,SAVE   ::   IGROUP=0
INTEGER,DIMENSION(:),ALLOCATABLE :: ITABCHAR
LOGICAL   ::   GPACK
!------------------------------------------------------------------------------
GPACK=LPACK
LPACK=.FALSE.
YCOMMENT='Nothing'
!
IF(HGROUP == 'END')THEN

  IF(IGROUP == 0)THEN
!   print *,' No record for the diachronic file' mettre les prints dans le fichier LISTING
    LPACK=GPACK
    RETURN
  ENDIF
  IGRID=0
  ILENG=LEN_HREC*IGROUP
  ILENCH=LEN(YCOMMENT)
  YRECFM='MENU_BUDGET.DIM'
  CALL FMWRIT(HFILEDIA,YRECFM,HLUOUTDIA,'--',ILENG,&
  IGRID,ILENCH,YCOMMENT,IRESPDIA)

  YRECFM='MENU_BUDGET'
  ALLOCATE(ITABCHAR(ILENG))
  DO JJ=1,IGROUP
    DO J = 1,LEN_HREC
      ITABCHAR(LEN_HREC*(JJ-1)+J) = ICHAR(YGROUP(JJ)(J:J))
    ENDDO
  ENDDO
  CALL FMWRIT(HFILEDIA,YRECFM,HLUOUTDIA,'--',ITABCHAR, &
  IGRID,ILENCH,YCOMMENT,IRESPDIA)
  DEALLOCATE(ITABCHAR)

ELSE IF(HGROUP == 'READ')THEN

  YRECFM='MENU_BUDGET.DIM'
  CALL FMREAD(HFILEDIA,YRECFM,HLUOUTDIA,'--',ILENG,&
  IGRID,ILENCH,YCOMMENT,IRESPDIA)
  IF(IRESPDIA == -47)THEN
!   print *,' No record MENU_BUDGET '
    LPACK=GPACK
    RETURN
  ENDIF

  ALLOCATE(ITABCHAR(ILENG))
  YRECFM='MENU_BUDGET'
  CALL FMREAD(HFILEDIA,YRECFM,HLUOUTDIA,'--',ITABCHAR, &
  IGRID,ILENCH,YCOMMENT,IRESPDIA)
  IGROUP=ILENG/LEN_HREC
! print *,' ILENG ILENCH IGROUP ',ILENG,ILENCH,IGROUP
  DO JJ=1,IGROUP
    DO J = 1,LEN_HREC
      YGROUP(JJ)(J:J)=CHAR(ITABCHAR(LEN_HREC*(JJ-1)+J))
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
