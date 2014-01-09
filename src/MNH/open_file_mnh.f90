!MNH_LIC Copyright 1994-2013 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENCE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! MASDEV4_7 surfex 2006/05/18 13:07:25
!-----------------------------------------------------------------
!     #########################
      MODULE MODI_OPEN_FILE_MNH
!     #########################
INTERFACE
      SUBROUTINE OPEN_FILE_MNH(KUNIT,HFILE,HFORM,HACTION,HACCESS,KRECL)
!
INTEGER,           INTENT(OUT):: KUNIT    ! logical unit
CHARACTER(LEN=28), INTENT(IN) :: HFILE    ! file to open
CHARACTER(LEN=11), INTENT(IN) :: HFORM    ! type of file
CHARACTER(LEN=9),  INTENT(IN) :: HACTION  ! action
CHARACTER(LEN=6),  INTENT(IN) :: HACCESS  ! access type
INTEGER,           INTENT(IN) :: KRECL    ! record length
!
END SUBROUTINE OPEN_FILE_MNH
!
END INTERFACE
END MODULE MODI_OPEN_FILE_MNH
!
!     #######################################################
      SUBROUTINE OPEN_FILE_MNH(KUNIT,HFILE,HFORM,HACTION,HACCESS,KRECL)
!     #######################################################
!
!!****  *OPEN_FILE_MNH* - routine to open a file
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!	V. Masson   *Meteo France*	
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2003 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODE_IO_ll
USE MODD_IO_NAM, ONLY : CFILE
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
INTEGER,           INTENT(OUT):: KUNIT    ! logical unit
CHARACTER(LEN=28), INTENT(IN) :: HFILE    ! file to open
CHARACTER(LEN=11), INTENT(IN) :: HFORM    ! type of file
CHARACTER(LEN=9),  INTENT(IN) :: HACTION  ! action
CHARACTER(LEN=6),  INTENT(IN) :: HACCESS  ! access type
INTEGER,           INTENT(IN) :: KRECL    ! record length
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER :: IRESP
!
!-------------------------------------------------------------------------------
!
CFILE = HFILE
!
IF (HFORM=='FORMATTED') THEN
  CALL OPEN_ll(UNIT=KUNIT,FILE=HFILE,IOSTAT=IRESP,ACTION=HACTION,   &
               FORM=HFORM, MODE=GLOBAL                              )
ELSE 
  IF (HACCESS=='DIRECT') THEN
    CALL OPEN_ll(UNIT=KUNIT,FILE=HFILE,IOSTAT=IRESP,ACTION=HACTION,   &
                 FORM=HFORM,ACCESS=HACCESS,RECL=KRECL,                &
                 MODE=GLOBAL                                          )
  ELSE
    CALL OPEN_ll(UNIT=KUNIT,FILE=HFILE,IOSTAT=IRESP,ACTION=HACTION,   &
                 FORM=HFORM, MODE=GLOBAL                               )
  END IF
END IF
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE OPEN_FILE_MNH
