!MNH_LIC Copyright 1996-2021 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!      ###########################
MODULE MODI_WRITE_DUMMY_GR_FIELD_n
!      ###########################
!
INTERFACE
!
      SUBROUTINE WRITE_DUMMY_GR_FIELD_n(TPFILE)
!
USE MODD_IO, ONLY: TFILEDATA
!
TYPE(TFILEDATA),   INTENT(IN) :: TPFILE ! File characteristics
!
END SUBROUTINE WRITE_DUMMY_GR_FIELD_n
!
END INTERFACE
!
END MODULE MODI_WRITE_DUMMY_GR_FIELD_n
!
!     ##########################################
      SUBROUTINE WRITE_DUMMY_GR_FIELD_n(TPFILE)
!     ##########################################
!
!!****  *WRITE_DUMMY_GR_FIELD_n* - routine to write in LFIFM file the
!!                          dummy physiographic data file of model _n
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!          
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
!!      Original    27/09/96 
!!                  15/03/99 (V. Masson) use of cover types
!                   02/07/00 (F.Solmon) adaptation for patch variables
!!  Philippe Wautelet: 05/2016-04/2018: new data structures and calls for I/O
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_DUMMY_GR_FIELD_n, ONLY: NDUMMY_GR_NBR, CDUMMY_GR_NAME,    &
                                 CDUMMY_GR_AREA, XDUMMY_GR_FIELDS
use modd_field,            only: tfieldmetadata, TYPEINT, TYPEREAL
USE MODD_IO,               ONLY: TFILEDATA
USE MODD_PARAMETERS,       ONLY: NMNHNAMELGTMAX
!
USE MODE_IO_FIELD_WRITE,   only: IO_Field_write
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
TYPE(TFILEDATA),   INTENT(IN) :: TPFILE ! File characteristics
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER           :: JDUMMY         ! loop counter
!
CHARACTER(LEN=NMNHNAMELGTMAX) :: YRECFM  ! Name of the article to be written
CHARACTER(LEN=20)             :: YSTRING20
CHARACTER(LEN=3)              :: YSTRING03
!
REAL, DIMENSION(:,:), ALLOCATABLE :: ZWORK2D
!
TYPE(TFIELDMETADATA) :: TZFIELD
!
!-------------------------------------------------------------------------------
!
!*       1.     WRITES IN THE LFI FILE
!	        -----------------------
!
!*       2.     Physiographic data fields:
!               -------------------------
!
ALLOCATE(ZWORK2D(SIZE(XDUMMY_GR_FIELDS,1),SIZE(XDUMMY_GR_FIELDS,2)))
!
!-------------------------------------------------------------------------------
!
!*       3.     Dummy fields :
!               ------------
!
TZFIELD = TFIELDMETADATA(                                   &
  CMNHNAME   = 'DUMMY_GR_NBR',                              &
  CSTDNAME   = '',                                          &
  CLONGNAME  = 'DUMMY_GR_NBR',                              &
  CUNITS     = '',                                          &
  CDIR       = '--',                                        &
  CCOMMENT   = 'number of dummy pgd fields chosen by user', &
  NGRID      = 0,                                           &
  NTYPE      = TYPEINT,                                     &
  NDIMS      = 0,                                           &
  LTIMEDEP   = .FALSE.                                      )
CALL IO_Field_write(TPFILE,TZFIELD,NDUMMY_GR_NBR)
!
DO JDUMMY=1,NDUMMY_GR_NBR
  WRITE(YRECFM,'(A8,I3.3)') 'DUMMY_GR',JDUMMY
  YSTRING20=CDUMMY_GR_NAME(JDUMMY)
  YSTRING03=CDUMMY_GR_AREA(JDUMMY)
  !
  TZFIELD = TFIELDMETADATA(                            &
    CMNHNAME   = TRIM(YRECFM),                         &
    CSTDNAME   = '',                                   &
    CLONGNAME  = TRIM(YRECFM),                         &
    CUNITS     = '',                                   &
    CDIR       = 'XY',                                 &
    CCOMMENT   = 'X_Y_'//YRECFM//YSTRING20//YSTRING03, &
    NGRID      = 4,                                    &
    NTYPE      = TYPEREAL,                             &
    NDIMS      = 2,                                    &
    LTIMEDEP   = .TRUE.                                )
  !
  ZWORK2D(:,:) = XDUMMY_GR_FIELDS(:,:,JDUMMY)
  !
  CALL IO_Field_write(TPFILE,TZFIELD,ZWORK2D)
END DO
!
!-------------------------------------------------------------------------------
!
DEALLOCATE(ZWORK2D)
!
END SUBROUTINE WRITE_DUMMY_GR_FIELD_n
