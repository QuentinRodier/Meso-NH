!MNH_LIC Copyright 1994-2013 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENCE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! MASDEV4_7 init 2006/05/18 13:07:25
!-----------------------------------------------------------------
!      ###########################
MODULE MODI_WRITE_DUMMY_GR_FIELD_n
!      ###########################
!
INTERFACE
!
      SUBROUTINE WRITE_DUMMY_GR_FIELD_n(HFMFILE)
!
CHARACTER(LEN=28), INTENT(IN) :: HFMFILE      ! Name of FM-file to write
!
END SUBROUTINE WRITE_DUMMY_GR_FIELD_n
!
END INTERFACE
!
END MODULE MODI_WRITE_DUMMY_GR_FIELD_n
!
!     ##########################################
      SUBROUTINE WRITE_DUMMY_GR_FIELD_n(HFMFILE)
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
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_DUMMY_GR_FIELD_n, ONLY : NDUMMY_GR_NBR, CDUMMY_GR_NAME,    &
                                  CDUMMY_GR_AREA, XDUMMY_GR_FIELDS
USE MODD_LUNIT_n,          ONLY : CLUOUT
!
USE MODE_FMWRIT
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
CHARACTER(LEN=28), INTENT(IN) :: HFMFILE      ! Name of FM-file to write
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER           :: IRESP          ! IRESP  : return-code if a problem appears 
                                    ! at the open of the file in LFI  routines 
INTEGER           :: IGRID          ! IGRID : grid indicator
INTEGER           :: ILENCH         ! ILENCH : length of comment string
INTEGER           :: JDUMMY         ! loop counter
!
CHARACTER(LEN=16) :: YRECFM         ! Name of the article to be written
CHARACTER(LEN=100):: YCOMMENT       ! Comment string
CHARACTER(LEN=20 ):: YSTRING20      ! string
CHARACTER(LEN=3  ):: YSTRING03      ! string
!
REAL, DIMENSION(:,:), ALLOCATABLE :: ZWORK2D
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
YRECFM='DUMMY_GR_NBR'
YCOMMENT='INTEGER'
ILENCH=LEN(YCOMMENT)
!
CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'--',NDUMMY_GR_NBR,0,ILENCH,YCOMMENT,IRESP)
!
DO JDUMMY=1,NDUMMY_GR_NBR
  !
  WRITE(YRECFM,'(A8,I3.3,A5)') 'DUMMY_GR',JDUMMY,'     '
  YSTRING20=CDUMMY_GR_NAME(JDUMMY)
  YSTRING03=CDUMMY_GR_AREA(JDUMMY)
  YCOMMENT='X_Y_'//YRECFM//YSTRING20//YSTRING03//  &
           '                                                             '
  IGRID=4
  ILENCH=LEN(YCOMMENT)
  ZWORK2D(:,:) = XDUMMY_GR_FIELDS(:,:,JDUMMY)
  CALL FMWRIT(HFMFILE,YRECFM,CLUOUT,'XY',ZWORK2D,IGRID,ILENCH,YCOMMENT,IRESP)
END DO
!
!-------------------------------------------------------------------------------
!
DEALLOCATE(ZWORK2D)
!
END SUBROUTINE WRITE_DUMMY_GR_FIELD_n
