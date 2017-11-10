!MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! MASDEV4_7 surfex 2006/05/24 18:05:49
!-----------------------------------------------------------------
!     #############################
      MODULE MODI_MNHOPEN_NAMELIST
!     #############################
INTERFACE
      SUBROUTINE MNHOPEN_NAMELIST(HPROGRAM,KLUNAM,HFILE)
!
CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! main program
INTEGER,           INTENT(OUT) :: KLUNAM   ! logical unit of namelist
CHARACTER(LEN=28), INTENT(IN)  :: HFILE ! ASCII file to open
!
END SUBROUTINE MNHOPEN_NAMELIST
!
END INTERFACE
END MODULE MODI_MNHOPEN_NAMELIST
!
!     #######################################################
      SUBROUTINE MNHOPEN_NAMELIST(HPROGRAM,KLUNAM,HFILE)
!     #######################################################
!
!!****  *MNHOPEN_NAMELIST* - opens namelists files for surface (MESONH universe)
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
!!      Original    01/2004 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_CONF,        ONLY : CPROGRAM
USE MODD_IO_ll,       ONLY : TFILEDATA
USE MODD_IO_NAM,      ONLY : CNAM
USE MODD_LUNIT,       ONLY : CLUOUT0
!
USE MODE_FM
USE MODE_IO_ll
USE MODE_ll
USE MODE_IO_MANAGE_STRUCT, ONLY : IO_FILE_ADD2LIST
USE MODE_MSG
USE MODE_IO_MANAGE_STRUCT, ONLY : io_file_print_list
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! main program
INTEGER,           INTENT(OUT) :: KLUNAM   ! logical unit of namelist
CHARACTER(LEN=28), INTENT(IN)  :: HFILE ! ASCII file to open
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER           :: IRESP          ! IRESP  : return-code if a problem appears 
                                    ! at the open of the file in LFI  routines 
INTEGER           :: IMI            ! model index
!
TYPE(TFILEDATA),POINTER :: TZFILE
!-------------------------------------------------------------------------------
!
TZFILE => NULL()
!-------------------------------------------------------------------------------
!
!* reading of namelist
!  -------------------
!
IF (LEN_TRIM(HFILE)>0) THEN
  CNAM = HFILE
ELSE
 SELECT CASE(CPROGRAM)
  CASE('PGD   ')
    CNAM='PRE_PGD1.nam '
  CASE('REAL  ')
    CNAM='PRE_REAL1.nam'
  CASE('IDEAL ')
    CNAM='PRE_IDEA1.nam'
  CASE('MESONH')
    CALL GET_MODEL_NUMBER_ll(IMI)
    WRITE(CNAM,FMT='(A5,I1,A22)') 'EXSEG',IMI,'.nam                  '
  CASE('DIAG  ')
    CNAM='DIAG1.nam    '
  CASE('SPAWN ')
    CNAM='SPAWN1.nam   '
  CASE('NESPGD')
    CNAM='PRE_NEST_PGD1.nam'
  CASE('ZOOMPG')
    CNAM='PRE_ZOOM1.nam'
  CASE('SPEC ')
    CNAM='SPEC1.nam'
  CASE DEFAULT
    CALL PRINT_MSG(NVERB_FATAL,'IO','MNHOPEN_NAMELIST','CPROGRAM '//TRIM(CPROGRAM)//' not allowed')
 END SELECT
END IF
!
CALL PRINT_MSG(NVERB_DEBUG,'IO','MNHOPEN_NAMELIST','called for '//TRIM(CNAM))
!
CALL IO_FILE_ADD2LIST(TZFILE,TRIM(CNAM),'NML','READ',OOLD=.TRUE.) !OOLD=T because the file may already be in list
CALL IO_FILE_OPEN_ll(TZFILE)
!
KLUNAM = TZFILE%NLU
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE MNHOPEN_NAMELIST
