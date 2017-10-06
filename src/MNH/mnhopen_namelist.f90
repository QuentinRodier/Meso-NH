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
USE MODE_ll
USE MODE_IO_ll
USE MODE_FM
USE MODD_CONF,        ONLY : CPROGRAM
USE MODD_LUNIT,       ONLY : CLUOUT0
USE MODD_IO_NAM,      ONLY : CNAM
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
INTEGER           :: ILUOUT         ! output listing logical unit
CHARACTER(LEN=16) :: YLUOUT         ! output listing file name
!-------------------------------------------------------------------------------
!
SELECT CASE(CPROGRAM)
  CASE('MESONH','SPAWN ')
    CALL GET_MODEL_NUMBER_ll  (IMI)
    WRITE(YLUOUT,FMT='(A14,I1,A1)') 'OUTPUT_LISTING',IMI,' '
  CASE DEFAULT
    YLUOUT = CLUOUT0
END SELECT
!
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
     print*,"MNHOPEN_NAMELIST :: CPROGRAM=", CPROGRAM,"####"
     STOP "MNHOPEN_NAMELIST : CPROGRAM NOT ALLOWED " 
 END SELECT
END IF
!
CALL OPEN_ll(KLUNAM,FILE=CNAM,IOSTAT=IRESP,ACTION='READ', &
             FORM="FORMATTED",POSITION="REWIND",MODE='GLOBAL')
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE MNHOPEN_NAMELIST
