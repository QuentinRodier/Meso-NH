!MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! MASDEV4_7 surfex 2006/05/18 13:07:25
!-----------------------------------------------------------------
!     #############################
      MODULE MODI_MNHCLOSE_NAMELIST
!     #############################
INTERFACE
      SUBROUTINE MNHCLOSE_NAMELIST(HPROGRAM,KLUNAM)
!
CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! main program
INTEGER,           INTENT(IN)  :: KLUNAM   ! logical unit of namelist
!
END SUBROUTINE MNHCLOSE_NAMELIST
!
END INTERFACE
END MODULE MODI_MNHCLOSE_NAMELIST
!
!     #######################################################
      SUBROUTINE MNHCLOSE_NAMELIST(HPROGRAM,KLUNAM)
!     #######################################################
!
!!****  *MNHCLOSE_NAMELIST* - closes namelists read by surface in MESOHN
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
USE MODE_ll
USE MODE_FM
USE MODE_IO_ll
USE MODD_LUNIT,       ONLY : CLUOUT0
USE MODD_CONF,        ONLY : CPROGRAM
USE MODD_IO_NAM,      ONLY : CNAM
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! main program
INTEGER,           INTENT(IN)  :: KLUNAM   ! logical unit of namelist
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER           :: IRESP          ! IRESP  : return-code if a problem appears 
                                    ! at the open of the file in LFI  routines 
!
INTEGER           :: INAM           ! logical unit of namelist
INTEGER           :: IMI            ! model index
INTEGER           :: ILUOUT         ! output listing logical unit
CHARACTER(LEN=16) :: YLUOUT         ! output listing file name
!-------------------------------------------------------------------------------
!
SELECT CASE(CPROGRAM)
  CASE('REAL  ','IDEAL ','DIAG  ')
    YLUOUT = CLUOUT0
  CASE('MESONH','SPAWN ')
    CALL GET_MODEL_NUMBER_ll  (IMI)
    WRITE(YLUOUT,FMT='(A14,I1,A1)') 'OUTPUT_LISTING',IMI,' '
END SELECT
!
!-------------------------------------------------------------------------------
!
!* closes the namelist
!  -------------------
!
CALL FMLOOK_ll(CNAM,YLUOUT,INAM,IRESP)
IF (INAM==KLUNAM) THEN
  CALL CLOSE_ll(CNAM,IRESP)
  CNAM = "                            "
ELSE
  CALL FMLOOK_ll(YLUOUT,YLUOUT,ILUOUT,IRESP)
  WRITE(ILUOUT,*) 'Error for closing a namelist file: '
  WRITE(ILUOUT,*) 'logical unit ',KLUNAM,' does not correspond to namelist file', CNAM 
!callabortstop
  CALL CLOSE_ll(CLUOUT0,IOSTAT=IRESP)
  CALL ABORT
  STOP
END IF
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE MNHCLOSE_NAMELIST
