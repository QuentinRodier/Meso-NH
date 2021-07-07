!MNH_LIC Copyright 2020-2021 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!     #######################
       MODULE MODI_EOL_ERROR
!     #######################
!
INTERFACE
!
! **********
! EOL_READER
! **********
!
SUBROUTINE EOL_CSVNOTFOUND_ERROR(HFILE)
  CHARACTER(LEN=*),  INTENT(IN)    :: HFILE    ! file read   
END SUBROUTINE EOL_CSVNOTFOUND_ERROR
!
SUBROUTINE EOL_CSVEMPTY_ERROR(HFILE,KNBLINE)
  CHARACTER(LEN=*),  INTENT(IN)    :: HFILE    ! file read   
  INTEGER,           INTENT(IN)    :: KNBLINE  ! number of lines
END SUBROUTINE EOL_CSVEMPTY_ERROR
!
!
! ***
! ALM
! ***
!
SUBROUTINE EOL_AIRFOILNOTFOUND_ERROR(HFILE,HVAR)
  CHARACTER(LEN=*),  INTENT(IN)    :: HFILE    ! file read   
  CHARACTER(LEN=*),  INTENT(IN)    :: HVAR     ! missing data
END SUBROUTINE EOL_AIRFOILNOTFOUND_ERROR
!
SUBROUTINE EOL_WTCFL_ERROR(PMAXTSTEP)
   REAL, INTENT(IN) :: PMAXTSTEP               ! maximum acceptable time-step 
END SUBROUTINE EOL_WTCFL_ERROR
!
SUBROUTINE EOL_BLADEDATA_ERROR(PDELTARAD)
   REAL, INTENT(IN) :: PDELTARAD               ! Span lenght of an element
END SUBROUTINE EOL_BLADEDATA_ERROR
!
END INTERFACE
!
END MODULE MODI_EOL_ERROR
!-------------------------------------------------------------------
!
!!****  *EOL_ERROR* -
!!
!!    PURPOSE
!!    -------
!!    Some usefull subs to manage errors linked to wind turbines
!!
!!    AUTHOR
!!    ------
!!     PA. Joulin 		*CNRM & IFPEN*
!!
!!    MODIFICATIONS
!!    -------------
!!    Original     15/12/2020  
!!
!!---------------------------------------------------------------
!
!#########################################################
SUBROUTINE EOL_CSVNOTFOUND_ERROR(HFILE)
!
USE MODD_LUNIT_n, ONLY: TLUOUT
USE MODE_IO_FILE, ONLY: IO_File_close
USE MODE_MSG
!
CHARACTER(LEN=*),  INTENT(IN)    :: HFILE    ! file read
!
CMNHMSG(1) = 'EOL Initialization error: CSV file for wind turbine missing'
CMNHMSG(2) = 'File: ' // TRIM( HFILE ) // ' not found'
CALL PRINT_MSG( NVERB_FATAL, 'GEN', 'EOL_CSVNOTFOUND_ERROR' )
!
END SUBROUTINE EOL_CSVNOTFOUND_ERROR
!#########################################################

!
!#########################################################
SUBROUTINE EOL_CSVEMPTY_ERROR(HFILE,KNBLINE)
!
USE MODD_LUNIT_n, ONLY: TLUOUT
USE MODE_IO_FILE, ONLY: IO_File_close
USE MODE_MSG
!
CHARACTER(LEN=*),  INTENT(IN)    :: HFILE    ! file read   
INTEGER,           INTENT(IN)    :: KNBLINE  ! number of lines
!
CHARACTER(LEN=8) :: YLINES
!
WRITE( YLINES, '( I8 )' ) KNBLINE
!
CMNHMSG(1) = 'EOL Initialization error: missing data in CSV file for wind turbine'
CMNHMSG(2) = TRIM( YLINES ) // ' line(s) have been read in file ' // TRIM( HFILE )
CMNHMSG(3) = 'At least 2 should be there: header + data'
CALL PRINT_MSG( NVERB_FATAL, 'GEN', 'EOL_CSVEMPTY_ERROR' )
!
END SUBROUTINE EOL_CSVEMPTY_ERROR
!#########################################################
!
!#########################################################
SUBROUTINE EOL_AIRFOILNOTFOUND_ERROR(HFILE,HVAR)
!
USE MODD_LUNIT_n, ONLY: TLUOUT
USE MODE_IO_FILE, ONLY: IO_File_close
USE MODE_MSG
!
CHARACTER(LEN=*),  INTENT(IN)    :: HFILE    ! file read   
CHARACTER(LEN=*),  INTENT(IN)    :: HVAR     ! missing data
!
CMNHMSG(1) = 'EOL Initialization error: missing data for airfoil'
CMNHMSG(2) = 'Characteristics for ' // TRIM( HVAR )
CMNHMSG(3) = 'not found in file ' // TRIM( HFILE )
CALL PRINT_MSG( NVERB_FATAL, 'GEN', 'EOL_AIRFOILNOTFOUND_ERROR' )
!
END SUBROUTINE EOL_AIRFOILNOTFOUND_ERROR
!#########################################################
!
!#########################################################
SUBROUTINE EOL_WTCFL_ERROR(PMAXTSTEP)
!
USE MODD_LUNIT_n, ONLY: TLUOUT
USE MODE_IO_FILE, ONLY: IO_File_close
USE MODE_MSG
!
REAL,    INTENT(IN) :: PMAXTSTEP    ! maximum acceptable time-step
!
CHARACTER(LEN=10) :: YMAXTSTEP
! 
WRITE( YMAXTSTEP, '( F10.8 )' ) PMAXTSTEP
!
CMNHMSG(1) = 'EOL Initialization error: wrong time-step with wind turbine'
CMNHMSG(2) = 'Time-step XTSTEP too large: blades can jump over one or several cells'
CMNHMSG(3) = 'Turn on the time-splitting method (LTIMESPLIT=.TRUE.)'
CMNHMSG(4) = 'or decrease XTSTEP to a value lower than ' // TRIM(YMAXTSTEP) // ' s'
CALL PRINT_MSG( NVERB_FATAL, 'GEN', 'EOL_WTCFL_ERROR' )
!
END SUBROUTINE EOL_WTCFL_ERROR
!#########################################################
!
!#########################################################
SUBROUTINE EOL_BLADEDATA_ERROR(PDELTARAD)
!
USE MODD_LUNIT_n, ONLY: TLUOUT
USE MODE_IO_FILE, ONLY: IO_File_close
USE MODE_MSG
USE MODD_EOL_SHARED_IO, ONLY: CBLADE_CSVDATA
!
REAL,    INTENT(IN) :: PDELTARAD    ! hals section width 
! 
CHARACTER(LEN=4) :: YDELTARAD
!
WRITE( YDELTARAD, '( F4.2 )' ) PDELTARAD
!
CMNHMSG(1) = 'EOL Initialization error: error in blade data'
CMNHMSG(2) = 'A blade element center position is set to ' // TRIM( YDELTARAD )
CMNHMSG(3) = 'As a blade element center, it has to be set in ]0%;100%['
CMNHMSG(4) = 'Please, check your blade data in ' // TRIM(CBLADE_CSVDATA)
CMNHMSG(5) = 'and make sure it is element centers (not nodes) along the blade'
CALL PRINT_MSG( NVERB_FATAL, 'GEN', 'EOL_BLADEDATA_ERROR' )
!
END SUBROUTINE EOL_BLADEDATA_ERROR
!#########################################################
!
