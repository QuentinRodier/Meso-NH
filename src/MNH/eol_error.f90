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
WRITE(TLUOUT%NLU,*) ''
WRITE(TLUOUT%NLU,'(A)')                                                &
 ' EOL Initialization error. Program aborted.'
WRITE(TLUOUT%NLU,'(A,A,A)')                                            &
 ' File "', TRIM(HFILE), '" not found in directory.'
WRITE(TLUOUT%NLU,*) ''
!
CALL IO_File_close(TLUOUT)
CALL PRINT_MSG( NVERB_FATAL, 'GEN', 'EOL_CSVNOTFOUND_ERROR', 'CSV file for wind turbine missing' )
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
WRITE(TLUOUT%NLU,*) ''
WRITE(TLUOUT%NLU,'(A)')                                                &
 ' EOL Initialization error. Program aborted.'
WRITE(TLUOUT%NLU,'(I2,A,A,A)')                                         &
 KNBLINE, ' line have been read in file: ', TRIM(HFILE), '.'
WRITE(TLUOUT%NLU,'(A)')                                                &
 ' At least 2 should be there: header + data.'
WRITE(TLUOUT%NLU,*) ''
!
CALL IO_File_close(TLUOUT)
CALL PRINT_MSG( NVERB_FATAL, 'GEN', 'EOL_CSVEMPTY_ERROR', 'CSV file for wind turbine missing data' )
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
WRITE(TLUOUT%NLU,*) ''
WRITE(TLUOUT%NLU,'(A)')                                      &
 ' EOL Initialization error. Program aborted.'
WRITE(TLUOUT%NLU,'(A,A,A)')                                  &
 ' I am looking for the characteristics of ', TRIM(HVAR), '.'
WRITE(TLUOUT%NLU,'(A,A)')                                    &
 ' But I cannot find them in file: ', TRIM(HFILE)
WRITE(TLUOUT%NLU,*) ''
!
CALL IO_File_close(TLUOUT)
CALL PRINT_MSG( NVERB_FATAL, 'GEN', 'EOL_AIRFOILNOTFOUND_ERROR', 'File for Airfol missing' )
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
WRITE(TLUOUT%NLU,*) ''
WRITE(TLUOUT%NLU,'(A)')                                            &
 'Sorry but I had to stop the simulation. '
WRITE(TLUOUT%NLU,'(A)')                                            &
 'The time step XTSTEP is too large: '
WRITE(TLUOUT%NLU,'(A)')                                            &
 'the blades can jump over one or several cells. '
WRITE(TLUOUT%NLU,'(A)')                                            &
 'Please, turn on the time-splitting method (LTIMESPLIT=.TRUE.), '
WRITE(TLUOUT%NLU,'(A,F10.8,A)')                                    &
 'or decrease XTSTEP to a value lower than ', PMAXTSTEP, ' sec.'
WRITE(TLUOUT%NLU,*) ''
!
CALL IO_File_close(TLUOUT)
CALL PRINT_MSG( NVERB_FATAL, 'GEN', 'EOL_WTCFL_ERROR', 'WRONG TIME-STEP with wind turbine' )
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
WRITE(TLUOUT%NLU,*) ''
WRITE(TLUOUT%NLU,'(A)')                                            &
 ' EOL Initialization error. Program aborted.'
WRITE(TLUOUT%NLU,'(A,F4.2,A)')                                     &
 'A blade element center position is set to ', PDELTARAD, '.'
WRITE(TLUOUT%NLU,'(A)')                                            &
 'As a blade element center, it has to be set in ]0%;100%[. '
WRITE(TLUOUT%NLU,'(A,A,A)')                                        &
 'Please, check your blade data in ', TRIM(CBLADE_CSVDATA), ','
WRITE(TLUOUT%NLU,'(A)')                                            &
'and make sure it is element centers (not nodes) along the blade.'
WRITE(TLUOUT%NLU,*) ''
!
CALL IO_File_close(TLUOUT)
CALL PRINT_MSG( NVERB_FATAL, 'GEN', 'EOL_BLADEDATA_ERROR', 'ERROR IN BLADE DATA' )
!
END SUBROUTINE EOL_BLADEDATA_ERROR
!#########################################################
!
