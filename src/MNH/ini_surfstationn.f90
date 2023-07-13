!MNH_LIC Copyright 1994-2023 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!      #########################
MODULE MODI_INI_SURFSTATION_n
!      #########################
!
INTERFACE
!
      SUBROUTINE INI_SURFSTATION_n( )
!
END SUBROUTINE INI_SURFSTATION_n
!
END INTERFACE
!
END MODULE MODI_INI_SURFSTATION_n
!
!     ###############################
      SUBROUTINE INI_SURFSTATION_n( )
!     ###############################
!
!
!!****  *INI_SURFSTATION_n* - 
!!
!!    PURPOSE
!!    -------
!
!
!!**  METHOD
!!    ------
!!    
!!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!      Valery Masson             * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!  P. Tulet     15/01/2002
!  A. Lemonsu   19/11/2002
!  P. Wautelet  05/2016-04/2018: new data structures and calls for I/O
!  P. Wautelet  13/09/2019: budget: simplify and modernize date/time management
!  R. Schoetter    11/2019: work for cartesian coordinates + parallel.
!  E.Jezequel      02/2021: read stations from CVS file
!  P. Wautelet     04/2022: restructure stations for better performance, reduce memory usage and correct some problems/bugs
! --------------------------------------------------------------------------
!
!*      0. DECLARATIONS
!          ------------
!
USE MODD_ALLSTATION_n
USE MODD_CONF,           ONLY: LCARTESIAN
USE MODD_DYN,            ONLY: XSEGLEN
USE MODD_DYN_n,          ONLY: DYN_MODEL, XTSTEP
USE MODD_STATION_n
USE MODD_TYPE_STATPROF
!
USE MODE_MSG
USE MODE_STATPROF_READER, ONLY: STATPROF_CSV_READ
USE MODE_STATPROF_TOOLS,  ONLY: STATION_ADD, STATPROF_INI_INTERP, STATPROF_POSITION
!
IMPLICIT NONE
!
!
!*      0.1  declarations of arguments
!
! NONE
!
!-------------------------------------------------------------------------------
!
!       0.2  declaration of local variables
!
INTEGER :: INUMBSTAT                        ! Total number of stations (inside physical domain of model)
INTEGER :: ISTORE                           ! number of storage instants
INTEGER :: JI
LOGICAL :: GINSIDE                          ! True if station is inside physical domain of model
LOGICAL :: GPRESENT                         ! True if station is present on the current process
TYPE(TSTATIONDATA)           :: TZSTATION
!
!----------------------------------------------------------------------------

TSTATIONS_TIME%XTSTEP = XSTEP_STAT

if ( tstations_time%xtstep < xtstep ) then
  call Print_msg( NVERB_WARNING, 'GEN', 'INI_SURFSTATION_n', 'Timestep for stations was smaller than model timestep' )
  tstations_time%xtstep = xtstep
end if

ISTORE = NINT ( ( XSEGLEN - DYN_MODEL(1)%XTSTEP ) / TSTATIONS_TIME%XTSTEP ) + 1

allocate( tstations_time%tpdates(istore) )
!
! Stations initialization
!
NUMBSTAT_LOC = 0

IF (CFILE_STAT=="NO_INPUT_CSV") THEN
  IF ( NNUMB_STAT > NNOCSVSTATIONMAX ) &
    call Print_msg( NVERB_FATAL, 'IO', 'INI_SURFSTATION_n', 'number of stations is limited to NNOCSVSTATIONMAX' )

  ! Treat namelist
  INUMBSTAT = 0
  IF ( NNUMB_STAT > 0 ) THEN
    DO JI = 1, NNUMB_STAT
      IF ( LCARTESIAN ) THEN
        TZSTATION%XX_CUR = XX_STAT(JI)
        TZSTATION%XY_CUR = XY_STAT(JI)
      ELSE
        TZSTATION%XLAT_CUR = XLAT_STAT(JI)
        TZSTATION%XLON_CUR = XLON_STAT(JI)
        CALL STATPROF_INI_INTERP( TZSTATION )
      END IF
      TZSTATION%XZ_CUR = XZ_STAT(JI)
      TZSTATION%CNAME  = CNAME_STAT(JI)

      CALL STATPROF_POSITION( TZSTATION, GINSIDE, GPRESENT )

      IF ( GINSIDE ) THEN
        INUMBSTAT = INUMBSTAT + 1
        TZSTATION%NID = INUMBSTAT
      END IF

      IF ( GPRESENT ) CALL STATION_ADD( TZSTATION )
    END DO
  END IF
ELSE
  !Treat CSV datafile
  CALL STATPROF_CSV_READ( TZSTATION, CFILE_STAT, INUMBSTAT )
END IF

LSTATION = ( INUMBSTAT > 0 )

DO JI = 1, NUMBSTAT_LOC
  CALL TSTATIONS(JI)%DATA_ARRAYS_ALLOCATE( ISTORE )
END DO

!----------------------------------------------------------------------------

END SUBROUTINE INI_SURFSTATION_n
