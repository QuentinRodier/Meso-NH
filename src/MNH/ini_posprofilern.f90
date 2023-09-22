!MNH_LIC Copyright 1994-2023 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!      #########################
MODULE MODI_INI_POSPROFILER_n
!      #########################
!
INTERFACE
!
      SUBROUTINE INI_POSPROFILER_n( )
!
END SUBROUTINE INI_POSPROFILER_n
!
END INTERFACE
!
END MODULE MODI_INI_POSPROFILER_n
!
!     ###############################
      SUBROUTINE INI_POSPROFILER_n( )
!     ###############################
!
!
!!****  *INI_POSPROFILER_n* - 
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
!!     P. Tulet 15/01/2002  
!!     C.Lac 10/2016  Add visibility diagnostic
!!  Philippe Wautelet: 05/2016-04/2018: new data structures and calls for I/O
!  P. Wautelet 13/09/2019: budget: simplify and modernize date/time management
!  M. Taufour  05/07/2021: modify RARE for hydrometeors containing ice and add bright band calculation for RARE
!  P. Wautelet    04/2022: restructure profilers for better performance, reduce memory usage and correct some problems/bugs
!! --------------------------------------------------------------------------
!
!*      0. DECLARATIONS
!          ------------
!
USE MODD_ALLPROFILER_n
USE MODD_CONF,           ONLY: LCARTESIAN
USE MODD_DYN,            ONLY: XSEGLEN
USE MODD_DYN_n,          ONLY: DYN_MODEL, XTSTEP
USE MODD_PROFILER_n,     ONLY: LPROFILER, NUMBPROFILER_LOC, TPROFILERS, TPROFILERS_TIME
USE MODD_TYPE_STATPROF,  ONLY: TPROFILERDATA

USE MODE_MSG
USE MODE_STATPROF_READER, ONLY: STATPROF_CSV_READ
USE MODE_STATPROF_TOOLS,  ONLY: PROFILER_ADD, STATPROF_INI_INTERP, STATPROF_POSITION

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
INTEGER :: INUMBPROF                        ! Total number of profilers (inside physical domain of model)
INTEGER :: ISTORE                           ! number of storage instants
INTEGER :: JI
LOGICAL :: GINSIDE                          ! True if profiler is inside physical domain of model
LOGICAL :: GPRESENT                         ! True if profiler is present on the current process
TYPE(TPROFILERDATA)          :: TZPROFILER
!
!----------------------------------------------------------------------------

TPROFILERS_TIME%XTSTEP = XSTEP_PROF

if ( tprofilers_time%xtstep < xtstep ) then
  call Print_msg( NVERB_WARNING, 'GEN', 'INI_POSPROFILER_n', 'Timestep for profilers was smaller than model timestep' )
  tprofilers_time%xtstep = xtstep
end if

ISTORE = NINT ( ( XSEGLEN - DYN_MODEL(1)%XTSTEP ) / TPROFILERS_TIME%XTSTEP ) + 1

allocate( tprofilers_time%tpdates(istore) )
!
! Profilers initialization
!
NUMBPROFILER_LOC = 0

IF (CFILE_PROF=="NO_INPUT_CSV") THEN
  IF ( NNUMB_PROF > NNOCSVPROFILERMAX ) &
    call Print_msg( NVERB_FATAL, 'IO', 'INI_POSPROFILER_n', 'number of profilers is limited to NNOCSVPROFILERMAX' )

  ! Treat namelist
  INUMBPROF = 0
  IF ( NNUMB_PROF > 0 ) THEN
    DO JI = 1, NNUMB_PROF
      IF ( LCARTESIAN ) THEN
        TZPROFILER%XX_CUR = XX_PROF(JI)
        TZPROFILER%XY_CUR = XY_PROF(JI)
      ELSE
        TZPROFILER%XLAT_CUR = XLAT_PROF(JI)
        TZPROFILER%XLON_CUR = XLON_PROF(JI)
        CALL STATPROF_INI_INTERP( TZPROFILER )
      END IF
      TZPROFILER%XZ_CUR = XZ_PROF(JI)
      TZPROFILER%CNAME  = CNAME_PROF(JI)

      CALL STATPROF_POSITION( TZPROFILER, GINSIDE, GPRESENT )

      IF ( GINSIDE ) THEN
        INUMBPROF = INUMBPROF + 1
        TZPROFILER%NID = INUMBPROF
      END IF

      IF ( GPRESENT ) CALL PROFILER_ADD( TZPROFILER )
    END DO
  END IF
ELSE
  !Treat CSV datafile
  CALL STATPROF_CSV_READ( TZPROFILER, CFILE_PROF, INUMBPROF )
END IF

LPROFILER = ( INUMBPROF > 0 )

DO JI = 1, NUMBPROFILER_LOC
  TPROFILERS(JI)%LFIX = .TRUE.
  CALL TPROFILERS(JI)%DATA_ARRAYS_ALLOCATE( ISTORE )
END DO
!----------------------------------------------------------------------------
!
END SUBROUTINE INI_POSPROFILER_n
