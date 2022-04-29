!MNH_LIC Copyright 1994-2022 CNRS, Meteo-France and Universite Paul Sabatier
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
USE MODD_GRID_n,         ONLY: XXHAT, XYHAT
USE MODD_PARAMETERS,     ONLY: JPHEXT, JPVEXT
USE MODD_STATION_n
USE MODD_TYPE_STATPROF
!
USE MODE_ALLOCBUFFER_ll,  ONLY: ALLOCBUFFER_ll
USE MODE_GATHER_ll,       ONLY: GATHERALL_FIELD_ll
USE MODE_MSG
USE MODE_STATPROF_READER, ONLY: STATPROF_CSV_READ
USE MODE_STATPROF_TOOLS,  ONLY: STATION_ADD, STATION_ALLOCATE, STATPROF_INI_INTERP, STATPROF_POSITION
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
INTEGER :: IERR
INTEGER :: IIU
INTEGER :: IJU
INTEGER :: INUMBSTAT                        ! Total number of stations (inside physical domain of model)
INTEGER :: ISTORE                           ! number of storage instants
INTEGER :: JI
LOGICAL :: GALLOCX, GALLOCY
LOGICAL :: GINSIDE                          ! True if station is inside physical domain of model
LOGICAL :: GPRESENT                         ! True if station is present on the current process
REAL    :: ZXHATM_PHYS_MIN, ZYHATM_PHYS_MIN ! Minimum X coordinate of mass points in the physical domain
REAL    :: ZXHATM_PHYS_MAX, ZYHATM_PHYS_MAX ! Minimum X coordinate of mass points in the physical domain
REAL, DIMENSION(SIZE(XXHAT)) :: ZXHATM      ! mass point coordinates
REAL, DIMENSION(SIZE(XYHAT)) :: ZYHATM      ! mass point coordinates
REAL, DIMENSION(:), POINTER  :: ZXHAT_GLOB
REAL, DIMENSION(:), POINTER  :: ZYHAT_GLOB
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
! Prepare positioning data
!
IF ( CFILE_STAT /= "NO_INPUT_CSV" .OR. NNUMB_STAT > 0 ) THEN
  IIU = SIZE( XXHAT )
  IJU = SIZE( XYHAT )

  ! Get global XHAT and YHAT (needed by STATPROF_POSITION)
  CALL ALLOCBUFFER_ll( ZXHAT_GLOB, XXHAT, 'XX', GALLOCX )
  CALL ALLOCBUFFER_ll( ZYHAT_GLOB, XYHAT, 'YY', GALLOCY )
  CALL GATHERALL_FIELD_ll( 'XX', XXHAT, ZXHAT_GLOB, IERR )
  CALL GATHERALL_FIELD_ll( 'YY', XYHAT, ZYHAT_GLOB, IERR )

  ! Interpolations of model variables to mass points
  ZXHATM(1:IIU-1) = 0.5 * XXHAT(1:IIU-1) + 0.5 * XXHAT(2:IIU  )
  ZXHATM(  IIU  ) = 1.5 * XXHAT(  IIU  ) - 0.5 * XXHAT(  IIU-1)

  ZYHATM(1:IJU-1) = 0.5 * XYHAT(1:IJU-1) + 0.5 * XYHAT(2:IJU  )
  ZYHATM(  IJU  ) = 1.5 * XYHAT(  IJU  ) - 0.5 * XYHAT(  IJU-1)

  ZXHATM_PHYS_MIN = 0.5 * ( ZXHAT_GLOB(1+JPHEXT) + ZXHAT_GLOB(2+JPHEXT) )
  ZXHATM_PHYS_MAX = 0.5 * ( ZXHAT_GLOB(UBOUND(ZXHAT_GLOB,1)-JPHEXT) + ZXHAT_GLOB(UBOUND(ZXHAT_GLOB,1)-JPHEXT+1) )
  ZYHATM_PHYS_MIN = 0.5 * ( ZYHAT_GLOB(1+JPHEXT) + ZYHAT_GLOB(2+JPHEXT) )
  ZYHATM_PHYS_MAX = 0.5 * ( ZYHAT_GLOB(UBOUND(ZYHAT_GLOB,1)-JPHEXT) + ZYHAT_GLOB(UBOUND(ZYHAT_GLOB,1)-JPHEXT+1) )
END IF
!
! Stations initialization
!
NUMBSTAT_LOC = 0

IF (CFILE_STAT=="NO_INPUT_CSV") THEN
  ! Treat namelist
  INUMBSTAT = 0
  IF ( NNUMB_STAT > 0 ) THEN
    DO JI = 1, NNUMB_STAT
      IF ( LCARTESIAN ) THEN
        TZSTATION%XX = XX_STAT(JI)
        TZSTATION%XY = XY_STAT(JI)
      ELSE
        TZSTATION%XLAT = XLAT_STAT(JI)
        TZSTATION%XLON = XLON_STAT(JI)
        CALL STATPROF_INI_INTERP( TZSTATION )
      END IF
      TZSTATION%XZ    = XZ_STAT(JI)
      TZSTATION%CNAME = CNAME_STAT(JI)

      CALL STATPROF_POSITION( TZSTATION, ZXHAT_GLOB, ZYHAT_GLOB, ZXHATM, ZYHATM,                 &
                             ZXHATM_PHYS_MIN, ZXHATM_PHYS_MAX, ZYHATM_PHYS_MIN, ZYHATM_PHYS_MAX, &
                             GINSIDE, GPRESENT                                                   )

      IF ( GINSIDE ) THEN
        INUMBSTAT = INUMBSTAT + 1
        TZSTATION%NID = INUMBSTAT
      END IF

      IF ( GPRESENT ) CALL STATION_ADD( TZSTATION )
    END DO
  END IF
ELSE
  !Treat CSV datafile
  CALL STATPROF_CSV_READ( TZSTATION, CFILE_STAT, ZXHAT_GLOB, ZYHAT_GLOB, ZXHATM, ZYHATM,     &
                          ZXHATM_PHYS_MIN, ZXHATM_PHYS_MAX,ZYHATM_PHYS_MIN, ZYHATM_PHYS_MAX, &
                          INUMBSTAT                                                          )
END IF

LSTATION = ( INUMBSTAT > 0 )

DO JI = 1, NUMBSTAT_LOC
  CALL STATION_ALLOCATE( TSTATIONS(JI), ISTORE )
END DO
!
! Clean positioning data
!
IF ( CFILE_STAT /= "NO_INPUT_CSV" .OR. NNUMB_STAT > 0 ) THEN
  IF ( GALLOCX ) DEALLOCATE( ZXHAT_GLOB )
  IF ( GALLOCY ) DEALLOCATE( ZYHAT_GLOB )
END IF

!----------------------------------------------------------------------------

END SUBROUTINE INI_SURFSTATION_n
