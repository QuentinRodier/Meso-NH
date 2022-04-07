!MNH_LIC Copyright 2002-2022 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!     #######################
      SUBROUTINE INI_STATION_n
!     #######################
!
!
!!****  *INI_STATION_n* - user initializes the station location
!!
!!    PURPOSE
!!    -------
!
!
!!**  METHOD
!!    ------
!!    
!!   Must be defined (for each station):
!!   ---------------
!!
!!  No default exist for these variables.
!!  ************************************
!!
!!  1) Number of stations
!!  2) the model in which these stations are
!!     if NOT initialized, the stations are NOT used.
!!
!!  3) the (LAT, LON, ALT) latitude,longitude and altitude of the station location.
!!  4) the station name
!!
!!
!!
!!   Can be defined  (for each station):
!!   --------------
!!
!!
!!  9) the time step for data storage.
!!    default is 60s
!!
!! 10) the name or title describing the balloon (8 characters)
!!     default is the balloon type (6 characters) + the balloon numbers (2 characters)
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
!!      Pierre Tulet             * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!     Original 15/01/2002
!  E. Jezequel     02/2021: read stations from CVS file
!  P. Wautelet  07/04/2022: rewrite types for stations
! --------------------------------------------------------------------------
!       
!*      0. DECLARATIONS
!          ------------
!
USE MODD_ALLSTATION_n
USE MODD_CONF,         ONLY: LCARTESIAN
USE MODD_PARAMETERS
!
USE MODI_STATION_READER
!
!
IMPLICIT NONE
!
!
!*      0.1  declarations of arguments
!
!
!-------------------------------------------------------------------------------
!
!       0.2  declaration of local variables
!
INTEGER :: JI
!
!----------------------------------------------------------------------------
IF (CFILE_STAT=="NO_INPUT_CSV") THEN
!
!*      1.   Namelist
!            --------
  NUMBSTAT             = NNUMB_STAT

  IF (NUMBSTAT > 0) THEN
    ALLOCATE( TSTATIONS(NUMBSTAT) )

    IF (LCARTESIAN) THEN
      DO JI=1,NUMBSTAT
        TSTATIONS(JI)%XX = XX_STAT(JI)
        TSTATIONS(JI)%XY = XY_STAT(JI)
        TSTATIONS(JI)%XZ = XZ_STAT(JI)
        TSTATIONS(JI)%CNAME = CNAME_STAT(JI)
        TSTATIONS(JI)%CTYPE = CTYPE_STAT(JI)
      END DO
    ELSE
      DO JI=1,NUMBSTAT
        TSTATIONS(JI)%XLAT = XLAT_STAT(JI)
        TSTATIONS(JI)%XLON = XLON_STAT(JI)
        TSTATIONS(JI)%XZ   = XZ_STAT(JI)
        TSTATIONS(JI)%CNAME = CNAME_STAT(JI)
        TSTATIONS(JI)%CTYPE = CTYPE_STAT(JI)
      END DO
    END IF
  END IF
ELSE
!
!*      2.   CSV DATA 
!
  CALL READ_CSV_STATION( CFILE_STAT, TSTATIONS, LCARTESIAN )
END IF

TSTATIONS_TIME%XTSTEP = XSTEP_STAT

END SUBROUTINE INI_STATION_n
