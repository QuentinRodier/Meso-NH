!MNH_LIC Copyright 2002-2021 CNRS, Meteo-France and Universite Paul Sabatier
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
!!     Modification: 02/2021 (E.Jezequel) Read stations from CVS file
!!
!! --------------------------------------------------------------------------
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
!
!*      1.   Nameliste 
!            ---------

IF (CFILE_STAT=="NO_INPUT_CSV") THEN
  NUMBSTAT             = NNUMB_STAT

  IF (NUMBSTAT > 0) THEN
    ALLOCATE  (TSTATION%LAT(NUMBSTAT))
    ALLOCATE  (TSTATION%LON(NUMBSTAT))
    ALLOCATE  (TSTATION%X(NUMBSTAT))
    ALLOCATE  (TSTATION%Y(NUMBSTAT))
    ALLOCATE  (TSTATION%Z(NUMBSTAT))
    ALLOCATE  (TSTATION%K(NUMBSTAT))
    ALLOCATE  (TSTATION%NAME(NUMBSTAT))
    ALLOCATE  (TSTATION%TYPE(NUMBSTAT))
    !
    TSTATION%LON  = XUNDEF
    TSTATION%LAT  = XUNDEF
    TSTATION%Z    = XUNDEF
    TSTATION%K    = XUNDEF
    TSTATION%X    = XUNDEF
    TSTATION%Y    = XUNDEF
    TSTATION%NAME = "        "
    TSTATION%TYPE = "        "
    !
    TSTATION%STEP = XSTEP_STAT
    !
    IF (LCARTESIAN) THEN
      DO JI=1,NUMBSTAT
        TSTATION%X(JI)= XX_STAT(JI)
        TSTATION%Y(JI)= XY_STAT(JI)
        TSTATION%Z(JI)= XZ_STAT(JI)
        TSTATION%NAME(JI)= CNAME_STAT(JI)
        TSTATION%TYPE(JI)= CTYPE_STAT(JI)
      END DO
    ELSE
      DO JI=1,NUMBSTAT
        TSTATION%LAT(JI)= XLAT_STAT(JI)
        TSTATION%LON(JI)= XLON_STAT(JI)
        TSTATION%Z(JI)= XZ_STAT(JI)
        TSTATION%NAME(JI)= CNAME_STAT(JI)
        TSTATION%TYPE(JI)= CTYPE_STAT(JI)
      END DO
    ENDIF
  ENDIF
ELSE
!
!*      2.   CSV DATA 
!
  CALL READ_CSV_STATION(CFILE_STAT,TSTATION,LCARTESIAN)
  TSTATION%STEP = XSTEP_STAT
END IF 

!
END SUBROUTINE INI_STATION_n
