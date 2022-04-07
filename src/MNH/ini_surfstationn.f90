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
      SUBROUTINE INI_SURFSTATION_n(PTSTEP, PSEGLEN,          &
                                   KRR, KSV, OUSETKE, KMI,   &
                                   PLATOR, PLONOR            )
!
USE MODD_TYPE_DATE
REAL,               INTENT(IN) :: PTSTEP  ! time step
REAL,               INTENT(IN) :: PSEGLEN ! segment length
INTEGER,            INTENT(IN) :: KRR     ! number of moist variables
INTEGER,            INTENT(IN) :: KSV     ! number of scalar variables
LOGICAL,            INTENT(IN) :: OUSETKE ! flag to use tke
REAL,               INTENT(IN) :: PLATOR  ! latitude of origine point
REAL,               INTENT(IN) :: PLONOR  ! longitude of origine point
INTEGER,            INTENT(IN) :: KMI     ! MODEL NUMBER
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE INI_SURFSTATION_n
!
END INTERFACE
!
END MODULE MODI_INI_SURFSTATION_n
!
!     ########################################################
      SUBROUTINE INI_SURFSTATION_n(PTSTEP, PSEGLEN,          &
                                   KRR, KSV, OUSETKE, KMI,   &
                                   PLATOR, PLONOR            )
!     ########################################################
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
!  P. Wautelet  07/04/2022: rewrite types for stations
! --------------------------------------------------------------------------
!
!*      0. DECLARATIONS
!          ------------
!
USE MODD_ALLSTATION_n
USE MODD_CONF
USE MODD_DIM_n
USE MODD_DYN_n
USE MODD_GRID
USE MODD_GRID_n
USE MODD_NESTING
USE MODD_PARAMETERS
USE MODD_SHADOWS_n
USE MODD_STATION_n
USE MODD_TYPE_DATE
USE MODD_VAR_ll,          ONLY: IP
!
USE MODE_GATHER_ll
USE MODE_GRIDPROJ
USE MODE_ll
USE MODE_MSG
!
USE MODI_INI_STATION_N
!
IMPLICIT NONE
!
!
!*      0.1  declarations of arguments
!
!
REAL,               INTENT(IN) :: PTSTEP  ! time step
REAL,               INTENT(IN) :: PSEGLEN ! segment length
INTEGER,            INTENT(IN) :: KRR     ! number of moist variables
INTEGER,            INTENT(IN) :: KSV     ! number of scalar variables
LOGICAL,            INTENT(IN) :: OUSETKE ! flag to use tke
REAL,               INTENT(IN) :: PLATOR  ! latitude of origine point
REAL,               INTENT(IN) :: PLONOR  ! longitude of origine point
INTEGER,            INTENT(IN) :: KMI     ! MODEL NUMBER
!
!-------------------------------------------------------------------------------
!
!       0.2  declaration of local variables
!
INTEGER :: ISTORE ! number of storage instants
INTEGER :: IIU_ll,IJU_ll,IRESP
!
!----------------------------------------------------------------------------
!
!*      1.   Default values
!            --------------
!
CALL DEFAULT_STATION_n()
!
!
!*      3.   Stations initialization
!            -----------------------
!
CALL INI_STATION_n()
LSTATION = (NUMBSTAT>0)
!
!----------------------------------------------------------------------------
!
!*      4.   Allocations of storage arrays
!            -----------------------------
!
IF(NUMBSTAT>0) THEN
  CALL ALLOCATE_STATION_n()
  IF (.NOT. LCARTESIAN) CALL INI_INTERP_STATION_n()
ENDIF
!----------------------------------------------------------------------------
!
CONTAINS
!
!----------------------------------------------------------------------------
SUBROUTINE DEFAULT_STATION_n()

USE MODD_DYN_N, ONLY: XTSTEP
!
NUMBSTAT   = 0
TSTATIONS_TIME%XTSTEP = XTSTEP
!
END SUBROUTINE DEFAULT_STATION_n
!----------------------------------------------------------------------------
!----------------------------------------------------------------------------
SUBROUTINE ALLOCATE_STATION_n()

INTEGER :: JI

if ( tstations_time%xtstep < xtstep ) then
  call Print_msg( NVERB_WARNING, 'GEN', 'INI_SURFSTATION_n', 'Timestep for stations was smaller than model timestep' )
  tstations_time%xtstep = xtstep
end if

ISTORE = NINT ( ( PSEGLEN - DYN_MODEL(1)%XTSTEP ) / TSTATIONS_TIME%XTSTEP ) + 1

allocate( tstations_time%tpdates(istore) )

DO JI = 1, NUMBSTAT
  ALLOCATE(TSTATIONS(JI)%XZON   (ISTORE))
  ALLOCATE(TSTATIONS(JI)%XMER   (ISTORE))
  ALLOCATE(TSTATIONS(JI)%XW     (ISTORE))
  ALLOCATE(TSTATIONS(JI)%XP     (ISTORE))
  IF (OUSETKE) THEN
    ALLOCATE(TSTATIONS(JI)%XTKE (ISTORE))
  ELSE
    ALLOCATE(TSTATIONS(JI)%XTKE (0))
  END IF
  ALLOCATE(TSTATIONS(JI)%XTH    (ISTORE))
  ALLOCATE(TSTATIONS(JI)%XR     (ISTORE,KRR))
  ALLOCATE(TSTATIONS(JI)%XSV    (ISTORE,KSV))
  ALLOCATE(TSTATIONS(JI)%XTSRAD (ISTORE))
  ALLOCATE(TSTATIONS(JI)%XT2M   (ISTORE))
  ALLOCATE(TSTATIONS(JI)%XQ2M   (ISTORE))
  ALLOCATE(TSTATIONS(JI)%XHU2M  (ISTORE))
  ALLOCATE(TSTATIONS(JI)%XZON10M(ISTORE))
  ALLOCATE(TSTATIONS(JI)%XMER10M(ISTORE))
  ALLOCATE(TSTATIONS(JI)%XRN    (ISTORE))
  ALLOCATE(TSTATIONS(JI)%XH     (ISTORE))
  ALLOCATE(TSTATIONS(JI)%XLE    (ISTORE))
  ALLOCATE(TSTATIONS(JI)%XLEI   (ISTORE))
  ALLOCATE(TSTATIONS(JI)%XGFLUX (ISTORE))
  ALLOCATE(TSTATIONS(JI)%XSWD   (ISTORE))
  ALLOCATE(TSTATIONS(JI)%XSWU   (ISTORE))
  ALLOCATE(TSTATIONS(JI)%XLWD   (ISTORE))
  ALLOCATE(TSTATIONS(JI)%XLWU   (ISTORE))
  ALLOCATE(TSTATIONS(JI)%XSWDIR (ISTORE))
  ALLOCATE(TSTATIONS(JI)%XSWDIFF(ISTORE))
  ALLOCATE(TSTATIONS(JI)%XDSTAOD(ISTORE))
  ALLOCATE(TSTATIONS(JI)%XSFCO2 (ISTORE))

  TSTATIONS(JI)%XZON(:)    = XUNDEF
  TSTATIONS(JI)%XMER(:)    = XUNDEF
  TSTATIONS(JI)%XW(:)      = XUNDEF
  TSTATIONS(JI)%XP(:)      = XUNDEF
  TSTATIONS(JI)%XTKE(:)    = XUNDEF
  TSTATIONS(JI)%XTH(:)     = XUNDEF
  TSTATIONS(JI)%XR(:,:)    = XUNDEF
  TSTATIONS(JI)%XSV(:,:)   = XUNDEF
  TSTATIONS(JI)%XTSRAD(:)  = XUNDEF
  TSTATIONS(JI)%XT2M(:)    = XUNDEF
  TSTATIONS(JI)%XQ2M(:)    = XUNDEF
  TSTATIONS(JI)%XHU2M(:)   = XUNDEF
  TSTATIONS(JI)%XZON10M(:) = XUNDEF
  TSTATIONS(JI)%XMER10M(:) = XUNDEF
  TSTATIONS(JI)%XRN(:)     = XUNDEF
  TSTATIONS(JI)%XH(:)      = XUNDEF
  TSTATIONS(JI)%XLE(:)     = XUNDEF
  TSTATIONS(JI)%XLEI(:)    = XUNDEF
  TSTATIONS(JI)%XGFLUX(:)  = XUNDEF
  TSTATIONS(JI)%XSWD(:)    = XUNDEF
  TSTATIONS(JI)%XSWU(:)    = XUNDEF
  TSTATIONS(JI)%XLWD(:)    = XUNDEF
  TSTATIONS(JI)%XLWU(:)    = XUNDEF
  TSTATIONS(JI)%XSWDIR(:)  = XUNDEF
  TSTATIONS(JI)%XSWDIFF(:) = XUNDEF
  TSTATIONS(JI)%XDSTAOD(:) = XUNDEF
  TSTATIONS(JI)%XSFCO2(:)  = XUNDEF
END DO

END SUBROUTINE ALLOCATE_STATION_n
!----------------------------------------------------------------------------
!----------------------------------------------------------------------------
SUBROUTINE INI_INTERP_STATION_n()
!
INTEGER :: JII
INTEGER :: IIU, IJU
!
IF ( ALL(TSTATIONS(:)%XLAT /= XUNDEF) .AND. ALL(TSTATIONS(:)%XLON /= XUNDEF) ) THEN
 DO JII = 1, NUMBSTAT
   CALL GET_DIM_EXT_ll ('B',IIU,IJU)
   CALL SM_XYHAT(PLATOR,PLONOR,                        &
                 TSTATIONS(JII)%XLAT, TSTATIONS(JII)%XLON, &
                 TSTATIONS(JII)%XX,   TSTATIONS(JII)%XY    )
 END DO
ELSE
  CMNHMSG(1) = 'Error in station position'
  CMNHMSG(1) = 'either LATitude or LONgitude segment'
  CMNHMSG(1) = 'or I and J segment'
  CMNHMSG(1) = 'definition is not complete.'
  CALL PRINT_MSG( NVERB_FATAL, 'GEN', 'INI_SURFSTATION_n' )
END IF

END SUBROUTINE INI_INTERP_STATION_n
!----------------------------------------------------------------------------
!----------------------------------------------------------------------------
!
END SUBROUTINE INI_SURFSTATION_n
