!MNH_LIC Copyright 2002-2022 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!      ##########################
MODULE MODI_STATION_n
!      ##########################
!
INTERFACE
!
      SUBROUTINE STATION_n( PTSTEP, PZ,                     &
                            PU, PV, PW, PTH, PR, PSV, PTKE, &
                            PTS, PP )
!
REAL,                     INTENT(IN)     :: PTSTEP ! time step
REAL, DIMENSION(:,:,:),   INTENT(IN)     :: PZ     ! z array
REAL, DIMENSION(:,:,:),   INTENT(IN)     :: PU     ! horizontal wind X component
REAL, DIMENSION(:,:,:),   INTENT(IN)     :: PV     ! horizontal wind Y component
REAL, DIMENSION(:,:,:),   INTENT(IN)     :: PW     ! vertical wind
REAL, DIMENSION(:,:,:),   INTENT(IN)     :: PTH    ! potential temperature
REAL, DIMENSION(:,:,:,:), INTENT(IN)     :: PR     ! water mixing ratios
REAL, DIMENSION(:,:,:,:), INTENT(IN)     :: PSV    ! Scalar variables
REAL, DIMENSION(:,:,:),   INTENT(IN)     :: PTKE   ! turbulent kinetic energy
REAL, DIMENSION(:,:),     INTENT(IN)     :: PTS    ! surface temperature
REAL, DIMENSION(:,:,:),   INTENT(IN)     :: PP     ! pressure
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE STATION_n
!
END INTERFACE
!
END MODULE MODI_STATION_n
!
!     #######################################################
      SUBROUTINE STATION_n( PTSTEP, PZ,                     &
                            PU, PV, PW, PTH, PR, PSV, PTKE, &
                            PTS, PP )
!     #######################################################
!
!
!!****  *STATION_n* - (advects and) stores 
!!                                stations/s in the model
!!
!!    PURPOSE
!!    -------
!
!
!!**  METHOD
!!    ------
!!    
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
!!      Pierre TULET / Valery Masson             * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!     Original 15/02/2002
!  A. Lemonsu   19/11/2002
!  P. Aumond    01/07/2011: add model levels
!  C. Lac          04/2013: correction on the vertical levels
!  C. Lac          04/2013: add I/JK positioning
!  P. Wautelet  28/03/2018: replace TEMPORAL_DIST by DATETIME_DISTANCE
!  P. Wautelet  05/2016-04/2018: new data structures and calls for I/O
!  P. Wautelet  13/09/2019: budget: simplify and modernize date/time management
!  R. Schoetter    11/2019: use LCARTESIAN instead of LSTATLAT for multiproc in cartesian
!  P. Wautelet     04/2022: restructure stations for better performance, reduce memory usage and correct some problems/bugs
!
! --------------------------------------------------------------------------
!
!*      0. DECLARATIONS
!          ------------
!
USE MODD_CONF,          ONLY: LCARTESIAN
USE MODD_CST,           ONLY: XPI
USE MODD_DIAG_IN_RUN
USE MODD_GRID,          ONLY: XBETA, XLON0, XRPK
USE MODD_PARAMETERS,    ONLY: JPVEXT, XUNDEF
USE MODD_PARAM_n,       ONLY: CRAD
USE MODD_STATION_n
USE MODD_ALLSTATION_n,  ONLY: LDIAG_SURFRAD
USE MODD_TIME_n,        ONLY: tdtcur
!
USE MODE_STATPROF_TOOLS, ONLY: STATPROF_INTERP_2D, STATPROF_INTERP_2D_U, STATPROF_INTERP_2D_V
!
!
IMPLICIT NONE
!
!
!*      0.1  declarations of arguments
!
!
REAL,                     INTENT(IN)     :: PTSTEP ! time step
REAL, DIMENSION(:,:,:),   INTENT(IN)     :: PZ     ! z array
REAL, DIMENSION(:,:,:),   INTENT(IN)     :: PU     ! horizontal wind X component
REAL, DIMENSION(:,:,:),   INTENT(IN)     :: PV     ! horizontal wind Y component
REAL, DIMENSION(:,:,:),   INTENT(IN)     :: PW     ! vertical wind
REAL, DIMENSION(:,:,:),   INTENT(IN)     :: PTH    ! potential temperature
REAL, DIMENSION(:,:,:,:), INTENT(IN)     :: PR     ! water mixing ratios
REAL, DIMENSION(:,:,:,:), INTENT(IN)     :: PSV    ! Scalar variables
REAL, DIMENSION(:,:,:),   INTENT(IN)     :: PTKE   ! turbulent kinetic energy
REAL, DIMENSION(:,:),     INTENT(IN)     :: PTS    ! surface temperature
REAL, DIMENSION(:,:,:),   INTENT(IN)     :: PP     ! pressure
!
!-------------------------------------------------------------------------------
!
!       0.2  declaration of local variables
!
!
INTEGER :: IN       ! time index
INTEGER :: JSV      ! loop counter
!
REAL    :: ZU_STAT     ! horizontal wind speed at station location (along x)
REAL    :: ZV_STAT     ! horizontal wind speed at station location (along y)
REAL    :: ZGAM        ! rotation between meso-nh base and spherical lat-lon base.
!
INTEGER :: JS          ! loop for stations
INTEGER :: JK          ! loop for levels
!
!----------------------------------------------------------------------------
!
!*      3.4  instant of storage
!            ------------------
!
IF ( TSTATIONS_TIME%XTIME_CUR == XUNDEF ) TSTATIONS_TIME%XTIME_CUR = TSTATIONS_TIME%XTSTEP - PTSTEP
!
TSTATIONS_TIME%XTIME_CUR = TSTATIONS_TIME%XTIME_CUR + PTSTEP
!
IF ( TSTATIONS_TIME%XTIME_CUR >= TSTATIONS_TIME%XTSTEP - 1.E-10 ) THEN
  TSTATIONS_TIME%XTIME_CUR = TSTATIONS_TIME%XTIME_CUR - TSTATIONS_TIME%XTSTEP
  TSTATIONS_TIME%N_CUR = TSTATIONS_TIME%N_CUR + 1
  IN = TSTATIONS_TIME%N_CUR
  tstations_time%tpdates(in) = tdtcur
ELSE
  !No station storage at this time step
  RETURN
END IF
!
!----------------------------------------------------------------------------
!
!*      8.   DATA RECORDING
!            --------------
!
STATION: DO JS = 1,NUMBSTAT_LOC
  JK = TSTATIONS(JS)%NK

  IF (LCARTESIAN) THEN
    TSTATIONS(JS)%XZON(IN) =   STATPROF_INTERP_2D_U( TSTATIONS(JS), PU(:,:,JK) )
    TSTATIONS(JS)%XMER(IN) =   STATPROF_INTERP_2D_V( TSTATIONS(JS), PV(:,:,JK) )
  ELSE
    ZU_STAT                = STATPROF_INTERP_2D_U( TSTATIONS(JS), PU(:,:,JK) )
    ZV_STAT                = STATPROF_INTERP_2D_V( TSTATIONS(JS), PV(:,:,JK) )
    ZGAM                   = (XRPK * (TSTATIONS(JS)%XLON - XLON0) - XBETA)*(XPI/180.)
    TSTATIONS(JS)%XZON(IN) =   ZU_STAT * COS(ZGAM) + ZV_STAT * SIN(ZGAM)
    TSTATIONS(JS)%XMER(IN) = - ZU_STAT * SIN(ZGAM) + ZV_STAT * COS(ZGAM)
  END IF
  TSTATIONS(JS)%XW (IN) = STATPROF_INTERP_2D( TSTATIONS(JS), PW(:,:,JK) )
  TSTATIONS(JS)%XTH(IN) = STATPROF_INTERP_2D( TSTATIONS(JS), PTH(:,:,JK) )
  TSTATIONS(JS)%XP (IN) = STATPROF_INTERP_2D( TSTATIONS(JS), PP(:,:,JK) )

  DO JSV=1,SIZE(PR,4)
    TSTATIONS(JS)%XR(IN,JSV) = STATPROF_INTERP_2D( TSTATIONS(JS), PR(:,:,JK,JSV) )
  END DO

  DO JSV=1,SIZE(PSV,4)
    TSTATIONS(JS)%XSV(IN,JSV) = STATPROF_INTERP_2D( TSTATIONS(JS), PSV(:,:,JK,JSV) )
  END DO

  IF (SIZE(PTKE)>0) TSTATIONS(JS)%XTKE(IN) = STATPROF_INTERP_2D( TSTATIONS(JS), PTKE(:,:,JK) )
  IF ( CRAD /= 'NONE' ) TSTATIONS(JS)%XTSRAD(IN) = STATPROF_INTERP_2D( TSTATIONS(JS), PTS )
  TSTATIONS(JS)%XZS  = STATPROF_INTERP_2D( TSTATIONS(JS), PZ(:,:,1+JPVEXT))

  IF ( LDIAG_SURFRAD ) THEN
    TSTATIONS(JS)%XZON10M(IN) = STATPROF_INTERP_2D( TSTATIONS(JS), XCURRENT_ZON10M )
    TSTATIONS(JS)%XMER10M(IN) = STATPROF_INTERP_2D( TSTATIONS(JS), XCURRENT_MER10M )
    TSTATIONS(JS)%XT2M   (IN) = STATPROF_INTERP_2D( TSTATIONS(JS), XCURRENT_T2M    )
    TSTATIONS(JS)%XQ2M   (IN) = STATPROF_INTERP_2D( TSTATIONS(JS), XCURRENT_Q2M    )
    TSTATIONS(JS)%XHU2M  (IN) = STATPROF_INTERP_2D( TSTATIONS(JS), XCURRENT_HU2M   )
    TSTATIONS(JS)%XRN    (IN) = STATPROF_INTERP_2D( TSTATIONS(JS), XCURRENT_RN     )
    TSTATIONS(JS)%XH     (IN) = STATPROF_INTERP_2D( TSTATIONS(JS), XCURRENT_H      )
    TSTATIONS(JS)%XLE    (IN) = STATPROF_INTERP_2D( TSTATIONS(JS), XCURRENT_LE     )
    TSTATIONS(JS)%XLEI   (IN) = STATPROF_INTERP_2D( TSTATIONS(JS), XCURRENT_LEI    )
    TSTATIONS(JS)%XGFLUX (IN) = STATPROF_INTERP_2D( TSTATIONS(JS), XCURRENT_GFLUX  )
    IF ( CRAD /= 'NONE' ) THEN
      TSTATIONS(JS)%XSWD   (IN) = STATPROF_INTERP_2D( TSTATIONS(JS), XCURRENT_SWD    )
      TSTATIONS(JS)%XSWU   (IN) = STATPROF_INTERP_2D( TSTATIONS(JS), XCURRENT_SWU    )
      TSTATIONS(JS)%XLWD   (IN) = STATPROF_INTERP_2D( TSTATIONS(JS), XCURRENT_LWD    )
      TSTATIONS(JS)%XLWU   (IN) = STATPROF_INTERP_2D( TSTATIONS(JS), XCURRENT_LWU    )
      TSTATIONS(JS)%XSWDIR (IN) = STATPROF_INTERP_2D( TSTATIONS(JS), XCURRENT_SWDIR  )
      TSTATIONS(JS)%XSWDIFF(IN) = STATPROF_INTERP_2D( TSTATIONS(JS), XCURRENT_SWDIFF )
      TSTATIONS(JS)%XDSTAOD(IN) = STATPROF_INTERP_2D( TSTATIONS(JS), XCURRENT_DSTAOD )
    END IF
    TSTATIONS(JS)%XSFCO2(IN) = STATPROF_INTERP_2D( TSTATIONS(JS), XCURRENT_SFCO2 )
  END IF
END DO STATION
!
!----------------------------------------------------------------------------
!
END SUBROUTINE STATION_n
