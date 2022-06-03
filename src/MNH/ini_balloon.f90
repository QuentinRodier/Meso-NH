!MNH_LIC Copyright 2000-2022 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!     ######################
      SUBROUTINE INI_BALLOON
!     ######################
!
!
!!****  *INI_BALLOON* - user initializes the balloon characteristics
!!
!!    PURPOSE
!!    -------
!
!
!!**  METHOD
!!    ------
!!    
!!    For constant volume Balloon, horizontal advection using horizontal wind
!!        vertical spped of the balloon calculated using the balloon equation
!!        (Koffi et AL 2000, JAS vol 57 P.2007-2021)
!!
!!   Must be defined (for each balloon):
!!   ---------------
!!
!!  No default exist for these variables.
!!  ************************************
!!
!!  1) the model in which the balloon will evolve
!!     if NOT initialized, the balloon is NOT used.
!!  1.1) the possibility to switch from a model to its dad or kid
!!       'FIX' : NMODEL used during the run
!!       'MOB' : best resolution model used. NMODEL=1 is used at the beginning
!!
!!  2) the type of balloon
!!
!!     'RADIOS' for radiosounding balloon
!!     'ISODEN' for iso-density balloon
!!     'CVBALL' for constant volume Balloon
!!
!!  3) the launching date and time
!!
!!  4) the latitude of the launching site
!!
!!  5) the longitude of the launching site
!!
!!  6) the altitude of the launching site (for 'RADIOS')
!!
!!                      OR
!!
!!     the altitude OR pressure of balloon at start of the leveled flight
!!     (for 'ISODEN'). In this case, the density of this level will be computed,
!!     and the balloon will evolve at this density level.
!!
!!
!!
!!   Can be defined  (for each balloon):
!!   --------------
!!
!!  7) the ascentional vertical speed of the ballon (in calm air) (for 'RADIOS')
!!     default is 5m/s
!!
!!  8) the time step for data storage.
!!    default is 60s
!!
!!  9) the name or title describing the balloon (8 characters)
!!     default is the balloon type (6 characters) + the balloon numbers (2 characters)
!!
!!  10) for 'CVBALL' the aerodynamic drag coefficient of the balloon
!!
!!  11) for 'CVBALL' the induced drag coefficient (i.e. air shifted by the balloon)
!!
!!  12) for 'CVBALL' the volume of the balloon
!!
!!  13) for 'CVBALL' the mass of the balloon
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
!!     Original 15/05/2000
!!              Apr,19, 2001 (G.Jaubert) add CVBALL type and switch in models
!  P. Wautelet    06/2022: reorganize flyers
!! --------------------------------------------------------------------------
!       
!*      0. DECLARATIONS
!          ------------
!
USE MODD_AIRCRAFT_BALLOON
USE MODD_CST

USE MODE_MSG

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
!----------------------------------------------------------------------------
NBALLOONS = 0

ALLOCATE( TBALLOONS(NBALLOONS) )

IF ( NBALLOONS < 1 ) RETURN
!
!*      1.   Balloon number 1
!            ----------------
#if 0
!
!* model number
!
TBALLOONS(1)%NMODEL             = 0
TBALLOONS(1)%CMODEL             = 'MOB'
!
!* balloon type
!
TBALLOONS(1)%CTYPE               = 'CVBALL'
!
!* balloon name
!
TBALLOONS(1)%CTITLE              = 'CVB1MOBI'
!
!* launching date and time
!
TBALLOONS(1)%TLAUNCH%nyear  =  1999
TBALLOONS(1)%TLAUNCH%nmonth =    09
TBALLOONS(1)%TLAUNCH%nday   =    19
TBALLOONS(1)%TLAUNCH%xtime  = 32460.
!
!* latitude and longitude of launching site (decimal degree)
!
TBALLOONS(1)%XLATLAUNCH                = 45.800
TBALLOONS(1)%XLONLAUNCH                =  8.629
!
!* altitude of the launching site for 'RADIOS'
!* altitude or pressure of the flight level for 'ISODEN'
!
!TBALLOONS(1)%XALTLAUNCH                =   3959.
TBALLOONS(1)%XPRES               = 98450.
!
!* time step for data storage  (s)
!
TBALLOONS(1)%TFLYER_TIME%XTSTEP = 20.
!
!* ascentional vertical speed of the ballon (in calm air) (for 'RADIOS')
!
TBALLOONS(1)%XWASCENT            = 0.
!
!* aerodynamic drag coefficient of the balloon (for 'CVBALL')
!* induced drag coefficient (i.e. air shifted by the balloon) (for 'CVBALL')
!* volume of the balloon (m3) (for 'CVBALL')
!* mass of the balloon (kg) (for 'CVBALL')
!
TBALLOONS(1)%XAERODRAG           = 0.44
TBALLOONS(1)%XINDDRAG           = 0.014
TBALLOONS(1)%XVOLUME           = 3.040
TBALLOONS(1)%XMASS           = 2.4516
TBALLOONS(1)%XDIAMETER           = ((3.*TBALLOONS(1)%XVOLUME)/(4.*XPI))**(1./3.)
!
IF ( NBALLOONS < 2 ) RETURN
#else
CALL PRINT_MSG( NVERB_FATAL, 'GEN', 'INI_BALLOON', 'balloon characteristics are commented' )
#endif
!----------------------------------------------------------------------------
!
!*      2.   Balloon number 2
!            ----------------
#if 0
!
!* model number
!
TBALLOONS(2)%NMODEL             = 0
TBALLOONS(2)%MODEL             = 'MOB'
!
!* balloon type
!
TBALLOONS(2)%CTYPE               = 'CVBALL'
!
!* balloon name
!
TBALLOONS(2)%CTITLE              = 'CVB2MOBI'
!
!* launching date and time
!
TBALLOONS(2)%TLAUNCH%nyear  =  1999
TBALLOONS(2)%TLAUNCH%nmonth =    09
TBALLOONS(2)%TLAUNCH%nday   =    19
TBALLOONS(2)%TLAUNCH%xtime  = 39660.
!
!* latitude and longitude of launching site (decimal degree)
!
TBALLOONS(2)%XLATLAUNCH                = 45.800
TBALLOONS(2)%XLONLAUNCH                =  8.630
!
!* altitude of the launching site for 'RADIOS'
!* altitude or pressure of the flight level for 'ISODEN'
!
!TBALLOONS(2)%XALTLAUNCH                =   3959.
TBALLOONS(2)%XPRES               = 98490.
!
!* time step for data storage  (s)
!
TBALLOONS(2)%TFLYER_TIME%XTSTEP = 20.
!
!* ascentional vertical speed of the ballon (in calm air) (for 'RADIOS')
!
TBALLOONS(2)%XWASCENT            = 0.
!
!* aerodynamic drag coefficient of the balloon (for 'CVBALL')
!* induced drag coefficient (i.e. air shifted by the balloon) (for 'CVBALL')
!* volume of the balloon (m3) (for 'CVBALL')
!* mass of the balloon (kg) (for 'CVBALL')
!
TBALLOONS(2)%XAERODRAG           = 0.44
TBALLOONS(2)%XINDDRAG           = 0.014
TBALLOONS(2)%XVOLUME           = 3.040
TBALLOONS(2)%XMASS           = 2.58087
TBALLOONS(2)%XDIAMETER           = ((3.*TBALLOONS(2)%XVOLUME)/(4.*XPI))**(1./3.)
!
IF ( NBALLOONS < 3 ) RETURN
#else
CALL PRINT_MSG( NVERB_FATAL, 'GEN', 'INI_BALLOON', 'balloon characteristics are commented' )
#endif
!----------------------------------------------------------------------------
!
!*      3.   Balloon number 3
!            ----------------
#if 0
!
!* model number
!
TBALLOONS(3)%NMODEL             = 0
TBALLOONS(3)%CMODEL             = 'MOB'
!
!* balloon type
!
TBALLOONS(3)%CTYPE               = 'RADIOS'
!
!* balloon name
!
TBALLOONS(3)%CTITLE              = 'RSMASE19'
!
!* launching date and time
!
TBALLOONS(3)%TLAUNCH%nyear  =  1999
TBALLOONS(3)%TLAUNCH%nmonth =    09
TBALLOONS(3)%TLAUNCH%nday   =    19
TBALLOONS(3)%TLAUNCH%xtime  = 68400.
!
!* latitude and longitude of launching site (decimal degree)
!
TBALLOONS(3)%XLATLAUNCH                = 46.810
TBALLOONS(3)%XLONLAUNCH                =  9.39
!
!* altitude of the launching site for 'RADIOS'
!* altitude or pressure of the flight level for 'ISODEN'
!
TBALLOONS(3)%XALTLAUNCH                =   865.
!TBALLOONS(3)%XPRES               = 62360.
!
!* time step for data storage  (s)
!
TBALLOONS(3)%TFLYER_TIME%XTSTEP = 20.
!
!* ascentional vertical speed of the ballon (in calm air) (for 'RADIOS')
!
TBALLOONS(3)%XWASCENT            = 4.85
!
!* aerodynamic drag coefficient of the balloon (for 'CVBALL')
!* induced drag coefficient (i.e. air shifted by the balloon) (for 'CVBALL')
!* volume of the balloon (m3) (for 'CVBALL')
!* mass of the balloon (kg) (for 'CVBALL')
!
TBALLOONS(3)%XAERODRAG           = 0.44
TBALLOONS(3)%XINDDRAG           = 0.014
TBALLOONS(3)%XVOLUME           = 3.040
TBALLOONS(3)%XMASS           = 2.4516
TBALLOONS(3)%XDIAMETER           = ((3.*TBALLOONS(3)%XVOLUME)/(4.*XPI))**(1./3.)
!
IF ( NBALLOONS < 4 ) RETURN
#else
CALL PRINT_MSG( NVERB_FATAL, 'GEN', 'INI_BALLOON', 'balloon characteristics are commented' )
#endif
!----------------------------------------------------------------------------
!
!*      4.   Balloon number 4
!            ----------------
#if 0
!
!* model number
!
TBALLOONS(4)%NMODEL             = 0
TBALLOONS(4)%CMODEL             = 'FIX'
!
!* balloon type
!
TBALLOONS(4)%CTYPE               = 'CVBALL'
!
!* balloon name
!
TBALLOONS(4)%CTITLE              = 'CVB1ACVB'
!
!* launching date and time
!
TBALLOONS(4)%TLAUNCH%nyear  =  1999
TBALLOONS(4)%TLAUNCH%nmonth =    09
TBALLOONS(4)%TLAUNCH%nday   =    19
TBALLOONS(4)%TLAUNCH%xtime  = 32460.
!
!* latitude and longitude of launching site (decimal degree)
!
TBALLOONS(4)%XLATLAUNCH                = 45.922
TBALLOONS(4)%XLONLAUNCH                =  8.646
!
!* altitude of the launching site for 'RADIOS'
!* altitude or pressure of the flight level for 'ISODEN'
!
TBALLOONS(4)%XALTLAUNCH                =   3959.
!TBALLOONS(4)%XPRES               = 62360.
!
!* time step for data storage  (s)
!
TBALLOONS(4)%TFLYER_TIME%XTSTEP = 20.
!
!* ascentional vertical speed of the ballon (in calm air) (for 'RADIOS')
!
!TBALLOONS(4)%XWASCENT            = 2.55
!
!* aerodynamic drag coefficient of the balloon (for 'CVBALL')
!* induced drag coefficient (i.e. air shifted by the balloon) (for 'CVBALL')
!* volume of the balloon (m3) (for 'CVBALL')
!* mass of the balloon (kg) (for 'CVBALL')
!
TBALLOONS(4)%XAERODRAG           = 0.44
TBALLOONS(4)%XINDDRAG           = 0.014
TBALLOONS(4)%XVOLUME           = 3.040
TBALLOONS(4)%XMASS           = 2.4516
TBALLOONS(4)%XDIAMETER           = ((3.*TBALLOONS(4)%XVOLUME)/(4.*XPI))**(1./3.)
!
IF ( NBALLOONS < 5 ) RETURN
#else
CALL PRINT_MSG( NVERB_FATAL, 'GEN', 'INI_BALLOON', 'balloon characteristics are commented' )
#endif
!----------------------------------------------------------------------------
!
!*      5.   Balloon number 5
!            ----------------
#if 0
!
!* model number
!
TBALLOONS(5)%NMODEL             = 0
TBALLOONS(5)%CMODEL             = 'FIX'
!
!* balloon type
!
TBALLOONS(5)%CTYPE               = 'CVBALL'
!
!* balloon name
!
TBALLOONS(5)%CTITLE              = 'CVB1DEPA'
!
!* launching date and time
!
TBALLOONS(5)%TLAUNCH%nyear  =  1999
TBALLOONS(5)%TLAUNCH%nmonth =    09
TBALLOONS(5)%TLAUNCH%nday   =    19
TBALLOONS(5)%TLAUNCH%xtime  = 32435.
!
!* latitude and longitude of launching site (decimal degree)
!
TBALLOONS(5)%XLATLAUNCH                = 45.800
TBALLOONS(5)%XLONLAUNCH                =  8.630
!
!* altitude of the launching site for 'RADIOS'
!* altitude or pressure of the flight level for 'ISODEN'
!
TBALLOONS(5)%XALTLAUNCH                =    340.
!TBALLOONS(5)%XPRES               = 62360.
!
!* time step for data storage  (s)
!
TBALLOONS(5)%TFLYER_TIME%XTSTEP = 20.
!
!* ascentional vertical speed of the ballon (in calm air) (for 'RADIOS')
!
!TBALLOONS(5)%XWASCENT            = 2.55
!
!* aerodynamic drag coefficient of the balloon (for 'CVBALL')
!* induced drag coefficient (i.e. air shifted by the balloon) (for 'CVBALL')
!* volume of the balloon (m3) (for 'CVBALL')
!* mass of the balloon (kg) (for 'CVBALL')
!
TBALLOONS(5)%XAERODRAG           = 0.44
TBALLOONS(5)%XINDDRAG           = 0.014
TBALLOONS(5)%XVOLUME           = 3.040
TBALLOONS(5)%XMASS           = 2.4516
TBALLOONS(5)%XDIAMETER           = ((3.*TBALLOONS(5)%XVOLUME)/(4.*XPI))**(1./3.)
!
IF ( NBALLOONS < 6 ) RETURN
#else
CALL PRINT_MSG( NVERB_FATAL, 'GEN', 'INI_BALLOON', 'balloon characteristics are commented' )
#endif
!----------------------------------------------------------------------------
!
!*      6.   Balloon number 6
!            ----------------
#if 0
!
!* model number
!
TBALLOONS(6)%NMODEL             = 0
TBALLOONS(6)%CMODEL             = 'FIX'
!
!* balloon type
!
TBALLOONS(6)%CTYPE               = 'CVBALL'
!
!* balloon name
!
TBALLOONS(6)%CTITLE              = 'CVB1RCVB'
!
!* launching date and time
!
TBALLOONS(6)%TLAUNCH%nyear  =  1999
TBALLOONS(6)%TLAUNCH%nmonth =    09
TBALLOONS(6)%TLAUNCH%nday   =    19
TBALLOONS(6)%TLAUNCH%xtime  = 32460.
!
!* latitude and longitude of launching site (decimal degree)
!
TBALLOONS(6)%XLATLAUNCH                = 45.922
TBALLOONS(6)%XLONLAUNCH                =  8.646
!
!* altitude of the launching site for 'RADIOS'
!* altitude or pressure of the flight level for 'ISODEN'
!
!TBALLOONS(6)%XALTLAUNCH                =   3959.
!TBALLOONS(6)%XPRES               = 62360.
!
!* time step for data storage  (s)
!
TBALLOONS(6)%TFLYER_TIME%XTSTEP = 20.
!
!* ascentional vertical speed of the ballon (in calm air) (for 'RADIOS')
!
!TBALLOONS(6)%XWASCENT            = 2.55
!
!* aerodynamic drag coefficient of the balloon (for 'CVBALL')
!* induced drag coefficient (i.e. air shifted by the balloon) (for 'CVBALL')
!* volume of the balloon (m3) (for 'CVBALL')
!* mass of the balloon (kg) (for 'CVBALL')
!
TBALLOONS(6)%XAERODRAG           = 0.44
TBALLOONS(6)%XINDDRAG           = 0.014
TBALLOONS(6)%XVOLUME           = 3.040
TBALLOONS(6)%XMASS           = 2.4516
TBALLOONS(6)%XDIAMETER           = ((3.*TBALLOONS(6)%XVOLUME)/(4.*XPI))**(1./3.)
!
IF ( NBALLOONS < 7 ) RETURN
#else
CALL PRINT_MSG( NVERB_FATAL, 'GEN', 'INI_BALLOON', 'balloon characteristics are commented' )
#endif
!----------------------------------------------------------------------------
!
!*      7.   Balloon number 7
!            ----------------
#if 0
!
!* model number
!
TBALLOONS(7)%NMODEL             = 0
TBALLOONS(7)%CMODEL             = 'FIX'
!
!* balloon type
!
TBALLOONS(7)%CTYPE               = 'CVBALL'
!
!* balloon name
!
TBALLOONS(7)%CTITLE              = 'CVB1PISO'
!
!* launching date and time
!
TBALLOONS(7)%TLAUNCH%nyear  =  1999
TBALLOONS(7)%TLAUNCH%nmonth =    09
TBALLOONS(7)%TLAUNCH%nday   =    19
TBALLOONS(7)%TLAUNCH%xtime  = 32460.
!
!* latitude and longitude of launching site (decimal degree)
!
TBALLOONS(7)%XLATLAUNCH                = 45.922
TBALLOONS(7)%XLONLAUNCH                =  8.646
!
!* altitude of the launching site for 'RADIOS'
!* altitude or pressure of the flight level for 'ISODEN'
!
!TBALLOONS(7)%XALTLAUNCH                =   3959.
TBALLOONS(7)%XPRES               = 62360.
!
!* time step for data storage  (s)
!
TBALLOONS(7)%TFLYER_TIME%XTSTEP = 20.
!
!* ascentional vertical speed of the ballon (in calm air) (for 'RADIOS')
!
!TBALLOONS(7)%XWASCENT            = 2.55
!
!* aerodynamic drag coefficient of the balloon (for 'CVBALL')
!* induced drag coefficient (i.e. air shifted by the balloon) (for 'CVBALL')
!* volume of the balloon (m3) (for 'CVBALL')
!* mass of the balloon (kg) (for 'CVBALL')
!
TBALLOONS(7)%XAERODRAG           = 0.44
TBALLOONS(7)%XINDDRAG           = 0.014
TBALLOONS(7)%XVOLUME           = 3.040
TBALLOONS(7)%XMASS           = 2.4516
TBALLOONS(7)%XDIAMETER           = ((3.*TBALLOONS(7)%XVOLUME)/(4.*XPI))**(1./3.)
#else
CALL PRINT_MSG( NVERB_FATAL, 'GEN', 'INI_BALLOON', 'balloon characteristics are commented' )
#endif
!
!----------------------------------------------------------------------------
!
END SUBROUTINE INI_BALLOON
