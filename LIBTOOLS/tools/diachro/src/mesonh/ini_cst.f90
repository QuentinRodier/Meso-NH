!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$ $Date$
!-----------------------------------------------------------------
!-----------------------------------------------------------------
!-----------------------------------------------------------------
!     ###################
      MODULE MODI_INI_CST
!     ###################
!
INTERFACE
!
SUBROUTINE INI_CST
END SUBROUTINE INI_CST 
!
END INTERFACE
!
END MODULE MODI_INI_CST
!
!
!
!     ##################
      SUBROUTINE INI_CST 
!     ##################
!
!!****  *INI_CST * - routine to initialize the module MODD_CST
!!
!!    PURPOSE
!!    -------
!       The purpose of this routine is to initialize  the physical constants
!     stored in  module MODD_CST.
!      
!
!!**  METHOD
!!    ------
!!      The physical constants are set to their numerical values 
!!     
!!
!!    EXTERNAL
!!    --------
!!      FMLOOK : to retrieve logical unit number associated to a file
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!      Module MODD_CST     : contains physical constants
!!
!!    REFERENCE
!!    ---------
!!      Book2 of the documentation (module MODD_CST, routine INI_CST)
!!      
!!
!!    AUTHOR
!!    ------
!!  	V. Ducrocq       * Meteo France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    18/05/94 
!!      J. Stein    02/01/95  add the volumic mass of liquid water
!!      J.-P. Pinty 13/12/95  add the water vapor pressure over solid ice
!!      J. Stein    29/06/97  add XTH00
!!      V. Masson   05/10/98  add XRHOLI
!!      C. Mari     31/10/00  add NDAYSEC
!!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_CST
!
IMPLICIT NONE
!  
!-------------------------------------------------------------------------------
!
!*	 1.     FUNDAMENTAL CONSTANTS
!	        ---------------------
!
XPI         = 2.*ASIN(1.)
XKARMAN     = 0.4
XLIGHTSPEED = 299792458.
XPLANCK     = 6.6260755E-34
XBOLTZ      = 1.380658E-23
XAVOGADRO   = 6.0221367E+23
!
!-------------------------------------------------------------------------------
!
!*       2.     ASTRONOMICAL CONSTANTS
!	        ----------------------
!
XDAY   = 86400.
XSIYEA = 365.25*XDAY*2.*XPI/ 6.283076
XSIDAY = XDAY/(1.+XDAY/XSIYEA)
XOMEGA = 2.*XPI/XSIDAY
NDAYSEC = 24*3600 ! Number of seconds in a day
!
!-------------------------------------------------------------------------------!
!
!
!*       3.     TERRESTRIAL GEOIDE CONSTANTS
!	        ----------------------------
!
XRADIUS = 6371229.
XG      = 9.80665
!
!-------------------------------------------------------------------------------
!
!*	 4.     REFERENCE PRESSURE
!	        -------------------
!
XP00 = 1.E5
XTH00 = 300.
!-------------------------------------------------------------------------------
!
!*	 5.     RADIATION CONSTANTS
!	        -------------------
!
XSTEFAN = 2.* XPI**5 * XBOLTZ**4 / (15.* XLIGHTSPEED**2 * XPLANCK**3)
XI0     = 1370.
!
!-------------------------------------------------------------------------------
!
!*	 6.     THERMODYNAMIC CONSTANTS
!	        -----------------------
!
XMD    = 28.9644E-3
XMV    = 18.0153E-3
XRD    = XAVOGADRO * XBOLTZ / XMD
XRV    = XAVOGADRO * XBOLTZ / XMV
XCPD   = 7.* XRD /2.
XCPV   = 4.* XRV
XRHOLW = 1000.
XRHOLI = 900.
XCL    = 4.218E+3
XCI    = 2.106E+3
XTT    = 273.16
XLVTT  = 2.5008E+6
XLSTT  = 2.8345E+6
XLMTT  = XLSTT - XLVTT
XESTT  = 611.14
XGAMW  = (XCL - XCPV) / XRV
XBETAW = (XLVTT/XRV) + (XGAMW * XTT)
XALPW  = LOG(XESTT) + (XBETAW /XTT) + (XGAMW *LOG(XTT))
XGAMI  = (XCI - XCPV) / XRV
XBETAI = (XLSTT/XRV) + (XGAMI * XTT)
XALPI  = LOG(XESTT) + (XBETAI /XTT) + (XGAMI *LOG(XTT))
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE INI_CST 
