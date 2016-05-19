SUBROUTINE INI_OCEAN_CSTS 
!     ##################
!
!!****  *INI_OCEAN_CSTS * - routine to initialize the module MODD_OCEAN_CSTS
!!
!!    PURPOSE
!!    -------
!       The purpose of this routine is to initialize  the physical constants
!     stored in  module MODD_OCEAN_CSTS.
!      
!
!!**  METHOD
!!    ------
!!      The physical constants are set to their numerical values 
!!     
!!
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!      Module MODD_OCEAN_CST     : contains physical constants
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!  	C. Lebeaupin Brossier       * Meteo France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2008
!!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_OCEAN_CSTS
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!  
!-------------------------------------------------------------------------------
!
!
!*	 1.     THERMODYNAMICAL CONSTANTS OF SEAWATER
!	        -------------------------------------
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('INI_OCEAN_CSTS',0,ZHOOK_HANDLE)
!
NOCKMIN = 0
NOCKMAX = 40
!
XRHOSW=1024.
XRHOSWREF=1024.458
XCPSW=4178.4
!
!*	 2.     CORIOLIS FRACTION
!	        -----------------
XGAMA=0.53
!
!*	 3.     TURBULENCE CONSTANTS
!	        --------------------
XZCE=1./0.7
XCKL=0.1
!
!*	 4.      RADIATION CONSTANTS
!	        --------------------
!from Jerlov's 1976 optical water C type I
XR=0.58
XD1=0.35
XD2=23.
!albedo of seawater
XALBEDOSW=0.06
!
!*       5.      OCEANIC MODEL TIME STEP
XOCEAN_TSTEP=5.*60.
IF (LHOOK) CALL DR_HOOK('INI_OCEAN_CSTS',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE INI_OCEAN_CSTS 
