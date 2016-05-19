!     ######spl
      MODULE MODN_CONDSAMP
!     ##################
!-------------------------------------------------------------------------------
!***	MODD_CONDSAMP  Declaration of namelist NAM_CONDSAMP
!
!!    AUTHOR
!!    ------
!	           : C.Lac                            
!	Creation   : 05.06.2011
!-------------------------------------------------------------------------------
!
!
!*       0.   DECLARATIONS
!             ------------
!
USE MODD_CONDSAMP
!
IMPLICIT NONE
!
NAMELIST /NAM_CONDSAMP/ &
     LCONDSAMP,NCONDSAMP,XRADIO,XSCAL,XHEIGHT_BASE,XDEPTH_BASE, &
     XHEIGHT_TOP,XDEPTH_TOP
!
END MODULE MODN_CONDSAMP
