!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! MASDEV4_7 modd 2006/05/18 13:07:25
!-----------------------------------------------------------------
!     ###########################
      MODULE MODD_RAIN_KHKO_PARAM
!     ###########################
!
!!****  *MODD_RAIN_KHKO_PARAM* - declaration of some microphysical factors
!!                               extensively used in the warm scheme.
!!
!!    PURPOSE
!!    -------
!       The purpose of this declarative module is to declare some precomputed
!     microphysical paramters directly used in routine RAIN_KHKO.
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      None 
!!
!!    REFERENCE
!!    ---------
!!      
!!          
!!    AUTHOR
!!    ------
!!	O.Geoffroy (GMEI)
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    03/2006                      
!!       
!!
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
IMPLICIT NONE 
!
!
REAL,SAVE ::   XR0                               ! new drizzle drops radius
    	                                         ! autoconversion
!
REAL,SAVE :: XCEVAP                              ! Constants for raindrop
                                                 ! evaporation 
!
END MODULE MODD_RAIN_KHKO_PARAM 
