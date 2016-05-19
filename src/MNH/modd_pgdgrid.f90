!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! MASDEV4_7 modd 2006/05/18 13:07:25
!-----------------------------------------------------------------
!     ###################
      MODULE MODD_PGDGRID
!     ###################
!
!!****  *MODD_PGDGRID* - declaration of grid variables of physiographic 
!!                       data file.
!!
!!    PURPOSE
!!    -------
!       The purpose of this declarative module is to declare  the variables
!     describing the grid of physiographic data file. 
!    
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      None 
!!
!!    REFERENCE
!!    ---------
!!      Book2 of documentation of Meso-NH (module MODD_PGDGRID)
!!
!!    AUTHOR
!!    ------
!!	V. Masson   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    24/01/95                      
!!      Masson      25/07/97   add map factor
!!      Masson      15/03/99   add grid mesh sizes
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
IMPLICIT NONE
REAL :: XPGDLONOR,XPGDLATOR  ! Longitude and latitude of the Origine point
                             !  for the conformal projection
REAL, DIMENSION(:),   ALLOCATABLE :: XPGDXHAT  ! Position x in the conformal
                                               ! plane or on the cartesian plane
REAL, DIMENSION(:),   ALLOCATABLE :: XPGDYHAT  ! Position y in the conformal
                                               ! plane or on the cartesian plane
REAL, DIMENSION(:,:), ALLOCATABLE :: XPGDMAP   ! map factor
!
REAL, DIMENSION(:),  ALLOCATABLE :: XPGD_DXHAT ! grid mesh sizes in x direction
REAL, DIMENSION(:),  ALLOCATABLE :: XPGD_DYHAT ! grid mesh sizes in y direction
!-------------------------------------------------------------------------------
!  
END MODULE MODD_PGDGRID
