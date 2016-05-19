!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$ $Date$
!-----------------------------------------------------------------
!-----------------------------------------------------------------
!-----------------------------------------------------------------
!     ######################################
      MODULE MODD_TURB_FLUX_AIRCRAFT_BALLOON
!     ######################################
!
!!****  *MODD_CVERT* - Declares work arrays for vertical cross-sections
!!
!!    PURPOSE
!!    -------
!       For vertical cross-sections only, this declarative module declares 
!     the arrays containing the sea-level altitudes and the model topography 
!     of the oblique cross-section points.     
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      None 
!!
!!    REFERENCE
!!    ---------
!!
!!     Book2 of the TRACE volume of the Meso-NH user manual
!!     (MODD_CVERT) 
!!       
!!    AUTHOR
!!    ------
!!      P.Lacarrere
!!
!!    MODIFICATIONS
!!    -------------
!!      Original       18/09/06                      
!!        
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
!
IMPLICIT NONE
!
REAL,DIMENSION(:,:,:)  ,ALLOCATABLE,SAVE   :: XTHW_FLUX !sensible flux 
REAL,DIMENSION(:,:,:)  ,ALLOCATABLE,SAVE   :: XRCW_FLUX !Latent flux
REAL,DIMENSION(:,:,:,:),ALLOCATABLE,SAVE   :: XSVW_FLUX !turb scalar flux
!
END MODULE MODD_TURB_FLUX_AIRCRAFT_BALLOON
