!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! MASDEV4_7 modd 2006/05/18 13:07:25
!-----------------------------------------------------------------
!     ###############
      MODULE MODD_REF
!     ###############
!
!!****  *MODD_REF* - declaration of reference state  profile
!!
!!    PURPOSE
!!    -------
!       The purpose of this declarative module is to declare  the vertical
!     profile of  the reference state, used for the anelastic 
!     approximation. 
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      None 
!!
!!    REFERENCE
!!    ---------
!!      Book2 of documentation of Meso-NH (module MODD_REF)
!!      Technical Specifications Report of the Meso-NH (chapters 2 and 3)
!!
!!    AUTHOR
!!    ------
!!	V. Ducrocq   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original   07/06/94   
!!                    07/13 (C.Lac) Add LBOUSS
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
IMPLICIT NONE
!          
REAL,SAVE, DIMENSION(:), ALLOCATABLE :: XRHODREFZ ! rhod(z) for reference
                                             ! state without orography
REAL,SAVE, DIMENSION(:), ALLOCATABLE :: XTHVREFZ  ! Thetav(z) for reference
                                             ! state without orography    
REAL,SAVE                            :: XEXNTOP   ! Exner function at model top 
LOGICAL, SAVE                        :: LBOUSS    ! Boussinesq approximation
! 
END MODULE MODD_REF
