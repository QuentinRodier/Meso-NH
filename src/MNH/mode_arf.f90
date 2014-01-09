!MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$ $Date$
!-----------------------------------------------------------------
!     ######spl
      MODULE MODE_ARF
!     ####################
!
!!****  *MODE_ARF* -  module routines  
!!
!!    PURPOSE
!!    -------
!!      Compute Axis ratio function according to parameter CARF
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      Module MODD_RADAR
!!        CARF
!!
!!    REFERENCE
!!    ---------
!!      Andsager, K., K. V. Beard and N. F. Laird, 1999: Laboratory measurements of
!!  axis ratios for large raindrops. J. Atmos. Sci., 56(15), 2673-2683.
!!
!!      Pruppacher, H. R. and K. V. Beard, 1970: A wind tunnel investigation of the
!!  internal circulation and shape of water drops falling at terminal velocity in
!!  air. Quart. J. Roy. Meteor. Soc., 96, 247-256.
!!    
!!
!!    AUTHOR
!!    ------
!!      O. Caumont   * Meteo France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original   08/02/2005    
!--------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS

!              ------------
USE MODD_RADAR, ONLY:CARF
!
!-------------------------------------------------------------------------------
!
CONTAINS
!*       4.   FUNCTION ARF
!             -------------------
!-------------------------------------------------------------------------------
!   ###########################################
    FUNCTION ARF(PDEQ)
!   ##########################################
!   returns the drop axis ratios
    IMPLICIT NONE
    REAL, INTENT(IN) :: PDEQ
    REAL :: ARF
    
    IF (CARF=="PB70") THEN
       IF(PDEQ>=5E-4) THEN
          ARF=1.03-62.*PDEQ
       ELSE
          ARF=1.
       END IF
    ELSE
       IF(PDEQ>=1.1E-3.and.PDEQ<=4.4E-3) THEN
          ARF=1.012-14.4*PDEQ-1.03E4*PDEQ**2
       ELSE
          ARF=MAX(1.0048+.57*PDEQ-2.628E4*PDEQ**2+3.682E6*PDEQ**3-1.677E8*PDEQ**4,.2)
       END IF
    END IF
    
  END FUNCTION ARF
!

END MODULE MODE_ARF
