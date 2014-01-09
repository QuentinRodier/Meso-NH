!ORILAM_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!ORILAM_LIC This is part of the ORILAM software governed by the CeCILL-C licence
!ORILAM_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!ORILAM_LIC for details.
!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! MASDEV4_7 newsrc 2006/11/16 16:45:25
!-----------------------------------------------------------------
!!   ##############################
     MODULE MODI_SALT_FILTER
!!   ##############################
!!
INTERFACE
!
SUBROUTINE SALT_FILTER(PSV, PRHODREF)

IMPLICIT NONE

REAL,  DIMENSION(:,:,:,:),  INTENT(INOUT) :: PSV
REAL,  DIMENSION(:,:,:),    INTENT(IN)    :: PRHODREF

END SUBROUTINE SALT_FILTER
!!
END INTERFACE
!!
END MODULE MODI_SALT_FILTER
!!
!!   #######################################
     SUBROUTINE SALT_FILTER(PSV, PRHODREF)
!!   #######################################
!!
!!   PURPOSE
!!   -------
!!
!!   REFERENCE
!!   ---------
!!   none
!!
!!   AUTHOR
!!    ------
!!    Pierre TULET (CNRM/GMEI) 
!!
!!   MODIFICATIONS
!!    -------------
!!   Original
!!
! Entry variables:
!
! PRSVS(INOUT)       -Array of moments included in PRSVS
!
!*************************************************************
! Exit variables:
!
!*************************************************************
! Variables used during the deposition velocity calculation
! 
! ZVGK       -Polydisperse settling velocity of the kth moment (m/s)
!************************************************************
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!   IMPLICIT ARGUMENTS
!
USE MODD_SALT
USE MODD_CSTS_SALT
!
USE MODE_SALT_PSD
USE MODD_CST, ONLY : XMNH_TINY
!
IMPLICIT NONE
!
!*       0.1   Declarations of dummy arguments :
!
REAL,  DIMENSION(:,:,:,:),  INTENT(INOUT) :: PSV
REAL,  DIMENSION(:,:,:),    INTENT(IN)    :: PRHODREF
!
!*       0.2   Declarations of local variables :
!
INTEGER :: JN
INTEGER :: IMODEIDX
REAL,    DIMENSION(NMODE_SLT*3) :: ZPMIN
REAL,    DIMENSION(NMODE_SLT)   :: ZINIRADIUS
REAL,    DIMENSION(SIZE(PSV,1), SIZE(PSV,2), SIZE(PSV,3), NMODE_SLT*3)  :: ZM                  ! [aerosol units] local array which goes to output later

REAL :: ZRGMIN, ZSIGMIN
REAL :: ZRHOP, ZMI
INTEGER,DIMENSION(NMODE_SLT) :: NM0, NM3, NM6
!
!*       0.3  initialize constant
!
ZRHOP = XDENSITY_SALT
ZMI   = XMOLARWEIGHT_SALT ! molecular mass in kg/mol
!
!-------------------------------------------------------------------------------

!
PSV(:,:,:,:) =  MAX(PSV(:,:,:,:), XMNH_TINY)
!

END SUBROUTINE SALT_FILTER
