!     ####################################
      MODULE MODD_LIMA_PRECIP_SCAVENGING_n
!     ####################################
!
!!****  *MODD_PRECIP_SCAVENGING$n* - declaration of scavenged aerosols 
!!                                   precipitating fields
!!
!!    PURPOSE
!!    -------
!       Stores the INstantaneous and ACcumulated PRecipitating fields of 
!!      scavenged aerosol by rain
!!
!!    AUTHOR
!!    ------
!!      J.-P. Pinty      * Laboratoire d'Aerologie*
!!      S.    Berthet    * Laboratoire d'Aerologie*
!!      B.    Vié        * Laboratoire d'Aerologie*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original             ??/??/13 
!!
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
USE MODD_PARAMETERS, ONLY: JPMODELMAX
!
IMPLICIT NONE
!
REAL, DIMENSION(:,:), POINTER :: XINPAP=>NULL(), XACPAP=>NULL()
                                         ! Instant and cumul of ground
                                         ! precipitation fields of Scavenged
                                         ! Aerosol Particles

CONTAINS

SUBROUTINE LIMA_PRECIP_SCAVENGING_GOTO_MODEL(KFROM, KTO)
  INTEGER, INTENT(IN) :: KFROM, KTO
END SUBROUTINE LIMA_PRECIP_SCAVENGING_GOTO_MODEL
!
!
END MODULE MODD_LIMA_PRECIP_SCAVENGING_n
