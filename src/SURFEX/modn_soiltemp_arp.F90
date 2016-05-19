!!
!!    #####################
      MODULE MODN_SOILTEMP_ARP
!!    #####################
!!
!!*** *MODN_SOITEMP_ARP*
!!
!!    PURPOSE
!!    -------
!       Namelist for  
!!
!!**  AUTHOR
!!    ------
!
!!    MODIFICATIONS
!!    -------------
!!    Original 24/02/05
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!-----------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!        -----------------
IMPLICIT NONE
!
INTEGER, PARAMETER          :: NMAX_LAYER=10
LOGICAL                     :: LTEMP_ARP
INTEGER                     :: NTEMPLAYER_ARP
REAL, DIMENSION(NMAX_LAYER) :: SODELX
!
NAMELIST /NAM_SOILTEMP_ARP/LTEMP_ARP, NTEMPLAYER_ARP, SODELX
!
END MODULE MODN_SOILTEMP_ARP
