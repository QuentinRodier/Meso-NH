!ORILAM_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!ORILAM_LIC This is part of the ORILAM software governed by the CeCILL-C licence
!ORILAM_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!ORILAM_LIC for details.
!     #######################
     MODULE MODI_AER_VELGRAV
!!   ########################
!!
INTERFACE
!!
SUBROUTINE AER_VELGRAV(PRG, PABST, KMODE, PMU, &
                       PVGG, PDPG,PTEMP, PCOR, PDENSITY_AER)
IMPLICIT NONE
REAL, DIMENSION(:,:), INTENT(IN) :: PRG
REAL, DIMENSION(:),   INTENT(IN) :: PTEMP
REAL, DIMENSION(:),   INTENT(IN) :: PABST   
INTEGER,              INTENT(IN) :: KMODE
REAL, DIMENSION(:,:), INTENT(IN) :: PDENSITY_AER
REAL, DIMENSION(:),   INTENT(OUT) :: PMU
REAL, DIMENSION(:,:), INTENT(OUT) :: PVGG, PDPG
REAL, DIMENSION(:,:), INTENT(OUT) :: PCOR
END SUBROUTINE AER_VELGRAV
!!
END INTERFACE
!!
END MODULE MODI_AER_VELGRAV
!     ##################################################
     SUBROUTINE AER_VELGRAV(PRG, PABST, KMODE, PMU, &
                            PVGG, PDPG,PTEMP, PCOR, PDENSITY_AER)
!!   ###################################################
!!
!!   PURPOSE                               
!!   -------
!!  Calculate the fall speed of an aerosol 
!!
!!   REFERENCE
!!   ---------
!!  Seinfeld and Pandis  p455   
!!
!!   AUTHOR
!!    ------
!!   P. Tulet (CNRM/GMEI)                                 
!!
!!   MODIFICATIONS
!!    -------------
!!   K.  Kaku 09/01/07   (CNRM/GMEI) adapted for one 
!!       dimensional calculations 
!!       and explicit Cunningham's correction factor
!!
! Entry variables:
!
! PM(IN)       -Array of moments
!
!*************************************************************
! Exit variables:
!
! PFSED(IN)  -Array of moment variation due to dry deposition
!
!*************************************************************
! Variables used during the deposition velocity calculation
! 
! PDPG       -Polydisperse diffusivity (m2/s)
! PVGG       -Polydisperse settling velocity  (m/s)
!************************************************************
!!
!-----------------------------------------------------------------
!       
!*      0. DECLARATIONS
!          ------------
!
USE MODD_CST
!
IMPLICIT NONE
!
!*      0.1  declarations of arguments
!
REAL, DIMENSION(:,:), INTENT(IN) :: PRG    ! aerosol radius [�m]
REAL, DIMENSION(:),   INTENT(IN) :: PTEMP  ! air tempurature [K]
REAL, DIMENSION(:),   INTENT(IN) :: PABST  ! Pressure (Pa)
INTEGER,              INTENT(IN) :: KMODE  ! Nb aerosols modes
REAL, DIMENSION(:,:), INTENT(IN) :: PDENSITY_AER
REAL, DIMENSION(:),   INTENT(OUT) :: PMU   ! air viscosity 
REAL, DIMENSION(:,:), INTENT(OUT) :: PVGG  ! Gravitation velovity (m/s)
REAL, DIMENSION(:,:), INTENT(OUT) :: PDPG  ! Diffusion coeff (m2/s)
REAL, DIMENSION(:,:), INTENT(OUT) :: PCOR  ! Cunningham factor [unitless]

!
!*      0.2  declaration of local variables
!
REAL, DIMENSION(SIZE(PRG,1)) :: ZLAMBDA
!
REAL, DIMENSION(SIZE(PRG,1)) :: ZRG  !radius in meters

!
REAL, PARAMETER :: gasmw=28.9644E-3
REAL, PARAMETER :: gasr=8.3143

REAL :: ZK
INTEGER :: JI
!
!-----------------------------------------------------------------
!
! Sutherland's equation for viscosity
PMU(:)=1.8325d-5*416.16/(PTEMP(:)+120)*(PTEMP(:)/296.16)*SQRT(PTEMP(:)/296.16)
PMU(:)=MAX(PMU(:),1.e-12)
!
! Mean free path of air (Seinfeld and Pandis p455)
ZLAMBDA(:)=2*PMU(:)/(PABST(:)*SQRT(8*gasmw/(XPI*gasr*PTEMP(:))))

!
DO JI=1,KMODE
!  
!put radius into meters
  ZRG(:)=PRG(:,JI) * 1E-6 
   !
!Slip Correction Factor
  PCOR(:,JI)=1+ZLAMBDA(:)/ZRG(:)*(1.257+ &
              0.4*exp(-1.1*ZRG(:)/ZLAMBDA(:)))
  PCOR(:,JI)=MAX(PCOR(:,JI),1.0)
                                      
  !
  PVGG(:,JI)= 2.*XG*PDENSITY_AER(:,JI)*ZRG(:)**2 /(9.*PMU(:))*PCOR(:,JI)
  PDPG(:,JI)=XBOLTZ*PTEMP(:)*PCOR(:,JI) &
                 / (6.*XPI* ZRG(:)*PMU(:))
  ! 
ENDDO
!
!!
END SUBROUTINE AER_VELGRAV
