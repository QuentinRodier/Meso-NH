!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE AVERAGE_CO2(U, IM, PRHOA, PSFCO2, PSFCO2NAT, PSFCO2ANT )
!     #################################################################
!
!
!!****  *AVERAGE_CO2*  
!!
!!    PURPOSE
!!    -------
!      Average CO2 fluxes.
!     
!!**  METHOD
!!    ------
!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------ 
!!
!!      
!!    REFERENCE
!!    ---------
!!      
!!    AUTHOR
!!    ------
!!      B. Decharme           * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    07/04/2017 
!!      Roland Séférian    03/05/2017 update total SFCO2
!!      Roland Séférian    03/05/2020 crop harvesting
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_SURF_ATM_n,ONLY : SURF_ATM_t
USE MODD_SURFEX_n,  ONLY : ISBA_MODEL_t

USE MODD_SURF_PAR,  ONLY : XUNDEF
USE MODD_SURF_ATM,  ONLY : LCO2FOS
USE MODD_CO2V_PAR,  ONLY : XMC, XMCO2
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
TYPE(SURF_ATM_t),      INTENT(INOUT)  :: U
TYPE(ISBA_MODEL_t),    INTENT(INOUT)  :: IM
!
REAL, DIMENSION(:),     INTENT(IN)    :: PRHOA         ! air density
!
REAL, DIMENSION(:),     INTENT(INOUT) :: PSFCO2        ! total CO2 flux to atm (m/s*kg_CO2/kg_air)
!
REAL, DIMENSION(:),     INTENT(OUT)   :: PSFCO2NAT     ! natural CO2 flux to atm (kgCO2/m2/s)
REAL, DIMENSION(:),     INTENT(OUT)   :: PSFCO2ANT     ! total anthoprogenic CO2 flux to atm (kgCO2/m2/s)
!
!
!*      0.2    declarations of local variables
!
INTEGER :: JI, JP  ! loop counter on tiles
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('AVERAGE_CO2',0,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
!       1.     Init
!              ----
!
PSFCO2NAT(:) = 0.0
PSFCO2ANT(:) = 0.0
!
!       2.     natural CO2 flux
!              ----------------
!
PSFCO2NAT(:) = PSFCO2(:) * PRHOA(:) * XMC/XMCO2               ! kgC02/kgAir m/s -> kgC/m2/s
!
!       3.     land use treatement
!              -------------------
!
IF(IM%O%LLULCC)THEN
  DO JP=1,U%NSIZE_NATURE
     JI=U%NR_NATURE(JP) 
     PSFCO2ANT(JI) = IM%S%XFLUTOATM(JP) + IM%S%XFHARVESTTOATM(JP)   
  ENDDO
ENDIF
!
!       4.     fossil fuel
!              ------------
!
IF(LCO2FOS)THEN
  PSFCO2ANT(:) = PSFCO2ANT(:) + U%XCO2FOS(:)
ENDIF
!
!
!       5.     Update net carbon flux out from land surface
!              --------------------------------------------
!
PSFCO2(:) = PSFCO2(:) + PSFCO2ANT(:) * (XMCO2/XMC) / PRHOA(:)       ! kgC/m2/s -> kgC02/kgAir m/s
!
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('AVERAGE_CO2',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE AVERAGE_CO2
