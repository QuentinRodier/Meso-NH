!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
    SUBROUTINE LANDUSE_CARBON_MANAGING(PVEGTYPE_PATCH_OLD,PLULCC_HARVEST,  &
                                       PEXPORT_DECADAL,PEXPORT_CENTURY,    &
                                       PCSTOCK_DECADAL,PCSTOCK_CENTURY,    &
                                       PFLUATM,PFANTATM,PFLUANT            )       
!   ###############################################################
!!
!!    PURPOSE
!!    -------
!
!     Performs land use land cover change managing computation at yearly time step
!               
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!    none
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!      
!!    none
!!
!!    REFERENCE
!!    ---------
!!
!!      
!!    AUTHOR
!!    ------
!!    R. Séférian 08/2015
!!    B. Decharme 08/2021
!!
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_LANDUSE_PAR
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
REAL, DIMENSION(:,:), INTENT(IN)     :: PVEGTYPE_PATCH_OLD ! previous fraction of vegtype by patch
!
REAL, DIMENSION(:),   INTENT(IN)     :: PLULCC_HARVEST     ! Carbon carbon transferred to litter pools including patch fraction (kgC/m2)
!
REAL, DIMENSION(:,:), INTENT(INOUT)  :: PCSTOCK_DECADAL    ! Decadal carbon anthropogenic stock
REAL, DIMENSION(:,:), INTENT(INOUT)  :: PEXPORT_DECADAL    ! Decadal use of carbon anthropogenic stock
REAL, DIMENSION(:,:), INTENT(INOUT)  :: PCSTOCK_CENTURY    ! Centennial carbon anthropogenic stock
REAL, DIMENSION(:,:), INTENT(INOUT)  :: PEXPORT_CENTURY    ! Centennial use of carbon anthropogenic stock
!
REAL, DIMENSION(:),   INTENT(INOUT)  :: PFLUATM            ! LULCC carbon flux Cstock -> atm   (kgC/m2)
REAL, DIMENSION(:),   INTENT(INOUT)  :: PFLUANT            ! LULCC carbon flux Cstock -> Canth (kgC/m2)
REAL, DIMENSION(:),   INTENT(INOUT)  :: PFANTATM           ! LULCC carbon flux Canth-> atm     (kgC/m2)
!
!*      0.2    declarations of local parameter
!
REAL, DIMENSION(SIZE(PLULCC_HARVEST,1))                           :: ZFLUATM_ANNUAL          
REAL, DIMENSION(SIZE(PLULCC_HARVEST,1))                           :: ZFLUATM_DECADAL          
REAL, DIMENSION(SIZE(PLULCC_HARVEST,1))                           :: ZFLUATM_CENTURY         
REAL, DIMENSION(SIZE(PLULCC_HARVEST,1))                           :: ZFLUANT          
REAL, DIMENSION(SIZE(PLULCC_HARVEST,1))                           :: ZEXPORT_COEF_ANNUAL
REAL, DIMENSION(SIZE(PLULCC_HARVEST,1))                           :: ZEXPORT_COEF_DECADAL
REAL, DIMENSION(SIZE(PLULCC_HARVEST,1))                           :: ZEXPORT_COEF_CENTURY
!
REAL, DIMENSION(SIZE(PLULCC_HARVEST,1),SIZE(PCSTOCK_DECADAL,2)+1) :: ZCSTOCK_DECADAL 
REAL, DIMENSION(SIZE(PLULCC_HARVEST,1),SIZE(PCSTOCK_CENTURY,2)+1) :: ZCSTOCK_CENTURY
!
INTEGER :: JI, JIND, JYEAR, JVEG
INTEGER :: INI, INVEG, INDEC, INCEN
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-----------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('LANDUSE_CARBON_MANAGING',0,ZHOOK_HANDLE)
!-----------------------------------------------------------------
!
!*      1.     Preliminaries
!              -------------
!
INI  =SIZE(PVEGTYPE_PATCH_OLD,1)
INVEG=SIZE(PVEGTYPE_PATCH_OLD,2)
INDEC=SIZE(PCSTOCK_DECADAL,2)
INCEN=SIZE(PCSTOCK_CENTURY,2)
!
! 1.1 Initialize matrix 
!
ZFLUATM_ANNUAL (:) = 0.
ZFLUATM_DECADAL(:) = 0.
ZFLUATM_CENTURY(:) = 0.
ZFLUANT        (:) = 0.
!
! 1.2 Initialize stock matrix
! Current year (year=1) anthropogenic stock of carbon is set to zero
! other years (2...N) conserve the previous stock of anth carbon
!
ZCSTOCK_DECADAL(:,:) = 0.
ZCSTOCK_DECADAL(:,2:SIZE(ZCSTOCK_DECADAL,2)) = PCSTOCK_DECADAL(:,:)
!
ZCSTOCK_CENTURY(:,:) = 0.
ZCSTOCK_CENTURY(:,2:SIZE(ZCSTOCK_CENTURY,2)) = PCSTOCK_CENTURY(:,:)
!
! 1.3 Characterized biomass export properties for each patch
!
ZEXPORT_COEF_ANNUAL (:)   = 0.
ZEXPORT_COEF_DECADAL(:)   = 0.
ZEXPORT_COEF_CENTURY(:)   = 0.

DO JVEG=1,INVEG
   DO JI=1,INI
      ZEXPORT_COEF_ANNUAL (JI) = ZEXPORT_COEF_ANNUAL (JI) + XEXPORT_COEF_ANNUAL (JVEG) * PVEGTYPE_PATCH_OLD(JI,JVEG)
      ZEXPORT_COEF_DECADAL(JI) = ZEXPORT_COEF_DECADAL(JI) + XEXPORT_COEF_DECADAL(JVEG) * PVEGTYPE_PATCH_OLD(JI,JVEG)
      ZEXPORT_COEF_CENTURY(JI) = ZEXPORT_COEF_CENTURY(JI) + XEXPORT_COEF_CENTURY(JVEG) * PVEGTYPE_PATCH_OLD(JI,JVEG)
   ENDDO
ENDDO
!
!-----------------------------------------------------------------
!
!*      3.     LULCC Carbon Fluxes
!              -------------------
!
!* Annual export of carbon
ZFLUATM_ANNUAL(:) = ZFLUATM_ANNUAL(:) + ZEXPORT_COEF_ANNUAL(:) * PLULCC_HARVEST(:)
!
!* Decadal export of carbon
ZCSTOCK_DECADAL(:,1) = ZCSTOCK_DECADAL(:,1) + ZEXPORT_COEF_DECADAL(:) * PLULCC_HARVEST(:)
!
!* Carbon flux to the decadal anthropogenic C pool
ZFLUANT(:) = ZFLUANT(:) + ZEXPORT_COEF_DECADAL(:) * PLULCC_HARVEST(:)
!
!temporal decay
DO JIND=1,INDEC-1
    JYEAR = INDEC - JIND + 1
    DO JI=1,INI
       ZFLUATM_DECADAL(JI      ) = ZFLUATM_DECADAL(JI        ) + PEXPORT_DECADAL(JI,JYEAR  )
       ZCSTOCK_DECADAL(JI,JYEAR) = ZCSTOCK_DECADAL(JI,JYEAR-1) - PEXPORT_DECADAL(JI,JYEAR-1)
       PEXPORT_DECADAL(JI,JYEAR) = PEXPORT_DECADAL(JI,JYEAR-1)
       IF(ZCSTOCK_DECADAL(JI,JYEAR) < 0.)THEN
          ZCSTOCK_DECADAL(JI,JYEAR) = 0.
       ENDIF
    ENDDO
ENDDO
!
!update of the stock  
ZFLUATM_DECADAL(:)   = ZFLUATM_DECADAL(:) + PEXPORT_DECADAL(:,2)  
PEXPORT_DECADAL(:,2) = ZCSTOCK_DECADAL(:,1)/REAL(INDEC)
ZCSTOCK_DECADAL(:,2) = ZCSTOCK_DECADAL(:,1)
!
!
!* Centennial export of carbon
ZCSTOCK_CENTURY(:,1) = ZCSTOCK_CENTURY(:,1) + ZEXPORT_COEF_CENTURY(:) * PLULCC_HARVEST(:)
!
!* Carbon flux to the centennial anthropogenic C pool
ZFLUANT(:) = ZFLUANT(:) + ZEXPORT_COEF_CENTURY(:) * PLULCC_HARVEST(:)
!
!  temporal decay
DO JIND=1,INCEN-1
    JYEAR = INCEN - JIND + 1
    DO JI=1,INI
       ZFLUATM_CENTURY(JI      ) = ZFLUATM_CENTURY(JI        ) + PEXPORT_CENTURY(JI,JYEAR  )
       ZCSTOCK_CENTURY(JI,JYEAR) = ZCSTOCK_CENTURY(JI,JYEAR-1) - PEXPORT_CENTURY(JI,JYEAR-1)
       PEXPORT_CENTURY(JI,JYEAR) = PEXPORT_CENTURY(JI,JYEAR-1)
       IF(ZCSTOCK_CENTURY(JI,JYEAR) < 0.)THEN
          ZCSTOCK_CENTURY(JI,JYEAR) = 0.
       ENDIF
    ENDDO
ENDDO
!
!update of the stock  
ZFLUATM_CENTURY(:) = ZFLUATM_CENTURY(:) + PEXPORT_CENTURY(:,2)  
PEXPORT_CENTURY(:,2) = ZCSTOCK_CENTURY(:,1)/REAL(INCEN)
ZCSTOCK_CENTURY(:,2) = ZCSTOCK_CENTURY(:,1)
!
!reset current year reservoir
ZCSTOCK_DECADAL(:,1) = 0.
ZCSTOCK_CENTURY(:,1) = 0.
!
!* Finalize
! 
! Carbon flux due to vegetation clearance
PFLUATM(:)  =  ZFLUATM_ANNUAL(:) 
!
! Carbon flux from the anthropogenic carbon pool
PFANTATM(:) = (ZFLUATM_DECADAL(:)+ ZFLUATM_CENTURY(:)) 
!
! Carbon flux from natural carbon stock to anthropogenic carbon pool
PFLUANT(:)  =  ZFLUANT(:)
!
! Conserve history of stock for the following years
PCSTOCK_DECADAL(:,:) = ZCSTOCK_DECADAL(:,2:SIZE(ZCSTOCK_DECADAL,2)) 
PCSTOCK_CENTURY(:,:) = ZCSTOCK_CENTURY(:,2:SIZE(ZCSTOCK_CENTURY,2)) 
!
!-----------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('LANDUSE_CARBON_MANAGING',1,ZHOOK_HANDLE)
!-----------------------------------------------------------------
!
END SUBROUTINE LANDUSE_CARBON_MANAGING
