!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #####################
      MODULE MODD_INIT_LANDUSE
!     #####################
!
!!****  *MODD_INIT_LANDUSE* - declaration of landuse types used for landuse initialisation
!!
!!    PURPOSE
!!    -------
!       The purpose of this declarative module is to define landuse types used for landuse initialisation
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      NONE 
!!
!!    REFERENCE
!!    --------- 
!!       
!!    AUTHOR
!!    ------
!!      B. Decharme   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    20/12/2021                      
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
TYPE LULCC_P_t
!
  REAL, DIMENSION(:,:),   POINTER :: DG            ! previous year DG
  REAL, DIMENSION(:,:),   POINTER :: DZG           ! previous year DZG
!
  REAL, DIMENSION(:,:),   POINTER :: WG            ! previous year WG
  REAL, DIMENSION(:,:),   POINTER :: WGI           ! previous year WGI
  REAL, DIMENSION(:,:),   POINTER :: WSN           ! previous year SWE
  REAL, DIMENSION(:),     POINTER :: WR            ! previous year WR
!
  REAL, DIMENSION(:),     POINTER :: LAI           ! previous year LAI
  REAL, DIMENSION(:),     POINTER :: PATCH         ! previous year PATCHES
!
  REAL, DIMENSION(:,:),   POINTER :: BIOMASS       ! previous year biomass (Kg/m2)
  REAL, DIMENSION(:,:),   POINTER :: LIGNIN_STRUC  ! previous year lignin (gC/m2)
  REAL, DIMENSION(:,:,:), POINTER :: LITTER        ! previous year litter (gC/m2)
  REAL, DIMENSION(:,:),   POINTER :: SOILCARB      ! previous year soil carbon (gC/m2)
!
  REAL, DIMENSION(:),     POINTER :: LULCC_HARVEST ! Carbon due to biomass harvested after land use change including patch fraction (kgC/m2)
  REAL, DIMENSION(:,:),   POINTER :: TURNOVER      ! turnover between Biomass and litter (gC/m2)
!
  REAL, DIMENSION(:),     POINTER :: SURF_LIGNIN   ! DIF option previous year lignin (gC/m2)
  REAL, DIMENSION(:,:),   POINTER :: SURF_LITTER   ! DIF option previous year litter (gC/m2)
  REAL, DIMENSION(:,:),   POINTER :: SOIL_LIGNIN   ! DIF option previous year lignin (gC/m2)
  REAL, DIMENSION(:,:,:), POINTER :: SOIL_LITTER   ! DIF option previous year litter (gC/m2)
  REAL, DIMENSION(:,:,:), POINTER :: SOIL_CARBON   ! DIF option previous year soil carbon (gC/m2)
!
  INTEGER, DIMENSION(:),  POINTER :: WG_LAYER      ! previous year hydrological layers
!
  REAL, DIMENSION(:,:),   POINTER :: VEGTYPE_PATCH ! previous vegtype by patch
!
END TYPE LULCC_P_t
!
!-------------------------------------------------------------------------------
!
TYPE LULCC_NP_t
!
   TYPE(LULCC_P_t), DIMENSION(:), POINTER :: AL=>NULL()
!
END TYPE LULCC_NP_t
!
!-------------------------------------------------------------------------------
!
CONTAINS
!
!-------------------------------------------------------------------------------
!
SUBROUTINE INIT_LULCC_P(YLULCC_P)
TYPE(LULCC_P_t), INTENT(INOUT) :: YLULCC_P
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK("MODD_INIT_LANDUSE:INIT_LULCC_P",0,ZHOOK_HANDLE)
!
NULLIFY(YLULCC_P%DG) 
NULLIFY(YLULCC_P%DZG) 
NULLIFY(YLULCC_P%WG) 
NULLIFY(YLULCC_P%WGI) 
NULLIFY(YLULCC_P%WSN) 
NULLIFY(YLULCC_P%WR) 
NULLIFY(YLULCC_P%LAI) 
NULLIFY(YLULCC_P%PATCH) 
NULLIFY(YLULCC_P%BIOMASS) 
NULLIFY(YLULCC_P%LIGNIN_STRUC) 
NULLIFY(YLULCC_P%LITTER) 
NULLIFY(YLULCC_P%SOILCARB) 
NULLIFY(YLULCC_P%LULCC_HARVEST)
NULLIFY(YLULCC_P%TURNOVER)
NULLIFY(YLULCC_P%SURF_LIGNIN) 
NULLIFY(YLULCC_P%SURF_LITTER) 
NULLIFY(YLULCC_P%SOIL_LIGNIN) 
NULLIFY(YLULCC_P%SOIL_LITTER) 
NULLIFY(YLULCC_P%SOIL_CARBON) 
NULLIFY(YLULCC_P%WG_LAYER) 
NULLIFY(YLULCC_P%VEGTYPE_PATCH) 
!
IF (LHOOK) CALL DR_HOOK("MODD_INIT_LANDUSE:INIT_LULCC_P",1,ZHOOK_HANDLE)
END SUBROUTINE INIT_LULCC_P
!
!-------------------------------------------------------------------------------
!
SUBROUTINE INIT_LULCC_NP(YLULCC_NP,KPATCH)
TYPE(LULCC_NP_t), INTENT(INOUT) :: YLULCC_NP
INTEGER, INTENT(IN) :: KPATCH
INTEGER :: JP
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK("MODD_INIT_LANDUSE:INIT_LULCC_NP",0,ZHOOK_HANDLE)
!
IF (ASSOCIATED(YLULCC_NP%AL)) THEN
   DEALLOCATE(YLULCC_NP%AL)
ELSE
   ALLOCATE(YLULCC_NP%AL(KPATCH))
   DO JP = 1,KPATCH
      CALL INIT_LULCC_P(YLULCC_NP%AL(JP))
   ENDDO
ENDIF
!
IF (LHOOK) CALL DR_HOOK("MODD_INIT_LANDUSE:INIT_LULCC_NP",1,ZHOOK_HANDLE)
END SUBROUTINE INIT_LULCC_NP
!
!-------------------------------------------------------------------------------
!
END MODULE MODD_INIT_LANDUSE

