!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
SUBROUTINE DEFAULT_URBAN_FRAC_VEG(PDATA_TOWN,PDATA_GARDEN,PDATA_VEGTYPE, &
         PDATA_FRAC_HVEG, PDATA_FRAC_LVEG, PDATA_FRAC_NVEG)
!
USE MODD_TYPE_DATE_SURF  
USE MODD_SURF_PAR
USE MODD_DATA_COVER_PAR
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
IMPLICIT NONE
!
REAL, DIMENSION(:), INTENT(IN)  :: PDATA_TOWN
REAL, DIMENSION(:), INTENT(IN)  :: PDATA_GARDEN
REAL, DIMENSION(:,:), INTENT(IN):: PDATA_VEGTYPE
REAL, DIMENSION(:), INTENT(OUT) :: PDATA_FRAC_HVEG
REAL, DIMENSION(:), INTENT(OUT) :: PDATA_FRAC_LVEG
REAL, DIMENSION(:), INTENT(OUT) :: PDATA_FRAC_NVEG
!
!*    Declaration of local variables
!     ------------------------------ 
!
REAL, DIMENSION(JPCOVER) :: ZFRAC_GROUND

REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('DEFAULT_URBAN_FRAC_VEG',0,ZHOOK_HANDLE) 
!
!-------------------------------------------------------------------                                                    
! computes no, low and high vegetation fractions in gardens from vegtypes.
! first estimates the fraction of garden that is no or low vegetation
  ZFRAC_GROUND=PDATA_VEGTYPE(:,NVT_NO) + PDATA_VEGTYPE(:,NVT_ROCK) + PDATA_VEGTYPE(:,NVT_SNOW)   &
              +PDATA_VEGTYPE(:,NVT_C3) + PDATA_VEGTYPE(:,NVT_C4) + PDATA_VEGTYPE(:,NVT_IRR)      &
              +PDATA_VEGTYPE(:,NVT_GRAS) + PDATA_VEGTYPE(:,NVT_TROG) + PDATA_VEGTYPE(:,NVT_PARK) &
              +PDATA_VEGTYPE(:,NVT_BOGR)
  ZFRAC_GROUND=MAX(ZFRAC_GROUND,1.E-6)

! as defined for garden, sum of no and low vegetation fractions equals to garden fraction (so it refers to the whole urban area).
! high vegetation is between 0 and road + garden fractions (here limited to garden), and is located above ground.
! note that park, in urban covers, is supposed composed by half of trees.
!
  WHERE (PDATA_TOWN>0.)
    PDATA_FRAC_NVEG(:)= PDATA_GARDEN(:) *  &
              MIN((PDATA_VEGTYPE(:,NVT_NO) + PDATA_VEGTYPE(:,NVT_ROCK) + PDATA_VEGTYPE(:,NVT_SNOW))/ZFRAC_GROUND,1.)
    PDATA_FRAC_LVEG(:)= MAX(PDATA_GARDEN(:)-PDATA_FRAC_NVEG(:),0.)
    PDATA_FRAC_HVEG(:)= PDATA_GARDEN(:) * (                                                          &
               PDATA_VEGTYPE(:,NVT_TEBD) + PDATA_VEGTYPE(:,NVT_BONE) + PDATA_VEGTYPE(:,NVT_TRBE)     &
              +PDATA_VEGTYPE(:,NVT_TRBD) + PDATA_VEGTYPE(:,NVT_TEBE)                                 &
              +PDATA_VEGTYPE(:,NVT_TENE) + PDATA_VEGTYPE(:,NVT_BOBD) + PDATA_VEGTYPE(:,NVT_BOND)     &
              +PDATA_VEGTYPE(:,NVT_SHRB)  )
  END WHERE
!-------------------------------------------------------------------  
!
IF (LHOOK) CALL DR_HOOK('DEFAULT_URBAN_FRAC_VEG',1,ZHOOK_HANDLE) 
!
END SUBROUTINE DEFAULT_URBAN_FRAC_VEG
