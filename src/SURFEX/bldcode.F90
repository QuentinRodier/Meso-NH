!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
FUNCTION BLDCODE(KTYPE,KAGE) RESULT(KCODE)
!
USE MODD_BLD_DESCRIPTION
!
IMPLICIT NONE
INTEGER, DIMENSION(:), INTENT(IN) :: KTYPE ! Type of building
INTEGER, DIMENSION(:), INTENT(IN) :: KAGE  ! date of construction (or total renovation) of building
INTEGER, DIMENSION(SIZE(KTYPE))   :: KCODE ! Building code (merges type & age info).
!
INTEGER :: JL        ! loop counter on points
INTEGER :: JAGE      ! loop counter on construction date ranges
INTEGER :: ICODE_AGE ! code for the adequate construction date range
!
DO JL=1,SIZE(KTYPE)
  ICODE_AGE=NDESC_AGE_LIST(NDESC_AGE) ! default value is the more recent building
  DO JAGE=NDESC_AGE,1,-1
    IF (NDESC_AGE_DATE(JAGE)>=KAGE(JL)) ICODE_AGE = NDESC_AGE_LIST(JAGE)
  END DO
  KCODE(JL) = 100*KTYPE(JL)+ICODE_AGE
END DO
!
END FUNCTION BLDCODE
