!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
SUBROUTINE BLDCODE(KTYPE,KAGE,KUSAGE,KTERRY,KCODE)
!
IMPLICIT NONE
!
INTEGER, INTENT(IN) :: KTYPE   ! Type of building
INTEGER, INTENT(IN) :: KAGE    ! Date of construction (or total renovation) of building
INTEGER, INTENT(IN) :: KUSAGE  ! Usage of building
INTEGER, INTENT(IN) :: KTERRY  ! Territory
!
INTEGER, INTENT(OUT) :: KCODE  ! Building code
!
KCODE=1000000*KTYPE+10000*KAGE+100*KUSAGE+KTERRY
!
RETURN
!
END SUBROUTINE BLDCODE
