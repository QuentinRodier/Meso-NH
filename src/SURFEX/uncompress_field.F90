!SFX_LIC Copyright 1994-2019 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SFX_LIC for details. version 1.
!------------------------------------------------------------------------------------------------------------
! Modifications:
!  P. Wautelet 19/09/2019: correct support of 64bit integers (MNH_INT=8)
!------------------------------------------------------------------------------------------------------------
SUBROUTINE UNCOMPRESS_FIELD(KLONG,PSEUIL,PFIELD_IN,PFIELD_OUT)

IMPLICIT NONE
 
INTEGER, INTENT(IN) :: KLONG
REAL, INTENT(IN) :: PSEUIL
REAL, DIMENSION(:), INTENT(IN) :: PFIELD_IN
REAL, DIMENSION(:), INTENT(OUT) :: PFIELD_OUT
INTEGER :: ICPT, I, K

ICPT = 0

I = 1

PFIELD_OUT(:) = 0.
!
! boucle sur les colonnes
DO 
           
  ! si on a dépassé la dernière colonne, on sort de la boucle
  IF (ICPT>=KLONG) EXIT

  ! si la valeur est valide
  IF (PFIELD_IN(I)<PSEUIL) THEN

    ! on la met dans lwrite à l'indice icpt
    ICPT = ICPT + 1
    PFIELD_OUT(ICPT) = PFIELD_IN(I)
!!!!!!!!!!!!!!!!!!!!test temporary: to remove after
    IF (MOD(PFIELD_OUT(ICPT),100.)==0) PFIELD_OUT(ICPT)=0.
!!!!!!!!!!!!!!!!!!!!!! to remove after
    ! on incrémente i
    I = I+1

  ELSE

    !ideb = icpt + 1
    DO K = 1,NINT(PFIELD_IN(I)-(PSEUIL))
      ICPT = ICPT + 1
      IF (ICPT>KLONG) EXIT
      PFIELD_OUT(ICPT) = 0.
    ENDDO

    I = I+1

  ENDIF

ENDDO

END SUBROUTINE UNCOMPRESS_FIELD
