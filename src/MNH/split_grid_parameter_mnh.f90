!MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!     #############################################################
      SUBROUTINE SPLIT_GRID_PARAMETERX1_MNH(HGRID,HREC,KDIM,KSIZE,PFIELD,PFIELD_SPLIT)
!     #############################################################
!
!!****  * - routine to split a real array on the splitted grid 
!
!
USE MODD_IO_SURF_MNH, ONLY : NHALO
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
CHARACTER(LEN=10),      INTENT(IN) :: HGRID       ! grid type
CHARACTER(LEN=6),       INTENT(IN) :: HREC        ! name of the parameter
INTEGER,                INTENT(IN) :: KDIM        ! size of PFIELD
INTEGER,                INTENT(IN) :: KSIZE       ! size of PFIELD_SPLIT
REAL, DIMENSION(KDIM ), INTENT(IN) :: PFIELD      ! real field for complete grid
REAL, DIMENSION(KSIZE), INTENT(OUT):: PFIELD_SPLIT! real field for splitted grid
!
!*      0.2   Declarations of local variables
!
INTEGER :: JI, JJ
INTEGER :: IIB, IIE, IJB, IJE
INTEGER :: NIMAX_ll, NJMAX_ll
INTEGER :: IXOR_ll, IYOR_ll
INTEGER :: NIMAX, NJMAX
!
REAL, DIMENSION(:), ALLOCATABLE :: ZX, ZY
REAL                            :: ZDX, ZDY
!
INTEGER :: IINDEX
!-------------------------------------------------------------------------------
!
CALL GET_INDICE_ll (IIB,IJB,IIE,IJE)
CALL GET_GLOBALDIMS_ll (NIMAX_ll,NJMAX_ll)
CALL GET_OR_ll('B',IXOR_ll,IYOR_ll)
NIMAX=IIE-IIB+1
NJMAX=IJE-IJB+1
!

!
IF (HREC=='XX' .OR. HREC=='DX') THEN
  ALLOCATE(ZX(NIMAX_ll+2*NHALO))
  ZX(1+NHALO:NIMAX_ll+NHALO) = PFIELD(1:NIMAX_ll)
  IF (HREC=='DX') THEN
    ZDX = PFIELD(1)
    DO JI=NHALO,1,-1
      ZX(JI) = ZDX
      ZX(NIMAX_ll+2*NHALO-JI+1) = ZDX
    END DO
  ELSE IF (HREC=='XX') THEN
    ZDX = 0.
    IF (NIMAX_ll>1) ZDX = PFIELD(2) - PFIELD(1)
    IF (NIMAX_ll==1) ZDX = PFIELD(1) ! in 1D conf, one assumes that grid
                                     ! is located between X=DX/2 and X=3DX/2
    DO JI=NHALO,1,-1
      ZX(JI) = ZX(JI+1) - ZDX
      ZX(NIMAX_ll+2*NHALO-JI+1) = ZX(NIMAX_ll+2*NHALO-JI) + ZDX
    END DO
  END IF

!
  DO JJ=1,NJMAX+2*NHALO
    DO JI=1,NIMAX+2*NHALO
      IINDEX = JI+(JJ-1)*(NIMAX+2*NHALO)
      PFIELD_SPLIT(IINDEX) = ZX(JI+IXOR_ll-1)
    END DO
  END DO
  DEALLOCATE(ZX)
  
ELSEIF (HREC=='YY' .OR. HREC=='DY') THEN
  ALLOCATE(ZY(NJMAX_ll+2*NHALO))
  DO JJ=1+NHALO,NJMAX_ll+NHALO
    ZY(JJ) = PFIELD(1+(JJ-1-NHALO)*NIMAX_ll)
  END DO
  IF (HREC=='DY') THEN
    ZDY = PFIELD(1)
    DO JJ=NHALO,1,-1
      ZY(JJ) = ZDY
      ZY(NJMAX_ll+2*NHALO-JJ+1) = ZDY
    END DO
  ELSE IF (HREC=='YY') THEN
    ZDY = 0.
    IF (NJMAX_ll>1) ZDY = PFIELD(1+NIMAX_ll) - PFIELD(1)
    IF (NJMAX_ll==1) ZDY = PFIELD(1) ! in 1D or 2D conf, one assumes that grid
                                     ! is located between Y=DY/2 and Y=3DY/2
    DO JJ=NHALO,1,-1
      ZY(JJ) = ZY(JJ+1) - ZDY
      ZY(NJMAX_ll+2*NHALO-JJ+1) = ZY(NJMAX_ll+2*NHALO-JJ) + ZDY
    END DO
  END IF

  DO JJ=1,NJMAX+2*NHALO
    DO JI=1,NIMAX+2*NHALO
      IINDEX = JI+(JJ-1)*(NIMAX+2*NHALO)
      PFIELD_SPLIT(IINDEX) = ZY(JJ+IYOR_ll-1)
    END DO
  END DO
  DEALLOCATE(ZY)

END IF  
!
!-------------------------------------------------------------------------------
END SUBROUTINE SPLIT_GRID_PARAMETERX1_MNH
!
!
!     #############################################################
      SUBROUTINE SPLIT_GRID_PARAMETERN0_MNH(HGRID,HREC,KFIELD,KFIELD_SPLIT)
!     #############################################################
!
!!****  * - routine to define an integer related to splitted grid
!
!
!
USE MODE_ll
!
USE MODD_IO_SURF_MNH, ONLY : NHALO
!
IMPLICIT NONE
!
!*      0.1   Declarations of arguments
!
CHARACTER(LEN=10), INTENT(IN) :: HGRID        ! grid type
CHARACTER(LEN=6),  INTENT(IN) :: HREC         ! name of the parameter
INTEGER,           INTENT(IN) :: KFIELD       ! integer scalar for complete grid
INTEGER,           INTENT(OUT):: KFIELD_SPLIT ! integer scalar for splitted grid
!*      0.2   Declarations of local variables
!
INTEGER :: IIB, IIE, IJB, IJE
!-------------------------------------------------------------------------------
!
CALL GET_INDICE_ll (IIB,IJB,IIE,IJE)
!
IF (HREC=='IMAX') KFIELD_SPLIT = IIE-IIB+1 + 2*NHALO
IF (HREC=='JMAX') KFIELD_SPLIT = IJE-IJB+1 + 2*NHALO
!
!-------------------------------------------------------------------------------
END SUBROUTINE SPLIT_GRID_PARAMETERN0_MNH
