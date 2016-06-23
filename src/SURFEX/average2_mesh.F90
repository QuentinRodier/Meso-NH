!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE AVERAGE2_MESH(PPGDARRAY)
!     #########################################
!
!!**** *AVERAGE2_MESH* computes a PGD field
!!
!!    PURPOSE
!!    -------
!!
!!
!!    AUTHOR
!!    ------
!!
!!    V. Masson         Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    12/09/95
!!     V. Masson  03/2004  externalization
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_PGDWORK,        ONLY : NSIZE, XSUMVAL, CATYPE, &
                                NVALNBR, NVALCOUNT, XVALLIST
USE MODD_DATA_COVER_PAR, ONLY : XCDREF
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
REAL,    DIMENSION(:), INTENT(INOUT) :: PPGDARRAY ! Mesonh field
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*    0.2    Declaration of other local variables
!            ------------------------------------
!
INTEGER :: JLOOP ! loop counter on grid points
INTEGER :: JVAL  ! loop counter on values encountered in grid mesh
INTEGER :: IMAX  ! Maximum of times a value has been encountered in the grid mesh
INTEGER :: IVAL  ! Index of this value
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('AVERAGE2_MESH',0,ZHOOK_HANDLE)
SELECT CASE (CATYPE)

  CASE ('ARI')
  WHERE (NSIZE(:)/=0)
    PPGDARRAY(:)=XSUMVAL(:)/NSIZE(:)
  ENDWHERE

  CASE ('INV')
  WHERE (NSIZE(:)/=0)
    PPGDARRAY(:)=NSIZE(:)/XSUMVAL(:)
  ENDWHERE

  CASE ('CDN')
  WHERE (NSIZE(:)/=0)
    PPGDARRAY(:)=XCDREF/EXP(SQRT(NSIZE(:)/XSUMVAL(:)))
  ENDWHERE

  CASE ('MAJ')
  DO JLOOP=1,SIZE(NSIZE)
    IF(NSIZE(JLOOP)==0) CYCLE
    !* determines the index of the value which has been the most encountered
    !  in the grid mesh
    IMAX=0
    DO JVAL=1,NVALNBR(JLOOP)
      IF (NVALCOUNT(JLOOP,JVAL)>IMAX) THEN
        IMAX=NVALCOUNT(JLOOP,JVAL)
        IVAL = JVAL
      END IF
    END DO
    !* sets this value to the PGD field
    PPGDARRAY(JLOOP)=XVALLIST(JLOOP,IVAL)
  END DO

END SELECT
IF (LHOOK) CALL DR_HOOK('AVERAGE2_MESH',1,ZHOOK_HANDLE)

!-------------------------------------------------------------------------------
!
END SUBROUTINE AVERAGE2_MESH
