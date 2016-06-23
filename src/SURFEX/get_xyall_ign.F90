!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ################################################################
SUBROUTINE GET_XYALL_IGN(PX,PY,PDX,PDY,PXALL,PYALL,KDIMX,KDIMY)
!     ################################################################
!
!!****  *GET_XYALL_IGN* 
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!      S. Faroux   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    07/2011 
!-------------------------------------------------------------------------------
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
REAL, DIMENSION(:), INTENT(IN) :: PX
REAL, DIMENSION(:), INTENT(IN) :: PY
REAL, DIMENSION(:), INTENT(IN) :: PDX
REAL, DIMENSION(:), INTENT(IN) :: PDY
REAL, DIMENSION(:), INTENT(OUT) :: PXALL
REAL, DIMENSION(:), INTENT(OUT) :: PYALL
INTEGER, INTENT(OUT) :: KDIMX
INTEGER, INTENT(OUT) :: KDIMY
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
REAL, DIMENSION(MAX(SIZE(PXALL),SIZE(PYALL))*3) :: ZALL
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!----------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('GET_XYALL_IGN',0,ZHOOK_HANDLE)
!
KDIMX = 0
KDIMY = 0
!
 CALL GET_COORD(PX,PDX,ZALL,KDIMX)
!sort values from lower to grower
 CALL SORT(KDIMX,ZALL,PXALL)
!
 CALL GET_COORD(PY,PDY,ZALL,KDIMY)
!sort values from lower to grower
 CALL SORT(KDIMY,ZALL,PYALL)
!
IF (LHOOK) CALL DR_HOOK('GET_XYALL_IGN',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
 CONTAINS
!
SUBROUTINE GET_COORD(PIN,PDIN,POUT,KSIZE)
!
IMPLICIT NONE
!
REAL, DIMENSION(:), INTENT(IN) :: PIN
REAL, DIMENSION(:), INTENT(IN) :: PDIN
REAL, DIMENSION(:), INTENT(OUT) :: POUT
INTEGER, INTENT(INOUT) :: KSIZE
REAL, DIMENSION(SIZE(POUT)) :: ZDOUT
INTEGER :: I, J
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('GET_XYALL_IGN:GET_COORD',0,ZHOOK_HANDLE)
!
ZDOUT(:) = 0.
POUT (:) = -9999.
IF (SIZE(POUT)>0) THEN
  ZDOUT(1) = PDIN(1)/2.
  POUT (1) = PIN(1)
  IF (SIZE(POUT)>1) POUT (2) = PIN(1)-PDIN(1)
  IF (SIZE(POUT)>2) POUT (3) = PIN(1)+PDIN(1)
ENDIF
KSIZE = MIN(3,SIZE(PIN))
!
DO I=1,SIZE(PIN)
  !point
  DO J=1,KSIZE
    IF ( POUT(J) == PIN(I) ) EXIT
    IF ( J == KSIZE ) THEN
      KSIZE = KSIZE + 1
      POUT (KSIZE) = PIN (I)
      ZDOUT(KSIZE) = PDIN(I)/2.
    ENDIF
  ENDDO
  !limits of the mesh
   DO J=1,KSIZE
     IF ( POUT(J)<PIN(I) .AND. POUT(J)+ZDOUT(J)>=PIN(I)-PDIN(I) ) EXIT
     IF ( J == KSIZE ) THEN     
       KSIZE = KSIZE + 1
       POUT(KSIZE) = PIN(I)-PDIN(I)
     ENDIF
   ENDDO
   DO J=1,KSIZE
     IF ( POUT(J)>PIN(I) .AND. POUT(J)-ZDOUT(J)<=PIN(I)+PDIN(I) ) EXIT
     IF ( J == KSIZE ) THEN     
       KSIZE = KSIZE + 1
       POUT(KSIZE) = PIN(I)+PDIN(I)
     ENDIF
   ENDDO   
ENDDO
!
IF (LHOOK) CALL DR_HOOK('GET_XYALL_IGN:GET_COORD',1,ZHOOK_HANDLE)
!
END SUBROUTINE GET_COORD
!
SUBROUTINE SORT(KSIZE,PIN,POUT)
!
IMPLICIT NONE
!
INTEGER, INTENT(INOUT) :: KSIZE
REAL, DIMENSION(:), INTENT(INOUT) :: PIN
REAL, DIMENSION(:), INTENT(OUT) :: POUT
REAL, DIMENSION(SIZE(PIN)) :: ZOUT
REAL :: ZMIN, ZMAX
INTEGER,DIMENSION(1) :: IDMIN
INTEGER :: J
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('GET_XYALL_IGN:SORT',0,ZHOOK_HANDLE)
!
ZMAX  = MAXVAL(PIN(1:KSIZE))
DO J=1,KSIZE
  ZMIN  = MINVAL(PIN(1:KSIZE))
  ZOUT(J) = ZMIN
  IDMIN = MINLOC(PIN(1:KSIZE))
  PIN(IDMIN(1)) = ZMAX+1
ENDDO
!
!to suppress fictive points at boundaries
POUT(1:KSIZE-2) = ZOUT(2:KSIZE-1)
KSIZE = MAX(0,KSIZE-2)
!
IF (LHOOK) CALL DR_HOOK('GET_XYALL_IGN:SORT',1,ZHOOK_HANDLE)
!
END SUBROUTINE SORT
!
END SUBROUTINE GET_XYALL_IGN
