!     #########
      SUBROUTINE GET_NEAR_MESHES_IGN(KGRID_PAR,KL,PGRID_PAR,KNEAR_NBR,KNEAR)
!     ##############################################################
!
!!**** *GET_NEAR_MESHES_IGN* get the near grid mesh indices
!!
!!    PURPOSE
!!    -------
!!
!!    METHOD
!!    ------
!!   
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!
!!    V. Masson         Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    03/2004
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODE_GRIDTYPE_IGN
!
USE MODD_SURF_PAR, ONLY : XUNDEF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
INTEGER,                         INTENT(IN)    :: KGRID_PAR ! size of PGRID_PAR
INTEGER,                         INTENT(IN)    :: KL        ! number of points
INTEGER,                         INTENT(IN)    :: KNEAR_NBR ! number of nearest points wanted
REAL,    DIMENSION(KGRID_PAR),   INTENT(IN)    :: PGRID_PAR ! grid parameters
INTEGER, DIMENSION(:,:),POINTER :: KNEAR    ! near mesh indices
!
!*    0.2    Declaration of other local variables
!            ------------------------------------
!
REAL, DIMENSION(KL) :: ZDIS
REAL,DIMENSION(KL)  :: ZX
REAL,DIMENSION(KL)  :: ZY
REAL,DIMENSION(KL)  :: ZDX
REAL,DIMENSION(KL)  :: ZDY
REAL :: ZDMAX
INTEGER :: ID0
INTEGER :: JP1, JP2, JN
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!----------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('GET_NEAR_MESHES_IGN',0,ZHOOK_HANDLE)
!
 CALL GET_GRIDTYPE_IGN(PGRID_PAR,PX=ZX,PY=ZY,PDX=ZDX,PDY=ZDY)
!
KNEAR  (:,:) = 0
!
! calcul de la distance de tous les points 2 à 2
!
ZDIS = XUNDEF
!
DO JP1=1,KL

  DO JP2=1,KL
    ZDIS(JP2) = SQRT((ZX(JP1)-ZX(JP2))**2+(ZY(JP1)-ZY(JP2))**2)
  ENDDO
  ZDMAX = MAXVAL(ZDIS(:)) + 1.
  ZDIS(JP1) = ZDMAX
  !
  ! on prend les knear_nbr premiers, pour chaque
  !
  DO JN=1,MIN(KL-1,KNEAR_NBR)
    !
    ID0 = 0
    !
    ID0 = MAXVAL(MINLOC(ZDIS(:)))        
    !
    KNEAR(JP1,JN) = ID0
    ZDIS(ID0) = ZDMAX
    !
  ENDDO
  !
ENDDO
!
IF (LHOOK) CALL DR_HOOK('GET_NEAR_MESHES_IGN',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE GET_NEAR_MESHES_IGN
