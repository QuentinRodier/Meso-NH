!-----------------------------------------------------------------
!     ##########################
      SUBROUTINE WRITE_FILE_ISBAMAP(KUNIT,PVAR,KI)
!     ##########################
!
!!
!!    PURPOSE
!!    -------
!        
!     
!!**  METHOD
!!    ------
!
!!    EXTERNAL
!!    --------
!!
!!    none
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------ 
!!      
!!    REFERENCE
!!    ---------
!!     
!!    AUTHOR
!!    ------
!!
!!      K. Chancibault	* Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!
!!      Original   25/01/2005
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
!
USE MODD_TOPODYN
USE MODD_SURF_ATM_GRID_n, ONLY : XGRID_PAR
USE MODD_SURF_PAR,        ONLY : XUNDEF
!
USE MODE_GRIDTYPE_CONF_PROJ
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
INTEGER, INTENT(IN)             :: KUNIT  ! file unit
REAL, DIMENSION(:), INTENT(IN)  :: PVAR   ! variable to write in the file
INTEGER, INTENT(IN)             :: KI    ! Grid dimensions
!
!
!*      0.2    declarations of local variables
INTEGER                    :: JJ,JI
INTEGER                    :: JINDEX ! reference number of the pixel
REAL                       :: ZOUT
REAL                       :: ZMAX,ZMIN
REAL, DIMENSION(KI)        :: ZXI, ZYI     ! natural coordinates of ISBA grid (conformal projection)
REAL, DIMENSION(KI)        :: ZDXI, ZDYI   ! Isba grid resolution in the conformal projection
INTEGER                    :: IIMAX,IJMAX
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('WRITE_FILE_ISBAMAP',0,ZHOOK_HANDLE)
!
!*       0.     Initialization:
!               ---------------
!
 CALL GET_GRIDTYPE_CONF_PROJ(XGRID_PAR,PX=ZXI,PY=ZYI,KIMAX=IIMAX,KJMAX=IJMAX,PDX=ZDXI)
!
ZMAX = MAXVAL(PVAR)
ZMIN = MINVAL(PVAR)
ZOUT = XUNDEF
!
DO JJ=1,5
  WRITE(KUNIT,*)
ENDDO
!
WRITE(KUNIT,*) ZXI(1)
WRITE(KUNIT,*) ZYI(1)
WRITE(KUNIT,*) IIMAX
WRITE(KUNIT,*) IJMAX
WRITE(KUNIT,*) ZOUT
WRITE(KUNIT,*) ZDXI(1)
WRITE(KUNIT,*) ZMIN
WRITE(KUNIT,*) ZMAX
!
DO JJ = 1,IJMAX
  DO JI = 1,IIMAX
    JINDEX = (JJ-1) * IIMAX + JI
    WRITE(KUNIT,*) PVAR(JINDEX)
  ENDDO
ENDDO
!
IF (LHOOK) CALL DR_HOOK('WRITE_FILE_ISBAMAP',1,ZHOOK_HANDLE)
!
END SUBROUTINE WRITE_FILE_ISBAMAP
