!-----------------------------------------------------------------
!     ##########################
      SUBROUTINE READ_FILE_ISBAMAP(KUNIT,PVAR,INI)
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
USE MODD_SURF_PAR, ONLY : XUNDEF
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
REAL, DIMENSION(:), INTENT(OUT) :: PVAR   ! variable to write in the file
INTEGER, INTENT(IN)             :: INI    ! Grid dimensions
!
!
!*      0.2    declarations of local variables
INTEGER                    :: JJ,JI
INTEGER                    :: JINDEX ! reference number of the pixel
REAL                       :: ZOUT
REAL                       :: ZMAX,ZMIN
REAL, DIMENSION(INI)       :: ZXI, ZYI   ! natural coordinates of ISBA grid (conformal projection)
REAL, DIMENSION(INI)       :: ZDXI, ZDYI   ! Isba grid resolution in the conformal projection
INTEGER                    :: IIMAX,IJMAX
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('READ_FILE_ISBAMAP',0,ZHOOK_HANDLE)
!
!*       0.     Initialization:
!               ---------------
!
 CALL GET_GRIDTYPE_CONF_PROJ(XGRID_PAR,PX=ZXI,PY=ZYI,KIMAX=IIMAX,KJMAX=IJMAX,PDX=ZDXI)
!
ZMAX=MAXVAL(PVAR)
ZMIN=MINVAL(PVAR)
ZOUT = XUNDEF
!
DO JJ=1,5
  READ(KUNIT,*)
ENDDO
!
READ(KUNIT,*) ZXI(1)
READ(KUNIT,*) ZYI(1)
READ(KUNIT,*) IIMAX
READ(KUNIT,*) IJMAX
READ(KUNIT,*) ZOUT
READ(KUNIT,*) ZDXI(1)
READ(KUNIT,*) ZMIN
READ(KUNIT,*) ZMAX
!
DO JJ=1,IJMAX
  DO JI=1,IIMAX
    JINDEX=(JJ - 1) * IIMAX + JI
    READ(KUNIT,*) PVAR(JINDEX)
  ENDDO
ENDDO
!
IF (LHOOK) CALL DR_HOOK('READ_FILE_ISBAMAP',1,ZHOOK_HANDLE)
!
END SUBROUTINE READ_FILE_ISBAMAP
