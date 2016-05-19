!     #########
      SUBROUTINE RW_PRECIP_n(HPROGRAM,PRAIN,PSNOW)
!     ############################################
!
!!****  *RW_PRECIP_n* - Initialize/Save precip field for a ARPEGE/ALADIN run
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!
!!    EXTERNAL
!!    --------
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
!!	B. Decharme   *Meteo France*	
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    04/2009
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
USE MODD_SURF_ATM,       ONLY : LRW_PRECIP
USE MODD_SURF_ATM_n,     ONLY : LINIT_PRECIP
USE MODD_DIAG_SURF_ATM_n,ONLY : XRW_RAIN, XRW_SNOW
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! calling program
!
REAL, DIMENSION(:), INTENT(INOUT) :: PRAIN     ! liquid precipitation   (kg/m2/s)
REAL, DIMENSION(:), INTENT(INOUT) :: PSNOW     ! snow precipitation     (kg/m2/s)
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('RW_PRECIP_N',0,ZHOOK_HANDLE)
IF(LRW_PRECIP)THEN
   IF(LINIT_PRECIP)THEN
     PRAIN(:)=XRW_RAIN(:)
     PSNOW(:)=XRW_SNOW(:)
     LINIT_PRECIP=.FALSE.
   ELSE
     XRW_RAIN(:)=PRAIN(:)
     XRW_SNOW(:)=PSNOW(:)
   ENDIF
ENDIF
IF (LHOOK) CALL DR_HOOK('RW_PRECIP_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE RW_PRECIP_n
