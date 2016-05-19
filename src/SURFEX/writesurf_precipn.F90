!     #########
      SUBROUTINE WRITESURF_PRECIP_n(HPROGRAM)
!     #######################################
!
!!****  *WRITESURF_PRECIP_n* - routine to write precip fields into the restart file 
!!
!!    PURPOSE
!!    -------
!!       The purpose of this routine is to store the 
!!       precip field into the restart file . Indeed, 
!!       when ARPEGE/ALADIN is used, the precip field 
!!       is not initialize at the begin of a run.
!!
!!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!      
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
USE MODD_SURF_ATM_n,         ONLY : NSIZE_FULL
!
USE MODD_SURF_ATM,           ONLY : LRW_PRECIP,LSAVE_PRECIP
USE MODD_DIAG_SURF_ATM_n,    ONLY : XRW_RAIN, XRW_SNOW
!
USE MODI_WRITE_SURF
!
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
!*       0.2   Declarations of local variables
!              -------------------------------
!

!
INTEGER           :: IRESP          ! Error code after redding
 CHARACTER(LEN=12) :: YRECFM         ! Name of the article to be read
 CHARACTER(LEN=100):: YCOMMENT       ! Comment string
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('WRITESURF_PRECIP_N',0,ZHOOK_HANDLE)
YCOMMENT='flag to store precip fields in restart file'
 CALL WRITE_SURF(HPROGRAM,'RW_PRECIP',LRW_PRECIP,IRESP,HCOMMENT=YCOMMENT)
!
IF(LRW_PRECIP.AND.LSAVE_PRECIP)THEN
!
   YRECFM='RW_RAIN'
   YCOMMENT='X_Y_RAINFALL (kg/m2/s)'
   CALL WRITE_SURF(HPROGRAM,YRECFM,XRW_RAIN(:),IRESP,HCOMMENT=YCOMMENT)
!
   YRECFM='RW_SNOW'
   YCOMMENT='X_Y_SNOWFALL (kg/m2/s)'
   CALL WRITE_SURF(HPROGRAM,YRECFM,XRW_SNOW(:),IRESP,HCOMMENT=YCOMMENT)
!
ENDIF
IF (LHOOK) CALL DR_HOOK('WRITESURF_PRECIP_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE WRITESURF_PRECIP_n
