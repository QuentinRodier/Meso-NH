!     #########
      SUBROUTINE WRITESURF_FLAKE_n(HPROGRAM)
!     ########################################
!
!!****  *WRITESURF_FLAKE_n* - writes FLAKE fields
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
!!	V. Masson   *Meteo France*	
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2003 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_FLAKE_n,  ONLY : XTS, TTIME    , &
                            XT_SNOW       , &
                            XT_ICE        , &
                            XT_MNW        , &
                            XT_WML        , &
                            XT_BOT        , &
                            XT_B1         , &
                            XCT           , &
                            XH_SNOW       , &
                            XH_ICE        , &
                            XH_ML         , &
                            XH_B1         , &
                            XZ0           , &
                            XUSTAR  
!
USE MODI_WRITE_SURF
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
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! program calling

!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER           :: IRESP          ! IRESP  : return-code if a problem appears
 CHARACTER(LEN=12) :: YRECFM         ! Name of the article to be read
 CHARACTER(LEN=100):: YCOMMENT       ! Comment string
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!
!*       3.     Prognostic fields:
!               -----------------
!
!* water temperature
!
IF (LHOOK) CALL DR_HOOK('WRITESURF_FLAKE_N',0,ZHOOK_HANDLE)
YRECFM='TS_WATER'
YCOMMENT='TS_WATER (K)'
 CALL WRITE_SURF(HPROGRAM,YRECFM,XTS(:),IRESP,HCOMMENT=YCOMMENT)


YRECFM='T_SNOW'
YCOMMENT='T_SNOW (K)'
 CALL WRITE_SURF(HPROGRAM,YRECFM,XT_SNOW(:),IRESP,HCOMMENT=YCOMMENT)
YRECFM='T_ICE'
YCOMMENT='T_ICE (K)'
 CALL WRITE_SURF(HPROGRAM,YRECFM,XT_ICE(:),IRESP,HCOMMENT=YCOMMENT)
YRECFM='T_MNW'
YCOMMENT='T_WATER_MEAN (K)'
 CALL WRITE_SURF(HPROGRAM,YRECFM,XT_MNW(:),IRESP,HCOMMENT=YCOMMENT)
YRECFM='T_WML'
YCOMMENT='T_WATER_ML (K)'
 CALL WRITE_SURF(HPROGRAM,YRECFM,XT_WML(:),IRESP,HCOMMENT=YCOMMENT)
YRECFM='T_BOT'
YCOMMENT='T_WATER_BOT (K)'
 CALL WRITE_SURF(HPROGRAM,YRECFM,XT_BOT(:),IRESP,HCOMMENT=YCOMMENT)
YRECFM='T_B1'
YCOMMENT='T_B1 (K)'
 CALL WRITE_SURF(HPROGRAM,YRECFM,XT_B1(:),IRESP,HCOMMENT=YCOMMENT)
YRECFM='CT'
YCOMMENT='C_SHAPE_FACTOR ()'
 CALL WRITE_SURF(HPROGRAM,YRECFM,XCT(:),IRESP,HCOMMENT=YCOMMENT)
YRECFM='H_SNOW'
YCOMMENT='H_SNOW (m)'
 CALL WRITE_SURF(HPROGRAM,YRECFM,XH_SNOW(:),IRESP,HCOMMENT=YCOMMENT)
YRECFM='H_ICE'
YCOMMENT='H_ICE (m)'
 CALL WRITE_SURF(HPROGRAM,YRECFM,XH_ICE(:),IRESP,HCOMMENT=YCOMMENT)
YRECFM='H_ML'
YCOMMENT='H_ML (m)'
 CALL WRITE_SURF(HPROGRAM,YRECFM,XH_ML(:),IRESP,HCOMMENT=YCOMMENT)
YRECFM='H_B1'
YCOMMENT='H_B1 (m)'
 CALL WRITE_SURF(HPROGRAM,YRECFM,XH_B1(:),IRESP,HCOMMENT=YCOMMENT)

!
!-------------------------------------------------------------------------------
!
!*       4.     Semi-prognostic fields:
!               ----------------------
!
!* roughness length
!
YRECFM='Z0WATER'
YCOMMENT='Z0WATER (m)'
 CALL WRITE_SURF(HPROGRAM,YRECFM,XZ0(:),IRESP,HCOMMENT=YCOMMENT)
!
!* friction velocity
!
YRECFM='USTAR_WATER'
YCOMMENT='USTAR_WATER (m/s)'
 CALL WRITE_SURF(HPROGRAM,YRECFM,XUSTAR(:),IRESP,HCOMMENT=YCOMMENT)
!
!
!-------------------------------------------------------------------------------
!
!*       5.  Time
!            ----
!
YRECFM='DTCUR'
YCOMMENT='s'
 CALL WRITE_SURF(HPROGRAM,YRECFM,TTIME,IRESP,HCOMMENT=YCOMMENT)
IF (LHOOK) CALL DR_HOOK('WRITESURF_FLAKE_N',1,ZHOOK_HANDLE)
!

!-------------------------------------------------------------------------------
!
END SUBROUTINE WRITESURF_FLAKE_n
