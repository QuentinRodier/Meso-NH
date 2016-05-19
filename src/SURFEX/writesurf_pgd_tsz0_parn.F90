!     #########
      SUBROUTINE WRITESURF_PGD_TSZ0_PAR_n(HPROGRAM)
!     ################################################
!
!!****  *WRITESURF_PGD_TSZ0_PAR_n* - writes TSZ0 physiographic fields
!!                        
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
!!      P. Le Moigne 12/2004 : add type of photosynthesis 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_DATA_TSZ0_n,    ONLY : NTIME, XDATA_DTS, XDATA_DHUGRD   
!
USE MODI_WRITE_SURF
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
IF (LHOOK) CALL DR_HOOK('WRITESURF_PGD_TSZ0_PAR_N',0,ZHOOK_HANDLE)
!
NTIME = SIZE(XDATA_DTS)
YRECFM   = 'ND_TSZ0_TIME'
YCOMMENT = '(-)'
 CALL WRITE_SURF(HPROGRAM,YRECFM,NTIME,IRESP,HCOMMENT=YCOMMENT)
!
YRECFM   = 'D_DTS'
YCOMMENT = 'X_Y_DATA_DTS'
 CALL WRITE_SURF(HPROGRAM,YRECFM,XDATA_DTS(:),IRESP,HCOMMENT=YCOMMENT,HDIR='-')
!
YRECFM   = 'D_DHUGRD'
YCOMMENT = 'X_Y_DATA_DHUGRD'
 CALL WRITE_SURF(HPROGRAM,YRECFM,XDATA_DHUGRD(:),IRESP,HCOMMENT=YCOMMENT,HDIR='-')
!
IF (LHOOK) CALL DR_HOOK('WRITESURF_PGD_TSZ0_PAR_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE WRITESURF_PGD_TSZ0_PAR_n
