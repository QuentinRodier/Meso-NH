!     #########
      SUBROUTINE WRITESURF_COVER_n(HPROGRAM)
!     #################################
!
!!****  *WRITESURF_COVER_n* - writes cover fields
!!
!!    PURPOSE
!!    -------
!!       
!!
!!
!!**  METHOD
!!    ------
!!      
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
USE MODD_SURF_ATM_n,     ONLY : XSEA, XWATER, XNATURE, XTOWN, XCOVER, LCOVER, &
                                XZS, LECOCLIMAP
!
USE MODD_DATA_COVER_PAR, ONLY : JPCOVER
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
!*       1.     Cover classes :
!               -------------
!
IF (LHOOK) CALL DR_HOOK('WRITESURF_COVER_N',0,ZHOOK_HANDLE)
!
YCOMMENT = '(-)'
 CALL WRITE_SURF(HPROGRAM,'FRAC_SEA   ',XSEA,   IRESP,HCOMMENT=YCOMMENT)
 CALL WRITE_SURF(HPROGRAM,'FRAC_NATURE',XNATURE,IRESP,HCOMMENT=YCOMMENT)
 CALL WRITE_SURF(HPROGRAM,'FRAC_WATER ',XWATER, IRESP,HCOMMENT=YCOMMENT)
 CALL WRITE_SURF(HPROGRAM,'FRAC_TOWN  ',XTOWN,  IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='COVER_LIST'
YCOMMENT='(LOGICAL LIST)'
 CALL WRITE_SURF(HPROGRAM,YRECFM,LCOVER(:),IRESP,HCOMMENT=YCOMMENT,HDIR='-')
!
YCOMMENT='COVER FIELDS'
 CALL WRITE_SURF(HPROGRAM,'COVER',XCOVER(:,:),LCOVER,IRESP,HCOMMENT=YCOMMENT)
!
!-------------------------------------------------------------------------------
!
!*       2.     Orography :
!               ---------
!
YRECFM='ZS'
YCOMMENT='X_Y_ZS (M)'
 CALL WRITE_SURF(HPROGRAM,YRECFM,XZS(:),IRESP,HCOMMENT=YCOMMENT)
!
IF (LHOOK) CALL DR_HOOK('WRITESURF_COVER_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE WRITESURF_COVER_n
