!     #########
      SUBROUTINE WRITESURF_DUMMY_n(HPROGRAM)
!     ##########################################
!
!!****  *WRITESURF_DUMMY_n* - routine to write dummy surface fields
!!
!!    PURPOSE
!!    -------
!!
!!    AUTHOR
!!    ------
!!	V. Masson   *Meteo France*	
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    03/2004
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_DUMMY_SURF_FIELDS_n, ONLY : NDUMMY_NBR,  CDUMMY_NAME,    &
                                       CDUMMY_AREA, XDUMMY_FIELDS  
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
 CHARACTER(LEN=6), INTENT(IN) :: HPROGRAM     ! 
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER           :: JDUMMY         ! loop counter
!
 CHARACTER(LEN=20 ):: YSTRING20      ! string
 CHARACTER(LEN=3  ):: YSTRING03      ! string
!
INTEGER           :: IRESP          ! IRESP  : return-code if a problem appears
 CHARACTER(LEN=12) :: YRECFM         ! Name of the article to be read
 CHARACTER(LEN=100):: YCOMMENT       ! Comment string
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!*       1.     Number of dummy fields :
!               ----------------------
!
IF (LHOOK) CALL DR_HOOK('WRITESURF_DUMMY_N',0,ZHOOK_HANDLE)
YRECFM='DUMMY_GR_NBR'
YCOMMENT=' '
!
 CALL WRITE_SURF(HPROGRAM,YRECFM,NDUMMY_NBR,IRESP,HCOMMENT=YCOMMENT)
!
!-------------------------------------------------------------------------------
!
!*       2.     Dummy fields :
!               ------------
!
DO JDUMMY=1,NDUMMY_NBR
  !
  WRITE(YRECFM,'(A8,I3.3,A5)') 'DUMMY_GR',JDUMMY,'     '
  YSTRING20=CDUMMY_NAME(JDUMMY)
  YSTRING03=CDUMMY_AREA(JDUMMY)
  YCOMMENT='X_Y_'//YRECFM//YSTRING20//YSTRING03//  &
             '                                                             '  
  CALL WRITE_SURF(HPROGRAM,YRECFM,XDUMMY_FIELDS(:,JDUMMY),IRESP,HCOMMENT=YCOMMENT)
END DO
IF (LHOOK) CALL DR_HOOK('WRITESURF_DUMMY_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE WRITESURF_DUMMY_n
