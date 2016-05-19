!     ####################################
      SUBROUTINE WRITESURF_SSO_CANOPY_n(HPROGRAM,HWRITE,OWRITE)
!     ####################################
!
!!****  *WRITE_SSO_n* - writes SSO fields
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
!!      E. Martin   01/2012 avoid write of XUNDEF fields
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
!
USE MODD_SSO_CANOPY_n,   ONLY : NLVL, XZ, XU, XTKE
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
 CHARACTER(LEN=3),  INTENT(IN)  :: HWRITE   ! 'PREP' : does not write SBL XUNDEF fields
!                                          ! 'ALL' : all fields are written
LOGICAL,           INTENT(IN)  :: OWRITE   ! flag to write canopy terms
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER           :: IRESP          ! IRESP  : return-code if a problem appears
 CHARACTER(LEN=12) :: YRECFM         ! Name of the article to be read
 CHARACTER(LEN=100):: YCOMMENT       ! Comment string
!
INTEGER :: JLAYER  ! loop counter on layers
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!*       1.     Prognostic fields:
!               -----------------
!
!
!* flag to define if canopy is computed
!
IF (LHOOK) CALL DR_HOOK('WRITESURF_SSO_CANOPY_N',0,ZHOOK_HANDLE)
YRECFM='SSO_CANOPY'
YCOMMENT='flag to use canopy levels'
 CALL WRITE_SURF(HPROGRAM,YRECFM,OWRITE,IRESP,HCOMMENT=YCOMMENT)
!
IF (.NOT. OWRITE .AND. LHOOK) CALL DR_HOOK('WRITESURF_SSO_CANOPY_N',1,ZHOOK_HANDLE)
IF (.NOT. OWRITE) RETURN
!
!* number of levels
!
YRECFM='SSO_CAN_LVL'
YCOMMENT='number of canopy levels'
 CALL WRITE_SURF(HPROGRAM,YRECFM,NLVL,IRESP,HCOMMENT=YCOMMENT)
!
!* altitudes
!
DO JLAYER=1,NLVL
  WRITE(YRECFM,'(A9,I2.2,A1)') 'SSO_CAN_Z',JLAYER,' '
  YCOMMENT='altitudes of canopy levels (m)'
  CALL WRITE_SURF(HPROGRAM,YRECFM,XZ(:,JLAYER),IRESP,HCOMMENT=YCOMMENT)
END DO
!
IF (HWRITE/='PRE') THEN
  !
  !* wind in canopy
  !
  DO JLAYER=1,NLVL
    WRITE(YRECFM,'(A9,I2.2,A1)') 'SSO_CAN_U',JLAYER,' '
    YCOMMENT='wind at canopy levels (m/s)'
    CALL WRITE_SURF(HPROGRAM,YRECFM,XU(:,JLAYER),IRESP,HCOMMENT=YCOMMENT)
  END DO
  !
  !* Tke in canopy
  !
  DO JLAYER=1,NLVL
    WRITE(YRECFM,'(A9,I2.2,A1)') 'SSO_CAN_E',JLAYER,' '
    YCOMMENT='Tke at canopy levels (m2/s2)'
    CALL WRITE_SURF(HPROGRAM,YRECFM,XTKE(:,JLAYER),IRESP,HCOMMENT=YCOMMENT)
  END DO
  !
ENDIF
!
IF (LHOOK) CALL DR_HOOK('WRITESURF_SSO_CANOPY_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE WRITESURF_SSO_CANOPY_n
