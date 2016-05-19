!     ###########################################################
      SUBROUTINE ZOOM_PGD_NATURE(HPROGRAM,HINIFILE,HINIFILETYPE,HFILE,HFILETYPE, &
                                   OECOCLIMAP                                      )  
!     ###########################################################

!!
!!    PURPOSE
!!    -------
!!   This program prepares the physiographic data fields.
!!
!!    METHOD
!!    ------
!!   
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!
!!    V. Masson                   Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original     13/10/03
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_SURF_ATM_n,       ONLY : CNATURE
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ZOOM_PGD_ISBA
!
IMPLICIT NONE
!
!*    0.1    Declaration of dummy arguments
!            ------------------------------
!
 CHARACTER(LEN=6),     INTENT(IN)  :: HPROGRAM    ! program calling
 CHARACTER(LEN=28),    INTENT(IN)  :: HINIFILE    ! input atmospheric file name
 CHARACTER(LEN=6),     INTENT(IN)  :: HINIFILETYPE! input atmospheric file type
 CHARACTER(LEN=28),    INTENT(IN)  :: HFILE       ! output file name
 CHARACTER(LEN=6),     INTENT(IN)  :: HFILETYPE   ! output file type
LOGICAL,              INTENT(IN)  :: OECOCLIMAP  ! flag to use ecoclimap
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
!------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('ZOOM_PGD_NATURE',0,ZHOOK_HANDLE)
IF (CNATURE=='NONE  ') THEN
  IF (LHOOK) CALL DR_HOOK('ZOOM_PGD_NATURE',1,ZHOOK_HANDLE)
  RETURN
ELSE IF (CNATURE=='FLUX  ') THEN
  IF (LHOOK) CALL DR_HOOK('ZOOM_PGD_NATURE',1,ZHOOK_HANDLE)
  RETURN
ELSE IF (CNATURE=='ISBA  ' .OR. CNATURE=='TSZ0') THEN
  CALL ZOOM_PGD_ISBA(HPROGRAM,HINIFILE,HINIFILETYPE,HFILE,HFILETYPE,OECOCLIMAP)
END IF
IF (LHOOK) CALL DR_HOOK('ZOOM_PGD_NATURE',1,ZHOOK_HANDLE)
!
!_______________________________________________________________________________
!
END SUBROUTINE ZOOM_PGD_NATURE
