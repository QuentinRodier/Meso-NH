!     ###########################################################
      SUBROUTINE ZOOM_PGD_INLAND_WATER(HPROGRAM,HINIFILE,HINIFILETYPE,HFILE,HFILETYPE)
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
USE MODD_SURF_ATM_n,       ONLY : CWATER
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_PGD_FLAKE
USE MODI_PGD_WATFLUX
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
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
!------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('ZOOM_PGD_INLAND_WATER',0,ZHOOK_HANDLE)
IF (CWATER=='NONE  ') THEN
  IF (LHOOK) CALL DR_HOOK('ZOOM_PGD_INLAND_WATER',1,ZHOOK_HANDLE)
  RETURN
ELSE IF (CWATER=='FLUX  ') THEN
  IF (LHOOK) CALL DR_HOOK('ZOOM_PGD_INLAND_WATER',1,ZHOOK_HANDLE)
  RETURN
ELSE IF (CWATER=='WATFLX') THEN
  CALL PGD_WATFLUX(HPROGRAM)
ELSE IF (CWATER=='FLAKE ') THEN
  CALL PGD_FLAKE(HPROGRAM)
END IF
IF (LHOOK) CALL DR_HOOK('ZOOM_PGD_INLAND_WATER',1,ZHOOK_HANDLE)
!
!_______________________________________________________________________________
!
END SUBROUTINE ZOOM_PGD_INLAND_WATER
