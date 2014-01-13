!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
      SUBROUTINE GET_COVER_n(HPROGRAM,KI,KCOVER,PCOVER)
!     ########################################
!
!!****  *GET_COVER_n* - routine to get some surface fields
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
!!      Original    01/2004
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODI_GET_LUOUT
!
USE MODD_SURF_ATM_n,     ONLY : XCOVER
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABOR1_SFX
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
 CHARACTER(LEN=6),           INTENT(IN)  :: HPROGRAM
INTEGER,                    INTENT(IN)  :: KI      ! horizontal dim. of cover
INTEGER,                    INTENT(IN)  :: KCOVER  ! number of covers
REAL, DIMENSION(KI), INTENT(OUT) :: PCOVER  ! cover types
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER :: ILUOUT
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('GET_COVER_N',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!-------------------------------------------------------------------------------
!
IF ( SIZE(PCOVER) /= SIZE(XCOVER,1) ) THEN
  WRITE(ILUOUT,*) 'try to get COVER field from atmospheric model, but size is not correct'
  WRITE(ILUOUT,*) 'size of field expected by the atmospheric model (PCOVER) :', SIZE(PCOVER)
  WRITE(ILUOUT,*) 'size of field inthe surface                     (XCOVER) :', SIZE(XCOVER)
  CALL ABOR1_SFX('GET_COVERN: (1) COVER SIZE NOT CORRECT')
ELSE
  PCOVER=XCOVER(:,KCOVER)
END IF
IF (LHOOK) CALL DR_HOOK('GET_COVER_N',1,ZHOOK_HANDLE)
!
!==============================================================================
!
END SUBROUTINE GET_COVER_n
