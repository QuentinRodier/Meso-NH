!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     ######spl
      SUBROUTINE PUT_ZS_SEA_n(HPROGRAM,KI,PZS)
!     ###########################################
!
!!****  *PUT_ZS_SURF_ATM_n* - routine to modify nature oropgraphy using atmospheric
!                    model orography
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
!!	P. Le Moigne   *Meteo France*	
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    05/2007
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODI_GET_LUOUT
!
USE MODD_SEAFLUX_n,     ONLY : XZS
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
 CHARACTER(LEN=6),    INTENT(IN)  :: HPROGRAM
INTEGER,             INTENT(IN)  :: KI      ! horizontal dim. of cover
REAL, DIMENSION(KI), INTENT(IN)  :: PZS     ! orography
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER :: ILUOUT
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!

!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('PUT_ZS_SEA_N',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!-------------------------------------------------------------------------------
!
IF ( SIZE(PZS) /= SIZE(XZS) ) THEN
  WRITE(ILUOUT,*) 'try to get ZS field from atmospheric model, but size is not correct'
  WRITE(ILUOUT,*) 'size of field expected by the atmospheric model (PZS) :', SIZE(PZS)
  WRITE(ILUOUT,*) 'size of field over sea                          (XZS) :', SIZE(XZS)
  STOP
ELSE
  XZS = PZS
END IF
IF (LHOOK) CALL DR_HOOK('PUT_ZS_SEA_N',1,ZHOOK_HANDLE)
!
!==============================================================================
!
END SUBROUTINE PUT_ZS_SEA_n
