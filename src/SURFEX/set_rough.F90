!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
      SUBROUTINE SET_ROUGH(OCANOPY,HROUGH)
!     ########################################################################
!
!!****  *SET_ROUGH* - routine to set default values for roughness ISBA configuration
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
USE MODD_SURF_PAR,   ONLY : XUNDEF
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
!
LOGICAL,           INTENT(IN)     :: OCANOPY    ! True  = SBL scheme
                                                ! False = no SBL scheme
 CHARACTER(LEN=4),  INTENT(INOUT)  :: HROUGH     ! type of roughness length
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
!-------------------------------------------------------------------------------
!
!* default value if needed
!
IF (LHOOK) CALL DR_HOOK('SET_ROUGH',0,ZHOOK_HANDLE)
IF (HROUGH  == "UNDE") THEN
  IF (OCANOPY) THEN
    HROUGH = "BE04"
  ELSE
    HROUGH = "Z04D"
  END IF
END IF
!
!* coherence check
!
IF (OCANOPY .AND. (HROUGH=="Z01D" .OR. HROUGH=="Z04D")) THEN
  CALL ABOR1_SFX('If LISBA_CANOPY=T, orographic roughness cannot be computed by CROUGH="Z01D" or "Z04D"')
END IF
!
IF (.NOT. OCANOPY .AND. HROUGH=="BE04") THEN
  CALL ABOR1_SFX('If LISBA_CANOPY=F, orographic roughness cannot be computed by CROUGH="BE04"')
END IF
IF (LHOOK) CALL DR_HOOK('SET_ROUGH',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE SET_ROUGH
