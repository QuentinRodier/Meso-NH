!     #########
      SUBROUTINE UPDATE_DATA_COVER(KYEAR)
!     #########################
!
!!**** *INI_DATA_COVER* initializes cover-field correspondance arrays
!!
!!    PURPOSE
!!    -------
!!
!!    METHOD
!!    ------
!!
!!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!
!!    V. Masson        Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    09/2008
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_DATA_COVER_n,   ONLY : XDATA_VEGTYPE, NYEAR
USE MODD_DATA_COVER,     ONLY : XDATA_NATURE, XDATA_GARDEN, XDATA_LAI, XDATA_H_TREE, &
                                  XDATA_VEG, XDATA_GREEN, XDATA_Z0, XDATA_EMIS_ECO
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ECOCLIMAP2_LAI
!
USE MODI_INI_DATA_PARAM
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
INTEGER,             INTENT(IN)    :: KYEAR        ! new year
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
!
!*    0.3    Declaration of namelists
!            ------------------------
!
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('UPDATE_DATA_COVER',0,ZHOOK_HANDLE)
IF (KYEAR /= NYEAR) THEN        
  NYEAR = KYEAR
  CALL ECOCLIMAP2_LAI
  CALL INI_DATA_PARAM(XDATA_VEGTYPE, PSURF=XDATA_NATURE, PSURF2=XDATA_GARDEN, &
             PLAI=XDATA_LAI, PH_TREE=XDATA_H_TREE, PVEG_OUT=XDATA_VEG, &
             PGREEN=XDATA_GREEN, PZ0=XDATA_Z0, PEMIS_ECO=XDATA_EMIS_ECO)
END IF
IF (LHOOK) CALL DR_HOOK('UPDATE_DATA_COVER',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------

END SUBROUTINE UPDATE_DATA_COVER
