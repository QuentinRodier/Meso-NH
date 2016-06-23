!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ######spl
      SUBROUTINE CONVERT_TEB (TOP, &
                              PCOVER,PTEB_PATCH)
!     ##############################################################
!
!!**** *CONVERT_TEB* initialisation of TEB parameters valid for all patches
!!
!!    PURPOSE
!!    -------
!!
!!    METHOD
!!    ------
!!   
!
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
!!    Original    11/11
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
!
USE MODD_TEB_OPTION_n, ONLY : TEB_OPTIONS_t
!
USE MODD_SURF_PAR,       ONLY : XUNDEF
!
!
USE MODI_INI_DATA_PARAM
USE MODI_AV_PGD
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABOR1_SFX
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
!
TYPE(TEB_OPTIONS_t), INTENT(INOUT) :: TOP
!
REAL, DIMENSION(:,:),   INTENT(IN)    :: PCOVER
!
REAL, DIMENSION(:,:), INTENT(OUT), OPTIONAL   :: PTEB_PATCH !  fraction of each TEB patch
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER               :: JPATCH    ! loop counter on patch
INTEGER               :: JCOVER    ! loop counter
!
!*    0.3    Declaration of namelists
!            ------------------------
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!*    1.      Initializations
!             ---------------
!
IF (LHOOK) CALL DR_HOOK('CONVERT_TEB',0,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
!       GARDEN fraction
!       ---------------
IF (PRESENT(PTEB_PATCH)) THEN
  DO JPATCH=1,TOP%NTEB_PATCH
    PTEB_PATCH(:,JPATCH) = 1./FLOAT(TOP%NTEB_PATCH)
  END DO
ENDIF
!
!
IF (LHOOK) CALL DR_HOOK('CONVERT_TEB',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE CONVERT_TEB
