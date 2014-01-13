!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
      SUBROUTINE PGD_NATURE(HPROGRAM,OECOCLIMAP)
!     #############################################################
!
!!****  *PGD_NATURE* - routine to choose initialization of vegetation scheme
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
!!      Original    03/2004
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_SURF_ATM_n, ONLY : CNATURE
!
USE MODI_PGD_ISBA
USE MODI_PGD_TSZ0_PAR
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
!
 CHARACTER(LEN=6), INTENT(IN)  :: HPROGRAM   ! program calling surf. schemes
LOGICAL,          INTENT(IN)  :: OECOCLIMAP ! T if parameters are computed with ecoclimap
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!                                           ! F if all parameters must be specified
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
!-------------------------------------------------------------------------------
!
!*       2.     Selection of surface scheme
!               ---------------------------
!
IF (LHOOK) CALL DR_HOOK('PGD_NATURE',0,ZHOOK_HANDLE)
IF (CNATURE=='NONE  ') THEN
  IF (LHOOK) CALL DR_HOOK('PGD_NATURE',1,ZHOOK_HANDLE)
  RETURN
ELSE IF (CNATURE=='FLUX  ') THEN
  IF (LHOOK) CALL DR_HOOK('PGD_NATURE',1,ZHOOK_HANDLE)
  RETURN
ELSE IF (CNATURE=='ISBA  ' .OR. CNATURE=='TSZ0') THEN
  CALL PGD_ISBA(HPROGRAM,OECOCLIMAP)
  IF (CNATURE=='TSZ0') CALL PGD_TSZ0_PAR(HPROGRAM)
END IF
IF (LHOOK) CALL DR_HOOK('PGD_NATURE',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE PGD_NATURE
