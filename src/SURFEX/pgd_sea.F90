!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
      SUBROUTINE PGD_SEA(HPROGRAM)
!     #############################################################
!
!!****  *PGD_SEA* - routine to choose initialization of sea scheme
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
USE MODD_SURF_ATM_n, ONLY : CSEA
!
USE MODI_PGD_SEAFLUX
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
!
 CHARACTER(LEN=6),                INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
REAL(KIND=JPRB) :: ZHOOK_HANDLE
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
IF (LHOOK) CALL DR_HOOK('PGD_SEA',0,ZHOOK_HANDLE)
IF (CSEA=='NONE  ') THEN
  IF (LHOOK) CALL DR_HOOK('PGD_SEA',1,ZHOOK_HANDLE)
  RETURN
ELSE IF (CSEA=='FLUX  ') THEN
  IF (LHOOK) CALL DR_HOOK('PGD_SEA',1,ZHOOK_HANDLE)
  RETURN
ELSE IF (CSEA=='SEAFLX') THEN
  CALL PGD_SEAFLUX(HPROGRAM)
END IF
IF (LHOOK) CALL DR_HOOK('PGD_SEA',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE PGD_SEA
