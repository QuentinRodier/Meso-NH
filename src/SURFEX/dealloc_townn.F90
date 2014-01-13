!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
SUBROUTINE DEALLOC_TOWN_n
!     ###############################################################################
!
!!****  *DEALLOC_TOWN_n * - Deallocate all arrays
!!
!!    PURPOSE
!!    -------
!
!!**  METHOD
!!    ------
!!
!!    REFERENCE
!!    ---------
!!      
!!
!!    AUTHOR
!!    ------
!!     V. Masson 
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2004
!!------------------------------------------------------------------
!

!
USE MODD_SURF_ATM_n, ONLY : CTOWN
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_DEALLOC_IDEAL_FLUX
!
USE MODI_DEALLOC_TEB_n
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
!*      0.2    declarations of local variables
!
!-------------------------------------------------------------------------------------
!

REAL(KIND=JPRB) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('DEALLOC_TOWN_N',0,ZHOOK_HANDLE)
IF (CTOWN=='TEB   ') THEN
  CALL DEALLOC_TEB_n
ELSE IF (CTOWN=='FLUX  ') THEN
  CALL DEALLOC_IDEAL_FLUX
END IF
IF (LHOOK) CALL DR_HOOK('DEALLOC_TOWN_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE DEALLOC_TOWN_n
