!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
SUBROUTINE TOWN_PRESENCE(HFILETYPE,OTEB)
!     #################################################################################
!
USE MODI_READ_SURF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
 CHARACTER(LEN=6),   INTENT(IN)  :: HFILETYPE ! type of input file
LOGICAL,            INTENT(OUT) :: OTEB      ! TRUE if TEB data exist in the file
!
!*      0.2    declarations of local variables
!
INTEGER           :: IRESP     ! reading return code
 CHARACTER(LEN=6)  :: YTOWN     ! scheme for towns in input file
INTEGER           :: IDIM_TOWN ! number of TEB points in input file
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------------
!
!*      1.     reads if TEB fields exist in the input file
!              -------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('TOWN_PRESENCE',0,ZHOOK_HANDLE)
!
 CALL READ_SURF(HFILETYPE,'TOWN',YTOWN,IRESP)
 CALL READ_SURF(HFILETYPE,'DIM_TOWN',IDIM_TOWN,IRESP)
!
OTEB = (YTOWN=='TEB   ') .AND. (IDIM_TOWN > 0)
IF (LHOOK) CALL DR_HOOK('TOWN_PRESENCE',1,ZHOOK_HANDLE)
!
!---------------------------------------------------------------------------------------
!
END SUBROUTINE TOWN_PRESENCE
