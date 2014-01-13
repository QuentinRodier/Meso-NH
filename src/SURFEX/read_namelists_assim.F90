!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
SUBROUTINE READ_NAMELISTS_ASSIM(HPROGRAM)
!     #######################################################
!
!---------------------------    
!
USE MODD_ASSIM,           ONLY : LASSIM,CASSIM,CASSIM_ISBA,LPRINT,LAROME,LECSST,    &
                                 LAESST,LAESNM,LALADSURF,LREAD_SST_FROM_FILE,       &
                                 LEXTRAP_SEA,LEXTRAP_WATER,LEXTRAP_NATURE,LWATERTG2
USE MODI_DEFAULT_ASSIM
USE MODI_READ_ASSIM_CONF
USE MODI_INI_ASSIM
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
 CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM      ! program calling surf. schemes
REAL(KIND=JPRB)                 :: ZHOOK_HANDLE

!---------------------------------------------------
IF (LHOOK) CALL DR_HOOK('READ_NAMELISTS_ASSIM',0,ZHOOK_HANDLE)

! Set default assimilation options/schemes
 CALL DEFAULT_ASSIM(LASSIM,CASSIM,CASSIM_ISBA,LPRINT,        &
                   LAROME,LECSST,LAESST,LAESNM,             &
                   LALADSURF,LREAD_SST_FROM_FILE,           &
                   LEXTRAP_SEA,LEXTRAP_WATER,LEXTRAP_NATURE,&
                   LWATERTG2)

! Set default assimilations values/constants
 CALL INI_ASSIM

! Override with namelist values
 CALL READ_ASSIM_CONF(HPROGRAM)

IF (LHOOK) CALL DR_HOOK('READ_NAMELISTS_ASSIM',1,ZHOOK_HANDLE)
!---------------------------------------------------------
END SUBROUTINE READ_NAMELISTS_ASSIM
