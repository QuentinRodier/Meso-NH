!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!   ##########################################################################
SUBROUTINE UTCI_TEB(PT_CAN, PQ_CAN, PTI_BLD, PQI_BLD, PU10, PPS,         &
                    PREF_SW_GRND, PREF_SW_FAC, PSCA_SW, PDIR_SW, PZENITH,&
                    PEMIT_LW_FAC, PEMIT_LW_GRND, PLW_RAD, PTRAD_IN,      &
                    PBLD, PBLD_HEIGHT, PWALL_O_HOR,                      &
                    PUTCI_IN, PUTCI_OUTSUN, PUTCI_OUTSHADE,              &
                    PTRAD_SUN, PTRAD_SHADE )
!   ##########################################################################
!
!!****  *UTCI_TEB*  
!!
!!    PURPOSE
!!    -------
!
!     Computes the Universal Thermal and Climate Index Equivalent temperature
!     for 3 persons in the urban environment
!         
!     
!!**  METHOD
!     ------
!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!! à compléter
!!    MODD_CST
!!
!!    REFERENCE
!!    ---------
!!   www.utci.org
!!      
!!    AUTHOR
!!    ------
!!
!!	G. Pigeon           * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original  03/2011
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_CSTS, ONLY : XTT
USE MODI_UTCI_APPROX
USE MODI_TRAD_BODY
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
REAL, DIMENSION(:), INTENT(IN)  :: PT_CAN !Air canyon temperature (K) 
REAL, DIMENSION(:), INTENT(IN)  :: PQ_CAN !Canyon specific humidity (kg/kg)
REAL, DIMENSION(:), INTENT(IN)  :: PTI_BLD !Indoor air temperature (K) 
REAL, DIMENSION(:), INTENT(IN)  :: PQI_BLD !Indoor specific humidity (kg/kg) 
REAL, DIMENSION(:), INTENT(IN)  :: PU10 !Canyon wind speed at 10m (m/s)
REAL, DIMENSION(:), INTENT(IN)  :: PPS !Atmospheric Pressure (Pa)
REAL, DIMENSION(:), INTENT(IN)  :: PREF_SW_GRND !Solar radiation reflected by ground [road + garden] (W/m²)
REAL, DIMENSION(:), INTENT(IN)  :: PREF_SW_FAC !Solar radiation reflected by facade [wall + glazing] (W/m²)
REAL, DIMENSION(:), INTENT(IN)  :: PSCA_SW !Diffuse solar radiation (W/m²)
REAL, DIMENSION(:), INTENT(IN)  :: PDIR_SW !Direct solar radiation (W/m²)
REAL, DIMENSION(:), INTENT(IN)  :: PZENITH !solar zenithal angle (rad from vert.)
REAL, DIMENSION(:), INTENT(IN)  :: PEMIT_LW_FAC !Longwave radiation emitted by the facade [wall + glazing] (W/m²)
REAL, DIMENSION(:), INTENT(IN)  :: PEMIT_LW_GRND !Longwave radiation emitted by the ground [road + garden] (W/m²)
REAL, DIMENSION(:), INTENT(IN)  :: PLW_RAD !Atmospheric longwave radiation (W/m²)
REAL, DIMENSION(:), INTENT(IN)  :: PTRAD_IN !Indoor radiant temperature (K)
REAL, DIMENSION(:), INTENT(IN)  :: PBLD !Building surface fraction
REAL, DIMENSION(:), INTENT(IN)  :: PBLD_HEIGHT !Building surface fraction
REAL, DIMENSION(:), INTENT(IN)  :: PWALL_O_HOR !Building surface fraction
REAL, DIMENSION(:), INTENT(OUT) :: PUTCI_IN !UTCI for indoor person (°C)
REAL, DIMENSION(:), INTENT(OUT) :: PUTCI_OUTSUN !UTCI for outdoor person at sun (°C)
REAL, DIMENSION(:), INTENT(OUT) :: PUTCI_OUTSHADE !UTCI for outdoor person in shade (°C)
REAL, DIMENSION(:), INTENT(OUT) :: PTRAD_SUN !Mean radiant temperature at sun (°C)
REAL, DIMENSION(:), INTENT(OUT) :: PTRAD_SHADE !Mean radiant temperature in shade (°C)
!
!*      0.2    declarations of local variables
REAL, DIMENSION(SIZE(PTI_BLD)) :: ZEHPA !water vapour pressure (hPa)
REAL, DIMENSION(SIZE(PTI_BLD)) :: ZUIN !indoor air wind speed (m/s)
REAL, DIMENSION(SIZE(PTI_BLD)) :: ZDIR_SW !direct solar radiation
REAL, DIMENSION(SIZE(PTI_BLD)) :: ZZENITH !zenithal angle
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('UTCI_TEB',0,ZHOOK_HANDLE)
! 1-calculation of UTCI_IN
ZEHPA = PQI_BLD * PPS /(0.622 + 0.378 * PQI_BLD) / 100.
ZUIN = 0.5
PUTCI_IN = UTCI_APPROX(PTI_BLD - XTT, ZEHPA, PTRAD_IN - XTT, ZUIN)
!
! 2-calculation of UTCI_OUTSUN
ZEHPA = PQ_CAN * PPS / (0.622 + 0.378 * PQ_CAN) /100.
PTRAD_SUN = TRAD_BODY(PSCA_SW, PREF_SW_FAC, PREF_SW_GRND,  &
                      PEMIT_LW_FAC, PEMIT_LW_GRND, PLW_RAD,&
                      PBLD, PBLD_HEIGHT, PWALL_O_HOR, PDIR_SW, PZENITH )
PUTCI_OUTSUN = UTCI_APPROX(PT_CAN - XTT, ZEHPA, PTRAD_SUN - XTT, PU10)
!
! 3-calculation of UTCI_OUTSHADE
ZDIR_SW=0.
ZZENITH=0.
!!PTRAD_SHADE = TRAD_BODY(PSCA_SW,PREF_SW_FAC, PREF_SW_GRND, &
!!                      PEMIT_LW_FAC, PEMIT_LW_GRND, PLW_RAD,&
!!                      PBLD, PBLD_HEIGHT, PWALL_O_HOR, ZDIR_SW, ZZENITH)
PTRAD_SHADE = TRAD_BODY(PSCA_SW,PREF_SW_FAC, PREF_SW_GRND, &
                      PEMIT_LW_FAC, PEMIT_LW_GRND, PLW_RAD,&
                      PBLD, PBLD_HEIGHT, PWALL_O_HOR)
PUTCI_OUTSHADE = UTCI_APPROX(PT_CAN - XTT, ZEHPA, PTRAD_SHADE - XTT, PU10)
IF (LHOOK) CALL DR_HOOK('UTCI_TEB',1,ZHOOK_HANDLE)
END SUBROUTINE UTCI_TEB
