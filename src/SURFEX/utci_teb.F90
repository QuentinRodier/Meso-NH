!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!   ##########################################################################
SUBROUTINE UTCI_TEB(T, DUT, TOP, KCOMP, HPROGRAM, PTI_BLD, PQI_BLD, PU10, PT1M, PQ1M, &
     PPS, PREF_SW_GRND, PREF_SW_FAC, PSCA_SW, PDIR_SW, PZENITH, PEMIT_LW_FAC,         &
     PEMIT_LW_GRND, PEMIT_LW_HVEG, PSCA_SW_SKY, PLW_RAD_SKY,  PLW_RAD, PTRAD_IN,      &
     PTAU_SR, PSCA_SW_GROUND_DOWN, PSCA_SW_GROUND_UP, PSCA_SW_GROUND_HOR,             &
     PLW_GROUND_DOWN, PLW_GROUND_HOR, HTEST )
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
!! a supplement
!!    MODD_CST
!!
!!    REFERENCE
!!    ---------
!!   www.utci.org
!!      
!!    AUTHOR
!!    ------
!!
!!      G. Pigeon           * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original  03/2011
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_DIAG_UTCI_n, ONLY : DIAG_UTCI_t
USE MODD_TEB_n, ONLY : TEB_t
USE MODD_TEB_OPTION_n, ONLY : TEB_OPTIONS_t
!
USE MODD_CSTS, ONLY : XTT
USE MODI_UTCI_APPROX
USE MODI_TRAD_BODY_TEB
!
USE YOMHOOK , ONLY : LHOOK, DR_HOOK
USE PARKIND1, ONLY : JPRB
!
  IMPLICIT NONE
!
!*      0.1    declarations of arguments
TYPE(TEB_t), INTENT(INOUT) :: T
TYPE(DIAG_UTCI_t), INTENT(INOUT) :: DUT
TYPE(TEB_OPTIONS_t), INTENT(IN) :: TOP
!
  CHARACTER(LEN=2),   INTENT(IN) :: HTEST ! must be equal to 'OK'
  INTEGER,            INTENT(IN) :: KCOMP         ! compartiment index
  CHARACTER(LEN=6)  , INTENT(IN) :: HPROGRAM       ! program calling surf. schemes
REAL, DIMENSION(:), INTENT(IN) :: PTI_BLD !Indoor air temperature (K) 
REAL, DIMENSION(:), INTENT(IN) :: PQI_BLD !Indoor specific humidity (kg/kg) 
REAL, DIMENSION(:), INTENT(IN) :: PU10 !Canyon wind speed at 10 m (m/s)
REAL, DIMENSION(:), INTENT(IN)  :: PT1M !Canyon temperature at 1 m (K)
REAL, DIMENSION(:), INTENT(IN)  :: PQ1M !Canyon specific humidity at 1 m (kg/kg)
REAL, DIMENSION(:), INTENT(IN)  :: PPS !Atmospheric Pressure (Pa)
REAL, DIMENSION(:), INTENT(IN) :: PREF_SW_GRND !Solar radiation reflected by ground [road + garden] (W/m2)
REAL, DIMENSION(:), INTENT(IN) :: PREF_SW_FAC !Solar radiation reflected by facade [wall + glazing] (W/m2)
REAL, DIMENSION(:), INTENT(IN) :: PSCA_SW !Diffuse solar radiation (W/m2)
REAL, DIMENSION(:), INTENT(IN) :: PDIR_SW !Direct solar radiation (W/m2)
REAL, DIMENSION(:), INTENT(IN) :: PZENITH !solar zenithal angle (rad from vert.)
REAL, DIMENSION(:), INTENT(IN) :: PEMIT_LW_FAC !Longwave radiation emitted by the facade [wall + glazing] (W/m2)
REAL, DIMENSION(:), INTENT(IN) :: PEMIT_LW_GRND !Longwave radiation emitted by the ground [road + garden] (W/m2)
REAL, DIMENSION(:), INTENT(IN) :: PEMIT_LW_HVEG  ! Longwave radiation emitted by the tree canopy            (W/m2)
REAL, DIMENSION(:), INTENT(IN) :: PSCA_SW_SKY    ! Diff sol rad from sky received by people (incl attenuation by trees)
REAL, DIMENSION(:), INTENT(IN) :: PLW_RAD_SKY    ! IR rad from sky received by people (incl attenuation by trees)
REAL, DIMENSION(:), INTENT(IN) :: PLW_RAD !Atmospheric longwave radiation (W/m2)
REAL, DIMENSION(:), INTENT(IN) :: PTRAD_IN !Indoor radiant temperature (K)
REAL, DIMENSION(:), INTENT(IN) :: PTAU_SR  ! part of radiation coming from the sky not intercepted by trees.
REAL, DIMENSION(:), INTENT(IN) :: PSCA_SW_GROUND_DOWN ! Diffusive downwelling solar radiation at ground level (W/m2)
REAL, DIMENSION(:), INTENT(IN) :: PSCA_SW_GROUND_UP   ! Diffusive upwelling solar radiation at ground level (W/m2)
REAL, DIMENSION(:), INTENT(IN) :: PSCA_SW_GROUND_HOR  ! Diffusive solar radiation in horizontal direction at ground level (W/m2)
REAL, DIMENSION(:), INTENT(IN) :: PLW_GROUND_DOWN     ! Diffusive downwelling longwave radiation at ground level (W/m2)
REAL, DIMENSION(:), INTENT(IN) :: PLW_GROUND_HOR      ! Diffusive longwave radiation in horizontal direction at ground level (W/m2)
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
!
IF (HTEST/='OK') THEN
   CALL ABOR1_SFX('COUPLING_TEBN: FATAL ERROR DURING ARGUMENT TRANSFER')
END IF
!
! 1-calculation of UTCI_IN
!
ZEHPA = PQI_BLD * PPS /(0.622 + 0.378 * PQI_BLD) / 100.
ZUIN = 0.5
!
DUT%XUTCI_IN(:,KCOMP) = UTCI_APPROX(PTI_BLD - XTT, ZEHPA, PTRAD_IN - XTT, ZUIN)
!
IF (KCOMP>1) THEN
   IF (LHOOK) CALL DR_HOOK('UTCI_TEB',1,ZHOOK_HANDLE)
     RETURN
END IF
!
! 2-calculation of UTCI_OUTSUN
!
ZEHPA = PQ1M * PPS / (0.622 + 0.378 * PQ1M) /100.
!
DUT%XTRAD_SUN = TRAD_BODY_TEB(HPROGRAM, PSCA_SW, PREF_SW_FAC, PREF_SW_GRND, &
       PEMIT_LW_FAC, PEMIT_LW_GRND, PEMIT_LW_HVEG, PSCA_SW_SKY, PLW_RAD_SKY,  &
       PLW_RAD, T%XBLD, T%XBLD_HEIGHT, T%XWALL_O_HOR, PTAU_SR, PDIR_SW,       &
       PZENITH, TOP%LSPARTACUS, PSCA_SW_GROUND_DOWN, PSCA_SW_GROUND_UP,       &
       PSCA_SW_GROUND_HOR, PLW_GROUND_DOWN, PLW_GROUND_HOR )
!
DUT%XUTCI_OUTSUN = UTCI_APPROX(PT1M - XTT, ZEHPA, DUT%XTRAD_SUN - XTT, PU10)
!
! 3-calculation of UTCI_OUTSHADE
!
ZDIR_SW=0.
ZZENITH=0.
!
DUT%XTRAD_SHADE = TRAD_BODY_TEB(HPROGRAM, PSCA_SW, PREF_SW_FAC, PREF_SW_GRND, &
       PEMIT_LW_FAC, PEMIT_LW_GRND, PEMIT_LW_HVEG, PSCA_SW_SKY, PLW_RAD_SKY,    &
       PLW_RAD, T%XBLD, T%XBLD_HEIGHT, T%XWALL_O_HOR, PTAU_SR, ZDIR_SW,         &
       ZZENITH, TOP%LSPARTACUS, PSCA_SW_GROUND_DOWN, PSCA_SW_GROUND_UP,         &
       PSCA_SW_GROUND_HOR, PLW_GROUND_DOWN, PLW_GROUND_HOR )
!
DUT%XUTCI_OUTSHADE = UTCI_APPROX(PT1M - XTT, ZEHPA, DUT%XTRAD_SHADE - XTT, PU10)
!
IF (LHOOK) CALL DR_HOOK('UTCI_TEB',1,ZHOOK_HANDLE)
!
END SUBROUTINE UTCI_TEB
