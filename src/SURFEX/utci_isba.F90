!   ##########################################################################
SUBROUTINE UTCI_ISBA(HPROGRAM, PT1M, PQ1M, PU10M, PPS, PREF_SW, PSCA_SW, &
     PDIR_SW, PZENITH, PEMIT_LW, PLW_RAD, PUTCI_OUTSUN, PUTCI_OUTSHADE,  &
     PTRAD_SUN, PTRAD_SHADE )
  !   ##########################################################################
  !
  !!****  *UTCI_ISBA*  
  !!
  !!    PURPOSE
  !!    -------
  !
  !     Computes the Universal Thermal and Climate Index Equivalent temperature
  !     for 2 persons in the rural environment
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
  !!
  !!    MODD_CST
  !!
  !!    REFERENCE
  !!    ---------
  !!   www.utci.org
  !!      
  !!    AUTHOR
  !!    ------
  !!
  !!      R. Schoetter           * Meteo-France *
  !!
  !!    MODIFICATIONS
  !!    -------------
  !!      Original  03/2017
  !-------------------------------------------------------------------------------
  !
  !*       0.     DECLARATIONS
  !               ------------
  !
  USE MODD_CSTS, ONLY : XTT
  USE MODI_UTCI_APPROX
  USE MODI_TRAD_BODY_ISBA
  !
  USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
  USE PARKIND1  ,ONLY : JPRB
  !
  IMPLICIT NONE
  !
  !*      0.1    declarations of arguments
  CHARACTER(LEN=6)  , INTENT(IN)  :: HPROGRAM ! program calling surf. schemes
  REAL, DIMENSION(:), INTENT(IN)  :: PT1M     ! Air temperature at 1 m above ground (K) 
  REAL, DIMENSION(:), INTENT(IN)  :: PQ1M     ! Specific humidity at 1 m above ground (kg/kg)
  REAL, DIMENSION(:), INTENT(IN)  :: PU10M    ! Wind speed in 10 m above ground (m/s)
  REAL, DIMENSION(:), INTENT(IN)  :: PPS      ! Atmospheric Pressure (Pa)
  REAL, DIMENSION(:), INTENT(IN)  :: PREF_SW  ! Solar radiation reflected by ground (W/m2)
  REAL, DIMENSION(:), INTENT(IN)  :: PSCA_SW  ! Diffuse solar radiation (W/m2)
  REAL, DIMENSION(:), INTENT(IN)  :: PDIR_SW  ! Direct solar radiation (W/m2)
  REAL, DIMENSION(:), INTENT(IN)  :: PZENITH  ! Solar zenithal angle (rad from vert.)
  REAL, DIMENSION(:), INTENT(IN)  :: PEMIT_LW ! Longwave radiation emitted by the ground (W/m2)
  REAL, DIMENSION(:), INTENT(IN)  :: PLW_RAD  ! Atmospheric longwave radiation (W/m2)
  REAL, DIMENSION(:), INTENT(OUT) :: PUTCI_OUTSUN   ! UTCI for outdoor person at sun (C)
  REAL, DIMENSION(:), INTENT(OUT) :: PUTCI_OUTSHADE ! UTCI for outdoor person in shade (C)
  REAL, DIMENSION(:), INTENT(OUT) :: PTRAD_SUN      ! Mean radiant temperature at sun (C)
  REAL, DIMENSION(:), INTENT(OUT) :: PTRAD_SHADE    ! Mean radiant temperature in shade (C)
  !
  !*      0.2    declarations of local variables
  REAL, DIMENSION(SIZE(PT1M)) :: ZEHPA !water vapour pressure (hPa)
  REAL, DIMENSION(SIZE(PT1M)) :: ZDIR_SW !direct solar radiation
  REAL, DIMENSION(SIZE(PT1M)) :: ZZENITH !zenithal angle
  !
  REAL(KIND=JPRB) :: ZHOOK_HANDLE
  !
  IF (LHOOK) CALL DR_HOOK('UTCI_ISBA',0,ZHOOK_HANDLE)
  !
  ! Calculation of UTCI_OUTSUN
  !
  ZEHPA = PQ1M * PPS / (0.622 + 0.378 * PQ1M) /100.
  PTRAD_SUN = TRAD_BODY_ISBA(HPROGRAM, PSCA_SW, PREF_SW, PEMIT_LW, PLW_RAD,&
       PDIR_SW, PZENITH )
  !
  PUTCI_OUTSUN = UTCI_APPROX(PT1M - XTT, ZEHPA, PTRAD_SUN - XTT, PU10M)
  !
  ! Calculation of UTCI_OUTSHADE
  !
  ZDIR_SW=0.
  ZZENITH=0.
  !
  PTRAD_SHADE = TRAD_BODY_ISBA(HPROGRAM, PSCA_SW, PREF_SW, PEMIT_LW, PLW_RAD, ZDIR_SW, ZZENITH)
  !
  PUTCI_OUTSHADE = UTCI_APPROX(PT1M - XTT, ZEHPA, PTRAD_SHADE - XTT, PU10M)
  !
  IF (LHOOK) CALL DR_HOOK('UTCI_ISBA',1,ZHOOK_HANDLE)
  !
END SUBROUTINE UTCI_ISBA
