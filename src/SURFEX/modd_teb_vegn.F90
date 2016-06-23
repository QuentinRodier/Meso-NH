!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ################
      MODULE MODD_TEB_VEG_n
!     ################
!
!!****  *MODD_TEB_VEG_n - declaration of options and parameters for urban vegetation
!!                        (for parameters common to all types of urban vegetation)
!!
!!    PURPOSE
!!    -------
!     Declaration of options and surface parameters
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      None 
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!      C. de Munck & A. Lemonsu   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original       07/2012
!
!*       0.   DECLARATIONS
!             ------------
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE


TYPE TEB_VEG_OPTIONS_t
! ISBA options common of all types of urban vegetation
!
!
  LOGICAL                          :: LCANOPY_DRAG ! T: drag activated in SBL scheme within the canopy
                                                   ! F: no drag activated in SBL atmospheric layers
!
  LOGICAL                          :: LVEGUPD      ! T = update vegetation parameters every decade
                                                   ! F = keep vegetation parameters constant in time
!
  LOGICAL                          :: LTR_ML
!  
  LOGICAL                          :: LNITRO_DILU  ! nitrogen dilution fct of CO2 (Calvet et al. 2008)
!-------------------------------------------------------------------------------
!
  CHARACTER(LEN=3)                 :: CISBA       ! type of ISBA version:
                                                  ! '2-L' (default)
                                                  ! '3-L'
                                                  ! 'DIF'
!
  CHARACTER(LEN=4)                 :: CROUGH      ! type of roughness length
                                                  ! 'Z01D'
                                                  ! 'Z04D'
!
  CHARACTER(LEN=4)                 :: CPEDOTF     ! NOTE: Only used when HISBA = DIF
                                                  ! 'CH78' = Clapp and Hornberger 1978 for BC (Default)
                                                  ! 'CO84' = Cosby et al. 1988 for BC
                                                  ! 'CP88' = Carsel and Parrish 1988 for VG
                                                  ! 'WO99' = Wosten et al. 1999 for VG
!
  CHARACTER(LEN=3)                 :: CPHOTO      ! type of photosynthesis
                                                  ! 'NON'
                                                  ! 'AGS'
                                                  ! 'LAI'
                                                  ! 'LST'
                                                  ! 'AST'
                                                  ! 'NIT'
                                                  ! 'NCB'
!
  CHARACTER(LEN=4)                 :: CALBEDO     ! albedo type
                                                  ! 'DRY ' 
                                                  ! 'EVOL' 
                                                  ! 'WET ' 
                                                  ! 'USER' 
!
  CHARACTER(LEN=4)                 :: CSCOND      ! Thermal conductivity
                                                  ! 'DEF ' = DEFault: NP89 implicit method
                                                  ! 'PL98' = Peters-Lidard et al. 1998 used
                                                  ! for explicit computation of CG
!
  CHARACTER(LEN=4)                 :: CC1DRY      ! C1 formulation for dry soils
                                                  ! 'DEF ' = DEFault: Giard-Bazile formulation
                                                  ! 'GB93' = Giordani 1993, Braud 1993 
                                                  !discontinuous at WILT
!
  CHARACTER(LEN=3)                 :: CSOILFRZ    ! soil freezing-physics option
                                                  ! 'DEF' = Default (Boone et al. 2000; 
                                                  !        Giard and Bazile 2000)
                                                  ! 'LWT' = Phase changes as above,
                                                  !         but relation between unfrozen 
                                                  !         water and temperature considered
!                            NOTE that when using the YISBA='DIF' multi-layer soil option,
!                            the 'LWT' method is used. It is only an option
!                            when using the force-restore soil method ('2-L' or '3-L')
!
  CHARACTER(LEN=4)                 :: CDIFSFCOND  ! Mulch effects
                                                  ! 'MLCH' = include the insulating effect of
                                                  ! leaf litter/mulch on the surf. thermal cond.
                                                  ! 'DEF ' = no mulch effect
!                           NOTE: Only used when YISBA = DIF
!
  CHARACTER(LEN=3)                 :: CSNOWRES    ! Turbulent exchanges over snow
                                                  ! 'DEF' = Default: Louis (ISBA)
                                                  ! 'RIL' = Maximum Richardson number limit
                                                  !         for stable conditions ISBA-SNOW3L
                                                  !         turbulent exchange option
!                                           
  CHARACTER(LEN=3)                 :: CRESPSL     ! Soil respiration
                                                  ! 'DEF' = Default: Norman (1992)
                                                  ! 'PRM' = New Parameterization
                                                  ! 'CNT' = CENTURY model (Gibelin 2007)
!                                           
  CHARACTER(LEN=3)                 :: CCPSURF     ! specific heat at surface
                                                  ! 'DRY' = default value (dry Cp)
                                                  ! 'HUM' = Cp as a fct of specific humidity
! - SGH scheme and vertical hydrology
!                                                     
  CHARACTER(LEN=4)                 :: CRUNOFF     ! surface runoff formulation
                                                  ! 'WSAT'
                                                  ! 'DT92'
                                                  ! 'SGH ' Topmodel
!                                                     
  CHARACTER(LEN=3)                 :: CKSAT       ! ksat
                                                  ! 'DEF' = default value 
                                                  ! 'SGH' = profil exponentiel
!
  LOGICAL                          :: LSOC        ! soil organic carbon effect
!                                                 ! FALSE = default value 
!                                                 ! TRUE  = SOC profil
!
  CHARACTER(LEN=3)                 :: CRAIN       ! Rainfall spatial distribution
                                                  ! 'DEF' = No rainfall spatial distribution
                                                  ! 'SGH' = Rainfall exponential spatial distribution
!
  CHARACTER(LEN=3)                 :: CHORT       ! Horton runoff
                                                  ! 'DEF' = no Horton runoff
                                                  ! 'SGH' = Horton runoff
!
! -----------------------------------------------------------------------------------------------------------
!
  INTEGER                          :: NNBIOMASS   ! number of biomass pools
  REAL                             :: XCGMAX      ! maximum soil heat capacity (=2.E-5)
  REAL                             :: XCDRAG      ! drag coefficient in canopy
  REAL                             :: XTSTEP      !  time step  
!
END TYPE TEB_VEG_OPTIONS_t



 CONTAINS
!----------------------------------------------------------------------------

!




SUBROUTINE TEB_VEG_OPTIONS_INIT(YTEB_VEG_OPTIONS)
TYPE(TEB_VEG_OPTIONS_t), INTENT(INOUT) :: YTEB_VEG_OPTIONS
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK("MODD_TEB_VEG_N:TEB_VEG_OPTIONS_INIT",0,ZHOOK_HANDLE)
YTEB_VEG_OPTIONS%LCANOPY_DRAG=.FALSE.
YTEB_VEG_OPTIONS%LVEGUPD=.FALSE. 
YTEB_VEG_OPTIONS%LNITRO_DILU=.FALSE. 
YTEB_VEG_OPTIONS%LTR_ML=.FALSE.
YTEB_VEG_OPTIONS%CISBA=' '
YTEB_VEG_OPTIONS%CROUGH=' '
YTEB_VEG_OPTIONS%CSCOND=' '
YTEB_VEG_OPTIONS%CPEDOTF=' '
YTEB_VEG_OPTIONS%CPHOTO=' '
YTEB_VEG_OPTIONS%CALBEDO=' '
YTEB_VEG_OPTIONS%CC1DRY=' '
YTEB_VEG_OPTIONS%CSOILFRZ=' '
YTEB_VEG_OPTIONS%CDIFSFCOND=' '
YTEB_VEG_OPTIONS%CSNOWRES=' '
YTEB_VEG_OPTIONS%CRESPSL=' '
YTEB_VEG_OPTIONS%CCPSURF=' '
YTEB_VEG_OPTIONS%CRUNOFF=' '
YTEB_VEG_OPTIONS%CKSAT=' '
YTEB_VEG_OPTIONS%LSOC=.FALSE.
YTEB_VEG_OPTIONS%CRAIN=' '
YTEB_VEG_OPTIONS%CHORT=' '
YTEB_VEG_OPTIONS%NNBIOMASS=0
YTEB_VEG_OPTIONS%XCGMAX=0.
YTEB_VEG_OPTIONS%XCDRAG=0.
YTEB_VEG_OPTIONS%XTSTEP=0.
IF (LHOOK) CALL DR_HOOK("MODD_TEB_VEG_N:TEB_VEG_OPTIONS_INIT",1,ZHOOK_HANDLE)
END SUBROUTINE TEB_VEG_OPTIONS_INIT


!----------------------------------------------------------------------------

END MODULE MODD_TEB_VEG_n
