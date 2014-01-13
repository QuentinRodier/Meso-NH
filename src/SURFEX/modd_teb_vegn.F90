!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
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
!!	C. de Munck & A. Lemonsu   *Meteo France*
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
! - SGH scheme
!                                                     
  CHARACTER(LEN=4)                 :: CRUNOFF     ! surface runoff formulation
                                                  ! 'WSAT'
                                                  ! 'DT92'
                                                  ! 'SGH ' Topmodel
!                                                     
  CHARACTER(LEN=3)                 :: CTOPREG     ! Wolock and McCabe (2000) linear regression for Topmodel
                                                  ! 'DEF' = Reg
                                                  ! 'NON' = no Reg  
!                                           
  CHARACTER(LEN=3)                 :: CKSAT       ! ksat
                                                  ! 'DEF' = default value 
                                                  ! 'SGH' = profil exponentiel
!
  CHARACTER(LEN=3)               :: CSOC          ! soil organic carbon effect
!                                                 ! 'DEF' = default value 
!                                                 ! 'SGH' = SOC profil
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

TYPE(TEB_VEG_OPTIONS_t), ALLOCATABLE, TARGET, SAVE :: TEB_VEG_OPTIONS_MODEL(:)

LOGICAL, POINTER                 :: LCANOPY_DRAG=>NULL()
!$OMP THREADPRIVATE(LCANOPY_DRAG)
LOGICAL, POINTER                 :: LVEGUPD=>NULL()
!$OMP THREADPRIVATE(LVEGUPD)
LOGICAL,          POINTER        :: LTR_ML=>NULL()
!$OMP THREADPRIVATE(LTR_ML)
 CHARACTER(LEN=3), POINTER        :: CISBA=>NULL()
!$OMP THREADPRIVATE(CISBA)
 CHARACTER(LEN=4), POINTER        :: CROUGH=>NULL()
!$OMP THREADPRIVATE(CROUGH)
 CHARACTER(LEN=4), POINTER        :: CPEDOTF=>NULL()
!$OMP THREADPRIVATE(CPEDOTF)
 CHARACTER(LEN=3), POINTER        :: CPHOTO=>NULL()
!$OMP THREADPRIVATE(CPHOTO)
 CHARACTER(LEN=4), POINTER        :: CALBEDO=>NULL()
!$OMP THREADPRIVATE(CALBEDO)
 CHARACTER(LEN=4), POINTER        :: CSCOND=>NULL()
!$OMP THREADPRIVATE(CSCOND)
 CHARACTER(LEN=4), POINTER        :: CC1DRY=>NULL()
!$OMP THREADPRIVATE(CC1DRY)
 CHARACTER(LEN=3), POINTER        :: CSOILFRZ=>NULL()
!$OMP THREADPRIVATE(CSOILFRZ)
 CHARACTER(LEN=4), POINTER        :: CDIFSFCOND=>NULL()
!$OMP THREADPRIVATE(CDIFSFCOND)
 CHARACTER(LEN=3), POINTER        :: CSNOWRES=>NULL()
!$OMP THREADPRIVATE(CSNOWRES)
 CHARACTER(LEN=3), POINTER        :: CRESPSL=>NULL()
!$OMP THREADPRIVATE(CRESPSL)
 CHARACTER(LEN=3), POINTER        :: CCPSURF=>NULL()
!$OMP THREADPRIVATE(CCPSURF)
 CHARACTER(LEN=4), POINTER        :: CRUNOFF=>NULL()
!$OMP THREADPRIVATE(CRUNOFF)
 CHARACTER(LEN=3), POINTER        :: CTOPREG=>NULL()
!$OMP THREADPRIVATE(CTOPREG)
 CHARACTER(LEN=3), POINTER        :: CKSAT=>NULL()
!$OMP THREADPRIVATE(CKSAT)
 CHARACTER(LEN=3), POINTER      :: CSOC=>NULL()
!$OMP THREADPRIVATE(CSOC)
 CHARACTER(LEN=3), POINTER        :: CRAIN=>NULL()
!$OMP THREADPRIVATE(CRAIN)
 CHARACTER(LEN=3), POINTER        :: CHORT=>NULL()
!$OMP THREADPRIVATE(CHORT)
INTEGER, POINTER                 :: NNBIOMASS=>NULL()
!$OMP THREADPRIVATE(NNBIOMASS)
REAL, POINTER                    :: XCGMAX=>NULL()
!$OMP THREADPRIVATE(XCGMAX)
REAL, POINTER                    :: XCDRAG=>NULL()
!$OMP THREADPRIVATE(XCDRAG)
REAL, POINTER                    :: XTSTEP=>NULL()
!$OMP THREADPRIVATE(XTSTEP)
!--------------------------------------------------------------------------


!----------------------------------------------------------------------------
CONTAINS
!----------------------------------------------------------------------------

SUBROUTINE TEB_VEG_OPTIONS_GOTO_MODEL(KFROM, KTO, LKFROM)
LOGICAL, INTENT(IN) :: LKFROM
INTEGER, INTENT(IN) :: KFROM, KTO
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
! Save current state for allocated arrays
IF (LKFROM) THEN
! Aucune variable a plusieurs dimensions
ENDIF
!
! Current model is set to model KTO
IF (LHOOK) CALL DR_HOOK('MODD_TEB_VEG_N:TEB_VEG_OPTIONS_GOTO_MODEL',0,ZHOOK_HANDLE)
LCANOPY_DRAG=>TEB_VEG_OPTIONS_MODEL(KTO)%LCANOPY_DRAG
LVEGUPD=>TEB_VEG_OPTIONS_MODEL(KTO)%LVEGUPD
LTR_ML=>TEB_VEG_OPTIONS_MODEL(KTO)%LTR_ML
CISBA=>TEB_VEG_OPTIONS_MODEL(KTO)%CISBA
CROUGH=>TEB_VEG_OPTIONS_MODEL(KTO)%CROUGH
CPEDOTF=>TEB_VEG_OPTIONS_MODEL(KTO)%CPEDOTF
CPHOTO=>TEB_VEG_OPTIONS_MODEL(KTO)%CPHOTO
CALBEDO=>TEB_VEG_OPTIONS_MODEL(KTO)%CALBEDO
CSCOND=>TEB_VEG_OPTIONS_MODEL(KTO)%CSCOND
CC1DRY=>TEB_VEG_OPTIONS_MODEL(KTO)%CC1DRY
CSOILFRZ=>TEB_VEG_OPTIONS_MODEL(KTO)%CSOILFRZ
CDIFSFCOND=>TEB_VEG_OPTIONS_MODEL(KTO)%CDIFSFCOND
CSNOWRES=>TEB_VEG_OPTIONS_MODEL(KTO)%CSNOWRES
CRESPSL=>TEB_VEG_OPTIONS_MODEL(KTO)%CRESPSL
CCPSURF=>TEB_VEG_OPTIONS_MODEL(KTO)%CCPSURF
CRUNOFF=>TEB_VEG_OPTIONS_MODEL(KTO)%CRUNOFF
CTOPREG=>TEB_VEG_OPTIONS_MODEL(KTO)%CTOPREG
CKSAT=>TEB_VEG_OPTIONS_MODEL(KTO)%CKSAT
CSOC=>TEB_VEG_OPTIONS_MODEL(KTO)%CSOC
CRAIN=>TEB_VEG_OPTIONS_MODEL(KTO)%CRAIN
CHORT=>TEB_VEG_OPTIONS_MODEL(KTO)%CHORT
NNBIOMASS=>TEB_VEG_OPTIONS_MODEL(KTO)%NNBIOMASS
XCGMAX=>TEB_VEG_OPTIONS_MODEL(KTO)%XCGMAX
XCDRAG=>TEB_VEG_OPTIONS_MODEL(KTO)%XCDRAG
XTSTEP=>TEB_VEG_OPTIONS_MODEL(KTO)%XTSTEP
IF (LHOOK) CALL DR_HOOK('MODD_TEB_VEG_N:TEB_VEG_OPTIONS_GOTO_MODEL',1,ZHOOK_HANDLE)

END SUBROUTINE TEB_VEG_OPTIONS_GOTO_MODEL

SUBROUTINE TEB_VEG_OPTIONS_ALLOC(KMODEL)
INTEGER, INTENT(IN) :: KMODEL
INTEGER :: J,JP
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK("MODD_TEB_VEG_N:TEB_VEG_OPTIONS_ALLOC",0,ZHOOK_HANDLE)
ALLOCATE(TEB_VEG_OPTIONS_MODEL(KMODEL))
TEB_VEG_OPTIONS_MODEL(:)%LCANOPY_DRAG=.FALSE.
TEB_VEG_OPTIONS_MODEL(:)%LVEGUPD=.FALSE. 
TEB_VEG_OPTIONS_MODEL(:)%LTR_ML=.FALSE.
TEB_VEG_OPTIONS_MODEL(:)%CISBA=' '
TEB_VEG_OPTIONS_MODEL(:)%CROUGH=' '
TEB_VEG_OPTIONS_MODEL(:)%CSCOND=' '
TEB_VEG_OPTIONS_MODEL(:)%CPEDOTF=' '
TEB_VEG_OPTIONS_MODEL(:)%CPHOTO=' '
TEB_VEG_OPTIONS_MODEL(:)%CALBEDO=' '
TEB_VEG_OPTIONS_MODEL(:)%CC1DRY=' '
TEB_VEG_OPTIONS_MODEL(:)%CSOILFRZ=' '
TEB_VEG_OPTIONS_MODEL(:)%CDIFSFCOND=' '
TEB_VEG_OPTIONS_MODEL(:)%CSNOWRES=' '
TEB_VEG_OPTIONS_MODEL(:)%CRESPSL=' '
TEB_VEG_OPTIONS_MODEL(:)%CCPSURF=' '
TEB_VEG_OPTIONS_MODEL(:)%CRUNOFF=' '
TEB_VEG_OPTIONS_MODEL(:)%CTOPREG=' '
TEB_VEG_OPTIONS_MODEL(:)%CKSAT=' '
TEB_VEG_OPTIONS_MODEL(:)%CSOC=' '
TEB_VEG_OPTIONS_MODEL(:)%CRAIN=' '
TEB_VEG_OPTIONS_MODEL(:)%CHORT=' '
TEB_VEG_OPTIONS_MODEL(:)%NNBIOMASS=0
TEB_VEG_OPTIONS_MODEL(:)%XCGMAX=0.
TEB_VEG_OPTIONS_MODEL(:)%XCDRAG=0.
TEB_VEG_OPTIONS_MODEL(:)%XTSTEP=0.
IF (LHOOK) CALL DR_HOOK("MODD_TEB_VEG_N:TEB_VEG_OPTIONS_ALLOC",1,ZHOOK_HANDLE)
END SUBROUTINE TEB_VEG_OPTIONS_ALLOC

SUBROUTINE TEB_VEG_OPTIONS_DEALLO
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK("MODD_TEB_VEG_N:TEB_VEG_OPTIONS_DEALLO",0,ZHOOK_HANDLE)
IF (ALLOCATED(TEB_VEG_OPTIONS_MODEL)) DEALLOCATE(TEB_VEG_OPTIONS_MODEL)
IF (LHOOK) CALL DR_HOOK("MODD_TEB_VEG_N:TEB_VEG_OPTIONS_DEALLO",1,ZHOOK_HANDLE)
END SUBROUTINE TEB_VEG_OPTIONS_DEALLO

!----------------------------------------------------------------------------

END MODULE MODD_TEB_VEG_n
