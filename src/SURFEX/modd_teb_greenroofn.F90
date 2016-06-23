!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!##################
MODULE MODD_TEB_GREENROOF_n
!##################
!
!!****  *MODD_TEB_GREENROOF - declaration of ISBA scheme packed surface parameters for urban green roofs
!!
!!    PURPOSE
!!    -------
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
!!      A. Lemonsu *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original       09/2009
!!      C. de Munck     06/2011 
!!      V. Masson       06/2013  splits module in 4
!!
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
USE MODD_TYPE_SNOW
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE

TYPE TEB_GREENROOF_1P_t
!-------------------------------------------------------------------------------
!
! Prognostic variables:
!
! - Snow Cover:
!
  TYPE(SURF_SNOW)                :: TSNOW            ! snow state: 
                                                     ! scheme type/option                      (-)
                                                     ! number of layers                        (-)
                                                     ! snow (& liq. water) content             (kg/m2)
                                                     ! heat content                            (J/m2)
                                                     ! temperature                             (K)
                                                     ! density                                 (kg m-3)
!
! - Soil and vegetation heat and water:
!
  REAL, POINTER, DIMENSION(:)    :: XWR              ! liquid water retained on the
                                                     ! foliage of the vegetation
                                                     ! canopy                                  (kg/m2)
  REAL, POINTER, DIMENSION(:,:)  :: XTG              ! surface and sub-surface soil 
                                                     ! temperature profile                     (K)
  REAL, POINTER, DIMENSION(:,:)  :: XWG              ! soil volumetric water content profile   (m3/m3)
  REAL, POINTER, DIMENSION(:,:)  :: XWGI             ! soil liquid water equivalent volumetric 
                                                     ! ice content profile                     (m3/m3)
  REAL, POINTER, DIMENSION(:)    :: XRESA            ! aerodynamic resistance                  (s/m)
!

! - Vegetation: Ags Prognostic (YPHOTO = 'AGS', 'LAI', 'AST', 'LST', 'NIT', 'NCB')
!
  REAL, POINTER, DIMENSION(:)    :: XAN              ! net CO2 assimilation                    (mg/m2/s)
  REAL, POINTER, DIMENSION(:)    :: XANDAY           ! daily net CO2 assimilation              (mg/m2)
  REAL, POINTER, DIMENSION(:)    :: XANFM            ! maximum leaf assimilation               (mg/m2/s)
  REAL, POINTER, DIMENSION(:)    :: XLE              ! evapotranspiration                      (W/m2)
  REAL, POINTER, DIMENSION(:)     :: XFAPARC       ! Fapar of vegetation (cumul)
  REAL, POINTER, DIMENSION(:)     :: XFAPIRC       ! Fapir of vegetation (cumul)
  REAL, POINTER, DIMENSION(:)     :: XLAI_EFFC     ! Effective LAI (cumul)
  REAL, POINTER, DIMENSION(:)     :: XMUS          ! cos zenithal angle (cumul)    
!
! - Vegetation: Ags Prognostic (YPHOTO = 'NIT', 'NCB')
!
  REAL, POINTER, DIMENSION(:,:)  :: XRESP_BIOMASS    ! daily cumulated respiration of 
                                                     ! biomass                              (kg/m2/s)
  REAL, POINTER, DIMENSION(:,:)  :: XBIOMASS         ! biomass of previous day              (kg/m2) 
!
! - SGH scheme
!                                                     
  REAL, POINTER, DIMENSION(:)    :: XKSAT_ICE       ! hydraulic conductivity at saturation
                                                    ! over frozen area (m s-1)                                     
!-------------------------------------------------------------------------------
!
! - Snow and flood fractions and total albedo at time t:
!
  REAL, POINTER, DIMENSION(:)    :: XPSNG              ! Snow fraction over ground
  REAL, POINTER, DIMENSION(:)    :: XPSNV              ! Snow fraction over vegetation
  REAL, POINTER, DIMENSION(:)    :: XPSNV_A            ! Snow fraction over vegetation
  REAL, POINTER, DIMENSION(:)    :: XPSN               ! Total Snow fraction
! 
  REAL, POINTER, DIMENSION(:)    :: XSNOWFREE_ALB      ! snow free albedo                        (-)
  REAL, POINTER, DIMENSION(:)    :: XSNOWFREE_ALB_VEG  ! snow free albedo for vegetation         (-)
  REAL, POINTER, DIMENSION(:)    :: XSNOWFREE_ALB_SOIL ! snow free albedo for soil               (-)
!
!-------------------------------------------------------------------------------
!
END TYPE TEB_GREENROOF_1P_t
!
TYPE TEB_GREENROOF_t
  !
  TYPE(TEB_GREENROOF_1P_t), POINTER :: ALP(:) => NULL()
  TYPE(TEB_GREENROOF_1P_t), POINTER :: CUR => NULL()
  !
END TYPE TEB_GREENROOF_t
!
 CONTAINS
!
!
SUBROUTINE TEB_GREENROOF_GOTO_PATCH(YTEB_GREENROOF,KTO_PATCH)
        TYPE(TEB_GREENROOF_t), INTENT(INOUT) :: YTEB_GREENROOF
INTEGER, INTENT(IN) :: KTO_PATCH
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
! Current patch is set to patch KTO_PATCH
IF (LHOOK) CALL DR_HOOK('MODD_TEB_GREENROOF_N:TEB_GREENROOF_GOTO_PATCH',0,ZHOOK_HANDLE)

YTEB_GREENROOF%CUR => YTEB_GREENROOF%ALP(KTO_PATCH)

IF (LHOOK) CALL DR_HOOK('MODD_TEB_GREENROOF_N:TEB_GREENROOF_GOTO_PATCH',1,ZHOOK_HANDLE)
!
END SUBROUTINE TEB_GREENROOF_GOTO_PATCH
!
SUBROUTINE TEB_GREENROOF_INIT(YTEB_GREENROOF,KPATCH)
TYPE(TEB_GREENROOF_t), INTENT(INOUT) :: YTEB_GREENROOF
INTEGER, INTENT(IN) :: KPATCH
INTEGER :: JP
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK("MODD_TEB_GREENROOF_N:TEB_GREENROOF_INIT",0,ZHOOK_HANDLE)
 ALLOCATE(YTEB_GREENROOF%ALP(KPATCH))
 YTEB_GREENROOF%CUR => YTEB_GREENROOF%ALP(1)
DO JP=1,KPATCH
  NULLIFY(YTEB_GREENROOF%ALP(JP)%XWR)
  NULLIFY(YTEB_GREENROOF%ALP(JP)%XTG)
  NULLIFY(YTEB_GREENROOF%ALP(JP)%XWG)
  NULLIFY(YTEB_GREENROOF%ALP(JP)%XWGI)
  NULLIFY(YTEB_GREENROOF%ALP(JP)%XRESA)
  NULLIFY(YTEB_GREENROOF%ALP(JP)%XAN)
  NULLIFY(YTEB_GREENROOF%ALP(JP)%XANDAY)
  NULLIFY(YTEB_GREENROOF%ALP(JP)%XANFM)
  NULLIFY(YTEB_GREENROOF%ALP(JP)%XLE)
  NULLIFY(YTEB_GREENROOF%ALP(JP)%XFAPARC)
  NULLIFY(YTEB_GREENROOF%ALP(JP)%XFAPIRC)
  NULLIFY(YTEB_GREENROOF%ALP(JP)%XLAI_EFFC)  
  NULLIFY(YTEB_GREENROOF%ALP(JP)%XMUS)     
  NULLIFY(YTEB_GREENROOF%ALP(JP)%XRESP_BIOMASS)
  NULLIFY(YTEB_GREENROOF%ALP(JP)%XBIOMASS)
  NULLIFY(YTEB_GREENROOF%ALP(JP)%XKSAT_ICE)
  NULLIFY(YTEB_GREENROOF%ALP(JP)%XPSNG)
  NULLIFY(YTEB_GREENROOF%ALP(JP)%XPSNV)
  NULLIFY(YTEB_GREENROOF%ALP(JP)%XPSNV_A)
  NULLIFY(YTEB_GREENROOF%ALP(JP)%XPSN)
  NULLIFY(YTEB_GREENROOF%ALP(JP)%XSNOWFREE_ALB)
  NULLIFY(YTEB_GREENROOF%ALP(JP)%XSNOWFREE_ALB_VEG)
  NULLIFY(YTEB_GREENROOF%ALP(JP)%XSNOWFREE_ALB_SOIL)
ENDDO 
IF (LHOOK) CALL DR_HOOK("MODD_TEB_GREENROOF_N:TEB_GREENROOF_INIT",1,ZHOOK_HANDLE)
END SUBROUTINE TEB_GREENROOF_INIT


END MODULE MODD_TEB_GREENROOF_n
