!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE FIRE_DYN(PK, PEK, DMK, PTSTEP_DAY, PWFC, PWWILT, PSURFACE_LITTER)
!     ##################################
!
!!****  *FIRE_DYN* -  calculate fire extent
!!
!!    PURPOSE
!!    -------
!!
!!    Fire is treated on a multi daily basis (fireindex has a long-term memory).
!!    Only, fire from natural surfaces are taken into accoun
!!    Fire simple dynamic are inspired from GlobFirm model (Thonickle et al. 2001)
!!    with additional idea from Li et al. (2012)
!!
!!**  METHOD
!!    ------
!!    inspired from Thonickle et al. 2001 = GlobFirm model
!!    inspired from Li et al. 2012
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
!!	R. Alkama
!!	R. Seferian
!!      B. Decharme
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    07/2021
!!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_ISBA_n,           ONLY : ISBA_P_t, ISBA_PE_t
USE MODD_DIAG_MISC_ISBA_n, ONLY : DIAG_MISC_ISBA_t
!
USE MODD_CSTS,           ONLY : XTT, XPI
USE MODD_SURF_PAR,       ONLY : XUNDEF
!
USE MODD_FIRE_PAR,       ONLY : XNATURAL, XLITTER_CRIT, XLITTER_MAX, &
                                XFLAM, XTEMP_LITTER_CRIT, XMIN_FIRE, &
                                XTAU_FIRE
!
USE MODI_FIREFRA_FUNC
!
USE YOMHOOK    ,  ONLY : LHOOK,   DR_HOOK
USE PARKIND1   ,  ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
TYPE(ISBA_P_t),         INTENT(INOUT) :: PK
TYPE(ISBA_PE_t),        INTENT(INOUT) :: PEK
TYPE(DIAG_MISC_ISBA_t), INTENT(INOUT) :: DMK
!
REAL,                 INTENT(IN)    :: PTSTEP_DAY     ! Time step in days
REAL, DIMENSION(:),   INTENT(IN)    :: PWFC
REAL, DIMENSION(:),   INTENT(IN)    :: PWWILT 
!
REAL, DIMENSION(:,:), INTENT(IN)    :: PSURFACE_LITTER ! surface litter pools   (gC/m2)
!
!*      0.2  declarations of local variables
!
!
REAL, DIMENSION(SIZE(PEK%XTEMPLIT_FIRE))   :: ZFIRE_DISTURB       ! fire perturbation
REAL, DIMENSION(SIZE(PEK%XTEMPLIT_FIRE))   :: ZMOIST_EXTINCTION   ! Moisture of extinction
REAL, DIMENSION(SIZE(PEK%XTEMPLIT_FIRE))   :: ZSURFACE_LITTER     ! total litter above the ground for a vegetation type (gC/m2)
REAL, DIMENSION(SIZE(PEK%XTEMPLIT_FIRE))   :: ZFIREIND            ! daily fire index
!
REAL, DIMENSION(SIZE(PEK%XTEMPLIT_FIRE))   :: ZFUEL_AVAILABILITY  ! Fuel availability
REAL, DIMENSION(SIZE(PEK%XTEMPLIT_FIRE))   :: ZX                  ! intermediate variable
!
REAL, DIMENSION(SIZE(PEK%XTEMPLIT_FIRE))   :: ZFLAM               ! parameters pending on pfts/patch
REAL, DIMENSION(SIZE(PEK%XTEMPLIT_FIRE))   :: ZNATURAL            ! parameters pending on pfts/patch
!
LOGICAL, DIMENSION(SIZE(PEK%XTEMPLIT_FIRE)):: GMASK
!
INTEGER                           :: JI,JLIT,JTYPE,INI,INLIT,INTYPE      ! index
!
REAL(KIND=JPRB)                   :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('FIRE_DYN',0,ZHOOK_HANDLE)
!
! 1 Initializations
!
INI    = SIZE(PEK%XTEMPLIT_FIRE)
INLIT  = SIZE(PSURFACE_LITTER,2)
INTYPE = SIZE(PK%XVEGTYPE_PATCH,2)
!
! 1.1 Initialize outputs
!
DMK%XFIREFRA(:) = 0.0
!
! 1.2 Initialize local variables
!
ZFIREIND          (:) = XUNDEF
ZSURFACE_LITTER   (:) = XUNDEF
ZFUEL_AVAILABILITY(:) = XUNDEF
ZMOIST_EXTINCTION (:) = XUNDEF
ZX                (:) = XUNDEF
ZNATURAL          (:) = XUNDEF
ZFLAM             (:) = XUNDEF
!
GMASK(:) = .FALSE.
!
! 1.3 Characterized vegtype assemblage in each patch
!
ZFLAM   (:) = 0.0
ZNATURAL(:) = 0.0
DO JTYPE=1,INTYPE
   DO JI = 1,INI
      ZFLAM    (JI) = ZFLAM    (JI) + XFLAM   (JTYPE) * PK%XVEGTYPE_PATCH(JI,JTYPE)
      ZNATURAL (JI) = ZNATURAL (JI) + XNATURAL(JTYPE) * PK%XVEGTYPE_PATCH(JI,JTYPE)
    ENDDO 
ENDDO  
!
!-----------------------------------------------------------------
!
! 2. Determine fire probability for natural PFTs
!
! 2.1 Fuel availability
!
ZSURFACE_LITTER   (:) = 0.0
DO JLIT=1,INLIT
   DO JI=1,INI
      ZSURFACE_LITTER(JI)=ZSURFACE_LITTER(JI) + PSURFACE_LITTER(JI,JLIT)
   ENDDO
ENDDO
!
ZFUEL_AVAILABILITY(:)= MIN(1.0,MAX(0.0,ZSURFACE_LITTER(:)-XLITTER_CRIT)/(XLITTER_MAX-XLITTER_CRIT))
!
! 2.2 daily fire index.
!
! In original GlobFirm model, the litter moisture of extinction is equal to 20% or 30% of the saturation
! In a physical model, it mean that moisture of extinction is logicaly close to the litter wilting point
! Because we assume that the litter long-term moisture content is equivalent to surface soil moisture,
! moisture of extinction is taken to be equal to the wilting point (possibly adjusted from plant flamability)
!
WHERE(ZNATURAL(:)>=0.8)
      ZMOIST_EXTINCTION(:)=MIN(PWWILT(:)*ZFLAM(:),PWFC(:)) ! moisture of extinction
ELSEWHERE
      ZMOIST_EXTINCTION(:)=0.0
ENDWHERE
!
! Fire only occurs if :
! (1) natural surface
! (2) if there is enough fuel to be burn
! (3) litter long-term moisture content is under field capacity
! (4) litter temperature is above 20°C
!
GMASK(:) = (ZMOIST_EXTINCTION(:)>0.0.AND.ZFUEL_AVAILABILITY(:)>0.0.AND. &
            PEK%XMOISTLIT_FIRE(:)<PWFC(:).AND.(PEK%XTEMPLIT_FIRE(:)-XTT)>XTEMP_LITTER_CRIT)
!
WHERE(GMASK(:))
  ZX      (:) = PEK%XMOISTLIT_FIRE(:)/ZMOIST_EXTINCTION(:)
  ZFIREIND(:) = EXP(-XPI*ZX(:)*ZX(:))
ELSEWHERE
  ZFIREIND(:) = 0.
ENDWHERE
!
! 2.3 long-term fire index
!
PEK%XFIREIND(:) = ((XTAU_FIRE-PTSTEP_DAY) * PEK%XFIREIND(:) + PTSTEP_DAY*ZFIREIND(:)) / XTAU_FIRE
!
WHERE(PEK%XFIREIND(:)<XMIN_FIRE)
      PEK%XFIREIND(:)=0.0
ENDWHERE
!
!-----------------------------------------------------------------
!
! 3. Calculate fire fraction from long-term fireindex and fuel availability
!
DMK%XFIREFRA(:) = FIREFRA_FUNC(PEK%XFIREIND(:)) * ZFUEL_AVAILABILITY(:)
!
WHERE(DMK%XFIREFRA(:)<XMIN_FIRE)
      DMK%XFIREFRA(:)=0.0
ENDWHERE
!
! ------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('FIRE_DYN',1,ZHOOK_HANDLE)
!
END SUBROUTINE FIRE_DYN
