!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE AVERAGED_ALBEDO_TEB(TOP, BOP, KI, KSW, T, TPN, B, GDP, PZENITH, PAZIM, &
                                     PTRANS_HVCR, PSHAD_BEHAV_ANYWAY, PSHAD_BEHAV_ADAPTI,     &
                                     PALB_GARDEN, PALB_GREENROOF, PALB_HVEG, PDIR_ALB_TOWN, PSCA_ALB_TOWN)
!     ###################################################
!
!!**** *AVERAGED_ALBEDO_TEB* computes averaged albedo for TEB scheme
!!
!!    PURPOSE
!!    -------
!!
!!    METHOD
!!    ------
!!   
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!
!!    V. Masson        Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    01/2004
!     C. de Munck & A. Lemonsu   09/2011 Greenroofs
!!    G. Pigeon                  09/2012 B%XTRAN_WIN as arguments
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_BEM_OPTION_n, ONLY : BEM_OPTIONS_t
USE MODD_TEB_OPTION_n, ONLY : TEB_OPTIONS_t
USE MODD_TEB_n, ONLY : TEB_t
USE MODD_BEM_n, ONLY : BEM_t
USE MODD_ISBA_n, ONLY : ISBA_P_t, ISBA_K_t, ISBA_PE_t
USE MODD_TEB_PANEL_n, ONLY : TEB_PANEL_t
USE MODD_TEB_HYDRO_n,  ONLY : TEB_HYDRO_t
USE MODD_DIAG_MISC_TEB_n, ONLY : DIAG_MISC_TEB_t, DIAG_MISC_TEB_INIT
USE MODD_DIAG_MISC_TEB_OPTIONS_n, ONLY : DIAG_MISC_TEB_OPTIONS_t, DIAG_MISC_TEB_OPTIONS_INIT
USE MODD_SURFEX_n, ONLY : TEB_HYDRO_MODEL_t
!
USE MODD_SURF_PAR, ONLY : XUNDEF
USE MODD_TYPE_SNOW
!
USE MODI_URBAN_SOLAR_ABS
USE MODE_SURF_SNOW_FRAC
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
TYPE(TEB_OPTIONS_t), INTENT(INOUT) :: TOP
TYPE(BEM_OPTIONS_t), INTENT(INOUT) :: BOP
TYPE(TEB_t), INTENT(INOUT) :: T
TYPE(BEM_t), INTENT(INOUT) :: B
TYPE(TEB_PANEL_t), INTENT(INOUT) :: TPN
TYPE(ISBA_P_t), INTENT(INOUT) :: GDP
!
INTEGER, INTENT(IN) :: KI                       ! number of points
INTEGER, INTENT(IN) :: KSW                      ! number of shortwave bands
!
REAL, DIMENSION(:), INTENT(IN) :: PZENITH       ! zenithal solar angle
REAL, DIMENSION(:), INTENT(IN) :: PAZIM         ! solar azimuthal angle
!                                               ! (radian from N, clockwise)
REAL, DIMENSION(:), INTENT(IN) :: PTRANS_HVCR   ! transmissivity for all crown of high veg
REAL, DIMENSION(:,:), INTENT(IN) :: PSHAD_BEHAV_ANYWAY  ! Fraction of shades closes anyway
REAL, DIMENSION(:,:), INTENT(IN) :: PSHAD_BEHAV_ADAPTI  ! Fraction of shades available for adaptive closing
REAL, DIMENSION(:), INTENT(IN) :: PALB_GARDEN   ! green areas albedo
REAL, DIMENSION(:), INTENT(IN) :: PALB_GREENROOF! green roof albedo
REAL, DIMENSION(:), INTENT(IN) :: PALB_HVEG     ! high vegetation albedo
REAL, DIMENSION(:), INTENT(OUT):: PDIR_ALB_TOWN ! direct albedo
REAL, DIMENSION(:), INTENT(OUT):: PSCA_ALB_TOWN ! diffuse albedo
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
REAL, DIMENSION(SIZE(T%XBLD)) :: ZDN_ROOF       ! snow fraction 
REAL, DIMENSION(SIZE(T%XBLD)) :: ZDN_ROAD       ! on the surface
REAL, DIMENSION(SIZE(T%XBLD)) :: ZDF_ROOF       ! free-snow fraction 
REAL, DIMENSION(SIZE(T%XBLD)) :: ZDF_ROAD       ! on the surface
LOGICAL, DIMENSION(SIZE(T%XBLD)) :: GMASK       ! .false. (= no snow precip.)
REAL, DIMENSION(SIZE(T%XBLD)) :: ZDIR_SW        ! direct and diffuse shortwave radiation
REAL, DIMENSION(SIZE(T%XBLD)) :: ZSCA_SW        ! to mimic radiation behaviour of town
REAL, DIMENSION(SIZE(T%XBLD)) :: ZREC_SW_GARDEN ! shortwave received by green areas
REAL, DIMENSION(SIZE(T%XBLD)) :: ZREC_SW_ROOF   ! shortwave received by roofs
REAL, DIMENSION(SIZE(T%XBLD)) :: ZREC_SW_WIN    ! shortwave received by walls
REAL, DIMENSION(SIZE(T%XBLD)) :: ZREF_SW_GRND   !
REAL, DIMENSION(SIZE(T%XBLD)) :: ZREF_SW_FAC    !
REAL, DIMENSION(SIZE(T%XBLD)) :: ZREF_SW_HV     !
REAL, DIMENSION(SIZE(T%XBLD)) :: ZE_SHADING     !
!
TYPE(DIAG_MISC_TEB_t) :: YDMT
TYPE(DIAG_MISC_TEB_t) :: YDMTC
TYPE(DIAG_MISC_TEB_OPTIONS_t) :: YDMTO
TYPE(TEB_HYDRO_t)     :: YTH
TYPE(ISBA_PE_t)       :: YPE
TYPE(TEB_t)           :: YT
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!* snow fractions
!  --------------
!
IF (LHOOK) CALL DR_HOOK('AVERAGED_ALBEDO_TEB',0,ZHOOK_HANDLE)
GMASK(:) = .FALSE.
 CALL SNOW_FRAC_ROAD(T%TSNOW_ROAD%WSNOW(:,1),GMASK,ZDN_ROAD,ZDF_ROAD)
 CALL SNOW_FRAC_ROOF(T%TSNOW_ROOF%WSNOW(:,1),GMASK,ZDN_ROOF,ZDF_ROOF)
!
!
!* town  direct and diffuse albedo
!  -------------------------------
!
ZDIR_SW=1.
ZSCA_SW=1.
!
CALL DIAG_MISC_TEB_INIT(YDMT)

ALLOCATE(YTH%XWG_ROAD  (KI,TOP%NTEB_SOIL))
ALLOCATE(YTH%XWG_BLD   (KI,TOP%NTEB_SOIL))
ALLOCATE(YPE%XWG       (KI,TOP%NTEB_SOIL))
ALLOCATE(YTH%XWGI_ROAD (KI,TOP%NTEB_SOIL))
ALLOCATE(YTH%XWGI_BLD  (KI,TOP%NTEB_SOIL))
ALLOCATE(YPE%XWGI      (KI,TOP%NTEB_SOIL))
ALLOCATE(YPE%XWR       (KI))
ALLOCATE(YT%XWS_ROAD   (KI))
ALLOCATE(YT%XWS_ROOF   (KI))
ALLOCATE(YT%TSNOW_ROAD%WSNOW   (KI,1))
ALLOCATE(YT%TSNOW_ROOF%WSNOW   (KI,1))
ALLOCATE(YPE%TSNOW%WSNOW        (KI,SIZE(T%TSNOW_ROAD%WSNOW,2)))
YTH%XWG_ROAD(:,:)  = 0
YTH%XWG_BLD(:,:)   = 0
YTH%XWGI_ROAD(:,:) = 0
YTH%XWGI_BLD(:,:)  = 0
YPE%XWG(:,:)       = 0
YPE%XWGI(:,:)      = 0
YPE%XWR(:)         = 0
YT%XWS_ROAD(:)     = 0
YT%XWS_ROOF(:)     = 0
CALL DIAG_MISC_TEB_INIT_n(YDMTC, YDMT, YDMTO, TOP, BOP, KI, KSW, YTH, YPE, YT)
!
CALL URBAN_SOLAR_ABS(TOP, T, B, YDMT, GDP, ZDIR_SW, ZSCA_SW, PZENITH, PAZIM,   &
                     TPN%XFRAC_PANEL, TPN%XALB_PANEL, PALB_GARDEN,        &
                     PALB_GREENROOF, PALB_HVEG, ZDN_ROOF, ZDF_ROOF,       &
                     ZDN_ROAD, ZDF_ROAD,                                  &
                     PTRANS_HVCR,                                         &  
                     ZREC_SW_GARDEN,      &
                     ZREC_SW_ROOF, PDIR_ALB_TOWN, PSCA_ALB_TOWN,          &
                     ZREC_SW_WIN, ZREF_SW_GRND,           &
                     ZREF_SW_FAC, ZREF_SW_HV, ZE_SHADING,                 &
                     PSHAD_BEHAV_ANYWAY,PSHAD_BEHAV_ADAPTI,               &
                     OALB_ONLY=.TRUE.                                     )
!
IF (LHOOK) CALL DR_HOOK('AVERAGED_ALBEDO_TEB',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE AVERAGED_ALBEDO_TEB
