!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
    SUBROUTINE URBAN_SOLAR_ABS(TOP, T, B, DMT, GDP, PDIR_SW, PSCA_SW, PZENITH, PAZIM,  &
                               PFRAC_PANEL, PALB_PANEL, PALB_GD, PALB_GR, PALB_HVEG,&
                               PDN_RF, PDF_RF, PDN_RD, PDF_RD, PTRANS_HVCR,         &
                               PREC_SW_GD, PREC_SW_RF, PDIR_ALB_TWN, PSCA_ALB_TWN,  &
                               PREC_SW_WIN, PREF_SW_GRND, PREF_SW_FAC,  &
                               PREF_SW_HV,                                          &
                               PE_SHADING, PSHAD_BEHAV_ANYWAY,PSHAD_BEHAV_ADAPTI,   &
                               OALB_ONLY )
!   ##########################################################################
!
!!****  *URBAN_SOLAR_ABS*  
!!
!!    PURPOSE
!!    -------
!
!     Computes the solar radiation flux absorbed by roofs, roads and walls.
!     The absorption by roofs is trivial.
!         
!     
!!**  METHOD
!     ------
!
!
!        computation of input solar radiation on each surface
!        ****************************************************
!
!    direct fluxes:
!    -------------
!
!    dir_Rg_road (Wm-2) =   S * 2*theta0/pi
!                         - S *2/tan(zen) * h/W /pi * (1-cos(theta0))
!
!    dir_Rg_wall (Wm-2) =   S / tan(zen) /pi * (1-cos(theta0))
!                         + S * W/h * (1/2 -theta0/pi)
!
!   where zen      is the zenithal angle, from horizon
!         h/W      is the aspect ratio of the canyon
!         S        is the direct solar radiation flux on a horizontal surface
!
!         theta0 = arcsin(min(W/h * tan(zen),1))
!
!   The surfaces will keep (1-a) times these fluxes, and reflect the
!   remaining
!
!    scattered fluxes:
!    ----------------
!
!   sca_Rg_road = sca_Rg * SVF_road
!
!   sca_Rg_wall = sca_Rg * SVF_wall
!
!
!    solar flux and isotropic reflections :
!    ------------------------------------
!
!  after 0 reflection, the absorbed part of the flux is:
!
!      ARg_r(0) = (1-a_r) (sca_Rg_road + dir_Rg_road)
!
!      ARg_w(0) = (1-a_w) (sca_Rg_wall + dir_Rg_wall)
!  
!    and the reflected parts are
!
!      RRg_r(0) = a_r (sca_Rg_road + dir_Rg_road)
!
!      RRg_w(0) = a_w (sca_Rg_wall + dir_Rg_wall)
!
!  after n reflection:
!
!      ARg_r(n) = ARg_r(n-1) + RRg_w(n-1) * (1-  SVF_r)(1-a_r)
!
!      ARg_w(n) = ARg_w(n-1) + RRg_r(n-1) *      SVF_w (1-a_w)
!                            + RRg_w(n-1) * (1-2*SVF_w)(1-a_w)
!
!      RRg_r(n) = (1- SVF_r) a_r RRg_w(n-1)
!
!      RRg_w(n) =     SVF_w  a_w RRg_r(n-1)
!                +(1-2SVF_w) a_w RRg_w(n-1)
!
!
!   i.e.
!                                               n-1
!      ARg_r(n) = ARg_r(0) + (1-  SVF_r)(1-a_r) SUM RRg_w(k)
!                                               k=0
!
!                                               n-1
!      ARg_w(n) = ARg_w(0) +      SVF_w (1-a_w) SUM RRg_r(k)
!                                               k=0
!                                               n-1
!                          + (1-2*SVF_w)(1-a_w) SUM RRg_w(k)
!                                               k=0
!
! with
!
!     n                             n-1
!    SUM RRg_r(k) = (1-  SVF_r) a_r SUM RRg_w(k)      +  RRg_r(0)
!    k=0                            k=0
!
!     n                             n-1
!    SUM RRg_w(k) =      SVF_w  a_w SUM RRg_r(k) 
!    k=0                            k=0
!                                   n-1
!                  +(1-2*SVF_w) a_w SUM RRg_w(k)      +  RRg_w(0)
!                                   k=0
!
!
!   Then
!
!     n                                        n-1
!    SUM RRg_w(k) =  (1-2*SVF_w)       a_w     SUM RRg_w(k)
!    k=0                                       k=0
!                                              n-2
!                  + (1-  SVF_r) SVF_w a_w a_r SUM RRg_w(k) 
!                                              k=0
!
!                  + RRg_w(0) + SVF_w a_w RRg_r(0)
!
!
!
!
!  solving this system, lead after an infinity of reflections/absorptions:
!
!    inf                      RRg_w(0) + SVF_w a_w RRg_r(0)
!    SUM RRg_w(k) = ----------------------------------------------------
!    k=0             1 - (1-2*SVF_w) a_w - (1-  SVF_r) SVF_w a_w a_r
!
!
!    inf            (1-  SVF_r) a_r ( a_w SVF_w RRg_r(0) + RRg_w(0) )
!    SUM RRg_r(k) = ------------------------------------------------------------ + RRg_r(0)
!    k=0             1 - (1-2*SVF_w) a_w - (1-  SVF_r) SVF_w a_w a_r
!
!
! ARg_r(n) and ARg_w(n) follow
!
!
! If snow is present, the albedos in all these formulae (and only these,
! not the final net radiation budget) are modified by the albedo of the
! snow-covered surface.
! 
!
!
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    MODD_CST
!!
!!      
!!    REFERENCE
!!    ---------
!!
!!      
!!    AUTHOR
!!    ------
!!
!!      V. Masson           * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    23/01/98 
!!                  21/11/00 (V. Masson)  bug in reflections for roads
!!                     12/02 (A. Lemonsu) bug in diagnostic of albedo
!!                     12/11 (V. Masson ) adds road direction option
!!                     01/12 (V. Masson ) adds 2 different wall direct insulations
!!                     04/12 (G. Pigeon) add PTRAN_WIN 
!!                     09/12 (C. de Munck-A. Lemonsu) add green roofs
!!                       /15 (V. Masson) add solar panels              
!!                     01/16 (E. Redon - A. Lemonsu) add high vegetation  
!!                     12/16 (V. Masson) bug in reflections by facades: wall
!!                                       albedo was used instead of wall+window albedo            
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_TEB_OPTION_n, ONLY : TEB_OPTIONS_t
USE MODD_TEB_n, ONLY : TEB_t
USE MODD_ISBA_n, ONLY : ISBA_P_t, ISBA_K_t
USE MODD_BEM_n, ONLY : BEM_t
USE MODD_DIAG_MISC_TEB_n, ONLY : DIAG_MISC_TEB_t
!
USE MODD_CSTS,     ONLY : XPI
USE MODD_BEM_CST,  ONLY : XWIN_SW_MAX
USE MODD_SURF_PAR, ONLY : XUNDEF, XSURF_EPSILON
!
USE MODI_WINDOW_SHADING
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
TYPE(TEB_OPTIONS_t), INTENT(INOUT) :: TOP
              ! 'UNIF' : classical TEB version, all walls are identical
              ! 'TWO ' : the two opposite walls are different & receive different solar energy
TYPE(TEB_t), INTENT(INOUT) :: T
TYPE(BEM_t), INTENT(INOUT) :: B
TYPE(ISBA_P_t), INTENT(INOUT) :: GDP
TYPE(DIAG_MISC_TEB_t), INTENT(INOUT) :: DMT
!
REAL, DIMENSION(:), INTENT(IN)    :: PDIR_SW           ! incoming direct solar radiation
REAL, DIMENSION(:), INTENT(IN)    :: PSCA_SW           ! scattered incoming solar rad.
REAL, DIMENSION(:), INTENT(IN)    :: PZENITH           ! solar zenithal angle
REAL, DIMENSION(:), INTENT(IN)    :: PAZIM             ! solar azimuthal angle
!                                                      ! (radian from N, clockwise)
REAL, DIMENSION(:), INTENT(IN)    :: PFRAC_PANEL       ! Fraction of solar panel on roofs (-)
REAL, DIMENSION(:), INTENT(IN)    :: PALB_PANEL        ! Albedo     of solar panels (-)
REAL, DIMENSION(:), INTENT(IN)    :: PALB_GD       ! GD areas albedo
REAL, DIMENSION(:), INTENT(IN)    :: PALB_GR       ! green roof albedo
REAL, DIMENSION(:), INTENT(IN)    :: PALB_HVEG         ! high-vegetation albedo
REAL, DIMENSION(:), INTENT(IN)    :: PDN_RF          ! snow-covered roof fraction
REAL, DIMENSION(:), INTENT(IN)    :: PDF_RF          ! snow-free    roof fraction
REAL, DIMENSION(:), INTENT(IN)    :: PDN_RD          ! snow-covered road fraction
REAL, DIMENSION(:), INTENT(IN)    :: PDF_RD          ! snow-free    road fraction
!
!new argument for parametrization of urban trees 
REAL, DIMENSION(:),   INTENT(IN)     :: PTRANS_HVCR      ! transmissivity for all crown of high veg
!
!new arguments for shading
REAL, DIMENSION(:), INTENT(OUT)    :: PREC_SW_GD     ! solar radiation received
!                                                    ! by GD areas
REAL, DIMENSION(:), INTENT(OUT)    :: PREC_SW_RF     ! solar radiation received
!                                                    ! by RF areas (below solar panels if any)
REAL, DIMENSION(:), INTENT(OUT)    :: PDIR_ALB_TWN   ! town direct albedo
REAL, DIMENSION(:), INTENT(OUT)    :: PSCA_ALB_TWN   ! town diffuse albedo
!
REAL, DIMENSION(:), INTENT(OUT)    :: PREC_SW_WIN      ! solar radiation received by windows

REAL, DIMENSION(:), INTENT(OUT)    :: PREF_SW_GRND     ! total solar radiation reflected by ground
REAL, DIMENSION(:), INTENT(OUT)    :: PREF_SW_FAC      ! total solar radiation reflected by wall
REAL, DIMENSION(:), INTENT(OUT)    :: PREF_SW_HV       ! total solar radiation reflected by high vegetation
!new arguments for shading
REAL, DIMENSION(:), INTENT(OUT)    :: PE_SHADING       ! Energy that is not reflected 
                                                       ! by the shading, nor transmitted through
                                                       ! the bld, nor absorbed by the
                                                       ! [W/m2(win)]
REAL, DIMENSION(:,:), INTENT(IN)   :: PSHAD_BEHAV_ANYWAY  ! Fraction of shades closes anyway
REAL, DIMENSION(:,:), INTENT(IN)   :: PSHAD_BEHAV_ADAPTI  ! Fraction of shades available for adaptive closing
!
!
LOGICAL, INTENT(IN), OPTIONAL :: OALB_ONLY
!
!*      0.2    declarations of local variables
!
!                                                           
REAL, DIMENSION(SIZE(PDIR_SW)) :: ZDIR_SW               ! direct and diffuse incoming radiation
REAL, DIMENSION(SIZE(PDIR_SW)) :: ZSCA_SW               ! with a minimum to compute albedo
REAL, DIMENSION(SIZE(PDIR_SW)) :: ZDIR_SW_UP            ! direct outgoing radiation
REAL, DIMENSION(SIZE(PDIR_SW)) :: ZSCA_SW_UP            ! diffuse outgoing radiation
REAL, DIMENSION(SIZE(PDIR_SW)) :: ZSW_UP_ROOF           ! direct + diffuse reflected radiation by roofs
REAL, DIMENSION(SIZE(PDIR_SW)) :: ZSW_UP_CAN            ! direct + diffuse reflected radiation by the canyon
REAL, DIMENSION(SIZE(PDIR_SW)) :: ZTANZEN               ! tangente of solar zenithal angle
REAL, DIMENSION(SIZE(PDIR_SW)) :: ZTHETA0_R             ! canyon angle for
!                                                       ! which solar
!                                                       ! radiation
!                                                       ! reaches the road
REAL, DIMENSION(SIZE(PDIR_SW)) :: ZTHETA0_HV            ! canyon angle for
!                                                       ! which solar
!                                                       ! radiation
!                                                       ! reaches the road
!
REAL, DIMENSION(SIZE(PDIR_SW)) :: ZAALB_RD            ! averaged albedo
REAL, DIMENSION(SIZE(PDIR_SW)) :: ZDIR_SW_RD          ! direct radiation reaching
REAL, DIMENSION(SIZE(PDIR_SW)) :: ZDIR_SW_WL_A        ! road, wall A,
REAL, DIMENSION(SIZE(PDIR_SW)) :: ZDIR_SW_WL_B        ! wall B,
REAL, DIMENSION(SIZE(PDIR_SW)) :: ZDIR_SW_GD          ! GD areas,
REAL, DIMENSION(SIZE(PDIR_SW)) :: ZDIR_SW_HV          ! high vegetation areas,
REAL, DIMENSION(SIZE(PDIR_SW)) :: ZDIR_SW_WL          ! and on average on 2 walls
REAL, DIMENSION(SIZE(PDIR_SW)) :: ZSCA_SW_RD          ! diffuse radiation reaching
REAL, DIMENSION(SIZE(PDIR_SW)) :: ZSCA_SW_WL          ! road, wall,
REAL, DIMENSION(SIZE(PDIR_SW)) :: ZSCA_SW_GD          ! and GD areas
REAL, DIMENSION(SIZE(PDIR_SW)) :: ZSCA_SW_HV          ! and high vegetation areas
REAL, DIMENSION(SIZE(PDIR_SW)) :: ZNTR_DIR_SW_HV      ! direct SR no transmitted (refl/abs) by high veg
!
REAL, DIMENSION(SIZE(PDIR_SW)) :: ZCOEFF_RT           ! coefficient for constraining reflections of HV to R in SWrad upward
REAL, DIMENSION(SIZE(PDIR_SW)) :: ZCOEFF_WT           ! coefficient for constraining reflections of HV to W in SWrad upward
!
!
REAL, DIMENSION(SIZE(PDIR_SW)) :: ZABS_DIR_SW_RF      ! solar radiation
REAL, DIMENSION(SIZE(PDIR_SW)) :: ZABS_DIR_SW_RD      ! absorbed by roofs,
REAL, DIMENSION(SIZE(PDIR_SW)) :: ZABS_DIR_SW_WL_A    ! road, wall A,
REAL, DIMENSION(SIZE(PDIR_SW)) :: ZABS_DIR_SW_WL_B    ! wall B,
REAL, DIMENSION(SIZE(PDIR_SW)) :: ZABS_DIR_SW_WL      ! both walls on average,
REAL, DIMENSION(SIZE(PDIR_SW)) :: ZABS_DIR_SW_GD      ! GD areas,
REAL, DIMENSION(SIZE(PDIR_SW)) :: ZABS_DIR_SW_HV      ! direct SR absorbed by high veg
REAL, DIMENSION(SIZE(PDIR_SW)) :: ZABS_DIR_SW_GR      ! green roof areas,
REAL, DIMENSION(SIZE(PDIR_SW)) :: ZABS_DIR_SW_PANEL     ! solar panels,
REAL, DIMENSION(SIZE(PDIR_SW)) :: ZABS_DIR_SW_WIN       ! window (abs+trans), and snow
REAL, DIMENSION(SIZE(PDIR_SW)) :: ZABS_DIR_SW_SN_RF ! over roof, wall,
REAL, DIMENSION(SIZE(PDIR_SW)) :: ZABS_DIR_SW_SN_RD ! and GD areas
REAL, DIMENSION(SIZE(PDIR_SW)) :: ZABS_DIR_SW_SKY       ! sky   
REAL, DIMENSION(SIZE(PDIR_SW)) :: ZABS_SCA_SW_RF      ! solar radiation
REAL, DIMENSION(SIZE(PDIR_SW)) :: ZABS_SCA_SW_RD      ! absorbed by roofs,
REAL, DIMENSION(SIZE(PDIR_SW)) :: ZABS_SCA_SW_WL      ! road, wall,
REAL, DIMENSION(SIZE(PDIR_SW)) :: ZABS_SCA_SW_GD    ! GD areas,
REAL, DIMENSION(SIZE(PDIR_SW)) :: ZABS_SCA_SW_HV      ! high vegetation areas,
REAL, DIMENSION(SIZE(PDIR_SW)) :: ZABS_SCA_SW_GR    ! green roof areas,
REAL, DIMENSION(SIZE(PDIR_SW)) :: ZABS_SCA_SW_PANEL     ! solar panels,
REAL, DIMENSION(SIZE(PDIR_SW)) :: ZABS_SCA_SW_WIN       ! window (abs+trans), and snow
REAL, DIMENSION(SIZE(PDIR_SW)) :: ZABS_SCA_SW_SN_RF ! over roof and wall,
REAL, DIMENSION(SIZE(PDIR_SW)) :: ZABS_SCA_SW_SN_RD ! coming from diffuse rad.
REAL, DIMENSION(SIZE(PDIR_SW)) :: ZABS_SCA_SW_SKY       ! sky       
REAL, DIMENSION(SIZE(PDIR_SW)) :: ZDW                   ! difference of radiation
!                                                       ! absorbed by the 2 walls
REAL, DIMENSION(SIZE(PDIR_SW)) :: ZREF_DIR_SW_RF      ! solar radiation
REAL, DIMENSION(SIZE(PDIR_SW)) :: ZREF_DIR_SW_RD      ! refelcted by roofs,
REAL, DIMENSION(SIZE(PDIR_SW)) :: ZREF_DIR_SW_WL      ! both walls on average,
REAL, DIMENSION(SIZE(PDIR_SW)) :: ZREF_DIR_SW_GD      ! GARDEN areas,
REAL, DIMENSION(SIZE(PDIR_SW)) :: ZREF_DIR_SW_HV      ! GARDEN areas,
REAL, DIMENSION(SIZE(PDIR_SW)) :: ZREF_SCA_SW_RF      ! solar radiation
REAL, DIMENSION(SIZE(PDIR_SW)) :: ZREF_SCA_SW_RD      ! refelcted by roofs,
REAL, DIMENSION(SIZE(PDIR_SW)) :: ZREF_SCA_SW_WL      ! both walls on average,
REAL, DIMENSION(SIZE(PDIR_SW)) :: ZREF_SCA_SW_GD      ! GARDEN areas,
REAL, DIMENSION(SIZE(PDIR_SW)) :: ZREF_SCA_SW_HV      ! GARDEN areas,
!
!
REAL, DIMENSION(SIZE(PDIR_SW)) :: ZRD                 !
REAL, DIMENSION(SIZE(PDIR_SW)) :: ZRD_DIR             ! Road direction
!                                                     ! (radian from N, clockwise)
REAL, DIMENSION(SIZE(PDIR_SW)) :: ZGD                 !  garden fraction in the canyon
!
REAL, DIMENSION(SIZE(PDIR_SW)) :: ZREC_DIR_SW_WIN
REAL, DIMENSION(SIZE(PDIR_SW)) :: ZREC_SCA_SW_WIN
REAL, DIMENSION(SIZE(PDIR_SW)) :: ZAALB_WL
!
REAL, DIMENSION(SIZE(PDIR_SW)) :: ZFRAC_SHAD
REAL, DIMENSION(SIZE(PDIR_SW),SIZE(B%XTI_BLD,2)) :: ZTRAN_WIN    ! solar transmittivity of windows
REAL, DIMENSION(SIZE(PDIR_SW),SIZE(B%XTI_BLD,2)) :: ZABS_WIN     ! solar transmittivity of windows
REAL, DIMENSION(SIZE(PDIR_SW),SIZE(B%XTI_BLD,2)) :: ZALB_WIN     ! solar reflectance of windows
REAL, DIMENSION(SIZE(PDIR_SW))                   :: ZAGG_ABS_WIN ! solar transmittivity of windows
REAL, DIMENSION(SIZE(PDIR_SW),SIZE(B%XTI_BLD,2)) :: ZEFF_SHAD    ! Indicator for shading status
REAL, DIMENSION(SIZE(PDIR_SW),SIZE(B%XTI_BLD,2)) :: ZE_SHADING 
!
LOGICAL :: GALB_ONLY
INTEGER                        :: JJ, JCOMP ! loop index
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('URBAN_SOLAR_ABS',0,ZHOOK_HANDLE)
!
GALB_ONLY = .FALSE.
IF (PRESENT(OALB_ONLY)) GALB_ONLY = OALB_ONLY
!
ZDIR_SW = MAX(PDIR_SW,0.)
ZSCA_SW = MAX(PSCA_SW,0.000001)
!
ZRD_DIR = T%XROAD_DIR(:) * XPI/180.
!
ZDIR_SW_UP        (:) = 0.
ZSCA_SW_UP        (:) = 0.
ZSW_UP_ROOF       (:) = 0.
ZSW_UP_CAN        (:) = 0.
!
DO JJ=1,SIZE(T%XROAD)
!
  IF (T%XROAD(JJ)+T%XGARDEN(JJ).NE.0.) THEN
    ZRD(JJ)   = T%XROAD(JJ) / (T%XROAD(JJ)+T%XGARDEN(JJ))
    ZGD(JJ) = T%XGARDEN(JJ) / (T%XROAD(JJ)+T%XGARDEN(JJ))
  ELSE
    ZRD(JJ)=0.
    ZGD(JJ)=0.
  ENDIF
!
!
!-------------------------------------------------------------------------------
!
!*      1.     SOLAR RADIATIONS FOR ROOFS
!              --------------------------
!
!* One supposes that solar panels, if present, intercept all solar radiation
!
  ZABS_DIR_SW_PANEL    (JJ) = ZDIR_SW(JJ) * (1. - PALB_PANEL    (JJ))
  ZABS_SCA_SW_PANEL    (JJ) = ZSCA_SW(JJ) * (1. - PALB_PANEL    (JJ))
!
!* solar energy received by the surfaces below solar panels
  ZABS_DIR_SW_RF   (JJ) = ZDIR_SW(JJ) * (1. - T%XALB_ROOF     (JJ)) * (1.-PFRAC_PANEL(JJ))
  ZABS_DIR_SW_SN_RF(JJ) = ZDIR_SW(JJ) * (1. - T%TSNOW_ROOF%ALB(JJ)) * (1.-PFRAC_PANEL(JJ))
  ZABS_DIR_SW_GR   (JJ) = ZDIR_SW(JJ) * (1. - PALB_GR         (JJ)) * (1.-PFRAC_PANEL(JJ))
  ZABS_SCA_SW_RF   (JJ) = ZSCA_SW(JJ) * (1. - T%XALB_ROOF     (JJ)) * (1.-PFRAC_PANEL(JJ))
  ZABS_SCA_SW_SN_RF(JJ) = ZSCA_SW(JJ) * (1. - T%TSNOW_ROOF%ALB(JJ)) * (1.-PFRAC_PANEL(JJ))
  ZABS_SCA_SW_GR   (JJ) = ZSCA_SW(JJ) * (1. - PALB_GR         (JJ)) * (1.-PFRAC_PANEL(JJ))
!
!-------------------------------------------------------------------------------
!
!*      2.     SOLAR RADIATIONS FOR ROADS AND WALLS
!              ------------------------------------
!
  IF (ABS(0.5*XPI-PZENITH(JJ)) <  1.E-6) THEN
    IF(0.5*XPI-PZENITH(JJ) > 0.)  ZTANZEN(JJ)=TAN(0.5*XPI-1.E-6)
    IF(0.5*XPI-PZENITH(JJ) <= 0.) ZTANZEN(JJ)=TAN(0.5*XPI+1.E-6)
  ELSEIF (ABS(PZENITH(JJ)) <  1.E-6) THEN
    ZTANZEN(JJ)=SIGN(1.,PZENITH(JJ))*TAN(1.E-6)
  ELSE
    ZTANZEN(JJ) = TAN(PZENITH(JJ))
  ENDIF
!
!
! Case with buildings
! -------------------
  IF (T%XBLD(JJ) .GT. 0.) THEN
!
!*      2.1    radiation coefficients
!              ----------------------
!
!* a.    Case with isotropic street orientations
!        ---------------------------------------
IF (TOP%CROAD_DIR=='UNIF') THEN
  ZTHETA0_R(JJ) = ASIN( MIN(ABS( 1./ZTANZEN(JJ))/T%XCAN_HW_RATIO(JJ), 1. ) )
!
!
!*      2.2    direct solar radiation received by high vegetation (above HV, before interception)
!              ----------------------------------------------------------------------------------
!
!     * a.1.  Case with explicit high vegetation
!             ----------------------------------
  IF (T%XURBTREE(JJ).GT.0.) THEN
!
!       thetha0_hveg = (h/w)*((hbld-htree)/h)=(hbld-htree)/w
!
    ZTHETA0_HV(JJ) = ASIN( MAX(MIN(ABS( 1./ZTANZEN(JJ))/(T%XCAN_HW_RATIO(JJ)   &
                   * ((MAX(1.0E-6,T%XBLD_HEIGHT(JJ)-GDP%XH_TREE(JJ)))/T%XBLD_HEIGHT(JJ))), &
                   (1.0-XSURF_EPSILON) ), (-1.0+XSURF_EPSILON) ))
!
    ZDIR_SW_HV(JJ)   = (  ZDIR_SW(JJ) * 2. * ZTHETA0_HV(JJ) / XPI                          &
                   - ZDIR_SW(JJ) * 2. * ZTANZEN(JJ) / XPI                                 &
                   * (T%XCAN_HW_RATIO(JJ)*((T%XBLD_HEIGHT(JJ)-GDP%XH_TREE(JJ))/T%XBLD_HEIGHT(JJ)))  &
                   * (1.-COS(ZTHETA0_HV(JJ)))  ) 
!
!       flux of direct solar radiation reflected or absorbed by high vegetation
!              (in other words, not transmitted)
!
    ZNTR_DIR_SW_HV(JJ) = ZDIR_SW_HV(JJ)*(1.-PTRANS_HVCR(JJ))
!
!     * a.2.  Case without explicit high vegetation
!             -------------------------------------
  ELSE
    ZDIR_SW_HV(JJ)     = 0.
    ZNTR_DIR_SW_HV(JJ) = 0.
  ENDIF
!
!*      2.3    direct solar radiation received by roads and garden areas
!              ---------------------------------------------------------
!
  ZDIR_SW_RD (JJ) = (  ZDIR_SW(JJ) - T%XURBTREE(JJ)*ZNTR_DIR_SW_HV(JJ) )  &
                *( 2. * ZTHETA0_R(JJ) / XPI                             &
                -2. * ZTANZEN(JJ) / XPI                               &
                * T%XCAN_HW_RATIO(JJ) * (1.-COS(ZTHETA0_R(JJ)))  )
!
  ZDIR_SW_GD(JJ)= (  ZDIR_SW(JJ) - T%XURBTREE(JJ)*ZNTR_DIR_SW_HV(JJ) )  &
                *( 2. * ZTHETA0_R(JJ) / XPI                            &
                -2. * ZTANZEN(JJ) / XPI                              &
                * T%XCAN_HW_RATIO(JJ) * (1.-COS(ZTHETA0_R(JJ)))  )  
!
!
  IF (.NOT. GALB_ONLY) THEN
    IF (PZENITH(JJ).LT.0.5*XPI) THEN
        DMT%XROAD_SHADE(JJ) = 1.0 - ( 2. * ZTHETA0_R(JJ) / XPI -2. * ZTANZEN(JJ) / XPI  &
                      * T%XCAN_HW_RATIO(JJ) * (1.-COS(ZTHETA0_R(JJ)))  )
        IF (DMT%XROAD_SHADE(JJ).LT.-XSURF_EPSILON) DMT%XROAD_SHADE(JJ) = 1.0
    ELSE
        DMT%XROAD_SHADE(JJ) = 1.0
    ENDIF
  ENDIF
!
!* b.    Case with prescribed street orientations
!        ----------------------------------------
ELSE
!
!     * b.1.  Case with explicit high vegetation
!             ----------------------------------
  IF (T%XURBTREE(JJ).GT.0.) THEN
!
!       The reference height for tree canopy is the top of trees
    ZDIR_SW_HV (JJ) = ZDIR_SW(JJ)* MAX(0.,                                   &
                   1.-(T%XCAN_HW_RATIO(JJ)*((T%XBLD_HEIGHT(JJ)-GDP%XH_TREE(JJ))/T%XBLD_HEIGHT(JJ)))   &
                   *ZTANZEN(JJ)*ABS(SIN(PAZIM(JJ)-ZRD_DIR(JJ))) ) 
!
!     * b.2.  Case without explicit high vegetation
!             -------------------------------------
  ELSE                               
    ZDIR_SW_HV (JJ) = 0.
  ENDIF
!
! Flux of direct solar radiation reflected or absorbed by high vegetation (or no transmitted)
! 
  ZNTR_DIR_SW_HV(JJ)   = ZDIR_SW_HV(JJ)*(1.-PTRANS_HVCR(JJ))
!
! Flux received by roads and gardens (at ground level)
  ZDIR_SW_RD (JJ)      = (ZDIR_SW(JJ) - T%XURBTREE(JJ)*ZNTR_DIR_SW_HV(JJ) ) * &
                MAX(0.,1.-T%XCAN_HW_RATIO(JJ)*ZTANZEN(JJ)           * &
                ABS(SIN(PAZIM(JJ)-ZRD_DIR(JJ))))
  ZDIR_SW_GD(JJ)   = ZDIR_SW_RD(JJ)
!
  IF (.NOT. GALB_ONLY) THEN
    IF (PZENITH(JJ).LT.0.5*XPI) THEN
      DMT%XROAD_SHADE(JJ) = 1.0 - MAX(0.,1.-T%XCAN_HW_RATIO(JJ)*ZTANZEN(JJ) * &
                      ABS(SIN(PAZIM(JJ)-ZRD_DIR(JJ))))
      IF (DMT%XROAD_SHADE(JJ).LT.-XSURF_EPSILON) DMT%XROAD_SHADE(JJ) = 1.0
    ELSE
      DMT%XROAD_SHADE(JJ) = 1.0 
    ENDIF
  ENDIF
!
END IF
!
!*      2.4    direct solar radiation received by walls
!              ----------------------------------------
!
  ZDIR_SW_WL(JJ)     = (ZDIR_SW(JJ) - (ZDIR_SW_RD(JJ)*ZRD(JJ)+            &
                       ZDIR_SW_GD(JJ)*ZGD(JJ)+        &
                       ZNTR_DIR_SW_HV(JJ)*T%XURBTREE(JJ)) ) &
                      * 0.5 / T%XCAN_HW_RATIO(JJ)  
!
! Case without buildings
! ----------------------
  ELSE
!
    IF (T%XURBTREE(JJ).GT.0.) THEN
       ZDIR_SW_HV(JJ)     = ZDIR_SW(JJ)
    ELSE
       ZDIR_SW_HV(JJ)     = 0.
    ENDIF
!
!   Flux of direct solar radiation reflected or absorbed by high vegetation (or no transmitted)
    ZNTR_DIR_SW_HV(JJ)   = ZDIR_SW_HV(JJ)*(1.-PTRANS_HVCR(JJ))
!
    ZDIR_SW_WL (JJ)  = 0.
    ZDIR_SW_RD (JJ)  = ZDIR_SW(JJ) - T%XURBTREE(JJ)*ZNTR_DIR_SW_HV(JJ)   
    ZDIR_SW_GD  (JJ) = ZDIR_SW(JJ) - T%XURBTREE(JJ)*ZNTR_DIR_SW_HV(JJ)
!
  ENDIF
!
IF (TOP%CROAD_DIR=='UNIF' .OR. TOP%CWALL_OPT=='UNIF') THEN
!* if walls are averaged, then
  ZDIR_SW_WL_A(JJ) = ZDIR_SW_WL(JJ)
  ZDIR_SW_WL_B(JJ) = ZDIR_SW_WL(JJ)
ELSE
!* if walls are separated, then radiation reaches the wall facing sun
! Note that wall A is the one facing mostly to the South (depending to
! road orientation), and wall B in the one facing mostly to the North
!
! In case of N-S road, wall A is the West  wall (= East-facing  wall), 
!                  and wall B is the East  wall (= West-facing  wall)
! In case of E-W road, wall A is the North wall (= South-facing wall), 
!                  and wall B is the South wall (= North-facing wall)
  IF (SIN(PAZIM(JJ)-ZRD_DIR(JJ))>0.) THEN
    ZDIR_SW_WL_A(JJ) = 2.* ZDIR_SW_WL(JJ)
    ZDIR_SW_WL_B(JJ) = 0.
  ELSE
    ZDIR_SW_WL_A(JJ) = 0.
    ZDIR_SW_WL_B(JJ) = 2.* ZDIR_SW_WL(JJ)
  END IF
END IF
!
! The direct sw radiation received by the high vegetation is corrected by transmitted part =
! the intercepted direct sw radiation (absorbed + reflected terms), before reflections calculations
!
  ZDIR_SW_HV(JJ) = ZDIR_SW_HV(JJ)*(1.-PTRANS_HVCR(JJ))
!
!*      2.5    diffuse solar radiation received by roads and garden areas
!              ---------------------------------------------------------
!
!
  ZSCA_SW_RD (JJ) = ZSCA_SW(JJ) * T%XSVF_RS(JJ) * T%XTAU_SR(JJ)
  ZSCA_SW_GD (JJ) = ZSCA_SW(JJ) * T%XSVF_RS(JJ) * T%XTAU_SR(JJ)
!
!*      2.6    diffuse solar radiation received by walls
!              -----------------------------------------
!
  ZSCA_SW_WL (JJ) = ZSCA_SW(JJ) *T%XSVF_WS(JJ) * T%XTAU_SW(JJ)
!
!*      2.7    diffuse solar radiation received by high veg
!              -----------------------------------------
!
! The diffuse sw radiation received by the high vegetation is corrected by transmitted part =
! the intercepted diffuse sw radiation (absorbed + reflected terms) 
!
  IF (T%XURBTREE(JJ).GT.0.) THEN
     ZSCA_SW_HV   (JJ) = (ZSCA_SW(JJ)                                          &
             - (ZRD(JJ)                   * ZSCA_SW_RD(JJ)   + &
             ZGD(JJ)                   * ZSCA_SW_GD(JJ) + &
             T%XWALL_O_HOR(JJ)/(1.-T%XBLD(JJ)) * ZSCA_SW_WL(JJ))   )&
             / T%XURBTREE(JJ)
  ELSE
     ZSCA_SW_HV   (JJ) = 0.
  ENDIF

!
!*      2.9    averaged albedos when snow is present
!              -------------------------------------
!
  ZAALB_RD   (JJ) =  PDF_RD (JJ) * T%XALB_ROAD   (JJ) + PDN_RD (JJ) * T%TSNOW_ROAD%ALB (JJ)  
!
!
ENDDO
!
!*      2.9b    averaged facade albedo
!              -------------------------------------
!
IF (TOP%CBEM=='BEM') THEN
    !
    ZEFF_SHAD(:,:) = -9999.0
    DMT%XDIAGSHAD(:,:) = 0.0
    !
    DO JJ=1,SIZE(B%XSHADEARCHI,1)
       DO JCOMP=1,SIZE(B%XFRACOMP,2) 
          !
          IF (B%XSHADEARCHI(JJ).LT.0.5) THEN
            !
            ! If shading elements are not present, shading is not possible
            !
            ZEFF_SHAD(JJ,JCOMP) = 0.0
            DMT%XDIAGSHAD(JJ,JCOMP) = 0.0
            !
          ELSE IF (B%XSHADEARCHI(JJ).GT.1.5) THEN
            !
            ! In this case shading elements are always present
            ! However, PDIAGSHAD=0 since this diagnostic
            ! represents only shading due to human behaviour.
            !
            ZEFF_SHAD(JJ,JCOMP) = 1.0
            DMT%XDIAGSHAD(JJ,JCOMP) = 0.0
            !
          ELSE
            !
            ! If shading elements are present, the shading behavioural
            ! indicators are used to determine the fraction of shades closed
            ! A logistic function is used to determine the fractions of shades actually closed.
            !
            ZFRAC_SHAD(JJ) = 1.0/(1.0+EXP(-0.05*(ZDIR_SW_WL(JJ)+ZSCA_SW_WL(JJ)-B%XWIN_SW_MAX(JJ))))
            !
            ZEFF_SHAD(JJ,JCOMP) = PSHAD_BEHAV_ANYWAY(JJ,JCOMP) + PSHAD_BEHAV_ADAPTI(JJ,JCOMP)*ZFRAC_SHAD(JJ)
            !
            DMT%XDIAGSHAD(JJ,JCOMP) = ZEFF_SHAD(JJ,JCOMP)          
            !
          ENDIF
       ENDDO
    ENDDO
    !
    IF ((MINVAL(DMT%XDIAGSHAD).LT.-XSURF_EPSILON).OR. &
        (MAXVAL(DMT%XDIAGSHAD).GT.(1.0+XSURF_EPSILON))) CALL ABOR1_SFX("Wrong shading fraction")
    !
    DO JCOMP=1,SIZE(B%XFRACOMP,2)
       ZTRAN_WIN (:,JCOMP) = B%XTRAN_WIN(:)
    ENDDO
    !
    CALL WINDOW_SHADING(B%XSHGC_SH, ZEFF_SHAD, T%XALB_WALL, &
          B%XABS_WIN, ZABS_WIN, ZALB_WIN, ZTRAN_WIN      )
    !
    ! The values of the window albedo and window absorbance are aggregated
    ! over the bem compartments
    !
    ZAGG_ABS_WIN(:)=0.0
    B%XALB_WIN(:)=0.0
    DO JCOMP=1,SIZE(B%XFRACOMP,2)
       B%XALB_WIN(:)   = B%XALB_WIN(:)   + B%XFRACOMP(:,JCOMP) * ZALB_WIN(:,JCOMP)
       ZAGG_ABS_WIN(:) = ZAGG_ABS_WIN(:) + B%XFRACOMP(:,JCOMP) * ZABS_WIN(:,JCOMP)
    ENDDO
    !
ELSE
  !
  ZAGG_ABS_WIN(:) = 0.0
  ZABS_WIN (:,:)  = 0.0
  B%XALB_WIN (:)  = 0.0
  ZALB_WIN (:,:)  = 0.0
  ZTRAN_WIN(:,:)  = 0.0
  !
ENDIF
!
ZAALB_WL(:) =  B%XGR(:) * B%XALB_WIN(:) + (1.-B%XGR(:)) * T%XALB_WALL(:)
!
!*      2.9    absorption of direct incoming solar radiation
!              ---------------------------------------------
!
!
 CALL SOLAR_REFLECTIONS( ZDIR_SW, ZDIR_SW_RD,ZDIR_SW_WL, ZDIR_SW_GD, ZDIR_SW_HV, &
                        ZREF_DIR_SW_RD, ZREF_DIR_SW_WL,                     &
                        ZREF_DIR_SW_GD, ZREF_DIR_SW_HV,                     &
                        ZABS_DIR_SW_SKY,                                    &
                        ZABS_DIR_SW_RD, ZABS_DIR_SW_SN_RD,                  &
                        ZABS_DIR_SW_WL, ZABS_DIR_SW_GD, ZABS_DIR_SW_WIN,    &
                        ZABS_DIR_SW_HV                                      )
!
IF (TOP%CROAD_DIR=='UNIF' .OR. TOP%CWALL_OPT=='UNIF') THEN
!* if walls are averaged, then
  ZABS_DIR_SW_WL_A = ZABS_DIR_SW_WL
  ZABS_DIR_SW_WL_B = ZABS_DIR_SW_WL
ELSE
!* if walls are separated, then radiation reaches the wall facing sun
! Note that wall A is the one facing mostly to the North (depending to
! road orientation), and wall B in the one facing mostly to the South.
  ZDW = (1.-T%XALB_WALL(:)) * ZAALB_WL(:) * (1.-2.*T%XSVF_WS(:)) &
       / (1.+ZAALB_WL(:)*(1.-2.*T%XSVF_WS(:)))                 &
       * 0.5 * (ZDIR_SW_WL_A(:)-ZDIR_SW_WL_B(:))            &
       + 0.5 * (1.-T%XALB_WALL(:)) * (ZDIR_SW_WL_A-ZDIR_SW_WL_B)
  DO JJ=1,SIZE(T%XROAD)
     IF (ZDW(JJ) .GE. 0.) THEN
        ZDW(JJ) = MIN( ZDW(JJ) , ZABS_DIR_SW_WL(JJ) )
     ELSE
        ZDW(JJ) = - MIN( ABS(ZDW(JJ)) , ZABS_DIR_SW_WL(JJ) )
     ENDIF
  ENDDO
  ZABS_DIR_SW_WL_A = ZABS_DIR_SW_WL + ZDW
  ZABS_DIR_SW_WL_B = ZABS_DIR_SW_WL - ZDW
END IF
!
!*      2.10    absorption of diffuse incoming solar radiation
!              ----------------------------------------------
!
 CALL SOLAR_REFLECTIONS( ZSCA_SW,                         &
                         ZSCA_SW_RD,ZSCA_SW_WL,                              &
                         ZSCA_SW_GD, ZSCA_SW_HV,                             &
                         ZREF_DIR_SW_RD, ZREF_DIR_SW_WL,                     &
                         ZREF_DIR_SW_GD, ZREF_DIR_SW_HV,                     &
                         ZABS_SCA_SW_SKY,                                    &
                         ZABS_SCA_SW_RD, ZABS_SCA_SW_SN_RD,                  &
                         ZABS_SCA_SW_WL, ZABS_SCA_SW_GD, ZABS_SCA_SW_WIN,    &
                         ZABS_SCA_SW_HV                                      )
!
DO JJ=1,SIZE(T%XROAD)
  !
  ! solar flux reflected for façade (wall + window) and soil (road + garden) 
  !
  PREF_SW_GRND(JJ) = ZRD(JJ)   * T%XALB_ROAD(JJ)   / MAX( XSURF_EPSILON,(1. - T%XALB_ROAD(JJ)) ) * &
          (ZABS_DIR_SW_RD(JJ) + ZABS_SCA_SW_RD(JJ))      &
          + ZGD(JJ) * PALB_GD(JJ) / MAX( XSURF_EPSILON,(1. - PALB_GD(JJ))) * &
          (ZABS_DIR_SW_GD(JJ) + ZABS_SCA_SW_GD(JJ))
  !
  PREF_SW_FAC(JJ) = (1 - B%XGR(JJ)) * T%XALB_WALL(JJ)  / MAX( XSURF_EPSILON,(1. - T%XALB_WALL(JJ)) ) * &
          (ZABS_DIR_SW_WL(JJ) + ZABS_SCA_SW_WL(JJ))     &
          +      B%XGR(JJ)  * B%XALB_WIN(JJ)   / MAX( XSURF_EPSILON,(1 - B%XALB_WIN(JJ))  )     * &
          (ZABS_DIR_SW_WIN(JJ) + ZABS_SCA_SW_WIN(JJ))
  !
  ! solar flux reflected for high vegetation
  !
  !! Calcul1   
  !! -------
  !
  IF (T%XURBTREE(JJ).GT.0.) THEN                     
      ! Calcul2 
      ! -------
      PREF_SW_HV(JJ) =   ZRD(JJ)                          * (ZDIR_SW_RD(JJ)         + ZSCA_SW_RD(JJ))         + &
             ZGD(JJ)                          * (ZDIR_SW_GD(JJ)         + ZSCA_SW_GD(JJ))         + &
             T%XWALL_O_HOR(JJ)/(1.-T%XBLD(JJ))* (ZDIR_SW_WL(JJ)         + ZSCA_SW_WL(JJ))         + &
             T%XURBTREE(JJ)                   * (ZDIR_SW_HV(JJ)         + ZSCA_SW_HV(JJ)  )       - &
             ZRD(JJ)                          * (ZABS_DIR_SW_RD(JJ)     + ZABS_SCA_SW_RD(JJ))     - &
             ZGD(JJ)                          * (ZABS_DIR_SW_GD(JJ)     + ZABS_SCA_SW_GD(JJ))     - &
             T%XWALL_O_HOR(JJ)/(1.-T%XBLD(JJ))* (ZABS_DIR_SW_WL(JJ)     + ZABS_SCA_SW_WL(JJ)  )   - &
             T%XURBTREE(JJ)                   * (ZABS_DIR_SW_HV(JJ)     + ZABS_SCA_SW_HV(JJ)  )   - &
             PREF_SW_GRND(JJ)                                                                     - &
             T%XWALL_O_HOR(JJ)/(1.-T%XBLD(JJ))* PREF_SW_FAC(JJ)
  ELSE
      PREF_SW_HV(JJ) = 0.
  ENDIF
  !
ENDDO
!----------------------------------------------------------------------------------------------------------
!
!*      3.     Town albedo
!              -----------
!
!*      3.1    direct albedo
!              -------------
!
 CALL TOWN_ALBEDO(ZDIR_SW,ZABS_DIR_SW_RF,ZABS_DIR_SW_SN_RF,           &
                   ZABS_DIR_SW_RD, ZABS_DIR_SW_SN_RD,ZABS_DIR_SW_WL,  &
                   ZABS_DIR_SW_GD, ZABS_DIR_SW_GR, ZABS_DIR_SW_WIN,&
                   ZABS_DIR_SW_HV, ZABS_DIR_SW_PANEL,                       & 
                   PDIR_ALB_TWN,ZDIR_SW_UP                                   )  
!
!*      3.2    diffuse albedo
!              -------------
!
 CALL TOWN_ALBEDO(ZSCA_SW,ZABS_SCA_SW_RF,ZABS_SCA_SW_SN_RF,           &
                   ZABS_SCA_SW_RD, ZABS_SCA_SW_SN_RD,ZABS_SCA_SW_WL,  &
                   ZABS_SCA_SW_GD, ZABS_SCA_SW_GR, ZABS_SCA_SW_WIN,&
                   ZABS_SCA_SW_HV, ZABS_SCA_SW_PANEL,                     &
                   PSCA_ALB_TWN,ZSCA_SW_UP                                   )  
!
IF (GALB_ONLY) THEN
   IF (LHOOK) CALL DR_HOOK('URBAN_SOLAR_ABS',1,ZHOOK_HANDLE)
   RETURN
ENDIF
!-------------------------------------------------------------------------------
!
!*      4.     Trivial cases
!              -------------
!
WHERE(PDIR_SW(:)==0.)
  !
  ZABS_DIR_SW_RF   (:) = 0.
  ZABS_DIR_SW_RD   (:) = 0.
  ZABS_DIR_SW_WL_A (:) = 0.
  ZABS_DIR_SW_WL_B (:) = 0.
  ZABS_DIR_SW_GD   (:) = 0.
  ZABS_DIR_SW_HV   (:) = 0.
  ZABS_DIR_SW_GR   (:) = 0.
  ZABS_DIR_SW_PANEL(:) = 0.
  ZABS_DIR_SW_WIN  (:) = 0.
  ZABS_DIR_SW_SN_RF(:) = 0.
  ZABS_DIR_SW_SN_RD(:) = 0.
  !
  ZREF_DIR_SW_RF (:) = 0.
  ZREF_DIR_SW_RD (:) = 0.
  ZREF_DIR_SW_WL (:) = 0.
  ZREF_DIR_SW_GD (:) = 0.
  ZREF_DIR_SW_HV (:) = 0.
  !
  ZREC_DIR_SW_WIN (:) = 0.
  !
  ZDIR_SW_RD (:) = 0.
  ZDIR_SW_WL_A (:) = 0.
  ZDIR_SW_WL_B (:) = 0.
  ZDIR_SW_GD (:) = 0.
  ZDIR_SW_HV (:) = 0.
  ZDIR_SW_WL (:) = 0.
  !
END WHERE
!
WHERE(PSCA_SW(:)==0.)
  ZABS_SCA_SW_RF   (:) = 0.
  ZABS_SCA_SW_RD   (:) = 0.
  ZABS_SCA_SW_WL   (:) = 0.
  ZABS_SCA_SW_GD   (:) = 0.
  ZABS_SCA_SW_HV   (:) = 0.
  ZABS_SCA_SW_GR   (:) = 0.
  ZABS_SCA_SW_PANEL(:) = 0.
  ZABS_SCA_SW_WIN  (:) = 0.
  ZABS_SCA_SW_SN_RF(:) = 0.
  ZABS_SCA_SW_SN_RD(:) = 0.
  !
  ZREF_SCA_SW_RF (:) = 0.
  ZREF_SCA_SW_RD (:) = 0.
  ZREF_SCA_SW_WL (:) = 0.
  ZREF_SCA_SW_GD (:) = 0.
  ZREF_SCA_SW_HV (:) = 0.
  !
  ZREC_SCA_SW_WIN (:) = 0.
  !
  ZSCA_SW_RD (:) = 0.
  ZSCA_SW_WL (:) = 0.
  ZSCA_SW_GD (:) = 0. 
  ZSCA_SW_HV (:) = 0. 
  !
END WHERE
!
WHERE((PDIR_SW(:)==0.).AND.(PSCA_SW(:)==0.))
  PREF_SW_HV   (:) = 0.0
  PREF_SW_FAC  (:) = 0.0
  PREF_SW_GRND (:) = 0.0
END WHERE
!
WHERE (PDIR_ALB_TWN==XUNDEF) PDIR_ALB_TWN = PSCA_ALB_TWN
!
!-------------------------------------------------------------------------------
!
DO JJ=1,SIZE(T%XROAD)
!
!*      5.     Total solar radiation absorbed by each surface
!              ----------------------------------------------
!
! solar radiation absorbed by sky
!
  DMT%XABS_SW_SKY     (JJ)  = ZABS_DIR_SW_SKY     (JJ) + ZABS_SCA_SW_SKY     (JJ)
!
! solar radiation absorbed by roofs
!
  DMT%XABS_SW_ROOF     (JJ) = ZABS_DIR_SW_RF       (JJ) + ZABS_SCA_SW_RF       (JJ)
!
! solar radiation absorbed by roads
!
  DMT%XABS_SW_ROAD     (JJ) = ZABS_DIR_SW_RD       (JJ) + ZABS_SCA_SW_RD       (JJ)
!
! solar radiation absorbed by garden areas
!
  DMT%XABS_SW_GARDEN   (JJ) = ZABS_DIR_SW_GD       (JJ) + ZABS_SCA_SW_GD       (JJ)
!
! solar radiation absorbed by high vegetation
!
  DMT%XABS_SW_HVEG     (JJ) = ZABS_DIR_SW_HV       (JJ) + ZABS_SCA_SW_HV       (JJ)
!
! solar radiation absorbed by greenroofs
!
  DMT%XABS_SW_GREENROOF(JJ) = ZABS_DIR_SW_GR       (JJ) + ZABS_SCA_SW_GR       (JJ)
!
! solar radiation absorbed by solar panels
!
  DMT%XABS_SW_PANEL(JJ)= ZABS_DIR_SW_PANEL   (JJ) + ZABS_SCA_SW_PANEL    (JJ)
!
! solar radiation absorbed by walls
!
  DMT%XABS_SW_WALL_A   (JJ) = ZABS_DIR_SW_WL_A     (JJ) + ZABS_SCA_SW_WL       (JJ)
  DMT%XABS_SW_WALL_B   (JJ) = ZABS_DIR_SW_WL_B     (JJ) + ZABS_SCA_SW_WL       (JJ)
!
! solar radiation absorbed (but not transmitted) by windows
!
  ZREC_DIR_SW_WIN(JJ) = ZABS_DIR_SW_WIN(JJ) / MAX( XSURF_EPSILON,(1.-B%XALB_WIN(JJ)) )
  ZREC_SCA_SW_WIN(JJ) = ZABS_SCA_SW_WIN(JJ) / MAX( XSURF_EPSILON,(1.-B%XALB_WIN(JJ)) )
!
  DMT%XABS_SW_WIN      (JJ) = (ZREC_DIR_SW_WIN  (JJ) + ZREC_SCA_SW_WIN   (JJ)) * ZAGG_ABS_WIN(JJ)
!
!
! solar radiation absorbed by snow on roofs
!
  IF (PDN_RF(JJ).GT.0.0) THEN
     DMT%XABS_SW_SNOW_ROOF (JJ) = ZABS_DIR_SW_SN_RF (JJ) + ZABS_SCA_SW_SN_RF (JJ)
  ELSE
     DMT%XABS_SW_SNOW_ROOF (JJ) = XUNDEF
  ENDIF
!
! solar radiation absorbed by snow on roads
!
  IF (PDN_RD(JJ).GT.0.0) THEN  
     DMT%XABS_SW_SNOW_ROAD (JJ) = ZABS_DIR_SW_SN_RD (JJ) + ZABS_SCA_SW_SN_RD (JJ)
  ELSE
     DMT%XABS_SW_SNOW_ROAD (JJ) = XUNDEF
  ENDIF
!
!-------------------------------------------------------------------------------
!
!*      6.     total solar radiation received by each surfaces
!              -------------------------------------------------------
!
  PREC_SW_WIN     (JJ) = ZREC_DIR_SW_WIN(JJ) + ZREC_SCA_SW_WIN(JJ)
!
  PREC_SW_GD      (JJ) = DMT%XABS_SW_GARDEN    (JJ)/MAX( XSURF_EPSILON,(1.-PALB_GD (JJ)))
!
!*      6.2    total solar radiation received by roof surfaces below solar panels
!
  PREC_SW_RF      (JJ) = (PDIR_SW(JJ) + PSCA_SW(JJ)) * (1.-PFRAC_PANEL(JJ))
!
!-------------------------------------------------------------------------------
!
!*      6.2    solar radiation received by high veg
!
  IF (T%XURBTREE(JJ).GT.0. .AND. PTRANS_HVCR(JJ)<1.) THEN
    DMT%XREC_SW_HVEG    (JJ)  = DMT%XABS_SW_HVEG      (JJ)                     &
             / ( MAX( XSURF_EPSILON,(1.-PALB_HVEG (JJ))) * (1.-PTRANS_HVCR(JJ)) )
  ELSE
    DMT%XREC_SW_HVEG    (JJ)  = 0.  
  ENDIF
!

!
!*      8.     total solar radiation reflected by roof
!              --------------------------------------------------
!
!
  DMT%XSW_UP_ROOF(JJ) = (PDIR_SW(JJ) + PSCA_SW(JJ))                               &
          - ( (1.-T%XGREENROOF(JJ)) * PDF_RF(JJ) * DMT%XABS_SW_ROOF(JJ)       &
          +(1.-T%XGREENROOF(JJ)) * PDN_RF(JJ) * DMT%XABS_SW_SNOW_ROOF(JJ)  &
          +    T%XGREENROOF(JJ)               * DMT%XABS_SW_GREENROOF(JJ)  &
          +    PFRAC_PANEL(JJ)                * DMT%XABS_SW_PANEL(JJ) )
!-------------------------------------------------------------------------------
!
!*      9.     total solar radiation reflected by the canyon
!              --------------------------------------------------
!
  DMT%XSW_UP_CAN(JJ) = (PDIR_SW(JJ) + PSCA_SW(JJ))                                   &
          - ( ZRD(JJ)                    *PDF_RD(JJ) *DMT%XABS_SW_ROAD (JJ)    &
          +ZRD(JJ)                    *PDN_RD(JJ) *DMT%XABS_SW_SNOW_ROAD(JJ)&
          +ZGD(JJ)                                *DMT%XABS_SW_GARDEN(JJ)   &
          +T%XURBTREE(JJ)                         *DMT%XABS_SW_HVEG(JJ)     &
          +T%XCAN_HW_RATIO(JJ)        *(1.-B%XGR(JJ)) *DMT%XABS_SW_WALL_A(JJ)   &
          +T%XCAN_HW_RATIO(JJ)        *(1.-B%XGR(JJ)) *DMT%XABS_SW_WALL_B(JJ)   &
          +T%XCAN_HW_RATIO(JJ) * 2.   *    B%XGR(JJ)  *DMT%XABS_SW_WIN (JJ)     )
!
!-------------------------------------------------------------------------------
ENDDO
!-------------------------------------------------------------------------------
!
!*      7.     total solar radiation transmitted inside building
!*             and energy not ref., nor absorbed, nor transmitted
!              --------------------------------------------------
!
IF (TOP%CBEM=='BEM') THEN
  PE_SHADING(:) = 0.0
  DO JCOMP=1,SIZE(B%XFRACOMP,2)
     DMT%XTR_SW_WIN(:,JCOMP) = PREC_SW_WIN(:) * ZTRAN_WIN(:,JCOMP)
     ZE_SHADING(:,JCOMP) = PREC_SW_WIN(:) * &
          (1.-ZALB_WIN(:,JCOMP)-ZABS_WIN(:,JCOMP)-ZTRAN_WIN(:,JCOMP))
     PE_SHADING(:) = PE_SHADING(:) + B%XFRACOMP(:,JCOMP) * ZE_SHADING(:,JCOMP)
  ENDDO
END IF
!
IF (TOP%LSOLAR_PANEL) THEN
   !
   DO JJ=1,SIZE(T%XROAD)
      !
      ! solar radiation absorbed by solar panels
      !
      DMT%XABS_SW_PANEL(JJ) = ZABS_DIR_SW_PANEL    (JJ) + ZABS_SCA_SW_PANEL    (JJ)
      !
   ENDDO
   !
ENDIF
!-------------------------------------------------------------------------------
!
! E.R. Diagnostics for urban trees
DMT%XDIR_SW_ROAD(:)     = ZDIR_SW_RD(:)
DMT%XDIR_SW_WALL(:)     = ZDIR_SW_WL(:)
DMT%XDIR_SW_WALL_A(:)   = ZDIR_SW_WL_A(:)
DMT%XDIR_SW_WALL_B(:)   = ZDIR_SW_WL_B(:)
DMT%XDIR_SW_GARDEN(:)   = ZDIR_SW_GD(:)
DMT%XDIR_SW_HVEG(:)     = ZDIR_SW_HV(:)
DMT%XNTR_DIR_SW_HVEG(:) = ZNTR_DIR_SW_HV(:)
DMT%XSCA_SW_ROAD(:)     = ZSCA_SW_RD(:)
DMT%XSCA_SW_WALL(:)     = ZSCA_SW_WL(:)
DMT%XSCA_SW_GARDEN(:)   = ZSCA_SW_GD(:)
DMT%XSCA_SW_HVEG(:)     = ZSCA_SW_HV(:)
!
!
IF (LHOOK) CALL DR_HOOK('URBAN_SOLAR_ABS',1,ZHOOK_HANDLE)
CONTAINS
!
!-------------------------------------------------------------------------------
SUBROUTINE SOLAR_REFLECTIONS(  ZSW, ZSW_RD,ZSW_WL,ZSW_GD, ZSW_HV, &
                             ZSREF_SW_RD,ZSREF_SW_WL, ZSREF_SW_GD,ZSREF_SW_HV, &
                             ZABS_SW_SKY, ZABS_SW_RD,ZABS_SW_SN_RD,            &
                             ZABS_SW_WL, ZABS_SW_GD, ZABS_SW_WIN, ZABS_SW_HV   )
!
REAL, DIMENSION(:), INTENT(IN) :: ZSW             ! incoming solar radiation
REAL, DIMENSION(:), INTENT(IN) :: ZSW_RD          ! solar radiation received by 
REAL, DIMENSION(:), INTENT(IN) :: ZSW_WL          ! road, wall, GARDEN areas
REAL, DIMENSION(:), INTENT(IN) :: ZSW_GD          ! & urban trees
REAL, DIMENSION(:), INTENT(IN) :: ZSW_HV          ! before reflection
REAL, DIMENSION(:), INTENT(OUT):: ZABS_SW_SKY     ! solar radiation absorbed by sky
REAL, DIMENSION(:), INTENT(OUT):: ZABS_SW_RD      ! solar radiation absorbed by
REAL, DIMENSION(:), INTENT(OUT):: ZABS_SW_SN_RD   ! solar radiation absorbed by
REAL, DIMENSION(:), INTENT(OUT):: ZABS_SW_WL      ! road, snow over road, and wall 
REAL, DIMENSION(:), INTENT(OUT):: ZABS_SW_GD      ! solar radiation absorbed by garden
REAL, DIMENSION(:), INTENT(OUT):: ZABS_SW_HV      ! solar radiation absorbed by high vegetation
REAL, DIMENSION(:), INTENT(OUT):: ZABS_SW_WIN     ! solar radiation absorbed by window
REAL, DIMENSION(:), INTENT(OUT) :: ZSREF_SW_RD    ! sum of all reflections
REAL, DIMENSION(:), INTENT(OUT) :: ZSREF_SW_WL    ! against road, wall,
REAL, DIMENSION(:), INTENT(OUT) :: ZSREF_SW_GD    ! and GARDEN areas
REAL, DIMENSION(:), INTENT(OUT) :: ZSREF_SW_HV    ! and high vegetation areas
!
REAL, DIMENSION(SIZE(ZSW)) :: ZREF0_SW_RD    ! first solar reflection
REAL, DIMENSION(SIZE(ZSW)) :: ZREF0_SW_WL    ! against road, wall,
REAL, DIMENSION(SIZE(ZSW)) :: ZREF0_SW_GD    ! GARDEN areas,
REAL, DIMENSION(SIZE(ZSW)) :: ZREF0_SW_HV    ! High vegetation,
!
REAL, DIMENSION(SIZE(ZSW)) :: ZDENOM
REAL, DIMENSION(SIZE(ZSW)) :: ZGEOM_SR, ZGEOM_SG, ZGEOM_SW, ZGEOM_ST ! geometric variables 
REAL, DIMENSION(SIZE(ZSW)) :: ZGEOM_RW, ZGEOM_RT                     ! W: wall, R: road, G: garden, T: tree 
REAL, DIMENSION(SIZE(ZSW)) :: ZGEOM_GW, ZGEOM_GT
REAL, DIMENSION(SIZE(ZSW)) :: ZGEOM_WR, ZGEOM_WG, ZGEOM_WW, ZGEOM_WT
REAL, DIMENSION(SIZE(ZSW)) :: ZGEOM_TR, ZGEOM_TG, ZGEOM_TW                     
INTEGER :: JJ
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('SOLAR_REFLECTIONS',0,ZHOOK_HANDLE)
!
DO JJ=1,SIZE(ZSW)
!*      A.     first solar radiation reflection
!              --------------------------------
!
  ZREF0_SW_RD(JJ) = ZAALB_RD(JJ) * ZSW_RD(JJ) 
!
  ZREF0_SW_GD(JJ) = PALB_GD(JJ)  * ZSW_GD(JJ)
!
  ZREF0_SW_WL(JJ) = ZAALB_WL(JJ) * ZSW_WL(JJ)
!
  IF (T%XURBTREE(JJ).GT.0.) THEN
     ZREF0_SW_HV(JJ)   = PALB_HVEG(JJ)   * ZSW_HV(JJ) 
  ELSE
     ZREF0_SW_HV(JJ)   = 0.
  ENDIF
!
!*      B.     sum of solar radiation reflected
!              --------------------------------
!
!* 1. Case with explicit high vegetation
!     ----------------------------------
!
  IF (T%XURBTREE(JJ).GT.0.) THEN
!
!       B1.    calculation of intermediate variables for radiative calculations
!
! fluxes defined on one pi 
    ZCOEFF_RT(JJ)  =    0. ! no reflection HV to R (upwards only)
    ZCOEFF_WT(JJ)  =    (1. - T%XSVF_ST(JJ)) / (2. - T%XSVF_ST(JJ) - T%XSVF_RT(JJ)) ! no reflection HV to W (upwards only)
    ZGEOM_SR(JJ)  =    T%XSVF_RS(JJ) * T%XTAU_SR(JJ) * ZRD(JJ)
    ZGEOM_SG(JJ)  =    T%XSVF_RS(JJ) * T%XTAU_SR(JJ) * ZGD(JJ)
    ZGEOM_SW(JJ)  =    T%XSVF_SW(JJ) * T%XTAU_SW(JJ)
    ZGEOM_ST(JJ)  =    T%XSVF_ST(JJ) * T%XURBTREE(JJ)
    ZGEOM_RW(JJ)  =    T%XSVF_RW(JJ) * T%XTAU_WR(JJ)
    ZGEOM_RT(JJ)  =    T%XSVF_RT(JJ) * T%XURBTREE(JJ) * ZCOEFF_RT(JJ) 
    ZGEOM_GW(JJ)  =    T%XSVF_RW(JJ) * T%XTAU_WR(JJ)
    ZGEOM_GT(JJ)  =    T%XSVF_RT(JJ) * T%XURBTREE(JJ) * ZCOEFF_RT(JJ) 
    ZGEOM_WR(JJ)  =    T%XSVF_WR(JJ) * T%XTAU_WR(JJ) * ZRD(JJ)
    ZGEOM_WG(JJ)  =    T%XSVF_WR(JJ) * T%XTAU_WR(JJ) * ZGD(JJ)
    ZGEOM_WW(JJ)  =    T%XSVF_WW(JJ) * T%XTAU_WW(JJ) 
    ZGEOM_WT(JJ)  =    T%XSVF_WT(JJ) * T%XURBTREE(JJ) * ZCOEFF_WT(JJ) 
    ZGEOM_TW(JJ)  =  ( T%XSVF_SW(JJ) *(1.-T%XTAU_SW(JJ) )   &
               +T%XSVF_RW(JJ) *(1.-T%XTAU_WR(JJ) )   &
               +T%XSVF_WW(JJ) *(1.-T%XTAU_WW(JJ) ) ) &
               * (          1. / T%XURBTREE(JJ) ) 
    ZGEOM_TR(JJ)  =  ( T%XSVF_RS(JJ) *(1.-T%XTAU_SR(JJ) )   &
               +T%XSVF_WR(JJ) *(1.-T%XTAU_WR(JJ) ) ) & 
               * (  ZRD(JJ)/ T%XURBTREE(JJ) ) 
    ZGEOM_TG(JJ)  =  ( T%XSVF_RS(JJ) *(1.-T%XTAU_SR(JJ) )   &
               +T%XSVF_WR(JJ) *(1.-T%XTAU_WR(JJ) ) ) &
               * ( ZGD(JJ) / T%XURBTREE(JJ) )  
!
!       B2.    calculation of total reflections
!
    ZDENOM(JJ) =   (1.- ZGEOM_WR(JJ)*ZAALB_WL(JJ)*ZGEOM_RW(JJ)*ZAALB_RD(JJ)    &
               - ZGEOM_WG(JJ)*ZAALB_WL(JJ)*ZGEOM_GW(JJ)*PALB_GD(JJ)     &
               - ZGEOM_WW(JJ)*ZAALB_WL(JJ)                           )  &
               * (1.- ZGEOM_TR(JJ)*PALB_HVEG(JJ) *ZGEOM_RT(JJ)*ZAALB_RD(JJ)    &
               - ZGEOM_TG(JJ)*PALB_HVEG(JJ) *ZGEOM_GT(JJ)*PALB_GD(JJ))    &
               - (    ZGEOM_WR(JJ)*ZAALB_WL(JJ)*ZGEOM_RT(JJ)*ZAALB_RD(JJ)    &
               + ZGEOM_WG(JJ)*ZAALB_WL(JJ)*ZGEOM_GT(JJ)*PALB_GD(JJ)     &
               + ZGEOM_WT(JJ)*ZAALB_WL(JJ)                           )  &
               * (    ZGEOM_TR(JJ)*PALB_HVEG(JJ) *ZGEOM_RW(JJ)*ZAALB_RD(JJ)    &
               + ZGEOM_TG(JJ)*PALB_HVEG(JJ) *ZGEOM_GW(JJ)*PALB_GD(JJ)     &
               + ZGEOM_TW(JJ)*PALB_HVEG(JJ)                            )
!
    ZSREF_SW_HV(JJ) =    ( 1.-ZGEOM_WR(JJ)*ZAALB_WL(JJ)*ZGEOM_RW(JJ)*ZAALB_RD(JJ)                                    &
               -ZGEOM_WG(JJ)*ZAALB_WL(JJ)*ZGEOM_GW(JJ)*PALB_GD(JJ)                                      &
               -ZGEOM_WW(JJ)*ZAALB_WL(JJ)                                                                 ) &
               / ZDENOM(JJ) * ZREF0_SW_HV(JJ)                                                                &
               + ( ZGEOM_TR(JJ)*PALB_HVEG(JJ)                                                                    &
               *(1.-ZGEOM_WG(JJ)*ZAALB_WL(JJ)*ZGEOM_GW(JJ)*PALB_GD(JJ) - ZGEOM_WW(JJ)*ZAALB_WL(JJ))  &
               +ZGEOM_WR(JJ)*ZAALB_WL(JJ)                                                                   &
               *(ZGEOM_TG(JJ)*PALB_HVEG(JJ)*ZGEOM_GW(JJ)*PALB_GD(JJ)+ZGEOM_TW(JJ)*PALB_HVEG(JJ)     ) )  &
               / ZDENOM(JJ) * ZREF0_SW_RD(JJ)                                                                &
               + ( ZGEOM_TG(JJ)*PALB_HVEG(JJ)                                                                    &
               *(1.-ZGEOM_WR(JJ)*ZAALB_WL(JJ)*ZGEOM_RW(JJ)*ZAALB_RD(JJ)-ZGEOM_WW(JJ)*ZAALB_WL(JJ)   )  &
               +ZGEOM_WG(JJ)*ZAALB_WL(JJ)                                                                   &
               *(ZGEOM_TR(JJ)*PALB_HVEG(JJ)*ZGEOM_RW(JJ)*ZAALB_RD(JJ)+ZGEOM_TW(JJ)*PALB_HVEG(JJ)      ) )  &
               / ZDENOM(JJ) * ZREF0_SW_GD(JJ)                                                              &
               + ( ZGEOM_TR(JJ)*PALB_HVEG(JJ)*ZGEOM_RW(JJ)*ZAALB_RD(JJ)                                        &
               +ZGEOM_TG(JJ)*PALB_HVEG(JJ)*ZGEOM_GW(JJ)*PALB_GD(JJ)                                       &
               +ZGEOM_TW(JJ)*PALB_GD(JJ)                                                                ) & 
               / ZDENOM(JJ) * ZREF0_SW_WL(JJ)
!
    ZSREF_SW_WL(JJ) =    ( ZGEOM_WR(JJ)*ZAALB_WL(JJ)*ZGEOM_RT(JJ)*ZAALB_RD(JJ)                                       &
               +ZGEOM_WG(JJ)*ZAALB_WL(JJ)*ZGEOM_GT(JJ)*PALB_GD(JJ)                                      &
               +ZGEOM_WT(JJ)*ZAALB_WL(JJ)                                                                 ) &
               / ZDENOM(JJ) * ZREF0_SW_HV(JJ)                                                                &
               + ( ZGEOM_WR(JJ)*ZAALB_WL(JJ)                                                                   &
               *(1.-ZGEOM_TG(JJ)*PALB_HVEG(JJ)*ZGEOM_GT(JJ)*PALB_GD(JJ)                             )    &
               +ZGEOM_TR(JJ)*PALB_HVEG(JJ)                                                                    &
               *(ZGEOM_WG(JJ)*ZAALB_WL(JJ)*ZGEOM_GT(JJ)*PALB_GD(JJ)+ZGEOM_WT(JJ)*ZAALB_WL(JJ)     ) )& 
               / ZDENOM(JJ) * ZREF0_SW_RD(JJ)                                                                &
               + ( ZGEOM_WG(JJ)*ZAALB_WL(JJ)                                                                   &
               *(1.-ZGEOM_TR(JJ)*PALB_HVEG(JJ)*ZGEOM_RT(JJ)*ZAALB_RD(JJ)                              )    &
               +ZGEOM_TG(JJ)*PALB_HVEG(JJ)                                                                    &
               *(ZGEOM_WR(JJ)*ZAALB_WL(JJ)*ZGEOM_RT(JJ)*ZAALB_RD(JJ)+ZGEOM_WT(JJ)*ZAALB_WL(JJ)      ) )&
               / ZDENOM(JJ) * ZREF0_SW_GD(JJ)                                                              &
               + ( 1.-ZGEOM_RT(JJ)*ZAALB_RD(JJ)*ZGEOM_TR(JJ)*PALB_HVEG(JJ)                                     &
               -ZGEOM_TG(JJ)*PALB_HVEG(JJ)*ZGEOM_GT(JJ)*PALB_GD(JJ)                                    )  & 
               / ZDENOM(JJ) * ZREF0_SW_WL(JJ)
!
    ZSREF_SW_RD(JJ  )   =  ZGEOM_RW(JJ)*ZAALB_RD(JJ)*ZSREF_SW_WL(JJ)                     &
               +ZGEOM_RT(JJ)*ZAALB_RD(JJ)*ZSREF_SW_HV(JJ)                     &
               +ZREF0_SW_RD(JJ)
!
    ZSREF_SW_GD(JJ)     =  ZGEOM_GW(JJ)*PALB_GD(JJ)*ZSREF_SW_WL(JJ)                    &
               +ZGEOM_GT(JJ)*PALB_GD(JJ)*ZSREF_SW_HV(JJ)                    &
               +ZREF0_SW_GD(JJ)
!
!* 2. Case without explicit high vegetation
!     -------------------------------------
  ELSE
!
    ZGEOM_SR(JJ) = T%XSVF_RS(JJ) * ZRD(JJ)
    ZGEOM_SG(JJ) = T%XSVF_RS(JJ) * ZGD(JJ)
    ZGEOM_SW(JJ) = T%XSVF_SW(JJ) 
    ZGEOM_ST(JJ) = 0.
    ZGEOM_RT(JJ) = 0.
    ZGEOM_GT(JJ) = 0.
    ZGEOM_WT(JJ) = 0.
    ZGEOM_TW(JJ) = 0.
    ZGEOM_TR(JJ) = 0.
    ZGEOM_TG(JJ) = 0.
    ZGEOM_RW(JJ) = T%XSVF_RW(JJ)
    ZGEOM_GW(JJ) = T%XSVF_RW(JJ)
    ZGEOM_WR(JJ) = T%XSVF_WR(JJ) * ZRD(JJ)
    ZGEOM_WG(JJ) = T%XSVF_WR(JJ) * ZGD(JJ)
    ZGEOM_WW(JJ) = T%XSVF_WW(JJ)
!
!
    ZDENOM(JJ) =    1.- ZGEOM_WR(JJ)*ZAALB_WL(JJ)*ZGEOM_RW(JJ)*ZAALB_RD(JJ)      &
               - ZGEOM_WG(JJ)*ZAALB_WL(JJ)*ZGEOM_GW(JJ)*PALB_GD(JJ)     &
               - ZGEOM_WW(JJ)*ZAALB_WL(JJ)                             
!
    ZSREF_SW_HV(JJ) = 0. 
!
    ZSREF_SW_WL(JJ) =   ZGEOM_WR(JJ)*ZAALB_WL(JJ) / ZDENOM(JJ) * ZREF0_SW_RD(JJ)       &
               + ZGEOM_WG(JJ)*ZAALB_WL(JJ) / ZDENOM(JJ) * ZREF0_SW_GD(JJ)     &
               + 1. / ZDENOM(JJ) * ZREF0_SW_WL(JJ)
!
    ZSREF_SW_RD(JJ)   =  ZGEOM_RW(JJ)*ZAALB_RD(JJ)*ZSREF_SW_WL(JJ)                     &
               +ZREF0_SW_RD(JJ)
!
    ZSREF_SW_GD(JJ) =  ZGEOM_GW(JJ)*PALB_GD(JJ)*ZSREF_SW_WL(JJ)                    &
               +ZREF0_SW_GD(JJ)
!
  ENDIF
!
! *********************************************************************
!*      C.     total solar radiation received by sky
!              -------------------------------------
!
! half of energy from high veg received by sky (only up contribution)
  ZABS_SW_SKY (JJ)      =   ZSREF_SW_RD(JJ)   * ZGEOM_SR(JJ) &
            + ZSREF_SW_GD(JJ)   * ZGEOM_SG(JJ) &
            + ZSREF_SW_WL(JJ)   * ZGEOM_SW(JJ) &
            + ZSREF_SW_HV(JJ)   * ZGEOM_ST(JJ)      ! high veg reflects only upwards
!
!
!*      D.     total solar radiation received by roads and GARDEN areas
!              -------------------------------------------------------
!
  ZABS_SW_RD(JJ)      = (1.-T%XALB_ROAD(JJ) )                &
            * ( ZSW_RD(JJ)                       &
            + ZSREF_SW_WL(JJ) * ZGEOM_RW(JJ)   &
            + ZSREF_SW_HV(JJ) * ZGEOM_RT(JJ) )      ! high veg reflects only upwards
!
  ZABS_SW_SN_RD(JJ) = (1.-T%TSNOW_ROAD%ALB(JJ) )             &
            * ( ZSW_RD(JJ)                         &
            + ZSREF_SW_WL(JJ) * ZGEOM_RW(JJ)   &    
            + ZSREF_SW_HV(JJ) * ZGEOM_RT(JJ) )      ! high veg reflects only upwards
!
  ZABS_SW_GD(JJ)    = (1.-PALB_GD(JJ))                     &
            * (ZSW_GD(JJ)                       &
            + ZSREF_SW_WL(JJ) * ZGEOM_GW(JJ)   &     
            + ZSREF_SW_HV(JJ) * ZGEOM_GT(JJ) )      ! high veg reflects only upwards 

!
!*      E.     total solar radiation received by walls
!              ---------------------------------------
!
  ZABS_SW_WL(JJ)        = (1.-T%XALB_WALL(JJ))                 &
            * ( ZSW_WL(JJ)                       &
            + ZSREF_SW_RD(JJ)   * ZGEOM_WR(JJ)  &
            + ZSREF_SW_GD(JJ)   * ZGEOM_WG(JJ)  &
            + ZSREF_SW_WL(JJ)   * ZGEOM_WW(JJ)  &
            + ZSREF_SW_HV(JJ)   * ZGEOM_WT(JJ) )    ! high veg reflects only upwards 
       !
       ZABS_SW_WIN (JJ)      = (1.-B%XALB_WIN (JJ))                 &
            * (  ZSW_WL(JJ)                       &
            + ZSREF_SW_RD(JJ)   * ZGEOM_WR(JJ)  &
            + ZSREF_SW_GD(JJ)   * ZGEOM_WG(JJ)  &
            + ZSREF_SW_WL(JJ)   * ZGEOM_WW(JJ)  &
            + ZSREF_SW_HV(JJ)   * ZGEOM_WT(JJ) )    ! high veg reflects only upwards 

!
!*      F.     total solar radiation received by high veg
!              -------------------------------------------
!
  IF (T%XURBTREE(JJ).GT.0.) THEN
     ZABS_SW_HV(JJ)      =   (1./ T%XURBTREE(JJ)) * ( ZSW(JJ)                               &
               - (                                   ZABS_SW_SKY(JJ)           &
               +ZRD  (JJ)          * PDF_RD(JJ) * ZABS_SW_RD(JJ)            &
               +ZRD  (JJ)          * PDN_RD(JJ) * ZABS_SW_SN_RD(JJ)         &
               +ZGD  (JJ)          *              ZABS_SW_GD(JJ)            &
               + 2*T%XCAN_HW_RATIO(JJ) * (1.-B%XGR(JJ)) * ZABS_SW_WL(JJ)    &
               + 2*T%XCAN_HW_RATIO(JJ) *     B%XGR(JJ)  * ZABS_SW_WIN(JJ) )  )
  ELSE
      ZABS_SW_HV(JJ)      =   0.
  ENDIF
!
ENDDO
!
IF (LHOOK) CALL DR_HOOK('SOLAR_REFLECTIONS',1,ZHOOK_HANDLE)
!
END SUBROUTINE SOLAR_REFLECTIONS
!
!-------------------------------------------------------------------------------
!
SUBROUTINE TOWN_ALBEDO(ZSW,ZABS_SW_RF,ZABS_SW_SN_RF,              &
                       ZABS_SW_RD, ZABS_SW_SN_RD,ZABS_SW_WL,    &
                       ZABS_SW_GD, ZABS_SW_GR, ZABS_SW_WIN,     &
                       ZABS_SW_HV, ZABS_SW_PANEL,               &
                       ZALBEDO,ZSW_UP                           )  
!
REAL, DIMENSION(:), INTENT(IN) :: ZSW               ! incoming solar radiation
REAL, DIMENSION(:), INTENT(IN) :: ZABS_SW_RF        ! solar radiation absorbed by roofs
REAL, DIMENSION(:), INTENT(IN) :: ZABS_SW_RD        ! solar radiation absorbed by roads
REAL, DIMENSION(:), INTENT(IN) :: ZABS_SW_WL        ! solar radiation absorbed by walls
REAL, DIMENSION(:), INTENT(IN) :: ZABS_SW_WIN       ! solar radiation absorbed & transmitted by windows
REAL, DIMENSION(:), INTENT(IN) :: ZABS_SW_GD        ! solar radiation absorbed by GARDEN areas
REAL, DIMENSION(:), INTENT(IN) :: ZABS_SW_GR        ! solar radiation absorbed by green roof areas
REAL, DIMENSION(:), INTENT(IN) :: ZABS_SW_SN_RF     ! solar radiation absorbed by roof snow
REAL, DIMENSION(:), INTENT(IN) :: ZABS_SW_SN_RD     ! solar radiation absorbed by road snow
REAL, DIMENSION(:), INTENT(IN) :: ZABS_SW_HV        ! solar radiation absorbed by high veg areas
REAL, DIMENSION(:), INTENT(IN) :: ZABS_SW_PANEL     ! solar radiation absorbed by solar panels
REAL, DIMENSION(:), INTENT(OUT):: ZALBEDO           ! town averaged albedo

REAL, DIMENSION(:), INTENT(OUT):: ZSW_UP            ! outgoing solar radiation
INTEGER :: JJ
REAL(KIND=JPRB) :: ZHOOK_HANDLE

IF (LHOOK) CALL DR_HOOK('TOWN_ALBEDO',0,ZHOOK_HANDLE)
DO JJ=1,SIZE(ZSW)

  ZSW_UP(JJ) = ZSW(JJ)                                                           &
            - ( T%XBLD(JJ)   *(1.-T%XGREENROOF(JJ))*PDF_RF(JJ) *ZABS_SW_RF   (JJ)&
               +T%XBLD(JJ)   *(1.-T%XGREENROOF(JJ))*PDN_RF(JJ) *ZABS_SW_SN_RF(JJ)&
               +T%XBLD(JJ)   *    T%XGREENROOF(JJ)             *ZABS_SW_GR    (JJ)&
               +T%XBLD(JJ)   *    PFRAC_PANEL(JJ)              *ZABS_SW_PANEL (JJ)&
               +T%XROAD(JJ)                        *PDF_RD(JJ) *ZABS_SW_RD (JJ)    &
               +T%XROAD(JJ)                        *PDN_RD(JJ) *ZABS_SW_SN_RD(JJ)&
               +T%XGARDEN(JJ)                                  *ZABS_SW_GD(JJ)   &
               +T%XURBTREE(JJ)*(1.-T%XBLD(JJ))                 *ZABS_SW_HV(JJ)     &
               +T%XWALL_O_HOR(JJ)              *(1.-B%XGR(JJ)) *ZABS_SW_WL(JJ)     &
               +T%XWALL_O_HOR(JJ)              *    B%XGR(JJ)  *ZABS_SW_WIN (JJ)     )  
!
  IF (ZSW(JJ)>0.) THEN
    ZALBEDO(JJ)  = ZSW_UP(JJ) / ZSW(JJ)
  ELSE
    ZALBEDO(JJ)  = XUNDEF
  END IF
!
ENDDO
IF (LHOOK) CALL DR_HOOK('TOWN_ALBEDO',1,ZHOOK_HANDLE)
!
END SUBROUTINE TOWN_ALBEDO
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE URBAN_SOLAR_ABS
