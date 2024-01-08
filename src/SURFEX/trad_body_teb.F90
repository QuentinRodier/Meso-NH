!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
MODULE MODI_TRAD_BODY_TEB
INTERFACE
FUNCTION TRAD_BODY_TEB(HPROGRAM, PSCA_SW, PREF_SW_FAC, PREF_SW_GRND,       &
                       PEMIT_LW_FAC, PEMIT_LW_GRND, PEMIT_LW_HVEG,         &
                       PSCA_SW_SKY, PLW_RAD_SKY, PLW_RAD,                  &
                       PBLD, PBLD_HEIGHT, PWALL_O_HOR, PTAU_SR,            &
                       PDIR_SW, PZENITH, LSPARTACUS, PSCA_SW_GROUND_DOWN,  &
                       PSCA_SW_GROUND_UP, PSCA_SW_GROUND_HOR,              &
                       PLW_GROUND_DOWN, PLW_GROUND_HOR ) RESULT(PTRAD_BODY_TEB)
!
CHARACTER(LEN=6)  , INTENT(IN) :: HPROGRAM  ! program calling surf. schemes
REAL, DIMENSION(:), INTENT(IN) :: PSCA_SW       ! Diffuse solar radiation (W/m2)
REAL, DIMENSION(:), INTENT(IN) :: PREF_SW_FAC   ! Solar radiation reflected by facade [wall + glazing] (W/m2)
REAL, DIMENSION(:), INTENT(IN) :: PREF_SW_GRND  ! Solar radiation reflected by ground [road + garden] (W/m2)
REAL, DIMENSION(:), INTENT(IN) :: PEMIT_LW_FAC  ! Longwave radiation emitted by the facade [wall + glazing] (W/m2)
REAL, DIMENSION(:), INTENT(IN) :: PEMIT_LW_GRND ! Longwave radiation emitted by the ground [road + garden] (W/m2)
REAL, DIMENSION(:), INTENT(IN) :: PEMIT_LW_HVEG ! Longwave radiation emitted by the trees  [road + garden] (W/m2)
REAL, DIMENSION(:), INTENT(IN) :: PSCA_SW_SKY   ! Diffuse solar radiation attenuated by trees (W/m2)
REAL, DIMENSION(:), INTENT(IN) :: PLW_RAD_SKY   ! Atmospheric longwave radiation attenuated by trees (W/m2)
REAL, DIMENSION(:), INTENT(IN) :: PLW_RAD       ! Atmospheric longwave radiation (W/m2)
REAL, DIMENSION(:), INTENT(IN) :: PBLD          ! plan area density of building (m2(bld)/m2(urban))
REAL, DIMENSION(:), INTENT(IN) :: PBLD_HEIGHT   ! building height (m)
REAL, DIMENSION(:), INTENT(IN) :: PWALL_O_HOR   ! ratio between facade and urban horizontal surface (m2(facade)/m2(urban))
REAL, DIMENSION(:), INTENT(IN) :: PTAU_SR       ! part of radiation from sky not intercepted by trees
REAL, DIMENSION(:), INTENT(IN) :: PDIR_SW !Direct solar radiation (W/m2)
REAL, DIMENSION(:), INTENT(IN) :: PZENITH !solar zenithal angle (rad from vert.)
REAL, DIMENSION(:), INTENT(IN) :: PSCA_SW_GROUND_DOWN ! Diffusive downwelling solar radiation at ground level (W/m2)
REAL, DIMENSION(:), INTENT(IN) :: PSCA_SW_GROUND_UP   ! Diffusive upwelling solar radiation at ground level (W/m2)
REAL, DIMENSION(:), INTENT(IN) :: PSCA_SW_GROUND_HOR  ! Diffusive solar radiation in horizontal direction at ground level (W/m2)
REAL, DIMENSION(:), INTENT(IN) :: PLW_GROUND_DOWN     ! Downwelling longwave radiation at ground level (W/m2)
REAL, DIMENSION(:), INTENT(IN) :: PLW_GROUND_HOR      ! Longwave radiation in horizontal direction at ground level (W/m2)

LOGICAL, INTENT(IN) :: LSPARTACUS
!
REAL, DIMENSION(SIZE(PSCA_SW)) :: PTRAD_BODY_TEB
!
END FUNCTION TRAD_BODY_TEB
END INTERFACE
END MODULE MODI_TRAD_BODY_TEB
!   ##########################################################################
FUNCTION TRAD_BODY_TEB(HPROGRAM, PSCA_SW, PREF_SW_FAC, PREF_SW_GRND,         &
                   PEMIT_LW_FAC, PEMIT_LW_GRND, PEMIT_LW_HVEG,               &
                   PSCA_SW_SKY, PLW_RAD_SKY, PLW_RAD,                        &
                   PBLD, PBLD_HEIGHT, PWALL_O_HOR, PTAU_SR,                  &
                   PDIR_SW, PZENITH, LSPARTACUS, PSCA_SW_GROUND_DOWN,        &
                   PSCA_SW_GROUND_UP, PSCA_SW_GROUND_HOR, PLW_GROUND_DOWN,   &
                   PLW_GROUND_HOR ) RESULT(PTRAD_BODY_TEB)
!   ##########################################################################
!
!!****  *TRAD_BODY_TEB  
!!
!!    PURPOSE
!!    -------
!
!     Computes the radiant temperature equivalent to the total radiation
!     received by the human body
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
!!      V.MASSON   08/2014 : bug in road view factor in computation of Universal Thermal Climate Index (diagnostic only)
!!      V. Masson  12/2016 : bug in Fanger 1970 correction of direct radiation received by human body
!!      A. Lemonsu 06/2018 : include tree canopy effects
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_CSTS, ONLY : XSTEFAN, XPI, XSURF_EPSILON, XI0
USE MODI_ABOR1_SFX
USE MODI_GET_LUOUT
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
CHARACTER(LEN=6),   INTENT(IN) :: HPROGRAM      ! program calling surf. schemes
REAL, DIMENSION(:), INTENT(IN) :: PSCA_SW       ! Diffuse solar radiation (W/m2)
REAL, DIMENSION(:), INTENT(IN) :: PREF_SW_FAC   ! Solar radiation reflected by facade [wall + glazing] (W/m2)
REAL, DIMENSION(:), INTENT(IN) :: PREF_SW_GRND  ! Solar radiation reflected by ground [road + garden] (W/m2)
REAL, DIMENSION(:), INTENT(IN) :: PEMIT_LW_FAC  ! Longwave radiation emitted by the facade [wall + glazing] (W/m2)
REAL, DIMENSION(:), INTENT(IN) :: PEMIT_LW_GRND ! Longwave radiation emitted by the ground [road + garden] (W/m2)
REAL, DIMENSION(:), INTENT(IN) :: PEMIT_LW_HVEG ! Longwave radiation emitted by the tree canopy
REAL, DIMENSION(:), INTENT(IN) :: PSCA_SW_SKY   ! Diff sol rad from sky attenuated by trees 
REAL, DIMENSION(:), INTENT(IN) :: PLW_RAD_SKY   ! Atmospheric longwave radiation attenuated by trees
REAL, DIMENSION(:), INTENT(IN) :: PLW_RAD       ! Atmospheric longwave radiation (W/m2)
REAL, DIMENSION(:), INTENT(IN) :: PBLD          ! plan area density of building (m2(bld)/m2(urban))
REAL, DIMENSION(:), INTENT(IN) :: PBLD_HEIGHT   ! building height (m)
REAL, DIMENSION(:), INTENT(IN) :: PWALL_O_HOR   ! ratio between facade and urban horizontal surface (m2(facade)/m2(urban))
REAL, DIMENSION(:), INTENT(IN) :: PTAU_SR       ! part of radiation from sky not intercepted by trees
REAL, DIMENSION(:), INTENT(IN) :: PDIR_SW       ! Direct solar radiation (W/m2)
REAL, DIMENSION(:), INTENT(IN) :: PZENITH       ! solar zenithal angle (rad from vert.)
REAL, DIMENSION(:), INTENT(IN) :: PSCA_SW_GROUND_DOWN ! Diffusive downwelling solar radiation at ground level (W/m2)
REAL, DIMENSION(:), INTENT(IN) :: PSCA_SW_GROUND_UP   ! Diffusive upwelling solar radiation at ground level (W/m2)
REAL, DIMENSION(:), INTENT(IN) :: PSCA_SW_GROUND_HOR  ! Diffusive solar radiation at ground level in horizontal direction (W/m2)
REAL, DIMENSION(:), INTENT(IN) :: PLW_GROUND_DOWN     ! Downwelling longwave radiation at ground level (W/m2)
REAL, DIMENSION(:), INTENT(IN) :: PLW_GROUND_HOR      ! Longwave radiation in horizontal direction at ground level (W/m2)
LOGICAL, INTENT(IN)            :: LSPARTACUS
REAL, DIMENSION(SIZE(PSCA_SW)) :: PTRAD_BODY_TEB

!*      0.2    declarations of local variables
REAL :: ZHB = 1.7 !average height of human person (m)
REAL :: ZAB = 0.7 !absorption coef of solar radiation by human body
REAL :: ZEB = 0.97 !emissivity of human body
REAL, DIMENSION(SIZE(PBLD)) :: ZWROAD        ! width of the road (m)
REAL, DIMENSION(SIZE(PBLD)) :: ZL1, ZL2, ZL4 ! lengths for view factor calculation
REAL, DIMENSION(SIZE(PBLD)) :: ZFFAC         ! facade view factor of human body
REAL, DIMENSION(SIZE(PBLD)) :: ZFGRND        ! ground view factor of human body
REAL, DIMENSION(SIZE(PBLD)) :: ZFSKY         ! sky view factor of human body
REAL, DIMENSION(SIZE(PBLD)) :: ZFHVEG        ! trees view factor of human body
REAL, DIMENSION(SIZE(PBLD)) :: ZDIRSWBODY    ! solar radiation received by human body
REAL, DIMENSION(SIZE(PBLD)) :: ZELEV         ! solar elevation angle
REAL, DIMENSION(SIZE(PBLD)) :: ZRADBODY      ! total radiation received by human body
REAL :: ZDIR_PERP
INTEGER :: ILUOUT ! Unit number
INTEGER :: JJ
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('TRAD_BODY_TEB',0,ZHOOK_HANDLE)
!
DO JJ = 1, SIZE(PBLD_HEIGHT)
  !
  !*  1 - calculation of view factors
  ZWROAD(JJ) = PBLD_HEIGHT(JJ) * 2. * (1. - PBLD(JJ)) / PWALL_O_HOR(JJ)
  !
  ZL1(JJ) = SQRT(ZHB**2                   + (ZWROAD(JJ)/2.)**2)
  ZL2(JJ) = SQRT( PBLD_HEIGHT(JJ)**2      + (ZWROAD(JJ)/2.)**2)
  ZL4(JJ) = SQRT((PBLD_HEIGHT(JJ)-ZHB)**2 + (ZWROAD(JJ)/2.)**2)
  !
  ZFFAC (JJ) = (ZL1(JJ) + ZL2(JJ) - ZWROAD(JJ)/2. - ZL4(JJ)) / (2. * ZHB)
  ZFGRND(JJ) = 0.5*ZWROAD(JJ)/ZHB
  ZFGRND(JJ) = 0.5 * (ZFGRND(JJ) + 1. - SQRT(ZFGRND(JJ)**2 + 1.))
  !
  ZFSKY (JJ) = 1. - ZFFAC(JJ) - ZFGRND(JJ)
  !
  ! Trees are supposed, in this UTCI computation, to simply obscure the sky view factor
  ZFHVEG(JJ) = ZFSKY(JJ) * (1.-PTAU_SR(JJ))
  ZFSKY (JJ) = ZFSKY(JJ) *     PTAU_SR(JJ)
  !
  !
  !*  2 - base calculation for both sun and shade
  !
  IF (.NOT.LSPARTACUS) THEN
     !
     ! View factor based approach
     !
     ZRADBODY(JJ) = ZAB/ZEB * &
         (PSCA_SW_SKY  (JJ)*ZFSKY (JJ) + PREF_SW_FAC (JJ)*ZFFAC(JJ) + PREF_SW_GRND (JJ)*ZFGRND(JJ))  &
        + PLW_RAD_SKY  (JJ)*ZFSKY (JJ) + PEMIT_LW_FAC(JJ)*ZFFAC(JJ) + PEMIT_LW_GRND(JJ)*ZFGRND(JJ)   &
        + PEMIT_LW_HVEG(JJ)*ZFHVEG(JJ)
     !
  ELSE
     !
     ! SPARTACUS-Surface: Average between horzintal, up- and downwelling diffusive radiation
     !                    The weighting factor for horizontal (0.88), up- and downwelling (0.06 each)
     !                    radiative fluxes recommanded on
     !                    https://www.urbanclimate.net/rayman/description.htm are taken.
     !
     ! FIXME: It is assumed that all reflected radiation is diffusive
     !
     ZRADBODY(JJ) = ZAB/ZEB * &
        ( 0.06 * PSCA_SW_GROUND_DOWN(JJ) + 0.06 * PSCA_SW_GROUND_UP(JJ) + 0.88 * PSCA_SW_GROUND_HOR(JJ) ) &
        + 0.06 * PEMIT_LW_GRND(JJ)       + 0.06 * PLW_GROUND_DOWN(JJ)   + 0.88 * PLW_GROUND_HOR(JJ)    
     !
  ENDIF
  !
ENDDO
!
!*  3 - add direct contribution in case of sunny conditions 
DO JJ = 1, SIZE(PBLD_HEIGHT)
   ZELEV(JJ) = XPI/2. - PZENITH(JJ)
   IF (ZELEV(JJ) < 1E-6) ZELEV(JJ) = 0.
   !
   !The solar radiation must correspond to a plane 
   !perdendicular to the incident solar radiation.
   !However, PDIR_SW corresponds to the radiation received by
   !the surface. Therefore the radiation must be divided 
   !by sin(ZELEV).
   !
   ! Calculation of solar flux density with respect to a plane
   ! perpendicular to the direction of solar radiation
   !
   ZDIR_PERP = PDIR_SW(JJ) / MAX(SIN(ZELEV(JJ)),0.05) 
   !
   ! Limit to 0.85 time the solar constant
   !
   ZDIR_PERP = MAX(ZDIR_PERP, XSURF_EPSILON)
   ZDIR_PERP = MIN(ZDIR_PERP, 0.85 * XI0)
   !
   ZDIRSWBODY(JJ) = ZDIR_PERP * 0.308 * COS( ZELEV(JJ)*(1-(ZELEV(JJ)*180./XPI)**2/48402.) )
   !
   ! the direct solar radiation is weighted by a projected area factor which can be expressed by this equation
   ! for a rotationally symmetric human being (Fanger, 1970)
   !
   ZRADBODY (JJ) = ZRADBODY(JJ) + ZAB/ZEB*ZDIRSWBODY(JJ)
   !
ENDDO
!
IF ((MINVAL(ZDIRSWBODY).LT.-XSURF_EPSILON).OR.(MAXVAL(ZDIRSWBODY).GT.3000.0)) THEN
   CALL GET_LUOUT(HPROGRAM,ILUOUT)
   WRITE(ILUOUT,*) "                                                    "
   WRITE(ILUOUT,*) "Unplausible value of radiation received by the body "
   WRITE(ILUOUT,*) "                                                    "
   WRITE(ILUOUT,*) "MINVAL(ZDIRSWBODY) ",MINVAL(ZDIRSWBODY)
   WRITE(ILUOUT,*) "MAXVAL(ZDIRSWBODY) ",MAXVAL(ZDIRSWBODY)
   CALL FLUSH(ILUOUT)
   CALL ABOR1_SFX("TRAD_BODY_TEB: Error in radiation received by body, check report")
ENDIF
!
IF ((MINVAL(ZRADBODY).LT.-XSURF_EPSILON).OR.(MAXVAL(ZRADBODY).GT.3000.0)) THEN
   CALL GET_LUOUT(HPROGRAM,ILUOUT)
   WRITE(ILUOUT,*) "                                                    "
   WRITE(ILUOUT,*) "Unplausible value of radiation received by the body "
   WRITE(ILUOUT,*) "                                                    "
   WRITE(ILUOUT,*) "MINVAL(ZRADBODY) ",MINVAL(ZRADBODY)
   WRITE(ILUOUT,*) "MAXVAL(ZRADBODY) ",MAXVAL(ZRADBODY)
   CALL FLUSH(ILUOUT)
   CALL ABOR1_SFX("TRAD_BODY_TEB: Error in radiation received by body, check report")
ENDIF
!
PTRAD_BODY_TEB(:) = (ZRADBODY(:)/XSTEFAN)**0.25
!
IF (LHOOK) CALL DR_HOOK('TRAD_BODY_TEB',1,ZHOOK_HANDLE)
!
END FUNCTION TRAD_BODY_TEB
