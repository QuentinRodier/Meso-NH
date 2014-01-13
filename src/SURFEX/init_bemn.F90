!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #############################################################
      SUBROUTINE INIT_BEM_n(KLUOUT)
!     #############################################################
!
!!****  *INIT_BEM_n* - routine to initialize Building Energy Model
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
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
!!	G. Pigeon   *Meteo France*	
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    04/2012
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_TEB_n,           ONLY: XCOVER, XBLD, XBLD_HEIGHT, XWALL_O_HOR, XWALL_O_BLD, CBEM 
USE MODD_BEM_n,           ONLY: NFLOOR_LAYER, XHC_FLOOR, XTC_FLOOR, XD_FLOOR,            &
                                XTCOOL_TARGET, XTHEAT_TARGET, XF_WASTE_CAN, XEFF_HEAT,   &
                                XQIN, XQIN_FRAD, XSHGC, XSHGC_SH, XU_WIN, XGR,           &
                                XFLOOR_HEIGHT, XINF, XQIN_FLAT, XHR_TARGET, XV_VENT,     &
                                XCAP_SYS_HEAT, XAUX_MAX, XCAP_SYS_RAT, XT_ADP,           &
                                XM_SYS_RAT, XCOP_RAT, XT_SIZE_MAX, XT_SIZE_MIN,          &
                                CCOOL_COIL, CHEAT_COIL, XF_WATER_COND, LSHAD_DAY,        &
                                LNATVENT_NIGHT, LSHADE, XSHADE, CNATVENT, XNATVENT,      &
                                LAUTOSIZE, XT_WIN1, XALB_WIN, XABS_WIN, XUGG_WIN,        &
                                XN_FLOOR, XGLAZ_O_BLD, XMASS_O_BLD, XFLOOR_HW_RATIO,     &
                                XF_FLOOR_MASS, XF_FLOOR_WALL, XF_FLOOR_WIN,              &
                                XF_FLOOR_ROOF, XF_WALL_FLOOR, XF_WALL_MASS,              &
                                XF_WALL_WIN, XF_WIN_FLOOR, XF_WIN_MASS, XF_WIN_WALL,     &
                                XF_MASS_FLOOR, XF_MASS_WALL, XF_MASS_WIN, XTRAN_WIN,     &
                                XF_WIN_WIN
!
!
USE MODI_CONVERT_PATCH_TEB
USE MODI_WINDOW_DATA
USE MODI_HVAC_AUTOSIZE
USE MODI_BEM_MORPHO
USE MODI_STORES_HVAC_AUTOSIZE
!
USE MODI_ABOR1_SFX
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
INTEGER, INTENT(IN) :: KLUOUT ! logical unit of output listing
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER                         :: JJ               ! counter
INTEGER                         :: ILU              ! sizes of TEB arrays
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!         Initialisation for IO
!

IF (LHOOK) CALL DR_HOOK('INIT_BEM_N',0,ZHOOK_HANDLE)
!
!
!*       3.     Physiographic data fields from land cover:
!               -----------------------------------------
!
ILU = SIZE(XCOVER,1)
IF (CBEM=='DEF') ILU=0
!
ALLOCATE(XHC_FLOOR    (ILU,NFLOOR_LAYER))
ALLOCATE(XTC_FLOOR    (ILU,NFLOOR_LAYER))
ALLOCATE(XD_FLOOR     (ILU,NFLOOR_LAYER))
!
ALLOCATE(XTCOOL_TARGET(ILU))
ALLOCATE(XTHEAT_TARGET(ILU))
ALLOCATE(XEFF_HEAT    (ILU))
ALLOCATE(XSHGC        (ILU))
ALLOCATE(XQIN         (ILU))
ALLOCATE(XQIN_FRAD    (ILU))
ALLOCATE(XSHGC_SH     (ILU))
ALLOCATE(XU_WIN       (ILU))
ALLOCATE(XTRAN_WIN    (ILU))
ALLOCATE(XFLOOR_HEIGHT(ILU))
ALLOCATE(XINF         (ILU))
!
ALLOCATE(XQIN_FLAT    (ILU))
ALLOCATE(XHR_TARGET   (ILU))
ALLOCATE(XV_VENT      (ILU))
ALLOCATE(XCAP_SYS_HEAT(ILU))
ALLOCATE(XCAP_SYS_RAT (ILU))
ALLOCATE(XT_ADP       (ILU))
ALLOCATE(XM_SYS_RAT   (ILU))
ALLOCATE(XCOP_RAT     (ILU))
ALLOCATE(XT_SIZE_MAX  (ILU))
ALLOCATE(XT_SIZE_MIN  (ILU))
ALLOCATE(XF_WATER_COND(ILU))
ALLOCATE(CNATVENT     (ILU))
ALLOCATE(XNATVENT     (ILU))
!
ALLOCATE(XABS_WIN (ILU))
ALLOCATE(XUGG_WIN (ILU))
ALLOCATE(LSHADE   (ILU))
ALLOCATE(XSHADE   (ILU))
ALLOCATE(LSHAD_DAY(ILU))
ALLOCATE(LNATVENT_NIGHT(ILU))
ALLOCATE(XAUX_MAX    (ILU))
ALLOCATE(XN_FLOOR(ILU))
ALLOCATE(XGLAZ_O_BLD(ILU))
ALLOCATE(XMASS_O_BLD(ILU))
ALLOCATE(XFLOOR_HW_RATIO(ILU))
ALLOCATE(XF_FLOOR_MASS(ILU))
ALLOCATE(XF_FLOOR_WALL(ILU))
ALLOCATE(XF_FLOOR_WIN(ILU))
ALLOCATE(XF_FLOOR_ROOF(ILU))
ALLOCATE(XF_WALL_FLOOR(ILU))
ALLOCATE(XF_WALL_MASS(ILU))
ALLOCATE(XF_WALL_WIN(ILU))
ALLOCATE(XF_WIN_FLOOR(ILU))
ALLOCATE(XF_WIN_MASS(ILU))
ALLOCATE(XF_WIN_WALL(ILU))
ALLOCATE(XF_WIN_WIN(ILU))
ALLOCATE(XF_MASS_FLOOR(ILU))
ALLOCATE(XF_MASS_WALL(ILU))
ALLOCATE(XF_MASS_WIN(ILU))

SELECT CASE(CBEM)
!----------
CASE("DEF")
!-----------
   !parameters that needs to be 0 for calculation
   XGR  (:)         = 0.
   XF_WASTE_CAN(:)  = 0.
!----------
CASE("BEM")
!----------

  XAUX_MAX(:) = 5.
  CALL CONVERT_PATCH_TEB(XCOVER,0.,                                                   &
                      PHC_FLOOR=XHC_FLOOR, PTC_FLOOR=XTC_FLOOR, PD_FLOOR=XD_FLOOR,    &
                      PTCOOL_TARGET=XTCOOL_TARGET, PTHEAT_TARGET=XTHEAT_TARGET,       &
                      PF_WASTE_CAN=XF_WASTE_CAN, PEFF_HEAT=XEFF_HEAT, PQIN=XQIN,      &
                      PQIN_FRAD=XQIN_FRAD, PSHGC=XSHGC, PU_WIN=XU_WIN, PGR=XGR,       &
                      PSHGC_SH=XSHGC_SH, PFLOOR_HEIGHT=XFLOOR_HEIGHT, PINF=XINF,      &
                      PF_WATER_COND=XF_WATER_COND, PQIN_FLAT=XQIN_FLAT,               &
                      PHR_TARGET=XHR_TARGET, PV_VENT=XV_VENT,                         &
                      PCAP_SYS_HEAT=XCAP_SYS_HEAT, PCAP_SYS_RAT=XCAP_SYS_RAT,         &
                      PT_ADP=XT_ADP, PM_SYS_RAT=XM_SYS_RAT, PCOP_RAT=XCOP_RAT,        &
                      PT_SIZE_MAX=XT_SIZE_MAX, PT_SIZE_MIN=XT_SIZE_MIN,               &
                      PSHADE=XSHADE, PNATVENT=XNATVENT)
   !
   !
   ! *.     indoor relative surf. and view factors
   !        --------------------------------------
   !
   CALL BEM_MORPHO(XBLD, XWALL_O_HOR, XBLD_HEIGHT, XFLOOR_HEIGHT, XGR,         &
                   XN_FLOOR, XWALL_O_BLD, XGLAZ_O_BLD, XMASS_O_BLD,            &
                   XFLOOR_HW_RATIO, XF_FLOOR_MASS, XF_FLOOR_WALL, XF_FLOOR_WIN,&
                   XF_FLOOR_ROOF, XF_WALL_FLOOR, XF_WALL_MASS,                 &
                   XF_WALL_WIN, XF_WIN_FLOOR, XF_WIN_MASS, XF_WIN_WALL,        &
                   XF_MASS_FLOOR, XF_MASS_WALL, XF_MASS_WIN, XF_WASTE_CAN,     &
                   XF_WIN_WIN      )
   !
   ! *.     Window optical and thermal data
   !        -------------------------------
   !
   CALL WINDOW_DATA(ILU, XSHGC, XU_WIN, XALB_WIN, XABS_WIN, XUGG_WIN, XTRAN_WIN)
   DO JJ=1,SIZE(XSHADE)
      IF (XSHADE(JJ) >= 0.0 .AND. XSHADE(JJ) < 0.5) THEN
         LSHADE(JJ) = .FALSE.
      ELSEIF (XSHADE(JJ) >= 0.5 .AND. XSHADE(JJ) <= 1.0) THEN
         LSHADE(JJ) = .TRUE.
      ELSE
       PRINT*,'ERROR INTRODUCING SHADE'
      ENDIF
   ENDDO
   LSHAD_DAY(:) = .FALSE.
   !
   ! *.     Nocturnal surventilation
   !        ------------------------
   DO JJ=1,SIZE(XNATVENT)
      IF (XNATVENT(JJ) >= 0.0 .AND. XNATVENT(JJ) < 0.5) THEN
        CNATVENT(JJ) = 'NONE'
      ELSEIF (XNATVENT(JJ) >= 0.5 .AND. XNATVENT(JJ) < 1.5) THEN
        CNATVENT(JJ) = 'MANU'
      ELSEIF (XNATVENT(JJ) >= 1.5 .AND. XNATVENT(JJ) <= 2.) THEN
        CNATVENT(JJ) = 'AUTO'        
      ELSE
        PRINT*,'ERROR INTRODUCING NATVENT'
      ENDIF
    ENDDO

   LNATVENT_NIGHT(:) = .FALSE.
   !
END SELECT
!
!-------------------------------------------------------------------------------
!
!*       8.     Building HVAC automatic sizing:
!               -------------------------------  
IF (CBEM=='BEM' .AND. LAUTOSIZE) THEN
  CALL HVAC_AUTOSIZE(ILU,KLUOUT)
  !* stores the real systems characteristics in physiographic data 
  !  for further use
  CALL STORES_HVAC_AUTOSIZE
ENDIF
!
!-------------------------------------------------------------------------------
!
!
IF (LHOOK) CALL DR_HOOK('INIT_BEM_N',1,ZHOOK_HANDLE)
!
!
END SUBROUTINE INIT_BEM_n
