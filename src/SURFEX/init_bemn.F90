!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #############################################################
      SUBROUTINE INIT_BEM_n ( DGU, DTCO, UG, U, TM, &
                             KLUOUT)
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
!!      G. Pigeon   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    04/2012
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
USE MODD_DIAG_SURF_ATM_n, ONLY : DIAG_SURF_ATM_t
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_SURFEX_n, ONLY : TEB_MODEL_t
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
!
TYPE(DIAG_SURF_ATM_t), INTENT(INOUT) :: DGU
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(TEB_MODEL_t), INTENT(INOUT) :: TM
!
INTEGER, INTENT(IN) :: KLUOUT ! logical unit of output listing
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER                         :: JJ               ! counter
INTEGER                         :: ILU              ! sizes of TEB arrays
LOGICAL                         :: GPRINT           ! flag for warning prints in output file
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
ILU = SIZE(TM%TOP%XCOVER,1)
IF (TM%TOP%CBEM=='DEF') ILU=0
!
ALLOCATE(TM%B%CUR%XHC_FLOOR    (ILU,TM%BOP%NFLOOR_LAYER))
ALLOCATE(TM%B%CUR%XTC_FLOOR    (ILU,TM%BOP%NFLOOR_LAYER))
ALLOCATE(TM%B%CUR%XD_FLOOR     (ILU,TM%BOP%NFLOOR_LAYER))
!
ALLOCATE(TM%B%CUR%XTCOOL_TARGET(ILU))
ALLOCATE(TM%B%CUR%XTHEAT_TARGET(ILU))
ALLOCATE(TM%B%CUR%XEFF_HEAT    (ILU))
ALLOCATE(TM%B%CUR%XSHGC        (ILU))
ALLOCATE(TM%B%CUR%XQIN         (ILU))
ALLOCATE(TM%B%CUR%XQIN_FRAD    (ILU))
ALLOCATE(TM%B%CUR%XSHGC_SH     (ILU))
ALLOCATE(TM%B%CUR%XU_WIN       (ILU))
ALLOCATE(TM%B%CUR%XTRAN_WIN    (ILU))
ALLOCATE(TM%B%CUR%XFLOOR_HEIGHT(ILU))
ALLOCATE(TM%B%CUR%XINF         (ILU))
!
ALLOCATE(TM%B%CUR%XQIN_FLAT    (ILU))
ALLOCATE(TM%B%CUR%XHR_TARGET   (ILU))
ALLOCATE(TM%B%CUR%XV_VENT      (ILU))
ALLOCATE(TM%B%CUR%XCAP_SYS_HEAT(ILU))
ALLOCATE(TM%B%CUR%XCAP_SYS_RAT (ILU))
ALLOCATE(TM%B%CUR%XT_ADP       (ILU))
ALLOCATE(TM%B%CUR%XM_SYS_RAT   (ILU))
ALLOCATE(TM%B%CUR%XCOP_RAT     (ILU))
ALLOCATE(TM%B%CUR%XT_SIZE_MAX  (ILU))
ALLOCATE(TM%B%CUR%XT_SIZE_MIN  (ILU))
ALLOCATE(TM%B%CUR%XF_WATER_COND(ILU))
ALLOCATE(TM%B%CUR%CNATVENT     (ILU))
ALLOCATE(TM%B%CUR%XNATVENT     (ILU))
!
ALLOCATE(TM%B%CUR%XABS_WIN (ILU))
ALLOCATE(TM%B%CUR%XUGG_WIN (ILU))
ALLOCATE(TM%B%CUR%LSHADE   (ILU))
ALLOCATE(TM%B%CUR%XSHADE   (ILU))
ALLOCATE(TM%B%CUR%LSHAD_DAY(ILU))
ALLOCATE(TM%B%CUR%LNATVENT_NIGHT(ILU))
ALLOCATE(TM%B%CUR%XAUX_MAX    (ILU))
ALLOCATE(TM%B%CUR%XN_FLOOR(ILU))
ALLOCATE(TM%B%CUR%XGLAZ_O_BLD(ILU))
ALLOCATE(TM%B%CUR%XMASS_O_BLD(ILU))
ALLOCATE(TM%B%CUR%XFLOOR_HW_RATIO(ILU))
ALLOCATE(TM%B%CUR%XF_FLOOR_MASS(ILU))
ALLOCATE(TM%B%CUR%XF_FLOOR_WALL(ILU))
ALLOCATE(TM%B%CUR%XF_FLOOR_WIN(ILU))
ALLOCATE(TM%B%CUR%XF_FLOOR_ROOF(ILU))
ALLOCATE(TM%B%CUR%XF_WALL_FLOOR(ILU))
ALLOCATE(TM%B%CUR%XF_WALL_MASS(ILU))
ALLOCATE(TM%B%CUR%XF_WALL_WIN(ILU))
ALLOCATE(TM%B%CUR%XF_WIN_FLOOR(ILU))
ALLOCATE(TM%B%CUR%XF_WIN_MASS(ILU))
ALLOCATE(TM%B%CUR%XF_WIN_WALL(ILU))
ALLOCATE(TM%B%CUR%XF_WIN_WIN(ILU))
ALLOCATE(TM%B%CUR%XF_MASS_FLOOR(ILU))
ALLOCATE(TM%B%CUR%XF_MASS_WALL(ILU))
ALLOCATE(TM%B%CUR%XF_MASS_WIN(ILU))

SELECT CASE(TM%TOP%CBEM)
!----------
 CASE("DEF")
!-----------
   !parameters that needs to be 0 for calculation
   TM%B%CUR%XGR  (:)         = 0.
   TM%B%CUR%XF_WASTE_CAN(:)  = 0.
!----------
 CASE("BEM")
!----------

  TM%B%CUR%XAUX_MAX(:) = 5.
  CALL CONVERT_PATCH_TEB(TM%BDD, TM%DTB, DTCO, TM%DTT, TM%TOP, &
                         TM%TOP%XCOVER,TM%TOP%LCOVER,0.,                 &
                      PHC_FLOOR=TM%B%CUR%XHC_FLOOR, PTC_FLOOR=TM%B%CUR%XTC_FLOOR, &
                      PD_FLOOR=TM%B%CUR%XD_FLOOR, PTCOOL_TARGET=TM%B%CUR%XTCOOL_TARGET, &
                      PTHEAT_TARGET=TM%B%CUR%XTHEAT_TARGET, PF_WASTE_CAN=TM%B%CUR%XF_WASTE_CAN, &
                      PEFF_HEAT=TM%B%CUR%XEFF_HEAT, PQIN=TM%B%CUR%XQIN, PQIN_FRAD=TM%B%CUR%XQIN_FRAD, &
                      PSHGC=TM%B%CUR%XSHGC, PU_WIN=TM%B%CUR%XU_WIN, PGR=TM%B%CUR%XGR,  &
                      PSHGC_SH=TM%B%CUR%XSHGC_SH, PFLOOR_HEIGHT=TM%B%CUR%XFLOOR_HEIGHT, &
                      PINF=TM%B%CUR%XINF, PF_WATER_COND=TM%B%CUR%XF_WATER_COND, &
                      PQIN_FLAT=TM%B%CUR%XQIN_FLAT, PHR_TARGET=TM%B%CUR%XHR_TARGET, &
                      PV_VENT=TM%B%CUR%XV_VENT, PCAP_SYS_HEAT=TM%B%CUR%XCAP_SYS_HEAT, &
                      PCAP_SYS_RAT=TM%B%CUR%XCAP_SYS_RAT, PT_ADP=TM%B%CUR%XT_ADP, &
                      PM_SYS_RAT=TM%B%CUR%XM_SYS_RAT, PCOP_RAT=TM%B%CUR%XCOP_RAT,     &
                      PT_SIZE_MAX=TM%B%CUR%XT_SIZE_MAX, PT_SIZE_MIN=TM%B%CUR%XT_SIZE_MIN,    &
                      PSHADE=TM%B%CUR%XSHADE, PNATVENT=TM%B%CUR%XNATVENT)
   !
   !
   ! *.     indoor relative surf. and view factors
   !        --------------------------------------
   !
   CALL BEM_MORPHO(TM%T%CUR%XBLD, TM%T%CUR%XWALL_O_HOR, TM%T%CUR%XBLD_HEIGHT, &
                   TM%B%CUR%XFLOOR_HEIGHT, TM%B%CUR%XGR, TM%B%CUR%XN_FLOOR, TM%T%CUR%XWALL_O_BLD, &
                   TM%B%CUR%XGLAZ_O_BLD, TM%B%CUR%XMASS_O_BLD, TM%B%CUR%XFLOOR_HW_RATIO, &
                   TM%B%CUR%XF_FLOOR_MASS, TM%B%CUR%XF_FLOOR_WALL, TM%B%CUR%XF_FLOOR_WIN, &
                   TM%B%CUR%XF_FLOOR_ROOF, TM%B%CUR%XF_WALL_FLOOR, TM%B%CUR%XF_WALL_MASS, &
                   TM%B%CUR%XF_WALL_WIN, TM%B%CUR%XF_WIN_FLOOR, TM%B%CUR%XF_WIN_MASS, &
                   TM%B%CUR%XF_WIN_WALL,  TM%B%CUR%XF_MASS_FLOOR, TM%B%CUR%XF_MASS_WALL, &
                   TM%B%CUR%XF_MASS_WIN, TM%B%CUR%XF_WASTE_CAN, TM%B%CUR%XF_WIN_WIN      )
   !
   ! *.     Window optical and thermal data
   !        -------------------------------
   !
   CALL WINDOW_DATA(ILU, TM%B%CUR%XSHGC, TM%B%CUR%XU_WIN, TM%B%CUR%XALB_WIN, &
                    TM%B%CUR%XABS_WIN, TM%B%CUR%XUGG_WIN, TM%B%CUR%XTRAN_WIN)
   GPRINT = .FALSE.
   DO JJ=1,SIZE(TM%B%CUR%XSHADE)
      IF (TM%B%CUR%XSHADE(JJ) >= 0.0 .AND. TM%B%CUR%XSHADE(JJ) < 0.5) THEN
         TM%B%CUR%LSHADE(JJ) = .FALSE.
      ELSEIF (TM%B%CUR%XSHADE(JJ) >= 0.5 .AND. TM%B%CUR%XSHADE(JJ) <= 1.0) THEN
         TM%B%CUR%LSHADE(JJ) = .TRUE.
      ELSE
       GPRINT = .TRUE.
       TM%B%CUR%LSHADE(JJ) = .FALSE.
      ENDIF
   ENDDO
   IF (GPRINT) WRITE(KLUOUT,*) &
   'TEB-BEM : Error in specifying shading devices for at least one point, no shading device for these points'
   TM%B%CUR%LSHAD_DAY(:) = .FALSE.
   !
   ! *.     Nocturnal surventilation
   !        ------------------------
   GPRINT = .FALSE.
   DO JJ=1,SIZE(TM%B%CUR%XNATVENT)
      IF (TM%B%CUR%XNATVENT(JJ) >= 0.0 .AND. TM%B%CUR%XNATVENT(JJ) < 0.5) THEN
        TM%B%CUR%CNATVENT(JJ) = 'NONE'
      ELSEIF (TM%B%CUR%XNATVENT(JJ) >= 0.5 .AND. TM%B%CUR%XNATVENT(JJ) < 1.5) THEN
        TM%B%CUR%CNATVENT(JJ) = 'MANU'
      ELSEIF (TM%B%CUR%XNATVENT(JJ) >= 1.5 .AND. TM%B%CUR%XNATVENT(JJ) <= 2.5) THEN
        TM%B%CUR%CNATVENT(JJ) = 'AUTO'        
      ELSEIF (TM%B%CUR%XNATVENT(JJ) >= 2.5 .AND. TM%B%CUR%XNATVENT(JJ) <= 3.5) THEN
        TM%B%CUR%CNATVENT(JJ) = 'MECH'        
      ELSE
        GPRINT = .TRUE.
        TM%B%CUR%CNATVENT(JJ) = 'NONE'        
      ENDIF
    ENDDO
    IF (GPRINT) WRITE(KLUOUT,*) 'TEB-BEM : Chosen option for surventilation is not yet implemented; None venting is kept instead'

   TM%B%CUR%LNATVENT_NIGHT(:) = .FALSE.
   !
END SELECT
!
!-------------------------------------------------------------------------------
!
!*       8.     Building HVAC automatic sizing:
!               -------------------------------  
IF (TM%TOP%CBEM=='BEM' .AND. TM%BOP%LAUTOSIZE) THEN
  CALL HVAC_AUTOSIZE(TM%BDD, DTCO, DGU, TM%B, TM%BOP, UG, U, TM%TG, TM%T, TM%TOP, &
                     ILU,KLUOUT)
  !* stores the real systems characteristics in physiographic data 
  !  for further use
  CALL STORES_HVAC_AUTOSIZE(TM%B, TM%BOP, TM%DTB)
ENDIF
!
!-------------------------------------------------------------------------------
!
!
IF (LHOOK) CALL DR_HOOK('INIT_BEM_N',1,ZHOOK_HANDLE)
!
!
END SUBROUTINE INIT_BEM_n
