!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #############################################################
      SUBROUTINE INIT_BEM_n ( DTCO, TOP, BOP, DTT, DTB, BDD, G, &
                              T, B, GDM, KSW, KLUOUT, HPROGRAM, HINIT)
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
!!      Jean Wurtz  05/2022 Adding check for compartment, initialization to 1 with 6 compartment cause invalid budget in BEM
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
USE MODD_CSTS, ONLY : XSURF_EPSILON
USE MODD_SURFEX_n, ONLY : TEB_GARDEN_MODEL_t
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
!
USE MODD_TEB_OPTION_n, ONLY : TEB_OPTIONS_t
USE MODD_BEM_OPTION_n, ONLY : BEM_OPTIONS_t
USE MODD_DATA_TEB_n, ONLY : DATA_TEB_t
USE MODD_DATA_BEM_n, ONLY : DATA_BEM_t
USE MODD_BLD_DESCRIPTION_n, ONLY : BLD_DESC_t
USE MODD_SFX_GRID_n, ONLY : GRID_t 
USE MODD_TEB_n, ONLY : TEB_t
USE MODD_BEM_n, ONLY : BEM_t
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
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(TEB_OPTIONS_t), INTENT(INOUT) :: TOP
TYPE(BEM_OPTIONS_t), INTENT(INOUT) :: BOP
TYPE(DATA_TEB_t), INTENT(INOUT) :: DTT
TYPE(DATA_BEM_t), INTENT(INOUT) :: DTB
TYPE(BLD_DESC_t), INTENT(INOUT) :: BDD
TYPE(TEB_GARDEN_MODEL_t), INTENT(INOUT) :: GDM
TYPE(GRID_t), INTENT(INOUT) :: G
TYPE(TEB_t), INTENT(INOUT) :: T
TYPE(BEM_t), INTENT(INOUT) :: B
!
INTEGER, INTENT(IN) :: KSW    ! number of short-wave spectral bands
INTEGER, INTENT(IN) :: KLUOUT ! logical unit of output listing
CHARACTER(LEN=6), INTENT(IN)     :: HPROGRAM    ! program calling surf. schemes
CHARACTER(LEN=3)    , INTENT(IN) :: HINIT       ! choice of fields to initialize
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER :: JJ         ! counter
INTEGER :: JCOMP      ! counter
INTEGER :: JLIST      ! counter
INTEGER :: JI         ! counter
INTEGER :: ILU        ! sizes of TEB arrays
INTEGER :: NDAY_SCHED ! Number of schedules for day of week
INTEGER :: NCRE_SCHED ! Number of schedules per day
INTEGER :: NHOLIDAY   ! Number of schedules per day
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
ILU = SIZE(TOP%XCOVER,1)
IF (TOP%CBEM=='DEF') ILU=0
!
NDAY_SCHED = 3
NCRE_SCHED = 4
NHOLIDAY   = 1
!
ALLOCATE(B%XHC_FLOOR    (ILU,BOP%NFLOOR_LAYER))
ALLOCATE(B%XTC_FLOOR    (ILU,BOP%NFLOOR_LAYER))
ALLOCATE(B%XD_FLOOR     (ILU,BOP%NFLOOR_LAYER))
!
ALLOCATE(B%XHC_MASS    (ILU,BOP%NMASS_LAYER))
ALLOCATE(B%XTC_MASS    (ILU,BOP%NMASS_LAYER))
ALLOCATE(B%XD_MASS     (ILU,BOP%NMASS_LAYER))
ALLOCATE(B%XISMASS     (ILU))
ALLOCATE(B%XSHGC        (ILU))
ALLOCATE(B%XQIN         (ILU,BOP%NBEMCOMP))
ALLOCATE(B%XQIN_FRAD    (ILU))
ALLOCATE(B%XSHGC_SH     (ILU))
ALLOCATE(B%XU_WIN       (ILU))
ALLOCATE(B%XTRAN_WIN    (ILU))
ALLOCATE(B%XFLOOR_HEIGHT(ILU))
ALLOCATE(B%XN50         (ILU))
!
ALLOCATE(B%XQIN_FLAT    (ILU))
ALLOCATE(B%XHR_TARGET   (ILU))
ALLOCATE(B%XCAP_SYS_HEAT(ILU))
ALLOCATE(B%XCAP_SYS_RAT (ILU))
ALLOCATE(B%XT_ADP       (ILU))
ALLOCATE(B%XM_SYS_RAT   (ILU))
ALLOCATE(B%XCOP_RAT     (ILU))
ALLOCATE(B%XCOP_DCS     (ILU))
ALLOCATE(B%XT_SIZE_MAX  (ILU))
ALLOCATE(B%XT_SIZE_MIN  (ILU))
ALLOCATE(B%XF_WATER_COND(ILU))
ALLOCATE(B%X_DCS_AREA   (ILU))
!
ALLOCATE(B%CNATVENT (ILU,BOP%NBEMCOMP))
ALLOCATE(B%XNATVENT (ILU,BOP%NBEMCOMP))
!
ALLOCATE(B%XSHAD_BEHAV_ANYWAY (ILU,BOP%NBEMCOMP))
ALLOCATE(B%XSHAD_BEHAV_ADAPTI (ILU,BOP%NBEMCOMP))
!
ALLOCATE(B%XISMECH      (ILU))
ALLOCATE(B%XMECHRATE    (ILU))
ALLOCATE(B%XSHADEARCHI  (ILU))
!
ALLOCATE(B%XABS_WIN (ILU))
ALLOCATE(B%XUGG_WIN (ILU))
ALLOCATE(B%XAUX_MAX    (ILU))
ALLOCATE(B%XN_FLOOR(ILU))
ALLOCATE(B%XGLAZ_O_BLD(ILU))
ALLOCATE(B%XMASS_O_BLD(ILU))
ALLOCATE(B%XFLOOR_HW_RATIO(ILU))
ALLOCATE(B%XF_FLOOR_MASS(ILU))
ALLOCATE(B%XF_FLOOR_WALL(ILU))
ALLOCATE(B%XF_FLOOR_WIN(ILU))
ALLOCATE(B%XF_FLOOR_ROOF(ILU))
ALLOCATE(B%XF_WALL_FLOOR(ILU))
ALLOCATE(B%XF_WALL_MASS(ILU))
ALLOCATE(B%XF_WALL_WIN(ILU))
ALLOCATE(B%XF_WIN_FLOOR(ILU))
ALLOCATE(B%XF_WIN_MASS(ILU))
ALLOCATE(B%XF_WIN_WALL(ILU))
ALLOCATE(B%XF_WIN_WIN(ILU))
ALLOCATE(B%XF_MASS_FLOOR(ILU))
ALLOCATE(B%XF_MASS_WALL(ILU))
ALLOCATE(B%XF_MASS_WIN(ILU))
ALLOCATE(B%XF_MASS_WIN(ILU))
ALLOCATE(B%XFSNIG(ILU,BOP%NBEMCOMP))
ALLOCATE(B%XFVNIG(ILU,BOP%NBEMCOMP))
ALLOCATE(B%XMODQIN_VCD(ILU,BOP%NBEMCOMP))
ALLOCATE(B%XMODQIN_VLD(ILU,BOP%NBEMCOMP))
ALLOCATE(B%XMODQIN_NIG(ILU,BOP%NBEMCOMP))
ALLOCATE(B%XTHEAT_OCCD(ILU,BOP%NBEMCOMP))
ALLOCATE(B%XTHEAT_OCCN(ILU,BOP%NBEMCOMP))
ALLOCATE(B%XTHEAT_VCDD(ILU,BOP%NBEMCOMP))
ALLOCATE(B%XTHEAT_VCDN(ILU,BOP%NBEMCOMP))
ALLOCATE(B%XTHEAT_VCLD(ILU,BOP%NBEMCOMP))
ALLOCATE(B%XTCOOL_OCCD(ILU,BOP%NBEMCOMP))
ALLOCATE(B%XTCOOL_OCCN(ILU,BOP%NBEMCOMP))
ALLOCATE(B%XTCOOL_VCDD(ILU,BOP%NBEMCOMP))
ALLOCATE(B%XTCOOL_VCDN(ILU,BOP%NBEMCOMP))
ALLOCATE(B%XTCOOL_VCLD(ILU,BOP%NBEMCOMP))
ALLOCATE(B%XTDESV(ILU))
ALLOCATE(B%XWIN_SW_MAX(ILU))
ALLOCATE(B%XFOPEN(ILU))
ALLOCATE(B%XFVSUM(ILU,BOP%NBEMCOMP))
ALLOCATE(B%XFVVAC(ILU,BOP%NBEMCOMP))
ALLOCATE(B%XFSSUM(ILU,BOP%NBEMCOMP))
ALLOCATE(B%XFSVAC(ILU,BOP%NBEMCOMP))
ALLOCATE(B%XBEG_HOLIDAY(ILU,NHOLIDAY,BOP%NBEMCOMP))
ALLOCATE(B%XEND_HOLIDAY(ILU,NHOLIDAY,BOP%NBEMCOMP))
ALLOCATE(B%XMOD_HOLIDAY(ILU,BOP%NBEMCOMP))
ALLOCATE(B%XDAYWBEG_SCHED(ILU,NDAY_SCHED,BOP%NBEMCOMP))
ALLOCATE(B%XHOURBEG_SCHED(ILU,NDAY_SCHED*NCRE_SCHED,BOP%NBEMCOMP))
ALLOCATE(B%XPROBOCC(ILU,NDAY_SCHED*NCRE_SCHED,BOP%NBEMCOMP))
!
! Dimension of FRACOMP must not be zero for HBEM=DEF
!
ALLOCATE(B%XFRACOMP(SIZE(TOP%XCOVER,1),BOP%NBEMCOMP))
ALLOCATE(B%XRESIDENTIAL(SIZE(TOP%XCOVER,1)))
!

SELECT CASE(TOP%CBEM)
!----------
CASE("DEF")
!-----------
   !parameters that needs to be 0 for calculation
   B%XGR  (:)         = 0.
   B%XF_WASTE_CAN(:)  = 0.
   B%XFRACOMP(:,1)    = 1.0
   B%XRESIDENTIAL(:)  = 1.0   
!----------
CASE("BEM")
!----------
!
! Robert: Further Allocations need to be made here
!
  ALLOCATE(B%XFRAC_HEAT_ELEC(ILU))
  ALLOCATE(B%XFRAC_HEAT_GAS(ILU))
  ALLOCATE(B%XFRAC_HEAT_FUEL(ILU))
  ALLOCATE(B%XFRAC_HEAT_OTHER(ILU))
  ALLOCATE(B%XF_HW_GAS(ILU))
  ALLOCATE(B%XHOTWAT(ILU,BOP%NBEMCOMP))
!
!

  B%XAUX_MAX(:) = 5.
  CALL CONVERT_PATCH_TEB(HPROGRAM, BDD, DTB, DTCO, DTT, TOP, 0., B = B  )
   !
   !
   ! *.     indoor relative surf. and view factors
   !        --------------------------------------
   !
   ! Robert: If indicated by architectural information the 
   !         floor height is set to the building height
   !
    DO JJ=1,SIZE(B%XISMASS,1)
      IF (B%XISMASS(JJ).LT.0.5) B%XFLOOR_HEIGHT(JJ)=T%XBLD_HEIGHT(JJ)
    ENDDO

   CALL BEM_MORPHO(T%XBLD, T%XWALL_O_HOR, T%XBLD_HEIGHT, &
                   T%XWALL_O_BLD, B     )
   !
   ! *.     Window optical and thermal data
   !        -------------------------------
   !
   CALL WINDOW_DATA(ILU, B )
   !
   ! *.     Ventilation modus
   !        ------------------------
   !
   DO JJ=1,SIZE(B%XNATVENT,1)
      DO JCOMP=1,SIZE(B%XNATVENT,2)
        IF     ( (B%XNATVENT(JJ,JCOMP).GE.0.0) .AND. (B%XNATVENT(JJ,JCOMP).LT.0.5) ) THEN
           B%CNATVENT(JJ,JCOMP) = 'NONE'
        ELSEIF ( (B%XNATVENT(JJ,JCOMP).GE.0.5) .AND. (B%XNATVENT(JJ,JCOMP).LT.1.5) ) THEN
           B%CNATVENT(JJ,JCOMP) = 'MANU'
        ELSEIF ( (B%XNATVENT(JJ,JCOMP).GE.1.5) .AND. (B%XNATVENT(JJ,JCOMP).LT.2.5) ) THEN
           B%CNATVENT(JJ,JCOMP) = 'AUTO'
        ELSE
          CALL ABOR1_SFX("INIT_BEM: Wrong option for ventilation modus")
        ENDIF
      ENDDO
   ENDDO
   !
END SELECT

DO JLIST=1,SIZE( B%XFRACOMP,1)
      !
      ! Check whether the individual compartment fractions by more than 1.0E-6 wrong
      !
      DO JI=1,SIZE( B%XFRACOMP,2)
         IF (( B%XFRACOMP(JLIST,JI).LT.-1.0E-6).OR.( B%XFRACOMP(JLIST,JI).GT.(1.0+1.0E-6))) THEN
		  CALL ABOR1_SFX("INIT_BEMN: Wrong fraction of compartments")
         ENDIF
      ENDDO
      !
      ! Very small deviations from 0.0 or 1.0 are corrected
      !
      WHERE ( B%XFRACOMP(JLIST,:).LT.0.0)
          B%XFRACOMP(JLIST,:) = 0.0
      ELSEWHERE ( B%XFRACOMP(JLIST,:).GT.1.0)
          B%XFRACOMP(JLIST,:) = 1.0
      ENDWHERE
      !
      ! Check whether the sum is far from 1.0
      !
      IF (ABS(SUM( B%XFRACOMP(JLIST,:))-1.0) .GT. 1.0E-6) THEN
		  CALL ABOR1_SFX("INIT_BEMN : Wrong initial sum of compartment fractions, ensure usage are given if NCOMP>1")
      ENDIF
      !
      ! Renormalise the sum
      !
       B%XFRACOMP(JLIST,:) =  B%XFRACOMP(JLIST,:)/SUM( B%XFRACOMP(JLIST,:))
      !
      ! Check whether the renormalised sum is exactly 1.0
      !
      IF (ABS(SUM( B%XFRACOMP(JLIST,:))-1.0).GT.XSURF_EPSILON) THEN
		  CALL ABOR1_SFX("INIT_BEMN : Wrong final sum of compartment fractions, ensure usage are given if NCOMP>1")
      ENDIF
      !

ENDDO

!
!-------------------------------------------------------------------------------
!
!*       8.     Building HVAC automatic sizing:
!               -------------------------------  
IF (TOP%CBEM=='BEM' .AND. BOP%LAUTOSIZE .AND. ((HINIT=='ALL').OR.(HINIT=='SOD')))  THEN
  CALL HVAC_AUTOSIZE(B, BOP, G, T, TOP, GDM, ILU, KSW, KLUOUT)
  !* stores the real systems characteristics in physiographic data 
  !  for further use
  CALL STORES_HVAC_AUTOSIZE(B, BOP, DTB)
ENDIF
!
!-------------------------------------------------------------------------------
!
!
IF (LHOOK) CALL DR_HOOK('INIT_BEM_N',1,ZHOOK_HANDLE)
!
!
END SUBROUTINE INIT_BEM_n
