!MNH_LIC Copyright 2019-2023 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!---------------------------------------------------------------
!     #######################
MODULE MODI_DRAG_BLD
  !     #######################
  !
  INTERFACE
    !
    SUBROUTINE DRAG_BLD( PTSTEP, PUT, PVT, PTKET, PPABST, PTHT, PRT,              &
                         PSVT, PRHODJ, PZZ, PRUS, PRVS, PRTKES, PRTHS, PRRS,      &
                         PSFTH_WALL, PSFTH_ROOF, PCD_ROOF, PSFRV_WALL, PSFRV_ROOF )
       !
       REAL,                     INTENT(IN)    :: PTSTEP ! Time step
       REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PUT, PVT   ! variables
       REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PTKET           !   at t
       REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PPABST          !   at t
       REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PTHT            !   at t
       REAL, DIMENSION(:,:,:,:), INTENT(IN)    :: PRT             !   at t
       REAL, DIMENSION(:,:,:,:), INTENT(IN)    :: PSVT            !   at t
       !
       REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRHODJ    ! dry Density * Jacobian
       REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PZZ       ! Height (z)
       !
       REAL, DIMENSION(:,:),     INTENT(IN)    :: PSFTH_WALL ! Wall flux of theta
       REAL, DIMENSION(:,:),     INTENT(IN)    :: PSFTH_ROOF ! Roof flux of theta
       REAL, DIMENSION(:,:),     INTENT(IN)    :: PCD_ROOF   ! Drag coefficient due to roofs
       REAL, DIMENSION(:,:),     INTENT(IN)    :: PSFRV_WALL ! Wall flux of vapor
       REAL, DIMENSION(:,:),     INTENT(IN)    :: PSFRV_ROOF ! Roof flux of vapor
       !
       REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PRUS, PRVS       ! Sources of Momentum
       REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PRTKES           ! Sources of Tke
       REAL, DIMENSION(:,:,:,:), INTENT(INOUT) :: PRRS
       REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PRTHS
       !
     END SUBROUTINE DRAG_BLD

  END INTERFACE

END MODULE MODI_DRAG_BLD
!
!     #########################################################################
SUBROUTINE DRAG_BLD( PTSTEP, PUT, PVT, PTKET, PPABST, PTHT, PRT,              &
                     PSVT, PRHODJ, PZZ, PRUS, PRVS, PRTKES, PRTHS, PRRS,      &
                     PSFTH_WALL, PSFTH_ROOF, PCD_ROOF, PSFRV_WALL, PSFRV_ROOF )
  !     #######################################################################
  !
  !!****  *DRAG_BLD_n * -
  !!
  !!    PURPOSE
  !!    -------
  !
  !    Drag force due to buildings
  !
  !!**  METHOD
  !!    ------
  !!
  !!    REFERENCE
  !!    ---------
  !!
  !!    AUTHOR
  !!    ------
  !!     R. Schoetter
  !!
  !!    MODIFICATIONS
  !!    -------------
  !!      Original    09/2019
  !  P. Wautelet  04/03/2021: budgets: add DRAGB source term
  !  R. Schoetter    12/2021: multi-level coupling between MesoNH and SURFEX
  !!---------------------------------------------------------------
  !
  !*       0.    DECLARATIONS
  !              ------------
  !
  use modd_budget,     only: lbudget_u, lbudget_v, lbudget_tke, lbudget_th, lbudget_rv, &
                             NBUDGET_U, NBUDGET_V, NBUDGET_TKE, NBUDGET_TH, NBUDGET_RV, &
                             tbudgets
  USE MODD_CONF
  USE MODD_CST
  USE MODD_DRAGBLDG_n
  USE MODD_DYN
  USE MODD_DYN_n
  USE MODD_GROUND_PAR
  USE MODD_NSV
  USE MODD_PGDFIELDS

  use mode_budget,     only: Budget_store_init, Budget_store_end
  USE MODE_MSG

  USE MODI_MNHGET_SURF_PARAM_n
  USE MODI_SHUMAN
  !
  IMPLICIT NONE
  !  
  !*       0.1   Declarations of dummy arguments :
  !
  REAL,                     INTENT(IN)    :: PTSTEP   ! Time step
  REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PUT, PVT ! variables
  REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PTKET    !   at t
  REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PPABST   !   at t
  REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PTHT     !   at t
  REAL, DIMENSION(:,:,:,:), INTENT(IN)    :: PRT      !   at t
  REAL, DIMENSION(:,:,:,:), INTENT(IN)    :: PSVT     !   at t
  !
  REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRHODJ   ! dry Density * Jacobian
  REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PZZ      ! Height (z)
  !
  REAL, DIMENSION(:,:),     INTENT(IN)    :: PSFTH_WALL ! Wall flux of theta
  REAL, DIMENSION(:,:),     INTENT(IN)    :: PSFTH_ROOF ! Roof flux of theta
  REAL, DIMENSION(:,:),     INTENT(IN)    :: PCD_ROOF   ! Drag coefficient due to roofs
  REAL, DIMENSION(:,:),     INTENT(IN)    :: PSFRV_WALL ! Wall flux of vapor
  REAL, DIMENSION(:,:),     INTENT(IN)    :: PSFRV_ROOF ! Roof flux of vapor
  !
  REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PRUS, PRVS       ! Sources of Momentum
  REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PRTKES           ! Sources of Tke
  REAL, DIMENSION(:,:,:,:), INTENT(INOUT) :: PRRS
  REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PRTHS
  !
  !*       0.2   Declarations of local variables :
  !
  INTEGER :: IIU,IJU,IKU,IKV  ! array size along the k direction 
  INTEGER :: JI, JJ, JK       ! loop index
  INTEGER :: INFO_ll
  INTEGER :: ICHECK
  !
  REAL, DIMENSION(SIZE(PUT,1),SIZE(PUT,2),SIZE(PUT,3)) :: &
       ZWORK1, ZWORK2, ZWORK3, ZUT_SCAL, ZVT_SCAL, &
       ZUS, ZVS, ZTKES, ZTKET
  REAL, DIMENSION(SIZE(PUT,1),SIZE(PUT,2),SIZE(PUT,3)) :: &
       ZCDRAG, ZDENSITY
  !
  REAL, DIMENSION(SIZE(PUT,1),SIZE(PUT,2),SIZE(PUT,3)) :: ZCDRAG_BUILDG
  REAL, DIMENSION(SIZE(PUT,1),SIZE(PUT,2),SIZE(PUT,3)) :: ZCDRAG_ROOF
  REAL, DIMENSION(SIZE(PUT,1),SIZE(PUT,2),SIZE(PUT,3)) :: ZCDRAG_URBVEG
  REAL, DIMENSION(SIZE(PUT,1),SIZE(PUT,2),SIZE(PUT,3)) :: ZDENSITY_BUILDG
  REAL, DIMENSION(SIZE(PUT,1),SIZE(PUT,2),SIZE(PUT,3)) :: ZDENSITY_ROOF
  REAL, DIMENSION(SIZE(PUT,1),SIZE(PUT,2),SIZE(PUT,3)) :: ZDENSITY_URBVEG
  !
  REAL, DIMENSION(SIZE(PUT,1),SIZE(PUT,2),SIZE(PUT,3)) :: ZDELTA_T_WALL
  REAL, DIMENSION(SIZE(PUT,1),SIZE(PUT,2),SIZE(PUT,3)) :: ZDELTA_R_WALL
  REAL, DIMENSION(SIZE(PUT,1),SIZE(PUT,2),SIZE(PUT,3)) :: ZDELTA_T_ROOF
  REAL, DIMENSION(SIZE(PUT,1),SIZE(PUT,2),SIZE(PUT,3)) :: ZDELTA_R_ROOF
  !
  REAL, DIMENSION(SIZE(PUT,1),SIZE(PUT,2)) :: ZFRAC_TOWN   ! Town Fraction
  REAL, DIMENSION(SIZE(PUT,1),SIZE(PUT,2)) :: ZWALL_O_HOR  ! Wall surface density
  REAL, DIMENSION(SIZE(PUT,1),SIZE(PUT,2)) :: ZH_BLD     ! Building height
  REAL, DIMENSION(SIZE(PUT,1),SIZE(PUT,2)) :: ZF_BLD     ! Wall frontal density
  REAL, DIMENSION(SIZE(PUT,1),SIZE(PUT,2)) :: ZLAI_URBVEG  ! LAI of high urban vegetation
  REAL, DIMENSION(SIZE(PUT,1),SIZE(PUT,2)) :: ZH_URBTREE   ! Height of trees in urban vegetation
  REAL, DIMENSION(SIZE(PUT,1),SIZE(PUT,2)) :: ZH_URBTRUN ! Height of trunks in urban vegetation
  REAL, DIMENSION(SIZE(PUT,1),SIZE(PUT,2)) :: ZFRAC_HVEG   ! Fraction of high in urban vegetation
  REAL, DIMENSION(SIZE(PUT,3)) :: ZLAD_CAN
  REAL :: ZSUM_LAD_CAN, ZSUM_BLD_DENSITY, ZLEV_K0, ZLEV_K1
  REAL :: ZSUM_SFTH_WALL, ZSUM_SFTH_ROOF, ZSUM_SFRV_WALL, ZSUM_SFRV_ROOF 
  !
  !*       0.3     Initialization
  !
  if ( lbudget_u   ) call Budget_store_init( tbudgets(NBUDGET_U  ), 'DRAGB', prus  (:, :, :) )
  if ( lbudget_v   ) call Budget_store_init( tbudgets(NBUDGET_V  ), 'DRAGB', prvs  (:, :, :) )
  if ( lbudget_tke ) call Budget_store_init( tbudgets(NBUDGET_TKE), 'DRAGB', prtkes(:, :, :) )
  if ( lbudget_th  ) call Budget_store_init( tbudgets(NBUDGET_TH ), 'DRAGB', prths(:, :, :) )
  if ( lbudget_rv  ) call Budget_store_init( tbudgets(NBUDGET_RV ), 'DRAGB', prrs (:, :, :,1) )

  IIU = SIZE(PUT,1)
  IJU = SIZE(PUT,2)
  IKU = SIZE(PUT,3)
  !
  ZUS   (:,:,:) = 0.0
  ZVS   (:,:,:) = 0.0
  ZTKES (:,:,:) = 0.0
  !
  ZFRAC_TOWN   (:,:) = XUNDEF
  ZWALL_O_HOR  (:,:) = XUNDEF
  ZH_BLD       (:,:) = XUNDEF
  ZF_BLD       (:,:) = XUNDEF
  ZLAI_URBVEG  (:,:) = XUNDEF
  ZH_URBTREE   (:,:) = XUNDEF
  ZH_URBTRUN   (:,:) = XUNDEF
  ZFRAC_HVEG   (:,:) = XUNDEF
  !
  ZCDRAG_BUILDG   (:,:,:) = 0.
  ZCDRAG_ROOF     (:,:,:) = 0.
  ZCDRAG_URBVEG   (:,:,:) = 0.
  ZDENSITY_BUILDG (:,:,:) = 0.
  ZDENSITY_ROOF   (:,:,:) = 0.
  ZDENSITY_URBVEG (:,:,:) = 0.
  !
  ZDELTA_T_WALL (:,:,:) = 0.
  ZDELTA_R_WALL (:,:,:) = 0.
  ZDELTA_T_ROOF (:,:,:) = 0.
  ZDELTA_R_ROOF (:,:,:) = 0.
  !
  CALL MNHGET_SURF_PARAM_n( PTOWN = ZFRAC_TOWN, PBUILD_HEIGHT = ZH_BLD,         &
                            PWALL_O_HOR = ZWALL_O_HOR, PLAI_HVEG = ZLAI_URBVEG, &
                            PH_URBTREE = ZH_URBTREE, PHTRUNK_HVEG = ZH_URBTRUN, &
                            PFRAC_HVEG=ZFRAC_HVEG )
  !
  WHERE(ZFRAC_TOWN(:,:).GT.1.0) ZFRAC_TOWN(:,:)=0.0
  !
  WHERE(ZFRAC_TOWN (:,:).EQ.0.0)
     ZWALL_O_HOR (:,:) = 0.0
     ZH_BLD      (:,:) = 0.0
     ZLAI_URBVEG (:,:) = 0.0
     ZH_URBTREE  (:,:) = 0.0
     ZH_URBTRUN  (:,:) = 0.0
     ZFRAC_HVEG  (:,:) = 0.0
  ENDWHERE
  !
  ! For buildings, the frontal wall area density is calculated [m^2(walls facing the wind)/m^2]
  ! The division by PI means that cylindrical buildings are assumed (circle perimeter = PI*D, circle frontal area = D)
  ! [m^2(walls facing the wind)/m^2]=[m^2(wall)/m^2(town)*m^2(town)/m^2/PI]
  ! It will be assumed that the frontal wall area is equally distributed with height (all buildings same height)
  !
  ZF_BLD(:,:) = ZFRAC_TOWN (:,:) * ZWALL_O_HOR(:,:) / XPI
  !
  ! Set urban vegetation parameters to 0.0 in the case the drag shall not be accounted for
  !
  IF (.NOT.LDRAGURBVEG) THEN
     ZH_URBTREE  (:,:) = 0.0
     ZLAI_URBVEG (:,:) = 0.0
     ZH_URBTRUN  (:,:) = 0.0
     ZFRAC_HVEG  (:,:) = 0.0
  ENDIF
  !
  ! Limit trunk height to 0.3 times the tree height and check afterwards
  !
  DO JJ=2,(IJU-1)
     DO JI=2,(IIU-1)
        !
        ZH_URBTRUN(JI,JJ) = MIN(ZH_URBTRUN(JI,JJ),0.3*ZH_URBTREE(JI,JJ))
        !
        IF (ZH_URBTRUN(JI,JJ).GT.ZH_URBTREE(JI,JJ)) THEN
           CALL PRINT_MSG( NVERB_FATAL, 'GEN', 'DRAG_BLD', 'Trunk higher than tree' )
        ENDIF
        !
     ENDDO
  ENDDO
  !
  !-------------------------------------------------------------------------------
  !
  !*       1.     COMPUTES THE TRUE VELOCITY COMPONENTS
  !	        -------------------------------------
  !
  ZUT_SCAL(:,:,:) = MXF(PUT(:,:,:))
  ZVT_SCAL(:,:,:) = MYF(PVT(:,:,:))
  ZTKET(:,:,:)    = PTKET(:,:,:)
  !
  !-------------------------------------------------------------------------------
  !
  !*      1.     Computations of wind tendency due to canopy drag
  !              ------------------------------------------------
  !
  ! Ext = - Cdrag  * u- * u- * Sv       tree canopy drag
  !       - u'w'(ground)     * Sh       horizontal surfaces (ground)
  !              ------------------------------
  !
  DO JJ=2,(IJU-1)
     DO JI=2,(IIU-1)
        !
        ! Set density and drag coefficient for buildings
        ! The drag coefficient is set to 0.4 although studies like Santiago and Martilli (2010)
        ! provide hints that better results can be achieved with more sophisticated approaches.
        !
        IF (ZH_BLD(JI,JJ) /= 0) THEN
           !
           ZSUM_BLD_DENSITY = 0.0
           ICHECK=0
           !
           DO JK=2,(IKU-1) 
              !
              ! Height above ground of w-model levels (grid boundaries for density calculation)
              !
              ZLEV_K0 = PZZ(JI,JJ,JK  ) - PZZ(JI,JJ,2)
              ZLEV_K1 = PZZ(JI,JJ,JK+1) - PZZ(JI,JJ,2)
              !
              IF ( (ZLEV_K0.LT.ZH_BLD(JI,JJ)).AND.(ZLEV_K1.LT.ZH_BLD(JI,JJ)) ) THEN
                 !
                 ZCDRAG_BUILDG(JI,JJ,JK) = 0.4
                 !
                 ZDENSITY_BUILDG(JI,JJ,JK) = ZF_BLD(JI,JJ) / ZH_BLD(JI,JJ)
                 !
              ELSE IF ( (ZLEV_K0.LT.ZH_BLD(JI,JJ)).AND.(ZLEV_K1.GE.ZH_BLD(JI,JJ)) ) THEN
                 !
                 ICHECK=ICHECK+1
                 !
                 ZCDRAG_BUILDG(JI,JJ,JK) = 0.4
                 !
                 ZDENSITY_BUILDG(JI,JJ,JK) = ((ZH_BLD(JI,JJ)-ZLEV_K0)/(ZLEV_K1-ZLEV_K0)) * &
                   (ZF_BLD(JI,JJ) / ZH_BLD(JI,JJ))
                 !
                 ZCDRAG_ROOF  (JI,JJ,JK) = PCD_ROOF(JI,JJ)
                 ZDENSITY_ROOF(JI,JJ,JK) = 1.0 / ( ZLEV_K1 - ZLEV_K0 )
                 !
              ENDIF
              !
              ZSUM_BLD_DENSITY = ZSUM_BLD_DENSITY + ZDENSITY_BUILDG(JI,JJ,JK) * (ZLEV_K1-ZLEV_K0)
              !
           ENDDO
           !
           IF ( ICHECK .NE. 1 ) THEN
            CALL PRINT_MSG( NVERB_FATAL, 'GEN', 'DRAG_BLD', 'Roof level not attributed' )
           ENDIF
           !
           IF ( ABS(ZSUM_BLD_DENSITY-ZF_BLD(JI,JJ)) .GT. 1.0E-6 ) THEN
            CALL PRINT_MSG( NVERB_FATAL, 'GEN', 'DRAG_BLD', 'Wrong normalisation of frontal area density' )
           ENDIF
           !
        ENDIF
        !
        ! Set density and drag coefficient for urban vegetation
        !
        IF (ZH_URBTREE(JI,JJ) /= 0) THEN
           !
           ZLAD_CAN(:)  = 0.0
           ZSUM_LAD_CAN = 0.0
           !
           DO JK=2,(IKU-1)
              !
              ! Height above ground of model levels
              !
              ZLEV_K0 = PZZ(JI,JJ,JK  ) - PZZ(JI,JJ,2)
              ZLEV_K1 = PZZ(JI,JJ,JK+1) - PZZ(JI,JJ,2)
              !
              ! A) Model level entirely in between trunk and tree height
              !
              IF      ( (ZLEV_K0.LT.ZH_URBTREE(JI,JJ)).AND.(ZLEV_K1.LT.ZH_URBTREE(JI,JJ)) .AND. &
                        (ZLEV_K0.GT.ZH_URBTRUN(JI,JJ))) THEN
                 !
                 ZLAD_CAN(JK) = ZLAI_URBVEG(JI,JJ) / ( ZH_URBTREE(JI,JJ)-ZH_URBTRUN(JI,JJ) )
                 !
              !
              ! B) Model level intersects tree height, but not trunk height
              !
              ELSE IF ( (ZLEV_K0.LT.ZH_URBTREE(JI,JJ)).AND.(ZLEV_K1.GT.ZH_URBTREE(JI,JJ)).AND. &
                        (ZLEV_K0.GT.ZH_URBTRUN(JI,JJ)) ) THEN
                 !
                 ZLAD_CAN(JK) = (ZH_URBTREE(JI,JJ)-ZLEV_K0)/(ZLEV_K1-ZLEV_K0) * &
                 ( ZLAI_URBVEG(JI,JJ) / (ZH_URBTREE(JI,JJ)-ZH_URBTRUN(JI,JJ) ) )
                 !
              !
              ! C) Model level intersects trunk height, but not tree height
              !
              ELSE IF ( (ZLEV_K1.LT.ZH_URBTREE(JI,JJ)).AND.(ZLEV_K0.LT.ZH_URBTRUN(JI,JJ)).AND. &
                        (ZLEV_K1.GT.ZH_URBTRUN(JI,JJ)) ) THEN
                 !
                 ZLAD_CAN(JK) = (ZLEV_K1-ZH_URBTRUN(JI,JJ))/(ZLEV_K1-ZLEV_K0) * &
                   ( ZLAI_URBVEG(JI,JJ) / (ZH_URBTREE(JI,JJ)-ZH_URBTRUN(JI,JJ) ) )
                 !
              !
              ! D) Model level intersects both tree and trunk height
              !
              ELSE IF ( (ZLEV_K0.LT.ZH_URBTREE(JI,JJ)).AND.(ZLEV_K1.GT.ZH_URBTREE(JI,JJ)).AND. &
                        (ZLEV_K0.LT.ZH_URBTRUN(JI,JJ)) ) THEN
                 !
                 ZLAD_CAN(JK) = (ZH_URBTREE(JI,JJ)-ZH_URBTRUN(JI,JJ))/(ZLEV_K1-ZLEV_K0) * &
                        ( ZLAI_URBVEG(JI,JJ) / (ZH_URBTREE(JI,JJ)-ZH_URBTRUN(JI,JJ) ) )
                 !
                 ! Remark: This equation simplifies to ZLAD_CAN(JK) = ZLAI_URBVEG(JI,JJ) /(ZLEV_K1-ZLEV_K0)  
                 !
              ENDIF
              !
              ZSUM_LAD_CAN = ZSUM_LAD_CAN + ZLAD_CAN(JK)*(ZLEV_K1-ZLEV_K0)
              !
              ! The division by PI assumes isotropic orientation of leaves.
              ! To me it is not fully clear wether this is also considered in drag_veg.
              !
              ZDENSITY_URBVEG(JI,JJ,JK) = ZFRAC_TOWN(JI,JJ) * ZFRAC_HVEG(JI,JJ) * ZLAD_CAN(JK) / XPI
              ZCDRAG_URBVEG(JI,JJ,JK) = 0.20
              !
           ENDDO
           !
           ! Check for correct normalisation of PLAD_CAN
           !
           IF ( ABS(ZSUM_LAD_CAN-ZLAI_URBVEG(JI,JJ)).GT.1.0E-6 ) THEN
            CALL PRINT_MSG( NVERB_FATAL, 'GEN', 'DRAG_BLD', 'Wrong normalisation of vegetation density' )
           ENDIF
           !
        ENDIF
        !
     ENDDO
  ENDDO
  !
  !*      1.2    Drag force by wall surfaces
  !              ---------------------------
  !
  !* drag force by vertical surfaces
  !
  ZUS(:,:,:) = PUT(:,:,:)/( 1.0 + MXM ( ( ZCDRAG_BUILDG(:,:,:) * ZDENSITY_BUILDG(:,:,:) + &
                                          ZCDRAG_ROOF  (:,:,:) * ZDENSITY_ROOF  (:,:,:) + &
                                          ZCDRAG_URBVEG(:,:,:) * ZDENSITY_URBVEG(:,:,:) ) &
                                 * PTSTEP * SQRT(ZUT_SCAL(:,:,:)**2+ZVT_SCAL(:,:,:)**2) ) )
  !
  ZVS(:,:,:) = PVT(:,:,:)/( 1.0 + MYM ( ( ZCDRAG_BUILDG(:,:,:) * ZDENSITY_BUILDG(:,:,:) + &
                                          ZCDRAG_ROOF  (:,:,:) * ZDENSITY_ROOF  (:,:,:) + &
                                          ZCDRAG_URBVEG(:,:,:) * ZDENSITY_URBVEG(:,:,:) ) &
                                 * PTSTEP * SQRT(ZUT_SCAL(:,:,:)**2+ZVT_SCAL(:,:,:)**2) ) )
  !
  PRUS(:,:,:) = PRUS(:,:,:) + (ZUS(:,:,:)-PUT(:,:,:)) * MXM(PRHODJ(:,:,:)) / PTSTEP
  !
  PRVS(:,:,:) = PRVS(:,:,:) + (ZVS(:,:,:)-PVT(:,:,:)) * MYM(PRHODJ(:,:,:)) / PTSTEP
  !
  !
  !*      3.     Computations of TKE  tendency due to canopy drag
  !              ------------------------------------------------
  !*      3.1    Creation of TKE by wake
  !              -----------------------
  !
  ZTKES(:,:,:) = ( ZTKET(:,:,:) + PTSTEP * ( ZCDRAG_URBVEG(:,:,:) * ZDENSITY_URBVEG(:,:,:) + &
                                             ZCDRAG_ROOF  (:,:,:) * ZDENSITY_ROOF  (:,:,:) + &
                                             ZCDRAG_BUILDG(:,:,:) * ZDENSITY_BUILDG(:,:,:) ) &
     * (SQRT( ZUT_SCAL(:,:,:)**2 + ZVT_SCAL(:,:,:)**2 ))**3 ) /      &
       ( 1. + PTSTEP * ZCDRAG_URBVEG(:,:,:) * ZDENSITY_URBVEG(:,:,:) * &
     SQRT(ZUT_SCAL(:,:,:)**2+ZVT_SCAL(:,:,:)**2) )
  !
  PRTKES(:,:,:) = PRTKES(:,:,:) + ( ZTKES(:,:,:) - ZTKET(:,:,:) ) * PRHODJ(:,:,:) / PTSTEP
  !
  ! with Vair = Vair/Vtot * Vtot = (Vair/Vtot) * Stot * Dz
  ! and  Sv/Vair = (Sv/Stot) * Stot/Vair = (Sv/Stot) / (Vair/Vtot) / Dz
  !
  !* 4.     Computations of temperature and mixing ratio tendency due to wall and roof heat fluxes
  !              ------------------------------------------------
  !
  DO JJ=2,(IJU-1)
     DO JI=2,(IIU-1)
        !
        IF (ZH_BLD(JI,JJ) /= 0) THEN
           !
           ZSUM_SFTH_WALL = 0.0
           ZSUM_SFTH_ROOF = 0.0
           ZSUM_SFRV_WALL = 0.0
           ZSUM_SFRV_ROOF = 0.0
           !
           DO JK=2,(IKU-1) 
              !
              ! Height above ground of model levels
              !
              ZLEV_K0 = PZZ(JI,JJ,JK  ) - PZZ(JI,JJ,2)
              ZLEV_K1 = PZZ(JI,JJ,JK+1) - PZZ(JI,JJ,2)
              !
              ! Fluxes from walls distributed equally over entire building height
              !
              IF      ( (ZLEV_K0.LT.ZH_BLD(JI,JJ)).AND.(ZLEV_K1.LT.ZH_BLD(JI,JJ)) ) THEN
                 ZDELTA_T_WALL(JI,JJ,JK) = PSFTH_WALL(JI,JJ)*PTSTEP/ZH_BLD(JI,JJ)
                 ZDELTA_R_WALL(JI,JJ,JK) = PSFRV_WALL(JI,JJ)*PTSTEP/ZH_BLD(JI,JJ)
              ELSE IF ( (ZLEV_K0.LT.ZH_BLD(JI,JJ)).AND.(ZLEV_K1.GE.ZH_BLD(JI,JJ)) ) THEN
                 ZDELTA_T_WALL(JI,JJ,JK) = ((ZH_BLD(JI,JJ)-ZLEV_K0)/(ZLEV_K1-ZLEV_K0)) * & 
                    (PSFTH_WALL(JI,JJ)*PTSTEP/ZH_BLD(JI,JJ))
                 ZDELTA_R_WALL(JI,JJ,JK) = ((ZH_BLD(JI,JJ)-ZLEV_K0)/(ZLEV_K1-ZLEV_K0)) * &
                    (PSFRV_WALL(JI,JJ)*PTSTEP/ZH_BLD(JI,JJ))
              ENDIF
              !
              ZSUM_SFTH_WALL = ZSUM_SFTH_WALL + (ZLEV_K1-ZLEV_K0) * ZDELTA_T_WALL(JI,JJ,JK) / PTSTEP
              ZSUM_SFRV_WALL = ZSUM_SFRV_WALL + (ZLEV_K1-ZLEV_K0) * ZDELTA_R_WALL(JI,JJ,JK) / PTSTEP
              !
              ! Fluxes from roofs are applied at roof level
              !
              IF ( (ZLEV_K0.LT.ZH_BLD(JI,JJ)).AND.(ZLEV_K1.GE.ZH_BLD(JI,JJ)) ) THEN
                 ZDELTA_T_ROOF(JI,JJ,JK) = PSFTH_ROOF(JI,JJ)*PTSTEP/(ZLEV_K1-ZLEV_K0)
                 ZDELTA_R_ROOF(JI,JJ,JK) = PSFRV_ROOF(JI,JJ)*PTSTEP/(ZLEV_K1-ZLEV_K0)
              ENDIF
              !
              ZSUM_SFTH_ROOF = ZSUM_SFTH_ROOF + (ZLEV_K1-ZLEV_K0) * ZDELTA_T_ROOF(JI,JJ,JK) / PTSTEP
              ZSUM_SFRV_ROOF = ZSUM_SFRV_ROOF + (ZLEV_K1-ZLEV_K0) * ZDELTA_R_ROOF(JI,JJ,JK) / PTSTEP
              !
           ENDDO
           !
           ! Check for correct normalisation of fluxes
           !
           IF ( ABS(ZSUM_SFTH_WALL-PSFTH_WALL(JI,JJ)).GT.1.0E-6 ) THEN
            CALL PRINT_MSG( NVERB_FATAL, 'GEN', 'DRAG_BLD', 'Wrong normalisation of wall heat flux' )
           ENDIF
           !
           IF ( ABS(ZSUM_SFRV_WALL-PSFRV_WALL(JI,JJ)).GT.1.0E-6 ) THEN
            CALL PRINT_MSG( NVERB_FATAL, 'GEN', 'DRAG_BLD', 'Wrong normalisation of roof heat flux' )
           ENDIF
           !
           IF ( ABS(ZSUM_SFRV_ROOF-PSFRV_ROOF(JI,JJ)).GT.1.0E-6 ) THEN
            CALL PRINT_MSG( NVERB_FATAL, 'GEN', 'DRAG_BLD', 'Wrong normalisation of roof evaporative flux' )
           ENDIF
           !
        ENDIF
        !
     ENDDO
  ENDDO
  !
  PRTHS(:,:,:)  = PRTHS(:,:,:)  + ( ZDELTA_T_WALL(:,:,:) + ZDELTA_T_ROOF(:,:,:) ) * PRHODJ(:,:,:) / PTSTEP
  PRRS(:,:,:,1) = PRRS(:,:,:,1) + ( ZDELTA_R_WALL(:,:,:) + ZDELTA_R_ROOF(:,:,:) ) * PRHODJ(:,:,:) / PTSTEP
  !
  if ( lbudget_u   ) call Budget_store_end( tbudgets(NBUDGET_U  ), 'DRAGB', prus  (:, :, :) )
  if ( lbudget_v   ) call Budget_store_end( tbudgets(NBUDGET_V  ), 'DRAGB', prvs  (:, :, :) )
  if ( lbudget_tke ) call Budget_store_end( tbudgets(NBUDGET_TKE), 'DRAGB', prtkes(:, :, :) )
  if ( lbudget_th  ) call Budget_store_end( tbudgets(NBUDGET_TH ), 'DRAGB', prths (:, :, :) )
  if ( lbudget_rv  ) call Budget_store_end( tbudgets(NBUDGET_RV ), 'DRAGB', prrs  (:, :, :,1) )
  !
END SUBROUTINE DRAG_BLD
