!MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!     #######################
MODULE MODI_DRAG_BLD
  !     #######################
  !
  INTERFACE
     !
     SUBROUTINE DRAG_BLD(PTSTEP, PUT, PVT, PTKET, PRHODJ, PZZ, PRUS, PRVS, PRTKES )
       !
       REAL,                     INTENT(IN)    :: PTSTEP ! Time step
       REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PUT, PVT   ! variables
       REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PTKET           !   at t
       !
       REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRHODJ    ! dry Density * Jacobian
       REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PZZ       ! Height (z)
       !
       REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PRUS, PRVS       ! Sources of Momentum
       REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PRTKES           ! Sources of Tke
       !
     END SUBROUTINE DRAG_BLD

  END INTERFACE

END MODULE MODI_DRAG_BLD
!
!     ###################################################################
SUBROUTINE DRAG_BLD(PTSTEP, PUT, PVT, PTKET, PRHODJ, PZZ, PRUS, PRVS, PRTKES )
  !     ###################################################################
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
  !!---------------------------------------------------------------
  !
  !*       0.    DECLARATIONS
  !              ------------
  !
  USE MODD_CONF
  USE MODD_CST
  USE MODD_DRAGBLDG_n
  USE MODD_DYN
  USE MODD_DYN_n
  USE MODD_GROUND_PAR
  USE MODD_PGDFIELDS
  USE MODD_NSV
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
  !
  REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRHODJ   ! dry Density * Jacobian
  REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PZZ      ! Height (z)
  !
  REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PRUS, PRVS       ! Sources of Momentum
  REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PRTKES           ! Sources of Tke
  !
  !*       0.2   Declarations of local variables :
  !
  INTEGER :: IIU,IJU,IKU,IKV  ! array size along the k direction 
  INTEGER :: JI, JJ, JK       ! loop index
  INTEGER :: INFO_ll
  !
  REAL, DIMENSION(SIZE(PUT,1),SIZE(PUT,2),SIZE(PUT,3)) :: &
       ZWORK1, ZWORK2, ZWORK3, ZUT_SCAL, ZVT_SCAL, &
       ZUS, ZVS, ZTKES, ZTKET
  REAL, DIMENSION(SIZE(PUT,1),SIZE(PUT,2),SIZE(PUT,3)) :: &
       ZCDRAG, ZDENSITY
  !
  REAL, DIMENSION(:,:), ALLOCATABLE :: ZH_BUILD_PGD
  REAL, DIMENSION(:,:), ALLOCATABLE :: ZWALL_O_HOR_PGD
  REAL, DIMENSION(:,:), ALLOCATABLE :: ZFRAC_TOWN_PGD
  !
  REAL, DIMENSION(SIZE(PUT,1),SIZE(PUT,2)) ::           &
       ZH_BLD,ZF_BLD           !  Building height, frontal density
  REAL, DIMENSION(SIZE(PZZ,1),SIZE(PZZ,2),SIZE(PZZ,3)):: ZT,ZEXN,ZLV,ZCPH
  !
  !*       0.3     Initialization
  !
  IIU = SIZE(PUT,1)
  IJU = SIZE(PUT,2)
  IKU = SIZE(PUT,3)
  !
  ZUS   (:,:,:) = 0.0
  ZVS   (:,:,:) = 0.0
  ZTKES (:,:,:) = 0.0
  !
  ZH_BLD (:,:) = 0.
  ZF_BLD (:,:) = 0.
  !
  ZCDRAG   (:,:,:) = 0.
  ZDENSITY (:,:,:) = 0.
  !
  ALLOCATE(ZFRAC_TOWN_PGD(IIU,IJU))
  ALLOCATE(ZH_BUILD_PGD(IIU,IJU))
  ALLOCATE(ZWALL_O_HOR_PGD(IIU,IJU))
  !
  ZFRAC_TOWN_PGD  (:,:) = XUNDEF
  ZH_BUILD_PGD    (:,:) = XUNDEF
  ZWALL_O_HOR_PGD (:,:) = XUNDEF
  !
  CALL MNHGET_SURF_PARAM_n( PTOWN=ZFRAC_TOWN_PGD,    &
       PBUILD_HEIGHT=ZH_BUILD_PGD,                   &
       PWALL_O_HOR=ZWALL_O_HOR_PGD                   )
  !
  ! FIXME: Some values of ZFRAC_TOWN_PGD are 999. This is a bit strange since the
  !        TOWN fraction should be defined everywhere.
  !        It is set to 0.0 provisionally
  !
  WHERE(ZFRAC_TOWN_PGD(:,:).GT.1.0) ZFRAC_TOWN_PGD(:,:)=0.0
  !
  ! The values for wall density and building height are set to 0.0 where the Town fraction is 0.0
  ! For the wall density this would not be necessary since it will be multiplied by the town
  ! fraction anyway.
  !
  WHERE(ZFRAC_TOWN_PGD(:,:).EQ.0.0) 
     ZWALL_O_HOR_PGD(:,:) = 0.0
     ZH_BUILD_PGD(:,:) = 0.0
  ENDWHERE
  !
  ! For buildings, the frontal wall area density is calculated [m^2(walls facing the wind)/m^2]
  ! The division by PI means that cylindrical buildings are assumed (circle perimeter = PI*D, circle frontal area = D)
  ! [m^2(walls facing the wind)/m^2]=[m^2(wall)/m^2(town)*m^2(town)/m^2/PI]
  ! It will be assumed that the frontal wall area is equally distributed with height (all buildings same height)
  !
  ZH_BLD(:,:) = ZH_BUILD_PGD(:,:)
  ZF_BLD(:,:) = ZFRAC_TOWN_PGD(:,:)*ZWALL_O_HOR_PGD(:,:)/XPI
  !
  DEALLOCATE(ZFRAC_TOWN_PGD)
  DEALLOCATE(ZH_BUILD_PGD)
  DEALLOCATE(ZWALL_O_HOR_PGD)
  !
  !-------------------------------------------------------------------------------
  !
  !
  !*       1.     COMPUTES THE TRUE VELOCITY COMPONENTS
  !	        -------------------------------------
  !
  ZUT_SCAL(:,:,:) = MXF(PUT(:,:,:))
  ZVT_SCAL(:,:,:) = MYF(PVT(:,:,:))
  ZTKET(:,:,:)    = PTKET(:,:,:)
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
        !
        IF (ZH_BLD(JI,JJ) /= 0) THEN
           !
           DO JK=2,(IKU-1) 
              !
              IF ( (PZZ(JI,JJ,JK)-PZZ(JI,JJ,2)) .LT. ZH_BLD(JI,JJ) ) THEN
                 !
                 ! FIXME: Check literature values for the drag coefficient here
                 !
                 ZCDRAG(JI,JJ,JK)  = 0.2
                 !
                 ! A uniform distribution of building heights is assumed
                 !
                 ZDENSITY(JI,JJ,JK) = ZF_BLD(JI,JJ) / ZH_BLD(JI,JJ)
                 !
              ENDIF
              !
           ENDDO
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
  ZUS(:,:,:) = PUT(:,:,:)/( 1.0 + MXM ( ZCDRAG(:,:,:) * ZDENSITY(:,:,:) & 
       * PTSTEP * SQRT(ZUT_SCAL(:,:,:)**2+ZVT_SCAL(:,:,:)**2) ) )
  !
  ZVS(:,:,:) = PVT(:,:,:)/( 1.0 + MYM ( ZCDRAG(:,:,:) * ZDENSITY(:,:,:) & 
       * PTSTEP * SQRT(ZUT_SCAL(:,:,:)**2+ZVT_SCAL(:,:,:)**2) ) )
  !
  PRUS(:,:,:) = PRUS(:,:,:) + (ZUS(:,:,:)-PUT(:,:,:)) * MXM(PRHODJ(:,:,:)) / PTSTEP
  !
  PRVS(:,:,:) = PRVS(:,:,:) + (ZVS(:,:,:)-PVT(:,:,:)) * MYM(PRHODJ(:,:,:)) / PTSTEP
  !
  !*      3.     Computations of TKE  tendency due to canopy drag
  !              ------------------------------------------------
  !*      3.1    Creation of TKE by wake
  !              -----------------------
  !
  ! from Kanda and Hino (1994)
  !
  ! Ext = + Cd * u+^3  * Sv/Vair        vertical surfaces or trees
  !
  ! with Vair = Vair/Vtot * Vtot = (Vair/Vtot) * Stot * Dz
  ! and  Sv/Vair = (Sv/Stot) * Stot/Vair = (Sv/Stot) / (Vair/Vtot) / Dz
  !
  ZTKES(:,:,:) = ZTKET(:,:,:) + & 
     PTSTEP * ZCDRAG(:,:,:) * ZDENSITY(:,:,:) * (SQRT( ZUT_SCAL(:,:,:)**2 + ZVT_SCAL(:,:,:)**2 ))**3
  !
  PRTKES(:,:,:) = PRTKES(:,:,:) + (ZTKES(:,:,:)-ZTKET(:,:,:))*PRHODJ(:,:,:)/PTSTEP
  !
END SUBROUTINE DRAG_BLD
