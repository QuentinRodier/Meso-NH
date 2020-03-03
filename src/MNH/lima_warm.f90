!MNH_LIC Copyright 2013-2018 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!      #####################
       MODULE MODI_LIMA_WARM
!      #####################
!
INTERFACE
      SUBROUTINE LIMA_WARM (OACTIT, OSEDC, ORAIN, KSPLITR, PTSTEP, KMI,   &
                            TPFILE, OCLOSE_OUT, KRR, PZZ, PRHODJ,&
                            PRHODREF, PEXNREF, PW_NU, PPABSM, PPABST,     &
                            PTHM, PRCM,                                   &
                            PTHT, PRT, PSVT,                              &
                            PTHS, PRS, PSVS,                              &
                            PINPRC, PINPRR, PINDEP, PINPRR3D, PEVAP3D     )
!
USE MODD_IO_ll,   ONLY: TFILEDATA
!
LOGICAL,                  INTENT(IN)    :: OACTIT     ! Switch to activate the
                                                      ! activation by radiative
                                                      ! tendency
LOGICAL,                  INTENT(IN)    :: OSEDC      ! switch to activate the 
                                                      ! cloud droplet sedimentation
LOGICAL,                  INTENT(IN)    :: ORAIN      ! switch to activate the 
                                                      ! rain formation by coalescence
INTEGER,                  INTENT(IN)    :: KSPLITR    ! Number of small time step 
                                                      ! for sedimendation
REAL,                     INTENT(IN)    :: PTSTEP     ! Double Time step
                                                      ! (single if cold start)
INTEGER,                  INTENT(IN)    :: KMI        ! Model index 
TYPE(TFILEDATA),          INTENT(IN)    :: TPFILE     ! Output file
LOGICAL,                  INTENT(IN)    :: OCLOSE_OUT ! Conditional closure of 
                                                      ! the tput FM fileoutp
INTEGER,                  INTENT(IN)    :: KRR        ! Number of moist variables
!
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PZZ        ! Height (z)
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRHODJ     ! Dry density * Jacobian
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRHODREF   ! Reference density
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PEXNREF    ! Reference Exner function
!
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PW_NU      ! updraft velocity used for
                                                      ! the nucleation param.
!
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PPABSM     ! abs. pressure at time t-dt
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PPABST     ! abs. pressure at time t
!
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PTHM       ! Theta at time t-dt
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRCM       ! Cloud water m.r. at t-dt
!
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PTHT       ! Theta at time t
REAL, DIMENSION(:,:,:,:), INTENT(IN)    :: PRT        ! m.r. at t 
REAL, DIMENSION(:,:,:,:), INTENT(IN)    :: PSVT       ! Concentrations at t 
!
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PTHS       ! Theta source
REAL, DIMENSION(:,:,:,:), INTENT(INOUT) :: PRS        ! m.r. source
REAL, DIMENSION(:,:,:,:), INTENT(INOUT) :: PSVS       ! Concentrations source
!
!
!
REAL, DIMENSION(:,:),     INTENT(INOUT) :: PINPRC     ! Cloud instant precip
REAL, DIMENSION(:,:),     INTENT(INOUT) :: PINPRR     ! Rain instant precip
REAL, DIMENSION(:,:),     INTENT(INOUT) :: PINDEP     ! Cloud droplets deposition
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PINPRR3D   ! Rain inst precip 3D
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PEVAP3D    ! Rain evap profile
!
END SUBROUTINE LIMA_WARM
END INTERFACE
END MODULE MODI_LIMA_WARM
!     #####################################################################
      SUBROUTINE LIMA_WARM (OACTIT, OSEDC, ORAIN, KSPLITR, PTSTEP, KMI,   &
                            TPFILE, OCLOSE_OUT, KRR, PZZ, PRHODJ,         &
                            PRHODREF, PEXNREF, PW_NU, PPABSM, PPABST,     &
                            PTHM, PRCM,                                   &
                            PTHT, PRT, PSVT,                              &
                            PTHS, PRS, PSVS,                              &
                            PINPRC, PINPRR, PINDEP, PINPRR3D, PEVAP3D     )
!     #####################################################################
!
!!
!!    PURPOSE
!!    -------
!!      The purpose of this routine is to compute the warm microphysical 
!!    sources: nucleation, sedimentation, autoconversion, accretion,  
!!    self-collection and vaporisation which are parameterized according  
!!    to Cohard and Pinty, QJRMS, 2000
!!
!!
!!**  METHOD
!!    ------
!!      The activation of CCN is checked for quasi-saturated air parcels 
!!    to update the cloud droplet number concentration. Then assuming a 
!!    generalized gamma distribution law for the cloud droplets and the 
!!    raindrops, the zeroth and third order moments tendencies are evaluated
!!    for all the coalescence terms by integrating the Stochastic Collection 
!!    Equation. As autoconversion is a process that cannot be resolved 
!!    analytically, the Berry-Reinhardt parameterisation is employed with
!!    modifications to initiate the raindrop spectrum mode. The integration
!!    of the raindrop evaporation below clouds is straightforward.
!!
!!      The sedimentation rates are computed with a time spliting technique: 
!!    an upstream scheme, written as a difference of non-advective fluxes. 
!!    This source term is added to the next coming time step (split-implicit 
!!    process).
!!
!!    REFERENCE
!!    ---------
!!
!!      Cohard, J.-M. and J.-P. Pinty, 2000: A comprehensive two-moment warm 
!!      microphysical bulk scheme. 
!!        Part I: Description and tests
!!        Part II: 2D experiments with a non-hydrostatic model
!!      Accepted for publication in Quart. J. Roy. Meteor. Soc. 
!!
!!    AUTHOR
!!    ------
!!      J.-M. Cohard     * Laboratoire d'Aerologie*
!!      J.-P. Pinty      * Laboratoire d'Aerologie*
!!      S.    Berthet    * Laboratoire d'Aerologie*
!!      B.    Vié        * Laboratoire d'Aerologie*
!!
!!
!!    MODIFICATIONS
!!    -------------
!!      Original             ??/??/13 
!!      C. Barthe  * LACy *  jan. 2014   add budgets
!!      J. Escobar : for real*4 , use XMNH_HUGE
!!  Philippe Wautelet: 05/2016-04/2018: new data structures and calls for I/O
!!  B.Vié 03/02/2020 : correction of activation of water deposition on the ground
!!  B.Vié 03/03/2020 : use DTHRAD instead of dT/dt in Smax diagnostic computation
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_PARAMETERS
USE MODD_CST
USE MODD_CONF
USE MODD_PARAM_LIMA
USE MODD_PARAM_LIMA_WARM
USE MODD_NSV
!
!
USE MODD_BUDGET
USE MODI_BUDGET
!
USE MODE_FM
USE MODE_FMWRIT
!
USE MODI_LIMA_WARM_SEDIMENTATION
USE MODI_LIMA_WARM_NUCL
USE MODI_LIMA_WARM_COAL
USE MODI_LIMA_WARM_EVAP
USE MODD_IO_ll,   ONLY: TFILEDATA
USE MODD_LUNIT_n, ONLY: TLUOUT
!
IMPLICIT NONE
!
!*       0.1   Declarations of dummy arguments :
!
LOGICAL,                  INTENT(IN)    :: OACTIT     ! Switch to activate the
                                                      ! activation by radiative
                                                      ! tendency
LOGICAL,                  INTENT(IN)    :: OSEDC      ! switch to activate the 
                                                      ! cloud droplet sedimentation
LOGICAL,                  INTENT(IN)    :: ORAIN      ! switch to activate the 
                                                      ! rain formation by coalescence
INTEGER,                  INTENT(IN)    :: KSPLITR    ! Number of small time step 
                                                      ! for sedimendation
REAL,                     INTENT(IN)    :: PTSTEP     ! Double Time step
                                                      ! (single if cold start)
INTEGER,                  INTENT(IN)    :: KMI        ! Model index 
TYPE(TFILEDATA),          INTENT(IN)   :: TPFILE     ! Output file
LOGICAL,                  INTENT(IN)    :: OCLOSE_OUT ! Conditional closure of 
                                                      ! the tput FM fileoutp
INTEGER,                  INTENT(IN)    :: KRR        ! Number of moist variables
!
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PZZ        ! Height (z)
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRHODJ     ! Dry density * Jacobian
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRHODREF   ! Reference density
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PEXNREF    ! Reference Exner function
!
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PW_NU      ! updraft velocity used for
                                                      ! the nucleation param.
!
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PPABSM     ! abs. pressure at time t-dt
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PPABST     ! abs. pressure at time t
!
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PTHM       ! Theta at time t-dt
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRCM       ! Cloud water m.r. at t-dt
!
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PTHT       ! Theta at time t
REAL, DIMENSION(:,:,:,:), INTENT(IN)    :: PRT        ! m.r. at t 
REAL, DIMENSION(:,:,:,:), INTENT(IN)    :: PSVT       ! Concentrations at t 
!
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PTHS       ! Theta source
REAL, DIMENSION(:,:,:,:), INTENT(INOUT) :: PRS        ! m.r. source
REAL, DIMENSION(:,:,:,:), INTENT(INOUT) :: PSVS       ! Concentrations source
!
!
!
REAL, DIMENSION(:,:),     INTENT(INOUT) :: PINPRC     ! Cloud instant precip
REAL, DIMENSION(:,:),     INTENT(INOUT) :: PINPRR     ! Rain instant precip
REAL, DIMENSION(:,:),     INTENT(INOUT) :: PINDEP     ! Cloud droplets deposition
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PINPRR3D   ! Rain inst precip 3D
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PEVAP3D    ! Rain evap profile
!
!*       0.2   Declarations of local variables :
!
REAL, DIMENSION(SIZE(PRHODREF,1),SIZE(PRHODREF,2),SIZE(PRHODREF,3))   &
                                    :: PRVT,    & ! Water vapor m.r. at t 
                                       PRCT,    & ! Cloud water m.r. at t 
                                       PRRT,    & ! Rain water m.r. at t 
                                       !
                                       PRVS,    & ! Water vapor m.r. source
                                       PRCS,    & ! Cloud water m.r. source
                                       PRRS,    & ! Rain water m.r. source
                                       !
                                       PCCT,    & ! Cloud water C. at t
                                       PCRT,    & ! Rain water C. at t
                                       !
                                       PCCS,    & ! Cloud water C. source
                                       PCRS       ! Rain water C. source
!
REAL, DIMENSION(:,:,:,:), ALLOCATABLE :: ZNFS     ! CCN C. available source
                                                  !used as Free ice nuclei for
                                                  !HOMOGENEOUS nucleation of haze
REAL, DIMENSION(:,:,:,:), ALLOCATABLE :: ZNAS     ! Cloud  C. nuclei C. source
                                                  !used as Free ice nuclei for
                                                  !IMMERSION freezing
!
!
REAL,    DIMENSION(SIZE(PRHODREF,1),SIZE(PRHODREF,2),SIZE(PRHODREF,3))   &
                                  :: ZT
REAL,    DIMENSION(SIZE(PRHODREF,1),SIZE(PRHODREF,2),SIZE(PRHODREF,3))   &
                                  :: ZWLBDR,ZWLBDR3,ZWLBDC,ZWLBDC3
INTEGER :: JL
!
LOGICAL, DIMENSION(SIZE(PRHODREF,1),SIZE(PRHODREF,2)) :: GDEP
!
!-------------------------------------------------------------------------------
!
!
!*       0.     3D MICROPHYSCAL VARIABLES
!	        -------------------------
!
!
! Prepare 3D water mixing ratios
PRVT(:,:,:) = PRT(:,:,:,1)
PRVS(:,:,:) = PRS(:,:,:,1)
!
PRCT(:,:,:) = 0.
PRCS(:,:,:) = 0.
PRRT(:,:,:) = 0.
PRRS(:,:,:) = 0.
!
IF ( KRR .GE. 2 ) PRCT(:,:,:) = PRT(:,:,:,2)
IF ( KRR .GE. 2 ) PRCS(:,:,:) = PRS(:,:,:,2)
IF ( KRR .GE. 3 ) PRRT(:,:,:) = PRT(:,:,:,3)
IF ( KRR .GE. 3 ) PRRS(:,:,:) = PRS(:,:,:,3)
!
! Prepare 3D number concentrations
PCCT(:,:,:) = 0.
PCRT(:,:,:) = 0.
PCCS(:,:,:) = 0.
PCRS(:,:,:) = 0.
!
IF ( LWARM ) PCCT(:,:,:) = PSVT(:,:,:,NSV_LIMA_NC)
IF ( LWARM .AND. LRAIN ) PCRT(:,:,:) = PSVT(:,:,:,NSV_LIMA_NR)
!
IF ( LWARM ) PCCS(:,:,:) = PSVS(:,:,:,NSV_LIMA_NC)
IF ( LWARM .AND. LRAIN ) PCRS(:,:,:) = PSVS(:,:,:,NSV_LIMA_NR)
!
IF ( NMOD_CCN .GE. 1 ) THEN
   ALLOCATE( ZNFS(SIZE(PRHODJ,1),SIZE(PRHODJ,2),SIZE(PRHODJ,3),NMOD_CCN) )
   ALLOCATE( ZNAS(SIZE(PRHODJ,1),SIZE(PRHODJ,2),SIZE(PRHODJ,3),NMOD_CCN) )
   ZNFS(:,:,:,:) = PSVS(:,:,:,NSV_LIMA_CCN_FREE:NSV_LIMA_CCN_FREE+NMOD_CCN-1)
   ZNAS(:,:,:,:) = PSVS(:,:,:,NSV_LIMA_CCN_ACTI:NSV_LIMA_CCN_ACTI+NMOD_CCN-1)
ELSE
   ALLOCATE( ZNFS(SIZE(PRHODJ,1),SIZE(PRHODJ,2),SIZE(PRHODJ,3),1) )
   ALLOCATE( ZNAS(SIZE(PRHODJ,1),SIZE(PRHODJ,2),SIZE(PRHODJ,3),1) )
   ZNFS(:,:,:,:) = 0.
   ZNAS(:,:,:,:) = 0.
END IF
!
!-------------------------------------------------------------------------------
!
!
!*       1.     COMPUTE THE SLOPE PARAMETERS ZLBDC,ZLBDR
!   	        ----------------------------------------
!
!
ZWLBDC3(:,:,:) = XMNH_HUGE
ZWLBDC(:,:,:)  = 1.E15
!
WHERE (PRCT(:,:,:)>XRTMIN(2) .AND. PCCT(:,:,:)>XCTMIN(2))
   ZWLBDC3(:,:,:) = XLBC * PCCT(:,:,:) / PRCT(:,:,:)
   ZWLBDC(:,:,:)  = ZWLBDC3(:,:,:)**XLBEXC
END WHERE
!
ZWLBDR3(:,:,:) = 1.E30
ZWLBDR(:,:,:)  = 1.E10
WHERE (PRRT(:,:,:)>XRTMIN(3) .AND. PCRT(:,:,:)>XCTMIN(3))
   ZWLBDR3(:,:,:) = XLBR * PCRT(:,:,:) / PRRT(:,:,:)
   ZWLBDR(:,:,:)  = ZWLBDR3(:,:,:)**XLBEXR
END WHERE
ZT(:,:,:)  = PTHT(:,:,:) * (PPABST(:,:,:)/XP00)**(XRD/XCPD)
!
!-------------------------------------------------------------------------------
!
!
!*       2.     COMPUTE THE SEDIMENTATION (RS) SOURCE
!	        -------------------------------------
!
!
CALL LIMA_WARM_SEDIMENTATION (OSEDC, KSPLITR, PTSTEP, KMI,  &
                              PZZ, PRHODREF, PPABST, ZT,    &
                              ZWLBDC,                       &
                              PRCT, PRRT, PCCT, PCRT,       &
                              PRCS, PRRS, PCCS, PCRS,       &
                              PINPRC, PINPRR,               &
                              PINPRR3D    )
!
IF (LBUDGET_RC .AND. OSEDC) CALL BUDGET (PRCS(:,:,:)*PRHODJ(:,:,:),7 ,'SEDI_BU_RRC')
IF (LBUDGET_RR .AND. ORAIN) CALL BUDGET (PRRS(:,:,:)*PRHODJ(:,:,:),8 ,'SEDI_BU_RRR')
IF (LBUDGET_SV) THEN
  IF (OSEDC) CALL BUDGET (PCCS(:,:,:)*PRHODJ(:,:,:),12+NSV_LIMA_NC,'SEDI_BU_RSV')
  IF (ORAIN) CALL BUDGET (PCRS(:,:,:)*PRHODJ(:,:,:),12+NSV_LIMA_NR,'SEDI_BU_RSV')
END IF
!
! 2.bis Deposition at 1st level above ground
!
IF (LDEPOC) THEN
  PINDEP(:,:)=0.
  GDEP(:,:) = .FALSE.
  GDEP(:,:) =    PRCS(:,:,2) >0 .AND. PCCS(:,:,2) >0 .AND. PRCT(:,:,2) >0 .AND. PCCT(:,:,2) >0
  WHERE (GDEP)
     PRCS(:,:,2) = PRCS(:,:,2) - XVDEPOC * PRCT(:,:,2) / ( PZZ(:,:,3) - PZZ(:,:,2))
     PCCS(:,:,2) = PCCS(:,:,2) - XVDEPOC * PCCT(:,:,2) / ( PZZ(:,:,3) - PZZ(:,:,2))
     PINPRC(:,:) = PINPRC(:,:) + XVDEPOC * PRCT(:,:,2) * PRHODREF(:,:,2) /XRHOLW
     PINDEP(:,:) = XVDEPOC * PRCT(:,:,2) *  PRHODREF(:,:,2) /XRHOLW
  END WHERE
!
  IF ( LBUDGET_RC ) CALL BUDGET (PRCS(:,:,:)*PRHODJ(:,:,:),7             ,'DEPO_BU_RRC')
  IF ( LBUDGET_SV ) CALL BUDGET (PCCS(:,:,:)*PRHODJ(:,:,:),12+NSV_LIMA_NC,'DEPO_BU_RSV') 
END IF
! 
!-------------------------------------------------------------------------------
!
!*       2.     COMPUTES THE NUCLEATION PROCESS SOURCES
!   	        --------------------------------------
!
!
IF (LACTI .AND. NMOD_CCN.GE.1) THEN
!
   CALL LIMA_WARM_NUCL (OACTIT, PTSTEP, KMI, TPFILE, OCLOSE_OUT,&
                        PRHODREF, PEXNREF, PPABST, ZT, PTHM, PW_NU,       &
                        PRCM, PRVT, PRCT, PRRT,                          &
                        PTHS, PRVS, PRCS, PCCS, ZNFS, ZNAS               )
!
   IF (LBUDGET_TH) CALL BUDGET (PTHS(:,:,:)*PRHODJ(:,:,:),4,'HENU_BU_RTH')
   IF (LBUDGET_RV) CALL BUDGET (PRVS(:,:,:)*PRHODJ(:,:,:),6,'HENU_BU_RRV')
   IF (LBUDGET_RC) CALL BUDGET (PRCS(:,:,:)*PRHODJ(:,:,:),7,'HENU_BU_RRC')
   IF (LBUDGET_SV) THEN
      CALL BUDGET (PCCS(:,:,:)*PRHODJ(:,:,:),12+NSV_LIMA_NC,'HENU_BU_RSV') ! RCN
         DO JL=1, NMOD_CCN
            CALL BUDGET ( ZNFS(:,:,:,JL)*PRHODJ(:,:,:),12+NSV_LIMA_CCN_FREE+JL-1,'HENU_BU_RSV') 
         END DO
   END IF
!
END IF ! LACTI
!
!
!------------------------------------------------------------------------------
!
!*       3.    COALESCENCE PROCESSES
!              ---------------------
!
!
   CALL LIMA_WARM_COAL (PTSTEP, KMI,                                &
                        PRHODREF, ZWLBDC3, ZWLBDC, ZWLBDR3, ZWLBDR, &
                        PRCT, PRRT, PCCT, PCRT,                     &
                        PRCS, PRRS, PCCS, PCRS,                     &
                        PRHODJ                                      )
!
!
!-------------------------------------------------------------------------------
!
!        4.    EVAPORATION OF RAINDROPS
!              ------------------------
!
!
IF (ORAIN) THEN
!
   CALL LIMA_WARM_EVAP (PTSTEP, KMI,                                &
                        PRHODREF, PEXNREF, PPABST, ZT,              &
                        ZWLBDC3, ZWLBDC, ZWLBDR3, ZWLBDR,           &
                        PRVT, PRCT, PRRT, PCRT,                     &
                        PRVS, PRCS, PRRS, PCCS, PCRS, PTHS,         &
                        PEVAP3D                                     )
!
   IF (LBUDGET_RV) CALL BUDGET (PRVS(:,:,:)*PRHODJ(:,:,:),6 ,'REVA_BU_RRV')
   IF (LBUDGET_RC) CALL BUDGET (PRCS(:,:,:)*PRHODJ(:,:,:),7 ,'REVA_BU_RRC')
   IF (LBUDGET_RR) CALL BUDGET (PRRS(:,:,:)*PRHODJ(:,:,:),8 ,'REVA_BU_RRR')
   IF (LBUDGET_TH) CALL BUDGET (PTHS(:,:,:)*PRHODJ(:,:,:),4 ,'REVA_BU_RTH')
   IF (LBUDGET_SV) CALL BUDGET (PCCS(:,:,:)*PRHODJ(:,:,:),12+NSV_LIMA_NC,'REVA_BU_RSV')
   IF (LBUDGET_SV) CALL BUDGET (PCRS(:,:,:)*PRHODJ(:,:,:),12+NSV_LIMA_NR,'REVA_BU_RSV')
!
!
!-------------------------------------------------------------------------------
!
!        5.    SPONTANEOUS BREAK-UP (NUMERICAL FILTER)
!              --------------------
!
   ZWLBDR(:,:,:) = 1.E10
   WHERE (PRRS(:,:,:)>XRTMIN(3)/PTSTEP.AND.PCRS(:,:,:)>XCTMIN(3)/PTSTEP )
      ZWLBDR3(:,:,:) = XLBR * PCRS(:,:,:) / PRRS(:,:,:)
      ZWLBDR(:,:,:)  = ZWLBDR3(:,:,:)**XLBEXR
   END WHERE
   WHERE (ZWLBDR(:,:,:)<(XACCR1/XSPONBUD1))
      PCRS(:,:,:) = PCRS(:,:,:)*MAX((1.+XSPONCOEF2*(XACCR1/ZWLBDR(:,:,:)-XSPONBUD1)**2),&
                                                     (XACCR1/ZWLBDR(:,:,:)/XSPONBUD3)**3)
   END WHERE
!
! Budget storage
   IF (LBUDGET_SV) &
      CALL BUDGET (PCRS(:,:,:)*PRHODJ(:,:,:),12+NSV_LIMA_NR,&
                    &'BRKU_BU_RSV') 

!
ENDIF ! ORAIN
!
!------------------------------------------------------------------------------
!
!
!*       6.    REPORT 3D MICROPHYSICAL VARIABLES IN PRS AND PSVS
!              -------------------------------------------------
!
PRS(:,:,:,1) = PRVS(:,:,:)
IF ( KRR .GE. 2 ) PRS(:,:,:,2) = PRCS(:,:,:)
IF ( KRR .GE. 3 ) PRS(:,:,:,3) = PRRS(:,:,:)
!
! Prepare 3D number concentrations
!
IF ( LWARM ) PSVS(:,:,:,NSV_LIMA_NC) = PCCS(:,:,:)
IF ( LWARM .AND. LRAIN ) PSVS(:,:,:,NSV_LIMA_NR) = PCRS(:,:,:)
!
IF ( NMOD_CCN .GE. 1 ) THEN
   PSVS(:,:,:,NSV_LIMA_CCN_FREE:NSV_LIMA_CCN_FREE+NMOD_CCN-1) = ZNFS(:,:,:,:)
   PSVS(:,:,:,NSV_LIMA_CCN_ACTI:NSV_LIMA_CCN_ACTI+NMOD_CCN-1) = ZNAS(:,:,:,:)
END IF
!
IF (ALLOCATED(ZNFS)) DEALLOCATE(ZNFS)
IF (ALLOCATED(ZNAS)) DEALLOCATE(ZNAS)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE LIMA_WARM
