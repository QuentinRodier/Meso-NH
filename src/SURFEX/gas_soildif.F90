!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE GAS_SOILDIF (IO, PK, PEK, KLUOUT, PTSTEP,                          &
                        PO2_ATM, PCO2_ATM, PCH4_ATM, PPO2, PPCO2, PPCH4,      &
                        PDIFFO2, PDIFFCO2, PDIFFCH4, PRO2_OXIC, PRCO2_OXIC,   &
                        PRCH4_MG, PRO2_MT, PRCO2_MT, PRCH4_MT,                &
                        PCOEF_EVAP_O2, PCOEF_EVAP_CO2, PCOEF_EVAP_CH4,        &
                        PCOEF_SURF_O2, PCOEF_SURF_CO2, PCOEF_SURF_CH4,        &
                        PCOEF_PMT_O2, PCOEF_PMT_CO2, PCOEF_PMT_CH4, PXEBU,    &
                        PVBUBBLE, PBUBBLE_OUT_CH4, PBUBBLE_IN_CH4,            &
                        PRESP_HETERO_SOIL, PFLUX_O2_SOIL, PFLUX_CH4_SOIL,     &
                        PSURF_O2, PSURF_CO2, PSURF_CH4,                       &
                        PEVAP_O2, PEVAP_CO2, PEVAP_CH4,                       &
                        PPMT_O2, PPMT_CO2, PPMT_CH4, PEBU_CH4,                &
                        PFCONS_CO2, PFPROD_CO2, PFMT_CH4, PFMG_CH4            )
!
!   ###############################################################
!!**  GAS_SOILDIF 
!!
!!    PURPOSE
!!    -------
!!    Solve gases diffusion using semi-implicit Crank–Nicolson method
!!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!    none
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!      
!!    none
!!
!!    REFERENCE
!!    ---------
!!
!!      Morel (2018) THESIS
!!      Morel et al. (2019) JAMES
!!      
!!    AUTHOR
!!    ------
!!
!!      B. Decharme           * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    23/06/20
!!
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
USE MODD_ISBA_n,         ONLY : ISBA_P_t, ISBA_PE_t
!
USE MODD_SURF_PAR, ONLY : XUNDEF
!
USE MODE_HYDRO_DIF
!
USE MODI_ABOR1_SFX
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!-------------------------------------------------------------------------------
!
!*       0.1 input
!
!
TYPE(ISBA_OPTIONS_t),   INTENT(INOUT) :: IO
TYPE(ISBA_P_t),         INTENT(INOUT) :: PK
TYPE(ISBA_PE_t),        INTENT(INOUT) :: PEK
!
!
INTEGER,              INTENT(IN)      :: KLUOUT
!
REAL,                 INTENT(IN)      :: PTSTEP                 ! time step
!
REAL, DIMENSION(:),   INTENT(IN)      :: PO2_ATM        ! O2 atmospheric concentrations for boundary conditions (g/m3 air)
REAL, DIMENSION(:),   INTENT(IN)      :: PCO2_ATM       ! CO2 atmospheric concentrations for boundary conditions (g/m3 air)
REAL, DIMENSION(:),   INTENT(IN)      :: PCH4_ATM       ! CH4 atmospheric concentrations for boundary conditions (g/m3 air)
!
REAL, DIMENSION(:,:), INTENT(IN)      :: PPO2           ! O2 total soil porosity (m3/m3)
REAL, DIMENSION(:,:), INTENT(IN)      :: PPCO2          ! CO2 total soil porosity (m3/m3)
REAL, DIMENSION(:,:), INTENT(IN)      :: PPCH4          ! CH4 total soil porosity (m3/m3)
!
REAL, DIMENSION(:,:), INTENT(IN)      :: PDIFFO2        ! Bulk soil O2 gas diffusion coefficient (m2/s)
REAL, DIMENSION(:,:), INTENT(IN)      :: PDIFFCO2       ! Bulk soil CO2 gas diffusion coefficient (m2/s)
REAL, DIMENSION(:,:), INTENT(IN)      :: PDIFFCH4       ! Bulk soil CH4 gas diffusion coefficient (m2/s)
!
REAL, DIMENSION(:,:), INTENT(IN)      :: PRO2_OXIC      ! O2 consumed during oxic decomposition (g/m2/s sol) eq.18 of M19
REAL, DIMENSION(:,:), INTENT(IN)      :: PRCO2_OXIC     ! CO2 consumed during oxic decomposition (g/m2/s sol) eq.18 of M19
REAL, DIMENSION(:,:), INTENT(IN)      :: PRCH4_MG       ! CH4 produced during methanogenesis (gCH4/m2/s)
!
REAL, DIMENSION(:,:), INTENT(IN)      :: PRO2_MT        ! O2 consumed during methanotrophy (gO2/m2/s soil)
REAL, DIMENSION(:,:), INTENT(IN)      :: PRCO2_MT       ! CO2 produced during methanotrophy (gCO2/m2/s soil)
REAL, DIMENSION(:,:), INTENT(IN)      :: PRCH4_MT       ! CH4 consumed during methanotrophy (gCH4/m2/s soil)
!
REAL, DIMENSION(:,:), INTENT(IN)      :: PCOEF_EVAP_O2  ! flux coefficient for O2 transported by evapotranspiration (s-1)
REAL, DIMENSION(:,:), INTENT(IN)      :: PCOEF_EVAP_CO2 ! flux coefficient for CO2 transported by evapotranspiration (s-1)
REAL, DIMENSION(:,:), INTENT(IN)      :: PCOEF_EVAP_CH4 ! flux coefficient for CH4 transported by evapotranspiration (s-1)
!
REAL, DIMENSION(:),   INTENT(IN)      :: PCOEF_SURF_O2  ! soil/atmosphere interface flux coefficient for O2 (s-1)
REAL, DIMENSION(:),   INTENT(IN)      :: PCOEF_SURF_CO2 ! soil/atmosphere interface flux coefficient for CO2 (s-1)
REAL, DIMENSION(:),   INTENT(IN)      :: PCOEF_SURF_CH4 ! soil/atmosphere interface flux coefficient for CH4 (s-1)
!
REAL, DIMENSION(:,:), INTENT(IN)      :: PCOEF_PMT_O2   ! PMT coefficiet for O2 (s-1)
REAL, DIMENSION(:,:), INTENT(IN)      :: PCOEF_PMT_CO2  ! PMT coefficient for CO2 (s-1)
REAL, DIMENSION(:,:), INTENT(IN)      :: PCOEF_PMT_CH4  ! PMT coefficient for CH4 (s-1)
!
REAL, DIMENSION(:,:), INTENT(IN)      :: PXEBU          ! CH4 ebullition concentration threshold (g/m3 Air)
REAL, DIMENSION(:,:), INTENT(IN)      :: PVBUBBLE       ! Ebulition bubble veliocity (m/s)
!
REAL, DIMENSION(:,:), INTENT(IN)      :: PBUBBLE_OUT_CH4 ! gCH4/m2/s soil
REAL, DIMENSION(:,:), INTENT(IN)      :: PBUBBLE_IN_CH4  ! gCH4/m2/s soil
!
!*       0.2 output
!
REAL, DIMENSION(:), INTENT(OUT)       :: PRESP_HETERO_SOIL     ! soil heterotrophic respiration (gCO2/m2/s)
REAL, DIMENSION(:), INTENT(OUT)       :: PFLUX_O2_SOIL         ! soil O2 flux (gO2/m2/s)
REAL, DIMENSION(:), INTENT(OUT)       :: PFLUX_CH4_SOIL        ! soil CH4 flux (gCH4/m2/s)
!
REAL, DIMENSION(:), INTENT(OUT)       :: PSURF_O2  ! O2 transported by diffusion at soil/atmosphere interface (gO2/m2/s)
REAL, DIMENSION(:), INTENT(OUT)       :: PSURF_CO2 ! CO2 transported by diffusion at soil/atmosphere interface (gCO2/m2/s)
REAL, DIMENSION(:), INTENT(OUT)       :: PSURF_CH4 ! CH4 transported by diffusion at soil/atmosphere interface (gCH4/m2/s)
!
REAL, DIMENSION(:), INTENT(OUT)       :: PEVAP_O2  ! O2 transported by evapotranspiration (gO2/m2/s)
REAL, DIMENSION(:), INTENT(OUT)       :: PEVAP_CO2 ! CO2 transported by evapotranspiration (gCO2/m2/s)
REAL, DIMENSION(:), INTENT(OUT)       :: PEVAP_CH4 ! CH4 transported by evapotranspiration (gCH4/m2/s)
!
REAL, DIMENSION(:), INTENT(OUT)       :: PPMT_O2   ! O2 transported by plant (gO2/m2/s)
REAL, DIMENSION(:), INTENT(OUT)       :: PPMT_CO2  ! CO2 transported by plant (gCO2/m2/s)
REAL, DIMENSION(:), INTENT(OUT)       :: PPMT_CH4  ! CH4 transported by plant (gCH4/m2/s)
!
REAL, DIMENSION(:), INTENT(OUT)       :: PEBU_CH4  ! CH4 transported by plant (gCH4/m2/s)
!
REAL, DIMENSION(:), INTENT(OUT)       :: PFCONS_CO2  ! O2 consumed during oxic decomposition and methanotrophy (gO2/m2/s)
REAL, DIMENSION(:), INTENT(OUT)       :: PFPROD_CO2 ! CO2 produced during oxic decomposition and methanotrophy (gCO2/m2/s)
!
REAL, DIMENSION(:), INTENT(OUT)       :: PFMT_CH4   ! total soil CH4 consumed during methanotrophy (gCH4/m2/s)
REAL, DIMENSION(:), INTENT(OUT)       :: PFMG_CH4   ! total soil CH4 produced during methanogenis (gCH4/m2/s)
!
!-------------------------------------------------------------------------------
!
!*       0.3 local parameters
!
REAL, PARAMETER                            :: ZBUDLIM = 1.0E-15
!
REAL, PARAMETER                            :: ZWGHT = 0.5  ! time scheme weight for calculating flux.
!                                                            varies from 0 (explicit time scheme)
!                                                            to 1 (backward difference implicit)
!                                                            Default is 1/2 (Crank-Nicholson)
!
!-------------------------------------------------------------------------------
!
!*       0.4 local variables
!
REAL                                             :: ZDT, ZSPIN, ZWORK1, ZWORK2
!
REAL, DIMENSION(SIZE(PK%XDZG,1),SIZE(PK%XDZG,2)) :: ZALPHA, ZBETA, ZGAMMA, ZK, ZSOURCE, ZKLIM
!
REAL, DIMENSION(SIZE(PK%XDZG,1),SIZE(PK%XDZG,2)) :: ZFRCV, ZAMTRX, ZBMTRX, ZCMTRX
!
REAL, DIMENSION(SIZE(PK%XDZG,1),SIZE(PK%XDZG,2)) :: ZSGASO2, ZSGASCO2, ZSGASCH4
!
REAL, DIMENSION(SIZE(PK%XDZG,1),SIZE(PK%XDZG,2)) :: ZCOEF_EBU_CH4, ZEBUDIAG
!
REAL, DIMENSION(SIZE(PK%XDZG,1))                 :: ZDO2, ZDCO2, ZDCH4,          &
                                                    ZBUD_O2, ZBUD_CO2, ZBUD_CH4, &
                                                    ZEBU_OUT, ZEBU_IN
!
! dimensions
INTEGER         :: INI, INL, IDEPTH
!
! indices
INTEGER         :: JI, JL
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('GAS_SOILDIF',0,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!*       1. Initialisations
!-------------------------------------------------------------------------------
!
!*       1.1 dimensions
!
INI = SIZE(PK%XDZG,1)
INL = SIZE(PK%XDZG,2)
!
ZSPIN = REAL(IO%NSPINS)
!
!ZDT = ZSPIN*PTSTEP
ZDT = PTSTEP
!
!*       1.2 set output to zero
!
PRESP_HETERO_SOIL(:) = 0.0
!
PSURF_O2(:) = 0.0
PEVAP_O2(:) = 0.0
PPMT_O2 (:) = 0.0
!
PSURF_CO2(:) = 0.0
PEVAP_CO2(:) = 0.0
PPMT_CO2 (:) = 0.0
!
PSURF_CH4(:) = 0.0
PEVAP_CH4(:) = 0.0
PPMT_CH4 (:) = 0.0
!
PEBU_CH4(:) = 0.0
!
PFCONS_CO2 (:) = 0.0
PFPROD_CO2(:) = 0.0
PFMT_CH4  (:) = 0.0
PFMG_CH4  (:) = 0.0
!
!*       1.3 Initialize local variables
!
ZSGASO2 (:,:) = PEK%XSGASO2(:,:)
ZSGASCO2(:,:) = PEK%XSGASCO2(:,:)
ZSGASCH4(:,:) = PEK%XSGASCH4(:,:)
!
!*       1.4 Initialize gas conductivity security (m.s-1)
!
ZKLIM(:,:) = PK%XDZDIF(:,:)/ZDT
!
!-------------------------------------------------------------------------------
!*       2. O2 diffusion
!-------------------------------------------------------------------------------
!
!*       2.1 set local variables to zero
!
ZK     (:,:) = 0.0
ZSOURCE(:,:) = 0.0
!
ZALPHA(:,:) = 0.0
ZBETA (:,:) = 0.0
ZGAMMA(:,:) = 0.0
!
ZAMTRX(:,:) = 0.0
ZBMTRX(:,:) = 0.0
ZCMTRX(:,:) = 0.0
ZFRCV (:,:) = 0.0
!
!*       2.2 Prepare solver
!
! gas conductivity (m/s) using interfacial harmonic mean
!
DO JL=1,INL
   DO JI=1,INI
      IDEPTH=PK%NWG_LAYER(JI)
      IF(JL<IDEPTH)THEN
        ZWORK1 = PK%XDZG(JI,JL  )/(2.0*PK%XDZDIF(JI,JL)*PDIFFO2(JI,JL  ))
        ZWORK2 = PK%XDZG(JI,JL+1)/(2.0*PK%XDZDIF(JI,JL)*PDIFFO2(JI,JL+1))
        ZK(JI,JL) = MIN(1.0/(PK%XDZDIF(JI,JL)*(ZWORK1+ZWORK2)),ZKLIM(JI,JL))
        ZK(JI,JL) = ZK(JI,JL)*PPO2(JI,JL)
      ENDIF
   ENDDO
ENDDO
!
! Source/Sink term (gO2/m3)
!
DO JL=1,INL
   DO JI=1,INI
      IDEPTH=PK%NWG_LAYER(JI)
      IF(JL<=IDEPTH)THEN
        ZSOURCE(JI,JL) = PCOEF_PMT_O2(JI,JL)*PO2_ATM(JI)-(PRO2_OXIC(JI,JL)+PRO2_MT(JI,JL))/PK%XDZG(JI,JL)
      ENDIF
   ENDDO
ENDDO
!
!*       2.2 Upper BC : surface layer (Soil/atmosphere exchange flux implicited)
!
ZALPHA(:,1) = 0.0
ZBETA (:,1) = ZK(:,1)/PK%XDZG(:,1)
ZGAMMA(:,1) = ZBETA(:,1)+PCOEF_PMT_O2(:,1)+PCOEF_EVAP_O2(:,1)
!
ZAMTRX(:,1) = 0.0
ZBMTRX(:,1) = PPO2(:,1)/ZDT+PCOEF_SURF_O2(:)+ZWGHT*ZGAMMA(:,1)
ZCMTRX(:,1) = -ZWGHT*ZBETA(:,1)
ZFRCV (:,1) = PO2_ATM(:)*PCOEF_SURF_O2(:)       &
            + PEK%XSGASO2(:,1)*(PPO2(:,1)/ZDT-(1.0-ZWGHT)*ZGAMMA(:,1)) &
            + PEK%XSGASO2(:,2)*(1.0-ZWGHT)*ZBETA(:,1)
!
!*       2.3 Interior Grid : Other sub-surface layers
!
DO JL=2,INL-1
   DO JI=1,INI
      IDEPTH=PK%NWG_LAYER(JI)
      IF(JL<IDEPTH)THEN
        ZALPHA(JI,JL) = ZK(JI,JL-1)/PK%XDZG(JI,JL)
        ZBETA (JI,JL) = ZK(JI,JL  )/PK%XDZG(JI,JL)
        ZGAMMA(JI,JL) = ZALPHA(JI,JL)+ZBETA(JI,JL)+PCOEF_PMT_O2(JI,JL)+PCOEF_EVAP_O2(JI,JL)

        ZAMTRX(JI,JL) = -ZWGHT*ZALPHA(JI,JL)
        ZBMTRX(JI,JL) = PPO2(JI,JL)/ZDT+ZWGHT*ZGAMMA(JI,JL)
        ZCMTRX(JI,JL) = -ZWGHT*ZBETA(JI,JL)
        ZFRCV (JI,JL) = PEK%XSGASO2(JI,JL-1)*(1.0-ZWGHT)*ZALPHA(JI,JL)       &
                      + PEK%XSGASO2(JI,JL  )*(PPO2(JI,JL)/ZDT-(1.0-ZWGHT)*ZGAMMA(JI,JL)) &
                      + PEK%XSGASO2(JI,JL+1)*(1.0-ZWGHT)*ZBETA (JI,JL)
      ENDIF
   ENDDO
ENDDO
!
!*       2.4 Lower BC : last layer
!
DO JI=1,INI
   IDEPTH=PK%NWG_LAYER(JI)
   ZALPHA(JI,IDEPTH) = ZK(JI,IDEPTH-1)/PK%XDZG(JI,IDEPTH)
   ZBETA (JI,IDEPTH) = 0.0
   ZGAMMA(JI,IDEPTH) = ZALPHA(JI,IDEPTH)+PCOEF_PMT_O2(JI,IDEPTH)+PCOEF_EVAP_O2(JI,IDEPTH)

   ZAMTRX(JI,IDEPTH) = -ZWGHT*ZALPHA(JI,IDEPTH)
   ZBMTRX(JI,IDEPTH) = PPO2(JI,IDEPTH)/ZDT+ZWGHT*ZGAMMA(JI,IDEPTH)
   ZCMTRX(JI,IDEPTH) = 0.0
   ZFRCV (JI,IDEPTH) = PEK%XSGASO2(JI,IDEPTH-1)*(1.0-ZWGHT)*ZALPHA(JI,IDEPTH)       &
                     + PEK%XSGASO2(JI,IDEPTH  )*(PPO2(JI,IDEPTH)/ZDT-(1.0-ZWGHT)*ZGAMMA(JI,IDEPTH))
ENDDO
!
!*       2.5 Solver
!
ZFRCV (:,:) = ZFRCV(:,:)+ZSOURCE(:,:)
!
CALL TRIDIAG_DIF(ZAMTRX(:,:),ZBMTRX(:,:),ZCMTRX(:,:),ZFRCV(:,:),PK%NWG_LAYER(:),INL,PEK%XSGASO2(:,:))
!
!*       2.5 Final fluxes computation (gO2.m-2.s-1)
!
PSURF_O2(:) = PCOEF_SURF_O2(:)*(PEK%XSGASO2(:,1)-PO2_ATM(:))*PK%XDZG(:,1)
!
PEVAP_O2 (:) = 0.0
PPMT_O2  (:) = 0.0
PFCONS_CO2(:) = 0.0
ZDO2     (:) = 0.0
!
JL=0
DO JL=1,INL
   DO JI=1,INI
      IDEPTH=PK%NWG_LAYER(JI)
      IF(JL<=IDEPTH)THEN
!
        ZDO2(JI) = ZDO2(JI) + (PEK%XSGASO2(JI,JL)-ZSGASO2(JI,JL))*PPO2(JI,JL)*PK%XDZG(JI,JL)/ZDT
!
        PFCONS_CO2(JI) = PFCONS_CO2(JI) + (PRO2_OXIC(JI,JL)+PRO2_MT(JI,JL))
!
        PEVAP_O2(JI) = PEVAP_O2(JI) +      ZWGHT *PCOEF_EVAP_O2(JI,JL)*PEK%XSGASO2(JI,JL)*PK%XDZG(JI,JL) &
                                    + (1.0-ZWGHT)*PCOEF_EVAP_O2(JI,JL)*ZSGASO2(JI,JL)*PK%XDZG(JI,JL) 
!
        PPMT_O2(JI) = PPMT_O2(JI) +      ZWGHT *PCOEF_PMT_O2(JI,JL)*(PEK%XSGASO2(JI,JL)-PO2_ATM(JI))*PK%XDZG(JI,JL) &
                                  + (1.0-ZWGHT)*PCOEF_PMT_O2(JI,JL)*(ZSGASO2(JI,JL)-PO2_ATM(JI))*PK%XDZG(JI,JL)
!
      ENDIF
   ENDDO
ENDDO
!
PFLUX_O2_SOIL(:) = PSURF_O2(:) + PEVAP_O2(:) + PPMT_O2(:)
!
ZBUD_O2(:) = ZDO2(:) + PFCONS_CO2(:) + PFLUX_O2_SOIL(:)
!
DO JI=1,INI
   IF(ABS(ZBUD_O2(JI))>ZBUDLIM)THEN
      WRITE(KLUOUT,*) '!!!!!!!!!!!!!!!!!!!!'
      WRITE(KLUOUT,*) 'Problem O2 diffusion'
      WRITE(KLUOUT,*) 'Bilan = ',ZBUD_O2(JI)
      WRITE(KLUOUT,*) 'Grid-cell = ',JI
      WRITE(KLUOUT,*) '!!!!!!!!!!!!!!!!!!!!'
      CALL ABOR1_SFX('GAS_SOILDIF: PROBLEM O2 DIFFUSION')
   ENDIF
ENDDO
!
!-------------------------------------------------------------------------------
!*       3. CO2 diffusion
!-------------------------------------------------------------------------------
!
!*       3.1 set local variables to zero
!
ZK     (:,:) = 0.0
ZSOURCE(:,:) = 0.0
!
ZALPHA(:,:) = 0.0
ZBETA (:,:) = 0.0
ZGAMMA(:,:) = 0.0
!
ZAMTRX(:,:) = 0.0
ZBMTRX(:,:) = 0.0
ZCMTRX(:,:) = 0.0
ZFRCV (:,:) = 0.0
!
!*       3.2 Prepare solver
!
! gas conductivity (m/s) using interfacial harmonic mean
!
DO JL=1,INL
   DO JI=1,INI
      IDEPTH=PK%NWG_LAYER(JI)
      IF(JL<IDEPTH)THEN
        ZWORK1 = PK%XDZG(JI,JL  )/(2.0*PK%XDZDIF(JI,JL)*PDIFFCO2(JI,JL  ))
        ZWORK2 = PK%XDZG(JI,JL+1)/(2.0*PK%XDZDIF(JI,JL)*PDIFFCO2(JI,JL+1))
        ZK(JI,JL) = MIN(1.0/(PK%XDZDIF(JI,JL)*(ZWORK1+ZWORK2)),ZKLIM(JI,JL))
        ZK(JI,JL) = ZK(JI,JL)*PPCO2(JI,JL)        
      ENDIF
   ENDDO
ENDDO
!
! Source/Sink term (gCO2/m3)
!
DO JL=1,INL
   DO JI=1,INI
      IDEPTH=PK%NWG_LAYER(JI)
      IF(JL<=IDEPTH)THEN
        ZSOURCE(JI,JL) = PCOEF_PMT_CO2(JI,JL)*PCO2_ATM(JI)+(PRCO2_OXIC(JI,JL)+PRCO2_MT(JI,JL))/PK%XDZG(JI,JL)
      ENDIF
   ENDDO
ENDDO
!
!*       3.2 Upper BC : surface layer (Soil/atmosphere exchange flux implicited)
!
ZALPHA(:,1) = 0.0
ZBETA (:,1) = ZK(:,1)/PK%XDZG(:,1)
ZGAMMA(:,1) = ZBETA(:,1)+(PCOEF_PMT_CO2(:,1)+PCOEF_EVAP_CO2(:,1))
!
ZAMTRX(:,1) = 0.0
ZBMTRX(:,1) = PPCO2(:,1)/ZDT+PCOEF_SURF_CO2(:)+ZWGHT*ZGAMMA(:,1)
ZCMTRX(:,1) = -ZWGHT*ZBETA(:,1)
ZFRCV (:,1) = PCO2_ATM(:)*PCOEF_SURF_CO2(:)       &
            + PEK%XSGASCO2(:,1)*(PPCO2(:,1)/ZDT-(1.0-ZWGHT)*ZGAMMA(:,1)) &
            + PEK%XSGASCO2(:,2)*(1.0-ZWGHT)*ZBETA(:,1)
!
!*       3.3 Interior Grid : Other sub-surface layers
!
DO JL=2,INL-1
   DO JI=1,INI
      IDEPTH=PK%NWG_LAYER(JI)
      IF(JL<IDEPTH)THEN
        ZALPHA(JI,JL) = ZK(JI,JL-1)/PK%XDZG(JI,JL)
        ZBETA (JI,JL) = ZK(JI,JL  )/PK%XDZG(JI,JL)
        ZGAMMA(JI,JL) = ZALPHA(JI,JL)+ZBETA(JI,JL)+PCOEF_PMT_CO2(JI,JL)+PCOEF_EVAP_CO2(JI,JL)
        ZAMTRX(JI,JL) = -ZWGHT*ZALPHA(JI,JL)
        ZBMTRX(JI,JL) = PPCO2(JI,JL)/ZDT+ZWGHT*ZGAMMA(JI,JL)
        ZCMTRX(JI,JL) = -ZWGHT*ZBETA (JI,JL)
        ZFRCV (JI,JL) = PEK%XSGASCO2(JI,JL-1)*(1.0-ZWGHT)*ZALPHA(JI,JL)       &
                      + PEK%XSGASCO2(JI,JL  )*(PPCO2(JI,JL)/ZDT-(1.0-ZWGHT)*ZGAMMA(JI,JL)) &
                      + PEK%XSGASCO2(JI,JL+1)*(1.0-ZWGHT)*ZBETA (JI,JL)

      ENDIF
   ENDDO
ENDDO
!*       3.4 Lower BC : last layer
!
DO JI=1,INI
   IDEPTH=PK%NWG_LAYER(JI)
   ZALPHA(JI,IDEPTH) = ZK(JI,IDEPTH-1)/PK%XDZG(JI,IDEPTH)
   ZBETA (JI,IDEPTH) = 0.0
   ZGAMMA(JI,IDEPTH) = ZALPHA(JI,IDEPTH)+PCOEF_PMT_CO2(JI,IDEPTH)+PCOEF_EVAP_CO2(JI,IDEPTH)

   ZAMTRX(JI,IDEPTH) = -ZWGHT*ZALPHA(JI,IDEPTH)
   ZBMTRX(JI,IDEPTH) = PPCO2(JI,IDEPTH)/ZDT+ZWGHT*ZGAMMA(JI,IDEPTH)
   ZCMTRX(JI,IDEPTH) = 0.0
   ZFRCV (JI,IDEPTH) = PEK%XSGASCO2(JI,IDEPTH-1)*(1.0-ZWGHT)*ZALPHA(JI,IDEPTH)       &
                     + PEK%XSGASCO2(JI,IDEPTH  )*(PPCO2(JI,IDEPTH)/ZDT-(1.0-ZWGHT)*ZGAMMA(JI,IDEPTH))
ENDDO
!
!*       3.5 Solver
!
ZFRCV (:,:) = ZFRCV(:,:)+ZSOURCE(:,:)
!
CALL TRIDIAG_DIF(ZAMTRX(:,:),ZBMTRX(:,:),ZCMTRX(:,:),ZFRCV(:,:),PK%NWG_LAYER(:),INL,PEK%XSGASCO2(:,:))
!
!*       2.5 Final fluxes computation (gCO2.m-2.s-1)
!
PSURF_CO2(:) = PCOEF_SURF_CO2(:)*(PEK%XSGASCO2(:,1)-PCO2_ATM(:))*PK%XDZG(:,1)
!
PEVAP_CO2 (:) = 0.0
PPMT_CO2  (:) = 0.0
PFPROD_CO2(:) = 0.0
ZDCO2     (:) = 0.0
!
DO JL=1,INL
   DO JI=1,INI
      IDEPTH=PK%NWG_LAYER(JI)
      IF(JL<=IDEPTH)THEN
!
        ZDCO2(JI) = ZDCO2(JI) + (PEK%XSGASCO2(JI,JL)-ZSGASCO2(JI,JL))*PPCO2(JI,JL)*PK%XDZG(JI,JL)/ZDT
!
        PFPROD_CO2(JI) = PFPROD_CO2(JI) + (PRCO2_OXIC(JI,JL)+PRCO2_MT(JI,JL))
!
        PEVAP_CO2(JI) = PEVAP_CO2(JI) +      ZWGHT *PCOEF_EVAP_CO2(JI,JL)*PEK%XSGASCO2(JI,JL)*PK%XDZG(JI,JL) &
                                      + (1.0-ZWGHT)*PCOEF_EVAP_CO2(JI,JL)*ZSGASCO2(JI,JL)*PK%XDZG(JI,JL) 
!
        PPMT_CO2(JI) = PPMT_CO2(JI) +      ZWGHT *PCOEF_PMT_CO2(JI,JL)*(PEK%XSGASCO2(JI,JL)-PCO2_ATM(JI))*PK%XDZG(JI,JL) &
                                    + (1.0-ZWGHT)*PCOEF_PMT_CO2(JI,JL)*(ZSGASCO2(JI,JL)-PCO2_ATM(JI))*PK%XDZG(JI,JL)
!
      ENDIF
   ENDDO
ENDDO
!
PRESP_HETERO_SOIL(:) = PSURF_CO2(:) + PEVAP_CO2(:) + PPMT_CO2(:)
!
ZBUD_CO2(:) = ZDCO2(:) - PFPROD_CO2(:) + PRESP_HETERO_SOIL(:)
!
DO JI=1,INI
   IF(ABS(ZBUD_CO2(JI))>ZBUDLIM)THEN
      WRITE(KLUOUT,*) '!!!!!!!!!!!!!!!!!!!!'
      WRITE(KLUOUT,*) 'Problem CO2 diffusion'
      WRITE(KLUOUT,*) 'Bilan = ',ZBUD_CO2(JI)
      WRITE(KLUOUT,*) 'Grid-cell = ',JI
      WRITE(KLUOUT,*) '!!!!!!!!!!!!!!!!!!!!'
      CALL ABOR1_SFX('GAS_SOILDIF: PROBLEM CO2 DIFFUSION')
   ENDIF
ENDDO
!
!-------------------------------------------------------------------------------
!*       4. CH4 diffusion
!-------------------------------------------------------------------------------
!
!*       4.1 set local variables to zero
!
ZK     (:,:) = 0.0
ZSOURCE(:,:) = 0.0
!
ZALPHA(:,:) = 0.0
ZBETA (:,:) = 0.0
ZGAMMA(:,:) = 0.0
!
ZAMTRX(:,:) = 0.0
ZBMTRX(:,:) = 0.0
ZCMTRX(:,:) = 0.0
ZFRCV (:,:) = 0.0
!
ZCOEF_EBU_CH4(:,:) = 0.0
!
!*       4.2 Ebulition as advection (if direct, PVBUBBLE=0.0)
!
WHERE(PEK%XSGASCH4(:,:)>PXEBU(:,:))
      ZCOEF_EBU_CH4(:,:) = PPCH4(:,:)*MIN(PVBUBBLE(:,:),ZKLIM(:,:))
ENDWHERE
!
!*       4.3 Prepare solver
!
! gas conductivity (m/s) using interfacial harmonic mean
!
DO JL=1,INL
   DO JI=1,INI
      IDEPTH=PK%NWG_LAYER(JI)
      IF(JL<IDEPTH)THEN
        ZWORK1 = PK%XDZG(JI,JL  )/(2.0*PK%XDZDIF(JI,JL)*PDIFFCH4(JI,JL  ))
        ZWORK2 = PK%XDZG(JI,JL+1)/(2.0*PK%XDZDIF(JI,JL)*PDIFFCH4(JI,JL+1))
        ZK(JI,JL) = MIN(1.0/(PK%XDZDIF(JI,JL)*(ZWORK1+ZWORK2)),ZKLIM(JI,JL))
        ZK(JI,JL) = ZK(JI,JL)*PPCH4(JI,JL)       
      ENDIF
   ENDDO
ENDDO
!
! Source/Sink term (gCH4/m3)
!
DO JL=1,INL
   DO JI=1,INI
      IDEPTH=PK%NWG_LAYER(JI)
      IF(JL<=IDEPTH)THEN
        ZSOURCE(JI,JL) = PCOEF_PMT_CH4(JI,JL)*PCH4_ATM(JI)             &
                       + (PRCH4_MG(JI,JL)-PRCH4_MT(JI,JL))/PK%XDZG(JI,JL) &
                       + (PBUBBLE_IN_CH4(JI,JL)-PBUBBLE_OUT_CH4(JI,JL))/PK%XDZG(JI,JL)
      ENDIF
   ENDDO
ENDDO
!
!*       4.4 Upper BC : surface layer (Soil/atmosphere exchange flux implicited)
!
ZALPHA(:,1) = 0.0
ZBETA (:,1) = ZK(:,1)/PK%XDZG(:,1)
ZGAMMA(:,1) = ZBETA(:,1)+PCOEF_PMT_CH4(:,1)+PCOEF_EVAP_CH4(:,1)+ZCOEF_EBU_CH4(:,1)/PK%XDZG(:,1)
!
ZAMTRX(:,1) = 0.0
ZBMTRX(:,1) = PPCH4(:,1)/ZDT+PCOEF_SURF_CH4(:)+ZWGHT*ZGAMMA(:,1)
ZCMTRX(:,1) = -ZWGHT*(ZBETA(:,1)+ZCOEF_EBU_CH4(:,2)/PK%XDZG(:,1))
ZFRCV (:,1) = PCH4_ATM(:)*PCOEF_SURF_CH4(:)       &
            + PEK%XSGASCH4(:,1)*(PPCH4(:,1)/ZDT-(1.0-ZWGHT)*ZGAMMA(:,1)) &
            + PEK%XSGASCH4(:,2)*(1.0-ZWGHT)*(ZBETA(:,1)+ZCOEF_EBU_CH4(:,2)/PK%XDZG(:,1)) &
            + PXEBU(:,1)*ZCOEF_EBU_CH4(:,1)/PK%XDZG(:,1) &
            - PXEBU(:,2)*ZCOEF_EBU_CH4(:,2)/PK%XDZG(:,1)
!
!*       4.5 Interior Grid : Other sub-surface layers
!
DO JL=2,INL-1
   DO JI=1,INI
      IDEPTH=PK%NWG_LAYER(JI)
      IF(JL<IDEPTH)THEN
!              
        ZALPHA(JI,JL) = ZK(JI,JL-1)/PK%XDZG(JI,JL)
        ZBETA (JI,JL) = ZK(JI,JL  )/PK%XDZG(JI,JL)
        ZGAMMA(JI,JL) = ZALPHA(JI,JL)+ZBETA(JI,JL)+PCOEF_PMT_CH4(JI,JL)+PCOEF_EVAP_CH4(JI,JL) &
                                                  +ZCOEF_EBU_CH4(JI,JL)/PK%XDZG(JI,JL)
!
        ZAMTRX(JI,JL) = -ZWGHT*ZALPHA(JI,JL)
        ZBMTRX(JI,JL) = PPCH4(JI,JL)/ZDT+ZWGHT*ZGAMMA(JI,JL)
        ZCMTRX(JI,JL) = -ZWGHT*(ZBETA(JI,JL)+ZCOEF_EBU_CH4(JI,JL+1)/PK%XDZG(JI,JL))
        ZFRCV (JI,JL) = PEK%XSGASCH4(JI,JL-1)*(1.0-ZWGHT)*ZALPHA(JI,JL)       &
                      + PEK%XSGASCH4(JI,JL  )*(PPCH4(JI,JL)/ZDT-(1.0-ZWGHT)*ZGAMMA(JI,JL)) &
                      + PEK%XSGASCH4(JI,JL+1)*(1.0-ZWGHT)*(ZBETA(JI,JL)+ZCOEF_EBU_CH4(JI,JL+1)/PK%XDZG(JI,JL)) &
                      + PXEBU(JI,JL)*ZCOEF_EBU_CH4(JI,JL)/PK%XDZG(JI,JL) &
                      - PXEBU(JI,JL+1)*ZCOEF_EBU_CH4(JI,JL+1)/PK%XDZG(JI,JL)
!
      ENDIF
   ENDDO
ENDDO
!*       4.6 Lower BC : last layer
!
DO JI=1,INI
   IDEPTH=PK%NWG_LAYER(JI)
   ZALPHA(JI,IDEPTH) = ZK(JI,IDEPTH-1)/PK%XDZG(JI,IDEPTH)
   ZBETA (JI,IDEPTH) = 0.0
   ZGAMMA(JI,IDEPTH) = ZALPHA(JI,IDEPTH)+PCOEF_PMT_CH4(JI,IDEPTH)+PCOEF_EVAP_CH4(JI,IDEPTH) &
                                        +ZCOEF_EBU_CH4(JI,IDEPTH)/PK%XDZG(JI,IDEPTH)

   ZAMTRX(JI,IDEPTH) = -ZWGHT*ZALPHA(JI,IDEPTH)
   ZBMTRX(JI,IDEPTH) = PPCH4(JI,IDEPTH)/ZDT+ZWGHT*ZGAMMA(JI,IDEPTH)
   ZCMTRX(JI,IDEPTH) = 0.0
   ZFRCV (JI,IDEPTH) = PEK%XSGASCH4(JI,IDEPTH-1)*(1.0-ZWGHT)*ZALPHA(JI,IDEPTH)       &
                     + PEK%XSGASCH4(JI,IDEPTH  )*(PPCH4(JI,IDEPTH)/ZDT-(1.0-ZWGHT)*ZGAMMA(JI,IDEPTH)) &
                     +PXEBU(JI,IDEPTH)*ZCOEF_EBU_CH4(JI,IDEPTH)/PK%XDZG(JI,IDEPTH)
ENDDO
!
!*       4.7 Solver
!
ZFRCV (:,:) = ZFRCV(:,:)+ZSOURCE(:,:)
!
CALL TRIDIAG_DIF(ZAMTRX(:,:),ZBMTRX(:,:),ZCMTRX(:,:),ZFRCV(:,:),PK%NWG_LAYER(:),INL,PEK%XSGASCH4(:,:))
!
!*       4.8 Final fluxes computation (gCH4.m-2.s-1)
!
PSURF_CH4(:) = PCOEF_SURF_CH4(:)*(PEK%XSGASCH4(:,1)-PCH4_ATM(:))*PK%XDZG(:,1)
!
PEVAP_CH4  (:) = 0.0
PPMT_CH4   (:) = 0.0
PFMT_CH4   (:) = 0.0
PFMG_CH4   (:) = 0.0
ZDCH4      (:) = 0.0
!
ZEBUDIAG(:,:) = 0.0
ZEBU_OUT(:) = 0.0
ZEBU_IN (:) = 0.0
!
DO JL=1,INL
   DO JI=1,INI
      IDEPTH=PK%NWG_LAYER(JI)
      IF(JL<=IDEPTH)THEN
!
        ZDCH4(JI) = ZDCH4(JI) + (PEK%XSGASCH4(JI,JL)-ZSGASCH4(JI,JL))*PPCH4(JI,JL)*PK%XDZG(JI,JL)/ZDT
!
        PFMG_CH4(JI) = PFMG_CH4(JI) + PRCH4_MG(JI,JL)
!        
        PFMT_CH4(JI) = PFMT_CH4(JI) + PRCH4_MT(JI,JL)
!
        PEVAP_CH4(JI) = PEVAP_CH4(JI) +      ZWGHT *PCOEF_EVAP_CH4(JI,JL)*PEK%XSGASCH4(JI,JL)*PK%XDZG(JI,JL) &
                                      + (1.0-ZWGHT)*PCOEF_EVAP_CH4(JI,JL)*ZSGASCH4(JI,JL)*PK%XDZG(JI,JL) 
!
        PPMT_CH4(JI) = PPMT_CH4(JI) +      ZWGHT *PCOEF_PMT_CH4(JI,JL)*(PEK%XSGASCH4(JI,JL)-PCH4_ATM(JI))*PK%XDZG(JI,JL) &
                                    + (1.0-ZWGHT)*PCOEF_PMT_CH4(JI,JL)*(ZSGASCH4(JI,JL)-PCH4_ATM(JI))*PK%XDZG(JI,JL)
!
        ZEBUDIAG(JI,JL) =      ZWGHT * ZCOEF_EBU_CH4(JI,JL)*(PEK%XSGASCH4(JI,JL)-PXEBU(JI,JL)) &
                        + (1.0-ZWGHT)* ZCOEF_EBU_CH4(JI,JL)*(ZSGASCH4(JI,JL)-PXEBU(JI,JL))
!
        ZEBU_OUT(JI) = ZEBU_OUT(JI) + PBUBBLE_OUT_CH4(JI,JL)
        ZEBU_IN (JI) = ZEBU_IN (JI) + PBUBBLE_IN_CH4 (JI,JL)
!
      ENDIF
   ENDDO
ENDDO
!
PEBU_CH4(:) = (ZEBU_OUT(:)-ZEBU_IN(:)) + ZEBUDIAG(:,1)
!
PFLUX_CH4_SOIL(:) = PSURF_CH4(:) + PEVAP_CH4(:) + PPMT_CH4(:) + PEBU_CH4(:)
!
ZBUD_CH4(:) = ZDCH4(:) - PFMG_CH4(:) + PFMT_CH4(:) + PFLUX_CH4_SOIL(:)
!
DO JI=1,INI
   IF(ABS(ZBUD_CH4(JI))>ZBUDLIM)THEN
      WRITE(KLUOUT,*) '!!!!!!!!!!!!!!!!!!!!'
      WRITE(KLUOUT,*) 'Problem CH4 diffusion'
      WRITE(KLUOUT,*) 'Bilan = ',ZBUD_CH4(JI)
      WRITE(KLUOUT,*) 'Grid-cell = ',JI
      WRITE(KLUOUT,*) '!!!!!!!!!!!!!!!!!!!!'
      CALL ABOR1_SFX('GAS_SOILDIF: PROBLEM CH4 DIFFUSION')
   ENDIF
ENDDO
!
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('GAS_SOILDIF',1,ZHOOK_HANDLE)
!
END SUBROUTINE GAS_SOILDIF
