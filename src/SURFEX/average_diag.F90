!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE AVERAGE_DIAG(K2M, OT2MMW, OSURF_BUDGET, OSURF_BUDGETC, OCOEF,    &
                                OSURF_VARS, PFRAC_TILE,                           &
                                PRN_TILE, PH_TILE, PLE_TILE, PLEI_TILE ,          &
                                PGFLUX_TILE, PRI_TILE, PCD_TILE, PCH_TILE,        &
                                PCE_TILE, PT2M_TILE, PTS_TILE, PQ2M_TILE,         &
                                PHU2M_TILE, PZON10M_TILE, PMER10M_TILE,           &
                                PQS_TILE, PZ0_TILE, PZ0H_TILE,                    &
                                PSWD_TILE, PSWU_TILE, PSWBD_TILE, PSWBU_TILE,     &
                                PLWD_TILE, PLWU_TILE, PFMU_TILE, PFMV_TILE,       &
                                PRNC_TILE, PHC_TILE, PLEC_TILE, PGFLUXC_TILE,     &
                                PSWDC_TILE, PSWUC_TILE, PLWDC_TILE, PLWUC_TILE,   &
                                PFMUC_TILE, PFMVC_TILE, PT2M_MIN_TILE,            &
                                PT2M_MAX_TILE, PLEIC_TILE,                        &
                                PRN, PH, PLE, PLEI, PGFLUX, PRI, PCD, PCH, PCE,   &
                                PT2M, PTS, PQ2M, PHU2M, PZON10M, PMER10M,         &
                                PQS, PZ0, PZ0H, PUREF, PZREF,                     &
                                PSWD, PSWU, PSWBD, PSWBU,PLWD, PLWU, PFMU, PFMV,  &
                                PRNC, PHC, PLEC, PGFLUXC, PSWDC, PSWUC, PLWDC,    &
                                PLWUC, PFMUC, PFMVC, PT2M_MIN, PT2M_MAX, PLEIC,   &
                                PHU2M_MIN_TILE, PHU2M_MAX_TILE, PHU2M_MIN,        &
                                PHU2M_MAX, PWIND10M_TILE, PWIND10M_MAX_TILE,      &
                                PWIND10M, PWIND10M_MAX,                           & 
                                PEVAP_TILE, PEVAPC_TILE, PEVAP, PEVAPC,           &
                                PSUBL_TILE, PSUBLC_TILE, PSUBL, PSUBLC            )                                
!     ######################################################################
!
!
!!****  *AVERAGE_DIAG*  
!!
!!    PURPOSE
!!    -------
!      Average the fluxes from the land and water surfaces depending on the
!      fraction of each surface cover type in the mesh area.
!     
!!**  METHOD
!!    ------
!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------ 
!!
!!      
!!    REFERENCE
!!    ---------
!!      
!!    AUTHOR
!!    ------
!!    V. Masson * Meteo-France-
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    06/2003
!!      Modified    08/2009 (B. Decharme) : new diag
!     02/2010 - S. Riette - Security for wind average in case of XUNDEF values
!       B. decharme 04/2013 : Add EVAP and SUBL diag
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
INTEGER,              INTENT(IN) :: K2M          ! Flag for 2m and 10m diagnostics
LOGICAL,              INTENT(IN) :: OT2MMW       ! Flag to perform modified weighting of 2m temperature
LOGICAL,              INTENT(IN) :: OSURF_BUDGET ! Flag for surface energy budget
LOGICAL,              INTENT(IN) :: OSURF_BUDGETC! Flag for surface cumulated energy budget
LOGICAL,              INTENT(IN) :: OCOEF        ! Flag for transfer coefficients
LOGICAL,              INTENT(IN) :: OSURF_VARS
REAL, DIMENSION(:,:), INTENT(IN) :: PFRAC_TILE   ! Fraction in a mesh-area of 
!                                                ! a given surface
!* fields for each tile
REAL, DIMENSION(:,:), INTENT(IN) :: PRN_TILE      ! Net radiation       (W/m2)
REAL, DIMENSION(:,:), INTENT(IN) :: PH_TILE       ! Sensible heat flux  (W/m2)
REAL, DIMENSION(:,:), INTENT(IN) :: PLE_TILE      ! Total latent heat flux       (W/m2)
REAL, DIMENSION(:,:), INTENT(IN) :: PLEI_TILE     ! Sublimation latent heat flux (W/m2)
REAL, DIMENSION(:,:), INTENT(IN) :: PGFLUX_TILE   ! Storage flux        (W/m2)
REAL, DIMENSION(:,:), INTENT(IN) :: PEVAP_TILE    ! Total evapotranspiration  (kg/m2/s)
REAL, DIMENSION(:,:), INTENT(IN) :: PSUBL_TILE    ! Sublimation (kg/m2/s)
REAL, DIMENSION(:,:), INTENT(IN) :: PRI_TILE      ! Richardson number   (-)
REAL, DIMENSION(:,:), INTENT(IN) :: PCD_TILE      ! drag coefficient for wind (W/s2)
REAL, DIMENSION(:,:), INTENT(IN) :: PCH_TILE      ! drag coefficient for heat (W/s)
REAL, DIMENSION(:,:), INTENT(IN) :: PCE_TILE      ! drag coefficient for evaporation (W/s/K)
REAL, DIMENSION(:,:), INTENT(IN) :: PT2M_TILE     ! temperature at 2m   (K)
REAL, DIMENSION(:,:), INTENT(IN) :: PTS_TILE      ! surface temperature         (K)
REAL, DIMENSION(:,:), INTENT(IN) :: PT2M_MIN_TILE ! minimum temperature at 2m   (K)
REAL, DIMENSION(:,:), INTENT(IN) :: PT2M_MAX_TILE ! maximum temperature at 2m   (K)
REAL, DIMENSION(:,:), INTENT(IN) :: PQ2M_TILE     ! humidity at 2m      (kg/kg)
REAL, DIMENSION(:,:), INTENT(IN) :: PHU2M_TILE    ! relative humidity at 2m (-)
REAL, DIMENSION(:,:), INTENT(IN) :: PHU2M_MAX_TILE! maximum relative humidity at 2m (-)
REAL, DIMENSION(:,:), INTENT(IN) :: PHU2M_MIN_TILE! minimum relative humidity at 2m (-)
REAL, DIMENSION(:,:), INTENT(IN) :: PZON10M_TILE  ! zonal wind at 10m   (m/s)
REAL, DIMENSION(:,:), INTENT(IN) :: PMER10M_TILE  ! meridian wind at 10m(m/s)
REAL, DIMENSION(:,:), INTENT(IN) :: PWIND10M_TILE ! wind at 10m (m/s)
REAL, DIMENSION(:,:), INTENT(IN) :: PWIND10M_MAX_TILE  ! maximum wind at 10m(m/s)
REAL, DIMENSION(:,:), INTENT(IN) :: PQS_TILE
REAL, DIMENSION(:,:), INTENT(IN) :: PZ0_TILE      ! roughness lenght for momentum (m)
REAL, DIMENSION(:,:), INTENT(IN) :: PZ0H_TILE     ! roughness lenght for heat     (m)
REAL, DIMENSION(:,:), INTENT(IN) :: PSWD_TILE     ! short wave incoming radiation (W/m2)
REAL, DIMENSION(:,:), INTENT(IN) :: PSWU_TILE     ! short wave outgoing radiation (W/m2)
REAL, DIMENSION(:,:,:), INTENT(IN) :: PSWBD_TILE  ! short wave incoming radiation for each spectral band (W/m2)
REAL, DIMENSION(:,:,:), INTENT(IN) :: PSWBU_TILE  ! short wave outgoing radiation for each spectral band (W/m2)
REAL, DIMENSION(:,:), INTENT(IN) :: PLWD_TILE     ! long wave incoming radiation (W/m2)
REAL, DIMENSION(:,:), INTENT(IN) :: PLWU_TILE     ! long wave outgoing radiation (W/m2)
REAL, DIMENSION(:,:), INTENT(IN) :: PFMU_TILE     ! zonal friction
REAL, DIMENSION(:,:), INTENT(IN) :: PFMV_TILE     ! meridian friction
REAL, DIMENSION(:,:), INTENT(IN) :: PRNC_TILE      ! Net radiation       (J/m2)
REAL, DIMENSION(:,:), INTENT(IN) :: PHC_TILE       ! Sensible heat flux  (J/m2)
REAL, DIMENSION(:,:), INTENT(IN) :: PLEC_TILE      ! Total latent heat flux    (J/m2)
REAL, DIMENSION(:,:), INTENT(IN) :: PLEIC_TILE     ! Sublimation latent heat flux    (J/m2)
REAL, DIMENSION(:,:), INTENT(IN) :: PGFLUXC_TILE   ! Storage flux        (J/m2)
REAL, DIMENSION(:,:), INTENT(IN) :: PEVAPC_TILE    ! Total evapotranspiration  (kg/m2)
REAL, DIMENSION(:,:), INTENT(IN) :: PSUBLC_TILE    ! Sublimation (kg/m2)
REAL, DIMENSION(:,:), INTENT(IN) :: PSWDC_TILE     ! short wave incoming radiation (J/m2)
REAL, DIMENSION(:,:), INTENT(IN) :: PSWUC_TILE     ! short wave outgoing radiation (J/m2)
REAL, DIMENSION(:,:), INTENT(IN) :: PLWDC_TILE     ! long wave incoming radiation (J/m2)
REAL, DIMENSION(:,:), INTENT(IN) :: PLWUC_TILE     ! long wave outgoing radiation (J/m2)
REAL, DIMENSION(:,:), INTENT(IN) :: PFMUC_TILE     ! zonal friction
REAL, DIMENSION(:,:), INTENT(IN) :: PFMVC_TILE     ! meridian friction
!
REAL, DIMENSION(:), INTENT(IN) :: PUREF      ! reference height for wind  (m)
REAL, DIMENSION(:), INTENT(IN) :: PZREF      ! reference height for T,q   (m)
!
!* aggregated fields
REAL, DIMENSION(:), INTENT(OUT) :: PRN      ! Net radiation       (W/m2)
REAL, DIMENSION(:), INTENT(OUT) :: PH       ! Sensible heat flux  (W/m2)
REAL, DIMENSION(:), INTENT(OUT) :: PLE      ! Total latent heat flux    (W/m2)
REAL, DIMENSION(:), INTENT(OUT) :: PLEI     ! Sublimation latent heat flux    (W/m2)
REAL, DIMENSION(:), INTENT(OUT) :: PGFLUX   ! Storage flux        (W/m2)
REAL, DIMENSION(:), INTENT(OUT) :: PEVAP    ! Total evapotranspiration  (kg/m2/s)
REAL, DIMENSION(:), INTENT(OUT) :: PSUBL    ! Sublimation (kg/m2/s)
REAL, DIMENSION(:), INTENT(OUT) :: PRI      ! Richardson number   (-)
REAL, DIMENSION(:), INTENT(OUT) :: PCD      ! drag coefficient for wind (W/s2)
REAL, DIMENSION(:), INTENT(OUT) :: PCH      ! drag coefficient for heat (W/s)
REAL, DIMENSION(:), INTENT(OUT) :: PCE      ! drag coefficient for evaporation (W/s/K)
REAL, DIMENSION(:), INTENT(OUT) :: PT2M     ! temperature at 2m   (K)
REAL, DIMENSION(:), INTENT(OUT) :: PTS      ! surface temperature (K)
REAL, DIMENSION(:), INTENT(OUT) :: PQ2M     ! humidity at 2m      (kg/kg)
REAL, DIMENSION(:), INTENT(OUT) :: PHU2M    ! relative humidity at 2m (-)
REAL, DIMENSION(:), INTENT(OUT) :: PZON10M  ! zonal wind at 10m   (m/s)
REAL, DIMENSION(:), INTENT(OUT) :: PMER10M  ! meridian wind at 10m(m/s)
REAL, DIMENSION(:), INTENT(OUT) :: PQS
REAL, DIMENSION(:), INTENT(OUT) :: PZ0      ! roughness lenght for momentum (m)
REAL, DIMENSION(:), INTENT(OUT) :: PZ0H     ! roughness lenght for heat     (m)
REAL, DIMENSION(:), INTENT(OUT) :: PSWD     ! short wave incoming radiation (W/m2)
REAL, DIMENSION(:), INTENT(OUT) :: PSWU     ! short wave outgoing radiation (W/m2)
REAL, DIMENSION(:,:), INTENT(OUT) :: PSWBD  ! short wave incoming radiation for each spectral band (W/m2)
REAL, DIMENSION(:,:), INTENT(OUT) :: PSWBU  ! short wave outgoing radiation for each spectral band (W/m2)
REAL, DIMENSION(:), INTENT(OUT) :: PLWD     ! long wave incoming radiation (W/m2)
REAL, DIMENSION(:), INTENT(OUT) :: PLWU     ! long wave outgoing radiation (W/m2)
REAL, DIMENSION(:), INTENT(OUT) :: PFMU     ! zonal friction
REAL, DIMENSION(:), INTENT(OUT) :: PFMV     ! meridian friction
REAL, DIMENSION(:), INTENT(OUT) :: PRNC     ! Net radiation       (J/m2)
REAL, DIMENSION(:), INTENT(OUT) :: PHC      ! Sensible heat flux  (J/m2)
REAL, DIMENSION(:), INTENT(OUT) :: PLEC     ! Total latent heat flux    (J/m2)
REAL, DIMENSION(:), INTENT(OUT) :: PLEIC    ! Sublimation latent heat flux    (J/m2)
REAL, DIMENSION(:), INTENT(OUT) :: PGFLUXC  ! Storage flux        (J/m2)
REAL, DIMENSION(:), INTENT(OUT) :: PEVAPC   ! Total evapotranspiration  (kg/m2/s)
REAL, DIMENSION(:), INTENT(OUT) :: PSUBLC   ! Sublimation (kg/m2/s)
REAL, DIMENSION(:), INTENT(OUT) :: PSWDC    ! incoming short wave radiation (J/m2)
REAL, DIMENSION(:), INTENT(OUT) :: PSWUC    ! outgoing short wave radiation (J/m2)
REAL, DIMENSION(:), INTENT(OUT) :: PLWDC    ! incoming long wave radiation (J/m2)
REAL, DIMENSION(:), INTENT(OUT) :: PLWUC    ! outgoing long wave radiation (J/m2)
REAL, DIMENSION(:), INTENT(OUT) :: PFMUC    ! zonal friction
REAL, DIMENSION(:), INTENT(OUT) :: PFMVC    ! meridian friction
!
REAL, DIMENSION(:), INTENT(OUT) :: PHU2M_MIN! Minimum relative humidity at 2m (-)
REAL, DIMENSION(:), INTENT(OUT) :: PHU2M_MAX! Maximum relative humidity at 2m (-)
REAL, DIMENSION(:), INTENT(OUT) :: PT2M_MIN ! Minimum temperature at 2m   (K)
REAL, DIMENSION(:), INTENT(OUT) :: PT2M_MAX ! Maximum temperature at 2m   (K)
REAL, DIMENSION(:), INTENT(OUT) :: PWIND10M ! wind at 10m(m/s)
REAL, DIMENSION(:), INTENT(OUT) :: PWIND10M_MAX ! Maximum wind at 10m(m/s)
!
!*      0.2    declarations of local variables
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!       1.     Grid-Box average fluxes
!              -----------------------
!
IF (LHOOK) CALL DR_HOOK('AVERAGE_DIAG',0,ZHOOK_HANDLE)
!
IF (OSURF_BUDGET) THEN
!
! Net radiation
!
  CALL MAKE_AVERAGE(PFRAC_TILE,PRN_TILE,PRN)
!
! Sensible heat flux
!
  CALL MAKE_AVERAGE(PFRAC_TILE,PH_TILE,PH)
!
! Total latent heat flux
!
  CALL MAKE_AVERAGE(PFRAC_TILE,PLE_TILE,PLE)
!
! Sublimation latent heat flux
!
  CALL MAKE_AVERAGE(PFRAC_TILE,PLEI_TILE,PLEI)
!
! Total evapotranspiration
!
  CALL MAKE_AVERAGE(PFRAC_TILE,PEVAP_TILE,PEVAP)
!
! Sublimation
!
  CALL MAKE_AVERAGE(PFRAC_TILE,PSUBL_TILE,PSUBL)
!
! Storage flux
!
  CALL MAKE_AVERAGE(PFRAC_TILE,PGFLUX_TILE,PGFLUX)
!
! Downwards short wave radiation
!
  CALL MAKE_AVERAGE(PFRAC_TILE,PSWD_TILE,PSWD)
!
! Upwards short wave radiation
!
  CALL MAKE_AVERAGE(PFRAC_TILE,PSWU_TILE,PSWU)
!
! Downwards long wave radiation
!
  CALL MAKE_AVERAGE(PFRAC_TILE,PLWD_TILE,PLWD)
!
! Upwards long wave radiation
!
  CALL MAKE_AVERAGE(PFRAC_TILE,PLWU_TILE,PLWU)
!
! Zonal wind stress
!
  CALL MAKE_AVERAGE(PFRAC_TILE,PFMU_TILE,PFMU)
!
! Meridian wind stress
!
  CALL MAKE_AVERAGE(PFRAC_TILE,PFMV_TILE,PFMV)
!
! Downwards short wave radiation for each spectral band
!
  CALL MAKE_AVERAGE_2D(PFRAC_TILE,PSWBD_TILE,PSWBD)
!
! Upwards short wave radiation for each spectral band
!
  CALL MAKE_AVERAGE_2D(PFRAC_TILE,PSWBU_TILE,PSWBU)
!
END IF
!
IF (OSURF_BUDGETC) THEN
!
! Net radiation
!
  CALL MAKE_AVERAGE(PFRAC_TILE,PRNC_TILE,PRNC)
!
! Sensible heat flux
!
  CALL MAKE_AVERAGE(PFRAC_TILE,PHC_TILE,PHC)
!
! Total latent heat flux
!
  CALL MAKE_AVERAGE(PFRAC_TILE,PLEC_TILE,PLEC)
!
! Sublimation latent heat flux
!
  CALL MAKE_AVERAGE(PFRAC_TILE,PLEIC_TILE,PLEIC)
!
! Storage flux
!
  CALL MAKE_AVERAGE(PFRAC_TILE,PGFLUXC_TILE,PGFLUXC)
!
! Total evapotranspiration
!
  CALL MAKE_AVERAGE(PFRAC_TILE,PEVAPC_TILE,PEVAPC)
!
! Sublimation
!
  CALL MAKE_AVERAGE(PFRAC_TILE,PSUBLC_TILE,PSUBLC)
!
! Downwards short wave radiation
!
  CALL MAKE_AVERAGE(PFRAC_TILE,PSWDC_TILE,PSWDC)
!
! Upwards short wave radiation
!
  CALL MAKE_AVERAGE(PFRAC_TILE,PSWUC_TILE,PSWUC)
!
! Downwards long wave radiation
!
  CALL MAKE_AVERAGE(PFRAC_TILE,PLWDC_TILE,PLWDC)
!
! Upwards long wave radiation
!
  CALL MAKE_AVERAGE(PFRAC_TILE,PLWUC_TILE,PLWUC)
!
! Zonal wind stress
!
  CALL MAKE_AVERAGE(PFRAC_TILE,PFMUC_TILE,PFMUC)
!
! Meridian wind stress
!
  CALL MAKE_AVERAGE(PFRAC_TILE,PFMVC_TILE,PFMVC)
!
END IF
!
!-------------------------------------------------------------------------------
!
!       2.     Richardson number
!              -----------------
!
IF (K2M>=1) THEN
!
  CALL MAKE_AVERAGE(PFRAC_TILE,PRI_TILE,PRI)
!
ENDIF
!
!-------------------------------------------------------------------------------
!
!       3.     Operational parameters at surface, 2 and 10 meters
!              --------------------------------------------------
!
!
IF (K2M>=1.OR.OSURF_BUDGET.OR.OSURF_BUDGETC) THEN
!
! Surface temperature
!
  CALL MAKE_AVERAGE(PFRAC_TILE,PTS_TILE,PTS)
!
ENDIF
!
IF (K2M>=1) THEN
!
! Temperature at 2 meters
!
  IF (OT2MMW) THEN
! Modified weighting giving increased weight to LAND temperature
    CALL MAKE_AVERAGE_MW(PFRAC_TILE,PT2M_TILE,PT2M)
    CALL MAKE_AVERAGE_MW(PFRAC_TILE,PT2M_MIN_TILE,PT2M_MIN)
    CALL MAKE_AVERAGE_MW(PFRAC_TILE,PT2M_MAX_TILE,PT2M_MAX)
  ELSE
    CALL MAKE_AVERAGE(PFRAC_TILE,PT2M_TILE,PT2M)
    CALL MAKE_AVERAGE(PFRAC_TILE,PT2M_MIN_TILE,PT2M_MIN)
    CALL MAKE_AVERAGE(PFRAC_TILE,PT2M_MAX_TILE,PT2M_MAX)
  ENDIF
!
! Relative humidity at 2 meters
!
  CALL MAKE_AVERAGE(PFRAC_TILE,PHU2M_TILE,PHU2M)
  CALL MAKE_AVERAGE(PFRAC_TILE,PHU2M_MIN_TILE,PHU2M_MIN)
  CALL MAKE_AVERAGE(PFRAC_TILE,PHU2M_MAX_TILE,PHU2M_MAX)
!
! Specific humidity at 2 meters
!
  CALL MAKE_AVERAGE(PFRAC_TILE,PQ2M_TILE,PQ2M)
!
! Wind at 10 meters
!
  CALL MAKE_AVERAGE(PFRAC_TILE,PZON10M_TILE,PZON10M)
!
  CALL MAKE_AVERAGE(PFRAC_TILE,PMER10M_TILE,PMER10M)
!
  CALL MAKE_AVERAGE(PFRAC_TILE,PWIND10M_TILE,PWIND10M)
  CALL MAKE_AVERAGE(PFRAC_TILE,PWIND10M_MAX_TILE,PWIND10M_MAX)
!
END IF
!-------------------------------------------------------------------------------
!
!       4.     Transfer coeffients and roughness lengths
!              -----------------------------------------
!
IF (OCOEF) THEN
!
  CALL MAKE_AVERAGE(PFRAC_TILE,PCD_TILE,PCD)
!
  CALL MAKE_AVERAGE(PFRAC_TILE,PCH_TILE,PCH)
!
  CALL MAKE_AVERAGE(PFRAC_TILE,PCE_TILE,PCE)
!
  CALL MAKE_AVERAGE_Z0(PFRAC_TILE,PUREF,PZ0_TILE,PZ0)
!
  CALL MAKE_AVERAGE_Z0(PFRAC_TILE,PZREF,PZ0H_TILE,PZ0H)
!
ENDIF
!
IF (OSURF_VARS) THEN
!
  CALL MAKE_AVERAGE(PFRAC_TILE,PQS_TILE,PQS)
!
ENDIF
!
IF (LHOOK) CALL DR_HOOK('AVERAGE_DIAG',1,ZHOOK_HANDLE)
!
 CONTAINS
!
SUBROUTINE MAKE_AVERAGE(PFRAC,PFIELD_IN,PFIELD_OUT)
!
USE MODD_SURF_PAR, ONLY : XUNDEF
!
IMPLICIT NONE
!
REAL, DIMENSION(:,:),INTENT(IN)   :: PFRAC
REAL, DIMENSION(:,:),INTENT(IN)   :: PFIELD_IN
REAL, DIMENSION(:), INTENT(OUT) :: PFIELD_OUT
LOGICAL, DIMENSION(SIZE(PFIELD_IN,1)) :: GMASK
REAL(KIND=JPRB) :: ZHOOK_HANDLE
INTEGER :: JT
!
IF (LHOOK) CALL DR_HOOK('AVERAGE_DIAG:MAKE_AVERAGE',0,ZHOOK_HANDLE)
!
GMASK(:) = .TRUE.
DO JT=1,SIZE(PFIELD_IN,2)
  WHERE (PFIELD_IN(:,JT)==XUNDEF .AND. PFRAC(:,JT)/=0.) GMASK(:) = .FALSE.
END DO
!
PFIELD_OUT(:) = 0.
DO JT=1,SIZE(PFIELD_IN,2)
  PFIELD_OUT(:) = PFIELD_OUT(:) + PFRAC(:,JT) * PFIELD_IN(:,JT)
END DO
WHERE(.NOT. GMASK(:)) PFIELD_OUT(:) = XUNDEF
!
IF (LHOOK) CALL DR_HOOK('AVERAGE_DIAG:MAKE_AVERAGE',1,ZHOOK_HANDLE)
!
END SUBROUTINE MAKE_AVERAGE
!
SUBROUTINE MAKE_AVERAGE_2D(PFRAC,PFIELD_IN,PFIELD_OUT)
!
USE MODD_SURF_PAR, ONLY : XUNDEF
!
IMPLICIT NONE
!
REAL, DIMENSION(:,:),INTENT(IN)   :: PFRAC
REAL, DIMENSION(:,:,:),INTENT(IN)   :: PFIELD_IN
REAL, DIMENSION(:,:), INTENT(OUT) :: PFIELD_OUT
LOGICAL, DIMENSION(SIZE(PFIELD_IN,1)) :: GMASK
REAL(KIND=JPRB) :: ZHOOK_HANDLE
INTEGER :: JT, JL
!
IF (LHOOK) CALL DR_HOOK('AVERAGE_DIAG:MAKE_AVERAGE_2D',0,ZHOOK_HANDLE)
!
DO JL=1,SIZE(PFIELD_IN,3)
  PFIELD_OUT(:,JL) = 0.
  GMASK(:) = .TRUE.
  DO JT=1,SIZE(PFIELD_IN,2)
    WHERE (PFIELD_IN(:,JT,JL)==XUNDEF .AND. PFRAC(:,JT)/=0.) GMASK(:) = .FALSE.
    PFIELD_OUT(:,JL) = PFIELD_OUT(:,JL) + PFRAC(:,JT) * PFIELD_IN(:,JT,JL)
  END DO
  WHERE(.NOT. GMASK(:)) PFIELD_OUT(:,JL) = XUNDEF
END DO
!
IF (LHOOK) CALL DR_HOOK('AVERAGE_DIAG:MAKE_AVERAGE_2D',1,ZHOOK_HANDLE)
!
END SUBROUTINE MAKE_AVERAGE_2D
!
SUBROUTINE MAKE_AVERAGE_Z0(PFRAC,PREF,PFIELD_IN,PFIELD_OUT)
!
USE MODD_SURF_PAR, ONLY : XUNDEF
!
IMPLICIT NONE
!
REAL, DIMENSION(:,:),INTENT(IN)   :: PFRAC
REAL, DIMENSION(:,:),INTENT(IN)   :: PFIELD_IN
REAL, DIMENSION(:),INTENT(IN)   :: PREF
REAL, DIMENSION(:), INTENT(OUT) :: PFIELD_OUT
LOGICAL, DIMENSION(SIZE(PFIELD_IN,1)) :: GMASK
REAL(KIND=JPRB) :: ZHOOK_HANDLE
INTEGER :: JT, JL,JI
!
IF (LHOOK) CALL DR_HOOK('AVERAGE_DIAG:MAKE_AVERAGE_Z0',0,ZHOOK_HANDLE)
!
GMASK(:) = .TRUE.
DO JT=1,SIZE(PFIELD_IN,2)
  DO JI=1,SIZE(PFIELD_IN,1)
   IF (PFIELD_IN(JI,JT)==XUNDEF .AND. PFRAC(JI,JT)/=0.) THEN
     GMASK(:) = .FALSE.
   END IF
  END DO
END DO
!
PFIELD_OUT(:) = 0.
DO JT=1,SIZE(PFIELD_IN,2)
  PFIELD_OUT(:) = PFIELD_OUT(:) + PFRAC(:,JT) *  1./(LOG(PREF(:)/PFIELD_IN(:,JT)))**2
END DO
WHERE (PFIELD_OUT(:) == 0.)
  PFIELD_OUT(:) = XUNDEF
ELSEWHERE
  PFIELD_OUT(:) = PREF(:) * EXP( - SQRT(1./PFIELD_OUT(:)) )
ENDWHERE
WHERE(.NOT. GMASK(:)) PFIELD_OUT(:) = XUNDEF
!
IF (LHOOK) CALL DR_HOOK('AVERAGE_DIAG:MAKE_AVERAGE_Z0',1,ZHOOK_HANDLE)
!
END SUBROUTINE MAKE_AVERAGE_Z0
!
SUBROUTINE MAKE_AVERAGE_MW(PFRAC,PFIELD_IN,PFIELD_OUT)
!
USE MODD_SURF_PAR, ONLY : XUNDEF
!
IMPLICIT NONE
!
REAL, DIMENSION(:,:),INTENT(IN)   :: PFRAC
REAL, DIMENSION(:,:),INTENT(IN)   :: PFIELD_IN
REAL, DIMENSION(:),  INTENT(OUT)  :: PFIELD_OUT
LOGICAL, DIMENSION(SIZE(PFIELD_IN,1)) :: GMASK
REAL(KIND=JPRB) :: ZHOOK_HANDLE
INTEGER :: JT
REAL, DIMENSION(SIZE(PFIELD_IN,1))    :: ZT2M_LAND, ZT2M_SEA, ZFRL, ZALFA
!
IF (LHOOK) CALL DR_HOOK('AVERAGE_DIAG:MAKE_AVERAGE_MW',0,ZHOOK_HANDLE)
!
GMASK(:) = .TRUE.
DO JT=1,SIZE(PFIELD_IN,2)
  WHERE (PFIELD_IN(:,JT)==XUNDEF .AND. PFRAC(:,JT)/=0.) GMASK(:) = .FALSE.
END DO
!
ZT2M_SEA     (:)= 0.
ZT2M_LAND    (:)= 0.
ZFRL         (:)= 0.
DO JT=1,2
  ZT2M_SEA     (:) = ZT2M_SEA(:)  + PFRAC(:,JT) * PFIELD_IN(:,JT)
END DO
!
DO JT=3,4
  ZT2M_LAND    (:) = ZT2M_LAND(:) + PFRAC(:,JT) * PFIELD_IN(:,JT)
  ZFRL         (:) = ZFRL     (:) + PFRAC(:,JT)
END DO
! 
WHERE(ZFRL(:)>0.)
  ZT2M_LAND    (:) = ZT2M_LAND(:)/ZFRL(:)
ENDWHERE
WHERE(ZFRL(:)<1.)
  ZT2M_SEA     (:) = ZT2M_SEA (:)/(1.-ZFRL(:))
ENDWHERE
!
ZALFA     (:) = 1. - EXP(-10.*ZFRL(:))
PFIELD_OUT(:) = ZALFA(:) * ZT2M_LAND(:) + (1. - ZALFA(:)) * ZT2M_SEA(:)

WHERE(.NOT. GMASK(:)) PFIELD_OUT(:) = XUNDEF
!
IF (LHOOK) CALL DR_HOOK('AVERAGE_DIAG:MAKE_AVERAGE_MW',1,ZHOOK_HANDLE)
!
END SUBROUTINE MAKE_AVERAGE_MW
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE AVERAGE_DIAG
