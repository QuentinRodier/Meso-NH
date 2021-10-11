!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
    SUBROUTINE URBAN_SNOW_EVOL(TOP, T, B, DMT, CT, LW, HPROGRAM, PT_LWCN, PQ_LWCN, PU_LWCN, PTS_RF, PTS_RD, PTS_WL_A,   &
                               PTS_WL_B, PPS, PTA, PQA, PRHOA, PLW_RAD, PSR, PZREF, PUREF,  &
                               PVMOD, PTSTEP, PZ_LWCN, PDN_RF, PABS_SW_SN_RF, PABS_LW_SN_RF,&
                               PDN_RD, PABS_SW_SN_RD, PABS_LW_SN_RD, PRNSN_RF, PHSN_RF,     &
                               PLESN_RF, PGSN_RF, PMELT_RF, PRNSN_RD, PHSN_RD, PLESN_RD,    &
                               PGSN_RD, PMELT_RD,  &
                               PTS_HVEG,   &
                               PEMIT_LW_SN_RD,PEMIT_LW_SN_RF,                               &
                               PSNOW_HEAT_ROAD, PSNOW_HEAT_ROOF                             )
!   ##########################################################################
!
!!****  *URBAN_SNOW_EVOL*  
!!
!!    PURPOSE
!!    -------
!
!     
!!**  METHOD
!     ------
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
!!      M. Goret   04/09/2017 : add diagnostic of heat storage link to snow
!!      V. Masson   04.2020 completes energy check for high vegetation IR exchanges
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_TEB_OPTION_n, ONLY : TEB_OPTIONS_t      
USE MODD_TEB_n, ONLY : TEB_t
USE MODD_BEM_n, ONLY : BEM_t
USE MODD_DIAG_MISC_TEB_n, ONLY : DIAG_MISC_TEB_t
USE MODD_CHECK_TEB, ONLY : CHECK_TEB_t
USE MODD_LW_COEF, ONLY : LW_COEF_t
!
USE MODD_SNOW_PAR, ONLY : XZ0SN, XZ0HSN,                                    &
                            XANSMIN_ROOF, XANSMAX_ROOF, XANS_TODRY_ROOF,      &
                            XANS_T_ROOF, XRHOSMIN_ROOF, XRHOSMAX_ROOF,        &
                            XWCRN_ROOF,                                       &
                            XANSMIN_ROAD, XANSMAX_ROAD, XANS_TODRY_ROAD,      &
                            XANS_T_ROAD, XRHOSMIN_ROAD, XRHOSMAX_ROAD,        &
                            XWCRN_ROAD  
USE MODD_CSTS,     ONLY : XSTEFAN
!
USE MODE_SURF_SNOW_FRAC
!
USE MODI_ROOF_IMPL_COEF
USE MODI_SNOW_COVER_1LAYER
!
USE MODD_SURF_PAR, ONLY : XUNDEF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
TYPE(TEB_OPTIONS_t), INTENT(INOUT) :: TOP
TYPE(DIAG_MISC_TEB_t), INTENT(INOUT) :: DMT
TYPE(TEB_t), INTENT(INOUT) :: T
TYPE(BEM_t), INTENT(INOUT) :: B
TYPE(CHECK_TEB_t), INTENT(INOUT) :: CT
TYPE(LW_COEF_t), INTENT(IN) :: LW
!
 CHARACTER(LEN=6), INTENT(IN)        :: HPROGRAM ! program calling surf. schemes
REAL, DIMENSION(:),   INTENT(IN)    :: PT_LWCN  ! LWCN air temperature
REAL, DIMENSION(:),   INTENT(IN)    :: PQ_LWCN  ! LWCN air specific humidity
REAL, DIMENSION(:),   INTENT(IN)    :: PU_LWCN  ! LWCN hor. wind
REAL, DIMENSION(:),   INTENT(IN)    :: PTS_RF   ! roof surface temperature
REAL, DIMENSION(:),   INTENT(IN)    :: PTS_RD   ! road surface temperature
REAL, DIMENSION(:),   INTENT(IN)    :: PTS_WL_A ! wall surface temperature
REAL, DIMENSION(:),   INTENT(IN)    :: PTS_WL_B ! wall surface temperature
!
REAL, DIMENSION(:), INTENT(IN)    :: PPS      ! pressure at the surface
REAL, DIMENSION(:), INTENT(IN)    :: PTA      ! temperature at the lowest level
REAL, DIMENSION(:), INTENT(IN)    :: PQA      ! specific humidity
                                              ! at the lowest level
REAL, DIMENSION(:), INTENT(IN)    :: PVMOD    ! module of the horizontal wind
REAL, DIMENSION(:), INTENT(IN)    :: PRHOA    ! air density at the lowest level
REAL, DIMENSION(:), INTENT(IN)    :: PLW_RAD  ! atmospheric infrared radiation
REAL, DIMENSION(:), INTENT(IN)    :: PSR      ! snow rate
REAL, DIMENSION(:), INTENT(IN)    :: PZREF    ! reference height of the first
                                              ! atmospheric level (temperature)
REAL, DIMENSION(:), INTENT(IN)    :: PUREF    ! reference height of the first
                                              ! atmospheric level (wind)
                                              ! at first atmospheric level
REAL,               INTENT(IN)    :: PTSTEP   ! time step
REAL, DIMENSION(:), INTENT(IN)    :: PZ_LWCN  ! height of forcing
!
REAL, DIMENSION(:), INTENT(IN)    :: PDN_RF          ! snow-covered roof frac.
REAL, DIMENSION(:), INTENT(IN)    :: PABS_SW_SN_RF ! SW absorbed by roof snow
REAL, DIMENSION(:), INTENT(OUT)   :: PABS_LW_SN_RF ! absorbed IR rad by snow on roof
REAL, DIMENSION(:), INTENT(INOUT) :: PDN_RD          ! snow-covered road frac.
REAL, DIMENSION(:), INTENT(IN)    :: PABS_SW_SN_RD ! SW absorbed by road snow
REAL, DIMENSION(:), INTENT(OUT)   :: PABS_LW_SN_RD ! absorbed IR rad by snow on road
!
REAL, DIMENSION(:), INTENT(OUT)   :: PRNSN_RF ! net radiation over snow
REAL, DIMENSION(:), INTENT(OUT)   :: PHSN_RF  ! sensible heat flux over snow
REAL, DIMENSION(:), INTENT(OUT)   :: PLESN_RF ! latent heat flux over snow
REAL, DIMENSION(:), INTENT(OUT)   :: PGSN_RF  ! flux under the snow
REAL, DIMENSION(:), INTENT(OUT)   :: PMELT_RF   ! snow melt
REAL, DIMENSION(:), INTENT(OUT)   :: PRNSN_RD ! net radiation over snow
REAL, DIMENSION(:), INTENT(OUT)   :: PHSN_RD  ! sensible heat flux over snow
REAL, DIMENSION(:), INTENT(OUT)   :: PLESN_RD ! latent heat flux over snow
REAL, DIMENSION(:), INTENT(OUT)   :: PGSN_RD  ! flux under the snow
REAL, DIMENSION(:), INTENT(OUT)   :: PMELT_RD   ! snow melt
!
REAL, DIMENSION(:), INTENT(IN)    :: PTS_HVEG            ! High vegetation temperature
!
REAL, DIMENSION(:), INTENT(OUT)   :: PEMIT_LW_SN_RD
REAL, DIMENSION(:), INTENT(OUT)   :: PEMIT_LW_SN_RF
!
REAL, DIMENSION(:), INTENT(OUT)   :: PSNOW_HEAT_ROAD  !heat storage link to snow on road (W/m2 (road))
REAL, DIMENSION(:), INTENT(OUT)   :: PSNOW_HEAT_ROOF  !heat storage link to snow on roof (W/m2 (roof))
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(SIZE(PTA)) :: ZLW1_RD   ! independant from
REAL, DIMENSION(SIZE(PTA)) :: ZLW1_RF   ! surface temperature
!
REAL, DIMENSION(SIZE(PTA)) :: ZLW2_RD   ! to be multiplied by
REAL, DIMENSION(SIZE(PTA)) :: ZLW2_RF   ! 4th power of
!                                         ! surface temperature

REAL, DIMENSION(SIZE(PTA)) :: ZSR_RF    ! snow fall on roof snow (kg/s/m2 of snow)
REAL, DIMENSION(SIZE(PTA)) :: ZSR_RD    ! snow fall on road snow (kg/s/m2 of snow)
!
REAL, DIMENSION(SIZE(PTA)) :: ZT_SKY      ! sky temperature
REAL, DIMENSION(SIZE(PTA)) :: ZTS_COEFA   ! Coefficient A for implicit coupling
!                                         ! of snow with the underlying surface
REAL, DIMENSION(SIZE(PTA)) :: ZTS_COEFB   ! Coefficient B for implicit coupling
!                                         ! of snow with the underlying surface
REAL, DIMENSION(SIZE(PTA)) :: ZSEN_SNOW_DIF_ROAD ! Sensible heat due to snowfall on road
REAL, DIMENSION(SIZE(PTA)) :: ZSEN_SNOW_DIF_ROOF ! Sensible heat due to snowfall on roof
REAL, DIMENSION(SIZE(PTA)) :: ZDQS_SN_RF ! Heat storage in snowpack on roofs
REAL, DIMENSION(SIZE(PTA)) :: ZDQS_SN_RD ! Heat storage in snowpack on roads
!
! flags to call to snow routines
!
LOGICAL :: GSN_RF, GSN_RD
!
! loop counters
!
INTEGER :: JL
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('URBAN_SNOW_EVOL',0,ZHOOK_HANDLE)
PRNSN_RF(:)=0.
PHSN_RF (:)=0.
PLESN_RF(:)=0.
PGSN_RF (:)=0.
PMELT_RF(:)=0.
PRNSN_RD(:)=0.
PHSN_RD (:)=0.
PLESN_RD(:)=0.
PGSN_RD (:)=0.
PMELT_RD(:)=0.
PABS_LW_SN_RF(:)=0.
PABS_LW_SN_RD(:)=0.
!
PEMIT_LW_SN_RD(:)=0.0
PEMIT_LW_SN_RF(:)=0.0
!
PSNOW_HEAT_ROAD(:)=0.
PSNOW_HEAT_ROOF(:)=0.
!
IF (CT%LCHECK_TEB.AND..NOT.TOP%LEXPLW) THEN
  CT%XLW_WALA_TO_SNOW(:)=0.0
  CT%XLW_WALB_TO_SNOW(:)=0.0
  CT%XLW_WIND_TO_SNOW(:)=0.0
  CT%XLW_HV_TO_SNOW  (:)=0.0
END IF
!
ZSEN_SNOW_DIF_ROAD(:)=0.0
ZSEN_SNOW_DIF_ROOF(:)=0.0
!
ZDQS_SN_RF(:)=0.0
ZDQS_SN_RD(:)=0.0
!
!-------------------------------------------------------------------------------
!
GSN_RF = ANY( PSR(:)>0. .OR. T%TSNOW_ROOF%WSNOW(:,1)>0. )
GSN_RD = ANY( PSR(:)>0. .OR. T%TSNOW_ROAD%WSNOW(:,1)>0. )
!
!-------------------------------------------------------------------------------
!
!*      5.     Snow mantel model
!              -----------------
!
!*      5.1    roofs
!              -----
!
IF ( GSN_RF ) THEN
!
!* initializes LW radiative coefficients
!
  ZLW1_RF(:) =   T%TSNOW_ROOF%EMIS(:) * PLW_RAD(:)
  ZLW2_RF(:) = - T%TSNOW_ROOF%EMIS(:) * XSTEFAN
!
!* The global amount of snow on roofs is supposed located on a
!  fraction of the roof surface. All computations are then
!  done only for each m2 of snow, and not for each m2 of roof.
!
  DO JL=1,SIZE(T%TSNOW_ROOF%WSNOW,2)
    WHERE (PDN_RF(:)>0.) T%TSNOW_ROOF%WSNOW(:,JL) = T%TSNOW_ROOF%WSNOW(:,JL) / PDN_RF(:)
  END DO
  ZSR_RF=0.
  WHERE (PDN_RF(:)>0.) ZSR_RF   (:) = PSR   (:) / PDN_RF(:)
!
!* Estimates implicit coupling between snow and roof
! (strictly equal to an implicit formulation for 100% snow coverage)
!
  CALL ROOF_IMPL_COEF(T, PTSTEP, ZTS_COEFA, ZTS_COEFB)
!
!* call to snow mantel scheme
!
  IF (T%TSNOW_ROOF%SCHEME=='1-L') THEN
   CALL SNOW_COVER_1LAYER(CT, TOP, HPROGRAM, PTSTEP, XANSMIN_ROOF, XANSMAX_ROOF, XANS_TODRY_ROOF, &
                          XRHOSMIN_ROOF, XRHOSMAX_ROOF, XANS_T_ROOF, .TRUE., 0., &
                          XWCRN_ROOF, XZ0SN, XZ0HSN, T%TSNOW_ROOF, PTS_RF,       &
                          ZTS_COEFA, ZTS_COEFB, PABS_SW_SN_RF, ZLW1_RF, ZLW2_RF, &
                          PTA, PQA, PVMOD, PPS, PRHOA, ZSR_RF, PZREF, PUREF,     &
                          PRNSN_RF, PHSN_RF, PLESN_RF, PGSN_RF, PMELT_RF,        &
                          ZDQS_SN_RF, PABS_LW_SN_RF, DMT%XABS_LW_SNOW_ROOF,      &
                          ZSEN_SNOW_DIF_ROOF, PEMIT_LW_SN_RF, PSNOW_HEAT_ROOF, "OK" )
!
  ENDIF
!
!* The global amount of snow on roofs is reported to total roof surface.
!
  DO JL=1,SIZE(T%TSNOW_ROOF%WSNOW,2)
    T%TSNOW_ROOF%WSNOW(:,JL) = T%TSNOW_ROOF%WSNOW(:,JL) * PDN_RF(:)
  END DO
!
  PSNOW_HEAT_ROOF= PSNOW_HEAT_ROOF* PDN_RF
!
END IF
  !
IF (CT%LCHECK_TEB) THEN
  CT%XSEN_SNOW_DIF_ROOF(:) = ZSEN_SNOW_DIF_ROOF(:) * PDN_RF(:)
  CT%XDQS_SNOW_ROOF(:) = ZDQS_SN_RF(:)
END IF
!      
!
!*      5.2    roads
!              -----
!
IF ( GSN_RD ) THEN
  !
  ZT_SKY(:) = (PLW_RAD(:)/XSTEFAN)**0.25
!
  ZLW1_RD(:) = LW%XLW_S_TO_NR  (:)   * (ZT_SKY   (:) - T%TSNOW_ROAD%TS(:)) &
          + LW%XLW_WA_TO_NR (:) * (PTS_WL_A (:) - T%TSNOW_ROAD%TS(:)) &
          + LW%XLW_WB_TO_NR (:) * (PTS_WL_B (:) - T%TSNOW_ROAD%TS(:)) &
          + LW%XLW_WIN_TO_NR(:) * (B%XT_WIN1(:) - T%TSNOW_ROAD%TS(:)) &
          + LW%XLW_HV_TO_NR (:) * (PTS_HVEG(:)  - T%TSNOW_ROAD%TS(:))
  !
  ! Robert: include diagnostics of longwave radiation exchange between
  !         the snow on roads and the other surfaces (W/m²(urb))
  !
  IF (CT%LCHECK_TEB.AND..NOT.TOP%LEXPLW) THEN
     CT%XLW_WALA_TO_SNOW(:)=T%XROAD(:)*PDN_RD(:)*LW%XLW_WA_TO_NR(:) *(PTS_WL_A(:) - T%TSNOW_ROAD%TS(:))
     CT%XLW_WALB_TO_SNOW(:)=T%XROAD(:)*PDN_RD(:)*LW%XLW_WB_TO_NR(:) *(PTS_WL_B(:) - T%TSNOW_ROAD%TS(:))
     CT%XLW_WIND_TO_SNOW(:)=T%XROAD(:)*PDN_RD(:)*LW%XLW_WIN_TO_NR(:)*(B%XT_WIN1(:)- T%TSNOW_ROAD%TS(:))
     CT%XLW_HV_TO_SNOW  (:)=T%XROAD(:)*PDN_RD(:)*LW%XLW_HV_TO_NR (:)*(PTS_HVEG(:) - T%TSNOW_ROAD%TS(:))
  END IF
  !
  ZLW2_RD(:) =  0.0
  !
  !* The global amount of snow on roads is supposed located on a
  !  fraction of the road surface. All computations are then
  !  done only for each m2 of snow, and not for each m2 of road.
  !
  DO JL=1,SIZE(T%TSNOW_ROAD%WSNOW,2)
    WHERE (PDN_RD(:)>0.) T%TSNOW_ROAD%WSNOW(:,JL) = T%TSNOW_ROAD%WSNOW(:,JL) / PDN_RD(:)
  END DO
  ZSR_RD=0.
  WHERE (PDN_RD(:)>0.) ZSR_RD   (:) = PSR   (:) / PDN_RD(:)
  !
  !* no implicit coupling necessary with road
  !
  ZTS_COEFA = 0.
  ZTS_COEFB = PTS_RD
  !
  !* call to snow mantel scheme
  !
  IF (T%TSNOW_ROAD%SCHEME=='1-L') THEN
     !
     CALL SNOW_COVER_1LAYER(CT, TOP, HPROGRAM, PTSTEP, XANSMIN_ROAD, XANSMAX_ROAD, XANS_TODRY_ROAD, &
                            XRHOSMIN_ROAD, XRHOSMAX_ROAD, XANS_T_ROAD, .FALSE.,         &
                            0., XWCRN_ROAD, XZ0SN, XZ0HSN, T%TSNOW_ROAD, PTS_RD,        &
                            ZTS_COEFA, ZTS_COEFB,  PABS_SW_SN_RD, ZLW1_RD, ZLW2_RD,     &
                            PT_LWCN, PQ_LWCN, PU_LWCN, PPS, PRHOA, ZSR_RD, PZ_LWCN,     &
                            PZ_LWCN, PRNSN_RD, PHSN_RD, PLESN_RD, PGSN_RD,              &
                            PMELT_RD, ZDQS_SN_RD, PABS_LW_SN_RD, DMT%XABS_LW_SNOW_ROAD, &
                            ZSEN_SNOW_DIF_ROAD, PEMIT_LW_SN_RD, PSNOW_HEAT_ROAD, "OK" )
     !
  ENDIF
  !
  !* The global amount of snow on roads is reported to total road surface.
  !
  DO JL=1,SIZE(T%TSNOW_ROAD%WSNOW,2)
     T%TSNOW_ROAD%WSNOW(:,JL) = T%TSNOW_ROAD%WSNOW(:,JL) * PDN_RD(:)
  END DO
  !
  PSNOW_HEAT_ROAD= PSNOW_HEAT_ROAD* PDN_RD
  !
END IF
!
IF (CT%LCHECK_TEB) THEN
  CT%XSEN_SNOW_DIF_ROAD(:) = ZSEN_SNOW_DIF_ROAD(:)* PDN_RD(:)
  CT%XDQS_SNOW_ROAD(:) = ZDQS_SN_RD(:)
END IF
IF (LHOOK) CALL DR_HOOK('URBAN_SNOW_EVOL',1,ZHOOK_HANDLE)
!
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE URBAN_SNOW_EVOL
