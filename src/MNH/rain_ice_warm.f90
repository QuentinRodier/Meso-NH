!MNH_LIC Copyright 1995-2019 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
! Modifications:
!  P. Wautelet 25/02/2019: split rain_ice (cleaner and easier to maintain/debug)
!  P. Wautelet 03/06/2019: remove PACK/UNPACK intrinsics (to get more performance and better OpenACC support)
!-----------------------------------------------------------------
MODULE MODE_RAIN_ICE_WARM

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: RAIN_ICE_WARM

CONTAINS

SUBROUTINE RAIN_ICE_WARM(OMICRO, KMICRO, K1, K2, K3,                                                           &
                         PRHODREF, PRVT, PRCT, PRRT, PHLC_HCF, PHLC_LCF, PHLC_HRC, PHLC_LRC,                   &
                         PRHODJ, PPRES, PZT, PLBDAR, PLBDAR_RF, PLVFACT, PCJ, PKA, PDV, PRF, PCF, PTHT, PTHLT, &
                         PRHODJ3D, PTHS3D, PRVS3D, PRVS, PRCS, PRRS, PTHS, PUSW, PEVAP3D)
!
!*      0. DECLARATIONS
!          ------------
!
use MODD_BUDGET,         only: LBUDGET_RC, LBUDGET_RR, LBUDGET_RV, LBUDGET_TH
use MODD_CST,            only: XALPW, XBETAW, XCL, XCPV, XGAMW, XLVTT, XMD, XMV, XRV, XTT
use MODD_PARAM_ICE,      only: CSUBG_RC_RR_ACCR, CSUBG_RR_EVAP
use MODD_RAIN_ICE_DESCR, only: XCEXVT, XRTMIN
use MODD_RAIN_ICE_PARAM, only: X0EVAR, X1EVAR, XCRIAUTC, XEX0EVAR, XEX1EVAR, XEXCACCR, XFCACCR, XTIMAUTC
!
use MODE_MSG
!
use MODI_BUDGET
!
IMPLICIT NONE
!
!*       0.1   Declarations of dummy arguments :
!
LOGICAL,  DIMENSION(:,:,:), intent(in)    :: OMICRO   ! Test where to compute all processes
INTEGER,                    intent(in)    :: KMICRO
INTEGER,  DIMENSION(:),     intent(in)    :: K1
INTEGER,  DIMENSION(:),     intent(in)    :: K2
INTEGER,  DIMENSION(:),     intent(in)    :: K3
REAL,     DIMENSION(:),     intent(in)    :: PRHODREF ! RHO Dry REFerence
REAL,     DIMENSION(:),     intent(in)    :: PRVT     ! Water vapor m.r. at t
REAL,     DIMENSION(:),     intent(in)    :: PRCT     ! Cloud water m.r. at t
REAL,     DIMENSION(:),     intent(in)    :: PRRT     ! Rain water m.r. at t
REAL,     DIMENSION(:),     intent(in)    :: PHLC_HCF ! HLCLOUDS : fraction of High Cloud Fraction in grid
REAL,     DIMENSION(:),     intent(in)    :: PHLC_LCF ! HLCLOUDS : fraction of Low  Cloud Fraction in grid
REAL,     DIMENSION(:),     intent(in)    :: PHLC_HRC ! HLCLOUDS : LWC that is High LWC in grid
REAL,     DIMENSION(:),     intent(in)    :: PHLC_LRC ! HLCLOUDS : LWC that is Low  LWC in grid
REAL,     DIMENSION(:),     intent(in)    :: PRHODJ   ! RHO times Jacobian
REAL,     DIMENSION(:),     intent(in)    :: PPRES    ! Pressure
REAL,     DIMENSION(:),     intent(in)    :: PZT      ! Temperature
REAL,     DIMENSION(:),     intent(in)    :: PLBDAR   ! Slope parameter of the raindrop  distribution
REAL,     DIMENSION(:),     intent(in)    :: PLBDAR_RF! Slope parameter of the raindrop  distribution
                                                      ! for the Rain Fraction part
REAL,     DIMENSION(:),     intent(in)    :: PLVFACT  ! L_v/(Pi_ref*C_ph)
REAL,     DIMENSION(:),     intent(in)    :: PCJ      ! Function to compute the ventilation coefficient
REAL,     DIMENSION(:),     intent(in)    :: PKA      ! Thermal conductivity of the air
REAL,     DIMENSION(:),     intent(in)    :: PDV      ! Diffusivity of water vapor in the air
REAL,     DIMENSION(:),     intent(in)    :: PRF      ! Rain fraction
REAL,     DIMENSION(:),     intent(in)    :: PCF      ! Cloud fraction
REAL,     DIMENSION(:),     intent(in)    :: PTHT     ! Potential temperature
REAL,     DIMENSION(:),     intent(in)    :: PTHLT    ! Liquid potential temperature
REAL,     DIMENSION(:,:,:), INTENT(IN)    :: PRHODJ3D ! Dry density * Jacobian
REAL,     DIMENSION(:,:,:), INTENT(IN)    :: PTHS3D   ! Theta source
REAL,     DIMENSION(:,:,:), INTENT(IN)    :: PRVS3D   ! Water vapor m.r. source
REAL,     DIMENSION(:),     INTENT(INOUT) :: PRVS     ! Water vapor m.r. source
REAL,     DIMENSION(:),     INTENT(INOUT) :: PRCS     ! Cloud water m.r. source
REAL,     DIMENSION(:),     INTENT(INOUT) :: PRRS     ! Rain water m.r. source
REAL,     DIMENSION(:),     INTENT(INOUT) :: PTHS     ! Theta source
!PW: PUSW could be a purely local variable?
REAL,     DIMENSION(:),     INTENT(INOUT) :: PUSW     ! Undersaturation over water
REAL,     DIMENSION(:,:,:), INTENT(INOUT) :: PEVAP3D  ! Rain evap profile
!
!*       0.2  declaration of local variables
!
INTEGER                         :: JL
REAL, DIMENSION(size(PRHODREF)) :: ZZW  ! Work array
REAL, DIMENSION(size(PRHODREF)) :: ZZW2 ! Work array
REAL, DIMENSION(size(PRHODREF)) :: ZZW3 ! Work array
REAL, DIMENSION(size(PRHODREF)) :: ZZW4 ! Work array
!
!-------------------------------------------------------------------------------
!
!*       4.2    compute the autoconversion of r_c for r_r production: RCAUTR
!

    WHERE( PRCS(:)>0.0 .AND. PHLC_HCF(:).GT.0.0 )
      ZZW(:) = XTIMAUTC*MAX( PHLC_HRC(:)/PHLC_HCF(:)  - XCRIAUTC/PRHODREF(:),0.0)
      ZZW(:) = MIN( PRCS(:),PHLC_HCF(:)*ZZW(:))
      PRCS(:) = PRCS(:) - ZZW(:)
      PRRS(:) = PRRS(:) + ZZW(:)
    END WHERE
!
      IF (LBUDGET_RC) CALL BUDGET (                                               &
                       UNPACK(PRCS(:)*PRHODJ(:),MASK=OMICRO(:,:,:),FIELD=0.0),    &
                                                                7,'AUTO_BU_RRC')
      IF (LBUDGET_RR) CALL BUDGET (                                               &
                       UNPACK(PRRS(:)*PRHODJ(:),MASK=OMICRO(:,:,:),FIELD=0.0),    &
                                                                8,'AUTO_BU_RRR')
!
!*       4.3    compute the accretion of r_c for r_r production: RCACCR
!
    IF (CSUBG_RC_RR_ACCR=='NONE') THEN
      !CLoud water and rain are diluted over the grid box
      WHERE( PRCT(:)>XRTMIN(2) .AND. PRRT(:)>XRTMIN(3) .AND. PRCS(:)>0.0 )
        ZZW(:) = MIN( PRCS(:), XFCACCR * PRCT(:)                &
                 * PLBDAR(:)**XEXCACCR    &
                 * PRHODREF(:)**(-XCEXVT) )
        PRCS(:) = PRCS(:) - ZZW(:)
        PRRS(:) = PRRS(:) + ZZW(:)
      END WHERE

    ELSEIF (CSUBG_RC_RR_ACCR=='PRFR') THEN
      !Cloud water is concentrated over its fraction with possibly to parts with high and low content as set for autoconversion
      !Rain is concnetrated over its fraction
      !Rain in high content area fraction: PHLC_HCF
      !Rain in low content area fraction:
      ! if PRF<PCF (rain is entirely falling in cloud): PRF-PHLC_HCF
      ! if PRF>PCF (rain is falling in cloud and in clear sky): PCF-PHLC_HCF
      ! => min(PCF, PRF)-PHLC_HCF
      ZZW(:) = 0.
      WHERE( PHLC_HRC(:)>XRTMIN(2) .AND. PRRT(:)>XRTMIN(3) .AND. PRCS(:)>0.0 &
            .AND. PHLC_HCF(:)>0 )
        !Accretion due to rain falling in high cloud content
        ZZW(:) = XFCACCR * ( PHLC_HRC(:)/PHLC_HCF(:) )     &
               * PLBDAR_RF(:)**XEXCACCR &
               * PRHODREF(:)**(-XCEXVT) &
               * PHLC_HCF
      END WHERE
      WHERE( PHLC_LRC(:)>XRTMIN(2) .AND. PRRT(:)>XRTMIN(3) .AND. PRCS(:)>0.0 &
            .AND. PHLC_LCF(:)>0 )
        !We add acrretion due to rain falling in low cloud content
        ZZW(:) = ZZW(:) + XFCACCR * ( PHLC_LRC(:)/PHLC_LCF(:) )     &
                        * PLBDAR_RF(:)**XEXCACCR &
                        * PRHODREF(:)**(-XCEXVT) &
                        * (MIN(PCF(:), PRF(:))-PHLC_HCF(:))
      END WHERE
      ZZW(:)=MIN(PRCS(:), ZZW(:))
      PRCS(:) = PRCS(:) - ZZW(:)
      PRRS(:) = PRRS(:) + ZZW(:)

    ELSE
      !wrong CSUBG_RC_RR_ACCR case
      WRITE(*,*) 'wrong CSUBG_RC_RR_ACCR case'
            CALL PRINT_MSG(NVERB_FATAL,'GEN','RAIN_ICE_WARM','')
    ENDIF

    IF (LBUDGET_RC) CALL BUDGET (                                               &
                     UNPACK(PRCS(:)*PRHODJ(:),MASK=OMICRO(:,:,:),FIELD=0.0),    &
                                                              7,'ACCR_BU_RRC')
    IF (LBUDGET_RR) CALL BUDGET (                                               &
                     UNPACK(PRRS(:)*PRHODJ(:),MASK=OMICRO(:,:,:),FIELD=0.0),    &
                                                              8,'ACCR_BU_RRR')
!
!*       4.4    compute the evaporation of r_r: RREVAV
!
    ZZW(:) = 0.0

    IF (CSUBG_RR_EVAP=='NONE') THEN
      !Evaporation only when there's no cloud (RC must be 0)
       WHERE( (PRRT(:)>XRTMIN(3)) .AND. (PRCT(:)<=XRTMIN(2)) )
          ZZW(:)  = EXP( XALPW - XBETAW/PZT(:) - XGAMW*ALOG(PZT(:) ) ) ! es_w
          PUSW(:) = 1.0 - PRVT(:)*( PPRES(:)-ZZW(:) ) / ( (XMV/XMD) * ZZW(:) )
                                                        ! Undersaturation over water
          ZZW(:) = ( XLVTT+(XCPV-XCL)*(PZT(:)-XTT) )**2 / ( PKA(:)*XRV*PZT(:)**2 ) &
               + ( XRV*PZT(:) ) / ( PDV(:)*ZZW(:) )
          ZZW(:) = MIN( PRRS(:),( MAX( 0.0,PUSW(:) )/(PRHODREF(:)*ZZW(:)) ) *      &
            ( X0EVAR*PLBDAR(:)**XEX0EVAR+X1EVAR*PCJ(:)*PLBDAR(:)**XEX1EVAR ) )
          PRRS(:) = PRRS(:) - ZZW(:)
          PRVS(:) = PRVS(:) + ZZW(:)
          PTHS(:) = PTHS(:) - ZZW(:)*PLVFACT(:)
       END WHERE

    ELSEIF (CSUBG_RR_EVAP=='CLFR' .OR. CSUBG_RR_EVAP=='PRFR') THEN
      !Evaporation in clear sky part
      !With CLFR, rain is diluted over the grid box
      !With PRFR, rain is concentrated in its fraction
      !Use temperature and humidity in clear sky part like Bechtold et al. (1993)
      IF (CSUBG_RR_EVAP=='CLFR') THEN
        ZZW4(:)=1. !Precipitation fraction
        ZZW3(:)=PLBDAR(:)
      ELSE
        ZZW4(:)=PRF(:) !Precipitation fraction
        ZZW3(:)=PLBDAR_RF(:)
      ENDIF

      !ATTENTION
      !Il faudrait recalculer les variables PKA, PDV, PCJ en tenant compte de la température T^u
      !Ces variables devraient être sorties de rain_ice_slow et on mettrait le calcul de T^u, T^s
      !et plusieurs versions (comme actuellement, en ciel clair, en ciel nuageux) de PKA, PDV, PCJ dans rain_ice
      !On utiliserait la bonne version suivant l'option NONE, CLFR... dans l'évaporation et ailleurs

      WHERE(  (PRRT(:)>XRTMIN(3)) .AND. ( ZZW4(:) > PCF(:) ) )
        ! outside the cloud (environment) the use of T^u (unsaturated) instead of T
        ! Bechtold et al. 1993
        !
        ! T^u = T_l = theta_l * (T/theta)
        ZZW2(:) =  PTHLT(:) * PZT(:) / PTHT(:)
        !
        ! es_w with new T^u
        ZZW(:)  = EXP( XALPW - XBETAW/ZZW2(:) - XGAMW*ALOG(ZZW2(:) ) )
        !
        ! S, Undersaturation over water (with new theta^u)
        PUSW(:) = 1.0 - PRVT(:)*( PPRES(:)-ZZW(:) ) / ( (XMV/XMD) * ZZW(:) )
        !
        ZZW(:) = ( XLVTT+(XCPV-XCL)*(ZZW2(:)-XTT) )**2 / ( PKA(:)*XRV*ZZW2(:)**2 ) &
               + ( XRV*ZZW2(:) ) / ( PDV(:)*ZZW(:) )
        !
        ZZW(:) = MAX( 0.0,PUSW(:) )/(PRHODREF(:)*ZZW(:))  *      &
               ( X0EVAR*ZZW3(:)**XEX0EVAR+X1EVAR*PCJ(:)*ZZW3(:)**XEX1EVAR )
        !
        ZZW(:) = MIN( PRRS(:),  ZZW(:) *( ZZW4(:) - PCF(:) ) )
        !
        PRRS(:) = PRRS(:) - ZZW(:)
        PRVS(:) = PRVS(:) + ZZW(:)
        PTHS(:) = PTHS(:) - ZZW(:)*PLVFACT(:)
      END WHERE

    ELSE
      !wrong CSUBG_RR_EVAP case
      WRITE(*,*) 'wrong CSUBG_RR_EVAP case'
      CALL PRINT_MSG(NVERB_FATAL,'GEN','RAIN_ICE_WARM','')
    END IF

    IF (LBUDGET_TH) CALL BUDGET (                                               &
                 UNPACK(PTHS(:),MASK=OMICRO(:,:,:),FIELD=PTHS3D)*PRHODJ3D(:,:,:),   &
                                                              4,'REVA_BU_RTH')
    IF (LBUDGET_RV) CALL BUDGET (                                               &
                 UNPACK(PRVS(:),MASK=OMICRO(:,:,:),FIELD=PRVS3D)*PRHODJ3D(:,:,:),   &
                                                              6,'REVA_BU_RRV')
    IF (LBUDGET_RR) CALL BUDGET (                                               &
                     UNPACK(PRRS(:)*PRHODJ(:),MASK=OMICRO(:,:,:),FIELD=0.0),    &
                                                              8,'REVA_BU_RRR')

    DO JL = 1, KMICRO
      PEVAP3D(K1(JL), K2(JL), K3(JL)) = ZZW( JL )
    END DO
!
  END SUBROUTINE RAIN_ICE_WARM

END MODULE MODE_RAIN_ICE_WARM
