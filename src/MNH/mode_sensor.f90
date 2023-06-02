!MNH_LIC Copyright 2023-2023 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
! Original version:
!  P. Wautelet: 25/05/2023
! Modifications:
!-----------------------------------------------------------------
MODULE MODE_SENSOR
  USE MODD_SENSOR

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: SENSOR_WC_COMPUTE
  PUBLIC :: SENSOR_RARE_COMPUTE
  PUBLIC :: SENSOR_DIACHRO_POINT_ADD
  PUBLIC :: SENSOR_DIACHRO_PROFILE_ADD

  CONTAINS

  SUBROUTINE SENSOR_WC_COMPUTE( TPSENSOR, KSTORE_ID, PR, PRHODREF )
    USE MODD_PARAM_N, ONLY: CCLOUD

    CLASS(TSENSOR), INTENT(INOUT) :: TPSENSOR
    INTEGER,        INTENT(IN)    :: KSTORE_ID

    REAL, DIMENSION(:,:,:,:), INTENT(IN) :: PR       ! water mixing ratios
    REAL, DIMENSION(:,:,:),   INTENT(IN) :: PRHODREF ! dry air density of the reference state

    ! initialization LWC and IWC
    TPSENSOR%XLWCZ(:,KSTORE_ID) = 0.
    TPSENSOR%XIWCZ(:,KSTORE_ID) = 0.

    IF ( CCLOUD == "LIMA" .OR. CCLOUD=="ICE3" ) THEN
      TPSENSOR%XLWCZ(:,KSTORE_ID) = TPSENSOR%INTERP_HOR_FROM_MASSPOINT( (PR(:,:,:,2)+PR(:,:,:,3)            ) * PRHODREF(:,:,:) )
      TPSENSOR%XIWCZ(:,KSTORE_ID) = TPSENSOR%INTERP_HOR_FROM_MASSPOINT( (PR(:,:,:,4)+PR(:,:,:,5)+PR(:,:,:,6)) * PRHODREF(:,:,:) )
    END IF
  END SUBROUTINE SENSOR_WC_COMPUTE


  SUBROUTINE SENSOR_RARE_COMPUTE( TPSENSOR, KSTORE_ID, PR, PSV, PRHODREF, PCIT, PTH_EXN, PZMZ, PSEA )
    USE MODD_CST,              ONLY: XLAM_CRAD, XLIGHTSPEED, XPI, XRHOLW, XTT
    USE MODD_NSV,              ONLY: NSV_LIMA_NC, NSV_LIMA_NR, NSV_LIMA_NI
    USE MODD_PARAMETERS,       ONLY: XUNDEF
    USE MODD_PARAM_ICE,        ONLY: LSNOW_T_I => LSNOW_T
    USE MODD_PARAM_LIMA,       ONLY: LSNOW_T_L => LSNOW_T,                                                       &
                                     XALPHAR_L => XALPHAR, XNUR_L => XNUR, XALPHAS_L => XALPHAS, XNUS_L => XNUS, &
                                     XALPHAG_L => XALPHAG, XNUG_L => XNUG, XALPHAI_L => XALPHAI, XNUI_L => XNUI, &
                                     XRTMIN_L => XRTMIN, XALPHAC_L => XALPHAC, XNUC_L => XNUC
    USE MODD_PARAM_LIMA_COLD,  ONLY: XAI_L => XAI, XBI_L => XBI, XLBEXS_L => XLBEXS, XLBS_L => XLBS, XCCS_L => XCCS,  &
                                     XAS_L => XAS, XBS_L => XBS, XCXS_L => XCXS,                                      &
                                     XLBDAS_MAX_L => XLBDAS_MAX, XLBDAS_MIN_L => XLBDAS_MIN,                          &
                                     XNS_L => XNS, XTRANS_MP_GAMMAS_L=>XTRANS_MP_GAMMAS
    USE MODD_PARAM_LIMA_MIXED, ONLY: XLBEXG_L => XLBEXG, XLBG_L => XLBG, XCCG_L => XCCG, XAG_L => XAG, XBG_L => XBG, XCXG_L => XCXG
    USE MODD_PARAM_LIMA_WARM,  ONLY: XAC_L => XAC, XAR_L => XAR, XBC_L => XBC, XBR_L => XBR
    USE MODD_PARAM_N,          ONLY: CCLOUD, CSURF
    USE MODD_RAIN_ICE_DESCR,   ONLY: XALPHAR_I => XALPHAR, XNUR_I => XNUR, XLBEXR_I => XLBEXR,                   &
                                     XLBR_I => XLBR, XCCR_I => XCCR, XBR_I => XBR, XAR_I => XAR,                 &
                                     XALPHAC_I => XALPHAC, XNUC_I => XNUC, XBC_I => XBC, XAC_I => XAC,           &
                                     XALPHAC2_I => XALPHAC2, XNUC2_I => XNUC2,                                   &
                                     XALPHAS_I => XALPHAS, XNUS_I => XNUS, XLBEXS_I => XLBEXS,                   &
                                     XLBS_I => XLBS, XCCS_I => XCCS, XAS_I => XAS, XBS_I => XBS, XCXS_I => XCXS, &
                                     XALPHAG_I => XALPHAG, XNUG_I => XNUG, XLBEXG_I => XLBEXG,                   &
                                     XLBG_I => XLBG, XCCG_I => XCCG, XAG_I => XAG, XBG_I => XBG, XCXG_I => XCXG, &
                                     XALPHAI_I => XALPHAI, XNUI_I => XNUI, XLBEXI_I => XLBEXI,                   &
                                     XLBI_I => XLBI, XAI_I => XAI, XBI_I => XBI,                                 &
                                     XNS_I => XNS, XRTMIN_I => XRTMIN, XCONC_LAND, XCONC_SEA,                    &
                                     XLBDAS_MAX_I => XLBDAS_MAX, XLBDAS_MIN_I => XLBDAS_MIN,                     &
                                     XTRANS_MP_GAMMAS_I => XTRANS_MP_GAMMAS

    USE MODE_FGAU,             ONLY: GAULAG
    USE MODE_FSCATTER,         ONLY: BHMIE, QEPSI, QEPSW, MG, MOMG

    CLASS(TSENSOR),                 INTENT(INOUT) :: TPSENSOR   ! sensor data and metadata
    INTEGER,                        INTENT(IN)    :: KSTORE_ID  ! storage index
    REAL, DIMENSION(:,:,:,:),       INTENT(IN)    :: PR         ! water mixing ratios
    REAL, DIMENSION(:,:,:,:),       INTENT(IN)    :: PSV        ! scalar variables
    REAL, DIMENSION(:,:,:),         INTENT(IN)    :: PRHODREF   ! dry air density of the reference state
    REAL, DIMENSION(:,:,:),         INTENT(IN)    :: PCIT       ! pristine ice concentration
    REAL, DIMENSION(:,:,:),         INTENT(IN)    :: PTH_EXN    ! potential temperature multiplied by exner function
    REAL,DIMENSION(:),              INTENT(IN)    :: PZMZ       ! altitude of model levels at station location
    REAL, DIMENSION(:,:), OPTIONAL, INTENT(IN)    :: PSEA


    INTEGER, PARAMETER :: JPTS_GAULAG = 7 ! number of points for Gauss-Laguerre quadrature

    COMPLEX                         :: QMW, QMI, QM, QEPSIW, QEPSWI   ! dielectric parameters
    INTEGER                         :: IKU
    INTEGER                         :: ICOUNT
    INTEGER                         :: JJ         ! loop counter for qdrature
    INTEGER                         :: JK         ! loop index
    INTEGER                         :: JLOOP      ! loop counter
    LOGICAL                         :: GCALC
    REAL                            :: ZA, ZB, ZCC, ZCX, ZALPHA, ZNS, ZNU, ZLB, ZLBEX, ZRHOHYD ! generic microphysical parameters
    REAL                            :: ZAETOT, ZAETMP, ZREFLOC, ZQSCA, ZQBACK, ZQEXT ! temporary scattering parameters
    REAL                            :: ZLBDA        ! slope distribution parameter
    REAL                            :: ZDELTA_EQUIV ! mass-equivalent gauss-laguerre point
    REAL                            :: ZFW          ! liquid fraction
    REAL                            :: ZFPW         ! weight for mixed-phase reflectivity
    REAL                            :: ZN           ! number concentration
    REAL, DIMENSION(SIZE(PR,3))     :: ZTEMPZ     ! vertical profile of temperature
    REAL, DIMENSION(SIZE(PR,3))     :: ZRHODREFZ  ! vertical profile of dry air density of the reference state
    REAL, DIMENSION(SIZE(PR,3))     :: ZCIT       ! pristine ice concentration
    REAL, DIMENSION(SIZE(PR,3))     :: ZCCI, ZCCR, ZCCC  ! ice, rain, cloud concentration (LIMA)
    REAL, DIMENSION(:), ALLOCATABLE :: ZAELOC       ! temporary array
    REAL, DIMENSION(:), ALLOCATABLE :: ZX, ZW       ! Gauss-Laguerre points and weights
    REAL, DIMENSION(:), ALLOCATABLE :: ZRTMIN       ! local values for xrtmin
    REAL, DIMENSION(SIZE(PR,3),SIZE(PR,4)+1) :: ZRZ  ! vertical profile of hydrometeor mixing ratios

    IKU = SIZE( PRHODREF, 3 )

    ! initialization CRARE and CRARE_ATT
    TPSENSOR%XCRARE    (:,KSTORE_ID) = 0.
    TPSENSOR%XCRARE_ATT(:,KSTORE_ID) = 0.

    IF ( CCLOUD == "LIMA" .OR. CCLOUD=="ICE3" ) THEN
      ZTEMPZ(:)    = TPSENSOR%INTERP_HOR_FROM_MASSPOINT( PTH_EXN(:,:,:)  )
      ZRHODREFZ(:) = TPSENSOR%INTERP_HOR_FROM_MASSPOINT( PRHODREF(:,:,:) )
      IF ( CCLOUD == "LIMA" ) THEN
        ZCCI(:) = TPSENSOR%INTERP_HOR_FROM_MASSPOINT( PSV(:,:,:,NSV_LIMA_NI) )
        ZCCR(:) = TPSENSOR%INTERP_HOR_FROM_MASSPOINT( PSV(:,:,:,NSV_LIMA_NR) )
        ZCCC(:) = TPSENSOR%INTERP_HOR_FROM_MASSPOINT( PSV(:,:,:,NSV_LIMA_NC) )
      ELSE
        ZCIT(:) = TPSENSOR%INTERP_HOR_FROM_MASSPOINT( PCIT(:,:,:) )
      END IF
      DO JLOOP = 3, 6
        ZRZ(:,JLOOP) = TPSENSOR%INTERP_HOR_FROM_MASSPOINT( PR(:,:,:,JLOOP) )
      END DO
      IF ( CSURF == 'EXTE' ) THEN
        DO JK = 1, IKU
          ZRZ(JK,2) = TPSENSOR%INTERP_HOR_FROM_MASSPOINT( PR(:,:,JK,2) * PSEA(:,:)      ) ! becomes cloud mixing ratio over sea
          ZRZ(JK,7) = TPSENSOR%INTERP_HOR_FROM_MASSPOINT( PR(:,:,JK,2) * (1.-PSEA(:,:)) ) ! becomes cloud mixing ratio over land
        END DO
      ELSE
        !if csurf/='EXTE', psea is not allocated
        DO JK=1,IKU
          ZRZ(JK,2)=TPSENSOR%INTERP_HOR_FROM_MASSPOINT( PR(:,:,JK,2) )
          ZRZ(JK,7) = 0.
        END DO
      END IF

      ALLOCATE(ZAELOC(IKU))
      ZAELOC(:) = 0.

      ! initialization of quadrature points and weights
      ALLOCATE( ZX(JPTS_GAULAG), ZW(JPTS_GAULAG) )
      CALL GAULAG( JPTS_GAULAG, ZX, ZW ) ! for integration over diameters

      ! initialize minimum values
      ALLOCATE( ZRTMIN(SIZE( PR, 4 ) + 1) )
      IF ( CCLOUD == 'LIMA') THEN
        ZRTMIN(2) = XRTMIN_L(2) ! cloud water over sea
        ZRTMIN(3) = XRTMIN_L(3)
        ZRTMIN(4) = XRTMIN_L(4)
        ZRTMIN(5) = 1E-10
        ZRTMIN(6) = XRTMIN_L(6)
        ZRTMIN(7) = XRTMIN_L(2) ! cloud water over land
      ELSE
        ZRTMIN(2) = XRTMIN_I(2) ! cloud water over sea
        ZRTMIN(3) = XRTMIN_I(3)
        ZRTMIN(4) = XRTMIN_I(4)
        ZRTMIN(5) = 1E-10
        ZRTMIN(6) = XRTMIN_I(6)
        ZRTMIN(7) = XRTMIN_I(2) ! cloud water over land
      END IF

      ! compute cloud radar reflectivity from vertical profiles of temperature and mixing ratios
      DO JK = 1, IKU
        QMW = SQRT( QEPSW( ZTEMPZ(JK), XLIGHTSPEED / XLAM_CRAD ) )
        QMI = SQRT( QEPSI( ZTEMPZ(JK), XLIGHTSPEED / XLAM_CRAD ) )
        DO JLOOP = 2, 7
          IF ( CCLOUD == 'LIMA' ) THEN
            GCALC = ( ZRZ(JK,JLOOP) > ZRTMIN(JLOOP) ) .AND. ( JLOOP /= 4 .OR. ZCCI(JK) > 0. ) .AND. &
                    ( JLOOP /=3 .OR. ZCCR(JK) > 0. ) .AND. ( ( JLOOP /=2 .AND. JLOOP /= 7 ) .OR. ZCCC(JK) > 0. )
          ELSE
            GCALC = ZRZ(JK,JLOOP) > ZRTMIN(JLOOP) .AND. ( JLOOP /= 4 .OR. ZCIT(JK) > 0. )
          END IF

          IF( GCALC ) THEN
            SELECT CASE( JLOOP )
              CASE( 2 ) ! cloud water over sea
                IF ( CCLOUD == 'LIMA' ) THEN
                  ZA = XAC_L
                  ZB = XBC_L
                  ZCC = ZCCC(JK) * ZRHODREFZ(JK)
                  ZCX = 0.
                  ZALPHA = XALPHAC_L
                  ZNU = XNUC_L
                  ZLBEX = 1.0 / ( ZCX - ZB )
                  ZLB =( ZA * ZCC * MOMG( ZALPHA, ZNU, ZB ) ) ** ( -ZLBEX )
                ELSE
                  ZA = XAC_I
                  ZB = XBC_I
                  ZCC = XCONC_SEA
                  ZCX = 0.
                  ZALPHA = XALPHAC2_I
                  ZNU = XNUC2_I
                  ZLBEX = 1.0 / (ZCX - ZB )
                  ZLB =  ( ZA * ZCC * MOMG( ZALPHA, ZNU, ZB ) ) ** ( -ZLBEX )
                END IF

              CASE( 3 ) ! rain water
                IF ( CCLOUD == 'LIMA' ) THEN
                  ZA = XAR_L
                  ZB = XBR_L
                  ZCC = ZCCR(JK) * ZRHODREFZ(JK)
                  ZCX = 0.
                  ZALPHA = XALPHAR_L
                  ZNU = XNUR_L
                  ZLBEX = 1.0 / ( ZCX - ZB )
                  ZLB = ( ZA * ZCC * MOMG( ZALPHA, ZNU, ZB ) ) ** ( -ZLBEX )
                ELSE
                  ZA = XAR_I
                  ZB = XBR_I
                  ZCC = XCCR_I
                  ZCX = -1.
                  ZALPHA = XALPHAR_I
                  ZNU = XNUR_I
                  ZLB = XLBR_I
                  ZLBEX = XLBEXR_I
                END IF

              CASE( 4 ) ! pristine ice
                IF ( CCLOUD == 'LIMA' ) THEN
                  ZA = XAI_L
                  ZB = XBI_L
                  ZCC = ZCCI(JK) * ZRHODREFZ(JK)
                  ZCX = 0.
                  ZALPHA = XALPHAI_L
                  ZNU = XNUI_L
                  ZLBEX = 1.0 / ( ZCX - ZB )
                  ZLB = ( ZA * ZCC * MOMG( ZALPHA, ZNU, ZB ) ) ** ( -ZLBEX ) ! because zcc not included in xlbi
                  ZFW = 0.
                ELSE
                  ZA = XAI_I
                  ZB = XBI_I
                  ZCC = ZCIT(JK)
                  ZCX = 0.
                  ZALPHA = XALPHAI_I
                  ZNU = XNUI_I
                  ZLBEX = XLBEXI_I
                  ZLB = XLBI_I * ZCC ** ( -ZLBEX ) ! because zcc not included in xlbi
                  ZFW = 0.
                END IF

              CASE( 5 ) ! snow
                IF ( CCLOUD == 'LIMA' ) THEN
                  ZA = XAS_L
                  ZB = XBS_L
                  ZCC = XCCS_L
                  ZCX = XCXS_L
                  ZALPHA = XALPHAS_L
                  ZNU = XNUS_L
                  ZNS = XNS_L
                  ZLB = XLBS_L
                  ZLBEX = XLBEXS_L
                  ZFW = 0.
                ELSE
                  ZA = XAS_I
                  ZB = XBS_I
                  ZCC = XCCS_I
                  ZCX = XCXS_I
                  ZALPHA = XALPHAS_I
                  ZNU = XNUS_I
                  ZNS = XNS_I
                  ZLB = XLBS_I
                  ZLBEX = XLBEXS_I
                  ZFW = 0.
                END IF

              CASE( 6 ) ! graupel
                !if temperature between -10 and 10 C and Mr and Mg over min threshold: melting graupel
                ! with liquid water fraction Fw=Mr/(Mr+Mg) else dry graupel (Fw=0)
                IF( ZTEMPZ(JK) > XTT-10. .AND. ZTEMPZ(JK) < XTT+10. .AND. ZRZ(JK,3) > ZRTMIN(3) ) THEN
                  ZFW = ZRZ(JK,3) / ( ZRZ(JK,3) + ZRZ(JK,JLOOP) )
                ELSE
                  ZFW = 0.
                END IF
                IF ( CCLOUD == 'LIMA' ) THEN
                  ZA = XAG_L
                  ZB = XBG_L
                  ZCC = XCCG_L
                  ZCX = XCXG_L
                  ZALPHA = XALPHAG_L
                  ZNU = XNUG_L
                  ZLB = XLBG_L
                  ZLBEX = XLBEXG_L
                ELSE
                  ZA = XAG_I
                  ZB = XBG_I
                  ZCC = XCCG_I
                  ZCX = XCXG_I
                  ZALPHA = XALPHAG_I
                  ZNU = XNUG_I
                  ZLB = XLBG_I
                  ZLBEX = XLBEXG_I
                END IF

              CASE( 7 ) ! cloud water over land
                IF ( CCLOUD == 'LIMA' ) THEN
                  ZA = XAC_L
                  ZB = XBC_L
                  ZCC = ZCCC(JK) * ZRHODREFZ(JK)
                  ZCX = 0.
                  ZALPHA = XALPHAC_L
                  ZNU = XNUC_L
                  ZLBEX = 1.0 / ( ZCX - ZB )
                  ZLB = ( ZA * ZCC * MOMG( ZALPHA, ZNU, ZB ) ) ** ( -ZLBEX )
                ELSE
                  ZA = XAC_I
                  ZB = XBC_I
                  ZCC = XCONC_LAND
                  ZCX = 0.
                  ZALPHA = XALPHAC_I
                  ZNU = XNUC_I
                  ZLBEX = 1.0 / ( ZCX - ZB )
                  ZLB = ( ZA * ZCC * MOMG( ZALPHA, ZNU, ZB ) ) ** ( -ZLBEX )
                END IF

            END SELECT

            IF ( JLOOP ==  5 .AND. CCLOUD == 'LIMA' .AND. LSNOW_T_L ) THEN
              IF ( ZTEMPZ(JK) > XTT-10. ) THEN
                ZLBDA = MAX( MIN( XLBDAS_MAX_L, 10. ** ( 14.554 - 0.0423 * ZTEMPZ(JK) ) ), XLBDAS_MIN_L ) * XTRANS_MP_GAMMAS_L
              ELSE
                ZLBDA = MAX( MIN( XLBDAS_MAX_L, 10. ** (  6.226 - 0.0106 * ZTEMPZ(JK) ) ), XLBDAS_MIN_L ) * XTRANS_MP_GAMMAS_L
              END IF
              ZN = ZNS * ZRHODREFZ(JK) * ZRZ(JK, JLOOP) * ZLBDA ** ZB
            ELSE IF ( JLOOP ==  5 .AND. CCLOUD =='ICE3' .AND. LSNOW_T_I ) THEN
              IF ( ZTEMPZ(JK) > XTT-10. ) THEN
                ZLBDA = MAX( MIN( XLBDAS_MAX_I, 10. ** ( 14.554 - 0.0423 * ZTEMPZ(JK) ) ), XLBDAS_MIN_I ) * XTRANS_MP_GAMMAS_I
              ELSE
                ZLBDA = MAX( MIN( XLBDAS_MAX_I, 10 ** (   6.226 - 0.0106 * ZTEMPZ(JK) ) ), XLBDAS_MIN_I ) * XTRANS_MP_GAMMAS_I
              END IF
              ZN = ZNS * ZRHODREFZ(JK) * ZRZ(JK, JLOOP) * ZLBDA ** ZB
            ELSE
              ZLBDA = ZLB * ( ZRHODREFZ(JK) * ZRZ(JK,JLOOP) ) ** ZLBEX
              ZN = ZCC * ZLBDA ** ZCX
            END IF

            ZREFLOC = 0.
            ZAETMP  = 0.
            DO JJ = 1, JPTS_GAULAG ! Gauss-Laguerre quadrature
              ZDELTA_EQUIV = ZX(JJ) ** ( 1. / ZALPHA ) / ZLBDA
              SELECT CASE( JLOOP )
                CASE( 2, 3, 7 )
                  QM = QMW
                CASE( 4, 5, 6 )
                  ! pristine ice, snow, dry graupel
                  ZRHOHYD = MIN( 6. * ZA * ZDELTA_EQUIV ** ( ZB - 3. ) / XPI, .92 * XRHOLW )
                  QM = SQRT( MG( QMI ** 2, CMPLX(1,0), ZRHOHYD / .92 / XRHOLW ) )

                  ! water inclusions in ice in air
                  QEPSWI = MG( QMW ** 2, QM ** 2, ZFW )
                  ! ice in air inclusions in water
                  QEPSIW = MG( QM ** 2, QMW ** 2, 1. - ZFW )

                  !MG weighted rule (Matrosov 2008)
                  IF( ZFW < 0.37 ) THEN
                    ZFPW = 0.
                  ELSE IF( ZFW > 0.63 ) THEN
                    ZFPW = 1.
                  ELSE
                    ZFPW = ( ZFW - 0.37 ) / ( 0.63 - 0.37 )
                  END IF
                  QM = SQRT( QEPSWI * ( 1. - ZFPW ) + QEPSIW * ZFPW )
              END SELECT
              CALL BHMIE( XPI / XLAM_CRAD * ZDELTA_EQUIV, QM, ZQEXT, ZQSCA, ZQBACK )
              ZREFLOC = ZREFLOC + ZQBACK * ZX(JJ) ** ( ZNU - 1. ) * ZDELTA_EQUIV ** 2 * ZW(JJ)
              ZAETMP  = ZAETMP  + ZQEXT  * ZX(JJ) ** ( ZNU - 1. ) * ZDELTA_EQUIV ** 2 * ZW(JJ)
            END DO
            ZREFLOC = ZREFLOC * ( XLAM_CRAD / XPI ) ** 4 * ZN / ( 4. * GAMMA( ZNU ) * .93 )
            ZAETMP  = ZAETMP  *               XPI        * ZN / ( 4. * GAMMA( ZNU ) )
            TPSENSOR%XCRARE(JK, KSTORE_ID) = TPSENSOR%XCRARE(JK, KSTORE_ID) + ZREFLOC
            ZAELOC(JK) = ZAELOC(JK) + ZAETMP
          END IF
        END DO
      END DO

      ! apply attenuation
      ! nadir
      ZAETOT = 1.
      ICOUNT = COUNT( TPSENSOR%XZ_CUR >= PZMZ(:) )
      DO JK = ICOUNT, 1, -1
        IF( JK == ICOUNT ) THEN
          IF( TPSENSOR%XZ_CUR <= PZMZ(JK) + .5 * ( PZMZ(JK+1) -PZMZ(JK) ) ) THEN
            ! only attenuation from zaeloc(jk)
            ZAETOT = ZAETOT * EXP( -2. * ( ZAELOC(JK) * ( TPSENSOR%XZ_CUR - PZMZ(JK) ) ) )
          ELSE
            ! attenuation from zaeloc(jk) and zaeloc(jk+1)
            ZAETOT = ZAETOT * EXP( -2. * ( ZAELOC(JK+1) * (TPSENSOR%XZ_CUR - .5 * ( PZMZ(JK+1) + PZMZ(JK) ) ) &
                     + ZAELOC(JK) * .5 * ( PZMZ(JK+1) - PZMZ(JK) ) ) )
          END IF
        ELSE
          ! attenuation from zaeloc(jk) and zaeloc(jk+1)
          ZAETOT = ZAETOT * EXP( - ( ZAELOC(JK+1) + ZAELOC(JK) ) * ( PZMZ(JK+1) - PZMZ(JK) ) )
        END IF
        TPSENSOR%XCRARE_ATT(JK,KSTORE_ID) = TPSENSOR%XCRARE(JK,KSTORE_ID) * ZAETOT
      END DO

      ! zenith
      ZAETOT = 1.
      ICOUNT = MAX( COUNT( TPSENSOR%XZ_CUR >= PZMZ(:) ), 1 ) + 1
      DO JK = ICOUNT, IKU
        IF ( JK == ICOUNT ) THEN
          IF( TPSENSOR%XZ_CUR >= PZMZ(JK) - .5 * ( PZMZ(JK) - PZMZ(JK-1) ) ) THEN
            ! only attenuation from zaeloc(jk)
            ZAETOT = ZAETOT * EXP( -2. * ( ZAELOC(JK) * ( PZMZ(JK) - TPSENSOR%XZ_CUR) ) )
          ELSE
            ! attenuation from zaeloc(jk) and zaeloc(jk-1)
            ZAETOT = ZAETOT * EXP( -2. * ( ZAELOC(JK-1) * ( .5 * ( PZMZ(JK) + PZMZ(JK-1) ) - TPSENSOR%XZ_CUR ) &
                     + ZAELOC(JK) * .5 * ( PZMZ(JK) - PZMZ(JK-1) ) ) )
          END IF
        ELSE
          ! attenuation from zaeloc(jk) and zaeloc(jk-1)
          ZAETOT = ZAETOT * EXP ( - ( ZAELOC(JK-1) + ZAELOC(JK) ) * ( PZMZ(JK) - PZMZ(JK-1) ) )
        END IF
        TPSENSOR%XCRARE_ATT(JK,KSTORE_ID) = TPSENSOR%XCRARE(JK,KSTORE_ID) * ZAETOT
      END DO

      ! m^3 → mm^6/m^3 → dbz
      WHERE( TPSENSOR%XCRARE(:, KSTORE_ID) > 0 )
        TPSENSOR%XCRARE(:, KSTORE_ID) = 10. * LOG10( 1.E18 * TPSENSOR%XCRARE(:, KSTORE_ID) )
      ELSEWHERE
        TPSENSOR%XCRARE(:, KSTORE_ID) = XUNDEF
      END WHERE
      WHERE( TPSENSOR%XCRARE_ATT(:, KSTORE_ID) > 0 )
        TPSENSOR%XCRARE_ATT(:, KSTORE_ID) = 10. * LOG10( 1.E18 * TPSENSOR%XCRARE_ATT(:, KSTORE_ID) )
      ELSEWHERE
        TPSENSOR%XCRARE_ATT(:, KSTORE_ID ) = XUNDEF
      END WHERE
    END IF ! end LIMA / ICE3

  END SUBROUTINE SENSOR_RARE_COMPUTE


  SUBROUTINE SENSOR_DIACHRO_POINT_ADD( HTITLEIN, HCOMMENTIN, HUNITSIN, PFIELDIN, KPROC_CUR, KMAXPROC, &
                                       HTITLES,  HCOMMENTS,  HUNITS,   PDATA )
    USE MODE_MSG

    CHARACTER(LEN=*),                         INTENT(IN)    :: HTITLEIN
    CHARACTER(LEN=*),                         INTENT(IN)    :: HCOMMENTIN
    CHARACTER(LEN=*),                         INTENT(IN)    :: HUNITSIN
    REAL, DIMENSION(:),                       INTENT(IN)    :: PFIELDIN
    INTEGER,                                  INTENT(INOUT) :: KPROC_CUR
    INTEGER,                                  INTENT(IN)    :: KMAXPROC
    CHARACTER(LEN=*), DIMENSION(:),           INTENT(INOUT) :: HTITLES
    CHARACTER(LEN=*), DIMENSION(:),           INTENT(INOUT) :: HCOMMENTS
    CHARACTER(LEN=*), DIMENSION(:),           INTENT(INOUT) :: HUNITS
    REAL,             DIMENSION(:,:,:,:,:,:), INTENT(INOUT) :: PDATA

    KPROC_CUR = KPROC_CUR + 1

    IF ( KPROC_CUR > KMAXPROC ) CALL PRINT_MSG( NVERB_FATAL, 'IO', 'Sensor_diachro_point_add', 'more processes than expected' )

    HTITLES(KPROC_CUR)   = TRIM( HTITLEIN)
    IF ( LEN_TRIM( HTITLEIN ) > LEN( HTITLES(KPROC_CUR) ) )                                           &
      CALL PRINT_MSG( NVERB_WARNING, 'IO', 'Sensor_diachro_point_add',                                &
                      'title was truncated to ' // HTITLES(KPROC_CUR) // ' from ' // TRIM( HTITLEIN ) )

    HCOMMENTS(KPROC_CUR) = TRIM( HCOMMENTIN )
    IF ( LEN_TRIM( HCOMMENTIN ) > LEN( HCOMMENTS(KPROC_CUR) ) )                                             &
      CALL PRINT_MSG( NVERB_WARNING, 'IO', 'Sensor_diachro_point_add',                                      &
                      'comment was truncated to ' // HCOMMENTS(KPROC_CUR) // ' from ' // TRIM( HCOMMENTIN ) )

    HUNITS(KPROC_CUR)    = TRIM( HUNITSIN )
    IF ( LEN_TRIM( HUNITSIN ) > LEN( HUNITS(KPROC_CUR) ) )                                           &
      CALL PRINT_MSG( NVERB_WARNING, 'IO', 'Sensor_diachro_point_add',                               &
                      'units was truncated to ' // HUNITS(KPROC_CUR) // ' from ' // TRIM( HUNITSIN ) )

    PDATA(1, 1, 1, :, 1, KPROC_CUR) = PFIELDIN(:)

  END SUBROUTINE SENSOR_DIACHRO_POINT_ADD


  SUBROUTINE SENSOR_DIACHRO_PROFILE_ADD( HTITLEIN, HCOMMENTIN, HUNITSIN, PFIELDIN, KPROC_CUR, KMAXPROC, &
                                         HTITLES,  HCOMMENTS,  HUNITS,   PDATA )
    USE MODE_MSG

    CHARACTER(LEN=*),                         INTENT(IN)    :: HTITLEIN
    CHARACTER(LEN=*),                         INTENT(IN)    :: HCOMMENTIN
    CHARACTER(LEN=*),                         INTENT(IN)    :: HUNITSIN
    REAL, DIMENSION(:,:),                     INTENT(IN)    :: PFIELDIN
    INTEGER,                                  INTENT(INOUT) :: KPROC_CUR
    INTEGER,                                  INTENT(IN)    :: KMAXPROC
    CHARACTER(LEN=*), DIMENSION(:),           INTENT(INOUT) :: HTITLES
    CHARACTER(LEN=*), DIMENSION(:),           INTENT(INOUT) :: HCOMMENTS
    CHARACTER(LEN=*), DIMENSION(:),           INTENT(INOUT) :: HUNITS
    REAL,             DIMENSION(:,:,:,:,:,:), INTENT(INOUT) :: PDATA

    KPROC_CUR = KPROC_CUR + 1

    IF ( KPROC_CUR > KMAXPROC ) CALL PRINT_MSG( NVERB_FATAL, 'IO', 'Sensor_diachro_profile_add', 'more processes than expected' )

    HTITLES(KPROC_CUR)   = TRIM( HTITLEIN)
    IF ( LEN_TRIM( HTITLEIN ) > LEN( HTITLES(KPROC_CUR) ) )                                           &
      CALL PRINT_MSG( NVERB_WARNING, 'IO', 'Sensor_diachro_profile_add',                              &
                      'title was truncated to ' // HTITLES(KPROC_CUR) // ' from ' // TRIM( HTITLEIN ) )

    HCOMMENTS(KPROC_CUR) = TRIM( HCOMMENTIN )
    IF ( LEN_TRIM( HCOMMENTIN ) > LEN( HCOMMENTS(KPROC_CUR) ) )                                             &
      CALL PRINT_MSG( NVERB_WARNING, 'IO', 'Sensor_diachro_profile_add',                                    &
                      'comment was truncated to ' // HCOMMENTS(KPROC_CUR) // ' from ' // TRIM( HCOMMENTIN ) )

    HUNITS(KPROC_CUR)    = TRIM( HUNITSIN )
    IF ( LEN_TRIM( HUNITSIN ) > LEN( HUNITS(KPROC_CUR) ) )                                           &
      CALL PRINT_MSG( NVERB_WARNING, 'IO', 'Sensor_diachro_profile_add',                             &
                      'units was truncated to ' // HUNITS(KPROC_CUR) // ' from ' // TRIM( HUNITSIN ) )

    PDATA(1, 1, :, :, 1, KPROC_CUR) = PFIELDIN(:, :)

  END SUBROUTINE SENSOR_DIACHRO_PROFILE_ADD
END MODULE MODE_SENSOR
