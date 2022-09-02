!MNH_LIC Copyright 2000-2022 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!      ##########################
MODULE MODI_AIRCRAFT_BALLOON_EVOL
!      ##########################
!
INTERFACE
!
      SUBROUTINE AIRCRAFT_BALLOON_EVOL(PTSTEP,               &
                       PXHAT, PYHAT, PZ,                     &
                       PMAP, PLONOR, PLATOR,                 &
                       PU, PV, PW, PP, PTH, PR, PSV, PTKE,   &
                       PTS, PRHODREF, PCIT,TPFLYER, PSEA     )
!
USE MODD_AIRCRAFT_BALLOON
!
REAL,                     INTENT(IN)     :: PTSTEP ! time step
REAL, DIMENSION(:),       INTENT(IN)     :: PXHAT  ! x coordinate
REAL, DIMENSION(:),       INTENT(IN)     :: PYHAT  ! y coordinate
REAL, DIMENSION(:,:,:),   INTENT(IN)     :: PZ     ! z array
REAL, DIMENSION(:,:),     INTENT(IN)     :: PMAP   ! map factor
REAL,                     INTENT(IN)     :: PLONOR ! origine longitude
REAL,                     INTENT(IN)     :: PLATOR ! origine latitude
REAL, DIMENSION(:,:,:),   INTENT(IN)     :: PU     ! horizontal wind X component
REAL, DIMENSION(:,:,:),   INTENT(IN)     :: PV     ! horizontal wind Y component
REAL, DIMENSION(:,:,:),   INTENT(IN)     :: PW     ! vertical wind
REAL, DIMENSION(:,:,:),   INTENT(IN)     :: PP     ! pressure
REAL, DIMENSION(:,:,:),   INTENT(IN)     :: PTH    ! potential temperature
REAL, DIMENSION(:,:,:,:), INTENT(IN)     :: PR     ! water mixing ratios
REAL, DIMENSION(:,:,:,:), INTENT(IN)     :: PSV    ! Scalar variables
REAL, DIMENSION(:,:,:),   INTENT(IN)     :: PTKE   ! turbulent kinetic energy
REAL, DIMENSION(:,:),     INTENT(IN)     :: PTS    ! surface temperature
REAL, DIMENSION(:,:,:),   INTENT(IN)     :: PRHODREF ! dry air density of the reference state
REAL, DIMENSION(:,:,:),   INTENT(IN)     :: PCIT     ! pristine ice concentration
!
CLASS(TFLYERDATA),        INTENT(INOUT)  :: TPFLYER! balloon/aircraft
REAL, DIMENSION(:,:), OPTIONAL, INTENT(IN) :: PSEA
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE AIRCRAFT_BALLOON_EVOL
!
END INTERFACE
!
END MODULE MODI_AIRCRAFT_BALLOON_EVOL
!
!     ########################################################
      SUBROUTINE AIRCRAFT_BALLOON_EVOL(PTSTEP,               &
                       PXHAT, PYHAT, PZ,                     &
                       PMAP, PLONOR, PLATOR,                 &
                       PU, PV, PW, PP, PTH, PR, PSV, PTKE,   &
                       PTS, PRHODREF, PCIT,TPFLYER, PSEA     )
!     ########################################################
!
!
!!****  *AIRCRAFT_BALLOON_EVOL* - (advects and) stores 
!!                                balloons/aircrafts in the model
!!
!!    PURPOSE
!!    -------
!
!
!!**  METHOD
!!    ------
!!    
!! 1) All the balloons are tested. If the current balloon is
!!     a) in the current model
!!     b) not crashed
!!   the following computations are done.
!!
!! 2) The balloon position is computed.
!!       Interpolations at balloon positions are performed according to mass
!! points (because density is computed here for iso-density balloons).
!! Therefore, all model variables are used at mass points. Shuman averaging
!! are performed on X, Y, Z, U, V, W.
!!
!! 3) Storage of balloon data
!!       If storage is asked for this time-step, the data are recorded in the
!! balloon time-series.
!!
!! 4) Balloon advection
!!       If the balloon is launched, it is advected according its type
!!    a) iso-density balloons are advected following horizontal wind.
!!          the slope of the iso-density surfaces is neglected.
!!    b) radio-sounding balloons are advected according to all wind velocities.
!!          the vertical ascent speed is added to the vertical wind speed.  
!!    c) Constant Volume balloons are advected according to all wind velocities.
!!          the vertical ascent speed is computed using the balloon equation
!!  
!!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!      Valery Masson             * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!     Original 15/05/2000
!!     Apr,19, 2001 (G.Jaubert) add CVBALL type
!!     March, 2008 (P.Lacarrere) Add 3D fluxes
!!     Dec,12, 2008 (M. Leriche) move ZTDIST out from if.not.(tpflyer%fly)
!!     Dec,15, 2008 (V. Masson) correct do while aircraft move
!!     March, 2013 (O.Caumont) add radar reflectivities
!!     April, 2014 (C.Lac) allow RARE calculation only if CCLOUD=ICE3
!!     May, 2014 (O.Caumont) modify RARE for hydrometeors containing ice
!!                           add bright band calculation for RARE
!!     Feb, 2015 (C.Lac) Correction to prevent aircraft crash 
!!     July, 2015 (O.Nuissier/F.Duffourg) Add microphysics diagnostic for
!!                                      aircraft, ballon and profiler
!!      October, 2016 (G.DELAUTIER) LIMA
!!     March,28, 2018 (P. Wautelet) replace TEMPORAL_DIST by DATETIME_DISTANCE
!!  Philippe Wautelet: 05/2016-04/2018: new data structures and calls for I/O
!  P. Wautelet 13/09/2019: budget: simplify and modernize date/time management
!  P. Wautelet 01/10/2020: bugfix: initialize GSTORE
!  P. Wautelet 14/01/2021: bugfixes: -ZXCOEF and ZYCOEF were not computed if CVBALL
!                                    -PCIT was used if CCLOUD/=ICEx (not allocated)
!                                    -PSEA was always used even if not allocated (CSURF/=EXTE)
!                                    -do not use PMAP if cartesian domain
!  P. Wautelet    06/2022: reorganize flyers
!! --------------------------------------------------------------------------
!       
!*      0. DECLARATIONS
!          ------------
!
USE MODD_AIRCRAFT_BALLOON
USE MODD_CONF
USE MODD_CST
USE MODD_DIAG_IN_RUN
USE MODD_GRID
USE MODD_GRID_n,           ONLY: XXHATM, XYHATM
USE MODD_LUNIT_n,          ONLY: TLUOUT
USE MODD_NESTING
USE MODD_NSV,              ONLY : NSV_LIMA_NI,NSV_LIMA_NR,NSV_LIMA_NC
USE MODD_PARAMETERS
USE MODD_PARAM_LIMA,       ONLY: XALPHAR_L=>XALPHAR,XNUR_L=>XNUR,XALPHAS_L=>XALPHAS,XNUS_L=>XNUS,&
                                 XALPHAG_L=>XALPHAG,XNUG_L=>XNUG, XALPHAI_L=>XALPHAI,XNUI_L=>XNUI,&
                                 XRTMIN_L=>XRTMIN,XALPHAC_L=>XALPHAC,XNUC_L=>XNUC
USE MODD_PARAM_LIMA_COLD,  ONLY: XDI_L=>XDI,XLBEXI_L=>XLBEXI,XLBI_L=>XLBI,XAI_L=>XAI,XBI_L=>XBI,XC_I_L=>XC_I,&
                                 XLBEXS_L=>XLBEXS,XLBS_L=>XLBS,XCCS_L=>XCCS,&
                                 XAS_L=>XAS,XBS_L=>XBS,XCXS_L=>XCXS
USE MODD_PARAM_LIMA_MIXED, ONLY: XDG_L=>XDG,XLBEXG_L=>XLBEXG,XLBG_L=>XLBG,XCCG_L=>XCCG,&
                                 XAG_L=>XAG,XBG_L=>XBG,XCXG_L=>XCXG,XCG_L=>XCG
USE MODD_PARAM_LIMA_WARM,  ONLY: XLBEXR_L=>XLBEXR,XLBR_L=>XLBR,XBR_L=>XBR,XAR_L=>XAR,&
                                 XBC_L=>XBC,XAC_L=>XAC
USE MODD_PARAM_n,          ONLY: CCLOUD, CSURF
USE MODD_RAIN_ICE_DESCR,   ONLY: XALPHAR_I=>XALPHAR,XNUR_I=>XNUR,XLBEXR_I=>XLBEXR,&
                                 XLBR_I=>XLBR,XCCR_I=>XCCR,XBR_I=>XBR,XAR_I=>XAR,&
                                 XALPHAC_I=>XALPHAC,XNUC_I=>XNUC,&
                                 XLBC_I=>XLBC,XBC_I=>XBC,XAC_I=>XAC,&
                                 XALPHAC2_I=>XALPHAC2,XNUC2_I=>XNUC2,&
                                 XALPHAS_I=>XALPHAS,XNUS_I=>XNUS,XLBEXS_I=>XLBEXS,&
                                 XLBS_I=>XLBS,XCCS_I=>XCCS,XAS_I=>XAS,XBS_I=>XBS,XCXS_I=>XCXS,&
                                 XALPHAG_I=>XALPHAG,XNUG_I=>XNUG,XDG_I=>XDG,XLBEXG_I=>XLBEXG,&
                                 XLBG_I=>XLBG,XCCG_I=>XCCG,XAG_I=>XAG,XBG_I=>XBG,XCXG_I=>XCXG,XCG_I=>XCG,&
                                 XALPHAI_I=>XALPHAI,XNUI_I=>XNUI,XDI_I=>XDI,XLBEXI_I=>XLBEXI,&
                                 XLBI_I=>XLBI,XAI_I=>XAI,XBI_I=>XBI,XC_I_I=>XC_I,&
                                 XRTMIN_I=>XRTMIN,XCONC_LAND,XCONC_SEA
USE MODD_REF_n,            ONLY: XRHODREF
USE MODD_TIME,             only: tdtexp
USE MODD_TIME_n,           only: tdtcur
USE MODD_TURB_FLUX_AIRCRAFT_BALLOON
!
USE MODE_DATETIME
USE MODE_FGAU,             ONLY: GAULAG
USE MODE_FSCATTER,         ONLY: QEPSW,QEPSI,BHMIE,MOMG,MG
USE MODE_GRIDPROJ
USE MODE_ll
USE MODE_MSG
USE MODE_STATPROF_TOOLS,   ONLY: STATPROF_INSTANT
!
USE MODI_GAMMA,            ONLY: GAMMA
USE MODI_WATER_SUM
!
IMPLICIT NONE
!
!
!*      0.1  declarations of arguments
!
!
REAL,                     INTENT(IN)     :: PTSTEP ! time step
REAL, DIMENSION(:),       INTENT(IN)     :: PXHAT  ! x coordinate
REAL, DIMENSION(:),       INTENT(IN)     :: PYHAT  ! y coordinate
REAL, DIMENSION(:,:,:),   INTENT(IN)     :: PZ     ! z array
REAL, DIMENSION(:,:),     INTENT(IN)     :: PMAP   ! map factor
REAL,                     INTENT(IN)     :: PLONOR ! origine longitude
REAL,                     INTENT(IN)     :: PLATOR ! origine latitude
REAL, DIMENSION(:,:,:),   INTENT(IN)     :: PU     ! horizontal wind X component
REAL, DIMENSION(:,:,:),   INTENT(IN)     :: PV     ! horizontal wind Y component
REAL, DIMENSION(:,:,:),   INTENT(IN)     :: PW     ! vertical wind
REAL, DIMENSION(:,:,:),   INTENT(IN)     :: PP     ! pressure
REAL, DIMENSION(:,:,:),   INTENT(IN)     :: PTH    ! potential temperature
REAL, DIMENSION(:,:,:,:), INTENT(IN)     :: PR     ! water mixing ratios
REAL, DIMENSION(:,:,:,:), INTENT(IN)     :: PSV    ! Scalar variables
REAL, DIMENSION(:,:,:),   INTENT(IN)     :: PTKE   ! turbulent kinetic energy
REAL, DIMENSION(:,:),     INTENT(IN)     :: PTS    ! surface temperature
REAL, DIMENSION(:,:,:),   INTENT(IN)     :: PRHODREF ! dry air density of the reference state
REAL, DIMENSION(:,:,:),   INTENT(IN)     :: PCIT     ! pristine ice concentration
!
CLASS(TFLYERDATA),        INTENT(INOUT)  :: TPFLYER! balloon/aircraft
REAL, DIMENSION(:,:),     INTENT(IN)     :: PSEA
!
!-------------------------------------------------------------------------------
!
!       0.2  declaration of local variables
!
!
INTEGER :: IMI        ! model index
REAL    :: ZTHIS_PROC ! 1 if balloon is currently treated by this proc., else 0
!
INTEGER :: IIB        ! current processor domain sizes
INTEGER :: IJB
INTEGER :: IIE
INTEGER :: IJE
INTEGER :: IIU
INTEGER :: IJU
INTEGER :: IKB
INTEGER :: IKE
INTEGER :: IKU
!
INTEGER :: JK         ! loop index
!
REAL, DIMENSION(2,2,SIZE(PZ,3))     :: ZZM    ! mass point coordinates
REAL, DIMENSION(2,2,SIZE(PZ,3))     :: ZZU    ! U points z coordinates
REAL, DIMENSION(2,2,SIZE(PZ,3))     :: ZZV    ! V points z coordinates
REAL, DIMENSION(2,2,SIZE(PZ,3))     :: ZWM    ! mass point wind
!
REAL, DIMENSION(2,2,SIZE(PTH,3))    :: ZTHV   ! virtual potential temperature
REAL, DIMENSION(2,2,SIZE(PTH,3))    :: ZTV    ! virtual temperature
REAL, DIMENSION(2,2,SIZE(PTH,3))    :: ZTEMP  ! temperature
REAL, DIMENSION(2,2,SIZE(PTH,3))    :: ZEXN   ! Exner function
REAL, DIMENSION(2,2,SIZE(PTH,3))    :: ZRHO   ! air density
REAL                                :: ZFLYER_EXN ! balloon/aircraft Exner func.
REAL, DIMENSION(2,2,SIZE(PTH,3))    :: ZTHW_FLUX  !       
REAL, DIMENSION(2,2,SIZE(PTH,3))    :: ZRCW_FLUX  !
REAL, DIMENSION(2,2,SIZE(PSV,3),SIZE(PSV,4))    :: ZSVW_FLUX
!
REAL    :: ZTDIST   ! time until launch (sec)
LOGICAL :: GLAUNCH  ! launch/takeoff is effective at this time-step (if true)
LOGICAL :: GSTORE   ! storage occurs at this time step
!
INTEGER :: II       ! mass balloon position (x index)
INTEGER :: IJ       ! mass balloon position (y index)
INTEGER :: IU       ! U flux point balloon position (x index)
INTEGER :: IV       ! V flux point balloon position (y index)
INTEGER :: IDU      ! difference between IU and II
INTEGER :: IDV      ! difference between IV and IJ
!
INTEGER :: IK00     ! balloon position for II  , IJ
INTEGER :: IK01     ! balloon position for II  , IJ+1
INTEGER :: IK10     ! balloon position for II+1, IJ
INTEGER :: IK11     ! balloon position for II+1, IJ+1
INTEGER :: IU00     ! balloon position for IU  , IJ
INTEGER :: IU01     ! balloon position for IU  , IJ+1
INTEGER :: IU10     ! balloon position for IU+1, IJ
INTEGER :: IU11     ! balloon position for IU+1, IJ+1
INTEGER :: IV00     ! balloon position for II  , IV
INTEGER :: IV01     ! balloon position for II  , IV+1
INTEGER :: IV10     ! balloon position for II+1, IV
INTEGER :: IV11     ! balloon position for II+1, IV+1
!
REAL :: ZXCOEF      ! X direction interpolation coefficient
REAL :: ZUCOEF      ! X direction interpolation coefficient (for U)
REAL :: ZYCOEF      ! Y direction interpolation coefficient
REAL :: ZVCOEF      ! Y direction interpolation coefficient (for V)
!
REAL :: ZZCOEF00    ! Z direction interpolation coefficient for II  , IJ
REAL :: ZZCOEF01    ! Z direction interpolation coefficient for II  , IJ+1
REAL :: ZZCOEF10    ! Z direction interpolation coefficient for II+1, IJ
REAL :: ZZCOEF11    ! Z direction interpolation coefficient for II+1, IJ+1
REAL :: ZUCOEF00    ! Z direction interpolation coefficient for IU  , IJ
REAL :: ZUCOEF01    ! Z direction interpolation coefficient for IU  , IJ+1
REAL :: ZUCOEF10    ! Z direction interpolation coefficient for IU+1, IJ
REAL :: ZUCOEF11    ! Z direction interpolation coefficient for IU+1, IJ+1
REAL :: ZVCOEF00    ! Z direction interpolation coefficient for II  , IV
REAL :: ZVCOEF01    ! Z direction interpolation coefficient for II  , IV+1
REAL :: ZVCOEF10    ! Z direction interpolation coefficient for II+1, IV
REAL :: ZVCOEF11    ! Z direction interpolation coefficient for II+1, IV+1
!
INTEGER :: IN       ! time index
INTEGER :: JLOOP,JLOOP2    ! loop counter
!
REAL    :: ZU_BAL   ! horizontal wind speed at balloon location (along x)
REAL    :: ZV_BAL   ! horizontal wind speed at balloon location (along y)
REAL    :: ZW_BAL   ! vertical   wind speed at balloon location (along z)
REAL    :: ZMAP     ! map factor at balloon location
REAL    :: ZGAM     ! rotation between meso-nh base and spherical lat-lon base.
INTEGER :: IL       ! flight segment index
REAL    :: ZSEG_FRAC! fraction of flight in the current segment
REAL    :: ZRO_BAL  ! air density at balloon location
!
INTEGER :: IINFO_ll ! return code
INTEGER :: ILUOUT   ! logical unit
INTEGER :: IRESP    ! return code
!
! specific to cloud radar
REAL, DIMENSION(SIZE(PR,3))    :: ZTEMPZ! vertical profile of temperature
REAL, DIMENSION(SIZE(PR,3))    :: ZRHODREFZ ! vertical profile of dry air density of the reference state
REAL, DIMENSION(SIZE(PR,3))    :: ZCIT     ! pristine ice concentration
REAL, DIMENSION(SIZE(PR,3))    :: ZCCI,ZCCR,ZCCC     ! ICE,RAIN CLOUD concentration (LIMA)
REAL, DIMENSION(SIZE(PR,1),SIZE(PR,2),SIZE(PR,3))    :: ZR   
REAL, DIMENSION(SIZE(PR,3),SIZE(PR,4)+1) :: ZRZ  ! vertical profile of hydrometeor mixing ratios
REAL                           :: ZA,ZB,ZCC,ZCX,ZALPHA,ZNU,ZLB,ZLBEX,ZRHOHYD   ! generic microphysical parameters
INTEGER                        :: JJ    ! loop counter for quadrature
COMPLEX                        :: QMW,QMI,QM,QB,QEPSIW,QEPSWI   ! dielectric parameter
REAL                           :: ZAETOT,ZAETMP,ZREFLOC,ZQSCA,ZQBACK,ZQEXT ! temporary scattering parameters
REAL,DIMENSION(:),ALLOCATABLE  :: ZAELOC,ZZMZ ! temporary arrays
INTEGER                        :: JPTS_GAULAG=7 ! number of points for Gauss-Laguerre quadrature
REAL                           :: ZLBDA   ! slope distribution parameter
REAL                           :: ZFRAC_ICE  ! ice water fraction
REAL                           :: ZDELTA_EQUIV ! mass-equivalent Gauss-Laguerre point
REAL                           :: ZFW ! liquid fraction
REAL                           :: ZFPW ! weight for mixed-phase reflectivity
REAL,DIMENSION(:),ALLOCATABLE  :: ZX,ZW ! Gauss-Laguerre points and weights
REAL,DIMENSION(:),ALLOCATABLE  :: ZRTMIN ! local values for XRTMIN
LOGICAL                        :: GCALC
!----------------------------------------------------------------------------
!
!*      1.   PRELIMINARIES
!            -------------
!
IF(.NOT. ALLOCATED(XTHW_FLUX)) &
ALLOCATE(XTHW_FLUX(SIZE(PTH,1),SIZE(PTH,2),SIZE(PTH,3)))
IF(.NOT. ALLOCATED(XRCW_FLUX)) &
ALLOCATE(XRCW_FLUX(SIZE(PTH,1),SIZE(PTH,2),SIZE(PTH,3)))
IF(.NOT. ALLOCATED(XSVW_FLUX)) &
ALLOCATE(XSVW_FLUX(SIZE(PSV,1),SIZE(PSV,2),SIZE(PSV,3),SIZE(PSV,4)))
ILUOUT = TLUOUT%NLU
!
ZR = 0.
GSTORE = .FALSE.
!
!*      1.0  initialization of processor test
!            --------------------------------
!
ZTHIS_PROC=0.
!
!
!*      1.1  test on the model
!            -----------------
!
CALL GET_MODEL_NUMBER_ll  (IMI)
!
!
IF ( TPFLYER%CMODEL /= 'FIX' .AND. COUNT( NDAD(:) == IMI ) /= 0       &
     .AND. ( TPFLYER%NMODEL == IMI .OR. NDAD(TPFLYER%NMODEL) == IMI ) &
     .AND. TPFLYER%XX_CUR /= XUNDEF .AND. TPFLYER%XY_CUR /= XUNDEF    &
     .AND. TPFLYER%LFLY .AND. .NOT. TPFLYER%LCRASH                    &
     .AND. CPROGRAM == 'MESONH'                                       ) THEN
  CALL FLYER_CHANGE_MODEL( IMI )
ENDIF
!
IF ( TPFLYER%NMODEL /= IMI ) RETURN
!
!----------------------------------------------------------------------------
!
!*      2.   PRELIMINARIES-2
!            -------------
!
!*      2.1  Indices
!            -------
!
CALL GET_INDICE_ll (IIB,IJB,IIE,IJE)
IKB =   1   + JPVEXT
IKE = SIZE(PZ,3) - JPVEXT
IKU = SIZE(PZ,3)
!
!
!*      2.2  Interpolations of model variables to mass points
!            ------------------------------------------------
!
IIU=SIZE(PXHAT)
IJU=SIZE(PYHAT)
!
!----------------------------------------------------------------------------
!
!*      2.3  Compute time until launch by comparison of dates and times
!            ----------------------------------------------------------
!
CALL DATETIME_DISTANCE( TPFLYER%TLAUNCH, TDTCUR, ZTDIST )
!
!*      3.   LAUNCH
!            ------
!
GLAUNCH     = .FALSE.
!
!
IF ( .NOT. TPFLYER%LFLY ) THEN
!
!*      3.2  launch/takeoff is effective
!            ---------------------------
!
  IF (ZTDIST >= - PTSTEP ) THEN
    SELECT TYPE ( TPFLYER )
      CLASS IS ( TAIRCRAFTDATA)
!
!*     3.2.1 Determination of flight segment
!            -------------------------------
!
        TPFLYER%NSEGCURN = 1
        IL = TPFLYER%NSEGCURN
        !
        TPFLYER%XSEGCURT = ZTDIST
        !
        DO WHILE (TPFLYER%XSEGCURT>TPFLYER%XSEGTIME(IL) .AND. IL <= TPFLYER%NSEG)
          TPFLYER%NSEGCURN = TPFLYER%NSEGCURN + 1
          IL = TPFLYER%NSEGCURN
          TPFLYER%XSEGCURT = TPFLYER%XSEGCURT - TPFLYER%XSEGTIME(IL-1)
          IF (IL>TPFLYER%NSEG) EXIT
        END DO
        !
        !* end of flight
        !
        IF (IL > TPFLYER%NSEG) THEN
          TPFLYER%LFLY = .FALSE.
        ELSE
          TPFLYER%LFLY   = .TRUE.
          GLAUNCH        = .TRUE.
          TPFLYER%LCRASH =.FALSE.
          IF (ZTDIST <= PTSTEP ) THEN
            WRITE(ILUOUT,*) '-------------------------------------------------------------------'
            WRITE(ILUOUT,*) 'Aircraft ',TPFLYER%CTITLE,' takes off the   ', &
                            TDTCUR%nday,'/',TDTCUR%nmonth,'/',              &
                            TDTCUR%nyear,' at ',NINT(TDTCUR%xtime),' sec.'
            WRITE(ILUOUT,*) '-------------------------------------------------------------------'
          ENDIF
        ENDIF

      CLASS IS ( TBALLOONDATA)
        IF (ZTDIST <= PTSTEP ) THEN
          TPFLYER%LFLY = .TRUE.
          GLAUNCH      = .TRUE.
          WRITE(ILUOUT,*) '-------------------------------------------------------------------'
          WRITE(ILUOUT,*) 'Balloon  ',TPFLYER%CTITLE,' is launched the ', &
                          TDTCUR%nday,'/',TDTCUR%nmonth,'/',              &
                          TDTCUR%nyear,' at ',NINT(TDTCUR%xtime),' sec.'
          WRITE(ILUOUT,*) '-------------------------------------------------------------------'
        END IF

      CLASS DEFAULT
        CALL PRINT_MSG( NVERB_ERROR, 'GEN', 'AIRCRAFT_BALLOON_EVOL', 'unknown type for TPFLYER', OLOCAL = .TRUE. )

    END SELECT
!
!*      3.3  Initial horizontal positions
!            ----------------------------
!
    SELECT TYPE ( TPFLYER )
      CLASS IS ( TBALLOONDATA)
        IF ( TPFLYER%CTYPE == 'RADIOS' .OR. TPFLYER%CTYPE == 'ISODEN' .OR. TPFLYER%CTYPE == 'CVBALL' ) THEN
          TPFLYER%XX_CUR = TPFLYER%XXLAUNCH
          TPFLYER%XY_CUR = TPFLYER%XYLAUNCH
        END IF

      CLASS IS ( TAIRCRAFTDATA)
!
!
!*       3.3.2 Determination of initial position
!              -----------------------------
!
        IF (TPFLYER%LFLY) THEN
          ZSEG_FRAC = TPFLYER%XSEGCURT / TPFLYER%XSEGTIME(IL)
          !
          TPFLYER%XX_CUR = (1.-ZSEG_FRAC) * TPFLYER%XSEGX(IL  ) &
                         +     ZSEG_FRAC  * TPFLYER%XSEGX(IL+1)
          TPFLYER%XY_CUR = (1.-ZSEG_FRAC) * TPFLYER%XSEGY(IL  ) &
                         +     ZSEG_FRAC  * TPFLYER%XSEGY(IL+1)
        END IF
    END SELECT
  END IF
END IF
!
!*      3.4  instant of storage
!            ------------------
!
CALL  STATPROF_INSTANT( TPFLYER%TFLYER_TIME, IN )
IF ( IN > 0 ) GSTORE = .TRUE. ! else no profiler storage at this time step
!
IF ( TPFLYER%LFLY ) THEN
!
!----------------------------------------------------------------------------
!
!*      4.   FLYER POSITION
!            --------------
!
!*      4.1  X position
!            ----------
!
  IU=COUNT( PXHAT (:)<=TPFLYER%XX_CUR )
  II=COUNT( XXHATM(:)<=TPFLYER%XX_CUR )
!
  IF ( IU < IIB .AND. LWEST_ll() ) THEN
    IF ( TPFLYER%CMODEL == 'FIX' .OR. TPFLYER%NMODEL == 1 ) THEN
      TPFLYER%LCRASH = .TRUE.
    ELSE
      II = IIB
      IU = IIB
    END IF
  END IF
  IF ( IU > IIE .AND. LEAST_ll() )  THEN
    IF ( TPFLYER%CMODEL == 'FIX'  .OR. TPFLYER%NMODEL == 1 ) THEN
      TPFLYER%LCRASH = .TRUE.
    ELSE
      II = IIE
      IU = IIE
    END IF
  END IF
!
!
!*      4.2  Y position
!            ----------
!
  IV=COUNT( PYHAT (:)<=TPFLYER%XY_CUR )
  IJ=COUNT( XYHATM(:)<=TPFLYER%XY_CUR )
!
  IF ( IV < IJB .AND. LSOUTH_ll() ) THEN
    IF ( TPFLYER%CMODEL == 'FIX'  .OR. TPFLYER%NMODEL == 1 ) THEN
      TPFLYER%LCRASH = .TRUE.
    ELSE
      IJ = IJB
      IV = IJB
    END IF
  END IF
  IF (IV > IJE .AND. LNORTH_ll() ) THEN
    IF ( TPFLYER%CMODEL == 'FIX'  .OR. TPFLYER%NMODEL == 1 ) THEN
      TPFLYER%LCRASH = .TRUE.
    ELSE
      IJ = IJE
      IV = IJE
    END IF
  END IF
!
!
!*      4.3  Position of balloon according to processors
!            -------------------------------------------
!
  IF (IU>=IIB .AND. IU<=IIE .AND. IV>=IJB .AND. IV<=IJE) ZTHIS_PROC=1.
!
!
!*      4.4  Computations only on correct processor
!            --------------------------------------
!
!----------------------------------------------------------------------------
  IF ( ZTHIS_PROC > 0. .AND. .NOT. TPFLYER%LCRASH ) THEN
!----------------------------------------------------------------------------
!
!*      4.5  Interpolations of model variables to mass points
!            ------------------------------------------------
!

    ZZM(:,:,1:IKU-1)=0.5 *PZ(II  :II+1,IJ  :IJ+1,1:IKU-1)+0.5 *PZ(II  :II+1,IJ  :IJ+1,2:IKU  )
    ZZM(:,:,  IKU  )=1.5 *PZ(II  :II+1,IJ  :IJ+1,  IKU-1)-0.5 *PZ(II  :II+1,IJ  :IJ+1,  IKU-2)
!
    IDU = IU - II
    ZZU(:,:,1:IKU-1)=0.25*PZ(IDU+II-1:IDU+II,  IJ  :IJ+1,1:IKU-1)+0.25*PZ(IDU+II-1:IDU+II  ,IJ  :IJ+1,2:IKU  ) &
                  +0.25*PZ(IDU+II  :IDU+II+1,IJ  :IJ+1,1:IKU-1)+0.25*PZ(IDU+II  :IDU+II+1,IJ  :IJ+1,2:IKU  )
    ZZU(:,:,  IKU  )=0.75*PZ(IDU+II-1:IDU+II  ,IJ  :IJ+1,  IKU-1)-0.25*PZ(IDU+II-1:IDU+II  ,IJ  :IJ+1,  IKU-2) &
                  +0.75*PZ(IDU+II  :IDU+II+1,IJ  :IJ+1,  IKU-1)-0.25*PZ(IDU+II  :IDU+II+1,IJ  :IJ+1,  IKU-2)

    IDV = IV - IJ 
    ZZV(:,:,1:IKU-1)=0.25*PZ(II  :II+1,IDV+IJ-1:IDV+IJ  ,1:IKU-1)+0.25*PZ(II  :II+1,IDV+IJ-1:IDV+IJ  ,2:IKU  ) &
                  +0.25*PZ(II  :II+1,IDV+IJ  :IDV+IJ+1,1:IKU-1)+0.25*PZ(II  :II+1,IDV+IJ  :IDV+IJ+1,2:IKU  )
    ZZV(:,:,  IKU  )=0.75*PZ(II  :II+1,IDV+IJ-1:IDV+IJ  ,  IKU-1)-0.25*PZ(II  :II+1,IDV+IJ-1:IDV+IJ  ,  IKU-2) &
                  +0.75*PZ(II  :II+1,IDV+IJ  :IDV+IJ+1,  IKU-1)-0.25*PZ(II  :II+1,IDV+IJ  :IDV+IJ+1,  IKU-2)
!
!
    ZWM(:,:,1:IKU-1)=0.5*PW(II:II+1,IJ:IJ+1,1:IKU-1)+0.5*PW(II:II+1,IJ:IJ+1,2:IKU  )
    ZWM(:,:,  IKU  )=1.5*PW(II:II+1,IJ:IJ+1,  IKU-1)-0.5*PW(II:II+1,IJ:IJ+1,  IKU-2)
!
!----------------------------------------------------------------------------
!
!*      5.   BALLOON/AIRCRAFT VERTICAL POSITION
!            ----------------------------------
!
!
!*      5.1  Density
!            -------
!
    ZEXN(:,:,:    ) = (PP(II:II+1,IJ:IJ+1,:)/XP00)**(XRD/XCPD)
    DO JK=IKB-1,1,-1
      ZEXN(:,:,JK) = 1.5 * ZEXN(:,:,JK+1) - 0.5 * ZEXN(:,:,JK+2)
    END DO
    DO JK=IKE+1,IKU
      ZEXN(:,:,JK) = 1.5 * ZEXN(:,:,JK-1) - 0.5 * ZEXN(:,:,JK-2)
    END DO
    !
    IF ( TPFLYER%CTYPE == 'ISODEN' .OR. TPFLYER%CTYPE == 'CVBALL' .OR. TPFLYER%CTYPE == 'AIRCRA' ) THEN
      ZTHV(:,:,:) = PTH(II:II+1,IJ:IJ+1,:)
      IF (SIZE(PR,4)>0)                                                     &
      ZTHV(:,:,:) = ZTHV(:,:,:) * ( 1. + XRV/XRD*PR(II:II+1,IJ:IJ+1,:,1) )  &
                                / ( 1. + WATER_SUM(PR(II:II+1,IJ:IJ+1,:,:)) )
      !
      ZTV (:,:,:) = ZTHV(:,:,:) * ZEXN(:,:,:)
      ZRHO(:,:,:) = PP(II:II+1,IJ:IJ+1,:) / (XRD*ZTV(:,:,:))
      DO JK=IKB-1,1,-1
        ZRHO(:,:,JK) = 1.5 * ZRHO(:,:,JK+1) - 0.5 * ZRHO(:,:,JK+2)
      END DO
      DO JK=IKE+1,IKU
        ZRHO(:,:,JK) = 1.5 * ZRHO(:,:,JK-1) - 0.5 * ZRHO(:,:,JK-2)
      END DO
     ZTHW_FLUX(:,:,:) = ZRHO(:,:,:)*XCPD *XTHW_FLUX(II:II+1,IJ:IJ+1,:)
     ZRCW_FLUX(:,:,:) = ZRHO(:,:,:)*XLVTT*XRCW_FLUX(II:II+1,IJ:IJ+1,:)
     ZSVW_FLUX(:,:,:,:) = XSVW_FLUX(II:II+1,IJ:IJ+1,:,:)
    END IF

!
!*      5.2  Initial vertical positions
!            --------------------------
!
    IF (GLAUNCH) THEN
      SELECT TYPE ( TPFLYER )
        CLASS IS ( TBALLOONDATA)
          SELECT CASE ( TPFLYER%CTYPE )
!
!*      5.2.1 Iso-density balloon
!
            CASE ( 'ISODEN' )
              ZXCOEF = (TPFLYER%XX_CUR - XXHATM(II)) / (XXHATM(II+1) - XXHATM(II))
              ZXCOEF = MAX (0.,MIN(ZXCOEF,1.))
              ZYCOEF = (TPFLYER%XY_CUR - XYHATM(IJ)) / (XYHATM(IJ+1) - XYHATM(IJ))
              ZYCOEF = MAX (0.,MIN(ZYCOEF,1.))
              IF ( TPFLYER%XALTLAUNCH /= XNEGUNDEF ) THEN
                IK00 = MAX ( COUNT (TPFLYER%XALTLAUNCH >= ZZM(1,1,:)), 1)
                IK01 = MAX ( COUNT (TPFLYER%XALTLAUNCH >= ZZM(1,2,:)), 1)
                IK10 = MAX ( COUNT (TPFLYER%XALTLAUNCH >= ZZM(2,1,:)), 1)
                IK11 = MAX ( COUNT (TPFLYER%XALTLAUNCH >= ZZM(2,2,:)), 1)
                ZZCOEF00 = (TPFLYER%XALTLAUNCH - ZZM(1,1,IK00)) / ( ZZM(1,1,IK00+1) - ZZM(1,1,IK00))
                ZZCOEF01 = (TPFLYER%XALTLAUNCH - ZZM(1,2,IK01)) / ( ZZM(1,2,IK01+1) - ZZM(1,2,IK01))
                ZZCOEF10 = (TPFLYER%XALTLAUNCH - ZZM(2,1,IK10)) / ( ZZM(2,1,IK10+1) - ZZM(2,1,IK10))
                ZZCOEF11 = (TPFLYER%XALTLAUNCH - ZZM(2,2,IK11)) / ( ZZM(2,2,IK11+1) - ZZM(2,2,IK11))
                TPFLYER%XRHO = FLYER_INTERP(ZRHO)
              ELSE IF ( TPFLYER%XPRES /= XNEGUNDEF ) THEN
                ZFLYER_EXN = (TPFLYER%XPRES/XP00)**(XRD/XCPD)
                IK00 = MAX ( COUNT (ZFLYER_EXN <= ZEXN(1,1,:)), 1)
                IK01 = MAX ( COUNT (ZFLYER_EXN <= ZEXN(1,2,:)), 1)
                IK10 = MAX ( COUNT (ZFLYER_EXN <= ZEXN(2,1,:)), 1)
                IK11 = MAX ( COUNT (ZFLYER_EXN <= ZEXN(2,2,:)), 1)
                ZZCOEF00 = (ZFLYER_EXN - ZEXN(1,1,IK00)) / ( ZEXN(1,1,IK00+1) - ZEXN(1,1,IK00))
                ZZCOEF01 = (ZFLYER_EXN - ZEXN(1,2,IK01)) / ( ZEXN(1,2,IK01+1) - ZEXN(1,2,IK01))
                ZZCOEF10 = (ZFLYER_EXN - ZEXN(2,1,IK10)) / ( ZEXN(2,1,IK10+1) - ZEXN(2,1,IK10))
                ZZCOEF11 = (ZFLYER_EXN - ZEXN(2,2,IK11)) / ( ZEXN(2,2,IK11+1) - ZEXN(2,2,IK11))
                TPFLYER%XRHO = FLYER_INTERP(ZRHO)
              ELSE
                CMNHMSG(1) = 'Error in balloon initial position (balloon ' // TRIM(TPFLYER%CTITLE) // ' )'
                CMNHMSG(2) = 'neither initial ALTITUDE or PRESsure is given'
                CMNHMSG(3) = 'Check your INI_BALLOON routine'
                CALL PRINT_MSG( NVERB_FATAL, 'GEN', 'AIRCRAFT_BALLOON_EVOL', OLOCAL = .TRUE. )
              END IF
!
!*      5.2.2 Radiosounding balloon
!
            CASE ( 'RADIOS' )
              TPFLYER%XZ_CUR = TPFLYER%XALTLAUNCH
              TPFLYER%XZ_CUR = MAX ( TPFLYER%XZ_CUR , ZZM(1,1,IKB) )
              TPFLYER%XZ_CUR = MAX ( TPFLYER%XZ_CUR , ZZM(2,1,IKB) )
              TPFLYER%XZ_CUR = MAX ( TPFLYER%XZ_CUR , ZZM(1,2,IKB) )
              TPFLYER%XZ_CUR = MAX ( TPFLYER%XZ_CUR , ZZM(2,2,IKB) )
!
!*      5.2.4 Constant Volume Balloon
!
            CASE ( 'CVBALL' )
              ZXCOEF = (TPFLYER%XX_CUR - XXHATM(II)) / (XXHATM(II+1) - XXHATM(II))
              ZXCOEF = MAX (0.,MIN(ZXCOEF,1.))
              ZYCOEF = (TPFLYER%XY_CUR - XYHATM(IJ)) / (XYHATM(IJ+1) - XYHATM(IJ))
              ZYCOEF = MAX (0.,MIN(ZYCOEF,1.))
              IF ( TPFLYER%XALTLAUNCH /= XNEGUNDEF ) THEN
                IK00 = MAX ( COUNT (TPFLYER%XALTLAUNCH >= ZZM(1,1,:)), 1)
                IK01 = MAX ( COUNT (TPFLYER%XALTLAUNCH >= ZZM(1,2,:)), 1)
                IK10 = MAX ( COUNT (TPFLYER%XALTLAUNCH >= ZZM(2,1,:)), 1)
                IK11 = MAX ( COUNT (TPFLYER%XALTLAUNCH >= ZZM(2,2,:)), 1)
                IF (IK00*IK01*IK10*IK11 .EQ. 0) THEN
                  TPFLYER%XZ_CUR = TPFLYER%XALTLAUNCH
                  TPFLYER%XZ_CUR = MAX ( TPFLYER%XZ_CUR , ZZM(1,1,IKB) )
                  TPFLYER%XZ_CUR = MAX ( TPFLYER%XZ_CUR , ZZM(2,1,IKB) )
                  TPFLYER%XZ_CUR = MAX ( TPFLYER%XZ_CUR , ZZM(1,2,IKB) )
                  TPFLYER%XZ_CUR = MAX ( TPFLYER%XZ_CUR , ZZM(2,2,IKB) )
                ELSE
                  ZZCOEF00 = (TPFLYER%XALTLAUNCH - ZZM(1,1,IK00)) / ( ZZM(1,1,IK00+1) - ZZM(1,1,IK00))
                  ZZCOEF01 = (TPFLYER%XALTLAUNCH - ZZM(1,2,IK01)) / ( ZZM(1,2,IK01+1) - ZZM(1,2,IK01))
                  ZZCOEF10 = (TPFLYER%XALTLAUNCH - ZZM(2,1,IK10)) / ( ZZM(2,1,IK10+1) - ZZM(2,1,IK10))
                  ZZCOEF11 = (TPFLYER%XALTLAUNCH - ZZM(2,2,IK11)) / ( ZZM(2,2,IK11+1) - ZZM(2,2,IK11))
                  TPFLYER%XRHO = FLYER_INTERP(ZRHO)
                  TPFLYER%XZ_CUR = FLYER_INTERP(ZZM)
                END IF
              ELSE IF ( TPFLYER%XPRES /= XNEGUNDEF ) THEN
                ZFLYER_EXN = (TPFLYER%XPRES/XP00)**(XRD/XCPD)
                IK00 = MAX ( COUNT (ZFLYER_EXN <= ZEXN(1,1,:)), 1)
                IK01 = MAX ( COUNT (ZFLYER_EXN <= ZEXN(1,2,:)), 1)
                IK10 = MAX ( COUNT (ZFLYER_EXN <= ZEXN(2,1,:)), 1)
                IK11 = MAX ( COUNT (ZFLYER_EXN <= ZEXN(2,2,:)), 1)
                IF (IK00*IK01*IK10*IK11 .EQ. 0) THEN
                  TPFLYER%XZ_CUR = ZZM(1,1,IKB)
                  TPFLYER%XZ_CUR = MAX ( TPFLYER%XZ_CUR , ZZM(2,1,IKB) )
                  TPFLYER%XZ_CUR = MAX ( TPFLYER%XZ_CUR , ZZM(1,2,IKB) )
                  TPFLYER%XZ_CUR = MAX ( TPFLYER%XZ_CUR , ZZM(2,2,IKB) )
                ELSE
                  ZZCOEF00 = (ZFLYER_EXN - ZEXN(1,1,IK00)) / ( ZEXN(1,1,IK00+1) - ZEXN(1,1,IK00))
                  ZZCOEF01 = (ZFLYER_EXN - ZEXN(1,2,IK01)) / ( ZEXN(1,2,IK01+1) - ZEXN(1,2,IK01))
                  ZZCOEF10 = (ZFLYER_EXN - ZEXN(2,1,IK10)) / ( ZEXN(2,1,IK10+1) - ZEXN(2,1,IK10))
                  ZZCOEF11 = (ZFLYER_EXN - ZEXN(2,2,IK11)) / ( ZEXN(2,2,IK11+1) - ZEXN(2,2,IK11))
                  TPFLYER%XRHO = FLYER_INTERP(ZRHO)
                  TPFLYER%XZ_CUR = FLYER_INTERP(ZZM)
                END IF
              ELSE
                TPFLYER%XRHO = TPFLYER%XMASS / TPFLYER%XVOLUME
                IK00 = MAX ( COUNT (TPFLYER%XRHO <= ZRHO(1,1,:)), 1)
                IK01 = MAX ( COUNT (TPFLYER%XRHO <= ZRHO(1,2,:)), 1)
                IK10 = MAX ( COUNT (TPFLYER%XRHO <= ZRHO(2,1,:)), 1)
                IK11 = MAX ( COUNT (TPFLYER%XRHO <= ZRHO(2,2,:)), 1)
                IF (IK00*IK01*IK10*IK11 .EQ. 0) THEN
                  TPFLYER%XZ_CUR = ZZM(1,1,IKB)
                  TPFLYER%XZ_CUR = MAX ( TPFLYER%XZ_CUR , ZZM(2,1,IKB) )
                  TPFLYER%XZ_CUR = MAX ( TPFLYER%XZ_CUR , ZZM(1,2,IKB) )
                  TPFLYER%XZ_CUR = MAX ( TPFLYER%XZ_CUR , ZZM(2,2,IKB) )
                ELSE
                  ZZCOEF00 = (TPFLYER%XRHO - ZRHO(1,1,IK00)) / ( ZRHO(1,1,IK00+1) - ZRHO(1,1,IK00))
                  ZZCOEF01 = (TPFLYER%XRHO - ZRHO(1,2,IK01)) / ( ZRHO(1,2,IK01+1) - ZRHO(1,2,IK01))
                  ZZCOEF10 = (TPFLYER%XRHO - ZRHO(2,1,IK10)) / ( ZRHO(2,1,IK10+1) - ZRHO(2,1,IK10))
                  ZZCOEF11 = (TPFLYER%XRHO - ZRHO(2,2,IK11)) / ( ZRHO(2,2,IK11+1) - ZRHO(2,2,IK11))
                  TPFLYER%XZ_CUR = FLYER_INTERP(ZZM)
                END IF
              END IF
          END SELECT
!
!*      5.2.3 Aircraft
!
        CLASS IS ( TAIRCRAFTDATA)
          IF (TPFLYER%LALTDEF) THEN
            TPFLYER%XP_CUR = (1.-ZSEG_FRAC) * TPFLYER%XSEGP(IL  ) &
                           +     ZSEG_FRAC  * TPFLYER%XSEGP(IL+1)
          ELSE
            TPFLYER%XZ_CUR = (1.-ZSEG_FRAC) * TPFLYER%XSEGZ(IL ) &
                           +     ZSEG_FRAC  * TPFLYER%XSEGZ(IL +1 )
          END IF
      END SELECT
    END IF
!
!
!
!*      5.3  Vertical position
!            -----------------
!
    SELECT TYPE ( TPFLYER )
      CLASS IS ( TBALLOONDATA)
        IF ( TPFLYER%CTYPE == 'ISODEN' ) THEN
          IK00 = MAX ( COUNT (TPFLYER%XRHO <= ZRHO(1,1,:)), 1)
          IK01 = MAX ( COUNT (TPFLYER%XRHO <= ZRHO(1,2,:)), 1)
          IK10 = MAX ( COUNT (TPFLYER%XRHO <= ZRHO(2,1,:)), 1)
          IK11 = MAX ( COUNT (TPFLYER%XRHO <= ZRHO(2,2,:)), 1)
        ELSE IF ( TPFLYER%CTYPE == 'RADIOS' .OR. TPFLYER%CTYPE == 'CVBALL' ) THEN
          IK00 = MAX ( COUNT (TPFLYER%XZ_CUR >= ZZM(1,1,:)), 1)
          IK01 = MAX ( COUNT (TPFLYER%XZ_CUR >= ZZM(1,2,:)), 1)
          IK10 = MAX ( COUNT (TPFLYER%XZ_CUR >= ZZM(2,1,:)), 1)
          IK11 = MAX ( COUNT (TPFLYER%XZ_CUR >= ZZM(2,2,:)), 1)
        END IF

      CLASS IS ( TAIRCRAFTDATA)
        IF ( TPFLYER%LALTDEF ) THEN
          ZFLYER_EXN = (TPFLYER%XP_CUR/XP00)**(XRD/XCPD)
          IK00 = MAX ( COUNT (ZFLYER_EXN <= ZEXN(1,1,:)), 1)
          IK01 = MAX ( COUNT (ZFLYER_EXN <= ZEXN(1,2,:)), 1)
          IK10 = MAX ( COUNT (ZFLYER_EXN <= ZEXN(2,1,:)), 1)
          IK11 = MAX ( COUNT (ZFLYER_EXN <= ZEXN(2,2,:)), 1)
        ELSE
          IK00 = MAX ( COUNT (TPFLYER%XZ_CUR >= ZZM(1,1,:)), 1)
          IK01 = MAX ( COUNT (TPFLYER%XZ_CUR >= ZZM(1,2,:)), 1)
          IK10 = MAX ( COUNT (TPFLYER%XZ_CUR >= ZZM(2,1,:)), 1)
          IK11 = MAX ( COUNT (TPFLYER%XZ_CUR >= ZZM(2,2,:)), 1)
        END IF
    END SELECT

    IK00 = MAX ( IK00, IKB )
    IK01 = MAX ( IK01, IKB )
    IK10 = MAX ( IK10, IKB )
    IK11 = MAX ( IK11, IKB )
!
!
!*      5.4  Crash of the balloon
!            --------------------
!
!
    IF (IK00 <  IKB .OR. IK01 <  IKB .OR. IK10 <  IKB .OR. IK11 <  IKB .OR. &
        IK00 >= IKE .OR. IK01 >= IKE .OR. IK10 >= IKE .OR. IK11 >= IKE  ) THEN
      TPFLYER%LCRASH = .TRUE.
    END IF
!
  END IF
!
!
  IF ( TPFLYER%LCRASH ) THEN
    TPFLYER%LFLY = .FALSE.
    IF ( TPFLYER%CTYPE == 'AIRCRA' .AND. .NOT. GLAUNCH ) THEN
      WRITE(ILUOUT,*) 'Aircraft ',TPFLYER%CTITLE,' flew out of the domain the ', &
                      TDTCUR%nday,'/',TDTCUR%nmonth,'/',                         &
                      TDTCUR%nyear,' at ',TDTCUR%xtime,' sec.'
    ELSE IF (TPFLYER%CTYPE /= 'AIRCRA') THEN
      WRITE(ILUOUT,*) 'Balloon ',TPFLYER%CTITLE,' crashed the ',                 &
                      TDTCUR%nday,'/',TDTCUR%nmonth,'/',                         &
                      TDTCUR%nyear,' at ',TDTCUR%xtime,' sec.'
    END IF
  ELSE
    SELECT TYPE ( TPFLYER )
      CLASS IS ( TAIRCRAFTDATA)
        IF ( .NOT. GLAUNCH .AND. ZTDIST > PTSTEP ) THEN
          WRITE(ILUOUT,*) '-------------------------------------------------------------------'
          WRITE(ILUOUT,*) 'Aircraft ',TPFLYER%CTITLE,' flies  in leg',TPFLYER%NSEGCURN ,' the ',  &
                          TDTCUR%nday,'/',TDTCUR%nmonth,'/',      &
                          TDTCUR%nyear,' at ',NINT(TDTCUR%xtime),' sec.'
          WRITE(ILUOUT,*) '-------------------------------------------------------------------'
        ENDIF
    END SELECT
!
!----------------------------------------------------------------------------
    IF (ZTHIS_PROC>0.) THEN
!----------------------------------------------------------------------------
!
!*      6.   INITIALIZATIONS FOR INTERPOLATIONS
!            ----------------------------------
!
!*      6.1  Interpolation coefficient for X
!            -------------------------------
!
      ZXCOEF = (TPFLYER%XX_CUR - XXHATM(II)) / (XXHATM(II+1) - XXHATM(II))
      ZXCOEF = MAX (0.,MIN(ZXCOEF,1.))
!
!
!*      6.2  Interpolation coefficient for y
!            -------------------------------
!
      ZYCOEF = (TPFLYER%XY_CUR - XYHATM(IJ)) / (XYHATM(IJ+1) - XYHATM(IJ))
      ZYCOEF = MAX (0.,MIN(ZYCOEF,1.))
!
!
!*      6.3  Interpolation coefficients for the 4 suroundings verticals
!            ----------------------------------------------------------
!
      SELECT TYPE ( TPFLYER )
        CLASS IS ( TBALLOONDATA)
          IF ( TPFLYER%CTYPE == 'ISODEN' ) THEN
            ZZCOEF00 = (TPFLYER%XRHO - ZRHO(1,1,IK00)) / ( ZRHO(1,1,IK00+1) - ZRHO(1,1,IK00) )
            ZZCOEF01 = (TPFLYER%XRHO - ZRHO(1,2,IK01)) / ( ZRHO(1,2,IK01+1) - ZRHO(1,2,IK01) )
            ZZCOEF10 = (TPFLYER%XRHO - ZRHO(2,1,IK10)) / ( ZRHO(2,1,IK10+1) - ZRHO(2,1,IK10) )
            ZZCOEF11 = (TPFLYER%XRHO - ZRHO(2,2,IK11)) / ( ZRHO(2,2,IK11+1) - ZRHO(2,2,IK11) )
            TPFLYER%XZ_CUR = FLYER_INTERP(ZZM)
          ELSE IF ( TPFLYER%CTYPE == 'RADIOS' .OR. TPFLYER%CTYPE == 'CVBALL' ) THEN
            ZZCOEF00 = (TPFLYER%XZ_CUR - ZZM(1,1,IK00)) / ( ZZM(1,1,IK00+1) - ZZM(1,1,IK00) )
            ZZCOEF01 = (TPFLYER%XZ_CUR - ZZM(1,2,IK01)) / ( ZZM(1,2,IK01+1) - ZZM(1,2,IK01) )
            ZZCOEF10 = (TPFLYER%XZ_CUR - ZZM(2,1,IK10)) / ( ZZM(2,1,IK10+1) - ZZM(2,1,IK10) )
            ZZCOEF11 = (TPFLYER%XZ_CUR - ZZM(2,2,IK11)) / ( ZZM(2,2,IK11+1) - ZZM(2,2,IK11) )
          END IF

        CLASS IS ( TAIRCRAFTDATA)
          IF ( TPFLYER%LALTDEF ) THEN
            ZZCOEF00 = (ZFLYER_EXN - ZEXN(1,1,IK00)) / ( ZEXN(1,1,IK00+1) - ZEXN(1,1,IK00) )
            ZZCOEF01 = (ZFLYER_EXN - ZEXN(1,2,IK01)) / ( ZEXN(1,2,IK01+1) - ZEXN(1,2,IK01) )
            ZZCOEF10 = (ZFLYER_EXN - ZEXN(2,1,IK10)) / ( ZEXN(2,1,IK10+1) - ZEXN(2,1,IK10) )
            ZZCOEF11 = (ZFLYER_EXN - ZEXN(2,2,IK11)) / ( ZEXN(2,2,IK11+1) - ZEXN(2,2,IK11) )
            TPFLYER%XZ_CUR = FLYER_INTERP(ZZM)
          ELSE
            ZZCOEF00 = (TPFLYER%XZ_CUR - ZZM(1,1,IK00)) / ( ZZM(1,1,IK00+1) - ZZM(1,1,IK00) )
            ZZCOEF01 = (TPFLYER%XZ_CUR - ZZM(1,2,IK01)) / ( ZZM(1,2,IK01+1) - ZZM(1,2,IK01) )
            ZZCOEF10 = (TPFLYER%XZ_CUR - ZZM(2,1,IK10)) / ( ZZM(2,1,IK10+1) - ZZM(2,1,IK10) )
            ZZCOEF11 = (TPFLYER%XZ_CUR - ZZM(2,2,IK11)) / ( ZZM(2,2,IK11+1) - ZZM(2,2,IK11) )
            TPFLYER%XP_CUR = FLYER_INTERP(PP)
          END IF
      END SELECT
!
!----------------------------------------------------------------------------
!
!*      7.   INITIALIZATIONS FOR INTERPOLATIONS OF U AND V
!            ---------------------------------------------
!
!*      7.1  Interpolation coefficient for X (for U)
!            -------------------------------
!
      ZUCOEF = (TPFLYER%XX_CUR - PXHAT(IU)) / (PXHAT(IU+1) - PXHAT(IU))
      ZUCOEF = MAX(0.,MIN(ZUCOEF,1.))
!
!
!*      7.2  Interpolation coefficient for y (for V)
!            -------------------------------
!
      ZVCOEF = (TPFLYER%XY_CUR - PYHAT(IV)) / (PYHAT(IV+1) - PYHAT(IV))
      ZVCOEF = MAX(0.,MIN(ZVCOEF,1.))
!
!
!*      7.3  Interpolation coefficients for the 4 suroundings verticals (for U)
!            ----------------------------------------------------------
!
      IU00 = MAX( COUNT (TPFLYER%XZ_CUR >= ZZU(1,1,:)), 1)
      IU01 = MAX( COUNT (TPFLYER%XZ_CUR >= ZZU(1,2,:)), 1)
      IU10 = MAX( COUNT (TPFLYER%XZ_CUR >= ZZU(2,1,:)), 1)
      IU11 = MAX( COUNT (TPFLYER%XZ_CUR >= ZZU(2,2,:)), 1)
      ZUCOEF00 = (TPFLYER%XZ_CUR - ZZU(1,1,IU00)) / ( ZZU(1,1,IU00+1) - ZZU(1,1,IU00) )
      ZUCOEF01 = (TPFLYER%XZ_CUR - ZZU(1,2,IU01)) / ( ZZU(1,2,IU01+1) - ZZU(1,2,IU01) )
      ZUCOEF10 = (TPFLYER%XZ_CUR - ZZU(2,1,IU10)) / ( ZZU(2,1,IU10+1) - ZZU(2,1,IU10) )
      ZUCOEF11 = (TPFLYER%XZ_CUR - ZZU(2,2,IU11)) / ( ZZU(2,2,IU11+1) - ZZU(2,2,IU11) )
!
!
!*      7.4  Interpolation coefficients for the 4 suroundings verticals (for V)
!            ----------------------------------------------------------
!

      IV00 = MAX ( COUNT (TPFLYER%XZ_CUR >= ZZV(1,1,:)), 1)
      IV01 = MAX ( COUNT (TPFLYER%XZ_CUR >= ZZV(1,2,:)), 1)
      IV10 = MAX ( COUNT (TPFLYER%XZ_CUR >= ZZV(2,1,:)), 1)
      IV11 = MAX ( COUNT (TPFLYER%XZ_CUR >= ZZV(2,2,:)), 1)
      ZVCOEF00 = (TPFLYER%XZ_CUR - ZZV(1,1,IV00)) / ( ZZV(1,1,IV00+1) - ZZV(1,1,IV00) )
      ZVCOEF01 = (TPFLYER%XZ_CUR - ZZV(1,2,IV01)) / ( ZZV(1,2,IV01+1) - ZZV(1,2,IV01) )
      ZVCOEF10 = (TPFLYER%XZ_CUR - ZZV(2,1,IV10)) / ( ZZV(2,1,IV10+1) - ZZV(2,1,IV10) )
      ZVCOEF11 = (TPFLYER%XZ_CUR - ZZV(2,2,IV11)) / ( ZZV(2,2,IV11+1) - ZZV(2,2,IV11) )
!
!----------------------------------------------------------------------------
!
!*      8.   DATA RECORDING
!            --------------
!
      IF ( GSTORE ) THEN
        TPFLYER%XX   (IN) = TPFLYER%XX_CUR
        TPFLYER%XY   (IN) = TPFLYER%XY_CUR
        TPFLYER%XZ   (IN) = TPFLYER%XZ_CUR
        !
        CALL SM_LATLON(PLATOR,PLONOR,          &
                     TPFLYER%XX_CUR, TPFLYER%XY_CUR,       &
                     TPFLYER%XLAT(IN), TPFLYER%XLON(IN)  )
        !
        ZU_BAL = FLYER_INTERP_U(PU)
        ZV_BAL = FLYER_INTERP_V(PV)
        ZGAM   = (XRPK * (TPFLYER%XLON(IN) - XLON0) - XBETA)*(XPI/180.)
        TPFLYER%XZON (IN) = ZU_BAL * COS(ZGAM) + ZV_BAL * SIN(ZGAM)
        TPFLYER%XMER (IN) = - ZU_BAL * SIN(ZGAM) + ZV_BAL * COS(ZGAM)
        !
        TPFLYER%XW   (IN) = FLYER_INTERP(ZWM)
        TPFLYER%XTH  (IN) = FLYER_INTERP(PTH)
        !
        ZFLYER_EXN = FLYER_INTERP(ZEXN)
        TPFLYER%XP   (IN) = XP00 * ZFLYER_EXN**(XCPD/XRD)
        !
        DO JLOOP=1,SIZE(PR,4)
          TPFLYER%XR   (IN,JLOOP) = FLYER_INTERP(PR(:,:,:,JLOOP))
          IF (JLOOP>=2) ZR(:,:,:) = ZR(:,:,:) + PR(:,:,:,JLOOP)
        END DO
        DO JLOOP=1,SIZE(PSV,4)
          TPFLYER%XSV  (IN,JLOOP) = FLYER_INTERP(PSV(:,:,:,JLOOP))
        END DO
        TPFLYER%XRTZ  (IN,:) = FLYER_INTERPZ(ZR(:,:,:))
        DO JLOOP=1,SIZE(PR,4)
          TPFLYER%XRZ  (IN,:,JLOOP) = FLYER_INTERPZ(PR(:,:,:,JLOOP))
        END DO
        ! Fin Modifs ON
        TPFLYER%XFFZ  (IN,:) = FLYER_INTERPZ(SQRT(PU**2+PV**2))
        IF (CCLOUD=="LIMA") THEN                                  
          TPFLYER%XCIZ  (IN,:) = FLYER_INTERPZ(PSV(:,:,:,NSV_LIMA_NI))
          TPFLYER%XCCZ  (IN,:) = FLYER_INTERPZ(PSV(:,:,:,NSV_LIMA_NC))
          TPFLYER%XCRZ  (IN,:) = FLYER_INTERPZ(PSV(:,:,:,NSV_LIMA_NR))
        ELSE IF ( CCLOUD=="ICE3" .OR. CCLOUD=="ICE4" ) THEN
          TPFLYER%XCIZ  (IN,:) = FLYER_INTERPZ(PCIT(:,:,:))
        ENDIF             
        ! initialization CRARE and CRARE_ATT + LWC and IWC
        TPFLYER%XCRARE(IN,:) = 0.
        TPFLYER%XCRARE_ATT(IN,:) = 0.
        TPFLYER%XLWCZ  (IN,:) = 0.
        TPFLYER%XIWCZ  (IN,:) = 0.
      IF (CCLOUD=="LIMA" .OR. CCLOUD=="ICE3" ) THEN ! only for ICE3 and LIMA
       TPFLYER%XLWCZ  (IN,:) = FLYER_INTERPZ((PR(:,:,:,2)+PR(:,:,:,3))*PRHODREF(:,:,:))
       TPFLYER%XIWCZ  (IN,:) = FLYER_INTERPZ((PR(:,:,:,4)+PR(:,:,:,5)+PR(:,:,:,6))*PRHODREF(:,:,:))
       ZTEMPZ(:)=FLYER_INTERPZ(PTH(II:II+1,IJ:IJ+1,:) * ZEXN(:,:,:))
        ZRHODREFZ(:)=FLYER_INTERPZ(PRHODREF(:,:,:))
        IF (CCLOUD=="LIMA") THEN
          ZCCI(:)=FLYER_INTERPZ(PSV(:,:,:,NSV_LIMA_NI))
          ZCCR(:)=FLYER_INTERPZ(PSV(:,:,:,NSV_LIMA_NR))
          ZCCC(:)=FLYER_INTERPZ(PSV(:,:,:,NSV_LIMA_NC))
        ELSE
          ZCIT(:)=FLYER_INTERPZ(PCIT(:,:,:))
        ENDIF
        DO JLOOP=3,6
          ZRZ(:,JLOOP)=FLYER_INTERPZ(PR(:,:,:,JLOOP))
        END DO
        if ( csurf == 'EXTE' ) then
          DO JK=1,IKU
            ZRZ(JK,2)=FLYER_INTERP_2D(PR(:,:,JK,2)*PSEA(:,:))       ! becomes cloud mixing ratio over sea
            ZRZ(JK,7)=FLYER_INTERP_2D(PR(:,:,JK,2)*(1.-PSEA(:,:)))  ! becomes cloud mixing ratio over land
          END DO
        else
          !if csurf/='EXTE', psea is not allocated
          DO JK=1,IKU
            ZRZ(JK,2)=FLYER_INTERP_2D(PR(:,:,JK,2))
            ZRZ(JK,7) = 0.
          END DO
        end if
        ALLOCATE(ZAELOC(IKU))
        !
        ZAELOC(:)=0.
        ! initialization of quadrature points and weights
        ALLOCATE(ZX(JPTS_GAULAG),ZW(JPTS_GAULAG))
        CALL GAULAG(JPTS_GAULAG,ZX,ZW) ! for integration over diameters
        ! initialize minimum values
        ALLOCATE(ZRTMIN(SIZE(PR,4)+1))
        IF (CCLOUD == 'LIMA') THEN
          ZRTMIN(2)=XRTMIN_L(2) ! cloud water over sea
          ZRTMIN(3)=XRTMIN_L(3)
          ZRTMIN(4)=XRTMIN_L(4)
          ZRTMIN(5)=1E-10
          ZRTMIN(6)=XRTMIN_L(6)
          ZRTMIN(7)=XRTMIN_L(2) ! cloud water over land
        ELSE
          ZRTMIN(2)=XRTMIN_I(2) ! cloud water over sea
          ZRTMIN(3)=XRTMIN_I(3)
          ZRTMIN(4)=XRTMIN_I(4)
          ZRTMIN(5)=1E-10
          ZRTMIN(6)=XRTMIN_I(6)
          ZRTMIN(7)=XRTMIN_I(2) ! cloud water over land
        ENDIF
        ! compute cloud radar reflectivity from vertical profiles of temperature and mixing ratios
        DO JK=1,IKU
          QMW=SQRT(QEPSW(ZTEMPZ(JK),XLIGHTSPEED/XLAM_CRAD))
          QMI=SQRT(QEPSI(ZTEMPZ(JK),XLIGHTSPEED/XLAM_CRAD))
          DO JLOOP=2,7
            IF (CCLOUD == 'LIMA') THEN
              GCALC=(ZRZ(JK,JLOOP)>ZRTMIN(JLOOP).AND.(JLOOP.NE.4.OR.ZCCI(JK)>0.).AND.&
                    (JLOOP.NE.3.OR.ZCCR(JK)>0.).AND.((JLOOP.NE.2.AND. JLOOP.NE.7).OR.ZCCC(JK)>0.))
            ELSE
              GCALC=(ZRZ(JK,JLOOP)>ZRTMIN(JLOOP).AND.(JLOOP.NE.4.OR.ZCIT(JK)>0.))
            ENDIF
            IF(GCALC) THEN
              SELECT CASE(JLOOP)
                CASE(2) ! cloud water over sea
                  IF (CCLOUD == 'LIMA') THEN
                    ZA=XAC_L
                    ZB=XBC_L
                    ZCC=ZCCC(JK)*ZRHODREFZ(JK)
                    ZCX=0.
                    ZALPHA=XALPHAC_L
                    ZNU=XNUC_L
                    ZLBEX=1.0/(ZCX-ZB)
                    ZLB=( ZA*ZCC*MOMG(ZALPHA,ZNU,ZB) )**(-ZLBEX)
                  ELSE
                    ZA=XAC_I
                    ZB=XBC_I
                    ZCC=XCONC_SEA
                    ZCX=0.
                    ZALPHA=XALPHAC2_I
                    ZNU=XNUC2_I
                    ZLBEX=1.0/(ZCX-ZB)
                    ZLB=( ZA*ZCC*MOMG(ZALPHA,ZNU,ZB) )**(-ZLBEX)
                  ENDIF
                CASE(3) ! rain water
                  IF (CCLOUD == 'LIMA') THEN
                    ZA=XAR_L
                    ZB=XBR_L
                    ZCC=ZCCR(JK)*ZRHODREFZ(JK)
                    ZCX=0.
                    ZALPHA=XALPHAR_L
                    ZNU=XNUR_L
                    ZLBEX=1.0/(ZCX-ZB)
                    ZLB=( ZA*ZCC*MOMG(ZALPHA,ZNU,ZB) )**(-ZLBEX)
                  ELSE
                    ZA=XAR_I
                    ZB=XBR_I
                    ZCC=XCCR_I
                    ZCX=-1.
                    ZALPHA=XALPHAR_I
                    ZNU=XNUR_I
                    ZLB=XLBR_I
                    ZLBEX=XLBEXR_I
                  ENDIF
                CASE(4) ! pristine ice
                  IF (CCLOUD == 'LIMA') THEN
                    ZA=XAI_L
                    ZB=XBI_L
                    ZCC=ZCCI(JK)*ZRHODREFZ(JK)
                    ZCX=0.
                    ZALPHA=XALPHAI_L
                    ZNU=XNUI_L
                    ZLBEX=1.0/(ZCX-ZB)
                    ZLB=( ZA*ZCC*MOMG(ZALPHA,ZNU,ZB) )**(-ZLBEX) ! because ZCC not included in XLBI
                    ZFW=0
                  ELSE
                    ZA=XAI_I
                    ZB=XBI_I
                    ZCC=ZCIT(JK)
                    ZCX=0.
                    ZALPHA=XALPHAI_I
                    ZNU=XNUI_I
                    ZLBEX=XLBEXI_I
                    ZLB=XLBI_I*ZCC**(-ZLBEX) ! because ZCC not included in XLBI
                    ZFW=0
                  ENDIF                          
                CASE(5) ! snow
                  IF (CCLOUD == 'LIMA') THEN
                    ZA=XAS_L
                    ZB=XBS_L
                    ZCC=XCCS_L
                    ZCX=XCXS_L
                    ZALPHA=XALPHAS_L
                    ZNU=XNUS_L
                    ZLB=XLBS_L
                    ZLBEX=XLBEXS_L
                    ZFW=0
                  ELSE
                    ZA=XAS_I
                    ZB=XBS_I
                    ZCC=XCCS_I
                    ZCX=XCXS_I
                    ZALPHA=XALPHAS_I
                    ZNU=XNUS_I
                    ZLB=XLBS_I
                    ZLBEX=XLBEXS_I
                    ZFW=0
                  ENDIF
                CASE(6) ! graupel
                  !If temperature between -10 and 10°C and Mr and Mg over min threshold: melting graupel
                  ! with liquid water fraction Fw=Mr/(Mr+Mg) else dry graupel (Fw=0)    
                  IF( ZTEMPZ(JK) > XTT-10 .AND. ZTEMPZ(JK) < XTT+10 &
                    .AND. ZRZ(JK,3) > ZRTMIN(3) ) THEN
                    ZFW=ZRZ(JK,3)/(ZRZ(JK,3)+ZRZ(JK,JLOOP))
                  ELSE
                    ZFW=0
                  ENDIF
                  IF (CCLOUD == 'LIMA') THEN
                    ZA=XAG_L
                    ZB=XBG_L
                    ZCC=XCCG_L
                    ZCX=XCXG_L
                    ZALPHA=XALPHAG_L
                    ZNU=XNUG_L
                    ZLB=XLBG_L
                    ZLBEX=XLBEXG_L
                  ELSE
                    ZA=XAG_I
                    ZB=XBG_I
                    ZCC=XCCG_I
                    ZCX=XCXG_I
                    ZALPHA=XALPHAG_I
                    ZNU=XNUG_I
                    ZLB=XLBG_I
                    ZLBEX=XLBEXG_I
                  ENDIF                          
                CASE(7) ! cloud water over land
                  IF (CCLOUD == 'LIMA') THEN
                    ZA=XAC_L
                    ZB=XBC_L
                    ZCC=ZCCC(JK)*ZRHODREFZ(JK)
                    ZCX=0.
                    ZALPHA=XALPHAC_L
                    ZNU=XNUC_L
                    ZLBEX=1.0/(ZCX-ZB)
                    ZLB=( ZA*ZCC*MOMG(ZALPHA,ZNU,ZB) )**(-ZLBEX)
                  ELSE
                    ZA=XAC_I
                    ZB=XBC_I
                    ZCC=XCONC_LAND
                    ZCX=0.
                    ZALPHA=XALPHAC_I
                    ZNU=XNUC_I
                    ZLBEX=1.0/(ZCX-ZB)
                    ZLB=( ZA*ZCC*MOMG(ZALPHA,ZNU,ZB) )**(-ZLBEX)
                  ENDIF
              END SELECT
              ZLBDA=ZLB*(ZRHODREFZ(JK)*ZRZ(JK,JLOOP))**ZLBEX
              ZREFLOC=0.
              ZAETMP=0.
              DO JJ=1,JPTS_GAULAG ! Gauss-Laguerre quadrature
                ZDELTA_EQUIV=ZX(JJ)**(1./ZALPHA)/ZLBDA
                SELECT CASE(JLOOP)
                  CASE(2,3,7)
                    QM=QMW
                  CASE(4,5,6)
                    ! pristine ice, snow, dry graupel
                    ZRHOHYD=MIN(6.*ZA*ZDELTA_EQUIV**(ZB-3.)/XPI,.92*XRHOLW)
                    QM=sqrt(MG(QMI**2,CMPLX(1,0),ZRHOHYD/.92/XRHOLW))
                    
                    ! water inclusions in ice in air
                    QEPSWI=MG(QMW**2,QM**2,ZFW)
                    ! ice in air inclusions in water
                    QEPSIW=MG(QM**2,QMW**2,1.-ZFW)
                  
                    !MG weighted rule (Matrosov 2008)
                    IF(ZFW .LT. 0.37) THEN
                      ZFPW=0
                    ELSE IF(ZFW .GT. 0.63) THEN
                      ZFPW=1
                    ELSE
                      ZFPW=(ZFW-0.37)/(0.63-0.37)
                    ENDIF  
                    QM=sqrt(QEPSWI*(1.-ZFPW)+QEPSIW*ZFPW)
                END SELECT
                CALL BHMIE(XPI/XLAM_CRAD*ZDELTA_EQUIV,QM,ZQEXT,ZQSCA,ZQBACK)
                ZREFLOC=ZREFLOC+ZQBACK*ZX(JJ)**(ZNU-1)*ZDELTA_EQUIV**2*ZW(JJ)
                ZAETMP =ZAETMP +ZQEXT *ZX(JJ)**(ZNU-1)*ZDELTA_EQUIV**2*ZW(JJ)
              END DO
              ZREFLOC=ZREFLOC*(XLAM_CRAD/XPI)**4*ZCC*ZLBDA**ZCX/(4.*GAMMA(ZNU)*.93)
              ZAETMP=ZAETMP  *           XPI    *ZCC*ZLBDA**ZCX/(4.*GAMMA(ZNU))
              TPFLYER%XCRARE(IN,JK)=TPFLYER%XCRARE(IN,JK)+ZREFLOC
              ZAELOC(JK)=ZAELOC(JK)+ZAETMP
            END IF

          END DO

        END DO

        ! apply attenuation
        ALLOCATE(ZZMZ(IKU))
        ZZMZ(:)=FLYER_INTERPZ(ZZM(:,:,:))
        ! nadir
        ZAETOT=1.
        DO JK=COUNT(TPFLYER%XZ_CUR >= ZZMZ(:)),1,-1
          IF(JK.EQ.COUNT(TPFLYER%XZ_CUR >= ZZMZ(:))) THEN
            IF(TPFLYER%XZ_CUR<=ZZMZ(JK)+.5*(ZZMZ(JK+1)-ZZMZ(JK))) THEN
              ! only attenuation from ZAELOC(JK)
              ZAETOT=ZAETOT*EXP(-2.*(ZAELOC(JK)*(TPFLYER%XZ_CUR-ZZMZ(JK))))
            ELSE
              ! attenuation from ZAELOC(JK) and ZAELOC(JK+1)
              ZAETOT=ZAETOT*EXP(-2.*(ZAELOC(JK+1)*(TPFLYER%XZ_CUR-.5*(ZZMZ(JK+1)+ZZMZ(JK))) &
                +ZAELOC(JK)*.5*(ZZMZ(JK+1)-ZZMZ(JK))))
            END IF
          ELSE
            ! attenuation from ZAELOC(JK) and ZAELOC(JK+1)
            ZAETOT=ZAETOT*EXP(-(ZAELOC(JK+1)+ZAELOC(JK))*(ZZMZ(JK+1)-ZZMZ(JK)))
          END IF
          TPFLYER%XCRARE_ATT(IN,JK)=TPFLYER%XCRARE(IN,JK)*ZAETOT
        END DO
        ! zenith
        ZAETOT=1.
        DO JK = MAX(COUNT(TPFLYER%XZ_CUR >= ZZMZ(:)),1)+1,IKU
          IF ( JK .EQ. (MAX(COUNT(TPFLYER%XZ_CUR >= ZZMZ(:)),1)+1) ) THEN
            IF(TPFLYER%XZ_CUR>=ZZMZ(JK)-.5*(ZZMZ(JK)-ZZMZ(JK-1))) THEN
              ! only attenuation from ZAELOC(JK)
              ZAETOT=ZAETOT*EXP(-2.*(ZAELOC(JK)*(ZZMZ(JK)-TPFLYER%XZ_CUR)))
            ELSE
              ! attenuation from ZAELOC(JK) and ZAELOC(JK-1)
              ZAETOT=ZAETOT*EXP(-2.*(ZAELOC(JK-1)*(.5*(ZZMZ(JK)+ZZMZ(JK-1))-TPFLYER%XZ_CUR) &
                +ZAELOC(JK)*.5*(ZZMZ(JK)-ZZMZ(JK-1))))
            END IF
          ELSE
            ! attenuation from ZAELOC(JK) and ZAELOC(JK-1)
            ZAETOT=ZAETOT*EXP(-(ZAELOC(JK-1)+ZAELOC(JK))*(ZZMZ(JK)-ZZMZ(JK-1)))
          END IF
          TPFLYER%XCRARE_ATT(IN,JK)=TPFLYER%XCRARE(IN,JK)*ZAETOT
        END DO
        TPFLYER%XZZ  (IN,:) = ZZMZ(:)
        DEALLOCATE(ZZMZ,ZAELOC)
        ! m^3 → mm^6/m^3 → dBZ
        WHERE(TPFLYER%XCRARE(IN,:)>0)
          TPFLYER%XCRARE(IN,:)=10.*LOG10(1.E18*TPFLYER%XCRARE(IN,:))
        ELSEWHERE
          TPFLYER%XCRARE(IN,:)=XUNDEF
        END WHERE
        WHERE(TPFLYER%XCRARE_ATT(IN,:)>0)
          TPFLYER%XCRARE_ATT(IN,:)=10.*LOG10(1.E18*TPFLYER%XCRARE_ATT(IN,:))
        ELSEWHERE
          TPFLYER%XCRARE_ATT(IN,:)=XUNDEF
        END WHERE
        DEALLOCATE(ZX,ZW,ZRTMIN)
      END IF ! end LOOP ICE3
        ! vertical wind
        TPFLYER%XWZ  (IN,:) = FLYER_INTERPZ(ZWM(:,:,:))
        IF (SIZE(PTKE)>0) TPFLYER%XTKE  (IN)    = FLYER_INTERP(PTKE)
        IF (SIZE(PTS) >0) TPFLYER%XTSRAD(IN)    = FLYER_INTERP_2D(PTS)
        IF (LDIAG_IN_RUN) TPFLYER%XTKE_DISS(IN) = FLYER_INTERP(XCURRENT_TKE_DISS)
        TPFLYER%XZS(IN)  = FLYER_INTERP_2D(PZ(:,:,1+JPVEXT))
        TPFLYER%XTHW_FLUX(IN) = FLYER_INTERP(ZTHW_FLUX)
        TPFLYER%XRCW_FLUX(IN) = FLYER_INTERP(ZRCW_FLUX)
        DO JLOOP=1,SIZE(PSV,4)
         TPFLYER%XSVW_FLUX(IN,JLOOP) = FLYER_INTERP(ZSVW_FLUX(:,:,:,JLOOP))
        END DO
      END IF
!
!----------------------------------------------------------------------------
!
!*      9.   BALLOON ADVECTION
!            -----------------
!
      SELECT TYPE ( TPFLYER )
        CLASS IS ( TBALLOONDATA)
          IF ( TPFLYER%CTYPE == 'RADIOS' .OR. TPFLYER%CTYPE == 'ISODEN' .OR. TPFLYER%CTYPE == 'CVBALL' ) THEN
            ZU_BAL = FLYER_INTERP_U(PU)
            ZV_BAL = FLYER_INTERP_V(PV)
            if ( .not. lcartesian ) then
              ZMAP = FLYER_INTERP_2D(PMAP)
            else
              ZMAP = 1.
            end if
            !
            TPFLYER%XX_CUR = TPFLYER%XX_CUR   +   ZU_BAL * PTSTEP * ZMAP
            TPFLYER%XY_CUR = TPFLYER%XY_CUR   +   ZV_BAL * PTSTEP * ZMAP
          END IF
          !
          IF ( TPFLYER%CTYPE == 'RADIOS' ) THEN
            ZW_BAL = FLYER_INTERP(ZWM)
            TPFLYER%XZ_CUR = TPFLYER%XZ_CUR + ( ZW_BAL + TPFLYER%XWASCENT ) * PTSTEP
          END IF
          !
          IF ( TPFLYER%CTYPE == 'CVBALL' ) THEN
            ZW_BAL = FLYER_INTERP(ZWM)
            ZRO_BAL = FLYER_INTERP(ZRHO)
            ! calculation with a time step of 1 second or less
            IF (INT(PTSTEP) .GT. 1 ) THEN
              DO JK=1,INT(PTSTEP)
                TPFLYER%XWASCENT = TPFLYER%XWASCENT &
                  -  ( 1. / (1. + TPFLYER%XINDDRAG ) ) * 1. * &
                     ( XG * ( ( TPFLYER%XMASS / TPFLYER%XVOLUME ) - ZRO_BAL ) / ( TPFLYER%XMASS / TPFLYER%XVOLUME ) &
                        + TPFLYER%XWASCENT * ABS ( TPFLYER%XWASCENT ) * &
                          TPFLYER%XDIAMETER * TPFLYER%XAERODRAG / ( 2. * TPFLYER%XVOLUME ) &
                      )
                TPFLYER%XZ_CUR = TPFLYER%XZ_CUR + ( ZW_BAL + TPFLYER%XWASCENT ) * 1.
              END DO
            END IF
            IF (PTSTEP .GT. INT(PTSTEP)) THEN
                TPFLYER%XWASCENT = TPFLYER%XWASCENT &
                  -  ( 1. / (1. + TPFLYER%XINDDRAG ) ) * (PTSTEP-INT(PTSTEP)) * &
                     ( XG * ( ( TPFLYER%XMASS / TPFLYER%XVOLUME ) - ZRO_BAL ) / ( TPFLYER%XMASS / TPFLYER%XVOLUME ) &
                        + TPFLYER%XWASCENT * ABS ( TPFLYER%XWASCENT ) * &
                          TPFLYER%XDIAMETER * TPFLYER%XAERODRAG / ( 2. * TPFLYER%XVOLUME ) &
                      )
                TPFLYER%XZ_CUR = TPFLYER%XZ_CUR + ( ZW_BAL + TPFLYER%XWASCENT ) * (PTSTEP-INT(PTSTEP))
            END IF
          END IF
      END SELECT
!
!----------------------------------------------------------------------------
  END IF
!----------------------------------------------------------------------------
!
!*     10.   AIRCRAFT MOVE (computations done on all processors, to limit exchanges)
!            -------------
!
    SELECT TYPE ( TPFLYER )
      CLASS IS ( TAIRCRAFTDATA )
!
!
!*     10.1  Determination of flight segment
!            -------------------------------
!
        IL = TPFLYER%NSEGCURN
        !
        TPFLYER%XSEGCURT = TPFLYER%XSEGCURT + PTSTEP
        !
         DO WHILE (TPFLYER%XSEGCURT>TPFLYER%XSEGTIME(IL))
           TPFLYER%NSEGCURN = TPFLYER%NSEGCURN + 1
           IL = TPFLYER%NSEGCURN
           TPFLYER%XSEGCURT = TPFLYER%XSEGCURT - TPFLYER%XSEGTIME(IL-1)
           IF (IL>TPFLYER%NSEG) EXIT
        END DO
!        DO WHILE (TPFLYER%XSEGCURT>TPFLYER%XSEGTIME(IL) .AND. IL <= TPFLYER%NSEG)
!          TPFLYER%NSEGCURN = TPFLYER%NSEGCURN + 1
!          IL = TPFLYER%NSEGCURN
!          TPFLYER%XSEGCURT = TPFLYER%XSEGCURT - TPFLYER%XSEGTIME(IL-1)
!        END DO
        !
        !* end of flight
        !
        IF (IL > TPFLYER%NSEG) TPFLYER%LFLY = .FALSE.
!
!
!*     10.2  Determination of new position
!            -----------------------------
!
        IF (TPFLYER%LFLY) THEN
          ZSEG_FRAC = TPFLYER%XSEGCURT / TPFLYER%XSEGTIME(IL)
          !
          TPFLYER%XX_CUR = (1.-ZSEG_FRAC) * TPFLYER%XSEGX(IL  ) &
                        +     ZSEG_FRAC  * TPFLYER%XSEGX(IL+1)
          TPFLYER%XY_CUR = (1.-ZSEG_FRAC) * TPFLYER%XSEGY(IL  ) &
                        +     ZSEG_FRAC  * TPFLYER%XSEGY(IL+1)
            IF (TPFLYER%LALTDEF) THEN
               TPFLYER%XP_CUR = (1.-ZSEG_FRAC) * TPFLYER%XSEGP(IL  ) &
                        +     ZSEG_FRAC  * TPFLYER%XSEGP(IL+1)
            ELSE
               TPFLYER%XZ_CUR = (1.-ZSEG_FRAC) * TPFLYER%XSEGZ(IL  ) &
                        +     ZSEG_FRAC  * TPFLYER%XSEGZ(IL+1)
            END IF
        END IF
    END SELECT
  !
  END IF
! 
END IF
!
!----------------------------------------------------------------------------
!
!*     11.   EXCHANGE OF INFORMATION BETWEEN PROCESSORS
!            ------------------------------------------
!
!*     11.1  current position
!            ----------------
!
CALL DISTRIBUTE_FLYER_L(TPFLYER%LFLY)
CALL DISTRIBUTE_FLYER_L(TPFLYER%LCRASH)
CALL DISTRIBUTE_FLYER(TPFLYER%XX_CUR)
CALL DISTRIBUTE_FLYER(TPFLYER%XY_CUR)

SELECT TYPE ( TPFLYER )
  CLASS IS ( TBALLOONDATA )
    IF ( TPFLYER%CTYPE == 'CVBALL' ) THEN
      CALL DISTRIBUTE_FLYER(TPFLYER%XZ_CUR)
      CALL DISTRIBUTE_FLYER(TPFLYER%XWASCENT)
    ELSE IF ( TPFLYER%CTYPE == 'RADIOS' ) THEN
      CALL DISTRIBUTE_FLYER(TPFLYER%XZ_CUR)
    ELSE IF ( TPFLYER%CTYPE == 'ISODEN' ) THEN
      CALL DISTRIBUTE_FLYER(TPFLYER%XRHO)
    END IF

  CLASS IS ( TAIRCRAFTDATA )
    IF (TPFLYER%LALTDEF) THEN
      CALL DISTRIBUTE_FLYER(TPFLYER%XP_CUR)
    ELSE
      CALL DISTRIBUTE_FLYER(TPFLYER%XZ_CUR)
    ENDIF
END SELECT
!
!*     11.2  data stored
!            -----------
!
IF ( GSTORE ) THEN
  CALL DISTRIBUTE_FLYER(TPFLYER%XX  (IN))
  CALL DISTRIBUTE_FLYER(TPFLYER%XY  (IN))
  CALL DISTRIBUTE_FLYER(TPFLYER%XZ  (IN))
  CALL DISTRIBUTE_FLYER(TPFLYER%XLON(IN))
  CALL DISTRIBUTE_FLYER(TPFLYER%XLAT(IN))
  CALL DISTRIBUTE_FLYER(TPFLYER%XZON(IN))
  CALL DISTRIBUTE_FLYER(TPFLYER%XMER(IN))
  CALL DISTRIBUTE_FLYER(TPFLYER%XW  (IN))
  CALL DISTRIBUTE_FLYER(TPFLYER%XP  (IN))
  CALL DISTRIBUTE_FLYER(TPFLYER%XTH (IN))
  DO JLOOP=1,SIZE(PR,4)
    CALL DISTRIBUTE_FLYER(TPFLYER%XR   (IN,JLOOP))
  END DO
  DO JLOOP=1,SIZE(PSV,4)
    CALL DISTRIBUTE_FLYER(TPFLYER%XSV  (IN,JLOOP))
  END DO
  DO JLOOP=1,IKU              
    CALL DISTRIBUTE_FLYER(TPFLYER%XRTZ (IN,JLOOP))
    DO JLOOP2=1,SIZE(PR,4)
      CALL DISTRIBUTE_FLYER(TPFLYER%XRZ (IN,JLOOP,JLOOP2))
    ENDDO
    CALL DISTRIBUTE_FLYER(TPFLYER%XFFZ (IN,JLOOP))
    CALL DISTRIBUTE_FLYER(TPFLYER%XCIZ (IN,JLOOP))
    IF (CCLOUD== 'LIMA' ) THEN
      CALL DISTRIBUTE_FLYER(TPFLYER%XCRZ (IN,JLOOP))
      CALL DISTRIBUTE_FLYER(TPFLYER%XCCZ (IN,JLOOP))
    ENDIF
    CALL DISTRIBUTE_FLYER(TPFLYER%XIWCZ (IN,JLOOP))
    CALL DISTRIBUTE_FLYER(TPFLYER%XLWCZ (IN,JLOOP))
    CALL DISTRIBUTE_FLYER(TPFLYER%XCRARE (IN,JLOOP))
    CALL DISTRIBUTE_FLYER(TPFLYER%XCRARE_ATT (IN,JLOOP))
    CALL DISTRIBUTE_FLYER(TPFLYER%XWZ (IN,JLOOP))
    CALL DISTRIBUTE_FLYER(TPFLYER%XZZ (IN,JLOOP))
  END DO
  IF (SIZE(PTKE)>0) CALL DISTRIBUTE_FLYER(TPFLYER%XTKE  (IN))
  IF (SIZE(PTS) >0) CALL DISTRIBUTE_FLYER(TPFLYER%XTSRAD(IN))
  CALL DISTRIBUTE_FLYER(TPFLYER%XZS  (IN))
  CALL DISTRIBUTE_FLYER(TPFLYER%XTHW_FLUX(IN))
  CALL DISTRIBUTE_FLYER(TPFLYER%XRCW_FLUX(IN))
  DO JLOOP=1,SIZE(PSV,4)
    CALL DISTRIBUTE_FLYER(TPFLYER%XSVW_FLUX(IN,JLOOP))
  END DO
END IF
!
!----------------------------------------------------------------------------
!----------------------------------------------------------------------------
!
CONTAINS
!
!----------------------------------------------------------------------------
!----------------------------------------------------------------------------
FUNCTION FLYER_INTERP(PA) RESULT(PB)
!
REAL, DIMENSION(:,:,:), INTENT(IN) :: PA
REAL                               :: PB
!
INTEGER :: JI, JJ
!
IF (SIZE(PA,1)==2) THEN
  JI=1
  JJ=1
ELSE
  JI=II
  JJ=IJ
END IF
!
PB = (1.- ZYCOEF) * (1.-ZXCOEF) * ( (1.-ZZCOEF00) * PA(JI  ,JJ  ,IK00) + ZZCOEF00 * PA(JI  ,JJ  ,IK00+1)) &
   + (1.- ZYCOEF) * (   ZXCOEF) * ( (1.-ZZCOEF10) * PA(JI+1,JJ  ,IK10) + ZZCOEF10 * PA(JI+1,JJ  ,IK10+1)) &
   + (    ZYCOEF) * (1.-ZXCOEF) * ( (1.-ZZCOEF01) * PA(JI  ,JJ+1,IK01) + ZZCOEF01 * PA(JI  ,JJ+1,IK01+1)) &
   + (    ZYCOEF) * (   ZXCOEF) * ( (1.-ZZCOEF11) * PA(JI+1,JJ+1,IK11) + ZZCOEF11 * PA(JI+1,JJ+1,IK11+1))
!
END FUNCTION FLYER_INTERP
!----------------------------------------------------------------------------
FUNCTION FLYER_INTERPZ(PA) RESULT(PB)
!
REAL, DIMENSION(:,:,:), INTENT(IN) :: PA
REAL, DIMENSION(SIZE(PA,3))        :: PB
!
INTEGER :: JI, JJ, JK
!
IF (SIZE(PA,1)==2) THEN
  JI=1
  JJ=1
ELSE
  JI=II
  JJ=IJ
END IF
!
!
DO JK=1,SIZE(PA,3)
 IF ( (PA(JI,JJ,JK) /= XUNDEF) .AND. (PA(JI+1,JJ,JK) /= XUNDEF) .AND. &
      (PA(JI,JJ+1,JK) /= XUNDEF) .AND. (PA(JI+1,JJ+1,JK) /= XUNDEF) ) THEN
   PB(JK) = (1.-ZYCOEF) * (1.-ZXCOEF) *  PA(JI,JJ,JK) + &
        (1.-ZYCOEF) * (ZXCOEF) *  PA(JI+1,JJ,JK)  + &
        (ZYCOEF) * (1.-ZXCOEF) *  PA(JI,JJ+1,JK)  + &
        (ZYCOEF) * (ZXCOEF) *  PA(JI+1,JJ+1,JK) 
 ELSE
   PB(JK) = XUNDEF 
 END IF    
END DO
!
END FUNCTION FLYER_INTERPZ
!----------------------------------------------------------------------------
FUNCTION FLYER_INTERP_U(PA) RESULT(PB)
!
REAL, DIMENSION(:,:,:), INTENT(IN) :: PA
REAL                               :: PB
!
INTEGER :: JI, JJ
!
IF (SIZE(PA,1)==2) THEN
  JI=1
  JJ=1
ELSE
  JI=IU
  JJ=IJ
END IF
!
PB = (1.- ZYCOEF) * (1.-ZUCOEF) * ( (1.-ZUCOEF00) * PA(JI  ,JJ  ,IU00) + ZUCOEF00 * PA(JI  ,JJ  ,IU00+1)) &
   + (1.- ZYCOEF) * (   ZUCOEF) * ( (1.-ZUCOEF10) * PA(JI+1,JJ  ,IU10) + ZUCOEF10 * PA(JI+1,JJ  ,IU10+1)) &
   + (    ZYCOEF) * (1.-ZUCOEF) * ( (1.-ZUCOEF01) * PA(JI  ,JJ+1,IU01) + ZUCOEF01 * PA(JI  ,JJ+1,IU01+1)) &
   + (    ZYCOEF) * (   ZUCOEF) * ( (1.-ZUCOEF11) * PA(JI+1,JJ+1,IU11) + ZUCOEF11 * PA(JI+1,JJ+1,IU11+1))
!
END FUNCTION FLYER_INTERP_U
!----------------------------------------------------------------------------
!----------------------------------------------------------------------------
FUNCTION FLYER_INTERP_V(PA) RESULT(PB)
!
REAL, DIMENSION(:,:,:), INTENT(IN) :: PA
REAL                               :: PB
!
INTEGER :: JI, JJ
!
IF (SIZE(PA,1)==2) THEN
  JI=1
  JJ=1
ELSE
  JI=II
  JJ=IV
END IF
!
PB = (1.- ZVCOEF) * (1.-ZXCOEF) * ( (1.-ZVCOEF00) * PA(JI  ,JJ  ,IV00) + ZVCOEF00 * PA(JI  ,JJ  ,IV00+1)) &
   + (1.- ZVCOEF) * (   ZXCOEF) * ( (1.-ZVCOEF10) * PA(JI+1,JJ  ,IV10) + ZVCOEF10 * PA(JI+1,JJ  ,IV10+1)) &
   + (    ZVCOEF) * (1.-ZXCOEF) * ( (1.-ZVCOEF01) * PA(JI  ,JJ+1,IV01) + ZVCOEF01 * PA(JI  ,JJ+1,IV01+1)) &
   + (    ZVCOEF) * (   ZXCOEF) * ( (1.-ZVCOEF11) * PA(JI+1,JJ+1,IV11) + ZVCOEF11 * PA(JI+1,JJ+1,IV11+1))
!
END FUNCTION FLYER_INTERP_V
!----------------------------------------------------------------------------
!----------------------------------------------------------------------------
FUNCTION FLYER_INTERP_2D(PA) RESULT(PB)
!
REAL, DIMENSION(:,:), INTENT(IN) :: PA
REAL                             :: PB
!
INTEGER :: JI, JJ
!
IF (SIZE(PA,1)==2) THEN
  JI=1
  JJ=1
ELSE
  JI=II
  JJ=IJ
END IF
!
PB = (1.- ZYCOEF) * (1.-ZXCOEF) * PA(JI  ,JJ  ) &
   + (1.- ZYCOEF) * (   ZXCOEF) * PA(JI+1,JJ  ) &
   + (    ZYCOEF) * (1.-ZXCOEF) * PA(JI  ,JJ+1) &
   + (    ZYCOEF) * (   ZXCOEF) * PA(JI+1,JJ+1)
!
END FUNCTION FLYER_INTERP_2D
!----------------------------------------------------------------------------
!----------------------------------------------------------------------------
SUBROUTINE DISTRIBUTE_FLYER(PA)
!
REAL, INTENT(INOUT) :: PA
!
PA = PA * ZTHIS_PROC
CALL REDUCESUM_ll(PA,IINFO_ll)
!
END SUBROUTINE DISTRIBUTE_FLYER
!----------------------------------------------------------------------------
!----------------------------------------------------------------------------
SUBROUTINE DISTRIBUTE_FLYER_N(KA)
!
INTEGER, INTENT(INOUT) :: KA
REAL                   :: ZA
!
ZA=KA
!
ZA = ZA * ZTHIS_PROC
CALL REDUCESUM_ll(ZA,IINFO_ll)
!
IF (NINT(ZA)/=0) KA=NINT(ZA)
!
END SUBROUTINE DISTRIBUTE_FLYER_N
!----------------------------------------------------------------------------
!----------------------------------------------------------------------------
SUBROUTINE DISTRIBUTE_FLYER_L(OA)
!
LOGICAL, INTENT(INOUT) :: OA
REAL                   :: ZA
!
ZA=0.
IF (OA) ZA=1.
!
CALL REDUCESUM_ll(ZA,IINFO_ll)
!
IF (ZA==0.) THEN
  OA=.FALSE.
ELSE
  OA=.TRUE.
END IF
!
END SUBROUTINE DISTRIBUTE_FLYER_L
!----------------------------------------------------------------------------
!----------------------------------------------------------------------------
SUBROUTINE FLYER_CHANGE_MODEL(IMI)
!
INTEGER, INTENT(IN) :: IMI ! model index
!
INTEGER :: IMK      ! kid model index
INTEGER :: IMODEL   ! TPFLYER model index at the beginning of the subroutine
INTEGER :: IU       ! U flux point balloon position (x index)
INTEGER :: IV       ! V flux point balloon position (y index)
INTEGER :: IU_ABS   ! U flux point balloon  position (in the model)
INTEGER :: IV_ABS   ! V flux point balloon position (in the model)
INTEGER :: IXOR     ! Origin's coordinates of the extended 2 way subdomain
INTEGER :: IYOR     ! Origin's coordinates of the extended 2 way subdomain
INTEGER :: IIB      ! current processor domain sizes
INTEGER :: IJB
INTEGER :: IIE
INTEGER :: IJE
!
!
IMODEL=TPFLYER%NMODEL
CALL GET_INDICE_ll (IIB,IJB,IIE,IJE)
IU=COUNT( PXHAT (:)<=TPFLYER%XX_CUR )
IV=COUNT( PYHAT (:)<=TPFLYER%XY_CUR )
ZTHIS_PROC=0.
IF (IU>=IIB .AND. IU<=IIE .AND. IV>=IJB .AND. IV<=IJE) ZTHIS_PROC=1.
IF (ZTHIS_PROC .EQ. 1) THEN
  CALL GET_OR_LL('B',IXOR,IYOR)
  IU_ABS=IU + IXOR - 1
  IV_ABS=IV + IYOR - 1 
  !
  IF (TPFLYER%NMODEL == IMI) THEN
    !
    ! go to the kid model if the flyer location is inside
    ! ------------------
    !
    DO IMK=IMI+1,NMODEL
      IF (NDAD(IMK) == IMI .AND. &
         IU_ABS>=NXOR_ALL(IMK)  .AND. IU_ABS<=NXEND_ALL(IMK)  .AND. &
         IV_ABS>=NYOR_ALL(IMK)  .AND. IV_ABS<=NYEND_ALL(IMK) ) THEN
        TPFLYER%NMODEL = IMK
        !
      END IF
    END DO
    !
  ELSE
    !
    ! come from the kid model if the flyer location is outside
    ! ------------------
    !
    IMK = TPFLYER%NMODEL
    IF (IU_ABS<NXOR_ALL(IMK)  .OR. IU_ABS>NXEND_ALL(IMK)  .OR. &
         IV_ABS<NYOR_ALL(IMK)  .OR. IV_ABS>NYEND_ALL(IMK) ) THEN
        TPFLYER%NMODEL = IMI
        !
    END IF
  END IF
END IF
!
! send the information to all the processors
! ----------------------------------------
!
CALL DISTRIBUTE_FLYER_N(TPFLYER%NMODEL)
ZTHIS_PROC=0.
!
! print if the model changes
!---------------------------------
IF (TPFLYER%NMODEL /= IMODEL) THEN
   IF (NDAD(IMODEL) == TPFLYER%NMODEL) THEN
      WRITE(ILUOUT,*) '-------------------------------------------------------------------'
      WRITE(ILUOUT,*) TPFLYER%CTITLE,' comes from model ',IMODEL,' in  model ', &
                      TPFLYER%NMODEL,' at ',NINT(TDTCUR%xtime),' sec.'
      WRITE(ILUOUT,*) '-------------------------------------------------------------------'
   ELSE
      WRITE(ILUOUT,*) '-------------------------------------------------------------------'
      WRITE(ILUOUT,*) TPFLYER%CTITLE,' goes from model ',IMODEL,' to  model ', &
                      TPFLYER%NMODEL,' at ',NINT(TDTCUR%xtime),' sec.'
      WRITE(ILUOUT,*) '-------------------------------------------------------------------'
   ENDIF
ENDIF
!
!
END SUBROUTINE FLYER_CHANGE_MODEL
!----------------------------------------------------------------------------
!----------------------------------------------------------------------------
END SUBROUTINE AIRCRAFT_BALLOON_EVOL
