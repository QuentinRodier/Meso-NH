!MNH_LIC Copyright 2000-2022 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!      ##########################
MODULE MODE_AIRCRAFT_BALLOON_EVOL
!      ##########################

USE MODE_MSG

IMPLICIT NONE

PRIVATE

PUBLIC :: AIRCRAFT_BALLOON_EVOL

CONTAINS
!     ########################################################
      SUBROUTINE AIRCRAFT_BALLOON_EVOL(PTSTEP,               &
                       PZ, PMAP, PLONOR, PLATOR,             &
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
USE MODD_GRID_n
USE MODD_IO,               ONLY: ISP
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
USE MODD_TIME,             only: TDTSEG
USE MODD_TIME_n,           only: tdtcur
USE MODD_TURB_FLUX_AIRCRAFT_BALLOON
!
USE MODE_DATETIME
USE MODE_FGAU,             ONLY: GAULAG
USE MODE_FSCATTER,         ONLY: QEPSW,QEPSI,BHMIE,MOMG,MG
USE MODE_GRIDPROJ
USE MODE_ll
USE MODE_POSITION_TOOLS,   ONLY: FIND_PROCESS_AND_MODEL_FROM_XY_POS
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
!       0.2  declaration of local variables
!
!
INTEGER :: IMI        ! model index
REAL    :: ZTHIS_PROC ! 1 if balloon is currently treated by this proc., else 0
!
INTEGER :: IIB        ! current process domain sizes
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
INTEGER :: II_M     ! mass balloon position (x index)
INTEGER :: IJ_M     ! mass balloon position (y index)
INTEGER :: II_U     ! U flux point balloon position (x index)
INTEGER :: IJ_V     ! V flux point balloon position (y index)
INTEGER :: IDU      ! difference between II_U and II_M
INTEGER :: IDV      ! difference between IJ_V and IJ_M
!
INTEGER :: IK00     ! balloon position for II_M  , IJ_M
INTEGER :: IK01     ! balloon position for II_M  , IJ_M+1
INTEGER :: IK10     ! balloon position for II_M+1, IJ_M
INTEGER :: IK11     ! balloon position for II_M+1, IJ_M+1
INTEGER :: IU00     ! balloon position for II_U  , IJ_M
INTEGER :: IU01     ! balloon position for II_U  , IJ_M+1
INTEGER :: IU10     ! balloon position for II_U+1, IJ_M
INTEGER :: IU11     ! balloon position for II_U+1, IJ_M+1
INTEGER :: IV00     ! balloon position for II_M  , IJ_V
INTEGER :: IV01     ! balloon position for II_M  , IJ_V+1
INTEGER :: IV10     ! balloon position for II_M+1, IJ_V
INTEGER :: IV11     ! balloon position for II_M+1, IJ_V+1
!
REAL :: ZXCOEF      ! X direction interpolation coefficient
REAL :: ZUCOEF      ! X direction interpolation coefficient (for U)
REAL :: ZYCOEF      ! Y direction interpolation coefficient
REAL :: ZVCOEF      ! Y direction interpolation coefficient (for V)
!
REAL :: ZZCOEF00    ! Z direction interpolation coefficient for II_M  , IJ_M
REAL :: ZZCOEF01    ! Z direction interpolation coefficient for II_M  , IJ_M+1
REAL :: ZZCOEF10    ! Z direction interpolation coefficient for II_M+1, IJ_M
REAL :: ZZCOEF11    ! Z direction interpolation coefficient for II_M+1, IJ_M+1
REAL :: ZUCOEF00    ! Z direction interpolation coefficient for II_U  , IJ_M
REAL :: ZUCOEF01    ! Z direction interpolation coefficient for II_U  , IJ_M+1
REAL :: ZUCOEF10    ! Z direction interpolation coefficient for II_U+1, IJ_M
REAL :: ZUCOEF11    ! Z direction interpolation coefficient for II_U+1, IJ_M+1
REAL :: ZVCOEF00    ! Z direction interpolation coefficient for II_M  , IJ_V
REAL :: ZVCOEF01    ! Z direction interpolation coefficient for II_M  , IJ_V+1
REAL :: ZVCOEF10    ! Z direction interpolation coefficient for II_M+1, IJ_V
REAL :: ZVCOEF11    ! Z direction interpolation coefficient for II_M+1, IJ_V+1
!
INTEGER :: ISTORE          ! time index for storage
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
!
INTEGER :: IRANK
INTEGER :: IMODEL
INTEGER :: IMODEL_OLD
REAL    :: ZX_OLD, ZY_OLD
REAL    :: ZDELTATIME
REAL    :: ZTSTEP
REAL    :: ZDIVTMP

!----------------------------------------------------------------------------
IKU = SIZE(PZ,3)

CALL GET_MODEL_NUMBER_ll(IMI)

SELECT TYPE ( TPFLYER )
  CLASS IS ( TAIRCRAFTDATA)

    !Do the positioning only if model 1 (data will be available to others after)
    MODEL1: IF ( IMI == 1 ) THEN
      !Do we have to store aircraft data?
      CALL  STATPROF_INSTANT( TPFLYER%TFLYER_TIME, ISTORE )
      IF ( ISTORE < 1 ) THEN
        !No profiler storage at this time step
        TPFLYER%LSTORE = .FALSE.
        RETURN
      ELSE
        TPFLYER%LSTORE = .TRUE.
      END IF

      ! Is the aircraft in flight ?
      IF ( TDTCUR >= TPFLYER%TLAUNCH .AND. TDTCUR <= TPFLYER%TLAND ) THEN
        TPFLYER%LFLY = .TRUE.

        ! Find the flight segment
        ZTDIST = TDTCUR - TPFLYER%TLAUNCH
        IL = TPFLYER%NPOSCUR
        DO WHILE ( ZTDIST > TPFLYER%XPOSTIME(IL+1) )
          IL = IL + 1
          IF ( IL > TPFLYER%NPOS-1 ) THEN
            !Security (should not happen)
            IL = TPFLYER%NPOS-1
            EXIT
          END IF
        END DO
        TPFLYER%NPOSCUR = IL

        ! Compute the current position
        ZSEG_FRAC = ( ZTDIST - TPFLYER%XPOSTIME(IL) ) / ( TPFLYER%XPOSTIME(IL+1) - TPFLYER%XPOSTIME(IL) )

        TPFLYER%XX_CUR = (1.-ZSEG_FRAC) * TPFLYER%XPOSX(IL  ) &
                       +     ZSEG_FRAC  * TPFLYER%XPOSX(IL+1)
        TPFLYER%XY_CUR = (1.-ZSEG_FRAC) * TPFLYER%XPOSY(IL  ) &
                       +     ZSEG_FRAC  * TPFLYER%XPOSY(IL+1)
        IF (TPFLYER%LALTDEF) THEN
          TPFLYER%XP_CUR = (1.-ZSEG_FRAC) * TPFLYER%XPOSP(IL  ) &
                         +     ZSEG_FRAC  * TPFLYER%XPOSP(IL+1)
        ELSE
          TPFLYER%XZ_CUR = (1.-ZSEG_FRAC) * TPFLYER%XPOSZ(IL ) &
                         +     ZSEG_FRAC  * TPFLYER%XPOSZ(IL +1 )
        END IF

        ! Get rank of the process where the aircraft is and the model number
        IF ( TPFLYER%CMODEL == 'FIX' ) THEN
          IMODEL = TPFLYER%NMODEL
        ELSE
          IMODEL = 0
        END IF
        CALL FIND_PROCESS_AND_MODEL_FROM_XY_POS( TPFLYER%XX_CUR, TPFLYER%XY_CUR, IRANK, IMODEL )
        IF ( IRANK < 1 ) THEN
          TPFLYER%NMODEL = 1 !Set to 1 because it is always a valid model (to prevent crash if NDAD(TPFLYER%NMODEL) )
          TPFLYER%LCRASH = .TRUE.
          TPFLYER%NCRASH = NCRASH_OUT_HORIZ
          TPFLYER%LFLY   = .FALSE.
        ELSE
          TPFLYER%NMODEL = IMODEL
          TPFLYER%LCRASH = .FALSE.
          TPFLYER%NCRASH = NCRASH_NO
          TPFLYER%NRANK_CUR = IRANK
        END IF
      ELSE
        TPFLYER%LFLY = .FALSE.

      END IF
    END IF MODEL1

    IF ( IMI == TPFLYER%NMODEL .AND. TPFLYER%LFLY ) THEN
      ! Check if it is the right moment to store data
      IF ( TPFLYER%LSTORE ) THEN
        ISTORE = TPFLYER%TFLYER_TIME%N_CUR
        IF ( ABS( TDTCUR - TPFLYER%TFLYER_TIME%TPDATES(ISTORE) ) < 1e-10 ) THEN

          ISOWNER: IF ( TPFLYER%NRANK_CUR == ISP ) THEN
            ZTHIS_PROC = 1.
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
            !
            !
            !*      2.2  Interpolations of model variables to mass points
            !            ------------------------------------------------
            !
            IIU=SIZE(XXHAT)
            IJU=SIZE(XYHAT)
            ! X position
            II_U = COUNT( XXHAT (:) <= TPFLYER%XX_CUR )
            II_M = COUNT( XXHATM(:) <= TPFLYER%XX_CUR )

            ! Y position
            IJ_V=COUNT( XYHAT (:)<=TPFLYER%XY_CUR )
            IJ_M=COUNT( XYHATM(:)<=TPFLYER%XY_CUR )
            !
            !*      4.5  Interpolations of model variables to mass points
            !            ------------------------------------------------
            !
            ZZM(:,:,1:IKU-1)=0.5 *PZ(II_M  :II_M+1,IJ_M  :IJ_M+1,1:IKU-1)+0.5 *PZ(II_M  :II_M+1,IJ_M  :IJ_M+1,2:IKU  )
            ZZM(:,:,  IKU  )=1.5 *PZ(II_M  :II_M+1,IJ_M  :IJ_M+1,  IKU-1)-0.5 *PZ(II_M  :II_M+1,IJ_M  :IJ_M+1,  IKU-2)

        IDU = II_U - II_M
        ZZU(:,:,1:IKU-1)=0.25*PZ(IDU+II_M-1:IDU+II_M,  IJ_M  :IJ_M+1,1:IKU-1)+0.25*PZ(IDU+II_M-1:IDU+II_M  ,IJ_M  :IJ_M+1,2:IKU  ) &
                      +0.25*PZ(IDU+II_M  :IDU+II_M+1,IJ_M  :IJ_M+1,1:IKU-1)+0.25*PZ(IDU+II_M  :IDU+II_M+1,IJ_M  :IJ_M+1,2:IKU  )
        ZZU(:,:,  IKU  )=0.75*PZ(IDU+II_M-1:IDU+II_M  ,IJ_M  :IJ_M+1,  IKU-1)-0.25*PZ(IDU+II_M-1:IDU+II_M  ,IJ_M  :IJ_M+1,  IKU-2) &
                      +0.75*PZ(IDU+II_M  :IDU+II_M+1,IJ_M  :IJ_M+1,  IKU-1)-0.25*PZ(IDU+II_M  :IDU+II_M+1,IJ_M  :IJ_M+1,  IKU-2)

        IDV = IJ_V - IJ_M
        ZZV(:,:,1:IKU-1)=0.25*PZ(II_M  :II_M+1,IDV+IJ_M-1:IDV+IJ_M  ,1:IKU-1)+0.25*PZ(II_M  :II_M+1,IDV+IJ_M-1:IDV+IJ_M  ,2:IKU  ) &
                      +0.25*PZ(II_M  :II_M+1,IDV+IJ_M  :IDV+IJ_M+1,1:IKU-1)+0.25*PZ(II_M  :II_M+1,IDV+IJ_M  :IDV+IJ_M+1,2:IKU  )
        ZZV(:,:,  IKU  )=0.75*PZ(II_M  :II_M+1,IDV+IJ_M-1:IDV+IJ_M  ,  IKU-1)-0.25*PZ(II_M  :II_M+1,IDV+IJ_M-1:IDV+IJ_M  ,  IKU-2) &
                      +0.75*PZ(II_M  :II_M+1,IDV+IJ_M  :IDV+IJ_M+1,  IKU-1)-0.25*PZ(II_M  :II_M+1,IDV+IJ_M  :IDV+IJ_M+1,  IKU-2)

            ZWM(:,:,1:IKU-1)=0.5*PW(II_M:II_M+1,IJ_M:IJ_M+1,1:IKU-1)+0.5*PW(II_M:II_M+1,IJ_M:IJ_M+1,2:IKU  )
            ZWM(:,:,  IKU  )=1.5*PW(II_M:II_M+1,IJ_M:IJ_M+1,  IKU-1)-0.5*PW(II_M:II_M+1,IJ_M:IJ_M+1,  IKU-2)
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
            ZEXN(:,:,:    ) = (PP(II_M:II_M+1,IJ_M:IJ_M+1,:)/XP00)**(XRD/XCPD)
            DO JK=IKB-1,1,-1
              ZEXN(:,:,JK) = 1.5 * ZEXN(:,:,JK+1) - 0.5 * ZEXN(:,:,JK+2)
            END DO
            DO JK=IKE+1,IKU
              ZEXN(:,:,JK) = 1.5 * ZEXN(:,:,JK-1) - 0.5 * ZEXN(:,:,JK-2)
            END DO
            !
            ! IF ( TPFLYER%CTYPE == 'ISODEN' .OR. TPFLYER%CTYPE == 'CVBALL' .OR. TPFLYER%CTYPE == 'AIRCRA' ) THEN
            ZTHV(:,:,:) = PTH(II_M:II_M+1,IJ_M:IJ_M+1,:)
            IF (SIZE(PR,4)>0)                                                     &
            ZTHV(:,:,:) = ZTHV(:,:,:) * ( 1. + XRV/XRD*PR(II_M:II_M+1,IJ_M:IJ_M+1,:,1) )  &
                                      / ( 1. + WATER_SUM(PR(II_M:II_M+1,IJ_M:IJ_M+1,:,:)) )
            !
            ZTV (:,:,:) = ZTHV(:,:,:) * ZEXN(:,:,:)
            ZRHO(:,:,:) = PP(II_M:II_M+1,IJ_M:IJ_M+1,:) / (XRD*ZTV(:,:,:))
            DO JK=IKB-1,1,-1
              ZRHO(:,:,JK) = 1.5 * ZRHO(:,:,JK+1) - 0.5 * ZRHO(:,:,JK+2)
            END DO
            DO JK=IKE+1,IKU
              ZRHO(:,:,JK) = 1.5 * ZRHO(:,:,JK-1) - 0.5 * ZRHO(:,:,JK-2)
            END DO
            ZTHW_FLUX(:,:,:) = ZRHO(:,:,:)*XCPD *XTHW_FLUX(II_M:II_M+1,IJ_M:IJ_M+1,:)
            ZRCW_FLUX(:,:,:) = ZRHO(:,:,:)*XLVTT*XRCW_FLUX(II_M:II_M+1,IJ_M:IJ_M+1,:)
            ZSVW_FLUX(:,:,:,:) = XSVW_FLUX(II_M:II_M+1,IJ_M:IJ_M+1,:,:)
            ! END IF

            ! Vertical position
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

            IF ( ANY( [ IK00, IK01, IK10, IK11 ] < IKB ) ) THEN
              !Minimum altitude is on the ground at IKB (no crash if too low)
              IK00 = MAX ( IK00, IKB )
              IK01 = MAX ( IK01, IKB )
              IK10 = MAX ( IK10, IKB )
              IK11 = MAX ( IK11, IKB )

              CMNHMSG(1) = 'flyer ' // TRIM( TPFLYER%CTITLE ) // ' is near the ground'
              WRITE( CMNHMSG(2), "( 'at ', I2, '/', I2, '/', I4, ' ', F18.12, 's' )" ) &
                     TDTCUR%NDAY, TDTCUR%NMONTH, TDTCUR%NYEAR, TDTCUR%XTIME
              CALL PRINT_MSG( NVERB_INFO, 'GEN', 'AIRCRAFT_BALLOON_EVOL', OLOCAL = .TRUE. )
            END IF

            IF (IK00 >= IKE .OR. IK01 >= IKE .OR. IK10 >= IKE .OR. IK11 >= IKE  ) THEN
              TPFLYER%LCRASH = .TRUE.
              TPFLYER%NCRASH = NCRASH_OUT_HIGH
            END IF
            !
            !*      6.   INITIALIZATIONS FOR INTERPOLATIONS
            !            ----------------------------------
            !
            !*      6.1  Interpolation coefficient for X
            !            -------------------------------
            !
            ZXCOEF = (TPFLYER%XX_CUR - XXHATM(II_M)) / (XXHATM(II_M+1) - XXHATM(II_M))
            ZXCOEF = MAX (0.,MIN(ZXCOEF,1.))
            !
            !
            !*      6.2  Interpolation coefficient for y
            !            -------------------------------
            !
            ZYCOEF = (TPFLYER%XY_CUR - XYHATM(IJ_M)) / (XYHATM(IJ_M+1) - XYHATM(IJ_M))
            ZYCOEF = MAX (0.,MIN(ZYCOEF,1.))
            !
            !
            !*      6.3  Interpolation coefficients for the 4 suroundings verticals
            !            ----------------------------------------------------------
            !
            !       SELECT TYPE ( TPFLYER )
            !         CLASS IS ( TBALLOONDATA)
            !           IF ( TPFLYER%CTYPE == 'ISODEN' ) THEN
            !             ZZCOEF00 = (TPFLYER%XRHO - ZRHO(1,1,IK00)) / ( ZRHO(1,1,IK00+1) - ZRHO(1,1,IK00) )
            !             ZZCOEF01 = (TPFLYER%XRHO - ZRHO(1,2,IK01)) / ( ZRHO(1,2,IK01+1) - ZRHO(1,2,IK01) )
            !             ZZCOEF10 = (TPFLYER%XRHO - ZRHO(2,1,IK10)) / ( ZRHO(2,1,IK10+1) - ZRHO(2,1,IK10) )
            !             ZZCOEF11 = (TPFLYER%XRHO - ZRHO(2,2,IK11)) / ( ZRHO(2,2,IK11+1) - ZRHO(2,2,IK11) )
            !             TPFLYER%XZ_CUR = FLYER_INTERP(ZZM)
            !           ELSE IF ( TPFLYER%CTYPE == 'RADIOS' .OR. TPFLYER%CTYPE == 'CVBALL' ) THEN
            !             ZZCOEF00 = (TPFLYER%XZ_CUR - ZZM(1,1,IK00)) / ( ZZM(1,1,IK00+1) - ZZM(1,1,IK00) )
            !             ZZCOEF01 = (TPFLYER%XZ_CUR - ZZM(1,2,IK01)) / ( ZZM(1,2,IK01+1) - ZZM(1,2,IK01) )
            !             ZZCOEF10 = (TPFLYER%XZ_CUR - ZZM(2,1,IK10)) / ( ZZM(2,1,IK10+1) - ZZM(2,1,IK10) )
            !             ZZCOEF11 = (TPFLYER%XZ_CUR - ZZM(2,2,IK11)) / ( ZZM(2,2,IK11+1) - ZZM(2,2,IK11) )
            !           END IF
            !
            !         CLASS IS ( TAIRCRAFTDATA)
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
            ! END SELECT
            !
            !----------------------------------------------------------------------------
            !
            !*      7.   INITIALIZATIONS FOR INTERPOLATIONS OF U AND V
            !            ---------------------------------------------
            !
            !*      7.1  Interpolation coefficient for X (for U)
            !            -------------------------------
            !
            ZUCOEF = (TPFLYER%XX_CUR - XXHAT(II_U)) / (XXHAT(II_U+1) - XXHAT(II_U))
            ZUCOEF = MAX(0.,MIN(ZUCOEF,1.))
            !
            !
            !*      7.2  Interpolation coefficient for y (for V)
            !            -------------------------------
            !
            ZVCOEF = (TPFLYER%XY_CUR - XYHAT(IJ_V)) / (XYHAT(IJ_V+1) - XYHAT(IJ_V))
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
            !
            !*      8.   DATA RECORDING
            !            --------------
            !
            TPFLYER%NMODELHIST(ISTORE) = TPFLYER%NMODEL
            TPFLYER%XX   (ISTORE) = TPFLYER%XX_CUR
            TPFLYER%XY   (ISTORE) = TPFLYER%XY_CUR
            TPFLYER%XZ   (ISTORE) = TPFLYER%XZ_CUR
            !
            CALL SM_LATLON(PLATOR,PLONOR,          &
                        TPFLYER%XX_CUR, TPFLYER%XY_CUR,       &
                        TPFLYER%XLAT(ISTORE), TPFLYER%XLON(ISTORE)  )
            !
            ZU_BAL = FLYER_INTERP_U(PU)
            ZV_BAL = FLYER_INTERP_V(PV)
            ZGAM   = (XRPK * (TPFLYER%XLON(ISTORE) - XLON0) - XBETA)*(XPI/180.)
            TPFLYER%XZON (ISTORE) = ZU_BAL * COS(ZGAM) + ZV_BAL * SIN(ZGAM)
            TPFLYER%XMER (ISTORE) = - ZU_BAL * SIN(ZGAM) + ZV_BAL * COS(ZGAM)
            !
            TPFLYER%XW   (ISTORE) = FLYER_INTERP(ZWM)
            TPFLYER%XTH  (ISTORE) = FLYER_INTERP(PTH)
            !
            ZFLYER_EXN = FLYER_INTERP(ZEXN)
            TPFLYER%XP   (ISTORE) = XP00 * ZFLYER_EXN**(XCPD/XRD)

            ZR(:,:,:) = 0.
            DO JLOOP=1,SIZE(PR,4)
              TPFLYER%XR   (ISTORE,JLOOP) = FLYER_INTERP(PR(:,:,:,JLOOP))
              IF (JLOOP>=2) ZR(:,:,:) = ZR(:,:,:) + PR(:,:,:,JLOOP)
            END DO
            DO JLOOP=1,SIZE(PSV,4)
              TPFLYER%XSV  (ISTORE,JLOOP) = FLYER_INTERP(PSV(:,:,:,JLOOP))
            END DO
            TPFLYER%XRTZ  (ISTORE,:) = FLYER_INTERPZ(ZR(:,:,:))
            DO JLOOP=1,SIZE(PR,4)
              TPFLYER%XRZ  (ISTORE,:,JLOOP) = FLYER_INTERPZ(PR(:,:,:,JLOOP))
            END DO
            ! Fin Modifs ON
            TPFLYER%XFFZ  (ISTORE,:) = FLYER_INTERPZ(SQRT(PU**2+PV**2))
            IF (CCLOUD=="LIMA") THEN
              TPFLYER%XCIZ  (ISTORE,:) = FLYER_INTERPZ(PSV(:,:,:,NSV_LIMA_NI))
              TPFLYER%XCCZ  (ISTORE,:) = FLYER_INTERPZ(PSV(:,:,:,NSV_LIMA_NC))
              TPFLYER%XCRZ  (ISTORE,:) = FLYER_INTERPZ(PSV(:,:,:,NSV_LIMA_NR))
            ELSE IF ( CCLOUD=="ICE3" .OR. CCLOUD=="ICE4" ) THEN
              TPFLYER%XCIZ  (ISTORE,:) = FLYER_INTERPZ(PCIT(:,:,:))
            ENDIF
            ! initialization CRARE and CRARE_ATT + LWC and IWC
            TPFLYER%XCRARE(ISTORE,:) = 0.
            TPFLYER%XCRARE_ATT(ISTORE,:) = 0.
            TPFLYER%XLWCZ  (ISTORE,:) = 0.
            TPFLYER%XIWCZ  (ISTORE,:) = 0.
            IF (CCLOUD=="LIMA" .OR. CCLOUD=="ICE3" ) THEN ! only for ICE3 and LIMA
              TPFLYER%XLWCZ  (ISTORE,:) = FLYER_INTERPZ((PR(:,:,:,2)+PR(:,:,:,3))*PRHODREF(:,:,:))
              TPFLYER%XIWCZ  (ISTORE,:) = FLYER_INTERPZ((PR(:,:,:,4)+PR(:,:,:,5)+PR(:,:,:,6))*PRHODREF(:,:,:))
              ZTEMPZ(:)=FLYER_INTERPZ(PTH(II_M:II_M+1,IJ_M:IJ_M+1,:) * ZEXN(:,:,:))
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
                    TPFLYER%XCRARE(ISTORE,JK)=TPFLYER%XCRARE(ISTORE,JK)+ZREFLOC
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
                TPFLYER%XCRARE_ATT(ISTORE,JK)=TPFLYER%XCRARE(ISTORE,JK)*ZAETOT
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
                TPFLYER%XCRARE_ATT(ISTORE,JK)=TPFLYER%XCRARE(ISTORE,JK)*ZAETOT
              END DO
              TPFLYER%XZZ  (ISTORE,:) = ZZMZ(:)
              DEALLOCATE(ZZMZ,ZAELOC)
              ! m^3 → mm^6/m^3 → dBZ
              WHERE(TPFLYER%XCRARE(ISTORE,:)>0)
                TPFLYER%XCRARE(ISTORE,:)=10.*LOG10(1.E18*TPFLYER%XCRARE(ISTORE,:))
              ELSEWHERE
                TPFLYER%XCRARE(ISTORE,:)=XUNDEF
              END WHERE
              WHERE(TPFLYER%XCRARE_ATT(ISTORE,:)>0)
                TPFLYER%XCRARE_ATT(ISTORE,:)=10.*LOG10(1.E18*TPFLYER%XCRARE_ATT(ISTORE,:))
              ELSEWHERE
                TPFLYER%XCRARE_ATT(ISTORE,:)=XUNDEF
              END WHERE
              DEALLOCATE(ZX,ZW,ZRTMIN)
            END IF ! end LOOP ICE3
            ! vertical wind
            TPFLYER%XWZ  (ISTORE,:) = FLYER_INTERPZ(ZWM(:,:,:))
            IF (SIZE(PTKE)>0) TPFLYER%XTKE  (ISTORE)    = FLYER_INTERP(PTKE)
            IF (SIZE(PTS) >0) TPFLYER%XTSRAD(ISTORE)    = FLYER_INTERP_2D(PTS)
            IF (LDIAG_IN_RUN) TPFLYER%XTKE_DISS(ISTORE) = FLYER_INTERP(XCURRENT_TKE_DISS)
            TPFLYER%XZS(ISTORE)  = FLYER_INTERP_2D(PZ(:,:,1+JPVEXT))
            TPFLYER%XTHW_FLUX(ISTORE) = FLYER_INTERP(ZTHW_FLUX)
            TPFLYER%XRCW_FLUX(ISTORE) = FLYER_INTERP(ZRCW_FLUX)
            DO JLOOP=1,SIZE(PSV,4)
            TPFLYER%XSVW_FLUX(ISTORE,JLOOP) = FLYER_INTERP(ZSVW_FLUX(:,:,:,JLOOP))
            END DO

          ELSE ISOWNER

            ZTHIS_PROC = 0.

          END IF ISOWNER

!----------------------------------------------------------------------------
          !
          !*     11.   EXCHANGE OF INFORMATION BETWEEN PROCESSES
          !            -----------------------------------------
          !
          !*     11.1  current position
          !            ----------------
          !
          !
          !*     11.2  data stored
          !            -----------
          !
          ! IF ( GSTORE ) THEN
          CALL DISTRIBUTE_FLYER_N(TPFLYER%NMODELHIST(ISTORE))
          CALL DISTRIBUTE_FLYER(TPFLYER%XX  (ISTORE))
          CALL DISTRIBUTE_FLYER(TPFLYER%XY  (ISTORE))
          CALL DISTRIBUTE_FLYER(TPFLYER%XZ  (ISTORE))
          CALL DISTRIBUTE_FLYER(TPFLYER%XLON(ISTORE))
          CALL DISTRIBUTE_FLYER(TPFLYER%XLAT(ISTORE))
          CALL DISTRIBUTE_FLYER(TPFLYER%XZON(ISTORE))
          CALL DISTRIBUTE_FLYER(TPFLYER%XMER(ISTORE))
          CALL DISTRIBUTE_FLYER(TPFLYER%XW  (ISTORE))
          CALL DISTRIBUTE_FLYER(TPFLYER%XP  (ISTORE))
          CALL DISTRIBUTE_FLYER(TPFLYER%XTH (ISTORE))
          DO JLOOP=1,SIZE(PR,4)
            CALL DISTRIBUTE_FLYER(TPFLYER%XR   (ISTORE,JLOOP))
          END DO
          DO JLOOP=1,SIZE(PSV,4)
            CALL DISTRIBUTE_FLYER(TPFLYER%XSV  (ISTORE,JLOOP))
          END DO
          DO JLOOP=1,IKU
            CALL DISTRIBUTE_FLYER(TPFLYER%XRTZ (ISTORE,JLOOP))
            DO JLOOP2=1,SIZE(PR,4)
              CALL DISTRIBUTE_FLYER(TPFLYER%XRZ (ISTORE,JLOOP,JLOOP2))
            ENDDO
            CALL DISTRIBUTE_FLYER(TPFLYER%XFFZ (ISTORE,JLOOP))
            CALL DISTRIBUTE_FLYER(TPFLYER%XCIZ (ISTORE,JLOOP))
            IF (CCLOUD== 'LIMA' ) THEN
              CALL DISTRIBUTE_FLYER(TPFLYER%XCRZ (ISTORE,JLOOP))
              CALL DISTRIBUTE_FLYER(TPFLYER%XCCZ (ISTORE,JLOOP))
            ENDIF
            CALL DISTRIBUTE_FLYER(TPFLYER%XIWCZ (ISTORE,JLOOP))
            CALL DISTRIBUTE_FLYER(TPFLYER%XLWCZ (ISTORE,JLOOP))
            CALL DISTRIBUTE_FLYER(TPFLYER%XCRARE (ISTORE,JLOOP))
            CALL DISTRIBUTE_FLYER(TPFLYER%XCRARE_ATT (ISTORE,JLOOP))
            CALL DISTRIBUTE_FLYER(TPFLYER%XWZ (ISTORE,JLOOP))
            CALL DISTRIBUTE_FLYER(TPFLYER%XZZ (ISTORE,JLOOP))
          END DO
          IF (SIZE(PTKE)>0) CALL DISTRIBUTE_FLYER(TPFLYER%XTKE  (ISTORE))
          IF (SIZE(PTS) >0) CALL DISTRIBUTE_FLYER(TPFLYER%XTSRAD(ISTORE))
          IF (LDIAG_IN_RUN) CALL DISTRIBUTE_FLYER(TPFLYER%XTKE_DISS(ISTORE))
          CALL DISTRIBUTE_FLYER(TPFLYER%XZS  (ISTORE))
          CALL DISTRIBUTE_FLYER(TPFLYER%XTHW_FLUX(ISTORE))
          CALL DISTRIBUTE_FLYER(TPFLYER%XRCW_FLUX(ISTORE))
          DO JLOOP=1,SIZE(PSV,4)
            CALL DISTRIBUTE_FLYER(TPFLYER%XSVW_FLUX(ISTORE,JLOOP))
          END DO
        END IF
      END IF
    END IF


  CLASS IS ( TBALLOONDATA)
    GLAUNCH = .FALSE. !Set to true only at the launch instant (set to false in flight after launch)

    ! Initialize model number (and rank)
    ! This is not done in initialisation phase because some data is not yet available at this early stage
    ! (XXHAT_ll of all models are needed by FIND_PROCESS_AND_MODEL_FROM_XY_POS)
    IF ( .NOT. TPFLYER%LFLY .AND. .NOT. TPFLYER%LCRASH .AND. TPFLYER%NRANK_CUR < 0 ) THEN
      ! Get rank of the process where the balloon is and the model number
      IF ( TPFLYER%CMODEL == 'FIX' ) THEN
        IMODEL = TPFLYER%NMODEL
      ELSE
        IMODEL = 0
      END IF
      CALL FIND_PROCESS_AND_MODEL_FROM_XY_POS( TPFLYER%XXLAUNCH, TPFLYER%XYLAUNCH, IRANK, IMODEL )

      IF ( IRANK < 1 ) THEN
        TPFLYER%NMODEL = 1 !Set to 1 because it is always a valid model (to prevent crash if NDAD(TPFLYER%NMODEL) )
        TPFLYER%LCRASH = .TRUE.
        TPFLYER%NCRASH = NCRASH_OUT_HORIZ
        TPFLYER%LFLY   = .FALSE.
        CALL PRINT_MSG( NVERB_WARNING, 'GEN', 'AIRCRAFT_BALLOON_EVOL', 'balloon ' // TRIM( TPFLYER%CTITLE ) &
                        // ': launch coordinates are outside of horizontal physical domain' )
      ELSE
        TPFLYER%NMODEL = IMODEL
        TPFLYER%LCRASH = .FALSE.
        TPFLYER%NCRASH = NCRASH_NO
        TPFLYER%NRANK_CUR = IRANK
      END IF
    END IF

    ! Launch?
    LAUNCH: IF ( .NOT. TPFLYER%LFLY .AND. .NOT. TPFLYER%LCRASH .AND. TPFLYER%NMODEL == IMI ) THEN
      LAUNCHTIME: IF ( ( TDTCUR - TPFLYER%TLAUNCH ) >= -1.e-10 ) THEN
        TPFLYER%LFLY = .TRUE.
        GLAUNCH = .TRUE.

        TPFLYER%XX_CUR = TPFLYER%XXLAUNCH
        TPFLYER%XY_CUR = TPFLYER%XYLAUNCH
        TPFLYER%TPOS_CUR = TDTCUR
        ! Get rank of the process where the balloon is and the model number
        IF ( TPFLYER%CMODEL == 'FIX' ) THEN
          IMODEL = TPFLYER%NMODEL
        ELSE
          IMODEL = 0
        END IF
        CALL FIND_PROCESS_AND_MODEL_FROM_XY_POS( TPFLYER%XX_CUR, TPFLYER%XY_CUR, IRANK, IMODEL )
        IF ( IRANK < 1 ) THEN
          TPFLYER%NMODEL = 1 !Set to 1 because it is always a valid model (to prevent crash if NDAD(TPFLYER%NMODEL) )
          TPFLYER%LCRASH = .TRUE.
          TPFLYER%NCRASH = NCRASH_OUT_HORIZ
          TPFLYER%LFLY   = .FALSE.
          WRITE( CMNHMSG(1), "( 'Balloon ', A, ' crashed the ', I2, '/', I2, '/', I4, ' at ', F18.12, &
                              's (out of the horizontal boundaries)' )" ) &
             TRIM( TPFLYER%CTITLE ), TDTCUR%NDAY, TDTCUR%NMONTH, TDTCUR%NYEAR, TDTCUR%XTIME
          CALL PRINT_MSG( NVERB_WARNING, 'GEN', 'AIRCRAFT_BALLOON_EVOL' )
        ELSE
          TPFLYER%NMODEL = IMODEL
          TPFLYER%LCRASH = .FALSE.
          TPFLYER%NCRASH = NCRASH_NO
          TPFLYER%NRANK_CUR = IRANK
        END IF
      END IF LAUNCHTIME
    END IF LAUNCH

    MODEL1BAL: IF ( TPFLYER%NMODEL == IMI .AND. &
      !Do we have to store balloon data?
      CALL  STATPROF_INSTANT( TPFLYER%TFLYER_TIME, ISTORE )
      IF ( ISTORE < 1 ) THEN
        !No profiler storage at this time step
        TPFLYER%LSTORE = .FALSE.
      ELSE
        TPFLYER%LSTORE = .TRUE.
      END IF
    END IF MODEL1BAL

    ! In flight
    INFLIGHTONMODEL: IF ( TPFLYER%LFLY .AND. .NOT. TPFLYER%LCRASH .AND. TPFLYER%NMODEL == IMI &
                          .AND. ABS( TPFLYER%TPOS_CUR - TDTCUR ) < 1.e-8 ) THEN

      ISOWNERBAL: IF ( TPFLYER%NRANK_CUR == ISP ) THEN
        ZTHIS_PROC = 1.
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
        !
        !
        !*      2.2  Interpolations of model variables to mass points
        !            ------------------------------------------------
        !
        IIU=SIZE(XXHAT)
        IJU=SIZE(XYHAT)
        ! X position
        II_U = COUNT( XXHAT (:) <= TPFLYER%XX_CUR )
        II_M = COUNT( XXHATM(:) <= TPFLYER%XX_CUR )

        ! Y position
        IJ_V=COUNT( XYHAT (:)<=TPFLYER%XY_CUR )
        IJ_M=COUNT( XYHATM(:)<=TPFLYER%XY_CUR )
        !
        !*      4.5  Interpolations of model variables to mass points
        !            ------------------------------------------------
        !
        ZZM(:,:,1:IKU-1)=0.5 *PZ(II_M  :II_M+1,IJ_M  :IJ_M+1,1:IKU-1)+0.5 *PZ(II_M  :II_M+1,IJ_M  :IJ_M+1,2:IKU  )
        ZZM(:,:,  IKU  )=1.5 *PZ(II_M  :II_M+1,IJ_M  :IJ_M+1,  IKU-1)-0.5 *PZ(II_M  :II_M+1,IJ_M  :IJ_M+1,  IKU-2)

        IDU = II_U - II_M
        ZZU(:,:,1:IKU-1)=0.25*PZ(IDU+II_M-1:IDU+II_M,  IJ_M  :IJ_M+1,1:IKU-1)+0.25*PZ(IDU+II_M-1:IDU+II_M  ,IJ_M  :IJ_M+1,2:IKU  ) &
                      +0.25*PZ(IDU+II_M  :IDU+II_M+1,IJ_M  :IJ_M+1,1:IKU-1)+0.25*PZ(IDU+II_M  :IDU+II_M+1,IJ_M  :IJ_M+1,2:IKU  )
        ZZU(:,:,  IKU  )=0.75*PZ(IDU+II_M-1:IDU+II_M  ,IJ_M  :IJ_M+1,  IKU-1)-0.25*PZ(IDU+II_M-1:IDU+II_M  ,IJ_M  :IJ_M+1,  IKU-2) &
                      +0.75*PZ(IDU+II_M  :IDU+II_M+1,IJ_M  :IJ_M+1,  IKU-1)-0.25*PZ(IDU+II_M  :IDU+II_M+1,IJ_M  :IJ_M+1,  IKU-2)

        IDV = IJ_V - IJ_M
        ZZV(:,:,1:IKU-1)=0.25*PZ(II_M  :II_M+1,IDV+IJ_M-1:IDV+IJ_M  ,1:IKU-1)+0.25*PZ(II_M  :II_M+1,IDV+IJ_M-1:IDV+IJ_M  ,2:IKU  ) &
                      +0.25*PZ(II_M  :II_M+1,IDV+IJ_M  :IDV+IJ_M+1,1:IKU-1)+0.25*PZ(II_M  :II_M+1,IDV+IJ_M  :IDV+IJ_M+1,2:IKU  )
        ZZV(:,:,  IKU  )=0.75*PZ(II_M  :II_M+1,IDV+IJ_M-1:IDV+IJ_M  ,  IKU-1)-0.25*PZ(II_M  :II_M+1,IDV+IJ_M-1:IDV+IJ_M  ,  IKU-2) &
                      +0.75*PZ(II_M  :II_M+1,IDV+IJ_M  :IDV+IJ_M+1,  IKU-1)-0.25*PZ(II_M  :II_M+1,IDV+IJ_M  :IDV+IJ_M+1,  IKU-2)

        ZWM(:,:,1:IKU-1)=0.5*PW(II_M:II_M+1,IJ_M:IJ_M+1,1:IKU-1)+0.5*PW(II_M:II_M+1,IJ_M:IJ_M+1,2:IKU  )
        ZWM(:,:,  IKU  )=1.5*PW(II_M:II_M+1,IJ_M:IJ_M+1,  IKU-1)-0.5*PW(II_M:II_M+1,IJ_M:IJ_M+1,  IKU-2)
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
        ZEXN(:,:,:    ) = (PP(II_M:II_M+1,IJ_M:IJ_M+1,:)/XP00)**(XRD/XCPD)
        DO JK=IKB-1,1,-1
          ZEXN(:,:,JK) = 1.5 * ZEXN(:,:,JK+1) - 0.5 * ZEXN(:,:,JK+2)
        END DO
        DO JK=IKE+1,IKU
          ZEXN(:,:,JK) = 1.5 * ZEXN(:,:,JK-1) - 0.5 * ZEXN(:,:,JK-2)
        END DO
        !
        IF ( TPFLYER%CTYPE == 'ISODEN' .OR. TPFLYER%CTYPE == 'CVBALL' .OR. TPFLYER%CTYPE == 'AIRCRA' ) THEN
          ZTHV(:,:,:) = PTH(II_M:II_M+1,IJ_M:IJ_M+1,:)
          IF (SIZE(PR,4)>0)                                                     &
          ZTHV(:,:,:) = ZTHV(:,:,:) * ( 1. + XRV/XRD*PR(II_M:II_M+1,IJ_M:IJ_M+1,:,1) )  &
                                    / ( 1. + WATER_SUM(PR(II_M:II_M+1,IJ_M:IJ_M+1,:,:)) )
          !
          ZTV (:,:,:) = ZTHV(:,:,:) * ZEXN(:,:,:)
          ZRHO(:,:,:) = PP(II_M:II_M+1,IJ_M:IJ_M+1,:) / (XRD*ZTV(:,:,:))
          DO JK=IKB-1,1,-1
            ZRHO(:,:,JK) = 1.5 * ZRHO(:,:,JK+1) - 0.5 * ZRHO(:,:,JK+2)
          END DO
          DO JK=IKE+1,IKU
            ZRHO(:,:,JK) = 1.5 * ZRHO(:,:,JK-1) - 0.5 * ZRHO(:,:,JK-2)
          END DO
          ZTHW_FLUX(:,:,:) = ZRHO(:,:,:)*XCPD *XTHW_FLUX(II_M:II_M+1,IJ_M:IJ_M+1,:)
          ZRCW_FLUX(:,:,:) = ZRHO(:,:,:)*XLVTT*XRCW_FLUX(II_M:II_M+1,IJ_M:IJ_M+1,:)
          ZSVW_FLUX(:,:,:,:) = XSVW_FLUX(II_M:II_M+1,IJ_M:IJ_M+1,:,:)
        END IF
          SELECT CASE ( TPFLYER%CTYPE )
            !
            !*      5.2.1 Iso-density balloon
            !
            CASE ( 'ISODEN' )
              ZXCOEF = (TPFLYER%XX_CUR - XXHATM(II_M)) / (XXHATM(II_M+1) - XXHATM(II_M))
              ZXCOEF = MAX (0.,MIN(ZXCOEF,1.))
              ZYCOEF = (TPFLYER%XY_CUR - XYHATM(IJ_M)) / (XYHATM(IJ_M+1) - XYHATM(IJ_M))
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
              ZXCOEF = (TPFLYER%XX_CUR - XXHATM(II_M)) / (XXHATM(II_M+1) - XXHATM(II_M))
              ZXCOEF = MAX (0.,MIN(ZXCOEF,1.))
              ZYCOEF = (TPFLYER%XY_CUR - XYHATM(IJ_M)) / (XYHATM(IJ_M+1) - XYHATM(IJ_M))
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
        END IF LAUNCHVERTPOS
        !
        !
        !
        !*      5.3  Vertical position
        !            -----------------
!
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

        IF ( ANY( [ IK00, IK01, IK10, IK11 ] < IKB ) ) THEN
          !Minimum altitude is on the ground at IKB (no crash if too low)
          IK00 = MAX ( IK00, IKB )
          IK01 = MAX ( IK01, IKB )
          IK10 = MAX ( IK10, IKB )
          IK11 = MAX ( IK11, IKB )

          CMNHMSG(1) = 'flyer ' // TRIM( TPFLYER%CTITLE ) // ' is near the ground'
          WRITE( CMNHMSG(2), "( 'at ', I2, '/', I2, '/', I4, ' ', F18.12, 's' )" ) &
                TDTCUR%NDAY, TDTCUR%NMONTH, TDTCUR%NYEAR, TDTCUR%XTIME
          CALL PRINT_MSG( NVERB_INFO, 'GEN', 'AIRCRAFT_BALLOON_EVOL' , OLOCAL = .TRUE.)
        END IF

        !
        !
        !*      5.4  Crash of the balloon
        !            --------------------
        !
        !
        IF (IK00 <  IKB .OR. IK01 <  IKB .OR. IK10 <  IKB .OR. IK11 <  IKB ) THEN
          TPFLYER%LCRASH = .TRUE.
          TPFLYER%NCRASH = NCRASH_OUT_LOW
        END IF
        IF (IK00 >= IKE .OR. IK01 >= IKE .OR. IK10 >= IKE .OR. IK11 >= IKE  ) THEN
          TPFLYER%LCRASH = .TRUE.
          TPFLYER%NCRASH = NCRASH_OUT_HIGH
        END IF
        CRASH_VERT: IF ( TPFLYER%LCRASH ) THEN
          TPFLYER%LFLY = .FALSE.
        WRITE( CMNHMSG(1), "( 'Balloon ', A, ' crashed the ', I2, '/', I2, '/', I4, ' at ', F18.12, 's (too low or too high)' )" ) &
               TRIM( TPFLYER%CTITLE ), TDTCUR%NDAY, TDTCUR%NMONTH, TDTCUR%NYEAR, TDTCUR%XTIME
        CALL PRINT_MSG( NVERB_WARNING, 'GEN', 'AIRCRAFT_BALLOON_EVOL', OLOCAL = .TRUE. )
        ELSE CRASH_VERT
          !No vertical crash
          !
          !*      6.   INITIALIZATIONS FOR INTERPOLATIONS
          !            ----------------------------------
          !
          !*      6.1  Interpolation coefficient for X
          !            -------------------------------
          !
          ZXCOEF = (TPFLYER%XX_CUR - XXHATM(II_M)) / (XXHATM(II_M+1) - XXHATM(II_M))
          ZXCOEF = MAX (0.,MIN(ZXCOEF,1.))
          !
          !
          !*      6.2  Interpolation coefficient for y
          !            -------------------------------
          !
          ZYCOEF = (TPFLYER%XY_CUR - XYHATM(IJ_M)) / (XYHATM(IJ_M+1) - XYHATM(IJ_M))
          ZYCOEF = MAX (0.,MIN(ZYCOEF,1.))
          !
          !
          !*      6.3  Interpolation coefficients for the 4 suroundings verticals
          !            ----------------------------------------------------------
          !
          !       SELECT TYPE ( TPFLYER )
          !         CLASS IS ( TBALLOONDATA)
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
          !----------------------------------------------------------------------------
          !
          !*      7.   INITIALIZATIONS FOR INTERPOLATIONS OF U AND V
          !            ---------------------------------------------
          !
          !*      7.1  Interpolation coefficient for X (for U)
          !            -------------------------------
          !
          ZUCOEF = (TPFLYER%XX_CUR - XXHAT(II_U)) / (XXHAT(II_U+1) - XXHAT(II_U))
          ZUCOEF = MAX(0.,MIN(ZUCOEF,1.))
          !
          !
          !*      7.2  Interpolation coefficient for y (for V)
          !            -------------------------------
          !
          ZVCOEF = (TPFLYER%XY_CUR - XYHAT(IJ_V)) / (XYHAT(IJ_V+1) - XYHAT(IJ_V))
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

          ! Check if it is the right moment to store data
          IF ( TPFLYER%LSTORE ) THEN
            ISTORE = TPFLYER%TFLYER_TIME%N_CUR
            IF ( ABS( TDTCUR - TPFLYER%TFLYER_TIME%TPDATES(ISTORE) ) < 1e-10 ) THEN
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
              !
              !
              !*      2.2  Interpolations of model variables to mass points
              !            ------------------------------------------------
              !
              IIU=SIZE(XXHAT)
              IJU=SIZE(XYHAT)

              TPFLYER%NMODELHIST(ISTORE) = TPFLYER%NMODEL
              TPFLYER%XX   (ISTORE) = TPFLYER%XX_CUR
              TPFLYER%XY   (ISTORE) = TPFLYER%XY_CUR
              TPFLYER%XZ   (ISTORE) = TPFLYER%XZ_CUR
              !
              CALL SM_LATLON(PLATOR,PLONOR,          &
                          TPFLYER%XX_CUR, TPFLYER%XY_CUR,       &
                          TPFLYER%XLAT(ISTORE), TPFLYER%XLON(ISTORE)  )
              !
              ZU_BAL = FLYER_INTERP_U(PU)
              ZV_BAL = FLYER_INTERP_V(PV)
              ZGAM   = (XRPK * (TPFLYER%XLON(ISTORE) - XLON0) - XBETA)*(XPI/180.)
              TPFLYER%XZON (ISTORE) = ZU_BAL * COS(ZGAM) + ZV_BAL * SIN(ZGAM)
              TPFLYER%XMER (ISTORE) = - ZU_BAL * SIN(ZGAM) + ZV_BAL * COS(ZGAM)
              !
              TPFLYER%XW   (ISTORE) = FLYER_INTERP(ZWM)
              TPFLYER%XTH  (ISTORE) = FLYER_INTERP(PTH)
              !
              ZFLYER_EXN = FLYER_INTERP(ZEXN)
              TPFLYER%XP   (ISTORE) = XP00 * ZFLYER_EXN**(XCPD/XRD)
              !
              ZR(:,:,:) = 0.
              DO JLOOP=1,SIZE(PR,4)
                TPFLYER%XR   (ISTORE,JLOOP) = FLYER_INTERP(PR(:,:,:,JLOOP))
                IF (JLOOP>=2) ZR(:,:,:) = ZR(:,:,:) + PR(:,:,:,JLOOP)
              END DO
              DO JLOOP=1,SIZE(PSV,4)
                TPFLYER%XSV  (ISTORE,JLOOP) = FLYER_INTERP(PSV(:,:,:,JLOOP))
              END DO
              TPFLYER%XRTZ  (ISTORE,:) = FLYER_INTERPZ(ZR(:,:,:))
              DO JLOOP=1,SIZE(PR,4)
                TPFLYER%XRZ  (ISTORE,:,JLOOP) = FLYER_INTERPZ(PR(:,:,:,JLOOP))
              END DO
              ! Fin Modifs ON
              TPFLYER%XFFZ  (ISTORE,:) = FLYER_INTERPZ(SQRT(PU**2+PV**2))
              IF (CCLOUD=="LIMA") THEN
                TPFLYER%XCIZ  (ISTORE,:) = FLYER_INTERPZ(PSV(:,:,:,NSV_LIMA_NI))
                TPFLYER%XCCZ  (ISTORE,:) = FLYER_INTERPZ(PSV(:,:,:,NSV_LIMA_NC))
                TPFLYER%XCRZ  (ISTORE,:) = FLYER_INTERPZ(PSV(:,:,:,NSV_LIMA_NR))
              ELSE IF ( CCLOUD=="ICE3" .OR. CCLOUD=="ICE4" ) THEN
                TPFLYER%XCIZ  (ISTORE,:) = FLYER_INTERPZ(PCIT(:,:,:))
              ENDIF
              ! initialization CRARE and CRARE_ATT + LWC and IWC
              TPFLYER%XCRARE(ISTORE,:) = 0.
              TPFLYER%XCRARE_ATT(ISTORE,:) = 0.
              TPFLYER%XLWCZ  (ISTORE,:) = 0.
              TPFLYER%XIWCZ  (ISTORE,:) = 0.
              IF (CCLOUD=="LIMA" .OR. CCLOUD=="ICE3" ) THEN ! only for ICE3 and LIMA
                TPFLYER%XLWCZ  (ISTORE,:) = FLYER_INTERPZ((PR(:,:,:,2)+PR(:,:,:,3))*PRHODREF(:,:,:))
                TPFLYER%XIWCZ  (ISTORE,:) = FLYER_INTERPZ((PR(:,:,:,4)+PR(:,:,:,5)+PR(:,:,:,6))*PRHODREF(:,:,:))
                ZTEMPZ(:)=FLYER_INTERPZ(PTH(II_M:II_M+1,IJ_M:IJ_M+1,:) * ZEXN(:,:,:))
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
                      TPFLYER%XCRARE(ISTORE,JK)=TPFLYER%XCRARE(ISTORE,JK)+ZREFLOC
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
                  TPFLYER%XCRARE_ATT(ISTORE,JK)=TPFLYER%XCRARE(ISTORE,JK)*ZAETOT
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
                  TPFLYER%XCRARE_ATT(ISTORE,JK)=TPFLYER%XCRARE(ISTORE,JK)*ZAETOT
                END DO
                TPFLYER%XZZ  (ISTORE,:) = ZZMZ(:)
                DEALLOCATE(ZZMZ,ZAELOC)
                ! m^3 → mm^6/m^3 → dBZ
                WHERE(TPFLYER%XCRARE(ISTORE,:)>0)
                  TPFLYER%XCRARE(ISTORE,:)=10.*LOG10(1.E18*TPFLYER%XCRARE(ISTORE,:))
                ELSEWHERE
                  TPFLYER%XCRARE(ISTORE,:)=XUNDEF
                END WHERE
                WHERE(TPFLYER%XCRARE_ATT(ISTORE,:)>0)
                  TPFLYER%XCRARE_ATT(ISTORE,:)=10.*LOG10(1.E18*TPFLYER%XCRARE_ATT(ISTORE,:))
                ELSEWHERE
                  TPFLYER%XCRARE_ATT(ISTORE,:)=XUNDEF
                END WHERE
                DEALLOCATE(ZX,ZW,ZRTMIN)
              END IF ! end LOOP ICE3
              ! vertical wind
              TPFLYER%XWZ  (ISTORE,:) = FLYER_INTERPZ(ZWM(:,:,:))
              IF (SIZE(PTKE)>0) TPFLYER%XTKE  (ISTORE)    = FLYER_INTERP(PTKE)
              IF (SIZE(PTS) >0) TPFLYER%XTSRAD(ISTORE)    = FLYER_INTERP_2D(PTS)
              IF (LDIAG_IN_RUN) TPFLYER%XTKE_DISS(ISTORE) = FLYER_INTERP(XCURRENT_TKE_DISS)
              TPFLYER%XZS(ISTORE)  = FLYER_INTERP_2D(PZ(:,:,1+JPVEXT))
              TPFLYER%XTHW_FLUX(ISTORE) = FLYER_INTERP(ZTHW_FLUX)
              TPFLYER%XRCW_FLUX(ISTORE) = FLYER_INTERP(ZRCW_FLUX)
              DO JLOOP=1,SIZE(PSV,4)
              TPFLYER%XSVW_FLUX(ISTORE,JLOOP) = FLYER_INTERP(ZSVW_FLUX(:,:,:,JLOOP))
              END DO
            END IF
          END IF
          !
          !----------------------------------------------------------------------------
          !
          !*      9.   BALLOON ADVECTION
          !            -----------------
          !
          !       SELECT TYPE ( TPFLYER )
          !         CLASS IS ( TBALLOONDATA)
          ZTSTEP = PTSTEP

          IF ( TPFLYER%CTYPE == 'RADIOS' .OR. TPFLYER%CTYPE == 'ISODEN' .OR. TPFLYER%CTYPE == 'CVBALL' ) THEN
            ZU_BAL = FLYER_INTERP_U(PU)
            ZV_BAL = FLYER_INTERP_V(PV)
            if ( .not. lcartesian ) then
              ZMAP = FLYER_INTERP_2D(PMAP)
            else
              ZMAP = 1.
            end if
            !
            ZX_OLD = TPFLYER%XX_CUR
            ZY_OLD = TPFLYER%XY_CUR

            TPFLYER%XX_CUR = TPFLYER%XX_CUR + ZU_BAL * ZTSTEP * ZMAP
            TPFLYER%XY_CUR = TPFLYER%XY_CUR + ZV_BAL * ZTSTEP * ZMAP
          END IF
          !
          !Compute rank and model for next position
          !This is done here because we need to check if there is a change of model (for 'MOB' balloons)
          !because position has to be adapted to the timestep of a coarser model (if necessary)
          IMODEL_OLD = TPFLYER%NMODEL
          ! Get rank of the process where the balloon is and the model number
          IF ( TPFLYER%CMODEL == 'FIX' ) THEN
            IMODEL = TPFLYER%NMODEL
          ELSE
            IMODEL = 0
          END IF
          CALL FIND_PROCESS_AND_MODEL_FROM_XY_POS( TPFLYER%XX_CUR, TPFLYER%XY_CUR, IRANK, IMODEL )
          IF ( IRANK < 1 ) THEN
            TPFLYER%NMODEL = 1 !Set to 1 because it is always a valid model (to prevent crash if NDAD(TPFLYER%NMODEL) )
            TPFLYER%LCRASH = .TRUE.
            TPFLYER%NCRASH = NCRASH_OUT_HORIZ
            TPFLYER%LFLY   = .FALSE.
            WRITE( CMNHMSG(1), "( 'Balloon ', A, ' crashed the ', I2, '/', I2, '/', I4, ' at ', F18.12, &
                                's (out of the horizontal boundaries)' )" ) &
              TRIM( TPFLYER%CTITLE ), TDTCUR%NDAY, TDTCUR%NMONTH, TDTCUR%NYEAR, TDTCUR%XTIME
            CALL PRINT_MSG( NVERB_WARNING, 'GEN', 'AIRCRAFT_BALLOON_EVOL', OLOCAL = .TRUE. )
          ELSE
            TPFLYER%NMODEL = IMODEL
            TPFLYER%LCRASH = .FALSE.
            TPFLYER%NCRASH = NCRASH_NO
            TPFLYER%NRANK_CUR = IRANK
          END IF
          IF ( TPFLYER%NMODEL /= IMODEL_OLD .AND. .NOT. TPFLYER%LCRASH ) THEN
            IF ( NDAD(TPFLYER%NMODEL ) == IMODEL_OLD ) THEN
              !Nothing special to do
            ELSE IF ( TPFLYER%NMODEL == NDAD(IMODEL_OLD) ) THEN
              !Recompute position to be compatible with parent timestep

              !Determine step compatible with parent model at next parent timestep
              ZDELTATIME = TDTCUR - TDTSEG
              ZDIVTMP = ZDELTATIME / ( PTSTEP * NDTRATIO(IMODEL_OLD) )
              IF ( ABS( ZDIVTMP - NINT( ZDIVTMP ) ) < 1E-6 * PTSTEP * NDTRATIO(IMODEL_OLD) ) THEN
                ZTSTEP = ZTSTEP * NDTRATIO(IMODEL_OLD)
              ELSE
                ZTSTEP = ZTSTEP * ( 1 + NDTRATIO(IMODEL_OLD) )
                ! Detect if we need to skip a store (if time of next position is after time of next store)
                ! This can happen when a ballon goes to its parent model
                IF ( TDTCUR + ZTSTEP > &
                     TPFLYER%TFLYER_TIME%TPDATES(TPFLYER%TFLYER_TIME%N_CUR) + TPFLYER%TFLYER_TIME%XTSTEP + 1e-6 ) THEN
                  TPFLYER%TFLYER_TIME%N_CUR = TPFLYER%TFLYER_TIME%N_CUR + 1
                  ISTORE = TPFLYER%TFLYER_TIME%N_CUR
                  !Remark: by construction here, ISTORE is always > 1 => no risk with ISTORE-1 value
                  TPFLYER%TFLYER_TIME%TPDATES(ISTORE) = TPFLYER%TFLYER_TIME%TPDATES(ISTORE-1) + TPFLYER%TFLYER_TIME%XTSTEP

                  !Force a dummy store (nothing is computed, therefore default/initial values will be stored)
                  TPFLYER%LSTORE = .TRUE.

                  WRITE( CMNHMSG(1), "( 'Balloon ', A, ': store skipped at ', I2, '/', I2, '/', I4, ' at ', F18.12, 's' )" ) &
                         TRIM( TPFLYER%CTITLE ),                                                                             &
                         TPFLYER%TFLYER_TIME%TPDATES(ISTORE)%NDAY,  TPFLYER%TFLYER_TIME%TPDATES(ISTORE)%NMONTH,              &
                         TPFLYER%TFLYER_TIME%TPDATES(ISTORE)%NYEAR, TPFLYER%TFLYER_TIME%TPDATES(ISTORE)%XTIME
                  CMNHMSG(2) = 'due to change of model (child to its parent)'
                  CALL PRINT_MSG( NVERB_WARNING, 'GEN', 'AIRCRAFT_BALLOON_EVOL', OLOCAL = .TRUE. )
                END IF
              END IF

              TPFLYER%XX_CUR = TPFLYER%XX_CUR + ZU_BAL * ZTSTEP * ZMAP
              TPFLYER%XY_CUR = TPFLYER%XY_CUR + ZV_BAL * ZTSTEP * ZMAP

              ! Model number is now imposed
              IMODEL = TPFLYER%NMODEL
              CALL FIND_PROCESS_AND_MODEL_FROM_XY_POS( TPFLYER%XX_CUR, TPFLYER%XY_CUR, IRANK, IMODEL )
              IF ( IRANK < 1 ) THEN
                TPFLYER%NMODEL = 1 !Set to 1 because it is always a valid model (to prevent crash if NDAD(TPFLYER%NMODEL) )
                TPFLYER%LCRASH = .TRUE.
                TPFLYER%NCRASH = NCRASH_OUT_HORIZ
                TPFLYER%LFLY   = .FALSE.
                WRITE( CMNHMSG(1), "( 'Balloon ', A, ' crashed the ', I2, '/', I2, '/', I4, ' at ', F18.12, &
                       's (out of the horizontal boundaries)' )" )                                          &
                       TRIM( TPFLYER%CTITLE ), TDTCUR%NDAY, TDTCUR%NMONTH, TDTCUR%NYEAR, TDTCUR%XTIME
                CALL PRINT_MSG( NVERB_WARNING, 'GEN', 'AIRCRAFT_BALLOON_EVOL', OLOCAL = .TRUE. )
              ELSE
                !TPFLYER%NMODEL = IMODEL !Do not change model because we are in transition to parent
                TPFLYER%LCRASH = .FALSE.
                TPFLYER%NCRASH = NCRASH_NO
                TPFLYER%NRANK_CUR = IRANK
              END IF
            ELSE
              !Special case not-managed (different dads, change of several models in 1 step (going to grand parent)...)
              CMNHMSG(1) = 'unmanaged change of model for ballon ' // TPFLYER%CTITLE
              CMNHMSG(2) = 'its trajectory might be wrong'
              CALL PRINT_MSG( NVERB_WARNING, 'GEN', 'AIRCRAFT_BALLOON_EVOL', OLOCAL = .TRUE. )
            END IF
          END IF

          IF ( TPFLYER%CTYPE == 'RADIOS' ) THEN
            ZW_BAL = FLYER_INTERP(ZWM)
            TPFLYER%XZ_CUR = TPFLYER%XZ_CUR + ( ZW_BAL + TPFLYER%XWASCENT ) * ZTSTEP
          END IF
          !
          IF ( TPFLYER%CTYPE == 'CVBALL' ) THEN
            ZW_BAL = FLYER_INTERP(ZWM)
            ZRO_BAL = FLYER_INTERP(ZRHO)
            ! calculation with a time step of 1 second or less
            IF (INT(ZTSTEP) .GT. 1 ) THEN
              DO JK=1,INT(ZTSTEP)
                TPFLYER%XWASCENT = TPFLYER%XWASCENT &
                  -  ( 1. / (1. + TPFLYER%XINDDRAG ) ) * 1. * &
                     ( XG * ( ( TPFLYER%XMASS / TPFLYER%XVOLUME ) - ZRO_BAL ) / ( TPFLYER%XMASS / TPFLYER%XVOLUME ) &
                        + TPFLYER%XWASCENT * ABS ( TPFLYER%XWASCENT ) * &
                          TPFLYER%XDIAMETER * TPFLYER%XAERODRAG / ( 2. * TPFLYER%XVOLUME ) &
                      )
                TPFLYER%XZ_CUR = TPFLYER%XZ_CUR + ( ZW_BAL + TPFLYER%XWASCENT ) * 1.
              END DO
            END IF
            IF (ZTSTEP .GT. INT(ZTSTEP)) THEN
              TPFLYER%XWASCENT = TPFLYER%XWASCENT &
                -  ( 1. / (1. + TPFLYER%XINDDRAG ) ) * (ZTSTEP-INT(ZTSTEP)) * &
                   ( XG * ( ( TPFLYER%XMASS / TPFLYER%XVOLUME ) - ZRO_BAL ) / ( TPFLYER%XMASS / TPFLYER%XVOLUME ) &
                      + TPFLYER%XWASCENT * ABS ( TPFLYER%XWASCENT ) * &
                        TPFLYER%XDIAMETER * TPFLYER%XAERODRAG / ( 2. * TPFLYER%XVOLUME ) &
                    )
              TPFLYER%XZ_CUR = TPFLYER%XZ_CUR + ( ZW_BAL + TPFLYER%XWASCENT ) * (ZTSTEP-INT(ZTSTEP))
            END IF
          END IF

          TPFLYER%TPOS_CUR = TDTCUR + ZTSTEP
        END IF CRASH_VERT !end of no vertical crash branch
      ELSE ISOWNERBAL
        !The balloon is not present on this MPI process
        ZTHIS_PROC = 0.
      END IF ISOWNERBAL
      !----------------------------------------------------------------------------
      !
      !*     11.   EXCHANGE OF INFORMATION BETWEEN PROCESSES
      !            -----------------------------------------
      !
      !*     11.1  current position
      !            ----------------
      !
      IF ( TPFLYER%CMODEL == 'MOB' ) THEN
        CALL DISTRIBUTE_FLYER_N(TPFLYER%NMODEL)
      END IF
      CALL DISTRIBUTE_FLYER_N(TPFLYER%NRANK_CUR)
      CALL DISTRIBUTE_FLYER_L(TPFLYER%LFLY)
      CALL DISTRIBUTE_FLYER_L(TPFLYER%LCRASH)
      CALL DISTRIBUTE_FLYER_L(TPFLYER%LSTORE)
      CALL DISTRIBUTE_FLYER(TPFLYER%XX_CUR)
      CALL DISTRIBUTE_FLYER(TPFLYER%XY_CUR)
      CALL DISTRIBUTE_FLYER_N(TPFLYER%TPOS_CUR%NYEAR)
      CALL DISTRIBUTE_FLYER_N(TPFLYER%TPOS_CUR%NMONTH)
      CALL DISTRIBUTE_FLYER_N(TPFLYER%TPOS_CUR%NDAY)
      CALL DISTRIBUTE_FLYER  (TPFLYER%TPOS_CUR%XTIME)

      CALL DISTRIBUTE_FLYER_N(TPFLYER%TFLYER_TIME%N_CUR)

      ! SELECT TYPE ( TPFLYER )
      !   CLASS IS ( TBALLOONDATA )
      IF ( TPFLYER%CTYPE == 'CVBALL' ) THEN
        CALL DISTRIBUTE_FLYER(TPFLYER%XZ_CUR)
        CALL DISTRIBUTE_FLYER(TPFLYER%XWASCENT)
      ELSE IF ( TPFLYER%CTYPE == 'RADIOS' ) THEN
        CALL DISTRIBUTE_FLYER(TPFLYER%XZ_CUR)
      ELSE IF ( TPFLYER%CTYPE == 'ISODEN' ) THEN
        CALL DISTRIBUTE_FLYER(TPFLYER%XRHO)
      END IF

      IF ( TPFLYER%LSTORE ) THEN
        CALL DISTRIBUTE_FLYER_N(TPFLYER%TFLYER_TIME%TPDATES(ISTORE)%NYEAR)
        CALL DISTRIBUTE_FLYER_N(TPFLYER%TFLYER_TIME%TPDATES(ISTORE)%NMONTH)
        CALL DISTRIBUTE_FLYER_N(TPFLYER%TFLYER_TIME%TPDATES(ISTORE)%NDAY)
        CALL DISTRIBUTE_FLYER  (TPFLYER%TFLYER_TIME%TPDATES(ISTORE)%XTIME)

        CALL DISTRIBUTE_FLYER_N(TPFLYER%NMODELHIST(ISTORE))
        CALL DISTRIBUTE_FLYER(TPFLYER%XX  (ISTORE))
        CALL DISTRIBUTE_FLYER(TPFLYER%XY  (ISTORE))
        CALL DISTRIBUTE_FLYER(TPFLYER%XZ  (ISTORE))
        CALL DISTRIBUTE_FLYER(TPFLYER%XLON(ISTORE))
        CALL DISTRIBUTE_FLYER(TPFLYER%XLAT(ISTORE))
        CALL DISTRIBUTE_FLYER(TPFLYER%XZON(ISTORE))
        CALL DISTRIBUTE_FLYER(TPFLYER%XMER(ISTORE))
        CALL DISTRIBUTE_FLYER(TPFLYER%XW  (ISTORE))
        CALL DISTRIBUTE_FLYER(TPFLYER%XP  (ISTORE))
        CALL DISTRIBUTE_FLYER(TPFLYER%XTH (ISTORE))
        DO JLOOP=1,SIZE(PR,4)
          CALL DISTRIBUTE_FLYER(TPFLYER%XR   (ISTORE,JLOOP))
        END DO
        DO JLOOP=1,SIZE(PSV,4)
          CALL DISTRIBUTE_FLYER(TPFLYER%XSV  (ISTORE,JLOOP))
        END DO
        DO JLOOP=1,IKU
          CALL DISTRIBUTE_FLYER(TPFLYER%XRTZ (ISTORE,JLOOP))
          DO JLOOP2=1,SIZE(PR,4)
            CALL DISTRIBUTE_FLYER(TPFLYER%XRZ (ISTORE,JLOOP,JLOOP2))
          ENDDO
          CALL DISTRIBUTE_FLYER(TPFLYER%XFFZ (ISTORE,JLOOP))
          CALL DISTRIBUTE_FLYER(TPFLYER%XCIZ (ISTORE,JLOOP))
          IF (CCLOUD== 'LIMA' ) THEN
            CALL DISTRIBUTE_FLYER(TPFLYER%XCRZ (ISTORE,JLOOP))
            CALL DISTRIBUTE_FLYER(TPFLYER%XCCZ (ISTORE,JLOOP))
          ENDIF
          CALL DISTRIBUTE_FLYER(TPFLYER%XIWCZ (ISTORE,JLOOP))
          CALL DISTRIBUTE_FLYER(TPFLYER%XLWCZ (ISTORE,JLOOP))
          CALL DISTRIBUTE_FLYER(TPFLYER%XCRARE (ISTORE,JLOOP))
          CALL DISTRIBUTE_FLYER(TPFLYER%XCRARE_ATT (ISTORE,JLOOP))
          CALL DISTRIBUTE_FLYER(TPFLYER%XWZ (ISTORE,JLOOP))
          CALL DISTRIBUTE_FLYER(TPFLYER%XZZ (ISTORE,JLOOP))
        END DO
        IF (SIZE(PTKE)>0) CALL DISTRIBUTE_FLYER(TPFLYER%XTKE  (ISTORE))
        IF (SIZE(PTS) >0) CALL DISTRIBUTE_FLYER(TPFLYER%XTSRAD(ISTORE))
        IF (LDIAG_IN_RUN) CALL DISTRIBUTE_FLYER(TPFLYER%XTKE_DISS(ISTORE))
        CALL DISTRIBUTE_FLYER(TPFLYER%XZS  (ISTORE))
        CALL DISTRIBUTE_FLYER(TPFLYER%XTHW_FLUX(ISTORE))
        CALL DISTRIBUTE_FLYER(TPFLYER%XRCW_FLUX(ISTORE))
        DO JLOOP=1,SIZE(PSV,4)
          CALL DISTRIBUTE_FLYER(TPFLYER%XSVW_FLUX(ISTORE,JLOOP))
        END DO
      END IF
    END IF INFLIGHTONMODEL

END SELECT


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
  JI=II_M
  JJ=IJ_M
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
  JI=II_M
  JJ=IJ_M
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
  JI=II_U
  JJ=IJ_M
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
  JI=II_M
  JJ=IJ_V
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
  JI=II_M
  JJ=IJ_M
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


END SUBROUTINE AIRCRAFT_BALLOON_EVOL
!----------------------------------------------------------------------------

END MODULE MODE_AIRCRAFT_BALLOON_EVOL
