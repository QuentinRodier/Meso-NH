!     #########################################
      SUBROUTINE CANOPY_EVOL_NEUTRAL(KI,KLVL,PTSTEP,KIMPL,PWIND,PRHOA, &
                             PSFLUX_U,                                 &
                             PFORC_U,PDFORC_UDU,PFORC_E,PDFORC_EDE,    &
                             PZ,PZF,PDZ,PDZF,PU,PTKE,PUSTAR,           &
                             PALFAU,PBETAU                             )
!     #########################################
!
!!****  *CANOPY_EVOL* - evolution of canopy
!!                        
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!	V. Masson   *Meteo France*	
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    05/2010 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_CSTS,        ONLY : XKARMAN
USE MODD_SURF_PAR,    ONLY : XUNDEF
USE MODD_CANOPY_TURB, ONLY : XCMFS, XALPSBL, XCED
!
USE MODI_CANOPY_EVOL_WIND
USE MODI_CANOPY_EVOL_TKE
!
USE MODE_SBLS
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
INTEGER,                  INTENT(IN)    :: KI        ! number of horizontal points
INTEGER,                  INTENT(IN)    :: KLVL      ! number of levels in canopy
REAL,                     INTENT(IN)    :: PTSTEP    ! atmospheric time-step                 (s)
INTEGER,                  INTENT(IN)    :: KIMPL     ! implicitation code: 
!                                                    ! 1 : computes only alfa and beta coupling
!                                                    !     coefficients for all variables
!                                                    ! 2 : computes temporal evolution of the
!                                                    !     variables
REAL, DIMENSION(KI),      INTENT(IN)    :: PWIND     ! wind speed                            (m/s)
REAL, DIMENSION(KI),      INTENT(IN)    :: PRHOA     ! Air density at forcing level          (kg/m3)
REAL, DIMENSION(KI),      INTENT(IN)    :: PSFLUX_U  ! surface flux u'w'                     (m2/s2)
REAL, DIMENSION(KI,KLVL), INTENT(IN)    :: PFORC_U   ! tendency of wind due to canopy drag   (m/s2)
REAL, DIMENSION(KI,KLVL), INTENT(IN)    :: PDFORC_UDU! formal derivative of the tendency of
!                                                    ! wind due to canopy drag               (1/s)
REAL, DIMENSION(KI,KLVL), INTENT(IN)    :: PFORC_E   ! tendency of TKE  due to canopy drag   (m2/s3)
REAL, DIMENSION(KI,KLVL), INTENT(IN)    :: PDFORC_EDE! formal derivative of the tendency of
!                                                    ! TKE  due to canopy drag               (1/s)
!
REAL, DIMENSION(KI,KLVL), INTENT(INOUT) :: PZ        ! heights of canopy levels              (m)
REAL, DIMENSION(KI,KLVL), INTENT(IN)    :: PZF       ! heights of bottom of canopy levels    (m)
REAL, DIMENSION(KI,KLVL), INTENT(IN)    :: PDZ       ! depth   of canopy levels              (m)
REAL, DIMENSION(KI,KLVL), INTENT(IN)    :: PDZF      ! depth between canopy levels           (m)
REAL, DIMENSION(KI,KLVL), INTENT(INOUT) :: PU        ! wind speed at canopy levels           (m/s)
REAL, DIMENSION(KI,KLVL), INTENT(INOUT) :: PTKE      ! TKE at canopy levels                  (m2/s2)
REAL, DIMENSION(KI),      INTENT(OUT)   :: PUSTAR    ! friction velocity at forcing level    (m/s)
REAL, DIMENSION(KI),      INTENT(OUT)   :: PALFAU   ! V+(1) = alfa u'w'(1) + beta
REAL, DIMENSION(KI),      INTENT(OUT)   :: PBETAU   ! V+(1) = alfa u'w'(1) + beta
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER :: JLAYER                 ! loop counter on layers
INTEGER :: JI                     ! loop counter
!
REAL, DIMENSION(KI,KLVL) :: ZK       ! mixing coefficient
REAL, DIMENSION(KI,KLVL) :: ZDKDDVDZ ! formal derivative of mixing coefficient according to variable vertical gradient
REAL, DIMENSION(KI,KLVL) :: ZEXN     ! Exner function        at full levels
REAL, DIMENSION(KI,KLVL) :: ZUW      ! friction at mid levels
REAL, DIMENSION(KI,KLVL) :: ZTH      ! temperature(meaningless here)
REAL, DIMENSION(KI,KLVL) :: ZWTH     ! heat  flux (supposed equal to zero)
REAL, DIMENSION(KI,KLVL) :: ZWQ      ! vapor flux (supposed equal to zero)
REAL, DIMENSION(KI,KLVL) :: ZLEPS    ! Dissipative length at full levels
REAL, DIMENSION(KI,KLVL) :: ZZ       ! height above displacement height
REAL, DIMENSION(KI,KLVL) :: ZLM      ! Mixing      length at mid levels
REAL, DIMENSION(KI)      :: ZUSTAR   ! friction velocity (estimated from
!                                    ! fluxes at atmospheric forcing level)
!
REAL                     :: ZZ0      ! a value of z0 just for first time-step init.
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!*    1. First time step initialization
!        ------------------------------
!
!* first time step (i.e. wind profile is initially zero) : 
!  set a neutral wind profile in relation with forcing wind
IF (LHOOK) CALL DR_HOOK('CANOPY_EVOL_NEUTRAL',0,ZHOOK_HANDLE)
DO JI=1,KI
  IF(PWIND(JI)>0. .AND. PU(JI,KLVL-1)==0.) THEN
    ZZ0 = PZ(JI,1)/2.
    PU(JI,:) = PWIND(JI) * LOG (PZ(JI,:)/ZZ0) / LOG (PZ(JI,KLVL)/ZZ0)
  END IF
END DO
!-------------------------------------------------------------------------------
!
!*    2. mixing and dissipative lengths (at full levels)
!        ------------------------------
!
!* influence of ground surface on mixing length in neutral case
DO JLAYER=1,KLVL
  ZLM  (:,JLAYER) = XKARMAN/SQRT(XALPSBL)/XCMFS * PZ(:,JLAYER)
  ZLEPS(:,JLAYER) = XKARMAN*(XALPSBL**1.5)*XCED * PZ(:,JLAYER)
END DO
!
!-------------------------------------------------------------------------------
!
!*    3. time evolution of wind in canopy
!        --------------------------------
!
!*    3.1 mixing coefficient for wind at mid layers (at half levels)
!         ---------------------------
!
ZK = -999.
DO JLAYER=2,KLVL
  ZK(:,JLAYER) = 0.5 * XCMFS * ZLM(:,JLAYER)   * SQRT(PTKE(:,JLAYER)  ) &
               + 0.5 * XCMFS * ZLM(:,JLAYER-1) * SQRT(PTKE(:,JLAYER-1)) 
END DO
!
!
!*    3.2 mixing coefficient vertical derivative at mid layers (at half levels)
!         --------------------------------------
!
!* there is no formal dependency on wind
ZDKDDVDZ = 0.
!
!
!*    3.3  time evolution of wind in canopy
!          --------------------------------
!
 CALL CANOPY_EVOL_WIND(KI,KLVL,PTSTEP,KIMPL,PWIND,ZK,ZDKDDVDZ,PSFLUX_U,PFORC_U,PDFORC_UDU,PDZ,PDZF,PU,ZUW,PALFAU,PBETAU)
!
!*    3.4  Friction velocity at top of SBL layers
!          --------------------------------------
!
PUSTAR = SQRT(ABS(ZUW(:,KLVL)))
!
!-------------------------------------------------------------------------------
!
!*    4. time evolution of TKE in canopy
!        -------------------------------
!
!* neutral case, no thermal production
ZWTH = 0.
ZWQ  = 0.
ZTH  = 300.
!
 CALL CANOPY_EVOL_TKE(KI,KLVL,PTSTEP,PRHOA,PZ,PZF,PDZ,PDZF,PFORC_E,PDFORC_EDE, &
                     PU,ZTH,ZUW,ZWTH,ZWQ,ZLEPS,PTKE                           )
!
!-------------------------------------------------------------------------------
!
!*    5. Security at atmospheric forcing level
!        -------------------------------------
!
PU(:,KLVL) = PWIND(:)
IF (LHOOK) CALL DR_HOOK('CANOPY_EVOL_NEUTRAL',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
END SUBROUTINE CANOPY_EVOL_NEUTRAL


