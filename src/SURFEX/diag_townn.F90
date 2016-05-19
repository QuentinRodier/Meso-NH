!     #########
SUBROUTINE DIAG_TOWN_n(HPROGRAM,                                           &
                         PRN, PH, PLE, PLEI, PGFLUX, PRI, PCD, PCH, PCE, PQS,&
                         PZ0, PZ0H, PT2M, PTS, PQ2M, PHU2M, PZON10M, PMER10M,&
                         PSWD, PSWU, PSWBD, PSWBU, PLWD, PLWU, PFMU, PFMV,   &
                         PRNC, PHC, PLEC, PGFLUXC, PSWDC, PSWUC, PLWDC,      &
                         PLWUC, PFMUC, PFMVC, PT2M_MIN, PT2M_MAX, PLEIC,     &
                         PHU2M_MIN, PHU2M_MAX, PWIND10M, PWIND10M_MAX        )  
!     ######################################################################
!
!!****  *DIAG_TOWN_n * - Chooses the surface schemes for town diagnostics
!!
!!    PURPOSE
!!    -------
!
!!**  METHOD
!!    ------
!!
!!    REFERENCE
!!    ---------
!!      
!!
!!    AUTHOR
!!    ------
!!     V. Masson 
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2004
!!      Modified    01/2006 : sea flux parameterization.
!!      Modified    08/2009 : new diag
!!      Modified    09/2012 : new PLEI diag required by atmospheric model
!!------------------------------------------------------------------
!

!
USE MODD_SURF_PAR,   ONLY : XUNDEF
USE MODD_SURF_ATM_n, ONLY : CTOWN
USE MODD_CSTS,       ONLY : XTT
!
USE MODI_DIAG_TEB_n
USE MODI_DIAG_IDEAL_n
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
 CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM ! program calling surf. schemes
!
REAL, DIMENSION(:), INTENT(OUT) :: PRN      ! Net radiation       (W/m2)
REAL, DIMENSION(:), INTENT(OUT) :: PH       ! Sensible heat flux  (W/m2)
REAL, DIMENSION(:), INTENT(OUT) :: PLE      ! Total latent heat flux    (W/m2)
REAL, DIMENSION(:), INTENT(OUT) :: PLEI     ! Sublimation latent heat flux (W/m2)
REAL, DIMENSION(:), INTENT(OUT) :: PGFLUX   ! Storage flux        (W/m2)
REAL, DIMENSION(:), INTENT(OUT) :: PRI      ! Richardson number   (-)
REAL, DIMENSION(:), INTENT(OUT) :: PCD      ! drag coefficient    (W/s2)
REAL, DIMENSION(:), INTENT(OUT) :: PCH      ! transf. coef heat   (W/s)
REAL, DIMENSION(:), INTENT(OUT) :: PCE      ! transf. coef vapor  (W/s/K)
REAL, DIMENSION(:), INTENT(OUT) :: PQS
REAL, DIMENSION(:), INTENT(OUT) :: PZ0      ! rough. length wind  (m)
REAL, DIMENSION(:), INTENT(OUT) :: PZ0H     ! rough. length heat  (m)
REAL, DIMENSION(:), INTENT(OUT) :: PTS      ! surface temperature (K)
REAL, DIMENSION(:), INTENT(OUT) :: PT2M     ! temperature at 2m   (K)
REAL, DIMENSION(:), INTENT(OUT) :: PQ2M     ! humidity at 2m      (kg/kg)
REAL, DIMENSION(:), INTENT(OUT) :: PHU2M    ! relative humidity at 2m (-)
REAL, DIMENSION(:), INTENT(OUT) :: PZON10M  ! zonal wind at 10m   (m/s)
REAL, DIMENSION(:), INTENT(OUT) :: PMER10M  ! meridian wind at 10m(m/s)
REAL, DIMENSION(:), INTENT(OUT) :: PSWD     ! incoming short wave radiation (W/m2)
REAL, DIMENSION(:), INTENT(OUT) :: PSWU     ! outgoing short wave radiation (W/m2)
REAL, DIMENSION(:,:), INTENT(OUT) :: PSWBD  ! incoming short wave radiation by spectral band(W/m2)
REAL, DIMENSION(:,:), INTENT(OUT) :: PSWBU  ! outgoing short wave radiation by spectral band(W/m2)
REAL, DIMENSION(:), INTENT(OUT) :: PLWD     ! incoming long wave radiation (W/m2)
REAL, DIMENSION(:), INTENT(OUT) :: PLWU     ! outgoing long wave radiation (W/m2)
REAL, DIMENSION(:), INTENT(OUT) :: PFMU     ! zonal friction
REAL, DIMENSION(:), INTENT(OUT) :: PFMV     ! meridian friction 
REAL, DIMENSION(:), INTENT(OUT) :: PRNC     ! Net radiation       (J/m2)
REAL, DIMENSION(:), INTENT(OUT) :: PHC      ! Sensible heat flux  (J/m2)
REAL, DIMENSION(:), INTENT(OUT) :: PLEC     ! Total latent heat flux       (J/m2)
REAL, DIMENSION(:), INTENT(OUT) :: PLEIC    ! Sublimation latent heat flux (J/m2)
REAL, DIMENSION(:), INTENT(OUT) :: PGFLUXC  ! Storage flux        (J/m2)
REAL, DIMENSION(:), INTENT(OUT) :: PSWDC    ! incoming short wave radiation (J/m2)
REAL, DIMENSION(:), INTENT(OUT) :: PSWUC    ! outgoing short wave radiation (J/m2)
REAL, DIMENSION(:), INTENT(OUT) :: PLWDC    ! incoming long wave radiation (J/m2)
REAL, DIMENSION(:), INTENT(OUT) :: PLWUC    ! outgoing long wave radiation (J/m2)
REAL, DIMENSION(:), INTENT(OUT) :: PFMUC    ! zonal friction
REAL, DIMENSION(:), INTENT(OUT) :: PFMVC    ! meridian friction 
REAL, DIMENSION(:), INTENT(OUT) :: PT2M_MIN ! Minimum temperature at 2m   (K)
REAL, DIMENSION(:), INTENT(OUT) :: PT2M_MAX ! Maximum temperature at 2m   (K)
REAL, DIMENSION(:), INTENT(OUT) :: PHU2M_MIN! Minimum relative humidity at 2m (-)
REAL, DIMENSION(:), INTENT(OUT) :: PHU2M_MAX! Maximum relative humidity at 2m (-)
REAL, DIMENSION(:), INTENT(OUT) :: PWIND10M ! wind at 10m (m/s)
REAL, DIMENSION(:), INTENT(OUT) :: PWIND10M_MAX! Maximum wind at 10m (m/s)
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(SIZE(PRN)) :: ZDELTA
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('DIAG_TOWN_N',0,ZHOOK_HANDLE)
IF (CTOWN=='TEB   ') THEN
  CALL DIAG_TEB_n(HPROGRAM,                                          &
                    PRN, PH, PLE, PGFLUX, PRI, PCD, PCH, PCE, PQS,     &
                    PZ0, PZ0H, PT2M, PQ2M, PHU2M, PZON10M, PMER10M,    &
                    PSWD, PSWU, PLWD, PLWU, PSWBD, PSWBU, PFMU, PFMV   )  
!
! new diag not yet inplemeted for TEB (these diag are required for the climate model)
!
! Ok with atmospheric model but LEI (latent heat of sublimation) must by implemented in TEB
  IF (SIZE(PLEI)>0) THEN
    PLEI(:) = XUNDEF
    WHERE(PLE(:)/=XUNDEF)
      ZDELTA(:) = MAX(0.0,SIGN(1.0,XTT-PT2M(:)))
      PLEI  (:) = PLE(:) * ZDELTA(:)
    ENDWHERE
  ENDIF
!
  PTS      = XUNDEF
  PRNC     = XUNDEF
  PHC      = XUNDEF
  PLEC     = XUNDEF
  PLEIC    = XUNDEF
  PGFLUXC  = XUNDEF
  PSWDC    = XUNDEF
  PSWUC    = XUNDEF
  PLWDC    = XUNDEF
  PLWUC    = XUNDEF
  PFMUC    = XUNDEF
  PFMVC    = XUNDEF
  PT2M_MIN = XUNDEF
  PT2M_MAX = 0.0
  PHU2M_MIN= XUNDEF
  PHU2M_MAX= -XUNDEF
  PWIND10M_MAX = 0.0      
  PT2M_MIN = MIN(PT2M(:),PT2M_MIN(:))
  PT2M_MAX = MAX(PT2M(:),PT2M_MAX(:))
  PHU2M_MIN= MIN(PHU2M(:),PHU2M_MIN(:))
  PHU2M_MAX= MAX(PHU2M(:),PHU2M_MAX(:))
  PWIND10M    (:) = SQRT(PZON10M(:)**2+PMER10M(:)**2)
  PWIND10M_MAX(:) = MAX(PWIND10M_MAX(:),PWIND10M(:))
!      
ELSE IF (CTOWN=='FLUX  ') THEN
  CALL DIAG_IDEAL_n(HPROGRAM, PQS, PZ0, PZ0H, PH, PLE, PRN, PGFLUX)
  PLEI     = XUNDEF
  PRI      = XUNDEF
  PCD      = XUNDEF
  PCH      = XUNDEF
  PCE      = XUNDEF
  PTS      = XUNDEF
  PT2M     = XUNDEF
  PQ2M     = XUNDEF
  PHU2M    = XUNDEF
  PZON10M  = XUNDEF
  PMER10M  = XUNDEF
  PSWD     = XUNDEF
  PSWU     = XUNDEF
  PSWBD    = XUNDEF
  PSWBU    = XUNDEF
  PLWD     = XUNDEF
  PLWU     = XUNDEF
  PFMU     = XUNDEF
  PFMV     = XUNDEF
  PRNC     = XUNDEF
  PHC      = XUNDEF
  PLEC     = XUNDEF
  PLEIC    = XUNDEF
  PGFLUXC  = XUNDEF
  PSWDC    = XUNDEF
  PSWUC    = XUNDEF
  PLWDC    = XUNDEF
  PLWUC    = XUNDEF
  PFMUC    = XUNDEF
  PFMVC    = XUNDEF 
  PT2M_MIN = XUNDEF
  PT2M_MAX = XUNDEF
  PHU2M_MIN= XUNDEF
  PHU2M_MAX= XUNDEF  
  PWIND10M = XUNDEF
  PWIND10M_MAX = XUNDEF
ELSE IF (CTOWN=='NONE  ') THEN
  PRN      = XUNDEF
  PH       = XUNDEF
  PLE      = XUNDEF
  PLEI     = XUNDEF
  PGFLUX   = XUNDEF
  PRI      = XUNDEF
  PCD      = XUNDEF
  PCH      = XUNDEF
  PCE      = XUNDEF
  PQS      = XUNDEF
  PZ0      = XUNDEF
  PZ0H     = XUNDEF
  PTS      = XUNDEF
  PT2M     = XUNDEF
  PQ2M     = XUNDEF
  PHU2M    = XUNDEF
  PZON10M  = XUNDEF
  PMER10M  = XUNDEF
  PSWD     = XUNDEF
  PSWU     = XUNDEF
  PSWBD    = XUNDEF
  PSWBU    = XUNDEF
  PLWD     = XUNDEF
  PLWU     = XUNDEF
  PFMU     = XUNDEF
  PFMV     = XUNDEF
  PRNC     = XUNDEF
  PHC      = XUNDEF
  PLEC     = XUNDEF
  PLEIC    = XUNDEF
  PGFLUXC  = XUNDEF
  PSWDC    = XUNDEF
  PSWUC    = XUNDEF
  PLWDC    = XUNDEF
  PLWUC    = XUNDEF
  PFMUC    = XUNDEF
  PFMVC    = XUNDEF 
  PT2M_MIN = XUNDEF
  PT2M_MAX = XUNDEF
  PHU2M_MIN= XUNDEF
  PHU2M_MAX= XUNDEF  
  PWIND10M = XUNDEF
  PWIND10M_MAX = XUNDEF
END IF
IF (LHOOK) CALL DR_HOOK('DIAG_TOWN_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE DIAG_TOWN_n
