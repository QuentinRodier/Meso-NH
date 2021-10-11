!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!#########
SUBROUTINE SFX_OASIS_SEND(KLUOUT,KGPTOT,KI,KDATE,                               &
                          OSEND_LAND,OSEND_LAKE,OSEND_SEA,OSEND_WAVE,  &
                          PLAND_RUNOFF,PLAND_DRAIN,PLAND_CALVING,               &
                          PLAND_SRCFLOOD,                                       &
                          PLAKE_EVAP,PLAKE_RAIN,PLAKE_SNOW,PLAKE_WATF,          &
                          PSEA_FWSU,PSEA_FWSV,PSEA_HEAT,PSEA_SNET,PSEA_WIND,    &
                          PSEA_FWSM,PSEA_EVAP,PSEA_RAIN,PSEA_SNOW,              &
                          PSEA_WATF,PSEA_PRES,PSEAICE_HEAT,PSEAICE_SNET,        &
                          PSEAICE_EVAP,PWAVE_U10,PWAVE_V10            )
!###########################################
!
!!****  *SFX_OASIS_SEND* - Send coupling fields
!!
!!    PURPOSE
!!    -------
!!
!!    Attention : all fields are sent in Pa, m/s, W/m2 or kg/m2/s
!!   
!!
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
!!	B. Decharme   *Meteo France*	
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    10/2013
!!      Modified    11/2014 : J. Pianezze - add wave coupling parameters
!!                                          and surface pressure for ocean coupling
!!    10/2016 B. Decharme : bug surface/groundwater coupling
!!    02/2020 C. Lebeaupin : sfxsnd subroutine, KGPTOT entry
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODN_SFX_OASIS,  ONLY : XTSTEP_CPL_SEA, XTSTEP_CPL_WAVE, XTSTEP_CPL_LAKE, &
                            XTSTEP_CPL_LAND
!                    
USE MODD_SURF_PAR,   ONLY : XUNDEF, NUNDEF
!
USE MODD_SFX_OASIS
!
USE MODI_GET_LUOUT
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
#ifdef CPLOASIS
USE MOD_OASIS
#endif
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
INTEGER,             INTENT(IN) :: KLUOUT
INTEGER,             INTENT(IN) :: KI            ! number of physical points on proc
INTEGER,             INTENT(IN) :: KGPTOT        ! total number of points on proc
INTEGER,             INTENT(IN) :: KDATE  ! current coupling time step (s)
LOGICAL,             INTENT(IN) :: OSEND_LAND
LOGICAL,             INTENT(IN) :: OSEND_LAKE
LOGICAL,             INTENT(IN) :: OSEND_SEA
LOGICAL,             INTENT(IN) :: OSEND_WAVE
!
REAL, DIMENSION(KI), INTENT(IN) :: PLAND_RUNOFF    ! Cumulated Surface runoff             (kg/m2)
REAL, DIMENSION(KI), INTENT(IN) :: PLAND_DRAIN     ! Cumulated Deep drainage              (kg/m2)
REAL, DIMENSION(KI), INTENT(IN) :: PLAND_CALVING   ! Cumulated Calving flux               (kg/m2)
REAL, DIMENSION(KI), INTENT(IN) :: PLAND_SRCFLOOD  ! Cumulated flood freshwater flux      (kg/m2)
!
REAL, DIMENSION(KI), INTENT(IN) :: PLAKE_EVAP  ! Cumulated Evaporation              (kg/m2)
REAL, DIMENSION(KI), INTENT(IN) :: PLAKE_RAIN  ! Cumulated Rainfall rate            (kg/m2)
REAL, DIMENSION(KI), INTENT(IN) :: PLAKE_SNOW  ! Cumulated Snowfall rate            (kg/m2)
REAL, DIMENSION(KI), INTENT(IN) :: PLAKE_WATF  ! Cumulated freshwater flux          (kg/m2)
!
REAL, DIMENSION(KI), INTENT(IN) :: PSEA_FWSU  ! Cumulated zonal wind stress       (Pa.s)
REAL, DIMENSION(KI), INTENT(IN) :: PSEA_FWSV  ! Cumulated meridian wind stress    (Pa.s)
REAL, DIMENSION(KI), INTENT(IN) :: PSEA_HEAT  ! Cumulated Non solar net heat flux (J/m2)
REAL, DIMENSION(KI), INTENT(IN) :: PSEA_SNET  ! Cumulated Solar net heat flux     (J/m2)
REAL, DIMENSION(KI), INTENT(IN) :: PSEA_WIND  ! Cumulated 10m wind speed          (m)
REAL, DIMENSION(KI), INTENT(IN) :: PSEA_FWSM  ! Cumulated wind stress             (Pa.s)
REAL, DIMENSION(KI), INTENT(IN) :: PSEA_EVAP  ! Cumulated Evaporation             (kg/m2)
REAL, DIMENSION(KI), INTENT(IN) :: PSEA_RAIN  ! Cumulated Rainfall rate           (kg/m2)
REAL, DIMENSION(KI), INTENT(IN) :: PSEA_SNOW  ! Cumulated Snowfall rate           (kg/m2)
REAL, DIMENSION(KI), INTENT(IN) :: PSEA_WATF  ! Cumulated freshwater flux         (kg/m2)
REAL, DIMENSION(KI), INTENT(IN) :: PSEA_PRES  ! Cumulated Surface pressure        (Pa.s)
!
REAL, DIMENSION(KI), INTENT(IN) :: PSEAICE_HEAT ! Cumulated Sea-ice non solar net heat flux (J/m2)
REAL, DIMENSION(KI), INTENT(IN) :: PSEAICE_SNET ! Cumulated Sea-ice solar net heat flux     (J/m2)
REAL, DIMENSION(KI), INTENT(IN) :: PSEAICE_EVAP ! Cumulated Sea-ice sublimation             (kg/m2)
!
REAL, DIMENSION(KI), INTENT(IN) :: PWAVE_U10  ! 
REAL, DIMENSION(KI), INTENT(IN) :: PWAVE_V10  !
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
CHARACTER(LEN=50)     :: YCOMMENT
INTEGER               :: IERR   ! Error info
LOGICAL               :: GCUM
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
#ifdef CPLOASIS
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('SFX_OASIS_SEND',0,ZHOOK_HANDLE)
!
!*       1.     Initialize :
!               ------------
!
!-------------------------------------------------------------------------------
!
!*       2.     Send land fields to OASIS:
!               --------------------------
!
IF(OSEND_LAND)THEN
!
  GCUM=.TRUE.
!
! * Send river output fields
!
  YCOMMENT='Surface runoff over land'
  CALL SFXSND(KI,KGPTOT,KLUOUT,KDATE,YCOMMENT,XTSTEP_CPL_LAND,&
              NRUNOFF_ID,PLAND_RUNOFF,GCUM)
!
  YCOMMENT='Deep drainage over land'
  CALL SFXSND(KI,KGPTOT,KLUOUT,KDATE,YCOMMENT,XTSTEP_CPL_LAND,&
              NDRAIN_ID,PLAND_DRAIN,GCUM)
!
  IF(LCPL_CALVING)THEN
    YCOMMENT='calving flux over land'
    CALL SFXSND(KI,KGPTOT,KLUOUT,KDATE,YCOMMENT,XTSTEP_CPL_LAND,&
                NCALVING_ID,PLAND_CALVING,GCUM)
  ENDIF
!
  IF(LCPL_FLOOD)THEN      
    YCOMMENT='flood freshwater flux over land (P-E-I)'
    CALL SFXSND(KI,KGPTOT,KLUOUT,KDATE,YCOMMENT,XTSTEP_CPL_LAND,&
                NSRCFLOOD_ID,PLAND_SRCFLOOD,GCUM)
  ENDIF
!
ENDIF
!
!-------------------------------------------------------------------------------
!
!*       3.     Send lake fields to OASIS :
!               --------------------------
IF(OSEND_LAKE)THEN
!
  GCUM=.TRUE.
!
! * Send output fields (in kg/m2/s)
!
  YCOMMENT='Evaporation over lake'
  CALL SFXSND(KI,KGPTOT,KLUOUT,KDATE,YCOMMENT,XTSTEP_CPL_LAKE,&
              NLAKE_EVAP_ID,PLAKE_EVAP,GCUM)
!
  YCOMMENT='Rainfall rate over lake'
  CALL SFXSND(KI,KGPTOT,KLUOUT,KDATE,YCOMMENT,XTSTEP_CPL_LAKE,&
              NLAKE_RAIN_ID,PLAKE_RAIN,GCUM)
!
  YCOMMENT='Snowfall rate over lake'
  CALL SFXSND(KI,KGPTOT,KLUOUT,KDATE,YCOMMENT,XTSTEP_CPL_LAKE,&
              NLAKE_SNOW_ID,PLAKE_SNOW,GCUM)
!
  YCOMMENT='Freshwater flux over lake (P-E)'
  CALL SFXSND(KI,KGPTOT,KLUOUT,KDATE,YCOMMENT,XTSTEP_CPL_LAKE,&
              NLAKE_WATF_ID,PLAKE_WATF,GCUM)
!
ENDIF
!
!-------------------------------------------------------------------------------
!
!*       4.     Send sea fields to OASIS :
!               --------------------------
!
IF(OSEND_SEA)THEN
!
  GCUM=.TRUE.
!
! * Send sea output fields (in Pa, m/s, W/m2 or kg/m2/s)
!
  YCOMMENT='zonal wind stress over sea'
  CALL SFXSND(KI,KGPTOT,KLUOUT,KDATE,YCOMMENT,XTSTEP_CPL_SEA,&
              NSEA_FWSU_ID,PSEA_FWSU,GCUM)
!
  YCOMMENT='meridian wind stress over sea'
  CALL SFXSND(KI,KGPTOT,KLUOUT,KDATE,YCOMMENT,XTSTEP_CPL_SEA,&
              NSEA_FWSV_ID,PSEA_FWSV,GCUM)
!
  YCOMMENT='Non solar net heat flux over sea'
  CALL SFXSND(KI,KGPTOT,KLUOUT,KDATE,YCOMMENT,XTSTEP_CPL_SEA,&
              NSEA_HEAT_ID,PSEA_HEAT,GCUM)
!
  YCOMMENT='Solar net heat flux over sea'
  CALL SFXSND(KI,KGPTOT,KLUOUT,KDATE,YCOMMENT,XTSTEP_CPL_SEA,&
              NSEA_SNET_ID,PSEA_SNET,GCUM)
!
  YCOMMENT='10m wind speed over sea'
  CALL SFXSND(KI,KGPTOT,KLUOUT,KDATE,YCOMMENT,XTSTEP_CPL_SEA,&
              NSEA_WIND_ID,PSEA_WIND,GCUM)
!
  YCOMMENT='wind stress over sea'
  CALL SFXSND(KI,KGPTOT,KLUOUT,KDATE,YCOMMENT,XTSTEP_CPL_SEA,&
              NSEA_FWSM_ID,PSEA_FWSM,GCUM)
!
  YCOMMENT='Evaporation over sea'
  CALL SFXSND(KI,KGPTOT,KLUOUT,KDATE,YCOMMENT,XTSTEP_CPL_SEA,&
              NSEA_EVAP_ID,PSEA_EVAP,GCUM)
!
  YCOMMENT='Rainfall rate over sea'
  CALL SFXSND(KI,KGPTOT,KLUOUT,KDATE,YCOMMENT,XTSTEP_CPL_SEA,&
              NSEA_RAIN_ID,PSEA_RAIN,GCUM)
!
  YCOMMENT='Snowfall rate over sea'
  CALL SFXSND(KI,KGPTOT,KLUOUT,KDATE,YCOMMENT,XTSTEP_CPL_SEA,&
              NSEA_SNOW_ID,PSEA_SNOW,GCUM)
!
  YCOMMENT='Freshwater flux over sea (P-E)'
  CALL SFXSND(KI,KGPTOT,KLUOUT,KDATE,YCOMMENT,XTSTEP_CPL_SEA,&
              NSEA_WATF_ID,PSEA_WATF,GCUM)
!
  YCOMMENT='Surface pressure'
  CALL SFXSND(KI,KGPTOT,KLUOUT,KDATE,YCOMMENT,XTSTEP_CPL_SEA,&
              NSEA_PRES_ID,PSEA_PRES,GCUM)
!
! * Sea-ice output fields (in W/m2 or kg/m2/s)
!
  IF(LCPL_SEAICE)THEN
!
    YCOMMENT='Sea-ice non solar net heat flux over sea-ice'
    CALL SFXSND(KI,KGPTOT,KLUOUT,KDATE,YCOMMENT,XTSTEP_CPL_SEA,&
                NSEAICE_HEAT_ID,PSEAICE_HEAT,GCUM)
!
    YCOMMENT='Sea-ice solar net heat flux over sea-ice'
    CALL SFXSND(KI,KGPTOT,KLUOUT,KDATE,YCOMMENT,XTSTEP_CPL_SEA,&
                NSEAICE_SNET_ID,PSEAICE_SNET,GCUM)
!
    YCOMMENT='Sea-ice sublimation over sea-ice'
    CALL SFXSND(KI,KGPTOT,KLUOUT,KDATE,YCOMMENT,XTSTEP_CPL_SEA,&
                NSEAICE_EVAP_ID,PSEAICE_EVAP,GCUM)
!
  ENDIF
!
ENDIF
!
!-------------------------------------------------------------------------------
!
!*       5.     Send wave fields to OASIS :
!               --------------------------
IF(OSEND_WAVE)THEN
!
  GCUM=.FALSE.
!
! * Send output fields
!
  YCOMMENT='10m u-wind speed'
  CALL SFXSND(KI,KGPTOT,KLUOUT,KDATE,YCOMMENT,XTSTEP_CPL_WAVE,&
              NWAVE_U10_ID,PWAVE_U10,GCUM)
!
  YCOMMENT='10m v-wind speed'
  CALL SFXSND(KI,KGPTOT,KLUOUT,KDATE,YCOMMENT,XTSTEP_CPL_WAVE,&
              NWAVE_V10_ID,PWAVE_V10,GCUM)
!
ENDIF
!
IF (LHOOK) CALL DR_HOOK('SFX_OASIS_SEND',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
CONTAINS
!-------------------------------------------------------------------------------
!
SUBROUTINE SFXSND(KI,KGPTOT,KLUOUT,KDATE,HCOMMENT,PTSTEP_CPL,KVAR_ID,PVAR,OCUM)
!
IMPLICIT NONE
!
INTEGER, INTENT(IN)             :: KI
INTEGER, INTENT(IN)             :: KGPTOT
INTEGER, INTENT(IN)             :: KLUOUT
INTEGER, INTENT(IN)             :: KDATE
CHARACTER(LEN=*), INTENT(IN)    :: HCOMMENT
REAL, INTENT(IN)                :: PTSTEP_CPL ! coupling time step (s)
INTEGER, INTENT(IN)             :: KVAR_ID    ! flux id for OASIS
REAL, DIMENSION(KI), INTENT(IN) :: PVAR       ! Cumulated flux
LOGICAL, INTENT(IN)             :: OCUM       ! true if cumulated
!
REAL, DIMENSION(KI,1) :: ZWRITE ! Mean flux send to OASIS (Pa, m/s, W/m2 or kg/m2/s)
INTEGER               :: IERR   ! Error info
REAL(KIND=JPRB)       :: ZHOOK_HANDLE
#ifdef ARO
REAL, DIMENSION(KGPTOT,1) :: ZFLDW ! extended array
REAL, DIMENSION(KGPTOT)   :: ZEXTW ! extended array
#endif
!
!--------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('SFX_OASIS_SEND:SFXSND',0,ZHOOK_HANDLE)
!
ZWRITE(:,:)=XUNDEF
!
IF(KVAR_ID/=NUNDEF)THEN
!
#ifdef ARO
  IF (KI < KGPTOT) THEN  ! AROME case
    WRITE(KLUOUT,*) '**WARNING**: SFX_OASIS_SEND EXTENDED ARRAY FOR ', HCOMMENT
    ZEXTW(:)=XUNDEF
    ZEXTW(1:KI)= PVAR(:)
    IF(OCUM)THEN
      CALL OUTVAR(ZEXTW,PTSTEP_CPL,ZFLDW(:,1))
    ELSE
      ZFLDW(:,1)=ZEXTW(:)
    ENDIF
    CALL OASIS_PUT(KVAR_ID,KDATE,ZFLDW(:,:),IERR)
    CALL CHECK_SFX_SEND(KLUOUT,IERR,HCOMMENT,ZFLDW(:,1))
  ELSE
#endif
  IF(OCUM)THEN
    CALL OUTVAR(PVAR,PTSTEP_CPL,ZWRITE(:,1))
  ELSE
    ZWRITE(:,1)=PVAR(:)
  ENDIF
  CALL OASIS_PUT(KVAR_ID,KDATE,ZWRITE(:,:),IERR)
  CALL CHECK_SFX_SEND(KLUOUT,IERR,HCOMMENT,ZWRITE(:,1))
#ifdef ARO
  ENDIF
#endif
!
ENDIF
!
IF (LHOOK) CALL DR_HOOK('SFX_OASIS_SEND:SFXSND',1,ZHOOK_HANDLE)
!
END SUBROUTINE SFXSND
!
!-------------------------------------------------------------------------------
!
!
SUBROUTINE CHECK_SFX_SEND(KLUOUT,KERR,HCOMMENT,PWRITE)
!
USE MODI_ABOR1_SFX
!
IMPLICIT NONE
!
INTEGER,          INTENT(IN) :: KLUOUT
INTEGER,          INTENT(IN) :: KERR
CHARACTER(LEN=*), INTENT(IN) :: HCOMMENT
!
REAL, DIMENSION(:), INTENT(OUT):: PWRITE
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('SFX_OASIS_SEND:CHECK_SFX_SEND',0,ZHOOK_HANDLE)
!
PWRITE(:) = XUNDEF
!
IF (KERR/=OASIS_OK.AND.KERR<OASIS_SENT) THEN
   WRITE(KLUOUT,'(A,I4)')'Return OASIS code from sending '//TRIM(HCOMMENT)//' : ',KERR
   CALL ABOR1_SFX('SFX_OASIS_SEND: problem sending '//TRIM(HCOMMENT))
ENDIF 
!
IF (LHOOK) CALL DR_HOOK('SFX_OASIS_SEND:CHECK_SFX_SEND',1,ZHOOK_HANDLE)
!
END SUBROUTINE CHECK_SFX_SEND
!
!-------------------------------------------------------------------------------
!
SUBROUTINE OUTVAR(PIN,PDIV,PWRITE)
!
IMPLICIT NONE
!
REAL, DIMENSION(:), INTENT(IN) :: PIN
REAL,               INTENT(IN) :: PDIV
!
REAL, DIMENSION(:), INTENT(OUT):: PWRITE
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('SFX_OASIS_SEND:OUTVAR',0,ZHOOK_HANDLE)
!
WHERE(PIN(:)/=XUNDEF)
     PWRITE(:)=PIN(:)/PDIV
ELSEWHERE
     PWRITE(:)=XUNDEF
ENDWHERE
!
IF (LHOOK) CALL DR_HOOK('SFX_OASIS_SEND:OUTVAR',1,ZHOOK_HANDLE)
!
END SUBROUTINE OUTVAR
!
!-------------------------------------------------------------------------------
#endif
!-------------------------------------------------------------------------------
!
END SUBROUTINE SFX_OASIS_SEND
