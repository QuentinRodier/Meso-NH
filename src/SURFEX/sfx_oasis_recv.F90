!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!#########
SUBROUTINE SFX_OASIS_RECV(HPROGRAM,KGPTOT,KI,KSW,PTIMEC,         &
                          ORECV_LAND, ORECV_SEA, ORECV_WAVE,     &
                          PLAND_WTD,PLAND_FWTD,                  &
                          PLAND_FFLOOD,PLAND_PIFLOOD,            &
                          PSEA_SST,PSEA_UCU,PSEA_VCU,            &
                          PSEAICE_SIT,PSEAICE_CVR,PSEAICE_ALB,   &
                          PWAVE_CHA,PWAVE_UCU,PWAVE_VCU,         &
                          PWAVE_HS,PWAVE_TP             )
!########################################
!
!!****  *SFX_OASIS_RECV* - Receive coupling fields from oasis
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
!!      B. Decharme   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    10/2013
!!      Modified    11/2014 : J. Pianezze - add wave coupling parameters
!!                  01/2020 : C. Lebeaupin : 
!!                              add sfxrcv with phys/spectral domain distinction for ARO
!!                              current components inout (init to 0. before)
!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
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
CHARACTER(LEN=*),       INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
INTEGER,                INTENT(IN)  :: KI        ! number of physical points on proc
INTEGER,                INTENT(IN)  :: KGPTOT    ! total number of points on proc
INTEGER,                INTENT(IN)  :: KSW       ! number of short-wave spectral bands
REAL,                   INTENT(IN)  :: PTIMEC    ! Cumulated run time step (s)
!
LOGICAL,                INTENT(IN)  :: ORECV_LAND
LOGICAL,                INTENT(IN)  :: ORECV_SEA
LOGICAL,                INTENT(IN)  :: ORECV_WAVE
!
REAL, DIMENSION(KI),    INTENT(OUT) :: PLAND_WTD     ! Land water table depth (m)
REAL, DIMENSION(KI),    INTENT(OUT) :: PLAND_FWTD    ! Land grid-cell fraction of water table rise (-)
REAL, DIMENSION(KI),    INTENT(OUT) :: PLAND_FFLOOD  ! Land Floodplains fraction (-)
REAL, DIMENSION(KI),    INTENT(OUT) :: PLAND_PIFLOOD ! Land Potential flood infiltration (kg/m2/s)
!
REAL, DIMENSION(KI),    INTENT(OUT) :: PSEA_SST ! Sea surface temperature (K)
REAL, DIMENSION(KI),    INTENT(INOUT) :: PSEA_UCU ! Sea u-current (m/s)
REAL, DIMENSION(KI),    INTENT(INOUT) :: PSEA_VCU ! Sea v-current (m/s)
!
REAL, DIMENSION(KI),    INTENT(OUT) :: PSEAICE_SIT ! Sea-ice Temperature (K)
REAL, DIMENSION(KI),    INTENT(OUT) :: PSEAICE_CVR ! Sea-ice cover (-)
REAL, DIMENSION(KI),    INTENT(OUT) :: PSEAICE_ALB ! Sea-ice albedo (-)
!
REAL, DIMENSION(KI),    INTENT(OUT) :: PWAVE_CHA ! Charnock coefficient (-)
REAL, DIMENSION(KI),    INTENT(OUT) :: PWAVE_UCU ! u-current velocity (m/s)
REAL, DIMENSION(KI),    INTENT(OUT) :: PWAVE_VCU ! v-current velocity (m/s)
REAL, DIMENSION(KI),    INTENT(OUT) :: PWAVE_HS  ! Significant wave height (m)
REAL, DIMENSION(KI),    INTENT(OUT) :: PWAVE_TP  ! Peak period (s)
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER               :: IDATE  ! current coupling time step (s)
INTEGER               :: IERR   ! Error info
INTEGER               :: ILUOUT
CHARACTER(LEN=50)     :: YCOMMENT
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
#ifdef CPLOASIS
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('SFX_OASIS_RECV',0,ZHOOK_HANDLE)
!
!*       1.     Initialize :
!               ------------
!
CALL GET_LUOUT(HPROGRAM,ILUOUT)
#ifdef ARO
!
IF( KI /= KGPTOT ) THEN
  WRITE(ILUOUT,*) '**WARNING**: KI ', KI, 'is different from ',KGPTOT
  IF(HPROGRAM=='AROME ') WRITE(ILUOUT,*) 'SFX_OASIS_RECV: ARRAY REDUCED AFTER GET'
ENDIF
#endif
!
IDATE = INT(PTIMEC)
!
!-------------------------------------------------------------------------------
!
!*       2.     Get Land surface variable :
!               ------------------------------------
!
IF(ORECV_LAND)THEN
!
! * Init river input fields
!
  PLAND_WTD    (:) = XUNDEF
  PLAND_FWTD   (:) = XUNDEF
  PLAND_FFLOOD (:) = XUNDEF
  PLAND_PIFLOOD(:) = XUNDEF
!
! * Receive river input fields
!
  IF(LCPL_GW)THEN
!
    YCOMMENT='water table depth'
    CALL SFXRCV(KI,KGPTOT,ILUOUT,IDATE,HPROGRAM,YCOMMENT,NWTD_ID,PLAND_WTD)
!
    YCOMMENT='fraction of water table rise'
    CALL SFXRCV(KI,KGPTOT,ILUOUT,IDATE,HPROGRAM,YCOMMENT,NFWTD_ID,PLAND_FWTD)
!
  ENDIF
!
  IF(LCPL_FLOOD)THEN
!
    YCOMMENT='Flood fraction'
    CALL SFXRCV(KI,KGPTOT,ILUOUT,IDATE,HPROGRAM,YCOMMENT,NFFLOOD_ID,PLAND_FFLOOD)
!
    YCOMMENT='Potential flood infiltration'
    CALL SFXRCV(KI,KGPTOT,ILUOUT,IDATE,HPROGRAM,YCOMMENT,NPIFLOOD_ID,PLAND_PIFLOOD)
!
    WHERE(PLAND_PIFLOOD(:)==0.0)PLAND_FFLOOD(:)=0.0
!
  ENDIF
!
ENDIF
!
!-------------------------------------------------------------------------------
!
!*       3.     Get Sea variables :
!               -----------------------------
!
!
IF(ORECV_SEA)THEN
!
! * Init ocean input fields
!
  PSEA_SST (:) = XUNDEF
!  PSEA_UCU (:) = XUNDEF
!  PSEA_VCU (:) = XUNDEF
!
  PSEAICE_SIT (:) = XUNDEF
  PSEAICE_CVR (:) = XUNDEF
  PSEAICE_ALB (:) = XUNDEF
!
! * Receive ocean input fields
!
  YCOMMENT='Sea surface temperature'
  CALL SFXRCV(KI,KGPTOT,ILUOUT,IDATE,HPROGRAM,YCOMMENT,NSEA_SST_ID,PSEA_SST)
!
  YCOMMENT='Sea u-current'
  CALL SFXRCV(KI,KGPTOT,ILUOUT,IDATE,HPROGRAM,YCOMMENT,NSEA_UCU_ID,PSEA_UCU)
!
  YCOMMENT='Sea v-current'
  CALL SFXRCV(KI,KGPTOT,ILUOUT,IDATE,HPROGRAM,YCOMMENT,NSEA_VCU_ID,PSEA_VCU)
!
  IF(LCPL_SEAICE)THEN
!
    YCOMMENT='Sea-ice Temperature'
    CALL SFXRCV(KI,KGPTOT,ILUOUT,IDATE,HPROGRAM,YCOMMENT,NSEAICE_SIT_ID,PSEAICE_SIT)
!
    YCOMMENT='Sea-ice cover'
    CALL SFXRCV(KI,KGPTOT,ILUOUT,IDATE,HPROGRAM,YCOMMENT,NSEAICE_CVR_ID,PSEAICE_CVR)
!
    YCOMMENT='Sea-ice albedo'
    CALL SFXRCV(KI,KGPTOT,ILUOUT,IDATE,HPROGRAM,YCOMMENT,NSEAICE_ALB_ID,PSEAICE_ALB)
!
  ENDIF
!
ENDIF
!-------------------------------------------------------------------------------
!
!*       4.     Get Wave variables :
!               -----------------------------
!
!
IF(ORECV_WAVE)THEN
!
! * Init wave input fields
!
  PWAVE_CHA (:) = XUNDEF
  PWAVE_UCU  (:) = XUNDEF
  PWAVE_VCU  (:) = XUNDEF
  PWAVE_HS  (:) = XUNDEF
  PWAVE_TP  (:) = XUNDEF
!
! * Receive wave input fields
!
  YCOMMENT='Charnock coefficient'
  CALL SFXRCV(KI,KGPTOT,ILUOUT,IDATE,HPROGRAM,YCOMMENT,NWAVE_CHA_ID,PWAVE_CHA)
!
  YCOMMENT='u-current velocity'
  CALL SFXRCV(KI,KGPTOT,ILUOUT,IDATE,HPROGRAM,YCOMMENT,NWAVE_UCU_ID,PWAVE_UCU)
!
  YCOMMENT='v-current velocity'
  CALL SFXRCV(KI,KGPTOT,ILUOUT,IDATE,HPROGRAM,YCOMMENT,NWAVE_VCU_ID,PWAVE_VCU)
!
  YCOMMENT='Significant wave height'
  CALL SFXRCV(KI,KGPTOT,ILUOUT,IDATE,HPROGRAM,YCOMMENT,NWAVE_HS_ID,PWAVE_HS)
!
  YCOMMENT='Peak period'
  CALL SFXRCV(KI,KGPTOT,ILUOUT,IDATE,HPROGRAM,YCOMMENT,NWAVE_TP_ID,PWAVE_TP)
!
ENDIF
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('SFX_OASIS_RECV',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
CONTAINS
!-------------------------------------------------------------------------------
!
SUBROUTINE SFXRCV(KI,KGPTOT,KLUOUT,KDATE,HPROGRAM,HCOMMENT,KVAR_ID,PVAR)
!
IMPLICIT NONE
!
INTEGER, INTENT(IN)             :: KI
INTEGER, INTENT(IN)             :: KGPTOT
INTEGER, INTENT(IN)             :: KLUOUT
INTEGER, INTENT(IN)             :: KDATE
CHARACTER(LEN=*), INTENT(IN)    :: HPROGRAM
CHARACTER(LEN=*), INTENT(IN)    :: HCOMMENT
INTEGER, INTENT(IN)             :: KVAR_ID ! flux id for OASIS
REAL, DIMENSION(KI), INTENT(INOUT) :: PVAR  ! Cumulated flux
!
REAL, DIMENSION(KI,1) :: ZREAD 
INTEGER :: IERR ! Error info
REAL(KIND=JPRB) :: ZHOOK_HANDLE
#ifdef ARO
REAL(KIND=JPRB), DIMENSION(KGPTOT,1) :: ZFLDR !TARGET
#endif
!--------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('SFX_OASIS_RECV:SFXRCV',0,ZHOOK_HANDLE)
!
ZREAD(:,:) = XUNDEF
!
IF(KVAR_ID/=NUNDEF)THEN
#ifdef ARO
  IF (HPROGRAM=='AROME ') THEN
    CALL OASIS_GET(KVAR_ID,KDATE,ZFLDR(:,:),IERR)
    CALL CHECK_RECV(KLUOUT,IERR,HCOMMENT)
    ZREAD(:,:)=ZFLDR(1:KI,:)
  ELSE
#endif
  CALL OASIS_GET(KVAR_ID,KDATE,ZREAD(:,:),IERR)
  CALL CHECK_RECV(KLUOUT,IERR,HCOMMENT)
#ifdef ARO
  ENDIF
#endif
    PVAR(:)=ZREAD(:,1)
ENDIF
!
IF (LHOOK) CALL DR_HOOK('SFX_OASIS_RECV:SFXRCV',1,ZHOOK_HANDLE)
!
END SUBROUTINE SFXRCV
!
!-------------------------------------------------------------------------------
!
SUBROUTINE CHECK_RECV(KLUOUT,KERR,HCOMMENT)
!
USE MODI_ABOR1_SFX
!
IMPLICIT NONE
!
INTEGER,          INTENT(IN)  :: KLUOUT
INTEGER,          INTENT(IN)  :: KERR
CHARACTER(LEN=*), INTENT(IN)  :: HCOMMENT
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('SFX_OASIS_RECV:CHECK_RECV',0,ZHOOK_HANDLE)
!
IF (KERR/=OASIS_OK.AND.KERR<OASIS_RECVD) THEN
   WRITE(KLUOUT,'(A,I4)')'Return OASIS code receiving '//TRIM(HCOMMENT)//' : ',KERR
   CALL ABOR1_SFX('SFX_OASIS_RECV: problem receiving '//TRIM(HCOMMENT)//' from OASIS')
ENDIF
!
IF (LHOOK) CALL DR_HOOK('SFX_OASIS_RECV:CHECK_RECV',1,ZHOOK_HANDLE)
!
END SUBROUTINE CHECK_RECV
!
!-------------------------------------------------------------------------------
#endif
!-------------------------------------------------------------------------------
!
END SUBROUTINE SFX_OASIS_RECV
