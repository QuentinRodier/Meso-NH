!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE WRITE_DIAG_MIP_SURF_ATM_n (DTCO, DGO, D, U, HPROGRAM)
!     #################################
!
!!****  *WRITE_DIAG_MIP_SURF_ATM_n* - writes surface diagnostics
!!
!!    PURPOSE
!!    -------
!!
!!    Total grid-cell diag
!!
!!**  METHOD
!!    ------
!!
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
!!      Original    12/2016
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_DIAG_n,       ONLY : DIAG_t, DIAG_OPTIONS_t
USE MODD_SURF_ATM_n,   ONLY : SURF_ATM_t
!
USE MODD_SURF_PAR, ONLY : XUNDEF
USE MODD_CO2V_PAR, ONLY : XMC, XMCO2
USE MODD_SURF_ATM, ONLY : LCO2FOS
USE MODD_XIOS,     ONLY : LALLOW_ADD_DIM, YSWBAND_DIM_NAME
!
USE MODE_THERMOS
!
USE MODI_INIT_IO_SURF_n
USE MODI_WRITE_SURF
USE MODI_END_IO_SURF_n
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
TYPE(DATA_COVER_t),   INTENT(INOUT) :: DTCO
TYPE(DIAG_OPTIONS_t), INTENT(INOUT) :: DGO
TYPE(DIAG_t),         INTENT(INOUT) :: D
TYPE(SURF_ATM_t),     INTENT(INOUT) :: U
!
CHARACTER(LEN=6),     INTENT(IN)    :: HPROGRAM ! program calling
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
REAL, DIMENSION(SIZE(D%XTS))                 :: ZWORK
REAL, DIMENSION(SIZE(D%XTS),SIZE(D%XSWBD,2)) :: ZALB_DIR
REAL, DIMENSION(SIZE(D%XTS),SIZE(D%XSWBD,2)) :: ZALB_SCA
!
INTEGER           :: JI, INI,INB, JSW
INTEGER           :: IRESP          ! IRESP  : return-code if a problem appears
CHARACTER(LEN=12) :: YRECFM         ! Name of the article to be read
CHARACTER(LEN=100):: YCOMMENT       ! Comment string
CHARACTER(LEN=2)  :: YNUM
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('WRITE_DIAG_MIP_SURF_ATM_N',0,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
! * Total grid-cell diag
!-------------------------------------------------------------------------------
!
!         Initialisation for IO
!
CALL INIT_IO_SURF_n(DTCO, U, HPROGRAM,'FULL  ','SURF  ','WRITE','SURF_ATM_DIAGNOSTICS.OUT.nc')
!
!-------------------------------------------------------------------------------
! * Atmospheric forcing (offline mode)
!-------------------------------------------------------------------------------
!
INI=SIZE(D%XQA_FRC)
!
YRECFM='zref_frc'
YCOMMENT='zref level (m)'
CALL WRITE_SURF(DGO%CSELECT,HPROGRAM,YRECFM,D%XZREF,IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='uref_frc'
YCOMMENT='uref level (m)'
CALL WRITE_SURF(DGO%CSELECT,HPROGRAM,YRECFM,D%XUREF,IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='tas_frc'
YCOMMENT='surface air temperature at zref level (K)'
CALL WRITE_SURF(DGO%CSELECT,HPROGRAM,YRECFM,D%XTA_FRC,IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='ps_frc'
YCOMMENT='surface pressure (Pa)'
CALL WRITE_SURF(DGO%CSELECT,HPROGRAM,YRECFM,D%XPS,IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='huss_frc'
YCOMMENT='near-surface specific humidity at zref level (kg/kg)'
CALL WRITE_SURF(DGO%CSELECT,HPROGRAM,YRECFM,D%XQA_FRC,IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='hurs_frc'
YCOMMENT='near-surface relative humidity at zref level (%)'
DO JI=1,INI
   IF(D%XQA_FRC(JI)/=XUNDEF)THEN
     ZWORK(JI)=QSAT(D%XTA_FRC(JI),D%XPS(JI))
     ZWORK(JI)=100.*D%XQA_FRC(JI)/ZWORK(JI)
   ELSE
     ZWORK(JI)=XUNDEF
   ENDIF
ENDDO
CALL WRITE_SURF(DGO%CSELECT,HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='uas_frc'
YCOMMENT='Eastward component of the near-surface wind at Uref level (m/s)'
CALL WRITE_SURF(DGO%CSELECT,HPROGRAM,YRECFM,D%XU_FRC,IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='vas_frc'
YCOMMENT='Northward component of the near-surface wind at Uref level (m/s)'
CALL WRITE_SURF(DGO%CSELECT,HPROGRAM,YRECFM,D%XV_FRC,IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='sfcWind_frc'
YCOMMENT='near-surface wind wpeed at Uref level (m/s)'
WHERE(D%XQA_FRC(:)/=XUNDEF)
     ZWORK(:)=SQRT(D%XU_FRC(:)*D%XU_FRC(:)+D%XV_FRC(:)*D%XV_FRC(:))
ELSEWHERE
     ZWORK(:)=XUNDEF
ENDWHERE
CALL WRITE_SURF(DGO%CSELECT,HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='rainf_frc'
YCOMMENT='rainfall rate (kg m-2 s-1)'
CALL WRITE_SURF(DGO%CSELECT,HPROGRAM,YRECFM,D%XRAIN_FRC(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='snowf_frc'
YCOMMENT='snowfall rate (kg m-2 s-1)'
CALL WRITE_SURF(DGO%CSELECT,HPROGRAM,YRECFM,D%XSNOW_FRC(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='co2_frc'
YCOMMENT='atmospheric CO2 concentration (ppm)'
CALL WRITE_SURF(DGO%CSELECT,HPROGRAM,YRECFM,D%XCO2_FRC(:),IRESP,HCOMMENT=YCOMMENT)
!
!-------------------------------------------------------------------------------
! * Surface temperature
!-------------------------------------------------------------------------------
!
YRECFM='ts'
YCOMMENT='total surface temperature (K)'
CALL WRITE_SURF(DGO%CSELECT,HPROGRAM,YRECFM,D%XTS(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='tr'
YCOMMENT='total radiative surface temperature (K)'
CALL WRITE_SURF(DGO%CSELECT,HPROGRAM,YRECFM,D%XTRAD(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='tslsi'
YCOMMENT='Surface Temperature Where Land or Sea Ice  (K)'
CALL WRITE_SURF(DGO%CSELECT,HPROGRAM,YRECFM,D%XTSLSI(:),IRESP,HCOMMENT=YCOMMENT)
!
!-------------------------------------------------------------------------------
! * Near surface atmospheric variables
!-------------------------------------------------------------------------------
!
YRECFM='tas'
YCOMMENT='near-surface air temperature at 2m (K)'
CALL WRITE_SURF(DGO%CSELECT,HPROGRAM,YRECFM,D%XT2M(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='huss'
YCOMMENT='near-surface specific humidity at 2m (kg/kg)'
CALL WRITE_SURF(DGO%CSELECT,HPROGRAM,YRECFM,D%XQ2M(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='hurs'
YCOMMENT='near-surface relative humidity at 2m (-)'
CALL WRITE_SURF(DGO%CSELECT,HPROGRAM,YRECFM,D%XHU2M(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='uas'
YCOMMENT='Eastward component of the near-surface wind at 10m (m/s)'
CALL WRITE_SURF(DGO%CSELECT,HPROGRAM,YRECFM,D%XZON10M(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='vas'
YCOMMENT='Northward component of the near-surface wind at 10m (m/s)'
CALL WRITE_SURF(DGO%CSELECT,HPROGRAM,YRECFM,D%XMER10M(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='sfcWind'
YCOMMENT='near-surface wind speed at 10m (m/s)'
CALL WRITE_SURF(DGO%CSELECT,HPROGRAM,YRECFM,D%XWIND10M(:),IRESP,HCOMMENT=YCOMMENT)
!
!-------------------------------------------------------------------------------
! * Radiative fluxes
!-------------------------------------------------------------------------------
!
YRECFM='rss'
YCOMMENT='net short wave radiation (W m-2)'
ZWORK(:)=D%XSWD(:)-D%XSWU(:)
CALL WRITE_SURF(DGO%CSELECT,HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='rsds'
YCOMMENT='short wave downward radiation (W m-2)'
CALL WRITE_SURF(DGO%CSELECT,HPROGRAM,YRECFM,D%XSWD(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='rsdsdiff'
YCOMMENT='diffusive short wave downward radiation (W m-2)'
CALL WRITE_SURF(DGO%CSELECT,HPROGRAM,YRECFM,D%XSWD_SCA(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='rsus'
YCOMMENT='short wave upward radiation (W m-2)'
CALL WRITE_SURF(DGO%CSELECT,HPROGRAM,YRECFM,D%XSWU(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='rls'
YCOMMENT='net long wave radiation (W m-2)'
ZWORK(:)=D%XLWD(:)-D%XLWU(:)
CALL WRITE_SURF(DGO%CSELECT,HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='rlds'
YCOMMENT='long wave downward radiation (W m-2)'
CALL WRITE_SURF(DGO%CSELECT,HPROGRAM,YRECFM,D%XLWD(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='rlus'
YCOMMENT='long wave upward radiation (W m-2)'
CALL WRITE_SURF(DGO%CSELECT,HPROGRAM,YRECFM,D%XLWU(:),IRESP,HCOMMENT=YCOMMENT)
!
!-------------------------------------------------------------------------------
! * Surface albedo
!-------------------------------------------------------------------------------
!
YRECFM='albsrfc'
YCOMMENT='surface albedo (-)'
WHERE(D%XSWD(:)>0.0)
  ZWORK(:) = D%XSWU(:)/D%XSWD(:)
ELSEWHERE
  ZWORK(:) = XUNDEF
ENDWHERE
CALL WRITE_SURF(DGO%CSELECT,HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
!
WHERE(D%XSWBD(:,:)>0.0)
     ZALB_DIR(:,:) = D%XALB_DIR(:,:)
     ZALB_SCA(:,:) = D%XALB_SCA(:,:)
ELSEWHERE
     ZALB_DIR(:,:) = XUNDEF
     ZALB_SCA(:,:) = XUNDEF
ENDWHERE
!
IF (LALLOW_ADD_DIM)  THEN 
!
   YRECFM='albdirbnd'
   YCOMMENT='Direct surface albedo for each band (-)'
   CALL WRITE_SURF(DGO%CSELECT,HPROGRAM,YRECFM,ZALB_DIR(:,:),IRESP,HCOMMENT=YCOMMENT,HNAM_DIM=YSWBAND_DIM_NAME)
!
   YRECFM='albdiffbnd'
   YCOMMENT='diffuse surface albedo for each band (-)'
   CALL WRITE_SURF(DGO%CSELECT,HPROGRAM,YRECFM,ZALB_SCA(:,:),IRESP,HCOMMENT=YCOMMENT,HNAM_DIM=YSWBAND_DIM_NAME)
!
ELSE
!
   INB = SIZE(D%XALB_DIR,2)
!
   DO JSW=1,INB
!
          WRITE(YNUM,'(I2)')JSW
!
          YRECFM='albdirbnd'//ADJUSTL(YNUM(:LEN_TRIM(YNUM)))
          YCOMMENT='Direct surface albedo (-) for band '//ADJUSTL(YNUM(:LEN_TRIM(YNUM)))
          CALL WRITE_SURF(DGO%CSELECT,HPROGRAM,YRECFM,ZALB_DIR(:,JSW),IRESP,HCOMMENT=YCOMMENT)
!
          YRECFM='albdiffbnd'//ADJUSTL(YNUM(:LEN_TRIM(YNUM)))
          YCOMMENT='diffuse surface albedo (-) for band '//ADJUSTL(YNUM(:LEN_TRIM(YNUM)))
          CALL WRITE_SURF(DGO%CSELECT,HPROGRAM,YRECFM,ZALB_SCA(:,JSW),IRESP,HCOMMENT=YCOMMENT)
!
   ENDDO
!
ENDIF
!
!-------------------------------------------------------------------------------
! * Energy fluxes
!-------------------------------------------------------------------------------
!
YRECFM='hfls'
YCOMMENT='total latent heat flux (W m-2)'
CALL WRITE_SURF(DGO%CSELECT,HPROGRAM,YRECFM,D%XLE(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='hfsbl'
YCOMMENT='energy of sublimation (solid to vapor) over land (W m-2)'
CALL WRITE_SURF(DGO%CSELECT,HPROGRAM,YRECFM,D%XLEI(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='hfss'
YCOMMENT='Sensible heat flux (W m-2)'
CALL WRITE_SURF(DGO%CSELECT,HPROGRAM,YRECFM,D%XH(:),IRESP,HCOMMENT=YCOMMENT)
!
!-------------------------------------------------------------------------------
! * Momentum fluxes
!-------------------------------------------------------------------------------
!
YRECFM='tauu'
YCOMMENT='Surface Downward Eastward Wind Stress (Pa)'
CALL WRITE_SURF(DGO%CSELECT,HPROGRAM,YRECFM,D%XSSO_FMU(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='tauv'
YCOMMENT='Surface Downward Northward Wind Stress (Pa)'
CALL WRITE_SURF(DGO%CSELECT,HPROGRAM,YRECFM,D%XSSO_FMV(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='tau'
YCOMMENT='Surface Downward Wind Stress (Pa)'
ZWORK(:)=SQRT(D%XSSO_FMU(:)*D%XSSO_FMU(:)+D%XSSO_FMV(:)*D%XSSO_FMV(:))
CALL WRITE_SURF(DGO%CSELECT,HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
!
!-------------------------------------------------------------------------------
! * Water fluxes
!-------------------------------------------------------------------------------
!
YRECFM='evspsbl'
YCOMMENT='water evaporation flux including sublimation (kg m-2 s-1)'
CALL WRITE_SURF(DGO%CSELECT,HPROGRAM,YRECFM,D%XEVAP(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='sbl'
YCOMMENT='surface snow and ice sublimation flux (kg m-2 s-1)'
CALL WRITE_SURF(DGO%CSELECT,HPROGRAM,YRECFM,D%XSUBL(:),IRESP,HCOMMENT=YCOMMENT)
!
!-------------------------------------------------------------------------------
! * Carbon fluxes
!-------------------------------------------------------------------------------
!
IF(LCO2FOS)THEN
  YRECFM='fco2all'
  YCOMMENT='Carbon Mass Flux into the Atmosphere (kg m-2 s-1)'
  ZWORK(:)=D%XSFCO2(:)*D%XRHOA(:)*(XMC/XMCO2)  ! kgC02/kgAir m/s --> kgC/m2/s
  CALL WRITE_SURF(DGO%CSELECT,HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='fco2nat'
  YCOMMENT='Carbon Mass Flux into the Atmosphere Due to Natural Sources (kg m-2 s-1)'
  CALL WRITE_SURF(DGO%CSELECT,HPROGRAM,YRECFM,D%XSFCO2NAT(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='fco2antt'
  YCOMMENT='Carbon Mass Flux into the Atmosphere Due to anthropogenic Sources (kg m-2 s-1)'
  CALL WRITE_SURF(DGO%CSELECT,HPROGRAM,YRECFM,D%XSFCO2ANT(:),IRESP,HCOMMENT=YCOMMENT)
  !
  YRECFM='fco2fos'
  YCOMMENT='Carbon Mass Flux into Atmosphere Due to Fossil Fuel Emissions only  (kg m-2 s-1)'
  CALL WRITE_SURF(DGO%CSELECT,HPROGRAM,YRECFM,U%XCO2FOS(:),IRESP,HCOMMENT=YCOMMENT)
ENDIF
!
!-------------------------------------------------------------------------------
!
!         End of IO
!
CALL END_IO_SURF_n(HPROGRAM)
!
IF (LHOOK) CALL DR_HOOK('WRITE_DIAG_MIP_SURF_ATM_N',1,ZHOOK_HANDLE)
!
!
END SUBROUTINE WRITE_DIAG_MIP_SURF_ATM_n
