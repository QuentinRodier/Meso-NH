!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     ###############################################################################
SUBROUTINE COUPLING_ISBA_OROGRAPHY_n(HPROGRAM, HCOUPLING,                                    &
                 PTSTEP, KYEAR, KMONTH, KDAY, PTIME, KI, KSV, KSW, PTSUN, PZENITH, PZENITH2, &
                 PAZIM, PZREF, PUREF, PZS, PU, PV, PQA, PTA, PRHOA, PSV, PCO2, HSV,          &
                 PRAIN, PSNOW, PLW, PDIR_SW, PSCA_SW, PSW_BANDS, PPS, PPA,                   &
                 PSFTQ, PSFTH, PSFTS, PSFCO2, PSFU, PSFV,                                    &
                 PTRAD, PDIR_ALB, PSCA_ALB, PEMIS,                                           &
                 PPEW_A_COEF, PPEW_B_COEF,                                                   &
                 PPET_A_COEF, PPEQ_A_COEF, PPET_B_COEF, PPEQ_B_COEF,                         &
                 HTEST                                                                       )  
!     ###############################################################################
!
!!****  *COUPLING_ISBA_OROGRAPHY_n * - Parameterizes effects of subgrid 
!!     orography on radiative and energy fluxes
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
!!      modified    05/2004 by P. LeMoigne: vertical shift of implicit
!!                          coefficients
!!      B. Decharme   2008   reset the subgrid topographic effect on the forcing
!----------------------------------------------------------------
!
USE MODD_CSTS,   ONLY : XSTEFAN, XCPD, XRD, XP00
USE MODD_ISBA_n, ONLY : XSSO_SLOPE, XEMIS_NAT, XTSRAD_NAT, XZS
!
USE MODD_SURF_ATM, ONLY : LNOSOF, LVERTSHIFT
!
USE MODI_FORCING_VERT_SHIFT
USE MODI_COUPLING_ISBA_CANOPY_n
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
 CHARACTER(LEN=6),    INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
 CHARACTER(LEN=1),    INTENT(IN)  :: HCOUPLING ! type of coupling
                                              ! 'E' : explicit
                                              ! 'I' : implicit
INTEGER,             INTENT(IN)  :: KYEAR     ! current year (UTC)
INTEGER,             INTENT(IN)  :: KMONTH    ! current month (UTC)
INTEGER,             INTENT(IN)  :: KDAY      ! current day (UTC)
REAL,                INTENT(IN)  :: PTIME     ! current time since midnight (UTC, s)
INTEGER,             INTENT(IN)  :: KI        ! number of points
INTEGER,             INTENT(IN)  :: KSV       ! number of scalars
INTEGER,             INTENT(IN)  :: KSW       ! number of short-wave spectral bands
REAL, DIMENSION(KI), INTENT(IN)  :: PTSUN     ! solar time                    (s from midnight)
REAL,                INTENT(IN)  :: PTSTEP    ! atmospheric time-step                 (s)
REAL, DIMENSION(KI), INTENT(IN)  :: PZREF     ! height of T,q forcing                 (m)
REAL, DIMENSION(KI), INTENT(IN)  :: PUREF     ! height of wind forcing                (m)
!
REAL, DIMENSION(KI), INTENT(IN)  :: PTA       ! air temperature forcing               (K)
REAL, DIMENSION(KI), INTENT(IN)  :: PQA       ! air humidity forcing                  (kg/m3)
REAL, DIMENSION(KI), INTENT(IN)  :: PRHOA     ! air density                           (kg/m3)
REAL, DIMENSION(KI,KSV),INTENT(IN) :: PSV     ! scalar variables
!                                             ! chemistry:   first char. in HSV: '#'  (molecule/m3)
!                                             !
 CHARACTER(LEN=6), DIMENSION(KSV),INTENT(IN):: HSV  ! name of all scalar variables
REAL, DIMENSION(KI), INTENT(IN)  :: PU        ! zonal wind                            (m/s)
REAL, DIMENSION(KI), INTENT(IN)  :: PV        ! meridian wind                         (m/s)
REAL, DIMENSION(KI,KSW),INTENT(IN) :: PDIR_SW ! direct  solar radiation (on horizontal surf.)
!                                             !                                       (W/m2)
REAL, DIMENSION(KI,KSW),INTENT(IN) :: PSCA_SW ! diffuse solar radiation (on horizontal surf.)
!                                             !                                       (W/m2)
REAL, DIMENSION(KSW),INTENT(IN)  :: PSW_BANDS ! mean wavelength of each shortwave band (m)
REAL, DIMENSION(KI), INTENT(IN)  :: PZENITH   ! zenithal angle at t  (radian from the vertical)
REAL, DIMENSION(KI), INTENT(IN)  :: PZENITH2  ! zenithal angle at t+1(radian from the vertical)
REAL, DIMENSION(KI), INTENT(IN)  :: PAZIM     ! azimuthal angle      (radian from North, clockwise)
REAL, DIMENSION(KI), INTENT(IN)  :: PLW       ! longwave radiation (on horizontal surf.)
!                                             !                                       (W/m2)
REAL, DIMENSION(KI), INTENT(IN)  :: PPS       ! pressure at atmospheric model surface (Pa)
REAL, DIMENSION(KI), INTENT(IN)  :: PPA       ! pressure at forcing level             (Pa)
REAL, DIMENSION(KI), INTENT(IN)  :: PZS       ! atmospheric model orography           (m)
REAL, DIMENSION(KI), INTENT(IN)  :: PCO2      ! CO2 concentration in the air          (kg/m3)
REAL, DIMENSION(KI), INTENT(IN)  :: PSNOW     ! snow precipitation                    (kg/m2/s)
REAL, DIMENSION(KI), INTENT(IN)  :: PRAIN     ! liquid precipitation                  (kg/m2/s)
!
!
REAL, DIMENSION(KI), INTENT(OUT) :: PSFTH     ! flux of heat                          (W/m2)
REAL, DIMENSION(KI), INTENT(OUT) :: PSFTQ     ! flux of water vapor                   (kg/m2/s)
REAL, DIMENSION(KI), INTENT(OUT) :: PSFU      ! zonal momentum flux                   (Pa)
REAL, DIMENSION(KI), INTENT(OUT) :: PSFV      ! meridian momentum flux                (Pa)
REAL, DIMENSION(KI), INTENT(OUT) :: PSFCO2    ! flux of CO2                           (kg/m2/s)
REAL, DIMENSION(KI,KSV),INTENT(OUT):: PSFTS   ! flux of scalar var.                   (kg/m2/s)
!
REAL, DIMENSION(KI), INTENT(OUT) :: PTRAD     ! radiative temperature                 (K)
REAL, DIMENSION(KI,KSW),INTENT(OUT):: PDIR_ALB! direct albedo for each spectral band  (-)
REAL, DIMENSION(KI,KSW),INTENT(OUT):: PSCA_ALB! diffuse albedo for each spectral band (-)
REAL, DIMENSION(KI), INTENT(OUT) :: PEMIS     ! emissivity                            (-)
!
REAL, DIMENSION(KI), INTENT(IN) :: PPEW_A_COEF! implicit coefficients
REAL, DIMENSION(KI), INTENT(IN) :: PPEW_B_COEF! needed if HCOUPLING='I'
REAL, DIMENSION(KI), INTENT(IN) :: PPET_A_COEF
REAL, DIMENSION(KI), INTENT(IN) :: PPEQ_A_COEF
REAL, DIMENSION(KI), INTENT(IN) :: PPET_B_COEF
REAL, DIMENSION(KI), INTENT(IN) :: PPEQ_B_COEF
 CHARACTER(LEN=2),    INTENT(IN) :: HTEST ! must be equal to 'OK'
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(KI)  :: ZTA    ! Temperature at forcing height above surface orography
REAL, DIMENSION(KI)  :: ZPA    ! Pressure    at forcing height above surface orography
REAL, DIMENSION(KI)  :: ZPS    ! Pressure    at surface orography
REAL, DIMENSION(KI)  :: ZQA    ! Humidity    at forcing height above surface orography
REAL, DIMENSION(KI)  :: ZRHOA  ! Density     at forcing height above surface orography
!
!
REAL, DIMENSION(KI)    :: Z3D_TOT_SURF ! ratio between actual surface
!                                               ! and horizontal surface
REAL, DIMENSION(KI)    :: Z3D_TOT_SURF_INV
REAL, DIMENSION(KI,KSW)::ZDIR_SW ! incoming direct SW radiation
!                                                         ! per m2 of actual surface
REAL, DIMENSION(KI,KSW)::ZSCA_SW ! incoming diffuse SW radiation
!                                                         ! per m2 of actual surface
REAL, DIMENSION(KI)      :: ZLW     ! incoming LW radiation per m2 of actual surface
!
REAL, DIMENSION(KI)    :: ZRAIN   ! liquid precipitation per m2 of actual surface
REAL, DIMENSION(KI)    :: ZSNOW   ! solid  precipitation per m2 of actual surface
!
REAL, DIMENSION(KI)  ::  ZPEQ_B_COEF   ! 1st explicit coefficient
REAL, DIMENSION(KI)  ::  ZPET_B_COEF   ! 2nd explicit coefficient
!
INTEGER                         :: ISWB    ! number of shortwave spectral bands
INTEGER                         :: JSWB    ! loop on shortwave spectral bands
INTEGER                         :: JSV     ! loop on scalar variables
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------
!        
!*      1.     Goes from atmospheric orography to surface orography
!              ----------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('COUPLING_ISBA_OROGRAPHY_N',0,ZHOOK_HANDLE)
!
ZPEQ_B_COEF = PPEQ_B_COEF
ZPET_B_COEF = PPET_B_COEF
!
IF(LVERTSHIFT)THEN
!        
  CALL FORCING_VERT_SHIFT(PZS,XZS,PTA,PQA,PPA,PRHOA,ZTA,ZQA,ZPA,ZRHOA)
!
  ZPS = ZPA + (PPS - PPA)
!
  IF (HCOUPLING=='I') THEN
    ZPEQ_B_COEF = PPEQ_B_COEF + ZQA - PQA
    ZPET_B_COEF = PPET_B_COEF + ZTA/(ZPA/XP00)**(XRD/XCPD) - PTA/(PPA/XP00)**(XRD/XCPD)
  ENDIF
!
ELSE
!
  ZTA     = PTA
  ZQA     = PQA
  ZPS     = PPS
  ZPA     = PPS
  ZRHOA   = PRHOA
!
ENDIF
!
!-------------------------------------------------------------------------------------
!
!*      2.     Presence of orography slopes
!              ----------------------------
!
!* Incoming and outgoing fluxes are supposed to be on a horizontal surface.
!  When slopes are present, the actual surface is LARGER than the
!  horizontal surface.
!
!* Therefore, this increase of surface will lead to modify the
!  radiative and energy fluxes.
!  
!* Note that momentum fluxes are not modified, because the
! effect of subgrid orography is already taken into account
! in the effective roughness length.
!
!
!
!*      2.     Estimation of total surface
!              ---------------------------
!
! The subgrid slope comes from the XSSO_SLOPE field.
!
IF(LNOSOF)THEN
   Z3D_TOT_SURF(:) = 1.
   Z3D_TOT_SURF_INV(:) = 1.
ELSE
   Z3D_TOT_SURF(:) = SQRT(1.+XSSO_SLOPE(:)**2)
   Z3D_TOT_SURF_INV(:) = 1./Z3D_TOT_SURF(:)
ENDIF
!
!
!-------------------------------------------------------------------------------------
!
!*      3.     Modification of the incoming radiation
!              --------------------------------------
!
! number of spectral shortwave bands
!
ISWB = SIZE(PSW_BANDS)
!
DO JSWB=1,ISWB
! correcting for the slope angle (scaterred SW flux)
!
  ZSCA_SW(:,JSWB) =  PSCA_SW(:,JSWB) * Z3D_TOT_SURF_INV(:)

! correcting for the slope angle (scaterred SW flux)
!
  ZDIR_SW(:,JSWB) =  PDIR_SW(:,JSWB) * Z3D_TOT_SURF_INV(:)
END DO
!
! part of LW flux is received from the surface itself, so the outgoing flux
! is needed.
!
! correction for LW flux.
!
ZLW(:) =  PLW(:)                                  *     Z3D_TOT_SURF_INV(:) &
          + XSTEFAN*XEMIS_NAT(:)*XTSRAD_NAT(:)**4 * (1.-Z3D_TOT_SURF_INV(:))  
!
!-------------------------------------------------------------------------------------
!
!*      4.     Modification of precipitation
!              -----------------------------
!
! correction for RAIN flux.
!
ZRAIN(:) = PRAIN(:) * Z3D_TOT_SURF_INV(:)
!
! correction for SNOW flux.
!
ZSNOW(:) = PSNOW(:) * Z3D_TOT_SURF_INV(:)
!
!-------------------------------------------------------------------------------------
!
!*      5.     Call of ISBA
!              ------------
!
 CALL COUPLING_ISBA_CANOPY_n(HPROGRAM, HCOUPLING,                                           &
               PTSTEP, KYEAR, KMONTH, KDAY, PTIME,                                           &
               KI, KSV, KSW,                                                                 &
               PTSUN, PZENITH, PZENITH2, PAZIM,                                              &
               PZREF, PUREF, PZS, PU, PV, ZQA, ZTA, ZRHOA, PSV, PCO2, HSV,                   &
               ZRAIN, ZSNOW, ZLW, ZDIR_SW, ZSCA_SW, PSW_BANDS, ZPS, ZPA,                     &
               PSFTQ, PSFTH, PSFTS, PSFCO2, PSFU, PSFV,                                      &
               PTRAD, PDIR_ALB, PSCA_ALB, PEMIS,                                             &
               PPEW_A_COEF, PPEW_B_COEF,                                                     &
               PPET_A_COEF, PPEQ_A_COEF, ZPET_B_COEF, ZPEQ_B_COEF,                           &
               'OK'                                                                          )  
!
!-------------------------------------------------------------------------------------
!
!*      6.     Modification of turbulent energy and gaz fluxes
!              -----------------------------------------------
!
PSFTH (:)  = PSFTH (:) * Z3D_TOT_SURF(:)
PSFTQ (:)  = PSFTQ (:) * Z3D_TOT_SURF(:)
PSFCO2(:)  = PSFCO2(:) * Z3D_TOT_SURF(:)
DO JSV=1,SIZE(PSFTS,2)
  PSFTS(:,JSV)  = PSFTS(:,JSV) * Z3D_TOT_SURF(:)
END DO
IF (LHOOK) CALL DR_HOOK('COUPLING_ISBA_OROGRAPHY_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE COUPLING_ISBA_OROGRAPHY_n
