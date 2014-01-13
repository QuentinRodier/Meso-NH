!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     ############################################################
SUBROUTINE COUPLING_IDEAL_FLUX(HPROGRAM, HCOUPLING, PTIMEC,                                  &
                 PTSTEP, KYEAR, KMONTH, KDAY, PTIME, KI, KSV, KSW, PTSUN, PZENITH, PAZIM,    &
                 PZREF, PUREF, PZS, PU, PV, PQA, PTA, PRHOA, PSV, PCO2, HSV,                 &
                 PRAIN, PSNOW, PLW, PDIR_SW, PSCA_SW, PSW_BANDS, PPS, PPA,                   &
                 PSFTQ, PSFTH, PSFTS, PSFCO2, PSFU, PSFV,                                    &
                 PTRAD, PDIR_ALB, PSCA_ALB, PEMIS,                                           &
                 PPEW_A_COEF, PPEW_B_COEF,                                                   &
                 PPET_A_COEF, PPEQ_A_COEF, PPET_B_COEF, PPEQ_B_COEF,                         &
                 HTEST                                                                       )  
!     ############################################################
!
!!****  *COUPLING_IDEAL_FLUX * - Computes the surface fluxes for the temperature, 
!!    vapor, horizontal components of the wind and the scalar variables.   
!!
!!    PURPOSE
!!    -------
!       Give prescribed values of the surface fluxes for the potential 
!     temperature, the vapor, the horizontal components of the wind and the 
!     scalar variables. These fluxes are unsteady when a diurnal cycle 
!     is taken into account.
!
!!**  METHOD
!!    ------
!!         A temporal interpolation is performed to recover the values of the 
!!    fluxes at every instant of the simulation. The different values of the
!!    prescribed fluxes are given at their declarations.
!!         For the wind, z0 can also be prescribed and the flux is determined 
!!    with a neutral drag coefficient.
!!
!!
!!    REFERENCE
!!    ---------
!!      
!!
!!    AUTHOR
!!    ------
!!     V. Masson 
!!     (from J. Cuxart and J. Stein)
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2004
!!      Modified    09/2012  : J. Escobar , SIZE(PTA) not allowed without-interface , replace by KI
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_CSTS,       ONLY : XRD, XCPD, XP00, XPI, XLVTT, XDAY
USE MODD_IDEAL_FLUX, ONLY : NFORCF, NFORCT, XSFTH, XSFTQ, XSFTS, XSFCO2, &
                            CUSTARTYPE, XUSTAR, XZ0, XALB, XEMIS, XTSRAD, &
                            XTIMEF, XTIMET 
USE MODD_SURF_PAR,   ONLY : XUNDEF
USE MODD_DIAG_IDEAL_n, ONLY : XH, XLE, XRN, XGFLUX, LSURF_BUDGET, &
                              LCOEF, XZ0_d=>XZ0, XZ0H_d=>XZ0H, &
                              LSURF_VARS, XQS
!
USE MODE_SBLS
USE MODE_THERMOS
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABOR1_SFX
!
IMPLICIT NONE
!
!*       0.1   declarations of arguments
! 
 CHARACTER(LEN=6),    INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
 CHARACTER(LEN=1),    INTENT(IN)  :: HCOUPLING ! type of coupling
                                              ! 'E' : explicit
                                              ! 'I' : implicit
REAL,                INTENT(IN)  :: PTIMEC    ! cumulated time since beginning of simulation
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
REAL, DIMENSION(KI), INTENT(IN)  :: PZENITH   ! zenithal angle       (radian from the vertical)
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
!
!*       0.2   declarations of local variables
!
REAL, DIMENSION(KI)  :: ZZ0     ! roughness length
REAL, DIMENSION(KI)  :: ZLMO    ! Monin-Obuhkov length
REAL, DIMENSION(KI)  :: ZTHA    ! air potential temperature
REAL, DIMENSION(KI)  :: ZRVA    ! water vapor mixing ratio
REAL, DIMENSION(KI)  :: ZUSTAR  ! friction velocity
REAL, DIMENSION(KI)  :: ZWIND   ! wind
REAL, DIMENSION(KI)  :: ZQ0     ! surface temperature flux (mK/s)
REAL, DIMENSION(KI)  :: ZE0     ! surface vapor flux (mkg/kg/s)
REAL, DIMENSION(KI)  :: ZQA     ! specific humidity (kg/kg)
!
REAL                        :: ZALPHA  ! interpolation coefficient
INTEGER                     :: IHOURF  ! number of hours since midnight
INTEGER                     :: IHOURT
INTEGER                     :: JITER   ! convergence loop counter
INTEGER                     :: JSV     ! loop on scalar variables
!
LOGICAL                     :: GCALL_LMO ! flag in non-neutral case
!
INTEGER                     :: ILUOUT  ! output listing logical unit
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('COUPLING_IDEAL_FLUX',0,ZHOOK_HANDLE)
IF (HTEST/='OK') THEN
  CALL ABOR1_SFX('COUPLING_IDEAL_FLUX: FATAL ERROR DURING ARGUMENT TRANSFER')
END IF
!----------------------------------------------------------------------------------
!
!*       2.    COMPUTE TIME
!              ------------
!
 CALL TEMP_FORC_DISTS (PTIMEC,NFORCF,XTIMEF,IHOURF,ZALPHA)
!
!----------------------------------------------------------------------------------
!
!*       3.    CONS. TEMPERATURE SURFACE FLUX
!              ------------------------------
!
PSFTH(:) = XSFTH(IHOURF) + ( XSFTH(IHOURF+1)-XSFTH(IHOURF) )*ZALPHA  
!
GCALL_LMO = ( XSFTH(IHOURF) + ( XSFTH(IHOURF+1)-XSFTH(IHOURF) )*ZALPHA ) /=0.
!----------------------------------------------------------------------------------
!
!*       4.    CONS. MIXING RATIO SURFACE FLUX
!              -------------------------------
!
PSFTQ(:) = XSFTQ(IHOURF) + ( XSFTQ(IHOURF+1)-XSFTQ(IHOURF) )*ZALPHA
!
GCALL_LMO = GCALL_LMO .OR. ( XSFTQ(IHOURF) + ( XSFTQ(IHOURF+1)-XSFTQ(IHOURF) )*ZALPHA ) /=0.
!----------------------------------------------------------------------------------
!
!*       5.    WIND SURFACE FLUX
!              -----------------
!
!*       5.1   wind
!
ZWIND(:) = SQRT(PU**2+PV**2)
!
!*       5.2   Compute the surface stresses
!
SELECT CASE (CUSTARTYPE)
!
!
  CASE ('USTAR')
    !  when u* is prescribed
    ZUSTAR(:) = XUSTAR(IHOURF) + ( XUSTAR(IHOURF+1)-XUSTAR(IHOURF) )*ZALPHA
    ! 
  CASE ('Z0   ')
    !
    !* spatialized roughness length
    !
    ZZ0(:) = XZ0
    !
    !* water mixing ratio
    !
    ZRVA(:) = 0.
    ZQA(:)  = PQA(:) / PRHOA(:)
    !
    WHERE (ZQA(:)/=0.) ZRVA(:) = 1./(1./ZQA(:) - 1.)
    !
    !* air potential temperature
    ZTHA(:) = PTA(:) * (XP00/PPA(:))**(XRD/XCPD)
    !
    !* cinematic surface fluxes
    ZQ0(:) = PSFTH(:) / XCPD / PRHOA(:)
    ZE0(:) = PSFTQ(:)        / PRHOA(:)
    !
    !
    !* neutral case, as guess
    ZLMO  (:) = XUNDEF
    ZUSTAR(:) = USTAR(ZWIND(:),PZREF(:),ZZ0(:),ZLMO(:))
    !
    !* iterations in non-neutral case
    IF (GCALL_LMO) THEN
      ZUSTAR(:) = MAX ( ZUSTAR(:), 0.01 )
      DO JITER=1,10
        ZLMO  (:) = LMO  (ZUSTAR(:),ZTHA(:),ZRVA(:),ZQ0(:),ZE0(:))
        ZUSTAR(:) = USTAR(ZWIND(:),PZREF(:),ZZ0(:),ZLMO(:))
      END DO
    END IF
    !
    !
END SELECT
!
PSFU = 0.
PSFV = 0.
WHERE (ZWIND>0.)
  PSFU = - PRHOA * ZUSTAR**2 * PU / ZWIND
  PSFV = - PRHOA * ZUSTAR**2 * PV / ZWIND
END WHERE
!
!-------------------------------------------------------------------------------
!  
!*       6.    SCALAR VARIABLES FLUXES.
!              -----------------------
!
DO JSV=1,SIZE(PSFTS,2)
  PSFTS(:,JSV) = XSFTS(IHOURF,JSV) + ( XSFTS(IHOURF+1,JSV)-XSFTS(IHOURF,JSV) )*ZALPHA
END DO
!
!-------------------------------------------------------------------------------
!  
!*       7.    CO2 FLUXES.
!              ----------
!
PSFCO2(:) = XSFCO2(IHOURF) + ( XSFCO2(IHOURF+1)-XSFCO2(IHOURF) )*ZALPHA
!
!-------------------------------------------------------------------------------
!  
!*       8.    OTHER OUTPUTS (RADIATIVE QUANTITIES) SET TO A CONSTANT VALUE
!              ------------------------------------
!
 CALL TEMP_FORC_DISTS (PTIMEC,NFORCT,XTIMET,IHOURT,ZALPHA)
!
PTRAD(:) = XTSRAD(IHOURT) + ( XTSRAD(IHOURT+1)-XTSRAD(IHOURT) )*ZALPHA
!
PDIR_ALB = XALB
PSCA_ALB = XALB
PEMIS    = XEMIS
!
!-------------------------------------------------------------------------------
!  
!*       9.    turbulent fluxes as diagnostics
!              ------------------------------------
IF (LSURF_BUDGET) THEN
  XH  = PSFTH
  XLE = XLVTT * PSFTQ
  XRN = XH+XLE
  XGFLUX = 0.
ENDIF
!
IF (LCOEF) THEN
  XZ0_d  = XZ0
  XZ0H_d = XZ0
ENDIF
!
IF (LSURF_VARS) THEN
  XQS(:) = QSAT(PTRAD(:),PPS(:))
ENDIF
!
IF (LHOOK) CALL DR_HOOK('COUPLING_IDEAL_FLUX',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
CONTAINS
!
SUBROUTINE TEMP_FORC_DISTS (PTIMEIN,KFORC,PTIMES,KHOUR,PALPHA)
!
IMPLICIT NONE
!
REAL, INTENT(IN) :: PTIMEIN
INTEGER, INTENT(IN) :: KFORC
REAL, DIMENSION(:), INTENT(IN) :: PTIMES
INTEGER, INTENT(OUT):: KHOUR
REAL, INTENT(OUT):: PALPHA
!
INTEGER :: JT
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('COUPLING_IDEAL_FLUX:TEMP_FORC_DISTS',0,ZHOOK_HANDLE)
!
IF (PTIMES(KFORC)==XUNDEF) THEN
  KHOUR = 1
  PALPHA = 0.
ELSEIF (PTIMEIN<PTIMES(1).OR.PTIMEIN>PTIMES(KFORC)) THEN
  CALL ABOR1_SFX("COUPLING_IDEAL_FLUX:TEMP_FORC_DISTS: PTIMEC OUT OF BOUNDS!!!")
ELSE
 DO JT = KFORC,1,-1
   IF (PTIMEIN.GE.PTIMES(JT)) THEN
     KHOUR = JT
     EXIT
   ENDIF
  ENDDO      
  PALPHA = (PTIMEIN-PTIMES(KHOUR)) / (PTIMES(KHOUR+1)-PTIMES(KHOUR))
ENDIF
!
IF (LHOOK) CALL DR_HOOK('COUPLING_IDEAL_FLUX:TEMP_FORC_DISTS',1,ZHOOK_HANDLE)
!
END SUBROUTINE TEMP_FORC_DISTS
!
END SUBROUTINE COUPLING_IDEAL_FLUX
