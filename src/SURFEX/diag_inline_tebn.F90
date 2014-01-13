!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
       SUBROUTINE DIAG_INLINE_TEB_n (OCANOPY, PTA, PTS, PQA, PPA, PPS, PRHOA,                  &
                                       PZONA, PMERA, PWIND, PHT, PHW,                          &
                                       PCD, PCDN, PRI, PCH, PZ0,                               &
                                       PTRAD, PEMIS, PDIR_ALB, PSCA_ALB,                       &
                                       PLW, PDIR_SW, PSCA_SW,                                  &
                                       PSFTH, PSFTQ, PSFZON, PSFMER, PSFCO2,                   &
                                       PRN, PH, PLE, PGFLUX                                    )  
!     ###############################################################################!
!!****  *DIAG_INLINE_TEB_n * - Computes diagnostics during TEB time-step
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
!!      S. Riette   06/2009 CLS_WIND has one more argument (height of diagnostic)
!!      S. Riette   01/2010 Use of interpol_sbl to compute 10m wind diagnostic
!!------------------------------------------------------------------
!

!
!
USE MODD_SURF_PAR,   ONLY : XUNDEF
USE MODD_TEB_n,      ONLY : XT_CANYON, XQ_CANYON
USE MODD_TEB_CANOPY_n, ONLY : XZ, XU, XP, XT, XQ
USE MODD_DIAG_TEB_n, ONLY : N2M, LSURF_BUDGET, LCOEF, LSURF_VARS, &
                              XT2M, XQ2M, XHU2M, XZON10M, XMER10M,  &
                              XRN, XH, XLE, XGFLUX, XRI, XCD, XCH,  &
                              XCE, XZ0, XZ0H, XQS, XSWD, XSWU, XLWD,&
                              XLWU, XSWBD, XSWBU, XFMU, XFMV, XSFCO2
!
USE MODI_CLS_WIND
USE MODI_PARAM_CLS
USE MODI_DIAG_SURF_BUDGET_TEB
USE MODI_INTERPOL_SBL
!
USE MODE_THERMOS
USE MODE_COUPLING_CANOPY
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
LOGICAL,            INTENT(IN)       :: OCANOPY  ! Flag for canopy
REAL, DIMENSION(:), INTENT(IN)       :: PTA      ! atmospheric temperature
REAL, DIMENSION(:), INTENT(IN)       :: PTS      ! surface temperature
REAL, DIMENSION(:), INTENT(IN)       :: PQA      ! atmospheric specific humidity
REAL, DIMENSION(:), INTENT(IN)       :: PPA      ! atmospheric level pressure
REAL, DIMENSION(:), INTENT(IN)       :: PPS      ! surface pressure
REAL, DIMENSION(:), INTENT(IN)       :: PRHOA    ! air density
REAL, DIMENSION(:), INTENT(IN)       :: PZONA    ! zonal wind
REAL, DIMENSION(:), INTENT(IN)       :: PMERA    ! meridian wind
REAL, DIMENSION(:), INTENT(IN)       :: PWIND    ! wind
REAL, DIMENSION(:), INTENT(IN)       :: PHT      ! atmospheric level height
REAL, DIMENSION(:), INTENT(IN)       :: PHW      ! atmospheric level height for wind
REAL, DIMENSION(:), INTENT(IN)       :: PCD      ! drag coefficient for momentum
REAL, DIMENSION(:), INTENT(IN)       :: PCDN     ! neutral drag coefficient
REAL, DIMENSION(:), INTENT(IN)       :: PSFZON   ! zonal friction
REAL, DIMENSION(:), INTENT(IN)       :: PSFMER   ! meridian friction
REAL, DIMENSION(:), INTENT(IN)       :: PSFCO2   ! CO2 flux
REAL, DIMENSION(:), INTENT(IN)       :: PSFTH    ! heat flux  (W/m2)
REAL, DIMENSION(:), INTENT(IN)       :: PSFTQ    ! water flux (kg/m2)
REAL, DIMENSION(:), INTENT(IN)       :: PRI      ! Richardson number
REAL, DIMENSION(:), INTENT(IN)       :: PCH      ! drag coefficient for heat
REAL, DIMENSION(:), INTENT(IN)       :: PZ0      ! roughness length for momentum
REAL, DIMENSION(:), INTENT(IN)       :: PRN      ! net radiation
REAL, DIMENSION(:), INTENT(IN)       :: PH       ! sensible heat flux
REAL, DIMENSION(:), INTENT(IN)       :: PLE      ! latent heat flux
REAL, DIMENSION(:), INTENT(IN)       :: PGFLUX   ! storage flux
REAL, DIMENSION(:,:),INTENT(IN)      :: PDIR_SW  ! direct  solar radiation (on horizontal surf.)
!                                                !                                      (W/m2)
REAL, DIMENSION(:,:),INTENT(IN)      :: PSCA_SW  ! diffuse solar radiation (on horizontal surf.)
!                                                !                                      (W/m2)
REAL, DIMENSION(:), INTENT(IN)       :: PLW      ! longwave radiation (on horizontal surf.)
REAL, DIMENSION(:), INTENT(IN)       :: PTRAD    ! radiative temperature                 (K)
REAL, DIMENSION(:,:),INTENT(IN)      :: PDIR_ALB ! direct albedo for each spectral band  (-)
REAL, DIMENSION(:,:),INTENT(IN)      :: PSCA_ALB ! diffuse albedo for each spectral band (-)
REAL, DIMENSION(:), INTENT(IN)       :: PEMIS    ! emissivity                            (-)
!
!*      0.2    declarations of local variables
!
REAL                                 :: ZZ0_O_Z0H
REAL, DIMENSION(SIZE(PTA))           :: ZH  
REAL, DIMENSION(SIZE(PTA))  :: ZU10
REAL, DIMENSION(SIZE(PTA))  :: ZWIND10M_MAX
REAL, DIMENSION(SIZE(PTA))  :: ZT2M_MIN
REAL, DIMENSION(SIZE(PTA))  :: ZT2M_MAX
REAL, DIMENSION(SIZE(PTA))  :: ZHU2M_MIN
REAL, DIMENSION(SIZE(PTA))  :: ZHU2M_MAX
INTEGER                              :: JJ    ! loop counter

REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('DIAG_INLINE_TEB_N',0,ZHOOK_HANDLE)
ZZ0_O_Z0H = 200.
!
!* 2m and 10m variables interpolated from canopy if used
!
IF (OCANOPY) THEN
  ZT2M_MIN(:) = XUNDEF
  ZT2M_MAX(:) = XUNDEF
  ZHU2M_MIN(:) = XUNDEF
  ZHU2M_MAX(:) = XUNDEF
  ZWIND10M_MAX(:) = XUNDEF
  IF (N2M>0) CALL INIT_2M_10M( XP(:,2), XT(:,2), XQ(:,2),  XU, XZ, &
                               PZONA, PMERA, PWIND, PRHOA,         &
                               XT2M, XQ2M, XHU2M, XZON10M, XMER10M,&
                               ZU10, ZWIND10M_MAX, ZT2M_MIN,       &
                               ZT2M_MAX, ZHU2M_MIN, ZHU2M_MAX )
ELSE
!* 2m and 10m variables using CLS laws
 IF (N2M==1) THEN
  CALL PARAM_CLS(PTA, PTS, PQA, PPA, PRHOA, PZONA, PMERA, PHT, PHW, &
                   PSFTH, PSFTQ, PSFZON, PSFMER,                       &
                   XT2M, XQ2M, XHU2M, XZON10M, XMER10M                )  
  !
  !* erases temperature and humidity 2m above roof level bu canyon air values
  !
  XT2M  = XT_CANYON
  XQ2M  = XQ_CANYON
  !
  !* Richardson number
  !
  XRI = PRI
  XHU2M = MIN(XQ_CANYON /QSAT(XT_CANYON,PPA),1.)
 ELSE IF (N2M==2) THEN
  ZH(:)=10.
  CALL CLS_WIND(PZONA, PMERA, PHW,  &
                  PCD, PCDN, PRI, ZH, &
                  XZON10M, XMER10M    )  
  XT2M  = XT_CANYON
  XQ2M  = XQ_CANYON
  XRI   = PRI
  XHU2M = MIN(XQ_CANYON /QSAT(XT_CANYON,PPA),1.)
 END IF
END IF
!
!
IF (LSURF_BUDGET) THEN
   !
   CALL DIAG_SURF_BUDGET_TEB(PDIR_SW, PSCA_SW, PDIR_ALB, PSCA_ALB,  &
                               PLW, PEMIS, PTRAD,                     &
                               XSWD, XSWU, XSWBD, XSWBU, XLWD, XLWU   )  
   !                             
   XRN    = PRN
   XH     = PH
   XLE    = PLE
   XGFLUX = PGFLUX
   XFMU   = PSFZON
   XFMV   = PSFMER
   XSFCO2 = PSFCO2
   !
END IF
!
IF (LCOEF) THEN
  XCD    = PCD
  XCH    = PCH
  XCE    = PCH
  XZ0    = PZ0
  XZ0H   = PZ0 / ZZ0_O_Z0H
END IF
!
IF (LSURF_VARS) THEN
  XQS    = XQ_CANYON
END IF
IF (LHOOK) CALL DR_HOOK('DIAG_INLINE_TEB_N',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------------
!
END SUBROUTINE DIAG_INLINE_TEB_n
