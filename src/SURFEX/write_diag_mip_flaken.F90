!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE WRITE_DIAG_MIP_FLAKE_n (DTCO, DUO, U, F, D, HPROGRAM)
!     #################################
!
!!    Writes the Flake diagnostic fields as specified by cmip
!!
!!    PURPOSE
!!    -------
!!
!!
!!**  METHOD
!!    ------
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
!!      Original    01/2017
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_SURF_ATM_n,   ONLY : SURF_ATM_t
USE MODD_FLAKE_n,      ONLY : FLAKE_t
USE MODD_DIAG_n,       ONLY : DIAG_t, DIAG_OPTIONS_t
!
USE MODD_SURF_PAR,      ONLY : XUNDEF
!
USE MODI_INIT_IO_SURF_n
USE MODI_WRITE_SURF
USE MODI_END_IO_SURF_n
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
!
!
TYPE(DATA_COVER_t),   INTENT(INOUT) :: DTCO
TYPE(DIAG_OPTIONS_t), INTENT(INOUT) :: DUO
TYPE(SURF_ATM_t),     INTENT(INOUT) :: U
TYPE(FLAKE_t),        INTENT(INOUT) :: F
TYPE(DIAG_t),         INTENT(INOUT) :: D
!
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! program calling
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER           :: IRESP          ! IRESP  : return-code if a problem appears
CHARACTER(LEN=12) :: YRECFM         ! Name of the article to be read
CHARACTER(LEN=100):: YCOMMENT       ! Comment string
CHARACTER(LEN=2)  :: YNUM
!
REAL, DIMENSION(SIZE(F%XTS(:)))  :: ZWORK, ZPATCH
!
REAL(KIND=JPRB)   :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('WRITE_DIAG_MIP_FLAKE_N',0,ZHOOK_HANDLE)
!
!         Initialisation for IO
!
CALL INIT_IO_SURF_n(DTCO, U, HPROGRAM,'WATER ','FLAKE ','WRITE','FLAKE_DIAGNOSTICS.OUT.nc')
!
!-------------------------------------------------------------------------------
! * Near surface atmospheric variables
!-------------------------------------------------------------------------------
!
YRECFM='tasLa'
YCOMMENT='near-surface air temperature at 2m over lake (K)'
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,D%XT2M(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='hussLa'
YCOMMENT='near-surface specific humidity at 2m over lake (kg/kg)'
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,D%XQ2M(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='hursLa'
YCOMMENT='near-surface relative humidity at 2m over lake (-)'
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,D%XHU2M(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='sfcWindLa'
YCOMMENT='near-surface near surface at 10m over lake (m/s)'
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,D%XWIND10M(:),IRESP,HCOMMENT=YCOMMENT)
!
!-------------------------------------------------------------------------------
! * Radiative fluxes 
!-------------------------------------------------------------------------------
!
! * Downward fluxes 
!
YRECFM='rsdsLa'
YCOMMENT='short wave downward radiation over lake (W/m2)'
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,D%XSWD(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='rldsLa'
YCOMMENT='long wave downward radiation over lake (W/m2)'
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,D%XLWD(:),IRESP,HCOMMENT=YCOMMENT)
!
! * Upward fluxes
!
YRECFM='rsusLa'
YCOMMENT='short wave upward radiation over lake (W/m2)'
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,D%XSWU(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='rlusLa'
YCOMMENT='long wave upward radiation over lake (W/m2)'
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,D%XLWU(:),IRESP,HCOMMENT=YCOMMENT)
!
! * Net fluxes
!
YRECFM='rssLa'
YCOMMENT='net short wave radiation over lake (W/m2)'
ZWORK(:)=D%XSWD(:)-D%XSWU(:)
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='rlsLa'
YCOMMENT='net long wave radiation over lake (W/m2)'
ZWORK(:)=D%XLWD(:)-D%XLWU(:)
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
!
!-------------------------------------------------------------------------------
! * Energy fluxes
!-------------------------------------------------------------------------------
!
YRECFM='hflsLa'
YCOMMENT='latent heat flux over lake (W/m2)'
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,D%XLE(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='hfssLa'
YCOMMENT='sensible heat flux over lake (W/m2)'
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,D%XH(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='hfsblLa'
YCOMMENT='energy of sublimation (solid to vapor) over lake (W/m2)'
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,D%XLEI(:),IRESP,HCOMMENT=YCOMMENT)
!
!-------------------------------------------------------------------------------
! * Momentum flux 
!-------------------------------------------------------------------------------
!
YRECFM='tauLa'
YCOMMENT='wind stress over lake (Pa)'
ZWORK(:)=SQRT(D%XFMU(:)*D%XFMU(:)+D%XFMV(:)*D%XFMV(:))
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
!
!-------------------------------------------------------------------------------
! * Lake Temperature (K)
!-------------------------------------------------------------------------------
!  
! * surface temperature
!
YRECFM='tsLa'
YCOMMENT='surface temperature over lake (K)'
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,F%XTS(:),IRESP,HCOMMENT=YCOMMENT)
!
! * Temperature at the air-snow interface [K]
!
YRECFM='tsnLa'
YCOMMENT='Temperature at the air-snow interface over lake (K)'
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,F%XT_SNOW(:),IRESP,HCOMMENT=YCOMMENT)
!
! * Temperature at the air-snow interface [K]
!
YRECFM='ticeLa'
YCOMMENT='Temperature at the snow-ice or air-ice interface over lake (K)'
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,F%XT_ICE(:),IRESP,HCOMMENT=YCOMMENT)
!
! * mixed-layer temperature [K]
!
YRECFM='tmxlLa'
YCOMMENT='mixed-layer temperature over lake (K)'
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,F%XT_WML(:),IRESP,HCOMMENT=YCOMMENT)
!
! * Temperature at the water-bottom sediment [K]
!
YRECFM='tbottomLa'
YCOMMENT='Temperature at the water-bottom sediment over lake (K)'
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,F%XT_BOT(:),IRESP,HCOMMENT=YCOMMENT)
!
! * mean lake temperature [K]
!
YRECFM='tmeanLa'
YCOMMENT='mean temperature over lake (K)'
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,F%XT_WML(:),IRESP,HCOMMENT=YCOMMENT)
!
!-------------------------------------------------------------------------------
! * Albedo
!-------------------------------------------------------------------------------
!
YRECFM='albsrfcLa'
YCOMMENT='surface albedo over lake (-)'
WHERE(D%XSWD(:)>0.0)
  ZWORK(:) = D%XSWU(:)/D%XSWD(:)
ELSEWHERE
  ZWORK(:) = XUNDEF
ENDWHERE
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
!
!-------------------------------------------------------------------------------
! * Total Evaporation fluxes
!-------------------------------------------------------------------------------
!
YRECFM='evspsblLa'
YCOMMENT='water evaporation flux including sublimation over lake (kg/m2/s)'
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,D%XEVAP(:),IRESP,HCOMMENT=YCOMMENT)
!  
YRECFM='sblLa'
YCOMMENT='surface snow and ice sublimation flux over lake (kg/m2/s)'
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,D%XSUBL(:),IRESP,HCOMMENT=YCOMMENT)
!
!-------------------------------------------------------------------------------
! * Snow, ice and mixed layer depth
!-------------------------------------------------------------------------------
!
YRECFM='sndLa'
YCOMMENT='surface snow thickness over lake (m)'
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,F%XH_SNOW(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='icedLa'
YCOMMENT='ice thickness over lake (m)'
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,F%XH_ICE(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='mxldLa'
YCOMMENT='mixed-layer thickness over lake (m)'
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,F%XH_ML(:),IRESP,HCOMMENT=YCOMMENT)
!
!-------------------------------------------------------------------------------
!
!         End of IO
!
 CALL END_IO_SURF_n(HPROGRAM)
!
IF (LHOOK) CALL DR_HOOK('WRITE_DIAG_MIP_FLAKE_N',1,ZHOOK_HANDLE)
!
!
END SUBROUTINE WRITE_DIAG_MIP_FLAKE_n
