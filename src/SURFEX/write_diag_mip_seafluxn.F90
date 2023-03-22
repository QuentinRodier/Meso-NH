!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE WRITE_DIAG_MIP_SEAFLUX_n (DTCO, DUO, U, S, D, DGMSI, HPROGRAM)
!     #################################
!
!!    Writes the seaflux diagnostic fields as specified by cmip
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
USE MODD_DATA_COVER_n,       ONLY : DATA_COVER_t
USE MODD_SURF_ATM_n,         ONLY : SURF_ATM_t
USE MODD_SEAFLUX_n,          ONLY : SEAFLUX_t
USE MODD_DIAG_n,             ONLY : DIAG_t, DIAG_OPTIONS_t
USE MODD_DIAG_MISC_SEAICE_n, ONLY : DIAG_MISC_SEAICE_t
!
USE MODD_SFX_OASIS,     ONLY : LCPL_SEAICE
USE MODD_SURF_PAR,      ONLY : XUNDEF
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
TYPE(DATA_COVER_t),       INTENT(INOUT) :: DTCO
TYPE(DIAG_OPTIONS_t),     INTENT(INOUT) :: DUO
TYPE(SURF_ATM_t),         INTENT(INOUT) :: U
TYPE(SEAFLUX_t),          INTENT(INOUT) :: S
TYPE(DIAG_t),             INTENT(INOUT) :: D
TYPE(DIAG_MISC_SEAICE_t), INTENT(INOUT) :: DGMSI
!
CHARACTER(LEN=6),         INTENT(IN)    :: HPROGRAM ! program calling
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER           :: IRESP          ! IRESP  : return-code if a problem appears
CHARACTER(LEN=12) :: YRECFM         ! Name of the article to be read
CHARACTER(LEN=100):: YCOMMENT       ! Comment string
CHARACTER(LEN=2)  :: YNUM
!
REAL, DIMENSION(SIZE(D%XTS))  :: ZWORK, ZPATCH
!
REAL(KIND=JPRB)   :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('WRITE_DIAG_MIP_SEAFLUX_N',0,ZHOOK_HANDLE)
!
!         Initialisation for IO
!
CALL INIT_IO_SURF_n(DTCO, U, HPROGRAM,'SEA   ','SEAFLX','WRITE','SEAFLUX_DIAGNOSTICS.OUT.nc')
!
!-------------------------------------------------------------------------------
! * Near surface atmospheric variables
!-------------------------------------------------------------------------------
!
YRECFM='tas_sea'
YCOMMENT='near-surface air temperature at 2m over sea (K)'
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,D%XT2M(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='huss_sea'
YCOMMENT='near-surface specific humidity at 2m over sea (kg/kg)'
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,D%XQ2M(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='hurs_sea'
YCOMMENT='near-surface relative humidity at 2m over sea (-)'
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,D%XHU2M(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='sfcWind_sea'
YCOMMENT='near-surface near surface at 10m over sea (m/s)'
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,D%XWIND10M(:),IRESP,HCOMMENT=YCOMMENT)
!
!-------------------------------------------------------------------------------
! * Radiative fluxes 
!-------------------------------------------------------------------------------
!
! * Downward fluxes 
!
YRECFM='rsds_sea'
YCOMMENT='short wave downward radiation over sea (W/m2)'
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,D%XSWD(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='rlds_sea'
YCOMMENT='long wave downward radiation over sea (W/m2)'
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,D%XLWD(:),IRESP,HCOMMENT=YCOMMENT)
!
! * Upward fluxes
!
YRECFM='rsus_sea'
YCOMMENT='short wave upward radiation over sea (W/m2)'
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,D%XSWU(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='rlus_sea'
YCOMMENT='long wave upward radiation over sea (W/m2)'
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,D%XLWU(:),IRESP,HCOMMENT=YCOMMENT)
!
! * Net fluxes
!
YRECFM='rss_sea'
YCOMMENT='net short wave radiation over sea (W/m2)'
ZWORK(:)=(D%XSWD(:)-D%XSWU(:))
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='rls_sea'
YCOMMENT='net long wave radiation over sea (W/m2)'
ZWORK(:)=(D%XLWD(:)-D%XLWU(:))
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
!
!-------------------------------------------------------------------------------
! * Energy fluxes 
!-------------------------------------------------------------------------------
!
YRECFM='hfls_sea'
YCOMMENT='latent heat flux over sea (W/m2)'
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,D%XLE(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='hfss_sea'
YCOMMENT='sensible heat flux over sea (W/m2)'
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,D%XH(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='hfsbl_sea'
YCOMMENT='energy of sublimation (solid to vapor) over sea (W/m2)'
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,D%XLEI(:),IRESP,HCOMMENT=YCOMMENT)
!
!-------------------------------------------------------------------------------
! * Momentum flux 
!-------------------------------------------------------------------------------
!
YRECFM='tau_sea'
YCOMMENT='wind stress over sea (Pa)'
ZWORK(:)=SQRT(D%XFMU(:)*D%XFMU(:)+D%XFMV(:)*D%XFMV(:))
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
!
!-------------------------------------------------------------------------------
! * Surface Temperature
!-------------------------------------------------------------------------------
!
! * Sea surface Temperature
!
YRECFM='ts_sea'
YCOMMENT='surface temperature over sea (K)'
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,D%XTS(:),IRESP,HCOMMENT=YCOMMENT)
!       
! * radiative surface temperature 
!
YRECFM='tr_sea'
YCOMMENT='radiative surface temperature over sea including seaice (K)'
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,D%XTSRAD(:),IRESP,HCOMMENT=YCOMMENT)

!
! * Sea-ice surface Temperature
!
YRECFM='sitemptop'
YCOMMENT='sea-ice surface temperature over sea (K)'
IF(S%LHANDLE_SIC)THEN
  WHERE(S%XSIC(:)>0.0)
    ZWORK(:) = S%XTICE(:)
  ELSEWHERE
    ZWORK(:) = XUNDEF
  ENDWHERE
ELSEIF(LCPL_SEAICE)THEN
  WHERE(S%XSIC(:)==1.0)
    ZWORK(:) = S%XSST(:)
  ELSEWHERE
    ZWORK(:) = XUNDEF
  ENDWHERE
ENDIF 
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
!
!-------------------------------------------------------------------------------
! * Albedo
!-------------------------------------------------------------------------------
!
! * total albedo
!
YRECFM='albsrfc_sea'
YCOMMENT='surface albedo over sea (-)'
WHERE(D%XSWD(:)>0.0)
  ZWORK(:) = D%XSWU(:)/D%XSWD(:)
ELSEWHERE
  ZWORK(:) = XUNDEF
ENDWHERE
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
!
IF (S%LHANDLE_SIC) THEN
  YRECFM='sialb'
  YCOMMENT='sea-ice albedo including snow over sea (-)'
  WHERE(D%XSWD(:)>0.0.AND.S%XSIC(:)>0.0)
    ZWORK(:) = S%XICE_ALB(:)
  ELSEWHERE
    ZWORK(:) = XUNDEF
  ENDWHERE
  CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)     
ENDIF
!
!-------------------------------------------------------------------------------
! * Total Evaporation fluxes
!-------------------------------------------------------------------------------
!
YRECFM='evspsbl_sea'
YCOMMENT='water evaporation flux including sublimation over sea (kg/m2/s)'
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,D%XEVAP(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='sbl_sea'
YCOMMENT='surface snow and ice sublimation flux over sea (kg/m2/s)'
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,D%XSUBL(:),IRESP,HCOMMENT=YCOMMENT)
!
!-------------------------------------------------------------------------------
! * Sea-ice cover
!-------------------------------------------------------------------------------
!
YRECFM='sic'
YCOMMENT='sea-ice cover fraction over sea (m)'
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,S%XSIC(:),IRESP,HCOMMENT=YCOMMENT)
!
!-------------------------------------------------------------------------------
! * Sea-ice properties
!-------------------------------------------------------------------------------
!
IF (DGMSI%LDIAG_MISC_SEAICE.AND.TRIM(S%CSEAICE_SCHEME)=='GELATO') THEN
!
  YRECFM='sithick'
  YCOMMENT='sea-ice thickness over sea (m)'
  WHERE(S%XSIC(:)>0.0)
    ZWORK(:) = DGMSI%XSIT(:)
  ELSEWHERE
    ZWORK(:) = XUNDEF
  ENDWHERE
  CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
!
  YRECFM='sisnthick'
  YCOMMENT='sea-ice snow depth over sea (m)'
  WHERE(S%XSIC(:)>0.0)
    ZWORK(:) = DGMSI%XSND(:)
  ELSEWHERE
    ZWORK(:) = XUNDEF
  ENDWHERE
  CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
!
ENDIF
!
!-------------------------------------------------------------------------------
!
!         End of IO
!
 CALL END_IO_SURF_n(HPROGRAM)
!
IF (LHOOK) CALL DR_HOOK('WRITE_DIAG_MIP_SEAFLUX_N',1,ZHOOK_HANDLE)
!
END SUBROUTINE WRITE_DIAG_MIP_SEAFLUX_n
