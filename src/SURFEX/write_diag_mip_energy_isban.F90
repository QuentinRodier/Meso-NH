!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE WRITE_DIAG_MIP_ENERGY_ISBA_n (DTCO, DUO, U, ID, IO, S, NP, NPE, HPROGRAM)
!     #################################
!
!!****  *WRITE_DIAG_MIP_ENERGY_ISBA*
!!
!!    PURPOSE
!!    -------
!!
!!    Writes the ISBA diagnostic fields relative to energy cycle as specified by cmip
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
!!      Original    12/2016
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_DATA_COVER_n,   ONLY : DATA_COVER_t
USE MODD_DIAG_n,         ONLY : DIAG_OPTIONS_t
USE MODD_SURF_ATM_n,     ONLY : SURF_ATM_t
USE MODD_SURFEX_n,       ONLY : ISBA_DIAG_t
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
USE MODD_ISBA_n,         ONLY : ISBA_NP_t, ISBA_P_t, ISBA_NPE_t, ISBA_PE_t, ISBA_S_t
!
USE MODD_XIOS, ONLY : LALLOW_ADD_DIM, YSNOW_LAYER_DIM_NAME, YTG_LAYER_DIM_NAME
!
USE MODD_SURF_PAR,   ONLY : XUNDEF
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
!
TYPE(DATA_COVER_t),   INTENT(INOUT) :: DTCO
TYPE(DIAG_OPTIONS_t), INTENT(INOUT) :: DUO
TYPE(SURF_ATM_t),     INTENT(INOUT) :: U
TYPE(ISBA_DIAG_t),    INTENT(INOUT) :: ID
TYPE(ISBA_OPTIONS_t), INTENT(INOUT) :: IO
TYPE(ISBA_S_t),       INTENT(INOUT) :: S
TYPE(ISBA_NP_t),      INTENT(INOUT) :: NP
TYPE(ISBA_NPE_t),     INTENT(INOUT) :: NPE
!
CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! program calling
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
TYPE(ISBA_P_t),  POINTER :: PK
TYPE(ISBA_PE_t), POINTER :: PEK
!
!
INTEGER           :: IRESP          ! IRESP  : return-code if a problem appears
CHARACTER(LEN=12) :: YRECFM         ! Name of the article to be write
CHARACTER(LEN=100):: YCOMMENT       ! Comment string
CHARACTER(LEN=2)  :: YNUM
CHARACTER(LEN=4 ) :: YLVL
!
REAL, DIMENSION(U%NSIZE_NATURE)                :: ZWORK
REAL, DIMENSION(U%NSIZE_NATURE)                :: ZPATCH
REAL, DIMENSION(U%NSIZE_NATURE)                :: ZTV
REAL, DIMENSION(U%NSIZE_NATURE)                :: ZTL
REAL, DIMENSION(U%NSIZE_NATURE)                :: ZTC
REAL, DIMENSION(U%NSIZE_NATURE)                :: ZQC
!
REAL, DIMENSION(U%NSIZE_NATURE,SIZE(NPE%AL(1)%XTG,2))  :: ZTG
REAL, DIMENSION(U%NSIZE_NATURE,NPE%AL(1)%TSNOW%NLAYER) :: ZTSN
REAL, DIMENSION(U%NSIZE_NATURE,NPE%AL(1)%TSNOW%NLAYER) :: ZCSN
!
INTEGER           :: JI, JL, JP, INL, INLS, INP, IWORK, IMASK
INTEGER           :: ISIZE_LMEB_PATCH   ! Number of patches where multi-energy balance should be applied
!
REAL(KIND=JPRB)   :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('WRITE_DIAG_MIP_ENERGY_ISBA_N',0,ZHOOK_HANDLE)
!
!         Initialisation for IO
!
INL  = IO%NGROUND_LAYER
INLS = NPE%AL(1)%TSNOW%NLAYER
INP  = IO%NPATCH
!
ISIZE_LMEB_PATCH=COUNT(IO%LMEB_PATCH(:))
!
ZWORK (:) = XUNDEF
ZPATCH(:) = XUNDEF
ZTV   (:) = XUNDEF
ZTL   (:) = XUNDEF
ZTC   (:) = XUNDEF
ZQC   (:) = XUNDEF
!
ZTG (:,:) = XUNDEF
ZTSN(:,:) = XUNDEF
ZCSN(:,:) = XUNDEF
!
CALL INIT_IO_SURF_n(DTCO, U, HPROGRAM,'NATURE','ISBA  ','WRITE', 'ISBA_DIAGNOSTICS.OUT.nc')
!
!-------------------------------------------------------------------------------
! * Near surface atmospheric variables 
!-------------------------------------------------------------------------------
!
YRECFM='tas_land'
YCOMMENT='near-surface air temperature at 2m over land (K)'
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ID%D%XT2M(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='huss_land'
YCOMMENT='near-surface specific humidity at 2m over land (kg kg-1)'
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ID%D%XQ2M(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='hurs_land'
YCOMMENT='near-surface relative humidity at 2m over land (-)'
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ID%D%XHU2M(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='sfcWind_land'
YCOMMENT='near-surface near surface at 10m over land (m s-1)'
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ID%D%XWIND10M(:),IRESP,HCOMMENT=YCOMMENT)
!
!-------------------------------------------------------------------------------
! * Transfer coefficients
!-------------------------------------------------------------------------------
!
YRECFM='ares_land'
YCOMMENT='aerodynamic_resistance over land (s/m)'
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ID%D%XARES(:),IRESP,HCOMMENT=YCOMMENT)
!
!-------------------------------------------------------------------------------
! * Radiative fluxes
!-------------------------------------------------------------------------------
!
! * Downward fluxes 
!
YRECFM='rsds_land'
YCOMMENT='short wave downward radiation over land (W m-2)'
ZWORK(:)=ID%D%XSWD(:)
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='rlds_land'
YCOMMENT='long wave downward radiation over land (W m-2)'
ZWORK(:)=ID%D%XLWD(:)
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
!
! * Upward fluxes
!
YRECFM='rsus_land'
YCOMMENT='short wave upward radiation over land (W m-2)'
ZWORK(:)=ID%D%XSWU(:)
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='rlus_land'
YCOMMENT='long wave upward radiation over land (W m-2)'
ZWORK(:)=ID%D%XLWU(:)
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
!
! * Net fluxes
!
YRECFM='rss_land'
YCOMMENT='net short wave radiation over land (W m-2)'
ZWORK(:)=(ID%D%XSWD(:)-ID%D%XSWU(:))
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='rls_land'
YCOMMENT='net long wave radiation over land (W m-2)'
ZWORK(:)=(ID%D%XLWD(:)-ID%D%XLWU(:))
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
!
! * snow-free net short wave radiation
!
YRECFM='rss_ns'
YCOMMENT='snow-free net short wave radiation over land (W m-2)'
ZWORK(:)=(ID%D%XSWD(:)-ID%DE%XSNFREE_SWU(:))
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
!
!-------------------------------------------------------------------------------
! * Energy fluxes
!-------------------------------------------------------------------------------
!
YRECFM='hfls_land'
YCOMMENT='total latent heat flux over land (W m-2)'
ZWORK(:)=ID%D%XLE(:)
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='hfsbl_land'
YCOMMENT='energy of sublimation (solid to vapor) over land (W m-2)'
ZWORK(:)=ID%D%XLEI(:)
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='hfss_land'
YCOMMENT='Sensible heat flux over land (W m-2)'
ZWORK(:)=ID%D%XH(:)
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='hfdsl'
YCOMMENT='Downward Heat Flux over land (W m-2)'
ZWORK(:)=ID%D%XGFLUX(:)
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='hfdsn'
YCOMMENT='Snow heat flux over land (W m-2)'
ZWORK(:)=ID%DM%XGFLUXSNOW(:)
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='hfdsnb'
YCOMMENT='Heat flux from snow into the ice or land under the snow (W m-2)'
ZWORK(:)=ID%DE%XGRNDFLUX(:)
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)

YRECFM='hfmlt'
YCOMMENT='energy of fusion (solid to liquid) over land (W m-2)'
ZWORK(:)=ID%DE%XDELPHASEN(:)
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='dtes'
YCOMMENT='Change in soil heat storage over land (J m-2)'
ZWORK(:)=ID%DE%XDELHEATG(:)+ID%DE%XDELPHASEG(:)
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='dtesn'
YCOMMENT='Change in snow heat storage over land (J m-2)'
ZWORK(:)=ID%DE%XDELHEATN(:)
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='hfdsl_sfc'
YCOMMENT='Ground heat flux surface to sub-surface over land (W m-2)'
ZWORK(:)=ID%DE%XRESTORE(:)
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='hfdsn_sfc'
YCOMMENT='Snow heat flux surface to sub-surface over land (W m-2)'
ZWORK(:)=ID%DE%XRESTOREN(:)
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='hfmlt_sfc'
YCOMMENT='energy of fusion (solid to liquid) of surface layer over land (W m-2)'
ZWORK(:)=ID%DE%XDELPHASEN_SFC(:)
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='dtes_sfc'
YCOMMENT='Change in first soil surface layer heat storage over land (J m-2)'
ZWORK(:)=ID%DE%XDELHEATG_SFC(:)+ID%DE%XDELPHASEG_SFC(:)
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='dtesn_sfc'
YCOMMENT='Change in first snow surface layer heat storage over land (J m-2)'
ZWORK(:)=ID%DE%XDELHEATN_SFC(:)
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='nrjbud_isba'
YCOMMENT='Energy budget as residue over land (W m-2)'
ZWORK(:)=ID%DE%XNRJBUD(:)
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
!
YRECFM='nrjbuds_isba'
YCOMMENT='Surface energy budget as residue over land (W m-2)'
ZWORK(:)=ID%DE%XNRJBUD_SFC(:)
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
!
!-------------------------------------------------------------------------------
! * Momentum flux 
!-------------------------------------------------------------------------------
!
YRECFM='tau_land'
YCOMMENT='wind stress over land (Pa)'
ZWORK(:)=SQRT(ID%D%XFMU(:)*ID%D%XFMU(:)+ID%D%XFMV(:)*ID%D%XFMV(:))
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
!
!-------------------------------------------------------------------------------
! * Canopy (or surface) air specific humidity [kg/kg]
!-------------------------------------------------------------------------------
!
ZWORK(:)=0.0
DO JP=1,INP
   PK => NP%AL(JP)
   PEK => NPE%AL(JP)
   DO JI=1,PK%NSIZE_P
      IMASK = PK%NR_P(JI)
      ZWORK(IMASK) = ZWORK(IMASK) + PK%XPATCH(JI) * PEK%XQC(JI)
   ENDDO
ENDDO
YRECFM='qc'
YCOMMENT='Canopy (or surface) air specific humidity over land (kg/kg)'
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
!
!-------------------------------------------------------------------------------
! * Soil Temperature profiles
!-------------------------------------------------------------------------------
!
IF(IO%LTEMP_ARP)THEN
  IWORK=IO%NTEMPLAYER_ARP
ELSEIF(IO%CISBA/='DIF')THEN
  IWORK=2
ELSE
  IWORK=INL
ENDIF
!
ZTG(:,:)=0.0
DO JP=1,INP
   PK => NP%AL(JP)
   PEK => NPE%AL(JP)
   DO JL=1,IWORK
      DO JI=1,PK%NSIZE_P
         IMASK = PK%NR_P(JI)
         ZTG(IMASK,JL) = ZTG(IMASK,JL) + PK%XPATCH(JI) * PEK%XTG(JI,JL)
      ENDDO
   ENDDO
ENDDO
!
IF (LALLOW_ADD_DIM)  THEN 
   !
   YRECFM='tsl' 
   YCOMMENT='Soil temperature by layer over land (K)'
   CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZTG(:,:),IRESP,YCOMMENT,HNAM_DIM=YTG_LAYER_DIM_NAME)
   !
   !Per Patch for possible nudging
   !
   DO JP=1,INP
      PK => NP%AL(JP)
      PEK => NPE%AL(JP)
      ZTG(:,:)=XUNDEF
      DO JI=1,PK%NSIZE_P
         IMASK = PK%NR_P(JI)
         ZTG(IMASK,:) = PEK%XTG(JI,:)
      ENDDO
      WRITE(YNUM,'(I2)') JP
      YRECFM='tg_patch'//ADJUSTL(YNUM(:LEN_TRIM(YNUM)))
      YCOMMENT='Temperature by layer per patch over land (K)'   
      CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZTG(:,:),IRESP,YCOMMENT,HNAM_DIM=YTG_LAYER_DIM_NAME)
   END DO
   !
ELSE
   !
   DO JL=1,IWORK
      WRITE(YLVL,'(I4)') JL
      YRECFM='tsl'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
      YCOMMENT='Soil temperature by layer over land (K)'
      CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZTG(:,JL),IRESP,YCOMMENT)
   END DO
   !
ENDIF
!
!-------------------------------------------------------------------------------
! * Snow Internal Temperature 
!-------------------------------------------------------------------------------
!
! * Mean snow internal temperature (K)
!
YRECFM='tsn'
YCOMMENT='snow internal temperature over land (K)'
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ID%DM%XTTSNOW(:),IRESP,HCOMMENT=YCOMMENT)
!
! * Snow temperature profile (K)
!
IF (NPE%AL(1)%TSNOW%SCHEME=='3-L' .OR. NPE%AL(1)%TSNOW%SCHEME=='CRO') THEN  
!
   ZTSN(:,:) = 0.0
   ZCSN(:,:) = 0.0
!   
   DO JP=1,INP
      PK => NP%AL(JP)
      PEK => NPE%AL(JP)
      DO JL=1,INLS
         DO JI=1,PK%NSIZE_P
            IMASK = PK%NR_P(JI)
            IF(PEK%TSNOW%WSNOW(JI,JL)>0.0)THEN
              ZTSN(IMASK,JL) = ZTSN(IMASK,JL) + PK%XPATCH(JI) * PEK%TSNOW%TEMP(JI,JL)
              ZCSN(IMASK,JL) = ZCSN(IMASK,JL) + PK%XPATCH(JI)
            ENDIF
         ENDDO
      ENDDO
   ENDDO
!
   WHERE(ZCSN(:,:)>0.0)
         ZTSN(:,:)=ZTSN(:,:)/ZCSN(:,:)
   ELSEWHERE
         ZTSN(:,:)=XUNDEF
   ENDWHERE
!
  IF (LALLOW_ADD_DIM)  THEN 
     YRECFM='tsnl' 
     YCOMMENT='Snow temperature by layer over land (K)'    
     CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZTSN(:,:),IRESP,YCOMMENT,HNAM_DIM=YSNOW_LAYER_DIM_NAME)
  ELSE
     DO JL=1,INLS
        WRITE(YLVL,'(I4)') JL
        YRECFM='tsnl'//ADJUSTL(YLVL(:LEN_TRIM(YLVL)))
        YCOMMENT='Snow temperature by layer over land (K)'
        CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZTSN(:,JL),IRESP,YCOMMENT)
     END DO
  ENDIF
!
ENDIF
!
!-------------------------------------------------------------------------------
! * Surface Temperature (K)
!-------------------------------------------------------------------------------
!       
! * surface temperature
!
YRECFM='ts_land'
YCOMMENT='surface temperature over land (K)'
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ID%D%XTS(:),IRESP,HCOMMENT=YCOMMENT)
!       
! * radiative surface temperature
!
YRECFM='tr_land'
YCOMMENT='radiative surface temperature over land (K)'
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,S%XTSRAD_NAT(:),IRESP,HCOMMENT=YCOMMENT)
!        
YRECFM='tgs'
YCOMMENT='surface ground temperature over land (K)'
ZWORK(:)=0.0
DO JP=1,INP
   PK => NP%AL(JP)
   PEK => NPE%AL(JP)
   DO JI=1,PK%NSIZE_P
      IMASK = PK%NR_P(JI)
      ZWORK(IMASK) = ZWORK(IMASK) + PK%XPATCH(JI) * PEK%XTG(JI,1)
   ENDDO
ENDDO
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
!
!-------------------------------------------------------------------------------
! * Surface Temperature (K)
!-------------------------------------------------------------------------------
!
IF (ISIZE_LMEB_PATCH>0) THEN
!
   ZTV  (:) = 0.0
   ZTL  (:) = 0.0
   ZTC  (:) = 0.0
   ZWORK(:) = 0.0
   DO JP=1,INP
      PK => NP%AL(JP)
      PEK => NPE%AL(JP)
      DO JI=1,PK%NSIZE_P
         IMASK = PK%NR_P(JI)
         IF(IO%LMEB_PATCH(JP))THEN
           ZTV  (IMASK) = ZTV  (IMASK) + PK%XPATCH(JI) * PEK%XTV(JI)
           ZTL  (IMASK) = ZTL  (IMASK) + PK%XPATCH(JI) * PEK%XTL(JI)
           ZTC  (IMASK) = ZTC  (IMASK) + PK%XPATCH(JI) * PEK%XTC(JI)
           ZWORK(IMASK) = ZWORK(IMASK) + PK%XPATCH(JI)
         ENDIF
      ENDDO
   ENDDO
   WHERE(ZWORK(:)>0.0)
        ZTV(:) = ZTV(:)/ZWORK(:)
        ZTL(:) = ZTL(:)/ZWORK(:)
        ZTC(:) = ZTC(:)/ZWORK(:)
   ELSEWHERE
        ZTV(:) = XUNDEF
        ZTL(:) = XUNDEF
        ZTC(:) = XUNDEF
   ENDWHERE
!       
  YRECFM='tcs'
  YCOMMENT='vegetation canopy temperature over land (K)'  
  CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZTV(:),IRESP,HCOMMENT=YCOMMENT)
!       
  YRECFM='tc'
  YCOMMENT='air canopy temperature over land (K)'  
  CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZTC(:),IRESP,HCOMMENT=YCOMMENT)
!      
  IF (IO%LMEB_LITTER) THEN
     YRECFM='tlit'
     YCOMMENT='surface litter temperature over land (K)'
     CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZTL(:),IRESP,HCOMMENT=YCOMMENT)
  ENDIF
!
ENDIF
!
IF (NPE%AL(1)%TSNOW%SCHEME=='3-L' .OR. NPE%AL(1)%TSNOW%SCHEME=='CRO') THEN  
!       
! * surface snow temperature
!
   YRECFM='tsns'
   YCOMMENT='surface snow temperature over land (K)'   
   CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ID%DM%XTTSNOW(:),IRESP,HCOMMENT=YCOMMENT)
!
ELSE
!       
! * surface soil and snow temperature
!      
   YRECFM='tsns'
   YCOMMENT='surface snow temperature over land (K)'   
   CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZTSN(:,1),IRESP,HCOMMENT=YCOMMENT)
!
END IF
!
!-------------------------------------------------------------------------------
! * Albedo fields
!-------------------------------------------------------------------------------
!
! * Total Albedo 
!
YRECFM='albsrfc_land'
YCOMMENT='surface albedo over land (-)'
WHERE(ID%D%XSWD(:)>0.0)
  ZWORK(:) = ID%D%XALBT(:)
ELSEWHERE
  ZWORK(:) = XUNDEF
ENDWHERE
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
!
! * Snow albedo (-) 
!
ZPATCH(:) = 0.0
ZWORK (:) = 0.0
DO JP=1,INP
   PK => NP%AL(JP)
   PEK => NPE%AL(JP)
   DO JI=1,PK%NSIZE_P
      IMASK = PK%NR_P(JI)
      IF(PEK%TSNOW%ALB(JI)/=XUNDEF)THEN
        ZWORK (IMASK) = ZWORK (IMASK) + PK%XPATCH(JI) * PEK%TSNOW%ALB(JI)
        ZPATCH(IMASK) = ZPATCH(IMASK) + PK%XPATCH(JI)
      ENDIF
   ENDDO
ENDDO
!
WHERE(ZPATCH(:)>0.0.AND.ID%D%XSWD(:)>0.0)
  ZWORK(:) = ZWORK(:) / ZPATCH(:)
ELSEWHERE
  ZWORK(:) = XUNDEF
ENDWHERE
!
YRECFM='albsn'
YCOMMENT='surface snow albedo over land (-)'
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
!
! * Snow free albedo (-) 
!
WHERE(ID%D%XSWD(:)>0.0)
  ZWORK(:) = ID%DE%XSNFREE_SWU(:)/ID%D%XSWD(:)
ELSEWHERE
  ZWORK(:) = XUNDEF
ENDWHERE
!
YRECFM='albsnfree'
YCOMMENT='surface snow-free albedo over land (-)'
CALL WRITE_SURF(DUO%CSELECT,HPROGRAM,YRECFM,ZWORK(:),IRESP,HCOMMENT=YCOMMENT)
!
!-------------------------------------------------------------------------------
!
!         End of IO
!
CALL END_IO_SURF_n(HPROGRAM)
!
IF (LHOOK) CALL DR_HOOK('WRITE_DIAG_MIP_ENERGY_ISBA_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
END SUBROUTINE WRITE_DIAG_MIP_ENERGY_ISBA_n
