!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!#############################################################
SUBROUTINE COMPUTE_ISBA_PARAMETERS (DTCO, DGU, UG, U, IM, DST, SLT, SV, &
                                    HPROGRAM,HINIT,OLAND_USE,            &
                             KI,KSV,KSW,                                &
                             HSV,PCO2,PRHOA,                            &
                             PZENITH,PSW_BANDS,PDIR_ALB,PSCA_ALB,       &
                             PEMIS,PTSRAD,PTSURF,                       &
                             HTEST                                      )  
!#############################################################
!
!!****  *COMPUTE_ISBA_PARAMETERS_n* - routine to initialize ISBA
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
!!      V. Masson   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2004
!!      Modified by P. Le Moigne (11/2004): miscellaneous diagnostics
!!      Modified by P. Le Moigne (06/2006): seeding and irrigation    
!!      Modified by B. Decharme    (2008) : SGH and Flooding scheme
!!      Modified by B. Decharme  (01/2009): optional deep soil temperature as in Arpege
!!      Modified by R. Hamdi     (01/2009): Cp and L
!!      Modified by B. Decharme  (06/2009): read topographic index statistics
!!      Modified by P. Le Moigne (01/2009): Beljaars sso
!!      Modified by B. Decharme  (08/2009): Active Trip coupling variable if Earth System Model
!!      A.L. Gibelin   04/09 : change BSLAI_NITRO initialisation
!!      A.L. Gibelin   04/09 : modifications for CENTURY model 
!!      A.L. Gibelin   06/09 : soil carbon initialisation
!!      Modified by B. Decharme  (09/2012): Bug in exponential profile calculation with DIF
!!      F. Bouttier    08/13 : apply random perturbation patterns for ensembles
!!      B. Vincendon   03/14 : bug correction for CISBA=3L and CKSAT=EXP (TOPD coupling)
!!      Modified by B. Decharme  (04/2013): Subsurface runoff if SGH (DIF option only)
!!                                          Delete CTOPREG (never used)
!!                                          Delete NWG_LAYER_TOT, NWG_SIZE
!!                                          water table / Surface coupling
!!      P. Samuelsson  02/14 : MEB
!!      B. Decharme    01/16 : Bug when vegetation veg, z0 and emis are imposed whith interactive vegetation
!!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_SURFEX_n, ONLY : ISBA_MODEL_t
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_DIAG_SURF_ATM_n, ONLY : DIAG_SURF_ATM_t
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_DST_n, ONLY : DST_t
USE MODD_SLT_n, ONLY : SLT_t
USE MODD_SV_n, ONLY : SV_t
!
USE MODD_SFX_OASIS,  ONLY : LCPL_LAND, LCPL_FLOOD, LCPL_GW, LCPL_CALVING
!
!
#ifdef TOPD
USE MODD_DUMMY_EXP_PROFILE,ONLY : XC_DEPTH_RATIO
#endif
!
USE MODD_ASSIM, ONLY : CASSIM_ISBA, LASSIM
!
USE MODD_DEEPSOIL,       ONLY : LPHYSDOMC, LDEEPSOIL, XTDEEP_CLI, XGAMMAT_CLI
USE MODD_AGRI,           ONLY : LAGRIP, XTHRESHOLD
!
!
USE MODD_SGH_PAR,        ONLY : NDIMTAB, XICE_DEPH_MAX, XF_DECAY
!
USE MODD_DATA_COVER_PAR, ONLY : NVEGTYPE
USE MODD_SURF_PAR,       ONLY : XUNDEF, NUNDEF
USE MODD_SNOW_PAR,       ONLY : XEMISSN
!
USE MODD_TOPD_PAR, ONLY : NUNIT
USE MODD_TOPODYN, ONLY : NNCAT, NMESHT
!
!
USE MODE_RANDOM
!
USE MODI_GET_LUOUT
USE MODI_ABOR1_SFX
USE MODI_INIT_IO_SURF_n
USE MODI_ALLOCATE_PHYSIO
USE MODI_INIT_ISBA_MIXPAR
USE MODI_CONVERT_PATCH_ISBA
USE MODI_INIT_VEG_PGD_n
USE MODI_INIT_TOP
USE MODI_EXP_DECAY_SOIL_FR
USE MODI_CARBON_INIT
USE MODI_SOILTEMP_ARP_PAR
USE MODI_END_IO_SURF_n
!
USE MODI_READ_SURF
USE MODI_READ_ISBA_n
USE MODI_INIT_ISBA_LANDUSE
USE MODI_READ_ISBA_CANOPY_n
USE MODI_INIT_VEG_n
USE MODI_AVERAGED_ALBEDO_EMIS_ISBA
USE MODI_DIAG_ISBA_INIT_n
USE MODI_INIT_SURF_TOPD
USE MODI_ISBA_SOC_PARAMETERS
!
USE MODI_READ_AND_SEND_MPI
USE MODI_ISBA_TO_TOPD
USE MODI_OPEN_FILE
USE MODI_CLOSE_FILE
USE MODI_FIX_MEB_VEG
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
TYPE(ISBA_MODEL_t), INTENT(INOUT) :: IM
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(DIAG_SURF_ATM_t), INTENT(INOUT) :: DGU
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(DST_t), INTENT(INOUT) :: DST
TYPE(SLT_t), INTENT(INOUT) :: SLT
TYPE(SV_t), INTENT(INOUT) :: SV
!
 CHARACTER(LEN=6),                 INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
 CHARACTER(LEN=3),                 INTENT(IN)  :: HINIT     ! choice of fields to initialize
LOGICAL,                          INTENT(IN)  :: OLAND_USE !
INTEGER,                          INTENT(IN)  :: KI        ! number of points
INTEGER,                          INTENT(IN)  :: KSV       ! number of scalars
INTEGER,                          INTENT(IN)  :: KSW       ! number of short-wave spectral bands
 CHARACTER(LEN=6), DIMENSION(KSV), INTENT(IN)  :: HSV       ! name of all scalar variables
REAL,             DIMENSION(KI),  INTENT(IN)  :: PCO2      ! CO2 concentration (kg/m3)
REAL,             DIMENSION(KI),  INTENT(IN)  :: PRHOA     ! air density
REAL,             DIMENSION(KI),  INTENT(IN)  :: PZENITH   ! solar zenithal angle
REAL,             DIMENSION(KSW), INTENT(IN)  :: PSW_BANDS ! middle wavelength of each band
REAL,             DIMENSION(KI,KSW),INTENT(OUT) :: PDIR_ALB  ! direct albedo for each band
REAL,             DIMENSION(KI,KSW),INTENT(OUT) :: PSCA_ALB  ! diffuse albedo for each band
REAL,             DIMENSION(KI),  INTENT(OUT) :: PEMIS     ! emissivity
REAL,             DIMENSION(KI),  INTENT(OUT) :: PTSRAD    ! radiative temperature
REAL,             DIMENSION(KI),  INTENT(OUT) :: PTSURF    ! surface effective temperature         (K)
!
 CHARACTER(LEN=2),                 INTENT(IN)  :: HTEST       ! must be equal to 'OK'
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
REAL, DIMENSION(U%NDIM_FULL)   :: ZF_PARAM, ZC_DEPTH_RATIO
!
REAL, DIMENSION(KI)     :: ZTSRAD_NAT !radiative temperature
REAL, DIMENSION(KI)     :: ZTSURF_NAT !effective temperature
!
REAL, DIMENSION(KI,IM%I%NPATCH)  :: ZWG1 ! work array for surface water content
REAL, DIMENSION(KI,IM%I%NPATCH)  :: ZTG1 ! work array for surface temperature
!
REAL, DIMENSION(KI)         :: ZM, ZWORK
REAL, DIMENSION(KI,IM%I%NPATCH)  :: ZF, ZPERTBUF
!
INTEGER :: IDIM_FULL, JL
INTEGER           :: JILU     ! loop increment
INTEGER           :: ILUOUT   ! unit of output listing file
INTEGER           :: IRESP   ! return code
INTEGER           :: IDECADE, IDECADE2  ! decade of simulation
INTEGER           :: JPATCH  ! loop counter on tiles
INTEGER           :: INFOMPI
INTEGER           :: ISIZE_LMEB_PATCH  ! Number of patches with MEB=true
!
LOGICAL                           :: LWORK
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!               Initialisation for IO
!
IF (LHOOK) CALL DR_HOOK('COMPUTE_ISBA_PARAMETERS',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
IF (HTEST/='OK') THEN
  CALL ABOR1_SFX('COMPUTE_ISBA_PARAMETERS: FATAL ERROR DURING ARGUMENT TRANSFER')
END IF
!
ZF   (:,:) = XUNDEF
ZM   (:)   = XUNDEF
ZWORK(:)   = XUNDEF
!
!*       2.3    Physiographic data fields from land cover:
!               -----------------------------------------
!
 CALL ALLOCATE_PHYSIO(IM%I, &
                      IM%I%CPHOTO, IM%I%CISBA, KI, NVEGTYPE, IM%I%NGROUND_LAYER, IM%I%NPATCH, &
                     IM%I%XVEGTYPE, IM%I%XLAI, IM%I%XVEG, IM%I%XZ0, IM%I%XEMIS, IM%I%XDG, IM%I%XD_ICE,      &
                     IM%I%XRSMIN, IM%I%XGAMMA, IM%I%XWRMAX_CF, IM%I%XRGL, IM%I%XCV,               &
                     IM%I%XZ0_O_Z0H, IM%I%XALBNIR_VEG, IM%I%XALBVIS_VEG, IM%I%XALBUV_VEG,    &
                     IM%I%XH_TREE, IM%I%XRE25, IM%I%XLAIMIN, IM%I%XBSLAI, IM%I%XSEFOLD,           &
                     IM%I%XGMES, IM%I%XGC, IM%I%XF2I, IM%I%XDMAX, IM%I%LSTRESS,                   &
                     IM%I%XCE_NITRO, IM%I%XCF_NITRO, IM%I%XCNA_NITRO,                   &
                     IM%I%TSEED, IM%I%TREAP, IM%I%XWATSUP, IM%I%XIRRIG,                      &
                     IM%I%XROOTFRAC, IM%I%NWG_LAYER, IM%I%XDROOT, IM%I%XDG2,                 &
                     IM%I%XGNDLITTER,IM%I%XRGLGV,IM%I%XGAMMAGV,IM%I%XRSMINGV,    &
                     IM%I%XROOTFRACGV,IM%I%XWRMAX_CFGV,IM%I%XLAIGV,IM%I%XZ0LITTER,IM%I%XH_VEG     )
!
IF (IM%I%TTIME%TDATE%MONTH /= NUNDEF) THEN
  IDECADE = 3 * ( IM%I%TTIME%TDATE%MONTH - 1 ) + MIN(IM%I%TTIME%TDATE%DAY-1,29) / 10 + 1
ELSE
  IDECADE = 1
END IF
!
IDECADE2 = IDECADE
!
 CALL INIT_ISBA_MIXPAR(DTCO, IM%DTI, IM%IG, IM%I, &
                       IM%I%CISBA,IDECADE,IDECADE2,IM%I%XCOVER,IM%I%LCOVER,IM%I%CPHOTO,'NAT')
!
ISIZE_LMEB_PATCH=COUNT(IM%I%LMEB_PATCH(:))
IF (ISIZE_LMEB_PATCH>0)  THEN
  CALL FIX_MEB_VEG(IM%DTI, IM%IG, IM%I, &
                   IM%I%NPATCH)
ENDIF
!
 CALL CONVERT_PATCH_ISBA(DTCO, IM%DTI, IM%I, &
                         IM%I%CISBA,IDECADE,IDECADE2,IM%I%XCOVER,IM%I%LCOVER,IM%I%CPHOTO,LAGRIP,   &
                        IM%I%LPERM,IM%I%LTR_ML,'NAT',PVEG=IM%I%XVEG,PLAI=IM%I%XLAI,                &
                        PRSMIN=IM%I%XRSMIN,PGAMMA=IM%I%XGAMMA,PWRMAX_CF=IM%I%XWRMAX_CF,       &
                        PRGL=IM%I%XRGL,PCV=IM%I%XCV,PSOILGRID=IM%I%XSOILGRID,                 &
                        PDG=IM%I%XDG,KWG_LAYER=IM%I%NWG_LAYER,PDROOT=IM%I%XDROOT,PDG2=IM%I%XDG2,   &
                        PZ0=IM%I%XZ0,PZ0_O_Z0H=IM%I%XZ0_O_Z0H,                           &
                        PALBNIR_VEG=IM%I%XALBNIR_VEG,PALBVIS_VEG=IM%I%XALBVIS_VEG,       &
                        PALBUV_VEG=IM%I%XALBUV_VEG,PEMIS_ECO=IM%I%XEMIS,                 &
                        PVEGTYPE=IM%I%XVEGTYPE,PROOTFRAC=IM%I%XROOTFRAC,                 &
                        PGMES=IM%I%XGMES,PBSLAI=IM%I%XBSLAI,PLAIMIN=IM%I%XLAIMIN,             &
                        PSEFOLD=IM%I%XSEFOLD,PGC=IM%I%XGC,                               &
                        PDMAX=IM%I%XDMAX,PF2I=IM%I%XF2I,OSTRESS=IM%I%LSTRESS,PH_TREE=IM%I%XH_TREE, &
                        PRE25=IM%I%XRE25,PCE_NITRO=IM%I%XCE_NITRO,PCF_NITRO=IM%I%XCF_NITRO,   &
                        PCNA_NITRO=IM%I%XCNA_NITRO,PD_ICE=IM%I%XD_ICE,TPSEED=IM%I%TSEED,      &
                        TPREAP=IM%I%TREAP,PWATSUP=IM%I%XWATSUP,PIRRIG=IM%I%XIRRIG,            &
                        PGNDLITTER=IM%I%XGNDLITTER,                                           &
                        PRGLGV=IM%I%XRGLGV,   &
                        PGAMMAGV=IM%I%XGAMMAGV,PRSMINGV=IM%I%XRSMINGV,                   &
                        PROOTFRACGV=IM%I%XROOTFRACGV,PWRMAX_CFGV=IM%I%XWRMAX_CFGV,       &
                        PLAIGV=IM%I%XLAIGV,PZ0LITTER=IM%I%XZ0LITTER,PH_VEG=IM%I%XH_VEG        )
!
!-------------------------------------------------------------------------------
!
 CALL INIT_VEG_PGD_n(IM%CHI, DTCO, DST, IM%I, SLT, U, &
                    HPROGRAM, 'NATURE', ILUOUT, KI, IM%I%NPATCH, IM%I%NGROUND_LAYER,      &
                    IM%I%TTIME%TDATE%MONTH,                                          &
                    IM%I%XVEGTYPE, IM%I%XPATCH, IM%I%XVEGTYPE_PATCH, IM%I%NSIZE_NATURE_P,           &
                    IM%I%NR_NATURE_P, IM%I%XRM_PATCH,                                     &
                    LDEEPSOIL, LPHYSDOMC, XTDEEP_CLI, XGAMMAT_CLI, IM%I%XTDEEP,      &
                    IM%I%XGAMMAT, LAGRIP, XTHRESHOLD, IM%AG%NIRRINUM, IM%AG%LIRRIDAY, IM%AG%LIRRIGATE, &
                    IM%AG%XTHRESHOLDSPT,                                              &
                    IM%I%CPHOTO, HINIT, IM%I%LTR_ML, IM%I%NNBIOMASS, PCO2, PRHOA, IM%I%XABC, IM%I%XPOI,  &
                    IM%I%XGMES, IM%I%XGC, IM%I%XDMAX, IM%I%XANMAX, IM%I%XFZERO, IM%I%XEPSO, IM%I%XGAMM, IM%I%XQDGAMM,   & 
                    IM%I%XQDGMES, IM%I%XT1GMES, IM%I%XT2GMES, IM%I%XAMAX, IM%I%XQDAMAX, IM%I%XT1AMAX, IM%I%XT2AMAX,&
                    IM%I%XAH, IM%I%XBH, IM%I%XTAU_WOOD, IM%I%XINCREASE, IM%I%XTURNOVER,                  &
                    KSV, HSV, IM%CHI%SVI, IM%CHI%CCH_NAMES, IM%CHI%CAER_NAMES, &
                    IM%CHI%CDSTNAMES, IM%CHI%CSLTNAMES, IM%CHI%CCHEM_SURF_FILE,                      &
                    DST%XSFDST, DST%XSFDSTM, SLT%XSFSLT,                                    &
                    IM%I%XAOSIP, IM%I%XAOSIM, IM%I%XAOSJP, IM%I%XAOSJM, IM%I%XHO2IP, IM%I%XHO2IM, IM%I%XHO2JP,     &
                    IM%I%XHO2JM, IM%I%XZ0, IM%I%XZ0EFFIP, IM%I%XZ0EFFIM, IM%I%XZ0EFFJP, IM%I%XZ0EFFJM, IM%I%XZ0REL,&
                    IM%I%XCLAY, IM%I%XSAND, IM%I%CPEDOTF,                                      &
                    IM%I%XCONDSAT, IM%I%XMPOTSAT, IM%I%XBCOEF, IM%I%XWWILT, IM%I%XWFC, IM%I%XWSAT, IM%I%XWD0,      &
                    IM%I%XKANISO, IM%I%CRUNOFF,                                           &
                    IM%I%XTAUICE, IM%I%XCGSAT, IM%I%XC1SAT, IM%I%XC2REF, IM%I%XC3, IM%I%XC4B, IM%I%XACOEF, IM%I%XPCOEF, &
                    IM%I%XC4REF, IM%I%XPCPS, IM%I%XPLVTT, IM%I%XPLSTT,                              &
                    IM%I%CSCOND, IM%I%CISBA, IM%I%XHCAPSOIL, IM%I%XCONDDRY, IM%I%XCONDSLD, IM%I%CCPSURF,      &
                    IM%I%XDG, IM%I%XDROOT, IM%I%XDG2, IM%I%XROOTFRAC, IM%I%XRUNOFFD, IM%I%XDZG, IM%I%XDZDIF,       &
                    IM%I%XSOILWGHT, IM%I%NWG_LAYER, IM%I%NLAYER_HORT, IM%I%NLAYER_DUN, IM%I%XD_ICE,      &
                    IM%I%XKSAT_ICE, IM%I%XALBNIR_DRY, IM%I%XALBVIS_DRY, IM%I%XALBUV_DRY,            &
                    IM%I%XALBNIR_WET, IM%I%XALBVIS_WET, IM%I%XALBUV_WET, IM%I%XBSLAI_NITRO,         &
                    IM%I%XCE_NITRO, IM%I%XCNA_NITRO, IM%I%XCF_NITRO, IM%I%XFWTD, IM%I%XWTD               )  
!
!-------------------------------------------------------------------------------
!
!DIF option :
!    Anisotropy coeficient for hydraulic conductivity for topmodel drainage (Fan et al. 2006)
!    Soil organic matter effect and/or Exponential decay for DIF option
!    Must be call before INIT_TOP
!
!
IF(IM%I%CISBA=='DIF') THEN
  !
  IF( IM%I%CKSAT=='SGH' )THEN
    WRITE(ILUOUT,*)'THE KSAT EXP PROFILE WITH ISBA-DF IS NOT PHYSIC AND HAS BEEN REMOVED FOR NOW' 
    WRITE(ILUOUT,*)'A NEW PHYSICAL APPROACH WILL BE DEVELLOPED ACCOUNTING FOR COMPACTION IN ALL ' 
    WRITE(ILUOUT,*)'HYDRODYNAMIC PARAMETERS (WSAT, PSISAT, KSAT, B) AND NOT ONLY IN KSAT        ' 
    CALL ABOR1_SFX('CKSAT=SGH is not physic with ISBA-DF and has been removed for now')    
  ENDIF
  !  
  IF(IM%I%LSOC)THEN   
    IF(.NOT.IM%I%LSOCP)THEN
      WRITE(ILUOUT,*)'LSOC = T can be activated only if SOC data given in PGD fields'
      CALL ABOR1_SFX('LSOC = T can be activated only if SOC data given in PGD fields')
    ENDIF
    ALLOCATE(IM%I%XFRACSOC(KI,IM%I%NGROUND_LAYER))
    IM%I%XFRACSOC(:,:)=0.0
    CALL ISBA_SOC_PARAMETERS(IM%I%CRUNOFF,IM%I%XPATCH,IM%I%XDG,IM%I%XSOC,IM%I%XBCOEF,IM%I%XMPOTSAT,   &
                             IM%I%XCONDSAT,IM%I%XWSAT,IM%I%XHCAPSOIL,IM%I%XCONDDRY,         &
                             IM%I%XCONDSLD,IM%I%XWFC,IM%I%XWWILT,IM%I%XWD0,IM%I%XKANISO,IM%I%XFRACSOC )
  ELSE
    ALLOCATE(IM%I%XFRACSOC(0,0))
  ENDIF
!
ELSE
  ALLOCATE(IM%I%XFRACSOC(0,0))
ENDIF
!
!Topmodel
!
!CRUNOFF used in hydro_sgh and isba_sgh_update
IF( IM%I%CRUNOFF=='SGH ') THEN 
!
  ALLOCATE(IM%I%XTAB_FSAT(KI,NDIMTAB))
  ALLOCATE(IM%I%XTAB_WTOP(KI,NDIMTAB))
  ALLOCATE(IM%I%XTAB_QTOP(KI,NDIMTAB))
!
  IM%I%XTAB_FSAT(:,:) = 0.0
  IM%I%XTAB_WTOP(:,:) = 0.0
  IM%I%XTAB_QTOP(:,:) = 0.0
!
  IF(HINIT/='PRE' .AND. .NOT.LASSIM)THEN
!
    WHERE(IM%I%XCLAY(:,1)==XUNDEF.AND.IM%I%XTI_MEAN(:)/=XUNDEF) IM%I%XTI_MEAN(:)=XUNDEF
!
    CALL INIT_TOP(IM%I, &
                   IM%I%CISBA, ILUOUT, IM%I%XPATCH, IM%I%XRUNOFFD,          &
                   IM%I%XWD0, IM%I%XWSAT, IM%I%XTI_MIN,                     &
                   IM%I%XTI_MAX, IM%I%XTI_MEAN, IM%I%XTI_STD, IM%I%XTI_SKEW,     &
                   IM%I%XSOILWGHT, IM%I%XTAB_FSAT, IM%I%XTAB_WTOP,          &
                   IM%I%XTAB_QTOP, ZM                             )  
!
!
    IF (IM%I%CKSAT=='SGH' .AND. IM%I%CISBA/='DIF') THEN
!     Exponential decay factor calculate using soil properties 
!     (eq. 11, Decharme et al., J. Hydrometeor, 2006)
      DO JILU=1,KI
        IF (ZM(JILU)/=XUNDEF) ZF(JILU,:) = (IM%I%XWSAT(JILU,1)-IM%I%XWD0(JILU,1))/ZM(JILU)
      ENDDO
!       
    ENDIF
!
  ENDIF
!
! Subsurface flow by layer (m/s)
  IF(IM%I%CISBA=='DIF') THEN
    ALLOCATE(IM%I%XTOPQS(KI,IM%I%NGROUND_LAYER,IM%I%NPATCH))
    IM%I%XTOPQS(:,:,:)=0.0
  ELSE
    ALLOCATE(IM%I%XTOPQS(0,0,0))
  ENDIF
!
ELSE                  
!  
  ALLOCATE(IM%I%XTAB_FSAT(0,0))
  ALLOCATE(IM%I%XTAB_WTOP(0,0))
  ALLOCATE(IM%I%XTAB_QTOP(0,0))
  ALLOCATE(IM%I%XTOPQS(0,0,0))  
!                  
ENDIF  
!
!Exponential decay for ISBA-FR option
!CKSAT used in hydro_soil.F90 and soil.F90
IF(HINIT/='PRE'.AND.IM%I%CISBA/='DIF')THEN 
  !
  IF(IM%I%CKSAT=='SGH') THEN
    !
    WHERE(ZF(:,:)==XUNDEF.AND.IM%I%XDG(:,2,:)/=XUNDEF) 
      ZF(:,:) = 4.0/IM%I%XDG(:,2,:)
    ENDWHERE
    ZF(:,:) = MIN(ZF(:,:),XF_DECAY)
    !
    ALLOCATE(IM%I%XF_PARAM (KI))
    IM%I%XF_PARAM(:) = ZF(:,1)
    !
    DO JPATCH=1,IM%I%NPATCH
      IF (IM%I%NSIZE_NATURE_P(JPATCH) == 0 ) CYCLE
        CALL EXP_DECAY_SOIL_FR(IM%I%CISBA, ZF(:,JPATCH),IM%I%XC1SAT(:,JPATCH),IM%I%XC2REF(:,JPATCH),   &
                                IM%I%XDG(:,:,JPATCH),IM%I%XD_ICE(:,JPATCH),IM%I%XC4REF(:,JPATCH),      &
                                IM%I%XC3(:,:,JPATCH),IM%I%XCONDSAT(:,:,JPATCH),IM%I%XKSAT_ICE(:,JPATCH))  
    ENDDO                       
    !
  ELSEIF ( IM%I%CKSAT=='EXP' .AND. IM%I%CISBA=='3-L' ) THEN
    !
    ALLOCATE(IM%I%XF_PARAM (KI))
    IM%I%XF_PARAM(:) = XUNDEF
    !
    IF (HPROGRAM/='AROME ' .AND. HPROGRAM/='MESONH ') THEN
      !
      CALL OPEN_FILE('ASCII ',NUNIT,HFILE='carte_f_dc.txt',HFORM='FORMATTED',HACTION='READ ')
      DO JILU=1,U%NDIM_FULL
        READ(NUNIT,*) ZF_PARAM(JILU), ZC_DEPTH_RATIO(JILU)
      ENDDO
      CALL CLOSE_FILE('ASCII ',NUNIT)
      CALL READ_AND_SEND_MPI(ZF_PARAM,IM%I%XF_PARAM,U%NR_NATURE)
#ifdef TOPD
IF (.NOT.ALLOCATED(XC_DEPTH_RATIO))    ALLOCATE(XC_DEPTH_RATIO (KI))
    XC_DEPTH_RATIO(:) = XUNDEF
      CALL READ_AND_SEND_MPI(ZC_DEPTH_RATIO,XC_DEPTH_RATIO,U%NR_NATURE)
#endif
      !
    ELSE
      WRITE(ILUOUT,*) "COMPUTE_ISBA_PARAMETERS: WITH CKSAT=EXP, IN NOT OFFLINE "//&
                      "MODE, TOPMODEL FILE FOR F_PARAM IS NOT READ "
    ENDIF
    !
    DO JPATCH=1,IM%I%NPATCH
      WHERE (IM%I%XF_PARAM(:)==XUNDEF.AND.IM%I%XDG(:,2,JPATCH)/=XUNDEF)
        ZF(:,JPATCH) = 4.0/IM%I%XDG(:,2,JPATCH)
      ELSEWHERE
        ZF(:,JPATCH) = IM%I%XF_PARAM(:)
      ENDWHERE
    ENDDO
     ZF(:,:) = MIN(ZF(:,:),XF_DECAY)
    !
    DO JPATCH=1,IM%I%NPATCH
      CALL EXP_DECAY_SOIL_FR(IM%I%CISBA, ZF(:,JPATCH),IM%I%XC1SAT(:,JPATCH),IM%I%XC2REF(:,JPATCH), &
                             IM%I%XDG(:,:,JPATCH),IM%I%XD_ICE(:,JPATCH),IM%I%XC4REF(:,JPATCH),   &
                             IM%I%XC3(:,:,JPATCH),IM%I%XCONDSAT(:,:,JPATCH),                &
                             IM%I%XKSAT_ICE(:,JPATCH))  
    ENDDO    
    !    
  ENDIF
  ! 
ENDIF
!
!
!*       2.10   Soil carbon
!               -----------                        
!
IF (HINIT == 'ALL' .AND. IM%I%CRESPSL=='CNT' .AND. IM%I%CPHOTO == 'NCB') THEN
  CALL CARBON_INIT(IM%I%NNBIOMASS, IM%I%NNLITTER, IM%I%NNLITTLEVS, IM%I%NNSOILCARB)
ENDIF
!
!Rainfall spatial distribution
!CRAIN used in HYDRO_VEG and HYDRO_SGH and ISBA_SGH_UPDATE
IF(IM%I%CRAIN=='SGH')THEN
  ALLOCATE(IM%I%XMUF(KI))
  IM%I%XMUF(:)=0.0
ELSE
  ALLOCATE(IM%I%XMUF(0))
ENDIF
!
ALLOCATE(IM%I%XFSAT(KI))  
IM%I%XFSAT(:) = 0.0
!
!-------------------------------------------------------------------------------
!
!*       6.2    Initialize of SFX - RRM coupling:
!               ---------------------------------
!
! * Check some key :
!
IF(LCPL_CALVING)THEN
   IF(.NOT.IM%I%LGLACIER)THEN
     CALL ABOR1_SFX('COMPUTE_ISBA_PARAMETERS: LGLACIER MUST BE ACTIVATED IF LCPL_CALVING')
   ENDIF
ENDIF
!
! * Initialize required coupling fields :
!
IM%I%LCPL_RRM = .FALSE.
IM%I%LFLOOD   = .FALSE.
IM%I%LWTD     = .FALSE.
!
IF(LCPL_LAND)THEN
!    
  IM%I%LCPL_RRM = .TRUE.
!
  ALLOCATE(IM%I%XCPL_DRAIN (KI))
  ALLOCATE(IM%I%XCPL_RUNOFF(KI))
  IM%I%XCPL_DRAIN (:) = 0.0
  IM%I%XCPL_RUNOFF(:) = 0.0
!
  IF(IM%I%LGLACIER)THEN
     ALLOCATE(IM%I%XCPL_ICEFLUX(KI))
     IM%I%XCPL_ICEFLUX(:) = 0.0
  ELSE
     ALLOCATE(IM%I%XCPL_ICEFLUX(0))
  ENDIF
!
  IF(LCPL_GW)THEN
    IM%I%LWTD = .TRUE.
    ALLOCATE(IM%I%XCPL_RECHARGE(KI))
    IM%I%XCPL_RECHARGE(:) = 0.0
  ELSE
    ALLOCATE(IM%I%XCPL_RECHARGE(0))
  ENDIF
!
  IF(LCPL_FLOOD)THEN
     IM%I%LFLOOD = .TRUE.
     ALLOCATE(IM%I%XCPL_EFLOOD(KI))
     ALLOCATE(IM%I%XCPL_PFLOOD(KI))
     ALLOCATE(IM%I%XCPL_IFLOOD(KI))
     IM%I%XCPL_EFLOOD(:)= 0.0
     IM%I%XCPL_PFLOOD(:)= 0.0
     IM%I%XCPL_IFLOOD(:)= 0.0    
  ELSE
    ALLOCATE(IM%I%XCPL_EFLOOD(0))
    ALLOCATE(IM%I%XCPL_PFLOOD(0))
    ALLOCATE(IM%I%XCPL_IFLOOD(0))     
  ENDIF     
!
ELSE
!
  ALLOCATE(IM%I%XCPL_RUNOFF  (0))
  ALLOCATE(IM%I%XCPL_DRAIN   (0))
  ALLOCATE(IM%I%XCPL_ICEFLUX (0))
  ALLOCATE(IM%I%XCPL_RECHARGE(0))
  ALLOCATE(IM%I%XCPL_EFLOOD  (0))
  ALLOCATE(IM%I%XCPL_PFLOOD  (0))
  ALLOCATE(IM%I%XCPL_IFLOOD  (0))
!
ENDIF
!
IF(IM%I%LWTD.AND..NOT.IM%I%LGW)THEN
  WRITE(ILUOUT,*)'COMPUTE_ISBA_PARAMETERS: Groundwater map is required by SFX - Groundwater coupling '
  WRITE(ILUOUT,*)'COMPUTE_ISBA_PARAMETERS: Please check your pgd namelist where this map must be     '
  WRITE(ILUOUT,*)'COMPUTE_ISBA_PARAMETERS: specified (YGW and YGWFILETYPE, or XUNIF_GW, or LIMP_GW)  '
  CALL ABOR1_SFX('COMPUTE_ISBA_PARAMETERS: Groundwater map is required by SFX - Groundwater coupling')
ENDIF
!
! * Initialize flood scheme :
!
IF(IM%I%LFLOOD)THEN
  ALLOCATE(IM%I%XFFLOOD (KI))
  ALLOCATE(IM%I%XPIFLOOD(KI))
  ALLOCATE(IM%I%XFF     (KI,IM%I%NPATCH))
  ALLOCATE(IM%I%XFFG    (KI,IM%I%NPATCH))
  ALLOCATE(IM%I%XFFV    (KI,IM%I%NPATCH))
  ALLOCATE(IM%I%XFFROZEN(KI,IM%I%NPATCH))
  ALLOCATE(IM%I%XALBF   (KI,IM%I%NPATCH))
  ALLOCATE(IM%I%XEMISF  (KI,IM%I%NPATCH)) 
  IM%I%XFFLOOD       = 0.0
  IM%I%XPIFLOOD      = 0.0
  IM%I%XFF           = 0.0
  IM%I%XFFG          = 0.0
  IM%I%XFFV          = 0.0
  IM%I%XFFROZEN      = 0.0
  IM%I%XALBF         = 0.0
  IM%I%XEMISF        = 0.0  
ELSE
  ALLOCATE(IM%I%XFFLOOD   (0))
  ALLOCATE(IM%I%XPIFLOOD  (0))
  ALLOCATE(IM%I%XFF     (0,0))
  ALLOCATE(IM%I%XFFG    (0,0))
  ALLOCATE(IM%I%XFFV    (0,0))
  ALLOCATE(IM%I%XFFROZEN(0,0))
  ALLOCATE(IM%I%XALBF   (0,0))
  ALLOCATE(IM%I%XEMISF  (0,0))
ENDIF
!
!-------------------------------------------------------------------------------
!
!*      7.     ISBA time-varying deep force-restore temperature initialization
!              ---------------------------------------------------------------
!
 CALL SOILTEMP_ARP_PAR(IM%I, &
                       HPROGRAM,IM%I%LTEMP_ARP,IM%I%NTEMPLAYER_ARP)
!
!-------------------------------------------------------------------------------
!
!*       9.     Prints of cover parameters in a tex file
!               ----------------------------------------
!
!* if only physiographic fields are to be initialized, stop here.
!
IF (HINIT/='ALL' .AND. HINIT/='SOD') THEN
  IF (LHOOK) CALL DR_HOOK('COMPUTE_ISBA_PARAMETERS',1,ZHOOK_HANDLE)
  RETURN
END IF
!
!-------------------------------------------------------------------------------
!
IF (CASSIM_ISBA=="ENKF ") THEN
  !
  CALL INIT_RANDOM_SEED()
  !
ENDIF
!
 CALL INIT_IO_SURF_n(DTCO, DGU, U, &
                    HPROGRAM,'NATURE','ISBA  ','READ ')
!
!*      10.     Prognostic and semi-prognostic fields
!               -------------------------------------
!
 CALL READ_ISBA_n(DTCO, IM%I, U, &
                  HPROGRAM)
!
IF (HINIT/='ALL') THEN
  CALL END_IO_SURF_n(HPROGRAM)
  IF (LHOOK) CALL DR_HOOK('COMPUTE_ISBA_PARAMETERS',1,ZHOOK_HANDLE)
  RETURN
END IF
!
IF (HINIT=='PRE' .AND. IM%I%TSNOW%SCHEME.NE.'3-L' .AND. IM%I%TSNOW%SCHEME.NE.'CRO' .AND. IM%I%CISBA=='DIF') THEN
    CALL ABOR1_SFX("INIT_ISBAN: WITH CISBA = DIF, CSNOW MUST BE 3-L OR CRO")
ENDIF
!
!-------------------------------------------------------------------------------
!
!*      11.  Extrapolation of the prognostic and semi-prognostic fields
!                           LAND USE case 
!               -------------------------------------
!
IF (OLAND_USE) THEN
   CALL INIT_ISBA_LANDUSE(DTCO, IM%IG, IM%I, UG, U, &
                          HPROGRAM)  
END IF
!
!-------------------------------------------------------------------------------
!
!*      12.     Canopy air fields:
!               -----------------
!
 CALL READ_ISBA_CANOPY_n(DTCO, IM%ICP, IM%I, U, &
                         HPROGRAM)
!
!-------------------------------------------------------------------------------
!
!*      13.     initialize radiative and physical properties
!               --------------------------------------------
!
ALLOCATE(IM%I%XDIR_ALB_WITH_SNOW(KI,KSW,IM%I%NPATCH))
ALLOCATE(IM%I%XSCA_ALB_WITH_SNOW(KI,KSW,IM%I%NPATCH))
IM%I%XDIR_ALB_WITH_SNOW = 0.0
IM%I%XSCA_ALB_WITH_SNOW = 0.0
!
 CALL INIT_VEG_n(IM%I%NPATCH, KI, IM%I%LCANOPY, IM%I%CROUGH, IM%I%LAGRI_TO_GRASS, IM%I%TSNOW, &
                 IM%I%CPHOTO, IM%DTI%LIMP_VEG, IM%DTI%LIMP_Z0, IM%DTI%LIMP_EMIS, &
                 IM%I%XLAIMIN, IM%I%XH_TREE, IM%I%XVEGTYPE_PATCH, IM%I%XLAI, &
                 IM%I%XZ0, IM%I%XVEG, IM%I%XEMIS, &
                 IM%I%LTR_ML, IM%I%XFAPARC, IM%I%XFAPIRC, IM%I%XLAI_EFFC, IM%I%XMUS, &
                 IM%I%XALBNIR_SOIL, IM%I%XALBVIS_SOIL, IM%I%XALBUV_SOIL, IM%I%XALBNIR, &
                 IM%I%XALBVIS, IM%I%XALBUV, &
                 IM%DGMI%LSURF_DIAG_ALBEDO, IM%I%XPSN, IM%I%XPSNG, IM%I%XPSNV, IM%I%XPSNV_A, &
                 PDIR_ALB, PSCA_ALB, PEMIS, PTSRAD )
!
DO JPATCH=1,IM%I%NPATCH
  ZWG1(:,JPATCH) = IM%I%XWG(:,1,JPATCH)
  ZTG1(:,JPATCH) = IM%I%XTG(:,1,JPATCH)
END DO
!
 CALL CONVERT_PATCH_ISBA(DTCO, IM%DTI, IM%I, &
                         IM%I%CISBA,IDECADE,IDECADE2,IM%I%XCOVER,IM%I%LCOVER,&
                          IM%I%CPHOTO,LAGRIP,IM%I%LPERM,IM%I%LTR_ML,'NAT',   &
                          PWG1 = ZWG1,               &
                          PALBNIR_SOIL=IM%I%XALBNIR_SOIL, &
                          PALBVIS_SOIL=IM%I%XALBVIS_SOIL, &
                          PALBUV_SOIL=IM%I%XALBUV_SOIL )
!
! Load randomly perturbed fields. Perturbation ratios are saved in case fields are reset later.
IF(IM%I%LPERTSURF) THEN
!
  CALL READ_SURF(&
                 HPROGRAM,'VEG',IM%I%XVEG(:,:),IRESP)
  ALLOCATE(IM%I%XPERTVEG(KI))
  IM%I%XPERTVEG(:)=IM%I%XVEG(:,1)
!
  CALL READ_SURF(&
                 HPROGRAM,'LAI',IM%I%XLAI(:,:),IRESP)
  ALLOCATE(IM%I%XPERTLAI(KI))
  IM%I%XPERTLAI(:)=IM%I%XLAI(:,1)
!
  CALL READ_SURF(&
                 HPROGRAM,'CV',IM%I%XCV(:,:),IRESP)
  ALLOCATE(IM%I%XPERTCV(KI))
  IM%I%XPERTCV(:)=IM%I%XCV(:,1)
!
  CALL READ_SURF(&
                 HPROGRAM,'PERTALB',ZPERTBUF(:,:),IRESP)
  ALLOCATE(IM%I%XPERTALB(KI))
  IM%I%XPERTALB(:)=ZPERTBUF(:,1)
  WHERE(IM%I%XALBNIR_VEG(:,1)/=XUNDEF)  IM%I%XALBNIR_VEG(:,1) = IM%I%XALBNIR_VEG(:,1) *( 1.+ IM%I%XPERTALB(:) )
  WHERE(IM%I%XALBVIS_VEG(:,1)/=XUNDEF)  IM%I%XALBVIS_VEG(:,1) = IM%I%XALBVIS_VEG(:,1) *( 1.+ IM%I%XPERTALB(:) )
  WHERE(IM%I%XALBUV_VEG(:,1)/=XUNDEF)   IM%I%XALBUV_VEG(:,1)  = IM%I%XALBUV_VEG(:,1)  *( 1.+ IM%I%XPERTALB(:) )
  WHERE(IM%I%XALBNIR_SOIL(:,1)/=XUNDEF) IM%I%XALBNIR_SOIL(:,1)= IM%I%XALBNIR_SOIL(:,1)*( 1.+ IM%I%XPERTALB(:) )
  WHERE(IM%I%XALBVIS_SOIL(:,1)/=XUNDEF) IM%I%XALBVIS_SOIL(:,1)= IM%I%XALBVIS_SOIL(:,1)*( 1.+ IM%I%XPERTALB(:) )
  WHERE(IM%I%XALBUV_SOIL(:,1)/=XUNDEF)  IM%I%XALBUV_SOIL(:,1) = IM%I%XALBUV_SOIL(:,1) *( 1.+ IM%I%XPERTALB(:) )
!
  CALL READ_SURF(&
                 HPROGRAM,'PERTZ0LAND',ZPERTBUF(:,:),IRESP)
  ALLOCATE(IM%I%XPERTZ0(KI))
  IM%I%XPERTZ0(:)=ZPERTBUF(:,1)
  WHERE(IM%I%XZ0(:,1)/=XUNDEF)      IM%I%XZ0(:,1)     =IM%I%XZ0(:,1)     *( 1.+ IM%I%XPERTZ0(:) )
  WHERE(IM%I%XZ0EFFIP(:,1)/=XUNDEF) IM%I%XZ0EFFIP(:,1)=IM%I%XZ0EFFIP(:,1)*( 1.+ IM%I%XPERTZ0(:) )
  WHERE(IM%I%XZ0EFFIM(:,1)/=XUNDEF) IM%I%XZ0EFFIM(:,1)=IM%I%XZ0EFFIM(:,1)*( 1.+ IM%I%XPERTZ0(:) )
  WHERE(IM%I%XZ0EFFJP(:,1)/=XUNDEF) IM%I%XZ0EFFJP(:,1)=IM%I%XZ0EFFJP(:,1)*( 1.+ IM%I%XPERTZ0(:) )
  WHERE(IM%I%XZ0EFFJM(:,1)/=XUNDEF) IM%I%XZ0EFFJM(:,1)=IM%I%XZ0EFFJM(:,1)*( 1.+ IM%I%XPERTZ0(:) )
!
ENDIF
!
!-------------------------------------------------------------------------------
!
!*       14.    Output radiative fields
!               -----------------------
!
ALLOCATE(IM%I%XEMIS_NAT   (KI))
IM%I%XEMIS_NAT (:) = XUNDEF
!
 CALL AVERAGED_ALBEDO_EMIS_ISBA(IM%I, &
                                IM%I%LFLOOD, IM%I%CALBEDO, PZENITH,                &
                                 IM%I%XVEG,IM%I%XZ0,IM%I%XLAI,                          &
                                 IM%I%LMEB_PATCH,IM%I%XGNDLITTER,IM%I%XZ0LITTER,IM%I%XLAIGV, &
                                 IM%I%XH_VEG, IM%I%XTV,               &
                                 ZTG1,                                   &
                                 IM%I%XPATCH,                                 &
                                 PSW_BANDS,                              &
                                 IM%I%XALBNIR_VEG,IM%I%XALBVIS_VEG,IM%I%XALBUV_VEG,     &
                                 IM%I%XALBNIR_SOIL,IM%I%XALBVIS_SOIL,IM%I%XALBUV_SOIL,  &
                                 IM%I%XEMIS,                                  &
                                 IM%I%TSNOW,                                  &
                                 IM%I%XALBNIR,IM%I%XALBVIS,IM%I%XALBUV,                 &
                                 PDIR_ALB, PSCA_ALB,                     &
                                 IM%I%XEMIS_NAT,ZTSRAD_NAT,ZTSURF_NAT         )  
!
PEMIS  = IM%I%XEMIS_NAT
PTSRAD = ZTSRAD_NAT
PTSURF = ZTSURF_NAT
!
!-------------------------------------------------------------------------------
!
!*      15.     ISBA diagnostics initialization
!               -------------------------------
!
IF(IM%I%NPATCH<=1) IM%DGI%LPATCH_BUDGET=.FALSE.
!
 CALL DIAG_ISBA_INIT_n(&
                       IM%CHI, IM%DGEI, IM%DGI, IM%DGMI, DGU, IM%GB, IM%I, &
                       HPROGRAM,KI,KSW)
!
!-------------------------------------------------------------------------------
!
 CALL INIT_SURF_TOPD(IM%DGEI, IM%I, UG, U, &
                     HPROGRAM,U%NDIM_FULL)
!
!-------------------------------------------------------------------------------
!
!         End of IO
!
 CALL END_IO_SURF_n(HPROGRAM)
IF (LHOOK) CALL DR_HOOK('COMPUTE_ISBA_PARAMETERS',1,ZHOOK_HANDLE)
!
END SUBROUTINE COMPUTE_ISBA_PARAMETERS


