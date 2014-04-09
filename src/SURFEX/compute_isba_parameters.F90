!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!#############################################################
SUBROUTINE COMPUTE_ISBA_PARAMETERS(HPROGRAM,HINIT,OLAND_USE,            &
                             KI,KSV,KSW,                                &
                             HSV,PCO2,PRHOA,                            &
                             PZENITH,PSW_BANDS,PDIR_ALB,PSCA_ALB,       &
                             PEMIS,PTSRAD,                              &
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
!!	V. Masson   *Meteo France*	
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
!!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_SURFEX_OMP, ONLY : NBLOCKTOT
USE MODD_SURFEX_MPI, ONLY : NWG_LAYER_TOT, NWG_SIZE,  NPIO, NCOMM, NPROC, NRANK, WLOG_MPI
!
#ifdef ASC
USE MODD_IO_SURF_ASC,  ONLY : NMASK_asc => NMASK
#endif
#ifdef FA
USE MODD_IO_SURF_FA ,  ONLY : NMASK_fa => NMASK
#endif
#ifdef LFI
USE MODD_IO_SURF_LFI,  ONLY : NMASK_lfi => NMASK
#endif
!
USE MODD_ISBA_n,   ONLY : CROUGH, CISBA, CPEDOTF, CPHOTO, CRUNOFF, CALBEDO,   &
                          CSCOND, CRESPSL, LTR_ML, NNBIOMASS, NNLITTER,       &
                          NNLITTLEVS, NNSOILCARB, XCLAY, XSAND, XSOC,         &
                          XWWILT, XWFC, XWSAT, XRM_PATCH, LPERM,              &
                          XCOVER, XVEG, XLAI, XRSMIN, XGAMMA, XRGL, XCV,      &
                          XDG, NWG_LAYER, XDROOT, XDG2, XDZG, XDZDIF,         &
                          XZ0, XZ0_O_Z0H, XABC, XPOI, XANMAX, XFZERO, XEPSO,  &
                          XGAMM, XQDGAMM, XQDGMES, XT1GMES, XT2GMES, XAMAX,   &
                          XQDAMAX, XT1AMAX, XT2AMAX, XAH, XBH, XTAU_WOOD,     &
                          XINCREASE, XTURNOVER, XALBNIR_VEG, XALBVIS_VEG,     &
                          XALBUV_VEG, XEMIS, XVEGTYPE, XGMES, XRE25, XBSLAI,  &
                          XLAIMIN, XGC,XDMAX, LSTRESS, XF2I,                  &
                          XSEFOLD, XH_TREE, XPATCH, NPATCH, XWRMAX_CF,        &
                          NR_NATURE_P, NSIZE_NATURE_P,                        &
                          XALBNIR_DRY, XALBVIS_DRY, XALBUV_DRY,               &
                          XALBNIR_WET, XALBVIS_WET, XALBUV_WET,               &
                          XALBNIR_SOIL, XALBVIS_SOIL, XALBUV_SOIL,            &
                          XWG, XTG, TSNOW, XALBNIR, XALBVIS, XALBUV,          &
                          XEMIS_NAT, XFAPARC, XFAPIRC, XLAI_EFFC, XMUS,       &
                          XAOSIP,XAOSIM,XAOSJP,XAOSJM,                        &
                          XHO2IP,XHO2IM,XHO2JP,XHO2JM,                        &
                          XZ0EFFIP,XZ0EFFIM,XZ0EFFJP,XZ0EFFJM, XZ0REL,        &
                          XVEGTYPE_PATCH,XROOTFRAC,XRUNOFFD,XSOILWGHT,        &
                          XCGSAT, XC1SAT, XC2REF, XC3, XC4B, XACOEF, XPCOEF,  &
                          XTAUICE, XBCOEF, XCONDSAT,                          &
                          XHCAPSOIL, XCONDDRY, XCONDSLD, XC4REF, XMPOTSAT,    &
                          XTDEEP, XGAMMAT, NGROUND_LAYER, XSOILGRID, TTIME,   &
                          XCE_NITRO, XCF_NITRO,                               &
                          XCNA_NITRO, XBSLAI_NITRO, CCPSURF, TSEED,           &
                          TREAP, XWATSUP, XIRRIG, XCGMAX,                     &
                          CKSAT, CTOPREG, CRAIN, LSOCP, CSOC, XFRACSOC,       &
                          XTI_MIN, XTI_MAX, XTI_MEAN, XTI_STD, XTI_SKEW,      &
                          XTAB_FSAT, XTAB_WTOP, XD_ICE, XKSAT_ICE,            &
                          XFSAT, XMUF, LTRIP, LFLOOD, XFFLOOD, XFFROZEN,      &
                          XPIFLOOD, XCPL_EFLOOD, XCPL_PFLOOD, XCPL_IFLOOD,    &
                          XCPL_DRAIN, XCPL_RUNOFF, LGLACIER,                  &
                          LTEMP_ARP, NTEMPLAYER_ARP, XPSN, XPSNG, XPSNV,      &
                          XPSNV_A, XFF, XFFG, XFFV, XPCPS, XPLVTT, XPLSTT,    &
                          LCANOPY, LCANOPY_DRAG, XDIR_ALB_WITH_SNOW,          &
                          XSCA_ALB_WITH_SNOW, XALBF, XEMISF, XCPL_ICEFLUX,    &
                          NLAYER_HORT, NLAYER_DUN, XF_PARAM, XC_DEPTH_RATIO
!
USE MODD_CH_ISBA_n, ONLY : CSV, CCH_NAMES, NBEQ, NSV_CHSBEG, NSV_CHSEND,         &
                           CCHEM_SURF_FILE, NDSTEQ, NSV_DSTBEG, NSV_DSTEND,      &
                           NSV_AERBEG, NSV_AEREND, NAEREQ, CDSTNAMES, CAER_NAMES,&
                           NSLTEQ, NSV_SLTBEG,  NSV_SLTEND, CSLTNAMES,           &
                           LCH_BIO_FLUX, CCH_DRY_DEP  
!
USE MODD_DEEPSOIL,       ONLY : LPHYSDOMC, LDEEPSOIL, XTDEEP_CLI, XGAMMAT_CLI
USE MODD_AGRI,           ONLY : LAGRIP, XTHRESHOLD
USE MODD_AGRI_n,         ONLY : NIRRINUM, XTHRESHOLDSPT, LIRRIDAY, LIRRIGATE
!
USE MODD_DIAG_ISBA_n,      ONLY : LPATCH_BUDGET
USE MODD_DIAG_MISC_ISBA_n, ONLY : LSURF_DIAG_ALBEDO
!
USE MODD_SURF_ATM,    ONLY : LCPL_ESM
USE MODD_SURF_ATM_n,  ONLY : NDIM_FULL
!
USE MODD_SGH_PAR,        ONLY : NDIMTAB, XICE_DEPH_MAX, XF_DECAY
!
USE MODD_DATA_COVER_PAR, ONLY : NVEGTYPE
USE MODD_SURF_PAR,       ONLY : XUNDEF, NUNDEF
USE MODD_SNOW_PAR,       ONLY : XEMISSN
!
USE MODD_TOPODYN, ONLY : NNCAT, NMESHT
USE MODD_SURF_ATM_n, ONLY : NR_NATURE, NDIM_FULL
!
USE MODD_DST_n
USE MODD_SLT_n
!
USE MODI_GET_LUOUT
USE MODI_ABOR1_SFX
USE MODI_INIT_IO_SURF_n
USE MODI_ALLOCATE_PHYSIO
USE MODI_INIT_ISBA_MIXPAR
USE MODI_CONVERT_PATCH_ISBA
USE MODI_INIT_VEG_PGD_n
USE MODI_INIT_TOP
USE MODI_EXP_DECAY_SOIL_DIF
USE MODI_EXP_DECAY_SOIL_FR
USE MODI_CARBON_INIT
USE MODI_SOILTEMP_ARP_PAR
USE MODI_WRITE_COVER_TEX_ISBA
USE MODI_WRITE_COVER_TEX_ISBA_PAR
USE MODI_END_IO_SURF_n
!
USE MODI_READ_ISBA_n
USE MODI_INIT_ISBA_LANDUSE
USE MODI_READ_ISBA_CANOPY_n
USE MODI_INIT_VEG_n
USE MODI_AVERAGED_ALBEDO_EMIS_ISBA
USE MODI_DIAG_ISBA_INIT_n
USE MODI_INIT_SURF_TOPD
USE MODI_ISBA_SOC_PARAMETERS
!
USE MODI_GATHER_AND_WRITE_MPI
!
USE MODI_READ_AND_SEND_MPI
USE MODI_ISBA_TO_TOPD
USE MODI_OPEN_FILE
USE MODI_CLOSE_FILE
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
#ifndef NOMPI
INCLUDE "mpif.h"
#endif
!
!*       0.1   Declarations of arguments
!              -------------------------
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
!
 CHARACTER(LEN=2),                 INTENT(IN)  :: HTEST       ! must be equal to 'OK'
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
REAL, DIMENSION(NDIM_FULL)   :: ZF_PARAM, ZC_DEPTH_RATIO
!
REAL, DIMENSION(KI)     :: ZTSRAD_NAT !radiative temperature
!
REAL, DIMENSION(:,:), ALLOCATABLE :: ZWG1 ! work array for surface water content
REAL, DIMENSION(:,:), ALLOCATABLE :: ZTG1 ! work array for surface temperature
!
REAL, DIMENSION(:),   ALLOCATABLE :: ZM, ZWORK
REAL, DIMENSION(:,:), ALLOCATABLE :: ZF
!
INTEGER :: IDIM_FULL, JL
INTEGER           :: JILU     ! loop increment
INTEGER           :: ILUOUT   ! unit of output listing file
INTEGER           :: IDECADE, IDECADE2  ! decade of simulation
INTEGER :: JPATCH  ! loop counter on tiles
INTEGER           :: IUNIT       ! unit of f/dc map file
INTEGER           :: INFOMPI
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
!
!*       2.3    Physiographic data fields from land cover:
!               -----------------------------------------
!
 CALL ALLOCATE_PHYSIO(CPHOTO, CISBA, KI, NVEGTYPE, NGROUND_LAYER, NPATCH, &
                     XVEGTYPE, XLAI, XVEG, XZ0, XEMIS, XDG, XD_ICE,      &
                     XRSMIN, XGAMMA, XWRMAX_CF, XRGL, XCV,               &
                     XZ0_O_Z0H, XALBNIR_VEG, XALBVIS_VEG, XALBUV_VEG,    &
                     XH_TREE, XRE25, XLAIMIN, XBSLAI, XSEFOLD,           &
                     XGMES, XGC, XF2I, XDMAX, LSTRESS,                   &
                     XCE_NITRO, XCF_NITRO, XCNA_NITRO,                   &
                     TSEED, TREAP, XWATSUP, XIRRIG,                      &
                     XROOTFRAC, NWG_LAYER, XDROOT, XDG2                  )
!
IF (TTIME%TDATE%MONTH /= NUNDEF) THEN
  IDECADE = 3 * ( TTIME%TDATE%MONTH - 1 ) + MIN(TTIME%TDATE%DAY-1,29) / 10 + 1
ELSE
  IDECADE = 1
END IF
!
IDECADE2 = IDECADE
!
 CALL INIT_ISBA_MIXPAR(CISBA,IDECADE,IDECADE2,XCOVER,CPHOTO,'NAT')
!
 CALL CONVERT_PATCH_ISBA(CISBA,IDECADE,IDECADE2,XCOVER,CPHOTO,LAGRIP,LPERM,    &
                        'NAT',PVEG=XVEG,PLAI=XLAI,                             &
                        PRSMIN=XRSMIN,PGAMMA=XGAMMA,PWRMAX_CF=XWRMAX_CF,       &
                        PRGL=XRGL,PCV=XCV,PSOILGRID=XSOILGRID,                 &
                        PDG=XDG,KWG_LAYER=NWG_LAYER,PDROOT=XDROOT,PDG2=XDG2,   &
                        PZ0=XZ0,PZ0_O_Z0H=XZ0_O_Z0H,                           &
                        PALBNIR_VEG=XALBNIR_VEG,PALBVIS_VEG=XALBVIS_VEG,       &
                        PALBUV_VEG=XALBUV_VEG,PEMIS_ECO=XEMIS,                 &
                        PVEGTYPE=XVEGTYPE,PROOTFRAC=XROOTFRAC,                 &
                        PGMES=XGMES,PBSLAI=XBSLAI,PLAIMIN=XLAIMIN,             &
                        PSEFOLD=XSEFOLD,PGC=XGC,                               &
                        PDMAX=XDMAX,PF2I=XF2I,OSTRESS=LSTRESS,PH_TREE=XH_TREE, &
                        PRE25=XRE25,PCE_NITRO=XCE_NITRO,PCF_NITRO=XCF_NITRO,   &
                        PCNA_NITRO=XCNA_NITRO,PD_ICE=XD_ICE,TPSEED=TSEED,      &
                        TPREAP=TREAP,PWATSUP=XWATSUP,PIRRIG=XIRRIG             )
!
IF(CISBA=='DIF')THEN
  IF (NPROC>1 .OR. NBLOCKTOT>1) THEN  
    IDIM_FULL = SIZE(NWG_LAYER_TOT,1)
!$OMP SINGLE
    DEALLOCATE(NWG_LAYER_TOT)
    ALLOCATE(NWG_LAYER_TOT(IDIM_FULL,SIZE(NWG_LAYER,2)))
!$OMP END SINGLE             
    DO JL = 1,SIZE(NWG_LAYER,2)
      IF (HPROGRAM=='ASCII ') THEN
#ifdef ASC
        CALL GATHER_AND_WRITE_MPI(NWG_LAYER(:,JL),NWG_LAYER_TOT(:,JL),NMASK_asc)
#endif
      ELSEIF (HPROGRAM=='LFI   ') THEN
#ifdef LFI
        CALL GATHER_AND_WRITE_MPI(NWG_LAYER(:,JL),NWG_LAYER_TOT(:,JL),NMASK_lfi)
#endif
      ELSEIF (HPROGRAM=='FA    ') THEN
#ifdef FA
        CALL GATHER_AND_WRITE_MPI(NWG_LAYER(:,JL),NWG_LAYER_TOT(:,JL),NMASK_fa)
#endif
      ELSE
        CALL ABOR1_SFX("COMPUTE_ISBA_PARAMETERS: WITH CISBA=DIF, THE GATHERING OF "//&
           "NWG_LAYER FOR ALL PROCS MUST BE IMPLEMENTED IN AROME AND MESONH CASES ")
      ENDIF
    ENDDO
  ELSE
    DEALLOCATE(NWG_LAYER_TOT)
    ALLOCATE(NWG_LAYER_TOT(SIZE(NWG_LAYER,1),SIZE(NWG_LAYER,2)))          
    NWG_LAYER_TOT = NWG_LAYER
  ENDIF
  NWG_SIZE = 0
  IF (NRANK==NPIO) NWG_SIZE=MAXVAL(NWG_LAYER_TOT(:,:),NWG_LAYER_TOT(:,:)/=NUNDEF)
  IF (NPROC>1) THEN
!$OMP SINGLE   
#ifndef NOMPI
    CALL MPI_BCAST(NWG_SIZE,KIND(NWG_SIZE)/4,MPI_INTEGER,NPIO,NCOMM,INFOMPI)
#endif
!$OMP END SINGLE
  ENDIF  
  !
ENDIF
!
!-------------------------------------------------------------------------------
!
 CALL INIT_VEG_PGD_n(HPROGRAM, 'NATURE',ILUOUT, KI, NPATCH, NGROUND_LAYER, TTIME%TDATE%MONTH,   &
                  XVEGTYPE, XPATCH, XVEGTYPE_PATCH, NSIZE_NATURE_P, NR_NATURE_P,    &
                  XRM_PATCH, &
                  LDEEPSOIL, LPHYSDOMC, XTDEEP_CLI, XGAMMAT_CLI, XTDEEP, XGAMMAT,   &
                  LAGRIP, XTHRESHOLD, NIRRINUM, LIRRIDAY, LIRRIGATE, XTHRESHOLDSPT, &
                  CPHOTO, HINIT, LTR_ML, NNBIOMASS, PCO2, PRHOA, XABC, XPOI,  &
                  XGMES, XGC, XDMAX, XANMAX, XFZERO, XEPSO, XGAMM, XQDGAMM,   & 
                  XQDGMES, XT1GMES, XT2GMES, XAMAX, XQDAMAX, XT1AMAX, XT2AMAX,&
                  XAH, XBH, XTAU_WOOD, XINCREASE, XTURNOVER,                  &
                  KSV, HSV, NBEQ, CSV, NAEREQ, NSV_CHSBEG, NSV_CHSEND,        &
                  NSV_AERBEG, NSV_AEREND, CCH_NAMES, CAER_NAMES, NDSTEQ,      &
                  NSV_DSTBEG, NSV_DSTEND, NSLTEQ, NSV_SLTBEG, NSV_SLTEND,     &
                  CDSTNAMES, CSLTNAMES, CCHEM_SURF_FILE,                      &
                  XSFDST, XSFDSTM, XSFSLT,                                    &
                  XAOSIP, XAOSIM, XAOSJP, XAOSJM, XHO2IP, XHO2IM, XHO2JP,     &
                  XHO2JM, XZ0, XZ0EFFIP, XZ0EFFIM, XZ0EFFJP, XZ0EFFJM, XZ0REL,&
                  XCLAY, XSAND, CPEDOTF,                                      &
                  XCONDSAT, XMPOTSAT, XBCOEF, XWWILT, XWFC, XWSAT,            &
                  XTAUICE, XCGSAT, XC1SAT, XC2REF, XC3, XC4B, XACOEF, XPCOEF, &
                  XC4REF, XPCPS, XPLVTT, XPLSTT,                              &
                  CSCOND, CISBA, XHCAPSOIL, XCONDDRY, XCONDSLD, CCPSURF,      &
                  XDG, XDROOT, XDG2, XROOTFRAC, XRUNOFFD, XDZG, XDZDIF,       &
                  XSOILWGHT, NWG_LAYER, NLAYER_HORT, NLAYER_DUN, XD_ICE,      &
                  XKSAT_ICE, XALBNIR_DRY, XALBVIS_DRY, XALBUV_DRY,            &
                  XALBNIR_WET, XALBVIS_WET, XALBUV_WET, XBSLAI_NITRO,         &
                  XCE_NITRO, XCNA_NITRO, XCF_NITRO                            )  
!
!-------------------------------------------------------------------------------
!
IF(CISBA=='DIF') THEN
  !
  IF( CKSAT=='SGH' )THEN 
    !
    ALLOCATE(ZWORK(KI))
    ALLOCATE(ZF(KI,NPATCH))
    ZWORK(:) = XUNDEF
    ZF(:,:)  = XUNDEF          
    DO JPATCH=1,NPATCH    
      IF (NSIZE_NATURE_P(JPATCH) == 0 ) CYCLE 
      DO JILU=1,KI
        IF(XPATCH(JILU,JPATCH)>0.0)THEN
          !no profile for non vegetated area : f and root = 0.0
          LWORK=(XDROOT(JILU,JPATCH)==0.0.OR.XDROOT(JILU,JPATCH)==XUNDEF)
          ZF   (JILU,JPATCH) = MIN(XF_DECAY,4.0/MAX(0.01,XDROOT(JILU,JPATCH)))
          ZF   (JILU,JPATCH) = MERGE(0.0,ZF    (JILU,JPATCH),LWORK) 
          ZWORK(JILU       ) = MERGE(0.0,XDROOT(JILU,JPATCH),LWORK)
        ENDIF
      ENDDO
      CALL EXP_DECAY_SOIL_DIF(ZF(:,JPATCH),XDG(:,:,JPATCH),NWG_LAYER(:,JPATCH),ZWORK(:),&
                              XCONDSAT(:,:,JPATCH))
    ENDDO  
    DEALLOCATE(ZWORK)
    DEALLOCATE(ZF)
  ENDIF
  !  
  IF(CSOC=='SGH')THEN   
    IF(.NOT.LSOCP)THEN
      CALL ABOR1_SFX('CSOC=SGH can be activated only if SOC data given in PGD fields')
    ENDIF
    ALLOCATE(XFRACSOC(KI,NGROUND_LAYER))
    XFRACSOC(:,:)=0.0
    CALL ISBA_SOC_PARAMETERS(XPATCH,XDG,XSOC,XBCOEF,XMPOTSAT,   &
                             XCONDSAT,XWSAT,XHCAPSOIL,XCONDDRY, &
                             XCONDSLD,XWFC,XWWILT,XFRACSOC )
  ELSE
    ALLOCATE(XFRACSOC(0,0))
  ENDIF
! 
ELSE
  ALLOCATE(XFRACSOC(0,0))
ENDIF
!
!Topmodel
!  
IF ((CKSAT=='SGH' .OR. CKSAT=='EXP') .AND. HINIT/='PRE' .AND. CISBA/='DIF') THEN
  ALLOCATE(ZF(KI,NPATCH))
  ZF (:,:) = XUNDEF
ENDIF
!
!CRUNOFF used in hydro_sgh and isba_sgh_update
IF( CRUNOFF=='SGH ') THEN 
!
  ALLOCATE(XTAB_FSAT(KI,NDIMTAB))
  ALLOCATE(XTAB_WTOP(KI,NDIMTAB))
!
  XTAB_FSAT(:,:) = 0.0
  XTAB_WTOP(:,:) = 0.0
!
  IF(HINIT/='PRE')THEN
!
    WHERE(XCLAY(:,1)==XUNDEF.AND.XTI_MEAN(:)/=XUNDEF) XTI_MEAN(:)=XUNDEF
!
    IF(CTOPREG/='DEF')THEN
       WRITE(ILUOUT,*)'!'
       WRITE(ILUOUT,*)'  YOU USE TOPMODEL WITHOUT THE REGRESSION    ' 
       WRITE(ILUOUT,*)' OF WOLOCK AND MCCABE (2000) (OPTION TOPREG) '
       WRITE(ILUOUT,*)'!'
    ENDIF
!      
    ALLOCATE(ZM(KI))
    ZM (:) = XUNDEF
!
    CALL INIT_TOP (CISBA, CTOPREG, ILUOUT, XPATCH, XRUNOFFD, &
                   XDZG, XWWILT, XWSAT, XTI_MIN,             &
                   XTI_MAX, XTI_MEAN, XTI_STD, XTI_SKEW,     &
                   XSOILWGHT, XTAB_FSAT, XTAB_WTOP, ZM       )  
!
!
    IF (CKSAT=='SGH' .AND. CISBA/='DIF') THEN
!     Exponential decay factor calculate using soil properties 
!     (eq. 11, Decharme et al., J. Hydrometeor, 2006)
      DO JILU=1,KI
        IF (ZM(JILU)/=XUNDEF) ZF(JILU,:) = (XWSAT(JILU,1)-XWWILT(JILU,1))/ZM(JILU)
      ENDDO
!       
    ENDIF
!
    DEALLOCATE(ZM)
!
  ENDIF
! 
ELSE                  
!  
  ALLOCATE(XTAB_FSAT(0,0))
  ALLOCATE(XTAB_WTOP(0,0))
!                  
ENDIF  
! 
!Exponential decay for ISBA-FR option
!CKSAT used in hydro_soil.F90 and soil.F90
IF(HINIT/='PRE'.AND.CISBA/='DIF')THEN 
  !
  IF(CKSAT=='SGH') THEN
    !
    WHERE(ZF(:,:)==XUNDEF.AND.XDG(:,2,:)/=XUNDEF) 
      ZF(:,:) = 4.0/XDG(:,2,:)
    ENDWHERE
    ZF(:,:) = MIN(ZF(:,:),XF_DECAY)
    !
    ALLOCATE(XF_PARAM (KI))
    ALLOCATE(XC_DEPTH_RATIO (KI))
    XF_PARAM(:) = ZF(:,1)
    XC_DEPTH_RATIO(:) = 1.25
    !
    DO JPATCH=1,NPATCH
      IF (NSIZE_NATURE_P(JPATCH) == 0 ) CYCLE
        CALL EXP_DECAY_SOIL_FR(CISBA, ZF(:,JPATCH),XC1SAT(:,JPATCH),XC2REF(:,JPATCH),   &
                                XDG(:,:,JPATCH),XD_ICE(:,JPATCH),XC4REF(:,JPATCH),      &
                                XC3(:,:,JPATCH),XCONDSAT(:,:,JPATCH),XKSAT_ICE(:,JPATCH))  
    ENDDO                       
    ! 
  ELSEIF ( CKSAT=='EXP' .AND. CISBA=='3-L' ) THEN
    !
    ALLOCATE(XF_PARAM (KI))
    ALLOCATE(XC_DEPTH_RATIO (KI))
    XF_PARAM(:) = XUNDEF
    XC_DEPTH_RATIO(:) = XUNDEF
    !
    IF (HPROGRAM/='AROME ' .AND. HPROGRAM/='MESONH ') THEN
      !
      CALL OPEN_FILE('ASCII ',IUNIT,HFILE='carte_f_dc.txt',HFORM='FORMATTED',HACTION='READ ')
      DO JILU=1,NDIM_FULL
        READ(IUNIT,*) ZF_PARAM(JILU), ZC_DEPTH_RATIO(JILU)
      ENDDO
      CALL CLOSE_FILE('ASCII ',IUNIT)
      CALL READ_AND_SEND_MPI(ZF_PARAM,XF_PARAM,NR_NATURE)
      CALL READ_AND_SEND_MPI(ZC_DEPTH_RATIO,XC_DEPTH_RATIO,NR_NATURE)
      !
    ELSE
      WRITE(ILUOUT,*) "COMPUTE_ISBA_PARAMETERS: WITH CKSAT=EXP, IN NOT OFFLINE "//&
                      "MODE, TOPMODEL FILE FOR F_PARAM IS NOT READ "
    ENDIF
    !
    DO JPATCH=1,NPATCH
      WHERE (XF_PARAM(:)/=XUNDEF)
        ZF(:,JPATCH) = XF_PARAM(:)
      ELSEWHERE
        ZF(:,JPATCH) = 4.0/XDG(:,2,JPATCH)
        ZF(:,JPATCH) = MIN(ZF(:,JPATCH),XF_DECAY)
      ENDWHERE
    ENDDO
    !
    DO JPATCH=1,NPATCH
      CALL EXP_DECAY_SOIL_FR(CISBA, ZF(:,JPATCH),XC1SAT(:,JPATCH),XC2REF(:,JPATCH), &
                             XDG(:,:,JPATCH),XD_ICE(:,JPATCH),XC4REF(:,JPATCH),   &
                             XC3(:,:,JPATCH),XCONDSAT(:,:,JPATCH),                &
                             XKSAT_ICE(:,JPATCH))  
    ENDDO    
    !
    DEALLOCATE(ZF)
    !    
  ENDIF
  ! 
ENDIF
!
!
!*       2.10   Soil carbon
!               -----------                        
!
IF (HINIT == 'ALL' .AND. CRESPSL=='CNT' .AND. CPHOTO == 'NCB') THEN
  CALL CARBON_INIT(NNBIOMASS, NNLITTER, NNLITTLEVS, NNSOILCARB)
ENDIF
!
!Rainfall spatial distribution
!CRAIN used in HYDRO_VEG and HYDRO_SGH and ISBA_SGH_UPDATE
IF(CRAIN=='SGH')THEN
  ALLOCATE(XMUF(KI))
  XMUF(:)=0.0
ELSE
  ALLOCATE(XMUF(0))
ENDIF
!
ALLOCATE(XFSAT(KI))  
XFSAT(:) = 0.0
!
!-------------------------------------------------------------------------------
!
!*       6.2    Initialize of TRIP or ESM coupling:'
!               ------------------------------------
!
IF(LCPL_ESM)THEN
   LTRIP=.TRUE.
   IF(.NOT.LGLACIER)THEN
     CALL ABOR1_SFX('LGLACIER MUST BE ACTIVATED WITH EARTH SYSTEM MODEL')
   ENDIF
ENDIF
!
IF(LGLACIER)THEN
   ALLOCATE(XCPL_ICEFLUX(KI))
   XCPL_ICEFLUX(:) = 0.0
ELSE
   ALLOCATE(XCPL_ICEFLUX(0))
ENDIF
!
IF(LTRIP)THEN
!        
  ALLOCATE(XCPL_DRAIN (KI))
  ALLOCATE(XCPL_RUNOFF(KI))
  XCPL_DRAIN  = 0.0
  XCPL_RUNOFF = 0.0
!
  IF(LFLOOD)THEN
    !
    ALLOCATE(XFFLOOD      (KI))
    ALLOCATE(XPIFLOOD     (KI))
    ALLOCATE(XCPL_EFLOOD  (KI))
    ALLOCATE(XCPL_PFLOOD  (KI))
    ALLOCATE(XCPL_IFLOOD  (KI))
    ALLOCATE(XFF          (KI,NPATCH))
    ALLOCATE(XFFG         (KI,NPATCH))
    ALLOCATE(XFFV         (KI,NPATCH))  
    ALLOCATE(XFFROZEN     (KI,NPATCH))  
    ALLOCATE(XALBF        (KI,NPATCH))  
    ALLOCATE(XEMISF       (KI,NPATCH))  
    XFFLOOD       = 0.0
    XPIFLOOD      = 0.0
    XCPL_EFLOOD   = 0.0
    XCPL_PFLOOD   = 0.0
    XCPL_IFLOOD   = 0.0
    XFF           = 0.0
    XFFG          = 0.0
    XFFV          = 0.0
    XFFROZEN      = 0.0
    XALBF         = 0.0
    XEMISF        = 0.0
  ELSE
    ALLOCATE(XFFLOOD      (0))
    ALLOCATE(XPIFLOOD     (0))
    ALLOCATE(XCPL_EFLOOD  (0))
    ALLOCATE(XCPL_PFLOOD  (0))
    ALLOCATE(XCPL_IFLOOD  (0))
    ALLOCATE(XFF        (0,0))
    ALLOCATE(XFFG       (0,0))
    ALLOCATE(XFFV       (0,0))
    ALLOCATE(XFFROZEN   (0,0))
    ALLOCATE(XALBF      (0,0))  
    ALLOCATE(XEMISF     (0,0))      
  ENDIF
  !
ELSE
!        
  ALLOCATE(XCPL_DRAIN (0))
  ALLOCATE(XCPL_RUNOFF(0))   
!  
ENDIF
!
!-------------------------------------------------------------------------------
!
!*      7.     ISBA time-varying deep force-restore temperature initialization
!              ---------------------------------------------------------------
!
 CALL SOILTEMP_ARP_PAR(HPROGRAM,LTEMP_ARP,NTEMPLAYER_ARP)
!
!-------------------------------------------------------------------------------
!
!*       9.     Prints of cover parameters in a tex file
!               ----------------------------------------
!
 CALL WRITE_COVER_TEX_ISBA    (NPATCH,NGROUND_LAYER,CISBA)
 CALL WRITE_COVER_TEX_ISBA_PAR(NPATCH,NGROUND_LAYER,CISBA,CPHOTO,XSOILGRID)
!
!* if only physiographic fields are to be initialized, stop here.
!
IF (HINIT/='ALL') THEN
  IF (LHOOK) CALL DR_HOOK('COMPUTE_ISBA_PARAMETERS',1,ZHOOK_HANDLE)
  RETURN
END IF
!
!-------------------------------------------------------------------------------
!
 CALL INIT_IO_SURF_n(HPROGRAM,'NATURE','ISBA  ','READ ')
!
!*      10.     Prognostic and semi-prognostic fields
!               -------------------------------------
!
 CALL READ_ISBA_n(HPROGRAM)
!
IF (HINIT=='PRE' .AND. TSNOW%SCHEME.NE.'3-L' .AND. TSNOW%SCHEME.NE.'CRO' .AND. CISBA=='DIF') THEN
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
   CALL INIT_ISBA_LANDUSE(HPROGRAM)  
END IF
!
!-------------------------------------------------------------------------------
!
!*      12.     Canopy air fields:
!               -----------------
!
 CALL READ_ISBA_CANOPY_n(HPROGRAM)
!
!-------------------------------------------------------------------------------
!
ALLOCATE(XDIR_ALB_WITH_SNOW(KI,KSW,NPATCH))
ALLOCATE(XSCA_ALB_WITH_SNOW(KI,KSW,NPATCH))
XDIR_ALB_WITH_SNOW = 0.0
XSCA_ALB_WITH_SNOW = 0.0
!
!-------------------------------------------------------------------------------
!
 CALL INIT_VEG_n(NPATCH, KI, LCANOPY, CROUGH, TSNOW, &
                   CPHOTO, XLAIMIN, XH_TREE, XVEGTYPE_PATCH, XLAI, XZ0, XVEG, XEMIS, &
                   LTR_ML, XFAPARC, XFAPIRC, XLAI_EFFC, XMUS, &
                   XALBNIR_SOIL, XALBVIS_SOIL, XALBUV_SOIL, XALBNIR, XALBVIS, XALBUV, &
                   LSURF_DIAG_ALBEDO, XPSN, XPSNG, XPSNV, XPSNV_A, &
                   PDIR_ALB, PSCA_ALB, PEMIS, PTSRAD ) 
!
!
ALLOCATE(ZWG1(KI,NPATCH))
ALLOCATE(ZTG1(KI,NPATCH))
DO JPATCH=1,NPATCH
  ZWG1(:,JPATCH) = XWG(:,1,JPATCH)
  ZTG1(:,JPATCH) = XTG(:,1,JPATCH)
END DO
!
 CALL CONVERT_PATCH_ISBA(CISBA,IDECADE,IDECADE2,XCOVER,CPHOTO,LAGRIP,LPERM,'NAT',&
                          PWG1 = ZWG1, &
                          PALBNIR_SOIL=XALBNIR_SOIL, &
                          PALBVIS_SOIL=XALBVIS_SOIL, &
                          PALBUV_SOIL=XALBUV_SOIL )
!
DEALLOCATE(ZWG1)
!
ALLOCATE(XEMIS_NAT   (KI))
XEMIS_NAT (:) = XUNDEF
!
 CALL AVERAGED_ALBEDO_EMIS_ISBA(LFLOOD, CALBEDO, PZENITH,                 &
                                 XVEG,XZ0,XLAI,ZTG1,                     &
                                 XPATCH,                                 &
                                 PSW_BANDS,                              &
                                 XALBNIR_VEG,XALBVIS_VEG,XALBUV_VEG,     &
                                 XALBNIR_SOIL,XALBVIS_SOIL,XALBUV_SOIL,  &
                                 XEMIS,                                  &
                                 TSNOW,                                  &
                                 XALBNIR,XALBVIS,XALBUV,                 &
                                 PDIR_ALB, PSCA_ALB,                     &
                                 XEMIS_NAT,ZTSRAD_NAT                    )  
!
PEMIS  = XEMIS_NAT
PTSRAD = ZTSRAD_NAT
!
DEALLOCATE(ZTG1)
!-------------------------------------------------------------------------------
!
!*      15.     ISBA diagnostics initialization
!               -------------------------------
!
IF(NPATCH<=1) LPATCH_BUDGET=.FALSE.
!
 CALL DIAG_ISBA_INIT_n(HPROGRAM,KI,KSW)
!
!-------------------------------------------------------------------------------
!
 CALL INIT_SURF_TOPD(HPROGRAM,NDIM_FULL)
!
!-------------------------------------------------------------------------------
!
!         End of IO
!
 CALL END_IO_SURF_n(HPROGRAM)
IF (LHOOK) CALL DR_HOOK('COMPUTE_ISBA_PARAMETERS',1,ZHOOK_HANDLE)
!
END SUBROUTINE COMPUTE_ISBA_PARAMETERS


