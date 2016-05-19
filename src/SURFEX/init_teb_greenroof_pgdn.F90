!#############################################################
SUBROUTINE INIT_TEB_GREENROOF_PGD_n(HPROGRAM,HINIT,OREAD_PGD, KI, KSV, HSV, KVERSION, PCO2, PRHOA)
!#############################################################
!
!!****  *INIT_TEB_GREENROOF_PGD_n* - routine to initialize ISBA
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
!!	A. Lemonsu  *Meteo France*	
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    09/2009
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_TYPE_DATE_SURF
USE MODD_TYPE_SNOW
!
USE MODD_TEB_n,                ONLY: TTIME, XGREENROOF
USE MODD_TEB_VEG_n,       ONLY: CPEDOTF, CPHOTO, NNBIOMASS, CCPSURF
USE MODD_TEB_GREENROOF_n,      ONLY: LSTRESS, XPCPS, XPLVTT, XPLSTT,                     &
                                     CISBA_GR, CSCOND_GR, CKSAT_GR, LTR_ML_GR,           &
                                     XCLAY_GR, XSAND_GR, XOM_GR,                         &
                                     XWWILT, XWFC, XWSAT,                                &
                                     XVEG, XRSMIN, XGAMMA, XRGL, XCV, XLAI,              &
                                     XDG, XZ0, XZ0_O_Z0H, XABC, XPOI,                    &
                                     XALBNIR_VEG, XALBVIS_VEG, XALBUV_VEG,               &
                                     XEMIS, XVEGTYPE, XGMES, XRE25, XBSLAI, XLAIMIN, XGC,&
                                     XDMAX, XF2I, XDG2, XDROOT, NWG_LAYER,               &
                                     XSEFOLD, XH_TREE, XWRMAX_CF, XDZG, XDZDIF,          &
                                     XALBNIR_DRY, XALBVIS_DRY, XALBUV_DRY,               &
                                     XALBNIR_WET, XALBVIS_WET, XALBUV_WET,               &
                                     XALBNIR_SOIL, XALBVIS_SOIL, XALBUV_SOIL,            &
                                     XALBNIR, XALBVIS, XALBUV,                           &
                                     XROOTFRAC,XRUNOFFD, XANMAX, XFZERO, XEPSO, XGAMM,   &
                                     XQDGAMM, XQDGMES, XT1GMES, XT2GMES, XAMAX, XQDAMAX, &
                                     XT1AMAX, XT2AMAX, XAH, XBH,                         &
                                     XCGSAT, XC1SAT, XC2REF, XC3, XC4B, XACOEF, XPCOEF,  &
                                     XTAUICE, XACOEF, XPCOEF, XTAUICE, XBCOEF, XCONDSAT, &
                                     XHCAPSOIL, XCONDDRY, XCONDSLD, XC4REF, XMPOTSAT,    &
                                     XTDEEP, XGAMMAT, NLAYER_GR, XSOILWGHT,              &
                                     XCE_NITRO, XCF_NITRO, NLAYER_HORT_GR, NLAYER_DUN_GR,&
                                     XCNA_NITRO, XBSLAI_NITRO,                           &
                                     XD_ICE, XKSAT_ICE,                                  &
                                     LPAR_GREENROOF      
USE MODD_CH_TEB_n,             ONLY: CSV, CCH_NAMES, NBEQ, NSV_CHSBEG, NSV_CHSEND,       &
                                     CCHEM_SURF_FILE, NDSTEQ, NSV_DSTBEG, NSV_DSTEND,    &
                                     NSV_AERBEG, NSV_AEREND, NAEREQ, CDSTNAMES,          &
                                     CAER_NAMES, NSLTEQ, NSV_SLTBEG,                     &
                                     NSV_SLTEND, CSLTNAMES, CCH_DRY_DEP, LCH_BIO_FLUX  

USE MODD_DATA_COVER_PAR,       ONLY: NVEGTYPE
USE MODD_SURF_PAR,             ONLY: XUNDEF, NUNDEF

USE MODD_SGH_PAR,              ONLY: NDIMTAB, XF_DECAY
!
USE MODI_GET_LUOUT
USE MODI_ALLOCATE_TEB_GREENROOF_PGD
USE MODI_READ_PGD_TEB_GREENROOF_n
USE MODI_CONVERT_PATCH_TEB_GREENROOF
USE MODI_INIT_FROM_DATA_GREENROOF_n
USE MODI_INIT_VEG_PGD_GARDEN_n
USE MODI_EXP_DECAY_SOIL_FR
USE MODI_EXP_DECAY_SOIL_DIF
USE MODI_ABOR1_SFX
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
 CHARACTER(LEN=6),                   INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
 CHARACTER(LEN=3),                   INTENT(IN)  :: HINIT     ! choice of fields to initialize
LOGICAL,                            INTENT(IN)  :: OREAD_PGD ! flag to read PGD fields in the file
INTEGER,                            INTENT(IN)  :: KI        ! number of points
INTEGER,                            INTENT(IN)  :: KSV       ! number of scalars
 CHARACTER(LEN=6), DIMENSION(KSV),   INTENT(IN)  :: HSV       ! name of all scalar variables
INTEGER,                            INTENT(IN)  :: KVERSION  ! version number of the file being read
REAL,             DIMENSION(KI),    INTENT(IN)  :: PCO2        ! CO2 concentration (kg/m3)
REAL,             DIMENSION(KI),    INTENT(IN)  :: PRHOA       ! air density
!
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER           :: JILU     ! loop increment
INTEGER           :: ILUOUT   ! unit of output listing file
!
INTEGER           :: IDECADE  ! decade of simulation
!
INTEGER :: JVEGTYPE, JLAYER  ! loop counter on layers
!
REAL, DIMENSION(KI)               :: ZF
REAL, DIMENSION(KI)               :: ZWORK
!
!*       0.3   Soil parameter values for organic matter - from Lawrence and Slater (2008):
!              ----------------------------------------------------------------------------------
!
REAL, PARAMETER   :: ZWSAT_OM      = 0.9       ! Porosity of OM (m3/m3)
REAL, PARAMETER   :: ZCONDSAT_OM   = 2.8E-4    ! Saturated hydraulic conductivity for OM (m/s)
REAL, PARAMETER   :: ZMPOTSAT_OM   = -10.3E-3  ! Saturated matric potential for OM (m)
REAL, PARAMETER   :: ZBCOEF_OM     = 2.7       ! CH78 b-parameter for OM (-)
!
REAL, PARAMETER   :: ZCONDDRY_OM   = 0.05      ! Dry thermal conductivity for OM (W/m/K)
REAL, PARAMETER   :: ZCONDSLD_OM   = 0.25      ! Soil solids thermal conductivity for OM (W/m/K)
REAL, PARAMETER   :: ZHCAPSOIL_OM  = 2.5E+6    ! Soil heat capacity for OM
!
REAL, PARAMETER   :: ZMPOT_WWILT   = -150.     ! Matric potential at wilting point (m)
REAL, PARAMETER   :: ZHYDCOND_WFC  = 1.157E-9  ! Hydraulic conductivity at field capacity (m/s)
!                                              ! = 0.1 mm/day
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!               Initialisation for IO
!
IF (LHOOK) CALL DR_HOOK('INIT_TEB_GREENROOF_PGD_n',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
!-------------------------------------------------------------------------------
!
!*       2.     Physiographic fields
!               --------------------
!
!
!*       2.1    Cover, soil and orographic fields:
!               ---------------------------------
!
IF (OREAD_PGD) &
 CALL READ_PGD_TEB_GREENROOF_n(HPROGRAM,KVERSION)
!
!
!* allocation of green roofs variables
!
 CALL ALLOCATE_TEB_GREENROOF_PGD(OREAD_PGD, KI, NVEGTYPE, NLAYER_GR, NDIMTAB)
!
!*       2.2    Physiographic data fields from land cover:
!               -----------------------------------------
!
IF (TTIME%TDATE%MONTH /= NUNDEF) THEN
  IDECADE = 3 * ( TTIME%TDATE%MONTH - 1 ) + MIN(TTIME%TDATE%DAY-1,29) / 10 + 1
ELSE
  IDECADE = 1
END IF
!
!
IF (.NOT. LPAR_GREENROOF) THEN
  CALL CONVERT_PATCH_TEB_GREENROOF(KI,IDECADE)
ELSE
 CALL INIT_FROM_DATA_GREENROOF_n(IDECADE,CPHOTO,                     &
                                 XOM_GR,                             &
                                 XSAND_GR, XCLAY_GR, XVEG,           &
                                 XLAI,XRSMIN,XGAMMA,XWRMAX_CF,       &
                                 XRGL,XCV,XDG,XD_ICE,XZ0,XZ0_O_Z0H,  &
                                 XALBNIR_VEG,XALBVIS_VEG,            &
                                 XALBUV_VEG,XEMIS,                   &
                                 XVEGTYPE,XROOTFRAC,                 &
                                 XGMES,XBSLAI,XLAIMIN,XSEFOLD,XGC,   &
                                 XDMAX, XF2I, LSTRESS, XH_TREE,XRE25,&
                                 XCE_NITRO,XCF_NITRO,XCNA_NITRO      )  
  IF (CISBA_GR=='DIF') THEN
    WHERE(XGREENROOF(:)/=0.)
      NWG_LAYER(:)=NLAYER_GR 
      XDG2  (:)=0.0
      XDROOT(:)=0.0
    ENDWHERE
    DO JLAYER=NLAYER_GR,1,-1
      DO JILU=1,KI
        IF(XGREENROOF(JILU)/=0..AND.XROOTFRAC(JILU,JLAYER)>=1.0)THEN
          XDG2  (JILU)=XDG(JILU,JLAYER)
          XDROOT(JILU)=XDG(JILU,JLAYER)
        ENDIF
      ENDDO
    ENDDO
  ENDIF
END IF
!
WHERE (XGREENROOF(:)==0.)
  ! GARDEN default values /may need changing for green roofs
  XOM_GR     (:,1) = 0.5
  XOM_GR     (:,2) = 0.5
  XSAND_GR   (:,1) = 0.33
  XSAND_GR   (:,2) = 0.33
  XCLAY_GR   (:,1) = 0.33
  XCLAY_GR   (:,2) = 0.33
  XVEG       (:  ) = 0.
  XLAI       (:  ) = 0.
  XRSMIN     (:  ) = 40.
  XGAMMA     (:  ) = 0.
  XWRMAX_CF  (:  ) = 0.2
  XRGL       (:  ) = 100.
  XCV        (:  ) = 2.E-5
  XZ0        (:  ) = 0.013
  XZ0_O_Z0H  (:  ) = 10.
  XALBNIR_VEG(:  ) = 0.30
  XALBVIS_VEG(:  ) = 0.30
  XALBUV_VEG (:  ) = 0.06
  XEMIS      (:  ) = 0.94
END WHERE
IF (CPHOTO/='NON') THEN
  WHERE (XGREENROOF(:)==0.)
    XGMES      (:  ) = 0.020
    XBSLAI     (:  ) = 0.36
    XLAIMIN    (:  ) = 0.3
    XSEFOLD    (:  ) = 90*86400.
    XH_TREE    (:  ) = 0.
    XRE25      (:  ) = 3.6E-7    
    XGC        (:  ) = 0.00025
  END WHERE
  IF (CPHOTO/='AGS' .AND. CPHOTO/='LAI') THEN
    WHERE (XGREENROOF(:)==0.)     
      XDMAX      (:  ) = 0.1
      XF2I       (:  ) = 0.3
    END WHERE
    IF (CPHOTO=='NIT' .OR. CPHOTO=='NCB') THEN
      WHERE (XGREENROOF(:)==0.)          
        XCE_NITRO  (:  ) = 7.68
        XCF_NITRO  (:  ) = -4.33
        XCNA_NITRO (:  ) = 1.3
      END WHERE
    ENDIF
  ENDIF
ENDIF  
IF(CISBA_GR/='DIF')THEN
  DO JLAYER=1,NLAYER_GR
    WHERE (XGREENROOF(:)==0.)
      XDG(:,JLAYER)=0.2*JLAYER
    END WHERE
  ENDDO
ELSE
  WHERE (XGREENROOF(:)==0.) 
    XDG(:,1)=0.01
    XDG(:,2)=0.04
    XROOTFRAC(:,1)=0.
    XROOTFRAC(:,2)=0.
  END WHERE        
  DO JLAYER=3,NLAYER_GR
    WHERE (XGREENROOF(:)==0.)
      XDG(:,JLAYER)=0.1*(JLAYER-2)
      XROOTFRAC(:,JLAYER)=0.
    END WHERE
  ENDDO               
  WHERE (XGREENROOF(:)==0.) 
    NWG_LAYER(:)=NLAYER_GR
    XDROOT   (:)=0.0
    XDG2     (:)=XDG(:,NLAYER_GR-1)
  ENDWHERE    
ENDIF  
WHERE (XGREENROOF(:)==0.) 
  XD_ICE(:)=0.8*XDG(:,2)
END WHERE  
DO JVEGTYPE=1,NVEGTYPE
  WHERE (XGREENROOF(:)==0.)
    XVEGTYPE(:,JVEGTYPE)=0.
    XVEGTYPE(:,1)=1.
  END WHERE
ENDDO
!
 CALL INIT_VEG_PGD_GARDEN_n(HPROGRAM, ILUOUT, KI, NLAYER_GR, TTIME%TDATE%MONTH,    &
                        XVEGTYPE, XTDEEP, XGAMMAT, CPHOTO, HINIT, LTR_ML_GR,        &
                        NNBIOMASS, PCO2, PRHOA, XABC, XPOI,                         &
                        XGMES, XGC, XDMAX, XANMAX, XFZERO, XEPSO, XGAMM, XQDGAMM,   &
                        XQDGMES, XT1GMES, XT2GMES, XAMAX, XQDAMAX, XT1AMAX, XT2AMAX,&
                        XAH, XBH,                                                   &
                        KSV, HSV, NBEQ, CSV, NAEREQ, NSV_CHSBEG, NSV_CHSEND,        &
                        NSV_AERBEG, NSV_AEREND, CCH_NAMES, CAER_NAMES, NDSTEQ,      &
                        NSV_DSTBEG, NSV_DSTEND, NSLTEQ, NSV_SLTBEG, NSV_SLTEND,     &
                        CDSTNAMES, CSLTNAMES, CCHEM_SURF_FILE,                      &
                        XCLAY_GR, XSAND_GR, CPEDOTF,                                &
                        XCONDSAT, XMPOTSAT, XBCOEF, XWWILT, XWFC, XWSAT,            &
                        XTAUICE, XCGSAT, XC1SAT, XC2REF, XC3, XC4B, XACOEF, XPCOEF, &
                        XC4REF, XPCPS, XPLVTT, XPLSTT,                              &
                        CSCOND_GR, CISBA_GR, XHCAPSOIL, XCONDDRY, XCONDSLD, CCPSURF,&
                        XDG, XDROOT, XDG2, XROOTFRAC, XRUNOFFD, XDZG, XDZDIF,       &
                        XSOILWGHT, NWG_LAYER, NLAYER_HORT_GR, NLAYER_DUN_GR, XD_ICE,&
                        XKSAT_ICE, XALBNIR_DRY, XALBVIS_DRY, XALBUV_DRY,            &
                        XALBNIR_WET, XALBVIS_WET, XALBUV_WET, XBSLAI_NITRO,         &
                        XCE_NITRO, XCNA_NITRO, XCF_NITRO                            )
!
!-------------------------------------------------------------------------------
!
!*       5.1     Soil thermal characteristics for greenroofs:
!               ----------------------------------------------
!
! WARNING: must be done before soil hydraulic characteristics (because of WSAT)
! Estimation of WSAT_MI for use in HEATCAPZ and THRMCONDZ for mineral fraction
! and allow weighted combination with regard to OM & no-OM fractions:
!
IF (CSCOND_GR=='PL98' .OR. CISBA_GR=='DIF') THEN
  DO JLAYER=1,NLAYER_GR
     XHCAPSOIL(:,JLAYER) =    XOM_GR(:,JLAYER)  * ZHCAPSOIL_OM +      &
                           (1-XOM_GR(:,JLAYER)) * XHCAPSOIL(:,JLAYER)  
  ENDDO
ELSE
END IF
!
IF (CSCOND_GR=='PL98') THEN
  DO JLAYER=1,NLAYER_GR
     XCONDDRY(:,JLAYER) = (ZCONDDRY_OM         * XCONDDRY(:,JLAYER))    &
                         /(  XOM_GR(:,JLAYER)  * XCONDDRY(:,JLAYER) +   &
                          (1-XOM_GR(:,JLAYER)) * ZCONDDRY_OM)
     XCONDSLD(:,JLAYER) = (ZCONDSLD_OM         * XCONDSLD(:,JLAYER))    &
                         /(  XOM_GR(:,JLAYER)  * XCONDSLD(:,JLAYER) +   &
                          (1-XOM_GR(:,JLAYER)) * ZCONDSLD_OM)
  ENDDO
ELSE
END IF
!
! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! Validation case : experimental values for Nancy 2011 case
! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! Substrate layer
DO JLAYER=1,4
    XCONDDRY (:,JLAYER) = 0.15
    XHCAPSOIL(:,JLAYER) = 1342000.
ENDDO
! Drainage layer
DO JLAYER=5,6
    XCONDDRY (:,JLAYER) = 0.09
    XHCAPSOIL(:,JLAYER) = 331500.
ENDDO
! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!*       5.2     Soil thermal characteristics:
!               --------------------------------
!
DO JLAYER=1,NLAYER_GR
  XCONDSAT(:,JLAYER) =   XOM_GR(:,JLAYER)* ZCONDSAT_OM   &
                        +(1-XOM_GR(:,JLAYER))* XCONDSAT(:,JLAYER)
END DO
!
! Note that if ISBA/=DIF, always CDIF = 'BC' and CPEDOTF = 'CH78'
DO JLAYER=1,NLAYER_GR
  XBCOEF  (:,JLAYER) =    XOM_GR(:,JLAYER) * ZBCOEF_OM        &
                       +(1-XOM_GR(:,JLAYER))* XBCOEF(:,JLAYER)
  XMPOTSAT(:,JLAYER) =    XOM_GR(:,JLAYER) * ZMPOTSAT_OM      &
                       +(1-XOM_GR(:,JLAYER))* XMPOTSAT(:,JLAYER)
END DO
!        
DO JLAYER=1,NLAYER_GR
   XWSAT (:,JLAYER) =    XOM_GR(:,JLAYER)* ZWSAT_OM            &
                     +(1-XOM_GR(:,JLAYER))* XWSAT(:,JLAYER)
   XWWILT(:,JLAYER) = EXP(((LOG(-1*ZMPOT_WWILT)-LOG(-1*XMPOTSAT(:,JLAYER)))   &
                    / (-1*XBCOEF(:,JLAYER)))+LOG(XWSAT(:,JLAYER)))
   XWFC  (:,JLAYER) = EXP(((LOG(ZHYDCOND_WFC)-LOG(XCONDSAT(:,JLAYER)))        &
                    / (2*XBCOEF(:,JLAYER)+3))+LOG(XWSAT(:,JLAYER)))
END DO
!
! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! Validation case : experimental values for Nancy 2011 case
! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! Substrate layer
DO JLAYER=1,4
  XWSAT   (:,JLAYER) = 0.674     ! Value tested
  XCONDSAT(:,JLAYER) = 2.162E-3  ! Value tested
  XMPOTSAT(:,JLAYER) = -0.932    ! Value tested
  XBCOEF  (:,JLAYER) = 3.9       ! Value tested
  XWWILT  (:,JLAYER) = 0.15      ! from OBS-NANCY
  XWFC    (:,JLAYER) = 0.37      ! from OBS-NANCY
ENDDO
! Drainage layer
DO JLAYER=5,6
   XWSAT   (:,JLAYER) = 0.9       ! Value tested
   XCONDSAT(:,JLAYER) = 3.32E-3   ! Value tested
   XMPOTSAT(:,JLAYER) = -0.121    ! Value tested
   XBCOEF  (:,JLAYER) = 2.7       ! Value tested
   XWWILT  (:,JLAYER) = 0.15      ! sert à initialiser le WG ds la couche
   XWFC    (:,JLAYER) = 0.37      ! sert à initialiser le WG ds la couche
ENDDO
!-------------------------------------------------------------------------------
!
!*       6.1    Initialize of the SGH scheme:'
!               ------------------------------
!
IF(CKSAT_GR=='SGH' .AND. HINIT/='PRE')THEN 
  !
  ZF (:) = XUNDEF
  !  
  !Soil organic carbon effect and/or Exponential decay for DIF option 
  IF(CISBA_GR=='DIF') THEN
    ZWORK(:) = XUNDEF
    ZF(:) = 4.0/MERGE(XDROOT(:),XDG2(:),XDROOT(:)>0.0) 
  ELSE
    WHERE (ZF(:)==XUNDEF) ZF(:) =  4.0/XDG(:,2)
  ENDIF
  ZF(:)=MIN(ZF(:),XF_DECAY)
  !
  IF(CISBA_GR=='DIF') THEN
    !   
    ZWORK(:) = MERGE(XDROOT(:),XDG2(:),XDROOT(:)>0.0)      
    CALL EXP_DECAY_SOIL_DIF(ZF(:),XDG(:,:),NWG_LAYER(:),ZWORK(:),XCONDSAT(:,:))   
    !Exponential decay for ISBA-FR option
  ELSE
    !
    CALL EXP_DECAY_SOIL_FR(CISBA_GR, ZF(:),XC1SAT(:),XC2REF(:),XDG(:,:),XD_ICE(:),&
                           XC4REF(:),XC3(:,:),XCONDSAT(:,:),XKSAT_ICE(:))  
    ! 
  ENDIF
  !
ENDIF
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('INIT_TEB_GREENROOF_PGD_n',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
!
END SUBROUTINE INIT_TEB_GREENROOF_PGD_n
