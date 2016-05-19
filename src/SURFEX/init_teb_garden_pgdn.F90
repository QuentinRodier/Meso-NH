!#############################################################
SUBROUTINE INIT_TEB_GARDEN_PGD_n(HPROGRAM,HINIT, OREAD_PGD,KI, KSV, HSV, KVERSION, KBUGFIX, &
        PCO2, PRHOA)
!#############################################################
!
!!****  *INIT_TEB_GARDEN_PGD_n* - routine to initialize ISBA
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
USE MODD_TEB_n,           ONLY: TTIME, XGARDEN
USE MODD_TEB_VEG_n,       ONLY: CISBA, CPEDOTF, CPHOTO, CSCOND, LTR_ML, NNBIOMASS,  &
                                CCPSURF, CKSAT, CSOC
USE MODD_TEB_GARDEN_n,    ONLY: LSTRESS, XPCPS, XPLVTT, XPLSTT,                     &
                                XCLAY, XSAND, XWWILT, XWFC, XWSAT,                  &
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
                                XT1AMAX, XT2AMAX, XAH, XBH,              &
                                XCGSAT, XC1SAT, XC2REF, XC3, XC4B, XACOEF, XPCOEF,  &
                                XTAUICE, XACOEF, XPCOEF, XBCOEF, XCONDSAT, &
                                XHCAPSOIL, XCONDDRY, XCONDSLD, XC4REF, XMPOTSAT,    &
                                XTDEEP, XGAMMAT, NGROUND_LAYER, XSOILWGHT,          &
                                XCE_NITRO, XCF_NITRO, NLAYER_HORT, NLAYER_DUN,      &
                                XCNA_NITRO, XBSLAI_NITRO,                           &
                                XD_ICE, XKSAT_ICE,                                  &
                                LPAR_GARDEN      
USE MODD_CH_TEB_n,        ONLY: CSV, CCH_NAMES, NBEQ, NSV_CHSBEG, NSV_CHSEND,       &
                                CCHEM_SURF_FILE, NDSTEQ, NSV_DSTBEG, NSV_DSTEND,    &
                                NSV_AERBEG, NSV_AEREND, NAEREQ, CDSTNAMES,          &
                                CAER_NAMES, NSLTEQ, NSV_SLTBEG,                     &
                                NSV_SLTEND, CSLTNAMES, CCH_DRY_DEP, LCH_BIO_FLUX 
                                
USE MODD_DATA_COVER_PAR,  ONLY: NVEGTYPE
USE MODD_SURF_PAR,        ONLY: XUNDEF, NUNDEF

USE MODD_SGH_PAR,         ONLY: NDIMTAB, XF_DECAY
!
USE MODI_GET_LUOUT
USE MODI_ALLOCATE_TEB_GARDEN_PGD
USE MODI_READ_PGD_TEB_GARDEN_n
USE MODI_CONVERT_PATCH_GARDEN
USE MODI_INIT_FROM_DATA_GRDN_n
USE MODI_INIT_VEG_PGD_GARDEN_n
USE MODI_EXP_DECAY_SOIL_DIF
USE MODI_EXP_DECAY_SOIL_FR
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
INTEGER,                            INTENT(IN)  :: KBUGFIX
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
INTEGER :: JVEGTYPE, JLAYER  ! loop counter on vegtypes
!
REAL, DIMENSION(KI)               :: ZF
REAL, DIMENSION(KI)               :: ZWORK
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!               Initialisation for IO
!
IF (LHOOK) CALL DR_HOOK('INIT_TEB_GARDEN_PGD_n',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
!-------------------------------------------------------------------------------
!
!*       2.     Physiographic fields
!               --------------------
!
!* allocation of urban green area variables
!
 CALL ALLOCATE_TEB_GARDEN_PGD(OREAD_PGD, KI, NVEGTYPE, NGROUND_LAYER, NDIMTAB)  
!
!
!*       2.1    Cover, soil and orographic fields:
!               ---------------------------------
!
IF (OREAD_PGD) &
 CALL READ_PGD_TEB_GARDEN_n(HPROGRAM,KVERSION,KBUGFIX)
!
!
!*       2.3    Physiographic data fields from land cover:
!               -----------------------------------------
!
IF (TTIME%TDATE%MONTH /= NUNDEF) THEN
  IDECADE = 3 * ( TTIME%TDATE%MONTH - 1 ) + MIN(TTIME%TDATE%DAY-1,29) / 10 + 1
ELSE
  IDECADE = 1
END IF
!
!
IF (.NOT. LPAR_GARDEN) THEN
  CALL CONVERT_PATCH_GARDEN(KI,IDECADE)
ELSE
 CALL INIT_FROM_DATA_GRDN_n(IDECADE,CPHOTO,                     &
                            XVEG,                               &
                            XLAI,XRSMIN,XGAMMA,XWRMAX_CF,       &
                            XRGL,XCV,XDG,XD_ICE,XZ0,XZ0_O_Z0H,  &
                            XALBNIR_VEG,XALBVIS_VEG,            &
                            XALBUV_VEG,XEMIS,                   &
                            XVEGTYPE,XROOTFRAC,                 &
                            XGMES,XBSLAI,XLAIMIN,XSEFOLD,XGC,   &
                            XDMAX, XF2I, LSTRESS, XH_TREE,XRE25,&
                            XCE_NITRO,XCF_NITRO,XCNA_NITRO      )  

  IF (CISBA=='DIF') THEN
    WHERE(XGARDEN(:)/=0.)
      NWG_LAYER(:)=NGROUND_LAYER 
      XDG2  (:)=0.0
      XDROOT(:)=0.0
    ENDWHERE
    DO JLAYER=NGROUND_LAYER,1,-1
      DO JILU=1,KI
        IF(XGARDEN(JILU)/=0..AND.XROOTFRAC(JILU,JLAYER)>=1.0)THEN
          XDG2  (JILU)=XDG(JILU,JLAYER)
          XDROOT(JILU)=XDG(JILU,JLAYER)
        ENDIF
      ENDDO
    ENDDO
  ENDIF

END IF
!

WHERE (XGARDEN(:)==0.)
  XVEG(:)=0.
  XLAI(:)=0.
  XRSMIN(:)=40.
  XGAMMA(:)=0.
  XWRMAX_CF(:)=0.2
  XRGL(:)=100.
  XCV(:)=2.E-5
  XZ0(:)=0.013
  XZ0_O_Z0H(:)=10.
  XALBNIR_VEG(:)=0.30
  XALBVIS_VEG(:)=0.30
  XALBUV_VEG(:)=0.06
  XEMIS(:)=0.94
ENDWHERE  
IF (CPHOTO/='NON') THEN
  WHERE (XGARDEN(:)==0.)
    XGMES(:)=0.020
    XBSLAI(:)=0.36
    XLAIMIN(:)=0.3
    XSEFOLD(:)=90*86400.
    XH_TREE(:)=0.
    XRE25(:)=3.6E-7
    XGC(:)=0.00025
  END WHERE
  IF (CPHOTO/='AGS' .AND. CPHOTO/='LAI') THEN
    WHERE (XGARDEN(:)==0.) 
      XDMAX(:)=0.1
      XF2I(:)=0.3
    END WHERE
    IF (CPHOTO=='NIT' .OR. CPHOTO=='NCB') THEN
      WHERE (XGARDEN(:)==0.)      
        XCE_NITRO(:)=7.68
        XCF_NITRO(:)=-4.33
        XCNA_NITRO(:)=1.3
      END WHERE
    ENDIF
  ENDIF
ENDIF
IF(CISBA/='DIF')THEN
  DO JLAYER=1,NGROUND_LAYER
    WHERE (XGARDEN(:)==0.)
      XDG(:,JLAYER)=0.2*JLAYER
    END WHERE
  ENDDO
ELSE
  WHERE (XGARDEN(:)==0.) 
    XDG(:,1)=0.01
    XDG(:,2)=0.04
    XROOTFRAC(:,1)=0.
    XROOTFRAC(:,2)=0.
  END WHERE        
  DO JLAYER=3,NGROUND_LAYER
    WHERE (XGARDEN(:)==0.)
      XDG(:,JLAYER)=0.1*(JLAYER-2)
      XROOTFRAC(:,JLAYER)=0.
    END WHERE
  ENDDO               
  WHERE (XGARDEN(:)==0.) 
    NWG_LAYER(:)=NGROUND_LAYER
    XDROOT   (:)=0.0
    XDG2     (:)=XDG(:,NGROUND_LAYER-1)
  ENDWHERE    
ENDIF  
WHERE (XGARDEN(:)==0.) 
  XD_ICE(:)=0.8*XDG(:,2)
END WHERE  
DO JVEGTYPE=1,NVEGTYPE
  WHERE (XGARDEN(:)==0.)
    XVEGTYPE(:,JVEGTYPE)=0.
    XVEGTYPE(:,1)=1.
  END WHERE
ENDDO
!
 CALL INIT_VEG_PGD_GARDEN_n(HPROGRAM, ILUOUT, KI, NGROUND_LAYER, TTIME%TDATE%MONTH,    &
                        XVEGTYPE, XTDEEP, XGAMMAT, CPHOTO, HINIT, LTR_ML,           &
                        NNBIOMASS, PCO2, PRHOA, XABC, XPOI,                         &
                        XGMES, XGC, XDMAX, XANMAX, XFZERO, XEPSO, XGAMM, XQDGAMM,   &
                        XQDGMES, XT1GMES, XT2GMES, XAMAX, XQDAMAX, XT1AMAX, XT2AMAX,&
                        XAH, XBH,                                                   &
                        KSV, HSV, NBEQ, CSV, NAEREQ, NSV_CHSBEG, NSV_CHSEND,        &
                        NSV_AERBEG, NSV_AEREND, CCH_NAMES, CAER_NAMES, NDSTEQ,      &
                        NSV_DSTBEG, NSV_DSTEND, NSLTEQ, NSV_SLTBEG, NSV_SLTEND,     &
                        CDSTNAMES, CSLTNAMES, CCHEM_SURF_FILE,                      &
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
IF(CISBA=='DIF'.AND.CSOC=='SGH')THEN
  CALL ABOR1_SFX('INIT_TEB_GARDEN_PGDn: SUBGRID Soil organic matter'//&
                 ' effect (CSOC) NOT YET IMPLEMENTED FOR GARDEN')
ELSEIF (CISBA=='3-L'.AND.CKSAT=='EXP') THEN 
  CALL ABOR1_SFX('INIT_TEB_GARDEN_PGDn: topmodel exponential decay not implemented for garden')
ENDIF
!
IF(CKSAT=='SGH' .AND. HINIT/='PRE')THEN 
  !
  ZF (:) = XUNDEF
  !  
  !Soil organic carbon effect and/or Exponential decay for DIF option 
  IF(CISBA=='DIF') THEN
    ZWORK(:) = XUNDEF
    ZF(:) = 4.0/MERGE(XDROOT(:),XDG2(:),XDROOT(:)>0.0) 
  ELSE
    WHERE (ZF(:)==XUNDEF) ZF(:) =  4.0/XDG(:,2)
  ENDIF
  ZF(:)=MIN(ZF(:),XF_DECAY)
  !
  IF(CISBA=='DIF') THEN
    !   
    ZWORK(:) = MERGE(XDROOT(:),XDG2(:),XDROOT(:)>0.0)      
    CALL EXP_DECAY_SOIL_DIF(ZF(:),XDG(:,:),NWG_LAYER(:),ZWORK(:),XCONDSAT(:,:))   
    !Exponential decay for ISBA-FR option
  ELSE
    !
    CALL EXP_DECAY_SOIL_FR(CISBA, ZF(:),XC1SAT(:),XC2REF(:),XDG(:,:),XD_ICE(:),&
                           XC4REF(:),XC3(:,:),XCONDSAT(:,:),XKSAT_ICE(:))  
    ! 
  ENDIF
  !
ENDIF
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('INIT_TEB_GARDEN_PGD_n',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
!
END SUBROUTINE INIT_TEB_GARDEN_PGD_n
