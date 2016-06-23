!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!#############################################################
SUBROUTINE INIT_TEB_GREENROOF_PGD_n (DTCO, U, CHI, DTI, I, DST, SLT, CHT, TG, T, TOP, TVG, GRM, &
                                     HPROGRAM,HINIT,OREAD_PGD, KI, KSV, HSV, KVERSION, PCO2, PRHOA)
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
!!      A. Lemonsu  *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    09/2009
!!                  11/2013 (B. Decharme) No exp profile with DIF
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_CH_ISBA_n, ONLY : CH_ISBA_t
USE MODD_DATA_ISBA_n, ONLY : DATA_ISBA_t
USE MODD_ISBA_n, ONLY : ISBA_t
USE MODD_DST_n, ONLY : DST_t
USE MODD_SLT_n, ONLY : SLT_t
USE MODD_CH_TEB_n, ONLY : CH_TEB_t
USE MODD_TEB_GRID_n, ONLY : TEB_GRID_t
USE MODD_TEB_n, ONLY : TEB_t
USE MODD_TEB_OPTION_n, ONLY : TEB_OPTIONS_t
USE MODD_TEB_VEG_n, ONLY : TEB_VEG_OPTIONS_t
USE MODD_SURFEX_n, ONLY : TEB_GREENROOF_MODEL_t
!
USE MODD_TYPE_DATE_SURF
USE MODD_TYPE_SNOW
!


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
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(CH_ISBA_t), INTENT(INOUT) :: CHI
TYPE(DATA_ISBA_t), INTENT(INOUT) :: DTI
TYPE(ISBA_t), INTENT(INOUT) :: I
TYPE(DST_t), INTENT(INOUT) :: DST
TYPE(SLT_t), INTENT(INOUT) :: SLT
TYPE(CH_TEB_t), INTENT(INOUT) :: CHT
TYPE(TEB_GRID_t), INTENT(INOUT) :: TG
TYPE(TEB_t), INTENT(INOUT) :: T
TYPE(TEB_OPTIONS_t), INTENT(INOUT) :: TOP
TYPE(TEB_VEG_OPTIONS_t), INTENT(INOUT) :: TVG
TYPE(TEB_GREENROOF_MODEL_t), INTENT(INOUT) :: GRM
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
 CALL READ_PGD_TEB_GREENROOF_n(CHT, DTCO, GRM%DTGR, GRM%GBGR, U, GRM%TGRO, GRM%TGRP, TG, &
                               HPROGRAM,KVERSION)
!
!
!* allocation of green roofs variables
!
 CALL ALLOCATE_TEB_GREENROOF_PGD(GRM%TGRPE, GRM%TGRP, &
                                 OREAD_PGD, KI, NVEGTYPE, GRM%TGRO%NLAYER_GR, NDIMTAB)
!
!*       2.2    Physiographic data fields from land cover:
!               -----------------------------------------
!
IF (TOP%TTIME%TDATE%MONTH /= NUNDEF) THEN
  IDECADE = 3 * ( TOP%TTIME%TDATE%MONTH - 1 ) + MIN(TOP%TTIME%TDATE%DAY-1,29) / 10 + 1
ELSE
  IDECADE = 1
END IF
!
!
IF (.NOT. GRM%TGRO%LPAR_GREENROOF) THEN
  CALL CONVERT_PATCH_TEB_GREENROOF(DTCO, DTI, I, GRM%TGRO, GRM%TGRPE, GRM%TGRP, TOP, TVG, &
                                   KI,IDECADE)
ELSE
 CALL INIT_FROM_DATA_GREENROOF_n(GRM%DTGR, GRM%TGRO, IDECADE,TVG%CPHOTO,        &
                                 GRM%TGRP%XOM_GR, GRM%TGRP%XSAND_GR, GRM%TGRP%XCLAY_GR, &
                                 GRM%TGRPE%CUR%XVEG, GRM%TGRPE%CUR%XLAI,GRM%TGRP%XRSMIN, &
                                 GRM%TGRP%XGAMMA,GRM%TGRP%XWRMAX_CF,GRM%TGRP%XRGL,GRM%TGRP%XCV,&
                                 GRM%TGRP%XDG,GRM%TGRP%XD_ICE,GRM%TGRPE%CUR%XZ0,GRM%TGRP%XZ0_O_Z0H,  &
                                 GRM%TGRP%XALBNIR_VEG,GRM%TGRP%XALBVIS_VEG,GRM%TGRP%XALBUV_VEG,&
                                 GRM%TGRPE%CUR%XEMIS,GRM%TGRP%XVEGTYPE,GRM%TGRP%XROOTFRAC,          &
                                 GRM%TGRP%XGMES,GRM%TGRP%XBSLAI,GRM%TGRP%XLAIMIN,GRM%TGRP%XSEFOLD,GRM%TGRP%XGC,   &
                                 GRM%TGRP%XDMAX, GRM%TGRP%XF2I, GRM%TGRP%LSTRESS, GRM%TGRP%XH_TREE,GRM%TGRP%XRE25,&
                                 GRM%TGRP%XCE_NITRO,GRM%TGRP%XCF_NITRO,GRM%TGRP%XCNA_NITRO      )  
  IF (GRM%TGRO%CISBA_GR=='DIF') THEN
    WHERE(T%CUR%XGREENROOF(:)/=0.)
      GRM%TGRP%NWG_LAYER(:)=GRM%TGRO%NLAYER_GR 
      GRM%TGRP%XDG2  (:)=0.0
      GRM%TGRP%XDROOT(:)=0.0
    ENDWHERE
    DO JLAYER=GRM%TGRO%NLAYER_GR,1,-1
      DO JILU=1,KI
        IF(T%CUR%XGREENROOF(JILU)/=0..AND.GRM%TGRP%XROOTFRAC(JILU,JLAYER)>=1.0)THEN
          GRM%TGRP%XDG2  (JILU)=GRM%TGRP%XDG(JILU,JLAYER)
          GRM%TGRP%XDROOT(JILU)=GRM%TGRP%XDG(JILU,JLAYER)
        ENDIF
      ENDDO
    ENDDO
  ENDIF
END IF
!
WHERE (T%CUR%XGREENROOF(:)==0.)
  ! GARDEN default values /may need changing for green roofs
  GRM%TGRP%XOM_GR     (:,1) = 0.5
  GRM%TGRP%XOM_GR     (:,2) = 0.5
  GRM%TGRP%XSAND_GR   (:,1) = 0.33
  GRM%TGRP%XSAND_GR   (:,2) = 0.33
  GRM%TGRP%XCLAY_GR   (:,1) = 0.33
  GRM%TGRP%XCLAY_GR   (:,2) = 0.33
  GRM%TGRPE%CUR%XVEG       (:  ) = 0.
  GRM%TGRPE%CUR%XLAI       (:  ) = 0.
  GRM%TGRP%XRSMIN     (:  ) = 40.
  GRM%TGRP%XGAMMA     (:  ) = 0.
  GRM%TGRP%XWRMAX_CF  (:  ) = 0.2
  GRM%TGRP%XRGL       (:  ) = 100.
  GRM%TGRP%XCV        (:  ) = 2.E-5
  GRM%TGRPE%CUR%XZ0        (:  ) = 0.013
  GRM%TGRP%XZ0_O_Z0H  (:  ) = 10.
  GRM%TGRP%XALBNIR_VEG(:  ) = 0.30
  GRM%TGRP%XALBVIS_VEG(:  ) = 0.30
  GRM%TGRP%XALBUV_VEG (:  ) = 0.06
  GRM%TGRPE%CUR%XEMIS      (:  ) = 0.94
END WHERE
IF (TVG%CPHOTO/='NON') THEN
  WHERE (T%CUR%XGREENROOF(:)==0.)
    GRM%TGRP%XGMES      (:  ) = 0.020
    GRM%TGRP%XBSLAI     (:  ) = 0.36
    GRM%TGRP%XLAIMIN    (:  ) = 0.3
    GRM%TGRP%XSEFOLD    (:  ) = 90*86400.
    GRM%TGRP%XH_TREE    (:  ) = 0.
    GRM%TGRP%XRE25      (:  ) = 3.6E-7    
    GRM%TGRP%XGC        (:  ) = 0.00025
  END WHERE
  IF (TVG%CPHOTO/='AGS' .AND. TVG%CPHOTO/='LAI') THEN
    WHERE (T%CUR%XGREENROOF(:)==0.)     
      GRM%TGRP%XDMAX      (:  ) = 0.1
      GRM%TGRP%XF2I       (:  ) = 0.3
    END WHERE
    IF (TVG%CPHOTO=='NIT' .OR. TVG%CPHOTO=='NCB') THEN
      WHERE (T%CUR%XGREENROOF(:)==0.)          
        GRM%TGRP%XCE_NITRO  (:  ) = 7.68
        GRM%TGRP%XCF_NITRO  (:  ) = -4.33
        GRM%TGRP%XCNA_NITRO (:  ) = 1.3
      END WHERE
    ENDIF
  ENDIF
ENDIF  
IF(GRM%TGRO%CISBA_GR/='DIF')THEN
  DO JLAYER=1,GRM%TGRO%NLAYER_GR
    WHERE (T%CUR%XGREENROOF(:)==0.)
      GRM%TGRP%XDG(:,JLAYER)=0.2*JLAYER
    END WHERE
  ENDDO
ELSE
  WHERE (T%CUR%XGREENROOF(:)==0.) 
    GRM%TGRP%XDG(:,1)=0.01
    GRM%TGRP%XDG(:,2)=0.04
    GRM%TGRP%XROOTFRAC(:,1)=0.
    GRM%TGRP%XROOTFRAC(:,2)=0.
  END WHERE        
  DO JLAYER=3,GRM%TGRO%NLAYER_GR
    WHERE (T%CUR%XGREENROOF(:)==0.)
      GRM%TGRP%XDG(:,JLAYER)=0.1*(JLAYER-2)
      GRM%TGRP%XROOTFRAC(:,JLAYER)=0.
    END WHERE
  ENDDO               
  WHERE (T%CUR%XGREENROOF(:)==0.) 
    GRM%TGRP%NWG_LAYER(:)=GRM%TGRO%NLAYER_GR
    GRM%TGRP%XDROOT   (:)=0.0
    GRM%TGRP%XDG2     (:)=GRM%TGRP%XDG(:,GRM%TGRO%NLAYER_GR-1)
  ENDWHERE    
ENDIF  
WHERE (T%CUR%XGREENROOF(:)==0.) 
  GRM%TGRP%XD_ICE(:)=0.8*GRM%TGRP%XDG(:,2)
END WHERE  
DO JVEGTYPE=1,NVEGTYPE
  WHERE (T%CUR%XGREENROOF(:)==0.)
    GRM%TGRP%XVEGTYPE(:,JVEGTYPE)=0.
    GRM%TGRP%XVEGTYPE(:,1)=1.
  END WHERE
ENDDO
!
 CALL INIT_VEG_PGD_GARDEN_n(CHI, DTCO, DST, I, SLT, U, &
                            HPROGRAM, ILUOUT, KI, GRM%TGRO%NLAYER_GR, TOP%TTIME%TDATE%MONTH,    &
                        GRM%TGRP%XVEGTYPE, GRM%TGRP%XTDEEP, GRM%TGRP%XGAMMAT, TVG%CPHOTO, HINIT, &
                        GRM%TGRO%LTR_ML_GR, GRM%TGRO%CRUNOFF_GR, TVG%NNBIOMASS, PCO2, PRHOA, &
                        GRM%TGRP%XABC, GRM%TGRP%XPOI, GRM%TGRP%XGMES, GRM%TGRP%XGC, GRM%TGRP%XDMAX, &
                        GRM%TGRP%XANMAX, GRM%TGRP%XFZERO, GRM%TGRP%XEPSO, GRM%TGRP%XGAMM, GRM%TGRP%XQDGAMM,   &
                        GRM%TGRP%XQDGMES, GRM%TGRP%XT1GMES, GRM%TGRP%XT2GMES, GRM%TGRP%XAMAX, GRM%TGRP%XQDAMAX, &
                        GRM%TGRP%XT1AMAX, GRM%TGRP%XT2AMAX,GRM%TGRP%XAH, GRM%TGRP%XBH,            &
                        KSV, HSV, CHT%SVT, CHT%CCH_NAMES, CHT%CAER_NAMES,CHT%CDSTNAMES, CHT%CSLTNAMES, &
                        CHT%CCHEM_SURF_FILE, GRM%TGRP%XCLAY_GR, GRM%TGRP%XSAND_GR, TVG%CPEDOTF,      &
                        GRM%TGRP%XCONDSAT, GRM%TGRP%XMPOTSAT, GRM%TGRP%XBCOEF, GRM%TGRP%XWWILT, &
                        GRM%TGRP%XWFC, GRM%TGRP%XWSAT, GRM%TGRP%XTAUICE, GRM%TGRP%XCGSAT, GRM%TGRP%XC1SAT, &
                        GRM%TGRP%XC2REF, GRM%TGRP%XC3, GRM%TGRP%XC4B, GRM%TGRP%XACOEF, GRM%TGRP%XPCOEF, &
                        GRM%TGRP%XC4REF, GRM%TGRP%XPCPS, GRM%TGRP%XPLVTT, GRM%TGRP%XPLSTT,        &
                        GRM%TGRO%CSCOND_GR, GRM%TGRO%CISBA_GR, GRM%TGRP%XHCAPSOIL, GRM%TGRP%XCONDDRY, &
                        GRM%TGRP%XCONDSLD, TVG%CCPSURF, GRM%TGRP%XDG, GRM%TGRP%XDROOT, GRM%TGRP%XDG2, &
                        GRM%TGRP%XROOTFRAC, GRM%TGRP%XRUNOFFD, GRM%TGRP%XDZG, GRM%TGRP%XDZDIF,       &
                        GRM%TGRP%XSOILWGHT, GRM%TGRP%NWG_LAYER, GRM%TGRO%NLAYER_HORT_GR, &
                        GRM%TGRO%NLAYER_DUN_GR, GRM%TGRP%XD_ICE,  &
                        GRM%TGRP%XKSAT_ICE, GRM%TGRP%XALBNIR_DRY, GRM%TGRP%XALBVIS_DRY, GRM%TGRP%XALBUV_DRY,   &
                        GRM%TGRP%XALBNIR_WET, GRM%TGRP%XALBVIS_WET, GRM%TGRP%XALBUV_WET, GRM%TGRP%XBSLAI_NITRO, &
                        GRM%TGRP%XCE_NITRO, GRM%TGRP%XCNA_NITRO, GRM%TGRP%XCF_NITRO                            )
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
IF (GRM%TGRO%CSCOND_GR=='PL98' .OR. GRM%TGRO%CISBA_GR=='DIF') THEN
  DO JLAYER=1,GRM%TGRO%NLAYER_GR
     GRM%TGRP%XHCAPSOIL(:,JLAYER) =    GRM%TGRP%XOM_GR(:,JLAYER)  * ZHCAPSOIL_OM +      &
                           (1-GRM%TGRP%XOM_GR(:,JLAYER)) * GRM%TGRP%XHCAPSOIL(:,JLAYER)  
     GRM%TGRP%XCONDDRY(:,JLAYER) = (ZCONDDRY_OM         * GRM%TGRP%XCONDDRY(:,JLAYER))    &
                         /(  GRM%TGRP%XOM_GR(:,JLAYER)  * GRM%TGRP%XCONDDRY(:,JLAYER) +   &
                          (1-GRM%TGRP%XOM_GR(:,JLAYER)) * ZCONDDRY_OM)
     GRM%TGRP%XCONDSLD(:,JLAYER) = (ZCONDSLD_OM         * GRM%TGRP%XCONDSLD(:,JLAYER))    &
                         /(  GRM%TGRP%XOM_GR(:,JLAYER)  * GRM%TGRP%XCONDSLD(:,JLAYER) +   &
                          (1-GRM%TGRP%XOM_GR(:,JLAYER)) * ZCONDSLD_OM)
  ENDDO
END IF
!
! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! Validation case : experimental values for Nancy 2011 case
! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! Substrate layer
DO JLAYER=1,4
    GRM%TGRP%XCONDDRY (:,JLAYER) = 0.15
    GRM%TGRP%XHCAPSOIL(:,JLAYER) = 1342000.
ENDDO
! Drainage layer
DO JLAYER=5,6
    GRM%TGRP%XCONDDRY (:,JLAYER) = 0.09
    GRM%TGRP%XHCAPSOIL(:,JLAYER) = 331500.
ENDDO
! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
!*       5.2     Soil thermal characteristics:
!               --------------------------------
!
DO JLAYER=1,GRM%TGRO%NLAYER_GR
  GRM%TGRP%XCONDSAT(:,JLAYER) =   GRM%TGRP%XOM_GR(:,JLAYER)* ZCONDSAT_OM   &
                        +(1-GRM%TGRP%XOM_GR(:,JLAYER))* GRM%TGRP%XCONDSAT(:,JLAYER)
END DO
!
! Note that if ISBA/=DIF, always CDIF = 'BC' and CPEDOTF = 'CH78'
DO JLAYER=1,GRM%TGRO%NLAYER_GR
  GRM%TGRP%XBCOEF  (:,JLAYER) =    GRM%TGRP%XOM_GR(:,JLAYER) * ZBCOEF_OM        &
                       +(1-GRM%TGRP%XOM_GR(:,JLAYER))* GRM%TGRP%XBCOEF(:,JLAYER)
  GRM%TGRP%XMPOTSAT(:,JLAYER) =    GRM%TGRP%XOM_GR(:,JLAYER) * ZMPOTSAT_OM      &
                       +(1-GRM%TGRP%XOM_GR(:,JLAYER))* GRM%TGRP%XMPOTSAT(:,JLAYER)
END DO
!        
DO JLAYER=1,GRM%TGRO%NLAYER_GR
   GRM%TGRP%XWSAT (:,JLAYER) =    GRM%TGRP%XOM_GR(:,JLAYER)* ZWSAT_OM            &
                     +(1-GRM%TGRP%XOM_GR(:,JLAYER))* GRM%TGRP%XWSAT(:,JLAYER)
   GRM%TGRP%XWWILT(:,JLAYER) = EXP(((LOG(-1*ZMPOT_WWILT)-LOG(-1*GRM%TGRP%XMPOTSAT(:,JLAYER)))   &
                    / (-1*GRM%TGRP%XBCOEF(:,JLAYER)))+LOG(GRM%TGRP%XWSAT(:,JLAYER)))
   GRM%TGRP%XWFC  (:,JLAYER) = EXP(((LOG(ZHYDCOND_WFC)-LOG(GRM%TGRP%XCONDSAT(:,JLAYER)))        &
                    / (2*GRM%TGRP%XBCOEF(:,JLAYER)+3))+LOG(GRM%TGRP%XWSAT(:,JLAYER)))
END DO
!
! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! Validation case : experimental values for Nancy 2011 case
! +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! Substrate layer
DO JLAYER=1,4
  GRM%TGRP%XWSAT   (:,JLAYER) = 0.674     ! Value tested
  GRM%TGRP%XCONDSAT(:,JLAYER) = 2.162E-3  ! Value tested
  GRM%TGRP%XMPOTSAT(:,JLAYER) = -0.932    ! Value tested
  GRM%TGRP%XBCOEF  (:,JLAYER) = 3.9       ! Value tested
  GRM%TGRP%XWWILT  (:,JLAYER) = 0.15      ! from OBS-NANCY
  GRM%TGRP%XWFC    (:,JLAYER) = 0.37      ! from OBS-NANCY
ENDDO
! Drainage layer
DO JLAYER=5,6
   GRM%TGRP%XWSAT   (:,JLAYER) = 0.9       ! Value tested
   GRM%TGRP%XCONDSAT(:,JLAYER) = 3.32E-3   ! Value tested
   GRM%TGRP%XMPOTSAT(:,JLAYER) = -0.121    ! Value tested
   GRM%TGRP%XBCOEF  (:,JLAYER) = 2.7       ! Value tested
   GRM%TGRP%XWWILT  (:,JLAYER) = 0.15      ! sert à initialiser le WG ds la couche
   GRM%TGRP%XWFC    (:,JLAYER) = 0.37      ! sert à initialiser le WG ds la couche
ENDDO
!-------------------------------------------------------------------------------
!
!*       6.1    Initialize of the SGH scheme:'
!               ------------------------------
!
IF(GRM%TGRO%CKSAT_GR=='SGH' .AND. GRM%TGRO%CISBA_GR/='DIF' .AND. HINIT/='PRE')THEN 
  ZF(:)=MIN(4.0/GRM%TGRP%XDG(:,2),XF_DECAY)
  CALL EXP_DECAY_SOIL_FR(GRM%TGRO%CISBA_GR, ZF(:),GRM%TGRP%XC1SAT(:),GRM%TGRP%XC2REF(:),&
                         GRM%TGRP%XDG(:,:),GRM%TGRP%XD_ICE(:),GRM%TGRP%XC4REF(:),&
                         GRM%TGRP%XC3(:,:),GRM%TGRP%XCONDSAT(:,:),GRM%TGRP%XKSAT_ICE(:))
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
