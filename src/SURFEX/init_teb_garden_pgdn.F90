!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!#############################################################
        SUBROUTINE INIT_TEB_GARDEN_PGD_n (DTCO, U, CHI, DTI, I, DST, SLT, CHT, TG, T, TOP, GDM, &
                                          HPROGRAM,HINIT, OREAD_PGD,KI, KSV, HSV, KVERSION, KBUGFIX, &
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
        USE MODD_SURFEX_n, ONLY : TEB_GARDEN_MODEL_t
        !
        USE MODD_TYPE_DATE_SURF
        USE MODD_TYPE_SNOW
        !
                                        
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
        TYPE(TEB_GARDEN_MODEL_t), INTENT(INOUT) :: GDM
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
         CALL ALLOCATE_TEB_GARDEN_PGD(GDM%TGDPE, GDM%TGDP, &
                                      OREAD_PGD, KI, NVEGTYPE, GDM%TGDO%NGROUND_LAYER, NDIMTAB)  
        !
        !
        !*       2.1    Cover, soil and orographic fields:
        !               ---------------------------------
        !
        IF (OREAD_PGD) &
         CALL READ_PGD_TEB_GARDEN_n(CHT, DTCO, GDM%DTGD, GDM%GBGD, U, GDM%TGDO, GDM%TGDP, TG, TOP, &
                                    HPROGRAM,KVERSION,KBUGFIX)
        !
        !
        !*       2.3    Physiographic data fields from land cover:
        !               -----------------------------------------
        !
        IF (TOP%TTIME%TDATE%MONTH /= NUNDEF) THEN
          IDECADE = 3 * ( TOP%TTIME%TDATE%MONTH - 1 ) + MIN(TOP%TTIME%TDATE%DAY-1,29) / 10 + 1
        ELSE
          IDECADE = 1
        END IF
        !
        !
        IF (.NOT. GDM%TGDO%LPAR_GARDEN) THEN
          CALL CONVERT_PATCH_GARDEN(DTCO, DTI, I, GDM%TGDO, GDM%TGDPE, GDM%TGDP, TOP, GDM%TVG, &
                                    KI,IDECADE)
        ELSE
         CALL INIT_FROM_DATA_GRDN_n(GDM%DTGD, &
                                    IDECADE,GDM%TVG%CPHOTO, GDM%TGDPE%CUR%XVEG, &
                                    GDM%TGDPE%CUR%XLAI,GDM%TGDP%XRSMIN,GDM%TGDP%XGAMMA,&
                                    GDM%TGDP%XWRMAX_CF, GDM%TGDP%XRGL,GDM%TGDP%XCV,GDM%TGDP%XDG,&
                                    GDM%TGDP%XD_ICE,GDM%TGDPE%CUR%XZ0,GDM%TGDP%XZ0_O_Z0H,&
                                    GDM%TGDP%XALBNIR_VEG,GDM%TGDP%XALBVIS_VEG,     &
                                    GDM%TGDP%XALBUV_VEG,GDM%TGDPE%CUR%XEMIS,      &
                                    GDM%TGDP%XVEGTYPE,GDM%TGDP%XROOTFRAC,GDM%TGDP%XGMES,&
                                    GDM%TGDP%XBSLAI,GDM%TGDP%XLAIMIN,GDM%TGDP%XSEFOLD,GDM%TGDP%XGC,   &
                                    GDM%TGDP%XDMAX, GDM%TGDP%XF2I, GDM%TGDP%LSTRESS, GDM%TGDP%XH_TREE,&
                                    GDM%TGDP%XRE25,GDM%TGDP%XCE_NITRO,GDM%TGDP%XCF_NITRO,GDM%TGDP%XCNA_NITRO      )  

          IF (GDM%TVG%CISBA=='DIF') THEN
            WHERE(T%CUR%XGARDEN(:)/=0.)
              GDM%TGDP%NWG_LAYER(:)=GDM%TGDO%NGROUND_LAYER 
              GDM%TGDP%XDG2  (:)=0.0
              GDM%TGDP%XDROOT(:)=0.0
            ENDWHERE
            DO JLAYER=GDM%TGDO%NGROUND_LAYER,1,-1
              DO JILU=1,KI
                IF(T%CUR%XGARDEN(JILU)/=0..AND.GDM%TGDP%XROOTFRAC(JILU,JLAYER)>=1.0)THEN
                  GDM%TGDP%XDG2  (JILU)=GDM%TGDP%XDG(JILU,JLAYER)
                  GDM%TGDP%XDROOT(JILU)=GDM%TGDP%XDG(JILU,JLAYER)
                ENDIF
              ENDDO
            ENDDO
          ENDIF

        END IF
        !

        WHERE (T%CUR%XGARDEN(:)==0.)
          GDM%TGDPE%CUR%XVEG(:)=0.
          GDM%TGDPE%CUR%XLAI(:)=0.
          GDM%TGDP%XRSMIN(:)=40.
          GDM%TGDP%XGAMMA(:)=0.
          GDM%TGDP%XWRMAX_CF(:)=0.2
          GDM%TGDP%XRGL(:)=100.
          GDM%TGDP%XCV(:)=2.E-5
          GDM%TGDPE%CUR%XZ0(:)=0.013
          GDM%TGDP%XZ0_O_Z0H(:)=10.
          GDM%TGDP%XALBNIR_VEG(:)=0.30
          GDM%TGDP%XALBVIS_VEG(:)=0.30
          GDM%TGDP%XALBUV_VEG(:)=0.06
          GDM%TGDPE%CUR%XEMIS(:)=0.94
        ENDWHERE  
        IF (GDM%TVG%CPHOTO/='NON') THEN
          WHERE (T%CUR%XGARDEN(:)==0.)
            GDM%TGDP%XGMES(:)=0.020
            GDM%TGDP%XBSLAI(:)=0.36
            GDM%TGDP%XLAIMIN(:)=0.3
            GDM%TGDP%XSEFOLD(:)=90*86400.
            GDM%TGDP%XH_TREE(:)=0.
            GDM%TGDP%XRE25(:)=3.6E-7
            GDM%TGDP%XGC(:)=0.00025
          END WHERE
          IF (GDM%TVG%CPHOTO/='AGS' .AND. GDM%TVG%CPHOTO/='LAI') THEN
            WHERE (T%CUR%XGARDEN(:)==0.) 
              GDM%TGDP%XDMAX(:)=0.1
              GDM%TGDP%XF2I(:)=0.3
            END WHERE
            IF (GDM%TVG%CPHOTO=='NIT' .OR. GDM%TVG%CPHOTO=='NCB') THEN
              WHERE (T%CUR%XGARDEN(:)==0.)      
                GDM%TGDP%XCE_NITRO(:)=7.68
                GDM%TGDP%XCF_NITRO(:)=-4.33
                GDM%TGDP%XCNA_NITRO(:)=1.3
              END WHERE
            ENDIF
          ENDIF
        ENDIF
        IF(GDM%TVG%CISBA/='DIF')THEN
          DO JLAYER=1,GDM%TGDO%NGROUND_LAYER
            WHERE (T%CUR%XGARDEN(:)==0.)
              GDM%TGDP%XDG(:,JLAYER)=0.2*JLAYER
            END WHERE
          ENDDO
        ELSE
          WHERE (T%CUR%XGARDEN(:)==0.) 
            GDM%TGDP%XDG(:,1)=0.01
            GDM%TGDP%XDG(:,2)=0.04
            GDM%TGDP%XROOTFRAC(:,1)=0.
            GDM%TGDP%XROOTFRAC(:,2)=0.
          END WHERE        
          DO JLAYER=3,GDM%TGDO%NGROUND_LAYER
            WHERE (T%CUR%XGARDEN(:)==0.)
              GDM%TGDP%XDG(:,JLAYER)=0.1*(JLAYER-2)
              GDM%TGDP%XROOTFRAC(:,JLAYER)=0.
            END WHERE
          ENDDO               
          WHERE (T%CUR%XGARDEN(:)==0.) 
            GDM%TGDP%NWG_LAYER(:)=GDM%TGDO%NGROUND_LAYER
            GDM%TGDP%XDROOT   (:)=0.0
            GDM%TGDP%XDG2     (:)=GDM%TGDP%XDG(:,GDM%TGDO%NGROUND_LAYER-1)
          ENDWHERE    
        ENDIF  
        WHERE (T%CUR%XGARDEN(:)==0.) 
          GDM%TGDP%XD_ICE(:)=0.8*GDM%TGDP%XDG(:,2)
        END WHERE  
        DO JVEGTYPE=1,NVEGTYPE
          WHERE (T%CUR%XGARDEN(:)==0.)
            GDM%TGDP%XVEGTYPE(:,JVEGTYPE)=0.
            GDM%TGDP%XVEGTYPE(:,1)=1.
          END WHERE
        ENDDO
        !
         CALL INIT_VEG_PGD_GARDEN_n(CHI, DTCO, DST, I, SLT, U, &
                            HPROGRAM, ILUOUT, KI, GDM%TGDO%NGROUND_LAYER, TOP%TTIME%TDATE%MONTH,    &
                        GDM%TGDP%XVEGTYPE, GDM%TGDP%XTDEEP, GDM%TGDP%XGAMMAT, GDM%TVG%CPHOTO, HINIT, &
                        GDM%TVG%LTR_ML, GDM%TVG%CRUNOFF, GDM%TVG%NNBIOMASS, PCO2, PRHOA, &
                        GDM%TGDP%XABC, GDM%TGDP%XPOI, GDM%TGDP%XGMES, GDM%TGDP%XGC, GDM%TGDP%XDMAX, &
                        GDM%TGDP%XANMAX, GDM%TGDP%XFZERO, GDM%TGDP%XEPSO, GDM%TGDP%XGAMM, GDM%TGDP%XQDGAMM,   &
                        GDM%TGDP%XQDGMES, GDM%TGDP%XT1GMES, GDM%TGDP%XT2GMES, GDM%TGDP%XAMAX, GDM%TGDP%XQDAMAX, &
                        GDM%TGDP%XT1AMAX, GDM%TGDP%XT2AMAX,GDM%TGDP%XAH, GDM%TGDP%XBH,            &
                        KSV, HSV, CHT%SVT, CHT%CCH_NAMES, CHT%CAER_NAMES,CHT%CDSTNAMES, CHT%CSLTNAMES, &
                        CHT%CCHEM_SURF_FILE, GDM%TGDP%XCLAY, GDM%TGDP%XSAND, GDM%TVG%CPEDOTF,      &
                        GDM%TGDP%XCONDSAT, GDM%TGDP%XMPOTSAT, GDM%TGDP%XBCOEF, GDM%TGDP%XWWILT, &
                        GDM%TGDP%XWFC, GDM%TGDP%XWSAT, GDM%TGDP%XTAUICE, GDM%TGDP%XCGSAT, GDM%TGDP%XC1SAT, &
                        GDM%TGDP%XC2REF, GDM%TGDP%XC3, GDM%TGDP%XC4B, GDM%TGDP%XACOEF, GDM%TGDP%XPCOEF, &
                        GDM%TGDP%XC4REF, GDM%TGDP%XPCPS, GDM%TGDP%XPLVTT, GDM%TGDP%XPLSTT,        &
                        GDM%TVG%CSCOND, GDM%TVG%CISBA, GDM%TGDP%XHCAPSOIL, GDM%TGDP%XCONDDRY, &
                        GDM%TGDP%XCONDSLD, GDM%TVG%CCPSURF, GDM%TGDP%XDG, GDM%TGDP%XDROOT, GDM%TGDP%XDG2, &
                        GDM%TGDP%XROOTFRAC, GDM%TGDP%XRUNOFFD, GDM%TGDP%XDZG, GDM%TGDP%XDZDIF,       &
                        GDM%TGDP%XSOILWGHT, GDM%TGDP%NWG_LAYER, GDM%TGDO%NLAYER_HORT, &
                        GDM%TGDO%NLAYER_DUN, GDM%TGDP%XD_ICE,  &
                        GDM%TGDP%XKSAT_ICE, GDM%TGDP%XALBNIR_DRY, GDM%TGDP%XALBVIS_DRY, GDM%TGDP%XALBUV_DRY,   &
                        GDM%TGDP%XALBNIR_WET, GDM%TGDP%XALBVIS_WET, GDM%TGDP%XALBUV_WET, GDM%TGDP%XBSLAI_NITRO, &
                        GDM%TGDP%XCE_NITRO, GDM%TGDP%XCNA_NITRO, GDM%TGDP%XCF_NITRO                            )
!
!-------------------------------------------------------------------------------
!
IF(GDM%TVG%CISBA=='DIF'.AND.GDM%TVG%LSOC)THEN
  CALL ABOR1_SFX('INIT_TEB_GARDEN_PGDn: SUBGRID Soil organic matter'//&
                 ' effect (LSOC) NOT YET IMPLEMENTED FOR GARDEN')
ELSEIF (GDM%TVG%CISBA=='3-L'.AND.GDM%TVG%CKSAT=='EXP') THEN 
  CALL ABOR1_SFX('INIT_TEB_GARDEN_PGDn: topmodel exponential decay not implemented for garden')
ENDIF
!
IF(GDM%TVG%CKSAT=='SGH' .AND. GDM%TVG%CISBA/='DIF' .AND. HINIT/='PRE')THEN 
  ZF(:)=MIN(4.0/GDM%TGDP%XDG(:,2),XF_DECAY)
  CALL EXP_DECAY_SOIL_FR(GDM%TVG%CISBA, ZF(:),GDM%TGDP%XC1SAT(:),GDM%TGDP%XC2REF(:),&
                         GDM%TGDP%XDG(:,:),GDM%TGDP%XD_ICE(:),GDM%TGDP%XC4REF(:),&
                         GDM%TGDP%XC3(:,:),GDM%TGDP%XCONDSAT(:,:),GDM%TGDP%XKSAT_ICE(:))
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
