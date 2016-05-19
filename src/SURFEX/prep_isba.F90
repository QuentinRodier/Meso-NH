!     #########
SUBROUTINE PREP_ISBA(HPROGRAM,HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE)
!     #################################################################################
!
!!****  *PREP_ISBA* - Prepares ISBA fields
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
!!      Modified by P. Le Moigne (11/2004): AGS fields
!!      Modified by B. Decharme   (2008)  : Floodplains
!!      Modified by B. Decharme  (01/2009): Consistency with Arpege deep soil
!!                                          temperature
!!      Modified by B. Decharme  (03/2009): Consistency with Arpege permanent
!!                                          snow/ice treatment
!!      A.L. Gibelin 04/2009 : BIOMASS and RESP_BIOMASS arrays 
!!      A.L. Gibelin 06/2009 : Soil carbon variables for CNT option
!!      Modified by S. Riette    (06/2009): PREP_ISBA_CANOPY has no more arg.
!!      Modified by S. Riette    (04/2010): ecmwf ice content is computed during
!!                                          grib reading (no longer here)
!!      B. Decharme  (10/2012): coherence between soil temp and liquid/solid water with DIF
!!                              bug in biomass prognostic fields calculation
!!      B. Decharme  (06/2013): XPSNV_A for EBA snow scheme not allocated
!!------------------------------------------------------------------
!
!
USE MODI_PREP_HOR_ISBA_FIELD
USE MODI_PREP_VER_ISBA
USE MODI_PREP_OUTPUT_GRID
USE MODI_GET_LUOUT
USE MODI_PREP_ISBA_CANOPY
!
USE MODD_READ_NAMELIST,  ONLY : LNAM_READ
USE MODD_SURF_ATM,       ONLY : LVERTSHIFT
USE MODD_DATA_COVER_PAR, ONLY : NVT_SNOW
!
USE MODD_ISBA_n,      ONLY : TSNOW, XRESA, XTSRAD_NAT, XEMIS, XLAI, XVEG,  &
                              XZ0, XALBNIR_VEG, XALBVIS_VEG, XALBUV_VEG,     &
                              CPHOTO, CRESPSL, XAN, XANFM, XANDAY, XLE,      &
                              NNBIOMASS, NNLITTER, NNLITTLEVS, NNSOILCARB,   &
                              XBSLAI, XBSLAI_NITRO, XBIOMASS, XRESP_BIOMASS, &
                              XLITTER, XSOILCARB, XLIGNIN_STRUC,             &
                              NPATCH, XWSAT, XWG, XWGI, CISBA, XTG, LCANOPY, &
                              LFLOOD, XZ0_FLOOD, XPATCH, CALBEDO,            &
                              XVEGTYPE_PATCH, LGLACIER, XICE_STO,            &
                              XPSN, XPSNG, XPSNV, XDIR_ALB_WITH_SNOW,        &
                              XSCA_ALB_WITH_SNOW, NGROUND_LAYER, XMPOTSAT,   &
                              XBCOEF, XPSNV_A
!                           
USE MODD_DEEPSOIL,    ONLY : LPHYSDOMC
USE MODD_CSTS,        ONLY : XTT, XG, XLMTT
USE MODD_SNOW_PAR,    ONLY : XZ0SN, XEMISSN
USE MODD_ISBA_PAR,    ONLY : XWGMIN
!
USE MODD_ISBA_GRID_n, ONLY : CGRID, XGRID_PAR, XLAT, XLON
USE MODD_CO2V_PAR,    ONLY : XANFMINIT, XCA_NIT, XCC_NIT
USE MODD_SURF_PAR,    ONLY : XUNDEF
USE MODD_PREP,        ONLY : XZS_LS
!
USE MODN_PREP_ISBA
!
USE MODI_VEGTYPE_TO_PATCH
USE MODI_PREP_PERM_SNOW
USE MODI_INIT_SNOW_LW
USE MODI_AVERAGED_ALBEDO_EMIS_ISBA
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_CLEAN_PREP_OUTPUT_GRID
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
 CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
 CHARACTER(LEN=28),  INTENT(IN)  :: HATMFILE    ! name of the Atmospheric file
 CHARACTER(LEN=6),   INTENT(IN)  :: HATMFILETYPE! type of the Atmospheric file
 CHARACTER(LEN=28),  INTENT(IN)  :: HPGDFILE    ! name of the Atmospheric file
 CHARACTER(LEN=6),   INTENT(IN)  :: HPGDFILETYPE! type of the Atmospheric file
!
!*      0.2    declarations of local variables
!
INTEGER :: ILUOUT, INI
INTEGER :: JP, JL, JJ
INTEGER :: ISNOW          ! patch number where permanent snow is
REAL    :: ZWORK, ZLOG, ZWTOT, ZMATPOT
!
REAL,             DIMENSION(1)   :: ZSW_BANDS ! middle wavelength of each band
REAL,             DIMENSION(SIZE(XLAI,1),SIZE(XLAI,2)) :: ZDIR_ALB  ! direct albedo for each band
REAL,             DIMENSION(SIZE(XLAI,1),SIZE(XLAI,2)) :: ZSCA_ALB  ! diffuse albedo for each band
REAL,             DIMENSION(SIZE(XLAI,1))   :: ZEMIS     ! emissivity
REAL,             DIMENSION(SIZE(XLAI,1))   :: ZZENITH   ! solar zenithal angle
REAL,             DIMENSION(SIZE(XLAI,1),SIZE(XLAI,2)) :: ZALBNIR  ! near-infra-red albedo   (-)
REAL,             DIMENSION(SIZE(XLAI,1),SIZE(XLAI,2)) :: ZALBVIS  ! visible albedo          (-)
REAL,             DIMENSION(SIZE(XLAI,1),SIZE(XLAI,2)) :: ZALBUV   ! UV albedo               (-)
REAL,             DIMENSION(SIZE(XLAI,1),SIZE(XLAI,2)) :: ZALBNIR_SOIL  ! soil near-infra-red albedo   (-)
REAL,             DIMENSION(SIZE(XLAI,1),SIZE(XLAI,2)) :: ZALBVIS_SOIL  ! soil visible albedo          (-)
REAL,             DIMENSION(SIZE(XLAI,1),SIZE(XLAI,2)) :: ZALBUV_SOIL   ! soil UV albedo               (-)
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------
!
!*      1.     Default of configuration
!
!*      1.1    Default
!
!
IF (LHOOK) CALL DR_HOOK('PREP_ISBA',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
 CALL PREP_OUTPUT_GRID(ILUOUT,CGRID,XGRID_PAR,XLAT,XLON)
!
!-------------------------------------------------------------------------------------
!
!*      2.     Reading and horizontal interpolations
!
!
!*      2.0    Large scale orography
!
 CALL PREP_HOR_ISBA_FIELD(HPROGRAM,'ZS     ',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE)
!
!*      2.1    Soil Water reservoirs
!
 CALL PREP_HOR_ISBA_FIELD(HPROGRAM,'WG     ',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE)
!
!*      2.2    Soil ice reservoirs
!
 CALL PREP_HOR_ISBA_FIELD(HPROGRAM,'WGI    ',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE)
!
!*      2.3    Leaves interception water reservoir
!
 CALL PREP_HOR_ISBA_FIELD(HPROGRAM,'WR     ',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE)
!
!*      2.4    Temperature profile
!
 CALL PREP_HOR_ISBA_FIELD(HPROGRAM,'TG     ',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE)
!
!*      2.5    Snow variables
!
 CALL PREP_HOR_ISBA_FIELD(HPROGRAM,'SN_VEG ',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE)
!
!*      2.6    LAI
!
 CALL PREP_HOR_ISBA_FIELD(HPROGRAM,'LAI    ',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE)
!
!-------------------------------------------------------------------------------------
 CALL CLEAN_PREP_OUTPUT_GRID
!-------------------------------------------------------------------------------------
!
!*      3.    Physical limitation: 
!
! No ice for force restore third layer:
IF (CISBA == '3-L') THEN
   DO JP=1,NPATCH
      WHERE(XWG(:,3,JP) /= XUNDEF)
        XWG(:,3,JP)  = MIN(XWG(:,3,JP)+XWGI(:,3,JP),XWSAT(:,3))
        XWGI(:,3,JP) = 0.
      END WHERE
   ENDDO
ENDIF
!
! Total water content should not exceed saturation:
DO JP=1,NPATCH
   WHERE(XWG(:,:,JP) /= XUNDEF .AND. (XWG(:,:,JP) + XWGI(:,:,JP)) > XWSAT(:,:) )
      XWGI(:,:,JP) = XWSAT(:,:) - XWG(:,:,JP)
   END WHERE
ENDDO
!
!-------------------------------------------------------------------------------------
!
!*      3.     Vertical interpolations of all variables
!
IF(LVERTSHIFT)THEN
  CALL PREP_VER_ISBA
ENDIF
!
DEALLOCATE(XZS_LS)
!-------------------------------------------------------------------------------------
!
!*      4.     Treatment of permanent snow
!
IF(LGLACIER)THEN
  ALLOCATE(XICE_STO(SIZE(XLAI,1),SIZE(XLAI,2)))
  XICE_STO(:,:) = 0.0
ENDIF
!
ISNOW = VEGTYPE_TO_PATCH(NVT_SNOW,NPATCH)
 CALL PREP_PERM_SNOW(TSNOW,XTG(:,:,ISNOW),XVEGTYPE_PATCH(:,:,ISNOW),ISNOW)
 CALL INIT_SNOW_LW(XEMISSN,TSNOW)
!
IF (LPHYSDOMC) THEN
   TSNOW%WSNOW(:,:,:)=0.
ENDIF
!
!-------------------------------------------------------------------------------------
!
!*      5.     coherence between soil temperature and liquid/solid water
!
IF (CISBA == 'DIF') THEN
   INI=SIZE(XWSAT,1)
   DO JP=1,NPATCH
      DO JL=1,NGROUND_LAYER
         DO JJ=1,INI
            IF(XWG(JJ,JL,JP)/=XUNDEF)THEN
!     
!             total soil moisture
              ZWTOT = XWG(JJ,JL,JP)+XWGI(JJ,JL,JP)
              ZWTOT = MIN(ZWTOT,XWSAT(JJ,JL))
!              
!             total matric potential
!             psi=mpotsat*(w/wsat)**(-bcoef)
              ZWORK   = ZWTOT/XWSAT(JJ,JL)
              ZLOG    = XBCOEF(JJ,JL)*LOG(ZWORK)
              ZMATPOT = XMPOTSAT(JJ,JL)*EXP(-ZLOG)
!
!             soil liquid water content computation
!             w=wsat*(psi/mpotsat)**(-1/bcoef)
              ZMATPOT       = ZMATPOT + XLMTT*MIN(0.0,XTG(JJ,JL,JP)-XTT)/(XG*XTG(JJ,JL,JP))        
              ZWORK         = MAX(1.0,ZMATPOT/XMPOTSAT(JJ,JL))
              ZLOG          = LOG(ZWORK)
              XWG(JJ,JL,JP) = XWSAT(JJ,JL)*EXP(-ZLOG/XBCOEF(JJ,JL))
              XWG(JJ,JL,JP) = MAX(XWGMIN,XWG(JJ,JL,JP))
!        
!             soil ice computation    
              XWGI(JJ,JL,JP) = MAX(0.0,ZWTOT-XWG(JJ,JL,JP))
! 
!             supress numerical artefact
              IF(XTG(JJ,JL,JP)>=XTT)THEN
                XWG (JJ,JL,JP) = MIN(XWG(JJ,JL,JP)+XWGI(JJ,JL,JP),XWSAT(JJ,JL))
                XWGI(JJ,JL,JP) = 0.0
              ENDIF
!
            ENDIF
        ENDDO        
      ENDDO        
   ENDDO
ENDIF
!
!-------------------------------------------------------------------------------------
!
!*      6.     Half prognostic fields
!
ALLOCATE(XRESA(SIZE(XLAI,1),SIZE(XLAI,2)))
XRESA = 100.
!
ALLOCATE(XTSRAD_NAT(SIZE(XLAI,1)))
ZALBNIR_SOIL(:,:)=0.
ZALBVIS_SOIL(:,:)=0.
ZALBUV_SOIL(:,:)=0.
ZZENITH(:)=0.
ZSW_BANDS(:)=0.
ALLOCATE(XPSN   (SIZE(XLAI,1),SIZE(XLAI,2)))
ALLOCATE(XPSNG  (SIZE(XLAI,1),SIZE(XLAI,2)))
ALLOCATE(XPSNV  (SIZE(XLAI,1),SIZE(XLAI,2)))
ALLOCATE(XPSNV_A(SIZE(XLAI,1),SIZE(XLAI,2)))
XPSN    = 0.0
XPSNG   = 0.0
XPSNV   = 0.0
XPSNV_A = 0.0
ALLOCATE(XDIR_ALB_WITH_SNOW(SIZE(XLAI,1),1,SIZE(XLAI,2)))
ALLOCATE(XSCA_ALB_WITH_SNOW(SIZE(XLAI,1),1,SIZE(XLAI,2)))
XDIR_ALB_WITH_SNOW = 0.0
XSCA_ALB_WITH_SNOW = 0.0
 CALL AVERAGED_ALBEDO_EMIS_ISBA(.FALSE., CALBEDO, ZZENITH,                &
                                 XVEG,XZ0,XLAI,XTG(:,1,:),               &
                                 XPATCH, ZSW_BANDS,                      &
                                 XALBNIR_VEG,XALBVIS_VEG,XALBUV_VEG,     &
                                 ZALBNIR_SOIL,ZALBVIS_SOIL,ZALBUV_SOIL,  &
                                 XEMIS,                                  &
                                 TSNOW,                                  &
                                 ZALBNIR,ZALBVIS,ZALBUV,                 &
                                 ZDIR_ALB, ZSCA_ALB,                     &
                                 ZEMIS,XTSRAD_NAT                        )  
DEALLOCATE(XPSN)
DEALLOCATE(XPSNG)
DEALLOCATE(XPSNV)
DEALLOCATE(XPSNV_A)
DEALLOCATE(XDIR_ALB_WITH_SNOW)
DEALLOCATE(XSCA_ALB_WITH_SNOW)
!
!-------------------------------------------------------------------------------------
!
!*      7.     Isba-Ags prognostic fields
!
IF (CPHOTO /= 'NON') THEN
!
   ALLOCATE(XAN(SIZE(XLAI,1),SIZE(XLAI,2)))
   XAN = 0.
!
   ALLOCATE(XANDAY(SIZE(XLAI,1),SIZE(XLAI,2)))
   XANDAY = 0.
!
   ALLOCATE(XANFM(SIZE(XLAI,1),SIZE(XLAI,2)))
   XANFM = XANFMINIT
!
   ALLOCATE(XLE(SIZE(XLAI,1),SIZE(XLAI,2)))
   XLE = 0.
!
ENDIF
!
IF (CPHOTO == 'AGS' .OR. CPHOTO == 'AST') THEN
!
   ALLOCATE(XBIOMASS(SIZE(XLAI,1),NNBIOMASS,SIZE(XLAI,2)))
   XBIOMASS(:,1,:) = 0.
!
   ALLOCATE(XRESP_BIOMASS(SIZE(XLAI,1),NNBIOMASS,SIZE(XLAI,2)))
   XRESP_BIOMASS(:,:,:) = 0.
!
ELSEIF (CPHOTO == 'LAI' .OR. CPHOTO == 'LST') THEN
!
   ALLOCATE(XBIOMASS(SIZE(XLAI,1),NNBIOMASS,SIZE(XLAI,2)))
   WHERE(XLAI(:,:)/=XUNDEF)
         XBIOMASS(:,1,:) = XLAI(:,:) * XBSLAI(:,:)
   ELSEWHERE
         XBIOMASS(:,1,:) = 0.0
   ENDWHERE
!
   ALLOCATE(XRESP_BIOMASS(SIZE(XLAI,1),NNBIOMASS,SIZE(XLAI,2)))
   XRESP_BIOMASS(:,:,:) = 0.
!
ELSEIF (CPHOTO == 'NIT' .OR. CPHOTO == 'NCB') THEN
!
   ALLOCATE(XBIOMASS(SIZE(XLAI,1),NNBIOMASS,SIZE(XLAI,2)))
   WHERE(XLAI(:,:)/=XUNDEF)
         XBIOMASS(:,1,:) = XLAI(:,:) * XBSLAI_NITRO(:,:)
   ELSEWHERE
         XBIOMASS(:,1,:) = 0.0
   ENDWHERE
   XBIOMASS(:,2,:) = MAX( 0., (XBIOMASS(:,1,:)/ (XCC_NIT/10.**XCA_NIT))  &
                              **(1.0/(1.0-XCA_NIT)) - XBIOMASS(:,1,:) )  
   XBIOMASS(:,3:NNBIOMASS,:) = 0.
!
   ALLOCATE(XRESP_BIOMASS(SIZE(XLAI,1),NNBIOMASS,SIZE(XLAI,2)))
   XRESP_BIOMASS(:,:,:) = 0.
!
ENDIF
!
!-------------------------------------------------------------------------------------
!
!*      8.     Isba-CC prognostic fields
!
IF (CRESPSL == 'CNT') THEN
!
   ALLOCATE(XLITTER(SIZE(XLAI,1),NNLITTER,NNLITTLEVS,SIZE(XLAI,2)))
   XLITTER(:,:,:,:) = 0.
!
   ALLOCATE(XSOILCARB(SIZE(XLAI,1),NNSOILCARB,SIZE(XLAI,2)))
   XSOILCARB(:,:,:) = 0.
!
   ALLOCATE(XLIGNIN_STRUC(SIZE(XLAI,1),NNLITTLEVS,SIZE(XLAI,2)))
   XLIGNIN_STRUC(:,:,:) = 0.
!
ENDIF
!
!-------------------------------------------------------------------------------------
!
!*      9.     Floodplains scheme
!
IF(LFLOOD)THEN
  ALLOCATE(XZ0_FLOOD(SIZE(XLAI,1),SIZE(XLAI,2)))
  XZ0_FLOOD(:,:) = XZ0SN
ENDIF
!
!-------------------------------------------------------------------------------------
!
!*      10.     Preparation of canopy air variables
!
!
LCANOPY = LISBA_CANOPY
IF (LCANOPY) CALL PREP_ISBA_CANOPY()
IF (LHOOK) CALL DR_HOOK('PREP_ISBA',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE PREP_ISBA
