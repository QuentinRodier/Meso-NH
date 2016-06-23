!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE PREP_ISBA (DTCO, ICP, IG, I, UG, U, USS,GCP, &
                      HPROGRAM,HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE)
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
!!      M. Lafaysse (04/2014) : LSNOW_PREP_PERM
!!      B. Decharme  (04/2013): Good computation for coherence between soil temp and 
!!                              liquid/solid water with DIF (results don't change)
!!                              if lglacier in input file, do not initialize again
!!      P. Samuelsson            (10/2014): MEB
!!------------------------------------------------------------------
!
!
!
!
!
!
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_ISBA_CANOPY_n, ONLY : ISBA_CANOPY_t
USE MODD_ISBA_GRID_n, ONLY : ISBA_GRID_t
USE MODD_ISBA_n, ONLY : ISBA_t
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_SURF_ATM_SSO_n, ONLY : SURF_ATM_SSO_t
USE MODD_GRID_CONF_PROJ, ONLY : GRID_CONF_PROJ_t
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
!                           
USE MODD_DEEPSOIL,    ONLY : LPHYSDOMC
USE MODD_CSTS,        ONLY : XTT, XG, XLMTT
USE MODD_SNOW_PAR,    ONLY : XEMISSN
USE MODD_ISBA_PAR,    ONLY : XWGMIN
!
USE MODD_CO2V_PAR,    ONLY : XANFMINIT
USE MODD_SURF_PAR,    ONLY : XUNDEF
USE MODD_PREP,        ONLY : XZS_LS

USE MODD_PREP_SNOW,   ONLY : LSNOW_PREP_PERM
!
USE MODN_PREP_ISBA
USE MODN_PREP_ISBA_SNOW, ONLY : LSWEMAX, XSWEMAX 
!
USE MODI_VEGTYPE_TO_PATCH
USE MODI_PREP_PERM_SNOW
USE MODI_INIT_SNOW_LW
USE MODI_AVERAGED_ALBEDO_EMIS_ISBA
USE MODI_PREP_HOR_ISBA_CC_FIELD
USE MODI_SOIL_ALBEDO
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
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(ISBA_CANOPY_t), INTENT(INOUT) :: ICP
TYPE(ISBA_GRID_t), INTENT(INOUT) :: IG
TYPE(ISBA_t), INTENT(INOUT) :: I
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(SURF_ATM_SSO_t), INTENT(INOUT) :: USS
TYPE(GRID_CONF_PROJ_t),INTENT(INOUT) :: GCP
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
REAL    :: ZWORK, ZLOG, ZWTOT, ZMATPOT, ZWL
!
REAL,             DIMENSION(1)   :: ZSW_BANDS ! middle wavelength of each band
REAL,             DIMENSION(SIZE(I%XLAI,1),SIZE(I%XLAI,2)) :: ZDIR_ALB  ! direct albedo for each band
REAL,             DIMENSION(SIZE(I%XLAI,1),SIZE(I%XLAI,2)) :: ZSCA_ALB  ! diffuse albedo for each band
REAL,             DIMENSION(SIZE(I%XLAI,1))   :: ZEMIS     ! emissivity
REAL,             DIMENSION(SIZE(I%XLAI,1))   :: ZZENITH   ! solar zenithal angle
REAL,             DIMENSION(SIZE(I%XLAI,1))   :: ZTSURF     ! surface effective temperature
!
LOGICAL         :: GPERMSNOW
LOGICAL         :: GTEMP2WGI
LOGICAL         :: GWG
LOGICAL         :: GWGI
LOGICAL         :: GTG
!
REAL            :: SMAX
!
INTEGER         :: ISIZE_LMEB_PATCH
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('PREP_ISBA',0,ZHOOK_HANDLE)
!
!*      1.     Default of configuration
!
GPERMSNOW = .TRUE.
GWG       = .TRUE.
GWGI      = .TRUE.
GTG       = .TRUE.
!
ISIZE_LMEB_PATCH=COUNT(I%LMEB_PATCH(:))
!
!*      1.1    Default
!
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
 CALL PREP_OUTPUT_GRID(UG, U, &
                       ILUOUT,IG%CGRID,IG%XGRID_PAR,IG%XLAT,IG%XLON)
!
!-------------------------------------------------------------------------------------
!
!*      2.     Reading and horizontal interpolations
!
!
!*      2.0    Large scale orography
!
 CALL PREP_HOR_ISBA_FIELD(DTCO, IG, I, UG, U, USS,GCP, &
                          HPROGRAM,'ZS     ',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE)
!
!*      2.1    Soil Water reservoirs
!
 CALL PREP_HOR_ISBA_FIELD(DTCO, IG, I, UG, U, USS,GCP, &
                          HPROGRAM,'WG     ',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE,GWG)
!
!*      2.2    Soil ice reservoirs
!
 CALL PREP_HOR_ISBA_FIELD(DTCO, IG, I, UG, U, USS,GCP, &
                          HPROGRAM,'WGI    ',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE,GWGI)
!
!*      2.3    Leaves interception water reservoir
!
 CALL PREP_HOR_ISBA_FIELD(DTCO, IG, I, UG, U, USS,GCP, &
                          HPROGRAM,'WR     ',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE)
!
!*      2.4    Temperature profile
!
 CALL PREP_HOR_ISBA_FIELD(DTCO, IG, I, UG, U, USS,GCP, &
                          HPROGRAM,'TG     ',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE,GTG)
!
!*      2.5    Snow variables
!
 CALL PREP_HOR_ISBA_FIELD(DTCO, IG, I, UG, U, USS,GCP, &
                          HPROGRAM,'SN_VEG ',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE,GPERMSNOW)
!
!*      2.6    LAI
!
 CALL PREP_HOR_ISBA_FIELD(DTCO, IG, I, UG, U, USS,GCP, &
                          HPROGRAM,'LAI    ',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE)
!
!*      2.7    GLACIER
!
IF(I%LGLACIER)THEN
  CALL PREP_HOR_ISBA_FIELD(DTCO, IG, I, UG, U, USS,GCP, &
                          HPROGRAM,'ICE_STO',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE)
ENDIF
!
!*      2.8    Canopy vegetation temperature and interception reservoirs and air variables
!
IF(ISIZE_LMEB_PATCH>0)THEN
  CALL PREP_HOR_ISBA_FIELD(DTCO, IG, I, UG, U, USS,GCP, &
                          HPROGRAM,'TV     ',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE)
  CALL PREP_HOR_ISBA_FIELD(DTCO, IG, I, UG, U, USS,GCP, &
                        HPROGRAM,'TL     ',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE)
  CALL PREP_HOR_ISBA_FIELD(DTCO, IG, I, UG, U, USS,GCP, &
                          HPROGRAM,'WRL    ',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE)
  CALL PREP_HOR_ISBA_FIELD(DTCO, IG, I, UG, U, USS,GCP, &
                          HPROGRAM,'WRLI   ',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE)
  CALL PREP_HOR_ISBA_FIELD(DTCO, IG, I, UG, U, USS,GCP, &
                          HPROGRAM,'WRVN   ',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE)
  CALL PREP_HOR_ISBA_FIELD(DTCO, IG, I, UG, U, USS,GCP, &
                          HPROGRAM,'TC     ',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE)
  CALL PREP_HOR_ISBA_FIELD(DTCO, IG, I, UG, U, USS,GCP, &
                          HPROGRAM,'QC     ',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE)
ENDIF
!
!-------------------------------------------------------------------------------------
!
!*      3.    Physical limitation: 
!
! No ice for force restore third layer:
IF (I%CISBA == '3-L') THEN
   DO JP=1,I%NPATCH
      WHERE(I%XWG(:,3,JP) /= XUNDEF)
        I%XWG(:,3,JP)  = MIN(I%XWG(:,3,JP)+I%XWGI(:,3,JP),I%XWSAT(:,3))
        I%XWGI(:,3,JP) = 0.
      END WHERE
   ENDDO
ENDIF
!
! Total water content should not exceed saturation:
DO JP=1,I%NPATCH
   WHERE(I%XWG(:,:,JP) /= XUNDEF .AND. (I%XWG(:,:,JP) + I%XWGI(:,:,JP)) > I%XWSAT(:,:) )
      I%XWGI(:,:,JP) = I%XWSAT(:,:) - I%XWG(:,:,JP)
   END WHERE
ENDDO
!
!-------------------------------------------------------------------------------------
!
!*      3.     Vertical interpolations of all variables
!
IF(LVERTSHIFT)THEN
  CALL PREP_VER_ISBA(I)
ENDIF
!
DEALLOCATE(XZS_LS)
!-------------------------------------------------------------------------------------
!
!*      4.     Treatment of permanent snow
!
IF (GPERMSNOW.AND.LSNOW_PREP_PERM) THEN
  ISNOW = VEGTYPE_TO_PATCH(NVT_SNOW,I%NPATCH)
  CALL PREP_PERM_SNOW(I, &
                      I%TSNOW,I%XTG(:,:,ISNOW),I%XVEGTYPE_PATCH(:,:,ISNOW),ISNOW)
ENDIF
!
 CALL INIT_SNOW_LW(XEMISSN,I%TSNOW)
!
IF (LPHYSDOMC) THEN
   I%TSNOW%WSNOW(:,:,:)=0.
ENDIF 
!------------------------------------------------------------------------------------- 
! 
!*      4.b     Possibility for setting an upper limit on the initial snow water equivalent field 
IF (LSWEMAX) THEN 
  SMAX = MAXVAL(I%TSNOW%WSNOW(:,:,:)) 
  WRITE(*,*) ' MAX(Snow content (kg/m2)): ', SMAX 
  WRITE(*,*) ' Set MAX to', XSWEMAX, '(kg/m2)' 
  I%TSNOW%WSNOW(:,:,:) = MIN(I%TSNOW%WSNOW(:,:,:),XSWEMAX) 
  SMAX = MAXVAL(I%TSNOW%WSNOW(:,:,:)) 
  WRITE(*,*) ' MAX(Snow content (kg/m2)): ', SMAX 
ENDIF
!
!-------------------------------------------------------------------------------------
!
!*      5.     coherence between soil temperature and liquid/solid water
!
GTEMP2WGI=(GWG.OR.GWGI.OR.GTG)
!
IF (I%CISBA == 'DIF'.AND.GTEMP2WGI) THEN
   INI=SIZE(I%XWSAT,1)
   DO JP=1,I%NPATCH
      DO JL=1,I%NGROUND_LAYER
         DO JJ=1,INI
            IF(I%XWG(JJ,JL,JP)/=XUNDEF)THEN
!     
!             total soil moisture
              ZWTOT = I%XWG(JJ,JL,JP)+I%XWGI(JJ,JL,JP)
              ZWTOT = MIN(ZWTOT,I%XWSAT(JJ,JL))
!              
!             total matric potential
!             psi=mpotsat*(w/wsat)**(-bcoef)
              ZWORK   = ZWTOT/I%XWSAT(JJ,JL)
              ZLOG    = I%XBCOEF(JJ,JL)*LOG(ZWORK)
              ZMATPOT = I%XMPOTSAT(JJ,JL)*EXP(-ZLOG)
!
!             soil liquid water content computation
!             w=wsat*(psi/mpotsat)**(-1/bcoef)
              ZMATPOT       = MIN(I%XMPOTSAT(JJ,JL),XLMTT*(I%XTG(JJ,JL,JP)-XTT)/(XG*I%XTG(JJ,JL,JP)))
              ZWORK         = MAX(1.0,ZMATPOT/I%XMPOTSAT(JJ,JL))
              ZLOG          = LOG(ZWORK)
              ZWL           = I%XWSAT(JJ,JL)*EXP(-ZLOG/I%XBCOEF(JJ,JL))
              ZWL           = MAX(ZWL,XWGMIN)
              I%XWG(JJ,JL,JP) = MIN(ZWL,ZWTOT )
!        
!             soil ice computation    
              I%XWGI(JJ,JL,JP) = MAX(0.0,ZWTOT-I%XWG(JJ,JL,JP))
! 
!             supress numerical artefact
              IF(I%XTG(JJ,JL,JP)>=XTT)THEN
                I%XWG (JJ,JL,JP) = MIN(I%XWG(JJ,JL,JP)+I%XWGI(JJ,JL,JP),I%XWSAT(JJ,JL))
                I%XWGI(JJ,JL,JP) = 0.0
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
!              The only variable used from the AVERAGED_ALBEDO_EMIS_ISBA call
!              is XTSRAD_NAT. All other variables are treated as dummies.
!
ALLOCATE(I%XRESA(SIZE(I%XLAI,1),SIZE(I%XLAI,2)))
I%XRESA = 100.
!
ALLOCATE(I%XTSRAD_NAT(SIZE(I%XLAI,1)))
ZZENITH(:)=0.
ZSW_BANDS(:)=0.
!
ALLOCATE(I%XALBNIR(SIZE(I%XLAI,1),SIZE(I%XLAI,2)))
ALLOCATE(I%XALBVIS(SIZE(I%XLAI,1),SIZE(I%XLAI,2)))
ALLOCATE(I%XALBUV(SIZE(I%XLAI,1),SIZE(I%XLAI,2)))
I%XALBNIR = 0.0
I%XALBVIS = 0.0
I%XALBUV = 0.0
!
ALLOCATE(I%XALBNIR_SOIL(SIZE(I%XLAI,1),SIZE(I%XLAI,2)))
ALLOCATE(I%XALBVIS_SOIL(SIZE(I%XLAI,1),SIZE(I%XLAI,2)))
ALLOCATE(I%XALBUV_SOIL(SIZE(I%XLAI,1),SIZE(I%XLAI,2)))
 CALL SOIL_ALBEDO (I%CALBEDO, I%XWSAT(:,1),I%XWG(:,1,:),     &
                    I%XALBVIS_DRY,I%XALBNIR_DRY,I%XALBUV_DRY, &
                    I%XALBVIS_WET,I%XALBNIR_WET,I%XALBUV_WET, &
                    I%XALBVIS_SOIL,I%XALBNIR_SOIL,I%XALBUV_SOIL )
!
ALLOCATE(I%XPSN   (SIZE(I%XLAI,1),SIZE(I%XLAI,2)))
ALLOCATE(I%XPSNG  (SIZE(I%XLAI,1),SIZE(I%XLAI,2)))
ALLOCATE(I%XPSNV  (SIZE(I%XLAI,1),SIZE(I%XLAI,2)))
ALLOCATE(I%XPSNV_A(SIZE(I%XLAI,1),SIZE(I%XLAI,2)))
I%XPSN    = 0.0
I%XPSNG   = 0.0
I%XPSNV   = 0.0
I%XPSNV_A = 0.0
ALLOCATE(I%XDIR_ALB_WITH_SNOW(SIZE(I%XLAI,1),1,SIZE(I%XLAI,2)))
ALLOCATE(I%XSCA_ALB_WITH_SNOW(SIZE(I%XLAI,1),1,SIZE(I%XLAI,2)))
I%XDIR_ALB_WITH_SNOW = 0.0
I%XSCA_ALB_WITH_SNOW = 0.0
 CALL AVERAGED_ALBEDO_EMIS_ISBA(I, &
                               .FALSE., I%CALBEDO, ZZENITH,                &
                                 I%XVEG,I%XZ0,I%XLAI,                          &
                                 I%LMEB_PATCH,I%XGNDLITTER,I%XZ0LITTER,I%XLAIGV, &
                                 I%XH_VEG, I%XTV,               &
                                 I%XTG(:,1,:),I%XPATCH, ZSW_BANDS,           &
                                 I%XALBNIR_VEG,I%XALBVIS_VEG,I%XALBUV_VEG,     &
                                 I%XALBNIR_SOIL,I%XALBVIS_SOIL,I%XALBUV_SOIL,  &
                                 I%XEMIS,                                  &
                                 I%TSNOW,                                  &
                                 I%XALBNIR,I%XALBVIS,I%XALBUV,                 &
                                 ZDIR_ALB, ZSCA_ALB,                     &
                                 ZEMIS,I%XTSRAD_NAT,ZTSURF                 )
DEALLOCATE(I%XPSN)
DEALLOCATE(I%XPSNG)
DEALLOCATE(I%XPSNV)
DEALLOCATE(I%XPSNV_A)
DEALLOCATE(I%XDIR_ALB_WITH_SNOW)
DEALLOCATE(I%XSCA_ALB_WITH_SNOW)
!
!-------------------------------------------------------------------------------------
!
!*      7.     Isba-Ags prognostic fields
!
IF (I%CPHOTO /= 'NON') THEN
!
   ALLOCATE(I%XAN(SIZE(I%XLAI,1),SIZE(I%XLAI,2)))
   I%XAN = 0.
!
   ALLOCATE(I%XANDAY(SIZE(I%XLAI,1),SIZE(I%XLAI,2)))
   I%XANDAY = 0.
!
   ALLOCATE(I%XANFM(SIZE(I%XLAI,1),SIZE(I%XLAI,2)))
   I%XANFM = XANFMINIT
!
   ALLOCATE(I%XLE(SIZE(I%XLAI,1),SIZE(I%XLAI,2)))
   I%XLE = 0.
!
   ALLOCATE(I%XRESP_BIOMASS(SIZE(I%XLAI,1),I%NNBIOMASS,SIZE(I%XLAI,2)))
   I%XRESP_BIOMASS(:,:,:) = 0.
!
ENDIF
!
IF (I%CPHOTO == 'AGS' .OR. I%CPHOTO == 'AST') THEN
!
   ALLOCATE(I%XBIOMASS(SIZE(I%XLAI,1),I%NNBIOMASS,SIZE(I%XLAI,2)))
   I%XBIOMASS(:,:,:) = 0.
!
ELSEIF (I%CPHOTO == 'LAI' .OR. I%CPHOTO == 'LST') THEN
!
   ALLOCATE(I%XBIOMASS(SIZE(I%XLAI,1),I%NNBIOMASS,SIZE(I%XLAI,2)))
   WHERE(I%XLAI(:,:)/=XUNDEF)
         I%XBIOMASS(:,1,:) = I%XLAI(:,:) * I%XBSLAI(:,:)
   ELSEWHERE
         I%XBIOMASS(:,1,:) = 0.0
   ENDWHERE
!
ELSEIF (I%CPHOTO == 'NIT' .OR. I%CPHOTO == 'NCB') THEN
!
   CALL PREP_HOR_ISBA_CC_FIELD(DTCO, U, &
                               IG, I,GCP, &
                               HPROGRAM,'BIOMASS ',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE)   
!
ENDIF
!
!-------------------------------------------------------------------------------------
!
!*      8.     Isba-CC prognostic fields
!
IF (I%CRESPSL == 'CNT') THEN
!
!*      8.1    Litter
!
 CALL PREP_HOR_ISBA_CC_FIELD(DTCO, U, &
                               IG, I,GCP, &
                               HPROGRAM,'LITTER  ',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE)
!
!*      8.2    Soil carbon
!
 CALL PREP_HOR_ISBA_CC_FIELD(DTCO, U, &
                               IG, I,GCP, &
                               HPROGRAM,'SOILCARB',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE)
!
!*      8.2    lignin
!
 CALL PREP_HOR_ISBA_CC_FIELD(DTCO, U, &
                               IG, I,GCP, &
                               HPROGRAM,'LIGNIN  ',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE)
!
ENDIF
!
!-------------------------------------------------------------------------------------
 CALL CLEAN_PREP_OUTPUT_GRID
!-------------------------------------------------------------------------------------
!
!*      10.     Preparation of canopy air variables
!
!
I%LCANOPY = LISBA_CANOPY
IF (I%LCANOPY) CALL PREP_ISBA_CANOPY(ICP, IG)
!
IF (LHOOK) CALL DR_HOOK('PREP_ISBA',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE PREP_ISBA
