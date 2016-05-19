!     #########
SUBROUTINE PREP_TEB_GREENROOF(HPROGRAM,HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE)
!     #################################################################################
!
!!****  *PREP_TEB_GREENROOF* - Prepares ISBA fields for greenroofs
!!
!!    PURPOSE
!!    -------
!
!!**  METHOD
!!    ------
!!    Based on "prep_teb_garden"
!!
!!    REFERENCE
!!    ---------
!!      
!!
!!    AUTHOR
!!    ------
!!      A. Lemonsu & C. de Munck 
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    07/2011
!!------------------------------------------------------------------
!
!
USE MODI_PREP_HOR_TEB_GREENROOF_FIELD
USE MODI_PREP_VER_TEB_GREENROOF
!
USE MODD_TEB_VEG_n,      ONLY : CPHOTO, CRESPSL,                                &
                                NNBIOMASS
USE MODD_TEB_n,          ONLY : XT_ROOF
USE MODD_TEB_GREENROOF_n,ONLY : XRESA, XLAI,                                    &
                                XAN, XANFM, XANDAY, XLE,                        &
                                XBSLAI, XBSLAI_NITRO, XBIOMASS, XRESP_BIOMASS,  &
                                XWSAT, XWG, XWGI, XTG, XTDEEP
                                ! A FAIRE :
                                ! IL FAUT RAJOUTER TSNOW
                                ! ----------------------
USE MODD_SURF_ATM,       ONLY : LVERTSHIFT
USE MODD_CSTS,           ONLY : XTT
USE MODD_SNOW_PAR,       ONLY : XZ0SN
USE MODD_ISBA_PAR,       ONLY : XWGMIN
USE MODD_CO2V_PAR,       ONLY : XCC_NIT, XCA_NIT, XANFMINIT
USE MODD_SURF_PAR,       ONLY : XUNDEF
!
USE MODN_PREP_ISBA
USE MODE_POS_SURF
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
 CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM    ! program calling surf. schemes
 CHARACTER(LEN=28),  INTENT(IN)  :: HATMFILE    ! name of the Atmospheric file
 CHARACTER(LEN=6),   INTENT(IN)  :: HATMFILETYPE! type of the Atmospheric file
 CHARACTER(LEN=28),  INTENT(IN)  :: HPGDFILE    ! name of the Atmospheric file
 CHARACTER(LEN=6),   INTENT(IN)  :: HPGDFILETYPE! type of the Atmospheric file
!
!*      0.2    declarations of local variables
!
INTEGER :: JP
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------
!
!*      1.     Default of configuration
!
!*      1.1    Default
!
!
!-------------------------------------------------------------------------------------
!
!*      2.     Reading and horizontal interpolations
!
!
!*      2.1    Soil Water reservoirs
!
IF (LHOOK) CALL DR_HOOK('PREP_TEB_GREENROOF',0,ZHOOK_HANDLE)
!
 CALL PREP_HOR_TEB_GREENROOF_FIELD(HPROGRAM,'WG     ',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE)
!
!*      2.2    Soil ice reservoirs
!
 CALL PREP_HOR_TEB_GREENROOF_FIELD(HPROGRAM,'WGI    ',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE)
!
!*      2.3    Leaves interception water reservoir
!
 CALL PREP_HOR_TEB_GREENROOF_FIELD(HPROGRAM,'WR     ',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE)
!
!*      2.4    Temperature profile
!
 CALL PREP_HOR_TEB_GREENROOF_FIELD(HPROGRAM,'TG     ',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE)
!
! Initializing deep GR temp. with that of the outer layer of the structural roof 
!
XTDEEP(:) = XT_ROOF(:,1)
!
!*      2.5    Snow variables
!
 CALL PREP_HOR_TEB_GREENROOF_FIELD(HPROGRAM,'SN_VEG ',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE)
!
!*      2.6    LAI
!
 CALL PREP_HOR_TEB_GREENROOF_FIELD(HPROGRAM,'LAI    ',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE)
!
!-------------------------------------------------------------------------------------
!
!*      3.    Physical limitations: 
!
! 3.1  If whole ice reservoir is empty (grib from ecmwf case) and surface temperature is
!      lower than -10°C, then ice content is maximum and water content minimum
!
IF (ALL(XWGI(:,:)==0.)) THEN
   WHERE(XTG(:,1:SIZE(XWG,2)) < XTT-10.)
      XWGI(:,:) = XWSAT(:,:)-XWGMIN
      XWG (:,:) = XWGMIN
   END WHERE
ENDIF
!
!
! 3.2.  Total water content should not exceed saturation:
WHERE(XWG(:,:) /= XUNDEF .AND. (XWG(:,:) + XWGI(:,:)) > XWSAT(:,:) )
   XWGI(:,:) = XWSAT(:,:) - XWG(:,:)
END WHERE
!
!-------------------------------------------------------------------------------------
!
!*      4.     Vertical interpolations of all variables
!
IF(LVERTSHIFT)THEN
  CALL PREP_VER_TEB_GREENROOF
ENDIF
!
!
!-------------------------------------------------------------------------------------
!
!*      5.     Half prognostic fields
!
ALLOCATE(XRESA(SIZE(XLAI)))
XRESA(:) = 100.
!
!-------------------------------------------------------------------------------------
!
!*      6.     Isba-Ags prognostic fields
!
IF (CPHOTO /= 'NON') THEN
!
   ALLOCATE(XAN(SIZE(XLAI)))
   XAN = 0.
!
   ALLOCATE(XANDAY(SIZE(XLAI)))
   XANDAY = 0.
!
   ALLOCATE(XANFM(SIZE(XLAI)))
   XANFM = XANFMINIT
!
   ALLOCATE(XLE(SIZE(XLAI)))
   XLE = 0.
!
ENDIF
!
IF (CPHOTO == 'AGS' .OR. CPHOTO == 'AST') THEN
!
   ALLOCATE(XBIOMASS(SIZE(XLAI),NNBIOMASS))
   XBIOMASS(:,1) = 0.
!
   ALLOCATE(XRESP_BIOMASS(SIZE(XLAI),NNBIOMASS))
   XRESP_BIOMASS(:,:) = 0.
!
ELSEIF (CPHOTO == 'LAI' .OR. CPHOTO == 'LST') THEN
!
   ALLOCATE(XBIOMASS(SIZE(XLAI),NNBIOMASS))
   XBIOMASS(:,1) = XLAI(:) * XBSLAI(:)
!
   ALLOCATE(XRESP_BIOMASS(SIZE(XLAI),NNBIOMASS))
   XRESP_BIOMASS(:,:) = 0.
!
ELSEIF (CPHOTO == 'NIT' .OR. CPHOTO == 'NCB') THEN
!
   ALLOCATE(XBIOMASS(SIZE(XLAI),NNBIOMASS))
   XBIOMASS(:,1) = XLAI(:) * XBSLAI_NITRO(:)
   XBIOMASS(:,2) = MAX( 0., (XBIOMASS(:,1)/ (XCC_NIT/10.**XCA_NIT))  &
                              **(1.0/(1.0-XCA_NIT)) - XBIOMASS(:,1) )  
   XBIOMASS(:,3:NNBIOMASS) = 0.
!
   ALLOCATE(XRESP_BIOMASS(SIZE(XLAI),NNBIOMASS))
   XRESP_BIOMASS(:,:) = 0.
!
ENDIF
!
!-------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('PREP_TEB_GREENROOF',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE PREP_TEB_GREENROOF
