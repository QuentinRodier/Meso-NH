!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE PREP_TEB_GARDEN (DTCO, UG, U, USS, IG, I, TG, TOP, GDM,GCP, &
                            HPROGRAM,HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE,KPATCH)
!     #################################################################################
!
!!****  *PREP_TEB_GARDEN* - Prepares ISBA fields
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
!!------------------------------------------------------------------
!
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_SURF_ATM_GRID_n, ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
USE MODD_SURF_ATM_SSO_n, ONLY : SURF_ATM_SSO_t
USE MODD_ISBA_GRID_n, ONLY : ISBA_GRID_t
USE MODD_ISBA_n, ONLY : ISBA_t
USE MODD_TEB_GRID_n, ONLY : TEB_GRID_t
USE MODD_TEB_OPTION_n, ONLY : TEB_OPTIONS_t
USE MODD_SURFEX_n, ONLY : TEB_GARDEN_MODEL_t
USE MODD_GRID_CONF_PROJ, ONLY : GRID_CONF_PROJ_t
!
USE MODI_PREP_HOR_TEB_GARDEN_FIELD
USE MODI_PREP_VER_TEB_GARDEN
!
USE MODD_SURF_ATM,       ONLY : LVERTSHIFT
!
                                ! A FAIRE :
                                ! IL FAUT RAJOUTER TSNOW
                                ! ----------------------
USE MODD_CSTS,        ONLY : XTT
USE MODD_SNOW_PAR,    ONLY : XZ0SN
USE MODD_ISBA_PAR,    ONLY : XWGMIN
USE MODD_CO2V_PAR,    ONLY : XANFMINIT, XCA_NIT, XCC_NIT
USE MODD_SURF_PAR,    ONLY : XUNDEF
!
USE MODN_PREP_ISBA
USE MODE_POS_SURF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
TYPE(SURF_ATM_SSO_t), INTENT(INOUT) :: USS
TYPE(ISBA_GRID_t), INTENT(INOUT) :: IG
TYPE(ISBA_t), INTENT(INOUT) :: I
TYPE(TEB_GRID_t), INTENT(INOUT) :: TG
TYPE(TEB_OPTIONS_t), INTENT(INOUT) :: TOP
TYPE(TEB_GARDEN_MODEL_t), INTENT(INOUT) :: GDM
TYPE(GRID_CONF_PROJ_t),INTENT(INOUT) :: GCP
!
 CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
 CHARACTER(LEN=28),  INTENT(IN)  :: HATMFILE    ! name of the Atmospheric file
 CHARACTER(LEN=6),   INTENT(IN)  :: HATMFILETYPE! type of the Atmospheric file
 CHARACTER(LEN=28),  INTENT(IN)  :: HPGDFILE    ! name of the Atmospheric file
 CHARACTER(LEN=6),   INTENT(IN)  :: HPGDFILETYPE! type of the Atmospheric file
!
INTEGER,            INTENT(IN)  :: KPATCH
!
!*      0.2    declarations of local variables
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
!-------------------------------------------------------------------------------------
!
!*      2.     Reading and horizontal interpolations
!
!
!*      2.1    Soil Water reservoirs
!
IF (LHOOK) CALL DR_HOOK('PREP_TEB_GARDEN',0,ZHOOK_HANDLE)
 CALL PREP_HOR_TEB_GARDEN_FIELD(DTCO, IG, I, UG, U, USS, &
                                GDM%TGD, GDM%TGDO, GDM%TGDPE, GDM%TGDP, TG, TOP, GDM%TVG,GCP, &
                                HPROGRAM,'WG     ',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE,KPATCH)
!
!*      2.2    Soil ice reservoirs
!
 CALL PREP_HOR_TEB_GARDEN_FIELD(DTCO, IG, I, UG, U, USS, &
                                GDM%TGD, GDM%TGDO, GDM%TGDPE, GDM%TGDP, TG, TOP, GDM%TVG,GCP, &
                                HPROGRAM,'WGI    ',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE,KPATCH)
!
!*      2.3    Leaves interception water reservoir
!
 CALL PREP_HOR_TEB_GARDEN_FIELD(DTCO, IG, I, UG, U, USS, &
                                GDM%TGD, GDM%TGDO, GDM%TGDPE, GDM%TGDP, TG, TOP, GDM%TVG,GCP, &
                                HPROGRAM,'WR     ',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE,KPATCH)
!
!*      2.4    Temperature profile
!
 CALL PREP_HOR_TEB_GARDEN_FIELD(DTCO, IG, I, UG, U, USS, &
                                GDM%TGD, GDM%TGDO, GDM%TGDPE, GDM%TGDP, TG, TOP, GDM%TVG,GCP, &
                                HPROGRAM,'TG     ',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE,KPATCH)
!
!*      2.5    Snow variables
!
 CALL PREP_HOR_TEB_GARDEN_FIELD(DTCO, IG, I, UG, U, USS, &
                                GDM%TGD, GDM%TGDO, GDM%TGDPE, GDM%TGDP, TG, TOP, GDM%TVG,GCP, &
                                HPROGRAM,'SN_VEG ',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE,KPATCH)

!
!*      2.6    LAI
!
IF (GDM%TVG%CPHOTO/='NON' .AND. GDM%TVG%CPHOTO/='AGS' .AND. GDM%TVG%CPHOTO/='LST')  &
 CALL PREP_HOR_TEB_GARDEN_FIELD(DTCO, IG, I, UG, U, USS, &
                                GDM%TGD, GDM%TGDO, GDM%TGDPE, GDM%TGDP, TG, TOP, GDM%TVG,GCP, &
                                HPROGRAM,'LAI    ',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE,KPATCH)
!
!-------------------------------------------------------------------------------------
!
!*      3.    Physical limitation: 
!
! If whole ice reservoir is empty (grib from ecmwf case) and surface temperature is
! lower than -10C, then ice content is maximum and water content minimum
!
IF (ALL(GDM%TGD%CUR%XWGI(:,:)==0.)) THEN
   WHERE(GDM%TGD%CUR%XTG(:,1:SIZE(GDM%TGD%CUR%XWG,2)) < XTT-10.)
       GDM%TGD%CUR%XWGI(:,:) = GDM%TGDP%XWSAT(:,:)-XWGMIN
       GDM%TGD%CUR%XWG (:,:) = XWGMIN
   END WHERE
ENDIF
!
! No ice for force restore third layer:
IF (GDM%TVG%CISBA == '3-L') THEN
      WHERE(GDM%TGD%CUR%XWG(:,3)/=XUNDEF.AND.GDM%TGD%CUR%XWGI(:,3)/=XUNDEF)
        GDM%TGD%CUR%XWG(:,3)  = MIN(GDM%TGD%CUR%XWG(:,3)+GDM%TGD%CUR%XWGI(:,3),GDM%TGDP%XWSAT(:,3))
        GDM%TGD%CUR%XWGI(:,3) = 0.
      END WHERE
ENDIF
!
! Total water content should not exceed saturation:
WHERE(GDM%TGD%CUR%XWG(:,:) /= XUNDEF .AND. &
                  (GDM%TGD%CUR%XWG(:,:) + GDM%TGD%CUR%XWGI(:,:)) > GDM%TGDP%XWSAT(:,:) )
     GDM%TGD%CUR%XWGI(:,:) = GDM%TGDP%XWSAT(:,:) - GDM%TGD%CUR%XWG(:,:)
END WHERE
!
!-------------------------------------------------------------------------------------
!
!*      3.     Vertical interpolations of all variables
!
IF(LVERTSHIFT)THEN
  CALL PREP_VER_TEB_GARDEN(GDM%TGD, GDM%TGDO, GDM%TGDP, TOP, GDM%TVG)
ENDIF
!
!
!-------------------------------------------------------------------------------------
!
!*      5.     Half prognostic fields
!
ALLOCATE(GDM%TGD%CUR%XRESA(SIZE(GDM%TGDPE%CUR%XLAI,1)))
GDM%TGD%CUR%XRESA = 100.
!
!-------------------------------------------------------------------------------------
!
!*      6.     Isba-Ags prognostic fields
!
IF (GDM%TVG%CPHOTO /= 'NON') THEN
!
   ALLOCATE(GDM%TGD%CUR%XAN(SIZE(GDM%TGDPE%CUR%XLAI,1)))
   GDM%TGD%CUR%XAN = 0.
!
   ALLOCATE(GDM%TGD%CUR%XANDAY(SIZE(GDM%TGDPE%CUR%XLAI,1)))
   GDM%TGD%CUR%XANDAY = 0.
!
   ALLOCATE(GDM%TGD%CUR%XANFM(SIZE(GDM%TGDPE%CUR%XLAI,1)))
   GDM%TGD%CUR%XANFM = XANFMINIT
!
   ALLOCATE(GDM%TGD%CUR%XLE(SIZE(GDM%TGDPE%CUR%XLAI,1)))
   GDM%TGD%CUR%XLE = 0.
!
ENDIF
!
IF (GDM%TVG%CPHOTO == 'AGS' .OR. GDM%TVG%CPHOTO == 'AST') THEN
!
   ALLOCATE(GDM%TGD%CUR%XBIOMASS(SIZE(GDM%TGDPE%CUR%XLAI,1),GDM%TVG%NNBIOMASS))
   GDM%TGD%CUR%XBIOMASS(:,1) = 0.
!
   ALLOCATE(GDM%TGD%CUR%XRESP_BIOMASS(SIZE(GDM%TGDPE%CUR%XLAI,1),GDM%TVG%NNBIOMASS))
   GDM%TGD%CUR%XRESP_BIOMASS(:,:) = 0.
!
ELSEIF (GDM%TVG%CPHOTO == 'LAI' .OR. GDM%TVG%CPHOTO == 'LST') THEN
!
   ALLOCATE(GDM%TGD%CUR%XBIOMASS(SIZE(GDM%TGDPE%CUR%XLAI,1),GDM%TVG%NNBIOMASS))
   GDM%TGD%CUR%XBIOMASS(:,1) = GDM%TGDPE%CUR%XLAI(:) * GDM%TGDP%XBSLAI(:)
!
   ALLOCATE(GDM%TGD%CUR%XRESP_BIOMASS(SIZE(GDM%TGDPE%CUR%XLAI,1),GDM%TVG%NNBIOMASS))
   GDM%TGD%CUR%XRESP_BIOMASS(:,:) = 0.
!
ELSEIF (GDM%TVG%CPHOTO == 'NIT' .OR. GDM%TVG%CPHOTO == 'NCB') THEN
!
   ALLOCATE(GDM%TGD%CUR%XBIOMASS(SIZE(GDM%TGDPE%CUR%XLAI,1),GDM%TVG%NNBIOMASS))
   GDM%TGD%CUR%XBIOMASS(:,1) = GDM%TGDPE%CUR%XLAI(:) * GDM%TGDP%XBSLAI_NITRO(:)
   GDM%TGD%CUR%XBIOMASS(:,2) = MAX( 0., (GDM%TGD%CUR%XBIOMASS(:,1)/ (XCC_NIT/10.**XCA_NIT))  &
                              **(1.0/(1.0-XCA_NIT)) - GDM%TGD%CUR%XBIOMASS(:,1) )  
   GDM%TGD%CUR%XBIOMASS(:,3:GDM%TVG%NNBIOMASS) = 0.
!
   ALLOCATE(GDM%TGD%CUR%XRESP_BIOMASS(SIZE(GDM%TGDPE%CUR%XLAI,1),GDM%TVG%NNBIOMASS))
   GDM%TGD%CUR%XRESP_BIOMASS(:,:) = 0.
!
ENDIF
!
!-------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('PREP_TEB_GARDEN',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE PREP_TEB_GARDEN
