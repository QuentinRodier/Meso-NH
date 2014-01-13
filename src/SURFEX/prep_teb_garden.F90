!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
SUBROUTINE PREP_TEB_GARDEN(HPROGRAM,HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE)
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
USE MODI_PREP_HOR_TEB_GARDEN_FIELD
USE MODI_PREP_VER_TEB_GARDEN
!
USE MODD_SURF_ATM,       ONLY : LVERTSHIFT
!
USE MODD_TEB_VEG_n,      ONLY : CPHOTO, CRESPSL,                              &
                                NNBIOMASS,                                    &
                                CISBA
USE MODD_TEB_GARDEN_n,   ONLY : XRESA, XLAI,                                  &
                                XAN, XANFM, XANDAY, XLE,                      &
                                XBSLAI, XBSLAI_NITRO, XBIOMASS, XRESP_BIOMASS,&
                                XWSAT, XWG, XWGI, XTG, XVEGTYPE
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
 CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
 CHARACTER(LEN=28),  INTENT(IN)  :: HATMFILE    ! name of the Atmospheric file
 CHARACTER(LEN=6),   INTENT(IN)  :: HATMFILETYPE! type of the Atmospheric file
 CHARACTER(LEN=28),  INTENT(IN)  :: HPGDFILE    ! name of the Atmospheric file
 CHARACTER(LEN=6),   INTENT(IN)  :: HPGDFILETYPE! type of the Atmospheric file
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
 CALL PREP_HOR_TEB_GARDEN_FIELD(HPROGRAM,'WG     ',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE)
!
!*      2.2    Soil ice reservoirs
!
 CALL PREP_HOR_TEB_GARDEN_FIELD(HPROGRAM,'WGI    ',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE)
!
!*      2.3    Leaves interception water reservoir
!
 CALL PREP_HOR_TEB_GARDEN_FIELD(HPROGRAM,'WR     ',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE)
!
!*      2.4    Temperature profile
!
 CALL PREP_HOR_TEB_GARDEN_FIELD(HPROGRAM,'TG     ',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE)
!
!*      2.5    Snow variables
!
 CALL PREP_HOR_TEB_GARDEN_FIELD(HPROGRAM,'SN_VEG ',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE)

!
!*      2.6    LAI
!
IF (CPHOTO/='NON' .AND. CPHOTO/='AGS' .AND. CPHOTO/='LST')  &
 CALL PREP_HOR_TEB_GARDEN_FIELD(HPROGRAM,'LAI    ',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE)
!
!-------------------------------------------------------------------------------------
!
!*      3.    Physical limitation: 
!
! If whole ice reservoir is empty (grib from ecmwf case) and surface temperature is
! lower than -10°C, then ice content is maximum and water content minimum
!
   IF (ALL(XWGI(:,:)==0.)) THEN
      WHERE(XTG(:,1:SIZE(XWG,2)) < XTT-10.)
         XWGI(:,:) = XWSAT(:,:)-XWGMIN
         XWG (:,:) = XWGMIN
      END WHERE
   ENDIF
!
! No ice for force restore third layer:
IF (CISBA == '3-L') THEN
      WHERE(XWG(:,3) /= XUNDEF)
        XWG(:,3)  = MIN(XWG(:,3)+XWGI(:,3),XWSAT(:,3))
        XWGI(:,3) = 0.
      END WHERE
ENDIF
!
! Total water content should not exceed saturation:
   WHERE(XWG(:,:) /= XUNDEF .AND. (XWG(:,:) + XWGI(:,:)) > XWSAT(:,:) )
      XWGI(:,:) = XWSAT(:,:) - XWG(:,:)
   END WHERE
!
!-------------------------------------------------------------------------------------
!
!*      3.     Vertical interpolations of all variables
!
IF(LVERTSHIFT)THEN
  CALL PREP_VER_TEB_GARDEN
ENDIF
!
!
!-------------------------------------------------------------------------------------
!
!*      5.     Half prognostic fields
!
ALLOCATE(XRESA(SIZE(XLAI,1)))
XRESA = 100.
!
!-------------------------------------------------------------------------------------
!
!*      6.     Isba-Ags prognostic fields
!
IF (CPHOTO /= 'NON') THEN
!
   ALLOCATE(XAN(SIZE(XLAI,1)))
   XAN = 0.
!
   ALLOCATE(XANDAY(SIZE(XLAI,1)))
   XANDAY = 0.
!
   ALLOCATE(XANFM(SIZE(XLAI,1)))
   XANFM = XANFMINIT
!
   ALLOCATE(XLE(SIZE(XLAI,1)))
   XLE = 0.
!
ENDIF
!
IF (CPHOTO == 'AGS' .OR. CPHOTO == 'AST') THEN
!
   ALLOCATE(XBIOMASS(SIZE(XLAI,1),NNBIOMASS))
   XBIOMASS(:,1) = 0.
!
   ALLOCATE(XRESP_BIOMASS(SIZE(XLAI,1),NNBIOMASS))
   XRESP_BIOMASS(:,:) = 0.
!
ELSEIF (CPHOTO == 'LAI' .OR. CPHOTO == 'LST') THEN
!
   ALLOCATE(XBIOMASS(SIZE(XLAI,1),NNBIOMASS))
   XBIOMASS(:,1) = XLAI(:) * XBSLAI(:)
!
   ALLOCATE(XRESP_BIOMASS(SIZE(XLAI,1),NNBIOMASS))
   XRESP_BIOMASS(:,:) = 0.
!
ELSEIF (CPHOTO == 'NIT' .OR. CPHOTO == 'NCB') THEN
!
   ALLOCATE(XBIOMASS(SIZE(XLAI,1),NNBIOMASS))
   XBIOMASS(:,1) = XLAI(:) * XBSLAI_NITRO(:)
   XBIOMASS(:,2) = MAX( 0., (XBIOMASS(:,1)/ (XCC_NIT/10.**XCA_NIT))  &
                              **(1.0/(1.0-XCA_NIT)) - XBIOMASS(:,1) )  
   XBIOMASS(:,3:NNBIOMASS) = 0.
!
   ALLOCATE(XRESP_BIOMASS(SIZE(XLAI,1),NNBIOMASS))
   XRESP_BIOMASS(:,:) = 0.
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
