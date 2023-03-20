!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE PREP_TEB_GARDEN (DTCO, UG, U, USS, GCP, TG, TOP, IO, S, K, P, PEK, PHV, PEKHV, NPAR_VEG_IRR_USE,&
                            HPROGRAM,HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE,KPATCH,YDCTL)
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
!!      P. Marguinaud10/2014, Support for a 2-part PREP
!!      A. Druel     02/2019, Transmit NPAR_VEG_IRR_USE for irrigation
!!
!!------------------------------------------------------------------
!
!
USE MODD_DATA_COVER_n,     ONLY : DATA_COVER_t
USE MODD_SURF_ATM_GRID_n,  ONLY : SURF_ATM_GRID_t
USE MODD_SURF_ATM_n,       ONLY : SURF_ATM_t
USE MODD_SSO_n,            ONLY : SSO_t
USE MODD_GRID_CONF_PROJ_n, ONLY : GRID_CONF_PROJ_t
!
USE MODD_SFX_GRID_n,       ONLY : GRID_t
!
USE MODD_TEB_OPTION_n,     ONLY : TEB_OPTIONS_t
!
USE MODD_ISBA_OPTIONS_n,   ONLY : ISBA_OPTIONS_t
USE MODD_ISBA_n,           ONLY : ISBA_S_t, ISBA_K_t, ISBA_P_t, ISBA_PE_t
!
USE MODI_PREP_HOR_TEB_GARDEN_FIELD
USE MODI_PREP_VER_TEB_VEG
!
USE MODD_SURF_ATM,         ONLY : LVERTSHIFT
!
                                ! A FAIRE :
                                ! IL FAUT RAJOUTER TSNOW
                                ! ----------------------
USE MODD_CSTS,        ONLY : XTT, XG, XRD, XP00
USE MODD_ISBA_PAR,    ONLY : XWGMIN
USE MODD_CO2V_PAR,    ONLY : XANFMINIT, XCA_NIT, XCC_NIT
USE MODD_SURF_PAR,    ONLY : XUNDEF
!
USE MODE_PREP_CTL,    ONLY : PREP_CTL
!
USE MODN_PREP_ISBA
USE MODE_POS_SURF
USE MODE_THERMOS
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
TYPE(DATA_COVER_t),    INTENT(INOUT) :: DTCO
TYPE(SURF_ATM_GRID_t), INTENT(INOUT) :: UG
TYPE(SURF_ATM_t),      INTENT(INOUT) :: U
TYPE(SSO_t),           INTENT(INOUT) :: USS
TYPE(GRID_CONF_PROJ_t),INTENT(INOUT) :: GCP
TYPE(GRID_t),          INTENT(INOUT) :: TG
TYPE(TEB_OPTIONS_t),   INTENT(INOUT) :: TOP
!
TYPE(ISBA_OPTIONS_t),  INTENT(INOUT) :: IO
TYPE(ISBA_S_t),        INTENT(INOUT) :: S
TYPE(ISBA_K_t),        INTENT(INOUT) :: K
TYPE(ISBA_P_t),        INTENT(INOUT) :: P, PHV
TYPE(ISBA_PE_t),       INTENT(INOUT) :: PEK, PEKHV
!
TYPE (PREP_CTL),       INTENT(INOUT) :: YDCTL
!
INTEGER, DIMENSION(:),INTENT(IN) :: NPAR_VEG_IRR_USE ! vegtype with irrigation
CHARACTER(LEN=6),     INTENT(IN) :: HPROGRAM    ! program calling surf. schemes
CHARACTER(LEN=28),    INTENT(IN) :: HATMFILE    ! name of the Atmospheric file
CHARACTER(LEN=6),     INTENT(IN) :: HATMFILETYPE! type of the Atmospheric file
CHARACTER(LEN=28),    INTENT(IN) :: HPGDFILE    ! name of the Atmospheric file
CHARACTER(LEN=6),     INTENT(IN) :: HPGDFILETYPE! type of the Atmospheric file
!
INTEGER,              INTENT(IN)  :: KPATCH
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(SIZE(TG%XLAT)) :: ZWORK
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------
!
!*      1.     Default of configuration
!
!*      1.1    Default
!
IF (LHOOK) CALL DR_HOOK('PREP_TEB_GARDEN',0,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
!*      2.     Reading and horizontal interpolations
!
!
!*      2.1    Soil Water reservoirs
!
CALL PREP_HOR_TEB_GARDEN_FIELD(DTCO, UG, U, USS, GCP, IO, S, K, P, PEK, TG, TOP, NPAR_VEG_IRR_USE, &
                               HPROGRAM,'WG     ',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE,KPATCH,YDCTL)
!
!*      2.2    Soil ice reservoirs
!
CALL PREP_HOR_TEB_GARDEN_FIELD(DTCO, UG, U, USS, GCP, IO, S, K, P, PEK, TG, TOP, NPAR_VEG_IRR_USE, &         
                               HPROGRAM,'WGI    ',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE,KPATCH,YDCTL)
!
!*      2.3    Leaves interception water reservoir
!
CALL PREP_HOR_TEB_GARDEN_FIELD(DTCO, UG, U, USS, GCP, IO, S, K, P, PEK, TG, TOP, NPAR_VEG_IRR_USE, &
                               HPROGRAM,'WR     ',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE,KPATCH,YDCTL)
!
!*      2.4    Temperature profile
!
CALL PREP_HOR_TEB_GARDEN_FIELD(DTCO, UG, U, USS, GCP, IO, S, K, P, PEK, TG, TOP, NPAR_VEG_IRR_USE, &
                               HPROGRAM,'TG     ',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE,KPATCH,YDCTL)
!
!*      2.5    Snow variables
!
CALL PREP_HOR_TEB_GARDEN_FIELD(DTCO, UG, U, USS, GCP, IO, S, K, P, PEK, TG, TOP, NPAR_VEG_IRR_USE, &
                               HPROGRAM,'SN_VEG ',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE,KPATCH,YDCTL)

!
!*      2.6    LAI
!
IF (IO%CPHOTO/='NON') THEN
 CALL PREP_HOR_TEB_GARDEN_FIELD(DTCO, UG, U, USS, GCP, IO, S, K, P, PEK, TG, TOP, NPAR_VEG_IRR_USE, &
                                HPROGRAM,'LAI    ',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE,KPATCH,YDCTL)
ENDIF
!
!*      2.7    saturation deficit near the leaf surface
!
ALLOCATE(PEK%XQC(SIZE(TG%XLAT)))
ZWORK  (:)=XP00*EXP(-(XG/XRD/PEK%XTG(:,1))*TOP%XZS(:))
PEK%XQC(:)=QSAT(PEK%XTG(:,1),ZWORK)
!
!-------------------------------------------------------------------------------------
!
!*      3.    Physical limitation: 
!
! If whole ice reservoir is empty (grib from ecmwf case) and surface temperature is
! lower than -10C, then ice content is maximum and water content minimum
!
IF (ALL(PEK%XWGI(:,:)==0.)) THEN
   WHERE(PEK%XTG(:,1:SIZE(PEK%XWG,2)) < XTT-10.)
       PEK%XWGI(:,:) = K%XWSAT(:,:)-XWGMIN
       PEK%XWG (:,:) = XWGMIN
   END WHERE
ENDIF
!
! No ice for force restore third layer:
IF (IO%CISBA == '3-L') THEN
      WHERE(PEK%XWG(:,3)/=XUNDEF.AND.PEK%XWGI(:,3)/=XUNDEF)
        PEK%XWG(:,3)  = MIN(PEK%XWG(:,3)+PEK%XWGI(:,3),K%XWSAT(:,3))
        PEK%XWGI(:,3) = 0.
      END WHERE
ENDIF
!
! Total water content should not exceed saturation:
WHERE(PEK%XWG(:,:) /= XUNDEF .AND. (PEK%XWG(:,:) + PEK%XWGI(:,:)) > K%XWSAT(:,:) )
     PEK%XWGI(:,:) = K%XWSAT(:,:) - PEK%XWG(:,:)
END WHERE
!
!-------------------------------------------------------------------------------------
!
!*      3.     Vertical interpolations of all variables
!
IF(LVERTSHIFT)THEN
  CALL PREP_VER_TEB_VEG(P, PEK, IO, TOP%XZS)
ENDIF
!
!-------------------------------------------------------------------------------------
!
!*      4.     High vegetation leaves temperature
!
! set equal to low vegetation surface temperature and saturation deficit near the leaf surface
IF (TOP%CURBTREE/='NONE') THEN
   !
   ALLOCATE(PEKHV%XTV(SIZE(TG%XLAT)))
   PEKHV%XTV = PEK%XTG(:,1)
   !
   ALLOCATE(PEKHV%XQC(SIZE(TG%XLAT)))
   ZWORK  (:)=XP00*EXP(-(XG/XRD/PEKHV%XTV)*TOP%XZS(:))
   PEKHV%XQC(:)=QSAT(PEKHV%XTV,ZWORK)
   !
END IF
!
!-------------------------------------------------------------------------------------
!
!*      5.     Half prognostic fields & Isba-Ags prognostic fields
!
CALL PREP_AGS_OPTIONS_VARIABLES(P,PEK)
IF (TOP%CURBTREE/='NONE') &
CALL PREP_AGS_OPTIONS_VARIABLES(PHV,PEKHV)
!
!-------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('PREP_TEB_GARDEN',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
CONTAINS
!-------------------------------------------------------------------------------------
SUBROUTINE PREP_AGS_OPTIONS_VARIABLES(PP,PE)
!
TYPE(ISBA_P_t),  INTENT(INOUT) :: PP
TYPE(ISBA_PE_t), INTENT(INOUT) :: PE
!-------------------------------------------------------------------------------------
!
!*      5.     Half prognostic fields
!
ALLOCATE(PE%XRESA(SIZE(PE%XLAI)))
PE%XRESA = 100.
!
!-------------------------------------------------------------------------------------
!
!*      6.     Isba-Ags prognostic fields
!
IF (IO%CPHOTO /= 'NON') THEN
!
   ALLOCATE(PE%XAN(SIZE(PE%XLAI)))
   PE%XAN = 0.
!
   ALLOCATE(PE%XANDAY(SIZE(PE%XLAI)))
   PE%XANDAY = 0.
!
   ALLOCATE(PE%XANFM(SIZE(PE%XLAI)))
   PE%XANFM = XANFMINIT
!
ENDIF
!
IF (IO%CPHOTO == 'AST') THEN
!
   ALLOCATE(PE%XBIOMASS(SIZE(PE%XLAI),IO%NNBIOMASS))
   PE%XBIOMASS(:,1) = 0.
!
   ALLOCATE(PE%XRESP_BIOMASS(SIZE(PE%XLAI),IO%NNBIOMASS))
   PE%XRESP_BIOMASS(:,:) = 0.
!
ELSEIF (IO%CPHOTO == 'NIT' .OR. IO%CPHOTO == 'NCB') THEN
!
   ALLOCATE(PE%XBIOMASS(SIZE(PE%XLAI),IO%NNBIOMASS))
   PE%XBIOMASS(:,1) = PE%XLAI(:) * PP%XBSLAI_NITRO(:)
   PE%XBIOMASS(:,2) = MAX( 0., (PE%XBIOMASS(:,1)/ (XCC_NIT/10.**XCA_NIT))  &
                              **(1.0/(1.0-XCA_NIT)) - PE%XBIOMASS(:,1) )  
   PE%XBIOMASS(:,3:IO%NNBIOMASS) = 0.
!
   ALLOCATE(PE%XRESP_BIOMASS(SIZE(PE%XLAI),IO%NNBIOMASS))
   PE%XRESP_BIOMASS(:,:) = 0.
!
ENDIF
!
END SUBROUTINE PREP_AGS_OPTIONS_VARIABLES
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE PREP_TEB_GARDEN
