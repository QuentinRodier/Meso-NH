!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE FIRE_CARBON(IO, PK, PEK, DEK, DMK, KLUOUT, PTSTEP_DAY,             &
                             PSURF_LITTER, PSURF_LIGNIN, PSOIL_LIGNIN, PSOIL_LITTER )
!     ##################################
!
!!****  *FIRE_CARBON* -  calculate fire extent and impact on plants.
!!
!!    PURPOSE
!!    -------
!!
!!  Fire burns litter and biomass on the ground 
!!  This is treated on a pseudo-daily basis (fireindex has a long-term memory).
!!  We only take into account the natural litter.
!!
!!**  METHOD
!!    ------
!!    inspired from Thonickle et al. 2001 = GlobFirm model
!!    inspired from Li et al. 2012
!!    inspired from Mangeon et al. 2016
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
!!	R. Alkama
!!	R. Séférian
!!      B. Decharme
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    07/2021
!!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_ISBA_OPTIONS_n,   ONLY : ISBA_OPTIONS_t
USE MODD_ISBA_n,           ONLY : ISBA_P_t, ISBA_PE_t
USE MODD_DIAG_EVAP_ISBA_n, ONLY : DIAG_EVAP_ISBA_t
USE MODD_DIAG_MISC_ISBA_n, ONLY : DIAG_MISC_ISBA_t
!
USE MODD_CO2V_PAR, ONLY : XPCCO2, XKGTOG, XGTOKG, XMC, XMCO2
USE MODD_SURF_PAR, ONLY : XUNDEF
!
USE MODD_FIRE_PAR
!
USE MODI_BIOMASS_TO_SURFACE_LITTER
USE MODI_BIOMASS_TO_SOIL_LITTER
!
USE MODI_ABOR1_SFX
!
USE YOMHOOK,  ONLY : LHOOK,   DR_HOOK
USE PARKIND1, ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
TYPE(ISBA_OPTIONS_t),   INTENT(INOUT) :: IO
TYPE(ISBA_P_t),         INTENT(INOUT) :: PK
TYPE(ISBA_PE_t),        INTENT(INOUT) :: PEK
TYPE(DIAG_EVAP_ISBA_t), INTENT(INOUT) :: DEK
TYPE(DIAG_MISC_ISBA_t), INTENT(IN   ) :: DMK
!
!
INTEGER,                INTENT(IN)    :: KLUOUT
!
REAL,                   INTENT(IN)    :: PTSTEP_DAY   ! Time step in days
!
!*       0.2 modified fields
!
REAL, DIMENSION(:,:),   INTENT(INOUT) :: PSURF_LITTER ! Surface litter pools (gC m-2)
REAL, DIMENSION(:),     INTENT(INOUT) :: PSURF_LIGNIN ! Surface L/C ratio in structural litter
REAL, DIMENSION(:,:,:), INTENT(INOUT) :: PSOIL_LITTER ! litter pools (gC m-2)
REAL, DIMENSION(:,:),   INTENT(INOUT) :: PSOIL_LIGNIN ! L/C ratio in structural litter 
!
!*      0.3    declarations of local parameter
!
INTEGER, PARAMETER                                 :: IABOVE = 1
INTEGER, PARAMETER                                 :: IBELOW = 2
!
!*      0.4    declarations of local variables
!
REAL, DIMENSION(SIZE(PEK%XBIOMASS,1),SIZE(PEK%XBIOMASS,2)) :: ZEMITC_BIOM     ! Emitted carbon from biomass fire (gDM/m2/day)
REAL, DIMENSION(SIZE(PEK%XBIOMASS,1),SIZE(PEK%XBIOMASS,2)) :: ZFIRECO2_BIOM   ! Emission of CO2 from biomass fire (gC/m2/day)
REAL, DIMENSION(SIZE(PEK%XBIOMASS,1),SIZE(PEK%XBIOMASS,2)) :: ZFIREBCS_BIOM   ! Emission of Black carbon from biomass fire (gBC/m2/day)
REAL, DIMENSION(SIZE(PEK%XBIOMASS,1),SIZE(PEK%XBIOMASS,2)) :: ZCCBIOMASS      ! Combustion completness factor for biomass (Li et al. 2012)
REAL, DIMENSION(SIZE(PEK%XBIOMASS,1),SIZE(PEK%XBIOMASS,2)) :: ZMORTALITY      ! Tissue-mortality factor (Li et al. 2012)
REAL, DIMENSION(SIZE(PEK%XBIOMASS,1),SIZE(PEK%XBIOMASS,2)) :: ZTURNOVER       ! Biomass mortality (gC/m2/day)
!
REAL, DIMENSION(SIZE(PSURF_LITTER,1),SIZE(PSURF_LITTER,2)) :: ZEMITC_LITTER   ! Emitted carbon from litter fire (gC/m2/day)
REAL, DIMENSION(SIZE(PSURF_LITTER,1),SIZE(PSURF_LITTER,2)) :: ZFIRECO2_LITTER ! Emission of CO2 from litter fire (gC/m2/day)
REAL, DIMENSION(SIZE(PSURF_LITTER,1),SIZE(PSURF_LITTER,2)) :: ZFIREBCS_LITTER ! Emission of Black carbon from litter fire (gBC/m2/day)
!
REAL, DIMENSION(SIZE(PEK%XLAI))                 :: ZDISTURB       
REAL, DIMENSION(SIZE(PEK%XLAI))                 :: ZFIRECO2
REAL, DIMENSION(SIZE(PEK%XLAI))                 :: ZFIREBCS
!
REAL, DIMENSION(SIZE(PEK%XLAI))                 :: ZTREE
REAL, DIMENSION(SIZE(PEK%XLAI))                 :: ZFEEBSC
REAL, DIMENSION(SIZE(PEK%XLAI))                 :: ZFEECO2
REAL, DIMENSION(SIZE(PEK%XLAI))                 :: ZCCLITTER
!
!- carbon conservation
!
REAL, DIMENSION(SIZE(PEK%XLAI))                 :: ZCBUDGET 
REAL, DIMENSION(SIZE(PEK%XLAI))                 :: ZVEG_IN
REAL, DIMENSION(SIZE(PEK%XLAI))                 :: ZLIT_IN_SURF
REAL, DIMENSION(SIZE(PEK%XLAI))                 :: ZLIT_IN_SOIL
REAL, DIMENSION(SIZE(PEK%XLAI))                 :: ZVEG_OUT
REAL, DIMENSION(SIZE(PEK%XLAI))                 :: ZLIT_OUT_SURF
REAL, DIMENSION(SIZE(PEK%XLAI))                 :: ZLIT_OUT_SOIL
!
INTEGER, DIMENSION(SIZE(PEK%XLAI))              :: IWG_LAYER
!
INTEGER                             :: JI,JL,JLIT,JB,JTYPE,INI,INL,INLIT,INTYPE,INB      ! index
!
REAL(KIND=JPRB)                     :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('FIRE_CARBON',0,ZHOOK_HANDLE)
!
! 1 Initializations
!
INI    = SIZE(PEK%XLAI)
INLIT  = SIZE(PSURF_LITTER,2)
INB    = SIZE(PEK%XBIOMASS,2)
INTYPE = SIZE(PK%XVEGTYPE_PATCH,2)
!
IF(IO%CRESPSL=='DIF')THEN
  INL          = SIZE(PSOIL_LITTER,2)
  IWG_LAYER(:) = PK%NWG_LAYER(:)
ELSE
  INL          = 1
  IWG_LAYER(:) = 1
ENDIF
! 1.1 Initialize outputs
!
DEK%XFIRETURNOVER (:) = 0.0
DEK%XFIRECO2      (:) = 0.0
DEK%XFIREBCS      (:) = 0.0
!
! 1.2 Initialize local variables
!
ZFIRECO2(:) = 0.0
ZFIREBCS(:) = 0.0
ZCBUDGET(:) = 0.0
!
ZEMITC_BIOM     (:,:) = 0.0
ZFIRECO2_BIOM(:,:) = 0.0
ZFIREBCS_BIOM(:,:) = 0.0
ZTURNOVER       (:,:) = 0.0
ZEMITC_LITTER   (:,:) = 0.0
ZFIRECO2_LITTER (:,:) = 0.0
ZFIREBCS_LITTER (:,:) = 0.0
!
! 1.3 Compute initial carbon budget
!
ZVEG_IN(:)=0.
DO JB=1,INB
   DO JI=1,INI
      ZVEG_IN(JI)=ZVEG_IN(JI) + PEK%XBIOMASS(JI,JB)*XPCCO2 ! kgC m-2
   ENDDO
ENDDO
!
ZLIT_IN_SURF(:)=0.
DO JLIT=1,INLIT
   DO JI=1,INI
      ZLIT_IN_SURF(JI)=ZLIT_IN_SURF(JI) + PSURF_LITTER(JI,JLIT) * XGTOKG ! kgC m-2
   ENDDO
ENDDO
!
ZLIT_IN_SOIL(:)=0.
DO JLIT=1,INLIT
   DO JL=1,INL
      DO JI=1,INI
         IF(JL<=IWG_LAYER(JI))THEN
            ZLIT_IN_SOIL(JI)=ZLIT_IN_SOIL(JI) + PSOIL_LITTER(JI,JL,JLIT) * XGTOKG ! kgC m-2
         ENDIF
      ENDDO
   ENDDO
ENDDO
!
! 1.4 Characterized vegtype assemblage in each patch
!
ZFEECO2  (:) = 0.0
ZFEEBSC  (:) = 0.0
ZTREE    (:) = 0.0
ZCCLITTER(:) = 0.0
!
DO JTYPE=1,INTYPE
   DO JI = 1,INI
      ZFEECO2  (JI) = ZFEECO2  (JI) + XFEECO2  (JTYPE) * PK%XVEGTYPE_PATCH(JI,JTYPE)
      ZFEEBSC  (JI) = ZFEEBSC  (JI) + XFEEBSC  (JTYPE) * PK%XVEGTYPE_PATCH(JI,JTYPE)
      ZTREE    (JI) = ZTREE    (JI) + XTREE    (JTYPE) * PK%XVEGTYPE_PATCH(JI,JTYPE)
      ZCCLITTER(JI) = ZCCLITTER(JI) + XCCLITTER(JTYPE) * PK%XVEGTYPE_PATCH(JI,JTYPE)
   ENDDO 
ENDDO  
!
! kgCO2/kgDM -> kgC/kgDM
ZFEECO2(:) = MIN(1.0,ZFEECO2(:)*XMC/XMCO2)
!
ZCCBIOMASS(:,:) = 0.0
ZMORTALITY(:,:) = 0.0
!
DO JTYPE=1,INTYPE
   DO JI = 1,INI
!
      ZCCBIOMASS(JI,ILEAF           ) = ZCCBIOMASS(JI,ILEAF           ) + XCCLEAF(JTYPE) * PK%XVEGTYPE_PATCH(JI,JTYPE)
      ZCCBIOMASS(JI,ISAPABOVEACTIVE ) = ZCCBIOMASS(JI,ISAPABOVEACTIVE ) + XCCLEAF(JTYPE) * PK%XVEGTYPE_PATCH(JI,JTYPE)
      ZCCBIOMASS(JI,ISAPABOVEPASSIVE) = ZCCBIOMASS(JI,ISAPABOVEPASSIVE) + XCCLEAF(JTYPE) * PK%XVEGTYPE_PATCH(JI,JTYPE)
      ZCCBIOMASS(JI,IWOODABOVE      ) = ZCCBIOMASS(JI,IWOODABOVE      ) + XCCSTEM(JTYPE) * PK%XVEGTYPE_PATCH(JI,JTYPE)
!
      ZMORTALITY(JI,ILEAF           ) = ZMORTALITY(JI,ILEAF           ) + XMLEAF(JTYPE) * PK%XVEGTYPE_PATCH(JI,JTYPE)
      ZMORTALITY(JI,ISAPABOVEACTIVE ) = ZMORTALITY(JI,ISAPABOVEACTIVE ) + XMLEAF(JTYPE) * PK%XVEGTYPE_PATCH(JI,JTYPE)
      ZMORTALITY(JI,ISAPABOVEPASSIVE) = ZMORTALITY(JI,ISAPABOVEPASSIVE) + XMLEAF(JTYPE) * PK%XVEGTYPE_PATCH(JI,JTYPE)
      ZMORTALITY(JI,IROOT           ) = ZMORTALITY(JI,IROOT           ) + XMROOT(JTYPE) * PK%XVEGTYPE_PATCH(JI,JTYPE)
      ZMORTALITY(JI,IWOODABOVE      ) = ZMORTALITY(JI,IWOODABOVE      ) + XMSTEM(JTYPE) * PK%XVEGTYPE_PATCH(JI,JTYPE)
      ZMORTALITY(JI,IWOODBELOW      ) = ZMORTALITY(JI,IWOODBELOW      ) + XMROOT(JTYPE) * PK%XVEGTYPE_PATCH(JI,JTYPE)
!
   ENDDO 
ENDDO  
!
! no root emissions
ZCCBIOMASS(:,IROOT     ) = 0.00
ZCCBIOMASS(:,IWOODBELOW) = 0.00 
!
!-----------------------------------------------------------------
!
! 2. Determine fire impact: calculate fire disturbance for each PFT
!
! 2.1 fire disturbance
!
!Trees are always disturbed
!Grasses are not disturbed if they are not in their growing season (because they don't exist)
!
ZDISTURB(:) = DMK%XFIREFRA(:)

WHERE(ZTREE(:)<0.8.AND.PEK%XLAI(:)<=PEK%XLAIMIN(:))
      ZDISTURB(:) = 0.0
ENDWHERE
!
! 2.2 Amount of gas species emitted to atmosphere from biomass fire and plant mortality
!
DO JB = 1,INB
!
   DO JI = 1,INI
!
!     2.2.1 Emitted carbon from biomass fire (kgDM/m2/day)
!
      ZEMITC_BIOM(JI,JB) = ZDISTURB(JI) * ZCCBIOMASS(JI,JB) * PEK%XBIOMASS(JI,JB) / PTSTEP_DAY
!
!     2.2.2 Amount of CO2 emissions (kgDM/m2/day -> kgC/m2/day)
!
      ZFIRECO2_BIOM(JI,JB) = ZEMITC_BIOM(JI,JB) * ZFEECO2(JI)
!
      DEK%XFIRECO2(JI) = DEK%XFIRECO2(JI) + ZFIRECO2_BIOM(JI,JB) 
!
!     2.2.3 Amount of Black carbon emissions (kgDM/m2/day -> kgBC/m2/day)
!
      ZFIREBCS_BIOM(JI,JB) = ZEMITC_BIOM(JI,JB) * ZFEEBSC(JI)
!
      DEK%XFIREBCS(JI) = DEK%XFIREBCS(JI) + ZFIREBCS_BIOM(JI,JB) 
!
!     2.2.4 Determine biomass mortality (kgDM/m2/day -> kgC/m2/day)
!
      ZTURNOVER(JI,JB) = ZDISTURB(JI) * (1.0-ZCCBIOMASS(JI,JB)) * ZMORTALITY(JI,JB) * PEK%XBIOMASS(JI,JB) * (XPCCO2/PTSTEP_DAY)
!
!     2.2.5 This residue becomes litter
!
      DEK%XFIRETURNOVER(JI) = DEK%XFIRETURNOVER(JI) + ZTURNOVER(JI,JB)
!
   ENDDO
!
ENDDO
!
! 2.3 New vegetation characteristics (kgC/m2/day -> kgDM/m2)
!
DO JB = 1,INB
   DO JI = 1,INI
      PEK%XBIOMASS(JI,JB) = PEK%XBIOMASS(JI,JB)-(ZFIRECO2_BIOM(JI,JB)+ZFIREBCS_BIOM(JI,JB)+ZTURNOVER(JI,JB)) * (PTSTEP_DAY/XPCCO2)
   ENDDO
ENDDO
!
!-----------------------------------------------------------------
!
! 3. Determine fire impact: calculate fire disturbance for above ground metabolic litter
!
DO JLIT=1,INLIT
   DO JI=1,INI
!
!     3.1 Emitted carbon from Metabolic litter fire (kgC/m2/day)
!
      ZEMITC_LITTER(JI,JLIT) = DMK%XFIREFRA(JI) * ZCCLITTER(JI) * PSURF_LITTER(JI,JLIT) * (XGTOKG/PTSTEP_DAY)
!
!     3.2 Amount of CO2 emissions (kgCO2/m2/day -> kgC/m2/day)
!
      ZFIRECO2_LITTER(JI,JLIT) = ZEMITC_LITTER(JI,JLIT) * ZFEECO2(JI) / XPCCO2
!
      DEK%XFIRECO2(JI) = DEK%XFIRECO2(JI) + ZFIRECO2_LITTER(JI,JLIT)
!
!     3.3 Amount of Black carbon emissions (kgBC/m2/day)
! 
      ZFIREBCS_LITTER(JI,JLIT) =  ZEMITC_LITTER(JI,JLIT) * ZFEEBSC(JI) / XPCCO2
!
      DEK%XFIREBCS(JI) = DEK%XFIREBCS(JI) + ZFIREBCS_LITTER(JI,JLIT)
!
  ENDDO
ENDDO
!
! 3.4 Update litter form species emissions (gC/m2)
!
PSURF_LITTER(:,:) = PSURF_LITTER(:,:) - (ZFIRECO2_LITTER(:,:)+ZFIREBCS_LITTER(:,:)) * (XKGTOG*PTSTEP_DAY)
!
ZTURNOVER(:,:) = ZTURNOVER(:,:) * PTSTEP_DAY
!
CALL BIOMASS_TO_SURFACE_LITTER(ZTURNOVER(:,:),PSURF_LITTER(:,:),PSURF_LIGNIN(:))
!
IF(IO%CRESPSL=='DIF')THEN
  CALL BIOMASS_TO_SOIL_LITTER(ZTURNOVER(:,:),PSOIL_LITTER(:,:,:),PSOIL_LIGNIN(:,:), &
                              KWG_LAYER=PK%NWG_LAYER, PROOTFRAC=PK%XROOTFRAC        )
ELSE
  CALL BIOMASS_TO_SOIL_LITTER(ZTURNOVER(:,:),PSOIL_LITTER(:,:,:),PSOIL_LIGNIN(:,:))
ENDIF
!
!-----------------------------------------------------------------
!
! 4. Closure of c budget (kgC m-2)
!
ZVEG_OUT(:)=0.
DO JB=1,INB
   DO JI=1,INI
      ZVEG_OUT(JI)=ZVEG_OUT(JI) + PEK%XBIOMASS(JI,JB)*XPCCO2 ! kgC m-2
   ENDDO
ENDDO
!
ZLIT_OUT_SURF(:)=0.
DO JLIT=1,INLIT
   DO JI=1,INI
      ZLIT_OUT_SURF(JI)=ZLIT_OUT_SURF(JI) + PSURF_LITTER(JI,JLIT) * XGTOKG ! kgC m-2
   ENDDO
ENDDO
!
ZLIT_OUT_SOIL(:)=0.
DO JLIT=1,INLIT
   DO JL=1,INL
      DO JI=1,INI
         IF(JL<=IWG_LAYER(JI))THEN
           ZLIT_OUT_SOIL(JI)=ZLIT_OUT_SOIL(JI) + PSOIL_LITTER(JI,JL,JLIT) * XGTOKG ! kgC m-2
         ENDIF
      ENDDO
   ENDDO
ENDDO
!
ZCBUDGET(:)= (ZVEG_IN (:)+ZLIT_IN_SURF (:)+ZLIT_IN_SOIL (:))  &
           - (ZVEG_OUT(:)+ZLIT_OUT_SURF(:)+ZLIT_OUT_SOIL(:))  &
           - (DEK%XFIRECO2(:)+DEK%XFIREBCS(:))*PTSTEP_DAY
!
DO JI=1,INI
   IF(ABS(ZCBUDGET(JI))>XMIN_FIRE)THEN
      WRITE(KLUOUT,*) '!!!!!!!!!!!!!!!!!!!!!!!!!'
      WRITE(KLUOUT,*) 'PROBLEM FIRE CONSERVATION'
      WRITE(KLUOUT,*) 'Bilan = ',ZCBUDGET(JI)
      WRITE(KLUOUT,*) 'Grid-cell = ',JI
      WRITE(KLUOUT,*) '!!!!!!!!!!!!!!!!!!!!!!!!!'
      CALL ABOR1_SFX('FIRE_CARBON: PROBLEM FIRE CONSERVATION')          
   ENDIF
ENDDO
!
! ------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('FIRE_CARBON',1,ZHOOK_HANDLE)
!
END SUBROUTINE FIRE_CARBON
