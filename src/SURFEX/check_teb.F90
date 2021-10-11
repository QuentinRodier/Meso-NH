!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ###############################################################################
SUBROUTINE CHECK_TEB (TOP, BOP, NT, NB, TD, TPN, TIR, GDM, GRM, HM, CT, &
                      HPROGRAM, KI, JP, PTSTEP, PTSUN, PRAIN, PSN)
  !     ###############################################################################
  !
  !!****  *COUPLING_TEB_n * - Driver for TEB 
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
  !!     R. Schoetter
  !!
  !!    MODIFICATIONS
  !!    -------------
  !!      Original    2017
  !!      V. Masson   04.2020 completes energy check for high vegetation IR exchanges
  !!      V. Masson   04.2020 put in a separate routine
  !!---------------------------------------------------------------
  !
  USE MODD_TEB_n, ONLY : TEB_NP_t
  USE MODD_BEM_n, ONLY : BEM_NP_t
  USE MODD_TEB_PANEL_n, ONLY : TEB_PANEL_t
  USE MODD_TEB_OPTION_n, ONLY : TEB_OPTIONS_t
  USE MODD_BEM_OPTION_n, ONLY : BEM_OPTIONS_t
  USE MODD_TEB_IRRIG_n, ONLY : TEB_IRRIG_t
  !
  USE MODD_SURFEX_n, ONLY : TEB_GARDEN_MODEL_t
  USE MODD_SURFEX_n, ONLY : TEB_GREENROOF_MODEL_t
  USE MODD_SURFEX_n, ONLY : TEB_HYDRO_MODEL_t
  USE MODD_SURFEX_n, ONLY : TEB_DIAG_t
  !
  USE MODD_CHECK_TEB, ONLY : CHECK_TEB_t

  USE MODD_CSTS
  USE MODD_SURF_PAR,     ONLY : XUNDEF
  ! 
  USE YOMHOOK, ONLY : LHOOK, DR_HOOK
  USE PARKIND1, ONLY : JPRB
  !
  USE MODI_ABOR1_SFX
  !
  IMPLICIT NONE
  !
  !*      0.1    declarations of arguments
  !
  TYPE(TEB_PANEL_t), INTENT(INOUT) :: TPN
  TYPE(TEB_IRRIG_t), INTENT(INOUT) :: TIR
  TYPE(TEB_NP_t), INTENT(INOUT) :: NT
  TYPE(TEB_OPTIONS_t), INTENT(INOUT) :: TOP
  TYPE(TEB_DIAG_t), INTENT(INOUT) :: TD
  TYPE(BEM_OPTIONS_t), INTENT(INOUT) :: BOP 
  TYPE(BEM_NP_t), INTENT(INOUT) :: NB
  TYPE(TEB_GARDEN_MODEL_t), INTENT(INOUT) :: GDM
  TYPE(TEB_GREENROOF_MODEL_t), INTENT(INOUT) :: GRM
  TYPE(TEB_HYDRO_MODEL_t), INTENT(INOUT) :: HM
  TYPE(CHECK_TEB_t), INTENT(INOUT) :: CT
  !
  CHARACTER(LEN=6),   INTENT(IN) :: HPROGRAM ! program calling surf. schemes
  INTEGER,            INTENT(IN) :: KI       ! number of points
  INTEGER,            INTENT(IN) :: JP       ! TEB current patch number
  REAL,               INTENT(IN) :: PTSTEP   ! time step
  REAL, DIMENSION(:), INTENT(IN) :: PTSUN    ! solar hour from solar midnight (sec)
  REAL, DIMENSION(:), INTENT(IN) :: PRAIN    ! rainfall rate
  REAL, DIMENSION(:), INTENT(IN) :: PSN      ! snowfall rate
  !
  !*      0.2    declarations of local variables
  !
  REAL, DIMENSION(KI) :: ZTHEWALL
  REAL, DIMENSION(KI) :: ZTHEROOF
  REAL, DIMENSION(KI) :: ZTHEFLOOR
  REAL, DIMENSION(KI) :: ZTHESOILBLD
  REAL, DIMENSION(KI) :: ZTHEMASS
  REAL, DIMENSION(KI) :: ZTHEROAD
  REAL, DIMENSION(KI) :: ZTHEAIRIN
  REAL, DIMENSION(KI) :: ZTHETOTAL
  REAL, DIMENSION(KI) :: ZLATWATROOF
  REAL, DIMENSION(KI) :: ZLATWATROAD
  REAL, DIMENSION(KI) :: ZLATICEROOF
  REAL, DIMENSION(KI) :: ZLATICEROAD
  REAL, DIMENSION(KI) :: ZLATAIRIN
  REAL, DIMENSION(KI) :: ZLATSOILROAD
  REAL, DIMENSION(KI) :: ZLATSOILBLD
  REAL, DIMENSION(KI) :: ZLATTOTAL
  REAL, DIMENSION(KI) :: ZENETOTAL
  !
  !
  ! Difference to previous time step
  !
  REAL, DIMENSION(KI) :: ZDIFFTHEWALL
  REAL, DIMENSION(KI) :: ZDIFFTHEROOF
  REAL, DIMENSION(KI) :: ZDIFFTHEFLOOR
  REAL, DIMENSION(KI) :: ZDIFFTHESOILBLD
  REAL, DIMENSION(KI) :: ZDIFFTHEMASS
  REAL, DIMENSION(KI) :: ZDIFFTHEROAD
  REAL, DIMENSION(KI) :: ZDIFFTHESNOWROAD
  REAL, DIMENSION(KI) :: ZDIFFTHESNOWROOF
  REAL, DIMENSION(KI) :: ZDIFFTHEAIRIN
  REAL, DIMENSION(KI) :: ZDIFFTHETOTAL
  REAL, DIMENSION(KI) :: ZDIFFLATWATROOF
  REAL, DIMENSION(KI) :: ZDIFFLATWATROAD
  REAL, DIMENSION(KI) :: ZDIFFLATICEROOF
  REAL, DIMENSION(KI) :: ZDIFFLATICEROAD
  REAL, DIMENSION(KI) :: ZDIFFLATAIRIN
  REAL, DIMENSION(KI) :: ZDIFFLATSOILROAD
  REAL, DIMENSION(KI) :: ZDIFFLATSOILBLD
  REAL, DIMENSION(KI) :: ZDIFFLATTOTAL
  REAL, DIMENSION(KI) :: ZDIFFENETOTAL
  !
  REAL, DIMENSION(KI) :: ZSRCSENFLX
  REAL, DIMENSION(KI) :: ZSRCLATFLX
  REAL, DIMENSION(KI) :: ZSRCNETRAD
  REAL, DIMENSION(KI) :: ZSRCPROPAN
  REAL, DIMENSION(KI) :: ZSRCSTOGAR
  REAL, DIMENSION(KI) :: ZSRCSENINT
  REAL, DIMENSION(KI) :: ZSRCLATINT
  REAL, DIMENSION(KI) :: ZSRCHVACCL
  REAL, DIMENSION(KI) :: ZSRCHVACHT
  REAL, DIMENSION(KI) :: ZSRCFLXFLO
  REAL, DIMENSION(KI) :: ZSRCFLXROF
  REAL, DIMENSION(KI) :: ZSRCFLXWAL
  REAL, DIMENSION(KI) :: ZSRCHTRAFF
  REAL, DIMENSION(KI) :: ZSRCLTRAFF
  REAL, DIMENSION(KI) :: ZSRCHINDUS
  REAL, DIMENSION(KI) :: ZSRCLINDUS
  REAL, DIMENSION(KI) :: ZSRCLATRAI
  REAL, DIMENSION(KI) :: ZSRCLATSNO
  REAL, DIMENSION(KI) :: ZSRCLATROI
  REAL, DIMENSION(KI) :: ZSRCLATGRI
  REAL, DIMENSION(KI) :: ZSRCRUNROF
  REAL, DIMENSION(KI) :: ZSRCRUNROD
  REAL, DIMENSION(KI) :: ZSRCRUNSOILBLD
  REAL, DIMENSION(KI) :: ZSRCRUNSOILROD
  REAL, DIMENSION(KI) :: ZSRCDRAINBLD
  REAL, DIMENSION(KI) :: ZSRCDRAINROD
  REAL, DIMENSION(KI) :: ZSRCNOCROF
  REAL, DIMENSION(KI) :: ZSRCNOCROD
  REAL, DIMENSION(KI) :: ZSRCSEWER
  REAL, DIMENSION(KI) :: ZSRCSTORM
  REAL, DIMENSION(KI) :: ZSRCSENSUM
  REAL, DIMENSION(KI) :: ZSRCLATSUM
  REAL, DIMENSION(KI) :: ZSRCALLSUM
  !
  LOGICAL, DIMENSION(KI) :: GCHECK_BUDGET
  !
  INTEGER              :: JLAYER
  INTEGER              :: JCOMP
  INTEGER              :: JJ
  !
  INTEGER              :: ILUOUT     ! Unit number
  !
  REAL(KIND=JPRB) :: ZHOOK_HANDLE
  !
  !-------------------------------------------------------------------------------------
  ! Preliminaries:
  !-------------------------------------------------------------------------------------
  IF (LHOOK) CALL DR_HOOK('CHECK_TEB',0,ZHOOK_HANDLE)
  CALL GET_LUOUT(HPROGRAM,ILUOUT)
  !
  !-------------------------------------------------------------------------------------
  !
  ! ###############################################################
  ! ###############################################################
  ! Robert: Verification of energy conservation
  ! ###############################################################
  ! ###############################################################
  !
  ! ############################################################
  ! A: Sensible heat stored
  ! ############################################################
  !
  ! Sensible heat stored in the wall [J/m²(urb)]
  !
  ZTHEWALL(:)=0.0
  DO JLAYER=1,SIZE(NT%AL(JP)%XD_WALL,2)
     ZTHEWALL(:) = ZTHEWALL(:) + NT%AL(JP)%XWALL_O_BLD(:)*NT%AL(JP)%XBLD(:) * &
          NT%AL(JP)%XD_WALL(:,JLAYER)*NT%AL(JP)%XHC_WALL(:,JLAYER)          * &
          0.5*(NT%AL(JP)%XT_WALL_A(:,JLAYER)+NT%AL(JP)%XT_WALL_B(:,JLAYER))
  ENDDO
  !
  ! Sensible heat stored in the roof [J/m²(urb)]
  !
  ZTHEROOF(:)=0.0
  DO JLAYER=1,SIZE(NT%AL(JP)%XD_ROOF,2)
     ZTHEROOF(:) = ZTHEROOF(:) +       NT%AL(JP)%XBLD(:) * &
          NT%AL(JP)%XD_ROOF(:,JLAYER)*NT%AL(JP)%XHC_ROOF(:,JLAYER)*NT%AL(JP)%XT_ROOF(:,JLAYER)
  ENDDO
  !
  ! Sensible heat stored in the ground floor and the soil below [J/m²(urb)]
  !
  ZTHEFLOOR(:)=0.0
  IF (TOP%CBEM=='BEM') THEN
     DO JLAYER=1,SIZE(NB%AL(JP)%XT_FLOOR,2)
        DO JCOMP=1,BOP%NBEMCOMP
           ZTHEFLOOR(:) = ZTHEFLOOR(:) + NB%AL(JP)%XFRACOMP(:,JCOMP) * &
                NT%AL(JP)%XBLD(:) * NB%AL(JP)%XD_FLOOR(:,JLAYER) *         &
                NB%AL(JP)%XHC_FLOOR(:,JLAYER)*NB%AL(JP)%XT_FLOOR(:,JLAYER,JCOMP)
        ENDDO
     ENDDO
  END IF
  !
  ZTHESOILBLD(:)=0.0
  DO JLAYER=1,SIZE(NT%AL(JP)%XT_BLD,2)
     ZTHESOILBLD(:) = ZTHESOILBLD(:) + &
          NT%AL(JP)%XBLD(:) * NT%AL(JP)%XD_BLD(:,JLAYER) *         &
          NT%AL(JP)%XHC_BLD(:,JLAYER)*NT%AL(JP)%XT_BLD(:,JLAYER)
  ENDDO
  !
  !
  ! Sensible heat stored in the mass [J/m²(urb)]
  ! The mass temperature can be averaged over the compartments
  !
  ZTHEMASS(:) = 0.0
  IF (TOP%CBEM=='BEM') THEN
     DO JJ=1,SIZE(NB%AL(JP)%XN_FLOOR)
        IF (NB%AL(JP)%XN_FLOOR(JJ).GT.1.5) THEN
           DO JLAYER=1,SIZE(NB%AL(JP)%XT_MASS,2)
              DO JCOMP=1,BOP%NBEMCOMP
                 ZTHEMASS(JJ) = ZTHEMASS(JJ) + NB%AL(JP)%XFRACOMP(JJ,JCOMP) * &
                      NT%AL(JP)%XBLD(JJ) * NB%AL(JP)%XMASS_O_BLD(JJ) *            &
                      NB%AL(JP)%XD_MASS(JJ,JLAYER)*NB%AL(JP)%XHC_MASS(JJ,JLAYER)* &
                      NB%AL(JP)%XT_MASS(JJ,JLAYER,JCOMP)
              ENDDO
           ENDDO
        ELSE
           ZTHEMASS(JJ)=0.0
        ENDIF
     ENDDO
  END IF
  !
  ! Sensible heat stored in the road [J/m²(urb)]
  !
  ZTHEROAD(:)=0.0
  DO JLAYER=1,SIZE(NT%AL(JP)%XT_ROAD,2)
     ZTHEROAD(:) = ZTHEROAD(:) +  NT%AL(JP)%XROAD(:)           * &
          NT%AL(JP)%XD_ROAD(:,JLAYER)*NT%AL(JP)%XHC_ROAD(:,JLAYER) * &
          NT%AL(JP)%XT_ROAD(:,JLAYER)
  ENDDO
  !
  ! Sensible heat stored in the indoor air [J/m²(urb)]
  !
  ZTHEAIRIN(:)=0.0
  IF (TOP%CBEM=='BEM') THEN
     IF (TOP%CBEM.EQ."BEM") THEN
        DO JCOMP=1,BOP%NBEMCOMP
           ZTHEAIRIN(:) =  ZTHEAIRIN(:) + NB%AL(JP)%XFRACOMP(:,JCOMP) * & 
                NT%AL(JP)%XBLD(:)*XCPD*NT%AL(JP)%XBLD_HEIGHT(:)*CT%XRHOI(:,JCOMP)*NB%AL(JP)%XTI_BLD(:,JCOMP)
        ENDDO
     ENDIF
  END IF
  !
  ! Total sensible heat stored [J/m²(urb)]
  !
  ZTHETOTAL(:)=ZTHEWALL(:)+ZTHEROOF(:)+ZTHEFLOOR(:)+ZTHESOILBLD(:)+ZTHEMASS(:)+ &
       ZTHEROAD(:)+ZTHEAIRIN(:)
  !
  ! ####################################################
  ! B: Latent heat stored (water or snow/ice stored)
  ! ####################################################
  !
  ! Potential equivalent Latent heat stored in the roof water and ice reservoir [J/m²(urb)]
  !
  ZLATWATROOF(:)=(1.0-NT%AL(JP)%XGREENROOF(:))*NT%AL(JP)%XBLD(:)*XLVTT*NT%AL(JP)%XWS_ROOF(:)
  ZLATICEROOF(:)=(1.0-NT%AL(JP)%XGREENROOF(:))*NT%AL(JP)%XBLD(:)*XLSTT*NT%AL(JP)%TSNOW_ROOF%WSNOW(:,1)
  !
  ! Potential equivalent Latent heat stored in the road water and ice reservoir [J/m²(urb)]
  !
  ZLATWATROAD(:)=NT%AL(JP)%XROAD(:)*XLVTT*NT%AL(JP)%XWS_ROAD(:)
  ZLATICEROAD(:)=NT%AL(JP)%XROAD(:)*XLSTT*NT%AL(JP)%TSNOW_ROAD%WSNOW(:,1)
  !
  ! Potential equivalent Latent heat stored in the indoor air humidity [J/m²(urb)]
  !
  ZLATAIRIN(:)=0.0
  IF (TOP%CBEM.EQ."BEM") THEN
     DO JCOMP=1,BOP%NBEMCOMP
        ZLATAIRIN(:) = ZLATAIRIN(:) + NB%AL(JP)%XFRACOMP(:,JCOMP) * NT%AL(JP)%XBLD(:)* &
             XLVTT*NT%AL(JP)%XBLD_HEIGHT(:)*CT%XRHOI(:,JCOMP)*NB%AL(JP)%XQI_BLD(:,JCOMP)
     ENDDO
  ENDIF
  !
  ! Variation in soil water contents
  !
  ZLATSOILROAD  (:)=0.0
  ZLATSOILBLD   (:)=0.0
  IF (TOP%LURBHYDRO) THEN
     DO JLAYER=1,SIZE(NT%AL(JP)%XT_ROAD,2)
         ZLATSOILROAD(:)  = ZLATSOILROAD(:) + NT%AL(JP)%XROAD(:)   * NT%AL(JP)%XD_ROAD(:,JLAYER) * XRHOLW * &
                          ( HM%NTH%AL(JP)%XWG_ROAD(:,JLAYER) * XLVTT + HM%NTH%AL(JP)%XWGI_ROAD(:,JLAYER) * XLSTT )
         ZLATSOILBLD (:)  = ZLATSOILBLD (:) + NT%AL(JP)%XBLD (:)   * NT%AL(JP)%XD_ROAD(:,JLAYER) * XRHOLW * &
                          ( HM%NTH%AL(JP)%XWG_BLD (:,JLAYER) * XLVTT + HM%NTH%AL(JP)%XWGI_BLD (:,JLAYER) * XLSTT )
      ENDDO
   ENDIF
   !
   !
   ! Total latent heat stored [J/m²(urb)]
   !
     ZLATTOTAL(:)=ZLATWATROOF(:)+ZLATICEROOF(:)+ZLATWATROAD(:)+ZLATICEROAD(:)+ZLATAIRIN(:) &
               + ZLATSOILROAD(:) + ZLATSOILBLD(:)
  !
  ! ####################################################
  ! C: Total stored heat & latent heat
  ! ####################################################
  !
  !
  ! Total energy stored in TEB-BEM [J/m²(urb)]
  !
  ZENETOTAL(:)=ZLATTOTAL(:)+ZTHETOTAL(:)
  !
  ! #######################################################
  ! E: Differences of storage terms to previous time step
  ! #######################################################
  !
  ZDIFFTHEAIRIN(:)   = (ZTHEAIRIN(:)   - NT%AL(JP)%XTHEAIRIN(:))/PTSTEP
  ZDIFFTHEWALL(:)    = (ZTHEWALL(:)    - NT%AL(JP)%XTHEWALL(:))/PTSTEP
  ZDIFFTHEROOF(:)    = (ZTHEROOF(:)    - NT%AL(JP)%XTHEROOF(:))/PTSTEP
  ZDIFFTHEFLOOR(:)   = (ZTHEFLOOR(:)   - NT%AL(JP)%XTHEFLOOR(:))/PTSTEP
  ZDIFFTHESOILBLD(:) = (ZTHESOILBLD(:) - NT%AL(JP)%XTHESOILBLD(:))/PTSTEP
  ZDIFFTHEMASS(:)    = (ZTHEMASS(:)    - NT%AL(JP)%XTHEMASS(:))/PTSTEP
  ZDIFFTHEROAD(:)    = (ZTHEROAD(:)    - NT%AL(JP)%XTHEROAD(:))/PTSTEP
  ZDIFFTHESNOWROAD(:)= NT%AL(JP)%XROAD(:)*TD%NDMT%AL(JP)%XDN_ROAD(:)*CT%XDQS_SNOW_ROAD(:)
  ZDIFFTHESNOWROOF(:)= (1.0-NT%AL(JP)%XGREENROOF(:))*NT%AL(JP)%XBLD(:)*TD%NDMT%AL(JP)%XDN_ROOF(:)*CT%XDQS_SNOW_ROOF(:)
  ZDIFFTHETOTAL(:)   = (ZTHETOTAL(:)   - NT%AL(JP)%XTHETOTAL(:))/PTSTEP
  ZDIFFLATWATROOF(:) = (ZLATWATROOF(:) - NT%AL(JP)%XLATWATROOF(:))/PTSTEP
  ZDIFFLATWATROAD(:) = (ZLATWATROAD(:) - NT%AL(JP)%XLATWATROAD(:))/PTSTEP
  ZDIFFLATICEROOF(:) = (ZLATICEROOF(:) - NT%AL(JP)%XLATICEROOF(:))/PTSTEP
  ZDIFFLATICEROAD(:) = (ZLATICEROAD(:) - NT%AL(JP)%XLATICEROAD(:))/PTSTEP
  ZDIFFLATAIRIN(:)   = (ZLATAIRIN(:)   - NT%AL(JP)%XLATAIRIN(:))/PTSTEP
  ZDIFFLATSOILROAD(:)= (ZLATSOILROAD(:)- NT%AL(JP)%XLATSOILROAD(:))/PTSTEP
  ZDIFFLATSOILBLD(:) = (ZLATSOILBLD(:) - NT%AL(JP)%XLATSOILBLD(:))/PTSTEP
  ZDIFFLATTOTAL(:)   = (ZLATTOTAL(:)   - NT%AL(JP)%XLATTOTAL(:))/PTSTEP
  ZDIFFENETOTAL(:)   = (ZENETOTAL(:)   - NT%AL(JP)%XENETOTAL(:))/PTSTEP
  !
  ! #######################################################
  ! D: Sources of sensible and latent heat
  ! #######################################################
  !
  ! Sensible and latent heat flux (W/m²(urb))
  !
  ZSRCSENFLX(:) = -CT%XH(:)
  ZSRCLATFLX(:) = -CT%XLE(:)
  !
  ! Net radiation (W/m²(urb))
  !
  ZSRCNETRAD(:) = CT%XRN(:)
  !
  ! Production by solar panels (W/m²(urb))
  !
  IF (TOP%LSOLAR_PANEL) THEN
     ZSRCPROPAN(:)=NT%AL(JP)%XBLD(:)*(TD%NDMT%AL(JP)%XTHER_PROD_BLD  (:) + TD%NDMT%AL(JP)%XPHOT_PROD_BLD(:))
     ELSE
        ZSRCPROPAN(:)=0.0
     ENDIF
     !
     ! Storage flux in garden (W/m²(urb))
     !
     IF (TOP%LGARDEN) THEN
        IF (TOP%LGARDEN .AND. TOP%CURBTREE/='NONE') THEN
          ZSRCSTOGAR(:) = - NT%AL(JP)%XGARDEN(:) * GDM%VD%ND%AL(JP)%XGFLUX - NT%AL(JP)%XFRAC_HVEG(:) * TD%NDMT%AL(JP)%XGFLUX_HVEG
        ELSE
          ZSRCSTOGAR(:) = - NT%AL(JP)%XGARDEN(:) * GDM%VD%ND%AL(JP)%XGFLUX
        ENDIF
     ELSE
        ZSRCSTOGAR(:) = 0.0
     ENDIF
     !
     ! Anthropogenic sources (W/m²(urb))
     !
     IF (TOP%CBEM.EQ."BEM") THEN
        !
        ZSRCSENINT(:) = TD%NDMT%AL(JP)%XQINOUT(:)*(1.0-NB%AL(JP)%XQIN_FLAT(:))
        ZSRCLATINT(:) = TD%NDMT%AL(JP)%XQINOUT(:)*(    NB%AL(JP)%XQIN_FLAT(:))
        !
        ZSRCHVACCL(:) = TD%NDMT%AL(JP)%XHVAC_COOL(:)
        ZSRCHVACHT(:) = TD%NDMT%AL(JP)%XHVAC_HEAT(:)
        !
        CT%XDIAG_DCS_AREA(:) = CT%XDIAG_DCS_AREA(:)
        !
     ELSE
        ZSRCSENINT(:) = 0.0
        ZSRCLATINT(:) = 0.0
        ZSRCHVACCL(:) = 0.0
        ZSRCHVACHT(:) = 0.0
        CT%XDIAG_DCS_AREA(:) = 0.0
     ENDIF
     !
     ! For CBEM=DEF the heat flux into the ground floor and convective 
     ! air exchange between indoor air and the roof and walls become
     ! source terms
     !
     IF (TOP%CBEM.EQ."DEF") THEN
        ZSRCFLXFLO(:) = CT%XFLUXFLOOR(:)
        ZSRCFLXROF(:) = NT%AL(JP)%XBLD(:)*CT%XFLX_BLD_ROOF(:)
        ZSRCFLXWAL(:) = NT%AL(JP)%XBLD(:)*NT%AL(JP)%XWALL_O_BLD(:)*0.5*(CT%XFLX_BLD_WALL_A(:)+CT%XFLX_BLD_WALL_B(:))
     ELSE
        ZSRCFLXFLO(:) = 0.0
        ZSRCFLXROF(:) = 0.0
        ZSRCFLXWAL(:) = 0.0
     ENDIF
     !
     ZSRCHTRAFF(:) = TD%NDMT%AL(JP)%XH_TRAFFIC_OUT(:)
     ZSRCLTRAFF(:) = TD%NDMT%AL(JP)%XLE_TRAFFIC_OUT(:)
     ZSRCHINDUS(:) = TD%NDMT%AL(JP)%XH_INDUSTRY_OUT (:)
     ZSRCLINDUS(:) = TD%NDMT%AL(JP)%XLE_INDUSTRY_OUT (:)
     !

     ! Potential equivalent Latent heat increase due to rain and snow
     ! on non-garden surfaces (W/m²(urb))
     !
     ZSRCLATRAI(:) = (1.0-NT%AL(JP)%XGARDEN(:))*XLVTT*PRAIN(:)
     ZSRCLATSNO(:) = (1.0-NT%AL(JP)%XGARDEN(:))*XLSTT*PSN(:)
     !
     ! Potential equivalent Latent heat increase due to road and garden irrigation (W/m²(urb))
     !
     ZSRCLATROI(:) = NT%AL(JP)%XROAD(:)*XLVTT*TD%NDMT%AL(JP)%XIRRIG_ROAD(:)
     ZSRCLATGRI(:) = XLVTT*NT%AL(JP)%XBLD(:)*NT%AL(JP)%XGREENROOF(:)*TD%NDMT%AL(JP)%XIRRIG_GREENROOF(:)
     !
     ! Potential equivalent Latent heat decrease due to run-off (W/m²(urb))
     !
     ZSRCRUNROF(:) = -NT%AL(JP)%XBLD(:) *XLVTT*TD%NDMT%AL(JP)%XRUNOFF_ROOF(:)
     ZSRCRUNROD(:) = -NT%AL(JP)%XROAD(:)*XLVTT*TD%NDMT%AL(JP)%XRUNOFF_ROAD(:)
     ZSRCRUNSOILROD(:) = -NT%AL(JP)%XROAD(:)*XLVTT*TD%NDMT%AL(JP)%XRUNOFFSOIL_ROAD(:)
     ZSRCRUNSOILBLD(:) = -NT%AL(JP)%XBLD (:)*XLVTT*TD%NDMT%AL(JP)%XRUNOFFSOIL_BLD (:)
     !
     ! Potential equivalent Latent heat decrease due to drainage (W/m²(urb))
     !
     ZSRCDRAINROD(:) = -NT%AL(JP)%XROAD(:)*XLVTT*TD%NDMT%AL(JP)%XDRAIN_ROAD(:)
     ZSRCDRAINBLD(:) = -NT%AL(JP)%XBLD (:)*XLVTT*TD%NDMT%AL(JP)%XDRAIN_BLD (:)
     !
     ! Potential equivalent Latent heat decrease to run-off not going to sewer (W/m²(urb)), going into gardens
     !
     ZSRCNOCROF(:) = -NT%AL(JP)%XBLD(:) *XLVTT*TD%NDMT%AL(JP)%XNOC_ROOF(:) &
                      * (1.-NT%AL(JP)%XGREENROOF(:)) ! non-connected runoff comes from structural roof only (not greenroofs)
     ZSRCNOCROD(:) = -NT%AL(JP)%XROAD(:)*XLVTT*TD%NDMT%AL(JP)%XNOC_ROAD(:)
     !
     ! Potential equivalent Latent heat decrease to subsurface infiltration to sewer and stormwater systems (W/m²(urb))
     !
     IF (TOP%LURBHYDRO) THEN
       ZSRCSEWER(:) = - NT%AL(JP)%XROAD(:) * XLVTT * TD%NDMT%AL(JP)%XRUNOFF_WW(:)
       ZSRCSTORM(:) = - NT%AL(JP)%XROAD(:) * XLVTT * TD%NDMT%AL(JP)%XRUNOFF_SW(:)
     ELSE
       ZSRCSEWER(:) = 0.
       ZSRCSTORM(:) = 0.
     END IF
     !
     ! horizontal transfert of liquid water from subsoil (road+bld) towards gardens
     !
     CT%XLAT_SOIL_TO_GARDEN(:) = - XLVTT * NT%AL(JP)%XGARDEN(:) * CT%XLAT_SOIL_TO_GARDEN(:)
     !
     ! Conversion from latent to sensible heat during
     ! condensation on roofs and roads (W/m²(urb))
     !
     CT%XCONV_LAT_SEN_ROAD(:) = NT%AL(JP)%XROAD(:)*CT%XCONV_LAT_SEN_ROAD(:)
     CT%XCONV_LAT_SEN_ROOF(:) = NT%AL(JP)%XBLD (:)*CT%XCONV_LAT_SEN_ROOF(:)
     !
     ! Heating/cooling of rain water falling on roofs and roads (W/m²(urb))
     !
     CT%XHEAT_RR_ROAD(:) = -NT%AL(JP)%XROAD(:)*CT%XHEAT_RR_ROAD(:)
     CT%XHEAT_RR_ROOF(:) = -NT%AL(JP)%XBLD (:)*CT%XHEAT_RR_ROOF(:)
     !
     ! Heating/cooling due to snow falling on roofs and road (W/m²(urb))
     !
     CT%XSEN_SNOW_DIF_ROAD(:)=NT%AL(JP)%XROAD(:)*CT%XSEN_SNOW_DIF_ROAD(:)
     CT%XSEN_SNOW_DIF_ROOF(:)=(1.0-NT%AL(JP)%XGREENROOF(:))*NT%AL(JP)%XBLD (:)*CT%XSEN_SNOW_DIF_ROOF(:)
     !
     ! Sum of sources and sinks separed for latent and sensible heat (W/m²(urb))
     !
     ZSRCSENSUM(:) = ZSRCSENFLX(:) + ZSRCNETRAD(:) - ZSRCPROPAN(:) + &
          ZSRCSENINT(:)+ZSRCFLXFLO + ZSRCFLXROF +  ZSRCFLXWAL       + &
          ZSRCHVACCL(:)+ZSRCHVACHT(:)+ZSRCHTRAFF(:)+ZSRCHINDUS(:)   + &
          CT%XHEAT_RR_ROAD(:)+CT%XHEAT_RR_ROOF(:)+CT%XSEN_SNOW_DIF_ROAD(:)   + &
          CT%XSEN_SNOW_DIF_ROOF(:)-CT%XSEN_MELT_ROAD(:)-CT%XSEN_MELT_ROOF(:) - &
          ZDIFFTHESNOWROAD(:)-ZDIFFTHESNOWROOF(:)+ZSRCSTOGAR(:) - &
          CT%XSEN_GREENROOF(:) - CT%XDIAG_DCS_AREA(:)
     !
     ZSRCLATSUM(:) = ZSRCLATFLX(:) + ZSRCLATINT(:)      + &
          ZSRCLTRAFF(:) + ZSRCLINDUS(:)+ZSRCLATRAI(:)+ZSRCLATSNO(:)  + &
          ZSRCLATROI(:)+ZSRCLATGRI(:)+ZSRCRUNROF(:) + ZSRCRUNROD(:)  + &
          ZSRCRUNSOILBLD(:) + ZSRCRUNSOILROD(:)                      + &
          ZSRCDRAINBLD(:) + ZSRCDRAINROD(:)                          + &
          ZSRCSEWER(:) + ZSRCSTORM(:)                                + &
          ZSRCNOCROF(:) + ZSRCNOCROD(:) - CT%XDIFF_SNOW_WAT_ROAD(:)  - &
          CT%XDIFF_SNOW_WAT_ROOF(:) - CT%XLAT_GREENROOF(:)           + & 
          CT%XLAT_SOIL_TO_GARDEN(:)
     !
     ! Total sum with conversion terms
     !
     ZSRCALLSUM(:) = ZSRCSENSUM(:)+ZSRCLATSUM(:)+CT%XCONV_LAT_SEN_ROAD(:)+CT%XCONV_LAT_SEN_ROOF(:)

     !
     !
     ! #######################################################
     ! Save results of current time step 
     ! #######################################################
     !
     GCHECK_BUDGET(:) = .TRUE.
     WHERE (NT%AL(JP)%XENETOTAL.EQ.XUNDEF) GCHECK_BUDGET = .FALSE.
     !
     NT%AL(JP)%XENETOTAL(:)   = ZENETOTAL(:)
     NT%AL(JP)%XTHEWALL(:)    = ZTHEWALL(:)
     NT%AL(JP)%XTHEROOF(:)    = ZTHEROOF(:)
     NT%AL(JP)%XTHEFLOOR(:)   = ZTHEFLOOR(:)
     NT%AL(JP)%XTHESOILBLD(:) = ZTHESOILBLD(:)
     NT%AL(JP)%XTHEMASS(:)    = ZTHEMASS(:)
     NT%AL(JP)%XTHEROAD(:)    = ZTHEROAD(:)
     NT%AL(JP)%XTHEAIRIN(:)   = ZTHEAIRIN(:)
     NT%AL(JP)%XTHETOTAL(:)   = ZTHETOTAL(:)
     NT%AL(JP)%XLATWATROOF(:) = ZLATWATROOF(:)
     NT%AL(JP)%XLATWATROAD(:) = ZLATWATROAD(:)
     NT%AL(JP)%XLATICEROOF(:) = ZLATICEROOF(:)
     NT%AL(JP)%XLATICEROAD(:) = ZLATICEROAD(:)
     NT%AL(JP)%XLATAIRIN(:)   = ZLATAIRIN(:)
     NT%AL(JP)%XLATSOILROAD(:)= ZLATSOILROAD(:)
     NT%AL(JP)%XLATSOILBLD(:) = ZLATSOILBLD(:)
     NT%AL(JP)%XLATTOTAL(:)   = ZLATTOTAL(:)
     !
     ! The model is halted when there is a violation of 
     ! energy conservation of more than 1.0E-6 W/m²(urb)
     !
     DO JJ=1,SIZE(ZDIFFENETOTAL)
        !
        IF (GCHECK_BUDGET(JJ)) THEN
           !
           IF ( ISNAN(ZDIFFENETOTAL(JJ)) .OR. ISNAN(ZSRCALLSUM(JJ)) .OR. &
                (ABS(ZDIFFENETOTAL(JJ)-ZSRCALLSUM(JJ)).GT.CT%XCHECK_ALL) ) THEN
              !
              CALL GET_LUOUT(HPROGRAM,ILUOUT)
              !
              WRITE(ILUOUT,*) "                                                                      "
              WRITE(ILUOUT,*) "----------------------------------------------------------------------"
              WRITE(ILUOUT,*) "----------------------------------------------------------------------"
              WRITE(ILUOUT,*) "NAN or violation of energy conservation, JJ= ",JJ
              WRITE(ILUOUT,*) "Solar time [s] :                             ",PTSUN(JJ)
              WRITE(ILUOUT,*) "----------------------------------------------------------------------"
              WRITE(ILUOUT,*) "                                                                      "
              WRITE(ILUOUT,*) "----------------------------------------------------------------------"
              WRITE(ILUOUT,*) "----------------------------------------------------------------------"
              WRITE(ILUOUT,*) "Difference of energy stored -------CHECK_TEB--------------------------"
              WRITE(ILUOUT,*) "----------------------------------------------------------------------"
              WRITE(ILUOUT,*) "----------------------------------------------------------------------"
              WRITE(ILUOUT,*) "----------------------------------------------------------------------"
              WRITE(ILUOUT,*) "- Considers energy stored as sensible heat and -----------------------"
              WRITE(ILUOUT,*) "- vapor/water/snow storage (as latent heat equivalent if released)----"
              WRITE(ILUOUT,*) "----------------------------------------------------------------------"
              WRITE(ILUOUT,*) "- Note that water/ice internal energy is not treated in the models----"
              WRITE(ILUOUT,*) "----------------------------------------------------------------------"
              WRITE(ILUOUT,*) "----------------------------------------------------------------------"
              WRITE(ILUOUT,'(A34,1X,F10.4)') "Floor sensible heat  (W/m²(urb)): ",ZDIFFTHEFLOOR(JJ)
              WRITE(ILUOUT,'(A34,1X,F10.4)') "bld soil sens. heat  (W/m²(urb)): ",ZDIFFTHESOILBLD(JJ)
              WRITE(ILUOUT,'(A34,1X,F10.4)') "Roof sensible heat   (W/m²(urb)): ",ZDIFFTHEROOF(JJ)
              WRITE(ILUOUT,'(A34,1X,F10.4)') "Wall sensible heat   (W/m²(urb)): ",ZDIFFTHEWALL(JJ)
              WRITE(ILUOUT,'(A34,1X,F10.4)') "Mass sensible heat   (W/m²(urb)): ",ZDIFFTHEMASS(JJ)
              WRITE(ILUOUT,'(A34,1X,F10.4)') "Road sensible heat   (W/m²(urb)): ",ZDIFFTHEROAD(JJ)
              WRITE(ILUOUT,'(A34,1X,F10.4)') "Road snow sen heat   (W/m²(urb)): ",ZDIFFTHESNOWROAD(JJ)
              WRITE(ILUOUT,'(A34,1X,F10.4)') "Roof snow sen heat   (W/m²(urb)): ",ZDIFFTHESNOWROOF(JJ)
              WRITE(ILUOUT,'(A34,1X,F10.4)') "In air sensible heat (W/m²(urb)): ",ZDIFFTHEAIRIN(JJ)
              WRITE(ILUOUT,'(A34,1X,F10.4)') "Roof wat lat heat eq (W/m²(urb)): ",ZDIFFLATWATROOF(JJ)
              WRITE(ILUOUT,'(A34,1X,F10.4)') "Roof ice lat heat eq (W/m²(urb)): ",ZDIFFLATICEROOF(JJ)
              WRITE(ILUOUT,'(A34,1X,F10.4)') "Road wat lat heat eq (W/m²(urb)): ",ZDIFFLATWATROAD(JJ)
              WRITE(ILUOUT,'(A34,1X,F10.4)') "Road ice lat heat eq (W/m²(urb)): ",ZDIFFLATICEROAD(JJ)
              WRITE(ILUOUT,'(A34,1X,F10.4)') "Road soil lat heat eq(W/m²(urb)): ",ZDIFFLATSOILROAD(JJ)
              WRITE(ILUOUT,'(A34,1X,F10.4)') "bld  soil lat heat eq(W/m²(urb)): ",ZDIFFLATSOILBLD(JJ)
              WRITE(ILUOUT,'(A34,1X,F10.4)') "Indo air lat heat eq (W/m²(urb)): ",ZDIFFLATAIRIN(JJ)
              WRITE(ILUOUT,*) "----------------------------------------------------------------------"
              WRITE(ILUOUT,'(A34,1X,F10.4)') "Total sensible heat  (W/m²(urb)): ",ZDIFFTHETOTAL(JJ)
              WRITE(ILUOUT,'(A34,1X,F10.4)') "Total    latent heat (W/m²(urb)): ",ZDIFFLATTOTAL(JJ)
              WRITE(ILUOUT,'(A34,1X,F10.4)') "Total           heat (W/m²(urb)): ",ZDIFFENETOTAL(JJ)
              WRITE(ILUOUT,*) "----------------------------------------------------------------------"
              WRITE(ILUOUT,*) "----------------------------------------------------------------------"
              WRITE(ILUOUT,*) "Sources and sinks ----------------------------------------------------"
              WRITE(ILUOUT,*) "----------------------------------------------------------------------"
              WRITE(ILUOUT,*) "----------------------------------------------------------------------"
              WRITE(ILUOUT,*) "- Considers sensible heat fluxes, water vapor latent heat fluxes------"
              WRITE(ILUOUT,*) "- and liquid water/snow fluxes (as latent heat equivalent)------------"
              WRITE(ILUOUT,*) "----------------------------------------------------------------------"
              WRITE(ILUOUT,'(A35,1X,F10.4)') "Sensible heat flux   (W/m²(Urb)): ",ZSRCSENFLX(JJ)
              WRITE(ILUOUT,'(A35,1X,F10.4)') "Latent   heat flux   (W/m²(Urb)): ",ZSRCLATFLX(JJ)
              WRITE(ILUOUT,'(A35,1X,F10.4)') "Net radiation        (W/m²(Urb)): ",ZSRCNETRAD(JJ)
              WRITE(ILUOUT,'(A35,1X,F10.4)') "Solar panel prod.    (W/m²(Urb)): ",ZSRCPROPAN(JJ)
              WRITE(ILUOUT,'(A35,1X,F10.4)') "Internal heat, sen   (W/m²(Urb)): ",ZSRCSENINT(JJ)
              WRITE(ILUOUT,'(A35,1X,F10.4)') "Internal heat, lat   (W/m²(Urb)): ",ZSRCLATINT(JJ)
              WRITE(ILUOUT,'(A35,1X,F10.4)') "Cooling demand       (W/m²(Urb)): ",ZSRCHVACCL(JJ)
              WRITE(ILUOUT,'(A35,1X,F10.4)') "Heating demand       (W/m²(Urb)): ",ZSRCHVACHT(JJ)
              WRITE(ILUOUT,'(A35,1X,F10.4)') "Flux to ground floor (W/m²(Urb)): ",ZSRCFLXFLO(JJ)
              WRITE(ILUOUT,'(A35,1X,F10.4)') "Flux roof to indoor  (W/m²(Urb)): ",ZSRCFLXROF(JJ)
              WRITE(ILUOUT,'(A35,1X,F10.4)') "Flux wall to indoor  (W/m²(Urb)): ",ZSRCFLXWAL(JJ)
              WRITE(ILUOUT,'(A35,1X,F10.4)') "Traffic sensible     (W/m²(Urb)): ",ZSRCHTRAFF(JJ)
              WRITE(ILUOUT,'(A35,1X,F10.4)') "Traffic latent       (W/m²(Urb)): ",ZSRCLTRAFF(JJ)
              WRITE(ILUOUT,'(A35,1X,F10.4)') "Industry sensible    (W/m²(Urb)): ",ZSRCHINDUS(JJ)
              WRITE(ILUOUT,'(A35,1X,F10.4)') "Industry latent      (W/m²(Urb)): ",ZSRCLINDUS(JJ)
              WRITE(ILUOUT,'(A35,1X,F10.4)') "Rainfall lat heat eq (W/m²(Urb)): ",ZSRCLATRAI(JJ)
              WRITE(ILUOUT,'(A35,1X,F10.4)') "Snowfall lat heat eq (W/m²(Urb)): ",ZSRCLATSNO(JJ)
              WRITE(ILUOUT,'(A35,1X,F10.4)') "Road irrig.   lat eq (W/m²(Urb)): ",ZSRCLATROI(JJ)
              WRITE(ILUOUT,'(A35,1X,F10.4)') "Green roof ir lat eq (W/m²(Urb)): ",ZSRCLATGRI(JJ)
              WRITE(ILUOUT,'(A35,1X,F10.4)') "Rain h/c road        (W/m²(Urb)): ",CT%XHEAT_RR_ROAD(JJ)
              WRITE(ILUOUT,'(A35,1X,F10.4)') "Rain h/c roof        (W/m²(Urb)): ",CT%XHEAT_RR_ROOF(JJ)
              WRITE(ILUOUT,'(A35,1X,F10.4)') "Snow h/c road        (W/m²(Urb)): ",CT%XSEN_SNOW_DIF_ROAD(JJ)
              WRITE(ILUOUT,'(A35,1X,F10.4)') "Snow h/c roof        (W/m²(Urb)): ",CT%XSEN_SNOW_DIF_ROOF(JJ)
              WRITE(ILUOUT,'(A35,1X,F10.4)') "Garden sensible heat (W/m²(urb)): ",ZSRCSTOGAR(JJ)
              WRITE(ILUOUT,'(A35,1X,F10.4)') "Green roof sen. heat (W/m²(urb)): ",CT%XSEN_GREENROOF(JJ)
              WRITE(ILUOUT,'(A35,1X,F10.4)') "Green roof lat. heat (W/m²(urb)): ",CT%XLAT_GREENROOF(JJ)
              WRITE(ILUOUT,'(A35,1X,F10.4)') "Roof runoff   lat eq (W/m²(Urb)): ",ZSRCRUNROF(JJ)
              WRITE(ILUOUT,'(A35,1X,F10.4)') "Road runoff   lat eq (W/m²(Urb)): ",ZSRCRUNROD(JJ)
              WRITE(ILUOUT,'(A35,1X,F10.4)') "Roof no sewer lat eq (W/m²(Urb)): ",ZSRCNOCROF(JJ)
              WRITE(ILUOUT,'(A35,1X,F10.4)') "Road no sewer lat eq (W/m²(Urb)): ",ZSRCNOCROD(JJ)
              WRITE(ILUOUT,'(A35,1X,F10.4)') "bl Soil runoff lat eq(W/m²(Urb)): ",ZSRCRUNSOILBLD(JJ)
              WRITE(ILUOUT,'(A35,1X,F10.4)') "rd Soil runoff lat eq(W/m²(Urb)): ",ZSRCRUNSOILROD(JJ)
              WRITE(ILUOUT,'(A35,1X,F10.4)') "bl Soil drain. lat eq(W/m²(Urb)): ",ZSRCDRAINBLD(JJ)
              WRITE(ILUOUT,'(A35,1X,F10.4)') "rd Soil drain. lat eq(W/m²(Urb)): ",ZSRCDRAINROD(JJ)
              WRITE(ILUOUT,'(A35,1X,F10.4)') "Hor soil to gd lat eq(W/m²(Urb)): ",CT%XLAT_SOIL_TO_GARDEN(JJ)
              WRITE(ILUOUT,'(A35,1X,F10.4)') "Sewer(w) inf. lat eq (W/m²(Urb)): ",ZSRCSEWER(JJ)
              WRITE(ILUOUT,'(A35,1X,F10.4)') "Sewer(s) inf. lat eq (W/m²(Urb)): ",ZSRCSTORM(JJ)
              WRITE(ILUOUT,'(A35,1X,F10.4)') "Sen heat melt road   (W/m²(Urb)): ",CT%XSEN_MELT_ROAD(JJ)
              WRITE(ILUOUT,'(A35,1X,F10.4)') "Sen heat melt roof   (W/m²(Urb)): ",CT%XSEN_MELT_ROOF(JJ)
              WRITE(ILUOUT,'(A35,1X,F10.4)') "Snow water conv road (W/m²(Urb)): ",CT%XDIFF_SNOW_WAT_ROAD(JJ)
              WRITE(ILUOUT,'(A35,1X,F10.4)') "Snow water conv roof (W/m²(Urb)): ",CT%XDIFF_SNOW_WAT_ROOF(JJ)
              WRITE(ILUOUT,'(A35,1X,F10.4)') "District cooling     (W/m²(Urb)): ",CT%XDIAG_DCS_AREA(JJ)
              WRITE(ILUOUT,*) "----------------------------------------------------------------------"
              WRITE(ILUOUT,'(A35,1X,F10.4)') "Sensible sum         (W/m²(Urb)): ",ZSRCSENSUM(JJ)
              WRITE(ILUOUT,'(A35,1X,F10.4)') "Latent sum           (W/m²(Urb)): ",ZSRCLATSUM(JJ)
              WRITE(ILUOUT,*) "----------------------------------------------------------------------"
              WRITE(ILUOUT,*) " Conversion between latent and sensible heat                          "
              WRITE(ILUOUT,*) "----------------------------------------------------------------------"
              WRITE(ILUOUT,'(A35,1X,F10.4)') "Roof                 (W/m²(Urb)): ",CT%XCONV_LAT_SEN_ROOF(JJ)
              WRITE(ILUOUT,'(A35,1X,F10.4)') "Road                 (W/m²(Urb)): ",CT%XCONV_LAT_SEN_ROAD(JJ)
              WRITE(ILUOUT,*) "----------------------------------------------------------------------"
              WRITE(ILUOUT,'(A35,1X,F10.4)') "Total sum            (W/m²(Urb)): ",ZSRCALLSUM(JJ)
              WRITE(ILUOUT,*) "----------------------------------------------------------------------"
              WRITE(ILUOUT,*) " Storage difference - sources and sinks (must be zero)                "
              WRITE(ILUOUT,*) "----------------------------------------------------------------------"
              WRITE(ILUOUT,'(A35,1X,F10.4)') "Energy budget imb.   (W/m²(Urb)): ",ZDIFFENETOTAL(JJ)-ZSRCALLSUM(JJ)
              WRITE(ILUOUT,*) "----------------------------------------------------------------------"
              WRITE(ILUOUT,*) "----------------------------------------------------------------------"
              WRITE(ILUOUT,*) "Diagnostics of outdoor longwave radiation imbalance not               "
              WRITE(ILUOUT,*) "detectable by the energy budget based on net radiation                "
              WRITE(ILUOUT,*) "----------------------------------------------------------------------"
              IF ( CT%XLW_GARD_TO_WALA(JJ)/=XUNDEF) &
              WRITE(ILUOUT,'(A35,1X,F10.4)') " Garden <--> Wall A  (W/m²(Urb)): ",CT%XLW_WALA_TO_GARD(JJ)+CT%XLW_GARD_TO_WALA(JJ)
              IF ( CT%XLW_GARD_TO_WALB(JJ)/=XUNDEF) &
              WRITE(ILUOUT,'(A35,1X,F10.4)') " Garden <--> Wall B  (W/m²(Urb)): ",CT%XLW_WALB_TO_GARD(JJ)+CT%XLW_GARD_TO_WALB(JJ)
              IF ( CT%XLW_ROAD_TO_WALA(JJ)/=XUNDEF) &
              WRITE(ILUOUT,'(A35,1X,F10.4)') " Road   <--> Wall A  (W/m²(Urb)): ",CT%XLW_WALA_TO_ROAD(JJ)+CT%XLW_ROAD_TO_WALA(JJ)
              IF ( CT%XLW_ROAD_TO_WALB(JJ)/=XUNDEF) &
              WRITE(ILUOUT,'(A35,1X,F10.4)') " Road   <--> Wall B  (W/m²(Urb)): ",CT%XLW_WALB_TO_ROAD(JJ)+CT%XLW_ROAD_TO_WALB(JJ)
              IF ( CT%XLW_SNOW_TO_WALA(JJ)/=XUNDEF .AND. CT%XLW_WALA_TO_SNOW(JJ)/=XUNDEF) &
              WRITE(ILUOUT,'(A35,1X,F10.4)') " Snow   <--> Wall A  (W/m²(Urb)): ",CT%XLW_WALA_TO_SNOW(JJ)+CT%XLW_SNOW_TO_WALA(JJ)
              IF ( CT%XLW_SNOW_TO_WALB(JJ)/=XUNDEF .AND. CT%XLW_WALB_TO_SNOW(JJ)/=XUNDEF) &
              WRITE(ILUOUT,'(A35,1X,F10.4)') " Snow   <--> Wall B  (W/m²(Urb)): ",CT%XLW_WALB_TO_SNOW(JJ)+CT%XLW_SNOW_TO_WALB(JJ)
              IF ( CT%XLW_WIND_TO_WALA(JJ)/=XUNDEF) &
              WRITE(ILUOUT,'(A35,1X,F10.4)') " Window <--> Walls   (W/m²(Urb)): ", & 
                   CT%XLW_WALL_TO_WIND(JJ)+(CT%XLW_WIND_TO_WALA(JJ)+CT%XLW_WIND_TO_WALB(JJ))
              IF ( CT%XLW_WIND_TO_ROAD(JJ)/=XUNDEF) &
              WRITE(ILUOUT,'(A35,1X,F10.4)') " Window <--> Road    (W/m²(Urb)): ",CT%XLW_ROAD_TO_WIND(JJ)+CT%XLW_WIND_TO_ROAD(JJ)
              IF ( CT%XLW_WIND_TO_GARD(JJ)/=XUNDEF) &
              WRITE(ILUOUT,'(A35,1X,F10.4)') " Window <--> Garden  (W/m²(Urb)): ",CT%XLW_GARD_TO_WIND(JJ)+CT%XLW_WIND_TO_GARD(JJ)
              IF ( CT%XLW_WIND_TO_SNOW(JJ)/=XUNDEF) &
              WRITE(ILUOUT,'(A35,1X,F10.4)') " Window <--> Snow    (W/m²(Urb)): ",CT%XLW_SNOW_TO_WIND(JJ)+CT%XLW_WIND_TO_SNOW(JJ)
              IF ( CT%XLW_WALA_TO_WALB(JJ)/=XUNDEF) &
              WRITE(ILUOUT,'(A35,1X,F10.4)') " Wall A <--> Wall B  (W/m²(Urb)): ",CT%XLW_WALB_TO_WALA(JJ)+CT%XLW_WALA_TO_WALB(JJ)
              IF ( CT%XLW_HV_TO_WALA(JJ)/=XUNDEF) &
              WRITE(ILUOUT,'(A35,1X,F10.4)') " Trees  <--> Wall A  (W/m²(Urb)): ",CT%XLW_HV_TO_WALA(JJ)+CT%XLW_WALA_TO_HV(JJ)
              IF ( CT%XLW_HV_TO_WALB(JJ)/=XUNDEF) &
              WRITE(ILUOUT,'(A35,1X,F10.4)') " Trees  <--> Wall B  (W/m²(Urb)): ",CT%XLW_HV_TO_WALB(JJ)+CT%XLW_WALB_TO_HV(JJ)
              IF ( CT%XLW_HV_TO_WIND(JJ)/=XUNDEF) &
              WRITE(ILUOUT,'(A35,1X,F10.4)') " Trees  <--> Window  (W/m²(Urb)): ",CT%XLW_HV_TO_WIND(JJ)+CT%XLW_WIND_TO_HV(JJ)
              IF ( CT%XLW_HV_TO_ROAD(JJ)/=XUNDEF) &
              WRITE(ILUOUT,'(A35,1X,F10.4)') " Trees  <--> Road    (W/m²(Urb)): ",CT%XLW_HV_TO_ROAD(JJ)+CT%XLW_ROAD_TO_HV(JJ)
              IF ( CT%XLW_HV_TO_SNOW(JJ)/=XUNDEF) &
              WRITE(ILUOUT,'(A35,1X,F10.4)') " Trees  <--> Snow    (W/m²(Urb)): ",CT%XLW_HV_TO_SNOW(JJ)+CT%XLW_SNOW_TO_HV(JJ)
              IF ( CT%XLW_HV_TO_GARD(JJ)/=XUNDEF) &
              WRITE(ILUOUT,'(A35,1X,F10.4)') " Trees  <--> Garden  (W/m²(Urb)): ",CT%XLW_HV_TO_GARD(JJ)+CT%XLW_GARD_TO_HV(JJ)
              WRITE(ILUOUT,*) "----------------------------------------------------------------------"
              WRITE(ILUOUT,*) "----------------------------------------------------------------------"
              WRITE(ILUOUT,*) "Model halt due to violation of energy conservation "
              WRITE(ILUOUT,*) "This can be due to coding errors or missing        "
              WRITE(ILUOUT,*) "reservoirs and processes, please check             "
              WRITE(ILUOUT,*) "----------------------------------------------------------------------"
              WRITE(ILUOUT,*) "----------------------------------------------------------------------"
              CALL FLUSH(ILUOUT)
              !

              CALL ABOR1_SFX("NAN or violation of energy conservation, check report")
              !
           ENDIF
        ENDIF
     ENDDO
     !
     !
     ! Calculation of new output diagnostics
     ! -------------------------------------
     !
     ! Sensible and latent heat storage in urban fabric (W/m²(urb))
     !
     TD%NDMT%AL(JP)%XSENFABSTOR(:)=ZDIFFTHEFLOOR(:)+ZDIFFTHESOILBLD(:)+ZDIFFTHEROOF(:)+ZDIFFTHEWALL(:) + &
          ZDIFFTHEMASS(:) +ZDIFFTHEROAD(:)+ZDIFFTHEAIRIN(:)
     !
     TD%NDMT%AL(JP)%XLATFABSTOR(:)=ZDIFFLATWATROOF(:)+ZDIFFLATICEROOF(:)+ZDIFFLATWATROAD(:) + &
          ZDIFFLATICEROAD(:)+ZDIFFLATAIRIN(:) + ZDIFFLATSOILROAD(:) + ZDIFFLATSOILBLD(:)
     !
     ! Total thickness of roof, wall and mass
     !
     TD%NDMT%AL(JP)%XROOFTK(:)=0.0
     DO JLAYER=1,SIZE(NT%AL(JP)%XT_ROOF,2)
        TD%NDMT%AL(JP)%XROOFTK(:)=TD%NDMT%AL(JP)%XROOFTK(:)+NT%AL(JP)%XD_ROOF(:,JLAYER)
     ENDDO
     !
     TD%NDMT%AL(JP)%XWALLTK(:)=0.0
     DO JLAYER=1,SIZE(NT%AL(JP)%XT_WALL_A,2)
        TD%NDMT%AL(JP)%XWALLTK(:)=TD%NDMT%AL(JP)%XWALLTK(:)+NT%AL(JP)%XD_WALL(:,JLAYER)
     ENDDO
     !
     IF (TOP%CBEM.EQ."BEM") THEN
        TD%NDMT%AL(JP)%XMASSTK(:)=0.0
        DO JJ=1,SIZE(NB%AL(JP)%XN_FLOOR)
           IF (NB%AL(JP)%XN_FLOOR(JJ).GT.1.5) THEN
              DO JLAYER=1,SIZE(NB%AL(JP)%XD_MASS,2)
                 TD%NDMT%AL(JP)%XMASSTK(JJ)=TD%NDMT%AL(JP)%XMASSTK(JJ)+NB%AL(JP)%XD_MASS(JJ,JLAYER)
              ENDDO
           ENDIF
        ENDDO
     END IF
     !
  ! Conversion of internal heat release as well 
  ! as heating and cooling from W/m²(urb) into kWh/m²(floor)/a
  !
  IF (TOP%CBEM.EQ."BEM") THEN
     TD%NDMT%AL(JP)%XQIN_KWH     (:) = ( 0.365 * 24.0 * TD%NDMT%AL(JP)%XQINOUT(:)   ) / &
          ( NT%AL(JP)%XBLD(:) * NB%AL(JP)%XN_FLOOR(:) )
     TD%NDMT%AL(JP)%XHVAC_HT_KWH (:) = ( 0.365 * 24.0 * TD%NDMT%AL(JP)%XHVAC_HEAT(:)) / &
          ( NT%AL(JP)%XBLD(:) * NB%AL(JP)%XN_FLOOR(:) )
     TD%NDMT%AL(JP)%XHVAC_CL_KWH (:) = ( 0.365 * 24.0 * TD%NDMT%AL(JP)%XHVAC_COOL(:)) / &
          ( NT%AL(JP)%XBLD(:) * NB%AL(JP)%XN_FLOOR(:) )
  ENDIF
  !
  ! End verification of energy conservation
  ! ################################################################ 

  IF (LHOOK) CALL DR_HOOK('CHECK_TEB',1,ZHOOK_HANDLE)
  !
  !
END SUBROUTINE CHECK_TEB


