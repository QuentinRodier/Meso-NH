!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE HYDRO_SGH  (HISBA,HRUNOFF,HRAIN,HHORT,PTSTEP,           &
                               PD_G,PDZG,PWSAT,PWFC,PWWILT,PWG,PWGI,     &
                               KWG_LAYER,PPG,PPG_MELT,PMUF,              &
                               PCONDSAT,PBCOEF,PMPOTSAT,                 &
                               PKSAT_ICE,PD_ICE,PFSAT,PHORTON,PDUNNE,    &
                               PFFLOOD,PPIFLOOD,PIFLOOD,PPFLOOD,         &
                               PRUNOFFB,PRUNOFFD,PCG,PSOILWGHT,          &
                               OFLOOD,KLAYER_HORT,KLAYER_DUN             )  
!
!     #####################################################################
!
!!****  *HYDRO_SGH*  
!!
!!    PURPOSE
!!    =======
!
!     1. Determine the Horton runoff that take account of a spatial subgri
!        exponential distribution of the precipitation and of the surface ksat.
!     1. Determine the surface saturated fraction (dt92 or Topmodel).
!     3. Determine the Dunne runoff (dt92 or Topmodel).
!     4. Determine the infiltration rate.
!     5. Determine the flooplains interception and infiltration rate.
!
!!    MODIFICATIONS
!!    -------------
!!
!!         03/16    (B. Decharme) Limit flood infiltration
!!
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!        ===================
!
!
USE MODD_CSTS,      ONLY : XRHOLW, XDAY, XCL, XCI, XRHOLI
USE MODD_ISBA_PAR,  ONLY : XWGMIN, XSPHSOIL, XDRYWGHT
USE MODD_SURF_PAR,  ONLY : XUNDEF
USE MODD_SGH_PAR,   ONLY : XHORT_DEPTH
!
#ifdef TOPD
USE MODD_COUPLING_TOPD, ONLY : LCOUPL_TOPD, XAS_NATURE, XATOP, NMASKT_PATCH
#endif
!
USE MODI_HYDRO_DT92
!
USE MODE_HYDRO_DIF
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
 CHARACTER(LEN=*),INTENT(IN)      :: HISBA   ! hydrology/soil:
!                                           ! '2-L'  = single column
!                                           ! '3-L'  = root zone/baseflow layer
!                                           ! 'DIF'  = N-layer diffusion: Richard's Eq.
!                                           !         (Boone and Etchevers 2001)
!
 CHARACTER(LEN=*),     INTENT(IN) :: HRUNOFF ! surface runoff formulation
!                                           ! 'WSAT'
!                                           ! 'DT92'
!                                           ! 'SGH ' Topmodel
!
!
 CHARACTER(LEN=*), INTENT(IN)     :: HRAIN   ! Rainfall spatial distribution
                                            ! 'DEF' = No rainfall spatial distribution
                                            ! 'SGH' = Rainfall exponential spatial distribution
                                            ! 
!
 CHARACTER(LEN=*), INTENT(IN)     :: HHORT   ! Horton runoff
                                            ! 'DEF' = no Horton runoff
                                            ! 'SGH' = Horton runoff
!
LOGICAL, INTENT(IN)              :: OFLOOD ! Flood scheme 
!
REAL, INTENT(IN)                 :: PTSTEP
!                                   timestep of the integration
!
REAL, DIMENSION(:,:), INTENT(IN) :: PWG,PWGI
!                                   PWG   = layer average liquid volumetric water content (m3 m-3)
!                                   PWGI  = layer average frozen volumetric water content (m3 m-3)
!
INTEGER, DIMENSION(:),INTENT(IN) :: KWG_LAYER  
!                                   KWG_LAYER = Number of soil moisture layers (DIF option)
!
REAL, DIMENSION(:,:), INTENT(IN) :: PD_G,PDZG,PWSAT,PWFC,PWWILT
REAL, DIMENSION(:,:), INTENT(IN) :: PCONDSAT
!                                   PD_G  = layer depth (m)
!                                   PDZG= layer thickness (m)
!                                   PCONDSAT = surface saturated hydraulic conductivity
!                                   PWSAT = soil porosity (m3 m-3)
!
REAL, DIMENSION(:,:), INTENT(IN) :: PBCOEF,PMPOTSAT
!                                   PMPOTSAT = matric potential at saturation (m) (BC parameters)
!                                   PBCOEF   = slope of the retention curve (-) (BC parameters)
!
REAL, DIMENSION(:,:),INTENT(IN) :: PSOILWGHT  ! ISBA-DIF: weights for vertical
!                                             ! integration of soil water and properties
INTEGER,             INTENT(IN) :: KLAYER_HORT! DIF optimization
INTEGER,             INTENT(IN) :: KLAYER_DUN ! DIF optimization
!
REAL, DIMENSION(:), INTENT(INOUT):: PFSAT
!                                   PFSAT = satured fraction
!
REAL, DIMENSION(:), INTENT(INOUT):: PPG
REAL, DIMENSION(:), INTENT(IN)   :: PPG_MELT, PMUF
!                                   PPG      = water reaching the ground
!                                   PPG_MELT = snowmelt reaching the ground
!                                   PMUF      = wet fraction reached by rain
!
REAL, DIMENSION(:), INTENT(IN)   :: PKSAT_ICE, PD_ICE
!                                   PKSAT_ICE = hydraulic conductivity at saturation (m s-1)
!                                               on frozen soil depth (Horton calculation)
!                                   PD_ICE    = depth of the soil column for
!                                               fraction of frozen soil calculation (m)
!
REAL, DIMENSION(:), INTENT(OUT)  :: PDUNNE, PHORTON
!                                   PDUNNE  = Dunne runoff
!                                   PHORTON = Horton runoff
!
REAL, DIMENSION(:), INTENT(IN   ) :: PFFLOOD
REAL, DIMENSION(:), INTENT(IN   ) :: PPIFLOOD
!                                    PPIFLOOD = Floodplain potential infiltration [kg/m²/s]
!                                             = Floodplain mass
REAL, DIMENSION(:), INTENT(INOUT) :: PIFLOOD, PPFLOOD
!                                    PIFLOOD = Floodplain infiltration     [kg/m²/s]
!                                    PPFLOOD = Floodplain interception     [kg/m²/s]
!
REAL, DIMENSION(:), INTENT(IN)    :: PRUNOFFB ! slope of the runoff curve
REAL, DIMENSION(:), INTENT(IN)    :: PRUNOFFD
!                                    PRUNOFFD = depth over which sub-grid runoff calculated (m)
!
REAL, DIMENSION(:), INTENT(IN)    :: PCG
!                                   PCG = soil heat capacity to compute thermal penetration depth
!
!*      0.2    declarations of local variables
!
REAL, PARAMETER                            :: ZEICE = 6.0  ! Ice vertical diffusion impedence factor 
!
REAL, DIMENSION(SIZE(PPG))                 :: ZPG_INI, ZFROZEN, ZIMAX_ICE, ZIMAX, &
                                              ZHORT_R, ZHORT_M, ZSOILMAX, ZIF_MAX,&
                                              ZPIFLDMAX
!                                             ZFROZEN  = frozen soil fraction for runoff
!                                             ZIMAX_ICE    = maximum infiltration rate for frozen soil
!                                             ZIMAX     = maximum infiltration rate for unfrozen soil
!                                             ZPIFLDMAX    = maximum floodplains infiltration during 1 day (kg/m2/s)
!
REAL, DIMENSION(SIZE(PPG))                 :: ZWG2_AVG, ZWGI2_AVG, ZWSAT_AVG, ZWWILT_AVG
!                                             Average water and ice content
!                                             values over the soil depth D2 (for calculating surface runoff)
!
REAL, DIMENSION(SIZE(PD_G,1),SIZE(PD_G,2)) :: ZWSAT, ZWFC, ZFRZ
!
REAL, DIMENSION(SIZE(PPG))                 :: ZPG_WORK, ZRUISDT, ZNL_HORT, ZDEPTH
!
REAL, DIMENSION(SIZE(PPG))                 :: ZRUNOFF_TOPD
!
REAL                                       :: ZEFFICE, ZLOG10, ZLOG, ZS, ZD_H
!
REAL                                       :: ZTDIURN, ZSOILHEATCAP
!                                             ZTDIURN      = thermal penetration depth for restore (m)
!                                             ZSOILHEATCAP = Total soil volumetric heat capacity [J/(m3 K)]
!
INTEGER                                    :: INI, INL, JJ, JL, IDEPTH
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!allocate
!
IF (LHOOK) CALL DR_HOOK('HYDRO_SGH',0,ZHOOK_HANDLE)
!
!initialize
!
ZFROZEN  (:)  = 0.0
ZIMAX_ICE(:)  = 0.0
ZIMAX    (:)  = 0.0
!
ZWSAT  (:,:)  = 0.0
ZWFC   (:,:)  = 0.0
!
ZLOG10 = LOG(10.0)
!
!HRUNOFF = DT92 ZFSAT calculation
ZWG2_AVG(:)   = 0.0
ZWGI2_AVG(:)  = 0.0
ZWSAT_AVG(:)  = 0.0
ZWWILT_AVG(:) = 0.0
!
!HHORT=SGH
ZHORT_R(:) = 0.0
ZHORT_M(:) = 0.0
!
!PIFLOOD calculation
ZSOILMAX (:) = 0.0
ZIF_MAX  (:) = 0.0
ZPIFLDMAX(:) = 0.0
!
!HRUNOFF = DT92 DUNNE calculation
ZPG_WORK(:)   = 0.0
ZRUISDT(:)    = 0.0
!
!HRUNOFF = TOPD DUNNE calculation
ZRUNOFF_TOPD(:) = 0.0
!
!to limit numerical artifacts
ZPG_INI(:) = PPG(:) + PPG_MELT(:)
!
!
INI=SIZE(PD_G,1)
INL=MAXVAL(KWG_LAYER(:))
!
!-------------------------------------------------------------------------------
!
!*           1. Surface saturated fraction
!            -----------------------------
!
IF( HRUNOFF=='DT92' .OR. HRUNOFF == 'TOPD' )THEN
!
! Calculate the layer average water content for the sub-grid
! surface runoff computation: use PRUNOFFD as the depth over which
! runoff is calculated.
!
! First, determine a weight for each layer's contribution
! to thickness averaged water content and soil properties for runoff.
!
   IF (HISBA == 'DIF') THEN
!
! Vertically averaged soil properties and moisture for surface runoff computation:
!
      DO JL=1,KLAYER_DUN
         DO JJ=1,INI
            IDEPTH=KWG_LAYER(JJ)
            IF(JL<=IDEPTH)THEN
              ZWG2_AVG  (JJ) = ZWG2_AVG  (JJ) + PSOILWGHT(JJ,JL)*PWG   (JJ,JL)/MAX(1.E-6,PRUNOFFD(JJ))
              ZWGI2_AVG (JJ) = ZWGI2_AVG (JJ) + PSOILWGHT(JJ,JL)*PWGI  (JJ,JL)/MAX(1.E-6,PRUNOFFD(JJ))
              ZWSAT_AVG (JJ) = ZWSAT_AVG (JJ) + PSOILWGHT(JJ,JL)*PWSAT (JJ,JL)/MAX(1.E-6,PRUNOFFD(JJ))
              ZWWILT_AVG(JJ) = ZWWILT_AVG(JJ) + PSOILWGHT(JJ,JL)*PWWILT(JJ,JL)/MAX(1.E-6,PRUNOFFD(JJ))
            ENDIF
         ENDDO
      ENDDO
!
   ELSE
!           
      ZWG2_AVG(:)   = PWG(:, 2)
      ZWGI2_AVG(:)  = PWGI(:,2)
      ZWSAT_AVG(:)  = PWSAT(:,1)
      ZWWILT_AVG(:) = PWWILT(:,1)
!      
   ENDIF
!
   IF(HHORT=='SGH')THEN
     !runoff over frozen soil explicitly calculated
     ZWGI2_AVG(:)=0.0
   ENDIF
!
   DO JJ=1,INI
      ZS=MIN(1.0,(ZWG2_AVG(JJ)+ZWGI2_AVG(JJ)-ZWWILT_AVG(JJ))/(ZWSAT_AVG(JJ)-ZWWILT_AVG(JJ)))
      PFSAT(JJ) = 1.0-(1.0-MAX(0.0,ZS))**(PRUNOFFB(JJ)/(PRUNOFFB(JJ)+1.))
   ENDDO        
!
ENDIF
!
!*           2. Horton runoff
!            ----------------
!
IF(HHORT=='SGH'.OR.OFLOOD)THEN  
!
  IF(HISBA == 'DIF')THEN
!
!   no subgrid frozen soil fraction of the grid cells
    ZFROZEN(:) = 0.0
!    
    DO JL=1,KLAYER_HORT
      DO JJ=1,INI   
!              
!       Modify soil porosity as ice assumed to become part
!       of solid soil matrix (with respect to liquid flow):                
        ZWSAT(JJ,JL) = MAX(XWGMIN, PWSAT(JJ,JL)-PWGI(JJ,JL)) 
        ZWFC (JJ,JL) = PWFC(JJ,JL)*ZWSAT(JJ,JL)/PWSAT(JJ,JL)
!        
!       Impedance Factor from (Johnsson and Lundin 1991).
        ZFRZ(JJ,JL) = EXP(ZLOG10*(-ZEICE*(PWGI(JJ,JL)/(PWGI(JJ,JL)+PWG(JJ,JL)))))
!
      ENDDO
    ENDDO    
!
!   Calculate infiltration MAX using green-ampt approximation (derived form)
    ZIMAX(:) = INFMAX_FUNC(PWG,ZWSAT,ZFRZ,PCONDSAT,PMPOTSAT,PBCOEF,PDZG,PD_G,KLAYER_HORT)
!  
  ELSE
!
    DO JJ=1,INI
!
!    Total soil volumetric heat capacity [J/(m3 K)]:
!
      ZSOILHEATCAP = XCL*XRHOLW*PWG (JJ,2) +                           &
                     XCI*XRHOLI*PWGI(JJ,2) +                           &
                     XSPHSOIL*XDRYWGHT*(1.0-PWSAT(JJ,1))
!                     
!     Soil thickness which corresponds to the diurnal surface temperature
!     wave penetration depth as T2 is the average temperature for this layer:
!
      ZTDIURN   = MIN(PD_G(JJ,2), 4./(ZSOILHEATCAP*PCG(JJ)))
!    
!     Effective frozen depth penetration 
!
      ZEFFICE=PD_G(JJ,2)*PWGI(JJ,2)/(PWGI(JJ,2)+PWG(JJ,2))
!
!     Modify soil porosity as ice assumed to become part
!     of solid soil matrix (with respect to liquid flow):
!
      ZWSAT(JJ,1) = MAX(XWGMIN, PWSAT(JJ,1)-PWGI(JJ,2)) 
!
!     calculate the subgrid frozen soil fraction of the grid cells
!
      ZFROZEN (JJ) = MIN(1.,ZEFFICE/MAX(PD_ICE(JJ),ZTDIURN))
!
!     Impedance Factor from (Johnsson and Lundin 1991).
!
      ZFRZ(JJ,1) = EXP(ZLOG10*(-ZEICE*MIN(1.,ZEFFICE/ZTDIURN)))
!
!     Calculate infiltration MAX on frozen soil as Johnsson and Lundin (1991).
!     The max infiltration is equal to the unsaturated conductivity function at a
!     water content corresponding to the total porosity less the ice-filled volume.
!
      ZS =MIN(1.,ZWSAT(JJ,1)/PWSAT(JJ,1))
      ZIMAX_ICE(JJ)=ZFRZ(JJ,1)*PKSAT_ICE(JJ)*(ZS**(2*PBCOEF(JJ,1)+3.))
!
!     Calculate infiltration MAX on unfrozen soil using green-ampt approximation
!    
      ZS   =MIN(1.,PWG(JJ,2)/ZWSAT(JJ,1))
      ZD_H =MIN(0.10,PD_G(JJ,2))
      ZIMAX(JJ)=PCONDSAT(JJ,1)*(PBCOEF(JJ,1)*PMPOTSAT(JJ,1)*(ZS-1.0)/ZD_H+1.0)
!
    ENDDO
!
  ENDIF
!
ENDIF
!
IF(HHORT=='SGH')THEN
!
! calculate the Horton runoff generated by the rainfall rate
!
  IF(HRAIN=='SGH')THEN
!
    WHERE(PPG(:)>0.)
       ZHORT_R(:) = (1.- ZFROZEN(:))* PPG(:)/((ZIMAX    (:)*XRHOLW*PMUF(:)/PPG(:)) + 1.) & !unfrozen soil
                  +      ZFROZEN(:) * PPG(:)/((ZIMAX_ICE(:)*XRHOLW*PMUF(:)/PPG(:)) + 1.)   !frozen soil
    END WHERE        
!
  ELSE
!
    ZHORT_R(:) = (1.- ZFROZEN(:))* MAX(0.,PPG(:)-ZIMAX    (:)*XRHOLW)          & !unfrozen soil
               +      ZFROZEN(:) * MAX(0.,PPG(:)-ZIMAX_ICE(:)*XRHOLW)            !frozen soil
!
  ENDIF
!
! calculate the Horton runoff generated by the snow melt
!        
  ZHORT_M(:) = (1.- ZFROZEN(:))* MAX(0.,PPG_MELT(:)-ZIMAX    (:)*XRHOLW)          & !unfrozen soil
             +      ZFROZEN(:) * MAX(0.,PPG_MELT(:)-ZIMAX_ICE(:)*XRHOLW)            !frozen soil
!
! calculate the  total Horton runoff 
!
  WHERE(PFFLOOD(:)<=PFSAT(:))
        PHORTON(:) = (1. -   PFSAT(:)) * (ZHORT_R(:) + ZHORT_M(:))
  ELSEWHERE
        PHORTON(:) = (1. - PFFLOOD(:)) * (ZHORT_R(:) + ZHORT_M(:))
  ENDWHERE
!
ELSE
!
  PHORTON(:) = 0.0
!
ENDIF
!
! calculate all water reaching the ground
!
PPG  (:) = PPG(:) + PPG_MELT(:)        
!
!
!*           3. Dunne runoff and flood interception
!            --------------------------------------
!
! Interception by the flooplains
!
IF(OFLOOD)THEN
  PPFLOOD(:)=PFFLOOD(:)*MAX(0.0,PPG(:))
ELSE
  PPFLOOD(:)=0.0
ENDIF
!
IF(HRUNOFF=='SGH ')THEN
!        
! calculate the Dunne runoff with TOPMODEL
!
  PDUNNE(:) = MAX(PPG(:),0.0) * MAX(PFSAT(:)-PFFLOOD(:),0.0)
!
ELSEIF (HRUNOFF=='DT92' .OR. HRUNOFF=='TOPD')THEN
!
!*       Dumenil et Todini (1992)  RUNOFF SCHEME
!        ---------------------------------------         
!
! surface runoff done only on the Fsat-Fflood fraction
!
  ZPG_WORK(:) = PPG(:) - PHORTON(:) - PPFLOOD(:)
!
#ifdef TOPD
  IF ( LCOUPL_TOPD.AND.HRUNOFF == 'TOPD' )THEN
    !
    DO JJ=1,SIZE(NMASKT_PATCH)
      IF (NMASKT_PATCH(JJ)/=0) THEN
        IF ( XATOP(NMASKT_PATCH(JJ))/=0. .AND. XAS_NATURE(NMASKT_PATCH(JJ))/=XUNDEF ) THEN
          ZRUNOFF_TOPD(JJ) = MAX(PPG(JJ),0.0) * MAX(XAS_NATURE(NMASKT_PATCH(JJ)),0.0)
        ENDIF
      ENDIF 
    ENDDO
    !
  ENDIF
#endif
  !
  CALL HYDRO_DT92(PTSTEP,                                &
                  PRUNOFFB, ZWWILT_AVG,                  &
                  PRUNOFFD, ZWSAT_AVG,                   &
                  ZWG2_AVG, ZWGI2_AVG,                   &
                  ZPG_WORK, ZRUISDT                      )
!
  PDUNNE(:) = ZRUISDT(:)*PRUNOFFD(:)*XRHOLW/PTSTEP
  !
#ifdef TOPD
  IF (LCOUPL_TOPD.AND.HRUNOFF == 'TOPD') THEN
    PDUNNE(:) = ZRUNOFF_TOPD(:) +  PDUNNE(:)*(1-XATOP(NMASKT_PATCH(:)))
  ENDIF
#endif
  !
  IF(OFLOOD)THEN
    WHERE(PFFLOOD(:)>=PFSAT(:).AND.PFFLOOD(:)>0.0)PDUNNE(:) = 0.0
  ENDIF   
  !
ELSE
! 
! Default case (no subgrid runoff)
!
  PFSAT (:) = 0.0
  PDUNNE(:) = 0.0
!
ENDIF
!
! calculate the infiltration rate after runoff
!
PPG  (:) = PPG(:) - PDUNNE(:) - PHORTON(:) - PPFLOOD(:)
!
! Supress numerical artifacts:
!
WHERE (ZPG_INI(:)<0.0)
       PPG(:)     = ZPG_INI(:)
       PHORTON(:) = 0.0
       PDUNNE (:) = 0.0
       PPFLOOD(:) = 0.0
ENDWHERE
!
!*           4. infiltration rate from floodplains (à revoir pour DF !!!)
!            -------------------------------------
!
IF(OFLOOD)THEN
!
! calculate the maximum flood infiltration
!
  ZPIFLDMAX(:) = MIN(PPIFLOOD(:),XRHOLW/XDAY) ! no more than 1 meter of water per days
!
  ZIF_MAX(:) = MAX(0.,(1.- ZFROZEN(:))) * ZIMAX    (:)*XRHOLW &   !unfrozen soil
             +             ZFROZEN(:)   * ZIMAX_ICE(:)*XRHOLW     !frozen soil
!
  IF(HISBA == 'DIF')THEN
    ZDEPTH(:)=0.0
    DO JL=1,KLAYER_HORT
       DO JJ=1,INI
          IF(ZDEPTH(JJ)<XHORT_DEPTH)THEN
            ZSOILMAX(JJ) = ZSOILMAX(JJ)+MAX(0.0,ZWFC(JJ,JL)-PWG(JJ,JL))*PDZG(JJ,JL)*XRHOLW/PTSTEP
            ZDEPTH  (JJ) = PD_G(JJ,JL)
          ENDIF
       ENDDO
    ENDDO
  ELSE
    DO JJ=1,INI
       ZWSAT(JJ,1)  = MAX(XWGMIN, PWSAT(JJ,1)-PWGI(JJ,2)) 
       ZSOILMAX(JJ) = MAX(0.0,ZWSAT(JJ,1)-PWG(JJ,2))*PD_G(JJ,2)*XRHOLW/PTSTEP
    ENDDO
  ENDIF
!
  ZSOILMAX(:) = MIN(ZSOILMAX(:),ZIF_MAX(:))
!
  PIFLOOD(:) = MAX(0.0,(PFFLOOD(:)-PFSAT(:))) * MIN(ZPIFLDMAX(:),ZSOILMAX(:))
!
ELSE
!
  PIFLOOD(:)=0.0
!
ENDIF
!
!calculate the infiltration rate
!
PPG  (:) = PPG(:) + PIFLOOD(:)
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('HYDRO_SGH',1,ZHOOK_HANDLE)
!
END SUBROUTINE HYDRO_SGH
