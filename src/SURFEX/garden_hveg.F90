!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
    SUBROUTINE GARDEN_HVEG (DTCO, G, SB, T, TOP, GDM, DMT, KTEB_P, TPTIME, PTSUN, &
                       PTSTEP, PZREF, PTA, PQA, PEXNS, PRHOA, PCO2, PPS,PZENITH, &
                       PVMOD,PSFCO2, PDH_HVEG, PDLE_HVEG, PLAD_CAN,              &
                       PQSAT_HVEG, PAC_HVEG, PHV, PLE_HVEG                      )
!   ##########################################################################
!
!!****  *GARDEN*  
!!
!!    PURPOSE
!!    -------
!
!!call the vegetation scheme (ISBA) inside TEB
!     
!!**  METHOD
!     ------
!
!
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!      
!!    REFERENCE
!!    ---------
!!
!!      
!!    AUTHOR
!!    ------
!!
!!      A. Lemonsu          * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!    Original    05/2009
!     B. decharme 04/2013 : variables for surf/atm coupling
!                           dummy for water table / surface coupling
!!    P. Samuelsson  10/2014  Introduced dummy variables in call to ISBA for MEB
!!    E. Redon       06/2017  Add net IR rad received by urban trees
!!    M. Goret       07/2017  Replace the if statement on GDM%TVG%CPHOTO by select case
!!    M. Goret       08/2017  Add garden diagnostics fill in
!!    C. de Munck et E. Bernard 10/2019  Added runoff of roads and roofs not connected to sewer 
!!                                       to garden irrigation (hydro) 
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_SFX_GRID_n, ONLY : GRID_t
USE MODD_SURFEX_n, ONLY : TEB_GARDEN_MODEL_t
USE MODD_DIAG_MISC_TEB_n, ONLY : DIAG_MISC_TEB_t
USE MODD_DIAG_EVAP_ISBA_n, ONLY : DIAG_EVAP_ISBA_t
USE MODD_TEB_n, ONLY : TEB_t
USE MODD_TEB_OPTION_n, ONLY : TEB_OPTIONS_t
USE MODD_SSO_n, ONLY : SSO_t, SSO_INIT
USE MODD_CANOPY_n, ONLY : CANOPY_t
USE MODD_SURF_ATM_n,        ONLY : SURF_ATM_t
!
USE MODD_TYPE_DATE_SURF,    ONLY: DATE_TIME
USE MODD_SURF_PAR,          ONLY: XUNDEF
USE MODD_CSTS,              ONLY: XCPD
USE MODD_ISBA_PAR,          ONLY: XEMISVEG
USE MODD_AGRI,              ONLY : LAGRIP, LIRRIGMODE
!
!
USE MODI_ISBA_HVEG
USE MODI_VEGETATION_UPDATE
USE MODE_THERMOS
!
USE MODI_VEGETATION_EVOL
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    Declarations of arguments
!
!
!
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(GRID_t), INTENT(INOUT) :: G
TYPE(TEB_t), INTENT(INOUT) :: T
TYPE(TEB_OPTIONS_t), INTENT(INOUT) :: TOP
TYPE(CANOPY_t),      INTENT(INOUT) :: SB
!
TYPE(TEB_GARDEN_MODEL_t), INTENT(INOUT) :: GDM
TYPE(DIAG_MISC_TEB_t), INTENT(INOUT) :: DMT
INTEGER,              INTENT(IN)    :: KTEB_P             ! TEB patch number
TYPE(DATE_TIME)     , INTENT(IN)    :: TPTIME             ! current date and time from teb
REAL, DIMENSION(:)  , INTENT(IN)    :: PTSUN              ! solar time      (s from midnight)
REAL                , INTENT(IN)    :: PTSTEP             ! time step
REAL, DIMENSION(:)  , INTENT(IN)    :: PZREF              ! height of atm. var. near the road
REAL, DIMENSION(:)  , INTENT(IN)    :: PTA                ! temp. near the road
REAL, DIMENSION(:)  , INTENT(IN)    :: PQA                ! hum. near the road
REAL, DIMENSION(:)  , INTENT(IN)    :: PPS                ! pressure at the surface
REAL, DIMENSION(:)  , INTENT(IN)    :: PEXNS              ! surface exner function
REAL, DIMENSION(:)  , INTENT(IN)    :: PRHOA              ! air density at the lowest level
REAL, DIMENSION(:)  , INTENT(IN)    :: PCO2               ! CO2 concentration in the air    (kg/m3)
REAL, DIMENSION(:)  , INTENT(IN)    :: PZENITH            ! solar zenithal angle
REAL, DIMENSION(:)  , INTENT(IN)    :: PVMOD              ! wind near the road
!
REAL, DIMENSION(:)  , INTENT(OUT)   :: PSFCO2             ! flux of CO2 positive toward the atmosphere (m/s*kg_CO2/kg_air)
REAL, DIMENSION(:,:), INTENT(OUT)   :: PDH_HVEG           ! sensible heat flux from trees discretized on canopy grid
REAL, DIMENSION(:,:), INTENT(OUT)   :: PDLE_HVEG          ! sensible heat flux from trees discretized on canopy grid
REAL, DIMENSION(:,:), INTENT(OUT)   :: PLAD_CAN           ! vertical profile of Leaf Area Density on canopy grid
!
!* For determination of exchanges for canyon air
!  ---------------------------------------------
!
REAL, DIMENSION(:),    INTENT(OUT) :: PQSAT_HVEG  ! humidity at saturation for temperature of leaves
REAL, DIMENSION(:),    INTENT(OUT) :: PAC_HVEG    ! aerodynamical conductance for sensible heat flux
REAL, DIMENSION(:),    INTENT(OUT) :: PHV         ! Halfseatd coefficient (relative humidity) of trees
REAL, DIMENSION(:,:),  INTENT(OUT) :: PLE_HVEG    ! soil profile of transpiration from trees
!
!
!*      0.2    Declarations of local variables
!
!TYPE(DIAG_EVAP_ISBA_t), POINTER :: GDDE
!
REAL, DIMENSION(SIZE(PPS),GDM%O%NNBIOMASS) :: ZRESP_BIOMASS_INST       ! instantaneous biomass respiration (kgCO2/kgair m/s)
!
TYPE(SSO_t)            :: YSS
!
REAL, DIMENSION(SIZE(PPS)) :: ZTA ! estimate of air temperature at future time
!                                 ! step as if modified by ISBA flux alone.
!
REAL, DIMENSION(SIZE(PTA)) :: ZEMIS_HVEG      ! emissivity for high vegetation
REAL, DIMENSION(SIZE(PTA)) :: ZGPP            ! Gross Primary Production                    (kgCO2/m2/s)

REAL, DIMENSION(SIZE(PTA),SIZE(GDM%S%XABC)) :: ZIACAN
INTEGER                    :: ILU
INTEGER                    :: JI, JLAYER
REAL                       :: ZWORK1, ZWORK2
LOGICAL                    :: GUPDATED
REAL, DIMENSION(SIZE(PTA),SB%NLVL):: ZH         ! Foliage thickness in vertical layers of canopy grid
REAL, DIMENSION(SIZE(PTA),SB%NLVL):: ZZ         ! Lower limit of layers from CANOPY grid
REAL, DIMENSION(SIZE(PTA),SB%NLVL):: ZLAD_PCHV  ! vertical profile of percentage of Leaf Area Density on canopy grid
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!*      1.     various initialisations
!              -----------------------
!
IF (LHOOK) CALL DR_HOOK('GARDEN',0,ZHOOK_HANDLE)
!
ILU = SIZE(PPS)
ZEMIS_HVEG = XEMISVEG
!
!GDDE => GDM%VD%NDE%AL(KTEB_P)
!
! --------------------------------------------------------------------------------------
!
!       2.     Vegetation update (in case of non-interactive vegetation)
!              ---------------------------------------------------------
!
!
GUPDATED=.FALSE.
CALL SSO_INIT(YSS)
!
CALL VEGETATION_UPDATE(DTCO, GDM%DTHV, G%NDIM, GDM%O, GDM%K, GDM%PHV, GDM%NPEHV%AL(KTEB_P), &
                       1, 1, 1, PTSTEP, TPTIME, TOP%XCOVER, TOP%LCOVER, .FALSE., &
                       .FALSE., .FALSE.,'GRD', YSS, GUPDATED, OABSENT=(T%XGARDEN==0.),&
                       OHG=TOP%LGARDEN, OHV=TOP%CURBTREE/='NONE'                   )
!
! --------------------------------------------------------------------------------------
!
!*      3.     Call simplified ISBA routines for high vegetation+ PAC_TOP(JJ) &
!              -------------------------------------------------
!
!
!
! put soil water content into high vegetation structure for soilstress computations
ALLOCATE(GDM%PHV%NWG_LAYER        (ILU)                    ) ; GDM%PHV%NWG_LAYER        = GDM%P%NWG_LAYER 
ALLOCATE(GDM%NPEHV%AL(KTEB_P)%XWG (ILU,GDM%O%NGROUND_LAYER)) ; GDM%NPEHV%AL(KTEB_P)%XWG = GDM%NPE%AL(KTEB_P)%XWG
ALLOCATE(GDM%NPEHV%AL(KTEB_P)%XWGI(ILU,GDM%O%NGROUND_LAYER)) ; GDM%NPEHV%AL(KTEB_P)%XWGI= GDM%NPE%AL(KTEB_P)%XWGI
!
CALL ISBA_HVEG(GDM%O, GDM%K, GDM%PHV, GDM%NPEHV%AL(KTEB_P), TPTIME,GDM%S%XPOI,GDM%S%XABC,ZIACAN,&
            PTSTEP, PZREF, PTA, PQA, PEXNS, PRHOA, PPS, PZENITH,                                &
            DMT%XABS_SW_HVEG, DMT%XABS_LW_HVEG, PVMOD, PCO2, ZRESP_BIOMASS_INST, ZGPP, PHV,     &
            PLE_HVEG, DMT%XRN_HVEG, DMT%XH_HVEG, DMT%XLE_HVEG, DMT%XGFLUX_HVEG, GDM%DTV%NPAR_VEG_IRR_USE)

DEALLOCATE(GDM%NPEHV%AL(KTEB_P)%XWG )
DEALLOCATE(GDM%NPEHV%AL(KTEB_P)%XWGI)
!
! calculation of vegetation CO2 flux
PSFCO2(:) = - ZGPP(:)
!
!  when there is no high vegetation, the foliage temperature is set to the air temperature (for convenience)
!
WHERE (T%XFRAC_HVEG(:)==0. .OR. GDM%NPEHV%AL(KTEB_P)%XLAI(:)==0.) GDM%NPEHV%AL(KTEB_P)%XTV(:)=PTA(:)
!
! --------------------------------------------------------------------------------------
!
!        4.    desaggregation of the fluxes on the vertical on SBL layers (if option activated)
!              ----------------------------------------------------------
!
PAC_HVEG    = 0.
PQSAT_HVEG  = 0.

  IF (TOP%LCANOPY) THEN 
     !
     ! Calculation of the vertical profile of PERCENTAGE of LAD on CANOPY grid
     ! for vertical distribution of turbulent fluxes
     ! ZZ = lower levels of CANOPY layers
     ! XZ = higher levels of CANOPY layers
     ZZ(:,:) = 0.
     DO JLAYER=1,SB%NLVL-1
       ZZ(:,JLAYER+1) = SB%XZ(:,JLAYER)
     ENDDO
     !
     DO JI=1,SIZE(GDM%PHV%XH_TREE)           
       IF (T%XURBTREE(JI).GT.0. .AND. GDM%NPEHV%AL(KTEB_P)%XLAI(JI)>0.)  THEN    
         DO JLAYER=1,SB%NLVL  
           ZWORK1 = SB%XZF(JI,JLAYER)             
           ZWORK2 = SB%XZF(JI,JLAYER)+SB%XDZ(JI,JLAYER)              
           IF (GDM%PHV%XHTRUNK_HVEG(JI) .GT. ZWORK2) THEN 
             ZH(JI,JLAYER) = 0.                                  
           ELSE IF (GDM%PHV%XH_TREE(JI) .LE. ZWORK1) THEN
             ZH(JI,JLAYER) = 0.
           ELSE IF (GDM%PHV%XHTRUNK_HVEG(JI) .GT. ZWORK1) THEN
             IF (GDM%PHV%XH_TREE(JI) .GT. ZWORK2) THEN
               ZH(JI,JLAYER) = ZWORK2-GDM%PHV%XHTRUNK_HVEG(JI)
             ELSE
               ZH(JI,JLAYER) = GDM%PHV%XH_TREE(JI)-GDM%PHV%XHTRUNK_HVEG(JI)
             ENDIF
           ELSE IF (GDM%PHV%XH_TREE(JI) .GT. ZWORK2) THEN
             ZH(JI,JLAYER) = ZWORK2-ZWORK1
           ELSE
             ZH(JI,JLAYER) = GDM%PHV%XH_TREE(JI)-ZWORK1
           ENDIF
           PLAD_CAN(JI,JLAYER)  = GDM%NPEHV%AL(KTEB_P)%XLAI(JI)* &
                          (ZH(JI,JLAYER)/(GDM%PHV%XH_TREE(JI)-GDM%PHV%XHTRUNK_HVEG(JI)))
           ZLAD_PCHV(JI,JLAYER) = PLAD_CAN(JI,JLAYER) / GDM%NPEHV%AL(KTEB_P)%XLAI(JI)
           PDH_HVEG(JI,JLAYER)  = DMT%XH_HVEG(JI)  * ZLAD_PCHV(JI,JLAYER)
           PDLE_HVEG(JI,JLAYER) = DMT%XLE_HVEG(JI) * ZLAD_PCHV(JI,JLAYER)
         ENDDO
       ELSE
         ZLAD_PCHV(JI,:)=0.
         PLAD_CAN(JI,:)=0.
         PDH_HVEG(JI,:)=0.
         PDLE_HVEG(JI,:)=0.
       ENDIF
     ENDDO
! 
  ELSE
!
!* For determination of exchanges for canyon air if CANOPY not activated
!
    PQSAT_HVEG = QSAT(GDM%NPEHV%AL(KTEB_P)%XTV,PPS)
    WHERE (GDM%NPEHV%AL(KTEB_P)%XTV/=PTA) PAC_HVEG = MAX(DMT%XH_HVEG  / (GDM%NPEHV%AL(KTEB_P)%XTV-PTA) / XCPD / PRHOA,0.)
  END IF
!
! --------------------------------------------------------------------------------------
!
!      5.     Vegetation evolution for interactive LAI
!             ----------------------------------------
!
IF (GDM%O%CPHOTO=='NIT') THEN
  !
  ! needs to use soil temperatures
  !
  ALLOCATE(GDM%NPEHV%AL(KTEB_P)%XTG(ILU,GDM%O%NGROUND_LAYER))
  ALLOCATE(GDM%PHV%XDG             (ILU,GDM%O%NGROUND_LAYER))
  GDM%NPEHV%AL(KTEB_P)%XTG      = GDM%NPE%AL(KTEB_P)%XTG
  GDM%NPEHV%AL(KTEB_P)%XTG(:,1) = GDM%NPEHV%AL(KTEB_P)%XTV
  GDM%PHV%XDG                   = GDM%P%XDG
  IF (GDM%O%CISBA=='DIF') THEN
    ALLOCATE(GDM%PHV%XDZG (ILU,GDM%O%NGROUND_LAYER))
    GDM%PHV%XDZG = GDM%P%XDZG
  ELSE
    ALLOCATE(GDM%PHV%XDZG (0,0))
  END IF
  !
  ! evolution of LAI
  !
  CALL VEGETATION_EVOL(GDM%O, GDM%DTHV, GDM%PHV, GDM%NPEHV%AL(KTEB_P), GDM%VD%NDE%AL(KTEB_P),&
                       .FALSE., PTSTEP, TPTIME%TDATE%MONTH, TPTIME%TDATE%DAY, TPTIME%TIME,   &
                       G%XLAT, PCO2, YSS, ZRESP_BIOMASS_INST, .FALSE., OTEB_HVEG_ONLY=.TRUE. )         
  !
  DEALLOCATE(GDM%NPEHV%AL(KTEB_P)%XTG)
  DEALLOCATE(GDM%PHV%XDG)
  DEALLOCATE(GDM%PHV%XDZG)
END IF
!
DEALLOCATE(GDM%PHV%NWG_LAYER)
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('GARDEN',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
!
END SUBROUTINE GARDEN_HVEG
