! ########################
        SUBROUTINE URBAN_HYDRO_SOIL(O, PURBDRAIN,                           & 
                                    PTSTEP, PPS, PWSOIL, PTG, PDZG,         &
                                    PWSAT, PWFC, PWWILT,                    &
                                    PCONDSAT, PMPOTSAT, PBCOEF,             &
                                    PWG, PWGI,                              &
                                    PRUNOFFSOIL, PDRAIN ,                   &
                                    PFWTD, PWTD, PTOPQS, PQSB               )
!###############################################
!!****  *URBAN_HYDRO_SOIL*  
!
!!    PURPOSE
!!    -------
!     
!     Calculates the evolution of the water variables for TEB (Road & Roof) i.e.,
!     deep-soil volumetric water content
!     Also determine the runoff and drainage into the soil.
!
!!**  METHOD
!!    ------
!
!     Direct calculation
!
!!    EXTERNAL
!!    --------
!
!     None
!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!!      
!!    REFERENCE
!!    ---------
!!
!!    Boone (2000)
!!    Boone et al. (2000)
!!      
!!    AUTHOR
!!    ------
!!
!!!!      JM Brun		  * IFSTTAR * 
!!
!!    MODIFICATIONS
!!    -------------
!!      Original                  02/04/2012   J-M Brun
!!      A. Lemonsu & C. de Munck     04/2020   + standard CALL HYDRO_SOILDIF            
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODD_CSTS,           ONLY : XTT, XRHOLW
USE MODD_ISBA_PAR,       ONLY : XWGMIN
USE MODD_SURFEX_n,       ONLY : ISBA_MODEL_t
USE MODD_SURFEX_n,       ONLY : TEB_HYDRO_MODEL_t
USE MODD_ISBA_n,         ONLY : ISBA_K_t, ISBA_P_t, ISBA_PE_t 
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_HYDRO_SOILDIF  
!
USE MODE_THERMOS
!
IMPLICIT NONE
!
!----------------------------------------------------------------------------------
!*      0.1    Declarations of arguments
!-----------------------------------------------
!
TYPE(ISBA_OPTIONS_t), INTENT(INOUT) :: O            ! ISBA options
!TYPE(ISBA_K_t),       INTENT(INOUT) :: HK           ! ISBA structures for call hydro_soildif
!TYPE(ISBA_P_t),       INTENT(INOUT) :: HP
!TYPE(ISBA_PE_t),      INTENT(INOUT) :: HPE
REAL, DIMENSION(:)  , INTENT(IN)    :: PURBDRAIN    ! Limitation fraction of urban deep drainage (0-1)
REAL,                 INTENT(IN)    :: PTSTEP       ! Time step (s)
REAL, DIMENSION(:)  , INTENT(IN)    :: PPS          ! Pressure at the surface (Pa)
REAL, DIMENSION(:)  , INTENT(IN)    :: PWSOIL       ! Water that infiltrates from the surface (kg/m2/s)
REAL, DIMENSION(:,:), INTENT(IN)    :: PTG          ! Soil temperature profile (K)
REAL, DIMENSION(:,:), INTENT(IN)    :: PDZG         ! Thickness of soil layers (m)
REAL, DIMENSION(:,:), INTENT(IN)    :: PWSAT        ! Water content at saturation (m3/m3)
REAL, DIMENSION(:,:), INTENT(IN)    :: PWFC         ! Field capacity              (m3/m3)
REAL, DIMENSION(:,:), INTENT(IN)    :: PWWILT       ! Wilting point               (m3/m3)
REAL, DIMENSION(:,:), INTENT(IN)    :: PCONDSAT     ! Hydraulic conductivity at sat (m/s)
REAL, DIMENSION(:,:), INTENT(IN)    :: PMPOTSAT     ! Matrix potential at sat     (m)
REAL, DIMENSION(:,:), INTENT(IN)    :: PBCOEF       ! b parameter for retention curve (-)
REAL, DIMENSION(:,:), INTENT(INOUT) :: PWG          ! Soil volumetric water content profile (m3/m3)
REAL, DIMENSION(:,:), INTENT(INOUT) :: PWGI         ! Soil volumetric water (ice) content profile (m3/m3)
REAL, DIMENSION(:)  , INTENT(OUT)   :: PRUNOFFSOIL  ! Lateral runoff  (kg/m2/s) 
REAL, DIMENSION(:)  , INTENT(OUT)   :: PDRAIN       ! Drainage (kg/m2/s)
!
REAL, DIMENSION(:)  , INTENT(IN)    :: PFWTD        !grid-cell fraction of water table to rise
REAL, DIMENSION(:)  , INTENT(IN)    :: PWTD         !water table depth (m)
REAL, DIMENSION(:,:), INTENT(IN)    :: PTOPQS       !Topmodel subsurface flow by layer (m/s)
REAL, DIMENSION(:)  , INTENT(OUT)   :: PQSB         !Lateral subsurface flow [kg/m²/s]
!
!----------------------------------------------------------------------------------
!*      0.2    Declarations of local variables
!-----------------------------------------------
!
TYPE(ISBA_K_t)  :: HK
TYPE(ISBA_P_t)  :: HP
TYPE(ISBA_PE_t) :: HPE
!
INTEGER :: JWRK      ! loop control                                       
INTEGER :: JJ,JL,IL  ! loop index
INTEGER :: INL,IDEPTH
INTEGER :: IMAX_LAYER
!
REAL,    DIMENSION(SIZE(PTG,1),SIZE(PTG,2))    :: ZLETR                 ! transpiration rate (W m-2)
REAL,    DIMENSION(SIZE(PTG,1))                :: ZLEG                  ! bare-soil evaporation rate (W m-2)
REAL,    DIMENSION(SIZE(PTG,1))                :: ZLEGI                 ! surface layer sublimation rate (W m-2)
REAL,    DIMENSION(SIZE(PTG,1),SIZE(PTG,2))    :: ZROOTFRAC             ! Cumulative root fraction (-)          ! unused
REAL,    PARAMETER                             :: ZRICHARDSDTMAX = 900. ! Maximum timescale for Richard's Eq. (s)
REAL,    DIMENSION(SIZE(PTG,1),SIZE(PTG,2))    :: ZPS,ZTI,ZQSAT,ZQSATI
REAL,    DIMENSION(SIZE(PTG,1),SIZE(PTG,2))    :: ZWSAT                 ! Ice modified soil porosity (m3 m-3) 
REAL,    DIMENSION(SIZE(PTG,1),SIZE(PTG,2))    :: ZDELTA_RECH           ! Recharge of the water content(m3/m3) 
REAL,    DIMENSION(SIZE(PTG,1))                :: ZEXCESS               ! Excess soil water which is used as a constraint
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('URBAN_HYDRO_SOIL',0,ZHOOK_HANDLE)
!
!--------------------------------------------------------------------------
!
!*      0.     Allocation and Initialisation of ISBA structures
!              ------------------------------------------------
!
ALLOCATE(HK%XBCOEF   (SIZE(PWG,1),SIZE(PWG,2)))
ALLOCATE(HK%XWSAT    (SIZE(PWG,1),SIZE(PWG,2)))
ALLOCATE(HK%XMPOTSAT (SIZE(PWG,1),SIZE(PWG,2)))
ALLOCATE(HK%XWFC     (SIZE(PWG,1),SIZE(PWG,2)))
ALLOCATE(HK%XWTD     (SIZE(PWG,1)))
ALLOCATE(HK%XFWTD    (SIZE(PWG,1)))
!
ALLOCATE(HP%XTOPQS   (SIZE(PWG,1),SIZE(PWG,2)))
ALLOCATE(HP%XDG      (SIZE(PWG,1),SIZE(PWG,2)))
ALLOCATE(HP%XDZG     (SIZE(PWG,1),SIZE(PWG,2)))
ALLOCATE(HP%XDZDIF   (SIZE(PWG,1),SIZE(PWG,2)))
ALLOCATE(HP%XCONDSAT (SIZE(PWG,1),SIZE(PWG,2)))
ALLOCATE(HP%NWG_LAYER(SIZE(PWG,1)))
!
ALLOCATE(HPE%XWG     (SIZE(PWG,1),SIZE(PWG,2)))
ALLOCATE(HPE%XWGI    (SIZE(PWG,1),SIZE(PWG,2)))
ALLOCATE(HPE%XTG     (SIZE(PWG,1),SIZE(PWG,2)))
!
HK%XBCOEF  (:,:) = PBCOEF  (:,:)
HK%XWSAT   (:,:) = PWSAT   (:,:)
HK%XMPOTSAT(:,:) = PMPOTSAT(:,:)
HK%XWFC    (:,:) = PWFC    (:,:)
HK%XWTD    (:)   = PWTD    (:)
HK%XFWTD   (:)   = PFWTD   (:)
!
HP%XTOPQS  (:,:) = PTOPQS  (:,:)
HP%XDZG    (:,:) = PDZG    (:,:)
HP%XDG     (:,:) = 0.
HP%XDZDIF  (:,:) = 0.
HP%XCONDSAT(:,:) = PCONDSAT(:,:)
HP%NWG_LAYER(:)  = SIZE(PTG,2)
!
HPE%XWG    (:,:) = PWG     (:,:)
HPE%XWGI   (:,:) = PWGI    (:,:)
HPE%XTG    (:,:) = PTG     (:,:)
!
!*      1.     Initialisation
!              --------------
!
ZWSAT(:,:) = MAX(XWGMIN, PWSAT(:,:)-HPE%XWGI(:,:))  
ZEXCESS(:) = 0.
!
! Root fraction is zero under roads and buildings
ZROOTFRAC(:,:) = 0.
!
! Topmodel saturated fraction, initialisé à 0. dans GARDEN, inutilisé dans URBAN_HYDRO_SOILDIF
!
! No evaporation
ZLETR(:,:)       =  0.
ZLEG(:)          =  0.
ZLEGI(:)         =  0.
!
! Moisture layers
IMAX_LAYER   = SIZE(PTG,2)
!
! Cumulative depth of soil layers
HP%XDG(:,1) = PDZG(:,1) 
DO JWRK=2,SIZE(PDZG,2) 
   HP%XDG(:,JWRK) = HP%XDG(:,JWRK-1) + PDZG(:,JWRK)  
END DO
!
! Distance between the midpoints of two consecutive layers (m)
DO JWRK=1,SIZE(PDZG,2)-1
   HP%XDZDIF(:,JWRK) = (PDZG(:,JWRK) + PDZG(:,JWRK+1))/2.  
END DO
HP%XDZDIF(:,SIZE(PDZG,2)) = PDZG(:,SIZE(PDZG,2))/2. 
!
!
!*      2.     RUN-OFF (cf hydro.F90) 
!              ----------------------
!
INL = SIZE(PTG,2)
!
! Initialize some field
! ---------------------
!
ZPS(:,:) = XUNDEF
ZTI(:,:) = XUNDEF
DO JL=1,INL
   DO JJ=1,SIZE(PTG,1)
      IDEPTH=HP%NWG_LAYER(JJ)
      IF(JL<=IDEPTH)THEN
        ZPS(JJ,JL) = PPS(JJ)
        ZTI(JJ,JL) = MIN(XTT,PTG(JJ,JL))
      ENDIF
   ENDDO
ENDDO
!

! Compute specific humidity at saturation for the vapor conductivity
! ------------------------------------------------------------------
!
ZQSAT (:,:) = QSAT (PTG(:,:),ZPS(:,:),HP%NWG_LAYER(:),INL)
ZQSATI(:,:) = QSATI(ZTI(:,:),ZPS(:,:),HP%NWG_LAYER(:),INL) 
!
!*      3.     Call hydro_soildif
!              ------------------
!
PRUNOFFSOIL(:) = 0.
! 
! PWSOIL is converted in m/s
!
CALL HYDRO_SOILDIF(O, HK, HP, HPE, PTSTEP,                   &
                   PWSOIL/XRHOLW, ZLETR, ZLEG, ZLEGI,        & 
                   PPS, ZQSAT, ZQSATI,                       &
                   PDRAIN, PRUNOFFSOIL,                      &
                   IMAX_LAYER, PQSB                          )
!
  DO JJ=1,SIZE(PTG,1)
!
    IDEPTH=HP%NWG_LAYER(JJ)
!
    ZDELTA_RECH(JJ,IDEPTH) = PURBDRAIN(JJ) *PDRAIN(JJ) !eb2011
!   Drainage or baseflow out of bottom of model (slow time response) (kg m-2 s-1):
    PDRAIN(JJ)     = (PDRAIN(JJ) - ZDELTA_RECH(JJ,IDEPTH)) !eb2011
!   Update liquide water content (m3/m3)
    HPE%XWG(JJ,IDEPTH) = HPE%XWG(JJ,IDEPTH) + ZDELTA_RECH(JJ,IDEPTH)*PTSTEP/XRHOLW/PDZG(JJ,IDEPTH)!eb2011
!
!   When the water content in the last layer exceeds the water content at saturation, 
!   then the water surplus is added to the layers above:   
    IF(HPE%XWG(JJ,IDEPTH)>ZWSAT(JJ,IDEPTH)) THEN
!
      IL=IDEPTH
!
!     Calculation of the water surplus in last layer:
      ZEXCESS(JJ)= MAX(0.0, HPE%XWG(JJ,IL) - ZWSAT(JJ,IL))

!     Water content that stays in last layer:
      HPE%XWG(JJ,IL) = MIN(HPE%XWG(JJ,IL),ZWSAT(JJ,IL))
! 
      DO WHILE (ZEXCESS(JJ) > 0.0 .AND. IL > 1) ! test
!        
!       Update of the water content in the layer above:
        HPE%XWG(JJ,IL-1) = HPE%XWG(JJ,IL-1) + ZEXCESS(JJ)*(PDZG(JJ,IL)/PDZG(JJ,IL-1))

!       Calculation of the water surplus in layer above:
        ZEXCESS(JJ)  = MAX(0.0, HPE%XWG(JJ,IL-1) - ZWSAT(JJ,IL-1))

!       Water content that stays in current layer:
        HPE%XWG(JJ,IL-1) = MIN(HPE%XWG(JJ,IL-1),ZWSAT(JJ,IL-1)) 

!       Index of layer is incremented to the top
        IL=IL-1
!
      ENDDO
!
      IF(IL==1 .AND. HPE%XWG(JJ,1)>ZWSAT(JJ,1)) THEN
!
!       Calculation of the water surplus in first layer:
        ZEXCESS(JJ)     = MAX(0.0, HPE%XWG(JJ,1) - ZWSAT(JJ,1))

!       Water content that stays in first layer:
        HPE%XWG(JJ,1)       = MIN(HPE%XWG(JJ,1),ZWSAT(JJ,1))

!       Water excess is added to surface runoff mm/s:
        PRUNOFFSOIL(JJ) = PRUNOFFSOIL(JJ) + (ZEXCESS(JJ) * (PDZG(JJ,1)/PTSTEP)) * XRHOLW 
!
      ENDIF
    ENDIF
  ENDDO
!
!
PWG(:,:)  = HPE%XWG    (:,:)
PWGI(:,:) = HPE%XWGI   (:,:)
!
! Desallocation of local structure variables :
! -------------------------------------------
DEALLOCATE(HK%XBCOEF   )
DEALLOCATE(HK%XWSAT    ) 
DEALLOCATE(HK%XMPOTSAT ) 
DEALLOCATE(HK%XWFC     )
DEALLOCATE(HK%XWTD     ) 
DEALLOCATE(HK%XFWTD    )
!
DEALLOCATE(HP%XTOPQS   )
DEALLOCATE(HP%XDG      )
DEALLOCATE(HP%XDZG     )
DEALLOCATE(HP%XDZDIF   )
DEALLOCATE(HP%XCONDSAT ) 
DEALLOCATE(HP%NWG_LAYER)
!
DEALLOCATE(HPE%XWG     )
DEALLOCATE(HPE%XWGI    )
DEALLOCATE(HPE%XTG     )
!
!-------------------------------------------------------------------------------
!
!
IF (LHOOK) CALL DR_HOOK('URBAN_HYDRO_SOIL',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE URBAN_HYDRO_SOIL


