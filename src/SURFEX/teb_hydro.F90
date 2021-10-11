!   #########
     SUBROUTINE TEB_HYDRO(O, T, HM, KTEB_P,                  & 
                         PTSTEP, PPS, PRR,                   &
                         PRUNOFF_SW, PRUNOFF_WW,             &
                         PRUNOFFSOIL_ROAD,PRUNOFFSOIL_BLD,   &
                         PDRAIN_ROAD, PDRAIN_BLD             )
!   ##########################################################################
!
!!****  *TEB_HYDRO*  
!!
!!    PURPOSE
!!    -------
!
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
!!	A. Lemonsu          * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!    Original    02/2013
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_SURF_PAR,         ONLY : XUNDEF
USE MODD_SURFEX_n,         ONLY : TEB_HYDRO_MODEL_t
USE MODD_ISBA_OPTIONS_n,   ONLY : ISBA_OPTIONS_t
USE MODD_TEB_n,            ONLY : TEB_t
!
USE MODI_URBAN_HYDRO_COND
USE MODI_URBAN_HYDRO_ROAD
USE MODI_URBAN_HYDRO_SOIL
!USE MODI_SOILDIF
USE MODI_SEWER
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    Declarations of arguments
!
TYPE(ISBA_OPTIONS_t),    INTENT(INOUT) :: O 
TYPE(TEB_t),       INTENT(INOUT) :: T
TYPE(TEB_HYDRO_MODEL_t), INTENT(INOUT) :: HM

INTEGER, INTENT(IN) :: KTEB_P                               ! TEB current patch number

REAL                ,    INTENT(IN)    :: PTSTEP            ! Time step
REAL, DIMENSION(:)  ,    INTENT(IN)    :: PPS               ! Surface pressure
REAL, DIMENSION(:)  ,    INTENT(IN)    :: PRR               ! Rain rate
REAL, DIMENSION(:),      INTENT(OUT)   :: PRUNOFF_SW        ! Stormwater sewer runoff    
REAL, DIMENSION(:),      INTENT(OUT)   :: PRUNOFF_WW        ! Wasterwater sewer runoff  
REAL, DIMENSION(:),      INTENT(OUT)   :: PRUNOFFSOIL_ROAD  ! lateral runoff from road soil column
REAL, DIMENSION(:),      INTENT(OUT)   :: PRUNOFFSOIL_BLD   ! lateral runoff from road soil column
REAL, DIMENSION(:),      INTENT(OUT)   :: PDRAIN_ROAD       ! Drainage from road soil column
REAL, DIMENSION(:),      INTENT(OUT)   :: PDRAIN_BLD        ! Drainage from bld  soil column
!
!*      0.2    Declarations of local variables
!
INTEGER, DIMENSION(SIZE(T%XD_BLD,1)) :: JWG_LAYER ! 
REAL, DIMENSION(SIZE(T%XD_BLD,1)) :: ZWSOIL    ! water quantity that infiltrates through the roads (kg/m2)
REAL, DIMENSION(SIZE(T%XD_BLD,1)) :: ZVEG      ! 
REAL, DIMENSION(SIZE(T%XD_BLD,1)) :: ZCV       ! 
REAL, DIMENSION(SIZE(T%XD_BLD,1)) :: ZFFG,ZFFV ! 
REAL, DIMENSION(SIZE(T%XD_BLD,1)) :: ZFWTD     ! grid-cell fraction of water table to rise
REAL, DIMENSION(SIZE(T%XD_BLD,1)) :: ZWTD ! water table depth
REAL, DIMENSION(SIZE(T%XD_BLD,1),SIZE(T%XD_BLD,2)) :: ZTOPQS ! Topmodel subsurface flow by layer (m/s)
REAL, DIMENSION(SIZE(T%XD_BLD,1)) :: ZQSB ! Lateral subsurface flow [kg/m²/s]
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!*      1.     Initializations
!              ---------------
!
IF (LHOOK) CALL DR_HOOK('TEB_HYDRO',0,ZHOOK_HANDLE)
!
!*      1.a    New soil properties according to hydrological state
!
! Default values
ZVEG(:) = 0.
ZCV (:) = 2.E-5
ZFFG(:) = 0.
ZFFV(:) = 0.
ZFWTD(:)= 0. 
!
ZWTD(:)     = XUNDEF  
ZTOPQS(:,:) = 0.      
!
JWG_LAYER(:) = SIZE(T%XD_BLD,2)
!
!-------------------------------------------------------------------------------
!
!*      2.     Hydrology for buildings subsoil
!              -------------------------------
!
! No water infiltration for buildings
ZWSOIL(:)   = 0.
!
CALL URBAN_HYDRO_SOIL(O,HM%THP%XURBDRAIN,                                    &
                      PTSTEP, PPS, ZWSOIL, T%XT_BLD, T%XD_BLD,               &
                      T%XWSAT_BLD, T%XWFC_BLD, T%XWWILT_BLD,                 &
                      T%XCONDSAT_BLD, T%XMPOTSAT_BLD, T%XBCOEF_BLD,          &
                      HM%NTH%AL(KTEB_P)%XWG_BLD, HM%NTH%AL(KTEB_P)%XWGI_BLD, &
                      PRUNOFFSOIL_BLD,  PDRAIN_BLD,                          &
                      ZFWTD, ZWTD, ZTOPQS, ZQSB                              )
!
!-------------------------------------------------------------------------------
!
!*      3.     Hydrology for roads subsoil
!              ---------------------------
!
!*      3.a    Surface hydrology including water infiltration
!
CALL URBAN_HYDRO_ROAD(HM%THP%XWS_ROAD_MAX, HM%THP%XINFIL_ROAD,            &
                      T%XWS_ROAD, PRR, PTSTEP,                      & 
                      ZWSOIL                                        )
!
!*      3.b    Subsoil hydrology based on ISDA-DF
!
!
CALL URBAN_HYDRO_SOIL(O,HM%THP%XURBDRAIN,                                      & 
                      PTSTEP, PPS, ZWSOIL/PTSTEP, T%XT_ROAD, T%XD_ROAD,        &
                      T%XWSAT_ROAD, T%XWFC_ROAD, T%XWWILT_ROAD,                &
                      T%XCONDSAT_ROAD, T%XMPOTSAT_ROAD, T%XBCOEF_ROAD,         &
                      HM%NTH%AL(KTEB_P)%XWG_ROAD, HM%NTH%AL(KTEB_P)%XWGI_ROAD, &
                      PRUNOFFSOIL_ROAD,  PDRAIN_ROAD,                          &
                      ZFWTD, ZWTD, ZTOPQS, ZQSB                                )
!
!*      3.c    Calculation of hydraulic conductivity
!
! - for soil column under buildings
!
CALL URBAN_HYDRO_COND(T%XBCOEF_BLD, T%XWSAT_BLD ,             &
                      T%XCONDSAT_BLD, T%XMPOTSAT_BLD ,        &
                      HM%NTH%AL(KTEB_P)%XWG_BLD, HM%NTH%AL(KTEB_P)%XWGI_BLD, JWG_LAYER,     &
                      HM%NTH%AL(KTEB_P)%XCOND_BLD                            )
!
! - for soil column under roads
!
CALL URBAN_HYDRO_COND(T%XBCOEF_ROAD, T%XWSAT_ROAD,              &
                      T%XCONDSAT_ROAD, T%XMPOTSAT_ROAD,         &
                      HM%NTH%AL(KTEB_P)%XWG_ROAD, HM%NTH%AL(KTEB_P)%XWGI_ROAD, JWG_LAYER,     &
                      HM%NTH%AL(KTEB_P)%XCOND_ROAD                             )
!
!*      3.d    Infiltration through sewer
!
CALL SEWER(PTSTEP,                                                              &
           HM%NTH%AL(KTEB_P)%XWG_ROAD, T%XD_ROAD,                               &
           T%XWSAT_ROAD, T%XWWILT_ROAD, T%XCONDSAT_ROAD,                        &
           HM%THP%XDENS_STORM, HM%THP%XDENS_WASTE, HM%THP%NLAYER_SEWER,         & 
           HM%THP%XIP_SEWER, HM%NTH%AL(KTEB_P)%XCOND_ROAD,                      &
           PRUNOFF_SW, PRUNOFF_WW                                               )
!
!*      3.e    Update of hydraulic conductivity of road soil column

CALL URBAN_HYDRO_COND(T%XBCOEF_ROAD, T%XWSAT_ROAD,              &
                      T%XCONDSAT_ROAD, T%XMPOTSAT_ROAD,         &
                      HM%NTH%AL(KTEB_P)%XWG_ROAD, HM%NTH%AL(KTEB_P)%XWGI_ROAD, JWG_LAYER,     &
                      HM%NTH%AL(KTEB_P)%XCOND_ROAD                             )
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('TEB_HYDRO',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE TEB_HYDRO
