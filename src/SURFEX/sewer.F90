!     #########
    SUBROUTINE SEWER(PTSTEP,                                            &
                     PWG, PD,                                           &
                     PWSAT_ROAD, PWWILT_ROAD, PCONDSAT_ROAD,            &
                     PDENSITY_SW, PDENSITY_WW, KLAYER_SEWER, PIP_SEWER, &
                     PCOND_ROAD,                                        &
                     PRUNOFF_SW, PRUNOFF_WW                             )
!   ##########################################################################
!
!!****  *SEWER*  
!!
!!    PURPOSE
!!    -------
!
!     Computes the evolution of infiltration in sewers from soil water in urbanized areas.
!         
!!**  METHOD
!     ------
!
!   (evacuation system, typical time scale: ???? day)
!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!      
!!    REFERENCE
!!    ---------
!!      
!!    AUTHOR
!!    ------
!!
!!      JM Brun     04/12   ** IFSTTAR **
!!
!!    MODIFICATIONS
!!    -------------
!!
!!      Original    08/12 
!!      C.de Munck  07/20 Separation of waste water and storm water runoffs
!!      
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
USE MODD_ISBA_PAR,     ONLY : XWGMIN
USE MODD_CSTS,         ONLY : XRHOLW
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!---------------------------------------------
!*      0.1    declarations of arguments
!---------------------------------------------
!
REAL,                    INTENT(IN)    :: PTSTEP          ! Time step
REAL,    DIMENSION(:,:), INTENT(INOUT) :: PWG             ! water reservoir (m3/m3)
REAL,    DIMENSION(:,:), INTENT(IN)    :: PD              ! soil layer thickness  (m)
REAL,    DIMENSION(:,:), INTENT(IN)    :: PWSAT_ROAD      ! water content at saturation
REAL,    DIMENSION(:,:), INTENT(IN)    :: PWWILT_ROAD     ! wilting point volumetric water content profile
REAL,    DIMENSION(:,:), INTENT(IN)    :: PCONDSAT_ROAD   ! hydraulic conductivity at saturation    (m/s)
REAL,    DIMENSION(:),   INTENT(IN)    :: PDENSITY_SW     ! Total stormwater sewer length density (-)
REAL,    DIMENSION(:),   INTENT(IN)    :: PDENSITY_WW     ! Total waste water sewer length density (-)
INTEGER, DIMENSION(:),   INTENT(IN)    :: KLAYER_SEWER    ! Ground layer where the sewer is located (-)
REAL,    DIMENSION(:),   INTENT(IN)    :: PIP_SEWER       ! Parameter for parasite infiltrations into sewer (-)
REAL,    DIMENSION(:,:), INTENT(IN)    :: PCOND_ROAD      ! hydraulic conductivity (m/s)
REAL,    DIMENSION(:)  , INTENT(OUT)   :: PRUNOFF_SW      ! stormwater sewer runoff (kg/m2/s)  
REAL,    DIMENSION(:)  , INTENT(OUT)   :: PRUNOFF_WW      ! wastewater sewer runoff (kg/m2/s)  
!
!---------------------------------------------
!*      0.2    declarations of local variables
!---------------------------------------------
!*
INTEGER :: JWRK     ! loop counter
REAL    :: ZWG, ZINFIL_SW, ZINFIL_WW, ZCOND_ROAD 
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!*      1.     Initialisation
!              --------------
!                                           
IF (LHOOK) CALL DR_HOOK('SEWER',0,ZHOOK_HANDLE)
!
! Initialization
!
PRUNOFF_SW(:) = 0.0 
PRUNOFF_WW(:) = 0.0 
!
! Option for calculation
!
!
ZWG = 0.
ZINFIL_SW = 0. 
ZINFIL_WW = 0. 
!
DO JWRK=1,SIZE(PD,1)
!
  IF (KLAYER_SEWER(JWRK).GT.0) THEN
!
!*      2.     Calculation of the soil water infiltration rate into the sewer
!              --------------------------------------------------------------
!
    ZCOND_ROAD   = MIN(PCOND_ROAD(JWRK,KLAYER_SEWER(JWRK)),PCONDSAT_ROAD(JWRK,KLAYER_SEWER(JWRK)))
    ZINFIL_SW = ZCOND_ROAD*PIP_SEWER(JWRK)*PDENSITY_SW(JWRK)
    ZINFIL_WW = ZCOND_ROAD*PIP_SEWER(JWRK)*PDENSITY_WW(JWRK)
!
!*      3.     Calculation of the new soil water content
!              ----------------------------------------- 
!
!   New soil water content and water quantities that joins the sewers :
    ZWG = PTSTEP * (ZINFIL_SW + ZINFIL_WW) / PD(JWRK,KLAYER_SEWER(JWRK))
    PWG(JWRK,KLAYER_SEWER(JWRK)) = PWG(JWRK,KLAYER_SEWER(JWRK)) - ZWG
!
    IF  (PWG(JWRK,KLAYER_SEWER(JWRK)) < XWGMIN) THEN
      PRUNOFF_SW(JWRK) = (ZINFIL_SW -                              & 
                         (XWGMIN-PWG(JWRK,KLAYER_SEWER(JWRK)))*    &
                         PD(JWRK,KLAYER_SEWER(JWRK))/PTSTEP)*XRHOLW
                
      PRUNOFF_WW(JWRK) = (ZINFIL_WW -                              & 
                         (XWGMIN-PWG(JWRK,KLAYER_SEWER(JWRK)))*    &
                         PD(JWRK,KLAYER_SEWER(JWRK))/PTSTEP)*XRHOLW
      PWG(JWRK,KLAYER_SEWER(JWRK)) = XWGMIN
    ELSE
      PRUNOFF_SW(JWRK) = ZINFIL_SW *XRHOLW        ! (kg/m2/s) 
      PRUNOFF_WW(JWRK) = ZINFIL_WW *XRHOLW        ! (kg/m2/s) 
    ENDIF
!
  ENDIF
!
ENDDO
!
!
IF (LHOOK) CALL DR_HOOK('SEWER',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE SEWER
