!     #################################
      SUBROUTINE BUDGET_HYDRO_n(DMTC, DMT, GDDC, GDDEC, GDDE, GRDC, GRDEC, T, P, PEK, TH, TOP)
!     #################################
!
!!****  *BUDGET_HYDRO* - calculates the water budget
!!
!!    PURPOSE
!!    -------
!!
!!
!!**  METHOD
!!    ------
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!	P. Le Moigne   *Meteo France*	
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    10/2004
!!      Modif       08/2017  X.Laffaille: Correction of water budget
!!      Modif       11/2019  C. de Munck: Update to V8.1 and corrections
!!      Modif       03/2021  E. Bernard : Including irrig, snow and ice 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODI_OPEN_FILE
USE MODI_WRITE_SURF
!
USE MODD_DIAG_MISC_TEB_n,  ONLY : DIAG_MISC_TEB_t
USE MODD_DIAG_EVAP_ISBA_n, ONLY : DIAG_EVAP_ISBA_t
USE MODD_DIAG_n,           ONLY : DIAG_t
USE MODD_ISBA_n,           ONLY : ISBA_P_t, ISBA_PE_t
USE MODD_TEB_n,            ONLY : TEB_t
USE MODD_TEB_OPTION_n,     ONLY : TEB_OPTIONS_t
USE MODD_TEB_HYDRO_n,      ONLY : TEB_HYDRO_t
USE MODD_CSTS,             ONLY : XRHOLW, XLMTT
!
USE MODD_CSTS,             ONLY : XLVTT, XLSTT          
USE MODD_SURF_PAR,         ONLY : XUNDEF
!
USE YOMHOOK   ,ONLY : LHOOK, DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
TYPE(DIAG_MISC_TEB_t),  INTENT(INOUT) :: DMTC
TYPE(DIAG_MISC_TEB_t),  INTENT(INOUT) :: DMT
TYPE(DIAG_t),           INTENT(INOUT) :: GDDC 
TYPE(DIAG_EVAP_ISBA_t), INTENT(INOUT) :: GDDEC
TYPE(DIAG_EVAP_ISBA_t), INTENT(INOUT) :: GDDE
TYPE(DIAG_t),           INTENT(INOUT) :: GRDC
TYPE(DIAG_EVAP_ISBA_t), INTENT(INOUT) :: GRDEC
TYPE(ISBA_P_t),         INTENT(INOUT) :: P
TYPE(ISBA_PE_t),        INTENT(INOUT) :: PEK
TYPE(TEB_t),            INTENT(INOUT) :: T
TYPE(TEB_HYDRO_t),      INTENT(INOUT) :: TH
TYPE(TEB_OPTIONS_t),    INTENT(INOUT) :: TOP
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
REAL, DIMENSION(:), ALLOCATABLE      :: ZWG_ROAD_INI, ZWG_BLD_INI, ZWG_GARDEN_INI        ! Initial groundwater storage
REAL, DIMENSION(:), ALLOCATABLE      :: ZWG_ROAD_FIN, ZWG_BLD_FIN, ZWG_GARDEN_FIN        ! Final groundwater storage
REAL, DIMENSION(:), ALLOCATABLE      :: ZWGI_ROAD_INI, ZWGI_BLD_INI, ZWGI_GARDEN_INI     ! Initial ground ice storage
REAL, DIMENSION(:), ALLOCATABLE      :: ZWGI_ROAD_FIN, ZWGI_BLD_FIN, ZWGI_GARDEN_FIN     ! Final ground ice storage
REAL, DIMENSION(:), ALLOCATABLE      :: ZRUNOFF_TOWN, ZRUNOFF_GD, ZRUNOFF_RD, ZRUNOFF_RF 
REAL, DIMENSION(:), ALLOCATABLE      :: ZRUNOFF_STRF, ZRUNOFF_GR,ZRUNOFF_SW, ZRUNOFF_WW   
REAL, DIMENSION(:), ALLOCATABLE      :: ZIN, ZOUT
REAL, DIMENSION(:), ALLOCATABLE      :: ZRUNOFFSOIL_BLD, ZRUNOFFSOIL_RD              
!
!-------------------------------------------------------------------------------
!
!         Initialisation for IO
!
IF (LHOOK) CALL DR_HOOK('BUDGET_HYDRO_N',0,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!*       Total Budget
!        ------------
!
!* Allocations  
!
ALLOCATE(ZWG_ROAD_INI  (SIZE(DMTC%XRUNOFF_TOWN)))
ALLOCATE(ZWG_BLD_INI   (SIZE(DMTC%XRUNOFF_TOWN)))
ALLOCATE(ZWG_GARDEN_INI(SIZE(DMTC%XRUNOFF_TOWN)))
ALLOCATE(ZWG_ROAD_FIN  (SIZE(DMTC%XRUNOFF_TOWN)))
ALLOCATE(ZWG_BLD_FIN   (SIZE(DMTC%XRUNOFF_TOWN)))
ALLOCATE(ZWG_GARDEN_FIN(SIZE(DMTC%XRUNOFF_TOWN)))
!
ALLOCATE(ZWGI_ROAD_INI  (SIZE(DMTC%XRUNOFF_TOWN)))
ALLOCATE(ZWGI_BLD_INI   (SIZE(DMTC%XRUNOFF_TOWN)))
ALLOCATE(ZWGI_GARDEN_INI(SIZE(DMTC%XRUNOFF_TOWN)))
ALLOCATE(ZWGI_ROAD_FIN  (SIZE(DMTC%XRUNOFF_TOWN)))
ALLOCATE(ZWGI_BLD_FIN   (SIZE(DMTC%XRUNOFF_TOWN)))
ALLOCATE(ZWGI_GARDEN_FIN(SIZE(DMTC%XRUNOFF_TOWN)))
!
ALLOCATE(ZRUNOFF_TOWN    (SIZE(DMTC%XRUNOFF_TOWN)))
ALLOCATE(ZRUNOFF_GD      (SIZE(DMTC%XRUNOFF_TOWN)))
ALLOCATE(ZRUNOFF_RD      (SIZE(DMTC%XRUNOFF_TOWN)))
ALLOCATE(ZRUNOFF_RF      (SIZE(DMTC%XRUNOFF_TOWN)))
ALLOCATE(ZRUNOFF_STRF    (SIZE(DMTC%XRUNOFF_TOWN)))
ALLOCATE(ZRUNOFF_GR      (SIZE(DMTC%XRUNOFF_TOWN)))
ALLOCATE(ZRUNOFF_SW      (SIZE(DMTC%XRUNOFF_TOWN)))
ALLOCATE(ZRUNOFF_WW      (SIZE(DMTC%XRUNOFF_TOWN)))
!
ALLOCATE(ZIN             (SIZE(DMTC%XRUNOFF_TOWN)))
ALLOCATE(ZOUT            (SIZE(DMTC%XRUNOFF_TOWN)))
!
ALLOCATE(ZRUNOFFSOIL_BLD (SIZE(DMTC%XRUNOFF_TOWN)))
ALLOCATE(ZRUNOFFSOIL_RD  (SIZE(DMTC%XRUNOFF_TOWN)))
!
!* Initialisation
ZWG_ROAD_INI(:)   =0.0
ZWG_BLD_INI(:)    =0.0
ZWG_GARDEN_INI(:) =0.0
ZWG_ROAD_FIN(:)   =0.0
ZWG_BLD_FIN(:)    =0.0
ZWG_GARDEN_FIN(:) =0.0
!
ZWGI_ROAD_INI(:)   =0.0
ZWGI_BLD_INI(:)    =0.0
ZWGI_GARDEN_INI(:) =0.0
ZWGI_ROAD_FIN(:)   =0.0
ZWGI_BLD_FIN(:)    =0.0
ZWGI_GARDEN_FIN(:) =0.0
!
ZRUNOFF_TOWN    (:) =0.0
ZRUNOFF_GD      (:) =0.0
ZRUNOFF_RD      (:) =0.0
ZRUNOFF_RF      (:) =0.0
ZRUNOFF_STRF    (:) =0.0
ZRUNOFF_GR      (:) =0.0
ZRUNOFF_SW      (:) =0.0
ZRUNOFF_WW      (:) =0.0
!
ZIN             (:) =0.0
ZOUT            (:) =0.0
!
ZRUNOFFSOIL_BLD (:) =0.0
ZRUNOFFSOIL_RD  (:) =0.0
!
!Transformation de la teneur en eau (m3/m3) en kg/m²
! 
!* Initial water storages in kg/m²
!
ZWG_ROAD_INI(:)   = SUM(DMT%XWG_ROAD_INI(:,:)     * T%XD_ROAD * XRHOLW ,2)
ZWG_BLD_INI(:)    = SUM(DMT%XWG_BLD_INI(:,:)      * T%XD_BLD  * XRHOLW ,2)  
ZWG_GARDEN_INI(:) = SUM(DMT%XWG_GARDEN_INI(:,:)   * P%XDZG    * XRHOLW ,2)   
! 
!* Initial ice storages in kg/m²
!
! Volumic mass of liq. water for ice: WGI is in liq water equivalent
ZWGI_ROAD_INI(:)   = SUM(DMT%XWGI_ROAD_INI(:,:)   * T%XD_ROAD * XRHOLW ,2)  
ZWGI_BLD_INI(:)    = SUM(DMT%XWGI_BLD_INI(:,:)    * T%XD_BLD  * XRHOLW ,2)  
ZWGI_GARDEN_INI(:) = SUM(DMT%XWGI_GARDEN_INI(:,:) * P%XDZG    * XRHOLW ,2)   
!
!* Final water storages in kg/m²
!
ZWG_ROAD_FIN(:)    = SUM(TH%XWG_ROAD(:,:)         * T%XD_ROAD * XRHOLW ,2)
ZWG_BLD_FIN(:)     = SUM(TH%XWG_BLD(:,:)          * T%XD_BLD  * XRHOLW ,2)
ZWG_GARDEN_FIN(:)  = SUM(PEK%XWG(:,:)             * P%XDZG    * XRHOLW ,2) 
!
!* Final ice storages in kg/m²
!
ZWGI_ROAD_FIN(:)   = SUM(TH%XWGI_ROAD(:,:)        * T%XD_ROAD * XRHOLW ,2)
ZWGI_BLD_FIN(:)    = SUM(TH%XWGI_BLD(:,:)         * T%XD_BLD  * XRHOLW ,2)
ZWGI_GARDEN_FIN(:) = SUM(PEK%XWGI(:,:)            * P%XDZG    * XRHOLW ,2) 
!
!* computing evolution of water storage in kg/m²
!
DMT%XDELTA_WGS_ROAD(:)     = (ZWG_ROAD_FIN(:)   - ZWG_ROAD_INI(:))       * T%XROAD(:)  + &   ! groundwater
                             (T%XWS_ROAD(:)     - DMT%XWS_ROAD_INI(:))   * T%XROAD(:)  + &   ! surface wat. res
                             (ZWGI_ROAD_FIN(:)   - ZWGI_ROAD_INI(:))     * T%XROAD(:)        ! ground ice
!                             
DMT%XDELTA_WGS_BLD(:)      = (ZWG_BLD_FIN(:)    - ZWG_BLD_INI(:))        * T%XBLD(:)   + &   ! groundwater 
                             (T%XWS_ROOF(:)     - DMT%XWS_ROOF_INI(:))   * T%XBLD(:)   + &   ! surface 
                             (ZWGI_BLD_FIN(:)   - ZWGI_BLD_INI(:))       * T%XBLD(:)         ! ground ice 


DMT%XDELTA_WGS_GARDEN(:)   = (ZWG_GARDEN_FIN(:) - ZWG_GARDEN_INI(:))     * (T%XGARDEN(:)) + &   ! groundwater
                             (PEK%XWR(:)        - DMT%XWS_GARDEN_INI(:)) * (T%XGARDEN(:)) + &   ! surface
                             (ZWGI_GARDEN_FIN(:)- ZWGI_GARDEN_INI(:))    * (T%XGARDEN(:))       ! ground ice
!
!* computing evolution of snow storage in kg/m²
!
DMT%XDELTA_WSNOW_RD(:)     = (T%TSNOW_ROAD%WSNOW(:,1)      - DMT%XWSNOW_ROAD_INI(:,1))   * T%XROAD(:)   ! surface snow res
DMT%XDELTA_WSNOW_RF(:)     = (T%TSNOW_ROOF%WSNOW(:,1)      - DMT%XWSNOW_ROOF_INI(:,1))   * (1.0-T%XGREENROOF(:))*T%XBLD(:)   ! surface snow res
DMT%XDELTA_WSNOW_GD(:)     = (SUM(PEK%TSNOW%WSNOW (:,:),2) - SUM(DMT%XWSNOW_GARDEN_INI(:,:),2))   * T%XGARDEN(:)    ! surface snow res
!
!***
!
!* computing sources : cumul rain et snow par maille + irrigation
!  Attendtion kg/m²/s différent de mm
ZIN(:) = DMTC%XRAIN(:)+DMTC%XSNOW(:) + & 
         DMTC%XIRRIG_GARDEN(:)*T%XGARDEN(:) +DMTC%XIRRIG_ROAD(:)*T%XROAD(:) + &
         DMTC%XIRRIG_GREENROOF(:)*T%XGREENROOF(:)*T%XBLD(:)  
!                                                                                    
!* computing sinks  
!
DMT%XWAT_OUT(:)    = ((GDDC%XLE(:)-GDDC%XLEI(:))  * T%XGARDEN(:))                     /XLVTT    + &
                     (GDDC%XLEI(:)                * T%XGARDEN(:))                     /XLSTT    + &
                     (DMTC%XLE_HVEG(:)            * T%XFRAC_HVEG(:))                  /XLVTT    + &
                     (DMTC%XLEW_RD(:)             * T%XROAD(:))                       /XLVTT    + &
                     (DMTC%XLESN_RD(:)            * T%XROAD(:))                       /XLSTT    + &
                     (DMTC%XLEW_RF(:)             * (1.-T%XGREENROOF(:)) * T%XBLD(:)) /XLVTT    + &
                     (DMTC%XLESN_RF(:)            * (1.-T%XGREENROOF(:)) * T%XBLD(:)) /XLSTT    + &
                     (DMTC%XRUNOFF_TOWN(:))                                                     + &
                     (DMTC%XDRAIN_ROAD(:)         * T%XROAD(:))                                 + &
                     (DMTC%XDRAIN_BLD(:)          * T%XBLD(:))                                  + &
                     (GDDEC%XDRAIN(:)             * T%XGARDEN(:))                               + &
                     DMT%XDELTA_WGS_ROAD(:) + DMT%XDELTA_WGS_BLD(:)  + DMT%XDELTA_WGS_GARDEN(:) + &
                     DMT%XDELTA_WSNOW_RD(:) + DMT%XDELTA_WSNOW_RF(:) + DMT%XDELTA_WSNOW_GD(:) 
IF (TOP%LGREENROOF) THEN
DMT%XWAT_OUT(:)  = DMT%XWAT_OUT(:)  + &  
                   (GRDC%XLE(:)          *(T%XGREENROOF(:))*T%XBLD(:))           /XLVTT 
ENDIF
!
!* Relative water budget in percentage
!
WHERE (ZIN(:)==0.0)
 DMT%XHYDRO_BUD(:) = 0.0
ELSEWHERE
 DMT%XHYDRO_BUD(:) = (DMT%XWAT_OUT(:)-ZIN(:))!/ZIN(:)*100
ENDWHERE
!
!*       Runoff budget in percentage for town
!        -------------
!
ZRUNOFF_TOWN(:) = DMTC%XRUNOFF_TOWN(:)
ZRUNOFF_GD     = 0.0
ZRUNOFF_RD     = 0.0
ZRUNOFF_RF     = 0.0
ZRUNOFF_STRF   = 0.0
ZRUNOFF_SW     = 0.0
ZRUNOFF_WW     = 0.0
ZRUNOFFSOIL_BLD= 0.0
ZRUNOFFSOIL_RD = 0.0
IF (TOP%LGREENROOF) ZRUNOFF_GR=0.0

WHERE (ZRUNOFF_TOWN==0.0)
    ZRUNOFF_TOWN=XUNDEF
ENDWHERE
!
!
   ZRUNOFF_GD     =(GDDEC%XRUNOFF         * T%XGARDEN)/ZRUNOFF_TOWN*100. ! garden surface RUNOFF
   ZRUNOFF_RD     =(DMTC%XRUNOFF_ROAD     * T%XROAD)  /ZRUNOFF_TOWN*100. ! road surface RUNOFF
   ZRUNOFF_STRF   =(DMTC%XRUNOFF_STRLROOF * T%XBLD)   /ZRUNOFF_TOWN*100. ! roof surface RUNOFF
   ZRUNOFF_SW     =(DMTC%XRUNOFF_SW       * T%XROAD)  /ZRUNOFF_TOWN*100. ! groundwater RUNOFF to stormwater sewers
   ZRUNOFF_WW     =(DMTC%XRUNOFF_WW       * T%XROAD)  /ZRUNOFF_TOWN*100. ! groundwater RUNOFF to waste water sewers
   ZRUNOFFSOIL_BLD=(DMTC%XRUNOFFSOIL_BLD  *T%XBLD)    /ZRUNOFF_TOWN*100. ! lateral RUNOFF in bld soil column
   ZRUNOFFSOIL_RD =(DMTC%XRUNOFFSOIL_ROAD *T%XROAD)   /ZRUNOFF_TOWN*100. ! lateral RUNOFF in road soil column
!***
!
IF (TOP%LGREENROOF) THEN
   ZRUNOFF_STRF=(DMTC%XRUNOFF_STRLROOF * T%XBLD*(1-T%XGREENROOF))/ZRUNOFF_TOWN*100.
   ZRUNOFF_GR  =(GRDEC%XRUNOFF         * T%XBLD*T%XGREENROOF)    /ZRUNOFF_TOWN*100.
ENDIF
!-------------------------------------------------------------------------------
!
!         End of IO
!
!! CALL END_IO_SURF_n(HPROGRAM)
!!IF (LHOOK) CALL DR_HOOK('BUDGET_HYDRO_N',1,ZHOOK_HANDLE)
!
END SUBROUTINE BUDGET_HYDRO_n
