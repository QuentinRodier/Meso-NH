!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
       SUBROUTINE DIAG_MISC_TEB_n (DGCT, DGMT, DGMTO, TOP, &
                                   PTSTEP, PDQS_TOWN,PQF_BLD,PQF_TOWN, PFLX_BLD,             &
                                    PRUNOFF_TOWN,                                           &
                                    PRN_ROAD, PH_ROAD, PLE_ROAD, PGFLUX_ROAD,               &
                                    PRUNOFF_ROAD, PIRRIG_ROAD,                              &
                                    PRN_WALL_A, PH_WALL_A, PGFLUX_WALL_A,                   &
                                    PRN_WALL_B, PH_WALL_B, PGFLUX_WALL_B,                   &
                                    PRN_ROOF, PH_ROOF, PLE_ROOF, PGFLUX_ROOF,               &
                                    PRUNOFF_ROOF,                                           &
                                    PRN_STRLROOF, PH_STRLROOF,                              &
                                    PLE_STRLROOF, PGFLUX_STRLROOF,                          &
                                    PRUNOFF_STRLROOF,                                       &
                                    PRN_GREENROOF, PH_GREENROOF,                            &
                                    PLE_GREENROOF, PGFLUX_GREENROOF, PG_GREENROOF_ROOF,     &
                                    PRUNOFF_GREENROOF, PDRAIN_GREENROOF, PIRRIG_GREENROOF,  &
                                    PRN_GARDEN,PH_GARDEN,PLE_GARDEN,PGFLUX_GARDEN,          &
                                    PRUNOFF_GARDEN, PDRAIN_GARDEN, PIRRIG_GARDEN,           &
                                    PRN_BLT,PH_BLT,PLE_BLT,PGFLUX_BLT,                      &
                                    PABS_SW_ROOF,PABS_LW_ROOF,                              &
                                    PABS_SW_SNOW_ROOF,PABS_LW_SNOW_ROOF,                    &
                                    PABS_SW_ROAD,PABS_LW_ROAD,                              &
                                    PABS_SW_SNOW_ROAD,PABS_LW_SNOW_ROAD,                    &
                                    PABS_SW_WALL_A, PABS_LW_WALL_A,                         &
                                    PABS_SW_WALL_B, PABS_LW_WALL_B,                         &
                                    PABS_SW_GARDEN,PABS_LW_GARDEN,                          &  
                                    PABS_SW_GREENROOF,PABS_LW_GREENROOF,                    &  
                                    PH_BLD_COOL, PT_BLD_COOL,                               &     
                                    PH_BLD_HEAT, PLE_BLD_COOL, PLE_BLD_HEAT,                &
                                    PH_WASTE, PLE_WASTE, PHVAC_COOL,                        &
                                    PHVAC_HEAT, PCAP_SYS, PM_SYS, PCOP,                     &
                                    PQ_SYS, PT_SYS, PTR_SW_WIN, PFAN_POWER,                 &
                                    PABS_SW_WIN, PABS_LW_WIN,                               &
                                    PTCOOL_TARGET, PTHEAT_TARGET, PQIN,                     &
                                    PABS_SW_PANEL, PABS_LW_PANEL, PRN_PANEL,                &
                                    PH_PANEL, PTHER_PROD_PANEL, PPHOT_PROD_PANEL,           &
                                    PPROD_PANEL, PTHER_PROD_BLD, PPHOT_PROD_BLD             )  
!     ###############################################################################
!
!!****  *DIAG_MISC-TEB_n * - additional diagnostics for TEB
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
!!     P. Le Moigne 
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    10/2005
!!------------------------------------------------------------------
!
!
!
!
!
!
USE MODD_DIAG_CUMUL_TEB_n, ONLY : DIAG_CUMUL_TEB_t
USE MODD_DIAG_MISC_TEB_n, ONLY : DIAG_MISC_TEB_t
USE MODD_DIAG_MISC_TEB_OPTION_n, ONLY : DIAG_MISC_TEB_OPTIONS_t
USE MODD_TEB_OPTION_n, ONLY : TEB_OPTIONS_t
!
USE MODI_CUMUL_DIAG_TEB_n
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
TYPE(DIAG_CUMUL_TEB_t), INTENT(INOUT) :: DGCT
TYPE(DIAG_MISC_TEB_t), INTENT(INOUT) :: DGMT
TYPE(DIAG_MISC_TEB_OPTIONS_t), INTENT(INOUT) :: DGMTO
TYPE(TEB_OPTIONS_t), INTENT(INOUT) :: TOP
!
       REAL,               INTENT(IN) :: PTSTEP            ! time step
       REAL, DIMENSION(:), INTENT(IN) :: PQF_BLD           ! domestic heating
       REAL, DIMENSION(:), INTENT(IN) :: PFLX_BLD          ! heat flux from bld
       REAL, DIMENSION(:), INTENT(IN) :: PQF_TOWN          ! total anthropogenic heat
       REAL, DIMENSION(:), INTENT(IN) :: PDQS_TOWN         ! storage inside town mat.
       REAL, DIMENSION(:), INTENT(IN) :: PRN_ROAD          ! net radiation for roads
       REAL, DIMENSION(:), INTENT(IN) :: PH_ROAD           ! sensible heat flux for roads
       REAL, DIMENSION(:), INTENT(IN) :: PLE_ROAD          ! latent heat flux for roads
       REAL, DIMENSION(:), INTENT(IN) :: PGFLUX_ROAD       ! storage flux for roads
       REAL, DIMENSION(:), INTENT(IN) :: PRUNOFF_ROAD      ! runoff for roads       
       REAL, DIMENSION(:), INTENT(IN) :: PIRRIG_ROAD       ! water supply for watering of roads       
       REAL, DIMENSION(:), INTENT(IN) :: PRN_WALL_A        ! net radiation for wall
       REAL, DIMENSION(:), INTENT(IN) :: PH_WALL_A         ! sensible heat flux for walls
       REAL, DIMENSION(:), INTENT(IN) :: PGFLUX_WALL_A     ! storage flux for walls
       REAL, DIMENSION(:), INTENT(IN) :: PRN_WALL_B        ! net radiation for wall
       REAL, DIMENSION(:), INTENT(IN) :: PH_WALL_B         ! sensible heat flux for walls
       REAL, DIMENSION(:), INTENT(IN) :: PGFLUX_WALL_B     ! storage flux for walls
       REAL, DIMENSION(:), INTENT(IN) :: PRN_ROOF          ! net radiation for roofs
       REAL, DIMENSION(:), INTENT(IN) :: PH_ROOF           ! sensible heat flux for roofs
       REAL, DIMENSION(:), INTENT(IN) :: PLE_ROOF          ! latent heat flux for roofs
       REAL, DIMENSION(:), INTENT(IN) :: PGFLUX_ROOF       ! storage flux for roofs       
       REAL, DIMENSION(:), INTENT(IN) :: PRUNOFF_ROOF      ! aggregated runoff for roof
       REAL, DIMENSION(:), INTENT(IN) :: PRUNOFF_TOWN      ! aggregated runoff for town
       REAL, DIMENSION(:), INTENT(IN) :: PRN_STRLROOF      ! net radiation for structural roofs
       REAL, DIMENSION(:), INTENT(IN) :: PH_STRLROOF       ! sensible heat flux for structural roofs
       REAL, DIMENSION(:), INTENT(IN) :: PLE_STRLROOF      ! latent heat flux for structural roofs
       REAL, DIMENSION(:), INTENT(IN) :: PGFLUX_STRLROOF   ! storage flux for structural roofs       
       REAL, DIMENSION(:), INTENT(IN) :: PRUNOFF_STRLROOF  ! runoff for structural roofs       
       REAL, DIMENSION(:), INTENT(IN) :: PRN_GREENROOF     ! net radiation for green roofs
       REAL, DIMENSION(:), INTENT(IN) :: PH_GREENROOF      ! sensible heat flux for green roofs
       REAL, DIMENSION(:), INTENT(IN) :: PLE_GREENROOF     ! latent heat flux for green roofs
       REAL, DIMENSION(:), INTENT(IN) :: PGFLUX_GREENROOF  ! storage flux for green roofs
       REAL, DIMENSION(:), INTENT(IN) :: PG_GREENROOF_ROOF ! heat flux between green/structural roofs
       REAL, DIMENSION(:), INTENT(IN) :: PRUNOFF_GREENROOF ! runoff for green roofs       
       REAL, DIMENSION(:), INTENT(IN) :: PDRAIN_GREENROOF  ! total vertical drainage for green roofs       
       REAL, DIMENSION(:), INTENT(IN) :: PIRRIG_GREENROOF  ! water supply from green roof ground irrigation
       REAL, DIMENSION(:), INTENT(IN) :: PRN_GARDEN        ! net radiation for GARDEN areas
       REAL, DIMENSION(:), INTENT(IN) :: PH_GARDEN         ! sensible heat flux for GARDEN areas
       REAL, DIMENSION(:), INTENT(IN) :: PLE_GARDEN        ! latent heat flux for GARDEN areas
       REAL, DIMENSION(:), INTENT(IN) :: PGFLUX_GARDEN     ! storage flux for GARDEN areas
       REAL, DIMENSION(:), INTENT(IN) :: PRUNOFF_GARDEN    ! runoff for green areas
       REAL, DIMENSION(:), INTENT(IN) :: PDRAIN_GARDEN     ! drainage for green areas
       REAL, DIMENSION(:), INTENT(IN) :: PIRRIG_GARDEN     ! water supply for irrigation for green areas
       REAL, DIMENSION(:), INTENT(IN) :: PRN_BLT           ! net radiation for built surf
       REAL, DIMENSION(:), INTENT(IN) :: PH_BLT            ! sensible heat flux for built surf
       REAL, DIMENSION(:), INTENT(IN) :: PLE_BLT           ! latent heat flux for built surf
       REAL, DIMENSION(:), INTENT(IN) :: PGFLUX_BLT        ! storage flux for built surf
!
       REAL, DIMENSION(:), INTENT(IN) :: PABS_SW_ROOF      ! Sdown absorbed by roofs
       REAL, DIMENSION(:), INTENT(IN) :: PABS_SW_SNOW_ROOF ! Sdown absorbed by snow on roofs
       REAL, DIMENSION(:), INTENT(IN) :: PABS_LW_ROOF      ! Ldown absorbed by roofs
       REAL, DIMENSION(:), INTENT(IN) :: PABS_LW_SNOW_ROOF ! Ldown absorbed by snow on roofs
       REAL, DIMENSION(:), INTENT(IN) :: PABS_SW_ROAD      ! Sdown absorbed by roads
       REAL, DIMENSION(:), INTENT(IN) :: PABS_SW_SNOW_ROAD ! Sdown absorbed by snow on roads
       REAL, DIMENSION(:), INTENT(IN) :: PABS_LW_ROAD      ! Ldown absorbed by roads
       REAL, DIMENSION(:), INTENT(IN) :: PABS_LW_SNOW_ROAD ! Ldown absorbed by snow on roads
       REAL, DIMENSION(:), INTENT(IN) :: PABS_SW_WALL_A    ! Sdown absorbed by walls
       REAL, DIMENSION(:), INTENT(IN) :: PABS_LW_WALL_A    ! Ldown absorbed by walls
       REAL, DIMENSION(:), INTENT(IN) :: PABS_SW_WALL_B    ! Sdown absorbed by walls
       REAL, DIMENSION(:), INTENT(IN) :: PABS_LW_WALL_B    ! Ldown absorbed by walls
       REAL, DIMENSION(:), INTENT(IN) :: PABS_SW_GARDEN    ! Sdown absorbed by GARDEN areas
       REAL, DIMENSION(:), INTENT(IN) :: PABS_LW_GARDEN    ! Ldown absorbed by GARDEN areas
       REAL, DIMENSION(:), INTENT(IN) :: PABS_SW_GREENROOF ! Sdown absorbed by green roofs
       REAL, DIMENSION(:), INTENT(IN) :: PABS_LW_GREENROOF ! Ldown absorbed by green roofs
!  new arguments after BEM
       REAL, DIMENSION(:), INTENT(IN) :: PH_BLD_COOL       ! Sensible cooling energy demand  
                                                           ! of the building [W m-2(bld)]
       REAL, DIMENSION(:), INTENT(IN) :: PT_BLD_COOL       ! Total cooling energy demand  
                                                           ! of the building [W m-2(bld)]
       REAL, DIMENSION(:), INTENT(IN) :: PH_BLD_HEAT       ! Heating energy demand       
                                                           ! of the building [W m-2(bld)]
       REAL, DIMENSION(:), INTENT(IN) :: PLE_BLD_COOL      ! Latent cooling energy demand 
                                                           ! of the building [W m-2(bld)]
       REAL, DIMENSION(:), INTENT(IN) :: PLE_BLD_HEAT      ! Latent heating energy demand 
                                                           ! of the building [W m-2(bld)]
       REAL, DIMENSION(:), INTENT(IN) :: PH_WASTE          ! Sensible waste heat from HVAC system
                                                           ! [W m-2(bld)]
       REAL, DIMENSION(:), INTENT(IN) :: PLE_WASTE         ! Latent waste heat from HVAC system
                                                           ! [W m-2(bld)]
       REAL, DIMENSION(:), INTENT(IN) :: PHVAC_COOL        ! Energy consumption of the cooling system
                                                           ! [W m-2(bld)]
       REAL, DIMENSION(:), INTENT(IN) :: PHVAC_HEAT        ! Energy consumption of the heating system
                                                           ! [W m-2(bld)]
       REAL, DIMENSION(:), INTENT(IN) :: PCAP_SYS          ! Actual capacity of the cooling system
                                                           ! [W m-2(bld)] 
       REAL, DIMENSION(:), INTENT(IN) :: PM_SYS            ! Actual HVAC mass flow rate 
                                                           ! [kg s-1 m-2(bld)]
       REAL, DIMENSION(:), INTENT(IN) :: PCOP              ! COP of the cooling system
       REAL, DIMENSION(:), INTENT(IN) :: PQ_SYS            ! Supply air specific humidity [kg kg-1]
       REAL, DIMENSION(:), INTENT(IN) :: PT_SYS            ! Supply air temperature [K]
       REAL, DIMENSION(:), INTENT(IN) :: PTR_SW_WIN        ! Solar radiation transmitted throught
                                                           ! windows [W m-2(bld)]
       REAL, DIMENSION(:), INTENT(IN) :: PFAN_POWER        ! HVAC fan power
       REAL, DIMENSION(:), INTENT(IN) :: PABS_SW_WIN       ! window absorbed shortwave radiation [W m-2] 
       REAL, DIMENSION(:), INTENT(IN) :: PABS_LW_WIN       ! absorbed infrared rad. [W m-2]
       !
       REAL, DIMENSION(:), INTENT(IN) :: PTCOOL_TARGET     ! Cooling system set point modulated by bld_occ_calendar [K]
       REAL, DIMENSION(:), INTENT(IN) :: PTHEAT_TARGET     ! Heating system set point modulated by bld_occ_calendar [K] 
       REAL, DIMENSION(:), INTENT(IN) :: PQIN              ! Building interal heat load modulated by bld_occ_calendar [W m-2(floor)]
!
       REAL, DIMENSION(:), INTENT(IN) :: PABS_SW_PANEL     ! absorbed solar    energy by solar panels [W m-2(panel)]
       REAL, DIMENSION(:), INTENT(IN) :: PABS_LW_PANEL     ! absorbed longwave energy by solar panels [W m-2(panel)]
       REAL, DIMENSION(:), INTENT(IN) :: PRN_PANEL         ! net radiation            of solar panels [W m-2(panel)]
       REAL, DIMENSION(:), INTENT(IN) :: PH_PANEL          ! sensible heat flux       of solar panels [W m-2(panel)]
       REAL, DIMENSION(:), INTENT(IN) :: PTHER_PROD_PANEL  ! thermal      production  of solar panels [W m-2(panel)]
       REAL, DIMENSION(:), INTENT(IN) :: PPHOT_PROD_PANEL  ! photovoltaic production  of solar panels [W m-2(panel)]
       REAL, DIMENSION(:), INTENT(IN) :: PPROD_PANEL       ! averaged     production  of solar panels [W m-2(panel)]
       REAL, DIMENSION(:), INTENT(IN) :: PTHER_PROD_BLD    ! thermal      production  of solar panels [W m-2(bld)]
       REAL, DIMENSION(:), INTENT(IN) :: PPHOT_PROD_BLD    ! photovoltaic production  of solar panels [W m-2(bld)]
!
!*      0.2    declarations of local variables
!
       REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('DIAG_MISC_TEB_N',0,ZHOOK_HANDLE)
IF (DGMTO%LSURF_MISC_BUDGET) THEN
   DGMT%CUR%XQF_BLD            =  PQF_BLD
   DGMT%CUR%XFLX_BLD           =  PFLX_BLD
   DGMT%CUR%XQF_TOWN           =  PQF_TOWN
   DGMT%CUR%XDQS_TOWN          =  PDQS_TOWN
   DGMT%CUR%XRN_ROAD           = PRN_ROAD
   DGMT%CUR%XH_ROAD            = PH_ROAD
   DGMT%CUR%XLE_ROAD           = PLE_ROAD
   DGMT%CUR%XGFLUX_ROAD        = PGFLUX_ROAD
   DGMT%CUR%XRN_WALL_A         = PRN_WALL_A
   DGMT%CUR%XH_WALL_A          = PH_WALL_A
   DGMT%CUR%XGFLUX_WALL_A      = PGFLUX_WALL_A
   DGMT%CUR%XRN_WALL_B         = PRN_WALL_B
   DGMT%CUR%XH_WALL_B          = PH_WALL_B
   DGMT%CUR%XGFLUX_WALL_B      = PGFLUX_WALL_B
   DGMT%CUR%XRN_ROOF           = PRN_ROOF
   DGMT%CUR%XH_ROOF            = PH_ROOF
   DGMT%CUR%XLE_ROOF           = PLE_ROOF
   DGMT%CUR%XGFLUX_ROOF        = PGFLUX_ROOF   
   DGMT%CUR%XRN_STRLROOF       = PRN_STRLROOF
   DGMT%CUR%XH_STRLROOF        = PH_STRLROOF
   DGMT%CUR%XLE_STRLROOF       = PLE_STRLROOF
   DGMT%CUR%XGFLUX_STRLROOF    = PGFLUX_STRLROOF
   DGMT%CUR%XRN_GREENROOF      = PRN_GREENROOF
   DGMT%CUR%XH_GREENROOF       = PH_GREENROOF
   DGMT%CUR%XLE_GREENROOF      = PLE_GREENROOF
   DGMT%CUR%XGFLUX_GREENROOF   = PGFLUX_GREENROOF
   DGMT%CUR%XG_GREENROOF_ROOF  = PG_GREENROOF_ROOF
   DGMT%CUR%XRUNOFF_TOWN       = PRUNOFF_TOWN
   DGMT%CUR%XRUNOFF_GARDEN     = PRUNOFF_GARDEN
   DGMT%CUR%XRUNOFF_ROAD       = PRUNOFF_ROAD
   DGMT%CUR%XRUNOFF_ROOF       = PRUNOFF_ROOF
   DGMT%CUR%XRUNOFF_STRLROOF   = PRUNOFF_STRLROOF
   DGMT%CUR%XRUNOFF_GREENROOF  = PRUNOFF_GREENROOF
   DGMT%CUR%XDRAIN_GARDEN      = PDRAIN_GARDEN
   DGMT%CUR%XDRAIN_GREENROOF   = PDRAIN_GREENROOF
   DGMT%CUR%XIRRIG_ROAD        = PIRRIG_ROAD
   DGMT%CUR%XIRRIG_GARDEN      = PIRRIG_GARDEN
   DGMT%CUR%XIRRIG_GREENROOF   = PIRRIG_GREENROOF
   DGMT%CUR%XRN_GARDEN         = PRN_GARDEN
   DGMT%CUR%XH_GARDEN          = PH_GARDEN
   DGMT%CUR%XLE_GARDEN         = PLE_GARDEN
   DGMT%CUR%XGFLUX_GARDEN      = PGFLUX_GARDEN  
   DGMT%CUR%XRN_BLT            = PRN_BLT  
   DGMT%CUR%XH_BLT             = PH_BLT  
   DGMT%CUR%XLE_BLT            = PLE_BLT  
   DGMT%CUR%XGFLUX_BLT         = PGFLUX_BLT    
!
   DGMT%CUR%XABS_SW_ROOF       = PABS_SW_ROOF
   DGMT%CUR%XABS_LW_ROOF       = PABS_LW_ROOF
   DGMT%CUR%XABS_SW_SNOW_ROOF  = PABS_SW_SNOW_ROOF
   DGMT%CUR%XABS_LW_SNOW_ROOF  = PABS_LW_SNOW_ROOF
   DGMT%CUR%XABS_SW_ROAD       = PABS_SW_ROAD
   DGMT%CUR%XABS_LW_ROAD       = PABS_LW_ROAD
   DGMT%CUR%XABS_SW_SNOW_ROAD  = PABS_SW_SNOW_ROAD
   DGMT%CUR%XABS_LW_SNOW_ROAD  = PABS_LW_SNOW_ROAD
   DGMT%CUR%XABS_SW_WALL_A     = PABS_SW_WALL_A
   DGMT%CUR%XABS_LW_WALL_A     = PABS_LW_WALL_A
   DGMT%CUR%XABS_SW_WALL_B     = PABS_SW_WALL_B
   DGMT%CUR%XABS_LW_WALL_B     = PABS_LW_WALL_B
   DGMT%CUR%XABS_SW_GARDEN     = PABS_SW_GARDEN
   DGMT%CUR%XABS_LW_GARDEN     = PABS_LW_GARDEN
   DGMT%CUR%XABS_SW_GREENROOF  = PABS_SW_GREENROOF
   DGMT%CUR%XABS_LW_GREENROOF  = PABS_LW_GREENROOF
   !
   IF (TOP%CBEM=='BEM') THEN
     DGMT%CUR%XH_BLD_COOL = PH_BLD_COOL 
     DGMT%CUR%XT_BLD_COOL = PT_BLD_COOL  
     DGMT%CUR%XH_BLD_HEAT = PH_BLD_HEAT  
     DGMT%CUR%XLE_BLD_COOL= PLE_BLD_COOL  
     DGMT%CUR%XLE_BLD_HEAT= PLE_BLD_HEAT 
     DGMT%CUR%XH_WASTE    = PH_WASTE      
     DGMT%CUR%XLE_WASTE   = PLE_WASTE     
     DGMT%CUR%XHVAC_COOL  = PHVAC_COOL    
     DGMT%CUR%XHVAC_HEAT  = PHVAC_HEAT     
     DGMT%CUR%XCAP_SYS    = PCAP_SYS        
     DGMT%CUR%XM_SYS      = PM_SYS         
     DGMT%CUR%XCOP        = PCOP          
     DGMT%CUR%XQ_SYS      = PQ_SYS     
     DGMT%CUR%XT_SYS      = PT_SYS  
     DGMT%CUR%XTR_SW_WIN  = PTR_SW_WIN
     DGMT%CUR%XFAN_POWER  = PFAN_POWER 
     !
     DGMT%CUR%XABS_SW_WIN = PABS_SW_WIN 
     DGMT%CUR%XABS_LW_WIN = PABS_LW_WIN
     !
     DGMT%CUR%XTCOOL_CUR_TARGET  = PTCOOL_TARGET    
     DGMT%CUR%XTHEAT_CUR_TARGET  = PTHEAT_TARGET    
     DGMT%CUR%XCUR_QIN           = PQIN    
   ENDIF
   !
   IF (TOP%LSOLAR_PANEL) THEN
     DGMT%CUR%XABS_SW_PANEL    = PABS_SW_PANEL
     DGMT%CUR%XABS_LW_PANEL    = PABS_LW_PANEL
     DGMT%CUR%XRN_PANEL        = PRN_PANEL
     DGMT%CUR%XH_PANEL         = PH_PANEL
     DGMT%CUR%XTHER_PROD_PANEL = PTHER_PROD_PANEL
     DGMT%CUR%XPHOT_PROD_PANEL = PPHOT_PROD_PANEL
     DGMT%CUR%XPROD_PANEL      = PPROD_PANEL
     DGMT%CUR%XTHER_PROD_BLD   = PTHER_PROD_BLD
     DGMT%CUR%XPHOT_PROD_BLD   = PPHOT_PROD_BLD
   END IF
   !
   ! cumulated diagnostics 
   ! ---------------------
   !
   CALL CUMUL_DIAG_TEB_n(DGCT, DGMT, TOP, &
                         PTSTEP)
   !
END IF
!
IF (LHOOK) CALL DR_HOOK('DIAG_MISC_TEB_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE DIAG_MISC_TEB_n
