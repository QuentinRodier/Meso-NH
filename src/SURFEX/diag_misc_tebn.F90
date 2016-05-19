!     #########
       SUBROUTINE DIAG_MISC_TEB_n(PTSTEP, PDQS_TOWN,PQF_BLD,PQF_TOWN, PFLX_BLD,             &
                                    PRN_ROAD, PH_ROAD, PLE_ROAD, PGFLUX_ROAD,               &
                                    PRN_WALL_A, PH_WALL_A, PGFLUX_WALL_A,                   &
                                    PRN_WALL_B, PH_WALL_B, PGFLUX_WALL_B,                   &
                                    PRN_ROOF, PH_ROOF, PLE_ROOF, PGFLUX_ROOF,               &
                                    PRUNOFF,                                                &
                                    PRN_STRLROOF, PH_STRLROOF,                              &
                                    PLE_STRLROOF, PGFLUX_STRLROOF,                          &
                                    PRN_GREENROOF, PH_GREENROOF,                            &
                                    PLE_GREENROOF, PGFLUX_GREENROOF, PG_GREENROOF_ROOF,     &
                                    PRUNOFF_GREENROOF, PDRAIN_GREENROOF,                    &
                                    PRN_GARDEN,PH_GARDEN,PLE_GARDEN,PGFLUX_GARDEN,          &
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
                                    PABS_SW_WIN, PABS_LW_WIN                                )  
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
USE MODD_DIAG_MISC_TEB_n,    ONLY : XQF_BLD, XQF_TOWN, XDQS_TOWN, XFLX_BLD,       &
                                    LSURF_MISC_BUDGET,                            &
                                    XRN_ROAD, XH_ROAD, XLE_ROAD,                  &
                                    XGFLUX_ROAD,                                  &
                                    XRN_WALL_A, XH_WALL_A, XGFLUX_WALL_A,         &
                                    XRN_WALL_B, XH_WALL_B, XGFLUX_WALL_B,         &
                                    XRN_ROOF, XH_ROOF, XLE_ROOF,                  &
                                    XGFLUX_ROOF,                                  &
                                    XRN_STRLROOF, XH_STRLROOF,                    &
                                    XLE_STRLROOF, XGFLUX_STRLROOF,                &
                                    XRN_GREENROOF, XH_GREENROOF,                  &
                                    XLE_GREENROOF, XGFLUX_GREENROOF,              &
                                    XG_GREENROOF_ROOF,                            &
                                    XRN_GARDEN,XH_GARDEN,XLE_GARDEN,XGFLUX_GARDEN,&
                                    XRN_BLT,XH_BLT,XLE_BLT,XGFLUX_BLT,            &
                                    XRUNOFF_GREENROOF, XDRAIN_GREENROOF,          &
                                    XABS_SW_ROOF,XABS_LW_ROOF,                    &
                                    XABS_SW_SNOW_ROOF,XABS_LW_SNOW_ROOF,          &
                                    XABS_SW_ROAD,XABS_LW_ROAD,                    &
                                    XABS_SW_SNOW_ROAD,XABS_LW_SNOW_ROAD,          &
                                    XABS_SW_WALL_A,XABS_LW_WALL_A,                &
                                    XABS_SW_WALL_B,XABS_LW_WALL_B,                &
                                    XABS_SW_GARDEN,XABS_LW_GARDEN,                &  
                                    XABS_SW_GREENROOF,XABS_LW_GREENROOF,          &  
                                    XH_BLD_COOL, XT_BLD_COOL,                     &     
                                    XH_BLD_HEAT, XLE_BLD_COOL, XLE_BLD_HEAT,      &
                                    XH_WASTE, XLE_WASTE, XHVAC_COOL,              &
                                    XHVAC_HEAT, XCAP_SYS, XM_SYS, XCOP,           &
                                    XQ_SYS, XT_SYS, XTR_SW_WIN, XFAN_POWER,       &
                                    XABS_SW_WIN, XABS_LW_WIN
!
USE MODD_TEB_n,              ONLY : CBEM 
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
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
       REAL, DIMENSION(:), INTENT(IN) :: PRUNOFF           ! runoff for town    
       REAL, DIMENSION(:), INTENT(IN) :: PRN_STRLROOF      ! net radiation for structural roofs
       REAL, DIMENSION(:), INTENT(IN) :: PH_STRLROOF       ! sensible heat flux for structural roofs
       REAL, DIMENSION(:), INTENT(IN) :: PLE_STRLROOF      ! latent heat flux for structural roofs
       REAL, DIMENSION(:), INTENT(IN) :: PGFLUX_STRLROOF   ! storage flux for structural roofs       
       REAL, DIMENSION(:), INTENT(IN) :: PRN_GREENROOF     ! net radiation for green roofs
       REAL, DIMENSION(:), INTENT(IN) :: PH_GREENROOF      ! sensible heat flux for green roofs
       REAL, DIMENSION(:), INTENT(IN) :: PLE_GREENROOF     ! latent heat flux for green roofs
       REAL, DIMENSION(:), INTENT(IN) :: PGFLUX_GREENROOF  ! storage flux for green roofs
       REAL, DIMENSION(:), INTENT(IN) :: PG_GREENROOF_ROOF ! heat flux between green/structural roofs
       REAL, DIMENSION(:), INTENT(IN) :: PRUNOFF_GREENROOF ! runoff for green roofs       
       REAL, DIMENSION(:), INTENT(IN) :: PDRAIN_GREENROOF  ! total vertical drainage for green roofs       
       REAL, DIMENSION(:), INTENT(IN) :: PRN_GARDEN        ! net radiation for GARDEN areas
       REAL, DIMENSION(:), INTENT(IN) :: PH_GARDEN         ! sensible heat flux for GARDEN areas
       REAL, DIMENSION(:), INTENT(IN) :: PLE_GARDEN        ! latent heat flux for GARDEN areas
       REAL, DIMENSION(:), INTENT(IN) :: PGFLUX_GARDEN     ! storage flux for GARDEN areas
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
!
!
!*      0.2    declarations of local variables
!
       REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('DIAG_MISC_TEB_N',0,ZHOOK_HANDLE)
IF (LSURF_MISC_BUDGET) THEN
   XQF_BLD            =  PQF_BLD
   XFLX_BLD           =  PFLX_BLD
   XQF_TOWN           =  PQF_TOWN
   XDQS_TOWN          =  PDQS_TOWN
   XRN_ROAD           = PRN_ROAD
   XH_ROAD            = PH_ROAD
   XLE_ROAD           = PLE_ROAD
   XGFLUX_ROAD        = PGFLUX_ROAD
   XRN_WALL_A         = PRN_WALL_A
   XH_WALL_A          = PH_WALL_A
   XGFLUX_WALL_A      = PGFLUX_WALL_A
   XRN_WALL_B         = PRN_WALL_B
   XH_WALL_B          = PH_WALL_B
   XGFLUX_WALL_B      = PGFLUX_WALL_B
   XRN_ROOF           = PRN_ROOF
   XH_ROOF            = PH_ROOF
   XLE_ROOF           = PLE_ROOF
   XGFLUX_ROOF        = PGFLUX_ROOF   
   XRN_STRLROOF       = PRN_STRLROOF
   XH_STRLROOF        = PH_STRLROOF
   XLE_STRLROOF       = PLE_STRLROOF
   XGFLUX_STRLROOF    = PGFLUX_STRLROOF
   XRN_GREENROOF      = PRN_GREENROOF
   XH_GREENROOF       = PH_GREENROOF
   XLE_GREENROOF      = PLE_GREENROOF
   XGFLUX_GREENROOF   = PGFLUX_GREENROOF
   XG_GREENROOF_ROOF  = PG_GREENROOF_ROOF
   XRUNOFF_GREENROOF  = PRUNOFF_GREENROOF
   XDRAIN_GREENROOF   = PDRAIN_GREENROOF
   XRN_GARDEN         = PRN_GARDEN
   XH_GARDEN          = PH_GARDEN
   XLE_GARDEN         = PLE_GARDEN
   XGFLUX_GARDEN      = PGFLUX_GARDEN  
   XRN_BLT            = PRN_BLT  
   XH_BLT             = PH_BLT  
   XLE_BLT            = PLE_BLT  
   XGFLUX_BLT         = PGFLUX_BLT    
!
   XABS_SW_ROOF       = PABS_SW_ROOF
   XABS_LW_ROOF       = PABS_LW_ROOF
   XABS_SW_SNOW_ROOF  = PABS_SW_SNOW_ROOF
   XABS_LW_SNOW_ROOF  = PABS_LW_SNOW_ROOF
   XABS_SW_ROAD       = PABS_SW_ROAD
   XABS_LW_ROAD       = PABS_LW_ROAD
   XABS_SW_SNOW_ROAD  = PABS_SW_SNOW_ROAD
   XABS_LW_SNOW_ROAD  = PABS_LW_SNOW_ROAD
   XABS_SW_WALL_A     = PABS_SW_WALL_A
   XABS_LW_WALL_A     = PABS_LW_WALL_A
   XABS_SW_WALL_B     = PABS_SW_WALL_B
   XABS_LW_WALL_B     = PABS_LW_WALL_B
   XABS_SW_GARDEN     = PABS_SW_GARDEN
   XABS_LW_GARDEN     = PABS_LW_GARDEN
   XABS_SW_GREENROOF  = PABS_SW_GREENROOF
   XABS_LW_GREENROOF  = PABS_LW_GREENROOF
   !
   IF (CBEM=='BEM') THEN
     XH_BLD_COOL = PH_BLD_COOL 
     XT_BLD_COOL = PT_BLD_COOL  
     XH_BLD_HEAT = PH_BLD_HEAT  
     XLE_BLD_COOL= PLE_BLD_COOL  
     XLE_BLD_HEAT= PLE_BLD_HEAT 
     XH_WASTE    = PH_WASTE      
     XLE_WASTE   = PLE_WASTE     
     XHVAC_COOL  = PHVAC_COOL    
     XHVAC_HEAT  = PHVAC_HEAT     
     XCAP_SYS    = PCAP_SYS        
     XM_SYS      = PM_SYS         
     XCOP        = PCOP          
     XQ_SYS      = PQ_SYS     
     XT_SYS      = PT_SYS  
     XTR_SW_WIN  = PTR_SW_WIN
     XFAN_POWER  = PFAN_POWER 
     !
     XABS_SW_WIN = PABS_SW_WIN 
     XABS_LW_WIN = PABS_LW_WIN
   ENDIF
   !
END IF
!
IF (LHOOK) CALL DR_HOOK('DIAG_MISC_TEB_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE DIAG_MISC_TEB_n
