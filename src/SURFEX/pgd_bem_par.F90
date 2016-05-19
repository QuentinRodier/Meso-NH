!     ##########################################
      SUBROUTINE PGD_BEM_PAR(HPROGRAM,OAUTOSIZE)
!     ##########################################
!
!!**** *PGD_BEM_PAR* monitor for averaging and interpolations of BEM input data
!!
!!    PURPOSE
!!    -------
!!
!!    METHOD
!!    ------
!!   
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
!!    G. Pigeon        Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    08/2011
!!    G. Pigeon   09/2012, NPAR_FLOOR_LAYER default to 1
!!
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_SURF_PAR,   ONLY : XUNDEF
USE MODD_TEB_GRID_n, ONLY : NDIM
USE MODD_DATA_BEM_n,  ONLY : NPAR_FLOOR_LAYER_n => NPAR_FLOOR_LAYER,          &
                              XPAR_TCOOL_TARGET, XPAR_THEAT_TARGET,           &
                              XPAR_HC_FLOOR, XPAR_TC_FLOOR, XPAR_D_FLOOR,     & 
                              XPAR_F_WASTE_CAN, XPAR_SHADE, XPAR_NATVENT,     &
                              XPAR_EFF_HEAT, XPAR_QIN, XPAR_QIN_FRAD,         &
                              XPAR_SHGC, XPAR_U_WIN, XPAR_GR,                 &
                              XPAR_FLOOR_HEIGHT, XPAR_INF, XPAR_QIN_FLAT,     &
                              XPAR_HR_TARGET, XPAR_V_VENT, XPAR_CAP_SYS_HEAT, &
                              XPAR_CAP_SYS_RAT, XPAR_T_ADP, XPAR_M_SYS_RAT,   &
                              XPAR_COP_RAT, XPAR_T_SIZE_MAX, XPAR_T_SIZE_MIN, &
                              XPAR_F_WATER_COND, XPAR_SHGC_SH,                &
                              LDATA_HC_FLOOR, LDATA_TC_FLOOR, LDATA_D_FLOOR,  &
                              LDATA_TCOOL_TARGET, LDATA_THEAT_TARGET,         &
                              LDATA_F_WASTE_CAN, LDATA_SHADE, LDATA_NATVENT,  &
                              LDATA_EFF_HEAT, LDATA_QIN, LDATA_QIN_FRAD,      &
                              LDATA_SHGC, LDATA_U_WIN, LDATA_GR,              &
                              LDATA_FLOOR_HEIGHT, LDATA_INF, LDATA_QIN_FLAT,     &
                              LDATA_HR_TARGET, LDATA_V_VENT, LDATA_CAP_SYS_HEAT, &
                              LDATA_CAP_SYS_RAT, LDATA_T_ADP, LDATA_M_SYS_RAT,   &
                              LDATA_COP_RAT, LDATA_T_SIZE_MAX, LDATA_T_SIZE_MIN, &
                              LDATA_F_WATER_COND, LDATA_SHGC_SH
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_GET_LUOUT
USE MODI_OPEN_NAMELIST
USE MODI_CLOSE_NAMELIST
USE MODI_TEST_NAM_VAR_SURF
USE MODI_INI_VAR_FROM_DATA_0D
USE MODI_INI_VAR_FROM_DATA
USE MODI_ABOR1_SFX
!
USE MODE_POS_SURF
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
 CHARACTER(LEN=6), INTENT(IN) :: HPROGRAM     ! Type of program
LOGICAL,          INTENT(IN) :: OAUTOSIZE    ! T for automatic determination
!                                            ! of HVAC systems charcateristics
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER               :: ILUOUT    ! output listing logical unit
INTEGER               :: ILUNAM    ! namelist file  logical unit
LOGICAL               :: GFOUND    ! true if namelist is found
INTEGER               :: JLAYER    ! loop counter on layers
!
!
!*    0.3    Declaration of namelists
!            ------------------------
!
! bem options
!
INTEGER, PARAMETER                      :: NFLOOR_MAX  = 9
INTEGER                                 :: NPAR_FLOOR_LAYER ! number of floor layers
!
REAL                                    :: XUNIF_SHADE    ! 
 CHARACTER(LEN=28)                       :: CFNAM_SHADE    ! 
 CHARACTER(LEN=6)                        :: CFTYP_SHADE    ! 
REAL                                    :: XUNIF_NATVENT    ! 
 CHARACTER(LEN=28)                       :: CFNAM_NATVENT    ! 
 CHARACTER(LEN=6)                        :: CFTYP_NATVENT    ! 
!
! Floor parameters
!
REAL, DIMENSION(NFLOOR_MAX)             :: XUNIF_HC_FLOOR     ! floor layers heat capacity (J/K/m3)
REAL, DIMENSION(NFLOOR_MAX)             :: XUNIF_TC_FLOOR     ! floor layers thermal conduc (W/K/m)
REAL, DIMENSION(NFLOOR_MAX)             :: XUNIF_D_FLOOR      ! depth of floor layers      (m)
REAL                                    :: XUNIF_FLOOR_HEIGHT ! building floor height [m]
 CHARACTER(LEN=28), DIMENSION(NFLOOR_MAX):: CFNAM_HC_FLOOR     ! file name for HC_FLOOR   
 CHARACTER(LEN=28), DIMENSION(NFLOOR_MAX):: CFNAM_TC_FLOOR     ! file name for TC_FLOOR
 CHARACTER(LEN=28), DIMENSION(NFLOOR_MAX):: CFNAM_D_FLOOR      ! file name for D_FLOOR
 CHARACTER(LEN=28)                       :: CFNAM_FLOOR_HEIGHT ! file name for FLOOR_HEIGHT
 CHARACTER(LEN=6),  DIMENSION(NFLOOR_MAX):: CFTYP_HC_FLOOR     ! file type for HC_FLOOR   
 CHARACTER(LEN=6),  DIMENSION(NFLOOR_MAX):: CFTYP_TC_FLOOR     ! file type for TC_FLOOR
 CHARACTER(LEN=6),  DIMENSION(NFLOOR_MAX):: CFTYP_D_FLOOR      ! file type for D_FLOOR
 CHARACTER(LEN=6)                        :: CFTYP_FLOOR_HEIGHT ! file type for FLOOR_HEIGHT
!
! AC systems parameters
!
REAL                                    :: XUNIF_TCOOL_TARGET  !cooling setpoint
REAL                                    :: XUNIF_THEAT_TARGET  !heating setpoint
REAL                                    :: XUNIF_F_WASTE_CAN  ! fraction of waste heat into the canyon
REAL                                    :: XUNIF_EFF_HEAT     ! efficiency of the heating system
REAL                                    :: XUNIF_HR_TARGET    ! Relative humidity setpoint
REAL                                    :: XUNIF_CAP_SYS_HEAT ! Capacity of the heating system 
REAL                                    :: XUNIF_CAP_SYS_RAT  ! Rated capacity of the cooling system
REAL                                    :: XUNIF_T_ADP        ! Apparatus dewpoint temperature of the
REAL                                    :: XUNIF_M_SYS_RAT    ! Rated HVAC mass flow rate 
REAL                                    :: XUNIF_COP_RAT      ! Rated COP of the cooling system
REAL                                    :: XUNIF_F_WATER_COND ! fraction of evaporation of condensers
 CHARACTER(LEN=28)                       :: CFNAM_TCOOL_TARGET ! file name for TCOOL_TARGET
 CHARACTER(LEN=28)                       :: CFNAM_THEAT_TARGET ! file name for THEAT_TARGET
 CHARACTER(LEN=28)                       :: CFNAM_F_WASTE_CAN  ! file name for F_WASTE_CAN
 CHARACTER(LEN=28)                       :: CFNAM_EFF_HEAT     ! file name for EFF_HEAT
 CHARACTER(LEN=28)                       :: CFNAM_HR_TARGET    ! Relative humidity setpoint
 CHARACTER(LEN=28)                       :: CFNAM_CAP_SYS_HEAT ! Capacity of the heating system 
 CHARACTER(LEN=28)                       :: CFNAM_CAP_SYS_RAT  ! Rated capacity of the cooling system
 CHARACTER(LEN=28)                       :: CFNAM_T_ADP        ! Apparatus dewpoint temperature of the
 CHARACTER(LEN=28)                       :: CFNAM_M_SYS_RAT    ! Rated HVAC mass flow rate 
 CHARACTER(LEN=28)                       :: CFNAM_COP_RAT      ! Rated COP of the cooling system
 CHARACTER(LEN=28)                       :: CFNAM_F_WATER_COND ! fraction of evaporation of condensers
 CHARACTER(LEN=6)                        :: CFTYP_TCOOL_TARGET ! file type for TCOOL_TARGET
 CHARACTER(LEN=6)                        :: CFTYP_THEAT_TARGET ! file type for THEAT_TARGET
 CHARACTER(LEN=6)                        :: CFTYP_F_WASTE_CAN  ! file type for F_WASTE_CAN
 CHARACTER(LEN=6)                        :: CFTYP_EFF_HEAT     ! file type for EFF_HEAT
 CHARACTER(LEN=6)                        :: CFTYP_HR_TARGET    ! Relative humidity setpoint
 CHARACTER(LEN=6)                        :: CFTYP_CAP_SYS_HEAT ! Capacity of the heating system 
 CHARACTER(LEN=6)                        :: CFTYP_CAP_SYS_RAT  ! Rated capacity of the cooling system
 CHARACTER(LEN=6)                        :: CFTYP_T_ADP        ! Apparatus dewpoint temperature of the
 CHARACTER(LEN=6)                        :: CFTYP_M_SYS_RAT    ! Rated HVAC mass flow rate 
 CHARACTER(LEN=6)                        :: CFTYP_COP_RAT      ! Rated COP of the cooling system
 CHARACTER(LEN=6)                        :: CFTYP_F_WATER_COND ! fraction of evaporation of condensers
!
! Internal heat gains
REAL                                    :: XUNIF_QIN          ! internal heat gains [W m-2(floor)]
REAL                                    :: XUNIF_QIN_FRAD     ! radiant fraction of int heat gains
REAL                                    :: XUNIF_QIN_FLAT     ! Latent franction of internal heat gains
 CHARACTER(LEN=28)                       :: CFNAM_QIN          ! file name for QIN
 CHARACTER(LEN=28)                       :: CFNAM_QIN_FRAD     ! file name for QIN_FRAD
 CHARACTER(LEN=28)                       :: CFNAM_QIN_FLAT     ! Latent franction of internal heat gains
 CHARACTER(LEN=6)                        :: CFTYP_QIN          ! file type for QIN
 CHARACTER(LEN=6)                        :: CFTYP_QIN_FRAD     ! file type for QIN_FRAD
 CHARACTER(LEN=6)                        :: CFTYP_QIN_FLAT     ! Latent franction of internal heat gains
!
! window parameters
REAL                                    :: XUNIF_GR           ! glazing ratio
REAL                                    :: XUNIF_SHGC         ! solar transmitance of windows
REAL                                    :: XUNIF_SHGC_SH      ! solar transmitance of windows + shading
REAL                                    :: XUNIF_U_WIN        ! glazing thermal resistance[K m W-2]
 CHARACTER(LEN=28)                       :: CFNAM_GR           ! file name for GR
 CHARACTER(LEN=28)                       :: CFNAM_SHGC         ! file name for SHGC
 CHARACTER(LEN=28)                       :: CFNAM_SHGC_SH      ! file name for SHGC_SH
 CHARACTER(LEN=28)                       :: CFNAM_U_WIN        ! file name for U_WIN
 CHARACTER(LEN=6)                        :: CFTYP_GR           ! file type for GR
 CHARACTER(LEN=6)                        :: CFTYP_SHGC         ! file type for SHGC
 CHARACTER(LEN=6)                        :: CFTYP_SHGC_SH      ! file type for SHGC
 CHARACTER(LEN=6)                        :: CFTYP_U_WIN        ! file type for U_WIN
!
! air renewal
REAL                                    :: XUNIF_INF          ! infiltration/ventilation flow rate [AC/H]
REAL                                    :: XUNIF_V_VENT       ! Ventilation flow rate [AC/H]
 CHARACTER(LEN=28)                       :: CFNAM_INF          ! file name for INF
 CHARACTER(LEN=28)                       :: CFNAM_V_VENT       ! Ventilation flow rate [AC/H]
 CHARACTER(LEN=6)                        :: CFTYP_INF          ! file type for INF
 CHARACTER(LEN=6)                        :: CFTYP_V_VENT       ! Ventilation flow rate [AC/H]
!
! parameters for autosize calculation of the AC systems
REAL                                    :: XUNIF_T_SIZE_MAX   !
REAL                                    :: XUNIF_T_SIZE_MIN    !
 CHARACTER(LEN=28)                       :: CFNAM_T_SIZE_MAX    ! 
 CHARACTER(LEN=28)                       :: CFNAM_T_SIZE_MIN    ! 
 CHARACTER(LEN=6)                        :: CFTYP_T_SIZE_MAX    !
 CHARACTER(LEN=6)                        :: CFTYP_T_SIZE_MIN    !
!
REAL, DIMENSION(NDIM) :: ZWORK
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
NAMELIST/NAM_DATA_BEM/ NPAR_FLOOR_LAYER,                          &
                  XUNIF_HC_FLOOR, XUNIF_TC_FLOOR, XUNIF_D_FLOOR,  &
                  XUNIF_FLOOR_HEIGHT,                             &
                  XUNIF_TCOOL_TARGET, XUNIF_THEAT_TARGET,         &
                  XUNIF_F_WASTE_CAN, XUNIF_EFF_HEAT,              &
                  XUNIF_F_WATER_COND, XUNIF_HR_TARGET,            &
                  XUNIF_QIN, XUNIF_QIN_FRAD, XUNIF_QIN_FLAT,      &   
                  XUNIF_SHGC, XUNIF_U_WIN, XUNIF_GR,XUNIF_SHGC_SH,&
                  XUNIF_INF, XUNIF_V_VENT,                        &
                  XUNIF_CAP_SYS_HEAT,                             &
                  XUNIF_CAP_SYS_RAT, XUNIF_T_ADP, XUNIF_M_SYS_RAT,&
                  XUNIF_COP_RAT, XUNIF_T_SIZE_MAX,                &
                  XUNIF_T_SIZE_MIN,                               &
                  XUNIF_SHADE, CFNAM_SHADE, CFTYP_SHADE,          &
                  XUNIF_NATVENT, CFNAM_NATVENT, CFTYP_NATVENT,    &
                  CFNAM_HC_FLOOR, CFNAM_TC_FLOOR, CFNAM_D_FLOOR,  &
                  CFNAM_FLOOR_HEIGHT,                             &
                  CFNAM_TCOOL_TARGET, CFNAM_THEAT_TARGET,         &
                  CFNAM_F_WASTE_CAN, CFNAM_EFF_HEAT,              &
                  CFNAM_F_WATER_COND, CFNAM_HR_TARGET,            &
                  CFNAM_QIN, CFNAM_QIN_FRAD, CFNAM_QIN_FLAT,      &   
                  CFNAM_SHGC, CFNAM_U_WIN, CFNAM_GR,              &
                  CFNAM_SHGC_SH, CFNAM_INF, CFNAM_V_VENT,         &
                  CFNAM_CAP_SYS_HEAT,                             &
                  CFNAM_CAP_SYS_RAT, CFNAM_T_ADP, CFNAM_M_SYS_RAT,&
                  CFNAM_COP_RAT, CFNAM_T_SIZE_MAX,                &
                  CFNAM_T_SIZE_MIN,                               &
                  CFTYP_HC_FLOOR, CFTYP_TC_FLOOR, CFTYP_D_FLOOR,  &
                  CFTYP_FLOOR_HEIGHT,                             &
                  CFTYP_TCOOL_TARGET, CFTYP_THEAT_TARGET,         &
                  CFTYP_F_WASTE_CAN, CFTYP_EFF_HEAT,              &
                  CFTYP_F_WATER_COND, CFTYP_HR_TARGET,            &
                  CFTYP_QIN, CFTYP_QIN_FRAD, CFTYP_QIN_FLAT,      &   
                  CFTYP_SHGC, CFTYP_U_WIN, CFTYP_GR,              &
                  CFTYP_SHGC_SH, CFTYP_INF, CFTYP_V_VENT,         &
                  CFTYP_CAP_SYS_HEAT,                             &
                  CFTYP_CAP_SYS_RAT, CFTYP_T_ADP, CFTYP_M_SYS_RAT,&
                  CFTYP_COP_RAT, CFTYP_T_SIZE_MAX,                &
                  CFTYP_T_SIZE_MIN
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('PGD_BEM_PAR',0,ZHOOK_HANDLE)
!
!*    1.      Initializations
!             ---------------
!
NPAR_FLOOR_LAYER = 1
XUNIF_SHADE   = XUNDEF
XUNIF_NATVENT = XUNDEF
XUNIF_HC_FLOOR     = XUNDEF
XUNIF_TC_FLOOR     = XUNDEF
XUNIF_D_FLOOR      = XUNDEF
XUNIF_TCOOL_TARGET = XUNDEF
XUNIF_THEAT_TARGET = XUNDEF
XUNIF_F_WASTE_CAN  = XUNDEF
XUNIF_EFF_HEAT     = XUNDEF
XUNIF_QIN          = XUNDEF
XUNIF_QIN_FRAD     = XUNDEF
XUNIF_SHGC         = XUNDEF
XUNIF_U_WIN        = XUNDEF
XUNIF_GR           = XUNDEF
XUNIF_SHGC_SH      = XUNDEF
XUNIF_FLOOR_HEIGHT = XUNDEF
XUNIF_INF          = XUNDEF
XUNIF_F_WATER_COND = XUNDEF
XUNIF_QIN_FLAT     = XUNDEF
XUNIF_HR_TARGET    = XUNDEF
XUNIF_V_VENT       = XUNDEF
XUNIF_CAP_SYS_HEAT = XUNDEF
XUNIF_CAP_SYS_RAT  = XUNDEF
XUNIF_T_ADP        = XUNDEF
XUNIF_M_SYS_RAT    = XUNDEF
XUNIF_COP_RAT      = XUNDEF
XUNIF_T_SIZE_MAX   = XUNDEF
XUNIF_T_SIZE_MIN   = XUNDEF
!
CFNAM_SHADE        = '                            '
CFNAM_NATVENT      = '                            '
CFNAM_HC_FLOOR (:) = '                            '
CFNAM_TC_FLOOR (:) = '                            '
CFNAM_D_FLOOR  (:) = '                            '
CFNAM_TCOOL_TARGET = '                            '
CFNAM_THEAT_TARGET = '                            '
CFNAM_F_WASTE_CAN  = '                            '
CFNAM_EFF_HEAT     = '                            '
CFNAM_QIN          = '                            '
CFNAM_QIN_FRAD     = '                            '
CFNAM_SHGC         = '                            '
CFNAM_U_WIN        = '                            '
CFNAM_GR           = '                            '
CFNAM_SHGC_SH      = '                            '
CFNAM_FLOOR_HEIGHT = '                            '
CFNAM_INF          = '                            '
CFNAM_F_WATER_COND = '                            '
CFNAM_QIN_FLAT     = '                            '
CFNAM_HR_TARGET    = '                            '
CFNAM_V_VENT       = '                            '
CFNAM_CAP_SYS_HEAT = '                            '
CFNAM_CAP_SYS_RAT  = '                            '
CFNAM_T_ADP        = '                            '
CFNAM_M_SYS_RAT    = '                            '
CFNAM_COP_RAT      = '                            '
CFNAM_T_SIZE_MAX   = '                            '
CFNAM_T_SIZE_MIN   = '                            '
!
CFTYP_SHADE        = '      '
CFTYP_NATVENT      = '      '
CFTYP_HC_FLOOR(:)  = '      '
CFTYP_TC_FLOOR(:)  = '      '
CFTYP_D_FLOOR(:)   = '      '
CFTYP_TCOOL_TARGET = '      '
CFTYP_THEAT_TARGET = '      '
CFTYP_F_WASTE_CAN  = '      '
CFTYP_EFF_HEAT     = '      '
CFTYP_QIN          = '      '
CFTYP_QIN_FRAD     = '      '
CFTYP_SHGC         = '      '
CFTYP_U_WIN        = '      '
CFTYP_GR           = '      '
CFTYP_SHGC_SH      = '      '
CFTYP_FLOOR_HEIGHT = '      '
CFTYP_INF          = '      '
CFTYP_F_WATER_COND = '      '
CFTYP_QIN_FLAT     = '      '
CFTYP_HR_TARGET    = '      '
CFTYP_V_VENT       = '      '
CFTYP_CAP_SYS_HEAT = '      '
CFTYP_CAP_SYS_RAT  = '      '
CFTYP_T_ADP        = '      '
CFTYP_M_SYS_RAT    = '      '
CFTYP_COP_RAT      = '      '
CFTYP_T_SIZE_MAX   = '      '
CFTYP_T_SIZE_MIN   = '      '
!
!-------------------------------------------------------------------------------
!
!*    2.      Input file for cover types
!             --------------------------
!
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
 CALL OPEN_NAMELIST(HPROGRAM,ILUNAM)
!
 CALL POSNAM(ILUNAM,'NAM_DATA_BEM',GFOUND,ILUOUT)
IF (GFOUND) READ(UNIT=ILUNAM,NML=NAM_DATA_BEM)
!
 CALL CLOSE_NAMELIST(HPROGRAM,ILUNAM)
!
!-------------------------------------------------------------------------------
!
NPAR_FLOOR_LAYER_n = NPAR_FLOOR_LAYER
!
!-------------------------------------------------------------------------------
!
!* coherence check
!
IF ((     ANY(XUNIF_HC_FLOOR/=XUNDEF) .OR. ANY(LEN_TRIM(CFNAM_HC_FLOOR)>0) &
     .OR. ANY(XUNIF_TC_FLOOR/=XUNDEF) .OR. ANY(LEN_TRIM(CFNAM_TC_FLOOR)>0) &
     .OR. ANY(XUNIF_D_FLOOR /=XUNDEF) .OR. ANY(LEN_TRIM(CFNAM_D_FLOOR )>0) &
    ) .AND. NPAR_FLOOR_LAYER<1                                  ) THEN
  CALL ABOR1_SFX('In order to initialize FLOOR thermal quantities, please specify NPAR_FLOOR_LAYER in namelist NAM_DATA_BEM')
END IF
!
!-------------------------------------------------------------------------------
!
ALLOCATE(XPAR_HC_FLOOR    (NDIM,NPAR_FLOOR_LAYER))
ALLOCATE(XPAR_TC_FLOOR    (NDIM,NPAR_FLOOR_LAYER))
ALLOCATE(XPAR_D_FLOOR     (NDIM,NPAR_FLOOR_LAYER))
!
ALLOCATE(XPAR_TCOOL_TARGET  (NDIM))
ALLOCATE(XPAR_THEAT_TARGET  (NDIM))
ALLOCATE(XPAR_F_WASTE_CAN   (NDIM))
ALLOCATE(XPAR_EFF_HEAT      (NDIM))
ALLOCATE(XPAR_QIN           (NDIM))
ALLOCATE(XPAR_QIN_FRAD      (NDIM))
ALLOCATE(XPAR_SHGC          (NDIM))
ALLOCATE(XPAR_U_WIN         (NDIM))
ALLOCATE(XPAR_GR            (NDIM))
ALLOCATE(XPAR_SHGC_SH       (NDIM))
ALLOCATE(XPAR_FLOOR_HEIGHT  (NDIM))
ALLOCATE(XPAR_INF           (NDIM))
!
ALLOCATE(XPAR_F_WATER_COND (NDIM))
ALLOCATE(XPAR_QIN_FLAT     (NDIM))
ALLOCATE(XPAR_HR_TARGET    (NDIM))
ALLOCATE(XPAR_V_VENT       (NDIM))
ALLOCATE(XPAR_CAP_SYS_HEAT (NDIM))
ALLOCATE(XPAR_CAP_SYS_RAT  (NDIM))
ALLOCATE(XPAR_T_ADP        (NDIM))
ALLOCATE(XPAR_M_SYS_RAT    (NDIM))
ALLOCATE(XPAR_COP_RAT      (NDIM))
ALLOCATE(XPAR_T_SIZE_MAX   (NDIM))
ALLOCATE(XPAR_T_SIZE_MIN   (NDIM))
!
ALLOCATE(XPAR_SHADE        (NDIM))
ALLOCATE(XPAR_NATVENT      (NDIM))
!
!-------------------------------------------------------------------------------
IF (NFLOOR_MAX < NPAR_FLOOR_LAYER) THEN
  WRITE(ILUOUT,*) '---------------------------------------------'
  WRITE(ILUOUT,*) 'Please update pgd_bem_par.F90 routine :      '
  WRITE(ILUOUT,*) 'The maximum number of FLOOR LAYER             '
  WRITE(ILUOUT,*) 'in the declaration of the namelist variables '
  WRITE(ILUOUT,*) 'must be increased to : ', NPAR_FLOOR_LAYER
  WRITE(ILUOUT,*) '---------------------------------------------'
  CALL ABOR1_SFX('PGD_BEM_PAR: MAXIMUM NUMBER OF NPAR_FLOOR_LAYER MUST BE INCREASED')
END IF
!-------------------------------------------------------------------------------
!
!*    3.      user defined fields are prescribed
!             ----------------------------------
!
!
 CALL INI_VAR_FROM_DATA_0D(HPROGRAM,'MAJ','SHADE      ','TWN', CFNAM_SHADE, CFTYP_SHADE, XUNIF_SHADE, &
        XPAR_SHADE, LDATA_SHADE )
IF (.NOT.LDATA_SHADE) DEALLOCATE(XPAR_SHADE)
!
 CALL INI_VAR_FROM_DATA_0D(HPROGRAM,'MAJ','NATVENT   ','TWN', CFNAM_NATVENT, CFTYP_NATVENT, XUNIF_NATVENT, &
        XPAR_NATVENT, LDATA_NATVENT )
IF (.NOT.LDATA_NATVENT) DEALLOCATE(XPAR_NATVENT)
!
 CALL INI_VAR_FROM_DATA(HPROGRAM,'INV','HC_FLOOR  ','TWN',CFNAM_HC_FLOOR,CFTYP_HC_FLOOR, &
        XUNIF_HC_FLOOR,XPAR_HC_FLOOR,LDATA_HC_FLOOR )
IF (.NOT.LDATA_HC_FLOOR) DEALLOCATE(XPAR_HC_FLOOR)
!
 CALL INI_VAR_FROM_DATA(HPROGRAM,'ARI','TC_FLOOR  ','TWN',CFNAM_TC_FLOOR,CFTYP_TC_FLOOR, &
                 XUNIF_TC_FLOOR ,XPAR_TC_FLOOR, LDATA_TC_FLOOR )
IF (.NOT.LDATA_TC_FLOOR) DEALLOCATE(XPAR_TC_FLOOR)
!
 CALL INI_VAR_FROM_DATA(HPROGRAM,'ARI','D_FLOOR   ','TWN',CFNAM_D_FLOOR,CFTYP_D_FLOOR, &
                 XUNIF_D_FLOOR  ,XPAR_D_FLOOR , LDATA_D_FLOOR )
IF (.NOT.LDATA_D_FLOOR) DEALLOCATE(XPAR_D_FLOOR)
!
 CALL INI_VAR_FROM_DATA_0D(HPROGRAM,'ARI','TCOOL_TARGET','TWN',CFNAM_TCOOL_TARGET, CFTYP_TCOOL_TARGET, XUNIF_TCOOL_TARGET, &
        XPAR_TCOOL_TARGET, LDATA_TCOOL_TARGET)
IF (.NOT.LDATA_TCOOL_TARGET) DEALLOCATE(XPAR_TCOOL_TARGET)
!
 CALL INI_VAR_FROM_DATA_0D(HPROGRAM,'ARI','THEAT_TARGET','TWN',CFNAM_THEAT_TARGET, CFTYP_THEAT_TARGET, XUNIF_THEAT_TARGET, &
        XPAR_THEAT_TARGET, LDATA_THEAT_TARGET)
IF (.NOT.LDATA_THEAT_TARGET) DEALLOCATE(XPAR_THEAT_TARGET)
!
 CALL INI_VAR_FROM_DATA_0D(HPROGRAM,'ARI','F_WASTE_CAN','TWN',CFNAM_F_WASTE_CAN, CFTYP_F_WASTE_CAN, XUNIF_F_WASTE_CAN, &
        XPAR_F_WASTE_CAN, LDATA_F_WASTE_CAN)
IF (.NOT.LDATA_F_WASTE_CAN) DEALLOCATE(XPAR_F_WASTE_CAN)
!
 CALL INI_VAR_FROM_DATA_0D(HPROGRAM,'ARI','EFF_HEAT','TWN',CFNAM_EFF_HEAT, CFTYP_EFF_HEAT, XUNIF_EFF_HEAT, &
        XPAR_EFF_HEAT, LDATA_EFF_HEAT)
IF (.NOT.LDATA_EFF_HEAT) DEALLOCATE(XPAR_EFF_HEAT)
!
 CALL INI_VAR_FROM_DATA_0D(HPROGRAM,'ARI','QIN','TWN',CFNAM_QIN, CFTYP_QIN, XUNIF_QIN, XPAR_QIN, LDATA_QIN)
IF (.NOT.LDATA_QIN) DEALLOCATE(XPAR_QIN)
!
 CALL INI_VAR_FROM_DATA_0D(HPROGRAM,'ARI','QIN_FRAD','TWN',CFNAM_QIN_FRAD, CFTYP_QIN_FRAD, XUNIF_QIN_FRAD, &
        XPAR_QIN_FRAD, LDATA_QIN_FRAD)
IF (.NOT.LDATA_QIN_FRAD) DEALLOCATE(XPAR_QIN_FRAD)
!
 CALL INI_VAR_FROM_DATA_0D(HPROGRAM,'ARI','SHGC','TWN',CFNAM_SHGC, CFTYP_SHGC, XUNIF_SHGC, XPAR_SHGC, LDATA_SHGC)
IF (.NOT.LDATA_SHGC) DEALLOCATE(XPAR_SHGC)
!
 CALL INI_VAR_FROM_DATA_0D(HPROGRAM,'ARI','U_WIN','TWN',CFNAM_U_WIN, CFTYP_U_WIN, XUNIF_U_WIN, XPAR_U_WIN, LDATA_U_WIN)
IF (.NOT.LDATA_U_WIN) DEALLOCATE(XPAR_U_WIN)
!
 CALL INI_VAR_FROM_DATA_0D(HPROGRAM,'ARI','GR','TWN',CFNAM_GR, CFTYP_GR, XUNIF_GR, XPAR_GR, LDATA_GR)
IF (.NOT.LDATA_GR) DEALLOCATE(XPAR_GR)
!
 CALL INI_VAR_FROM_DATA_0D(HPROGRAM,'ARI','SHGC_SH','TWN',CFNAM_SHGC_SH, CFTYP_SHGC_SH, XUNIF_SHGC_SH, &
        XPAR_SHGC_SH, LDATA_SHGC_SH)
IF (.NOT.LDATA_SHGC_SH) DEALLOCATE(XPAR_SHGC_SH)
!
 CALL INI_VAR_FROM_DATA_0D(HPROGRAM,'ARI','FLOOR_HEIGHT','TWN',CFNAM_FLOOR_HEIGHT, CFTYP_FLOOR_HEIGHT, XUNIF_FLOOR_HEIGHT, &
        XPAR_FLOOR_HEIGHT, LDATA_FLOOR_HEIGHT)
IF (.NOT.LDATA_FLOOR_HEIGHT) DEALLOCATE(XPAR_FLOOR_HEIGHT)
!
 CALL INI_VAR_FROM_DATA_0D(HPROGRAM,'ARI','INF','TWN',CFNAM_INF, CFTYP_INF, XUNIF_INF, XPAR_INF, LDATA_INF)
IF (.NOT.LDATA_INF) DEALLOCATE(XPAR_INF)
!
 CALL INI_VAR_FROM_DATA_0D(HPROGRAM,'ARI','F_WATER_COND','TWN',CFNAM_F_WATER_COND, CFTYP_F_WATER_COND, XUNIF_F_WATER_COND, &
        XPAR_F_WATER_COND, LDATA_F_WATER_COND)
IF (.NOT.LDATA_F_WATER_COND) DEALLOCATE(XPAR_F_WATER_COND)
!
 CALL INI_VAR_FROM_DATA_0D(HPROGRAM,'ARI','QIN_FLAT','TWN',CFNAM_QIN_FLAT, CFTYP_QIN_FLAT, XUNIF_QIN_FLAT, &
        XPAR_QIN_FLAT, LDATA_QIN_FLAT)
IF (.NOT.LDATA_QIN_FLAT) DEALLOCATE(XPAR_QIN_FLAT)
!
 CALL INI_VAR_FROM_DATA_0D(HPROGRAM,'ARI','HR_TARGET','TWN',CFNAM_HR_TARGET, CFTYP_HR_TARGET, XUNIF_HR_TARGET, &
        XPAR_HR_TARGET, LDATA_HR_TARGET)
IF (.NOT.LDATA_HR_TARGET) DEALLOCATE(XPAR_HR_TARGET)
!
 CALL INI_VAR_FROM_DATA_0D(HPROGRAM,'ARI','V_VENT','TWN',CFNAM_V_VENT, CFTYP_V_VENT, XUNIF_V_VENT, XPAR_V_VENT, LDATA_V_VENT)
IF (.NOT.LDATA_V_VENT) DEALLOCATE(XPAR_V_VENT)
!
!
 CALL INI_VAR_FROM_DATA_0D(HPROGRAM,'ARI','T_SIZE_MAX','TWN',CFNAM_T_SIZE_MAX, CFTYP_T_SIZE_MAX, XUNIF_T_SIZE_MAX, &
        XPAR_T_SIZE_MAX, LDATA_T_SIZE_MAX)
IF (.NOT.LDATA_T_SIZE_MAX) DEALLOCATE(XPAR_T_SIZE_MAX)
!
 CALL INI_VAR_FROM_DATA_0D(HPROGRAM,'ARI','T_SIZE_MIN','TWN',CFNAM_T_SIZE_MIN, CFTYP_T_SIZE_MIN, XUNIF_T_SIZE_MIN, &
        XPAR_T_SIZE_MIN, LDATA_T_SIZE_MIN)
IF (.NOT.LDATA_T_SIZE_MIN) DEALLOCATE(XPAR_T_SIZE_MIN)
!
 CALL INI_VAR_FROM_DATA_0D(HPROGRAM,'ARI','CAP_SYS_HEAT','TWN',CFNAM_CAP_SYS_HEAT, CFTYP_CAP_SYS_HEAT, XUNIF_CAP_SYS_HEAT, &
        XPAR_CAP_SYS_HEAT, LDATA_CAP_SYS_HEAT)
IF (.NOT.LDATA_CAP_SYS_HEAT) DEALLOCATE(XPAR_CAP_SYS_HEAT)
!
 CALL INI_VAR_FROM_DATA_0D(HPROGRAM,'ARI','CAP_SYS_RAT','TWN',CFNAM_CAP_SYS_RAT, CFTYP_CAP_SYS_RAT, XUNIF_CAP_SYS_RAT, &
        XPAR_CAP_SYS_RAT, LDATA_CAP_SYS_RAT)
IF (.NOT.LDATA_CAP_SYS_RAT) DEALLOCATE(XPAR_CAP_SYS_RAT)
!
 CALL INI_VAR_FROM_DATA_0D(HPROGRAM,'ARI','M_SYS_RAT','TWN',CFNAM_M_SYS_RAT, CFTYP_M_SYS_RAT, XUNIF_M_SYS_RAT, &
        XPAR_M_SYS_RAT, LDATA_M_SYS_RAT)
IF (.NOT.LDATA_M_SYS_RAT) DEALLOCATE(XPAR_M_SYS_RAT)
  !
IF (OAUTOSIZE) THEN
  IF (LDATA_CAP_SYS_HEAT .OR. LDATA_CAP_SYS_RAT .OR. LDATA_M_SYS_RAT) THEN
    WRITE(ILUOUT,*) '==> You choose LAUTOSIZE=T <=='
    WRITE(ILUOUT,*) 'Therefore HVAC systems characteristics will be computed automatically'
    IF (LDATA_CAP_SYS_HEAT) THEN
      WRITE(ILUOUT,*) 'Data you provided for CAP_SYS_HEAT are then discarded.'
      DEALLOCATE(XPAR_CAP_SYS_HEAT)
    END IF
    IF (LDATA_CAP_SYS_RAT ) THEN
      WRITE(ILUOUT,*) 'Data you provided for CAP_SYS_RAT  are then discarded.'
      DEALLOCATE(XPAR_CAP_SYS_RAT)
    END IF
    IF (LDATA_M_SYS_RAT   ) THEN
      WRITE(ILUOUT,*) 'Data you provided for M_SYS_RAT    are then discarded.'
      DEALLOCATE(XPAR_M_SYS_RAT)
    END IF
  END IF
  LDATA_CAP_SYS_HEAT = .FALSE.
  LDATA_CAP_SYS_RAT  = .FALSE.
  LDATA_M_SYS_RAT    = .FALSE.
ELSE
  IF (LDATA_T_SIZE_MAX .OR. LDATA_T_SIZE_MAX) THEN
    WRITE(ILUOUT,*) '==> You choose LAUTOSIZE=F <=='
    WRITE(ILUOUT,*) 'Therefore HVAC systems characteristics are specified'
    WRITE(ILUOUT,*) 'and you do not need the minimal and maximum temperatures'
    WRITE(ILUOUT,*) 'that would be used if you have chosen an automatic calibration.'
    IF (LDATA_T_SIZE_MAX) THEN
      WRITE(ILUOUT,*) 'Data you provided for T_SIZE_MAX are then discarded.'
      DEALLOCATE(XPAR_T_SIZE_MAX)
    END IF
    IF (LDATA_T_SIZE_MIN) THEN
      WRITE(ILUOUT,*) 'Data you provided for T_SIZE_MIN are then discarded.'
      DEALLOCATE(XPAR_T_SIZE_MIN)
    END IF
  END IF
  LDATA_T_SIZE_MAX = .FALSE.
  LDATA_T_SIZE_MIN = .FALSE.
END IF
!
 CALL INI_VAR_FROM_DATA_0D(HPROGRAM,'ARI','T_ADP','TWN',CFNAM_T_ADP, CFTYP_T_ADP, XUNIF_T_ADP, XPAR_T_ADP, LDATA_T_ADP)
IF (.NOT.LDATA_T_ADP) DEALLOCATE(XPAR_T_ADP)
!
 CALL INI_VAR_FROM_DATA_0D(HPROGRAM,'ARI','COP_RAT','TWN',CFNAM_COP_RAT, CFTYP_COP_RAT, XUNIF_COP_RAT, &
        XPAR_COP_RAT, LDATA_COP_RAT)
IF (.NOT.LDATA_COP_RAT) DEALLOCATE(XPAR_COP_RAT)

!-------------------------------------------------------------------------------
!
!* coherence checks
!
 CALL COHERENCE_THERMAL_DATA_FL('FLOOR',LDATA_HC_FLOOR,LDATA_TC_FLOOR,LDATA_D_FLOOR)
!
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('PGD_BEM_PAR',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
CONTAINS
SUBROUTINE COHERENCE_THERMAL_DATA_FL(HTYPE,ODATA_HC,ODATA_TC,ODATA_D)
 CHARACTER(LEN=5), INTENT(IN) :: HTYPE
LOGICAL,          INTENT(IN) :: ODATA_HC
LOGICAL,          INTENT(IN) :: ODATA_TC
LOGICAL,          INTENT(IN) :: ODATA_D
!
IF (ODATA_HC .OR. ODATA_TC .OR. ODATA_D) THEN
  IF (.NOT. (ODATA_HC .AND. ODATA_TC .AND. ODATA_D)) THEN
    WRITE(ILUOUT,*) '*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*'
    WRITE(ILUOUT,*) 'When specifying data for thermal ',TRIM(HTYPE),' characteristics,'
    WRITE(ILUOUT,*) 'All three parameters MUST be defined:'
    WRITE(ILUOUT,*) 'Heat capacity, Thermal conductivity and depths of layers'
    WRITE(ILUOUT,*) ' '
    WRITE(ILUOUT,*) 'In your case :'
    IF (ODATA_HC) THEN
      WRITE(ILUOUT,*) 'Heat capacity is defined'
    ELSE
      WRITE(ILUOUT,*) 'Heat capacity is NOT defined'
    END IF
    IF (ODATA_TC) THEN
      WRITE(ILUOUT,*) 'Thermal conductivity is defined'
    ELSE
      WRITE(ILUOUT,*) 'Thermal conductivity is NOT defined'
    END IF
    IF (ODATA_D) THEN
      WRITE(ILUOUT,*) 'Depths of layers are defined'
    ELSE
      WRITE(ILUOUT,*) 'Depths of layers are NOT defined'
    END IF
    WRITE(ILUOUT,*) '*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*'
    CALL ABOR1_SFX('Heat capacity, Thermal conductivity and depths of layers MUST all be defined for '//HTYPE)
  END IF
END IF
END SUBROUTINE COHERENCE_THERMAL_DATA_FL
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE PGD_BEM_PAR
