!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
    SUBROUTINE SEAICE_GELATO1D_n (S, &
                                  HPROGRAM,PTIMEC, PTSTEP,TPGLT, PSST, PSSS, &
         PFSIC, PFSIT, PSIC, PTICE, PICE_ALB)
!     #######################################################################
!
!!****  *SEAICE_GELATO1D_n*  
!!
!!    PURPOSE
!!    -------
!     Run Gelato Sea-ice model, which provides sea-ice cover, temperature
!     and albedo
!     
!!**  METHOD
!!    ------
!     At relevant time steps :
!     i) Feed Gelato input interface structures with relevant variables 
!        taken in XCPL_xxx fields from MODD_SEAFLUX_n
!     ii) call the minimal Gelato sequence , namely : 
!        a) ingest and transform inputs regarding atm and sea state, 
!        b) run Gelato , 
!        c) have it process its ouptuts for atm and oce, 
!     iii) feed output arguments using content of Gelato output interface 
!        structure
!
!     Note : for the time being, SST and SSS are not averaged over coupling 
!     period
!
!!    EXTERNAL :
!!    ---------
!!    - a number of glt<xxx> routines
!!
!!    IMPLICIT ARGUMENTS :  
!!    ------------------- 
!!    Variables XCPL_xxx in MODD_SEAFLUX_n, and also LINTERPOL_SIC and LINTERPOL_SIT
!!      
!!    REFERENCE : 
!!    ---------
!!    Salas y Melia D (2002) A global coupled sea ice-ocean model. 
!!    Ocean Model 4:137-172
!!
!!    AUTHOR
!!    ------
!!     S.Senesi  *Meteo-France* CNRM - GAME
!!
!!    MODIFICATIONS
!!    -------------
!!      Original     01/2014
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
!
USE MODD_SEAFLUX_n, ONLY : SEAFLUX_t
!
USE MODD_CSTS,ONLY : XTT
USE MODD_SURF_PAR,   ONLY : XUNDEF
USE MODD_TYPES_GLT , ONLY : T_GLT
USE MODD_GLT_PARAM , ONLY : XTSTEP=>DTT, LWG, LP1, LP2, LP3, LP4, LP5, &
                            NPRINTO, GELATO_DIM=>NX

USE MODI_GLT_GELATO
USE MODI_GLT_SNDATMF
USE MODI_GLT_SNDMLRF
USE MODI_GLT_GETMLRF
USE MODI_GLT_GETATMF
USE MODI_GLTOOLS_CHKINP
USE MODI_GLTOOLS_CHKOUT
USE MODE_GLT_STATS
USE MODE_GLTOOLS_SWFRZT
USE MODE_GLT_DIA_AR5
!
!
USE MODI_GET_LUOUT
USE MODI_ABOR1_SFX
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
!
TYPE(SEAFLUX_t), INTENT(INOUT) :: S
!
 CHARACTER(LEN=6),    INTENT(IN)       :: HPROGRAM  ! program calling surf. schemes
REAL,                INTENT(IN)       :: PTIMEC    ! current duration since start of the run (s)
REAL,                INTENT(IN)       :: PTSTEP    ! surface time-step (s)
TYPE(T_GLT)         ,INTENT(INOUT)    :: TPGLT     ! Gelato state variable
REAL, DIMENSION(:)  ,INTENT(IN)       :: PSST      ! sea surface temperature (K)
REAL, DIMENSION(:)  ,INTENT(IN)       :: PSSS      ! sea surface salinity (psu)
REAL, DIMENSION(:)  ,INTENT(IN)       :: PFSIC ! sea ice cover constraint ([0-1]). 
REAL, DIMENSION(:)  ,INTENT(IN)       :: PFSIT ! sea ice thickness constraint (m). 
!
REAL, DIMENSION(:)  ,INTENT(OUT)      :: PSIC      ! Sea-ice Cover ([0-1])
REAL, DIMENSION(:)  ,INTENT(OUT)      :: PTICE     ! Sea-ice temperature (K)
REAL, DIMENSION(:)  ,INTENT(OUT)      :: PICE_ALB  ! Sea-ce albedo ([0-1])
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(  SIZE(PSST)) :: ZSST  ! sea surface temperature with frozen sea set at freezing point
REAL, DIMENSION(  SIZE(PSST)) :: ZSIC  ! Will hold a forcing SIC field, even if PFSIC is missing
!
INTEGER :: IT      ! total number of Gelato timesteps in one atmospheric timestep
INTEGER :: JT      ! Running index 1 -> IT
REAL    :: ZT      ! total number of Gelato timesteps in one atmospheric timestep
REAL    :: ZTIMEG  ! Gelato simulation time (s)

! The time step for ice coupling could be a namelist parameter
! for now, it will be set, here, as equal to the ice model time step
REAL :: ZICE_COUPLING_TSTEP
!
!
INTEGER         :: ILUOUT              ! output listing logical unit
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('SEAICE_GELATO1D',0,ZHOOK_HANDLE)
!
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
! Must restore Gelato problem size (nx) to the correct value for the NPROMA block
! 
GELATO_DIM=SIZE(PSSS)
!
! Time steps stuff : default Gelato time step equals surface time step
!
IF (S%XSEAICE_TSTEP == XUNDEF) THEN
  IT=1
  ZT=1.
  XTSTEP=PTSTEP
!
!* case of a Gelato time-step specified using NAM_SEAICE
ELSE
   IF ( PTSTEP < S%XSEAICE_TSTEP ) THEN
      CALL ABOR1_SFX("XSEAICE_TSTEP SHOULD BE EQUAL OR LESS THAN ATMOSPHERIC TIME STEP")
   ELSE
      IT=NINT(PTSTEP/S%XSEAICE_TSTEP)
      ZT=FLOAT(IT)
      XTSTEP=PTSTEP/ZT
   ENDIF
ENDIF
!
!          Initializations
!________________________________________________________________________
!
! Ocean part 
!----------------------------------------------------------------------------------
! Surface salinity 
TPGLT%oce_all(:,1)%sml=PSSS(:)

! Ensure that SSS-dependant freezing point temperature is used on
! locations where SIC (or SST) forcing value calls for it

! First init ZSST with freezing temperature (which depends on local salinity)
! (Gelato uses Celsius scale for freezing temperatures)
ZSST=RESHAPE(glt_swfrzt2d(RESHAPE(PSSS,(/SIZE(PSSS),1/))) + XTT,(/SIZE(PSSS)/))

! Then replace freezing temp with Surfex-provided SST (PSST) where
! there is no (explicit or implicit) seaice and temperature is warmer
! than freezing point. And inits ZSIC accordingly
IF (S%LINTERPOL_SIC) THEN  
   ZSIC=PFSIC
   WHERE ( ZSIC(:) < 1.e-10 .AND. PSST(:) > ZSST(:) ) 
      ZSST(:)=PSST(:)
   ENDWHERE
ELSE 
   ! Implicit sea-ice cover
   WHERE (PSST(:) - XTT > S%XFREEZING_SST + 0.1 )
      ZSST(:)= PSST(:) 
      ZSIC(:)=0.
   ELSEWHERE
      ZSIC(:)=1.
   ENDWHERE
ENDIF
!
!        Run Gelato for the length of the atmospheric time step
!________________________________________________________________________
!
!  Reset output accumulation/averaging fields 
PSIC    = 0.
PTICE   = 0.
PICE_ALB= 0.
LP1 = (LWG.AND.NPRINTO>=1)
LP2 = (LWG.AND.NPRINTO>=2)
LP3 = (LWG.AND.NPRINTO>=3)
LP4 = (LWG.AND.NPRINTO>=4)
LP5 = (LWG.AND.NPRINTO>=5)

DO JT=1,IT
   IF (SIZE(PSSS) > 0) THEN 
      TPGLT%oce_all(:,1)%tml=ZSST(:)
      IF (S%LINTERPOL_SIC) TPGLT%sit_d(1,:,1)%fsi=ZSIC(:)
      IF (S%LINTERPOL_SIT) TPGLT%sit_d(1,:,1)%hsi=PFSIT(:)
      ! Gelato will compute heat flux from ocean by itself, thanks to 
      ! imposed namelist parameter nextqoc=0 
      TPGLT%oce_all(:,1)%qoc=0. 
      ! Zero Frazil flux
      TPGLT%oce_all(:,1)%qml=0.
      ! Don't bother for sea level variations
      TPGLT%oce_all(:,1)%ssh=0.
      ! (velocity components are useless in Surfex 1D setting)
      !
      ! Atmosphere part 
      !----------------
      ! Feed Gelato input structure with flux values from XCPL_xx
      ! 
      TPGLT%atm_all(:,1)%lip=S%XCPL_SEA_RAIN(:) / PTSTEP
      TPGLT%atm_all(:,1)%sop=S%XCPL_SEA_SNOW(:) / PTSTEP
      ! Fluxes over Sea water
      TPGLT%atm_wat(:,1)%eva=S%XCPL_SEA_EVAP(:) / PTSTEP
      TPGLT%atm_wat(:,1)%swa=S%XCPL_SEA_SNET(:) / PTSTEP 
      TPGLT%atm_wat(:,1)%nsf=S%XCPL_SEA_HEAT(:) / PTSTEP 
      TPGLT%atm_wat(:,1)%dfl=S%XSI_FLX_DRV ! W m-2 K-1    
      ! Fluxes over Sea ice
      TPGLT%atm_ice(1,:,1)%eva=S%XCPL_SEAICE_EVAP(:) / PTSTEP
      TPGLT%atm_ice(1,:,1)%swa=S%XCPL_SEAICE_SNET(:) / PTSTEP 
      TPGLT%atm_ice(1,:,1)%nsf=S%XCPL_SEAICE_HEAT(:) / PTSTEP 
      TPGLT%atm_ice(1,:,1)%dfl=S%XSI_FLX_DRV ! W m-2 K-1   
      ! (stress components are useless in Surfex 1D setting)
      !
      !       Let Gelato process its input data
      !
      CALL GLT_GETMLRF( TPGLT%oce_all,TPGLT%tml )
      CALL GLT_GETATMF( TPGLT )
      CALL GLTOOLS_CHKINP( 20010101,TPGLT )
      !
      ! Compute gelato time index
      !
      TPGLT%IND%CUR = ( PTIMEC + JT * XTSTEP ) / XTSTEP 
      !
      !       Let Gelato thermodynamic scheme run
      !
      CALL GLT_GELATO( TPGLT )
      !
      ! Have Gelato feed its coupling ouptut interface
      ! 
      CALL GLT_SNDATMF( TPGLT )
      CALL GLT_SNDMLRF( TPGLT%bat,TPGLT%dom,TPGLT%atm_all,TPGLT%tml, &
           TPGLT%dia,TPGLT%sit,TPGLT%tfl,TPGLT%ust,TPGLT%all_oce )
      CALL WRIDIA_AR5( TPGLT )
      CALL GLTOOLS_CHKOUT( 20010101,TPGLT ) ! Does not actually work with Arpege
      ! Sum output fields over Gelato model time step duration
      PSIC     = PSIC     + TPGLT%ice_atm(1,:,1)%fsi * XTSTEP
      PTICE    = PTICE    + TPGLT%ice_atm(1,:,1)%tsf * XTSTEP
      PICE_ALB = PICE_ALB + TPGLT%ice_atm(1,:,1)%alb * XTSTEP
   ENDIF
END DO
!   Average output fields over coupling time
PSIC     = PSIC     / (IT * XTSTEP)
PTICE    = PTICE    / (IT * XTSTEP) 
PICE_ALB = PICE_ALB / (IT * XTSTEP) 
!
! Resets input accumulation fields for next step 
!
S%XCPL_SEA_RAIN=0.
S%XCPL_SEA_SNOW=0.
S%XCPL_SEA_EVAP=0.
S%XCPL_SEA_SNET=0.
S%XCPL_SEA_HEAT=0.
S%XCPL_SEAICE_EVAP=0.
S%XCPL_SEAICE_SNET=0.
S%XCPL_SEAICE_HEAT=0.
!
IF (LHOOK) CALL DR_HOOK('SEAICE_GELATO1D',1,ZHOOK_HANDLE)
!!-------------------------------------------------------------------------------
!!-----------------------------------------------------------------------------
END SUBROUTINE SEAICE_GELATO1D_n
