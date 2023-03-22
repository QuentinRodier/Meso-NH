!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
    SUBROUTINE CARBON_LEACHING(PTSTEP, KSPINS,                       &
                               PCONTROL_TEMP, PCONTROL_MOIST ,       &
                               PCONTROL_SAND, PCONTROL_LEACH ,       &
                               PSOIL_LITTER, PSOILCARB,              &
                               PFDOC                                 )  
!   ###############################################################
!!****  *CARBON LEACHING*
!!
!!    PURPOSE
!!    -------
!
!     compute export of dissolved carbon due to water flow 
!              
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!    none
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!      
!!    none
!!
!!    REFERENCE
!!    ---------
!!
!!      
!!    AUTHOR
!!    ------
!!
!!    R. Séférian (04-2016)
!!    B. Decharme 11/2020 : Bug & Surface / Soil litter split
!!
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_SURF_PAR,       ONLY : XUNDEF
!
USE MODD_CO2V_PAR,       ONLY : XTAU_SOILCARB, XTAU_LITTER
!
USE YOMHOOK   ,          ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,          ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
REAL,                     INTENT(IN)  :: PTSTEP      ! Time step in s
! 
INTEGER,                  INTENT(IN)  :: KSPINS      ! (spinup) number of times is called for each time step
!
REAL, DIMENSION(:),   INTENT(IN)    :: PCONTROL_TEMP ! control in soil temperature due to decomposition
REAL, DIMENSION(:),   INTENT(IN)    :: PCONTROL_MOIST! control in soil moisture due to decomposition
REAL, DIMENSION(:),   INTENT(IN)    :: PCONTROL_LEACH! control in soil water flow
REAL, DIMENSION(:),   INTENT(IN)    :: PCONTROL_SAND ! control in soil texture
!
REAL, DIMENSION(:,:), INTENT(INOUT) :: PSOIL_LITTER  ! Soil litter pools   (gC/m2)
REAL, DIMENSION(:,:), INTENT(INOUT) :: PSOILCARB     ! carbon content in soil layer (gC/m2)
!
REAL, DIMENSION(:),   INTENT(OUT)   :: PFDOC         ! Soil carbon leaching due to soil water flow  (gC m-2)
!
!*      0.2    declarations of local parameter
!
REAL, DIMENSION(SIZE(PCONTROL_LEACH))                           :: ZFDOC        ! dissolved organic carbon flux         (gC m-2)
!
REAL, DIMENSION(SIZE(PSOILCARB,1),SIZE(PSOILCARB,2))            :: ZWORK
REAL, DIMENSION(SIZE(PSOIL_LITTER,1),SIZE(PSOIL_LITTER,2))      :: ZTAU_LITTER
!
INTEGER                               :: JI, JNL, JNC      ! index
!
INTEGER                               :: INI, INL, INC
!
REAL                                  :: ZDT, ZSPIN
!
REAL(KIND=JPRB)                       :: ZHOOK_HANDLE
!
!-----------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('CARBON_LEACHING',0,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
! correspondence between array indices and litter type (INL)
! LT_METABOLIC = 1
! LT_STRUCTURAL = 2
!-------------------------------------------------------------------------------
!
!*      1.     Preliminaries
!              -------------
!
INI = SIZE(PSOIL_LITTER(:,:),1)
INL = SIZE(PSOIL_LITTER(:,:),2)
INC = SIZE(PSOILCARB   (:,:),2)
!
ZSPIN = REAL(KSPINS)
!
PFDOC   (:) = 0.0
!
ZFDOC (:) = 0.0
!
ZDT = ZSPIN*PTSTEP
!
ZTAU_LITTER(:,:) =  0.0 
ZTAU_LITTER(:,1) =  XTAU_LITTER(1)
ZTAU_LITTER(:,2) =  XTAU_LITTER(2)
!
!-----------------------------------------------------------------
!
!*      2.     Soil Litter driven by soil drainage
!              -----------------------------------
!
ZFDOC (:) = 0.0
!
DO JNL=1,INL
   DO JI=1,INI
!
!     Leaching/Mobilisation Soil Litter
      ZFDOC (JI) = (ZDT/ZTAU_LITTER(JI,JNL))                              & ! Tscale 
                   * PCONTROL_TEMP(JI)*PCONTROL_MOIST(JI)*PCONTROL_LEACH(JI) & ! control
                   * PSOIL_LITTER(JI,JNL)                                    ! Cpool
!
      ZFDOC(JI) = MIN(ZFDOC(JI),PSOIL_LITTER(JI,JNL))
!
!     Update reservoirs
      PSOIL_LITTER(JI,JNL) = PSOIL_LITTER(JI,JNL)-ZFDOC(JI)
!
!     Update total doc flux  in gC m-2 s-1
      PFDOC(JI) = PFDOC(JI) + ZFDOC(JI) / ZDT
!
   ENDDO
ENDDO
!
!-----------------------------------------------------------------
!
!*      3.     Soil Carbon driven by soil drainage
!              -----------------------------------
!
ZFDOC (:) = 0.0
!
ZWORK(:,:) = 1.
ZWORK(:,1) = PCONTROL_SAND(:)
!
DO JNC=1,INC
   DO JI=1,INI
!
!     Leaching/Mobilisation
      ZFDOC (JI) = (ZDT/XTAU_SOILCARB(JNC))                                  & ! Tscale 
                   * PCONTROL_TEMP(JI)*PCONTROL_MOIST(JI)*PCONTROL_LEACH(JI)   & ! control
                   * ZWORK(JI,JNC)                                              & ! control texture 
                   * PSOILCARB(JI,JNC)                                            ! Cpool
!
      ZFDOC(JI) = MIN(ZFDOC(JI),PSOILCARB(JI,JNC))
!
!     Update reservoirs
      PSOILCARB(JI,JNC) = PSOILCARB(JI,JNC)-ZFDOC(JI)
!
!     Update total doc flux in gC m-2 s-1
      PFDOC(JI) = PFDOC(JI) + ZFDOC(JI) / ZDT
!
   ENDDO
ENDDO
!
!-----------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('CARBON_LEACHING',1,ZHOOK_HANDLE)
!-----------------------------------------------------------------
!
END SUBROUTINE CARBON_LEACHING
