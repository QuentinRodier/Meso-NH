!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
       SUBROUTINE DIAG_CPL_ESM_SEA (S, &
                                     PTSTEP,PZON10M,PMER10M,PSFU,PSFV,     &
                                      PSWD,PSWU,PGFLUX,PSFTQ,PRAIN,PSNOW, &
                                      PLW,PPS,PTICE,PSFTH_ICE,PSFTQ_ICE,  &
                                      PDIR_SW,PSCA_SW,PSWU_ICE,PLWU_ICE,  &
                                      OSIC                                )  
!     ###################################################################
!
!!****  *DIAG_CPL_ESM_SEA * - Computes diagnostics over sea for 
!!                Earth system model coupling or embedded seaice scheme
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
!!     B. Decharme 
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    08/2009
!!      S.Senesi    01/2014  Adapt to embedded seaice scheme (SWU and LWU 
!!                           for seaice are provided as inputs)
!!      A.Voldoire  04/2015  Add LCPL_SEAICE test
!!      Modified    11/2014 : J. Pianezze : Add surface pressure coupling parameter
!!------------------------------------------------------------------
!
!
USE MODD_SEAFLUX_n, ONLY : SEAFLUX_t
!
USE MODD_CSTS,      ONLY : XSTEFAN, XLSTT
USE MODD_WATER_PAR, ONLY : XEMISWATICE
!
USE MODD_SFX_OASIS, ONLY : LCPL_SEAICE
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
TYPE(SEAFLUX_t), INTENT(INOUT) :: S
!
REAL,               INTENT(IN) :: PTSTEP    ! atmospheric time-step
REAL, DIMENSION(:), INTENT(IN) :: PZON10M   ! zonal wind
REAL, DIMENSION(:), INTENT(IN) :: PMER10M   ! meridian wind
REAL, DIMENSION(:), INTENT(IN) :: PSFU      ! zonal wind stress
REAL, DIMENSION(:), INTENT(IN) :: PSFV      ! meridian wind stress
REAL, DIMENSION(:), INTENT(IN) :: PSWD      ! total incoming short wave radiation
REAL, DIMENSION(:), INTENT(IN) :: PSWU      ! total upward short wave radiation
REAL, DIMENSION(:), INTENT(IN) :: PGFLUX    ! storage flux
REAL, DIMENSION(:), INTENT(IN) :: PSFTQ     ! water flux
REAL, DIMENSION(:), INTENT(IN) :: PRAIN     ! Rainfall
REAL, DIMENSION(:), INTENT(IN) :: PSNOW     ! Snowfall
REAL, DIMENSION(:), INTENT(IN) :: PLW       ! longwave radiation (on horizontal surf.)
REAL, DIMENSION(:), INTENT(IN) :: PPS       ! Surface pressure
REAL, DIMENSION(:), INTENT(IN) :: PSFTH_ICE ! heat flux  (W/m2)
REAL, DIMENSION(:), INTENT(IN) :: PSFTQ_ICE ! water flux (kg/m2/s)
REAL, DIMENSION(:), INTENT(IN) :: PTICE     ! Ice Surface Temperature
REAL, DIMENSION(:,:),INTENT(IN):: PDIR_SW   ! direct  solar radiation (on horizontal surf.)
REAL, DIMENSION(:,:),INTENT(IN):: PSCA_SW   ! diffuse solar radiation (on horizontal surf.)
REAL, DIMENSION(:), INTENT(IN) :: PSWU_ICE  ! upward short wave radiation on seaice
REAL, DIMENSION(:), INTENT(IN) :: PLWU_ICE  ! upward long  wave radiation on seaice
LOGICAL,            INTENT(IN) :: OSIC
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(SIZE(S%XICE_ALB)) :: ZSWU, ZTICE4
!
INTEGER                      :: ISWB ! number of SW bands
INTEGER                      :: JSWB ! loop counter on number of SW bands
INTEGER                      :: INI  ! number of points
INTEGER                      :: JI   ! loop counter on number of points
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('DIAG_CPL_ESM_SEA',0,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
! Total or free-ice sea flux
!-------------------------------------------------------------------------------------
!
!* 10m wind speed (m)
!
S%XCPL_SEA_WIND(:) = S%XCPL_SEA_WIND(:) + PTSTEP * SQRT(PZON10M(:)**2+PMER10M(:)**2)
! 
!* wind stress (Pa.s)
!
S%XCPL_SEA_FWSU(:) = S%XCPL_SEA_FWSU(:) + PTSTEP * PSFU(:)
S%XCPL_SEA_FWSV(:) = S%XCPL_SEA_FWSV(:) + PTSTEP * PSFV(:)
S%XCPL_SEA_FWSM(:) = S%XCPL_SEA_FWSM(:) + PTSTEP * SQRT(PSFU(:)**2+PSFV(:)**2)
!
!* Solar net heat flux (J/m2)
!
S%XCPL_SEA_SNET(:) = S%XCPL_SEA_SNET(:) + PTSTEP * (PSWD(:) - PSWU(:))
!
!* Non solar heat flux (J/m2)
!
S%XCPL_SEA_HEAT(:) = S%XCPL_SEA_HEAT(:) + PTSTEP * (PGFLUX(:) + PSWU(:) - PSWD(:)) 
!
!* Evaporation (kg/m2)
!
S%XCPL_SEA_EVAP(:) = S%XCPL_SEA_EVAP(:) + PTSTEP * PSFTQ(:)
!
!* Precip (kg/m2)
! 
S%XCPL_SEA_RAIN(:) = S%XCPL_SEA_RAIN(:) + PTSTEP * PRAIN(:)
S%XCPL_SEA_SNOW(:) = S%XCPL_SEA_SNOW(:) + PTSTEP * PSNOW(:)
!
!* Evaporation - Precip (kg/m2)
! 
S%XCPL_SEA_EVPR(:) = S%XCPL_SEA_EVPR(:) + S%XCPL_SEA_EVAP(:) - S%XCPL_SEA_RAIN(:) - S%XCPL_SEA_SNOW(:)
!
!* Cumulated surface pressure (Pa.s)
! 
S%XCPL_SEA_PRES(:) = S%XCPL_SEA_PRES(:) + PTSTEP * PPS(:)
!
!-------------------------------------------------------------------------------------
! Ice flux
!-------------------------------------------------------------------------------------
IF (LCPL_SEAICE.OR.OSIC) THEN
!
  INI  = SIZE(PDIR_SW,1)
  ISWB = SIZE(PDIR_SW,2)
!
!* Solar net heat flux (J/m2)
!
  IF (OSIC) THEN
    ZSWU(:)=PSWU_ICE(:)
  ELSE
    ZSWU(:)=0.0
    DO JSWB=1,ISWB
      DO JI=1,INI
         ZSWU(JI) = ZSWU(JI) + (PDIR_SW(JI,JSWB)+PSCA_SW(JI,JSWB)) * S%XICE_ALB(JI)
      ENDDO
    ENDDO
  ENDIF
!
  S%XCPL_SEAICE_SNET(:) = S%XCPL_SEAICE_SNET(:) + PTSTEP * (PSWD(:) - ZSWU(:))
!
!* Non solar heat flux (J/m2)
!
  IF (OSIC) THEN
    S%XCPL_SEAICE_HEAT(:) = S%XCPL_SEAICE_HEAT(:) + PTSTEP * &
              ( PLW(:) - PLWU_ICE(:) - PSFTH_ICE(:) - XLSTT*PSFTQ_ICE(:) )
  ELSE
    ZTICE4(:)=PTICE(:)**4
    S%XCPL_SEAICE_HEAT(:) = S%XCPL_SEAICE_HEAT(:) + PTSTEP * ( XEMISWATICE*(PLW(:)-XSTEFAN*ZTICE4(:)) &
                                                         - PSFTH_ICE(:) - XLSTT*PSFTQ_ICE(:)      ) 
  ENDIF 
!
!* Sublimation (kg/m2)
!
  S%XCPL_SEAICE_EVAP(:) = S%XCPL_SEAICE_EVAP(:) + PTSTEP * PSFTQ_ICE(:)
!
ENDIF
!
IF (LHOOK) CALL DR_HOOK('DIAG_CPL_ESM_SEA',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE DIAG_CPL_ESM_SEA
