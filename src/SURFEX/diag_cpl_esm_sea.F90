!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
       SUBROUTINE DIAG_CPL_ESM_SEA (PTSTEP,PZON10M,PMER10M,PSFU,PSFV,   &
                                      PSWD,PSWU,PGFLUX,PSFTQ,PRAIN,PSNOW, &
                                      PLW,PTICE,PSFTH_ICE,PSFTQ_ICE,      &
                                      PDIR_SW,PSCA_SW                     )  
!     ###################################################################
!
!!****  *DIAG_CPL_ESM_SEA * - Computes diagnostics over sea for 
!!                                Earth system model coupling
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
!!------------------------------------------------------------------
!
USE MODD_CSTS,      ONLY : XSTEFAN, XLSTT
USE MODD_WATER_PAR, ONLY : XEMISWATICE
!
USE MODD_SEAFLUX_n, ONLY : XCPL_SEA_WIND,XCPL_SEA_EVAP,XCPL_SEA_HEAT, &
                             XCPL_SEA_SNET,XCPL_SEA_FWSU,XCPL_SEA_FWSV, &
                             XCPL_SEA_RAIN,XCPL_SEA_SNOW,XCPL_SEA_FWSM, &
                             XICE_ALB, XCPL_SEAICE_SNET,                &
                             XCPL_SEAICE_EVAP,XCPL_SEAICE_HEAT                             
! 
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
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
REAL, DIMENSION(:), INTENT(IN) :: PSFTH_ICE ! heat flux  (W/m2)
REAL, DIMENSION(:), INTENT(IN) :: PSFTQ_ICE ! water flux (kg/m2/s)
REAL, DIMENSION(:), INTENT(IN) :: PTICE     ! Ice Surface Temperature
REAL, DIMENSION(:,:),INTENT(IN):: PDIR_SW   ! direct  solar radiation (on horizontal surf.)
REAL, DIMENSION(:,:),INTENT(IN):: PSCA_SW   ! diffuse solar radiation (on horizontal surf.)
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(SIZE(XICE_ALB)) :: ZSWU
!
INTEGER                      :: ISWB ! number of SW bands
INTEGER                      :: JSWB ! loop counter on number of SW bands
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------
! Total or free-ice sea flux
!-------------------------------------------------------------------------------------
!
!* 10m wind speed (m)
!
IF (LHOOK) CALL DR_HOOK('DIAG_CPL_ESM_SEA',0,ZHOOK_HANDLE)
XCPL_SEA_WIND(:) = XCPL_SEA_WIND(:) + PTSTEP * SQRT(PZON10M(:)**2+PMER10M(:)**2)
! 
!* wind stress (Pa.s)
!
XCPL_SEA_FWSU(:) = XCPL_SEA_FWSU(:) + PTSTEP * PSFU(:)
XCPL_SEA_FWSV(:) = XCPL_SEA_FWSV(:) + PTSTEP * PSFV(:)
XCPL_SEA_FWSM(:) = XCPL_SEA_FWSM(:) + PTSTEP * SQRT(PSFU(:)**2+PSFV(:)**2)
!
!* Solar net heat flux (J/m2)
!
XCPL_SEA_SNET(:) = XCPL_SEA_SNET(:) + PTSTEP * (PSWD(:) - PSWU(:))
!
!* Non solar heat flux (J/m2)
!
XCPL_SEA_HEAT(:) = XCPL_SEA_HEAT(:) + PTSTEP * (PGFLUX(:) + PSWU(:) - PSWD(:)) 
!
!* Evaporation (mm/day)
!
XCPL_SEA_EVAP(:) = XCPL_SEA_EVAP(:) + PTSTEP * PSFTQ(:)
!
!* Precip (mm/day)
! 
XCPL_SEA_RAIN(:) = XCPL_SEA_RAIN(:) + PTSTEP * PRAIN(:)
XCPL_SEA_SNOW(:) = XCPL_SEA_SNOW(:) + PTSTEP * PSNOW(:)
!
!-------------------------------------------------------------------------------------
! Ice flux
!-------------------------------------------------------------------------------------
!
ISWB = SIZE(PDIR_SW,2)
!
!* Solar net heat flux (J/m2)
!
ZSWU(:)=0.0
DO JSWB=1,ISWB
   ZSWU(:) = ZSWU(:) + (PDIR_SW(:,JSWB)+PSCA_SW(:,JSWB)) * XICE_ALB(:)
ENDDO
!
XCPL_SEAICE_SNET(:) = XCPL_SEAICE_SNET(:) + PTSTEP * (PSWD(:) - ZSWU(:))
!
!* Non solar heat flux (J/m2)
!
XCPL_SEAICE_HEAT(:) = XCPL_SEAICE_HEAT(:) + PTSTEP * ( XEMISWATICE*(PLW(:)-XSTEFAN*PTICE(:)**4) &
                                                         - PSFTH_ICE(:) - XLSTT*PSFTQ_ICE(:)      )  
!
!* Sublimation (mm/day)
!
XCPL_SEAICE_EVAP(:) = XCPL_SEAICE_EVAP(:) + PTSTEP * PSFTQ_ICE(:)
IF (LHOOK) CALL DR_HOOK('DIAG_CPL_ESM_SEA',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE DIAG_CPL_ESM_SEA
