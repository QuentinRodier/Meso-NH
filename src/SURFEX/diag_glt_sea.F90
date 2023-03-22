!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
       SUBROUTINE DIAG_GLT_SEA (S, D, DI, PRAIN, PSNOW, PLW)  
!     ###################################################################
!
!!****  *DIAG_GLT_SEA * - Computes diagnostics over sea and seaice for 
!!                                 membedded seaice scheme  (GELATO1D)
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
!!     A. Voldoire 
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    06/2018
!!------------------------------------------------------------------
!
!
USE MODD_DIAG_n,    ONLY : DIAG_t
USE MODD_SEAFLUX_n, ONLY : SEAFLUX_t
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
TYPE(SEAFLUX_t), INTENT(INOUT) :: S
TYPE(DIAG_t), INTENT(INOUT) :: D
TYPE(DIAG_t), INTENT(INOUT) :: DI
!
REAL, DIMENSION(:), INTENT(IN) :: PRAIN     ! Rainfall
REAL, DIMENSION(:), INTENT(IN) :: PSNOW     ! Snowfall
REAL, DIMENSION(:), INTENT(IN) :: PLW       ! longwave radiation (on horizontal surf.)
!
!*      0.2    declarations of local variables
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('DIAG_GLT_SEA',0,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
! Total or free-ice sea flux
!-------------------------------------------------------------------------------------
!
!* Solar net heat flux (J/m2)
!
S%XGLT_SEA_SNET(:) = D%XSWD(:) - D%XSWU(:)
!
!* Non solar heat flux (J/m2)
!
S%XGLT_SEA_HEAT(:) = D%XGFLUX(:) - S%XGLT_SEA_SNET(:) 
!
!* Evaporation (kg/m2)
!
S%XGLT_SEA_EVAP(:) = D%XEVAP(:)
!
!* Precip (kg/m2)
! 
S%XGLT_SEA_RAIN(:) = PRAIN(:)
S%XGLT_SEA_SNOW(:) = PSNOW(:)
!
!-------------------------------------------------------------------------------------
! Ice flux
!-------------------------------------------------------------------------------------
!
!* Solar net heat flux (J/m2)
!
S%XGLT_SEAICE_SNET(:) = D%XSWD(:) - DI%XSWU(:)
!
!* Non solar heat flux (J/m2)
!
S%XGLT_SEAICE_HEAT(:) =  PLW(:)-DI%XLWU(:)-DI%XH(:)-DI%XLE(:) 
!
!* Sublimation (kg/m2)
!
S%XGLT_SEAICE_EVAP(:) = DI%XEVAP(:)
!
!
!-------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('DIAG_GLT_SEA',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE DIAG_GLT_SEA
