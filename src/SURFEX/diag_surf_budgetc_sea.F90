!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
       SUBROUTINE DIAG_SURF_BUDGETC_SEA (DGS, &
                                          PTSTEP, PRN, PH, PLE, PLE_ICE, PGFLUX,&
                                         PSWD, PSWU, PLWD, PLWU, PFMU, PFMV,   &  
                                         PEVAP, PSUBL, OHANDLE_SIC,            &
                                         PRN_ICE, PH_ICE, PGFLUX_ICE,          &
                                         PSWU_ICE, PLWU_ICE, PFMU_ICE, PFMV_ICE)  
!     ########################################################################
!
!!****  *DIAG_SURF_BUDGETC_SEA * - Computes cumulated diagnostics over sea
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
!!      S.Senesi    01/2014  Add fluxes on seaice
!!------------------------------------------------------------------
! 
!
!
!
USE MODD_DIAG_SEAFLUX_n, ONLY : DIAG_SEAFLUX_t
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
TYPE(DIAG_SEAFLUX_t), INTENT(INOUT) :: DGS
!
REAL,               INTENT(IN) :: PTSTEP    
REAL, DIMENSION(:), INTENT(IN) :: PRN      ! net radiation                         (W/m2)
REAL, DIMENSION(:), INTENT(IN) :: PH       ! sensible heat flux                    (W/m2)
REAL, DIMENSION(:), INTENT(IN) :: PLE      ! total latent heat flux                (W/m2)
REAL, DIMENSION(:), INTENT(IN) :: PLE_ICE  ! sublimation latent heat flux          (W/m2)
REAL, DIMENSION(:), INTENT(IN) :: PGFLUX   ! storage flux                          (W/m2)
REAL, DIMENSION(:), INTENT(IN) :: PEVAP    ! total evaporation                     (kg/m2/s)
REAL, DIMENSION(:), INTENT(IN) :: PSUBL    ! sublimation                           (kg/m2/s)
REAL, DIMENSION(:), INTENT(IN) :: PSWD     ! total incoming short wave radiation (W/m2)
REAL, DIMENSION(:), INTENT(IN) :: PSWU     ! total upward short wave radiation (W/m2)
REAL, DIMENSION(:), INTENT(IN) :: PLWD     ! Downward long wave radiation (W/m2)
REAL, DIMENSION(:), INTENT(IN) :: PLWU     ! upward long wave radiation (W/m2)
REAL, DIMENSION(:), INTENT(IN) :: PFMU     ! zonal wind stress
REAL, DIMENSION(:), INTENT(IN) :: PFMV     ! meridian wind stress
!
LOGICAL, INTENT(IN)         :: OHANDLE_SIC  ! Do we weight seaice and open sea fluxes
!
REAL, DIMENSION(:), INTENT(IN) :: PRN_ICE  ! net radiation                         (W/m2)
REAL, DIMENSION(:), INTENT(IN) :: PH_ICE   ! sensible heat flux                    (W/m2)
REAL, DIMENSION(:), INTENT(IN) :: PGFLUX_ICE!storage flux                          (W/m2)
REAL, DIMENSION(:), INTENT(IN) :: PSWU_ICE ! total upward short wave radiation (W/m2)
REAL, DIMENSION(:), INTENT(IN) :: PLWU_ICE ! upward long wave radiation (W/m2)
REAL, DIMENSION(:), INTENT(IN) :: PFMU_ICE ! zonal wind stress
REAL, DIMENSION(:), INTENT(IN) :: PFMV_ICE ! meridian wind stress
!
!
!*      0.2    declarations of local variables
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('DIAG_SURF_BUDGETC_SEA',0,ZHOOK_HANDLE)
!
!* total incoming and outgoing SW
!
DGS%XSWDC(:) = DGS%XSWDC(:) + PSWD(:) * PTSTEP
DGS%XSWUC(:) = DGS%XSWUC(:) + PSWU(:) * PTSTEP
!
!*incoming outgoing LW
!
DGS%XLWDC(:) = DGS%XLWDC(:) + PLWD(:) * PTSTEP
DGS%XLWUC(:) = DGS%XLWUC(:) + PLWU(:) * PTSTEP
!
!* net radiation
!
DGS%XRNC(:) = DGS%XRNC(:) + PRN(:) * PTSTEP
!
!* sensible heat flux
!
DGS%XHC(:) = DGS%XHC(:) + PH(:) * PTSTEP 
!
!* latent heat flux (J/m2)
!
DGS%XLEC    (:) = DGS%XLEC    (:) + PLE    (:) * PTSTEP 
DGS%XLEC_ICE(:) = DGS%XLEC_ICE(:) + PLE_ICE(:) * PTSTEP 
!
!* evaporation and sublimation (kg/m2)
!
DGS%XEVAPC(:) = DGS%XEVAPC(:) + PEVAP(:) * PTSTEP
DGS%XSUBLC(:) = DGS%XSUBLC(:) + PSUBL(:) * PTSTEP
!
!* storage flux
!
DGS%XGFLUXC(:) = DGS%XGFLUXC(:) + PGFLUX(:) * PTSTEP
!
!* wind stress
!
DGS%XFMUC(:) = DGS%XFMUC(:) + PFMU(:) * PTSTEP 
DGS%XFMVC(:) = DGS%XFMVC(:) + PFMV(:) * PTSTEP
!
IF (OHANDLE_SIC) THEN
!
!* total incoming and outgoing SW
!
   DGS%XSWUC_ICE(:) = DGS%XSWUC_ICE(:) + PSWU_ICE(:) * PTSTEP
!
!*incoming outgoing LW
!
   DGS%XLWUC_ICE(:) = DGS%XLWUC_ICE(:) + PLWU_ICE(:) * PTSTEP
!
!* net radiation
!
   DGS%XRNC_ICE(:) = DGS%XRNC_ICE(:) + PRN_ICE(:) * PTSTEP
!
!* sensible heat flux
!
   DGS%XHC_ICE(:) = DGS%XHC_ICE(:) + PH_ICE(:) * PTSTEP 
!
!* storage flux
!
   DGS%XGFLUXC_ICE(:) = DGS%XGFLUXC_ICE(:) + PGFLUX_ICE(:) * PTSTEP 
!
!* wind stress
!
   DGS%XFMUC_ICE(:) = DGS%XFMUC_ICE(:) + PFMU_ICE(:) * PTSTEP 
   DGS%XFMVC_ICE(:) = DGS%XFMVC_ICE(:) + PFMV_ICE(:) * PTSTEP
!        
ENDIF
!
IF (LHOOK) CALL DR_HOOK('DIAG_SURF_BUDGETC_SEA',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE DIAG_SURF_BUDGETC_SEA
