!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
       SUBROUTINE DIAG_SURF_BUDGETC_WATER (DGW, &
                                           PTSTEP, PRN, PH, PLE, PLEI, PGFLUX,  &
                                            PSWD, PSWU, PLWD, PLWU, PFMU, PFMV,&
                                            PEVAP, PSUBL                       )  
!     #########################################################################
!
!!****  *DIAG_SURF_BUDGETC_WATER * - Computes cumulated diagnostics over water
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
!
!
!
USE MODD_DIAG_WATFLUX_n, ONLY : DIAG_WATFLUX_t
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
TYPE(DIAG_WATFLUX_t), INTENT(INOUT) :: DGW
!
REAL,               INTENT(IN) :: PTSTEP    
REAL, DIMENSION(:), INTENT(IN) :: PRN      ! net radiation                         (W/m2)
REAL, DIMENSION(:), INTENT(IN) :: PH       ! sensible heat flux                    (W/m2)
REAL, DIMENSION(:), INTENT(IN) :: PLE      ! total latent heat flux                (W/m2)
REAL, DIMENSION(:), INTENT(IN) :: PLEI     ! sublimation latent heat flux          (W/m2)
REAL, DIMENSION(:), INTENT(IN) :: PGFLUX   ! storage flux                          (W/m2)
REAL, DIMENSION(:), INTENT(IN) :: PEVAP    ! total evaporation                     (kg/m2/s)
REAL, DIMENSION(:), INTENT(IN) :: PSUBL    ! sublimation                           (kg/m2/s)
REAL, DIMENSION(:), INTENT(IN) :: PSWD     ! total incoming short wave radiation (W/m2)
REAL, DIMENSION(:), INTENT(IN) :: PSWU     ! total upward short wave radiation (W/m2)
REAL, DIMENSION(:), INTENT(IN) :: PLWD     ! Downward long wave radiation (W/m2)
REAL, DIMENSION(:), INTENT(IN) :: PLWU     ! upward long wave radiation (W/m2)
REAL, DIMENSION(:), INTENT(IN) :: PFMU     ! zonal wind stress
REAL, DIMENSION(:), INTENT(IN) :: PFMV     ! meridian wind stress
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*      0.2    declarations of local variables
!
!-------------------------------------------------------------------------------------
!
!* total incoming and outgoing SW
!
IF (LHOOK) CALL DR_HOOK('DIAG_SURF_BUDGETC_WATER',0,ZHOOK_HANDLE)
DGW%XSWDC(:) = DGW%XSWDC(:) + PSWD(:) * PTSTEP
DGW%XSWUC(:) = DGW%XSWUC(:) + PSWU(:) * PTSTEP
!
!*incoming outgoing LW
!
DGW%XLWDC(:) = DGW%XLWDC(:) + PLWD(:) * PTSTEP
DGW%XLWUC(:) = DGW%XLWUC(:) + PLWU(:) * PTSTEP
!
!* net radiation
!
DGW%XRNC(:) = DGW%XRNC(:) + PRN(:) * PTSTEP
!
!* sensible heat flux
!
DGW%XHC(:) = DGW%XHC(:) + PH(:) * PTSTEP 
!
!* latent heat flux (J/m2)
!
DGW%XLEC (:) = DGW%XLEC (:) + PLE (:) * PTSTEP 
DGW%XLEIC(:) = DGW%XLEIC(:) + PLEI(:) * PTSTEP 
!
!* evaporation and sublimation (kg/m2)
!
DGW%XEVAPC(:) = DGW%XEVAPC(:) + PEVAP(:) * PTSTEP
DGW%XSUBLC(:) = DGW%XSUBLC(:) + PSUBL(:) * PTSTEP
!
!* storage flux
!
DGW%XGFLUXC(:) = DGW%XGFLUXC(:) + PGFLUX(:) * PTSTEP 
!
!* wind stress
!
DGW%XFMUC(:) = DGW%XFMUC(:) + PFMU(:) * PTSTEP 
DGW%XFMVC(:) = DGW%XFMVC(:) + PFMV(:) * PTSTEP
!
IF (LHOOK) CALL DR_HOOK('DIAG_SURF_BUDGETC_WATER',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE DIAG_SURF_BUDGETC_WATER
