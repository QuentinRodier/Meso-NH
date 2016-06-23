!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
       SUBROUTINE DIAG_SURF_BUDGETC_FLAKE (DGF, &
                                           PTSTEP, PRN, PH, PLE, PLEI, PGFLUX, &
                                            PSWD, PSWU, PLWD, PLWU, PFMU, PFMV,&  
                                            PEVAP, PSUBL                       )  
!     #########################################################################
!
!!****  *DIAG_SURF_BUDGETC_FLAKE * - Computes cumulated diagnostics over water
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
USE MODD_DIAG_FLAKE_n, ONLY : DIAG_FLAKE_t
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
TYPE(DIAG_FLAKE_t), INTENT(INOUT) :: DGF
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
IF (LHOOK) CALL DR_HOOK('DIAG_SURF_BUDGETC_FLAKE',0,ZHOOK_HANDLE)
DGF%XSWDC(:) = DGF%XSWDC(:) + PSWD(:) * PTSTEP
DGF%XSWUC(:) = DGF%XSWUC(:) + PSWU(:) * PTSTEP
!
!*incoming outgoing LW
!
DGF%XLWDC(:) = DGF%XLWDC(:) + PLWD(:) * PTSTEP
DGF%XLWUC(:) = DGF%XLWUC(:) + PLWU(:) * PTSTEP
!
!* net radiation
!
DGF%XRNC(:) = DGF%XRNC(:) + PRN(:) * PTSTEP
!
!* sensible heat flux
!
DGF%XHC(:) = DGF%XHC(:) + PH(:) * PTSTEP 
!
!* latent heat flux
!
DGF%XLEC (:) = DGF%XLEC (:) + PLE (:) * PTSTEP 
DGF%XLEIC(:) = DGF%XLEIC(:) + PLEI(:) * PTSTEP 
!
!* evaporation and sublimation (kg/m2)
!
DGF%XEVAPC(:) = DGF%XEVAPC(:) + PEVAP(:) * PTSTEP
DGF%XSUBLC(:) = DGF%XSUBLC(:) + PSUBL(:) * PTSTEP
!
!* storage flux
!
DGF%XGFLUXC(:) = DGF%XGFLUXC(:) + PGFLUX(:) * PTSTEP 
!
!* wind stress
!
DGF%XFMUC(:) = DGF%XFMUC(:) + PFMU(:) * PTSTEP 
DGF%XFMVC(:) = DGF%XFMVC(:) + PFMV(:) * PTSTEP
!
IF (LHOOK) CALL DR_HOOK('DIAG_SURF_BUDGETC_FLAKE',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE DIAG_SURF_BUDGETC_FLAKE
