!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
       SUBROUTINE DIAG_SURF_BUDGETC_IDEAL(DGL, PTSTEP, PRN, PH, PLE, PLEI, PGFLUX,  &
                                            PSWD, PSWU, PLWD, PLWU, PFMU, PFMV,&  
                                            PEVAP, PSUBL                       )  
!     #########################################################################
!
!!****  *DIAG_SURF_BUDGETC_IDEAL * - Computes cumulated diagnostics in IDEAL case
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
!!      Original    03/2015
!!------------------------------------------------------------------
! 
USE MODD_DIAG_IDEAL_n, ONLY : DIAG_IDEAL_t  
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
TYPE(DIAG_IDEAL_t) :: DGL
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
IF (LHOOK) CALL DR_HOOK('DIAG_SURF_BUDGETC_IDEAL',0,ZHOOK_HANDLE)
DGL%XSWDC(:) = DGL%XSWDC(:) + PSWD(:) * PTSTEP
DGL%XSWUC(:) = DGL%XSWUC(:) + PSWU(:) * PTSTEP
!
!*incoming outgoing LW
!
DGL%XLWDC(:) = DGL%XLWDC(:) + PLWD(:) * PTSTEP
DGL%XLWUC(:) = DGL%XLWUC(:) + PLWU(:) * PTSTEP
!
!* net radiation
!
DGL%XRNC(:) = DGL%XRNC(:) + PRN(:) * PTSTEP
!
!* sensible heat flux
!
DGL%XHC(:) = DGL%XHC(:) + PH(:) * PTSTEP 
!
!* latent heat flux
!
DGL%XLEC (:) = DGL%XLEC (:) + PLE (:) * PTSTEP 
DGL%XLEIC(:) = DGL%XLEIC(:) + PLEI(:) * PTSTEP 
!
!* evaporation and sublimation (kg/m2)
!
DGL%XEVAPC(:) = DGL%XEVAPC(:) + PEVAP(:) * PTSTEP
DGL%XSUBLC(:) = DGL%XSUBLC(:) + PSUBL(:) * PTSTEP
!
!* storage flux
!
DGL%XGFLUXC(:) = DGL%XGFLUXC(:) + PGFLUX(:) * PTSTEP 
!
!* wind stress
!
DGL%XFMUC(:) = DGL%XFMUC(:) + PFMU(:) * PTSTEP 
DGL%XFMVC(:) = DGL%XFMVC(:) + PFMV(:) * PTSTEP
!
IF (LHOOK) CALL DR_HOOK('DIAG_SURF_BUDGETC_IDEAL',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE DIAG_SURF_BUDGETC_IDEAL
