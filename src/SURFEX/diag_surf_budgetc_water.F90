!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
       SUBROUTINE DIAG_SURF_BUDGETC_WATER(PTSTEP, PRN, PH, PLE, PLEI, PGFLUX, &
                                            PSWD, PSWU, PLWD, PLWU, PFMU, PFMV  )  
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
USE MODD_DIAG_WATFLUX_n, ONLY : XRNC, XHC, XLEC, XGFLUXC, XSWDC,  &
                                  XSWUC, XLWDC, XLWUC, XFMUC, XFMVC,&
                                  XLEIC  
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL,               INTENT(IN) :: PTSTEP    
REAL, DIMENSION(:), INTENT(IN) :: PRN      ! net radiation                         (W/m2)
REAL, DIMENSION(:), INTENT(IN) :: PH       ! sensible heat flux                    (W/m2)
REAL, DIMENSION(:), INTENT(IN) :: PLE      ! total latent heat flux                (W/m2)
REAL, DIMENSION(:), INTENT(IN) :: PLEI     ! sublimation latent heat flux          (W/m2)
REAL, DIMENSION(:), INTENT(IN) :: PGFLUX   ! storage flux                          (W/m2)
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
XSWDC(:) = XSWDC(:) + PSWD(:) * PTSTEP
XSWUC(:) = XSWUC(:) + PSWU(:) * PTSTEP
!
!*incoming outgoing LW
!
XLWDC(:) = XLWDC(:) + PLWD(:) * PTSTEP
XLWUC(:) = XLWUC(:) + PLWU(:) * PTSTEP
!
!* net radiation
!
XRNC(:) = XRNC(:) + PRN(:) * PTSTEP
!
!* sensible heat flux
!
XHC(:) = XHC(:) + PH(:) * PTSTEP 
!
!* latent heat flux
!
XLEC (:) = XLEC (:) + PLE (:) * PTSTEP 
XLEIC(:) = XLEIC(:) + PLEI(:) * PTSTEP 
!
!* storage flux
!
XGFLUXC(:) = XGFLUXC(:) + PGFLUX(:) * PTSTEP 
!
!* wind stress
!
XFMUC(:) = XFMUC(:) + PFMU(:) * PTSTEP 
!
XFMVC(:) = XFMVC(:) + PFMV(:) * PTSTEP
IF (LHOOK) CALL DR_HOOK('DIAG_SURF_BUDGETC_WATER',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE DIAG_SURF_BUDGETC_WATER
