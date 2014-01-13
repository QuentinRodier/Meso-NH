!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #######################################################################################
      SUBROUTINE UPDATE_ESM_WATFLUX_n(HPROGRAM,KI,KSW,PZENITH,PDIR_ALB,PSCA_ALB,PEMIS,PTSRAD)
!     #######################################################################################
!
!!****  *UPDATE_ESM_WATFLUX_n* - routine to update WATFLUX radiative properties in Earth
!!                               System Model after the call to OASIS coupler in order 
!!                               to close the energy budget between radiative scheme and surfex
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
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
!!      Original    09/2009
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_CSTS,           ONLY : XTT
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODD_WATFLUX_n,      ONLY : XDIR_ALB, XSCA_ALB,           &
                                  XEMIS, XTS, CWAT_ALB  
!                                
USE MODI_UPDATE_RAD_SEAWAT
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
 CHARACTER(LEN=6),                   INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
INTEGER,                            INTENT(IN)  :: KI        ! number of points
INTEGER,                            INTENT(IN)  :: KSW       ! number of short-wave spectral bands
!
REAL,             DIMENSION(KI),    INTENT(IN)  :: PZENITH   ! solar zenithal angle
!
REAL,             DIMENSION(KI,KSW),INTENT(OUT) :: PDIR_ALB  ! direct albedo for each band
REAL,             DIMENSION(KI,KSW),INTENT(OUT) :: PSCA_ALB  ! diffuse albedo for each band
REAL,             DIMENSION(KI),    INTENT(OUT) :: PEMIS     ! emissivity
REAL,             DIMENSION(KI),    INTENT(OUT) :: PTSRAD    ! radiative temperature
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
!-------------------------------------------------------------------------------
!
!
!*            Albedo and emissivity on open sea and sea ice
!             ---------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('UPDATE_ESM_WATFLUX_N',0,ZHOOK_HANDLE)
 CALL UPDATE_RAD_SEAWAT(CWAT_ALB,XTS,PZENITH,XTT,XEMIS,XDIR_ALB,&
                         XSCA_ALB,PDIR_ALB,PSCA_ALB,PEMIS,PTSRAD )  
IF (LHOOK) CALL DR_HOOK('UPDATE_ESM_WATFLUX_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE UPDATE_ESM_WATFLUX_n
