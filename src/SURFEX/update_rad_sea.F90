!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE UPDATE_RAD_SEA(S,PZENITH,PTT,PDIR_ALB_ATMOS,PSCA_ALB_ATMOS,PEMIS_ATMOS,PTRAD,PU,PV)  
!     #######################################################################
!
!!****  *UPDATE_RAD_SEA * - update the radiative properties at time t+1 (see by the atmosphere) 
!                           in order to close the energy budget between surfex and the atmosphere
 
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
!!      Original    09/2009
!!      Modified    03/2011 : E. Bazile (MK10) albedo from Marat Khairoutdinov
!!      Modified    01/2014 : S. Senesi : handle fractional seaice
!!      Modified    02/2014 : split from update_rad_seawat.F90
!!      Modified    01/2015 : introduce interactive ocean surface albedo (R.Séférian)
!!      A. Voldoire 09/2016 : Switch to tile the fluxes calculation over sea and seaice
!!------------------------------------------------------------------
!
USE MODD_SEAFLUX_n, ONLY : SEAFLUX_t
!
USE MODD_WATER_PAR, ONLY : XEMISWAT, XEMISWATICE, &
                           XALBWAT, XALBSCA_WAT,  &
                           XALBSEAICE
USE MODD_SURF_PAR,  ONLY : XUNDEF
!
USE MODD_SFX_OASIS, ONLY : LCPL_SEAICE
!
USE MODI_ALBEDO_TA96
USE MODI_ALBEDO_MK10
USE MODI_ALBEDO_RS14
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
TYPE(SEAFLUX_t), INTENT(INOUT) :: S
!
REAL, DIMENSION(:),     INTENT(IN)   :: PZENITH   ! Zenithal angle at t+1
REAL,                   INTENT(IN)   :: PTT       ! Sea/ice transition temperature (different according to sea or inland water)
!
REAL, DIMENSION(:,:),   INTENT(OUT)  :: PDIR_ALB_ATMOS ! Direct albedo at t+1 for the atmosphere
REAL, DIMENSION(:,:),   INTENT(OUT)  :: PSCA_ALB_ATMOS ! Diffuse albedo at t+1 for the atmosphere
REAL, DIMENSION(:),     INTENT(OUT)  :: PEMIS_ATMOS    ! Emissivity at t+1 for the atmosphere
REAL, DIMENSION(:),     INTENT(OUT)  :: PTRAD          ! radiative temp at t+1 for the atmosphere
!
REAL, DIMENSION(:),     INTENT(IN)   , OPTIONAL :: PU        ! zonal wind (m/s)
REAL, DIMENSION(:),     INTENT(IN)   , OPTIONAL :: PV        ! meridian wind (m/s)
!
! *      0.2    declarations of local variables
!
REAL, DIMENSION(SIZE(PZENITH)) :: ZALBDIR_SEA
REAL, DIMENSION(SIZE(PZENITH)) :: ZALBSCA_SEA
REAL, DIMENSION(SIZE(PZENITH)) :: ZWIND
REAL, DIMENSION(SIZE(PZENITH)) :: ZT4
!
INTEGER :: JSWB
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('UPDATE_RAD_SEA',0,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------------
!
! *      1.0 init
!        ---------
!
ZALBDIR_SEA(:) = XUNDEF
ZALBSCA_SEA(:) = XUNDEF
!
!-------------------------------------------------------------------------------------
!
! *      2.0 computation of sea (ice-free) albedo
!        ----------------------------------------
!
IF (S%CSEA_ALB=='UNIF') THEN
!
! Uniform
!
  ZALBDIR_SEA(:) = XALBWAT
  ZALBSCA_SEA(:) = XALBWAT
!
ELSEIF (S%CSEA_ALB=='TA96') THEN
!
! Taylor et al 1996
!
  ZALBDIR_SEA(:) = ALBEDO_TA96(PZENITH(:))
  ZALBSCA_SEA(:) = XALBSCA_WAT
!  
ELSEIF (S%CSEA_ALB=='MK10') THEN
!      
! Khairoutdinov
!
  ZALBDIR_SEA(:) = ALBEDO_MK10(PZENITH(:))
  ZALBSCA_SEA(:) = XALBSCA_WAT
!  
ELSEIF (S%CSEA_ALB=='RS14') THEN
!
! Seferian et al 2017
!
  IF (PRESENT(PU).AND.PRESENT(PV)) THEN
     ZWIND(:) = SQRT(PU(:)**2+PV(:)**2)
     CALL ALBEDO_RS14(PZENITH(:),ZWIND(:),S%XDIR_ALB_SEA(:),S%XSCA_ALB_SEA(:))
  ENDIF
!
  ZALBDIR_SEA(:) = S%XDIR_ALB_SEA(:)
  ZALBSCA_SEA(:) = S%XSCA_ALB_SEA(:)
!
ENDIF
!
!-------------------------------------------------------------------------------------
!
! *      3.0 computation of sea (including) sea-ice radiative properties
!        ---------------------------------------------------------------
!
IF(LCPL_SEAICE.AND..NOT.(S%LHANDLE_SIC))THEN
!
! For Earth System Model in CNRM-CM5 : Single fluxes
!
  WHERE (S%XSST(:)>=PTT  )
    !* open water
    S%XEMIS   (:) = XEMISWAT
    S%XDIR_ALB(:) = ZALBDIR_SEA(:)
    S%XSCA_ALB(:) = ZALBSCA_SEA(:)
  ELSEWHERE
    !* sea ice
    S%XEMIS   (:) = XEMISWATICE
    S%XDIR_ALB(:) = S%XICE_ALB(:)
    S%XSCA_ALB(:) = S%XICE_ALB(:)
  END WHERE
!
ELSEIF(S%LHANDLE_SIC) THEN 
!
! Gelato 1D case or Double fluxes in CNRM-CM6 Earth System Model :
! Returned values are an average of open sea and seaice properties
! weighted by the seaice cover
!
  S%XEMIS   (:) = (1.0 - S%XSIC(:)) * XEMISWAT       + S%XSIC(:) * XEMISWATICE
  S%XDIR_ALB(:) = (1.0 - S%XSIC(:)) * ZALBDIR_SEA(:) + S%XSIC(:) * S%XICE_ALB(:)
  S%XSCA_ALB(:) = (1.0 - S%XSIC(:)) * ZALBSCA_SEA(:) + S%XSIC(:) * S%XICE_ALB(:)
!
ELSE
!
! Default case without sea ice scheme :
! Ice albedo is imposed
!
  WHERE (S%XSST(:)>=PTT)
  !* open water
    S%XDIR_ALB  (:) = ZALBDIR_SEA(:)
    S%XSCA_ALB  (:) = ZALBSCA_SEA(:)
    S%XEMIS     (:) = XEMISWAT
  ELSEWHERE
  !* sea ic should not be theree
    S%XDIR_ALB(:) = XALBSEAICE
    S%XSCA_ALB(:) = XALBSEAICE
    S%XEMIS   (:) = XEMISWATICE
  END WHERE
!
ENDIF
!
!-------------------------------------------------------------------------------------
!
! *      4.0 Final diag
!        --------------
!
! Albedo
!
DO JSWB=1,SIZE(PDIR_ALB_ATMOS,2)
  PDIR_ALB_ATMOS(:,JSWB) = S%XDIR_ALB(:)
  PSCA_ALB_ATMOS(:,JSWB) = S%XSCA_ALB(:)
END DO
!
! Emissivity
!
PEMIS_ATMOS(:) = S%XEMIS(:)
!
! Radiative temperature
!
IF(S%LHANDLE_SIC) THEN 
!
   ZT4  (:) = (1.0 - S%XSIC(:)) * XEMISWAT * S%XSST (:)**4 + S%XSIC(:) * XEMISWATICE * S%XTICE(:)**4
!
   PTRAD(:) = EXP(0.25*LOG(ZT4(:)/S%XEMIS(:)))
!
ELSE
!
   PTRAD(:) = S%XSST (:)
!
END IF
!
!-------------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('UPDATE_RAD_SEA',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------------
!
END SUBROUTINE UPDATE_RAD_SEA

