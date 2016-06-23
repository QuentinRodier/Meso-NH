!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE UPDATE_RAD_FLAKE(HALB,PTS,PZENITH,PH_ICE,PH_SNOW,PICE_ALB,PSNOW_ALB,   &
                            PDIR_ALB,PSCA_ALB,PEMIS,PDIR_ALB_ATMOS,PSCA_ALB_ATMOS,&
                            PEMIS_ATMOS,PTRAD )  
!     #######################################################################
!
!!****  *UPDATE_RAD_FLAKE * - update the radiative properties at time t+1 (see by the atmosphere) 
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
!!      Original    04/2013
!!------------------------------------------------------------------
!
USE MODD_WATER_PAR, ONLY : XALBSCA_WAT, XALBWAT, XEMISWAT, XEMISWATICE
!
USE modd_flake_parameters , ONLY : h_Snow_min_flk, h_Ice_min_flk
!
USE MODD_SNOW_PAR, ONLY : XEMISSN
!
USE MODI_ALBEDO_TA96
USE MODI_ALBEDO_MK10
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
 CHARACTER(LEN=4),       INTENT(IN)   :: HALB
!
REAL, DIMENSION(:),     INTENT(IN)   :: PTS       !  surface temperature
REAL, DIMENSION(:),     INTENT(IN)   :: PZENITH   ! Zenithal angle at t+1
REAL, DIMENSION(:),     INTENT(IN)   :: PH_ICE    ! ice depth at t+
REAL, DIMENSION(:),     INTENT(IN)   :: PH_SNOW   ! snow depth at t+
REAL, DIMENSION(:),     INTENT(IN)   :: PICE_ALB  ! ice albedo at t+
REAL, DIMENSION(:),     INTENT(IN)   :: PSNOW_ALB ! snow albedo at t+
!
REAL, DIMENSION(:),     INTENT(INOUT):: PDIR_ALB  ! Direct albedo at t+1
REAL, DIMENSION(:),     INTENT(INOUT):: PSCA_ALB  ! Diffuse albedo at t+1
REAL, DIMENSION(:),     INTENT(OUT)  :: PEMIS     ! emissivity (soil+vegetation) at t+1
!
REAL, DIMENSION(:,:),   INTENT(OUT)  :: PDIR_ALB_ATMOS ! Direct albedo at t+1 for the atmosphere
REAL, DIMENSION(:,:),   INTENT(OUT)  :: PSCA_ALB_ATMOS ! Diffuse albedo at t+1 for the atmosphere
REAL, DIMENSION(:),     INTENT(OUT)  :: PEMIS_ATMOS    ! Emissivity at t+1 for the atmosphere
REAL, DIMENSION(:),     INTENT(OUT)  :: PTRAD          ! radiative temp at t+1 for the atmosphere
!
!*      0.2    declarations of local variables
!
INTEGER :: JSWB
!
REAL, DIMENSION(SIZE(PTS)) :: ZALBDIR
REAL, DIMENSION(SIZE(PTS)) :: ZALBSCA
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('UPDATE_RAD_FLAKE',0,ZHOOK_HANDLE)
!
ZALBDIR(:) = 0.
ZALBSCA(:) = 0.
!
IF (HALB=='TA96') THEN
  ZALBDIR(:) = ALBEDO_TA96(PZENITH(:))
  ZALBSCA(:) = XALBSCA_WAT
ELSEIF (HALB=='MK10') THEN
  ZALBDIR(:) = ALBEDO_MK10(PZENITH(:))
  ZALBSCA(:) = XALBSCA_WAT
ELSE
  ZALBDIR(:) = XALBWAT
  ZALBSCA(:) = XALBWAT
ENDIF
!
WHERE (PH_SNOW(:)>=h_Snow_min_flk)
!* snow
  PDIR_ALB  (:) = PSNOW_ALB(:)
  PSCA_ALB  (:) = PSNOW_ALB(:)
  PEMIS     (:) = XEMISSN
ELSEWHERE(PH_ICE(:)>=h_ice_min_flk)
!* ice
  PDIR_ALB(:) = PICE_ALB(:)
  PSCA_ALB(:) = PICE_ALB(:)
  PEMIS   (:) = XEMISWATICE
ELSEWHERE
!* open water
  PDIR_ALB  (:) = ZALBDIR(:)
  PSCA_ALB  (:) = ZALBSCA(:)
  PEMIS     (:) = XEMISWAT    
END WHERE
!
!-------------------------------------------------------------------------------------
!
DO JSWB=1,SIZE(PDIR_ALB_ATMOS,2)
  PDIR_ALB_ATMOS(:,JSWB) = PDIR_ALB(:)
  PSCA_ALB_ATMOS(:,JSWB) = PSCA_ALB(:)
END DO
!
PEMIS_ATMOS(:) = PEMIS(:)
PTRAD      (:) = PTS  (:)
!
IF (LHOOK) CALL DR_HOOK('UPDATE_RAD_FLAKE',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE UPDATE_RAD_FLAKE

