!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE AVERAGE_TSLSI(U, SM, KSEA, PFRAC_TILE, PEMIS_TILE, PTRAD_TILE, PTSLSI)
!     #################################################################
!
!
!!****  *AVERAGE_TSLSI*  
!!
!!    PURPOSE
!!    -------
!      Average the radiative temperature from the land and water surfaces except open ocean
!      depending on the fraction of each surface cover type in the mesh area.
!     
!!**  METHOD
!!    ------
!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------ 
!!
!!      
!!    REFERENCE
!!    ---------
!!      
!!    AUTHOR
!!    ------
!!      B. Decharme           * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    07/04/2017
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_SURF_ATM_n,ONLY : SURF_ATM_t
USE MODD_SURFEX_n,  ONLY : SEAFLUX_MODEL_t
!
USE MODD_WATER_PAR, ONLY : XEMISWATICE
!
USE MODD_SURF_PAR,  ONLY : XUNDEF
!
USE MODD_SFX_OASIS, ONLY : LCPL_SEAICE

USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
TYPE(SURF_ATM_t),      INTENT(INOUT) :: U
TYPE(SEAFLUX_MODEL_t), INTENT(INOUT) :: SM
!
INTEGER,               INTENT(IN) :: KSEA
REAL, DIMENSION(:,:),  INTENT(IN) :: PFRAC_TILE    ! Fraction in a mesh-area of a given surface
REAL, DIMENSION(:,:),  INTENT(IN) :: PEMIS_TILE    ! emissivity
REAL, DIMENSION(:,:),  INTENT(IN) :: PTRAD_TILE    ! surface radiative temp.
!
REAL, DIMENSION(:),    INTENT(OUT):: PTSLSI        ! except open ocean
!
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(SIZE(PFRAC_TILE,1),SIZE(PFRAC_TILE,2)) :: ZFRAC_TILE
REAL, DIMENSION(SIZE(PFRAC_TILE,1),SIZE(PFRAC_TILE,2)) :: ZEMIS_TILE
REAL, DIMENSION(SIZE(PFRAC_TILE,1),SIZE(PFRAC_TILE,2)) :: ZTRAD_TILE
!
REAL, DIMENSION(SIZE(PTSLSI))                          :: ZTICE
REAL, DIMENSION(SIZE(PTSLSI))                          :: ZSIC
REAL, DIMENSION(SIZE(PTSLSI))                          :: ZEMIS
!
INTEGER :: INI,INP    ! dimenssion
INTEGER :: JI, JP     ! loop counter on tiles
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('AVERAGE_TSLSI',0,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
INI   = SIZE(PFRAC_TILE,1)
INP   = SIZE(PFRAC_TILE,2)
!
ZFRAC_TILE(:,:) = PFRAC_TILE(:,:)
ZEMIS_TILE(:,:) = PEMIS_TILE(:,:)
ZTRAD_TILE(:,:) = PTRAD_TILE(:,:)
!
!       1.     sea ice treatement
!              ------------------
!
ZSIC (:) = 0.0
ZTICE(:) = 0.0
!
IF(LCPL_SEAICE.OR.SM%S%LHANDLE_SIC)THEN
  DO JP=1,U%NSIZE_SEA
     JI        = U%NR_SEA(JP) 
     ZSIC (JI) = SM%S%XSIC (JP)
     ZTICE(JI) = SM%S%XTICE(JP)
  ENDDO
ENDIF
!
ZFRAC_TILE(:,KSEA) = PFRAC_TILE(:,KSEA) * ZSIC(:)
ZEMIS_TILE(:,KSEA) = XEMISWATICE
ZTRAD_TILE(:,KSEA) = ZTICE(:)
!
!       2.     emissivity
!              ------------------
!
ZEMIS(:) = 0.
!
DO JP = 1,INP
  DO JI = 1,INI
     ZEMIS(JI) = ZEMIS(JI) + ZFRAC_TILE(JI,JP)*ZEMIS_TILE(JI,JP)
  END DO
END DO
!
!       3.     radiative surface temperature
!              -----------------------------
!
! radiative surface temperature
!
PTSLSI (:)   = 0.
!
DO JP = 1,INP
  DO JI = 1,INI
     PTSLSI(JI) = PTSLSI(JI) + (ZTRAD_TILE(JI,JP)**4)*ZFRAC_TILE(JI,JP)*ZEMIS_TILE(JI,JP)
  END DO
END DO
!
WHERE(ZEMIS(:)>0.0)
      PTSLSI(:) = EXP(0.25*LOG(PTSLSI(:)/ZEMIS(:)))
ELSEWHERE
      PTSLSI(:) = XUNDEF
ENDWHERE
!
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('AVERAGE_TSLSI',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE AVERAGE_TSLSI
