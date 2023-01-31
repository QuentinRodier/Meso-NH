!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
       SUBROUTINE DIAG_INLINE_SURF_ATM_n (DGO, D, PHW, PHT, PPS, PRHOA, PTRAD, PEMIS, PSFU, PSFV, PSFCO2, &
                                          PZS, PSCA_SW, PTSLSI, PDIR_ALB, PSCA_ALB, PSFCO2NAT, PSFCO2ANT, &
                                          PTA, PQA, PU, PV, PRAIN, PSNOW, PCO2                            )
!     ###############################################################################!
!!****  *DIAG_INLINE_SURF_ATM_n * - Computes diagnostics during SURF_ATM time-step
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
!!     P. LeMoigne
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    02/2006
!!------------------------------------------------------------------
!
!
USE MODD_DIAG_n, ONLY : DIAG_t, DIAG_OPTIONS_t
!
USE MODD_SURF_PAR, ONLY : XUNDEF
USE MODD_CSTS,     ONLY : XMD
USE MODD_CO2V_PAR, ONLY : XMCO2
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
TYPE(DIAG_OPTIONS_t), INTENT(INOUT) :: DGO
TYPE(DIAG_t),         INTENT(INOUT) :: D
!
REAL, DIMENSION(:), INTENT(IN)       :: PHW    ! atmospheric level height for wind
REAL, DIMENSION(:), INTENT(IN)       :: PHT    ! atmospheric level height
REAL, DIMENSION(:), INTENT(IN)       :: PPS    ! surface pressure
REAL, DIMENSION(:), INTENT(IN)       :: PRHOA  ! air density
REAL, DIMENSION(:), INTENT(IN)       :: PTRAD  ! radiative temperature at t (K)
REAL, DIMENSION(:), INTENT(IN)       :: PEMIS  ! emissivity at t (-)
REAL, DIMENSION(:), INTENT(IN)       :: PSFU   ! zonal momentum flux                   (Pa)
REAL, DIMENSION(:), INTENT(IN)       :: PSFV   ! meridian momentum flux                (Pa)
REAL, DIMENSION(:), INTENT(IN)       :: PSFCO2 ! CO2 flux                              (kg/m2/s)
REAL, DIMENSION(:), INTENT(IN)       :: PSFCO2NAT ! CO2 flux                              (kgC/m2/s)
REAL, DIMENSION(:), INTENT(IN)       :: PSFCO2ANT ! CO2 flux                              (kgC/m2/s)
REAL, DIMENSION(:), INTENT(IN)       :: PZS       ! atmospheric model orography           (m)
REAL, DIMENSION(:), INTENT(IN)       :: PTSLSI    ! radiative temperature except open ocean at t+1  (K)
REAL, DIMENSION(:), INTENT(IN)       :: PTA
REAL, DIMENSION(:), INTENT(IN)       :: PQA
REAL, DIMENSION(:), INTENT(IN)       :: PU
REAL, DIMENSION(:), INTENT(IN)       :: PV
REAL, DIMENSION(:), INTENT(IN)       :: PRAIN
REAL, DIMENSION(:), INTENT(IN)       :: PSNOW
REAL, DIMENSION(:), INTENT(IN)       :: PCO2
!
REAL, DIMENSION(:,:),INTENT(IN)      :: PSCA_SW   ! diffuse solar radiation (on horizontal surf.) (W/m2)
REAL, DIMENSION(:,:),INTENT(IN)      :: PDIR_ALB  ! direct albedo for each spectral band  (-)
REAL, DIMENSION(:,:),INTENT(IN)      :: PSCA_ALB  ! diffuse albedo for each spectral band (-)
!
!*      0.2    declarations of local variables
!
INTEGER                      :: INI, JI
INTEGER                      :: ISWB ! number of SW bands
INTEGER                      :: JSWB ! loop counter on number of SW bands
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('DIAG_INLINE_SURF_ATM_N',0,ZHOOK_HANDLE)
!
IF (DGO%LCOEF) THEN
  D%XUREF = PHW
  D%XZREF = PHT
END IF
!
D%XRHOA    (:) = PRHOA(:)
D%XPS      (:) = PPS(:)
D%XTA_FRC  (:) = PTA(:)
D%XU_FRC   (:) = PU(:)
D%XV_FRC   (:) = PV(:)
D%XRAIN_FRC(:) = PRAIN(:)
D%XSNOW_FRC(:) = PSNOW(:)
!
WHERE(PPS(:)/=XUNDEF)
     D%XQA_FRC(:)=PQA(:)/PRHOA(:)
ELSEWHERE
     D%XQA_FRC(:)=XUNDEF
ENDWHERE
!
WHERE(PPS(:)/=XUNDEF)
     D%XCO2_FRC(:)=PCO2(:)/(PRHOA(:)*(XMCO2/XMD*1.E-6))
ELSEWHERE
     D%XCO2_FRC(:)=XUNDEF
ENDWHERE
!
D%XTRAD = PTRAD
D%XEMIS = PEMIS
!
D%XSSO_FMU   = PSFU
D%XSSO_FMV   = PSFV
!
D%XSFCO2 = PSFCO2
D%XSFCO2NAT = PSFCO2NAT
D%XSFCO2ANT = PSFCO2ANT
!
D%XOROG = PZS
!
D%XALB_DIR = PDIR_ALB
D%XALB_SCA = PSCA_ALB
!
IF(DGO%LRAD_BUDGET)THEN
  INI  = SIZE(PSCA_SW,1)
  ISWB = SIZE(PSCA_SW,2)
  D%XSWD_SCA = 0.0
  DO JSWB=1,ISWB
     DO JI=1,INI
        D%XSWD_SCA(JI) = D%XSWD_SCA(JI) + PSCA_SW(JI,JSWB)
     ENDDO
  ENDDO
ENDIF
!
IF (LHOOK) CALL DR_HOOK('DIAG_INLINE_SURF_ATM_N',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------------
!
END SUBROUTINE DIAG_INLINE_SURF_ATM_n
