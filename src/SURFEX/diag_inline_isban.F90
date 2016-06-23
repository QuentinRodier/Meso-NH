!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
 SUBROUTINE DIAG_INLINE_ISBA_n (DGEI, DGI, I, PKDI, &
                                 PTA, PTS, PQA, PPA, PPS, PRHOA, PZONA, PMERA,  &
                                  PHT, PHW, PCD, PCDN, PCH, PRI, PHU, PZ0, PZ0H, &
                                  PZ0EFF, PSFTH, PSFTQ, PSFZON, PSFMER, PQS,     &
                                  PDIR_ALB, PSCA_ALB, PDIR_SW, PSCA_SW, PLW, PRN )  
!     ###############################################################################
!
!!****  *DIAG_INLINE_ISBA_n * - computes diagnostics during ISBA time-step
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
!!     V. Masson 
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2004
!!      B. Decharme 08/2009 caculate cumulated diag LSURF_BUDGETC
!!      S. Riette   06/2009 CLS_2M becomes CLS_TQ, CLS_TQ and CLS_WIND have one
!!                          more argument (height of diagnostic)
!!------------------------------------------------------------------
!

!
!
!
USE MODD_DIAG_EVAP_ISBA_n, ONLY : DIAG_EVAP_ISBA_t
USE MODD_DIAG_ISBA_n, ONLY : DIAG_ISBA_t
USE MODD_ISBA_n, ONLY : ISBA_t
USE MODD_PACK_DIAG_ISBA, ONLY : PACK_DIAG_ISBA_t
!
USE MODD_SURF_PAR,         ONLY : XUNDEF
!
USE MODI_PARAM_CLS
USE MODI_CLS_TQ
USE MODI_CLS_WIND
USE MODI_DIAG_SURF_BUDGET_ISBA
! 
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
TYPE(DIAG_EVAP_ISBA_t), INTENT(INOUT) :: DGEI
TYPE(DIAG_ISBA_t), INTENT(INOUT) :: DGI
TYPE(ISBA_t), INTENT(INOUT) :: I
TYPE(PACK_DIAG_ISBA_t), INTENT(INOUT) :: PKDI
!
REAL, DIMENSION(:), INTENT(IN)       :: PTA      ! atmospheric temperature
REAL, DIMENSION(:), INTENT(IN)       :: PTS      ! surface temperature
REAL, DIMENSION(:), INTENT(IN)       :: PQA      ! atmospheric specific humidity
REAL, DIMENSION(:), INTENT(IN)       :: PPA      ! atmospheric level pressure
REAL, DIMENSION(:), INTENT(IN)       :: PPS      ! surface pressure
REAL, DIMENSION(:), INTENT(IN)       :: PRHOA    ! air density
REAL, DIMENSION(:), INTENT(IN)       :: PZONA    ! zonal wind
REAL, DIMENSION(:), INTENT(IN)       :: PMERA    ! meridian wind
REAL, DIMENSION(:), INTENT(IN)       :: PHT      ! atmospheric level height
REAL, DIMENSION(:), INTENT(IN)       :: PHW      ! atmospheric level height for wind
REAL, DIMENSION(:), INTENT(IN)       :: PCD      ! drag coefficient for momentum
REAL, DIMENSION(:), INTENT(IN)       :: PCDN     ! neutral drag coefficient
REAL, DIMENSION(:), INTENT(IN)       :: PCH      ! drag coefficient for heat
REAL, DIMENSION(:), INTENT(IN)       :: PRI      ! Richardson number
REAL, DIMENSION(:), INTENT(IN)       :: PHU      ! near-surface humidity
REAL, DIMENSION(:), INTENT(IN)       :: PZ0      ! roughness length for momentum
REAL, DIMENSION(:), INTENT(IN)       :: PZ0H     ! roughness length for heat
REAL, DIMENSION(:), INTENT(IN)       :: PZ0EFF   ! effective roughness length (z0+z0rel)
REAL, DIMENSION(:), INTENT(IN)       :: PQS      ! humidity at surface 
REAL, DIMENSION(:,:), INTENT(IN)     :: PDIR_ALB ! direct albedo for each spectral band
REAL, DIMENSION(:,:), INTENT(IN)     :: PSCA_ALB ! diffuse albedo for each spectral band (-)
REAL, DIMENSION(:,:), INTENT(IN)     :: PDIR_SW  ! direct  solar radiation (on horizontal surf.)
REAL, DIMENSION(:,:), INTENT(IN)     :: PSCA_SW  ! diffuse solar radiation (on horizontal surf.)
REAL, DIMENSION(:), INTENT(IN)       :: PLW      ! longwave radiation (on horizontal surf.)
REAL, DIMENSION(:), INTENT(IN)       :: PRN      ! Surface net radiation
!
REAL, DIMENSION(:), INTENT(IN)       :: PSFZON   ! zonal friction
REAL, DIMENSION(:), INTENT(IN)       :: PSFMER   ! meridian friction
REAL, DIMENSION(:), INTENT(IN)       :: PSFTH    ! heat flux (W/m2)
REAL, DIMENSION(:), INTENT(IN)       :: PSFTQ    ! water flux (kg/m2/s)
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(SIZE(PTA)) :: ZH
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('DIAG_INLINE_ISBA_N',0,ZHOOK_HANDLE)
!
! * Richardson number
!
IF (DGI%N2M>=1) THEN
    PKDI%XP_RI     = PRI        
ENDIF
!
! * Near surface atmospheric variables
!
IF (.NOT. I%LCANOPY) THEN
!        
  IF (DGI%N2M==1) THEN
    CALL PARAM_CLS(PTA, PTS, PQA, PPA, PRHOA, PZONA, PMERA, PHT, PHW,  &
                     PSFTH, PSFTQ, PSFZON, PSFMER,                     &
                     PKDI%XP_T2M, PKDI%XP_Q2M, PKDI%XP_HU2M, PKDI%XP_ZON10M, PKDI%XP_MER10M     )  
  ELSE IF (DGI%N2M==2) THEN
    ZH(:)=2.          
    CALL CLS_TQ(PTA, PQA, PPA, PPS, PHT,           &
                  PCD, PCH, PRI,                   &
                  PTS, PHU, PZ0H, ZH,              &
                  PKDI%XP_T2M, PKDI%XP_Q2M, PKDI%XP_HU2M          )  
    ZH(:)=10.                
    CALL CLS_WIND(PZONA, PMERA, PHW,               &
                    PCD, PCDN, PRI, ZH,            &
                    PKDI%XP_ZON10M, PKDI%XP_MER10M           )  
  END IF
!
ELSE
  !        
  IF (DGI%N2M>=1) THEN
    PKDI%XP_T2M    = XUNDEF
    PKDI%XP_Q2M    = XUNDEF
    PKDI%XP_HU2M   = XUNDEF
    PKDI%XP_ZON10M = XUNDEF
    PKDI%XP_MER10M = XUNDEF
  ENDIF
  !        
ENDIF
!
! * Surface energy budget
!
IF (DGI%LSURF_BUDGET.OR.DGEI%LSURF_BUDGETC) THEN
   !
   CALL DIAG_SURF_BUDGET_ISBA(PDIR_SW, PSCA_SW, PDIR_ALB, PSCA_ALB,  &
                                PLW, PRN,                              &
                                PKDI%XP_SWD, PKDI%XP_SWU, PKDI%XP_SWBD, PKDI%XP_SWBU,      &
                                PKDI%XP_LWD, PKDI%XP_LWU   )          
   !
   PKDI%XP_FMU = PSFZON
   PKDI%XP_FMV = PSFMER
   !
END IF
!
IF (DGI%LCOEF) THEN
  !
  !* Transfer coefficient
  !
  PKDI%XP_CD = PCD
  PKDI%XP_CH = PCH
  PKDI%XP_CE = PCH
  !
  !* Roughness lengths
  !
  PKDI%XP_Z0_WITH_SNOW  = PZ0
  PKDI%XP_Z0H_WITH_SNOW = PZ0H
  PKDI%XP_Z0EFF         = PZ0EFF
  !
ENDIF
!
IF (DGI%LSURF_VARS) THEN
  !
  !* Humidity at surface
  !
  PKDI%XP_QS = PQS
  !
ENDIF
IF (LHOOK) CALL DR_HOOK('DIAG_INLINE_ISBA_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE DIAG_INLINE_ISBA_n
