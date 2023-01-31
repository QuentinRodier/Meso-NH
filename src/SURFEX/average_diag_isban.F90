!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE AVERAGE_DIAG_ISBA_n (IO, DGO, D, DC, ND, NDC, NP, &
                                      PHW, PHT ,PSFCO2             )
!     #######################################
!
!
!!****  *AVERAGE_DIAG_ISBA_n*  
!!
!!    PURPOSE
!!    -------
!      Average the diagnostics from all ISBA tiles
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
!!	S. Belair           * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    10/03/95 
!!      V.Masson    20/03/96  remove abnormal averages and average TS**4 instead
!!                            of TS
!!      (J.Stein)   27/03/96 use only H and LE in the soil scheme
!!      A. Boone    27/11/02 revised to output ALMA variables, and general applications
!!      B. Decharme 17/08/09 cumulative radiatif budget
!!      V. Masson   10/2013  coherence between canopy and min/max T2M diagnostics
!!      B. Decharme    04/13 Averaged Trad already done in average_diag.F90
!!                           Good dimension for CO2 flux
!!      P. Samuelsson  10/13 Added min max for XT2M
!!      B. Decharme    02/15 No dependence on HW for 10M Wind diags
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_ISBA_OPTIONS_n, ONLY : ISBA_OPTIONS_t
USE MODD_DIAG_n,         ONLY : DIAG_t, DIAG_NP_t, DIAG_OPTIONS_t
USE MODD_ISBA_n,         ONLY : ISBA_P_t, ISBA_NP_t
!
USE MODD_SURF_PAR, ONLY : XUNDEF
!
USE YOMHOOK, ONLY : LHOOK, DR_HOOK
USE PARKIND1,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
TYPE(ISBA_OPTIONS_t), INTENT(INOUT) :: IO
TYPE(DIAG_OPTIONS_t), INTENT(INOUT) :: DGO
TYPE(DIAG_t),         INTENT(INOUT) :: D
TYPE(DIAG_t),         INTENT(INOUT) :: DC
TYPE(DIAG_NP_t),      INTENT(INOUT) :: ND
TYPE(DIAG_NP_t),      INTENT(INOUT) :: NDC
TYPE(ISBA_NP_t),      INTENT(INOUT) :: NP
!
REAL, DIMENSION(:), INTENT(IN)       :: PHW    ! atmospheric level height for wind (m)
REAL, DIMENSION(:), INTENT(IN)       :: PHT    ! atmospheric level height (m)
REAL, DIMENSION(:), INTENT(IN)       :: PSFCO2 ! CO2 flux (m/s*kg_CO2/kg_air)
!
!*      0.2    declarations of local variables
!
TYPE(DIAG_t),   POINTER :: DK
TYPE(ISBA_P_t), POINTER :: PK
!
INTEGER         :: INP, ISIZE_LMEB_PATCH
INTEGER         :: JP, JI, IMASK ! tile loop counter
INTEGER         :: JSWB   ! band loop counter
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('AVERAGE_DIAG_ISBA_N',0,ZHOOK_HANDLE)
!
!       0.     Initialization
!              --------------
!
INP = IO%NPATCH
!
ISIZE_LMEB_PATCH = COUNT(IO%LMEB_PATCH(:))
!
!       1.     Energy fluxes
!              -------------
!
IF (DGO%LSURF_BUDGET) THEN
  !
  CALL MAKE_AVERAGE(D,ND)
  !
  D%XSWBD(:,:) = 0.
  D%XSWBU(:,:) = 0.
  !
  DO JP=1,INP
    DK => ND%AL(JP)
    PK => NP%AL(JP)
    DO JI = 1,PK%NSIZE_P
      IMASK = PK%NR_P(JI)
      DO JSWB =1,SIZE(D%XSWBD,2)
        !
        ! Downwards SW radiation for each spectral band
        D%XSWBD(IMASK,JSWB) = D%XSWBD(IMASK,JSWB) + PK%XPATCH(JI) * DK%XSWBD(JI,JSWB)
        !
        ! Upwards SW radiation for each spectral band
        D%XSWBU(IMASK,JSWB) = D%XSWBU(IMASK,JSWB) + PK%XPATCH(JI) * DK%XSWBU(JI,JSWB)
        !
      END DO
    ENDDO
  END DO
  !
END IF
!
IF (DGO%LSURF_BUDGETC) THEN
  !
  CALL MAKE_AVERAGE(DC,NDC)
  !
ENDIF    
!
!       2.     surface temperature and 2 meters parameters
!              -------------------------------------------
!
D%XTS  (:) = 0.0
D%XALBT(:) = 0.0
DO JP=1,INP
  DK => ND%AL(JP)
  PK => NP%AL(JP)
  DO JI = 1,PK%NSIZE_P
    IMASK = PK%NR_P(JI)
    !Surface temperature
    D%XTS(IMASK) = D%XTS(IMASK) + PK%XPATCH(JI) * DK%XTS(JI)
    !Total albedo
    D%XALBT(IMASK) = D%XALBT(IMASK) + PK%XPATCH(JI) * DK%XALBT(JI)
  ENDDO
END DO
!
! No albedo during night with MEB
IF(ISIZE_LMEB_PATCH>0)THEN
  DO JP=1,INP
     PK => NP%AL(JP)
     DO JI = 1,PK%NSIZE_P
        IMASK = PK%NR_P(JI)
        IF(PK%XPATCH(JI)>0.0.AND.DK%XALBT(JI)==XUNDEF)THEN
          D%XALBT(IMASK) = XUNDEF
        ENDIF
     ENDDO
  ENDDO
ENDIF
!
IF (.NOT. IO%LCANOPY .AND. DGO%N2M>=1) THEN

  D%XT2M(:)  = 0.
  D%XQ2M(:)  = 0.
  D%XHU2M(:)  = 0.
  !
  DO JP=1,INP
    DK => ND%AL(JP)
    PK => NP%AL(JP)
    DO JI = 1,PK%NSIZE_P
      IMASK = PK%NR_P(JI) 
      !
      ! 2 meters temperature
      D%XT2M(IMASK) = D%XT2M(IMASK) + PK%XPATCH(JI) * DK%XT2M(JI)
      !
      ! 2 meters humidity
      D%XQ2M(IMASK) = D%XQ2M(IMASK) + PK%XPATCH(JI) * DK%XQ2M(JI)
      !
      ! 2 meters relative humidity
      D%XHU2M(IMASK) = D%XHU2M(IMASK) + PK%XPATCH(JI) * DK%XHU2M(JI)
      ! 
    ENDDO
  END DO
  !
  ! 10 meters wind
  !
  D%XZON10M (:)  = 0.
  D%XMER10M (:)  = 0.
  D%XWIND10M(:)  = 0.
  DO JP=1,INP
    DK => ND%AL(JP)
    PK => NP%AL(JP)
    DO JI = 1,PK%NSIZE_P
      IMASK = PK%NR_P(JI) 
      D%XZON10M (IMASK) = D%XZON10M (IMASK) + PK%XPATCH(JI) * DK%XZON10M (JI)
      D%XMER10M (IMASK) = D%XMER10M (IMASK) + PK%XPATCH(JI) * DK%XMER10M (JI)
      D%XWIND10M(IMASK) = D%XWIND10M(IMASK) + PK%XPATCH(JI) * DK%XWIND10M(JI)
    ENDDO
  ENDDO
  !
  ! min and max of XT2M
  !
  DO JP=1,INP
    DK => ND%AL(JP)
    DK%XT2M_MIN(:) = MIN(DK%XT2M_MIN(:),DK%XT2M(:))
    DK%XT2M_MAX(:) = MAX(DK%XT2M_MAX(:),DK%XT2M(:))
  ENDDO
  !  
  D%XT2M_MIN(:) = MIN(D%XT2M_MIN(:),D%XT2M(:))
  D%XT2M_MAX(:) = MAX(D%XT2M_MAX(:),D%XT2M(:))
  !
  D%XHU2M_MIN(:) = MIN(D%XHU2M_MIN(:),D%XHU2M(:))
  D%XHU2M_MAX(:) = MAX(D%XHU2M_MAX(:),D%XHU2M(:))
  !
  D%XWIND10M_MAX(:) = MAX(D%XWIND10M_MAX(:),D%XWIND10M(:))
  !
  ! Calculation of mean fields
  !
  D%NCOUNT_STEP = D%NCOUNT_STEP + 1
  !
  D%XT2M_MEAN    (:) = D%XT2M_MEAN    (:) + D%XT2M(:)
  D%XQ2M_MEAN    (:) = D%XQ2M_MEAN    (:) + D%XQ2M(:)
  D%XHU2M_MEAN   (:) = D%XHU2M_MEAN   (:) + D%XHU2M(:)
  D%XZON10M_MEAN (:) = D%XZON10M_MEAN (:) + D%XZON10M(:)
  D%XMER10M_MEAN (:) = D%XMER10M_MEAN (:) + D%XMER10M(:)
  !
END IF
!
! Richardson number
!
IF (DGO%N2M>=1) THEN
  !
  D%XRI   (:) = 0.
  D%XARES (:) = 0.
  D%XSFCO2(:) = PSFCO2(:)
  !
  DO JP=1,INP
    DK => ND%AL(JP)
    PK => NP%AL(JP)
    DO JI = 1,PK%NSIZE_P
      IMASK = PK%NR_P(JI)    
      D%XRI  (IMASK) = D%XRI  (IMASK) + PK%XPATCH(JI) * DK%XRI  (JI)
      D%XARES(IMASK) = D%XARES(IMASK) + PK%XPATCH(JI) / DK%XARES(JI)
    ENDDO
  END DO
  !
  D%XARES(:) = 1.0/D%XARES(:)
  !
END IF
!
!       3.     Transfer coefficients
!              ---------------------
!
IF (DGO%LCOEF) THEN
  !
  D%XCD   (:) = 0.
  D%XCH   (:) = 0.
  D%XCE   (:) = 0.
  D%XZ0   (:) = 0.
  D%XZ0H  (:) = 0.
  D%XZ0EFF(:) = 0.
  !
  DO JP=1,INP
    DK => ND%AL(JP)
    PK => NP%AL(JP)
    DO JI = 1,PK%NSIZE_P
      IMASK = PK%NR_P(JI)    
      !
      D%XCD(IMASK)  = D%XCD(IMASK) + PK%XPATCH(JI) * DK%XCD(JI)
      D%XCH(IMASK)  = D%XCH(IMASK) + PK%XPATCH(JI) * DK%XCH(JI)
      D%XCE(IMASK)  = D%XCE(IMASK) + PK%XPATCH(JI) * DK%XCE(JI)
      !            
      D%XZ0   (IMASK) = D%XZ0   (IMASK) + PK%XPATCH(JI) * 1./(LOG(PHW(IMASK)/DK%XZ0   (JI)))**2  
      D%XZ0H  (IMASK) = D%XZ0H  (IMASK) + PK%XPATCH(JI) * 1./(LOG(PHT(IMASK)/DK%XZ0H  (JI)))**2   
      D%XZ0EFF(IMASK) = D%XZ0EFF(IMASK) + PK%XPATCH(JI) * 1./(LOG(PHW(IMASK)/DK%XZ0EFF(JI)))**2
      !      
    ENDDO
  END DO
  !
  D%XZ0(:)    = PHW(:) *  EXP( - SQRT(1./D%XZ0(:)) )
  !
  D%XZ0H(:)   = PHT(:) *  EXP( - SQRT(1./D%XZ0H(:)) )
  !
  D%XZ0EFF(:) = PHW(:) *  EXP( - SQRT(1./D%XZ0EFF(:)) )
  !
END IF
!
IF (DGO%LSURF_VARS) THEN
  D%XQS(:)  = 0.
  !
  DO JP=1,INP
    DK => ND%AL(JP)
    PK => NP%AL(JP)
    DO JI = 1,PK%NSIZE_P  
      IMASK = PK%NR_P(JI)    
      !
      ! specific humidity at surface
      D%XQS(IMASK) = D%XQS(IMASK) + PK%XPATCH(JI) * DK%XQS(JI)
      !
    ENDDO
  END DO
END IF
!
IF (LHOOK) CALL DR_HOOK('AVERAGE_DIAG_ISBA_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
CONTAINS
!-------------------------------------------------------------------------------
!
SUBROUTINE MAKE_AVERAGE(DA,NDA)
!
TYPE(DIAG_t),    INTENT(INOUT) :: DA
TYPE(DIAG_NP_t), INTENT(INOUT) :: NDA
!
TYPE(DIAG_t), POINTER :: DAK
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('AVERAGE_DIAG_ISBA_N:MAKE_AVERAGE',0,ZHOOK_HANDLE)
!
DA%XRN   (:) = 0.
DA%XH    (:) = 0.
DA%XLE   (:) = 0.
DA%XLEI  (:) = 0.
DA%XGFLUX(:) = 0.
DA%XQF   (:) = 0.
!
DA%XSWD(:) = 0.
DA%XSWU(:) = 0.
DA%XLWD(:) = 0.
DA%XLWU(:) = 0.
DA%XFMU(:) = 0.
DA%XFMV(:) = 0.
!
DA%XEVAP (:) = 0.
DA%XSUBL (:) = 0.
!
DO JP=1,INP
  !
  DAK => NDA%AL(JP)
  PK  => NP%AL(JP)
  !
  DO JI = 1,PK%NSIZE_P 
    IMASK = PK%NR_P(JI) 
    !
    ! Net radiation
    DA%XRN   (IMASK) = DA%XRN   (IMASK) + PK%XPATCH(JI) * DAK%XRN(JI)
    !
    ! Sensible heat flux
    DA%XH    (IMASK) = DA%XH    (IMASK) + PK%XPATCH(JI) * DAK%XH(JI)
    !
    ! Total latent heat flux
    DA%XLE   (IMASK) = DA%XLE   (IMASK) + PK%XPATCH(JI) * DAK%XLE(JI)
    !
    ! Storage flux
    DA%XGFLUX(IMASK) = DA%XGFLUX(IMASK) + PK%XPATCH(JI) * DAK%XGFLUX(JI)
    !
    ! Total surface sublimation
    DA%XLEI  (IMASK) = DA%XLEI  (IMASK) + PK%XPATCH(JI) * DAK%XLEI(JI)          
    !
    ! Evapotranspiration
    DA%XEVAP (IMASK) = DA%XEVAP (IMASK) + PK%XPATCH(JI) * DAK%XEVAP(JI)
    !
    !  Sublimation
    DA%XSUBL (IMASK) = DA%XSUBL (IMASK) + PK%XPATCH(JI) * DAK%XSUBL(JI)
    !
    !  Downwards SW radiation
    DA%XSWD  (IMASK) = DA%XSWD  (IMASK) + PK%XPATCH(JI) * DAK%XSWD(JI)
    !
    !    Upwards SW radiation
    DA%XSWU  (IMASK) = DA%XSWU  (IMASK) + PK%XPATCH(JI) * DAK%XSWU(JI)
    !
    !    Downwards LW radiation
    DA%XLWD  (IMASK) = DA%XLWD  (IMASK) + PK%XPATCH(JI) * DAK%XLWD(JI)
    !
    !    Upwards LW radiation
    DA%XLWU  (IMASK) = DA%XLWU  (IMASK) + PK%XPATCH(JI) * DAK%XLWU(JI)
    !
    !    Zonal wind stress
    DA%XFMU  (IMASK) = DA%XFMU  (IMASK) + PK%XPATCH(JI) * DAK%XFMU(JI)
    !
    !    Meridian wind stress
    DA%XFMV  (IMASK) = DA%XFMV  (IMASK) + PK%XPATCH(JI) * DAK%XFMV(JI)
    !
  ENDDO
  !
END DO
!
IF (LHOOK) CALL DR_HOOK('AVERAGE_DIAG_ISBA_N:MAKE_AVERAGE',1,ZHOOK_HANDLE)
!
END SUBROUTINE MAKE_AVERAGE
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE AVERAGE_DIAG_ISBA_n
