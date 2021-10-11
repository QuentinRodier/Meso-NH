!SFX_LIC Copyright 1994-2018 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE WRITESURF_SBL_n (HSELECT, OSBL, SB, HPROGRAM, HWRITE, HSURF,SV)
!     ####################################
!
!!****  *WRITE_FLAKE_n* - writes FLAKE fields
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
!!      V. Masson   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2003 
!!      E. Martin   01/2012 avoid write of XUNDEF fields
!!      P. Wautelet 28/05/2018: check if SV is present before using it
!!      R. Schoetter 2019 addds time averaged fields
!!      V. Masson   04/2020 introduces GRESET
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_CANOPY_n, ONLY : CANOPY_t
USE MODD_SV_n, ONLY : SV_t
USE MODD_SURF_PAR, ONLY: LEN_HREC
!
#ifdef SFX_ARO
USE MODD_IO_SURF_ARO,   ONLY : NBLOCK
#endif
!
#ifdef SFX_OL
USE MODD_IO_SURF_OL, ONLY : LRESET_DIAG_ol=>LRESET_DIAG
#endif
!
#ifdef SFX_NC
USE MODD_IO_SURF_NC, ONLY : LRESET_DIAG_nc=>LRESET_DIAG
#endif
!
USE MODI_WRITE_SURF
USE MODI_END_IO_SURF_n
USE MODI_INIT_IO_SURF_n
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
 CHARACTER(LEN=*), DIMENSION(:), INTENT(IN) :: HSELECT 
 LOGICAL, INTENT(IN) :: OSBL
!
TYPE(CANOPY_t), INTENT(INOUT) :: SB
TYPE(SV_t), INTENT(IN),OPTIONAL :: SV
!
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! program calling
 CHARACTER(LEN=3),    INTENT(IN)  :: HWRITE    ! 'PREP' : does not write SBL XUNDEF fields
!                                             ! 'ALL' : all fields are written
 CHARACTER(LEN=6), INTENT(IN) :: HSURF
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER           :: IRESP          ! IRESP  : return-code if a problem appears
 !
 CHARACTER(LEN=7) :: YBASE
 CHARACTER(LEN=LEN_HREC) :: YRECFM         ! Name of the article to be read
 CHARACTER(LEN=13) :: YFORMAT 
 CHARACTER(LEN=13) :: YFORMATM
 CHARACTER(LEN=100):: YCOMMENT       ! Comment string
LOGICAL           :: GRESET
!
INTEGER :: JL,JN  ! loop counter on layers
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
GRESET=.TRUE.
#ifdef SFX_ARO
GRESET=(NBLOCK>0)
#endif
#ifdef SFX_OL
IF (.NOT. LRESET_DIAG_ol) GRESET = .FALSE.
#endif
!
#ifdef SFX_NC
IF (.NOT. LRESET_DIAG_nc) GRESET = .FALSE.
#endif
!
!*       1.     Prognostic fields:
!               -----------------
!
!* flag to define if SBL is computed
!
IF (LHOOK) CALL DR_HOOK('WRITESURF_SBL_N',0,ZHOOK_HANDLE)
!
IF (HSURF=="TOWN  ") THEN
  YRECFM='TEB_CANOPY'
ELSEIF (HSURF=="WATER ") THEN
  YRECFM='WAT_SBL'
ELSEIF (HSURF=="NATURE") THEN
  YRECFM='ISBA_CANOPY'
ELSEIF (HSURF=="SEA   ") THEN
  YRECFM='SEA_SBL'
ENDIF
YCOMMENT='flag to use SBL levels'
 CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,OSBL,IRESP,HCOMMENT=YCOMMENT)
!
IF (.NOT. OSBL .AND. LHOOK) CALL DR_HOOK('WRITESURF_SBL_N',1,ZHOOK_HANDLE)
IF (.NOT. OSBL) RETURN
!
IF (HSURF=="TOWN  ") THEN
  YBASE = "TEB_CAN"
ELSEIF (HSURF=="WATER ") THEN
  YBASE = "WAT_SBL"
ELSEIF (HSURF=="NATURE") THEN
  YBASE = "ISB_CAN"
ELSEIF (HSURF=="SEA   ") THEN
  YBASE = "SEA_SBL"
ENDIF
!
YFORMAT='(A9,I2.2) '
YFORMATM='(A10,I2.2) '
!
!* number of levels
!
YRECFM=TRIM(YBASE)//'_LVL'
YCOMMENT='number of SBL levels'
 CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,SB%NLVL,IRESP,HCOMMENT=YCOMMENT)
!
!* altitudes
!
DO JL=1,SB%NLVL
  WRITE(YRECFM,YFORMAT) TRIM(YBASE)//'_Z',JL
  YCOMMENT='altitudes of SBL levels (m)'
  CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,SB%XZ(:,JL),IRESP,HCOMMENT=YCOMMENT)
END DO
!
IF (HWRITE/='PRE') THEN
  !
  !* wind in SBL
  !
  DO JL=1,SB%NLVL
    WRITE(YRECFM,YFORMAT) TRIM(YBASE)//'_U',JL
    YCOMMENT='wind at SBL levels (m/s)'
    CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,SB%XU(:,JL),IRESP,HCOMMENT=YCOMMENT)
  END DO
  !
  !* Mean wind in SBL
  !
  IF (SB%NCOUNT_STEP.GE.1) THEN
     DO JL=1,SB%NLVL
        WRITE(YRECFM,YFORMATM) TRIM(YBASE)//'_UM',JL
        YCOMMENT='Mean wind at canopy levels (m/s)'
        CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,SB%XU_MEAN(:,JL)/SB%NCOUNT_STEP,IRESP,HCOMMENT=YCOMMENT)
     END DO
     IF (GRESET) SB%XU_MEAN(:,:) = 0.0
  ENDIF
  !
  !* temperature in SBL
  !
  DO JL=1,SB%NLVL
    WRITE(YRECFM,YFORMAT) TRIM(YBASE)//'_T',JL
    YCOMMENT='temperature at SBL levels (K)'
    CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,SB%XT(:,JL),IRESP,HCOMMENT=YCOMMENT)
  END DO
  !
  !* Mean temperature in SBL
  !
  IF (SB%NCOUNT_STEP.GE.1) THEN
     DO JL=1,SB%NLVL
        WRITE(YRECFM,YFORMATM) TRIM(YBASE)//'_TM',JL
        YCOMMENT='Mean temperature at canopy levels (m/s)'
        CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,SB%XT_MEAN(:,JL)/SB%NCOUNT_STEP,IRESP,HCOMMENT=YCOMMENT)
     END DO
     IF (GRESET) SB%XT_MEAN(:,:) = 0.0
  ENDIF
  !
  !* humidity in SBL
  !
  DO JL=1,SB%NLVL
    WRITE(YRECFM,YFORMAT) TRIM(YBASE)//'_Q',JL
    YCOMMENT='humidity at SBL levels (kg/m3)'
    CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,SB%XQ(:,JL),IRESP,HCOMMENT=YCOMMENT)
  END DO
  !
  !* Mean humidity in SBL
  !
  IF (SB%NCOUNT_STEP.GE.1) THEN
     DO JL=1,SB%NLVL
        WRITE(YRECFM,YFORMATM) TRIM(YBASE)//'_QM',JL
        YCOMMENT='Mean  humidity at canopy levels (kg/m3)'
        CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,SB%XQ_MEAN(:,JL)/SB%NCOUNT_STEP,IRESP,HCOMMENT=YCOMMENT)
     END DO
     IF (GRESET) SB%XQ_MEAN(:,:) = 0.0
  ENDIF
  !
  !* Mean temperature in SBL
  !
  IF (SB%NCOUNT_STEP.GE.1) THEN
     DO JL=1,SB%NLVL
        WRITE(YRECFM,YFORMATM) TRIM(YBASE)//'_RM',JL
        YCOMMENT='Mean relative humidity at canopy levels (1)'
        CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,SB%XRH_MEAN(:,JL)/SB%NCOUNT_STEP,IRESP,HCOMMENT=YCOMMENT)
     END DO
     IF (GRESET) SB%XRH_MEAN(:,:) = 0.0
  ENDIF
  !
  !* Tke in SBL
  !
  DO JL=1,SB%NLVL
    WRITE(YRECFM,YFORMAT) TRIM(YBASE)//'_E',JL
    YCOMMENT='Tke at SBL levels (m2/s2)'
    CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,SB%XTKE(:,JL),IRESP,HCOMMENT=YCOMMENT)
  END DO
  !
  !* Monin-Obhukov length
  !
  IF (HSURF=="TOWN  ") THEN
    !
    DO JL=1,SB%NLVL
      WRITE(YRECFM,'(A10,I2.2)') TRIM(YBASE)//'_MO',JL
      YCOMMENT='Monin-Obukhov length (m)'
      CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,SB%XLMO(:,JL),IRESP,HCOMMENT=YCOMMENT)
    END DO 
    !  
    !* mixing length
    !
    IF (ASSOCIATED(SB%XLM)) THEN
      DO JL=1,SB%NLVL
        WRITE(YRECFM,'(A10,I2.2)') TRIM(YBASE)//'_LM',JL
        YCOMMENT='mixing length (m)'
        CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,SB%XLM(:,JL),IRESP,HCOMMENT=YCOMMENT)
     END DO
    END IF
    !
    !* dissipative length
    !
    IF (ASSOCIATED(SB%XLEPS)) THEN
      DO JL=1,SB%NLVL
        WRITE(YRECFM,'(A10,I2.2)') TRIM(YBASE)//'_LE',JL
        YCOMMENT='mixing length (m)'
        CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,SB%XLEPS(:,JL),IRESP,HCOMMENT=YCOMMENT)
      END DO
    END IF 
    !   
  ELSE
    YRECFM=TRIM(YBASE)//'_LMO     '
    CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,SB%XLMO(:,SB%NLVL),IRESP,HCOMMENT=YCOMMENT)
  ENDIF
  !
  !* Air pressure in SBL
  !
  DO JL=1,SB%NLVL
    WRITE(YRECFM,YFORMAT) TRIM(YBASE)//'_P',JL
    YCOMMENT='Pressure at SBL levels (Pa)'
    CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,SB%XP(:,JL),IRESP,HCOMMENT=YCOMMENT)
  END DO
  !
  !* Mean pressure in SBL
  !
  IF (SB%NCOUNT_STEP.GE.1) THEN
     DO JL=1,SB%NLVL
        WRITE(YRECFM,YFORMATM) TRIM(YBASE)//'_PM',JL
        YCOMMENT='Mean pressure at canopy levels (Pa)'
        CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,SB%XP_MEAN(:,JL)/SB%NCOUNT_STEP,IRESP,HCOMMENT=YCOMMENT)
     END DO
     IF (GRESET) SB%XP_MEAN(:,:) = 0.0
  ENDIF
  !
  ! Set time step counter to 0.0
  !
  IF (GRESET) SB%NCOUNT_STEP = 0
  !
  IF (HSURF=="NATURE" .AND. PRESENT(SV)) THEN
    IF(SV%NSNWEQ>0) THEN
      DO JN=1,SV%NSNWEQ
      !DO JN=1,2
        DO JL=1,SB%NLVL
          WRITE(YRECFM,'(A8,I1.1,A1,I2.2)') 'CANSNW_M',JN,'L',JL
          YCOMMENT='Blown snow variables at canopy levels (__ /kg)'
          CALL WRITE_SURF(HSELECT,HPROGRAM,YRECFM,SB%XBLOWSNW(:,JL,JN),IRESP,HCOMMENT=YCOMMENT)
        END DO
      END DO
    ENDIF
  ENDIF           
ENDIF
!
IF (LHOOK) CALL DR_HOOK('WRITESURF_SBL_N',1,ZHOOK_HANDLE)
!
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE WRITESURF_SBL_n
