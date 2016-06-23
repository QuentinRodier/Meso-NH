!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
SUBROUTINE ASSIM_ISBA_UPDATE_SNOW (I, &
                                   HPROGRAM, KI, PSWE, PSWE_ORIG, OINITSNOW, OINC, HTEST )

! ------------------------------------------------------------------------------------------
!  *****************************************************************************************
!
!  Routine to update snow field for ISBA
!  Trygve Aspelien, Separating IO  06/2013
!
!
! ******************************************************************************************
! ------------------------------------------------------------------------------------------
!
USE MODD_ISBA_n, ONLY : ISBA_t
!
USE MODD_CSTS,        ONLY : XTT
USE MODD_SURF_PAR,    ONLY : XUNDEF
USE MODD_SNOW_PAR,    ONLY : XANSMIN, XANSMAX, XRHOSMIN, XRHOSMAX
!
!
USE MODI_ABOR1_SFX
!
USE YOMHOOK,          ONLY : LHOOK,DR_HOOK
USE PARKIND1,         ONLY : JPRB
!
IMPLICIT NONE
!
TYPE(ISBA_t), INTENT(INOUT) :: I
!
 CHARACTER(LEN=6),    INTENT(IN)    :: HPROGRAM  ! program calling surf. schemes
INTEGER,             INTENT(IN)    :: KI
REAL, DIMENSION(KI), INTENT(IN)    :: PSWE
REAL, DIMENSION(KI), INTENT(INOUT) :: PSWE_ORIG
LOGICAL,             INTENT(IN)    :: OINITSNOW
LOGICAL,             INTENT(IN)    :: OINC
 CHARACTER(LEN=2),    INTENT(IN)    :: HTEST     ! must be equal to 'OK'
!
!    Declarations of local variables
!
REAL, DIMENSION(KI) :: ZSWE     ! Snow before update
REAL, DIMENSION(KI) :: ZSWEINC
REAL, DIMENSION(KI) :: ZTS
!    Addtional snow fields with D95 snow scheme 
REAL, DIMENSION(KI) :: ZSNR     ! Snow density 
REAL, DIMENSION(KI) :: ZSNA     ! Snow albedo 
INTEGER  :: JL,JP
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
! ----------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('ASSIM_ISBA_UPDATE_SNOW',0,ZHOOK_HANDLE)
!
IF (HTEST/='OK') THEN
  CALL ABOR1_SFX('ASSIM_ISBA_n: FATAL ERROR DURING ARGUMENT TRANSFER')
END IF
!
IF ( I%TSNOW%SCHEME=='D95' ) THEN
  JL = 1
  JP = 1
  IF ( I%NPATCH > 1 ) CALL ABOR1_SFX("Update of snow is only implemented for D95 and one patch")
ELSE
  CALL ABOR1_SFX("Update of snow is only implemented for D95")
ENDIF
!
IF ( OINITSNOW ) THEN
  !
  PSWE_ORIG(:) = I%TSNOW%WSNOW(:,JL,JP)
  !
  ZTS(:) = I%XTG(:,1,JP)
  !
  ZSWE(:) = PSWE(:)
  ! Set snow=0 where 1. guess = 0 and Ts>0, to avoid that the snow analysis introduce snow where it is no snow.
  WHERE ( PSWE(:)/=XUNDEF .AND. PSWE(:)<1.0E-10 .AND. ZTS(:)>XTT )
    ZSWE(:)   = 0.0
  END WHERE
  !
  I%TSNOW%WSNOW(:,JL,JP) = ZSWE(:)
  !
ENDIF


! Update snow
IF ( OINC ) THEN

  ZSWE(:) = I%TSNOW%WSNOW(:,JL,JP)  
  ZSNA(:) = I%TSNOW%ALB  (:,JP)
  ZSNR(:) = I%TSNOW%RHO  (:,JL,JP)

  ! If we only do second step, we must set working SWE as input SWE
  IF ( .NOT. OINITSNOW ) ZSWE(:) = PSWE(:)
 
  ! Calculate increments
  ZSWEINC(:) = ZSWE(:) - PSWE_ORIG(:)
  WRITE(*,'("  SURFRESERV.NEIGE - min, mean, max: ",3E13.4)') MINVAL(ZSWE),MAXVAL(ZSWE),SUM(ZSWE)/KI
  WRITE(*,*) 'Mean SN increments over NATURE ',SUM(ZSWEINC)/KI

  ! Snow albedo and density are given initial values in points  
  ! which get initial snow in the snow analysis 
  WHERE ( PSWE_ORIG(:) < 1.0E-10 .AND. ZSWE(:)>= 1.0E-10 ) 
    ZSNA(:)    = 0.5 * ( XANSMIN + XANSMAX ) 
    ZSNR(:)    = 0.5 * ( XRHOSMIN + XRHOSMAX )
  END WHERE 
  !
  I%TSNOW%WSNOW(:,JL,JP) = ZSWE(:)
  I%TSNOW%ALB  (:,JP)    = ZSNA(:)
  I%TSNOW%RHO  (:,JL,JP) = ZSNR(:)
  !
ENDIF
!
! -------------------------------------------------------------------------------------
 IF (LHOOK) CALL DR_HOOK('ASSIM_ISBA_UPDATE_SNOW',1,ZHOOK_HANDLE)
 END SUBROUTINE ASSIM_ISBA_UPDATE_SNOW
