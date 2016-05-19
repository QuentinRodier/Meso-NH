!     #########
SUBROUTINE ISBA_BUDGET_INIT(HISBA, HSNOW_ISBA,                   &
                            PWG, PWGI, PWR, PSNOWSWE, PDG, PDZG, &
                            PWG_INI, PWGI_INI, PWR_INI, PSWE_INI )
!     ###############################################################################
!
!!****  *ISBA_BUDGET_INIT * - initialize water and energy budget for ISBA
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
!!      Original    10/2012
!!
!!------------------------------------------------------------------
!
USE MODD_SURF_PAR,   ONLY : XUNDEF
USE MODD_CSTS,       ONLY : XRHOLW
!    
USE MODD_DIAG_EVAP_ISBA_n, ONLY : LWATER_BUDGET
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
 CHARACTER(LEN=*),     INTENT(IN)  :: HISBA      ! type of ISBA version:
!                                               ! '2-L' (default)
!                                               ! '3-L'
!                                               ! 'DIF'
 CHARACTER(LEN=*),     INTENT(IN)  :: HSNOW_ISBA ! 'DEF' = Default F-R snow scheme
!                                               !         (Douville et al. 1995)
!                                               ! '3-L' = 3-L snow scheme (option)
!                                               !         (Boone and Etchevers 2000)
!                                               ! 'CRO' = Crocus snow scheme
!
REAL, DIMENSION(:,:),  INTENT(IN) :: PWG        ! liquid water content by layer  (m3/m3)
REAL, DIMENSION(:,:),  INTENT(IN) :: PWGI       ! ice content by layer           (m3/m3)
REAL, DIMENSION(:),    INTENT(IN) :: PWR        ! liquid water on veg canopy     (kg m-2)
REAL, DIMENSION(:,:),  INTENT(IN) :: PSNOWSWE   ! snow water equivalent by layer (kg m-2)
REAL, DIMENSION(:,:),  INTENT(IN) :: PDG        ! soil layer depth               (m)
REAL, DIMENSION(:,:),  INTENT(IN) :: PDZG       ! soil layer thickness           (m)
!
REAL, DIMENSION(:), INTENT(OUT)   :: PWG_INI    ! total wg at t-1                (kg m-2)
REAL, DIMENSION(:), INTENT(OUT)   :: PWGI_INI   ! total wgi at t-1               (kg m-2)
REAL, DIMENSION(:), INTENT(OUT)   :: PWR_INI    ! total wr at t-1                (kg m-2)
REAL, DIMENSION(:), INTENT(OUT)   :: PSWE_INI   ! total swe at t-1               (kg m-2)
!
!*      0.2    declarations of local variables
!
INTEGER :: INI, INL, INLS
INTEGER :: JI, JL
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('ISBA_BUDGET_INIT',0,ZHOOK_HANDLE)
!
INI =SIZE(PWG,1)
INL =SIZE(PWG,2)
INLS=SIZE(PSNOWSWE,2)
!
PWG_INI (:) = XUNDEF
PWGI_INI(:) = XUNDEF
PSWE_INI(:) = XUNDEF
PWR_INI (:) = XUNDEF
!
!
! * Water budget
!   ------------
!
IF(LWATER_BUDGET)THEN
!
! total wr at t-1
  PWR_INI(:)=PWR(:)
!
! total swe at t-1
  PSWE_INI(:)=0.0
  DO JL=1,INLS
     DO JI=1,INI
        PSWE_INI(JI)=PSWE_INI(JI)+PSNOWSWE(JI,JL)
     ENDDO
  ENDDO
!
! total wg and wgi at t-1
  PWG_INI (:)= 0.0
  PWGI_INI(:)= 0.0
  IF(HISBA=='DIF')THEN
    DO JL=1,INL
       DO JI=1,INI
          IF(PWG(JI,JL)/=XUNDEF)THEN
             PWG_INI (JI)=PWG_INI (JI)+PWG (JI,JL)*PDZG(JI,JL)*XRHOLW
             PWGI_INI(JI)=PWGI_INI(JI)+PWGI(JI,JL)*PDZG(JI,JL)*XRHOLW
          ENDIF
       ENDDO
    ENDDO
  ELSE
    PWG_INI (:)=PWG (:,2)*PDG(:,2)*XRHOLW
    PWGI_INI(:)=PWGI(:,2)*PDG(:,2)*XRHOLW
    IF(HISBA=='3-L')THEN
      PWG_INI(:)=PWG_INI(:)+PWG(:,3)*(PDG(:,3)-PDG(:,2))*XRHOLW
    ENDIF
  ENDIF
!
ENDIF
!
! * Energy budget
!   -------------
!
! not yet implemented
!
!
IF (LHOOK) CALL DR_HOOK('ISBA_BUDGET_INIT',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------------
!
END SUBROUTINE ISBA_BUDGET_INIT
