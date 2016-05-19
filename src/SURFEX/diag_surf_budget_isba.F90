!     #########
       SUBROUTINE DIAG_SURF_BUDGET_ISBA (PDIR_SW, PSCA_SW, PDIR_ALB, PSCA_ALB, &
                                          PLW, PRN,                              &
                                          PSWD, PSWU, PSWBD, PSWBU, PLWD, PLWU   )  
!     ###############################################################################
!
!!****  *DIAG_SURF_BUDGET_ISBA * - Computes diagnostics over ISBA
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
!!     P. Le Moigne 
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    04/2006
!!      Modified    08/2008 (B. Decharme) LWU diag
!!------------------------------------------------------------------
!
USE MODD_CSTS,           ONLY : XSTEFAN
!
! 
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, DIMENSION(:,:),INTENT(IN)  :: PDIR_SW   ! direct  solar radiation (on horizontal surf.)
!                                             !                                       (W/m2)
REAL, DIMENSION(:,:),INTENT(IN)  :: PSCA_SW   ! diffuse solar radiation (on horizontal surf.)
!                                             !                                       (W/m2)
REAL, DIMENSION(:), INTENT(IN)   :: PLW       ! longwave radiation (on horizontal surf.)
REAL, DIMENSION(:,:),INTENT(IN)  :: PDIR_ALB  ! direct albedo for each spectral band  (-)
REAL, DIMENSION(:,:),INTENT(IN)  :: PSCA_ALB  ! diffuse albedo for each spectral band (-)
REAL, DIMENSION(:), INTENT(IN)   :: PRN       ! Surface net radiation
!
REAL, DIMENSION(:,:), INTENT(OUT):: PSWBD     ! incoming short wave radiation by spectral band (W/m2)
REAL, DIMENSION(:,:), INTENT(OUT):: PSWBU     ! upward  short wave radiation by spectral band (W/m2)
REAL, DIMENSION(:), INTENT(OUT)  :: PSWD      ! total incoming short wave radiation (W/m2)
REAL, DIMENSION(:), INTENT(OUT)  :: PSWU      ! total upward short wave radiation (W/m2)
REAL, DIMENSION(:), INTENT(OUT)  :: PLWD      ! Downward long wave radiation (W/m2)
REAL, DIMENSION(:), INTENT(OUT)  :: PLWU      ! upward long wave radiation (W/m2)  
!
!
!*      0.2    declarations of local variables
!
INTEGER                          :: ISWB      ! number of SW bands
INTEGER                          :: JSWB      ! loop counter on number of SW bands
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------------
!
!
IF (LHOOK) CALL DR_HOOK('DIAG_SURF_BUDGET_ISBA',0,ZHOOK_HANDLE)
ISWB = SIZE(PDIR_SW,2)
! 
!* total incoming and outgoing SW
!
DO JSWB=1,ISWB
  PSWBD(:,JSWB) = PDIR_SW(:,JSWB)                    + PSCA_SW(:,JSWB)
  PSWBU(:,JSWB) = PDIR_SW(:,JSWB) * PDIR_ALB(:,JSWB) + PSCA_SW(:,JSWB) * PSCA_ALB(:,JSWB) 
ENDDO
!
PSWD(:) = 0.
PSWU(:) = 0.
DO JSWB=1,ISWB
   PSWD(:)=PSWD(:)+PSWBD(:,JSWB)
   PSWU(:)=PSWU(:)+PSWBU(:,JSWB)
ENDDO
!
!*incoming outgoing LW
!
!Wrong old diag : LWU=EMIS*STEFAN*Ts**4 + (1.-EMIS)*LW
!Due to e_budget.f90 linearization, LWU can not be calculated using actual Ts
!
PLWD(:)=PLW(:)
PLWU(:)=PSWD(:)-PSWU(:)+PLWD(:)-PRN(:)
IF (LHOOK) CALL DR_HOOK('DIAG_SURF_BUDGET_ISBA',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE DIAG_SURF_BUDGET_ISBA
