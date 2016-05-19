!     #########
SUBROUTINE ISBA_BUDGET(HISBA, HSNOW_ISBA, OGLACIER, PTSTEP,    &
                       PWG, PWGI, PWR, PSNOWSWE, PDG, PDZG,    &
                       PWG_INI, PWGI_INI, PWR_INI, PSWE_INI,   & 
                       PRAIN, PSNOW, PEVAP, PDRAIN, PRUNOFF,   &
                       PIFLOOD, PPFLOOD, PICEFLUX, PIRRIG_FLUX,&
                       PDWG, PDWGI, PDWR, PDSWE, PWATBUD       )
!     ###############################################################################
!
!!****  *ISBA_BUDGET * - water and energy budget for ISBA
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
!!      Original    07/2012
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
 CHARACTER(LEN=*),      INTENT(IN) :: HISBA      ! type of ISBA version:
!                                               ! '2-L' (default)
!                                               ! '3-L'
!                                               ! 'DIF'
 CHARACTER(LEN=*),     INTENT(IN)  :: HSNOW_ISBA ! 'DEF' = Default F-R snow scheme
!                                               !         (Douville et al. 1995)
!                                               ! '3-L' = 3-L snow scheme (option)
!                                               !         (Boone and Etchevers 2000)
!                                               ! 'CRO' = Crocus snow scheme
LOGICAL,               INTENT(IN) :: OGLACIER   ! True = glacier over permanent snow and ice
                                                ! False = No specific treatment
REAL,                  INTENT(IN) :: PTSTEP     ! timestep of the integration    (s)
!
REAL, DIMENSION(:,:),  INTENT(IN) :: PWG        ! liquid water content by layer  (m3/m3)
REAL, DIMENSION(:,:),  INTENT(IN) :: PWGI       ! ice content by layer           (m3/m3)
REAL, DIMENSION(:),    INTENT(IN) :: PWR        ! liquid water on veg canopy     (kg m-2)
REAL, DIMENSION(:,:),  INTENT(IN) :: PSNOWSWE   ! snow water equivalent by layer (kg m-2)
REAL, DIMENSION(:,:),  INTENT(IN) :: PDG        ! soil layer depth               (m)
REAL, DIMENSION(:,:),  INTENT(IN) :: PDZG       ! soil layer thickness           (m)
!
REAL, DIMENSION(:),    INTENT(IN) :: PWG_INI    ! total wg at t-1                (kg m-2)
REAL, DIMENSION(:),    INTENT(IN) :: PWGI_INI   ! total wgi at t-1               (kg m-2)
REAL, DIMENSION(:),    INTENT(IN) :: PWR_INI    ! total wr at t-1                (kg m-2)
REAL, DIMENSION(:),    INTENT(IN) :: PSWE_INI   ! total swe at t-1               (kg m-2)
!
REAL, DIMENSION(:),    INTENT(IN)  :: PRAIN     ! Rain rate                      (kg/m2/s)
REAL, DIMENSION(:),    INTENT(IN)  :: PSNOW     ! Snow rate                      (kg/m2/s)
REAL, DIMENSION(:),    INTENT(IN)  :: PEVAP     ! total evaporative flux         (kg/m2/s)
REAL, DIMENSION(:),    INTENT(IN)  :: PDRAIN    ! drainage                       (kg/m2/s)
REAL, DIMENSION(:),    INTENT(IN)  :: PRUNOFF   ! surface runoff                 (kg/m2/s)
REAL, DIMENSION(:),    INTENT(IN)  :: PIFLOOD   ! Floodplain infiltration        (kg/m2/s)
REAL, DIMENSION(:),    INTENT(IN)  :: PPFLOOD   ! Floodplain direct precip runoff(kg/m2/s)
REAL, DIMENSION(:),    INTENT(IN)  :: PICEFLUX  ! Ice flux from Snow reservoir   (kg/m2/s)
REAL ,DIMENSION(:),    INTENT(IN)  :: PIRRIG_FLUX! additional water flux from irrigation (kg/m2/s)
! 
REAL, DIMENSION(:),    INTENT(OUT) :: PDWG
REAL, DIMENSION(:),    INTENT(OUT) :: PDWGI
REAL, DIMENSION(:),    INTENT(OUT) :: PDWR
REAL, DIMENSION(:),    INTENT(OUT) :: PDSWE
REAL, DIMENSION(:),    INTENT(OUT) :: PWATBUD   ! ISBA water budget              (kg/m2/s)
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(SIZE(PWR)) :: ZINPUT
REAL, DIMENSION(SIZE(PWR)) :: ZOUTPUT
REAL, DIMENSION(SIZE(PWR)) :: ZTENDENCY
REAL, DIMENSION(SIZE(PWR)) :: ZICEFLUX
REAL, DIMENSION(SIZE(PWR)) :: ZSWE_T
REAL, DIMENSION(SIZE(PWR)) :: ZWG_T
REAL, DIMENSION(SIZE(PWR)) :: ZWGI_T
!
INTEGER :: INI, INL, INLS
INTEGER :: JI, JL
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('ISBA_BUDGET',0,ZHOOK_HANDLE)
!
!*      1.0    Init
!       -----------
!
INI =SIZE(PWG,1)
INL =SIZE(PWG,2)
INLS=SIZE(PSNOWSWE,2)
!
PDWG (:) = XUNDEF
PDWGI(:) = XUNDEF
PDWR (:) = XUNDEF
PDSWE(:) = XUNDEF
!
!
!*      2.0    Comptut isba water budget in kg/m2/s
!       -------------------------------------------
!
IF(LWATER_BUDGET)THEN
!
! total swe at t in kg/m2
  ZSWE_T(:)=0.0
  DO JL=1,INLS
     DO JI=1,INI
        ZSWE_T(JI)=ZSWE_T(JI)+PSNOWSWE(JI,JL)
     ENDDO
  ENDDO
!
! total wg and wgi at t in kg/m2
  ZWG_T (:)= 0.0
  ZWGI_T(:)= 0.0
  IF(HISBA=='DIF')THEN
    DO JL=1,INL
       DO JI=1,INI
          IF(PWG(JI,JL)/=XUNDEF)THEN
             ZWG_T (JI)=ZWG_T (JI)+PWG (JI,JL)*PDZG(JI,JL)*XRHOLW
             ZWGI_T(JI)=ZWGI_T(JI)+PWGI(JI,JL)*PDZG(JI,JL)*XRHOLW
          ENDIF
       ENDDO
    ENDDO
  ELSE
    ZWG_T (:)=PWG (:,2)*PDG(:,2)*XRHOLW
    ZWGI_T(:)=PWGI(:,2)*PDG(:,2)*XRHOLW
    IF(HISBA=='3-L')THEN
      ZWG_T(:)=ZWG_T(:)+PWG(:,3)*(PDG(:,3)-PDG(:,2))*XRHOLW
    ENDIF
  ENDIF
!
! Comptut reservoir time tendencies in kg/m2/s
  PDWG (:) = (ZWG_T (:)-PWG_INI (:))/PTSTEP
  PDWGI(:) = (ZWGI_T(:)-PWGI_INI(:))/PTSTEP
  PDWR (:) = (PWR   (:)-PWR_INI (:))/PTSTEP
  PDSWE(:) = (ZSWE_T(:)-PSWE_INI(:))/PTSTEP
!
! ice calving flux if used
  IF(OGLACIER)THEN
    ZICEFLUX(:)=PICEFLUX(:)
  ELSE
    ZICEFLUX(:)=0.0
  ENDIF
!
! total input water in the system at t
  ZINPUT(:)=PRAIN(:)+PSNOW(:)+PIFLOOD(:)+PIRRIG_FLUX(:)
!
! total output water in the system at t
  ZOUTPUT(:) = PEVAP(:)+PDRAIN(:)+PRUNOFF(:)+PPFLOOD(:)+ZICEFLUX(:)
!
! total reservoir time tendencies at "t - (t-1)"
  ZTENDENCY(:) = PDWG(:)+PDWGI(:)+PDWR(:)+PDSWE(:)
!
! isba water budget (dw/dt=in-out) in kg/m2/s
  PWATBUD(:)=ZTENDENCY(:)-(ZINPUT(:)-ZOUTPUT(:))
!
ENDIF
!
!*      3.0    Comptut isba energy budget in W/m2
!       -----------------------------------------
!
! not yet implemented
!
IF (LHOOK) CALL DR_HOOK('ISBA_BUDGET',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------------
!
END SUBROUTINE ISBA_BUDGET
