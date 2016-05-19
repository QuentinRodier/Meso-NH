!     #########
      SUBROUTINE GARDEN_SOIL_DEPTH(HNVEG,HLVEG,HHVEG,PNVEG,PLVEG,PHVEG,PDG)
!     #########################################
!
!!****  *GARDEN_SOIL_DEPTH* - routine to initialise garden soil depth from data 
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
!!	V. Masson   *Meteo France*	
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    10/2011 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_ISBA_PAR,          ONLY : XOPTIMGRID, NOPTIMLAYER
!
USE MODI_ABOR1_SFX
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
 CHARACTER(LEN=3)                   :: HNVEG  ! type of no   vegetation
 CHARACTER(LEN=3)                   :: HLVEG  ! type of low  vegetation
 CHARACTER(LEN=3)                   :: HHVEG  ! type of high vegetation
REAL, DIMENSION(:),     INTENT(IN) :: PNVEG  ! fraction of no   vegetation
REAL, DIMENSION(:),     INTENT(IN) :: PLVEG  ! fraction of low  vegetation
REAL, DIMENSION(:),     INTENT(IN) :: PHVEG  ! fraction of high vegetation
REAL, DIMENSION(:,:,:), INTENT(OUT):: PDG    ! soil depth
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
REAL, DIMENSION(SIZE(PDG,1),SIZE(PDG,2),3) :: ZDATA_DG
INTEGER                          :: JL, JI
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('GARDEN_SOIL_DEPTH',0,ZHOOK_HANDLE)

IF (SIZE(PDG,2)<=3) THEN

  ZDATA_DG(:,1,:) = 0.01
  ZDATA_DG(:,2,:) = 1.50
  IF(HHVEG=='TREE')  ZDATA_DG(:,2,1)= 2.0
  IF(HHVEG=='CONI')  ZDATA_DG(:,2,1)= 2.0 
  IF(HHVEG=='EVER')  ZDATA_DG(:,2,1)= 2.0
  IF(HNVEG=='NO  ')  ZDATA_DG(:,2,3)= 0.5
  IF(HNVEG=='ROCK')  ZDATA_DG(:,2,3)= 0.5
  IF(HNVEG=='SNOW')  ZDATA_DG(:,2,3)= 0.5

  IF (SIZE(PDG,2)==3) THEN
    ZDATA_DG(:,3,:) = 2.00
    IF(HHVEG=='TREE')  ZDATA_DG(:,3,1)= 3.0
    IF(HHVEG=='CONI')  ZDATA_DG(:,3,1)= 3.0
    IF(HHVEG=='EVER')  ZDATA_DG(:,3,1)= 3.0
    IF(HNVEG=='NO  ')  ZDATA_DG(:,3,3)= 1.0
    IF(HNVEG=='ROCK')  ZDATA_DG(:,3,3)= 1.0
    IF(HNVEG=='SNOW')  ZDATA_DG(:,3,3)= 1.0
  ENDIF

ELSEIF (SIZE(PDG,2)<=NOPTIMLAYER) THEN

  DO JI=1,SIZE(PDG,1)
    DO JL=1,3
      ZDATA_DG(JI,:,JL) = XOPTIMGRID(:)
    ENDDO
  ENDDO
 
ELSE 

  CALL ABOR1_SFX("GARDEN_SOIL_DEPTH: WITH MORE THAN 14 SOIL LAYERS, "//&
     "WITHOUT ECOCLIMAP, GARDEN CANNOT RUN")

ENDIF
!
DO JL=1,SIZE(PDG,2)
 PDG(:,JL,1) =   ZDATA_DG(:,JL,1)*PHVEG(:)   &
               + ZDATA_DG(:,JL,2)*PLVEG(:)   &
               + ZDATA_DG(:,JL,3)*PNVEG(:)  
END DO
!
IF (LHOOK) CALL DR_HOOK('GARDEN_SOIL_DEPTH',1,ZHOOK_HANDLE)
!
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE GARDEN_SOIL_DEPTH
