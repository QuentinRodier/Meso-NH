!     #########
      SUBROUTINE TRIP_GROUND_WATER (PTSTEP,PGROUND_STO,PGROUND_STO2,PDRAIN,PTAUG, &
                                      PGOUT,PGSTO_ALL,PGSTO2_ALL,PGIN_ALL,PGOUT_ALL)  
!     #############################################################################
!
!!****  *TRIP_GROUND_WATER*  
!!
!!    PURPOSE
!!    -------
!
!     Calculate the storage in the next time step based on the storage
!     of current time step.The deep drainage is constant during the time step.
!     
!!**  METHOD
!!    ------
!
!     Direct calculation
!
!!    EXTERNAL
!!    --------
!
!     None
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
!!	B. Decharme     
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/02/05 
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, INTENT(IN)                     :: PTSTEP
!                                       KTSTEP = timestep value (=FRC) [s]
!                                              = 10800s
!
!
REAL, DIMENSION(:,:), INTENT(IN)     :: PDRAIN, PTAUG
!                                       PRUNOFF = Surface runoff from ISBA    [kg/m²]
!                                       PTAUG   = ground water transfer time  [s]
!
REAL, DIMENSION(:,:), INTENT(IN   )  :: PGROUND_STO
REAL, DIMENSION(:,:), INTENT(INOUT)  :: PGROUND_STO2
!                                       PGROUND_STO  = ground water storage at t    [kg]
!                                       PGROUND_STO2 = ground water storage at t+1  [kg]
!
REAL, DIMENSION(:,:), INTENT(OUT)    :: PGOUT
!                                       PGOUT = Outflow from the ground reservoir  
!
REAL,                 INTENT(OUT)    :: PGSTO_ALL,PGSTO2_ALL,PGIN_ALL,PGOUT_ALL
!                                       Final budget variable
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(SIZE(PGROUND_STO,1),SIZE(PGROUND_STO,2)) :: ZGSTOMAX, ZGOUT
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('TRIP_GROUND_WATER',0,ZHOOK_HANDLE)
!
PGROUND_STO2(:,:) = 0.0
PGOUT       (:,:) = 0.0
!
ZGSTOMAX    (:,:) = 0.0
ZGOUT       (:,:) = 0.0
!
PGSTO_ALL  = 0.0
PGSTO2_ALL = 0.0
PGIN_ALL   = 0.0
PGOUT_ALL  = 0.0
!
WHERE(PTAUG(:,:)>0.0)
!
!     ---------------------------------------------------------------------------
!     ground water storage calculation
!
      PGROUND_STO2(:,:)=PGROUND_STO(:,:)*EXP(-PTSTEP/PTAUG(:,:)) &
                         + (1.0-EXP(-PTSTEP/PTAUG(:,:)))*PDRAIN(:,:)*PTAUG(:,:)  
!
!     ---------------------------------------------------------------------------
!     supress numerical artifacs
!
      ZGSTOMAX(:,:)=PDRAIN(:,:)*PTSTEP+PGROUND_STO(:,:)
!      
      PGROUND_STO2(:,:)=MIN(ZGSTOMAX(:,:),PGROUND_STO2(:,:))
!
!     ---------------------------------------------------------------------------
!     ground water discharge calculation                
!
      ZGOUT(:,:)=(PGROUND_STO(:,:)-PGROUND_STO2(:,:))/PTSTEP+PDRAIN(:,:)
!      
!     ---------------------------------------------------------------------------
!     supress numerical artifacs
!
      PGOUT(:,:)=MAX(0.0,ZGOUT(:,:))
      PGROUND_STO2(:,:) = PGROUND_STO2(:,:) + (PGOUT(:,:)-ZGOUT(:,:))             
!
ENDWHERE
!
!-------------------------------------------------------------------------------
!budget calculation
!
PGSTO_ALL  = SUM(PGROUND_STO (:,:), PTAUG(:,:)>0.0)
PGSTO2_ALL = SUM(PGROUND_STO2(:,:), PTAUG(:,:)>0.0)
PGIN_ALL   = SUM(PDRAIN      (:,:), PTAUG(:,:)>0.0)
PGOUT_ALL  = SUM(PGOUT       (:,:), PTAUG(:,:)>0.0)
!
IF (LHOOK) CALL DR_HOOK('TRIP_GROUND_WATER',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
END SUBROUTINE TRIP_GROUND_WATER
