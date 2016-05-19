!     #########
      SUBROUTINE TRIP_SURFACE_WATER (PTSTEP,KGRCN,KSEQ,KNEXTX,KNEXTY,KSEQMAX, &
                                       PLEN,PRUNOFF,PGOUT,PSURF_STO,PSURF_STO2, &
                                       PSIN,PSOUT,PSSTO_ALL,PSSTO2_ALL,PSIN_ALL,&
                                       PDRUN_ALL,PSOUT_ALL)  
!     ################################################################
!
!!****  *TRIP_SURFACE_WATER*  
!!
!!    PURPOSE
!!    -------
!
!     Calculate the river storage in the next time step based on the storage
!     of current time step using fixed 0.5m/s stream flow velocity.
!
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
USE MODD_TRIP_n,   ONLY : XCVEL
USE MODD_TRIP_PAR, ONLY : XRHOLW_T
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, INTENT(IN)                     :: PTSTEP ! Trip timestep value (10800s)
!
INTEGER, DIMENSION(:,:),INTENT(IN)   :: KGRCN  ! Flow direction (1->8)
INTEGER, DIMENSION(:,:),INTENT(IN)   :: KSEQ   ! River sequence
INTEGER, DIMENSION(:,:),INTENT(IN)   :: KNEXTX ! returns x and y point
INTEGER, DIMENSION(:,:),INTENT(IN)   :: KNEXTY ! of destination grid:
!                                                                    8 1 2
!                                                                    7   3
!                                                                    6 5 4
!
INTEGER, INTENT(IN)                  :: KSEQMAX ! maximum down flow
!
REAL,DIMENSION(:,:), INTENT(IN)      :: PLEN       ! river length       [m] 
REAL,DIMENSION(:,:), INTENT(IN)      :: PRUNOFF    ! Surface runoff from ISBA    [kg/s]
REAL,DIMENSION(:,:), INTENT(IN)      :: PGOUT      ! ground water outflow        [kg/s]
REAL,DIMENSION(:,:), INTENT(IN)      :: PSURF_STO  ! river channel storage at t    [kg]
!
REAL,DIMENSION(:,:), INTENT(INOUT)   :: PSURF_STO2 ! river channel storage at t+1     [kg]
!
REAL,DIMENSION(:,:), INTENT(OUT)     :: PSIN  ! Inflow to the surface river reservoir [kg/s]
REAL,DIMENSION(:,:), INTENT(OUT)     :: PSOUT ! Outflow from the surface river reservoir [kg/s]
!
REAL,                INTENT(OUT)     :: PSSTO_ALL,PSSTO2_ALL,PSIN_ALL,    &
                                          PDRUN_ALL,PSOUT_ALL  
!                                       Final budget variable
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(SIZE(PLEN,1),SIZE(PLEN,2)) :: ZQIN
!
REAL    :: ZRC,ZQOUT,ZSTOMAX
!
INTEGER :: ILON, ILAT, I, J, ISEQ
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('TRIP_SURFACE_WATER',0,ZHOOK_HANDLE)
ILON = SIZE(PLEN,1)
ILAT = SIZE(PLEN,2)
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!
PSURF_STO2 (:,:) = 0.0
PSIN       (:,:) = 0.0
PSOUT      (:,:) = 0.0
!
ZQIN  (:,:) = 0.0
!
PSSTO_ALL   = 0.0
PSSTO2_ALL  = 0.0
PSIN_ALL    = 0.0
PDRUN_ALL   = 0.0
PSOUT_ALL   = 0.0
!
!-------------------------------------------------------------------------------
!Sequence loop
!
DO ISEQ=1,KSEQMAX
   DO J=1,ILAT
      DO I=1,ILON
!      
         IF(KSEQ(I,J)/=ISEQ.OR.KSEQ(I,J)==0)CYCLE      
!
!        ---------------------------------------------------------------------
!        inflow calculation
!
         ZQIN(I,J)=ZQIN(I,J)+PRUNOFF(I,J)+PGOUT(I,J)
         PDRUN_ALL=PDRUN_ALL+PRUNOFF(I,J)+PGOUT(I,J)
!
         PSIN(I,J)=ZQIN(I,J)
!
!        ------------------------------------------------------------------
!        Fixed river channel velocity             
         ZRC = XCVEL / PLEN(I,J)
!              
!        ------------------------------------------------------------------
!        river channel storage calculation
         PSURF_STO2(I,J) = PSURF_STO(I,J)*EXP(-(ZRC*PTSTEP))+(1.0-EXP(-(ZRC*PTSTEP)))*ZQIN(I,J)/ZRC 
!
!        -------------------------------------------------------------------
!        supress numerical artifacs
         ZSTOMAX=ZQIN(I,J)*PTSTEP+PSURF_STO(I,J)
!      
         PSURF_STO2(I,J)=MIN(ZSTOMAX, PSURF_STO2(I,J))
!
!        ------------------------------------------------------------------
!        river channel outflow calculation and supress numerical artifacs
!
         ZQOUT      = (PSURF_STO(I,J)-PSURF_STO2(I,J))/PTSTEP+ZQIN(I,J)
         PSOUT(I,J) = MAX(ZQOUT,0.0)
!             
         PSURF_STO2(I,J) = PSURF_STO2(I,J) + (PSOUT(I,J)-ZQOUT)             
!            
!        ------------------------------------------------------------------
!        budget calculation
         PSSTO_ALL  = PSSTO_ALL  + PSURF_STO(I,J)  
         PSSTO2_ALL = PSSTO2_ALL + PSURF_STO2(I,J)
         PSIN_ALL   = PSIN_ALL   + ZQIN(I,J)
         PSOUT_ALL  = PSOUT_ALL  + PSOUT(I,J)
!              
!        ------------------------------------------------------------------
         IF(KGRCN(I,J)>=1.AND.KGRCN(I,J)<=8)THEN
           ZQIN(KNEXTX(I,J),KNEXTY(I,J))=ZQIN(KNEXTX(I,J),KNEXTY(I,J))+PSOUT(I,J)
         ENDIF
!
      ENDDO
   ENDDO
ENDDO
!
IF (LHOOK) CALL DR_HOOK('TRIP_SURFACE_WATER',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
END SUBROUTINE TRIP_SURFACE_WATER
