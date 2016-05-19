!     #########
      SUBROUTINE TRIP_SURFACE_WATER_VELVAR (PTSTEP,KGRCN,KSEQ,KNEXTX,KNEXTY,KSEQMAX,       &
                                             PLEN,PSLOPEBED,PWIDTH,PN,PRUNOFF,PSURF_STO,   &
                                             PSURF_STO2,PGOUT,PSIN,PSOUT,PVEL,PHS,PAREA,   &
                                             PSSTO_ALL,PSSTO2_ALL,PSIN_ALL,PDRUN_ALL,      &
                                             PSOUT_ALL,PVEL_ALL,PHS_ALL                    ) 
!     ################################################################
!
!!****  *TRIP_SURFACE_WATER_VELVAR*  
!!
!!    PURPOSE
!!    -------
!
!     Calculate the river storage in the next time step based on the storage
!     of current time step using the Manning equation and the Arora (1999) 
!     variable flow velocity scheme.
!     Numérical method = RK Ordre 4 Rang 4:
!
!     Point de depart       : X0(t)     =X(t)
!     point intermediaire K1: X1(t+dt/2)=X(t) + dt/2 * F(X0(t))
!     point intermediaire K2: X2(t+dt/2)=X(t) + dt/2 * F(X1(t+dt/2))
!     point intermediaire K3: X3(t+dt)  =X(t) + dt   * F(X2(t+dt/2))
!     point final         K4:                          F(X3(t+dt))
!
!     point Final: X(t+dt)=X(t)+dt/6*(F(X0(t))+2*F(X1(t+dt/2))+2*F(X2(t+dt/2))+F(X3(t+dt)))
!
!     
!!**  METHOD
!!    ------
!
!     RK Ordre 4 Rang 4
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
!!      Original    01/02/09 
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_TRIP_PAR, ONLY : XRHOLW_T
!
USE MODE_TRIP_FUNCTION
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
REAL,DIMENSION(:,:), INTENT(IN)      :: PSLOPEBED  ! river bed slopes             [m/m]
REAL,DIMENSION(:,:), INTENT(IN)      :: PWIDTH     ! river widths                 [m]
REAL,DIMENSION(:,:), INTENT(IN)      :: PN         ! Manning roughness coeficient [-] (0.03 to 0.065)
REAL,DIMENSION(:,:), INTENT(IN)      :: PAREA      ! Grid-cell area    [m²]
REAL,DIMENSION(:,:), INTENT(IN)      :: PRUNOFF    ! Surface runoff from ISBA    [kg/s]
REAL,DIMENSION(:,:), INTENT(IN)      :: PGOUT      ! ground water outflow        [kg/s]
REAL,DIMENSION(:,:), INTENT(IN)      :: PSURF_STO  ! river channel storage at t    [kg]
!
REAL,DIMENSION(:,:), INTENT(INOUT)   :: PSURF_STO2 ! river channel storage at t+1     [kg]
!
REAL,DIMENSION(:,:), INTENT(OUT)     :: PHS   ! river channel height [m]
REAL,DIMENSION(:,:), INTENT(OUT)     :: PSIN  ! Inflow to the surface river reservoir [kg/s]
REAL,DIMENSION(:,:), INTENT(OUT)     :: PSOUT ! Outflow from the surface river reservoir [kg/s]
REAL,DIMENSION(:,:), INTENT(OUT)     :: PVEL  ! River channel velocity  [m/s]
!
REAL,                 INTENT(OUT)    :: PSSTO_ALL,PSSTO2_ALL,PSIN_ALL,    &
                                         PDRUN_ALL,PSOUT_ALL,PVEL_ALL,     &
                                         PHS_ALL 
!                                       Final budget variable
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(SIZE(PLEN,1),SIZE(PLEN,2)) :: ZQIN
!
REAL    :: ZVELCOEF,ZQOUT,ZAREA, &
            ZTSTEP,ZSTOMAX 
!
REAL    :: ZS1,ZS2,ZS3
REAL    :: ZK1,ZK2,ZK3,ZK4
!
INTEGER :: ILON, ILAT, I, J, ISEQ
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('MODI_TRIP_SURFACE_WATER_VELVAR:TRIP_SURFACE_WATER_VELVAR',0,ZHOOK_HANDLE)
ILON = SIZE(PLEN,1)
ILAT = SIZE(PLEN,2)
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!
PSURF_STO2 (:,:) = 0.0
PSIN       (:,:) = 0.0
PSOUT      (:,:) = 0.0
PVEL       (:,:) = 0.0
ZQIN       (:,:) = 0.0
!
PSSTO_ALL   = 0.0
PSSTO2_ALL  = 0.0
PSIN_ALL    = 0.0
PDRUN_ALL   = 0.0
PSOUT_ALL   = 0.0
PVEL_ALL    = 0.0
PHS_ALL=0.0
!
ZAREA=SUM(PAREA,PLEN>0.0)
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
         IF(PN(I,J)>0.0)THEN
           ZVELCOEF=SQRT(PSLOPEBED(I,J))/PN(I,J)
         ELSE
           ZVELCOEF=0.0
         ENDIF
!
!        ------------------------------------------------------------------
!        Runge Kutta 4            
!        ------------------------------------------------------------------
!           
         ZTSTEP=PTSTEP/2.0
         ZK1=FUNCVEL(ZVELCOEF,PLEN(I,J),PWIDTH(I,J),PSURF_STO(I,J),PSIN(I,J))
         ZS1=PSURF_STO(I,J)+ZTSTEP*ZK1
!
         ZTSTEP=PTSTEP/2.0
         ZK2=FUNCVEL(ZVELCOEF,PLEN(I,J),PWIDTH(I,J),ZS1,PSIN(I,J))
         ZS2=PSURF_STO(I,J)+ZTSTEP*ZK2
!
         ZTSTEP=PTSTEP
         ZK3=FUNCVEL(ZVELCOEF,PLEN(I,J),PWIDTH(I,J),ZS2,PSIN(I,J))
         ZS3=PSURF_STO(I,J)+ZTSTEP*ZK3
!
         ZK4=FUNCVEL(ZVELCOEF,PLEN(I,J),PWIDTH(I,J),ZS3,PSIN(I,J))
!
         ZTSTEP=PTSTEP/6.0
         PSURF_STO2(I,J)=PSURF_STO(I,J)+ZTSTEP*(ZK1+2.0*ZK2+2.0*ZK3+ZK4)
!
!        -------------------------------------------------------------------
!        supress numerical artifacs
         ZSTOMAX=ZQIN(I,J)*PTSTEP+PSURF_STO(I,J)
!      
         PSURF_STO2(I,J)=MIN(ZSTOMAX,PSURF_STO2(I,J))
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
!        river channel height and velocity diagnostic             
!           
         IF(ZVELCOEF==0.0)THEN
            PHS(I,J)=0.0
         ELSE
            PHS(I,J)=PSURF_STO2(I,J)/(XRHOLW_T*PLEN(I,J)*PWIDTH(I,J))
         ENDIF
!           
         PVEL(I,J)=DIAGVEL(ZVELCOEF,PLEN(I,J),PWIDTH(I,J),PHS(I,J))
!
!        ------------------------------------------------------------------
!        budget calculation
         PSSTO_ALL  = PSSTO_ALL  + PSURF_STO (I,J)  
         PSSTO2_ALL = PSSTO2_ALL + PSURF_STO2(I,J)
         PSIN_ALL   = PSIN_ALL   + ZQIN      (I,J)
         PSOUT_ALL  = PSOUT_ALL  + PSOUT     (I,J)
         PVEL_ALL   = PVEL_ALL   + PVEL      (I,J) * PAREA(I,J)
         PHS_ALL    = PHS_ALL    + PHS       (I,J) * PAREA(I,J)
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
PVEL_ALL = PVEL_ALL / ZAREA
PHS_ALL = PHS_ALL / ZAREA
!
IF (LHOOK) CALL DR_HOOK('MODI_TRIP_SURFACE_WATER_VELVAR:TRIP_SURFACE_WATER_VELVAR',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
END SUBROUTINE TRIP_SURFACE_WATER_VELVAR
