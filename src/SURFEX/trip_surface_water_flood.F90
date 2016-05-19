      SUBROUTINE TRIP_SURFACE_WATER_FLOOD (KLUOUT,PTSTEP,KGRCN,KSEQ,KNEXTX,KNEXTY,    &
                                            KSEQMAX,PLEN,PFLOOD_LEN,PSLOPEBED,        &
                                            PWIDTH,PN,PN_FLOOD,PHC,PHFLOOD,PRUNOFF,   &
                                            PSURF_STO,PSURF_STO2,PGOUT,PSOURCE,       &
                                            PFLOOD_STO,PFLOOD_STO2,PSOUT,PVEL,PHS,    &
                                            PFFLOOD,PAREA,PQFR,PQRF,PVFIN,PVFOUT,     &
                                            PHSF,PSIN,                                &
                                            PSSTO_ALL,PSSTO2_ALL,PSIN_ALL,PDRUN_ALL,  &
                                            PSOUT_ALL,PVEL_ALL,PFSTO_ALL,PFSTO2_ALL,  &
                                            PSOURCE_ALL,PFIN_ALL,PFOUT_ALL,PWFLOOD,   &
                                            PSFLOOD_ALL,PHS_ALL,PTAB_F,PTAB_H,        &
                                            PTAB_VF,KTABMAX                           ) 
!     ################################################################
!
!!****  *TRIP_SURFACE_WATER_FLOOD*  
!!
!!    PURPOSE
!!    -------
!
!     Calculate the river and flood storage in the next time step based on storages
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
USE MODD_TRIP_PAR,    ONLY : XRHOLW_T
!
USE MODE_TRIP_FUNCTION
!
USE MODI_FLOOD_UPDATE
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
INTEGER, INTENT(IN)                  :: KLUOUT
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
REAL,DIMENSION(:,:), INTENT(IN)      :: PN_FLOOD   ! Manning coeficient over floodplains   [-] (0.1)
REAL,DIMENSION(:,:), INTENT(IN)      :: PHC        ! River bed depth              [m]
REAL,DIMENSION(:,:), INTENT(IN)      :: PSOURCE    ! precip-infiltration-evaporation [kg/s]
REAL,DIMENSION(:,:), INTENT(IN)      :: PAREA      ! Grid-cell area    [m²]
REAL,DIMENSION(:,:), INTENT(IN)      :: PRUNOFF    ! Surface runoff from ISBA    [kg/s]
REAL,DIMENSION(:,:), INTENT(IN)      :: PGOUT      ! ground water outflow        [kg/s]
REAL,DIMENSION(:,:), INTENT(IN)      :: PSURF_STO  ! river channel storage at t    [kg]
REAL,DIMENSION(:,:), INTENT(IN)      :: PFLOOD_STO ! Floodplain water storage at t [kg]
!
REAL,DIMENSION(:,:,:), INTENT(IN)    :: PTAB_F  ! Flood fraction array
REAL,DIMENSION(:,:,:), INTENT(IN)    :: PTAB_H  ! Topo height array
REAL,DIMENSION(:,:,:), INTENT(IN)    :: PTAB_VF ! Flood volume array
INTEGER, DIMENSION(:,:),INTENT(IN)   :: KTABMAX
!
REAL,DIMENSION(:,:), INTENT(INOUT)   :: PSURF_STO2 ! river channel storage at t+1     [kg]
REAL,DIMENSION(:,:), INTENT(INOUT)   :: PFLOOD_STO2! Floodplain water storage at t+1  [kg]
!
REAL,DIMENSION(:,:), INTENT(OUT)     :: PHFLOOD    ! Floodplain water depth       [m]
REAL,DIMENSION(:,:), INTENT(OUT)     :: PWFLOOD    ! Floodplain width             [m]
REAL,DIMENSION(:,:), INTENT(OUT)     :: PFLOOD_LEN ! Floodplain length along the river [m]
REAL,DIMENSION(:,:), INTENT(OUT)     :: PFFLOOD    ! Fraction of flood [-]
!
REAL,DIMENSION(:,:), INTENT(OUT)     :: PSIN  ! Inflow to the surface river reservoir [kg/s]
REAL,DIMENSION(:,:), INTENT(OUT)     :: PSOUT ! Outflow from the surface river reservoir [kg/s]
REAL,DIMENSION(:,:), INTENT(OUT)     :: PVEL  ! River channel velocity  [m/s]
REAL,DIMENSION(:,:), INTENT(OUT)     :: PHS
REAL,DIMENSION(:,:), INTENT(OUT)     :: PQFR  ! Flood flow to river
REAL,DIMENSION(:,:), INTENT(OUT)     :: PQRF  ! River flow to floodplain
!
REAL,DIMENSION(:,:), INTENT(OUT)     :: PVFIN ! River flow to flood velocity [m/s]
REAL,DIMENSION(:,:), INTENT(OUT)     :: PVFOUT! Flood flow to river velocity [m/s]
REAL,DIMENSION(:,:), INTENT(OUT)     :: PHSF  ! River-Floodplain depth comparison [m] during dt
!
REAL,                 INTENT(OUT)    :: PSSTO_ALL,PSSTO2_ALL,PSIN_ALL,    &
                                         PDRUN_ALL,PSOUT_ALL,PVEL_ALL,     &
                                         PFSTO_ALL,PFSTO2_ALL,PSOURCE_ALL, &
                                         PFIN_ALL,PFOUT_ALL,PSFLOOD_ALL,PHS_ALL 
!                                       Final budget variable
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(SIZE(PLEN,1),SIZE(PLEN,2)) :: ZQIN
!
INTEGER :: ILON, ILAT, I, J, ISEQ
!
REAL    :: ZVELCOEF,ZQOUT,ZAREA,ZTSTEP,ZSTOMAX,ZDELTA,&
            ZFLOOD_STO,ZRECUP,ZFOUT,ZHF,ZWF,ZLF,ZFF 
!
REAL    :: ZS1,ZS2,ZS3
REAL    :: ZF1,ZF2,ZF3
REAL    :: ZK1,ZK2,ZK3,ZK4
REAL    :: ZQ1,ZQ2,ZQ3,ZQ4
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('MODI_TRIP_SURFACE_WATER_FLOOD:TRIP_SURFACE_WATER_FLOOD',0,ZHOOK_HANDLE)
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
PHS        (:,:) = 0.0
PFLOOD_STO2(:,:) = 0.0
!
PVFIN      (:,:) = 0.0
PVFOUT     (:,:) = 0.0
PHSF       (:,:) = 0.0
!
ZQIN  (:,:) = 0.0
!
PFSTO_ALL   = 0.0
PFSTO2_ALL  = 0.0
PSOURCE_ALL = 0.0
PSSTO_ALL   = 0.0
PSSTO2_ALL  = 0.0
PSIN_ALL    = 0.0
PDRUN_ALL   = 0.0
PSOUT_ALL   = 0.0
PVEL_ALL    = 0.0
PFIN_ALL    = 0.0
PFOUT_ALL   = 0.0
PSFLOOD_ALL = 0.0
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
!        Update the floodplain storage due to source (Precip inter - LEf - Infil)
         ZFLOOD_STO=PFLOOD_STO(I,J)+PTSTEP*PSOURCE(I,J)
         ZRECUP    =MIN(ZFLOOD_STO,0.0)
         ZFLOOD_STO=MAX(ZFLOOD_STO,0.0)
! 
         CALL FLOOD_UPDATE(PTAB_F(I,J,1:KTABMAX(I,J)),PTAB_H(I,J,1:KTABMAX(I,J)),PTAB_VF(I,J,1:KTABMAX(I,J)), &
                            PAREA(I,J),ZFLOOD_STO,ZHF,ZFF,ZLF,ZWF) 
!
!        ------------------------------------------------------------------
!        delta calculation to resolve the end of a flood event without Runge Kutta
!        ie : if delta=1, the flood is very small, so it run off directly to the stream
         ZDELTA=DELTA_FLOOD(PSURF_STO(I,J),ZFLOOD_STO,ZHF,ZLF,ZWF,PHC(I,J),PWIDTH(I,J),PLEN(I,J))
!
!        ------------------------------------------------------------------
!        Runge Kutta 4            
!        ------------------------------------------------------------------
!
!        1-----------------------------------------------------------------
         ZTSTEP=PTSTEP/2.0
!
         ZQ1=FUNCFLOOD(PSURF_STO(I,J),ZFLOOD_STO,ZHF,ZLF,ZWF,PHC(I,J),PN_FLOOD(I,J),PWIDTH(I,J),PLEN(I,J),ZDELTA,ZFF)
         ZK1=FUNCVEL(ZVELCOEF,PLEN(I,J),PWIDTH(I,J),PSURF_STO(I,J),PSIN(I,J))
!             
         ZF1=ZFLOOD_STO    +ZTSTEP*ZQ1
         ZS1=PSURF_STO(I,J)+ZTSTEP*(ZK1-ZQ1)
!
         CALL FLOOD_UPDATE(PTAB_F(I,J,1:KTABMAX(I,J)),PTAB_H(I,J,1:KTABMAX(I,J)),PTAB_VF(I,J,1:KTABMAX(I,J)), &
                            PAREA(I,J),ZF1,ZHF,ZFF,ZLF,ZWF) 
!
!        2-----------------------------------------------------------------
         ZTSTEP=PTSTEP/2.0
! 
         ZQ2=FUNCFLOOD(ZS1,ZF1,ZHF,ZLF,ZWF,PHC(I,J),PN_FLOOD(I,J),PWIDTH(I,J),PLEN(I,J),ZDELTA,ZFF)
         ZK2=FUNCVEL(ZVELCOEF,PLEN(I,J),PWIDTH(I,J),ZS1,PSIN(I,J))
!             
         ZF2=ZFLOOD_STO    +ZTSTEP*ZQ2
         ZS2=PSURF_STO(I,J)+ZTSTEP*(ZK2-ZQ2)
!
         CALL FLOOD_UPDATE(PTAB_F(I,J,1:KTABMAX(I,J)),PTAB_H(I,J,1:KTABMAX(I,J)),PTAB_VF(I,J,1:KTABMAX(I,J)), &
                            PAREA(I,J),ZF2,ZHF,ZFF,ZLF,ZWF) 
!
!        3-----------------------------------------------------------------
         ZTSTEP=PTSTEP
!   
         ZQ3=FUNCFLOOD(ZS2,ZF2,ZHF,ZLF,ZWF,PHC(I,J),PN_FLOOD(I,J),PWIDTH(I,J),PLEN(I,J),ZDELTA,ZFF)
         ZK3=FUNCVEL(ZVELCOEF,PLEN(I,J),PWIDTH(I,J),ZS2,PSIN(I,J))
!
         ZF3=ZFLOOD_STO    +ZTSTEP*ZQ3
         ZS3=PSURF_STO(I,J)+ZTSTEP*(ZK3-ZQ3)
!
         CALL FLOOD_UPDATE(PTAB_F(I,J,1:KTABMAX(I,J)),PTAB_H(I,J,1:KTABMAX(I,J)),PTAB_VF(I,J,1:KTABMAX(I,J)), &
                            PAREA(I,J),ZF3,ZHF,ZFF,ZLF,ZWF) 
!
!        4-----------------------------------------------------------------
!
         ZQ4=FUNCFLOOD(ZS3,ZF3,ZHF,ZLF,ZWF,PHC(I,J),PN_FLOOD(I,J),PWIDTH(I,J),PLEN(I,J),ZDELTA,ZFF)
         ZK4=FUNCVEL(ZVELCOEF,PLEN(I,J),PWIDTH(I,J),ZS3,PSIN(I,J))
!
!        Final points------------------------------------------------------
         ZTSTEP=PTSTEP/6.0
!
         PFLOOD_STO2(I,J)=ZFLOOD_STO    +ZTSTEP*(ZQ1+2.0*ZQ2+2.0*ZQ3+ZQ4)-ZDELTA*ZFLOOD_STO
         PSURF_STO2 (I,J)=PSURF_STO(I,J)+ZTSTEP*(ZK1-ZQ1+2.0*(ZK2-ZQ2)+2.0*(ZK3-ZQ3)+ZK4-ZQ4)+ZDELTA*ZFLOOD_STO
!        -------------------------------------------------------------------
!        supress numerical artifacs
         IF(PSURF_STO2(I,J)<=0.0.AND.PFLOOD_STO2(I,J)>0.0)THEN
            WRITE(KLUOUT,*)'-------------------------------------------------------'
            WRITE(KLUOUT,*)'Stream reservoir (kg/m²) :',PSURF_STO2 (I,J)/PAREA(I,J)
            WRITE(KLUOUT,*)'Flood  reservoir (kg/m²) :',PFLOOD_STO2(I,J)/PAREA(I,J)
            WRITE(KLUOUT,*)'Longitude index :',I,'Latitude index :',J
            PSURF_STO2 (I,J)=MIN(PLEN(I,J)*PWIDTH(I,J)*PHC(I,J)*XRHOLW_T,PFLOOD_STO2(I,J))
            PFLOOD_STO2(I,J)=MAX(0.0,PFLOOD_STO2(I,J)-PSURF_STO2 (I,J))
            WRITE(KLUOUT,*)'Stream reservoir (kg/m²) :',PSURF_STO2 (I,J)/PAREA(I,J)
            WRITE(KLUOUT,*)'Flood  reservoir (kg/m²) :',PFLOOD_STO2(I,J)/PAREA(I,J)               
            WRITE(KLUOUT,*)'-------------------------------------------------------'
         ENDIF
!             
         IF(PSURF_STO2(I,J)<0.0)THEN
            WRITE(KLUOUT,*)'-------------------------------------------------------'
            WRITE(KLUOUT,*)'TRIP_SURFACE_WATER_FLOOD : Stream reservoir is negatif '
            WRITE(KLUOUT,*)'                           after RK                    '
            WRITE(KLUOUT,*)'Stream reservoir     (kg/m²) :',PSURF_STO2(I,J)/PAREA(I,J)
            WRITE(KLUOUT,*)'Stream reservoir in  (kg/m²) :',PSURF_STO (I,J)/PAREA(I,J)
            WRITE(KLUOUT,*)'Flood  reservoir     (kg/m²) :',PFLOOD_STO2(I,J)/PAREA(I,J)
            WRITE(KLUOUT,*)'Longitude index :',I,'Latitude index :',J
            WRITE(KLUOUT,*)'Perhaps TRIP time step too big : ',PTSTEP
            WRITE(KLUOUT,*)'-------------------------------------------------------'
            STOP
         ENDIF             
!
!        -------------------------------------------------------------------
!        total flood flux  
         ZFOUT = (ZFLOOD_STO-PFLOOD_STO2(I,J))/PTSTEP
!
!        -------------------------------------------------------------------
!        supress numerical artifacs
         ZSTOMAX=(ZQIN(I,J)+ZFOUT)*PTSTEP+PSURF_STO(I,J)
!      
         PSURF_STO2(I,J)=MIN(ZSTOMAX,PSURF_STO2(I,J))
!
!        ------------------------------------------------------------------
!        river channel outflow calculation and supress numerical artifacs
!
         ZQOUT      = (PSURF_STO(I,J)-PSURF_STO2(I,J))/PTSTEP+ZQIN(I,J)+ZFOUT
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
!        -------------------------------------------------------------------
!        supress numerical artifacs
!
         ZRECUP=ZRECUP+MIN(PFLOOD_STO2(I,J),0.0)
         PFLOOD_STO2(I,J)=MAX(PFLOOD_STO2(I,J),0.0)
!
         PSURF_STO2(I,J) =PSURF_STO2(I,J)+ZRECUP
!             
!        -------------------------------------------------------------------
         IF(PSURF_STO2(I,J)<0.0)THEN
            WRITE(KLUOUT,*)'-------------------------------------------------------'
            WRITE(KLUOUT,*)'TRIP_SURFACE_WATER_FLOOD : Stream reservoir is negatif '
            WRITE(KLUOUT,*)'                       when supress numerical artifacs '
            WRITE(KLUOUT,*)'Stream reservoir     (kg/m²) :',PSURF_STO2 (I,J)/PAREA(I,J)
            WRITE(KLUOUT,*)'Recup                (kg/m²) :',ZRECUP/PAREA(I,J)
            WRITE(KLUOUT,*)'Flood  reservoir in  (kg/m²) :',ZFLOOD_STO/PAREA(I,J)
            WRITE(KLUOUT,*)'Flood  reservoir end (kg/m²) :',PFLOOD_STO2(I,J)/PAREA(I,J)
            WRITE(KLUOUT,*)'Longitude index :',I,'Latitude index :',J
            WRITE(KLUOUT,*)'Perhaps TRIP time step too big : ',PTSTEP
            WRITE(KLUOUT,*)'-------------------------------------------------------'
            STOP
         ENDIF   
!         
!        ------------------------------------------------------------------
!        flood diag using flood variables at t and stream variables at t+dt
!
         PQFR(I,J)=    MAX(0.0,ZFOUT)
         PQRF(I,J)=ABS(MIN(0.0,ZFOUT))
!             
         IF(PHC(I,J)>0.0)THEN           
            PHSF(I,J)=PHS(I,J)-PHC(I,J)-PHFLOOD(I,J)
         ENDIF
!
         CALL VELFLOOD(PHS(I,J),PFFLOOD(I,J),PHFLOOD(I,J),PFLOOD_LEN(I,J),PWFLOOD(I,J),PHC(I,J),&
                        PN_FLOOD(I,J),PWIDTH(I,J),ZDELTA,PVFIN(I,J),PVFOUT(I,J)     ) 
!             
         IF(PHFLOOD(I,J)>0.0)THEN
            PVFOUT(I,J)=ZDELTA/PTSTEP+(1.0-ZDELTA)*PVFOUT(I,J)
         ENDIF            
!
!        ------------------------------------------------------------------
!        Final flood update
!
         CALL FLOOD_UPDATE(PTAB_F(I,J,1:KTABMAX(I,J)),PTAB_H(I,J,1:KTABMAX(I,J)),PTAB_VF(I,J,1:KTABMAX(I,J)), &
                            PAREA(I,J),PFLOOD_STO2(I,J),ZHF,ZFF,ZLF,ZWF) 
!
         PFFLOOD   (I,J)=ZFF
         PHFLOOD   (I,J)=ZHF
         PFLOOD_LEN(I,J)=ZLF
         PWFLOOD   (I,J)=ZWF             
!
!        ------------------------------------------------------------------
!        budget calculation
         PSSTO_ALL  = PSSTO_ALL  + PSURF_STO(I,J)  
         PSSTO2_ALL = PSSTO2_ALL + PSURF_STO2(I,J) - ZRECUP
         PSIN_ALL   = PSIN_ALL   + ZQIN(I,J)
         PSOUT_ALL  = PSOUT_ALL  + PSOUT(I,J)
         PVEL_ALL   = PVEL_ALL   + PVEL(I,J) * PAREA(I,J)
         PHS_ALL    = PHS_ALL    + PHS (I,J) * PAREA(I,J)
!             
         IF(PHC(I,J)>0.0)THEN           
            PSFLOOD_ALL= PSFLOOD_ALL+ ZFOUT   
            PFSTO_ALL  = PFSTO_ALL  + PFLOOD_STO(I,J)
            PFSTO2_ALL = PFSTO2_ALL + PFLOOD_STO2(I,J) + ZRECUP
            PFIN_ALL   = PFIN_ALL   + PQRF(I,J)
            PFOUT_ALL  = PFOUT_ALL  + PQFR(I,J)
            PSOURCE_ALL= PSOURCE_ALL+ PSOURCE(I,J)
         ENDIF
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
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('MODI_TRIP_SURFACE_WATER_FLOOD:TRIP_SURFACE_WATER_FLOOD',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
END SUBROUTINE TRIP_SURFACE_WATER_FLOOD
