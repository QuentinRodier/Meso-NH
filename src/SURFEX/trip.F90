!     #########
      SUBROUTINE TRIP (KLUOUT,HGROUNDW,HVIT,OFLOOD,OPRINT,PTSTEP,         &
                         KGRCN,KSEQ,KNEXTX,KNEXTY,KSEQMAX,PTAUG,PAREA,    &
                         PLEN,PFLOOD_LEN,PSLOPEBED,PWIDTH,PN,PN_FLOOD,    &
                         PHC_BED,PWFLOOD,PTAB_F,PTAB_H,PTAB_VF,PDRAIN,    &
                         PRUNOFF,PSOURCE,PGROUND_STO,PSURF_STO,           &
                         PFLOOD_STO,PSOUT,PGOUT,PHS,PHFLOOD,PVEL,         &
                         PFFLOOD,PQFR,PQRF,PVFIN,PVFOUT,                  &
                         PHSF,PSIN,KTRIP,KTSEPT,KTSTEP_END,KTABMAX        )  
!     ###################################################################
!
!!****  *TRIP*  
!!
!!    PURPOSE
!!    -------
!
!     TRIP river routing and Floodplains schemes.
!     
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
!!      Modif.      28/05/08 
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_TRIP_PAR, ONLY : XRHOLW_T,XSEA_T,XYEAR_T
!
USE MODI_TRIP_GROUND_WATER
!
USE MODI_TRIP_SURFACE_WATER
USE MODI_TRIP_SURFACE_WATER_VELVAR
USE MODI_TRIP_SURFACE_WATER_FLOOD
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
INTEGER, INTENT(IN)                  :: KLUOUT
INTEGER, INTENT(IN)                  :: KTRIP
INTEGER, INTENT(IN)                  :: KTSEPT
INTEGER, INTENT(IN)                  :: KTSTEP_END
!
 CHARACTER(LEN=3), INTENT(IN)         :: HGROUNDW !Groundwater scheme key
 CHARACTER(LEN=3), INTENT(IN)         :: HVIT     !Variable velocity scheme key
!
LOGICAL, INTENT(IN)                  :: OFLOOD   !Flood scheme key
LOGICAL, INTENT(IN)                  :: OPRINT   !Printable budget key 
!
REAL, INTENT(IN)                     :: PTSTEP   !Trip timestep
!
INTEGER, INTENT(IN)                  :: KSEQMAX  !maximum down flow
INTEGER, DIMENSION(:,:),INTENT(IN)   :: KGRCN    !Flow direction (1->8)
INTEGER, DIMENSION(:,:),INTENT(IN)   :: KSEQ     !River sequence
INTEGER, DIMENSION(:,:),INTENT(IN)   :: KNEXTX   !returns x and y point
INTEGER, DIMENSION(:,:),INTENT(IN)   :: KNEXTY   !of destination grid
!
REAL, DIMENSION(:,:), INTENT(IN)     :: PTAUG    !ground water transfer time  [s]
!
REAL,DIMENSION(:,:), INTENT(IN)      :: PLEN       ! river length       [m] 
REAL,DIMENSION(:,:), INTENT(IN)      :: PSLOPEBED  ! river bed slopes             [m/m]
REAL,DIMENSION(:,:), INTENT(IN)      :: PWIDTH     ! river widths                 [m]
REAL,DIMENSION(:,:), INTENT(IN)      :: PN         ! Manning roughness coeficient [-] (0.03 to 0.065)
REAL,DIMENSION(:,:), INTENT(IN)      :: PN_FLOOD   ! Manning coeficient over floodplains   [-] (0.1)
REAL,DIMENSION(:,:), INTENT(IN)      :: PHC_BED    ! River bed depth              [m]
REAL,DIMENSION(:,:), INTENT(IN)      :: PSOURCE    ! precip-infiltration-evaporation [kg/s]
REAL,DIMENSION(:,:), INTENT(IN)      :: PAREA      ! Grid-cell area    [m²]
REAL,DIMENSION(:,:), INTENT(IN)      :: PDRAIN     ! Subsurface runoff from ISBA [kg/s]
REAL,DIMENSION(:,:), INTENT(INOUT)   :: PRUNOFF    ! Surface runoff from ISBA    [kg/s]
!
REAL,DIMENSION(:,:), INTENT(INOUT)   :: PHFLOOD    ! Floodplain water depth       [m]
REAL,DIMENSION(:,:), INTENT(INOUT)   :: PWFLOOD    ! Floodplain width             [m]
REAL,DIMENSION(:,:), INTENT(INOUT)   :: PFLOOD_LEN ! Floodplain length along the river [m]
REAL,DIMENSION(:,:), INTENT(INOUT)   :: PFFLOOD    ! Fraction of flood [-]
REAL,DIMENSION(:,:), INTENT(INOUT)   :: PSURF_STO  ! river channel storage at t    [kg]
REAL,DIMENSION(:,:), INTENT(INOUT)   :: PGROUND_STO! groundwater storage at t    [kg]
REAL,DIMENSION(:,:), INTENT(INOUT)   :: PFLOOD_STO ! Floodplain water storage at t [kg]
!
REAL,DIMENSION(:,:), INTENT(OUT)     :: PSOUT ! Outflow from the surface river reservoir [kg/s]
REAL,DIMENSION(:,:), INTENT(OUT)     :: PSIN  ! Inflow to the surface river reservoir [kg/s]
REAL,DIMENSION(:,:), INTENT(OUT)     :: PGOUT      ! ground water outflow        [kg/s]
REAL,DIMENSION(:,:), INTENT(OUT)     :: PHS   ! River height [m]
REAL,DIMENSION(:,:), INTENT(OUT)     :: PVEL  ! River channel velocity  [m/s]
REAL,DIMENSION(:,:), INTENT(OUT)     :: PQFR  ! Flood flow to river
REAL,DIMENSION(:,:), INTENT(OUT)     :: PQRF  ! River flow to floodplain
!
REAL,DIMENSION(:,:), INTENT(OUT)     :: PVFIN ! River flow to flood velocity [m/s]
REAL,DIMENSION(:,:), INTENT(OUT)     :: PVFOUT! Flood flow to river velocity [m/s]
REAL,DIMENSION(:,:), INTENT(OUT)     :: PHSF  ! River-Floodplain depth comparison [m]
!
REAL,DIMENSION(:,:,:), INTENT(IN)    :: PTAB_F  ! Flood fraction array
REAL,DIMENSION(:,:,:), INTENT(IN)    :: PTAB_H  ! Topo height array
REAL,DIMENSION(:,:,:), INTENT(IN)    :: PTAB_VF ! Flood volume array
INTEGER, DIMENSION(:,:),INTENT(IN)   :: KTABMAX
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(SIZE(PLEN,1),SIZE(PLEN,2)) :: ZSURF_STO2  ! river channel storage at t+1     [kg]    
REAL, DIMENSION(SIZE(PLEN,1),SIZE(PLEN,2)) :: ZGROUND_STO2! Groundwater storage at t+1     [kg] 
REAL, DIMENSION(SIZE(PLEN,1),SIZE(PLEN,2)) :: ZFLOOD_STO2 ! Floodplain water storage at t+1  [kg]   
!
REAL    :: ZGSTO_ALL, ZGSTO2_ALL, ZGIN_ALL, ZGOUT_ALL, ZFSTO_ALL,   &
             ZFSTO2_ALL, ZSOURCE_ALL, ZSSTO_ALL, ZSSTO2_ALL, ZSIN_ALL,&
             ZDRUN_ALL, ZSOUT_ALL, ZVEL_ALL, ZFIN_ALL, ZFOUT_ALL,     &
             ZSFLOOD_ALL,ZHS_ALL,ZHF_ALL,ZFF_ALL  
!
INTEGER :: ILON, ILAT
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------!  
!
IF (LHOOK) CALL DR_HOOK('TRIP',0,ZHOOK_HANDLE)
ILON = SIZE(PLEN,1)
ILAT = SIZE(PLEN,2)
!
ZGSTO_ALL   = 0.0
ZGSTO2_ALL  = 0.0
ZGIN_ALL    = 0.0
ZGOUT_ALL   = 0.0
ZFSTO_ALL   = 0.0
ZFSTO2_ALL  = 0.0
ZSOURCE_ALL = 0.0
ZSSTO_ALL   = 0.0
ZSSTO2_ALL  = 0.0
ZSIN_ALL    = 0.0
ZDRUN_ALL   = 0.0
ZSOUT_ALL   = 0.0
ZVEL_ALL    = 0.0
ZFIN_ALL    = 0.0
ZFOUT_ALL   = 0.0
ZSFLOOD_ALL = 0.0
ZHS_ALL     = 0.0
ZHF_ALL     = 0.0
ZFF_ALL     = 0.0
!
ZSURF_STO2 = 0.0
ZGROUND_STO2 = 0.0
ZFLOOD_STO2 = 0.0
!
! * Ground water storage
!
IF(HGROUNDW/='DEF')THEN
!        
  CALL TRIP_GROUND_WATER(PTSTEP,PGROUND_STO,ZGROUND_STO2,PDRAIN,PTAUG, &
                           PGOUT,ZGSTO_ALL,ZGSTO2_ALL,ZGIN_ALL,ZGOUT_ALL )  
!            
  PGROUND_STO = ZGROUND_STO2  
!
ELSE
!
   PGOUT=PDRAIN
!
ENDIF
!
! * Surface water storage with or without Floodplains
!
IF(OFLOOD)THEN
!        
   CALL TRIP_SURFACE_WATER_FLOOD(KLUOUT,PTSTEP,KGRCN,KSEQ,KNEXTX,KNEXTY,         &
                                   KSEQMAX,PLEN,PFLOOD_LEN,PSLOPEBED,PWIDTH,     &
                                   PN,PN_FLOOD,PHC_BED,PHFLOOD,PRUNOFF,          &
                                   PSURF_STO,ZSURF_STO2,PGOUT,PSOURCE,           &
                                   PFLOOD_STO,ZFLOOD_STO2,PSOUT,PVEL,PHS,        &
                                   PFFLOOD,PAREA,PQFR,PQRF,                      &
                                   PVFIN,PVFOUT,PHSF,PSIN,                       &
                                   ZSSTO_ALL,ZSSTO2_ALL,ZSIN_ALL,ZDRUN_ALL,      &
                                   ZSOUT_ALL,ZVEL_ALL,ZFSTO_ALL,ZFSTO2_ALL,      &
                                   ZSOURCE_ALL,ZFIN_ALL,ZFOUT_ALL,PWFLOOD,       &
                                   ZSFLOOD_ALL,ZHS_ALL,PTAB_F,PTAB_H,PTAB_VF,    &
                                   KTABMAX                                       )  
!  
   PFLOOD_STO(:,:) = ZFLOOD_STO2(:,:)
!      
   IF(OPRINT)THEN
     ZHF_ALL = SUM(PHFLOOD(:,:)*PAREA(:,:),PWIDTH(:,:)>0.0)/SUM(PAREA(:,:),PWIDTH(:,:)>0.0)
     ZFF_ALL = SUM(PFFLOOD(:,:)*PAREA(:,:),PWIDTH(:,:)>0.0)/SUM(PAREA(:,:),PWIDTH(:,:)>0.0)
   ENDIF
!                                
ELSEIF(HVIT=='VAR')THEN
!        
   CALL TRIP_SURFACE_WATER_VELVAR(PTSTEP,KGRCN,KSEQ,KNEXTX,KNEXTY,KSEQMAX,      &
                                    PLEN,PSLOPEBED,PWIDTH,PN,PRUNOFF,PSURF_STO,   &
                                    ZSURF_STO2,PGOUT,PSIN,PSOUT,PVEL,PHS,PAREA,   &
                                    ZSSTO_ALL,ZSSTO2_ALL,ZSIN_ALL,ZDRUN_ALL,      &
                                    ZSOUT_ALL,ZVEL_ALL,ZHS_ALL                    )  
!
ELSE
!        
   CALL TRIP_SURFACE_WATER(PTSTEP,KGRCN,KSEQ,KNEXTX,KNEXTY,KSEQMAX, &
                             PLEN,PRUNOFF,PGOUT,PSURF_STO,ZSURF_STO2, &
                             PSIN,PSOUT,ZSSTO_ALL,ZSSTO2_ALL,ZSIN_ALL,&
                             ZDRUN_ALL,ZSOUT_ALL)  
!
ENDIF
!
PSURF_STO(:,:) = ZSURF_STO2(:,:)
!   
! * Writting the budget
!
IF(OPRINT.AND.KTRIP==1.AND.KTSEPT==1)THEN
!
  WRITE(KLUOUT,*)''
  WRITE(KLUOUT,*)'        START RUN ISBA-TRIP-FP     '
  WRITE(KLUOUT,*)'          Budget en 10^12 kg       '
  WRITE(KLUOUT,*)''
  WRITE(KLUOUT,'(a81)')'RUN TSTEP   S_err    F_err     G_err    MR(mm/y) Vel(m/s)  Hs      Hf         Ff'
  WRITE(KLUOUT,*)''
!  
ENDIF
!
IF(OPRINT) &
    WRITE(KLUOUT,'(i3,1x,i3,3(2x,g8.2),f8.2,f8.2,7(2x,g8.2))')                 &
    KTRIP,KTSEPT,                                                              &
! surface budget S_err
    (ZSSTO_ALL-ZSSTO2_ALL)+PTSTEP*(ZSIN_ALL-ZSOUT_ALL+ZSFLOOD_ALL),        &
! floodplains budget F_err
    (ZFSTO_ALL-ZFSTO2_ALL)+PTSTEP*(ZSOURCE_ALL+ZFIN_ALL-ZFOUT_ALL),        &
! ground budget G_err
    (ZGSTO_ALL-ZGSTO2_ALL)+PTSTEP*(ZGIN_ALL-ZGOUT_ALL),                    &
! output flow to the sea, MR(mm/y) 
    (ZSSTO_ALL-ZSSTO2_ALL +PTSTEP*ZDRUN_ALL)/XSEA_T*XYEAR_T*REAL(KTSTEP_END),&
! mean flow velocity, Vel(m/s), and mean stream depth, Hs (m) 
! mean floddplains depth, Hf (m), and mean flood fraction, Ff (%) 
    ZVEL_ALL,ZHS_ALL,ZHF_ALL,100.*ZFF_ALL  
IF (LHOOK) CALL DR_HOOK('TRIP',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
END SUBROUTINE TRIP
