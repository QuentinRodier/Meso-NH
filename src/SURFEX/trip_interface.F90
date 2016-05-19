!####################################################################
SUBROUTINE TRIP_INTERFACE (KLUOUT,KLON,KLAT,ORESTART,KYEAR,KMONTH,  &
                            KTRIP,PDURATION,PRUNOFF,PDRAIN,PSRC_FLOOD) 
!####################################################################
!
!!****  *TRIP*  
!!
!!    PURPOSE
!!    -------
!
!     Driver for the TRIP river routing.
!     Here, we call the physical and the diag routines     
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
USE MODD_TRIP_PAR,    ONLY : XRHOLW_T
USE MODD_TRIP_GRID_n, ONLY : XAREA
USE MODD_DIAG_TRIP_n
USE MODD_TRIP_n
!
!
USE MODI_TRIP
USE MODI_DIAG_TRIP_n
USE MODI_RESTART_TRIP_n
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
LOGICAL, INTENT(IN)                  :: ORESTART
!
INTEGER, INTENT(IN)                  :: KLON
INTEGER, INTENT(IN)                  :: KLAT
INTEGER, INTENT(IN)                  :: KLUOUT
INTEGER, INTENT(IN)                  :: KYEAR
INTEGER, INTENT(IN)                  :: KMONTH
INTEGER, INTENT(IN)                  :: KTRIP
!
REAL,    INTENT(IN)                  :: PDURATION
!
REAL, DIMENSION(KLON,KLAT), INTENT(INOUT)  :: PRUNOFF
REAL, DIMENSION(KLON,KLAT), INTENT(INOUT)  :: PDRAIN
!
REAL, DIMENSION(KLON,KLAT), INTENT(INOUT),OPTIONAL  :: PSRC_FLOOD
!
!*      0.2    declarations of local variables
!
INTEGER, DIMENSION(5,2) :: ISIZE
INTEGER :: JTSTEP, ITRIP_TSTEP
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
! Allocate local variables
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('TRIP_INTERFACE',0,ZHOOK_HANDLE)
!
ISIZE(:,:) = 0
!
IF (LTRIP_DIAG_MISC) THEN
  ISIZE(1,1) = KLON
  ISIZE(1,2) = KLAT
ENDIF
!
IF (CGROUNDW/='DEF') THEN
  ISIZE(2,1) = KLON
  ISIZE(2,2) = KLAT
ENDIF
!
IF (CVIT=='VAR') THEN
  ISIZE(3,1) = KLON
  ISIZE(3,2) = KLAT
ENDIF
!
IF (LFLOODT) THEN
  !
  IF (.NOT.PRESENT(PSRC_FLOOD)) THEN
    WRITE(KLUOUT,*)'!Probleme when you call the coupling_trip_n.f90 routine!!!!'
    STOP 
  ENDIF  
  !  
  ISIZE(4,1) = KLON
  ISIZE(4,2) = KLAT
  !
  IF(LTRIP_DIAG_MISC)THEN
    ISIZE(5,1) = KLON
    ISIZE(5,2) = KLAT
  ENDIF
ENDIF
!
 CALL TRIP_INTERFACE_DIM(ISIZE)
!
!-------------------------------------------------------------------------------
!Write restart
!-------------------------------------------------------------------------------
!
IF(ORESTART.AND.KTRIP==INT(PDURATION/XTSTEP_COUPLING))THEN
   CALL RESTART_TRIP_n(KLUOUT)
ENDIF
!
IF (LHOOK) CALL DR_HOOK('TRIP_INTERFACE',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
CONTAINS
!
SUBROUTINE TRIP_INTERFACE_DIM(KSIZE)
!
IMPLICIT NONE
!
INTEGER, DIMENSION(:,:), INTENT(IN) :: KSIZE
!
REAL, DIMENSION(KLON,KLAT) :: ZGOUT          
REAL, DIMENSION(KLON,KLAT) :: ZSOUT   
REAL, DIMENSION(KLON,KLAT) :: ZSIN
REAL, DIMENSION(KLON,KLAT) :: ZSURF_STO_OLD  
REAL, DIMENSION(KLON,KLAT) :: ZQDIS_OLD      
!
REAL, DIMENSION(KSIZE(1,1),KSIZE(1,2)) :: ZQIN_OLD
!
REAL, DIMENSION(KSIZE(2,1),KSIZE(2,2)) :: ZGROUND_STO_OLD
REAL, DIMENSION(KSIZE(2,1),KSIZE(2,2)) :: ZQGF_OLD  
!
REAL, DIMENSION(KSIZE(3,1),KSIZE(3,2)) :: ZVEL           
REAL, DIMENSION(KSIZE(3,1),KSIZE(3,2)) :: ZHS            
REAL, DIMENSION(KSIZE(3,1),KSIZE(3,2)) :: ZVEL_OLD       
REAL, DIMENSION(KSIZE(3,1),KSIZE(3,2)) :: ZHS_OLD  
!
REAL, DIMENSION(KSIZE(4,1),KSIZE(4,2)) :: ZSOURCE
REAL, DIMENSION(KSIZE(4,1),KSIZE(4,2)) :: ZQFR           
REAL, DIMENSION(KSIZE(4,1),KSIZE(4,2)) :: ZQRF           
REAL, DIMENSION(KSIZE(4,1),KSIZE(4,2)) :: ZVFIN
REAL, DIMENSION(KSIZE(4,1),KSIZE(4,2)) :: ZVFOUT
REAL, DIMENSION(KSIZE(4,1),KSIZE(4,2)) :: ZHSF
REAL, DIMENSION(KSIZE(4,1),KSIZE(4,2)) :: ZFLOOD_STO_OLD 
REAL, DIMENSION(KSIZE(4,1),KSIZE(4,2)) :: ZFF_OLD        
REAL, DIMENSION(KSIZE(4,1),KSIZE(4,2)) :: ZHF_OLD        
!
REAL, DIMENSION(KSIZE(5,1),KSIZE(5,2)) :: ZQFR_OLD       
REAL, DIMENSION(KSIZE(5,1),KSIZE(5,2)) :: ZQRF_OLD
REAL, DIMENSION(KSIZE(5,1),KSIZE(5,2)) :: ZVFIN_OLD
REAL, DIMENSION(KSIZE(5,1),KSIZE(5,2)) :: ZVFOUT_OLD
REAL, DIMENSION(KSIZE(5,1),KSIZE(5,2)) :: ZWF_OLD
REAL, DIMENSION(KSIZE(5,1),KSIZE(5,2)) :: ZLF_OLD
REAL, DIMENSION(KSIZE(5,1),KSIZE(5,2)) :: ZHSF_OLD
REAL, DIMENSION(KSIZE(5,1),KSIZE(5,2)) :: ZSOURCE_OLD
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('TRIP_INTERFACE:TRIP_INTERFACE_DIM',0,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!Put ISBA variables in TRIP dimension (kg --> kg/s)
!-------------------------------------------------------------------------------
!
PRUNOFF(:,:) = PRUNOFF(:,:) / XTSTEP_COUPLING
PDRAIN (:,:) = PDRAIN (:,:) / XTSTEP_COUPLING
!
!-------------------------------------------------------------
!
ZGOUT        (:,:) = 0.0
ZSIN         (:,:) = 0.0
ZSOUT        (:,:) = 0.0
ZSURF_STO_OLD(:,:) = XDIAG_SURF_STO
ZQDIS_OLD    (:,:) = XDIAG_QDIS
!
IF(LTRIP_DIAG_MISC) ZQIN_OLD(:,:) = XDIAG_QIN
!
! No groundwater treatement (e.g. permafrost)
!
IF(CGROUNDW/='DEF')THEN
  !
  ZGROUND_STO_OLD(:,:) = XDIAG_GROUND_STO
  ZQGF_OLD       (:,:) = XDIAG_QGF 
  !
  WHERE(XTAUG(:,:)==0.0)
    PRUNOFF(:,:) = PRUNOFF(:,:)+PDRAIN(:,:)
    PDRAIN(:,:)  = 0.0
  ENDWHERE  
  !
ENDIF
!
IF(CVIT=='VAR')THEN
  ZVEL    (:,:) = 0.0
  ZHS     (:,:) = 0.0
  ZVEL_OLD(:,:) = XDIAG_VEL
  ZHS_OLD (:,:) = XDIAG_HS
ENDIF
!
IF(LFLOODT)THEN
  !  
  ZSOURCE(:,:) = PSRC_FLOOD(:,:) / XTSTEP_COUPLING
  WHERE(XFFLOOD(:,:)==1.0.AND.ZSOURCE(:,:)>0.0)
        PRUNOFF(:,:) = PRUNOFF(:,:) + ZSOURCE(:,:)
        ZSOURCE(:,:) = 0.0
  ENDWHERE  
  !
  ZQFR          (:,:) = 0.0
  ZQRF          (:,:) = 0.0
  ZVFIN         (:,:) = 0.0
  ZVFOUT        (:,:) = 0.0
  ZHSF          (:,:) = 0.0
  ZFLOOD_STO_OLD(:,:) = XDIAG_FLOOD_STO
  ZFF_OLD       (:,:) = XDIAG_FF
  ZHF_OLD       (:,:) = XDIAG_HF
  !

  IF(LTRIP_DIAG_MISC)THEN
    ZQFR_OLD      (:,:) = XDIAG_QFR
    ZQRF_OLD      (:,:) = XDIAG_QRF
    ZVFIN_OLD     (:,:) = XDIAG_VFIN
    ZVFOUT_OLD    (:,:) = XDIAG_VFOUT
    ZWF_OLD       (:,:) = XDIAG_WF
    ZLF_OLD       (:,:) = XDIAG_LF
    ZHSF_OLD      (:,:) = XDIAG_HSF
    ZSOURCE_OLD   (:,:) = XDIAG_SOURCE
  ENDIF
!
ENDIF
!
!-------------------------------------------------------------------------------
!Call physical routines
!-------------------------------------------------------------------------------
!
ITRIP_TSTEP = INT(XTSTEP_COUPLING/XTRIP_TSTEP)
!
DO JTSTEP=1,ITRIP_TSTEP
!   
! * TRIP time step loop
!
  CALL TRIP(KLUOUT,CGROUNDW,CVIT,LFLOODT,LPRINT_TRIP,        &
            XTRIP_TSTEP,IGRCN,ISEQ,INEXTX,INEXTY,ISEQMAX,    &
            XTAUG,XAREA,XLEN,XFLOOD_LEN,XSLOPEBED,XWIDTH,XN, &
            XN_FLOOD,XHC_BED,XWFLOOD,XTAB_F,XTAB_H,XTAB_VF,  &
            PDRAIN,PRUNOFF,ZSOURCE,XGROUND_STO,XSURF_STO,    &
            XFLOOD_STO,ZSOUT,ZGOUT,ZHS,XHFLOOD,ZVEL,         &
            XFFLOOD,ZQFR,ZQRF,ZVFIN,ZVFOUT,                  &
            ZHSF,ZSIN,KTRIP,JTSTEP,ITRIP_TSTEP,ITABMAX       ) 
!
! * Actualisation of diagnostic variables   
!        
  XDIAG_QDIS(:,:) = XDIAG_QDIS(:,:) + ZSOUT(:,:) * XTRIP_TSTEP / XRHOLW_T
!   
  IF(CGROUNDW/='DEF')THEN
    XDIAG_QGF(:,:) = XDIAG_QGF(:,:)  + ZGOUT(:,:) * XTRIP_TSTEP / XRHOLW_T
  ENDIF
!           
  IF(LTRIP_DIAG_MISC)THEN
    XDIAG_QIN (:,:) = XDIAG_QIN (:,:) + ZSIN (:,:) * XTRIP_TSTEP / XRHOLW_T
    IF(LFLOODT)THEN
      XDIAG_QFR(:,:) = XDIAG_QFR(:,:) + ZQFR(:,:) * XTRIP_TSTEP / XRHOLW_T
      XDIAG_QRF(:,:) = XDIAG_QRF(:,:) + ZQRF(:,:) * XTRIP_TSTEP / XRHOLW_T
    ENDIF
  ENDIF
!
ENDDO ! * End TRIP time step loop
!
XDIAG_SURF_STO(:,:) = XDIAG_SURF_STO(:,:) + XSURF_STO(:,:) / XAREA(:,:)
!
IF(CGROUNDW/='DEF')THEN
  XDIAG_GROUND_STO(:,:) = XDIAG_GROUND_STO(:,:) + XGROUND_STO(:,:) / XAREA(:,:)
ENDIF
!
IF(CVIT=='VAR')THEN
   XDIAG_VEL(:,:) = XDIAG_VEL(:,:) + ZVEL(:,:)
   XDIAG_HS (:,:) = XDIAG_HS (:,:) + ZHS (:,:)
ENDIF

IF(LFLOODT)THEN
!          
   XDIAG_FLOOD_STO(:,:) = XDIAG_FLOOD_STO(:,:) + XFLOOD_STO(:,:) / XAREA(:,:)
   XDIAG_FF       (:,:) = XDIAG_FF       (:,:) + XFFLOOD (:,:)
   XDIAG_HF       (:,:) = XDIAG_HF       (:,:) + XHFLOOD (:,:)
!
  IF(LTRIP_DIAG_MISC)THEN
     XDIAG_VFIN  (:,:) = XDIAG_VFIN  (:,:) + ZVFIN     (:,:)
     XDIAG_VFOUT (:,:) = XDIAG_VFOUT (:,:) + ZVFOUT    (:,:)
     XDIAG_WF    (:,:) = XDIAG_WF    (:,:) + XWFLOOD   (:,:)
     XDIAG_LF    (:,:) = XDIAG_LF    (:,:) + XFLOOD_LEN(:,:)
     XDIAG_HSF   (:,:) = XDIAG_HSF   (:,:) + ZHSF      (:,:)
     XDIAG_SOURCE(:,:) = XDIAG_SOURCE(:,:) + ZSOURCE   (:,:) * XTSTEP_COUPLING / XAREA(:,:)
  ENDIF
!
! Coupling variables
!
  XPIFLOOD(:,:)=XFLOOD_STO(:,:)
!
ENDIF       
!
!-------------------------------------------------------------------------------
!Store result
!-------------------------------------------------------------------------------
!
IF(LPRINT_TRIP)WRITE(KLUOUT,*)''
!
 CALL DIAG_TRIP_n(KLUOUT,KYEAR,KMONTH,KTRIP,PDURATION,      &
                  ZSURF_STO_OLD,ZQDIS_OLD,ZGROUND_STO_OLD, &
                  ZQGF_OLD,ZVEL_OLD,ZHS_OLD,ZFLOOD_STO_OLD,&
                  ZFF_OLD,ZHF_OLD,ZQFR_OLD,ZQRF_OLD,       &
                  ZVFIN_OLD,ZVFOUT_OLD,ZWF_OLD,ZLF_OLD,    &
                  ZHSF_OLD,ZQIN_OLD,ZSOURCE_OLD            ) 
!
IF (LHOOK) CALL DR_HOOK('TRIP_INTERFACE:TRIP_INTERFACE_DIM',1,ZHOOK_HANDLE)
!
END SUBROUTINE TRIP_INTERFACE_DIM
!
END SUBROUTINE TRIP_INTERFACE
