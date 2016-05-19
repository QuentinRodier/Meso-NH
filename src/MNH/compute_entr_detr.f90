!     ######spl
     MODULE MODI_COMPUTE_ENTR_DETR
!    ##############################
!
INTERFACE
!
      SUBROUTINE COMPUTE_ENTR_DETR(KK,KKB,KKE,KKL,OTEST,OTESTLCL,HFRAC_ICE, &
                          PFRAC_ICE,PPABSM,PZZ,PDZZ,&
                          PTHVM,PTHLM,PRTM,PW_UP2,&
                          PTHL_UP,PRT_UP,PLUP,&
                          PRC_UP,PRI_UP,PRC_MIX,PRI_MIX,      &
                          PENTR,PDETR,PBUO_INTEG)

!
!
!
INTEGER,                INTENT(IN)   :: KK          ! near ground physical index
INTEGER,                INTENT(IN)   :: KKB          ! near ground physical index
INTEGER,                INTENT(IN)   :: KKE          ! uppest atmosphere physical index
INTEGER,                INTENT(IN)   :: KKL          ! +1 if grid goes from ground to atmosphere top, -1 otherwise
LOGICAL,DIMENSION(:),INTENT(INOUT)  :: OTEST
LOGICAL,DIMENSION(:),INTENT(INOUT)  :: OTESTLCL !test of condensation 
CHARACTER*1,INTENT(IN)              :: HFRAC_ICE 
REAL, DIMENSION(:)  ,INTENT(IN)     :: PFRAC_ICE
 
!
!    prognostic variables at t- deltat
REAL, DIMENSION(:,:), INTENT(IN) ::  PPABSM    ! Pressure
REAL, DIMENSION(:,:), INTENT(IN) ::  PZZ       !  Height at the flux point
REAL, DIMENSION(:,:), INTENT(IN) ::  PDZZ      !   metrics coefficient
REAL, DIMENSION(:,:), INTENT(IN) ::  PTHVM     ! ThetaV environment 

!
!
!   thermodynamical variables which are transformed in conservative var.
REAL, DIMENSION(:),   INTENT(IN)     ::  PTHLM        ! Thetal
REAL, DIMENSION(:),   INTENT(IN)     ::  PRTM         ! total mixing ratio
REAL, DIMENSION(:,:), INTENT(INOUT)  ::  PW_UP2       ! Vertical velocity^2
REAL, DIMENSION(:),   INTENT(IN)     ::  PTHL_UP,PRT_UP  ! updraft properties
REAL, DIMENSION(:),   INTENT(IN)     ::  PLUP         ! LUP compute from the ground
REAL, DIMENSION(:),   INTENT(IN)     ::  PRC_UP,PRI_UP   ! Updraft cloud content
REAL, DIMENSION(:),   INTENT(INOUT)  ::  PRC_MIX, PRI_MIX      ! Mixture cloud content
REAL, DIMENSION(:),   INTENT(INOUT)  ::  PENTR        ! Mass flux entrainment of the updraft
REAL, DIMENSION(:),   INTENT(INOUT)  ::  PDETR        ! Mass flux detrainment of the updraft
REAL, DIMENSION(:),   INTENT(INOUT)  ::  PBUO_INTEG   ! Integrated Buoyancy
!
!
END SUBROUTINE COMPUTE_ENTR_DETR

END INTERFACE
!
END MODULE MODI_COMPUTE_ENTR_DETR
!     ######spl
          SUBROUTINE COMPUTE_ENTR_DETR(KK,KKB,KKE,KKL,OTEST,OTESTLCL,&
                            HFRAC_ICE,PFRAC_ICE,PPABSM,PZZ,PDZZ,&
                            PTHVM,PTHLM,PRTM,PW_UP2,&
                            PTHL_UP,PRT_UP,PLUP,&
                            PRC_UP,PRI_UP,PRC_MIX,PRI_MIX,      &
                            PENTR,PDETR,PBUO_INTEG)
!         #############################################################

!!
!!***COMPUTE_ENTR_DETR* - calculates caracteristics of the updraft or downdraft
!!                       using model of the EDMF scheme 
!!
!!    PURPOSE
!!    -------
!!****  The purpose of this routine is to compute entrainement and
!!      detrainement at one level of the updraft
!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!      
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!     REFERENCE
!!     ---------
!!       Book 1 of Meso-NH documentation (chapter Convection)
!!       
!!
!!     AUTHOR
!!     ------
!!    J.Pergaud : 2009
!!
!!    MODIFICATIONS
!!    -------------
!!      Y.Seity (06/2010) Bug correction
!!      V.Masson (09/2010) Optimization
!!      S. Riette april 2011 : ice added, protection against zero divide by Yves Bouteloup
!!                             protection against too big ZPART_DRY, interface modified
!!      S. Riette Jan 2012: support for both order of vertical levels 
!!      S. Riette & J. Escobar (11/2013) : remove div by 0 on real*4 case
!! --------------------------------------------------------------------------
!
!*      0. DECLARATIONS
!          ------------
!                         
USE MODD_CST
!
USE MODD_PARAM_MFSHALL_n
!
USE MODI_TH_R_FROM_THL_RT_1D 

USE MODE_THERMO

IMPLICIT NONE
!
!                         
!*                    1.1  Declaration of Arguments
!
!
INTEGER,                INTENT(IN)   :: KK
INTEGER,                INTENT(IN)   :: KKB          ! near ground physical index
INTEGER,                INTENT(IN)   :: KKE          ! uppest atmosphere physical index
INTEGER,                INTENT(IN)   :: KKL          ! +1 if grid goes from ground to atmosphere top, -1 otherwise
LOGICAL,DIMENSION(:),INTENT(INOUT)  :: OTEST ! test to see if updraft is running
LOGICAL,DIMENSION(:),INTENT(INOUT)  :: OTESTLCL !test of condensation 
CHARACTER*1,INTENT(IN)              :: HFRAC_ICE ! frac_ice can be compute using
                                              ! Temperature (T) or prescribed
                                              ! (Y)
REAL, DIMENSION(:), INTENT(IN)      :: PFRAC_ICE ! fraction of ice
!
!    prognostic variables at t- deltat
!
REAL, DIMENSION(:,:),   INTENT(IN) ::  PPABSM      ! Pressure at time t-1
REAL, DIMENSION(:,:),   INTENT(IN) ::  PZZ       !  Height at the flux point
REAL, DIMENSION(:,:),   INTENT(IN) ::  PDZZ       !  metrics coefficient
REAL, DIMENSION(:,:),   INTENT(IN) ::  PTHVM      ! ThetaV environment 

!
!   thermodynamical variables which are transformed in conservative var.
!
REAL, DIMENSION(:),   INTENT(IN)     ::  PTHLM     ! Thetal
REAL, DIMENSION(:),   INTENT(IN)     ::  PRTM      ! total mixing ratio 
REAL, DIMENSION(:,:), INTENT(INOUT)  ::  PW_UP2    ! Vertical velocity^2
REAL, DIMENSION(:),   INTENT(IN)     ::  PTHL_UP,PRT_UP  ! updraft properties
REAL, DIMENSION(:),   INTENT(IN)     ::  PLUP      ! LUP compute from the ground
REAL, DIMENSION(:),   INTENT(IN)     ::  PRC_UP,PRI_UP   ! Updraft cloud content
REAL, DIMENSION(:),   INTENT(INOUT)  ::  PRC_MIX, PRI_MIX      ! Mixture cloud content
REAL, DIMENSION(:),   INTENT(INOUT)  ::  PENTR     ! Mass flux entrainment of the updraft
REAL, DIMENSION(:),   INTENT(INOUT)  ::  PDETR     ! Mass flux detrainment of the updraft
REAL, DIMENSION(:),   INTENT(INOUT)  ::  PBUO_INTEG! Integral Buoyancy
!
!
!                       1.2  Declaration of local variables
!
!

! Variables for cloudy part

REAL, DIMENSION(SIZE(PTHLM))   :: ZKIC           ! fraction of env. mass in the muxtures
REAL, DIMENSION(SIZE(PTHLM))   :: ZEPSI,ZDELTA   ! factor entrainment detrainment
REAL, DIMENSION(SIZE(PTHLM))   :: ZEPSI_CLOUD    ! factor entrainment detrainment
REAL, DIMENSION(SIZE(PTHLM))   :: ZCOEFFMF_CLOUD ! factor for compputing entr. detr.

REAL, DIMENSION(SIZE(PTHLM))   :: ZMIXTHL,ZMIXRT ! Thetal and rt in the mixtures
!
REAL, DIMENSION(SIZE(PTHLM))   :: ZTHMIX         ! Theta and Thetav  of mixtures
REAL, DIMENSION(SIZE(PTHLM))   :: ZRVMIX,ZRCMIX,ZRIMIX ! mixing ratios in mixtures

REAL, DIMENSION(SIZE(PTHLM))   :: ZTHMIX_F2     ! Theta and Thetav  of mixtures
REAL, DIMENSION(SIZE(PTHLM))   :: ZRVMIX_F2,ZRCMIX_F2,ZRIMIX_F2 ! mixing ratios in mixtures

REAL, DIMENSION(SIZE(PTHLM))   :: ZTHMIX_M2
REAL, DIMENSION(SIZE(PTHLM))   :: ZRVMIX_M2, ZRCMIX_M2, ZRIMIX_M2

REAL, DIMENSION(SIZE(PTHLM))   :: ZTHV_UP       ! thvup at mass point kk


REAL, DIMENSION(SIZE(PTHLM))   :: ZTHVMIX_1,ZTHVMIX_2 ! Theta and Thetav  of mixtures


! Variables for dry part

REAL, DIMENSION(SIZE(PTHLM))   :: ZBUO_INTEG,&         ! Temporary integral Buoyancy
                                  ZDZ_HALF,&           ! half-DeltaZ between 2 flux points
                                  ZDZ_STOP,&           ! Exact Height of the LCL 
                                  ZTHV_MINUS_HALF,&    ! Thv at flux point(kk)  
                                  ZTHV_PLUS_HALF,&     ! Thv at flux point(kk+kkl)
                                  ZCOEFF_MINUS_HALF,&  ! Variation of Thv between mass points kk-kkl and kk
                                  ZCOEFF_PLUS_HALF,&   ! Variation of Thv between mass points kk and kk+kkl
                                  ZCOTHVU_MINUS_HALF,& ! Variation of Thvup between flux point kk and mass point kk
                                  ZCOTHVU_PLUS_HALF,&  ! Variation of Thvup between mass point kk and flux point kk+kkl
                                  ZW2_HALF,&           ! w**2 at mass point KK  
                                  ZWK                  ! temp correction for Lup - z 

REAL, DIMENSION(SIZE(PTHLM))   :: ZCOPRE_MINUS_HALF,&  ! Variation of pressure between mass points kk-kkl and kk
                                  ZCOPRE_PLUS_HALF,&   ! Variation of pressure between mass points kk and kk+kkl
                                  ZPRE_MINUS_HALF,&    ! pressure at flux point kk
                                  ZPRE_PLUS_HALF,&     ! pressure at flux point kk+kkl
                                  ZTHV_UP_F1,&         ! thv_up at flux point kk
                                  ZTHV_UP_F2           ! thv_up at flux point kk+kkl
REAL, DIMENSION(SIZE(PTHLM))   :: ZCOEFF_QSAT,&        ! variation of Qsat at the transition between dry part and cloudy part
                                  ZRC_ORD,&            ! 
                                  ZPART_DRY            ! part of dry part at the transition level
!
REAL, DIMENSION(SIZE(PTHVM,1),SIZE(PTHVM,2))   ::ZG_O_THVREF
!
REAL, DIMENSION(SIZE(PTHLM))   :: ZFRAC_ICE            ! fraction of ice
REAL, DIMENSION(SIZE(PTHLM))   :: ZRSATW, ZRSATI
!
LOGICAL, DIMENSION(SIZE(OTEST,1)) :: GTEST_LOCAL_LCL,& ! true if LCL found between flux point KK and mass point KK  
                                     GTEST_LOCAL_LCL2  ! true if LCL found between mass point KK and flux point KK+KKL
!
REAL     :: ZRDORV       ! RD/RV
REAL     :: ZRVORD       ! RV/RD


!----------------------------------------------------------------------------------
                        
!                1.3 Initialisation
!                ------------------

  
  ZRDORV   = XRD / XRV   !=0.622
  ZRVORD   = XRV / XRD   !=1.607
  ZG_O_THVREF=XG/PTHVM
  
  ZCOEFF_QSAT=0.
  ZRC_ORD=0.
  ZPART_DRY=1.
  GTEST_LOCAL_LCL=.FALSE.
  ZDZ_HALF(:) = (PZZ(:,KK+KKL)-PZZ(:,KK))/2.
  ZDZ_STOP(:) = ZDZ_HALF(:)

  ZFRAC_ICE(:)=PFRAC_ICE(:) ! to not modify fraction of ice
 
  ZKIC(:)=0.1  ! starting value for critical mixed fraction for CLoudy Part


!                Computation of KIC
!                ---------------------

!        2.1    Compute critical mixed fraction by estimating unknown  
!               T^mix r_c^mix and r_i^mix from thl^mix and r_t^mix
!               We determine the zero crossing of the linear curve
!               evaluating the derivative using ZMIXF=0.1.
!               -----------------------------------------------------

  ZMIXTHL(:) = ZKIC(:) * PTHLM(:)+(1. - ZKIC(:))*PTHL_UP(:)
  ZMIXRT(:)  = ZKIC(:) * PRTM(:)+(1. - ZKIC(:))*PRT_UP(:)

  ! MIXTURE FOR CLOUDY PART
  !  Compute pressure at flux level KK
  ZCOPRE_PLUS_HALF(:) = ((PPABSM(:,KK+KKL)-PPABSM(:,KK))/PDZZ(:,KK+KKL))
  ZPRE_PLUS_HALF(:) = ZCOPRE_PLUS_HALF*0.5*(PZZ(:,KK+KKL)-PZZ(:,KK))+PPABSM(:,KK)

  !  Compute pressure at flux level KK+KKL
  IF(KK/=KKB)THEN
    ZCOPRE_MINUS_HALF(:) = ((PPABSM(:,KK)-PPABSM(:,KK-KKL))/PDZZ(:,KK))
    ZPRE_MINUS_HALF(:)= ZCOPRE_MINUS_HALF*0.5*(PZZ(:,KK)-PZZ(:,KK-KKL))+PPABSM(:,KK-KKL)
  ELSE
    ZPRE_MINUS_HALF(:)=PPABSM(:,KK)
  ENDIF

  !  Compute non cons. var. of mixture at the mass level
  CALL TH_R_FROM_THL_RT_1D(HFRAC_ICE,ZFRAC_ICE,&
               PPABSM(:,KK),ZMIXTHL,ZMIXRT,&
               ZTHMIX,ZRVMIX,PRC_MIX,PRI_MIX,&
               ZRSATW, ZRSATI)

  ! Compute theta_v of mixture at mass level KK for KF90        
  ZTHVMIX_1(:) = ZTHMIX(:)*(1.+ZRVORD*ZRVMIX(:))/(1.+ZMIXRT(:))

  !  Compute non cons. var. of mixture at the flux level KK+KKL
  ZRCMIX=PRC_MIX
  ZRIMIX=PRI_MIX
  CALL TH_R_FROM_THL_RT_1D(HFRAC_ICE,ZFRAC_ICE,&
               ZPRE_PLUS_HALF,ZMIXTHL,ZMIXRT,&
               ZTHMIX,ZRVMIX,ZRCMIX,ZRIMIX,&
               ZRSATW, ZRSATI)
             

  ! compute theta_v of mixture at the flux level KK+KKL for KF90       
  ZTHVMIX_2(:) = ZTHMIX(:)*(1.+ZRVORD*ZRVMIX(:))/(1.+ZMIXRT(:))


!        2.1    Compute critical mixed fraction by estimating unknown  
!               T^mix r_c^mix and r_i^mix from thl^mix and r_t^mix
!               We determine the zero crossing of the linear curve
!               evaluating the derivative using ZMIXF=0.1.
!               -----------------------------------------------------


! THV_UP FOR DRY PART
  ! Compute theta_v of updraft at flux level KK                 
  ZRCMIX=PRC_UP
  ZRIMIX=PRI_UP
  CALL TH_R_FROM_THL_RT_1D(HFRAC_ICE,ZFRAC_ICE,&
               ZPRE_MINUS_HALF,PTHL_UP,PRT_UP,&
               ZTHMIX,ZRVMIX,ZRCMIX,ZRIMIX,&
               ZRSATW, ZRSATI)
  ZTHV_UP_F1(:) = ZTHMIX(:)*(1.+ZRVORD*ZRVMIX(:))/(1.+PRT_UP(:))
 
  ! Compute theta_v of updraft at mass level KK                 
  ZRCMIX=PRC_UP
  ZRIMIX=PRI_UP
  CALL TH_R_FROM_THL_RT_1D(HFRAC_ICE,ZFRAC_ICE,&
               PPABSM(:,KK),PTHL_UP,PRT_UP,&
               ZTHMIX,ZRVMIX,ZRCMIX,ZRIMIX,&
               ZRSATW, ZRSATI)
  ZTHV_UP(:) = ZTHMIX(:)*(1.+ZRVORD*ZRVMIX(:))/(1.+PRT_UP(:))

  ! Compute theta_v of updraft at flux level KK+KKL                   
  ZRCMIX_F2=PRC_UP
  ZRIMIX_F2=PRI_UP
  CALL TH_R_FROM_THL_RT_1D(HFRAC_ICE,ZFRAC_ICE,&
               ZPRE_PLUS_HALF,PTHL_UP,PRT_UP,&
               ZTHMIX_F2,ZRVMIX_F2,ZRCMIX_F2,ZRIMIX_F2,&
               ZRSATW, ZRSATI)
  ZTHV_UP_F2(:) = ZTHMIX_F2(:)*(1.+ZRVORD*ZRVMIX_F2(:))/(1.+PRT_UP(:))

  ! Computation of RC and RI on mass point KK+KKL
  ZRCMIX_M2=PRC_UP
  ZRIMIX_M2=PRI_UP
  CALL TH_R_FROM_THL_RT_1D(HFRAC_ICE,ZFRAC_ICE,&
               PPABSM(:,KK+KKL),PTHL_UP,PRT_UP,&
               ZTHMIX_M2,ZRVMIX_M2,ZRCMIX_M2,ZRIMIX_M2,&
               ZRSATW, ZRSATI)
   
!
!*   2.2     Compute final values for entr. and detr. 
!            ----------------------------------------
!
! Dry PART  

 ! Computation of integral entrainment and detrainment between flux level KK
 ! and mass level KK 

  WHERE ((ZRCMIX(:)+ZRIMIX(:)>0.).AND.(.NOT.OTESTLCL))
! If rc and/or ri is found between flux level KK and mass level KK
! a part of dry entrainment/detrainment is defined
! the exact height of LCL is also determined
     ZCOEFF_QSAT(:) = ((ZRCMIX_F2(:)+ZRIMIX_F2(:)) - (ZRCMIX(:)+ZRIMIX(:)))/ ZDZ_HALF(:)
     WHERE ((ZCOEFF_QSAT(:)>0.) .OR. (ZCOEFF_QSAT(:)<0.))
       ZRC_ORD(:) = (ZRCMIX(:)+ZRIMIX(:)) - ZCOEFF_QSAT(:) * ZDZ_HALF(:)
       ZDZ_STOP = (- ZRC_ORD(:)/ZCOEFF_QSAT(:))
       ZPART_DRY(:) = MAX(MIN(ZDZ_STOP / (PZZ(:,KK+KKL)-PZZ(:,KK)),0.5),0.)
       GTEST_LOCAL_LCL(:)=.TRUE.
     ENDWHERE
  ENDWHERE
  
  IF(KK/=KKB)THEN
    ZCOEFF_MINUS_HALF = ((PTHVM(:,KK)-PTHVM(:,KK-KKL))/PDZZ(:,KK))
  ELSE
    ZCOEFF_MINUS_HALF = 0.
  ENDIF
  ZCOEFF_PLUS_HALF  = ((PTHVM(:,KK+KKL)-PTHVM(:,KK))/PDZZ(:,KK+KKL))

  ZCOTHVU_MINUS_HALF = (ZTHV_UP(:)-ZTHV_UP_F1(:))/ZDZ_HALF(:)
  ZCOTHVU_PLUS_HALF  = (ZTHV_UP_F2(:)-ZTHV_UP(:))/ZDZ_HALF(:)

  IF(KK/=KKB)THEN
    ZTHV_MINUS_HALF = ZCOEFF_MINUS_HALF*0.5*(PZZ(:,KK)-PZZ(:,KK-KKL))+PTHVM(:,KK-KKL)
    ZTHV_PLUS_HALF  = ZCOEFF_PLUS_HALF*0.5*(PZZ(:,KK)-PZZ(:,KK-KKL))+ ZTHV_MINUS_HALF
  ELSE
    ZTHV_MINUS_HALF = PTHVM(:,KK)
    ZTHV_PLUS_HALF  = ZCOEFF_PLUS_HALF*0.5*(PZZ(:,KK+KKL)-PZZ(:,KK))+ ZTHV_MINUS_HALF !according to PZZ computation at KKB-KKL
  ENDIF

  ! Integral Buoyancy between flux level KK and mass level KK 
  PBUO_INTEG = ZG_O_THVREF(:,KK)*ZDZ_HALF(:)*&
              (0.5*( ZCOTHVU_MINUS_HALF - ZCOEFF_MINUS_HALF)*ZDZ_HALF(:) &
                - ZTHV_MINUS_HALF + ZTHV_UP_F1(:) ) 
 
  WHERE ((OTEST).AND.(.NOT.OTESTLCL))
     PENTR=0.
     PDETR=0.
 
     ZBUO_INTEG = ZG_O_THVREF(:,KK)*ZDZ_STOP(:)*&
                (0.5 * (  - ZCOEFF_MINUS_HALF)* ZDZ_STOP(:) &
                  - ZTHV_MINUS_HALF + ZTHV_UP_F1(:) ) 
     WHERE (ZBUO_INTEG(:)>=0.)
         PENTR = 0.5/(XABUO-XBENTR*XENTR_DRY)*&
                 LOG(1.+ (2.*(XABUO-XBENTR*XENTR_DRY)/PW_UP2(:,KK))* &
                 ZBUO_INTEG)
         PDETR = 0.
    
         ZW2_HALF = PW_UP2(:,KK) +  2*(XABUO-XBENTR*XENTR_DRY)*(ZBUO_INTEG)
     ELSEWHERE
         PENTR = 0.
         PDETR = 0.5/(XABUO)*&
                 LOG(1.+ (2.*(XABUO)/PW_UP2(:,KK))* &
                 MAX(0.,-ZBUO_INTEG))

         ZW2_HALF = PW_UP2(:,KK) +  2*(XABUO)*(ZBUO_INTEG)
     ENDWHERE
 ENDWHERE

 
 ZDZ_STOP(:) = ZDZ_HALF(:)
  
! total Integral Buoyancy between flux level KK and flux level KK+KKL
 PBUO_INTEG = PBUO_INTEG + ZG_O_THVREF(:,KK)*ZDZ_HALF(:)*&
                (0.5*(ZCOTHVU_PLUS_HALF - ZCOEFF_PLUS_HALF)* ZDZ_HALF(:) - & 
                PTHVM(:,KK) + ZTHV_UP(:) ) 

 IF(KK*KKL<(KKE-KKL)*KKL) THEN !Computation only if we are strictly below KKE-KKL
   WHERE ((((ZRCMIX_F2(:)+ZRIMIX_F2(:)>0.).AND.(ZRCMIX(:)+ZRIMIX(:)<=0.)).AND.(.NOT.OTESTLCL)).AND.(.NOT.GTEST_LOCAL_LCL(:)))
     ! If rc and/or ri is found between mass level KK and flux level KK+KKL
     ! a part of dry entrainment is defined
     ! the exact height of LCL is also determined
     ZCOEFF_QSAT(:) = ((ZRCMIX_M2(:)+ZRIMIX_M2(:)) - (ZRCMIX_F2(:)+ZRIMIX_F2(:))) / &
                    & (0.5* (PZZ(:,KK+2*KKL)-PZZ(:,KK+KKL)))
     !old formulation without ice (and perhaps with errors)
     !ZCOEFF_QSAT(:) = (PRT_UP(:) - &
     !           QSAT(ZTHMIX_F2(:)*((PPABSM(:,KK+KKL)/XP00)**(XRD/XCPD)),&
     !           PPABSM(:,KK+KKL)) - &
     !           ZRCMIX(:))/ (0.5* (PZZ(:,KK+2*KKL)-PZZ(:,KK+KKL)))
     WHERE ((ZCOEFF_QSAT(:)>0.) .OR. (ZCOEFF_QSAT(:)<0.))
       ZRC_ORD(:) = ZRCMIX_F2(:)+ZRIMIX_F2(:) - ZCOEFF_QSAT(:) * ZDZ_HALF(:)
       ZDZ_STOP = (- ZRC_ORD(:)/ZCOEFF_QSAT(:))
       ZPART_DRY(:) = 0.5+MAX(MIN(ZDZ_STOP  / (PZZ(:,KK+KKL)-PZZ(:,KK)),0.5),0.)
       GTEST_LOCAL_LCL2(:)=.TRUE.
     ENDWHERE
   ENDWHERE
 ENDIF

 WHERE (((OTEST).AND.(.NOT.OTESTLCL)).AND.(.NOT.GTEST_LOCAL_LCL(:)))
     ZBUO_INTEG = ZG_O_THVREF(:,KK)*ZDZ_STOP(:)*&
                (0.5*( - ZCOEFF_PLUS_HALF)* ZDZ_STOP(:)&
                - PTHVM(:,KK) + ZTHV_UP(:) )

     WHERE (ZW2_HALF>0.)
        WHERE (ZBUO_INTEG(:)>=0.)
           PENTR = PENTR + 0.5/(XABUO-XBENTR*XENTR_DRY)* &
                LOG(1.+ (2.*(XABUO-XBENTR*XENTR_DRY)/ZW2_HALF(:)) * ZBUO_INTEG)
          
           PDETR = PDETR
        ELSEWHERE
          PENTR = PENTR
          PDETR = PDETR + 0.5/(XABUO)* &
                LOG(1.+ (2.*(XABUO)/ZW2_HALF(:)) * &
                MAX(-ZBUO_INTEG,0.))
        ENDWHERE     
     ELSEWHERE
        ! if w**2<0 the updraft is stopped 
           OTEST=.FALSE.
           PENTR = PENTR 
           PDETR = PDETR 
     ENDWHERE
 ENDWHERE
 PENTR = XENTR_DRY*PENTR/(PZZ(:,KK+KKL)-PZZ(:,KK))    
 PDETR = XDETR_DRY*PDETR/(PZZ(:,KK+KKL)-PZZ(:,KK))

 ZWK(:)=PLUP(:) - 0.5*(PZZ(:,KK)+PZZ(:,KK+KKL))
 ZWK(:)=SIGN(MAX(1., ABS(ZWK(:))), ZWK(:))
 PDETR(:)=MAX(ZPART_DRY(:)*XDETR_LUP/ZWK(:),PDETR(:)) 

! compute final value of critical mixed fraction using theta_v
! of mixture, grid-scale and updraft in cloud
 WHERE ((OTEST).AND.(OTESTLCL))
     ZKIC(:) = MAX(0.,ZTHV_UP(:)-PTHVM(:,KK))*ZKIC(:) /  &  
                 (ZTHV_UP(:)-ZTHVMIX_1(:)+XMNH_EPSILON)
                       
     ZKIC(:) = MAX(0., MIN(1., ZKIC(:)))
    
     ZEPSI(:) = ZKIC(:) **2.
     ZDELTA(:) = (1.-ZKIC(:))**2.
     ZEPSI_CLOUD=MIN(ZDELTA,ZEPSI)
     ZCOEFFMF_CLOUD(:)=XENTR_MF * XG / XCRAD_MF         
     PENTR(:) = ZCOEFFMF_CLOUD(:)*ZEPSI_CLOUD(:)
     PDETR(:) = ZCOEFFMF_CLOUD(:)*ZDELTA(:)
 ENDWHERE
 
! compute final value of critical mixed fraction using theta_v
! of mixture, grid-scale and updraft in cloud
 WHERE (((OTEST).AND.(.NOT.(OTESTLCL))).AND.((GTEST_LOCAL_LCL(:).OR.GTEST_LOCAL_LCL2(:))))
     ZKIC(:) = MAX(0.,ZTHV_UP_F2(:)-ZTHV_PLUS_HALF)*ZKIC(:) /  &  
                       (ZTHV_UP_F2(:)-ZTHVMIX_2(:)+XMNH_EPSILON)                      
     ZKIC(:) = MAX(0., MIN(1., ZKIC(:)))
     ZEPSI(:) = ZKIC(:) **2.
     ZDELTA(:) = (1.-ZKIC(:))**2.
     ZEPSI_CLOUD=MIN(ZDELTA,ZEPSI)
     ZCOEFFMF_CLOUD(:)=XENTR_MF * XG / XCRAD_MF     
     PENTR(:) = PENTR+(1.-ZPART_DRY(:))*ZCOEFFMF_CLOUD(:)*ZEPSI_CLOUD(:)
     PDETR(:) = PDETR+(1.-ZPART_DRY(:))*ZCOEFFMF_CLOUD(:)*ZDELTA(:)
 ENDWHERE

END SUBROUTINE COMPUTE_ENTR_DETR  
