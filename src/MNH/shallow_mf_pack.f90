!MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!    ######################
     MODULE MODI_SHALLOW_MF_PACK
!    ######################
!
INTERFACE
!     #################################################################
      SUBROUTINE SHALLOW_MF_PACK(KRR,KRRL,KRRI,                       &
                HMF_UPDRAFT, HMF_CLOUD, OMIXUV,                       &
                OCLOSE_OUT,OMF_FLX,HFMFILE,HLUOUT,PTIME_LES,          &
                PIMPL_MF, PTSTEP,                                     &
                PDZZ, PZZ,                                            &
                PRHODJ, PRHODREF,                                     &
                PPABSM, PEXN,                                         &
                PSFTH,PSFRV,                                          &
                PTHM,PRM,PUM,PVM,PTKEM,PSVM,                          &
                PRTHS,PRRS,PRUS,PRVS,PRSVS,                           &
                PSIGMF,PRC_MF,PRI_MF,PCF_MF,PFLXZTHVMF  )
!     #################################################################
!!
!               
!*               1.1  Declaration of Arguments
!                
!
INTEGER,                INTENT(IN)   :: KRR          ! number of moist var.
INTEGER,                INTENT(IN)   :: KRRL         ! number of liquid water var.
INTEGER,                INTENT(IN)   :: KRRI         ! number of ice water var.
CHARACTER (LEN=4),      INTENT(IN)   :: HMF_UPDRAFT  ! Type of Mass Flux Scheme
                                     ! 'NONE' if no parameterization 
CHARACTER (LEN=4),      INTENT(IN)   :: HMF_CLOUD    ! Type of statistical cloud
                                                     ! scheme
LOGICAL,                INTENT(IN)   :: OMIXUV    ! True if mixing of momentum
LOGICAL,                INTENT(IN)   ::  OCLOSE_OUT   ! switch for synchronous
                                                      ! file opening
LOGICAL,                INTENT(IN)   ::  OMF_FLX      ! switch to write the
                                 ! MF fluxes in the synchronous FM-file
CHARACTER(LEN=*),       INTENT(IN)   ::  HFMFILE      ! Name of the output
                                                      ! FM-file
CHARACTER(LEN=*),       INTENT(IN)   ::  HLUOUT       ! Output-listing name for
                                                      ! model n
REAL*8,DIMENSION(2),                  INTENT(OUT)  :: PTIME_LES     ! time spent in LES computations
REAL,                   INTENT(IN)   :: PIMPL_MF     ! degre of implicitness
REAL,              INTENT(IN)     ::  PTSTEP   ! Dynamical timestep 

REAL, DIMENSION(:,:,:), INTENT(IN) ::  PZZ         ! Height of flux point
REAL, DIMENSION(:,:,:), INTENT(IN) ::  PDZZ        ! Metric coefficients
REAL, DIMENSION(:,:,:), INTENT(IN) ::  PRHODJ      ! dry density * Grid size
REAL, DIMENSION(:,:,:), INTENT(IN) ::  PRHODREF    ! dry density of the
                                                     ! reference state
REAL, DIMENSION(:,:,:), INTENT(IN) ::  PPABSM      ! Pressure at time t-1
REAL, DIMENSION(:,:,:), INTENT(IN) ::  PEXN        ! Exner function at t-dt

REAL, DIMENSION(:,:),   INTENT(IN) ::  PSFTH,PSFRV ! normal surface fluxes of theta and Rv 
REAL, DIMENSION(:,:,:), INTENT(IN) ::  PTHM        ! Theta at t-dt
REAL, DIMENSION(:,:,:,:),INTENT(IN)::  PRM         ! water var. at t-dt
REAL, DIMENSION(:,:,:), INTENT(IN) ::  PUM,PVM     ! wind components at t-dt
REAL, DIMENSION(:,:,:), INTENT(IN) ::  PTKEM       ! tke at t-dt

REAL, DIMENSION(:,:,:,:), INTENT(IN) ::  PSVM        ! scalar variable a t-dt

REAL, DIMENSION(:,:,:),   INTENT(INOUT) ::  PRUS,PRVS,PRTHS ! Meso-NH sources
REAL, DIMENSION(:,:,:,:), INTENT(INOUT) ::  PRRS 
REAL, DIMENSION(:,:,:,:), INTENT(INOUT) ::  PRSVS            ! Scalar sources 
REAL, DIMENSION(:,:,:), INTENT(OUT)     ::  PSIGMF,PRC_MF,PRI_MF,PCF_MF ! cloud info for the cloud scheme
REAL, DIMENSION(:,:,:), INTENT(OUT)     ::  PFLXZTHVMF           ! Thermal production for TKE scheme


END SUBROUTINE SHALLOW_MF_PACK

END INTERFACE
!
END MODULE MODI_SHALLOW_MF_PACK

!     #################################################################
      SUBROUTINE SHALLOW_MF_PACK(KRR,KRRL,KRRI,                       &
                HMF_UPDRAFT, HMF_CLOUD, OMIXUV,                       &
                OCLOSE_OUT,OMF_FLX,HFMFILE,HLUOUT,PTIME_LES,          &
                PIMPL_MF, PTSTEP,                                     &
                PDZZ, PZZ,                                            &
                PRHODJ, PRHODREF,                                     &
                PPABSM, PEXN,                                         &
                PSFTH,PSFRV,                                          &
                PTHM,PRM,PUM,PVM,PTKEM,PSVM,                          &
                PRTHS,PRRS,PRUS,PRVS,PRSVS,                           &
                PSIGMF,PRC_MF,PRI_MF,PCF_MF,PFLXZTHVMF  )
!     #################################################################
!!
!!****  *SHALLOW_MF_PACK* - 
!!       
!!
!!    PURPOSE
!!    -------
!!****  The purpose of this routine is
!!
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
!!
!!
!!     AUTHOR
!!     ------
!!      V.Masson 09/2010
!! --------------------------------------------------------------------------
!
!*      0. DECLARATIONS
!          ------------
!
USE MODD_PARAMETERS
USE MODD_CST
USE MODD_CONF
USE MODD_NSV

USE MODD_PARAM_MFSHALL_n
USE MODD_BUDGET

USE MODE_FMWRIT

USE MODI_SHALLOW_MF
USE MODI_BUDGET
USE MODI_SHUMAN
USE MODI_DIAGNOS_LES_MF
!
IMPLICIT NONE

!*                    0.1  Declaration of Arguments
!
!
!
INTEGER,                INTENT(IN)   :: KRR          ! number of moist var.
INTEGER,                INTENT(IN)   :: KRRL         ! number of liquid water var.
INTEGER,                INTENT(IN)   :: KRRI         ! number of ice water var.
CHARACTER (LEN=4),      INTENT(IN)   :: HMF_UPDRAFT  ! Type of Mass Flux Scheme
                                     ! 'NONE' if no parameterization 
CHARACTER (LEN=4),      INTENT(IN)   :: HMF_CLOUD    ! Type of statistical cloud
                                                     ! scheme
LOGICAL,                INTENT(IN)   :: OMIXUV    ! True if mixing of momentum
LOGICAL,                INTENT(IN)   ::  OCLOSE_OUT   ! switch for synchronous
                                                      ! file opening
LOGICAL,                INTENT(IN)   ::  OMF_FLX      ! switch to write the
                                 ! MF fluxes in the synchronous FM-file
CHARACTER(LEN=*),       INTENT(IN)   ::  HFMFILE      ! Name of the output
                                                      ! FM-file
CHARACTER(LEN=*),       INTENT(IN)   ::  HLUOUT       ! Output-listing name for
                                                      ! model n
REAL*8,DIMENSION(2),                   INTENT(OUT)  :: PTIME_LES     ! time spent in LES computations
REAL,                   INTENT(IN)   :: PIMPL_MF     ! degre of implicitness
REAL,              INTENT(IN)     ::  PTSTEP   ! Dynamical timestep 

REAL, DIMENSION(:,:,:), INTENT(IN) ::  PZZ         ! Height of flux point
REAL, DIMENSION(:,:,:), INTENT(IN) ::  PDZZ        ! Metric coefficients
REAL, DIMENSION(:,:,:), INTENT(IN) ::  PRHODJ      ! dry density * Grid size
REAL, DIMENSION(:,:,:), INTENT(IN) ::  PRHODREF    ! dry density of the
                                                     ! reference state
REAL, DIMENSION(:,:,:), INTENT(IN) ::  PPABSM      ! Pressure at time t-1
REAL, DIMENSION(:,:,:), INTENT(IN) ::  PEXN        ! Exner function at t-dt

REAL, DIMENSION(:,:),   INTENT(IN) ::  PSFTH,PSFRV ! normal surface fluxes of theta and Rv 
REAL, DIMENSION(:,:,:), INTENT(IN) ::  PTHM        ! Theta at t-dt
REAL, DIMENSION(:,:,:,:),INTENT(IN)::  PRM         ! water var. at t-dt
REAL, DIMENSION(:,:,:), INTENT(IN) ::  PUM,PVM     ! wind components at t-dt
REAL, DIMENSION(:,:,:), INTENT(IN) ::  PTKEM       ! tke at t-dt

REAL, DIMENSION(:,:,:,:), INTENT(IN) ::  PSVM        ! scalar variable a t-dt

REAL, DIMENSION(:,:,:),   INTENT(INOUT) ::  PRUS,PRVS,PRTHS ! Meso-NH sources
REAL, DIMENSION(:,:,:,:), INTENT(INOUT) ::  PRRS 
REAL, DIMENSION(:,:,:,:), INTENT(INOUT) ::  PRSVS            ! Scalar sources 
REAL, DIMENSION(:,:,:), INTENT(OUT)     ::  PSIGMF,PRC_MF,PRI_MF,PCF_MF ! cloud info for the cloud scheme
REAL, DIMENSION(:,:,:), INTENT(OUT)     ::  PFLXZTHVMF           ! Thermal production for TKE scheme
!
!                     0.2  Declaration of local variables
!
REAL, DIMENSION(SIZE(PTHM,1)*SIZE(PTHM,2),SIZE(PTHM,3)) ::  ZZZ         ! Height of flux point
REAL, DIMENSION(SIZE(PTHM,1)*SIZE(PTHM,2),SIZE(PTHM,3)) ::  ZDZZ        ! Metric coefficients
REAL, DIMENSION(SIZE(PTHM,1)*SIZE(PTHM,2),SIZE(PTHM,3)) ::  ZRHODJ      ! dry density * Grid size
REAL, DIMENSION(SIZE(PTHM,1)*SIZE(PTHM,2),SIZE(PTHM,3)) ::  ZRHODREF    ! dry density of the
                                                     ! reference state
REAL, DIMENSION(SIZE(PTHM,1)*SIZE(PTHM,2),SIZE(PTHM,3)) ::  ZPABSM      ! Pressure at time t-1
REAL, DIMENSION(SIZE(PTHM,1)*SIZE(PTHM,2),SIZE(PTHM,3)) ::  ZEXN        ! Exner function at t-dt

REAL, DIMENSION(SIZE(PTHM,1)*SIZE(PTHM,2),SIZE(PTHM,3)) ::  ZTHM        ! Theta at t-dt
REAL, DIMENSION(SIZE(PTHM,1)*SIZE(PTHM,2),SIZE(PTHM,3),SIZE(PRM,4)) ::  ZRM         ! water var. at t-dt
REAL, DIMENSION(SIZE(PTHM,1)*SIZE(PTHM,2),SIZE(PTHM,3)) ::  ZUM,ZVM     ! wind components at t-dt
REAL, DIMENSION(SIZE(PTHM,1)*SIZE(PTHM,2),SIZE(PTHM,3)) ::  ZTKEM       ! tke at t-dt

REAL, DIMENSION(SIZE(PTHM,1)*SIZE(PTHM,2),SIZE(PTHM,3),SIZE(PSVM,4)) ::  ZSVM        ! scalar variable a t-dt

REAL, DIMENSION(SIZE(PTHM,1)*SIZE(PTHM,2),SIZE(PTHM,3)) ::  ZDUDT_TURB   ! tendency of U   by turbulence only
REAL, DIMENSION(SIZE(PTHM,1)*SIZE(PTHM,2),SIZE(PTHM,3)) ::  ZDVDT_TURB   ! tendency of V   by turbulence only
REAL, DIMENSION(SIZE(PTHM,1)*SIZE(PTHM,2),SIZE(PTHM,3)) ::  ZDTHLDT_TURB ! tendency of thl by turbulence only
REAL, DIMENSION(SIZE(PTHM,1)*SIZE(PTHM,2),SIZE(PTHM,3)) ::  ZDRTDT_TURB  ! tendency of rt  by turbulence only
REAL, DIMENSION(SIZE(PTHM,1)*SIZE(PTHM,2),SIZE(PTHM,3),SIZE(PSVM,4)) ::  ZDSVDT_TURB  ! tendency of Sv  by turbulence only
REAL, DIMENSION(SIZE(PTHM,1)*SIZE(PTHM,2),SIZE(PTHM,3)) ::  ZDUDT_MF   ! tendency of U   by massflux scheme
REAL, DIMENSION(SIZE(PTHM,1)*SIZE(PTHM,2),SIZE(PTHM,3)) ::  ZDVDT_MF   ! tendency of V   by massflux scheme
REAL, DIMENSION(SIZE(PTHM,1)*SIZE(PTHM,2),SIZE(PTHM,3)) ::  ZDTHLDT_MF ! tendency of thl by massflux scheme
REAL, DIMENSION(SIZE(PTHM,1)*SIZE(PTHM,2),SIZE(PTHM,3)) ::  ZDRTDT_MF  ! tendency of Rt by massflux scheme
REAL, DIMENSION(SIZE(PTHM,1)*SIZE(PTHM,2),SIZE(PTHM,3),SIZE(PSVM,4)) ::  ZDSVDT_MF  ! tendency of Sv by massflux scheme
REAL, DIMENSION(SIZE(PTHM,1)*SIZE(PTHM,2),SIZE(PTHM,3)) ::  ZSIGMF,ZRC_MF,ZRI_MF,ZCF_MF ! cloud info for the cloud scheme
REAL, DIMENSION(SIZE(PTHM,1)*SIZE(PTHM,2),SIZE(PTHM,3)) ::  ZFLXZTHVMF           ! Thermal production for TKE scheme
REAL, DIMENSION(SIZE(PTHM,1)*SIZE(PTHM,2),SIZE(PTHM,3)) ::  ZFLXZTHMF
REAL, DIMENSION(SIZE(PTHM,1)*SIZE(PTHM,2),SIZE(PTHM,3)) ::  ZFLXZRMF
REAL, DIMENSION(SIZE(PTHM,1)*SIZE(PTHM,2),SIZE(PTHM,3)) ::  ZFLXZUMF
REAL, DIMENSION(SIZE(PTHM,1)*SIZE(PTHM,2),SIZE(PTHM,3)) ::  ZFLXZVMF
REAL, DIMENSION(SIZE(PTHM,1)*SIZE(PTHM,2),SIZE(PTHM,3)) ::  ZTHL_UP   ! updraft characteristics
REAL, DIMENSION(SIZE(PTHM,1)*SIZE(PTHM,2),SIZE(PTHM,3)) ::  ZRT_UP    ! updraft characteristics
REAL, DIMENSION(SIZE(PTHM,1)*SIZE(PTHM,2),SIZE(PTHM,3)) ::  ZRV_UP    ! updraft characteristics
REAL, DIMENSION(SIZE(PTHM,1)*SIZE(PTHM,2),SIZE(PTHM,3)) ::  ZU_UP     ! updraft characteristics
REAL, DIMENSION(SIZE(PTHM,1)*SIZE(PTHM,2),SIZE(PTHM,3)) ::  ZV_UP     ! updraft characteristics
REAL, DIMENSION(SIZE(PTHM,1)*SIZE(PTHM,2),SIZE(PTHM,3)) ::  ZRC_UP    ! updraft characteristics
REAL, DIMENSION(SIZE(PTHM,1)*SIZE(PTHM,2),SIZE(PTHM,3)) ::  ZRI_UP    ! updraft characteristics
REAL, DIMENSION(SIZE(PTHM,1)*SIZE(PTHM,2),SIZE(PTHM,3)) ::  ZTHV_UP   ! updraft characteristics
REAL, DIMENSION(SIZE(PTHM,1)*SIZE(PTHM,2),SIZE(PTHM,3)) ::  ZW_UP     ! updraft characteristics
REAL, DIMENSION(SIZE(PTHM,1)*SIZE(PTHM,2),SIZE(PTHM,3)) ::  ZFRAC_UP  ! updraft characteristics
REAL, DIMENSION(SIZE(PTHM,1)*SIZE(PTHM,2),SIZE(PTHM,3)) ::  ZEMF      ! updraft characteristics
REAL, DIMENSION(SIZE(PTHM,1)*SIZE(PTHM,2),SIZE(PTHM,3)) ::  ZDETR     ! updraft characteristics
REAL, DIMENSION(SIZE(PTHM,1)*SIZE(PTHM,2),SIZE(PTHM,3)) ::  ZENTR     ! updraft characteristics
INTEGER,DIMENSION(SIZE(PTHM,1)*SIZE(PTHM,2))     :: IKLCL,IKETL,IKCTL ! level of LCL,ETL and CTL
REAL, DIMENSION(SIZE(PTHM,1)*SIZE(PTHM,2)) ::  ZSFTH    ! Surface sensible heat flux
REAL, DIMENSION(SIZE(PTHM,1)*SIZE(PTHM,2)) ::  ZSFRV    ! Surface latent   heat flux
!
!
!* 3D arrays
REAL, DIMENSION(SIZE(PTHM,1),SIZE(PTHM,2),SIZE(PTHM,3)) ::  ZWORK    ! work array
REAL, DIMENSION(SIZE(PTHM,1),SIZE(PTHM,2),SIZE(PTHM,3)) ::  ZUMM     ! wind on mass point
REAL, DIMENSION(SIZE(PTHM,1),SIZE(PTHM,2),SIZE(PTHM,3)) ::  ZVMM     ! wind on mass point
REAL, DIMENSION(SIZE(PTHM,1),SIZE(PTHM,2),SIZE(PTHM,3)) ::  ZDUDT    ! tendency of U   by massflux scheme
REAL, DIMENSION(SIZE(PTHM,1),SIZE(PTHM,2),SIZE(PTHM,3)) ::  ZDVDT    ! tendency of V   by massflux scheme
REAL, DIMENSION(SIZE(PTHM,1),SIZE(PTHM,2),SIZE(PTHM,3)) ::  ZDTHLDT  ! tendency of thl by massflux scheme
REAL, DIMENSION(SIZE(PTHM,1),SIZE(PTHM,2),SIZE(PTHM,3)) ::  ZDRTDT   ! tendency of Rt by massflux scheme
REAL, DIMENSION(SIZE(PTHM,1),SIZE(PTHM,2),SIZE(PTHM,3),SIZE(PSVM,4)) ::  ZDSVDT  ! tendency of Sv by massflux scheme

INTEGER :: IIU, IJU, IKU, IKB, IKE, IRR, ISV  
INTEGER :: JK,JRR,JSV                          ! Loop counters

INTEGER             :: IRESP        ! Return code of FM routines 
INTEGER             :: IGRID        ! C-grid indicator in LFIFM file 
INTEGER             :: ILENCH       ! Length of comment string in LFIFM file
CHARACTER (LEN=100) :: YCOMMENT     ! comment string in LFIFM file
CHARACTER (LEN=16)  :: YRECFM       ! Name of the desired field in LFIFM file
!------------------------------------------------------------------------

!!! 1. Initialisation

! Internal Domain
IIU=SIZE(PTHM,1)
IJU=SIZE(PTHM,2)
IKU=SIZE(PTHM,3)
IKB=1+JPVEXT
IKE=IKU-JPVEXT

! number of moist  var
IRR=SIZE(PRM,4)
! number of scalar var
ISV=SIZE(PSVM,4)

ZSVM(:,:,:) = 0.
!
!
! wind on mass points
ZUMM=MXF(PUM)
ZVMM=MYF(PVM)
!
!!! 2. Pack input variables
!
DO JK=1,IKU
  ZZZ    (:,JK) = RESHAPE(PZZ    (:,:,JK),(/ IIU*IJU /) )
  ZDZZ   (:,JK) = RESHAPE(PDZZ    (:,:,JK),(/ IIU*IJU /) )  
  ZRHODJ (:,JK) = RESHAPE(PRHODJ  (:,:,JK),(/ IIU*IJU /) )  
  ZTHM   (:,JK) = RESHAPE(PTHM   (:,:,JK),(/ IIU*IJU /) )
  ZTKEM  (:,JK) = RESHAPE(PTKEM  (:,:,JK),(/ IIU*IJU /) )
  ZPABSM (:,JK) = RESHAPE(PPABSM (:,:,JK),(/ IIU*IJU /) )
  ZEXN   (:,JK) = RESHAPE(PEXN   (:,:,JK),(/ IIU*IJU /) )
  ZRHODJ (:,JK) = RESHAPE(PRHODJ (:,:,JK),(/ IIU*IJU /) )  
  ZRHODREF(:,JK) = RESHAPE(PRHODREF(:,:,JK),(/ IIU*IJU /) )  
  ZUM    (:,JK) = RESHAPE(ZUMM   (:,:,JK),(/ IIU*IJU /) )
  ZVM    (:,JK) = RESHAPE(ZVMM   (:,:,JK),(/ IIU*IJU /) )
  DO JRR=1,IRR
    ZRM   (:,JK,JRR) = RESHAPE(PRM    (:,:,JK,JRR),(/ IIU*IJU /) ) 
  END DO
  DO JSV=1,ISV 
    IF (LNOMIXLG .AND. JSV >= NSV_LGBEG .AND. JSV<= NSV_LGEND) CYCLE
    ZSVM(:,JK,JSV)   = RESHAPE(PSVM  (:,:,JK,JSV),(/ IIU*IJU /) ) 
  END DO  
END DO

ZSFTH(:)=RESHAPE(PSFTH(:,:),(/ IIU*IJU /) )
ZSFRV(:)=RESHAPE(PSFRV(:,:),(/ IIU*IJU /) )

!!! 3. Call of the physical parameterization of massflux vertical transport

CALL SHALLOW_MF(1,IKU,1,KRR,KRRL,KRRI,                              &
                HMF_UPDRAFT, HMF_CLOUD, 'T', OMIXUV,                  &
                LNOMIXLG,NSV_LGBEG,NSV_LGEND,                         &
                PIMPL_MF, PTSTEP,                                     &
                ZDZZ, ZZZ,                                            &
                ZRHODJ,ZRHODREF,                                      &
                ZPABSM, ZEXN,                                         &
                ZSFTH,ZSFRV,                                          &
                ZTHM,ZRM,ZUM,ZVM,ZTKEM,ZSVM,                          &
                ZDUDT_MF,ZDVDT_MF,                                    &
                ZDTHLDT_MF,ZDRTDT_MF,ZDSVDT_MF,                       &
                ZSIGMF,ZRC_MF,ZRI_MF,ZCF_MF,ZFLXZTHVMF,               &
                ZFLXZTHMF,ZFLXZRMF,ZFLXZUMF,ZFLXZVMF,                 &
                ZTHL_UP,ZRT_UP,ZRV_UP,ZRC_UP,ZRI_UP,                  &
                ZU_UP, ZV_UP, ZTHV_UP, ZW_UP,                         &
                ZFRAC_UP,ZEMF,ZDETR,ZENTR,                            &
                IKLCL,IKETL,IKCTL                                     )

!!! 4. Unpack output variables

ZDTHLDT(:,:,:)=RESHAPE(ZDTHLDT_MF(:,:),(/ IIU,IJU,IKU /) )
ZDRTDT(:,:,:)=RESHAPE(ZDRTDT_MF(:,:),(/ IIU,IJU,IKU /) )
ZDUDT(:,:,:)=RESHAPE(ZDUDT_MF(:,:),(/ IIU,IJU,IKU /) )
ZDVDT(:,:,:)=RESHAPE(ZDVDT_MF(:,:),(/ IIU,IJU,IKU /) )
PSIGMF(:,:,:)=RESHAPE(ZSIGMF(:,:),(/ IIU,IJU,IKU /) )
PRC_MF(:,:,:)=RESHAPE(ZRC_MF(:,:),(/ IIU,IJU,IKU /) )
PRI_MF(:,:,:)=RESHAPE(ZRI_MF(:,:),(/ IIU,IJU,IKU /) )
PCF_MF(:,:,:)=RESHAPE(ZCF_MF(:,:),(/ IIU,IJU,IKU /) )
PFLXZTHVMF(:,:,:)=RESHAPE(ZFLXZTHVMF(:,:),(/ IIU,IJU,IKU /) )
DO JSV=1,ISV 
  IF (LNOMIXLG .AND. JSV >= NSV_LGBEG .AND. JSV<= NSV_LGEND) CYCLE
    ZDSVDT(:,:,:,JSV)   = RESHAPE(ZDSVDT_MF(:,:,JSV),(/ IIU,IJU,IKU /) ) 
END DO  
!
!!! 5. Compute source terms for Meso-NH pronostic variables
!!!    ----------------------------------------------------


! As the pronostic variable of Meso-Nh are not (yet) the conservative variables
! the thl tendency is put in th and the rt tendency in rv
! the adjustment will do later the repartition between vapor and cloud
PRTHS(:,:,:)  = PRTHS(:,:,:)  +   &
                  PRHODJ(:,:,:)*ZDTHLDT(:,:,:)
PRRS(:,:,:,1) = PRRS(:,:,:,1) +   &
                  PRHODJ(:,:,:)*ZDRTDT(:,:,:)
PRUS(:,:,:)   = PRUS(:,:,:)  +MXM(  &
                  PRHODJ(:,:,:)*ZDUDT(:,:,:))
PRVS(:,:,:)   = PRVS(:,:,:)  +MYM(  &
                  PRHODJ(:,:,:)*ZDVDT(:,:,:))

DO JSV=1,ISV 
  IF (LNOMIXLG .AND. JSV >= NSV_LGBEG .AND. JSV<= NSV_LGEND) CYCLE
  PRSVS(:,:,:,JSV)   = PRSVS(:,:,:,JSV)  +    &
                  PRHODJ(:,:,:)*ZDSVDT(:,:,:,JSV)
END DO     

!!! 7. call to MesoNH budgets

IF (LBUDGET_TH) CALL BUDGET (PRTHS,4,'MAFL_BU_RTH')
IF (LBUDGET_RV) CALL BUDGET (PRRS(:,:,:,1),6,'MAFL_BU_RRV')
IF (LBUDGET_U) CALL BUDGET (PRUS,1,'MAFL_BU_RU')
IF (LBUDGET_V) CALL BUDGET (PRVS,2,'MAFL_BU_RV')
DO JSV=1,ISV 
 IF (LBUDGET_SV) CALL BUDGET (PRSVS(:,:,:,JSV),12+JSV,'MAFL_BU_RSV')
END DO                 
  
!!! 8. Prints the fluxes in output file
!
IF ( OMF_FLX .AND. OCLOSE_OUT ) THEN
  ! stores the conservative potential temperature vertical flux
  YRECFM  ='MF_THW_FLX'
  YCOMMENT='X_Y_Z_MF_THW_FLX (K*M/S)'
  ILENCH  = LEN(YCOMMENT)
  IGRID   = 4   
  ZWORK(:,:,:)=RESHAPE(ZFLXZTHMF (:,:),(/ IIU,IJU,IKU /) )
  CALL FMWRIT(HFMFILE,YRECFM,HLUOUT,'XY',ZWORK,IGRID,ILENCH,YCOMMENT,IRESP)
  ! stores the conservative mixing ratio vertical flux
  YRECFM  ='MF_RCONSW_FLX'
  YCOMMENT='X_Y_Z_MF_RCONSW_FLX (K*M/S)'
  ILENCH  = LEN(YCOMMENT)
  IGRID   = 4   
  ZWORK(:,:,:)=RESHAPE(ZFLXZRMF(:,:),(/ IIU,IJU,IKU /) )
  CALL FMWRIT(HFMFILE,YRECFM,HLUOUT,'XY',ZWORK,IGRID,ILENCH,YCOMMENT,IRESP)
  ! stores the theta_v vertical flux
   YRECFM  ='MF_THVW_FLX'
   YCOMMENT='X_Y_Z_MF_THVW_FLX (K*M/S)'
   ILENCH  = LEN(YCOMMENT)
   IGRID   = 4   
   CALL FMWRIT(HFMFILE,YRECFM,HLUOUT,'XY',PFLXZTHVMF,IGRID,ILENCH,YCOMMENT,IRESP)
 IF (OMIXUV) THEN
  ! stores the U momentum vertical flux
  YRECFM  ='MF_UW_FLX'
  YCOMMENT='X_Y_Z_MF_UW_FLX (M2/S2)'
  ILENCH  = LEN(YCOMMENT)
  IGRID   = 4   
  ZWORK(:,:,:)=RESHAPE(ZFLXZUMF(:,:),(/ IIU,IJU,IKU /) )
  CALL FMWRIT(HFMFILE,YRECFM,HLUOUT,'XY',ZWORK,IGRID,ILENCH,YCOMMENT,IRESP)
  ! stores the V momentum vertical flux
  YRECFM  ='MF_VW_FLX'
  YCOMMENT='X_Y_Z_MF_VW_FLX (M2/S2)'
  ILENCH  = LEN(YCOMMENT)
  IGRID   = 4   
  ZWORK(:,:,:)=RESHAPE(ZFLXZVMF(:,:),(/ IIU,IJU,IKU /) )
  CALL FMWRIT(HFMFILE,YRECFM,HLUOUT,'XY',ZWORK,IGRID,ILENCH,YCOMMENT,IRESP)
  !
 END IF
END IF

!!! 9. Externalised LES Diagnostic for Mass Flux Scheme
!!!    ------------------------------------------------

      CALL DIAGNOS_LES_MF(IIU,IJU,IKU,PTIME_LES,               &
                          ZTHL_UP,ZRT_UP,ZRV_UP,ZRC_UP,ZRI_UP, &
                          ZU_UP,ZV_UP,ZTHV_UP,ZW_UP,           &
                          ZFRAC_UP,ZEMF,ZDETR,ZENTR,           &
                          ZFLXZTHMF,ZFLXZTHVMF,ZFLXZRMF,       &
                          ZFLXZUMF,ZFLXZVMF,                   &
                          IKLCL,IKETL,IKCTL )
               

END SUBROUTINE SHALLOW_MF_PACK
