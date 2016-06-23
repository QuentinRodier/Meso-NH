!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########################
SUBROUTINE ISBA_SOC_PARAMETERS (HRUNOFF,PPATCH,PDG,PSOC,PBCOEF,PMPOTSAT,   &
                                PCONDSAT,PWSAT,PHCAPSOIL,PCONDDRY,PCONDSLD,&
                                PWFC,PWWILT,PWD0,PANISO,PFRACSOC           )
!     ########################################################################
!
!!****  *ISBA_SOC_PARAMETERS*  
!!
!!    PURPOSE
!!    -------
!
!    ISBA parameterizations for soil thermal and hydraulic properties 
!    are modified to accommodate both mineral and organic carbon soils
!    according to observations from Boelter (1969).
!    Disctinction is made for Fibric soil (76.8038 % of fiber content)
!    and Sapric soil (21.7815 % of fiber content)
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
!!      B. Decharme     
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    10/12/11 
!!      (B. Decharme) 04/2013 ksat anisotropy factor
!-------------------------------------------------------------------------------
!
USE MODD_SURF_PAR, ONLY : XUNDEF
USE MODD_CSTS,     ONLY : XDAY
USE MODD_ISBA_PAR, ONLY : XOMRHO, XOMSPH, XOMCONDDRY, XOMCONDSLD
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB

!*      0.1    declarations of arguments
!
!
IMPLICIT NONE
!
 CHARACTER(LEN=4),      INTENT(IN)    :: HRUNOFF
!
REAL, DIMENSION(:,:,:),INTENT(IN)    :: PDG
!
REAL, DIMENSION(:,:),  INTENT(IN)    :: PPATCH
!
REAL, DIMENSION(:,:),  INTENT(IN)    :: PSOC
!
REAL, DIMENSION(:,:,:),INTENT(INOUT) :: PCONDSAT
!
REAL, DIMENSION(:,:),  INTENT(INOUT) :: PBCOEF,PMPOTSAT,    &
                                        PHCAPSOIL,PCONDDRY, &
                                        PCONDSLD
!
REAL, DIMENSION(:,:),  INTENT(INOUT) :: PWSAT,PWFC,PWWILT,PWD0
!
REAL, DIMENSION(:,:),  INTENT(INOUT) :: PANISO
!
REAL, DIMENSION(:,:),  INTENT(OUT)   :: PFRACSOC
!
!*      0.2    declarations of local parameter
!
REAL, DIMENSION(2), PARAMETER :: ZCONDSAT = (/24.192,0.00864/)  !Peatland hydraulic conductivity        (m/day)
                                                                !from Letts et al. (2000)
!                                                                
REAL, DIMENSION(2), PARAMETER :: ZBCOEF   = (/2.7,12.0/)        !Peatland b coef                        (-)
                                                                !from Letts et al. (2000)
!                                                                
REAL, DIMENSION(2), PARAMETER :: ZMPOTSAT = (/-0.0103,-0.0101/) !Peatland matric potential              (m)
                                                                !from Letts et al. (2000)
!                                                                
REAL, DIMENSION(2), PARAMETER :: ZWSAT    = (/0.930,0.845/)     !Peatland porosity                      (-)
                                                                !from Boelter (1969) PTF for 
                                                                !Fibric soil = 76.8038 % of fiber content
                                                                !Sapric soil = 21.7815 % of fiber content

REAL, DIMENSION(2), PARAMETER :: ZWFC     = (/0.369,0.719/)     !Peatland field capacity                (-)
                                                                !Water potential at -0.1 bar given by  
                                                                !Boelter (1969) PTF 
!                                                                
REAL, DIMENSION(2), PARAMETER :: ZWWILT   = (/0.073,0.222/)     !Peatland wilting point                 (-)
                                                                !Water potential at -15 bar given by 
                                                                !Boelter (1969) PTF
!                                                                
REAL, DIMENSION(2), PARAMETER :: ZWD0     = (/0.212,0.716/)     !Peatland Topmodel D0 water equivalent  (-)
                                                                !using hydro cond at 0.1mm/days
!                                                                
REAL, DIMENSION(2), PARAMETER :: ZANISO   = (/2.0,48.0/)        !Peatland ksat anisotropy factor        (-)
!
!
!HWSD data profile
REAL, PARAMETER :: ZDGHWSD_TOP = 0.3
REAL, PARAMETER :: ZDGHWSD_SUB = 1.0
REAL, PARAMETER :: ZDGHWSD_INF = 1000.
!
!*      0.3    declarations of local variables
!
REAL, DIMENSION(SIZE(PDG,1))              :: ZPEAT_PROFILE, ZMOSS_DEPTH
!
REAL, DIMENSION(SIZE(PDG,1))              :: ZMASK, ZRHO_TOP, ZRHO_SUB, ZRHO_INF
!
REAL, DIMENSION(SIZE(PDG,1),SIZE(PDG,2))  :: ZDG_SOIL, ZDZG_SOIL, ZRHO_SOC, ZMID_SOIL
!
REAL, DIMENSION(SIZE(PDG,1),SIZE(PDG,2))  :: ZPEAT_BCOEF,ZPEAT_MPOTSAT,&
                                             ZPEAT_WSAT,ZPEAT_WFC,     &
                                             ZPEAT_WWILT,ZPEAT_WD0,    &
                                             ZPEAT_ANISO, ZPEAT_RHO
!
REAL, DIMENSION(SIZE(PDG,1),SIZE(PDG,2),SIZE(PDG,3))  :: ZPEAT_CONDSAT, ZMID_CONDSAT
!
REAL, DIMENSION(SIZE(PDG,1))              ::  ZREFDEPTH,ZF_BCOEF,ZF_MPOTSAT,    &
                                              ZLOG_MOSS,ZLOG_PEAT_DEPTH    ,    &
                                              ZF_WSAT,ZF_CONDSAT,ZF_WFC,        &
                                              ZF_WWILT, ZF_WD0, ZF_ANISO

REAL :: ZA, ZB, ZLOG1, ZLOG2, ZMOSS_DENSITY,  &
        ZTOP, ZSUB, ZFTOP, ZFSUB                                 
!
REAL, DIMENSION(2) :: ZLOG_CONDSAT,ZLOG_BCOEF,ZLOG_MPOTSAT, &
                      ZLOG_WSAT,ZLOG_WFC,ZLOG_WWILT,ZLOG_WD0,&
                      ZLOG_ANISO
!
INTEGER :: INI, INL, INP, JI, JL, JP
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('ISBA_SOC_PARAMETERS',0,ZHOOK_HANDLE)
!
INI=SIZE(PDG,1)
INL=SIZE(PDG,2)
INP=SIZE(PDG,3)
!
ZMASK   (:) = 0.0
ZRHO_TOP(:) = 0.0
ZRHO_SUB(:) = 0.0
ZRHO_INF(:) = 0.0
!
ZDG_SOIL(:,:) = 0.0
ZRHO_SOC(:,:) = 0.0
!
ZPEAT_RHO    (:,:  )=0.0
ZPEAT_BCOEF  (:,:  )=0.0
ZPEAT_MPOTSAT(:,:  )=0.0
ZPEAT_WSAT   (:,:  )=0.0
ZPEAT_WFC    (:,:  )=0.0
ZPEAT_WWILT  (:,:  )=0.0
ZPEAT_WD0    (:,:  )=0.0
ZPEAT_ANISO  (:,:  )=0.0
ZPEAT_CONDSAT(:,:,:)=0.0
!
PFRACSOC (:,:)=XUNDEF
!
!-------------------------------------------------------------------------------
!
DO JP=1,INP
  DO JI=1,INI
     ZMASK(JI)=ZMASK(JI)+PPATCH(JI,JP)
  ENDDO
ENDDO
!  
DO JL=1,INL
   DO JI=1,INI
     IF(ZMASK(JI)>0.0)THEN
       ZDG_SOIL (JI,JL)=SUM(PDG(JI,JL,:)*PPATCH(JI,:),PPATCH(JI,:)>0.0) &
                       /SUM(PPATCH(JI,:),PPATCH(JI,:)>0.0)
     ENDIF
  ENDDO
ENDDO
!
ZDZG_SOIL(:,1)=ZDG_SOIL(:,1)
DO JL=2,INL
  DO JI=1,INI
     ZDZG_SOIL(JI,JL)=ZDG_SOIL(JI,JL)-ZDG_SOIL(JI,JL-1)
  ENDDO
ENDDO
!
ZMID_SOIL(:,1)=0.5*ZDG_SOIL(:,1)
DO JL=2,INL
  DO JI=1,INI
     ZMID_SOIL(JI,JL)=0.5*(ZDG_SOIL(JI,JL)+ZDG_SOIL(JI,JL-1))
  ENDDO
ENDDO
!
DO JP=1,INP
  DO JL=1,INL
     DO JI=1,INI
        IF(PPATCH(JI,JP)/=XUNDEF)THEN
          ZMID_CONDSAT(JI,JL,JP)=ZMID_SOIL(JI,JL)
        ENDIF
     ENDDO
  ENDDO
ENDDO
!
!-------------------------------------------------------------------------------
!
! Compute the SOC density distribution (kg.m-3)
!
ZLOG1=LOG(ZDGHWSD_TOP/ZDGHWSD_SUB)
ZLOG2=LOG(ZDGHWSD_INF/ZDGHWSD_SUB)
DO JI=1,INI
   IF(ZMASK(JI)>0.0)THEN
     ZRHO_TOP(JI) = PSOC(JI,1)/ZDGHWSD_TOP
     ZRHO_SUB(JI) = PSOC(JI,2)/(ZDGHWSD_SUB-ZDGHWSD_TOP)           
     IF(ZRHO_TOP(JI)>ZRHO_SUB(JI))THEN
       ZB           = LOG(PSOC(JI,1)/(PSOC(JI,1)+PSOC(JI,2)))/ZLOG1
       ZA           = (PSOC(JI,1)+PSOC(JI,2))/(ZDGHWSD_INF-ZDGHWSD_SUB)
       ZRHO_INF(JI) = ZA*(EXP(ZB*ZLOG2)-1.0)
     ELSE
       ZRHO_INF(JI) = ZRHO_SUB(JI)
     ENDIF
   ENDIF
ENDDO
!
! Compute the SOC density distribution (kg.m-3)
!
!
DO JI=1,INI
  ZTOP=0.0
  ZSUB=0.0
  DO JL=1,INL
     ZTOP=ZSUB
     ZSUB=ZSUB+ZDZG_SOIL(JI,JL)     
     IF(ZSUB<=ZDGHWSD_TOP)THEN
       ZRHO_SOC(JI,JL)=ZRHO_TOP(JI)
     ELSEIF(ZTOP>=ZDGHWSD_TOP.AND.ZSUB<=ZDGHWSD_SUB)THEN
       ZRHO_SOC(JI,JL)=ZRHO_SUB(JI)
     ELSEIF(ZTOP>=ZDGHWSD_SUB)THEN
       ZRHO_SOC(JI,JL)=ZRHO_INF(JI)       
     ELSEIF(ZTOP<ZDGHWSD_TOP.AND.ZSUB>ZDGHWSD_TOP)THEN
       ZFTOP=MIN(1.0,MAX(0.0,ZDGHWSD_TOP-ZTOP))/(ZSUB-ZTOP)
       ZFSUB=MIN(1.0,MAX(0.0,ZSUB-ZDGHWSD_TOP))/(ZSUB-ZTOP)
       ZRHO_SOC(JI,JL)=ZFTOP*ZRHO_TOP(JI)+ZFSUB*ZRHO_SUB(JI)
     ELSEIF(ZTOP<ZDGHWSD_SUB.AND.ZSUB>ZDGHWSD_SUB)THEN
       ZFTOP=MIN(1.0,MAX(0.0,ZDGHWSD_SUB-ZTOP))/(ZSUB-ZTOP)
       ZFSUB=MIN(1.0,MAX(0.0,ZSUB-ZDGHWSD_SUB))/(ZSUB-ZTOP)
       ZRHO_SOC(JI,JL)=ZFTOP*ZRHO_SUB(JI)+ZFSUB*ZRHO_INF(JI)
     ENDIF
  ENDDO
ENDDO 
!
!-------------------------------------------------------------------------------
!
! Define the Peatland soil properties
!
ZLOG_CONDSAT(:) = LOG(ZCONDSAT (:))
ZLOG_BCOEF  (:) = LOG(ZBCOEF   (:))
ZLOG_MPOTSAT(:) = LOG(-ZMPOTSAT(:))
ZLOG_WSAT   (:) = LOG(ZWSAT    (:))
ZLOG_WFC    (:) = LOG(ZWFC     (:))
ZLOG_WWILT  (:) = LOG(ZWWILT   (:))
ZLOG_WD0    (:) = LOG(ZWD0     (:))
ZLOG_ANISO  (:) = LOG(ZANISO   (:))
!
ZPEAT_PROFILE(:) = 1.0
!
ZMOSS_DENSITY=(1.0-ZWSAT(1))*XOMRHO
!
WHERE(ZRHO_TOP(:)<ZMOSS_DENSITY)
   ZMOSS_DEPTH(:) = 2.5E-3 ! => Small fibric soil at surface (<=> moss=2.5mm)
ELSEWHERE
   ZMOSS_DEPTH(:) = 0.01   ! => Fibric soil at surface (<=> moss=1cm)
ENDWHERE
!
WHERE(ZMASK(:)>0.0)
!
  ZLOG_MOSS       (:) = LOG(ZMOSS_DEPTH  (:))
  ZLOG_PEAT_DEPTH (:) = LOG(ZPEAT_PROFILE(:))
!
  ZF_CONDSAT(:) =(ZLOG_CONDSAT(2)-ZLOG_CONDSAT(1))/(ZLOG_PEAT_DEPTH(:)-ZLOG_MOSS(:))
  ZF_BCOEF  (:) =(ZLOG_BCOEF  (2)-ZLOG_BCOEF  (1))/(ZLOG_PEAT_DEPTH(:)-ZLOG_MOSS(:))
  ZF_MPOTSAT(:) =(ZLOG_MPOTSAT(2)-ZLOG_MPOTSAT(1))/(ZLOG_PEAT_DEPTH(:)-ZLOG_MOSS(:))
  ZF_WSAT   (:) =(ZLOG_WSAT   (2)-ZLOG_WSAT   (1))/(ZLOG_PEAT_DEPTH(:)-ZLOG_MOSS(:))
  ZF_WFC    (:) =(ZLOG_WFC    (2)-ZLOG_WFC    (1))/(ZLOG_PEAT_DEPTH(:)-ZLOG_MOSS(:))
  ZF_WWILT  (:) =(ZLOG_WWILT  (2)-ZLOG_WWILT  (1))/(ZLOG_PEAT_DEPTH(:)-ZLOG_MOSS(:))
  ZF_WD0    (:) =(ZLOG_WD0    (2)-ZLOG_WD0    (1))/(ZLOG_PEAT_DEPTH(:)-ZLOG_MOSS(:))
  ZF_ANISO  (:) =(ZLOG_ANISO  (2)-ZLOG_ANISO  (1))/(ZLOG_PEAT_DEPTH(:)-ZLOG_MOSS(:))
!
ENDWHERE
!
!-------------------------------------------------------------------------------
!
! Compute the Peatland soil properties profile
!
DO JL=1,INL
   DO JI=1,INI
     IF(ZMASK(JI)>0.0)THEN
!      
      ZREFDEPTH(JI)=MIN(ZPEAT_PROFILE(JI),MAX(ZMOSS_DEPTH(JI),ZMID_SOIL(JI,JL)))
      ZREFDEPTH(JI)=LOG(ZREFDEPTH(JI))-ZLOG_MOSS(JI)
      ZPEAT_MPOTSAT(JI,JL)=ZMPOTSAT(1)*EXP(ZF_MPOTSAT(JI)*ZREFDEPTH(JI)) 
      ZPEAT_WSAT   (JI,JL)=ZWSAT   (1)*EXP(ZF_WSAT   (JI)*ZREFDEPTH(JI))
      ZPEAT_BCOEF  (JI,JL)=ZBCOEF  (1)*EXP(ZF_BCOEF  (JI)*ZREFDEPTH(JI))
      ZPEAT_WWILT  (JI,JL)=ZWWILT  (1)*EXP(ZF_WWILT  (JI)*ZREFDEPTH(JI))
      ZPEAT_WD0    (JI,JL)=ZWD0    (1)*EXP(ZF_WD0    (JI)*ZREFDEPTH(JI))
      ZPEAT_ANISO  (JI,JL)=ZANISO  (1)*EXP(ZF_ANISO  (JI)*ZREFDEPTH(JI))
      ZPEAT_WFC    (JI,JL)=ZWFC    (1)*EXP(ZF_WFC    (JI)*ZREFDEPTH(JI))
!
      ZPEAT_RHO    (JI,JL)=(1.0-ZPEAT_WSAT(JI,JL))*XOMRHO
!
      DO JP=1,INP
         IF(PPATCH(JI,JP)/=XUNDEF)THEN
           ZREFDEPTH(JI)=MIN(ZPEAT_PROFILE(JI),MAX(ZMOSS_DEPTH(JI),ZMID_CONDSAT(JI,JL,JP)))
           ZREFDEPTH(JI)=LOG(ZREFDEPTH(JI))-ZLOG_MOSS(JI)                       
           ZPEAT_CONDSAT(JI,JL,JP)=ZCONDSAT(1)*EXP(ZF_CONDSAT(JI)*ZREFDEPTH(JI))/XDAY
         ENDIF
      ENDDO
!      
     ENDIF
   ENDDO
ENDDO
!
!-------------------------------------------------------------------------------
!
DO JL=1,INL
   DO JI=1,INI
     IF(ZMASK(JI)>0.0)THEN            
!      Soil organic carbon fraction
       PFRACSOC (JI,JL  ) = MIN(1.0,ZRHO_SOC(JI,JL)/ZPEAT_RHO(JI,JL))   
!      New soil thermal properties      
       PHCAPSOIL(JI,JL  ) = (1.0-PFRACSOC(JI,JL))*PHCAPSOIL(JI,JL) + PFRACSOC(JI,JL)*XOMRHO*XOMSPH
       PCONDDRY (JI,JL  ) = (PCONDDRY(JI,JL)**(1.0-PFRACSOC(JI,JL))) * (XOMCONDDRY**PFRACSOC(JI,JL))
       PCONDSLD (JI,JL  ) = (PCONDSLD(JI,JL)**(1.0-PFRACSOC(JI,JL))) * (XOMCONDSLD**PFRACSOC(JI,JL))
!      New soil hydraulic properties
       PBCOEF   (JI,JL  ) = (1.0-PFRACSOC(JI,JL))*PBCOEF   (JI,JL) + PFRACSOC(JI,JL)*ZPEAT_BCOEF  (JI,JL)
       PMPOTSAT (JI,JL  ) = (1.0-PFRACSOC(JI,JL))*PMPOTSAT (JI,JL) + PFRACSOC(JI,JL)*ZPEAT_MPOTSAT(JI,JL)
       PWSAT    (JI,JL  ) = (1.0-PFRACSOC(JI,JL))*PWSAT    (JI,JL) + PFRACSOC(JI,JL)*ZPEAT_WSAT   (JI,JL)  
       PWFC     (JI,JL  ) = (1.0-PFRACSOC(JI,JL))*PWFC     (JI,JL) + PFRACSOC(JI,JL)*ZPEAT_WFC    (JI,JL)
       PWWILT   (JI,JL  ) = (1.0-PFRACSOC(JI,JL))*PWWILT   (JI,JL) + PFRACSOC(JI,JL)*ZPEAT_WWILT  (JI,JL)
       DO JP=1,INP
          IF(PPATCH(JI,JP)/=XUNDEF)THEN
            PCONDSAT (JI,JL,JP) = PCONDSAT(JI,JL,JP)**(1.0-PFRACSOC(JI,JL))*ZPEAT_CONDSAT(JI,JL,JP)**PFRACSOC(JI,JL)
          ENDIF
       ENDDO
     ENDIF
   ENDDO   
ENDDO
!
IF(HRUNOFF=='SGH')THEN
  DO JL=1,INL
     DO JI=1,INI
       IF(ZMASK(JI)>0.0)THEN
         PWD0  (JI,JL) = (1.0-PFRACSOC(JI,JL))*PWD0  (JI,JL) + PFRACSOC(JI,JL)*ZPEAT_WD0  (JI,JL)
         PANISO(JI,JL) = (1.0-PFRACSOC(JI,JL))*PANISO(JI,JL) + PFRACSOC(JI,JL)*ZPEAT_ANISO(JI,JL)
       ENDIF
     ENDDO   
  ENDDO
ENDIF
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('ISBA_SOC_PARAMETERS',1,ZHOOK_HANDLE)
!
END SUBROUTINE ISBA_SOC_PARAMETERS





