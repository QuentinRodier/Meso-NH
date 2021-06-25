!MNH_LIC Copyright 2019-2021 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!
!    #######################
MODULE MODI_IBM_BALANCE
  !    ####################### 
  !
  INTERFACE
     !
     SUBROUTINE IBM_BALANCE(PPHI,PVOL,PRUS,PRVS,PRWS,PBAL)
       !
       REAL, DIMENSION(:,:,:,:) ,INTENT(IN)    :: PPHI
       REAL, DIMENSION(:,:,:,:) ,INTENT(IN)    :: PVOL
       REAL, DIMENSION(:,:,:)   ,INTENT(INOUT) :: PRUS,PRVS,PRWS
       REAL, DIMENSION(:,:,:)   ,INTENT(INOUT) :: PBAL
       !
     END SUBROUTINE IBM_BALANCE
     !
  END INTERFACE
  !
END MODULE MODI_IBM_BALANCE
!
!     #####################################################
SUBROUTINE IBM_BALANCE(PPHI,PVOL,PRUS,PRVS,PRWS,PBAL)
  !     #####################################################
  !
  !
  !****  IBM_BALANCE computes the velocity divergence using a volumic approach 
  !                                
  !    PURPOSE
  !    -------
  !****  The purpose of this routine is to compute div(U)=1/V*int_S(u.n)dS
  !      S is the modified surface and is estimated before MNH
  !      U is approximated using adjacents points  

  !    METHOD
  !    ------
  !
  !    EXTERNAL
  !    --------
  !      SUBROUTINE ?
  !
  !    IMPLICIT ARGUMENTS
  !    ------------------
  !       MODD_?   
  !
  !    REFERENCE
  !    ---------
  !
  !    AUTHOR
  !    ------
  !      Franck Auguste (CERFACS-AE)
  !
  !    MODIFICATIONS
  !    -------------
  !      Original         01/01/2019
  !
  !------------------------------------------------------------------------------
  !       
  !**** 0. DECLARATIONS
  !     ---------------
  ! module
  USE MODE_ll
  USE MODE_IO
  !
  ! declaration
  USE MODD_CST, ONLY: XPI
  USE MODD_IBM_PARAM_n
  USE MODD_GRID_n, ONLY: XXHAT,XYHAT,XZHAT,XZZ 
  USE MODD_PARAMETERS, ONLY: JPVEXT,JPHEXT
  USE MODD_LBC_n
  USE MODD_REF_n
  !
  ! interface
  USE MODI_SHUMAN
  !
  IMPLICIT NONE
  !
  !------------------------------------------------------------------------------
  !
  !       0.1  declarations of arguments
  !
  REAL, DIMENSION(:,:,:,:) ,INTENT(IN)    :: PPHI
  REAL, DIMENSION(:,:,:,:) ,INTENT(IN)    :: PVOL
  REAL, DIMENSION(:,:,:)   ,INTENT(INOUT) :: PRUS,PRVS,PRWS
  REAL, DIMENSION(:,:,:)   ,INTENT(INOUT) :: PBAL
  !
  !------------------------------------------------------------------------------
  !
  !       0.2  declaration of local variables
  !
  INTEGER                               :: IIU,IJU,IKU
  INTEGER                               :: IIE,IIB,IJE,IJB,IKE,IKB
  INTEGER                               :: JI,JJ,JK,JL,JI2,JJ2,JK2,JM
  REAL                                  :: ZPH0,ZPH1,ZPH2,ZDEL,ZBAR,ZRAY,ZCOE,ZCO2
  REAL                                  :: ZVIT1,ZVIT2,ZVIT0,ZSIG0,ZSIG1,ZSIG2
  REAL, DIMENSION(:,:,:,:) ,ALLOCATABLE :: ZIBM_FLUX
  REAL, DIMENSION(:,:,:)   ,ALLOCATABLE :: ZFLU
  REAL                                  :: ZTOTO
  REAL                                  :: ZINVROOTPI
  !
  !------------------------------------------------------------------------------
  !
  !       0.3  Allocation
  !
  CALL GET_DIM_EXT_ll('B',IIU,IJU)
  IKU = SIZE(PPHI,3)
  CALL GET_INDICE_ll(IIB,IJB,IIE,IJE)
  IKE = IKU - JPVEXT
  IKB =   1 + JPVEXT
  ALLOCATE(ZIBM_FLUX(IIU,IJU,IKU,3))
  ALLOCATE(ZFLU(IIU,IJU,IKU))
  ! 
  !------------------------------------------------------------------------------
  !
  !**** 1. PRELIMINARIES
  !     ----------------
  !
  ZIBM_FLUX = 0.
  ZFLU = 0.
  ZTOTO = 1.0
  ZINVROOTPI = 1.0/SQRT(XPI)
  !
  !------------------------------------------------------------------------------
  !
  !**** 2. EXECUTIONS
  !     -------------
  !
  !
  DO JK=IKB,IKE
     DO JJ=IJB,IJE
        DO JI=IIB,IIE
           !
           IF (PVOL(JI,JJ,JK,3).gt.XIBM_EPSI) THEN
              !
              ! Flux, west
              JL = 2
              JI2 = JI
              ZIBM_FLUX(JI2,JJ,JK,JL-1) = 0.
              ZDEL = SQRT((XYHAT(JJ+1)-XYHAT(JJ))*0.5*(XZZ(JI2,JJ,JK+1)-XZZ(JI2,JJ,JK)+XZZ(JI2-1,JJ,JK+1)-XZZ(JI2-1,JJ,JK)))
              ZPH1 = PPHI(JI2 ,JJ  ,JK  ,JL)
              ZSIG1 = max(0.,-ZPH1/abs(ZPH1))
              ZVIT1 = ZSIG1*PRUS(JI2,JJ ,JK )
              !
              DO JM=1,8
                 IF (JM==1) THEN
                    JJ2 = JJ-1
                    JK2 = JK-1
                    ZCOE = sqrt(2.)
                 ENDIF
                 IF (JM==2) THEN
                    JJ2 = JJ-1
                    JK2 = JK
                    ZCOE = sqrt(1.)
                 ENDIF
                 IF (JM==3) THEN
                    JJ2 = JJ-1
                    JK2 = JK+1
                    ZCOE = sqrt(2.)
                 ENDIF
                 IF (JM==4) THEN
                    JJ2 = JJ+1
                    JK2 = JK-1
                    ZCOE = sqrt(2.)
                 ENDIF
                 IF (JM==5) THEN
                    JJ2 = JJ+1
                    JK2 = JK
                    ZCOE = sqrt(1.)
                 ENDIF
                 IF (JM==6) THEN
                    JJ2 = JJ+1
                    JK2 = JK+1
                    ZCOE = sqrt(2.)
                 ENDIF
                 IF (JM==7) THEN
                    JJ2 = JJ
                    JK2 = JK-1
                    ZCOE = sqrt(1.)
                 ENDIF
                 IF (JM==8) THEN
                    JJ2 = JJ
                    JK2 = JK+1
                    ZCOE = sqrt(1.)
                 ENDIF
                 !
                 ZPH2 = PPHI(JI2,JJ2,JK2,JL)
                 ZSIG2 = max(0.,-ZPH2/abs(ZPH2))
                 ZSIG0 = max(0.,-ZPH1*ZPH2/abs(ZPH1*ZPH2))
                 ZVIT2 = ZSIG2*PRUS(JI2,JJ2,JK2)
                 ZRAY = ZDEL*ZINVROOTPI*ZTOTO
                 ZBAR = 0.
                 !
                 IF (ABS(ZPH2-ZPH1).GT.XIBM_EPSI) ZBAR=-ZPH1/(ZPH2-ZPH1)*ZDEL*ZCOE
                 !
                 ZBAR = MIN(ZBAR,ZRAY)
                 ZBAR = MAX(ZBAR,  0.)
                 ZIBM_FLUX(JI2,JJ,JK,JL-1) = ZIBM_FLUX(JI2,JJ,JK,JL-1) + &
                      (ZSIG1*ZSIG2*ZVIT1+ZSIG0*(ZVIT1+ZVIT2)*abs(ZSIG2-(ZBAR/ZRAY)**2.))/8.*ZDEL**2.*ZTOTO**(-2.)
              ENDDO
              !
              ! Flux, East
              JL = 2
              JI2 = JI+1
              ZIBM_FLUX(JI2,JJ,JK,JL-1) = 0.
              ZDEL = SQRT((XYHAT(JJ+1)-XYHAT(JJ))*0.5*(XZZ(JI2,JJ,JK+1)-XZZ(JI2,JJ,JK)+XZZ(JI2-1,JJ,JK+1)-XZZ(JI2-1,JJ,JK)))
              ZPH1 = PPHI(JI2  ,JJ  ,JK  ,JL)
              ZSIG1 = max(0.,-ZPH1/abs(ZPH1))
              ZVIT1 = ZSIG1*PRUS(JI2,JJ ,JK )
              DO JM=1,8
                 IF (JM==1) THEN
                    JJ2 = JJ-1
                    JK2 = JK-1
                    ZCOE = sqrt(2.)
                 ENDIF
                 IF (JM==2) THEN
                    JJ2 = JJ-1
                    JK2 = JK
                    ZCOE = sqrt(1.)
                 ENDIF
                 IF (JM==3) THEN
                    JJ2 = JJ-1
                    JK2 = JK+1
                    ZCOE = sqrt(2.)
                 ENDIF
                 IF (JM==4) THEN
                    JJ2 = JJ+1
                    JK2 = JK-1
                    ZCOE = sqrt(2.)
                 ENDIF
                 IF (JM==5) THEN
                    JJ2 = JJ+1
                    JK2 = JK
                    ZCOE = sqrt(1.)
                 ENDIF
                 IF (JM==6) THEN
                    JJ2 = JJ+1
                    JK2 = JK+1
                    ZCOE = sqrt(2.)
                 ENDIF
                 IF (JM==7) THEN
                    JJ2 = JJ
                    JK2 = JK-1
                    ZCOE = sqrt(1.)
                 ENDIF
                 IF (JM==8) THEN
                    JJ2 = JJ
                    JK2 = JK+1
                    ZCOE = sqrt(1.)
                 ENDIF
                 !
                 ZPH2 = PPHI(JI2,JJ2,JK2,JL)
                 ZSIG2 = max(0.,-ZPH2/abs(ZPH2))
                 ZSIG0 = max(0.,-ZPH1*ZPH2/abs(ZPH1*ZPH2))
                 ZVIT2 = ZSIG2*PRUS(JI2,JJ2,JK2)
                 ZRAY = ZDEL*ZINVROOTPI*ZTOTO
                 ZBAR = 0.
                 IF (ABS(ZPH2-ZPH1).GT.XIBM_EPSI) ZBAR=-ZPH1/(ZPH2-ZPH1)*ZDEL*ZCOE
                 ZBAR = MIN(ZBAR,ZRAY)
                 ZBAR = MAX(ZBAR,  0.)
                 ZIBM_FLUX(JI2,JJ,JK,JL-1) = ZIBM_FLUX(JI2,JJ,JK,JL-1) + &
                      (ZSIG1*ZSIG2*ZVIT1+ZSIG0*(ZVIT1+ZVIT2)*abs(ZSIG2-(ZBAR/ZRAY)**2.))/8.*ZDEL**2.*ZTOTO**(-2.)
                 !
              ENDDO
              !
              ! Flux, south
              JL = 3 
              JJ2 = JJ
              ZIBM_FLUX(JI,JJ2,JK,JL-1) = 0.
              ZDEL = SQRT((XXHAT(JI+1)-XXHAT(JI))*0.5*(XZZ(JI,JJ2,JK+1)-XZZ(JI,JJ2,JK)+XZZ(JI,JJ2-1,JK+1)-XZZ(JI,JJ2-1,JK)))
              ZPH1 = PPHI(JI  ,JJ2  ,JK  ,JL)
              ZSIG1 = max(0.,-ZPH1/abs(ZPH1))
              ZVIT1 = ZSIG1*PRVS(JI ,JJ2,JK )
              !
              DO JM=1,8
                 !
                 IF (JM==1) THEN
                    JI2 = JI-1
                    JK2 = JK-1
                    ZCOE = sqrt(2.)
                 ENDIF
                 IF (JM==2) THEN
                    JI2 = JI-1
                    JK2 = JK
                    ZCOE = sqrt(1.)
                 ENDIF
                 IF (JM==3) THEN
                    JI2 = JI-1
                    JK2 = JK+1
                    ZCOE = sqrt(2.)
                 ENDIF
                 IF (JM==4) THEN
                    JI2 = JI+1
                    JK2 = JK-1
                    ZCOE = sqrt(2.)
                 ENDIF
                 IF (JM==5) THEN
                    JI2 = JI+1
                    JK2 = JK
                    ZCOE = sqrt(1.)
                 ENDIF
                 IF (JM==6) THEN
                    JI2 = JI+1
                    JK2 = JK+1
                    ZCOE = sqrt(2.)
                 ENDIF
                 IF (JM==7) THEN
                    JI2 = JI
                    JK2 = JK-1
                    ZCOE = sqrt(1.)
                 ENDIF
                 IF (JM==8) THEN
                    JI2 = JI
                    JK2 = JK+1
                    ZCOE = sqrt(1.)
                 ENDIF
                 !
                 ZPH2 = PPHI(JI2,JJ2,JK2,JL)
                 ZSIG2 = max(0.,-ZPH2/abs(ZPH2))
                 ZSIG0 = max(0.,-ZPH1*ZPH2/abs(ZPH1*ZPH2))
                 ZVIT2 = ZSIG2*PRVS(JI2,JJ2,JK2)
                 ZRAY = ZDEL * ZINVROOTPI * ZTOTO
                 ZBAR = 0.
                 IF (ABS(ZPH2-ZPH1).GT.XIBM_EPSI) ZBAR = -ZPH1/(ZPH2-ZPH1)*ZDEL*ZCOE
                 ZBAR = MIN(ZBAR,ZRAY)
                 ZBAR = MAX(ZBAR,  0.)
                 ZIBM_FLUX(JI,JJ2,JK,JL-1) = ZIBM_FLUX(JI,JJ2,JK,JL-1) + &
                      (ZSIG1*ZSIG2*ZVIT1+ZSIG0*(ZVIT1+ZVIT2)*abs(ZSIG2-(ZBAR/ZRAY)**2.))/8.*ZDEL**2.*ZTOTO**(-2.)
              ENDDO
              !
              ! Flux, north
              JL = 3 
              JJ2 = JJ+1
              ZIBM_FLUX(JI,JJ2,JK,JL-1) = 0.
              ZDEL = SQRT((XXHAT(JI+1)-XXHAT(JI))*0.5*(XZZ(JI,JJ2,JK+1)-XZZ(JI,JJ2,JK)+XZZ(JI,JJ2-1,JK+1)-XZZ(JI,JJ2-1,JK)))
              ZPH1 = PPHI(JI  ,JJ2  ,JK  ,JL)
              ZSIG1 = max(0.,-ZPH1/abs(ZPH1))
              ZVIT1 = ZSIG1*PRVS(JI ,JJ2,JK )
              !
              DO JM=1,8
                 !
                 IF (JM==1) THEN
                    JI2 = JI-1
                    JK2 = JK-1
                    ZCOE = sqrt(2.)
                 ENDIF
                 IF (JM==2) THEN
                    JI2 = JI-1
                    JK2 = JK
                    ZCOE = sqrt(1.)
                 ENDIF
                 IF (JM==3) THEN
                    JI2 = JI-1
                    JK2 = JK+1
                    ZCOE = sqrt(2.)
                 ENDIF
                 IF (JM==4) THEN
                    JI2 = JI+1
                    JK2 = JK-1
                    ZCOE = sqrt(2.)
                 ENDIF
                 IF (JM==5) THEN
                    JI2 = JI+1
                    JK2 = JK
                    ZCOE = sqrt(1.)
                 ENDIF
                 IF (JM==6) THEN
                    JI2 = JI+1
                    JK2 = JK+1
                    ZCOE = sqrt(2.)
                 ENDIF
                 IF (JM==7) THEN
                    JI2 = JI
                    JK2 = JK-1
                    ZCOE = sqrt(1.)
                 ENDIF
                 IF (JM==8) THEN
                    JI2 = JI
                    JK2 = JK+1
                    ZCOE = sqrt(1.)
                 ENDIF
                 !
                 ZPH2 = PPHI(JI2,JJ2,JK2,JL)
                 ZSIG2 = max(0.,-ZPH2/abs(ZPH2))
                 ZSIG0 = max(0.,-ZPH1*ZPH2/abs(ZPH1*ZPH2))
                 ZVIT2 = ZSIG2*PRVS(JI2,JJ2,JK2)
                 ZRAY = ZDEL * ZINVROOTPI * ZTOTO
                 ZBAR = 0.
                 IF (ABS(ZPH2-ZPH1).GT.XIBM_EPSI) ZBAR = -ZPH1/(ZPH2-ZPH1)*ZDEL*ZCOE
                 ZBAR = MIN(ZBAR,ZRAY)
                 ZBAR = MAX(ZBAR,  0.)
                 ZIBM_FLUX(JI,JJ2,JK,JL-1) = ZIBM_FLUX(JI,JJ2,JK,JL-1) + &
                      (ZSIG1*ZSIG2*ZVIT1+ZSIG0*(ZVIT1+ZVIT2)*abs(ZSIG2-(ZBAR/ZRAY)**2.))/8.*ZDEL**2.*ZTOTO**(-2.)
                 !
              ENDDO
              !
              ! Flux, bottom
              JL = 4 
              JK2 = JK
              ZIBM_FLUX(JI,JJ,JK2,JL-1) = 0.
              ZDEL = SQRT((XXHAT(JI+1)-XXHAT(JI))*(XYHAT(JJ+1)-XYHAT(JJ)))
              ZPH1 = PPHI(JI  ,JJ  ,JK2 ,JL)
              ZSIG1 = max(0.,-ZPH1/abs(ZPH1))
              ZVIT1 = ZSIG1*PRWS(JI ,JJ ,JK2)
              !
              DO JM=1,8
                 IF (JM==1) THEN
                    JJ2 = JJ-1
                    JI2 = JI-1
                    ZCOE = sqrt(2.)
                 ENDIF
                 IF (JM==2) THEN
                    JJ2 = JJ-1
                    JI2 = JI
                    ZCOE = sqrt(1.)
                 ENDIF
                 IF (JM==3) THEN
                    JJ2 = JJ-1
                    JI2 = JI+1
                    ZCOE = sqrt(2.)
                 ENDIF
                 IF (JM==4) THEN
                    JJ2 = JJ+1
                    JI2 = JI-1
                    ZCOE = sqrt(2.)
                 ENDIF
                 IF (JM==5) THEN
                    JJ2 = JJ+1
                    JI2 = JI
                    ZCOE = sqrt(1.)
                 ENDIF
                 IF (JM==6) THEN
                    JJ2 = JJ+1
                    JI2 = JI+1
                    ZCOE = sqrt(2.)
                 ENDIF
                 IF (JM==7) THEN
                    JJ2 = JJ
                    JI2 = JI-1
                    ZCOE = sqrt(1.)
                 ENDIF
                 IF (JM==8) THEN
                    JJ2 = JJ
                    JI2 = JI+1
                    ZCOE = sqrt(1.)
                 ENDIF
                 !
                 ZPH2 = PPHI(JI2,JJ2,JK2,JL)
                 ZSIG2 = max(0.,-ZPH2/abs(ZPH2))
                 ZSIG0 = max(0.,-ZPH1*ZPH2/abs(ZPH1*ZPH2))
                 ZVIT2 = ZSIG2*PRWS(JI2,JJ2,JK2)
                 ZRAY = ZDEL * ZINVROOTPI * ZTOTO
                 ZBAR = 0.
                 !
                 IF (ABS(ZPH2-ZPH1).GT.XIBM_EPSI) ZBAR = -ZPH1/(ZPH2-ZPH1)*ZDEL*ZCOE
                 ZBAR = MIN(ZBAR,ZRAY)
                 ZBAR = MAX(ZBAR,  0.)
                 ZIBM_FLUX(JI,JJ,JK2,JL-1) = ZIBM_FLUX(JI,JJ,JK2,JL-1) + &
                      (ZSIG1*ZSIG2*ZVIT1+ZSIG0*(ZVIT1+ZVIT2)*abs(ZSIG2-(ZBAR/ZRAY)**2.))/8.*ZDEL**2.*ZTOTO**(-2.)
                 !
              ENDDO
              !
              ! Flux, top
              JL = 4 
              JK2 = JK+1
              ZIBM_FLUX(JI,JJ,JK2,JL-1) = 0.
              ZDEL = SQRT((XXHAT(JI+1)-XXHAT(JI))*(XYHAT(JJ+1)-XYHAT(JJ)))
              ZPH1 = PPHI(JI  ,JJ  ,JK2 ,JL)
              ZSIG1 = max(0.,-ZPH1/abs(ZPH1))
              ZVIT1 = ZSIG1*PRWS(JI ,JJ ,JK2)
              !
              DO JM=1,8
                 !
                 IF (JM==1) THEN
                    JJ2 = JJ-1
                    JI2 = JI-1
                    ZCOE = sqrt(2.)
                 ENDIF
                 IF (JM==2) THEN
                    JJ2 = JJ-1
                    JI2 = JI
                    ZCOE = sqrt(1.)
                 ENDIF
                 IF (JM==3) THEN
                    JJ2 = JJ-1
                    JI2 = JI+1
                    ZCOE = sqrt(2.)
                 ENDIF
                 IF (JM==4) THEN
                    JJ2 = JJ+1
                    JI2 = JI-1
                    ZCOE = sqrt(2.)
                 ENDIF
                 IF (JM==5) THEN
                    JJ2 = JJ+1
                    JI2 = JI
                    ZCOE = sqrt(1.)
                 ENDIF
                 IF (JM==6) THEN
                    JJ2 = JJ+1
                    JI2 = JI+1
                    ZCOE = sqrt(2.)
                 ENDIF
                 IF (JM==7) THEN
                    JJ2 = JJ
                    JI2 = JI-1
                    ZCOE = sqrt(1.)
                 ENDIF
                 IF (JM==8) THEN
                    JJ2 = JJ
                    JI2 = JI+1
                    ZCOE = sqrt(1.)
                 ENDIF
                 !
                 ZPH2 = PPHI(JI2,JJ2,JK2,JL)
                 ZSIG2 = max(0.,-ZPH2/abs(ZPH2))
                 ZSIG0 = max(0.,-ZPH1*ZPH2/abs(ZPH1*ZPH2))
                 ZVIT2 = ZSIG2*PRWS(JI2,JJ2,JK2)
                 ZRAY = ZDEL * ZINVROOTPI * ZTOTO
                 ZBAR = 0.
                 IF (ABS(ZPH2-ZPH1).GT.XIBM_EPSI) ZBAR = -ZPH1/(ZPH2-ZPH1)*ZDEL*ZCOE
                 ZBAR = MIN(ZBAR,ZRAY)
                 ZBAR = MAX(ZBAR,  0.)
                 ZIBM_FLUX(JI,JJ,JK2,JL-1) = ZIBM_FLUX(JI,JJ,JK2,JL-1) + &
                      (ZSIG1*ZSIG2*ZVIT1+ZSIG0*(ZVIT1+ZVIT2)*abs(ZSIG2-(ZBAR/ZRAY)**2.))/8.*ZDEL**2.*ZTOTO**(-2.)
                 !
              ENDDO
              !
           ENDIF
           !
        ENDDO
     ENDDO
  ENDDO
  !
  ZFLU(IIB:IIE,IJB:IJE,IKB:IKE) = (ZIBM_FLUX(IIB+1:IIE+1,IJB  :IJE  ,IKB  :IKE  ,1)-ZIBM_FLUX(IIB:IIE,IJB:IJE,IKB:IKE,1) +&
       ZIBM_FLUX(IIB  :IIE  ,IJB+1:IJE+1,IKB  :IKE  ,2)-ZIBM_FLUX(IIB:IIE,IJB:IJE,IKB:IKE,2) +&
       ZIBM_FLUX(IIB  :IIE  ,IJB  :IJE  ,IKB+1:IKE+1,3)-ZIBM_FLUX(IIB:IIE,IJB:IJE,IKB:IKE,3))*&
       XRHODREF(IIB:IIE,IJB:IJE,IKB:IKE)/XRHODJ(IIB:IIE,IJB:IJE,IKB:IKE)
  !
  PBAL(IIB-1:IIE+1,IJB-1:IJE+1,IKB-1:IKE+1) = PBAL(IIB-1:IIE+1,IJB-1:IJE+1,IKB-1:IKE+1)* &
       PVOL(IIB-1:IIE+1,IJB-1:IJE+1,IKB-1:IKE+1,2)+ZFLU(IIB-1:IIE+1,IJB-1:IJE+1,IKB-1:IKE+1)* &
       PVOL(IIB-1:IIE+1,IJB-1:IJE+1,IKB-1:IKE+1,3)
  !
  !**** X. DEALLOCATIONS/CLOSES
  !     -----------------------
  DEALLOCATE(ZIBM_FLUX,ZFLU)
  ! 
  RETURN
  !
END SUBROUTINE IBM_BALANCE
