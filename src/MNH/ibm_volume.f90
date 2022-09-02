!MNH_LIC Copyright 2019-2022 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!
!    ########################
MODULE MODI_IBM_VOLUME
  !    ######################## 
  !
  INTERFACE
     !
     SUBROUTINE IBM_VOLUME(PPHI,PVOL)
       !
       REAL, DIMENSION(:,:,:,:) , INTENT(IN)    :: PPHI
       REAL, DIMENSION(:,:,:,:) , INTENT(INOUT) :: PVOL
       !
     END SUBROUTINE IBM_VOLUME
     !
  END INTERFACE
  !
END MODULE MODI_IBM_VOLUME
!
!     ##################################
SUBROUTINE IBM_VOLUME(PPHI,PVOL)
  !     ##################################
  !
  !
  !****  IBM_VOLUME computes surface and volume used in the alteration of the pseudo-equation 
  !                                
  !    PURPOSE
  !    -------
  !****  The purpose of this routine is to compute :
  !         - the surface used in the balance of momentum curvature 
  !         - a volumic fraction deducted from the LS function
  !
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
  USE MODD_IBM_PARAM_n        
  USE MODD_PARAMETERS, ONLY: JPVEXT,JPHEXT
  USE MODD_LBC_n
  USE MODD_LUNIT_n, ONLY: TLUOUT
  !
  ! interface
  !
  USE MODI_IBM_INTERPOS
  !
  IMPLICIT NONE
  !
  !------------------------------------------------------------------------------
  !
  !       0.1  declarations of arguments
  !
  REAL, DIMENSION(:,:,:,:), INTENT(IN)    :: PPHI ! LS functions
  REAL, DIMENSION(:,:,:,:), INTENT(INOUT) :: PVOL
  !
  !------------------------------------------------------------------------------
  !
  !       0.2  declaration of local variables
  !
  INTEGER                            :: IIU,IJU,IKU
  INTEGER                            :: IIE,IIB,IJE,IJB,IKE,IKB
  INTEGER                            :: JI,JJ,JK,JL,JM
  REAL                               :: ZPH1,ZPH2,ZPH3,ZPH4,ZPH5,ZCOE,ZRAY
  REAL                               :: ZPH6,ZPH7,ZPH8,ZDEL,ZPH0,ZBAR,ZVOL                               
  REAL,DIMENSION(:,:,:), ALLOCATABLE :: ZXREF,ZYREF,ZZREF
  TYPE(LIST_ll), POINTER             :: TZFIELDS_ll ! list of fields to exchange
  INTEGER                            :: IINFO_ll
  !
  !------------------------------------------------------------------------------
  !
  !       0.3  Allocation
  !
  IIU = SIZE(PPHI,1)
  IJU = SIZE(PPHI,2)
  IKU = SIZE(PPHI,3)
  !
  CALL GET_INDICE_ll(IIB,IJB,IIE,IJE)
  !
  IKE = IKU - JPVEXT
  IKB =   1 + JPVEXT
  ! 
  !------------------------------------------------------------------------------
  !
  !**** 1. PRELIMINARIES
  !     ----------------
  !
  ALLOCATE(ZXREF(IIU,IJU,IKU))
  ALLOCATE(ZYREF(IIU,IJU,IKU))
  ALLOCATE(ZZREF(IIU,IJU,IKU))
  !
  ZXREF = 0.
  ZYREF = 0.
  ZZREF = 0.
  !
  PVOL(:,:,:,:)=0.0
  !
  !------------------------------------------------------------------------------
  !
  !**** 2. EXECUTIONS
  !     -------------
  !
  !
  ! Volume computations
  JL = 1 
  PVOL(IIB:IIE,IJB:IJE,IKB:IKE,1:2)=1.
  !
  CALL IBM_INTERPOS(ZXREF,ZYREF,ZZREF,'P')
  !
  DO JK=IKB,IKE
     DO JJ=IJB,IJE
        DO JI=IIB,IIE
           !
           ZDEL = ((ZXREF(JI+1,JJ,JK)-ZXREF(JI,JJ,JK))*&
                (ZYREF(JI,JJ+1,JK)-ZYREF(JI,JJ,JK))*&
                (ZZREF(JI,JJ,JK+1)-ZZREF(JI,JJ,JK)))**(1./3.)
           !
           ZRAY = ZDEL/2.
           ZCOE =   1./2.
           ZPH1 = PPHI(JI  ,JJ  ,JK  ,1)
           !
           DO JM=1,6
              !
              IF (JM==1) ZPH2 = PPHI(JI  ,JJ  ,JK  ,2)
              IF (JM==2) ZPH2 = PPHI(JI+1,JJ  ,JK  ,2)
              IF (JM==3) ZPH2 = PPHI(JI  ,JJ  ,JK  ,3)
              IF (JM==4) ZPH2 = PPHI(JI  ,JJ+1,JK  ,3)
              IF (JM==5) ZPH2 = PPHI(JI  ,JJ  ,JK  ,4)
              IF (JM==6) ZPH2 = PPHI(JI  ,JJ  ,JK+1,4)
              !
              ZBAR=0.
              !
              IF (ABS(ZPH2-ZPH1).GT.(XIBM_EPSI)) ZBAR = - ZPH1 / ( ZPH2 - ZPH1 ) * ZDEL * ZCOE
              !
              ZBAR=min(ZRAY,ZBAR)
              ZBAR=max(0.,ZBAR)
              !
              PVOL(JI,JJ,JK,1) = -max(0.,+ZPH2/abs(ZPH2))*max(0.,+ZPH1/abs(ZPH1))/6. + PVOL(JI,JJ,JK,1)  &
                   -max(0.,-ZPH2*ZPH1/abs(ZPH2*ZPH1))*ABS(max(0.,+ZPH2/abs(ZPH2))-(ZBAR/ZRAY)**3.)/6.
              !
              PVOL(JI,JJ,JK,1) = min(1.,PVOL(JI,JJ,JK,1))
              !
           ENDDO
           !
        ENDDO
     ENDDO
  ENDDO
  !
  IF (LWEST_ll ()) PVOL(IIB-1,:,:,1)=PVOL(IIB,:,:,1)
  IF (LEAST_ll ()) PVOL(IIE+1,:,:,1)=PVOL(IIE,:,:,1)
  IF (LSOUTH_ll()) PVOL(:,IJB-1,:,1)=PVOL(:,IJB,:,1)
  IF (LNORTH_ll()) PVOL(:,IJE+1,:,1)=PVOL(:,IJE,:,1)
  !
  PVOL(:,:,IKB-1,1)=PVOL(:,:,IKB,1)
  PVOL(:,:,IKE+1,1)=PVOL(:,:,IKE,1)
  !
  IF(LWEST_ll()) THEN
     PVOL(IIB-1,IJB:IJE,IKB-1,1)=PVOL(IIB-1,IJB:IJE,IKB,1)
     PVOL(IIB-1,IJB:IJE,IKE+1,1)=PVOL(IIB-1,IJB:IJE,IKE,1)
  END IF
  !
  IF (LEAST_ll()) THEN
     PVOL(IIE+1,IJB:IJE,IKB-1,1)=PVOL(IIE+1,IJB:IJE,IKB,1)
     PVOL(IIE+1,IJB:IJE,IKE+1,1)=PVOL(IIE+1,IJB:IJE,IKE,1)
  END IF
  !
  IF (LSOUTH_ll()) THEN
     PVOL(IIB:IIE,IJB-1,IKB-1,1)=PVOL(IIB:IIE,IJB-1,IKB,1)
     PVOL(IIB:IIE,IJB-1,IKE+1,1)=PVOL(IIB:IIE,IJB-1,IKE,1)
  END IF
  !
  IF (LNORTH_ll()) THEN
     PVOL(IIB:IIE,IJE+1,IKB-1,1)=PVOL(IIB:IIE,IJE+1,IKB,1)
     PVOL(IIB:IIE,IJE+1,IKE+1,1)=PVOL(IIB:IIE,IJE+1,IKE,1)
  END IF
  !
  !**************************************************
  !
  WHERE ( PVOL(:,:,:,1).lt.(XIBM_EPSI) ) PVOL(:,:,:,1)=0.
  WHERE ( PVOL(:,:,:,1).lt.(1.)        ) PVOL(:,:,:,2)=0.
  WHERE ( (PVOL(:,:,:,1)-PVOL(:,:,:,2) ) .GT. 0.0 ) PVOL(:,:,:,3)=1.0
  !
  !------------------------------------------------------------------------------
  !**** X. DEALLOCATIONS/CLOSES
  !     -----------------------
  DEALLOCATE(ZXREF,ZYREF,ZZREF)
  !
  RETURN
  !
END SUBROUTINE IBM_VOLUME
