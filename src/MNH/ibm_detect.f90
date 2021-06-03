!MNH_LIC Copyright 1994-2018 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!
!    ######################
MODULE MODI_IBM_DETECT  
  !    ###################### 
  !
  INTERFACE
     !
     SUBROUTINE IBM_DETECT(PPHI)
       !
       REAL, DIMENSION(:,:,:,:) ,INTENT(IN) :: PPHI
       !
     END SUBROUTINE IBM_DETECT
     !
  END INTERFACE
  !
END MODULE MODI_IBM_DETECT
!
!     ###########################
SUBROUTINE IBM_DETECT(PPHI)
  !     ###########################
  !
  !
  !****  IBM_DETECT is dedicated to the characterization of the ghost point and
  !                    associated image points
  !                
  !    PURPOSE
  !    -------
  !****  The purpose of this routine is to affect an specific index to cells where
  !      ghost points are localized. Depending on order of numerical scheme the 
  !      thickness of ghost points layer varies as the index value. For each cell
  !      marked as ghost the corresponding image point location is stored. 

  !    METHOD
  !    ------
  !****  Iterative procedure to characterize ghost point locations 
  !      - local test on the sign change of the levelset function (first layer)
  !      - local detection of the first layer to define the neighboring second layer
  !      - repeat of the previous step for high order numerical scheme
  !    
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
  !
  ! module
  USE MODE_POS
  USE MODE_ll
  USE MODE_IO
  !
  ! declaration
  USE MODD_IBM_PARAM_n
  USE MODD_PARAMETERS, ONLY: JPVEXT,JPHEXT
  USE MODD_GRID_n, ONLY: XXHAT,XYHAT,XZHAT,XZZ
  USE MODD_METRICS_n, ONLY: XDXX,XDYY,XDZZ,XDZX,XDZY
  USE MODD_LBC_n
  USE MODD_CONF, ONLY: NHALO
  USE MODD_VAR_ll, ONLY: IP
  USE MODD_REF_n, ONLY: XRHODJ,XRHODREF
  !
  ! interface
  USE MODI_SHUMAN
  USE MODI_GRADIENT_M
  USE MODI_GRADIENT_U
  USE MODI_GRADIENT_V
  USE MODI_GRADIENT_W
  USE MODI_IBM_LOCATCORN
  USE MODI_IBM_VALUECORN
  USE MODI_IBM_INTERPOS
  USE MODI_GRADIENT_UV
  USE MODI_GRADIENT_VW
  USE MODI_GRADIENT_UW
  USE MODI_GDIV
  !
  IMPLICIT NONE
  !
  !------------------------------------------------------------------------------
  !
  !       0.1  declarations of arguments
  !
  REAL, DIMENSION(:,:,:,:), INTENT(IN) :: PPHI ! LevelSet functions
  !
  !------------------------------------------------------------------------------
  !
  !       0.2  declaration of local variables
  !
  INTEGER                                  :: IIB,IJB,IKB,IIE,IJE,IKE                    ! physical domain size
  INTEGER                                  :: IIU,IJU,IKU,IIUM,IJUM,IKUM,JN1,JN2         ! numerical domain size
  INTEGER                                  :: JI,JJ,JK,JI2,JJ2,JK2,JL,JM,JN,JMM,JNN,JP   ! loop index
  INTEGER                                  :: JIM1,JIP1,JJM1,JJP1,JKM1,JKP1,JI3,JJ3,JK3  ! loop boundaries
  INTEGER                                  :: JIM2,JIP2,JJM2,JJP2,JKM2,JKP2
  INTEGER, DIMENSION(:,:,:,:), ALLOCATABLE :: I_INDE_GHOST                               ! ghosts index storage
  INTEGER                                  :: I_DIME_GHOST,I_INDE_LOCAT
  INTEGER, DIMENSION(:,:)    , ALLOCATABLE :: I_NUMB_GHOST
  INTEGER, DIMENSION(:)      , ALLOCATABLE :: I_INDE_TEMPO,I_INDE_TEMPO2
  TYPE(LIST_ll), POINTER                   :: TZFIELDS_ll                                ! list of fields to exchange
  INTEGER                                  :: IINFO_ll,I_NUMB_LAYER
  REAL, DIMENSION(:,:,:,:)   , ALLOCATABLE :: ZXPOS,ZYPOS,ZZPOS,Z_NORM_TEMP1             ! staggered grid arrays
  REAL, DIMENSION(:,:,:)     , ALLOCATABLE :: Z_NORM_TEMP2,Z_NORM_TEMP3
  REAL, DIMENSION(:,:,:,:)   , ALLOCATABLE :: Z_NORM_GHOST                               ! vec(n)
  REAL, DIMENSION(:,:,:,:)   , ALLOCATABLE :: Z_NORM_TEMPO,ZIBM_TESTING,ZPHI
  REAL                                     :: ZLGHO
  REAL, DIMENSION(:)         , ALLOCATABLE :: ZVECT,ZPROD,Z_PHI
  REAL, DIMENSION(:,:)       , ALLOCATABLE :: Z_IMG,Z_GHO
  INTEGER                                  :: I_NUMB_LAYERV,I_NUMB_LAYERP,I_DIME_GHOSTV,I_DIME_GHOSTP
  REAL                                     :: ZSEAR,ZISI,ZJSI,ZKSI,ZLIMG
  REAL                                     :: ZIBM_TESTI,PPHI_CORR,PPHI_TEST
  INTEGER                                  :: JHALO,IKM,JLL
  !
  !------------------------------------------------------------------------------
  !
  !       0.3  Allocation
  !
  IIU=SIZE(PPHI,1)
  IJU=SIZE(PPHI,2)
  IKU=SIZE(PPHI,3)
  CALL GET_INDICE_ll(IIB,IJB,IIE,IJE)
  IKB=1  +JPVEXT
  IKE=IKU-JPVEXT
  IKM=INT(IKU/2.)
  !
  ALLOCATE(I_INDE_GHOST(IIB:IIE,IJB:IJE,IKB:IKE,4))
  ALLOCATE(ZIBM_TESTING(IIU,IJU,IKU,4))
  ALLOCATE(Z_PHI(8),ZPROD(6),ZVECT(3),Z_IMG(8,3),Z_GHO(8,3),I_INDE_TEMPO(3),I_INDE_TEMPO2(3))
  !
  !------------------------------------------------------------------------------
  !
  !**** 1. PRELIMINARIES
  !     ----------------
  JHALO                 = 0
  ZVECT(:)              = 0.
  ZPROD(:)              = 0.
  Z_PHI(:)              = 0.
  Z_IMG(:,:)            = 0.
  Z_GHO(:,:)            = 0.
  I_INDE_TEMPO(:)       = 0
  I_INDE_GHOST(:,:,:,:) = 0
  Z_NORM_GHOST(:,:,:,:) = 0.
  Z_NORM_TEMPO(:,:,:,:) = 0.
  ZIBM_TESTING(:,:,:,:) = 0.
  !
  !**** 2. EXECUTIONS
  !     -------------
  !
  !I_IBM_NUMB_GHOST stores the ghost number per layer and node type 
  I_NUMB_LAYERV = NIBM_LAYER_V
  I_NUMB_LAYERP = max(NIBM_LAYER_P,NIBM_LAYER_T,NIBM_LAYER_E, &
       NIBM_LAYER_R,NIBM_LAYER_Q,NIBM_LAYER_S)
  I_NUMB_LAYER = max(I_NUMB_LAYERV,I_NUMB_LAYERP)
  !
  ALLOCATE(I_NUMB_GHOST(4,I_NUMB_LAYER))
  I_NUMB_GHOST(:,:)=0
  !
  ! Ghost cells detection
  DO JL = 1,4
     ! 
     ! Number of ghost layers per PUVW nodes
     IF (JL==1) THEN
        I_NUMB_LAYER = I_NUMB_LAYERP
     ELSE
        I_NUMB_LAYER = I_NUMB_LAYERV
     ENDIF
     !
     ! arrays computation
     IF (JL==1) THEN
        IIUM=IIE
        IJUM=IJE
        IKUM=IKE
     ENDIF
     IF (JL==2) THEN
        IIUM=IIE
        IJUM=IJE
        IKUM=IKE
     ENDIF
     IF (JL==3) THEN
        IIUM=IIE
        IJUM=IJE
        IKUM=IKE
     ENDIF
     IF (JL==4) THEN
        IIUM=IIE
        IJUM=IJE
        IKUM=IKE
     ENDIF
     !
     DO JK = IKB,IKUM
        !
        JKM1 = JK-I_NUMB_LAYER
        JKP1 = JK+I_NUMB_LAYER
        !
        IF (JK==IKB ) JKM1 = JK
        IF (JK==IKUM) JKP1 = JK
        IF (I_NUMB_LAYER>=2) THEN
           IF (JK==IKB+1 ) JKM1 = JK-1
           IF (JK==IKUM-1) JKP1 = JK+1
        ENDIF
        IF (I_NUMB_LAYER>=3) THEN
           IF (JK==IKB+2 ) JKM1 = JK-2
           IF (JK==IKUM-2) JKP1 = JK+2
        ENDIF
        JKM1 = max(2    ,JKM1)
        JKP1 = min(IKU-1,JKP1)
        !
        DO JJ = IJB,IJUM
           !  
           JJM1 = JJ-I_NUMB_LAYER
           JJP1 = JJ+I_NUMB_LAYER
           !
           IF (LSOUTH_ll().and.JJ==IJB)    JJM1=JJ
           IF (LNORTH_ll().and.JJ==IJUM)   JJP1=JJ
           IF (I_NUMB_LAYER>=2) THEN
              IF (LSOUTH_ll().and.JJ==IJB+1)  JJM1=JJ-1
              IF (LNORTH_ll().and.JJ==IJUM-1) JJP1=JJ+1
           ENDIF
           IF (I_NUMB_LAYER>=3) THEN
              IF (LSOUTH_ll().and.JJ==IJB+2)  JJM1=JJ-2
              IF (LNORTH_ll().and.JJ==IJUM-2) JJP1=JJ+2
           ENDIF
           JJM1 = max(1  ,JJM1)
           JJP1 = min(IJU,JJP1)
           ! 
           DO JI = IIB,IIUM
              !
              JIM1 = JI-I_NUMB_LAYER
              JIP1 = JI+I_NUMB_LAYER
              !
              IF (LWEST_ll().and.JI==IIB)    JIM1=JI
              IF (LEAST_ll().and.JI==IIUM)   JIP1=JI
              IF (I_NUMB_LAYER>=2) THEN        
                 IF (LWEST_ll().and.JI==IIB+1)  JIM1=JI-1
                 IF (LEAST_ll().and.JI==IIUM-1) JIP1=JI+1
              ENDIF
              IF (I_NUMB_LAYER>=3) THEN
                 IF (LWEST_ll().and.JI==IIB+2)  JIM1=JI-2
                 IF (LEAST_ll().and.JI==IIUM-2) JIP1=JI+2
              ENDIF
              JIM1 = max(1  ,JIM1)
              JIP1 = min(IIU,JIP1)
              !
              ! test for embedded solid region
              IF (PPHI(JI,JJ,JK,JL).gt.-XIBM_EPSI) THEN
                 !
                 DO JM=1,3

                    IF (JM==1) THEN
                       JIM2 = JI
                       JIP2 = JI
                       JJM2 = JJ
                       JJP2 = JJ
                       JKM2 = JKM1
                       JKP2 = JKP1
                    ENDIF
                    IF (JM==2) THEN
                       JIM2 = JIM1
                       JIP2 = JIP1
                       JJM2 = JJ
                       JJP2 = JJ
                       JKM2 = JK
                       JKP2 = JK
                    ENDIF
                    IF (JM==3) THEN
                       JIM2 = JI
                       JIP2 = JI
                       JJM2 = JJM1
                       JJP2 = JJP1
                       JKM2 = JK
                       JKP2 = JK
                    ENDIF
                    !
                    DO JK2= JKM2,JKP2
                       DO JJ2= JJM2,JJP2
                          DO JI2= JIM2,JIP2
                             !
                             ! interface presence test (multi layer) 
                             IF ((PPHI(JI,JJ,JK,JL)*PPHI(JI2,JJ2,JK2,JL)).lt.-XIBM_EPSI) THEN
                                I_INDE_LOCAT = max(abs(JI-JI2),abs(JJ-JJ2),abs(JK-JK2))
                                IF (I_INDE_GHOST(JI,JJ,JK,JL)/=0) THEN
                                   I_INDE_GHOST(JI,JJ,JK,JL) = min(I_INDE_GHOST(JI,JJ,JK,JL),I_INDE_LOCAT)
                                   ZIBM_TESTING(JI,JJ,JK,JL)=I_INDE_GHOST(JI,JJ,JK,JL)*1.
                                ELSE
                                   I_INDE_GHOST(JI,JJ,JK,JL) = I_INDE_LOCAT
                                   ZIBM_TESTING(JI,JJ,JK,JL)=I_INDE_GHOST(JI,JJ,JK,JL)*1.
                                ENDIF
                             ENDIF
                          ENDDO
                       ENDDO
                    ENDDO
                    !
                 ENDDO
                 !
                 ! ghosts counter
                 IF (I_INDE_GHOST(JI,JJ,JK,JL)>0) THEN
                    I_NUMB_GHOST(JL,I_INDE_GHOST(JI,JJ,JK,JL))=I_NUMB_GHOST(JL,I_INDE_GHOST(JI,JJ,JK,JL))+1
                 ENDIF
                 !
              ENDIF
              !
           ENDDO
        ENDDO
     ENDDO
     !
  ENDDO
  !
  I_DIME_GHOSTV = 0
  DO JL=1,I_NUMB_LAYERV
     I_DIME_GHOSTV = max(I_DIME_GHOSTV,I_NUMB_GHOST(2,JL),I_NUMB_GHOST(3,JL),I_NUMB_GHOST(4,JL))
  ENDDO
  I_DIME_GHOSTP = 0
  DO JL=1,I_NUMB_LAYERP  
     I_DIME_GHOSTP = max(I_DIME_GHOSTP,I_NUMB_GHOST(1,JL))
  ENDDO
  !
  ! === GHOSTS storage === 
  ! NIBM_STOR_GHOSV(A,B,C)
  ! A : number of ghosts for each type of nodes
  ! B : type of ghosts layer
  ! C : type of ghosts PUVW
  ! D : index location IJK
  ALLOCATE(NIBM_GHOST_V(I_DIME_GHOSTV,I_NUMB_LAYERV,3,3))
  NIBM_GHOST_V(:,:,:,:) = 0
  !
  ! NIBM_STOR_GHOSP(A,B,C)
  ! A : number of ghosts for each type of nodes P
  ! B : type of ghosts layer
  ! C : ---
  ! D : index location IJK
  ALLOCATE(NIBM_GHOST_P(I_DIME_GHOSTP,I_NUMB_LAYERP,1,3))
  NIBM_GHOST_P(:,:,:,:) = 0  
  !
  ! XIBM_STOR_GHOSV(A,B,C,D)
  ! A : number of ghosts in each type of nodes PUVW
  ! B : layer number  
  ! C : type of nodes UVW for the image(s)
  ! D : location of the ghost
  ALLOCATE(XIBM_GHOST_V(I_DIME_GHOSTV,I_NUMB_LAYERV,3,3))
  XIBM_GHOST_V(:,:,:,:) = 0.
  !
  ! XIBM_STOR_GHOSP(A,B,C,D,E)
  ! A : number of ghosts in each type of nodes P
  ! B : layer number
  ! C : ---
  ! D : location of the ghost
  ALLOCATE(XIBM_GHOST_P(I_DIME_GHOSTP,I_NUMB_LAYERP,1,3))
  XIBM_GHOST_P(:,:,:,:) = 0.
  !
  ! Reset ghost research
  I_NUMB_GHOST(:,:) = 0
  DO JL = 1,4
     ! 
     ! Number of ghost layers per PUVW nodes
     IF (JL==1) THEN
        I_NUMB_LAYER = I_NUMB_LAYERP
     ELSE
        I_NUMB_LAYER = I_NUMB_LAYERV
     ENDIF
     !
     IIUM=IIE
     IJUM=IJE
     IKUM=IKE
     !
     DO JM = 1,I_NUMB_LAYER
        DO JK = IKB,IKUM
           DO JJ = IJB,IJUM
              DO JI = IIB,IIUM
                 IF (I_INDE_GHOST(JI,JJ,JK,JL)==JM) THEN
                    I_NUMB_GHOST(JL,JM) = I_NUMB_GHOST(JL,JM) + 1
                    IF (JL==1) THEN
                       NIBM_GHOST_P(I_NUMB_GHOST(JL,JM),JM,JL  ,1) = JI
                       NIBM_GHOST_P(I_NUMB_GHOST(JL,JM),JM,JL  ,2) = JJ
                       NIBM_GHOST_P(I_NUMB_GHOST(JL,JM),JM,JL  ,3) = JK
                    ELSE
                       NIBM_GHOST_V(I_NUMB_GHOST(JL,JM),JM,JL-1,1) = JI
                       NIBM_GHOST_V(I_NUMB_GHOST(JL,JM),JM,JL-1,2) = JJ
                       NIBM_GHOST_V(I_NUMB_GHOST(JL,JM),JM,JL-1,3) = JK
                    ENDIF
                 ENDIF
              ENDDO
           ENDDO
        ENDDO
     ENDDO
     !
  ENDDO
  !
  !=== IMAGES cells detection === 
  !
  ! NIBM_TEST_IMAGV(A,B,C,D,E,F)
  ! A : number of ghosts in each type of nodes UVW
  ! B : layer number
  ! C : UVW node type for ghost
  ! D : UVW node type for image
  ! E : 1 for MIRROR or IMAGE1 - 2 for IMAGE2 - 3 for MIRROR
  ! F : corner index
  ALLOCATE(XIBM_TESTI_V(I_DIME_GHOSTV,I_NUMB_LAYERV,3,3,3,8))
  XIBM_TESTI_V = 1.
  !
  ! NIBM_TEST_IMAGP(A,B,C,D,E,F)
  ! A : number of ghosts in each type of nodes P
  ! B : layer number
  ! C : ---
  ! D : ---
  ! E : 1 for MIRROR or IMAGE1 - 2 for IMAGE2 - 3 for MIRROR
  ! F : corner index
  ALLOCATE(XIBM_TESTI_P(I_DIME_GHOSTP,I_NUMB_LAYERP,1,1,3,8))
  XIBM_TESTI_P = 1.
  !
  ! NIBM_STOR_IMAGV(A,B,C,D,E,F)
  ! A : number of ghosts in each type of nodes UVW
  ! B : layer number
  ! C : UVW node type for ghost
  ! D : UVW node type for image
  ! E : 1 for MIRROR or IMAGE1 - 2 for IMAGE2 - 3 for MIRROR
  ! F : index of the image(s) 
  ALLOCATE(NIBM_IMAGE_V(I_DIME_GHOSTV,I_NUMB_LAYERV,3,3,3,3))
  NIBM_IMAGE_V(:,:,:,:,:,:) = 0
  !
  ! NIBM_STOR_IMAGP(A,B,C,D,E,F)
  ! A : number of ghosts in each type of nodes P
  ! B : layer number
  ! C : ---
  ! D : ---
  ! E : 1 for MIRROR or IMAGE1 - 2 for IMAGE2 - 3 for MIRROR
  ! F : index of the image(s) 
  ALLOCATE(NIBM_IMAGE_P(I_DIME_GHOSTP,I_NUMB_LAYERP,1,1,3,3))
  NIBM_IMAGE_P(:,:,:,:,:,:) = 0
  !
  ! XIBM_STOR_IMAGV(A,B,C,D,E)
  ! A : number of ghosts in each type of nodes PUVW
  ! B : layer number  
  ! C : type of nodes UVW for the image(s)
  ! D : 1 for IMAGE1 - 2 for IMAGE2 - 3 for MIRROR 
  ! E : location of the image(s)
  ALLOCATE(XIBM_IMAGE_V(I_DIME_GHOSTV,I_NUMB_LAYERV,3,3,3))
  XIBM_IMAGE_V(:,:,:,:,:) = 0.
  !
  ! XIBM_STOR_IMAGP(A,B,C,D,E)
  ! A : number of ghosts in each type of nodes P
  ! B : layer number
  ! C : ---
  ! D : 1 for IMAGE1 - 2 for IMAGE2 - 3 for MIRROR 
  ! E : location of the image(s)
  ALLOCATE(XIBM_IMAGE_P(I_DIME_GHOSTP,I_NUMB_LAYERP,1,3,3))
  XIBM_IMAGE_P(:,:,:,:,:) = 0.
  !
  ALLOCATE(Z_NORM_GHOST(IIU,IJU,IKU,3),Z_NORM_TEMPO(IIU,IJU,IKU,3),Z_NORM_TEMP1(IIU,IJU,IKU,4),Z_NORM_TEMP2(IIU,IJU,IKU), & 
       Z_NORM_TEMP3(IIU,IJU,IKU))
  ALLOCATE(ZPHI(IIU,IJU,IKU,4))
  ZPHI = 0.
  !
  DO JL = 1,4
     ! 
     ! Number of ghost layers per PUVW nodes
     IF (JL==1) THEN
        I_NUMB_LAYER = I_NUMB_LAYERP
     ELSE
        I_NUMB_LAYER = I_NUMB_LAYERV
     ENDIF
     IF (I_NUMB_LAYER==0) GO TO 667
     !
     ! div(n) computation
     IF (JL==1) THEN
        Z_NORM_TEMPO(:,:,:,1) = - GX_U_M(PPHI(:,:,:,2),XDXX,XDZZ,XDZX)
        Z_NORM_TEMPO(:,:,:,2) = - GY_V_M(PPHI(:,:,:,3),XDYY,XDZZ,XDZY)
        Z_NORM_TEMPO(:,:,:,3) = - GZ_W_M(PPHI(:,:,:,4),XDZZ)
        Z_NORM_TEMP1(:,:,:,1) = - GX_M_U(1,IKU,1,PPHI(:,:,:,1),XDXX,XDZZ,XDZX) 
        Z_NORM_TEMP1(:,:,:,2) = - GY_M_V(1,IKU,1,PPHI(:,:,:,1),XDYY,XDZZ,XDZY)
        Z_NORM_TEMP1(:,:,:,3) = - GZ_M_W(1,IKU,1,PPHI(:,:,:,1),XDZZ)
        CALL GDIV(CLBCX,CLBCY,XDXX,XDYY,XDZX,XDZY,XDZZ,Z_NORM_TEMP1(:,:,:,1),Z_NORM_TEMP1(:,:,:,2),Z_NORM_TEMP1(:,:,:,3), &
             XIBM_CURV(:,:,:))
        XIBM_CURV(:,:,:)=-XIBM_CURV(:,:,:)*(XRHODJ(:,:,:)/XRHODREF(:,:,:))**(2./3.)
        IF (LWEST_ll ()) XIBM_CURV(1,:,:)   = XIBM_CURV(2    ,:,:)
        IF (LEAST_ll ()) XIBM_CURV(IIU,:,:) = XIBM_CURV(IIU-1,:,:)  
        IF (LSOUTH_ll()) XIBM_CURV(:,1,:)   = XIBM_CURV(:,2    ,:)
        IF (LNORTH_ll()) XIBM_CURV(:,IJU,:) = XIBM_CURV(:,IJU-1,:)
        XIBM_CURV(:,:,1  ) = XIBM_CURV(:,:,    2)
        XIBM_CURV(:,:,IKU) = XIBM_CURV(:,:,IKU-1)
        NULLIFY(TZFIELDS_ll)
        CALL ADD3DFIELD_ll(TZFIELDS_ll,XIBM_CURV(:,:,:),'IBM_DETECT::XIBM_CURV')
        CALL UPDATE_HALO_ll(TZFIELDS_ll,IINFO_ll)
        CALL CLEANLIST_ll(TZFIELDS_ll)
        XIBM_SU(:,:,:,1)=MXM(XIBM_CURV(:,:,:))
        XIBM_SU(:,:,:,2)=MYM(XIBM_CURV(:,:,:))
        XIBM_SU(:,:,:,3)=MZM(XIBM_CURV(:,:,:))
        IF (LWEST_ll ()) XIBM_SU(1,:,:,1)   = XIBM_SU(2    ,:,:,1)
        IF (LEAST_ll ()) XIBM_SU(IIU,:,:,1) = XIBM_SU(IIU-1,:,:,1)  
        IF (LSOUTH_ll()) XIBM_SU(:,1,:,1)   = XIBM_SU(:,2    ,:,1)
        IF (LNORTH_ll()) XIBM_SU(:,IJU,:,1) = XIBM_SU(:,IJU-1,:,1)
        XIBM_SU(:,:,1  ,1) = XIBM_SU(:,:,    2,1)
        XIBM_SU(:,:,IKU,1) = XIBM_SU(:,:,IKU-1,1)
        IF (LWEST_ll ()) XIBM_SU(1,:,:,2)   = XIBM_SU(2    ,:,:,2)
        IF (LEAST_ll ()) XIBM_SU(IIU,:,:,2) = XIBM_SU(IIU-1,:,:,2)  
        IF (LSOUTH_ll()) XIBM_SU(:,1,:,2)   = XIBM_SU(:,2    ,:,2)
        IF (LNORTH_ll()) XIBM_SU(:,IJU,:,2) = XIBM_SU(:,IJU-1,:,2)
        XIBM_SU(:,:,1  ,2) = XIBM_SU(:,:,    2,2)
        XIBM_SU(:,:,IKU,2) = XIBM_SU(:,:,IKU-1,2)
        IF (LWEST_ll ()) XIBM_SU(1,:,:,3)   = XIBM_SU(2    ,:,:,3)
        IF (LEAST_ll ()) XIBM_SU(IIU,:,:,3) = XIBM_SU(IIU-1,:,:,3)  
        IF (LSOUTH_ll()) XIBM_SU(:,1,:,3)   = XIBM_SU(:,2    ,:,3)
        IF (LNORTH_ll()) XIBM_SU(:,IJU,:,3) = XIBM_SU(:,IJU-1,:,3)
        XIBM_SU(:,:,1  ,3) = XIBM_SU(:,:,    2,3)
        XIBM_SU(:,:,IKU,3) = XIBM_SU(:,:,IKU-1,3)
        !
        NULLIFY(TZFIELDS_ll)
        CALL ADD3DFIELD_ll(TZFIELDS_ll,XIBM_SU(:,:,:,1),'IBM_DETECT::XIBM_SU')
        CALL ADD3DFIELD_ll(TZFIELDS_ll,XIBM_SU(:,:,:,2),'IBM_DETECT::XIBM_SU')
        CALL ADD3DFIELD_ll(TZFIELDS_ll,XIBM_SU(:,:,:,3),'IBM_DETECT::XIBM_SU')
        CALL UPDATE_HALO_ll(TZFIELDS_ll,IINFO_ll)
        CALL CLEANLIST_ll(TZFIELDS_ll)
        !
        XIBM_CURV(:,:,:)=0.5*XIBM_CURV(:,:,:)+0.5/3.*(MXF(XIBM_SU(:,:,:,1))+ &
             MYF(XIBM_SU(:,:,:,2))+ &
             MZF(XIBM_SU(:,:,:,3)))
        !
        IF (LWEST_ll ()) XIBM_CURV(1,:,:)   = XIBM_CURV(2    ,:,:)
        IF (LEAST_ll ()) XIBM_CURV(IIU,:,:) = XIBM_CURV(IIU-1,:,:)  
        IF (LSOUTH_ll()) XIBM_CURV(:,1,:)   = XIBM_CURV(:,2    ,:)
        IF (LNORTH_ll()) XIBM_CURV(:,IJU,:) = XIBM_CURV(:,IJU-1,:)
        XIBM_CURV(:,:,1  ) = XIBM_CURV(:,:,    2)
        XIBM_CURV(:,:,IKU) = XIBM_CURV(:,:,IKU-1)    
        !
        XIBM_CURV(:,:,:)=1./(ABS(XIBM_CURV(:,:,:))+XIBM_EPSI)
        XIBM_CURV(:,:,:)=MIN(1., XIBM_CURV(:,:,:))
        XIBM_CURV(:,:,:)=MAX(0., XIBM_CURV(:,:,:))
        XIBM_CURV(:,:,:)=1.-XIBM_CURV(:,:,:)
        NULLIFY(TZFIELDS_ll)
        CALL ADD3DFIELD_ll(TZFIELDS_ll,XIBM_CURV(:,:,:),'IBM_DETECT::XIBM_CURV')
        CALL UPDATE_HALO_ll(TZFIELDS_ll,IINFO_ll)
        CALL CLEANLIST_ll(TZFIELDS_ll)
        !
     ENDIF
     !
     IF (JL==2) THEN
        Z_NORM_TEMP1(:,:,:,1) = - GX_U_M(PPHI(:,:,:,2),XDXX,XDZZ,XDZX)
        Z_NORM_TEMP1(:,:,:,2) = - GY_V_M(PPHI(:,:,:,3),XDYY,XDZZ,XDZY)
        Z_NORM_TEMP1(:,:,:,3) = - GZ_W_M(PPHI(:,:,:,4),XDZZ)
        Z_NORM_TEMPO(:,:,:,1) = MXM(Z_NORM_TEMP1(:,:,:,1)) 
        Z_NORM_TEMPO(:,:,:,2) = MXM(Z_NORM_TEMP1(:,:,:,2))
        Z_NORM_TEMPO(:,:,:,3) = MXM(Z_NORM_TEMP1(:,:,:,3))
     ENDIF
     IF (JL==3) THEN
        Z_NORM_TEMP1(:,:,:,1) = - GX_U_M(PPHI(:,:,:,2),XDXX,XDZZ,XDZX)
        Z_NORM_TEMP1(:,:,:,2) = - GY_V_M(PPHI(:,:,:,3),XDYY,XDZZ,XDZY)
        Z_NORM_TEMP1(:,:,:,3) = - GZ_W_M(PPHI(:,:,:,4),XDZZ)
        Z_NORM_TEMPO(:,:,:,1) = MYM(Z_NORM_TEMP1(:,:,:,1))
        Z_NORM_TEMPO(:,:,:,2) = MYM(Z_NORM_TEMP1(:,:,:,2))
        Z_NORM_TEMPO(:,:,:,3) = MYM(Z_NORM_TEMP1(:,:,:,3))
     ENDIF
     IF (JL==4) THEN
        Z_NORM_TEMP1(:,:,:,1) = - GX_U_M(PPHI(:,:,:,2),XDXX,XDZZ,XDZX)
        Z_NORM_TEMP1(:,:,:,2) = - GY_V_M(PPHI(:,:,:,3),XDYY,XDZZ,XDZY)
        Z_NORM_TEMP1(:,:,:,3) = - GZ_W_M(PPHI(:,:,:,4),XDZZ)
        Z_NORM_TEMPO(:,:,:,1) = MZM(Z_NORM_TEMP1(:,:,:,1))
        Z_NORM_TEMPO(:,:,:,2) = MZM(Z_NORM_TEMP1(:,:,:,2))
        Z_NORM_TEMPO(:,:,:,3) = MZM(Z_NORM_TEMP1(:,:,:,3))           
     ENDIF
     !
     Z_NORM_TEMPO(:,:,1  ,1) = +Z_NORM_TEMPO(:,:,    2,1)
     Z_NORM_TEMPO(:,:,IKU,1) = +Z_NORM_TEMPO(:,:,IKU-1,1)
     Z_NORM_TEMPO(:,:,1  ,2) = +Z_NORM_TEMPO(:,:,    2,2)
     Z_NORM_TEMPO(:,:,IKU,2) = +Z_NORM_TEMPO(:,:,IKU-1,2)
     Z_NORM_TEMPO(:,:,1  ,3) = 2*Z_NORM_TEMPO(:,:,    2,3)-Z_NORM_TEMPO(:,:,    3,3)
     Z_NORM_TEMPO(:,:,IKU,3) = 2*Z_NORM_TEMPO(:,:,IKU-1,3)-Z_NORM_TEMPO(:,:,IKU-2,3)
     Z_NORM_TEMPO(:,:,1  ,3) = MAX(0.,Z_NORM_TEMPO(:,:,1  ,3))
     Z_NORM_TEMPO(:,:,2  ,3) = MAX(0.,Z_NORM_TEMPO(:,:,2  ,3))
     Z_NORM_TEMPO(:,:,3  ,3) = MAX(0.,Z_NORM_TEMPO(:,:,3  ,3))
     Z_NORM_TEMPO(:,:,IKU  ,3) = MIN(0.,Z_NORM_TEMPO(:,:,IKU  ,3))
     Z_NORM_TEMPO(:,:,IKU-1,3) = MIN(0.,Z_NORM_TEMPO(:,:,IKU-1,3))
     Z_NORM_TEMPO(:,:,IKU-2,3) = MIN(0.,Z_NORM_TEMPO(:,:,IKU-2,3))
     !
     IF (LWEST_ll ()) THEN
        DO JLL=1,3
           Z_NORM_TEMPO(JLL  ,:,1:IKM-1,1) = 0.
           Z_NORM_TEMPO(JLL  ,:,1:IKM-1,2) = 0.
           Z_NORM_TEMPO(JLL  ,:,1:IKM-1,3) =+1.
           Z_NORM_TEMPO(JLL  ,:,IKM:IKU,1) = 0.
           Z_NORM_TEMPO(JLL  ,:,IKM:IKU,2) = 0.
           Z_NORM_TEMPO(JLL  ,:,IKM:IKU,3) =-1.
        ENDDO
     ENDIF
     IF (LEAST_ll ()) THEN
        DO JLL=1,3
           Z_NORM_TEMPO(IIU-JLL+1,:,1:IKM-1,1) = 0.
           Z_NORM_TEMPO(IIU-JLL+1,:,1:IKM-1,2) = 0.
           Z_NORM_TEMPO(IIU-JLL+1,:,1:IKM-1,3) =+1.
           Z_NORM_TEMPO(IIU-JLL+1,:,IKM:IKU,1) = 0.
           Z_NORM_TEMPO(IIU-JLL+1,:,IKM:IKU,2) = 0.
           Z_NORM_TEMPO(IIU-JLL+1,:,IKM:IKU,3) =-1.
        ENDDO
     ENDIF
     IF (LSOUTH_ll()) THEN
        DO JLL=1,3
           Z_NORM_TEMPO(:,JLL,1:IKM-1,1) = 0.
           Z_NORM_TEMPO(:,JLL,1:IKM-1,2) = 0.
           Z_NORM_TEMPO(:,JLL,1:IKM-1,3) =+1.
           Z_NORM_TEMPO(:,JLL,IKM:IKU,1) = 0.
           Z_NORM_TEMPO(:,JLL,IKM:IKU,2) = 0.
           Z_NORM_TEMPO(:,JLL,IKM:IKU,3) =-1.
        ENDDO
     ENDIF
     IF (LNORTH_ll()) THEN
        DO JLL=1,3
           Z_NORM_TEMPO(:,IJU-JLL+1,1:IKM-1,1) = 0.
           Z_NORM_TEMPO(:,IJU-JLL+1,1:IKM-1,2) = 0.
           Z_NORM_TEMPO(:,IJU-JLL+1,1:IKM-1,3) =+1.
           Z_NORM_TEMPO(:,IJU-JLL+1,IKM:IKU,1) = 0.
           Z_NORM_TEMPO(:,IJU-JLL+1,IKM:IKU,2) = 0.
           Z_NORM_TEMPO(:,IJU-JLL+1,IKM:IKU,3) =-1.
        ENDDO
     ENDIF
     !
     NULLIFY(TZFIELDS_ll)
     CALL ADD3DFIELD_ll(TZFIELDS_ll,Z_NORM_TEMPO(:,:,:,1),'IBM_DETECT::Z_NORM_TEMPO')
     CALL ADD3DFIELD_ll(TZFIELDS_ll,Z_NORM_TEMPO(:,:,:,2),'IBM_DETECT::Z_NORM_TEMPO')
     CALL ADD3DFIELD_ll(TZFIELDS_ll,Z_NORM_TEMPO(:,:,:,3),'IBM_DETECT::Z_NORM_TEMPO')
     CALL UPDATE_HALO_ll(TZFIELDS_ll,IINFO_ll)
     CALL CLEANLIST_ll(TZFIELDS_ll)
     !
     Z_NORM_TEMP2(:,:,:)  = sqrt(Z_NORM_TEMPO(:,:,:,1)**2.+Z_NORM_TEMPO(:,:,:,2)**2.+Z_NORM_TEMPO(:,:,:,3)**2.)  
     !
     WHERE (abs(Z_NORM_TEMP2(:,:,:)) .gt. XIBM_EPSI)
        Z_NORM_GHOST(:,:,:,1) =  Z_NORM_TEMPO(:,:,:,1)/Z_NORM_TEMP2(:,:,:)
        Z_NORM_GHOST(:,:,:,2) =  Z_NORM_TEMPO(:,:,:,2)/Z_NORM_TEMP2(:,:,:)
        Z_NORM_GHOST(:,:,:,3) =  Z_NORM_TEMPO(:,:,:,3)/Z_NORM_TEMP2(:,:,:)
     ELSEWHERE
        Z_NORM_GHOST(:,:,:,1) = 0.
        Z_NORM_GHOST(:,:,:,2) = 0.
        Z_NORM_GHOST(:,:,:,3) = 1.
     ENDWHERE
     !
     WHERE (abs(Z_NORM_TEMP2(:,:,:)) .gt. XIBM_EPSI)
        Z_NORM_TEMPO(:,:,:,1) =  1./Z_NORM_TEMP2(:,:,:)
        Z_NORM_TEMPO(:,:,:,2) =  1./Z_NORM_TEMP2(:,:,:)
        Z_NORM_TEMPO(:,:,:,3) =  1./Z_NORM_TEMP2(:,:,:)
     ELSEWHERE
        Z_NORM_TEMPO(:,:,:,1) = 1.
        Z_NORM_TEMPO(:,:,:,2) = 1.
        Z_NORM_TEMPO(:,:,:,3) = 1.
     ENDWHERE
     !
     DO JMM = 1, I_NUMB_LAYER  
        !
        DO JM = 1, I_NUMB_GHOST(JL,JMM)
           !
           ! ghost index
           IF (JL==1) THEN
              I_INDE_TEMPO(:) = NIBM_GHOST_P(JM,JMM,JL  ,:)
           ELSE
              I_INDE_TEMPO(:) = NIBM_GHOST_V(JM,JMM,JL-1,:)
           ENDIF
           JI2 = I_INDE_TEMPO(1)
           JJ2 = I_INDE_TEMPO(2)
           JK2 = I_INDE_TEMPO(3)
           !
           ! ghost location
           Z_GHO(:,:) =  IBM_LOCATCORN(I_INDE_TEMPO,JL)
           ZLGHO = (abs(Z_GHO(1,1)-Z_GHO(8,1))* &
                abs(Z_GHO(1,2)-Z_GHO(8,2))* &
                abs(Z_GHO(1,3)-Z_GHO(8,3)))**(1./3.)
           ZVECT(1) = Z_GHO(1,1)  
           ZVECT(2) = Z_GHO(1,2) 
           ZVECT(3) = Z_GHO(1,3) 
           !
           PPHI_TEST = ABS(Z_NORM_GHOST(JI2,JJ2,JK2,1))+ABS(Z_NORM_GHOST(JI2,JJ2,JK2,2))+ABS(Z_NORM_GHOST(JI2,JJ2,JK2,3))
           PPHI_CORR = MAX(PPHI(JI2,JJ2,JK2,JL),(JMM*1.-1.)*ZLGHO*PPHI_TEST)
           PPHI_CORR = MIN(PPHI_CORR           ,(JMM*1.+0.)*ZLGHO*PPHI_TEST)
           !
           ! Storage of mirror/image1/image2/mirror locations
           IF (JL==1) THEN
              XIBM_IMAGE_P(JM,JMM,JL  ,1,:) = (1.0*ZLGHO+PPHI_CORR)*Z_NORM_GHOST(JI2,JJ2,JK2,:) +  ZVECT(:) 
              XIBM_IMAGE_P(JM,JMM,JL  ,2,:) = (2.0*ZLGHO+PPHI_CORR)*Z_NORM_GHOST(JI2,JJ2,JK2,:) +  ZVECT(:)
              XIBM_IMAGE_P(JM,JMM,JL  ,3,:) = (0.5*ZLGHO+PPHI_CORR)*Z_NORM_GHOST(JI2,JJ2,JK2,:) +  ZVECT(:)
              XIBM_GHOST_P(JM,JMM,JL    ,:) = ZVECT(:)
              XIBM_IMAGE_P(JM,JMM,JL  ,1,3) = MAX(XIBM_EPSI,XIBM_IMAGE_P(JM,JMM,JL  ,1,3))
              XIBM_IMAGE_P(JM,JMM,JL  ,2,3) = MAX(XIBM_EPSI,XIBM_IMAGE_P(JM,JMM,JL  ,2,3))
              XIBM_IMAGE_P(JM,JMM,JL  ,3,3) = MAX(XIBM_EPSI,XIBM_IMAGE_P(JM,JMM,JL  ,3,3))
              XIBM_GHOST_P(JM,JMM,JL    ,3) = MAX(XIBM_EPSI,XIBM_GHOST_P(JM,JMM,JL    ,3))
           ELSE
              XIBM_IMAGE_V(JM,JMM,JL-1,1,:) = (1.0*ZLGHO+PPHI_CORR)*Z_NORM_GHOST(JI2,JJ2,JK2,:) +  ZVECT(:)
              XIBM_IMAGE_V(JM,JMM,JL-1,2,:) = (2.0*ZLGHO+PPHI_CORR)*Z_NORM_GHOST(JI2,JJ2,JK2,:) +  ZVECT(:)
              XIBM_IMAGE_V(JM,JMM,JL-1,3,:) = (0.5*ZLGHO+PPHI_CORR)*Z_NORM_GHOST(JI2,JJ2,JK2,:) +  ZVECT(:)
              XIBM_GHOST_V(JM,JMM,JL-1  ,:) = ZVECT(:)
              XIBM_IMAGE_V(JM,JMM,JL-1,1,3) = MAX(XIBM_EPSI,XIBM_IMAGE_V(JM,JMM,JL-1,1,3))
              XIBM_IMAGE_V(JM,JMM,JL-1,2,3) = MAX(XIBM_EPSI,XIBM_IMAGE_V(JM,JMM,JL-1,2,3))
              XIBM_IMAGE_V(JM,JMM,JL-1,3,3) = MAX(XIBM_EPSI,XIBM_IMAGE_V(JM,JMM,JL-1,3,3))
              XIBM_GHOST_V(JM,JMM,JL-1  ,3) = MAX(XIBM_EPSI,XIBM_GHOST_V(JM,JMM,JL-1  ,3))
           ENDIF
           !
           ! iterative procedure to find image cell
           ZISI = 0.
           ZJSI = 0.
           ZKSI = 0.
           IF (abs(Z_NORM_GHOST(JI2,JJ2,JK2,1)).gt.XIBM_EPSI) THEN
              ZISI =Z_NORM_GHOST(JI2,JJ2,JK2,1)/abs(Z_NORM_GHOST(JI2,JJ2,JK2,1))
           ENDIF
           IF (abs(Z_NORM_GHOST(JI2,JJ2,JK2,2)).gt.XIBM_EPSI) THEN
              ZJSI =Z_NORM_GHOST(JI2,JJ2,JK2,2)/abs(Z_NORM_GHOST(JI2,JJ2,JK2,2))
           ENDIF
           IF (abs(Z_NORM_GHOST(JI2,JJ2,JK2,3)).gt.XIBM_EPSI) THEN
              ZKSI =Z_NORM_GHOST(JI2,JJ2,JK2,3)/abs(Z_NORM_GHOST(JI2,JJ2,JK2,3))
           ENDIF
           JIM1 = 3-2*JMM*int(min(0.,ZISI))
           JIP1 = 3+2*JMM*int(max(0.,ZISI))
           JJM1 = 3-2*JMM*int(min(0.,ZJSI))
           JJP1 = 3+2*JMM*int(max(0.,ZJSI))
           JKM1 = 3-2*JMM*int(min(0.,ZKSI))
           JKP1 = 3+2*JMM*int(max(0.,ZKSI))
           JIM2=max(1    ,JI2-JIM1)
           JIP2=min(IIU-1,JI2+JIP1)       
           JJM2=max(1    ,JJ2-JJM1)
           JJP2=min(IJU-1,JJ2+JJP1)           
           JKM2=max(1    ,JK2-JKM1)
           JKP2=min(IKU-1,JK2+JKP1)  
           !
           JN1 = 1
           JN2 = 1
           IF (JL/=1) THEN
              JN1 = 2 
              JN2 = 4
           ENDIF
           !
           DO JNN=1,3
              !
              ! image1/image2/mirror location
              IF (JL==1) THEN
                 ZVECT(:) = XIBM_IMAGE_P(JM,JMM,JL  ,JNN,:)
              ELSE
                 ZVECT(:) = XIBM_IMAGE_V(JM,JMM,JL-1,JNN,:)
              ENDIF
              !
              DO JN =JN1,JN2
                 !
                 ! search image depending on location type 
                 ZSEAR = 0.
                 DO JK= JKM2,JKP2
                    DO JJ= JJM2,JJP2
                       DO JI= JIM2,JIP2
                          !
                          ! nodes of the potential image cell
                          I_INDE_TEMPO(1)  = JI
                          I_INDE_TEMPO(2)  = JJ
                          I_INDE_TEMPO(3)  = JK
                          Z_IMG(:,:) = IBM_LOCATCORN(I_INDE_TEMPO,JN)
                          !
                          ! location of the potential cell
                          ZPROD(1)  = min(Z_IMG(1,1),Z_IMG(2,1),Z_IMG(3,1),Z_IMG(4,1),&
                               Z_IMG(5,1),Z_IMG(6,1),Z_IMG(7,1),Z_IMG(8,1))
                          ZPROD(2)  = max(Z_IMG(1,1),Z_IMG(2,1),Z_IMG(3,1),Z_IMG(4,1),&
                               Z_IMG(5,1),Z_IMG(6,1),Z_IMG(7,1),Z_IMG(8,1))    
                          ZPROD(3)  = min(Z_IMG(1,2),Z_IMG(2,2),Z_IMG(3,2),Z_IMG(4,2),&
                               Z_IMG(5,2),Z_IMG(6,2),Z_IMG(7,2),Z_IMG(8,2))
                          ZPROD(4)  = max(Z_IMG(1,2),Z_IMG(2,2),Z_IMG(3,2),Z_IMG(4,2),&
                               Z_IMG(5,2),Z_IMG(6,2),Z_IMG(7,2),Z_IMG(8,2))
                          ZPROD(5)  = min(Z_IMG(1,3),Z_IMG(2,3),Z_IMG(3,3),Z_IMG(4,3),&
                               Z_IMG(5,3),Z_IMG(6,3),Z_IMG(7,3),Z_IMG(8,3))
                          ZPROD(6)  = max(Z_IMG(1,3),Z_IMG(2,3),Z_IMG(3,3),Z_IMG(4,3),&
                               Z_IMG(5,3),Z_IMG(6,3),Z_IMG(7,3),Z_IMG(8,3))
                          !
                          IF (((ZVECT(1).gt.(ZPROD(1)-XIBM_EPSI)).and.(ZVECT(1).lt.(ZPROD(2)+XIBM_EPSI))).and.&
                               ((ZVECT(2).gt.(ZPROD(3)-XIBM_EPSI)).and.(ZVECT(2).lt.(ZPROD(4)+XIBM_EPSI))).and.&
                               ((ZVECT(3).gt.(ZPROD(5)-XIBM_EPSI)).and.(ZVECT(3).lt.(ZPROD(6)+XIBM_EPSI)))) THEN
                             !
                             JI3=JI
                             JJ3=JJ
                             JK3=JK
                             !
                             IF (JL==1) THEN
                                ZSEAR = 0.5
                                NIBM_IMAGE_P(JM,JMM,JL  ,JN  ,JNN,1) = JI3
                                NIBM_IMAGE_P(JM,JMM,JL  ,JN  ,JNN,2) = JJ3
                                NIBM_IMAGE_P(JM,JMM,JL  ,JN  ,JNN,3) = MAX(JK3,IKB)
                                I_INDE_TEMPO2(1)= JI3
                                I_INDE_TEMPO2(2)= JJ3
                                I_INDE_TEMPO2(3)= JK3
                                Z_PHI(:) = IBM_VALUECORN(PPHI(:,:,:,JN),I_INDE_TEMPO2)
                                IF (JMM==1) ZIBM_TESTI = 0.
                                IF (JMM/=1) ZIBM_TESTI = 1.
                                DO JP=1,8
                                   IF (Z_PHI(JP).gt.-XIBM_EPSI) THEN
                                      XIBM_TESTI_P(JM,JMM,JL  ,JN  ,JNN,JP)=0.
                                   ELSE
                                      XIBM_TESTI_P(JM,JMM,JL  ,JN  ,JNN,JP)=1.
                                   ENDIF
                                   ZIBM_TESTI = ZIBM_TESTI+XIBM_TESTI_P(JM,JMM,JL  ,JN,JNN,JP)
                                ENDDO
                                IF (ZIBM_TESTI.gt.+XIBM_EPSI) THEN
                                   IF (LIBM_TROUBLE) XIBM_SUTR(JI2,JJ2,JK2,JL)=0.
                                   IF ((JI2>=IIB.AND.JI2<=IIE).AND.(JI3<=IIB).AND.(JMM==1)) THEN
                                      JHALO = MAX(JHALO,ABS(JI3-IIB))
                                   ENDIF
                                   IF ((JI2>=IIB.AND.JI2<=IIE).AND.(JI3>=IIE).AND.(JMM==1)) THEN
                                      JHALO = MAX(JHALO,ABS(JI3-IIE))
                                   ENDIF
                                   IF ((JJ2>=IJB.AND.JJ2<=IJE).AND.(JJ3<=IJB).AND.(JMM==1)) THEN
                                      JHALO = MAX(JHALO,ABS(JJ3-IJB))
                                   ENDIF
                                   IF ((JJ2>=IJB.AND.JJ2<=IJE).AND.(JJ3>=IJE).AND.(JMM==1)) THEN
                                      JHALO = MAX(JHALO,ABS(JJ3-IJE))
                                   ENDIF
                                   ZSEAR = 1.
                                ENDIF
                                GO TO 666
                             ELSE
                                ZSEAR = 0.5
                                NIBM_IMAGE_V(JM,JMM,JL-1,JN-1,JNN,1) = JI3
                                NIBM_IMAGE_V(JM,JMM,JL-1,JN-1,JNN,2) = JJ3
                                NIBM_IMAGE_V(JM,JMM,JL-1,JN-1,JNN,3) = MAX(JK3,IKB)
                                I_INDE_TEMPO2(1)= JI3
                                I_INDE_TEMPO2(2)= JJ3
                                I_INDE_TEMPO2(3)= JK3
                                Z_PHI(:) = IBM_VALUECORN(PPHI(:,:,:,JN),I_INDE_TEMPO2)
                                IF (JMM==1) ZIBM_TESTI = 0.
                                IF (JMM/=1) ZIBM_TESTI = 1.
                                DO JP=1,8
                                   IF (Z_PHI(JP).gt.-XIBM_EPSI) THEN
                                      XIBM_TESTI_V(JM,JMM,JL-1,JN-1,JNN,JP)=0. 
                                   ELSE
                                      XIBM_TESTI_V(JM,JMM,JL-1,JN-1,JNN,JP)=1.
                                   ENDIF
                                   ZIBM_TESTI = ZIBM_TESTI+XIBM_TESTI_V(JM,JMM,JL-1,JN-1,JNN,JP)
                                ENDDO
                                IF (ZIBM_TESTI.gt.+XIBM_EPSI) THEN
                                   IF (LIBM_TROUBLE) XIBM_SUTR(JI2,JJ2,JK2,JL)=0.
                                   IF ((JI2>=IIB.AND.JI2<=IIE).AND.(JI3<=IIB).AND.(JMM==1)) THEN
                                      JHALO = MAX(JHALO,ABS(JI3-IIB))
                                   ENDIF
                                   IF ((JI2>=IIB.AND.JI2<=IIE).AND.(JI3>=IIE).AND.(JMM==1)) THEN
                                      JHALO = MAX(JHALO,ABS(JI3-IIE))
                                   ENDIF
                                   IF ((JJ2>=IJB.AND.JJ2<=IJE).AND.(JJ3<=IJB).AND.(JMM==1)) THEN
                                      JHALO = MAX(JHALO,ABS(JJ3-IJB))
                                   ENDIF
                                   IF ((JJ2>=IJB.AND.JJ2<=IJE).AND.(JJ3>=IJE).AND.(JMM==1)) THEN
                                      JHALO = MAX(JHALO,ABS(JJ3-IJE))
                                   ENDIF
                                   ZSEAR = 1.
                                ENDIF
                                GO TO 666
                             ENDIF
                          ENDIF
                       ENDDO
                    ENDDO
                 ENDDO
                 !
666              CONTINUE
                 !
                 IF ((ZSEAR.gt.0.25).AND.(ZSEAR.lt.0.75).AND.(JMM==1)) THEN
                    ZPHI(JI2,JJ2,JK2,JL)=1.
                    IF (JL==1) THEN
                       WRITE(*,*)'===== IBM WARNING NEW ======'
                       WRITE(*,*)'Non detected PPP images cell'
                       WRITE(*,*)'ghost',JI2,JJ2,JK2
                       WRITE(*,*)'ghost typ',JL,JMM
                       WRITE(*,*)'levelset',PPHI(JI2,JJ2,JK2,JL)
                       WRITE(*,*)XIBM_GHOST_P(JM,JMM,JL,1),XIBM_GHOST_P(JM,JMM,JL,2),XIBM_GHOST_P(JM,JMM,JL,3)
                       WRITE(*,*)Z_NORM_GHOST(JI2,JJ2,JK2,1),Z_NORM_GHOST(JI2,JJ2,JK2,2),Z_NORM_GHOST(JI2,JJ2,JK2,3) 
                       WRITE(*,*)Z_NORM_TEMPO(JI2,JJ2,JK2,1),Z_NORM_TEMPO(JI2,JJ2,JK2,2),Z_NORM_TEMPO(JI2,JJ2,JK2,3) 
                       WRITE(*,*)'image loc:',JN
                       WRITE(*,*)'image typ:',JNN
                       WRITE(*,*)XIBM_IMAGE_P(JM,JMM,JL,JNN,1),XIBM_IMAGE_P(JM,JMM,JL,JNN,2),XIBM_IMAGE_P(JM,JMM,JL,JNN,3)
                    ELSE
                       WRITE(*,*)'===== IBM WARNING NEW ======'
                       WRITE(*,*)'Non detected UVW images cell'
                       WRITE(*,*)'ghost:',JI2,JJ2,JK2
                       WRITE(*,*)'ghost typ',JL,JMM
                       WRITE(*,*)'levelset',PPHI(JI2,JJ2,JK2,JL)
                       WRITE(*,*)XIBM_GHOST_V(JM,JMM,JL-1,1),XIBM_GHOST_V(JM,JMM,JL-1,2),XIBM_GHOST_V(JM,JMM,JL-1,3)
                       WRITE(*,*)Z_NORM_GHOST(JI2,JJ2,JK2,1),Z_NORM_GHOST(JI2,JJ2,JK2,2),Z_NORM_GHOST(JI2,JJ2,JK2,3) 
                       WRITE(*,*)Z_NORM_TEMPO(JI2,JJ2,JK2,1),Z_NORM_TEMPO(JI2,JJ2,JK2,2),Z_NORM_TEMPO(JI2,JJ2,JK2,3)
                       WRITE(*,*)'image loc:',JN
                       WRITE(*,*)'image typ:',JNN
                       WRITE(*,*)XIBM_IMAGE_V(JM,JMM,JL-1,JNN,1),XIBM_IMAGE_V(JM,JMM,JL-1,JNN,2),XIBM_IMAGE_V(JM,JMM,JL-1,JNN,3)
                    ENDIF
                    I_INDE_TEMPO(1)  = JIM2
                    I_INDE_TEMPO(2)  = JJM2
                    I_INDE_TEMPO(3)  = JKM2
                    Z_IMG(:,:) = IBM_LOCATCORN(I_INDE_TEMPO,JN)
                    WRITE(*,*)'LOC MIN',Z_IMG(1,1),Z_IMG(1,2),Z_IMG(1,3) 
                    I_INDE_TEMPO(1)  = JIP2
                    I_INDE_TEMPO(2)  = JJP2
                    I_INDE_TEMPO(3)  = JKP2
                    Z_IMG(:,:) = IBM_LOCATCORN(I_INDE_TEMPO,JN)
                    WRITE(*,*)'LOC MAX',Z_IMG(8,1),Z_IMG(8,2),Z_IMG(8,3) 
                 ENDIF
                 !
                 IF ((ZSEAR.lt.0.25).AND.(JMM==1)) THEN
                    ZPHI(JI2,JJ2,JK2,JL)=1.
                    IF (JL==1) THEN
                       WRITE(*,*)'===== IBM WARNING ======'
                       WRITE(*,*)'Non detected PPP images cell'
                       WRITE(*,*)'ghost',JI2,JJ2,JK2
                       WRITE(*,*)'ghost typ',JL,JMM
                       WRITE(*,*)'levelset',PPHI(JI2,JJ2,JK2,JL)
                       WRITE(*,*)XIBM_GHOST_P(JM,JMM,JL,1),XIBM_GHOST_P(JM,JMM,JL,2),XIBM_GHOST_P(JM,JMM,JL,3)
                       WRITE(*,*)Z_NORM_GHOST(JI2,JJ2,JK2,1),Z_NORM_GHOST(JI2,JJ2,JK2,2),Z_NORM_GHOST(JI2,JJ2,JK2,3) 
                       WRITE(*,*)Z_NORM_TEMPO(JI2,JJ2,JK2,1),Z_NORM_TEMPO(JI2,JJ2,JK2,2),Z_NORM_TEMPO(JI2,JJ2,JK2,3) 
                       WRITE(*,*)'image loc:',JN
                       WRITE(*,*)'image typ:',JNN
                       WRITE(*,*)XIBM_IMAGE_P(JM,JMM,JL,JNN,1),XIBM_IMAGE_P(JM,JMM,JL,JNN,2),XIBM_IMAGE_P(JM,JMM,JL,JNN,3)
                    ELSE
                       WRITE(*,*)'===== IBM WARNING ======'
                       WRITE(*,*)'Non detected UVW images cell'
                       WRITE(*,*)'ghost:',JI2,JJ2,JK2
                       WRITE(*,*)'ghost typ',JL,JMM
                       WRITE(*,*)'levelset',PPHI(JI2,JJ2,JK2,JL)
                       WRITE(*,*)XIBM_GHOST_V(JM,JMM,JL-1,1),XIBM_GHOST_V(JM,JMM,JL-1,2),XIBM_GHOST_V(JM,JMM,JL-1,3)
                       WRITE(*,*)Z_NORM_GHOST(JI2,JJ2,JK2,1),Z_NORM_GHOST(JI2,JJ2,JK2,2),Z_NORM_GHOST(JI2,JJ2,JK2,3) 
                       WRITE(*,*)Z_NORM_TEMPO(JI2,JJ2,JK2,1),Z_NORM_TEMPO(JI2,JJ2,JK2,2),Z_NORM_TEMPO(JI2,JJ2,JK2,3)
                       WRITE(*,*)'image loc:',JN
                       WRITE(*,*)'image typ:',JNN
                       WRITE(*,*)XIBM_IMAGE_V(JM,JMM,JL-1,JNN,1),XIBM_IMAGE_V(JM,JMM,JL-1,JNN,2),XIBM_IMAGE_V(JM,JMM,JL-1,JNN,3)
                    ENDIF
                    I_INDE_TEMPO(1)  = JIM2
                    I_INDE_TEMPO(2)  = JJM2
                    I_INDE_TEMPO(3)  = JKM2
                    Z_IMG(:,:) = IBM_LOCATCORN(I_INDE_TEMPO,JN)
                    WRITE(*,*)'LOC MIN',Z_IMG(1,1),Z_IMG(1,2),Z_IMG(1,3) 
                    I_INDE_TEMPO(1)  = JIP2
                    I_INDE_TEMPO(2)  = JJP2
                    I_INDE_TEMPO(3)  = JKP2
                    Z_IMG(:,:) = IBM_LOCATCORN(I_INDE_TEMPO,JN)
                    WRITE(*,*)'LOC MAX',Z_IMG(8,1),Z_IMG(8,2),Z_IMG(8,3) 
                 ENDIF
              ENDDO
           ENDDO
        ENDDO
     ENDDO
     !
667  CONTINUE
     !
     IF ((NHALO<=JHALO).AND.(JMM==1)) WRITE(*,*)'### WARNING HALO ###',JHALO,IP
     !
  ENDDO
  WRITE(*,*)'### HALO ###',NHALO,JHALO
  !        
  !**** X. DEALLOCATIONS/CLOSES
  !     -----------------------
  !
  DEALLOCATE(I_INDE_TEMPO,I_INDE_TEMPO2,I_NUMB_GHOST)
  DEALLOCATE(Z_NORM_GHOST,Z_NORM_TEMPO,Z_NORM_TEMP1,Z_NORM_TEMP2,Z_NORM_TEMP3)
  DEALLOCATE(ZVECT,ZPROD,ZPHI)
  DEALLOCATE(Z_PHI,Z_IMG,Z_GHO)
  ! 
  RETURN
  !
END SUBROUTINE IBM_DETECT
