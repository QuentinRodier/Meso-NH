!MNH_LIC Copyright 1994-2018 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!
!    #######################
MODULE MODI_IBM_PREP_LS  
  !    ####################### 
  !
  INTERFACE
     !
     SUBROUTINE IBM_PREP_LS(OIBM,HIBM_TYPE,PPHI)
       !
       LOGICAL                  , INTENT(IN)    :: OIBM
       CHARACTER(LEN=4)         , INTENT(IN)    :: HIBM_TYPE  
       REAL, DIMENSION(:,:,:,:) , INTENT(INOUT) :: PPHI    
       !
     END SUBROUTINE IBM_PREP_LS
     !
  END INTERFACE
  !
END MODULE MODI_IBM_PREP_LS
!
!    ###########################################
SUBROUTINE IBM_PREP_LS(OIBM,HIBM_TYPE,PPHI)
  !    ###########################################
  !
  !
  !****  IBM_PREP_LS computes the LS level set function        
  !                
  !    PURPOSE
  !    -------
  !****  The purpose of this routine is to localize fluid-solid interface
  !      for the immersed boundary method in the help of LS function. 
  !      This functions allow the access to interface characteristics 
  !      (normal vector, curvature,...)
  !
  !    METHOD
  !    ------
  !****  Three main steps
  !      - read input ASCII files
  !      - Types of topography:
  !          IDEA : idealized obstacles (x,y coordinates)
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
  USE MODD_VAR_ll, ONLY: IP
  USE MODD_CONF, ONLY: NHALO
  !
  ! declaration
  USE MODD_IBM_PARAM_n 
  USE MODD_IBM_LSF       
  USE MODD_DIM_n, ONLY: NIMAX,NJMAX,NKMAX    
  USE MODD_PARAMETERS, ONLY: JPVEXT,JPHEXT   
  USE MODD_GRID_n, ONLY: XXHAT,XYHAT,XZZ 
  USE MODD_METRICS_n, ONLY: XDXX,XDYY,XDZZ,XDZX,XDZY 
  USE MODD_LBC_n
  USE MODD_ARGSLIST_ll, ONLY : LIST_ll
  !
  ! interface
  USE MODI_SHUMAN
  USE MODI_GDIV
  USE MODI_IBM_IDEALRP
  USE MODI_IBM_IDEALEE
  !
  USE MODD_GRID
  USE MODD_CST
  USE MODD_GRID_n
  USE MODE_GRIDPROJ
  !
  IMPLICIT NONE
  !
  !------------------------------------------------------------------------------
  !
  !       0.1  declarations of arguments
  !
  LOGICAL                  ,INTENT(IN)    :: OIBM      ! flag for immersed boundary method
  CHARACTER(LEN=4)         ,INTENT(IN)    :: HIBM_TYPE ! switch generalized/idealised object
  REAL, DIMENSION(:,:,:,:) ,INTENT(INOUT) :: PPHI      ! LS functions
  !
  !------------------------------------------------------------------------------
  !
  !       0.2  declaration of local variables
  !
  INTEGER :: JN,JM,JNM,JL,JMM,JI,JJ,JK,JF,JV                           ! loop index
  INTEGER :: IIU,IJU,KII,KJJ
  REAL    :: ZX_MIN,ZX_MAX,ZY_MIN,ZY_MAX,DX_LOW,DY_LOW,DX_HIGH,DY_HIGH
  INTEGER :: JI2,JJ2,JK2,JI3,JJ3,JK3
  INTEGER :: JIM1,JIP1,JIM2,JIP2,JIM4,JIP4
  INTEGER :: JJM1,JJP1,JJM2,JJP2,JJM4,JJP4
  INTEGER :: JI2_MIN,JI2_MAX,JJ2_MIN,JJ2_MAX
  INTEGER :: IIB,IIE,IJB,IJE,IKB,IKE,ILOOP,JLOOP,KLOOP
  INTEGER :: IGRIB,IIBM_LEVEL,KIBM_LEVEL,IIBM_MIDDLE,KIBM_LEVEL2
  INTEGER :: KIII,KJJJ,KIIM1,KIIP1,KJJM1,KJJP1
  INTEGER :: ILUIBMIDEA,IRESPIBMGENE,ILUIBMGENE,IRESPIBMIDEA ! integers for open/read files
  INTEGER :: IIBM_NUMB_NODE_SURF ! number of surface points (generalized case) 
  INTEGER :: IIBM_NUMB_TYPE_SURF ! number of surface type   (idealized case) 
  INTEGER :: IIBM_TYPE_SURF      ! type of surfaces            
  INTEGER :: IIBM_NUMB_SURF      ! number of surfaces in each type 
  REAL    :: ZIBM_X1,ZIBM_X2,ZIBM_Y1,ZIBM_Y2,ZIBM_Z1,ZIBM_Z2 ! location of surface points for one object
  REAL    :: ZIBM_TYPE_SURF
  REAL, DIMENSION(:,:), ALLOCATABLE :: ZIBM_XYZ1,ZIBM_XYZ2  ! location of surface points for all object
  REAL, DIMENSION(:,:), ALLOCATABLE :: ZV1,ZV1_2,ZV2,ZV2_2,ZV3,ZV3_2
  REAL, DIMENSION(:,:), ALLOCATABLE :: NORM_FACES,NORM_FACES2
  REAL, DIMENSION(:,:,:), ALLOCATABLE :: ZIBM_FACES,ZIBM_FACES2,ZIBM_FACES2b  ! extremities of triangle faces for all object
  REAL                                  :: XXX,YYY,ZZZ
  INTEGER                               :: IRESPIBMREAL,ILUIBMREAL ! reading/writing ASCII files
  REAL, DIMENSION(:,:,:)  , ALLOCATABLE :: ZSURF,ZINDI,ZTMP2,ZTMP3 ! SSF and ISF functions + temporary arrays
  REAL, DIMENSION(:,:,:,:), ALLOCATABLE :: ZTMP,ZPHI               ! temporary arrays
  REAL                   :: ZLAT,ZLON,ZHEI,ZIBM_HEI                ! lat/lon/z coordinates
  INTEGER                :: KNUM,KBOR,KIBM_BATMIN,KIBM_BATMAX      ! index of buildings (BATI, REAL)
  CHARACTER(LEN=10)      :: YHCOUNT1                               ! reading/writing ASCII files
  CHARACTER(LEN=24)      :: YHCOUNT2
  LOGICAL                :: GHCOUNT3
  REAL                   :: ZXX,ZYY,ZXM2,ZYM2,ZII,ZJJ              ! temporary values 
  REAL                   :: ZMAX1,ZMAX2,ZMAX3
  REAL                   :: IIMAX,IJMAX,IKMAX
  REAL                   :: ZXX1,ZYY1,ZZZ1,ZXX2,ZYY2,ZZZ2
  REAL                   :: ZTES1,ZTES2,ZTES3,ZTES4,ZDIS,ZHIGH,ZHORI
  REAL                   :: ZLATMIN,ZLATMAX,ZLONMIN,ZLONMAX,ZXM2MIN,ZXM2MAX, ZYM2MIN, ZYM2MAX
  REAL                   :: SIGN1,SIGN2,SIGN3,SIGN4,ZHEI2
  REAL                   :: ZX1,ZY1,ZX2,ZY2,ZIND
  TYPE(LIST_ll), POINTER :: TZFIELDS_ll
  INTEGER                :: IINFO_ll
  LOGICAL                :: LCAEP,LAZF
  CHARACTER(LEN=12)      :: HFILEGENE, HFILEIDEA
  CHARACTER(LEN=100)     :: YSTRING,YSTRING2
  INTEGER                :: NS1,NS2,NS3,NS4,NS5,NS6
  INTEGER                :: ZN1,ZN2,ZN3,JCOUNT
  REAL, DIMENSION(3)     :: ZNA,ZNB
  !
  !------------------------------------------------------------------------------
  !
  !       0.3  Allocation
  ILUIBMIDEA = 43
  HFILEIDEA = "ibm_idea.nam"
  !
  IIU = SIZE(XXHAT)
  IJU = SIZE(XYHAT)
  !
  !
  !------------------------------------------------------------------------------
  !
  !* *** 1. PRELIMINARIES
  !     ----------------
  !
  !  Read input files in order to compute interface location
  !    - 'm_ideal.nam' for idealized case
  !       (NUMB_NODE_SURF is the number of objects) 
  !       (NUMB_TYPE_SURF is the number of surface types:  
  !       (     TYPE_SURF = 1 for parallelepipedic shape
  !             TYPE_SURF = 2      for ellipsoidal shape)
  !       (     NUMB_SURF is the objects number in each type)
  !
  !
  IF ((HIBM_TYPE=='IDEA')) THEN
     !
     OPEN(ILUIBMIDEA , FILE= HFILEIDEA , IOSTAT=IRESPIBMIDEA , FORM='FORMATTED' , &
          STATUS='OLD', ACCESS='SEQUENTIAL', ACTION='READ')
     !
     READ(UNIT=ILUIBMIDEA,FMT=*) IIBM_NUMB_NODE_SURF, IIBM_NUMB_TYPE_SURF 
     ALLOCATE(ZIBM_XYZ2(IIBM_NUMB_NODE_SURF,7))
     !
     ZIBM_XYZ2(:,:) = 0.
     JNM = 0
     DO JN=1,IIBM_NUMB_TYPE_SURF
     !
        READ(UNIT=ILUIBMIDEA,FMT=*) IIBM_TYPE_SURF, IIBM_NUMB_SURF
        ZIBM_TYPE_SURF= float(IIBM_TYPE_SURF)
        !
        DO JM=1,IIBM_NUMB_SURF
           !
           READ(UNIT=ILUIBMIDEA,FMT=*) ZIBM_X1,ZIBM_X2,ZIBM_Y1,ZIBM_Y2,ZIBM_Z1,ZIBM_Z2
           !
           JNM = JNM + 1
           ZIBM_XYZ2(JNM,1) = ZIBM_X1          !x_mini(pp) or x_cent(ee)
           ZIBM_XYZ2(JNM,2) = ZIBM_X2          !x_maxi(pp) or x_delt(ee)
           ZIBM_XYZ2(JNM,3) = ZIBM_Y1          !y_mini(pp) or y_cent(ee)
           ZIBM_XYZ2(JNM,4) = ZIBM_Y2          !y_maxi(pp) or y_delt(ee)
           ZIBM_XYZ2(JNM,5) = ZIBM_Z1          !z_mini(pp) or z_cent(ee)
           ZIBM_XYZ2(JNM,6) = ZIBM_Z2          !z_maxi(pp) or z_delt(ee)
           ZIBM_XYZ2(JNM,7) = ZIBM_TYPE_SURF   !surface type (1=pp/2=ee)
           !
        ENDDO
        !
     ENDDO
  ENDIF
  !
  !**** 2. EXECUTIONS
  !     -------------
  ! 
  ! Computations of volumic fraction (VF) and Level Set function (LS) for all kinds of initialization
  !        idealized shape  => construction of VF/LS function using analytical
  !                            locations of interface (ellipsoidal/parallelepipedic shapes)
  !
  IF ((HIBM_TYPE=='IDEA')) then
     DO JN=1,JNM
        !
        IF (abs(ZIBM_XYZ2(JN,7)-1.).lt.XIBM_EPSI) CALL IBM_IDEALRP(JN,ZIBM_XYZ2,PPHI)
        IF (abs(ZIBM_XYZ2(JN,7)-2.).lt.XIBM_EPSI) CALL IBM_IDEALEE(JN,ZIBM_XYZ2,PPHI)
     ENDDO
     !
  ENDIF
  !
END SUBROUTINE IBM_PREP_LS
