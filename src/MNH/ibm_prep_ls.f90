!MNH_LIC Copyright 2021-2021 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
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
  !****  Types of topography:
  !          1)GENE : generalized obstacles (x,y coordinates)
  !          => read the informations (triangles constituting the
  !          faces of obstacles) from an .obj file.
  !          The .obj file must have a particular organization:
  !              a) A line with 'usemtl' indicates the 2 materials 
  !          of each side of the interface. Only the faces with 
  !          their external face in contact with the outside air
  !          are read (mat2=air)
  !              b) A line starting with 'v' indicates the location
  !          (x,y,z coordinates) of a face vortex.
  !              c) A line starting with 'f' indicates the vortices
  !          constituting the face.
  !                   usemtl mat1:mat2
  !                   v      xv1      yv1      zv1
  !                   v      xv2      yv2      zv2
  !                   v      xv3      yv3      zv3
  !                   v      xv4      yv4      zv4
  !                   f         1         2         3
  !                   f         1         3         4
  !          2)IDEA : idealized obstacles (x,y coordinates)
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
  !    For the generalized case, the method is based on '3D Distance from a
  !    Point to a Triangle' a technical report from Mark W. Jones, University 
  !    of Wales Swansea [Jones (1995)].
  !
  !    AUTHORS
  !    ------
  !      Franck Auguste (CERFACS-AE), Tim Nagel (Météo-France)
  !
  !    MODIFICATIONS
  !    -------------
  !      Original         01/06/2021
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
  USE MODI_IBM_GENERLS
  USE MODI_IBM_IDEALRP
  USE MODI_IBM_IDEALEE
  !
  USE MODD_GRID
  USE MODD_CST
  USE MODD_GRID_n
  USE MODE_GRIDPROJ
  USE MODE_MSG
  !
  IMPLICIT NONE
  !
  !------------------------------------------------------------------------------
  !
  !       0.1  declarations of arguments
  !
  LOGICAL                  ,INTENT(IN)    :: OIBM                        ! flag for immersed boundary method
  CHARACTER(LEN=4)         ,INTENT(IN)    :: HIBM_TYPE                   ! switch generalized/idealized object
  REAL, DIMENSION(:,:,:,:) ,INTENT(INOUT) :: PPHI                        ! LS functions
  !
  !------------------------------------------------------------------------------
  !
  !       0.2  declaration of local variables
  !
  INTEGER :: JN,JM,JNM,JL,JMM,JI,JJ,JK,JF,JV                             ! loop index
  INTEGER :: IIU,IJU
  REAL    :: ZX_MIN,ZX_MAX,ZY_MIN,ZY_MAX,DX_LOW,DY_LOW,DX_HIGH,DY_HIGH
  INTEGER :: ILUIBMIDEA,IRESPIBMGENE,ILUIBMGENE,IRESPIBMIDEA             ! integers for open/read files
  INTEGER :: IIBM_NUMB_NODE_SURF                                         ! number of surface points (generalized case) 
  INTEGER :: IIBM_NUMB_TYPE_SURF                                         ! number of surface type   (idealized case) 
  INTEGER :: IIBM_TYPE_SURF                                              ! type of surfaces            
  INTEGER :: IIBM_NUMB_SURF                                              ! number of surfaces in each type 
  REAL    :: ZIBM_X1,ZIBM_X2,ZIBM_Y1,ZIBM_Y2,ZIBM_Z1,ZIBM_Z2             ! location of surface points for one object
  REAL    :: ZIBM_TYPE_SURF
  REAL, DIMENSION(:,:), ALLOCATABLE :: ZIBM_XYZ1,ZIBM_XYZ2               ! location of surface points for all object
  REAL, DIMENSION(:,:), ALLOCATABLE :: ZV1,ZV1_2,ZV2,ZV2_2,ZV3,ZV3_2     ! face vectors
  REAL, DIMENSION(:,:), ALLOCATABLE :: NORM_FACES,NORM_FACES2                 ! norm of the faces
  REAL, DIMENSION(:,:,:), ALLOCATABLE :: ZIBM_FACES,ZIBM_FACES2,ZIBM_FACES2b  ! extremities of triangle faces for all object
  TYPE(LIST_ll), POINTER :: TZFIELDS_ll
  INTEGER                :: IINFO_ll
  CHARACTER(LEN=12)      :: HFILEGENE, HFILEIDEA
  CHARACTER(LEN=100)     :: YSTRING,YSTRING2
  INTEGER                :: NS1,NS2,NS3,NS4,NS5,NS6
  INTEGER                :: ZN1,ZN2,ZN3,JCOUNT
  REAL, DIMENSION(3)     :: ZNA,ZNB
  !
  !------------------------------------------------------------------------------
  !
  !       0.3  Allocation
  ILUIBMGENE = 42
  ILUIBMIDEA = 43
  HFILEGENE = "ibm_gene.obj"
  HFILEIDEA = "ibm_idea.nam"
  !
  IIU = SIZE(XXHAT)
  IJU = SIZE(XYHAT)
  !
  DX_LOW = XXHAT(2)-XXHAT(1)
  DX_HIGH = XXHAT(IIU)-XXHAT(IIU-1)
  DY_LOW = XYHAT(2)-XYHAT(1)
  DY_HIGH = XYHAT(IJU)-XYHAT(IJU-1)
  !
  !Collect the face up to 10 gridsize out of the current processor
  ZX_MIN = XXHAT(1)-10.*DX_LOW
  ZX_MAX = XXHAT(IIU)+(11.)*DX_HIGH
  ZY_MIN = XYHAT(1)-10.*DY_LOW
  ZY_MAX = XYHAT(IJU)+(11.)*DY_HIGH
  !
  !------------------------------------------------------------------------------
  !
  !* *** 1. PRELIMINARIES
  !     ----------------
  !
  !  Read input files in order to compute interface location
  !    - 'ibm_gene.obj' for generalized case 
  !       => read the informations (triangles constituting the
  !       faces of obstacles) from an .obj file
  !    - 'm_ideal.nam' for idealized case
  !       (NUMB_NODE_SURF is the number of objects) 
  !       (NUMB_TYPE_SURF is the number of surface types:  
  !       (     TYPE_SURF = 1 for parallelepipedic shape
  !             TYPE_SURF = 2      for ellipsoidal shape)
  !       (     NUMB_SURF is the objects number in each type)
  !
  !
  !--------------------------------
  !--------Generalized case--------
  !--------------------------------
  IF (HIBM_TYPE=='GENE') THEN
     !
     !Allocate the tables containing the vortices, the faces locations,
     !the norms, which are needed to calculate the LSF
     ALLOCATE(ZIBM_XYZ1(5400000,3))
     ALLOCATE(ZIBM_FACES(3150000,3,3))
     ALLOCATE(ZIBM_FACES2b(3150000,3,3))
     ALLOCATE(NORM_FACES(3150000,3))
     ALLOCATE(ZV1(3150000,3))
     ALLOCATE(ZV2(3150000,3))
     ALLOCATE(ZV3(3150000,3))
     !
     OPEN(ILUIBMGENE , FILE= HFILEGENE , IOSTAT=IRESPIBMGENE , STATUS='OLD')
     !
     JV=1
     JF=0
     JCOUNT=0
     !
     !Only the faces that are in contact with the air (external faces of
     !the obstacles) are read.
     DO
        IF (IRESPIBMGENE/=0) EXIT
        READ(UNIT=ILUIBMGENE,FMT='(A100)',IOSTAT=IRESPIBMGENE) YSTRING
        NS1=LEN(TRIM(YSTRING))
        IF (TRIM(YSTRING(1:7))=='usemtl') THEN
           IF (TRIM(YSTRING(NS1-3:NS1))==':air') THEN
              JN=1
           ELSE
              JN=0
           ENDIF
        ENDIF
        IF (TRIM(YSTRING(1:2))=='v') THEN
           NS2=INDEX(TRIM(YSTRING)," ",back=.true.) 
           NS3=LEN(TRIM(YSTRING(:NS2)))
           NS4=INDEX(TRIM(YSTRING(:NS3))," ",back=.true.)
           NS5=LEN(TRIM(YSTRING(:NS4)))
           NS6=INDEX(TRIM(YSTRING(:NS5))," ",back=.true.)
           READ(YSTRING(NS6:NS5) , *) ZIBM_XYZ1(JV,1)
           READ(YSTRING(NS4:NS3) , *) ZIBM_XYZ1(JV,2)
           READ(YSTRING(NS2:NS1) , *) ZIBM_XYZ1(JV,3)
           !FIXME temporary spatial modification
           ZIBM_XYZ1(JV,1) = ZIBM_XYZ1(JV,1) +200.
           ZIBM_XYZ1(JV,2) = ZIBM_XYZ1(JV,2) +200.
           JV=JV+1
        ENDIF
        IF (JN==1.AND.TRIM(YSTRING(1:2))=='f') THEN
           NS2=INDEX(TRIM(YSTRING)," ",back=.true.)
           NS3=LEN(TRIM(YSTRING(:NS2)))
           NS4=INDEX(TRIM(YSTRING(:NS3))," ",back=.true.)
           NS5=LEN(TRIM(YSTRING(:NS4)))
           NS6=INDEX(TRIM(YSTRING(:NS5))," ",back=.true.)
           READ(YSTRING(NS6:NS5) , *) ZN1
           READ(YSTRING(NS4:NS3) , *) ZN2
           READ(YSTRING(NS2:NS1) , *) ZN3
           ! If the face extremities are far outside of the processor they are not read
           IF (ZIBM_XYZ1(ZN1,1)<ZX_MIN.AND.ZIBM_XYZ1(ZN2,1)<ZX_MIN.AND.ZIBM_XYZ1(ZN3,1)<ZX_MIN) CYCLE
           IF (ZIBM_XYZ1(ZN1,1)>ZX_MAX.AND.ZIBM_XYZ1(ZN2,1)>ZX_MAX.AND.ZIBM_XYZ1(ZN3,1)>ZX_MAX) CYCLE
           IF (ZIBM_XYZ1(ZN1,2)<ZY_MIN.AND.ZIBM_XYZ1(ZN2,2)<ZY_MIN.AND.ZIBM_XYZ1(ZN3,2)<ZY_MIN) CYCLE
           IF (ZIBM_XYZ1(ZN1,2)>ZY_MAX.AND.ZIBM_XYZ1(ZN2,2)>ZY_MAX.AND.ZIBM_XYZ1(ZN3,2)>ZY_MAX) CYCLE
           JF=JF+1
           !
           ZIBM_FACES(JF,1,1) = ZIBM_XYZ1(ZN1,1)
           ZIBM_FACES(JF,1,2) = ZIBM_XYZ1(ZN1,2)
           ZIBM_FACES(JF,1,3) = ZIBM_XYZ1(ZN1,3)
           ZIBM_FACES(JF,2,1) = ZIBM_XYZ1(ZN2,1)
           ZIBM_FACES(JF,2,2) = ZIBM_XYZ1(ZN2,2)
           ZIBM_FACES(JF,2,3) = ZIBM_XYZ1(ZN2,3)
           ZIBM_FACES(JF,3,1) = ZIBM_XYZ1(ZN3,1)
           ZIBM_FACES(JF,3,2) = ZIBM_XYZ1(ZN3,2)
           ZIBM_FACES(JF,3,3) = ZIBM_XYZ1(ZN3,3)
           !
           ZNA(1) = ZIBM_FACES(JF,1,1)-ZIBM_FACES(JF,2,1)
           ZNA(2) = ZIBM_FACES(JF,1,2)-ZIBM_FACES(JF,2,2)
           ZNA(3) = ZIBM_FACES(JF,1,3)-ZIBM_FACES(JF,2,3)
           ZNB(1) = ZIBM_FACES(JF,1,1)-ZIBM_FACES(JF,3,1)
           ZNB(2) = ZIBM_FACES(JF,1,2)-ZIBM_FACES(JF,3,2)
           ZNB(3) = ZIBM_FACES(JF,1,3)-ZIBM_FACES(JF,3,3)
           !Elimination of '1D' faces
           IF (ZNA(1)==0..AND.ZNA(2)==0..AND.ZNA(3)==0.) CYCLE
           IF (ZNB(1)==0..AND.ZNB(2)==0..AND.ZNB(3)==0.) CYCLE
           IF (ZNA(2)==0..AND.ZNA(3)==0..AND.ZNB(2)==0..AND.ZNB(3)==0.) CYCLE
           IF (ZNA(1)==ZNB(1).AND.ZNA(2)==ZNB(2).AND.ZNA(3)==ZNB(3)) CYCLE
           JCOUNT=JCOUNT+1
           NORM_FACES(JCOUNT,:)= CROSSPRODUCT(ZNA,ZNB)
           ZIBM_FACES2b(JCOUNT,:,:)=ZIBM_FACES(JF,:,:)
           !
           !Equation (6) of Jones (1995)
           ZV1(JCOUNT,1) = (ZIBM_FACES(JF,1,1)-ZIBM_FACES(JF,2,1))/ SQRT((ZIBM_FACES(JF,1,1)-ZIBM_FACES(JF,2,1))**2 + &
                (ZIBM_FACES(JF,1,2)-ZIBM_FACES(JF,2,2))**2 +(ZIBM_FACES(JF,1,3)-ZIBM_FACES(JF,2,3))**2)+ &
                (ZIBM_FACES(JF,1,1)-ZIBM_FACES(JF,3,1))/ SQRT((ZIBM_FACES(JF,1,1)-ZIBM_FACES(JF,3,1))**2 + &
                (ZIBM_FACES(JF,1,2)-ZIBM_FACES(JF,3,2))**2 +(ZIBM_FACES(JF,1,3)-ZIBM_FACES(JF,3,3))**2)
           !
           ZV1(JCOUNT,2) = (ZIBM_FACES(JF,1,2)-ZIBM_FACES(JF,2,2))/ SQRT((ZIBM_FACES(JF,1,1)-ZIBM_FACES(JF,2,1))**2 + &
                (ZIBM_FACES(JF,1,2)-ZIBM_FACES(JF,2,2))**2 +(ZIBM_FACES(JF,1,3)-ZIBM_FACES(JF,2,3))**2)+ &
                (ZIBM_FACES(JF,1,2)-ZIBM_FACES(JF,3,2))/ SQRT((ZIBM_FACES(JF,1,1)-ZIBM_FACES(JF,3,1))**2 + &
                (ZIBM_FACES(JF,1,2)-ZIBM_FACES(JF,3,2))**2 +(ZIBM_FACES(JF,1,3)-ZIBM_FACES(JF,3,3))**2)
           !
           ZV1(JCOUNT,3) = (ZIBM_FACES(JF,1,3)-ZIBM_FACES(JF,2,3))/ SQRT((ZIBM_FACES(JF,1,1)-ZIBM_FACES(JF,2,1))**2 + &
                (ZIBM_FACES(JF,1,2)-ZIBM_FACES(JF,2,2))**2 +(ZIBM_FACES(JF,1,3)-ZIBM_FACES(JF,2,3))**2)+ &
                (ZIBM_FACES(JF,1,3)-ZIBM_FACES(JF,3,3))/ SQRT((ZIBM_FACES(JF,1,1)-ZIBM_FACES(JF,3,1))**2 + &
                (ZIBM_FACES(JF,1,2)-ZIBM_FACES(JF,3,2))**2 +(ZIBM_FACES(JF,1,3)-ZIBM_FACES(JF,3,3))**2) 
           !
           ZV2(JCOUNT,1) = (ZIBM_FACES(JF,2,1)-ZIBM_FACES(JF,3,1))/ SQRT((ZIBM_FACES(JF,2,1)-ZIBM_FACES(JF,3,1))**2 + &
                (ZIBM_FACES(JF,2,2)-ZIBM_FACES(JF,3,2))**2 +(ZIBM_FACES(JF,2,3)-ZIBM_FACES(JF,3,3))**2)+ &
                (ZIBM_FACES(JF,2,1)-ZIBM_FACES(JF,1,1))/ SQRT((ZIBM_FACES(JF,2,1)-ZIBM_FACES(JF,1,1))**2 + &
                (ZIBM_FACES(JF,2,2)-ZIBM_FACES(JF,1,2))**2 +(ZIBM_FACES(JF,2,3)-ZIBM_FACES(JF,1,3))**2)
           !
           ZV2(JCOUNT,2) = (ZIBM_FACES(JF,2,2)-ZIBM_FACES(JF,3,2))/ SQRT((ZIBM_FACES(JF,2,1)-ZIBM_FACES(JF,3,1))**2 + &
                (ZIBM_FACES(JF,2,2)-ZIBM_FACES(JF,3,2))**2 +(ZIBM_FACES(JF,2,3)-ZIBM_FACES(JF,3,3))**2)+ &
                (ZIBM_FACES(JF,2,2)-ZIBM_FACES(JF,1,2))/ SQRT((ZIBM_FACES(JF,2,1)-ZIBM_FACES(JF,1,1))**2 + &
                (ZIBM_FACES(JF,2,2)-ZIBM_FACES(JF,1,2))**2 +(ZIBM_FACES(JF,2,3)-ZIBM_FACES(JF,1,3))**2)
           !
           ZV2(JCOUNT,3) = (ZIBM_FACES(JF,2,3)-ZIBM_FACES(JF,3,3))/ SQRT((ZIBM_FACES(JF,2,1)-ZIBM_FACES(JF,3,1))**2 + &
                (ZIBM_FACES(JF,2,2)-ZIBM_FACES(JF,3,2))**2 +(ZIBM_FACES(JF,2,3)-ZIBM_FACES(JF,3,3))**2)+ &
                (ZIBM_FACES(JF,2,3)-ZIBM_FACES(JF,1,3))/ SQRT((ZIBM_FACES(JF,2,1)-ZIBM_FACES(JF,1,1))**2 + &
                (ZIBM_FACES(JF,2,2)-ZIBM_FACES(JF,1,2))**2 +(ZIBM_FACES(JF,2,3)-ZIBM_FACES(JF,1,3))**2)
           !
           ZV3(JCOUNT,1) = (ZIBM_FACES(JF,3,1)-ZIBM_FACES(JF,1,1))/ SQRT((ZIBM_FACES(JF,3,1)-ZIBM_FACES(JF,1,1))**2 + &
                (ZIBM_FACES(JF,3,2)-ZIBM_FACES(JF,1,2))**2 +(ZIBM_FACES(JF,3,3)-ZIBM_FACES(JF,1,3))**2)+ &
                (ZIBM_FACES(JF,3,1)-ZIBM_FACES(JF,2,1))/ SQRT((ZIBM_FACES(JF,3,1)-ZIBM_FACES(JF,2,1))**2 + &
                (ZIBM_FACES(JF,3,2)-ZIBM_FACES(JF,2,2))**2 +(ZIBM_FACES(JF,3,3)-ZIBM_FACES(JF,2,3))**2)
           !
           ZV3(JCOUNT,2) = (ZIBM_FACES(JF,3,2)-ZIBM_FACES(JF,1,2))/ SQRT((ZIBM_FACES(JF,3,1)-ZIBM_FACES(JF,1,1))**2 + &
                (ZIBM_FACES(JF,3,2)-ZIBM_FACES(JF,1,2))**2 +(ZIBM_FACES(JF,3,3)-ZIBM_FACES(JF,1,3))**2)+ &
                (ZIBM_FACES(JF,3,2)-ZIBM_FACES(JF,2,2))/ SQRT((ZIBM_FACES(JF,3,1)-ZIBM_FACES(JF,2,1))**2 + &
                (ZIBM_FACES(JF,3,2)-ZIBM_FACES(JF,2,2))**2 +(ZIBM_FACES(JF,3,3)-ZIBM_FACES(JF,2,3))**2)
           !
           ZV3(JCOUNT,3) = (ZIBM_FACES(JF,3,3)-ZIBM_FACES(JF,1,3))/ SQRT((ZIBM_FACES(JF,3,1)-ZIBM_FACES(JF,1,1))**2 + &
                (ZIBM_FACES(JF,3,2)-ZIBM_FACES(JF,1,2))**2 +(ZIBM_FACES(JF,3,3)-ZIBM_FACES(JF,1,3))**2)+ &
                (ZIBM_FACES(JF,3,3)-ZIBM_FACES(JF,2,3))/ SQRT((ZIBM_FACES(JF,3,1)-ZIBM_FACES(JF,2,1))**2 + &
                (ZIBM_FACES(JF,3,2)-ZIBM_FACES(JF,2,2))**2 +(ZIBM_FACES(JF,3,3)-ZIBM_FACES(JF,2,3))**2)
           !
        ENDIF
        !
        IF (JN==1.AND.TRIM(YSTRING(1:2))=='vn') THEN
           call Print_msg( NVERB_FATAL, 'GEN', 'IBM_PREP_LS', 'Unable to read vn found in .obj' )
        ENDIF
        !
        IF (JN==1.AND.TRIM(YSTRING(1:2))=='vt') THEN
           call Print_msg( NVERB_FATAL, 'GEN', 'IBM_PREP_LS', 'Unable to read vt found in .obj' )
        ENDIF
        !
     END DO
     !
     ALLOCATE(ZIBM_FACES2(JCOUNT,3,3))
     ALLOCATE(NORM_FACES2(JCOUNT,3))
     ALLOCATE(ZV1_2(JCOUNT,3))
     ALLOCATE(ZV2_2(JCOUNT,3))
     ALLOCATE(ZV3_2(JCOUNT,3))
     !
     NORM_FACES2 = NORM_FACES(:JCOUNT,:)
     ZV1_2 = ZV1(:JCOUNT,:)
     ZV2_2 = ZV2(:JCOUNT,:)
     ZV3_2 = ZV3(:JCOUNT,:)
     ZIBM_FACES2 = ZIBM_FACES2b(:JCOUNT,:,:)
     !
  ENDIF
  !
  !----------------------------
  !---------Idealized case-----
  !----------------------------
  IF (HIBM_TYPE=='IDEA') THEN
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
  !        generalized shape => construction of LS function (z<z_interface <=> phi>0)
  !                             the method is based on '3D Distance from a Point to a Triangle' [Jones (1995)].
  !                          => conversion of LS function to VF function (Sussman, JCP (1994)
  !        idealized shape   => construction of VF/LS function using analytical
  !                            locations of interface (ellipsoidal/parallelepipedic shapes)
  !
  IF (HIBM_TYPE=='GENE') THEN
     CALL IBM_GENERLS(ZIBM_FACES2,NORM_FACES2,ZV1_2,ZV2_2,ZV3_2,ZX_MIN,ZY_MIN,ZX_MAX,ZY_MAX,PPHI)
  ENDIF
  !
  IF (HIBM_TYPE=='IDEA') then
     DO JN=1,JNM
        IF (abs(ZIBM_XYZ2(JN,7)-1.).lt.XIBM_EPSI) CALL IBM_IDEALRP(JN,ZIBM_XYZ2,PPHI)
        IF (abs(ZIBM_XYZ2(JN,7)-2.).lt.XIBM_EPSI) CALL IBM_IDEALEE(JN,ZIBM_XYZ2,PPHI)
     ENDDO
  ENDIF
  !
CONTAINS
  !
  FUNCTION CROSSPRODUCT(PA,PB) RESULT(CROSS)
    !
    REAL, DIMENSION(3)             :: CROSS
    REAL                           :: VAL
    REAL, DIMENSION(3), INTENT(IN) :: PA, PB
    !
    CROSS(1) = PA(2) * PB(3) - PA(3) * PB(2)
    CROSS(2) = PA(3) * PB(1) - PA(1) * PB(3)
    CROSS(3) = PA(1) * PB(2) - PA(2) * PB(1)
    !
    VAL = (CROSS(1)**2+CROSS(2)**2+CROSS(3)**2)**(0.5)
    !
    CROSS(1) = CROSS(1)/VAL
    CROSS(2) = CROSS(2)/VAL
    CROSS(3) = CROSS(3)/VAL
    !
  END FUNCTION CROSSPRODUCT
  !
END SUBROUTINE IBM_PREP_LS
