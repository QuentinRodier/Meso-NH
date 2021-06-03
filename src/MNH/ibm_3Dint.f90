!MNH_LIC Copyright 1994-2018 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!
!     #####################
MODULE MODI_IBM_3DINT
  !     #####################
  !
  INTERFACE 
     !
     FUNCTION IBM_3DINT(KTYPUVW,PVALUEI,PLOCATI,PTESTG0,PLOCAT1,PVALUE1,PLOCAT2,HINTERP,PRADIUS,PPOWERS) RESULT(PVALUE2)
       !
       INTEGER                             :: KTYPUVW
       REAL,   DIMENSION(:)   , INTENT(IN) :: PVALUEI
       REAL,   DIMENSION(:)   , INTENT(IN) :: PLOCATI
       REAL,   DIMENSION(:)   , INTENT(IN) :: PTESTG0
       REAL,   DIMENSION(:,:) , INTENT(IN) :: PLOCAT1
       REAL,   DIMENSION(:)   , INTENT(IN) :: PVALUE1
       REAL,   DIMENSION(:)   , INTENT(IN) :: PLOCAT2
       REAL                                :: PVALUE2               
       CHARACTER(LEN=3)       , INTENT(IN) :: HINTERP
       REAL                   , INTENT(IN) :: PRADIUS 
       REAL                   , INTENT(IN) :: PPOWERS
       !
     END FUNCTION IBM_3DINT
     !
  END INTERFACE
  !
END MODULE MODI_IBM_3DINT
!
!     ###################################################################################################################
FUNCTION IBM_3DINT(KTYPUVW,PVALUEI,PLOCATI,PTESTG0,PLOCAT1,PVALUE1,PLOCAT2,HINTERP,PRADIUS,PPOWERS) RESULT(PVALUE2)
  !     ###################################################################################################################
  !
  !****  ===IBM_INTER_IDW=== inverse distance weighting interpolation
  !
  !    PURPOSE
  !    -------
  !     This function interpolates the 3D fields from the initial grid
  !     to the image point associated to each ghost nodes. The interpolation
  !     weighting is based on the inverse of the (square of) the interpolation distance
  !     between the image point F(X,Y,Z) and each N selected nearest nodes F(Xi,Yi,Zi).
  !
  !    METHOD
  !    ------
  !
  !       F(X,Y,Z)= sum(i=1toN)[|1/Di|F(Xi,Yi,Zi)] / sum(i=1toN)[|1/Di|]
  !       Di as a power of the distance interpolation
  !
  !****  ===IBM_INTER_MDW=== modified inverse distance weighting interpolation
  !
  !    PURPOSE
  !    -------
  !     This function interpolates the 3D fields from the initial grid
  !     to the image point associated to each ghost nodes. The interpolation
  !     weighting is based on the Franke formulation (2004) between the image point 
  !     F(X,Y,Z) and each N selected nearest nodes F(Xi,Yi,Zi). The number of
  !     nodes is depending on the interpolation order. 
  !
  !    METHOD
  !    ------
  !
  !          F(X,Y,Z)= sum(i=1toN)[|1/Di|F(Xi,Yi,Zi)] / sum(i=1toN)[|1/Di|]
  !          Di according to :
  !                             "Scattered Data: tests of some methods."
  !                              Franke R., Mathematics of computation, 2004
  !
  !****  ===IBM_INTER_CLI=== classical Lagrange interpolation
  !
  !    PURPOSE
  !    -------
  !     This function interpolates the 3D fields from the initial grid
  !     to the image point associated to each ghost nodes. The interpolation
  !     weighting is based on the trilinear interpolation via Lagrange polynomials 
  !
  !    METHOD
  !    ------
  !
  !        F(X,Y,Z)= sum(i=1toN)[|Li|F(Xi,Yi,Zi)]
  !        Li = prod[(x-xj)(xi-xj)] (xi/=xj)
  !
  !     INDEX DEFINITION 
  !     ----------------
  !          1 <-> i  ,j  ,k
  !          2 <-> i+1,j  ,k
  !          3 <-> i  ,j+1,k
  !          4 <-> i+1,j+1,k
  !          5 <-> i  ,j  ,k+1
  !          6 <-> i+1,j  ,k+1
  !          7 <-> i  ,j+1,k+1
  !          8 <-> i+1,j+1,k+1
  ! 
  !    EXTERNAL
  !    --------
  !      NONE
  !
  !    IMPLICIT ARGUMENTS
  !    ------------------
  !
  !    REFERENCE
  !    ---------
  !
  !    AUTHOR
  !    ------
  !	
  !      Franck Auguste       * CERFACS(AE) *
  !
  !    MODIFICATIONS
  !    -------------
  !      Original    01/01/2019
  !
  !-------------------------------------------------------------------------------
  !
  !**** 0.    DECLARATIONS
  !     ------------------
  !
  ! module
  !
  ! declaration
  USE MODD_IBM_PARAM_n
  !
  ! interface
  !
  IMPLICIT NONE
  !
  !------------------------------------------------------------------------------
  !
  !       0.1   Declaration of arguments
  INTEGER                             :: KTYPUVW
  REAL,   DIMENSION(:)   , INTENT(IN) :: PVALUEI
  REAL,   DIMENSION(:)   , INTENT(IN) :: PLOCATI
  REAL,   DIMENSION(:)   , INTENT(IN) :: PTESTG0
  REAL,   DIMENSION(:,:) , INTENT(IN) :: PLOCAT1
  REAL,   DIMENSION(:)   , INTENT(IN) :: PVALUE1
  REAL,   DIMENSION(:)   , INTENT(IN) :: PLOCAT2
  REAL                                :: PVALUE2
  CHARACTER(LEN=3)       , INTENT(IN) :: HINTERP
  REAL                   , INTENT(IN) :: PRADIUS
  REAL                   , INTENT(IN) :: PPOWERS
  !
  !------------------------------------------------------------------------------
  !
  !       0.2   Declaration of local variables
  !
  INTEGER                         :: JM,JN,JMM             ! loop index
  REAL, DIMENSION(:), ALLOCATABLE :: Z_WEIGHT0                      ! interpolation weighting array
  REAL                            :: Z_WEIGHT1,Z_WEIGHT2,Z_WEIGHT3  ! interpolation weighting scalar        
  REAL                            :: Z_LENGHTX,Z_LENGHTY,Z_LENGHTZ  ! interpolation distance
  REAL                            :: Z_LENGHTM,Z_VOLUME,Z_VALUE3    ! interpolation module
  REAL                            :: Z_ORDINT, Z_TESTSB,Z_VALUE2    ! interpolation radius
  CHARACTER(LEN=3)                :: Y_INTERP,Y_INTERP2 
  !
  !-------------------------------------------------------------------------------
  !
  !**** 1. PRELIMINARIES
  !     ----------------
  !
  ALLOCATE(Z_WEIGHT0(10))
  Z_WEIGHT0(:) = 0.
  Z_WEIGHT1 = 0.
  Z_WEIGHT2 = 0.
  Z_WEIGHT3 = 0.
  Z_VOLUME  = 0.
  Z_VALUE2 = 0.
  Z_VALUE3 = 0.
  JN = 0
  !
  !------------------------------------------------------------------------------
  !       
  !**** 2. EXECUTIONS
  !     -------------
  !
  !
  ! Switch interface distance dependence
  !
  Z_TESTSB = 1.
  DO JN=1,8 
     Z_TESTSB = min(Z_TESTSB,PTESTG0(JN))
  ENDDO
  !
  Y_INTERP = HINTERP
  Y_INTERP2 = 'CLI'
  IF (HINTERP=='LAI') THEN
     IF (Z_TESTSB.lt.+XIBM_EPSI) THEN 
        Y_INTERP = 'IDW'
     ELSE
        Y_INTERP = 'CLI' 
     ENDIF
  ENDIF
  IF (HINTERP=='LAM') THEN
     IF (Z_TESTSB.lt.+XIBM_EPSI) THEN 
        Y_INTERP = 'MDW'
     ELSE
        Y_INTERP = 'CLI' 
     ENDIF
  ENDIF
  !
  ! === Trilinear Lagrange interpolation ===
  !
  IF (Y_INTERP=='CLI') THEN
     !
     DO JM=1,8
        JN=8-JM+1
        IF ((ABS((PLOCAT1(JM,1)-PLOCAT1(JN,1))).GT.XIBM_EPSI).AND.&
             (ABS((PLOCAT1(JM,2)-PLOCAT1(JN,2))).GT.XIBM_EPSI).AND.&
             (ABS((PLOCAT1(JM,3)-PLOCAT1(JN,3))).GT.XIBM_EPSI)) THEN
           !
           Z_WEIGHT0(JM)=(PLOCAT2(1)-PLOCAT1(JN,1))/(PLOCAT1(JM,1)-PLOCAT1(JN,1))*&
                (PLOCAT2(2)-PLOCAT1(JN,2))/(PLOCAT1(JM,2)-PLOCAT1(JN,2))*&
                (PLOCAT2(3)-PLOCAT1(JN,3))/(PLOCAT1(JM,3)-PLOCAT1(JN,3))
           !
        ELSE
           !
           Z_VALUE3 = 1.
           Z_WEIGHT0(JM) = +XIBM_EPSI
           !
        ENDIF
     ENDDO
     !
     IF (Z_VALUE3<XIBM_EPSI) THEN
        !
        DO JM=1,8
           Z_VALUE2 = Z_VALUE2 + Z_WEIGHT0(JM)
        ENDDO
        IF (ABS(Z_VALUE2-1.)>0.1) THEN
           Z_WEIGHT0(:) = 1./8.
        ENDIF
        !
        PVALUE2 = 0.
        DO JM=1,8
           PVALUE2 = PVALUE2 + PVALUE1(JM)*Z_WEIGHT0(JM)
        ENDDO
        !
     ELSE
        !
        Y_INTERP2 = 'IDW'
        PVALUE2 = 0.
        !
     ENDIF
     !
  ENDIF
  !
  IF (Y_INTERP2 == 'IDW') Y_INTERP = 'IDW'
  !
  ! === Inverse distance weighting interpolation (Modified or classical) ===
  ! 
  IF (Y_INTERP=='IDW'.or.Y_INTERP=='MDW') THEN
     !
     Z_VOLUME = ABS(PLOCAT1(1,1)-PLOCAT1(8,1))*&
          ABS(PLOCAT1(1,2)-PLOCAT1(8,2))*&
          ABS(PLOCAT1(1,3)-PLOCAT1(8,3))
     !
     JMM = 8
     DO JM=1,JMM 
        !
        IF (JM<=8) THEN
           Z_LENGHTX = (PLOCAT2(1)-PLOCAT1(JM,1))
           Z_LENGHTY = (PLOCAT2(2)-PLOCAT1(JM,2))
           Z_LENGHTZ = (PLOCAT2(3)-PLOCAT1(JM,3))
        ELSE
           Z_LENGHTX = (PLOCAT2(1)-PLOCATI(1))
           Z_LENGHTY = (PLOCAT2(2)-PLOCATI(2))
           Z_LENGHTZ = (PLOCAT2(3)-PLOCATI(3))
        ENDIF
        Z_LENGHTM = (Z_LENGHTX**2.+Z_LENGHTY**2.+Z_LENGHTZ**2.)**0.5
        !
        Z_LENGHTM = MAX(Z_LENGHTM,0.0001*Z_VOLUME**(1./3.))      
        IF ((Z_LENGHTM.lt.(0.01*Z_VOLUME**(1./3.))).AND.(PTESTG0(JM).GT.0.5)) THEN
           Z_WEIGHT1 = 2.*XIBM_IEPS
           Z_WEIGHT3 = 1.
           JN=JM
        ELSE
           Z_WEIGHT1 = 0.
           IF (Z_LENGHTM.lt.PRADIUS*Z_VOLUME**(1./3.)) THEN
              !
              IF (JM<=8.and.Y_INTERP=='IDW') Z_WEIGHT1 = PTESTG0(JM)*(1./Z_LENGHTM)**PPOWERS
              IF (JM==9.and.Y_INTERP=='IDW') Z_WEIGHT1 =             (1./Z_LENGHTM)**PPOWERS
              IF (JM<=8.and.Y_INTERP=='MDW') Z_WEIGHT1 = PTESTG0(JM)*((PRADIUS*Z_VOLUME**(1./3.)-Z_LENGHTM)/&
                   (PRADIUS*Z_VOLUME**(1./3.)*Z_LENGHTM))**PPOWERS
              IF (JM==9.and.Y_INTERP=='MDW') Z_WEIGHT1 =             ((PRADIUS*Z_VOLUME**(1./3.)-Z_LENGHTM)/&
                   (PRADIUS*Z_VOLUME**(1./3.)*Z_LENGHTM))**PPOWERS            
           ENDIF
        ENDIF
        !
        Z_WEIGHT2 = Z_WEIGHT2+Z_WEIGHT1
        Z_WEIGHT0(JM)=Z_WEIGHT1
        !
     ENDDO
     !
     Z_WEIGHT0(10)=Z_WEIGHT2
     !
     IF (Z_WEIGHT3.gt.XIBM_EPSI) THEN
        Z_WEIGHT0(:)=0.
        Z_WEIGHT0(JN)=1.
        Z_WEIGHT0(10)=1.
     ENDIF
     !
     IF (ABS(Z_WEIGHT0(10)).GT.XIBM_EPSI) THEN
        !
        PVALUE2 = 0.
        DO JM=1,8
           PVALUE2 = PVALUE2 + PVALUE1(JM)*Z_WEIGHT0(JM)/Z_WEIGHT0(10)
        ENDDO
        !
     ELSE
        !
        PVALUE2 = 0.
        DO JM=1,8
           PVALUE2 = PVALUE2 + PVALUE1(JM)*(1./8.)
        ENDDO
        !
     ENDIF
     !
  ENDIF
  !
  DEALLOCATE(Z_WEIGHT0)
  !
  RETURN
  !
END FUNCTION IBM_3DINT
