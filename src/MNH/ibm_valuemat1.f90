!MNH_LIC Copyright 1994-2018 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!
!     #########################
MODULE MODI_IBM_VALUEMAT1
  !   #########################
  !
  INTERFACE 
     !
     FUNCTION  IBM_VALUEMAT1(PLOCATG,PLOCATI,PVELOCI,HINTERP) RESULT(PMATRIX)
       !
       REAL,   DIMENSION(:)   , INTENT(IN) :: PLOCATG
       REAL,   DIMENSION(:)   , INTENT(IN) :: PLOCATI
       REAL,   DIMENSION(:,:) , INTENT(IN) :: PVELOCI
       CHARACTER(LEN=3)       , INTENT(IN) :: HINTERP
       REAL,   DIMENSION(3,3)              :: PMATRIX
       !
     END FUNCTION IBM_VALUEMAT1
     !
  END INTERFACE
  !
END MODULE MODI_IBM_VALUEMAT1
!
!     #######################################################################
FUNCTION IBM_VALUEMAT1(PLOCATG,PLOCATI,PVELOCI,HINTERP) RESULT(PMATRIX)
  !     #######################################################################
  !
  !****  *IBM_INTER_VALUEMAT1* - Change of basis (u,v,w) to (n,t,c)
  !
  !    PURPOSE
  !    -------
  !     This function calculates the vector normal to the interface, the 
  !     tangent and binormal vectors in order to project the basis
  !     (u,v,w) to (n,t,c). The projection is stored in the PMATRIX matrix. 
  !
  !
  !    METHOD
  !    ------
  !
  !        HINTERP can be defined as HIBM_TYPE_BOUND in regard of the tangent vector:
  !                                  HIBM_TYPE_BOUND="CST" (Image 1 direction)
  !                                  HIBM_TYPE_BOUND="LIN" (linear evolution)
  !                                  HIBM_TYPE_BOUND="LOG" (logarithmic evol)
  !     
  !    INDEX
  !    -----
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
  !
  ! interface
  USE MODD_IBM_PARAM_n
  !
  IMPLICIT NONE
  !
  !-----------------------------------------------------------------------------
  !
  !       0.1   Declaration of arguments
  REAL,   DIMENSION(:)   , INTENT(IN)  :: PLOCATG
  REAL,   DIMENSION(:)   , INTENT(IN)  :: PLOCATI
  REAL,   DIMENSION(:,:) , INTENT(IN)  :: PVELOCI
  CHARACTER(LEN=3)       , INTENT(IN)  :: HINTERP
  REAL,   DIMENSION(3,3)               :: PMATRIX
  !
  !-----------------------------------------------------------------------------
  !
  !       0.2   Declaration of local variables
  !
  INTEGER                              :: JI,JJ,JK,JL,JH,JLL,JL1,JL2
  REAL,   DIMENSION(:,:) , ALLOCATABLE :: Z_IMAGE_VECT
  REAL,   DIMENSION(:,:) , ALLOCATABLE :: Z_IMAGE_TEMP
  REAL,   DIMENSION(:)   , ALLOCATABLE :: Z_NORMA_VECT
  REAL,   DIMENSION(:)   , ALLOCATABLE :: Z_TANGE_VECT
  REAL,   DIMENSION(:)   , ALLOCATABLE :: Z_BINOR_VECT
  REAL                                 :: Z_NORMA_TEMP,Z_PRODV_TEMP
  REAL                                 :: Z_COEFI1,Z_COEFI2
  !
  !-----------------------------------------------------------------------------
  !
  !**** 1. PRELIMINARIES
  !     ----------------
  !
  ALLOCATE(Z_IMAGE_VECT(4,3),Z_IMAGE_TEMP(4,3))
  ALLOCATE(Z_NORMA_VECT(3),Z_TANGE_VECT(3),Z_BINOR_VECT(3))
  !
  Z_IMAGE_VECT(:,:) = 0.
  Z_IMAGE_TEMP(:,:) = 0.
  Z_NORMA_VECT(:)   = 0.
  Z_TANGE_VECT(:)   = 0.
  Z_BINOR_VECT(:)   = 0.
  !------------------------------------------------------------------------------
  !       
  !**** 2. EXECUTIONS
  !     -------------
  !
  ! vec(n) 
  Z_NORMA_VECT(:) = PLOCATI(:)-PLOCATG(:)
  Z_NORMA_TEMP    = sqrt(Z_NORMA_VECT(1)**2.+Z_NORMA_VECT(2)**2.+Z_NORMA_VECT(3)**2.)+XIBM_EPSI
  Z_NORMA_VECT(:) = Z_NORMA_VECT(:) / Z_NORMA_TEMP
  !
  ! vec(v)
  DO JL=1,2
     IF (JL==1) JL1=0
     IF (JL==2) JL2=0
     Z_IMAGE_TEMP(JL,1)    = sqrt(PVELOCI(JL,1)**2.+PVELOCI(JL,2)**2.+PVELOCI(JL,3)**2.)
     Z_PRODV_TEMP = ABS((PVELOCI(JL,2)*Z_NORMA_VECT(3)-PVELOCI(JL,3)*Z_NORMA_VECT(2))+ &
          (PVELOCI(JL,3)*Z_NORMA_VECT(1)-PVELOCI(JL,1)*Z_NORMA_VECT(3))+ &
          (PVELOCI(JL,1)*Z_NORMA_VECT(2)-PVELOCI(JL,2)*Z_NORMA_VECT(1))+XIBM_EPSI)
     IF (Z_IMAGE_TEMP(JL,1).gt.XIBM_EPSI.and.Z_PRODV_TEMP.gt.XIBM_EPSI) THEN
        Z_IMAGE_VECT(JL,:) = PVELOCI(JL,:)/Z_IMAGE_TEMP(JL,1)
     ELSE 
        IF (JL==1) JL1=1
        IF (JL==2) JL2=1
        Z_NORMA_TEMP = XIBM_IEPS
        DO JLL=1,3
           IF (abs(Z_NORMA_VECT(JLL)).lt.Z_NORMA_TEMP) THEN
              Z_NORMA_TEMP = abs(Z_NORMA_VECT(JLL))
              JH = JLL          
           ENDIF
        ENDDO
        Z_IMAGE_VECT(JL,:) = 0.
        Z_IMAGE_VECT(JL,JH) = 1.   
     ENDIF
  ENDDO
  !
  IF (JL1==1.AND.JL2==0) Z_IMAGE_VECT(1,:)=Z_IMAGE_VECT(2,:)
  IF (JL2==1.AND.JL1==0) Z_IMAGE_VECT(2,:)=Z_IMAGE_VECT(1,:)
  !
  ! vec(c)
  DO JL=1,2
     !
     ! vec(c)
     Z_IMAGE_TEMP(JL,1) = -(Z_IMAGE_VECT(JL,2)*Z_NORMA_VECT(3)-Z_IMAGE_VECT(JL,3)*Z_NORMA_VECT(2))
     Z_IMAGE_TEMP(JL,2) = +(Z_IMAGE_VECT(JL,1)*Z_NORMA_VECT(3)-Z_IMAGE_VECT(JL,3)*Z_NORMA_VECT(1))
     Z_IMAGE_TEMP(JL,3) = -(Z_IMAGE_VECT(JL,1)*Z_NORMA_VECT(2)-Z_IMAGE_VECT(JL,2)*Z_NORMA_VECT(1))
     Z_NORMA_TEMP       =  sqrt(Z_IMAGE_TEMP(JL,1)**2.+Z_IMAGE_TEMP(JL,2)**2.+Z_IMAGE_TEMP(JL,3)**2.)
     Z_IMAGE_TEMP(JL,:)   = Z_IMAGE_TEMP(JL,:) / Z_NORMA_TEMP
     !
     ! vec(t)
     Z_IMAGE_VECT(JL,1) = +(Z_IMAGE_TEMP(JL,2)*Z_NORMA_VECT(3)-Z_IMAGE_TEMP(JL,3)*Z_NORMA_VECT(2))
     Z_IMAGE_VECT(JL,2) = -(Z_IMAGE_TEMP(JL,1)*Z_NORMA_VECT(3)-Z_IMAGE_TEMP(JL,3)*Z_NORMA_VECT(1))
     Z_IMAGE_VECT(JL,3) = +(Z_IMAGE_TEMP(JL,1)*Z_NORMA_VECT(2)-Z_IMAGE_TEMP(JL,2)*Z_NORMA_VECT(1))
     Z_NORMA_TEMP       = sqrt(Z_IMAGE_VECT(JL,1)**2.+Z_IMAGE_VECT(JL,2)**2.+Z_IMAGE_VECT(JL,3)**2.)
     Z_IMAGE_VECT(JL,:)   = Z_IMAGE_VECT(JL,:) / Z_NORMA_TEMP
     !
  ENDDO
  !
  IF (HINTERP=='CST') THEN
     Z_COEFI1 = 1.
     Z_COEFI2 = 0.
  ENDIF
  !
  IF (HINTERP=='LIN') THEN
     Z_COEFI1 = 2.
     Z_COEFI2 =-1.
  ENDIF
  !
  ! (n/t/c) at the interface
  Z_TANGE_VECT(:) = Z_COEFI1*Z_IMAGE_VECT(1,:)+Z_COEFI2*Z_IMAGE_VECT(2,:)
  Z_NORMA_TEMP       = sqrt(Z_TANGE_VECT(1)**2.+Z_TANGE_VECT(2)**2.+Z_TANGE_VECT(3)**2.)
  Z_TANGE_VECT(:)   = Z_TANGE_VECT(:) / Z_NORMA_TEMP
  !
  Z_BINOR_VECT(1) = -(Z_TANGE_VECT(2)*Z_NORMA_VECT(3)-Z_TANGE_VECT(3)*Z_NORMA_VECT(2))
  Z_BINOR_VECT(2) = +(Z_TANGE_VECT(1)*Z_NORMA_VECT(3)-Z_TANGE_VECT(3)*Z_NORMA_VECT(1))
  Z_BINOR_VECT(3) = -(Z_TANGE_VECT(1)*Z_NORMA_VECT(2)-Z_TANGE_VECT(2)*Z_NORMA_VECT(1)) 
  Z_NORMA_TEMP       = sqrt(Z_BINOR_VECT(1)**2.+Z_BINOR_VECT(2)**2.+Z_BINOR_VECT(3)**2.)
  Z_BINOR_VECT(:)   = Z_BINOR_VECT(:) / Z_NORMA_TEMP
  !
  ! matrix
  PMATRIX(1,1) = Z_NORMA_VECT(1)
  PMATRIX(1,2) = Z_NORMA_VECT(2)
  PMATRIX(1,3) = Z_NORMA_VECT(3)
  PMATRIX(2,1) = Z_TANGE_VECT(1)
  PMATRIX(2,2) = Z_TANGE_VECT(2)
  PMATRIX(2,3) = Z_TANGE_VECT(3)
  PMATRIX(3,1) = Z_BINOR_VECT(1)
  PMATRIX(3,2) = Z_BINOR_VECT(2)
  PMATRIX(3,3) = Z_BINOR_VECT(3)
  !
  DEALLOCATE(Z_IMAGE_VECT,Z_IMAGE_TEMP)
  DEALLOCATE(Z_NORMA_VECT,Z_TANGE_VECT,Z_BINOR_VECT)
  !
  RETURN
  !
END FUNCTION IBM_VALUEMAT1
