!MNH_LIC Copyright 2019-2021 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!
!     #########################
MODULE MODI_IBM_VALUEMAT2
  !   #########################
  !
  INTERFACE 
     !
     FUNCTION  IBM_VALUEMAT2(PMATRI1) RESULT(PMATRI2)
       !
       REAL, DIMENSION(:,:) , INTENT(IN) :: PMATRI1
       REAL, DIMENSION(3,3)              :: PMATRI2
       !
     END FUNCTION IBM_VALUEMAT2
     !
  END INTERFACE
  !
END MODULE MODI_IBM_VALUEMAT2
!
!    ###############################################
FUNCTION IBM_VALUEMAT2(PMATRI1) RESULT(PMATRI2)
  !  ###############################################
  !
  !****  *IBM_INTER_VALUEMAT2* - Change of basis (n,t,c) to (u,v,w) 
  !
  !    PURPOSE
  !    -------
  !     Matrix inversion 
  !
  !
  !    METHOD
  !    ------
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
  USE MODD_IBM_PARAM_n
  !
  ! interface
  !
  IMPLICIT NONE
  !
  !-----------------------------------------------------------------------------
  !
  !       0.1   Declaration of arguments
  REAL, DIMENSION(:,:), INTENT(IN) :: PMATRI1
  REAL, DIMENSION(3,3)             :: PMATRI2
  !
  !-----------------------------------------------------------------------------
  !
  !       0.2   Declaration of local variables
  !
  INTEGER :: JI,JJ,JK,JL
  REAL    :: Z_DETER
  !-----------------------------------------------------------------------------
  !
  !**** 1. PRELIMINARIES
  !     ----------------
  !
  !-----------------------------------------------------------------------------
  !       
  !**** 2. EXECUTIONS
  !     -------------
  !
  ! det(M)
  Z_DETER = PMATRI1(1,1)*PMATRI1(2,2)*PMATRI1(3,3) + &
       PMATRI1(1,2)*PMATRI1(2,3)*PMATRI1(3,1) + &
       PMATRI1(1,3)*PMATRI1(2,1)*PMATRI1(3,2) - &
       PMATRI1(1,3)*PMATRI1(2,2)*PMATRI1(3,1) - &
       PMATRI1(2,3)*PMATRI1(3,2)*PMATRI1(1,1) - &
       PMATRI1(3,3)*PMATRI1(1,2)*PMATRI1(2,1) 
  !
  ! M^(-1)
  PMATRI2(1,1) = PMATRI1(2,2)*PMATRI1(3,3)-PMATRI1(2,3)*PMATRI1(3,2) 
  PMATRI2(1,2) = PMATRI1(1,3)*PMATRI1(3,2)-PMATRI1(1,2)*PMATRI1(3,3)
  PMATRI2(1,3) = PMATRI1(1,2)*PMATRI1(2,3)-PMATRI1(1,3)*PMATRI1(2,2)
  PMATRI2(2,1) = PMATRI1(2,3)*PMATRI1(3,1)-PMATRI1(2,1)*PMATRI1(3,3)
  PMATRI2(2,2) = PMATRI1(1,1)*PMATRI1(3,3)-PMATRI1(1,3)*PMATRI1(3,1)
  PMATRI2(2,3) = PMATRI1(1,3)*PMATRI1(2,1)-PMATRI1(1,1)*PMATRI1(2,3)
  PMATRI2(3,1) = PMATRI1(2,1)*PMATRI1(3,2)-PMATRI1(2,2)*PMATRI1(3,1)
  PMATRI2(3,2) = PMATRI1(1,2)*PMATRI1(3,1)-PMATRI1(1,1)*PMATRI1(3,2)
  PMATRI2(3,3) = PMATRI1(1,1)*PMATRI1(2,2)-PMATRI1(1,2)*PMATRI1(2,1)
  !
  PMATRI2(:,:) = PMATRI2(:,:)/Z_DETER
  !
  RETURN
  !
END FUNCTION IBM_VALUEMAT2
