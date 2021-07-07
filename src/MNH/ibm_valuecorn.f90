!MNH_LIC Copyright 2019-2021 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!
!      #########################
MODULE MODI_IBM_VALUECORN
  !    #########################
  !
  INTERFACE
     !
     FUNCTION IBM_VALUECORN(PVAR,IINDEX) RESULT(PVALUE)
       !
       REAL, DIMENSION(:,:,:) , INTENT(IN) :: PVAR
       INTEGER,DIMENSION(:)   , INTENT(IN) :: IINDEX
       REAL, DIMENSION(8)                  :: PVALUE
       !
     END FUNCTION IBM_VALUECORN
     !
  END INTERFACE
  !
END MODULE MODI_IBM_VALUECORN
!
!       ##################################################
FUNCTION IBM_VALUECORN(PVAR,IINDEX) RESULT(PVALUE)
  !     ##################################################
  !
  !****     *IBM_VALUECORN*  - routine to affect values at cornes cell
  !
  !      PURPOSE
  !      -------
  !         The purpose of this routine is to compute (VAR) at corners of cell (U,V,W,P)
  !
  !      METHOD
  !      ------
  !     Index initial value 
  !     1 <-> i  ,j  ,k
  !     2 <-> i+1,j  ,k
  !     3 <-> i  ,j+1,k
  !     4 <-> i+1,j+1,k
  !     5 <-> i  ,j  ,k+1
  !     6 <-> i+1,j  ,k+1
  !     7 <-> i  ,j+1,k+1
  !     8 <-> i+1,j+1,k+1
  !
  !      EXTERNAL
  !      --------
  !        NONE
  !
  !      IMPLICIT ARGUMENTS
  !      ------------------
  !
  !      REFERENCE
  !      ---------
  !
  !      AUTHOR
  !      ------
  !        Franck Auguste       * CERFACS(AE) *
  !
  !      MODIFICATIONS
  !      -------------
  !        Original          01/01/2019
  !
  !------------------------------------------------------------------------------
  !       
  !**** 0. DECLARATIONS
  !     ---------------
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
  !       0.1  declarations of arguments
  REAL, DIMENSION(:,:,:) ,INTENT(IN) :: PVAR   ! variable array
  INTEGER, DIMENSION(:)  ,INTENT(IN) :: IINDEX ! IJK reference
  REAL, DIMENSION(8)                 :: PVALUE
  !
  !-----------------------------------------------------------------------------
  !
  !       0.2  declaration of local variables
  INTEGER :: JI,JJ,JK,JL  ! loop index
  !
  !-----------------------------------------------------------------------------
  !
  !       0.3  Allocation
  !
  !
  !-----------------------------------------------------------------------------
  !       
  !     Index initial value 
  !     1 <-> i  ,j  ,k
  !     2 <-> i+1,j  ,k
  !     3 <-> i  ,j+1,k
  !     4 <-> i+1,j+1,k
  !     5 <-> i  ,j  ,k+1
  !     6 <-> i+1,j  ,k+1
  !     7 <-> i  ,j+1,k+1
  !     8 <-> i+1,j+1,k+1
  !
  DO JL = 1,8
     !
     ! corners index 
     IF (JL==1) THEN
        JI = IINDEX(1)
        JJ = IINDEX(2)
        JK = IINDEX(3)
     ENDIF
     IF (JL==2) THEN
        JI = IINDEX(1)+1
        JJ = IINDEX(2)
        JK = IINDEX(3)
     ENDIF
     IF (JL==3) THEN
        JI = IINDEX(1)
        JJ = IINDEX(2)+1
        JK = IINDEX(3)
     ENDIF
     IF (JL==4) THEN
        JI = IINDEX(1)+1
        JJ = IINDEX(2)+1
        JK = IINDEX(3)
     ENDIF
     IF (JL==5) THEN
        JI = IINDEX(1)
        JJ = IINDEX(2)
        JK = IINDEX(3)+1
     ENDIF
     IF (JL==6) THEN
        JI = IINDEX(1)+1
        JJ = IINDEX(2)
        JK = IINDEX(3)+1
     ENDIF
     IF (JL==7) THEN
        JI = IINDEX(1)
        JJ = IINDEX(2)+1
        JK = IINDEX(3)+1
     ENDIF
     IF (JL==8) THEN
        JI = IINDEX(1)+1
        JJ = IINDEX(2)+1
        JK = IINDEX(3)+1
     ENDIF
     !
     PVALUE(JL) = PVAR(JI,JJ,JK)
     !
  ENDDO
  !
  !**** X. DEALLOCATIONS/CLOSES
  !     -----------------------
  !
  RETURN
  !
END FUNCTION IBM_VALUECORN
