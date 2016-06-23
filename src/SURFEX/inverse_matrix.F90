!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
SUBROUTINE INVERSE_MATRIX(N,A,P)
!--------------------------------------------------------
!
! Explicit inversed matrix after Cholesky decomposition
!
!--------------------------------------------------------
 USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
 USE PARKIND1  ,ONLY : JPRB
!
 IMPLICIT NONE
 INTEGER, INTENT(IN) :: N
 REAL, DIMENSION (N,N),  INTENT(INOUT) :: A
 REAL, DIMENSION (N),  INTENT(IN)      :: P
 REAL ZSUM
 INTEGER :: I, J, K
 REAL(KIND=JPRB) :: ZHOOK_HANDLE

 IF (LHOOK) CALL DR_HOOK('INVERSE_MATRIX',0,ZHOOK_HANDLE)
 DO I=1,N
   A(I,I)=1./P(I)
   DO J=I+1,N
     ZSUM = 0.
     DO K=I,J-1
       ZSUM = ZSUM - A(J,K)*A(K,I)
     ENDDO
     A(J,I) = ZSUM/P(J)
   ENDDO
 ENDDO  
 DO I=1,N
   DO J=I+1,N
      A(I,J) =0.0
   ENDDO
 ENDDO
 A = MATMUL(TRANSPOSE(A),A)

 IF (LHOOK) CALL DR_HOOK('INVERSE_MATRIX',1,ZHOOK_HANDLE)

END SUBROUTINE INVERSE_MATRIX
