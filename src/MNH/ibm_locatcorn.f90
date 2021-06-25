!MNH_LIC Copyright 2019-2021 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!
!       #########################
MODULE MODI_IBM_LOCATCORN
  !       #########################
  !
  INTERFACE
     !
     FUNCTION IBM_LOCATCORN(IINDEX,KPOS) RESULT(PLOCAT)
       !
       INTEGER,DIMENSION(:) , INTENT(IN) :: IINDEX
       INTEGER              , INTENT(IN) :: KPOS
       REAL, DIMENSION(8,3)              :: PLOCAT
       !
     END FUNCTION IBM_LOCATCORN
     !
  END INTERFACE
  !
END MODULE MODI_IBM_LOCATCORN
!
!       ##################################################
FUNCTION IBM_LOCATCORN(IINDEX,KPOS) RESULT(PLOCAT)
  !       ##################################################
  !
  !****     *IBM_LOCATCORN*  - routine to search location of each type of nodes
  !                            for one cell
  !
  !      PURPOSE
  !      -------
  !         The purpose of this routine is to compute (X,Y,Z) for corners of cell (U,V,W,P)
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
  USE MODD_GRID_n, ONLY: XXHAT,XYHAT,XZZ
  USE MODD_VAR_ll, ONLY: IP
  !
  ! interface
  !
  IMPLICIT NONE
  !
  !-----------------------------------------------------------------------------
  !
  !       0.1  declarations of arguments
  INTEGER, DIMENSION(:) , INTENT(IN) :: IINDEX  ! IJK reference
  INTEGER               , INTENT(IN) :: KPOS    ! cell type UVWP
  REAL, DIMENSION(8,3)               :: PLOCAT  ! location corner
  !
  !-----------------------------------------------------------------------------
  !
  !       0.2  declaration of local variables
  INTEGER :: JI,JJ,JK,JL  ! loop index
  INTEGER :: IIU,IJU,IKU
  INTEGER :: JIM1,JIP1,JJM1,JJP1,JKM1,JKP1
  REAL    :: ZIP1,ZJP1,ZKP1,ZIM1,ZJM1,ZKM1
  REAL    :: ZXXP,ZYYP,ZZZP,ZDXP,ZDYP,ZDZP
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
  IIU = size(XZZ,1)
  IJU = size(XZZ,2)
  IKU = size(XZZ,3)
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
     JIM1=max(1  ,JI-1)
     JJM1=max(1  ,JJ-1)
     JKM1=max(1  ,JK-1)
     JIP1=min(IIU,JI+1)
     JJP1=min(IJU,JJ+1)
     JKP1=min(IKU,JK+1)
     !
     IF(IINDEX(1)==0.or.IINDEX(2)==0.or.IINDEX(3)==0) WRITE(*,*) 'IINDEX(1,2,3): ', IINDEX(1),IINDEX(2),IINDEX(3)
     ZXXP = XXHAT(IINDEX(1))
     ZYYP = XYHAT(IINDEX(2))
     ZZZP = XZZ(IINDEX(1),IINDEX(2),IINDEX(3))
     ZDXP = XXHAT(IINDEX(1)+1)-XXHAT(IINDEX(1))
     ZDYP = XYHAT(IINDEX(2)+1)-XYHAT(IINDEX(2))
     ZDZP = XZZ(IINDEX(1),IINDEX(2),IINDEX(3)+1)-XZZ(IINDEX(1),IINDEX(2),IINDEX(3))
     !
     IF (KPOS==1) THEN
        PLOCAT(JL,1) = ZXXP+ZDXP/2.+(JI-IINDEX(1))*ZDXP
        PLOCAT(JL,2) = ZYYP+ZDYP/2.+(JJ-IINDEX(2))*ZDYP
        PLOCAT(JL,3) = ZZZP+ZDZP/2.+(JK-IINDEX(3))*ZDZP
     ENDIF
     IF (KPOS==2) THEN
        PLOCAT(JL,1) = ZXXP        +(JI-IINDEX(1))*ZDXP
        PLOCAT(JL,2) = ZYYP+ZDYP/2.+(JJ-IINDEX(2))*ZDYP
        PLOCAT(JL,3) = ZZZP+ZDZP/2.+(JK-IINDEX(3))*ZDZP
     ENDIF
     IF (KPOS==3) THEN
        PLOCAT(JL,1) = ZXXP+ZDXP/2.+(JI-IINDEX(1))*ZDXP
        PLOCAT(JL,2) = ZYYP        +(JJ-IINDEX(2))*ZDYP
        PLOCAT(JL,3) = ZZZP+ZDZP/2.+(JK-IINDEX(3))*ZDZP
     ENDIF
     IF (KPOS==4) THEN
        PLOCAT(JL,1) = ZXXP+ZDXP/2.+(JI-IINDEX(1))*ZDXP
        PLOCAT(JL,2) = ZYYP+ZDYP/2.+(JJ-IINDEX(2))*ZDYP
        PLOCAT(JL,3) = ZZZP        +(JK-IINDEX(3))*ZDZP
     ENDIF
     !
  ENDDO
  !
  !**** X. DEALLOCATIONS/CLOSES
  !     -----------------------
  !
  RETURN
  !
END FUNCTION IBM_LOCATCORN
