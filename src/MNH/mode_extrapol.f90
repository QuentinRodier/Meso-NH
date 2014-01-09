!MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
MODULE MODE_EXTRAPOL

  INTERFACE EXTRAPOL

     MODULE PROCEDURE EXTRAPOL3D,EXTRAPOL3DN,EXTRAPOL2D,EXTRAPOL2DN

  END INTERFACE

CONTAINS

  SUBROUTINE EXTRAPOL3D(HBORD,PTAB)
    USE MODD_LBC_n
    USE MODE_ll
    !
    IMPLICIT NONE
    !
    !*       0.1   Declarations of arguments
    !
    CHARACTER              :: HBORD
    REAL, DIMENSION(:,:,:) :: PTAB

    !
    !*       0.2   Declarations of local variables
    !
    INTEGER          :: IIB,IJB,IKB    ! Begining useful area  in x,y,z directions
    INTEGER          :: IIE,IJE,IKE    ! End useful area in x,y,z directions
    !
    !-------------------------------------------------------------------------------
    !
    !*       1.     EXTRAPOLE LATERAL BOUNDARY CONDITIONS :
    !               ---------------------------------------
    !
    !RETURN
    CALL GET_INDICE_ll (IIB,IJB,IIE,IJE)

    SELECT  CASE (HBORD)
    CASE ('W')
       IF (LWEST_ll() .AND. CLBCX(1)/='CYCL')  &
            PTAB(IIB-1,:,:) = 2. * PTAB(IIB,:,:) - PTAB(IIB+1,:,:)
    CASE ('E')
       IF (LEAST_ll() .AND. CLBCX(1)/='CYCL')   &
            PTAB(IIE+1,:,:) = 2. * PTAB(IIE,:,:) - PTAB(IIE-1,:,:)
    CASE ('S')
       IF (LSOUTH_ll() .AND. CLBCY(1)/='CYCL') &
            PTAB(:,IJB-1,:) = 2. * PTAB(:,IJB,:) - PTAB(:,IJB+1,:)
    CASE ('N')
       IF (LNORTH_ll() .AND. CLBCY(1)/='CYCL') &
            PTAB(:,IJE+1,:) = 2. * PTAB(:,IJE,:) - PTAB(:,IJE-1,:)
    CASE   DEFAULT
    END SELECT

  END SUBROUTINE EXTRAPOL3D

  SUBROUTINE EXTRAPOL3DN(HBORD,P1,P2,P3,P4,P5,P6 )
    IMPLICIT NONE
    CHARACTER              :: HBORD
    REAL, DIMENSION(:,:,:)            :: P1,P2
    REAL, DIMENSION(:,:,:) , OPTIONAL :: P3,P4,P5,P6

    CALL EXTRAPOL(HBORD,P1)
    CALL EXTRAPOL(HBORD,P2)
    IF (PRESENT(P3)) CALL EXTRAPOL(HBORD,P3)
    IF (PRESENT(P4)) CALL EXTRAPOL(HBORD,P4)
    IF (PRESENT(P5)) CALL EXTRAPOL(HBORD,P5)
    IF (PRESENT(P6)) CALL EXTRAPOL(HBORD,P6)

  END SUBROUTINE EXTRAPOL3DN

  SUBROUTINE EXTRAPOL2D(HBORD,PTAB)
    USE MODD_LBC_n
    USE MODE_ll
    !
    IMPLICIT NONE
    !
    !*       0.1   Declarations of arguments
    !
    CHARACTER              :: HBORD
    REAL, DIMENSION(:,:) :: PTAB

    !
    !*       0.2   Declarations of local variables
    !
    INTEGER          :: IIB,IJB    ! Begining useful area  in x,y,z directions
    INTEGER          :: IIE,IJE    ! End useful area in x,y,z directions
    !
    !-------------------------------------------------------------------------------
    !
    !*       1.     EXTRAPOLE LATERAL BOUNDARY CONDITIONS :
    !               ---------------------------------------
    !
    !RETURN
    CALL GET_INDICE_ll (IIB,IJB,IIE,IJE)

    SELECT  CASE (HBORD)
    CASE ('W')
       IF (LWEST_ll() .AND. CLBCX(1)/='CYCL')  &
            PTAB(IIB-1,:) = 2. * PTAB(IIB,:) - PTAB(IIB+1,:)
    CASE ('E')
       IF (LEAST_ll() .AND. CLBCX(1)/='CYCL')   &
            PTAB(IIE+1,:) = 2. * PTAB(IIE,:) - PTAB(IIE-1,:)
    CASE ('S')
       IF (LSOUTH_ll() .AND. CLBCY(1)/='CYCL') &
            PTAB(:,IJB-1) = 2. * PTAB(:,IJB) - PTAB(:,IJB+1)
    CASE ('N')
       IF (LNORTH_ll() .AND. CLBCY(1)/='CYCL') &
            PTAB(:,IJE+1) = 2. * PTAB(:,IJE) - PTAB(:,IJE-1)
    CASE   DEFAULT
    END SELECT

  END SUBROUTINE EXTRAPOL2D

  SUBROUTINE EXTRAPOL2DN(HBORD,P1,P2,P3,P4,P5,P6 )
    IMPLICIT NONE
    CHARACTER              :: HBORD
    REAL, DIMENSION(:,:)            :: P1,P2
    REAL, DIMENSION(:,:) , OPTIONAL :: P3,P4,P5,P6

    CALL EXTRAPOL(HBORD,P1)
    CALL EXTRAPOL(HBORD,P2)
    IF (PRESENT(P3)) CALL EXTRAPOL(HBORD,P3)
    IF (PRESENT(P4)) CALL EXTRAPOL(HBORD,P4)
    IF (PRESENT(P5)) CALL EXTRAPOL(HBORD,P5)
    IF (PRESENT(P6)) CALL EXTRAPOL(HBORD,P6)

  END SUBROUTINE EXTRAPOL2DN

END MODULE MODE_EXTRAPOL
