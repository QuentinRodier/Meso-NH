!MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!    ###########################
     MODULE MODI_COMPUTE_BL89_ML
!    ###########################

INTERFACE

!     ###################################################################
      SUBROUTINE COMPUTE_BL89_ML(KKA,KKB,KKE,KKU,KKL,PDZZ2D, &
             PTKEM2D,PG_O_THVREF2D,PVPT,KK,OUPORDN,PLWORK)
!     ###################################################################

!*               1.1  Declaration of Arguments

INTEGER,                INTENT(IN)   :: KKA          ! near ground array index
INTEGER,                INTENT(IN)   :: KKB          ! near ground physical index
INTEGER,                INTENT(IN)   :: KKE          ! uppest atmosphere physical index
INTEGER,                INTENT(IN)   :: KKU          ! uppest atmosphere array index
INTEGER,                INTENT(IN)   :: KKL          ! +1 if grid goes from ground to atmosphere top, -1 otherwise
REAL, DIMENSION(:,:),   INTENT(IN)  :: PDZZ2D
REAL, DIMENSION(:,:),   INTENT(IN)  :: PTKEM2D
REAL, DIMENSION(:,:),   INTENT(IN)  :: PG_O_THVREF2D
REAL, DIMENSION(:,:),   INTENT(IN)  :: PVPT
INTEGER,                INTENT(IN)  :: KK
LOGICAL,                INTENT(IN)  :: OUPORDN
REAL, DIMENSION(:),     INTENT(OUT) :: PLWORK

END SUBROUTINE COMPUTE_BL89_ML

END INTERFACE
!
END MODULE MODI_COMPUTE_BL89_ML



!     ###################################################################
      SUBROUTINE COMPUTE_BL89_ML(KKA,KKB,KKE,KKU,KKL,PDZZ2D, &
             PTKEM2D,PG_O_THVREF2D,PVPT,KK,OUPORDN,PLWORK)
!     ###################################################################
!!
!!     COMPUTE_BL89_ML routine to:
!!       1/ compute upward or downward mixing length with BL89 formulation
!!
!!    AUTHOR
!!    ------
!!     J. PERGAUD
!!
!!    MODIFICATIONS
!!    -------------
!!     Original   19/01/06
!!     S. Riette Jan 2012: support for both order of vertical levels and cleaning
!!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!
!              ------------
!
!!!!!!!!!!!!
!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!! WARNING !!
!!!!!!!!!!!!
!!!!!!!!!!!!
!Any modification done to this routine must be copied in bl89.f90.
!This routine was inlined in bl89 for numerical performance reasons
!but algorithm must remain the same.
!!!!!!!!!!!!
!
USE MODD_CTURB
USE MODD_PARAMETERS, ONLY: JPVEXT
USE MODI_SHUMAN_MF
!
IMPLICIT NONE
!
!          0.1 arguments
!
INTEGER,                INTENT(IN)   :: KKA          ! near ground array index
INTEGER,                INTENT(IN)   :: KKB          ! near ground physical index
INTEGER,                INTENT(IN)   :: KKE          ! uppest atmosphere physical index
INTEGER,                INTENT(IN)   :: KKU          ! uppest atmosphere array index
INTEGER,                INTENT(IN)   :: KKL          ! +1 if grid goes from ground to atmosphere top, -1 otherwise
REAL, DIMENSION(:,:),   INTENT(IN)  :: PDZZ2D
REAL, DIMENSION(:,:),   INTENT(IN)  :: PTKEM2D
REAL, DIMENSION(:,:),   INTENT(IN)  :: PG_O_THVREF2D
REAL, DIMENSION(:,:),   INTENT(IN)  :: PVPT
INTEGER,                INTENT(IN)  :: KK
LOGICAL,                INTENT(IN)  :: OUPORDN
REAL, DIMENSION(:),     INTENT(OUT) :: PLWORK

!          0.2 Local variable
!
REAL, DIMENSION(SIZE(PTKEM2D,1)) :: ZLWORK1,ZLWORK2 ! Temporary mixing length
REAL, DIMENSION(SIZE(PTKEM2D,1)) :: ZINTE,ZPOTE     ! TKE and potential energy
                                                    ! between 2 levels
!
REAL, DIMENSION(SIZE(PTKEM2D,1),SIZE(PTKEM2D,2)) :: ZDELTVPT,ZHLVPT                                
                      !Virtual Potential Temp at Half level and DeltaThv between
                      !2 levels

INTEGER :: IIJU                 !Internal Domain
INTEGER :: J1D                  !horizontal loop counter
INTEGER :: JKK                  !loop counters
INTEGER :: JRR                  !moist loop counter
INTEGER :: JIJK                 !loop counters 
REAL    :: ZTEST,ZTEST0,ZTESTM  !test for vectorization
!-------------------------------------------------------------------------------------
!
!*       1.    INITIALISATION
!              --------------
IIJU=SIZE(PTKEM2D,1)
!
ZDELTVPT(:,:)=DZM_MF(KKA,KKU,KKL,PVPT(:,:))
ZDELTVPT(:,KKA)=0.
WHERE (ABS(ZDELTVPT(:,:))<XLINF)
  ZDELTVPT(:,:)=XLINF
END WHERE
!
ZHLVPT(:,:)=MZM_MF(KKA,KKU,KKL,PVPT(:,:))
!
!
!
!*       2.    CALCULATION OF THE UPWARD MIXING LENGTH
!              ---------------------------------------
!

IF (OUPORDN.EQV..TRUE.) THEN 
 ZINTE(:)=PTKEM2D(:,KK)
 PLWORK=0.
 ZTESTM=1.
 DO JKK=KK+KKL,KKE,KKL
    IF(ZTESTM > 0.) THEN
      ZTESTM=0
      DO J1D=1,IIJU
        ZTEST0=0.5+SIGN(0.5,ZINTE(J1D))
        ZPOTE(J1D) = ZTEST0*(PG_O_THVREF2D(J1D,KK)      *      &
            (ZHLVPT(J1D,JKK) - PVPT(J1D,KK)))  * PDZZ2D(J1D,JKK) !particle keeps its temperature
        ZTEST =0.5+SIGN(0.5,ZINTE(J1D)-ZPOTE(J1D))
        ZTESTM=ZTESTM+ZTEST0
        ZLWORK1(J1D)=PDZZ2D(J1D,JKK)
        !ZLWORK2 jump of the last reached level
        ZLWORK2(J1D)=        ( - PG_O_THVREF2D(J1D,KK) *                     &
            (  PVPT(J1D,JKK-KKL) - PVPT(J1D,KK) )                              &
          + SQRT (ABS(                                                       &
            ( PG_O_THVREF2D(J1D,KK) * (PVPT(J1D,JKK-KKL) - PVPT(J1D,KK)) )**2  &
            + 2. * ZINTE(J1D) * PG_O_THVREF2D(J1D,KK)                        &
                 * ZDELTVPT(J1D,JKK) / PDZZ2D(J1D,JKK) ))    ) /             &
        ( PG_O_THVREF2D(J1D,KK) * ZDELTVPT(J1D,JKK) / PDZZ2D(J1D,JKK) ) 
      !
        PLWORK(J1D)=PLWORK(J1D)+ZTEST0*(ZTEST*ZLWORK1(J1D)+  &
                                    (1-ZTEST)*ZLWORK2(J1D))
        ZINTE(J1D) = ZINTE(J1D) - ZPOTE(J1D)
      END DO 
    ENDIF
  END DO 
ENDIF
!!
!*       2.    CALCULATION OF THE DOWNWARD MIXING LENGTH
!              ---------------------------------------
!

IF (OUPORDN.EQV..FALSE.) THEN 
 ZINTE(:)=PTKEM2D(:,KK)
 PLWORK=0.
 ZTESTM=1.
 DO JKK=KK,KKB,-KKL
    IF(ZTESTM > 0.) THEN
      ZTESTM=0
      DO J1D=1,IIJU
        ZTEST0=0.5+SIGN(0.5,ZINTE(J1D))
        ZPOTE(J1D) = -ZTEST0*(PG_O_THVREF2D(J1D,KK)      *      &
            (ZHLVPT(J1D,JKK) - PVPT(J1D,KK)))  * PDZZ2D(J1D,JKK) !particle keeps its temperature
        ZTEST =0.5+SIGN(0.5,ZINTE(J1D)-ZPOTE(J1D))
        ZTESTM=ZTESTM+ZTEST0
        ZLWORK1(J1D)=PDZZ2D(J1D,JKK)
        ZLWORK2(J1D)=        ( + PG_O_THVREF2D(J1D,KK) *                     &
            (  PVPT(J1D,JKK) - PVPT(J1D,KK) )                              &
          + SQRT (ABS(                                                       &
            ( PG_O_THVREF2D(J1D,KK) * (PVPT(J1D,JKK) - PVPT(J1D,KK)) )**2  &
            + 2. * ZINTE(J1D) * PG_O_THVREF2D(J1D,KK)                        &
                 * ZDELTVPT(J1D,JKK) / PDZZ2D(J1D,JKK) ))    ) /             &
        ( PG_O_THVREF2D(J1D,KK) * ZDELTVPT(J1D,JKK) / PDZZ2D(J1D,JKK) ) 
      !
        PLWORK(J1D)=PLWORK(J1D)+ZTEST0*(ZTEST*ZLWORK1(J1D)+  &
                                    (1-ZTEST)*ZLWORK2(J1D)) 
        ZINTE(J1D) = ZINTE(J1D) - ZPOTE(J1D)
      END DO 
    ENDIF
  END DO 
ENDIF
  
END SUBROUTINE COMPUTE_BL89_ML
