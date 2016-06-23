!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE PACK_PGD_SOIL (DTCO, IG, I, U, &
                                HPROGRAM, PSAND, PCLAY, PRUNOFFB, PWDRAIN)
!     ##############################################################
!
!!**** *PACK_PGD_SOIL* packs ISBA physiographic fields from all surface points to ISBA points
!!
!!    PURPOSE
!!    -------
!!
!!    METHOD
!!    ------
!!   
!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!
!!    V. Masson        Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    03/2004
!!    Escobar J.  08/02/2005 : bug declare ILU local variable
!!    B. Decharme     20008  : XWDRAIN
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
!
!
!
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_ISBA_GRID_n, ONLY : ISBA_GRID_t
USE MODD_ISBA_n, ONLY : ISBA_t
USE MODD_SURF_ATM_n, ONLY : SURF_ATM_t
!
USE MODI_PACK_SAME_RANK
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_GET_SURF_MASK_n
!
USE MODI_GET_TYPE_DIM_n
!
USE MODI_GET_LUOUT
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
!
TYPE(DATA_COVER_t), INTENT(INOUT) :: DTCO
TYPE(ISBA_GRID_t), INTENT(INOUT) :: IG
TYPE(ISBA_t), INTENT(INOUT) :: I
TYPE(SURF_ATM_t), INTENT(INOUT) :: U
!
 CHARACTER(LEN=6),        INTENT(IN) :: HPROGRAM  ! Type of program
REAL,    DIMENSION(:,:), INTENT(IN) :: PSAND     ! sand   on all surface points
REAL,    DIMENSION(:,:), INTENT(IN) :: PCLAY     ! clay   on all surface points
REAL,    DIMENSION(:),   INTENT(IN) :: PRUNOFFB  ! runoff coef. on all surface points
REAL,    DIMENSION(:),   INTENT(IN) :: PWDRAIN   ! drainage coef. on all surface points
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER                        :: ILU    ! expected physical size of full surface array
INTEGER                        :: ILUOUT ! output listing logical unit
INTEGER, DIMENSION(:), POINTER :: IMASK  ! mask for packing from complete field to nature field
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('PACK_PGD_SOIL',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
!*    1.      Number of points and packing
!             ----------------------------
!
 CALL GET_TYPE_DIM_n(DTCO, U, &
                     'NATURE',IG%NDIM)
ALLOCATE(IMASK(IG%NDIM))
ILU=0
 CALL GET_SURF_MASK_n(DTCO, U, &
                      'NATURE',IG%NDIM,IMASK,ILU,ILUOUT)
!
!
!-------------------------------------------------------------------------------
!
!*    2.      Packing of fields
!             -----------------
!
ALLOCATE(I%XSAND(IG%NDIM,I%NGROUND_LAYER))
 CALL PACK_SAME_RANK(IMASK,PSAND(:,:),I%XSAND(:,:))
!
ALLOCATE(I%XCLAY(IG%NDIM,I%NGROUND_LAYER))
 CALL PACK_SAME_RANK(IMASK,PCLAY(:,:),I%XCLAY(:,:))
!
!- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
!
ALLOCATE(I%XRUNOFFB(IG%NDIM))
 CALL PACK_SAME_RANK(IMASK,PRUNOFFB(:),I%XRUNOFFB(:))
!
ALLOCATE(I%XWDRAIN(IG%NDIM))
 CALL PACK_SAME_RANK(IMASK,PWDRAIN(:),I%XWDRAIN(:))
IF (LHOOK) CALL DR_HOOK('PACK_PGD_SOIL',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE PACK_PGD_SOIL
