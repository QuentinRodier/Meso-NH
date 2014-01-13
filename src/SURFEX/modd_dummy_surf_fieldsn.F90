!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     ####################
      MODULE MODD_DUMMY_SURF_FIELDS_n
!     ####################
!
!!****  *MODD_DUMMY_SURF_FIELDS* - declaration of dummy physiographic data arrays
!!
!!    PURPOSE
!!    -------
!       The purpose of this declarative module is to specify  the 
!     dummy physiographic data arrays.
!
!!
!!    AUTHOR
!!    ------
!!	V. Masson   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    03/2004                      
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE

TYPE DUMMY_SURF_FIELDS_t
!
  INTEGER                                          :: NDUMMY_NBR
!                          ! number of dummy pgd fields chosen by user
  CHARACTER(LEN=3) , DIMENSION(:), POINTER         :: CDUMMY_AREA =>  NULL()
!                          ! areas where dummy pgd fields are defined
!                          ! 'ALL' : everywhere
!                          ! 'SEA' : where sea exists
!                          ! 'LAN' : where land exists
!                          ! 'WAT' : where inland water exists
!                          ! 'NAT' : where natural or agricultural areas exist
!                          ! 'TWN' : where town areas exist
!                          ! 'STR' : where streets are present
!                          ! 'BLD' : where buildings are present
!                          !
  CHARACTER(LEN=20), DIMENSION(:), POINTER         :: CDUMMY_NAME =>  NULL()
!                          ! name of the dummy pgd fields (for information)
  REAL,              DIMENSION(:,:), POINTER       :: XDUMMY_FIELDS =>  NULL()
!                          ! dummy pgd fields themselves
!
!-------------------------------------------------------------------------------
!
END TYPE DUMMY_SURF_FIELDS_t

TYPE(DUMMY_SURF_FIELDS_t), ALLOCATABLE, TARGET, SAVE :: DUMMY_SURF_FIELDS_MODEL(:)
LOGICAL, ALLOCATABLE, SAVE :: DUMMY_SURF_FIELDS_FIRST_CALL(:)

INTEGER, POINTER :: NDUMMY_NBR=>NULL()
!$OMP THREADPRIVATE(NDUMMY_NBR)
 CHARACTER(LEN=3) , DIMENSION(:), POINTER :: CDUMMY_AREA=>NULL()
!$OMP THREADPRIVATE(CDUMMY_AREA)
 CHARACTER(LEN=20), DIMENSION(:), POINTER :: CDUMMY_NAME=>NULL()
!$OMP THREADPRIVATE(CDUMMY_NAME)
REAL,              DIMENSION(:,:), POINTER   :: XDUMMY_FIELDS=>NULL()
!$OMP THREADPRIVATE(XDUMMY_FIELDS)

CONTAINS

SUBROUTINE DUMMY_SURF_FIELDS_GOTO_MODEL(KFROM, KTO, LKFROM)
LOGICAL, INTENT(IN) :: LKFROM
INTEGER, INTENT(IN) :: KFROM, KTO
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
! Save current state for allocated arrays
IF (LKFROM) THEN
DUMMY_SURF_FIELDS_MODEL(KFROM)%XDUMMY_FIELDS=>XDUMMY_FIELDS
ENDIF
!
IF (LHOOK) CALL DR_HOOK('MODD_DUMMY_SURF_FIELDS_N:DUMMY_SURF_FIELDS_GOTO_MODEL',0,ZHOOK_HANDLE)
IF (DUMMY_SURF_FIELDS_FIRST_CALL(KTO)) THEN
ALLOCATE (DUMMY_SURF_FIELDS_MODEL(KTO)%CDUMMY_AREA(1000))
ALLOCATE (DUMMY_SURF_FIELDS_MODEL(KTO)%CDUMMY_NAME(1000))
DUMMY_SURF_FIELDS_FIRST_CALL(KTO) = .FALSE.
ENDIF
! Current model is set to model KTO
NDUMMY_NBR=>DUMMY_SURF_FIELDS_MODEL(KTO)%NDUMMY_NBR
CDUMMY_AREA=>DUMMY_SURF_FIELDS_MODEL(KTO)%CDUMMY_AREA
CDUMMY_NAME=>DUMMY_SURF_FIELDS_MODEL(KTO)%CDUMMY_NAME
XDUMMY_FIELDS=>DUMMY_SURF_FIELDS_MODEL(KTO)%XDUMMY_FIELDS
IF (LHOOK) CALL DR_HOOK('MODD_DUMMY_SURF_FIELDS_N:DUMMY_SURF_FIELDS_GOTO_MODEL',1,ZHOOK_HANDLE)

END SUBROUTINE DUMMY_SURF_FIELDS_GOTO_MODEL

SUBROUTINE DUMMY_SURF_FIELDS_ALLOC(KMODEL)
INTEGER, INTENT(IN) :: KMODEL
INTEGER :: J
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK("MODD_DUMMY_SURF_FIELDS_N:DUMMY_SURF_FIELDS_ALLOC",0,ZHOOK_HANDLE)
ALLOCATE(DUMMY_SURF_FIELDS_MODEL(KMODEL))
ALLOCATE(DUMMY_SURF_FIELDS_FIRST_CALL(KMODEL))
DO J=1,KMODEL
  NULLIFY(DUMMY_SURF_FIELDS_MODEL(J)%CDUMMY_NAME)
  NULLIFY(DUMMY_SURF_FIELDS_MODEL(J)%CDUMMY_AREA)
ENDDO
DUMMY_SURF_FIELDS_MODEL(:)%NDUMMY_NBR=0
DUMMY_SURF_FIELDS_FIRST_CALL(:)=.TRUE.
IF (LHOOK) CALL DR_HOOK("MODD_DUMMY_SURF_FIELDS_N:DUMMY_SURF_FIELDS_ALLOC",1,ZHOOK_HANDLE)
END SUBROUTINE DUMMY_SURF_FIELDS_ALLOC

SUBROUTINE DUMMY_SURF_FIELDS_DEALLO
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK("MODD_DUMMY_SURF_FIELDS_N:DUMMY_SURF_FIELDS_DEALLO",0,ZHOOK_HANDLE)
IF (ALLOCATED(DUMMY_SURF_FIELDS_MODEL)) DEALLOCATE(DUMMY_SURF_FIELDS_MODEL)
IF (ALLOCATED(DUMMY_SURF_FIELDS_FIRST_CALL)) DEALLOCATE(DUMMY_SURF_FIELDS_FIRST_CALL)
IF (LHOOK) CALL DR_HOOK("MODD_DUMMY_SURF_FIELDS_N:DUMMY_SURF_FIELDS_DEALLO",1,ZHOOK_HANDLE)
END SUBROUTINE DUMMY_SURF_FIELDS_DEALLO

END MODULE MODD_DUMMY_SURF_FIELDS_n
