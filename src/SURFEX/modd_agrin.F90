!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     ##################
      MODULE MODD_AGRI_n
!     ##################
!
!!****  *MODD_AGRI_n - declaration of SEEDING date for summer crops 
!!      
!!    PURPOSE
!!    -------
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      None 
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!	P. LE MOIGNE   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original       06/2006
!
!*       0.   DECLARATIONS
!             ------------
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!-------------------------------------------------------------------------------
TYPE AGRI_t
!                                          
INTEGER, POINTER, DIMENSION (:,:)   :: NIRRINUM       
                                        ! Stage for Irrigation (4 stages)
!
LOGICAL, POINTER,DIMENSION(:,:)     :: LIRRIGATE 
                                        ! True if irrigation performed
!
LOGICAL, POINTER,DIMENSION(:,:)     :: LIRRIDAY 
                                        ! True if irrigation occurs during present day
!                                          
REAL, POINTER, DIMENSION(:,:)       :: XTHRESHOLDSPT 
                                        ! Spatialized threshold

END TYPE AGRI_t
!-------------------------------------------------------------------------------

TYPE(AGRI_t), ALLOCATABLE, TARGET, SAVE :: AGRI_MODEL(:)

INTEGER, POINTER, DIMENSION (:,:)   :: NIRRINUM=>NULL()
!$OMP THREADPRIVATE(NIRRINUM)
LOGICAL, POINTER, DIMENSION (:,:)   :: LIRRIGATE=>NULL()
!$OMP THREADPRIVATE(LIRRIGATE)
LOGICAL, POINTER, DIMENSION (:,:)   :: LIRRIDAY=>NULL()
!$OMP THREADPRIVATE(LIRRIDAY)
REAL, POINTER, DIMENSION (:,:)   :: XTHRESHOLDSPT=>NULL()
!$OMP THREADPRIVATE(XTHRESHOLDSPT)

CONTAINS

SUBROUTINE AGRI_GOTO_MODEL(KFROM,KTO, LKFROM)                                        
LOGICAL, INTENT(IN) :: LKFROM
!
INTEGER, INTENT(IN) :: KFROM, KTO
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
! Save current state for allocated arrays
IF (LKFROM) THEN
AGRI_MODEL(KFROM)%NIRRINUM=>NIRRINUM
AGRI_MODEL(KFROM)%LIRRIGATE=>LIRRIGATE
AGRI_MODEL(KFROM)%LIRRIDAY=>LIRRIDAY
AGRI_MODEL(KFROM)%XTHRESHOLDSPT=>XTHRESHOLDSPT
ENDIF
!
! Current model is set to model KTO
IF (LHOOK) CALL DR_HOOK('MODD_AGRI_N:AGRI_GOTO_MODEL',0,ZHOOK_HANDLE)
NIRRINUM=>AGRI_MODEL(KTO)%NIRRINUM          
LIRRIGATE=>AGRI_MODEL(KTO)%LIRRIGATE          
LIRRIDAY=>AGRI_MODEL(KTO)%LIRRIDAY          
XTHRESHOLDSPT=>AGRI_MODEL(KTO)%XTHRESHOLDSPT          
IF (LHOOK) CALL DR_HOOK('MODD_AGRI_N:AGRI_GOTO_MODEL',1,ZHOOK_HANDLE)
                                        
END SUBROUTINE AGRI_GOTO_MODEL

SUBROUTINE AGRI_ALLOC(KMODEL)
INTEGER, INTENT(IN) :: KMODEL
INTEGER :: J
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK("MODD_AGRI_N:AGRI_ALLOC",0,ZHOOK_HANDLE)
ALLOCATE(AGRI_MODEL(KMODEL))
DO J=1,KMODEL
  NULLIFY(AGRI_MODEL(J)%NIRRINUM)
  NULLIFY(AGRI_MODEL(J)%LIRRIGATE)
  NULLIFY(AGRI_MODEL(J)%LIRRIDAY)
  NULLIFY(AGRI_MODEL(J)%XTHRESHOLDSPT)
ENDDO
IF (LHOOK) CALL DR_HOOK("MODD_AGRI_N:AGRI_ALLOC",1,ZHOOK_HANDLE)
END SUBROUTINE AGRI_ALLOC

SUBROUTINE AGRI_DEALLO
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK("MODD_AGRI_N:AGRI_DEALLO",0,ZHOOK_HANDLE)
IF (ALLOCATED(AGRI_MODEL)) DEALLOCATE(AGRI_MODEL)
IF (LHOOK) CALL DR_HOOK("MODD_AGRI_N:AGRI_DEALLO",1,ZHOOK_HANDLE)
END SUBROUTINE AGRI_DEALLO
!-------------------------------------------------------------------------------
!
END MODULE MODD_AGRI_n
