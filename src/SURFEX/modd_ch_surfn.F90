!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #####################
      MODULE MODD_CH_SURF_n
!     #####################
!
!!
!!    PURPOSE
!!    -------
!     
!   
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      None
!!
!!
!!    AUTHOR
!!    ------
!!  P. Tulet   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!  16/07/03 (P. Tulet)  restructured for externalization
!!   10/2011 (S. Queguiner) Add CCH_EMIS
!------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE

TYPE CH_SURF_t
!
  CHARACTER(LEN=4)              :: CCH_EMIS            ! Option for chemical emissions
                                                       ! 'NONE' : no emission
                                                       ! 'AGGR' : one aggregated value
                                                       !    for each specie and hour
                                                       ! 'SNAP' : from SNAP data using
                                                       !    potential emission & temporal profiles
  CHARACTER(LEN=6), DIMENSION(:), POINTER :: CCH_NAMES ! NAME OF CHEMICAL
  CHARACTER(LEN=6), DIMENSION(:), POINTER :: CAER_NAMES ! NAME OF AEROSOL SPECIES
                                                       ! SPECIES (FOR DIAG ONLY)
  CHARACTER(LEN=28)             :: CCHEM_SURF_FILE     ! name of general 
                                                       ! (chemical) purpose
                                                       ! ASCII input file
  REAL, DIMENSION(:), POINTER   :: XCONVERSION         ! emission unit 
                                                       ! conversion factor
  LOGICAL  :: LCH_SURF_EMIS                            ! T : chemical emissions
                                                       ! are used
  LOGICAL  :: LCH_EMIS                                 ! T : chemical emissions
                                                       ! are present in the file
  LOGICAL  :: LRW_CH_EMIS                              ! flag to call read and
                                                       ! write emissions routine
!
END TYPE CH_SURF_t

TYPE(CH_SURF_t), ALLOCATABLE, TARGET, SAVE :: CH_SURF_MODEL(:)

 CHARACTER(LEN=4),               POINTER :: CCH_EMIS=>NULL()
!$OMP THREADPRIVATE(CCH_EMIS)
 CHARACTER(LEN=6), DIMENSION(:), POINTER :: CCH_NAMES=>NULL()
!$OMP THREADPRIVATE(CCH_NAMES)
 CHARACTER(LEN=6), DIMENSION(:), POINTER :: CAER_NAMES=>NULL()
!$OMP THREADPRIVATE(CAER_NAMES)
 CHARACTER(LEN=28), POINTER :: CCHEM_SURF_FILE=>NULL()
!$OMP THREADPRIVATE(CCHEM_SURF_FILE)
REAL, DIMENSION(:), POINTER   :: XCONVERSION=>NULL()
!$OMP THREADPRIVATE(XCONVERSION)
LOGICAL, POINTER :: LCH_SURF_EMIS=>NULL()
!$OMP THREADPRIVATE(LCH_SURF_EMIS)
LOGICAL, POINTER :: LCH_EMIS=>NULL()
!$OMP THREADPRIVATE(LCH_EMIS)
LOGICAL, POINTER :: LRW_CH_EMIS=>NULL()
!$OMP THREADPRIVATE(LRW_CH_EMIS)

CONTAINS

SUBROUTINE CH_SURF_GOTO_MODEL(KFROM, KTO, LKFROM)
LOGICAL, INTENT(IN) :: LKFROM
INTEGER, INTENT(IN) :: KFROM, KTO
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
! Save current state for allocated arrays
IF (LKFROM) THEN
CH_SURF_MODEL(KFROM)%CCH_NAMES=>CCH_NAMES
CH_SURF_MODEL(KFROM)%CAER_NAMES=>CAER_NAMES
CH_SURF_MODEL(KFROM)%XCONVERSION=>XCONVERSION
ENDIF
!
! Current model is set to model KTO
IF (LHOOK) CALL DR_HOOK('MODD_CH_SURF_N:CH_SURF_GOTO_MODEL',0,ZHOOK_HANDLE)
CCH_EMIS=>CH_SURF_MODEL(KTO)%CCH_EMIS
CCH_NAMES=>CH_SURF_MODEL(KTO)%CCH_NAMES
CAER_NAMES=>CH_SURF_MODEL(KTO)%CAER_NAMES
CCHEM_SURF_FILE=>CH_SURF_MODEL(KTO)%CCHEM_SURF_FILE
XCONVERSION=>CH_SURF_MODEL(KTO)%XCONVERSION
LCH_SURF_EMIS=>CH_SURF_MODEL(KTO)%LCH_SURF_EMIS
LCH_EMIS=>CH_SURF_MODEL(KTO)%LCH_EMIS
LRW_CH_EMIS=>CH_SURF_MODEL(KTO)%LRW_CH_EMIS
IF (LHOOK) CALL DR_HOOK('MODD_CH_SURF_N:CH_SURF_GOTO_MODEL',1,ZHOOK_HANDLE)

END SUBROUTINE CH_SURF_GOTO_MODEL

SUBROUTINE CH_SURF_ALLOC(KMODEL)
INTEGER, INTENT(IN) :: KMODEL
INTEGER :: J
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK("MODD_CH_SURF_N:CH_SURF_ALLOC",0,ZHOOK_HANDLE)
ALLOCATE(CH_SURF_MODEL(KMODEL))
DO J=1,KMODEL
  NULLIFY(CH_SURF_MODEL(J)%CCH_NAMES)
  NULLIFY(CH_SURF_MODEL(J)%CAER_NAMES)
  NULLIFY(CH_SURF_MODEL(J)%XCONVERSION)
ENDDO
CH_SURF_MODEL(:)%CCH_EMIS=' '
CH_SURF_MODEL(:)%CCHEM_SURF_FILE=' '
CH_SURF_MODEL(:)%LCH_SURF_EMIS=.FALSE.
CH_SURF_MODEL(:)%LCH_EMIS=.FALSE.
CH_SURF_MODEL(:)%LRW_CH_EMIS=.FALSE.
IF (LHOOK) CALL DR_HOOK("MODD_CH_SURF_N:CH_SURF_ALLOC",1,ZHOOK_HANDLE)
END SUBROUTINE CH_SURF_ALLOC

SUBROUTINE CH_SURF_DEALLO
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK("MODD_CH_SURF_N:CH_SURF_DEALLO",0,ZHOOK_HANDLE)
IF (ALLOCATED(CH_SURF_MODEL)) DEALLOCATE(CH_SURF_MODEL)
IF (LHOOK) CALL DR_HOOK("MODD_CH_SURF_N:CH_SURF_DEALLO",1,ZHOOK_HANDLE)
END SUBROUTINE CH_SURF_DEALLO

END MODULE MODD_CH_SURF_n
