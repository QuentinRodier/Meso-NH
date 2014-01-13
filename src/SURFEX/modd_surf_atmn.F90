!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     ####################
      MODULE MODD_SURF_ATM_n
!     ######################
!
!!****  *MODD_SURF_ATM - declaration of surface parameters
!!
!!    PURPOSE
!!    -------
!     Declaration of surface parameters
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
!!	V. Masson and A. Boone   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original       01/2004
!
!*       0.   DECLARATIONS
!             ------------
!
USE MODD_TYPE_DATE_SURF
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE

TYPE SURF_ATM_t
!
!-----------------------------------------------------------------------------------------------------
!
! Type of each surface scheme
!
  CHARACTER(LEN=6) :: CTOWN      ! name of the urban surface scheme
  CHARACTER(LEN=6) :: CNATURE    ! name of the soil&vegetation surface scheme
  CHARACTER(LEN=6) :: CWATER     ! name of the scheme for inland water
  CHARACTER(LEN=6) :: CSEA       ! name for the ocean scheme
!
!-----------------------------------------------------------------------------------------------------
!
! Surface/Tile Fractions:
!
  REAL, POINTER, DIMENSION(:)   :: XTOWN     ! urban surface fraction of the grid box   (-)
  REAL, POINTER, DIMENSION(:)   :: XNATURE   ! natural surface fraction of the grid box (-)
  REAL, POINTER, DIMENSION(:)   :: XWATER    ! inland water fraction of the grid box    (-)
  REAL, POINTER, DIMENSION(:)   :: XSEA      ! sea/ocean fraction of the grid box       (-)
!
!-------------------------------------------------------------------------------
!
! type of initialization of vegetation: from cover types (ecoclimap) or parameters prescribed
!
  LOGICAL                        :: LECOCLIMAP ! T: parameters computed from ecoclimap
!                                              ! F: they are read in the file
!
!-------------------------------------------------------------------------------
!
! change water (not lake) to nature and/or town to rock : arrange cover properly
!
  LOGICAL                        :: LWATER_TO_NATURE ! T: Change Wetland treated as inland water into nature 
  LOGICAL                        :: LTOWN_TO_ROCK    ! T: Change Town into Rock
!
!-------------------------------------------------------------------------------
!
! include urban green areas for urbanized covers
!
  LOGICAL                        :: LGARDEN    ! T: define urban green areas
!                                              ! F: no urban green areas
!
!-----------------------------------------------------------------------------------------------------
!
! Masks and number of grid elements for each tile surface
!
! Sea/Ocean:
!
  INTEGER                               :: NSIZE_SEA    ! number of grid points by proc containing a
!                                                     ! sea surface                              (-)
  INTEGER                               :: NDIM_SEA     ! total number of grid points containing a
!                                                     ! sea surface                             (-)
  INTEGER, POINTER, DIMENSION(:)    :: NR_SEA       ! sea/ocean surface mask                  (-)
!
! Inland Water:
!
  INTEGER                               :: NSIZE_WATER  ! number of grid points containing an 
!                                                     ! inland water surface                    (-)
  INTEGER                               :: NDIM_WATER   ! total number of grid points by proc containing an
!                                                     ! inland surface
  INTEGER, POINTER, DIMENSION(:)    :: NR_WATER
!
! Town:
!
  INTEGER                               :: NSIZE_TOWN   ! number of grid points by proc containing an 
!                                                     ! urban surface                           (-)
  INTEGER                               :: NDIM_TOWN    ! total number of grid points containing an
!                                                     ! urban surface
  INTEGER, POINTER, DIMENSION(:)    :: NR_TOWN      ! urban surface mask                      (-)
!
! Natural surface:
!
  INTEGER                               :: NSIZE_NATURE ! number of grid points by proc containing a 
!                                                     ! natural surface                         (-)
  INTEGER                               :: NDIM_NATURE  ! total number of grid points containing a
!                                                     ! natural surface                         (-)
  INTEGER, POINTER, DIMENSION(:)    :: NR_NATURE    ! natural surface mask                    (-)
!
! All surfaces:
!
  INTEGER                               :: NSIZE_FULL   ! total number of grid points by proc     (-)
  INTEGER                               :: NDIM_FULL    ! total number of grid points             (-)
!
!-----------------------------------------------------------------------------------------------------
!
! Surface fields (only 1 horizontal dimension)
!
  REAL, POINTER, DIMENSION(:,:) :: XCOVER    ! fraction of each ecosystem for each grid box (-)
  LOGICAL, POINTER, DIMENSION(:):: LCOVER    ! GCOVER(i)=T --> ith cover field is not 0.
  REAL, POINTER, DIMENSION(:)   :: XZS       ! orography                                    (m)
!
!-------------------------------------------------------------------------------
!
  TYPE (DATE_TIME)                      :: TTIME            ! current date and time
!
  REAL                                  :: XOUT_TSTEP       ! output writing time step
!
!-----------------------------------------------------------------------------------------------------
!
! flag used to Read/Write precipitation forcing from/into the restart file for ARPEGE/ALADIN run
!
  LOGICAL    :: LINIT_PRECIP
!
!-----------------------------------------------------------------------------------------------------
!
!


END TYPE SURF_ATM_t

TYPE(SURF_ATM_t), ALLOCATABLE, TARGET, SAVE :: SURF_ATM_MODEL(:)

 CHARACTER(LEN=6), POINTER :: CTOWN=>NULL()
!$OMP THREADPRIVATE(CTOWN)
 CHARACTER(LEN=6), POINTER :: CNATURE=>NULL()
!$OMP THREADPRIVATE(CNATURE)
 CHARACTER(LEN=6), POINTER :: CWATER=>NULL()
!$OMP THREADPRIVATE(CWATER)
 CHARACTER(LEN=6), POINTER :: CSEA=>NULL()
!$OMP THREADPRIVATE(CSEA)
REAL, POINTER, DIMENSION(:)   :: XTOWN=>NULL()
!$OMP THREADPRIVATE(XTOWN)
REAL, POINTER, DIMENSION(:)   :: XNATURE=>NULL()
!$OMP THREADPRIVATE(XNATURE)
REAL, POINTER, DIMENSION(:)   :: XWATER=>NULL()
!$OMP THREADPRIVATE(XWATER)
REAL, POINTER, DIMENSION(:)   :: XSEA=>NULL()
!$OMP THREADPRIVATE(XSEA)
LOGICAL, POINTER :: LECOCLIMAP=>NULL()
!$OMP THREADPRIVATE(LECOCLIMAP)
LOGICAL, POINTER :: LWATER_TO_NATURE=>NULL()
!$OMP THREADPRIVATE(LWATER_TO_NATURE)
LOGICAL, POINTER :: LTOWN_TO_ROCK=>NULL()
!$OMP THREADPRIVATE(LTOWN_TO_ROCK)
LOGICAL, POINTER :: LGARDEN=>NULL()
!$OMP THREADPRIVATE(LGARDEN)
INTEGER, POINTER :: NSIZE_SEA=>NULL()
!$OMP THREADPRIVATE(NSIZE_SEA)
INTEGER, POINTER :: NDIM_SEA=>NULL()
!$OMP THREADPRIVATE(NDIM_SEA)
INTEGER, POINTER, DIMENSION(:)    :: NR_SEA=>NULL()
!$OMP THREADPRIVATE(NR_SEA)
INTEGER, POINTER :: NSIZE_WATER=>NULL()
!$OMP THREADPRIVATE(NSIZE_WATER)
INTEGER, POINTER :: NDIM_WATER=>NULL()
!$OMP THREADPRIVATE(NDIM_WATER)
INTEGER, POINTER, DIMENSION(:)    :: NR_WATER=>NULL()
!$OMP THREADPRIVATE(NR_WATER)
INTEGER, POINTER :: NSIZE_TOWN=>NULL()
!$OMP THREADPRIVATE(NSIZE_TOWN)
INTEGER, POINTER :: NDIM_TOWN=>NULL()
!$OMP THREADPRIVATE(NDIM_TOWN)
INTEGER, POINTER, DIMENSION(:)    :: NR_TOWN=>NULL()
!$OMP THREADPRIVATE(NR_TOWN)
INTEGER, POINTER :: NSIZE_NATURE=>NULL()
!$OMP THREADPRIVATE(NSIZE_NATURE)
INTEGER, POINTER :: NDIM_NATURE=>NULL()
!$OMP THREADPRIVATE(NDIM_NATURE)
INTEGER, POINTER, DIMENSION(:)    :: NR_NATURE=>NULL()
!$OMP THREADPRIVATE(NR_NATURE)
INTEGER, POINTER :: NSIZE_FULL=>NULL()
!$OMP THREADPRIVATE(NSIZE_FULL)
INTEGER, POINTER :: NDIM_FULL=>NULL()
!$OMP THREADPRIVATE(NDIM_FULL)
REAL, POINTER, DIMENSION(:,:) :: XCOVER=>NULL()
!$OMP THREADPRIVATE(XCOVER)
LOGICAL, POINTER, DIMENSION(:):: LCOVER=>NULL()
!$OMP THREADPRIVATE(LCOVER)
REAL, POINTER, DIMENSION(:)   :: XZS=>NULL()
!$OMP THREADPRIVATE(XZS)
TYPE (DATE_TIME), POINTER :: TTIME=>NULL()
!$OMP THREADPRIVATE(TTIME)
REAL, POINTER :: XOUT_TSTEP=>NULL()
!$OMP THREADPRIVATE(XOUT_TSTEP)
LOGICAL, POINTER :: LINIT_PRECIP=>NULL()
!$OMP THREADPRIVATE(LINIT_PRECIP)
CONTAINS

SUBROUTINE SURF_ATM_GOTO_MODEL(KFROM, KTO, LKFROM)
LOGICAL, INTENT(IN) :: LKFROM
INTEGER, INTENT(IN) :: KFROM, KTO
REAL(KIND=JPRB) :: ZHOOK_HANDLE

!
! Save current state for allocated arrays
IF (LKFROM) THEN
SURF_ATM_MODEL(KFROM)%XTOWN=>XTOWN
SURF_ATM_MODEL(KFROM)%XNATURE=>XNATURE
SURF_ATM_MODEL(KFROM)%XWATER=>XWATER
SURF_ATM_MODEL(KFROM)%XSEA=>XSEA
SURF_ATM_MODEL(KFROM)%NR_SEA=>NR_SEA
SURF_ATM_MODEL(KFROM)%NR_WATER=>NR_WATER
SURF_ATM_MODEL(KFROM)%NR_TOWN=>NR_TOWN
SURF_ATM_MODEL(KFROM)%NR_NATURE=>NR_NATURE
SURF_ATM_MODEL(KFROM)%XCOVER=>XCOVER
SURF_ATM_MODEL(KFROM)%LCOVER=>LCOVER
SURF_ATM_MODEL(KFROM)%XZS=>XZS
ENDIF
!
! Current model is set to model KTO
IF (LHOOK) CALL DR_HOOK('MODD_SURF_ATM_N:SURF_ATM_GOTO_MODEL',0,ZHOOK_HANDLE)

CTOWN=>SURF_ATM_MODEL(KTO)%CTOWN
CNATURE=>SURF_ATM_MODEL(KTO)%CNATURE
CWATER=>SURF_ATM_MODEL(KTO)%CWATER
CSEA=>SURF_ATM_MODEL(KTO)%CSEA
XTOWN=>SURF_ATM_MODEL(KTO)%XTOWN
XNATURE=>SURF_ATM_MODEL(KTO)%XNATURE
XWATER=>SURF_ATM_MODEL(KTO)%XWATER
XSEA=>SURF_ATM_MODEL(KTO)%XSEA
LECOCLIMAP=>SURF_ATM_MODEL(KTO)%LECOCLIMAP
LWATER_TO_NATURE=>SURF_ATM_MODEL(KTO)%LWATER_TO_NATURE
LTOWN_TO_ROCK=>SURF_ATM_MODEL(KTO)%LTOWN_TO_ROCK
LGARDEN=>SURF_ATM_MODEL(KTO)%LGARDEN
NSIZE_SEA=>SURF_ATM_MODEL(KTO)%NSIZE_SEA
NDIM_SEA=>SURF_ATM_MODEL(KTO)%NDIM_SEA
NR_SEA=>SURF_ATM_MODEL(KTO)%NR_SEA
NSIZE_WATER=>SURF_ATM_MODEL(KTO)%NSIZE_WATER
NDIM_WATER=>SURF_ATM_MODEL(KTO)%NDIM_WATER
NR_WATER=>SURF_ATM_MODEL(KTO)%NR_WATER
NSIZE_TOWN=>SURF_ATM_MODEL(KTO)%NSIZE_TOWN
NDIM_TOWN=>SURF_ATM_MODEL(KTO)%NDIM_TOWN
NR_TOWN=>SURF_ATM_MODEL(KTO)%NR_TOWN
NSIZE_NATURE=>SURF_ATM_MODEL(KTO)%NSIZE_NATURE
NDIM_NATURE=>SURF_ATM_MODEL(KTO)%NDIM_NATURE
NR_NATURE=>SURF_ATM_MODEL(KTO)%NR_NATURE
NSIZE_FULL=>SURF_ATM_MODEL(KTO)%NSIZE_FULL
NDIM_FULL=>SURF_ATM_MODEL(KTO)%NDIM_FULL
XCOVER=>SURF_ATM_MODEL(KTO)%XCOVER
LCOVER=>SURF_ATM_MODEL(KTO)%LCOVER
XZS=>SURF_ATM_MODEL(KTO)%XZS
TTIME=>SURF_ATM_MODEL(KTO)%TTIME
XOUT_TSTEP=>SURF_ATM_MODEL(KTO)%XOUT_TSTEP
LINIT_PRECIP=>SURF_ATM_MODEL(KTO)%LINIT_PRECIP

IF (LHOOK) CALL DR_HOOK('MODD_SURF_ATM_N:SURF_ATM_GOTO_MODEL',1,ZHOOK_HANDLE)

END SUBROUTINE SURF_ATM_GOTO_MODEL

SUBROUTINE SURF_ATM_ALLOC(KMODEL)
INTEGER, INTENT(IN) :: KMODEL
INTEGER :: J
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK("MODD_SURF_ATM_N:SURF_ATM_ALLOC",0,ZHOOK_HANDLE)
ALLOCATE(SURF_ATM_MODEL(KMODEL))
DO J=1,KMODEL
  NULLIFY(SURF_ATM_MODEL(J)%XTOWN)
  NULLIFY(SURF_ATM_MODEL(J)%XNATURE)
  NULLIFY(SURF_ATM_MODEL(J)%XWATER)
  NULLIFY(SURF_ATM_MODEL(J)%XSEA)
  NULLIFY(SURF_ATM_MODEL(J)%NR_SEA)
  NULLIFY(SURF_ATM_MODEL(J)%NR_WATER)
  NULLIFY(SURF_ATM_MODEL(J)%NR_TOWN)
  NULLIFY(SURF_ATM_MODEL(J)%NR_NATURE)
  NULLIFY(SURF_ATM_MODEL(J)%XCOVER)
  NULLIFY(SURF_ATM_MODEL(J)%LCOVER)
  NULLIFY(SURF_ATM_MODEL(J)%XZS)
ENDDO
SURF_ATM_MODEL(:)%CTOWN=' '
SURF_ATM_MODEL(:)%CNATURE=' '
SURF_ATM_MODEL(:)%CWATER=' '
SURF_ATM_MODEL(:)%CSEA=' '
SURF_ATM_MODEL(:)%LECOCLIMAP=.FALSE.
SURF_ATM_MODEL(:)%LWATER_TO_NATURE=.FALSE.
SURF_ATM_MODEL(:)%LTOWN_TO_ROCK=.FALSE.
SURF_ATM_MODEL(:)%LGARDEN=.FALSE.
SURF_ATM_MODEL(:)%NSIZE_SEA=0
SURF_ATM_MODEL(:)%NDIM_SEA=0
SURF_ATM_MODEL(:)%NSIZE_WATER=0
SURF_ATM_MODEL(:)%NDIM_WATER=0
SURF_ATM_MODEL(:)%NSIZE_TOWN=0
SURF_ATM_MODEL(:)%NDIM_TOWN=0
SURF_ATM_MODEL(:)%NSIZE_NATURE=0
SURF_ATM_MODEL(:)%NDIM_NATURE=0
SURF_ATM_MODEL(:)%NSIZE_FULL=0
SURF_ATM_MODEL(:)%NDIM_FULL=0
SURF_ATM_MODEL(:)%XOUT_TSTEP=0.
SURF_ATM_MODEL(:)%LINIT_PRECIP=.FALSE.
IF (LHOOK) CALL DR_HOOK("MODD_SURF_ATM_N:SURF_ATM_ALLOC",1,ZHOOK_HANDLE)
END SUBROUTINE SURF_ATM_ALLOC

SUBROUTINE SURF_ATM_DEALLO
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK("MODD_SURF_ATM_N:SURF_ATM_DEALLO",0,ZHOOK_HANDLE)
IF (ALLOCATED(SURF_ATM_MODEL)) DEALLOCATE(SURF_ATM_MODEL)
IF (LHOOK) CALL DR_HOOK("MODD_SURF_ATM_N:SURF_ATM_DEALLO",1,ZHOOK_HANDLE)
END SUBROUTINE SURF_ATM_DEALLO

END MODULE MODD_SURF_ATM_n

