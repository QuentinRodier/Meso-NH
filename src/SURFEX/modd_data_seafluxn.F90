!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     ##########################
      MODULE MODD_DATA_SEAFLUX_n
!     ##########################
!
!!****  *MODD_DATA_SEAFLUX - declaration of SEAFLUX surface parameters for SEAFLUX scheme
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
!!	P. Le Moigne  *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original       09/2007
!
!*       0.   DECLARATIONS
!             ------------
!
!
USE MODD_TYPE_DATE_SURF
!      
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE

TYPE DATA_SEAFLUX_t
!-------------------------------------------------------------------------------
!
TYPE (DATE_TIME), POINTER, DIMENSION(:)  :: TDATA_SST  ! date of sst field 
!
!-------------------------------------------------------------------------------
!
  REAL, POINTER, DIMENSION(:,:)  :: XDATA_SST       ! sea surface temperature for
!                                                   ! each grid mesh                   (-)
!
!-------------------------------------------------------------------------------
!
  INTEGER                        :: NTIME           ! number of time data
!                                                   ! for SST
!
!-------------------------------------------------------------------------------
!
  LOGICAL                        :: LSST_DATA       ! flag to use SST data
!
!-------------------------------------------------------------------------------
END TYPE DATA_SEAFLUX_t

TYPE(DATA_SEAFLUX_t), ALLOCATABLE, TARGET, SAVE :: DATA_SEAFLUX_MODEL(:)

INTEGER, POINTER :: NTIME=>NULL()
!$OMP THREADPRIVATE(NTIME)
REAL, POINTER, DIMENSION(:,:)    :: XDATA_SST=>NULL()
!$OMP THREADPRIVATE(XDATA_SST)
TYPE (DATE_TIME), POINTER, DIMENSION(:)  :: TDATA_SST=>NULL()
!$OMP THREADPRIVATE(TDATA_SST)
LOGICAL, POINTER :: LSST_DATA=>NULL()
!$OMP THREADPRIVATE(LSST_DATA)

CONTAINS

SUBROUTINE DATA_SEAFLUX_GOTO_MODEL(KFROM, KTO, LKFROM)
LOGICAL, INTENT(IN) :: LKFROM
INTEGER, INTENT(IN) :: KFROM, KTO
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
! Save current state for allocated arrays
IF (LKFROM) THEN
DATA_SEAFLUX_MODEL(KFROM)%XDATA_SST=>XDATA_SST
DATA_SEAFLUX_MODEL(KFROM)%TDATA_SST=>TDATA_SST
ENDIF
!
! Current model is set to model KTO
IF (LHOOK) CALL DR_HOOK('MODD_DATA_SEAFLUX_N:DATA_SEAFLUX_GOTO_MODEL',0,ZHOOK_HANDLE)
NTIME=>DATA_SEAFLUX_MODEL(KTO)%NTIME
XDATA_SST=>DATA_SEAFLUX_MODEL(KTO)%XDATA_SST
TDATA_SST=>DATA_SEAFLUX_MODEL(KTO)%TDATA_SST
LSST_DATA=>DATA_SEAFLUX_MODEL(KTO)%LSST_DATA
IF (LHOOK) CALL DR_HOOK('MODD_DATA_SEAFLUX_N:DATA_SEAFLUX_GOTO_MODEL',1,ZHOOK_HANDLE)

END SUBROUTINE DATA_SEAFLUX_GOTO_MODEL

SUBROUTINE DATA_SEAFLUX_ALLOC(KMODEL)
INTEGER, INTENT(IN) :: KMODEL
INTEGER :: J
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK("MODD_DATA_SEAFLUX_N:DATA_SEAFLUX_ALLOC",0,ZHOOK_HANDLE)
ALLOCATE(DATA_SEAFLUX_MODEL(KMODEL))
DO J=1,KMODEL
  NULLIFY(DATA_SEAFLUX_MODEL(J)%XDATA_SST)
ENDDO
DATA_SEAFLUX_MODEL(:)%NTIME=0
DATA_SEAFLUX_MODEL(:)%LSST_DATA=.FALSE.
IF (LHOOK) CALL DR_HOOK("MODD_DATA_SEAFLUX_N:DATA_SEAFLUX_ALLOC",1,ZHOOK_HANDLE)
END SUBROUTINE DATA_SEAFLUX_ALLOC

SUBROUTINE DATA_SEAFLUX_DEALLO
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK("MODD_DATA_SEAFLUX_N:DATA_SEAFLUX_DEALLO",0,ZHOOK_HANDLE)
IF (ALLOCATED(DATA_SEAFLUX_MODEL)) DEALLOCATE(DATA_SEAFLUX_MODEL)
IF (LHOOK) CALL DR_HOOK("MODD_DATA_SEAFLUX_N:DATA_SEAFLUX_DEALLO",1,ZHOOK_HANDLE)
END SUBROUTINE DATA_SEAFLUX_DEALLO

END MODULE MODD_DATA_SEAFLUX_n
