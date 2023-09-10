!MNH_LIC Copyright 2002-2022 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!     ############################
      MODULE MODD_PROFILER_n
!     ############################
!
!!****  *MODD_PROFILER* - declaration of profilers
!!
!!    PURPOSE
!!    -------
!       The purpose of this declarative module is to define
!      the different profilers types.
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      NONE 
!!
!!    REFERENCE
!!    --------- 
!!       
!!    AUTHOR
!!    ------
!!	P. Tulet   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    15/01/02
!  P. Wautelet    04/2022: restructure profilers for better performance, reduce memory usage and correct some problems/bugs
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
!
USE MODD_PARAMETERS,    ONLY: JPMODELMAX
USE MODD_TYPE_STATPROF, ONLY: TPROFILERDATA
USE MODD_SENSOR,        ONLY: TSENSORTIME

IMPLICIT NONE

PRIVATE

PUBLIC :: LPROFILER, NUMBPROFILER_LOC, TPROFILERS_TIME, TPROFILERS

PUBLIC :: PROFILER_GOTO_MODEL

TYPE PROFILER_t
!
!-------------------------------------------------------------------------------------------
!
  LOGICAL                          :: LPROFILER    ! flag to use profilers
  INTEGER                          :: NUMBPROFILER_LOC = 0 ! number of profilers on this process
!
  TYPE(TSENSORTIME) :: TPROFILERS_TIME
  TYPE(TPROFILERDATA), DIMENSION(:), POINTER :: TPROFILERS ! characteristics and records of the profilers
!
END TYPE PROFILER_t

TYPE(PROFILER_t), DIMENSION(JPMODELMAX), TARGET, SAVE :: PROFILER_MODEL

LOGICAL, POINTER :: LPROFILER=>NULL()
INTEGER, POINTER :: NUMBPROFILER_LOC=>NULL()
TYPE(TSENSORTIME),                 POINTER :: TPROFILERS_TIME => NULL()
TYPE(TPROFILERDATA), DIMENSION(:), POINTER :: TPROFILERS      => NULL()

CONTAINS

SUBROUTINE PROFILER_GOTO_MODEL(KFROM, KTO)
INTEGER, INTENT(IN) :: KFROM, KTO
!
! Save current state for allocated arrays
PROFILER_MODEL(KFROM)%TPROFILERS => TPROFILERS
!
! Current model is set to model KTO
LPROFILER        => PROFILER_MODEL(KTO)%LPROFILER
NUMBPROFILER_LOC => PROFILER_MODEL(KTO)%NUMBPROFILER_LOC
TPROFILERS_TIME  => PROFILER_MODEL(KTO)%TPROFILERS_TIME
TPROFILERS       => PROFILER_MODEL(KTO)%TPROFILERS

END SUBROUTINE PROFILER_GOTO_MODEL

END MODULE MODD_PROFILER_n
