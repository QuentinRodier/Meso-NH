!MNH_LIC Copyright 2021-2023 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!     ############################
      MODULE MODD_ALLPROFILER_n
!     ############################
!
!!****  *MODD_PROFILER* - declaration of profilers
!!
!!    PURPOSE
!!    -------
!       The purpose of this declarative module is to define
!      the different profilers.
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
!!	E. Jezequel   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/06/21
!  P. Wautelet     04/2022: restructure stations for better performance, reduce memory usage and correct some problems/bugs
!  P. Wautelet  27/04/2022: copied from modd_allstationn.f90
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
!
USE MODD_PARAMETERS, ONLY: JPMODELMAX, NFILENAMELGTMAX, NSENSORNAMELGTMAX

IMPLICIT NONE

PRIVATE

PUBLIC :: NNUMB_PROF, XSTEP_PROF, XX_PROF, XY_PROF, XLAT_PROF, XLON_PROF, XZ_PROF
PUBLIC :: CNAME_PROF, CFILE_PROF, LDIAG_SURFRAD_PROF

PUBLIC :: ALLPROFILER_GOTO_MODEL


INTEGER, PUBLIC, PARAMETER :: NNOCSVPROFILERMAX = 100

TYPE ALLPROFILER_t
!
!-------------------------------------------------------------------------------------------
!
!
  INTEGER                          :: NNUMB_PROF  !Number of stations as defined in namelist
  REAL,                             DIMENSION(NNOCSVPROFILERMAX) :: XX_PROF, XY_PROF, XZ_PROF, XLAT_PROF, XLON_PROF
  CHARACTER(LEN=NSENSORNAMELGTMAX), DIMENSION(NNOCSVPROFILERMAX) :: CNAME_PROF
  CHARACTER(LEN=NFILENAMELGTMAX)   :: CFILE_PROF
  REAL                             :: XSTEP_PROF
  LOGICAL                          :: LDIAG_SURFRAD_PROF
  !
!
END TYPE ALLPROFILER_t

TYPE(ALLPROFILER_t), DIMENSION(JPMODELMAX), TARGET, SAVE :: ALLPROFILER_MODEL

INTEGER, POINTER                             :: NNUMB_PROF=>NULL()
REAL, POINTER                                :: XSTEP_PROF=>NULL()
REAL, DIMENSION(:), POINTER                  :: XX_PROF=>NULL()
REAL, DIMENSION(:), POINTER                  :: XY_PROF=>NULL()
REAL, DIMENSION(:), POINTER                  :: XLAT_PROF=>NULL()
REAL, DIMENSION(:), POINTER                  :: XLON_PROF=>NULL()
REAL, DIMENSION(:), POINTER                  :: XZ_PROF=>NULL()
CHARACTER (LEN=NSENSORNAMELGTMAX),DIMENSION(:), POINTER :: CNAME_PROF=>NULL()
CHARACTER (LEN=NFILENAMELGTMAX),POINTER      :: CFILE_PROF=>NULL()
LOGICAL, POINTER                             :: LDIAG_SURFRAD_PROF => NULL()
CONTAINS

SUBROUTINE ALLPROFILER_GOTO_MODEL(KFROM, KTO)
INTEGER, INTENT(IN) :: KFROM, KTO
!
! Save current state for allocated arrays
!
! Current model is set to model KTO

NNUMB_PROF    =>ALLPROFILER_MODEL(KTO)%NNUMB_PROF
XSTEP_PROF    =>ALLPROFILER_MODEL(KTO)%XSTEP_PROF
XX_PROF       =>ALLPROFILER_MODEL(KTO)%XX_PROF
XY_PROF       =>ALLPROFILER_MODEL(KTO)%XY_PROF
XZ_PROF       =>ALLPROFILER_MODEL(KTO)%XZ_PROF
XLAT_PROF     =>ALLPROFILER_MODEL(KTO)%XLAT_PROF
XLON_PROF     =>ALLPROFILER_MODEL(KTO)%XLON_PROF
CNAME_PROF    =>ALLPROFILER_MODEL(KTO)%CNAME_PROF
CFILE_PROF    =>ALLPROFILER_MODEL(KTO)%CFILE_PROF
LDIAG_SURFRAD_PROF =>ALLPROFILER_MODEL(KTO)%LDIAG_SURFRAD_PROF
END SUBROUTINE ALLPROFILER_GOTO_MODEL

END MODULE MODD_ALLPROFILER_n
