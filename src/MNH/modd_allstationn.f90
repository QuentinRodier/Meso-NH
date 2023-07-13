!MNH_LIC Copyright 2021-2023 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!     ############################
      MODULE MODD_ALLSTATION_n
!     ############################
!
!!****  *MODD_STATION* - declaration of stations
!!
!!    PURPOSE
!!    -------
!       The purpose of this declarative module is to define
!      the different stations types.
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
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
!
USE MODD_PARAMETERS, ONLY: JPMODELMAX, NFILENAMELGTMAX, NSENSORNAMELGTMAX

IMPLICIT NONE

PRIVATE

PUBLIC :: NNUMB_STAT, XSTEP_STAT, XX_STAT, XY_STAT, XLAT_STAT, XLON_STAT, XZ_STAT
PUBLIC :: CNAME_STAT, CFILE_STAT, LDIAG_SURFRAD_STAT

PUBLIC :: ALLSTATION_GOTO_MODEL


INTEGER, PUBLIC, PARAMETER :: NNOCSVSTATIONMAX = 100

TYPE ALLSTATION_t
!
!-------------------------------------------------------------------------------------------
!
!
  INTEGER                          :: NNUMB_STAT  !Number of stations as defined in namelist
  REAL,                             DIMENSION(NNOCSVSTATIONMAX) :: XX_STAT, XY_STAT, XZ_STAT, XLAT_STAT, XLON_STAT
  CHARACTER(LEN=NSENSORNAMELGTMAX), DIMENSION(NNOCSVSTATIONMAX) :: CNAME_STAT
  CHARACTER(LEN=NFILENAMELGTMAX)   :: CFILE_STAT
  REAL                             :: XSTEP_STAT
  LOGICAL                          :: LDIAG_SURFRAD_STAT
  !
!
END TYPE ALLSTATION_t

TYPE(ALLSTATION_t), DIMENSION(JPMODELMAX), TARGET, SAVE :: ALLSTATION_MODEL

INTEGER, POINTER                             :: NNUMB_STAT=>NULL()
REAL, POINTER                                :: XSTEP_STAT=>NULL()
REAL, DIMENSION(:), POINTER                  :: XX_STAT=>NULL()
REAL, DIMENSION(:), POINTER                  :: XY_STAT=>NULL()
REAL, DIMENSION(:), POINTER                  :: XLAT_STAT=>NULL()
REAL, DIMENSION(:), POINTER                  :: XLON_STAT=>NULL()
REAL, DIMENSION(:), POINTER                  :: XZ_STAT=>NULL()
CHARACTER (LEN=NSENSORNAMELGTMAX),DIMENSION(:), POINTER :: CNAME_STAT=>NULL()
CHARACTER (LEN=NFILENAMELGTMAX),POINTER      :: CFILE_STAT=>NULL()
LOGICAL, POINTER                             :: LDIAG_SURFRAD_STAT=>NULL()
CONTAINS

SUBROUTINE ALLSTATION_GOTO_MODEL(KFROM, KTO)
INTEGER, INTENT(IN) :: KFROM, KTO
!
! Save current state for allocated arrays
!
! Current model is set to model KTO

NNUMB_STAT    =>ALLSTATION_MODEL(KTO)%NNUMB_STAT
XSTEP_STAT    =>ALLSTATION_MODEL(KTO)%XSTEP_STAT
XX_STAT       =>ALLSTATION_MODEL(KTO)%XX_STAT
XY_STAT       =>ALLSTATION_MODEL(KTO)%XY_STAT
XZ_STAT       =>ALLSTATION_MODEL(KTO)%XZ_STAT
XLAT_STAT     =>ALLSTATION_MODEL(KTO)%XLAT_STAT
XLON_STAT     =>ALLSTATION_MODEL(KTO)%XLON_STAT
CNAME_STAT    =>ALLSTATION_MODEL(KTO)%CNAME_STAT
CFILE_STAT    =>ALLSTATION_MODEL(KTO)%CFILE_STAT
LDIAG_SURFRAD_STAT =>ALLSTATION_MODEL(KTO)%LDIAG_SURFRAD_STAT
END SUBROUTINE ALLSTATION_GOTO_MODEL

END MODULE MODD_ALLSTATION_n
