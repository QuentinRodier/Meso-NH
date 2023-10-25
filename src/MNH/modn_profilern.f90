!MNH_LIC Copyright 2020-2023 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!!
!!    #####################
      MODULE MODN_PROFILER_n
!!    #####################
!!
!!*** *MODN_PROFILER*
!!
!!    PURPOSE
!!    -------
!       Namelist to define the stations 
!!
!!**  AUTHOR
!!    ------
!!    E. Jézéquel                   *CNRM & IFPEN*
!
!!    MODIFICATIONS
!!    -------------
!!    Original 10/03/20
!  P. Wautelet    04/2022: restructure stations for better performance, reduce memory usage and correct some problems/bugs
!  P. Wautelet  27/07/2022: copied from modn_stationn.f90
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
USE MODD_ALLPROFILER_n, ONLY:&
        NNOCSVPROFILERMAX               ,&
        NNUMB_PROF_n    =>NNUMB_PROF    ,&
        XSTEP_PROF_n    =>XSTEP_PROF    ,&
        XX_PROF_n       =>XX_PROF       ,&
        XY_PROF_n       =>XY_PROF       ,&
        XLAT_PROF_n     =>XLAT_PROF     ,&
        XLON_PROF_n     =>XLON_PROF     ,&
        XZ_PROF_n       =>XZ_PROF       ,&
        CNAME_PROF_n    =>CNAME_PROF    ,&
        CFILE_PROF_n    =>CFILE_PROF    ,&
        LDIAG_SURFRAD_n =>LDIAG_SURFRAD_PROF
USE MODD_PARAMETERS, ONLY: NFILENAMELGTMAX, NSENSORNAMELGTMAX
!
!-----------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!        -----------------

IMPLICIT NONE

INTEGER                          ,SAVE:: NNUMB_PROF
REAL                             ,SAVE:: XSTEP_PROF
REAL,                              DIMENSION(NNOCSVPROFILERMAX),SAVE:: XX_PROF, XY_PROF, XZ_PROF, XLAT_PROF, XLON_PROF
CHARACTER (LEN=NSENSORNAMELGTMAX), DIMENSION(NNOCSVPROFILERMAX),SAVE:: CNAME_PROF
CHARACTER (LEN=NFILENAMELGTMAX)  ,SAVE:: CFILE_PROF              !filename
LOGICAL                          ,SAVE:: LDIAG_SURFRAD

NAMELIST /NAM_PROFILERn/  &
     NNUMB_PROF, XSTEP_PROF, &
     XX_PROF,XY_PROF,XZ_PROF,&
     XLON_PROF,XLAT_PROF,&
     CNAME_PROF,&
     CFILE_PROF, LDIAG_SURFRAD
     
!
CONTAINS
!
SUBROUTINE INIT_NAM_PROFILERn
  NNUMB_PROF   = NNUMB_PROF_n
  XSTEP_PROF   = XSTEP_PROF_n
  XX_PROF      = XX_PROF_n
  XY_PROF      = XY_PROF_n
  XLAT_PROF    = XLAT_PROF_n
  XLON_PROF    = XLON_PROF_n
  XZ_PROF      = XZ_PROF_n
  CNAME_PROF   = CNAME_PROF_n
  CFILE_PROF   = CFILE_PROF_n
  LDIAG_SURFRAD= LDIAG_SURFRAD_n
END SUBROUTINE INIT_NAM_PROFILERn

SUBROUTINE UPDATE_NAM_PROFILERn
  NNUMB_PROF_n   = NNUMB_PROF
  XSTEP_PROF_n   = XSTEP_PROF
  XX_PROF_n      = XX_PROF
  XY_PROF_n      = XY_PROF
  XLAT_PROF_n    = XLAT_PROF
  XLON_PROF_n    = XLON_PROF
  XZ_PROF_n      = XZ_PROF
  CNAME_PROF_n   = CNAME_PROF
  CFILE_PROF_n   = CFILE_PROF
  LDIAG_SURFRAD_n= LDIAG_SURFRAD
END SUBROUTINE UPDATE_NAM_PROFILERn
END MODULE MODN_PROFILER_n
