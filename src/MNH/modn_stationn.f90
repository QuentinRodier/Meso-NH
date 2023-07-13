!MNH_LIC Copyright 2020-2023 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!!
!!    #####################
      MODULE MODN_STATION_n
!!    #####################
!!
!!*** *MODN_STATION*
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
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
USE MODD_ALLSTATION_n, ONLY:&
        NNOCSVSTATIONMAX                ,&
        NNUMB_STAT_n    =>NNUMB_STAT    ,&
        XSTEP_STAT_n    =>XSTEP_STAT    ,&
        XX_STAT_n       =>XX_STAT       ,&
        XY_STAT_n       =>XY_STAT       ,&
        XLAT_STAT_n     =>XLAT_STAT     ,&
        XLON_STAT_n     =>XLON_STAT     ,&
        XZ_STAT_n       =>XZ_STAT       ,&
        CNAME_STAT_n    =>CNAME_STAT    ,&
        CFILE_STAT_n    =>CFILE_STAT    ,&
        LDIAG_SURFRAD_n =>LDIAG_SURFRAD_STAT
USE MODD_PARAMETERS, ONLY: NFILENAMELGTMAX, NSENSORNAMELGTMAX
!
!-----------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!        -----------------

IMPLICIT NONE

INTEGER                          ,SAVE:: NNUMB_STAT
REAL                             ,SAVE:: XSTEP_STAT
REAL,                              DIMENSION(NNOCSVSTATIONMAX),SAVE:: XX_STAT, XY_STAT, XZ_STAT, XLAT_STAT, XLON_STAT
CHARACTER (LEN=NSENSORNAMELGTMAX), DIMENSION(NNOCSVSTATIONMAX),SAVE:: CNAME_STAT
CHARACTER (LEN=NFILENAMELGTMAX)  ,SAVE:: CFILE_STAT              !filename
LOGICAL                          ,SAVE:: LDIAG_SURFRAD

NAMELIST /NAM_STATIONn/  &
     NNUMB_STAT, XSTEP_STAT, &
     XX_STAT,XY_STAT,XZ_STAT,&
     XLON_STAT,XLAT_STAT,&
     CNAME_STAT,&
     CFILE_STAT,LDIAG_SURFRAD
     
!
CONTAINS
!
SUBROUTINE INIT_NAM_STATIONn
  NNUMB_STAT   = NNUMB_STAT_n
  XSTEP_STAT   = XSTEP_STAT_n
  XX_STAT      = XX_STAT_n
  XY_STAT      = XY_STAT_n  
  XLAT_STAT    = XLAT_STAT_n
  XLON_STAT    = XLON_STAT_n
  XZ_STAT      = XZ_STAT_n
  CNAME_STAT   = CNAME_STAT_n
  CFILE_STAT   = CFILE_STAT_n
  LDIAG_SURFRAD= LDIAG_SURFRAD_n
END SUBROUTINE INIT_NAM_STATIONn

SUBROUTINE UPDATE_NAM_STATIONn
  NNUMB_STAT_n   = NNUMB_STAT
  XSTEP_STAT_n   = XSTEP_STAT
  XX_STAT_n      = XX_STAT
  XY_STAT_n      = XY_STAT
  XLAT_STAT_n    = XLAT_STAT
  XLON_STAT_n    = XLON_STAT
  XZ_STAT_n      = XZ_STAT
  CNAME_STAT_n   = CNAME_STAT
  CFILE_STAT_n   = CFILE_STAT
  LDIAG_SURFRAD_n= LDIAG_SURFRAD
END SUBROUTINE UPDATE_NAM_STATIONn
END MODULE MODN_STATION_n
