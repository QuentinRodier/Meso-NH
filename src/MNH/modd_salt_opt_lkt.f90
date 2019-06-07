!ORILAM_LIC Copyright 2011-2019 CNRS, Meteo-France and Universite Paul Sabatier
!ORILAM_LIC This is part of the ORILAM software governed by the CeCILL-C licence
!ORILAM_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!ORILAM_LIC for details.
!-----------------------------------------------------------------
!!     ########################
       MODULE MODD_SALT_OPT_LKT
!!     ########################
!!
!!     PURPOSE
!!     -------
!!
!! Purpose: Contains look up tables for dust optical properties
!! The parameters to be looked up are: 
!! 1) Single scattering albedo (fraction scattered) 
!! 2) Assymetry factor (=1 for all scattered forward, =-1 for all scattered backwards)
!! 3) extinction coefficient (m2/g) = surface seen by radiation per gram of dust
!! 
!! All values are pre-calculated from the SHDOM mie code compiled with pgf90 fortran compiler
!! and run on on a linux pc (SHDOM home page==>http://nit.colorado.edu/~evans/shdom.html)
!! 
!!
!!     METHOD
!!     ------
!!
!!
!!     REFERENCE
!!     ---------
!!     none
!!
!!
!!     AUTHOR
!!     ------
!!     Pierre Tulet (CNRM)
!!
!!
!!     MODIFICATIONS
!!     -------------
!!
!  P. Wautelet 07/06/2019: arrays are now ALLOCATABLE to save memory when not used
!!--------------------------------------------------------------------
!!     DECLARATIONS
!!     ------------

  IMPLICIT NONE
  PUBLIC

  INTEGER, PARAMETER    :: NMAX_RADIUS_LKT=100 !Max number of radii in look up tables
  INTEGER, PARAMETER    :: NMAX_SIGMA_LKT=20   !Max number of dispersion coeffient in lkt
  INTEGER, PARAMETER    :: NMAX_WVL_SW=6       !Max number of wavelengths in lkt

  !Declaration of the look up tables 
  REAL, SAVE, DIMENSION(:,:,:), ALLOCATABLE :: XEXT_COEFF_WVL_LKT
  REAL, SAVE, DIMENSION(:,:),   ALLOCATABLE :: XEXT_COEFF_550_LKT
  REAL, SAVE, DIMENSION(:,:,:), ALLOCATABLE :: XPIZA_LKT
  REAL, SAVE, DIMENSION(:,:,:), ALLOCATABLE :: XCGA_LKT

  !Declaration of the max and min values taken into account in the tables
  REAL, PARAMETER      :: XRADIUS_LKT_MIN = 0.01  ![um] smallest number median radius taken into account
  REAL, PARAMETER      :: XRADIUS_LKT_MAX = 30.0  ![um] largest number median radius taken into account
  REAL, PARAMETER      :: XSIGMA_LKT_MIN = 1.0    ![-] smallest dispersion coefficient taken into account
  REAL, PARAMETER      :: XSIGMA_LKT_MAX = 3.0    ![-] largest dispersion coefficient taken into account

END MODULE MODD_SALT_OPT_LKT
