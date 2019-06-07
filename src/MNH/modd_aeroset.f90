!ORILAM_LIC Copyright 2011-2019 CNRS, Meteo-France and Universite Paul Sabatier
!ORILAM_LIC This is part of the ORILAM software governed by the CeCILL-C licence
!ORILAM_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!ORILAM_LIC for details.
!-----------------------------------------------------------------
!!     ########################
       MODULE MODD_AEROSET
!!     ########################
!!
!!     PURPOSE
!!     -------
!!
!! Purpose: Contains look up tables for aerosol optical properties
!! The parameters to be looked up are: 
!! 1) Single scattering albedo (fraction scattered) 
!! 2) Assymetry factor (=1 for all scattered forward, =-1 for all scattered backwards)
!! 3) extinction coefficient (m2/g) = surface seen by radiation per gram of dust
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
!!     Benjamin Aouizerats (CNRM)
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

  REAL,SAVE,DIMENSION(:,:,:,:,:),ALLOCATABLE ::POLYTAU
  REAL,SAVE,DIMENSION(:,:,:,:,:),ALLOCATABLE ::POLYSSA
  REAL,SAVE,DIMENSION(:,:,:,:,:),ALLOCATABLE ::POLYG

END MODULE MODD_AEROSET
