!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! MASDEV4_7 modd 2006/05/18 13:07:25
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
!!--------------------------------------------------------------------
!!     DECLARATIONS
!!     ------------

  IMPLICIT NONE

  REAL,SAVE,DIMENSION(6,10,8,6,13) ::POLYTAU
  REAL,SAVE,DIMENSION(6,10,8,6,13) ::POLYSSA
  REAL,SAVE,DIMENSION(6,10,8,6,13) ::POLYG

END MODULE MODD_AEROSET
