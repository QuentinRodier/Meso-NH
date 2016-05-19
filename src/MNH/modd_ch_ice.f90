!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source: /home//MESONH/MNH-V4-8-2B_gitOK/src/MNH/modd_ch_ice.f90
!-----------------------------------------------------------------
!-----------------------------------------------------------------
!-----------------------------------------------------------------
!!     ##################
       MODULE MODD_CH_ICE
!!     ##################
!!
!!     PURPOSE
!!     -------
!!
!         Declaration of variables for the ice phase chemical species
!      coefficients de retention
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
!!     Maud Leriche (LA)
!!
!!
!!     MODIFICATIONS
!!     -------------
!!    Original 15/07/10
!!
!!--------------------------------------------------------------------
!!     DECLARATIONS
!!     ------------
!
IMPLICIT NONE
!
! Retention coefficients
REAL, PARAMETER :: XRETNA=1.    ! strong acid as nitric acid
REAL, PARAMETER :: XRETSU=0.02  ! moderatly soluble as SO2
REAL, PARAMETER :: XRETHP=0.64  ! soluble as hydrogen peroxide
REAL, PARAMETER :: XRETDF=0.    ! default value = degassing

END MODULE MODD_CH_ICE
