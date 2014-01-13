!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     ######################
      MODULE MODD_ISBA_PAR
!     ######################
!
!!****  *MODD_ISBA_PAR* - declaration of ISBA parameters
!!
!!    PURPOSE
!!    -------
!       The purpose of this declarative module is to specify  the 
!     parameters related to the surface parameterization ISBA. 
!
!!
!!      
!!
!!    AUTHOR
!!    ------
!!	S. Belair   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original       29/04/95                      
!!      (V.Masson)     05/10/98+ add XCDZ0EFF, XRHOSMIN, XRHOSMAX
!!      (V.Masson)     15/03/99 add number of layers
!!      (A.Boone)      02/05/02 add ISBA-ES parameters
!!      (A.Boone)      21/11/11 add Rsmax
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
IMPLICIT NONE
!
!------------------------------------------------------------------------------
! Vegetation: (for additional parameters related to vegetation, see
! modd_co2v_par.f90)
!------------------------------------------------------------------------------
!
! vegetation emissivity
!
REAL, PARAMETER       :: XEMISVEG = 0.97
!
! drag coefficient in z0eff computation
!
REAL, PARAMETER       :: XCDZ0EFF = 0.8
!
! minimum vegetation fraction (for C3 grassland: for the case with large 
! VEG and low LAI, such as wintertime)
!
REAL, PARAMETER       :: XVEGMIN   = 0.95
!
! Maximum stomatal resistance (s m-1)
!
REAL, PARAMETER       :: XRS_MAX   = 5000. 
!
!--------------------------------------------------------------------------------
! Soil:
!--------------------------------------------------------------------------------
!                        
! Caracteristic time for ice in force-restore (s)
!
REAL, PARAMETER       :: XTAU_ICE = 3300.
!                        
! Bare soil emissivity
!
REAL, PARAMETER       :: XEMISSOIL = 0.94
!                        
! Minimum allowable volumetric liquid water content of soil
!
REAL, PARAMETER       :: XWGMIN   = 0.001   ! (m3 m-3)
!
! Peters-Lidard et al. (JAS, 1998) from method of Johanssen (1975)
! thermal conductivity (option) parameters:
!
REAL, PARAMETER       :: XSPHSOIL  = 733.   ! J/(kg K) Soil specific heat
REAL, PARAMETER       :: XDRYWGHT  = 2700.0 ! kg/m3    Soil solids dry weight
REAL, PARAMETER       :: XCONDQRTZ = 7.7    ! W/(m K)  Quartz thermal conductivity
REAL, PARAMETER       :: XCONDOTH1 = 2.0    ! W/(m K)  Other thermal conductivity
REAL, PARAMETER       :: XCONDOTH2 = 3.0    ! W/(m K)  Other thermal conductivity
REAL, PARAMETER       :: XCONDWTR  = 0.57   ! W/(m K)  Water thermal conductivity
!
!--------------------------------------------------------------------------------
! Vegetation radiative properties
!--------------------------------------------------------------------------------
!                        
! Wavelength between near-infra-red and visible parts of the solar spectra
!
REAL, PARAMETER       :: XRED_EDGE = 0.0000007  ! (m)   0.7 micro-m
!
!                        
! Wavelength between visible and UV parts of the solar spectra
!
REAL, PARAMETER       :: XUV_EDGE  = 0.00000025 ! (m)   0.1 micro-m
!
!--------------------------------------------------------------------------------
! Soil geometry if DF option
!--------------------------------------------------------------------------------
!
INTEGER,                      PARAMETER :: NOPTIMLAYER=14
REAL, DIMENSION(NOPTIMLAYER), PARAMETER :: XOPTIMGRID = & 
      (/0.01,0.04,0.10,0.20,0.40,0.60,0.80,1.00,1.50,2.00,3.00,5.00,8.00,12.0/)
!
!--------------------------------------------------------------------------------
!
END MODULE MODD_ISBA_PAR












