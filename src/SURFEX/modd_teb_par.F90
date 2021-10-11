!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!####################
MODULE MODD_TEB_PAR
!####################
!
!!****  *MODD_TEB_PAR - declaration of TEB surface parameters
!!
!!    PURPOSE
!!    -------
!     Declaration of surface parameters
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      None 
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!      V. Masson *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original       11/2011
!!
!!      01/2016     K.Chancibault/A.Lemonsu    Default floor temperature for coupling with subsoil
!!      03/2017     M.Goret                    add CO2 emitted by on person
!!                                             add traffic cycle
!!      05/2017     M.Goret                    move traffic cycle to modd_teb_optionn    
!!
!
!*       0.   DECLARATIONS
!             ------------
!
!
IMPLICIT NONE
!
!-----------------------------------------------------------------------------------------------------
INTEGER, PARAMETER :: NTEB_PATCH_MAX = 9  ! maximum number of patches
REAL   , PARAMETER :: XD_FLOOR_DEF   = 0.15     ! Default floor layer thickness (m)
REAL   , PARAMETER :: XHC_FLOOR_DEF  = 2016000. ! Default heat capacity of floor layer (J/m3/K)
REAL   , PARAMETER :: XTC_FLOOR_DEF  = 1.95     ! Default thermal conductivity of floor layer (W/m/s)
REAL   , PARAMETER :: XTS_FLOOR      = 292.16   ! Default floor surface temperature (=19°C)
REAL,    PARAMETER :: XEMIS_WIN_CST  = 0.84     ! Emissivity of windows
!                                               ! from Energy Plus Engineering Reference, p219
!                                               ! see http://apps1.eere.energy.gov/buildings/energyplus/
REAL,    PARAMETER :: XHUM_CO2       =8.87E-6   ! CO2 emitted by one person (Kg/s) (Moriwaki and Kanda 2004)   
!-----------------------------------------------------------------------------------------------------
!
END MODULE MODD_TEB_PAR
