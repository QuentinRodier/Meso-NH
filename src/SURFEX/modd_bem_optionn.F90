!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ################
      MODULE MODD_BEM_OPTION_n
!     ################
!
!!****  *MODD_BEM_n - declaration of parameters and option for BEM
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
!!      B. Bueno   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original       10/2010
!!      G. Pigeon      06/2011 add LSHAD_DAY
!!      G. Pigeon      07/2011 add LNATVENT_NIGHT
!!      G. Pigeon      08/2011 change from MODD_BLD -> MODD_BEM
!!      G. Pigeon      10/2011 add indoor relative surf. and view factors
!!      G. Pigeon      09/2012 add TRAN_WIN
!!      G. Pigeon      10/2012 add XF_WIN_WIN
!!      V. Masson      06/2013 splits module in two
!!	M. Goret       02/2017 add variables for CO2 flux calculation
!!      M. Goret       09/2017 add HHV over LHV for calculation of latent energy waste by heating
!
!*       0.   DECLARATIONS
!             ------------
!
USE MODD_SURF_PAR,   ONLY: XUNDEF
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE

TYPE BEM_OPTIONS_t
! BLD scheme option
!
! Number of layers
!
  INTEGER                       :: NFLOOR_LAYER   ! number of layers in walls
  INTEGER                       :: NMASS_LAYER    ! number of layers in mass
  INTEGER                       :: NBEMCOMP       ! number of compartments in bem
  CHARACTER(LEN=6)              :: CCOOL_COIL    ! type of cooling coil
  CHARACTER(LEN=6)              :: CHEAT_COIL    ! type of heating coil
  LOGICAL                       :: LAUTOSIZE     ! Flag to activate autosize calculations
  REAL                          :: XCF_CO2_ELEC    ! Conversion factor CO2/electricity (Kg/J)
  REAL                          :: XCF_CO2_GAS     ! Conversion factor CO2/gas (Kg/J)
  REAL                          :: XCF_CO2_FUEL    ! Conversion factor CO2/fuel (Kg/J)
  REAL                          :: XCF_CO2_OTHER   ! Conversion factor CO2/other source(Kg/J) 
  REAL                          :: XEFF_HEAT_ELEC  ! Efficiency of the electric heating
  REAL                          :: XEFF_HEAT_GAS   ! Efficiency of the gas heating
  REAL                          :: XEFF_HEAT_FUEL  ! Efficiency of the fuel heating
  REAL                          :: XEFF_HEAT_OTHER ! Efficiency of other heating
  REAL                          :: XLHV_HHV_ELEC   ! LHV over HHV of the electric heating
  REAL                          :: XLHV_HHV_GAS    ! LHV over HHV of the gas heating
  REAL                          :: XLHV_HHV_FUEL   ! LHV over HHV of the fuel heating
  REAL                          :: XLHV_HHV_OTHER  ! LHV over HHV of other heating

!
END TYPE BEM_OPTIONS_t
!


CONTAINS

!




SUBROUTINE BEM_OPTIONS_INIT(YBEM_OPTIONS)
TYPE(BEM_OPTIONS_t), INTENT(INOUT) :: YBEM_OPTIONS
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK("MODD_BEM_N:BEM_OPTIONS_INIT",0,ZHOOK_HANDLE)
YBEM_OPTIONS%NFLOOR_LAYER = 0
YBEM_OPTIONS%CCOOL_COIL   = '      '
YBEM_OPTIONS%CHEAT_COIL   = '      '
YBEM_OPTIONS%LAUTOSIZE    = .FALSE.
!
YBEM_OPTIONS%XEFF_HEAT_ELEC = 1
YBEM_OPTIONS%XEFF_HEAT_GAS  = 0.7
YBEM_OPTIONS%XEFF_HEAT_FUEL = 0.7    
YBEM_OPTIONS%XEFF_HEAT_OTHER= 0.7
!
YBEM_OPTIONS%XLHV_HHV_ELEC = 1.
YBEM_OPTIONS%XLHV_HHV_GAS  = 1./1.11
YBEM_OPTIONS%XLHV_HHV_FUEL = 1./1.07    
YBEM_OPTIONS%XLHV_HHV_OTHER= 1./1.11
! from https://www.picbleu.fr/page/pouvoir-calorifique-des-combustibles-energies-en-pcs-et-pci
!
YBEM_OPTIONS%XCF_CO2_ELEC  = 0
YBEM_OPTIONS%XCF_CO2_GAS   = 57E-9
YBEM_OPTIONS%XCF_CO2_FUEL  = 75E-9
YBEM_OPTIONS%XCF_CO2_OTHER = 92E-9
!from ADEME
!see http://jean-dupuy.entmip.fr/lectureFichiergw.do?ID_FICHIER=6323
!see http://www.bilans-ges.ademe.fr/ for other values
!
IF (LHOOK) CALL DR_HOOK("MODD_BEM_N:BEM_OPTIONS_INIT",1,ZHOOK_HANDLE)
END SUBROUTINE BEM_OPTIONS_INIT


!----------------------------------------------------------------------------
!
END MODULE MODD_BEM_OPTION_n
