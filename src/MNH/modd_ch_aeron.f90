!ORILAM_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!ORILAM_LIC This is part of the ORILAM software governed by the CeCILL-C licence
!ORILAM_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!ORILAM_LIC for details.
!!     #####################
       MODULE MODD_CH_AERO_n
!!     #####################
!!
!!     PURPOSE
!!     -------
!!     declaration of variables and types for the aerosol system
!!
!!     METHOD
!!     ------
!!
!!     REFERENCE
!!     ---------
!!     none
!!
!!     AUTHOR
!!     ------
!!     P. Tulet (LA)
!!
!!     MODIFICATIONS
!!     -------------
!!     (30-01-01) P.Tulet (LA) * modifications for secondary biogenics aerosols
!!
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
USE MODD_PARAMETERS, ONLY: JPMODELMAX
!
IMPLICIT NONE
!
TYPE CH_AERO_t
  !
  !* normalisation parameters
  !
  REAL, DIMENSION(:,:),       POINTER :: XN0=>NULL()       ! Number concentration
  REAL, DIMENSION(:,:),       POINTER :: XRG0=>NULL()      ! Geometric mean size
  REAL, DIMENSION(:,:),       POINTER :: XSIG0=>NULL()     ! Dispersion ln(sigma)
  REAL, DIMENSION(:,:,:,:),   POINTER :: XN3D=>NULL()      ! Number concentration
  REAL, DIMENSION(:,:,:,:),   POINTER :: XRG3D=>NULL()     ! Geometric mean size
  REAL, DIMENSION(:,:,:,:),   POINTER :: XSIG3D=>NULL()    ! dispersion (sigma)
  REAL, DIMENSION(:,:,:,:),   POINTER :: XM3D=>NULL()      ! moments
  REAL, DIMENSION(:,:,:,:),   POINTER :: XSEDA=>NULL()     ! sedimentation
  REAL, DIMENSION(:,:,:),     POINTER :: XVDEPAERO=>NULL() ! aerosol dry deposition
  REAL, DIMENSION(:,:,:,:,:), POINTER :: XCTOTA3D=>NULL()  ! Total concentration of species
  !
  REAL, DIMENSION(:,:,:),     POINTER :: XFTEST=>NULL()
  REAL, DIMENSION(:,:,:),     POINTER :: XCTOTA=>NULL() ! Total concentration of species
                                                        ! (HNO3, ! H2SO4, NH3) present in
                                                        ! each of the aerosol mode (ug/m3)
  REAL, DIMENSION(:,:,:),     POINTER :: XCCTOT=>NULL()  ! Composition of 3rd Moment (%)
  REAL, DIMENSION(:,:),       POINTER :: XCTOTG=>NULL()  ! Total concentration of volatile
                                                         ! species (HNO3, NH3) (ug/m3) in
                                                         ! the air	
  REAL, DIMENSION(:,:,:,:),   POINTER :: XFRAC=>NULL()   ! Gas fraction into organic species
  REAL, DIMENSION(:,:,:,:),   POINTER :: XMI=>NULL()     ! Molar mass of aerosol species (g/mol)
  REAL, DIMENSION(:,:,:,:),   POINTER :: XSOLORG=>NULL() ! Solubility fraction of SOA (%) 
  REAL, DIMENSION(:,:),       POINTER :: XRHOP0=>NULL()  ! Condensed phase density (kg/m3)
  REAL, DIMENSION(:,:,:,:),   POINTER :: XRHOP3D=>NULL() ! Condensed phase density (kg/m3)
  REAL, DIMENSION(:),         POINTER :: XLAMBDA=>NULL() ! Mean free path of background
                                              ! gas molecules
  REAL, DIMENSION(:),         POINTER :: XMU=>NULL()          ! gas viscosity (kg/(ms))
  REAL, DIMENSION(:,:,:),     POINTER :: XJNUC=>NULL()        ! nucleation rate (molec.cm-3.s-1)
  REAL, DIMENSION(:,:,:),     POINTER :: XJ2RAT=>NULL()       ! particle formation rate for 2 nm
  REAL, DIMENSION(:,:,:),     POINTER :: XCONC_MASS=>NULL()   ! available mass (ug.m-3)
  REAL, DIMENSION(:,:,:),     POINTER :: XCOND_MASS_I=>NULL() ! condensated mass mode i (ug.m-3)
  REAL, DIMENSION(:,:,:),     POINTER :: XCOND_MASS_J=>NULL() ! condensated mass mode j (ug.m-3)
  REAL, DIMENSION(:,:,:),     POINTER :: XNUCL_MASS=>NULL()   ! nucleation mass (ug.m-3)
  !
  REAL, DIMENSION(:,:,:,:),   POINTER :: XMBEG=>NULL()
  REAL, DIMENSION(:,:,:,:),   POINTER :: XMINT=>NULL()
  REAL, DIMENSION(:,:,:,:),   POINTER :: XMEND=>NULL()
  !
  REAL, DIMENSION(:,:,:,:),   POINTER :: XDMINTRA=>NULL()
  REAL, DIMENSION(:,:,:,:),   POINTER :: XDMINTER=>NULL()
  REAL, DIMENSION(:,:,:,:),   POINTER :: XDMCOND=>NULL()
  REAL, DIMENSION(:,:,:,:),   POINTER :: XDMNUCL=>NULL()
  REAL, DIMENSION(:,:,:,:),   POINTER :: XDMMERG=>NULL()
  !
  !* Growth parameters
  !
  REAL, DIMENSION(:,:), POINTER :: XOM=>NULL()
  !
  !* Nucleation/cond. growth parameters
  !
  REAL, DIMENSION(:), POINTER :: XSO4RAT=>NULL()
                                ! Rate of gas phase production of
                                ! sulfuric acid (molec./cm3/s)
  !
  LOGICAL       :: GSEDFIX = .TRUE. ! flag used in CH_AER_SEDIM_n routine
  !
END TYPE CH_AERO_t
!
TYPE(CH_AERO_t), DIMENSION(JPMODELMAX), TARGET, SAVE :: CH_AERO_MODEL
!
REAL, DIMENSION(:,:),       POINTER :: XN0=>NULL()
REAL, DIMENSION(:,:),       POINTER :: XRG0=>NULL()
REAL, DIMENSION(:,:),       POINTER :: XSIG0=>NULL()
REAL, DIMENSION(:,:,:,:),   POINTER :: XN3D=>NULL()
REAL, DIMENSION(:,:,:,:),   POINTER :: XRG3D=>NULL()
REAL, DIMENSION(:,:,:,:),   POINTER :: XSIG3D=>NULL()
REAL, DIMENSION(:,:,:,:),   POINTER :: XM3D=>NULL()
REAL, DIMENSION(:,:,:,:),   POINTER :: XSEDA=>NULL()
REAL, DIMENSION(:,:,:),     POINTER :: XVDEPAERO=>NULL()
REAL, DIMENSION(:,:,:,:,:), POINTER :: XCTOTA3D=>NULL()
REAL, DIMENSION(:,:,:),     POINTER :: XFTEST=>NULL()
REAL, DIMENSION(:,:,:),     POINTER :: XCTOTA=>NULL()
REAL, DIMENSION(:,:,:),     POINTER :: XCCTOT=>NULL()
REAL, DIMENSION(:,:),       POINTER :: XCTOTG=>NULL()
REAL, DIMENSION(:,:,:,:),   POINTER :: XFRAC=>NULL()
REAL, DIMENSION(:,:,:,:),   POINTER :: XMI=>NULL()
REAL, DIMENSION(:,:,:,:),   POINTER :: XSOLORG=>NULL()
REAL, DIMENSION(:,:),       POINTER :: XRHOP0=>NULL()
REAL, DIMENSION(:,:,:,:),   POINTER :: XRHOP3D=>NULL()
REAL, DIMENSION(:),         POINTER :: XLAMBDA=>NULL()
REAL, DIMENSION(:),         POINTER :: XMU=>NULL()
REAL, DIMENSION(:,:),       POINTER :: XOM=>NULL()
REAL, DIMENSION(:),         POINTER :: XSO4RAT=>NULL()
LOGICAL,                    POINTER :: GSEDFIX=>NULL()
REAL, DIMENSION(:,:,:),     POINTER :: XJNUC=>NULL()
REAL, DIMENSION(:,:,:),     POINTER :: XJ2RAT=>NULL()
REAL, DIMENSION(:,:,:),     POINTER :: XCONC_MASS=>NULL() ! Available mass (ug.m-3)
REAL, DIMENSION(:,:,:),     POINTER :: XCOND_MASS_I=>NULL() ! Condensated mass mode i (ug.m-3)
REAL, DIMENSION(:,:,:),     POINTER :: XCOND_MASS_J=>NULL() ! Condensated mass mode j (ug.m-3)
REAL, DIMENSION(:,:,:),     POINTER :: XNUCL_MASS=>NULL() ! Nucleation mass (ug.m-3)
REAL, DIMENSION(:,:,:,:),   POINTER :: XMBEG=>NULL()
REAL, DIMENSION(:,:,:,:),   POINTER :: XMINT=>NULL()
REAL, DIMENSION(:,:,:,:),   POINTER :: XMEND=>NULL()
REAL, DIMENSION(:,:,:,:),   POINTER :: XDMINTRA=>NULL()
REAL, DIMENSION(:,:,:,:),   POINTER :: XDMINTER=>NULL()
REAL, DIMENSION(:,:,:,:),   POINTER :: XDMCOND=>NULL()
REAL, DIMENSION(:,:,:,:),   POINTER :: XDMNUCL=>NULL()
REAL, DIMENSION(:,:,:,:),   POINTER :: XDMMERG=>NULL()
!
CONTAINS
!
SUBROUTINE CH_AERO_GOTO_MODEL(KFROM, KTO)
INTEGER, INTENT(IN) :: KFROM, KTO
!
! Save current state for allocated arrays
CH_AERO_MODEL(KFROM)%XN0=>XN0
CH_AERO_MODEL(KFROM)%XRG0=>XRG0
CH_AERO_MODEL(KFROM)%XSIG0=>XSIG0
CH_AERO_MODEL(KFROM)%XN3D=>XN3D
CH_AERO_MODEL(KFROM)%XRG3D=>XRG3D
CH_AERO_MODEL(KFROM)%XSIG3D=>XSIG3D
CH_AERO_MODEL(KFROM)%XM3D=>XM3D
CH_AERO_MODEL(KFROM)%XSEDA=>XSEDA
CH_AERO_MODEL(KFROM)%XVDEPAERO=>XVDEPAERO
CH_AERO_MODEL(KFROM)%XCTOTA3D=>XCTOTA3D
CH_AERO_MODEL(KFROM)%XFTEST=>XFTEST
CH_AERO_MODEL(KFROM)%XCTOTA=>XCTOTA
CH_AERO_MODEL(KFROM)%XCCTOT=>XCCTOT
CH_AERO_MODEL(KFROM)%XCTOTG=>XCTOTG
CH_AERO_MODEL(KFROM)%XFRAC=>XFRAC
CH_AERO_MODEL(KFROM)%XMI=>XMI
CH_AERO_MODEL(KFROM)%XSOLORG=>XSOLORG
CH_AERO_MODEL(KFROM)%XRHOP0=>XRHOP0
CH_AERO_MODEL(KFROM)%XRHOP3D=>XRHOP3D
CH_AERO_MODEL(KFROM)%XLAMBDA=>XLAMBDA
CH_AERO_MODEL(KFROM)%XMU=>XMU
CH_AERO_MODEL(KFROM)%XOM=>XOM
CH_AERO_MODEL(KFROM)%XSO4RAT=>XSO4RAT
CH_AERO_MODEL(KFROM)%XJNUC=>XJNUC
CH_AERO_MODEL(KFROM)%XJ2RAT=>XJ2RAT
CH_AERO_MODEL(KFROM)%XCONC_MASS=>XCONC_MASS
CH_AERO_MODEL(KFROM)%XCOND_MASS_I=>XCOND_MASS_I
CH_AERO_MODEL(KFROM)%XCOND_MASS_J=>XCOND_MASS_J
CH_AERO_MODEL(KFROM)%XNUCL_MASS=>XNUCL_MASS
CH_AERO_MODEL(KFROM)%XMBEG=>XMBEG
CH_AERO_MODEL(KFROM)%XMINT=>XMINT
CH_AERO_MODEL(KFROM)%XMEND=>XMEND
CH_AERO_MODEL(KFROM)%XDMINTRA=>XDMINTRA
CH_AERO_MODEL(KFROM)%XDMINTER=>XDMINTER
CH_AERO_MODEL(KFROM)%XDMCOND=>XDMCOND
CH_AERO_MODEL(KFROM)%XDMNUCL=>XDMNUCL
CH_AERO_MODEL(KFROM)%XDMMERG=>XDMMERG
!
! Current model is set to model KTO
XN0=>CH_AERO_MODEL(KTO)%XN0
XRG0=>CH_AERO_MODEL(KTO)%XRG0
XSIG0=>CH_AERO_MODEL(KTO)%XSIG0
XN3D=>CH_AERO_MODEL(KTO)%XN3D
XRG3D=>CH_AERO_MODEL(KTO)%XRG3D
XSIG3D=>CH_AERO_MODEL(KTO)%XSIG3D
XM3D=>CH_AERO_MODEL(KTO)%XM3D
XSEDA=>CH_AERO_MODEL(KTO)%XSEDA
XVDEPAERO=>CH_AERO_MODEL(KTO)%XVDEPAERO
XCTOTA3D=>CH_AERO_MODEL(KTO)%XCTOTA3D
XFTEST=>CH_AERO_MODEL(KTO)%XFTEST
XCTOTA=>CH_AERO_MODEL(KTO)%XCTOTA
XCCTOT=>CH_AERO_MODEL(KTO)%XCCTOT
XCTOTG=>CH_AERO_MODEL(KTO)%XCTOTG
XFRAC=>CH_AERO_MODEL(KTO)%XFRAC
XMI=>CH_AERO_MODEL(KTO)%XMI
XSOLORG=>CH_AERO_MODEL(KTO)%XSOLORG
XRHOP0=>CH_AERO_MODEL(KTO)%XRHOP0
XRHOP3D=>CH_AERO_MODEL(KTO)%XRHOP3D
XLAMBDA=>CH_AERO_MODEL(KTO)%XLAMBDA
XMU=>CH_AERO_MODEL(KTO)%XMU
XOM=>CH_AERO_MODEL(KTO)%XOM
XSO4RAT=>CH_AERO_MODEL(KTO)%XSO4RAT
GSEDFIX=>CH_AERO_MODEL(KTO)%GSEDFIX
XJNUC=>CH_AERO_MODEL(KTO)%XJNUC
XJ2RAT=>CH_AERO_MODEL(KTO)%XJ2RAT
XCONC_MASS=>CH_AERO_MODEL(KTO)%XCONC_MASS
XCOND_MASS_I=>CH_AERO_MODEL(KTO)%XCOND_MASS_I
XCOND_MASS_J=>CH_AERO_MODEL(KTO)%XCOND_MASS_J
XNUCL_MASS=>CH_AERO_MODEL(KTO)%XNUCL_MASS
XMBEG=>CH_AERO_MODEL(KTO)%XMBEG
XMINT=>CH_AERO_MODEL(KTO)%XMINT
XMEND=>CH_AERO_MODEL(KTO)%XMEND
XDMINTRA=>CH_AERO_MODEL(KTO)%XDMINTRA
XDMINTER=>CH_AERO_MODEL(KTO)%XDMINTER
XDMCOND=>CH_AERO_MODEL(KTO)%XDMCOND
XDMNUCL=>CH_AERO_MODEL(KTO)%XDMNUCL
XDMMERG=>CH_AERO_MODEL(KTO)%XDMMERG
END SUBROUTINE CH_AERO_GOTO_MODEL
!
END MODULE MODD_CH_AERO_n
