!ORILAM_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!ORILAM_LIC This is part of the ORILAM software governed by the CeCILL-C licence
!ORILAM_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!ORILAM_LIC for details.
!-----------------------------------------------------------------
!!   #####################
     MODULE MODI_CH_ORILAM
!!   ##################### 
!!
INTERFACE
!!
SUBROUTINE CH_ORILAM(PAERO, PCHEM, PM, PLNSIG, PRG, PN, PCTOTG, PCTOTA, &
                     PCCTOT, PDTACT, PSEDA,                             &
                     PRHOP, PSO4RAT,                                    &
                     PRV, PDENAIR, PPRESSURE, PTEMP, PRC, PFRAC, PMI,   &
                     PTIME, GSCHEME, PSOLORG,                           &
                     PJNUC,PJ2RAT,PMBEG,PMINT,PMEND,PDMINTRA,           &
                     PDMINTER,PDMCOND,PDMNUCL,PDMMERG,                  &
                     PCONC_MASS,PCOND_MASS_I,PCOND_MASS_J,PNUCL_MASS)
!!
IMPLICIT NONE
REAL,                                   INTENT(IN)    :: PDTACT, PTIME
REAL,                 DIMENSION(:,:),   INTENT(INOUT) :: PRHOP
REAL,                 DIMENSION(:),     INTENT(INOUT) :: PSO4RAT
REAL,                 DIMENSION(:),     INTENT(INOUT) :: PJNUC, PJ2RAT
REAL,                 DIMENSION(:,:),   INTENT(INOUT) :: PM
REAL,                 DIMENSION(:,:),   INTENT(INOUT) :: PLNSIG, PRG, PN
REAL,                 DIMENSION(:,:),   INTENT(INOUT) :: PCTOTG
REAL,                 DIMENSION(:,:),   INTENT(INOUT) :: PSEDA
REAL,                 DIMENSION(:,:),   INTENT(INOUT) :: PCHEM
REAL,                 DIMENSION(:,:),   INTENT(INOUT) :: PAERO
REAL,                 DIMENSION(:,:),   INTENT(INOUT) :: PFRAC
REAL,                 DIMENSION(:,:),   INTENT(INOUT) :: PMI
REAL,                 DIMENSION(:,:,:), INTENT(INOUT) :: PCTOTA, PCCTOT
REAL,                 DIMENSION(:,:),   INTENT(INOUT) :: PSOLORG
REAL,                 DIMENSION(:),     INTENT(IN)    :: PRV, PDENAIR, PPRESSURE, PTEMP, PRC
CHARACTER(LEN=10),                      INTENT(IN)    :: GSCHEME
REAL,                 DIMENSION(:,:),   INTENT(INOUT) :: PMBEG,PMINT,PMEND
REAL,                 DIMENSION(:,:),   INTENT(INOUT) :: PDMINTRA,PDMINTER,PDMCOND,PDMNUCL,PDMMERG
REAL,                 DIMENSION(:),     INTENT(INOUT) :: PCONC_MASS,PCOND_MASS_I,PCOND_MASS_J,PNUCL_MASS
!!
END SUBROUTINE CH_ORILAM
!!
END INTERFACE
!!
END MODULE MODI_CH_ORILAM
!!
!! #######################################################################
SUBROUTINE CH_ORILAM(PAERO, PCHEM, PM, PLNSIG, PRG, PN, PCTOTG, PCTOTA,  &
                     PCCTOT, PDTACT, PSEDA,                              &
                     PRHOP, PSO4RAT,                                     &
                     PRV, PDENAIR, PPRESSURE, PTEMP, PRC, PFRAC, PMI,    &
                     PTIME, GSCHEME, PSOLORG,                            &
                     PJNUC, PJ2RAT, PMBEG, PMINT, PMEND, PDMINTRA,       &
                     PDMINTER, PDMCOND, PDMNUCL, PDMMERG,                &
                     PCONC_MASS, PCOND_MASS_I, PCOND_MASS_J, PNUCL_MASS)
!! #######################################################################
!!
!!    PURPOSE
!!    -------
!!    ORILAM aerosol Code
!!
!!    REFERENCE
!!    ---------
!!    P. Tulet, V. Crassier, F. Cousin, K. Suhre, R. Rosset, jgr
!!    ORILAM, A three moment lognormal aerosol scheme for mesoscale atmospheric 
!!    model.
!!    On-line coupling into the Meso-NH-C model and validation  on the Escompte 
!!    campaign.
!!
!!    AUTHOR
!!    ------
!!    Pierre Tulet (GMEI) and Vincent Crassier (LA)
!!
!!    MODIFICATIONS
!!    -------------
!!    Original
!!    M. Leriche (08/16) add initialization of ZMASK
!!
!!    EXTERNAL
!!    --------
!!    MODI_CH_AER_TRANS
!!    MODI_CH_AER_DRIVER
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!    MODD_CH_AEROSOL
!!
!-------------------------------------------------------------------------------
!
!*      0. DECLARATIONS
!          ------------
!
USE MODI_CH_AER_TRANS
USE MODI_CH_AER_DRIVER
!
USE MODD_CH_AEROSOL
!
IMPLICIT NONE
!
!
!*      0.1    declarations of arguments
!
REAL,                                   INTENT(IN)    :: PDTACT, PTIME
REAL,                 DIMENSION(:,:),   INTENT(INOUT) :: PM
REAL,                 DIMENSION(:,:),   INTENT(INOUT) :: PRHOP 
REAL,                 DIMENSION(:),     INTENT(INOUT) :: PSO4RAT
REAL,                 DIMENSION(:),     INTENT(INOUT) :: PJNUC, PJ2RAT
REAL,                 DIMENSION(:,:),   INTENT(INOUT) :: PSEDA
REAL,                 DIMENSION(:,:),   INTENT(INOUT) :: PCHEM
REAL,                 DIMENSION(:,:),   INTENT(INOUT) :: PAERO
REAL,                 DIMENSION(:,:),   INTENT(INOUT) :: PFRAC
REAL,                 DIMENSION(:,:),   INTENT(INOUT) :: PMI
REAL,                 DIMENSION(:,:),   INTENT(INOUT) :: PLNSIG, PRG, PN
REAL,                 DIMENSION(:,:),   INTENT(INOUT) :: PCTOTG
REAL,                 DIMENSION(:,:,:), INTENT(INOUT) :: PCTOTA, PCCTOT
REAL,                 DIMENSION(:,:),   INTENT(INOUT) :: PSOLORG
REAL,                 DIMENSION(:),     INTENT(IN)    :: PRV, PDENAIR, PPRESSURE, PTEMP, PRC
CHARACTER(LEN=10),                      INTENT(IN)    :: GSCHEME
REAL,                 DIMENSION(:,:),   INTENT(INOUT) :: PMBEG,PMINT,PMEND
REAL,                 DIMENSION(:,:),   INTENT(INOUT) :: PDMINTRA,PDMINTER,PDMCOND,PDMNUCL,PDMMERG
REAL,                 DIMENSION(:),     INTENT(INOUT) :: PCONC_MASS,PCOND_MASS_I,PCOND_MASS_J,PNUCL_MASS
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(SIZE(PAERO,1),JPMODE)                 :: ZMASK
REAL, DIMENSION(SIZE(PAERO,1))                        :: ZSULF
!
!-------------------------------------------------------------------------------
!
!*      1. COMPUTATION
!          -----------
!
ZMASK(:,:) = 1.
!
!*      1.1    transfer gas phase variables into aerosol variables
!
CALL CH_AER_TRANS(0, PM, PLNSIG, PRG, PN, PRHOP,PAERO, PCHEM, PCTOTG, PCTOTA, PCCTOT, &
                  PFRAC, PMI, ZMASK, GSCHEME)
!
!*      1.2    integrate aerosol variables
!
CALL CH_AER_DRIVER(PM,PLNSIG, PRG, PN,  PCTOTG, PCTOTA, PCCTOT,          &
                      PDTACT, PSEDA, PRHOP, PSO4RAT,                     &
                      PRV, PDENAIR, PPRESSURE, PTEMP, PRC, ZMASK, PTIME, &
                      PSOLORG,PJNUC,PJ2RAT,PMBEG,PMINT,PMEND,PDMINTRA,   &
                      PDMINTER,PDMCOND,PDMNUCL,PDMMERG,                  &
                      PCONC_MASS,PCOND_MASS_I,PCOND_MASS_J,PNUCL_MASS    )
!
!*      1.3    transfer aerosol variables back into gas phase variables
!
CALL CH_AER_TRANS(1, PM, PLNSIG, PRG, PN, PRHOP, PAERO, PCHEM, PCTOTG, PCTOTA, PCCTOT, &
                  PFRAC, PMI, ZMASK,GSCHEME)
!
END SUBROUTINE CH_ORILAM
