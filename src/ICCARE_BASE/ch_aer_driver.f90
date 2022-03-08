!ORILAM_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!ORILAM_LIC This is part of the ORILAM software governed by the CeCILL-C licence
!ORILAM_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!ORILAM_LIC for details.
!    #########################
     MODULE MODI_CH_AER_DRIVER
!    ######################### 
! 
INTERFACE
! 
SUBROUTINE CH_AER_DRIVER(PM, PLNSIG, PRG, PN, PCTOTG, PCTOTA,              &
                         PCCTOT, PDTACT, PSEDA,                            &
                         PRHOP, PSO4RAT,                                   &
                         PRV, PDENAIR, PPRESSURE, PTEMP, PRC, PMASK,       &
                         PTIME, PSOLORG,                                   &
                         PJNUC, PJ2RAT, PMBEG, PMINT, PMEND,               &
                         PDMINTRA, PDMINTER, PDMCOND, PDMNUCL, PDMMERG,    &
                         PCONC_MASS, PCOND_MASS_I, PCOND_MASS_J, PNUCL_MASS)
IMPLICIT NONE
REAL,                   INTENT(IN)    :: PDTACT, PTIME
REAL, DIMENSION(:,:),   INTENT(INOUT) :: PRHOP 
REAL, DIMENSION(:),     INTENT(INOUT) :: PSO4RAT
REAL, DIMENSION(:,:),   INTENT(INOUT) :: PM
REAL, DIMENSION(:,:),   INTENT(INOUT) :: PLNSIG, PRG, PN
REAL, DIMENSION(:,:),   INTENT(INOUT) :: PCTOTG
REAL, DIMENSION(:,:),   INTENT(INOUT) :: PSOLORG
REAL, DIMENSION(:,:),   INTENT(INOUT) :: PSEDA
REAL, DIMENSION(:,:),   INTENT(INOUT) :: PMASK
REAL, DIMENSION(:,:,:), INTENT(INOUT) :: PCTOTA, PCCTOT
REAL, DIMENSION(:),     INTENT(IN)    :: PRV, PDENAIR, PPRESSURE, PTEMP, PRC
REAL, DIMENSION(:),     INTENT(INOUT) :: PJNUC, PJ2RAT
REAL, DIMENSION(:,:),   INTENT(INOUT) :: PMBEG,PMINT,PMEND
REAL, DIMENSION(:,:),   INTENT(INOUT) :: PDMINTRA,PDMINTER,PDMCOND,PDMNUCL,PDMMERG
REAL, DIMENSION(:),     INTENT(INOUT) :: PCONC_MASS,PCOND_MASS_I,PCOND_MASS_J,PNUCL_MASS
END SUBROUTINE CH_AER_DRIVER
! 
END INTERFACE
! 
END MODULE MODI_CH_AER_DRIVER
! 
!#####################################################################################
SUBROUTINE CH_AER_DRIVER(PM, PLNSIG, PRG, PN, PCTOTG, PCTOTA,           &
                         PCCTOT, PDTACT, PSEDA,                         &
                         PRHOP, PSO4RAT,                                &
                         PRV, PDENAIR, PPRESSURE, PTEMP, PRC, PMASK,    &
                         PTIME, PSOLORG,                                &
                         PJNUC,PJ2RAT,PMBEG,PMINT,PMEND,                &
                         PDMINTRA,PDMINTER,PDMCOND,PDMNUCL,PDMMERG,     &
                         PCONC_MASS,PCOND_MASS_I,PCOND_MASS_J,PNUCL_MASS)
!#####################################################################################
!!
!!    PURPOSE
!!    -------
!!      Compute the right hand side of the moment equations and solve the moment equations
!!
!!    EXTERNAL
!!    --------
!!      Subroutine CH_AER_COAG         : compute coagulation moment tendency terms
!!      Subroutine CH_AER_COND         : compute condensation from CMAQ model
!!      Subroutine CH_AER_NUCL         : compute nucleation rate 
!!      Subroutine CH_AER_MODE_MERGING : adjust tendency terms in case of mode i > mode j
!!      Subroutine CH_AER_SOLV         : solve moment equations
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!      MODD_CH_AEROSOL
!!
!!    REFERENCE
!!    ---------
!!    none
!!
!!    AUTHOR
!!    ------
!!    Vincent Crassier (LA)
!!
!!    MODIFICATIONS
!!    -------------
!!       Original 
!!       M. Leriche  (??/2015) Calcul de la fraction massique entre les modes
!!       M. Leriche  (08/2016) Suppress moments index declaration already in modd_aerosol
!!       J. Pianezze (06/2018) ...
!-------------------------------------------------------------------------------
!
! *       0.     DECLARATIONS
!                ------------
!
USE MODI_CH_AER_COAG
USE MODI_CH_AER_COND
USE MODI_CH_AER_NUCL
USE MODI_CH_AER_MODE_MERGING
USE MODI_CH_AER_SOLV
!
USE MODD_CH_AEROSOL
USE MODD_CONF, ONLY : NVERB
USE MODD_CST,  ONLY : XAVOGADRO
!
IMPLICIT NONE
!
! *       0.1   declarations of arguments
!
REAL,                   INTENT(IN)    :: PDTACT, PTIME
REAL, DIMENSION(:,:),   INTENT(INOUT) :: PRHOP 
REAL, DIMENSION(:),     INTENT(INOUT) :: PSO4RAT
REAL, DIMENSION(:,:),   INTENT(INOUT) :: PM
REAL, DIMENSION(:,:),   INTENT(INOUT) :: PLNSIG, PRG, PN
REAL, DIMENSION(:,:),   INTENT(INOUT) :: PCTOTG
REAL, DIMENSION(:,:),   INTENT(INOUT) :: PSOLORG
REAL, DIMENSION(:,:),   INTENT(INOUT) :: PSEDA
REAL, DIMENSION(:,:),   INTENT(INOUT) :: PMASK
REAL, DIMENSION(:,:,:), INTENT(INOUT) :: PCTOTA, PCCTOT
REAL, DIMENSION(:),     INTENT(IN)    :: PRV, PDENAIR, PPRESSURE, PTEMP, PRC
REAL, DIMENSION(:),     INTENT(INOUT) :: PJNUC, PJ2RAT
REAL, DIMENSION(:,:),   INTENT(INOUT) :: PMBEG,PMINT,PMEND
REAL, DIMENSION(:,:),   INTENT(INOUT) :: PDMINTRA,PDMINTER,PDMCOND,PDMNUCL,PDMMERG
REAL, DIMENSION(:),     INTENT(INOUT) :: PCONC_MASS,PCOND_MASS_I,PCOND_MASS_J,PNUCL_MASS
!
! *       0.2   declarations of local variables
!
INTEGER                            :: JI, JJ
!
REAL                                :: ZGASMW              ! Molecular weight of background gas (g/mol) 
REAL, DIMENSION(SIZE(PM,1))         :: ZRH,ZPSAT           ! Relative humidity, ?
REAL, DIMENSION(SIZE(PM,1))         :: ZPKM, ZPKH2O
REAL, DIMENSION(SIZE(PM,1))         :: ZMU, ZLAMBDA
!
REAL, DIMENSION(SIZE(PM,1))          :: ZDMNDT, ZDM3DT, ZDM6DT, ZDMN3DT, ZDMN6DT
REAL, DIMENSION(SIZE(PM,1),JPMODE)   :: ZDMC0DT, ZDMC3DT, ZDMC6DT
REAL, DIMENSION(SIZE(PM,1),3*JPMODE) :: ZDMGROW
REAL, DIMENSION(SIZE(PM,1))          :: ZSULF 
!
!-------------------------------------------------------------------------------
!
! *       1.     INITIALIZATION
!                --------------
!
PDMINTRA(:,:) = 0.0
PDMINTER(:,:) = 0.0
PDMCOND(:,:)  = 0.0
PDMNUCL(:,:)  = 0.0
PDMMERG(:,:)  = 0.0
!
ZDMGROW(:,:)  = 0.0
!
ZDMC0DT(:,:)  = 0.0
ZDMC3DT(:,:)  = 0.0
ZDMC6DT(:,:)  = 0.0
!
ZDMN3DT(:)    = 0.0
ZDMN6DT(:)    = 0.0
!
! *  Compute relative humidity
!
ZPKM  (:) = 1E-3*PDENAIR(:) * XAVOGADRO / 28.9644
ZPKH2O(:) = ZPKM(:)*1.6077*PRV(:)
ZPSAT (:) = 0.611*EXP(17.2694*(PTEMP(:)-273.16)/(PTEMP(:)-35.86))
ZPSAT (:) = ZPSAT(:)*1000.
ZRH   (:) = (ZPKH2O(:)/(ZPKM(:)*1.6077))*PPRESSURE(:)/&
          & (0.622+(ZPKH2O(:)/(ZPKM(:)*1.6077)))/ZPSAT(:)
ZGASMW = 29.0
!
! *  gas viscosity
ZMU(:) = 0.003661*PTEMP(:)
ZMU(:) = 0.0066164*ZMU(:)*sqrt(ZMU(:))/(PTEMP(:)+114.d0)
! *  mean free path
ZLAMBDA(:)=ZMU(:)/PDENAIR(:)*sqrt(1.89d-4*ZGASMW/PTEMP(:))*1.e6
!
!
! [ug.m-3.s-1] = [molec.cm-3.s-1] * *XH2SO4 / (XAVOGADRO*1.E-12)
PSO4RAT(:) = PSO4RAT(:) * XH2SO4 / (XAVOGADRO*1.E-12)
!
!   ZSULF [ug.m-3.s-1] = production rate of sulfuric acid 
ZSULF(:) = PSO4RAT(:)
!
!   Stock value for diag
PCONC_MASS(:)  = ZSULF(:) * PDTACT
!
!-------------------------------------------------------
!
! *      2. COMPUTE COAGULATION TERMS
!        ------------------------------------------
!
IF (LCOAGULATION) THEN
  CALL CH_AER_COAG(PM, PLNSIG, PRG, PN, PDMINTRA, PDMINTER,  &
                   PTEMP, ZMU, ZLAMBDA, PRHOP                )
ELSE
  PDMINTRA(:,:) = 0.0
  PDMINTER(:,:) = 0.0
ENDIF
!
!-------------------------------------------------------
!
! *      3. COMPUTE NUCLEATION
!        --------------------------------------------
!
!
! *             2.0 Compute sulfuric acid concentration available for nucleation
!                   -----------------------------------------------------------
!
! dC / dt = P - Cs / time
!
CALL CH_AER_COND(PM, PLNSIG, PRG, PPRESSURE, PTEMP, &
                 ZDMC3DT, ZDMC6DT                   )
!
ZSULF(:) = ZSULF(:) / (ZDMC3DT(:,1)+ZDMC3DT(:,2)) 
!
!
!*             2.1    NUCLEATION
!                     ----------
!
!
IF (CNUCLEATION == 'NONE') THEN
  PJNUC = 0.0
ELSE
  CALL CH_AER_NUCL(ZRH,PTEMP,ZSULF,PJNUC,PJ2RAT)
END IF
!
!   Convert nucleation rate
!   [ug.m-3.s-1] = [molec.cm-3.s-1] * XH2SO4 / (XAVOGADRO*1.E-12)
!
ZDMNDT(:) = PJNUC(:) * XH2SO4 / (XAVOGADRO*1.E-12)
!
!   H2SO4 final [ug.m-3] = H2SO4 initial [ug.m-3]
!                        - H2SO4 rate consumed by nucleation [ug.m-3.s-1] * Time step [s]
!
DO JI=1, SIZE(PM(:,1))
  !
  IF ( ZDMNDT(JI) .GT. PSO4RAT(JI) ) THEN
    !
    ZDMNDT(JI) = PSO4RAT(JI)
    PJNUC (JI) = ZDMNDT (JI) / XH2SO4 * (XAVOGADRO*1.E-12)
    !
  END IF
ENDDO
!
ZSULF(:) = (PSO4RAT(:)-ZDMNDT(:)) * PDTACT
!
PNUCL_MASS(:) = ZDMNDT(:) * PDTACT
!
!
!  Update moment tendencies for nucleation
! 
PDMNUCL(:,NM0(1)) = ZDMNDT(:)/(XFAC(JP_AER_SO4)*( (XRADIUS_NUCL)**3)*EXP(4.5 * LOG(XSIGMA_NUCL)**2))
PDMNUCL(:,NM3(1)) = ZDMNDT(:)/XFAC(JP_AER_SO4) 
PDMNUCL(:,NM6(1)) = PDMNUCL(:,NM0(1))*( (XRADIUS_NUCL)**6*EXP(18.*LOG(XSIGMA_NUCL)**2))
PDMNUCL(:,NM0(2)) = 0.0
PDMNUCL(:,NM3(2)) = 0.0
PDMNUCL(:,NM6(2)) = 0.0
!
!-------------------------------------------------------
!
! *      3. COMPUTE CONDENSATION
!           --------------------
!
!
IF (LCONDENSATION) THEN
  !
  !     Update dM0_cond / dt
  PDMCOND(:,NM0(1)) = 0.0
  PDMCOND(:,NM0(2)) = 0.0
  !
  !     Update of dM3_cond/dt from new dMass_cond/dt
  !
  PDMCOND(:,NM3(1)) = (ZSULF(:)/PDTACT) / XFAC(JP_AER_SO4) * (ZDMC3DT(:,1) / (ZDMC3DT(:,1)+ZDMC3DT(:,2) ))
  PDMCOND(:,NM3(2)) = (ZSULF(:)/PDTACT) / XFAC(JP_AER_SO4) * (ZDMC3DT(:,2) / (ZDMC3DT(:,1)+ZDMC3DT(:,2) ))
  !
  !     Compute dM0_cond/dt --> usefull for calculation of dM6_cond/dt
  !
  ZDMC0DT(:,1) = PDMCOND(:,NM3(1)) / (( (PRG(:,1))**3 ) * EXP(4.5 * PLNSIG(:,1)**2))
  ZDMC0DT(:,2) = PDMCOND(:,NM3(2)) / (( (PRG(:,2))**3 ) * EXP(4.5 * PLNSIG(:,2)**2))
  !
  PDMCOND(:,NM6(1)) = ZDMC0DT(:,1) * (( (PRG(:,1))**6 ) * EXP(18. * PLNSIG(:,1)**2))
  PDMCOND(:,NM6(2)) = ZDMC0DT(:,2) * (( (PRG(:,2))**6 ) * EXP(18. * PLNSIG(:,2)**2))
  !
ELSE
  !
  ZDMC0DT(:,:) = 0.0
  ZDMC3DT(:,:) = 0.0
  ZDMC6DT(:,:) = 0.0
  PDMCOND(:,:) = 0.0
  !
ENDIF
! 
!     Stock new values of condensated mass for diagnostic
!
! [ug.m-3] = [um3.m-3.s-1]*[s]*XFAC
PCOND_MASS_I(:) = PDMCOND(:,NM3(1)) * PDTACT * XFAC(JP_AER_SO4)
PCOND_MASS_J(:) = PDMCOND(:,NM3(2)) * PDTACT * XFAC(JP_AER_SO4)
!
!-------------------------------------------------------------------------------
!
! *       4.    MODE MERGING
!               ------------
!
!     This code implements Section 1.5 of Binkowski and Roselle (2003).
!     If the Aitken mode mass is growing faster than accumulation mode
!     mass and the Aitken mode number concentration exceeds the
!     accumulation mode number concentration, then moments tendency
!     are adjusted.
!
IF (LMODE_MERGING) THEN
  ZDMGROW(:,NM0(1)) = PDMCOND(:,NM0(1)) + PDMINTER(:,NM0(1)) + PDMINTRA(:,NM0(1))
  ZDMGROW(:,NM3(1)) = PDMCOND(:,NM3(1)) + PDMINTER(:,NM3(1)) + PDMINTRA(:,NM3(1))
  ZDMGROW(:,NM6(1)) = PDMCOND(:,NM6(1)) + PDMINTER(:,NM6(1)) + PDMINTRA(:,NM6(1))
  ZDMGROW(:,NM0(2)) = PDMCOND(:,NM0(2)) + PDMINTER(:,NM0(2)) + PDMINTRA(:,NM0(2))
  ZDMGROW(:,NM3(2)) = PDMCOND(:,NM3(2)) + PDMINTER(:,NM3(2)) + PDMINTRA(:,NM3(2))
  ZDMGROW(:,NM6(2)) = PDMCOND(:,NM6(2)) + PDMINTER(:,NM6(2)) + PDMINTRA(:,NM6(2))
  CALL CH_AER_MODE_MERGING(PM, PLNSIG, PRG, ZDMGROW, PDMMERG)
ELSE
  PDMMERG(:,:)=0.0
ENDIF
!
!-------------------------------------------------------------------------------
!
! *       5.    UPDATE OF SULFURIC ACID CONCENTRATION
!               -------------------------------------
!
PCTOTG(:,JP_AER_SO4g)=PCTOTG(:,JP_AER_SO4g)-PCOND_MASS_I(:)-PCOND_MASS_J(:)-PNUCL_MASS(:)
!
!-------------------------------------------------------
!
! *      6. MASK DIFFERENT TERMS
!           --------------------
!
! DIRE A QUOI SERVENT CES MASKS.... ?
!
DO JI=1,JPMODE
  PDMINTRA(:,NM0(JI)) = PDMINTRA(:,NM0(JI)) * PMASK(:,JI)
  PDMINTRA(:,NM3(JI)) = PDMINTRA(:,NM3(JI)) * PMASK(:,JI)
  PDMINTRA(:,NM6(JI)) = PDMINTRA(:,NM6(JI)) * PMASK(:,JI)
  PDMINTER(:,NM0(JI)) = PDMINTER(:,NM0(JI)) * PMASK(:,JI)
  PDMINTER(:,NM3(JI)) = PDMINTER(:,NM3(JI)) * PMASK(:,JI)
  PDMINTER(:,NM6(JI)) = PDMINTER(:,NM6(JI)) * PMASK(:,JI)
  PDMCOND (:,NM0(JI)) = PDMCOND (:,NM0(JI)) * PMASK(:,JI)
  PDMCOND (:,NM3(JI)) = PDMCOND (:,NM3(JI)) * PMASK(:,JI)
  PDMCOND (:,NM6(JI)) = PDMCOND (:,NM6(JI)) * PMASK(:,JI)
  PDMNUCL (:,NM0(JI)) = PDMNUCL (:,NM0(JI)) * PMASK(:,JI)
  PDMNUCL (:,NM3(JI)) = PDMNUCL (:,NM3(JI)) * PMASK(:,JI)
  PDMNUCL (:,NM6(JI)) = PDMNUCL (:,NM6(JI)) * PMASK(:,JI)
  PDMMERG (:,NM0(JI)) = PDMMERG (:,NM0(JI)) * PMASK(:,JI)
  PDMMERG (:,NM3(JI)) = PDMMERG (:,NM3(JI)) * PMASK(:,JI)
  PDMMERG (:,NM6(JI)) = PDMMERG (:,NM6(JI)) * PMASK(:,JI)
  PSEDA   (:,NM0(JI)) = PSEDA   (:,NM0(JI)) * PMASK(:,JI)
  PSEDA   (:,NM3(JI)) = PSEDA   (:,NM3(JI)) * PMASK(:,JI)
  PSEDA   (:,NM6(JI)) = PSEDA   (:,NM6(JI)) * PMASK(:,JI)
END DO
!
!-------------------------------------------------------
!
! *      7. SOLVE MOMENT EQUATIONS
!           ----------------------   
!
CALL CH_AER_SOLV(PM, PLNSIG, PRG, PN, PCTOTG, PCTOTA, PCCTOT,          &
                 PDMINTRA, PDMINTER, PDMCOND, PDMNUCL, PDMMERG, PSEDA, &
                 PDTACT, PRV, PDENAIR, PPRESSURE, PTEMP, PRC, PTIME,   &
                 PSOLORG, PMBEG, PMINT, PMEND)
!
END SUBROUTINE CH_AER_DRIVER
