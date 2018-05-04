!     ######################  
      MODULE MODN_PARAM_LIMA
!     ######################
!
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
USE MODD_PARAM_LIMA
!
IMPLICIT NONE
!
!
NAMELIST/NAM_PARAM_LIMA/LCOLD, LNUCL, LSEDI, LSNOW, LHAIL, LHHONI, LMEYERS,&
                        NMOD_IFN, XIFN_CONC, LIFN_HOM,                     &
                        CIFN_SPECIES, CINT_MIXING, NMOD_IMM, NIND_SPECIE,  &
                        CPRISTINE_ICE_LIMA, CHEVRIMED_ICE_LIMA,            &
                        XALPHAI, XNUI, XALPHAS, XNUS, XALPHAG, XNUG,       &
                        XFACTNUC_DEP, XFACTNUC_CON, NPHILLIPS,             &
!
                        LWARM, LACTI, LRAIN, LSEDC, LACTIT, LBOUND,        &
                        NMOD_CCN, XCCN_CONC,                               &
                        LCCN_HOM, CCCN_MODES, HINI_CCN, HTYPE_CCN,         &
                        XALPHAC, XNUC, XALPHAR, XNUR,                      &
                        XFSOLUB_CCN, XACTEMP_CCN, XAERDIFF, XAERHEIGHT,    &
                        LSCAV, LAERO_MASS, LDEPOC, XVDEPOC, LACTTKE,       &
                        LPTSPLIT, LFEEDBACKT, NMAXITER, XMRSTEP, XTSTEP_TS
!
END MODULE MODN_PARAM_LIMA
