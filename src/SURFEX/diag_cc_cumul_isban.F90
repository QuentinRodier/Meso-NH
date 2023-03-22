!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-_V1-en.txt and CeCILL-_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE DIAG_CC_CUMUL_ISBA_n (DECK, DCK, DEK, DK, IO, PTSTEP, KSIZE )
!     ###############################################################################
!
!!****  *DIAG_CC_CUMUL_ISBA_n* - carbon cycle diagnostics for ISBA-CC
!!                               Carbon fluxes already in kgCO2/m2/s
!!
!!    PURPOSE
!!    -------
!
!!**  METHOD
!!    ------
!!
!!    REFERENCE
!!    ---------
!!      
!!
!!    AUTHOR
!!    ------
!!     B. Decharme 
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    03/2022
!!------------------------------------------------------------------
!
USE MODD_DIAG_EVAP_ISBA_n, ONLY : DIAG_EVAP_ISBA_t
USE MODD_DIAG_n,           ONLY : DIAG_t
USE MODD_ISBA_n,           ONLY : ISBA_PE_t
USE MODD_ISBA_OPTIONS_n,   ONLY : ISBA_OPTIONS_t
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
TYPE(DIAG_EVAP_ISBA_t), INTENT(INOUT) :: DECK
TYPE(DIAG_EVAP_ISBA_t), INTENT(INOUT) :: DEK
TYPE(DIAG_t),           INTENT(INOUT) :: DCK
TYPE(DIAG_t),           INTENT(INOUT) :: DK
TYPE(ISBA_OPTIONS_t),   INTENT(INOUT) :: IO
!
!
REAL,    INTENT(IN) :: PTSTEP        ! time step
INTEGER, INTENT(IN) :: KSIZE  
!
!*      0.2    declarations of local variables
!
INTEGER :: JI
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('DIAG_CC_CUMUL_ISBA_N',0,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------------
!
!*       1.     ISBA-Ags and ISBA-CC specific fluxes:
!               -------------------------------------
!
IF(IO%CPHOTO/='NON')THEN
!  !cdir nodep
  DO JI=1,KSIZE
     !Transform units from kgC m-2 s-1 to kgC m-2
     DECK%XGPP      (JI) =  DECK%XGPP      (JI) + DEK%XGPP      (JI) * PTSTEP
     DECK%XRESP_AUTO(JI) =  DECK%XRESP_AUTO(JI) + DEK%XRESP_AUTO(JI) * PTSTEP
     DECK%XRESP_ECO (JI) =  DECK%XRESP_ECO (JI) + DEK%XRESP_ECO (JI) * PTSTEP
     DECK%XFLTOSCARB(JI) =  DECK%XFLTOSCARB(JI) + DEK%XFLTOSCARB(JI) * PTSTEP
     DECK%XRESPSCARB(JI) =  DECK%XRESPSCARB(JI) + DEK%XRESPSCARB(JI) * PTSTEP
     DECK%XRESPLIT  (JI) =  DECK%XRESPLIT  (JI) + DEK%XRESPLIT  (JI) * PTSTEP
  ENDDO
ELSE  
  DECK%XGPP      (:)=0.0
  DECK%XRESP_AUTO(:)=0.0
  DECK%XRESP_ECO (:)=0.0 
  DECK%XFLTOSCARB(:)=0.0
  DECK%XRESPSCARB(:)=0.0
  DECK%XRESPLIT  (:)=0.0
ENDIF
!  
IF (IO%CRESPSL=='CNT'.OR.IO%CRESPSL=='DIF') THEN
!  !cdir nodep
  DO JI=1,KSIZE
     !Transform units from kgC m-2 s-1 to kgC m-2
     DECK%XTURNVTOT(JI) =  DECK%XTURNVTOT(JI) + DEK%XTURNVTOT(JI) * PTSTEP
  ENDDO
ELSE  
  DECK%XTURNVTOT(:)=0.0
ENDIF
!
!-------------------------------------------------------------------------------------
!
!*       2.     ISBA-CC soil leaching :
!               -----------------------
!
IF(IO%LCLEACH) THEN
  !cdir nodep
  DO JI=1,KSIZE
     !Transform units from kgC m-2 s-1 to kgC m-2
     DECK%XFDOCLIT(JI)  =  DECK%XFDOCLIT(JI) + DEK%XFDOCLIT(JI) * PTSTEP
     DECK%XFDOC   (JI)  =  DECK%XFDOC   (JI) + DEK%XFDOC   (JI) * PTSTEP
  ENDDO
ENDIF    
!
!-------------------------------------------------------------------------------------
!
!*       3.     ISBA-CC fire scheme :
!               ---------------------
!
IF(IO%LFIRE)THEN
  !cdir nodep
  DO JI=1,KSIZE
     !Transform units from kgC m-2 s-1 to kgC m-2
     DECK%XFIRETURNOVER(JI)  =  DECK%XFIRETURNOVER(JI) + DEK%XFIRETURNOVER(JI) * PTSTEP
     DECK%XFIRECO2     (JI)  =  DECK%XFIRECO2     (JI) + DEK%XFIRECO2     (JI) * PTSTEP
     DECK%XFIREBCS     (JI)  =  DECK%XFIREBCS     (JI) + DEK%XFIREBCS     (JI) * PTSTEP
  ENDDO
ENDIF
!
!-------------------------------------------------------------------------------------
!
!*       4.     ISBA-CC Land-use carbon flux :
!               -----------------------
!
IF(IO%LLULCC) THEN
  !cdir nodep
  DO JI=1,KSIZE
     !Transform units from kgC m-2 s-1 to kgC m-2
     DECK%XFHARVEST(JI)  =  DECK%XFHARVEST(JI) + DEK%XFHARVEST(JI) * PTSTEP
  ENDDO
ENDIF    
! 
!-------------------------------------------------------------------------------------
!
!*       5.     ISBA-CC soil gas scheme fluxes:
!               -------------------------------
!
IF(IO%LSOILGAS)THEN
  !cdir nodep
  DO JI=1,KSIZE
     !Transform units from kgC m-2 s-1 to kgC m-2
     DECK%XSURF_O2  (JI) = DECK%XSURF_O2  (JI) + DEK%XSURF_O2  (JI) * PTSTEP
     DECK%XSURF_CO2 (JI) = DECK%XSURF_CO2 (JI) + DEK%XSURF_CO2 (JI) * PTSTEP
     DECK%XSURF_CH4 (JI) = DECK%XSURF_CH4 (JI) + DEK%XSURF_CH4 (JI) * PTSTEP
     DECK%XEVAP_O2  (JI) = DECK%XEVAP_O2  (JI) + DEK%XEVAP_O2  (JI) * PTSTEP
     DECK%XEVAP_CO2 (JI) = DECK%XEVAP_CO2 (JI) + DEK%XEVAP_CO2 (JI) * PTSTEP
     DECK%XEVAP_CH4 (JI) = DECK%XEVAP_CH4 (JI) + DEK%XEVAP_CH4 (JI) * PTSTEP
     DECK%XPMT_O2   (JI) = DECK%XPMT_O2   (JI) + DEK%XPMT_O2   (JI) * PTSTEP
     DECK%XPMT_CO2  (JI) = DECK%XPMT_CO2  (JI) + DEK%XPMT_CO2  (JI) * PTSTEP
     DECK%XPMT_CH4  (JI) = DECK%XPMT_CH4  (JI) + DEK%XPMT_CH4  (JI) * PTSTEP
     DECK%XEBU_CH4  (JI) = DECK%XEBU_CH4  (JI) + DEK%XEBU_CH4  (JI) * PTSTEP
     DECK%XFCONS_O2 (JI) = DECK%XFCONS_O2 (JI) + DEK%XFCONS_O2 (JI) * PTSTEP
     DECK%XFPROD_CO2(JI) = DECK%XFPROD_CO2(JI) + DEK%XFPROD_CO2(JI) * PTSTEP
     DECK%XFMT_CH4  (JI) = DECK%XFMT_CH4  (JI) + DEK%XFMT_CH4  (JI) * PTSTEP
     DECK%XFMG_CH4  (JI) = DECK%XFMG_CH4  (JI) + DEK%XFMG_CH4  (JI) * PTSTEP     
     DECK%XCH4FLUX  (JI) = DECK%XCH4FLUX  (JI) + DEK%XCH4FLUX  (JI) * PTSTEP     
     DECK%XO2FLUX   (JI) = DECK%XO2FLUX   (JI) + DEK%XO2FLUX   (JI) * PTSTEP     
  ENDDO
ENDIF
!
!-------------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('DIAG_CC_CUMUL_ISBA_N',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------------
!
END SUBROUTINE DIAG_CC_CUMUL_ISBA_n
