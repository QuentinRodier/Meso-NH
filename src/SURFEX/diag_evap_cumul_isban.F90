!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-_V1-en.txt and CeCILL-_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE DIAG_EVAP_CUMUL_ISBA_n (DE, DECK, DCK, DEK, DK, PEK, &
                                   IO, PTSTEP, KSIZE, KPATCH    )
!     ###############################################################################
!
!!****  *DIAG_EVAP-ISBA_n * - additional diagnostics for ISBA
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
!!     P. LeMoigne 
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2004
!!                     2008      New diag
!!      B. Decharme    2012      New snow diag LESL
!!                               Add carbon fluxes diag
!!                               Add isba water budget diag
!!      B. Decharme  04/2013     add Subsurface runoff if SGH (DIF option only) 
!!                               add sublimation
!!      P Samuelsson   04/2012   MEB
!!      R. Séférian    08/2016   Fire and carbon leaching module
!!      D. Delire      08/2016   Add turnover diagnostics 
!!      B. Decharme    02/2017   add energy/snowmelt diags
!!      R. Séférian    11/2020   Crop harvesting
!!      B. Decharme    03/2021   Delete PRHOA
!!                               Carbon fluxes now in DIAG_CC_CUMUL_ISBA_n
!!------------------------------------------------------------------
!
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
TYPE(DIAG_EVAP_ISBA_t), INTENT(INOUT) :: DE
TYPE(DIAG_EVAP_ISBA_t), INTENT(INOUT) :: DECK
TYPE(DIAG_EVAP_ISBA_t), INTENT(INOUT) :: DEK
TYPE(DIAG_t),           INTENT(INOUT) :: DCK
TYPE(DIAG_t),           INTENT(INOUT) :: DK
TYPE(ISBA_PE_t),        INTENT(INOUT) :: PEK
TYPE(ISBA_OPTIONS_t),   INTENT(INOUT) :: IO
!
!
REAL,    INTENT(IN) :: PTSTEP        ! time step
INTEGER, INTENT(IN) :: KSIZE  
INTEGER, INTENT(IN) :: KPATCH
!
!*      0.2    declarations of local variables
!
INTEGER :: JI
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('DIAG_EVAP_CUMUL_ISBA_N',0,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------------
!
!*       1.     General fluxes:
!               ---------------
!
!cdir nodep
DO JI=1,KSIZE
   !
   DCK%XRN   (JI) = DCK%XRN   (JI) + DK%XRN   (JI) * PTSTEP
   DCK%XH    (JI) = DCK%XH    (JI) + DK%XH    (JI) * PTSTEP
   DCK%XLE   (JI) = DCK%XLE   (JI) + DK%XLE   (JI) * PTSTEP
   DCK%XLEI  (JI) = DCK%XLEI  (JI) + DK%XLEI  (JI) * PTSTEP
   DCK%XGFLUX(JI) = DCK%XGFLUX(JI) + DK%XGFLUX(JI) * PTSTEP
   DCK%XEVAP (JI) = DCK%XEVAP (JI) + DK%XEVAP (JI) * PTSTEP
   DCK%XSUBL (JI) = DCK%XSUBL (JI) + DK%XSUBL (JI) * PTSTEP
   DCK%XSWD  (JI) = DCK%XSWD  (JI) + DK%XSWD  (JI) * PTSTEP
   DCK%XSWU  (JI) = DCK%XSWU  (JI) + DK%XSWU  (JI) * PTSTEP
   DCK%XLWD  (JI) = DCK%XLWD  (JI) + DK%XLWD  (JI) * PTSTEP
   DCK%XLWU  (JI) = DCK%XLWU  (JI) + DK%XLWU  (JI) * PTSTEP
   DCK%XFMU  (JI) = DCK%XFMU  (JI) + DK%XFMU  (JI) * PTSTEP
   DCK%XFMV  (JI) = DCK%XFMV  (JI) + DK%XFMV  (JI) * PTSTEP   
   !
   DECK%XEPOT      (JI) = DECK%XEPOT      (JI) + DEK%XEPOT      (JI) * PTSTEP
   DECK%XLEG       (JI) = DECK%XLEG       (JI) + DEK%XLEG       (JI) * PTSTEP
   DECK%XLEGI      (JI) = DECK%XLEGI      (JI) + DEK%XLEGI      (JI) * PTSTEP
   DECK%XLEV       (JI) = DECK%XLEV       (JI) + DEK%XLEV       (JI) * PTSTEP
   DECK%XLES       (JI) = DECK%XLES       (JI) + DEK%XLES       (JI) * PTSTEP
   DECK%XLER       (JI) = DECK%XLER       (JI) + DEK%XLER       (JI) * PTSTEP
   DECK%XLETR      (JI) = DECK%XLETR      (JI) + DEK%XLETR      (JI) * PTSTEP
   DECK%XDRAIN     (JI) = DECK%XDRAIN     (JI) + DEK%XDRAIN     (JI) * PTSTEP
   DECK%XQSB       (JI) = DECK%XQSB       (JI) + DEK%XQSB       (JI) * PTSTEP
   DECK%XRUNOFF    (JI) = DECK%XRUNOFF    (JI) + DEK%XRUNOFF    (JI) * PTSTEP
   DECK%XHORT      (JI) = DECK%XHORT      (JI) + DEK%XHORT      (JI) * PTSTEP
   DECK%XDRIP      (JI) = DECK%XDRIP      (JI) + DEK%XDRIP      (JI) * PTSTEP
   DECK%XRRVEG     (JI) = DECK%XRRVEG     (JI) + DEK%XRRVEG     (JI) * PTSTEP
   DECK%XMELT      (JI) = DECK%XMELT      (JI) + DEK%XMELT      (JI) * PTSTEP
   DECK%XMELTSTOT  (JI) = DECK%XMELTSTOT  (JI) + DEK%XMELTSTOT  (JI) * PTSTEP
   DECK%XIFLOOD    (JI) = DECK%XIFLOOD    (JI) + DEK%XIFLOOD    (JI) * PTSTEP
   DECK%XPFLOOD    (JI) = DECK%XPFLOOD    (JI) + DEK%XPFLOOD    (JI) * PTSTEP
   DECK%XLE_FLOOD  (JI) = DECK%XLE_FLOOD  (JI) + DEK%XLE_FLOOD  (JI) * PTSTEP
   DECK%XLEI_FLOOD (JI) = DECK%XLEI_FLOOD (JI) + DEK%XLEI_FLOOD (JI) * PTSTEP
   DECK%XIRRIG_FLUX(JI) = DECK%XIRRIG_FLUX(JI) + DEK%XIRRIG_FLUX(JI) * PTSTEP
   DECK%XICEFLUX   (JI) = DECK%XICEFLUX   (JI) + DEK%XICEFLUX   (JI) * PTSTEP
   !
ENDDO
!
!-------------------------------------------------------------------------------------
!
!*       2.     Meb specific fluxes:
!               --------------------
!
IF (IO%LMEB_PATCH(KPATCH)) THEN
   !
   !cdir nodep
   DO JI=1,KSIZE
      !
      DECK%XLEV_CV (JI) = DECK%XLEV_CV (JI) + DEK%XLEV_CV (JI) * PTSTEP
      DECK%XLES_CV (JI) = DECK%XLES_CV (JI) + DEK%XLES_CV (JI) * PTSTEP
      DECK%XLETR_CV(JI) = DECK%XLETR_CV(JI) + DEK%XLETR_CV(JI) * PTSTEP
      DECK%XLER_CV (JI) = DECK%XLER_CV (JI) + DEK%XLER_CV (JI) * PTSTEP
      DECK%XLE_CV  (JI) = DECK%XLE_CV  (JI) + DEK%XLE_CV  (JI) * PTSTEP
      DECK%XH_CV   (JI) = DECK%XH_CV   (JI) + DEK%XH_CV   (JI) * PTSTEP
      DECK%XMELT_CV(JI) = DECK%XMELT_CV(JI) + DEK%XMELT_CV(JI) * PTSTEP
      DECK%XFRZ_CV (JI) = DECK%XFRZ_CV (JI) + DEK%XFRZ_CV (JI) * PTSTEP     
      DECK%XLE_GV  (JI) = DECK%XLE_GV  (JI) + DEK%XLE_GV  (JI) * PTSTEP
      DECK%XH_GV   (JI) = DECK%XH_GV   (JI) + DEK%XH_GV   (JI) * PTSTEP   
      !
      DECK%XLE_GN    (JI) = DECK%XLE_GN    (JI) + DEK%XLE_GN    (JI) * PTSTEP
      DECK%XH_GN     (JI) = DECK%XH_GN     (JI) + DEK%XH_GN     (JI) * PTSTEP
      DECK%XSR_GN    (JI) = DECK%XSR_GN    (JI) + DEK%XSR_GN    (JI) * PTSTEP
      DECK%XSWDOWN_GN(JI) = DECK%XSWDOWN_GN(JI) + DEK%XSWDOWN_GN(JI) * PTSTEP
      DECK%XLWDOWN_GN(JI) = DECK%XLWDOWN_GN(JI) + DEK%XLWDOWN_GN(JI) * PTSTEP       
      DECK%XLE_CA    (JI) = DECK%XLE_CA    (JI) + DEK%XLE_CA    (JI) * PTSTEP
      DECK%XH_CA     (JI) = DECK%XH_CA     (JI) + DEK%XH_CA     (JI) * PTSTEP
      !
      DECK%XSWNET_V  (JI)  = DECK%XSWNET_V  (JI)  + DEK%XSWNET_V    (JI) * PTSTEP
      DECK%XSWNET_G  (JI)  = DECK%XSWNET_G  (JI)  + DEK%XSWNET_G    (JI) * PTSTEP
      DECK%XSWNET_N  (JI)  = DECK%XSWNET_N  (JI)  + DEK%XSWNET_N    (JI) * PTSTEP
      DECK%XSWNET_NS (JI)  = DECK%XSWNET_NS (JI)  + DEK%XSWNET_NS   (JI) * PTSTEP
      DECK%XLWNET_V  (JI)  = DECK%XLWNET_V  (JI)  + DEK%XLWNET_V    (JI) * PTSTEP
      DECK%XLWNET_G  (JI)  = DECK%XLWNET_G  (JI)  + DEK%XLWNET_G    (JI) * PTSTEP
      DECK%XLWNET_N  (JI)  = DECK%XLWNET_N  (JI)  + DEK%XLWNET_N    (JI) * PTSTEP
      !
   ENDDO   
   !
ENDIF
!
!-------------------------------------------------------------------------------------
!
!*       3.     Snow or glacier specific fluxes:
!               --------------------------------
!
IF (PEK%TSNOW%SCHEME=='3-L' .OR. PEK%TSNOW%SCHEME=='CRO') THEN
   !cdir nodep
   DO JI=1,KSIZE
      DECK%XLESL      (JI) = DECK%XLESL      (JI) + DEK%XLESL      (JI) * PTSTEP
      DECK%XSNDRIFT   (JI) = DECK%XSNDRIFT   (JI) + DEK%XSNDRIFT   (JI) * PTSTEP
      DECK%XSNFREE_SWU(JI) = DECK%XSNFREE_SWU(JI) + DEK%XSNFREE_SWU(JI) * PTSTEP
      DECK%XSNREFREEZ (JI) = DECK%XSNREFREEZ (JI) + DEK%XSNREFREEZ (JI) * PTSTEP
   ENDDO
ENDIF
!  
!-------------------------------------------------------------------------------------
!
!*       4.     ISBA water and energy budget specific fluxes:
!               ---------------------------------------------
!
IF(DE%LWATER_BUDGET)THEN
  !cdir nodep
  DO JI=1,KSIZE
     DECK%XDWG    (JI) =  DECK%XDWG    (JI) + DEK%XDWG    (JI) * PTSTEP
     DECK%XDWGI   (JI) =  DECK%XDWGI   (JI) + DEK%XDWGI   (JI) * PTSTEP
     DECK%XDWR    (JI) =  DECK%XDWR    (JI) + DEK%XDWR    (JI) * PTSTEP
     DECK%XDSWE   (JI) =  DECK%XDSWE   (JI) + DEK%XDSWE   (JI) * PTSTEP
     DECK%XDSWFREE(JI) =  DECK%XDSWFREE(JI) + DEK%XDSWFREE(JI) * PTSTEP
     DECK%XWATBUD (JI) =  DECK%XWATBUD (JI) + DEK%XWATBUD (JI) * PTSTEP
  ENDDO
ENDIF
!
IF(DE%LENERGY_BUDGET)THEN
  !cdir nodep
  DO JI=1,KSIZE
     DECK%XNRJBUD       (JI) = DECK%XNRJBUD       (JI) + DEK%XNRJBUD       (JI) * PTSTEP
     DECK%XNRJBUD_SFC   (JI) = DECK%XNRJBUD_SFC   (JI) + DEK%XNRJBUD_SFC   (JI) * PTSTEP
     DECK%XGRNDFLUX     (JI) = DECK%XGRNDFLUX     (JI) + DEK%XGRNDFLUX     (JI) * PTSTEP
     DECK%XRESTORE      (JI) = DECK%XRESTORE      (JI) + DEK%XRESTORE      (JI) * PTSTEP
     DECK%XRESTOREN     (JI) = DECK%XRESTOREN     (JI) + DEK%XRESTOREN     (JI) * PTSTEP
     DECK%XDELHEATG     (JI) = DECK%XDELHEATG     (JI) + DEK%XDELHEATG     (JI) * PTSTEP
     DECK%XDELHEATN     (JI) = DECK%XDELHEATN     (JI) + DEK%XDELHEATN     (JI) * PTSTEP
     DECK%XDELPHASEG    (JI) = DECK%XDELPHASEG    (JI) + DEK%XDELPHASEG    (JI) * PTSTEP
     DECK%XDELPHASEN    (JI) = DECK%XDELPHASEN    (JI) + DEK%XDELPHASEN    (JI) * PTSTEP
     DECK%XDELHEATG_SFC (JI) = DECK%XDELHEATG_SFC (JI) + DEK%XDELHEATG_SFC (JI) * PTSTEP
     DECK%XDELHEATN_SFC (JI) = DECK%XDELHEATN_SFC (JI) + DEK%XDELHEATN_SFC (JI) * PTSTEP
     DECK%XDELPHASEG_SFC(JI) = DECK%XDELPHASEG_SFC(JI) + DEK%XDELPHASEG_SFC(JI) * PTSTEP
     DECK%XDELPHASEN_SFC(JI) = DECK%XDELPHASEN_SFC(JI) + DEK%XDELPHASEN_SFC(JI) * PTSTEP    
  ENDDO
ENDIF
!
!-------------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('DIAG_EVAP_CUMUL_ISBA_N',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------------
!
END SUBROUTINE DIAG_EVAP_CUMUL_ISBA_n
