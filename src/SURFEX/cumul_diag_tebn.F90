!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!##################################
SUBROUTINE CUMUL_DIAG_TEB_n (DGCT, DGMT, TOP, &
                             PTSTEP)
!##################################
!
!
!!****  *CUMUL_DIAG_TEB_n*  
!!
!!    PURPOSE
!!    -------
!      Cumulates some diagnostics for TEB
!     
!!**  METHOD
!!    ------
!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------ 
!!
!!      
!!    REFERENCE
!!    ---------
!!      
!!    AUTHOR
!!    ------
!!      C. de Munck       * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    02/2013
!!                  08/2013 (V. Masson) adds solar panels
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_DIAG_CUMUL_TEB_n, ONLY : DIAG_CUMUL_TEB_t
USE MODD_DIAG_MISC_TEB_n, ONLY : DIAG_MISC_TEB_t
USE MODD_TEB_OPTION_n, ONLY : TEB_OPTIONS_t
!
USE MODD_SURF_PAR,        ONLY :  XUNDEF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
TYPE(DIAG_CUMUL_TEB_t), INTENT(INOUT) :: DGCT
TYPE(DIAG_MISC_TEB_t), INTENT(INOUT) :: DGMT
TYPE(TEB_OPTIONS_t), INTENT(INOUT) :: TOP
!
      REAL,               INTENT(IN) :: PTSTEP            ! time step
!
!*      0.2    declarations of local variables
!
INTEGER :: JI 

REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!       0.     Initialization
!              --------------
IF (LHOOK) CALL DR_HOOK('CUMUL_DIAG_TEB_N',0,ZHOOK_HANDLE)
!
!       1.     Time-cumulated diagnostics for TEB
!              ----------------------------------
!
DO JI=1,SIZE(DGMT%CUR%XRUNOFF_ROOF,1)
!
 IF (TOP%LSOLAR_PANEL) THEN
    IF (DGMT%CUR%XTHER_PROD_BLD(JI) .NE. XUNDEF) THEN
     DGCT%CUR%XTHER_PROD_BLDC(JI)     =  DGCT%CUR%XTHER_PROD_BLDC(JI)     + DGMT%CUR%XTHER_PROD_BLD(JI)  * PTSTEP
    ENDIF
    !
    IF (DGMT%CUR%XPHOT_PROD_BLD(JI) .NE. XUNDEF) THEN
     DGCT%CUR%XPHOT_PROD_BLDC(JI)     =  DGCT%CUR%XPHOT_PROD_BLDC(JI)     + DGMT%CUR%XPHOT_PROD_BLD(JI)  * PTSTEP
    ENDIF
 END IF

 IF (TOP%CBEM == 'BEM') THEN
    IF (DGMT%CUR%XHVAC_COOL(JI) .NE. XUNDEF) THEN
     DGCT%CUR%XHVACC_COOL(JI)     =  DGCT%CUR%XHVACC_COOL(JI)       + DGMT%CUR%XHVAC_COOL(JI)        * PTSTEP
    ENDIF
    !
    IF (DGMT%CUR%XHVAC_HEAT(JI) .NE. XUNDEF) THEN
     DGCT%CUR%XHVACC_HEAT(JI)     =  DGCT%CUR%XHVACC_HEAT(JI)       + DGMT%CUR%XHVAC_HEAT(JI)        * PTSTEP
    ENDIF
 ENDIF
 !
 IF (DGMT%CUR%XRUNOFF_TOWN(JI) .NE. XUNDEF) THEN
  DGCT%CUR%XRUNOFFC_TOWN(JI)      =  DGCT%CUR%XRUNOFFC_TOWN(JI)     + DGMT%CUR%XRUNOFF_TOWN(JI)      * PTSTEP
 ENDIF
 !
 IF (DGMT%CUR%XRUNOFF_GARDEN(JI) .NE. XUNDEF) THEN
  DGCT%CUR%XRUNOFFC_GARDEN(JI)    =  DGCT%CUR%XRUNOFFC_GARDEN(JI)   + DGMT%CUR%XRUNOFF_GARDEN(JI)    * PTSTEP
 ENDIF
 !
 IF (DGMT%CUR%XRUNOFF_ROAD(JI) .NE. XUNDEF) THEN
  DGCT%CUR%XRUNOFFC_ROAD(JI)      =  DGCT%CUR%XRUNOFFC_ROAD(JI)     + DGMT%CUR%XRUNOFF_ROAD(JI)      * PTSTEP
 ENDIF
 !
 IF (DGMT%CUR%XRUNOFF_ROOF(JI) .NE. XUNDEF) THEN 
  DGCT%CUR%XRUNOFFC_ROOF(JI)      =  DGCT%CUR%XRUNOFFC_ROOF(JI)     + DGMT%CUR%XRUNOFF_ROOF(JI)      * PTSTEP
 ENDIF
 !
 IF (DGMT%CUR%XRUNOFF_STRLROOF(JI) .NE. XUNDEF) THEN
  DGCT%CUR%XRUNOFFC_STRLROOF(JI)  =  DGCT%CUR%XRUNOFFC_STRLROOF(JI) + DGMT%CUR%XRUNOFF_STRLROOF(JI)  * PTSTEP
 ENDIF
 !
 IF (DGMT%CUR%XDRAIN_GARDEN(JI) .NE. XUNDEF) THEN
   DGCT%CUR%XDRAINC_GARDEN(JI)    =  DGCT%CUR%XDRAINC_GARDEN(JI)    + DGMT%CUR%XDRAIN_GARDEN(JI)     * PTSTEP
 ENDIF
 !
 IF (DGMT%CUR%XIRRIG_GARDEN(JI) .NE. XUNDEF) THEN
   DGCT%CUR%XIRRIGC_GARDEN(JI)    =  DGCT%CUR%XIRRIGC_GARDEN(JI)    + DGMT%CUR%XIRRIG_GARDEN(JI)     * PTSTEP
 ENDIF
 !
 IF (DGMT%CUR%XIRRIG_ROAD(JI) .NE. XUNDEF) THEN
   DGCT%CUR%XIRRIGC_ROAD(JI)      =  DGCT%CUR%XIRRIGC_ROAD(JI)      + DGMT%CUR%XIRRIG_ROAD(JI)       * PTSTEP
 ENDIF
 !
 IF (TOP%LGREENROOF) THEN 
    IF (DGMT%CUR%XRUNOFF_GREENROOF(JI) .NE. XUNDEF) THEN
     DGCT%CUR%XRUNOFFC_GREENROOF(JI) =  DGCT%CUR%XRUNOFFC_GREENROOF(JI)+ DGMT%CUR%XRUNOFF_GREENROOF(JI) * PTSTEP
    ENDIF
    !
    IF (DGMT%CUR%XDRAIN_GREENROOF(JI) .NE. XUNDEF) THEN
     DGCT%CUR%XDRAINC_GREENROOF(JI)  =  DGCT%CUR%XDRAINC_GREENROOF(JI) + DGMT%CUR%XDRAIN_GREENROOF(JI)  * PTSTEP
    ENDIF
    !
    IF (DGMT%CUR%XIRRIG_GREENROOF(JI) .NE. XUNDEF) THEN
     DGCT%CUR%XIRRIGC_GREENROOF(JI)  =  DGCT%CUR%XIRRIGC_GREENROOF(JI) + DGMT%CUR%XIRRIG_GREENROOF(JI)  * PTSTEP
    ENDIF
 ENDIF
 !
ENDDO
!
IF (LHOOK) CALL DR_HOOK('CUMUL_DIAG_TEB_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE CUMUL_DIAG_TEB_n
