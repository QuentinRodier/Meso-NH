!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE UPDATE_THERMALPROP( PWG, PWGI,                   &
                         PHCAPSOILZ, PCONDDRYZ, PCONDSLDZ, PWSAT, &
                         PSOILCONDZ, PSOILHCAPZ                   )
!     ##########################################################################
!
!!****  *SOIL*  
!!
!!    PURPOSE
!!    -------
!
!     Update thermal properties of soil columns under roads and buildings
!         
!     
!!**  METHOD
!!    ------
!
!     Direct calculation
!
!!    EXTERNAL
!!    --------
!
!     None
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    USE MODD_CST
!!    USE MODD_PARAMETERS
!!
!!      
!!    REFERENCE
!!    ---------
!!
!!    Noilhan and Planton (1989)
!!    Belair (1995)
!!    Boone (2000)
!!      
!!    AUTHOR
!!    ------
!!      A. Boone           * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    
!!                  25/03/99      (Boone)   Added Johansen (1975)/Peters-Lidard 
!!                                          option to explicitly compute CG
!!                  08/25/02      (Boone)   DIF option code only
!!                  25/05/08     (Decharme) Add Flood properties
!!                  03/08/11     (Decharme) Optimization
!!                     04/13     (Decharme) good soil moisture extrapolation computation
!!                  23/07/13     (Decharme) Surface / Water table depth coupling
!!                  23/10/14     (Decharme) revise all thermo properties
!!                                          delete NP89 option for thermal cond
!!                                          because not physical with explicit soil.
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_CSTS,       ONLY : XCL, XCI, XRHOLW, XRHOLI, XCONDI
USE MODD_ISBA_PAR,   ONLY : XCONDWTR, XWGMIN
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
REAL, DIMENSION(:,:), INTENT(IN)  :: PHCAPSOILZ, PCONDDRYZ, PCONDSLDZ
!                                    PHCAPSOILZ = soil heat capacity [J/(K m3)]
!                                    PCONDDRYZ  = soil dry thermal conductivity 
!                                                 [W/(m K)] 
!                                    PCONDSLDZ  = soil solids thermal conductivity 
!                                                 [W/(m K)]
!
REAL, DIMENSION(:,:), INTENT(IN)  :: PWSAT
!                                    PWSAT    = profile of porosity (m3/m3)
!
REAL, DIMENSION(:,:),INTENT(INOUT):: PWG, PWGI
!                                    PWG    = soil liquid water content (m3/m3)
!                                    PWGI   = soil frozen water content (m3/m3)
!
REAL, DIMENSION(:,:), INTENT(OUT) :: PSOILCONDZ, PSOILHCAPZ
!                                    PSOILHCAP = soil heat capacity        (J m-3 K-1)
!                                    PSOILCOND = soil thermal conductivity (W m-1 K-1)
!
!*      0.2    declarations of local variables
!
!
REAL                         :: ZFROZEN2DF, ZUNFROZEN2DF, ZCONDSATDF, ZLOG_CONDI, ZLOG_CONDWTR,  &
                                ZSATDEGDF, ZKERSTENDF, ZWORK1, ZWORK2, ZWORK3
!
INTEGER :: INI, INL, JJ, JL
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!*       0.     Initialization:
!               ---------------
!
IF (LHOOK) CALL DR_HOOK('UPDAT_THERMALPROP',0,ZHOOK_HANDLE)
!
INI=SIZE(PWG,1)
INL=SIZE(PWG,2)
!
!-------------------------------------------------------------------------------
!
!*       1.     THE THERMAL CONDUCTIVITY OF BARE-GROUND
!               ---------------------------------------
!
! Calculate thermal conductivity using PL98 :
!
ZLOG_CONDI   = LOG(XCONDI)
ZLOG_CONDWTR = LOG(XCONDWTR)
!
DO JL=1,INL
   DO JJ=1,INI
!     
      ZFROZEN2DF   = PWGI(JJ,JL)/(PWGI(JJ,JL) + MAX(PWG(JJ,JL),XWGMIN))
      ZUNFROZEN2DF = (1.0-ZFROZEN2DF)*PWSAT(JJ,JL)
!
      ZWORK1      = LOG(PCONDSLDZ(JJ,JL))*(1.0-PWSAT(JJ,JL))
      ZWORK2      = ZLOG_CONDI*(PWSAT(JJ,JL)-ZUNFROZEN2DF)
      ZWORK3      = ZLOG_CONDWTR*ZUNFROZEN2DF
      ZCONDSATDF  = EXP(ZWORK1+ZWORK2+ZWORK3)
!
      ZSATDEGDF   = MAX(0.1, (PWGI(JJ,JL)+PWG(JJ,JL))/PWSAT(JJ,JL))
      ZSATDEGDF   = MIN(1.0,ZSATDEGDF)
      ZKERSTENDF  = LOG10(ZSATDEGDF) + 1.0
      ZKERSTENDF  = (1.0-ZFROZEN2DF)*ZKERSTENDF + ZFROZEN2DF *ZSATDEGDF  
!
!     Thermal conductivity of soil:
      PSOILCONDZ(JJ,JL) = ZKERSTENDF*(ZCONDSATDF-PCONDDRYZ(JJ,JL)) + PCONDDRYZ(JJ,JL)
!
   ENDDO
ENDDO
!
!-------------------------------------------------------------------------------
!
!*       2.     THE HEAT CAPACITY OF BARE-GROUND
!               --------------------------------
!
! Soil Heat capacity [J/(m3 K)]
!
DO JL=1,INL
   DO JJ=1,INI
      PSOILHCAPZ(JJ,JL) = (1.0-PWSAT(JJ,JL))*PHCAPSOILZ(JJ,JL) +         &
                               PWG  (JJ,JL) *XCL*XRHOLW        +         &
                               PWGI (JJ,JL) *XCI*XRHOLI    
   ENDDO
ENDDO
!
!
!-------------------------------------------------------------------------------
!
!
IF (LHOOK) CALL DR_HOOK('UPDATE_THERMALPROP',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE UPDATE_THERMALPROP
