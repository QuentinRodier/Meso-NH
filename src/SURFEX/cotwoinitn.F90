!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE COTWOINIT_n (I, &
                              HPHOTO,PVEGTYPE,PGMES,PCO2,PGC,PDMAX,            &
                            PABC,PPOI,PANMAX,                                 &
                            PFZERO,PEPSO,PGAMM,PQDGAMM,PQDGMES,PT1GMES,       &
                            PT2GMES,PAMAX,PQDAMAX,PT1AMAX,PT2AMAX,PAH,PBH,    &
                            PTAU_WOOD                                         )  
!     #######################################################################
!
!!****  *COTWOINIT*  
!!
!!    PURPOSE
!!    -------
!
!     Initialize model to calculate net assimilation of 
!     CO2 and leaf conductance.
!              
!!**  METHOD
!!    ------
!     Calvet at al (1998) [from model of Jacobs(1994)]
!!
!!    EXTERNAL
!!    --------
!!    none
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!      
!!    USE MODD_CO2V_PAR
!!    USE MODI_COTWO  
!!
!!    REFERENCE
!!    ---------
!!
!!    Calvet et al. (1998)
!!      
!!    AUTHOR
!!    ------
!!
!!      A. Boone           * Meteo-France *
!!      (following Belair)
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    27/10/97
!!      (V. Rivalland) 10/04/02  Add: PAH and PBH coefficients for
!!                               herbaceous water stress response
!!      (P. LeMoigne) 03/2004:   computation of zgmest in SI units
!!      (P. LeMoigne) 10/2004:   possibility of 2 different FZERO
!!      (L. Jarlan)   10/2004:   initialization of DMAX
!!      P Le Moigne   09/2005    AGS modifs of L. Jarlan
!!      S. Lafont     03/2009    change unit of AMAX
!!      A.L. Gibelin  04/2009    TAU_WOOD for NCB option 
!!      A.L. Gibelin  04/2009    Suppress useless GPP and RDK arguments 
!!      A.L. Gibelin  07/2009    Suppress PPST and PPSTF as outputs
!!      B. Decharme   05/2012    Optimization
!!      R. Alkama     05/2012    add 7 new vegtype (19  instead 12)
!!      C. Delire     01/2014    Define a dummy LAI from top and total lai for Dark respiration 
!!
!-------------------------------------------------------------------------------
!
!
USE MODD_ISBA_n, ONLY : ISBA_t
!
USE MODD_DATA_COVER_PAR, ONLY : NVEGTYPE, NVT_C3, NVT_C4, NVT_IRR, NVT_TROG,     &
                                NVT_TEBD, NVT_BONE, NVT_TRBE, NVT_TRBD, NVT_TEBE,&
                                NVT_TENE, NVT_BOBD, NVT_BOND, NVT_SHRB, NVT_GRAS
USE MODD_CSTS,           ONLY : XMD
USE MODD_CO2V_PAR,       ONLY : XTOPT, XFZERO1, XFZERO2, XFZEROTROP, XEPSO, XGAMM, XQDGAMM, &
                                  XQDGMES, XT1GMES, XT2GMES, XAMAX,               &
                                  XQDAMAX, XT1AMAX, XT2AMAX, XAH, XBH,            &
                                  XDSPOPT, XIAOPT, XAW, XBW, XMCO2, XMC, XTAU_WOOD  
! 
USE MODE_COTWO,          ONLY : GAULEG
USE MODI_COTWO  
!
!*       0.     DECLARATIONS
!               ------------
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
!
TYPE(ISBA_t), INTENT(INOUT) :: I
!
 CHARACTER(LEN=3),   INTENT(IN)   :: HPHOTO      ! type of photosynthesis
REAL,DIMENSION(:,:),INTENT(IN)   :: PVEGTYPE
!                                     PVEGTYPE = fraction of each
!                                     vegetation classification index;
!                                     C3 =>1, C4 => 2
!
REAL,DIMENSION(:),INTENT(IN)  :: PGMES, PCO2
!                                     PGMES     = mesophyll conductance (m s-1)
!                                     PCO2      = atmospheric CO2 concentration
!
REAL,DIMENSION(:),INTENT(IN)   :: PDMAX, PGC    
!                                     PDMAX     = maximum air saturation deficit tolerate
!                                                 by vegetation
!                                     PGC       = cuticular conductance (m s-1)
!
REAL, DIMENSION(:), INTENT(OUT)  :: PABC, PPOI
!                                     ZABC      = abscissa needed for integration
!                                     of net assimilation and stomatal conductance
!                                     over canopy depth
!                                     ZPOI      = Gaussian weights (as above)
!
REAL,DIMENSION(:),INTENT(OUT)  :: PANMAX
!                                     PANMAX    = maximum net assimilation
!
REAL,DIMENSION(:),INTENT(OUT)  :: PFZERO, PEPSO, PGAMM, PQDGAMM, PQDGMES,  &
                                    PT1GMES, PT2GMES, PAMAX, PQDAMAX,       &
                                    PT1AMAX, PT2AMAX, PTAU_WOOD  
!                                     PFZERO    = ideal value of F, no photorespiration or 
!                                                 saturation deficit
!                                     PEPSO     = maximum initial quantum use efficiency 
!                                                 (kgCO2 kgAir-1 J-1 PAR)
!                                     PGAMM     = CO2 conpensation concentration (kgCO2 kgAir-1)
!                                     PQDGAMM   = Log of Q10 function for CO2 conpensation 
!                                                 concentration
!                                     PQDGMES   = Log of Q10 function for mesophyll conductance 
!                                     PT1GMES   = reference temperature for computing 
!                                                 compensation concentration function for 
!                                                 mesophyll conductance: minimum temperature 
!                                     PT2GMES   = reference temperature for computing 
!                                                 compensation concentration function for 
!                                                 mesophyll conductance: maximum temperature
!                                     PAMAX     = leaf photosynthetic capacity (Units of kgCO2 kgAir-1 m s-1)
!                                     PQDAMAX   = Log of Q10 function for leaf photosynthetic capacity
!                                     PT1AMAX   = reference temperature for computing 
!                                                 compensation concentration function for leaf 
!                                                 photosynthetic capacity: minimum temperature
!                                     PT2AMAX   = reference temperature for computing 
!                                                 compensation concentration function for leaf 
!                                                 photosynthetic capacity: maximum temperature
!                                     PTAU_WOOD = residence time in woody biomass (s)
!
REAL,DIMENSION(:),INTENT(OUT)  :: PAH, PBH
!                                     PAH       = coeficient of universal relationship for herbaceous
!                                     PBH       = coeficient of universal relationship for herbaceous
!
!*      0.2    declaration of local variables
!
INTEGER                           :: JCLASS    ! indexes for loops
INTEGER                           :: ICLASS    ! indexes for loops
INTEGER                           :: ICO2TYPE  ! type of CO2 vegetation
INTEGER                           :: IRAD      ! with or without new radiative transfer
!
REAL, DIMENSION(SIZE(PANMAX))     :: ZGS, ZGAMMT, ZTOPT, ZANMAX, ZGMEST, ZGPP, ZRDK, ZEPSO
!                                    ZTOPT     = optimum  temperature for compensation 
!                                                point
!                                    ZANMAX    = maximum photosynthesis rate
!                                    ZGS       = leaf conductance
!                                    ZGAMMT    = temperature compensation point
!                                    ZGPP      = gross primary production
!                                    ZRDK      = dark respiration
!
!
REAL, DIMENSION(SIZE(PANMAX))     :: ZCO2INIT3, ZCO2INIT4, ZCO2INIT5, ZCO2INIT2,ZCO2INIT1
!                                    working arrays for initializing surface 
!                                    temperature, saturation deficit, global radiation,
!                                    optimum temperature for determining maximum 
!                                    photosynthesis rate, and soil water stress (none)
REAL, DIMENSION(SIZE(PDMAX))      :: ZDMAX
REAL, DIMENSION(SIZE(PDMAX))      :: ZWORK
!                                    Local variable in order to initialise DMAX
!                                    following Calvet, 2000 (AST or LST cases)
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('COTWOINIT_N',0,ZHOOK_HANDLE)
!
ZTOPT  (:) = 0.
PFZERO (:) = 0.
PEPSO  (:) = 0.
PGAMM  (:) = 0.
PQDGAMM(:) = 0.
PQDGMES(:) = 0.
PT1GMES(:) = 0.
PT2GMES(:) = 0.
PAMAX  (:) = 0.
PQDAMAX(:) = 0.
PT1AMAX(:) = 0.
PT2AMAX(:) = 0.
PTAU_WOOD(:) = 0.
!
PAH    (:) = 0.
PBH    (:) = 0.
!
ZEPSO (:) = 0.
ZGPP (:) = 0.
ZRDK (:) = 0.
ZGAMMT (:) = 0.
ZANMAX (:) = 0.
ZGMEST (:) = 0.
ZCO2INIT3(:) = 0.
ZCO2INIT4(:) = 0.
ZCO2INIT5(:) = 0.
!
!-------------------------------------------------------------------------------
!
!-------------------------------------------------------------------------------
!
! DETERMINE GAUSSIAN WEIGHTS NEEDED FOR CO2 MODEL 
! -----------------------------------------------
!
 CALL GAULEG(0.0,1.0,PABC,PPOI,SIZE(PABC))
!
!
! INITIALIZE VARIOUS PARAMETERS FOR CO2 MODEL:
! --------------------------------------------
! as a function of CO2 vegetation class, C3=>1, C4=>2
!
DO JCLASS=1,NVEGTYPE
  !
  IF (JCLASS==NVT_C4 .OR. JCLASS==NVT_IRR .OR. JCLASS==NVT_TROG) THEN
    ICO2TYPE = 2   ! C4 type
  ELSE
    ICO2TYPE = 1   ! C3 type
  END IF
  IF(I%LAGRI_TO_GRASS.AND.(JCLASS==NVT_C4 .OR. JCLASS==NVT_IRR)) ICO2TYPE = 1
  IF (I%LTR_ML) THEN
    IRAD = 1   ! running with new radiative transfer
  ELSE
    IRAD = 2
  ENDIF
  !
  ZTOPT  (:) = ZTOPT  (:) + XTOPT  (ICO2TYPE) * PVEGTYPE(:,JCLASS)
  IF (HPHOTO == 'AGS' .OR. HPHOTO == 'LAI') THEN
     PFZERO (:) = PFZERO (:) + XFZERO1 (ICO2TYPE) * PVEGTYPE(:,JCLASS)
  ELSE
     IF((JCLASS==NVT_TEBD) .OR. (JCLASS==NVT_BONE) .OR.                         &
        (JCLASS==NVT_TRBD) .OR. (JCLASS==NVT_TEBE) .OR. (JCLASS==NVT_TENE) .OR. &
        (JCLASS==NVT_BOBD) .OR. (JCLASS==NVT_BOND) .OR. (JCLASS==NVT_SHRB)) THEN
        PFZERO (:) = PFZERO (:) + ((XAW - LOG(PGMES(:)*1000.0))/XBW)*PVEGTYPE(:,JCLASS)
     ELSE IF (JCLASS==NVT_TRBE) THEN
        PFZERO (:) = PFZERO (:) + XFZEROTROP(IRAD) * PVEGTYPE(:,JCLASS)
     ELSE
        PFZERO (:) = PFZERO (:) + XFZERO2 (ICO2TYPE) * PVEGTYPE(:,JCLASS)
     ENDIF
  ENDIF
  !
  PEPSO  (:) = PEPSO  (:) + XEPSO  (ICO2TYPE) * PVEGTYPE(:,JCLASS)
  PGAMM  (:) = PGAMM  (:) + XGAMM  (ICO2TYPE) * PVEGTYPE(:,JCLASS)
  PQDGAMM(:) = PQDGAMM(:) + XQDGAMM(ICO2TYPE) * PVEGTYPE(:,JCLASS)
  PQDGMES(:) = PQDGMES(:) + XQDGMES(ICO2TYPE) * PVEGTYPE(:,JCLASS)
  PT1GMES(:) = PT1GMES(:) + XT1GMES(ICO2TYPE) * PVEGTYPE(:,JCLASS)
  PT2GMES(:) = PT2GMES(:) + XT2GMES(ICO2TYPE) * PVEGTYPE(:,JCLASS)
  PQDAMAX(:) = PQDAMAX(:) + XQDAMAX(ICO2TYPE) * PVEGTYPE(:,JCLASS)
  PT1AMAX(:) = PT1AMAX(:) + XT1AMAX(ICO2TYPE) * PVEGTYPE(:,JCLASS)
  PT2AMAX(:) = PT2AMAX(:) + XT2AMAX(ICO2TYPE) * PVEGTYPE(:,JCLASS)
  PAH    (:) = PAH    (:) + XAH    (ICO2TYPE) * PVEGTYPE(:,JCLASS)
  PBH    (:) = PBH    (:) + XBH    (ICO2TYPE) * PVEGTYPE(:,JCLASS)
  !
  IF(I%LAGRI_TO_GRASS.AND.(JCLASS==NVT_C3 .OR. JCLASS==NVT_C4 .OR. JCLASS==NVT_IRR))THEN
    ICLASS=NVT_GRAS
  ELSE
    ICLASS=JCLASS
  ENDIF    
  !
  PTAU_WOOD(:) = PTAU_WOOD(:) + XTAU_WOOD(ICLASS) * PVEGTYPE(:,JCLASS)
  PAMAX    (:) = PAMAX    (:) + XAMAX    (ICLASS) * PVEGTYPE(:,JCLASS)
  !
END DO
!
PQDGAMM(:)=LOG(PQDGAMM(:))
PQDGMES(:)=LOG(PQDGMES(:))
PQDAMAX(:)=LOG(PQDAMAX(:))
!
!
! INITIALIZE VARIOUS VARIABLES FOR CO2 MODEL:
! -------------------------------------------
!
!
! compute temperature responses:
!
!before optimization (with non log PQDGAMM) : 
!ZGAMMT(:) = PGAMM(:)*(PQDGAMM(:)**(0.1*(ZTOPT(:)-25.0)))
ZWORK (:) = (0.1*(ZTOPT(:)-25.0)) * PQDGAMM(:)
ZGAMMT(:) = PGAMM(:)*EXP(ZWORK(:))
!
!before optimization (with non log PQDAMAX) :
!ZANMAX(:) = ( PAMAX(:)*PQDAMAX(:)**(0.1*(ZTOPT(:)-25.0)) ) / ...
ZWORK (:) = (0.1*(ZTOPT(:)-25.0)) * PQDAMAX(:)
ZANMAX(:) = ( PAMAX(:)*EXP(ZWORK(:)) )                   &
               /( (1.0+EXP(0.3*(PT1AMAX(:)-ZTOPT(:))))*  &
                  (1.0+EXP(0.3*(ZTOPT(:)-PT2AMAX(:)))) )  
!
!before optimization (with non log PQDGMES) :
!ZGMEST(:) = ( PGMES(:)*PQDGMES(:)**(0.1*(ZTOPT(:)-25.0)) )    &
ZWORK (:) = (0.1*(ZTOPT(:)-25.0)) * PQDGMES(:)
ZGMEST(:) = ( PGMES(:)*EXP(ZWORK(:)) )                   &
               /( (1.0+EXP(0.3*(PT1GMES(:)-ZTOPT(:))))*  &
                  (1.0+EXP(0.3*(ZTOPT(:)-PT2GMES(:)))) )  
!
!
! initialize other variables: (using optimum values for some variables)
!
ZCO2INIT3(:) = XDSPOPT
ZCO2INIT4(:) = XIAOPT
ZCO2INIT5(:) = 1.0
!
! Define a dummy LAI from top (zco2init2=0.1) and total lai (zco2init=1) for Dark respiration extinction parameterization 
!
ZCO2INIT2(:) = 0.1
ZCO2INIT1(:) = 1.0
!
! Add soil moisture stress effect to leaf conductance:
!
ZGMEST(:) = ZGMEST(:)*ZCO2INIT5(:)
!
! Initialise DMAX following Calvet (2000) in the case of 'AST' or 'LST' photosynthesis option
!
IF((HPHOTO=='AST').OR.(HPHOTO=='LST').OR.(HPHOTO=='NIT').OR.(HPHOTO=='NCB')) THEN
   ZDMAX(:) = EXP((LOG(ZGMEST(:)*1000.)-PAH(:))/PBH(:))/1000.
ELSE
   ZDMAX(:) = PDMAX(:)
ENDIF
!
! Compute maximum/initial/optimum net assimilation of CO2:
!
! Unit conversion with a constant value of 1.2 for PRHOA as it is not known here
! ZANMAX and ZEPSO from kgCO2/m2/s to kgCO2/kgair m/s by dividing by RHOA (kgair/m3)
! ZGAMMT from ppm to kgCO2/kgair
ZANMAX(:)=ZANMAX(:)/1.2
ZEPSO(:)=PEPSO(:)/1.2
ZGAMMT(:)=ZGAMMT(:)*XMCO2/XMD*1e-6
!
 CALL COTWO(PCO2, ZCO2INIT5, ZCO2INIT4, ZCO2INIT3, ZGAMMT, &
           PFZERO, ZEPSO, ZANMAX, ZGMEST, PGC, ZDMAX,     &
           PANMAX, ZGS, ZRDK, ZCO2INIT2, ZCO2INIT1        )                     
! change by sebastien PEPSO change into ZEPSO for units consistency
!
!
!
IF (LHOOK) CALL DR_HOOK('COTWOINIT_N',1,ZHOOK_HANDLE)
!
END SUBROUTINE COTWOINIT_n
