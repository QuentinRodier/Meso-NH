!     #########
      SUBROUTINE SOILDIF( HSCOND, HDIFSFCOND,                                    &
                         PVEG, PCV, PFFG, PFFV,                                  &
                         PCG, PCGMAX, PCT, PFROZEN1,                             &
                         PD_G, PTG, PWG, PWGI, KWG_LAYER,                        &
                         PHCAPSOILZ, PCONDDRYZ, PCONDSLDZ,                       &
                         PBCOEF, PWSAT, PMPOTSAT, PSOILCONDZ, PSOILHCAPZ         )  
!     ##########################################################################
!
!!****  *SOIL*  
!!
!!    PURPOSE
!!    -------
!
!     Calculates the coefficients related to the soil (i.e., surface heat capacities, CG, CT,
!     and thermal conductivity and heat capacity profiles)
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
!!	A. Boone           * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    
!!                  25/03/99      (Boone)   Added Johansen (1975)/Peters-Lidard 
!!                                          option to explicitly compute CG
!!                  08/25/02      (Boone)   DIF option code only
!!                  25/05/08     (Decharme) Add Flood properties
!!                  03/08/11     (Decharme) Optimization
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_CSTS,       ONLY : XCL, XCI, XRHOLW, XRHOLI, XPI, XDAY, XCONDI, XTT, XLMTT, XG
USE MODD_ISBA_PAR,   ONLY : XCONDWTR, XWGMIN
USE MODD_SURF_PAR,   ONLY : XUNDEF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
 CHARACTER(LEN=*),     INTENT(IN)   :: HSCOND  ! thermal conductivity formulation
!                                             ! 'NP89' :  Noilhan and Planton 
!                                             !  (1989: McCumber-Pielke (1981) and
!                                             !  Clapp and Hornberger (1978))
!                                             ! 'PL98' Method of Johansen (1975) as
!                                             ! presented by Peters-Lidard (JAS: 1998)
!
 CHARACTER(LEN=*),     INTENT(IN)  :: HDIFSFCOND ! NOTE: Only used when HISBA = DIF
!                                               ! MLCH' = include the insulating effect of leaf
!                                               !         litter/mulch on the surface thermal cond.
!                                               ! 'DEF' = no mulch effect
!
REAL, DIMENSION(:), INTENT(IN)    :: PVEG, PCV
!                                      Soil and vegetation parameters
!                                      PVEG = fraction of vegetation
!                                      PCV  = the heat capacity of the vegetation
!
REAL, DIMENSION(:,:), INTENT(IN)  :: PHCAPSOILZ, PCONDDRYZ, PCONDSLDZ, PD_G 
!                                    PHCAPSOILZ = soil heat capacity [J/(K m3)]
!                                    PCONDDRYZ  = soil dry thermal conductivity 
!                                                 [W/(m K)] 
!                                    PCONDSLDZ  = soil solids thermal conductivity 
!                                                 [W/(m K)]
!                                    PD_G       = soil layer depth [m]
!
REAL, DIMENSION(:,:), INTENT(IN)  :: PBCOEF, PWSAT, PMPOTSAT, PTG
!                                    PBCOEF   = profile of b-parameter (-)
!                                    PWSAT    = profile of porosity (m3/m3)
!                                    PMPOTSAT = profile of matric potential at saturation (m)
!
REAL, DIMENSION(:,:),INTENT(INOUT):: PWG, PWGI
!                                    PWG    = soil liquid water content (m3/m3)
!                                    PWGI   = soil frozen water content (m3/m3)
!
INTEGER, DIMENSION(:), INTENT(IN) :: KWG_LAYER  
!                                    KWG_LAYER = Number of soil moisture layers (DIF option)
!
REAL, DIMENSION(:), INTENT(OUT)   :: PFROZEN1, PCG, PCT
!                                      PFROZEN1 = fraction of ice in superficial soil
!                                      PCT      = averaged surface heat capacity of the grid (m2 K J-1)
!                                      PCG      = averaged surface soil heat capacity (m2 K J-1)
!
REAL,               INTENT(IN)    :: PCGMAX
!                                      Maximum soil heat capacity
!
REAL, DIMENSION(:,:), INTENT(OUT) :: PSOILCONDZ, PSOILHCAPZ
!                                    PSOILHCAP = soil heat capacity        (J m-3 K-1)
!                                    PSOILCOND = soil thermal conductivity (W m-1 K-1)
!
!
REAL, DIMENSION(:), INTENT(IN)   :: PFFV, PFFG
!                                   PFFG = Floodplain fraction over the ground
!                                   without snow (ES)
!                                   PFFV = Floodplain fraction over vegetation
!                                   without snow (ES)
!
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(SIZE(PWG,1),SIZE(PWG,2)) :: ZMATPOT
!                                           ZMATPOT    = soil matric potential (m)
!
REAL                         :: ZFROZEN2DF, ZUNFROZEN2DF, ZCONDSATDF, ZLOG_CONDI, ZLOG_CONDWTR, &
                                ZSATDEGDF, ZKERSTENDF, ZWORK1, ZWORK2, ZWORK3, ZLOG, ZWTOT
                                
!
REAL, PARAMETER              :: ZVEGMULCH  = 0.10 ! reduction factor for the surface layer
!                                                 ! thermal condictivity due to the presence
!                                                 ! of mulch/organic material (ISBA-DF)
!                                                 ! Set to 1 to remove this effect. 
!
REAL, DIMENSION(SIZE(PVEG)) :: ZFF, ZCF !heat capacity of the flood
!
REAL, DIMENSION(SIZE(PVEG)) :: ZWTD ! Water table depth (m)  
!
INTEGER :: INI, INL, JJ, JL, IDEPTH
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!*       0.     Initialization:
!               ---------------
!
IF (LHOOK) CALL DR_HOOK('SOILDIF',0,ZHOOK_HANDLE)
!
INI=SIZE(PWG,1)
INL=SIZE(PWG,2)
!
ZCF    (:)   = XUNDEF
!
!-------------------------------------------------------------------------------
!
!*       1.     MATRIC POTENTIAL AND MOISTURE EXTRAPOLATION
!               -------------------------------------------
!
!
!Water Table Depth (m) 
!WTD will be able to evolve according to observations or MODCOU/TRIP inputs 
ZWTD(:)=100.
!
DO JL=1,INL
   DO JJ=1,INI
!   
      IDEPTH=KWG_LAYER(JJ)
      IF(JL>IDEPTH)THEN
!                           
!       total matric potential
        ZWORK1  = MIN(1.0,(PWG(JJ,IDEPTH)+PWGI(JJ,IDEPTH))/PWSAT(JJ,IDEPTH))
        ZLOG    = PBCOEF(JJ,IDEPTH)*LOG(ZWORK1)
        ZMATPOT(JJ,IDEPTH) = PMPOTSAT(JJ,IDEPTH)*EXP(-ZLOG)

!       extrapolation of total matric potential
        ZWORK1         = 0.5*(PD_G(JJ,IDEPTH)+PD_G(JJ,IDEPTH-1))
        ZWORK2         = 0.5*(PD_G(JJ,JL)+PD_G(JJ,JL-1))
        ZWORK3         = (PMPOTSAT(JJ,JL)-ZMATPOT(JJ,IDEPTH))/MAX(1.E-6,ZWTD(JJ)-ZWORK1)
        ZMATPOT(JJ,JL) = PMPOTSAT(JJ,JL)-ZWORK3*MAX(0.0,ZWTD(JJ)-ZWORK2)
!
!       total soil water content computation
        ZWORK1      = MAX(1.0,ZMATPOT(JJ,JL)/PMPOTSAT(JJ,JL))
        ZLOG        = LOG(ZWORK1)/PBCOEF(JJ,JL)
        ZWTOT       = PWSAT(JJ,JL)*EXP(-ZLOG)
        ZWTOT       = MAX(XWGMIN,ZWTOT)
!
!       soil liquid water content computation
        ZMATPOT(JJ,JL) = ZMATPOT(JJ,JL) + XLMTT*MIN(0.0,PTG(JJ,JL)-XTT)/(XG*PTG(JJ,JL))
!        
        ZWORK1      = MAX(1.0,ZMATPOT(JJ,JL)/PMPOTSAT(JJ,JL))
        ZLOG        = LOG(ZWORK1)/PBCOEF(JJ,JL)
        PWG (JJ,JL) = PWSAT(JJ,JL)*EXP(-ZLOG)
        PWG (JJ,JL) = MAX(XWGMIN,PWG(JJ,JL))
!        
!       soil ice computation        
        PWGI(JJ,JL) = MAX(0.0,ZWTOT-PWG(JJ,JL))
!        
      ENDIF
   ENDDO
ENDDO
!
!-------------------------------------------------------------------------------
!
!*       2.     THE HEAT CAPACITY OF BARE-GROUND
!               --------------------------------
!               Explicit soil thermal diffusion option:
!
IF(HSCOND == 'NP89')THEN
!
! Calculate the thermal conductivity [W/(m K)] using the method of McCumber and 
! Pielke (1981) used implicitly by Noilhan and Planton (1989).
! First calculate the soil water potential using Clapp and Hornberger (1978)
! (m): NOTE that this method DOES NOT explicitly account for soil ice
! so use a ice-weighted average (to prevent excessively low values
! when a soil layer totally frozen): 
!
    DO JL=1,INL
     DO JJ=1,INI
!        
!       matric potential
        ZWORK1  = MIN(1.0,PWG(JJ,JL)/PWSAT(JJ,JL))
        ZLOG    = PBCOEF(JJ,JL)*LOG(ZWORK1)
        ZMATPOT(JJ,JL) = PMPOTSAT(JJ,JL)*EXP(-ZLOG)
!
!       thermal conductivity
        ZWORK1  = LOG10(-ZMATPOT(JJ,JL))+4.7
        ZWORK2  = 418.*EXP(-ZWORK1)
        ZWORK3  = MAX(0.171,ZWORK2)
        PSOILCONDZ(JJ,JL) = (1.0-PWSAT(JJ,JL)+PWG(JJ,JL)+PWGI(JJ,JL))   &        
                          / ((PWGI(JJ,JL)/XCONDI)+((1.0-PWSAT(JJ,JL)+PWG(JJ,JL))/ZWORK3))  
!
     ENDDO
  ENDDO
                         
!
ELSE
!
! Calculate thermal conductivity using PL98, but for explicit layers:
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
!Old: CONDSATDF=(CONDSLDZ**(1.0-WSAT))*(CONDI**(WSAT-UNFROZEN2DF))*(CONDWTR**UNFROZEN2DF)  
        ZWORK1      = LOG(PCONDSLDZ(JJ,JL))*(1.0-PWSAT(JJ,JL))
        ZWORK2      = ZLOG_CONDI*(PWSAT(JJ,JL)-ZUNFROZEN2DF)
        ZWORK3      = ZLOG_CONDWTR*ZUNFROZEN2DF        
        ZCONDSATDF  = EXP(ZWORK1+ZWORK2+ZWORK3)
!
        ZSATDEGDF   = MAX(0.1, PWG(JJ,JL)/PWSAT(JJ,JL))
        ZKERSTENDF  = LOG10(ZSATDEGDF) + 1.0
        ZKERSTENDF  = (1.0-ZFROZEN2DF)*ZKERSTENDF + ZFROZEN2DF *ZSATDEGDF  
!
! Thermal conductivity of soil:
!
        PSOILCONDZ(JJ,JL) = ZKERSTENDF*(ZCONDSATDF-PCONDDRYZ(JJ,JL)) + PCONDDRYZ(JJ,JL)  
!
     ENDDO
  ENDDO
!
ENDIF
!
! Surface soil water reservoir frozen fraction:
!
PFROZEN1(:) = PWGI(:,1)/(PWGI(:,1) + MAX(PWG(:,1),XWGMIN))
!
! This takes into account the insulating effect of dead vegetation/leaf litter/mulch on
! the uppermost soil layer thermal conductivity: it is a simple modification
! of the ideas presented by Gonzalez-Sosa et al., AFM, 1999: the thermal
! conductivity is reduced by the factor 'ZVEGMULCH'. The main impact is
! to reduce the thermal coupling between the surface layer and the 
! sub-surface soil. In the limit when
! there is no vegetation, the conductivity collapses into the bare-soil value.
! If the option is not in force ( HDIFSFCOND /= 'MLCH') then use only soil
! properties (no mulch effect). 
!
IF(HDIFSFCOND == 'MLCH') PSOILCONDZ(:,1) = (1.0 - PVEG(:) * (1.0 - ZVEGMULCH)) * PSOILCONDZ(:,1) 
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
! Surface soil thermal inertia [(m2 K)/J]
!
PCG(:) = 2.*SQRT(XPI/(PSOILCONDZ(:,1)*PSOILHCAPZ(:,1)*XDAY))
PCG(:) = MIN( PCG(:), PCGMAX )
!
!-------------------------------------------------------------------------------
!
!*       3.     THE HEAT CAPACITY OF FLOOD
!               --------------------------------
!
ZFF(:) = PVEG(:)*PFFV(:) + (1.-PVEG(:))*PFFG(:)
!
WHERE (ZFF(:) > 0.)                                                 
       ZCF(:) = 2.0 * SQRT( XPI/(XCONDWTR*XRHOLW*XCL*XDAY) )
END WHERE                 
!
!-------------------------------------------------------------------------------
!
!*      4.      GRID-AVERAGED HEAT CAPACITY
!               ---------------------------
!
! With contribution from the ground, flood and vegetation for explicit
! (ISBA-ES) snow scheme option (i.e. no snow effects included here):
!
PCT(:) = 1. / ( (1.-PVEG(:))*(1.-PFFG(:)) / PCG(:)     &
                 +  PVEG(:) *(1.-PFFV(:)) / PCV(:)     &
                 +  ZFF (:)               / ZCF(:)     )  
!
!
!
!-------------------------------------------------------------------------------
!
!*      5.      RESTORE DEFAULT VALUES
!               ----------------------
!
! restore default moisture and ice values under moisture soil depth
!
DO JL=1,INL
   DO JJ=1,INI
      IDEPTH=KWG_LAYER(JJ)
      IF(JL>IDEPTH)THEN
        PWG (JJ,JL) = XUNDEF
        PWGI(JJ,JL) = XUNDEF
      ENDIF
   ENDDO
ENDDO
!
IF (LHOOK) CALL DR_HOOK('SOILDIF',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE SOILDIF
