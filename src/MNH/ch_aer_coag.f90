!ORILAM_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!ORILAM_LIC This is part of the ORILAM software governed by the CeCILL-C licence
!ORILAM_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!ORILAM_LIC for details.
!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! MASDEV4_7 chimie 2006/05/18 13:07:25
!-----------------------------------------------------------------
!!   ########################
     MODULE MODI_CH_AER_COAG
!!   ########################
!!
INTERFACE
!!
SUBROUTINE CH_AER_COAG(PM,PLNSIG,PRG,PN,PDMINTRA,PDMINTER,&
                       PTEMP,PMU,PLAMBDA,PRHOP            )
IMPLICIT NONE
REAL, DIMENSION(:,:), INTENT(INOUT) :: PM,PRHOP 
REAL, DIMENSION(:),   INTENT(INOUT) :: PLAMBDA, PMU
REAL, DIMENSION(:,:), INTENT(INOUT) :: PDMINTRA
REAL, DIMENSION(:,:), INTENT(INOUT) :: PDMINTER
REAL, DIMENSION(:),   INTENT(IN)    :: PTEMP
REAL, DIMENSION(:,:), INTENT(IN)    :: PLNSIG, PRG, PN
END SUBROUTINE CH_AER_COAG
!!
END INTERFACE
!!
END MODULE MODI_CH_AER_COAG
!!
!!   #############################################
     SUBROUTINE CH_AER_COAG(PM,PLNSIG,PRG,PN,PDMINTRA,PDMINTER,&
                            PTEMP,PMU,PLAMBDA,PRHOP            )
!!   #############################################
!!
!!   PURPOSE
!!   -------
!!   Compute the terms due to Brownian, turbulent and gravitational
!!   coagulation: 
!!   a set of arrays are used to evaluate the double integral
!!   Based on Whitby et al. 1991 : Appendix H
!!
!!   METHOD
!!   ------
!!   * Arrays of numerical evaluation of coagulation terms
!!     in the free-molecule regime (computed from the ESMAP code)
!!     ZINTRA     - Intamodal coagulation, mode i,j 0th and 6th Moment
!!     ZINTER0I   - Intermodal coagulation, mode i, 0th Moment
!!     ZINTER3I   - Intermodal coagulation, mode i, 3rd Moment
!!     ZINTER6I   - Intermodal coagulation, mode i, 6th Moment
!!     ZINTER6J   - Intermodal coagulation, mode j, 6th Moment
!!
!!   * Variables used during the coefficients evaluation
!!     ZXINT(i)   - Variables values where the interpolation
!!                  is to be made
!!
!!   * intramodal coagulation terms
!!     ZXINTRAMIN     - Minimal value of ln(sigma)
!!     ZXINTRAMAX     - Maximal value of ln(sigma)
!!     ZDXINTRA       - Step of ln(sigma) in the array
!!
!!   * intermodal coagulation terms:
!!     ZXINTERMIN(i)  - Minimal value of the variable i
!!     ZXINTERMAX(i)  - Maximal value of the variable i
!!     ZDXINTER(i)    - Step of the variable i in the arrays
!!     i=1           - ln(sigmaj)
!!     i=2           - ln(sigmai)
!!     i=3           - ln((ZR=Rgj/Rgi)**2)
!!
!!   EXTERNAL
!!   -------
!!
!!   IMPLICIT ARGUMENTS
!!   ------------------
!!
!!   REFERENCE
!!   ---------
!!   none
!!
!!   AUTHOR
!!   ------
!!   Vincent Crassier (LA)
!!
!!   MODIFICATIONS
!!   -------------
!!   October 2018 J. Pianezze - Add comments, cleaning and debug
!!                              + move mode merging into ch_aer_driver
!!
!!
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
! 
USE MODD_CH_AEROSOL
USE MODD_CST,       ONLY : XPI, XBOLTZ
USE MODD_CONF,      ONLY : NVERB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!
REAL, DIMENSION(:,:), INTENT(INOUT) :: PM,PRHOP 
REAL, DIMENSION(:),   INTENT(INOUT) :: PLAMBDA, PMU
REAL, DIMENSION(:,:), INTENT(INOUT) :: PDMINTRA
REAL, DIMENSION(:,:), INTENT(INOUT) :: PDMINTER
REAL, DIMENSION(:),   INTENT(IN)    :: PTEMP
REAL, DIMENSION(:,:), INTENT(IN)    :: PLNSIG, PRG, PN
!
!*       0.2   Declarations of local variables
!
INTEGER                             :: JI,JJ
!
REAL, DIMENSION(SIZE(PM,1))         :: ZKFM,ZKNC
REAL, DIMENSION(SIZE(PM,1))         :: ZR,ZR2,ZR4
REAL, DIMENSION(SIZE(PM,1))         :: ZRM,ZRM2,ZRM3
REAL, DIMENSION(SIZE(PM,1))         :: ZKNG
REAL, DIMENSION(SIZE(PM,1))         :: ZAI,ZKNGI,ZAJ,ZKNGJ
REAL, DIMENSION(SIZE(PM,1))         :: ZINTRA0NC,ZINTRA0FM,ZINTRA0
REAL, DIMENSION(SIZE(PM,1))         :: ZINTRA6NC,ZINTRA6FM,ZINTRA6
REAL, DIMENSION(SIZE(PM,1))         :: ZINTERNC,ZINTERFM,ZINTER
REAL, DIMENSION(SIZE(PM,1))         :: ZAPPROX
!
REAL, DIMENSION(SIZE(PM,1))         :: ZRGJ, ZRGI, ZRG
!
REAL, DIMENSION(SIZE(PM,1),JPMODE)  :: ZESG01,ZESG04,ZESG05,ZESG08,ZESG09
REAL, DIMENSION(SIZE(PM,1),JPMODE)  :: ZESG12,ZESG16
REAL, DIMENSION(SIZE(PM,1),JPMODE)  :: ZESG20,ZESG24,ZESG25,ZESG28
REAL, DIMENSION(SIZE(PM,1),JPMODE)  :: ZESG32,ZESG36
REAL, DIMENSION(SIZE(PM,1),JPMODE)  :: ZESG49
REAL, DIMENSION(SIZE(PM,1),JPMODE)  :: ZESG52
REAL, DIMENSION(SIZE(PM,1),JPMODE)  :: ZESG64
REAL, DIMENSION(SIZE(PM,1),JPMODE)  :: ZESG81,ZESG85
REAL, DIMENSION(SIZE(PM,1),JPMODE)  :: ZESG100,ZESG121,ZESG144,ZESG169,ZESG196
REAL, DIMENSION(SIZE(PM,1),JPMODE)  :: ZESG256
REAL, DIMENSION(SIZE(PM,1))         :: ZRB0,ZRB6
REAL, DIMENSION(SIZE(PM,1))         :: ZRES
!
!-------------------------------------------------------------------------------
!
!*       1.    INITIALIZATION
!              --------------
!
PDMINTRA(:,:) = 0.0
PDMINTER(:,:) = 0.0
!
ZKNC(:)       = 2.*XBOLTZ*PTEMP(:)/(3.*PMU(:))
!
! Compute coagulation coefficients : Whitby et al. 1991 : Appendix H
!
ZESG01 (:,:) = EXP(0.125*PLNSIG(:,:)**2)            
ZESG04 (:,:) = ZESG01 (:,:) ** 4            
ZESG05 (:,:) = ZESG04 (:,:) * ZESG01 (:,:)                        
ZESG08 (:,:) = ZESG04 (:,:) * ZESG04 (:,:)                        
ZESG09 (:,:) = ZESG04 (:,:) * ZESG05 (:,:)            
ZESG12 (:,:) = ZESG04 (:,:) * ZESG04 (:,:) * ZESG04(:,:)
ZESG16 (:,:) = ZESG08 (:,:) * ZESG08 (:,:)
ZESG20 (:,:) = ZESG16 (:,:) * ZESG04 (:,:)
ZESG24 (:,:) = ZESG12 (:,:) * ZESG12 (:,:)
ZESG25 (:,:) = ZESG16 (:,:) * ZESG09 (:,:)
ZESG28 (:,:) = ZESG20 (:,:) * ZESG08 (:,:)
ZESG32 (:,:) = ZESG16 (:,:) * ZESG16 (:,:)
ZESG36 (:,:) = ZESG16 (:,:) * ZESG20 (:,:)
ZESG49 (:,:) = ZESG25 (:,:) * ZESG20 (:,:) * ZESG04(:,:)
ZESG52 (:,:) = ZESG16 (:,:) * ZESG36 (:,:)
ZESG64 (:,:) = ZESG32 (:,:) * ZESG32 (:,:)
ZESG81 (:,:) = ZESG49 (:,:) * ZESG32 (:,:)
ZESG85 (:,:) = ZESG64 (:,:) * ZESG20 (:,:) * ZESG01(:,:)
ZESG100(:,:) = ZESG36 (:,:) * ZESG64 (:,:) 
ZESG121(:,:) = ZESG85 (:,:) * ZESG36 (:,:)
ZESG144(:,:) = ZESG100(:,:) * ZESG36 (:,:) * ZESG08(:,:)
ZESG169(:,:) = ZESG144(:,:) * ZESG25 (:,:)
ZESG196(:,:) = ZESG144(:,:) * ZESG52 (:,:)
ZESG256(:,:) = ZESG144(:,:) * ZESG100(:,:) * ZESG12(:,:)
!
!-------------------------------------------------------------------------------
!
!*       2.    COMPUTE INTRA-MODAL COAGULATION TERMS
!              -------------------------------------
!
DO JI=1,JPMODE
  !
  !*          2.0 INITIALIZATION
  !               --------------
  !
  ZRG(:)  = PRG(:,JI)
  ZKNG(:) = PLAMBDA(:)/ZRG(:)
  ZAI(:)  = 1.392*ZKNG(:)**0.0783
  ZKFM(:) = SQRT(3.*XBOLTZ*PTEMP(:)/PRHOP(:,JI))*1.E-3
  !   
  ZRB0(:) = 0.8
  ZRB6(:) = ZRB0
  !    
  !*          2.1 FREE-MOLECULE REGIME (KN > 10) 
  !               ------------------------------
  !
  ZINTRA0FM(:) = ZKFM(:) * ZRB0(:) * SQRT(2.*ZRG(:)) *     &
               & (ZESG01(:,JI)+ZESG25(:,JI)+2.*ZESG05(:,JI))
  ZINTRA6FM(:) = ZKFM(:)*ZRB6(:)*sqrt(ZRG(:))**13*sqrt(2.)*ZESG85(:,JI) * &
               & (1.+2.*ZESG04(:,JI)+ZESG24(:,JI))
  !
  !*          2.2 NEAR-CONTINUUM (KN < 0.1)
  !               -------------------------
  !
  ZINTRA0NC(:) = ZKNC(:)*(1.+ZESG08(:,JI)+ZAI(:)*ZKNG(:)*(ZESG20(:,JI)+ZESG04(:,JI)))
  ZINTRA6NC(:) = 2.*ZKNC(:)*(ZRG(:))**6*ZESG52(:,JI)*(ZESG20(:,JI)+ZESG28(:,JI)+&
                 ZAI(:)*ZKNG(:)*(1.+ZESG16(:,JI)))
  !
  !*         2.3 HARMONIC MEAN
  !              -------------
  !
  ZINTRA0(:) = ZINTRA0FM(:)*(ZINTRA0NC(:) / (ZINTRA0FM(:)+ZINTRA0NC(:)))
  ZINTRA6(:) = ZINTRA6FM(:)*(ZINTRA6NC(:) / (ZINTRA6FM(:)+ZINTRA6NC(:)))
  !
  PDMINTRA(:,NM0(JI)) = - ZINTRA0(:) * PM(:,NM0(JI))**2.0
  PDMINTRA(:,NM3(JI)) =   0.0
  PDMINTRA(:,NM6(JI)) =   ZINTRA6(:) * PM(:,NM0(JI))**2.0
  !
ENDDO
!
!-------------------------------------------------------------------------------
!
!*       3.    COMPUTE INTER-MODAL COAGULATION TERMS
!              -------------------------------------
!
! JPMODE = 2
! donc :
!   - JI = 1
!   - JJ = 2
!
DO JI=1,(JPMODE-1)
  DO JJ=(JI+1),JPMODE
    !
    ZRGI (:) = PRG(:,JI)
    ZKNGI(:) = PLAMBDA(:)/ZRGI(:)
    ZAI  (:) = 1.392*ZKNGI(:)**0.0783
    !
    ZRGJ (:) = PRG(:,JJ)
    ZKNGJ(:) = PLAMBDA(:)/ZRGJ(:)
    ZAJ  (:) = 1.392*ZKNGJ(:)**0.0783
    !      
    ZR  (:)  = SQRT(ZRGJ(:)/ZRGI(:))
    ZR2 (:)  = ZR(:)*ZR(:)
    ZR4 (:)  = ZR2(:)*ZR2(:)
    ZRM (:)  = 1./ZR(:)
    ZRM2(:)  = ZRM(:)*ZRM(:)
    ZRM3(:)  = ZRM(:)*ZRM2(:)
    !
    !    
    ! * 3.1 Free-Molecule Regime (Kn > 10) : appendix H.2.2.1 - Whitby et al. 1991
    !       ----------------------------------------------------------------------
    !
    ZRES(:)=0.9
    !
    !       moment 0
    !
    ZAPPROX(:)=sqrt(2.*ZRGI(:))*(ZESG01(:,JI)+ZR(:)*ZESG01(:,JJ)+2.*ZR2(:)*ZESG01(:,JI)*ZESG04(:,JJ)&
               +ZR4(:)*ZESG09(:,JI)*ZESG16(:,JJ)+ZRM3(:)*ZESG16(:,JI)*ZESG09(:,JJ)+&
               2.*ZRM(:)*ZESG04(:,JI)*ZESG01(:,JJ))
       
    ZINTERFM(:)=ZKFM(:)*ZRES(:)*ZAPPROX(:)

    ZAPPROX(:)=2.+ZAI(:)*ZKNGI(:)*(ZESG04(:,JI)+ZR2(:)*ZESG16(:,JI)*ZESG04(:,JJ))+&
                ZAJ(:)*ZKNGJ(:)*(ZESG04(:,JJ)+ZRM2(:)*ZESG16(:,JJ)*ZESG04(:,JI))+&
                (ZR2(:)+ZRM2(:))*(ZESG04(:,JI)*ZESG04(:,JJ))

    ZINTERNC(:)=ZKNC(:)*ZAPPROX(:)

    ZINTER(:)=ZINTERNC(:)*(ZINTERFM(:)/(ZINTERNC(:)+ZINTERFM(:)))

    PDMINTER(:,NM0(JI))=-PM(:,NM0(JI))*PM(:,NM0(JJ))*ZINTER(:)
    PDMINTER(:,NM0(JJ))= 0.0
    !
    !       moment 3
    !
    ZAPPROX(:)=sqrt(2.)*sqrt(ZRGI(:))**7*(ZESG49(:,JI)+ZR(:)*ZESG36(:,JI)*ZESG01(:,JJ)+2.*ZR2(:)*&
                 ZESG25(:,JI)*ZESG04(:,JJ)+ZR4(:)*ZESG09(:,JI)*ZESG16(:,JJ)+ZRM3(:)*&
                 ZESG100(:,JI)*ZESG09(:,JJ)+2.*ZRM(:)*ZESG64(:,JI)*ZESG01(:,JJ))

    ZINTERFM(:)=ZKFM(:)*ZRES(:)*ZAPPROX(:)

    ZAPPROX(:)=(2.*ZESG36(:,JI)+ZAI(:)*ZKNGI(:)*(ZESG16(:,JI)+ZR2(:)*ZESG04(:,JI)*ZESG04(:,JJ))+&
    ZAJ(:)*ZKNGJ(:)*(ZESG36(:,JI)*ZESG04(:,JJ)+ZRM2(:)*ZESG16(:,JJ)*ZESG64(:,JI))+&
    ZR2(:)*ZESG16(:,JI)*ZESG04(:,JJ)+ZRM2(:)*ZESG64(:,JI)*ZESG04(:,JJ))*(ZRGI(:))**3      

    ZINTERNC(:)=ZKNC(:)*ZAPPROX(:)

    ZINTER(:)=ZINTERNC(:)*(ZINTERFM(:)/(ZINTERNC(:)+ZINTERFM(:)))
       
    PDMINTER(:,NM3(JI))=-PM(:,NM0(JI))*PM(:,NM0(JJ))*ZINTER(:)
    PDMINTER(:,NM3(JJ))=-PDMINTER(:,NM3(JI))

    !       moment 6

    ZAPPROX(:)=sqrt(2.)*sqrt(ZRGI(:))**13*(ZESG169(:,JI)+ZR(:)*ZESG144(:,JI)*ZESG01(:,JJ)+&
           2.*ZR2(:)*ZESG121(:,JI)*ZESG04(:,JJ)+ZR4(:)*ZESG81(:,JI)*ZESG16(:,JJ)+&
           ZRM3(:)*ZESG256(:,JI)*ZESG09(:,JJ)+2*ZRM(:)*ZESG196(:,JI)*ZESG01(:,JJ))
      
    ZINTERFM(:)=ZKFM(:)*ZRES(:)*ZAPPROX(:)

    ZAPPROX(:)=(ZRGI(:))**6*(2.*ZESG144(:,JI)+ZAI(:)*ZKNGI(:)*(ZESG100(:,JI)+&
       ZR2(:)*ZESG64(:,JI)*ZESG04(:,JJ))+ZAJ(:)*ZKNGJ(:)*(ZESG144(:,JI)*ZESG04(:,JJ)+&
       ZRM2(:)*ZESG196(:,JI)*ZESG16(:,JJ))+ZR2(:)*ZESG100(:,JI)*ZESG04(:,JJ)+&
       ZRM2(:)*ZESG196(:,JI)*ZESG04(:,JJ))

    ZINTERNC(:)=ZKNC(:)*ZAPPROX(:)

    ZINTER(:)=ZINTERNC(:)*(ZINTERFM(:)/(ZINTERNC(:)+ZINTERFM(:)))

    PDMINTER(:,NM6(JI))=-PM(:,NM0(JI))*PM(:,NM0(JJ))*ZINTER(:)

    ZAPPROX(:)=sqrt(2.)*sqrt(ZRGI(:))**7*sqrt(ZRGJ(:))**6*(ZESG49(:,JI)*&
           ZESG36(:,JJ)+ZR(:)*ZESG36(:,JI)*ZESG49(:,JJ)+2.*ZR2(:)*ZESG25(:,JI)*&
           ZESG64(:,JJ)+ZR4(:)*ZESG09(:,JI)*ZESG100(:,JJ)+ZRM3(:)*ZESG100(:,JI)*&
           ZESG09(:,JJ)+2.*ZRM(:)*ZESG64(:,JI)*ZESG25(:,JJ))
       
    ZINTERFM(:)=ZKFM(:)*ZRES(:)*ZAPPROX(:)

    ZAPPROX(:)=(ZRGI(:))**3*(ZRGJ(:))**3*(2.*ZESG36(:,JI)*ZESG36(:,JJ)+&
       ZAI(:)*ZKNGI(:)*(ZESG16(:,JI)*ZESG16(:,JJ)+ZR2(:)*ZESG04(:,JI)*ZESG64(:,JJ))+&
       ZAJ(:)*ZKNGJ(:)*(ZESG36(:,JI)*ZESG16(:,JJ)+ZRM2(:)*ZESG64(:,JI)*ZESG04(:,JJ))+&
       ZR2(:)*ZESG16(:,JI)*ZESG64(:,JJ)+ZRM2(:)*ZESG64(:,JI)*ZESG16(:,JJ))

    ZINTERNC(:)=ZKNC(:)*ZAPPROX(:)

    ZINTER(:)=ZINTERNC(:)*(ZINTERFM(:)/(ZINTERNC(:)+ZINTERFM(:)))
       
    PDMINTER(:,NM6(JJ))=-PDMINTER(:,NM6(JI))+2.*PM(:,NM0(JI))*PM(:,NM0(JJ))*ZINTER(:)
 
  ENDDO
ENDDO

END SUBROUTINE CH_AER_COAG 
