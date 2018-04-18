!MNH_LIC Copyright 1994-2018 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!     #######################
      MODULE MODI_C2R2_ADJUST
!     #######################
!
INTERFACE
!
      SUBROUTINE C2R2_ADJUST(KRR, TPFILE, HRAD,                           &
                             HTURBDIM, OCLOSE_OUT, OSUBG_COND, PTSTEP,    &
                             PRHODJ, PSIGS, PPABST,                       &
                             PTHS, PRVS, PRCS, PCNUCS,                    &
                             PCCS, PSRCS, PCLDFR, PRRS )
!
USE MODD_IO_ll, ONLY: TFILEDATA
!
INTEGER,                  INTENT(IN)    :: KRR      ! Number of moist variables
TYPE(TFILEDATA),          INTENT(IN)    :: TPFILE   ! Output file
CHARACTER*4,              INTENT(IN)    :: HTURBDIM ! Dimensionality of the
                                                    ! turbulence scheme
CHARACTER*4,              INTENT(IN)    :: HRAD     ! Radiation scheme name
LOGICAL,                  INTENT(IN)    :: OCLOSE_OUT ! Conditional closure of 
                                                    ! the OUTPUT FM-file
LOGICAL,                  INTENT(IN)    :: OSUBG_COND ! Switch for Subgrid 
                                                    ! Condensation
REAL,                     INTENT(IN)    :: PTSTEP   ! Double Time step
                                                   ! (single if cold start)
!
REAL, DIMENSION(:,:,:),   INTENT(IN)   ::  PRHODJ  ! Dry density * Jacobian
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PSIGS   ! Sigma_s at time t
REAL, DIMENSION(:,:,:),   INTENT(IN)   ::  PPABST  ! Absolute Pressure at t     
!
REAL, DIMENSION(:,:,:),   INTENT(INOUT)       :: PTHS  ! Theta source
REAL, DIMENSION(:,:,:),   INTENT(INOUT)       :: PRVS  ! Water vapor m.r. source
REAL, DIMENSION(:,:,:),   INTENT(INOUT)       :: PRCS  ! Cloud water m.r. source
REAL, DIMENSION(:,:,:),   INTENT(INOUT)    :: PCNUCS  ! Nucl. aero. conc. source
REAL, DIMENSION(:,:,:),   INTENT(INOUT)    :: PCCS    ! Cloud water conc. source
!
REAL, DIMENSION(:,:,:),   INTENT(OUT)   :: PSRCS   ! Second-order flux
                                                   ! s'rc'/2Sigma_s2 at time t+1
                                                   ! multiplied by Lambda_3
REAL, DIMENSION(:,:,:),   INTENT(OUT)   :: PCLDFR  ! Cloud fraction          
REAL, DIMENSION(:,:,:), OPTIONAL,  INTENT(IN) :: PRRS  ! Rain  water m.r. source
!
END SUBROUTINE C2R2_ADJUST
!
END INTERFACE
!
END MODULE MODI_C2R2_ADJUST
!     ##########################################################################
      SUBROUTINE C2R2_ADJUST(KRR, TPFILE, HRAD,                           &
                             HTURBDIM, OCLOSE_OUT, OSUBG_COND, PTSTEP,    &
                             PRHODJ, PSIGS, PPABST,                       &
                             PTHS, PRVS, PRCS, PCNUCS,                    &
                             PCCS, PSRCS, PCLDFR, PRRS )
!     ##########################################################################
!
!!****  *C2R2_ADJUST* -  compute the fast  microphysical sources for C2R2 or KHKO scheme
!!
!!    PURPOSE
!!    -------
!!    The purpose of this routine is to compute the fast microphysical sources
!!    through a saturation ajustement procedure.
!!
!!
!!**  METHOD
!!    ------
!!    Langlois, Tellus, 1973 for the cloudless version.
!!    When cloud water is taken into account, refer to book 1 of the
!!    documentation.
!!
!!     
!!
!!    EXTERNAL
!!    --------
!!      None
!!     
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!      Module MODD_CST
!!         XP00               ! Reference pressure
!!         XMD,XMV            ! Molar mass of dry air and molar mass of vapor
!!         XRD,XRV            ! Gaz constant for dry air, gaz constant for vapor
!!         XCPD,XCPV          ! Cpd (dry air), Cpv (vapor)
!!         XCL                ! Cl (liquid)
!!         XTT                ! Triple point temperature
!!         XLVTT              ! Vaporization heat constant
!!         XALPW,XBETAW,XGAMW ! Constants for saturation vapor 
!!                            !  pressure  function 
!!      Module  MODD_CONF 
!!         CCONF
!!      Module MODD_BUDGET:
!!         NBUMOD 
!!         CBUTYPE
!!         NBUPROCCTR 
!!         LBU_RTH    
!!         LBU_RRV  
!!         LBU_RRC  
!!      Module MODD_LES : NCTR_LES,LTURB_LES,NMODNBR_LES
!!                        XNA declaration (cloud fraction as global var)
!!
!!    REFERENCE
!!    ---------
!!
!!      Book 1 and Book2 of documentation ( routine FAST_TERMS )
!!      Langlois, Tellus, 1973
!!    AUTHOR
!!    ------
!!      E. Richard       * Laboratoire d'Aerologie*
!!   
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    20/12/94 
!!      Modifications: March 1, 1995 (J.M. Carriere) 
!!                                  Introduction of cloud water with order 1
!!                                  formulation
!!      Modifications: June 8, 1995 ( J.Stein )
!!                                  Cleaning 
!!      Modifications: August 30, 1995 ( J.Stein )
!!                                  add Lambda3 for the subgrid condensation
!!   
!!                     October 16, 1995 (J. Stein)     change the budget calls 
!!                     March   16, 1996 (J. Stein)     store the cloud fraction
!!                     April   03, 1996 (J. Stein)     displace the nebulosity
!!                                      computation in the all and nothing case
!!                     April   15, 1996 (J. Stein)     displace the lambda 3 
!!                         multiplication and change the nebulosity threshold
!!                     September 16, 1996  (J. Stein)  bug in the SG cond for
!!                                                     the M computation
!!                     October 10, 1996 (J. Stein)     reformulate the Subgrid
!!                                                     condensation scheme
!!                     October 8,  1996 (Cuxart,Sanchez) Cloud frac. LES diag (XNA)
!!                     December 6, 1996 (J.-P. Pinty)  correction of Delta_2
!!                     November 5, 1996 (J. Stein) remove Rnp<0 values
!!                     November 13 1996 (V. Masson) add prints in test above
!!                     March 11, 1997 (J.-M. Cohard)  C2R2 option
!!                     March 2006 (O.Geoffroy) Add KHKO scheme
!!                     October 2009 (G. Tanguy) add ILENCH=LEN(YCOMMENT) after
!!                                              change of YCOMMENT
!!  Philippe Wautelet: 05/2016-04/2018: new data structures and calls for I/O
!!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_BUDGET
USE MODD_CONF
USE MODD_CST
USE MODD_IO_ll,   ONLY: TFILEDATA
USE MODD_LUNIT_n, ONLY: TLUOUT
USE MODD_NSV,     ONLY: NSV_C2R2BEG
USE MODD_PARAMETERS
!
USE MODI_CONDENS
USE MODI_BUDGET
!
USE MODE_FIELD
USE MODE_FMWRIT
USE MODE_MSG
!
IMPLICIT NONE
!
!*       0.1   Declarations of dummy arguments :
!
!
INTEGER,                  INTENT(IN)    :: KRR      ! Number of moist variables
TYPE(TFILEDATA),          INTENT(IN)    :: TPFILE   ! Output file
CHARACTER*4,              INTENT(IN)    :: HTURBDIM ! Dimensionality of the
                                                    ! turbulence scheme
CHARACTER*4,              INTENT(IN)    :: HRAD     ! Radiation scheme name
LOGICAL,                  INTENT(IN)    :: OCLOSE_OUT ! Conditional closure of 
                                                    ! the OUTPUT FM-file
LOGICAL,                  INTENT(IN)    :: OSUBG_COND ! Switch for Subgrid 
                                                    ! Condensation
REAL,                     INTENT(IN)    :: PTSTEP   ! Double Time step
                                                   ! (single if cold start)
!
REAL, DIMENSION(:,:,:),   INTENT(IN)   ::  PRHODJ  ! Dry density * Jacobian
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PSIGS   ! Sigma_s at time t
REAL, DIMENSION(:,:,:),   INTENT(IN)   ::  PPABST  ! Absolute Pressure at t     
!
REAL, DIMENSION(:,:,:),   INTENT(INOUT)       :: PTHS  ! Theta source
REAL, DIMENSION(:,:,:),   INTENT(INOUT)       :: PRVS  ! Water vapor m.r. source
REAL, DIMENSION(:,:,:),   INTENT(INOUT)       :: PRCS  ! Cloud water m.r. source
REAL, DIMENSION(:,:,:),   INTENT(INOUT)    :: PCNUCS  ! Nucl. aero. conc. source
REAL, DIMENSION(:,:,:),   INTENT(INOUT)    :: PCCS    ! Cloud water conc. source
!
REAL, DIMENSION(:,:,:),   INTENT(OUT)   :: PSRCS   ! Second-order flux
                                                   ! s'rc'/2Sigma_s2 at time t+1
                                                   ! multiplied by Lambda_3
REAL, DIMENSION(:,:,:),   INTENT(OUT)   :: PCLDFR  ! Cloud fraction          
REAL, DIMENSION(:,:,:), OPTIONAL,  INTENT(IN) :: PRRS  ! Rain  water m.r. source
!
!
!
!*       0.2   Declarations of local variables :
!
!
REAL  :: ZEPS  ! Mv/Md
REAL, DIMENSION(SIZE(PRHODJ,1),SIZE(PRHODJ,2),SIZE(PRHODJ,3)) &
                         :: ZEXNS,&      ! guess of the Exner function at t+1
                            ZT,   &      ! guess of the temperature at t+1
                            ZCPH, &      ! guess of the CPh for the mixing
                            ZLV,  &      ! guess of the Lv at t+1
                            ZW1,ZW2,ZW3  ! Work arrays for intermediate
                                         ! fields
!
INTEGER             :: IRESP      ! Return code of FM routines
INTEGER             :: JITER,ITERMAX  ! iterative loop for first order adjustment
INTEGER             :: ILUOUT     ! Logical unit of output listing 
TYPE(TFIELDDATA)    :: TZFIELD
!-------------------------------------------------------------------------------
!
!*       1.     PRELIMINARIES
!               -------------
!
ILUOUT = TLUOUT%NLU
ZEPS= XMV / XMD
!
IF (OSUBG_COND) THEN
  ITERMAX=2
ELSE
  ITERMAX=1
END IF
!
!
!-------------------------------------------------------------------------------
!
!*       2.     COMPUTE QUANTITIES WITH THE GUESS OF THE FUTURE INSTANT
!               -------------------------------------------------------
!
!*       2.1    remove negative non-precipitating negative water
!               ------------------------------------------------
!
IF (ANY(PRCS(:,:,:) < 0. .OR. PCCS(:,:,:) < 0.)) THEN
  WRITE(ILUOUT,*) 'C2R2_ADJUST beginning:  negative values of PRCS or PCCS'
  WRITE(ILUOUT,*) '  location of minimum of PRCS:', MINLOC(PRCS(:,:,:))
  WRITE(ILUOUT,*) ' value of minimum   :', MINVAL(PRCS(:,:,:))
  WRITE(ILUOUT,*) '  location of minimum of PCCS:', MINLOC(PCCS(:,:,:))
  WRITE(ILUOUT,*) ' value of minimum   :', MINVAL(PCCS(:,:,:))
END IF
!
IF (ANY(PRCS(:,:,:)+PRVS(:,:,:) < 0.) .AND. NVERB>5) THEN
  WRITE(ILUOUT,*) 'C2R2_ADJUST:  negative values of total water (reset to zero)'
  WRITE(ILUOUT,*) '  location of minimum:', MINLOC(PRCS(:,:,:)+PRVS(:,:,:))
  WRITE(ILUOUT,*) '  value of minimum   :', MINVAL(PRCS(:,:,:)+PRVS(:,:,:))
!callabortstop
  CALL PRINT_MSG(NVERB_FATAL,'GEN','C2R2_ADJUST','')
END IF
!
WHERE ( PRCS(:,:,:)+PRVS(:,:,:) < 0.)
  PRVS(:,:,:) = -  PRCS(:,:,:)
END WHERE
!
!*       2.2    estimate the Exner function at t+1
!
ZEXNS(:,:,:) = (  PPABST(:,:,:)  / XP00 ) ** (XRD/XCPD)  
!
!    beginning of the iterative loop
!
DO JITER =1,ITERMAX
!
!*       2.3    compute the intermediate temperature at t+1, T*
!  
  ZT(:,:,:) = ( PTHS(:,:,:) * PTSTEP ) * ZEXNS(:,:,:)
!
!*       2.4    compute the latent heat of vaporization Lv(T*) at t+1
!
  ZLV(:,:,:) = XLVTT + ( XCPV - XCL ) * ( ZT(:,:,:) -XTT )
!
!*       2.5    compute the specific heat for moist air (Cph) at t+1
!
  IF     ( KRR == 3 ) THEN 
    ZCPH(:,:,:) =  XCPD + XCPV *PTSTEP*   PRVS(:,:,:)       &
                        + XCL  *PTSTEP* ( PRCS(:,:,:) + PRRS(:,:,:) )
  ELSE IF( KRR == 2 ) THEN
    ZCPH(:,:,:) =  XCPD + XCPV *PTSTEP*   PRVS(:,:,:)       &
                        + XCL  *PTSTEP*   PRCS(:,:,:)  
  END IF
!
!*       2.6    compute the saturation vapor pressure at t+1
!
  ZW1(:,:,:) = EXP( XALPW - XBETAW/ZT(:,:,:) - XGAMW*ALOG( ZT(:,:,:) ) )
!
!*       2.7    compute the saturation mixing ratio at t+1
!
  ZW2(:,:,:) =  ZW1(:,:,:) * ZEPS /              &
                ( PPABST(:,:,:) - ZW1(:,:,:) )   
!
!*       2.8    compute the saturation mixing ratio derivative (rvs')
!
  ZW1(:,:,:) = ( XBETAW / ZT(:,:,:)  - XGAMW ) / ZT(:,:,:)   & 
               * ZW2(:,:,:) * ( 1. + ZW2(:,:,:) / ZEPS )
!
!*       2.9    compute  Cph + Lv * rvs'
!
  ZW3(:,:,:) = ZCPH(:,:,:) + ZLV(:,:,:) * ZW1(:,:,:)
!
!
!-------------------------------------------------------------------------------
!
!*       3.     FIRST ORDER SUBGRID CONDENSATION SCHEME
!               ---------------------------------------
!
  IF ( OSUBG_COND ) THEN
!
!*       3.1    compute  Q1              
!
    ZW2(:,:,:) = ( ( PRVS(:,:,:)*PTSTEP - ZW2(:,:,:) ) * ZCPH(:,:,:) / ZW3(:,:,:) &
                  + PRCS(:,:,:)*PTSTEP                                            &
                 ) / ( 2. * PSIGS(:,:,:) )

!
!*       3.2    compute s'rc'/2Sigma_s2, Rc and the nebolisity
!
    CALL CONDENS(HTURBDIM, ZW2,ZW1,ZW3,PSRCS) ! ZW1 = Cloud fraction 
                                              ! PSRC = s'rc'/(2 Sigma_s**2) 
                                              ! ZW3 = Rc / (2 Sigma_s)
    ZW3(:,:,:) = 2. * PSIGS(:,:,:) * ZW3(:,:,:)    ! Rc 
!
!    multiply PSRCS by the lambda3 coefficient
!
    IF ( HTURBDIM == '1DIM'.AND.JITER==ITERMAX ) THEN
      PSRCS(:,:,:) = PSRCS(:,:,:) * MIN( 3. , MAX(1.,1.-ZW2(:,:,:)) )
    END IF       ! in the 3D case lamda_3 = 1.
!
!*       3.3    compute the variation of mixing ratio
!
                                                      !         Rc - Rc*
    ZW3(:,:,:) = (ZW3(:,:,:)/PTSTEP) - PRCS(:,:,:)       ! Pcon = ----------
                                                      !         2 Delta t
  ELSE
!
!
!*       4.     SECOND ORDER ALL OR NOTHING CONDENSATION SCHEME
!               -----------------------------------------------
!
!*       4.1    compute Delta 2
!
    ZW1(:,:,:) = (ZW1(:,:,:) * ZLV(:,:,:) / ZW3(:,:,:) ) *                     &
          ( ((-2.*XBETAW+XGAMW*ZT(:,:,:)) / (XBETAW-XGAMW*ZT(:,:,:))           &
               + (XBETAW/ZT(:,:,:)-XGAMW)*(1.0+2.0*ZW2(:,:,:)/ZEPS))/ZT(:,:,:) )
!
!*       4.2    compute Delta 1
!
    ZW2(:,:,:) = ZLV(:,:,:) / ZW3(:,:,:) * ( ZW2(:,:,:) - PRVS(:,:,:)*PTSTEP )
!
!*       4.3    compute the variation of mixing ratio
!
    ZW3(:,:,:) = - ZW2(:,:,:) * ( 1 + 0.5 * ZW2(:,:,:) * ZW1(:,:,:) )  &
                 * ZCPH(:,:,:) / ZLV(:,:,:) / PTSTEP
!
!  end of the IF structure on the all or nothing or statistical condensation
!  scheme
!
  END IF
!
!
!*       5.     COMPUTE THE SOURCES AND STORES THE CLOUD FRACTION
!               -------------------------------------------------
!
!
!*       5.1    compute the sources 
! 
  ZW3(:,:,:) = MAX ( ZW3(:,:,:), -PRCS(:,:,:) )  
  WHERE (ZW3(:,:,:) > 0.0)
    ZW3(:,:,:) = MIN ( ZW3(:,:,:),  PRVS(:,:,:) )
  END WHERE
 WHERE (PCCS(:,:,:) > 0.0)
  PRCS(:,:,:) = PRCS(:,:,:) + ZW3(:,:,:)
  PRVS(:,:,:) = PRVS(:,:,:) - ZW3(:,:,:) 
  PTHS(:,:,:) = PTHS(:,:,:) + ZW3(:,:,:) * ZLV(:,:,:) / ZCPH(:,:,:)     &
                                         / ZEXNS(:,:,:)
 END WHERE
!
!  WHERE (PRCS(:,:,:) <= 0.0) ! full evaporation of the cloud droplets
!     PCNUCS(:,:,:) = 0.0
!     PCCS(:,:,:) = 0.0
! END WHERE
  WHERE (PRCS(:,:,:) <= 1.E-7/ PTSTEP ) ! full evaporation of the cloud droplets (PRCS >=0)
    PRVS(:,:,:) = PRVS(:,:,:) + PRCS(:,:,:)
    PTHS(:,:,:) = PTHS(:,:,:) - PRCS(:,:,:) * ZLV(:,:,:) / ZCPH(:,:,:)     &
                                       / ZEXNS(:,:,:)
    PRCS(:,:,:) = 0.0
    PCNUCS(:,:,:) = 0.0
    PCCS(:,:,:) = 0.0
  END WHERE
!  end of the iterative loop
!
END DO
!
!*       5.2    compute the cloud fraction PCLDFR
!
IF ( .NOT. OSUBG_COND ) THEN
  WHERE (PRCS(:,:,:) > 1.E-12 / PTSTEP)
    ZW1(:,:,:)  = 1.
  ELSEWHERE
    ZW1(:,:,:)  = 0. 
  ENDWHERE 
  IF ( SIZE(PSRCS,3) /= 0 ) THEN
    PSRCS(:,:,:) = ZW1(:,:,:) 
  END IF
END IF 
!
IF ( HRAD /= 'NONE' ) THEN
  PCLDFR(:,:,:) = ZW1(:,:,:)
END IF
!
IF ( OCLOSE_OUT ) THEN
  TZFIELD%CMNHNAME   = 'NEB'
  TZFIELD%CSTDNAME   = ''
  TZFIELD%CLONGNAME  = 'NEB'
  TZFIELD%CUNITS     = '1'
  TZFIELD%CDIR       = 'XY'
  TZFIELD%CCOMMENT   = 'X_Y_Z_NEB'
  TZFIELD%NGRID      = 1
  TZFIELD%NTYPE      = TYPEREAL
  TZFIELD%NDIMS      = 3
  TZFIELD%LTIMEDEP   = .TRUE.
  CALL IO_WRITE_FIELD(TPFILE,TZFIELD,ZW1)
END IF
!
!
!        6.  Horizontal mean Cloud fraction (for LES uses)
!            ---------------------------------------------
!
!
!*       7.  STORE THE BUDGET TERMS
!            ----------------------
!
!
IF (LBUDGET_RV) CALL BUDGET (PRVS(:,:,:) * PRHODJ(:,:,:),6,'COND_BU_RRV')
IF (LBUDGET_RC) CALL BUDGET (PRCS(:,:,:) * PRHODJ(:,:,:),7,'COND_BU_RRC')
IF (LBUDGET_TH) CALL BUDGET (PTHS(:,:,:) * PRHODJ(:,:,:),4,'COND_BU_RTH')
IF (LBUDGET_SV) THEN
  CALL BUDGET (PCNUCS(:,:,:) * PRHODJ(:,:,:),13+(NSV_C2R2BEG-1),'CEVA_BU_RSV') ! RCN
  CALL BUDGET (PCCS(:,:,:) * PRHODJ(:,:,:),14+(NSV_C2R2BEG-1),'CEVA_BU_RSV') ! RCC
END IF
!
!------------------------------------------------------------------------------
!
!
END SUBROUTINE C2R2_ADJUST
