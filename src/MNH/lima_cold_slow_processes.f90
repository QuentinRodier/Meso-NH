!MNH_LIC Copyright 2013-2018 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!      #####################
       MODULE MODI_LIMA_COLD_SLOW_PROCESSES
!      #####################
!
INTERFACE
      SUBROUTINE LIMA_COLD_SLOW_PROCESSES (PTSTEP, KMI, PZZ, PRHODJ,                 &
                                           PRHODREF, PEXNREF, PPABST,                &
                                           PTHT, PRVT, PRCT, PRRT, PRIT, PRST, PRGT, &
                                           PTHS, PRVS, PRIS, PRSS,                   &
                                           PCIT, PCIS                                )
!
REAL,                     INTENT(IN)    :: PTSTEP  ! Time step          
INTEGER,                  INTENT(IN)    :: KMI     ! Model index 
!
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PZZ     ! Height (z)
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRHODJ  ! Dry density * Jacobian
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRHODREF! Reference density
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PEXNREF ! Reference Exner function
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PPABST  ! abs. pressure at time t
!
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PTHT    ! Theta at time t
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRVT    ! Water vapor m.r. at t 
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRCT    ! Cloud water m.r. at t 
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRRT    ! Rain water m.r. at t 
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRIT    ! Cloud ice m.r. at t 
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRST    ! Snow/aggregate m.r. at t 
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRGT    ! Graupel m.r. at t 
!
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PTHS    ! Theta source
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PRVS    ! Water vapor m.r. source
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PRIS    ! Pristine ice m.r. source
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PRSS    ! Snow/aggregate m.r. source
!
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PCIT    ! Ice crystal C. at t
!
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PCIS    ! Ice crystal C. source
!
END SUBROUTINE LIMA_COLD_SLOW_PROCESSES
END INTERFACE
END MODULE MODI_LIMA_COLD_SLOW_PROCESSES
!
!     ################################################################################
      SUBROUTINE LIMA_COLD_SLOW_PROCESSES (PTSTEP, KMI, PZZ, PRHODJ,                 &
                                           PRHODREF, PEXNREF, PPABST,                &
                                           PTHT, PRVT, PRCT, PRRT, PRIT, PRST, PRGT, &
                                           PTHS, PRVS, PRIS, PRSS,                   &
                                           PCIT, PCIS                                )
!     ################################################################################
!
!!    PURPOSE
!!    -------
!!      The purpose of this routine is to compute the microphysical sources
!!    for slow cold processes :
!!      - conversion of snow to ice
!!      - deposition of vapor on snow
!!      - conversion of ice to snow (Harrington 1995)
!!      - aggregation of ice on snow
!!
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!      J.-M. Cohard     * Laboratoire d'Aerologie*
!!      J.-P. Pinty      * Laboratoire d'Aerologie*
!!      S.    Berthet    * Laboratoire d'Aerologie*
!!      B.    Vié        * Laboratoire d'Aerologie*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original             ??/??/13 
!!      C. Barthe  * LACy *  jan. 2014   add budgets
!!  Philippe Wautelet: 05/2016-04/2018: new data structures and calls for I/O
!!      B.Vie 03/2020 Correction of budgets parallelization
!!
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_PARAMETERS,      ONLY : JPHEXT, JPVEXT
USE MODD_CST,             ONLY : XP00, XRD, XRV, XMV, XMD, XCPD, XCPV,        &
                                 XCL, XCI, XTT, XLSTT, XALPI, XBETAI, XGAMI
USE MODD_PARAM_LIMA,      ONLY : LSNOW, XRTMIN, XCTMIN, XALPHAI, XALPHAS,     &
                                 XNUI 
USE MODD_PARAM_LIMA_COLD, ONLY : XLBI, XLBEXI, XLBS, XLBEXS, XBI, XCXS, XCCS, &
                                 XLBDAS_MAX, XDSCNVI_LIM, XLBDASCNVI_MAX,     &
                                 XC0DEPSI, XC1DEPSI, XR0DEPSI, XR1DEPSI,      &
                                 XSCFAC, X1DEPS, X0DEPS, XEX1DEPS, XEX0DEPS,  &
                                 XDICNVS_LIM, XLBDAICNVS_LIM,                 &
                                 XC0DEPIS, XC1DEPIS, XR0DEPIS, XR1DEPIS,      &
                                 XCOLEXIS, XAGGS_CLARGE1, XAGGS_CLARGE2,      &
                                 XAGGS_RLARGE1, XAGGS_RLARGE2  
USE MODI_LIMA_FUNCTIONS,  ONLY : COUNTJV
USE MODD_BUDGET
USE MODD_NSV, ONLY : NSV_LIMA_NI
USE MODI_BUDGET

!
IMPLICIT NONE
!
!*       0.1   Declarations of dummy arguments :
!
REAL,                     INTENT(IN)    :: PTSTEP  ! Time step          
INTEGER,                  INTENT(IN)    :: KMI     ! Model index 
!
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PZZ     ! Height (z)
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRHODJ  ! Dry density * Jacobian
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRHODREF! Reference density
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PEXNREF ! Reference Exner function
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PPABST  ! abs. pressure at time t
!
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PTHT    ! Theta at time t
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRVT    ! Water vapor m.r. at t 
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRCT    ! Cloud water m.r. at t 
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRRT    ! Rain water m.r. at t 
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRIT    ! Cloud ice m.r. at t 
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRST    ! Snow/aggregate m.r. at t 
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRGT    ! Graupel m.r. at t 
!
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PTHS    ! Theta source
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PRVS    ! Water vapor m.r. source
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PRIS    ! Pristine ice m.r. source
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PRSS    ! Snow/aggregate m.r. source
!
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PCIT    ! Ice crystal C. at t
!
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PCIS    ! Ice crystal C. source
!
!*       0.2   Declarations of local variables :
!
LOGICAL, DIMENSION(SIZE(PRHODREF,1),SIZE(PRHODREF,2),SIZE(PRHODREF,3)) &
			  :: GMICRO ! Computations only where necessary
INTEGER :: IMICRO
INTEGER , DIMENSION(SIZE(GMICRO)) :: I1,I2,I3 ! Used to replace PACK
INTEGER                           :: JL       ! and PACK intrinsics
!
REAL, DIMENSION(:), ALLOCATABLE :: ZRVT    ! Water vapor m.r. at t
REAL, DIMENSION(:), ALLOCATABLE :: ZRCT    ! Cloud water m.r. at t
REAL, DIMENSION(:), ALLOCATABLE :: ZRRT    ! Rain water m.r. at t
REAL, DIMENSION(:), ALLOCATABLE :: ZRIT    ! Pristine ice m.r. at t
REAL, DIMENSION(:), ALLOCATABLE :: ZRST    ! Snow/aggregate m.r. at t
REAL, DIMENSION(:), ALLOCATABLE :: ZRGT    ! Graupel/hail m.r. at t
!
REAL, DIMENSION(:), ALLOCATABLE :: ZCIT    ! Pristine ice conc. at t
!
REAL, DIMENSION(:), ALLOCATABLE :: ZRVS    ! Water vapor m.r. source
REAL, DIMENSION(:), ALLOCATABLE :: ZRIS    ! Pristine ice m.r. source
REAL, DIMENSION(:), ALLOCATABLE :: ZRSS    ! Snow/aggregate m.r. source
!
REAL, DIMENSION(:),   ALLOCATABLE :: ZTHS    ! Theta source
!
REAL, DIMENSION(:),   ALLOCATABLE :: ZCIS    ! Pristine ice conc. source
!
REAL, DIMENSION(:), ALLOCATABLE &
                   :: ZRHODREF, & ! RHO Dry REFerence
                      ZRHODJ,   & ! RHO times Jacobian
                      ZZT,      & ! Temperature
                      ZPRES,    & ! Pressure
                      ZEXNREF,  & ! EXNer Pressure REFerence
                      ZZW,      & ! Work array
                      ZZX,      & ! Work array
                      ZLSFACT,  & ! L_s/(Pi_ref*C_ph)
                      ZSSI,     & ! Supersaturation over ice
                      ZLBDAI,   & ! Slope parameter of the ice crystal distr.
                      ZLBDAS,   & ! Slope parameter of the aggregate distr.
                      ZAI,      & ! Thermodynamical function
                      ZCJ,      & ! used to compute the ventilation coefficient
                      ZKA,      & ! Thermal conductivity of the air
                      ZDV,      & ! Diffusivity of water vapor in the air
                      ZVISCA      ! Viscosity of air
!
REAL, DIMENSION(:,:), ALLOCATABLE :: ZZW1      ! Work arrays
!
REAL, DIMENSION(SIZE(PRHODREF,1),SIZE(PRHODREF,2),SIZE(PRHODREF,3))   &
                                  :: ZT, ZW ! Temperature
!
INTEGER :: IIB, IIE, IJB, IJE, IKB, IKE        ! Physical domain
!
REAL,    DIMENSION(:),   ALLOCATABLE :: ZRTMIN, ZCTMIN
!
!-------------------------------------------------------------------------------
!
! Physical domain
!
IIB=1+JPHEXT
IIE=SIZE(PZZ,1) - JPHEXT
IJB=1+JPHEXT
IJE=SIZE(PZZ,2) - JPHEXT
IKB=1+JPVEXT
IKE=SIZE(PZZ,3) - JPVEXT
!
! Physical limitations
!
ALLOCATE(ZRTMIN(SIZE(XRTMIN)))
ALLOCATE(ZCTMIN(SIZE(XCTMIN)))
ZRTMIN(:) = XRTMIN(:) / PTSTEP
ZCTMIN(:) = XCTMIN(:) / PTSTEP
!
! Temperature
ZT(:,:,:)  = PTHT(:,:,:) * ( PPABST(:,:,:)/XP00 ) ** (XRD/XCPD)
!
! Looking for regions where computations are necessary
!
GMICRO(:,:,:) = .FALSE.
GMICRO(IIB:IIE,IJB:IJE,IKB:IKE) =                            &
     PRIT(IIB:IIE,IJB:IJE,IKB:IKE)>XRTMIN(4) .OR. &
     PRST(IIB:IIE,IJB:IJE,IKB:IKE)>XRTMIN(5)
!
IMICRO = COUNTJV( GMICRO(:,:,:),I1(:),I2(:),I3(:))
!
IF( IMICRO >= 1 ) THEN
!
!------------------------------------------------------------------------------
!
!
!*       1.    Optimization : packing variables
!              --------------------------------
!
!
!
   ALLOCATE(ZRVT(IMICRO)) 
   ALLOCATE(ZRCT(IMICRO)) 
   ALLOCATE(ZRRT(IMICRO)) 
   ALLOCATE(ZRIT(IMICRO)) 
   ALLOCATE(ZRST(IMICRO)) 
   ALLOCATE(ZRGT(IMICRO)) 
!
   ALLOCATE(ZCIT(IMICRO)) 
!
   ALLOCATE(ZRVS(IMICRO))  
   ALLOCATE(ZRIS(IMICRO))
   ALLOCATE(ZRSS(IMICRO))
!
   ALLOCATE(ZTHS(IMICRO))
!
   ALLOCATE(ZCIS(IMICRO))
! 
   ALLOCATE(ZRHODREF(IMICRO)) 
   ALLOCATE(ZZT(IMICRO)) 
   ALLOCATE(ZPRES(IMICRO)) 
   ALLOCATE(ZEXNREF(IMICRO))
   DO JL=1,IMICRO   
      ZRVT(JL) = PRVT(I1(JL),I2(JL),I3(JL))
      ZRCT(JL) = PRCT(I1(JL),I2(JL),I3(JL))
      ZRRT(JL) = PRRT(I1(JL),I2(JL),I3(JL))
      ZRIT(JL) = PRIT(I1(JL),I2(JL),I3(JL))
      ZRST(JL) = PRST(I1(JL),I2(JL),I3(JL))
      ZRGT(JL) = PRGT(I1(JL),I2(JL),I3(JL))
!
      ZCIT(JL) = PCIT(I1(JL),I2(JL),I3(JL))
!
      ZRVS(JL) = PRVS(I1(JL),I2(JL),I3(JL))
      ZRIS(JL) = PRIS(I1(JL),I2(JL),I3(JL))
      ZRSS(JL) = PRSS(I1(JL),I2(JL),I3(JL))
!
      ZTHS(JL) = PTHS(I1(JL),I2(JL),I3(JL))
!
      ZCIS(JL) = PCIS(I1(JL),I2(JL),I3(JL))
!
      ZRHODREF(JL) = PRHODREF(I1(JL),I2(JL),I3(JL))
      ZZT(JL)      = ZT(I1(JL),I2(JL),I3(JL))
      ZPRES(JL)    = PPABST(I1(JL),I2(JL),I3(JL))
      ZEXNREF(JL)  = PEXNREF(I1(JL),I2(JL),I3(JL))
   ENDDO
!
   IF (NBUMOD==KMI .AND. LBU_ENABLE) THEN
      ALLOCATE(ZRHODJ(IMICRO))
      ZRHODJ(:) = PACK( PRHODJ(:,:,:),MASK=GMICRO(:,:,:) )
   END IF
!
!
!------------------------------------------------------------------------------
!
!
!*       2.    Microphysical computations
!              --------------------------
! 
!
   ALLOCATE(ZZW(IMICRO))
   ALLOCATE(ZZX(IMICRO))
   ALLOCATE(ZLSFACT(IMICRO))
   ALLOCATE(ZSSI(IMICRO))
   ALLOCATE(ZLBDAI(IMICRO)) 
   ALLOCATE(ZLBDAS(IMICRO))
   ALLOCATE(ZAI(IMICRO))
   ALLOCATE(ZCJ(IMICRO))
   ALLOCATE(ZKA(IMICRO))
   ALLOCATE(ZDV(IMICRO))
   ALLOCATE(ZZW1(IMICRO,7))
!
! Preliminary computations
!
   ZZW(:)  = ZEXNREF(:)*( XCPD+XCPV*ZRVT(:)+XCL*(ZRCT(:)+ZRRT(:)) &
                                   +XCI*(ZRIT(:)+ZRST(:)+ZRGT(:)) )
!
   ZLSFACT(:) = (XLSTT+(XCPV-XCI)*(ZZT(:)-XTT))/ZZW(:) ! L_s/(Pi_ref*C_ph)
!
   ZZW(:) = EXP( XALPI - XBETAI/ZZT(:) - XGAMI*ALOG(ZZT(:) ) )
   ZSSI(:) = ZRVT(:)*( ZPRES(:)-ZZW(:) ) / ( (XMV/XMD) * ZZW(:) ) - 1.0
                                                       ! Supersaturation over ice
! Distribution parameters for ice and snow
   ZLBDAI(:)  = 1.E10
   WHERE (ZRIT(:)>XRTMIN(4) .AND. ZCIT(:)>XCTMIN(4))
      ZLBDAI(:) = ( XLBI*ZCIT(:) / ZRIT(:) )**XLBEXI
   END WHERE
   ZLBDAS(:)  = 1.E10
   WHERE (ZRST(:)>XRTMIN(5) )
      ZLBDAS(:) = XLBS*( ZRHODREF(:)*ZRST(:) )**XLBEXS
   END WHERE
!
   ZKA(:) = 2.38E-2 + 0.0071E-2 * ( ZZT(:) - XTT )          ! k_a
   ZDV(:) = 0.211E-4 * (ZZT(:)/XTT)**1.94 * (XP00/ZPRES(:)) ! D_v
!
! Thermodynamical function ZAI = A_i(T,P)
   ZAI(:) = ( XLSTT + (XCPV-XCI)*(ZZT(:)-XTT) )**2 / (ZKA(:)*XRV*ZZT(:)**2) &
                                         + ( XRV*ZZT(:) ) / (ZDV(:)*ZZW(:))
! ZCJ = c^prime_j/c_i (in the ventilation factor) ( c_i from v(D)=c_i*D^(d_i) )
   ZCJ(:) = XSCFAC * ZRHODREF(:)**0.3 / SQRT( 1.718E-5+0.0049E-5*(ZZT(:)-XTT) )
!
!
!
!
!*       2.1    Conversion of snow to r_i: RSCNVI
!        ----------------------------------------
!
!
      WHERE ( ZRST(:)>XRTMIN(5) )
         ZLBDAS(:)  = MIN( XLBDAS_MAX,                                           &
                           XLBS*( ZRHODREF(:)*MAX( ZRST(:),XRTMIN(5) ) )**XLBEXS )
      END WHERE
      ZZW(:) = 0.0
      WHERE ( ZLBDAS(:)<XLBDASCNVI_MAX .AND. (ZRST(:)>XRTMIN(5)) &
                                       .AND. (ZSSI(:)<0.0)       )
         ZZW(:) = (ZLBDAS(:)*XDSCNVI_LIM)**(XALPHAS)
         ZZX(:) = ( -ZSSI(:)/ZAI(:) ) * (XCCS*ZLBDAS(:)**XCXS)/ZRHODREF(:) * (ZZW(:)**XNUI) &
                                                               * EXP(-ZZW(:))
!
         ZZW(:) = MIN( ( XR0DEPSI+XR1DEPSI*ZCJ(:) )*ZZX(:),ZRSS(:) )
         ZRIS(:) = ZRIS(:) + ZZW(:)
         ZRSS(:) = ZRSS(:) - ZZW(:)
!
         ZZW(:) = ZZW(:)*( XC0DEPSI+XC1DEPSI*ZCJ(:) )/( XR0DEPSI+XR1DEPSI*ZCJ(:) )
         ZCIS(:) = ZCIS(:) + ZZW(:)
      END WHERE
      ZW(:,:,:) = PRIS(:,:,:)
      PRIS(:,:,:) = UNPACK( ZRIS(:),MASK=GMICRO(:,:,:),FIELD=ZW(:,:,:) )
      ZW(:,:,:) = PRSS(:,:,:)
      PRSS(:,:,:) = UNPACK( ZRSS(:),MASK=GMICRO(:,:,:),FIELD=ZW(:,:,:) )
      ZW(:,:,:) = PCIS(:,:,:)
      PCIS(:,:,:) = UNPACK( ZCIS(:),MASK=GMICRO(:,:,:),FIELD=ZW(:,:,:) )
   END IF ! IMICRO
!
! Budget storage
   IF (NBUMOD==KMI .AND. LBU_ENABLE) THEN
      IF (LBUDGET_RI) CALL BUDGET (PRIS(:,:,:)*PRHODJ(:,:,:),9,'CNVI_BU_RRI')
      IF (LBUDGET_RS) CALL BUDGET (PRSS(:,:,:)*PRHODJ(:,:,:),10,'CNVI_BU_RRS')
      IF (LBUDGET_SV) CALL BUDGET (PCIS(:,:,:)*PRHODJ(:,:,:),12+NSV_LIMA_NI,'CNVI_BU_RSV')
   END IF
!
!
!*       2.2    Deposition of water vapor on r_s: RVDEPS
!        -----------------------------------------------
!
!
   IF( IMICRO >= 1 ) THEN
      ZZW(:) = 0.0
      WHERE ( (ZRST(:)>XRTMIN(5)) .AND. (ZRSS(:)>ZRTMIN(5)) )
         ZZW(:) = ( ZSSI(:)/(ZRHODREF(:)*ZAI(:)) ) *                               &
                  ( X0DEPS*ZLBDAS(:)**XEX0DEPS + X1DEPS*ZCJ(:)*ZLBDAS(:)**XEX1DEPS )
         ZZW(:) =    MIN( ZRVS(:),ZZW(:)      )*(0.5+SIGN(0.5,ZZW(:))) &
                   - MIN( ZRSS(:),ABS(ZZW(:)) )*(0.5-SIGN(0.5,ZZW(:)))
         ZRSS(:) = ZRSS(:) + ZZW(:)
         ZRVS(:) = ZRVS(:) - ZZW(:)
         ZTHS(:) = ZTHS(:) + ZZW(:)*ZLSFACT(:)
      END WHERE
      !
      ZW(:,:,:) = PRVS(:,:,:)
      PRVS(:,:,:) = UNPACK( ZRVS(:),MASK=GMICRO(:,:,:),FIELD=ZW(:,:,:) )
      ZW(:,:,:) = PRSS(:,:,:)
      PRSS(:,:,:) = UNPACK( ZRSS(:),MASK=GMICRO(:,:,:),FIELD=ZW(:,:,:) )
      ZW(:,:,:) = PTHS(:,:,:)
      PTHS(:,:,:) = UNPACK( ZTHS(:),MASK=GMICRO(:,:,:),FIELD=ZW(:,:,:) )
   END IF
! Budget storage
   IF (NBUMOD==KMI .AND. LBU_ENABLE) THEN
      IF (LBUDGET_TH) CALL BUDGET (PTHS(:,:,:)*PRHODJ(:,:,:),4,'DEPS_BU_RTH')
      IF (LBUDGET_RV) CALL BUDGET (PRVS(:,:,:)*PRHODJ(:,:,:),6,'DEPS_BU_RRV')
      IF (LBUDGET_RS) CALL BUDGET (PRSS(:,:,:)*PRHODJ(:,:,:),10,'DEPS_BU_RRS')
   END IF
!
!
!*       2.3    Conversion of pristine ice to r_s: RICNVS
!        ------------------------------------------------
!
!
   IF( IMICRO >= 1 ) THEN
      ZZW(:) = 0.0
      WHERE ( (ZLBDAI(:)<XLBDAICNVS_LIM) .AND. (ZCIT(:)>XCTMIN(4)) &
                                         .AND. (ZSSI(:)>0.0)       )
         ZZW(:) = (ZLBDAI(:)*XDICNVS_LIM)**(XALPHAI)
         ZZX(:) = ( ZSSI(:)/ZAI(:) )*ZCIT(:) * (ZZW(:)**XNUI) *EXP(-ZZW(:))
!
! Correction BVIE
!         ZZW(:) = MAX( MIN( ( XR0DEPIS + XR1DEPIS*ZCJ(:) )*ZZX(:)/ZRHODREF(:) &
         ZZW(:) = MAX( MIN( ( XR0DEPIS + XR1DEPIS*ZCJ(:) )*ZZX(:) &
                            ,ZRIS(:) ) + ZRTMIN(5), ZRTMIN(5) ) - ZRTMIN(5)
         ZRIS(:) = ZRIS(:) - ZZW(:)
         ZRSS(:) = ZRSS(:) + ZZW(:)
!
         ZZW(:) = MIN( ZZW(:)*(( XC0DEPIS+XC1DEPIS*ZCJ(:) )                   &
                             /( XR0DEPIS+XR1DEPIS*ZCJ(:) )),ZCIS(:) )
         ZCIS(:) = ZCIS(:) - ZZW(:)
      END WHERE
!
      ZW(:,:,:) = PRIS(:,:,:)
      PRIS(:,:,:) = UNPACK( ZRIS(:),MASK=GMICRO(:,:,:),FIELD=ZW(:,:,:) )
      ZW(:,:,:) = PRSS(:,:,:)
      PRSS(:,:,:) = UNPACK( ZRSS(:),MASK=GMICRO(:,:,:),FIELD=ZW(:,:,:) )
      ZW(:,:,:) = PCIS(:,:,:)
      PCIS(:,:,:) = UNPACK( ZCIS(:),MASK=GMICRO(:,:,:),FIELD=ZW(:,:,:) )
   END IF
! Budget storage
   IF (NBUMOD==KMI .AND. LBU_ENABLE) THEN
      IF (LBUDGET_RI) CALL BUDGET (PRIS(:,:,:)*PRHODJ(:,:,:),9,'CNVS_BU_RRI')
      IF (LBUDGET_RS) CALL BUDGET (PRSS(:,:,:)*PRHODJ(:,:,:),10,'CNVS_BU_RRS')
      IF (LBUDGET_SV) CALL BUDGET (PCIS(:,:,:)*PRHODJ(:,:,:),12+NSV_LIMA_NI,'CNVS_BU_RSV')
   END IF
!
!
!*       2.4    Aggregation of r_i on r_s: CIAGGS and RIAGGS
!        ---------------------------------------------------
!
!
   IF( IMICRO >= 1 ) THEN
      WHERE ( (ZRIT(:)>XRTMIN(4)) .AND. (ZRST(:)>XRTMIN(5)) .AND. (ZRIS(:)>ZRTMIN(4)) &
                                                            .AND. (ZCIS(:)>ZCTMIN(4)) )
         ZZW1(:,3) = (ZLBDAI(:) / ZLBDAS(:))**3
         ZZW1(:,1) = (ZCIT(:)*(XCCS*ZLBDAS(:)**XCXS)/ZRHODREF(:)*EXP( XCOLEXIS*(ZZT(:)-XTT) )) &
                                                    / (ZLBDAI(:)**3)
         ZZW1(:,2) = MIN( ZZW1(:,1)*(XAGGS_CLARGE1+XAGGS_CLARGE2*ZZW1(:,3)),ZCIS(:) )
         ZCIS(:) = ZCIS(:) - ZZW1(:,2)
!
         ZZW1(:,1) = ZZW1(:,1) / ZLBDAI(:)**XBI
         ZZW1(:,2) = MIN( ZZW1(:,1)*(XAGGS_RLARGE1+XAGGS_RLARGE2*ZZW1(:,3)),ZRIS(:) )
         ZRIS(:) = ZRIS(:) - ZZW1(:,2)
         ZRSS(:) = ZRSS(:) + ZZW1(:,2)
      END WHERE
      !
      ZW(:,:,:) = PRIS(:,:,:)
      PRIS(:,:,:) = UNPACK( ZRIS(:),MASK=GMICRO(:,:,:),FIELD=ZW(:,:,:) )
      ZW(:,:,:) = PRSS(:,:,:)
      PRSS(:,:,:) = UNPACK( ZRSS(:),MASK=GMICRO(:,:,:),FIELD=ZW(:,:,:) )
      ZW(:,:,:) = PCIS(:,:,:)
      PCIS(:,:,:) = UNPACK( ZCIS(:),MASK=GMICRO(:,:,:),FIELD=ZW(:,:,:) )
   END IF
! Budget storage
   IF (NBUMOD==KMI .AND. LBU_ENABLE) THEN
      IF (LBUDGET_RI) CALL BUDGET (PRIS(:,:,:)*PRHODJ(:,:,:),9,'AGGS_BU_RRI')
      IF (LBUDGET_RS) CALL BUDGET (PRSS(:,:,:)*PRHODJ(:,:,:),10,'AGGS_BU_RRS')
      IF (LBUDGET_SV) CALL BUDGET (PCIS(:,:,:)*PRHODJ(:,:,:),12+NSV_LIMA_NI,'AGGS_BU_RSV')
   END IF
!
!
!------------------------------------------------------------------------------
!
!
!*       3.    Unpacking & Deallocating
!              ------------------------
!
   IF( IMICRO >= 1 ) THEN
      DEALLOCATE(ZRVT) 
      DEALLOCATE(ZRCT) 
      DEALLOCATE(ZRRT) 
      DEALLOCATE(ZRIT) 
      DEALLOCATE(ZRST) 
      DEALLOCATE(ZRGT) 
      DEALLOCATE(ZCIT) 
      DEALLOCATE(ZRVS)  
      DEALLOCATE(ZRIS)
      DEALLOCATE(ZRSS)
      DEALLOCATE(ZTHS)
      DEALLOCATE(ZCIS)  
      DEALLOCATE(ZRHODREF) 
      DEALLOCATE(ZZT) 
      DEALLOCATE(ZPRES) 
      DEALLOCATE(ZEXNREF)
      DEALLOCATE(ZZW)
      DEALLOCATE(ZZX)
      DEALLOCATE(ZLSFACT)
      DEALLOCATE(ZSSI)
      DEALLOCATE(ZLBDAI) 
      DEALLOCATE(ZLBDAS)
      DEALLOCATE(ZAI)
      DEALLOCATE(ZCJ)
      DEALLOCATE(ZKA)
      DEALLOCATE(ZDV)
      DEALLOCATE(ZZW1)
      IF (NBUMOD==KMI .AND. LBU_ENABLE) DEALLOCATE(ZRHODJ)
   END IF
!
   DEALLOCATE(ZRTMIN)
   DEALLOCATE(ZCTMIN)
!
 END SUBROUTINE LIMA_COLD_SLOW_PROCESSES
