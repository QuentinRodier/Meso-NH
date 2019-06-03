!MNH_LIC Copyright 1995-2019 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
! Modifications:
!  P. Wautelet 25/02/2019: split rain_ice (cleaner and easier to maintain/debug)
!  P. Wautelet 26/04/2019: replace non-standard FLOAT function by REAL function
!  P. Wautelet 03/06/2019: remove PACK/UNPACK intrinsics (to get more performance and better OpenACC support)
!-----------------------------------------------------------------
MODULE MODE_RAIN_ICE_FAST_RH

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: RAIN_ICE_FAST_RH

CONTAINS

SUBROUTINE RAIN_ICE_FAST_RH(OMICRO, PRHODREF, PRVT, PRCT, PRIT, PRST, PRGT, PRHT, PRHODJ, PPRES, &
                            PZT, PLBDAS, PLBDAG, PLBDAH, PLSFACT, PLVFACT, PCJ, PKA, PDV, PRHODJ3D, PTHS3D, &
                            PRCS, PRRS, PRIS, PRSS, PRGS, PRHS, PTHS, PUSW)
!
!*      0. DECLARATIONS
!          ------------
!
use MODD_BUDGET,         only: LBUDGET_RC, LBUDGET_RG, LBUDGET_RH, LBUDGET_RI, LBUDGET_RR, LBUDGET_RS, LBUDGET_TH
use MODD_CST,            only: XCI, XCL, XCPV, XESTT, XLMTT, XLVTT, XMD, XMV, XRV, XTT
use MODD_RAIN_ICE_DESCR, only: XBG, XBS, XCEXVT, XCXG, XCXH, XCXS, XDH, XLBEXH, XLBH, XRTMIN
use MODD_RAIN_ICE_PARAM, only: NWETLBDAG, NWETLBDAH, NWETLBDAS, X0DEPH, X1DEPH, &
                               XEX0DEPH, XEX1DEPH, XFGWETH, XFSWETH, XFWETH, XKER_GWETH, XKER_SWETH, &
                               XLBGWETH1, XLBGWETH2, XLBGWETH3, XLBSWETH1, XLBSWETH2, XLBSWETH3,     &
                               XWETINTP1G, XWETINTP1H, XWETINTP1S, XWETINTP2G, XWETINTP2H, XWETINTP2S
!
use MODI_BUDGET
!
IMPLICIT NONE
!
!*       0.1   Declarations of dummy arguments :
!
LOGICAL,  DIMENSION(:,:,:), intent(in)    :: OMICRO   ! Test where to compute all processes
REAL,     DIMENSION(:),     intent(in)    :: PRHODREF ! RHO Dry REFerence
REAL,     DIMENSION(:),     intent(in)    :: PRVT     ! Water vapor m.r. at t
REAL,     DIMENSION(:),     intent(in)    :: PRCT     ! Cloud water m.r. at t
REAL,     DIMENSION(:),     intent(in)    :: PRIT     ! Pristine ice m.r. at t
REAL,     DIMENSION(:),     intent(in)    :: PRST     ! Snow/aggregate m.r. at t
REAL,     DIMENSION(:),     intent(in)    :: PRGT     ! Graupel m.r. at t
REAL,     DIMENSION(:),     intent(in)    :: PRHT    ! Hail m.r. at t
REAL,     DIMENSION(:),     intent(in)    :: PRHODJ   ! RHO times Jacobian
REAL,     DIMENSION(:),     intent(in)    :: PPRES    ! Pressure
REAL,     DIMENSION(:),     intent(in)    :: PZT      ! Temperature
REAL,     DIMENSION(:),     intent(in)    :: PLBDAS   ! Slope parameter of the aggregate distribution
REAL,     DIMENSION(:),     intent(in)    :: PLBDAG   ! Slope parameter of the graupel   distribution
REAL,     DIMENSION(:),     intent(inout) :: PLBDAH   ! Slope parameter of the hail      distribution
REAL,     DIMENSION(:),     intent(in)    :: PLSFACT  ! L_s/(Pi_ref*C_ph)
REAL,     DIMENSION(:),     intent(in)    :: PLVFACT  ! L_v/(Pi_ref*C_ph)
REAL,     DIMENSION(:),     intent(in)    :: PCJ      ! Function to compute the ventilation coefficient
REAL,     DIMENSION(:),     intent(in)    :: PKA      ! Thermal conductivity of the air
REAL,     DIMENSION(:),     intent(in)    :: PDV      ! Diffusivity of water vapor in the air
REAL,     DIMENSION(:,:,:), INTENT(IN)    :: PRHODJ3D ! Dry density * Jacobian
REAL,     DIMENSION(:,:,:), INTENT(IN)    :: PTHS3D   ! Theta source
REAL,     DIMENSION(:),     INTENT(INOUT) :: PRCS     ! Cloud water m.r. source
REAL,     DIMENSION(:),     INTENT(INOUT) :: PRRS     ! Rain water m.r. source
REAL,     DIMENSION(:),     INTENT(INOUT) :: PRIS     ! Pristine ice m.r. source
REAL,     DIMENSION(:),     INTENT(INOUT) :: PRSS     ! Snow/aggregate m.r. source
REAL,     DIMENSION(:),     INTENT(INOUT) :: PRGS     ! Graupel m.r. source
REAL,     DIMENSION(:),     INTENT(INOUT) :: PRHS     ! Hail m.r. source
REAL,     DIMENSION(:),     INTENT(INOUT) :: PTHS     ! Theta source
REAL,     DIMENSION(:),     intent(inout) :: PUSW     ! Undersaturation over water
!
!*       0.2  declaration of local variables
!
INTEGER                              :: IHAIL, IGWET
INTEGER                              :: JJ
INTEGER, DIMENSION(size(PRHODREF))   :: I1
INTEGER, DIMENSION(:), ALLOCATABLE   :: IVEC1, IVEC2      ! Vectors of indices for interpolations
LOGICAL, DIMENSION(size(PRHODREF))   :: GWET              ! Test where to compute wet growth
LOGICAL, DIMENSION(size(PRHODREF))   :: GHAIL             ! Test where to compute hail growth
REAL,    DIMENSION(:), ALLOCATABLE   :: ZVEC1,ZVEC2,ZVEC3 ! Work vectors for interpolations
REAL,    DIMENSION(size(PRHODREF))   :: ZZW               ! Work array
REAL,    DIMENSION(size(PRHODREF),6) :: ZZW1              ! Work arrays
!
!-------------------------------------------------------------------------------
!
  IHAIL = 0
  DO JJ = 1, SIZE(GHAIL)
    IF ( PRHT(JJ)>XRTMIN(7) ) THEN
      IHAIL = IHAIL + 1
      I1(IHAIL) = JJ
      GHAIL(JJ) = .TRUE.
    ELSE
      GHAIL(JJ) = .FALSE.
    END IF
  END DO
!
  IF( IHAIL>0 ) THEN
!
!*       7.2    compute the Wet growth of hail
!
    WHERE ( GHAIL(:) )
      PLBDAH(:)  = XLBH*( PRHODREF(:)*MAX( PRHT(:),XRTMIN(7) ) )**XLBEXH
    END WHERE
!
    ZZW1(:,:) = 0.0
    WHERE( GHAIL(:) .AND. ((PRCT(:)>XRTMIN(2) .AND. PRCS(:)>0.0)) )
      ZZW(:) = PLBDAH(:)**(XCXH-XDH-2.0) * PRHODREF(:)**(-XCEXVT)
      ZZW1(:,1) = MIN( PRCS(:),XFWETH * PRCT(:) * ZZW(:) )             ! RCWETH
    END WHERE
    WHERE( GHAIL(:) .AND. ((PRIT(:)>XRTMIN(4) .AND. PRIS(:)>0.0)) )
      ZZW(:) = PLBDAH(:)**(XCXH-XDH-2.0) * PRHODREF(:)**(-XCEXVT)
      ZZW1(:,2) = MIN( PRIS(:),XFWETH * PRIT(:) * ZZW(:) )             ! RIWETH
    END WHERE
!
!*       7.2.1  accretion of aggregates on the hailstones
!
    IGWET = 0
    DO JJ = 1, SIZE(GWET)
      IF ( GHAIL(JJ) .AND. PRST(JJ)>XRTMIN(5) .AND. PRSS(JJ)>0.0 ) THEN
        IGWET = IGWET + 1
        I1(IGWET) = JJ
        GWET(JJ) = .TRUE.
      ELSE
        GWET(JJ) = .FALSE.
      END IF
    END DO
!
    IF( IGWET>0 ) THEN
!
!*       7.2.2  allocations
!
      ALLOCATE(ZVEC1(IGWET))
      ALLOCATE(ZVEC2(IGWET))
      ALLOCATE(ZVEC3(IGWET))
      ALLOCATE(IVEC1(IGWET))
      ALLOCATE(IVEC2(IGWET))
!
!*       7.2.3  select the (PLBDAH,PLBDAS) couplet
!
      DO JJ = 1, IGWET
        ZVEC1(JJ) = PLBDAH(I1(JJ))
        ZVEC2(JJ) = PLBDAS(I1(JJ))
      END DO
!
!*       7.2.4  find the next lower indice for the PLBDAG and for the PLBDAS
!               in the geometrical set of (Lbda_h,Lbda_s) couplet use to
!               tabulate the SWETH-kernel
!
      ZVEC1(1:IGWET) = MAX( 1.00001, MIN( REAL(NWETLBDAH)-0.00001,           &
                            XWETINTP1H * LOG( ZVEC1(1:IGWET) ) + XWETINTP2H ) )
      IVEC1(1:IGWET) = INT( ZVEC1(1:IGWET) )
      ZVEC1(1:IGWET) = ZVEC1(1:IGWET) - REAL( IVEC1(1:IGWET) )
!
      ZVEC2(1:IGWET) = MAX( 1.00001, MIN( REAL(NWETLBDAS)-0.00001,           &
                            XWETINTP1S * LOG( ZVEC2(1:IGWET) ) + XWETINTP2S ) )
      IVEC2(1:IGWET) = INT( ZVEC2(1:IGWET) )
      ZVEC2(1:IGWET) = ZVEC2(1:IGWET) - REAL( IVEC2(1:IGWET) )
!
!*       7.2.5  perform the bilinear interpolation of the normalized
!               SWETH-kernel
!
      DO JJ = 1,IGWET
        ZVEC3(JJ) = (  XKER_SWETH(IVEC1(JJ)+1,IVEC2(JJ)+1)* ZVEC2(JJ)          &
                     - XKER_SWETH(IVEC1(JJ)+1,IVEC2(JJ)  )*(ZVEC2(JJ) - 1.0) ) &
                                                                   * ZVEC1(JJ) &
                   - ( XKER_SWETH(IVEC1(JJ)  ,IVEC2(JJ)+1)* ZVEC2(JJ)          &
                     - XKER_SWETH(IVEC1(JJ)  ,IVEC2(JJ)  )*(ZVEC2(JJ) - 1.0) ) &
                                                          * (ZVEC1(JJ) - 1.0)
      END DO
      ZZW(:) = 0.
      DO JJ = 1, IGWET
        ZZW(I1(JJ)) = ZVEC3(JJ)
      END DO
!
      WHERE( GWET(:) )
        ZZW1(:,3) = MIN( PRSS(:),XFSWETH*ZZW(:)                       & ! RSWETH
                      *( PLBDAS(:)**(XCXS-XBS) )*( PLBDAH(:)**XCXH )  &
                         *( PRHODREF(:)**(-XCEXVT-1.) )               &
                         *( XLBSWETH1/( PLBDAH(:)**2              ) + &
                            XLBSWETH2/( PLBDAH(:)   * PLBDAS(:)   ) + &
                            XLBSWETH3/(               PLBDAS(:)**2) ) )
      END WHERE
      DEALLOCATE(IVEC2)
      DEALLOCATE(IVEC1)
      DEALLOCATE(ZVEC3)
      DEALLOCATE(ZVEC2)
      DEALLOCATE(ZVEC1)
    END IF
!
!*       7.2.6  accretion of graupeln on the hailstones
!
    IGWET = 0
    DO JJ = 1, SIZE(GWET)
      IF ( GHAIL(JJ) .AND. PRGT(JJ)>XRTMIN(6) .AND. PRGS(JJ)>0.0 ) THEN
        IGWET = IGWET + 1
        I1(IGWET) = JJ
        GWET(JJ) = .TRUE.
      ELSE
        GWET(JJ) = .FALSE.
      END IF
    END DO
!
    IF( IGWET>0 ) THEN
!
!*       7.2.7  allocations
!
      ALLOCATE(ZVEC1(IGWET))
      ALLOCATE(ZVEC2(IGWET))
      ALLOCATE(ZVEC3(IGWET))
      ALLOCATE(IVEC1(IGWET))
      ALLOCATE(IVEC2(IGWET))
!
!*       7.2.8  select the (PLBDAH,PLBDAG) couplet
!
      DO JJ = 1, IGWET
        ZVEC1(JJ) = PLBDAH(I1(JJ))
        ZVEC2(JJ) = PLBDAG(I1(JJ))
      END DO
!
!*       7.2.9  find the next lower indice for the PLBDAH and for the PLBDAG
!               in the geometrical set of (Lbda_h,Lbda_g) couplet use to
!               tabulate the GWETH-kernel
!
      ZVEC1(1:IGWET) = MAX( 1.00001, MIN( REAL(NWETLBDAG)-0.00001,           &
                            XWETINTP1H * LOG( ZVEC1(1:IGWET) ) + XWETINTP2H ) )
      IVEC1(1:IGWET) = INT( ZVEC1(1:IGWET) )
      ZVEC1(1:IGWET) = ZVEC1(1:IGWET) - REAL( IVEC1(1:IGWET) )
!
      ZVEC2(1:IGWET) = MAX( 1.00001, MIN( REAL(NWETLBDAG)-0.00001,           &
                            XWETINTP1G * LOG( ZVEC2(1:IGWET) ) + XWETINTP2G ) )
      IVEC2(1:IGWET) = INT( ZVEC2(1:IGWET) )
      ZVEC2(1:IGWET) = ZVEC2(1:IGWET) - REAL( IVEC2(1:IGWET) )
!
!*       7.2.10 perform the bilinear interpolation of the normalized
!               GWETH-kernel
!
      DO JJ = 1,IGWET
        ZVEC3(JJ) = (  XKER_GWETH(IVEC1(JJ)+1,IVEC2(JJ)+1)* ZVEC2(JJ)          &
                     - XKER_GWETH(IVEC1(JJ)+1,IVEC2(JJ)  )*(ZVEC2(JJ) - 1.0) ) &
                                                                   * ZVEC1(JJ) &
                  - (  XKER_GWETH(IVEC1(JJ)  ,IVEC2(JJ)+1)* ZVEC2(JJ)          &
                     - XKER_GWETH(IVEC1(JJ)  ,IVEC2(JJ)  )*(ZVEC2(JJ) - 1.0) ) &
                                                          * (ZVEC1(JJ) - 1.0)
      END DO
      ZZW(:) = 0.
      DO JJ = 1, IGWET
        ZZW(I1(JJ)) = ZVEC3(JJ)
      END DO
!
      WHERE( GWET(:) )
        ZZW1(:,5) = MAX(MIN( PRGS(:),XFGWETH*ZZW(:)                       & ! RGWETH
                      *( PLBDAG(:)**(XCXG-XBG) )*( PLBDAH(:)**XCXH )  &
                         *( PRHODREF(:)**(-XCEXVT-1.) )               &
                         *( XLBGWETH1/( PLBDAH(:)**2              ) + &
                            XLBGWETH2/( PLBDAH(:)   * PLBDAG(:)   ) + &
                            XLBGWETH3/(               PLBDAG(:)**2) ) ),0. )
      END WHERE
      DEALLOCATE(IVEC2)
      DEALLOCATE(IVEC1)
      DEALLOCATE(ZVEC3)
      DEALLOCATE(ZVEC2)
      DEALLOCATE(ZVEC1)
    END IF
!
!*       7.3    compute the Wet growth of hail
!
    ZZW(:) = 0.0
    WHERE( GHAIL(:) .AND. PZT(:)<XTT )
      ZZW(:) = PRVT(:)*PPRES(:)/((XMV/XMD)+PRVT(:)) ! Vapor pressure
      ZZW(:) = PKA(:)*(XTT-PZT(:)) +                                 &
                ( PDV(:)*(XLVTT + ( XCPV - XCL ) * ( PZT(:) - XTT )) &
                            *(XESTT-ZZW(:))/(XRV*PZT(:))             )
!
! compute RWETH
!
      ZZW(:)  =  MAX(0.,  ( ZZW(:) * ( X0DEPH*       PLBDAH(:)**XEX0DEPH +     &
                                X1DEPH*PCJ(:)*PLBDAH(:)**XEX1DEPH ) +   &
                   ( ZZW1(:,2)+ZZW1(:,3)+ZZW1(:,5) ) *                  &
                   ( PRHODREF(:)*(XLMTT+(XCI-XCL)*(XTT-PZT(:)))   ) ) / &
                         ( PRHODREF(:)*(XLMTT-XCL*(XTT-PZT(:))) ) )
!
      ZZW1(:,6) = MAX( ZZW(:) - ZZW1(:,2) - ZZW1(:,3) - ZZW1(:,5),0.) ! RCWETH+RRWETH
    END WHERE
    WHERE ( GHAIL(:) .AND. PZT(:)<XTT  .AND. ZZW1(:,6)/=0.)
!
! limitation of the available rainwater mixing ratio (RRWETH < RRS !)
!
      ZZW1(:,4) = MAX( 0.0,MIN( ZZW1(:,6),PRRS(:)+ZZW1(:,1) ) )
      PUSW(:)   = ZZW1(:,4) / ZZW1(:,6)
      ZZW1(:,2) = ZZW1(:,2)*PUSW(:)
      ZZW1(:,3) = ZZW1(:,3)*PUSW(:)
      ZZW1(:,5) = ZZW1(:,5)*PUSW(:)
      ZZW(:)    = ZZW1(:,4) + ZZW1(:,2) + ZZW1(:,3) + ZZW1(:,5)
!
!*       7.1.6  integrate the Wet growth of hail
!
      PRCS(:) = PRCS(:) - ZZW1(:,1)
      PRIS(:) = PRIS(:) - ZZW1(:,2)
      PRSS(:) = PRSS(:) - ZZW1(:,3)
      PRGS(:) = PRGS(:) - ZZW1(:,5)
      PRHS(:) = PRHS(:) + ZZW(:)
      PRRS(:) = MAX( 0.0,PRRS(:) - ZZW1(:,4) + ZZW1(:,1) )
      PTHS(:) = PTHS(:) + ZZW1(:,4)*(PLSFACT(:)-PLVFACT(:))
                           ! f(L_f*(RCWETH+RRWETH))
    END WHERE
  END IF
    IF (LBUDGET_TH) CALL BUDGET (                                                 &
                   UNPACK(PTHS(:),MASK=OMICRO(:,:,:),FIELD=PTHS3D)*PRHODJ3D(:,:,:),&
                                                                4,'WETH_BU_RTH')
    IF (LBUDGET_RC) CALL BUDGET (                                                 &
                       UNPACK(PRCS(:)*PRHODJ(:),MASK=OMICRO(:,:,:),FIELD=0.0), &
                                                                7,'WETH_BU_RRC')
    IF (LBUDGET_RR) CALL BUDGET (                                                 &
                       UNPACK(PRRS(:)*PRHODJ(:),MASK=OMICRO(:,:,:),FIELD=0.0), &
                                                                8,'WETH_BU_RRR')
    IF (LBUDGET_RI) CALL BUDGET (                                                 &
                       UNPACK(PRIS(:)*PRHODJ(:),MASK=OMICRO(:,:,:),FIELD=0.0), &
                                                                9,'WETH_BU_RRI')
    IF (LBUDGET_RS) CALL BUDGET (                                                 &
                       UNPACK(PRSS(:)*PRHODJ(:),MASK=OMICRO(:,:,:),FIELD=0.0), &
                                                               10,'WETH_BU_RRS')
    IF (LBUDGET_RG) CALL BUDGET (                                                 &
                       UNPACK(PRGS(:)*PRHODJ(:),MASK=OMICRO(:,:,:),FIELD=0.0), &
                                                               11,'WETH_BU_RRG')
    IF (LBUDGET_RH) CALL BUDGET (                                                 &
                       UNPACK(PRHS(:)*PRHODJ(:),MASK=OMICRO(:,:,:),FIELD=0.0), &
                                                               12,'WETH_BU_RRH')
!
!
! ici LRECONVH et un flag pour autoriser une reconversion partielle de
!la grele en gresil
!
!  IF( IHAIL>0  ) THEN
!
!UPG_CD
!
!
!*       7.45   Conversion of the hailstones into graupel
!
!    XDUMMY6=0.01E-3
!    XDUMMY7=0.001E-3
!    WHERE( PRHT(:)<XDUMMY6 .AND. PRCT(:)<XDUMMY7 .AND. PZT(:)<XTT )
!      ZZW(:) = MIN( 1.0,MAX( 0.0,1.0-(PRCT(:)/XDUMMY7) ) )
!
! assume a linear percent conversion rate of hail into graupel
!
!      ZZW(:)  = PRHS(:)*ZZW(:)
!      PRGS(:) = PRGS(:) + ZZW(:)                      !   partial conversion
!      PRHS(:) = PRHS(:) - ZZW(:)                      ! of hail into graupel
!
!    END WHERE
!  END IF




  IF( IHAIL>0 ) THEN
!
!*       7.5    Melting of the hailstones
!
    ZZW(:) = 0.0
    WHERE( GHAIL(:) .AND. (PRHS(:)>0.0) .AND. (PZT(:)>XTT) )
      ZZW(:) = PRVT(:)*PPRES(:)/((XMV/XMD)+PRVT(:)) ! Vapor pressure
      ZZW(:) = PKA(:)*(XTT-PZT(:)) +                              &
             ( PDV(:)*(XLVTT + ( XCPV - XCL ) * ( PZT(:) - XTT )) &
                             *(XESTT-ZZW(:))/(XRV*PZT(:))         )
!
! compute RHMLTR
!
      ZZW(:)  = MIN( PRHS(:), MAX( 0.0,( -ZZW(:) *                     &
                             ( X0DEPH*       PLBDAH(:)**XEX0DEPH +     &
                               X1DEPH*PCJ(:)*PLBDAH(:)**XEX1DEPH ) -   &
                      ZZW1(:,6)*( PRHODREF(:)*XCL*(XTT-PZT(:))) ) /    &
                                               ( PRHODREF(:)*XLMTT ) ) )
      PRRS(:) = PRRS(:) + ZZW(:)
      PRHS(:) = PRHS(:) - ZZW(:)
      PTHS(:) = PTHS(:) - ZZW(:)*(PLSFACT(:)-PLVFACT(:)) ! f(L_f*(-RHMLTR))
    END WHERE
  END IF

    IF (LBUDGET_TH) CALL BUDGET (                                                 &
                   UNPACK(PTHS(:),MASK=OMICRO(:,:,:),FIELD=PTHS3D)*PRHODJ3D(:,:,:),&
                                                                4,'HMLT_BU_RTH')
    IF (LBUDGET_RR) CALL BUDGET (                                                 &
                       UNPACK(PRRS(:)*PRHODJ(:),MASK=OMICRO(:,:,:),FIELD=0.0), &
                                                                8,'HMLT_BU_RRR')
    IF (LBUDGET_RH) CALL BUDGET (                                                 &
                       UNPACK(PRHS(:)*PRHODJ(:),MASK=OMICRO(:,:,:),FIELD=0.0), &
                                                               12,'HMLT_BU_RRH')
!
END SUBROUTINE RAIN_ICE_FAST_RH

END MODULE MODE_RAIN_ICE_FAST_RH
