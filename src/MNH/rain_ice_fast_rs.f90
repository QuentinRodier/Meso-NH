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
MODULE MODE_RAIN_ICE_FAST_RS

  IMPLICIT NONE

  PRIVATE

  PUBLIC :: RAIN_ICE_FAST_RS

CONTAINS

SUBROUTINE RAIN_ICE_FAST_RS(PTSTEP, OMICRO, PRHODREF, PRVT, PRCT, PRRT, PRST, PRHODJ, PPRES, PZT, &
                            PLBDAR, PLBDAS, PLSFACT, PLVFACT, PCJ, PKA, PDV, PRHODJ3D, PTHS3D, &
                            PRCS, PRRS, PRSS, PRGS, PTHS)
!
!*      0. DECLARATIONS
!          ------------
!
use MODD_BUDGET,         only: LBUDGET_RC, LBUDGET_RG, LBUDGET_RR, LBUDGET_RS, LBUDGET_TH
use MODD_CST,            only: XCL, XCPV, XESTT, XLMTT, XLVTT, XMD, XMV, XRV, XTT
use MODD_RAIN_ICE_DESCR, only: XBS, XCEXVT, XCXS, XRTMIN
use MODD_RAIN_ICE_PARAM, only: NACCLBDAR, NACCLBDAS, NGAMINC, X0DEPS, X1DEPS, XACCINTP1R, XACCINTP1S, XACCINTP2R, XACCINTP2S, &
                               XCRIMSG, XCRIMSS, XEX0DEPS, XEX1DEPS, XEXCRIMSG, XEXCRIMSS, XEXSRIMCG, XFRACCSS,               &
                               XFSACCRG, XFSCVMG, XGAMINC_RIM1, XGAMINC_RIM1, XGAMINC_RIM2, XKER_RACCS,                       &
                               XKER_RACCSS, XKER_SACCRG, XLBRACCS1, XLBRACCS2, XLBRACCS3, XLBSACCR1, XLBSACCR2, XLBSACCR3,    &
                               XRIMINTP1, XRIMINTP2, XSRIMCG
!
use MODI_BUDGET
!
IMPLICIT NONE
!
!*       0.1   Declarations of dummy arguments :
!
REAL,                       intent(in)    :: PTSTEP   ! Double Time step
                                                      ! (single if cold start)
LOGICAL,  DIMENSION(:,:,:), intent(in)    :: OMICRO   ! Test where to compute all processes
REAL,     DIMENSION(:),     intent(in)    :: PRHODREF ! RHO Dry REFerence
REAL,     DIMENSION(:),     intent(in)    :: PRVT     ! Water vapor m.r. at t
REAL,     DIMENSION(:),     intent(in)    :: PRCT     ! Cloud water m.r. at t
REAL,     DIMENSION(:),     intent(in)    :: PRRT     ! Rain water m.r. at t
REAL,     DIMENSION(:),     intent(in)    :: PRST     ! Snow/aggregate m.r. at t
REAL,     DIMENSION(:),     intent(in)    :: PRHODJ   ! RHO times Jacobian
REAL,     DIMENSION(:),     intent(in)    :: PPRES    ! Pressure
REAL,     DIMENSION(:),     intent(in)    :: PZT      ! Temperature
REAL,     DIMENSION(:),     intent(in)    :: PLBDAR   ! Slope parameter of the raindrop  distribution
REAL,     DIMENSION(:),     intent(in)    :: PLBDAS   ! Slope parameter of the aggregate distribution
REAL,     DIMENSION(:),     intent(in)    :: PLSFACT  ! L_s/(Pi_ref*C_ph)
REAL,     DIMENSION(:),     intent(in)    :: PLVFACT  ! L_v/(Pi_ref*C_ph)
REAL,     DIMENSION(:),     intent(in)    :: PCJ      ! Function to compute the ventilation coefficient
REAL,     DIMENSION(:),     intent(in)    :: PKA      ! Thermal conductivity of the air
REAL,     DIMENSION(:),     intent(in)    :: PDV      ! Diffusivity of water vapor in the air
REAL,     DIMENSION(:,:,:), INTENT(IN)    :: PRHODJ3D ! Dry density * Jacobian
REAL,     DIMENSION(:,:,:), INTENT(IN)    :: PTHS3D   ! Theta source
REAL,     DIMENSION(:),     INTENT(INOUT) :: PRCS     ! Cloud water m.r. source
REAL,     DIMENSION(:),     INTENT(INOUT) :: PRRS     ! Rain water m.r. source
REAL,     DIMENSION(:),     INTENT(INOUT) :: PRSS     ! Snow/aggregate m.r. source
REAL,     DIMENSION(:),     INTENT(INOUT) :: PRGS     ! Graupel m.r. source
REAL,     DIMENSION(:),     INTENT(INOUT) :: PTHS     ! Theta source
!
!*       0.2  declaration of local variables
!
INTEGER                              :: IGRIM, IGACC
INTEGER                              :: JJ
INTEGER, DIMENSION(size(PRHODREF))   :: I1
INTEGER, DIMENSION(:), ALLOCATABLE   :: IVEC1, IVEC2      ! Vectors of indices for interpolations
LOGICAL, DIMENSION(size(PRHODREF))   :: GRIM              ! Test where to compute riming
LOGICAL, DIMENSION(size(PRHODREF))   :: GACC              ! Test where to compute accretion
REAL,    DIMENSION(size(PRHODREF))   :: ZZW               ! Work array
REAL,    DIMENSION(:), ALLOCATABLE   :: ZVEC1,ZVEC2,ZVEC3 ! Work vectors for interpolations
REAL,    DIMENSION(size(PRHODREF),4) :: ZZW1              ! Work arrays
!-------------------------------------------------------------------------------
!
!*       5.1    cloud droplet riming of the aggregates
!
  ZZW1(:,:) = 0.0
!
  IGRIM = 0
  DO JJ = 1, SIZE(GRIM)
    IF (PRCT(JJ)>XRTMIN(2) .AND. PRST(JJ)>XRTMIN(5) .AND. PRCS(JJ)>0.0 .AND. PZT(JJ)<XTT ) THEN
      IGRIM = IGRIM + 1
      I1(IGRIM) = JJ
      GRIM(JJ) = .TRUE.
    ELSE
      GRIM(JJ) = .FALSE.
    END IF
  END DO
  !
  IF( IGRIM>0 ) THEN
!
!        5.1.0  allocations
!
    ALLOCATE(ZVEC1(IGRIM))
    ALLOCATE(ZVEC2(IGRIM))
    ALLOCATE(IVEC2(IGRIM))
!
!        5.1.1  select the PLBDAS
!
    DO JJ = 1, IGRIM
      ZVEC1(JJ) = PLBDAS(I1(JJ))
    END DO
!
!        5.1.2  find the next lower indice for the PLBDAS in the geometrical
!               set of Lbda_s used to tabulate some moments of the incomplete
!               gamma function
!
    ZVEC2(1:IGRIM) = MAX( 1.00001, MIN( REAL(NGAMINC)-0.00001,           &
                          XRIMINTP1 * LOG( ZVEC1(1:IGRIM) ) + XRIMINTP2 ) )
    IVEC2(1:IGRIM) = INT( ZVEC2(1:IGRIM) )
    ZVEC2(1:IGRIM) = ZVEC2(1:IGRIM) - REAL( IVEC2(1:IGRIM) )
!
!        5.1.3  perform the linear interpolation of the normalized
!               "2+XDS"-moment of the incomplete gamma function
!
    ZVEC1(1:IGRIM) =   XGAMINC_RIM1( IVEC2(1:IGRIM)+1 )* ZVEC2(1:IGRIM)      &
                     - XGAMINC_RIM1( IVEC2(1:IGRIM)   )*(ZVEC2(1:IGRIM) - 1.0)
    ZZW(:) = 0.
    DO JJ = 1, IGRIM
      ZZW(I1(JJ)) = ZVEC1(JJ)
    END DO
!
!        5.1.4  riming of the small sized aggregates
!
    WHERE ( GRIM(:) )
      ZZW1(:,1) = MIN( PRCS(:),                                &
                     XCRIMSS * ZZW(:) * PRCT(:)                & ! RCRIMSS
                                      *   PLBDAS(:)**XEXCRIMSS &
                                      * PRHODREF(:)**(-XCEXVT) )
      PRCS(:) = PRCS(:) - ZZW1(:,1)
      PRSS(:) = PRSS(:) + ZZW1(:,1)
      PTHS(:) = PTHS(:) + ZZW1(:,1)*(PLSFACT(:)-PLVFACT(:)) ! f(L_f*(RCRIMSS))
    END WHERE
!
!        5.1.5  perform the linear interpolation of the normalized
!               "XBS"-moment of the incomplete gamma function
!
    ZVEC1(1:IGRIM) =  XGAMINC_RIM2( IVEC2(1:IGRIM)+1 )* ZVEC2(1:IGRIM)      &
                    - XGAMINC_RIM2( IVEC2(1:IGRIM)   )*(ZVEC2(1:IGRIM) - 1.0)
    ZZW(:) = 0.
    DO JJ = 1, IGRIM
      ZZW(I1(JJ)) = ZVEC1(JJ)
    END DO
!
!        5.1.6  riming-conversion of the large sized aggregates into graupeln
!
!
    WHERE ( GRIM(:) .AND. (PRSS(:)>0.0) )
      ZZW1(:,2) = MIN( PRCS(:),                     &
                   XCRIMSG * PRCT(:)                & ! RCRIMSG
                           *  PLBDAS(:)**XEXCRIMSG  &
                           * PRHODREF(:)**(-XCEXVT) &
                           - ZZW1(:,1)              )
      ZZW1(:,3) = MIN( PRSS(:),                         &
                       XSRIMCG * PLBDAS(:)**XEXSRIMCG   & ! RSRIMCG
                               * (1.0 - ZZW(:) )/(PTSTEP*PRHODREF(:)) )
      PRCS(:) = PRCS(:) - ZZW1(:,2)
      PRSS(:) = PRSS(:) - ZZW1(:,3)
      PRGS(:) = PRGS(:) + ZZW1(:,2)+ZZW1(:,3)
      PTHS(:) = PTHS(:) + ZZW1(:,2)*(PLSFACT(:)-PLVFACT(:)) ! f(L_f*(RCRIMSG))
    END WHERE
    DEALLOCATE(IVEC2)
    DEALLOCATE(ZVEC2)
    DEALLOCATE(ZVEC1)
  END IF
  IF (LBUDGET_TH) CALL BUDGET (                                               &
               UNPACK(PTHS(:),MASK=OMICRO(:,:,:),FIELD=PTHS3D)*PRHODJ3D(:,:,:),   &
                                                             4,'RIM_BU_RTH')
  IF (LBUDGET_RC) CALL BUDGET (                                               &
                   UNPACK(PRCS(:)*PRHODJ(:),MASK=OMICRO(:,:,:),FIELD=0.0),    &
                                                             7,'RIM_BU_RRC')
  IF (LBUDGET_RS) CALL BUDGET (                                               &
                   UNPACK(PRSS(:)*PRHODJ(:),MASK=OMICRO(:,:,:),FIELD=0.0),    &
                                                            10,'RIM_BU_RRS')
  IF (LBUDGET_RG) CALL BUDGET (                                               &
                   UNPACK(PRGS(:)*PRHODJ(:),MASK=OMICRO(:,:,:),FIELD=0.0),    &
                                                            11,'RIM_BU_RRG')
!
!*       5.2    rain accretion onto the aggregates
!
  ZZW1(:,2:3) = 0.0

  IGACC = 0
  DO JJ = 1, SIZE(GACC)
    IF (PRRT(JJ)>XRTMIN(3) .AND. PRST(JJ)>XRTMIN(5) .AND. PRRS(JJ)>0.0 .AND. PZT(JJ)<XTT ) THEN
      IGACC = IGACC + 1
      I1(IGACC) = JJ
      GACC(JJ) = .TRUE.
    ELSE
      GACC(JJ) = .FALSE.
    END IF
  END DO
  !
  IF( IGACC>0 ) THEN
!
!        5.2.0  allocations
!
    ALLOCATE(ZVEC1(IGACC))
    ALLOCATE(ZVEC2(IGACC))
    ALLOCATE(ZVEC3(IGACC))
    ALLOCATE(IVEC1(IGACC))
    ALLOCATE(IVEC2(IGACC))
!
!        5.2.1  select the (PLBDAS,PLBDAR) couplet
!
    DO JJ = 1, IGACC
      ZVEC1(JJ) = PLBDAS(I1(JJ))
      ZVEC2(JJ) = PLBDAR(I1(JJ))
    END DO
!
!        5.2.2  find the next lower indice for the PLBDAS and for the PLBDAR
!               in the geometrical set of (Lbda_s,Lbda_r) couplet use to
!               tabulate the RACCSS-kernel
!
    ZVEC1(1:IGACC) = MAX( 1.00001, MIN( REAL(NACCLBDAS)-0.00001,           &
                          XACCINTP1S * LOG( ZVEC1(1:IGACC) ) + XACCINTP2S ) )
    IVEC1(1:IGACC) = INT( ZVEC1(1:IGACC) )
    ZVEC1(1:IGACC) = ZVEC1(1:IGACC) - REAL( IVEC1(1:IGACC) )
!
    ZVEC2(1:IGACC) = MAX( 1.00001, MIN( REAL(NACCLBDAR)-0.00001,           &
                          XACCINTP1R * LOG( ZVEC2(1:IGACC) ) + XACCINTP2R ) )
    IVEC2(1:IGACC) = INT( ZVEC2(1:IGACC) )
    ZVEC2(1:IGACC) = ZVEC2(1:IGACC) - REAL( IVEC2(1:IGACC) )
!
!        5.2.3  perform the bilinear interpolation of the normalized
!               RACCSS-kernel
!
    DO JJ = 1,IGACC
      ZVEC3(JJ) =  (  XKER_RACCSS(IVEC1(JJ)+1,IVEC2(JJ)+1)* ZVEC2(JJ)          &
                    - XKER_RACCSS(IVEC1(JJ)+1,IVEC2(JJ)  )*(ZVEC2(JJ) - 1.0) ) &
                                                          * ZVEC1(JJ) &
                 - (  XKER_RACCSS(IVEC1(JJ)  ,IVEC2(JJ)+1)* ZVEC2(JJ)          &
                    - XKER_RACCSS(IVEC1(JJ)  ,IVEC2(JJ)  )*(ZVEC2(JJ) - 1.0) ) &
                                                          * (ZVEC1(JJ) - 1.0)
    END DO
    ZZW(:) = 0.
    DO JJ = 1, IGACC
      ZZW(I1(JJ)) = ZVEC3(JJ)
    END DO
!
!        5.2.4  raindrop accretion on the small sized aggregates
!
    WHERE ( GACC(:) )
      ZZW1(:,2) =                                            & !! coef of RRACCS
              XFRACCSS*( PLBDAS(:)**XCXS )*( PRHODREF(:)**(-XCEXVT-1.) ) &
         *( XLBRACCS1/((PLBDAS(:)**2)               ) +                  &
            XLBRACCS2/( PLBDAS(:)    * PLBDAR(:)    ) +                  &
            XLBRACCS3/(               (PLBDAR(:)**2)) )/PLBDAR(:)**4
      ZZW1(:,4) = MIN( PRRS(:),ZZW1(:,2)*ZZW(:) )           ! RRACCSS
      PRRS(:) = PRRS(:) - ZZW1(:,4)
      PRSS(:) = PRSS(:) + ZZW1(:,4)
      PTHS(:) = PTHS(:) + ZZW1(:,4)*(PLSFACT(:)-PLVFACT(:)) ! f(L_f*(RRACCSS))
    END WHERE
!
!        5.2.4b perform the bilinear interpolation of the normalized
!               RACCS-kernel
!
    DO JJ = 1,IGACC
      ZVEC3(JJ) =  (   XKER_RACCS(IVEC2(JJ)+1,IVEC1(JJ)+1)* ZVEC1(JJ)          &
                    -  XKER_RACCS(IVEC2(JJ)+1,IVEC1(JJ)  )*(ZVEC1(JJ) - 1.0) ) &
                                                                   * ZVEC2(JJ) &
                 - (   XKER_RACCS(IVEC2(JJ)  ,IVEC1(JJ)+1)* ZVEC1(JJ)          &
                    -  XKER_RACCS(IVEC2(JJ)  ,IVEC1(JJ)  )*(ZVEC1(JJ) - 1.0) ) &
                                                           * (ZVEC2(JJ) - 1.0)
    END DO
    DO JJ = 1, IGACC
      ZZW1(I1(JJ), 2) =  ZZW1(I1(JJ), 2 ) * ZVEC3(JJ)
    END DO
                                                                       !! RRACCS!
!        5.2.5  perform the bilinear interpolation of the normalized
!               SACCRG-kernel
!
    DO JJ = 1,IGACC
      ZVEC3(JJ) =  (  XKER_SACCRG(IVEC2(JJ)+1,IVEC1(JJ)+1)* ZVEC1(JJ)          &
                    - XKER_SACCRG(IVEC2(JJ)+1,IVEC1(JJ)  )*(ZVEC1(JJ) - 1.0) ) &
                                                          * ZVEC2(JJ) &
                 - (  XKER_SACCRG(IVEC2(JJ)  ,IVEC1(JJ)+1)* ZVEC1(JJ)          &
                    - XKER_SACCRG(IVEC2(JJ)  ,IVEC1(JJ)  )*(ZVEC1(JJ) - 1.0) ) &
                                                          * (ZVEC2(JJ) - 1.0)
    END DO
    ZZW(:) = 0.
    DO JJ = 1, IGACC
      ZZW(I1(JJ)) = ZVEC3(JJ)
    END DO
!
!        5.2.6  raindrop accretion-conversion of the large sized aggregates
!               into graupeln
!
    WHERE ( GACC(:) .AND. (PRSS(:)>0.0) )
      ZZW1(:,2) = MAX( MIN( PRRS(:),ZZW1(:,2)-ZZW1(:,4) ),0.0 )       ! RRACCSG
    END WHERE
    WHERE ( GACC(:) .AND. (PRSS(:)>0.0) .AND. ZZW1(:,2)>0.0 )
      ZZW1(:,3) = MIN( PRSS(:),XFSACCRG*ZZW(:)*                     & ! RSACCRG
            ( PLBDAS(:)**(XCXS-XBS) )*( PRHODREF(:)**(-XCEXVT-1.) ) &
           *( XLBSACCR1/((PLBDAR(:)**2)               ) +           &
              XLBSACCR2/( PLBDAR(:)    * PLBDAS(:)    ) +           &
              XLBSACCR3/(               (PLBDAS(:)**2)) )/PLBDAR(:) )
      PRRS(:) = PRRS(:) - ZZW1(:,2)
      PRSS(:) = PRSS(:) - ZZW1(:,3)
      PRGS(:) = PRGS(:) + ZZW1(:,2)+ZZW1(:,3)
      PTHS(:) = PTHS(:) + ZZW1(:,2)*(PLSFACT(:)-PLVFACT(:)) !
                               ! f(L_f*(RRACCSG))
    END WHERE
    DEALLOCATE(IVEC2)
    DEALLOCATE(IVEC1)
    DEALLOCATE(ZVEC3)
    DEALLOCATE(ZVEC2)
    DEALLOCATE(ZVEC1)
  END IF
  IF (LBUDGET_TH) CALL BUDGET (                                               &
               UNPACK(PTHS(:),MASK=OMICRO(:,:,:),FIELD=PTHS3D)*PRHODJ3D(:,:,:),   &
                                                             4,'ACC_BU_RTH')
  IF (LBUDGET_RR) CALL BUDGET (                                               &
                   UNPACK(PRRS(:)*PRHODJ(:),MASK=OMICRO(:,:,:),FIELD=0.0),    &
                                                             8,'ACC_BU_RRR')
  IF (LBUDGET_RS) CALL BUDGET (                                               &
                   UNPACK(PRSS(:)*PRHODJ(:),MASK=OMICRO(:,:,:),FIELD=0.0),    &
                                                            10,'ACC_BU_RRS')
  IF (LBUDGET_RG) CALL BUDGET (                                               &
                   UNPACK(PRGS(:)*PRHODJ(:),MASK=OMICRO(:,:,:),FIELD=0.0),    &
                                                            11,'ACC_BU_RRG')
!
!*       5.3    Conversion-Melting of the aggregates
!
  ZZW(:) = 0.0
  WHERE( (PRST(:)>XRTMIN(5)) .AND. (PRSS(:)>0.0) .AND. (PZT(:)>XTT) )
    ZZW(:) = PRVT(:)*PPRES(:)/((XMV/XMD)+PRVT(:)) ! Vapor pressure
    ZZW(:) =  PKA(:)*(XTT-PZT(:)) +                                 &
               ( PDV(:)*(XLVTT + ( XCPV - XCL ) * ( PZT(:) - XTT )) &
                           *(XESTT-ZZW(:))/(XRV*PZT(:))             )
!
! compute RSMLT
!
    ZZW(:)  = MIN( PRSS(:), XFSCVMG*MAX( 0.0,( -ZZW(:) *             &
                           ( X0DEPS*       PLBDAS(:)**XEX0DEPS +     &
                             X1DEPS*PCJ(:)*PLBDAS(:)**XEX1DEPS ) -   &
                                     ( ZZW1(:,1)+ZZW1(:,4) ) *       &
                              ( PRHODREF(:)*XCL*(XTT-PZT(:))) ) /    &
                                             ( PRHODREF(:)*XLMTT ) ) )
!
! note that RSCVMG = RSMLT*XFSCVMG but no heat is exchanged (at the rate RSMLT)
! because the graupeln produced by this process are still icy!!!
!
    PRSS(:) = PRSS(:) - ZZW(:)
    PRGS(:) = PRGS(:) + ZZW(:)
  END WHERE
  IF (LBUDGET_RS) CALL BUDGET (                                                 &
                     UNPACK(PRSS(:)*PRHODJ(:),MASK=OMICRO(:,:,:),FIELD=0.0),    &
                                                             10,'CMEL_BU_RRS')
  IF (LBUDGET_RG) CALL BUDGET (                                                 &
                     UNPACK(PRGS(:)*PRHODJ(:),MASK=OMICRO(:,:,:),FIELD=0.0),    &
                                                             11,'CMEL_BU_RRG')
!
END SUBROUTINE RAIN_ICE_FAST_RS

END MODULE MODE_RAIN_ICE_FAST_RS
