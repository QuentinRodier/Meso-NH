!MNH_LIC Copyright 2013-2018 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!      ###############################
       MODULE MODI_LIMA_CCN_ACTIVATION
!      ###############################
!
INTERFACE
   SUBROUTINE LIMA_CCN_ACTIVATION (PTSTEP, TPFILE, OCLOSE_OUT,                    &
                                   PRHODREF, PEXNREF, PPABST, PT, PDTHRAD, PW_NU, &
                                   PTHT, PRVT, PRCT, PCCT, PRRT, PNFT, PNAT       )
USE MODD_IO_ll, ONLY: TFILEDATA
!
REAL,                     INTENT(IN)    :: PTSTEP     ! Double Time step
                                                      ! (single if cold start)
TYPE(TFILEDATA),          INTENT(IN)    :: TPFILE     ! Output file
LOGICAL,                  INTENT(IN)    :: OCLOSE_OUT ! Conditional closure of 
                                                      ! the output FM file
!
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRHODREF   ! Reference density
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PEXNREF    ! Reference Exner function
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PPABST     ! abs. pressure at time t
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PT         ! Temperature
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PDTHRAD    ! Radiative temperature tendency
!
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PW_NU      ! updraft velocity used for
                                                      ! the nucleation param.
!   
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PTHT       ! Theta at t 
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PRVT       ! Water vapor m.r. at t 
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PRCT       ! Cloud water m.r. at t 
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PCCT       ! Cloud water m.r. at t 
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRRT       ! Cloud water m.r. at t 
REAL, DIMENSION(:,:,:,:), INTENT(INOUT) :: PNFT       ! CCN C. available at t
REAL, DIMENSION(:,:,:,:), INTENT(INOUT) :: PNAT       ! CCN C. activated at t
!
END SUBROUTINE LIMA_CCN_ACTIVATION
END INTERFACE
END MODULE MODI_LIMA_CCN_ACTIVATION
!     #############################################################################
   SUBROUTINE LIMA_CCN_ACTIVATION (PTSTEP, TPFILE, OCLOSE_OUT,                    &
                                   PRHODREF, PEXNREF, PPABST, PT, PDTHRAD, PW_NU, &
                                   PTHT, PRVT, PRCT, PCCT, PRRT, PNFT, PNAT       )
!     #############################################################################
!
!!
!!    PURPOSE
!!    -------
!!      The purpose of this routine is to compute the activation of CCN 
!!    according to Cohard and Pinty, QJRMS, 2000
!!
!!
!!**  METHOD
!!    ------
!!      The activation of CCN is checked for quasi-saturated air parcels 
!!    to update the cloud droplet number concentration.
!!
!!    Computation steps :
!!      1- Check where computations are necessary
!!      2- and 3- Compute the maximum of supersaturation using the iterative 
!!                Ridder algorithm
!!      4- Compute the nucleation source
!!      5- Deallocate local variables
!! 
!!    Contains :
!!      6- Functions : Ridder algorithm
!!
!!
!!    REFERENCE
!!    ---------
!!
!!      Cohard, J.-M. and J.-P. Pinty, 2000: A comprehensive two-moment warm 
!!      microphysical bulk scheme. 
!!        Part I: Description and tests
!!        Part II: 2D experiments with a non-hydrostatic model
!!      Accepted for publication in Quart. J. Roy. Meteor. Soc. 
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
!!
!!      B.Vié 03/03/2020 : use DTHRAD instead of dT/dt in Smax diagnostic computation
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_PARAMETERS,      ONLY : JPHEXT, JPVEXT
USE MODD_CST,             ONLY : XALPW, XBETAW, XCL, XCPD, XCPV, XGAMW, XLVTT, XMD, XMV, XRV, XTT
USE MODD_PARAM_LIMA,      ONLY : LACTIT, NMOD_CCN, XKHEN_MULTI, XCTMIN, XLIMIT_FACTOR
USE MODD_PARAM_LIMA_WARM, ONLY : XWMIN, NAHEN, NHYP, XAHENINTP1, XAHENINTP2, XCSTDCRIT, XHYPF12, &
                                 XHYPINTP1, XHYPINTP2, XTMIN, XHYPF32, XPSI3, XAHENG, XPSI1
!
USE MODI_GAMMA
USE MODI_LIMA_FUNCTIONS,  ONLY : COUNTJV
!
USE MODD_IO_ll,   ONLY: TFILEDATA
USE MODD_LUNIT_n,         ONLY : TLUOUT
USE MODE_FIELD, ONLY : TFIELDDATA, TYPEREAL
USE MODE_FM
USE MODE_FMWRIT
!
IMPLICIT NONE
!
!*       0.1   Declarations of dummy arguments :
!
REAL,                     INTENT(IN)    :: PTSTEP     ! Double Time step
                                                      ! (single if cold start)
TYPE(TFILEDATA),          INTENT(IN)    :: TPFILE     ! Output file
LOGICAL,                  INTENT(IN)    :: OCLOSE_OUT ! Conditional closure of 
                                                      ! the output FM file
!
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRHODREF   ! Reference density
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PEXNREF    ! Reference Exner function
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PPABST     ! abs. pressure at time t
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PT         ! Temperature
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PDTHRAD    ! Radiative temperature tendency
!
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PW_NU      ! updraft velocity used for
                                                      ! the nucleation param.
!   
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PTHT       ! Theta at t 
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PRVT       ! Water vapor m.r. at t 
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PRCT       ! Cloud water m.r. at t 
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PCCT       ! Cloud water m.r. at t 
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRRT       ! Cloud water m.r. at t 
REAL, DIMENSION(:,:,:,:), INTENT(INOUT) :: PNFT       ! CCN C. available at t
REAL, DIMENSION(:,:,:,:), INTENT(INOUT) :: PNAT       ! CCN C. activated at t
!
!
!*       0.1   Declarations of local variables :
!
! Packing variables
LOGICAL, DIMENSION(SIZE(PRHODREF,1),SIZE(PRHODREF,2),SIZE(PRHODREF,3)) :: GNUCT 
INTEGER :: INUCT
INTEGER , DIMENSION(SIZE(GNUCT))   :: I1,I2,I3 ! Used to replace the COUNT
INTEGER                            :: JL       ! and PACK intrinsics 
!
! Packed micophysical variables
REAL, DIMENSION(:,:), ALLOCATABLE  :: ZNFT     ! available nucleus conc. source
REAL, DIMENSION(:,:), ALLOCATABLE  :: ZNAT     ! activated nucleus conc. source
!
! Other packed variables
REAL, DIMENSION(:)  , ALLOCATABLE  :: ZRHODREF ! RHO Dry REFerence
REAL, DIMENSION(:)  , ALLOCATABLE  :: ZEXNREF  ! EXNer Pressure REFerence
REAL, DIMENSION(:)  , ALLOCATABLE  :: ZZT      ! Temperature
!
! Work arrays
REAL, DIMENSION(:), ALLOCATABLE    :: ZZW1, ZZW2, ZZW3, ZZW4, ZZW5, ZZW6, &
                                      ZZTDT,          & ! dT/dt
                                      ZSW,            & ! real supersaturation                                      
                                      ZSMAX,          & ! Maximum supersaturation
                                      ZVEC1
!
REAL, DIMENSION(:,:), ALLOCATABLE  :: ZTMP, ZCHEN_MULTI
!
REAL, DIMENSION(SIZE(PRHODREF,1),SIZE(PRHODREF,2),SIZE(PRHODREF,3))   &
                                   :: ZTDT, ZDRC, ZRVSAT, ZW, ZW2  
REAL, DIMENSION(SIZE(PNFT,1),SIZE(PNFT,2),SIZE(PNFT,3))               &
                                   :: ZCONC_TOT         ! total CCN C. available
!
INTEGER, DIMENSION(:), ALLOCATABLE :: IVEC1             ! Vectors of indices for
                                                        ! interpolations
!
! 
REAL    :: ZEPS                                ! molar mass ratio
REAL    :: ZS1, ZS2, ZXACC 
INTEGER :: JMOD
INTEGER :: IIB, IIE, IJB, IJE, IKB, IKE        ! Physical domain
!
INTEGER                  :: ILUOUT     ! Logical unit of output listing 
TYPE(TFIELDDATA) :: TZFIELD   
!-------------------------------------------------------------------------------
!
ILUOUT = TLUOUT%NLU
ZW(:,:,:)=0.
!
!*       1.     PREPARE COMPUTATIONS - PACK
!   	        ---------------------------
!
IIB=1+JPHEXT
IIE=SIZE(PRHODREF,1) - JPHEXT
IJB=1+JPHEXT
IJE=SIZE(PRHODREF,2) - JPHEXT
IKB=1+JPVEXT
IKE=SIZE(PRHODREF,3) - JPVEXT
!
!  Saturation vapor mixing ratio and radiative tendency                    
!
ZEPS= XMV / XMD
ZRVSAT(:,:,:) = ZEPS / (PPABST(:,:,:)*EXP(-XALPW+XBETAW/PT(:,:,:)+XGAMW*ALOG(PT(:,:,:))) - 1.0)
ZTDT(:,:,:)   = 0.
IF (LACTIT .AND. SIZE(PDTHRAD).GT.0) ZTDT(:,:,:)   = PDTHRAD(:,:,:) * PEXNREF(:,:,:)
!IF (LACTIT) ZTDT(:,:,:)   = (PT(:,:,:)-PTM(:,:,:))/PTSTEP                   ! dT/dt
!
!  find locations where CCN are available
!
ZCONC_TOT(:,:,:) = 0.0
DO JMOD = 1, NMOD_CCN 
   ZCONC_TOT(:,:,:) = ZCONC_TOT(:,:,:) + PNFT(:,:,:,JMOD) ! sum over the free CCN
ENDDO
!
!  optimization by looking for locations where
!  the updraft velocity is positive!!!
!
GNUCT(:,:,:) = .FALSE.
!
! NEW : -22°C = limit sup for condensation freezing in Fridlin et al., 2007
IF( LACTIT ) THEN
   GNUCT(IIB:IIE,IJB:IJE,IKB:IKE) = (PW_NU(IIB:IIE,IJB:IJE,IKB:IKE)>XWMIN  .OR. &
                                     ZTDT(IIB:IIE,IJB:IJE,IKB:IKE)<XTMIN   .OR. &
        PRVT(IIB:IIE,IJB:IJE,IKB:IKE)>ZRVSAT(IIB:IIE,IJB:IJE,IKB:IKE)    ) .AND.&
            PRVT(IIB:IIE,IJB:IJE,IKB:IKE)>(0.98*ZRVSAT(IIB:IIE,IJB:IJE,IKB:IKE))&
             .AND. PT(IIB:IIE,IJB:IJE,IKB:IKE)>(XTT-22.)                        &
             .AND. ZCONC_TOT(IIB:IIE,IJB:IJE,IKB:IKE)>XCTMIN(4)
ELSE 
   GNUCT(IIB:IIE,IJB:IJE,IKB:IKE) =   (PW_NU(IIB:IIE,IJB:IJE,IKB:IKE)>XWMIN .OR. &
        PRVT(IIB:IIE,IJB:IJE,IKB:IKE)>ZRVSAT(IIB:IIE,IJB:IJE,IKB:IKE)    ) .AND.&
            PRVT(IIB:IIE,IJB:IJE,IKB:IKE)>(0.98*ZRVSAT(IIB:IIE,IJB:IJE,IKB:IKE))&
             .AND. PT(IIB:IIE,IJB:IJE,IKB:IKE)>(XTT-22.)                        &
             .AND. ZCONC_TOT(IIB:IIE,IJB:IJE,IKB:IKE)>XCTMIN(4)
END IF
INUCT = COUNTJV( GNUCT(:,:,:),I1(:),I2(:),I3(:))
!
IF( INUCT >= 1 ) THEN
!
   ALLOCATE(ZNFT(INUCT,NMOD_CCN))
   ALLOCATE(ZNAT(INUCT,NMOD_CCN))
   ALLOCATE(ZTMP(INUCT,NMOD_CCN))
   ALLOCATE(ZZT(INUCT)) 
   ALLOCATE(ZZTDT(INUCT)) 
   ALLOCATE(ZSW(INUCT))    
   ALLOCATE(ZZW1(INUCT))
   ALLOCATE(ZZW2(INUCT))
   ALLOCATE(ZZW3(INUCT))
   ALLOCATE(ZZW4(INUCT))
   ALLOCATE(ZZW5(INUCT))
   ALLOCATE(ZZW6(INUCT))
   ALLOCATE(ZCHEN_MULTI(INUCT,NMOD_CCN))
   ALLOCATE(ZVEC1(INUCT))
   ALLOCATE(IVEC1(INUCT))
   ALLOCATE(ZRHODREF(INUCT)) 
   ALLOCATE(ZEXNREF(INUCT)) 
   DO JL=1,INUCT
      ZZT(JL)  = PT(I1(JL),I2(JL),I3(JL))
      ZZW1(JL) = ZRVSAT(I1(JL),I2(JL),I3(JL))
      ZZW2(JL) = PW_NU(I1(JL),I2(JL),I3(JL))
      ZZTDT(JL)  = ZTDT(I1(JL),I2(JL),I3(JL))
      ZSW(JL)  = PRVT(I1(JL),I2(JL),I3(JL))/ZRVSAT(I1(JL),I2(JL),I3(JL)) - 1.
      ZRHODREF(JL) = PRHODREF(I1(JL),I2(JL),I3(JL))
      ZEXNREF(JL)  = PEXNREF(I1(JL),I2(JL),I3(JL))
      DO JMOD = 1,NMOD_CCN
         ZNFT(JL,JMOD)        = PNFT(I1(JL),I2(JL),I3(JL),JMOD)
         ZNAT(JL,JMOD)        = PNAT(I1(JL),I2(JL),I3(JL),JMOD)
         ZCHEN_MULTI(JL,JMOD) = (ZNFT(JL,JMOD)+ZNAT(JL,JMOD))*ZRHODREF(JL) &
                                                             / XLIMIT_FACTOR(JMOD)
      ENDDO
   ENDDO
!
   ZZW1(:) = 1.0/ZEPS + 1.0/ZZW1(:)                                   &
             + (((XLVTT+(XCPV-XCL)*(ZZT(:)-XTT))/ZZT(:))**2)/(XCPD*XRV) ! Psi2
!
!
!-------------------------------------------------------------------------------
!
!
!*       2. compute the constant term (ZZW3) relative to smax    
!   	 ----------------------------------------------------
!
!  Remark : in LIMA's nucleation parameterization, Smax=0.01 for a supersaturation of 1% !
!
!
   ZVEC1(:) = MAX( 1.0001, MIN( FLOAT(NAHEN)-0.0001, XAHENINTP1 * ZZT(:) + XAHENINTP2 ) )
   IVEC1(:) = INT( ZVEC1(:) )
   ZVEC1(:) = ZVEC1(:) - FLOAT( IVEC1(:) )
   ALLOCATE(ZSMAX(INUCT))
!
!
   IF (LACTIT) THEN ! including a cooling rate
!
!       Compute the tabulation of function of ZZW3 :
!
!                                                  (Psi1*w+Psi3*DT/Dt)**1.5 
!       ZZW3 = XAHENG*(Psi1*w + Psi3*DT/Dt)**1.5 = ------------------------ 
!                                                   2*pi*rho_l*G**(3/2)     
!
!
        ZZW4(:)=XPSI1( IVEC1(:)+1)*ZZW2(:)+XPSI3(IVEC1(:)+1)*ZZTDT(:)
        ZZW5(:)=XPSI1( IVEC1(:)  )*ZZW2(:)+XPSI3(IVEC1(:)  )*ZZTDT(:)
        WHERE (ZZW4(:) < 0. .OR. ZZW5(:) < 0.)
           ZZW4(:) = 0.
           ZZW5(:) = 0.
        END WHERE
        ZZW3(:) =   XAHENG( IVEC1(:)+1)*(ZZW4(:)**1.5)* ZVEC1(:)      &
                  - XAHENG( IVEC1(:)  )*(ZZW5(:)**1.5)*(ZVEC1(:) - 1.0)
                       ! Cste*((Psi1*w+Psi3*dT/dt)/(G))**1.5
!
!
   ELSE ! LACTIT , for clouds
!
!
!       Compute the tabulation of function of ZZW3 :
!
!                                             (Psi1 * w)**1.5       
!       ZZW3 = XAHENG * (Psi1 * w)**1.5  = -------------------------
!                                            2 pi rho_l * G**(3/2)  
!
!
        ZZW2(:)=MAX(ZZW2(:),0.)
        ZZW3(:)=XAHENG(IVEC1(:)+1)*((XPSI1(IVEC1(:)+1)*ZZW2(:))**1.5)* ZVEC1(:)    &
               -XAHENG(IVEC1(:)  )*((XPSI1(IVEC1(:)  )*ZZW2(:))**1.5)*(ZVEC1(:)-1.0)
!
   END IF ! LACTIT
!
!
!              (Psi1*w+Psi3*DT/Dt)**1.5   rho_air
!       ZZW3 = ------------------------ * -------
!                 2*pi*rho_l*G**(3/2)       Psi2
!
   ZZW5(:) = 1.
   ZZW3(:) = (ZZW3(:)/ZZW1(:))*ZRHODREF(:) ! R.H.S. of Eq 9 of CPB 98 but
                                           ! for multiple aerosol modes
   WHERE (ZZW3(:) == 0. .AND. .NOT.(ZSW>0.))
      ZZW5(:) = -1.
   END WHERE
!
!
!-------------------------------------------------------------------------------
!
!
!*       3. Compute the maximum of supersaturation
!   	 -----------------------------------------
!
!
! estimate S_max for the CPB98 parameterization with SEVERAL aerosols mode
! Reminder : Smax=0.01 for a 1% supersaturation
!
! Interval bounds to tabulate sursaturation Smax
! Check with values used for tabulation in ini_lima_warm.f90
   ZS1 = 1.0E-5                   ! corresponds to  0.001% supersaturation
   ZS2 = 5.0E-2                   ! corresponds to 5.0% supersaturation 
   ZXACC = 1.0E-7                 ! Accuracy needed for the search in [NO UNITS]
!
   ZSMAX(:) = ZRIDDR(ZS1,ZS2,ZXACC,ZZW3(:),INUCT)    ! ZSMAX(:) is in [NO UNITS]
   ZSMAX(:) = MIN(MAX(ZSMAX(:), ZSW(:)),ZS2)
!
!
!-------------------------------------------------------------------------------
!
!
!*       4. Compute the nucleus source
!   	 -----------------------------
!
!
! Again : Smax=0.01 for a 1% supersaturation
! Modified values for Beta and C (see in init_aerosol_properties) account for that
!
   WHERE (ZZW5(:) > 0. .AND. ZSMAX(:) > 0.)
      ZVEC1(:) = MAX( 1.0001, MIN( FLOAT(NHYP)-0.0001, XHYPINTP1*LOG(ZSMAX(:))+XHYPINTP2 ) )
      IVEC1(:) = INT( ZVEC1(:) )
      ZVEC1(:) = ZVEC1(:) - FLOAT( IVEC1(:) )
   END WHERE
   ZZW6(:)  = 0. ! initialize the change of cloud droplet concentration
!
   ZTMP(:,:)=0.0
!
! Compute the concentration of activable aerosols for each mode
! based on the max of supersaturation ( -> ZTMP )
!
   DO JMOD = 1, NMOD_CCN                     ! iteration on mode number
      ZZW1(:) = 0.
      ZZW2(:) = 0.
      ZZW3(:) = 0.
   !
      WHERE( ZZW5(:) > 0. .AND. ZSMAX(:)>0.0 )
         ZZW2(:) =  XHYPF12( IVEC1(:)+1,JMOD )* ZVEC1(:)      & ! hypergeo function
                  - XHYPF12( IVEC1(:)  ,JMOD )*(ZVEC1(:) - 1.0) ! XHYPF12 is tabulated
   !
         ZTMP(:,JMOD) = ZCHEN_MULTI(:,JMOD)/ZRHODREF(:)*ZSMAX(:)**XKHEN_MULTI(JMOD)*ZZW2(:)
      ENDWHERE
   ENDDO
!
! Compute the concentration of aerosols activated at this time step
! as the difference between ZTMP and the aerosols already activated at t-dt (ZZW1)
!
   DO JMOD = 1, NMOD_CCN                     ! iteration on mode number
      ZZW1(:) = 0.
      ZZW2(:) = 0.
      ZZW3(:) = 0.
   !
      WHERE( SUM(ZTMP(:,:),DIM=2) .GT. 15.E6/ZRHODREF(:) ) 
         ZZW1(:) = MIN( ZNFT(:,JMOD),MAX( ZTMP(:,JMOD)- ZNAT(:,JMOD) , 0.0 ) )
      ENDWHERE
   !
   !* update the concentration of activated CCN = Na
   !
      PNAT(:,:,:,JMOD) = PNAT(:,:,:,JMOD) + UNPACK( ZZW1(:), MASK=GNUCT(:,:,:), FIELD=0.0 )
   !
   !* update the concentration of free CCN = Nf
   !
      PNFT(:,:,:,JMOD) = PNFT(:,:,:,JMOD) - UNPACK( ZZW1(:), MASK=GNUCT(:,:,:), FIELD=0.0 )
   !
   !* prepare to update the cloud water concentration 
   !
      ZZW6(:) = ZZW6(:) + ZZW1(:)
   ENDDO
!
! Output tendencies
!
   ZZW1(:)=0.
   WHERE (ZZW5(:)>0.0 .AND. ZSMAX(:)>0.0) ! ZZW1 is computed with ZSMAX [NO UNIT]
      ZZW1(:) = MIN(XCSTDCRIT*ZZW6(:)/(((ZZT(:)*ZSMAX(:))**3)*ZRHODREF(:)),1.E-5)
   END WHERE
   ZW(:,:,:) = MIN( UNPACK( ZZW1(:),MASK=GNUCT(:,:,:),FIELD=0.0 ),PRVT(:,:,:) )
!
   PTHT(:,:,:) = PTHT(:,:,:) + ZW(:,:,:) * (XLVTT+(XCPV-XCL)*(PT(:,:,:)-XTT))/                &
            (PEXNREF(:,:,:)*(XCPD+XCPV*PRVT(:,:,:)+XCL*(PRCT(:,:,:)+PRRT(:,:,:))))
   PRVT(:,:,:) = PRVT(:,:,:) - ZW(:,:,:) 
   PRCT(:,:,:) = PRCT(:,:,:) + ZW(:,:,:) 
   PCCT(:,:,:) = PCCT(:,:,:) + UNPACK( ZZW6(:),MASK=GNUCT(:,:,:),FIELD=0. ) 
!
   ZW(:,:,:)   = UNPACK( 100.0*ZSMAX(:),MASK=GNUCT(:,:,:),FIELD=0.0 )
   ZW2(:,:,:)  = UNPACK( ZZW6(:),MASK=GNUCT(:,:,:),FIELD=0.0 )
!
!
!-------------------------------------------------------------------------------
!
!
!*       5. Cleaning
!   	 -----------
!
!
   DEALLOCATE(IVEC1)
   DEALLOCATE(ZVEC1)
   DEALLOCATE(ZNFT)
   DEALLOCATE(ZNAT)
   DEALLOCATE(ZZT)
   DEALLOCATE(ZSMAX)
   DEALLOCATE(ZZW1)
   DEALLOCATE(ZZW2)
   DEALLOCATE(ZZW3)
   DEALLOCATE(ZZW4)
   DEALLOCATE(ZZW5)
   DEALLOCATE(ZZW6)
   DEALLOCATE(ZZTDT)
   DEALLOCATE(ZSW)
   DEALLOCATE(ZRHODREF)
   DEALLOCATE(ZCHEN_MULTI)
   DEALLOCATE(ZEXNREF)
!
END IF ! INUCT
!
IF ( OCLOSE_OUT ) THEN
  TZFIELD%CMNHNAME   ='SMAX'
  TZFIELD%CSTDNAME   = ''
  TZFIELD%CLONGNAME  = TRIM(TZFIELD%CMNHNAME)
  TZFIELD%CUNITS     = ''
  TZFIELD%CDIR       = 'XY'
  TZFIELD%CCOMMENT   = 'X_Y_Z_SMAX'
  TZFIELD%NGRID      = 1
  TZFIELD%NTYPE      = TYPEREAL
  TZFIELD%NDIMS      = 3
  TZFIELD%LTIMEDEP   = .TRUE.
  CALL IO_WRITE_FIELD(TPFILE,TZFIELD,ZW)
  !
  TZFIELD%CMNHNAME   ='NACT'
  TZFIELD%CSTDNAME   = ''
  TZFIELD%CLONGNAME  = TRIM(TZFIELD%CMNHNAME)
  TZFIELD%CUNITS     = 'kg-1'
  TZFIELD%CDIR       = 'XY'
  TZFIELD%CCOMMENT   = 'X_Y_Z_NACT'
  TZFIELD%NGRID      = 1
  TZFIELD%NTYPE      = TYPEREAL
  TZFIELD%NDIMS      = 3
  TZFIELD%LTIMEDEP   = .TRUE.
  CALL IO_WRITE_FIELD(TPFILE,TZFIELD,ZW2)
END IF
!
!
!-------------------------------------------------------------------------------
!
!
!*       6. Functions used to compute the maximum of supersaturation
!   	 -----------------------------------------------------------
!
!
CONTAINS
!------------------------------------------------------------------------------
!
  FUNCTION ZRIDDR(PX1,PX2INIT,PXACC,PZZW3,NPTS)  RESULT(PZRIDDR)
!
!
!!****  *ZRIDDR* - iterative algorithm to find root of a function
!!
!!
!!    PURPOSE
!!    -------
!!       The purpose of this function is to find the root of a given function
!!     the arguments are the brackets bounds (the interval where to find the root)
!!     the accuracy needed and the input parameters of the given function.
!!     Using Ridders' method, return the root of a function known to lie between 
!!     PX1 and PX2. The root, returned as PZRIDDR, will be refined to an approximate
!!     accuracy PXACC.
!! 
!!**  METHOD
!!    ------
!!       Ridders' method
!!
!!    EXTERNAL
!!    --------
!!       FUNCSMAX  
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!      NUMERICAL RECIPES IN FORTRAN 77: THE ART OF SCIENTIFIC COMPUTING 
!!     (ISBN 0-521-43064-X)
!!      Copyright (C) 1986-1992 by Cambridge University Press.
!!      Programs Copyright (C) 1986-1992 by Numerical Recipes Software.
!!
!!    AUTHOR
!!    ------
!!      Frederick Chosson *CERFACS*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original     12/07/07
!!      S.BERTHET        2008 vectorization 
!------------------------------------------------------------------------------
!
!*       0. DECLARATIONS
!
!
IMPLICIT NONE
!
!*       0.1 declarations of arguments and result
!
INTEGER,            INTENT(IN)     :: NPTS
REAL, DIMENSION(:), INTENT(IN)     :: PZZW3
REAL,               INTENT(IN)     :: PX1, PX2INIT, PXACC
REAL, DIMENSION(:), ALLOCATABLE    :: PZRIDDR
!
!*       0.2 declarations of local variables
!
!
INTEGER, PARAMETER                 :: MAXIT=60
REAL,    PARAMETER                 :: UNUSED=0.0 !-1.11e30
REAL,    DIMENSION(:), ALLOCATABLE :: fh,fl, fm,fnew
REAL                               :: s,xh,xl,xm,xnew
REAL                               :: PX2
INTEGER                            :: j, JL
!
ALLOCATE(  fh(NPTS))
ALLOCATE(  fl(NPTS))
ALLOCATE(  fm(NPTS))
ALLOCATE(fnew(NPTS))
ALLOCATE(PZRIDDR(NPTS))
!
PZRIDDR(:)= UNUSED
PX2       = PX2INIT 
fl(:)     = FUNCSMAX(PX1,PZZW3(:),NPTS)
fh(:)     = FUNCSMAX(PX2,PZZW3(:),NPTS)
!
DO JL = 1, NPTS
   PX2 = PX2INIT
100 if ((fl(JL) > 0.0 .and. fh(JL) < 0.0) .or. (fl(JL) < 0.0 .and. fh(JL) > 0.0)) then
      xl         = PX1
      xh         = PX2
      do j=1,MAXIT
         xm     = 0.5*(xl+xh)
         fm(JL) = SINGL_FUNCSMAX(xm,PZZW3(JL),JL)
         s      = sqrt(fm(JL)**2-fl(JL)*fh(JL))
         if (s == 0.0) then
            GO TO 101
         endif
         xnew  = xm+(xm-xl)*(sign(1.0,fl(JL)-fh(JL))*fm(JL)/s)
         if (abs(xnew - PZRIDDR(JL)) <= PXACC) then
            GO TO 101 
         endif
         PZRIDDR(JL) = xnew
         fnew(JL)  = SINGL_FUNCSMAX(PZRIDDR(JL),PZZW3(JL),JL)
         if (fnew(JL) == 0.0) then
            GO TO 101
         endif
         if (sign(fm(JL),fnew(JL)) /= fm(JL)) then
            xl    =xm
            fl(JL)=fm(JL)
            xh    =PZRIDDR(JL)
            fh(JL)=fnew(JL)
         else if (sign(fl(JL),fnew(JL)) /= fl(JL)) then
            xh    =PZRIDDR(JL)
            fh(JL)=fnew(JL)
         else if (sign(fh(JL),fnew(JL)) /= fh(JL)) then
            xl    =PZRIDDR(JL)
            fl(JL)=fnew(JL)
         else if (PX2 .lt. 0.05) then
            PX2 = PX2 + 1.0E-2
            PRINT*, 'PX2 ALWAYS too small, we put a greater one : PX2 =',PX2
            fh(JL)   = SINGL_FUNCSMAX(PX2,PZZW3(JL),JL)
            go to 100
            print*, 'PZRIDDR: never get here'
            STOP
         end if
         if (abs(xh-xl) <= PXACC) then
            GO TO 101 
         endif
!!SB
!!$      if (j == MAXIT .and. (abs(xh-xl) > PXACC) ) then
!!$        PZRIDDR(JL)=0.0
!!$        go to 101
!!$      endif   
!!SB
      end do
      print*, 'PZRIDDR: exceeded maximum iterations',j
      STOP
   else if (fl(JL) == 0.0) then
      PZRIDDR(JL)=PX1
   else if (fh(JL) == 0.0) then
      PZRIDDR(JL)=PX2
   else if (PX2 .lt. 0.05) then
      PX2 = PX2 + 1.0E-2
      PRINT*, 'PX2 too small, we put a greater one : PX2 =',PX2
      fh(JL)   = SINGL_FUNCSMAX(PX2,PZZW3(JL),JL)
      go to 100
   else
!!$      print*, 'PZRIDDR: root must be bracketed'
!!$      print*,'npts ',NPTS,'jl',JL
!!$      print*, 'PX1,PX2,fl,fh',PX1,PX2,fl(JL),fh(JL)
!!$      print*, 'PX2 = 30 % of supersaturation, there is no solution for Smax'
!!$      print*, 'try to put greater PX2 (upper bound for Smax research)'
!!$      STOP
      PZRIDDR(JL)=0.0
      go to 101
   end if
101 ENDDO
!
DEALLOCATE(  fh)
DEALLOCATE(  fl)
DEALLOCATE(  fm)
DEALLOCATE(fnew)
!
END FUNCTION ZRIDDR
!
!------------------------------------------------------------------------------
!
  FUNCTION FUNCSMAX(PPZSMAX,PPZZW3,NPTS)  RESULT(PFUNCSMAX)
!
!
!!****  *FUNCSMAX* - function describing SMAX function that you want to find the root
!!
!!
!!    PURPOSE
!!    -------
!!       This function describe the equilibrium between Smax and two aerosol mode
!!     acting as CCN. This function is derive from eq. (9) of CPB98 but for two
!!     aerosols mode described by their respective parameters C, k, Mu, Beta.
!!     the arguments are the supersaturation in "no unit" and the r.h.s. of this eq.
!!     and the ratio of concentration of injected aerosols on maximum concentration
!!     of injected aerosols ever.
!!**  METHOD
!!    ------
!!       This function is called by zriddr.f90
!!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!    Module MODD_PARAM_LIMA_WARM
!!        XHYPF32
!!
!!        XHYPINTP1
!!        XHYPINTP2
!!
!!    Module MODD_PARAM_C2R2
!!        XKHEN_MULTI()
!!        NMOD_CCN
!!       
!!    REFERENCE
!!    ---------
!!    Cohard, J.M., J.P.Pinty, K.Suhre, 2000:"On the parameterization of activation
!!             spectra from cloud condensation nuclei microphysical properties",
!!             J. Geophys. Res., Vol.105, N0.D9, pp. 11753-11766
!!
!!    AUTHOR
!!    ------
!!      Frederick Chosson *CERFACS*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original     12/07/07
!!      S.Berthet    19/03/08 Extension a une population multimodale d aerosols
!
!------------------------------------------------------------------------------
!
!*       0. DECLARATIONS
!
IMPLICIT NONE
!
!*       0.1 declarations of arguments and result
!
INTEGER,            INTENT(IN)  :: NPTS
REAL,               INTENT(IN)  :: PPZSMAX   ! supersaturation is already in no units
REAL, DIMENSION(:), INTENT(IN)  :: PPZZW3    ! 
REAL, DIMENSION(:), ALLOCATABLE :: PFUNCSMAX ! 
!
!*       0.2 declarations of local variables
!
REAL                           :: ZHYPF
!
REAL                           :: PZVEC1
INTEGER                        :: PIVEC1
!
ALLOCATE(PFUNCSMAX(NPTS))
!
PFUNCSMAX(:) = 0.
PZVEC1 = MAX( 1.0001,MIN( FLOAT(NHYP)-0.0001,               &
                           XHYPINTP1*LOG(PPZSMAX)+XHYPINTP2 ) )
PIVEC1 = INT( PZVEC1 )
PZVEC1 = PZVEC1 - FLOAT( PIVEC1 )
DO JMOD = 1, NMOD_CCN
   ZHYPF        = 0.          ! XHYPF32 is tabulated with ZSMAX in [NO UNITS]
   ZHYPF        =   XHYPF32( PIVEC1+1,JMOD ) * PZVEC1              &
                  - XHYPF32( PIVEC1  ,JMOD ) *(PZVEC1 - 1.0)
                             ! sum of s**(ki+2) * F32 * Ci * ki * beta(ki/2,3/2)
   PFUNCSMAX(:) =  PFUNCSMAX(:) + (PPZSMAX)**(XKHEN_MULTI(JMOD) + 2) &
                 * ZHYPF* XKHEN_MULTI(JMOD) * ZCHEN_MULTI(:,JMOD)    &
                 * GAMMA_X0D( XKHEN_MULTI(JMOD)/2.0)*GAMMA_X0D(3.0/2.0)      &
                 / GAMMA_X0D((XKHEN_MULTI(JMOD)+3.0)/2.0)
ENDDO
! function l.h.s. minus r.h.s. of eq. (9) of CPB98 but for NMOD_CCN aerosol mode
PFUNCSMAX(:) = PFUNCSMAX(:) - PPZZW3(:)
!
END FUNCTION FUNCSMAX
!
!------------------------------------------------------------------------------
!
  FUNCTION SINGL_FUNCSMAX(PPZSMAX,PPZZW3,KINDEX)  RESULT(PSINGL_FUNCSMAX)
!
!
!!****  *SINGL_FUNCSMAX* - same function as FUNCSMAX
!!
!!
!!    PURPOSE
!!    -------
!        As for FUNCSMAX but for a scalar
!!
!!**  METHOD
!!    ------
!!       This function is called by zriddr.f90
!!
!------------------------------------------------------------------------------
!
!*       0. DECLARATIONS
!
IMPLICIT NONE
!
!*       0.1 declarations of arguments and result
!
INTEGER,            INTENT(IN)  :: KINDEX
REAL,               INTENT(IN)  :: PPZSMAX   ! supersaturation is "no unit"
REAL,               INTENT(IN)  :: PPZZW3    ! 
REAL                            :: PSINGL_FUNCSMAX ! 
!
!*       0.2 declarations of local variables
!
REAL                           :: ZHYPF
!
REAL                           :: PZVEC1
INTEGER                        :: PIVEC1
!
PSINGL_FUNCSMAX = 0.
PZVEC1    = MAX( 1.0001,MIN( FLOAT(NHYP)-0.0001,               &
                              XHYPINTP1*LOG(PPZSMAX)+XHYPINTP2 ) )
PIVEC1 = INT( PZVEC1 )
PZVEC1 = PZVEC1 - FLOAT( PIVEC1 )
DO JMOD = 1, NMOD_CCN
   ZHYPF        = 0.          ! XHYPF32 is tabulated with ZSMAX in [NO UNITS]
   ZHYPF        =   XHYPF32( PIVEC1+1,JMOD ) * PZVEC1              &
                  - XHYPF32( PIVEC1  ,JMOD ) *(PZVEC1 - 1.0)
                             ! sum of s**(ki+2) * F32 * Ci * ki * bêta(ki/2,3/2)
   PSINGL_FUNCSMAX = PSINGL_FUNCSMAX + (PPZSMAX)**(XKHEN_MULTI(JMOD) + 2)   &
                   * ZHYPF* XKHEN_MULTI(JMOD) * ZCHEN_MULTI(KINDEX,JMOD) &
                   * GAMMA_X0D( XKHEN_MULTI(JMOD)/2.0)*GAMMA_X0D(3.0/2.0)        &
                   / GAMMA_X0D((XKHEN_MULTI(JMOD)+3.0)/2.0)
ENDDO
! function l.h.s. minus r.h.s. of eq. (9) of CPB98 but for NMOD_CCN aerosol mode
PSINGL_FUNCSMAX = PSINGL_FUNCSMAX - PPZZW3
!
END FUNCTION SINGL_FUNCSMAX
!
!-----------------------------------------------------------------------------
!
END SUBROUTINE LIMA_CCN_ACTIVATION
