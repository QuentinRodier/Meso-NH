!MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! masdev4_8 2008/06/30 13:57:54
!-----------------------------------------------------------------
!      ######################
       MODULE MODI_RAIN_KHKO
!      ######################
!
INTERFACE
      SUBROUTINE RAIN_KHKO (OACTIT, OSEDC, ORAIN, KSPLITR, PTSTEP,             & 
                            KMI, PZZ, PRHODJ, PRHODREF, PEXNREF,               &
                            PPABST, PTHT, PRVT, PRCT, PRRT,                    &
                            PTHM, PRCM,  PPABSM,                               &
                            PW_NU, PTHS, PRVS, PRCS, PRRS,                     &
                            PCNT, PCCT, PCRT, PCNS, PCCS, PCRS, PINPRC,PINPRR, &
                            PINPRR3D, PEVAP3D, PAEROT,                         &
                            PSOLORG, PMI, HACTCCN                              )
!
!
!
LOGICAL,                  INTENT(IN)    :: OACTIT ! Switch to activate the
                                                  ! activation by radiative
                                                  ! tendency
LOGICAL,                  INTENT(IN)    :: OSEDC   ! switch to activate the 
                                                   ! cloud droplet sedimentation
LOGICAL,                  INTENT(IN)    :: ORAIN   ! switch to activate the 
                                                   ! rain formation by coalescence
INTEGER,                  INTENT(IN)    :: KSPLITR ! Number of small time step 
                                      ! integration for  rain sedimendation
REAL,                     INTENT(IN)    :: PTSTEP ! Time step :XTSTEP in namelist
INTEGER,                  INTENT(IN)    :: KMI     ! Model index 
!
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PZZ     ! Height (z)
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRHODJ  ! Dry density * Jacobian
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRHODREF! Reference density
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PEXNREF ! Reference Exner function
!
!
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PPABST  ! abs. pressure at time t
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PTHT    ! Theta at time t
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRVT    ! Water vapor m.r. at t 
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRCT    ! Cloud water m.r. at t 
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRRT    ! Rain water m.r. at t 
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PTHM    ! Theta at time t-Dt
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PPABSM  ! Pressure time t-Dt
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRCM    ! Cloud water m.r. at time t-Dt
!
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PW_NU   ! updraft velocity used for
                                                   ! the nucleation param.
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PTHS    ! Theta source
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PRVS    ! Water vapor m.r. source
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PRCS    ! Cloud water m.r. source
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PRRS    ! Rain water m.r. source
!
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PCNT    ! Water vapor C. at t
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PCCT    ! Cloud water C. at t
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PCRT    ! Rain water C. at t
!
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PCNS    ! Water vapor C. source
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PCCS    ! Cloud water C. source
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PCRS    ! Rain water C. source
!
REAL, DIMENSION(:,:),     INTENT(INOUT) :: PINPRC  ! Cloud instant precip
REAL, DIMENSION(:,:),     INTENT(INOUT) :: PINPRR  ! Rain instant precip
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PINPRR3D! Rain inst precip 3D
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PEVAP3D! Rain evap profile
REAL, DIMENSION(:,:,:,:), INTENT(INOUT) :: PAEROT  ! Aerosol concentration
REAL, DIMENSION(:,:,:,:), INTENT(IN)    :: PSOLORG ![%] solubility fraction of soa
REAL, DIMENSION(:,:,:,:), INTENT(IN)    :: PMI
CHARACTER(LEN=4),         INTENT(IN)    :: HACTCCN  ! kind of CCN activation scheme
!
END SUBROUTINE RAIN_KHKO
END INTERFACE
END MODULE MODI_RAIN_KHKO
!     ######################################################################
      SUBROUTINE RAIN_KHKO (OACTIT, OSEDC, ORAIN, KSPLITR, PTSTEP,             & 
                            KMI, PZZ, PRHODJ, PRHODREF, PEXNREF,               &
                            PPABST, PTHT, PRVT, PRCT, PRRT,                    &
                            PTHM, PRCM,  PPABSM,                               &
                            PW_NU, PTHS, PRVS, PRCS, PRRS,                     &
                            PCNT, PCCT, PCRT, PCNS, PCCS, PCRS, PINPRC,PINPRR, &
                            PINPRR3D, PEVAP3D, PAEROT,                         &
                            PSOLORG, PMI, HACTCCN                              )
!     ######################################################################
!
!!****  * -  compute the explicit microphysical sources of cloud water and
!!           rain water concentrations and mixing ratios
!!
!!    PURPOSE
!!    -------
!!      The purpose of this routine is to compute the microphysical sources:
!!    drizzle drops sedimentation, autoconversion, accretion and vaporisation   
!!    which are parameterized according to Khairoutdinov and Kogan 2000, 
!!    nucleation and cloud droplets sedimentation which are parameterized  
!!    according to Cohard and Pinty QJRMS, 2000
!!

!!
!!
!!**  METHOD
!!    ------
!!      The activation of CCN is checked for quasi-saturated air parcels 
!!    to update the cloud droplet number concentration. 
!!
!!
!!     ...
!!
!!    EXTERNAL
!!    --------
!!      None
!!     
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!      Module MODD_PARAMETERS
!!          JPHEXT       : Horizontal external points number
!!          JPVEXT       : Vertical external points number
!!      Module MODD_CONF :
!!          CCONF configuration of the model for the first time step
!!
!!      Module MODD_CST     
!!          XP00               ! Reference pressure
!!          XRD,XRV            ! Gaz  constant for dry air, vapor
!!          XMD,XMV            ! Molecular weight for dry air, vapor
!!          XCPD               ! Cpd (dry air)
!!          XCL                ! Cl (liquid)
!!          XTT                ! Triple point temperature
!!          XLVTT              ! Vaporization heat constant
!!          XALPW,XBETAW,XGAMW ! Constants for saturation vapor pressure
!!                             ! function over liquid water
!!      Module MODD_BUDGET:
!!         NBUMOD       : model in which budget is calculated
!!         CBUTYPE      : type of desired budget
!!                          'CART' for cartesian box configuration
!!                          'MASK' for budget zone defined by a mask 
!!                          'NONE'  ' for no budget
!!         NBUPROCCTR   : process counter used for each budget variable
!!         LBU_RTH      : logical for budget of RTH (potential temperature)
!!                        .TRUE. = budget of RTH        
!!                        .FALSE. = no budget of RTH
!!         LBU_RRV      : logical for budget of RRV (water vapor)
!!                        .TRUE. = budget of RRV 
!!                        .FALSE. = no budget of RRV 
!!         LBU_RRC      : logical for budget of RRC (cloud water)
!!                        .TRUE. = budget of RRC 
!!                        .FALSE. = no budget of RRC 
!!         LBU_RRR      : logical for budget of RRR (rain water)
!!                        .TRUE. = budget of RRR 
!!                        .FALSE. = no budget of RRR 
!!
!!    REFERENCE
!!    ---------
!!
!!      M. Khairoutdinov and Y. Kogan,"A new Cloud Physics Parametererization
!!      in a Large-Eddy Simulation Model of Marine Stratocumulus"
!!      Mon. Weather Rev.,128, 229-243-2000
!!
!!    AUTHOR
!!    ------
!!
!!      O. Geoffroy  * CNRM Meteo-France* : 07/2006
!!
!!    MODIFICATIONS
!!    -------------
!!
!!      C.Lac                11/09     Distinction of the TSTEPs
!!      C.Lac, V.Masson      09/10     Corrections in sedimentation and
!!                                     evaporation for reproducibility
!!      O.Thouron            03/13     Add prognostic supersaturation +
!!                                     corrections in the sedimentation
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_PARAMETERS
USE MODD_CST
USE MODD_CONF
USE MODD_PARAM_C2R2
USE MODD_RAIN_C2R2_DESCR
USE MODD_RAIN_C2R2_PARAM
USE MODD_RAIN_KHKO_PARAM
USE MODD_BUDGET
USE MODD_NSV, ONLY : NSV_C2R2BEG
USE MODD_CH_AEROSOL
USE MODD_DUST
USE MODD_SALT
!
USE MODI_BUDGET
!
USE MODI_GAMMA
IMPLICIT NONE
!
!*       0.1   Declarations of dummy arguments :
!
!
!
!
LOGICAL,                  INTENT(IN)    :: OACTIT ! Switch to activate the
                                                  ! activation by radiative
                                                  ! tendency
LOGICAL,                  INTENT(IN)    :: OSEDC   ! switch to activate the 
                                                   ! cloud droplet sedimentation
LOGICAL,                  INTENT(IN)    :: ORAIN   ! switch to activate the 
                                                   ! rain formation by coalescence
INTEGER,                  INTENT(IN)    :: KSPLITR ! Number of small time step 
                                      ! integration for  rain sedimendation
REAL,                     INTENT(IN)    :: PTSTEP ! Time step :XTSTEP in namelist
INTEGER,                  INTENT(IN)    :: KMI     ! Model index 
!
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PZZ     ! Height (z)
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRHODJ  ! Dry density * Jacobian
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRHODREF! Reference density
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PEXNREF ! Reference Exner function
!
!
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PPABST  ! abs. pressure at time t
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PTHT    ! Theta at time t
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRVT    ! Water vapor m.r. at t
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRCT    ! Cloud water m.r. at t
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRRT    ! Rain water m.r. at t
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PTHM    ! Theta at time t-Dt
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PPABSM  ! Pressure time t-Dt
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PRCM    ! Cloud water m.r. at time t-Dt
!
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PW_NU   ! updraft velocity used for
                                                   ! the nucleation param.
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PTHS    ! Theta source
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PRVS    ! Water vapor m.r. source
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PRCS    ! Cloud water m.r. source
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PRRS    ! Rain water m.r. source
!
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PCNT    ! Water vapor C. at t
REAL, DIMENSION(:,:,:),   INTENT(IN)    :: PCCT    ! Cloud water C. at t
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PCRT    ! Rain water C. at t
!
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PCNS    ! Water vapor C. source
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PCCS    ! Cloud water C. source
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PCRS    ! Rain water C. source
!
REAL, DIMENSION(:,:),     INTENT(INOUT) :: PINPRC  ! Cloud instant precip
REAL, DIMENSION(:,:),     INTENT(INOUT) :: PINPRR  ! Rain instant precip
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PINPRR3D! Rain inst precip 3D
REAL, DIMENSION(:,:,:),   INTENT(INOUT) :: PEVAP3D! Rain evap profile
REAL, DIMENSION(:,:,:,:), INTENT(INOUT) :: PAEROT  ! Aerosol concentration
REAL, DIMENSION(:,:,:,:), INTENT(IN)    :: PSOLORG ![%] solubility fraction of soa
REAL, DIMENSION(:,:,:,:), INTENT(IN)    :: PMI
CHARACTER(LEN=4),         INTENT(IN)    :: HACTCCN  ! kind of CCN activation scheme
!
!*       0.2   Declarations of local variables :
!
INTEGER :: JK            ! Vertical loop index for the rain sedimentation 
INTEGER :: JN            ! Temporal loop index for the rain sedimentation
INTEGER :: IIB           !  Define the domain where is 
INTEGER :: IIE           !  the microphysical sources have to be computed
INTEGER :: IJB           ! 
INTEGER :: IJE           !
INTEGER :: IKB           ! 
INTEGER :: IKE           !
INTEGER :: ISIZE         !
!
REAL    :: ZTSPLITR      ! Small time step for rain sedimentation
REAL    :: ZEPS          ! molar mass ratio
!
INTEGER :: ISEDIM, INUCT, & ! Case number of sedimentation, nucleation,
           IMICRO, IEVAP    ! coalescence and rain_evaporation locations
LOGICAL, DIMENSION(SIZE(PRHODREF,1),SIZE(PRHODREF,2),SIZE(PRHODREF,3)) &
            :: GSEDIM ! Test where to compute the SED processes
LOGICAL, DIMENSION(SIZE(PRHODREF,1),SIZE(PRHODREF,2),SIZE(PRHODREF,3)) &
            :: GNUCT  ! Test where to compute the HEN process
LOGICAL, DIMENSION(SIZE(PRHODREF,1),SIZE(PRHODREF,2),SIZE(PRHODREF,3)) &
            :: GMICRO ! Test where to compute coalescence proc.
LOGICAL, DIMENSION(SIZE(PRHODREF,1),SIZE(PRHODREF,2),SIZE(PRHODREF,3)) &
            :: GEVAP  ! Test where to compute rain_evap. proc.
INTEGER, DIMENSION(:), ALLOCATABLE :: IVEC1             ! Vectors of indices for
                                                        ! interpolations
REAL,    DIMENSION(:), ALLOCATABLE :: ZVEC1             ! Work vectors for 
                                                        ! interpolations
REAL,    DIMENSION(SIZE(PRHODREF,1),SIZE(PRHODREF,2),SIZE(PRHODREF,3))   &
                                  :: ZW ! work array
REAL,    DIMENSION(SIZE(PRHODREF,1),SIZE(PRHODREF,2),SIZE(PRHODREF,3))   &
                                  :: ZWSEDR, ZWSEDC, &! sedimentation fluxes
                                     ZZW1LOG          ! supersaturation
REAL,    DIMENSION(SIZE(PRHODREF,1),SIZE(PRHODREF,2),SIZE(PRHODREF,3))   &
                                  :: ZT, ZTM,ZTDT, ZDRC  ! Temperature
REAL,    DIMENSION(SIZE(PRHODREF,1),SIZE(PRHODREF,2),SIZE(PRHODREF,3))   &
                                  :: ZRVSAT,ZZA,ZCHEN
REAL,    DIMENSION(SIZE(PRHODREF,1),SIZE(PRHODREF,2),SIZE(PRHODREF,3))   &
                                  :: ZLV !latent heat of vaporization
REAL,    DIMENSION(SIZE(PRHODREF,1),SIZE(PRHODREF,2),SIZE(PRHODREF,3))   &
                                  :: ZWLBDC,ZWLBDC3,ZWLBDR,ZWLBDR3
REAL,    DIMENSION(SIZE(PRHODREF,1),SIZE(PRHODREF,2),SIZE(PRHODREF,3))   &
                                  :: ZMVRR,ZVRR,ZVCR
REAL,    DIMENSION(SIZE(PRHODREF,1),SIZE(PRHODREF,2),SIZE(PRHODREF,3))   &
                                  :: ZMVRC !Cloud water mean volumic radius
REAL, DIMENSION(:), ALLOCATABLE :: ZRVT    ! Water vapor m.r. at t 
REAL, DIMENSION(:), ALLOCATABLE :: ZRCT    ! Cloud water m.r. at t 
REAL, DIMENSION(:), ALLOCATABLE :: ZRRT    ! Rain water m.r. at t 
REAL, DIMENSION(:), ALLOCATABLE :: ZCCT    ! cloud conc. at t
REAL, DIMENSION(:), ALLOCATABLE :: ZCRT    ! rain conc. at t
!
REAL, DIMENSION(:), ALLOCATABLE :: ZRVS    ! Water vapor m.r. source
REAL, DIMENSION(:), ALLOCATABLE :: ZRCS    ! Cloud water m.r. source
REAL, DIMENSION(:), ALLOCATABLE :: ZRRS    ! Rain water m.r. source
REAL, DIMENSION(:), ALLOCATABLE :: ZCNS    ! nucleus conc. source
REAL, DIMENSION(:), ALLOCATABLE :: ZCCS    ! cloud conc. source
REAL, DIMENSION(:), ALLOCATABLE :: ZCRS    ! rain conc. source
REAL, DIMENSION(:), ALLOCATABLE :: ZTHS    ! Theta source
!
REAL,    DIMENSION(:), ALLOCATABLE :: ZZVRR    !terminal velocity for drop concentration
REAL,    DIMENSION(:), ALLOCATABLE :: ZZVCR    !erminal velocity for rain water
!
REAL, DIMENSION(:), ALLOCATABLE :: &
                  ZRHODREF, & ! RHO Dry REFerence
                  ZZT,      & ! Temperature
                  ZTDTBIS,  & ! dT/dt
                  ZEXNREF,  & ! EXNer Pressure REFerence
                  ZZW1,     & ! Work array
                  ZZW2,     & ! Work array
                  ZZW3,     & ! Work array
                  ZZW4,     & ! Work array
                  ZZW5,     & ! Work array
                  ZZLV,     & ! Latent heat of vaporization at T
                  ZSMAX,    & ! Maximum supersaturation
                  ZLBDC, ZLBDR,   &  ! Lambda parameter
                  ZLBDC3, ZLBDR3, & ! Lambda**3
                  ZPABST, ZNCN, ZMCN
REAL, DIMENSION(:), ALLOCATABLE    :: ZDG3
REAL, DIMENSION(:,:), ALLOCATABLE  :: ZAERO, ZAEROS, ZSOLORG, ZMI
REAL  :: ZFACT, JSV, ZMU, ZALPHA
REAL, DIMENSION(:), ALLOCATABLE    :: ZRTMIN
REAL, DIMENSION(:), ALLOCATABLE    :: ZCTMIN
!
!-------------------------------------------------------------------------------
!
!*       1.     COMPUTE THE SLOPE PARAMETERS ZLBDC
!   	        ----------------------------------------
!
IIB=1+JPHEXT
IIE=SIZE(PZZ,1) - JPHEXT
IJB=1+JPHEXT
IJE=SIZE(PZZ,2) - JPHEXT
IKB=1+JPVEXT
IKE=SIZE(PZZ,3) - JPVEXT
!
ISIZE = SIZE(XRTMIN)
ISIZE = SIZE(XCTMIN)
ALLOCATE(ZCTMIN(ISIZE))
ALLOCATE(ZRTMIN(ISIZE))
ZRTMIN(:) = XRTMIN(:) / PTSTEP
ZCTMIN(:) = XCTMIN(:) / PTSTEP
!
ZWLBDC3(:,:,:) = 1.E30
ZWLBDC(:,:,:)  = 1.E10
!
WHERE (PRCT(:,:,:)>XRTMIN(2) .AND. PCCT(:,:,:)>XCTMIN(2))
  ZWLBDC3(:,:,:) = XLBC * PCCT(:,:,:) / (PRHODREF(:,:,:) * PRCT(:,:,:))
  ZWLBDC(:,:,:)  = ZWLBDC3(:,:,:)**XLBEXC
END WHERE
!
! Commented by O.Thouron 03/2013
!ZMVRC(:,:,:)= 0.
!WHERE (PCCS(:,:,:) > 0. .and. PRCS(:,:,:)>0. )
!   ZMVRC(:,:,:)= ((3. * PRHODREF(:,:,:)*PRCS(:,:,:))/   &
!                  (4. * XPI *XRHOLW*PCCS(:,:,:)))**0.333 ! in m
!ENDWHERE
!WHERE (ZMVRC(:,:,:) .GT. 25.E-6)
!   PCCS(:,:,:) = (3. * PRHODREF(:,:,:)*PRCS(:,:,:))/   &
!                       (4. * XPI *XRHOLW*(25.E-6)**3.)
!   PCNS(:,:,:) = PCCS(:,:,:)
!ENDWHERE
!-------------------------------------------------------------------------------
!
!
!
!
!*       2.     COMPUTES THE NUCLEATION PROCESS SOURCES
!   	        --------------------------------------
!
IF ((HACTCCN == 'ABRK').AND.((LORILAM).OR.(LDUST).OR.(LSALT))) THEN
 CALL AER_NUCLEATION
ELSE
 IF (.NOT. LSUPSAT) THEN
   CALL KHKO_NUCLEATION
 ELSE
  ZEPS= XMV / XMD
  ZT(:,:,:)  = PTHT(:,:,:) * (PPABST(:,:,:)/XP00)**(XRD/XCPD)
!
  ZRVSAT(:,:,:) = ZEPS / (PPABST(:,:,:) * &
                   EXP(-XALPW+XBETAW/ZT(:,:,:)+XGAMW*LOG(ZT(:,:,:))) - 1.0)
 END IF
ENDIF
!
!------------------------------------------------------------------------------
!
!*       3.    COALESCENCE PROCESSES
!              ---------------------
! 
IF (ORAIN) THEN
!
!  optimization by looking for locations where
!  the microphysical fields are larger than a minimal value only !!!
!
  CALL KHKO_COALESCENCE
!
!-------------------------------------------------------------------------------
!
!        4.    EVAPORATION OF RAINDROPS
!              ------------------------
!
  CALL KHKO_EVAPORATION
!
!-------------------------------------------------------------------------------
!
!        5.    SPONTANEOUS BREAK-UP (NUMERICAL FILTER)
!              --------------------
!
  ZWLBDR(:,:,:) = 1.E10
  WHERE (PRRS(:,:,:)>0.0.AND.PCRS(:,:,:)>0.0 )
    ZWLBDR3(:,:,:) = XLBR * PCRS(:,:,:) / (PRHODREF(:,:,:) * PRRS(:,:,:))
    ZWLBDR(:,:,:)  = ZWLBDR3(:,:,:)**XLBEXR
  END WHERE
  WHERE (ZWLBDR(:,:,:)<(XACCR1/XSPONBUD1))
    PCRS(:,:,:) = PCRS(:,:,:)*MAX((1.+XSPONCOEF2*(XACCR1/ZWLBDR(:,:,:)-XSPONBUD1)**2),&
                                                 (XACCR1/ZWLBDR(:,:,:)/XSPONBUD3)**3)
  END WHERE
!
ENDIF
!
IF (LBUDGET_SV) &         
  CALL BUDGET (PCRS(:,:,:)*PRHODJ(:,:,:),15+(NSV_C2R2BEG-1),&
                    &'BRKU_BU_RSV') ! RCR
!
!*       6.     COMPUTE THE SEDIMENTATION (RS) SOURCE
!	        -------------------------------------
!
!*       6.1    time splitting loop initialization        
!
ZTSPLITR = PTSTEP / FLOAT(KSPLITR)       ! Small time step
!
!
!*       6.2    compute the sedimentation velocities for rain
!   	        --------------------------------------------
!
ZMVRR(:,:,:) = 0.
ZVRR(:,:,:) = 0.
ZVCR(:,:,:) = 0.
WHERE (PCRT(:,:,:) > XCTMIN(3) .and. PRRT(:,:,:)>XRTMIN(3) )
  ZMVRR(:,:,:) = ((3. * PRHODREF(:,:,:)*PRRT(:,:,:))/   &
                  (4. * XPI *XRHOLW*PCRT(:,:,:)))**0.333 ! in m
  ZVRR(:,:,:) = 0.012 * 1.0E6 * ZMVRR(:,:,:) - 0.2 ! velocity for mixing ratio
  ZVCR(:,:,:) = 0.007 * 1.0E6 * ZMVRR(:,:,:) - 0.1 ! velocity for concentration
END WHERE
WHERE (ZVRR(:,:,:) .lt. 0.0 .OR. ZVCR(:,:,:) .lt. 0.0)
  ZVRR(:,:,:) = 0.0
  ZVCR(:,:,:) = 0.0
END WHERE
!
CALL KHKO_SEDIMENTATION
! 
!-------------------------------------------------------------------------------
DEALLOCATE(ZRTMIN)
DEALLOCATE(ZCTMIN)
!
CONTAINS
!
!
!-------------------------------------------------------------------------------
!
!  
  SUBROUTINE KHKO_SEDIMENTATION
!
!*      0. DECLARATIONS
!          ------------
!
IMPLICIT NONE
!
!*       0.2  declaration of local variables
!
!
INTEGER , DIMENSION(SIZE(GSEDIM)) :: I1,I2,I3 ! Used to replace the COUNT
INTEGER                           :: JL       ! and PACK intrinsics 
!
!-------------------------------------------------------------------------------
!
!*       2.1    compute the fluxes  
!
!  optimization by looking for locations where
!  the precipitating fields are larger than a minimal value only !!!
!
DO JN = 1 , KSPLITR
  GSEDIM(:,:,:) = .FALSE.
  IF( OSEDC ) THEN
    GSEDIM(IIB:IIE,IJB:IJE,IKB:IKE) =                               &
            PRCT(IIB:IIE,IJB:IJE,IKB:IKE)/PTSTEP>ZRTMIN(2) .OR.     &
            (PRRT(IIB:IIE,IJB:IJE,IKB:IKE)/PTSTEP>ZRTMIN(3) .AND.   &
            PCRT(IIB:IIE,IJB:IJE,IKB:IKE)/PTSTEP>ZCTMIN(3))
  ELSE
    GSEDIM(IIB:IIE,IJB:IJE,IKB:IKE) =                               &
            PRRT(IIB:IIE,IJB:IJE,IKB:IKE)/PTSTEP>ZRTMIN(3) .AND.    &
            PCRT(IIB:IIE,IJB:IJE,IKB:IKE)/PTSTEP>ZCTMIN(3)
  END IF
!
  ISEDIM = COUNTJV( GSEDIM(:,:,:),I1(:),I2(:),I3(:))
!
  IF( ISEDIM >= 1 ) THEN
!
    IF( JN==1 ) THEN
      IF( OSEDC ) THEN
        PCCS(:,:,:) = PCCS(:,:,:) * PTSTEP
        PRCS(:,:,:) = PRCS(:,:,:) * PTSTEP
      END IF
      PCRS(:,:,:) = PCRS(:,:,:) * PTSTEP
      PRRS(:,:,:) = PRRS(:,:,:) * PTSTEP
      DO JK = IKB , IKE
        ZW(:,:,JK) = ZTSPLITR/(PZZ(:,:,JK+1) -PZZ(:,:,JK))
      END DO
    END IF
!
    ALLOCATE(ZRHODREF(ISEDIM))
    DO JL = 1,ISEDIM
      ZRHODREF(JL) = PRHODREF(I1(JL),I2(JL),I3(JL))
    END DO
!
    ALLOCATE(ZZW1(ISEDIM)) 
    ALLOCATE(ZZW2(ISEDIM)) 
    ALLOCATE(ZZW3(ISEDIM)) 
!
!*       2.21   for cloud
!
    ZZW1(:) = 0.0
    ZZW2(:) = 0.0
    ZZW3(:) = 0.0
!
    IF( OSEDC.AND.MAXVAL(PRCS(:,:,:))>0.0 ) THEN
      ALLOCATE(ZRCS(ISEDIM))
      ALLOCATE(ZCCS(ISEDIM))
      ALLOCATE(ZLBDC(ISEDIM))
      DO JL = 1,ISEDIM
        ZRCS(JL) = PRCT(I1(JL),I2(JL),I3(JL))
        ZCCS(JL) = PCCT(I1(JL),I2(JL),I3(JL))
        ZLBDC(JL) = ZWLBDC(I1(JL),I2(JL),I3(JL))
      END DO
      WHERE( ZRCS(:)>XRTMIN(2) )
        ZZW3(:) = ZRHODREF(:)**(-XCEXVT) * ZLBDC(:)**(-XDC)
        ZZW1(:) = XFSEDRC * ZRCS(:) * ZZW3(:) * ZRHODREF(:)
        ZZW2(:) = XFSEDCC * ZCCS(:) * ZZW3(:)
      END WHERE
      ZWSEDR(:,:,:) = UNPACK( ZZW1(:),MASK=GSEDIM(:,:,:),FIELD=0.0 )
      ZWSEDC(:,:,:) = UNPACK( ZZW2(:),MASK=GSEDIM(:,:,:),FIELD=0.0 )
      ZWSEDR(:,:,:) = MAX(MIN(ZW(:,:,:)*ZWSEDR(:,:,:)/PRHODREF(:,:,:), &
                PRCS(:,:,:)),0.0) 
      ZWSEDC(:,:,:) = MAX(MIN(ZW(:,:,:)*ZWSEDC(:,:,:),PCCS(:,:,:)),0.0)
!      
!*       2.3     update the rain tendency
!
      DO JK = IKB , IKE
        PRCS(:,:,JK) = PRCS(:,:,JK)+ZWSEDR(:,:,JK+1)-ZWSEDR(:,:,JK)
        PCCS(:,:,JK) = PCCS(:,:,JK)+ZWSEDC(:,:,JK+1)-ZWSEDC(:,:,JK)
      END DO
      IF( JN.EQ.1 ) THEN
        PINPRC(:,:) = ZWSEDR(:,:,IKB)/XRHOLW                           ! in m/s
      END IF
      DEALLOCATE(ZRCS)
      DEALLOCATE(ZCCS)
      DEALLOCATE(ZLBDC)
    END IF
!
!*       2.22   for drizzle
!
    ZZW1(:) = 0.0
    ZZW2(:) = 0.0
!
    IF( MAXVAL(PRRS(:,:,:))>0.0 ) THEN
      ALLOCATE(ZRRS(ISEDIM)) 
      ALLOCATE(ZCRS(ISEDIM))
      ALLOCATE(ZZVRR(ISEDIM))
      ALLOCATE(ZZVCR(ISEDIM))
      DO JL = 1,ISEDIM
        ZRRS(JL) = PRRT(I1(JL),I2(JL),I3(JL))
        ZCRS(JL) = PCRT(I1(JL),I2(JL),I3(JL))
        ZZVRR(JL) = ZVRR(I1(JL),I2(JL),I3(JL))
        ZZVCR(JL) = ZVCR(I1(JL),I2(JL),I3(JL))
      END DO
      WHERE (ZRRS(:)>XRTMIN(3) )
        ZZW1(:) = ZZVRR(:) * ZRRS(:) * ZRHODREF(:)
        ZZW2(:) = ZZVCR(:) * ZCRS(:)
      END WHERE
      ZWSEDR(:,:,:) = UNPACK( ZZW1(:),MASK=GSEDIM(:,:,:),FIELD=0.0 )
      ZWSEDC(:,:,:) = UNPACK( ZZW2(:),MASK=GSEDIM(:,:,:),FIELD=0.0 )
!
      ZWSEDR(:,:,:) = MAX(MIN(ZW(:,:,:)*ZWSEDR(:,:,:)/PRHODREF(:,:,:),&
                      PRRS(:,:,:)),0.0) 
      ZWSEDC(:,:,:)= MAX(MIN(ZW(:,:,:)*ZWSEDC(:,:,:),PCRS(:,:,:)),0.0) 
!      
!*       2.3     update the rain tendency
!
      DO JK = IKB , IKE
        PRRS(:,:,JK) = PRRS(:,:,JK) +ZWSEDR(:,:,JK+1)-ZWSEDR(:,:,JK)
        PCRS(:,:,JK) = PCRS(:,:,JK) +ZWSEDC(:,:,JK+1)-ZWSEDC(:,:,JK)
      END DO
!
      DEALLOCATE(ZRRS)
      DEALLOCATE(ZCRS)
      DEALLOCATE(ZZVRR)
      DEALLOCATE(ZZVCR)
!
    ELSE
      ZWSEDR(:,:,IKB) = 0.0
    END IF
!
!*       2.4     compute the explicit accumulated precipitations
!         
    IF( JN.EQ.1 ) THEN
      PINPRR(:,:) = ZWSEDR(:,:,IKB)/XRHOLW                           ! in m/s
      PINPRR3D(:,:,:) = ZWSEDR(:,:,:)/XRHOLW                           ! in m/s
    END IF
!
    DEALLOCATE(ZRHODREF)
    DEALLOCATE(ZZW1)
    DEALLOCATE(ZZW2)
    DEALLOCATE(ZZW3)
    IF( JN==KSPLITR ) THEN
      IF( OSEDC ) THEN
        PRCS(:,:,:) = PRCS(:,:,:) / PTSTEP
        PCCS(:,:,:) = PCCS(:,:,:) / PTSTEP
      END IF
      PRRS(:,:,:) = PRRS(:,:,:) / PTSTEP
      PCRS(:,:,:) = PCRS(:,:,:) / PTSTEP
    END IF
  END IF
END DO
!
!*       2.5     budget storage
!
IF (LBUDGET_RC.AND.OSEDC)                                              &
               CALL BUDGET (PRCS(:,:,:)*PRHODJ(:,:,:),7 ,'SEDI_BU_RRC')
IF (LBUDGET_RR) CALL BUDGET (PRRS(:,:,:)*PRHODJ(:,:,:),8 ,'SEDI_BU_RRR')
IF (LBUDGET_SV) THEN
  IF (OSEDC) CALL BUDGET (PCCS(:,:,:)*PRHODJ(:,:,:),14+(NSV_C2R2BEG-1),&
                    &'SEDI_BU_RSV') ! RCC
  CALL BUDGET (PCRS(:,:,:)*PRHODJ(:,:,:),15+(NSV_C2R2BEG-1),&
                    &'SEDI_BU_RSV') ! RCR
END IF
!
  END SUBROUTINE KHKO_SEDIMENTATION
!
!-------------------------------------------------------------------------------
!
!
  SUBROUTINE KHKO_NUCLEATION
!
!*      0. DECLARATIONS
!          ------------
!
IMPLICIT NONE
!
!*       0.2  declaration of local variables
!
REAL, DIMENSION(:), ALLOCATABLE   :: ZTCELSIUS
INTEGER , DIMENSION(SIZE(GNUCT))  :: I1,I2,I3 ! Used to replace the COUNT
INTEGER                           :: JL       ! and PACK intrinsics
INTEGER                           :: J1
!
!-------------------------------------------------------------------------------
!
!  compute the temperature T and
!          the latent heat of vaporization Lv(T) and 
!          the specific heat for moist air Cph
!
ZEPS= XMV / XMD
!
ZT(:,:,:)  = PTHT(:,:,:) * (PPABST(:,:,:)/XP00)**(XRD/XCPD)
!
ZRVSAT(:,:,:) = ZEPS / (PPABST(:,:,:) * &
                   EXP(-XALPW+XBETAW/ZT(:,:,:)+XGAMW*ALOG(ZT(:,:,:))) - 1.0)
ZZW1LOG(:,:,:)= 0. ! supersaturation
ZTDT(:,:,:) = 0.
IF (OACTIT) THEN
  ZTM(:,:,:)    = PTHM(:,:,:) * (PPABSM(:,:,:)/XP00)**(XRD/XCPD)
  ZTDT(:,:,:)   = (ZT(:,:,:)-ZTM(:,:,:))/PTSTEP                              ! dT/dt
  ZDRC(:,:,:)   = (PRCT(:,:,:)-PRCM(:,:,:))/PTSTEP                           ! drc/dt
  ZTDT(:,:,:)   = MIN(0.,ZTDT(:,:,:)+(XG*PW_NU(:,:,:))/XCPD- &
  (XLVTT+(XCPV-XCL)*(ZT(:,:,:)-XTT))*ZDRC(:,:,:)/XCPD)
END IF
!
!  optimization by looking for locations where
!  the updraft velocity is positive!!!
!
GNUCT(:,:,:) = .FALSE.
IF( OACTIT ) THEN
 GNUCT(IIB:IIE,IJB:IJE,IKB:IKE) = (PW_NU(IIB:IIE,IJB:IJE,IKB:IKE)>XWMIN .OR. &
 ZTDT(IIB:IIE,IJB:IJE,IKB:IKE)<XTMIN)  .AND.   &
 PRVT(IIB:IIE,IJB:IJE,IKB:IKE)>(0.98*ZRVSAT(IIB:IIE,IJB:IJE,IKB:IKE))
ELSE
 GNUCT(IIB:IIE,IJB:IJE,IKB:IKE) = PW_NU(IIB:IIE,IJB:IJE,IKB:IKE)>XWMIN .AND.   &
           PRVT(IIB:IIE,IJB:IJE,IKB:IKE)>(0.98*ZRVSAT(IIB:IIE,IJB:IJE,IKB:IKE))
END IF
INUCT = COUNTJV( GNUCT(:,:,:),I1(:),I2(:),I3(:))
IF( INUCT >= 1 ) THEN
  ALLOCATE(ZRVT(INUCT))
  ALLOCATE(ZRCT(INUCT))
  ALLOCATE(ZRRT(INUCT))
  ALLOCATE(ZCNS(INUCT))
  ALLOCATE(ZCCS(INUCT))
  ALLOCATE(ZZT(INUCT)) 
  ALLOCATE(ZTDTBIS(INUCT))
  ALLOCATE(ZZW1(INUCT))
  ALLOCATE(ZZW2(INUCT))
  ALLOCATE(ZZW3(INUCT))
  ALLOCATE(ZZW4(INUCT))
  ALLOCATE(ZZW5(INUCT))
  ALLOCATE(ZVEC1(INUCT))
  ALLOCATE(IVEC1(INUCT))
  ALLOCATE(ZRHODREF(INUCT)) 
  ALLOCATE(ZEXNREF(INUCT)) 
  DO JL=1,INUCT
    ZRVT(JL) = PRVT(I1(JL),I2(JL),I3(JL))
    ZRCT(JL) = PRCT(I1(JL),I2(JL),I3(JL))
    ZRRT(JL) = PRRT(I1(JL),I2(JL),I3(JL))
    ZCNS(JL) = PCNS(I1(JL),I2(JL),I3(JL))
    ZCCS(JL) = PCCS(I1(JL),I2(JL),I3(JL))
    ZZT(JL)  = ZT(I1(JL),I2(JL),I3(JL))
    ZZW1(JL) = ZRVSAT(I1(JL),I2(JL),I3(JL))
    ZZW2(JL) = PW_NU(I1(JL),I2(JL),I3(JL))
    ZTDTBIS(JL) = ZTDT(I1(JL),I2(JL),I3(JL))
    ZRHODREF(JL) = PRHODREF(I1(JL),I2(JL),I3(JL))
    ZEXNREF(JL)  = PEXNREF(I1(JL),I2(JL),I3(JL))
  ENDDO
  ZZW1(:) = 1.0/ZEPS + 1.0/ZZW1(:)                                   &
          + (((XLVTT+(XCPV-XCL)*(ZZT(:)-XTT))/ZZT(:))**2)/(XCPD*XRV) ! Psi2
!
!*       3.1     compute the heterogeneous nucleation source: RVHENC, CVHENC
!
!*       3.1.1   compute the constant term (ZZW3)
!
  ZVEC1(:) = MAX( 1.00001, MIN( FLOAT(NAHEN)-0.00001, &
                  XAHENINTP1 * ZZT(:) + XAHENINTP2 )  )
  IVEC1(:) = INT( ZVEC1(:) )
  ZVEC1(:) = ZVEC1(:) - FLOAT( IVEC1(:) )
  ALLOCATE(ZSMAX(INUCT))
!
!
  IF( HPARAM_CCN == 'TFH' ) THEN
    ZZW2(:) = 100.*ZZW2(:) ! FH is in CGS units
    ALLOCATE(ZTCELSIUS(INUCT)); ZTCELSIUS(:) = ZZT(:) - XTT
    ZZW3(:) =   XAHENF( IVEC1(:)+1 )* ZVEC1(:)      &
              - XAHENF( IVEC1(:)   )*(ZVEC1(:) - 1.0)      ! Cste*(Psi1/Gr)
    ZZW3(:) = ZZW3(:)/ZZW2(:)**(XWCOEF_F1+ZTCELSIUS(:)*  &
                     (XWCOEF_F2+XWCOEF_F3*ZTCELSIUS(:)))
    ZZW3(:) = (ZZW3(:)/ZZW1(:)) * ZZW2(:) * ZRHODREF(:) ! R.H.S. of
                                                        ! Eq. (12) in FH92
!
!*       3.1.1.1   compute the maximum fo supersaturation
!
    ZSMAX(:) = ZZW3(:)**(1.0/(XKHEN+1.0)) ! first estimate (y_bar=0)
!
! 4 iterations to estimate S_max for the TFH parameterization
!
    ZZW1(:) =   XAHENY( IVEC1(:)+1 )* ZVEC1(:)      &
              - XAHENY( IVEC1(:)   )*(ZVEC1(:) - 1.0)     ! y_bar
    ZZW1(:) = ZZW1(:)*ZZW2(:)** (XWCOEF_Y1+ZTCELSIUS(:)*  &
                      (XWCOEF_Y2+XWCOEF_Y3*ZTCELSIUS(:)))
    DO J1 = 1,4
      ZSMAX(:) = (ZZW1(:)*ZSMAX(:)**XKHEN + ZSMAX(:))**(1.0/(XKHEN+1.0))
    END DO
    DEALLOCATE(ZTCELSIUS)
    ZZW3(:) = 1.0
  ELSE
    IF (OACTIT) THEN
      ZZW4(:)=XPSI1( IVEC1(:)+1)*ZZW2(:)+XPSI3(IVEC1(:)+1)*ZTDTBIS(:)
      ZZW5(:)=XPSI1( IVEC1(:))*ZZW2(:)+XPSI3(IVEC1(:))*ZTDTBIS(:)
      WHERE (ZZW4(:) < 0. .OR. ZZW5(:) < 0.)
             ZZW4(:) = 0.
             ZZW5(:) = 0.
      END WHERE
      ZZW3(:) = XCHEN*XAHENG(IVEC1(:)+1)*(ZZW4(:)**1.5)*ZVEC1(:)/XCHEN      &
              - XCHEN*XAHENG( IVEC1(:))*(ZZW5(:)**1.5)*(ZVEC1(:) - 1.0)/XCHEN
                       ! Cste*((Psi1*w+Psi3*dT/dt)/(G))**1.5
    ELSE
      ZZW3(:) = XAHENG( IVEC1(:)+1)*((XPSI1( IVEC1(:)+1)*ZZW2(:))**1.5)* ZVEC1(:)      &
              - XAHENG( IVEC1(:))*((XPSI1(IVEC1(:))*ZZW2(:))**1.5)*(ZVEC1(:) - 1.0)
    END IF
    ZZW5(:) = 1.
    ZZW3(:) = (ZZW3(:)/ZZW1(:))*ZRHODREF(:) ! R.H.S. of
                                                      ! Eq 9 of CPB 98 
    WHERE (ZZW3(:) == 0.)
            ZZW5(:)= -1.
    END WHERE
!
!*       3.1.2.1   compute the maximum fo supersaturation
!
    ZSMAX(:) = ZZW3(:)**(1.0/(XKHEN+2.0)) ! Smax has no unit
!
! 4 iterations to estimate S_max for the CPB98 parameterization
!
    IF( HPARAM_CCN == 'CPB' ) THEN
      DO J1 = 1,4
       WHERE (ZZW5(:) > 0.)
        ZVEC1(:) = MAX( 1.00001, MIN( FLOAT(NHYP)-0.00001,      &
                        XHYPINTP1*LOG(ZSMAX(:))+XHYPINTP2 ) )
        IVEC1(:) = INT( ZVEC1(:) )
        ZVEC1(:) = ZVEC1(:) - FLOAT( IVEC1(:) )
        ZZW2(:)  =   XHYPF32( IVEC1(:)+1 )* ZVEC1(:)      &
                   - XHYPF32( IVEC1(:)   )*(ZVEC1(:) - 1.0)
        ZSMAX(:) = (ZZW3(:)/ZZW2(:))**(1.0/(XKHEN+2.0))
       ELSEWHERE
         ZSMAX(:)=0.
       END WHERE
      END DO
!
!*       3.2    compute the nucleus source
!
! ZSMAX(:) is used in percent in the nucleation formula
!
      ZZW3(:) =   XHYPF12( IVEC1(:)+1 )* ZVEC1(:)      &
                - XHYPF12( IVEC1(:)   )*(ZVEC1(:) - 1.0)
    ELSE
      ZZW3(:) = 1.0
    END IF
  END IF
  ZZW1LOG(:,:,:) = UNPACK( 100*ZSMAX(:),MASK=GNUCT(:,:,:),FIELD=0.0 )
  
!
! the CCN spectra formula uses ZSMAX in percent
!
 IF (XCONC_CCN > 0) THEN
  ZZW1(:) = MIN( XCONC_CCN,XCHEN * (100.0*ZSMAX(:))**XKHEN * ZZW3(:) ) / PTSTEP
 ELSE
  ZZW1(:) = XCHEN * (100.0*ZSMAX(:))**XKHEN * ZZW3(:) / PTSTEP
 ENDIF
  ZW(:,:,:)   = PCNS(:,:,:)
  PCNS(:,:,:) = UNPACK( MAX( ZZW1(:),ZCNS(:) ),MASK=GNUCT(:,:,:), &
                                                 FIELD=ZW(:,:,:)  )
!
  DEALLOCATE(IVEC1)
  DEALLOCATE(ZVEC1)
!
!*       3.3    compute the cloud water concentration and mixing ratio sources
!
  ZZW2(:) = MAX( (ZZW1(:)-ZCNS(:)),0.0 )
  ZZW1(:)=0.
  WHERE (ZZW5(:) > 0.)
    ZZW1(:) = MIN( XCSTDCRIT * ZZW2(:) / ( ((ZZT(:)*ZSMAX(:))**3.)*ZRHODREF(:) ),&
                 1.E-5 )
  END WHERE
  ZW(:,:,:) = MIN( UNPACK( ZZW1(:),MASK=GNUCT(:,:,:),FIELD=0.0 ),PRVS(:,:,:) )
!
  PRVS(:,:,:) = PRVS(:,:,:) - ZW(:,:,:)
  PRCS(:,:,:) = PRCS(:,:,:) + ZW(:,:,:) 
  ZW(:,:,:) = ZW(:,:,:)*(XLVTT+(XCPV-XCL)*(ZT(:,:,:)-XTT))/                      &
                    (PEXNREF(:,:,:)*( XCPD+XCPV*PRVT(:,:,:)+XCL*(PRCT(:,:,:)+PRRT(:,:,:))))
  PTHS(:,:,:) = PTHS(:,:,:) + ZW(:,:,:)
  ZW(:,:,:)   = PCCS(:,:,:)
  PCCS(:,:,:) = UNPACK( ZZW2(:)+ZCCS(:),MASK=GNUCT(:,:,:),FIELD=ZW(:,:,:) )
!
!
  DEALLOCATE(ZRVT)
  DEALLOCATE(ZRCT)
  DEALLOCATE(ZRRT)
  DEALLOCATE(ZCNS)
  DEALLOCATE(ZCCS)
  DEALLOCATE(ZZT)
  DEALLOCATE(ZSMAX)
  DEALLOCATE(ZZW1)
  DEALLOCATE(ZZW2)
  DEALLOCATE(ZZW3)
  DEALLOCATE(ZZW4)
  DEALLOCATE(ZZW5)
  DEALLOCATE(ZTDTBIS)
  DEALLOCATE(ZRHODREF)
  DEALLOCATE(ZEXNREF)
END IF
!
!*       3.4   budget storage
!
IF (LBUDGET_TH) CALL BUDGET (PTHS(:,:,:)*PRHODJ(:,:,:),4,'HENU_BU_RTH')
IF (LBUDGET_RV) CALL BUDGET (PRVS(:,:,:)*PRHODJ(:,:,:),6,'HENU_BU_RRV')
IF (LBUDGET_RC) CALL BUDGET (PRCS(:,:,:)*PRHODJ(:,:,:),7,'HENU_BU_RRC')
IF (LBUDGET_SV) THEN
  CALL BUDGET (PCNS(:,:,:)*PRHODJ(:,:,:),13+(NSV_C2R2BEG-1),'HENU_BU_RSV') ! RCN
  CALL BUDGET (PCCS(:,:,:)*PRHODJ(:,:,:),14+(NSV_C2R2BEG-1),'HENU_BU_RSV') ! RCC
END IF
!
  END SUBROUTINE KHKO_NUCLEATION
!
!-------------------------------------------------------------------------------
!
  SUBROUTINE AER_NUCLEATION
!
!*      0. DECLARATIONS
!          ------------
!
USE MODD_NSV
USE MODE_AERO_PSD
USE MODI_CH_AER_ACTIVATION

IMPLICIT NONE
!
!*       0.2  declaration of local variables
!
REAL, DIMENSION(:), ALLOCATABLE   :: ZTCELSIUS
INTEGER , DIMENSION(SIZE(GNUCT))  :: I1,I2,I3 ! Used to replace the COUNT
INTEGER                           :: JL       ! and PACK intrinsics
INTEGER                           :: J1
INTEGER                           :: JSV
!
!-------------------------------------------------------------------------------
!
!  compute the saturation vapor mixing ratio  
!          the radiative tendency                    
!
ZEPS= XMV / XMD
!
ZT(:,:,:)  = PTHT(:,:,:) * (PPABST(:,:,:)/XP00)**(XRD/XCPD)
!
ZRVSAT(:,:,:) = ZEPS / (PPABST(:,:,:) * &
                   EXP(-XALPW+XBETAW/ZT(:,:,:)+XGAMW*ALOG(ZT(:,:,:))) - 1.0)
ZZW1LOG(:,:,:)= 0. ! supersaturation
ZTDT(:,:,:)   = 0.
ZDRC(:,:,:)   = 0.
IF (OACTIT) THEN
  ZTM(:,:,:)    = PTHM(:,:,:) * (PPABSM(:,:,:)/XP00)**(XRD/XCPD)
  ZTDT(:,:,:)   = (ZT(:,:,:)-ZTM(:,:,:))/PTSTEP                              ! dT/dt
  ZDRC(:,:,:)   = (PRCT(:,:,:)-PRCM(:,:,:))/PTSTEP                           ! drc/dt
! Ratio 2 due to leap-frog
  ZTDT(:,:,:)   = MIN(0.,ZTDT(:,:,:)+(XG*PW_NU(:,:,:))/XCPD- &
  (XLVTT+(XCPV-XCL)*(ZT(:,:,:)-XTT))*ZDRC(:,:,:)/XCPD)
END IF

!  optimization by looking for locations where
!  the updraft velocity is positive!!!
!
GNUCT(:,:,:) = .FALSE.
IF( OACTIT ) THEN
 GNUCT(IIB:IIE,IJB:IJE,IKB:IKE) = (PW_NU(IIB:IIE,IJB:IJE,IKB:IKE)>XWMIN .OR. &
 ZTDT(IIB:IIE,IJB:IJE,IKB:IKE)<XTMIN)  .AND.   &
 PRVT(IIB:IIE,IJB:IJE,IKB:IKE)>(0.98*ZRVSAT(IIB:IIE,IJB:IJE,IKB:IKE))
ELSE
 GNUCT(IIB:IIE,IJB:IJE,IKB:IKE) = PW_NU(IIB:IIE,IJB:IJE,IKB:IKE)>XWMIN .AND.   &
           PRVT(IIB:IIE,IJB:IJE,IKB:IKE)>(0.98*ZRVSAT(IIB:IIE,IJB:IJE,IKB:IKE))
END IF
!
 INUCT = COUNTJV(GNUCT(:,:,:),I1(:),I2(:),I3(:))
IF( INUCT >= 1 ) THEN
  ALLOCATE(ZRVT(INUCT))
  ALLOCATE(ZRCT(INUCT))
  ALLOCATE(ZRRT(INUCT))
  ALLOCATE(ZZT(INUCT))
  ALLOCATE(ZTDTBIS(INUCT)) 
  ALLOCATE(ZZW1(INUCT))
  ALLOCATE(ZZW2(INUCT))
  ALLOCATE(ZZW3(INUCT))
  ALLOCATE(ZZW4(INUCT))
  ALLOCATE(ZDG3(INUCT))
  ALLOCATE(ZCCS(INUCT))
  ALLOCATE(ZCNS(INUCT))
  ALLOCATE(ZRHODREF(INUCT)) 
  ALLOCATE(ZEXNREF(INUCT)) 
  ALLOCATE(ZPABST(INUCT)) 
  ALLOCATE(ZNCN(INUCT)) 
  ALLOCATE(ZMCN(INUCT)) 
  ALLOCATE(ZAERO(INUCT,SIZE(PAEROT,4)))
  ALLOCATE(ZSMAX(INUCT)) 
  ALLOCATE(ZAEROS(INUCT,NSV_AER)) 
  ALLOCATE(ZSOLORG(INUCT,SIZE(PSOLORG,4))) 
  ALLOCATE(ZMI(INUCT,SIZE(PMI,4)))
  ALLOCATE(ZLBDC3(INUCT)) 

  DO JL=1,INUCT
    ZRVT(JL) = PRVT(I1(JL),I2(JL),I3(JL))
    ZRCT(JL) = PRCT(I1(JL),I2(JL),I3(JL))
    ZRRT(JL) = PRRT(I1(JL),I2(JL),I3(JL))
    ZCCS(JL) = PCCS(I1(JL),I2(JL),I3(JL))
    ZCNS(JL) = PCNS(I1(JL),I2(JL),I3(JL))
    ZZT(JL)  = ZT(I1(JL),I2(JL),I3(JL))
    ZZW1(JL) = ZRVSAT(I1(JL),I2(JL),I3(JL))
    ZZW2(JL) = PW_NU(I1(JL),I2(JL),I3(JL))
    ZTDTBIS(JL) = ZTDT(I1(JL),I2(JL),I3(JL))
    ZRHODREF(JL) = PRHODREF(I1(JL),I2(JL),I3(JL))
    ZEXNREF(JL)  = PEXNREF(I1(JL),I2(JL),I3(JL))
    ZPABST(JL)   = PPABST(I1(JL),I2(JL),I3(JL))
    ZAERO(JL,:)   = PAEROT(I1(JL),I2(JL),I3(JL),:)
    ZLBDC3(JL) = ZWLBDC3(I1(JL),I2(JL),I3(JL))
  ENDDO
!
ZSMAX(:) = 0.
IF (LORILAM) THEN
  DO JL=1,INUCT
    ZSOLORG(JL,:) = PSOLORG(I1(JL),I2(JL),I3(JL),:)
    ZMI(JL,:) = PMI(I1(JL),I2(JL),I3(JL),:)
  ENDDO
ELSE
  ZSOLORG(:,:) = 0.
  ZMI(:,:) = 0.
END IF


CALL CH_AER_ACTIVATION(ZAERO, ZZT, ZZW2, ZTDTBIS, ZRHODREF, ZPABST,&
                       ZNCN, ZMCN, ZSOLORG, ZMI, ZSMAX)
                       
! Nb de goutelettes activées
!test

    
ZZW1(:) = MAX(ZNCN(:)/PTSTEP - ZCNS(:), 0.)
!

ZW(:,:,:) = UNPACK( ZZW1(:),MASK=GNUCT(:,:,:),FIELD=0.0 )
PINPRR3D(:,:,:)=ZW(:,:,:) 
PCNS(:,:,:) = PCNS(:,:,:) + ZW(:,:,:)
!!
! Modification reservoir eau (gaz et liquide)
!
! valeur de petites goutelettes type brouillard (test)
ZZW2(:) = MAX(ZNCN(:)/PTSTEP - ZCNS(:), 0.)
ZZW1(:)=0.
WHERE(ZZW2(:).gt.0.0)
!    ZZW1(:) =(4.0/3.0)*XPI*1E3*ZZW2(:)*1E-6/ZRHODREF(:)
!    ZZW1(:) =MAX(ZZW1(:),XCSTDCRIT * ZZW2(:) / ( ((ZZT(:)*ZSMAX(:))**3.)&
!    *ZRHODREF(:) ))
!    ZZW1(:) =MIN( ZZW1(:), 1.E-5 )
    ZZW1(:)=MIN(XCSTDCRIT * ZZW2(:) / ( ((ZZT(:)*ZSMAX(:))**3.)&
    *ZRHODREF(:) ) , 1.E-5 )   
END WHERE
ZW(:,:,:) = MIN( UNPACK( ZZW1(:),MASK=GNUCT(:,:,:),FIELD=0.0 ),PRVS(:,:,:) )

PRVS(:,:,:) = PRVS(:,:,:) - ZW(:,:,:)
PRCS(:,:,:) = PRCS(:,:,:) + ZW(:,:,:) 
ZW(:,:,:) = ZW(:,:,:)*(XLVTT+(XCPV-XCL)*(ZT(:,:,:)-XTT))/                      &
       (PEXNREF(:,:,:)*( XCPD+XCPV*PRVT(:,:,:)+XCL*(PRCT(:,:,:)+PRRT(:,:,:))))
PTHS(:,:,:) = PTHS(:,:,:) + ZW(:,:,:)
ZW(:,:,:)   = PCCS(:,:,:) 
PCCS(:,:,:) = UNPACK( ZZW2(:)+ZCCS(:),MASK=GNUCT(:,:,:),FIELD=ZW(:,:,:))
!ZALPHA=0.8
!ZMU=3.
!ZDG3(:) =  1./ZLBDC3(:) * GAMMA(ZMU + 3./ZALPHA) / GAMMA(ZMU) ! integrated cubic diameter
!ZZW2(:) = ZZW1(:) + ZCCS(:)
!ZZW1(:) = XPI/6. * ZDG3(:)**3 * (ZZW1(:)) * 1000. /  ZRHODREF(:)
!
!ZW(:,:,:) = MIN( UNPACK( ZZW1(:),MASK=GNUCT(:,:,:),FIELD=0.0 ),PRVS(:,:,:) )

!PRVS(:,:,:) = PRVS(:,:,:) - ZW(:,:,:)
!PRCS(:,:,:) = PRCS(:,:,:) + ZW(:,:,:) 

! Modification temperature (diabatisme)
!ATTENTION POUR JEROME, JE PENSE QU'IL DEVRAIT AVOIR ZW A LA PLACE ZZW1
!ZZW1(:) = ZZW1(:)*(XLVTT+(XCPV-XCL)*(ZZT(:)-XTT))/                      &
!                    (ZEXNREF(:)*( XCPD+XCPV*ZRVT(:)+XCL*(ZRCT(:)+ZRRT(:))))
!
!ZW(:,:,:) = MIN( UNPACK( ZZW1(:),MASK=GNUCT(:,:,:),FIELD=0.0 ),PRVS(:,:,:) )
!
!PTHS(:,:,:) = PTHS(:,:,:) + ZW(:,:,:)
!
! Modification gouttes nuages
!ZW(:,:,:)   = PCCS(:,:,:)
!PCCS(:,:,:) = UNPACK(ZZW2(:),MASK=GNUCT(:,:,:),FIELD=ZW(:,:,:))
!
!
!
  DEALLOCATE(ZRVT)
  DEALLOCATE(ZRCT)
  DEALLOCATE(ZRRT)
  DEALLOCATE(ZZT)
  DEALLOCATE(ZTDTBIS) 
  DEALLOCATE(ZZW1)
  DEALLOCATE(ZZW2)
  DEALLOCATE(ZZW3)
  DEALLOCATE(ZZW4)
  DEALLOCATE(ZDG3)
  DEALLOCATE(ZCCS)
  DEALLOCATE(ZCNS)
  DEALLOCATE(ZRHODREF) 
  DEALLOCATE(ZEXNREF) 
  DEALLOCATE(ZPABST) 
  DEALLOCATE(ZNCN) 
  DEALLOCATE(ZMCN) 
  DEALLOCATE(ZAERO)
  DEALLOCATE(ZSMAX) 
  DEALLOCATE(ZAEROS) 
  DEALLOCATE(ZSOLORG) 
  DEALLOCATE(ZMI)
  DEALLOCATE(ZLBDC3) 

END IF
!
!*             budget storage
!
!
IF (LBUDGET_TH) CALL BUDGET (PTHS(:,:,:)*PRHODJ(:,:,:),4,'HENU_BU_RTH')
IF (LBUDGET_RV) CALL BUDGET (PRVS(:,:,:)*PRHODJ(:,:,:),6,'HENU_BU_RRV')
IF (LBUDGET_RC) CALL BUDGET (PRCS(:,:,:)*PRHODJ(:,:,:),7,'HENU_BU_RRC')
IF (LBUDGET_SV) THEN
  CALL BUDGET (PCNS(:,:,:)*PRHODJ(:,:,:),13+(NSV_C2R2BEG-1),'HENU_BU_RSV') ! RCN
  CALL BUDGET (PCCS(:,:,:)*PRHODJ(:,:,:),14+(NSV_C2R2BEG-1),'HENU_BU_RSV') ! RCC
END IF
!
!
  END SUBROUTINE AER_NUCLEATION
!
!-------------------------------------------------------------------------------
!
  SUBROUTINE KHKO_COALESCENCE
!
!          ------------
!
IMPLICIT NONE
!
!*       0.2  declaration of local variables
!
INTEGER , DIMENSION(SIZE(GNUCT))  :: I1,I2,I3 ! Used to replace the COUNT
INTEGER                           :: JL       ! and PACK intrinsics
!
!-------------------------------------------------------------------------------
!
GMICRO(:,:,:) = .FALSE.
GMICRO(IIB:IIE,IJB:IJE,IKB:IKE) =               &
  PRCT(IIB:IIE,IJB:IJE,IKB:IKE)>XRTMIN(2) .OR.  &
  PRRT(IIB:IIE,IJB:IJE,IKB:IKE)>XRTMIN(3)
IMICRO = COUNTJV( GMICRO(:,:,:),I1(:),I2(:),I3(:))
IF( IMICRO >= 1 ) THEN
  ALLOCATE(ZRCT(IMICRO))
  ALLOCATE(ZRRT(IMICRO))
  ALLOCATE(ZCCT(IMICRO))
  ALLOCATE(ZRCS(IMICRO))
  ALLOCATE(ZRRS(IMICRO))
  ALLOCATE(ZCCS(IMICRO))
  ALLOCATE(ZCRS(IMICRO))
  ALLOCATE(ZRHODREF(IMICRO))
!
  DO JL=1,IMICRO
    ZCCT(JL) = PCCT(I1(JL),I2(JL),I3(JL))
    ZRCT(JL) = PRCT(I1(JL),I2(JL),I3(JL))
    ZRRT(JL) = PRRT(I1(JL),I2(JL),I3(JL))
    ZCCS(JL) = PCCS(I1(JL),I2(JL),I3(JL))
    ZRCS(JL) = PRCS(I1(JL),I2(JL),I3(JL))
    ZRRS(JL) = PRRS(I1(JL),I2(JL),I3(JL))
    ZCRS(JL) = PCRS(I1(JL),I2(JL),I3(JL))
    ZRHODREF(JL) = PRHODREF(I1(JL),I2(JL),I3(JL))
  END DO
!
  ALLOCATE(ZZW1(IMICRO))
!
!*       4.1.1   autoconversion
!
  WHERE ( ZRCT(:) .GT. XRTMIN(2) .AND. ZCCT(:) .GT. XCTMIN(2)                 &
            .AND. (ZRCS(:) .GT. 0.0) .AND. (ZCCS(:) .GT. 0.0))
!
    ZZW1(:)= 1350.0 * ZRCT(:)**(2.47) * (ZCCT(:)/1.0E6)**(-1.79) ! ZCCT in cm-3         
    ZZW1(:) = min (ZRCS(:), ZZW1(:))
    ZRCS(:) = ZRCS(:) - ZZW1(:)
    ZRRS(:) = ZRRS(:) + ZZW1(:)
!
    ZCRS(:) = ZCRS(:) + ZZW1(:) * 3. * ZRHODREF(:)/(4.*XPI*XRHOLW*(XR0)**(3.))
!
    ZZW1(:) = min ( ZCCS(:),ZZW1(:) * ZCCT(:) / ZRCT(:))
    ZCCS(:) = ZCCS(:) - ZZW1(:)
!
  END WHERE
!
  ZW(:,:,:) = PRCS(:,:,:)
  PRCS(:,:,:) = UNPACK( ZRCS(:),MASK=GMICRO(:,:,:),FIELD=ZW(:,:,:) )
  ZW(:,:,:) = PCCS(:,:,:)
  PCCS(:,:,:) = UNPACK( ZCCS(:),MASK=GMICRO(:,:,:),FIELD=ZW(:,:,:) )
  ZW(:,:,:) = PRRS(:,:,:)
  PRRS(:,:,:) = UNPACK( ZRRS(:),MASK=GMICRO(:,:,:),FIELD=ZW(:,:,:) )
  ZW(:,:,:) = PCRS(:,:,:)
  PCRS(:,:,:) = UNPACK( ZCRS(:),MASK=GMICRO(:,:,:),FIELD=ZW(:,:,:) )
!
!*       4.1.2   budget storage
!
  IF (LBUDGET_SV) CALL BUDGET (PCCS(:,:,:)*PRHODJ(:,:,:),14+(NSV_C2R2BEG-1),&
                               &'SELF_BU_RSV') ! RCC
  IF (LBUDGET_RC) CALL BUDGET (PRCS*PRHODJ(:,:,:),7 ,'AUTO_BU_RRC')
  IF (LBUDGET_SV) CALL BUDGET (PCRS*PRHODJ(:,:,:),15+(NSV_C2R2BEG-1),'AUTO_BU_RSV')
  IF (LBUDGET_RR) CALL BUDGET (PRRS*PRHODJ(:,:,:),8 ,'AUTO_BU_RRR')
!
!*       4.2.1    Accretion sources
!
  WHERE ( (ZRCT(:) .GT. XRTMIN(2)) .AND. (ZRRT(:) .GT. XRTMIN(3))                 &
               .AND. (ZRCS(:) .GT. 0.0) .AND. (ZCCS(:) .GT. 0.0))

  ZZW1(:) = 67.0 * ( ZRCT(:) * ZRRT(:) )**1.15
  ZZW1(:) = MIN (ZRCS(:),ZZW1(:))
  ZRCS(:) = ZRCS(:) - ZZW1(:)
  ZRRS(:) = ZRRS(:) + ZZW1(:)
!
  ZZW1(:) = MIN (ZCCS(:),ZZW1(:) * ZCCT(:) / ZRCT(:))
  ZCCS(:) = ZCCS(:) - ZZW1(:)
!
  END WHERE
!
  ZW(:,:,:) = PRCS(:,:,:)
  PRCS(:,:,:) = UNPACK( ZRCS(:),MASK=GMICRO(:,:,:),FIELD=ZW(:,:,:) )
  ZW(:,:,:) = PCCS(:,:,:)
  PCCS(:,:,:) = UNPACK( ZCCS(:),MASK=GMICRO(:,:,:),FIELD=ZW(:,:,:) )
  ZW(:,:,:) = PRRS(:,:,:)
  PRRS(:,:,:) = UNPACK( ZRRS(:),MASK=GMICRO(:,:,:),FIELD=ZW(:,:,:) )
!
  DEALLOCATE(ZRCT)
  DEALLOCATE(ZRRT)
  DEALLOCATE(ZCCT)
  DEALLOCATE(ZRCS)
  DEALLOCATE(ZRRS)
  DEALLOCATE(ZCRS)
  DEALLOCATE(ZCCS)
  DEALLOCATE(ZRHODREF) 
  DEALLOCATE(ZZW1)
!
!*       4.2.2   budget storage
!
  IF (LBUDGET_SV) CALL BUDGET (PCCS(:,:,:)*PRHODJ(:,:,:),14+(NSV_C2R2BEG-1),&
                               &'ACCR_BU_RSV') ! RCC
  IF (LBUDGET_RC) CALL BUDGET (PRCS(:,:,:)*PRHODJ(:,:,:),7 ,'ACCR_BU_RRC')
  IF (LBUDGET_RR) CALL BUDGET (PRRS(:,:,:)*PRHODJ(:,:,:),8 ,'ACCR_BU_RRR')
!
ELSE
!
!*       4.3    Budgets are forwarded
!
  IF (LBUDGET_SV) CALL BUDGET (PCCS(:,:,:)*PRHODJ(:,:,:),14+(NSV_C2R2BEG-1),&
                               &'SELF_BU_RSV') ! RCC
  IF (LBUDGET_RC) CALL BUDGET (PRCS*PRHODJ(:,:,:),7 ,'AUTO_BU_RRC')
  IF (LBUDGET_SV) CALL BUDGET (PCRS*PRHODJ(:,:,:),15+(NSV_C2R2BEG-1),'AUTO_BU_RSV')
  IF (LBUDGET_RR) CALL BUDGET (PRRS*PRHODJ(:,:,:),8 ,'AUTO_BU_RRR')
  IF (LBUDGET_SV) CALL BUDGET (PCCS(:,:,:)*PRHODJ(:,:,:),14+(NSV_C2R2BEG-1),&
                               &'ACCR_BU_RSV') ! RCC
  IF (LBUDGET_RC) CALL BUDGET (PRCS(:,:,:)*PRHODJ(:,:,:),7 ,'ACCR_BU_RRC')
  IF (LBUDGET_RR) CALL BUDGET (PRRS(:,:,:)*PRHODJ(:,:,:),8 ,'ACCR_BU_RRR')

END IF
!
  END SUBROUTINE KHKO_COALESCENCE
!
!-------------------------------------------------------------------------------
!
!
  SUBROUTINE KHKO_EVAPORATION
!
!*      0. DECLARATIONS
!          ------------
!
IMPLICIT NONE
!
!*       0.2  declaration of local variables
!
INTEGER , DIMENSION(SIZE(GNUCT))  :: I1,I2,I3 ! Used to replace the COUNT
INTEGER                           :: JL       ! and PACK intrinsics
!
!-------------------------------------------------------------------------------
!
ZW(:,:,:) = 0.0
ZLV(:,:,:) = XLVTT + (XCPV-XCL)*(ZT(:,:,:)-XTT)   !!!latent heat of vaporization
!
!  optimization by looking for locations where
!  the raindrop mixing ratio is non-zero
!
GEVAP(:,:,:) = .FALSE.
!
GEVAP(IIB:IIE,IJB:IJE,IKB:IKE) =                              &
  PRRS(IIB:IIE,IJB:IJE,IKB:IKE)> 0.0 .AND.                    &
  PCRS(IIB:IIE,IJB:IJE,IKB:IKE)> 0.0 .AND.                    &
  PRRT(IIB:IIE,IJB:IJE,IKB:IKE)> 0.0 .AND.                    &
  PCRT(IIB:IIE,IJB:IJE,IKB:IKE)> 0.0 .AND.                    &
  PRVT(IIB:IIE,IJB:IJE,IKB:IKE)<ZRVSAT(IIB:IIE,IJB:IJE,IKB:IKE)
  !
IEVAP = COUNTJV( GEVAP(:,:,:),I1(:),I2(:),I3(:))
IF( IEVAP >= 1 ) THEN
  ALLOCATE(ZRVT(IEVAP))
  ALLOCATE(ZRCT(IEVAP))
  ALLOCATE(ZRRT(IEVAP))
  ALLOCATE(ZCRT(IEVAP))
  ALLOCATE(ZRVS(IEVAP))
  ALLOCATE(ZRRS(IEVAP))
  ALLOCATE(ZTHS(IEVAP))
  ALLOCATE(ZCRS(IEVAP))
  ALLOCATE(ZRHODREF(IEVAP))
  ALLOCATE(ZEXNREF(IEVAP))
  ALLOCATE(ZZT(IEVAP))  
  ALLOCATE(ZZLV(IEVAP)) 
  ALLOCATE(ZZW1(IEVAP)) 
  ALLOCATE(ZZW2(IEVAP))
  ALLOCATE(ZZW3(IEVAP))
!
  DO JL=1,IEVAP
    ZRVT(JL) = PRVT(I1(JL),I2(JL),I3(JL))
    ZRCT(JL) = PRCT(I1(JL),I2(JL),I3(JL))
    ZRRT(JL) = PRRT(I1(JL),I2(JL),I3(JL))
    ZCRT(JL) = PCRT(I1(JL),I2(JL),I3(JL))
    ZRRS(JL) = PRRS(I1(JL),I2(JL),I3(JL))
    ZRVS(JL) = PRVS(I1(JL),I2(JL),I3(JL))
    ZTHS(JL) = PTHS(I1(JL),I2(JL),I3(JL))
    ZCRS(JL) = PCRS(I1(JL),I2(JL),I3(JL))
    ZZT(JL) = ZT(I1(JL),I2(JL),I3(JL))
    ZZW1(JL) = ZRVSAT(I1(JL),I2(JL),I3(JL))
    ZRHODREF(JL) = PRHODREF(I1(JL),I2(JL),I3(JL))
    ZEXNREF(JL)  = PEXNREF(I1(JL),I2(JL),I3(JL))
    ZZLV(JL)  = ZLV(I1(JL),I2(JL),I3(JL))
  END DO
!  ZZLV(:) = XLVTT + (XCPV-XCL)*(ZZT(:)-XTT)   !!!latent heat of vaporization
!
!*       5.1  Compute the intermediate supersaturation mixing ratio
!
  ZZW3(:) = MAX((1.0 - ZRVT(:)/ZZW1(:)),0.0)  ! Subsaturation
!
!*       5.2  Compute the function G(T)
!
  ZZW2(:) = 1. / ( XRHOLW*((((ZZLV(:)/ZZT(:))**2)/(XTHCO*XRV)) +          & ! G
          (XRV*ZZT(:))/(XDIVA*EXP(XALPW-XBETAW/ZZT(:)-XGAMW*ALOG(ZZT(:))))))
!
!*       5.3  Compute the evaporation tendency
!
  ZZW2(:) = 3.0 * XCEVAP * ZZW2(:) * (4.*XPI*XRHOLW/(3.*ZRHODREF(:)))**(2./3.) *    &
                               (ZRRT(:))**(1./3.) * (ZCRT(:))**(2./3.) * ZZW3(:)
!
  ZZW2(:) = MIN(ZZW2(:),ZRRS(:))
!
!*       5.4  Adjust sources
!
  ZRVS(:) = ZRVS(:) + ZZW2(:)
  ZRRS(:) = ZRRS(:) - ZZW2(:)
  ZTHS(:) = ZTHS(:) - ZZW2(:) * ZZLV(:) /                                        &
                    ( ZEXNREF(:)*(XCPD + XCPV*ZRVT(:) + XCL*(ZRCT(:) + ZRRT(:)) ) )
  ZZW2(:) = MIN(ZZW2(:) * ZCRT(:)/ZRRT(:),ZCRS(:))
  ZCRS(:) = ZCRS(:) - ZZW2(:)
!
  ZW(:,:,:) = PRVS(:,:,:)
  PRVS(:,:,:) = UNPACK( ZRVS(:),MASK=GEVAP(:,:,:),FIELD=ZW(:,:,:) )
  ZW(:,:,:) = PRRS(:,:,:)
  PRRS(:,:,:) = UNPACK( ZRRS(:),MASK=GEVAP(:,:,:),FIELD=ZW(:,:,:) )
  ZW(:,:,:) = PTHS(:,:,:)
  PTHS(:,:,:) = UNPACK( ZTHS(:),MASK=GEVAP(:,:,:),FIELD=ZW(:,:,:) )
  ZW(:,:,:) = PCRS(:,:,:)
  PCRS(:,:,:) = UNPACK( ZCRS(:),MASK=GEVAP(:,:,:),FIELD=ZW(:,:,:) )
  ZW(:,:,:)= PEVAP3D(:,:,:)
  PEVAP3D(:,:,:) = UNPACK( ZZW2(:),MASK=GEVAP(:,:,:),FIELD=ZW(:,:,:) )
!
  DEALLOCATE(ZRCT)
  DEALLOCATE(ZRRT)
  DEALLOCATE(ZRVT)
  DEALLOCATE(ZCRT)
  DEALLOCATE(ZRVS)
  DEALLOCATE(ZRRS)
  DEALLOCATE(ZTHS)
  DEALLOCATE(ZCRS)
  DEALLOCATE(ZZLV)
  DEALLOCATE(ZZT)
  DEALLOCATE(ZRHODREF)
  DEALLOCATE(ZEXNREF)
  DEALLOCATE(ZZW1)
  DEALLOCATE(ZZW2)
  DEALLOCATE(ZZW3)
!
END IF
!------------------------------------------!
!*             correct negative values for rain
!   	      --------------------------------
!
IF (ANY(PRRS(:,:,:) < 0 .OR. PCRS(:,:,:) < 0)) THEN
  print*, 'RAIN_C2R2:  negative values of PRRS PCRS'
  print*, '  location of minimum:', MINLOC(PRRS(:,:,:))
  print*, '  value of minimum   :', MINVAL(PRRS(:,:,:))
  print*, '  location of minimum:', MINLOC(PCRS(:,:,:))
  print*, '  value of minimum   :', MINVAL(PCRS(:,:,:))
END IF
!
WHERE (PRRS(:,:,:)<0.) 
  PRCS(:,:,:) = PRCS(:,:,:)+PRRS(:,:,:)
  PRRS(:,:,:) = 0.
  PCRS(:,:,:) = 0.
END WHERE
!
!*           REMOVES NON-PHYSICAL LOW VALUES
!   	     -------------------------------
!
  GEVAP(:,:,:) = PRRS(:,:,:)< ZRTMIN(3) .AND. PCRS(:,:,:)< ZCTMIN(3)
  WHERE (GEVAP(:,:,:))
    PRVS(:,:,:) = PRVS(:,:,:) + PRRS(:,:,:)
    PTHS(:,:,:) = PTHS(:,:,:) - PRRS(:,:,:) * ZLV(:,:,:) /                         &
     ( PEXNREF(:,:,:)*(XCPD + XCPV*PRVT(:,:,:) + XCL*(PRCT(:,:,:) + PRRT(:,:,:)) ) )
    PCRS(:,:,:) = 0.0
    PRRS(:,:,:) = 0.0
  END WHERE
!
!
  IF (LBUDGET_RV) CALL BUDGET (PRVS(:,:,:)*PRHODJ(:,:,:),6 ,'REVA_BU_RRV')
  IF (LBUDGET_RR) CALL BUDGET (PRRS(:,:,:)*PRHODJ(:,:,:),8 ,'REVA_BU_RRR')
  IF (LBUDGET_TH) CALL BUDGET (PTHS(:,:,:)*PRHODJ(:,:,:),4 ,'REVA_BU_RTH')
  IF (LBUDGET_SV) CALL BUDGET (PCRS(:,:,:)*PRHODJ(:,:,:),15+(NSV_C2R2BEG-1),'CEVA_BU_RSV')
!
  END SUBROUTINE KHKO_EVAPORATION
!-------------------------------------------------------------------------------
!
!
  FUNCTION COUNTJV(LTAB,I1,I2,I3) RESULT(IC)
!
!*      0. DECLARATIONS
!          ------------
!
IMPLICIT NONE
!
!*       0.2  declaration of local variables
!
!
LOGICAL, DIMENSION(:,:,:) :: LTAB ! Mask
INTEGER, DIMENSION(:) :: I1,I2,I3 ! Used to replace the COUNT and PACK
INTEGER :: JI,JJ,JK,IC
!
!-------------------------------------------------------------------------------
!
IC = 0
DO JK = 1,SIZE(LTAB,3)
  DO JJ = 1,SIZE(LTAB,2)
    DO JI = 1,SIZE(LTAB,1)
      IF( LTAB(JI,JJ,JK) ) THEN
        IC = IC +1
        I1(IC) = JI
        I2(IC) = JJ
        I3(IC) = JK
      END IF
    END DO
  END DO
END DO
!
END FUNCTION COUNTJV 
!
!------------------------------------------------------------------------------
!
END SUBROUTINE RAIN_KHKO
