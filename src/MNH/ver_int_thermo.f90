!MNH_LIC Copyright 1994-2013 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENCE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
!-----------------------------------------------------------------
!     ##########################
      MODULE MODI_VER_INT_THERMO
!     ##########################
INTERFACE
      SUBROUTINE VER_INT_THERMO(OSHIFT,                                   &
                                PTHV_MX,PR_MX,PZS_LS,PZSMT_LS,PZMASS_MX,PZFLUX_MX, &
                                PPMHP_MX,PEXNTOP2D,PTHV,PR,PPMHP,PDIAG,   &
                                PLSTH_MX, PLSRV_MX, PLSTHM, PLSRVM        )
!
LOGICAL,                  INTENT(IN)  :: OSHIFT     ! T: vertical shift of BL (used for GRIB file data)
!                                                   ! F: no vertical shift (used for MESONH data)
REAL,   DIMENSION(:,:,:), INTENT(IN)  :: PTHV_MX    ! thetav on mixed grid
REAL,   DIMENSION(:,:,:,:), INTENT(IN):: PR_MX      ! r on mixed grid
REAL,   DIMENSION(:,:),   INTENT(IN)  :: PZS_LS     ! large scale orography
REAL,   DIMENSION(:,:),   INTENT(IN)  :: PZSMT_LS   ! large scale orography
REAL,   DIMENSION(:,:,:), INTENT(IN)  :: PZMASS_MX  ! altitude of the mass points
!                                                   ! of the mixed grid
REAL,   DIMENSION(:,:,:), INTENT(IN)  :: PZFLUX_MX  ! altitude of the flux points
!                                                   ! of the mixed grid
REAL,   DIMENSION(:,:,:), INTENT(IN)  :: PPMHP_MX   ! pressure minus hyd. pressure
REAL,   DIMENSION(:,:),   INTENT(IN)  :: PEXNTOP2D  ! top Exner function
REAL,   DIMENSION(:,:,:), INTENT(OUT) :: PTHV       ! thetav on MESO-NH grid
REAL,   DIMENSION(:,:,:,:), INTENT(OUT) :: PR       ! r on MESO-NH grid
REAL,   DIMENSION(:,:,:), INTENT(OUT) :: PPMHP      ! pressure minus hyd. pressure
REAL,                     INTENT(OUT) :: PDIAG      ! diagnostics computing time
REAL,DIMENSION(:,:,:), OPTIONAL, INTENT(IN) :: PLSTH_MX ! large scale potential temperature
REAL,DIMENSION(:,:,:), OPTIONAL, INTENT(IN) :: PLSRV_MX ! large scale vapor mixing ratios
REAL,DIMENSION(:,:,:), OPTIONAL, INTENT(OUT) :: PLSTHM  ! Large scale potential temperature
                                                   ! on mesonh grid
REAL,DIMENSION(:,:,:), OPTIONAL, INTENT(OUT) :: PLSRVM  ! Large scale vapor
                                                   ! mixing ratio on mesonh grid
END SUBROUTINE VER_INT_THERMO
END INTERFACE
END MODULE MODI_VER_INT_THERMO
!     #######################################################################
      SUBROUTINE VER_INT_THERMO(OSHIFT,                                   &
                                PTHV_MX,PR_MX,PZS_LS,PZSMT_LS,PZMASS_MX,PZFLUX_MX, &
                                PPMHP_MX,PEXNTOP2D,PTHV,PR,PPMHP,PDIAG,   &
                                PLSTH_MX, PLSRV_MX, PLSTHM, PLSRVM        )
!     #######################################################################
!
!!****  *VER_INT_THERMO* - Vertical shift and interpolation of thetav and rv.
!!
!!    PURPOSE
!!    -------
!!    This routine computes the 3D fields of virtual potential temperature
!!    and vapor mixing ratio on the MESO-NH grid with the MESO-NH orography
!!    from the corresponding fields on the hybrid grid with large scale
!!    orography.
!!
!!**  METHOD
!!    ------
!!  * The change of orography is performed by a altitude shifting method:
!!      - the shifting of the grid PZMASS_MX occurs in VER_SHIFT
!!      - the profile ZTHV_FREE is computed in FREE_ATM_PROFILE
!!      - the relative humidity is unchanged during the shift
!!      - the values on the shifted grid are computed as:
!!
!!     PTHV_MX(ZMASS_MX)-ZTHV_FREE(ZMASS_MX)+ZTHV_FREE(ZMASS_SH)
!!     ZHU_MX(ZMASS_MX)
!!
!!           the interpolations to obtain the shift profile values on the Aladin
!!           grid are performed by the generic function VER_INTERP.
!!  * The interpolation from the hybrid shifted grid to the MESO-NH grid is
!!    performed by the function V_INTERP. Only the inner points are passed
!!    as arguments in this function.
!!
!!
!!    EXTERNAL
!!    --------
!!
!!    subroutine MEAN_PROF  : to compute the shift profiles
!!    function VER_SHIFT    : to shift a array of altitudes
!!    function VER_INTERP   : to interpolate one field from one grid to another
!!    function FMLOOK       : to retrieve a logical unit number associated
!!                            with a file
!!    MZF                   : Shuman operator
!!    function SM_FOES      : to compute saturation vapor pressure
!!    function SM_PMR_HU    : function to compute vapor mixing ratio from HU
!!    routine COMPUTE_EXNER_FROM_TOP : to compute hydrostatic Exner function
!!
!!    Module MODI_SHUMAN    : interface for Shuman operators
!!    module MODI_MEAN_PROF : interface module for subroutine MEAN_PROF
!!    module MODI_VER_SHIFT : interface module for function VER_SHIFT
!!    module MODI_VER_INTERP: interface module for function VER_INTERP
!!    module MODE_THERMO    : contains routines SM_FOES and SM_PMR_HU
!!    module MODI_COMPUTE_EXNER_FROM_TOP
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!      Module MODD_CONF      : contains configuration variables for all models.
!!         NVERB : verbosity level for output-listing
!!      Module MODD_CONF1
!!         NRR
!!      Module MODD_LUNIT     :  contains logical unit names for all models
!!         CLUOUT0 : name of output-listing
!!      Module MODD_GRID1     : contains grid variables for model1
!!         XZS   : orography of MESO-NH
!!         XZZ   : altitude of the w points in the MESO-NH grid.
!!      Module MODD_PARAMETERS
!!         JPVEXT
!!
!!    REFERENCE
!!    ---------
!!
!!      Book 2
!!
!!    AUTHOR
!!    ------
!!
!!      V.Masson  Meteo-France
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    12/12/94
!!                  06/03/96 (V. Masson) conservation of relative humidity
!!                           instead of shift of Rv.
!!                  20/08/96 (V. Masson) definition of thetav
!!                  28/10/96 (V. Masson) bug in first call to rms_at_z
!!                  29/10/96 (V. Masson) add positivity control on rv
!!                  10/06/97 (V. Masson) add non-hydrostatic pressure
!!                  26/08/97 (V. Masson) call to new linear vertical
!!                                       interpolation routine
!!                  26/01/98 (J. Stein)  add the LS fields' treatment
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODE_THERMO           ! executive module
USE MODE_FM
USE MODI_FREE_ATM_PROFILE ! interface module
USE MODI_VER_SHIFT
USE MODI_COEF_VER_INTERP_LIN
USE MODI_VER_INTERP_LIN
USE MODI_SHUMAN
USE MODI_COMPUTE_EXNER_FROM_TOP
USE MODI_RMS_AT_Z
USE MODI_WATER_SUM
!
USE MODD_CONF      ! declaration modules
USE MODD_CONF_n
USE MODD_LUNIT
USE MODD_GRID_n
USE MODD_PARAMETERS
USE MODD_CST
USE MODD_VER_INTERP_LIN
!JUAN REALZ
USE MODE_MPPDB
USE MODE_EXTRAPOL
!JUAN REALZ
USE MODI_SECOND_MNH
!
IMPLICIT NONE
!
!*       0.1   Declaration of arguments
!              ------------------------
LOGICAL,                  INTENT(IN)  :: OSHIFT     ! T: vertical shift of BL (used for GRIB file data)
!                                                   ! F: no vertical shift (used for MESONH data)
REAL,   DIMENSION(:,:,:), INTENT(IN)  :: PTHV_MX    ! thetav on mixed grid
REAL,   DIMENSION(:,:,:,:), INTENT(IN):: PR_MX      ! r on mixed grid
REAL,   DIMENSION(:,:),   INTENT(IN)  :: PZS_LS     ! large scale orography
REAL,   DIMENSION(:,:),   INTENT(IN)  :: PZSMT_LS   ! large scale orography
REAL,   DIMENSION(:,:,:), INTENT(IN)  :: PZMASS_MX  ! altitude of the mass points
!                                                   ! of the mixed grid
REAL,   DIMENSION(:,:,:), INTENT(IN)  :: PZFLUX_MX  ! altitude of the flux points
!                                                   ! of the mixed grid
REAL,   DIMENSION(:,:,:), INTENT(IN)  :: PPMHP_MX   ! pressure minus hyd. pressure
REAL,   DIMENSION(:,:),   INTENT(IN)  :: PEXNTOP2D  ! top Exner function
REAL,   DIMENSION(:,:,:), INTENT(OUT) :: PTHV       ! thetav on MESO-NH grid
REAL,   DIMENSION(:,:,:,:), INTENT(OUT) :: PR       ! r on MESO-NH grid
REAL,   DIMENSION(:,:,:), INTENT(OUT) :: PPMHP      ! pressure minus hyd. pressure
REAL,                     INTENT(OUT) :: PDIAG      ! diagnostics computing time
REAL,DIMENSION(:,:,:), OPTIONAL, INTENT(IN) :: PLSTH_MX ! large scale potential temperature
REAL,DIMENSION(:,:,:), OPTIONAL, INTENT(IN) :: PLSRV_MX ! large scale vapor mixing ratios
REAL,DIMENSION(:,:,:), OPTIONAL, INTENT(OUT) :: PLSTHM  ! Large scale potential temperature
                                                   ! on mesonh grid
REAL,DIMENSION(:,:,:), OPTIONAL, INTENT(OUT) :: PLSRVM  ! Large scale vapor
                                                   ! mixing ratio on mesonh grid
!
!*       0.2   Declaration of local variables
!              ------------------------------
!
INTEGER                    ::ILUOUT0, IRESP
INTEGER                    ::IKB,IKE,IIB,IIE,IJB,IJE,IKU
INTEGER, DIMENSION(2)      ::IIJ
INTEGER                    :: IK4000
INTEGER                    ::JK
REAL                       ::ZTIME1, ZTIME2
REAL, DIMENSION(SIZE(PZMASS_MX,1),SIZE(PZMASS_MX,2),&
         SIZE(PZMASS_MX,3))  :: ZZMASS_SH        ! altitude of the mass points
!                                                ! of the shifted grid
REAL, DIMENSION(SIZE(PZMASS_MX,1),SIZE(PZMASS_MX,2),&
         SIZE(PZMASS_MX,3))  :: ZZFLUX_SH        ! altitude of the flux points
!                                                ! of the shifted grid
REAL                         :: ZTHVCLIMGR       ! gradients per default near
                                                 ! the ground for thetav
REAL, DIMENSION(SIZE(PZMASS_MX,1),SIZE(PZMASS_MX,2),              &
         SIZE(PZMASS_MX,3)):: ZEXNMASS_MX,                        &
                              ZHEXN_MX,ZHEXNMASS_MX,ZP_MX,ZTH_MX, &
                              ZRV_MX,ZT_MX,ZES_MX,ZHU_MX
!                                                ! exner functions, pressure
!                                                ! hyd. exner functions, pressure
!                                                ! theta, rv, T, saturation
!                                                ! vapor pressure and relative
!                                                ! humidity (in %)
REAL, DIMENSION(SIZE(PZMASS_MX,1),SIZE(PZMASS_MX,2),              &
         SIZE(PZMASS_MX,3)):: ZPMHPOHP_SH, ZPMHP_SH,              &
                              ZHEXNFLUX_SH, ZHEXNMASS_SH,         &
                              ZPMASS_SH, ZTV_SH, ZHU_SH
!                                                ! (pres.-hyd. pres.)/hyd. pres.
!                                                !  pres.-hyd. pres.
!                                                ! pressure minus hyd. pressure,
!                                                ! hyd. exner functions, pressure
!                                                ! Tv and relative humidity
!                                                ! (in %)
REAL, DIMENSION(SIZE(XZZ,1),SIZE(XZZ,2),SIZE(XZZ,3)) &
                             :: ZTHV_FREE, &     ! 3D arrays of profiles of free
                                ZZ_FREE          ! atmosphere thetav, and its
!                                                ! discretization in x,y,z
REAL, DIMENSION(SIZE(XZZ,1),SIZE(XZZ,2),SIZE(XZZ,3)) &
                             :: ZTHV_FREE_MX,   &! mean profile of thetav
                                ZTHV_FREE_SH     ! on mixed and shifted grids
REAL, DIMENSION(SIZE(PTHV_MX,1),SIZE(PTHV_MX,2), &
           SIZE(PTHV_MX,3))    :: ZTHV_SH        ! thetav on the shifted grid
REAL, DIMENSION(SIZE(PTHV_MX,1),SIZE(PTHV_MX,2), &
           SIZE(PTHV_MX,3),SIZE(PR,4)) :: ZR_SH  ! r on the shifted grid
REAL, DIMENSION(SIZE(XZZ,1),SIZE(XZZ,2),SIZE(XZZ,3))&
                            :: ZZMASS            ! altitude of the mass points
!                                                ! in the MESO-NH grid.
REAL, DIMENSION(SIZE(XZZ,1),SIZE(XZZ,2),SIZE(XZZ,3))&
                            :: ZHEXN             ! hyd. Exner function of the flux points
!                                                ! in the MESO-NH grid.
REAL, DIMENSION(SIZE(XZZ,1),SIZE(XZZ,2),SIZE(XZZ,3))&
                            :: ZHEXNMASS         ! hyd. Exner function of the mass points
!                                                ! in the MESO-NH grid.
REAL, DIMENSION(SIZE(XZZ,1),SIZE(XZZ,2),SIZE(XZZ,3))&
                            :: ZP                ! pressure of the mass points
!                                                ! in the MESO-NH grid.
REAL, DIMENSION(SIZE(XZZ,1),SIZE(XZZ,2),SIZE(XZZ,3))&
                            :: ZHU               ! relative humidity of the mass
!                                                ! points in the MESO-NH grid.
INTEGER                     :: JRR               ! counter for moist variables
!-------------------------------------------------------------------------------
!
CALL FMLOOK_ll(CLUOUT0,CLUOUT0,ILUOUT0,IRESP)
!
IKB=JPVEXT+1
IKE=SIZE(XZZ,3)-JPVEXT
IKU=SIZE(XZZ,3)
IIB=JPHEXT+1
IJB=JPHEXT+1
IIE=SIZE(XZZ,1)-JPHEXT
IJE=SIZE(XZZ,2)-JPHEXT
!
!
!-------------------------------------------------------------------------------
!
!
!*       1.    SHIFT OF THE HYBRID GRID
!              ------------------------
!
IF (OSHIFT) THEN
  ZZFLUX_SH(:,:,:)=VER_SHIFT(PZFLUX_MX,PZS_LS,XZS)
  ZZMASS_SH(:,:,:)=VER_SHIFT(PZMASS_MX,PZS_LS,XZS)
ELSE
  ZZFLUX_SH(:,:,:)=PZFLUX_MX
  ZZMASS_SH(:,:,:)=PZMASS_MX
END IF
!
!-------------------------------------------------------------------------------
!
!*       2.    SHIFT OF DIFFERENCE BETWEEN PRESSURE AND HYDROSTATIC PRESSURE
!              -------------------------------------------------------------
!
!*       2.1   pressure
!              --------
!
CALL COMPUTE_EXNER_FROM_TOP(PTHV_MX,PZFLUX_MX,PEXNTOP2D,ZHEXN_MX,ZHEXNMASS_MX)
!
ZP_MX(:,:,:)=PPMHP_MX(:,:,:) + XP00 * ZHEXNMASS_MX(:,:,:) ** (XCPD/XRD)

CALL EXTRAPOL('E',ZP_MX)
CALL MPPDB_CHECK3D(ZP_MX,"VER_INT_THERMO:ZP_MX",PRECISION)
!
!*       2.2   Exner function
!              --------------
!
ZEXNMASS_MX(:,:,:)= (ZP_MX(:,:,:)/XP00) ** (XRD/XCPD)
!
!*       2.3   shift
!              -----
!
ZPMHPOHP_SH(:,:,:) = PPMHP_MX(:,:,:) / (XP00*ZHEXNMASS_MX(:,:,:) ** (XCPD/XRD))
!
!-------------------------------------------------------------------------------
!
!*       3.    SHIFT OF THETAV
!              ---------------
!
!*       3.1   Computation of the shift profile
!              --------------------------------
!
ZTHVCLIMGR=3.5E-3 ! K/m
CALL FREE_ATM_PROFILE(PTHV_MX,PZMASS_MX,PZS_LS,PZSMT_LS,ZTHVCLIMGR,ZTHV_FREE,ZZ_FREE)
CALL MPPDB_CHECK3D(ZTHV_FREE,"VER_INT_THERMO:ZTHV_FREE",PRECISION)
!
!*       3.2   Computation of the value of thetav on the shifted grid
!              ------------------------------------------------------
!
CALL COEF_VER_INTERP_LIN(ZZ_FREE(:,:,:),PZMASS_MX(:,:,:))
ZTHV_FREE_MX(:,:,:)=VER_INTERP_LIN(ZTHV_FREE(:,:,:),NKLIN(:,:,:),XCOEFLIN(:,:,:))
CALL MPPDB_CHECK3D(ZTHV_FREE_MX,"VER_INT_THERMO:ZTHV_FREE_MX",PRECISION)
!

CALL COEF_VER_INTERP_LIN(ZZ_FREE(:,:,:),ZZMASS_SH(:,:,:))
ZTHV_FREE_SH(:,:,:)=VER_INTERP_LIN(ZTHV_FREE(:,:,:),NKLIN(:,:,:),XCOEFLIN(:,:,:))
CALL MPPDB_CHECK3D(ZTHV_FREE_SH,"VER_INT_THERMO:ZTHV_FREE_SH",PRECISION)
!
!
!
ZTHV_SH(:,:,:) = PTHV_MX(:,:,:) - ZTHV_FREE_MX(:,:,:) + ZTHV_FREE_SH(:,:,:)
!
!-------------------------------------------------------------------------------
!
!*       4.    SHIFT OF RELATIVE HUMIDITY
!              --------------------------
!
!*       4.1   Computation of relative humidity on the mixed grid
!              --------------------------------------------------
!
ZRV_MX(:,:,:)=MAX(PR_MX(:,:,:,1),1.E-10)
ZTH_MX(:,:,:)=PTHV_MX(:,:,:)*(1.+WATER_SUM(PR_MX(:,:,:,:)))/(1.+XRV/XRD*ZRV_MX(:,:,:))
ZT_MX(:,:,:)=ZTH_MX(:,:,:)*ZEXNMASS_MX(:,:,:)
ZES_MX(:,:,:)=SM_FOES(ZT_MX(:,:,:))
ZHU_MX(:,:,:)=100.*ZP_MX(:,:,:)/(XRD/XRV/ZRV_MX(:,:,:)+1.)/ZES_MX(:,:,:)
!
!*       4.2   Computation of the relative humidity on the shifted grid
!              --------------------------------------------------------
!
ZHU_SH(:,:,:) = ZHU_MX(:,:,:)
!
!*       4.3   Control of extrema
!              ------------------
!
ZHU_SH(:,:,:)=MIN(MAX(ZHU_SH(:,:,:),0.),100.)
!
!*       4.4   Computation of pressure on the shifted grid
!              -------------------------------------------
!
CALL COMPUTE_EXNER_FROM_TOP(ZTHV_SH,ZZFLUX_SH,PEXNTOP2D,ZHEXNFLUX_SH,ZHEXNMASS_SH)
CALL EXTRAPOL('E',ZHEXNMASS_SH)
!
ZPMASS_SH(:,:,:)= (ZPMHPOHP_SH(:,:,:)+1.) * XP00 * ZHEXNMASS_SH(:,:,:)**(XCPD/XRD)
CALL EXTRAPOL('E',ZPMASS_SH)
CALL MPPDB_CHECK3D(ZPMASS_SH,"VER_INT_THERMO:ZPMASS_SH",PRECISION)
!
!
ZPMHP_SH(:,:,:) = ZPMASS_SH(:,:,:) - XP00 * ZHEXNMASS_SH(:,:,:) ** (XCPD/XRD)
CALL MPPDB_CHECK3D(ZPMHP_SH,"VER_INT_THERMO:ZPMHP_SH",PRECISION)
!
!
!-------------------------------------------------------------------------------
!
!*       5.    SHIFT OF OTHER MIXING RATIOS
!              ----------------------------
!
!*       5.1   Shift
!              -----
!
IF (NRR > 1) ZR_SH(:,:,:,2:NRR) = PR_MX(:,:,:,2:NRR)
!
!*       5.2   Error control
!              -------------
!
CALL SECOND_MNH(ZTIME1)
!
IF (NVERB>4) THEN
  ZTV_SH(:,:,:)=ZTHV_SH(:,:,:)*(ZPMASS_SH(:,:,:)/XP00)**(XRD/XCPD)
  ZR_SH(:,:,:,1)=SM_PMR_HU(CLUOUT0,ZPMASS_SH(:,:,:),ZTV_SH(:,:,:),ZHU_SH(:,:,:),&
                           ZR_SH(:,:,:,:),KITERMAX=100)
  CALL RMS_AT_Z(PTHV_MX/(1.+XRV/XRD*PR_MX(:,:,:,1))*(1.+WATER_SUM(PR_MX(:,:,:,:))),          &
                    PZS_LS,PZMASS_MX,                                                        &
                    ZTHV_SH/(1.+XRV/XRD*ZR_SH(:,:,:,1))*(1.+WATER_SUM(ZR_SH(:,:,:,:))),      &
                    XZS,ZZMASS_SH,'RMS on theta between mixed and shifted grid (K):                                ')
  CALL RMS_AT_Z(ZP_MX,PZS_LS,PZMASS_MX,ZPMASS_SH,XZS,ZZMASS_SH, &
               'RMS on pressure between mixed and shifted grid (Pa):                            ')
END IF
!
CALL SECOND_MNH(ZTIME2)
PDIAG = ZTIME2 - ZTIME1
!-------------------------------------------------------------------------------
!
!*       6.    INTERPOLATION OF THETAV AND R ON THE MESO-NH GRID
!              -------------------------------------------------
!
!*       6.1   Altitude of the mass points on the MESO-NH grid
!              -----------------------------------------------
!
ZZMASS(:,:,:)=MZF(1,IKU,1,XZZ(:,:,:))
ZZMASS(:,:,SIZE(XZZ,3))=1.5*XZZ(:,:,SIZE(XZZ,3))-0.5*XZZ(:,:,SIZE(XZZ,3)-1)
!
!*       6.2   Interpolation on the MESO-NH grid
!              ---------------------------------
!
CALL COEF_VER_INTERP_LIN(ZZMASS_SH(:,:,:),ZZMASS(:,:,:))
!
!
PPMHP(:,:,:)=VER_INTERP_LIN(ZPMHP_SH(:,:,:),NKLIN(:,:,:),XCOEFLIN(:,:,:))
CALL EXTRAPOL('E',PPMHP)
CALL MPPDB_CHECK3D(PPMHP,"VER_INT_THERMO:PPMHP",PRECISION)

PTHV (:,:,:)=VER_INTERP_LIN(ZTHV_SH(:,:,:),NKLIN(:,:,:),XCOEFLIN(:,:,:))
CALL MPPDB_CHECK3D(PTHV,"VER_INT_THERMO:PTHV",PRECISION)

ZHU  (:,:,:)=VER_INTERP_LIN(ZHU_SH (:,:,:),NKLIN(:,:,:),XCOEFLIN(:,:,:))
ZHU  (:,:,:)=MIN(MAX(ZHU(:,:,:),0.),100.)
DO JRR=2,NRR
  PR(:,:,:,JRR)=VER_INTERP_LIN(ZR_SH(:,:,:,JRR),NKLIN(:,:,:),XCOEFLIN(:,:,:))
  PR(:,:,:,JRR)=MAX(PR(:,:,:,JRR),0.)
END DO
!
!*       6.3   Computation of water vapor mixing ratio
!              ---------------------------------------
!
CALL COMPUTE_EXNER_FROM_TOP(PTHV,XZZ,PEXNTOP2D,ZHEXN,ZHEXNMASS)
ZP(:,:,:) = PPMHP(:,:,:) + XP00 * ZHEXNMASS(:,:,:) ** (XCPD/XRD)
!
PR(:,:,:,1)=SM_PMR_HU(CLUOUT0,ZP(:,:,:),                        &
                      PTHV(:,:,:)*(ZP(:,:,:)/XP00)**(XRD/XCPD), &
                      ZHU(:,:,:),PR(:,:,:,:),KITERMAX=100)
!
!*       6.4   Interpolate the Large Scale fields
!              ----------------------------------
!
IF ( PRESENT(PLSTH_MX) ) THEN
!
  CALL COEF_VER_INTERP_LIN(PZMASS_MX(:,:,:),ZZMASS(:,:,:))
  PLSTHM(:,:,:)=VER_INTERP_LIN(PLSTH_MX(:,:,:),NKLIN(:,:,:),XCOEFLIN(:,:,:))
  PLSRVM(:,:,:)=VER_INTERP_LIN(PLSRV_MX(:,:,:),NKLIN(:,:,:),XCOEFLIN(:,:,:))
!
END IF
!
!
!*       6.5   Prints
!              ------
!
IF (NVERB>=1) THEN
  IK4000 = COUNT(XZHAT(:)<4000.)
  IIJ = MAXLOC(        SUM(ZHU_MX(IIB:IIE,IJB:IJE,JPVEXT+1:IK4000),3),                  &
                MASK=COUNT(ZHU_MX(IIB:IIE,IJB:IJE,JPVEXT+1:IKE)                         &
                           >=MAXVAL(ZHU_MX(IIB:IIE,IJB:IJE,JPVEXT+1:IKE))-0.01,DIM=3 )  &
                      >=1                                                   )           &
        + JPHEXT
  WRITE(ILUOUT0,*) ' '
  WRITE(ILUOUT0,*) 'Altitude and humidity on shifted grid     (I=',IIJ(1),';J=',IIJ(2),')'
  DO JK=IKB,IKE
    WRITE(ILUOUT0,'(6Hlevel ,F6.0,5H m : ,F6.2,2H %)') ZZMASS_SH(IIJ(1),IIJ(2),JK),ZHU_SH(IIJ(1),IIJ(2),JK)
  END DO
  !
  WRITE(ILUOUT0,*) ' '
  WRITE(ILUOUT0,*) 'Altitude and humidity on MESO-NH grid     (I=',IIJ(1),';J=',IIJ(2),')'
  DO JK=IKB,IKE
    WRITE(ILUOUT0,'(6Hlevel ,F6.0,5H m : ,F6.2,2H %)') ZZMASS   (IIJ(1),IIJ(2),JK),ZHU   (IIJ(1),IIJ(2),JK)
  END DO
END IF
!
!
!*       6.6   Error control
!              -------------
!
IF (NVERB<5) RETURN
!
CALL SECOND_MNH(ZTIME1)
!
CALL RMS_AT_Z(ZTHV_SH/(1.+XRV/XRD*ZR_SH(:,:,:,1))*(1.+WATER_SUM(ZR_SH(:,:,:,:))),    &
                  XZS,ZZMASS_SH,                                                     &
                  PTHV/(1.+XRV/XRD*PR(:,:,:,1))*(1.+WATER_SUM(PR(:,:,:,:))),         &
                  XZS,ZZMASS,                                                        &
                  'RMS on theta between shifted and MESO-NH grid (K):                              ')
!
!*       6.7   Error control on pressure
!              -------------------------
!
CALL RMS_AT_Z(ZPMASS_SH,XZS,ZZMASS_SH,ZP,XZS,ZZMASS,                      &
             'RMS on pressure between shifted and MESO-NH grid (Pa):                          ')
!
CALL SECOND_MNH(ZTIME2)
PDIAG = PDIAG + ZTIME2 - ZTIME1
!
!-------------------------------------------------------------------------------
!
WRITE(ILUOUT0,*) 'Routine VER_INT_THERMO completed'
!
END SUBROUTINE VER_INT_THERMO
