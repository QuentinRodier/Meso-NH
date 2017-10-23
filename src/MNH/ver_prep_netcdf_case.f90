!     ################################
      MODULE MODI_VER_PREP_NETCDF_CASE
!     ################################
INTERFACE
      SUBROUTINE VER_PREP_NETCDF_CASE(PDIAG)
!
REAL, INTENT(OUT)                 :: PDIAG    ! diagnostics computing time
!
END SUBROUTINE VER_PREP_NETCDF_CASE
END INTERFACE
END MODULE MODI_VER_PREP_NETCDF_CASE
!     ####################################################################
      SUBROUTINE VER_PREP_NETCDF_CASE(PDIAG)
!     ####################################################################
!
!!****  *VER_PREP_NETCDF_CASE* - monitors the preparation to orographic change
!!
!!    PURPOSE
!!    -------
!!    This routine monitors the preparation of variables to future change
!!    of orography, according to the type of input file.
!!
!!**  METHOD
!!    ------
!!
!!
!!    EXTERNAL
!!    --------
!!
!!    function MZF
!!    function FMLOOK  :to retrieve a logical unit number associated with a file
!!    routine VER_INTERP_TO_MIXED_GRID
!!    routine CHANGE_GRIBEX_VAR
!!
!!    module MODI_SHUMAN
!!    module MODI_VER_INTERP_TO_MIXED_GRID
!!    module MODI_CHANGE_GRIBEX_VAR
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!      Module MODD_CONF1     : contains configuration variables for all models.
!!         NVERB      : verbosity level for output-listing
!!      Module MODD_LUNIT     :  contains logical unit names for all models
!!         CLUOUT0 : name of output-listing
!!      Module MODD_CST       : contains physical constants
!!         XRD : gas constant for dry air
!!         XRV : gas constant for vapor
!!         XP00: reference pressure
!!         XCPD: specific heat for dry air
!!         XG  : gravity constant
!!         XRADIUS : earth radius
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
!!      Original    14/12/94
!!                  Jan, 31 1996 (V. Masson) duplication of the routine
!!                               to accept different input fields
!!                  May, 25 1996 (V. Masson) take into account the upper level
!!                  Aug, 20 1996 (V. Masson) correction on theta
!!                  Oct, 20 1996 (V. Masson) add deallocations
!!                  Dec, 06 1996 (V. Masson) add air temperature at ground
!!                  Dec, 12 1996 (V. Masson) add vertical wind velocity
!!                  May, 07 1997 (V. Masson) add null tke
!!                  Jun, 10 1997 (V. Masson) add null difference between
!!                                           pressure and hydrostatic pressure
!!                  Jul, 11 1997 (V. Masson) add null scalar variables
!!                  Nov, 22 2000 (I. Mallet) add scalar variables
!!                  Nov, 22 2000 (P. Jabouille) change routine name
!!                  May 2006                 Remove EPS
!!                  Oct 2017 (J.Escobar) minor, missing USE MODI_SECOND_MNH
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODE_THERMO
USE MODE_FM
!
USE MODI_SHUMAN         ! interface modules
USE MODI_CHANGE_GRIBEX_VAR
USE MODI_VER_INTERP_TO_MIXED_GRID
USE MODI_RMS_AT_Z
USE MODI_COMPUTE_EXNER_FROM_TOP
USE MODI_WATER_SUM
USE MODI_SECOND_MNH
!
USE MODD_CONF           ! declaration modules
USE MODD_CONF_n
USE MODD_LUNIT
USE MODD_CST
USE MODD_PREP_REAL
USE MODD_PARAMETERS, ONLY : JPVEXT, XUNDEF
!
IMPLICIT NONE
!
!*       0.1   Declaration of arguments
!              ------------------------
!
REAL, INTENT(OUT)                 :: PDIAG    ! diagnostics computing time
!
!*       0.2   Declaration of local variables
!              ------------------------------
INTEGER                            :: IRESP, ILUOUT0
INTEGER                            :: IIU,IJU,ILU
REAL                               :: ZTIME1, ZTIME2
REAL,DIMENSION(:,:,:), ALLOCATABLE :: ZTH_LS    ! potential temperature
REAL,DIMENSION(:,:,:), ALLOCATABLE :: ZTH_MX    ! potential temperature
REAL,DIMENSION(:,:,:), ALLOCATABLE :: ZPMASS_MX    ! pressure
REAL,DIMENSION(:,:,:), ALLOCATABLE :: ZHEXNFLUX_MX ! pressure function
REAL,DIMENSION(:,:,:), ALLOCATABLE :: ZHEXNMASS_MX ! pressure function
!
REAL,DIMENSION(:,:,:), ALLOCATABLE :: ZZFLUX_LS
REAL,DIMENSION(:,:,:), ALLOCATABLE :: ZZMASS_LS
REAL,DIMENSION(:,:,:), ALLOCATABLE :: ZPMHP_LS
REAL,DIMENSION(:,:,:), ALLOCATABLE :: ZTHV_LS
REAL,DIMENSION(:,:,:,:),ALLOCATABLE:: ZR_LS
REAL,DIMENSION(:,:,:,:),ALLOCATABLE:: ZSV_LS
REAL,DIMENSION(:,:,:), ALLOCATABLE :: ZHU_LS
REAL,DIMENSION(:,:,:), ALLOCATABLE :: ZU_LS
REAL,DIMENSION(:,:,:), ALLOCATABLE :: ZV_LS
REAL,DIMENSION(:,:,:), ALLOCATABLE :: ZW_LS
REAL,DIMENSION(:,:,:), ALLOCATABLE :: ZTKE_LS
INTEGER                            :: JRR     ! loop counter
INTEGER                            :: JSV     ! loop counter
INTEGER                            :: JK      ! loop counter
!-------------------------------------------------------------------------------
!
CALL FMLOOK_ll(CLUOUT0,CLUOUT0,ILUOUT0,IRESP)
!
!*       1.    CHANGING OF VARIABLES
!              ---------------------
!
  IIU=SIZE(XT_SV_LS,1)
  IJU=SIZE(XT_SV_LS,2)
  ILU=SIZE(XT_SV_LS,3)
!
!
  ALLOCATE(XPMASS_SV_LS(IIU,IJU,ILU))
  ALLOCATE(XZMASS_SV_LS(IIU,IJU,ILU),XZFLUX_SV_LS(IIU,IJU,ILU))
  ALLOCATE(XTHV_SV_LS(IIU,IJU,ILU),XR_SV_LS(IIU,IJU,ILU,NRR),XHU_SV_LS(IIU,IJU,ILU))
  CALL CHANGE_GRIBEX_VAR(XA_SV_LS,XB_SV_LS,XP00_SV_LS,XPS_SV_LS,XZS_SV_LS, &
                         XT_SV_LS,XQ_SV_LS,XPMASS_SV_LS,XZFLUX_SV_LS,XZMASS_SV_LS, &
                         XTHV_SV_LS,XR_SV_LS,XHU_SV_LS                      )
!
!-------------------------------------------------------------------------------
!
!*       2.    INTERPOLATION TO MIXED GRID AND DIAGNOSTIC VARIABLES
!              ----------------------------------------------------
!* Add extra points below and above grids, in order to use MESONH linear
! vertical interpolation programs with all ILU physical points
!
ALLOCATE(ZZMASS_LS(IIU,IJU,ILU+2*JPVEXT))
ALLOCATE(ZSV_LS(IIU,IJU,ILU+2*JPVEXT,SIZE(XSV_LS,4)))
!
ZZMASS_LS (:,:,JPVEXT+1:JPVEXT+ILU) = XZMASS_SV_LS(:,:,:)
DO JK=1,JPVEXT
   ZZMASS_LS(:,:,           JK) = XZMASS_SV_LS(:,:,1)   - (XZMASS_SV_LS(:,:,2)  -XZMASS_SV_LS(:,:,1)    )*(JPVEXT+1-JK)
   ZZMASS_LS(:,:,ILU+JPVEXT+JK) = XZMASS_SV_LS(:,:,ILU) + (XZMASS_SV_LS(:,:,ILU)-XZMASS_SV_LS(:,:,ILU-1))*          JK
END DO
!
!ZSV_LS    = XUNDEF
ZSV_LS    = -999.
!
DO JSV=1,SIZE(XSV_LS,4)
  ZSV_LS    (:,:,JPVEXT+1:JPVEXT+ILU,JSV) = XSV_LS (:,:,:,JSV)
END DO
!
  CALL VER_INTERP_TO_MIXED_GRID('CHEM',.TRUE.,XZS_SV_LS,XZS_SV_LS,&
                                ZZMASS_LS,ZSV_LS                 )
!
DEALLOCATE(ZZMASS_LS)
DEALLOCATE(ZSV_LS)
!-------------------------------------------------------------------------------
!
!*       3.    ERROR CONTROL
!              -------------
!
CALL SECOND_MNH(ZTIME1)
PDIAG = ZTIME2 - ZTIME1
!
!-------------------------------------------------------------------------------
!
!*       4.    DEALLOCATIONS
!              -------------
!
  DEALLOCATE(XA_SV_LS)
  DEALLOCATE(XB_SV_LS)
  DEALLOCATE(XT_SV_LS)
  DEALLOCATE(XQ_SV_LS)
  DEALLOCATE(XZMASS_SV_LS)
  DEALLOCATE(XZFLUX_SV_LS)
  DEALLOCATE(XTHV_SV_LS)
  DEALLOCATE(XR_SV_LS)
  DEALLOCATE(XHU_SV_LS)
  DEALLOCATE(XSV_LS)
!
!
!-------------------------------------------------------------------------------
!
WRITE(ILUOUT0,*) 'Routine VER_PREP_NETCDF_CASE completed'
!
END SUBROUTINE VER_PREP_NETCDF_CASE
