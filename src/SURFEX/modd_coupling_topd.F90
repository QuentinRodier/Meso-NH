!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ###########################
      MODULE MODD_COUPLING_TOPD
!     ###########################
!
!!****  *MODD_COUPLING_TOPD - declaration of exchanged variables from Topodyn to ISBA
!!
!!    PURPOSE
!!    -------
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      None 
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!     F. Habets and K. Chancibault
!!
!!    MODIFICATIONS
!!    -------------
!!     Original 29/09/03
!!              03/2014 (B. Vincendon) new variable to create a mask for N patches
!!              07/2017 (B. Vincendon) changing name of variable to ditinguish between 
!!                                     packed and full grid variables + new variables 
!!                                     for runoff management
!
!*       0.   DECLARATIONS
!             ------------
USE MODD_TOPD_PAR, ONLY : JPCAT
!
IMPLICIT NONE
!
!-------------------------------------------------------------------------------
!
LOGICAL                           :: LCOUPL_TOPD      !if T, performs coupling with Topmodel
LOGICAL                           :: LBUDGET_TOPD     !if T, computes budget
LOGICAL                           :: LTOPD_STEP
LOGICAL                           :: LPERT_PARAM
LOGICAL                           :: LPERT_INIT
!
INTEGER                             :: NTOPD_STEP
INTEGER                             :: NFREQ_MAPS_WG       !frequency of output WG maps
INTEGER                             :: NFREQ_MAPS_ASAT     !frequency of output ASAT maps
INTEGER                             :: NFREQ_MAPS_RUNOFF   !frequency of output RUNOFF maps
!
INTEGER                             :: NNB_TOPD   ! Ratio between Time steps of Topmodel and ISBA
!
INTEGER                             :: NIMAX     ! number of ISBA grid points on 
                                                 ! abscissa axis
INTEGER                             :: NJMAX     ! number of ISBA grid points on ordinate 
                                                 ! axis
REAL, ALLOCATABLE, DIMENSION(:)     :: XXI ! Extended Lambert II coordinates of Isba 
REAL, ALLOCATABLE, DIMENSION(:)     :: XYI ! nodes 
!
INTEGER, ALLOCATABLE, DIMENSION(:)     :: NNPIX     ! Number of Topmodel pixels in an ISBA mesh
INTEGER, ALLOCATABLE, DIMENSION(:,:,:) :: NMASKI ! pixel number of each catchment in each isba mesh
INTEGER, ALLOCATABLE, DIMENSION(:,:)   :: NMASKT    ! mask
INTEGER, ALLOCATABLE, DIMENSION(:)     :: NMASKT_PATCH    ! mask
!
REAL, ALLOCATABLE, DIMENSION(:)     :: XAS_NATURE   ! Packed contributive area fraction on Nature grid
REAL, ALLOCATABLE, DIMENSION(:,:)   :: XAS_IBV_P   ! Packed contributive area fraction on Nature grid by catchment
REAL, ALLOCATABLE, DIMENSION(:,:)   :: XAIBV_F     ! Unpacked area fraction of each catchment on Full grid
REAL, ALLOCATABLE, DIMENSION(:)     :: XATOP        ! Unpacked area fraction of all cacthments on Full grid
REAL, ALLOCATABLE, DIMENSION(:)     :: XATOP_NATURE ! Packed area fraction of all cacthments  on Nature grid
!
INTEGER, ALLOCATABLE, DIMENSION(:,:)  :: NNBV_IN_MESH   ! Number of pixel of a partical cathment in an ISBA mesh
REAL, ALLOCATABLE, DIMENSION(:,:)     :: XBV_IN_MESH    ! Area of the ISBA meshes covered by a partical cathment
REAL, ALLOCATABLE, DIMENSION(:)       :: XTOTBV_IN_MESH ! Area of the ISBA meshes covered by all cathments
!
REAL, ALLOCATABLE, DIMENSION(:)     :: XDTOPI   ! depth of the soil for lateral 
                                                ! distribution on ISBA grid (m)
REAL, ALLOCATABLE, DIMENSION(:,:)   :: XDTOPT   ! depth of the Isba soil on TOP-LAT 
                                                ! grid (m)
!
REAL, ALLOCATABLE, DIMENSION(:)     :: XWG_FULL   ! Water content from Isba on the full domain
REAL, ALLOCATABLE, DIMENSION(:,:)   :: XWGT     ! ISBA water content 
!
REAL, ALLOCATABLE, DIMENSION(:)     :: XWSTOPI  ! total water content at saturation (m3/m3)
                                                ! on XDTOPI on ISBA grid
REAL, ALLOCATABLE, DIMENSION(:,:)   :: XWSTOPT  ! total water content at saturation (m3/m3)
                                                ! on XDTOPT on TOP-LAT grid
REAL, ALLOCATABLE, DIMENSION(:)     :: XWFCTOPI ! total field capacity on XDTOPI (m3/m3)
REAL, ALLOCATABLE, DIMENSION(:,:)   :: XWFCTOPT ! total field capacity on XDTOPT (m3/m3)
REAL, ALLOCATABLE, DIMENSION(:)     :: XWWTOPI  ! hydraulic conductivity at saturation on 
                                                ! Isba grid, on XDTOPI
REAL, ALLOCATABLE, DIMENSION(:,:)   :: XWWTOPT  
REAL, ALLOCATABLE, DIMENSION(:,:)   :: XWTOPT   ! water storage on TOP-LAT grid, after
                                                ! lateral distribution
REAL, ALLOCATABLE, DIMENSION(:,:)   ::  XRUNOFF_IBV_P! Runoff by mesh and catchment on isba grid
REAL, ALLOCATABLE, DIMENSION(:)       :: XWOVSATI_P  ! Volume of water above saturation buy mesh on isba grid
!
! * pour bilans
REAL, ALLOCATABLE, DIMENSION(:)       :: XAVG_RUNOFFCM !cumulated runoff  (kg/m2) at t-dt
REAL, ALLOCATABLE, DIMENSION(:)       :: XAVG_DRAINCM ! cumulated drainage calculated from Isba (kg/m2) at t-dt
REAL, ALLOCATABLE, DIMENSION(:)       :: XRAINFALLCM ! cumulated rainfall calculated from Isba (kg/m2) at t-dt
REAL, ALLOCATABLE, DIMENSION(:)       :: XAVG_HORTCM ! cumulated Horton calculated from Isba (kg/m2) at t-dt
!
REAL, ALLOCATABLE, DIMENSION(:,:)     :: XKA_PRE   ! Hydrological indexes at the previous time step
REAL, ALLOCATABLE, DIMENSION(:)       :: XKAC_PRE  ! Hydrological index at saturation at the previous time step
!
REAL, ALLOCATABLE, DIMENSION(:,:)     :: XDMAXFC   ! Deficit at the field capacity level
!
REAL, ALLOCATABLE, DIMENSION(:)      ::  XDRAIN_TOP ! Value of drainage on TOPMODEL grid
!
REAL, ALLOCATABLE, DIMENSION(:)      ::  XFRAC_D2 ! fraction of the second layer concerned with lateral transferts
REAL, ALLOCATABLE, DIMENSION(:)      ::  XFRAC_D3 ! fraction of the third layer concerned with lateral transferts
!
REAL, ALLOCATABLE, DIMENSION(:)      ::  XWGI_FULL ! soil ice content
!
REAL, ALLOCATABLE, DIMENSION(:,:)    :: XRUN_TOROUT,XDR_TOROUT
!
LOGICAL                              :: LSTOCK_TOPD ! true to stock runoff and drainage values (for another simulation)
!
INTEGER                              :: NNB_STP_RESTART ! number of time step to restart from a previous simulation
INTEGER                              :: NNB_STP_STOCK   ! number of time step to write for the next simulation
!
INTEGER, DIMENSION(:), ALLOCATABLE :: NYEAR      ! Year of the beginning of the simulation.
INTEGER, DIMENSION(:), ALLOCATABLE :: NMONTH     ! Month of the beginning of the simulation.
INTEGER, DIMENSION(:), ALLOCATABLE :: NDAY      ! Date of the beginning of the simulation.
INTEGER, DIMENSION(:), ALLOCATABLE :: NH      ! Hour of the bFginning of the simulation.
INTEGER, DIMENSION(:), ALLOCATABLE :: NM      ! Minutes of the beginning of the simulation.
!
! **** For special f, dc exponential profile
REAL, DIMENSION(:), ALLOCATABLE :: XF_PARAM
REAL, DIMENSION(:), ALLOCATABLE :: XC_DEPTH_RATIO
!
! **** For sub-catchments 
LOGICAL                           :: LDUMMY_SUBCAT    !if T, dummy sub-catchments defined
LOGICAL                           :: LSUBCAT          !if T, sub-catchments will be computed
INTEGER, DIMENSION(JPCAT)         :: NSUBCAT
REAL,    DIMENSION(JPCAT,JPCAT)   :: XLX,XLY
REAL,    DIMENSION(JPCAT,JPCAT)   :: XQ2,XQ10,XQ50
CHARACTER(LEN=15), DIMENSION(JPCAT,JPCAT):: CSUBCAT         ! Names of catchments         
CHARACTER(LEN=15), DIMENSION(JPCAT):: CFILE_SUBCAT         ! File containing Sub cat information         
LOGICAL                           :: LWRITE_SEVERITY_MAPS !if T, severity maps  will be computed
!
END MODULE MODD_COUPLING_TOPD
