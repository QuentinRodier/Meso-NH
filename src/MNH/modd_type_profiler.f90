!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! MASDEV4_7 modd 2006/06/27 12:27:06
!-----------------------------------------------------------------
!     ############################
      MODULE MODD_TYPE_PROFILER
!     ############################
!
!!****  *MODD_PROFILER* - declaration of stations
!!
!!    PURPOSE
!!    -------
!       The purpose of this declarative module is to define
!      the different stations types.
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      NONE 
!!
IMPLICIT NONE
!!
!!    REFERENCE
!!    --------- 
!!       
!!    AUTHOR
!!    ------
!!	P. Tulet   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    15/01/02
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
!
!
TYPE PROFILER
!
!
!* general information
!
!
!* storage monitoring
!
REAL                          :: T_CUR  ! current time since last storage
INTEGER                       :: N_CUR  ! current step of storage
REAL                          :: STEP   ! storage time step
!
!* data records
!
CHARACTER(LEN=8),DIMENSION(:), POINTER   :: NAME=>NULL()   ! station name
CHARACTER(LEN=8),DIMENSION(:), POINTER   :: TYPE=>NULL()   ! station type
!
REAL, DIMENSION(:),       POINTER :: TIME=>NULL()     ! t(n)  (n: recording instants)
LOGICAL, DIMENSION(:),    POINTER :: ERROR=>NULL()  
REAL, DIMENSION(:),       POINTER :: X=>NULL()        ! X(n)
REAL, DIMENSION(:),       POINTER :: Y=>NULL()        ! Y(n)
REAL, DIMENSION(:),       POINTER :: LON=>NULL()      ! longitude(n)
REAL, DIMENSION(:),       POINTER :: LAT=>NULL()      ! latitude (n)
REAL, DIMENSION(:,:,:),   POINTER :: ZON=>NULL()      ! zonal wind(n)
REAL, DIMENSION(:,:,:),   POINTER :: MER=>NULL()      ! meridian wind(n)
REAL, DIMENSION(:,:,:),   POINTER :: FF=>NULL()       ! wind intensity  
REAL, DIMENSION(:,:,:),   POINTER :: DD=>NULL()       ! wind direction
REAL, DIMENSION(:,:,:),   POINTER :: W=>NULL()        ! w(n)  (air vertical speed)
REAL, DIMENSION(:,:,:),   POINTER :: P=>NULL()        ! p(n)
REAL, DIMENSION(:,:,:),   POINTER :: TKE=>NULL()      ! tke(n)
REAL, DIMENSION(:,:,:),   POINTER :: TH=>NULL()       ! th(n)
REAL, DIMENSION(:,:,:),   POINTER :: THV=>NULL()      ! thv(n)
REAL, DIMENSION(:,:,:),   POINTER :: RARE=>NULL()     ! radar reflectivity (n)
REAL, DIMENSION(:,:,:,:), POINTER :: R=>NULL()        ! r*(n)
REAL, DIMENSION(:,:,:,:), POINTER :: SV=>NULL()       ! Sv*(n)
REAL, DIMENSION(:,:,:,:), POINTER :: AER=>NULL()      ! AER*(n) aerosol extinction
REAL, DIMENSION(:,:),     POINTER :: DATIME=>NULL()   ! record for diachro
!
REAL, DIMENSION(:,:),     POINTER :: T2M=>NULL()      ! 2 m air temperature (°C)
REAL, DIMENSION(:,:),     POINTER :: Q2M=>NULL()      ! 2 m humidity (kg/kg)
REAL, DIMENSION(:,:),     POINTER :: HU2M=>NULL()     ! 2 m relative humidity (%)
REAL, DIMENSION(:,:),     POINTER :: ZON10M=>NULL()   ! 10 m zonal wind (m/s)  
REAL, DIMENSION(:,:),     POINTER :: MER10M=>NULL()   ! 10 m merid. wind (m/s)
REAL, DIMENSION(:,:),     POINTER :: RN=>NULL()       ! net radiation (W m2)
REAL, DIMENSION(:,:),     POINTER :: H=>NULL()        ! sensible heat flux (W m2)
REAL, DIMENSION(:,:),     POINTER :: LE=>NULL()       ! Total latent heat flux (W m2)
REAL, DIMENSION(:,:),     POINTER :: LEI=>NULL()      ! Solid latent heat flux (W m2)
REAL, DIMENSION(:,:),     POINTER :: GFLUX=>NULL()    ! storage heat flux (W m2)
REAL, DIMENSION(:,:),     POINTER :: LW=>NULL()       ! IR downward radiation (W m2)
REAL, DIMENSION(:,:),     POINTER :: SW=>NULL()       ! solar downward radiation (W m2)
!
REAL, DIMENSION(:,:,:),   POINTER :: TKE_DISS=>NULL() ! TKE dissipation rate
!
!
END TYPE PROFILER
!
END MODULE MODD_TYPE_PROFILER

