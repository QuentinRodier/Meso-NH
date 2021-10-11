!##################
MODULE MODD_TEB_HYDRO_PGD_n
!##################
!
!!****  *MODD_TEB_HYDRO_PGD - declaration of ISBA scheme packed surface parameters for urban hydrology
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
!!	A. Lemonsu *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original       02/2013
!!
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
USE MODD_TYPE_SNOW
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE

TYPE TEB_HYDRO_PGD_t
!-------------------------------------------------------------------------------
!
! Sewer characteristics
!
  REAL, POINTER, DIMENSION(:)    :: XDSEWER          ! Sewer depth    (m)
  REAL, POINTER, DIMENSION(:)    :: XDENS_WASTE     ! Wastewater sewer length density  (-)
  REAL, POINTER, DIMENSION(:)    :: XDENS_STORM     ! Stormwater sewer length density  (-)
  REAL, POINTER, DIMENSION(:)    :: XWS_ROOF_MAX     ! Max. capacity of surface roof water storage (mm)
  REAL, POINTER, DIMENSION(:)    :: XWS_ROAD_MAX     ! Max. capacity of surface road water storage (mm)
  REAL, POINTER, DIMENSION(:)    :: XIP_SEWER        ! Parameter for parasite infiltrations into sewer (-)
  REAL, POINTER, DIMENSION(:)    :: XCONNEX          ! Impervious surfaces fraction connexion rate to the sewer (-)
  REAL, POINTER, DIMENSION(:)    :: XDENSITY_SEWER   ! Total sewer length density (-)
  INTEGER, POINTER, DIMENSION(:) :: NLAYER_SEWER     ! Ground layer where the sewer is located (-)
  INTEGER                        :: NSTEP_OUT        ! number of run time step in output time step
  INTEGER                        :: NSTEP_FORC       ! forcing run time step loop
  INTEGER                        :: NSTEP_SURF       ! isba run time step loop
!
! Water infiltration through pavement
!
  REAL, POINTER, DIMENSION(:)    :: XINFIL_ROAD      ! Water infiltration through roads (kg/m2/s)
!
! Limitation of drainage
!
  REAL, POINTER, DIMENSION(:)   :: XURBDRAIN        ! 
!-------------------------------------------------------------------------------
!
END TYPE TEB_HYDRO_PGD_t

CONTAINS

SUBROUTINE TEB_HYDRO_PGD_INIT(YTEB_HYDRO_PGD)
TYPE(TEB_HYDRO_PGD_t), INTENT(INOUT) :: YTEB_HYDRO_PGD
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK("MODD_TEB_HYDRO_N:TEB_HYDRO_PGD_INIT",0,ZHOOK_HANDLE)
!
NULLIFY(YTEB_HYDRO_PGD%XDSEWER)
NULLIFY(YTEB_HYDRO_PGD%XDENS_WASTE)
NULLIFY(YTEB_HYDRO_PGD%XDENS_STORM)
NULLIFY(YTEB_HYDRO_PGD%XDENSITY_SEWER)
NULLIFY(YTEB_HYDRO_PGD%NLAYER_SEWER)
NULLIFY(YTEB_HYDRO_PGD%XINFIL_ROAD)
!YTEB_HYDRO_PGD%NSTEP_OUT    = 0.
!YTEB_HYDRO_PGD%NSTEP_SURF   = 0.
YTEB_HYDRO_PGD%NSTEP_OUT    = 0 
YTEB_HYDRO_PGD%NSTEP_FORC   = 0   
YTEB_HYDRO_PGD%NSTEP_SURF   = 0 
NULLIFY(YTEB_HYDRO_PGD%XWS_ROOF_MAX)
NULLIFY(YTEB_HYDRO_PGD%XWS_ROAD_MAX)
NULLIFY(YTEB_HYDRO_PGD%XIP_SEWER)
NULLIFY(YTEB_HYDRO_PGD%XCONNEX)
NULLIFY(YTEB_HYDRO_PGD%XURBDRAIN)
IF (LHOOK) CALL DR_HOOK("MODD_TEB_HYDRO_N:TEB_HYDRO_PGD_INIT",1,ZHOOK_HANDLE)
END SUBROUTINE TEB_HYDRO_PGD_INIT


END MODULE MODD_TEB_HYDRO_PGD_n
