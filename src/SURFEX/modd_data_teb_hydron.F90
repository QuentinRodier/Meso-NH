!     ##################
      MODULE MODD_DATA_TEB_HYDRO_n
!     ##################
!
!!****  *MODD_DATA_ISBA - declaration of DATA surface parameters for urban hydrology
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
!!      V. Masson  *Meteo France*  (modd_data_teb_gardenn.F90)
!!
!!    MODIFICATIONS
!!    -------------
!!      Original       05/2005
!!      K.Chancibault/A.Lemonsu 01/2016 New DATA surface parameters for urban hydrology
!
!*       0.   DECLARATIONS
!             ------------
!
!
USE MODD_SURF_PAR,   ONLY : XUNDEF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE

TYPE DATA_TEB_HYDRO_t
!-------------------------------------------------------------------------------
!
! Additional fields for urban hydrology (LURBHYDRO)
  LOGICAL                       :: LDATA_TEB_HYDRO       ! flag to indicate if user provided specific fields for ALL teb hydro fields
  REAL, POINTER, DIMENSION(:)   :: XPAR_DENS_WASTE       ! Wastewater sewer length density (-) 
  REAL, POINTER, DIMENSION(:)   :: XPAR_DENS_STORM       ! Stormwater sewer length density (-) 
  REAL, POINTER, DIMENSION(:)   :: XPAR_DSEWER           ! Waste water sewer depth  (m)
  REAL, POINTER, DIMENSION(:)   :: XPAR_WS_ROOF_MAX      ! Max capacity of surface roof water storage (mm)
  REAL, POINTER, DIMENSION(:)   :: XPAR_WS_ROAD_MAX      ! Max capacity of surface road water storage (mm)
  REAL, POINTER, DIMENSION(:)   :: XPAR_IP_SEWER         ! Parameter for parasite infiltrations into sewer (-)
  REAL, POINTER, DIMENSION(:)   :: XPAR_CONNEX           ! Impervious surfaces connexion rate to the sewer (-)
  REAL, POINTER, DIMENSION(:)   :: XPAR_INFIL_ROAD       ! Water infiltration through roads (m/s)
  REAL, POINTER, DIMENSION(:)   :: XPAR_URBDRAIN         ! Limitation of drainage (0-1)
!
!-------------------------------------------------------------------------------

END TYPE DATA_TEB_HYDRO_t



CONTAINS

!
SUBROUTINE DATA_TEB_HYDRO_INIT(YDATA_TEB_HYDRO)
TYPE(DATA_TEB_HYDRO_t), INTENT(INOUT) :: YDATA_TEB_HYDRO
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK("MODD_DATA_TEB_HYDRO_N:DATA_TEB_HYDRO_INIT",0,ZHOOK_HANDLE)
YDATA_TEB_HYDRO%LDATA_TEB_HYDRO=.FALSE.   
  NULLIFY(YDATA_TEB_HYDRO%XPAR_DENS_WASTE)
  NULLIFY(YDATA_TEB_HYDRO%XPAR_DENS_STORM)
  NULLIFY(YDATA_TEB_HYDRO%XPAR_DSEWER)
  NULLIFY(YDATA_TEB_HYDRO%XPAR_WS_ROOF_MAX)
  NULLIFY(YDATA_TEB_HYDRO%XPAR_WS_ROAD_MAX)
  NULLIFY(YDATA_TEB_HYDRO%XPAR_IP_SEWER  ) 
  NULLIFY(YDATA_TEB_HYDRO%XPAR_CONNEX    ) 
  NULLIFY(YDATA_TEB_HYDRO%XPAR_INFIL_ROAD) 
  NULLIFY(YDATA_TEB_HYDRO%XPAR_URBDRAIN  )  
IF (LHOOK) CALL DR_HOOK("MODD_DATA_TEB_HYDRO_N:DATA_TEB_HYDRO_INIT",1,ZHOOK_HANDLE)
END SUBROUTINE DATA_TEB_HYDRO_INIT


END MODULE MODD_DATA_TEB_HYDRO_n
