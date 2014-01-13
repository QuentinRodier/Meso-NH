!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     ######################
      MODULE MODD_IDEAL_FLUX
!     ######################
!
!!****  *MODD_IDEAL_FLUX * - Defines the quantities for ideal surface fluxes.
!!
!!    PURPOSE
!!    -------
!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------ 
!!
!!
!!    REFERENCE
!!    ---------
!!      
!!
!!    AUTHOR
!!    ------
!!	    V. Masson   * Meteo France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    02/2003 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
IMPLICIT NONE
!
REAL, DIMENSION(:), ALLOCATABLE   :: XTIMEF
REAL, DIMENSION(:), ALLOCATABLE   :: XTIMET
INTEGER, PARAMETER                :: NFORC_MAX = 48
INTEGER                           :: NFORCF     ! number of surface forcing instants
INTEGER                           :: NFORCT
!
REAL, DIMENSION(:), ALLOCATABLE   :: XSFTH     ! hourly data of heat surface flux        (W/m2)
REAL, DIMENSION(:), ALLOCATABLE   :: XSFTQ     ! hourly data of water vapor surface flux (kg/m2/s) or (W/m2)
REAL, DIMENSION(:,:), ALLOCATABLE :: XSFTS      ! hourly data of scalar surface flux      (kg/m2/s)
REAL, DIMENSION(:), ALLOCATABLE   :: XSFCO2    ! hourly data of CO2 surface flux         (kg/m2/s)
 CHARACTER(LEN=5)                  :: CUSTARTYPE ! type of computation for friction
                                                ! 'USTAR'
                                                ! 'Z0   '
REAL, DIMENSION(:), ALLOCATABLE   :: XUSTAR    ! hourly data of friction                 (m2/s2)
REAL                              :: XZ0        ! roughness length (m)
REAL                              :: XALB       ! albedo (-)
REAL                              :: XEMIS      ! emissivity (-)
REAL, DIMENSION(:), ALLOCATABLE   :: XTSRAD    ! radiative temperature (K)
!
!-------------------------------------------------------------------------------
!
END MODULE MODD_IDEAL_FLUX
