!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     ######################
      MODULE MODD_PACK_CH_ISBA
!     ######################
!
!!****  *MODD_PACK_CH_ISBA - declaration of packed surface parameters for chemistry
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
!!	A. Boone   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original       20/09/02
!
!*       0.   DECLARATIONS
!             ------------
!
!
IMPLICIT NONE
!-------------------------------------------------------------------------------
!
REAL, ALLOCATABLE, DIMENSION(:,:), TARGET :: XBLOCK_SIMPLE
!$OMP THREADPRIVATE(XBLOCK_SIMPLE)
!
REAL, POINTER, DIMENSION(:)    :: XP_SOILRC_SO2 ! for SO2 deposition
!$OMP THREADPRIVATE(XP_SOILRC_SO2)
REAL, POINTER, DIMENSION(:)    :: XP_SOILRC_O3  ! for SO2 deposition
!$OMP THREADPRIVATE(XP_SOILRC_O3)
REAL, ALLOCATABLE, DIMENSION(:,:)  :: XP_DEP        ! deposition velocity
!$OMP THREADPRIVATE(XP_DEP)
!
!-------------------------------------------------------------------------------
!
END MODULE MODD_PACK_CH_ISBA

