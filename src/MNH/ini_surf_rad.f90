!MNH_LIC Copyright 1994-2018 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!     ##########################
      MODULE MODI_INI_SURF_RAD
!     ##########################
!
INTERFACE
!
    SUBROUTINE INI_SURF_RAD(TPINIFILE, PDIR_ALB, PSCA_ALB, PEMIS, PTSRAD)
!
USE MODD_IO_ll, ONLY : TFILEDATA
!
TYPE(TFILEDATA),        INTENT(IN)  :: TPINIFILE ! Initial file
REAL, DIMENSION(:,:,:), INTENT(OUT) :: PDIR_ALB  ! Direct albedo
REAL, DIMENSION(:,:,:), INTENT(OUT) :: PSCA_ALB  ! Diffuse albedo
REAL, DIMENSION(:,:),   INTENT(OUT) :: PEMIS     ! emissivity
REAL, DIMENSION(:,:),   INTENT(OUT) :: PTSRAD    ! radiative surface temperature
!
END SUBROUTINE INI_SURF_RAD
!
END INTERFACE
!
END MODULE MODI_INI_SURF_RAD
!
!
!   #####################################################################
    SUBROUTINE INI_SURF_RAD(TPINIFILE, PDIR_ALB, PSCA_ALB, PEMIS, PTSRAD)
!   #####################################################################
!
!!****  *INI_SURF_RAD * - initialisation for ECMWF radiation scheme in the MesoNH framework
!!
!!    PURPOSE
!!    -------
!!
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
!!    AUTHOR
!!    ------
!!      V. Masson
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    03/03/03
!!  Philippe Wautelet: 05/2016-04/2018: new data structures and calls for I/O
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_IO_ll, ONLY : TFILEDATA
!
USE MODE_FMREAD
!
IMPLICIT NONE
!
!*       0.1   Declarations of dummy arguments :
!
TYPE(TFILEDATA),        INTENT(IN)  :: TPINIFILE ! Initial file
REAL, DIMENSION(:,:,:), INTENT(OUT) :: PDIR_ALB  ! Direct albedo
REAL, DIMENSION(:,:,:), INTENT(OUT) :: PSCA_ALB  ! Diffuse albedo
REAL, DIMENSION(:,:),   INTENT(OUT) :: PEMIS     ! emissivity
REAL, DIMENSION(:,:),   INTENT(OUT) :: PTSRAD    ! radiative surface temperature
!
!*       0.2   declarations of local variables
!
!-------------------------------------------------------------------------------
!
CALL IO_READ_FIELD(TPINIFILE,'DIR_ALB',PDIR_ALB)
CALL IO_READ_FIELD(TPINIFILE,'SCA_ALB',PSCA_ALB)
CALL IO_READ_FIELD(TPINIFILE,'EMIS',PEMIS)
CALL IO_READ_FIELD(TPINIFILE,'TSRAD',PTSRAD)
!  
!-------------------------------------------------------------------------------
!
END SUBROUTINE INI_SURF_RAD
