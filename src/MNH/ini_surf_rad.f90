!MNH_LIC Copyright 2003-2021 CNRS, Meteo-France and Universite Paul Sabatier
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
USE MODD_IO, ONLY : TFILEDATA
!
TYPE(TFILEDATA),        INTENT(IN)  :: TPINIFILE ! Initial file
REAL, DIMENSION(:,:,:), INTENT(OUT) :: PDIR_ALB  ! Direct albedo
REAL, DIMENSION(:,:,:), INTENT(OUT) :: PSCA_ALB  ! Diffuse albedo
REAL, DIMENSION(:,:,:),   INTENT(OUT) :: PEMIS     ! emissivity
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
!!                   02/2018 Q.Libois ECRAD
!!  Philippe Wautelet: 05/2016-04/2018: new data structures and calls for I/O
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
use modd_field,         only: tfieldmetadata, tfieldlist
USE MODD_IO,            ONLY: TFILEDATA
!
use mode_field,         only: Find_field_id_from_mnhname
USE MODE_IO_FIELD_READ, only: IO_Field_read
USE MODE_MSG
!
IMPLICIT NONE
!
!*       0.1   Declarations of dummy arguments :
!
TYPE(TFILEDATA),        INTENT(IN)  :: TPINIFILE ! Initial file
REAL, DIMENSION(:,:,:), INTENT(OUT) :: PDIR_ALB  ! Direct albedo
REAL, DIMENSION(:,:,:), INTENT(OUT) :: PSCA_ALB  ! Diffuse albedo
REAL, DIMENSION(:,:,:), INTENT(OUT) :: PEMIS     ! emissivity
REAL, DIMENSION(:,:),   INTENT(OUT) :: PTSRAD    ! radiative surface temperature
!
!*       0.2   declarations of local variables
!
INTEGER              :: IID, IRESP
TYPE(TFIELDMETADATA) :: TZFIELD
!-------------------------------------------------------------------------------
!
CALL IO_Field_read(TPINIFILE,'DIR_ALB',PDIR_ALB)
CALL IO_Field_read(TPINIFILE,'SCA_ALB',PSCA_ALB)
!
CALL PRINT_MSG(NVERB_INFO,'IO','INI_SURF_RAD','EMIS: reading only first band (copy on others)')
CALL FIND_FIELD_ID_FROM_MNHNAME('EMIS',IID,IRESP)
TZFIELD = TFIELDMETADATA( TFIELDLIST(IID) )
TZFIELD%NDIMS = 2
CALL IO_Field_read(TPINIFILE,TZFIELD,PEMIS(:,:,1))
PEMIS(:,:,:) = SPREAD(SOURCE=PEMIS(:,:,1),DIM=3,NCOPIES=SIZE(PEMIS,3))
!
CALL IO_Field_read(TPINIFILE,'TSRAD',PTSRAD)
!  
!-------------------------------------------------------------------------------
!
END SUBROUTINE INI_SURF_RAD
