!MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! MASDEV4_7 surfex 2006/05/18 13:07:25
!-----------------------------------------------------------------
!     ##########################
      MODULE MODI_INI_SURF_RAD
!     ##########################
!
INTERFACE
!
    SUBROUTINE INI_SURF_RAD(HINIFILE, HLUOUT, PDIR_ALB, PSCA_ALB, PEMIS, PTSRAD)
!
!
CHARACTER (LEN=*),      INTENT(IN)  :: HINIFILE  ! Name of the initial file
CHARACTER (LEN=*),      INTENT(IN)  :: HLUOUT    ! name for output-listing
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
!   #######################################################################
    SUBROUTINE INI_SURF_RAD(HINIFILE, HLUOUT, PDIR_ALB, PSCA_ALB, PEMIS, PTSRAD)
!   #######################################################################
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
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODE_FMREAD
!
IMPLICIT NONE
!
!*       0.1   Declarations of dummy arguments :
!
CHARACTER (LEN=*),      INTENT(IN)  :: HINIFILE  ! Name of the initial file
CHARACTER (LEN=*),      INTENT(IN)  :: HLUOUT    ! name for output-listing
REAL, DIMENSION(:,:,:), INTENT(OUT) :: PDIR_ALB  ! Direct albedo
REAL, DIMENSION(:,:,:), INTENT(OUT) :: PSCA_ALB  ! Diffuse albedo
REAL, DIMENSION(:,:,:),   INTENT(OUT) :: PEMIS     ! emissivity
REAL, DIMENSION(:,:),   INTENT(OUT) :: PTSRAD    ! radiative surface temperature
!
!*       0.2   declarations of local variables
!
INTEGER                :: IGRID,ILENCH,IRESP  !   File 
CHARACTER (LEN=LEN_HREC)     :: YRECFM              ! management
CHARACTER (LEN=100)    :: YCOMMENT            ! variables  
!
!-------------------------------------------------------------------------------
!
YRECFM      = 'DIR_ALB'
YCOMMENT    = 'X_Y_DIR_ALB (-)'
IGRID       = 1
ILENCH      = LEN(YCOMMENT)
CALL FMREAD(HINIFILE,YRECFM,HLUOUT,'XY',PDIR_ALB,IGRID,ILENCH,YCOMMENT,IRESP)
!
YRECFM      = 'SCA_ALB'
YCOMMENT    = 'X_Y_SCA_ALB (-)'
IGRID       = 1
ILENCH      = LEN(YCOMMENT)
CALL FMREAD(HINIFILE,YRECFM,HLUOUT,'XY',PSCA_ALB,IGRID,ILENCH,YCOMMENT,IRESP)
!
YRECFM      = 'EMIS'
YCOMMENT    = 'X_Y_EMIS (-)'
IGRID       = 1
ILENCH      = LEN(YCOMMENT)
CALL FMREAD(HINIFILE,YRECFM,HLUOUT,'XY',PEMIS,IGRID,ILENCH,YCOMMENT,IRESP)
!
YRECFM      = 'TSRAD'
YCOMMENT    = 'X_Y_TSRAD (-)'
IGRID       = 1
ILENCH      = LEN(YCOMMENT)
CALL FMREAD(HINIFILE,YRECFM,HLUOUT,'XY',PTSRAD,IGRID,ILENCH,YCOMMENT,IRESP)
!  
!-------------------------------------------------------------------------------
!
END SUBROUTINE INI_SURF_RAD
