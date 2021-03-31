!MNH_LIC Copyright 2000-2021 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!      ################
MODULE MODI_LES_VER_INT
!      ################
!
INTERFACE LES_VER_INT
!
      SUBROUTINE LES_VER_INT(PA_MNH, PA_LES)

REAL,    DIMENSION(:,:,:), INTENT(IN)  :: PA_MNH
!
REAL,    DIMENSION(:,:,:), INTENT(OUT) :: PA_LES
!
END SUBROUTINE LES_VER_INT
!
END INTERFACE
!
END MODULE MODI_LES_VER_INT
!
!     ######################################
      SUBROUTINE LES_VER_INT(PA_MNH, PA_LES)
!     ######################################
!
!
!!****  *LES_VER_INT* interpolates a MESONH field
!!                    on the LES output levels
!!
!!    PURPOSE
!!    -------
!!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
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
!!      Original         07/02/00
!  P. Wautelet 10/04/2019: replace ABORT and STOP calls by Print_msg
!  P. Wautelet 30/03/2021: budgets: LES cartesian subdomain limits are defined in the physical domain
! --------------------------------------------------------------------------
!       
!*      0. DECLARATIONS
!          ------------
!
USE MODD_LES
USE MODD_PARAMETERS
!
USE MODE_ll
use mode_msg
!
USE MODI_VER_INTERP_LIN
!
IMPLICIT NONE
!
!*      0.1  declarations of arguments
!
REAL,    DIMENSION(:,:,:), INTENT(IN)  :: PA_MNH
!
REAL,    DIMENSION(:,:,:), INTENT(OUT) :: PA_LES
!
!
!       0.2  declaration of local variables
!
INTEGER :: JK  ! vertical loop counter
!
!-------------------------------------------------------------------------------
!
IF (CLES_LEVEL_TYPE=='K') THEN
  DO JK = 1, NLES_K
    PA_LES(:,:,JK) = PA_MNH(:,:,NLES_LEVELS(JK) + JPVEXT )
  END DO
ELSE IF (CLES_LEVEL_TYPE=='Z') THEN
  PA_LES = VER_INTERP_LIN(PA_MNH,NKLIN_CURRENT_LES,XCOEFLIN_CURRENT_LES)
  !
  WHERE(NKLIN_CURRENT_LES<2)
    PA_LES = XUNDEF
  END WHERE
ELSE
  call Print_msg(NVERB_FATAL,'GEN','LES_VER_INT','invalid CLES_LEVEL_TYPE ('//trim(CLES_LEVEL_TYPE)//')')
END IF
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE LES_VER_INT
