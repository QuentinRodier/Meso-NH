!MNH_LIC Copyright 2018-2021 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!     #######################
       MODULE MODI_EOL_SMEAR
!     #######################
!
INTERFACE
!
SUBROUTINE SMEAR_1LIN(PFX, PFX_SMR)
        REAL, DIMENSION(:,:,:), INTENT(IN)   :: PFX                       ! Force to smear
        REAL, DIMENSION(:,:,:), INTENT(OUT)  :: PFX_SMR                   ! Smeared force
END SUBROUTINE SMEAR_1LIN
!
SUBROUTINE SMEAR_3LIN(PFX, PFY, PFZ, PFX_SMR, PFY_SMR, PFZ_SMR)
        REAL, DIMENSION(:,:,:), INTENT(IN)   :: PFX, PFY, PFZ             ! Force to smear
        REAL, DIMENSION(:,:,:), INTENT(OUT)  :: PFX_SMR, PFY_SMR, PFZ_SMR ! Smeared force
END SUBROUTINE SMEAR_3LIN
!
END INTERFACE
!
END MODULE MODI_EOL_SMEAR
!-------------------------------------------------------------------
!
!!****  *MODI_EOL_SMEAR* -
!!
!!    PURPOSE
!!    -------
!!    Smear the forces of wind turbine to avoid numerical instabilities 
!!
!!    AUTHOR
!!    ------
!!     PA. Joulin 		*CNRM & IFPEN*
!!
!!    MODIFICATIONS
!!    -------------
!!    Original      04/2018
!!    Modification 21/10/20 (PA. Joulin) Updated for a main version
!!
!!---------------------------------------------------------------
!
!#########################################################
SUBROUTINE SMEAR_1LIN(PFX, PFX_SMR)
!!
!!    METHOD
!!    ------
!!    A linear smearing is done is this subroutine.
!!
!---------------------------------------------------------
!
!*       0.    DECLARATIONS
!
 USE MODI_SHUMAN,     ONLY: MXF, MXM
!
!*       0.1   declarations of arguments
!
 REAL, DIMENSION(:,:,:), INTENT(IN)   :: PFX                       ! Force to smear
 REAL, DIMENSION(:,:,:), INTENT(OUT)  :: PFX_SMR                   ! Smeared force
!
!*       1.    SMEARING
!
 PFX_SMR(:,:,:) = MXF(MXM(PFX(:,:,:)))
!
END SUBROUTINE SMEAR_1LIN
!#########################################################
!
!#########################################################
SUBROUTINE SMEAR_3LIN(PFX, PFY, PFZ, PFX_SMR, PFY_SMR, PFZ_SMR)
!!
!!    METHOD
!!    ------
!!    A linear smearing is done is this subroutine.
!!
!---------------------------------------------------------
!
!*       0.    DECLARATIONS
!
USE MODD_PARAMETERS, ONLY: JPVEXT
USE MODI_SHUMAN,     ONLY: MXF, MYF, MZF
USE MODI_SHUMAN,     ONLY: MXM, MYM, MZM
!
!*       0.1   declarations of arguments
!
 REAL, DIMENSION(:,:,:), INTENT(IN)   :: PFX, PFY, PFZ             ! Force to smear
 REAL, DIMENSION(:,:,:), INTENT(OUT)  :: PFX_SMR, PFY_SMR, PFZ_SMR ! Smeared force
!
!*       0.2   declarations of local variables
!
 INTEGER    ::  IKU,IKB,IKE          ! Vertical indices
!
!*       1.    INITIALIZATIONS
!
 IKU = SIZE(PFX,3)                   ! Top of the domain end index
 IKB=1+JPVEXT                        ! Vertical begin index
 IKE=IKU-JPVEXT                      ! Vertical end index
!
!*       2.    SMEARING
!
 PFX_SMR(:,:,:) = MXF(MXM(MYF(MYM(PFX(:,:,:)))))
 PFY_SMR(:,:,:) = MXF(MXM(MYF(MYM(PFY(:,:,:)))))
 PFZ_SMR(:,:,:) = MXF(MXM(MYF(MYM(PFZ(:,:,:)))))
!
 PFX_SMR(:,:,:) = MZF(MZM(PFX_SMR(:,:,:)))
 PFY_SMR(:,:,:) = MZF(MZM(PFY_SMR(:,:,:)))
 PFZ_SMR(:,:,:) = MZF(MZM(PFZ_SMR(:,:,:)))
!
!*       3.    BOUNDARY VALUES
!
 PFX_SMR(:,:,IKB-1) = PFX_SMR(:,:,IKB)
 PFX_SMR(:,:,IKE+1) = PFX_SMR(:,:,IKE)
!
 PFY_SMR(:,:,IKB-1) = PFY_SMR(:,:,IKB)
 PFY_SMR(:,:,IKE+1) = PFY_SMR(:,:,IKE)
!
 PFZ_SMR(:,:,IKB-1) = PFZ_SMR(:,:,IKB)
 PFZ_SMR(:,:,IKE+1) = PFZ_SMR(:,:,IKE)
!
END SUBROUTINE SMEAR_3LIN
!#########################################################
