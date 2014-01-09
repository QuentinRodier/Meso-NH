!MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!       #######################
        MODULE  MODD_PRINT_ELEC
!       #######################
!
!!****  *MODD_PRINT_ELEC* - declaration of the LU and IOSTAT for extra prints
!!
!!	PURPOSE
!!	-------
!
!!**	IMPLICIT ARGUMENTS
!!	------------------
!!	  None
!!
!!	REFERENCE
!!	---------
!!
!!	AUTHOR
!!	------
!!       Jean-Pierre Pinty    * Laboratoire d'Aerologie *
!!
!!	MODIFICATIONS
!!	-------------
!!	  Original	17/01/2012
!!
!-------------------------------------------------------------------------------
!
!*	0.	DECLARATIONS
!		------------
!
IMPLICIT NONE
!
INTEGER :: NLU_series_cloud_elec, NIOSTAT_series_cloud_elec, &
           NLU_fgeom_diag,        NIOSTAT_fgeom_diag,        &
           NLU_fgeom_coord,       NIOSTAT_fgeom_coord,       &
           NLU_light_diag,        NIOSTAT_light_diag,        &
           NLU_light_coord,       NIOSTAT_light_coord
!
END MODULE MODD_PRINT_ELEC
