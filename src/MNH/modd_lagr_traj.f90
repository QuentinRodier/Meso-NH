!MNH_LIC Copyright 1994-2024 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
! Modifications:
!  P. Wautelet 05/2016-04/2018: new data structures and calls for I/O
!  P. Wautelet 15/02/2024: add time dimension for Lagrangian trajectories
!-----------------------------------------------------------------
!###################
MODULE MODD_LAGR_TRAJ
!###################

USE MODD_PARAMETERS, ONLY: NFILENAMELGTMAX
USE MODD_TYPE_DATE,  ONLY: DATE_TIME

SAVE

CHARACTER(LEN=NFILENAMELGTMAX) :: CFILES(100)      ! names of the files to be treated
INTEGER                        :: NSTART_SUPP(100) ! supplementary starts for the lagrangian trajectories

INTEGER                                    :: NTRAJSTLG = 0 ! Number of time starts for Lagrangian trajectories
TYPE(DATE_TIME), DIMENSION(:), ALLOCATABLE :: TLAGR_DATES   ! Times for Lagrangian trajectories

END MODULE MODD_LAGR_TRAJ
