!MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! MASDEV4_7 modd 2006/05/18 13:07:25
!-----------------------------------------------------------------
!     ######spl
MODULE MODD_STO_FILE
!################
CHARACTER (LEN=28), SAVE :: CFILES(100)       ! names of the files to be treated
CHARACTER (LEN=28), SAVE :: CFILES_STA(100)   ! status of these files 'INIT_SV'
                                              ! if a restart of the lagrangian
                                              ! tracers has been performed
INTEGER           , SAVE :: NSTART_SUPP(100)  ! supplementary starts 
                                              ! for the lagrangian trajectories 
!
END MODULE MODD_STO_FILE
