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
!     ##################
      MODULE MODD_OUTPUT
!     ##################
!
!!****  *MODD_OUTPUT* - declaration of informations on the instants for the
!!      outputs of all models
!!
!!    PURPOSE
!!    -------
!       The purpose of this  module is to declare the instants and some parameters
!       (compression and precision reduction) of the outputs realized by all models.
!       Introduced to facilitate the output FM-file managment in case of nesting
!     
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!
!!      Module MODD_PARAMETERS  :
!!         JPMODELMAX : Maximum allowed  number of nested models
!!
!!    REFERENCE
!!    ---------
!!      Book2 of Meso-NH documentation (module MODD_OUTPUT)
!!          
!!    AUTHOR
!!    ------
!!	J.P. Lafore      *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    26/07/96                     
!!      Ph. Wautelet : 2016: new structures for outputs/backups
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
USE MODD_PARAMETERS
!
IMPLICIT NONE
!
LOGICAL,SAVE :: LBAK_BEG = .FALSE. ! Force a backup/output at the first timestep
LOGICAL,SAVE :: LOUT_BEG = .FALSE. ! of the segment for all models
LOGICAL,SAVE :: LBAK_END = .FALSE. ! Force a backup/output at the last timestep
LOGICAL,SAVE :: LOUT_END = .FALSE. ! of the segment for all models
LOGICAL,SAVE,DIMENSION(JPMODELMAX) :: LOUT_REDUCE_FLOAT_PRECISION = .FALSE.
! Reduce the precision of floats to single precision instead of double precision (for netCDF)
LOGICAL,SAVE,DIMENSION(JPMODELMAX) :: LOUT_COMPRESS = .FALSE. ! Compress (float) arrays (for netCDF)
INTEGER,SAVE,DIMENSION(JPMODELMAX) :: NOUT_COMPRESS_LEVEL = 4 ! Compression level (for netCDF)
REAL,SAVE,ALLOCATABLE,DIMENSION(:,:)  ::   XBAK_TIME, XOUT_TIME
! XBAK_TIME(m,i) / XOUT_TIME(m,i) array of 
! the increments in seconds from the beginning of the segment to the
! instant where the i-th fields output on FM-files is realized by model "m"
INTEGER,SAVE,ALLOCATABLE,DIMENSION(:,:)  ::   NBAK_STEP, NOUT_STEP
! NBAK_STEP(m,i) / NOUT_STEP(m,i) array of
! the increments in steps from the beginning of the segment to the
! step where the i-th fields output on FM-files is realized by model "m"
INTEGER,SAVE,DIMENSION(JPMODELMAX) :: NBAK_STEP_FREQ = NNEGUNDEF, NOUT_STEP_FREQ = NNEGUNDEF
! Number of timesteps between 2 backups/outputs for each model
INTEGER,SAVE,DIMENSION(JPMODELMAX) :: NBAK_STEP_FREQ_FIRST = 1, NOUT_STEP_FREQ_FIRST = 1
! First timestep numbers between 2 backups/outputs for each model (if NBAK/OUT_STEP_FREQ is set)
REAL,SAVE,DIMENSION(JPMODELMAX) :: XBAK_TIME_FREQ = XNEGUNDEF, XOUT_TIME_FREQ = XNEGUNDEF
! Time between 2 backups/outputs for each model
REAL,SAVE,DIMENSION(JPMODELMAX) :: XBAK_TIME_FREQ_FIRST = 0., XOUT_TIME_FREQ_FIRST = 0.
! Time for first backup/output for each model (if XBAK/OUT_TIME_FREQ is set)
CHARACTER(LEN=NMNHNAMELGTMAX),SAVE,ALLOCATABLE,DIMENSION(:,:) :: COUT_VAR ! Name of the fields to output
!
!
END MODULE MODD_OUTPUT
