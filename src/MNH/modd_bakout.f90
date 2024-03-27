!MNH_LIC Copyright 1996-2024 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!     ##################
      MODULE MODD_BAKOUT
!     ##################
!
!!****  *MODD_BAKOUT* - declaration of informations on the instants for the
!!      outputs and backups of all models
!!
!!    PURPOSE
!!    -------
!       The purpose of this  module is to declare the instants and some parameters
!       (compression and precision reduction) of the outputs and backups realized by
!       all models.
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
!!      Book2 of Meso-NH documentation (module MODD_BAKOUT)
!!
!!    AUTHOR
!!    ------
!!	J.P. Lafore      *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    26/07/96
!  P. Wautelet 05/2016-04/2018: new data structures and calls for I/O
!  P. Wautelet 14/12/2023: add lossy compression for output files
!  P. Wautelet 07/02/2024: add compression for backup files
!  P. Wautelet 20/03/2024: add boxes for output files
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
USE MODD_PARAMETERS
!
IMPLICIT NONE
!
SAVE
!
INTEGER, PARAMETER :: NCOMPRNAMELGTMAX  = 10 ! Maximum length of compression algorithm name
INTEGER, PARAMETER :: NOUT_BOXMAX       = 20 ! Maximum number of boxes (subdomains) for each model output
!
LOGICAL :: LBAK_BEG = .FALSE. ! Force a backup/output at the first timestep
LOGICAL :: LOUT_BEG = .FALSE. ! of the segment for all models
LOGICAL :: LBAK_END = .FALSE. ! Force a backup/output at the last timestep
LOGICAL :: LOUT_END = .FALSE. ! of the segment for all models

! Compression

LOGICAL, DIMENSION(JPMODELMAX) :: LBAK_REDUCE_FLOAT_PRECISION = .FALSE. ! Reduce the precision of floats to single precision
LOGICAL, DIMENSION(JPMODELMAX) :: LOUT_REDUCE_FLOAT_PRECISION = .FALSE. !  instead of double precision (for netCDF)
LOGICAL, DIMENSION(JPMODELMAX) :: LBAK_COMPRESS = .FALSE. ! Compress (float) arrays (for netCDF)
LOGICAL, DIMENSION(JPMODELMAX) :: LOUT_COMPRESS = .FALSE. ! Compress (float) arrays (for netCDF)
INTEGER, DIMENSION(JPMODELMAX) :: NBAK_COMPRESS_LEVEL = 4 ! Compression level (for netCDF)
INTEGER, DIMENSION(JPMODELMAX) :: NOUT_COMPRESS_LEVEL = 4 ! Compression level (for netCDF)

! Lossy compression
LOGICAL, DIMENSION(JPMODELMAX) :: LOUT_COMPRESS_LOSSY = .FALSE. ! Lossy compression of (float) arrays (for netCDF)
CHARACTER(LEN=NCOMPRNAMELGTMAX), DIMENSION(JPMODELMAX) :: COUT_COMPRESS_LOSSY_ALGO = 'GRANULARBR' ! Quantization algorithm
INTEGER, DIMENSION(JPMODELMAX) :: NOUT_COMPRESS_LOSSY_NSD  = 3  ! Number of Significant Digits (or Bits)

REAL, ALLOCATABLE, DIMENSION(:,:)  ::   XBAK_TIME, XOUT_TIME
! XBAK_TIME(m,i) / XOUT_TIME(m,i) array of
! the increments in seconds from the beginning of the segment to the
! instant where the i-th fields output on FM-files is realized by model "m"
INTEGER, ALLOCATABLE, DIMENSION(:,:)  ::   NBAK_STEP, NOUT_STEP
! NBAK_STEP(m,i) / NOUT_STEP(m,i) array of
! the increments in steps from the beginning of the segment to the
! step where the i-th fields output on FM-files is realized by model "m"
INTEGER, DIMENSION(JPMODELMAX) :: NBAK_STEP_FREQ = NNEGUNDEF, NOUT_STEP_FREQ = NNEGUNDEF
! Number of timesteps between 2 backups/outputs for each model
INTEGER, DIMENSION(JPMODELMAX) :: NBAK_STEP_FREQ_FIRST = NNEGUNDEF, NOUT_STEP_FREQ_FIRST = NNEGUNDEF
! First timestep numbers between 2 backups/outputs for each model (if NBAK/OUT_STEP_FREQ is set)
REAL, DIMENSION(JPMODELMAX) :: XBAK_TIME_FREQ = XNEGUNDEF, XOUT_TIME_FREQ = XNEGUNDEF
! Time between 2 backups/outputs for each model
REAL, DIMENSION(JPMODELMAX) :: XBAK_TIME_FREQ_FIRST = XNEGUNDEF, XOUT_TIME_FREQ_FIRST = XNEGUNDEF
! Time for first backup/output for each model (if XBAK/OUT_TIME_FREQ is set)
CHARACTER(LEN=NMNHNAMELGTMAX), ALLOCATABLE, DIMENSION(:,:) :: COUT_VAR ! Name of the fields to output
!
!Directory names for backups/outputs
CHARACTER(LEN=NDIRNAMELGTMAX) :: CBAK_DIR='', COUT_DIR=''

! Boxes (subdomains) for outputs
INTEGER, DIMENSION(JPMODELMAX) :: NOUT_BOXES = 0 ! Number of sub-boxes inside each modelgrid
CHARACTER(LEN=NMNHNAMELGTMAX), DIMENSION(:,:), ALLOCATABLE :: COUT_BOX_NAME ! Names of the boxes
CHARACTER(LEN=NMNHNAMELGTMAX), DIMENSION(:,:,:), ALLOCATABLE :: COUT_BOX_VAR_SUPP ! Name of the fields to output separately
                                                                                  ! in the different boxes (added to the COUT_VAR)
LOGICAL, DIMENSION(JPMODELMAX) :: LOUT_MAINDOMAIN_WRITE = .FALSE. ! True to write the main domain
                                                                  ! (automatically forced to .TRUE. if NOUT_BOXES=0)
LOGICAL, DIMENSION(JPMODELMAX) :: LOUT_TOP_ABSORBING_LAYER_REMOVE = .TRUE. ! Remove the top absorbing layer

INTEGER, DIMENSION(:,:), ALLOCATABLE :: NOUT_BOX_IINF ! Box coordinates in physical domain (for each model and for each box)
INTEGER, DIMENSION(:,:), ALLOCATABLE :: NOUT_BOX_ISUP
INTEGER, DIMENSION(:,:), ALLOCATABLE :: NOUT_BOX_JINF
INTEGER, DIMENSION(:,:), ALLOCATABLE :: NOUT_BOX_JSUP
INTEGER, DIMENSION(:,:), ALLOCATABLE :: NOUT_BOX_KINF
INTEGER, DIMENSION(:,:), ALLOCATABLE :: NOUT_BOX_KSUP

END MODULE MODD_BAKOUT
