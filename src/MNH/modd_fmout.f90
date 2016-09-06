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
!     #################
      MODULE MODD_FMOUT
!     #################
!
!!****  *MODD_FMOUT* - declaration of informations on the instants for the 
!!      outputs of all models
!!
!!    PURPOSE
!!    -------
!       The purpose of this  module is to declare the instants for the outputs 
!!     realized by all models.         
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
!!      Book2 of Meso-NH documentation (module MODD_FMOUT)
!!          
!!    AUTHOR
!!    ------
!!	J.P. Lafore      *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    26/07/96                     
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
USE MODD_PARAMETERS
!
IMPLICIT NONE
!
LOGICAL,SAVE :: LBAK_BEG = .FALSE. ! Force a backup at the first timestep
                                   ! of the segment for all models
LOGICAL,SAVE :: LBAK_END = .FALSE. ! Force a backup at the last timestep
                                   ! of the segment for all models
REAL,SAVE,ALLOCATABLE,DIMENSION(:,:)  ::   XBAK_TIME 
! XBAK_TIME(m,i) array of 
! the increments in seconds from the beginning of the segment to the
! instant where the i-th fields output on FM-files is realized by model "m"
INTEGER,SAVE,ALLOCATABLE,DIMENSION(:,:)  ::   NBAK_STEP
! NBAK_STEP(m,i) array of
! the increments in steps from the beginning of the segment to the
! step where the i-th fields output on FM-files is realized by model "m"
INTEGER,SAVE,DIMENSION(JPMODELMAX) :: NBAK_STEP_FREQ = NNEGUNDEF ! Number
! of timesteps between 2 backups for each model
INTEGER,SAVE,DIMENSION(JPMODELMAX) :: NBAK_STEP_FREQ_FIRST = 1   ! First
! timestep numbers between 2 backups for each model (if NBAK_STEP_FREQ is set)
REAL,SAVE,DIMENSION(JPMODELMAX) :: XBAK_TIME_FREQ = XNEGUNDEF ! Time between
! 2 backups for each model
REAL,SAVE,DIMENSION(JPMODELMAX) :: XBAK_TIME_FREQ_FIRST = 0.   ! Time for first
! backup for each model (if XBAK_TIME_FREQ is set)
!
!
END MODULE MODD_FMOUT
