!MNH_LIC Copyright 1996-2024 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!     ##################
      MODULE MODN_BACKUP
!     ##################
!
!!****  *MODN_BACKUP* - declaration of namelist NAM_BACKUP
!!
!!    PURPOSE
!!    -------
!       The purpose of this  module is to specify the namelist NAM_BACKUP
!       which concerns the instants and some parameters (compression and precision reduction)
!       of the backups realized by all models.
!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      Module MODD_BAKOUT : contains declaration of the variables describing
!!                           the instants of the backups
!!
!!    REFERENCE
!!    ---------
!!      Book2 of Meso-NH documentation (module MODD_BACKUP)
!!
!!    AUTHOR
!!    ------
!!	J.P. Lafore      *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    26/07/96
!  P. Wautelet       2016: new structures for outputs/backups
!  P. Wautelet 02/10/2017: split NAM_OUTPUT in NAM_BACKUP and NAM_OUTPUT
!  P. Wautelet 07/02/2024: add compression for backup files
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
USE MODD_BAKOUT
!
IMPLICIT NONE
!
NAMELIST/NAM_BACKUP/LBAK_BEG,LBAK_END,&
                   XBAK_TIME,NBAK_STEP,&
                   NBAK_STEP_FREQ,NBAK_STEP_FREQ_FIRST,&
                   XBAK_TIME_FREQ,XBAK_TIME_FREQ_FIRST,&
                   LBAK_REDUCE_FLOAT_PRECISION,        &
                   LBAK_COMPRESS, NBAK_COMPRESS_LEVEL, &
                   CBAK_DIR
!
CONTAINS

SUBROUTINE BACKUP_NML_ALLOCATE( )
  USE MODD_CONF,       ONLY: NMODEL
  USE MODD_IO,         ONLY: NFILE_NUM_MAX
  USE MODD_PARAMETERS, ONLY: NNEGUNDEF, XNEGUNDEF

  IF ( .NOT.ALLOCATED(XBAK_TIME) ) THEN
    ALLOCATE( XBAK_TIME(NMODEL, NFILE_NUM_MAX) )
    XBAK_TIME(:,:) = XNEGUNDEF
  END IF

  IF ( .NOT.ALLOCATED(NBAK_STEP) ) THEN
    ALLOCATE( NBAK_STEP(NMODEL, NFILE_NUM_MAX) )
    NBAK_STEP(:,:) = NNEGUNDEF
  END IF

END SUBROUTINE BACKUP_NML_ALLOCATE


SUBROUTINE BACKUP_NML_DEALLOCATE( )

  DEALLOCATE( XBAK_TIME )
  DEALLOCATE( NBAK_STEP )

END SUBROUTINE BACKUP_NML_DEALLOCATE

END MODULE MODN_BACKUP
