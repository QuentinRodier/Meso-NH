!MNH_LIC Copyright 1996-2024 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!     ##################
      MODULE MODN_OUTPUT
!     ##################
!
!!****  *MODN_OUTPUT* - declaration of namelist NAM_OUTPUT
!!
!!    PURPOSE
!!    -------
!       The purpose of this  module is to specify the namelist  NAM_OUTPUT
!       which concerns the instants and some parameters (compression and precision reduction)
!       of the outputs realized by all models.
!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      Module MODD_BAKOUT : contains declaration of the variables describing
!!                           the instants and some parameters (compression and
!!                           precision reduction) of the outputs
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
!  P. Wautelet       2016: new structures for outputs/backups
!  P. Wautelet 02/10/2017: split NAM_OUTPUT in NAM_BACKUP and NAM_OUTPUT
!  P. Wautelet 14/12/2023: add lossy compression for output files
!  P. Wautelet 20/03/2024: add boxes for output files
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
USE MODD_BAKOUT
!
IMPLICIT NONE
!
NAMELIST/NAM_OUTPUT/LOUT_BEG,LOUT_END,&
                   XOUT_TIME,NOUT_STEP,&
                   NOUT_STEP_FREQ,NOUT_STEP_FREQ_FIRST,&
                   XOUT_TIME_FREQ,XOUT_TIME_FREQ_FIRST, &
                   COUT_VAR, &
                   LOUT_REDUCE_FLOAT_PRECISION, &
                   LOUT_COMPRESS, NOUT_COMPRESS_LEVEL,&
                   LOUT_COMPRESS_LOSSY, COUT_COMPRESS_LOSSY_ALGO, NOUT_COMPRESS_LOSSY_NSD, &
                   COUT_DIR, &
                   NOUT_BOXES, COUT_BOX_NAME, LOUT_MAINDOMAIN_WRITE, &
                   NOUT_BOX_IINF, NOUT_BOX_ISUP, NOUT_BOX_JINF, NOUT_BOX_JSUP, NOUT_BOX_KINF, NOUT_BOX_KSUP

LOGICAL, SAVE, PRIVATE :: LOUTPUT_NML_ALLOCATED = .FALSE.

CONTAINS

SUBROUTINE OUTPUT_NML_ALLOCATE( )
  USE MODD_CONF,       ONLY: NMODEL
  USE MODD_IO,         ONLY: NFILE_NUM_MAX
  USE MODD_PARAMETERS, ONLY: NNEGUNDEF, XNEGUNDEF

  USE MODE_MSG

  INTEGER :: JI

  IF ( LOUTPUT_NML_ALLOCATED ) THEN
      CALL PRINT_MSG( NVERB_DEBUG, 'GEN', 'OUTPUT_NML_ALLOCATE', 'was altready allocated' )
      RETURN
    END IF

  ALLOCATE( XOUT_TIME(NMODEL, NFILE_NUM_MAX) )
  ALLOCATE( NOUT_STEP(NMODEL, NFILE_NUM_MAX) )
  ALLOCATE( COUT_VAR(NMODEL, JPOUTVARMAX) )

  ALLOCATE( COUT_BOX_NAME(NMODEL, NOUT_BOXMAX) )

  ALLOCATE( NOUT_BOX_IINF(NMODEL, NOUT_BOXMAX) )
  ALLOCATE( NOUT_BOX_ISUP(NMODEL, NOUT_BOXMAX) )
  ALLOCATE( NOUT_BOX_JINF(NMODEL, NOUT_BOXMAX) )
  ALLOCATE( NOUT_BOX_JSUP(NMODEL, NOUT_BOXMAX) )
  ALLOCATE( NOUT_BOX_KINF(NMODEL, NOUT_BOXMAX) )
  ALLOCATE( NOUT_BOX_KSUP(NMODEL, NOUT_BOXMAX) )

  XOUT_TIME(:,:) = XNEGUNDEF
  NOUT_STEP(:,:) = NNEGUNDEF
  COUT_VAR(:,:)  = ''

  COUT_BOX_NAME(:,:) = ''

  NOUT_BOX_IINF(:,:) = NNEGUNDEF
  NOUT_BOX_ISUP(:,:) = NNEGUNDEF
  NOUT_BOX_JINF(:,:) = NNEGUNDEF
  NOUT_BOX_JSUP(:,:) = NNEGUNDEF
  NOUT_BOX_KINF(:,:) = NNEGUNDEF
  NOUT_BOX_KSUP(:,:) = NNEGUNDEF

  LOUTPUT_NML_ALLOCATED = .TRUE.

END SUBROUTINE OUTPUT_NML_ALLOCATE


SUBROUTINE OUTPUT_NML_DEALLOCATE( )
  USE MODE_MSG

  IF ( .NOT. LOUTPUT_NML_ALLOCATED ) THEN
    CALL PRINT_MSG( NVERB_WARNING, 'GEN', 'OUTPUT_NML_DEALLOCATE', 'was not cleanly allocated' )
    RETURN
  END IF

  DEALLOCATE( XOUT_TIME )
  DEALLOCATE( NOUT_STEP )
  DEALLOCATE( COUT_VAR  )

  DEALLOCATE( COUT_BOX_NAME )

  DEALLOCATE( NOUT_BOX_IINF )
  DEALLOCATE( NOUT_BOX_ISUP )
  DEALLOCATE( NOUT_BOX_JINF )
  DEALLOCATE( NOUT_BOX_JSUP )
  DEALLOCATE( NOUT_BOX_KINF )
  DEALLOCATE( NOUT_BOX_KSUP )

  LOUTPUT_NML_ALLOCATED = .FALSE.

END SUBROUTINE OUTPUT_NML_DEALLOCATE

END MODULE MODN_OUTPUT
