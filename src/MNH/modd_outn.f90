!MNH_LIC Copyright 1994-2024 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!     ################
      MODULE MODD_OUT_n
!     ################
!
!!****  *MODD_OUT$n* - declaration of informations on the instants for the 
!!      outputs
!!
!!    PURPOSE
!!    -------
!       The purpose of this  module is to declare the variables
!     describing the instants for the outputs realized by one nested model.         
!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!
!!      None 
!!
!!    REFERENCE
!!    ---------
!!      Book2 of Meso-NH documentation (module MODD_OUTn)
!!          
!!    AUTHOR
!!    ------
!!	J.Stein      *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    20/10/94                      
!  P. Wautelet 05/2016-04/2018: new data structures and calls for I/O
!  P. Wautelet 02/02/2024: restructure backups/outputs lists
!  P. Wautelet 20/03/2024: add boxes for output files
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
!
USE MODD_PARAMETERS, ONLY: JPMODELMAX, NMNHNAMELGTMAX, NNEGUNDEF

IMPLICIT NONE

SAVE

TYPE TOUTBOXMETADATA
  CHARACTER(LEN=NMNHNAMELGTMAX):: CNAME = '' ! Name of the box
  INTEGER :: NIINF = NNEGUNDEF ! Box coordinates in physical domain
  INTEGER :: NISUP = NNEGUNDEF
  INTEGER :: NJINF = NNEGUNDEF
  INTEGER :: NJSUP = NNEGUNDEF
  INTEGER :: NKINF = NNEGUNDEF
  INTEGER :: NKSUP = NNEGUNDEF
END TYPE TOUTBOXMETADATA

TYPE OUT_t
  INTEGER                            :: NBAK_NUMB = 0 ! Number of backups performed by model n
  INTEGER                            :: NOUT_NUMB = 0 ! Number of outputs performed by model n
  INTEGER                            :: NOUT_NBOXES = 0 ! Number of boxes (sub-domains) to write in output files
  INTEGER                            :: NBAK_PREV_STEP = 1 ! Timestep number of the previous backup
  INTEGER                            :: NBAK_STEPFREQ      = NNEGUNDEF ! Time steps between 2 backups
  INTEGER                            :: NOUT_STEPFREQ      = NNEGUNDEF ! Time steps between 2 outputs
  INTEGER                            :: NBAK_STEPFREQFIRST = NNEGUNDEF ! First backup (if regular)
  INTEGER                            :: NOUT_STEPFREQFIRST = NNEGUNDEF ! First output (if regular)
  INTEGER, DIMENSION(:), ALLOCATABLE :: NBAK_STEPLIST                  ! List of time steps to do backups (except regular series)
  INTEGER, DIMENSION(:), ALLOCATABLE :: NOUT_STEPLIST                  ! List of time steps to do outputs (except regular series)
  INTEGER, DIMENSION(:), ALLOCATABLE :: NOUT_FIELDLIST                 ! List of fields to write in outputs
  TYPE(TOUTBOXMETADATA), DIMENSION(:), ALLOCATABLE :: TOUT_BOXES
END TYPE OUT_t

TYPE(OUT_t), DIMENSION(JPMODELMAX), TARGET :: OUT_MODEL

INTEGER,               POINTER :: NBAK_NUMB          => NULL()
INTEGER,               POINTER :: NOUT_NUMB          => NULL()
INTEGER,               POINTER :: NOUT_NBOXES        => NULL()
INTEGER,               POINTER :: NBAK_PREV_STEP     => NULL()
INTEGER,               POINTER :: NBAK_STEPFREQ      => NULL()
INTEGER,               POINTER :: NOUT_STEPFREQ      => NULL()
INTEGER,               POINTER :: NBAK_STEPFREQFIRST => NULL()
INTEGER,               POINTER :: NOUT_STEPFREQFIRST => NULL()
INTEGER, DIMENSION(:), POINTER :: NBAK_STEPLIST      => NULL()
INTEGER, DIMENSION(:), POINTER :: NOUT_STEPLIST      => NULL()
INTEGER, DIMENSION(:), POINTER :: NOUT_FIELDLIST     => NULL()
TYPE(TOUTBOXMETADATA), DIMENSION(:), POINTER :: TOUT_BOXES  => NULL()

CONTAINS

SUBROUTINE OUT_GOTO_MODEL(KFROM, KTO)
INTEGER, INTENT(IN) :: KFROM, KTO
!
! Current model is set to model KTO
NBAK_NUMB          => OUT_MODEL(KTO)%NBAK_NUMB
NOUT_NUMB          => OUT_MODEL(KTO)%NOUT_NUMB
NOUT_NBOXES        => OUT_MODEL(KTO)%NOUT_NBOXES
NBAK_PREV_STEP     => OUT_MODEL(KTO)%NBAK_PREV_STEP
NBAK_STEPFREQ      => OUT_MODEL(KTO)%NBAK_STEPFREQ
NOUT_STEPFREQ      => OUT_MODEL(KTO)%NOUT_STEPFREQ
NBAK_STEPFREQFIRST => OUT_MODEL(KTO)%NBAK_STEPFREQFIRST
NOUT_STEPFREQFIRST => OUT_MODEL(KTO)%NOUT_STEPFREQFIRST
NBAK_STEPLIST      => OUT_MODEL(KTO)%NBAK_STEPLIST
NOUT_STEPLIST      => OUT_MODEL(KTO)%NOUT_STEPLIST
NOUT_FIELDLIST     => OUT_MODEL(KTO)%NOUT_FIELDLIST
TOUT_BOXES         => OUT_MODEL(KTO)%TOUT_BOXES

END SUBROUTINE OUT_GOTO_MODEL

END MODULE MODD_OUT_n
