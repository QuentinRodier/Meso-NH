!MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source$ $Revision$
! MASDEV4_7 modn 2006/05/18 13:07:25
!-----------------------------------------------------------------
!     ##################
      MODULE MODN_BACKUP
!     ##################
!
!!****  *MODN_BACKUP* - declaration of namelist NAM_OUTPUT
!!
!!    PURPOSE
!!    -------
!       The purpose of this  module is to specify the namelist  NAM_OUTPUT
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
!!      Ph. Wautelet : 2016: new structures for outputs/backups
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
                   XBAK_TIME_FREQ,XBAK_TIME_FREQ_FIRST
!
END MODULE MODN_BACKUP
