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
!     #################
      MODULE MODN_FMOUT
!     #################
!
!!****  *MODN_FMOUT* - declaration of namelist NAM_FMOUT
!!
!!    PURPOSE
!!    -------
!       The purpose of this  module is to specify the namelist  NAM_FMOUT
!     which concerns the instants for the outputs realized by all models.         
!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      Module MODD_FMOUT : contains declaration of the variables describing 
!!                          the instants for the outputs
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
!!      Ph. Wautelet : 2016: new structures for outputs/backups
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
USE MODD_FMOUT
!
IMPLICIT NONE
!
NAMELIST/NAM_FMOUT/LBAK_BEG,LBAK_END,&
                   XBAK_TIME,NBAK_STEP,&
                   NBAK_STEP_FREQ,NBAK_STEP_FREQ_FIRST,&
                   XBAK_TIME_FREQ,XBAK_TIME_FREQ_FIRST, &
                   LOUT_BEG,LOUT_END,&
                   XOUT_TIME,NOUT_STEP,&
                   NOUT_STEP_FREQ,NOUT_STEP_FREQ_FIRST,&
                   XOUT_TIME_FREQ,XOUT_TIME_FREQ_FIRST, &
                   COUT_VAR
!
END MODULE MODN_FMOUT
