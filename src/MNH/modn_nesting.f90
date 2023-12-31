!MNH_LIC Copyright 1995-2021 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!     ###################
      MODULE MODN_NESTING
!     ###################
!
!!****  *MODN_NESTING* - declaration of namelist NAM_NESTING
!!
!!    PURPOSE
!!    -------
!       The purpose of this  module is to specify  the namelist NAM_NESTING
!     which concerns the gridnesting configuration. 
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      Module MODD_NESTING : contains declaration of the gridnesting configuration
!!
!!         NDAD(m)     : model number of the father of each model "m"
!!                           0       no father (always the case for m=1; NDAD(1)=0)
!!                       constraint:               NDAD(m) < m   
!!
!!         NDTRATIO(m) : time step ratio betwen models NDAD(m) and m
!!
!!         XWAY(m)     : interactive nesting level of model m with its father NDAD(m)
!!
!!
!!    REFERENCE
!!    ---------
!!      Book2 and book3 of documentation of Meso-NH (module MODN_NESTING)
!!       
!!    AUTHOR
!!    ------
!!	J. P. Lafore     *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    16/08/95 
!!     JL Redelsperger  03/2021 : Add Auto-coupled O-A LES case                      
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
USE MODD_NESTING
USE MODD_REF, ONLY: LCOUPLES
!
IMPLICIT NONE
!
NAMELIST/NAM_NESTING/NDAD,NDTRATIO,XWAY,LCOUPLES
!
END MODULE MODN_NESTING
