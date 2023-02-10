!MNH_LIC Copyright 2018-2023 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!     ##################
      MODULE MODN_FIRE_n
!     ##################
!
!!****  *MODN_FIRE_n* - declaration of namelist NAM_FIREn
!!
!!    PURPOSE
!!    -------
!       The purpose of this  module is to specify the namelist  NAM_FIREn
!     which concerns the instants for the outputs realized by all models.
!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      Module MODD_FIRE_n : contains declaration of the variables describing
!!                          the instants for the outputs
!!
!!
!!    REFERENCE
!!    ---------
!!      Book2 of Meso-NH documentation (module MODD_FIRE_n)
!!
!!    AUTHOR
!!    ------
!!  A. Costes      *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    23/07/2018
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
USE MODD_FIRE_n
!
IMPLICIT NONE
!

!PW: gerer le grid nesting (cft modn_stationn?)

NAMELIST/NAM_FIREn/LBLAZE,&
CPROPAG_MODEL,CHEAT_FLUX_MODEL,CLATENT_FLUX_MODEL,XFERR,&
NFIRE_RK_ORDER,NFIRE_WENO_ORDER,LSGBAWEIGHT,&
NREFINX,NREFINY,XCFLMAXFIRE,CFIRE_CPL_MODE,CBMAPFILE,&
LINTERPWIND,XLSDIFFUSION,XROSDIFFUSION,NNBSMOKETRACER,&
XFLUXZEXT,XFLUXZMAX,XFLXCOEFTMP,&
LWINDFILTER,CWINDFILTER,XEWAMTAU,XWLIMUTH,XWLIMUTMAX,NWINDSLOPECPLMODE
!
END MODULE MODN_FIRE_n
