!MNH_LIC Copyright 2021-2023 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!
!     #######################  
      MODULE MODN_RECYCL_PARAM_n
!     #######################
!
!****  *MODN_RECYCL_PARAM$n* - declaration of namelist NAM_RECYCL_PARAMn
!
!    PURPOSE
!    -------
!****  The purpose of this declarative module is to declare the constants
!      allowing to initialize the turbulence recycling method 
!
!
!    IMPLICIT ARGUMENTS
!    ------------------
!      Module MODD_RECYCL_PARAM$n : contains declaration of RECYCLING parameters 
!
!
!    REFERENCE
!    ---------
!          
!    AUTHOR
!    ------
!	   Tim Nagel (Meteo-France)
!
!    MODIFICATIONS
!    -------------
!      Original    01/02/2021
!
USE MODD_RECYCL_PARAM_n, ONLY: &
         LRECYCL_n => LRECYCL, &            
         LRECYCLN_n => LRECYCLN, &
         LRECYCLW_n => LRECYCLW, &
         LRECYCLE_n => LRECYCLE, &
         LRECYCLS_n => LRECYCLS, &
         XDRECYCLN_n => XDRECYCLN , &
         XARECYCLN_n => XARECYCLN , &
         XDRECYCLW_n => XDRECYCLW , &
         XARECYCLW_n => XARECYCLW , &
         XDRECYCLE_n => XDRECYCLE , &
         XARECYCLE_n => XARECYCLE , &
         XDRECYCLS_n => XDRECYCLS , &
         XARECYCLS_n => XARECYCLS , &
         NTMOY_n      => NTMOY,      &
         NTMOYCOUNT_n => NTMOYCOUNT, &
         NNUMBELT_n   => NNUMBELT,   &
         XRCOEFF_n => XRCOEFF,       &
         XTBVTOP_n => XTBVTOP,     &
         XTBVBOT_n => XTBVBOT
!
IMPLICIT NONE
!
INTEGER, SAVE :: NTMOY, NTMOYCOUNT, NNUMBELT
LOGICAL,SAVE  :: LRECYCL,LRECYCLN,LRECYCLW,LRECYCLE,LRECYCLS
REAL,SAVE     :: XDRECYCLN,XARECYCLN,XDRECYCLW,XARECYCLW,XDRECYCLE,XARECYCLE,XDRECYCLS,&
                 XARECYCLS,XRCOEFF,XTBVTOP,XTBVBOT

!
NAMELIST /NAM_RECYCL_PARAMn/ NTMOY, NTMOYCOUNT, NNUMBELT,                                                 &
                             LRECYCL, LRECYCLN, LRECYCLW, LRECYCLE, LRECYCLS,                             &
                             XDRECYCLW, XARECYCLW, XDRECYCLN, XARECYCLN, XDRECYCLE, XARECYCLE, XDRECYCLS, &
                             XARECYCLS, XRCOEFF, XTBVTOP, XTBVBOT
!
CONTAINS
!
SUBROUTINE INIT_NAM_RECYCL_PARAMn
  LRECYCL = LRECYCL_n
  LRECYCLN = LRECYCLN_n
  LRECYCLW = LRECYCLW_n
  LRECYCLE = LRECYCLE_n
  LRECYCLS = LRECYCLS_n
  XDRECYCLN = XDRECYCLN_n 
  XARECYCLN = XARECYCLN_n 
  XDRECYCLW = XDRECYCLW_n
  XARECYCLW = XARECYCLW_n  
  XDRECYCLE = XDRECYCLE_n
  XARECYCLE = XARECYCLE_n
  XDRECYCLS = XDRECYCLS_n
  XARECYCLS = XARECYCLS_n
  NTMOY      = NTMOY_n
  NTMOYCOUNT = NTMOYCOUNT_n
  NNUMBELT   = NNUMBELT_n
  XRCOEFF = XRCOEFF_n
  XTBVTOP = XTBVTOP_n
  XTBVBOT = XTBVBOT_n


END SUBROUTINE INIT_NAM_RECYCL_PARAMn

SUBROUTINE UPDATE_NAM_RECYCL_PARAMn
  LRECYCL_n = LRECYCL
  LRECYCLN_n = LRECYCLN
  LRECYCLW_n = LRECYCLW
  LRECYCLE_n = LRECYCLE
  LRECYCLS_n = LRECYCLS
  XDRECYCLN_n = XDRECYCLN 
  XARECYCLN_n = XARECYCLN 
  XDRECYCLW_n = XDRECYCLW
  XARECYCLW_n = XARECYCLW
  XDRECYCLE_n = XDRECYCLE
  XARECYCLE_n = XARECYCLE
  XDRECYCLS_n = XDRECYCLS
  XARECYCLS_n = XARECYCLS
  NTMOY_n      = NTMOY
  NTMOYCOUNT_n = NTMOYCOUNT
  NNUMBELT_n   = NNUMBELT
  XRCOEFF_n = XRCOEFF
  XTBVTOP_n = XTBVTOP
  XTBVBOT_n = XTBVBOT


END SUBROUTINE UPDATE_NAM_RECYCL_PARAMn
!------------------------------------------------------------------------------
END MODULE MODN_RECYCL_PARAM_n
!
