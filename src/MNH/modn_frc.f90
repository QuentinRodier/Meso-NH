!MNH_LIC Copyright 1996-2021 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!     ###############
      MODULE MODN_FRC
!     ###############
!!
!!***  *MODN_FRC -  Namelist module for the forcing control parameters
!!
!!    PURPOSE
!!    -------
!       This module contains the following control parameters are used 
!     by FORCING:
!     - LGEOST_UV_FRC and LGEOST_TH_FRC
!     - LTEND_THRV_FRC and LTEND_UV_FRC
!     - LVERT_MOTION_FRC
!     - LRELAX_THRV_FRC, LRELAX_UV_FRC and LRELAX_UVMEAN_FRC using:
!         XRELAX_TIME_FRC, XRELAX_HEIGHT_FRC and CRELAX_HEIGHT_TYPE
!     - LTRANS
!         XUTRANS, XVTRANS
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      None 
!!
!!    REFERENCE
!!    ---------
!!      Book2 of documentation of Meso-NH (module MODD_FRC)
!!      
!!
!!    AUTHOR
!!    ------
!  !	Marc Georgelin Labo d'aerologie
!!
!!    MODIFICATIONS
!!    -------------
!!      Original 29/07/96 
!!      29/07/96 (Pinty&Suhre) revised
!!      18/11/96 (Pinty)       add translation
!!      27/01/98 (P. Bechtold) use tendency forcing
!!                             add SST and surf pressure forcing
!!      06/2003  (V. Masson)   removes SST forcing (externalisation of surface)
!!                   09/2017 Q.Rodier add LTEND_UV_FRC
!!                   06/2021 F.Couvreux add LRELAX_UVMEAN_FRC
!-------------------------------------------------------------------------------
USE MODD_FRC
!
IMPLICIT NONE
!
NAMELIST /NAM_FRC/ LGEOST_UV_FRC      , &
                   LGEOST_TH_FRC      , &
                   LTEND_THRV_FRC     , &
                   LTEND_UV_FRC       , &
                   LVERT_MOTION_FRC   , &
                   LRELAX_THRV_FRC    , &
                   LRELAX_UV_FRC      , &
                   LRELAX_UVMEAN_FRC  , &
                   XRELAX_TIME_FRC    , &
                   XRELAX_HEIGHT_FRC  , &
                   CRELAX_HEIGHT_TYPE , &
                   LTRANS             , &
                   XUTRANS            , &
                   XVTRANS            , &
                   LPGROUND_FRC       ,&
                   LDEEPOC ,&
                   XCENTX_OC ,&
                   XCENTY_OC ,&
                   XRADX_OC  ,&
                   XRADY_OC 
!
END MODULE MODN_FRC
