!MNH_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!MNH_LIC This is part of the Meso-NH software governed by the CeCILL-C licence
!MNH_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!MNH_LIC for details. version 1.
!-----------------------------------------------------------------
!--------------- special set of characters for RCS information
!-----------------------------------------------------------------
! $Source: /home/cvsroot/MNH-VX-Y-Z/src/MNH/modd_precipn.f90,v $ $Revision: 1.2.2.1.2.1.18.1 $
! MASDEV4_7 modd 2006/10/17 13:34:38
!-----------------------------------------------------------------
!     ####################
      MODULE MODD_PRECIP_n
!     ####################
!
!!****  *MODD_PRECIP$n* - declaration of precipitating fields
!!
!!    PURPOSE
!!    -------
!       Stores the INstantaneous and ACcumulated PRecipitating fields of 
!!      resolved clouds
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      None 
!!
!!    REFERENCE
!!    ---------
!!      Book2 of documentation of Meso-NH (module MODD_PRECIPn)
!!          
!!    AUTHOR
!!    ------
!!      J.-P. Pinty *LA*
!!
!!    MODIFICATIONS
!!    -------------
!!
!!       J.-P. Pinty   29/11/02 add C3R5, ICE2, ICE4
!!                    10/2016 (C.Lac) Add droplet deposition
!!
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
USE MODD_PARAMETERS, ONLY: JPMODELMAX
IMPLICIT NONE

TYPE PRECIP_t
!
  REAL, DIMENSION(:,:), POINTER :: XINPRC=>NULL(), XACPRC=>NULL(),  &
                   XINDEP=>NULL(), XACDEP=>NULL(),                  &
                   XINPRR=>NULL(), XACPRR=>NULL(),                  &
                   XINPRS=>NULL(), XACPRS=>NULL(), XINPRG=>NULL(),  &
                   XACPRG=>NULL(), XINPRH=>NULL(), XACPRH=>NULL()
                                         ! Instant and cumul of ground
                                         ! precipitation fields of Cloud,Rain, 
                                         !    Snow, Graupel and Hail
  REAL, DIMENSION(:,:,:), POINTER :: XINPRR3D=>NULL(), & !Instant precip 3D
                   XEVAP3D=>NULL()! Evap profile 3D
!
END TYPE PRECIP_t

TYPE(PRECIP_t), DIMENSION(JPMODELMAX), TARGET, SAVE :: PRECIP_MODEL

REAL, DIMENSION(:,:), POINTER :: XINPRC=>NULL(), XACPRC=>NULL(),  &
                 XINDEP=>NULL(), XACDEP=>NULL(),                  &
                 XINPRR=>NULL(), XACPRR=>NULL(),                  &
                 XINPRG=>NULL(), XINPRS=>NULL(), XACPRS=>NULL(),  &
                 XACPRG=>NULL(), XINPRH=>NULL(), XACPRH=>NULL()
  REAL, DIMENSION(:,:,:), POINTER :: XINPRR3D=>NULL(), & !Instant precip 3D
                   XEVAP3D=>NULL()! Evap profile 3D

CONTAINS

SUBROUTINE PRECIP_GOTO_MODEL(KFROM, KTO)
INTEGER, INTENT(IN) :: KFROM, KTO
!
! Save current state for allocated arrays
PRECIP_MODEL(KFROM)%XINPRC=>XINPRC
PRECIP_MODEL(KFROM)%XACPRC=>XACPRC
PRECIP_MODEL(KFROM)%XINDEP=>XINDEP
PRECIP_MODEL(KFROM)%XACDEP=>XACDEP
PRECIP_MODEL(KFROM)%XINPRR=>XINPRR
PRECIP_MODEL(KFROM)%XINPRR3D=>XINPRR3D
PRECIP_MODEL(KFROM)%XEVAP3D=>XEVAP3D
PRECIP_MODEL(KFROM)%XACPRR=>XACPRR
PRECIP_MODEL(KFROM)%XINPRS=>XINPRS
PRECIP_MODEL(KFROM)%XACPRS=>XACPRS
PRECIP_MODEL(KFROM)%XINPRG=>XINPRG
PRECIP_MODEL(KFROM)%XACPRG=>XACPRG
PRECIP_MODEL(KFROM)%XINPRH=>XINPRH
PRECIP_MODEL(KFROM)%XACPRH=>XACPRH
!
! Current model is set to model KTO
XINPRC=>PRECIP_MODEL(KTO)%XINPRC
XACPRC=>PRECIP_MODEL(KTO)%XACPRC
XINDEP=>PRECIP_MODEL(KTO)%XINDEP
XACDEP=>PRECIP_MODEL(KTO)%XACDEP
XINPRR=>PRECIP_MODEL(KTO)%XINPRR
XINPRR3D=>PRECIP_MODEL(KTO)%XINPRR3D
XEVAP3D=>PRECIP_MODEL(KTO)%XEVAP3D
XACPRR=>PRECIP_MODEL(KTO)%XACPRR
XINPRS=>PRECIP_MODEL(KTO)%XINPRS
XACPRS=>PRECIP_MODEL(KTO)%XACPRS
XINPRG=>PRECIP_MODEL(KTO)%XINPRG
XACPRG=>PRECIP_MODEL(KTO)%XACPRG
XINPRH=>PRECIP_MODEL(KTO)%XINPRH
XACPRH=>PRECIP_MODEL(KTO)%XACPRH

END SUBROUTINE PRECIP_GOTO_MODEL

END MODULE MODD_PRECIP_n
