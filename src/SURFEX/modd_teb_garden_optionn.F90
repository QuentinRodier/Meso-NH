!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!##################
MODULE MODD_TEB_GARDEN_OPTION_n
!##################
!
!!****  *MODD_TEB_GARDEN - declaration of packed surface parameters for ISBA scheme
!!
!!    PURPOSE
!!    -------
!
!!
!!**  IMPLICIT ARGUMENTS
!!    ------------------
!!      None 
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!      A. Lemonsu   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original       01/2011
!!      V. Masson      06/2013 splits module in two
!!
!-------------------------------------------------------------------------------
!
!*       0.   DECLARATIONS
!             ------------
!
USE MODD_TYPE_SNOW
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE

TYPE TEB_GARDEN_OPTIONS_t
!-------------------------------------------------------------------------------
!
! type of initialization of vegetation: from cover types (ecoclimap) or parameters prescribed
!
  LOGICAL                              :: LPAR_GARDEN      ! T: parameters computed from ecoclimap
!                                                          ! F: they are read in the file
!
! Number of inside garden vegetation (not TEB) patches and of layers
! 
!
  INTEGER                              :: NGROUND_LAYER    ! number of ground layers
!
  INTEGER                              :: NLAYER_HORT
  INTEGER                              :: NLAYER_DUN
!
  REAL, POINTER, DIMENSION(:)          :: XSOILGRID        ! Soil layer grid as reference for DIF
!
END TYPE TEB_GARDEN_OPTIONS_t
!-------------------------------------------------------------------------------



 CONTAINS

!


!

SUBROUTINE TEB_GARDEN_OPTIONS_INIT(YTEB_GARDEN_OPTIONS)
TYPE(TEB_GARDEN_OPTIONS_t), INTENT(INOUT) :: YTEB_GARDEN_OPTIONS
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK("MODD_TEB_GARDEN_N:TEB_GARDEN_INIT",0,ZHOOK_HANDLE)
  NULLIFY(YTEB_GARDEN_OPTIONS%XSOILGRID)
YTEB_GARDEN_OPTIONS%LPAR_GARDEN=.TRUE.
YTEB_GARDEN_OPTIONS%NGROUND_LAYER=0
YTEB_GARDEN_OPTIONS%NLAYER_HORT=0
YTEB_GARDEN_OPTIONS%NLAYER_DUN=0
IF (LHOOK) CALL DR_HOOK("MODD_TEB_GARDEN_N:TEB_GARDEN_OPTIONS_INIT",1,ZHOOK_HANDLE)
END SUBROUTINE TEB_GARDEN_OPTIONS_INIT


END MODULE MODD_TEB_GARDEN_OPTION_n
