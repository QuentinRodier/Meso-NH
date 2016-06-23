!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!##################
MODULE MODD_TEB_GREENROOF_OPTION_n
!##################
!
!!****  *MODD_TEB_GREENROOF - declaration of ISBA scheme packed surface parameters for urban green roofs
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
!!      A. Lemonsu *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original       09/2009
!!      C. de Munck     06/2011 
!!      V. Masson       06/2013 splits module in 4
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




TYPE TEB_GREENROOF_OPTIONS_t
!-------------------------------------------------------------------------------
!
! type of initialization : from cover types (ecoclimap) or parameters prescribed
!
  LOGICAL                         :: LPAR_GREENROOF ! T: parameters computed from ecoclimap
!                                                   ! F: they are read in the file
!
! ISBA Scheme Options specific to urban green roofs:
!
  CHARACTER(LEN=3)                :: CISBA_GR       ! type of ISBA version ('2-L' = default, '3-L', 'DIF')
  CHARACTER(LEN=4)                :: CSCOND_GR      ! Thermal conductivity ('DEF '= NP89 implicit method , 
                                                    ! 'PL98' = Peters-Lidard et al. 1998 used for explicit computation of CG)
!
  LOGICAL                          :: LTR_ML_GR
!-------------------------------------------------------------------------------
!
! type of initialization of vegetation: from cover types (ecoclimap) or parameters prescribed
!
  INTEGER                         :: NLAYER_GR       ! number of ground layers
  INTEGER                         :: NTIME_GR        ! number of time data : for VEG, LAI, EMIS, Z0
!
  INTEGER                              :: NLAYER_HORT_GR
  INTEGER                              :: NLAYER_DUN_GR
!
  REAL, POINTER, DIMENSION(:)          :: XSOILGRID_GR        ! Soil layer grid as reference for DIF
!-------------------------------------------------------------------------------
!
! - SGH scheme
!                                                     
  CHARACTER(LEN=4)                :: CRUNOFF_GR      ! surface runoff formulation for green roofs
!                                                    ! 'WSAT'
!                                                    ! 'DT92'
!                                                    ! 'SGH ' Topmodel
!
!SGH scheme and vertical hydrology
!
  CHARACTER(LEN=3)                :: CKSAT_GR        ! ksat
!                                                    ! 'DEF' = default value 
!                                                    ! 'SGH' = profil exponentiel
  CHARACTER(LEN=3)                :: CHORT_GR        ! Horton runoff
!                                                    ! 'DEF' = no Horton runoff
!                                                    ! 'SGH' = Horton runoff
  LOGICAL                         :: LSOC_GR         ! soil organic carbon effect
!                                                    ! False = default value 
!                                                    ! True = SOC profil
!
!-------------------------------------------------------------------------------
!                                 
! Type of green roof (characterization of green roof structure based on GR vegetation)
!
  CHARACTER(LEN=5)                :: CTYP_GR         ! type of green roof
!
!-------------------------------------------------------------------------------
!
END TYPE TEB_GREENROOF_OPTIONS_t



 CONTAINS

!


!

SUBROUTINE TEB_GREENROOF_OPTIONS_INIT(YTEB_GREENROOF_OPTIONS)
TYPE(TEB_GREENROOF_OPTIONS_t), INTENT(INOUT) :: YTEB_GREENROOF_OPTIONS
REAL(KIND=JPRB) :: ZHOOK_HANDLE
IF (LHOOK) CALL DR_HOOK("MODD_TEB_GREENROOF_N:TEB_GREENROOF_INIT",0,ZHOOK_HANDLE)
  NULLIFY(YTEB_GREENROOF_OPTIONS%XSOILGRID_GR)
YTEB_GREENROOF_OPTIONS%LPAR_GREENROOF=.TRUE.
YTEB_GREENROOF_OPTIONS%CISBA_GR=' '
YTEB_GREENROOF_OPTIONS%LTR_ML_GR=.FALSE.
YTEB_GREENROOF_OPTIONS%LSOC_GR=.FALSE.
YTEB_GREENROOF_OPTIONS%CRUNOFF_GR=' '
YTEB_GREENROOF_OPTIONS%CSCOND_GR=' '
YTEB_GREENROOF_OPTIONS%CKSAT_GR=' '
YTEB_GREENROOF_OPTIONS%CHORT_GR=' '
YTEB_GREENROOF_OPTIONS%CTYP_GR=' '
YTEB_GREENROOF_OPTIONS%NLAYER_GR=0
YTEB_GREENROOF_OPTIONS%NLAYER_HORT_GR=0
YTEB_GREENROOF_OPTIONS%NLAYER_DUN_GR=0
YTEB_GREENROOF_OPTIONS%NTIME_GR=0
IF (LHOOK) CALL DR_HOOK("MODD_TEB_GREENROOF_N:TEB_GREENROOF_OPTIONS_INIT",1,ZHOOK_HANDLE)
END SUBROUTINE TEB_GREENROOF_OPTIONS_INIT


END MODULE MODD_TEB_GREENROOF_OPTION_n
