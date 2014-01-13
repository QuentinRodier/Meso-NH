!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     ##################
      MODULE MODE_SNOW3L
!     ##################
!
!!****  *MODE_SNOW * - contains explicit snow (ISBA-ES) characteristics functions
!!                     for total liquid water holding capacity of a snow layer (m)
!!                     and the thermal heat capacity of a layer (J K-1 m-3)
!!
!!    PURPOSE
!!    -------
!
!!**  METHOD
!!    ------
!!    direct calculation
!!
!!    EXTERNAL
!!    --------
!!       
!!    IMPLICIT ARGUMENTS
!!    ------------------ 
!!
!!    REFERENCE
!!    ---------
!!    Boone and Etchevers, J. HydroMeteor., 2001
!!      
!!
!!    AUTHOR
!!    ------
!!	A. Boone       * Meteo France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original        01/08/02
!!      V. Masson       01/2004  add snow grid computations
!!      V. Vionnet      06/2008 -Introduction of Crocus formulation to
!                       calculate maximum liquid water holding capacity
!!                              - New algorithm to compute snow grid :
!                       10 layers
!!                              - Routine to aggregate snow grain type
!                       from 2 layers    
!!                              _ Routine to compute average grain
!                       type when snow depth< 3 cm. 
!     S. Morin          02/2011 - Add routines for Crocus
!     A. Boone          02/2012 - Add optimization of do-loops.
!     M. Lafaysse       01/2013 - Remove SNOWCROWLIQMAX routines (not used)
!----------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!
!
INTERFACE SNOW3LWLIQMAX
  MODULE PROCEDURE SNOW3LWLIQMAX_3D
  MODULE PROCEDURE SNOW3LWLIQMAX_2D
  MODULE PROCEDURE SNOW3LWLIQMAX_1D
END INTERFACE
INTERFACE SNOW3LHOLD
  MODULE PROCEDURE SNOW3LHOLD_3D
  MODULE PROCEDURE SNOW3LHOLD_2D
  MODULE PROCEDURE SNOW3LHOLD_1D
  MODULE PROCEDURE SNOW3LHOLD_0D
END INTERFACE
INTERFACE SNOWCROHOLD
  MODULE PROCEDURE SNOWCROHOLD_3D
  MODULE PROCEDURE SNOWCROHOLD_2D
  MODULE PROCEDURE SNOWCROHOLD_1D
  MODULE PROCEDURE SNOWCROHOLD_0D
END INTERFACE
!
INTERFACE SNOW3LSCAP
  MODULE PROCEDURE SNOW3LSCAP_3D
  MODULE PROCEDURE SNOW3LSCAP_2D
  MODULE PROCEDURE SNOW3LSCAP_1D
  MODULE PROCEDURE SNOW3LSCAP_0D
END INTERFACE
!
INTERFACE SNOW3L_MARBOUTY
  MODULE PROCEDURE SNOW3L_MARBOUTY
END INTERFACE
!
INTERFACE SNOW3LGRID
  MODULE PROCEDURE SNOW3LGRID_2D
  MODULE PROCEDURE SNOW3LGRID_1D
END INTERFACE
!
INTERFACE SNOW3LAGREG
  MODULE PROCEDURE SNOW3LAGREG
END INTERFACE
!
INTERFACE SNOW3LAVGRAIN
  MODULE PROCEDURE SNOW3LAVGRAIN
END INTERFACE
!
INTERFACE SNOW3LDIFTYP
  MODULE PROCEDURE SNOW3LDIFTYP
END INTERFACE
!
!-------------------------------------------------------------------------------
CONTAINS
!
!####################################################################
      FUNCTION SNOW3LWLIQMAX_3D(PSNOWRHO) RESULT(PWLIQMAX)
!
!!    PURPOSE
!!    -------
!     Calculate the maximum liquid water holding capacity of
!     snow layer(s).
!
USE MODD_SNOW_PAR, ONLY : XRHOSMAX_ES, XSNOWRHOHOLD,      &
                          XWSNOWHOLDMAX2, XWSNOWHOLDMAX1
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, DIMENSION(:,:,:), INTENT(IN)                                  :: PSNOWRHO ! (kg/m3)
!
REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2),SIZE(PSNOWRHO,3)) :: PWLIQMAX ! (kg/m3)
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2),SIZE(PSNOWRHO,3)) :: ZHOLDMAXR, ZSNOWRHO
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-----------------------------------------------------------------------
! Evaluate capacity using upper density limit:
!
IF (LHOOK) CALL DR_HOOK('MODE_SNOW3L:SNOW3LWLIQMAX_3D',0,ZHOOK_HANDLE)
ZSNOWRHO(:,:,:) = MIN(XRHOSMAX_ES, PSNOWRHO(:,:,:))
!
! Maximum ratio of liquid to SWE:
!
ZHOLDMAXR(:,:,:) = XWSNOWHOLDMAX1 + (XWSNOWHOLDMAX2-XWSNOWHOLDMAX1)*    &
                  MAX(0.,XSNOWRHOHOLD-ZSNOWRHO(:,:,:))/XSNOWRHOHOLD 
!
! Maximum liquid water holding capacity of the snow (kg/m3):
!
PWLIQMAX(:,:,:) = ZHOLDMAXR(:,:,:)*ZSNOWRHO(:,:,:)           
IF (LHOOK) CALL DR_HOOK('MODE_SNOW3L:SNOW3LWLIQMAX_3D',1,ZHOOK_HANDLE)
!
END FUNCTION SNOW3LWLIQMAX_3D
!####################################################################
      FUNCTION SNOW3LWLIQMAX_2D(PSNOWRHO) RESULT(PWLIQMAX)
!
!!    PURPOSE
!!    -------
!     Calculate the maximum liquid water holding capacity of
!     snow layer(s).
!
USE MODD_SNOW_PAR, ONLY : XRHOSMAX_ES, XSNOWRHOHOLD,      &
                          XWSNOWHOLDMAX2, XWSNOWHOLDMAX1
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, DIMENSION(:,:), INTENT(IN)                   :: PSNOWRHO ! (kg/m3)
!
REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2)) :: PWLIQMAX ! (kg/m3)
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2)) :: ZHOLDMAXR, ZSNOWRHO
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-----------------------------------------------------------------------
! Evaluate capacity using upper density limit:
!
IF (LHOOK) CALL DR_HOOK('MODE_SNOW3L:SNOW3LWLIQMAX_2D',0,ZHOOK_HANDLE)
ZSNOWRHO(:,:) = MIN(XRHOSMAX_ES, PSNOWRHO(:,:))
!
! Maximum ratio of liquid to SWE:
!
ZHOLDMAXR(:,:) = XWSNOWHOLDMAX1 + (XWSNOWHOLDMAX2-XWSNOWHOLDMAX1)*    &
                  MAX(0.,XSNOWRHOHOLD-ZSNOWRHO(:,:))/XSNOWRHOHOLD 
!
! Maximum liquid water holding capacity of the snow (kg/m3):
!
PWLIQMAX(:,:) = ZHOLDMAXR(:,:)*ZSNOWRHO(:,:)               
IF (LHOOK) CALL DR_HOOK('MODE_SNOW3L:SNOW3LWLIQMAX_2D',1,ZHOOK_HANDLE)
!
END FUNCTION SNOW3LWLIQMAX_2D
!####################################################################
      FUNCTION SNOW3LWLIQMAX_1D(PSNOWRHO) RESULT(PWLIQMAX)
!
!!    PURPOSE
!!    -------
!     Calculate the maximum liquid water holding capacity of
!     snow layer(s).
!
USE MODD_SNOW_PAR, ONLY : XRHOSMAX_ES, XSNOWRHOHOLD,      &
                           XWSNOWHOLDMAX2, XWSNOWHOLDMAX1
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, DIMENSION(:), INTENT(IN)  :: PSNOWRHO ! (kg/m3)
!
REAL, DIMENSION(SIZE(PSNOWRHO)) :: PWLIQMAX ! (kg/m3)
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(SIZE(PSNOWRHO)) :: ZHOLDMAXR, ZSNOWRHO
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!----------------------------------------------------------------------
! Evaluate capacity using upper density limit:
!
IF (LHOOK) CALL DR_HOOK('MODE_SNOW3L:SNOW3LWLIQMAX_1D',0,ZHOOK_HANDLE)
ZSNOWRHO(:) = MIN(XRHOSMAX_ES, PSNOWRHO(:))
!
! Maximum ratio of liquid to SWE:
!
ZHOLDMAXR(:) = XWSNOWHOLDMAX1 + (XWSNOWHOLDMAX2-XWSNOWHOLDMAX1)*    &
                  MAX(0.,XSNOWRHOHOLD-ZSNOWRHO(:))/XSNOWRHOHOLD 
!
! Maximum liquid water holding capacity of the snow (kg/m3):
!
PWLIQMAX(:) = ZHOLDMAXR(:)*ZSNOWRHO(:)                
IF (LHOOK) CALL DR_HOOK('MODE_SNOW3L:SNOW3LWLIQMAX_1D',1,ZHOOK_HANDLE)
!
END FUNCTION SNOW3LWLIQMAX_1D
!####################################################################
!####################################################################
      FUNCTION SNOW3LHOLD_3D(PSNOWRHO,PSNOWDZ) RESULT(PWHOLDMAX)
!
!!    PURPOSE
!!    -------
!     Calculate the maximum liquid water holding capacity of
!     snow layer(s).
!
USE MODD_CSTS,     ONLY : XRHOLW
USE MODD_SNOW_PAR, ONLY : XRHOSMAX_ES, XSNOWRHOHOLD,      &
                          XWSNOWHOLDMAX2, XWSNOWHOLDMAX1
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, DIMENSION(:,:,:), INTENT(IN)                 :: PSNOWDZ, PSNOWRHO
!
REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2),SIZE(PSNOWRHO,3)) :: PWHOLDMAX
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2),SIZE(PSNOWRHO,3)) :: ZHOLDMAXR, ZSNOWRHO
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------
! Evaluate capacity using upper density limit:
!
IF (LHOOK) CALL DR_HOOK('MODE_SNOW3L:SNOW3LHOLD_3D',0,ZHOOK_HANDLE)
ZSNOWRHO(:,:,:) = MIN(XRHOSMAX_ES, PSNOWRHO(:,:,:))
!
! Maximum ratio of liquid to SWE:
!
ZHOLDMAXR(:,:,:) = XWSNOWHOLDMAX1 + (XWSNOWHOLDMAX2-XWSNOWHOLDMAX1)*    &
                  MAX(0.,XSNOWRHOHOLD-ZSNOWRHO(:,:,:))/XSNOWRHOHOLD 
!
! Maximum liquid water holding capacity of the snow (m):
!
PWHOLDMAX(:,:,:) = ZHOLDMAXR(:,:,:)*PSNOWDZ(:,:,:)*ZSNOWRHO(:,:,:)/XRHOLW
IF (LHOOK) CALL DR_HOOK('MODE_SNOW3L:SNOW3LHOLD_3D',1,ZHOOK_HANDLE)
!
END FUNCTION SNOW3LHOLD_3D
!####################################################################
      FUNCTION SNOW3LHOLD_2D(PSNOWRHO,PSNOWDZ) RESULT(PWHOLDMAX)
!
!!    PURPOSE
!!    -------
!     Calculate the maximum liquid water holding capacity of
!     snow layer(s).
!
USE MODD_CSTS,     ONLY : XRHOLW
USE MODD_SNOW_PAR, ONLY : XRHOSMAX_ES, XSNOWRHOHOLD,      &
                          XWSNOWHOLDMAX2, XWSNOWHOLDMAX1
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, DIMENSION(:,:), INTENT(IN)                   :: PSNOWDZ, PSNOWRHO
!
REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2)) :: PWHOLDMAX
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2)) :: ZHOLDMAXR, ZSNOWRHO
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-----------------------------------------------------------------------
! Evaluate capacity using upper density limit:
!
IF (LHOOK) CALL DR_HOOK('MODE_SNOW3L:SNOW3LHOLD_2D',0,ZHOOK_HANDLE)
ZSNOWRHO(:,:) = MIN(XRHOSMAX_ES, PSNOWRHO(:,:))
!
! Maximum ratio of liquid to SWE:
!
ZHOLDMAXR(:,:) = XWSNOWHOLDMAX1 + (XWSNOWHOLDMAX2-XWSNOWHOLDMAX1)*    &
                  MAX(0.,XSNOWRHOHOLD-ZSNOWRHO(:,:))/XSNOWRHOHOLD 
!
! Maximum liquid water holding capacity of the snow (m):
!
PWHOLDMAX(:,:) = ZHOLDMAXR(:,:)*PSNOWDZ(:,:)*ZSNOWRHO(:,:)/XRHOLW
IF (LHOOK) CALL DR_HOOK('MODE_SNOW3L:SNOW3LHOLD_2D',1,ZHOOK_HANDLE)
!
END FUNCTION SNOW3LHOLD_2D
!####################################################################
      FUNCTION SNOW3LHOLD_1D(PSNOWRHO,PSNOWDZ) RESULT(PWHOLDMAX)
!
!!    PURPOSE
!!    -------
!     Calculate the maximum liquid water holding capacity of
!     snow layer(s).
!
USE MODD_CSTS,     ONLY : XRHOLW
USE MODD_SNOW_PAR, ONLY : XRHOSMAX_ES, XSNOWRHOHOLD,     &
                          XWSNOWHOLDMAX2, XWSNOWHOLDMAX1 
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, DIMENSION(:), INTENT(IN)                     :: PSNOWDZ, PSNOWRHO
!
REAL, DIMENSION(SIZE(PSNOWRHO))                    :: PWHOLDMAX
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(SIZE(PSNOWRHO))                    :: ZHOLDMAXR, ZSNOWRHO
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-----------------------------------------------------------------------
! Evaluate capacity using upper density limit:
!
IF (LHOOK) CALL DR_HOOK('MODE_SNOW3L:SNOW3LHOLD_1D',0,ZHOOK_HANDLE)
ZSNOWRHO(:) = MIN(XRHOSMAX_ES, PSNOWRHO(:))
!
! Maximum ratio of liquid to SWE:
!
ZHOLDMAXR(:) = XWSNOWHOLDMAX1 + (XWSNOWHOLDMAX2-XWSNOWHOLDMAX1)*     &
                  MAX(0.,XSNOWRHOHOLD-ZSNOWRHO(:))/XSNOWRHOHOLD 
!
! Maximum liquid water holding capacity of the snow (m):
!
PWHOLDMAX(:) = ZHOLDMAXR(:)*PSNOWDZ(:)*ZSNOWRHO(:)/XRHOLW               
IF (LHOOK) CALL DR_HOOK('MODE_SNOW3L:SNOW3LHOLD_1D',1,ZHOOK_HANDLE)
!
END FUNCTION SNOW3LHOLD_1D
!####################################################################
      FUNCTION SNOW3LHOLD_0D(PSNOWRHO,PSNOWDZ) RESULT(PWHOLDMAX)
!
!!    PURPOSE
!!    -------
!     Calculate the maximum liquid water holding capacity of
!     snow layer(s).
!
USE MODD_CSTS,     ONLY : XRHOLW
USE MODD_SNOW_PAR, ONLY : XRHOSMAX_ES, XSNOWRHOHOLD,     &
                          XWSNOWHOLDMAX2, XWSNOWHOLDMAX1
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, INTENT(IN)        :: PSNOWDZ, PSNOWRHO
!
REAL                    :: PWHOLDMAX
!
!*      0.2    declarations of local variables
!
REAL                    :: ZHOLDMAXR, ZSNOWRHO
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
! Evaluate capacity using upper density limit:
!
IF (LHOOK) CALL DR_HOOK('MODE_SNOW3L:SNOW3LHOLD_0D',0,ZHOOK_HANDLE)
ZSNOWRHO = MIN(XRHOSMAX_ES, PSNOWRHO)
!
! Maximum ratio of liquid to SWE:
!
ZHOLDMAXR = XWSNOWHOLDMAX1 + (XWSNOWHOLDMAX2-XWSNOWHOLDMAX1)*     &
                  MAX(0.,XSNOWRHOHOLD-ZSNOWRHO)/XSNOWRHOHOLD 
!
! Maximum liquid water holding capacity of the snow (m):
!
PWHOLDMAX = ZHOLDMAXR*PSNOWDZ*ZSNOWRHO/XRHOLW              
IF (LHOOK) CALL DR_HOOK('MODE_SNOW3L:SNOW3LHOLD_0D',1,ZHOOK_HANDLE)
!
END FUNCTION SNOW3LHOLD_0D
!####################################################################
      FUNCTION SNOWCROHOLD_3D(PSNOWRHO,PSNOWLIQ,PSNOWDZ) RESULT(PWHOLDMAX)
!
!!    PURPOSE
!!    -------
!     Calculate the maximum liquid water holding capacity of
!     snow layer(s).
!
USE MODD_CSTS,     ONLY : XRHOLW,XRHOLI
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, DIMENSION(:,:,:), INTENT(IN)                 :: PSNOWDZ, PSNOWLIQ, PSNOWRHO
!
REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2),SIZE(PSNOWRHO,3)) :: PWHOLDMAX
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
! computation of water holding capacity based on Crocus, 
!taking into account the conversion between wet and dry density - 
!S. Morin/V. Vionnet 2010 12 09

! PWHOLDMAX is expressed in m water for each layer
! In short, PWHOLDMAX = XPERCENTAGEPORE * porosity * PSNOWDZ .
! The porosity is computed as (rho_ice - (rho_snow - lwc))/(rho_ice)
! where everything has to be in kg m-3 units. In practice, since
! PSNOWLIQ is expressed in m water, expressing the lwc in kg m-3
! is achieved as PSNOWLIQ*XRHOLW/PSNOWDZ. After some rearranging one
! obtains the equation given above.
! Note that equation (19) in Vionnet et al., GMD 2012, is wrong,
! because it does not take into account the fact that liquid water
! content has to be substracted from total density to compute the
! porosity.

IF (LHOOK) CALL DR_HOOK('MODE_SNOW3L:SNOWCROHOLD_3D',0,ZHOOK_HANDLE)
PWHOLDMAX(:,:,:) = 0.05/XRHOLI * (PSNOWDZ * (XRHOLI-PSNOWRHO) + PSNOWLIQ*XRHOLW)    
IF (LHOOK) CALL DR_HOOK('MODE_SNOW3L:SNOWCROHOLD_3D',1,ZHOOK_HANDLE)
!
END FUNCTION SNOWCROHOLD_3D
!####################################################################
      FUNCTION SNOWCROHOLD_2D(PSNOWRHO,PSNOWLIQ,PSNOWDZ) RESULT(PWHOLDMAX)
!
!!    PURPOSE
!!    -------
!     Calculate the maximum liquid water holding capacity of
!     snow layer(s).
!
USE MODD_CSTS,     ONLY : XRHOLW,XRHOLI
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, DIMENSION(:,:), INTENT(IN)                   :: PSNOWDZ, PSNOWRHO, PSNOWLIQ
!
REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2)) :: PWHOLDMAX
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
! computation of water holding capacity based on Crocus, 
!taking into account the conversion between wet and dry density - 
!S. Morin/V. Vionnet 2010 12 09

! PWHOLDMAX is expressed in m water for each layer
! In short, PWHOLDMAX = XPERCENTAGEPORE * porosity * PSNOWDZ .
! The porosity is computed as (rho_ice - (rho_snow - lwc))/(rho_ice)
! where everything has to be in kg m-3 units. In practice, since
! PSNOWLIQ is expressed in m water, expressing the lwc in kg m-3
! is achieved as PSNOWLIQ*XRHOLW/PSNOWDZ. After some rearranging one
! obtains the equation given above.
! Note that equation (19) in Vionnet et al., GMD 2012, is wrong,
! because it does not take into account the fact that liquid water
! content has to be substracted from total density to compute the
! porosity.

IF (LHOOK) CALL DR_HOOK('MODE_SNOW3L:SNOWCROHOLD_2D',0,ZHOOK_HANDLE)
PWHOLDMAX(:,:) = 0.05/XRHOLI * (PSNOWDZ * (XRHOLI-PSNOWRHO) + PSNOWLIQ*XRHOLW)    
IF (LHOOK) CALL DR_HOOK('MODE_SNOW3L:SNOWCROHOLD_2D',1,ZHOOK_HANDLE)
!
END FUNCTION SNOWCROHOLD_2D
!####################################################################
!####################################################################
!####################################################################
      FUNCTION SNOWCROHOLD_1D(PSNOWRHO,PSNOWLIQ,PSNOWDZ) RESULT(PWHOLDMAX)
!
!!    PURPOSE
!!    -------
!     Calculate the maximum liquid water holding capacity of
!     snow layer(s).
!
USE MODD_CSTS,     ONLY : XRHOLW,XRHOLI
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, DIMENSION(:), INTENT(IN)                     :: PSNOWDZ, PSNOWRHO, PSNOWLIQ
!
REAL, DIMENSION(SIZE(PSNOWRHO))                    :: PWHOLDMAX
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
! computation of water holding capacity based on Crocus, 
!taking into account the conversion between wet and dry density -
!S. Morin/V. Vionnet 2010 12 09

! PWHOLDMAX is expressed in m water for each layer
! In short, PWHOLDMAX = XPERCENTAGEPORE * porosity * PSNOWDZ .
! The porosity is computed as (rho_ice - (rho_snow - lwc))/(rho_ice)
! where everything has to be in kg m-3 units. In practice, since
! PSNOWLIQ is expressed in m water, expressing the lwc in kg m-3
! is achieved as PSNOWLIQ*XRHOLW/PSNOWDZ. After some rearranging one
! obtains the equation given above.
! Note that equation (19) in Vionnet et al., GMD 2012, is wrong,
! because it does not take into account the fact that liquid water
! content has to be substracted from total density to compute the
! porosity.

IF (LHOOK) CALL DR_HOOK('MODE_SNOW3L:SNOWCROHOLD_1D',0,ZHOOK_HANDLE)
PWHOLDMAX(:) = 0.05/XRHOLI * (PSNOWDZ * (XRHOLI-PSNOWRHO) + PSNOWLIQ*XRHOLW)  
IF (LHOOK) CALL DR_HOOK('MODE_SNOW3L:SNOWCROHOLD_1D',1,ZHOOK_HANDLE)
!
END FUNCTION SNOWCROHOLD_1D
!####################################################################
      FUNCTION SNOWCROHOLD_0D(PSNOWRHO,PSNOWLIQ,PSNOWDZ) RESULT(PWHOLDMAX)
!
!!    PURPOSE
!!    -------
!     Calculate the maximum liquid water holding capacity of
!     snow layer(s).
!
USE MODD_CSTS,     ONLY : XRHOLW,XRHOLI
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, INTENT(IN)        :: PSNOWDZ, PSNOWRHO, PSNOWLIQ
!
REAL                    :: PWHOLDMAX
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
! computation of water holding capacity based on Crocus, 
!taking into account the conversion between wet and dry density - 
!S. Morin/V. Vionnet 2010 12 09

! PWHOLDMAX is expressed in m water for each layer
! In short, PWHOLDMAX = XPERCENTAGEPORE * porosity * PSNOWDZ .
! The porosity is computed as (rho_ice - (rho_snow - lwc))/(rho_ice)
! where everything has to be in kg m-3 units. In practice, since
! PSNOWLIQ is expressed in m water, expressing the lwc in kg m-3
! is achieved as PSNOWLIQ*XRHOLW/PSNOWDZ. After some rearranging one
! obtains the equation given above.
! Note that equation (19) in Vionnet et al., GMD 2012, is wrong,
! because it does not take into account the fact that liquid water
! content has to be substracted from total density to compute the
! porosity.

IF (LHOOK) CALL DR_HOOK('MODE_SNOW3L:SNOWCROHOLD_0D',0,ZHOOK_HANDLE)
PWHOLDMAX = 0.05/XRHOLI * (PSNOWDZ * (XRHOLI-PSNOWRHO) + PSNOWLIQ*XRHOLW) 
IF (LHOOK) CALL DR_HOOK('MODE_SNOW3L:SNOWCROHOLD_0D',1,ZHOOK_HANDLE)
!
END FUNCTION SNOWCROHOLD_0D
!####################################################################
!####################################################################
!####################################################################
      FUNCTION SNOW3LSCAP_3D(PSNOWRHO) RESULT(PSCAP)
!
!!    PURPOSE
!!    -------
!     Calculate the heat capacity of a snow layer.
!
USE MODD_CSTS,ONLY : XCI
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, DIMENSION(:,:,:), INTENT(IN)                                  :: PSNOWRHO
!
REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2),SIZE(PSNOWRHO,3)) :: PSCAP
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!     The method of Verseghy (1991), Int. J. Climat., 11, 111-133.
!
IF (LHOOK) CALL DR_HOOK('MODE_SNOW3L:SNOW3LSCAP_3D',0,ZHOOK_HANDLE)
PSCAP(:,:,:) = PSNOWRHO(:,:,:)*XCI      ! (J K-1 m-3)
IF (LHOOK) CALL DR_HOOK('MODE_SNOW3L:SNOW3LSCAP_3D',1,ZHOOK_HANDLE)
!
END FUNCTION SNOW3LSCAP_3D
!####################################################################
      FUNCTION SNOW3LSCAP_2D(PSNOWRHO) RESULT(PSCAP)
!
!!    PURPOSE
!!    -------
!     Calculate the heat capacity of a snow layer.
!
USE MODD_CSTS,ONLY : XCI
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, DIMENSION(:,:), INTENT(IN)                   :: PSNOWRHO
!
REAL, DIMENSION(SIZE(PSNOWRHO,1),SIZE(PSNOWRHO,2)) :: PSCAP
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!     The method of Verseghy (1991), Int. J. Climat., 11, 111-133.
!
IF (LHOOK) CALL DR_HOOK('MODE_SNOW3L:SNOW3LSCAP_2D',0,ZHOOK_HANDLE)
PSCAP(:,:) = PSNOWRHO(:,:)*XCI      ! (J K-1 m-3)
IF (LHOOK) CALL DR_HOOK('MODE_SNOW3L:SNOW3LSCAP_2D',1,ZHOOK_HANDLE)
!
END FUNCTION SNOW3LSCAP_2D
!####################################################################
      FUNCTION SNOW3LSCAP_1D(PSNOWRHO) RESULT(PSCAP)
!
!!    PURPOSE
!!    -------
!     Calculate the heat capacity of a snow layer.
!
USE MODD_CSTS,ONLY : XCI
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, DIMENSION(:), INTENT(IN)                   :: PSNOWRHO
!
REAL, DIMENSION(SIZE(PSNOWRHO))                  :: PSCAP
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!     The method of Verseghy (1991), Int. J. Climat., 11, 111-133.
!
IF (LHOOK) CALL DR_HOOK('MODE_SNOW3L:SNOW3LSCAP_1D',0,ZHOOK_HANDLE)
PSCAP(:) = PSNOWRHO(:)*XCI      ! (J K-1 m-3)
IF (LHOOK) CALL DR_HOOK('MODE_SNOW3L:SNOW3LSCAP_1D',1,ZHOOK_HANDLE)
!
END FUNCTION SNOW3LSCAP_1D
!####################################################################
      FUNCTION SNOW3LSCAP_0D(PSNOWRHO) RESULT(PSCAP)
!
!!    PURPOSE
!!    -------
!     Calculate the heat capacity of a snow layer.
!
USE MODD_CSTS,ONLY : XCI
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, INTENT(IN)       :: PSNOWRHO
!
REAL                   :: PSCAP
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!     The method of Verseghy (1991), Int. J. Climat., 11, 111-133.
!
IF (LHOOK) CALL DR_HOOK('MODE_SNOW3L:SNOW3LSCAP_0D',0,ZHOOK_HANDLE)
PSCAP = PSNOWRHO*XCI      ! (J K-1 m-3)
IF (LHOOK) CALL DR_HOOK('MODE_SNOW3L:SNOW3LSCAP_0D',1,ZHOOK_HANDLE)
!
END FUNCTION SNOW3LSCAP_0D
!
!####################################################################
!####################################################################
!####################################################################
      FUNCTION SNOW3L_MARBOUTY(PSNOWRHO,PSNOWTEMP,PGRADT) RESULT(PDANGL) 
!**** *ZDANGL* - CROISSANCE DES GRAINS NON DENDRITIQUES ET ANGULEUX .
!              - GROWTH RATES FOR NON DENDRITIC GRAINS WITH SPHERICITY=0 


!     OBJET.
!     ------

!**   INTERFACE.
!     ----------

!     *ZDANGL*(PST,PSRO,PGRADT)*

!        *PST*     -  TEMPERATURE DE LA STRATE DE NEIGE.
!        *PSRO*    -  MASSE VOLUMIQUE DE LA STRATE DE NEIGE.
!        *PGRADT*  -  GRADIENT DE TEMPERATURE AFFECTANT LA STRATE DE NEIGE.

!     METHODE.
!     --------
!     THE FUNCTION RETURN A VALUE BETWEEN 0 AND 1 WHICH IS USED IN THE DETERMINATION OF THE 
!     GROWTH RATE FOR THE CONSIDERED LAYER.
!     THIS VALUE (WITHOUT UNIT) IS THE PRODUCT OF 3 FUNCTIONS (WHICH HAVE THEIR SOLUTIONS BETWEEN 0 AND 1) :
!     F(TEMPERATURE) * H(DENSITY) * G(TEMPERATURE GRADIENT)

!     EXTERNES.
!     ---------

!     REFERENCES.
!     -----------
!        MARBOUTY D (1980) AN EXPERIMENTAL STUDY OF TEMPERATURE GRADIENT 
!                          METAMORPHISM J GLACIOLOGY

!     AUTEURS.
!     --------
!        DOMINIQUE MARBOUTY (FMARBO/GMARBO/HMARBO).

!     MODIFICATIONS.
!     --------------
!        08/95: YANNICK DANIELOU - CODAGE A LA NORME DOCTOR.
!        03/06: JM WILLEMET      - F90 AND SI UNITS
!        01/08: JM WILLEMET      - ERROR ON THE FORMULATION OF G FUNCTION (WITH GRADIENT) IS CORRECTED 

USE MODD_CSTS, ONLY : XTT
USE MODD_SNOW_METAMO  
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE


!     DECLARATIONS.
!     -------------
REAL ,INTENT(IN) :: PSNOWTEMP,PSNOWRHO,PGRADT
REAL             :: PDANGL
REAL(KIND=JPRB) :: ZHOOK_HANDLE


IF (LHOOK) CALL DR_HOOK('MODE_SNOW3L:SNOW3L_MARBOUTY',0,ZHOOK_HANDLE)
PDANGL=0.0
! INFLUENCE DE LA TEMPERATURE /TEMPERATURE INFLUENCE.

IF(PSNOWTEMP >= XTT-VTANG1)THEN
  IF(PSNOWTEMP >= XTT-VTANG2)THEN
    PDANGL=VTANG4+VTANG5*(XTT-PSNOWTEMP)/VTANG6
  ELSEIF(PSNOWTEMP >= XTT-VTANG3)THEN
    PDANGL=VTANG7-VTANG8*(XTT-VTANG2-PSNOWTEMP)/VTANG9
  ELSE
    PDANGL=VTANGA-VTANGB*(XTT-VTANG3-PSNOWTEMP)/VTANGC
  ENDIF

! INFLUENCE DE LA MASSE VOLUMIQUE / DENSITY INFLUENCE.

  IF (PSNOWRHO <= VRANG1) THEN
    IF(PSNOWRHO > VRANG2) THEN
      PDANGL=PDANGL*(1.-(PSNOWRHO-VRANG2)/(VRANG1-VRANG2))
    ENDIF
!   INFLUENCE DU GRADIENT DE TEMPERATURE / TEMPERATURE GRADIENT INFLUENCE.

    IF(PGRADT <= VGANG1)THEN
      IF(PGRADT <= VGANG2)THEN
        PDANGL=PDANGL*VGANG5*(PGRADT-VGANG6)/(VGANG2-VGANG6)
      ELSEIF(PGRADT <= VGANG3)THEN
        PDANGL = PDANGL*(VGANG7 + VGANG8*(PGRADT-VGANG2)/(VGANG3-VGANG2))
      ELSEIF(PGRADT <= VGANG4)THEN
        PDANGL = PDANGL*(VGANG9 + VGANGA*(PGRADT-VGANG3)/(VGANG4-VGANG3))
      ELSE
        PDANGL = PDANGL*(VGANGB + VGANGC*(PGRADT-VGANG4)/(VGANG1-VGANG4))
      ENDIF
    ENDIF
  ELSE
    PDANGL=0.
  ENDIF
ELSE
  PDANGL=0.
ENDIF                   
IF (LHOOK) CALL DR_HOOK('MODE_SNOW3L:SNOW3L_MARBOUTY',1,ZHOOK_HANDLE)
END FUNCTION SNOW3L_MARBOUTY     
        
!####################################################################
!####################################################################
!####################################################################

      SUBROUTINE SNOW3LGRID_2D(PSNOWDZ,PSNOW)
!
!!    PURPOSE
!!    -------
!     Once during each time step, update grid to maintain
!     grid proportions. Similar to approach of Lynch-Steiglitz,
!     1994, J. Clim., 7, 1842-1855. Corresponding mass and
!     heat adjustments made directly after the call to this
!     routine. 3 grid configurations:
!     1) for very thin snow, constant grid spacing
!     2) for intermediate thicknesses, highest resolution at soil/snow
!        interface and at the snow/atmosphere interface
!     3) for deep snow, vertical resoution finest at snow/atmosphere
!        interface (set to a constant value) and increases with snow depth.
!        Second layer can't be more than an order of magnitude thicker
!        than surface layer.
!
!
USE MODD_SURF_PAR,   ONLY : XUNDEF
USE MODD_SNOW_PAR,   ONLY : XSNOWCRITD
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, DIMENSION(:), INTENT(IN)    :: PSNOW
!
REAL, DIMENSION(:,:), INTENT(OUT) :: PSNOWDZ
!
!*      0.1    declarations of local variables
!
INTEGER                           :: JJ, JI
!
INTEGER                           :: INLVLS, INI
!   
REAL, DIMENSION(SIZE(PSNOW))      :: ZWORK
!
! ISBA-ES snow grid parameters
!
REAL, PARAMETER, DIMENSION(3)     :: ZSGCOEF1  = (/0.25, 0.50, 0.25/) 
REAL, PARAMETER, DIMENSION(2)     :: ZSGCOEF2  = (/0.05, 0.34/)       
REAL, PARAMETER, DIMENSION(10)    :: ZSGCOEF3  = (/0.025, 0.033, 0.043,&
  0.055, 0.071, 0.091, 0.117, 0.150, 0.193, 0.247/) 
      
! Minimum total snow depth at which surface layer thickness is constant:
!
REAL, PARAMETER                   :: ZSNOWTRANS = 0.20                ! (m)
REAL, PARAMETER                   :: ZSNOWTRANS1 = 0.40                ! (m)
REAL, PARAMETER                   :: ZSNOWTRANS2 = 0.6061                ! (m)
REAL, PARAMETER                   :: ZSNOWTRANS3 = 0.7143               ! (m)
REAL, PARAMETER                   :: ZSNOWTRANS4 = 0.9259                ! (m)
REAL, PARAMETER                   :: ZSNOWTRANS5 = 1.4493                ! (m)
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
! 0. Initialization:
! ------------------
!
IF (LHOOK) CALL DR_HOOK('MODE_SNOW3L:SNOW3LGRID_2D',0,ZHOOK_HANDLE)
INLVLS = SIZE(PSNOWDZ(:,:),2)
INI    = SIZE(PSNOWDZ(:,:),1)
!
ZWORK(:)  = 0.0
!
! 1. Calculate current grid for 3-layer (default) configuration):
! ---------------------------------------------------------------
! Based on formulation of Lynch-Stieglitz (1994)
! except for 3 modifications: 
! i) smooth transition here at ZSNOWTRANS
! ii) constant ratio for very thin snow:
! iii) ratio of layer 2 to surface layer <= 10
!
IF(INLVLS == 3)THEN
!
   WHERE(PSNOW <= XSNOWCRITD+0.01)
      PSNOWDZ(:,1) = MIN(0.01, PSNOW(:)/INLVLS)
      PSNOWDZ(:,3) = MIN(0.01, PSNOW(:)/INLVLS)
      PSNOWDZ(:,2) = PSNOW(:) - PSNOWDZ(:,1) - PSNOWDZ(:,3)
   END WHERE
!
   WHERE(PSNOW <= ZSNOWTRANS .AND. PSNOW > XSNOWCRITD+0.01)
      PSNOWDZ(:,1) = PSNOW(:)*ZSGCOEF1(1)
      PSNOWDZ(:,2) = PSNOW(:)*ZSGCOEF1(2)
      PSNOWDZ(:,3) = PSNOW(:)*ZSGCOEF1(3)
   END WHERE
!
   WHERE(PSNOW > ZSNOWTRANS)
      PSNOWDZ(:,1) = ZSGCOEF2(1)
      PSNOWDZ(:,2) = (PSNOW(:)-ZSGCOEF2(1))*ZSGCOEF2(2) + ZSGCOEF2(1)
!
! When using simple finite differences, limit the thickness
! factor between the top and 2nd layers to at most 10
! 
      PSNOWDZ(:,2) = MIN(10*ZSGCOEF2(1),  PSNOWDZ(:,2))
      PSNOWDZ(:,3) = PSNOW(:) - PSNOWDZ(:,2) - PSNOWDZ(:,1)
   END WHERE
!
! 2. For more than 3-layers:
! --------------------------
! For the case when more than 3 layers are to be used, specifiy how
! grid should be defined here. For now, a very simple arbitrary method
! herein. WARNING: Detailed testing using more than 3-layers has not been done
! to date, only minor tests.
!
ELSE IF(INLVLS>3 .AND. INLVLS<10) THEN
      DO JJ=1,INLVLS
         DO JI=1,INI
            PSNOWDZ(JI,JJ)  = PSNOW(JI)/INLVLS
         ENDDO
      ENDDO
!
     PSNOWDZ(:,INLVLS) = PSNOWDZ(:,INLVLS) + (PSNOWDZ(:,1) - MIN(0.05, PSNOWDZ(:,1)))
     PSNOWDZ(:,1)      = MIN(0.05, PSNOWDZ(:,1))
! ajout EB pour permettre > 10
ELSE IF(INLVLS==10) THEN 
!
!   
!    DO JJ=1,INLVLS
!        WHERE(PSNOW <=XSNOWCRITD+0.07 )
!        PSNOWDZ(:,JJ) = PSNOW(:)/INLVLS
!        ENDWHERE
!        WHERE(PSNOW >XSNOWCRITD+0.07)
!        PSNOWDZ(:,JJ) = (PSNOW(:)-1)*ZSGCOEF3(JJ)
!        ENDWHERE
!    END DO
!        
!     
!   WHERE(PSNOW <= ZSNOWTRANS2 .AND. PSNOW > ZSNOWTRANS1)
!        PSNOWDZ(:,INLVLS)=PSNOWDZ(:,INLVLS)-0.01+PSNOWDZ(:,1)
!        PSNOWDZ(:,1)=0.01
!   END WHERE
!
!    WHERE(PSNOW <= ZSNOWTRANS3 .AND. PSNOW > ZSNOWTRANS2)
!        PSNOWDZ(:,INLVLS)=PSNOWDZ(:,INLVLS)+(PSNOWDZ(:,1)-0.01)+(PSNOWDZ(:,2)-0.02)
!        PSNOWDZ(:,1)=0.01
!        PSNOWDZ(:,2)=0.02
!   END WHERE
!
!    WHERE(PSNOW <= ZSNOWTRANS4 .AND. PSNOW > ZSNOWTRANS3)
!        PSNOWDZ(:,INLVLS)=PSNOWDZ(:,INLVLS)+(PSNOWDZ(:,1)-0.01)+(PSNOWDZ(:,2)-0.02)+(PSNOWDZ(:,3)-0.03)
!        PSNOWDZ(:,1)=0.01
!        PSNOWDZ(:,2)=0.02
!        PSNOWDZ(:,3)=0.03
!   END WHERE
!
!    WHERE(PSNOW <= ZSNOWTRANS5 .AND. PSNOW > ZSNOWTRANS4)
!        PSNOWDZ(:,INLVLS)=PSNOWDZ(:,INLVLS)+(PSNOWDZ(:,1)-0.01)+(PSNOWDZ(:,2)-0.02)+(PSNOWDZ(:,3)-0.03)&
!        &+(PSNOWDZ(:,4)-0.05) 
!        PSNOWDZ(:,1)=0.01
!        PSNOWDZ(:,2)=0.02
!        PSNOWDZ(:,3)=0.03
!        PSNOWDZ(:,4)=0.05
!   END WHERE
!        
!    WHERE(PSNOW > ZSNOWTRANS5)
!
!         PSNOWDZ(:,INLVLS) = PSNOWDZ(:,INLVLS)+(PSNOWDZ(:,1)-0.01)+(PSNOWDZ(:,2)-0.02)+(PSNOWDZ(:,3)-0.03)+&
!&(PSNOWDZ(:,4)-0.05)+(PSNOWDZ(:,5)-0.1)
!
!        PSNOWDZ(:,1)=0.01
!        PSNOWDZ(:,2)=0.02
!        PSNOWDZ(:,3)=0.03
!        PSNOWDZ(:,4)=0.05
!        PSNOWDZ(:,5)=0.1
!                
!     END WHERE
        
     WHERE(PSNOW <= XSNOWCRITD+0.07)
      PSNOWDZ(:,1) = MIN(0.01, PSNOW(:)/INLVLS)
      PSNOWDZ(:,2) = MIN(0.01, PSNOW(:)/INLVLS)
      PSNOWDZ(:,3) = MIN(0.01, PSNOW(:)/INLVLS)
      PSNOWDZ(:,4) = MIN(0.01, PSNOW(:)/INLVLS)
      PSNOWDZ(:,5) = MIN(0.01, PSNOW(:)/INLVLS)
      PSNOWDZ(:,6) = MIN(0.01, PSNOW(:)/INLVLS)
      PSNOWDZ(:,7) = MIN(0.01, PSNOW(:)/INLVLS)
      PSNOWDZ(:,8) = MIN(0.01, PSNOW(:)/INLVLS)
      PSNOWDZ(:,9) = MIN(0.01, PSNOW(:)/INLVLS)
      PSNOWDZ(:,INLVLS) = PSNOW(:) - SUM(PSNOWDZ(:,1:9),2)
     END WHERE

     WHERE(PSNOW > XSNOWCRITD+0.07 .AND. PSNOW<=0.19)
      PSNOWDZ(:,1) = 0.01 
      PSNOWDZ(:,2) = 0.01+(PSNOW(:)-0.1)/9
      PSNOWDZ(:,3) = 0.01+(PSNOW(:)-0.1)/9
      PSNOWDZ(:,4) = 0.01+(PSNOW(:)-0.1)/9
      PSNOWDZ(:,5) = 0.01+(PSNOW(:)-0.1)/9 
      PSNOWDZ(:,6) = 0.01+(PSNOW(:)-0.1)/9
      PSNOWDZ(:,7) = 0.01+(PSNOW(:)-0.1)/9
      PSNOWDZ(:,8) = 0.01+(PSNOW(:)-0.1)/9
      PSNOWDZ(:,9) = 0.01+(PSNOW(:)-0.1)/9
      PSNOWDZ(:,INLVLS) = PSNOW(:) - SUM(PSNOWDZ(:,1:9),2)
     END WHERE

     WHERE(PSNOW > 0.19 .AND. PSNOW<=0.27)
      PSNOWDZ(:,1) = 0.01 
      PSNOWDZ(:,2) = 0.02
      PSNOWDZ(:,3) = 0.02+(PSNOW(:)-0.19)/8
      PSNOWDZ(:,4) = 0.02+(PSNOW(:)-0.19)/8
      PSNOWDZ(:,5) = 0.02+(PSNOW(:)-0.19)/8 
      PSNOWDZ(:,6) = 0.02+(PSNOW(:)-0.19)/8
      PSNOWDZ(:,7) = 0.02+(PSNOW(:)-0.19)/8
      PSNOWDZ(:,8) = 0.02+(PSNOW(:)-0.19)/8
      PSNOWDZ(:,9) = 0.02+(PSNOW(:)-0.19)/8
      PSNOWDZ(:,INLVLS) = PSNOW(:) - SUM(PSNOWDZ(:,1:9),2)
     END WHERE

      WHERE(PSNOW > 0.27 .AND. PSNOW<=0.41)
      PSNOWDZ(:,1) = 0.01 
      PSNOWDZ(:,2) = 0.02
      PSNOWDZ(:,3) = 0.03
      PSNOWDZ(:,4) = 0.03+(PSNOW(:)-0.27)/7
      PSNOWDZ(:,5) = 0.03+(PSNOW(:)-0.27)/7 
      PSNOWDZ(:,6) = 0.03+(PSNOW(:)-0.27)/7
      PSNOWDZ(:,7) = 0.03+(PSNOW(:)-0.27)/7
      PSNOWDZ(:,8) = 0.03+(PSNOW(:)-0.27)/7
      PSNOWDZ(:,9) = 0.03+(PSNOW(:)-0.27)/7
      PSNOWDZ(:,INLVLS) = PSNOW(:) - SUM(PSNOWDZ(:,1:9),2)
     END WHERE

      WHERE(PSNOW > 0.41 .AND. PSNOW<=0.71)
      PSNOWDZ(:,1) = 0.01 
      PSNOWDZ(:,2) = 0.02
      PSNOWDZ(:,3) = 0.03
      PSNOWDZ(:,4) = 0.05
      PSNOWDZ(:,5) = 0.05+(PSNOW(:)-0.41)/6 
      PSNOWDZ(:,6) = 0.05+(PSNOW(:)-0.41)/6
      PSNOWDZ(:,7) = 0.05+(PSNOW(:)-0.41)/6
      PSNOWDZ(:,8) = 0.05+(PSNOW(:)-0.41)/6
      PSNOWDZ(:,9) = 0.05+(PSNOW(:)-0.41)/6
      PSNOWDZ(:,INLVLS) = PSNOW(:) - SUM(PSNOWDZ(:,1:9),2)
     END WHERE
  
      WHERE(PSNOW > 0.71)
      PSNOWDZ(:,1) = 0.01 
      PSNOWDZ(:,2) = 0.02
      PSNOWDZ(:,3) = 0.03
      PSNOWDZ(:,4) = 0.05
      PSNOWDZ(:,5) = 0.1
      PSNOWDZ(:,6) = 0.1+(PSNOW(:)-0.71)/5
      PSNOWDZ(:,7) = 0.1+(PSNOW(:)-0.71)/5
      PSNOWDZ(:,8) = 0.1+(PSNOW(:)-0.71)/5
      PSNOWDZ(:,9) = 0.1+(PSNOW(:)-0.71)/5
      PSNOWDZ(:,INLVLS) = PSNOW(:) - SUM(PSNOWDZ(:,1:9),2)
     END WHERE
       
! ajout EB pour permettre cas INLVLS > 10
ELSE   
      PSNOWDZ(:,1) = MIN(0.02,PSNOW(:)/INLVLS) 
      PSNOWDZ(:,2) = MIN(0.01*PSNOW(:),PSNOW(:)/INLVLS)
      PSNOWDZ(:,3) = MIN(0.02*PSNOW(:),PSNOW(:)/INLVLS)
      PSNOWDZ(:,4) = MIN(0.03*PSNOW(:),PSNOW(:)/INLVLS)
      PSNOWDZ(:,5) = MIN(0.05*PSNOW(:),PSNOW(:)/INLVLS)
      PSNOWDZ(:,INLVLS)=MIN(0.05*PSNOW(:),PSNOW(:)/INLVLS) 
      ZWORK(:) = SUM(PSNOWDZ(:,1:5),2)
      DO JJ=6,INLVLS-1,1
         DO JI=1,INI
            PSNOWDZ(JI,JJ) = (PSNOW(JI) - ZWORK(JI) -PSNOWDZ(JI,INLVLS)) &
                 /(INLVLS-6) 
         ENDDO
      ENDDO
!
ENDIF
!
DO JJ=1,INLVLS
   DO JI=1,INI
      IF(PSNOW(JI)==XUNDEF)THEN
         PSNOWDZ(JI,JJ) = XUNDEF
      ENDIF
   ENDDO
ENDDO
!
IF (LHOOK) CALL DR_HOOK('MODE_SNOW3L:SNOW3LGRID_2D',1,ZHOOK_HANDLE)
!
END SUBROUTINE SNOW3LGRID_2D
!####################################################################
!####################################################################
!####################################################################

      SUBROUTINE SNOW3LGRID_1D(PSNOWDZ,PSNOW)
!
!!    PURPOSE
!!    -------
!     Once during each time step, update grid to maintain
!     grid proportions. Similar to approach of Lynch-Steiglitz,
!     1994, J. Clim., 7, 1842-1855. Corresponding mass and
!     heat adjustments made directly after the call to this
!     routine. 3 grid configurations:
!     1) for very thin snow, constant grid spacing
!     2) for intermediate thicknesses, highest resolution at soil/snow
!        interface and at the snow/atmosphere interface
!     3) for deep snow, vertical resoution finest at snow/atmosphere
!        interface (set to a constant value) and increases with snow depth.
!        Second layer can't be more than an order of magnitude thicker
!        than surface layer.
!
!
USE MODD_SURF_PAR,   ONLY : XUNDEF
USE MODD_SNOW_PAR,   ONLY : XSNOWCRITD
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL, INTENT(IN)    :: PSNOW
!
REAL, DIMENSION(:), INTENT(OUT) :: PSNOWDZ
!
!*      0.1    declarations of local variables
!
INTEGER JJ
!
INTEGER                           :: INLVLS
!
REAL                              :: ZWORK
!
! ISBA-ES snow grid parameters
!
REAL, PARAMETER, DIMENSION(3)     :: ZSGCOEF1  = (/0.25, 0.50, 0.25/) 
REAL, PARAMETER, DIMENSION(2)     :: ZSGCOEF2  = (/0.05, 0.34/)       
REAL, PARAMETER, DIMENSION(10)    :: ZSGCOEF3  = (/0.025, 0.033, 0.043,&
  0.055, 0.071, 0.091, 0.117, 0.150, 0.193, 0.247/) 
      
! Minimum total snow depth at which surface layer thickness is constant:
!
REAL, PARAMETER                   :: ZSNOWTRANS  = 0.20                ! (m)
REAL, PARAMETER                   :: ZSNOWTRANS1 = 0.40                ! (m)
REAL, PARAMETER                   :: ZSNOWTRANS2 = 0.6061                ! (m)
REAL, PARAMETER                   :: ZSNOWTRANS3 = 0.7143               ! (m)
REAL, PARAMETER                   :: ZSNOWTRANS4 = 0.9259                ! (m)
REAL, PARAMETER                   :: ZSNOWTRANS5 = 1.4493                ! (m)
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
! 0. Initialization:
! ------------------
!
IF (LHOOK) CALL DR_HOOK('MODE_SNOW3L:SNOW3LGRID_1D',0,ZHOOK_HANDLE)
INLVLS = SIZE(PSNOWDZ(:),1)
!
! 1. Calculate current grid for 3-layer (default) configuration):
! ---------------------------------------------------------------
! Based on formulation of Lynch-Stieglitz (1994)
! except for 3 modifications: 
! i) smooth transition here at ZSNOWTRANS
! ii) constant ratio for very thin snow:
! iii) ratio of layer 2 to surface layer <= 10
!
IF(INLVLS == 3)THEN
!
   IF(PSNOW <= XSNOWCRITD+0.01)THEN
      PSNOWDZ(1) = MIN(0.01, PSNOW/INLVLS)
      PSNOWDZ(3) = MIN(0.01, PSNOW/INLVLS)
      PSNOWDZ(2) = PSNOW - PSNOWDZ(1) - PSNOWDZ(3)
   ENDIF
!
   IF(PSNOW <= ZSNOWTRANS .AND. PSNOW > XSNOWCRITD+0.01)THEN
      PSNOWDZ(1) = PSNOW*ZSGCOEF1(1)
      PSNOWDZ(2) = PSNOW*ZSGCOEF1(2)
      PSNOWDZ(3) = PSNOW*ZSGCOEF1(3)
   ENDIF
!
   IF(PSNOW > ZSNOWTRANS)THEN
      PSNOWDZ(1) = ZSGCOEF2(1)
      PSNOWDZ(2) = (PSNOW-ZSGCOEF2(1))*ZSGCOEF2(2) + ZSGCOEF2(1)
!
! When using simple finite differences, limit the thickness
! factor between the top and 2nd layers to at most 10
! 
      PSNOWDZ(2) = MIN(10*ZSGCOEF2(1),  PSNOWDZ(2))
      PSNOWDZ(3) = PSNOW - PSNOWDZ(2) - PSNOWDZ(1)
   END IF
!
!
!plm
ELSE IF(INLVLS>3 .AND. INLVLS<10) THEN
      DO JJ=1,INLVLS
         PSNOWDZ(JJ)  = PSNOW/INLVLS
      ENDDO
!
     PSNOWDZ(INLVLS) = PSNOWDZ(INLVLS) + (PSNOWDZ(1) - MIN(0.05, PSNOWDZ(1)))
     PSNOWDZ(1)      = MIN(0.05, PSNOWDZ(1))
! ajout EB pour permettre > 10
ELSE IF(INLVLS==10) THEN 
! plm
! ELSE 
!
! 2. For more than 3-layers:
! --------------------------
! For the case when more than 3 layers are to be used, specifiy how
! grid should be defined here. For now, a very simple arbitrary method
! herein. WARNING: Detailed testing using more than 3-layers has not been done
! to date, only minor tests.
!           
     IF(PSNOW <= XSNOWCRITD+0.07)THEN
      PSNOWDZ(1) = MIN(0.01, PSNOW/INLVLS)
      PSNOWDZ(2) = MIN(0.01, PSNOW/INLVLS)
      PSNOWDZ(3) = MIN(0.01, PSNOW/INLVLS)
      PSNOWDZ(4) = MIN(0.01, PSNOW/INLVLS)
      PSNOWDZ(5) = MIN(0.01, PSNOW/INLVLS)
      PSNOWDZ(6) = MIN(0.01, PSNOW/INLVLS)
      PSNOWDZ(7) = MIN(0.01, PSNOW/INLVLS)
      PSNOWDZ(8) = MIN(0.01, PSNOW/INLVLS)
      PSNOWDZ(9) = MIN(0.01, PSNOW/INLVLS)
      PSNOWDZ(INLVLS) = PSNOW - SUM(PSNOWDZ(1:9))
!  
     ELSEIF(PSNOW > XSNOWCRITD+0.07 .AND. PSNOW<=0.19)THEN
      PSNOWDZ(1) = 0.01 
      PSNOWDZ(2) = 0.01+(PSNOW-0.1)/9
      PSNOWDZ(3) = 0.01+(PSNOW-0.1)/9
      PSNOWDZ(4) = 0.01+(PSNOW-0.1)/9
      PSNOWDZ(5) = 0.01+(PSNOW-0.1)/9 
      PSNOWDZ(6) = 0.01+(PSNOW-0.1)/9
      PSNOWDZ(7) = 0.01+(PSNOW-0.1)/9
      PSNOWDZ(8) = 0.01+(PSNOW-0.1)/9
      PSNOWDZ(9) = 0.01+(PSNOW-0.1)/9
      PSNOWDZ(INLVLS) = PSNOW - SUM(PSNOWDZ(1:9))
!
     ELSEIF(PSNOW > 0.19 .AND. PSNOW<=0.27)THEN
      PSNOWDZ(1) = 0.01 
      PSNOWDZ(2) = 0.02
      PSNOWDZ(3) = 0.02+(PSNOW-0.19)/8
      PSNOWDZ(4) = 0.02+(PSNOW-0.19)/8
      PSNOWDZ(5) = 0.02+(PSNOW-0.19)/8 
      PSNOWDZ(6) = 0.02+(PSNOW-0.19)/8
      PSNOWDZ(7) = 0.02+(PSNOW-0.19)/8
      PSNOWDZ(8) = 0.02+(PSNOW-0.19)/8
      PSNOWDZ(9) = 0.02+(PSNOW-0.19)/8
      PSNOWDZ(INLVLS) = PSNOW - SUM(PSNOWDZ(1:9))
!
      ELSEIF(PSNOW > 0.27 .AND. PSNOW<=0.41)THEN
      PSNOWDZ(1) = 0.01 
      PSNOWDZ(2) = 0.02
      PSNOWDZ(3) = 0.03
      PSNOWDZ(4) = 0.03+(PSNOW-0.27)/7
      PSNOWDZ(5) = 0.03+(PSNOW-0.27)/7 
      PSNOWDZ(6) = 0.03+(PSNOW-0.27)/7
      PSNOWDZ(7) = 0.03+(PSNOW-0.27)/7
      PSNOWDZ(8) = 0.03+(PSNOW-0.27)/7
      PSNOWDZ(9) = 0.03+(PSNOW-0.27)/7
      PSNOWDZ(INLVLS) = PSNOW - SUM(PSNOWDZ(1:9))
!
      ELSEIF(PSNOW > 0.41 .AND. PSNOW<=0.71)THEN
      PSNOWDZ(1) = 0.01 
      PSNOWDZ(2) = 0.02
      PSNOWDZ(3) = 0.03
      PSNOWDZ(4) = 0.05
      PSNOWDZ(5) = 0.05+(PSNOW-0.41)/6 
      PSNOWDZ(6) = 0.05+(PSNOW-0.41)/6
      PSNOWDZ(7) = 0.05+(PSNOW-0.41)/6
      PSNOWDZ(8) = 0.05+(PSNOW-0.41)/6
      PSNOWDZ(9) = 0.05+(PSNOW-0.41)/6
      PSNOWDZ(INLVLS) = PSNOW - SUM(PSNOWDZ(1:9))
!     
      ELSE
      PSNOWDZ(1) = 0.01 
      PSNOWDZ(2) = 0.02
      PSNOWDZ(3) = 0.03
      PSNOWDZ(4) = 0.05
      PSNOWDZ(5) = 0.1
      PSNOWDZ(6) = 0.1+(PSNOW-0.71)/5
      PSNOWDZ(7) = 0.1+(PSNOW-0.71)/5
      PSNOWDZ(8) = 0.1+(PSNOW-0.71)/5
      PSNOWDZ(9) = 0.1+(PSNOW-0.71)/5
      PSNOWDZ(INLVLS) = PSNOW - SUM(PSNOWDZ(1:9))
     ENDIF
       
! ajout EB pour permettre cas INLVLS > 10
ELSE   
      PSNOWDZ(1) = MIN(0.02,PSNOW/INLVLS) 
      PSNOWDZ(2) = MIN(0.01*PSNOW,PSNOW/INLVLS)
      PSNOWDZ(3) = MIN(0.02*PSNOW,PSNOW/INLVLS)
      PSNOWDZ(4) = MIN(0.03*PSNOW,PSNOW/INLVLS)
      PSNOWDZ(5) = MIN(0.05*PSNOW,PSNOW/INLVLS)
      PSNOWDZ(INLVLS)=MIN(0.05*PSNOW,PSNOW/INLVLS) 
      ZWORK = SUM(PSNOWDZ(1:5))          
      DO JJ=6,INLVLS-1,1
         PSNOWDZ(JJ) = (PSNOW - ZWORK -PSNOWDZ(INLVLS)) &
          /(INLVLS-6) 
      END DO
!
ENDIF
!
DO JJ=1,INLVLS
  IF(PSNOW==XUNDEF)THEN
    PSNOWDZ(JJ) = XUNDEF
  ENDIF
END DO
IF (LHOOK) CALL DR_HOOK('MODE_SNOW3L:SNOW3LGRID_1D',1,ZHOOK_HANDLE)
!
END SUBROUTINE SNOW3LGRID_1D
!
!###################################################################################
!###################################################################################
!
!       
        SUBROUTINE SNOW3LAGREG(PSNOWDZN,PSNOWDZ,PSNOWRHO,PSNOWGRAN1, PSNOWGRAN2,&
                                 PSNOWHIST,PSNOWGRAN1N,PSNOWGRAN2N,PSNOWHISTN,   &
                                 KILAYER1,KILAYER2,PSNOWDDZ) 
!
USE MODD_SNOW_METAMO
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
        IMPLICIT NONE
!
!       0.1 declarations of arguments        
!        
        REAL, DIMENSION(:), INTENT(IN)         :: PSNOWDZN,PSNOWDZ,PSNOWRHO,&
                                                     PSNOWDDZ  
!                                                    
        REAL, DIMENSION(:), INTENT(IN)      :: PSNOWGRAN1, PSNOWGRAN2,&
                                                     PSNOWHIST 
                                                    
        REAL, DIMENSION(:), INTENT(OUT)      :: PSNOWGRAN1N, PSNOWGRAN2N,&
                                                     PSNOWHISTN                                                    

        INTEGER, INTENT(IN)                      :: KILAYER1 ! Indice
!                                                couche de référence (i)
        INTEGER, INTENT(IN)                      :: KILAYER2 ! Indice de
!        la couche (i-1 ou i+1) dont une partie est aggrégée à la couche (i)
!
!       0.2 declaration of local variables
!        
        REAL, DIMENSION(SIZE(PSNOWRHO,1)) :: ZSNOWRHO
                                      
!
        INTEGER                         :: IDENT, IVIEU, ILAYER
!
        REAL                            :: ZDELTA
        REAL, DIMENSION(SIZE(PSNOWRHO,1))   :: ZDIAMD,&
                                                 ZDIAMV,ZSPHERD,ZSPHERV,&
                                                 ZDIAMN,ZSPHERN,ZDENT 
        REAL(KIND=JPRB) :: ZHOOK_HANDLE
     
!
IF (LHOOK) CALL DR_HOOK('MODE_SNOW3L:SNOW3LAGREG',0,ZHOOK_HANDLE)
IF(KILAYER1<KILAYER2)THEN
        ZDELTA=0.0
        ILAYER = KILAYER1
ELSE
        ZDELTA=1.0
        ILAYER = KILAYER2
ENDIF        
! Mean Properties
!
!       1. History
!
        IF(PSNOWHIST(KILAYER1)/=PSNOWHIST(KILAYER2)) THEN
        PSNOWHISTN(KILAYER1)=0.0
        ENDIF
!
!       2. New grain types
!
!       2.1 Same grain type
!
!
          
        IF((PSNOWGRAN1(KILAYER1)*PSNOWGRAN1(KILAYER2)>0.0).OR.            &
         (PSNOWGRAN1(KILAYER1)==0.AND.PSNOWGRAN1(KILAYER2)>=0.0).OR.       &
         (PSNOWGRAN1(KILAYER2)==0.AND.PSNOWGRAN1(KILAYER1)>=0.0)) THEN 
!
!code original vincent          PSNOWGRAN1N(KILAYER1)=(PSNOWGRAN1(KILAYER1)*PSNOWRHO(KILAYER1)&
!code original vincent        *(PSNOWDZN(KILAYER1)-(1.0-ZDELTA)*abs(PSNOWDDZ(KILAYER1))-ZDELTA*&
!code original vincent        abs(PSNOWDDZ(KILAYER2)))+PSNOWGRAN1(KILAYER2)*                   &
!code original vincent        PSNOWRHO(KILAYER2)*((1.0-ZDELTA)*abs(PSNOWDDZ(KILAYER1))+        &   
!code original vincent        ZDELTA*abs(PSNOWDDZ(KILAYER2))))/((PSNOWDZN(KILAYER1)-(1.0-ZDELTA)&
!code original vincent        *abs(PSNOWDDZ(KILAYER1))-ZDELTA*abs(PSNOWDDZ(KILAYER2)))*        &
!code original vincent        PSNOWRHO(KILAYER1)+((1.0-ZDELTA)*abs(PSNOWDDZ(KILAYER1))+        &   
!code original vincent        ZDELTA*abs(PSNOWDDZ(KILAYER2)))*PSNOWRHO(KILAYER2))
!code original vincent !
!code original vincent          PSNOWGRAN2N(KILAYER1)=(PSNOWGRAN2(KILAYER1)*PSNOWRHO(KILAYER1) &
!code original vincent        *(PSNOWDZN(KILAYER1)-(1.0-ZDELTA)*abs(PSNOWDDZ(KILAYER1))-ZDELTA* &
!code original vincent        abs(PSNOWDDZ(KILAYER2)))+PSNOWGRAN2(KILAYER2)*                   &
!code original vincent        PSNOWRHO(KILAYER2)*((1.0-ZDELTA)*abs(PSNOWDDZ(KILAYER1))        &
!code original vincent        +ZDELTA*abs(PSNOWDDZ(KILAYER2))))/((PSNOWDZN(KILAYER1)-(1.0-ZDELTA)&
!code original vincent        *abs(PSNOWDDZ(KILAYER1))-ZDELTA*abs(PSNOWDDZ(KILAYER2)))*        &
!code original vincent        PSNOWRHO(KILAYER1)+((1.0-ZDELTA)*abs(PSNOWDDZ(KILAYER1))+        &   
!code original vincent        ZDELTA*abs(PSNOWDDZ(KILAYER2)))*PSNOWRHO(KILAYER2))
!     
!plm
         PSNOWGRAN1N(KILAYER1) = (   PSNOWGRAN1(KILAYER1) * PSNOWRHO(KILAYER1)         &
                                    * (PSNOWDZN(KILAYER1) - abs(PSNOWDDZ(ILAYER)))      &
                                    + PSNOWGRAN1(KILAYER2) * PSNOWRHO(KILAYER2)         &
                                    * abs(PSNOWDDZ(ILAYER))                        )  / &
                                  (   (PSNOWDZN(KILAYER1)-abs(PSNOWDDZ(ILAYER)))          &
                                    * PSNOWRHO(KILAYER1)                                &
                                    + abs(PSNOWDDZ(ILAYER))*PSNOWRHO(KILAYER2)     ) 
!
         PSNOWGRAN2N(KILAYER1) = (   PSNOWGRAN2(KILAYER1) * PSNOWRHO(KILAYER1)         &
                                    * (PSNOWDZN(KILAYER1) - abs(PSNOWDDZ(ILAYER)))      &
                                    + PSNOWGRAN2(KILAYER2) * PSNOWRHO(KILAYER2)         &
                                    * abs(PSNOWDDZ(ILAYER))                        )  / &
                                  (   (PSNOWDZN(KILAYER1)-abs(PSNOWDDZ(ILAYER)))          &
                                    * PSNOWRHO(KILAYER1)                                &
                                    + abs(PSNOWDDZ(ILAYER))*PSNOWRHO(KILAYER2)     ) 
!plm
!     
        ELSE
!
!       2.2 Different types
!        
        IF(PSNOWGRAN1(KILAYER1)<0.0) THEN
          IDENT = KILAYER1
          IVIEU = KILAYER2
        ELSE
          IDENT = KILAYER2
          IVIEU = KILAYER1
        ENDIF  
!                        
        ZDIAMD(KILAYER1)=-PSNOWGRAN1(IDENT)*XDIAET/XGRAN+              &
                         (1.0+PSNOWGRAN1(IDENT)/XGRAN)*(PSNOWGRAN2(IDENT) &
                         *XDIAGF/XGRAN+(1.0-PSNOWGRAN2(IDENT)/XGRAN)&
                         *XDIAFP)                  
        ZSPHERD(KILAYER1)=PSNOWGRAN2(IDENT)/XGRAN                
        ZDIAMV(KILAYER1)=PSNOWGRAN2(IVIEU)
        ZSPHERV(KILAYER1)=PSNOWGRAN1(IVIEU)/XGRAN
        !IF(KILAYER1==1)THEN
        !write(*,*) 'ZDD1',ZDIAMD(1),'ZSD1',ZSPHERD(1)
        !write(*,*) 'ZDV1',ZDIAMV(1),'ZSV1',ZSPHERV(1)
        !ENDIF
!
!        
!
        IF(IDENT==KILAYER1) THEN
!code original vincent        ZDIAMN(KILAYER1)= (ZDIAMD(KILAYER1)*PSNOWRHO(IDENT)*&
!code original vincent            (PSNOWDZN(IDENT)-(1.0-ZDELTA)*abs(PSNOWDDZ(KILAYER1))-ZDELTA*      &
!code original vincent            abs(PSNOWDDZ(KILAYER2)))+ZDIAMV(KILAYER1)*PSNOWRHO(IVIEU)*(       &
!code original vincent            (1.0-ZDELTA)*abs(PSNOWDDZ(KILAYER1))-ZDELTA*abs(PSNOWDDZ(KILAYER2))))/&
!code original vincent            ((PSNOWDZN(KILAYER1)-(1.0-ZDELTA)*                                    &
!code original vincent            abs(PSNOWDDZ(KILAYER1))-ZDELTA*abs(PSNOWDDZ(KILAYER2)))*            &
!code original vincent            PSNOWRHO(KILAYER1)+((1.0-ZDELTA)*abs(PSNOWDDZ(KILAYER1))+          &   
!code original vincent            ZDELTA*abs(PSNOWDDZ(KILAYER2)))*PSNOWRHO(KILAYER2))
!
!plm
        ZDIAMN(KILAYER1)= (ZDIAMD(KILAYER1)*PSNOWRHO(IDENT)*&
             (PSNOWDZN(IDENT)-abs(PSNOWDDZ(ILAYER)))+        &
             ZDIAMV(KILAYER1)*PSNOWRHO(IVIEU)*abs(PSNOWDDZ(ILAYER)))/&
             ((PSNOWDZN(KILAYER1)-abs(PSNOWDDZ(ILAYER)))*            &
             PSNOWRHO(KILAYER1)+abs(PSNOWDDZ(ILAYER))*PSNOWRHO(KILAYER2)) 
!plm

!
!         
!code original vincent        ZSPHERN(KILAYER1)= (ZSPHERD(KILAYER1)*PSNOWRHO(IDENT)*&
!code original vincent            (PSNOWDZN(IDENT)-(1.0-ZDELTA)*abs(PSNOWDDZ(KILAYER1))-ZDELTA*      &
!code original vincent            abs(PSNOWDDZ(KILAYER2)))+ZSPHERV(KILAYER1)*PSNOWRHO(IVIEU)*(       &
!code original vincent            (1.0-ZDELTA)*abs(PSNOWDDZ(KILAYER1))-ZDELTA*abs(PSNOWDDZ(KILAYER2))))/&
!code original vincent            ((PSNOWDZN(KILAYER1)-(1.0-ZDELTA)*                                    &
!code original vincent            abs(PSNOWDDZ(KILAYER1))-ZDELTA*abs(PSNOWDDZ(KILAYER2)))*            &
!code original vincent            PSNOWRHO(KILAYER1)+((1.0-ZDELTA)*abs(PSNOWDDZ(KILAYER1))+          &   
!code original vincent            ZDELTA*abs(PSNOWDDZ(KILAYER2)))*PSNOWRHO(KILAYER2))

!plm
        ZSPHERN(KILAYER1)= (ZSPHERD(KILAYER1)*PSNOWRHO(IDENT)*&
             (PSNOWDZN(IDENT)-abs(PSNOWDDZ(ILAYER)))+          &
             ZSPHERV(KILAYER1)*PSNOWRHO(IVIEU)* abs(PSNOWDDZ(ILAYER)))/&
             ((PSNOWDZN(KILAYER1)-abs(PSNOWDDZ(ILAYER)))*            &
             PSNOWRHO(KILAYER1)+abs(PSNOWDDZ(ILAYER))*PSNOWRHO(KILAYER2)) 
!plm
!
!
!
        ELSE
!code original vincent        ZDIAMN(KILAYER1)= (ZDIAMD(KILAYER1)*PSNOWRHO(IDENT)*&
!code original vincent            ((1.0-ZDELTA)*abs(PSNOWDDZ(KILAYER1))+ZDELTA*abs(PSNOWDDZ(KILAYER2)))&
!code original vincent            +ZDIAMV(KILAYER1)*PSNOWRHO(IVIEU)*(PSNOWDZN(IVIEU)-(1.0-ZDELTA)*  & 
!code original vincent            abs(PSNOWDDZ(KILAYER1))-ZDELTA*abs(PSNOWDDZ(KILAYER2))))/&
!code original vincent            ((PSNOWDZN(KILAYER1)-(1.0-ZDELTA)*                          &
!code original vincent            abs(PSNOWDDZ(KILAYER1))-ZDELTA*abs(PSNOWDDZ(KILAYER2)))*    &
!code original vincent            PSNOWRHO(KILAYER1)+((1.0-ZDELTA)*abs(PSNOWDDZ(KILAYER1))+   &
!code original vincent            ZDELTA*abs(PSNOWDDZ(KILAYER2)))*PSNOWRHO(KILAYER2))
!code original vincent!            
!code original vincent         ZSPHERN(KILAYER1)= (ZSPHERD(KILAYER1)*PSNOWRHO(IDENT)*&
!code original vincent            ((1.0-ZDELTA)*abs(PSNOWDDZ(KILAYER1))+ZDELTA*abs(PSNOWDDZ(KILAYER2)))&
!code original vincent            +ZSPHERV(KILAYER1)*PSNOWRHO(IVIEU)*(PSNOWDZN(IVIEU)-(1.0-ZDELTA)* & 
!code original vincent            abs(PSNOWDDZ(KILAYER1))-ZDELTA*abs(PSNOWDDZ(KILAYER2))))/&
!code original vincent            ((PSNOWDZN(KILAYER1)-(1.0-ZDELTA)*                                    &
!code original vincent            abs(PSNOWDDZ(KILAYER1))-ZDELTA*abs(PSNOWDDZ(KILAYER2)))*            &
!code original vincent            PSNOWRHO(KILAYER1)+((1.0-ZDELTA)*abs(PSNOWDDZ(KILAYER1))+          &   
!code original vincent            ZDELTA*abs(PSNOWDDZ(KILAYER2)))*PSNOWRHO(KILAYER2))

!plm
        ZDIAMN(KILAYER1)= (ZDIAMD(KILAYER1)*PSNOWRHO(IDENT)*&
             abs(PSNOWDDZ(ILAYER))&
             +ZDIAMV(KILAYER1)*PSNOWRHO(IVIEU)*(PSNOWDZN(IVIEU)-abs(PSNOWDDZ(ILAYER))))/&
             ((PSNOWDZN(KILAYER1)-abs(PSNOWDDZ(ILAYER)))*    &
             PSNOWRHO(KILAYER1)+abs(PSNOWDDZ(ILAYER))*PSNOWRHO(KILAYER2)) 
!            
         ZSPHERN(KILAYER1)= (ZSPHERD(KILAYER1)*PSNOWRHO(IDENT)*&
             abs(PSNOWDDZ(ILAYER))&
             +ZSPHERV(KILAYER1)*PSNOWRHO(IVIEU)*(PSNOWDZN(IVIEU)-abs(PSNOWDDZ(ILAYER))))/&
             ((PSNOWDZN(KILAYER1)-abs(PSNOWDDZ(ILAYER)))*            &
             PSNOWRHO(KILAYER1)+abs(PSNOWDDZ(ILAYER))*PSNOWRHO(KILAYER2)) 
!plm
        ENDIF
!
!
!        
        IF(ZDIAMN(KILAYER1)<ZSPHERN(KILAYER1)*XDIAGF+&
                    (1-ZSPHERN(KILAYER1))*XDIAFP) THEN 
         ZDENT(KILAYER1)=(ZDIAMN(KILAYER1)-(ZSPHERN(KILAYER1)*&
             XDIAGF+(1-ZSPHERN(KILAYER1))*XDIAFP))/(XDIAET-&
             (ZSPHERN(KILAYER1)*XDIAGF+(1-ZSPHERN(KILAYER1))*XDIAFP)) 
         !IF(KILAYER1==1) write(*,*) 'ZDENT',ZDENT(1)   
         PSNOWGRAN1N(KILAYER1)=-XGRAN*ZDENT(KILAYER1)
         PSNOWGRAN2N(KILAYER1)= XGRAN*ZSPHERN(KILAYER1)
        ELSE
        PSNOWGRAN1N(KILAYER1)= XGRAN*ZSPHERN(KILAYER1)
        PSNOWGRAN2N(KILAYER1)= ZDIAMN(KILAYER1)
        ENDIF                       
        ENDIF
       IF (LHOOK) CALL DR_HOOK('MODE_SNOW3L:SNOW3LAGREG',1,ZHOOK_HANDLE)
!
!       3. Update snow grains parameters : GRAN1, GRAN2        
!        PSNOWGRAN1(KILAYER1)=ZSNOWGRAN1(KILAYER1)
!        PSNOWGRAN2(KILAYER1)=ZSNOWGRAN2(KILAYER1)
              
       
       END SUBROUTINE SNOW3LAGREG    
!
!###############################################################################                 
!###############################################################################
!
!       
!ajout EB : ajout des arguments "N" pour faire idem variables d'origine

        SUBROUTINE SNOW3LAVGRAIN(PSNOWGRAN1, PSNOWGRAN2, PSNOWHIST,   &
                       ZSNOWGRAN1N, ZSNOWGRAN2N, ZSNOWHISTN,PNDENT, PNVIEU) 
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
!
        IMPLICIT NONE
!
!       0.1 declarations of arguments        
!        
        REAL, DIMENSION(:,:), INTENT(INOUT)  :: PSNOWGRAN1,PSNOWGRAN2,  &
                                                 PSNOWHIST 
!
! ajout EB
        REAL, DIMENSION(:,:), INTENT(INOUT)  :: ZSNOWGRAN1N,ZSNOWGRAN2N,  &
                                                 ZSNOWHISTN 

        REAL, DIMENSION(:), INTENT(IN)    :: PNDENT, PNVIEU          
!
!
!       0.2 declaration of local variables
!
         INTEGER                              :: JI, JJ
         INTEGER                              :: INLVLS, INI
!
         REAL, DIMENSION(SIZE(PSNOWGRAN1,1)) ::KGRAN1, KGRAN2, KHIST    
         REAL(KIND=JPRB) :: ZHOOK_HANDLE
!         
!       0.3 initialization         
!
IF (LHOOK) CALL DR_HOOK('MODE_SNOW3L:SNOW3LAVGRAIN',0,ZHOOK_HANDLE)
INLVLS    = SIZE(PSNOWGRAN1,2) 
INI       = SIZE(PSNOWGRAN1,1) 
KGRAN1(:) = 0.0
KGRAN2(:) = 0.0
KHIST(:)  = 0.0
!
!      
!         
DO JJ=1,INLVLS
   DO JI=1,INI
      IF(PNDENT(JI)==0.0 .AND. PNVIEU(JI)==0.0)THEN
         KGRAN1(JI)    = 1.0
         KGRAN2(JI)    = 1.0 
         KHIST(JI)     = 1.0
      ELSEIF(PNDENT(JI)>=PNVIEU(JI))THEN      ! more dendritic than non dendritic snow layer
         IF(PSNOWGRAN1(JI,JJ)<0.0)THEN
            KGRAN1(JI) = KGRAN1(JI)+PSNOWGRAN1(JI,JJ)
            KGRAN2(JI) = KGRAN2(JI)+PSNOWGRAN2(JI,JJ)
         ENDIF
      ELSE                              ! more non dendritic than dendritic snow layers  
         IF(PSNOWGRAN1(JI,JJ)>=0)THEN
            KGRAN1(JI) = KGRAN1(JI)+PSNOWGRAN1(JI,JJ)
            KGRAN2(JI) = KGRAN2(JI)+PSNOWGRAN2(JI,JJ)
            KHIST(JI)  = KHIST(JI)+PSNOWHIST(JI,JJ) 
         ENDIF
      ENDIF
   ENDDO
ENDDO
!
DO JJ=1,INLVLS
   DO JI=1,INI
      IF(PNDENT(JI)==0.0 .AND. PNVIEU(JI)==0.0)THEN
         KGRAN1(JI)        =1.0
      ELSEIF(PNDENT(JI)>=PNVIEU(JI))THEN
         ZSNOWGRAN1N(JI,JJ)= KGRAN1(JI)/PNDENT(JI)   
         ZSNOWGRAN2N(JI,JJ)= KGRAN2(JI)/PNDENT(JI)
         ZSNOWHISTN(JI,JJ) = 0.0
      ELSE
         ZSNOWGRAN1N(JI,JJ)= KGRAN1(JI)/PNVIEU(JI)
         ZSNOWGRAN2N(JI,JJ)= KGRAN2(JI)/PNVIEU(JI)
         ZSNOWHISTN(JI,JJ) = KHIST(JI) /PNVIEU(JI)
      ENDIF
   ENDDO
ENDDO
!
IF (LHOOK) CALL DR_HOOK('MODE_SNOW3L:SNOW3LAVGRAIN',1,ZHOOK_HANDLE)
END SUBROUTINE SNOW3LAVGRAIN         
!        
!####################################################################
!####################################################################
!####################################################################
FUNCTION SNOW3LDIFTYP(PGRAIN1,PGRAIN2,PGRAIN3,PGRAIN4) RESULT(ZDIFTYPE)
!
! à remplacer sans doute par une routine equivalente du nouveau crocus
!*    CALCUL DE LA DIFFERENCE ENTRE DEUX TYPES DE GRAINS
!     VALEUR ENTRE 200 ET 0
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!*      0.1    declarations of arguments
REAL  :: PGRAIN1, PGRAIN2, PGRAIN3, PGRAIN4, ZDIFTYPE
REAL(KIND=JPRB) :: ZHOOK_HANDLE

!*      0.2    calcul de la difference entre type de grains
IF (LHOOK) CALL DR_HOOK('MODE_SNOW3L:SNOW3LDIFTYP',0,ZHOOK_HANDLE)
if (pgrain1==0.and.pgrain2>=0) then
ZDIFTYPE=ABS(PGRAIN1-PGRAIN2)+ABS(PGRAIN3-PGRAIN4)*5.*10000.
elseIF(PGRAIN1>=0.and.PGRAIN2==0) then
ZDIFTYPE=ABS(PGRAIN1-PGRAIN2)+ABS(PGRAIN3-PGRAIN4)*5.*10000.
else if(PGRAIN1<0.and.PGRAIN2==0) then
       zdiftype=200.
else if(PGRAIN1==0.and.PGRAIN2<0) then
       zdiftype=200.
ELSEIF(PGRAIN1*PGRAIN2.LT.0.) THEN
ZDIFTYPE=200.
ELSEIF(PGRAIN1.LT.0.) THEN
ZDIFTYPE=ABS(PGRAIN1-PGRAIN2)*.5+ABS(PGRAIN3-PGRAIN4)*.5
ELSE
ZDIFTYPE=ABS(PGRAIN1-PGRAIN2)+ABS(PGRAIN3-PGRAIN4)*5.*10000.
ENDIF
IF (LHOOK) CALL DR_HOOK('MODE_SNOW3L:SNOW3LDIFTYP',1,ZHOOK_HANDLE)
END FUNCTION SNOW3LDIFTYP
!####################################################################
!####################################################################
!####################################################################

END MODULE MODE_SNOW3L

