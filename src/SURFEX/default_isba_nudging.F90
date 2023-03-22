!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     ######### 
      SUBROUTINE DEFAULT_ISBA_NUDGING(ONUDG_SWE,ONUDG_SWE_MASK,  &
                                      PTRELAX_SWE,HNUDG_WG,      &
                                      ONUDG_WG_MASK,PTRELAX_WG,  &
                                      PNUDG_Z_WG                 )
!                     
!     ########################################################################
!
!!****  *DEFAULT_ISBA_NUDGING* - routine to set default values for the configuration
!!                               for the nudging of ISBA (soil moisture and snow)
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!      J. Colin   *Meteo France*
!!
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    08/2016 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_SURF_PAR,   ONLY : XUNDEF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
! Nudging
LOGICAL,            INTENT(OUT) :: ONUDG_SWE     ! Switch to nudge the snow mass
LOGICAL,            INTENT(OUT) :: ONUDG_SWE_MASK
CHARACTER(LEN=3),   INTENT(OUT) :: HNUDG_WG      ! Switch to nudge the (total) soil water
LOGICAL,            INTENT(OUT) :: ONUDG_WG_MASK      
!
REAL,               INTENT(OUT) :: PTRELAX_SWE   ! Time relaxation for the nudging of the snow
REAL,               INTENT(OUT) :: PTRELAX_WG    !  Time relaxation for the nudging of the soil water
!
REAL, DIMENSION(:), INTENT(OUT) :: PNUDG_Z_WG
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('DEFAULT_ISBA_NUDGING',0,ZHOOK_HANDLE)
! 
ONUDG_SWE      = .FALSE.
ONUDG_SWE_MASK = .FALSE.
HNUDG_WG       = 'DEF'
ONUDG_WG_MASK  = .FALSE.
!
PTRELAX_SWE    = 86400.
PTRELAX_WG     = 86400.
!
PNUDG_Z_WG(:)  = 1.0
!
IF (LHOOK) CALL DR_HOOK('DEFAULT_ISBA_NUDGING',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE DEFAULT_ISBA_NUDGING
