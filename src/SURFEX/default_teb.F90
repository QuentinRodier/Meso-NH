!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE DEFAULT_TEB(HZ0H,HZ0EFF_GD,PTSTEP,POUT_TSTEP, HCH_BEM, HURB_LM, HSOLAR_PANEL)
!     ########################################################################
!
!!****  *DEFAULT_TEB* - routine to set default values for the configuration for TEB scheme
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
!!      V. Masson   *Meteo France*
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2004 
!!      modified    08/2012 G. Pigeon, add HCH_BEM for building conv. coef. 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_SURF_PAR,   ONLY : XUNDEF
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
!
CHARACTER(LEN=6),  INTENT(OUT) :: HZ0H       ! TEB option for z0h roof & road
CHARACTER(LEN=4),  INTENT(OUT) :: HZ0EFF_GD  ! TEB option for effective roughness length for low urban vegetation
CHARACTER(LEN=5),  INTENT(OUT) :: HCH_BEM    ! TEB option building conv. coef.
CHARACTER(LEN=4),  INTENT(OUT) :: HURB_LM    ! TEB option for urban mixing length
CHARACTER(LEN=3),  INTENT(OUT) :: HSOLAR_PANEL ! TEB solar panel option
REAL,              INTENT(OUT) :: PTSTEP     ! time step for run
REAL,              INTENT(OUT) :: POUT_TSTEP ! time step for writing
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('DEFAULT_TEB',0,ZHOOK_HANDLE)
!
HZ0H       = 'KAND07'
HZ0EFF_GD  = 'LR21'     ! Lemonsu Redon et al 2021
PTSTEP     = XUNDEF
POUT_TSTEP = XUNDEF
HCH_BEM    = 'ROW30'
HURB_LM    = 'LMEZ'
HSOLAR_PANEL='PV '
!
IF (LHOOK) CALL DR_HOOK('DEFAULT_TEB',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE DEFAULT_TEB
