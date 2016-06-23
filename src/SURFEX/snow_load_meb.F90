!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!   ############################################################################
SUBROUTINE SNOW_LOAD_MEB(PTSTEP,PSR,PTV,PWRVNMAX,PKVN,PCHEATV,PLERCV,PLESC,PMELTVN, &
          PVELC,PMELTCV,PFRZCV,PUNLOADSNOW,PWRV,PWRVN,PSUBVCOR,PLVTT,PLSTT)
!   ############################################################################
!
!!****  *SNOW_LOAD_MEB*
!!
!!    PURPOSE
!!    -------
!
!     Calculate temporal evolution of canopy-intercepted intercepted snow
!     
!!**  METHOD
!!    ------
!
!
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!
!!      
!!    REFERENCE
!!    ---------
!!
!!      
!!    AUTHOR
!!    ------
!!
!!      P. Samuelsson           * SMHI *
!!      A. Boone                * CNRM-GAME, Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    02/2011
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_CSTS,     ONLY : XTT, XLMTT
!
USE MODD_SNOW_PAR, ONLY : XRHOSMAX_ES
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    Declaration of Arguments
!
REAL,               INTENT(IN)    :: PTSTEP
!
REAL, DIMENSION(:), INTENT(IN)    :: PLVTT, PLSTT
REAL, DIMENSION(:), INTENT(IN)    :: PSR,PCHEATV, PLERCV, PVELC,              &
                                     PLESC, PMELTVN, PWRVNMAX, PKVN
!
REAL, DIMENSION(:), INTENT(INOUT) :: PWRVN, PWRV, PTV
!
REAL, DIMENSION(:), INTENT(OUT)   :: PMELTCV, PFRZCV, PUNLOADSNOW, PSUBVCOR
!
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(SIZE(PSR))        :: ZSRINT, ZUNLOAD, ZWRVN, ZSUB
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*      0.3    declarations of local parameters
!
! Snow unloading parameters (Roesch el al., Clim. Dyn., 2001)
!
REAL, PARAMETER                   :: ZUNLOAD_T     = 1.5E+5   ! K s
REAL, PARAMETER                   :: ZUNLOAD_TT    = 270.15   ! K
REAL, PARAMETER                   :: ZUNLOAD_V     = 1.87E+5  ! m 
!
!-------------------------------------------------
! 0) Initialization
!
IF (LHOOK) CALL DR_HOOK('SNOW_LOAD_MEB',0,ZHOOK_HANDLE)
!
ZSRINT(:)      = 0.0
ZWRVN(:)       = 0.0
ZSUB(:)        = 0.0
ZUNLOAD(:)     = 0.0
!
!
! 1) First consider the case when maximum interception is zero...
! this only occurs when vegetation canopy is *totally* buried. The follwing line
! results in non-zero snow loading (total removal of intercepted snow) 
! only during the timestep when vegetation has just been buried:
!
!
WHERE(PWRVNMAX(:) == 0.0)
!
   PUNLOADSNOW(:) = PWRVN(:)/PTSTEP    ! kg m-2 s-1
   PWRVN(:)       = 0.0

! for a totally buried canopy, the following are zero:

   PMELTCV(:)     = 0.0
   PFRZCV(:)      = 0.0
   PSUBVCOR(:)    = 0.0
!
!
ELSEWHERE
!
!
! 2) Case for snow beneath or only partially covering the vegetation canopy:
!
!
! The following are computed as steps to ensure mass conservation.
!
! Interception: gain

   ZSRINT(:)      = MAX(0.0,PWRVNMAX(:)-PWRVN(:))*(1.0-EXP(-PKVN(:)*PSR(:)*PTSTEP)) ! kg m-2
   ZSRINT(:)      = MIN(PSR(:)*PTSTEP, ZSRINT(:))  ! kg m-2 
   ZWRVN(:)       = PWRVN(:) + ZSRINT(:)           ! kg m-2 

   PUNLOADSNOW(:) = MAX(0.0, PSR(:) - ZSRINT(:)/PTSTEP) ! kg m-2 s-1

! Sublimation: gain or loss
! NOTE for the rare case that sublimation exceeds snow mass (possible as traces of snow disappear)
! compute a mass correction to be removed from soil (to conserve mass): PSUBVCOR

   ZSUB(:)        = PLESC(:)*(PTSTEP/PLSTT(:))           ! kg m-2
   PSUBVCOR(:)    = MAX(0.0, ZSUB(:) - ZWRVN(:))/PTSTEP  ! kg m-2 s-1
   ZWRVN(:)       = MAX(0.0, ZWRVN(:) - ZSUB(:))         ! kg m-2

! Phase change: loss (melt of snow mass)

   PMELTCV(:)     = PTSTEP*MAX(0.0, PMELTVN(:))         ! kg m-2  
   PMELTCV(:)     = MIN(PMELTCV(:), ZWRVN(:))
   ZWRVN(:)       = ZWRVN(:) - PMELTCV(:)
   PWRV(:)        = PWRV(:)  + PMELTCV(:)               ! NOTE...liq reservoir can exceed maximum holding
                                                        !        capacity here, but this is accounted for
                                                        !        in main prognostic PWRV routine.

! Phase change: gain (freeze of intercepted water) 
! Note, to get a better estimate of water available for freezing, remove Er in 
! estimation of water for freezing:
! Also, update liquid water stored on the canopy here:

   PFRZCV(:)      = PTSTEP*MAX(0.0, -PMELTVN(:))        ! kg m-2  
   PFRZCV(:)      = MIN(PFRZCV(:), MAX(0.0,PWRV(:)-PLERCV(:)*(PTSTEP/PLVTT(:))))
   ZWRVN(:)       = ZWRVN(:) + PFRZCV(:)
   PWRV(:)        = PWRV(:)  - PFRZCV(:)

! Unloading (falling off branches, etc...): loss
! Note, the temperature effect is assumed to vanish for cold temperatures.

   ZUNLOAD(:)     = MIN(ZWRVN(:), PWRVN(:)*( PVELC(:)*(PTSTEP/ZUNLOAD_V)          &
                     + MAX(0.0, PTV(:)-ZUNLOAD_TT)*(PTSTEP/ZUNLOAD_T) ))            ! kg m-2 
   ZWRVN(:)       = ZWRVN(:) - ZUNLOAD(:)                                           ! kg m-2 
   PUNLOADSNOW(:) = PUNLOADSNOW(:) + ZUNLOAD(:)/PTSTEP

! Diagnostic updates:
! final phase change (units)

   PMELTCV(:)     = PMELTCV(:)/PTSTEP ! kg m-2 s-1
   PFRZCV(:)      = PFRZCV(:) /PTSTEP ! kg m-2 s-1

! Prognostic Updates:

   PWRVN(:)       = ZWRVN(:)

   PTV(:)         = PTV(:) + (PFRZCV(:) - PMELTCV(:))*(XLMTT*PTSTEP)/PCHEATV(:) ! K

END WHERE
!
IF (LHOOK) CALL DR_HOOK('SNOW_LOAD_MEB',1,ZHOOK_HANDLE)
!
END SUBROUTINE SNOW_LOAD_MEB
