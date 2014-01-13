!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
      SUBROUTINE TSZ0( PTIME, PTSTEP, PWFC, PTG, PWG )
!     ################################################################
!
!
!!****  *TSZ0*  
!!
!!    PURPOSE
!!    -------
!       This subroutine computes the surface fluxes when  the soil temperature,
!     humidity and rugisty length are prescribed. It uses these values and the 
!     atmospheric fields at the first level located at dz/2 to compute a
!     vertical gradient and a drag coefficient is computed according to a
!     stability index ( Richardson number )    
!     
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
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
!!	J. Stein           * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    25/01/96 
!!                  25/03/96   spatialize the input TS, WG, SST fields
!!                  22/05/96   correct igrid value for the rain rate
!!                  27/11/96   set realistic values for z0 fields on sea
!!      (V.Masson)  09/07/97  add directional z0 computations and RESA correction
!!      (V.Masson)  15/03/99  some computations are now done in GROUND_PARAMn
!!      (V.Masson)  04/01/00  all computations are now done in ISBA
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_CSTS,       ONLY : XPI
USE MODD_SURF_PAR,   ONLY : XUNDEF
USE MODD_DATA_TSZ0_n
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
!* general variables
!  -----------------
REAL,                   INTENT(IN)  :: PTIME      ! Current time
REAL,                   INTENT(IN)  :: PTSTEP     ! timestep of the integration
!
!* soil variables
!  --------------
!
REAL, DIMENSION(:,:),   INTENT(IN)  :: PWFC       ! field capacity
!
!* prognostic variables
!  --------------------
!
REAL, DIMENSION(:,:,:), INTENT(INOUT) :: PTG      ! surface temperature
REAL, DIMENSION(:,:,:), INTENT(INOUT) :: PWG      ! near-surface volumetric water
!
!
!*      0.2    declarations of local variables
!
!
!* local variables for Ts time interpolation
!   ----------------------------------------
!
REAL    :: ZA,ZTIMEP    ! weigths and instant for the temporal interpolation
INTEGER :: IHOURP       ! hourly data index for intant t +dt
INTEGER :: JPATCH       ! loop counter on patches
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! enter here the temporal variations of the soil fields increments
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
! prescribed values of the surface temperature increment over land (K)
REAL :: ZDTS_HOUR 
! prescribed values of the soil humidity increment at every hour (fraction)
REAL :: ZDHUGRD_HOUR              
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!*       1.     TEMPORAL INTERPOLATION OF THE SURFACE TEMPERATURES AT T+DT
!               ----------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('TSZ0',0,ZHOOK_HANDLE)
!
ZTIMEP = MOD(PTIME+PTSTEP,86400.)  ! recover the time from O HTU
!
IHOURP = INT(ZTIMEP/3600.)+1
!
IF (NTIME==25) THEN
  ZDTS_HOUR    = XDATA_DTS   (IHOURP)
  ZDHUGRD_HOUR = XDATA_DHUGRD(IHOURP)
ELSEIF (NTIME==1) THEN
  ZDTS_HOUR    = XDATA_DTS   (1)
  ZDHUGRD_HOUR = XDATA_DHUGRD(1)
ENDIF
!
! temporal interpolation of the surface temperature increment  over land at time t
ZA= ZDTS_HOUR /3600. * PTSTEP
WHERE (PTG(:,:,:)/=XUNDEF)
  PTG(:,:,:)= PTG(:,:,:) + ZA
END WHERE
!
! temporal interpolation of the soil humidity increment at time t
ZA= ZDHUGRD_HOUR /3600.* PTSTEP
DO JPATCH=1,SIZE(PWG,3)
  WHERE (PWG(:,:,JPATCH)/=XUNDEF)
  PWG(:,:,JPATCH)= ACOS( 1.                                                               &
      - 2.* MIN( 0.5 * (1. - COS( XPI * MIN(PWG(:,:,JPATCH) /PWFC(:,:),1.)  )) + ZA , 1.) &
            ) / XPI * PWFC(:,:)  
  END WHERE
END DO
!
IF (LHOOK) CALL DR_HOOK('TSZ0',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE TSZ0
