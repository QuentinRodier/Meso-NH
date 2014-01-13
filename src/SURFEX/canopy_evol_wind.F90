!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
      SUBROUTINE CANOPY_EVOL_WIND(KI,KLVL,PTSTEP,KIMPL,PWIND,PK,PDKDDVDZ,        &
                                   PSFLUX_U,PFORC_U,PDFORC_UDU,PDZ,PDZF,PU,PUW,   &
                                   PALFA,PBETA                                    ) 
!     #########################################
!
!!****  *CANOPY_EVOL_WIND* - evolution of wind in canopy 
!!                        
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
!!	V. Masson   *Meteo France*	
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    07/2006 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODI_CANOPY_EVOL_FIELD
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
INTEGER,                  INTENT(IN)    :: KI        ! number of horizontal points
INTEGER,                  INTENT(IN)    :: KLVL      ! number of levels in canopy
REAL,                     INTENT(IN)    :: PTSTEP    ! time-step                             (s)
INTEGER,                  INTENT(IN)    :: KIMPL     ! implicitation code: 
!                                                    ! 1 : computes only alfa and beta coupling
!                                                    !     coefficients for all variables
!                                                    ! 2 : computes temporal evolution of the
!                                                    !     variables
REAL, DIMENSION(KI),      INTENT(IN)    :: PWIND     ! wind at forcing level                 (m/s)
REAL, DIMENSION(KI,KLVL), INTENT(IN)    :: PK        ! mixing exchange coefficient           (m2/s)
REAL, DIMENSION(KI,KLVL), INTENT(IN)    :: PDKDDVDZ  ! derivative of mixing coefficient as a
!                                                    ! function of vertical gradient of wind
!                                                    ! (at mid levels)                       (m2)
REAL, DIMENSION(KI),      INTENT(IN)    :: PSFLUX_U  ! Surface flux u'w'                     (m2/s2)
REAL, DIMENSION(KI,KLVL), INTENT(IN)    :: PFORC_U   ! tendency of wind due to canopy drag   (m/s2)
REAL, DIMENSION(KI,KLVL), INTENT(IN)    :: PDFORC_UDU! formal derivative of the tendency of
!                                                    ! wind due to canopy drag              (1/s)
REAL, DIMENSION(KI,KLVL), INTENT(IN)    :: PDZ       ! deltaZ between canopy half levels,
!                                                    ! located at full levels                (m)
REAL, DIMENSION(KI,KLVL), INTENT(IN)    :: PDZF      ! deltaZ between canopy (full) levels,
!                                                    ! located at half levels                (m)
REAL, DIMENSION(KI,KLVL), INTENT(INOUT) :: PU        ! wind speed at canopy levels           (m/s)
REAL, DIMENSION(KI,KLVL), INTENT(OUT)   :: PUW       ! turbulent flux (at half levels)       (m2/s2)
REAL, DIMENSION(KI),      INTENT(OUT)   :: PALFA     !  V+(1) = alfa u'w' + beta
REAL, DIMENSION(KI),      INTENT(OUT)   :: PBETA     !  V+(1) = alfa u'w' + beta
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER                     :: JLAYER   ! loop counter on layers
!
REAL, DIMENSION(KI,KLVL)   :: ZDUDZ    ! dU/dz at mid levels
REAL, DIMENSION(KI,KLVL)   :: ZF       ! turbulent flux at mid levels
REAL, DIMENSION(KI,KLVL)   :: ZDFDDVDZ ! derivative of turbulent flux as a
!                                       ! function of vertical gradient of wind variable
!                                       ! (at mid levels)
REAL, DIMENSION(KI,KLVL)   :: ZEXT     ! external forcing at full levels
REAL, DIMENSION(KI,KLVL)   :: ZDEXTDV  ! derivative of external forcing as a
!                                       ! function of vertical variable
!                                       ! (at full levels)
REAL, DIMENSION(KI,KLVL)   :: ZU       ! work variable : wind at futur instant 
!                                       ! (or past at the end of the routine) 
REAL, DIMENSION(KI)         :: ZDUADT   ! dUa/dt   at forcing level
LOGICAL                     :: LIMPL
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!
!*    1. initializations
!        ---------------
!
!* external forces
!
IF (LHOOK) CALL DR_HOOK('CANOPY_EVOL_WIND',0,ZHOOK_HANDLE)
ZEXT = 0.
ZDEXTDV = 0.
!
!* coupling coefficient with the surface
!
PALFA(:)=0.
PBETA(:)=PU(:,1)
!
!-------------------------------------------------------------------------------
!
!*    5. Forcing due to drag (at full levels)
!        -------------------
!
!* drag force by canopy
!
ZEXT = ZEXT + PFORC_U(:,:)
!
ZDEXTDV = ZDEXTDV + PDFORC_UDU(:,:)
!
!-------------------------------------------------------------------------------
!
!*    6. External forcing due to large-scale forces (at full levels)
!        ------------------------------------------
!
!
!* forces due to large-scale forcing
!
! These are computed from wind evolution equation at forcing level :
!
! dUa/dt = Large_Scale_Forcing - d(u'w')/dz|z=forcing_level
!
! because vertical derivative of turbulent flux is not available at forcing
! level, one must make the assumption that the turbulent flux is uniform
! between the forcing level and the level just below.
! This means that one assume that the forcing layer is in the inertail sublayer
! (where turbulent fluxes are constant).
!
ZDUADT(:) = ( PWIND(:) - PU(:,KLVL) ) /PTSTEP
!
!* for smoother evolution at large time steps, does not include explicitely
!  forcing term in lower layers
DO JLAYER=1,KLVL-1
  ZEXT(:,JLAYER) = ZEXT(:,JLAYER)
END DO

!* evolution of forcing layer forced by forcing...
ZEXT(:,KLVL) = ZDUADT(:)
!
 CALL  CANOPY_EVOL_FIELD(KI, KLVL, PTSTEP, KIMPL, PK, PDKDDVDZ, &
                        PSFLUX_U, PFORC_U, PDFORC_UDU, PDZ, PDZF,     &
                        ZEXT, ZDEXTDV, PU, PUW, PALFA, PBETA          ) 
!
!-------------------------------------------------------------------------------
!
!*   10. Security at all levels : positivity of wind speed
!        ----------------------
!
PU(:,:) = MAX(PU,0.)
IF (LHOOK) CALL DR_HOOK('CANOPY_EVOL_WIND',1,ZHOOK_HANDLE)
!
!----------------------------------------------------------------
!
END SUBROUTINE CANOPY_EVOL_WIND
