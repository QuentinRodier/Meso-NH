!     #########
    SUBROUTINE URBAN_RUNOFF(PWS_MAX, PWS, PRR, PIRRIG, PTSTEP, PCONNEX,   &
                            PRUNOFF, PNOC                                 )
!   ##########################################################################
!
!!****  *URBAN_RUNOFF*  
!!
!!    PURPOSE
!!    -------
!
!     Computes the evolution of prognostic water reservoirs
!     of urbanized areas.
!         
!     
!!**  METHOD
!     ------
!
!
!   The roof reservoir runoff goes directly into the road reservoir.
!
!   Runoff occurs for road reservoir (too much water), as well as drainage
!   (evacuation system, typical time scale: 1 day)
!
!
!
!
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    MODD_CST
!!
!!      
!!    REFERENCE
!!    ---------
!!
!!      
!!    AUTHOR
!!    ------
!!
!!      V. Masson           * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    23/01/98 
!!      Modifs      01/16 (K.Chancibault/A.Lemonsu)  Generic urban_hydro.F90 routine for roads/roofs
!!                                                   PRUNOFF_TOWN is now computed afterwards
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
REAL, DIMENSION(:), INTENT(IN)    :: PWS_MAX         ! maximum deepness of water reservoir !
REAL, DIMENSION(:), INTENT(INOUT) :: PWS             ! water reservoir
REAL, DIMENSION(:), INTENT(IN)    :: PRR             ! rain rate
REAL, DIMENSION(:), INTENT(IN)    :: PIRRIG          ! watering rate
REAL,               INTENT(IN)    :: PTSTEP          ! time step
REAL, DIMENSION(:), INTENT(IN)    :: PCONNEX         ! Impervious surfaces connexion rate to the sewer (-)
REAL, DIMENSION(:), INTENT(OUT)   :: PRUNOFF         ! runoff (kg/m2/s)
REAL, DIMENSION(:), INTENT(OUT)   :: PNOC            ! runoff from surfaces not connected to sewer (kg/m2/s)
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*      0.2    declarations of local variables
!
!-------------------------------------------------------------------------------
!
!*      1.     Water reservoir evolution
!              -------------------------
!
!
!                                           evolution of the water reservoir
!                                           (if we don't consider the runoff)
!                                           PRR in kg/m2/s therefore PWS in mm
!
IF (LHOOK) CALL DR_HOOK('URBAN_RUNOFF',0,ZHOOK_HANDLE)
!
! 1. Evolution of water reservoir content 
!    ------------------------------------
!
  PWS(:) =  PWS(:) + PTSTEP * (PRR(:) + PIRRIG(:))  
!  
! Ws must be positive
  PWS(:) = MAX(0., PWS(:))
!
!
! 2. Calculation of surface runoff
!    -----------------------------
!
! if Ws_town > Ws_town_max, there is runoff
!
  PRUNOFF(:) = MAX(0., (PWS(:) - PWS_MAX(:)) / PTSTEP ) !
!
! Calculation of runoff reaching or not the sewer
  PNOC(:)    = (1.-PCONNEX(:)) * PRUNOFF(:)
  PRUNOFF(:) =     PCONNEX(:)  * PRUNOFF(:) 
!
! 3. Update of water reservoir content
!    ---------------------------------
!
! The reservoir water content is limited by the max capacity of water storage
  PWS(:)     = MIN(PWS(:), PWS_MAX(:)) !
!
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('URBAN_RUNOFF',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE URBAN_RUNOFF
