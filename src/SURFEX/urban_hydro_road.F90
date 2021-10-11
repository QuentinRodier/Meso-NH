!     #########
    SUBROUTINE URBAN_HYDRO_ROAD(PWS_MAX, PINFIL_ROAD,             &
                                PWS, PRR, PTSTEP,                 &
                                PWSOIL                            )
!   ###############################################################
!
!!****  *URBAN_HYDRO_ROAD*  
!!
!!    PURPOSE
!!    -------
!
!     Computes the evolution of prognostic liquid water reservoirs (retained on the road)
!     of urbanized areas in the DIF version.     
!     Also determine the runoff from the water that reaches the
!     ground on the road
! 
!!**  METHOD
!     ------
!
!   The roof reservoir runoff goes directly into the road reservoir.
!   Runoff occurs for road reservoir (too much water), as well as drainage
!   (evacuation system, typical time scale: 1 day)
!   
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    MODD_CST
!!      
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!
!!	V. Masson           * Meteo-France *
!!      J-M. Brun           ** IFSTTAR **
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    23/01/98 
!!      Modif       04/12
!!      
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
REAL, DIMENSION(:), INTENT(IN)    :: PWS_MAX      ! maximum deepness of roof water reservoir (mm)
REAL, DIMENSION(:), INTENT(IN)    :: PINFIL_ROAD  ! Water infiltration through roads (kg/m2/s)
REAL, DIMENSION(:), INTENT(INOUT) :: PWS          ! water reservoir (mm)
REAL, DIMENSION(:), INTENT(IN)    :: PRR          ! rain rate (kg/m2/s)
REAL,               INTENT(IN)    :: PTSTEP       ! time step (s)
REAL, DIMENSION(:), INTENT(OUT)   :: PWSOIL       ! infiltration through the road (kg/m2) (mm)
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!*      0.2    declarations of local variables
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('URBAN_HYDRO_ROAD',0,ZHOOK_HANDLE)
!
!*      1. Road water reservoir evolution
!          ------------------------------
!
!
! (if we don't consider the runoff)
! PRR in kg/m2/s therefore PWS in mm = kg/m2
!
PWS(:) = PWS(:) - PTSTEP * PINFIL_ROAD(:) 
!
!                                           Ws_town must be positive!
!
WHERE (PWS(:) > 0.)
  PWSOIL(:) = PTSTEP * PINFIL_ROAD 
ELSEWHERE
  PWSOIL(:) = MAX( 0. , PTSTEP * PINFIL_ROAD(:) - ABS(PWS(:)) )
!   
ENDWHERE
!
PWS(:) = MAX(0., PWS(:))
PWS(:) = MIN(PWS(:), PWS_MAX)
!
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('URBAN_HYDRO_ROAD',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE URBAN_HYDRO_ROAD
