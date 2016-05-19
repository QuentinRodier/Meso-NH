!     #########
      SUBROUTINE AVERAGE_RAD(PFRAC_TILE,                              &
                   PDIR_ALB_TILE, PSCA_ALB_TILE, PEMIS_TILE, PTRAD_TILE,  &
                   PDIR_ALB, PSCA_ALB, PEMIS, PTRAD                       )  
!     #################################################################
!
!
!!****  *AVERAGE_RAD*  
!!
!!    PURPOSE
!!    -------
!      Average the radiative fluxes from the land and water surfaces depending on the
!      fraction of each surface cover type in the mesh area.
!     
!!**  METHOD
!!    ------
!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------ 
!!
!!      
!!    REFERENCE
!!    ---------
!!      
!!    AUTHOR
!!    ------
!!	S. Belair           * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    10/03/95 
!!      V.Masson    20/03/96  remove abnormal averages and average TS**4 instead
!!                            of TS
!!      (J.Stein)   27/03/96 use only H and LE in the soil scheme
!!      A. Boone    27/11/02 revised to output ALMA variables, and general applications
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
!
!
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
!*      0.1    declarations of arguments
!
REAL, DIMENSION(:,:),   INTENT(IN) :: PFRAC_TILE    ! Fraction in a mesh-area of 
!                                                   ! a given surface
REAL, DIMENSION(:,:),   INTENT(IN) :: PEMIS_TILE    ! emissivity
REAL, DIMENSION(:,:,:), INTENT(IN) :: PDIR_ALB_TILE ! direct albedo
REAL, DIMENSION(:,:,:), INTENT(IN) :: PSCA_ALB_TILE ! diffuse albedo
REAL, DIMENSION(:,:),   INTENT(IN) :: PTRAD_TILE    ! surface radiative temp.
REAL, DIMENSION(:),     INTENT(OUT):: PEMIS         ! emissivity
REAL, DIMENSION(:,:),   INTENT(OUT):: PDIR_ALB      ! direct albedo
REAL, DIMENSION(:,:),   INTENT(OUT):: PSCA_ALB      ! diffuse albedo
REAL, DIMENSION(:),     INTENT(OUT):: PTRAD         ! surface radiative temp.
!
!
!*      0.2    declarations of local variables
!
INTEGER :: JSWB ! loop counter on number of SW spectral bands
INTEGER :: JTILE! loop counter on tiles
REAL, PARAMETER    :: ZEPS = 1.E-10
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!       1.     Grid-Box average surface temperatures, radiative properties
!              -----------------------------------------------------------
!
! albedo:
! 
IF (LHOOK) CALL DR_HOOK('AVERAGE_RAD',0,ZHOOK_HANDLE)
PDIR_ALB   (:,:) = 0.
PSCA_ALB   (:,:) = 0.
!
DO JSWB = 1,SIZE(PDIR_ALB_TILE,2)
  DO JTILE = 1,SIZE(PDIR_ALB_TILE,3)
    PDIR_ALB(:,JSWB) = PDIR_ALB(:,JSWB) + PFRAC_TILE(:,JTILE) * PDIR_ALB_TILE(:,JSWB,JTILE)
    PSCA_ALB(:,JSWB) = PSCA_ALB(:,JSWB) + PFRAC_TILE(:,JTILE) * PSCA_ALB_TILE(:,JSWB,JTILE)
  END DO
END DO
!
! emissivity
!
PEMIS      (:)   = 0.
!
DO JTILE = 1,SIZE(PEMIS_TILE,2)
  PEMIS(:) = PEMIS(:) + PFRAC_TILE(:,JTILE) * PEMIS_TILE(:,JTILE)
END DO
!
! radiative surface temperature
!
PTRAD      (:)   = 0.
!
DO JTILE = 1, SIZE(PEMIS_TILE,2)
!
   PTRAD(:) = PTRAD(:) + (PTRAD_TILE(:,JTILE)**4)*PFRAC_TILE(:,JTILE)*PEMIS_TILE(:,JTILE)
!
END DO

PTRAD(:) = ( PTRAD(:) / MAX(PEMIS(:),ZEPS) )**0.25
IF (LHOOK) CALL DR_HOOK('AVERAGE_RAD',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE AVERAGE_RAD
