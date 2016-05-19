!     #########
SUBROUTINE UPDATE_RAD_SEAWAT(HALB,PSST,PZENITH,PTT,PEMIS,PDIR_ALB,PSCA_ALB,     &
                               PDIR_ALB_ATMOS,PSCA_ALB_ATMOS,PEMIS_ATMOS,PTRAD)  
!     #######################################################################
!
!!****  *UPDATE_RAD_SEAWAT * - update the radiative properties at time t+1 (see by the atmosphere) 
!                           in order to close the energy budget between surfex and the atmosphere
 
!!
!!    PURPOSE
!!    -------
!
!!**  METHOD
!!    ------
!!
!!    REFERENCE
!!    ---------
!!      
!!
!!    AUTHOR
!!    ------
!!     B. Decharme 
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    09/2009
!!      Modified    03/2011 : E. Bazile (MK10) albedo from Marat Khairoutdinov
!!------------------------------------------------------------------
!
USE MODD_WATER_PAR
USE MODD_CSTS,       ONLY : XTT
USE MODD_SURF_ATM,   ONLY : LCPL_ESM
!
USE MODI_ALBEDO_TA96
USE MODI_ALBEDO_MK10
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
 CHARACTER(LEN=4),       INTENT(IN)   :: HALB
!
REAL, DIMENSION(:),     INTENT(IN)   :: PSST      ! Sea surface temperature
REAL, DIMENSION(:),     INTENT(IN)   :: PZENITH   ! Zenithal angle at t+1
REAL,                   INTENT(IN)   :: PTT       ! Sea/ice transition temperature (different according to sea or inland water)
!
REAL, DIMENSION(:),     INTENT(INOUT):: PDIR_ALB  ! Direct albedo at t+1
REAL, DIMENSION(:),     INTENT(INOUT):: PSCA_ALB  ! Diffuse albedo at t+1
REAL, DIMENSION(:),     INTENT(OUT)  :: PEMIS     ! emissivity (soil+vegetation) at t+1
!
REAL, DIMENSION(:,:),   INTENT(OUT)  :: PDIR_ALB_ATMOS ! Direct albedo at t+1 for the atmosphere
REAL, DIMENSION(:,:),   INTENT(OUT)  :: PSCA_ALB_ATMOS ! Diffuse albedo at t+1 for the atmosphere
REAL, DIMENSION(:),     INTENT(OUT)  :: PEMIS_ATMOS    ! Emissivity at t+1 for the atmosphere
REAL, DIMENSION(:),     INTENT(OUT)  :: PTRAD          ! radiative temp at t+1 for the atmosphere
!
!*      0.2    declarations of local variables
!
INTEGER :: JSWB
REAL, DIMENSION(SIZE(PSST)) :: ZALBEDO
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('UPDATE_RAD_SEAWAT',0,ZHOOK_HANDLE)
!
ZALBEDO(:) = 0.
IF (HALB=='TA96') THEN
  ZALBEDO(:) = ALBEDO_TA96(PZENITH(:))
ELSEIF (HALB=='MK10') THEN
  ZALBEDO(:) = ALBEDO_MK10(PZENITH(:))
ENDIF
!
IF(LCPL_ESM)THEN !Earth System Model
!
!Sea and/or ice albedo already given by oceanic model
!Except for Taylor et al (1996) formulation
!
  !
  WHERE (PSST(:)>=PTT  )
    !* open water
    PEMIS   (:) = XEMISWAT
  ELSEWHERE
    !* sea ice
    PEMIS   (:) = XEMISWATICE
  END WHERE
  !
  IF (HALB=='TA96' .OR. HALB=='MK10') THEN
    !* Taylor et al 1996
    !* open water
    WHERE (PSST(:)>=PTT) PDIR_ALB(:) = ZALBEDO(:)
    WHERE (PSST(:)>=PTT) PSCA_ALB(:) = XALBSCA_WAT
  ENDIF
  !
ELSE
  !
  IF (HALB=='UNIF') THEN
  !* uniform albedo
    WHERE (PSST(:)>=PTT  )
    !* open water
      PDIR_ALB  (:) = XALBWAT
      PSCA_ALB  (:) = XALBWAT
      PEMIS     (:) = XEMISWAT
    ELSEWHERE
    !* sea ice
      PDIR_ALB(:) = XALBWATICE
      PSCA_ALB(:) = XALBWATICE
      PEMIS   (:) = XEMISWATICE
    END WHERE
  !
  ELSE IF (HALB=='TA96' .OR. HALB=='MK10') THEN
    !* Taylor et al 1996
    WHERE (PSST(:)>=PTT) PDIR_ALB(:) = ZALBEDO(:)
    !
    WHERE (PSST(:)>=PTT)
    !* open water
      PSCA_ALB  (:) = XALBSCA_WAT
      PEMIS     (:) = XEMISWAT
    ELSEWHERE
    !* sea ice
      PDIR_ALB(:) = XALBWATICE
      PSCA_ALB(:) = XALBWATICE
      PEMIS   (:) = XEMISWATICE
    END WHERE
    !
  ENDIF
  !
ENDIF
!
!-------------------------------------------------------------------------------------
!
DO JSWB=1,SIZE(PDIR_ALB_ATMOS,2)
  PDIR_ALB_ATMOS(:,JSWB) = PDIR_ALB(:)
  PSCA_ALB_ATMOS(:,JSWB) = PSCA_ALB(:)
END DO
!
PEMIS_ATMOS(:) = PEMIS(:)
PTRAD      (:) = PSST (:)
!
IF (LHOOK) CALL DR_HOOK('UPDATE_RAD_SEAWAT',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE UPDATE_RAD_SEAWAT

