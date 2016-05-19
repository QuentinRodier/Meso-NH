!     ######################################################################
      SUBROUTINE Z0REL_1D(PAOSIP,PAOSIM,PAOSJP,PAOSJM,            &
                          PHO2IP,PHO2IM,PHO2JP,PHO2JM,            &
                          PZ0REL,OMASK                            )
!     ######################################################################
!
!!*SUBSCALE_Z0EFF  computes an effective roughness lenght deduced
!!                 from the subgrid-scale orography.
!!
!!
!!    METHOD
!!    ------
!!    See M.Georgelin and al. July 1994, Monthly Weather Review.
!!   
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
!!
!!    M. Georgelin      Laboratoire d'Aerologie
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    18/12/95
!!                22/12/97 (V Masson) call with dummy arguments
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_SURF_PAR, ONLY : XUNDEF
USE MODD_CSTS,     ONLY : XKARMAN
USE MODD_ISBA_PAR, ONLY : XCDZ0EFF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*    0.1    Declaration of dummy arguments
!            ------------------------------
!
REAL, DIMENSION(:), INTENT(IN)  :: PAOSIP  ! A/S for increasing x
REAL, DIMENSION(:), INTENT(IN)  :: PAOSIM  ! A/S for decreasing x
REAL, DIMENSION(:), INTENT(IN)  :: PAOSJP  ! A/S for increasing y
REAL, DIMENSION(:), INTENT(IN)  :: PAOSJM  ! A/S for decreasing y
REAL, DIMENSION(:), INTENT(IN)  :: PHO2IP  ! h/2 for increasing x
REAL, DIMENSION(:), INTENT(IN)  :: PHO2IM  ! h/2 for decreasing x
REAL, DIMENSION(:), INTENT(IN)  :: PHO2JP  ! h/2 for increasing y
REAL, DIMENSION(:), INTENT(IN)  :: PHO2JM  ! h/2 for decreasing y
!
REAL, DIMENSION(:), INTENT(OUT) :: PZ0REL  ! roughness length
!                                                      ! of SSO only
LOGICAL, DIMENSION(:), INTENT(IN) :: OMASK ! mask where computations
                                                       ! are done
!
!*    0.2    Declaration of other local variables
!            ------------------------------------
!
REAL,    DIMENSION(SIZE(PAOSIP)) :: ZLOC
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!----------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('Z0REL_1D',0,ZHOOK_HANDLE)
!
PZ0REL=XUNDEF
!
ZLOC(:) = 0.
!
WHERE (OMASK(:))
  ZLOC  (:) = 0.25 * XCDZ0EFF/(2.*XKARMAN**2)                  &
                   * (PAOSIP(:)+PAOSIM(:)+PAOSJP(:)+PAOSJM(:))        
  WHERE ( ZLOC(:) > 0. )
    PZ0REL(:) = 0.25 * (PHO2IP(:)+PHO2IM(:)+PHO2JP(:)+PHO2JM(:)) &
                     * EXP(-SQRT(1./ZLOC(:)))
    PZ0REL(:) = MAX(PZ0REL(:),1E-10)                     
  ELSEWHERE
    PZ0REL(:) = 0.
  END WHERE
END WHERE
!
IF (LHOOK) CALL DR_HOOK('Z0REL_1D',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
END SUBROUTINE Z0REL_1D
