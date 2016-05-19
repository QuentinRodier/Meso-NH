!     ##########################
      MODULE MODI_SUBSCALE_Z0EFF
!     ##########################
INTERFACE SUBSCALE_Z0EFF
      SUBROUTINE SUBSCALE_Z0EFF_1D_NVEG(PAOSIP,PAOSIM,PAOSJP,PAOSJM,            &
                                PHO2IP,PHO2IM,PHO2JP,PHO2JM,PZ0VEG,        &
                                PZ0EFFIP,PZ0EFFIM,PZ0EFFJP,PZ0EFFJM,       &
                                PZ0REL,OMASK                               )
!
REAL, DIMENSION(:), INTENT(IN)  :: PAOSIP  ! A/S for increasing x
REAL, DIMENSION(:), INTENT(IN)  :: PAOSIM  ! A/S for decreasing x
REAL, DIMENSION(:), INTENT(IN)  :: PAOSJP  ! A/S for increasing y
REAL, DIMENSION(:), INTENT(IN)  :: PAOSJM  ! A/S for decreasing y
REAL, DIMENSION(:), INTENT(IN)  :: PHO2IP  ! h/2 for increasing x
REAL, DIMENSION(:), INTENT(IN)  :: PHO2IM  ! h/2 for decreasing x
REAL, DIMENSION(:), INTENT(IN)  :: PHO2JP  ! h/2 for increasing y
REAL, DIMENSION(:), INTENT(IN)  :: PHO2JM  ! h/2 for decreasing y
REAL,               INTENT(IN)  :: PZ0VEG  ! vegetation roughness length
!
REAL, DIMENSION(:), INTENT(INOUT) :: PZ0EFFIP! roughness length for increasing x
REAL, DIMENSION(:), INTENT(INOUT) :: PZ0EFFIM! roughness length for decreasing x
REAL, DIMENSION(:), INTENT(INOUT) :: PZ0EFFJP! roughness length for increasing y
REAL, DIMENSION(:), INTENT(INOUT) :: PZ0EFFJM! roughness length for decreasing y
!
REAL, DIMENSION(:), INTENT(OUT), OPTIONAL :: PZ0REL  ! roughness length
!                                                      ! of SSO only
LOGICAL, DIMENSION(:), INTENT(IN), OPTIONAL :: OMASK ! mask where computations
                                                       ! are done
!
END SUBROUTINE SUBSCALE_Z0EFF_1D_NVEG
!
      SUBROUTINE SUBSCALE_Z0EFF_1D_BIS(PAOSIP,PAOSIM,PAOSJP,PAOSJM,        &
                                PHO2IP,PHO2IM,PHO2JP,PHO2JM,PZ0VEG,        &
                                PZ0EFFIP,PZ0EFFIM,PZ0EFFJP,PZ0EFFJM,       &
                                PZ0REL,OMASK                               )
!
REAL, DIMENSION(:), INTENT(IN)  :: PAOSIP  ! A/S for increasing x
REAL, DIMENSION(:), INTENT(IN)  :: PAOSIM  ! A/S for decreasing x
REAL, DIMENSION(:), INTENT(IN)  :: PAOSJP  ! A/S for increasing y
REAL, DIMENSION(:), INTENT(IN)  :: PAOSJM  ! A/S for decreasing y
REAL, DIMENSION(:), INTENT(IN)  :: PHO2IP  ! h/2 for increasing x
REAL, DIMENSION(:), INTENT(IN)  :: PHO2IM  ! h/2 for decreasing x
REAL, DIMENSION(:), INTENT(IN)  :: PHO2JP  ! h/2 for increasing y
REAL, DIMENSION(:), INTENT(IN)  :: PHO2JM  ! h/2 for decreasing y
REAL, DIMENSION(:), INTENT(IN)  :: PZ0VEG  ! vegetation roughness length
!
REAL, DIMENSION(:), INTENT(INOUT) :: PZ0EFFIP! roughness length for increasing x
REAL, DIMENSION(:), INTENT(INOUT) :: PZ0EFFIM! roughness length for decreasing x
REAL, DIMENSION(:), INTENT(INOUT) :: PZ0EFFJP! roughness length for increasing y
REAL, DIMENSION(:), INTENT(INOUT) :: PZ0EFFJM! roughness length for decreasing y
!
REAL, DIMENSION(:), INTENT(OUT), OPTIONAL :: PZ0REL  ! roughness length
!                                                      ! of SSO only
LOGICAL, DIMENSION(:), INTENT(IN), OPTIONAL :: OMASK ! mask where computations
                                                       ! are done
!
END SUBROUTINE SUBSCALE_Z0EFF_1D_BIS

SUBROUTINE SUBSCALE_Z0EFF_1D_PATCH(PAOSIP,PAOSIM,PAOSJP,PAOSJM,            &
                                PHO2IP,PHO2IM,PHO2JP,PHO2JM,PZ0VEG,        &
                                PZ0EFFIP,PZ0EFFIM,PZ0EFFJP,PZ0EFFJM,       &
                                PZ0REL,OMASK                               )
!
REAL, DIMENSION(:), INTENT(IN)  :: PAOSIP  ! A/S for increasing x
REAL, DIMENSION(:), INTENT(IN)  :: PAOSIM  ! A/S for decreasing x
REAL, DIMENSION(:), INTENT(IN)  :: PAOSJP  ! A/S for increasing y
REAL, DIMENSION(:), INTENT(IN)  :: PAOSJM  ! A/S for decreasing y
REAL, DIMENSION(:), INTENT(IN)  :: PHO2IP  ! h/2 for increasing x
REAL, DIMENSION(:), INTENT(IN)  :: PHO2IM  ! h/2 for decreasing x
REAL, DIMENSION(:), INTENT(IN)  :: PHO2JP  ! h/2 for increasing y
REAL, DIMENSION(:), INTENT(IN)  :: PHO2JM  ! h/2 for decreasing y
REAL, DIMENSION(:,:), INTENT(IN)  :: PZ0VEG  ! vegetation roughness length
!
REAL, DIMENSION(:,:), INTENT(INOUT) :: PZ0EFFIP! roughness length for increasing x
REAL, DIMENSION(:,:), INTENT(INOUT) :: PZ0EFFIM! roughness length for decreasing x
REAL, DIMENSION(:,:), INTENT(INOUT) :: PZ0EFFJP! roughness length for increasing y
REAL, DIMENSION(:,:), INTENT(INOUT) :: PZ0EFFJM! roughness length for decreasing y
!
REAL, DIMENSION(:), INTENT(OUT), OPTIONAL :: PZ0REL  ! roughness length
!                                                      ! of SSO only
LOGICAL, DIMENSION(:), INTENT(IN), OPTIONAL :: OMASK ! mask where computations
                                                       ! are done
!
END SUBROUTINE SUBSCALE_Z0EFF_1D_PATCH

END INTERFACE
!
END MODULE MODI_SUBSCALE_Z0EFF
!     ######################################################################
      SUBROUTINE SUBSCALE_Z0EFF_1D_NVEG(PAOSIP,PAOSIM,PAOSJP,PAOSJM,       &
                                PHO2IP,PHO2IM,PHO2JP,PHO2JM,PZ0VEG,        &
                                PZ0EFFIP,PZ0EFFIM,PZ0EFFJP,PZ0EFFJM,       &
                                PZ0REL,OMASK                               )
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
!
USE MODI_SUBSCALE_Z0EFF_1D
USE MODI_Z0REL_1D
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
REAL,               INTENT(IN)  :: PZ0VEG  ! vegetation roughness length
!
REAL, DIMENSION(:), INTENT(INOUT) :: PZ0EFFIP! roughness length for increasing x
REAL, DIMENSION(:), INTENT(INOUT) :: PZ0EFFIM! roughness length for decreasing x
REAL, DIMENSION(:), INTENT(INOUT) :: PZ0EFFJP! roughness length for increasing y
REAL, DIMENSION(:), INTENT(INOUT) :: PZ0EFFJM! roughness length for decreasing y
!
REAL, DIMENSION(:), INTENT(OUT), OPTIONAL :: PZ0REL  ! roughness length
!                                                    ! of SSO only
LOGICAL, DIMENSION(:), INTENT(IN), OPTIONAL :: OMASK ! mask where computations
                                                       ! are done
!
REAL,    DIMENSION(SIZE(PZ0EFFIM)) :: ZZ0VEG
LOGICAL, DIMENSION(SIZE(PZ0EFFIM)) :: GMASK
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('MODI_SUBSCALE_Z0EFF:SUBSCALE_Z0EFF_1D_NVEG',0,ZHOOK_HANDLE)
!
IF (PRESENT(OMASK)) THEN
  GMASK=OMASK
ELSE
  GMASK=(PAOSIP/=XUNDEF)    ! computations always performed where SSO data exist
  PZ0EFFIP = XUNDEF
  PZ0EFFIM = XUNDEF
  PZ0EFFJP = XUNDEF
  PZ0EFFJM = XUNDEF
END IF
!
ZZ0VEG(:) = PZ0VEG
!
 CALL SUBSCALE_Z0EFF_1D(PAOSIP,PAOSIM,PAOSJP,PAOSJM,             &
                       PHO2IP,PHO2IM,PHO2JP,PHO2JM,ZZ0VEG,      &
                       PZ0EFFIP,PZ0EFFIM,PZ0EFFJP,PZ0EFFJM,     &
                       GMASK                                    )
!
IF (PRESENT(PZ0REL)) CALL Z0REL_1D(PAOSIP,PAOSIM,PAOSJP,PAOSJM, &
                                   PHO2IP,PHO2IM,PHO2JP,PHO2JM, &
                                   PZ0REL,GMASK                 )
!
IF (LHOOK) CALL DR_HOOK('MODI_SUBSCALE_Z0EFF:SUBSCALE_Z0EFF_1D_NVEG',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
END SUBROUTINE SUBSCALE_Z0EFF_1D_NVEG
!-------------------------------------------------------------------------------
!
!     ######################################################################
      SUBROUTINE SUBSCALE_Z0EFF_1D_BIS(PAOSIP,PAOSIM,PAOSJP,PAOSJM,        &
                                PHO2IP,PHO2IM,PHO2JP,PHO2JM,PZ0VEG,        &
                                PZ0EFFIP,PZ0EFFIM,PZ0EFFJP,PZ0EFFJM,       &
                                PZ0REL,OMASK                               )
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
!
USE MODI_SUBSCALE_Z0EFF_1D
USE MODI_Z0REL_1D
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
REAL, DIMENSION(:), INTENT(IN)  :: PZ0VEG  ! vegetation roughness length
!
REAL, DIMENSION(:), INTENT(INOUT) :: PZ0EFFIP! roughness length for increasing x
REAL, DIMENSION(:), INTENT(INOUT) :: PZ0EFFIM! roughness length for decreasing x
REAL, DIMENSION(:), INTENT(INOUT) :: PZ0EFFJP! roughness length for increasing y
REAL, DIMENSION(:), INTENT(INOUT) :: PZ0EFFJM! roughness length for decreasing y
!
REAL, DIMENSION(:), INTENT(OUT), OPTIONAL :: PZ0REL  ! roughness length
!                                                      ! of SSO only
LOGICAL, DIMENSION(:), INTENT(IN), OPTIONAL :: OMASK ! mask where computations
                                                       ! are done
!
!*    0.2    Declaration of other local variables
!            ------------------------------------
!
LOGICAL, DIMENSION(SIZE(PZ0EFFIM)) :: GMASK
!
INTEGER         :: JJ
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!----------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('MODI_SUBSCALE_Z0EFF:SUBSCALE_Z0EFF_1D_BIS',0,ZHOOK_HANDLE)
!
IF (PRESENT(OMASK)) THEN
  GMASK=OMASK
ELSE
  GMASK=(PAOSIP/=XUNDEF)    ! computations always performed where SSO data exist
  PZ0EFFIP = XUNDEF
  PZ0EFFIM = XUNDEF
  PZ0EFFJP = XUNDEF
  PZ0EFFJM = XUNDEF
END IF
!
 CALL SUBSCALE_Z0EFF_1D(PAOSIP,PAOSIM,PAOSJP,PAOSJM,             &
                       PHO2IP,PHO2IM,PHO2JP,PHO2JM,PZ0VEG,      &
                       PZ0EFFIP,PZ0EFFIM,PZ0EFFJP,PZ0EFFJM,     &
                       GMASK                                    )
!
IF (PRESENT(PZ0REL)) CALL Z0REL_1D(PAOSIP,PAOSIM,PAOSJP,PAOSJM, &
                                   PHO2IP,PHO2IM,PHO2JP,PHO2JM, &
                                   PZ0REL,GMASK                 )
!
IF (LHOOK) CALL DR_HOOK('MODI_SUBSCALE_Z0EFF:SUBSCALE_Z0EFF_1D_BIS',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE SUBSCALE_Z0EFF_1D_BIS
!
!     ######spl
      SUBROUTINE SUBSCALE_Z0EFF_1D_PATCH(PAOSIP,PAOSIM,PAOSJP,PAOSJM,      &
                                PHO2IP,PHO2IM,PHO2JP,PHO2JM,PZ0VEG,        &
                                PZ0EFFIP,PZ0EFFIM,PZ0EFFJP,PZ0EFFJM,       &
                                PZ0REL,OMASK                               )
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
USE MODD_CSTS,       ONLY : XKARMAN
USE MODD_ISBA_PAR,   ONLY : XCDZ0EFF
USE MODD_SURF_PAR,   ONLY : XUNDEF
!
USE MODI_SUBSCALE_Z0EFF_1D
USE MODI_Z0REL_1D
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
REAL, DIMENSION(:,:), INTENT(IN)  :: PZ0VEG  ! vegetation roughness length
!
REAL, DIMENSION(:,:), INTENT(INOUT) :: PZ0EFFIP! roughness length for increasing x
REAL, DIMENSION(:,:), INTENT(INOUT) :: PZ0EFFIM! roughness length for decreasing x
REAL, DIMENSION(:,:), INTENT(INOUT) :: PZ0EFFJP! roughness length for increasing y
REAL, DIMENSION(:,:), INTENT(INOUT) :: PZ0EFFJM! roughness length for decreasing y
!
REAL, DIMENSION(:), INTENT(OUT), OPTIONAL :: PZ0REL  ! roughness length
!                                                      ! of SSO only
LOGICAL, DIMENSION(:), INTENT(IN), OPTIONAL :: OMASK ! mask where computations
                                                       ! are done
!
!*    0.2    Declaration of other local variables
!            ------------------------------------
!
LOGICAL, DIMENSION(SIZE(PZ0EFFIM,1)) :: GMASK
!
INTEGER :: IPATCH  ! number of patches
INTEGER :: JPATCH  ! loop counter on number of patches
INTEGER :: JJ      ! loop counter on points
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!----------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('MODI_SUBSCALE_Z0EFF:SUBSCALE_Z0EFF_1D_PATCH',0,ZHOOK_HANDLE)
PZ0EFFIP = XUNDEF
PZ0EFFIM = XUNDEF
PZ0EFFJP = XUNDEF
PZ0EFFJM = XUNDEF
!
IPATCH = SIZE(PZ0VEG,2)
!----------------------------------------------------------------------------
DO JPATCH=1,IPATCH
!----------------------------------------------------------------------------
!
  IF (PRESENT(OMASK)) THEN
    GMASK=OMASK
  ELSE
    GMASK=PZ0VEG(:,JPATCH) /= XUNDEF    ! computations always performed where defined
  END IF
!
  CALL SUBSCALE_Z0EFF_1D(PAOSIP,PAOSIM,PAOSJP,PAOSJM,                &
                       PHO2IP,PHO2IM,PHO2JP,PHO2JM,PZ0VEG(:,JPATCH), &
                       PZ0EFFIP(:,JPATCH),PZ0EFFIM(:,JPATCH),        &
                       PZ0EFFJP(:,JPATCH),PZ0EFFJM(:,JPATCH),        &
                       GMASK                                         )
!
END DO
!
IF (PRESENT(OMASK)) THEN
  GMASK=OMASK
ELSE
  GMASK=(PAOSIP/=XUNDEF)
END IF
!
IF (PRESENT(PZ0REL)) CALL Z0REL_1D(PAOSIP,PAOSIM,PAOSJP,PAOSJM, &
                                   PHO2IP,PHO2IM,PHO2JP,PHO2JM, &
                                   PZ0REL,GMASK                 )
!
IF (LHOOK) CALL DR_HOOK('MODI_SUBSCALE_Z0EFF:SUBSCALE_Z0EFF_1D_PATCH',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE SUBSCALE_Z0EFF_1D_PATCH
