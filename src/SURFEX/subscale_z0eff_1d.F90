!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     ######################################################################
      SUBROUTINE SUBSCALE_Z0EFF_1D(PAOSIP,PAOSIM,PAOSJP,PAOSJM,            &
                                PHO2IP,PHO2IM,PHO2JP,PHO2JM,PZ0VEG,        &
                                PZ0EFFIP,PZ0EFFIM,PZ0EFFJP,PZ0EFFJM,       &
                                OMASK                                      )
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
!!                24/08/12 (B Decharme) optimization (loop into subroutine)
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
REAL, DIMENSION(:), INTENT(IN)  :: PZ0VEG  ! vegetation roughness length
!
REAL, DIMENSION(:), INTENT(INOUT) :: PZ0EFFIP! roughness length for increasing x
REAL, DIMENSION(:), INTENT(INOUT) :: PZ0EFFIM! roughness length for decreasing x
REAL, DIMENSION(:), INTENT(INOUT) :: PZ0EFFJP! roughness length for increasing y
REAL, DIMENSION(:), INTENT(INOUT) :: PZ0EFFJM! roughness length for decreasing y
!
LOGICAL, DIMENSION(:), INTENT(IN), OPTIONAL :: OMASK ! mask where computations
                                                       ! are done
!
!*    0.2    Declaration of other local variables
!            ------------------------------------
!
LOGICAL, DIMENSION(SIZE(PZ0EFFIM)) :: GMASK
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!----------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('SUBSCALE_Z0EFF_1D',0,ZHOOK_HANDLE)
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
!*    1.     Computations from A/S and h/2
!            -----------------------------
!      
 CALL GET_Z0EFF(GMASK(:),PZ0VEG(:),PHO2JP(:),PAOSJP(:),PZ0EFFJP(:))
 CALL GET_Z0EFF(GMASK(:),PZ0VEG(:),PHO2JM(:),PAOSJM(:),PZ0EFFJM(:))
 CALL GET_Z0EFF(GMASK(:),PZ0VEG(:),PHO2IM(:),PAOSIM(:),PZ0EFFIM(:))
 CALL GET_Z0EFF(GMASK(:),PZ0VEG(:),PHO2IP(:),PAOSIP(:),PZ0EFFIP(:))
!
IF (LHOOK) CALL DR_HOOK('SUBSCALE_Z0EFF_1D',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
CONTAINS
!
SUBROUTINE GET_Z0EFF(OCOMPUT,PZ0,PHO,PAO,PZ0EFF)
!
IMPLICIT NONE
!
LOGICAL, DIMENSION(:), INTENT(IN) :: OCOMPUT
REAL,    DIMENSION(:), INTENT(IN) :: PZ0
REAL,    DIMENSION(:), INTENT(IN) :: PHO
REAL,    DIMENSION(:), INTENT(IN) :: PAO
REAL,    DIMENSION(:), INTENT(OUT):: PZ0EFF
!
LOGICAL, DIMENSION(SIZE(PZ0)) :: LWORK1
!
REAL    :: ZLOC1,ZLOC2,ZLOC3
INTEGER :: JJ, INI
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('SUBSCALE_Z0EFF_1D:GET_ZOEFF',0,ZHOOK_HANDLE)
!
INI=SIZE(PZ0)
!
LWORK1(:)=(PHO(:)>PZ0(:).AND.(PZ0(:)/=0.0.OR.PAO(:)/=0.0))
!
DO JJ=1,INI
  IF (OCOMPUT(JJ)) THEN
    IF (LWORK1(JJ)) THEN 
      ZLOC1  = (XCDZ0EFF/(2.*XKARMAN**2))*PAO(JJ)
      IF ( PZ0(JJ) > 0. ) THEN
        ZLOC2 = 1./(ALOG(PHO(JJ)/PZ0(JJ)))**2
      ELSE
        ZLOC2 = 0.
      ENDIF 
      ZLOC3  = SQRT(1./(ZLOC1+ZLOC2))
      PZ0EFF(JJ) = PHO(JJ) * EXP(-ZLOC3)
    ELSE
      PZ0EFF(JJ) = PZ0(JJ) 
    ENDIF
  ENDIF
ENDDO
!
IF (LHOOK) CALL DR_HOOK('SUBSCALE_Z0EFF_1D:GET_ZOEFF',1,ZHOOK_HANDLE)
!
END SUBROUTINE GET_Z0EFF
! 
END SUBROUTINE SUBSCALE_Z0EFF_1D
