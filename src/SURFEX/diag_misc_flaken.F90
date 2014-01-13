!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
       SUBROUTINE DIAG_MISC_FLAKE_n(PT_WML,PT_BOT,PH_ML,PCT,PWATER_DEPTH)
!     ###############################################################################
!
!!****  *DIAG_MISC-FLAKE_n * - additional diagnostics for FLake
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
!!     P. Le Moigne 
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    10/2005
!!------------------------------------------------------------------
!
!
!
USE MODD_DIAG_MISC_FLAKE_n,    ONLY : LWATER_PROFILE, XZW_PROFILE, XTW_PROFILE
USE MODD_SURF_PAR,           ONLY : XUNDEF
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
       REAL, DIMENSION(:), INTENT(IN) :: PT_WML       ! Mixed-layer temperature [K]
       REAL, DIMENSION(:), INTENT(IN) :: PT_BOT       ! Temperature at the water-bottom sediment 
       REAL, DIMENSION(:), INTENT(IN) :: PH_ML        ! Thickness of the mixed-layer [m]
       REAL, DIMENSION(:), INTENT(IN) :: PCT          ! Shape factor (thermocline)
       REAL, DIMENSION(:), INTENT(IN) :: PWATER_DEPTH ! Lake depth 
!
!*      0.2    declarations of local variables
!
       REAL, DIMENSION(SIZE(XZW_PROFILE),SIZE(PT_WML)) :: ZCSI      ! Vertical normalized coordinate
       REAL, DIMENSION(SIZE(XZW_PROFILE),SIZE(PT_WML)) :: ZSHAPE    ! Shape function 
       INTEGER ::                           IZW
       REAL(KIND=JPRB) :: ZHOOK_HANDLE

!
!-------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('DIAG_MISC_FLAKE_N',0,ZHOOK_HANDLE)
XTW_PROFILE = XUNDEF
IF (LWATER_PROFILE) THEN
   DO IZW=1,SIZE(XZW_PROFILE)
      WHERE (PWATER_DEPTH(:)==PH_ML(:))
         ZCSI(IZW,:) = 0.
      ELSEWHERE
         ZCSI(IZW,:) = (XZW_PROFILE(IZW) - PH_ML(:))/(PWATER_DEPTH(:) - PH_ML(:))
      END WHERE
      ZSHAPE(IZW,:) = (40./3.*PCT-20./3.)*ZCSI(IZW,:)+(18-30*PCT)*ZCSI(IZW,:)**2 &
                        +(20*PCT-12)*ZCSI(IZW,:)**3+(5./3.-10./3.*PCT)*ZCSI(IZW,:)**4  
   END DO
   DO IZW=1,SIZE(XZW_PROFILE)
      WHERE (PH_ML(:) >= XZW_PROFILE(IZW))
         XTW_PROFILE(IZW,:) =  PT_WML(:) 
      ELSEWHERE (PWATER_DEPTH(:) >= XZW_PROFILE(IZW)) 
         XTW_PROFILE(IZW,:) = PT_WML(:) - (PT_WML(:) - PT_BOT(:)) * ZSHAPE(IZW,:)
      END WHERE
   END DO
END IF
IF (LHOOK) CALL DR_HOOK('DIAG_MISC_FLAKE_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE DIAG_MISC_FLAKE_n
