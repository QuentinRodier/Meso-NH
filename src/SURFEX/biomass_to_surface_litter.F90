!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE BIOMASS_TO_SURFACE_LITTER (PTURNOVER, PSURFACE_LITTER, PSURFACE_LIGNIN_STRUC)  

!   ###############################################################
!!**  BIOMASS_TO_SURFACE_LITTER 
!!
!!    PURPOSE
!!    -------
!!    Calculates surface litter evolution.
!!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!    none
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!      
!!    none
!!
!!    REFERENCE
!!    ---------
!!
!!      Parton et al., Biogeochemestry, 1988
!!      Krinner et al., Global Biochemical Cycles, 2005
!!      Gibelin et al. 2008, AFM
!!      
!!    AUTHOR
!!    ------
!!
!!     B. Decharme           * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    11/2020
!!
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!    
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODD_CO2V_PAR,       ONLY : XLC, XFRAC_LITTER
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1 input
!
! Turnover rates (gC/m**2)
REAL, DIMENSION(:,:), INTENT(IN)                                 :: PTURNOVER
!
!*       0.2 modified fields
!
! metabolic and structural litter, above ground 
REAL, DIMENSION(:,:),   INTENT(INOUT)                            :: PSURFACE_LITTER       ! Surface litter pools (gC m-2)
!
! ratio Lignin/Carbon in structural litter, above ground (gC/m**2)
REAL, DIMENSION(:),     INTENT(INOUT)                            :: PSURFACE_LIGNIN_STRUC ! Surface L/C ratio in structural litter
!
!*       0.4 local
! old structural litter, above (gC/m**2)
REAL, DIMENSION(SIZE(PSURFACE_LITTER,1))                         :: ZOLD_SURFACE_STRUC
!
! increase of metabolic and structural surface litter (gC/m**2)
REAL, DIMENSION(SIZE(PSURFACE_LITTER,1),SIZE(PSURFACE_LITTER,2)) :: ZSURFACE_LITTER_INC
!
! lignin increase in structural litterabove ground (gC/m**2)
REAL, DIMENSION(SIZE(PSURFACE_LITTER,1))                         :: ZSURFACE_LIGNIN_STRUC_INC
!
REAL, DIMENSION(SIZE(PTURNOVER,1),SIZE(PTURNOVER,2),2)           :: ZTURNOVER
!
REAL, DIMENSION(SIZE(PSURFACE_LITTER,1))                         :: ZWORK
!
! indices
INTEGER                                                          :: INI,INB,JI,JNB
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
! correspondence between array indices and litter type
! LT_METABOLIC = 1
! LT_STRUCTURAL = 2
!-------------------------------------------------------------------------------
!
!*    1 Initialisations
!
!
!*    1.1 dimensions
!
IF (LHOOK) CALL DR_HOOK('BIOMASS_TO_SURFACE_LITTER',0,ZHOOK_HANDLE)
!
INI = SIZE(PSURFACE_LITTER,1)
INB = SIZE(PTURNOVER,2)
!
!*    2 Add biomass to different litterpools
!
!*    2.1 first, save old structural litter (needed for lignin fractions).
!            (above/above)
!
ZOLD_SURFACE_STRUC(:) = PSURFACE_LITTER(:,2)
!
! *   2.2 update litter, and lignin content in structural litter
!
ZSURFACE_LITTER_INC    (:,:) = 0.0
ZSURFACE_LIGNIN_STRUC_INC(:) = 0.0
!
!*    2.2.1 calculate litter increase (per m**2 of ground).
!           Litter increase for structural and metabolic, above/above
!
DO JNB=1,INB
   DO JI=1,INI
      ZTURNOVER(JI,JNB,1) = XFRAC_LITTER(JNB,1) * PTURNOVER(JI,JNB)
      ZTURNOVER(JI,JNB,2) = XFRAC_LITTER(JNB,2) * PTURNOVER(JI,JNB)
   ENDDO
ENDDO
!
! metabolic
ZSURFACE_LITTER_INC(:,1) = ZTURNOVER(:,1,1)+ZTURNOVER(:,2,1)+ZTURNOVER(:,3,1)+ZTURNOVER(:,5,1)
! structural
ZSURFACE_LITTER_INC(:,2) = ZTURNOVER(:,1,2)+ZTURNOVER(:,2,2)+ZTURNOVER(:,3,2)+ZTURNOVER(:,5,2) 
!
!*    2.2.2 lignin increase in structural litter
!
ZSURFACE_LIGNIN_STRUC_INC(:) = XLC(1)*ZTURNOVER(:,1,2)+XLC(2)*ZTURNOVER(:,2,2) &
                             + XLC(3)*ZTURNOVER(:,3,2)+XLC(5)*ZTURNOVER(:,5,2)
!
!*    2.2.3 add new litter (struct/met, above/above)
!
WHERE(PSURFACE_LITTER(:,:)/=XUNDEF)
      PSURFACE_LITTER(:,:) = PSURFACE_LITTER(:,:) + ZSURFACE_LITTER_INC(:,:)
ENDWHERE
!
!*    2.2.4 for security: can't add more lignin than structural litter
!
WHERE(ZSURFACE_LITTER_INC(:,2)>=0.0)
      !positif turnover
      ZSURFACE_LIGNIN_STRUC_INC(:) = MIN(ZSURFACE_LIGNIN_STRUC_INC(:),ZSURFACE_LITTER_INC(:,2))
ELSEWHERE
      !negatif turnover (only for carbon conservation during land use change)
      ZSURFACE_LIGNIN_STRUC_INC(:) = MAX(ZSURFACE_LIGNIN_STRUC_INC(:),ZSURFACE_LITTER_INC(:,2))
ENDWHERE
!
!*    2.2.5 new lignin content: add old lignin and lignin increase, divide by 
!           total structural litter
!
ZWORK(:) = PSURFACE_LIGNIN_STRUC(:)*ZOLD_SURFACE_STRUC(:)+ZSURFACE_LIGNIN_STRUC_INC(:)
!
WHERE(PSURFACE_LITTER(:,2)>0.0.AND.PSURFACE_LITTER(:,2)/=XUNDEF)
      PSURFACE_LIGNIN_STRUC(:) = ZWORK(:)/PSURFACE_LITTER(:,2)
ENDWHERE
!
IF (LHOOK) CALL DR_HOOK('BIOMASS_TO_SURFACE_LITTER',1,ZHOOK_HANDLE)

!
END SUBROUTINE BIOMASS_TO_SURFACE_LITTER
