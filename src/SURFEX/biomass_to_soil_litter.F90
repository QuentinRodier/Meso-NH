!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE BIOMASS_TO_SOIL_LITTER (PTURNOVER, PSOIL_LITTER, PSOIL_LIGNIN_STRUC, &
                                   KWG_LAYER, PROOTFRAC)  
!
!   ###############################################################
!!**  BIOMASS_TO_SOIL_LITTER 
!!
!!    PURPOSE
!!    -------
!!    Calculates litter evolution.
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
REAL, DIMENSION(:,:), INTENT(IN)                                   :: PTURNOVER          ! Turnover rates (gC/m**2)
!
!*       0.2 modified fields
!
REAL, DIMENSION(:,:,:), INTENT(INOUT)                              :: PSOIL_LITTER       ! metabolic and structural litter (gC/m**2)
!
REAL, DIMENSION(:,:), INTENT(INOUT)                                :: PSOIL_LIGNIN_STRUC ! ratio Lignin/Carbon in structural litter (gC/m**2)
!
!*       0.3 optional
!
INTEGER, DIMENSION(:), INTENT(IN), OPTIONAL                        :: KWG_LAYER
!
REAL, DIMENSION(:,:), INTENT(IN), OPTIONAL                         :: PROOTFRAC     ! Cumulative
!
!*       0.4 local
!
REAL, DIMENSION(SIZE(PSOIL_LITTER,1),SIZE(PSOIL_LITTER,2))                      :: ZOLD_SOIL_STRUC        ! old structural litter (gC/m**2)
!
REAL, DIMENSION(SIZE(PSOIL_LITTER,1),SIZE(PSOIL_LITTER,2),SIZE(PSOIL_LITTER,3)) :: ZSOIL_LITTER_INC       ! increase of metabolic and structural litter (gC/m**2)
!
REAL, DIMENSION(SIZE(PSOIL_LIGNIN_STRUC,1),SIZE(PSOIL_LIGNIN_STRUC,2))          :: ZSOIL_LIGNIN_STRUC_INC ! lignin increase in structural litter (gC/m**2)
!
REAL, DIMENSION(SIZE(PTURNOVER,1),SIZE(PTURNOVER,2),2)                          :: ZTURNOVER
!
INTEGER, DIMENSION(SIZE(PSOIL_LITTER,1))                                        :: IWG_LAYER
!
REAL, DIMENSION(SIZE(PSOIL_LITTER,1),SIZE(PSOIL_LITTER,2))                      :: ZROOTFRAC ! Non cumulative
!
INTEGER :: INI,INL,JI,JL,IDEPTH
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
IF (LHOOK) CALL DR_HOOK('BIOMASS_TO_SOIL_LITTER',0,ZHOOK_HANDLE)
!
INI = SIZE(PSOIL_LITTER,1)
INL = SIZE(PSOIL_LITTER,2)
!
IF(PRESENT(KWG_LAYER))THEN
  IWG_LAYER(:) = KWG_LAYER(:)
ELSE
  IWG_LAYER(:) = 1
ENDIF
!
IF(PRESENT(PROOTFRAC))THEN
  ZROOTFRAC(:,1)=PROOTFRAC(:,1)
  DO JL=2,INL
     DO JI=1,INI
        IF(JL<=KWG_LAYER(JI))THEN
          ZROOTFRAC(JI,JL)=MAX(0.0,PROOTFRAC(JI,JL)-PROOTFRAC(JI,JL-1))
        ENDIF
     ENDDO
  ENDDO
ELSE
  ZROOTFRAC(:,:) = 1.0
ENDIF
!
!-------------------------------------------------------------------------------
!
!*    2. Compute input biomass to different litterpools
!        ----------------------------------------------
!
!*    2.1 first, save old structural litter (needed for lignin fractions).
!
ZOLD_SOIL_STRUC(:,:) = PSOIL_LITTER(:,:,2)
!
! *   2.2 update litter, and lignin content in structural litter
!
ZSOIL_LITTER_INC      (:,:,:) = 0.0
ZSOIL_LIGNIN_STRUC_INC(:,:)   = 0.0
!
!*    2.2.1 calculate litter and lignin increase (per m**2 of ground).
!           Litter increase for structural and metabolic, below
!
DO JL=1,SIZE(PTURNOVER,2)
   DO JI=1,INI
      ZTURNOVER(JI,JL,1) = XFRAC_LITTER(JL,1) * PTURNOVER(JI,JL)
      ZTURNOVER(JI,JL,2) = XFRAC_LITTER(JL,2) * PTURNOVER(JI,JL)
   ENDDO
ENDDO
!
DO JL=1,INL
  DO JI=1,INI
     IDEPTH=IWG_LAYER(JI)
     IF (JL<=IDEPTH) THEN
        ZSOIL_LITTER_INC      (JI,JL,1) = (       ZTURNOVER(JI,4,1)+       ZTURNOVER(JI,6,1)) * ZROOTFRAC(JI,JL)
        ZSOIL_LITTER_INC      (JI,JL,2) = (       ZTURNOVER(JI,4,2)+       ZTURNOVER(JI,6,2)) * ZROOTFRAC(JI,JL)
        ZSOIL_LIGNIN_STRUC_INC(JI,JL  ) = (XLC(4)*ZTURNOVER(JI,4,2)+XLC(6)*ZTURNOVER(JI,6,2)) * ZROOTFRAC(JI,JL)
     ENDIF
  ENDDO
ENDDO
!
!-------------------------------------------------------------------------------
!
!*    3 . Add turnover yo new litter (struct/met, below)
!       ------------------------------------------------
!
WHERE(PSOIL_LITTER(:,:,:)/=XUNDEF)
  PSOIL_LITTER(:,:,:) = PSOIL_LITTER(:,:,:) + ZSOIL_LITTER_INC(:,:,:)
ENDWHERE
!
!*    2.2.4 for security: can't add more lignin than structural litter
!
ZSOIL_LIGNIN_STRUC_INC(:,:) = MIN( ZSOIL_LIGNIN_STRUC_INC(:,:), ZSOIL_LITTER_INC(:,:,2) )
!
!*    2.2.5 new lignin content: add old lignin and lignin increase, divide by total structural litter
!
WHERE(PSOIL_LITTER(:,:,2)>0.0.AND.PSOIL_LITTER(:,:,2)/=XUNDEF)
      PSOIL_LIGNIN_STRUC(:,:) = (PSOIL_LIGNIN_STRUC(:,:)*ZOLD_SOIL_STRUC(:,:)+ZSOIL_LIGNIN_STRUC_INC(:,:))/PSOIL_LITTER(:,:,2)
ENDWHERE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('BIOMASS_TO_SOIL_LITTER',1,ZHOOK_HANDLE)

!
END SUBROUTINE BIOMASS_TO_SOIL_LITTER
