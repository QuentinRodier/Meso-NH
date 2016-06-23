!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!#######################
MODULE MODI_GREEN_FROM_LAI
!#######################
!
INTERFACE GREEN_FROM_LAI
!
    FUNCTION GREEN_FROM_LAI_0D(PLAI,PVEGTYPE,OAGRI_TO_GRASS) RESULT(PGREEN)
!
REAL,                 INTENT(IN) :: PLAI         ! Leaf area Index
REAL,   DIMENSION(:), INTENT(IN) :: PVEGTYPE     ! type of vegetation
LOGICAL,              INTENT(IN) :: OAGRI_TO_GRASS
!
REAL                             :: PGREEN       ! greeness fraction
!
END FUNCTION GREEN_FROM_LAI_0D
!
!
    FUNCTION GREEN_FROM_LAI_1D(PLAI,PVEGTYPE,OAGRI_TO_GRASS) RESULT(PGREEN)
!
REAL,   DIMENSION(:),   INTENT(IN) :: PLAI         ! Leaf area Index
REAL,   DIMENSION(:,:), INTENT(IN) :: PVEGTYPE     ! type of vegetation
LOGICAL,                INTENT(IN) :: OAGRI_TO_GRASS
!
REAL,   DIMENSION(SIZE(PLAI))      :: PGREEN       ! greeness fraction
!
END FUNCTION GREEN_FROM_LAI_1D
!
!
    FUNCTION GREEN_FROM_LAI_2D(PLAI,PVEGTYPE,OAGRI_TO_GRASS) RESULT(PGREEN)
!
REAL,   DIMENSION(:,:),   INTENT(IN) :: PLAI         ! Leaf area Index
REAL,   DIMENSION(:,:,:), INTENT(IN) :: PVEGTYPE     ! type of vegetation
LOGICAL,                  INTENT(IN) :: OAGRI_TO_GRASS
!
REAL,   DIMENSION(SIZE(PLAI,1),SIZE(PLAI,2))::PGREEN ! greeness fraction
!
END FUNCTION GREEN_FROM_LAI_2D
!

    FUNCTION GREEN_FROM_LAI_PATCH_1D(PLAI,PVEGTYPE,OAGRI_TO_GRASS) RESULT(PGREEN)
!
REAL,   DIMENSION(:), INTENT(IN) :: PLAI         ! Leaf area Index for each vegtype
REAL,   DIMENSION(:), INTENT(IN) :: PVEGTYPE     ! 
LOGICAL,              INTENT(IN) :: OAGRI_TO_GRASS
!
REAL,   DIMENSION(SIZE(PLAI)) :: PGREEN ! greeness fraction
!
END FUNCTION GREEN_FROM_LAI_PATCH_1D
!
END INTERFACE
!
END MODULE MODI_GREEN_FROM_LAI
!
!   ####################################################
    FUNCTION GREEN_FROM_LAI_0D(PLAI,PVEGTYPE,OAGRI_TO_GRASS) RESULT(PGREEN)
!   ####################################################
!!
!!    PURPOSE
!!    -------
!
!     Calculates coverage of soil by vegetation from leaf
!    area index and type of vegetation
!    (most of types; forest and vineyards; grassland)
!              
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
!!      
!!    AUTHOR
!!    ------
!!
!!      V. Masson and A. Boone          * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    25/03/99
!!
!!      R. Alkama    05/2012  : Add 7 new vegtype (19 rather than 12)
!!      B. Decharme  05/2013  new param for equatorial forest
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_DATA_COVER_PAR, ONLY : NVT_NO, NVT_ROCK, NVT_SNOW, NVT_PARK,        &
                                NVT_TEBD, NVT_BONE, NVT_TRBE, NVT_TRBD,      &
                                NVT_TEBE, NVT_TENE, NVT_BOBD, NVT_BOND,      &
                                NVT_SHRB, NVT_C3, NVT_C4, NVT_IRR,           &
                                NVT_GRAS, NVT_BOGR, NVT_TROG   
!
USE MODD_REPROD_OPER,    ONLY : XEVERG_VEG
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL,                 INTENT(IN) :: PLAI         ! Leaf area Index
REAL,   DIMENSION(:), INTENT(IN) :: PVEGTYPE     ! type of vegetation
LOGICAL,              INTENT(IN) :: OAGRI_TO_GRASS
!
REAL                             :: PGREEN       ! greeness fraction
!
!*      0.2    declarations of local variables
!
REAL :: ZLAI, ZAGRI
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-----------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('MODI_GREEN_FROM_LAI:GREEN_FROM_LAI_0D',0,ZHOOK_HANDLE)
!
ZLAI = PLAI
IF ( PVEGTYPE(NVT_NO  ) + PVEGTYPE(NVT_ROCK)< 1.) THEN
  ZLAI = PLAI / (1.-PVEGTYPE(NVT_NO)-PVEGTYPE(NVT_ROCK))
END IF
!
ZAGRI=(1. - EXP( -0.6 * ZLAI ))
IF(OAGRI_TO_GRASS)ZAGRI=MIN(ZAGRI,0.95)
!
PGREEN= ZAGRI                     *(PVEGTYPE(NVT_C4  ) +     &! C4 crops
                                    PVEGTYPE(NVT_IRR ) +   &! irrigated crops
                                    PVEGTYPE(NVT_C3  )  )  &! C3 crops
       + MIN(1. - EXP( -0.5 * ZLAI ),0.95)                 &
                                  *(PVEGTYPE(NVT_TRBD) +   &! tropical broadleaf deciduous
                                    PVEGTYPE(NVT_TEBE) +   &! temperate broadleaf evergreen
                                    PVEGTYPE(NVT_TEBD) +   &! temperate broadleaf cold-deciduous (summergreen)
                                    PVEGTYPE(NVT_TENE) +   &! temperate needleleaf evergreen
                                    PVEGTYPE(NVT_BOBD) +   &! boreal broadleaf cold-deciduous (summergreen)
                                    PVEGTYPE(NVT_BONE) +   &! boreal needleleaf evergreen
                                    PVEGTYPE(NVT_BONE) +   &! boreal needleleaf cold-deciduous (summergreen)
                                    PVEGTYPE(NVT_SHRB) )   &! shrub
       + XEVERG_VEG               * PVEGTYPE(NVT_TRBE)     &! tropical broadleaf evergreen
       + MIN(1. - EXP( -0.6 * ZLAI ),0.95)                 &
                                  *(PVEGTYPE(NVT_GRAS) +   &! grassland
                                    PVEGTYPE(NVT_BOGR) +   &! Boreal grassland
                                    PVEGTYPE(NVT_TROG) +   &! tropical grassland
                                    PVEGTYPE(NVT_PARK)  )  &! irr. parks
       + 0.                       * PVEGTYPE(NVT_NO  )     &! no vegetation (smooth)
       + 0.                       * PVEGTYPE(NVT_SNOW)     &! no vegetation (snow)
       + 0.                       * PVEGTYPE(NVT_ROCK)      ! no vegetation (rocks)  
!
IF (LHOOK) CALL DR_HOOK('MODI_GREEN_FROM_LAI:GREEN_FROM_LAI_0D',1,ZHOOK_HANDLE)
!-----------------------------------------------------------------
!
END FUNCTION GREEN_FROM_LAI_0D
!
!   ####################################################
    FUNCTION GREEN_FROM_LAI_1D(PLAI,PVEGTYPE,OAGRI_TO_GRASS) RESULT(PGREEN)
!   ####################################################
!!
!!    PURPOSE
!!    -------
!
!     Calculates coverage of soil by vegetation from leaf
!    area index and type of vegetation
!    (most of types; forest and vineyards; grassland)
!              
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
!!      
!!    AUTHOR
!!    ------
!!
!!      V. Masson and A. Boone          * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    25/03/99
!!      B. Decharme  05/2013  new param for equatorial forest
!!
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_DATA_COVER_PAR, ONLY : NVT_NO, NVT_ROCK, NVT_SNOW, NVT_PARK,        &
                                NVT_TEBD, NVT_BONE, NVT_TRBE, NVT_TRBD,      &
                                NVT_TEBE, NVT_TENE, NVT_BOBD, NVT_BOND,      &
                                NVT_SHRB, NVT_C3, NVT_C4, NVT_IRR,           &
                                NVT_GRAS, NVT_BOGR, NVT_TROG
!
USE MODD_REPROD_OPER,    ONLY : XEVERG_VEG
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL,   DIMENSION(:),   INTENT(IN) :: PLAI         ! Leaf area Index
REAL,   DIMENSION(:,:), INTENT(IN) :: PVEGTYPE     ! type of vegetation
LOGICAL,                INTENT(IN) :: OAGRI_TO_GRASS
!
REAL,   DIMENSION(SIZE(PLAI))      :: PGREEN       ! greeness fraction
!
!*      0.2    declarations of local variables
!
REAL,   DIMENSION(SIZE(PLAI))      :: ZLAI, ZAGRI
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-----------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('MODI_GREEN_FROM_LAI:GREEN_FROM_LAI_1D',0,ZHOOK_HANDLE)
!
ZLAI(:) = PLAI(:)
WHERE ( PVEGTYPE(:,NVT_NO  ) + PVEGTYPE(:,NVT_ROCK) + PVEGTYPE(:,NVT_SNOW) < 1.) 
  ZLAI(:) = PLAI(:) / (1.-PVEGTYPE(:,NVT_NO)-PVEGTYPE(:,NVT_ROCK)-PVEGTYPE(:,NVT_SNOW))
END WHERE
!
ZAGRI(:)=(1. - EXP( -0.6 * ZLAI(:) ))
IF(OAGRI_TO_GRASS)ZAGRI(:)=MIN(ZAGRI(:),0.95)
!
PGREEN(:)= ZAGRI(:)                     *(PVEGTYPE(:,NVT_C4  ) +   &! C4 crops
                                          PVEGTYPE(:,NVT_IRR ) +   &! irrigated crops
                                          PVEGTYPE(:,NVT_C3  )  )  &! C3 crops
          + MIN(1. - EXP( -0.5 * ZLAI(:) ),0.95)               &
                                    *(PVEGTYPE(:,NVT_TRBD) +   &! tropical broadleaf deciduous
                                      PVEGTYPE(:,NVT_TEBE) +   &! temperate broadleaf evergreen
                                      PVEGTYPE(:,NVT_TEBD) +   &! temperate broadleaf cold-deciduous (summergreen)
                                      PVEGTYPE(:,NVT_TENE) +   &! temperate needleleaf evergreen
                                      PVEGTYPE(:,NVT_BOBD) +   &! boreal broadleaf cold-deciduous (summergreen)
                                      PVEGTYPE(:,NVT_BONE) +   &! boreal needleleaf evergreen
                                      PVEGTYPE(:,NVT_BONE) +   &! boreal needleleaf cold-deciduous (summergreen)
                                      PVEGTYPE(:,NVT_SHRB) )   &! shrub
          + XEVERG_VEG              * PVEGTYPE(:,NVT_TRBE)     &! tropical broadleaf evergreen
          + MIN(1. - EXP( -0.6 * ZLAI(:) ),0.95)               &
                                    *(PVEGTYPE(:,NVT_GRAS) +   &! grassland
                                      PVEGTYPE(:,NVT_BOGR) +   &! Boreal grassland                                        
                                      PVEGTYPE(:,NVT_TROG) +   &! torp. grass
                                      PVEGTYPE(:,NVT_PARK)  )  &! irr. parks
          + 0.                      * PVEGTYPE(:,NVT_NO  )     &! no vegetation (smooth)
          + 0.                      * PVEGTYPE(:,NVT_SNOW)     &! no vegetation (snow)
          + 0.                      * PVEGTYPE(:,NVT_ROCK)      ! no vegetation (rocks)  
!
IF (LHOOK) CALL DR_HOOK('MODI_GREEN_FROM_LAI:GREEN_FROM_LAI_1D',1,ZHOOK_HANDLE)
!-----------------------------------------------------------------
!
END FUNCTION GREEN_FROM_LAI_1D
!
!   ####################################################
    FUNCTION GREEN_FROM_LAI_2D(PLAI,PVEGTYPE,OAGRI_TO_GRASS) RESULT(PGREEN)
!   ####################################################
!!
!!    PURPOSE
!!    -------
!
!     Calculates coverage of soil by vegetation from leaf
!    area index and type of vegetation
!    (most of types; forest and vineyards; grassland)
!              
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
!!      
!!    AUTHOR
!!    ------
!!
!!      V. Masson and A. Boone          * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    25/03/99
!!
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_DATA_COVER_PAR, ONLY : NVT_NO, NVT_ROCK, NVT_SNOW, NVT_PARK,        &
                                NVT_TEBD, NVT_BONE, NVT_TRBE, NVT_TRBD,      &
                                NVT_TEBE, NVT_TENE, NVT_BOBD, NVT_BOND,      &
                                NVT_SHRB, NVT_C3, NVT_C4, NVT_IRR,           &
                                NVT_GRAS, NVT_BOGR, NVT_TROG
USE MODD_SURF_PAR,       ONLY : XUNDEF
!
USE MODD_REPROD_OPER,    ONLY : XEVERG_VEG
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL,   DIMENSION(:,:),   INTENT(IN) :: PLAI         ! Leaf area Index
REAL,   DIMENSION(:,:,:), INTENT(IN) :: PVEGTYPE     ! type of vegetation
LOGICAL,                  INTENT(IN) :: OAGRI_TO_GRASS
!
REAL,   DIMENSION(SIZE(PLAI,1),SIZE(PLAI,2))::PGREEN ! greeness fraction
!
!*      0.2    declarations of local variables
!
REAL,   DIMENSION(SIZE(PLAI,1),SIZE(PLAI,2)) :: ZLAI, ZAGRI
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-----------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('MODI_GREEN_FROM_LAI:GREEN_FROM_LAI_2D',0,ZHOOK_HANDLE)
ZLAI(:,:) = PLAI(:,:)
WHERE ( PVEGTYPE(:,:,NVT_NO  ) + PVEGTYPE(:,:,NVT_ROCK) + PVEGTYPE(:,:,NVT_SNOW) < 1.) 
  ZLAI(:,:) = PLAI(:,:) / (1.-PVEGTYPE(:,:,NVT_NO)-PVEGTYPE(:,:,NVT_ROCK)-PVEGTYPE(:,:,NVT_SNOW))
END WHERE
!
PGREEN(:,:) = XUNDEF
ZAGRI (:,:) = XUNDEF
!
WHERE (PLAI(:,:) /= XUNDEF)
      ZAGRI(:,:)=(1. - EXP( -0.6 * ZLAI(:,:) ))
ENDWHERE
IF(OAGRI_TO_GRASS)ZAGRI(:,:)=MIN(ZAGRI(:,:),0.95)
!
WHERE (PLAI(:,:) /= XUNDEF)
PGREEN(:,:)= ZAGRI(:,:)                   *(PVEGTYPE(:,:,NVT_C4  ) +   &! C4 crops
                                            PVEGTYPE(:,:,NVT_IRR ) +   &! irrigated crops
                                            PVEGTYPE(:,:,NVT_C3  )  )  &! C3 crops
           + MIN((1. - EXP( -0.5 * ZLAI(:,:) )),0.95)            &
                                    *(PVEGTYPE(:,:,NVT_TRBD) +   &! tropical broadleaf deciduous
                                      PVEGTYPE(:,:,NVT_TEBE) +   &! temperate broadleaf evergreen
                                      PVEGTYPE(:,:,NVT_TEBD) +   &! temperate broadleaf cold-deciduous (summergreen)
                                      PVEGTYPE(:,:,NVT_TENE) +   &! temperate needleleaf evergreen
                                      PVEGTYPE(:,:,NVT_BOBD) +   &! boreal broadleaf cold-deciduous (summergreen)
                                      PVEGTYPE(:,:,NVT_BONE) +   &! boreal needleleaf evergreen
                                      PVEGTYPE(:,:,NVT_BONE) +   &! boreal needleleaf cold-deciduous (summergreen)
                                      PVEGTYPE(:,:,NVT_SHRB) )   &! shrub
          + XEVERG_VEG              * PVEGTYPE(:,:,NVT_TRBE)     &! tropical broadleaf evergreen
          + MIN((1. - EXP( -0.6 * ZLAI(:,:) )),0.95)                 &
                                    *(PVEGTYPE(:,:,NVT_GRAS) + &! grassland
                                      PVEGTYPE(:,:,NVT_BOGR) +   &! Boreal grassland  
                                      PVEGTYPE(:,:,NVT_TROG) + &! trop grassland
                                      PVEGTYPE(:,:,NVT_PARK)  )&! irr. parks
          + 0.                      * PVEGTYPE(:,:,NVT_NO  )   &! no vegetation (smooth)
          + 0.                      * PVEGTYPE(:,:,NVT_SNOW)   &! no vegetation (snow)
          + 0.                      * PVEGTYPE(:,:,NVT_ROCK)    ! no vegetation (rocks)  
END WHERE
IF (LHOOK) CALL DR_HOOK('MODI_GREEN_FROM_LAI:GREEN_FROM_LAI_2D',1,ZHOOK_HANDLE)
!
!-----------------------------------------------------------------
!
END FUNCTION GREEN_FROM_LAI_2D
!
!
!
!   ####################################################
    FUNCTION GREEN_FROM_LAI_PATCH_1D(PLAI,PVEGTYPE,OAGRI_TO_GRASS) RESULT(PGREEN)
!   ####################################################
!!
!!    PURPOSE
!!    -------
!
!     Calculates coverage of soil by vegetation from leaf
!    area index and type of vegetation for each vegetation patch
!    (most of types; forest and vineyards; grassland)
!              
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
!!      
!!    AUTHOR
!!    ------
!!
!!    F.Solmon/V.Masson
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    25/03/99
!!
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_DATA_COVER_PAR, ONLY : NVT_NO, NVT_ROCK, NVT_SNOW, NVT_PARK,        &
                                NVT_TEBD, NVT_BONE, NVT_TRBE, NVT_TRBD,      &
                                NVT_TEBE, NVT_TENE, NVT_BOBD, NVT_BOND,      &
                                NVT_SHRB, NVT_C3, NVT_C4, NVT_IRR,           &
                                NVT_GRAS, NVT_BOGR, NVT_TROG
!
USE MODD_SURF_PAR,       ONLY : XUNDEF
!
USE MODD_REPROD_OPER,    ONLY : XEVERG_VEG
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL,   DIMENSION(:), INTENT(IN) :: PLAI         ! Leaf area Index
REAL,   DIMENSION(:), INTENT(IN) :: PVEGTYPE     ! type of vegetation
LOGICAL,              INTENT(IN) :: OAGRI_TO_GRASS
!
REAL,   DIMENSION(SIZE(PLAI)) :: PGREEN ! greeness fraction
!
!*      0.2    declarations of local variables
!
REAL,   DIMENSION(SIZE(PLAI)) :: ZLAI
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-----------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('MODI_GREEN_FROM_LAI:GREEN_FROM_LAI_PATCH_1D',0,ZHOOK_HANDLE)
ZLAI(:) = PLAI(:)
PGREEN(:) = XUNDEF
!
IF(OAGRI_TO_GRASS)THEN
  IF (PVEGTYPE(NVT_C4  )>0.) PGREEN(NVT_C4  )=  MIN(1. - EXP( -0.6 * ZLAI(NVT_C4  ) ),0.95)
  IF (PVEGTYPE(NVT_IRR )>0.) PGREEN(NVT_IRR )=  MIN(1. - EXP( -0.6 * ZLAI(NVT_IRR ) ),0.95)
  IF (PVEGTYPE(NVT_C3  )>0.) PGREEN(NVT_C3  )=  MIN(1. - EXP( -0.6 * ZLAI(NVT_C3  ) ),0.95)       
ELSE
  IF (PVEGTYPE(NVT_C4  )>0.) PGREEN(NVT_C4  )=  1. - EXP( -0.6 * ZLAI(NVT_C4  ) )
  IF (PVEGTYPE(NVT_IRR )>0.) PGREEN(NVT_IRR )=  1. - EXP( -0.6 * ZLAI(NVT_IRR ) )
  IF (PVEGTYPE(NVT_C3  )>0.) PGREEN(NVT_C3  )=  1. - EXP( -0.6 * ZLAI(NVT_C3  ) )
ENDIF
!
IF (PVEGTYPE(NVT_TEBD)>0.) PGREEN(NVT_TEBD)=  MIN(1. - EXP( -0.5 * ZLAI(NVT_TEBD) ),0.95)
IF (PVEGTYPE(NVT_BONE)>0.) PGREEN(NVT_BONE)=  MIN(1. - EXP( -0.5 * ZLAI(NVT_BONE) ),0.95)
IF (PVEGTYPE(NVT_TRBD)>0.) PGREEN(NVT_TRBD)=  MIN(1. - EXP( -0.5 * ZLAI(NVT_TRBD) ),0.95)
IF (PVEGTYPE(NVT_TEBE)>0.) PGREEN(NVT_TEBE)=  MIN(1. - EXP( -0.5 * ZLAI(NVT_TEBE) ),0.95)
IF (PVEGTYPE(NVT_TENE)>0.) PGREEN(NVT_TENE)=  MIN(1. - EXP( -0.5 * ZLAI(NVT_TENE) ),0.95)
IF (PVEGTYPE(NVT_BOBD)>0.) PGREEN(NVT_BOBD)=  MIN(1. - EXP( -0.5 * ZLAI(NVT_BOBD) ),0.95)
IF (PVEGTYPE(NVT_BOND)>0.) PGREEN(NVT_BOND)=  MIN(1. - EXP( -0.5 * ZLAI(NVT_BOND) ),0.95)
IF (PVEGTYPE(NVT_SHRB)>0.) PGREEN(NVT_SHRB)=  MIN(1. - EXP( -0.5 * ZLAI(NVT_SHRB) ),0.95)

IF (PVEGTYPE(NVT_TRBE)>0.) PGREEN(NVT_TRBE)=  XEVERG_VEG
!
IF (PVEGTYPE(NVT_GRAS)>0.) PGREEN(NVT_GRAS)=  MIN(1. - EXP( -0.6 * ZLAI(NVT_GRAS) ),0.95)
IF (PVEGTYPE(NVT_BOGR)>0.) PGREEN(NVT_BOGR)=  MIN(1. - EXP( -0.6 * ZLAI(NVT_BOGR) ),0.95)
IF (PVEGTYPE(NVT_TROG)>0.) PGREEN(NVT_TROG)=  MIN(1. - EXP( -0.6 * ZLAI(NVT_TROG) ),0.95)
IF (PVEGTYPE(NVT_PARK)>0.) PGREEN(NVT_PARK)=  MIN(1. - EXP( -0.6 * ZLAI(NVT_PARK) ),0.95)
!
IF (PVEGTYPE(NVT_NO  )>0.) PGREEN(NVT_NO  )= 0.
IF (PVEGTYPE(NVT_SNOW)>0.) PGREEN(NVT_SNOW)= 0.
IF (PVEGTYPE(NVT_ROCK)>0.) PGREEN(NVT_ROCK)= 0.  
IF (LHOOK) CALL DR_HOOK('MODI_GREEN_FROM_LAI:GREEN_FROM_LAI_PATCH_1D',1,ZHOOK_HANDLE)

!
END FUNCTION GREEN_FROM_LAI_PATCH_1D
!
!--------------------------------------------
!
