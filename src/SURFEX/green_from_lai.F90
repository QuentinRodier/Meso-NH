!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!#######################
MODULE MODI_GREEN_FROM_LAI
!#######################
!
INTERFACE GREEN_FROM_LAI
!
    FUNCTION GREEN_FROM_LAI_0D(PLAI,PVEGTYPE) RESULT(PGREEN)
!
REAL,                 INTENT(IN) :: PLAI         ! Leaf area Index
REAL,   DIMENSION(:), INTENT(IN) :: PVEGTYPE     ! type of vegetation
!
REAL                             :: PGREEN       ! greeness fraction
!
END FUNCTION GREEN_FROM_LAI_0D
!
!
    FUNCTION GREEN_FROM_LAI_1D(PLAI,PVEGTYPE) RESULT(PGREEN)
!
REAL,   DIMENSION(:),   INTENT(IN) :: PLAI         ! Leaf area Index
REAL,   DIMENSION(:,:), INTENT(IN) :: PVEGTYPE     ! type of vegetation
!
REAL,   DIMENSION(SIZE(PLAI))      :: PGREEN       ! greeness fraction
!
END FUNCTION GREEN_FROM_LAI_1D
!
!
    FUNCTION GREEN_FROM_LAI_2D(PLAI,PVEGTYPE) RESULT(PGREEN)
!
REAL,   DIMENSION(:,:),   INTENT(IN) :: PLAI         ! Leaf area Index
REAL,   DIMENSION(:,:,:), INTENT(IN) :: PVEGTYPE     ! type of vegetation
!
REAL,   DIMENSION(SIZE(PLAI,1),SIZE(PLAI,2))::PGREEN ! greeness fraction
!
END FUNCTION GREEN_FROM_LAI_2D
!

    FUNCTION GREEN_FROM_LAI_PATCH_1D(PLAI,PVEGTYPE) RESULT(PGREEN)
!
REAL,   DIMENSION(:),   INTENT(IN) :: PLAI         ! Leaf area Index for each vegtype
REAL,   DIMENSION(:), INTENT(IN) :: PVEGTYPE     ! 
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
    FUNCTION GREEN_FROM_LAI_0D(PLAI,PVEGTYPE) RESULT(PGREEN)
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
!!	V. Masson and A. Boone          * Meteo-France *
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
USE MODD_DATA_COVER_PAR, ONLY : NVT_NO, NVT_ROCK, NVT_SNOW,   &
                                  NVT_C3, NVT_C4, NVT_IRR,      &
                                  NVT_CONI, NVT_TREE, NVT_EVER, &
                                  NVT_TROG, NVT_PARK, NVT_GRAS  
!
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
!
REAL                             :: PGREEN       ! greeness fraction
!
!*      0.2    declarations of local variables
!
REAL :: ZLAI
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-----------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('MODI_GREEN_FROM_LAI:GREEN_FROM_LAI_0D',0,ZHOOK_HANDLE)
ZLAI = PLAI
IF ( PVEGTYPE(NVT_NO  ) + PVEGTYPE(NVT_ROCK)< 1.) THEN
  ZLAI = PLAI / (1.-PVEGTYPE(NVT_NO)-PVEGTYPE(NVT_ROCK))
END IF
!
PGREEN=(1. - EXP( -0.6 * ZLAI ))*(PVEGTYPE(NVT_C4  ) +   &! C4 crops
                                    PVEGTYPE(NVT_IRR ) +   &! irrigated crops
                                    PVEGTYPE(NVT_C3  )  )  &! C3 crops
       + MIN(1. - EXP( -0.5 * ZLAI ),0.95)                &
                                  *(PVEGTYPE(NVT_TREE) +   &! brodleaf forest
                                    PVEGTYPE(NVT_CONI)  )  &! coniferous forest
       + 0.99                     * PVEGTYPE(NVT_EVER)     &! equatorial forest
       + MIN(1. - EXP( -0.6 * ZLAI ),0.95)                &
                                  *(PVEGTYPE(NVT_GRAS) +   &! grassland
                                    PVEGTYPE(NVT_TROG) +   &! tropical grassland
                                    PVEGTYPE(NVT_PARK)  )  &! irr. parks
       + 0.                       * PVEGTYPE(NVT_NO  )     &! no vegetation (smooth)
       + 0.                       * PVEGTYPE(NVT_SNOW)     &! no vegetation (snow)
       + 0.                       * PVEGTYPE(NVT_ROCK)      ! no vegetation (rocks)  
IF (LHOOK) CALL DR_HOOK('MODI_GREEN_FROM_LAI:GREEN_FROM_LAI_0D',1,ZHOOK_HANDLE)
!-----------------------------------------------------------------
!
END FUNCTION GREEN_FROM_LAI_0D
!
!   ####################################################
    FUNCTION GREEN_FROM_LAI_1D(PLAI,PVEGTYPE) RESULT(PGREEN)
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
!!	V. Masson and A. Boone          * Meteo-France *
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
USE MODD_DATA_COVER_PAR, ONLY : NVT_NO, NVT_ROCK, NVT_SNOW,   &
                                  NVT_C3, NVT_C4, NVT_IRR,      &
                                  NVT_CONI, NVT_TREE, NVT_EVER, &
                                  NVT_TROG, NVT_PARK, NVT_GRAS  
!
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
!
REAL,   DIMENSION(SIZE(PLAI))      :: PGREEN       ! greeness fraction
!
!*      0.2    declarations of local variables
!
REAL,   DIMENSION(SIZE(PLAI))      :: ZLAI
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-----------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('MODI_GREEN_FROM_LAI:GREEN_FROM_LAI_1D',0,ZHOOK_HANDLE)
ZLAI(:) = PLAI(:)
WHERE ( PVEGTYPE(:,NVT_NO  ) + PVEGTYPE(:,NVT_ROCK) + PVEGTYPE(:,NVT_SNOW) < 1.) 
  ZLAI(:) = PLAI(:) / (1.-PVEGTYPE(:,NVT_NO)-PVEGTYPE(:,NVT_ROCK)-PVEGTYPE(:,NVT_SNOW))
END WHERE
!
PGREEN(:)=(1. - EXP( -0.6 * ZLAI(:) ))*(PVEGTYPE(:,NVT_C4  ) +   &! C4 crops
                                          PVEGTYPE(:,NVT_IRR ) +   &! irrigated crops
                                          PVEGTYPE(:,NVT_C3  )  )  &! C3 crops
          + MIN(1. - EXP( -0.5 * ZLAI(:) ),0.95)                  &
                                        *(PVEGTYPE(:,NVT_TREE) +   &! broadleaf forest
                                          PVEGTYPE(:,NVT_CONI)  )  &! coniferous forest
          + 0.99                        * PVEGTYPE(:,NVT_EVER)     &! equatorial forest
          + MIN(1. - EXP( -0.6 * ZLAI(:) ),0.95)                  &
                                        *(PVEGTYPE(:,NVT_GRAS) +   &! grassland
                                          PVEGTYPE(:,NVT_TROG) +   &! torp. grass
                                          PVEGTYPE(:,NVT_PARK)  )  &! irr. parks
          + 0.                          * PVEGTYPE(:,NVT_NO  )     &! no vegetation (smooth)
          + 0.                          * PVEGTYPE(:,NVT_SNOW)     &! no vegetation (snow)
          + 0.                          * PVEGTYPE(:,NVT_ROCK)      ! no vegetation (rocks)  
IF (LHOOK) CALL DR_HOOK('MODI_GREEN_FROM_LAI:GREEN_FROM_LAI_1D',1,ZHOOK_HANDLE)
!
!-----------------------------------------------------------------
!
END FUNCTION GREEN_FROM_LAI_1D
!
!   ####################################################
    FUNCTION GREEN_FROM_LAI_2D(PLAI,PVEGTYPE) RESULT(PGREEN)
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
!!	V. Masson and A. Boone          * Meteo-France *
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
USE MODD_DATA_COVER_PAR, ONLY : NVT_NO, NVT_ROCK, NVT_SNOW,   &
                                  NVT_C3, NVT_C4, NVT_IRR,      &
                                  NVT_CONI, NVT_TREE, NVT_EVER, &
                                  NVT_TROG, NVT_PARK, NVT_GRAS  
USE MODD_SURF_PAR,       ONLY : XUNDEF
!
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
!
REAL,   DIMENSION(SIZE(PLAI,1),SIZE(PLAI,2))::PGREEN ! greeness fraction
!
!*      0.2    declarations of local variables
!
REAL,   DIMENSION(SIZE(PLAI,1),SIZE(PLAI,2)) :: ZLAI
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-----------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('MODI_GREEN_FROM_LAI:GREEN_FROM_LAI_2D',0,ZHOOK_HANDLE)
ZLAI(:,:) = PLAI(:,:)
WHERE ( PVEGTYPE(:,:,NVT_NO  ) + PVEGTYPE(:,:,NVT_ROCK) + PVEGTYPE(:,:,NVT_SNOW) < 1.) 
  ZLAI(:,:) = PLAI(:,:) / (1.-PVEGTYPE(:,:,NVT_NO)-PVEGTYPE(:,:,NVT_ROCK)-PVEGTYPE(:,:,NVT_SNOW))
END WHERE
!
PGREEN(:,:) = XUNDEF
!
WHERE (PLAI(:,:) /= XUNDEF)
PGREEN(:,:)=(1. - EXP( -0.6 * ZLAI(:,:) ))*(PVEGTYPE(:,:,NVT_C4  ) +   &! C4 crops
                                              PVEGTYPE(:,:,NVT_IRR ) +   &! irrigated crops
                                              PVEGTYPE(:,:,NVT_C3  )  )  &! C3 crops
            + MIN((1. - EXP( -0.5 * ZLAI(:,:) )),0.95)                  &
                                            *(PVEGTYPE(:,:,NVT_TREE) +   &! broadleaf forest
                                              PVEGTYPE(:,:,NVT_CONI)  )  &! coniferous forest
            + 0.99                          * PVEGTYPE(:,:,NVT_EVER)     &! equatorial forest
            + MIN((1. - EXP( -0.6 * ZLAI(:,:) )),0.95)                  &
                                            *(PVEGTYPE(:,:,NVT_GRAS) +   &! grassland
                                              PVEGTYPE(:,:,NVT_TROG) +   &! trop grassland
                                              PVEGTYPE(:,:,NVT_PARK)  )  &! irr. parks
            + 0.                            * PVEGTYPE(:,:,NVT_NO  )     &! no vegetation (smooth)
            + 0.                            * PVEGTYPE(:,:,NVT_SNOW)     &! no vegetation (snow)
            + 0.                            * PVEGTYPE(:,:,NVT_ROCK)      ! no vegetation (rocks)  
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
    FUNCTION GREEN_FROM_LAI_PATCH_1D(PLAI,PVEGTYPE) RESULT(PGREEN)
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
USE MODD_DATA_COVER_PAR, ONLY : NVT_NO, NVT_ROCK, NVT_SNOW,   &
                                  NVT_C3, NVT_C4, NVT_IRR,      &
                                  NVT_CONI, NVT_TREE, NVT_EVER, &
                                  NVT_TROG, NVT_PARK, NVT_GRAS  
USE MODD_SURF_PAR,       ONLY : XUNDEF
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
REAL,   DIMENSION(:),   INTENT(IN) :: PLAI         ! Leaf area Index
REAL,   DIMENSION(:), INTENT(IN) :: PVEGTYPE     ! type of vegetation
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
IF (PVEGTYPE(NVT_C4  )>0.) PGREEN(NVT_C4  )=  1. - EXP( -0.6 * ZLAI(NVT_C4  ) )
IF (PVEGTYPE(NVT_IRR )>0.) PGREEN(NVT_IRR )=  1. - EXP( -0.6 * ZLAI(NVT_IRR ) )
IF (PVEGTYPE(NVT_C3  )>0.) PGREEN(NVT_C3  )=  1. - EXP( -0.6 * ZLAI(NVT_C3  ) )
!
IF (PVEGTYPE(NVT_TREE)>0.) PGREEN(NVT_TREE)=  MIN(1. - EXP( -0.5 * ZLAI(NVT_TREE) ),0.95)
IF (PVEGTYPE(NVT_CONI)>0.) PGREEN(NVT_CONI)=  MIN(1. - EXP( -0.5 * ZLAI(NVT_CONI) ),0.95)
IF (PVEGTYPE(NVT_EVER)>0.) PGREEN(NVT_EVER)=  0.99
!
IF (PVEGTYPE(NVT_GRAS)>0.) PGREEN(NVT_GRAS)=  MIN(1. - EXP( -0.6 * ZLAI(NVT_GRAS) ),0.95)
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
