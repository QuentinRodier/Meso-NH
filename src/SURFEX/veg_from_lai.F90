!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!#######################
MODULE MODI_VEG_FROM_LAI
!#######################
!
INTERFACE VEG_FROM_LAI
!
    FUNCTION VEG_FROM_LAI_0D(PLAI,PVEGTYPE,OAGRI_TO_GRASS) RESULT(PVEG)
!
REAL,                 INTENT(IN) :: PLAI         ! Leaf area Index
REAL,   DIMENSION(:), INTENT(IN) :: PVEGTYPE     ! type of vegetation
LOGICAL,              INTENT(IN) :: OAGRI_TO_GRASS
!
REAL                             :: PVEG         ! vegetation fraction
!
END FUNCTION VEG_FROM_LAI_0D
!
!
    FUNCTION VEG_FROM_LAI_1D(PLAI,PVEGTYPE,OAGRI_TO_GRASS) RESULT(PVEG)
!
REAL,   DIMENSION(:),   INTENT(IN) :: PLAI         ! Leaf area Index
REAL,   DIMENSION(:,:), INTENT(IN) :: PVEGTYPE     ! type of vegetation
LOGICAL,                INTENT(IN) :: OAGRI_TO_GRASS
!
REAL,   DIMENSION(SIZE(PLAI))      :: PVEG         ! vegetation fraction
!
END FUNCTION VEG_FROM_LAI_1D
!
!
    FUNCTION VEG_FROM_LAI_2D(PLAI,PVEGTYPE,OAGRI_TO_GRASS) RESULT(PVEG)
!
REAL,   DIMENSION(:,:),   INTENT(IN) :: PLAI         ! Leaf area Index
REAL,   DIMENSION(:,:,:), INTENT(IN) :: PVEGTYPE     ! type of vegetation
LOGICAL,                  INTENT(IN) :: OAGRI_TO_GRASS
!
REAL,   DIMENSION(SIZE(PLAI,1),SIZE(PLAI,2)) :: PVEG ! vegetation fraction
!
END FUNCTION VEG_FROM_LAI_2D
!

    FUNCTION VEG_FROM_LAI_PATCH_1D(PLAI,PVEGTYPE,OAGRI_TO_GRASS) RESULT(PVEG)
!
REAL,   DIMENSION(:), INTENT(IN) :: PLAI         ! Leaf area Index for each vegtype
REAL,   DIMENSION(:), INTENT(IN) :: PVEGTYPE     ! 
LOGICAL,              INTENT(IN) :: OAGRI_TO_GRASS
!
REAL,   DIMENSION(SIZE(PLAI)) :: PVEG ! vegetation fraction
!
END FUNCTION VEG_FROM_LAI_PATCH_1D
!
END INTERFACE
!
END MODULE MODI_VEG_FROM_LAI
!
!   ####################################################
    FUNCTION VEG_FROM_LAI_0D(PLAI,PVEGTYPE,OAGRI_TO_GRASS) RESULT(PVEG)
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
!!      R. Alkama    05/2012 : extantion from 12 to 19 vegtypes
!!      B. Decharme  05/2013  new param for equatorial forest
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_DATA_COVER_PAR, ONLY :NVT_NO, NVT_ROCK, NVT_SNOW, NVT_TEBD,     & 
                                 NVT_BONE, NVT_TRBE, NVT_C3, NVT_C4,     &
                                 NVT_IRR, NVT_GRAS, NVT_TROG, NVT_PARK,  &
                                 NVT_TRBD, NVT_TEBE, NVT_TENE, NVT_BOBD, &
                                 NVT_BOND, NVT_BOGR, NVT_SHRB
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
REAL                             :: PVEG         ! vegetation fraction
!
!*      0.2    declarations of local variables
!
REAL :: ZLAI, ZAGRI
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-----------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('MODI_VEG_FROM_LAI:VEG_FROM_LAI_0D',0,ZHOOK_HANDLE)
ZLAI = PLAI
IF ( PVEGTYPE(NVT_NO  ) + PVEGTYPE(NVT_ROCK) + PVEGTYPE(NVT_SNOW)< 1.) THEN
  ZLAI = PLAI / (1.-PVEGTYPE(NVT_NO)-PVEGTYPE(NVT_ROCK)-PVEGTYPE(NVT_SNOW))
END IF
!
IF(OAGRI_TO_GRASS)THEN
  ZAGRI = 0.95
ELSE
  ZAGRI = (1. - EXP( -0.6 * ZLAI ))
ENDIF
!
PVEG = ZAGRI                      *(PVEGTYPE(NVT_C4  ) +   &! C4 crops
                                    PVEGTYPE(NVT_IRR ) +   &! irrigated crops
                                    PVEGTYPE(NVT_C3  )  )  &! C3 crops
       + 0.95                     *(PVEGTYPE(NVT_TEBD) +   &! TREE
                                    PVEGTYPE(NVT_TRBD) +   &! TREE
                                    PVEGTYPE(NVT_TEBE) +   &! TREE
                                    PVEGTYPE(NVT_BOBD) +   &! TREE
                                    PVEGTYPE(NVT_SHRB) +   &! TREE
                                    PVEGTYPE(NVT_BONE) +   &! CONI
                                    PVEGTYPE(NVT_TENE) +   &! CONI
                                    PVEGTYPE(NVT_BOND) )   &! CONI
       + XEVERG_VEG               * PVEGTYPE(NVT_TRBE)     &! EVER 
       + 0.95                     *(PVEGTYPE(NVT_GRAS) +   &! grassland C3
                                    PVEGTYPE(NVT_BOGR) +   &! boral grass C3
                                    PVEGTYPE(NVT_TROG) +   &! tropical grass C4
                                    PVEGTYPE(NVT_PARK)  )  &! irr. parks
       + 0.                       * PVEGTYPE(NVT_NO  )     &! no vegetation (smooth)
       + 0.                       * PVEGTYPE(NVT_SNOW)     &! no vegetation (snow)
       + 0.                       * PVEGTYPE(NVT_ROCK)      ! no vegetation (rocks)  
IF (LHOOK) CALL DR_HOOK('MODI_VEG_FROM_LAI:VEG_FROM_LAI_0D',1,ZHOOK_HANDLE)
!-----------------------------------------------------------------
!
END FUNCTION VEG_FROM_LAI_0D
!
!   ####################################################
    FUNCTION VEG_FROM_LAI_1D(PLAI,PVEGTYPE,OAGRI_TO_GRASS) RESULT(PVEG)
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
USE MODD_DATA_COVER_PAR, ONLY : NVT_NO, NVT_ROCK, NVT_SNOW, NVT_TEBD,    & 
                                 NVT_BONE, NVT_TRBE, NVT_C3, NVT_C4,     &
                                 NVT_IRR, NVT_GRAS, NVT_TROG, NVT_PARK,  &
                                 NVT_TRBD, NVT_TEBE, NVT_TENE, NVT_BOBD, &
                                 NVT_BOND, NVT_BOGR, NVT_SHRB 
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
REAL,   DIMENSION(SIZE(PLAI))      :: PVEG         ! vegetation fraction
!
!*      0.2    declarations of local variables
!
REAL,   DIMENSION(SIZE(PLAI))      :: ZLAI, ZAGRI
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-----------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('MODI_VEG_FROM_LAI:VEG_FROM_LAI_1D',0,ZHOOK_HANDLE)
ZLAI(:) = PLAI(:)
WHERE ( PVEGTYPE(:,NVT_NO  ) + PVEGTYPE(:,NVT_ROCK) + PVEGTYPE(:,NVT_SNOW) < 1.) 
  ZLAI(:) = PLAI(:) / (1.-PVEGTYPE(:,NVT_NO)-PVEGTYPE(:,NVT_ROCK)-PVEGTYPE(:,NVT_SNOW))
END WHERE
!
IF(OAGRI_TO_GRASS)THEN
  ZAGRI(:) = 0.95
ELSE
  ZAGRI(:) = (1. - EXP( -0.6 * ZLAI(:) ))
ENDIF
!
PVEG(:) = ZAGRI(:)                *(PVEGTYPE(:,NVT_C4  ) +   &! C4 crops
                                    PVEGTYPE(:,NVT_IRR ) +   &! irrigated crops
                                    PVEGTYPE(:,NVT_C3  )  )  &! C3 crops
       + 0.95                     *(PVEGTYPE(:,NVT_TEBD) +   &! TREE
                                    PVEGTYPE(:,NVT_TRBD) +   &! TREE
                                    PVEGTYPE(:,NVT_TEBE) +   &! TREE
                                    PVEGTYPE(:,NVT_BOBD) +   &! TREE
                                    PVEGTYPE(:,NVT_SHRB) +   &! TREE
                                    PVEGTYPE(:,NVT_BONE) +   &! CONI
                                    PVEGTYPE(:,NVT_TENE) +   &! CONI
                                    PVEGTYPE(:,NVT_BOND) )   &! CONI
       + XEVERG_VEG               * PVEGTYPE(:,NVT_TRBE)     &! EVER 
       + 0.95                     *(PVEGTYPE(:,NVT_GRAS) +   &! grassland C3
                                    PVEGTYPE(:,NVT_BOGR) +   &! boral grass C3
                                    PVEGTYPE(:,NVT_TROG) +   &! tropical grass C4
                                    PVEGTYPE(:,NVT_PARK)  )  &! irr. parks
       + 0.                       * PVEGTYPE(:,NVT_NO  )     &! no vegetation (smooth)
       + 0.                       * PVEGTYPE(:,NVT_SNOW)     &! no vegetation (snow)
       + 0.                       * PVEGTYPE(:,NVT_ROCK)      ! no vegetation (rocks)

IF (LHOOK) CALL DR_HOOK('MODI_VEG_FROM_LAI:VEG_FROM_LAI_1D',1,ZHOOK_HANDLE)
!
!-----------------------------------------------------------------
!
END FUNCTION VEG_FROM_LAI_1D
!
!   ####################################################
    FUNCTION VEG_FROM_LAI_2D(PLAI,PVEGTYPE,OAGRI_TO_GRASS) RESULT(PVEG)
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
USE MODD_DATA_COVER_PAR, ONLY : NVT_NO, NVT_ROCK, NVT_SNOW, NVT_TEBD,    & 
                                 NVT_BONE, NVT_TRBE, NVT_C3, NVT_C4,     &
                                 NVT_IRR, NVT_GRAS, NVT_TROG, NVT_PARK,  &
                                 NVT_TRBD, NVT_TEBE, NVT_TENE, NVT_BOBD, &
                                 NVT_BOND, NVT_BOGR, NVT_SHRB 
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
REAL,   DIMENSION(SIZE(PLAI,1),SIZE(PLAI,2)) :: PVEG ! vegetation fraction
!
!*      0.2    declarations of local variables
!
REAL,   DIMENSION(SIZE(PLAI,1),SIZE(PLAI,2)) :: ZLAI, ZAGRI
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-----------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('MODI_VEG_FROM_LAI:VEG_FROM_LAI_2D',0,ZHOOK_HANDLE)
ZLAI(:,:) = PLAI(:,:)
WHERE ( PVEGTYPE(:,:,NVT_NO  ) + PVEGTYPE(:,:,NVT_ROCK) + PVEGTYPE(:,:,NVT_SNOW) < 1.) 
  ZLAI(:,:) = PLAI(:,:) / (1.-PVEGTYPE(:,:,NVT_NO)-PVEGTYPE(:,:,NVT_ROCK)-PVEGTYPE(:,:,NVT_SNOW))
END WHERE
!
PVEG(:,:) = XUNDEF
!
IF(OAGRI_TO_GRASS)THEN
  ZAGRI(:,:) = 0.95
ELSE
  WHERE (PLAI(:,:) /= XUNDEF)
        ZAGRI(:,:) = (1. - EXP( -0.6 * ZLAI(:,:) ))
  ELSEWHERE
        ZAGRI(:,:) = XUNDEF
  ENDWHERE
ENDIF
!
WHERE (PLAI(:,:) /= XUNDEF)
PVEG(:,:) = ZAGRI(:,:)               *(PVEGTYPE(:,:,NVT_C4  ) +   &! C4 crops
                                       PVEGTYPE(:,:,NVT_IRR ) +   &! irrigated crops
                                       PVEGTYPE(:,:,NVT_C3  )  )  &! C3 crops
       + 0.95                        *(PVEGTYPE(:,:,NVT_TEBD) +   &! TREE
                                       PVEGTYPE(:,:,NVT_TRBD) +   &! TREE
                                       PVEGTYPE(:,:,NVT_TEBE) +   &! TREE
                                       PVEGTYPE(:,:,NVT_BOBD) +   &! TREE
                                       PVEGTYPE(:,:,NVT_SHRB) +   &! TREE
                                       PVEGTYPE(:,:,NVT_BONE) +   &! CONI
                                       PVEGTYPE(:,:,NVT_TENE) +   &! CONI
                                       PVEGTYPE(:,:,NVT_BOND) )   &! CONI
       + XEVERG_VEG                  * PVEGTYPE(:,:,NVT_TRBE)     &! EVER 
       + 0.95                        *(PVEGTYPE(:,:,NVT_GRAS) +   &! grassland C3
                                       PVEGTYPE(:,:,NVT_BOGR) +   &! boral grass C3
                                       PVEGTYPE(:,:,NVT_TROG) +   &! tropical grass C4
                                       PVEGTYPE(:,:,NVT_PARK)  )  &! irr. parks
       + 0.                          * PVEGTYPE(:,:,NVT_NO  )     &! no vegetation (smooth)
       + 0.                          * PVEGTYPE(:,:,NVT_SNOW)     &! no vegetation (snow)
       + 0.                          * PVEGTYPE(:,:,NVT_ROCK)      ! no vegetation (rocks)
END WHERE
IF (LHOOK) CALL DR_HOOK('MODI_VEG_FROM_LAI:VEG_FROM_LAI_2D',1,ZHOOK_HANDLE)
!
!-----------------------------------------------------------------
!
END FUNCTION VEG_FROM_LAI_2D
!
!
!
!   ####################################################
    FUNCTION VEG_FROM_LAI_PATCH_1D(PLAI,PVEGTYPE,OAGRI_TO_GRASS) RESULT(PVEG)
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
USE MODD_DATA_COVER_PAR, ONLY : NVT_NO, NVT_ROCK, NVT_SNOW, NVT_TEBD,    & 
                                 NVT_BONE, NVT_TRBE, NVT_C3, NVT_C4,     &
                                 NVT_IRR, NVT_GRAS, NVT_TROG, NVT_PARK,  &
                                 NVT_TRBD, NVT_TEBE, NVT_TENE, NVT_BOBD, &
                                 NVT_BOND, NVT_BOGR, NVT_SHRB 

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
REAL,   DIMENSION(SIZE(PLAI)) :: PVEG ! vegetation fraction
!
!*      0.2    declarations of local variables
!
REAL,   DIMENSION(SIZE(PLAI)) :: ZLAI
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-----------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('MODI_VEG_FROM_LAI:VEG_FROM_LAI_PATCH_1D',0,ZHOOK_HANDLE)
ZLAI(:) = PLAI(:)
PVEG(:) = XUNDEF
!
IF(OAGRI_TO_GRASS)THEN
  IF (PVEGTYPE(NVT_C4  )>0.) PVEG(NVT_C4  )= 0.95
  IF (PVEGTYPE(NVT_IRR )>0.) PVEG(NVT_IRR )= 0.95
  IF (PVEGTYPE(NVT_C3  )>0.) PVEG(NVT_C3  )= 0.95
ELSE
  IF (PVEGTYPE(NVT_C4  )>0.) PVEG(NVT_C4  )= 1. - EXP( -0.6 * ZLAI(NVT_C4  ) )
  IF (PVEGTYPE(NVT_IRR )>0.) PVEG(NVT_IRR )= 1. - EXP( -0.6 * ZLAI(NVT_IRR ) )
  IF (PVEGTYPE(NVT_C3  )>0.) PVEG(NVT_C3  )= 1. - EXP( -0.6 * ZLAI(NVT_C3  ) )
ENDIF
!
IF (PVEGTYPE(NVT_TEBD)>0.) PVEG(NVT_TEBD)=  0.95
IF (PVEGTYPE(NVT_TRBD)>0.) PVEG(NVT_TRBD)=  0.95
IF (PVEGTYPE(NVT_TEBE)>0.) PVEG(NVT_TEBE)=  0.95
IF (PVEGTYPE(NVT_BOBD)>0.) PVEG(NVT_BOBD)=  0.95
IF (PVEGTYPE(NVT_SHRB)>0.) PVEG(NVT_SHRB)=  0.95
IF (PVEGTYPE(NVT_BONE)>0.) PVEG(NVT_BONE)=  0.95
IF (PVEGTYPE(NVT_TENE)>0.) PVEG(NVT_TENE)=  0.95
IF (PVEGTYPE(NVT_BOND)>0.) PVEG(NVT_BOND)=  0.95
IF (PVEGTYPE(NVT_TRBE)>0.) PVEG(NVT_TRBE)=  XEVERG_VEG
!
IF (PVEGTYPE(NVT_GRAS)>0.) PVEG(NVT_GRAS)=  0.95
IF (PVEGTYPE(NVT_BOGR)>0.) PVEG(NVT_BOGR)=  0.95
IF (PVEGTYPE(NVT_TROG)>0.) PVEG(NVT_TROG)=  0.95
IF (PVEGTYPE(NVT_PARK)>0.) PVEG(NVT_PARK)=  0.95
!
IF (PVEGTYPE(NVT_NO  )>0.) PVEG(NVT_NO  )= 0.
IF (PVEGTYPE(NVT_SNOW)>0.) PVEG(NVT_SNOW)= 0.
IF (PVEGTYPE(NVT_ROCK)>0.) PVEG(NVT_ROCK)= 0.  
IF (LHOOK) CALL DR_HOOK('MODI_VEG_FROM_LAI:VEG_FROM_LAI_PATCH_1D',1,ZHOOK_HANDLE)
!
END FUNCTION VEG_FROM_LAI_PATCH_1D
!
!--------------------------------------------
!
