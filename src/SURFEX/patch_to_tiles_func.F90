!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!   #############
FUNCTION PATCH_TO_TILE_FUNC(PPATCH,PFIELD,LFRAC) RESULT (ZFIELD_TILE)

!   ###############################################################
!!**   PATCH_TO_TILE_FUNC
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!    
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!      
!!
!!
!!    REFERENCE
!!    ---------
!!
!!      
!!    AUTHOR
!!    ------
!!    R. Séférian 08/2015
!!
!!    MODIFICATIONS
!!    -------------
!!
!!    R. Séférian 01/2016 adaptation for C4MIP tiles
!
!-------------------------------------------------------------------------------
!
!! The following tables of parameters for ISBA 19s PFTs
!! are in the following order :
!!NVT_NO   = 1   ! 1  ! no vegetation (smooth)
!!NVT_ROCK = 2   ! 2  ! no vegetation (rocks)
!!NVT_SNOW = 3   ! 3  ! permanent snow and ice
!!NVT_TEBD = 4   ! 4  ! temperate broadleaf cold-deciduous summergreen (TREE)
!!NVT_BONE = 5   ! 5  ! boreal needleleaf evergreen  (CONI)
!!NVT_TRBE = 6   ! 6  ! tropical broadleaf evergreen (EVER)
!!NVT_C3   = 7   ! 7  ! C3 cultures types
!!NVT_C4   = 8   ! 8  ! C4 cultures types
!!NVT_IRR  = 9   ! 9  ! irrigated crops
!!NVT_GRAS =10   !10  ! grassland
!!NVT_TROG =11   !11  ! tropical grassland
!!NVT_PARK =12   !12  ! peat bogs, parks and gardens (irrigated grass)
!!NVT_TRBD =13   ! 4  ! tropical broadleaf deciduous (TREE)
!!NVT_TEBE =14   ! 4  ! temperate broadleaf evergreen (TREE)
!!NVT_TENE =15   ! 5  ! temperate needleleaf evergreen (CONI)
!!NVT_BOBD =16   ! 4  ! boreal broadleaf cold-deciduous summergreen (TREE)
!!NVT_BOND =17   ! 5  ! boreal needleleaf cold-deciduous summergreen (CONI)
!!NVT_BOGR =18   !10  ! boreal grass (GRAS)
!!NVT_SHRB =19   ! 4  ! shrub (TREE)
!  
! landusetype4 = psl,crp,pst,urb
!-
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_SURF_PAR,   ONLY : XUNDEF
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1 input
!
REAL, DIMENSION(:,:), INTENT(IN) :: PPATCH
REAL, DIMENSION(:,:), INTENT(IN) :: PFIELD
!
LOGICAL, OPTIONAL,    INTENT(IN) :: LFRAC
!
!*       0.2 output
!
REAL, DIMENSION(SIZE(PPATCH,1),7) :: ZFIELD_TILE
REAL, DIMENSION(SIZE(PPATCH,1),7) :: ZPATCH_TILE
!
!*       0.3 local variable
!
LOGICAL, DIMENSION(SIZE(PPATCH,1),SIZE(PPATCH,2)) :: GMASK
!
LOGICAL :: GFRAC
!
INTEGER :: JI, INI, JP, INP
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('PATCH_TO_TILE_FUNC',0,ZHOOK_HANDLE)
!
! tree, shrub, grass, crop, past, bare-soil, Ice
!
!*       1 Initialisations
!
INI = SIZE(PPATCH,1)
INP = SIZE(PPATCH,2)
!
GFRAC=.FALSE.
IF(PRESENT(LFRAC))GFRAC=LFRAC
!
ZFIELD_TILE(:,:) = 0.0 
ZPATCH_TILE(:,:) = 0.0
!
GMASK(:,:) = (PPATCH(:,:)>0.0.OR.PFIELD(:,:)/=XUNDEF)
!
!------------------------------------------------------------------------
! 12 Patches
!------------------------------------------------------------------------
!
IF (INP==12) THEN
!
! Tree
!
  DO JP = 4,6
     DO JI = 1,INI
        IF(GMASK(JI,JP))THEN
          ZFIELD_TILE(JI,1) = ZFIELD_TILE(JI,1) + PPATCH(JI,JP)*PFIELD(JI,JP) 
          ZPATCH_TILE(JI,1) = ZPATCH_TILE(JI,1) + PPATCH(JI,JP)
        ENDIF
     ENDDO
  ENDDO
!
! Shrub (no Shrub with 12 patches)
!
  ZFIELD_TILE (:,2) = 0.0
  ZPATCH_TILE (:,2) = 0.0
!
! Grass
!
  DO JP = 10,12
     DO JI = 1,INI
        IF(GMASK(JI,JP))THEN
          ZFIELD_TILE(JI,3) = ZFIELD_TILE(JI,3) + PPATCH(JI,JP)*PFIELD(JI,JP) 
          ZPATCH_TILE(JI,3) = ZPATCH_TILE(JI,3) + PPATCH(JI,JP)
        ENDIF
     ENDDO
  ENDDO
!
! Crop
!
  DO JP = 7,9
     DO JI = 1,INI
        IF(GMASK(JI,JP))THEN
          ZFIELD_TILE(JI,4) = ZFIELD_TILE(JI,4) + PPATCH(JI,JP)*PFIELD(JI,JP) 
          ZPATCH_TILE(JI,4) = ZPATCH_TILE(JI,4) + PPATCH(JI,JP)
        ENDIF
     ENDDO
  ENDDO
!
! Pasture (not yet implemented in ISBA)
!
  ZFIELD_TILE (:,5) = 0.0 
  ZFIELD_TILE (:,5) = 0.0
!
! no-vegetated 
!
  DO JP = 1,2
     DO JI = 1,INI
        IF(GMASK(JI,JP))THEN
          ZFIELD_TILE(JI,6) = ZFIELD_TILE(JI,6) + PPATCH(JI,JP)*PFIELD(JI,JP) 
          ZPATCH_TILE(JI,6) = ZPATCH_TILE(JI,6) + PPATCH(JI,JP)
        ENDIF
     ENDDO
  ENDDO
!
! permanent snow and ice
!
  WHERE(GMASK(:, 3))
    ZFIELD_TILE(:,7) = PPATCH(:,3)*PFIELD(:,3)
    ZPATCH_TILE(:,7) = PPATCH(:,3)
  ENDWHERE
!
ENDIF
!
!------------------------------------------------------------------------
! 19 Patches
!------------------------------------------------------------------------
!
IF (INP==19) THEN
!
! Tree
!
  DO JP = 4,6
     DO JI = 1,INI
        IF(GMASK(JI,JP))THEN
          ZFIELD_TILE(JI,1) = ZFIELD_TILE(JI,1) + PPATCH(JI,JP)*PFIELD(JI,JP) 
          ZPATCH_TILE(JI,1) = ZPATCH_TILE(JI,1) + PPATCH(JI,JP)
        ENDIF
     ENDDO
  ENDDO
  DO JP = 13,18
     DO JI = 1,INI
        IF(GMASK(JI,JP))THEN
          ZFIELD_TILE(JI,1) = ZFIELD_TILE(JI,1) + PPATCH(JI,JP)*PFIELD(JI,JP) 
          ZPATCH_TILE(JI,1) = ZPATCH_TILE(JI,1) + PPATCH(JI,JP)
        ENDIF
     ENDDO
  ENDDO
!
! Shrub
!
  WHERE(GMASK(:,19))
    ZFIELD_TILE (:,2) = PPATCH(:,19)*PFIELD(:,19)
    ZPATCH_TILE (:,2) = PPATCH(:,19)
  ENDWHERE
!
! Grass
!
  DO JP = 10,12
     DO JI = 1,INI
        IF(GMASK(JI,JP))THEN
          ZFIELD_TILE(JI,3) = ZFIELD_TILE(JI,3) + PPATCH(JI,JP)*PFIELD(JI,JP) 
          ZPATCH_TILE(JI,3) = ZPATCH_TILE(JI,3) + PPATCH(JI,JP)
        ENDIF
     ENDDO
  ENDDO
!
! Crop
!
  DO JP = 7,9
     DO JI = 1,INI
        IF(GMASK(JI,JP))THEN
          ZFIELD_TILE(JI,4) = ZFIELD_TILE(JI,4) + PPATCH(JI,JP)*PFIELD(JI,JP) 
          ZPATCH_TILE(JI,4) = ZPATCH_TILE(JI,4) + PPATCH(JI,JP)
        ENDIF
     ENDDO
  ENDDO
!
! Pasture (not yet implemented in ISBA)
!
  ZFIELD_TILE (:,5) = 0.0
  ZFIELD_TILE (:,5) = 0.0
!
! no-vegetated 
!
  DO JP = 1,2
     DO JI = 1,INI
        IF(GMASK(JI,JP))THEN
          ZFIELD_TILE(JI,6) = ZFIELD_TILE(JI,6) + PPATCH(JI,JP)*PFIELD(JI,JP) 
          ZPATCH_TILE(JI,6) = ZPATCH_TILE(JI,6) + PPATCH(JI,JP)
        ENDIF
     ENDDO
  ENDDO
!
! permanent snow and ice
!
  WHERE(GMASK(:,3))
    ZFIELD_TILE(:,7) = PPATCH(:,3)*PFIELD(:, 3)
    ZPATCH_TILE(:,7) = PPATCH(:,3)
  ENDWHERE
!
ENDIF
!
!------------------------------------------------------------------------
! Final computation
!------------------------------------------------------------------------
!
IF(.NOT.GFRAC)THEN
  WHERE(ZPATCH_TILE(:,:)>0.0)
        ZFIELD_TILE(:,:)=ZFIELD_TILE(:,:)/ZPATCH_TILE(:,:)
  ELSEWHERE
        ZFIELD_TILE(:,:)=XUNDEF
  ENDWHERE
ENDIF
!
!-----------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('PATCH_TO_TILE_FUNC',1,ZHOOK_HANDLE)

END FUNCTION PATCH_TO_TILE_FUNC
