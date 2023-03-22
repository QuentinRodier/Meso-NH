!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!   #############
FUNCTION PATCH_TO_LUT_FUNC(PPATCH,PFIELD,LFRAC) RESULT (ZFIELD_LUT)

!   ###############################################################
!!**   PATCH_TO_LUT_FUNC
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
!
!-------------------------------------------------------------------------------
!
! The following tables of parameters for ISBA 19s PFTs are in the following order :
!
! NVT_NO   = 1   ! 1  ! no vegetation (smooth)
! NVT_ROCK = 2   ! 2  ! no vegetation (rocks)
! NVT_SNOW = 3   ! 3  ! permanent snow and ice
! NVT_TEBD = 4   ! 4  ! temperate broadleaf cold-deciduous summergreen (TREE)
! NVT_BONE = 5   ! 5  ! boreal needleleaf evergreen  (CONI)
! NVT_TRBE = 6   ! 6  ! tropical broadleaf evergreen (EVER)
! NVT_C3   = 7   ! 7  ! C3 cultures types
! NVT_C4   = 8   ! 8  ! C4 cultures types
! NVT_IRR  = 9   ! 9  ! irrigated crops
! NVT_GRAS =10   !10  ! grassland
! NVT_TROG =11   !11  ! tropical grassland
! NVT_PARK =12   !12  ! peat bogs, parks and gardens (irrigated grass)
! NVT_TRBD =13   ! 4  ! tropical broadleaf deciduous (TREE)
! NVT_TEBE =14   ! 4  ! temperate broadleaf evergreen (TREE)
! NVT_TENE =15   ! 5  ! temperate needleleaf evergreen (CONI)
! NVT_BOBD =16   ! 4  ! boreal broadleaf cold-deciduous summergreen (TREE)
! NVT_BOND =17   ! 5  ! boreal needleleaf cold-deciduous summergreen (CONI)
! NVT_BOGR =18   !10  ! boreal grass (GRAS)
! NVT_SHRB =19   ! 4  ! shrub (TREE)
!  
!-------------------------------------------------------------------------------
!
! The following 4 surface cover types required :
!
! surface cover type 1 = Primary and secondary land (Forest, grasslands, and bare ground)
!
! surface cover type 2 = Cropland (Includes managed pastureland and rangeland)
!
! surface cover type 3 = Pastureland (not yet implemented in ISBA)
!
! surface cover type 4 = Urban settlement (not yet implemented in ISBA, should be implemented with TEB)
!
!-------------------------------------------------------------------------------
!
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_SURF_PAR,   ONLY : XUNDEF
USE MODD_XIOS,       ONLY : NLUT
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
REAL, DIMENSION(SIZE(PPATCH,1),NLUT) :: ZFIELD_LUT
REAL, DIMENSION(SIZE(PPATCH,1),NLUT) :: ZPATCH_LUT
!
!*       0.3 local variable
!
LOGICAL, DIMENSION(SIZE(PPATCH,1),SIZE(PPATCH,2)) :: GMASK
!
LOGICAL :: GFRAC
!
INTEGER :: JI, INI, JP
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('PATCH_TO_LUT_FUNC',0,ZHOOK_HANDLE)
!
!
!*       1 Initialisations
!
INI = SIZE(PPATCH,1)
!
GFRAC=.FALSE.
IF(PRESENT(LFRAC))GFRAC=LFRAC
!
ZFIELD_LUT(:,:) = 0.0
ZPATCH_LUT(:,:) = 0.0
!
GMASK(:,:) = (PPATCH(:,:)>0.0.OR.PFIELD(:,:)/=XUNDEF)
!
!------------------------------------------------------------------------
! 12 Patches case
!------------------------------------------------------------------------
!
IF (SIZE(PFIELD,2)==12) THEN
!
! Primary and secondary land
!
  DO JP = 1,2
     DO JI = 1,INI
        IF(GMASK(JI,JP))THEN
          ZFIELD_LUT(JI,1) = ZFIELD_LUT(JI,1) + PPATCH(JI,JP)*PFIELD(JI,JP) 
          ZPATCH_LUT(JI,1) = ZPATCH_LUT(JI,1) + PPATCH(JI,JP)
        ENDIF
     ENDDO
  ENDDO
  DO JP = 4,6
     DO JI = 1,INI
        IF(GMASK(JI,JP))THEN
          ZFIELD_LUT(JI,1) = ZFIELD_LUT(JI,1) + PPATCH(JI,JP)*PFIELD(JI,JP) 
          ZPATCH_LUT(JI,1) = ZPATCH_LUT(JI,1) + PPATCH(JI,JP)
        ENDIF
     ENDDO
  ENDDO
  DO JP = 10,12
     DO JI = 1,INI
        IF(GMASK(JI,JP))THEN
          ZFIELD_LUT(JI,1) = ZFIELD_LUT(JI,1) + PPATCH(JI,JP)*PFIELD(JI,JP) 
          ZPATCH_LUT(JI,1) = ZPATCH_LUT(JI,1) + PPATCH(JI,JP)
        ENDIF
     ENDDO
  ENDDO
!
! Cropland
!
  DO JP = 7,9
     DO JI = 1,INI
        IF(GMASK(JI,JP))THEN
          ZFIELD_LUT(JI,2) = ZFIELD_LUT(JI,2) + PPATCH(JI,JP)*PFIELD(JI,JP) 
          ZPATCH_LUT(JI,2) = ZPATCH_LUT(JI,2) + PPATCH(JI,JP)
        ENDIF
     ENDDO
  ENDDO
!
ENDIF
!
!------------------------------------------------------------------------
! 19 Patches case
!------------------------------------------------------------------------
!
IF (SIZE(PFIELD,2)==19) THEN
!
! Primary and secondary land
!
  DO JP = 1,2
     DO JI = 1,INI
        IF(GMASK(JI,JP))THEN
          ZFIELD_LUT(JI,1) = ZFIELD_LUT(JI,1) + PPATCH(JI,JP)*PFIELD(JI,JP) 
          ZPATCH_LUT(JI,1) = ZPATCH_LUT(JI,1) + PPATCH(JI,JP)
        ENDIF
     ENDDO
  ENDDO
  DO JP = 4,6
     DO JI = 1,INI
        IF(GMASK(JI,JP))THEN
          ZFIELD_LUT(JI,1) = ZFIELD_LUT(JI,1) + PPATCH(JI,JP)*PFIELD(JI,JP) 
          ZPATCH_LUT(JI,1) = ZPATCH_LUT(JI,1) + PPATCH(JI,JP)
        ENDIF
     ENDDO
  ENDDO
  DO JP = 10,19
     DO JI = 1,INI
        IF(GMASK(JI,JP))THEN
          ZFIELD_LUT(JI,1) = ZFIELD_LUT(JI,1) + PPATCH(JI,JP)*PFIELD(JI,JP) 
          ZPATCH_LUT(JI,1) = ZPATCH_LUT(JI,1) + PPATCH(JI,JP)
        ENDIF
     ENDDO
  ENDDO
!
! Cropland
!
  DO JP = 7,9
     DO JI = 1,INI
        IF(GMASK(JI,JP))THEN
          ZFIELD_LUT(JI,2) = ZFIELD_LUT(JI,2) + PPATCH(JI,JP)*PFIELD(JI,JP) 
          ZPATCH_LUT(JI,2) = ZPATCH_LUT(JI,2) + PPATCH(JI,JP)
        ENDIF
     ENDDO
  ENDDO
!
ENDIF
!
!------------------------------------------------------------------------
! Final computation
!------------------------------------------------------------------------
!
IF(.NOT.GFRAC)THEN
  WHERE(ZPATCH_LUT(:,:)>0.0)
        ZFIELD_LUT(:,:)=ZFIELD_LUT(:,:)/ZPATCH_LUT(:,:)
  ELSEWHERE
        ZFIELD_LUT(:,:)=XUNDEF
  ENDWHERE
ENDIF
!
!-----------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('PATCH_TO_LUT_FUNC',1,ZHOOK_HANDLE)

END FUNCTION PATCH_TO_LUT_FUNC
