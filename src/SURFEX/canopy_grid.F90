!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########################################
      SUBROUTINE CANOPY_GRID(KI,KLVL,PZ,PZF,PDZ,PDZF)
!     #########################################
!
!!****  *CANOPY_GRID* - computation of vertical grid coordinatesa at 
!!                      half levels and grid depths at half and full
!!                      levels
!!                        
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!
!
!
!  --------------------------------- PZ(k+1)                     PDZ(k+1)
!                                                                           ^
!                                                                           |
!                                                                           |
!  - - - - - - - - - - - - - - - - - PZf(k+1)                               | PDZf(k+1)
!                                                              ^            |
!                                                              |            |
!  --------------------------------- PZ(k), XU, XT, XQ, XTKE   | PDZ(k)     V
!                                                              |            ^
!  - - - - - - - - - - - - - - - - - PZf(k)                    V            | PDZf(k)
!  --------------------------------- PZ(k-1)                     PDZ(k-1)   V
!  - - - - - - - - - - - - - - - - - PZf(k-1)
!

!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!	V. Masson   *Meteo France*	
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    07/2006 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
INTEGER,                  INTENT(IN)    :: KI     ! number of horizontal points
INTEGER,                  INTENT(IN)    :: KLVL   ! number of levels in canopy
REAL, DIMENSION(KI,KLVL), INTENT(IN)    :: PZ     ! heights of canopy levels              (m)
REAL, DIMENSION(KI,KLVL), INTENT(OUT)   :: PZF    ! heights of surface between canopy lev.(m)
REAL, DIMENSION(KI,KLVL), INTENT(OUT)   :: PDZF   ! depth between 2 full canopy levels    (m)
!                                                 ! PDZF is located at half levels
REAL, DIMENSION(KI,KLVL), INTENT(OUT)   :: PDZ    ! depth between 2 half canopy levels    (m)
!                                                 ! PDZ is located at full levels
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER :: JLAYER                 ! loop counter on layers
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!*    1. Geometric computations
!        ----------------------
!
!
!*    1.1 layer depths (variable located at half levels below full levels)
!         ------------
!
IF (LHOOK) CALL DR_HOOK('CANOPY_GRID',0,ZHOOK_HANDLE)
PDZF(:,:) = -999.
PDZF(:,1) = 2.*PZ(:,1)
DO JLAYER=2,KLVL
  PDZF(:,JLAYER) = PZ(:,JLAYER) - PZ(:,JLAYER-1)
END DO
!
!*    1.2 Layer heights (variable located at half levels below full levels)
!         -------------
!
PZF(:,:) = -999.
PZF(:,1) = 0.
DO JLAYER=2,KLVL
  PZF(:,JLAYER) = 2.*PZ(:,JLAYER-1) - PZF(:,JLAYER-1)
END DO
!
!
!*    1.3 layer depths (variable located at full levels)
!         ------------
!
PDZ(:,:) = -999.
DO JLAYER=1,KLVL-1
  PDZ(:,JLAYER) = PZF(:,JLAYER+1) - PZF(:,JLAYER)
END DO
PDZ(:,KLVL) = 2.*(PZ(:,KLVL)-PZF(:,KLVL))
IF (LHOOK) CALL DR_HOOK('CANOPY_GRID',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
END SUBROUTINE CANOPY_GRID
