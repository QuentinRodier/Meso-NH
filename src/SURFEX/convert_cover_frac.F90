!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
      SUBROUTINE CONVERT_COVER_FRAC   (PCOVER,                     &
                                         PSEA,PNATURE,PTOWN,PWATER   )  
!     ##############################################################
!
!!**** *CONVERT_COVER* convert surface cover classes into secondary
!!                     physiographic variables
!!
!!    PURPOSE
!!    -------
!!
!!    METHOD
!!    ------
!!   
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!
!!    V. Masson        Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    01/2004
!     
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_SURF_PAR,   ONLY : XUNDEF
USE MODD_DATA_COVER_n,ONLY : XDATA_NATURE, XDATA_TOWN, XDATA_SEA, XDATA_WATER
!
USE MODI_AV_PGD
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
REAL, DIMENSION(:,:), INTENT(IN)    :: PCOVER
REAL, DIMENSION(:),   INTENT(OUT)   :: PSEA
REAL, DIMENSION(:),   INTENT(OUT)   :: PNATURE
REAL, DIMENSION(:),   INTENT(OUT)   :: PTOWN
REAL, DIMENSION(:),   INTENT(OUT)   :: PWATER
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!
!-------------------------------------------------------------------------------
!
!*    1.      cover main type fractions
!             -------------------------
!
IF (LHOOK) CALL DR_HOOK('CONVERT_COVER_FRAC',0,ZHOOK_HANDLE)
 CALL AV_PGD (PSEA    ,PCOVER(:,:),XDATA_SEA    (:),'ALL','ARI')
 CALL AV_PGD (PTOWN   ,PCOVER(:,:),XDATA_TOWN   (:),'ALL','ARI')
 CALL AV_PGD (PNATURE ,PCOVER(:,:),XDATA_NATURE (:),'ALL','ARI')
 CALL AV_PGD (PWATER  ,PCOVER(:,:),XDATA_WATER  (:),'ALL','ARI')

!
WHERE (PSEA   (:) == XUNDEF) PSEA   (:) = 0.
WHERE (PNATURE(:) == XUNDEF) PNATURE(:) = 0.
WHERE (PTOWN  (:) == XUNDEF) PTOWN  (:) = 0.
WHERE (PWATER (:) == XUNDEF) PWATER (:) = 0.
IF (LHOOK) CALL DR_HOOK('CONVERT_COVER_FRAC',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE CONVERT_COVER_FRAC
