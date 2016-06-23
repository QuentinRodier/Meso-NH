!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########################
      SUBROUTINE AVERAGE2_OROGRAPHY (USS)
!     #########################
!
!!**** *AVERAGE2_OROGRAPHY* computes the cover fractions
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
!!    V. Masson         Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    10/12/97
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
!
USE MODD_SURF_ATM_SSO_n, ONLY : SURF_ATM_SSO_t
!
USE MODD_PGDWORK,       ONLY : NSIZE, XSUMVAL, XSUMVAL2, LSSQO, XSSQO, NSSO
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
!
!*    0.2    Declaration of other local variables
!            ------------------------------------
!
!
TYPE(SURF_ATM_SSO_t), INTENT(INOUT) :: USS
!
INTEGER                  :: JL
REAL,    DIMENSION(NSSO) :: ZMAXX
REAL,    DIMENSION(NSSO) :: ZMAXY
LOGICAL, DIMENSION(NSSO) :: GSEGX
LOGICAL, DIMENSION(NSSO) :: GSEGY
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!----------------------------------------------------------------------------
!
!*    1.     Mean orography
!            --------------
!
IF (LHOOK) CALL DR_HOOK('AVERAGE2_OROGRAPHY',0,ZHOOK_HANDLE)
WHERE (NSIZE(:)/=0)
  USS%XAVG_ZS(:)=XSUMVAL(:)/NSIZE(:)
END WHERE
!
!-------------------------------------------------------------------------------
!
!*    2.     Standard deviation
!            ------------------
!
WHERE (NSIZE(:)/=0)
  USS%XSSO_STDEV(:)=SQRT( MAX(0.,XSUMVAL2(:)/NSIZE(:) - USS%XAVG_ZS(:)*USS%XAVG_ZS(:)) )
END WHERE
!
!-------------------------------------------------------------------------------
!
!*    3.     Silhouette orography
!            --------------------
!
DO JL=1,SIZE(USS%XSIL_ZS)
  IF (NSIZE(JL)==0) CYCLE
  ZMAXX(:) = MAXVAL(XSSQO(:,:,JL),DIM=2)
  GSEGX(:) = ANY   (LSSQO(:,:,JL),DIM=2)
  ZMAXY(:) = MAXVAL(XSSQO(:,:,JL),DIM=1)
  GSEGY(:) = ANY   (LSSQO(:,:,JL),DIM=1)
  USS%XSIL_ZS(JL) =0.5*(  SUM(ZMAXX(:),MASK=GSEGX(:)) / COUNT(GSEGX(:)) &
                      + SUM(ZMAXY(:),MASK=GSEGY(:)) / COUNT(GSEGY(:)) )  
  
END DO
IF (LHOOK) CALL DR_HOOK('AVERAGE2_OROGRAPHY',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE AVERAGE2_OROGRAPHY
