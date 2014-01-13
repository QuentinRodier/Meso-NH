!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########################
      SUBROUTINE AVERAGE2_COVER
!     #########################
!
!!**** *AVERAGE2_COVER* computes the cover fractions
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
USE MODD_PGDWORK,   ONLY : NSIZE, XSUMCOVER
USE MODD_SURF_ATM_n, ONLY : XCOVER
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
REAL, DIMENSION(:), ALLOCATABLE :: ZUNITY
!
INTEGER :: JCOVER ! loop counter on cover classes
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!----------------------------------------------------------------------------
!
!*    1.     Average values
!            --------------
!
IF (LHOOK) CALL DR_HOOK('AVERAGE2_COVER',0,ZHOOK_HANDLE)
ALLOCATE(ZUNITY(SIZE(NSIZE)))
ZUNITY (:) = 0.
!
DO JCOVER=1,SIZE(XCOVER,2)
  WHERE (NSIZE(:)/=0)
    XCOVER(:,JCOVER)=XSUMCOVER(:,JCOVER) /NSIZE(:)
    ZUNITY(:)=ZUNITY(:) + XCOVER(:,JCOVER)
  ENDWHERE
END DO
!
DO JCOVER=1,SIZE(XCOVER,2)
  WHERE (NSIZE(:) /=0 )
    XCOVER(:,JCOVER)=XCOVER(:,JCOVER) / ZUNITY(:)
  END WHERE
END DO
!
!-------------------------------------------------------------------------------
DEALLOCATE(ZUNITY)
IF (LHOOK) CALL DR_HOOK('AVERAGE2_COVER',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE AVERAGE2_COVER
