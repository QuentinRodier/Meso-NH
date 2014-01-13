!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
      SUBROUTINE READ_COVER_n(HPROGRAM)
!     ################################
!
!!****  *READ_COVER_n* - routine to read a file for
!!                         physiographic data file of model _n 
!!
!!    PURPOSE
!!    -------
!!       The purpose of this routine is to initialise the 
!!       physiographic data file.
!!
!!
!!**  METHOD
!!    ------
!!      The data are read in the initial surface file :
!!        - 2D physiographic data fields
!!          
!!      It does not read the grid definition. This should have been
!!      read already.
!!
!!    EXTERNAL
!!    --------
!!      
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
!!      Original    01/2003
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_SURF_ATM_n,     ONLY : XSEA, XWATER, XNATURE, XTOWN, &
                                XCOVER, XZS, TTIME, LCOVER, NSIZE_FULL
!
USE MODD_DATA_COVER_PAR, ONLY : NBARE_SOIL, JPCOVER
!
USE MODI_READ_LCOVER
USE MODI_READ_SURF
USE MODI_CONVERT_COVER_FRAC
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! calling program
!
!*       0.2   Declarations of local variables
!              -------------------------------
!

INTEGER           :: IRESP          ! Error code after redding
! 
INTEGER           :: IVERSION       ! surface version
!
 CHARACTER(LEN=12) :: YRECFM         ! Name of the article to be read
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!
!*       2.     Physiographic data fields:
!               -------------------------
!
!*       2.1    Cover classes :
!               -------------
!
IF (LHOOK) CALL DR_HOOK('READ_COVER_N',0,ZHOOK_HANDLE)
!
YRECFM='VERSION'
 CALL READ_SURF(HPROGRAM,YRECFM,IVERSION,IRESP)
!
ALLOCATE(LCOVER(JPCOVER))
 CALL READ_LCOVER(HPROGRAM,LCOVER)
!
!
ALLOCATE(XCOVER(NSIZE_FULL,JPCOVER))
 CALL READ_SURF(HPROGRAM,'COVER',XCOVER(:,:),LCOVER,IRESP)
!
!*       2.1    Fractions :
!               ---------
!
ALLOCATE(XSEA   (NSIZE_FULL))
ALLOCATE(XNATURE(NSIZE_FULL))
ALLOCATE(XWATER (NSIZE_FULL))
ALLOCATE(XTOWN  (NSIZE_FULL))
!
IF (IVERSION>=7) THEN
  !
  CALL READ_SURF(HPROGRAM,'FRAC_SEA   ',XSEA,   IRESP)
  CALL READ_SURF(HPROGRAM,'FRAC_NATURE',XNATURE,IRESP)
  CALL READ_SURF(HPROGRAM,'FRAC_WATER ',XWATER, IRESP)
  CALL READ_SURF(HPROGRAM,'FRAC_TOWN  ',XTOWN,  IRESP)
  !
ELSE
  CALL CONVERT_COVER_FRAC(XCOVER,XSEA,XNATURE,XTOWN,XWATER)
ENDIF
!
!*       2.2    Orography :
!               ---------
!
!
ALLOCATE(XZS(NSIZE_FULL))
YRECFM='ZS'
 CALL READ_SURF(HPROGRAM,YRECFM,XZS(:),IRESP)
IF (LHOOK) CALL DR_HOOK('READ_COVER_N',1,ZHOOK_HANDLE)
!
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE READ_COVER_n
