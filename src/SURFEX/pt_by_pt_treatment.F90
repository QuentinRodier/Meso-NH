!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
      SUBROUTINE PT_BY_PT_TREATMENT(KLUOUT,PLAT,PLON,PVALUE,HSUBROUTINE)
!     ###################################################################
!
!!**** *PT_BY_PT_TREATMENT* 
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
!!    V. Masson          Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    12/09/95
!!                27/03/96 (V. Masson) modify the arguments for the call of 
!!                         interpolation subroutine
!!                06/2009 (B. Decharme) call Topographic index statistics calculation
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODI_AVERAGE1_COVER
USE MODI_AVERAGE1_OROGRAPHY
USE MODI_AVERAGE1_CTI
USE MODI_AVERAGE1_LDB
USE MODI_AVERAGE1_MESH
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
INTEGER,           INTENT(IN) :: KLUOUT
REAL,DIMENSION(:), INTENT(IN) :: PLAT
REAL,DIMENSION(:), INTENT(IN) :: PLON
REAL,DIMENSION(:), INTENT(IN) :: PVALUE
 CHARACTER(LEN=6), INTENT(IN)  :: HSUBROUTINE   ! Name of the subroutine to call
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
!----------------------------------------------------------------------------
!
      IF (LHOOK) CALL DR_HOOK('PT_BY_PT_TREATMENT',0,ZHOOK_HANDLE)
      SELECT CASE (HSUBROUTINE)

      CASE ('A_COVR')
      CALL AVERAGE1_COVER(KLUOUT,PLAT,PLON,PVALUE)

      CASE ('A_OROG')
      CALL AVERAGE1_OROGRAPHY(KLUOUT,PLAT,PLON,PVALUE)

      CASE ('A_CTI ')
      CALL AVERAGE1_CTI(KLUOUT,PLAT,PLON,PVALUE)

      CASE ('A_LDBD')
      CALL AVERAGE1_LDB(KLUOUT,PLAT,PLON,PVALUE,'D')

      CASE ('A_LDBS')
      CALL AVERAGE1_LDB(KLUOUT,PLAT,PLON,PVALUE,'S')
      
      CASE ('A_MESH')
      CALL AVERAGE1_MESH(KLUOUT,PLAT,PLON,PVALUE)

      END SELECT
IF (LHOOK) CALL DR_HOOK('PT_BY_PT_TREATMENT',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE PT_BY_PT_TREATMENT
