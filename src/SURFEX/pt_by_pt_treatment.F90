!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
      SUBROUTINE PT_BY_PT_TREATMENT (USS, &
                                     KLUOUT,PLAT,PLON,PVALUE,HSUBROUTINE,KNBLINES,PNODATA)
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
!
!
USE MODD_SURF_ATM_SSO_n, ONLY : SURF_ATM_SSO_t
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
!
TYPE(SURF_ATM_SSO_t), INTENT(INOUT) :: USS
!
INTEGER,           INTENT(IN) :: KLUOUT
REAL,DIMENSION(:), INTENT(IN) :: PLAT
REAL,DIMENSION(:), INTENT(IN) :: PLON
REAL,DIMENSION(:), INTENT(IN) :: PVALUE
 CHARACTER(LEN=6), INTENT(IN)  :: HSUBROUTINE   ! Name of the subroutine to call
INTEGER, OPTIONAL, INTENT(IN) :: KNBLINES
REAL, OPTIONAL,    INTENT(IN) :: PNODATA
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER :: INBLINES
!----------------------------------------------------------------------------
!
      IF (LHOOK) CALL DR_HOOK('PT_BY_PT_TREATMENT',0,ZHOOK_HANDLE)
      INBLINES = 1
      IF (PRESENT(KNBLINES)) INBLINES = KNBLINES

      SELECT CASE (HSUBROUTINE)

      CASE ('A_COVR')
      IF (PRESENT(PNODATA)) THEN
        CALL AVERAGE1_COVER(KLUOUT,INBLINES,PLAT,PLON,PVALUE,PNODATA)
      ELSE
        CALL AVERAGE1_COVER(KLUOUT,INBLINES,PLAT,PLON,PVALUE)
      ENDIF

      CASE ('A_OROG')
      IF (PRESENT(PNODATA)) THEN
        CALL AVERAGE1_OROGRAPHY(USS, &
                                KLUOUT,INBLINES,PLAT,PLON,PVALUE,PNODATA)
      ELSE
        CALL AVERAGE1_OROGRAPHY(USS, &
                                KLUOUT,INBLINES,PLAT,PLON,PVALUE)
      ENDIF

      CASE ('A_CTI ')
      IF (PRESENT(PNODATA)) THEN
        CALL AVERAGE1_CTI(KLUOUT,INBLINES,PLAT,PLON,PVALUE,PNODATA)
      ELSE
        CALL AVERAGE1_CTI(KLUOUT,INBLINES,PLAT,PLON,PVALUE)
      ENDIF

      CASE ('A_LDBD')
      IF (PRESENT(PNODATA)) THEN
        CALL AVERAGE1_LDB(KLUOUT,INBLINES,PLAT,PLON,PVALUE,'D',PNODATA)
      ELSE
        CALL AVERAGE1_LDB(KLUOUT,INBLINES,PLAT,PLON,PVALUE,'D')
      ENDIF
              
      CASE ('A_LDBS')
      IF (PRESENT(PNODATA)) THEN
        CALL AVERAGE1_LDB(KLUOUT,INBLINES,PLAT,PLON,PVALUE,'S',PNODATA)
      ELSE
        CALL AVERAGE1_LDB(KLUOUT,INBLINES,PLAT,PLON,PVALUE,'S')
      ENDIF
                    
      CASE ('A_MESH')
      IF (PRESENT(PNODATA)) THEN
        CALL AVERAGE1_MESH(KLUOUT,INBLINES,PLAT,PLON,PVALUE,PNODATA)
      ELSE
        CALL AVERAGE1_MESH(KLUOUT,INBLINES,PLAT,PLON,PVALUE)
      ENDIF
              
      END SELECT
IF (LHOOK) CALL DR_HOOK('PT_BY_PT_TREATMENT',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE PT_BY_PT_TREATMENT
