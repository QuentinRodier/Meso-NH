!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
      SUBROUTINE WRITESURF_WATFLUX_n(HPROGRAM)
!     ########################################
!
!!****  *WRITESURF_WATFLUX_n* - writes WATFLUX fields
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
USE MODD_WATFLUX_n,      ONLY : XTS, XZ0, TTIME,  &
                                  LINTERPOL_TS, XTS_MTH  
!
USE MODI_WRITE_SURF
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
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! program calling

!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER          :: JMTH,INMTH
 CHARACTER(LEN=2) :: YMTH
!
INTEGER           :: IRESP          ! IRESP  : return-code if a problem appears
 CHARACTER(LEN=12) :: YRECFM         ! Name of the article to be read
 CHARACTER(LEN=100):: YCOMMENT       ! Comment string
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!
!*       3.     Prognostic fields:
!               -----------------
!
!* water temperature
!
IF (LHOOK) CALL DR_HOOK('WRITESURF_WATFLUX_N',0,ZHOOK_HANDLE)
IF(LINTERPOL_TS)THEN
!
  INMTH=SIZE(XTS_MTH,2)
!
  DO JMTH=1,INMTH
     WRITE(YMTH,'(I2)') (JMTH-1)
     YRECFM='TS_WATER'//ADJUSTL(YMTH(:LEN_TRIM(YMTH)))
     YCOMMENT='TS_WATER month t'//ADJUSTL(YMTH(:LEN_TRIM(YMTH)))
     CALL WRITE_SURF(HPROGRAM,YRECFM,XTS_MTH(:,JMTH),IRESP,HCOMMENT=YCOMMENT)
  ENDDO
!
ENDIF
!
YRECFM='TS_WATER'
YCOMMENT='TS_WATER (K)'
 CALL WRITE_SURF(HPROGRAM,YRECFM,XTS(:),IRESP,HCOMMENT=YCOMMENT)
!
!-------------------------------------------------------------------------------
!
!*       4.     Semi-prognostic fields:
!               ----------------------
!
!* roughness length
!
YRECFM='Z0WATER'
YCOMMENT='Z0WATER (m)'
 CALL WRITE_SURF(HPROGRAM,YRECFM,XZ0(:),IRESP,HCOMMENT=YCOMMENT)
!
!
!-------------------------------------------------------------------------------
!
!*       5.  Time
!            ----
!
YRECFM='DTCUR'
YCOMMENT='s'
 CALL WRITE_SURF(HPROGRAM,YRECFM,TTIME,IRESP,HCOMMENT=YCOMMENT)
IF (LHOOK) CALL DR_HOOK('WRITESURF_WATFLUX_N',1,ZHOOK_HANDLE)
!

!-------------------------------------------------------------------------------
!
END SUBROUTINE WRITESURF_WATFLUX_n
