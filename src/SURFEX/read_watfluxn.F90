!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
      SUBROUTINE READ_WATFLUX_n(HPROGRAM)
!     #########################################
!
!!****  *READ_WATFLUX_n* - reads WATFLUX variables
!! 
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
USE MODD_WATFLUX_n,      ONLY : XTS, XZ0, LINTERPOL_TS, &
                                  CINTERPOL_TS, XTS_MTH, &
                                  TTIME  
!
USE MODI_READ_SURF
USE MODI_INTERPOL_TS_WATER_MTH
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_GET_TYPE_DIM_n
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! calling program
!
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER           :: JMTH, INMTH
 CHARACTER(LEN=2 ) :: YMTH
!
INTEGER           :: ILU          ! 1D physical dimension
!
INTEGER           :: IRESP          ! Error code after redding
!
 CHARACTER(LEN=12) :: YRECFM         ! Name of the article to be read
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
!* 1D physical dimension
!
IF (LHOOK) CALL DR_HOOK('READ_WATFLUX_N',0,ZHOOK_HANDLE)
YRECFM='SIZE_WATER'
 CALL GET_TYPE_DIM_n('WATER ',ILU)
!
!*       3.     Prognostic fields:
!               -----------------
!
!* water temperature
!
ALLOCATE(XTS(ILU))
!
IF(LINTERPOL_TS)THEN
!
! Precedent, Current and Next Monthly SST
  INMTH=3
! Precedent, Current and Next Annual Monthly SST
  IF(CINTERPOL_TS=='ANNUAL')INMTH=14
!
  ALLOCATE(XTS_MTH(SIZE(XTS),INMTH))
  DO JMTH=1,INMTH
     WRITE(YMTH,'(I2)') (JMTH-1)
     YRECFM='TS_WATER'//ADJUSTL(YMTH(:LEN_TRIM(YMTH)))
     CALL READ_SURF(HPROGRAM,YRECFM,XTS_MTH(:,JMTH),IRESP)
  ENDDO
!
  CALL INTERPOL_TS_WATER_MTH(TTIME%TDATE%YEAR,TTIME%TDATE%MONTH,TTIME%TDATE%DAY,XTS)
!
ELSE
! 
  ALLOCATE(XTS_MTH(0,0))
!
  YRECFM='TS_WATER'
  CALL READ_SURF(HPROGRAM,YRECFM,XTS(:),IRESP)
!
ENDIF
!
!
!-------------------------------------------------------------------------------
!
!*       4.     Semi-prognostic fields:
!               ----------------------
!
!* roughness length
!
ALLOCATE(XZ0(ILU))
YRECFM='Z0WATER'
XZ0(:) = 0.001
  CALL READ_SURF(HPROGRAM,YRECFM,XZ0(:),IRESP)
IF (LHOOK) CALL DR_HOOK('READ_WATFLUX_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------

!
END SUBROUTINE READ_WATFLUX_n
