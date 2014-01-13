!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
      SUBROUTINE READ_SEAFLUX_n(HPROGRAM)
!     #########################################
!
!!****  *READ_SEAFLUX_n* - read SEAFLUX varaibles
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
!!      Modified    02/2008 Add oceanic variables initialisation
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_SEAFLUX_n,      ONLY : XSST, XZ0, LINTERPOL_SST, &
                                  CINTERPOL_SST, XSST_MTH, TTIME  
USE MODD_OCEAN_n,        ONLY : LMERCATOR
!
USE MODI_READ_SURF
USE MODI_INTERPOL_SST_MTH
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
!
!-------------------------------------------------------------------------------
!
!* 1D physical dimension
!
IF (LHOOK) CALL DR_HOOK('READ_SEAFLUX_N',0,ZHOOK_HANDLE)
YRECFM='SIZE_SEA'
 CALL GET_TYPE_DIM_n('SEA   ',ILU)
!
!*       2.     Prognostic fields:
!               -----------------
!
!* water temperature
!
ALLOCATE(XSST(ILU))
!
IF(LINTERPOL_SST)THEN
!
! Precedent, Current and Next Monthly SST
  INMTH=3
! Precedent, Current and Next Annual Monthly SST
  IF(CINTERPOL_SST=='ANNUAL')INMTH=14
!
  ALLOCATE(XSST_MTH(SIZE(XSST),INMTH))
  DO JMTH=1,INMTH
     WRITE(YMTH,'(I2)') (JMTH-1)
     YRECFM='SST_MTH'//ADJUSTL(YMTH(:LEN_TRIM(YMTH)))
     CALL READ_SURF(HPROGRAM,YRECFM,XSST_MTH(:,JMTH),IRESP)
  ENDDO
!
  CALL INTERPOL_SST_MTH(TTIME%TDATE%YEAR,TTIME%TDATE%MONTH,TTIME%TDATE%DAY,XSST)
!
ELSE
! 
  ALLOCATE(XSST_MTH(0,0))
!
  YRECFM='SST'
  CALL READ_SURF(HPROGRAM,YRECFM,XSST(:),IRESP)
!
ENDIF
!
!-------------------------------------------------------------------------------
!
!*       3.     Semi-prognostic fields:
!               ----------------------
!
!* roughness length
!
ALLOCATE(XZ0(ILU))
YRECFM='Z0SEA'
XZ0(:) = 0.001
 CALL READ_SURF(HPROGRAM,YRECFM,XZ0(:),IRESP)
IF (LHOOK) CALL DR_HOOK('READ_SEAFLUX_N',1,ZHOOK_HANDLE)
!
!------------------------------------------------------------------------------
END SUBROUTINE READ_SEAFLUX_n
