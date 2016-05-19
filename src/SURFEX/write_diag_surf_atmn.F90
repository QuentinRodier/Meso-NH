!     #################################################################################
SUBROUTINE WRITE_DIAG_SURF_ATM_n(HPROGRAM,HWRITE)
!     #################################################################################
!
!!****  *WRITE_DIAG_SURF_ATM_n * - Chooses the surface schemes for diagnostics writing
!!
!!    PURPOSE
!!    -------
!
!!**  METHOD
!!    ------
!!
!!    REFERENCE
!!    ---------
!!      
!!
!!    AUTHOR
!!    ------
!!     V. Masson 
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2004
!!------------------------------------------------------------------
!
USE MODD_SURF_CONF,      ONLY : CPROGNAME
USE MODD_SURF_ATM_n,     ONLY : NDIM_SEA,  NDIM_TOWN,  NDIM_NATURE,  NDIM_WATER, &
                                  XSEA    ,  XTOWN    ,  XNATURE    ,  XWATER,     &
                                  TTIME  

USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODD_DIAG_SURF_ATM_n,ONLY : XDIAG_TSTEP, LFRAC, LDIAG_GRID
!
USE MODI_INIT_IO_SURF_n
USE MODI_WRITE_SURF
USE MODI_END_IO_SURF_n
!
USE MODI_WRITE_DIAG_NATURE_n 
USE MODI_WRITE_DIAG_SEA_n 
USE MODI_WRITE_DIAG_INLAND_WATER_n 
USE MODI_WRITE_DIAG_TOWN_n 
!
USE MODI_WRITE_DIAG_SEB_SURF_ATM_n
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
 CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
 CHARACTER(LEN=3),   INTENT(IN)  :: HWRITE    ! 'PGD' : only physiographic fields are written
!                                            ! 'ALL' : all fields are written
!
!
!*      0.2    declarations of local variables
!
 CHARACTER(LEN=100) :: YCOMMENT
INTEGER            :: IRESP
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('WRITE_DIAG_SURF_ATM_N',0,ZHOOK_HANDLE)
CPROGNAME = HPROGRAM
!
IF (NDIM_SEA    >0) CALL WRITE_DIAG_SEA_n         (HPROGRAM,HWRITE)
IF (NDIM_WATER  >0) CALL WRITE_DIAG_INLAND_WATER_n(HPROGRAM,HWRITE)
IF (NDIM_NATURE >0) CALL WRITE_DIAG_NATURE_n      (HPROGRAM,HWRITE)
IF (NDIM_TOWN   >0) CALL WRITE_DIAG_TOWN_n        (HPROGRAM,HWRITE)
!
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
! Writing
! - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
!
!
IF (XDIAG_TSTEP==XUNDEF .OR. ABS(NINT(TTIME%TIME/XDIAG_TSTEP)*XDIAG_TSTEP-TTIME%TIME)<1.E-3 ) THEN
  !
  IF (LFRAC) THEN
    CALL INIT_IO_SURF_n(HPROGRAM,'FULL  ','SURF  ','WRITE')
    YCOMMENT = '(-)'
    CALL WRITE_SURF(HPROGRAM,'FRAC_SEA   ',XSEA,   IRESP,HCOMMENT=YCOMMENT)
    CALL WRITE_SURF(HPROGRAM,'FRAC_NATURE',XNATURE,IRESP,HCOMMENT=YCOMMENT)
    CALL WRITE_SURF(HPROGRAM,'FRAC_WATER ',XWATER, IRESP,HCOMMENT=YCOMMENT)
    CALL WRITE_SURF(HPROGRAM,'FRAC_TOWN  ',XTOWN,  IRESP,HCOMMENT=YCOMMENT)
    CALL END_IO_SURF_n(HPROGRAM)
  END IF
  !
  IF (HWRITE/='PGD'.AND.LDIAG_GRID) CALL WRITE_DIAG_SEB_SURF_ATM_n(HPROGRAM)
END IF
IF (LHOOK) CALL DR_HOOK('WRITE_DIAG_SURF_ATM_N',1,ZHOOK_HANDLE)
!
!--------------------------------------------------------------------------------------
!
END SUBROUTINE WRITE_DIAG_SURF_ATM_n
