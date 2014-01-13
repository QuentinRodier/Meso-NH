!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
      SUBROUTINE WRITESURF_PGD_SEAF_PAR_n(HPROGRAM)
!     ################################################
!
!!****  *WRITESURF_PGD_SEAF_PAR_n* - writes SEAFLUX sst
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
!!	P. Le Moigne   *Meteo France*	
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    09/2007 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_TYPE_DATE_SURF
USE MODD_DATA_SEAFLUX_n,    ONLY : NTIME, XDATA_SST, TDATA_SST
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
INTEGER           :: IRESP          ! IRESP  : return-code if a problem appears
 CHARACTER(LEN=12) :: YRECFM         ! Name of the article to be read
 CHARACTER(LEN=100):: YCOMMENT       ! Comment string
INTEGER           :: JTIME          ! loop index
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('WRITESURF_PGD_SEAF_PAR_N',0,ZHOOK_HANDLE)
NTIME = SIZE(XDATA_SST,2)
YRECFM='ND_SEA_TIME'
YCOMMENT='(-)'
 CALL WRITE_SURF(HPROGRAM,YRECFM,NTIME,IRESP,HCOMMENT=YCOMMENT)
!
DO JTIME=1,NTIME
  WRITE(YRECFM,FMT='(A7,I3.3)') 'D_SST_T',JTIME
  YCOMMENT='X_Y_DATA_SST'
  CALL WRITE_SURF(HPROGRAM,YRECFM,XDATA_SST(:,JTIME),IRESP,HCOMMENT=YCOMMENT)
END DO
!
YRECFM='TD_SST'
YCOMMENT='(-)'
 CALL WRITE_SURF(HPROGRAM,YRECFM,TDATA_SST,IRESP,HCOMMENT=YCOMMENT)
IF (LHOOK) CALL DR_HOOK('WRITESURF_PGD_SEAF_PAR_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE WRITESURF_PGD_SEAF_PAR_n
