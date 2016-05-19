!-------------------------------------------------------------------------------
!     #############################################################
      SUBROUTINE INIT_SURF_TOPD(HPROGRAM,KI)
!     #############################################################
!
!!****  *INIT_SURF_TOPD* - routine to initialize variables needed for coupling with Topmodel
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!    The routine open and read the namelists NAM_COUPL_TOPD and NAM_TOPD,
!! calculates the number of catchments concerned, the different time step 
!! variables and all the variables nedded for coupling with Topmodel. 
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
!!	B. Vincendon   *Meteo France*	
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    11/2006
!!      Modif 04/2007: Arguments PTOPD_STEP,KNB_TOPD_STEP become module
!!                     variables from MODD_TOPDDYN
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_SURFEX_MPI, ONLY : NPROC
USE MODD_SURFEX_OMP, ONLY : NBLOCKTOT
!
USE MODD_TOPODYN, ONLY :CCAT, XSPEEDR, XSPEEDH, NNCAT, &
                        XRTOP_D2, XSPEEDG
USE MODD_COUPLING_TOPD, ONLY :  LCOUPL_TOPD, NNB_TOPD, LBUDGET_TOPD
!
USE MODD_ISBA_n, ONLY : TSNOW
!
USE MODI_GET_LUOUT
USE MODI_ABOR1_SFX
!
USE MODI_INIT_TOPD
USE MODI_INIT_COUPL_TOPD
USE MODI_INIT_BUDGET_COUPL_ROUT
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
 CHARACTER(LEN=*),  INTENT(IN)     :: HPROGRAM      !
INTEGER,           INTENT(IN)     :: KI            ! grid dimension
!
INTEGER :: ILUOUT
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('INIT_SURF_TOPD',0,ZHOOK_HANDLE)
!
IF (LCOUPL_TOPD) THEN
  IF (NPROC>1) CALL ABOR1_SFX('INIT_SURF_TOPD: TOPD CANNOT RUN WITH MORE THAN 1 MPI TASK') 
  IF (NBLOCKTOT>1) CALL ABOR1_SFX("INIT_SURF_TOPD: TOPD CANNOT RUN WITH NUMEROUS OPENMP BLOCKS")
  IF (TSNOW%SCHEME/='3-L') &
        CALL ABOR1_SFX("INIT_SURF_TOPD: coupling with topmodel only runs with TSNOW%SCHEME=3-L")
ENDIF
!
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!            
!         1.   Reads the namelists
!              --------------------
!
WRITE(ILUOUT,*) 'Debut init_surf_topo_n'
!
IF (LCOUPL_TOPD) THEN
  !
  !         3.   Initialises variables specific to Topmodel
  !              -------------------------------------------
  WRITE(ILUOUT,*) 'NNCAT',NNCAT
  !
  CALL INIT_TOPD(HPROGRAM)
  !
  !         4.   Initialises variables nedded for coupling with Topmodel
  !              -------------------------------------------------------
  !
  CALL INIT_COUPL_TOPD(HPROGRAM,KI)
  !
  WRITE(ILUOUT,*) 'Couplage avec TOPMODEL active'
  !
  IF (LBUDGET_TOPD) CALL INIT_BUDGET_COUPL_ROUT(KI)
  !
ELSE
  !
  WRITE(ILUOUT,*) 'Pas de couplage avec TOPMODEL'
  !
ENDIF
!
IF (LHOOK) CALL DR_HOOK('INIT_SURF_TOPD',1,ZHOOK_HANDLE)
!
END SUBROUTINE INIT_SURF_TOPD
