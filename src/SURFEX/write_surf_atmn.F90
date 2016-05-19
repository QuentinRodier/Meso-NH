!     ####################################
      SUBROUTINE WRITE_SURF_ATM_n(HPROGRAM,HWRITE,OLAND_USE)
!     ####################################
!
!!****  *WRITE_SURF_ATM_n* - routine to write surface variables 
!!                           in their respective files or in file
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
!!      Modified    06/2007, P.LeMoigne: do not write pgd fields in
!!                                       historical files
!!      Modified    03/2009, B.Decharme: keys for arrange cover
!!      Modified    04/2009, B.Decharme: write precipitation forcing into the restart file for ARPEGE/ALADIN run
!       Modified    06/2009, B.Decharme: flag to desactivate writing of horizontal grid 
!       Modified    08/2009, B.Decharme: BUDGETC for all tiles
!       Modified    07/2011, B.Decharme: delete write pgd fields
!       Modified    07/2011, B.Decharme: land_use key for writing semi-prognostic variables
!       Modified    05/2012, B.Decharme: supress LPROVAR_TO_DIAG to write prognostic fields if user want
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
USE MODD_SURF_CONF,       ONLY : CPROGNAME
USE MODD_SURF_PAR,        ONLY : NVERSION, NBUGFIX
USE MODD_WRITE_SURF_ATM,  ONLY : LNOWRITE_CANOPY
USE MODD_SURF_ATM_n,      ONLY : NDIM_FULL, NDIM_SEA, NDIM_WATER, NDIM_TOWN, NDIM_NATURE, TTIME
USE MODD_SURF_ATM_SSO_n,  ONLY : CROUGH
USE MODD_DIAG_SURF_ATM_n, ONLY : TIME_BUDGETC,LSURF_BUDGETC,LSELECT  
USE MODD_CH_SURF_n,       ONLY : LCH_EMIS, LRW_CH_EMIS, CCH_EMIS
USE MODD_SURF_ATM_GRID_n, ONLY : CGRID, XGRID_PAR, XLAT, XLON, XMESH_SIZE
!
USE MODI_INIT_IO_SURF_n
USE MODI_WRITE_SURF
USE MODI_WRITE_SEA_n
USE MODI_WRITE_INLAND_WATER_n
USE MODI_WRITE_NATURE_n
USE MODI_WRITE_TOWN_n
USE MODI_END_IO_SURF_n
USE MODI_WRITE_GRID
!
USE MODI_WRITESURF_ATM_CONF_n
USE MODI_WRITESURF_SSO_CANOPY_n
USE MODI_READWRITE_EMIS_FIELD_n
USE MODI_WRITESURF_PRECIP_n
USE MODI_WRITE_DIAG_CH_SNAP_n
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
 CHARACTER(LEN=6),    INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
 CHARACTER(LEN=3),    INTENT(IN)  :: HWRITE    ! 'PREP' : does not write SBL XUNDEF fields
!                                             ! 'ALL' : all fields are written
LOGICAL,             INTENT(IN)  :: OLAND_USE !
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
 CHARACTER(LEN=100) :: YCOMMENT
INTEGER            :: IRESP
LOGICAL            :: LSAVE_SELECT
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('WRITE_SURF_ATM_N',0,ZHOOK_HANDLE)
CPROGNAME = HPROGRAM
!
!*       1.     Configuration and cover fields:
!               ------------------------------
!
!
!         Initialisation for IO
!
 CALL INIT_IO_SURF_n(HPROGRAM,'FULL  ','SURF  ','WRITE')
!
LSAVE_SELECT=LSELECT
LSELECT     =.FALSE.
!
YCOMMENT='(-)'
 CALL WRITE_SURF(HPROGRAM,'VERSION',NVERSION,IRESP,YCOMMENT)
 CALL WRITE_SURF(HPROGRAM,'BUG    ',NBUGFIX ,IRESP,YCOMMENT)
 CALL WRITE_SURF(HPROGRAM,'STORAGETYPE',HWRITE,IRESP,YCOMMENT)
 CALL WRITE_SURF(HPROGRAM,'DIM_FULL  ',NDIM_FULL,IRESP,HCOMMENT=YCOMMENT)
!
YCOMMENT='s'
 CALL WRITE_SURF(HPROGRAM,'DTCUR',TTIME,IRESP,YCOMMENT)
!
LSELECT=LSAVE_SELECT
!
 CALL WRITE_GRID(HPROGRAM,CGRID,XGRID_PAR,XLAT,XLON,XMESH_SIZE,IRESP)
!
 CALL WRITESURF_ATM_CONF_n(HPROGRAM)
!
 CALL WRITESURF_SSO_CANOPY_n(HPROGRAM,HWRITE,(CROUGH=='BE04' .AND. .NOT. LNOWRITE_CANOPY))
!
 CALL WRITESURF_PRECIP_n(HPROGRAM)
!
YCOMMENT='flag for accumulated variables'
 CALL WRITE_SURF(HPROGRAM,'BUDC',LSURF_BUDGETC,IRESP,HCOMMENT=YCOMMENT)
!
IF (LSURF_BUDGETC) THEN
   YCOMMENT='time of beginning of accumulation'
   CALL WRITE_SURF(HPROGRAM,'TBUDC',TIME_BUDGETC,IRESP,HCOMMENT=YCOMMENT)   
END IF
!  
!         End of IO
!
 CALL END_IO_SURF_n(HPROGRAM)
!
!
!*       2.     Chemistry
!               ---------
!
IF (LCH_EMIS) THEN
  IF (CCH_EMIS=='AGGR') THEN 
    IF (LRW_CH_EMIS) CALL READWRITE_EMIS_FIELD_n(HPROGRAM)
  ELSE IF (CCH_EMIS=='SNAP') THEN
    CALL WRITE_DIAG_CH_SNAP_n(HPROGRAM)
  END IF
END IF
!
!
!*       3.     Sea
!               ---
!
IF (NDIM_SEA>0) CALL WRITE_SEA_n(HPROGRAM,HWRITE)
!
!
!*       4.     Inland water
!               ------------
!
IF (NDIM_WATER>0) CALL WRITE_INLAND_WATER_n(HPROGRAM,HWRITE)
!
!
!*       5.     Vegetation scheme
!               -----------------
!
IF (NDIM_NATURE>0) CALL WRITE_NATURE_n(HPROGRAM,HWRITE,OLAND_USE)
!
!
!*       6.     Urban scheme
!               ------------
!
IF (NDIM_TOWN>0) CALL WRITE_TOWN_n(HPROGRAM,HWRITE)
IF (LHOOK) CALL DR_HOOK('WRITE_SURF_ATM_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE WRITE_SURF_ATM_n
