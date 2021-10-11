!#############################################################
SUBROUTINE INIT_TEB_HYDRO_n(DTCO,TOP,U,TH,KPATCH, KTEB_SOIL, HPROGRAM,HINIT,KI)
!#############################################################
!
!!****  *INIT_TEB_HYDRO_n* - routine to initialize ISBA for urban hydrology
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
!!	A. Lemonsu  *Meteo France*	
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    02/2013
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!USE MODD_TYPE_DATE_SURF
!USE MODD_TYPE_SNOW
!
USE MODD_SURF_PAR, ONLY : XUNDEF
USE MODD_DATA_COVER_n, ONLY : DATA_COVER_t
USE MODD_SURF_ATM_n,   ONLY : SURF_ATM_t
USE MODD_TEB_HYDRO_n,     ONLY : TEB_HYDRO_t
USE MODD_TEB_OPTION_n, ONLY : TEB_OPTIONS_t
!
USE MODI_GET_LUOUT
USE MODI_READ_TEB_HYDRO_n
USE MODI_OPEN_FILE
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*       0.1   Declarations of arguments
!              -------------------------
!
TYPE(DATA_COVER_t),      INTENT(INOUT) :: DTCO
TYPE(TEB_OPTIONS_t),     INTENT(INOUT) :: TOP
TYPE(SURF_ATM_t),        INTENT(INOUT) :: U
TYPE(TEB_HYDRO_t),       INTENT(INOUT) :: TH
INTEGER,                 INTENT(IN)    :: KPATCH      !
INTEGER,                 INTENT(IN)    :: KTEB_SOIL   !
CHARACTER(LEN=6),        INTENT(IN)    :: HPROGRAM    ! program calling surf. schemes
CHARACTER(LEN=3),        INTENT(IN)    :: HINIT       ! choice of fields to initialize
INTEGER,                 INTENT(IN)    :: KI          ! number of points
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER          :: ILUOUT ! unit of output listing file
CHARACTER(LEN=3) :: YPATCH ! patch identificator
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------
!
!               Initialisation for IO
!
IF (LHOOK) CALL DR_HOOK('INIT_TEB_HYDRO_N',0,ZHOOK_HANDLE)
CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
!-------------------------------------------------------------------------------
!
IF (HINIT/='ALL') THEN
  IF (LHOOK) CALL DR_HOOK('INIT_TEB_HYDRO_N',1,ZHOOK_HANDLE)      
  RETURN
ENDIF
!
!-------------------------------------------------------------------------------
!
!*   1       Allocations
!
ALLOCATE(TH%XCOND_ROAD(KI,KTEB_SOIL))
ALLOCATE(TH%XCOND_BLD (KI,KTEB_SOIL))
ALLOCATE(TH%XCOND_GD  (KI,KTEB_SOIL))
!
TH%XCOND_ROAD(:,:)= XUNDEF
TH%XCOND_BLD(:,:) = XUNDEF
TH%XCOND_GD(:,:)  = XUNDEF
!
!*      3.      Prognostic and semi-prognostic fields
!               -------------------------------------
!
  YPATCH='   '
   IF (TOP%NTEB_PATCH>1) WRITE(YPATCH,FMT='(A,I1,A)') 'T',KPATCH,'_'
!
  CALL READ_TEB_HYDRO_n(DTCO,U,TH,HPROGRAM,YPATCH,KTEB_SOIL)
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('INIT_TEB_HYDRO_N',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE INIT_TEB_HYDRO_n
