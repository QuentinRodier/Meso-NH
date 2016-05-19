!     #########
SUBROUTINE PREP_FLAKE_ASCLLV(HPROGRAM,HSURF,KLUOUT,PFIELD)
!     #################################################################################
!
!!****  *PREP_FLAKE_ASCLLV* - prepares FLAKE field from prescribed values
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
!!     P. Le Moigne 
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    03/2007
!!------------------------------------------------------------------
!
!
USE MODD_PREP,           ONLY : CINTERP_TYPE
USE MODD_PGD_GRID,       ONLY : NL,LLATLONMASK,CGRID,XGRID_PAR,NGRID_PAR
USE MODD_PGDWORK,        ONLY : CATYPE
USE MODD_SURF_PAR,       ONLY : XUNDEF
USE MODD_PREP_FLAKE,     ONLY : CTYPE, CFILE_FLAKE
USE MODI_PGD_FIELD
USE MODI_GET_LATLONMASK_n
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_GET_TYPE_DIM_n
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
 CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
 CHARACTER(LEN=7),   INTENT(IN)  :: HSURF     ! type of field
INTEGER,            INTENT(IN)  :: KLUOUT    ! logical unit of output listing
REAL, POINTER, DIMENSION(:,:)   :: PFIELD    ! field to interpolate horizontally
!
!*      0.2    declarations of local variables
!
INTEGER :: IL
!
REAL, ALLOCATABLE, DIMENSION(:)     :: ZFIELD
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('PREP_FLAKE_ASCLLV',0,ZHOOK_HANDLE)
CATYPE = 'ARI'
!
!*      1.    get full dimension of grid
!
 CALL GET_TYPE_DIM_n('FULL  ',NL)
!
!*      2.    get water dimension
!
 CALL GET_TYPE_DIM_n('WATER ',IL)
!
ALLOCATE(ZFIELD(IL))
!
!*      3.    get grid informations known over full grid
!
 CALL GET_LATLONMASK_n(LLATLONMASK,CGRID,XGRID_PAR,NGRID_PAR)
!
!
SELECT CASE(HSURF)
!
!
!*      5.    surface temperature

  CASE('TS     ')

    CALL PGD_FIELD(HPROGRAM,'TS_WATER: temperature','WAT',CFILE_FLAKE,   &
                        CTYPE,XUNDEF,ZFIELD(:))  

    ALLOCATE(PFIELD(IL,1))
    PFIELD(:,1) = ZFIELD(:)

END SELECT
!
!*      6.     Interpolation method
!              --------------------
!
CINTERP_TYPE='NONE  '
DEALLOCATE(ZFIELD)
IF (LHOOK) CALL DR_HOOK('PREP_FLAKE_ASCLLV',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
END SUBROUTINE PREP_FLAKE_ASCLLV
