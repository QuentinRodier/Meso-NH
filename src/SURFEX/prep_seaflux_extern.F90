!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
SUBROUTINE PREP_SEAFLUX_EXTERN(HPROGRAM,HSURF,HFILE,HFILETYPE,HFILEPGD,HFILEPGDTYPE,KLUOUT,PFIELD)
!     #################################################################################
!
USE MODD_TYPE_DATE_SURF
!
USE MODI_PREP_GRID_EXTERN
USE MODI_READ_SURF
USE MODI_OPEN_AUX_IO_SURF
USE MODI_CLOSE_AUX_IO_SURF
!
USE MODD_PREP,       ONLY : CINGRID_TYPE, CINTERP_TYPE
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
 CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
 CHARACTER(LEN=7),   INTENT(IN)  :: HSURF     ! type of field
 CHARACTER(LEN=28),  INTENT(IN)  :: HFILE     ! name of file
 CHARACTER(LEN=6),   INTENT(IN)  :: HFILETYPE ! type of input file
 CHARACTER(LEN=28),  INTENT(IN)  :: HFILEPGD     ! name of file
 CHARACTER(LEN=6),   INTENT(IN)  :: HFILEPGDTYPE ! type of input file
INTEGER,            INTENT(IN)  :: KLUOUT    ! logical unit of output listing
REAL,DIMENSION(:,:), POINTER    :: PFIELD    ! field to interpolate horizontally
!
!*      0.2    declarations of local variables
!
!
 CHARACTER(LEN=12) :: YRECFM         ! Name of the article to be read
INTEGER           :: IRESP          ! reading return code
!
INTEGER           :: INI            ! total 1D dimension
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------------
!
!*      1.     Preparation of IO for reading in the file
!              -----------------------------------------
!
!* Note that all points are read, even those without physical meaning.
!  These points will not be used during the horizontal interpolation step.
!  Their value must be defined as XUNDEF.
!
IF (LHOOK) CALL DR_HOOK('PREP_SEAFLUX_EXTERN',0,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
!*      2.     Reading of grid
!              ---------------
!
 CALL OPEN_AUX_IO_SURF(HFILEPGD,HFILEPGDTYPE,'SEA   ')
 CALL PREP_GRID_EXTERN(HFILEPGDTYPE,KLUOUT,CINGRID_TYPE,CINTERP_TYPE,INI)
 CALL CLOSE_AUX_IO_SURF(HFILEPGD,HFILEPGDTYPE)
!
!---------------------------------------------------------------------------------------
SELECT CASE(HSURF)
!---------------------------------------------------------------------------------------
!
!*     3.      Orography
!              ---------
!
  CASE('ZS     ')
    ALLOCATE(PFIELD(INI,1))
    PFIELD(:,:) = 0.
!
!*      4.  Sea surface temperature
!           -----------------------
!
  CASE('SST    ')
    ALLOCATE(PFIELD(INI,1))
    YRECFM='SST'
    CALL OPEN_AUX_IO_SURF(HFILE,HFILETYPE,'SEA   ')
    CALL READ_SURF(HFILETYPE,YRECFM,PFIELD(:,1),IRESP,HDIR='A')
    CALL CLOSE_AUX_IO_SURF(HFILE,HFILETYPE)
!
!---------------------------------------------------------------------------------------
END SELECT
!-------------------------------------------------------------------------------------
!
!*      6.     End of IO
!              ---------
!
IF (LHOOK) CALL DR_HOOK('PREP_SEAFLUX_EXTERN',1,ZHOOK_HANDLE)
!
!---------------------------------------------------------------------------------------
!
END SUBROUTINE PREP_SEAFLUX_EXTERN
