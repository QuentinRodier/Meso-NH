!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
SUBROUTINE PREP_ISBA_ESC_EXTERN (GCP,HPROGRAM,HSURF,HFILE,HFILETYPE,HFILEPGD,HFILEPGDTYPE,KLUOUT,PFIELD,OPREP)
!     #################################################################################
!
!!****  *PREP_ISBA_ESC_EXTERN* - initializes ISBA-CC Explicit Soil fields from external isba field
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
!!     B. Decharme 
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    10/2021
!!------------------------------------------------------------------
!
USE MODD_GRID_CONF_PROJ_n, ONLY : GRID_CONF_PROJ_t
!
USE MODD_SURFEX_MPI, ONLY : NRANK, NPIO
USE MODD_PREP,           ONLY : CINGRID_TYPE, CINTERP_TYPE
USE MODD_SURF_PAR,       ONLY : XUNDEF, LEN_HREC
!
USE MODI_PREP_GRID_EXTERN
USE MODI_READ_SURF
USE MODI_OPEN_AUX_IO_SURF
USE MODI_CLOSE_AUX_IO_SURF
USE MODI_MAKE_CHOICE_ARRAY
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
TYPE(GRID_CONF_PROJ_t),INTENT(INOUT) :: GCP
!
 CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
 CHARACTER(LEN=8),   INTENT(IN)  :: HSURF     ! type of field
 CHARACTER(LEN=28),  INTENT(IN)  :: HFILE     ! name of file
 CHARACTER(LEN=6),   INTENT(IN)  :: HFILETYPE ! type of input file
 CHARACTER(LEN=28),  INTENT(IN)  :: HFILEPGD     ! name of file
 CHARACTER(LEN=6),   INTENT(IN)  :: HFILEPGDTYPE ! type of input file
INTEGER,             INTENT(IN)  :: KLUOUT    ! logical unit of output listing
REAL,DIMENSION(:,:,:,:), POINTER :: PFIELD    ! field to interpolate horizontally (on final soil grid)
LOGICAL,            INTENT(INOUT):: OPREP
!
!*      0.2    declarations of local variables
!
REAL, DIMENSION(:), ALLOCATABLE     :: ZMASK
!
CHARACTER(LEN=LEN_HREC) :: YRECFM         ! Name of the article to be read
INTEGER           :: IRESP          ! reading return code
INTEGER           :: INI            ! total 1D dimension
INTEGER           :: IPATCH         ! number of patch
CHARACTER(LEN=3)  :: YRESPSL
CHARACTER(LEN=4)  :: YLVL
!
INTEGER           :: IVERSION, IBUGFIX     ! surface version
!
LOGICAL :: GDIM
!
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!------------------------------------------------------------------------------
!
!*      1.     Preparation of IO for reading in the file
!              -----------------------------------------
!
!* Note that all points are read, even those without physical meaning.
!  These points will not be used during the horizontal interpolation step.
!  Their value must be defined as XUNDEF.
!
IF (LHOOK) CALL DR_HOOK('PREP_ISBA_ESC_EXTERN',0,ZHOOK_HANDLE)
!
!------------------------------------------------------------------------------
!
!*      2.     Reading of grid
!              ---------------
!
CALL OPEN_AUX_IO_SURF(HFILEPGD,HFILEPGDTYPE,'FULL  ')
!
CALL PREP_GRID_EXTERN(GCP,HFILEPGDTYPE,KLUOUT,CINGRID_TYPE,CINTERP_TYPE,INI)
!
YRECFM='VERSION'
CALL READ_SURF(HFILEPGDTYPE,YRECFM,IVERSION,IRESP)
!
ALLOCATE(ZMASK(INI))
IF (IVERSION>=7) THEN 
  YRECFM='FRAC_NATURE'
  CALL READ_SURF(HFILEPGDTYPE,YRECFM,ZMASK,IRESP,HDIR='A')
ELSE
  ZMASK(:) = 1.
ENDIF
!
CALL CLOSE_AUX_IO_SURF(HFILEPGD,HFILEPGDTYPE)
!
CALL OPEN_AUX_IO_SURF(HFILE,HFILETYPE,'FULL  ')
YRECFM='VERSION'
CALL READ_SURF(HFILEPGDTYPE,YRECFM,IVERSION,IRESP)
YRECFM='BUG'
CALL READ_SURF(HFILETYPE,YRECFM,IBUGFIX,IRESP)
GDIM = (IVERSION>8 .OR. IVERSION==8 .AND. IBUGFIX>0)
IF (GDIM) CALL READ_SURF(HFILETYPE,'SPLIT_PATCH',GDIM,IRESP)
!
CALL CLOSE_AUX_IO_SURF(HFILE,HFILETYPE)
!
IF (NRANK/=NPIO) INI = 0
!
!---------------------------------------------------------------------------------------
!
!*      3.     Transformation into physical quantity to be interpolated
!              --------------------------------------------------------
!
CALL OPEN_AUX_IO_SURF(HFILEPGD,HFILEPGDTYPE,'NATURE')
YRECFM='RESPSL'
CALL READ_SURF(HFILEPGDTYPE,YRECFM,YRESPSL,IRESP,HDIR='-')  
YRECFM='PATCH_NUMBER'
CALL READ_SURF(HFILEPGDTYPE,YRECFM,IPATCH,IRESP,HDIR='-')
CALL CLOSE_AUX_IO_SURF(HFILEPGD,HFILEPGDTYPE)
!
IF(IVERSION<9.OR.YRESPSL/='DIF')THEN
  OPREP = .FALSE.
  IF (LHOOK) CALL DR_HOOK('PREP_ISBA_ESC_EXTERN',1,ZHOOK_HANDLE)
  RETURN
ENDIF
!

OPREP = .FALSE.

!---------------------------------------------------------------------------
!
!*      6.     End of IO
!              ---------
!
IF (LHOOK) CALL DR_HOOK('PREP_ISBA_ESC_EXTERN',1,ZHOOK_HANDLE)
!
!---------------------------------------------------------------------------
!---------------------------------------------------------------------------
END SUBROUTINE PREP_ISBA_ESC_EXTERN
