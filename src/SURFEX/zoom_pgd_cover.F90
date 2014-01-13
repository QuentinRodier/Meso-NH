!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
      SUBROUTINE ZOOM_PGD_COVER(HPROGRAM,HINIFILE,HINIFILETYPE,OECOCLIMAP)
!     ###########################################################

!!
!!    PURPOSE
!!    -------
!!   This program prepares the physiographic data fields.
!!
!!    METHOD
!!    ------
!!   
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!
!!    V. Masson                   Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original     13/10/03
!     Modification 17/04/12 M.Tomasini All COVER physiographic fields are now 
!!                                     interpolated for spawning => 
!!                                     ABOR1_SFX if (.NOT.OECOCLIMAP) in comment
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_DATA_COVER_PAR,   ONLY : JPCOVER
USE MODD_SURF_ATM_GRID_n,  ONLY : XLAT, XLON, CGRID, XGRID_PAR
USE MODD_SURF_ATM_n,       ONLY : XCOVER, LCOVER, XSEA, XWATER, XNATURE, XTOWN, &
                                    NSIZE_NATURE, NSIZE_SEA, NR_NATURE, NR_SEA, &
                                    NSIZE_TOWN, NSIZE_WATER,NR_TOWN,NR_WATER,NSIZE_FULL,&
                                    NDIM_NATURE, NDIM_SEA,                  &
                                    NDIM_TOWN,NDIM_WATER,NDIM_FULL  
USE MODD_PREP,             ONLY : CINGRID_TYPE, CINTERP_TYPE
!
USE MODI_CONVERT_COVER_FRAC
USE MODI_OPEN_AUX_IO_SURF
USE MODI_READ_SURF
USE MODI_CLOSE_AUX_IO_SURF
USE MODI_PREP_GRID_EXTERN
USE MODI_HOR_INTERPOL
USE MODI_PREP_OUTPUT_GRID
USE MODI_OLD_NAME
USE MODI_SUM_ON_ALL_PROCS
USE MODI_GET_LUOUT
USE MODI_CLEAN_PREP_OUTPUT_GRID
USE MODI_GET_1D_MASK
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*    0.1    Declaration of dummy arguments
!            ------------------------------
!
 CHARACTER(LEN=6),     INTENT(IN)  :: HPROGRAM    ! program calling
 CHARACTER(LEN=28),    INTENT(IN)  :: HINIFILE    ! input atmospheric file name
 CHARACTER(LEN=6),     INTENT(IN)  :: HINIFILETYPE! input atmospheric file type
LOGICAL,              INTENT(OUT) :: OECOCLIMAP  ! flag to use ecoclimap
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER :: IRESP
INTEGER :: ILUOUT
INTEGER :: INI     ! total 1D dimension (input grid)
INTEGER :: IL      ! total 1D dimension (output grid)
INTEGER :: JCOVER  ! loop counter
INTEGER :: IVERSION       ! surface version
REAL, DIMENSION(:,:), POINTER     :: ZCOVER
REAL, DIMENSION(:,:), POINTER :: ZSEA1, ZWATER1, ZNATURE1, ZTOWN1
REAL, DIMENSION(:,:), POINTER :: ZSEA2, ZWATER2, ZNATURE2, ZTOWN2
REAL, DIMENSION(:),   ALLOCATABLE :: ZSUM
 CHARACTER(LEN=12) :: YRECFM         ! Name of the article to be read
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('ZOOM_PGD_COVER',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
!*      1.     Preparation of IO for reading in the file
!              -----------------------------------------
!
!* Note that all points are read, even those without physical meaning.
!  These points will not be used during the horizontal interpolation step.
!  Their value must be defined as XUNDEF.
!
 CALL OPEN_AUX_IO_SURF(HINIFILE,HINIFILETYPE,'FULL  ')
!
 CALL READ_SURF(HPROGRAM,'ECOCLIMAP',OECOCLIMAP,IRESP)
!
!------------------------------------------------------------------------------
!
!*      2.     Reading of grid
!              ---------------
!
 CALL PREP_GRID_EXTERN(HINIFILETYPE,ILUOUT,CINGRID_TYPE,CINTERP_TYPE,INI)
!
 CALL PREP_OUTPUT_GRID(ILUOUT,CGRID,XGRID_PAR,XLAT,XLON)
!
!------------------------------------------------------------------------------
!
!*      3.     Reading of cover
!              ----------------
!
YRECFM='VERSION'
 CALL READ_SURF(HPROGRAM,YRECFM,IVERSION,IRESP)
!
ALLOCATE(LCOVER(JPCOVER))
 CALL OLD_NAME(HPROGRAM,'COVER_LIST      ',YRECFM)
 CALL READ_SURF(HPROGRAM,YRECFM,LCOVER(:),IRESP,HDIR='-')
!
ALLOCATE(ZCOVER(INI,JPCOVER))
 CALL READ_SURF(HPROGRAM,YRECFM,ZCOVER(:,:),LCOVER,IRESP,HDIR='A')
!
ALLOCATE(ZSEA1   (INI,1))
ALLOCATE(ZNATURE1(INI,1))
ALLOCATE(ZWATER1 (INI,1))
ALLOCATE(ZTOWN1  (INI,1))
!
IF (IVERSION>=7) THEN
  CALL READ_SURF(HPROGRAM,'FRAC_SEA   ',ZSEA1(:,1),   IRESP,HDIR='A')
  CALL READ_SURF(HPROGRAM,'FRAC_NATURE',ZNATURE1(:,1),IRESP,HDIR='A')
  CALL READ_SURF(HPROGRAM,'FRAC_WATER ',ZWATER1(:,1), IRESP,HDIR='A')
  CALL READ_SURF(HPROGRAM,'FRAC_TOWN  ',ZTOWN1(:,1),  IRESP,HDIR='A')
  !
ELSE
  CALL CONVERT_COVER_FRAC(ZCOVER,ZSEA1(:,1),ZNATURE1(:,1),ZTOWN1(:,1),ZWATER1(:,1))
ENDIF
!
 CALL CLOSE_AUX_IO_SURF(HINIFILE,HINIFILETYPE)
!------------------------------------------------------------------------------
!
!*      4.     Interpolations
!              --------------
!
IL = SIZE(XLAT)
ALLOCATE(XCOVER(IL,JPCOVER))
!
 CALL HOR_INTERPOL(ILUOUT,ZCOVER,XCOVER)
!
DEALLOCATE(ZCOVER)
!
ALLOCATE(ZSEA2  (IL,1))
ALLOCATE(ZNATURE2(IL,1))
ALLOCATE(ZWATER2 (IL,1))
ALLOCATE(ZTOWN2  (IL,1))
!
 CALL HOR_INTERPOL(ILUOUT,ZSEA1,ZSEA2)
 CALL HOR_INTERPOL(ILUOUT,ZNATURE1,ZNATURE2)
 CALL HOR_INTERPOL(ILUOUT,ZWATER1,ZWATER2)
 CALL HOR_INTERPOL(ILUOUT,ZTOWN1,ZTOWN2)
!
DEALLOCATE(ZSEA1)
DEALLOCATE(ZNATURE1)
DEALLOCATE(ZWATER1)
DEALLOCATE(ZTOWN1)
!
ALLOCATE(XSEA   (IL))
ALLOCATE(XNATURE(IL))
ALLOCATE(XWATER (IL))
ALLOCATE(XTOWN  (IL))
!
XSEA(:)   = ZSEA2   (:,1)
XNATURE(:)= ZNATURE2(:,1)
XWATER(:) = ZWATER2 (:,1)
XTOWN(:)  = ZTOWN2  (:,1)
!
DEALLOCATE(ZSEA2)
DEALLOCATE(ZNATURE2)
DEALLOCATE(ZWATER2)
DEALLOCATE(ZTOWN2)
!
 CALL CLEAN_PREP_OUTPUT_GRID
!------------------------------------------------------------------------------
!
!*      5.     Coherence check
!              ---------------
! 
ALLOCATE(ZSUM(IL))
ZSUM = 0.
DO JCOVER=1,JPCOVER
  ZSUM(:) = ZSUM(:) + XCOVER(:,JCOVER)
END DO
!
DO JCOVER=1,JPCOVER
  XCOVER(:,JCOVER) = XCOVER(:,JCOVER)/ZSUM(:)
END DO
!
DO JCOVER=1,JPCOVER
  IF (ALL(XCOVER(:,JCOVER)==0.)) LCOVER(JCOVER) = .FALSE.
END DO
!------------------------------------------------------------------------------
!
!*      6.     Fractions
!              ---------
!
! When the model runs in multiproc, NSIZE* represents the number of points
! on a proc, and NDIM* the total number of points on all procs.
! The following definition of NDIM* won't be correct any more when the PGD
! runs in multiproc.
!
NSIZE_NATURE    = COUNT(XNATURE(:) > 0.0)
NSIZE_WATER     = COUNT(XWATER (:) > 0.0)
NSIZE_SEA       = COUNT(XSEA   (:) > 0.0)
NSIZE_TOWN      = COUNT(XTOWN  (:) > 0.0)
NSIZE_FULL      = IL
!
NDIM_NATURE    = SUM_ON_ALL_PROCS(HPROGRAM,CGRID,XNATURE(:) > 0., 'DIM')
NDIM_WATER     = SUM_ON_ALL_PROCS(HPROGRAM,CGRID,XWATER (:) > 0., 'DIM')
NDIM_SEA       = SUM_ON_ALL_PROCS(HPROGRAM,CGRID,XSEA   (:) > 0., 'DIM')
NDIM_TOWN      = SUM_ON_ALL_PROCS(HPROGRAM,CGRID,XTOWN  (:) > 0., 'DIM')
ZSUM=1.
NDIM_FULL      = SUM_ON_ALL_PROCS(HPROGRAM,CGRID,ZSUM   (:) ==1., 'DIM')
DEALLOCATE(ZSUM)
!
ALLOCATE(NR_NATURE (NSIZE_NATURE))
ALLOCATE(NR_TOWN   (NSIZE_TOWN  ))
ALLOCATE(NR_WATER  (NSIZE_WATER ))
ALLOCATE(NR_SEA    (NSIZE_SEA   ))
!
IF (NSIZE_SEA   >0)CALL GET_1D_MASK( NSIZE_SEA,    NSIZE_FULL, XSEA   , NR_SEA   )
IF (NSIZE_WATER >0)CALL GET_1D_MASK( NSIZE_WATER,  NSIZE_FULL, XWATER , NR_WATER )
IF (NSIZE_TOWN  >0)CALL GET_1D_MASK( NSIZE_TOWN,   NSIZE_FULL, XTOWN  , NR_TOWN  )
IF (NSIZE_NATURE>0)CALL GET_1D_MASK( NSIZE_NATURE, NSIZE_FULL, XNATURE, NR_NATURE)
IF (LHOOK) CALL DR_HOOK('ZOOM_PGD_COVER',1,ZHOOK_HANDLE)

!_______________________________________________________________________________
!
END SUBROUTINE ZOOM_PGD_COVER
