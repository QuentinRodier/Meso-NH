!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     ###########################################################
      SUBROUTINE ZOOM_PGD_TEB(HPROGRAM,HINIFILE,HINIFILETYPE,OECOCLIMAP,OGARDEN)
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
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_SURF_PAR,        ONLY : XUNDEF
!
USE MODD_DATA_COVER_PAR,  ONLY : JPCOVER
USE MODD_TEB_GRID_n,      ONLY : XLAT, XLON, CGRID, XGRID_PAR,          &
                                 XMESH_SIZE, NDIM
USE MODD_TEB_n,           ONLY : XCOVER, LCOVER, XZS,                   &
                                 NROOF_LAYER, NROAD_LAYER, NWALL_LAYER, &
                                 LECOCLIMAP, LGARDEN, NTEB_PATCH,       &
                                 CBEM, CBLD_ATYPE
USE MODD_BEM_n,           ONLY : NFLOOR_LAYER
!
USE MODD_PREP,            ONLY : CINGRID_TYPE, CINTERP_TYPE, LINTERP
!
USE MODI_GET_LUOUT
USE MODI_ABOR1_SFX
USE MODI_OPEN_AUX_IO_SURF
USE MODI_GET_SURF_SIZE_n
USE MODI_PACK_PGD
USE MODI_PREP_GRID_EXTERN
USE MODI_PREP_OUTPUT_GRID
USE MODI_READ_SURF
USE MODI_READ_PGD_TEB_PAR_n
USE MODI_CLOSE_AUX_IO_SURF
USE MODI_CLEAN_PREP_OUTPUT_GRID
USE MODI_GOTO_TEB
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
!
IMPLICIT NONE
!
!*    0.1    Declaration of dummy arguments
!            ------------------------------
!
 CHARACTER(LEN=6),     INTENT(IN)  :: HPROGRAM    ! program calling
 CHARACTER(LEN=28),    INTENT(IN)  :: HINIFILE    ! file to read
 CHARACTER(LEN=6),     INTENT(IN)  :: HINIFILETYPE! file type
LOGICAL,              INTENT(IN)  :: OECOCLIMAP  ! flag to use ecoclimap
LOGICAL,              INTENT(IN)  :: OGARDEN     ! flag to use garden
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER :: IRESP   ! error return code
INTEGER :: ILUOUT  ! output listing logical unit
INTEGER :: INI     ! total 1D dimension (input grid)
INTEGER :: JLAYER  ! loop counter
INTEGER :: ILU     ! total 1D dimension (output grid, TOWN points only)
INTEGER :: JPATCH  ! TEB patch
REAL(KIND=JPRB) :: ZHOOK_HANDLE
INTEGER           :: IVERSION
INTEGER           :: IBUGFIX
!------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('ZOOM_PGD_TEB',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
LECOCLIMAP = OECOCLIMAP
LGARDEN = OGARDEN
!
IF (.NOT. OECOCLIMAP) THEN
  WRITE(ILUOUT,*) 'ERROR'
  WRITE(ILUOUT,*) 'Ecoclimap is not used'
  WRITE(ILUOUT,*) 'Routine zoom_pgd_teb.f90 must be updated'
  WRITE(ILUOUT,*) 'to interpolate all TEB physiographic fields'
  CALL ABOR1_SFX('ZOOM_PGD_TEB: ECOCLIMAP NOT USED, ROUTINE MUST BE UPDATED')
END IF
!
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
 CALL GOTO_TEB(1)
!-------------------------------------------------------------------------------
!
!*    2.      Number of points and packing of general fields
!             ----------------------------------------------
!
!
 CALL GET_SURF_SIZE_n('TOWN  ',ILU)
!
ALLOCATE(LCOVER     (JPCOVER))
ALLOCATE(XCOVER     (ILU,JPCOVER))
ALLOCATE(XZS        (ILU))
ALLOCATE(XLAT       (ILU))
ALLOCATE(XLON       (ILU))
ALLOCATE(XMESH_SIZE (ILU))
!
 CALL PACK_PGD(HPROGRAM, 'TOWN  ',                      &
                CGRID,  XGRID_PAR,                     &
                LCOVER, XCOVER, XZS,                   &
                XLAT, XLON, XMESH_SIZE                 )  
!
NDIM = ILU
!
!
 CALL READ_SURF(HPROGRAM,'VERSION',IVERSION,IRESP)
 CALL READ_SURF(HPROGRAM,'BUG',IBUGFIX,IRESP)
!------------------------------------------------------------------------------
!
!*      3.     Reading of grid
!              ---------------
!
 CALL PREP_GRID_EXTERN(HINIFILETYPE,ILUOUT,CINGRID_TYPE,CINTERP_TYPE,INI)
!
 CALL PREP_OUTPUT_GRID(ILUOUT,CGRID,XGRID_PAR,XLAT,XLON)
!
!
!------------------------------------------------------------------------------
!
!*      4.     Reading & interpolation of fields
!              ---------------------------------
!
!
IF (IVERSION<7 .OR. IVERSION==7 .AND. IBUGFIX<=2) THEN
  NTEB_PATCH=1
ELSE
  CALL READ_SURF(HPROGRAM,'TEB_PATCH',NTEB_PATCH,IRESP)
END IF

!
 CALL READ_SURF(HPROGRAM,'ROOF_LAYER',NROOF_LAYER,IRESP)
 CALL READ_SURF(HPROGRAM,'ROAD_LAYER',NROAD_LAYER,IRESP)
 CALL READ_SURF(HPROGRAM,'WALL_LAYER',NWALL_LAYER,IRESP)
!
IF (IVERSION<7 .OR.( IVERSION==7 .AND. IBUGFIX<=2)) THEN
  CBLD_ATYPE='ARI'
  CBEM = 'DEF'
ELSE
  CALL READ_SURF(HPROGRAM,'BLD_ATYPE' ,CBLD_ATYPE,IRESP)
  CALL READ_SURF(HPROGRAM,'BEM'       ,CBEM      ,IRESP)
END IF
!
IF (CBEM/='DEF') THEN
  CALL READ_SURF(HPROGRAM,'FLOOR_LAYER',NFLOOR_LAYER,IRESP)
END IF
!
DO JPATCH=1,NTEB_PATCH
  CALL GOTO_TEB(JPATCH)
  CALL READ_PGD_TEB_PAR_n(HPROGRAM,INI,'A')
!
!------------------------------------------------------------------------------
!
!*      5.     Gardens
!              -------
!
  IF (LGARDEN) CALL ZOOM_PGD_TEB_GARDEN
END DO
!
 CALL CLOSE_AUX_IO_SURF(HINIFILE,HINIFILETYPE)
!
 CALL CLEAN_PREP_OUTPUT_GRID
!
!------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('ZOOM_PGD_TEB',1,ZHOOK_HANDLE)
!------------------------------------------------------------------------------
!
CONTAINS
!
SUBROUTINE ZOOM_PGD_TEB_GARDEN
!
USE MODI_HOR_INTERPOL
!
USE MODD_ISBA_PAR,     ONLY : XOPTIMGRID
USE MODD_TEB_VEG_n,    ONLY : CPHOTO, CISBA,                 &
                              CPEDOTF, NNBIOMASS
USE MODD_TEB_GARDEN_n, ONLY : NGROUND_LAYER,                 &
                              XSAND, XCLAY,                  &
                              XWDRAIN, XRUNOFFB, LPAR_GARDEN,&
                              XSOILGRID
!
IMPLICIT NONE
!
REAL, DIMENSION(:,:), POINTER     :: ZIN     ! field  on all surface points
!
REAL, DIMENSION(INI)              :: ZFIELD  ! field read
REAL, DIMENSION(ILU,1)            :: ZOUT    ! final field
REAL(KIND=JPRB) :: ZHOOK_HANDLE
 CHARACTER(LEN=12) :: YRECFM         ! Name of the article to be read
!
IF (LHOOK) CALL DR_HOOK('ZOOM_PGD_TEB:ZOOM_PGD_TEB_GARDEN',0,ZHOOK_HANDLE)
!
LINTERP(:) = .TRUE.
!
IF (IVERSION>7 .OR. IVERSION==7 .AND. IBUGFIX>=3) THEN
  CALL READ_SURF(HPROGRAM,'GD_LAYER',NGROUND_LAYER,IRESP)
  CALL READ_SURF(HPROGRAM,'GD_ISBA',CISBA,IRESP)
  CALL READ_SURF(HPROGRAM,'GD_PHOTO',CPHOTO,IRESP)
  CALL READ_SURF(HPROGRAM,'GD_PEDOTF',CPEDOTF,IRESP)
  NNBIOMASS=1
  IF (CPHOTO=='NIT') NNBIOMASS=3  
ELSE
  CALL READ_SURF(HPROGRAM,'TWN_LAYER',NGROUND_LAYER,IRESP)
  CALL READ_SURF(HPROGRAM,'TWN_ISBA',CISBA,IRESP)
  CALL READ_SURF(HPROGRAM,'TWN_PHOTO',CPHOTO,IRESP)
  CALL READ_SURF(HPROGRAM,'TWN_PEDOTF',CPEDOTF,IRESP)
  CALL READ_SURF(HPROGRAM,'TWN_NBIOMASS',NNBIOMASS,IRESP)
ENDIF
!
!* sand
!
ALLOCATE(ZIN(INI,NGROUND_LAYER))
YRECFM='TWN_SAND'
IF (IVERSION>7 .OR. IVERSION==7 .AND. IBUGFIX>=3) YRECFM='GD_SAND'
 CALL READ_SURF(HPROGRAM,YRECFM,ZFIELD,IRESP,HDIR='A')
DO JLAYER=1,NGROUND_LAYER
  ZIN(:,JLAYER) = ZFIELD(:)
END DO
ALLOCATE(XSAND(ILU,NGROUND_LAYER))
 CALL HOR_INTERPOL(ILUOUT,ZIN,XSAND)
DEALLOCATE(ZIN)
!
!* clay
!
ALLOCATE(ZIN(INI,NGROUND_LAYER))
YRECFM='TWN_CLAY'
IF (IVERSION>7 .OR. IVERSION==7 .AND. IBUGFIX>=3) YRECFM='GD_CLAY'
 CALL READ_SURF(HPROGRAM,YRECFM,ZFIELD,IRESP,HDIR='A')
DO JLAYER=1,NGROUND_LAYER
  ZIN(:,JLAYER) = ZFIELD(:)
END DO
ALLOCATE(XCLAY(ILU,NGROUND_LAYER))
 CALL HOR_INTERPOL(ILUOUT,ZIN,XCLAY)
DEALLOCATE(ZIN)
!
!* runoff & drainage
!
ALLOCATE(ZIN(INI,1))
YRECFM='TWN_RUNOFFB'
IF (IVERSION>7 .OR. IVERSION==7 .AND. IBUGFIX>=3) YRECFM='GD_RUNOFFB'
CALL READ_SURF(HPROGRAM,YRECFM,ZFIELD,IRESP,HDIR='A')
ZIN(:,1) = ZFIELD(:)
ALLOCATE(XRUNOFFB(ILU))
 CALL HOR_INTERPOL(ILUOUT,ZIN,ZOUT)
XRUNOFFB(:) = ZOUT(:,1)
!
IF (IVERSION<=3) THEN
  XWDRAIN = 0.
ELSE
 YRECFM='TWN_WDRAIN'
 IF (IVERSION>7 .OR. IVERSION==7 .AND. IBUGFIX>=3) YRECFM='GD_WDRAIN'
 CALL READ_SURF(HPROGRAM,YRECFM,ZFIELD,IRESP,HDIR='A')
 ZIN(:,1) = ZFIELD(:)
 ALLOCATE(XWDRAIN(ILU))
 CALL HOR_INTERPOL(ILUOUT,ZIN,ZOUT)
 XWDRAIN(:) = ZOUT(:,1)
ENDIF
!
DEALLOCATE(ZIN)
!
IF(CISBA=='DIF') THEN
  ALLOCATE(XSOILGRID(NGROUND_LAYER))
  XSOILGRID=XUNDEF
  IF (IVERSION>7 .OR. IVERSION==7 .AND. IBUGFIX>=2) THEN
    CALL READ_SURF(HPROGRAM,'GD_SOILGRID',XSOILGRID,IRESP,HDIR='-')
  ELSE
    XSOILGRID(1:NGROUND_LAYER)=XOPTIMGRID(1:NGROUND_LAYER)
  ENDIF
ELSE
  ALLOCATE(XSOILGRID(0))
ENDIF
!
!
!* other garden parameters
!
 CALL READ_SURF(HPROGRAM,'PAR_GARDEN',LPAR_GARDEN,IRESP)
!
!!
IF (LPAR_GARDEN) THEN
  WRITE(ILUOUT,*) 'ERROR'
  WRITE(ILUOUT,*) 'Specific garden fields are prescribed'
  WRITE(ILUOUT,*) 'Routine zoom_pgd_teb.f90 must be updated'
  WRITE(ILUOUT,*) 'to interpolate all TEB physiographic garden fields'
  CALL ABOR1_SFX('ZOOM_PGD_TEB: GARDEN fields used, ROUTINE MUST BE UPDATED')
END IF
!
IF (LHOOK) CALL DR_HOOK('ZOOM_PGD_TEB:ZOOM_PGD_TEB_GARDEN',1,ZHOOK_HANDLE)
!
END SUBROUTINE ZOOM_PGD_TEB_GARDEN
!_______________________________________________________________________________
!
END SUBROUTINE ZOOM_PGD_TEB
