!     ###########################################################
      SUBROUTINE ZOOM_PGD_ISBA(HPROGRAM,HINIFILE,HINIFILETYPE,HFILE,HFILETYPE,OECOCLIMAP)
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
!!    B. Decharme      2008  XWDRAIN
!!    M.Tomasini    17/04/12  Add interpolation for ISBA variables (MODD_DATA_ISBA_n)
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_SURF_PAR, ONLY : XUNDEF
USE MODD_DATA_COVER_PAR, ONLY : JPCOVER
USE MODD_ISBA_n,         ONLY : NPATCH, NGROUND_LAYER, CISBA,            &
                                  CPEDOTF, XCOVER, LCOVER, XZS,          &
                                  XZ0EFFJPDIR, CPHOTO, NNBIOMASS,        &
                                  XSAND, XCLAY, XRUNOFFB, XWDRAIN,       &
                                  LECOCLIMAP, LTR_ML, XSOILGRID
USE MODD_ISBA_GRID_n,    ONLY : CGRID, XGRID_PAR, XLAT, XLON, XMESH_SIZE
USE MODD_ISBA_PAR,    ONLY : XOPTIMGRID
USE MODD_PREP,           ONLY : CINGRID_TYPE, CINTERP_TYPE
!
USE MODI_GET_LUOUT
USE MODI_OPEN_AUX_IO_SURF
USE MODI_READ_SURF
USE MODI_CLOSE_AUX_IO_SURF
USE MODI_GET_SURF_SIZE_n
USE MODI_PACK_PGD
USE MODI_ZOOM_PGD_ISBA_FULL
USE MODI_GET_AOS_n
USE MODI_GET_SSO_n
USE MODI_PACK_PGD_ISBA
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
 CHARACTER(LEN=28),    INTENT(IN)  :: HFILE       ! output file name
 CHARACTER(LEN=6),     INTENT(IN)  :: HFILETYPE   ! output file type
LOGICAL,              INTENT(IN)  :: OECOCLIMAP  ! flag to use ecoclimap
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER :: IVERSION, IBUGFIX
INTEGER :: IRESP
INTEGER :: ILUOUT
INTEGER :: IL      ! total 1D dimension (output grid, total surface)
INTEGER :: ILU     ! total 1D dimension (output grid, ISBA points only)
REAL, DIMENSION(:), ALLOCATABLE   :: ZAOSIP    ! A/S i+ on all surface points
REAL, DIMENSION(:), ALLOCATABLE   :: ZAOSIM    ! A/S i- on all surface points
REAL, DIMENSION(:), ALLOCATABLE   :: ZAOSJP    ! A/S j+ on all surface points
REAL, DIMENSION(:), ALLOCATABLE   :: ZAOSJM    ! A/S j- on all surface points
REAL, DIMENSION(:), ALLOCATABLE   :: ZHO2IP    ! h/2 i+ on all surface points
REAL, DIMENSION(:), ALLOCATABLE   :: ZHO2IM    ! h/2 i- on all surface points
REAL, DIMENSION(:), ALLOCATABLE   :: ZHO2JP    ! h/2 j+ on all surface points
REAL, DIMENSION(:), ALLOCATABLE   :: ZHO2JM    ! h/2 j- on all surface points
REAL, DIMENSION(:), ALLOCATABLE   :: ZSSO_SLOPE! subgrid slope on all surface points
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!------------------------------------------------------------------------------
IF (LHOOK) CALL DR_HOOK('ZOOM_PGD_ISBA',0,ZHOOK_HANDLE)
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
 CALL READ_SURF(HINIFILETYPE,'VERSION',IVERSION,IRESP)
 CALL READ_SURF(HINIFILETYPE,'BUG',IBUGFIX,IRESP) 
 CALL READ_SURF(HINIFILETYPE,'PATCH_NUMBER',NPATCH,IRESP)
 CALL READ_SURF(HINIFILETYPE,'GROUND_LAYER',NGROUND_LAYER,IRESP)
 CALL READ_SURF(HINIFILETYPE,'ISBA',CISBA,IRESP)
IF (IVERSION >= 7) THEN
  CALL READ_SURF(HINIFILETYPE,'PEDOTF',CPEDOTF,IRESP)
ELSE
  CPEDOTF = 'CH78'
ENDIF
 CALL READ_SURF(HINIFILETYPE,'PHOTO',CPHOTO,IRESP)
!
IF (IVERSION>7 .OR. IVERSION==7 .AND. IBUGFIX>=2) THEN
  !
  CALL READ_SURF(HINIFILETYPE,'TR_ML',LTR_ML,IRESP)
  !
ELSE 
  LTR_ML = .FALSE.
ENDIF
!
IF(CISBA=='DIF') THEN
  ALLOCATE(XSOILGRID(NGROUND_LAYER))
  XSOILGRID=XUNDEF
  IF (IVERSION>7 .OR. IVERSION==7 .AND. IBUGFIX>=2) THEN
    CALL READ_SURF(HINIFILETYPE,'SOILGRID',XSOILGRID,IRESP,HDIR='-')
  ELSE
    XSOILGRID(1:NGROUND_LAYER)=XOPTIMGRID(1:NGROUND_LAYER)
  ENDIF
ELSE
  ALLOCATE(XSOILGRID(0))
ENDIF
!
!* number of biomass pools
!
IF (IVERSION>=6) THEN
  CALL READ_SURF(HPROGRAM,'NBIOMASS',NNBIOMASS,IRESP)
ELSE
  SELECT CASE (CPHOTO)
    CASE ('AGS','LAI','AST','LST')
      NNBIOMASS = 1
    CASE ('NIT')
      NNBIOMASS = 3
    CASE ('NCB')
      NNBIOMASS = 6
  END SELECT
ENDIF
!
 CALL CLOSE_AUX_IO_SURF(HINIFILE,HINIFILETYPE)
!
!------------------------------------------------------------------------------
LECOCLIMAP = OECOCLIMAP
!
!-------------------------------------------------------------------------------
!
!*    7.      Number of points and packing of general fields
!             ----------------------------------------------
!
!
 CALL GET_SURF_SIZE_n('NATURE',ILU)
!
ALLOCATE(LCOVER     (JPCOVER))
ALLOCATE(XCOVER     (ILU,JPCOVER))
ALLOCATE(XZS        (ILU))
ALLOCATE(XLAT       (ILU))
ALLOCATE(XLON       (ILU))
ALLOCATE(XMESH_SIZE (ILU))
ALLOCATE(XZ0EFFJPDIR(ILU))
!
 CALL PACK_PGD(HPROGRAM, 'NATURE',                    &
                CGRID,  XGRID_PAR,                     &
                LCOVER, XCOVER, XZS,                   &
                XLAT, XLON, XMESH_SIZE, XZ0EFFJPDIR    )  
!
!------------------------------------------------------------------------------
!
!*      3.     Reading of sand, clay, runoffb, wdrain and interpolations
!              --------------------------------------------------
!
ALLOCATE(XSAND(ILU,NGROUND_LAYER))
ALLOCATE(XCLAY(ILU,NGROUND_LAYER))
ALLOCATE(XRUNOFFB(ILU))
ALLOCATE(XWDRAIN (ILU))
 CALL ZOOM_PGD_ISBA_FULL(HPROGRAM,HINIFILE,HINIFILETYPE)
!
!-------------------------------------------------------------------------------
!
!*    8.      Packing of ISBA specific fields
!             -------------------------------
!
 CALL GET_SURF_SIZE_n('FULL  ',IL)
!
ALLOCATE(ZAOSIP(IL))
ALLOCATE(ZAOSIM(IL))
ALLOCATE(ZAOSJP(IL))
ALLOCATE(ZAOSJM(IL))
ALLOCATE(ZHO2IP(IL))
ALLOCATE(ZHO2IM(IL))
ALLOCATE(ZHO2JP(IL))
ALLOCATE(ZHO2JM(IL))
ALLOCATE(ZSSO_SLOPE(IL))

 CALL GET_AOS_n(HPROGRAM,IL,ZAOSIP,ZAOSIM,ZAOSJP,ZAOSJM,ZHO2IP,ZHO2IM,ZHO2JP,ZHO2JM)
 CALL GET_SSO_n(HPROGRAM,IL,ZSSO_SLOPE)

 CALL PACK_PGD_ISBA(HPROGRAM,                                    &
                     ZAOSIP, ZAOSIM, ZAOSJP, ZAOSJM,              &
                     ZHO2IP, ZHO2IM, ZHO2JP, ZHO2JM,              &
                     ZSSO_SLOPE                                   )  
!
DEALLOCATE(ZAOSIP)
DEALLOCATE(ZAOSIM)
DEALLOCATE(ZAOSJP)
DEALLOCATE(ZAOSJM)
DEALLOCATE(ZHO2IP)
DEALLOCATE(ZHO2IM)
DEALLOCATE(ZHO2JP)
DEALLOCATE(ZHO2JM)
DEALLOCATE(ZSSO_SLOPE)
IF (LHOOK) CALL DR_HOOK('ZOOM_PGD_ISBA',1,ZHOOK_HANDLE)
!-------------------------------------------------------------------------------
!
END SUBROUTINE ZOOM_PGD_ISBA
