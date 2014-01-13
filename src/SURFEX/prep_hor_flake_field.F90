!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
SUBROUTINE PREP_HOR_FLAKE_FIELD(HPROGRAM,HSURF,HATMFILE,HATMFILETYPE,&
                                HPGDFILE,HPGDFILETYPE,ONOVALUE)
!     #################################################################################
!
!!****  *PREP_HOR_FLAKE_FIELD* - Reads, interpolates and prepares a water field
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
!!     S. Malardel
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2004
!!      P. Le Moigne 10/2005, Phasage Arome
!!      E. Kourzeneva 09/2010, Make possible to interpolate 
!!                             only lake surface temperature, 
!!                             but not profiles
!!------------------------------------------------------------------
!
!
!
USE MODD_SURF_PAR,     ONLY : XUNDEF
USE MODD_PREP,         ONLY : CINGRID_TYPE, CINTERP_TYPE, XZS_LS, XLAT_OUT, XLON_OUT, &
                               XX_OUT, XY_OUT, CMASK
USE MODD_FLAKE_n,      ONLY : XZS, XTS, XT_SNOW, XT_ICE, XT_MNW, XT_WML, &
                              XT_BOT, XT_B1,   &
                              XCT, XH_SNOW, XH_ICE, XH_ML, XH_B1,   &
! Water depth is needed in order to compute XCT. 
!salgado - check if XWATER_DEPTH is initialized during prep!
                            XWATER_DEPTH
!
USE MODD_FLAKE_GRID_n, ONLY : XLAT, XLON
!
USE MODD_CSTS,       ONLY : XTT
USE MODD_PREP_FLAKE, ONLY : LCLIM_LAKE
!
USE MODI_READ_PREP_FLAKE_CONF
USE MODI_PREP_FLAKE_GRIB
USE MODI_PREP_FLAKE_ASCLLV
USE MODI_PREP_FLAKE_UNIF
USE MODI_PREP_FLAKE_BUFFER
USE MODI_HOR_INTERPOL
USE MODI_GET_LUOUT
USE MODI_PREP_FLAKE_EXTERN
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABOR1_SFX
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
 CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
 CHARACTER(LEN=7),   INTENT(IN)  :: HSURF     ! type of field
 CHARACTER(LEN=28),  INTENT(IN)  :: HATMFILE    ! name of the Atmospheric file
 CHARACTER(LEN=6),   INTENT(IN)  :: HATMFILETYPE! type of the Atmospheric file
 CHARACTER(LEN=28),  INTENT(IN)  :: HPGDFILE    ! name of the Atmospheric file
 CHARACTER(LEN=6),   INTENT(IN)  :: HPGDFILETYPE! type of the Atmospheric file
LOGICAL, OPTIONAL, INTENT(OUT) :: ONOVALUE  ! flag for the not given value
!
!
!*      0.2    declarations of local variables
!
 CHARACTER(LEN=6)              :: YFILETYPE ! type of input file
 CHARACTER(LEN=28)             :: YFILE     ! name of file
 CHARACTER(LEN=6)              :: YFILEPGDTYPE ! type of input file
 CHARACTER(LEN=28)             :: YFILEPGD     ! name of file
REAL, POINTER, DIMENSION(:,:) :: ZFIELDIN  ! field to interpolate horizontally
REAL, ALLOCATABLE, DIMENSION(:,:) :: ZFIELDOUT ! field interpolated   horizontally
INTEGER                       :: ILUOUT    ! output listing logical unit
!
LOGICAL                       :: GUNIF     ! flag for prescribed uniform field
LOGICAL                       :: GDEFAULT  ! flag for prescribed default field
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------------
!
!
!*      1.     Reading of input file name and type
!
IF (LHOOK) CALL DR_HOOK('PREP_HOR_FLAKE_FIELD',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
 CALL READ_PREP_FLAKE_CONF(HPROGRAM,HSURF,YFILE,YFILETYPE,YFILEPGD,YFILEPGDTYPE,&
                          HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE,ILUOUT,GUNIF)
!
CMASK = 'WATER'
!
GDEFAULT = (YFILETYPE=='      ' .OR. (HSURF(1:2)/='ZS' .AND. HSURF(1:2)/='TS' &
                .AND. SIZE(XLAT).NE.1)) .AND. .NOT.GUNIF
IF (PRESENT(ONOVALUE)) ONOVALUE = GDEFAULT
!
IF (.NOT. GDEFAULT) THEN
!
!-------------------------------------------------------------------------------------
!
!*      2.     Reading of input  configuration (Grid and interpolation type)
!
  IF (GUNIF) THEN
    CALL PREP_FLAKE_UNIF(ILUOUT,HSURF,ZFIELDIN)
  ELSE IF (YFILETYPE=='ASCLLV') THEN
    CALL PREP_FLAKE_ASCLLV(HPROGRAM,HSURF,ILUOUT,ZFIELDIN)
  ELSE IF (YFILETYPE=='GRIB  ') THEN
    CALL PREP_FLAKE_GRIB(HPROGRAM,HSURF,YFILE,ILUOUT,ZFIELDIN)
  ELSE IF (YFILETYPE=='MESONH' .OR. YFILETYPE=='ASCII ' .OR. YFILETYPE=='LFI   ') THEN
    CALL PREP_FLAKE_EXTERN(HPROGRAM,HSURF,YFILE,YFILETYPE,YFILEPGD,YFILEPGDTYPE,ILUOUT,ZFIELDIN)
  ELSE IF (YFILETYPE=='BUFFER') THEN
    CALL PREP_FLAKE_BUFFER(HPROGRAM,HSURF,ILUOUT,ZFIELDIN)
  ELSE
    CALL ABOR1_SFX('PREP_HOR_FLAKE_FIELD: data file type not supported : '//YFILETYPE)
  END IF
!
!
!*      4.     Horizontal interpolation
!
  !ALLOCATE(ZFIELDOUT(SIZE(XLAT),SIZE(ZFIELDIN,2)))
  ALLOCATE(ZFIELDOUT(SIZE(XLAT),1))
!
!Impossible to interpolate lake profiles, only the lake surface temperature! 
!But in uniform case and 1 point case
  IF(GUNIF .OR. SIZE(XLAT).EQ.1) THEN
    CALL HOR_INTERPOL(ILUOUT,ZFIELDIN,ZFIELDOUT)
  ELSE IF(HSURF(1:2)=='ZS' .OR. HSURF(1:2)=='TS') THEN
    WRITE(ILUOUT,*) "WARNING! Impossible to interpolate lake profiles in horisontal!"
    WRITE(ILUOUT,*) "So, interoplate only surface temperature and start from lakes mixed down to the bottom"
    CALL HOR_INTERPOL(ILUOUT,ZFIELDIN,ZFIELDOUT)
  END IF
!
!*      5.     Return to historical variable
!
  SELECT CASE (HSURF)
   CASE('ZS     ') 
    ALLOCATE(XZS_LS(SIZE(ZFIELDOUT,1)))
    XZS_LS(:) = ZFIELDOUT(:,1)
   CASE('TS     ')
    ALLOCATE(XTS(SIZE(ZFIELDOUT,1)))
    XTS(:) = ZFIELDOUT(:,1)
   CASE('T_SNOW ')
    ALLOCATE(XT_SNOW(SIZE(ZFIELDOUT,1)))
    XT_SNOW(:) = ZFIELDOUT(:,1)
   CASE('T_ICE  ')
    ALLOCATE(XT_ICE(SIZE(ZFIELDOUT,1)))
    XT_ICE(:) = ZFIELDOUT(:,1)
   CASE('T_WML  ')
    ALLOCATE(XT_WML(SIZE(ZFIELDOUT,1)))
    XT_WML(:) = ZFIELDOUT(:,1)
   CASE('T_BOT  ')
    ALLOCATE(XT_BOT(SIZE(ZFIELDOUT,1)))
    XT_BOT(:) = ZFIELDOUT(:,1)
   CASE('T_B1   ')
    ALLOCATE(XT_B1(SIZE(ZFIELDOUT,1)))
    XT_B1(:) = ZFIELDOUT(:,1)
   CASE('CT     ')
    ALLOCATE(XCT(SIZE(ZFIELDOUT,1)))
    XCT(:) = ZFIELDOUT(:,1)
   CASE('H_SNOW ')
    ALLOCATE(XH_SNOW(SIZE(ZFIELDOUT,1)))
    XH_SNOW(:) = ZFIELDOUT(:,1)
   CASE('H_ICE  ')
    ALLOCATE(XH_ICE(SIZE(ZFIELDOUT,1)))
    XH_ICE(:) = ZFIELDOUT(:,1)
   CASE('H_ML   ')
    ALLOCATE(XH_ML(SIZE(ZFIELDOUT,1)))
    XH_ML(:) = ZFIELDOUT(:,1)
   CASE('H_B1   ')
    ALLOCATE(XH_B1(SIZE(ZFIELDOUT,1)))
    XH_B1(:) = ZFIELDOUT(:,1)
  END SELECT
!*      6.     Deallocations
!
  IF (ALL(ZFIELDOUT==XUNDEF)) GDEFAULT = .TRUE.
!
  DEALLOCATE(ZFIELDIN )
  DEALLOCATE(ZFIELDOUT)
!
END IF

!
IF (GDEFAULT) THEN
!
!*      7.    Initial values of FLAKE variables are computed from TS
!             when uniform values are not prescribed 
  IF (HSURF(1:2)/='ZS') WRITE(ILUOUT,*) 'NO FILE FOR FIELD ',HSURF, &
                                        ': UNIFORM DEFAULT FIELD IS PRESCRIBED'
  
END IF
!
IF (LHOOK) CALL DR_HOOK('PREP_HOR_FLAKE_FIELD',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE PREP_HOR_FLAKE_FIELD
