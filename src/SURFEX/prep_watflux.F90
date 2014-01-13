!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
SUBROUTINE PREP_WATFLUX(HPROGRAM,HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE)
!     #################################################################################
!
!!****  *PREP_WATFLUX* - prepares WATFLUX fields
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
!!      S. Riette   06/2009 PREP_WATFLUX_SBL has no more argument
!!------------------------------------------------------------------
!

!
USE MODI_PREP_HOR_WATFLUX_FIELD
USE MODI_PREP_VER_WATFLUX
USE MODI_PREP_OUTPUT_GRID
USE MODI_GET_LUOUT
USE MODI_PREP_WATFLUX_SBL
!
USE MODD_READ_NAMELIST,  ONLY : LNAM_READ
USE MODN_PREP_WATFLUX
USE MODD_PREP,           ONLY : XZS_LS
USE MODD_SURF_ATM,       ONLY : LVERTSHIFT
USE MODD_WATFLUX_n,      ONLY : XZ0, XTS, LSBL, &
                                  LINTERPOL_TS,  &
                                  CINTERPOL_TS,  &
                                  XTS_MTH  
USE MODD_WATFLUX_GRID_n, ONLY : CGRID, XGRID_PAR, XLAT, XLON

!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_CLEAN_PREP_OUTPUT_GRID
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
 CHARACTER(LEN=6),   INTENT(IN)  :: HPROGRAM  ! program calling surf. schemes
 CHARACTER(LEN=28),  INTENT(IN)  :: HATMFILE    ! name of the Atmospheric file
 CHARACTER(LEN=6),   INTENT(IN)  :: HATMFILETYPE! type of the Atmospheric file
 CHARACTER(LEN=28),  INTENT(IN)  :: HPGDFILE    ! name of the Atmospheric file
 CHARACTER(LEN=6),   INTENT(IN)  :: HPGDFILETYPE! type of the Atmospheric file
!
!*      0.2    declarations of local variables
!
INTEGER :: JMTH, INMTH
!
INTEGER :: ILUOUT
LOGICAL :: GFOUND         ! Return code when searching namelist
INTEGER :: ILUNAM         ! logical unit of namelist file
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------------
!
!*      1.     Default of configuration
!
!
IF (LHOOK) CALL DR_HOOK('PREP_WATFLUX',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
 CALL PREP_OUTPUT_GRID(ILUOUT,CGRID,XGRID_PAR,XLAT,XLON)
!
!-------------------------------------------------------------------------------------
!
!*      2.     Reading and horizontal interpolations
!
!
!*      2.0    Large scale orography
!
 CALL PREP_HOR_WATFLUX_FIELD(HPROGRAM,'ZS     ',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE)
!
!*      2.1    Temperature
!
 CALL PREP_HOR_WATFLUX_FIELD(HPROGRAM,'TSWATER',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE)
!
!*      2.2    Roughness
!
ALLOCATE(XZ0(SIZE(XTS)))
XZ0 = 0.001
!
!-------------------------------------------------------------------------------------
 CALL CLEAN_PREP_OUTPUT_GRID
!-------------------------------------------------------------------------------------
!
!*      3.     Vertical interpolations of all variables
!
IF(LVERTSHIFT)THEN
  CALL PREP_VER_WATFLUX
ENDIF
!
DEALLOCATE(XZS_LS)
!-------------------------------------------------------------------------------------
!
!*      4.     Preparation of optional interpolation of monthly ts water
!
LINTERPOL_TS=.FALSE.
IF(CINTERPOL_TS/='NONE  ')THEN
  LINTERPOL_TS=.TRUE.
ENDIF
!
IF(LINTERPOL_TS)THEN
!
! Precedent, Current and Next Monthly TS water
  INMTH=3
! Precedent, Current and Next Annual Monthly TS water
  IF(CINTERPOL_TS=='ANNUAL')INMTH=14
!
  ALLOCATE(XTS_MTH(SIZE(XTS),INMTH))
  DO JMTH=1,INMTH
     XTS_MTH(:,JMTH)=XTS(:)
  ENDDO
!
ENDIF
!
!-------------------------------------------------------------------------------------
!
!*      5.     Preparation of SBL air variables
!
LSBL = LWAT_SBL
IF (LSBL) CALL PREP_WATFLUX_SBL()
IF (LHOOK) CALL DR_HOOK('PREP_WATFLUX',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE PREP_WATFLUX
