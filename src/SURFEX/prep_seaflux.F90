!     #########
SUBROUTINE PREP_SEAFLUX(HPROGRAM,HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE)
!     #################################################################################
!
!!****  *PREP_SEAFLUX* - prepares variables for SEAFLUX scheme
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
!!      S. Riette   06/2009 PREP_SEAFLUX_SBL has no more argument
!!      Modified    07/2012, P. Le Moigne : CMO1D phasing
!!------------------------------------------------------------------
!
!
USE MODI_PREP_HOR_SEAFLUX_FIELD
USE MODI_PREP_VER_SEAFLUX
USE MODI_PREP_OUTPUT_GRID
USE MODI_PREP_SEAFLUX_SBL
USE MODI_GET_LUOUT
!
USE MODN_PREP_SEAFLUX
USE MODD_READ_NAMELIST,  ONLY : LNAM_READ
USE MODD_SEAFLUX_n,      ONLY : XZ0, XSST, LSBL, XZ0H, &
                                  LINTERPOL_SST,         &
                                  CINTERPOL_SST,         &
                                  XSST_MTH  
USE MODD_PREP,           ONLY : XZS_LS
USE MODD_SURF_ATM,       ONLY : LVERTSHIFT
USE MODD_OCEAN_n,        ONLY : LMERCATOR, LCURRENT
USE MODD_SEAFLUX_GRID_n, ONLY : CGRID, XGRID_PAR, XLAT, XLON
!
USE MODD_OCEAN_REL_n, ONLY : XTAU_REL,LREL_CUR,LREL_TS, LFLUX_NULL, &
                           XQCORR,LFLX_CORR,LDIAPYCNAL
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
INTEGER :: JMTH,INMTH
INTEGER :: ILUOUT
LOGICAL :: GFOUND         ! Return code when searching namelist
INTEGER :: ILUNAM         ! logical unit of namelist file
REAL(KIND=JPRB) :: ZHOOK_HANDLE

!-------------------------------------------------------------------------------------
!
!*      0.     Default of configuration
!
!
IF (LHOOK) CALL DR_HOOK('PREP_SEAFLUX',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
 CALL PREP_OUTPUT_GRID(ILUOUT,CGRID,XGRID_PAR,XLAT,XLON)
!
!-------------------------------------------------------------------------------------
!
!*      1.     Read namelist
!
LSBL = LSEA_SBL
LMERCATOR = LOCEAN_MERCATOR
LCURRENT  = LOCEAN_CURRENT
! Relaxation-forcing parameters
XTAU_REL   = XTIME_REL
XQCORR     = XCORFLX
!
LREL_CUR   = LCUR_REL
LREL_TS    = LTS_REL
LFLUX_NULL = LZERO_FLUX
LFLX_CORR  = LCORR_FLUX
LDIAPYCNAL = LDIAPYC
!
!-------------------------------------------------------------------------------------
!
!*      2.     Reading and horizontal interpolations
!
!
!*      2.0    Large scale orography
!
 CALL PREP_HOR_SEAFLUX_FIELD(HPROGRAM,'ZS     ',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE)
!
!*      2.1    Temperature
!
 CALL PREP_HOR_SEAFLUX_FIELD(HPROGRAM,'SST    ',HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE)
!
!
!*      2.2    Roughness
!
ALLOCATE(XZ0(SIZE(XSST)))
XZ0 = 0.001
!
ALLOCATE(XZ0H(SIZE(XSST)))
XZ0H = XZ0
!
!
!-------------------------------------------------------------------------------------
 CALL CLEAN_PREP_OUTPUT_GRID
!-------------------------------------------------------------------------------------
!
!*      3.     Vertical interpolations of all variables
!
IF(LVERTSHIFT)THEN
  CALL PREP_VER_SEAFLUX
ENDIF
!
DEALLOCATE(XZS_LS)
!-------------------------------------------------------------------------------------
!
!*      4.     Preparation of optional interpolation of monthly sst
!
LINTERPOL_SST=.FALSE.
IF(CINTERPOL_SST/='NONE  ')THEN
  LINTERPOL_SST=.TRUE.
ENDIF
!
IF(LINTERPOL_SST)THEN
!
! Precedent, Current and Next Monthly SST
  INMTH=3
! Precedent, Current and Next Annual Monthly SST
  IF(CINTERPOL_SST=='ANNUAL')INMTH=14
!
  ALLOCATE(XSST_MTH(SIZE(XSST),INMTH))
  DO JMTH=1,INMTH
     XSST_MTH(:,JMTH)=XSST(:)
  ENDDO
!
ENDIF
!
!-------------------------------------------------------------------------------------
!
!*      5.     Preparation of SBL air variables
!
!
IF (LSBL) CALL PREP_SEAFLUX_SBL()
IF (LHOOK) CALL DR_HOOK('PREP_SEAFLUX',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!
END SUBROUTINE PREP_SEAFLUX
