!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
      SUBROUTINE PGD_SEAFLUX_PAR(HPROGRAM,OSST_DATA)
!     ##############################################################
!
!!**** *PGD_SEAFLUX_PAR* monitor for averaging and interpolations of sst
!!
!!    PURPOSE
!!    -------
!!
!!    METHOD
!!    ------
!!   
!
!!    EXTERNAL
!!    --------
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!    AUTHOR
!!    ------
!!
!!    P. Le Moigne        Meteo-France
!!
!!    MODIFICATION
!!    ------------
!!
!!    Original    09/2007
!!
!!
!----------------------------------------------------------------------------
!
!*    0.     DECLARATION
!            -----------
!
USE MODD_SURF_PAR,       ONLY : XUNDEF, NUNDEF
USE MODD_SEAFLUX_GRID_n, ONLY : NDIM
USE MODD_DATA_SEAFLUX_n, ONLY : XDATA_SST, TDATA_SST
!
USE MODD_PGDWORK,       ONLY : CATYPE
!
USE MODD_TYPE_DATE_SURF
!
USE MODI_GET_LUOUT
USE MODI_OPEN_NAMELIST
USE MODI_CLOSE_NAMELIST
USE MODI_PGD_FIELD
!
USE MODE_POS_SURF
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
USE MODI_ABOR1_SFX
!
IMPLICIT NONE
!
!*    0.1    Declaration of arguments
!            ------------------------
!
 CHARACTER(LEN=6),    INTENT(IN)    :: HPROGRAM     ! Type of program
LOGICAL         ,    INTENT(OUT)   :: OSST_DATA
!
!
!*    0.2    Declaration of local variables
!            ------------------------------
!
INTEGER               :: ILUOUT    ! output listing logical unit
INTEGER               :: ILUNAM    ! namelist file  logical unit
LOGICAL               :: GFOUND    ! true if namelist is found
!
INTEGER               :: JTIME     ! loop counter on time
!
!*    0.3    Declaration of namelists
!            ------------------------
!
INTEGER            :: NTIME
INTEGER, PARAMETER :: NTIME_MAX    = 365
!
REAL, DIMENSION(NTIME_MAX)     :: XUNIF_SST        ! sea surface temperature

INTEGER, DIMENSION(NTIME_MAX)  :: NYEAR_SST
INTEGER, DIMENSION(NTIME_MAX)  :: NMONTH_SST
INTEGER, DIMENSION(NTIME_MAX)  :: NDAY_SST
REAL, DIMENSION(NTIME_MAX)     :: XTIME_SST
LOGICAL                        :: LSST_DATA
!
! name of files containing data
!
 CHARACTER(LEN=28), DIMENSION(NTIME_MAX)   :: CFNAM_SST        ! sea surface temperature
 CHARACTER(LEN=6),  DIMENSION(NTIME_MAX)   :: CFTYP_SST        ! sea surface temperature
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
NAMELIST/NAM_DATA_SEAFLUX/NTIME, LSST_DATA, XUNIF_SST, CFNAM_SST, CFTYP_SST, &
                            NYEAR_SST, NMONTH_SST, NDAY_SST, XTIME_SST  
!-------------------------------------------------------------------------------
!
!*    1.      Initializations
!             ---------------
!
IF (LHOOK) CALL DR_HOOK('PGD_SEAFLUX_PAR',0,ZHOOK_HANDLE)
NTIME             = 12
XUNIF_SST (:)     = XUNDEF ! sea surface temperature
!
CFNAM_SST (:)     = '                            '
!
CFTYP_SST (:)     = '      '
!
NYEAR_SST  (:)    = NUNDEF
NMONTH_SST (:)    = NUNDEF
NDAY_SST   (:)    = NUNDEF
XTIME_SST  (:)    = XUNDEF
!
LSST_DATA         = .FALSE.
!-------------------------------------------------------------------------------
!
!*    2.      Input file for cover types
!             --------------------------
!
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
 CALL OPEN_NAMELIST(HPROGRAM,ILUNAM)
!
 CALL POSNAM(ILUNAM,'NAM_DATA_SEAFLUX',GFOUND,ILUOUT)
IF (GFOUND) READ(UNIT=ILUNAM,NML=NAM_DATA_SEAFLUX)
!
 CALL CLOSE_NAMELIST(HPROGRAM,ILUNAM)
!
OSST_DATA         = LSST_DATA
IF (.NOT. LSST_DATA .AND. LHOOK) CALL DR_HOOK('PGD_SEAFLUX_PAR',1,ZHOOK_HANDLE)
IF (.NOT. LSST_DATA) RETURN
!
IF (NTIME > NTIME_MAX) THEN
   WRITE(ILUOUT,*)'NTIME SHOULD NOT EXCEED',NTIME_MAX
   CALL ABOR1_SFX('PGD_SEAFLUX_PAR: NTIME TOO BIG')
ENDIF
ALLOCATE(XDATA_SST     (NDIM,NTIME))
ALLOCATE(TDATA_SST     (NTIME))
!
!-------------------------------------------------------------------------------
!
!*    3.      Uniform fields are prescribed
!             -----------------------------
!
CATYPE = 'ARI'
!
DO JTIME=1,NTIME
  CALL PGD_FIELD(HPROGRAM,'SST: sea surface temperature','SEA',CFNAM_SST(JTIME),   &
                   CFTYP_SST(JTIME),XUNIF_SST(JTIME),XDATA_SST(:,JTIME))  
!                 
  TDATA_SST(JTIME)%TDATE%YEAR  = NYEAR_SST(JTIME)
  TDATA_SST(JTIME)%TDATE%MONTH = NMONTH_SST(JTIME)
  TDATA_SST(JTIME)%TDATE%DAY   = NDAY_SST(JTIME)
  TDATA_SST(JTIME)%TIME        = XTIME_SST(JTIME)
!  
END DO
IF (LHOOK) CALL DR_HOOK('PGD_SEAFLUX_PAR',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------
!
END SUBROUTINE PGD_SEAFLUX_PAR
