!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
    SUBROUTINE SST_UPDATE(PSST,TTIME)
!   ###############################################################
!!****  *SST_UPDATE*
!!
!!    PURPOSE
!!    -------
!
!     performs the time evolution of sst
!              
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!    none
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!      
!!    none
!!
!!    REFERENCE
!!    ---------
!!
!!      
!!    AUTHOR
!!    ------
!!
!!	P. Le Moigne          * Meteo-France *
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    09/2007
!!
!-------------------------------------------------------------------------------
!
!*       0.     DECLARATIONS
!               ------------
!
USE MODD_TYPE_DATE_SURF
USE MODD_SEAFLUX_n, ONLY : XSST_INI, TZTIME, LTZTIME_DONE, JSX
USE MODD_DATA_SEAFLUX_n, ONLY : NTIME, XDATA_SST, TDATA_SST
USE MODI_TEMPORAL_DISTS
USE MODI_TEMPORAL_LTS
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
!*      0.1    declarations of arguments
!
!
TYPE(DATE_TIME),      INTENT(IN)    :: TTIME   ! UTC time
REAL,   DIMENSION(:), INTENT(INOUT) :: PSST    ! sst
!
!*      0.2    declarations of local variables
!
INTEGER                                  :: JXP
REAL, DIMENSION(SIZE(PSST))              :: ZSST, ZSST0
REAL                                     :: ZSDTJX
REAL                                     :: ZDT, ZALPHA
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-----------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('SST_UPDATE',0,ZHOOK_HANDLE)
!
IF (.NOT.LTZTIME_DONE) THEN
   LTZTIME_DONE = .TRUE.
   JSX = 1
   TZTIME%TDATE%YEAR  = TTIME%TDATE%YEAR
   TZTIME%TDATE%MONTH = TTIME%TDATE%MONTH
   TZTIME%TDATE%DAY   = TTIME%TDATE%DAY
   TZTIME%TIME        = TTIME%TIME
ENDIF
!
ZSST0(:) = XSST_INI(:)
!
IF ( TEMPORAL_LTS ( TTIME, TDATA_SST(1) ) ) THEN
  !
  CALL TEMPORAL_DISTS ( TDATA_SST(1)%TDATE%YEAR,TDATA_SST(1)%TDATE%MONTH, &
                        TDATA_SST(1)%TDATE%DAY ,TDATA_SST(1)%TIME,        &
                        TZTIME%TDATE%YEAR   ,TZTIME%TDATE%MONTH,          &
                        TZTIME%TDATE%DAY    ,TZTIME%TIME,                 &
                        ZSDTJX                                            )  
  !
  CALL TEMPORAL_DISTS ( TTIME%TDATE%YEAR   ,TTIME%TDATE%MONTH,           &
                        TTIME%TDATE%DAY    ,TTIME%TIME,                  &
                        TZTIME%TDATE%YEAR  ,TZTIME%TDATE%MONTH,          &
                        TZTIME%TDATE%DAY   ,TZTIME%TIME,                 &
                        ZDT                                              )  
  !
  ZALPHA = ZDT / ZSDTJX
  !
  ZSST(:)= ZSST0(:)+(XDATA_SST(:,1)-ZSST0(:))*ZALPHA
  !
ELSE IF ( .NOT. TEMPORAL_LTS ( TTIME, TDATA_SST(NTIME) ) ) THEN
  !
  ZSST(:) = XDATA_SST(:,NTIME)
  !
ELSE
  !
  DO
    JXP = JSX + 1
    IF ( TEMPORAL_LTS( TTIME, TDATA_SST(JXP)) ) EXIT
    JSX = JSX + 1
  ENDDO
  !  
  CALL TEMPORAL_DISTS ( TDATA_SST(JXP)%TDATE%YEAR,TDATA_SST(JXP)%TDATE%MONTH,   &
                        TDATA_SST(JXP)%TDATE%DAY ,TDATA_SST(JXP)%TIME,          &
                        TDATA_SST(JSX)%TDATE%YEAR ,TDATA_SST(JSX)%TDATE%MONTH,  &
                        TDATA_SST(JSX)%TDATE%DAY  ,TDATA_SST(JSX)%TIME,         &
                        ZSDTJX                                            )  
  !
  CALL TEMPORAL_DISTS ( TTIME%TDATE%YEAR   ,TTIME%TDATE%MONTH,                  &
                        TTIME%TDATE%DAY    ,TTIME%TIME,                         &
                        TDATA_SST(JSX)%TDATE%YEAR,TDATA_SST(JSX)%TDATE%MONTH,   &
                        TDATA_SST(JSX)%TDATE%DAY ,TDATA_SST(JSX)%TIME,          &
                        ZDT                                             )  
  !
  ZALPHA = ZDT / ZSDTJX

  !
  ZSST(:)= XDATA_SST(:,JSX)+(XDATA_SST(:,JXP)-XDATA_SST(:,JSX))*ZALPHA
  !                
END IF
!
PSST(:) = ZSST(:)
!
IF (LHOOK) CALL DR_HOOK('SST_UPDATE',1,ZHOOK_HANDLE)
!-----------------------------------------------------------------
!
END SUBROUTINE SST_UPDATE
