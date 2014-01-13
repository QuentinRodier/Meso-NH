!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
    SUBROUTINE PREP_SST_INIT(PSST)
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
USE MODD_SEAFLUX_n, ONLY : XSST_INI, TTIME, JSX
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
REAL,   DIMENSION(:), INTENT(INOUT) :: PSST    ! sst
!
!*      0.2    declarations of local variables
!
INTEGER                                  :: IDECADE  ! decade of simulation
INTEGER                                  :: JTIME    ! decade of simulation
INTEGER, SAVE                            :: JI
INTEGER                                  :: JXP
REAL, DIMENSION(SIZE(PSST))              :: ZSST
REAL, SAVE                               :: ZSDTJX
REAL                                     :: ZDT, ZALPHA
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-----------------------------------------------------------------
!
!
IF (LHOOK) CALL DR_HOOK('PREP_SST_INIT',0,ZHOOK_HANDLE)
LOOP: DO JI = NTIME-1,1,-1
         JSX = JI
         IF (.NOT.TEMPORAL_LTS(TTIME,TDATA_SST(JSX))) EXIT LOOP
      ENDDO LOOP

IF ( TEMPORAL_LTS ( TTIME, TDATA_SST(JSX) ) ) THEN
   ZSST(:) = XDATA_SST(:,JSX)     
ELSE IF ( .NOT. TEMPORAL_LTS ( TTIME, TDATA_SST(NTIME) ) ) THEN
  ZSST(:) = XDATA_SST(:,NTIME)
ELSE

   CALL TEMPORAL_DISTS ( TDATA_SST(JSX+1)%TDATE%YEAR,TDATA_SST(JSX+1)%TDATE%MONTH,   &
                           TDATA_SST(JSX+1)%TDATE%DAY ,TDATA_SST(JSX+1)%TIME,          &
                           TDATA_SST(JSX)%TDATE%YEAR,TDATA_SST(JSX)%TDATE%MONTH,       &
                           TDATA_SST(JSX)%TDATE%DAY ,TDATA_SST(JSX)%TIME,              &
                           ZSDTJX                                                      )  

   CALL TEMPORAL_DISTS ( TTIME%TDATE%YEAR   ,TTIME%TDATE%MONTH,                      &
                           TTIME%TDATE%DAY    ,TTIME%TIME,                             &
                           TDATA_SST(JSX)%TDATE%YEAR,TDATA_SST(JSX)%TDATE%MONTH,       &
                           TDATA_SST(JSX)%TDATE%DAY ,TDATA_SST(JSX)%TIME,              &
                           ZDT                                                         )  
!
    ZALPHA = ZDT / ZSDTJX
!
    ZSST(:)= XDATA_SST(:,JSX)+(XDATA_SST(:,JSX+1)-XDATA_SST(:,JSX))*ZALPHA
                       
END IF

PSST(:) = ZSST(:)
IF (LHOOK) CALL DR_HOOK('PREP_SST_INIT',1,ZHOOK_HANDLE)
!
!-----------------------------------------------------------------
!
END SUBROUTINE PREP_SST_INIT
