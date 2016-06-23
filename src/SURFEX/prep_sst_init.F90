!SFX_LIC Copyright 1994-2014 CNRS, Meteo-France and Universite Paul Sabatier
!SFX_LIC This is part of the SURFEX software governed by the CeCILL-C licence
!SFX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt  
!SFX_LIC for details. version 1.
!     #########
    SUBROUTINE PREP_SST_INIT (DTS, S, &
                              PSST)
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
!!      P. Le Moigne          * Meteo-France *
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
!
USE MODD_DATA_SEAFLUX_n, ONLY : DATA_SEAFLUX_t
USE MODD_SEAFLUX_n, ONLY : SEAFLUX_t
!
USE MODD_TYPE_DATE_SURF
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
!
TYPE(DATA_SEAFLUX_t), INTENT(INOUT) :: DTS
TYPE(SEAFLUX_t), INTENT(INOUT) :: S
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
LOOP: DO JI = DTS%NTIME-1,1,-1
         S%JSX = JI
         IF (.NOT.TEMPORAL_LTS(S%TTIME,DTS%TDATA_SST(S%JSX))) EXIT LOOP
      ENDDO LOOP

IF ( TEMPORAL_LTS ( S%TTIME, DTS%TDATA_SST(S%JSX) ) ) THEN
   ZSST(:) = DTS%XDATA_SST(:,S%JSX)     
ELSE IF ( .NOT. TEMPORAL_LTS ( S%TTIME, DTS%TDATA_SST(DTS%NTIME) ) ) THEN
  ZSST(:) = DTS%XDATA_SST(:,DTS%NTIME)
ELSE

   CALL TEMPORAL_DISTS ( DTS%TDATA_SST(S%JSX+1)%TDATE%YEAR,DTS%TDATA_SST(S%JSX+1)%TDATE%MONTH,   &
                           DTS%TDATA_SST(S%JSX+1)%TDATE%DAY ,DTS%TDATA_SST(S%JSX+1)%TIME,          &
                           DTS%TDATA_SST(S%JSX)%TDATE%YEAR,DTS%TDATA_SST(S%JSX)%TDATE%MONTH,       &
                           DTS%TDATA_SST(S%JSX)%TDATE%DAY ,DTS%TDATA_SST(S%JSX)%TIME,              &
                           ZSDTJX                                                      )  

   CALL TEMPORAL_DISTS ( S%TTIME%TDATE%YEAR   ,S%TTIME%TDATE%MONTH,                      &
                           S%TTIME%TDATE%DAY    ,S%TTIME%TIME,                             &
                           DTS%TDATA_SST(S%JSX)%TDATE%YEAR,DTS%TDATA_SST(S%JSX)%TDATE%MONTH,       &
                           DTS%TDATA_SST(S%JSX)%TDATE%DAY ,DTS%TDATA_SST(S%JSX)%TIME,              &
                           ZDT                                                         )  
!
    ZALPHA = ZDT / ZSDTJX
!
    ZSST(:)= DTS%XDATA_SST(:,S%JSX)+(DTS%XDATA_SST(:,S%JSX+1)-DTS%XDATA_SST(:,S%JSX))*ZALPHA
                       
END IF

PSST(:) = ZSST(:)
IF (LHOOK) CALL DR_HOOK('PREP_SST_INIT',1,ZHOOK_HANDLE)
!
!-----------------------------------------------------------------
!
END SUBROUTINE PREP_SST_INIT
