!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
      SUBROUTINE WRITESURF_TEB_n(HPROGRAM,KPATCH,HWRITE)
!     ####################################
!
!!****  *WRITE_TEB_n* - writes TEB fields
!!
!!    PURPOSE
!!    -------
!!
!!**  METHOD
!!    ------
!!
!!    EXTERNAL
!!    --------
!!
!!
!!    IMPLICIT ARGUMENTS
!!    ------------------
!!
!!    REFERENCE
!!    ---------
!!
!!
!!    AUTHOR
!!    ------
!!	V. Masson   *Meteo France*	
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2003 
!-------------------------------------------------------------------------------
!
!*       0.    DECLARATIONS
!              ------------
!
!
USE MODD_TEB_n,          ONLY : LGARDEN, LGREENROOF, CBEM,      &
                                NROOF_LAYER, XT_ROOF, XWS_ROOF, &
                                NROAD_LAYER, XT_ROAD, XWS_ROAD, &
                                NWALL_LAYER,XT_WALL_A,XT_WALL_B,&
                                XTI_ROAD,                       &
                                TSNOW_ROOF, TSNOW_ROAD,         &
                                XT_CANYON, XQ_CANYON,           &
                                TTIME, NTEB_PATCH, CROAD_DIR,   &
                                XROAD_DIR,                      &
                                CWALL_OPT, XROAD_DIR
USE MODD_BEM_n,          ONLY : NFLOOR_LAYER, XT_FLOOR,         &
                                XT_MASS, XT_WIN1, XT_WIN2,      &
                                XQI_BLD, XTI_BLD                                 
!
USE MODD_DATA_TEB_n,     ONLY : LDATA_ROAD_DIR
!
USE MODI_WRITE_SURF
USE MODI_WRITESURF_GR_SNOW
USE MODI_WRITESURF_TEB_GARDEN_n
USE MODI_WRITESURF_TEB_GREENROOF_n
!
!
USE YOMHOOK   ,ONLY : LHOOK,   DR_HOOK
USE PARKIND1  ,ONLY : JPRB
!
IMPLICIT NONE
!
#ifndef NOMPI
INCLUDE "mpif.h"
#endif
!
!*       0.1   Declarations of arguments
!              -------------------------
!
 CHARACTER(LEN=6),  INTENT(IN)  :: HPROGRAM ! program calling
INTEGER,           INTENT(IN)  :: KPATCH   ! current TEB patch
 CHARACTER(LEN=3),    INTENT(IN)  :: HWRITE    ! 'PREP' : does not write SBL XUNDEF fields
!                                             ! 'ALL' : all fields are written
!
!*       0.2   Declarations of local variables
!              -------------------------------
!
INTEGER           :: IRESP           ! IRESP  : return-code if a problem appears
 CHARACTER(LEN=12) :: YRECFM         ! Name of the article to be read
 CHARACTER(LEN=100):: YCOMMENT       ! Comment string
 CHARACTER(LEN=3)  :: YPATCH         ! Patch identificator
 CHARACTER(LEN=7)  :: YDIR           ! Direction identificator
 CHARACTER(LEN=100):: YSTRING        ! Comment string
!
INTEGER :: JLAYER ! loop on surface layers
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
!-------------------------------------------------------------------------------
!
IF (LHOOK) CALL DR_HOOK('WRITESURF_TEB_N',0,ZHOOK_HANDLE)
!
YPATCH='   '
IF (NTEB_PATCH>1) WRITE(YPATCH,FMT='(A,I1,A)') 'T',KPATCH,'_'
!
!
!*       2.     Option for road orientation:
!               ---------------------------
!
YCOMMENT='Option for Road orientation in TEB scheme'
 CALL WRITE_SURF(HPROGRAM,'ROAD_DIR',CROAD_DIR,IRESP,YCOMMENT)
YCOMMENT='Option for Wall representation in TEB scheme'
 CALL WRITE_SURF(HPROGRAM,'WALL_OPT',CWALL_OPT,IRESP,YCOMMENT)
!
!*       3.     Prognostic fields:
!               -----------------
!
!* roof temperatures
!

DO JLAYER=1,NROOF_LAYER
  WRITE(YRECFM,'(A3,A5,I1.1,A1)') YPATCH,'TROOF',JLAYER,' '
  WRITE(YCOMMENT,'(A9,I1.1,A4)') 'X_Y_TROOF',JLAYER,' (K)'
  YRECFM=ADJUSTL(YRECFM)
 CALL WRITE_SURF(HPROGRAM,YRECFM,XT_ROOF(:,JLAYER),IRESP,HCOMMENT=YCOMMENT)
END DO

!
!* roof water content
!

YRECFM=YPATCH//'WS_ROOF'
YRECFM=ADJUSTL(YRECFM)
YCOMMENT='WS_ROOF (kg/m2)'
 CALL WRITE_SURF(HPROGRAM,YRECFM,XWS_ROOF(:),IRESP,HCOMMENT=YCOMMENT)
!
!* road temperatures
!

DO JLAYER=1,NROAD_LAYER
  WRITE(YRECFM,'(A3,A5,I1.1,A1)') YPATCH,'TROAD',JLAYER,' '
  YRECFM=ADJUSTL(YRECFM)
  IF (CROAD_DIR=='UNIF' .OR. LDATA_ROAD_DIR) THEN
    YSTRING = 'X_Y_TROAD'
  ELSEIF (SIZE(XROAD_DIR)>0) THEN
    !* road direction is uniform spatially, one can then indicate it in the comment
    CALL ROAD_DIR(XROAD_DIR(1),YDIR)
    YSTRING=TRIM(YDIR)//' ROAD TEMP. LAYER '
  ELSE
    YSTRING='? ROAD TEMP. LAYER '
  ENDIF
  WRITE(YCOMMENT,'(A,I1.1,A4)') TRIM(YSTRING), JLAYER,' (K)'
  CALL WRITE_SURF(HPROGRAM,YRECFM,XT_ROAD(:,JLAYER),IRESP,HCOMMENT=YCOMMENT)
END DO
!
!* road water content
!

YRECFM=YPATCH//'WS_ROAD'
YRECFM=ADJUSTL(YRECFM)
YCOMMENT='WS_ROAD (kg/m2)'
 CALL WRITE_SURF(HPROGRAM,YRECFM,XWS_ROAD(:),IRESP,HCOMMENT=YCOMMENT)
!
!* wall temperatures
!

DO JLAYER=1,NWALL_LAYER
 IF (CWALL_OPT=='UNIF') THEN
  WRITE(YRECFM,'(A3,A5,I1.1,A1)') YPATCH,'TWALL',JLAYER,' '
  YRECFM=ADJUSTL(YRECFM)
  WRITE(YCOMMENT,'(A9,I1.1,A4)') 'X_Y_TWALL',JLAYER,' (K)'
  CALL WRITE_SURF(HPROGRAM,YRECFM,XT_WALL_A(:,JLAYER),IRESP,HCOMMENT=YCOMMENT)
 ELSE
  !* Wall A
  WRITE(YRECFM,'(A3,A6,I1.1)') YPATCH,'TWALLA',JLAYER
  YRECFM=ADJUSTL(YRECFM)
  IF (LDATA_ROAD_DIR) THEN
    YSTRING = 'X_Y_TWALL_A'
  ELSEIF (SIZE(XROAD_DIR)>0) THEN
    !* wall direction is uniform spatially, one can then indicate it in the comment
    CALL WALLA_DIR(XROAD_DIR(1),YDIR)
    YSTRING=TRIM(YDIR)//'-FACING WALL TEMP. LAYER '
  ELSE
    YSTRING='?-FACING WALL TEMP. LAYER '
  ENDIF
  WRITE(YCOMMENT,'(A,I1.1,A4)') TRIM(YSTRING), JLAYER,' (K)'
  CALL WRITE_SURF(HPROGRAM,YRECFM,XT_WALL_A(:,JLAYER),IRESP,HCOMMENT=YCOMMENT)
  !
  !* Wall B
  WRITE(YRECFM,'(A3,A6,I1.1)') YPATCH,'TWALLB',JLAYER
  YRECFM=ADJUSTL(YRECFM)
  IF (LDATA_ROAD_DIR) THEN
    YSTRING = 'X_Y_TWALL_B'
  ELSEIF (SIZE(XROAD_DIR)>0) THEN
    !* wall direction is uniform spatially, one can then indicate it in the comment
    CALL WALLB_DIR(XROAD_DIR(1),YDIR)
    YSTRING=TRIM(YDIR)//'-FACING WALL TEMP. LAYER '
  ELSE
    YSTRING='?-FACING WALL TEMP. LAYER '
  ENDIF
  WRITE(YCOMMENT,'(A,I1.1,A4)') TRIM(YSTRING), JLAYER,' (K)'
  CALL WRITE_SURF(HPROGRAM,YRECFM,XT_WALL_B(:,JLAYER),IRESP,HCOMMENT=YCOMMENT)
 END IF
END DO
!
!* internal building temperature
!
YRECFM=YPATCH//'TI_BLD'
YRECFM=ADJUSTL(YRECFM)
YCOMMENT='TI_BLD (K)'
 CALL WRITE_SURF(HPROGRAM,YRECFM,XTI_BLD(:),IRESP,HCOMMENT=YCOMMENT)
!
!
!* outdoor window temperature
!
YRECFM=YPATCH//'T_WIN1'
YRECFM=ADJUSTL(YRECFM)
YCOMMENT='T_WIN1 (K)'
 CALL WRITE_SURF(HPROGRAM,YRECFM,XT_WIN1(:),IRESP,HCOMMENT=YCOMMENT)
!
IF (CBEM=='BEM') THEN
!* internal building specific humidity
!
YRECFM=YPATCH//'QI_BLD'
YRECFM=ADJUSTL(YRECFM)
YCOMMENT='QI_BLD (kg/kg)'
 CALL WRITE_SURF(HPROGRAM,YRECFM,XQI_BLD(:),IRESP,HCOMMENT=YCOMMENT)
!
  !
  !* indoor window temperature
  !
  YRECFM=YPATCH//'T_WIN2'
  YRECFM=ADJUSTL(YRECFM)
  YCOMMENT='T_WIN2 (K)'
  CALL WRITE_SURF(HPROGRAM,YRECFM,XT_WIN2(:),IRESP,HCOMMENT=YCOMMENT)
  !
  !* floor temperatures
  !
  DO JLAYER=1,NFLOOR_LAYER
    WRITE(YRECFM,'(A3,A5,I1.1,A1)') YPATCH,'TFLOO',JLAYER,' '
    WRITE(YCOMMENT,'(A9,I1.1,A4)') 'X_Y_TFLOO',JLAYER,' (K)'
    YRECFM=ADJUSTL(YRECFM)
    CALL WRITE_SURF(HPROGRAM,YRECFM,XT_FLOOR(:,JLAYER),IRESP,HCOMMENT=YCOMMENT)
  END DO
  !
  !* internal th. mass temperature
  !
  DO JLAYER=1,NFLOOR_LAYER
    WRITE(YRECFM,'(A3,A5,I1.1,A1)') YPATCH,'TMASS',JLAYER,' '
    WRITE(YCOMMENT,'(A9,I1.1,A4)') 'X_Y_TMASS',JLAYER,' (K)'
    YRECFM=ADJUSTL(YRECFM)
    CALL WRITE_SURF(HPROGRAM,YRECFM,XT_MASS(:,JLAYER),IRESP,HCOMMENT=YCOMMENT)
  END DO        
  !
ENDIF
!
!* deep road temperature
!
YRECFM=YPATCH//'TI_ROAD'
YRECFM=ADJUSTL(YRECFM)
YCOMMENT='TI_ROAD (K)'
 CALL WRITE_SURF(HPROGRAM,YRECFM,XTI_ROAD(:),IRESP,HCOMMENT=YCOMMENT)
!
!* snow mantel
!
YRECFM='RF'
 CALL WRITESURF_GR_SNOW(HPROGRAM,YRECFM,YPATCH,TSNOW_ROOF  )
!
YRECFM='RD'
 CALL WRITESURF_GR_SNOW(HPROGRAM,YRECFM,YPATCH,TSNOW_ROAD  )
!
!-------------------------------------------------------------------------------
!
!*       4.     Semi-prognostic fields:
!               ----------------------
!
!* temperature of canyon air
!
YRECFM=YPATCH//'TCANYON'
YRECFM=ADJUSTL(YRECFM)
YCOMMENT='T_CANYON (K)'
 CALL WRITE_SURF(HPROGRAM,YRECFM,XT_CANYON(:),IRESP,HCOMMENT=YCOMMENT)
!
!* humidity of canyon air
!
YRECFM=YPATCH//'QCANYON'
YRECFM=ADJUSTL(YRECFM)
YCOMMENT='Q_CANYON (kg/kg)'
 CALL WRITE_SURF(HPROGRAM,YRECFM,XQ_CANYON(:),IRESP,HCOMMENT=YCOMMENT)
!
!-------------------------------------------------------------------------------
!
!*       5.  Time
!            ----
!
IF (KPATCH==1) THEN
  YRECFM='DTCUR'
  YCOMMENT='s'
  CALL WRITE_SURF(HPROGRAM,YRECFM,TTIME,IRESP,HCOMMENT=YCOMMENT)
END IF
!
!
!-------------------------------------------------------------------------------
!
!*       6.  §Urban green areas
!            ------------------
!
! Gardens
IF (LGARDEN) CALL WRITESURF_TEB_GARDEN_n(HPROGRAM,YPATCH)
!
! Grenn roofs
IF (LGREENROOF) CALL WRITESURF_TEB_GREENROOF_n(HPROGRAM,YPATCH)
!
IF (LHOOK) CALL DR_HOOK('WRITESURF_TEB_N',1,ZHOOK_HANDLE)
!
!
!-------------------------------------------------------------------------------
CONTAINS
SUBROUTINE ROAD_DIR(PDIR,HDIR)
REAL,             INTENT(IN)  :: PDIR
 CHARACTER(LEN=7), INTENT(OUT) :: HDIR
REAL :: ZDIR
ZDIR=PDIR
IF (PDIR<0) ZDIR = PDIR +360.
IF (ZDIR>=  0.   .AND. ZDIR< 11.25) HDIR='N-S    '
IF (ZDIR>= 11.25 .AND. ZDIR< 33.75) HDIR='NNE-SSW'
IF (ZDIR>= 33.75 .AND. ZDIR< 56.25) HDIR='NE-SW'
IF (ZDIR>= 56.25 .AND. ZDIR< 78.75) HDIR='ENE-WSW'
IF (ZDIR>= 78.75 .AND. ZDIR<101.25) HDIR='E-W    '
IF (ZDIR>=101.25 .AND. ZDIR<123.75) HDIR='ESE-WNW'
IF (ZDIR>=123.75 .AND. ZDIR<146.25) HDIR='SE-NW  '
IF (ZDIR>=146.25 .AND. ZDIR<168.75) HDIR='SSE-NNW'
IF (ZDIR>=168.75 .AND. ZDIR<180.00) HDIR='N-S    '
END SUBROUTINE ROAD_DIR
SUBROUTINE WALLA_DIR(PDIR,HDIR)
REAL,             INTENT(IN)  :: PDIR
 CHARACTER(LEN=7), INTENT(OUT) :: HDIR
REAL :: ZDIR
ZDIR=PDIR
IF (PDIR<0) ZDIR = PDIR +360.
IF (ZDIR>=  0.   .AND. ZDIR< 11.25) HDIR='E      '
IF (ZDIR>= 11.25 .AND. ZDIR< 33.75) HDIR='ESE    '
IF (ZDIR>= 33.75 .AND. ZDIR< 56.25) HDIR='SE     ' 
IF (ZDIR>= 56.25 .AND. ZDIR< 78.75) HDIR='SSE    '
IF (ZDIR>= 78.75 .AND. ZDIR<101.25) HDIR='S      '
IF (ZDIR>=101.25 .AND. ZDIR<123.75) HDIR='SSW    '
IF (ZDIR>=123.75 .AND. ZDIR<146.25) HDIR='SW     '
IF (ZDIR>=146.25 .AND. ZDIR<168.75) HDIR='WSW    '
IF (ZDIR>=168.75 .AND. ZDIR<180.00) HDIR='W      '
END SUBROUTINE WALLA_DIR
SUBROUTINE WALLB_DIR(PDIR,HDIR)
REAL,             INTENT(IN)  :: PDIR
 CHARACTER(LEN=7), INTENT(OUT) :: HDIR
REAL :: ZDIR
ZDIR=PDIR
IF (PDIR<0) ZDIR = PDIR +360.
IF (ZDIR>=  0.   .AND. ZDIR< 11.25) HDIR='W      '
IF (ZDIR>= 11.25 .AND. ZDIR< 33.75) HDIR='WNW    '
IF (ZDIR>= 33.75 .AND. ZDIR< 56.25) HDIR='NW     ' 
IF (ZDIR>= 56.25 .AND. ZDIR< 78.75) HDIR='NNW    '
IF (ZDIR>= 78.75 .AND. ZDIR<101.25) HDIR='N      '
IF (ZDIR>=101.25 .AND. ZDIR<123.75) HDIR='NNE    '
IF (ZDIR>=123.75 .AND. ZDIR<146.25) HDIR='NE     '
IF (ZDIR>=146.25 .AND. ZDIR<168.75) HDIR='ENE    '
IF (ZDIR>=168.75 .AND. ZDIR<180.00) HDIR='E      '
END SUBROUTINE WALLB_DIR
!-------------------------------------------------------------------------------
!
END SUBROUTINE WRITESURF_TEB_n
