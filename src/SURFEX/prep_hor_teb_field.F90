!SURFEX_LIC Copyright 1994-2014 Meteo-France 
!SURFEX_LIC This is part of the SURFEX software governed by the CeCILL-C  licence
!SURFEX_LIC version 1. See LICENSE, CeCILL-C_V1-en.txt and CeCILL-C_V1-fr.txt
!SURFEX_LIC for details. version 1.
!     #########
SUBROUTINE PREP_HOR_TEB_FIELD(HPROGRAM,HSURF,HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE)
!     #################################################################################
!
!
!!****  *PREP_HOR_TEB_FIELD* - reads, interpolates and prepares a TEB field
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
!!     V. Masson 
!!
!!    MODIFICATIONS
!!    -------------
!!      Original    01/2004
!!      P. Le Moigne 10/2005, Phasage Arome
!!------------------------------------------------------------------
!
!
USE MODD_PREP,     ONLY : CINGRID_TYPE, CINTERP_TYPE, XZS_LS, XLAT_OUT, XLON_OUT, &
                          XX_OUT, XY_OUT, CMASK
USE MODD_PREP_TEB, ONLY : XGRID_ROOF, XGRID_ROAD, XGRID_WALL, XGRID_FLOOR, LSNOW_IDEAL, &
                          XWSNOW_ROOF, XRSNOW_ROOF, XTSNOW_ROOF, XASNOW_ROOF,           &
                          XWSNOW_ROAD, XRSNOW_ROAD, XTSNOW_ROAD, XASNOW_ROAD,           &
                          XHUI_BLD, XHUI_BLD_DEF
USE MODD_TEB_n,     ONLY : TTIME, XWS_ROAD, XWS_ROOF, XT_ROAD, XT_ROOF,           &
                          XT_WALL_A, XT_WALL_B,                                   &
                          XT_CANYON,XQ_CANYON,XD_ROAD,XD_WALL,XD_ROOF,            &
                          NROAD_LAYER, NWALL_LAYER, NROOF_LAYER,                  &
                          TSNOW_ROOF, TSNOW_ROAD, XTI_ROAD, CWALL_OPT
USE MODD_BEM_n,     ONLY :XTI_BLD, XT_FLOOR, NFLOOR_LAYER, XD_FLOOR, XT_MASS, &
                          XQI_BLD, XT_WIN1, XT_WIN2                              
USE MODD_TEB_GRID_n,ONLY:  XLAT, XLON
!
USE MODD_CSTS, ONLY: XG, XP00
USE MODD_SURF_PAR, ONLY: XUNDEF
!
USE MODE_THERMOS
!
USE MODI_READ_PREP_TEB_CONF
USE MODI_READ_PREP_TEB_SNOW
USE MODI_PREP_TEB_GRIB
USE MODI_PREP_TEB_UNIF
USE MODI_PREP_TEB_BUFFER
USE MODI_HOR_INTERPOL
USE MODI_PREP_HOR_SNOW_FIELDS
USE MODI_GET_LUOUT
USE MODI_PREP_TEB_EXTERN
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
!
!*      0.2    declarations of local variables
!
 CHARACTER(LEN=6)              :: YFILETYPE ! type of input file
 CHARACTER(LEN=28)             :: YFILE     ! name of file
 CHARACTER(LEN=6)              :: YFILEPGDTYPE ! type of input file
 CHARACTER(LEN=28)             :: YFILEPGD     ! name of file
REAL, DIMENSION(:), ALLOCATABLE :: ZSG1SNOW, ZSG2SNOW, ZHISTSNOW, ZAGESNOW
REAL, POINTER, DIMENSION(:,:) :: ZFIELDIN  ! field to interpolate horizontally
REAL, ALLOCATABLE, DIMENSION(:,:) :: ZFIELDOUT ! field interpolated   horizontally
REAL, ALLOCATABLE, DIMENSION(:) :: ZPS !surface pressure
REAL, PARAMETER               :: ZRHOA=1.19 ! volumic mass of air at 20°C and 1000hPa
INTEGER                       :: ILUOUT    ! output listing logical unit
!
LOGICAL                       :: GUNIF     ! flag for prescribed uniform field
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!-------------------------------------------------------------------------------------
!
!
!*      1.     Reading of input file name and type
!
IF (LHOOK) CALL DR_HOOK('PREP_HOR_TEB_FIELD',0,ZHOOK_HANDLE)
 CALL GET_LUOUT(HPROGRAM,ILUOUT)
!
 CALL READ_PREP_TEB_CONF(HPROGRAM,HSURF,YFILE,YFILETYPE,YFILEPGD,YFILEPGDTYPE,&
                        HATMFILE,HATMFILETYPE,HPGDFILE,HPGDFILETYPE,ILUOUT,GUNIF)
!
CMASK = 'TOWN'
!
!-------------------------------------------------------------------------------------
!
!*      2.     Snow variables case?
!
IF (HSURF=='SN_ROOF') THEN
  CALL READ_PREP_TEB_SNOW(HPROGRAM,TSNOW_ROOF%SCHEME,TSNOW_ROOF%NLAYER,&
                                   TSNOW_ROAD%SCHEME,TSNOW_ROAD%NLAYER,&
                                   YFILE,YFILETYPE)
  IF (LEN_TRIM(YFILE)>0 .AND. LEN_TRIM(YFILETYPE)>0) GUNIF = .FALSE.                                   
  ALLOCATE(ZSG1SNOW(SIZE(XWSNOW_ROOF)))
  ALLOCATE(ZSG2SNOW(SIZE(XWSNOW_ROOF)))
  ALLOCATE(ZHISTSNOW(SIZE(XWSNOW_ROOF)))
  ALLOCATE(ZAGESNOW(SIZE(XWSNOW_ROOF)))                                 
  CALL PREP_HOR_SNOW_FIELDS(HPROGRAM,HSURF,              &
                            YFILE,YFILETYPE,             &
                            YFILEPGD, YFILEPGDTYPE,      &
                            ILUOUT,GUNIF,1,              &
                            SIZE(XLAT),TSNOW_ROOF, TTIME,&
                            XWSNOW_ROOF, XRSNOW_ROOF,    &
                            XTSNOW_ROOF, XASNOW_ROOF,    &
                            LSNOW_IDEAL, ZSG1SNOW,       &
                            ZSG2SNOW, ZHISTSNOW, ZAGESNOW )
  DEALLOCATE(ZSG1SNOW)
  DEALLOCATE(ZSG2SNOW)
  DEALLOCATE(ZHISTSNOW)
  DEALLOCATE(ZAGESNOW)                            
  IF (LHOOK) CALL DR_HOOK('PREP_HOR_TEB_FIELD',1,ZHOOK_HANDLE)
  RETURN
ELSE IF (HSURF=='SN_ROAD') THEN
  CALL READ_PREP_TEB_SNOW(HPROGRAM,TSNOW_ROOF%SCHEME,TSNOW_ROOF%NLAYER,&
                                   TSNOW_ROAD%SCHEME,TSNOW_ROAD%NLAYER,&
                                   YFILE,YFILETYPE)
  IF (LEN_TRIM(YFILE)>0 .AND. LEN_TRIM(YFILETYPE)>0) GUNIF = .FALSE.                                   
  ALLOCATE(ZSG1SNOW(SIZE(XWSNOW_ROAD)))
  ALLOCATE(ZSG2SNOW(SIZE(XWSNOW_ROAD)))
  ALLOCATE(ZHISTSNOW(SIZE(XWSNOW_ROAD)))
  ALLOCATE(ZAGESNOW(SIZE(XWSNOW_ROAD)))                                   
  CALL PREP_HOR_SNOW_FIELDS(HPROGRAM,HSURF,              &
                            YFILE,YFILETYPE,             &
                            YFILEPGD, YFILEPGDTYPE,      &                            
                            ILUOUT,GUNIF,1,              &
                            SIZE(XLAT),TSNOW_ROAD, TTIME,&
                            XWSNOW_ROAD, XRSNOW_ROAD,    &
                            XTSNOW_ROAD, XASNOW_ROAD,    &
                            LSNOW_IDEAL, ZSG1SNOW,       &
                            ZSG2SNOW, ZHISTSNOW, ZAGESNOW )
  DEALLOCATE(ZSG1SNOW)
  DEALLOCATE(ZSG2SNOW)
  DEALLOCATE(ZHISTSNOW)
  DEALLOCATE(ZAGESNOW)                               
  IF (LHOOK) CALL DR_HOOK('PREP_HOR_TEB_FIELD',1,ZHOOK_HANDLE)
  RETURN
END IF
!
!
!*      4.     Reading of input  configuration (Grid and interpolation type)
!
IF (GUNIF) THEN
  CALL PREP_TEB_UNIF(ILUOUT,HSURF,ZFIELDIN)
ELSE IF (YFILETYPE=='GRIB  ') THEN
  CALL PREP_TEB_GRIB(HPROGRAM,HSURF,YFILE,ILUOUT,ZFIELDIN)
 ELSE IF (YFILETYPE=='MESONH' .OR. YFILETYPE=='ASCII ' .OR. YFILETYPE=='LFI   ') THEN
  CALL PREP_TEB_EXTERN(HPROGRAM,HSURF,YFILE,YFILETYPE,YFILEPGD,YFILEPGDTYPE,ILUOUT,ZFIELDIN)
 ELSE IF (YFILETYPE=='BUFFER') THEN
  CALL PREP_TEB_BUFFER(HPROGRAM,HSURF,ILUOUT,ZFIELDIN)
 ELSE
  CALL ABOR1_SFX('PREP_HOR_TEB_FIELD: data file type not supported : '//YFILETYPE)
END IF
!
!*      5.     Horizontal interpolation
!
ALLOCATE(ZFIELDOUT(SIZE(XLAT),SIZE(ZFIELDIN,2)))
!
 CALL HOR_INTERPOL(ILUOUT,ZFIELDIN,ZFIELDOUT)
!
!*      6.     Return to historical variable
!
SELECT CASE (HSURF)
 CASE('ZS     ') 
  ALLOCATE(XZS_LS(SIZE(ZFIELDOUT,1)))
  XZS_LS(:) = ZFIELDOUT(:,1)
 CASE('WS_ROOF') 
  ALLOCATE(XWS_ROOF(SIZE(ZFIELDOUT,1)))
  XWS_ROOF(:) = ZFIELDOUT(:,1)
 CASE('WS_ROAD')
  ALLOCATE(XWS_ROAD(SIZE(ZFIELDOUT,1)))
  XWS_ROAD(:) = ZFIELDOUT(:,1)
 CASE('TI_ROAD') 
  ALLOCATE(XTI_ROAD(SIZE(ZFIELDOUT,1)))
  XTI_ROAD(:) = ZFIELDOUT(:,1)
 CASE('TI_BLD ') 
  ALLOCATE(XTI_BLD (SIZE(ZFIELDOUT,1)))
  XTI_BLD (:) = ZFIELDOUT(:,1)
 CASE('QI_BLD ') 
  ALLOCATE(XQI_BLD (SIZE(ZFIELDOUT,1)))
  IF (ALL(ZFIELDOUT .GE. XUNDEF-1.E+5 .AND. ZFIELDOUT .LE. XUNDEF+1.E+5)) THEN
     ALLOCATE(ZPS(SIZE(ZFIELDOUT,1)))
     ZPS = XP00 - ZRHOA * XG * XZS_LS
     IF (XHUI_BLD==XUNDEF) THEN
        ZFIELDOUT(:,1) = XHUI_BLD_DEF * QSAT(XTI_BLD, ZPS)
     ELSE
        ZFIELDOUT(:,1) = XHUI_BLD * QSAT(XTI_BLD, ZPS)
     ENDIF
     DEALLOCATE(ZPS)
  ENDIF
  XQI_BLD (:) = ZFIELDOUT(:,1)
 CASE('T_WIN1 ') 
  ALLOCATE(XT_WIN1 (SIZE(ZFIELDOUT,1)))
  XT_WIN1 (:) = ZFIELDOUT(:,1)
 CASE('T_WIN2 ') 
  ALLOCATE(XT_WIN2 (SIZE(ZFIELDOUT,1)))
  XT_WIN2 (:) = ZFIELDOUT(:,1)
 CASE('T_FLOOR')
  ALLOCATE(XT_FLOOR(SIZE(ZFIELDOUT,1),NFLOOR_LAYER))
  CALL INIT_FROM_REF_GRID(XGRID_FLOOR,ZFIELDOUT,XD_FLOOR,XT_FLOOR)
 CASE('T_MASS')
  ALLOCATE(XT_MASS(SIZE(ZFIELDOUT,1),NFLOOR_LAYER))
  CALL INIT_FROM_REF_GRID(XGRID_FLOOR,ZFIELDOUT,XD_FLOOR,XT_MASS)    
 CASE('T_ROAD ') 
  ALLOCATE(XT_ROAD(SIZE(ZFIELDOUT,1),NROAD_LAYER))
  CALL INIT_FROM_REF_GRID(XGRID_ROAD,ZFIELDOUT,XD_ROAD,XT_ROAD)
 CASE('T_WALLA')
  ALLOCATE(XT_WALL_A(SIZE(ZFIELDOUT,1),NWALL_LAYER))
  CALL INIT_FROM_REF_GRID(XGRID_WALL,ZFIELDOUT,XD_WALL,XT_WALL_A)
 CASE('T_WALLB')
  ALLOCATE(XT_WALL_B(SIZE(ZFIELDOUT,1),NWALL_LAYER))
  IF (CWALL_OPT=='UNIF') THEN
    XT_WALL_B = XT_WALL_A
  ELSE
    CALL INIT_FROM_REF_GRID(XGRID_WALL,ZFIELDOUT,XD_WALL,XT_WALL_B)
  END IF  
 CASE('T_ROOF ') 
  ALLOCATE(XT_ROOF(SIZE(ZFIELDOUT,1),NROOF_LAYER))
  CALL INIT_FROM_REF_GRID(XGRID_ROOF,ZFIELDOUT,XD_ROOF,XT_ROOF)
 CASE('T_CAN  ') 
  ALLOCATE(XT_CANYON(SIZE(ZFIELDOUT,1)))
  XT_CANYON (:) = ZFIELDOUT(:,1)
 CASE('Q_CAN  ') 
  ALLOCATE(XQ_CANYON(SIZE(ZFIELDOUT,1)))
  XQ_CANYON (:) = ZFIELDOUT(:,1)
END SELECT
!
!-------------------------------------------------------------------------------------
!
!*      7.     Deallocations
!
DEALLOCATE(ZFIELDIN )
DEALLOCATE(ZFIELDOUT)
IF (LHOOK) CALL DR_HOOK('PREP_HOR_TEB_FIELD',1,ZHOOK_HANDLE)
!
!-------------------------------------------------------------------------------------
!-------------------------------------------------------------------------------------
!
CONTAINS
!
!-------------------------------------------------------------------------------------
!-------------------------------------------------------------------------------------
SUBROUTINE INIT_FROM_REF_GRID(PGRID1,PT1,PD2,PT2)
!
USE MODI_INTERP_GRID
!
REAL, DIMENSION(:,:), INTENT(IN)  :: PT1    ! temperature profile
REAL, DIMENSION(:),   INTENT(IN)  :: PGRID1 ! normalized grid
REAL, DIMENSION(:,:), INTENT(IN)  :: PD2    ! output layer thickness
REAL, DIMENSION(:,:), INTENT(OUT) :: PT2    ! temperature profile
!
INTEGER                                  :: JL  ! loop counter
REAL, DIMENSION(SIZE(PT1,1),SIZE(PT1,2)) :: ZD1 ! input grid
REAL, DIMENSION(SIZE(PD2,1),SIZE(PD2,2)) :: ZD2 ! output grid
REAL, DIMENSION(SIZE(PD2,1))             :: ZD  ! output total thickness
REAL(KIND=JPRB) :: ZHOOK_HANDLE
!
IF (LHOOK) CALL DR_HOOK('INIT_FROM_REF_GRID',0,ZHOOK_HANDLE)
ZD2(:,:) = 0.
ZD (:)   = 0.
!
DO JL=1,SIZE(ZD2,2)
  ZD2(:,JL) = ZD(:) + PD2(:,JL)/2.
  ZD (:)    = ZD(:) + PD2(:,JL)
END DO
!
DO JL=1,SIZE(PT1,2)
  ZD1(:,JL) = PGRID1(JL) * ZD(:)
END DO
!
 CALL INTERP_GRID(ZD1,PT1,ZD2,PT2)
IF (LHOOK) CALL DR_HOOK('INIT_FROM_REF_GRID',1,ZHOOK_HANDLE)
!
END SUBROUTINE INIT_FROM_REF_GRID
!-------------------------------------------------------------------------------------
!
END SUBROUTINE PREP_HOR_TEB_FIELD
